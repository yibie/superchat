;;; report-review-regression.el --- the reviewer request behind a switch -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for Part B of
;; docs/decisions/accepted/2026-09-27-claims-checked-against-the-record.md:
;; with `superchat-report-review' on, a chat Run that made tool calls and
;; ends on a text-only reply gets one review request (a fresh context, no
;; tools: the request, the tool lines, the reply); findings go back to the
;; model once.  The smoke fixture scripts every request in order, the review
;; included.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/report-review-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^report-review/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(setq print-circle t
      ert-batch-print-level 6
      ert-batch-print-length 16
      ert-batch-backtrace-right-margin 160)

(defvar superchat-report-review)

(defmacro rv--fixture (review script &rest body)
  "Run BODY with `superchat-report-review' set to REVIEW and SCRIPT scripted."
  (declare (indent 2) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (superchat-changes-review-after-run nil)
           (superchat-report-review ,review)
           (target (get-buffer-create "rv-target")))
       (with-current-buffer target (erase-buffer) (insert "RV-TARGET-TEXT\n"))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (kill-buffer target)
         (setq superchat-llm-tools-list nil)))))

(defun rv--run (view text)
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun rv--prompts ()
  "Every request's prompt text, oldest first."
  (mapcar (lambda (r) (plist-get r :prompt-text)) (reverse smoke--requests)))

(defun rv--review-events (session run)
  (cl-remove-if-not (lambda (e) (and (equal "run.report-reviewed" (plist-get e :type))
                                     (equal run (plist-get e :run-id))))
                    (superchat-tape-replay-events session)))

(defconst rv--read-step '(:tool-uses (("read_buffer" (buffer . "rv-target")))))

(ert-deftest report-review/off-by-default-nothing-more ()
  "Guard: without the switch a tool-using Run is as before."
  (skip-when (getenv "SMOKE_REAL"))
  (should (boundp 'superchat-report-review))
  (should-not (default-value 'superchat-report-review))
  (rv--fixture nil (list rv--read-step '(:text "I changed rv-target."))
    (let ((view (smoke--open-chat)))
      (rv--run view "RV-REQUEST change it")
      (should (= 2 (length smoke--requests))))))

(ert-deftest report-review/ok-keeps-the-reply-and-runs-one-review ()
  (rv--fixture t (list rv--read-step '(:text "RV-REPLY: I read rv-target.") '(:text "OK"))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (rv--run view "RV-REQUEST read it")))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      (should (= 3 (length smoke--requests)))
      ;; The review request: the request, the tool line, the reply; no tools.
      (let ((review (car smoke--requests)))
        (should (string-match-p "RV-REQUEST read it" (plist-get review :prompt-text)))
        (should (string-match-p "Tool: read_buffer" (plist-get review :prompt-text)))
        (should (string-match-p "RV-REPLY: I read rv-target." (plist-get review :prompt-text)))
        (should (null (plist-get review :tools))))
      (should (string-match-p "RV-REPLY" (with-current-buffer view (buffer-string))))
      (let ((events (rv--review-events session run)))
        (should (= 1 (length events)))
        (should (equal "ok" (plist-get (plist-get (car events) :payload) :verdict)))))))

(ert-deftest report-review/findings-go-back-once-and-the-next-reply-is-final ()
  (rv--fixture t (list rv--read-step
                       '(:text "RV-REPLY: I edited rv-target, please save it.")
                       '(:text "- The report says rv-target was edited; the record shows only a read.")
                       '(:text "RV-CORRECTED: I only read rv-target; nothing was edited."))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (rv--run view "RV-REQUEST fix it")))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      ;; model, model, review, model -- and no second review.
      (should (= 4 (length smoke--requests)))
      (let ((correction (plist-get (car smoke--requests) :prompt-text)))
        (should (string-match-p "A check of your report against this task's tool record found" correction))
        (should (string-match-p "the record shows only a read" correction)))
      (should (string-match-p "RV-CORRECTED" (with-current-buffer view (buffer-string))))
      (let ((events (rv--review-events session run)))
        (should (= 1 (length events)))
        (should (equal "findings" (plist-get (plist-get (car events) :payload) :verdict)))))))

(ert-deftest report-review/no-tool-call-no-review ()
  (rv--fixture t (list '(:text "RV-PLAIN"))
    (let ((view (smoke--open-chat)))
      (rv--run view "hello")
      (should (= 1 (length smoke--requests))))))

(ert-deftest report-review/the-finish-check-goes-first-and-alone ()
  "A reply the finish check catches is not reviewed as well."
  (rv--fixture t (list rv--read-step
                       '(:text "Let me check the file again.")
                       '(:text "RV-FINAL"))
    (let ((view (smoke--open-chat)))
      (rv--run view "RV-REQUEST read it")
      ;; model, model (finish check fires), model -- no review request.
      (should (= 3 (length smoke--requests)))
      (should (string-match-p "RV-FINAL" (with-current-buffer view (buffer-string)))))))

(ert-deftest report-review/a-failed-review-leaves-the-reply-final ()
  (rv--fixture t (list rv--read-step
                       '(:text "RV-REPLY: done.")
                       '(:error (error . "RV review provider down")))
    (let* ((view (smoke--open-chat))
           (session (buffer-local-value 'superchat--session-id view))
           (run (rv--run view "RV-REQUEST read it")))
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
      ;; The review request was really made (and failed at the provider).
      (should (= 3 (length smoke--requests)))
      (should (string-match-p "RV-REPLY: done." (with-current-buffer view (buffer-string))))
      (should (equal "failed" (plist-get (plist-get (car (rv--review-events session run)) :payload)
                                         :verdict))))))

(provide 'report-review-regression)

;;; report-review-regression.el ends here
