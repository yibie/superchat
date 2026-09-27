;;; report-record-regression.el --- the record line under a tool-using answer -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for Part A of
;; docs/decisions/accepted/2026-09-27-claims-checked-against-the-record.md:
;; when a Run that made tool calls ends, the chat View shows one
;; product-written line saying what the Run's tools changed.  The canonical
;; smoke fixture supplies the Session and a scripted model.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/report-record-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^report-record/")'

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

(defmacro rr--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (superchat-changes-review-after-run nil)
           (target (get-buffer-create "rr-target")))
       (with-current-buffer target (erase-buffer) (insert "alpha beta\n") (set-buffer-modified-p nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (with-current-buffer target (set-buffer-modified-p nil))
         (kill-buffer target)
         (setq superchat-llm-tools-list nil)))))

(defun rr--run (view text)
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun rr--view-text (view)
  (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest report-record/an-edit-is-named-with-its-state ()
  (rr--fixture (list '(:tool-uses (("edit_buffer" (buffer_name . "rr-target")
                                    (old_string . "alpha") (new_string . "ALPHA"))))
                     '(:text "RR-DONE"))
    (let ((view (smoke--open-chat)))
      (rr--run view "change it")
      (should (with-current-buffer "rr-target" (buffer-modified-p)))
      (let ((text (rr--view-text view)))
        (should (string-match-p "RR-DONE" text))
        (should (string-match-p "Record of this task: changed rr-target (unsaved)" text))
        ;; After the answer, not before it.
        (should (< (string-match "RR-DONE" text) (string-match "Record of this task" text)))))))

(ert-deftest report-record/nothing-changed-is-said ()
  (rr--fixture (list '(:tool-uses (("read_buffer" (buffer . "rr-target"))))
                     '(:text "I edited rr-target, please save it."))
    (let ((view (smoke--open-chat)))
      (rr--run view "change it")
      (should (string-match-p "Record of this task: no buffer or file was changed by a tool"
                              (rr--view-text view))))))

(ert-deftest report-record/emacs-lisp-runs-are-counted ()
  (rr--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:tool-uses (("eval_elisp" (expression . "(+ 3 4)"))))
                     '(:text "RR-DONE"))
    (let ((view (smoke--open-chat)))
      (rr--run view "compute")
      (should (string-match-p "ran Emacs Lisp 2 times (what it changed is not tracked)"
                              (rr--view-text view))))))

(ert-deftest report-record/no-tools-no-line ()
  "Guard for the rule: a plain answer gets no record line."
  (rr--fixture (list '(:text "RR-PLAIN"))
    (let ((view (smoke--open-chat)))
      (rr--run view "hello")
      (should (string-match-p "RR-PLAIN" (rr--view-text view)))
      (should-not (string-match-p "Record of this task" (rr--view-text view))))))

(ert-deftest report-record/the-line-is-never-sent-to-the-model ()
  (rr--fixture (list '(:tool-uses (("read_buffer" (buffer . "rr-target"))))
                     '(:text "first")
                     '(:text "second"))
    (let ((view (smoke--open-chat)))
      (rr--run view "one")
      (should (string-match-p "Record of this task" (rr--view-text view)))
      (rr--run view "two")
      (should-not (string-match-p "Record of this task"
                                  (plist-get (car smoke--requests) :prompt-text))))))

(ert-deftest report-record/a-failed-run-shows-it-too ()
  (rr--fixture (list '(:tool-uses (("read_buffer" (buffer . "rr-target"))))
                     '(:error (error . "RR provider down")))
    (let* ((view (smoke--open-chat))
           (run (rr--run view "one")))
      (should (eq 'failed (plist-get (superchat-runtime-status run) :status)))
      (should (string-match-p "Record of this task: no buffer or file was changed by a tool"
                              (rr--view-text view))))))

(provide 'report-record-regression)

;;; report-record-regression.el ends here
