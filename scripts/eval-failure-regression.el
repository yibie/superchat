;;; eval-failure-regression.el --- an eval_elisp error is a failed call -*- lexical-binding: t; -*-

;;; Commentary:
;; Slice 9a (reports/guidance-2026-10-11/SLICE-9.md).  `eval_elisp' turned an
;; evaluation error into the text "Error: evaluating expression: ..." and
;; returned it as its result, so the Tape recorded `tool.completed' with
;; status ok: in the user's two Runs of 2026-10-18, 25 errors were all ok.
;; An evaluation error, and an expression that tried to ask the user, now
;; end the call as `tool.failed'.  The model receives the same text as
;; before.  The record line under the answer still counts every time Emacs
;; Lisp ran and says how many calls failed.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/eval-failure-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^eval-failure/")'

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

(defmacro ef--fixture (script &rest body)
  (declare (indent 1) (debug t))
  `(progn
     (skip-when (getenv "SMOKE_REAL"))
     (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
           (superchat-runtime--queue-timers (make-hash-table :test #'equal))
           (superchat-event-faults nil)
           (superchat-llm-reasoning nil)
           (superchat-llm-tool-names superchat-llm-default-tool-names)
           (superchat-llm-tools-list nil)
           (superchat-sandbox-default-mode 'danger-full-access)
           (superchat-changes-review-after-run nil))
       (unwind-protect (smoke--with-fixture ,script ,@body)
         (setq superchat-llm-tools-list nil)))))

(defun ef--run (view text)
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))
    run))

(defun ef--view-text (view)
  (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max))))

(defun ef--outcomes (view tool)
  "The `tool.completed' and `tool.failed' events of TOOL in VIEW's Session."
  (cl-remove-if-not
   (lambda (event)
     (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
          (equal tool (plist-get (plist-get event :payload) :tool-name))))
   (superchat-tape-replay-events
    (buffer-local-value 'superchat--session-id view))))

(defun ef--types (events)
  (mapcar (lambda (event) (plist-get event :type)) events))

(defun ef--second-prompt ()
  "The prompt text of the second request the scripted model received."
  (plist-get (car smoke--requests) :prompt-text))

(ert-deftest eval-failure/an-evaluation-error-is-a-failed-call ()
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(car 1)"))))
                     '(:text "EF-DONE"))
    (let* ((view (smoke--open-chat))
           (run (ef--run view "compute")))
      (let ((outcomes (ef--outcomes view "eval_elisp")))
        (should (equal '("tool.failed") (ef--types outcomes)))
        (should (equal "failed"
                       (plist-get (plist-get (car outcomes) :payload) :status))))
      ;; What the model is told has not changed.
      (should (string-match-p
               (regexp-quote "Error: evaluating expression: Wrong type argument: listp, 1")
               (ef--second-prompt)))
      (should-not (string-match-p (regexp-quote "[Tool call failed")
                                  (ef--second-prompt)))
      ;; A failed tool call does not fail the Run.
      (should (eq 'completed (plist-get (superchat-runtime-status run) :status))))))

(ert-deftest eval-failure/asking-the-user-is-a-failed-call ()
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(read-string \"Name: \")"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "ask")
      (should (equal '("tool.failed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p
               (regexp-quote "it tried to ask the user for input")
               (ef--second-prompt))))))

(ert-deftest eval-failure/asking-past-the-signal-hook-is-a-failed-call ()
  "An expression that binds `signal-hook-function' itself reaches the
handler of the call, not the hook; that is a failed call too."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(let ((signal-hook-function nil)) (read-string \"Name: \"))"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "ask")
      (should (equal '("tool.failed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p
               (regexp-quote "it tried to ask the user for input")
               (ef--second-prompt))))))

(ert-deftest eval-failure/the-chat-marks-a-failed-call-failed ()
  "The chat's heading of a call follows the call's outcome, not its text:
an `eval_elisp' error reads \"Error: ...\", and its heading showed ✓ while
the Tape held `tool.failed' and the failure brake counted it."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(car 1)"))))
                     '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (let ((text (ef--view-text view)))
        (should (string-match-p "^\\*\\*\\* ✗ eval_elisp (" text))
        (should (string-match-p "^\\*\\*\\* ✓ eval_elisp (" text))
        (should (< (string-match "^\\*\\*\\* ✗ eval_elisp" text)
                   (string-match "^\\*\\*\\* ✓ eval_elisp" text)))))))

(ert-deftest eval-failure/waiting-with-sit-for-is-a-completed-call ()
  "`sit-for' waits by reading an event with a timeout, which
`inhibit-interaction' stopped as if the expression had asked the user: in
the user's Run of 2026-10-22 every probe that waited for a process failed
that way.  A wait is not a question.  Batch Emacs's `sit-for' only sleeps,
so the expression takes the interactive path itself."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(let ((noninteractive nil)) (list (sit-for 0.01) (sit-for 0.01 t) 'waited))"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "wait")
      (should (equal '("tool.completed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p (regexp-quote "Result: (t t waited)")
                              (ef--second-prompt))))))

(ert-deftest eval-failure/a-wait-that-reads-input-names-the-way-to-wait ()
  "A wait that does read input, `read-event' with a timeout, still stops,
and the result says how to wait instead."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(read-event nil nil 0.01)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "wait")
      (should (equal '("tool.failed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p (regexp-quote "sleep-for") (ef--second-prompt)))
      (should (string-match-p (regexp-quote "accept-process-output")
                              (ef--second-prompt))))))

(ert-deftest eval-failure/more-than-one-expression-is-a-failed-call ()
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2) (+ 3 4)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (should (equal '("tool.failed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p (regexp-quote "Expected exactly one expression")
                              (ef--second-prompt))))))

;; 2026-10-22 field test: the model's expression lacked one closing
;; parenthesis.  "evaluating expression: End of file during parsing" read as
;; if its code had hit the end of the buffer it was scanning, so it sent the
;; same expression twice more and the failure brake ended the Run.
(ert-deftest eval-failure/an-unreadable-expression-says-it-did-not-read ()
  (ef--fixture (list '(:tool-uses (("eval_elisp"
                                    (expression . "(with-current-buffer \"x\"\n  (list 1 (+ 2 3))"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (should (equal '("tool.failed") (ef--types (ef--outcomes view "eval_elisp"))))
      (let ((prompt (ef--second-prompt)))
        (should (string-match-p (regexp-quote "nothing was evaluated") prompt))
        (should (string-match-p (regexp-quote "1 parenthesis is never closed")
                                prompt))
        (should (string-match-p
                 (regexp-quote "line 1, column 0: (with-current-buffer")
                 prompt))
        (should-not (string-match-p (regexp-quote "evaluating expression")
                                    prompt))))))

(ert-deftest eval-failure/a-value-is-still-a-completed-call ()
  "Guard: the shape of a successful result is unchanged."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (should (equal '("tool.completed") (ef--types (ef--outcomes view "eval_elisp"))))
      (should (string-match-p (regexp-quote "Result: 3") (ef--second-prompt))))))

(ert-deftest eval-failure/the-record-line-counts-runs-and-failures ()
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:tool-uses (("eval_elisp" (expression . "(car 1)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (should (string-match-p
               (regexp-quote "ran Emacs Lisp 2 times (what it changed is not tracked); 1 tool call failed.")
               (ef--view-text view))))))

(ert-deftest eval-failure/no-failure-no-failure-count ()
  "Guard: the record line of a Run whose calls all ran is as before."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:text "EF-DONE"))
    (let ((view (smoke--open-chat)))
      (ef--run view "compute")
      (should (string-match-p
               (regexp-quote "ran Emacs Lisp 1 time (what it changed is not tracked).")
               (ef--view-text view)))
      (should-not (string-match-p "failed\\." (ef--view-text view))))))

(ert-deftest eval-failure/a-denied-call-is-not-counted-as-failed ()
  "Guard: a call the permission level refuses is the user's decision, not a failure."
  (ef--fixture (list '(:tool-uses (("eval_elisp" (expression . "(+ 1 2)"))))
                     '(:text "EF-DONE"))
    (let ((superchat-sandbox-default-mode 'read-only)
          (view (smoke--open-chat)))
      (ef--run view "compute")
      (should (equal '("denied")
                     (mapcar (lambda (event) (plist-get (plist-get event :payload) :status))
                             (ef--outcomes view "eval_elisp"))))
      (should-not (string-match-p "tool calls? failed" (ef--view-text view))))))

(ert-deftest eval-failure/called-outside-a-tool-call-it-returns-the-text ()
  "Guard: a direct caller of the function still gets the error text back."
  (should (string-prefix-p "Error: evaluating expression: "
                           (superchat-tool-eval-elisp "(car 1)")))
  (should (equal "Result: 3" (superchat-tool-eval-elisp "(+ 1 2)"))))

(provide 'eval-failure-regression)

;;; eval-failure-regression.el ends here
