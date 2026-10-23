;;; tool-quit-regression.el --- C-g while a tool runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Pins what stops a tool's own code while it runs
;; (.scratch/issues/2026-09-30-eval-elisp-infinite-loop): a model-written
;; `eval_elisp' expression that loops or waits for ever freezes Emacs.  At its
;; time limit, or at a person's C-g, the call fails with a result that says
;; so, and the Run goes on (decision of 2026-10-22; until then a C-g
;; cancelled the Run).  The approval-question quit is covered by
;; scripts/m6-permissions-regression.el.
;;
;; Run with the canonical smoke dependency load paths, then:
;;   -l scripts/tool-quit-regression.el
;;   --eval '(ert-run-tests-batch-and-exit "^tool-quit/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro tq--guarded (seconds &rest body)
  "Run BODY; kill this Emacs from outside if it is still running after SECONDS.
A broken time limit would otherwise hang the test runner."
  (declare (indent 1))
  `(let ((guard (make-process :name "tq-guard" :noquery t :connection-type 'pipe
                              :command (list "sh" "-c" (format "sleep %d && kill -9 %d"
                                                               ,seconds (emacs-pid))))))
     (unwind-protect (progn ,@body) (delete-process guard))))

(defun tq--watchdogs ()
  (cl-remove-if-not (lambda (p) (string-prefix-p "superchat-eval-watchdog" (process-name p)))
                    (process-list)))

(defun tq--eval-from-a-timer (expression)
  "Evaluate EXPRESSION as a tool body run from a timer: quitting inhibited.
Return (TEXT ERROR-TYPE SECONDS)."
  (let ((superchat-tool-execution--body-failure (list nil))
        (start (float-time)))
    (let* ((text (let ((inhibit-quit t)) (superchat-tool-eval-elisp expression)))
           (seconds (- (float-time) start)))
      (list text (cdar superchat-tool-execution--body-failure) seconds))))

(ert-deftest tool-quit/a-runaway-eval-stops-at-its-time-limit ()
  "A loop, a loop that catches every error and a loop that inhibits
quitting: each is stopped at the limit, from a timer as tools run, and
nothing is left behind.  A synchronous process that does not end is stopped
the same way in an interactive Emacs, whose own timer signals break its
wait; batch Emacs sends none, so its `call-process' waits for the child
and that case is checked against a running Emacs, not here."
  (tq--guarded 40
    (let ((superchat-tool-eval-time-limit 1))
      (dolist (expression '("(while t)"
                            "(let ((n 0)) (while t (condition-case nil (car n) (t (setq n (1+ n))))))"
                            "(let ((inhibit-quit t) (n 0)) (while t (setq n (1+ n))))"))
        (pcase-let ((`(,text ,type ,seconds) (tq--eval-from-a-timer expression)))
          (ert-info ((format "%s => %S" expression text))
            (should (equal "time-limit" type))
            (should (string-match-p "stopped after 1 seconds, the time limit" text))
            (should (string-match-p "make-process" text))
            (should (< seconds 4))
            (should-not quit-flag)
            (should-not debug-on-quit)
            (should-not (get-buffer "*Backtrace*"))
            (should-not (tq--watchdogs))))))))

(ert-deftest tool-quit/a-quick-eval-leaves-no-watchdog ()
  "An expression that returns in time ends its watchdog, and no signal
reaches Emacs once the limit has passed."
  (tq--guarded 20
    (let ((superchat-tool-eval-time-limit 1))
      (should (equal '("Result: 3" nil)
                     (butlast (tq--eval-from-a-timer "(+ 1 2)"))))
      (should-not (tq--watchdogs))
      (let ((count 0))
        ;; Function calls past the limit: an armed debugger would run here.
        (while (< count 15) (sleep-for 0.1) (setq count (1+ (identity count))))))
    (should-not quit-flag)
    (should-not debug-on-quit)
    (should-not (get-buffer "*Backtrace*"))))

(ert-deftest tool-quit/an-eval-past-the-time-limit-returns-its-value ()
  "An expression that returns just as the signal comes keeps nothing armed."
  (tq--guarded 20
    (let ((superchat-tool-eval-time-limit 0.3))
      (tq--eval-from-a-timer "(progn (sleep-for 0.29) 'late)")
      (let ((count 0))
        (while (< count 10) (sleep-for 0.05) (setq count (1+ (identity count))))))
    (should-not quit-flag)
    (should-not debug-on-quit)
    (should-not (get-buffer "*Backtrace*"))
    (should-not (tq--watchdogs))))

(ert-deftest tool-quit/c-g-reaches-eval-elisp-from-a-timer ()
  "A C-g during the model's code stops it, even where Emacs inhibits quitting.
Tools run from llm.el's callbacks, which plz calls from a timer: Emacs binds
`inhibit-quit' to t there.  A real C-g only sets `quit-flag'.  The call
fails with a result that says the user stopped it; no quit is left."
  (let ((outcome nil))
    (unwind-protect
        (setq outcome
              (tq--eval-from-a-timer
               "(let ((i 0)) (setq quit-flag t) (while (< i 3000000) (setq i (1+ i))) i)"))
      (setq quit-flag nil))
    (should (equal "user-quit" (nth 1 outcome)))
    (should (string-match-p "the user stopped it with C-g" (car outcome)))
    (should-not quit-flag)))

(ert-deftest tool-quit/c-g-inside-eval-elisp-stops-only-the-call ()
  "A C-g stops the call and the Run goes on: the model is told the user
stopped it, and the chat shows the call failed."
  (skip-when (getenv "SMOKE_REAL"))
  (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
        (superchat-runtime--queue-timers (make-hash-table :test #'equal))
        (superchat-event-faults nil)
        (superchat-llm-reasoning nil)
        (superchat-llm-tool-names superchat-llm-default-tool-names)
        (superchat-llm-tools-list nil)
        (superchat-changes-review-after-run nil)
        (superchat-sandbox-default-mode 'danger-full-access))
    (smoke--with-fixture
        (list '(:tool-uses (("eval_elisp" (expression . "(progn (signal 'quit nil) 1)"))))
              '(:text "TQ-AFTER"))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (smoke--send view "loop")))
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 15))
        (should (eq 'completed (plist-get (superchat-runtime-status run) :status)))
        (should (= 2 (length smoke--requests)))
        (should (string-match-p "the user stopped it with C-g"
                                (plist-get (car smoke--requests) :prompt-text)))
        (let ((types (mapcar (lambda (e) (plist-get e :type))
                             (cl-remove-if-not
                              (lambda (e) (equal run (plist-get e :run-id)))
                              (superchat-tape-replay-events session)))))
          (should (member "tool.failed" types))
          (should-not (member "run.cancelled" types)))
        (let ((text (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max)))))
          (should (string-match-p "^\\*\\*\\* ✗ eval_elisp (" text))
          (should (string-match-p "TQ-AFTER" text)))))))

(provide 'tool-quit-regression)

;;; tool-quit-regression.el ends here
