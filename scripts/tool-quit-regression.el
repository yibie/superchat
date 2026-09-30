;;; tool-quit-regression.el --- C-g while a tool runs -*- lexical-binding: t; -*-

;;; Commentary:
;; Pins what a person's C-g does while a tool's own code is running
;; (.scratch/issues/2026-09-30-eval-elisp-infinite-loop): a model-written
;; `eval_elisp' expression that loops for ever freezes Emacs until C-g.  The
;; quit stops the tool, cancels the Run, sends the model nothing more, and the
;; chat shows the call as stopped.  The approval-question quit is covered by
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

(ert-deftest tool-quit/c-g-inside-eval-elisp-cancels-the-run ()
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
              '(:text "TQ-NEVER-SENT"))
      (let* ((view (smoke--open-chat))
             (session (buffer-local-value 'superchat--session-id view))
             (run (smoke--send view "loop"))
             (quit-seen nil))
        ;; The quit is re-signalled to whoever is running the tool, as a
        ;; person's C-g reaches the command loop.
        (condition-case nil
            (smoke--wait (lambda () (smoke--run-done-p run)) 15)
          (quit (setq quit-seen t)))
        (should quit-seen)
        (should (smoke--wait (lambda () (smoke--run-done-p run)) 15))
        (should (eq 'cancelled (plist-get (superchat-runtime-status run) :status)))
        (should (= 1 (length smoke--requests)))
        (let ((types (mapcar (lambda (e) (plist-get e :type))
                             (cl-remove-if-not
                              (lambda (e) (equal run (plist-get e :run-id)))
                              (superchat-tape-replay-events session)))))
          (should (member "tool.failed" types))
          (should (member "run.cancelled" types))
          (should (member "turn.completed" types)))
        (let ((text (with-current-buffer view (buffer-substring-no-properties (point-min) (point-max)))))
          (should (string-match-p "eval_elisp (stopped)" text))
          (should-not (string-match-p "TQ-NEVER-SENT" text)))))))

(ert-deftest tool-quit/c-g-reaches-eval-elisp-from-a-timer ()
  "A C-g during the model's code quits it, even where Emacs inhibits quitting.
Tools run from llm.el's callbacks, which plz calls from a timer: Emacs binds
`inhibit-quit' to t there.  A real C-g only sets `quit-flag'."
  (let ((outcome nil))
    (unwind-protect
        (let ((inhibit-quit t))
          (setq outcome
                (condition-case nil
                    (list :returned
                          (superchat-tool-eval-elisp
                           "(let ((i 0)) (setq quit-flag t) (while (< i 3000000) (setq i (1+ i))) i)"))
                  (quit (list :quit))))
          ;; A C-g the code never saw must not fire later, in the test runner.
          (setq quit-flag nil))
      (setq quit-flag nil))
    (should (equal '(:quit) outcome))))

(provide 'tool-quit-regression)

;;; tool-quit-regression.el ends here
