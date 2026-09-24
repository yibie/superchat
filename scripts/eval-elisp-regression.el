;;; eval-elisp-regression.el --- eval_elisp never asks the user -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of the decision "eval_elisp never asks the user"
;; (docs/decisions/accepted/2026-09-24-eval-elisp-never-asks-the-user.md).
;; Load with the prepared ELPA and the pinned llm source, then run
;; `^eval-elisp/' via ERT batch.  Run with stdin from /dev/null: before the
;; decision a prompt in batch reads stdin.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Eval-elisp regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defvar eval-elisp-test--mark nil
  "Set by an expression before it prompts, to show effects are not undone.")

(defun eval-elisp--asked-p (result)
  "Return non-nil when RESULT is the tool's refusal of a prompt."
  (and (stringp result)
       (string-prefix-p "Error: evaluating expression:" result)
       (string-match-p "tried to ask the user" result)
       (string-match-p "commandp" result)
       (string-match-p "ask_user" result)))

(ert-deftest eval-elisp/a-prompting-command-is-refused-and-explained ()
  "The acceptance run's expression: the command's minibuffer prompt stops
it, and the model is told to look the value up instead."
  (should (eval-elisp--asked-p
           (superchat-tool-eval-elisp
            "(with-temp-buffer (emacs-lisp-mode) (command-execute 'describe-function))"))))

(ert-deftest eval-elisp/every-prompt-that-emacs-can-stop-is-refused ()
  "Questions, completions, strings, passwords and single characters."
  (dolist (expression '("(y-or-n-p \"Proceed? \")"
                        "(yes-or-no-p \"Sure? \")"
                        "(completing-read \"Pick: \" '(\"a\" \"b\"))"
                        "(read-string \"Name: \")"
                        "(read-passwd \"Password: \")"
                        "(read-char \"Key: \")"
                        "(read-multiple-choice \"Choose: \" '((?a \"a\") (?b \"b\")))"))
    (ert-info (expression :prefix "expression: ")
      (should (eval-elisp--asked-p (superchat-tool-eval-elisp expression))))))

(ert-deftest eval-elisp/effects-before-the-prompt-stay ()
  "As for any evaluation error, what ran before the prompt is not undone."
  (let ((eval-elisp-test--mark nil))
    (should (eval-elisp--asked-p
             (superchat-tool-eval-elisp
              "(progn (setq eval-elisp-test--mark 'set) (read-string \"Name: \"))")))
    (should (eq 'set eval-elisp-test--mark))))

(ert-deftest eval-elisp/a-prompt-that-swallows-errors-still-ends ()
  "Code that asks inside `condition-case' and retries on error -- Emacs 30's
`read-multiple-choice' does -- would loop for ever on the stopped prompt;
the refusal must leave the whole expression at the first attempt."
  (let ((attempts 0))
    (cl-letf (((symbol-function 'eval-elisp-test--count)
               (lambda () (setq attempts (1+ attempts)))))
      (should (eval-elisp--asked-p
               (superchat-tool-eval-elisp
                "(dotimes (_ 3) (eval-elisp-test--count) (condition-case nil (read-event) (error nil)))")))
      (should (= 1 attempts)))))

(ert-deftest eval-elisp/ordinary-expressions-are-unchanged ()
  "Values, buffer work, messages and ordinary errors behave as before."
  (should (equal "Result: 3" (superchat-tool-eval-elisp "(+ 1 2)")))
  (should (equal "Result: \"x\""
                 (superchat-tool-eval-elisp
                  "(with-temp-buffer (insert \"x\") (buffer-string))")))
  (should (equal "Result: \"hello\"" (superchat-tool-eval-elisp "(message \"hello\")")))
  (let ((result (superchat-tool-eval-elisp "(car 1)")))
    (should (string-prefix-p "Error: evaluating expression:" result))
    (should-not (string-match-p "tried to ask the user" result))))

(ert-deftest eval-elisp/the-approval-question-is-asked-as-before ()
  "The approval question for a call runs before its expression and outside
the tool's binding: the answerer can still interact."
  (let ((seen nil))
    (let ((superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-event-faults nil)
          (superchat-llm-reasoning nil)
          (superchat-llm-tool-names superchat-llm-default-tool-names)
          (superchat-llm-tools-list nil))
      (smoke--with-fixture
          (list '(:tool-uses (("eval_elisp" (expression . "(+ 40 2)"))))
                '(:text "It is 42."))
        (let ((superchat-approval-function
               (lambda (_request)
                 (push inhibit-interaction seen)
                 t)))
          (let* ((view (smoke--open-chat))
                 (session (buffer-local-value 'superchat--session-id view))
                 (run (progn (superchat-permission-set session 'edit-buffers)
                             (smoke--send view "what is 40 + 2?"))))
            (should (smoke--wait (lambda () (smoke--run-done-p run)) 10))
            (should (equal '(nil) seen))
            (should (string-match-p "Result: 42"
                                    (plist-get (car smoke--requests) :prompt-text)))))))))

(ert-deftest eval-elisp/the-key-binding-page-says-not-to-run-the-command ()
  "The reference page steers the model to `commandp' instead of running it."
  (let ((body (plist-get (superchat-reference--page "emacs-keybindings") :body)))
    (should (string-match-p "(commandp 'COMMAND)" body))
    (should (string-match-p "Do not run the command to test it" body))))

(provide 'eval-elisp-regression)
;;; eval-elisp-regression.el ends here
