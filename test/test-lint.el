;;; test-lint.el --- tests for superchat-lint -*- lexical-binding: t; -*-

;;; Code:

(require 'helper)
(require 'superchat-lint)

(defmacro superchat-test-lint--in (mode text &rest body)
  "BODY in a temporary buffer in MODE holding TEXT."
  (declare (indent 2))
  `(with-temp-buffer (insert ,text) (funcall ,mode) ,@body))

(ert-deftest superchat-lint-sees-unbalanced-lisp ()
  (superchat-test-lint--in #'emacs-lisp-mode "(defun f ()\n  (list 1 2)\n"
    (should (equal (superchat-lint-check "f.el")
                   '("Lint: Unbalanced parentheses or an unclosed string near line 1"))))
  (superchat-test-lint--in #'emacs-lisp-mode "(defun f () (list 1 2))\n"
    (should-not (superchat-lint-check "f.el")))
  ;; Text is not Lisp: its brackets are its own.
  (superchat-test-lint--in #'text-mode "(a smile :)\n"
    (should-not (superchat-lint-check "f.txt"))))

(ert-deftest superchat-lint-sees-characters-rarely-meant ()
  (superchat-test-lint--in #'text-mode (concat "ok\nbad  here\n" "nul \0\n")
    (should (equal (superchat-lint-check "f.txt")
                   '("Lint: Line 2 holds the character U+E001, rarely meant (a control, private-use or replacement character)"
                     "Lint: Line 3 holds the character U+0000, rarely meant (a control, private-use or replacement character)"))))
  (superchat-test-lint--in #'text-mode "中文、tab\tand é\n"
    (should-not (superchat-lint-check "f.txt"))))

(ert-deftest superchat-lint-runs-the-functions-of-a-person ()
  (let ((superchat-lint-functions
         (list (lambda (file) (format "%s is too long\nsecond line" file))
               (lambda (_) nil)
               (lambda (_) (error "Broken check")))))
    (superchat-test-lint--in #'text-mode "x\n"
      (should (equal (superchat-lint-check "f.txt")
                     '("Lint: f.txt is too long" "Lint: second line"
                       "Lint: the check (a function) failed: Broken check"))))))

(ert-deftest superchat-lint-command-reports-a-failing-command ()
  (let ((check (superchat-lint-command "\\.md\\'" "printf 'a\\nb\\n'; echo %s; exit 3")))
    (should (equal (funcall check "spec x.md")
                   (format "printf 'a\\nb\\n'; echo %s; exit 3 exited 3:\na\nb\nspec x.md"
                           (shell-quote-argument "spec x.md"))))
    ;; A file it is not for, and a command that passes, say nothing.
    (should-not (funcall check "a.el"))
    (should-not (funcall (superchat-lint-command "" "true") "a.el")))
  (let ((superchat-lint-seconds 1))
    (should (string-match-p "stopped after 1 seconds"
                            (funcall (superchat-lint-command "" "sleep 5") "a.el")))))

(provide 'test-lint)
;;; test-lint.el ends here
