;;; magit-diff-regression.el --- the commit message is written from the staged diff -*- lexical-binding: t; -*-

;;; Commentary:
;; `superchat-magit-commit-message' took the diff from `magit-diff-staged',
;; a command that shows a buffer and returns no text, and turned any error
;; into nil.  The model was then asked for a commit message for "Git diff:
;; nil".  Magit itself is not loaded here: what it would answer is stubbed,
;; and the diff is read from a real repository.
;;
;; Run from the repository root:
;;   emacs -Q --batch -L . -l scripts/magit-diff-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^magit-diff/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-magit)
(defvar superchat-llm-backend)

(defun mdiff--git (&rest args)
  (unless (zerop (apply #'call-process "git" nil nil nil args))
    (error "git %s failed" (string-join args " "))))

(defmacro mdiff--with-repo (dir &rest body)
  "Run BODY in a fresh repository DIR with one commit."
  (declare (indent 1) (debug t))
  `(let* ((,dir (file-name-as-directory (file-truename (make-temp-file "mdiff-" t))))
          (default-directory ,dir)
          (process-environment
           (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null"
                     "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@example.invalid"
                     "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@example.invalid")
                   process-environment)))
     (unwind-protect
         (progn
           (mdiff--git "init" "-q")
           (with-temp-file "a.txt" (insert "MDIFF-OLD-A\n"))
           (with-temp-file "b.txt" (insert "MDIFF-OLD-B\n"))
           (mdiff--git "add" ".")
           (mdiff--git "commit" "-q" "-m" "first")
           ,@body)
       (delete-directory ,dir t))))

(defmacro mdiff--asking (prompt staged &rest body)
  "Run BODY with Magit and the model stubbed; PROMPT is set to what was asked.
STAGED is what `magit-staged-files' answers."
  (declare (indent 2) (debug t))
  `(let ((superchat-llm-backend 'mdiff))
     (cl-letf (((symbol-function 'superchat-plugin-require-active-row) #'ignore)
               ((symbol-function 'magit-staged-files) (lambda () ,staged))
               ((symbol-function 'superchat-magit--call-llm)
                (lambda (text) (setq ,prompt text) "fix: MDIFF-ANSWER"))
               ((symbol-function 'superchat-magit--deliver) #'ignore))
       ,@body)))

(ert-deftest magit-diff/the-model-is-asked-with-the-whole-staged-diff ()
  (skip-unless (executable-find "git"))
  (mdiff--with-repo dir
    (with-temp-file "a.txt" (insert "MDIFF-NEW-A\n"))
    (with-temp-file "b.txt" (insert "MDIFF-NEW-B\n"))
    (mdiff--git "add" "a.txt" "b.txt")
    ;; Not staged: it is no part of the commit.
    (with-temp-file "a.txt" (insert "MDIFF-UNSTAGED\n"))
    (let (prompt)
      (mdiff--asking prompt '("a.txt" "b.txt")
        (superchat-magit-commit-message))
      (should (stringp prompt))
      (should (string-match-p "^-MDIFF-OLD-A$" prompt))
      (should (string-match-p "^\\+MDIFF-NEW-A$" prompt))
      (should (string-match-p "^\\+MDIFF-NEW-B$" prompt))
      (should-not (string-match-p "MDIFF-UNSTAGED" prompt))
      (should-not (string-match-p "Git diff:\nnil" prompt)))))

(ert-deftest magit-diff/a-diff-that-cannot-be-read-asks-the-model-nothing ()
  (skip-unless (executable-find "git"))
  (let* ((dir (file-name-as-directory (file-truename (make-temp-file "mdiff-none-" t))))
         (default-directory dir)
         (process-environment (cons "GIT_CEILING_DIRECTORIES=/" process-environment))
         prompt)
    (unwind-protect
        (mdiff--asking prompt '("a.txt")
          ;; No repository here: git fails.
          (should-error (superchat-magit-commit-message))
          (should-not prompt))
      (delete-directory dir t))))

(provide 'magit-diff-regression)
;;; magit-diff-regression.el ends here
