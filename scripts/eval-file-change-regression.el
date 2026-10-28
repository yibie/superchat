;;; eval-file-change-regression.el --- eval_elisp renames, copies, deletes -*- lexical-binding: t; -*-

;;; Commentary:
;; G32 (2026-10-27): `rename-file', `copy-file' and `delete-file' change
;; files on disk without `write-region', so the G28 shadow did not hold
;; them; in a field run a rename with a relative target moved two photos
;; out of their directory.  Outside `superchat-tool-eval-writable-directories'
;; they now run only at a level of full control (`auto' and above), and
;; the result lists each one; below that level the call ends as a failure
;; (the user's choice, 2026-10-27).
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/eval-file-change-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^eval-file-change/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defvar superchat-display-style)

(defvar efc--project nil "A directory standing for the user's project.")
(defvar efc--scratch nil "A directory standing for the writable temporary one.")

(defmacro efc--with (mode &rest body)
  "Run BODY under the sandbox MODE with fresh project and scratch directories.
Every approval question is answered yes."
  (declare (indent 1) (debug t))
  `(let* ((efc--project (file-name-as-directory (file-truename (make-temp-file "efc-project" t))))
          (efc--scratch (file-name-as-directory (file-truename (make-temp-file "efc-scratch" t))))
          (superchat-tool-eval-writable-directories (list efc--scratch))
          (superchat-display-style 'editor)
          (superchat-sandbox-default-mode ,mode)
          (superchat-changes-review-after-run nil)
          (superchat-approval-function (lambda (&rest _) t)))
     (unwind-protect (hm--fixture nil ,@body)
       (delete-directory efc--project t)
       (delete-directory efc--scratch t))))

(defun efc--file (dir name &optional content)
  "NAME in DIR, holding CONTENT when it is given."
  (let ((file (expand-file-name name dir)))
    (when content (write-region content nil file nil 'silent))
    file))

(defun efc--run (expression)
  "Run EXPRESSION with `eval_elisp' in one turn; return the result text."
  (setq smoke--script
        (list `(:tool-uses (("eval_elisp" (expression . ,expression))))
              '(:text "done")))
  (hm--turn (smoke--open-chat) "go")
  (let ((tool-message (cl-find-if (lambda (m) (equal "tool" (alist-get 'role m)))
                                  (reverse (hm--wire (length hm--wires))))))
    (string-remove-prefix "Result of tool call is " (hm--text tool-message))))

(ert-deftest eval-file-change/below-full-control-a-rename-is-refused ()
  (efc--with 'edit-buffers
    (let* ((a (efc--file efc--project "IMG_1.JPG" "photo"))
           (result (efc--run (format "(rename-file %S %S)" a
                                     (expand-file-name "IMG_1.jpg.new" efc--project)))))
      (should (string-match-p "it tried to rename the file" result))
      (should (string-match-p "needs the level auto" result))
      (should (file-exists-p a))
      (should-not (file-exists-p (expand-file-name "IMG_1.jpg.new" efc--project))))))

(ert-deftest eval-file-change/below-full-control-copy-in-and-delete-are-refused ()
  (efc--with 'edit-buffers
    (let ((a (efc--file efc--project "a.el" "(a)"))
          (b (efc--file efc--scratch "b.el" "(b)")))
      (should (string-match-p "it tried to delete the file"
                              (efc--run (format "(delete-file %S)" a))))
      (should (file-exists-p a))
      (should (string-match-p "it tried to copy to the file"
                              (efc--run (format "(copy-file %S %S t)" b a))))
      (should (equal "(a)" (with-temp-buffer (insert-file-contents a) (buffer-string)))))))

(ert-deftest eval-file-change/in-the-temporary-directory-nothing-is-asked-or-listed ()
  (efc--with 'edit-buffers
    (let* ((a (efc--file efc--project "a.el" "(a)"))
           (copy (expand-file-name "a-copy.el" efc--scratch))
           (result (efc--run (format "(progn (copy-file %S %S) (rename-file %S %S) (delete-file %S) 'ok)"
                                     a copy copy (concat copy "2") (concat copy "2")))))
      (should (equal "Result: ok" result))
      (should (file-exists-p a)))))

(ert-deftest eval-file-change/at-full-control-it-runs-and-is-listed ()
  (efc--with 'danger-full-access
    (let* ((a (efc--file efc--project "IMG_1.JPG" "photo"))
           (new (expand-file-name "IMG_1.jpeg" efc--project))
           (result (efc--run (format "(progn (rename-file %S %S) 'ok)" a new))))
      (should (file-exists-p new))
      (should (string-prefix-p "Result: ok\nFiles changed on disk outside the temporary directories" result))
      (should (string-match-p (regexp-quote (format "- renamed %s to %s" a new)) result)))))

(ert-deftest eval-file-change/a-failure-after-a-change-still-lists-it ()
  (efc--with 'danger-full-access
    (let* ((a (efc--file efc--project "a.el" "(a)"))
           (result (efc--run (format "(progn (delete-file %S) (error \"boom\"))" a))))
      (should-not (file-exists-p a))
      (should (string-match-p "boom" result))
      (should (string-match-p (regexp-quote (format "- deleted %s" a)) result)))))

;;; eval-file-change-regression.el ends here
