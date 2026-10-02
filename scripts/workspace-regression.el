;;; workspace-regression.el --- Workspace resolution from a directory -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 1.1 of M11, under
;; docs/decisions/accepted/2026-10-01-workspace-work-record-session-rules.md
;; (W1, W3, W4, W6) and
;; docs/decisions/accepted/2026-10-02-m11-slice-1-workspace-resolution.md:
;; a Workspace is found from a directory through Emacs' project
;; machinery; its id is the `.superchat/id' file when present, else the
;; hash of the real path; only the declare command writes that file.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'project)
(require 'superchat-workspace)

(defmacro workspace-test--with-dir (var &rest body)
  "Run BODY with VAR bound to a fresh real temporary directory, then delete it."
  (declare (indent 1))
  `(let ((,var (file-name-as-directory
                (file-truename (make-temp-file "ws-" t)))))
     (unwind-protect (progn ,@body)
       (delete-directory ,var t))))

(defun workspace-test--stub-finder (root)
  "Return a `project-find-functions' list that finds ROOT as a project."
  (list (lambda (dir)
          (when (file-in-directory-p dir root)
            (cons 'transient root)))))

(defun workspace-test--write-id (root content)
  "Write CONTENT as the id file of ROOT."
  (let ((file (expand-file-name ".superchat/id" root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert content))))

(defun workspace-test--all-files (dir)
  "Return every file and directory under DIR, sorted."
  (sort (directory-files-recursively dir "" t) #'string<))

(ert-deftest workspace/a-directory-in-no-project-has-no-workspace ()
  (workspace-test--with-dir dir
    (let ((project-find-functions nil))
      (should-not (superchat-workspace-of-directory dir))
      (should-not (superchat-workspace-of-directory
                   (expand-file-name "missing/" dir))))))

(ert-deftest workspace/a-project-directory-resolves-to-its-root-id-and-name ()
  (workspace-test--with-dir dir
    (let* ((sub (expand-file-name "a/b/" dir))
           (project-find-functions (workspace-test--stub-finder dir)))
      (make-directory sub t)
      (let ((ws (superchat-workspace-of-directory sub)))
        (should (equal (plist-get ws :root) dir))
        (should (equal (plist-get ws :id)
                       (superchat-workspace-id-from-root dir)))
        (should (equal (plist-get ws :name)
                       (project-name (project-current nil sub))))
        (should (eq (plist-get ws :kind) 'project))))))

(ert-deftest workspace/the-path-id-matches-the-existing-binding-id ()
  (require 'superchat)
  (workspace-test--with-dir dir
    (should (equal (superchat-workspace-id-from-root dir)
                   (plist-get (superchat--workspace-binding-for-directory dir)
                              :id)))))

(ert-deftest workspace/an-id-file-wins-over-the-path ()
  (workspace-test--with-dir dir
    (let ((project-find-functions (workspace-test--stub-finder dir)))
      (workspace-test--write-id dir "  my-id.1_x-y \n")
      (should (equal (superchat-workspace-read-id dir) "my-id.1_x-y"))
      (should (equal (plist-get (superchat-workspace-of-directory dir) :id)
                     "my-id.1_x-y")))))

(ert-deftest workspace/a-malformed-or-empty-id-file-is-ignored ()
  (workspace-test--with-dir dir
    (let ((project-find-functions (workspace-test--stub-finder dir)))
      (dolist (bad (list "" "  \n" "has space" "bad/slash" "semi;colon"
                         (make-string 129 ?a)))
        (workspace-test--write-id dir bad)
        (should-not (superchat-workspace-read-id dir))
        (should (equal (plist-get (superchat-workspace-of-directory dir) :id)
                       (superchat-workspace-id-from-root dir))))
      (workspace-test--write-id dir (make-string 128 ?a))
      (should (superchat-workspace-read-id dir))
      (delete-file (superchat-workspace-id-file dir))
      (should-not (superchat-workspace-read-id dir))
      ;; An id path that is a directory must not raise.
      (make-directory (superchat-workspace-id-file dir) t)
      (should-not (superchat-workspace-read-id dir)))))

(ert-deftest workspace/declare-writes-the-id-in-use-and-survives-a-rename ()
  (workspace-test--with-dir parent
    (let* ((old (expand-file-name "old/" parent))
           (new (expand-file-name "new/" parent))
           (project-find-functions nil)
           (before (superchat-workspace-id-from-root old)))
      (make-directory old t)
      (setq old (file-name-as-directory (file-truename old)))
      (setq before (superchat-workspace-id-from-root old))
      (let ((declared (superchat-workspace-declare old)))
        (should (equal (plist-get declared :id) before))
        (should (equal (plist-get declared :root) old)))
      (should (equal (with-temp-buffer
                       (insert-file-contents (superchat-workspace-id-file old))
                       (buffer-string))
                     (concat before "\n")))
      (superchat-workspace-install-project-finder)
      (unwind-protect
          (progn
            (rename-file (directory-file-name old) (directory-file-name new))
            (let ((ws (superchat-workspace-of-directory new)))
              (should (equal (plist-get ws :id) before))
              (should (equal (plist-get ws :root)
                             (file-name-as-directory (file-truename new))))))
        (superchat-workspace-uninstall-project-finder)))))

(ert-deftest workspace/declare-in-a-subdirectory-of-a-project-declares-the-project-root ()
  (workspace-test--with-dir dir
    (let* ((sub (expand-file-name "x/y/" dir))
           (project-find-functions (workspace-test--stub-finder dir)))
      (make-directory sub t)
      (let ((declared (superchat-workspace-declare sub)))
        (should (equal (plist-get declared :root) dir))
        (should (file-exists-p (superchat-workspace-id-file dir)))
        (should-not (file-exists-p (expand-file-name ".superchat" sub)))))))

(ert-deftest workspace/declare-leaves-an-existing-id-file-alone ()
  (workspace-test--with-dir dir
    (let ((project-find-functions (workspace-test--stub-finder dir)))
      (workspace-test--write-id dir "kept-id")
      (let ((mtime (file-attribute-modification-time
                    (file-attributes (superchat-workspace-id-file dir)))))
        (should (equal (plist-get (superchat-workspace-declare dir) :id)
                       "kept-id"))
        (should (equal (with-temp-buffer
                         (insert-file-contents
                          (superchat-workspace-id-file dir))
                         (buffer-string))
                       "kept-id"))
        (should (equal mtime
                       (file-attribute-modification-time
                        (file-attributes
                         (superchat-workspace-id-file dir)))))))
    (should-error (superchat-workspace-declare
                   (expand-file-name "missing/" dir))
                  :type 'user-error)))

(ert-deftest workspace/the-finder-makes-a-declared-plain-directory-a-project ()
  (workspace-test--with-dir dir
    (let ((sub (expand-file-name "deep/" dir))
          (project-find-functions (list (lambda (_dir) nil))))
      (make-directory sub t)
      (should-not (project-current nil sub))
      (superchat-workspace-install-project-finder)
      (unwind-protect
          (progn
            (should (= 2 (length project-find-functions)))
            (should-not (project-current nil sub))
            (superchat-workspace-declare dir)
            (let ((pr (project-current nil sub)))
              (should (eq 'superchat-workspace (car pr)))
              (should (equal (file-truename (project-root pr))
                             (file-truename dir))))
            (should (equal (plist-get (superchat-workspace-of-directory sub)
                                      :root)
                           dir)))
        (superchat-workspace-uninstall-project-finder))
      (should (= 1 (length project-find-functions))))))

(ert-deftest workspace/the-finder-does-not-take-over-a-project-emacs-already-finds ()
  (workspace-test--with-dir dir
    (let* ((sub (expand-file-name "inner/" dir))
           (project-find-functions (workspace-test--stub-finder dir)))
      (make-directory (expand-file-name ".superchat" sub) t)
      (superchat-workspace-install-project-finder)
      (should (eq 'transient (car (project-current nil sub))))
      (should (equal (plist-get (superchat-workspace-of-directory sub) :root)
                     dir)))))

(ert-deftest workspace/the-finder-ignores-the-home-directory ()
  (workspace-test--with-dir home
    (let ((old-home (getenv "HOME")))
      (unwind-protect
          (progn
            (setenv "HOME" home)
            (make-directory (expand-file-name ".superchat" home) t)
            (make-directory (expand-file-name "sub" home) t)
            (should-not (superchat-workspace-project-finder home))
            (should-not (superchat-workspace-project-finder
                         (expand-file-name "sub/" home)))
            (should-not (superchat-workspace-project-finder
                         (expand-file-name "missing/" home))))
        (setenv "HOME" old-home)))))

(ert-deftest workspace/loading-the-file-does-not-touch-project-find-functions ()
  (let ((before (copy-sequence project-find-functions)))
    (load (locate-library "superchat-workspace") nil t)
    (should (equal before project-find-functions))
    (should-not (memq #'superchat-workspace-project-finder
                      project-find-functions))))

(ert-deftest workspace/resolution-never-creates-files ()
  (workspace-test--with-dir dir
    (let ((project-find-functions (workspace-test--stub-finder dir))
          (before (workspace-test--all-files dir)))
      (superchat-workspace-of-directory dir)
      (superchat-workspace-read-id dir)
      (superchat-workspace-of-buffer)
      (should (equal before (workspace-test--all-files dir))))))

(ert-deftest workspace/a-buffer-resolves-through-its-default-directory ()
  (workspace-test--with-dir dir
    (let ((project-find-functions (workspace-test--stub-finder dir))
          (buf (generate-new-buffer " *ws*")))
      (unwind-protect
          (progn
            (with-current-buffer buf (setq default-directory dir))
            (should (equal (plist-get (superchat-workspace-of-buffer buf) :root)
                           dir))
            (with-current-buffer buf
              (should (equal (superchat-workspace-of-buffer)
                             (superchat-workspace-of-buffer buf)))))
        (kill-buffer buf))
      (should-not (superchat-workspace-of-buffer buf)))))

(ert-deftest workspace/a-temporary-workspace-has-no-root ()
  (let ((ws (superchat-workspace-temporary "s1")))
    (should (equal (plist-get ws :id) "workspace-tmp-s1"))
    (should-not (plist-get ws :root))
    (should (equal (plist-get ws :name) "temporary"))
    (should (superchat-workspace-temporary-p ws))
    (should-not (superchat-workspace-temporary-p
                 (list :id "x" :kind 'project)))
    (should-error (superchat-workspace-temporary ""))
    (should-error (superchat-workspace-temporary nil))))

;;;; The default profile mounts the finder --------------------------------------

(ert-deftest workspace/the-default-profile-installs-the-finder-after-emacs-own ()
  "Loading Superchat mounts `workspace-resolution': the finder is on
`project-find-functions', after Emacs' own `project-try-vc'."
  (require 'superchat)
  (should (memq 'workspace-resolution superchat-plugins))
  (should (memq #'superchat-workspace-project-finder project-find-functions))
  (should (< (cl-position #'project-try-vc project-find-functions)
             (cl-position #'superchat-workspace-project-finder
                          project-find-functions))))

(ert-deftest workspace/unmounting-the-row-removes-the-finder ()
  "The finder is a Scope effect of its row: unmounting withdraws it, and
mounting again puts it back."
  (require 'superchat)
  (unwind-protect
      (progn
        (superchat-plugin-unmount 'workspace-resolution)
        (should-not (memq #'superchat-workspace-project-finder
                          project-find-functions)))
    (superchat-plugin-mount 'workspace-resolution))
  (should (memq #'superchat-workspace-project-finder project-find-functions)))

(provide 'workspace-regression)
;;; workspace-regression.el ends here
