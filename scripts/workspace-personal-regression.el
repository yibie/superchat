;;; workspace-personal-regression.el --- the Personal Workspace and its interfaces -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 3 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-3-personal-workspace.md,
;; contract W11): interfaces that belong to no directory -- the agenda,
;; feeds, chats -- belong to the Personal Workspace the user designates;
;; until one is designated a conversation called from them is temporary;
;; one buffer can be given to another Workspace; the Personal Workspace is
;; not a global layer.  The interface is simulated with a mode of this file.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/workspace-personal-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^workspace-personal/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(defvar superchat-display-style)

(defvar wpers--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wpers--finder (dir)
  (cl-loop for root in wpers--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wpers--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-personal-" t))))

(defmacro wpers--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wpers--roots nil)
          ,@(mapcar (lambda (v) `(,v (wpers--temp-dir))) vars)
          (project-find-functions (list #'wpers--finder))
          (superchat-display-style 'editor))
     (setq wpers--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wpers--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wpers--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wpers--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wpers--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(define-derived-mode wpers-agenda-mode special-mode "WPers-Agenda"
  "Stands for an interface that belongs to no directory, like the agenda.")

(define-derived-mode wpers-agenda-child-mode wpers-agenda-mode "WPers-Child"
  "A mode derived from `wpers-agenda-mode'.")

(defmacro wpers--with-personal (script vars personal &rest body)
  "Like `wpers--with', with the interface mode registered.
PERSONAL is the value of `superchat-personal-workspace-directory'."
  (declare (indent 3) (debug t))
  `(wpers--with ,script ,vars
     (let ((superchat-personal-workspace-modes '(wpers-agenda-mode))
           (superchat-personal-workspace-directory ,personal))
       ,@body)))

(defun wpers--agenda (&optional directory mode)
  "Return a fresh interface buffer whose `default-directory' is DIRECTORY."
  (let ((buffer (generate-new-buffer "wpers-agenda")))
    (with-current-buffer buffer
      (funcall (or mode #'wpers-agenda-mode))
      (when directory (setq default-directory directory)))
    buffer))

(defun wpers--plain-dir ()
  "A fresh directory that is no project root in these tests."
  (wpers--temp-dir))

(defun wpers--binding-root (view)
  (plist-get (buffer-local-value 'superchat--workspace-binding view) :root))

(defun wpers--all-files (dir)
  (sort (directory-files-recursively dir "" t) #'string<))

;;;; Which Workspace a source buffer belongs to -----------------------------------

(ert-deftest workspace-personal/the-default-modes-name-the-agenda-feeds-and-chats ()
  (dolist (mode '(org-agenda-mode elfeed-search-mode telega-chat-mode))
    (should (memq mode (default-value 'superchat-personal-workspace-modes))))
  (should-not (default-value 'superchat-personal-workspace-directory)))

(ert-deftest workspace-personal/an-interface-is-temporary-until-one-is-designated ()
  "Even when its `default-directory' happens to lie in a project."
  (wpers--with-personal nil (p) nil
    (let ((agenda (wpers--agenda p)))
      (unwind-protect
          (progn
            (should (superchat-workspace-personal-buffer-p agenda))
            (should-not (superchat-workspace-personal))
            (should-not (superchat-workspace-of-source agenda))
            ;; The buffer's directory alone would have given the project.
            (should (superchat-workspace-of-buffer agenda)))
        (kill-buffer agenda)))))

(ert-deftest workspace-personal/an-interface-belongs-to-the-designated-directory ()
  (let ((dir (wpers--plain-dir)))
    (unwind-protect
        (wpers--with-personal nil (p) dir
          (let ((agenda (wpers--agenda p))
                (child (wpers--agenda p #'wpers-agenda-child-mode))
                (before (wpers--all-files dir)))
            (unwind-protect
                (let ((ws (superchat-workspace-of-source agenda)))
                  (should (equal dir (plist-get ws :root)))
                  (should (equal (superchat-workspace-id-from-root dir)
                                 (plist-get ws :id)))
                  (should (equal (file-name-nondirectory (directory-file-name dir))
                                 (plist-get ws :name)))
                  (should (equal ws (superchat-workspace-personal)))
                  ;; A derived mode counts.
                  (should (equal ws (superchat-workspace-of-source child)))
                  ;; Resolution writes nothing.
                  (should (equal before (wpers--all-files dir))))
              (kill-buffer agenda)
              (kill-buffer child))))
      (delete-directory dir t))))

(ert-deftest workspace-personal/a-designated-directory-inside-a-project-is-that-project ()
  (wpers--with-personal nil (p) nil
    (let ((sub (expand-file-name "life/" p)))
      (make-directory sub t)
      (let ((superchat-personal-workspace-directory sub))
        (should (equal p (plist-get (superchat-workspace-personal) :root)))))))

(ert-deftest workspace-personal/a-missing-directory-designates-nothing ()
  (wpers--with-personal nil (p) "/no/such/dir/for-superchat/"
    (let ((agenda (wpers--agenda p)))
      (unwind-protect
          (progn
            (should-not (superchat-workspace-personal))
            (should-not (superchat-workspace-of-source agenda)))
        (kill-buffer agenda)))))

(ert-deftest workspace-personal/other-buffers-resolve-as-before ()
  (let ((dir (wpers--plain-dir)))
    (unwind-protect
        (wpers--with-personal nil (p) dir
          (let ((file (wpers--file p))
                (loose (generate-new-buffer "wpers-loose")))
            (unwind-protect
                (progn
                  (with-current-buffer loose (setq default-directory dir))
                  (should-not (superchat-workspace-personal-buffer-p file))
                  (should (equal p (plist-get (superchat-workspace-of-source file) :root)))
                  ;; A plain buffer sitting in the personal directory is not an
                  ;; interface: the directory is no project, so it is temporary.
                  (should-not (superchat-workspace-of-source loose))
                  (with-current-buffer file
                    (should (equal (superchat-workspace-of-source)
                                   (superchat-workspace-of-source file)))))
              (kill-buffer loose))
            (should-not (superchat-workspace-of-source loose))))
      (delete-directory dir t))))

(ert-deftest workspace-personal/a-buffer-can-be-given-to-another-workspace ()
  (let ((dir (wpers--plain-dir)))
    (unwind-protect
        (wpers--with-personal nil (p q) dir
          (let ((agenda (wpers--agenda p))
                (file (wpers--file p)))
            (unwind-protect
                (progn
                  (with-current-buffer agenda
                    (setq-local superchat-buffer-workspace-directory q))
                  (should (equal q (plist-get (superchat-workspace-of-source agenda) :root)))
                  (with-current-buffer file
                    (setq-local superchat-buffer-workspace-directory q))
                  (should (equal q (plist-get (superchat-workspace-of-source file) :root)))
                  ;; A directory that does not exist is ignored.
                  (with-current-buffer agenda
                    (setq-local superchat-buffer-workspace-directory "/no/such/dir/"))
                  (should (equal dir (plist-get (superchat-workspace-of-source agenda) :root))))
              (kill-buffer agenda))))
      (delete-directory dir t))))

(ert-deftest workspace-personal/designating-declares-and-saves ()
  (let ((dir (wpers--plain-dir))
        (superchat-personal-workspace-directory nil)
        saved)
    (unwind-protect
        (let ((project-find-functions nil))
          (cl-letf (((symbol-function 'customize-save-variable)
                     (lambda (symbol value &rest _)
                       (setq saved (list symbol value))
                       (set symbol value))))
            (let ((ws (superchat-set-personal-workspace dir)))
              (should (equal dir (plist-get ws :root)))
              (should (equal (list 'superchat-personal-workspace-directory dir) saved))
              (should (file-exists-p (superchat-workspace-id-file dir)))
              (should (equal (plist-get ws :id) (superchat-workspace-read-id dir)))
              (should (equal (plist-get ws :id)
                             (plist-get (superchat-workspace-personal) :id))))
            (should-error (superchat-set-personal-workspace
                           (expand-file-name "missing/" dir))
                          :type 'user-error)))
      (delete-directory dir t))))

;;;; Conversations called from an interface --------------------------------------

(ert-deftest workspace-personal/the-chat-called-from-an-interface-is-the-personal-conversation ()
  (let ((dir (wpers--plain-dir)))
    (unwind-protect
        (wpers--with-personal '((:text "ok") (:text "ok")) (p) dir
          (wpers--write dir "AGENTS.md" "RULE-MARKER-PERSONAL\n")
          (wpers--write p "AGENTS.md" "RULE-MARKER-P\n")
          (let* ((agenda (wpers--agenda p))
                 (view (with-current-buffer agenda (superchat)))
                 (other (wpers--agenda p))
                 (vp (with-current-buffer (wpers--file p) (superchat))))
            (unwind-protect
                (progn
                  (should (equal dir (wpers--binding-root view)))
                  ;; Another interface buffer reaches the same conversation.
                  (should (eq view (with-current-buffer other (superchat))))
                  (should-not (eq view vp))
                  (hm--turn view "what is on today")
                  (hm--turn vp "hello from p")
                  (should (string-match-p "RULE-MARKER-PERSONAL" (wpers--system 1)))
                  (should-not (string-match-p "RULE-MARKER-P\n" (wpers--system 1)))
                  ;; Not a global layer: the project never sees the personal rules.
                  (should (string-match-p "RULE-MARKER-P\n" (wpers--system 2)))
                  (should-not (string-match-p "RULE-MARKER-PERSONAL" (wpers--system 2))))
              (kill-buffer agenda)
              (kill-buffer other))))
      (delete-directory dir t))))

(ert-deftest workspace-personal/a-request-made-from-an-interface-joins-the-personal-conversation ()
  (let ((dir (wpers--plain-dir)))
    (unwind-protect
        (wpers--with-personal '((:text "ok") (:text "ok")) (p) dir
          (wpers--write dir "AGENTS.md" "RULE-MARKER-PERSONAL\n")
          (let ((agenda (wpers--agenda p)))
            (unwind-protect
                (progn
                  (with-current-buffer agenda (superchat-act "first"))
                  (wpers--settled)
                  (with-current-buffer agenda (superchat-act "second"))
                  (wpers--settled)
                  (let* ((id (plist-get (superchat-workspace-personal) :id))
                         (sessions (superchat-view-workspace-session-ids id)))
                    (should (= 1 (length sessions)))
                    (should (= 2 (length (hm--events (car sessions) "intent.received")))))
                  (should (string-match-p "RULE-MARKER-PERSONAL" (wpers--system 1)))
                  (should (string-match-p "RULE-MARKER-PERSONAL" (wpers--system 2))))
              (kill-buffer agenda))))
      (delete-directory dir t))))

(ert-deftest workspace-personal/without-one-the-interface-gets-a-temporary-conversation ()
  (wpers--with-personal '((:text "ok")) (p) nil
    (wpers--write p "AGENTS.md" "RULE-MARKER-P\n")
    (let* ((agenda (wpers--agenda p))
           (view (with-current-buffer agenda (superchat))))
      (unwind-protect
          (progn
            (should-not (buffer-local-value 'superchat--workspace-binding view))
            (hm--turn view "what is on today")
            (should-not (string-match-p "RULE-MARKER-P" (wpers--system 1)))
            (should (string-match-p "This is a temporary conversation"
                                    (wpers--system 1))))
        (kill-buffer agenda)))))

(provide 'workspace-personal-regression)
;;; workspace-personal-regression.el ends here
