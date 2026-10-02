;;; workspace-temporary-regression.el --- outside any project the workspace is temporary -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of contract W10: a buffer in no project gets a temporary
;; workspace: one conversation, no directory, no `.superchat', no tab group.
;; A temporary conversation is one without a `workspace.bound' fact.  One
;; temporary use is bounded by visibility: a temporary conversation showing
;; in the frame is continued, else a new one opens.  Run
;; `^workspace-temporary/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defvar superchat-display-style)

(defvar wt--roots nil)

(defun wt--finder (dir)
  (cl-loop for root in wt--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wt--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-temp-" t))))

(defun wt--reset-frame ()
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window)))
  (delete-other-windows))

(defmacro wt--with (script vars &rest body)
  "Run BODY in the history fixture; VARS are fresh directories.
Directories named in `wt--roots' (set by BODY) are projects."
  (declare (indent 2) (debug t))
  `(let* ((wt--roots nil)
          ,@(mapcar (lambda (v) `(,v (wt--temp-dir))) vars)
          (project-find-functions (list #'wt--finder))
          (superchat-display-style 'editor))
     (wt--reset-frame)
     (unwind-protect
         (hm--fixture ,script ,@body)
       (wt--reset-frame)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wt--file (dir &optional name)
  (let ((file (expand-file-name (or name "a.txt") dir)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wt--hide (&optional keep)
  "Leave no window showing any chat View; show KEEP (a buffer) instead."
  (wt--reset-frame)
  (switch-to-buffer (or keep (get-buffer-create "*wt-plain*"))))

(defun wt--kill (view)
  (let ((kill-buffer-query-functions nil))
    (kill-buffer view))
  (should-not (buffer-live-p view)))

(defun wt--shown-p (view)
  (get-buffer-window view t))

(ert-deftest workspace-temporary/a-visible-temporary-conversation-is-continued ()
  (wt--with nil (d)
    (let* ((t1 (with-current-buffer (wt--file d "a.txt") (superchat)))
           (t1b (with-current-buffer (wt--file d "b.txt") (superchat))))
      (should (wt--shown-p t1))
      (should (eq t1 t1b)))))

(ert-deftest workspace-temporary/a-new-one-opens-when-the-old-one-is-closed ()
  (wt--with '((:text "ok")) (d)
    (let* ((src (wt--file d))
           (t1 (with-current-buffer src (superchat)))
           (s1 (hm--session t1)))
      (hm--turn t1 "hi")
      (wt--hide src)
      (wt--kill t1)
      (let ((t2 (with-current-buffer src (superchat))))
        (should-not (equal s1 (hm--session t2)))
        (should (hm--events s1 "intent.received"))))))

(ert-deftest workspace-temporary/a-hidden-temporary-conversation-is-still-continued ()
  (wt--with nil (d)
    (let* ((src (wt--file d))
           (t1 (with-current-buffer src (superchat))))
      (wt--hide src)
      (should-not (wt--shown-p t1))
      (should (eq t1 (with-current-buffer src (superchat))))
      (wt--hide src)
      (should (eq (with-current-buffer src (superchat--ensure-request-chat-buffer))
                  (with-current-buffer src (superchat--ensure-request-chat-buffer))))
      (should (eq t1 (with-current-buffer src (superchat--ensure-request-chat-buffer)))))))

(ert-deftest workspace-temporary/the-two-do-not-share-history ()
  (wt--with '((:text "ok") (:text "ok")) (d)
    (let* ((src (wt--file d))
           (t1 (with-current-buffer src (superchat))))
      (hm--turn t1 "TMP-ONE-MARK")
      (wt--hide src)
      (wt--kill t1)
      (let ((t2 (with-current-buffer src (superchat))))
        (hm--turn t2 "second question")
        (should-not (string-match-p
                     "TMP-ONE-MARK"
                     (mapconcat #'hm--text (hm--wire 2) "\n")))))))

(ert-deftest workspace-temporary/an-in-place-command-joins-the-visible-one-else-a-new-one ()
  (wt--with nil (d)
    (let* ((src (wt--file d))
           (src2 (wt--file d "b.txt"))
           (t1 (with-current-buffer src (superchat))))
      (should (equal (hm--session t1)
                     (with-current-buffer src2 (superchat-conversation-session-id))))
      (wt--hide src)
      (wt--kill t1)
      (let* ((windows (length (window-list nil 'no-minibuf)))
             (id (with-current-buffer src2 (superchat-conversation-session-id))))
        (should (stringp id))
        (should-not (equal id (hm--session t1)))
        (should (= windows (length (window-list nil 'no-minibuf))))))))

(ert-deftest workspace-temporary/a-temporary-conversation-records-no-workspace ()
  (wt--with '((:text "ok")) (d)
    (let* ((src (wt--file d))
           (t1 (with-current-buffer src (superchat))))
      (hm--turn t1 "hi")
      (should-not (hm--events (hm--session t1) "workspace.bound"))
      (should-not (file-exists-p (expand-file-name ".superchat" d))))))

(ert-deftest workspace-temporary/a-project-conversation-is-never-taken-for-temporary ()
  (wt--with nil (p d)
    (setq wt--roots (list p))
    (let* ((vp (with-current-buffer (wt--file p) (superchat)))
           (vt (with-current-buffer (wt--file d) (superchat))))
      (should (buffer-local-value 'superchat--workspace-binding vp))
      (should-not (eq vp vt))
      (should-not (buffer-local-value 'superchat--workspace-binding vt)))))

(ert-deftest workspace-temporary/inside-a-temporary-view-superchat-stays ()
  (wt--with nil (d)
    (let ((t1 (with-current-buffer (wt--file d) (superchat))))
      (should (eq t1 (with-current-buffer t1 (superchat)))))))

(provide 'workspace-temporary-regression)
;;; workspace-temporary-regression.el ends here
