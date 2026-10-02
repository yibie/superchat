;;; workspace-screen-regression.el --- the screen shows a conversation's workspace -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of contract W7: the chat window's header names the
;; conversation's workspace, and a conversation that has a tab of its own
;; sits in the tab group of that name.  "Project" is a
;; `project-find-functions' stub.  Run `^workspace-screen/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(defvar superchat-display-style)

(defvar ws--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun ws--finder (dir)
  (cl-loop for root in ws--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun ws--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-screen-" t))))

(defun ws--reset-frame ()
  "Leave the frame with one untagged, ungrouped tab and one ordinary window."
  (let ((tab-bar-tab-prevent-close-functions nil))
    (tab-bar-close-other-tabs))
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (superchat-editor-workspace--clear-tab-session-id tab)
      (setf (alist-get 'group (cdr tab)) nil))
    (tab-bar-tabs-set tabs))
  (dolist (window (window-list nil 'no-minibuf))
    (when (and (window-live-p window) (window-parameter window 'window-side))
      (delete-window window)))
  (delete-other-windows))

(defmacro ws--with (script style vars &rest body)
  "Run BODY in the history fixture with SCRIPT and display STYLE.
VARS are fresh project roots."
  (declare (indent 3) (debug t))
  `(let* ((ws--roots nil)
          ,@(mapcar (lambda (v) `(,v (ws--temp-dir))) vars)
          (project-find-functions (list #'ws--finder))
          (superchat-display-style ,style))
     (setq ws--roots (list ,@vars))
     (ws--reset-frame)
     (unwind-protect
         (hm--fixture ,script ,@body)
       (ws--reset-frame)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun ws--file (root)
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun ws--name (root)
  (file-name-nondirectory (directory-file-name root)))

(defun ws--header (view)
  (substring-no-properties
   (with-current-buffer view (superchat--header-line-run-state))))

(defun ws--group ()
  (alist-get 'group (cdr (superchat-editor-workspace--current-tab))))

(ert-deftest workspace-screen/the-header-names-the-workspace ()
  (ws--with nil 'editor (p)
    (let ((view (with-current-buffer (ws--file p) (superchat))))
      (should (string-match-p (concat "Superchat · " (regexp-quote (ws--name p)))
                              (ws--header view))))))

(ert-deftest workspace-screen/the-header-of-an-unbound-view-is-unchanged ()
  (ws--with nil 'editor (p)
    (let* ((ws--roots nil)
           (view (with-current-buffer (ws--file p) (superchat)))
           (header (ws--header view)))
      (should-not (buffer-local-value 'superchat--workspace-binding view))
      (should-not (string-match-p (regexp-quote (ws--name p)) header))
      (should (string-match-p "Superchat · smoke-model" header))
      (should (equal header
                     (with-current-buffer view
                       (let ((superchat--workspace-binding nil))
                         (substring-no-properties
                          (superchat--header-line-run-state)))))))))

(ert-deftest workspace-screen/a-reopened-conversation-still-names-its-workspace ()
  (ws--with '((:text "ok")) 'editor (p)
    (let* ((view (with-current-buffer (ws--file p) (superchat))))
      (hm--turn view "hi")
      (let ((again (superchat--render-session-view (hm--session view))))
        (should (equal (ws--name p)
                       (plist-get (buffer-local-value 'superchat--workspace-binding
                                                      again)
                                  :name)))
        (should (string-match-p (concat "Superchat · " (regexp-quote (ws--name p)))
                                (ws--header again)))))))

(ert-deftest workspace-screen/the-name-is-not-stored-on-the-tape ()
  (ws--with nil 'editor (p)
    (let* ((view (with-current-buffer (ws--file p) (superchat)))
           (events (hm--events (hm--session view) "workspace.bound")))
      (should (= 1 (length events)))
      (let ((payload (plist-get (car events) :payload)))
        (should (= 4 (length payload)))
        (should (plist-member payload :workspace-id))
        (should (plist-member payload :workspace-root))))))

(ert-deftest workspace-screen/the-conversation-tab-joins-the-tab-group-of-its-workspace ()
  (ws--with nil 'agent (p)
    (with-current-buffer (ws--file p) (superchat))
    (should (equal (ws--name p) (ws--group)))))

(ert-deftest workspace-screen/the-tab-of-an-unbound-conversation-keeps-its-group ()
  (ws--with nil 'agent (p)
    (let* ((ws--roots nil)
           (before (ws--group)))
      (with-current-buffer (ws--file p) (superchat))
      (should (equal before (ws--group))))))

(ert-deftest workspace-screen/beside-the-code-the-tab-group-is-left-alone ()
  (ws--with nil 'editor (p)
    (let ((before (ws--group)))
      (with-current-buffer (ws--file p) (superchat))
      (should (equal before (ws--group))))))

(ert-deftest workspace-screen/the-header-does-not-look-up-the-project ()
  (ws--with nil 'editor (p)
    (let ((view (with-current-buffer (ws--file p) (superchat))))
      (cl-letf (((symbol-function 'project-current)
                 (lambda (&rest _) (error "project-current must not be called"))))
        (should (string-match-p (concat "Superchat · " (regexp-quote (ws--name p)))
                                (ws--header view)))))))

(provide 'workspace-screen-regression)
;;; workspace-screen-regression.el ends here
