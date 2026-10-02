;;; workspace-routing-regression.el --- a file's workspace decides which conversation answers -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of contracts W2 and S2: the AI follows the buffer the key
;; was pressed in.  From a file inside a project, `superchat', `superchat-act'
;; and the in-place commands go to that workspace's main conversation (its
;; most recently active one).  "Project" is simulated with a
;; `project-find-functions' stub.  Load with the prepared ELPA and the pinned
;; llm source, then run `^workspace-routing/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-inline)
(defvar superchat-display-style)

(defvar wr--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wr--finder (dir)
  (cl-loop for root in wr--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wr--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-routing-" t))))

(defmacro wr--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wr--roots nil)
          ,@(mapcar (lambda (v) `(,v (wr--temp-dir))) vars)
          (project-find-functions (list #'wr--finder))
          (superchat-display-style 'editor))
     (setq wr--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wr--file (root &optional name)
  "Return a live buffer visiting ROOT/NAME (default a.txt)."
  (let ((file (expand-file-name (or name "a.txt") root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wr--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(defun wr--binding-id (view)
  (plist-get (buffer-local-value 'superchat--workspace-binding view) :id))

(defun wr--ws-id (root)
  (plist-get (superchat-workspace-of-directory root) :id))

(defun wr--main-session (root)
  (car (superchat-view-workspace-session-ids (wr--ws-id root))))

(defun wr--all-text (wire)
  (mapconcat #'hm--text wire "\n"))

(ert-deftest workspace-routing/a-project-file-opens-the-conversation-of-its-workspace ()
  (wr--with nil (p q)
    (let* ((v1 (with-current-buffer (wr--file p) (superchat)))
           (v2 (with-current-buffer (wr--file q) (superchat)))
           (v1b (with-current-buffer (wr--file p "b.txt") (superchat))))
      (should (equal (wr--binding-id v1) (wr--ws-id p)))
      (should (equal (wr--binding-id v2) (wr--ws-id q)))
      (should-not (eq v1 v2))
      (should-not (equal (hm--session v1) (hm--session v2)))
      (should (eq v1 v1b))
      (should (equal (hm--session v1) (hm--session v1b))))))

(ert-deftest workspace-routing/the-main-conversation-is-the-most-recently-active ()
  (wr--with '((:text "ok")) (p)
    (let* ((src (wr--file p))
           (v1 (with-current-buffer src (superchat)))
           (v3 (with-current-buffer src (superchat-new-session))))
      (should-not (eq v1 v3))
      (should (eq v3 (with-current-buffer src (superchat))))
      (hm--turn v1 "hi")
      (should (eq v1 (with-current-buffer src (superchat)))))))

(ert-deftest workspace-routing/act-goes-to-the-workspace-of-the-file ()
  (wr--with '((:text "ok")) (p q)
    (let* ((vq (with-current-buffer (wr--file q) (superchat)))
           (sq (hm--session vq)))
      (with-current-buffer (wr--file p) (superchat-act "hello"))
      (wr--settled)
      (let ((sp (wr--main-session p)))
        (should sp)
        (should-not (equal sp sq))
        (should (hm--events sp "intent.received"))
        (should-not (hm--events sq "intent.received"))))))

(ert-deftest workspace-routing/in-place-commands-join-the-workspace-of-the-file ()
  (wr--with nil (p q)
    (let* ((vq (with-current-buffer (wr--file q) (superchat)))
           (sq (hm--session vq))
           (src (wr--file p)))
      (let ((a (with-current-buffer src (superchat-inline-session-id)))
            (b (with-current-buffer src (superchat-conversation-session-id)))
            (sp (wr--main-session p)))
        (should sp)
        (should (equal sp a))
        (should (equal sp b))
        (should-not (equal sq a))))))

(ert-deftest workspace-routing/neither-request-carries-the-other-workspace ()
  (wr--with '((:text "ok") (:text "ok") (:text "ok") (:text "ok")) (p q)
    (let* ((srcp (wr--file p))
           (srcq (wr--file q))
           (vp (with-current-buffer srcp (superchat)))
           (vq (with-current-buffer srcq (superchat))))
      (hm--turn vp "P-MARKER-ALPHA")
      (hm--turn vq "Q-MARKER-BETA")
      (with-current-buffer srcp (superchat-act "from p"))
      (wr--settled)
      (with-current-buffer srcq (superchat-act "from q"))
      (wr--settled)
      (let ((p1 (wr--all-text (hm--wire 1)))
            (q1 (wr--all-text (hm--wire 2)))
            (p2 (wr--all-text (hm--wire 3)))
            (q2 (wr--all-text (hm--wire 4))))
        (should-not (string-match-p "Q-MARKER-BETA" p1))
        (should-not (string-match-p "Q-MARKER-BETA" p2))
        (should-not (string-match-p "P-MARKER-ALPHA" q1))
        (should-not (string-match-p "P-MARKER-ALPHA" q2))
        (should (string-match-p "P-MARKER-ALPHA" p2))
        (should (string-match-p "Q-MARKER-BETA" q2))))))

(ert-deftest workspace-routing/a-chat-view-keeps-its-own-conversation ()
  (wr--with nil (p)
    (let ((v1 (with-current-buffer (wr--file p) (superchat))))
      (should (eq v1 (with-current-buffer v1 (superchat)))))))

(ert-deftest workspace-routing/an-unbound-chat-view-behaves-as-before ()
  (wr--with nil (p)
    (let* ((wr--roots nil)
           (view (with-current-buffer (wr--file p) (superchat))))
      (should (eq view (get-buffer superchat-buffer-name)))
      (should-not (buffer-local-value 'superchat--workspace-binding view))
      (should (eq view (with-current-buffer view (superchat)))))))

(ert-deftest workspace-routing/outside-any-project-nothing-changes ()
  (wr--with nil (p)
    (let* ((wr--roots nil)
           (src (wr--file p))
           (view (with-current-buffer src (superchat))))
      (should (eq view (get-buffer superchat-buffer-name)))
      (should (equal (hm--session view)
                     (with-current-buffer src
                       (superchat-conversation-session-id)))))))

(ert-deftest workspace-routing/the-conversation-is-found-again-after-its-view-is-gone ()
  (wr--with '((:text "ok")) (p)
    (let* ((src (wr--file p))
           (v1 (with-current-buffer src (superchat)))
           (session (hm--session v1)))
      (hm--turn v1 "remember-me-7")
      (let ((kill-buffer-query-functions nil))
        (kill-buffer v1))
      (should-not (buffer-live-p v1))
      (let ((again (with-current-buffer src (superchat))))
        (should (equal session (hm--session again)))
        (should (string-match-p
                 "remember-me-7"
                 (with-current-buffer again
                   (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest workspace-routing/the-side-panel-follows-an-in-place-command ()
  (wr--with nil (p q)
    (let ((srcp (wr--file p)) (srcq (wr--file q)))
      (with-current-buffer srcq (superchat))
      (let ((code (cl-find-if-not
                   (lambda (w) (window-parameter w 'window-side))
                   (window-list nil 'no-minibuf))))
        (set-window-buffer code srcp)
        (select-window code))
      (let ((id (with-current-buffer srcp (superchat-conversation-session-id)))
            (shown (cl-some
                    (lambda (w)
                      (and (eq 'right (window-parameter w 'window-side))
                           (window-buffer w)))
                    (window-list nil 'no-minibuf))))
        (should (equal id (wr--main-session p)))
        (should shown)
        (should (equal id (hm--session shown)))))))

(ert-deftest workspace-routing/no-panel-is-opened-for-an-in-place-command ()
  (wr--with nil (p)
    (let ((src (wr--file p))
          (before (length (window-list nil 'no-minibuf))))
      (set-window-buffer (selected-window) src)
      (with-current-buffer src (superchat-conversation-session-id))
      (should (= before (length (window-list nil 'no-minibuf))))
      (should-not (cl-some
                   (lambda (w) (buffer-local-value 'superchat--session-id
                                                   (window-buffer w)))
                   (window-list nil 'no-minibuf))))))

(ert-deftest workspace-routing/slash-workspace-opens-the-conversation-of-the-declared-workspace ()
  (wr--with '((:text "ok")) ()
    (let* ((wr--roots nil)
           (d (wr--temp-dir))
           (view (smoke--open-chat))
           (calls nil))
      (unwind-protect
          (progn
            (hm--turn view "hi")
            (cl-letf (((symbol-function 'run-at-time)
                       (lambda (&rest args) (push args calls) nil)))
              (with-current-buffer view
                (superchat--cmd-workspace "workspace" d nil nil nil)))
            (should (= 1 (length calls)))
            (let ((args (car calls)))
              (should (eq 'superchat--open-workspace-conversation (nth 2 args)))
              (let ((ws (nth 3 args)))
                (should (equal (superchat-workspace-read-id d) (plist-get ws :id)))
                (superchat--open-workspace-conversation ws)
                (should (superchat--chat-view-p (current-buffer)))
                (should (equal (plist-get ws :id)
                               (wr--binding-id (current-buffer)))))))
        (delete-directory d t)))))

(provide 'workspace-routing-regression)
;;; workspace-routing-regression.el ends here
