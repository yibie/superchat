;;; workspace-session-regression.el --- a conversation's workspace is fixed at creation -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline tests of contract S1: a conversation belongs to one workspace,
;; decided when it is created and never changed afterwards.  "Project" is
;; simulated with a `project-find-functions' stub, so no git is needed.
;; Load with the prepared ELPA and the pinned llm source, then run
;; `^workspace-session/' via ERT batch.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(when (getenv "SMOKE_REAL")
  (error "Workspace-session regression is offline; unset SMOKE_REAL"))
(defvar smoke-library-only)
(setq smoke-library-only t)
(load (expand-file-name "smoke.el" (file-name-directory load-file-name)) nil t)

(defvar workspace-session--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun workspace-session--finder (dir)
  "Stub finder: the root in `workspace-session--roots' containing DIR."
  (cl-loop for root in workspace-session--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun workspace-session--temp-dir ()
  "Return a fresh real-path temporary directory."
  (file-name-as-directory
   (file-truename (make-temp-file "ws-session-" t))))

(defmacro workspace-session--with (script vars &rest body)
  "Run BODY in the smoke fixture with SCRIPT.
VARS names symbols bound to fresh project roots (stubbed as projects),
each holding an `a.txt'; `src-VAR' style sources are made by
`workspace-session--source'.  Temporary directories are deleted after."
  (declare (indent 2) (debug t))
  `(let* ((workspace-session--roots nil)
          ,@(mapcar (lambda (v) `(,v (workspace-session--temp-dir))) vars)
          (project-find-functions (list #'workspace-session--finder))
          (superchat-runtime--session-queues (make-hash-table :test #'equal))
          (superchat-runtime--queue-timers (make-hash-table :test #'equal))
          (superchat-event-faults nil)
          (superchat-llm-reasoning nil)
          (superchat-llm-tool-names superchat-llm-default-tool-names)
          (superchat-llm-tools-list nil))
     (setq workspace-session--roots (list ,@vars))
     (unwind-protect
         (smoke--with-fixture ,script ,@body)
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun workspace-session--source (root)
  "Return a live file buffer for ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun workspace-session--bound-events (session)
  "Return the `workspace.bound' events on SESSION's Tape."
  (cl-remove-if-not (lambda (e) (equal (plist-get e :type) "workspace.bound"))
                    (superchat-tape-replay-events session)))

(defun workspace-session--session (view)
  (buffer-local-value 'superchat--session-id view))

(defun workspace-session--binding (view)
  (buffer-local-value 'superchat--workspace-binding view))

(defun workspace-session--round (view text)
  "Send TEXT from VIEW and wait for the run to finish."
  (let ((run (smoke--send view text)))
    (should (smoke--wait (lambda () (smoke--run-done-p run)) 20))))

(defun workspace-session--slash (view args)
  "Run /workspace ARGS in VIEW and return the echo content."
  (with-current-buffer view
    (plist-get (superchat--cmd-workspace "workspace" args nil nil nil) :content)))

(defun workspace-session--no-dot-superchat (dir)
  (should-not (file-exists-p (expand-file-name ".superchat" dir))))

(ert-deftest workspace-session/a-new-conversation-from-a-project-file-is-bound-to-its-workspace ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat)))
           (ws (superchat-workspace-of-directory p))
           (binding (workspace-session--binding view)))
      (unwind-protect
          (progn
            (should binding)
            (should (equal (plist-get binding :id) (plist-get ws :id)))
            (should (equal (plist-get binding :root) (plist-get ws :root)))
            (should (= 1 (length (workspace-session--bound-events
                                  (workspace-session--session view)))))
            (workspace-session--no-dot-superchat p))
        (kill-buffer src)))))

(ert-deftest workspace-session/a-new-conversation-outside-any-project-is-not-bound ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((workspace-session--roots nil)
           (src (workspace-session--source p))
           (view (with-current-buffer src (superchat))))
      (unwind-protect
          (progn
            (should-not (workspace-session--binding view))
            (should-not (workspace-session--bound-events
                         (workspace-session--session view))))
        (kill-buffer src)))))

(ert-deftest workspace-session/binding-happens-once ()
  (workspace-session--with '((:text "ok")) (p q)
    (let* ((src (workspace-session--source p))
           (src2 (workspace-session--source q))
           (view (with-current-buffer src (superchat)))
           (first (workspace-session--binding view)))
      (unwind-protect
          (progn
            (with-current-buffer src (superchat))
            (with-current-buffer src2 (superchat))
            (should (= 1 (length (workspace-session--bound-events
                                  (workspace-session--session view)))))
            (should (equal first (workspace-session--binding view))))
        (kill-buffer src)
        (kill-buffer src2)))))

(ert-deftest workspace-session/a-conversation-with-history-is-not-bound-later ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((workspace-session--roots nil)
           (src (workspace-session--source p))
           (view (smoke--open-chat)))
      (unwind-protect
          (progn
            (workspace-session--round view "hi")
            (setq workspace-session--roots (list p))
            (should-not (superchat--session-bind-workspace-at-creation view src))
            (should-not (workspace-session--bound-events
                         (workspace-session--session view)))
            (should-not (workspace-session--binding view)))
        (kill-buffer src)))))

(ert-deftest workspace-session/new-session-binds-too ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat-new-session)))
           (ws (superchat-workspace-of-directory p)))
      (unwind-protect
          (progn
            (should (equal (plist-get (workspace-session--binding view) :id)
                           (plist-get ws :id)))
            (should (= 1 (length (workspace-session--bound-events
                                  (workspace-session--session view))))))
        (kill-buffer src)))))

(ert-deftest workspace-session/act-from-a-project-file-binds-before-the-first-turn ()
  (workspace-session--with '((:text "ok")) (p)
    (let ((src (workspace-session--source p)))
      (unwind-protect
          (let* ((run (with-current-buffer src (superchat-act "hello")))
                 (view (get-buffer superchat-buffer-name))
                 (session (workspace-session--session view))
                 (types nil))
            (ignore run)
            (should (smoke--wait
                     (lambda () (null (superchat-runtime-active-runs))) 20))
            (setq types (mapcar (lambda (e) (plist-get e :type))
                                (superchat-tape-replay-events session)))
            (should (member "workspace.bound" types))
            (should (member "intent.received" types))
            (should (< (cl-position "workspace.bound" types :test #'equal)
                       (cl-position "intent.received" types :test #'equal))))
        (kill-buffer src)))))

(ert-deftest workspace-session/a-branch-inherits-the-workspace-of-its-parent ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat))))
      (unwind-protect
          (progn
            (workspace-session--round view "hi")
            (let* ((parent-id (plist-get (workspace-session--binding view) :id))
                   (child (with-current-buffer view (superchat-branch-session nil)))
                   (child-session (workspace-session--session child))
                   (events (workspace-session--bound-events child-session)))
              (should-not (equal child-session (workspace-session--session view)))
              (should (= 1 (length events)))
              (should (equal parent-id
                             (plist-get (plist-get (car events) :payload)
                                        :workspace-id)))
              (should (equal parent-id
                             (plist-get (workspace-session--binding child) :id)))))
        (kill-buffer src)))))

(ert-deftest workspace-session/slash-workspace-in-a-fresh-conversation-declares-and-binds ()
  (workspace-session--with '((:text "ok")) ()
    (let* ((workspace-session--roots nil)
           (d (workspace-session--temp-dir))
           (view (smoke--open-chat)))
      (unwind-protect
          (progn
            (workspace-session--slash view d)
            (let ((id-file (expand-file-name ".superchat/id" d)))
              (should (file-exists-p id-file))
              (should (equal (string-trim
                              (with-temp-buffer (insert-file-contents id-file)
                                                (buffer-string)))
                             (plist-get (workspace-session--binding view) :id))))
            (should (= 1 (length (workspace-session--bound-events
                                  (workspace-session--session view))))))
        (delete-directory d t)))))

(ert-deftest workspace-session/slash-workspace-does-not-move-a-conversation-with-history ()
  (workspace-session--with '((:text "ok")) ()
    (let* ((workspace-session--roots nil)
           (d (workspace-session--temp-dir))
           (view (smoke--open-chat)))
      (unwind-protect
          (progn
            (workspace-session--round view "hi")
            (let ((before (workspace-session--binding view))
                  (echo (workspace-session--slash view d)))
              (should (equal before (workspace-session--binding view)))
              (should-not (workspace-session--bound-events
                           (workspace-session--session view)))
              (should (file-exists-p (expand-file-name ".superchat/id" d)))
              (should (string-match-p "stays" echo))))
        (delete-directory d t)))))

(ert-deftest workspace-session/slash-workspace-clear-only-in-a-fresh-conversation ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat))))
      (unwind-protect
          (progn
            (should (workspace-session--binding view))
            (workspace-session--slash view "clear")
            (should-not (workspace-session--binding view)))
        (kill-buffer src)))
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat))))
      (unwind-protect
          (progn
            (workspace-session--round view "hi")
            (let ((echo (workspace-session--slash view "clear")))
              (should (workspace-session--binding view))
              (should (string-match-p "stays" echo))))
        (kill-buffer src)))))

(ert-deftest workspace-session/slash-workspace-on-a-subdirectory-binds-the-project-root ()
  (workspace-session--with '((:text "ok")) (p)
    (let ((sub (expand-file-name "sub/" p))
          (view (smoke--open-chat)))
      (make-directory sub t)
      (workspace-session--slash view sub)
      (should (equal (plist-get (workspace-session--binding view) :root) p))
      (should (file-exists-p (expand-file-name ".superchat/id" p)))
      (should-not (file-exists-p (expand-file-name ".superchat/id" sub))))))

(ert-deftest workspace-session/reopening-a-bound-conversation-restores-its-binding ()
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (view (with-current-buffer src (superchat))))
      (unwind-protect
          (progn
            (workspace-session--round view "hi")
            (let* ((before (workspace-session--binding view))
                   (session (workspace-session--session view))
                   (again (superchat--render-session-view session))
                   (after (workspace-session--binding again)))
              (should after)
              (should (equal (plist-get before :id) (plist-get after :id)))
              (should (equal (plist-get before :root) (plist-get after :root)))))
        (kill-buffer src)))))

(ert-deftest workspace-session/slash-workspace-leaves-a-fresh-conversation-bound-elsewhere-alone ()
  "A conversation bound at creation stays in that Workspace even before its
first turn; naming its own Workspace again records nothing."
  (workspace-session--with '((:text "ok")) (p)
    (let* ((src (workspace-session--source p))
           (d (workspace-session--temp-dir))
           (view (with-current-buffer src (superchat)))
           (before (workspace-session--binding view)))
      (unwind-protect
          (let ((echo (workspace-session--slash view d)))
            (should (string-match-p "stays" echo))
            (should (equal before (workspace-session--binding view)))
            (should (file-exists-p (expand-file-name ".superchat/id" d)))
            (should (string-match-p (regexp-quote (plist-get before :root))
                                    (workspace-session--slash view p)))
            (should (= 1 (length (workspace-session--bound-events
                                  (workspace-session--session view))))))
        (kill-buffer src)
        (delete-directory d t)))))

(ert-deftest workspace-session/the-binding-of-a-directory-is-its-workspace ()
  "`superchat--workspace-binding-for-directory': inside a project the root
is the project's, not the directory given; outside one the directory is the
root and an id file wins over the path."
  (workspace-session--with nil (p)
    (let ((sub (expand-file-name "deep/er/" p))
          (plain (workspace-session--temp-dir)))
      (unwind-protect
          (progn
            (make-directory sub t)
            (let ((binding (superchat--workspace-binding-for-directory sub)))
              (should (equal p (plist-get binding :root)))
              (should (equal (plist-get (superchat-workspace-of-directory p) :id)
                             (plist-get binding :id))))
            (let ((binding (superchat--workspace-binding-for-directory plain)))
              (should (equal plain (plist-get binding :root)))
              (should (equal (superchat-workspace-id-from-root plain)
                             (plist-get binding :id))))
            (make-directory (expand-file-name ".superchat" plain) t)
            (with-temp-file (expand-file-name ".superchat/id" plain)
              (insert "kept-id\n"))
            (should (equal "kept-id"
                           (plist-get (superchat--workspace-binding-for-directory plain)
                                      :id)))
            (should-not (file-exists-p (expand-file-name ".superchat" p))))
        (delete-directory plain t)))))

(provide 'workspace-session-regression)
;;; workspace-session-regression.el ends here
