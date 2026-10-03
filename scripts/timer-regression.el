;;; timer-regression.el --- timers and reminders that belong to a workspace -*- lexical-binding: t; -*-

;;; Commentary:
;; Offline coverage for slice 4 of M11
;; (docs/decisions/accepted/2026-10-02-m11-slice-4-timers-and-reminders.md,
;; contract T1-T3): a timer is kept as facts on the Tape and so survives a
;; restart; one missed while Emacs was closed fires once, a repeating one
;; only its latest occurrence; a reminder tells the user and leaves a line
;; in the Workspace's main conversation without starting the model; a task
;; starts the model there.  Time is moved with `superchat-timer-now-function'
;; and timers are fired by calling the module, never by waiting.
;;
;; Run from the repository root:
;;   emacs -Q --batch \
;;     --eval "(progn (require 'package) (setq package-user-dir \"$HOME/.cache/superchat-test-elpa\" native-comp-enable-subr-trampolines nil) (package-initialize))" \
;;     -L . -L plugins/superchat-cowork-telega \
;;     -l scripts/timer-regression.el \
;;     --eval '(ert-run-tests-batch-and-exit "^timer/")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(load (expand-file-name "history-messages-regression.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'superchat-workspace)
(require 'superchat-timer)
(defvar superchat-display-style)
(defvar superchat--tool-execution-context)

(defvar wtimer--roots nil
  "Directories the stub project finder recognises as project roots.")

(defun wtimer--finder (dir)
  (cl-loop for root in wtimer--roots
           when (file-in-directory-p dir root)
           return (cons 'transient (file-name-as-directory root))))

(defun wtimer--temp-dir ()
  (file-name-as-directory (file-truename (make-temp-file "ws-timer-" t))))

(defmacro wtimer--with (script vars &rest body)
  "Run BODY in the history fixture with SCRIPT; VARS are fresh project roots."
  (declare (indent 2) (debug t))
  `(let* ((wtimer--roots nil)
          ,@(mapcar (lambda (v) `(,v (wtimer--temp-dir))) vars)
          (project-find-functions (list #'wtimer--finder))
          (superchat-display-style 'editor)
          (wtimer--now 1800000000.0)
          (wtimer--told nil)
          (superchat-timer-now-function (lambda () wtimer--now))
          (superchat-timer-notify-function (lambda (fired) (push fired wtimer--told))))
     (setq wtimer--roots (list ,@vars))
     (unwind-protect
         (hm--fixture ,script ,@body)
       (superchat-timer-disarm-all)
       (dolist (b (buffer-list))
         (when-let* ((f (buffer-file-name b)))
           (when (cl-some (lambda (d) (string-prefix-p d f)) (list ,@vars))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b))))
       (dolist (d (list ,@vars)) (ignore-errors (delete-directory d t))))))

(defun wtimer--write (root name content)
  "Write CONTENT to the file NAME under ROOT."
  (with-temp-file (expand-file-name name root) (insert content)))

(defun wtimer--file (root)
  "Return a live buffer visiting ROOT/a.txt."
  (let ((file (expand-file-name "a.txt" root)))
    (with-temp-file file (insert "hello\n"))
    (find-file-noselect file)))

(defun wtimer--system (n)
  "The system text of the Nth provider request."
  (let ((first (car (hm--wire n))))
    (should (equal "system" (alist-get 'role first)))
    (hm--text first)))

(defun wtimer--settled ()
  (should (smoke--wait (lambda () (null (superchat-runtime-active-runs))) 20)))

(defvar wtimer--now nil "The time the module sees, in a test.")
(defvar wtimer--told nil "What the notify function was given, newest first.")

(defun wtimer--session (view) (hm--session view))

(defun wtimer--ids () (mapcar (lambda (timer) (plist-get timer :id)) (superchat-timer-list)))

(defun wtimer--fired-in (session)
  (mapcar (lambda (event) (plist-get (plist-get event :payload) :text))
          (hm--events session "timer.fired")))

;;;; Setting a timer --------------------------------------------------------------------

(ert-deftest timer/the-three-facts-are-known-tape-events ()
  (dolist (type '("timer.created" "timer.fired" "timer.cancelled"))
    (should (eq 'session-annotation (superchat-tape-event-relevance type)))))

(ert-deftest timer/a-timer-belongs-to-the-workspace-of-its-conversation ()
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (wtimer--session view))
           (timer (superchat-timer-create session 'reminder "call mom"
                                          (+ wtimer--now 1200))))
      (should (equal (plist-get (superchat-workspace-of-directory p) :id)
                     (plist-get timer :workspace-id)))
      (should (equal p (plist-get timer :workspace-root)))
      (let ((listed (car (superchat-timer-list))))
        (should (equal (plist-get timer :id) (plist-get listed :id)))
        (should (equal "call mom" (plist-get listed :text)))
        (should (eq 'reminder (plist-get listed :kind)))
        (should (= (+ wtimer--now 1200) (plist-get listed :next-due)))
        (should (equal session (plist-get listed :session-id))))
      (should (= 1 (length (hm--events session "timer.created")))))))

(ert-deftest timer/a-timer-survives-a-restart ()
  "What is pending is read back from the Tape alone."
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (id (plist-get (superchat-timer-create (wtimer--session view) 'reminder
                                                  "call mom" (+ wtimer--now 1200))
                          :id)))
      (superchat-timer-disarm-all)
      (kill-buffer view)
      (should (equal (list id) (wtimer--ids))))))

(ert-deftest timer/bad-timers-are-refused ()
  (should-error (superchat-timer-create nil 'reminder "x" 1.0))
  (should-error (superchat-timer-create "s" 'nag "x" 1.0))
  (should-error (superchat-timer-create "s" 'reminder "  " 1.0))
  (should-error (superchat-timer-create "s" 'reminder "x" "soon"))
  (should-error (superchat-timer-create "s" 'reminder "x" 1.0 30)))

;;;; Coming due -------------------------------------------------------------------------

(ert-deftest timer/a-reminder-tells-the-user-and-leaves-a-line-without-the-model ()
  (wtimer--with nil (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (wtimer--session view)))
      (superchat-timer-create session 'reminder "call mom" (+ wtimer--now 1200))
      (setq wtimer--now (+ wtimer--now 1200))
      (let ((fired (superchat-timer-catch-up)))
        (should (= 1 (length fired)))
        (should-not (plist-get (car fired) :late)))
      (should (equal '("call mom") (mapcar (lambda (f) (plist-get f :text)) wtimer--told)))
      (should (equal '("call mom") (wtimer--fired-in session)))
      (should (string-match-p "Reminder: call mom"
                              (with-current-buffer view (buffer-string))))
      ;; No model request was made, and the one-shot timer is done.
      (should-not hm--wires)
      (should-not (superchat-timer-list))
      (should-not (superchat-timer-catch-up)))))

(ert-deftest timer/the-line-goes-to-the-main-conversation-of-the-workspace ()
  (wtimer--with '((:text "ok")) (p)
    (let* ((src (wtimer--file p))
           (v1 (with-current-buffer src (superchat)))
           (s1 (wtimer--session v1)))
      (superchat-timer-create s1 'reminder "call mom" (+ wtimer--now 60))
      (let* ((v2 (with-current-buffer src (superchat-new-session)))
             (s2 (wtimer--session v2)))
        (hm--turn v2 "the newer conversation")
        (setq wtimer--now (+ wtimer--now 60))
        (superchat-timer-catch-up)
        (should (equal '("call mom") (wtimer--fired-in s2)))
        (should-not (wtimer--fired-in s1))))))

(ert-deftest timer/a-temporary-conversation-hears-its-own-timer ()
  (wtimer--with nil ()
    (let* ((view (smoke--open-chat))
           (session (wtimer--session view))
           (timer (superchat-timer-create session 'reminder "stretch" (+ wtimer--now 60))))
      (should-not (plist-get timer :workspace-id))
      (setq wtimer--now (+ wtimer--now 60))
      (superchat-timer-catch-up)
      (should (equal '("stretch") (wtimer--fired-in session))))))

(ert-deftest timer/a-task-starts-the-model-in-the-main-conversation ()
  (wtimer--with '((:text "TASK-ANSWER")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (wtimer--session view)))
      (superchat-timer-create session 'task "TASK-MARKER list today's agenda"
                              (+ wtimer--now 60))
      (setq wtimer--now (+ wtimer--now 60))
      (let ((fired (car (superchat-timer-catch-up))))
        (should (stringp (plist-get fired :run-id)))
        (wtimer--settled))
      (should (= 1 (length hm--wires)))
      (should (string-match-p "TASK-MARKER"
                              (mapconcat #'hm--text (hm--wire 1) "\n")))
      (should (= 1 (length (hm--events session "intent.received"))))
      (should (equal '("TASK-MARKER list today's agenda") (wtimer--fired-in session))))))

(ert-deftest timer/the-notes-never-reach-the-model ()
  (wtimer--with '((:text "ok")) (p)
    (let* ((view (with-current-buffer (wtimer--file p) (superchat)))
           (session (wtimer--session view)))
      (superchat-timer-create session 'reminder "NOTE-MARKER" (+ wtimer--now 60))
      (setq wtimer--now (+ wtimer--now 60))
      (superchat-timer-catch-up)
      (hm--turn view "hello")
      (should-not (string-match-p "NOTE-MARKER"
                                  (mapconcat #'hm--text (hm--wire 1) "\n"))))))

;;;; Missed while Emacs was closed ----------------------------------------------------

(ert-deftest timer/a-missed-timer-fires-once-and-says-it-is-late ()
  (wtimer--with nil (p)
    (let ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat)))))
      (superchat-timer-create session 'reminder "call mom" (+ wtimer--now 60))
      (setq wtimer--now (+ wtimer--now 7200))
      (let ((fired (superchat-timer-catch-up)))
        (should (= 1 (length fired)))
        (should (plist-get (car fired) :late)))
      (should-not (superchat-timer-catch-up))
      (should (= 1 (length (wtimer--fired-in session)))))))

(ert-deftest timer/a-repeating-timer-fires-only-its-latest-missed-occurrence ()
  (wtimer--with nil (p)
    (let* ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat))))
           (start wtimer--now)
           (day 86400))
      (superchat-timer-create session 'reminder "water the plants" start day)
      (setq wtimer--now (+ start (* 3.5 day)))
      (let ((fired (superchat-timer-catch-up)))
        (should (= 1 (length fired)))
        (should (= (+ start (* 3 day)) (plist-get (car fired) :due))))
      (should-not (superchat-timer-catch-up))
      (let ((pending (car (superchat-timer-list))))
        (should (= (+ start (* 4 day)) (plist-get pending :next-due))))
      (setq wtimer--now (+ start (* 4 day)))
      (should (= (+ start (* 4 day))
                 (plist-get (car (superchat-timer-catch-up)) :due))))))

;;;; Cancelling and arming --------------------------------------------------------------

(ert-deftest timer/a-cancelled-timer-never-fires ()
  (wtimer--with nil (p)
    (let* ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat))))
           (id (plist-get (superchat-timer-create session 'reminder "call mom"
                                                  (+ wtimer--now 60))
                          :id)))
      (should (superchat-timer-cancel id))
      (should-not (superchat-timer-cancel id))
      (should-not (superchat-timer-cancel "timer-none"))
      (should-not (superchat-timer-list))
      (setq wtimer--now (+ wtimer--now 120))
      (should-not (superchat-timer-catch-up))
      (should-not wtimer--told))))

(ert-deftest timer/the-row-arms-and-disarms ()
  (wtimer--with nil (p)
    (let* ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat))))
           (id (plist-get (superchat-timer-create session 'reminder "call mom"
                                                  (+ wtimer--now 600))
                          :id)))
      (should (memq 'timers superchat-plugins))
      (should (timerp (gethash id superchat-timer--armed)))
      (unwind-protect
          (progn
            (superchat-plugin-unmount 'timers)
            (should (= 0 (hash-table-count superchat-timer--armed))))
        (superchat-plugin-mount 'timers))
      ;; Mounting arms after loading finishes.
      (should (smoke--wait (lambda () (gethash id superchat-timer--armed)) 5))
      ;; The Emacs timer going off fires the reminder.
      (setq wtimer--now (+ wtimer--now 600))
      (superchat-timer--due id)
      (should (equal '("call mom") (wtimer--fired-in session)))
      (should-not (gethash id superchat-timer--armed)))))

;;;; The tools --------------------------------------------------------------------------

(ert-deftest timer/the-tools-are-on-the-default-surface-and-built-in ()
  (dolist (name '("timer_create" "timer_list" "timer_cancel"))
    (should (member name superchat-llm-default-tool-names))
    (should (eq 'builtin (superchat-sandbox-tool-class name)))))

(ert-deftest timer/the-create-tool-sets-a-timer-in-the-asking-conversation ()
  (wtimer--with nil (p)
    (let* ((session (wtimer--session (with-current-buffer (wtimer--file p) (superchat))))
           (superchat--tool-execution-context (list :session-id session)))
      (let ((result (superchat-timer-tool-create "给妈妈打电话" "reminder" 20)))
        (should (string-match-p "the user will be reminded at " result))
        (should (string-match-p (regexp-quote (superchat-timer-format-time (+ wtimer--now 1200)))
                                result)))
      (let ((timer (car (superchat-timer-list))))
        (should (equal session (plist-get timer :session-id)))
        (should (= (+ wtimer--now 1200) (plist-get timer :next-due))))
      (should (string-match-p "给妈妈打电话" (superchat-timer-tool-list)))
      (let ((at (superchat-timer-format-time (+ wtimer--now 86400))))
        (should (string-match-p "the task will run"
                                (superchat-timer-tool-create "agenda" "task" nil at "daily")))
        (should (= (superchat-timer-parse-time at)
                   (plist-get (cadr (superchat-timer-list)) :due))))
      (should (string-prefix-p "Error:" (superchat-timer-tool-create "x" "reminder")))
      (should (string-prefix-p "Error:" (superchat-timer-tool-create "x" "nag" 5)))
      (should (string-prefix-p "Error:" (superchat-timer-tool-create "x" "reminder" 5 nil "monthly")))
      (should (string-prefix-p "Error:" (superchat-timer-tool-create
                                         "x" "reminder" nil
                                         (superchat-timer-format-time (- wtimer--now 3600)))))
      (let ((id (plist-get (car (superchat-timer-list)) :id)))
        (should (string-match-p "cancelled" (superchat-timer-tool-cancel id)))
        (should (string-prefix-p "Error:" (superchat-timer-tool-cancel id)))))))

(ert-deftest timer/times-are-read-and-written-in-one-shape ()
  (let ((time (superchat-timer-parse-time "2026-10-02 08:30")))
    (should (equal "2026-10-02 08:30" (superchat-timer-format-time time)))
    (should (= time (superchat-timer-parse-time "2026-10-02T08:30"))))
  (should-error (superchat-timer-parse-time "tomorrow 8am"))
  (should-error (superchat-timer-parse-time nil)))

(provide 'timer-regression)
;;; timer-regression.el ends here
