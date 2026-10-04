;;; superchat-timer.el --- Timers and reminders that belong to a Workspace -*- lexical-binding: t; -*-

;;; Commentary:
;; A timer is an object of its own, not a Work Record (contract T1).  It
;; belongs to the Workspace of the conversation it was set in and lives on
;; that conversation's Tape as three facts: `timer.created',
;; `timer.fired' and `timer.cancelled'.  What is pending is replayed from
;; those facts, so a timer survives a restart with nothing else to save.
;;
;; When a timer comes due, the user is told in Emacs and a line is left
;; in the Workspace's main conversation (T2); a timer with a task also
;; starts the model on that task in the same conversation (T3).  A timer
;; that came due while Emacs was not running fires once when Superchat
;; loads; a repeating one fires only its most recent missed occurrence.
;;
;; "Take this work up again at that time" is a wait on a Work Record
;; (T4), not a timer; it is not here.
;;
;; The row `timers' arms every pending timer when it is mounted and
;; disarms them when it is unmounted.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'superchat-plugin)
(require 'superchat-db)
(require 'superchat-tape)

(declare-function superchat-work-current "superchat-work" (session-id))
(declare-function superchat-view-workspace-session-ids "superchat-tape-view"
                  (workspace-id))
(declare-function superchat-view-session-configuration "superchat-tape-view"
                  (session-id))
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role &optional create))
(declare-function superchat--render-session-view "superchat" (session-id))
(declare-function superchat--insert-persistent-system-message "superchat-render"
                  (text))
(declare-function superchat-send-input "superchat-dispatcher" ())
(defvar superchat-buffer-name)
(defvar superchat--active-run-id)
(defvar superchat--tool-execution-context)
(defvar superchat--session-id)

(defgroup superchat-timer nil
  "Timers and reminders."
  :group 'superchat)

(defcustom superchat-timer-notify-function #'superchat-timer-notify
  "Function that tells the user a timer came due.
It is called with one argument, the fired timer plist (`:text', `:kind',
`:due', `:late').  The default shows the text in the echo area and rings
the bell."
  :type 'function
  :group 'superchat-timer)

(defvar superchat-timer-now-function #'float-time
  "Function returning the current time in seconds since the epoch.
Tests replace it to move time.")

(defconst superchat-timer-repeats
  '(("hourly" . 3600) ("daily" . 86400) ("weekly" . 604800))
  "The repeat names a timer accepts, and their periods in seconds.")

(defvar superchat-timer--armed (make-hash-table :test #'equal)
  "Timer id -> the Emacs timer that will fire it.")

(defun superchat-timer--now ()
  (funcall superchat-timer-now-function))

;;;; The facts on the Tape --------------------------------------------------------

(defun superchat-timer--payload-get (payload &rest keys)
  "Return the first of KEYS present in the JSON alist PAYLOAD."
  (cl-some (lambda (key) (alist-get key payload)) keys))

(defun superchat-timer--rows ()
  "Every timer fact on the Tape, oldest first, as (SESSION TYPE PAYLOAD)."
  (let (out)
    (dolist (row (superchat-db-tape-select
                  "SELECT session_id, meta FROM tape
                   WHERE kind = 'system' AND meta LIKE '%\"timer.%'
                   ORDER BY id ASC"))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (meta (ignore-errors (json-read-from-string (or (nth 1 row) ""))))
             (type (alist-get 'event_type meta)))
        (when (member type '("timer.created" "timer.fired" "timer.cancelled"))
          (push (list (nth 0 row) type (alist-get 'payload meta)) out))))
    (nreverse out)))

(defun superchat-timer-list ()
  "Return every pending timer, soonest first.
Each is a plist (:id :session-id :kind :text :due :repeat :workspace-id
:workspace-root :last-fired :next-due).  A one-shot timer that fired and
any cancelled timer are not pending."
  (let ((table (make-hash-table :test #'equal)) order)
    (dolist (row (superchat-timer--rows))
      (cl-destructuring-bind (session type payload) row
        (let ((id (superchat-timer--payload-get payload 'timer_id 'timer-id)))
          (pcase type
            ("timer.created"
             (unless (gethash id table) (push id order))
             (puthash id
                      (list :id id :session-id session
                            :kind (intern (or (alist-get 'kind payload) "reminder"))
                            :text (alist-get 'text payload)
                            :due (alist-get 'due payload)
                            :repeat (alist-get 'repeat payload)
                            :workspace-id (superchat-timer--payload-get
                                           payload 'workspace_id 'workspace-id)
                            :workspace-root (superchat-timer--payload-get
                                             payload 'workspace_root 'workspace-root)
                            :last-fired nil)
                      table))
            ("timer.fired"
             (when-let* ((timer (gethash id table)))
               (puthash id (plist-put timer :last-fired
                                      (alist-get 'occurrence payload))
                        table)))
            ("timer.cancelled" (remhash id table))))))
    (let (pending)
      (dolist (id order)
        (when-let* ((timer (gethash id table))
                    (next (superchat-timer--next-due timer)))
          (push (plist-put (copy-sequence timer) :next-due next) pending)))
      (sort pending (lambda (a b) (< (plist-get a :next-due)
                                     (plist-get b :next-due)))))))

(defun superchat-timer-fired-since (since)
  "Return the timers that fired at or after SINCE, oldest first.
Each is a plist (:id :session-id :kind :text :due :late), :due being the
occurrence that fired.  A fired line is not sent to the model as history,
so this is how it can answer \"did I miss a reminder?\"."
  (let (out)
    (dolist (row (superchat-timer--rows))
      (cl-destructuring-bind (session type payload) row
        (when (equal type "timer.fired")
          (let ((occurrence (alist-get 'occurrence payload)))
            (when (and (numberp occurrence) (>= occurrence since))
              (push (list :id (superchat-timer--payload-get payload 'timer_id 'timer-id)
                          :session-id session
                          :kind (intern (or (alist-get 'kind payload) "reminder"))
                          :text (alist-get 'text payload)
                          :due occurrence
                          :late (eq t (alist-get 'late payload)))
                    out))))))
    (nreverse out)))

(defun superchat-timer--next-due (timer)
  "Return the next occurrence of TIMER that has not fired, or nil.
For a one-shot timer that is its due time until it fires; for a repeating
one, the first occurrence after the last one that fired."
  (let ((due (plist-get timer :due))
        (repeat (plist-get timer :repeat))
        (last (plist-get timer :last-fired)))
    (cond
     ((not (numberp due)) nil)
     ((not (and (numberp repeat) (> repeat 0))) (and (null last) due))
     ((null last) due)
     (t (+ due (* repeat (1+ (floor (/ (- last due) repeat)))))))))

(defun superchat-timer--latest-occurrence (timer now)
  "Return the latest occurrence of TIMER at or before NOW that has not fired.
Nil when none has come due."
  (let ((next (superchat-timer--next-due timer))
        (repeat (plist-get timer :repeat)))
    (when (and next (<= next now))
      (if (and (numberp repeat) (> repeat 0))
          (+ next (* repeat (floor (/ (- now next) repeat))))
        next))))

;;;; Creating and cancelling ---------------------------------------------------------

(defun superchat-timer--workspace-of (session-id)
  "Return (ID . ROOT) of the Workspace SESSION-ID is bound to, or nil."
  (when (fboundp 'superchat-view-session-configuration)
    (let* ((config (superchat-view-session-configuration session-id))
           (id (plist-get config :workspace-id)))
      (and (stringp id) (cons id (plist-get config :workspace-root))))))

(defun superchat-timer-create (session-id kind text due &optional repeat)
  "Set a timer in SESSION-ID and return its plist.
KIND is `reminder' (tell the user, T2) or `task' (also start the model on
TEXT, T3).  DUE is the time in seconds since the epoch; REPEAT nil or a
period in seconds.  The timer belongs to the Workspace SESSION-ID is
bound to, and is armed at once when the `timers' row is mounted."
  (unless (and (stringp session-id) (not (string-empty-p session-id)))
    (error "A timer needs a conversation"))
  (unless (memq kind '(reminder task))
    (error "Timer kind must be reminder or task: %S" kind))
  (unless (and (stringp text) (not (string-empty-p (string-trim text))))
    (error "A timer needs a text"))
  (unless (numberp due) (error "Timer due time must be a number: %S" due))
  (unless (or (null repeat) (and (numberp repeat) (>= repeat 60)))
    (error "A repeating timer needs a period of at least a minute"))
  (let* ((id (format "timer-%s-%04x" (format-time-string "%Y%m%d%H%M%S")
                     (random #x10000)))
         (workspace (superchat-timer--workspace-of session-id))
         (timer (list :id id :session-id session-id :kind kind :text text
                      :due (float due) :repeat repeat
                      :workspace-id (car workspace)
                      :workspace-root (cdr workspace)
                      :last-fired nil)))
    (superchat-tape-append-event
     (list :type "timer.created"
           :session-id session-id
           :actor-id "timers"
           :summary (format "%s set for %s%s: %s"
                            (if (eq kind 'task) "Scheduled task" "Reminder")
                            (superchat-timer-format-time due)
                            (if repeat (format ", every %s" (superchat-timer--repeat-name repeat)) "")
                            text)
           :payload (list :timer-id id :kind (symbol-name kind) :text text
                          :due (float due) :repeat repeat
                          :workspace-id (car workspace)
                          :workspace-root (cdr workspace))))
    (when (superchat-timer--active-p)
      (superchat-timer--arm (plist-put timer :next-due (float due))))
    timer))

(defun superchat-timer-cancel (id)
  "Cancel the pending timer ID; return non-nil when there was one."
  (when-let* ((timer (cl-find id (superchat-timer-list)
                              :key (lambda (timer) (plist-get timer :id))
                              :test #'equal)))
    (superchat-tape-append-event
     (list :type "timer.cancelled"
           :session-id (plist-get timer :session-id)
           :actor-id "timers"
           :summary (format "Timer cancelled: %s" (plist-get timer :text))
           :payload (list :timer-id id)))
    (superchat-timer--disarm id)
    t))

;;;; Firing ----------------------------------------------------------------------------

(defun superchat-timer--target-session (timer)
  "The conversation a fired TIMER speaks in: its Workspace's main one.
That is the most recently active conversation bound to the Workspace; a
timer set in a temporary conversation speaks in that conversation."
  (or (and (plist-get timer :workspace-root)
           (fboundp 'superchat-view-workspace-session-ids)
           (car (superchat-view-workspace-session-ids
                 (plist-get timer :workspace-id))))
      (plist-get timer :session-id)))

(defun superchat-timer--live-view (session-id)
  "The live chat View of SESSION-ID, or nil."
  (let ((buffer (and (fboundp 'superchat-editor-workspace-buffer)
                     (superchat-editor-workspace-buffer session-id 'session-view))))
    (and (buffer-live-p buffer) buffer)))

(defun superchat-timer-notify (fired)
  "Tell the user FIRED came due: the echo area and the bell."
  (ding t)
  (message "Superchat %s%s: %s"
           (if (eq (plist-get fired :kind) 'task) "scheduled task" "reminder")
           (if (plist-get fired :late)
               (format " (due %s)" (superchat-timer-format-time (plist-get fired :due)))
             "")
           (plist-get fired :text)))

(defun superchat-timer--start-task (session-id text)
  "Start the model on TEXT in the conversation SESSION-ID; return the Run id."
  (let ((view (or (superchat-timer--live-view session-id)
                  (and (fboundp 'superchat--render-session-view)
                       (superchat--render-session-view session-id)))))
    (when (buffer-live-p view)
      (with-current-buffer view
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert text))
        (superchat-send-input)
        superchat--active-run-id))))

(defun superchat-timer-fire (timer occurrence)
  "Fire OCCURRENCE (a due time) of TIMER and return what was done.
Records `timer.fired' in the Workspace's main conversation, tells the
user, shows the line in that conversation's View when it is open, and for
a task starts the model there.  The record comes first, so a crash
afterwards never fires the same occurrence twice."
  (let* ((session (superchat-timer--target-session timer))
         (now (superchat-timer--now))
         (late (> (- now occurrence) 60))
         (kind (plist-get timer :kind))
         (text (plist-get timer :text))
         (line (format "%s%s: %s"
                       (if (eq kind 'task) "Scheduled task" "Reminder")
                       (if late (format " (due %s)" (superchat-timer-format-time occurrence)) "")
                       text))
         (fired (list :id (plist-get timer :id) :session-id session :kind kind
                      :text text :due occurrence :late late)))
    (superchat-tape-append-event
     (list :type "timer.fired"
           :session-id session
           :actor-id "timers"
           :summary line
           :payload (list :timer-id (plist-get timer :id)
                          :occurrence occurrence
                          :kind (symbol-name kind) :text text
                          :late (and late t))))
    (ignore-errors (funcall superchat-timer-notify-function fired))
    (when-let* ((view (superchat-timer--live-view session)))
      (ignore-errors
        (let ((superchat-buffer-name (buffer-name view)))
          (with-current-buffer view
            (superchat--insert-persistent-system-message line)))))
    (when (eq kind 'task)
      (plist-put fired :run-id
                 (ignore-errors (superchat-timer--start-task session text))))
    fired))

;;;; Arming -----------------------------------------------------------------------------

(defun superchat-timer--active-p ()
  (and (fboundp 'superchat-plugin-row-active-p)
       (superchat-plugin-row-active-p 'timers)))

(defun superchat-timer--disarm (id)
  (when-let* ((emacs-timer (gethash id superchat-timer--armed)))
    (cancel-timer emacs-timer)
    (remhash id superchat-timer--armed)))

(defun superchat-timer-disarm-all ()
  "Cancel every Emacs timer this module set; the facts stay on the Tape."
  (maphash (lambda (_id emacs-timer) (cancel-timer emacs-timer))
           superchat-timer--armed)
  (clrhash superchat-timer--armed))

(defun superchat-timer--due (id)
  "The Emacs timer for ID went off: fire what is due now, arm the rest."
  (remhash id superchat-timer--armed)
  (when-let* ((timer (cl-find id (superchat-timer-list)
                              :key (lambda (timer) (plist-get timer :id))
                              :test #'equal))
              (occurrence (superchat-timer--latest-occurrence
                           timer (superchat-timer--now))))
    (superchat-timer-fire timer occurrence)
    (when-let* ((again (cl-find id (superchat-timer-list)
                                :key (lambda (timer) (plist-get timer :id))
                                :test #'equal)))
      (superchat-timer--arm again))))

(defun superchat-timer--arm (timer)
  "Set an Emacs timer for TIMER's next occurrence."
  (let ((id (plist-get timer :id))
        (delay (max 0 (- (plist-get timer :next-due) (superchat-timer--now)))))
    (superchat-timer--disarm id)
    (puthash id (run-at-time delay nil #'superchat-timer--due id)
             superchat-timer--armed)))

(defun superchat-timer-catch-up ()
  "Fire, once each, the timers that came due while nothing was armed.
A repeating timer fires only its most recent missed occurrence.  Return
the fired plists."
  (let ((now (superchat-timer--now)) fired)
    (dolist (timer (superchat-timer-list))
      (when-let* ((occurrence (superchat-timer--latest-occurrence timer now)))
        (push (superchat-timer-fire timer occurrence) fired)))
    (nreverse fired)))

(defun superchat-timer-arm-all ()
  "Catch up on missed timers, then arm every pending one."
  (superchat-timer-disarm-all)
  (superchat-timer-catch-up)
  (dolist (timer (superchat-timer-list))
    (superchat-timer--arm timer)))

;;;; Time -------------------------------------------------------------------------------

(defun superchat-timer-format-time (time)
  "TIME, seconds since the epoch, as local \"YYYY-MM-DD HH:MM\"."
  (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time time)))

(defun superchat-timer--repeat-name (seconds)
  (or (car (rassq seconds superchat-timer-repeats))
      (format "%d minutes" (/ seconds 60))))

(defun superchat-timer-parse-time (at)
  "Parse AT, local \"YYYY-MM-DD HH:MM\", to seconds since the epoch.
Signals an error for any other shape."
  (unless (and (stringp at)
               (string-match "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)[ T]\\([0-9]\\{1,2\\}\\):\\([0-9]\\{2\\}\\)\\'"
                             (string-trim at)))
    (error "Time must look like 2026-10-02 08:30: %S" at))
  (let ((at (string-trim at)))
    (float-time
     (encode-time (list 0
                        (string-to-number (match-string 5 at))
                        (string-to-number (match-string 4 at))
                        (string-to-number (match-string 3 at))
                        (string-to-number (match-string 2 at))
                        (string-to-number (match-string 1 at))
                        nil -1 nil)))))

;;;; Tools ------------------------------------------------------------------------------

(defun superchat-timer--tool-session ()
  (or (and (boundp 'superchat--tool-execution-context)
           (plist-get superchat--tool-execution-context :session-id))
      (and (boundp 'superchat--session-id) superchat--session-id)))

(defun superchat-timer-tool-create (text kind &optional in-minutes at repeat)
  "The `timer_create' tool: set a timer in the asking conversation."
  (condition-case err
      (let* ((now (superchat-timer--now))
             (kind (pcase kind
                     ((or "task" 'task) 'task)
                     ((or "reminder" 'reminder 'nil "") 'reminder)
                     (_ (error "kind must be reminder or task"))))
             (due (cond
                   ((and (stringp at) (not (string-empty-p at)))
                    (superchat-timer-parse-time at))
                   ((numberp in-minutes) (+ now (* 60 in-minutes)))
                   ((and (stringp in-minutes) (string-match-p "\\`[0-9.]+\\'" in-minutes))
                    (+ now (* 60 (string-to-number in-minutes))))
                   (t (error "Give in_minutes or at"))))
             (period (cond
                      ((or (null repeat) (member repeat '("" "none"))) nil)
                      ((cdr (assoc repeat superchat-timer-repeats)))
                      (t (error "repeat must be hourly, daily, weekly or none")))))
        (when (and (< due now) (null period))
          (error "That time has passed: %s" (superchat-timer-format-time due)))
        ;; "Take this work up again at that time" is a wait on its Work
        ;; Record, not a timer (contract T4): in the real task T4W the model
        ;; set a task timer for it, which ran in the old conversation.
        (when (and (eq kind 'task)
                   (fboundp 'superchat-work-current)
                   (superchat-work-current (superchat-timer--tool-session)))
          (error "This conversation has a current work record; to take that work up again later, use work_record with action wait, wait_for time and in_minutes or at -- it continues in a new conversation with the record.  A task timer is for a new, separate job"))
        (let ((timer (superchat-timer-create (superchat-timer--tool-session)
                                             kind text due period)))
          (format "Timer %s set: %s at %s%s (now %s).%s"
                  (plist-get timer :id)
                  (if (eq kind 'task) "the task will run" "the user will be reminded")
                  (superchat-timer-format-time due)
                  (if period (format ", then %s" (superchat-timer--repeat-name period)) "")
                  (superchat-timer-format-time now)
                  (if (plist-get timer :workspace-root)
                      ""
                    "  This conversation belongs to no workspace: the timer fires in it."))))
    (error (format "Error: %s" (error-message-string err)))))

(defun superchat-timer-tool-list ()
  "The `timer_list' tool: the pending timers, then those fired in the last day."
  (let ((timers (superchat-timer-list))
        (fired (superchat-timer-fired-since (- (superchat-timer--now) 86400))))
    (concat
     (format "Now %s.\n" (superchat-timer-format-time (superchat-timer--now)))
     (if (null timers)
         "No pending timers."
       (concat
        "Pending:\n"
        (mapconcat (lambda (timer)
                   (format "%s  %s  %s%s  %s"
                           (plist-get timer :id)
                           (superchat-timer-format-time (plist-get timer :next-due))
                           (plist-get timer :kind)
                           (if (plist-get timer :repeat)
                               (format " (%s)" (superchat-timer--repeat-name
                                                (plist-get timer :repeat)))
                             "")
                           (plist-get timer :text)))
                 timers "\n")))
     (if (null fired)
         "\nNone fired in the last 24 hours."
       (concat
        "\nFired in the last 24 hours:\n"
        (mapconcat (lambda (timer)
                     (format "%s  %s%s  %s"
                             (superchat-timer-format-time (plist-get timer :due))
                             (plist-get timer :kind)
                             (if (plist-get timer :late)
                                 " (Emacs was not running then; shown when it started)"
                               "")
                             (plist-get timer :text)))
                   fired "\n"))))))

(defun superchat-timer-tool-cancel (id)
  "The `timer_cancel' tool: cancel the pending timer ID."
  (if (superchat-timer-cancel id)
      (format "Timer %s cancelled." id)
    (format "Error: no pending timer %s." id)))

;;;; The row ------------------------------------------------------------------------------

(superchat-define-plugin timers
  :doc "Timers and reminders: every pending timer on the Tape is armed when
the row mounts (missed ones fire once first) and disarmed when it unmounts."
  :apply
  (lambda (scope _config)
    ;; After loading finishes, so a missed task never starts mid-`require'.
    (let ((start (run-at-time 0 nil #'superchat-timer-arm-all)))
      (superchat-scope-effect scope
                              (lambda ()
                                (cancel-timer start)
                                (superchat-timer-disarm-all))))))

(provide 'superchat-timer)
;;; superchat-timer.el ends here
