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
(declare-function superchat-runtime-active-runs "superchat-runtime" ())
(declare-function superchat-runtime-status "superchat-runtime" (run-id))
(declare-function superchat-tape-replay-events "superchat-tape"
                  (session-id &optional since-id limit))
(defvar superchat-buffer-name)
(defvar superchat-automated-input)
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

(defvar superchat-timer-retry-seconds 300
  "Seconds after a task that could not be started before it is tried again.")

(defvar superchat-timer-retry-limit 3
  "How many times one Emacs tries a task that failed to start again by itself.")

(defvar superchat-timer-busy-retry-seconds 30
  "Seconds before a task is tried again when its conversation was busy.")

(defvar superchat-timer--retries (make-hash-table :test #'equal)
  "(TIMER-ID . OCCURRENCE) -> how many starts of it failed in this Emacs.")

(defvar superchat-timer--not-before (make-hash-table :test #'equal)
  "(TIMER-ID . OCCURRENCE) -> the time before which its start is not tried again.
Set when a start fails, so that a retry another task caused does not bring
this one forward.")

(defvar superchat-timer--retry-timer nil
  "The Emacs timer that will try the unstarted tasks again, or nil.")

(defvar superchat-timer--retry-at nil
  "When `superchat-timer--retry-timer' is due, in seconds since the epoch.")

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
        (when (member type '("timer.created" "timer.fired" "timer.cancelled"
                             "timer.sending" "timer.started" "timer.unconfirmed"))
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

(defun superchat-timer--scope-of (session-id)
  "The id of the Workspace SESSION-ID belongs to, or nil when it has none.
A conversation belongs to a Workspace when it is bound to one with a root."
  (let ((workspace (and (stringp session-id)
                        (superchat-timer--workspace-of session-id))))
    (and (stringp (cdr workspace)) (not (string-empty-p (cdr workspace)))
         (car workspace))))

(defun superchat-timer--within-p (session-id timer)
  "Non-nil when SESSION-ID's conversation may see and cancel TIMER.
TIMER is a plist with :session-id, the conversation it was set in.  A
conversation of a Workspace: TIMER is that Workspace's.  A conversation of
no Workspace: TIMER was set in that very conversation.  A pending timer
carries its Workspace; a fired one is its conversation's."
  (let ((asking (superchat-timer--scope-of session-id))
        (owner (if (plist-member timer :workspace-root)
                   (and (stringp (plist-get timer :workspace-root))
                        (not (string-empty-p (plist-get timer :workspace-root)))
                        (plist-get timer :workspace-id))
                 (superchat-timer--scope-of (plist-get timer :session-id)))))
    (if asking
        (equal asking owner)
      (and (stringp session-id)
           (equal session-id (plist-get timer :session-id))))))

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
  "Cancel the timer ID; return non-nil when there was something to cancel.
That is a pending timer, or a fired task still waiting to be started."
  (let ((timer (cl-find id (superchat-timer-list)
                        :key (lambda (timer) (plist-get timer :id))
                        :test #'equal))
        (task (cl-find id (superchat-timer-unstarted-tasks)
                       :key (lambda (task) (plist-get task :id))
                       :test #'equal)))
    (when (or timer task)
      (superchat-tape-append-event
       (list :type "timer.cancelled"
             :session-id (plist-get (or timer task) :session-id)
             :actor-id "timers"
             :summary (format "Timer cancelled: %s"
                              (plist-get (or timer task) :text))
             :payload (list :timer-id id)))
      (superchat-timer--disarm id)
      t)))

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

(defun superchat-timer--fact (session-id type timer-id occurrence &optional more)
  "Append the timer fact TYPE for OCCURRENCE of TIMER-ID to SESSION-ID's Tape."
  (superchat-tape-append-event
   (list :type type :session-id session-id :actor-id "timers"
         :summary (format "%s: %s" type timer-id)
         :payload (append (list :timer-id timer-id :occurrence occurrence) more))))

(defun superchat-timer--session-busy-p (session-id)
  "Non-nil while an execution is running in the conversation SESSION-ID."
  (cl-some (lambda (run)
             (equal session-id
                    (plist-get (superchat-runtime-status run) :session-id)))
           (superchat-runtime-active-runs)))

(defvar superchat-timer-started-functions nil
  "Functions called with SESSION-ID and RUN-ID once a task's start is on record.
`timer.started' is written after the send returns.  An execution may have
ended by then; whoever acts on \"Superchat started this\" at the end of an
execution learns it here.")

(defun superchat-timer--note-started (session-id timer-id occurrence run-id)
  "Record that RUN-ID is the execution of OCCURRENCE of the task TIMER-ID."
  (superchat-timer--fact session-id "timer.started" timer-id occurrence
                         (list :run-id run-id))
  (run-hook-with-args 'superchat-timer-started-functions session-id run-id))

(defun superchat-timer--start-task (session-id timer-id occurrence text)
  "Start the model on TEXT in the conversation SESSION-ID; return the Run id.
Return `busy', sending nothing, while an execution runs there: the message
would only be queued in memory.  Otherwise record `timer.sending', send, and
record `timer.started' with the Run that began; nil when none began."
  (if (superchat-timer--session-busy-p session-id)
      'busy
    (let ((view (or (superchat-timer--live-view session-id)
                    (and (fboundp 'superchat--render-session-view)
                         (superchat--render-session-view session-id)))))
      (when (buffer-live-p view)
        (with-current-buffer view
          (let ((before superchat--active-run-id))
            (superchat-timer--fact session-id "timer.sending" timer-id occurrence)
            (let ((inhibit-read-only t)
                  (start (copy-marker (point-max))))
              (goto-char (point-max))
              (insert text)
              (condition-case err
                  ;; The timer speaks here, not the user.
                  (let ((superchat-automated-input t))
                    (superchat-send-input))
                ;; A failed send must not leave the text for the next try.
                (error (when (<= start (point-max))
                         (delete-region start (point-max)))
                       (signal (car err) (cdr err)))))
            (let ((run superchat--active-run-id))
              (when (and (stringp run) (not (equal run before)))
                (superchat-timer--note-started session-id timer-id occurrence run)
                run))))))))

(defun superchat-timer--retry-later (seconds)
  "Try the unstarted tasks again in SECONDS, or sooner when that is already set.
One Emacs timer serves every unstarted task, so it is due at the soonest
time any of them asked for."
  (let ((at (+ (float-time) seconds)))
    (when (or (not (timerp superchat-timer--retry-timer))
              (not (numberp superchat-timer--retry-at))
              (< at superchat-timer--retry-at))
      (when (timerp superchat-timer--retry-timer)
        (cancel-timer superchat-timer--retry-timer))
      (setq superchat-timer--retry-at at
            superchat-timer--retry-timer
            (run-at-time seconds nil
                         (lambda ()
                           (setq superchat-timer--retry-timer nil
                                 superchat-timer--retry-at nil)
                           (superchat-timer-retry-tasks)))))))

(defun superchat-timer--try-task (session-id timer-id occurrence text)
  "Start the task and return its Run id, or nil when no execution began.
A busy conversation is tried again after `superchat-timer-busy-retry-seconds'
without limit; a failed start after `superchat-timer-retry-seconds', at most
`superchat-timer-retry-limit' times in one Emacs.

Both are checked here, before the start is tried, not only when the next
try is scheduled: the one Emacs timer that retries serves every unstarted
task, so a task out of tries, or one that failed a moment ago, is reached
again whenever another task keeps that timer going."
  (let* ((key (cons timer-id occurrence))
         (failed (gethash key superchat-timer--retries 0))
         (wait (- (gethash key superchat-timer--not-before 0)
                  (superchat-timer--now))))
    (cond
     ;; Out of tries in this Emacs: it stays on record as not started.
     ((> failed superchat-timer-retry-limit) nil)
     ;; It failed a moment ago: its own wait is not over.
     ((> wait 0) (superchat-timer--retry-later wait) nil)
     (t
      (let ((run (condition-case err
                     (superchat-timer--start-task session-id timer-id occurrence text)
                   (error (message "superchat: scheduled task %s was not started: %s"
                                   timer-id (error-message-string err))
                          nil))))
        (cond ((stringp run)
               (remhash key superchat-timer--retries)
               (remhash key superchat-timer--not-before)
               run)
              ((eq run 'busy)
               (superchat-timer--retry-later superchat-timer-busy-retry-seconds)
               nil)
              (t (let ((failed (1+ failed)))
                   (puthash key failed superchat-timer--retries)
                   (when (<= failed superchat-timer-retry-limit)
                     (puthash key (+ (superchat-timer--now)
                                     superchat-timer-retry-seconds)
                              superchat-timer--not-before)
                     (superchat-timer--retry-later superchat-timer-retry-seconds)))
                 nil)))))))

(defun superchat-timer-retry-state (timer-id occurrence)
  "How the retries of the task TIMER-ID due at OCCURRENCE stand in this Emacs.
Nil when its start never failed here.  Otherwise a plist (:tries N
:again-at TIME :gave-up BOOL): how often the start failed, when it is tried
next, and whether this Emacs tries no more.  Reads; changes nothing."
  (let* ((key (cons timer-id occurrence))
         (failed (gethash key superchat-timer--retries 0)))
    (when (> failed 0)
      (let ((gave-up (> failed superchat-timer-retry-limit)))
        (list :tries failed
              :gave-up gave-up
              :again-at (and (not gave-up)
                             (gethash key superchat-timer--not-before)))))))

(defun superchat-timer-unstarted-tasks ()
  "The fired tasks with no execution on record, oldest first.
Each is a plist (:id :session-id :occurrence :text :sending).  Only an
occurrence whose `timer.fired' fact asks for a start is counted, so tasks
fired before this was recorded are left alone."
  (let ((table (make-hash-table :test #'equal)) order)
    (dolist (row (superchat-timer--rows))
      (cl-destructuring-bind (session type payload) row
        (let* ((id (superchat-timer--payload-get payload 'timer_id 'timer-id))
               (key (cons id (alist-get 'occurrence payload))))
          (pcase type
            ("timer.fired"
             (when (eq t (superchat-timer--payload-get payload 'awaits_start 'awaits-start))
               (push key order)
               (puthash key (list :id id :session-id session
                                  :occurrence (cdr key)
                                  :text (alist-get 'text payload)
                                  :sending nil)
                        table)))
            ("timer.sending"
             (when-let* ((task (gethash key table)))
               (puthash key (plist-put task :sending t) table)))
            ((or "timer.started" "timer.unconfirmed") (remhash key table))
            ("timer.cancelled"
             (dolist (k order)
               (when (equal id (car k)) (remhash k table))))))))
    (delq nil (mapcar (lambda (key) (gethash key table)) (nreverse order)))))

(defun superchat-timer--run-after-sending-p (task)
  "Non-nil when a Run began in TASK's conversation after it was last sent."
  (let (seen ran)
    (dolist (event (superchat-tape-replay-events (plist-get task :session-id)))
      (let ((payload (plist-get event :payload)))
        (cond ((and (equal "timer.sending" (plist-get event :type))
                    (equal (plist-get task :id) (plist-get payload :timer-id))
                    (equal (plist-get task :occurrence) (plist-get payload :occurrence)))
               (setq seen t ran nil))
              ((and seen (equal "run.started" (plist-get event :type)))
               (setq ran t)))))
    ran))

(defun superchat-timer-unconfirmed-tasks (&optional since)
  "The fired tasks Superchat could not tell were started, oldest first.
Each is a plist (:id :session-id :occurrence :text).  That is every
occurrence with a `timer.unconfirmed' fact: Emacs exited while the task was
being sent, a Run started in its conversation afterwards, and it was not
started again.  With SINCE, only occurrences due at or after that time."
  (let ((table (make-hash-table :test #'equal)) order)
    (dolist (row (superchat-timer--rows))
      (cl-destructuring-bind (session type payload) row
        (let* ((id (superchat-timer--payload-get payload 'timer_id 'timer-id))
               (key (cons id (alist-get 'occurrence payload))))
          (pcase type
            ("timer.fired"
             (puthash key (list :id id :session-id session
                                :occurrence (cdr key)
                                :text (alist-get 'text payload)
                                :unconfirmed nil)
                      table))
            ("timer.unconfirmed"
             (when-let* ((task (gethash key table)))
               (unless (plist-get task :unconfirmed)
                 (push key order))
               (puthash key (plist-put task :unconfirmed t) table)))))))
    (delq nil
          (mapcar (lambda (key)
                    (let ((task (gethash key table)))
                      (and (or (null since)
                               (and (numberp (plist-get task :occurrence))
                                    (>= (plist-get task :occurrence) since)))
                           (list :id (plist-get task :id)
                                 :session-id (plist-get task :session-id)
                                 :occurrence (plist-get task :occurrence)
                                 :text (plist-get task :text)))))
                  (nreverse order)))))

(defun superchat-timer-retry-tasks ()
  "Start every fired task that has no execution on record.
A task that was being sent when Emacs exited, in a conversation where a Run
started afterwards, may or may not have run: that is said in the
conversation, recorded as `timer.unconfirmed', and it is not started again."
  (dolist (task (superchat-timer-unstarted-tasks))
    (let ((session (plist-get task :session-id))
          (id (plist-get task :id))
          (occurrence (plist-get task :occurrence)))
      (if (and (plist-get task :sending)
               (not (superchat-timer--session-busy-p session))
               (superchat-timer--run-after-sending-p task))
          (let ((line (format "Scheduled task (due %s): %s -- Superchat cannot tell whether it started: Emacs exited while it was being sent. It was not started again."
                              (superchat-timer-format-time occurrence)
                              (plist-get task :text))))
            (superchat-timer--fact session "timer.unconfirmed" id occurrence)
            (when-let* ((view (superchat-timer--live-view session)))
              (ignore-errors
                (let ((superchat-buffer-name (buffer-name view)))
                  (with-current-buffer view
                    (superchat--insert-persistent-system-message line)))))
            (message "superchat: %s" line))
        (superchat-timer--try-task session id occurrence (plist-get task :text))))))


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
                          :late (and late t)
                          ;; A task is done only when its execution began.
                          :awaits-start (eq kind 'task))))
    (ignore-errors (funcall superchat-timer-notify-function fired))
    (when-let* ((view (superchat-timer--live-view session)))
      (ignore-errors
        (let ((superchat-buffer-name (buffer-name view)))
          (with-current-buffer view
            (superchat--insert-persistent-system-message line)))))
    (when (eq kind 'task)
      (plist-put fired :run-id
                 (superchat-timer--try-task session (plist-get timer :id)
                                            occurrence text)))
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
  (clrhash superchat-timer--armed)
  (when (timerp superchat-timer--retry-timer)
    (cancel-timer superchat-timer--retry-timer))
  (setq superchat-timer--retry-timer nil
        superchat-timer--retry-at nil))

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
  ;; First the tasks an earlier Emacs fired and never started.  How often
  ;; this Emacs already tried one, and how long it waits, is kept: turning
  ;; the row off and on is not a new Emacs.
  (superchat-timer-retry-tasks)
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

(defconst superchat-timer--off
  "Timers are turned off in this Emacs (the timers row is not mounted)"
  "What a timer tool says while nothing would fire a timer.")

(defun superchat-timer-tool-create (text kind &optional in-minutes at repeat)
  "The `timer_create' tool: set a timer in the asking conversation.
A timer is a promise that something happens at its time.  While the
`timers' row is not mounted nothing would fire it, so it is refused
rather than written and never acted on."
  (condition-case err
      (let* ((now (progn
                    (unless (superchat-timer--active-p)
                      (error "%s, so no timer was set and nothing will happen at that time; tell the user"
                             superchat-timer--off))
                    (superchat-timer--now)))
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
  "The `timer_list' tool: the pending timers, then those fired in the last day.
Only those of the asking conversation's Workspace; of that conversation
alone when it belongs to none."
  (let* ((session (superchat-timer--tool-session))
         (mine (lambda (timer) (superchat-timer--within-p session timer)))
         (timers (cl-remove-if-not mine (superchat-timer-list)))
         (fired (cl-remove-if-not
                 mine (superchat-timer-fired-since (- (superchat-timer--now) 86400)))))
    (concat
     (format "Now %s.\n" (superchat-timer-format-time (superchat-timer--now)))
     (if (superchat-timer--active-p)
         ""
       (format "%s: nothing below fires until they are turned on again.\n"
               superchat-timer--off))
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
  "The `timer_cancel' tool: cancel the pending timer ID.
A timer of another Workspace is refused, pending or fired and not yet
started; the user's own commands cancel any."
  (let* ((session (superchat-timer--tool-session))
         (by-id (lambda (timer) (equal id (plist-get timer :id))))
         (timer (or (cl-find-if by-id (superchat-timer-list))
                    (cl-find-if by-id (superchat-timer-unstarted-tasks)))))
    (cond ((null timer) (format "Error: no pending timer %s." id))
          ((not (superchat-timer--within-p session timer))
           (format "Error: timer %s belongs to another workspace; this conversation cannot cancel it."
                   id))
          ((superchat-timer-cancel id) (format "Timer %s cancelled." id))
          (t (format "Error: no pending timer %s." id)))))

(defvar superchat-timer--start-timer nil
  "The Emacs timer that arms everything once the `timers' row is mounted.")

;;;; The row ------------------------------------------------------------------------------

(superchat-define-plugin timers
  :doc "Timers and reminders: every pending timer on the Tape is armed when
the row mounts (missed ones fire once first) and disarmed when it unmounts."
  :apply
  (lambda (scope _config)
    ;; The armed timers are one table for the whole Emacs: the first
    ;; installation arms them, the last one out disarms them.
    (superchat-scope-shared-effect
     scope 'timers-armed
     (lambda ()
       ;; After loading finishes, so a missed task never starts mid-`require'.
       (setq superchat-timer--start-timer
             (run-at-time 0 nil #'superchat-timer-arm-all)))
     (lambda ()
       (when (timerp superchat-timer--start-timer)
         (cancel-timer superchat-timer--start-timer))
       (setq superchat-timer--start-timer nil)
       (superchat-timer-disarm-all)))))

(provide 'superchat-timer)
;;; superchat-timer.el ends here
