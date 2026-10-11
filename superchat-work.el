;;; superchat-work.el --- Work Records: work that outlives one execution -*- lexical-binding: t; -*-

;;; Commentary:
;; A Work Record keeps a piece of work that must survive more than one
;; execution (contract K1): what is to be delivered, how far it got, why
;; the next step cannot be taken yet, and where to pick it up (K2).  The
;; model judges and registers through the `work_record' tool; the product
;; keeps the record, adds the facts of what each execution changed, and
;; decides at the end of each execution how it stands (K3, K7).
;;
;; A record is facts on the Tape of the conversation that wrote them, only
;; ever appended (K12): `work.created', `work.updated', `work.waiting',
;; `work.completed', `work.focused' (the conversation's current record),
;; `work.facts' (what an execution changed, written by the product) and
;; `work.outcome' (how an execution left the record, written by the
;; product).  What a record is now is computed from those facts.
;;
;; Records belong to a Workspace with a root; a temporary conversation keeps
;; none (W10).  See
;; docs/decisions/accepted/2026-10-03-m11-slice-7-work-records.md.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'superchat-plugin)
(require 'superchat-db)
(require 'superchat-tape)
(require 'superchat-event)

(declare-function superchat-runtime--terminal-p "superchat-runtime" (status))

(declare-function superchat-view-session-configuration "superchat-tape-view"
                  (session-id))
(declare-function superchat-changes-candidates "superchat-changes"
                  (session-id &optional run-id))
(defvar superchat--tool-execution-context)

(defconst superchat-work-fact-types
  '("work.created" "work.updated" "work.waiting" "work.completed"
    "work.focused" "work.facts" "work.outcome" "work.woken" "work.reported"
    "work.task")
  "The Tape facts a Work Record is made of.")

(defconst superchat-work-wait-kinds '("user" "agent" "time")
  "What a record may wait for: the user's answer, another agent, a time.")

;;;; Reading the facts ----------------------------------------------------------

(defun superchat-work--get (payload &rest keys)
  "The first of KEYS present in the JSON alist PAYLOAD."
  (cl-some (lambda (key) (alist-get key payload)) keys))

(defun superchat-work--rows ()
  "Every Work Record fact, oldest first, as (SESSION TYPE PAYLOAD)."
  (let (out)
    (dolist (row (superchat-db-tape-select
                  "SELECT session_id, meta FROM tape
                   WHERE kind = 'system' AND meta LIKE '%\"work.%'
                   ORDER BY id ASC"))
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'symbol)
             (meta (ignore-errors (json-read-from-string (or (nth 1 row) ""))))
             (type (alist-get 'event_type meta)))
        (when (member type superchat-work-fact-types)
          (push (list (nth 0 row) type (alist-get 'payload meta)) out))))
    (nreverse out)))

(defun superchat-work--build (rows)
  "Fold ROWS into a hash table of records by id, and the order they began."
  (let ((table (make-hash-table :test #'equal)) order)
    (dolist (row rows)
      (cl-destructuring-bind (session type payload) row
        (let* ((id (superchat-work--get payload 'work_id 'work-id))
               (record (and id (gethash id table))))
          (pcase type
            ("work.created"
             (unless record
               (push id order)
               (puthash id
                        (list :id id :session-id session
                              :goal (alist-get 'goal payload)
                              :follows (alist-get 'follows payload)
                              :workspace-id (superchat-work--get payload 'workspace_id 'workspace-id)
                              :workspace-root (superchat-work--get payload 'workspace_root 'workspace-root)
                              :progress nil :blocked nil :resume nil
                              :waiting nil :conclusion nil :completed-by nil
                              :facts nil :outcomes nil)
                        table)))
            ("work.updated"
             (when record
               (dolist (field '(progress blocked resume))
                 (when-let* ((value (alist-get field payload)))
                   (setq record (plist-put record (intern (format ":%s" field)) value))))
               (puthash id record table)))
            ("work.waiting"
             (when record
               (puthash id (plist-put record :waiting
                                      (and (alist-get 'for payload)
                                           (list :for (alist-get 'for payload)
                                                 :detail (alist-get 'detail payload)
                                                 :until (alist-get 'until payload))))
                        table)))
            ("work.task"
             (when record
               (puthash id (plist-put record :tasks
                                      (append (plist-get record :tasks)
                                              (list (list :task (alist-get 'task payload)
                                                          :status (alist-get 'status payload)
                                                          :report (alist-get 'report payload)
                                                          :conversation (alist-get 'conversation payload)))))
                        table)))
            ("work.woken"
             (when record
               (setq record (plist-put record :woken
                                       (append (plist-get record :woken)
                                               (list (alist-get 'until payload)))))
               (puthash id (plist-put record :wakes
                                      (append (plist-get record :wakes)
                                              (list (list :until (alist-get 'until payload)
                                                          :detail (alist-get 'detail payload)
                                                          :session session
                                                          :main (alist-get 'main payload)))))
                        table)))
            ("work.reported"
             (when record
               (puthash id (plist-put record :reported
                                      (append (plist-get record :reported)
                                              (list (alist-get 'conversation payload))))
                        table)))
            ("work.completed"
             (when record
               (setq record (plist-put record :conclusion (alist-get 'conclusion payload)))
               (setq record (plist-put record :completed-by (alist-get 'by payload)))
               (puthash id (plist-put record :waiting nil) table)))
            ("work.facts"
             (when record
               (puthash id (plist-put record :facts
                                      (append (plist-get record :facts)
                                              (list (list :run-id (superchat-work--get payload 'run_id 'run-id)
                                                          :files (alist-get 'files payload)
                                                          :buffers (alist-get 'buffers payload)
                                                          :jj-change (superchat-work--get payload 'jj_change 'jj-change)))))
                        table)))
            ("work.outcome"
             (when record
               (setq record (plist-put record :executions
                                       (append (plist-get record :executions)
                                               (list (list :run-id (superchat-work--get payload 'run_id 'run-id)
                                                           :session session
                                                           :outcome (alist-get 'outcome payload))))))
               (puthash id (plist-put record :outcomes
                                      (append (plist-get record :outcomes)
                                              (list (alist-get 'outcome payload))))
                        table))))
          ;; Where the record's newest fact was written.
          (when-let* ((now (and id (gethash id table))))
            (puthash id (plist-put now :last-session session) table)))))
    (cons table (nreverse order))))

(defun superchat-work-status (record)
  "RECORD's status: `completed', `waiting-user', `waiting-other' or `continuable'."
  (cond ((plist-get record :conclusion) 'completed)
        ((plist-get record :waiting)
         (if (equal (plist-get (plist-get record :waiting) :for) "user")
             'waiting-user
           'waiting-other))
        (t 'continuable)))

(defun superchat-work-records (&optional workspace-id)
  "Every Work Record, oldest first; only WORKSPACE-ID's when it is given."
  (cl-destructuring-bind (table . order) (superchat-work--build (superchat-work--rows))
    (delq nil (mapcar (lambda (id)
                        (let ((record (gethash id table)))
                          (and (or (null workspace-id)
                                   (equal workspace-id (plist-get record :workspace-id)))
                               record)))
                      order))))

(defun superchat-work-get (id)
  "The Work Record ID, or nil."
  (cl-find id (superchat-work-records) :key (lambda (r) (plist-get r :id)) :test #'equal))

(defun superchat-work--focused (session-id)
  "The id of the open record SESSION-ID's last `work.focused' fact names, or nil.
Whose record it is has not been asked: `superchat-work-current' asks."
  (let (current)
    (dolist (row (superchat-work--rows))
      (cl-destructuring-bind (session type payload) row
        (when (and (equal session session-id) (equal type "work.focused"))
          (setq current (superchat-work--get payload 'work_id 'work-id)))))
    (and current
         (let ((record (superchat-work-get current)))
           (and record (not (plist-get record :conclusion)) current)))))

(defun superchat-work-current (session-id)
  "The id of SESSION-ID's current Work Record, or nil.
That is the record of its last `work.focused' fact, while not completed and
while it is one this conversation may reach (`superchat-work--within-p').
A focus on another Workspace's record, as a Tape written before records
belonged to a Workspace may hold, names no current record: nothing of that
record rides along with a request, is answered for, or is concluded here."
  (when-let* ((id (superchat-work--focused session-id))
              ((superchat-work--within-p session-id (superchat-work-get id))))
    id))

;;;; Writing the facts ----------------------------------------------------------

(defun superchat-work--append (session-id type summary payload)
  "Append the Work Record fact TYPE with PAYLOAD to SESSION-ID's Tape."
  (superchat-tape-append-event
   (list :type type :session-id session-id :actor-id "work-records"
         :summary summary :payload payload)))

(defun superchat-work--run-id ()
  "The Run the current tool call belongs to, or nil."
  (and (boundp 'superchat--tool-execution-context)
       (plist-get superchat--tool-execution-context :run-id)))

(defun superchat-work--workspace (session-id)
  "(ID . ROOT) of the Workspace SESSION-ID is bound to, or nil without a root."
  (when (fboundp 'superchat-view-session-configuration)
    (let* ((config (superchat-view-session-configuration session-id))
           (id (plist-get config :workspace-id))
           (root (plist-get config :workspace-root)))
      (and (stringp id) (stringp root) (not (string-empty-p root)) (cons id root)))))

(defun superchat-work--require (id)
  "The open record ID, or signal why it cannot be written to."
  (let ((record (superchat-work-get id)))
    (cond ((null record) (error "No work record %s" id))
          ((plist-get record :conclusion)
           (error "Work record %s is completed; start a new one that follows it" id))
          (t record))))

(defun superchat-work--within-p (session-id record)
  "Non-nil when SESSION-ID's conversation may read and change RECORD.
A conversation of a Workspace: RECORD is that Workspace's.  A conversation
of no Workspace: RECORD was begun in that very conversation."
  (let ((workspace (car (superchat-work--workspace session-id))))
    (if workspace
        (equal workspace (plist-get record :workspace-id))
      (and (stringp session-id)
           (equal session-id (plist-get record :session-id))))))

(defun superchat-work--for (session-id id)
  "The Work Record ID for a tool call from SESSION-ID, or signal why not.
This is the model's way to a record: one of another Workspace is refused,
and the refusal says nothing of what the record holds.  The user's own
commands and the product's own writes reach every record."
  (let ((record (superchat-work-get id)))
    (cond ((null record) (error "No work record %s" id))
          ((not (superchat-work--within-p session-id record))
           (error "Work record %s belongs to another workspace; this conversation cannot read or change it" id))
          (t record))))

(defun superchat-work-start (session-id goal &optional follows)
  "Start a Work Record for GOAL in SESSION-ID and make it current; return its id.
FOLLOWS is the id of a completed record this one continues (K11).  Signals
when SESSION-ID belongs to no Workspace with a root (W10)."
  (unless (and (stringp goal) (not (string-empty-p (string-trim goal))))
    (error "A work record needs a goal: what is to be delivered"))
  (let ((workspace (superchat-work--workspace session-id)))
    (unless workspace
      (error "This conversation belongs to no workspace, so nothing is kept; the user can make a directory a workspace with /workspace DIRECTORY"))
    (when (and follows (not (superchat-work-get follows)))
      (error "No work record %s to follow" follows))
    (let ((id (format "work-%s-%04x" (format-time-string "%Y%m%d%H%M%S")
                      (random #x10000))))
      (superchat-work--append session-id "work.created"
                              (format "Work record started: %s" goal)
                              (list :work-id id :goal goal :follows follows
                                    :workspace-id (car workspace)
                                    :workspace-root (cdr workspace)
                                    :run-id (superchat-work--run-id)))
      (superchat-work--append session-id "work.focused"
                              (format "Current work record: %s" id)
                              (list :work-id id))
      id)))

(defun superchat-work-update (session-id id &optional progress blocked resume)
  "Record PROGRESS, BLOCKED and RESUME on the open record ID from SESSION-ID.
They answer how far it got, why the next step cannot be taken yet, and where
to pick it up; nil leaves a field as it was.  Makes ID current here."
  (superchat-work--require id)
  (unless (or progress blocked resume)
    (error "Say how far it got, why the next step waits, or where to pick it up"))
  (superchat-work--append session-id "work.updated"
                          (format "Work record updated: %s" id)
                          (list :work-id id :progress progress :blocked blocked
                                :resume resume :run-id (superchat-work--run-id)))
  (unless (equal id (superchat-work-current session-id))
    (superchat-work--append session-id "work.focused"
                            (format "Current work record: %s" id)
                            (list :work-id id)))
  id)

(defun superchat-work--check-wait (for until)
  "Signal when a wait FOR something, until UNTIL for a time, cannot be kept.
A wait for a time is a promise that the record is taken up again then.
While the `work-records' row is not mounted nothing would do that, so the
wait is refused rather than written and never acted on."
  (when (and for (not (member for superchat-work-wait-kinds)))
    (error "A record waits for one of: %s" (string-join superchat-work-wait-kinds ", ")))
  (when (equal for "time")
    (unless (numberp until)
      (error "A wait for a time needs the time: in_minutes or at"))
    (unless (superchat-work--wake-active-p)
      (error "Taking work up again at a time is turned off in this Emacs (the work-records row is not mounted), so this wait was not registered and nothing will happen at that time; tell the user, or register wait_for user"))))

(defun superchat-work-wait (session-id id for &optional detail until)
  "Register that the open record ID waits FOR (user, agent, time), with DETAIL.
For a time, UNTIL is when, in seconds since the epoch; the record is then
taken up again in a new conversation (`superchat-work-wake').  A nil FOR
clears the wait."
  (superchat-work--require id)
  (superchat-work--check-wait for until)
  (superchat-work--append session-id "work.waiting"
                          (cond ((equal for "time")
                                 (format "Work record %s waits until %s" id
                                         (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time until))))
                                (for (format "Work record %s waits for %s" id for))
                                (t (format "Work record %s no longer waits" id)))
                          (list :work-id id :for for :detail detail
                                :until (and (equal for "time") (float until))
                                :run-id (superchat-work--run-id)))
  (when (and (equal for "time") (superchat-work--wake-active-p))
    (superchat-work--arm id until))
  id)

(defun superchat-work-complete (session-id id conclusion &optional by)
  "Complete the open record ID with CONCLUSION; BY is `user' or `model' (default)."
  (superchat-work--require id)
  (unless (and (stringp conclusion) (not (string-empty-p (string-trim conclusion))))
    (error "Completing a work record needs a conclusion: what was delivered"))
  (superchat-work--append session-id "work.completed"
                          (format "Work record completed: %s" id)
                          (list :work-id id :conclusion conclusion
                                :by (symbol-name (or by 'model))
                                :run-id (superchat-work--run-id)))
  id)

;;;; Text -----------------------------------------------------------------------

(defun superchat-work--clip (text max)
  "TEXT on one line, cut to MAX characters."
  (let ((line (replace-regexp-in-string "[\n\t ]+" " " (string-trim (or text "")))))
    (if (> (length line) max) (concat (substring line 0 (- max 3)) "...") line)))

(defun superchat-work-record-task (session-id id task status report conversation)
  "Record on ID, from SESSION-ID, the delegated TASK, its STATUS and REPORT.
CONVERSATION is the sub-agent's own conversation.  Written by the product
when the sub-agent hands back (K14), whatever the record's state."
  (when (superchat-work-get id)
    (superchat-work--append session-id "work.task"
                            (format "Work record %s: delegated task %s" id status)
                            (list :work-id id
                                  :task (superchat-work--clip task 1000)
                                  :status status
                                  :report (superchat-work--clip report 2000)
                                  :conversation conversation))))

(defun superchat-work-jj-change (root)
  "The short id of the jj change at ROOT's working copy, or nil.
Nil unless ROOT holds a `.jj' directory and jj answers; never signals."
  (when (and (stringp root) (file-directory-p (expand-file-name ".jj" root)))
    (ignore-errors
      (with-temp-buffer
        (let ((default-directory root))
          (when (eq 0 (process-file "jj" nil t nil "log" "-r" "@" "--no-graph"
                                    "--ignore-working-copy"
                                    "-T" "change_id.short()"))
            (let ((id (string-trim (buffer-string))))
              (and (string-match-p "\\`[a-z]+\\'" id) id))))))))

(defun superchat-work-status-text (record)
  "RECORD's status in words."
  (pcase (superchat-work-status record)
    ('completed "completed")
    ('waiting-user (format "waiting for the user%s"
                           (if-let* ((d (plist-get (plist-get record :waiting) :detail)))
                               (format ": %s" d) "")))
    ('waiting-other (format "waiting for %s%s"
                            (let ((waiting (plist-get record :waiting)))
                              (if (equal "agent" (plist-get waiting :for))
                                  "another agent"
                                (format "%s"
                                        (if (numberp (plist-get waiting :until))
                                            (format-time-string "%Y-%m-%d %H:%M"
                                                                (seconds-to-time (plist-get waiting :until)))
                                          "a time"))))
                            (if-let* ((d (plist-get (plist-get record :waiting) :detail)))
                                (format ": %s" d) "")))
    (_ "continuable")))

(defun superchat-work-render (record)
  "RECORD's full text: the four questions, its status, and the product's facts."
  (let ((files (delete-dups
                (apply #'append
                       (mapcar (lambda (f) (append (plist-get f :files) nil))
                               (plist-get record :facts)))))
        (changes (delete-dups
                  (delq nil (mapcar (lambda (f) (plist-get f :jj-change))
                                    (plist-get record :facts))))))
    (string-join
     (delq nil
           (list (format "Work record %s (%s)" (plist-get record :id)
                         (superchat-work-status-text record))
                 (format "To deliver: %s" (plist-get record :goal))
                 (and (plist-get record :follows)
                      (format "Follows: %s" (plist-get record :follows)))
                 (format "Done so far: %s" (or (plist-get record :progress) "nothing recorded yet"))
                 ;; A completed record has no next step: these would be stale.
                 (and (plist-get record :blocked) (not (plist-get record :conclusion))
                      (format "Why the next step waits: %s" (plist-get record :blocked)))
                 (and (plist-get record :resume) (not (plist-get record :conclusion))
                      (format "Pick up from: %s" (plist-get record :resume)))
                 (and (equal "interrupted" (car (last (plist-get record :outcomes))))
                      (not (plist-get record :conclusion))
                      "Its last execution was cut off when Emacs exited (recorded by Superchat): check what it left before going on.")
                 (and files
                      (format "Files its executions changed (recorded by Superchat): %s"
                              (string-join files ", ")))
                 (and changes
                      (format "jj changes those executions left in the workspace: %s"
                              (string-join changes ", ")))
                 (and (plist-get record :tasks)
                      (concat "Delegated tasks (recorded by Superchat):\n"
                              (mapconcat
                               (lambda (task)
                                 (format "- %s -- %s: %s"
                                         (superchat-work--clip (plist-get task :task) 120)
                                         (plist-get task :status)
                                         (superchat-work--clip (plist-get task :report) 300)))
                               (plist-get record :tasks) "\n")))
                 (and (plist-get record :conclusion)
                      (format "Conclusion: %s" (plist-get record :conclusion)))))
     "\n")))

;;;; What a request carries (K8, K9) ------------------------------------------------

(defconst superchat-work-list-max 8
  "Most records listed in each group of a request's list.")

(defconst superchat-work-background-text
  "The conversation's earlier turns are background: do not take up earlier \
work again unless the user's message or the current work record asks for it."
  "What a request with a current record says about the turns before it (K9).")

(defun superchat-work-request-text (session-id)
  "The Work Record text of a request from SESSION-ID, or nil.
The current record in full, then this Workspace's other open and recently
completed records by status, one line each; never another Workspace's (K8)."
  (when-let* ((workspace (superchat-work--workspace session-id)))
    (let* ((records (superchat-work-records (car workspace)))
           (current (superchat-work-current session-id))
           (others (cl-remove current records
                              :key (lambda (r) (plist-get r :id)) :test #'equal))
           (week-ago (- (float-time) (* 7 86400)))
           (groups
            (list (cons "Continuable" (cl-remove-if-not
                                       (lambda (r) (eq 'continuable (superchat-work-status r)))
                                       others))
                  (cons "Waiting for the user" (cl-remove-if-not
                                                (lambda (r) (eq 'waiting-user (superchat-work-status r)))
                                                others))
                  (cons "Waiting for something else" (cl-remove-if-not
                                                      (lambda (r) (eq 'waiting-other (superchat-work-status r)))
                                                      others))
                  (cons "Recently completed"
                        (reverse (cl-remove-if-not
                                  (lambda (r) (and (eq 'completed (superchat-work-status r))
                                                   (> (superchat-work--started r) week-ago)))
                                  others)))))
           (listed (cl-remove-if-not #'cdr groups)))
      (when (or current listed)
        (string-join
         (delq nil
               (list
                "[Work records]"
                (if current
                    (concat "Current work record:\n"
                            (superchat-work-render (superchat-work-get current))
                            "\n" superchat-work-background-text)
                  "No current work record in this conversation.")
                (and listed
                     (concat
                      "Other work records in this workspace (read one with work_record action read):\n"
                      (mapconcat
                       (lambda (group)
                         (concat (car group) ":\n"
                                 (mapconcat (lambda (r)
                                              (format "- %s: %s" (plist-get r :id) (plist-get r :goal)))
                                            (seq-take (cdr group) superchat-work-list-max)
                                            "\n")))
                       listed "\n")))))
         "\n\n")))))

(defun superchat-work--started (record)
  "When RECORD began, in seconds, from the time in its id."
  (let ((id (plist-get record :id)))
    (if (string-match "\\`work-\\([0-9]\\{14\\}\\)" id)
        (let ((stamp (match-string 1 id)))
          (float-time (encode-time (list (string-to-number (substring stamp 12 14))
                                         (string-to-number (substring stamp 10 12))
                                         (string-to-number (substring stamp 8 10))
                                         (string-to-number (substring stamp 6 8))
                                         (string-to-number (substring stamp 4 6))
                                         (string-to-number (substring stamp 0 4))
                                         nil -1 nil))))
      0)))

;;;; The tool -----------------------------------------------------------------------

(defun superchat-work--tool-session ()
  (or (and (boundp 'superchat--tool-execution-context)
           (plist-get superchat--tool-execution-context :session-id))
      (and (boundp 'superchat--session-id) superchat--session-id)))

(defvar superchat--session-id)

(declare-function superchat-timer-parse-time "superchat-timer" (at))

(defun superchat-work-tool (action &optional id goal progress blocked resume
                                   wait_for detail conclusion follows
                                   in_minutes at)
  "The `work_record' tool: ACTION is start, update, wait, done or read."
  (condition-case err
      (let* ((session (superchat-work--tool-session))
             ;; The focused record, whoever's it is: one of another
             ;; Workspace is refused below, by name, like any other.
             (id (if (and (stringp id) (not (string-empty-p id)))
                     id
                   (superchat-work--focused session))))
        ;; The record named, and the one a new record follows, are this
        ;; conversation's Workspace's, before anything is read or written.
        (when (and id (member action '("update" "wait" "done" "read")))
          (superchat-work--for session id))
        (when (and (equal action "start")
                   (stringp follows) (not (string-empty-p follows)))
          (superchat-work--for session follows))
        (pcase action
          ("start"
           ;; What comes with a start is recorded with it, or the start is
           ;; refused: never "Started" with part of the call dropped.
           (when (and (stringp conclusion) (not (string-empty-p conclusion)))
             (error "A record is not started and completed in one call; start it, and use action done with the conclusion when it is delivered"))
           (let* ((waits (and (stringp wait_for) (not (string-empty-p wait_for))))
                  (until (and waits
                              (cond ((and (stringp at) (not (string-empty-p at)))
                                     (superchat-timer-parse-time at))
                                    ((numberp in_minutes)
                                     (+ (superchat-work--now) (* 60 in_minutes))))))
                  (_ (when waits (superchat-work--check-wait wait_for until)))
                  (new (superchat-work-start session goal follows))
                  (said (format "Started %s; it is this conversation's current work record."
                                new)))
             (when (or progress blocked resume)
               (superchat-work-update session new progress blocked resume))
             (when waits
               (superchat-work-wait session new wait_for detail until))
             (if (or progress blocked resume waits)
                 (concat said "\n" (superchat-work-render (superchat-work-get new)))
               said)))
          ("update"
           (unless id (error "No current work record; start one or give its id"))
           ;; A wait given with an update is registered too, never dropped: in
           ;; the real task T4W the model put wait_for and in_minutes on an
           ;; update and believed the wait was registered.  It is checked
           ;; before anything is written, so a refused wait leaves no half
           ;; of the call behind.
           (let* ((waits (and (stringp wait_for) (not (string-empty-p wait_for))))
                  (until (and waits
                              (cond ((and (stringp at) (not (string-empty-p at)))
                                     (superchat-timer-parse-time at))
                                    ((numberp in_minutes)
                                     (+ (superchat-work--now) (* 60 in_minutes)))))))
             (unless (or progress blocked resume waits)
               (error "Say how far it got, why the next step waits, or where to pick it up"))
             (when waits (superchat-work--check-wait wait_for until))
             (when (or progress blocked resume)
               (superchat-work-update session id progress blocked resume))
             (when waits
               (superchat-work-wait session id wait_for detail until)))
           (superchat-work-render (superchat-work-get id)))
          ("wait"
           (unless id (error "No current work record; start one or give its id"))
           (superchat-work-wait
            session id (and (stringp wait_for) (not (string-empty-p wait_for)) wait_for)
            detail
            (cond ((and (stringp at) (not (string-empty-p at)))
                   (superchat-timer-parse-time at))
                  ((numberp in_minutes) (+ (superchat-work--now) (* 60 in_minutes)))))
           (superchat-work-render (superchat-work-get id)))
          ("done"
           (unless id (error "No current work record; give its id"))
           (superchat-work-complete session id conclusion)
           (format "Completed %s." id))
          ("read"
           (unless id (error "Give the id of a work record"))
           (superchat-work-render (or (superchat-work-get id)
                                      (error "No work record %s" id))))
          (_ (error "action must be start, update, wait, done or read"))))
    (error (format "Error: %s" (error-message-string err)))))

;;;; For the user (K11) ---------------------------------------------------------------

(defconst superchat-work-list-buffer-name "*superchat work records*"
  "Name of the buffer `superchat-work-list' shows.")

(defun superchat-work--status-order (record)
  (pcase (superchat-work-status record)
    ('continuable 0) ('waiting-user 1) ('waiting-other 2) (_ 3)))

(defun superchat-work--list-entries ()
  "Rows of the records list: open ones first, newest first within a status."
  (mapcar (lambda (record)
            (list (plist-get record :id)
                  (vector (plist-get record :id)
                          (superchat-work-status-text record)
                          (let ((root (plist-get record :workspace-root)))
                            (if root (file-name-nondirectory (directory-file-name root)) ""))
                          (or (plist-get record :goal) ""))))
          (sort (reverse (superchat-work-records))
                (lambda (a b) (< (superchat-work--status-order a)
                                 (superchat-work--status-order b))))))

(defvar superchat-work-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'superchat-work-list-show)
    (define-key map (kbd "c") #'superchat-work-list-complete)
    map)
  "Keys of the records list.")

(define-derived-mode superchat-work-list-mode tabulated-list-mode "Work-Records"
  "The Work Records of every Workspace.  RET shows one, c completes it."
  (setq tabulated-list-format [("Record" 26 t) ("Status" 30 t) ("Workspace" 16 t) ("To deliver" 0 t)])
  (setq tabulated-list-entries #'superchat-work--list-entries)
  (tabulated-list-init-header))

;;;###autoload
(defun superchat-work-list ()
  "Show the Work Records of every Workspace, open ones first."
  (interactive)
  (let ((buffer (get-buffer-create superchat-work-list-buffer-name)))
    (with-current-buffer buffer
      (superchat-work-list-mode)
      (tabulated-list-print))
    (pop-to-buffer buffer)
    buffer))

(defun superchat-work-executions-text (record)
  "RECORD's executions, one per line, oldest first; nil when it has none.
For the user, who asks which execution did what; a request does not carry it."
  (when-let* ((executions (plist-get record :executions)))
    (concat "Executions (recorded by Superchat):\n"
            (mapconcat (lambda (e)
                         (format "- %s in conversation %s: %s"
                                 (plist-get e :run-id) (plist-get e :session)
                                 (plist-get e :outcome)))
                       executions "\n"))))

(defun superchat-work-list-show (id)
  "Show the full text of record ID, by default the one at point."
  (interactive (list (tabulated-list-get-id)))
  (when id
    (with-help-window "*superchat work record*"
      (let ((record (superchat-work-get id)))
        (princ (superchat-work-render record))
        (when-let* ((text (superchat-work-executions-text record)))
          (princ "\n")
          (princ text))))))

;;;###autoload
(defun superchat-work-mark-complete (id conclusion)
  "Complete the open record ID yourself, with CONCLUSION (K11)."
  (interactive
   (let* ((open (cl-remove-if (lambda (r) (plist-get r :conclusion))
                              (superchat-work-records)))
          (choice (completing-read
                   "Complete work record: "
                   (mapcar (lambda (r) (format "%s  %s" (plist-get r :id) (plist-get r :goal)))
                           open)
                   nil t)))
     (list (car (split-string choice)) (read-string "Conclusion: "))))
  (let ((record (superchat-work--require id)))
    (superchat-work-complete-by-user record conclusion)))

(defun superchat-work-complete-by-user (record conclusion)
  "Write the user's completion of RECORD, with CONCLUSION, to its own Tape."
  (superchat-work-complete (plist-get record :session-id) (plist-get record :id)
                           conclusion 'user))

(defun superchat-work-list-complete (id conclusion)
  "Complete the record at point (ID) with CONCLUSION, then refresh the list."
  (interactive (list (tabulated-list-get-id) (read-string "Conclusion: ")))
  (when id
    (superchat-work-complete-by-user (superchat-work--require id) conclusion)
    (tabulated-list-print t)))

;;;; Waking a record whose time has come (K10, T4) ------------------------------------

(declare-function superchat-workspace-new-conversation "superchat" (workspace))
(declare-function superchat-view-workspace-session-ids "superchat-tape-view" (workspace-id))
(declare-function superchat-editor-workspace-buffer "superchat-editor-workspace"
                  (session-id role &optional create))
(declare-function superchat--insert-persistent-system-message "superchat-render" (text))
(declare-function superchat-send-input "superchat-dispatcher" ())
(declare-function superchat-runtime-status "superchat-runtime" (run-id))
(defvar superchat-buffer-name)
(defvar superchat--active-run-id)
(defvar superchat-automated-input)
(defvar superchat-user-input-functions)

(defvar superchat-work-now-function #'float-time
  "Function returning the current time in seconds; tests replace it.")

(defvar superchat-work--armed (make-hash-table :test #'equal)
  "Record id -> the Emacs timer that will wake it.")

(defvar superchat-work-wake-retry-seconds 300
  "Seconds after a wake that started nothing before it is tried again.")

(defvar superchat-work-wake-retry-limit 3
  "How many times one Emacs tries a failed wake again by itself.
After that the record stays waiting for its past time until Superchat starts
again.")

(defvar superchat-work--retries (make-hash-table :test #'equal)
  "Record id -> how many wakes of it failed in this Emacs.")

(defconst superchat-work-wake-prompt
  "Take up work record %s again: the time it was waiting for has come."
  "The first message of the conversation a woken record starts in.")

(defun superchat-work--now () (funcall superchat-work-now-function))

(defun superchat-work--wake-active-p ()
  (and (fboundp 'superchat-plugin-row-active-p)
       (superchat-plugin-row-active-p 'work-records)))

(defun superchat-work--session-ran-p (session-id)
  "Non-nil when an execution ever started in SESSION-ID, by its Tape."
  (cl-some (lambda (event) (equal "run.started" (plist-get event :type)))
           (superchat-tape-replay-events session-id)))

(defun superchat-work--due-p (record)
  "Non-nil when RECORD waits for a time that no execution was started for.
A wake whose conversation never ran an execution does not count: sending
failed, or Emacs quit in between, and the wait was put back."
  (let* ((waiting (plist-get record :waiting))
         (until (plist-get waiting :until)))
    (and (equal "time" (plist-get waiting :for))
         (numberp until)
         (not (plist-get record :conclusion))
         (not (cl-some (lambda (wake)
                         (and (equal until (plist-get wake :until))
                              (superchat-work--session-ran-p (plist-get wake :session))))
                       (plist-get record :wakes))))))

(defun superchat-work--restore-wait (session-id record-id until detail)
  "Put back RECORD-ID's wait for the time UNTIL: its wake started nothing."
  (superchat-work--append session-id "work.waiting"
                          (format "Work record %s waits again: its wake started nothing"
                                  record-id)
                          (list :work-id record-id :for "time" :until until
                                :detail detail)))

(defun superchat-work--restore-lost-wake (record)
  "Put back RECORD's wait when its last wake was cut off; non-nil if it did.
Cut off means: the wake wrote its facts, no execution ever started in its
conversation, and nothing was written to the record since.  This is what an
Emacs that quit in the middle of a wake leaves behind."
  (let ((wake (car (last (plist-get record :wakes)))))
    (when (and wake
               (not (plist-get record :conclusion))
               (not (plist-get record :waiting))
               (equal (plist-get wake :session) (plist-get record :last-session))
               (not (superchat-work--session-ran-p (plist-get wake :session))))
      (superchat-work--restore-wait (plist-get wake :session) (plist-get record :id)
                                    (plist-get wake :until) (plist-get wake :detail))
      t)))

(defun superchat-work--disarm (id)
  (when-let* ((timer (gethash id superchat-work--armed)))
    (cancel-timer timer)
    (remhash id superchat-work--armed)))

(defun superchat-work-disarm-all ()
  "Cancel every Emacs timer set to wake a record."
  (maphash (lambda (_id timer) (cancel-timer timer)) superchat-work--armed)
  (clrhash superchat-work--armed))

(defun superchat-work--arm (id until)
  (superchat-work--disarm id)
  (puthash id (run-at-time (max 0 (- until (superchat-work--now))) nil
                           #'superchat-work--wake-if-due id)
           superchat-work--armed))

(defun superchat-work--wake-if-due (id)
  (remhash id superchat-work--armed)
  (let ((record (superchat-work-get id)))
    (when (and record (superchat-work--due-p record)
               (<= (plist-get (plist-get record :waiting) :until) (superchat-work--now)))
      (superchat-work--try-wake record))))

(defun superchat-work--try-wake (record)
  "Wake RECORD; return its View, or nil when the wake started nothing.
A failed wake is tried again after `superchat-work-wake-retry-seconds', at
most `superchat-work-wake-retry-limit' times in one Emacs."
  (let ((id (plist-get record :id)))
    (condition-case err
        (prog1 (superchat-work-wake record)
          (remhash id superchat-work--retries))
      (error
       (let ((failed (1+ (gethash id superchat-work--retries 0))))
         (puthash id failed superchat-work--retries)
         (message "superchat: work record %s was not taken up: %s"
                  id (error-message-string err))
         (when (<= failed superchat-work-wake-retry-limit)
           (superchat-work--disarm id)
           (puthash id (run-at-time superchat-work-wake-retry-seconds nil
                                    #'superchat-work--wake-if-due id)
                    superchat-work--armed)))
       nil))))


(defun superchat-work-wake (record)
  "Take RECORD up again in a new conversation of its Workspace; return the View.
Records `work.woken' first, naming the Workspace's main conversation; clears
the wait; makes RECORD the new conversation's current record; sends the wake
message.  When sending signals, the wait is put back, so the record is due
again.  When that execution ends, a line is left in the main conversation."
  (let* ((id (plist-get record :id))
         (waiting (plist-get record :waiting))
         (until (plist-get waiting :until))
         (detail (plist-get waiting :detail))
         (workspace (list :id (plist-get record :workspace-id)
                          :root (plist-get record :workspace-root)))
         (main (or (car (superchat-view-workspace-session-ids (plist-get workspace :id)))
                   (plist-get record :session-id)))
         (view (superchat-workspace-new-conversation workspace))
         (session (buffer-local-value 'superchat--session-id view))
         (sent nil))
    (superchat-work--append session "work.woken"
                            (format "Work record %s woken" id)
                            (list :work-id id :until until :detail detail :main main))
    (superchat-work--append session "work.waiting"
                            (format "Work record %s no longer waits" id)
                            (list :work-id id :for nil))
    (superchat-work--append session "work.focused"
                            (format "Current work record: %s" id)
                            (list :work-id id))
    (unwind-protect
        (with-current-buffer view
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (insert (format superchat-work-wake-prompt id)))
          ;; The product speaks here, not the user.
          (let ((superchat-automated-input t))
            (superchat-send-input))
          (setq sent t))
      (unless sent
        (superchat-work--restore-wait session id until detail)))
    view))

(defun superchat-work--run-of (session-id)
  "The id of the first Run that started in SESSION-ID, by its Tape, or nil."
  (cl-some (lambda (event)
             (and (equal "run.started" (plist-get event :type))
                  (plist-get event :run-id)))
           (superchat-tape-replay-events session-id)))

(defun superchat-work--run-ended-p (session-id run-id)
  "Non-nil when RUN-ID has an ordinary end on SESSION-ID's Tape.
A Run sealed as abandoned after a restart did not end; it was cut off."
  (cl-some (lambda (event)
             (and (equal run-id (plist-get event :run-id))
                  (member (plist-get event :type)
                          '("run.completed" "run.failed" "run.cancelled"))
                  (not (eq t (plist-get (plist-get event :payload) :abandoned)))))
           (superchat-tape-replay-events session-id)))

(defun superchat-work--report-cut-off (record)
  "Report RECORD's wakes whose execution an earlier Emacs never finished.
Such an execution is on the Tape, this Emacs does not know it, and the main
conversation was never told.  It is recorded as interrupted and not started
again; return non-nil when something was reported."
  (let (reported)
    (dolist (wake (plist-get record :wakes))
      (let* ((session (plist-get wake :session))
             (main (plist-get wake :main))
             (run (and main
                       (not (member session (plist-get record :reported)))
                       (superchat-work--run-of session))))
        (when (and run (null (superchat-runtime-status run)))
          (let* ((id (plist-get record :id))
                 (ended (superchat-work--run-ended-p session run))
                 (line (if ended
                           (format "Work record %s was taken up at its time in conversation %s: %s."
                                   id session (superchat-work-status-text record))
                         (format "Work record %s was taken up at its time in conversation %s, and that execution was cut off when Emacs exited. It was not started again."
                                 id session))))
            (unless ended
              (superchat-work--append
               session "work.outcome" (format "Work record %s: interrupted" id)
               (list :work-id id :run-id run :outcome "interrupted")))
            (superchat-work--append main "work.reported" line
                                    (list :work-id id :run-id run :conversation session))
            (when-let* ((view (and (fboundp 'superchat-editor-workspace-buffer)
                                   (superchat-editor-workspace-buffer main 'session-view)))
                        ((buffer-live-p view)))
              (ignore-errors
                (let ((superchat-buffer-name (buffer-name view)))
                  (with-current-buffer view
                    (superchat--insert-persistent-system-message line)))))
            (setq reported t)))))
    reported))

(defun superchat-work-wake-due ()
  "Wake every record whose time has come; return the Views started.
First an execution an earlier Emacs never finished is reported, and a wake
cut off before it started anything is put back.  One record's wake failing
does not stop the others."
  (dolist (record (superchat-work-records))
    (ignore-errors (superchat-work--report-cut-off record))
    (superchat-work--restore-lost-wake record))
  (let ((now (superchat-work--now)) views)
    (dolist (record (superchat-work-records))
      (when (and (superchat-work--due-p record)
                 (<= (plist-get (plist-get record :waiting) :until) now))
        (when-let* ((view (superchat-work--try-wake record)))
          (push view views))))
    (nreverse views)))


(defun superchat-work-arm-all ()
  "Wake the records whose time came while nothing was armed, arm the rest.
A record whose wake just failed keeps the retry `superchat-work--try-wake'
armed for it."
  (superchat-work-disarm-all)
  (superchat-work-wake-due)
  (let ((now (superchat-work--now)))
    (dolist (record (superchat-work-records))
      (when (and (superchat-work--due-p record)
                 (> (plist-get (plist-get record :waiting) :until) now))
        (superchat-work--arm (plist-get record :id)
                             (plist-get (plist-get record :waiting) :until))))))


(defun superchat-work--report-woken (run status)
  "When the execution RUN of a woken record ends, note it in the main conversation.
Which record RUN took up, and where to say so, is read from the `work.woken'
fact of RUN's conversation, so it holds across an Emacs restart."
  (let ((session (plist-get status :session-id)))
    (dolist (record (superchat-work-records))
      (when-let* ((wake (cl-find session (plist-get record :wakes)
                                 :key (lambda (w) (plist-get w :session)) :test #'equal))
                  (main (plist-get wake :main))
                  ((not (member session (plist-get record :reported)))))
        (let* ((id (plist-get record :id))
               (line (format "Work record %s was taken up at its time in conversation %s: %s."
                             id session (superchat-work-status-text record))))
          (superchat-work--append main "work.reported" line
                                  (list :work-id id :run-id run :conversation session))
          (when-let* ((view (and (fboundp 'superchat-editor-workspace-buffer)
                                 (superchat-editor-workspace-buffer main 'session-view)))
                      ((buffer-live-p view)))
            (ignore-errors
              (let ((superchat-buffer-name (buffer-name view)))
                (with-current-buffer view
                  (superchat--insert-persistent-system-message line))))))))))

;;;; The user's answer (slice 9) ------------------------------------------------------

(defun superchat-work--on-user-input (session-id _text)
  "The user spoke in SESSION-ID: its current record stops waiting for the user.
The user's message is the signal the wait was registered for (K3), so the
product clears it; the model does not have to remember to."
  (when-let* ((id (superchat-work-current session-id))
              (record (superchat-work-get id))
              ((eq 'waiting-user (superchat-work-status record))))
    (superchat-work--append session-id "work.waiting"
                            (format "Work record %s: the user answered" id)
                            (list :work-id id :for nil :answered t))))

;;;; The end of an execution (K3, K7) -----------------------------------------

(defun superchat-work--facts-in-run (record-id run-id)
  "The Work Record facts of RECORD-ID written during RUN-ID, as (TYPE . PAYLOAD)."
  (delq nil
        (mapcar (lambda (row)
                  (cl-destructuring-bind (_session type payload) row
                    (and (equal record-id (superchat-work--get payload 'work_id 'work-id))
                         (equal run-id (superchat-work--get payload 'run_id 'run-id))
                         (cons type payload))))
                (superchat-work--rows))))

(defun superchat-work-outcome (record-id run-id run-status)
  "How RUN-ID, ended with RUN-STATUS, left RECORD-ID, from facts alone.
`completed' when it completed the record; `waiting' when it registered a wait
still standing; `failed' when the Run failed; otherwise `continuable'.  The
answer's words are never read (K7)."
  (let* ((facts (superchat-work--facts-in-run record-id run-id))
         (record (superchat-work-get record-id)))
    (cond ((assoc "work.completed" facts) 'completed)
          ((and (assoc "work.waiting" facts) (plist-get record :waiting)) 'waiting)
          ((not (eq run-status 'completed)) 'failed)
          (t 'continuable))))

(defun superchat-work--on-turn-completed (_run status)
  "At the end of an execution, attach its facts and its outcome to the record.
The record is the conversation's current one, or one this execution completed."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status)))
    (let* ((session (plist-get status :session-id))
           (run (plist-get status :run-id))
           (ids (delete-dups
                 (delq nil
                       (cons (superchat-work-current session)
                             (mapcar (lambda (row)
                                       (cl-destructuring-bind (s type payload) row
                                         (and (equal s session)
                                              (equal type "work.completed")
                                              (equal run (superchat-work--get payload 'run_id 'run-id))
                                              (superchat-work--get payload 'work_id 'work-id))))
                                     (superchat-work--rows)))))))
      (dolist (id ids)
        (ignore-errors
          (let ((rows (and (fboundp 'superchat-changes-candidates)
                           (superchat-changes-candidates session run))))
            (when rows
              (superchat-work--append
               session "work.facts" (format "Work record %s: what the execution changed" id)
               (list :work-id id :run-id run
                     :buffers (vconcat (mapcar (lambda (r) (plist-get r :name)) rows))
                     :files (vconcat (delq nil (mapcar (lambda (r) (plist-get r :file)) rows)))
                     ;; K13: in a jj repository, the change these edits sit in.
                     :jj-change (superchat-work-jj-change
                                 (plist-get (superchat-work-get id) :workspace-root))))))
          (let ((outcome (superchat-work-outcome id run (plist-get status :status))))
            (superchat-work--append
             session "work.outcome" (format "Work record %s: %s" id outcome)
             (list :work-id id :run-id run :outcome (symbol-name outcome))))))
      (ignore-errors (superchat-work--report-woken run status)))))

(defvar superchat-work--start-timer nil
  "The Emacs timer that arms every wake once `work-records' is mounted.")

(superchat-define-plugin work-records
  :doc "Work Records: at the end of every execution, the product attaches what
it changed to the conversation's current record and decides how it stands."
  :apply
  (lambda (scope _config)
    (superchat-on scope 'turn-completed 'work-outcome
                  #'superchat-work--on-turn-completed)
    ;; One hook and one table of wakes for the whole Emacs: the first
    ;; installation sets them up, the last one out takes them down.
    (superchat-scope-shared-effect
     scope 'work-records-armed
     (lambda ()
       (add-hook 'superchat-user-input-functions #'superchat-work--on-user-input)
       ;; After loading finishes, so a missed wake never starts mid-`require'.
       (setq superchat-work--start-timer
             (run-at-time 0 nil #'superchat-work-arm-all)))
     (lambda ()
       (remove-hook 'superchat-user-input-functions
                    #'superchat-work--on-user-input)
       (when (timerp superchat-work--start-timer)
         (cancel-timer superchat-work--start-timer))
       (setq superchat-work--start-timer nil)
       (superchat-work-disarm-all)))))

(provide 'superchat-work)
;;; superchat-work.el ends here
