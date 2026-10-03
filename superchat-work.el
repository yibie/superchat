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
    "work.focused" "work.facts" "work.outcome")
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
                                                 :detail (alist-get 'detail payload))))
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
                                                          :buffers (alist-get 'buffers payload)))))
                        table)))
            ("work.outcome"
             (when record
               (puthash id (plist-put record :outcomes
                                      (append (plist-get record :outcomes)
                                              (list (alist-get 'outcome payload))))
                        table)))))))
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

(defun superchat-work-current (session-id)
  "The id of SESSION-ID's current Work Record, or nil.
That is the record of its last `work.focused' fact, while not completed."
  (let (current)
    (dolist (row (superchat-work--rows))
      (cl-destructuring-bind (session type payload) row
        (when (and (equal session session-id) (equal type "work.focused"))
          (setq current (superchat-work--get payload 'work_id 'work-id)))))
    (and current
         (let ((record (superchat-work-get current)))
           (and record (not (plist-get record :conclusion)) current)))))

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

(defun superchat-work-wait (session-id id for &optional detail)
  "Register that the open record ID waits FOR (user, agent, time), with DETAIL.
A nil FOR clears the wait."
  (superchat-work--require id)
  (when (and for (not (member for superchat-work-wait-kinds)))
    (error "A record waits for one of: %s" (string-join superchat-work-wait-kinds ", ")))
  (superchat-work--append session-id "work.waiting"
                          (if for (format "Work record %s waits for %s" id for)
                            (format "Work record %s no longer waits" id))
                          (list :work-id id :for for :detail detail
                                :run-id (superchat-work--run-id)))
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

(defun superchat-work-status-text (record)
  "RECORD's status in words."
  (pcase (superchat-work-status record)
    ('completed "completed")
    ('waiting-user (format "waiting for the user%s"
                           (if-let* ((d (plist-get (plist-get record :waiting) :detail)))
                               (format ": %s" d) "")))
    ('waiting-other (format "waiting for %s%s"
                            (if (equal "agent" (plist-get (plist-get record :waiting) :for))
                                "another agent" "a time")
                            (if-let* ((d (plist-get (plist-get record :waiting) :detail)))
                                (format ": %s" d) "")))
    (_ "continuable")))

(defun superchat-work-render (record)
  "RECORD's full text: the four questions, its status, and the product's facts."
  (let ((files (delete-dups
                (apply #'append
                       (mapcar (lambda (f) (append (plist-get f :files) nil))
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
                 (and files
                      (format "Files its executions changed (recorded by Superchat): %s"
                              (string-join files ", ")))
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

(defun superchat-work-tool (action &optional id goal progress blocked resume
                                   wait_for detail conclusion follows)
  "The `work_record' tool: ACTION is start, update, wait, done or read."
  (condition-case err
      (let* ((session (superchat-work--tool-session))
             (id (if (and (stringp id) (not (string-empty-p id)))
                     id
                   (superchat-work-current session))))
        (pcase action
          ("start"
           (let ((new (superchat-work-start session goal follows)))
             (format "Started %s; it is this conversation's current work record."
                     new)))
          ("update"
           (unless id (error "No current work record; start one or give its id"))
           (superchat-work-update session id progress blocked resume)
           (superchat-work-render (superchat-work-get id)))
          ("wait"
           (unless id (error "No current work record; start one or give its id"))
           (superchat-work-wait session id (and (stringp wait_for)
                                                (not (string-empty-p wait_for))
                                                wait_for)
                                detail)
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

(defun superchat-work-list-show (id)
  "Show the full text of record ID, by default the one at point."
  (interactive (list (tabulated-list-get-id)))
  (when id
    (with-help-window "*superchat work record*"
      (princ (superchat-work-render (superchat-work-get id))))))

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
                     :files (vconcat (delq nil (mapcar (lambda (r) (plist-get r :file)) rows)))))))
          (let ((outcome (superchat-work-outcome id run (plist-get status :status))))
            (superchat-work--append
             session "work.outcome" (format "Work record %s: %s" id outcome)
             (list :work-id id :run-id run :outcome (symbol-name outcome)))))))))

(superchat-define-plugin work-records
  :doc "Work Records: at the end of every execution, the product attaches what
it changed to the conversation's current record and decides how it stands."
  :apply
  (lambda (scope _config)
    (superchat-on scope 'turn-completed 'work-outcome
                  #'superchat-work--on-turn-completed)))

(provide 'superchat-work)
;;; superchat-work.el ends here
