;;; superchat-subagent.el --- Isolated sub-agents for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Sub-agents let the main session delegate isolated tasks to
;; specialized presets (researcher, executor, introspector).  They run
;; with their own session_id and tape, and return a concise report that
;; is rendered in the main chat buffer.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-tape)
(defvar superchat--session-id)
(require 'superchat-projection)
(require 'json)
(require 'superchat-preset)
(require 'superchat-tool-execution)
(require 'superchat-sink)

;; External declarations
(declare-function superchat-sandbox-set-mode "superchat-sandbox" (session-id mode &optional pin))
(declare-function superchat-sandbox-effective-mode "superchat-sandbox" (session-id))
(declare-function superchat-approval-set-policy "superchat-approval" (session-id policy &optional pin))
(declare-function superchat-preset-apply "superchat-preset" (preset turn))
(declare-function superchat--execute-llm-query "superchat-dispatcher" (turn &optional template target-model))
(declare-function superchat--agent-wrap-tools "superchat-agent-loop" (tools &optional preset))
(declare-function superchat-get-llm-tools "superchat-tools" ())
(declare-function superchat--collect-llm-tools "superchat-llm" (&optional input tool-names))
(defvar superchat--conversation-history)
(defvar superchat--active-preset)
(defvar superchat--workspace-binding)
(defvar superchat-buffer-name)

(declare-function superchat-skills-get-available "superchat-skills" ())
(declare-function superchat-skills-load "superchat-skills" (skill-name))

;; ═══════════════════════════════════════════════════════════
;; Built-in sub-agent presets
;; ═══════════════════════════════════════════════════════════

(defun superchat--subagent-make-preset
    (name profile-id description type capability tools body)
  "Create a built-in sub-agent preset."
  (superchat-preset-from-plist
   (list :name name
         :profile-id profile-id
         :description description
         :type type
         :capability capability
         :tools tools
         :body body
         :source 'builtin
         :version "1.0")))

(defun superchat--subagent-preset-researcher ()
  "Return the built-in `researcher' sub-agent preset."
  (superchat--subagent-make-preset
   "researcher"
   "researcher"
   "Research sub-agent with durable Workspace delivery"
   'agent
   "research"
   '("list_buffers" "read_buffer" "eval_elisp"
     "workspace_read" "workspace_publish" "workspace_info")
   "You are a research sub-agent. Investigate using read-only source tools, then publish the requested deliverable with workspace_publish. Do not modify external files or execute commands."))

(defun superchat--subagent-preset-executor ()
  "Return the built-in `executor' sub-agent preset."
  (superchat--subagent-make-preset
   "executor"
   "executor"
   "Autonomous execution sub-agent"
   'agent
   "coding"
   '("list_buffers" "read_buffer" "eval_elisp" "shell-command"
     "edit_buffer" "replace_buffer"
     "workspace_read" "workspace_write" "workspace_info")
   "You are an execution sub-agent. Carry out the user's task autonomously using the available tools. You may read, write, edit, and run shell commands. Produce a concise final report summarizing what you did and the outcome."))

(defun superchat--subagent-preset-introspector ()
  "Return the built-in `introspector' sub-agent preset."
  (superchat--subagent-make-preset
   "introspector"
   "introspector"
   "Emacs introspection sub-agent"
   'agent
   "introspection"
   '("read_buffer" "describe-function" "describe-variable" "eval_elisp"
     "workspace_read" "workspace_info")
   "You are an Emacs introspection sub-agent. Use the available tools to inspect Emacs state, documentation, and Elisp. Produce a concise report with the requested information."))

(defconst superchat--subagent-builtin-presets
  '(("researcher" . superchat--subagent-preset-researcher)
    ("executor" . superchat--subagent-preset-executor)
    ("introspector" . superchat--subagent-preset-introspector))
  "Built-in sub-agent names and their preset constructors.")

(defun superchat--subagent-custom-name (name)
  "Return the on-disk skill name matching NAME case-insensitively."
  (when (fboundp 'superchat-skills-get-available)
    (let ((key (downcase name)))
      (cl-find-if (lambda (candidate)
                    (string= key (downcase candidate)))
                  (superchat-skills-get-available)))))

(defun superchat--subagent-preset (name)
  "Return a sub-agent preset by NAME (symbol or string)."
  (let* ((key (downcase (if (symbolp name) (symbol-name name) name)))
         (builtin (assoc key superchat--subagent-builtin-presets)))
    (if builtin
        (funcall (cdr builtin))
      (let* ((custom-name (superchat--subagent-custom-name key))
             (skill (and custom-name
                         (fboundp 'superchat-skills-load)
                         (superchat-skills-load custom-name))))
        (when (and skill (superchat-preset-agent-p skill))
          skill)))))

(defun superchat--subagent-registry ()
  "Return callable sub-agent names paired with their presets.
Built-ins come first and win name collisions; custom agents are sorted
by the skill filename used to invoke them."
  (let* ((builtins (mapcar (lambda (entry)
                             (cons (car entry) (funcall (cdr entry))))
                           superchat--subagent-builtin-presets))
         (seen-names (mapcar #'car builtins))
         (custom-names (if (fboundp 'superchat-skills-get-available)
                           (sort (copy-sequence
                                  (superchat-skills-get-available))
                                 #'string-lessp)
                         nil)))
    (append
     builtins
     (delq nil
           (mapcar (lambda (source-name)
                     (let ((name (downcase source-name)))
                       (unless (member name seen-names)
                         (push name seen-names)
                         (let ((preset (superchat-skills-load source-name)))
                         (when (and preset (superchat-preset-agent-p preset))
                             (cons name preset))))))
                   custom-names)))))

;; ═══════════════════════════════════════════════════════════
;; Team coordination — Tape-backed tasks and mailbox
;; ═══════════════════════════════════════════════════════════

(defvar superchat--team-id nil
  "Dynamically bound team id for the current agent tool call.")

(defvar superchat--team-actor nil
  "Dynamically bound actor id for the current agent tool call.")

(defun superchat--team-current-id ()
  "Return the current team id."
  (or superchat--team-id
      (and (boundp 'superchat--session-id) superchat--session-id)
      (error "No active Superchat session")))

(defun superchat--team-current-actor ()
  "Return the current actor id."
  (or superchat--team-actor "lead"))

(defun superchat--team-meta (json)
  "Decode event metadata JSON into an alist."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (condition-case nil
        (json-read-from-string (or json "{}"))
      (error nil))))

(defun superchat--team-event (row)
  "Normalize legacy or Runtime team ROW into a projection event."
  (let* ((id (nth 0 row))
         (topic (nth 1 row))
         (content (nth 2 row))
         (meta (superchat--team-meta (nth 3 row)))
         (runtime-type (alist-get 'event_type meta))
         (payload (or (alist-get 'payload meta) meta))
         (type
          (or runtime-type
              (pcase topic
                ("team/task"
                 (if (equal (alist-get 'event meta) "created")
                     "team.task.created"
                   "team.task.updated"))
                ("team/message" "team.message.sent")
                ("team/decision" "team.decision.published")
                ("team/member"
                 (format "subagent.%s" (alist-get 'event meta)))))))
    (list :id id
          :storage-id id
          :event-id (alist-get 'event_id meta)
          :actor-id (alist-get 'actor_id meta)
          :profile-id (alist-get 'profile_id meta)
          :run-id (alist-get 'run_id meta)
          :parent-run-id (alist-get 'parent_run_id meta)
          :type type
          :topic topic
          :content content
          :payload payload
          :created-at (nth 4 row))))

(defun superchat--team-projection-initial ()
  "Return empty Team projection state."
  (list :tasks (make-hash-table :test #'equal)
        :task-order nil
        :messages nil
        :decisions nil
        :members nil))

(defun superchat--team-projection-apply (state row)
  "Apply team Tape ROW to projection STATE."
  (let* ((event (superchat--team-event row))
         (id (plist-get event :storage-id))
         (event-id (plist-get event :event-id))
         (type (plist-get event :type))
         (payload (plist-get event :payload))
         (tasks (plist-get state :tasks))
         (task-id (or (alist-get 'task_id payload)
                      (and (equal type "team.task.created")
                           (format "T%d" id))))
         (task (and task-id (gethash task-id tasks))))
    (pcase type
      ("team.task.created"
       (unless task
         (setq state
               (plist-put state :task-order
                          (cons task-id (plist-get state :task-order)))))
       (puthash
        task-id
        (list :id task-id
              :title (or (alist-get 'title payload) "")
              :status (or (alist-get 'status payload) "pending")
              :owner (alist-get 'owner payload)
              :depends-on (or (alist-get 'depends_on payload) nil))
        tasks))
      ("team.task.updated"
       (when task
         (dolist (field '((status . :status)
                          (owner . :owner)
                          (title . :title)
                          (depends_on . :depends-on)))
           (when (assq (car field) payload)
             (setq task
                   (plist-put task (cdr field)
                              (alist-get (car field) payload)))))
         (puthash task-id task tasks)))
      ("team.message.sent"
       (setq state
             (plist-put
              state :messages
              (cons
               (list :id (format "M%d" id)
                     :event-id event-id
                     :storage-id id
                     :from (alist-get 'from payload)
                     :to (alist-get 'to payload)
                     :content (or (alist-get 'content payload)
                                  (plist-get event :content))
                     :created-at (plist-get event :created-at))
               (plist-get state :messages)))))
      ("team.decision.published"
       (setq state
             (plist-put
              state :decisions
              (cons
               (list :id (format "D%d" id)
                     :event-id event-id
                     :storage-id id
                     :from (alist-get 'from payload)
                     :content (or (alist-get 'content payload)
                                  (plist-get event :content))
                     :created-at (plist-get event :created-at))
               (plist-get state :decisions)))))
      ((or "subagent.started" "subagent.completed" "subagent.failed")
       (let ((member
              (list
               :event-id event-id
               :storage-id id
               :actor-id
               (or (plist-get event :actor-id)
                   (alist-get 'actor_id payload)
                   (alist-get 'agent_id payload))
               :profile-id
               (or (plist-get event :profile-id)
                   (alist-get 'profile_id payload)
                   (alist-get 'preset payload))
               :run-id
               (or (plist-get event :run-id)
                   (alist-get 'run_id payload))
               :parent-run-id
               (or (plist-get event :parent-run-id)
                   (alist-get 'parent_run_id payload))
               :status (string-remove-prefix "subagent." type)
               :task-id (alist-get 'task_id payload))))
         (setq state
               (plist-put state :members
                          (cons member (plist-get state :members)))))))
    state))


(defun superchat--team-state (&optional team-id)
  "Replay the logical Team state for TEAM-ID."
  (superchat-service-call nil 'tape 'replay-projection
   'team
   (superchat-service-call nil 'tape 'select
    "SELECT id, topic, content, meta, created_at
     FROM tape
     WHERE session_id = ? AND topic IN
       ('team/task', 'team/message', 'team/decision', 'team/member')
     ORDER BY id ASC"
    (list (or team-id (superchat--team-current-id))))))

(defun superchat--team-dependencies (value)
  "Normalize dependency VALUE to a list of task ids."
  (cond
   ((null value) nil)
   ((listp value) value)
   ((and (stringp value) (string-empty-p value)) nil)
   ((stringp value)
    (let ((parsed (let ((json-array-type 'list))
                    (json-read-from-string value))))
      (unless (listp parsed)
        (error "depends_on must be a JSON array"))
      parsed))
   (t (error "depends_on must be a JSON array"))))

(defun superchat--team-task-event (team-id event content meta)
  "Append a task EVENT with CONTENT and META to TEAM-ID."
  (superchat-service-call nil 'tape 'append-event
   (list :type (if (equal event "created")
                   "team.task.created"
                 "team.task.updated")
         :session-id team-id
         :team-id team-id
         :actor-id (or (alist-get 'actor meta) "lead")
         :topic "team/task"
         :summary content
         :requires-payload t
         :payload meta)))

(defun superchat-team-tasks (&optional team-id)
  "Project the current task list for TEAM-ID from Tape events."
  (let* ((state (superchat--team-state team-id))
         (tasks (plist-get state :tasks)))
    (mapcar (lambda (task-id) (gethash task-id tasks))
            (nreverse (plist-get state :task-order)))))

(defun superchat--team-task-find (task-id &optional team-id)
  "Return TASK-ID from TEAM-ID's projected task list."
  (cl-find task-id (superchat-team-tasks team-id)
           :key (lambda (task) (plist-get task :id))
           :test #'equal))

(defun superchat--team-task-patch (team-id task-id actor fields)
  "Append FIELDS as a task patch by ACTOR in TEAM-ID."
  (superchat--team-task-event
   team-id "patched" (format "%s updated %s" actor task-id)
   (append (list (cons 'task_id task-id)
                 (cons 'actor actor))
           fields))
  (superchat--team-task-find task-id team-id))

(defun superchat-team-task
    (action &optional task-id title owner depends-on)
  "Perform ACTION on the current team's task list.
Supported actions are create, list, claim, and complete."
  (let* ((team-id (superchat--team-current-id))
         (actor (superchat--team-current-actor))
         (tasks (superchat-team-tasks team-id))
         (task (and task-id
                    (cl-find task-id tasks
                             :key (lambda (item) (plist-get item :id))
                             :test #'equal))))
    (pcase action
      ("list" tasks)
      ("create"
       (unless (and (stringp title) (not (string-empty-p title)))
         (error "create requires a non-empty title"))
       (let ((dependencies (superchat--team-dependencies depends-on)))
         (dolist (dependency dependencies)
           (unless (cl-find dependency tasks
                            :key (lambda (item) (plist-get item :id))
                            :test #'equal)
             (error "Unknown dependency: %s" dependency)))
         (let ((row-id
                (superchat--team-task-event
                 team-id "created" title
                 (list (cons 'title title)
                       (cons 'status "pending")
                       (cons 'owner owner)
                       (cons 'depends_on dependencies)
                       (cons 'actor actor)))))
           (superchat--team-task-find (format "T%d" row-id) team-id))))
      ((or "claim" "complete")
       (unless task
         (error "Unknown task: %s" task-id))
       (if (equal action "claim")
           (progn
             (unless (equal (plist-get task :status) "pending")
               (error "Task %s is not pending" task-id))
             (when-let* ((assigned (plist-get task :owner)))
               (unless (equal assigned actor)
                 (error "Task %s is assigned to %s" task-id assigned)))
             (dolist (dependency (plist-get task :depends-on))
               (let ((required (cl-find dependency tasks
                                        :key (lambda (item)
                                               (plist-get item :id))
                                        :test #'equal)))
                 (unless (equal (plist-get required :status) "completed")
                   (error "Task %s is blocked by %s" task-id dependency))))
             (superchat--team-task-patch
              team-id task-id actor
              (list (cons 'status "in_progress")
                    (cons 'owner actor))))
         (unless (equal (plist-get task :status) "in_progress")
           (error "Task %s is not in progress" task-id))
         (unless (or (equal actor "lead")
                     (equal actor (plist-get task :owner)))
           (error "Task %s belongs to %s" task-id
                  (plist-get task :owner)))
         (superchat--team-task-patch
          team-id task-id actor (list (cons 'status "completed")))))
      (_ (error "Unknown team_task action: %s" action)))))

(defun superchat-team-message (action &optional to content since-id)
  "Send or list mailbox messages for the current team.
ACTION is send, list, publish, or decisions.  TO may be an actor id,
lead, or all.  Publish records a shared decision."
  (let ((team-id (superchat--team-current-id))
        (actor (superchat--team-current-actor)))
    (pcase action
      ("send"
       (unless (and (stringp to) (not (string-empty-p to)))
         (error "send requires a recipient"))
       (unless (and (stringp content) (not (string-empty-p content)))
         (error "send requires non-empty content"))
       (let ((id
              (superchat-service-call nil 'tape 'append-event
               (list :type "team.message.sent"
                     :session-id team-id
                     :team-id team-id
                     :actor-id actor
                     :audience "team"
                     :topic "team/message"
                     :summary (format "%s -> %s" actor to)
                     :requires-payload t
                     :payload
                     (list (cons 'from actor)
                           (cons 'to to)
                           (cons 'content content))))))
         (format "Message M%d sent to %s" id to)))
      ("list"
       (cl-remove-if-not
        (lambda (message)
          (and (or (equal (plist-get message :to) actor)
                   (equal (plist-get message :to) "all"))
               (or (null since-id)
                   (> (plist-get message :storage-id) since-id))))
        (nreverse
         (plist-get (superchat--team-state team-id) :messages))))
      ("publish"
       (unless (and (stringp content) (not (string-empty-p content)))
         (error "publish requires non-empty content"))
       (let ((id
              (superchat-service-call nil 'tape 'append-event
               (list :type "team.decision.published"
                     :session-id team-id
                     :team-id team-id
                     :actor-id actor
                     :audience "team"
                     :topic "team/decision"
                     :summary (format "%s published a decision" actor)
                     :requires-payload t
                     :payload
                     (list (cons 'from actor)
                           (cons 'content content))))))
         (format "Decision D%d published" id)))
      ("decisions"
       (nreverse
        (plist-get (superchat--team-state team-id) :decisions)))
      (_ (error "Unknown team_message action: %s" action)))))

(defun superchat--team-member-event (ctx event)
  "Record teammate CTX lifecycle EVENT on the shared team Tape.
EVENT must be one of `superchat-tape-subagent-event-suffixes'; that is
the inventory the dynamic `subagent.%s' type is recorded under."
  (unless (member event superchat-tape-subagent-event-suffixes)
    (error "Unknown subagent lifecycle event `%s'; expected one of %S"
           event superchat-tape-subagent-event-suffixes))
  (let ((actor-id (plist-get ctx :actor-id))
        (profile-id (plist-get ctx :profile-id))
        (run-id (plist-get ctx :run-id))
        (parent-run-id (plist-get ctx :parent-run-id)))
    (superchat-service-call nil 'tape 'append-event
     (list :type (format "subagent.%s" event)
           :session-id (plist-get ctx :team-id)
           :team-id (plist-get ctx :team-id)
           :run-id run-id
           :parent-run-id parent-run-id
           :actor-id actor-id
           :profile-id profile-id
           :topic "team/member"
           :summary (format "%s %s" actor-id event)
           :requires-payload t
           :payload
           (list (cons 'event event)
                 (cons 'actor_id actor-id)
                 (cons 'profile_id profile-id)
                 (cons 'run_id run-id)
                 (cons 'parent_run_id parent-run-id)
                 (cons 'task_id (plist-get ctx :task-id)))))))

(defun superchat--team-start-delegated-task (ctx title)
  "Create and claim TITLE for teammate CTX."
  (let* ((superchat--team-id (plist-get ctx :team-id))
         (superchat--team-actor "lead")
         (task (superchat-team-task
                "create" nil title (plist-get ctx :actor-id))))
    (let ((superchat--team-actor (plist-get ctx :actor-id)))
      (superchat-team-task "claim" (plist-get task :id)))))

(defun superchat--team-finish-delegated-task (entry report)
  "Update ENTRY's task and member state from REPORT."
  (let* ((team-id (plist-get entry :team-id))
         (actor-id (plist-get entry :actor-id))
         (task-id (plist-get entry :task-id))
         (failed (and (stringp report)
                      (string-match-p
                       "\\`\\[\\(?:Error\\|Cancelled\\|Timed out\\|Tool error\\)"
                       report)))
         (event (if failed "failed" "completed")))
    (when (and team-id actor-id task-id)
      (let* ((superchat--team-id team-id)
             (superchat--team-actor actor-id)
             (task (superchat--team-task-find task-id team-id)))
        (when (equal (plist-get task :status) "in_progress")
          (if failed
              (superchat--team-task-patch
               team-id task-id actor-id
               (list (cons 'status "pending") (cons 'owner nil)))
            (superchat-team-task "complete" task-id)))))
    ;; A failed Runtime admission has no accepted child Run to publish.
    (when (and team-id actor-id (plist-get entry :run-id))
      (superchat--team-member-event entry event))))

;; ═══════════════════════════════════════════════════════════
;; Sub-agent runner
;; ═══════════════════════════════════════════════════════════

(defcustom superchat-subagent-parallel-max 3
  "Maximum number of sub-agents in flight at once.
In the async engine this is a launch window: additional specs
queue and start as running sub-agents complete."
  :type 'integer
  :group 'superchat)

(defcustom superchat-subagent-max-depth 1
  "Maximum delegation depth for nested sub-agents.
The main session delegates at depth 1.  A sub-agent may itself
delegate only while its depth is below this limit, so the default
of 1 means sub-agents cannot delegate further."
  :type 'integer
  :group 'superchat)

;; ═══════════════════════════════════════════════════════════
;; Async engine — explicit context (v1.3)
;; ═══════════════════════════════════════════════════════════
;;
;; The engine threads an explicit context plist through closures:
;; tape writes and depth checks read the context, never the dynamic
;; environment.  All async callbacks run on the main thread, so there
;; is no data race — only completion-order nondeterminism, which the
;; parallel aggregator resolves with spec-indexed result slots.

(defvar superchat-llm-backend)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
(declare-function superchat--llm-round-limit-action "superchat-llm" ())
(declare-function make-llm-tool "llm")
(declare-function json-read-from-string "json" (string))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function superchat--agent-run-tool-hooks "superchat-agent-loop" (hooks &rest args))
(declare-function superchat--agent-effective-guardrails "superchat-agent-loop" (&optional preset))
(defvar superchat-agent-max-tool-calls)

(defcustom superchat-subagent-unknown-tool-retries 2
  "How many invalid tool calls a sub-agent run may recover from.

llm.el reports unknown names and malformed arguments as fatal request
errors.  Up to this many times per run, the request is repaired (the
dangling tool-use record is dropped, a corrective message is appended)
and re-sent instead.  Set to 0 to keep the fatal behaviour."
  :type 'natnum
  :group 'superchat)

(defcustom superchat-subagent-timeout 300
  "Global upper bound for a sub-agent run, in seconds.
Set to nil to disable the global bound.  A preset `timeout' may set a
shorter limit, but cannot extend this one."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'superchat)

(defvar superchat--subagent-running nil
  "Alist of active Subagent actor ids and their runtime metadata.")

(declare-function superchat-agents-panel-refresh "superchat-agents-panel" ())

(defun superchat--subagent-refresh-panel ()
  "Refresh the optional agents panel when it is loaded."
  (when (fboundp 'superchat-agents-panel-refresh)
    (ignore-errors (superchat-agents-panel-refresh))))

(defun superchat--subagent-entry (id)
  "Return the active control-plane entry for ID."
  (cdr (assoc id superchat--subagent-running)))

(cl-defun superchat-subagent-handoff-decision
    (session-id run-id &optional (trace nil trace-supplied-p))
  "Return whether RUN-ID may hand its result out of SESSION-ID.
The result's `:action' is `deliver', `wait', or `reject'.  TRACE, when
supplied, is the caller's Tape-ordered snapshot; nil is an empty snapshot."
  (if (not (and (stringp run-id) (not (string-empty-p run-id))))
      (list :action 'reject :run-id run-id :reason 'run-not-found)
    (condition-case err
        (let* ((events (if trace-supplied-p
                           trace
                         (superchat-service-call nil 'tape 'replay session-id)))
               (terminal
                (superchat-service-call nil 'tape 'recorded-run-terminal
                                        session-id run-id events))
               (state (plist-get terminal :state))
               action reason)
          (cond
           ((eq state 'absent)
            (setq action 'wait reason 'run-open))
           ((eq state 'present)
            (if (superchat-tape-recorded-run-completed-p
                 (plist-get terminal :event))
                (setq action 'deliver reason 'proved)
              (setq action 'reject reason 'run-not-successful)))
           (t
            (setq action 'reject
                  reason (or (plist-get terminal :reason)
                             'unknown-terminal))))
          (list :action action
                :run-id run-id
                :reason reason
                :terminal-state terminal))
      (error
       (list :action 'reject
             :run-id run-id
             :reason (list 'tape-read-failed (error-message-string err))
             :terminal-state (list :state 'unknown
                                   :reason 'tape-read-failed))))))


(defun superchat--subagent-handoff-report (entry report status)
  "Return REPORT only when completed ENTRY has a durable handoff proof."
  (if (not (eq status 'completed))
      report
    (condition-case err
        (let* ((run-id (plist-get entry :run-id))
               (decision
                (superchat-subagent-handoff-decision
                 (plist-get entry :id) run-id))
               (action (plist-get decision :action)))
          (if (eq action 'deliver)
              report
            (format "[Error: sub-agent handoff %s for Run %s]"
                    action (or run-id "unknown"))))
      (error
       (format "[Error: sub-agent handoff could not be verified: %s]"
               (error-message-string err))))))

(defun superchat--subagent-finish (id report &optional status)
  "Finish active sub-agent ID once and conditionally deliver REPORT.
STATUS is the terminal Runtime status when completion came from Runtime."
  (when-let* ((entry (superchat--subagent-entry id)))
    (when-let* ((timer (plist-get entry :timer)))
      (cancel-timer timer))
    (let ((report
           (superchat--subagent-handoff-report entry report status)))
      (ignore-errors
        (superchat--team-finish-delegated-task entry report))
      ;; The task and its result go to the Work Record it was part of (K14).
      (when (and (plist-get entry :work-record)
                 (plist-get entry :parent-session-id)
                 (fboundp 'superchat-work-record-task))
        (ignore-errors
          (superchat-work-record-task
           (plist-get entry :parent-session-id) (plist-get entry :work-record)
           (plist-get entry :task) (if status (format "%s" status) "completed")
           report id)))
      (setq superchat--subagent-running
            (assoc-delete-all id superchat--subagent-running))
      (superchat--subagent-refresh-panel)
      (funcall (plist-get entry :callback) report))))

(defun superchat-subagent-cancel (id &optional reason)
  "Cancel running sub-agent ID and return non-nil when it existed.
REASON defaults to a user-cancellation message."
  (interactive
   (list (completing-read "Cancel sub-agent: "
                          (mapcar #'car superchat--subagent-running)
                          nil t)))
  (when-let* ((entry (superchat--subagent-entry id)))
    (let ((message
           (or reason
               (concat "[Cancelled by user — the user no longer wants this "
                       "sub-task performed. Acknowledge the cancellation in one "
                       "short sentence. Do NOT attempt the task yourself.]"))))
      (unless (and (plist-get entry :run-id)
                   (superchat-service-call nil 'driver 'cancel
                    (plist-get entry :run-id) message))
        (superchat--subagent-finish id message)))
    t))

(defun superchat-subagent-running ()
  "Return active sub-agent metadata, newest first."
  (mapcar #'cdr superchat--subagent-running))

(defun superchat-subagent-list-running ()
  "Display and return a summary of running sub-agents."
  (interactive)
  (let ((summary
         (if (null superchat--subagent-running)
             "No running sub-agents."
           (mapconcat
            (lambda (pair)
              (let* ((entry (cdr pair))
                     (placeholder (plist-get entry :placeholder))
                     (position (and (consp placeholder)
                                    (markerp (car placeholder))
                                    (marker-position (car placeholder)))))
                (format "%s  %s  depth=%d  %.1fs%s"
                        (car pair) (plist-get entry :preset)
                        (plist-get entry :depth)
                        (- (float-time) (plist-get entry :started-at))
                        (if position (format "  placeholder=%d" position) ""))))
            superchat--subagent-running "\n"))))
    (when (called-interactively-p 'interactive) (message "%s" summary))
    summary))

(defun superchat--subagent-set-placeholder (id placeholder)
  "Associate PLACEHOLDER markers with running sub-agent ID."
  (when-let* ((entry (superchat--subagent-entry id)))
    (setcdr (assoc id superchat--subagent-running)
            (plist-put entry :placeholder placeholder))))

(defun superchat--subagent-arm-timeout (id delay timeout)
  "Arm sub-agent ID's timeout after DELAY seconds.
TIMEOUT is retained for the user-facing error message when a paused
timer is resumed with less than its original delay."
  (when-let* ((entry (superchat--subagent-entry id)))
    (let ((timer
           (run-at-time
            delay nil
            (lambda ()
              (superchat-subagent-cancel
               id (format "[Timed out after %ss]" timeout))))))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :timer timer)))))

(defun superchat--subagent-pause-timeout (id)
  "Pause ID's timer and retain its remaining duration, if any."
  (when-let* ((entry (superchat--subagent-entry id))
              (timer (plist-get entry :timer)))
    (let ((remaining (max 0
                          (- (time-to-seconds (timer--time timer))
                             (float-time)))))
      (cancel-timer timer)
      (setq entry (plist-put entry :timer nil))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :paused-timeout remaining)))))

(defun superchat--subagent-resume-timeout (id)
  "Resume ID's timeout after an interactive pause, if it was paused."
  (when-let* ((entry (superchat--subagent-entry id))
              (remaining (plist-get entry :paused-timeout)))
    (let ((timeout (plist-get entry :timeout)))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :paused-timeout nil))
      (superchat--subagent-arm-timeout id remaining timeout))))

(defun superchat--subagent-round-limit-action (ctx)
  "Choose a round-limit action for CTX without charging user input time."
  (let ((id (plist-get ctx :session-id)))
    (if (and (eq superchat-llm-round-limit-action 'ask)
             (not noninteractive))
        (unwind-protect
            (progn
              (superchat--subagent-pause-timeout id)
              (superchat--llm-round-limit-action))
          (superchat--subagent-resume-timeout id))
      (superchat--llm-round-limit-action))))

(defconst superchat--subagent-delegate-tool-names
  '("delegate_to_subagent" "delegate_to_subagent_parallel")
  "Tool names that re-enter the sub-agent engine (depth-guarded).")

(defconst superchat--subagent-team-tool-names
  '("team_task" "team_message")
  "Coordination tools every teammate receives.")

(defun superchat--subagent-team-instructions (ctx)
  "Return the compact coordination protocol for teammate CTX."
  (format
   (concat "Team: %s\nYour actor id: %s\nYour task id: %s\n"
           "Use team_task to inspect, claim, or complete shared work. "
           "Use team_message to exchange relevant findings with another "
           "actor id, lead, or all. When Workspace tools are available, "
           "inspect workspace_info before reading or writing an Artifact, "
           "and pass its revision when writing. Check your mailbox before "
           "finishing.\n\n")
   (plist-get ctx :team-id)
   (plist-get ctx :actor-id)
   (or (plist-get ctx :task-id) "untracked")))

(defun superchat--subagent-make-context (preset depth)
  "Create an explicit execution context for PRESET at DEPTH.
The context replaces the dynamic bindings of the sync runner: async
callbacks close over it, so session identity, depth, and the tool
call counter survive across the event loop."
  (let* ((id (format "subagent-%s-%s-%04x"
                     (superchat-preset-name preset)
                     (format-time-string "%Y%m%d-%H%M%S")
                     (random 65536)))
         (parent-context (bound-and-true-p superchat--tool-execution-context))
         ;; A delegated agent's permission scope is fixed when it starts and
         ;; cannot be widened from inside: it runs under its parent's sandbox
         ;; mode and nobody is asked on its behalf.
         (_permissions
          (when (stringp (plist-get parent-context :session-id))
            ;; Pinned: both values are recorded even when they equal today's
            ;; defaults, so a later change of a default cannot move them.
            (superchat-sandbox-set-mode
             id (superchat-sandbox-effective-mode
                 (plist-get parent-context :session-id))
             t)
            (superchat-approval-set-policy id 'never t)))
         (parent-run-id (plist-get parent-context :run-id))
         (workspace
          (or (and parent-context
                   (plist-get parent-context :workspace-id)
                   (list :id (plist-get parent-context :workspace-id)
                         :root (plist-get parent-context :workspace-root)))
              (and (boundp 'superchat--workspace-binding)
                   superchat--workspace-binding)))
         (parent-guardrails (plist-get parent-context :guardrails))
         (guardrails
          (if (fboundp 'superchat--agent-effective-guardrails)
              (superchat--agent-effective-guardrails preset)
            (list :max-tool-calls
                  (if (boundp 'superchat-agent-max-tool-calls)
                      superchat-agent-max-tool-calls
                    50))))
         (parent-max (plist-get parent-guardrails :max-tool-calls)))
    (when (numberp parent-max)
      (setq guardrails
            (plist-put
             guardrails :max-tool-calls
             (min parent-max (plist-get guardrails :max-tool-calls)))))
    (list :session-id id
          :team-id (or superchat--team-id
                       (and (boundp 'superchat--session-id)
                            superchat--session-id)
                       id)
          :actor-id id
          :profile-id (superchat-preset-profile-id preset)
          :run-id nil
          ;; Stable identity holder: a tool wrapped from this context reads
          ;; Run/Turn identity from this vector (`superchat--subagent-tool-context'),
          ;; not from `ctx' itself.  Every wrapped tool closure captures
          ;; `ctx' by reference, but nothing guarantees `ctx' itself stays
          ;; the exact list object `superchat--subagent-publish-identity'
          ;; later mutates -- a vector is a genuinely stable, shared,
          ;; explicitly mutable cell that survives any future rebuild of
          ;; `ctx' the way relying on `plist-put' aliasing would not.
          :runtime-identity (vector nil nil)
          :parent-run-id parent-run-id
          :preset preset
          :workspace-id (plist-get workspace :id)
          :workspace-root (plist-get workspace :root)
          ;; The Work Record this task is part of (K14): the parent
          ;; conversation's current record when the task was delegated.
          :parent-session-id (plist-get parent-context :session-id)
          :work-record (and (fboundp 'superchat-work-current)
                            (stringp (plist-get parent-context :session-id))
                            (superchat-work-current
                             (plist-get parent-context :session-id)))
          :depth depth
          :guardrails guardrails
          ;; Mutable tool-call counter: the car of this cons is
          ;; incremented in place by the tool wrappers.
          :tool-calls (cons 0 nil))))

(defun superchat--subagent-effective-timeout (preset)
  "Return the effective timeout for PRESET."
  (let ((global (and (numberp superchat-subagent-timeout)
                     (> superchat-subagent-timeout 0)
                     superchat-subagent-timeout))
        (profile (superchat-preset-timeout preset)))
    (cond ((and global profile) (min global profile))
          (profile profile)
          (t global))))

(defun superchat--subagent-publish-identity (ctx run-id turn-id)
  "Publish RUN-ID/TURN-ID for sub-agent CTX after Runtime accepts its Run.
Writes through CTX's stable `:runtime-identity' holder (a 2-element vector,
`[RUN-ID TURN-ID]', built once in `superchat--subagent-make-context'), the
identity source `superchat--subagent-tool-context' reads for every tool
wrapped from CTX -- `aset' mutates that vector object in place, so every
wrapped closure sees the update regardless of whether CTX's own plist head
is later rebuilt.  Does nothing when CTX carries no holder (a context not
built by `superchat--subagent-make-context')."
  (when-let* ((identity (plist-get ctx :runtime-identity)))
    (aset identity 0 run-id)
    (aset identity 1 turn-id)))

(defun superchat--subagent-tool-context (ctx &optional block)
  "Return the shared executor context for sub-agent CTX."
  (let* ((runtime-context
          (bound-and-true-p superchat--tool-execution-context))
         (identity (plist-get ctx :runtime-identity)))
    (list :session-id (plist-get ctx :session-id)
          :turn-id (or (and identity (aref identity 1))
                       (plist-get ctx :turn-id)
                       (plist-get runtime-context :turn-id))
          :actor-id (plist-get ctx :actor-id)
          :profile-id (plist-get ctx :profile-id)
          :run-id (or (and identity (aref identity 0))
                      (plist-get ctx :run-id)
                      (plist-get runtime-context :run-id))
          :parent-run-id (plist-get ctx :parent-run-id)
          :team-id (plist-get ctx :team-id)
          :correlation-id (plist-get ctx :team-id)
          :workspace-id (plist-get ctx :workspace-id)
          :workspace-root (plist-get ctx :workspace-root)
          :guardrails (plist-get ctx :guardrails)
          :block (or block (plist-get ctx :tool-block)))))

(defun superchat--subagent-wrap-function (ctx tool-name original-fn async
                                              &optional static-block)
  "Wrap ORIGINAL-FN for sub-agent CTX through the shared executor.
STATIC-BLOCK, when non-nil, is a denial reason fixed at wrap time (such as
the sub-agent depth limit); it takes precedence over the per-call tool-call
limit computed from CTX's counter.  CTX is read live at invocation time, not
copied, so identity Runtime assigns to CTX after this tool was wrapped (but
before it is ever invoked) is visible here."
  (let* ((counter (plist-get ctx :tool-calls))
         (max-tool-calls
          (plist-get (plist-get ctx :guardrails) :max-tool-calls)))
    (if async
        (lambda (callback &rest args)
          (cl-incf (car counter))
          (let ((superchat--team-id (plist-get ctx :team-id))
                (superchat--team-actor (plist-get ctx :actor-id)))
            (superchat-tool-execute
             tool-name original-fn args
             :async t
             :callback callback
             :context
             (superchat--subagent-tool-context
              ctx
              (or static-block
                  (when (> (car counter) max-tool-calls)
                    (format "Sub-agent exceeded maximum tool calls (%d)"
                            max-tool-calls)))))))
      (lambda (&rest args)
        (cl-incf (car counter))
        (let ((superchat--team-id (plist-get ctx :team-id))
              (superchat--team-actor (plist-get ctx :actor-id)))
          (superchat-tool-execute
           tool-name original-fn args
           :context
           (superchat--subagent-tool-context
            ctx
            (or static-block
                (when (> (car counter) max-tool-calls)
                  (format "Sub-agent exceeded maximum tool calls (%d)"
                          max-tool-calls))))))))))

(defun superchat--subagent-wrap-tool (ctx tool)
  "Return TOOL wrapped for sub-agent CTX.
Delegate tools are special-cased: below `superchat-subagent-max-depth'
they re-enter the async engine at depth + 1; at or beyond the limit
they degrade to a sync stub returning a denial string."
  (let* ((name (llm-tool-name tool))
         (depth (plist-get ctx :depth))
         (delegate-p (member name superchat--subagent-delegate-tool-names))
         (blocked (and delegate-p
                       (>= depth superchat-subagent-max-depth)
                       (format "Delegation denied: sub-agent depth limit %d reached"
                               superchat-subagent-max-depth)))
         (async (and (not blocked)
                     (if delegate-p t (llm-tool-async tool))))
         (function
          (cond
           (blocked (lambda (&rest _args) blocked))
           ((not delegate-p) (llm-tool-function tool))
           ((string= name "delegate_to_subagent")
            (lambda (callback preset task &optional context)
              (let ((superchat--team-id (plist-get ctx :team-id)))
                (superchat--subagent-run-async
                 preset task context callback (1+ depth)))))
           (t
            (lambda (callback tasks)
              (let ((specs (superchat--subagent-parse-specs tasks))
                    (superchat--team-id (plist-get ctx :team-id)))
                (if (stringp specs)
                    (funcall callback specs)
                  (superchat--subagent-run-parallel-async
                   specs callback (1+ depth)))))))))
    (make-llm-tool
     :name name
     :description (llm-tool-description tool)
     :args (llm-tool-args tool)
     :async async
     :function
     (superchat--subagent-wrap-function
      ctx name function async blocked))))

(defun superchat--subagent-wrap-tools (ctx tools)
  "Wrap all TOOLS for sub-agent CTX."
  (mapcar (lambda (tool) (superchat--subagent-wrap-tool ctx tool)) tools))

(defun superchat--subagent-requested-tools (tools)
  "Return TOOLS plus the coordination tools every teammate receives."
  (if (eq tools 'none)
      superchat--subagent-team-tool-names
    (delete-dups (append tools superchat--subagent-team-tool-names))))

(defun superchat--subagent-prepare-tools (ctx prompt requested-tools)
  "Collect REQUESTED-TOOLS for PROMPT and wrap them for CTX."
  (superchat--subagent-wrap-tools
   ctx
   (superchat--collect-llm-tools
    (when (stringp prompt) prompt)
    requested-tools)))

(defun superchat--subagent-delivery-sink (delivery)
  "Return a callback Sink that reproduces the legacy DELIVERY semantics.
DELIVERY is called as (TEXT STATUS TAPE-ID): `completed' with the final
text, or the failure's own status with its text (contract §2.3 bridge)."
  (superchat-sink-callback
   :id (format "subagent-%08x" (random #x100000000))
   :stream #'ignore
   :final (lambda (_run result)
            (funcall delivery (or (plist-get result :text) "")
                     'completed (plist-get result :tape-id)))
   :error (lambda (_run failure)
            (funcall delivery (or (plist-get failure :text) "")
                     (plist-get failure :status) (plist-get failure :tape-id)))))

(declare-function superchat-workspace-rules-text "superchat-workspace" (root))
(declare-function superchat-workspace-rules-problem "superchat-workspace" (root))

(defun superchat--subagent-system-with-rules (system root)
  "Return SYSTEM followed by the rules of the Workspace rooted at ROOT.
A sub-agent works in its parent's Workspace and carries the same rules
\(contract R3).  It gets the rules only, not the note about remembering:
writing rules is the main agent's.  SYSTEM unchanged when ROOT is nil or
has no rules file."
  (let ((rules (and (stringp root)
                    (fboundp 'superchat-workspace-rules-text)
                    (superchat-workspace-rules-text root))))
    (when-let* (((null rules))
                ((stringp root))
                ((fboundp 'superchat-workspace-rules-problem))
                (problem (superchat-workspace-rules-problem root)))
      (message "superchat: workspace rules are not in this sub-agent's request: %s"
               problem))
    (cond
     ((null rules) system)
     ((and (stringp system) (not (string-empty-p system)))
      (concat system "\n\n" rules))
     (t rules))))

(declare-function superchat-work-current "superchat-work" (session-id))
(declare-function superchat-work-get "superchat-work" (id))
(declare-function superchat-work-render "superchat-work" (record))
(declare-function superchat-work-record-task "superchat-work"
                  (session-id id task status report conversation))

(defconst superchat-subagent-work-record-preamble
  "This task is part of a work record the main agent keeps. Below is a \
snapshot of it, for context; you cannot change it. Report what you did; the \
main agent records it."
  "What a sub-agent is told before the snapshot of its Work Record (K14).")

(defun superchat--subagent-system-with-record (system record-id)
  "Return SYSTEM followed by a snapshot of the Work Record RECORD-ID.
SYSTEM unchanged when RECORD-ID is nil or names no record."
  (let ((record (and record-id (fboundp 'superchat-work-get)
                     (superchat-work-get record-id))))
    (if (null record)
        system
      (concat (if (and (stringp system) (not (string-empty-p system)))
                  (concat system "\n\n")
                "")
              superchat-subagent-work-record-preamble "\n\n"
              (superchat-work-render record)))))

(defun superchat--subagent-runtime-envelope
    (ctx task prompt result prepared-tools buffer delivery)
  "Build the Request envelope for sub-agent CTX (contract §2.2, S13a).
TASK is the raw input `intent.received' records; PROMPT is the prepared
user message, carried as `:context :prepared-prompt' so the driver sends
it unchanged; PREPARED-TOOLS are the already-wrapped descriptors, riding
in `:runtime-extras' for the LLM layer while their names are the explicit
`:tools' decision; BUFFER is the acquired protocol host; DELIVERY is the
legacy (TEXT STATUS TAPE-ID) callback wrapped into the callback Sink."
  (let* ((session-id (plist-get ctx :session-id))
         (buffer
          (if (bufferp buffer)
              buffer
            (superchat-service-call nil 'editor-workspace 'buffer
             session-id 'subagent-protocol t)))
         (tool-names (delq nil (mapcar (lambda (tool)
                                         (and (fboundp 'llm-tool-name)
                                              (ignore-errors (llm-tool-name tool))))
                                       prepared-tools)))
         (system (superchat--subagent-system-with-record
                  (superchat--subagent-system-with-rules
                   (plist-get result :system-prompt)
                   (plist-get ctx :workspace-root))
                  (plist-get ctx :work-record))))
    (superchat-service-call nil 'editor-workspace 'validate-buffer buffer session-id)
    (append
     (list :surface 'subagent
           :content task
           :session-id session-id
           ;; Legacy presets without a capability are Runtime's to warn
           ;; about; deriving one from the name here would double the
           ;; warning and leak display names into the Capability model.
           :capability (superchat-preset-capability (plist-get ctx :preset))
           :topology 'subagent
           :origin '(:surface subagent)
           :target-model (plist-get result :target-model)
           :system-prompt system
           :tools (or tool-names 'none)
           :sink (superchat--subagent-delivery-sink delivery)
           :context (list :host-buffer buffer
                          :prepared-prompt prompt
                          :prepared-system system)
           :runtime-extras
           (list :clean-input task
                 :actor-id (plist-get ctx :actor-id)
                 :correlation-id (plist-get ctx :team-id)
                 :workspace-id (plist-get ctx :workspace-id)
                 :workspace-root (plist-get ctx :workspace-root)
                 :preset (plist-get ctx :preset)
                 :prepared-tools prepared-tools
                 :tool-wrapper (lambda (tool) (superchat--subagent-wrap-tool ctx tool))
                 :round-limit-function
                 (lambda () (superchat--subagent-round-limit-action ctx))
                 :unknown-tool-retries superchat-subagent-unknown-tool-retries
                 :persist-synthesized-tools nil
                 :record-conversation nil))
     (when-let* ((parent-run-id (plist-get ctx :parent-run-id)))
       (list :parent-run-id parent-run-id)))))

(defun superchat--subagent-run-async (preset-name task context callback &optional depth)
  "Run sub-agent PRESET-NAME on TASK asynchronously.
CONTEXT is optional background text from the caller.  CALLBACK
receives the final report string; failures are delivered as
\"[Error: ...]\" strings — this function never signals.  DEPTH is
the delegation depth of the new sub-agent (default 1: delegated
from the main session).

Isolation comes from the explicit context object rather than dynamic
bindings, and Emacs stays interactive while the sub-agent runs."
  (let ((preset (superchat--subagent-preset preset-name)))
    (if (null preset)
        (funcall callback (format "[Error: unknown sub-agent preset: %s]"
                                  preset-name))
      (let ((id nil)
            acquisition accepted delivery-entered delivery-ready
            pending-delivery-p pending-report pending-status)
        (condition-case err
          (let* ((ctx (superchat--subagent-make-context preset (or depth 1)))
                 (turn (superchat-turn-new task))
                 (tracked-task
                  (ignore-errors
                    (superchat--team-start-delegated-task ctx task))))
            (when tracked-task
              (setq ctx (plist-put ctx :task-id
                                   (plist-get tracked-task :id))))
            (setq id (plist-get ctx :session-id))
            (push (cons id (list :id id
                                 :team-id (plist-get ctx :team-id)
                                 :actor-id (plist-get ctx :actor-id)
                                 :profile-id (plist-get ctx :profile-id)
                                 :parent-run-id
                                 (plist-get ctx :parent-run-id)
                                 :task-id (plist-get ctx :task-id)
                                 :preset (superchat-preset-name preset)
                                 :depth (or depth 1)
                                 :started-at (float-time)
                                 :work-record (plist-get ctx :work-record)
                                 :parent-session-id
                                 (plist-get ctx :parent-session-id)
                                 :task task
                                 :callback callback))
                  superchat--subagent-running)
            (superchat--subagent-refresh-panel)
            (superchat-preset-apply preset turn)
            (setf (superchat-turn-prompt turn)
                  (concat (superchat--subagent-team-instructions ctx)
                          (when (and context (not (string-empty-p context)))
                            (format "Context from main session:\n%s\n\n" context))
                          task))
            ;; Everything up to the Runtime submit runs synchronously, so
            ;; dynamic bindings are still safe for prompt building; the
            ;; async callbacks rely only on `ctx'.
            (let* ((result (let ((superchat--active-preset preset)
                                 (superchat--conversation-history nil)
                                 (superchat--session-id (plist-get ctx :session-id)))
                             (superchat--execute-llm-query turn)))
                   (prompt (plist-get result :prompt))
                   (requested-tools
                    (superchat--subagent-requested-tools
                     (plist-get result :tools)))
                   (tools (superchat--subagent-prepare-tools
                           ctx prompt requested-tools))
                   (_acquired
                    (setq acquisition
                          (superchat-service-call nil 'editor-workspace 'acquire-buffer
                           id 'subagent-protocol)))
                   (buffer (plist-get acquisition :buffer)))
                (let* ((handle
                        (superchat-service-call nil 'driver 'submit
                         (superchat--subagent-runtime-envelope
                          ctx task prompt result tools buffer
                          (lambda (report status _tape-id)
                            ;; Runtime publishes before delivery.  Publish the
                            ;; producer guard before any callback-facing work.
                            (setq accepted t
                                  delivery-entered t)
                            (if delivery-ready
                                (superchat--subagent-finish id report status)
                              (setq pending-delivery-p t
                                    pending-report report
                                    pending-status status))))))
                       (run-id (plist-get handle :run-id))
                       (turn-id (plist-get handle :turn-id))
                       (entry (superchat--subagent-entry id))
                       (timeout
                        (superchat--subagent-effective-timeout preset)))
                  (unless (and (stringp run-id)
                               (not (string-empty-p run-id)))
                    (error "Runtime accepted Subagent without run-id"))
                  (unless (and (stringp turn-id)
                               (not (string-empty-p turn-id)))
                    (error "Runtime accepted Subagent without turn-id"))
                  (setq accepted t)
                  ;; Both lines below matter: the stable holder is what
                  ;; every already-wrapped tool closure actually reads
                  ;; (`superchat--subagent-tool-context'); the plain
                  ;; `plist-put's keep CTX's own :run-id/:turn-id in sync
                  ;; for direct readers of CTX itself, such as
                  ;; `superchat--team-member-event' just below.
                  (superchat--subagent-publish-identity ctx run-id turn-id)
                  (setq ctx (plist-put ctx :run-id run-id))
                  (setq ctx (plist-put ctx :turn-id turn-id))
                  (when entry
                    (setq entry (plist-put entry :run-id run-id))
                    (setq entry (plist-put entry :buffer-name
                                           (buffer-name buffer)))
                    (setq entry (plist-put entry :timeout timeout))
                    (setcdr (assoc id superchat--subagent-running) entry)
                    (when timeout
                      (superchat--subagent-arm-timeout id timeout timeout)))
                  (ignore-errors
                    (superchat--team-member-event ctx "started"))
                  (setq delivery-ready t)
                  (when pending-delivery-p
                    (superchat--subagent-finish
                     id pending-report pending-status))
                  id)))
          ((error quit)
           (when (and acquisition (not accepted))
             ;; Rollback failure is secondary to the submission rejection.
             (condition-case nil
                 (superchat-service-call nil 'editor-workspace 'rollback-acquisition
                  id 'subagent-protocol
                  (plist-get acquisition :buffer)
                  (plist-get acquisition :newly-acquired))
               ((error quit) nil)))
           (if (and id delivery-entered pending-delivery-p)
               (progn
                 (setq delivery-ready t)
                 (superchat--subagent-finish
                  id pending-report pending-status))
             (if id
                 (superchat--subagent-finish
                  id (format "[Error: %s]" (error-message-string err)))
             (funcall callback (format "[Error: %s]"
                                       (error-message-string err)))))))))))

(defun superchat--subagent-parse-specs (tasks)
  "Parse TASKS (a JSON array string) into a list of spec plists.
Each element becomes (:preset ... :task ... :context ...).
Returns an error string when the JSON is invalid."
  (condition-case err
      (let ((items (if (fboundp 'json-parse-string)
                       (json-parse-string tasks :object-type 'plist
                                          :array-type 'list)
                     (json-read-from-string tasks))))
        (mapcar (lambda (item)
                  (list :preset (plist-get item :preset)
                        :task (plist-get item :task)
                        :context (or (plist-get item :context) "")))
                items))
    (error (format "[Error: invalid JSON tasks: %s]"
                   (error-message-string err)))))

(defun superchat--subagent-run-parallel-async (specs callback &optional depth placeholder)
  "Run sub-agent SPECS concurrently; CALLBACK gets the aggregated report.
Each spec is a plist (:preset :task :context).  At most
`superchat-subagent-parallel-max' sub-agents are in flight at once;
the rest queue and launch as slots free up.  Results aggregate in
spec order regardless of completion order.  DEPTH is passed through
to `superchat--subagent-run-async'.

Replaces the v1.2 `make-thread' implementation: llm.el requests run
on separate curl processes, so delegation is genuinely concurrent
and the UI never blocks on `thread-join'."
  (let* ((n (length specs))
         (results (make-vector n nil))
         (pending n)
         (next 0)
         (active 0)
         (window (max 1 superchat-subagent-parallel-max)))
    (if (zerop n)
        (funcall callback "[Error: no sub-agent specs given]")
      (cl-labels
          ((finish (idx spec report)
             (aset results idx (list :preset (plist-get spec :preset)
                                     :task (plist-get spec :task)
                                     :report report))
             (cl-decf pending)
             (cl-decf active)
             (if (zerop pending)
                 (funcall callback
                          (superchat--subagent-aggregate-reports
                           (append results nil)))
               (launch)))
           (launch ()
             (while (and (< next n) (< active window))
               (let ((idx next)
                     (spec (nth next specs)))
                 (cl-incf next)
                 (cl-incf active)
                 (let ((id (superchat--subagent-run-async
                            (plist-get spec :preset)
                            (plist-get spec :task)
                            (plist-get spec :context)
                            (lambda (report) (finish idx spec report))
                            depth)))
                   (superchat--subagent-set-placeholder id placeholder))))))
        (launch)))))

(defun superchat--subagent-aggregate-reports (results)
  "Format a list of sub-agent RESULTS into a single report string.
Each result is a plist with :preset, :report, and optionally :error."
  (mapconcat
   (lambda (r)
     (format "** Sub-agent report: %s\n#+begin_quote\n%s\n#+end_quote"
             (plist-get r :preset)
             (or (plist-get r :report)
                 (format "[ERROR: %s]" (plist-get r :error)))))
   results
   "\n\n"))

;; ═══════════════════════════════════════════════════════════
;; Main-buffer rendering
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--insert-system-message "superchat-render" (content))

(defun superchat--subagent-format-report (preset-name report)
  "Format a sub-agent REPORT block attributed to PRESET-NAME."
  (concat (propertize (format "** Sub-agent report: %s" preset-name)
                      'face 'font-lock-keyword-face)
          "\n#+begin_quote\n"
          (string-trim (or report ""))
          "\n#+end_quote\n"))

(defun superchat--subagent-render-report (preset-name report)
  "Render a sub-agent REPORT in the main superchat buffer.
PRESET-NAME is the name of the sub-agent that produced the report."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (superchat--subagent-format-report preset-name report))))))

;; ── RLM-style batch primitive ──

(defun superchat--subagent-map-pump ()
  "Wait briefly for sub-agent process output.
A seam around the C primitive so tests can stub the pump."
  (accept-process-output nil 0.05))

(defun superchat-subagent-map (chunks query &optional preset timeout)
  "Run QUERY against each of CHUNKS in parallel sub-agents; return reports.

The Elisp analogue of an RLM `sub_rlm_batched': CHUNKS is a list of
strings (each becomes one sub-agent's :context), QUERY is the shared
task, PRESET names the sub-agent preset (default `researcher'), and the
return value is a list of report strings in CHUNK order.  Designed to be
called from synthesized-tool bodies, where a for-loop over sub-agents
must be a function call, not a conversation turn — the delegating
model's context receives only what the tool body chooses to return.

Concurrency is bounded by `superchat-subagent-parallel-max'.  This
function BLOCKS the calling context (pumping process output) until all
chunks complete or TIMEOUT seconds (default 600) elapse; unfinished
sub-agents are cancelled and their slots read \"[Error: ...]\"."
  (let* ((preset (or preset 'researcher))
         (timeout (or timeout 600))
         (chunks (if (listp chunks) chunks (append chunks nil)))
         (n (length chunks))
         (results (make-vector n nil))
         (ids (make-vector n nil))
         (pending n)
         (next 0)
         (active 0)
         (aborted nil)
         (window (max 1 superchat-subagent-parallel-max))
         (transport-timer-owner
          (superchat-service-call nil 'tools 'transport-timer-owner))
         (deadline (+ (float-time) timeout)))
    (if (zerop n)
        nil
      (cl-labels
          ((launch ()
             ;; Once aborted (timeout cleanup), a cancellation that
             ;; synchronously fires a chunk's callback must NOT pull the
             ;; next queued chunk in just to cancel it too.
             (unless aborted
               (while (and (< next n) (< active window))
                 (let ((idx next)
                       (chunk (nth next chunks)))
                   (cl-incf next)
                   (cl-incf active)
                   (aset ids idx
                         (superchat--subagent-run-async
                          preset query chunk
                          (lambda (report)
                            (aset results idx report)
                            (cl-decf pending)
                            (cl-decf active)
                            (launch)))))))))
        (launch)
        (while (and (> pending 0) (< (float-time) deadline))
          (superchat--subagent-map-pump)
          ;; Keep the parent tool-calling request alive: without this a
          ;; long batch would let the main response finalize as timed out
          ;; while the map is still running.
          (when (fboundp 'superchat--extend-timeout)
            (ignore-errors
              (superchat--extend-timeout transport-timer-owner))))
        (when (> pending 0)
          (setq aborted t)
          (dotimes (idx n)
            (unless (aref results idx)
              (when (aref ids idx)
                (ignore-errors (superchat-subagent-cancel
                                (aref ids idx)
                                "[Error: superchat-subagent-map timed out]")))
              (unless (aref results idx)
                (aset results idx "[Error: superchat-subagent-map timed out]")))))
        (append results nil)))))

;; ── Progress placeholders (async delegation) ──

(defun superchat--subagent-begin-placeholder (label)
  "Insert a running placeholder for LABEL in the main chat buffer.
Returns a (BEG-MARKER . END-MARKER) cons for
`superchat--subagent-end-placeholder', or nil when the chat buffer
does not exist.  Concurrent placeholders coexist: markers track
buffer edits, so replacing one does not disturb the others."
  (let ((buf (get-buffer (bound-and-true-p superchat-buffer-name))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((beg (point-marker)))
              (insert (propertize (format "⏳ Sub-agent %s: running…" label)
                                  'face 'font-lock-comment-face)
                      "\n")
              (cons beg (point-marker)))))))))

(defun superchat--subagent-end-placeholder (placeholder label report)
  "Replace PLACEHOLDER with the final REPORT block for LABEL.
Falls back to appending at the end of the buffer when PLACEHOLDER
is nil or its buffer was killed."
  (if (and (consp placeholder)
           (markerp (car placeholder))
           (marker-buffer (car placeholder)))
      (with-current-buffer (marker-buffer (car placeholder))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (car placeholder))
            (delete-region (car placeholder) (cdr placeholder))
            (insert (superchat--subagent-format-report label report))))
        (set-marker (car placeholder) nil)
        (set-marker (cdr placeholder) nil))
    (superchat--subagent-render-report label report)))

;;;###autoload
(defun superchat--cmd-subagent (_cmd args _input _lang _target-model)
  "Delegate a task to a sub-agent.
ARGS format: `<preset> <task>'.  Available presets: researcher,
executor, introspector.  The sub-agent runs in isolation and its
report is rendered in the chat buffer."
  (superchat-plugin-require-active-row 'subagent)
  (let* ((trimmed (string-trim (or args "")))
         (preset (when (string-match "^\\([a-zA-Z0-9_-]+\\)" trimmed)
                   (match-string 1 trimmed)))
         (task (when preset
                 (string-trim (substring trimmed (match-end 0))))))
    (if (or (null preset) (string-empty-p task))
        '(:type :echo :content "Usage: /subagent <preset> <task>")
      ;; Async delegation: the placeholder renders immediately and is
      ;; replaced by the report when the sub-agent completes.  Emacs
      ;; stays interactive in the meantime.
      (let ((ph (superchat--subagent-begin-placeholder preset)))
        (let ((id (superchat--subagent-run-async
                   preset task ""
                   (lambda (report)
                     (superchat--subagent-end-placeholder ph preset report)))))
          (superchat--subagent-set-placeholder id ph))
        `(:type :echo :content ,(format "Sub-agent %s delegated — report renders on completion." preset))))))

;;;###autoload
(defun superchat--cmd-cancel-agent (_cmd args _input _lang _target-model)
  "Cancel the sub-agent named by ARGS."
  (let ((id (string-trim (or args ""))))
    (cond ((string-empty-p id)
           '(:type :echo :content "Usage: /cancel-agent <id>"))
          ((superchat-subagent-cancel id)
           `(:type :echo :content ,(format "Cancelled sub-agent %s." id)))
          (t `(:type :echo :content ,(format "No running sub-agent: %s" id))))))

(provide 'superchat-subagent)

;;; superchat-subagent.el ends here
