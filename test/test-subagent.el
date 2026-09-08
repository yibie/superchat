;;; test-subagent.el --- Tests for superchat-subagent -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for sub-agent presets, runner, and delegation tool.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'superchat-agent-loop)
(require 'superchat-dispatcher)
(require 'superchat-llm)
(require 'superchat-subagent)
(require 'superchat)
(require 'journey-fixture
         (expand-file-name "journey-fixture" (file-name-directory load-file-name)))

(defvar superchat-llm-backend nil)
(defvar superchat-llm-max-tool-rounds nil)
(defvar superchat-llm-round-limit-action nil)
(defvar superchat-buffer-name nil)
(defvar superchat-data-directory nil)
(defvar superchat-db--connection nil)
(defvar superchat-db--path nil)

(defmacro test-subagent--with-team-db (&rest body)
  "Run BODY against an isolated Tape database."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file "superchat-team-test-" t))
          (database (expand-file-name "superchat.db" directory))
          (superchat-data-directory directory)
          (superchat-db--connection nil)
          (superchat-db--path nil))
     (unwind-protect
         (progn
           (superchat-db-open database)
           ,@body)
       (superchat-db-close)
       (delete-directory directory t))))

(defun test-subagent--real-no-parent-handoff (failed)
  "Real child transport completion crosses Team/Tape/Closure without a parent."
  (test-subagent--with-team-db
      (let ((superchat-llm-backend (make-llm-ollama :chat-model "fixture"))
            (superchat--session-id nil)
            (superchat--team-id nil)
            (superchat--tool-execution-context nil)
            (superchat--subagent-running nil)
            (superchat-runtime--runs (make-hash-table :test #'equal))
            (superchat-editor-workspace--states (make-hash-table :test #'equal))
            (superchat-closure-terminal-guard-enabled t)
            (superchat-subagent-timeout nil)
            (superchat-response-timeout nil)
            response failure id entry host reports)
        (cl-letf (((symbol-function 'llm-chat-async)
                   (lambda (_provider _prompt on-response on-error &optional _multi)
                     (setq response on-response failure on-error) nil))
                  ((symbol-function 'llm-chat-streaming)
                   (lambda (_provider _prompt _partial on-response on-error
                            &optional _multi)
                     (setq response on-response failure on-error) nil)))
          (unwind-protect
              (progn
                (setq id (superchat--subagent-run-async
                          'executor "fixture child" nil
                          (lambda (report) (push report reports)))
                      entry (copy-tree (superchat--subagent-entry id)))
                (should entry)
                (setq host (get-buffer (plist-get entry :buffer-name)))
                (should (functionp response))
                (should (buffer-live-p host))
                (should (equal id (plist-get entry :team-id)))
                (if failed (funcall failure 'error "fixture transport failure")
                  (funcall response "fixture child answer"))
                (let* ((run (plist-get entry :run-id))
                       (status (superchat-runtime-status run))
                       (decision (superchat-subagent-handoff-decision id run)))
                  (message "F04 child failed=%S status=%S decision=%S report=%S"
                           failed (plist-get status :status)
                           (plist-get decision :action) reports)
                  (should (eq (if failed 'failed 'completed)
                              (plist-get status :status)))
                  (should (eq (if failed 'reject 'deliver)
                              (plist-get decision :action))))
                (should (= 1 (length reports)))
                (let ((types (mapcar (lambda (event) (plist-get event :type))
                                     (superchat-tape-replay-events id))))
                  (dolist (type '("team.task.created" "team.task.updated" "subagent.started"))
                    (should (member type types))))
                (unless failed (should (equal '("fixture child answer") reports)))
                (funcall response "duplicate late child")
                (should (= 1 (length reports)))
                (should (buffer-live-p host))
                (should (memq host (superchat-editor-workspace-owned-buffers id)))
                (superchat-session-workspace-close id)
                (should-not (buffer-live-p host)))
            (when-let* ((run (plist-get entry :run-id)))
              (superchat-runtime-cancel run "fixture cleanup"))
            (when id (superchat-session-workspace-close id)))))))

(defun subagent-test--deliver (request text status &optional tape-id)
  "Deliver TEXT with STATUS through REQUEST's callback Sink (S13a).
Reproduces the legacy (TEXT STATUS TAPE-ID) delivery the producer used to
receive directly: `completed' goes to the Sink's `:final', anything else
to its `:error'."
  (let ((sink (plist-get request :sink)))
    (should (superchat-sink-p sink))
    (if (eq status 'completed)
        (superchat-sink-emit sink 'final nil (list :text text :tape-id tape-id))
      (superchat-sink-emit sink 'error nil
                           (list :text text :status status :tape-id tape-id)))))

(ert-deftest subagent/real-no-parent-observation-success ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)))
  (test-subagent--real-no-parent-handoff nil))

(ert-deftest subagent/real-no-parent-observation-failure ()
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)))
  (test-subagent--real-no-parent-handoff t))

(ert-deftest subagent/real-native-parent-delegate-context ()
  "Native async delegation retains parent identity outside its current buffer."
  (skip-unless (and (require 'llm nil t) (require 'llm-ollama nil t)
                    (require 'llm-provider-utils nil t)))
  (dolist (explicit '(nil "explicit-team"))
    (dolist (failed '(nil t))
      (test-subagent--with-team-db
        (let* ((superchat-llm-backend (make-llm-ollama :chat-model "fixture"))
               (superchat-runtime--runs (make-hash-table :test #'equal))
               (superchat-editor-workspace--states (make-hash-table :test #'equal))
               (superchat--subagent-running nil)
               (superchat--team-id nil)
               (superchat-response-timeout nil)
               (superchat-subagent-timeout nil)
               (superchat-closure-terminal-guard-enabled t)
               (superchat-agent-confirm-destructive nil)
               (superchat-buffer-name (generate-new-buffer-name " *f04-parent*"))
               (view (superchat--ensure-chat-view superchat-buffer-name "f04-parent"))
               (foreign (generate-new-buffer " *f04-foreign*"))
               (transport-calls 0) (tool-reports 0)
               first-provider first-prompt first-response first-error first-multi
               child-response child-error followup parent entry child host delivered)
          (cl-labels
              ((transport (provider prompt response error-cb multi)
                 (cl-incf transport-calls)
                 (pcase transport-calls
                   (1 (setq first-provider provider first-prompt prompt
                            first-response response first-error error-cb
                            first-multi multi))
                   (2 (setq child-response response child-error error-cb))
                   (3 (setq followup response))
                   (_ (ert-fail "Unexpected extra model request")))
                 nil))
            (cl-letf (((symbol-function 'llm-chat-async)
                       (lambda (provider prompt response error-cb &optional multi)
                         (transport provider prompt response error-cb multi)))
                      ((symbol-function 'llm-chat-streaming)
                       (lambda (provider prompt _part response error-cb &optional multi)
                         (transport provider prompt response error-cb multi))))
              (unwind-protect
                  (progn
                    (with-current-buffer foreign
                      (setq-local superchat--session-id "f04-foreign"))
                    (setq parent
                          (superchat-runtime-submit
                           (list :session-id "f04-parent" :buffer view
                                 :input "delegate fixture" :prompt "delegate fixture"
                                 :tools '("delegate_to_subagent") :deadline nil
                                 :delivery-function
                                 (lambda (text status _id)
                                   (push (list text status) delivered)))))
                    (should (functionp first-response))
                    (let ((tool (cl-find "delegate_to_subagent"
                                         (llm-chat-prompt-tools first-prompt)
                                         :key #'llm-tool-name :test #'equal)))
                      (should tool)
                      (should (llm-tool-async tool)))
                    ;; Model transport supplies tool uses; llm's real async
                    ;; scheduler invokes the native Authority wrapper.
                    (with-current-buffer foreign
                      (let ((superchat--team-id explicit))
                        (llm-provider-utils-execute-tool-uses
                         first-provider first-prompt
                         (list (make-llm-provider-utils-tool-use
                                :id "f04-delegate" :name "delegate_to_subagent"
                                :args '((preset . "executor") (task . "child fixture"))))
                         first-multi nil
                         (lambda (result)
                           (cl-incf tool-reports)
                           (funcall first-response result))
                         first-error)))
                    (setq entry (copy-tree (cdar superchat--subagent-running))
                          child (plist-get entry :id))
                    (should entry)
                    (setq host (get-buffer (plist-get entry :buffer-name)))
                    (should (functionp child-response))
                    (message "F04 parent=%S child=%S team=%S expected=%S"
                             (plist-get parent :run-id) child (plist-get entry :team-id)
                             (or explicit "f04-parent"))
                    (should (equal (plist-get parent :run-id)
                                   (plist-get entry :parent-run-id)))
                    (should-not (equal child "f04-parent"))
                    (should (equal (or explicit "f04-parent")
                                   (plist-get entry :team-id)))
                    (should-not (superchat-tape-replay-events "f04-foreign"))
                    (with-current-buffer foreign
                      (if failed (funcall child-error 'error "child failed")
                        (funcall child-response "child report")))
                    (should (= 1 tool-reports))
                    (should (functionp followup))
                    (should (eq (if failed 'failed 'completed)
                                (plist-get (superchat-runtime-status
                                            (plist-get entry :run-id)) :status)))
                    (should (eq (if failed 'reject 'deliver)
                                (plist-get (superchat-subagent-handoff-decision
                                            child (plist-get entry :run-id)) :action)))
                    (let* ((team-trace (superchat-tape-replay-events
                                        (or explicit "f04-parent")))
                           (member-events
                            (seq-filter
                             (lambda (event)
                               (member (plist-get event :type)
                                       '("subagent.started" "subagent.completed" "subagent.failed")))
                             team-trace)))
                      (should (= 2 (length member-events)))
                      (dolist (event member-events)
                        (should (equal (plist-get parent :run-id) (plist-get event :parent-run-id)))
                        (should (equal (plist-get entry :run-id) (plist-get event :run-id))))
                      (should (seq-some (lambda (event) (equal "team.task.created" (plist-get event :type)))
                                        team-trace)))
                    (funcall child-response "late duplicate")
                    (should (= 1 tool-reports))
                    (funcall followup "parent final")
                    (should (equal '(("parent final" completed)) delivered))
                    (let* ((trace (superchat-tape-replay-events "f04-parent"))
                           (run (plist-get parent :run-id))
                           (types (mapcar (lambda (event) (plist-get event :type))
                                          (seq-filter
                                           (lambda (event) (equal run (plist-get event :run-id)))
                                           trace))))
                      (dolist (type '("tool.requested" "tool.execution.started"
                                      "tool.completed" "response.completed" "run.completed"))
                        (should (= 1 (cl-count type types :test #'equal))))
                      (should (eq 'proved (plist-get (superchat-closure-judge
                                                      (list :kind 'durable-run-terminal :run-id run)
                                                      trace) :verdict))))
                    (should (buffer-live-p host))
                    (should (memq host (superchat-editor-workspace-owned-buffers child)))
                    (superchat-session-workspace-close child)
                    (should-not (buffer-live-p host))
                    (should (buffer-live-p foreign)))
                (dolist (run (list (plist-get entry :run-id) (plist-get parent :run-id)))
                  (when run (superchat-runtime-cancel run "fixture cleanup")))
                (when child (superchat-session-workspace-close child))
                (superchat-session-workspace-close "f04-parent")
                (when (buffer-live-p foreign) (kill-buffer foreign))))))))))

(ert-deftest test-team-task-dependencies-and-lifecycle ()
  "Tasks are projected from Tape and blocked dependencies cannot be claimed."
  (test-subagent--with-team-db
    (let ((superchat--team-id "team-1")
          (superchat--team-actor "lead"))
      (let* ((first (superchat-team-task "create" nil "Investigate"))
             (first-id (plist-get first :id))
             (second
              (superchat-team-task
               "create" nil "Verify" nil
               (format "[\"%s\"]" first-id)))
             (second-id (plist-get second :id)))
        (let ((superchat--team-actor "verifier"))
          (should-error (superchat-team-task "claim" second-id)))
        (let ((superchat--team-actor "researcher"))
          (superchat-team-task "claim" first-id)
          (superchat-team-task "complete" first-id))
        (let ((superchat--team-actor "verifier"))
          (superchat-team-task "claim" second-id)
          (superchat-team-task "complete" second-id))
        (should
         (equal '("completed" "completed")
                (mapcar (lambda (task) (plist-get task :status))
                        (superchat-team-tasks "team-1"))))))))

(ert-deftest test-team-mailbox-is-recipient-scoped ()
  "Direct messages reach one actor while all reaches every actor."
  (test-subagent--with-team-db
    (let ((superchat--team-id "team-1")
          (superchat--team-actor "researcher"))
      (superchat-team-message "send" "verifier" "check this")
      (superchat-team-message "send" "all" "shared finding")
      (let ((superchat--team-actor "verifier"))
        (let ((messages (superchat-team-message "list")))
          (should (= 2 (length messages)))
          (dolist (message messages)
            (should (string-prefix-p
                     "evt_" (plist-get message :event-id)))
            (should (integerp (plist-get message :storage-id))))
          (should
           (= 1
              (length
               (superchat-team-message
                "list" nil nil
                (plist-get (car messages) :storage-id)))))))
      (let ((superchat--team-actor "executor"))
        (let ((messages (superchat-team-message "list")))
          (should (= 1 (length messages)))
          (should (equal "shared finding"
                         (plist-get (car messages) :content))))))))

(ert-deftest test-team-decisions-are-replayable ()
  "Published decisions are part of the Team projection."
  (test-subagent--with-team-db
    (let ((superchat--team-id "team-1")
          (superchat--team-actor "lead"))
      (should (string-match-p
               "Decision D[0-9]+ published"
               (superchat-team-message
                "publish" nil "Keep Session and Team projections on demand.")))
      (let ((decisions (superchat-team-message "decisions")))
        (should (= 1 (length decisions)))
        (should (equal "lead" (plist-get (car decisions) :from)))
        (should
         (equal "Keep Session and Team projections on demand."
                (plist-get (car decisions) :content)))))))

(ert-deftest test-team-projection-reads-legacy-task-events ()
  "Existing team/task rows remain readable after envelope migration."
  (test-subagent--with-team-db
    (superchat-db-tape-append
     "legacy-team" "system" "Legacy task"
     :topic "team/task"
     :meta '((event . "created")
             (title . "Legacy task")
             (status . "pending")
             (actor . "lead")))
    (let ((tasks (superchat-team-tasks "legacy-team")))
      (should (= 1 (length tasks)))
      (should (equal "Legacy task" (plist-get (car tasks) :title))))))

(ert-deftest test-team-projection-real-db-keeps-session-legacy-and-tolerance ()
  "Team replay scopes one Session and reads past malformed legacy metadata."
  (test-subagent--with-team-db
    (superchat-tape-append-event
     '(:type "team.task.created" :session-id "team-a" :topic "team/task"
       :payload ((task_id . "current-1") (title . "Current one")
                 (status . "pending"))))
    (superchat-db-tape-append
     "team-a" "system" "Legacy task" :topic "team/task"
     :meta '((event . "created") (task_id . "legacy-2")
             (title . "Legacy two") (status . "pending")))
    (superchat-tape-append-event
     '(:type "team.task.created" :session-id "team-b" :topic "team/task"
       :payload ((task_id . "foreign") (title . "Foreign")
                 (status . "pending"))))
    (superchat-tape-import-event
     '(:type "runtime.changed" :session-id "team-a" :topic "other"
       :payload ((task_id . "wrong-topic") (title . "Wrong topic"))))
    (sqlite-execute
     (superchat-db-open)
     "INSERT INTO tape (session_id, topic, kind, content, meta)
      VALUES ('team-a', 'team/task', 'system', 'bad', '{')")
    (superchat-tape-append-event
     '(:type "team.task.created" :session-id "team-a" :topic "team/task"
       :payload ((task_id . "current-3") (title . "Current three")
                 (status . "pending"))))
    (let ((tasks (superchat-team-tasks "team-a")))
      (should (equal '("current-1" "legacy-2" "current-3")
                     (mapcar (lambda (task) (plist-get task :id)) tasks)))
      (should (equal '("Current one" "Legacy two" "Current three")
                     (mapcar (lambda (task) (plist-get task :title)) tasks)))
      (should-not (cl-find "foreign" tasks
                           :key (lambda (task) (plist-get task :id))
                           :test #'equal)))))

(ert-deftest test-team-member-projection-separates-actor-profile-and-run ()
  "Team member state projects current identities and legacy `agent_id'."
  (test-subagent--with-team-db
    (superchat-tape-append-event
     '(:type "subagent.completed"
       :session-id "team-1"
       :team-id "team-1"
       :actor-id "coder-1"
       :run-id "run-1"
       :parent-run-id "run-parent"
       :topic "team/member"
       :payload ((event . "completed")
                 (actor_id . "coder-1")
                 (profile_id . "coder-safe")
                 (run_id . "run-1")
                 (parent_run_id . "run-parent"))))
    (superchat-db-tape-append
     "team-1" "system" "legacy finished"
     :topic "team/member"
     :meta '((event . "completed")
             (agent_id . "legacy-1")
             (preset . "researcher")))
    (let* ((members (plist-get (superchat--team-state "team-1") :members))
           (current
            (cl-find "coder-1" members
                     :key (lambda (member) (plist-get member :actor-id))
                     :test #'equal))
           (legacy
            (cl-find "legacy-1" members
                     :key (lambda (member) (plist-get member :actor-id))
                     :test #'equal)))
      (should (equal "coder-safe" (plist-get current :profile-id)))
      (should (equal "run-1" (plist-get current :run-id)))
      (should (equal "run-parent"
                     (plist-get current :parent-run-id)))
      (should (equal "completed" (plist-get current :status)))
      (should (equal "researcher" (plist-get legacy :profile-id))))))

(ert-deftest test-team-member-events-use-the-actor-contract ()
  "New Subagent events carry Actor, Profile, Run, and parent Run identities."
  (let (event)
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (value) (setq event value) 1)))
      (superchat--team-member-event
       '(:team-id "team-1"
         :actor-id "coder-1"
         :profile-id "profile.coder"
         :run-id "run-1"
         :parent-run-id "run-parent"
         :task-id "task-1")
       "started"))
    (should (equal "coder-1" (plist-get event :actor-id)))
    (should (equal "profile.coder" (plist-get event :profile-id)))
    (should (equal "run-1" (plist-get event :run-id)))
    (should (equal "run-parent" (plist-get event :parent-run-id)))
    (should-not (assq 'agent_id (plist-get event :payload)))))

(ert-deftest test-team-delegated-task-finishes-on-success ()
  "A delegated task is assigned to its actor and completed with its run."
  (test-subagent--with-team-db
    (let* ((superchat--session-id "team-1")
           (preset (superchat--subagent-preset 'researcher))
           (ctx (superchat--subagent-make-context preset 1))
           (task (superchat--team-start-delegated-task ctx "Investigate"))
           (ctx (plist-put ctx :task-id (plist-get task :id)))
           (entry (list :team-id (plist-get ctx :team-id)
                        :actor-id (plist-get ctx :actor-id)
                        :profile-id (plist-get ctx :profile-id)
                        :run-id "run-research"
                        :task-id (plist-get ctx :task-id))))
      (should (equal "in_progress" (plist-get task :status)))
      (superchat--team-finish-delegated-task entry "Done")
      (should
       (equal "completed"
              (plist-get
               (superchat--team-task-find
                (plist-get task :id) "team-1")
               :status))))))

(ert-deftest test-subagent-preset-researcher ()
  "The researcher preset should be read-only."
  (let ((preset (superchat--subagent-preset 'researcher)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (equal "research" (superchat-preset-capability preset)))
    (should (member "list_buffers" (superchat-preset-tools preset)))
    (should (member "read_buffer" (superchat-preset-tools preset)))
    (should (member "eval_elisp" (superchat-preset-tools preset)))
    (should (member "workspace_read" (superchat-preset-tools preset)))
    (should (member "workspace_info" (superchat-preset-tools preset)))
    (should (not (member "workspace_write"
                         (superchat-preset-tools preset))))
    (should (not (member "edit_buffer" (superchat-preset-tools preset))))))

(ert-deftest test-subagent-preset-executor ()
  "The executor preset should include write/execute tools."
  (let ((preset (superchat--subagent-preset 'executor)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (equal "coding" (superchat-preset-capability preset)))
    (should (member "edit_buffer" (superchat-preset-tools preset)))
    (should (member "replace_buffer" (superchat-preset-tools preset)))
    (should (member "eval_elisp" (superchat-preset-tools preset)))
    (should (member "shell-command" (superchat-preset-tools preset)))
    (should (member "workspace_read" (superchat-preset-tools preset)))
    (should (member "workspace_write" (superchat-preset-tools preset)))
    (should (member "workspace_info" (superchat-preset-tools preset)))))

(ert-deftest test-subagent-preset-introspector ()
  "The introspector preset should include Emacs introspection tools."
  (let ((preset (superchat--subagent-preset 'introspector)))
    (should preset)
    (should (eq (superchat-preset-type preset) 'agent))
    (should (equal "introspection" (superchat-preset-capability preset)))
    (should (member "describe-function" (superchat-preset-tools preset)))
    (should (member "eval_elisp" (superchat-preset-tools preset)))
    (should (member "workspace_read" (superchat-preset-tools preset)))
    (should (member "workspace_info" (superchat-preset-tools preset)))
    (should (not (member "workspace_write"
                         (superchat-preset-tools preset))))))

(ert-deftest test-subagent-render-report ()
  "`superchat--subagent-render-report' should insert a report block."
  (let ((superchat-buffer-name "*subagent-render-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (superchat--subagent-render-report "researcher" "Found the answer.")
          (let ((content (buffer-string)))
            (should (string-match-p "Sub-agent report: researcher" content))
            (should (string-match-p "Found the answer." content))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-cmd-subagent-parses-args ()
  "`superchat--cmd-subagent' should parse preset and task from args."
  (let ((result nil)
        (superchat-buffer-name "*cmd-subagent-test*"))
    (unwind-protect
        (cl-letf (((symbol-function 'superchat--subagent-run-async)
                   (lambda (preset task _context callback &optional _depth)
                     (setq result (list preset task))
                     (funcall callback "report"))))
          (superchat--cmd-subagent "subagent" "researcher find docs" nil nil nil)
          (should (equal result '("researcher" "find docs"))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

;; =======================================================
;; Async engine (v1.3)
;; =======================================================

(ert-deftest test-subagent-handoff-decision-maps-closure-verdicts ()
  "Child handoff maps Closure evidence to deliver, wait, review, or reject."
  (let (well-formed terminal)
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (&rest _args) '(:trace)))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     well-formed
                   terminal))))
      (dolist
          (case
           '(((:verdict valid)
              (:verdict proved :judgment (:terminal-kind completed))
              deliver)
             ((:verdict valid) (:verdict open) wait)
             ((:verdict valid) (:verdict unknown) manual-review)
             ((:verdict valid)
              (:verdict proved :judgment (:terminal-kind failed))
              reject)
             ((:verdict invalid) nil reject)
             ((:verdict unknown) nil manual-review)))
        (setq well-formed (nth 0 case)
              terminal (nth 1 case))
        (should
         (eq (nth 2 case)
             (plist-get
              (superchat-subagent-handoff-decision
               "child-session" "child-run")
              :action)))))))

(ert-deftest test-subagent-finish-withholds-unproved-report ()
  "A completed callback cannot expose a report without a handoff proof."
  (let* ((id "child-1")
         delivered
         team-report
         (superchat--subagent-running
          (list
           (cons id
                 (list :id id
                       :run-id "run-1"
                       :callback (lambda (report) (setq delivered report)))))))
    (cl-letf (((symbol-function 'superchat-subagent-handoff-decision)
               (lambda (&rest _args) '(:action wait :run-id "run-1")))
              ((symbol-function 'superchat--team-finish-delegated-task)
               (lambda (_entry report) (setq team-report report)))
              ((symbol-function 'superchat--subagent-refresh-panel) #'ignore))
      (superchat--subagent-finish id "unverified child report" 'completed))
    (should (string-prefix-p "[Error: sub-agent handoff" delivered))
    (should-not (string-match-p "unverified child report" delivered))
    (should (equal delivered team-report))
    (should-not superchat--subagent-running)))

(ert-deftest test-subagent-artifact-delivery-requires-proved-current-revision ()
  "Artifact handoff requires Child closure and the exact produced revision."
  (let ((reference '(:workspace-id "workspace-1"
                     :artifact-id "artifact-1"
                     :revision "revision-1"))
        (artifact '(:workspace-id "workspace-1"
                    :artifact-id "artifact-1"
                    :uri "buffer://artifact-1"
                    :revision "revision-1"
                    :content-hash "revision-1"
                    :event-id "evt-artifact"
                    :session-id "child-session"
                    :run-id "child-run"
                    :actor-id "child-actor"))
        (trace '((:event-id "evt-start")
                 (:event-id "evt-artifact")
                 (:event-id "evt-terminal")))
        (terminal '(:verdict proved
                    :judgment (:terminal-kind completed)
                    :witness (:event-ids ("evt-start" "evt-terminal")))))
    (cl-letf (((symbol-function 'superchat-tape-replay-events)
               (lambda (&rest _args) trace))
              ((symbol-function 'superchat-closure-judge)
               (lambda (query _trace)
                 (if (eq 'trace-well-formed (plist-get query :kind))
                     '(:verdict valid)
                   terminal)))
              ((symbol-function 'superchat-workspace-artifacts)
               (lambda (&rest _args) (list artifact))))
      (should
       (eq 'deliver
           (plist-get
            (superchat-workspace-artifact-delivery-decision
             "child-session" "child-run" reference)
            :action)))
      (setf (plist-get reference :revision) "stale")
      (should
       (eq 'reject
           (plist-get
            (superchat-workspace-artifact-delivery-decision
             "child-session" "child-run" reference)
            :action)))
      (setf (plist-get reference :revision) "revision-1")
      (setf (plist-get artifact :run-id) nil)
      (should
       (eq 'manual-review
           (plist-get
            (superchat-workspace-artifact-delivery-decision
             "child-session" "child-run" reference)
            :action))))))

(ert-deftest test-subagent-runtime-handoff-survives-local-state-loss ()
  "A real child report remains deliverable from Tape after local state loss."
  (skip-unless (and (fboundp 'sqlite-available-p) (sqlite-available-p)))
  (test-subagent--with-team-db
    ;; `let*': the fake-parent Run registered below must land in THIS test's
    ;; fresh `superchat-runtime--runs' table, not whatever table was active
    ;; when the outer `let' form itself is entered.
    (let* ((superchat--subagent-running nil)
          (superchat-runtime--runs (make-hash-table :test #'equal))
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (superchat-subagent-timeout nil)
          (superchat-closure-terminal-guard-enabled t)
          ;; S11-fix-1: retained so `unwind-protect' below can cancel the
          ;; parent Run and kill its buffer on every exit path, including
          ;; one where the body signals before reaching the later `clrhash'.
          (parent-buffer (generate-new-buffer " *sc11-fake-parent*"))
          ;; S11/§7.4: :parent-run-id must name a real, live Run -- a
          ;; fabricated id is refused at submission.  Register one directly
          ;; (its own transport stub never fires the terminal observer, so
          ;; it stays non-terminal for the child submission below) rather
          ;; than a literal string.
          (parent-run-id
           (plist-get
            (cl-letf (((symbol-function 'superchat--llm-generate-answer)
                       #'ignore))
              (superchat-runtime-submit
               (list :prompt "parent" :deadline nil :buffer parent-buffer
                     :session-id "parent-session")))
            :run-id))
          (superchat--tool-execution-context
           (list :run-id parent-run-id
                 :attendance 'attended
                 :workspace-id "research-workspace"))
          (superchat--team-id "parent-session")
          child-id report artifact-reference)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'superchat--execute-llm-query)
                       (lambda (_turn)
                         '(:prompt "child prompt" :system-prompt "system"
                           :tools none)))
                      ((symbol-function 'superchat--subagent-prepare-tools)
                       (lambda (&rest _) nil))
                      ((symbol-function 'superchat--team-start-delegated-task)
                       (lambda (&rest _) nil))
                      ((symbol-function 'superchat--team-finish-delegated-task)
                       #'ignore)
                      ((symbol-function 'superchat--subagent-refresh-panel) #'ignore)
                      ((symbol-function 'superchat--update-status) #'ignore)
                      ((symbol-function 'superchat--llm-generate-answer)
                       (lambda (&rest _args)
                         (with-temp-buffer
                           (insert "durable research artifact")
                           (let ((artifact
                                  (superchat-workspace-add-region
                                   (point-min) (point-max) 'report)))
                             (setq artifact-reference
                                   (list :workspace-id "research-workspace"
                                         :artifact-id
                                         (plist-get artifact :artifact-id)
                                         :revision
                                         (plist-get artifact :revision)))))
                         (funcall superchat--llm-runtime-observer
                                  'terminal
                                  '(:status completed
                                    :response "durable child report")))))
              (setq child-id
                    (superchat--subagent-run-async
                     'researcher "research" nil
                     (lambda (value) (setq report value)))))
            (should (equal "durable child report" report))
            (should artifact-reference)
            (let* ((trace (superchat-tape-replay-events child-id))
                   (started
                    (cl-find "run.started" trace
                             :key (lambda (event) (plist-get event :type))
                             :test #'equal))
                   (run-id (plist-get started :run-id)))
              (should (stringp run-id))
              ;; S11-fix-2: cancel the still-registered fake parent and pump
              ;; its timer-0 cascade (`run-at-time 0 nil
              ;; #'superchat-runtime--cancel-children') while the Run table
              ;; still knows about it -- the `clrhash' below would otherwise
              ;; make every later cancel attempt a guaranteed no-op.
              (should (superchat-runtime-cancel parent-run-id))
              (sit-for 0.05)
              (clrhash superchat-runtime--runs)
              (setq superchat-workspace--registry nil)
              (superchat-db-close)
              (should
               (eq 'deliver
                   (plist-get
                    (superchat-subagent-handoff-decision child-id run-id)
                    :action)))
              (should
               (eq 'deliver
                   (plist-get
                    (superchat-workspace-artifact-delivery-decision
                     child-id run-id artifact-reference)
                    :action)))))
        ;; Only reached on an early-error exit (the body above now cancels
        ;; the parent itself, before `clrhash', on the successful path): a
        ;; body that signals before that explicit cancel still leaves the
        ;; real, still-live parent registered, so cancel it here (pumping
        ;; its -- here empty -- cascade) and kill its buffer rather than
        ;; leaking either.  A no-op on the successful path, since the parent
        ;; is already cancelled and gone from the table by the time we get
        ;; here.
        (ignore-errors (superchat-runtime-cancel parent-run-id))
        (sit-for 0.05)
        (when (buffer-live-p parent-buffer)
          (let ((kill-buffer-query-functions nil))
            (kill-buffer parent-buffer)))))))

(ert-deftest test-subagent-control-plane-lists-and-cancels ()
  "A running Runtime run is visible and cancelled through Runtime."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        (superchat--tool-execution-context
         '(:run-id "run-parent" :attendance attended))
        submitted
        cancelled-run
        cancel-reason
        member-events
        (report nil))
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq submitted request)
                 '(:turn-id "turn-1" :run-id "run-1"
                   :session-id "session-1")))
              ((symbol-function 'superchat--team-member-event)
               (lambda (ctx event)
                 (push (list :event event :ctx (copy-tree ctx))
                       member-events)))
              ((symbol-function 'superchat-runtime-cancel)
               (lambda (run-id reason)
                 (setq cancelled-run run-id
                       cancel-reason reason)
                 (subagent-test--deliver submitted reason 'cancelled 9)
                 t)))
      (let ((id (superchat--subagent-run-async
                 'researcher "task" nil (lambda (r) (setq report r)))))
        (let ((entry (superchat--subagent-entry id)))
          (should (equal id (plist-get entry :actor-id)))
          (should (equal "researcher" (plist-get entry :profile-id)))
          (should (equal "run-1" (plist-get entry :run-id)))
          (should (equal "run-parent"
                         (plist-get entry :parent-run-id))))
        (should (equal "run-parent"
                       (plist-get submitted :parent-run-id)))
        (should-not (plist-member submitted :agent-id))
        (let* ((started
                (cl-find "started" member-events
                         :key (lambda (item) (plist-get item :event))
                         :test #'equal))
               (ctx (plist-get started :ctx)))
          (should (equal id (plist-get ctx :actor-id)))
          (should (equal "researcher" (plist-get ctx :profile-id)))
          (should (equal "run-1" (plist-get ctx :run-id)))
          (should (equal "run-parent" (plist-get ctx :parent-run-id))))
        (should (string-match-p id (superchat-subagent-list-running)))
        (should (equal "run-1"
                       (plist-get (superchat--subagent-entry id) :run-id)))
        (should (superchat-subagent-cancel id))
        (should (equal "run-1" cancelled-run))
        ;; The report opens with the cancellation marker and instructs the
        ;; delegating model not to redo the task itself.
        (should (equal report cancel-reason))
        (should (string-prefix-p "[Cancelled by user" report))
        (should (string-match-p "Do NOT attempt the task yourself" report))
        (should-not superchat--subagent-running)))))

(ert-deftest test-subagent-control-plane-finishes-once ()
  "A late Runtime delivery after cancellation is ignored."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        (calls 0)
        delivery)
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq delivery (lambda (text status tape-id)
                                 (subagent-test--deliver request text status tape-id)))
                 '(:turn-id "turn-1" :run-id "run-1"
                   :session-id "session-1")))
              ((symbol-function 'superchat-runtime-cancel)
               (lambda (_run-id reason)
                 (funcall delivery reason 'cancelled 9)
                 t)))
      (let ((id (superchat--subagent-run-async
                 'researcher "task" nil (lambda (_r) (cl-incf calls)))))
        (superchat-subagent-cancel id)
        (funcall delivery "late" 'completed 10)
        (should (= 1 calls))))))

(ert-deftest test-subagent-synchronous-delivery-keeps-run-lineage ()
  "A synchronous Runtime delivery waits until the child Run is identified."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        events report)
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-subagent-handoff-decision)
               (lambda (&rest _) '(:action deliver)))
              ((symbol-function 'superchat--team-start-delegated-task)
               (lambda (&rest _) nil))
              ((symbol-function 'superchat--team-member-event)
               (lambda (ctx event)
                 (push (list event (copy-tree ctx)) events)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (subagent-test--deliver request "done" 'completed 1)
                 '(:turn-id "turn-sync" :run-id "run-sync"
                   :session-id "session-sync"))))
      (superchat--subagent-run-async
       'researcher "task" nil (lambda (value) (setq report value)))
      (setq events (nreverse events))
      (should (equal "done" report))
      (should (equal '("started" "completed") (mapcar #'car events)))
      (dolist (item events)
        (should (equal "run-sync" (plist-get (cadr item) :run-id)))))))

(ert-deftest subagent/preaccept-rejection-rolls-back-fresh-only ()
  "Real Runtime rejection rolls back fresh hosts and preserves reused hosts."
  (dolist (fresh '(t nil))
    (let* ((superchat--subagent-running nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (session (format "subagent-preaccept-%s" (gensym)))
           (original-context (symbol-function 'superchat--subagent-make-context))
           (reused (unless fresh
                     (plist-get
                      (superchat-editor-workspace-acquire-buffer
                       session 'subagent-protocol)
                      :buffer)))
           report)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat--subagent-make-context)
                     (lambda (preset depth)
                       (let ((ctx (funcall original-context preset depth)))
                         (setq ctx (plist-put ctx :session-id session))
                         (plist-put ctx :actor-id session))))
                    ((symbol-function 'superchat--execute-llm-query)
                     (lambda (_turn) '(:prompt "prompt" :tools none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--team-start-delegated-task)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ;; A valid producer request has no naturally rejectable
                    ;; Runtime validation branch.  Replace only the public
                    ;; submission seam to model a true pre-accept rejection;
                    ;; acquisition, validation, and rollback remain real.
                    ((symbol-function 'superchat-runtime-submit)
                     (lambda (_request) (error "preaccept rejection"))))
            (superchat--subagent-run-async
             'researcher "task" nil (lambda (value) (setq report value)))
            (should (string-match-p "preaccept rejection" report))
            (if fresh
                (should-not
                 (superchat-editor-workspace-owned-buffers session))
              (should (equal (list reused)
                             (superchat-editor-workspace-owned-buffers
                              session)))))
        (ignore-errors (superchat-session-workspace-close session))))))

(ert-deftest subagent/delivery-before-propagated-signal-proves-acceptance ()
  "Every accepted terminal keeps the exact host and finishes at most once."
  (dolist (case '(success cancel handle provider-error provider-quit
                  callback-error callback-quit))
    (let* ((superchat--subagent-running nil)
           (superchat-runtime--runs (make-hash-table :test #'equal))
           (superchat-closure-shadow-enabled nil)
           (superchat-closure-terminal-guard-enabled nil)
           (session (format "subagent-accepted-%s" (gensym)))
           (original-context (symbol-function 'superchat--subagent-make-context))
           (callbacks 0) id report)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat--subagent-make-context)
                     (lambda (preset depth)
                       (let ((ctx (funcall original-context preset depth)))
                         (setq ctx (plist-put ctx :session-id session))
                         (plist-put ctx :actor-id session))))
                    ((symbol-function 'superchat--execute-llm-query)
                     (lambda (_turn) '(:prompt "prompt" :tools none)))
                    ((symbol-function 'superchat--subagent-prepare-tools)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--team-start-delegated-task)
                     (lambda (&rest _) nil))
                    ((symbol-function 'superchat--team-member-event) #'ignore)
                    ((symbol-function 'superchat--team-finish-delegated-task)
                     #'ignore)
                    ((symbol-function 'superchat--subagent-refresh-panel)
                     #'ignore)
                    ((symbol-function 'superchat-subagent-handoff-decision)
                     (lambda (&rest _) '(:action deliver)))
                    ((symbol-function 'superchat--update-status) #'ignore)
                    ((symbol-function 'superchat-tape-append-event)
                     (let ((event-id 0))
                       (lambda (_event) (cl-incf event-id))))
                    ((symbol-function 'superchat--llm-generate-answer)
                     (lambda (&rest _args)
                       (pcase case
                         ('handle nil)
                         ('cancel
                          (funcall superchat--llm-runtime-observer
                                   'terminal
                                   '(:status cancelled :response "cancelled")))
                         ('provider-error (error "provider error"))
                         ('provider-quit (signal 'quit '(provider-quit)))
                         (_
                          (funcall superchat--llm-runtime-observer
                                   'terminal
                                   '(:status completed :response "done")))))))
            (setq id
                  (superchat--subagent-run-async
                   'researcher "task" nil
                   (lambda (value)
                     (cl-incf callbacks)
                     (setq report value)
                     (pcase case
                       ('callback-error (error "callback error"))
                       ('callback-quit (signal 'quit '(callback-quit)))))))
            (when id (should (equal session id)))
            (should (= 1 (length
                          (superchat-editor-workspace-owned-buffers session))))
            (when (eq case 'handle)
              (should (superchat--subagent-entry session))
              (superchat-subagent-cancel session))
            (should (= 1 callbacks))
            (should report)
            (should-not (superchat--subagent-entry session)))
        (ignore-errors (superchat-session-workspace-close session))))))

(ert-deftest subagent/rollback-refusal-retains-public-retry-host ()
  "A rejected fresh child whose rollback is refused remains retryable."
  (let* ((superchat--subagent-running nil)
         (session (format "subagent-rollback-retained-%s" (gensym)))
         (original-context (symbol-function 'superchat--subagent-make-context))
         report host)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat--subagent-make-context)
                   (lambda (preset depth)
                     (let ((ctx (funcall original-context preset depth)))
                       (setq ctx (plist-put ctx :session-id session))
                       (plist-put ctx :actor-id session))))
                  ((symbol-function 'superchat--execute-llm-query)
                   (lambda (_turn) '(:prompt "prompt" :tools none)))
                  ((symbol-function 'superchat--subagent-prepare-tools)
                   (lambda (&rest _) nil))
                  ((symbol-function 'superchat--team-start-delegated-task)
                   (lambda (&rest _) nil))
                  ((symbol-function 'superchat--subagent-refresh-panel)
                   #'ignore)
                  ((symbol-function 'superchat-runtime-submit)
                   (lambda (_request)
                     (setq host
                           (car (superchat-editor-workspace-owned-buffers
                                 session)))
                     (with-current-buffer host
                       (setq-local kill-buffer-query-functions
                                   (list (lambda () nil))))
                     (error "primary rejection"))))
          (superchat--subagent-run-async
           'researcher "task" nil (lambda (value) (setq report value)))
          (should (string-match-p "primary rejection" report))
          (should (buffer-live-p host))
          (should (eq 'retained
                      (superchat-editor-workspace-state session)))
          (with-current-buffer host
            (setq-local kill-buffer-query-functions nil))
          (should (eq 'closed
                      (superchat-session-workspace-close session))))
      (ignore-errors (superchat-session-workspace-close session)))))

(ert-deftest test-subagent-round-limit-pauses-timeout-for-confirmation ()
  "An interactive round-limit prompt does not consume sub-agent runtime."
  (let* ((id "subagent-timeout-pause")
         (ctx (list :session-id id))
         (superchat-llm-round-limit-action 'ask)
         (superchat-llm-max-tool-rounds 12)
         (noninteractive nil)
         (superchat--subagent-running
          (list (cons id (list :id id :timer 'old-timer :timeout 120))))
         cancelled resumed-delay)
    (cl-letf (((symbol-function 'timerp) (lambda (_timer) t))
              ((symbol-function 'timer--time)
               (lambda (_timer)
                 (time-add (current-time) (seconds-to-time 30))))
              ((symbol-function 'cancel-timer)
               (lambda (timer) (setq cancelled timer)))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat _function &rest _args)
                 (setq resumed-delay delay)
                 'resumed-timer))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (eq 'finalize (superchat--subagent-round-limit-action ctx)))
      (should (eq 'old-timer cancelled))
      (should (and (>= resumed-delay 29) (<= resumed-delay 30)))
      (should (eq 'resumed-timer
                  (plist-get (superchat--subagent-entry id) :timer))))))

(ert-deftest test-subagent-timeout-cancels-request ()
  "Preset timeout schedules cancellation through the shared path."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout 30)
        submitted
        cancelled-run
        scheduled-delay scheduled-function report)
    (cl-letf (((symbol-function 'superchat--subagent-preset)
               (lambda (_name)
                 (superchat-preset-from-plist
                  '(:name "short" :type agent :body "Body" :tools none
                    :timeout 5))))
              ((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq submitted request)
                 '(:turn-id "turn-short" :run-id "run-short"
                   :session-id "session-short")))
              ((symbol-function 'superchat-runtime-cancel)
               (lambda (run-id reason)
                 (setq cancelled-run run-id)
                 (subagent-test--deliver submitted reason 'cancelled 9)
                 t))
              ((symbol-function 'run-at-time)
               (lambda (delay _repeat function &rest _args)
                 (setq scheduled-delay delay scheduled-function function)
                 'timer))
              ((symbol-function 'cancel-timer) #'ignore))
      (let ((id (superchat--subagent-run-async
                 "short" "task" nil (lambda (value) (setq report value)))))
        (should-not report)
        (should (= 5 scheduled-delay))
        (funcall scheduled-function)
        (should (equal "run-short" cancelled-run))
        (should (string-match-p "Timed out after 5s" report))
        (should-not (superchat--subagent-entry id))))))

(ert-deftest test-subagent-timeout-ignores-nonpositive-global-value ()
  "Invalid Customize values do not schedule immediate or past timers."
  (let ((superchat-subagent-timeout -1)
        (preset (superchat-preset-from-plist '(:name "plain"))))
    (should-not (superchat--subagent-effective-timeout preset))))

(ert-deftest test-subagent-run-async-unknown-preset ()
  "Unknown preset is delivered as an error string, never signaled."
  (let ((report nil))
    (superchat--subagent-run-async
     "no-such-preset" "task" nil (lambda (r) (setq report r)))
    (should (stringp report))
    (should (string-match-p "\\[Error" report))
    (should (string-match-p "no-such-preset" report))))

(ert-deftest test-subagent-run-async-returns-report ()
  "The async runner builds the prompt and delivers the LLM answer."
  (let ((report nil)
        (captured-prompt nil)
        (captured-system nil)
        (captured-request nil))
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (turn)
                 (list :prompt (superchat-turn-prompt turn)
                       :system-prompt "research sub-agent system"
                       :tools 'none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-subagent-handoff-decision)
               (lambda (&rest _) '(:action deliver)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (setq captured-request request
                       captured-prompt (plist-get request :prompt)
                       captured-system (plist-get request :system-prompt))
                 (subagent-test--deliver request "async report" 'completed 9)
                 '(:turn-id "turn-1" :run-id "run-1"
                   :session-id "session-1"))))
      (superchat--subagent-run-async
       'researcher "investigate X" "background info"
       (lambda (r) (setq report r))))
    (should (string= report "async report"))
    (should (string-match-p "investigate X" captured-prompt))
    (should (string-match-p "background info" captured-prompt))
    ;; Preset persona (body) travels as the system prompt.
    (should (stringp captured-system))
    (should (string-match-p "research sub-agent" captured-system))
    ;; Explicit context: isolated session id and default depth 1.
    (should (string-prefix-p "subagent-researcher-"
                             (plist-get captured-request :session-id)))
    (should (equal (plist-get captured-request :session-id)
                   (plist-get captured-request :actor-id)))
    (should (plist-member captured-request :record-conversation))
    (should (eq nil (plist-get captured-request :record-conversation)))
    (should-not (plist-member captured-request :agent-mode))))

(ert-deftest test-subagent-run-async-does-not-touch-main-session ()
  "Async delegation must not mutate the main session's dynamic state."
  (let ((superchat--session-id "main-session")
        (superchat--conversation-history '((:role user :content "main")))
        (report nil))
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) '(prepared-tools)))
              ((symbol-function 'superchat-subagent-handoff-decision)
               (lambda (&rest _) '(:action deliver)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (request)
                 (subagent-test--deliver request "ok" 'completed 9)
                 '(:turn-id "turn-1" :run-id "run-1"
                   :session-id "session-1"))))
      (superchat--subagent-run-async
       'researcher "task" nil (lambda (r) (setq report r))))
    (should (string= report "ok"))
    (should (string= superchat--session-id "main-session"))
    (should (equal superchat--conversation-history
                   '((:role user :content "main"))))))

(ert-deftest test-subagent-parallel-async-aggregates-in-spec-order ()
  "Results aggregate in spec order even when completion is out of order."
  (let ((held nil)   ; (callback . report) conses; car of list = launched last
        (final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset task _context callback &optional _depth)
                 (push (cons callback (format "report-%s-%s" preset task))
                       held))))
      (superchat--subagent-run-parallel-async
       '((:preset "researcher" :task "a")
         (:preset "executor" :task "b"))
       (lambda (r) (setq final r)))
      (should (= 2 (length held)))
      ;; Complete in REVERSE launch order: b first, then a.
      (dolist (entry held)
        (funcall (car entry) (cdr entry))))
    (should final)
    (should (< (string-match "report-researcher-a" final)
               (string-match "report-executor-b" final)))))

(ert-deftest test-subagent-parallel-async-launch-window ()
  "At most `superchat-subagent-parallel-max' sub-agents are in flight;
completions launch the queued remainder."
  (let ((superchat-subagent-parallel-max 2)
        (launched nil)  ; alist preset -> callback
        (final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset _task _context callback &optional _depth)
                 (push (cons preset callback) launched))))
      (superchat--subagent-run-parallel-async
       '((:preset "a" :task "1") (:preset "b" :task "2")
         (:preset "c" :task "3") (:preset "d" :task "4"))
       (lambda (r) (setq final r)))
      ;; Only the window is launched initially.
      (should (= 2 (length launched)))
      ;; Completing one launches exactly one more.
      (funcall (cdr (assoc "a" launched)) "ra")
      (should (= 3 (length launched)))
      (funcall (cdr (assoc "b" launched)) "rb")
      (should (= 4 (length launched)))
      (funcall (cdr (assoc "c" launched)) "rc")
      (funcall (cdr (assoc "d" launched)) "rd"))
    (should final)
    (should (string-match-p "Sub-agent report: d" final))))

(ert-deftest test-subagent-parallel-async-error-isolation ()
  "One failing sub-agent must not prevent the others from aggregating."
  (let ((final nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (preset task _context callback &optional _depth)
                 (funcall callback
                          (if (string= preset "executor")
                              "[Error: executor failed]"
                            (format "report-%s-%s" preset task))))))
      (superchat--subagent-run-parallel-async
       '((:preset "researcher" :task "a")
         (:preset "executor" :task "b"))
       (lambda (r) (setq final r))))
    (should (string-match-p "report-researcher-a" final))
    (should (string-match-p "executor failed" final))))

(ert-deftest test-subagent-parse-specs ()
  "JSON task arrays parse into spec plists; bad JSON returns an error string."
  (let ((specs (superchat--subagent-parse-specs
                "[{\"preset\": \"researcher\", \"task\": \"find docs\"}]")))
    (should (listp specs))
    (should (equal "researcher" (plist-get (car specs) :preset)))
    (should (equal "find docs" (plist-get (car specs) :task))))
  (should (stringp (superchat--subagent-parse-specs "not json"))))

(ert-deftest test-subagent-runtime-identity-reaches-wrapped-tools ()
  "Wrapped tools keep the Runtime Run and Turn identity Closure requires."
  (let ((superchat--subagent-running nil)
        (superchat-subagent-timeout nil)
        started-context)
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn)
                 '(:prompt "prepared prompt"
                   :system-prompt "system"
                   :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) nil))
              ((symbol-function 'superchat--team-start-delegated-task)
               (lambda (&rest _) nil))
              ((symbol-function 'superchat--team-member-event)
               (lambda (ctx event)
                 (when (equal event "started")
                   (setq started-context (copy-tree ctx)))))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (_request)
                 '(:turn-id "turn-runtime"
                   :run-id "run-runtime"
                   :session-id "session-runtime"))))
      (superchat--subagent-run-async 'researcher "task" nil #'ignore)
      (should (equal "turn-runtime"
                     (plist-get started-context :turn-id)))
      (should (equal "run-runtime"
                     (plist-get started-context :run-id)))
      (let ((tool-context
             (superchat--subagent-tool-context started-context)))
        (should (equal "turn-runtime"
                       (plist-get tool-context :turn-id)))
        (should (equal "run-runtime"
                       (plist-get tool-context :run-id)))))))

(ert-deftest test-subagent-tool-context-reads-identity-from-stable-holder ()
  "`superchat--subagent-tool-context' resolves :run-id/:turn-id from CTX's
stable :runtime-identity holder (published via
`superchat--subagent-publish-identity'), not by relying on `plist-put'
mutating the exact list object a wrapped tool closure already captured.
Covers a context with a pre-existing :run-id key (the real shape
`superchat--subagent-make-context' builds) and one without it entirely,
both with :turn-id initially absent, per S03b-fix-3's own instruction."
  (dolist (drop-run-id-key '(nil t))
    (let* ((preset (superchat--subagent-preset 'researcher))
           (ctx (superchat--subagent-make-context preset 1)))
      (when drop-run-id-key (cl-remf ctx :run-id))
      (should-not (plist-get ctx :turn-id))
      ;; Simulate CTX's own plist head later being rebuilt into a
      ;; different list object -- e.g. a future change, or simply any code
      ;; that reassigns CTX via a non-mutating operation such as `append'.
      ;; A wrapped tool closure captures CTX (or a context derived from it
      ;; before this point) by reference; it cannot see this rebuild.
      (let ((rebuilt (append ctx nil)))
        (superchat--subagent-publish-identity rebuilt "run-holder" "turn-holder")
        ;; `ctx' below is the pre-rebuild reference -- what a wrapped
        ;; closure would actually have captured.
        (let ((resolved (superchat--subagent-tool-context ctx)))
          (should (equal "run-holder" (plist-get resolved :run-id)))
          (should (equal "turn-holder" (plist-get resolved :turn-id))))))))

(defun test-subagent--fixture-preset-entry ()
  "Return a builtin-preset alist entry whose own tool list includes
`delegate_to_subagent'.  None of the real built-in presets (researcher,
executor, introspector) request that tool for themselves, so the depth-guard
tests need this one to let the real `superchat--subagent-prepare-tools' wrap
a real delegate tool without depending on, or changing, any built-in
preset's default tool selection."
  (cons "depth-guard-fixture"
        (lambda ()
          (superchat--subagent-make-preset
           "depth-guard-fixture" "depth-guard-fixture"
           "Depth-guard journey fixture preset." 'agent "test"
           '("delegate_to_subagent")
           "You are a test sub-agent used only to exercise the depth guard."))))

(ert-deftest test-subagent-depth-guard-blocks-nested-delegation ()
  "At the depth limit, the real prepare-tools/submit/invoke order still
denies delegation instead of losing the child Run's identity.

This exercises the real production ordering directly: `superchat--subagent-run-async'
prepares and wraps tools before `superchat-runtime-submit' returns, and only
`llm-chat-streaming'/`llm-chat-async' (via `journey-fixture-with') are
replaced.  Before this issue's fix, the depth-blocked tool's wrapper froze a
copy of the context at wrap time -- before Runtime ever assigned it a
Run/Turn identity -- so invoking it through this real order signaled an
identity error instead of returning the depth-limit denial."
  (skip-unless (fboundp 'llm-make-tool))
  (journey-fixture-with (:tools-policy 'always)
    (require 'superchat-subagent)
    (let* ((superchat-subagent-max-depth 1)
           (superchat--subagent-builtin-presets
            (cons (test-subagent--fixture-preset-entry)
                  superchat--subagent-builtin-presets))
           (report nil)
           child-id)
      (journey-fixture-set-script
       (list (list :tool-uses
                   '(("delegate_to_subagent"
                      . ((preset . "researcher") (task . "nested task")))))
             (list :text "final answer after the denial")))
      (superchat--subagent-run-async
       'depth-guard-fixture "top task" nil
       (lambda (r) (setq report r)) 1)
      ;; Everything up to the Runtime submit call runs synchronously (see
      ;; `superchat--subagent-run-async''s own comment to that effect), so the
      ;; new entry is already in place the instant the call above returns.
      (setq child-id (car (car superchat--subagent-running)))
      (should (stringp child-id))
      (should (journey-fixture-wait-until (lambda () report) 5))
      ;; Exactly 2 provider rounds: the tool-use round and the follow-up that
      ;; sees the tool's result.  A third round would mean native delegation
      ;; actually ran despite the depth limit.
      (should (= 2 (length (journey-fixture-requests))))
      (should
       (string-match-p
        "depth limit"
        (plist-get (nth 1 (journey-fixture-requests)) :prompt-text)))
      (let* ((events (journey-fixture-events child-id))
             (requested
              (cl-find-if
               (lambda (event)
                 (and (equal (plist-get event :type) "tool.requested")
                      (equal (plist-get (plist-get event :payload) :tool-name)
                             "delegate_to_subagent")))
               events)))
        (should requested)
        (should (and (stringp (plist-get requested :run-id))
                    (not (string-empty-p (plist-get requested :run-id)))))
        (should (and (stringp (plist-get requested :turn-id))
                    (not (string-empty-p (plist-get requested :turn-id)))))))))

(ert-deftest test-subagent-depth-guard-allows-below-limit ()
  "Below the depth limit, the same real order lets nested delegation
actually run and deliver its report back into the parent's next round."
  (skip-unless (fboundp 'llm-make-tool))
  (journey-fixture-with (:tools-policy 'always)
    (require 'superchat-subagent)
    (let* ((superchat-subagent-max-depth 2)
           (superchat--subagent-builtin-presets
            (cons (test-subagent--fixture-preset-entry)
                  superchat--subagent-builtin-presets))
           (report nil))
      (journey-fixture-set-script
       (list (list :tool-uses
                   '(("delegate_to_subagent"
                      . ((preset . "researcher") (task . "nested task")))))
             (list :text "nested findings recorded")
             (list :text "final report using the nested findings")))
      (superchat--subagent-run-async
       'depth-guard-fixture "top task" nil
       (lambda (r) (setq report r)) 1)
      (should (journey-fixture-wait-until (lambda () report) 5))
      (should-not (string-match-p "depth limit" report))
      ;; 3 provider rounds: the parent's tool-use round, the real nested
      ;; Run's own round, and the parent's follow-up -- proof delegation
      ;; actually reached a second, real sub-agent instead of being denied.
      (should (= 3 (length (journey-fixture-requests))))
      (should
       (string-match-p
        "nested findings recorded"
        (plist-get (nth 2 (journey-fixture-requests)) :prompt-text))))))

(ert-deftest test-subagent-wrapped-tool-counts-and-errors-as-strings ()
  "Wrapped sub-agent tools enforce max calls and stringify errors."
  (let* ((superchat-agent-max-tool-calls 50)
         (preset (superchat-preset-from-plist
                  (list :name "bounded" :type 'agent :max-tool-calls 2)))
         (ctx (superchat--subagent-make-context preset 1))
         (boom (lambda (&rest _) (error "boom")))
         (wrapped (superchat--subagent-wrap-function ctx "t" boom nil)))
    (let ((superchat-authority-approval-function (lambda (_request) t)))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        ;; Errors come back as strings, not signals.
        (should (string-match-p
                 "\\[Tool call failed: boom\\]" (funcall wrapped "x")))
        (should (string-match-p
                 "\\[Tool call failed: boom\\]" (funcall wrapped "x")))
        ;; Third call exceeds the limit.
        (should (string-match-p
                 "exceeded maximum tool calls" (funcall wrapped "x")))))))

(ert-deftest test-subagent-profile-can-require-destructive-confirmation ()
  "A profile can require confirmation when the global policy does not."
  (let* ((superchat-agent-confirm-destructive nil)
         (superchat-agent-destructive-tools '("write-file"))
         (preset (superchat-preset-from-plist
                  (list :name "careful" :type 'agent
                        :confirm-destructive t)))
         (ctx (superchat--subagent-make-context preset 1))
         (called nil)
         (wrapped (superchat--subagent-wrap-function
                   ctx "write-file" (lambda (&rest _) (setq called t)) nil)))
    (let ((superchat-authority-approval-function #'ignore))
      (cl-letf (((symbol-function 'superchat-tape-append-event)
                 (lambda (_event) 1)))
        (should
         (string-match-p "cancelled" (funcall wrapped "file" "body")))))
    (should-not called)))

(ert-deftest test-subagent-context-can-only-tighten-parent-authority ()
  "A child inherits attendance and cannot widen parent guardrails."
  (let* ((superchat-agent-max-tool-calls 50)
         (superchat-agent-confirm-destructive nil)
         (superchat--tool-execution-context
          '(:attendance unattended
            :authority-policy observer
            :workspace-id "workspace-1"
            :workspace-root "/project/"
            :guardrails (:max-tool-calls 3 :confirm-destructive t)))
         (preset
          (superchat-preset-from-plist
           (list :name "child" :type 'agent
                 :max-tool-calls 20 :confirm-destructive nil)))
         (ctx (superchat--subagent-make-context preset 1))
         (guardrails (plist-get ctx :guardrails)))
    (should (eq (plist-get ctx :attendance) 'unattended))
    (should (eq (plist-get ctx :authority-policy) 'observer))
    (should (equal (plist-get ctx :workspace-id) "workspace-1"))
    (should (equal (plist-get ctx :workspace-root) "/project/"))
    (should (= (plist-get guardrails :max-tool-calls) 3))
    (should (eq (plist-get guardrails :confirm-destructive) t))))

(ert-deftest test-subagent-context-uses-preset-profile-identity ()
  "A child obtains Profile identity from the shared Preset contract."
  (let ((preset
         (superchat-preset-from-plist
          '(:name coder-safe :profile-id "profile/coder-safe"
            :type agent :capability coding))))
    (should
     (equal "profile/coder-safe"
            (plist-get
             (superchat--subagent-make-context preset 1)
             :profile-id)))))

(ert-deftest test-subagent-placeholder-lifecycle ()
  "Placeholders render, then get replaced in place by the report."
  (let ((superchat-buffer-name "*subagent-ph-test*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (let ((ph1 (superchat--subagent-begin-placeholder "researcher"))
                (ph2 (superchat--subagent-begin-placeholder "executor")))
            (should (string-match-p "researcher: running" (buffer-string)))
            (should (string-match-p "executor: running" (buffer-string)))
            ;; Replace out of order: second placeholder first.
            (superchat--subagent-end-placeholder ph2 "executor" "exec done")
            (superchat--subagent-end-placeholder ph1 "researcher" "res done")
            (let ((content (buffer-string)))
              (should-not (string-match-p "running" content))
              (should (string-match-p "Sub-agent report: researcher" content))
              (should (string-match-p "res done" content))
              (should (string-match-p "Sub-agent report: executor" content))
              (should (string-match-p "exec done" content))
              ;; In-place: researcher block stays before executor block.
              (should (< (string-match "res done" content)
                         (string-match "exec done" content))))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

(ert-deftest test-subagent-end-placeholder-fallback ()
  "A nil placeholder falls back to appending the report."
  (let ((superchat-buffer-name "*subagent-ph-fallback*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create superchat-buffer-name)
          (erase-buffer)
          (superchat--subagent-end-placeholder nil "researcher" "fallback report")
          (should (string-match-p "Sub-agent report: researcher" (buffer-string)))
          (should (string-match-p "fallback report" (buffer-string))))
      (when (get-buffer superchat-buffer-name)
        (kill-buffer superchat-buffer-name)))))

;; =======================================================
;; Runtime request boundary
;; =======================================================

(ert-deftest test-subagent-runtime-request-carries-runtime-knobs ()
  "Sub-agents delegate transport policy to Runtime without recording chat.
S13a: the builder returns a Request envelope; the legacy-only knobs ride in
`:runtime-extras', the host in `:context :host-buffer', the prepared prompt
in `:context :prepared-prompt', and delivery is a callback Sink."
  (let* ((superchat-subagent-unknown-tool-retries 7)
         (preset (superchat--subagent-preset 'researcher))
         (ctx (list :session-id "subagent-1"
                    :actor-id "actor-1"
                    :profile-id "researcher"
                    :team-id "team-1"
                    :attendance 'unattended
                    :authority-policy "observer"
                    :workspace-id "workspace-1"
                    :workspace-root "/tmp/project/"
                    :preset preset))
         (delivered nil)
         (delivery (lambda (&rest args) (push args delivered)))
         (buffer (superchat-editor-workspace-buffer
                  "subagent-1" 'subagent-protocol t))
         (envelope
          (superchat--subagent-runtime-envelope
           ctx "task" "prompt"
           '(:target-model "model-x" :system-prompt "system-x")
           nil buffer delivery))
         (extras (plist-get envelope :runtime-extras))
         (context (plist-get envelope :context)))
    (should (eq 'subagent (plist-get envelope :surface)))
    (should (equal "task" (plist-get envelope :content)))
    (should (equal "subagent-1" (plist-get envelope :session-id)))
    (should (equal "research" (plist-get envelope :capability)))
    (should (eq 'subagent (plist-get envelope :topology)))
    (should (equal "observer" (plist-get envelope :authority-policy)))
    (should (equal "model-x" (plist-get envelope :target-model)))
    (should (equal "system-x" (plist-get envelope :system-prompt)))
    (should (eq 'unattended (plist-get envelope :attendance)))
    (should (eq 'none (plist-get envelope :tools)))
    (should (eq buffer (plist-get context :host-buffer)))
    (should (equal "prompt" (plist-get context :prepared-prompt)))
    (should (equal "actor-1" (plist-get extras :actor-id)))
    (should (equal "team-1" (plist-get extras :correlation-id)))
    (should (equal "workspace-1" (plist-get extras :workspace-id)))
    (should (equal "/tmp/project/" (plist-get extras :workspace-root)))
    (should (functionp (plist-get extras :tool-wrapper)))
    (should (functionp (plist-get extras :round-limit-function)))
    (should (= 7 (plist-get extras :unknown-tool-retries)))
    (should (plist-member extras :record-conversation))
    (should (eq nil (plist-get extras :record-conversation)))
    (should (eq nil (plist-get extras :persist-synthesized-tools)))
    (dolist (key '(:buffer :buffer-name :stream-function :delivery-function
                   :prompt :input :agent-mode :ephemeral-buffer :host-authority-mode))
      (should-not (plist-member envelope key)))
    ;; The Sink reproduces the legacy delivery shape.
    (let ((sink (plist-get envelope :sink)))
      (should (superchat-sink-p sink))
      (superchat-sink-emit sink 'final nil '(:text "done" :tape-id 3))
      (superchat-sink-emit sink 'error nil '(:text "boom" :status failed :tape-id 4))
      (should (equal '(("boom" failed 4) ("done" completed 3)) delivered)))
    (superchat-session-workspace-close "subagent-1")))

(ert-deftest test-subagent-child-sessions-own-distinct-protocol-hosts ()
  "Team, Artifact, parent, and delivery identity never merge child hosts."
  (let* ((preset (superchat--subagent-preset 'researcher))
         (shared (list :team-id "team" :workspace-id "artifact"
                       :parent-run-id "parent" :preset preset))
         (ctx-a (append '(:session-id "child-a" :actor-id "a") shared))
         (ctx-b (append '(:session-id "child-b" :actor-id "b") shared))
         (envelope-a (superchat--subagent-runtime-envelope
                      ctx-a "task" "prompt" nil nil nil #'ignore))
         (envelope-b (superchat--subagent-runtime-envelope
                      ctx-b "task" "prompt" nil nil nil #'ignore))
         (host-a (plist-get (plist-get envelope-a :context) :host-buffer))
         (host-b (plist-get (plist-get envelope-b :context) :host-buffer)))
    (unwind-protect
        (progn
          (should-not (eq host-a host-b))
          (should (equal "child-a"
                         (superchat-editor-workspace-owner-session host-a)))
          (should (equal "child-b"
                         (superchat-editor-workspace-owner-session host-b)))
          (dolist (envelope (list envelope-a envelope-b))
            (should (equal "parent" (plist-get envelope :parent-run-id)))
            (should-not (plist-member envelope :ephemeral-buffer))
            (should-not (plist-member envelope :host-authority-mode)))
          (should (buffer-live-p host-a))
          (should (buffer-live-p host-b)))
      (superchat-session-workspace-close "child-a")
      (superchat-session-workspace-close "child-b"))
    (should-not (buffer-live-p host-a))
    (should-not (buffer-live-p host-b))))

(ert-deftest subagent/child-envelope-carries-parent-run-id-and-callback-sink ()
  "S13a: the envelope the sub-agent submits carries `:parent-run-id', a
callback Sink whose `:final' finishes the sub-agent exactly once, and the
acquired protocol buffer as the Runtime host."
  (let* ((captured nil) (finished nil) (request nil))
    (cl-letf (((symbol-function 'superchat--execute-llm-query)
               (lambda (_turn) '(:prompt "child prompt" :system-prompt "system" :tools none)))
              ((symbol-function 'superchat--subagent-prepare-tools)
               (lambda (&rest _) nil))
              ((symbol-function 'superchat--team-start-delegated-task)
               (lambda (&rest _) nil))
              ((symbol-function 'superchat--team-finish-delegated-task) #'ignore)
              ((symbol-function 'superchat--subagent-refresh-panel) #'ignore)
              ((symbol-function 'superchat--update-status) #'ignore)
              ((symbol-function 'superchat-subagent-handoff-decision)
               (lambda (&rest _) '(:action deliver)))
              ((symbol-function 'superchat-runtime-submit)
               (lambda (req)
                 (setq request req)
                 '(:run-id "run-child" :turn-id "turn-child" :session-id "child"))))
      (superchat-stage-add 'collect-context 'test-envelope-capture
                           (lambda (env _state _tools) (setq captured env) nil)
                           :owner 'test)
      (unwind-protect
          (let ((superchat--tool-execution-context (list :run-id "run-parent")))
            (superchat--subagent-run-async 'researcher "research" nil
                                           (lambda (value) (push value finished)))
            ;; The prepared prompt bypasses collect-context; observe through
            ;; the legacy Request the pipeline built instead.
            (should request)
            (should (equal "run-parent" (plist-get request :parent-run-id)))
            (should (equal "child prompt" (plist-get request :prompt)))
            (should (equal "research" (plist-get request :input)))
            (should (superchat-sink-p (plist-get request :sink)))
            (should (eq 'subagent (plist-get request :surface)))
            ;; The Runtime host is the acquired protocol buffer owned by the
            ;; child Session, not the pipeline's hidden fallback buffer.
            (should (equal (plist-get request :session-id)
                           (superchat-editor-workspace-owner-session
                            (plist-get request :buffer))))
            (subagent-test--deliver request "child report" 'completed 1)
            (subagent-test--deliver request "child report again" 'completed 2)
            (should (equal '("child report") finished)))
        (superchat-stage-remove 'collect-context 'test-envelope-capture)
        (ignore captured)))))

;; =======================================================
;; superchat-subagent-map — RLM-style batch primitive
;; =======================================================

(ert-deftest test-subagent-map-returns-reports-in-chunk-order ()
  "Reports come back in chunk order even when completion order differs."
  (let ((launched nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (_preset _task context callback &optional _depth)
                 ;; Answer even chunks immediately, odd ones later via
                 ;; the stored callbacks — out of order on purpose.
                 (push (cons context callback) launched)
                 context)))
      (cl-letf (((symbol-function 'superchat--subagent-map-pump)
                 (lambda ()
                   ;; Pump: flush all pending callbacks, order reversed.
                   (dolist (pair launched)
                     (funcall (cdr pair) (format "report:%s" (car pair))))
                   (setq launched nil)
                   t)))
        (should (equal '("report:a" "report:b" "report:c")
                       (superchat-subagent-map '("a" "b" "c") "label them")))))))

(ert-deftest test-subagent-map-times-out-and-cancels ()
  "Unanswered chunks yield error strings and get cancelled."
  (let ((cancelled nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (_preset _task context _callback &optional _depth)
                 (format "id-%s" context)))       ; never calls back
              ((symbol-function 'superchat-subagent-cancel)
               (lambda (id &optional _reason) (push id cancelled) t))
              ((symbol-function 'superchat--subagent-map-pump)
               (lambda () t)))
      (let ((reports (superchat-subagent-map '("x" "y") "q" 'researcher 0)))
        (should (equal '("[Error: superchat-subagent-map timed out]"
                         "[Error: superchat-subagent-map timed out]")
                       reports))
        (should (member "id-x" cancelled))
        (should (member "id-y" cancelled))))))

(ert-deftest test-subagent-map-timeout-does-not-refill-queue ()
  "After a timeout, cancelling an active chunk must not pull queued
chunks in just to cancel them."
  (let ((superchat-subagent-parallel-max 1)
        (started 0)
        (cbs nil))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (_p _q ctx cb &rest _)
                 (cl-incf started)
                 (push (cons ctx cb) cbs)
                 (format "id-%s" ctx)))
              ((symbol-function 'superchat-subagent-cancel)
               (lambda (id &optional _r)
                 ;; A real cancel finishes the run, firing its callback.
                 (let ((pair (cl-find id cbs :test #'equal
                                      :key (lambda (p) (format "id-%s" (car p))))))
                   (when pair (funcall (cdr pair) "[cancelled]")))
                 t))
              ((symbol-function 'superchat--subagent-map-pump) (lambda () t))
              ((symbol-function 'superchat--extend-timeout) #'ignore))
      ;; window 1, timeout 0: only the first of three chunks ever starts.
      (superchat-subagent-map '("a" "b" "c") "q" 'researcher 0)
      (should (= started 1)))))

(ert-deftest test-subagent-map-empty-chunks ()
  "No chunks, no calls, empty result."
  (cl-letf (((symbol-function 'superchat--subagent-run-async)
             (lambda (&rest _) (error "must not be called"))))
    (should (null (superchat-subagent-map '() "q")))))

(ert-deftest runtime/team-member-event-rejects-unknown-suffix ()
  "The dynamic `subagent.%s' writer rejects a suffix outside the allowed
set, so it cannot silently emit a type Closure does not know about."
  (should-error
   (superchat--team-member-event '(:team-id "sc-gate-probe-team") "cancelled")))

(provide 'test-subagent)

;;; test-subagent.el ends here
