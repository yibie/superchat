;;; test-subagent.el --- Tests for superchat-subagent -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit tests for sub-agent presets, runner, and delegation tool.

;;; Code:

(require 'ert)
(require 'superchat-agent-loop)
(require 'superchat-dispatcher)
(require 'superchat-llm)
(require 'superchat-subagent)
(require 'superchat)

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
    (let ((superchat--subagent-running nil)
          (superchat-runtime--runs (make-hash-table :test #'equal))
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (superchat-subagent-timeout nil)
          (superchat-closure-terminal-guard-enabled t)
          (superchat--tool-execution-context
           '(:run-id "parent-run" :attendance attended
             :workspace-id "research-workspace"))
          (superchat--team-id "parent-session")
          child-id report artifact-reference)
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
              :action)))))))

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
                 (funcall (plist-get submitted :delivery-function)
                          reason 'cancelled 9)
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
                 (setq delivery (plist-get request :delivery-function))
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
                 (funcall (plist-get request :delivery-function)
                          "done" 'completed 1)
                 '(:turn-id "turn-sync" :run-id "run-sync"
                   :session-id "session-sync"))))
      (superchat--subagent-run-async
       'researcher "task" nil (lambda (value) (setq report value)))
      (setq events (nreverse events))
      (should (equal "done" report))
      (should (equal '("started" "completed") (mapcar #'car events)))
      (dolist (item events)
        (should (equal "run-sync" (plist-get (cadr item) :run-id)))))))

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
                 (funcall (plist-get submitted :delivery-function)
                          reason 'cancelled 9)
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
                 (funcall (plist-get request :delivery-function)
                          "async report" 'completed 9)
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
                 (funcall (plist-get request :delivery-function)
                          "ok" 'completed 9)
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

(ert-deftest test-subagent-depth-guard-blocks-nested-delegation ()
  "At the depth limit, delegate tools degrade to a denial stub."
  (skip-unless (fboundp 'llm-make-tool))
  (let* ((superchat-subagent-max-depth 1)
         (preset (superchat--subagent-preset 'researcher))
         (ctx (superchat--subagent-make-context preset 1))
         (tool (llm-make-tool :name "delegate_to_subagent"
                              :description "delegate"
                              :args nil
                              :async t
                              :function #'ignore))
         (wrapped (superchat--subagent-wrap-tool ctx tool)))
    (should-not (llm-tool-async wrapped))
    (should (string-match-p "depth limit"
                            (funcall (llm-tool-function wrapped)
                                     "researcher" "nested task")))))

(ert-deftest test-subagent-depth-guard-allows-below-limit ()
  "Below the depth limit, nested delegation re-enters at depth + 1."
  (skip-unless (fboundp 'llm-make-tool))
  (let* ((superchat-subagent-max-depth 2)
         (superchat-authority-approval-function (lambda (_request) t))
         (preset (superchat--subagent-preset 'researcher))
         (ctx (superchat--subagent-make-context preset 1))
         (tool (llm-make-tool :name "delegate_to_subagent"
                              :description "delegate"
                              :args nil
                              :async t
                              :function #'ignore))
         (wrapped (superchat--subagent-wrap-tool ctx tool))
         (captured-depth nil)
         (result nil))
    (should (llm-tool-async wrapped))
    (cl-letf (((symbol-function 'superchat--subagent-run-async)
               (lambda (_preset _task _context callback &optional depth)
                 (setq captured-depth depth)
                 (funcall callback "nested report"))))
      (funcall (llm-tool-function wrapped)
               (lambda (r) (setq result r))
               "researcher" "nested task"))
    (should (= 2 captured-depth))
    (should (string= result "nested report"))))

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
  "Sub-agents delegate transport policy to Runtime without recording chat."
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
         (delivery #'ignore)
         (request
          (superchat--subagent-runtime-request
           ctx "task" "prompt"
           '(:target-model "model-x" :system-prompt "system-x")
           '(prepared-tool) " *subagent-buffer*" delivery)))
    (should (equal "subagent-1" (plist-get request :session-id)))
    (should (equal "actor-1" (plist-get request :actor-id)))
    (should (equal "team-1" (plist-get request :correlation-id)))
    (should (equal "research" (plist-get request :capability)))
    (should (eq 'subagent (plist-get request :topology)))
    (should (equal "observer" (plist-get request :authority-policy)))
    (should (equal "workspace-1" (plist-get request :workspace-id)))
    (should (equal "/tmp/project/" (plist-get request :workspace-root)))
    (should (eq 'subagent (plist-get request :surface)))
    (should (equal "model-x" (plist-get request :target-model)))
    (should (equal "system-x" (plist-get request :system-prompt)))
    (should (equal '(prepared-tool) (plist-get request :prepared-tools)))
    (should (eq delivery (plist-get request :delivery-function)))
    (should (functionp (plist-get request :tool-wrapper)))
    (should (functionp (plist-get request :round-limit-function)))
    (should (= 7 (plist-get request :unknown-tool-retries)))
    (should (eq nil (plist-get request :persist-synthesized-tools)))
    (should (eq nil (plist-get request :record-conversation)))
    (should-not (plist-member request :agent-mode))
    (should (eq t (plist-get request :ephemeral-buffer)))))

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

(provide 'test-subagent)

;;; test-subagent.el ends here
