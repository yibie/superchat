;;; run.el --- DeepSeek Experience Cohort runner -*- lexical-binding: t; -*-

;;; Commentary:

;; Dedicated experiment runner.  It reuses the frozen Cohort v0 Research and
;; Agent Reach surfaces, but writes every new fact under a new experiment and
;; Runtime data directory.  Old Cohort Runs are never resumed or imported.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'llm-deepseek)

(load "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-v0/run-batch.el"
      nil nil t)

(defconst superchat-experience-cohort-deepseek-v6--directory
  "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-deepseek-v6/")

(defconst superchat-experience-cohort-deepseek-v6--root
  "/private/tmp/superchat-experience-cohort-deepseek-v6/")

(defconst superchat-experience-cohort-deepseek-v6--allowed-tools
  '("agent_reach_search" "agent_reach_read"
    "workspace_read" "workspace_publish" "workspace_info"
    "team_task" "team_message"))

(defconst superchat-experience-cohort-deepseek-v6--manifest-path
  (expand-file-name "manifest.json" superchat-experience-cohort-deepseek-v6--directory))

(defconst superchat-experience-cohort-deepseek-v6--assignments-path
  (expand-file-name "assignments.json" superchat-experience-cohort-deepseek-v6--directory))

(defconst superchat-experience-cohort-deepseek-v6--launch-record-path
  (expand-file-name "launch-record.json" superchat-experience-cohort-deepseek-v6--directory))

(defconst superchat-experience-cohort-deepseek-v6--prompt-record-path
  (expand-file-name "prompt-snapshots.json" superchat-experience-cohort-deepseek-v6--directory))

(defconst superchat-experience-cohort-deepseek-v6--deliverable-guard-path
  (expand-file-name "deliverable-guard-v2.txt"
                    superchat-experience-cohort-deepseek-v6--directory))

(defvar superchat-experience-cohort-deepseek-v6--launches nil)
(defvar superchat-experience-cohort-deepseek-v6--prompt-snapshots nil)

(setq superchat-data-directory
      (expand-file-name "runtime/" superchat-experience-cohort-deepseek-v6--root)
      superchat-llm-backend
      (make-llm-deepseek
       :key (lambda () (getenv "DEEPSEEK_API_KEY"))
       :chat-model "deepseek-v4-pro")
      superchat-llm-model nil
      superchat-llm-reasoning 'none
      llm-request-plz-connect-timeout 30
      plz-curl-program "/opt/local/bin/curl"
      plz-curl-default-args
      '("--silent" "--compressed" "--location"
        "--proxy" "http://127.0.0.1:7897"
        "--noproxy" "localhost,127.0.0.1"
        "--suppress-connect-headers"
        "--retry" "2" "--retry-all-errors"
        "--retry-delay" "1" "--retry-max-time" "90")
      superchat-response-timeout 7200
      superchat-subagent-timeout 21600)

(defun superchat-experience-cohort-deepseek-v6--read-json (path)
  "Read PATH as a plist-backed JSON object."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object json-false)))

(defun superchat-experience-cohort-deepseek-v6--write-json (path value)
  "Write VALUE as JSON to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (json-encode value))
    (insert "\n")))

(defun superchat-experience-cohort-deepseek-v6--read-file (path)
  "Return PATH contents without interpretation."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun superchat-experience-cohort-deepseek-v6--sha256-file (path)
  "Return protocol-prefixed SHA-256 for PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (concat "sha256:" (secure-hash 'sha256 (current-buffer)))))

(defun superchat-experience-cohort-deepseek-v6--sha256-string (value)
  "Return protocol-prefixed SHA-256 for VALUE."
  (concat "sha256:" (secure-hash 'sha256 (or value ""))))

(defun superchat-experience-cohort-deepseek-v6--harden-stage-prompt (prompt)
  "Append the frozen deliverable guard to Research stage PROMPT."
  (concat prompt "\n\n"
          (string-trim-right
           (superchat-experience-cohort-deepseek-v6--read-file
            superchat-experience-cohort-deepseek-v6--deliverable-guard-path))))

(defun superchat-experience-cohort-deepseek-v6--manifest ()
  "Return the frozen DeepSeek Experience Cohort Manifest."
  (superchat-experience-cohort-deepseek-v6--read-json
   superchat-experience-cohort-deepseek-v6--manifest-path))

(defun superchat-experience-cohort-deepseek-v6--assignments ()
  "Return the twenty frozen Cohort assignments."
  (plist-get
   (superchat-experience-cohort-deepseek-v6--read-json
    superchat-experience-cohort-deepseek-v6--assignments-path)
   :assignments))

(defun superchat-experience-cohort-deepseek-v6--verify-inputs ()
  "Fail unless every frozen experiment input still matches its hash."
  (let* ((manifest (superchat-experience-cohort-deepseek-v6--manifest))
         (assignments (superchat-experience-cohort-deepseek-v6--assignments)))
    (unless (equal "cohort-authorized" (plist-get manifest :status))
      (error "DeepSeek Experience Cohort is not authorized"))
    (unless (and (= 20 (length assignments))
                 (= 4 (cl-count "calibration" assignments
                                :key (lambda (item) (plist-get item :split))
                                :test #'equal))
                 (= 12 (cl-count "discovery" assignments
                                 :key (lambda (item) (plist-get item :split))
                                 :test #'equal))
                 (= 4 (cl-count "replicate" assignments
                                :key (lambda (item) (plist-get item :split))
                                :test #'equal))
                 (equal (number-sequence 1 20)
                        (mapcar (lambda (item) (plist-get item :run_index))
                                assignments)))
      (error "DeepSeek Experience Cohort assignments are invalid"))
    (unless (and (= 30 llm-request-plz-connect-timeout)
                 (equal "/opt/local/bin/curl" plz-curl-program)
                 (equal '("--silent" "--compressed" "--location"
                          "--proxy" "http://127.0.0.1:7897"
                          "--noproxy" "localhost,127.0.0.1"
                          "--suppress-connect-headers"
                          "--retry" "2" "--retry-all-errors"
                          "--retry-delay" "1" "--retry-max-time" "90")
                        plz-curl-default-args)
                 (equal superchat-experience-cohort-deepseek-v6--allowed-tools
                        (plist-get manifest :model_tool_surface)))
      (error "DeepSeek Experience Cohort execution surface changed"))
    (dolist (input (plist-get manifest :frozen_inputs))
      (let* ((path (expand-file-name
                    (plist-get input :path)
                    "/Users/chenyibin/Documents/emacs/package/superchat/"))
             (actual (and (file-readable-p path)
                          (superchat-experience-cohort-deepseek-v6--sha256-file path))))
        (unless (equal actual (plist-get input :sha256))
          (error "Frozen input changed or missing: %s" path))))
    manifest))

(defun superchat-experience-cohort-deepseek-v6--role (prompt)
  "Return the Research role encoded in PROMPT."
  (when (string-match "\\[research/\\(collector\\|analyst\\|reviewer\\)\\]"
                      (or prompt ""))
    (match-string 1 prompt)))

(defun superchat-experience-cohort-deepseek-v6--assignment-id (prompt)
  "Return the frozen assignment identity encoded in PROMPT."
  (when (string-match "assignment \\([^;\n]+\\);" (or prompt ""))
    (match-string 1 prompt)))

(defun superchat-experience-cohort-deepseek-v6--tool-names (tools)
  "Normalize model-facing TOOLS to ordered names."
  (mapcar (lambda (tool)
            (cond ((stringp tool) tool)
                  ((symbolp tool) (symbol-name tool))
                  ((listp tool) (plist-get tool :name))
                  (t (format "%s" tool))))
          tools))

(defun superchat-experience-cohort-deepseek-v6--write-prompts ()
  "Persist observed model-facing prompt snapshots."
  (superchat-experience-cohort-deepseek-v6--write-json
   superchat-experience-cohort-deepseek-v6--prompt-record-path
   (list :experiment_id "experience-cohort-deepseek-v6"
         :snapshot_count
         (length superchat-experience-cohort-deepseek-v6--prompt-snapshots)
         :snapshots
         (vconcat (reverse superchat-experience-cohort-deepseek-v6--prompt-snapshots)))))

(defun superchat-experience-cohort-deepseek-v6--capture-query (original turn &rest args)
  "Call ORIGINAL with TURN and ARGS, retaining the actual model-facing query."
  (let* ((result (apply original turn args))
         (prompt (plist-get result :prompt))
         (role (superchat-experience-cohort-deepseek-v6--role prompt))
         (assignment-id
          (superchat-experience-cohort-deepseek-v6--assignment-id prompt)))
    (when (and role assignment-id)
      (let ((snapshot
             (list :assignment_id assignment-id
                   :role role
                   :actor_id (and (boundp 'superchat--session-id)
                                  superchat--session-id)
                   :captured_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                   :model_prompt prompt
                   :model_prompt_sha256
                   (superchat-experience-cohort-deepseek-v6--sha256-string prompt)
                   :system_prompt (plist-get result :system-prompt)
                   :system_prompt_sha256
                   (superchat-experience-cohort-deepseek-v6--sha256-string
                    (plist-get result :system-prompt))
                   :target_model
                   (or (plist-get result :target-model) "deepseek-v4-pro")
                   :provider
                   (superchat--provider-name superchat-llm-backend)
                   :reasoning_mode (symbol-name superchat-llm-reasoning)
                   :connect_timeout_seconds llm-request-plz-connect-timeout
                   :prepared_tool_names
                   (vconcat
                    (superchat-experience-cohort-deepseek-v6--tool-names
                     (plist-get result :tools))))))
        (setq superchat-experience-cohort-deepseek-v6--prompt-snapshots
              (cons snapshot
                    (cl-remove-if
                     (lambda (old)
                       (and (equal assignment-id
                                   (plist-get old :assignment_id))
                            (equal role (plist-get old :role))))
                     superchat-experience-cohort-deepseek-v6--prompt-snapshots)))
        (superchat-experience-cohort-deepseek-v6--write-prompts)))
    result))

(defun superchat-experience-cohort-deepseek-v6--load-records ()
  "Restore experiment-only launch and prompt records after daemon restart."
  (when (file-readable-p superchat-experience-cohort-deepseek-v6--launch-record-path)
    (setq superchat-experience-cohort-deepseek-v6--launches
          (reverse
           (plist-get
            (superchat-experience-cohort-deepseek-v6--read-json
             superchat-experience-cohort-deepseek-v6--launch-record-path)
            :launches))))
  (when (file-readable-p superchat-experience-cohort-deepseek-v6--prompt-record-path)
    (setq superchat-experience-cohort-deepseek-v6--prompt-snapshots
          (reverse
           (plist-get
            (superchat-experience-cohort-deepseek-v6--read-json
             superchat-experience-cohort-deepseek-v6--prompt-record-path)
            :snapshots)))))

(defun superchat-experience-cohort-deepseek-v6--write-launches ()
  "Persist all new DeepSeek Experience Cohort launch identities."
  (superchat-experience-cohort-deepseek-v6--write-json
   superchat-experience-cohort-deepseek-v6--launch-record-path
   (list :experiment_id "experience-cohort-deepseek-v6"
         :manifest_sha256
         (superchat-experience-cohort-deepseek-v6--sha256-file
          superchat-experience-cohort-deepseek-v6--manifest-path)
         :status "runs-started"
         :launch_count (length superchat-experience-cohort-deepseek-v6--launches)
         :launches
         (vconcat (reverse superchat-experience-cohort-deepseek-v6--launches))
         :historical_runs_imported json-false
         :held_out_used json-false
         :effectiveness_claim "not_evaluated"
         :learning_claim "prohibited")))

(defun superchat-experience-cohort-deepseek-v6--assignment (run-index)
  "Return frozen assignment RUN-INDEX."
  (or (cl-find run-index
               (superchat-experience-cohort-deepseek-v6--assignments)
               :key (lambda (assignment) (plist-get assignment :run_index)))
      (error "Unknown DeepSeek Experience Cohort index: %s" run-index)))

(defun superchat-experience-cohort-deepseek-v6--preset ()
  "Return the frozen DeepSeek Research preset."
  (superchat--subagent-make-preset
   "researcher" "researcher"
   "DeepSeek Research Cohort with Agent Reach and durable delivery"
   'agent "research"
   superchat-experience-cohort-deepseek-v6--allowed-tools
   (concat
    "You are one stage of a frozen Research Workflow. Use agent_reach_search "
    "to locate primary research or official specifications, use "
    "agent_reach_read to inspect material sources, distinguish evidence from "
    "inference, retain URLs, inspect prior delivered Artifacts, and publish "
    "exactly one complete durable Artifact with workspace_publish. Do not "
    "finish or merely narrate a future action before publication succeeds.")))

(defun superchat-experience-cohort-deepseek-v6--permission (tool-name _args)
  "Allow only the frozen Cohort TOOL-NAME surface."
  (if (member tool-name superchat-experience-cohort-deepseek-v6--allowed-tools)
      'allow
    'deny))

(defun superchat-experience-cohort-deepseek-v6--configure-surface ()
  "Install the frozen Agent Reach and Research surfaces."
  (superchat-experience-cohort-v0--register-agent-reach-tools)
  (setcdr (assoc "researcher" superchat--subagent-builtin-presets)
          #'superchat-experience-cohort-deepseek-v6--preset)
  (setq superchat-agent-permission-functions
        '(superchat-experience-cohort-deepseek-v6--permission)))

(defun superchat-experience-cohort-deepseek-v6--query (assignment)
  "Return the frozen model-facing query for ASSIGNMENT."
  (format
   (concat "[Experience Cohort DeepSeek v6; assignment %s; split %s; family %s] %s\n\n"
           "Use primary research or official specifications. Search with "
           "agent_reach_search, inspect material sources with agent_reach_read, "
           "record inaccessible or contradictory sources, distinguish evidence "
           "from inference, and retain source URLs in the Artifact.")
   (plist-get assignment :assignment_id)
   (plist-get assignment :split)
   (plist-get assignment :family)
   (plist-get assignment :question)))

(defun superchat-experience-cohort-deepseek-v6--launch (run-index)
  "Return persisted launch RUN-INDEX, or nil."
  (cl-find run-index superchat-experience-cohort-deepseek-v6--launches
           :key (lambda (launch) (plist-get launch :run_index))))

(defun superchat-experience-cohort-deepseek-v6--settled-p (launch)
  "Return non-nil when LAUNCH cannot advance without a new decision."
  (let* ((view (superchat-research-workflow-view
                (plist-get launch :instance_id)))
         (steps (plist-get view :steps)))
    (or (eq 'completed (plist-get view :status))
        (cl-some (lambda (step)
                   (memq (plist-get step :resume-decision)
                         '(manual-review stop)))
                 steps))))

(defun superchat-experience-cohort-deepseek-v6-start (run-index)
  "Start one new frozen Cohort RUN-INDEX."
  (interactive "nCohort index (1-20): ")
  (superchat-experience-cohort-deepseek-v6--verify-inputs)
  (when (superchat-experience-cohort-deepseek-v6--launch run-index)
    (error "Cohort index already started: %s" run-index))
  (when-let* ((previous (car superchat-experience-cohort-deepseek-v6--launches)))
    (unless (superchat-experience-cohort-deepseek-v6--settled-p previous)
      (error "Previous Cohort index is still running: %s"
             (plist-get previous :run_index))))
  (let* ((assignment (superchat-experience-cohort-deepseek-v6--assignment run-index))
         (default-directory superchat-experience-cohort-deepseek-v6--root)
         (superchat--active-authority-policy "default")
         (instance-id
          (superchat-research-workflow
           (superchat-experience-cohort-deepseek-v6--query assignment)))
         (view (superchat-research-workflow-view instance-id))
         (collector (car (plist-get view :steps)))
         (launch
          (list :run_index run-index
                :assignment_id (plist-get assignment :assignment_id)
                :source_task_id (plist-get assignment :source_task_id)
                :split (plist-get assignment :split)
                :family (plist-get assignment :family)
                :replicate_of (plist-get assignment :replicate_of)
                :instance_id instance-id
                :actor_id (plist-get collector :actor-id)
                :run_id (plist-get collector :run-id)
                :started_at (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (push launch superchat-experience-cohort-deepseek-v6--launches)
    (superchat-experience-cohort-deepseek-v6--write-launches)
    launch))

(defun superchat-experience-cohort-deepseek-v6-status ()
  "Return current Workflow views for all launched Cohort tasks."
  (interactive)
  (mapcar
   (lambda (launch)
     (list :run_index (plist-get launch :run_index)
           :assignment_id (plist-get launch :assignment_id)
           :view
           (superchat-research-workflow-view
            (plist-get launch :instance_id))))
   (reverse superchat-experience-cohort-deepseek-v6--launches)))

(make-directory superchat-experience-cohort-deepseek-v6--root t)
(superchat-experience-cohort-deepseek-v6--verify-inputs)
(superchat-experience-cohort-deepseek-v6--load-records)
(superchat-experience-cohort-deepseek-v6--configure-surface)
(unless (advice-member-p
         #'superchat-experience-cohort-deepseek-v6--harden-stage-prompt
         'superchat-research-workflow--stage-prompt)
  (advice-add 'superchat-research-workflow--stage-prompt :filter-return
              #'superchat-experience-cohort-deepseek-v6--harden-stage-prompt))
(unless (advice-member-p #'superchat-experience-cohort-deepseek-v6--capture-query
                         'superchat--execute-llm-query)
  (advice-add 'superchat--execute-llm-query :around
              #'superchat-experience-cohort-deepseek-v6--capture-query))

(provide 'superchat-experience-cohort-deepseek-v6-run)

;;; run.el ends here
