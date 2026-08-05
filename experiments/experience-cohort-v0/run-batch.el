;;; run-batch.el --- Launch Cohort v0 Runs 002-020 -*- lexical-binding: t; -*-

;;; Commentary:

;; Experiment-only launcher for the dedicated `superchat-cohort-v0' daemon.
;; Web search uses Agent Reach's Exa backend; page reading uses Jina Reader.
;; Held-out tasks are never loaded by this file.

;;; Code:

(require 'json)
(require 'subr-x)
(unless (featurep 'superchat-experience-cohort-v0-calibration)
  (load "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-v0/run-calibration.el"
        nil nil t))
(load "/Users/chenyibin/Documents/emacs/package/superchat/superchat-subagent.el"
      nil nil t)

(defconst superchat-experience-cohort-v0--assignments-path
  (expand-file-name "batch-assignments-v0.json"
                    superchat-experience-cohort-v0--experiment-directory))

(defconst superchat-experience-cohort-v0--batch-manifest-path
  (expand-file-name "manifest-v2.json"
                    superchat-experience-cohort-v0--experiment-directory))

(defconst superchat-experience-cohort-v0--batch-allowed-tools
  '("agent_reach_search" "agent_reach_read"
    "workspace_read" "workspace_publish" "workspace_info"
    "team_task" "team_message"))

(defvar superchat-experience-cohort-v0-batch-launches nil)

(defun superchat-experience-cohort-v0--verify-batch-inputs ()
  "Fail unless the batch Manifest, assignments and runner still match."
  (let* ((manifest
          (superchat-experience-cohort-v0--read-json
           superchat-experience-cohort-v0--batch-manifest-path))
         (assignments-hash
          (plist-get (plist-get manifest :launch_target)
                     :assignments_sha256))
         (runner (plist-get manifest :execution)))
    (unless (equal "batch-authorized" (plist-get manifest :status))
      (error "Cohort v0 batch manifest is not authorized"))
    (unless (equal assignments-hash
                   (superchat-experience-cohort-v0--sha256-file
                    superchat-experience-cohort-v0--assignments-path))
      (error "Batch assignments changed after freeze"))
    (unless (equal (plist-get runner :runner_sha256)
                   (superchat-experience-cohort-v0--sha256-file
                    (expand-file-name (plist-get runner :runner_path)
                                      "/Users/chenyibin/Documents/emacs/package/superchat/")))
      (error "Batch runner changed after freeze"))))

(defun superchat-experience-cohort-v0--bounded-process
    (program args max-characters)
  "Run PROGRAM with ARGS and return at most MAX-CHARACTERS."
  (with-temp-buffer
    (let ((status (apply #'call-process
                         "/opt/homebrew/bin/gtimeout" nil t nil
                         "90s" program args)))
      (unless (and (integerp status) (zerop status))
        (error "%s failed with status %s: %s"
               program status (string-trim (buffer-string))))
      (let ((text (buffer-string)))
        (if (> (length text) max-characters)
            (concat (substring text 0 max-characters)
                    "\n[Result truncated by frozen tool limit]")
          text)))))

(defun superchat-experience-cohort-v0-agent-reach-search
    (query &optional num-results)
  "Search the web for QUERY through Agent Reach Exa."
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (error "Search query must be non-empty"))
  (let* ((count (max 1 (min 8 (truncate (or num-results 5)))))
         (call (format "exa.web_search_exa(query: %s, numResults: %d)"
                       (json-encode-string query) count)))
    (superchat-experience-cohort-v0--bounded-process
     "/Users/chenyibin/.bun/bin/mcporter" (list "call" call) 30000)))

(defun superchat-experience-cohort-v0-agent-reach-read (url)
  "Read HTTP(S) URL through Agent Reach Jina Reader."
  (unless (and (stringp url)
               (string-match-p "\\`https?://" url))
    (error "Reader URL must use HTTP or HTTPS"))
  (superchat-experience-cohort-v0--bounded-process
   "/opt/local/bin/curl"
   (list "--silent" "--show-error" "--location" "--max-time" "80"
         (concat "https://r.jina.ai/" url))
   40000))

(defun superchat-experience-cohort-v0--external-read-effects (args)
  "Return the frozen external-read effects for ARGS."
  (list (list :effect 'external-read :resource 'web :scope (car args))
        (list :effect 'process-exec :resource 'agent-reach
              :scope "frozen-read-only-adapter")))

(defun superchat-experience-cohort-v0--register-agent-reach-tools ()
  "Register the frozen model-facing Agent Reach tools."
  (superchat-register-tool
   "agent_reach_search"
   :function #'superchat-experience-cohort-v0-agent-reach-search
   :description
   "Search the public web through Agent Reach Exa. Prefer primary research and official specifications; retain source URLs."
   :args (list (list :name "query" :type 'string
                     :description "Precise web search query.")
               (list :name "num-results" :type 'number :optional t
                     :description "Number of results, from 1 to 8."))
   :effects #'superchat-experience-cohort-v0--external-read-effects)
  (superchat-register-tool
   "agent_reach_read"
   :function #'superchat-experience-cohort-v0-agent-reach-read
   :description
   "Read one public HTTP(S) page through Agent Reach Jina Reader and return bounded Markdown."
   :args (list (list :name "url" :type 'string
                     :description "Public HTTP(S) URL to read."))
   :effects #'superchat-experience-cohort-v0--external-read-effects))

(defun superchat-experience-cohort-v0--batch-researcher-preset ()
  "Return the frozen Cohort v0 Researcher preset."
  (superchat--subagent-make-preset
   "researcher" "researcher"
   "Research sub-agent with Agent Reach and durable Workspace delivery"
   'agent "research"
   superchat-experience-cohort-v0--batch-allowed-tools
   (concat
    "You are a research sub-agent in Experience Cohort v0. Use "
    "agent_reach_search to locate primary research or official specifications, "
    "agent_reach_read to inspect material sources, distinguish evidence from "
    "inference, retain URLs, inspect prior delivered Artifacts, and publish "
    "exactly one durable deliverable with workspace_publish.")))

(defun superchat-experience-cohort-v0--batch-permission (tool-name _args)
  "Allow only the frozen batch TOOL-NAME surface."
  (if (member tool-name superchat-experience-cohort-v0--batch-allowed-tools)
      'allow
    'deny))

(defun superchat-experience-cohort-v0--configure-batch-surface ()
  "Install the frozen batch surface in the dedicated daemon."
  (superchat-experience-cohort-v0--register-agent-reach-tools)
  (setcdr (assoc "researcher" superchat--subagent-builtin-presets)
          #'superchat-experience-cohort-v0--batch-researcher-preset)
  (setq superchat-agent-permission-functions
        '(superchat-experience-cohort-v0--batch-permission)
        superchat-response-timeout 7200
        superchat-subagent-timeout 21600))

(defun superchat-experience-cohort-v0--batch-assignments ()
  "Return frozen non-held-out batch assignments."
  (plist-get
   (superchat-experience-cohort-v0--read-json
    superchat-experience-cohort-v0--assignments-path)
   :assignments))

(defun superchat-experience-cohort-v0--batch-query (assignment)
  "Return model-facing query for ASSIGNMENT."
  (format
   (concat "[Experience Cohort v0; assignment %s; split %s; family %s] %s\n\n"
           "Use primary research or official specifications. Search with "
           "agent_reach_search, inspect material sources with agent_reach_read, "
           "record inaccessible or contradictory sources, distinguish evidence "
           "from inference, and retain source URLs in the Artifact.")
   (plist-get assignment :assignment_id)
   (plist-get assignment :split)
   (plist-get assignment :family)
   (plist-get assignment :question)))

(defun superchat-experience-cohort-v0--write-batch-launches ()
  "Persist all successful batch launch identities."
  (let ((path (expand-file-name "batch-launch-record.json"
                                superchat-experience-cohort-v0--experiment-directory)))
    (with-temp-file path
      (insert
       (json-encode
        (list :experiment_id "experience-cohort-v0-20260803"
              :manifest_revision "v2"
              :manifest_sha256
              (superchat-experience-cohort-v0--sha256-file
               superchat-experience-cohort-v0--batch-manifest-path)
              :status "runs.started"
              :existing_launch_record "launch-record.json"
              :new_launch_count (length superchat-experience-cohort-v0-batch-launches)
              :target_total_workflow_starts 20
              :launches
              (vconcat (reverse superchat-experience-cohort-v0-batch-launches))
              :effectiveness_claim "not_evaluated"
              :learning_claim "prohibited")))
      (insert "\n"))))

(defun superchat-experience-cohort-v0-start-assignment (run-index)
  "Start frozen assignment RUN-INDEX and persist its identities."
  (when (cl-find run-index superchat-experience-cohort-v0-batch-launches
                 :key (lambda (entry) (plist-get entry :run_index)))
    (error "Run index already started: %s" run-index))
  (let* ((assignment
          (cl-find run-index
                   (superchat-experience-cohort-v0--batch-assignments)
                   :key (lambda (entry) (plist-get entry :run_index))))
         (_ (unless assignment (error "Unknown run index: %s" run-index)))
         (default-directory superchat-experience-cohort-v0--root)
         (superchat--active-authority-policy "default")
         (instance-id
          (superchat-research-workflow
           (superchat-experience-cohort-v0--batch-query assignment)))
         (view (superchat-research-workflow-view instance-id))
         (collector (car (plist-get view :steps)))
         (launch
          (list :run_index run-index
                :assignment_id (plist-get assignment :assignment_id)
                :source_task_id (plist-get assignment :source_task_id)
                :split (plist-get assignment :split)
                :family (plist-get assignment :family)
                :instance_id instance-id
                :actor_id (plist-get collector :actor-id)
                :run_id (plist-get collector :run-id)
                :started_at (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
    (push launch superchat-experience-cohort-v0-batch-launches)
    (superchat-experience-cohort-v0--write-batch-launches)
    launch))

(defun superchat-experience-cohort-v0-start-remaining ()
  "Start every not-yet-launched frozen assignment."
  (mapcar
   (lambda (assignment)
     (let ((run-index (plist-get assignment :run_index)))
       (unless (cl-find run-index superchat-experience-cohort-v0-batch-launches
                        :key (lambda (entry) (plist-get entry :run_index)))
         (superchat-experience-cohort-v0-start-assignment run-index))))
   (superchat-experience-cohort-v0--batch-assignments)))

(superchat-experience-cohort-v0--verify-batch-inputs)
(superchat-experience-cohort-v0--configure-batch-surface)

(provide 'superchat-experience-cohort-v0-batch)

;;; run-batch.el ends here
