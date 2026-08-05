;;; run-calibration.el --- Start the first real Cohort v0 calibration -*- lexical-binding: t; -*-

;;; Commentary:

;; Load this file only in the dedicated `superchat-cohort-v0' Emacs daemon.
;; It starts one real Research Workflow task and writes launch evidence after
;; the collector Run has a durable identity.  It makes no effectiveness or
;; learning claim.

;;; Code:

(require 'json)
(require 'subr-x)
(require 'superchat)
(require 'superchat-workflow)
(require 'llm-ollama)

(defconst superchat-experience-cohort-v0--experiment-directory
  "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-v0/")

(defconst superchat-experience-cohort-v0--root
  "/Users/chenyibin/.agent-reach/superchat/experience-cohort-v0/")

(defconst superchat-experience-cohort-v0--allowed-tools
  '("read-file" "search-text" "list-files" "read_buffer"
    "workspace_read" "workspace_publish" "workspace_info"
    "team_task" "team_message"))

(defvar superchat-experience-cohort-v0-instance-id nil)

(setq superchat-data-directory
      (expand-file-name "runtime/" superchat-experience-cohort-v0--root)
      superchat-llm-backend
      (make-llm-ollama :chat-model "qwen3.6:latest")
      superchat-llm-model nil
      superchat-response-timeout 600
      superchat-subagent-timeout 1800)

(defun superchat-experience-cohort-v0--sha256-file (path)
  "Return PATH's SHA-256 with the protocol prefix."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (concat "sha256:" (secure-hash 'sha256 (current-buffer)))))

(defun superchat-experience-cohort-v0--read-json (path)
  "Read PATH as a plist-backed JSON object."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun superchat-experience-cohort-v0--permission (tool-name _args)
  "Allow only the frozen calibration TOOL-NAME surface."
  (if (member tool-name superchat-experience-cohort-v0--allowed-tools)
      'allow
    'deny))

(defun superchat-experience-cohort-v0--verify-inputs ()
  "Fail unless the frozen calibration inputs remain unchanged."
  (let* ((source-pack-path
          (expand-file-name "calibration-source-pack-v0.json"
                            superchat-experience-cohort-v0--experiment-directory))
         (source-pack
          (superchat-experience-cohort-v0--read-json source-pack-path))
         (manifest-path
          (expand-file-name "manifest-v1.json"
                            superchat-experience-cohort-v0--experiment-directory))
         (manifest (superchat-experience-cohort-v0--read-json manifest-path)))
    (unless (equal "calibration-authorized" (plist-get manifest :status))
      (error "Cohort v0 calibration manifest is not authorized"))
    (unless (equal (plist-get manifest :source_pack_sha256)
                   (superchat-experience-cohort-v0--sha256-file
                    source-pack-path))
      (error "Calibration source-pack manifest changed after freeze"))
    (dolist (source (plist-get source-pack :sources))
      (let ((path (plist-get source :path)))
        (unless (and (file-readable-p path)
                     (equal (plist-get source :sha256)
                            (superchat-experience-cohort-v0--sha256-file path)))
          (error "Frozen source missing or changed: %s" path))))
    manifest-path))

(defun superchat-experience-cohort-v0--query ()
  "Return the frozen first calibration query."
  (concat
   "[Experience Cohort v0 calibration; task cal-systems-01] "
   "Using only the four frozen SQLite official-document snapshots below, "
   "explain how rollback-journal and WAL modes differ in reader/writer "
   "behavior and crash-recovery guarantees. Distinguish documented facts "
   "from inference and cite the source file for every material claim.\n\n"
   "Sources:\n"
   "- sources/sqlite-wal.md\n"
   "- sources/sqlite-lockingv3.md\n"
   "- sources/sqlite-isolation.md\n"
   "- sources/sqlite-atomiccommit.md"))

(defun superchat-experience-cohort-v0--write-launch-record
    (manifest-path instance-id view)
  "Write launch evidence for INSTANCE-ID and VIEW under MANIFEST-PATH."
  (let* ((collector (car (plist-get view :steps)))
         (record
          (list
           :experiment_id "experience-cohort-v0-20260803"
           :manifest_revision "v1"
           :manifest_sha256
           (superchat-experience-cohort-v0--sha256-file manifest-path)
           :status "run.started"
           :claim_scope "calibration-execution-only"
           :started_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
           :instance_id instance-id
           :collector
           (list :task_id (plist-get collector :task-id)
                 :actor_id (plist-get collector :actor-id)
                 :run_id (plist-get collector :run-id)
                 :resume_decision (plist-get collector :resume-decision))
           :runtime_database
           (expand-file-name "superchat.db" superchat-data-directory)
           :provider "ollama"
           :model_id "qwen3.6:latest"
           :ollama_tag_id "07d35212591f"
           :model_blob_sha256
           "sha256:f5ee307a2982106a6eb82b62b2c00b575c9072145a759ae4660378acda8dcf2d"
           :source_pack_id "calibration-sqlite-official-v0"
           :effectiveness_claim "not_evaluated"
           :learning_claim "prohibited"))
         (path (expand-file-name "launch-record.json"
                                 superchat-experience-cohort-v0--experiment-directory)))
    (with-temp-file path
      (insert (json-encode record))
      (insert "\n"))
    record))

;;;###autoload
(defun superchat-experience-cohort-v0-start ()
  "Start the frozen first real calibration Run exactly once."
  (when superchat-experience-cohort-v0-instance-id
    (error "Calibration already started: %s"
           superchat-experience-cohort-v0-instance-id))
  (let* ((manifest-path (superchat-experience-cohort-v0--verify-inputs))
         (default-directory superchat-experience-cohort-v0--root)
         (superchat--active-authority-policy "developer-project"))
    (setq superchat-agent-permission-functions
          '(superchat-experience-cohort-v0--permission))
    (setq superchat-experience-cohort-v0-instance-id
          (superchat-research-workflow
           (superchat-experience-cohort-v0--query)))
    (let ((view (superchat-research-workflow-view
                 superchat-experience-cohort-v0-instance-id)))
      (superchat-experience-cohort-v0--write-launch-record
       manifest-path superchat-experience-cohort-v0-instance-id view))))

(provide 'superchat-experience-cohort-v0-calibration)

;;; run-calibration.el ends here
