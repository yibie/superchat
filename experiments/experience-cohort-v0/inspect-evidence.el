;;; inspect-evidence.el --- Snapshot Cohort v0 launch evidence -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only inspection over the dedicated Runtime/Tape and generated files.
;; The report is a point-in-time snapshot; running means running, not passed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst superchat-experience-cohort-v0-inspection--directory
  "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-v0/")

(defun superchat-experience-cohort-v0-inspection--read-json (name)
  "Read experiment JSON file NAME as a plist."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name name
                       superchat-experience-cohort-v0-inspection--directory))
    (json-parse-buffer :object-type 'plist :array-type 'list)))

(defun superchat-experience-cohort-v0-inspection--sha256 (path)
  "Return PATH's protocol-prefixed SHA-256."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (concat "sha256:" (secure-hash 'sha256 (current-buffer)))))

(defun superchat-experience-cohort-v0-inspection--launches ()
  "Return the frozen twenty Collector launch identities."
  (let* ((v1 (superchat-experience-cohort-v0-inspection--read-json
              "launch-record.json"))
         (collector (plist-get v1 :collector))
         (v3 (superchat-experience-cohort-v0-inspection--read-json
              "batch-launch-record-v3.json")))
    (cons
     (list :run_index 1
           :manifest_revision "v1"
           :assignment_id "cal-systems-01"
           :split "calibration"
           :instance_id (plist-get v1 :instance_id)
           :actor_id (plist-get collector :actor_id)
           :run_id (plist-get collector :run_id))
     (plist-get v3 :launches))))

(defun superchat-experience-cohort-v0-inspection--artifact (event)
  "Return availability evidence for Artifact EVENT."
  (let* ((payload (plist-get event :payload))
         (path (plist-get payload :path))
         (revision (plist-get payload :revision))
         (readable (and (stringp path) (file-readable-p path)))
         (hash (and readable
                    (substring
                     (superchat-experience-cohort-v0-inspection--sha256 path)
                     (length "sha256:")))))
    (list :artifact_id (plist-get payload :artifact-id)
          :role (plist-get payload :role)
          :path path
          :revision revision
          :readable (and readable t)
          :hash_matches (and hash (equal hash revision)))))

(defun superchat-experience-cohort-v0-inspection--row (launch)
  "Inspect one Collector LAUNCH against Runtime, Tape and Artifact files."
  (let* ((run-id (plist-get launch :run_id))
         (events
          (cl-remove-if-not
           (lambda (event) (equal run-id (plist-get event :run-id)))
           (superchat-tape-replay-events (plist-get launch :actor_id))))
         (run-started
          (cl-remove-if-not
           (lambda (event) (equal "run.started" (plist-get event :type)))
           events))
         (tool-events
          (cl-remove-if-not
           (lambda (event)
             (string-prefix-p "tool." (or (plist-get event :type) "")))
           events))
         (missing-turn
          (cl-count-if-not
           (lambda (event)
             (and (stringp (plist-get event :turn-id))
                  (not (string-empty-p (plist-get event :turn-id)))))
           tool-events))
         (terminals
          (cl-remove-if-not
           (lambda (event)
             (member (plist-get event :type)
                     '("run.completed" "run.failed" "run.cancelled")))
           events))
         (artifacts
          (mapcar
           #'superchat-experience-cohort-v0-inspection--artifact
           (cl-remove-if-not
            (lambda (event)
              (equal "workspace.artifact.added" (plist-get event :type)))
            events)))
         (runtime (superchat-runtime-status run-id))
         (status (plist-get runtime :status))
         (workflow
          (condition-case err
              (superchat-research-workflow-view
               (plist-get launch :instance_id))
            (error (list :inspection_error (error-message-string err)))))
         (workflow-status (plist-get workflow :status)))
    (append
     launch
     (list :runtime_status (if (symbolp status) (symbol-name status) status)
           :run_started_count (length run-started)
           :run_started_event_id (plist-get (car run-started) :event-id)
           :turn_id (plist-get (car run-started) :turn-id)
           :event_count (length events)
           :tool_event_count (length tool-events)
           :tool_events_missing_turn_id missing-turn
           :tool_identity
           (cond ((and (null tool-events) terminals) "not-applicable")
                 ((null tool-events) "not-yet-observed")
                 ((zerop missing-turn) "pass")
                 (t "invalid"))
           :terminal_count (length terminals)
           :terminal_type (plist-get (car terminals) :type)
           :artifacts (vconcat artifacts)
           :artifact_evidence
           (cond ((and (null artifacts) terminals) "missing")
                 ((null artifacts) "not-yet-observed")
                 ((cl-every (lambda (artifact)
                              (and (plist-get artifact :readable)
                                   (plist-get artifact :hash_matches)))
                            artifacts)
                  "pass")
                 (t "invalid"))
           :workflow_status
           (if (symbolp workflow-status)
               (symbol-name workflow-status)
             workflow-status)
           :workflow_inspection_error (plist-get workflow :inspection_error)
           :workflow_steps
           (vconcat
            (mapcar
             (lambda (step)
               (list :role (symbol-name (plist-get step :role))
                     :run_id (plist-get step :run-id)
                     :run_decision
                     (when-let* ((decision (plist-get step :run-decision)))
                       (symbol-name decision))
                     :resume_decision
                     (symbol-name (plist-get step :resume-decision))
                     :artifact_count (length (plist-get step :artifacts))
                     :available_artifact_count
                     (cl-count-if
                      (lambda (artifact) (plist-get artifact :available))
                      (plist-get step :artifacts))))
             (plist-get workflow :steps)))))))

(defun superchat-experience-cohort-v0-inspection--counts (rows key)
  "Return value counts for KEY over ROWS."
  (let ((table (make-hash-table :test #'equal)) result)
    (dolist (row rows)
      (let ((value (or (plist-get row key) "null")))
        (puthash value (1+ (gethash value table 0)) table)))
    (maphash (lambda (value count)
               (push (list :value value :count count) result))
             table)
    (sort result (lambda (a b)
                   (string< (format "%s" (plist-get a :value))
                            (format "%s" (plist-get b :value)))))))

(defun superchat-experience-cohort-v0-inspect-evidence ()
  "Write JSON and Markdown evidence/execution inspection snapshots."
  (interactive)
  (let* ((launches
          (superchat-experience-cohort-v0-inspection--launches))
         (rows (mapcar #'superchat-experience-cohort-v0-inspection--row
                       launches))
         (v1 (superchat-experience-cohort-v0-inspection--read-json
              "launch-record.json"))
         (v2 (superchat-experience-cohort-v0-inspection--read-json
              "batch-launch-record-v2.json"))
         (v3 (superchat-experience-cohort-v0-inspection--read-json
              "batch-launch-record-v3.json"))
         (manifest-v1 (superchat-experience-cohort-v0-inspection--read-json
                       "manifest-v1.json"))
         (manifest-v3 (superchat-experience-cohort-v0-inspection--read-json
                       "manifest-v3.json"))
         (manifest-checks
          (mapcar
           (lambda (entry)
             (let ((path (expand-file-name
                          (format "manifest-%s.json" (car entry))
                          superchat-experience-cohort-v0-inspection--directory)))
               (list :revision (car entry)
                     :expected (cdr entry)
                     :actual
                     (superchat-experience-cohort-v0-inspection--sha256 path)
                     :matches
                     (equal (cdr entry)
                            (superchat-experience-cohort-v0-inspection--sha256
                             path)))))
           `(("v1" . ,(plist-get v1 :manifest_sha256))
             ("v2" . ,(plist-get v2 :manifest_sha256))
             ("v3" . ,(plist-get v3 :manifest_sha256)))))
         (frozen-input-checks
          (mapcar
           (lambda (entry)
             (let* ((path (car entry))
                    (expected (cdr entry))
                    (actual
                     (superchat-experience-cohort-v0-inspection--sha256
                      (expand-file-name
                       path
                       "/Users/chenyibin/Documents/emacs/package/superchat/"))))
               (list :path path :expected expected :actual actual
                     :matches (equal expected actual))))
           `((,(plist-get (plist-get manifest-v3 :launch_target)
                          :assignments_path)
              . ,(plist-get (plist-get manifest-v3 :launch_target)
                            :assignments_sha256))
             (,(plist-get (plist-get manifest-v3 :closure_repair)
                          :source_path)
              . ,(plist-get (plist-get manifest-v3 :closure_repair)
                            :source_sha256))
             (,(plist-get (plist-get manifest-v3 :closure_repair)
                          :test_path)
              . ,(plist-get (plist-get manifest-v3 :closure_repair)
                            :test_sha256))
             (,(plist-get (plist-get manifest-v3 :execution)
                          :base_runner_path)
              . ,(plist-get (plist-get manifest-v3 :execution)
                            :base_runner_sha256))
             (,(plist-get (plist-get manifest-v3 :execution) :runner_path)
              . ,(plist-get (plist-get manifest-v3 :execution)
                            :runner_sha256))
             (,(plist-get (plist-get manifest-v1 :outcome) :rubric_path)
              . ,(plist-get (plist-get manifest-v3 :outcome) :rubric_sha256))
             (,(plist-get (plist-get manifest-v1 :outcome)
                          :evaluator_prompt_path)
              . ,(plist-get (plist-get manifest-v3 :outcome)
                            :evaluator_prompt_sha256)))))
         (report
          (list
           :experiment_id "experience-cohort-v0-20260803"
           :snapshot_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
           :scope "twenty-frozen-collector-launches"
           :workflow_start_count (length rows)
           :unique_run_id_count
           (length (delete-dups (mapcar (lambda (row)
                                          (plist-get row :run_id))
                                        rows)))
           :unique_instance_id_count
           (length (delete-dups (mapcar (lambda (row)
                                          (plist-get row :instance_id))
                                        rows)))
           :held_out_used
           (if (cl-find "held-out" rows
                        :key (lambda (row) (plist-get row :split))
                        :test #'equal)
               t
             json-false)
           :manifest_checks (vconcat manifest-checks)
           :frozen_input_checks (vconcat frozen-input-checks)
           :prior_launch_record_matches
           (equal (plist-get v3 :prior_launch_record_sha256)
                  (superchat-experience-cohort-v0-inspection--sha256
                   (expand-file-name
                    "batch-launch-record-v2.json"
                    superchat-experience-cohort-v0-inspection--directory)))
           :runtime_status_counts
           (vconcat
            (superchat-experience-cohort-v0-inspection--counts
             rows :runtime_status))
           :tool_identity_counts
           (vconcat
            (superchat-experience-cohort-v0-inspection--counts
             rows :tool_identity))
           :artifact_evidence_counts
           (vconcat
            (superchat-experience-cohort-v0-inspection--counts
             rows :artifact_evidence))
           :terminal_count
           (cl-count-if (lambda (row)
                          (= 1 (plist-get row :terminal_count)))
                        rows)
           :rows (vconcat rows)
           :effectiveness_claim "not_evaluated"
           :learning_claim "prohibited"))
         (json-path
          (expand-file-name "evidence-execution-inspection.json"
                            superchat-experience-cohort-v0-inspection--directory))
         (markdown-path
          (expand-file-name "evidence-execution-inspection.md"
                            superchat-experience-cohort-v0-inspection--directory)))
    (with-temp-file json-path
      (insert (json-encode report))
      (insert "\n"))
    (with-temp-file markdown-path
      (insert "# Experience Cohort v0 — Evidence and Execution Inspection\n\n")
      (insert (format "Snapshot: `%s`\n\n" (plist-get report :snapshot_at)))
      (insert (format "- Frozen Workflow starts: **%d**\n"
                      (plist-get report :workflow_start_count)))
      (insert (format "- Unique Run IDs: **%d**\n"
                      (plist-get report :unique_run_id_count)))
      (insert (format "- Unique instance IDs: **%d**\n"
                      (plist-get report :unique_instance_id_count)))
      (insert (format "- Held-out used: **%s**\n"
                      (if (eq t (plist-get report :held_out_used))
                          "yes"
                        "no")))
      (insert
       (format "- Frozen input hashes match: **%s**\n"
               (if (cl-every (lambda (check) (plist-get check :matches))
                             frozen-input-checks)
                   "yes"
                 "no")))
      (insert (format "- Durable terminal witnesses at snapshot: **%d**\n\n"
                      (plist-get report :terminal_count)))
      (insert "| Index | Manifest | Runtime | Workflow | Tool identity | Artifact evidence | Terminal |\n")
      (insert "|---:|---|---|---|---|---|---|\n")
      (dolist (row rows)
        (insert
         (format "| %s | %s | %s | %s | %s | %s | %s |\n"
                 (plist-get row :run_index)
                 (plist-get row :manifest_revision)
                 (plist-get row :runtime_status)
                 (or (plist-get row :workflow_status) "inspection-error")
                 (plist-get row :tool_identity)
                 (plist-get row :artifact_evidence)
                 (or (plist-get row :terminal_type) "pending"))))
      (insert "\nRunning and not-yet-observed are open evidence, not failures or passes.\n")
      (insert "The snapshot makes no effectiveness, Pattern, or learning claim.\n"))
    report))

(provide 'superchat-experience-cohort-v0-inspection)

;;; inspect-evidence.el ends here
