;;; evidence.el --- DeepSeek Experience Cohort evidence closure -*- lexical-binding: t; -*-

;;; Commentary:

;; Experimental consumer of Runtime facts.  It evaluates evidence closure and
;; the frozen Outcome protocol without changing Tape, Closure, or Workspace.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'llm-ollama)
(require 'superchat-experience-cohort-deepseek-v6-run)

(defun superchat-experience-cohort-deepseek-v6--run-directory (assignment-id)
  "Return evidence directory for ASSIGNMENT-ID."
  (expand-file-name
   (format "runs/%s/" assignment-id)
   superchat-experience-cohort-deepseek-v6--directory))

(defun superchat-experience-cohort-deepseek-v6--events (session-id)
  "Return ordered Runtime events for SESSION-ID."
  (superchat-tape-replay-events session-id))

(defun superchat-experience-cohort-deepseek-v6--closure-summary (step)
  "Return Closure and identity evidence for Workflow STEP."
  (let* ((actor-id (plist-get step :actor-id))
         (run-id (plist-get step :run-id))
         (trace (and actor-id
                     (superchat-experience-cohort-deepseek-v6--events actor-id)))
         (well-formed
          (and run-id
               (superchat-closure-judge
                (list :kind 'trace-well-formed :run-id run-id) trace)))
         (terminal
          (and run-id
               (superchat-closure-judge
                (list :kind 'durable-run-terminal :run-id run-id) trace)))
         (tool-events
          (cl-remove-if-not
           (lambda (event)
             (and (equal run-id (plist-get event :run-id))
                  (string-prefix-p "tool." (or (plist-get event :type) ""))))
           trace))
         (invalid-tool-identity
          (cl-count-if-not
           (lambda (event)
             (and (equal run-id (plist-get event :run-id))
                  (equal actor-id (plist-get event :actor-id))
                  (stringp (plist-get event :turn-id))
                  (not (string-empty-p (plist-get event :turn-id)))))
           tool-events)))
    (list :role (symbol-name (plist-get step :role))
          :actor_id actor-id
          :run_id run-id
          :run_decision
          (when-let* ((decision (plist-get step :run-decision)))
            (symbol-name decision))
          :resume_decision
          (symbol-name (plist-get step :resume-decision))
          :trace_verdict
          (and well-formed (symbol-name (plist-get well-formed :verdict)))
          :terminal_verdict
          (and terminal (symbol-name (plist-get terminal :verdict)))
          :terminal_kind
          (when-let* ((judgment (and terminal
                                     (plist-get terminal :judgment)))
                      (kind (plist-get judgment :terminal-kind)))
            (symbol-name kind))
          :terminal_witness_event_ids
          (and terminal
               (plist-get (plist-get terminal :witness) :event-ids))
          :tool_event_count (length tool-events)
          :tool_identity
          (cond ((null tool-events) "not-applicable")
                ((zerop invalid-tool-identity) "pass")
                (t "invalid"))
          :tool_events_invalid_identity invalid-tool-identity
          :tool_failed_count
          (cl-count "tool.failed" tool-events
                    :key (lambda (event) (plist-get event :type))
                    :test #'equal))))

(defun superchat-experience-cohort-deepseek-v6--artifact-summary (step)
  "Return Artifact Closure classification for Workflow STEP."
  (let* ((artifacts (plist-get step :artifacts))
         (deliverable
          (cl-remove-if-not
           (lambda (artifact)
             (and (eq 'deliver (plist-get artifact :delivery-decision))
                  (plist-get artifact :available)))
           artifacts))
         (classification
          (cond ((null artifacts) "artifact_absent")
                ((= 1 (length deliverable)) "artifact_closure_pass")
                (t "artifact_present_provenance_invalid"))))
    (list :role (symbol-name (plist-get step :role))
          :classification classification
          :artifact_count (length artifacts)
          :deliverable_count (length deliverable)
          :artifacts
          (vconcat
           (mapcar
            (lambda (artifact)
              (list :artifact_id (plist-get artifact :artifact-id)
                    :revision (plist-get artifact :revision)
                    :uri (plist-get artifact :uri)
                    :delivery_decision
                    (symbol-name (plist-get artifact :delivery-decision))
                    :available
                    (if (plist-get artifact :available) t json-false)))
            artifacts)))))

(defun superchat-experience-cohort-deepseek-v6--prompt-snapshots (assignment-id)
  "Return persisted model-facing prompts for ASSIGNMENT-ID."
  (cl-remove-if-not
   (lambda (snapshot)
     (equal assignment-id (plist-get snapshot :assignment_id)))
   superchat-experience-cohort-deepseek-v6--prompt-snapshots))

(defun superchat-experience-cohort-deepseek-v6--artifact-references (steps)
  "Return blind evaluation references from Workflow STEPS."
  (vconcat
   (apply
    #'append
    (mapcar
     (lambda (step)
       (mapcar
        (lambda (artifact)
          (list :role (symbol-name (plist-get step :role))
                :artifact_id (plist-get artifact :artifact-id)
                :revision (plist-get artifact :revision)
                :uri (plist-get artifact :uri)))
        (plist-get step :artifacts)))
     steps))))

(defun superchat-experience-cohort-deepseek-v6--parse-evaluator-json (raw)
  "Parse the outermost JSON object from evaluator response RAW."
  (when (stringp raw)
    (let ((begin (cl-position ?{ raw))
          (end (cl-position ?} raw :from-end t)))
      (when (and begin end (< begin end))
        (ignore-errors
          (json-parse-string (substring raw begin (1+ end))
                             :object-type 'plist :array-type 'list
                             :null-object nil
                             :false-object json-false))))))

(defun superchat-experience-cohort-deepseek-v6--decision (scores)
  "Return frozen primary outcome for criterion SCORES."
  (cond ((cl-some #'zerop scores) "reject")
        ((cl-every (lambda (score) (= score 2)) scores) "accept")
        (t "revise")))

(defun superchat-experience-cohort-deepseek-v6--valid-evaluation-p (evaluation)
  "Return non-nil when EVALUATION satisfies the frozen schema and rule."
  (let* ((expected-keys
          '(:rubric_id :evidence_completeness :citation_quality
            :contradiction_handling :primary_outcome :missing_evidence
            :abstained))
         (keys (cl-loop for (key _value) on evaluation by #'cddr
                        collect key))
         (criteria
          (mapcar (lambda (key) (plist-get evaluation key))
                  '(:evidence_completeness :citation_quality
                    :contradiction_handling)))
         (scores (mapcar (lambda (criterion)
                           (plist-get criterion :score))
                         criteria)))
    (and (listp evaluation)
         (null (cl-set-difference keys expected-keys))
         (null (cl-set-difference expected-keys keys))
         (equal "research-quality-v0" (plist-get evaluation :rubric_id))
         (cl-every (lambda (criterion)
                     (and (listp criterion)
                          (stringp (plist-get criterion :evidence))))
                   criteria)
         (cl-every (lambda (score)
                     (and (integerp score) (<= 0 score 2)))
                   scores)
         (equal (superchat-experience-cohort-deepseek-v6--decision scores)
                (plist-get evaluation :primary_outcome))
         (listp (plist-get evaluation :missing_evidence)))))

(defun superchat-experience-cohort-deepseek-v6--missing-outcome
    (assignment launch reason)
  "Return frozen missing-data Outcome for ASSIGNMENT and LAUNCH with REASON."
  (list :outcome_record_id
        (format "%s-outcome" (plist-get assignment :assignment_id))
        :experiment_id "experience-cohort-deepseek-v6"
        :assignment_id (plist-get assignment :assignment_id)
        :research_instance_id (plist-get launch :instance_id)
        :evaluation_mode "preregistered-missing-data-rule"
        :evaluation_status "pass"
        :outcome_closure "pass"
        :rubric_id "research-quality-v0"
        :rubric_sha256
        (plist-get (plist-get (superchat-experience-cohort-deepseek-v6--manifest)
                             :outcome)
                   :rubric_sha256)
        :evaluation_target nil
        :criteria
        (vector
         (list :id "evidence_completeness" :score 0 :evidence reason)
         (list :id "citation_quality" :score 0 :evidence reason)
         (list :id "contradiction_handling" :score 0 :evidence reason))
        :primary_outcome "reject"
        :missing_evidence (vector reason)
        :abstained json-false
        :evaluated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun superchat-experience-cohort-deepseek-v6--evaluate
    (assignment launch view)
  "Create the frozen Outcome Record for ASSIGNMENT, LAUNCH and Workflow VIEW."
  (let* ((assignment-id (plist-get assignment :assignment_id))
         (directory
          (superchat-experience-cohort-deepseek-v6--run-directory assignment-id))
         (path (expand-file-name "outcome-record.json" directory)))
    (if (file-readable-p path)
        (superchat-experience-cohort-deepseek-v6--read-json path)
      (let* ((steps (plist-get view :steps))
             (reviewer (cl-find 'reviewer steps
                                :key (lambda (step) (plist-get step :role))))
             (artifact
              (cl-find-if
               (lambda (candidate)
                 (and (eq 'deliver
                          (plist-get candidate :delivery-decision))
                      (plist-get candidate :available)))
               (plist-get reviewer :artifacts)))
             outcome)
        (if (null artifact)
            (setq outcome
                  (superchat-experience-cohort-deepseek-v6--missing-outcome
                   assignment launch "final_reviewer_artifact_missing"))
          (let* ((manifest (superchat-experience-cohort-deepseek-v6--manifest))
                 (outcome-spec (plist-get manifest :outcome))
                 (reviewer-content
                  (superchat-workspace-read
                   (plist-get artifact :artifact-id)
                   (plist-get artifact :workspace-id)
                   (plist-get artifact :revision)))
                 (references
                  (superchat-experience-cohort-deepseek-v6--artifact-references steps))
                 (evaluator-prompt
                  (superchat-experience-cohort-deepseek-v6--read-file
                   (expand-file-name
                    (plist-get outcome-spec :evaluator_prompt_path)
                    "/Users/chenyibin/Documents/emacs/package/superchat/")))
                 (rubric
                  (superchat-experience-cohort-deepseek-v6--read-file
                   (expand-file-name
                    (plist-get outcome-spec :rubric_path)
                    "/Users/chenyibin/Documents/emacs/package/superchat/")))
                 (system-prompt
                  (concat evaluator-prompt "\n\nFrozen rubric:\n" rubric))
                 (input
                  (json-encode
                   (list :task_input (plist-get assignment :question)
                         :reviewer_artifact reviewer-content
                         :artifact_evidence_references references)))
                 (session-id (format "cohort-evaluator-%s" assignment-id))
                 raw evaluation evaluator-events evaluator-run closure)
            (condition-case err
                (let ((superchat-llm-backend
                       (make-llm-ollama :chat-model "qwen3.6:latest"))
                      (superchat-llm-model nil)
                      (superchat-llm-reasoning 'none))
                  (setq raw
                        (superchat-runtime-submit-blocking
                         (list :type :llm-query
                               :session-id session-id
                               :actor-id "blind-evaluator"
                               :correlation-id (plist-get launch :instance_id)
                               :input "Blindly evaluate the final Research Artifact"
                               :prompt input
                               :system-prompt system-prompt
                               :tools 'none
                               :record-conversation nil
                               :authority-policy "default"
                               :topology 'single)
                         7200)))
              (error
               (setq raw (format "[Evaluator error: %s]"
                                 (error-message-string err)))))
            (setq evaluation
                  (superchat-experience-cohort-deepseek-v6--parse-evaluator-json raw)
                  evaluator-events
                  (superchat-experience-cohort-deepseek-v6--events session-id)
                  evaluator-run
                  (car
                   (last
                    (cl-remove-if-not
                     (lambda (event)
                       (equal "run.started" (plist-get event :type)))
                     evaluator-events))))
            (when evaluator-run
              (setq closure
                    (superchat-experience-cohort-deepseek-v6--closure-summary
                     (list :role 'evaluator
                           :actor-id session-id
                           :run-id (plist-get evaluator-run :run-id)
                           :run-decision 'deliver
                           :resume-decision 'advance))))
            (setq outcome
                  (list
                   :outcome_record_id (format "%s-outcome" assignment-id)
                   :experiment_id "experience-cohort-deepseek-v6"
                   :assignment_id assignment-id
                   :research_instance_id (plist-get launch :instance_id)
                   :evaluation_mode "blind-model"
                   :evaluation_status
                   (if (superchat-experience-cohort-deepseek-v6--valid-evaluation-p
                        evaluation)
                       "pass"
                     "invalid")
                   :outcome_closure
                   (if (and (superchat-experience-cohort-deepseek-v6--valid-evaluation-p
                             evaluation)
                            (equal "proved" (plist-get closure :terminal_verdict))
                            (equal "completed" (plist-get closure :terminal_kind)))
                       "pass"
                     "blocked")
                   :rubric_id "research-quality-v0"
                   :rubric_sha256 (plist-get outcome-spec :rubric_sha256)
                   :evaluator_id (plist-get outcome-spec :evaluator_id)
                   :evaluator_model_id "qwen3.6:latest"
                   :evaluator_model_blob_sha256
                   (plist-get outcome-spec :evaluator_model_blob_sha256)
                   :evaluator_prompt_sha256
                   (plist-get outcome-spec :evaluator_prompt_sha256)
                   :evaluator_system_prompt_sha256
                   (superchat-experience-cohort-deepseek-v6--sha256-string system-prompt)
                   :evaluator_session_id session-id
                   :evaluator_run_id (plist-get evaluator-run :run-id)
                   :evaluator_closure closure
                   :evaluation_target
                   (list :artifact_id (plist-get artifact :artifact-id)
                         :revision (plist-get artifact :revision))
                   :artifact_evidence_references references
                   :raw_response raw
                   :evaluation evaluation
                   :primary_outcome
                   (or (plist-get evaluation :primary_outcome) "reject")
                   :evaluated_at
                   (format-time-string "%Y-%m-%dT%H:%M:%S%z")))))
        (superchat-experience-cohort-deepseek-v6--write-json path outcome)
        outcome))))

(defun superchat-experience-cohort-deepseek-v6--build-evidence
    (assignment launch view outcome)
  "Write and return evidence for ASSIGNMENT, LAUNCH, VIEW and OUTCOME."
  (let* ((assignment-id (plist-get assignment :assignment_id))
         (steps (plist-get view :steps))
         (prompts
          (superchat-experience-cohort-deepseek-v6--prompt-snapshots assignment-id))
         (closure (mapcar #'superchat-experience-cohort-deepseek-v6--closure-summary
                          steps))
         (artifacts (mapcar #'superchat-experience-cohort-deepseek-v6--artifact-summary
                            steps))
         (expected-tools
          (sort (copy-sequence
                 superchat-experience-cohort-deepseek-v6--allowed-tools)
                #'string<))
         (guard
          (string-trim-right
           (superchat-experience-cohort-deepseek-v6--read-file
            superchat-experience-cohort-deepseek-v6--deliverable-guard-path)))
         (prior-pass
          (and (= 3 (length prompts))
               (cl-every
                (lambda (snapshot)
                  (and
                   (equal "deepseek-v4-pro"
                          (plist-get snapshot :target_model))
                   (equal "deepseek v4 pro" (plist-get snapshot :provider))
                   (equal "none" (plist-get snapshot :reasoning_mode))
                   (= 30 (plist-get snapshot :connect_timeout_seconds))
                   (string-match-p (regexp-quote guard)
                                   (plist-get snapshot :model_prompt))
                   (equal
                    (plist-get snapshot :model_prompt_sha256)
                    (superchat-experience-cohort-deepseek-v6--sha256-string
                     (plist-get snapshot :model_prompt)))
                   (equal
                    (plist-get snapshot :system_prompt_sha256)
                    (superchat-experience-cohort-deepseek-v6--sha256-string
                     (plist-get snapshot :system_prompt)))
                   (equal expected-tools
                          (sort
                           (append (plist-get snapshot :prepared_tool_names)
                                   nil)
                           #'string<))))
                prompts)))
         (execution-pass
          (and (= 3 (length closure))
               (cl-every
                (lambda (item)
                  (and (equal "valid" (plist-get item :trace_verdict))
                       (equal "proved" (plist-get item :terminal_verdict))
                       (equal "completed" (plist-get item :terminal_kind))
                       (not (equal "invalid" (plist-get item :tool_identity)))))
                closure)))
         (artifact-pass
          (and (= 3 (length artifacts))
               (cl-every
                (lambda (item)
                  (and (equal "artifact_closure_pass"
                              (plist-get item :classification))
                       (= 1 (plist-get item :artifact_count))))
                artifacts)))
         (outcome-pass (equal "pass" (plist-get outcome :outcome_closure)))
         (workflow-contract
          (if (and (eq 'completed (plist-get view :status)) artifact-pass)
              "satisfied"
            "violated"))
         (overall
          (and prior-pass execution-pass artifact-pass outcome-pass
               (equal "satisfied" workflow-contract)))
         (evidence
          (list
           :experiment_id "experience-cohort-deepseek-v6"
           :assignment_id assignment-id
           :run_index (plist-get launch :run_index)
           :research_instance_id (plist-get launch :instance_id)
           :generated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
           :gates
           (vector
            (list :name "prior" :verdict (if prior-pass "pass" "blocked"))
            (list :name "execution"
                  :verdict (if execution-pass "pass" "blocked"))
            (list :name "artifact"
                  :verdict (if artifact-pass "pass" "blocked"))
            (list :name "outcome"
                  :verdict (if outcome-pass "pass" "blocked"))
            (list :name "comparison" :verdict "not-applicable"))
           :evidence_closure (if overall "pass" "blocked")
           :workflow_contract workflow-contract
           :outcome_result (plist-get outcome :primary_outcome)
           :prompt_snapshots (vconcat prompts)
           :execution (vconcat closure)
           :artifacts (vconcat artifacts)
           :outcome_record
           (format "runs/%s/outcome-record.json" assignment-id)
           :effectiveness_claim "not_evaluated"
           :learning_claim "prohibited")))
    (superchat-experience-cohort-deepseek-v6--write-json
     (expand-file-name
      "evidence-bundle.json"
      (superchat-experience-cohort-deepseek-v6--run-directory assignment-id))
     evidence)
    evidence))

(defun superchat-experience-cohort-deepseek-v6--report ()
  "Write the current twenty-Run Cohort report."
  (let (rows)
    (dolist (assignment (superchat-experience-cohort-deepseek-v6--assignments))
      (let* ((assignment-id (plist-get assignment :assignment_id))
             (launch
              (superchat-experience-cohort-deepseek-v6--launch
               (plist-get assignment :run_index)))
             (evidence-path
              (expand-file-name
               "evidence-bundle.json"
               (superchat-experience-cohort-deepseek-v6--run-directory assignment-id)))
             (evidence (and (file-readable-p evidence-path)
                            (superchat-experience-cohort-deepseek-v6--read-json
                             evidence-path))))
        (push
         (list :run_index (plist-get assignment :run_index)
               :assignment_id assignment-id
               :split (plist-get assignment :split)
               :status
               (cond (evidence (plist-get evidence :evidence_closure))
                     (launch
                      (if (superchat-experience-cohort-deepseek-v6--settled-p launch)
                          "settled-not-finalized"
                        "running"))
                     (t "not-started"))
               :workflow_contract
               (and evidence (plist-get evidence :workflow_contract))
               :outcome_result
               (and evidence (plist-get evidence :outcome_result)))
         rows)))
    (setq rows (nreverse rows))
    (let* ((finalized-count
            (cl-count-if
             (lambda (row) (member (plist-get row :status)
                                   '("pass" "blocked")))
             rows))
           (pass-count
            (cl-count "pass" rows
                      :key (lambda (row) (plist-get row :status))
                      :test #'equal))
           (calibration-rows
            (cl-remove-if-not
             (lambda (row) (equal "calibration" (plist-get row :split)))
             rows))
           (calibration-pass-count
            (cl-count "pass" calibration-rows
                      :key (lambda (row) (plist-get row :status))
                      :test #'equal))
           (calibration-gate
            (if (= calibration-pass-count 4) "pass" "blocked"))
           (report
            (list :experiment_id
                  "experience-cohort-deepseek-v6"
                  :generated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")
                  :planned_run_count 20
                  :finalized_run_count finalized-count
                  :evidence_closure_pass_count pass-count
                  :cohort_execution_status
                  (if (= finalized-count 20) "completed" "incomplete")
                  :calibration_required_pass_count 4
                  :calibration_observed_pass_count calibration-pass-count
                  :calibration_gate calibration-gate
                  :rows (vconcat rows)
                  :discovery_authorized
                  (if (equal "pass" calibration-gate) t json-false)
                  :held_out_used json-false
                  :held_out_status "sealed"
                  :pattern_observation "not_started"
                  :effectiveness_claim "not_evaluated"
                  :learning_claim "prohibited"))
           (json-path
            (expand-file-name "cohort-report.json"
                              superchat-experience-cohort-deepseek-v6--directory))
           (markdown-path
            (expand-file-name "cohort-report.md"
                              superchat-experience-cohort-deepseek-v6--directory)))
      (superchat-experience-cohort-deepseek-v6--write-json json-path report)
      (with-temp-file markdown-path
        (insert "# DeepSeek Experience Cohort v3\n\n")
        (insert (format "Generated: `%s`\n\n" (plist-get report :generated_at)))
        (insert (format "Execution: **%s** (%d/20 finalized)\n\n"
                        (plist-get report :cohort_execution_status)
                        finalized-count))
        (insert (format "Calibration Gate: **%s** (%d/4)\n\n"
                        calibration-gate calibration-pass-count))
        (insert "| Index | Split | Assignment | Evidence | Workflow Contract | Outcome |\n")
        (insert "|---:|---|---|---|---|---|\n")
        (dolist (row rows)
          (insert
           (format "| %s | %s | %s | %s | %s | %s |\n"
                   (plist-get row :run_index)
                   (plist-get row :split)
                   (plist-get row :assignment_id)
                   (plist-get row :status)
                   (or (plist-get row :workflow_contract) "-")
                   (or (plist-get row :outcome_result) "-"))))
        (insert "\nDiscovery is authorized only by a 4/4 Calibration pass. ")
        (insert "Replicates do not increase occurrence thresholds. Held-out remains sealed.\n")
        (insert "No effectiveness, Pattern, or learning claim is authorized.\n"))
      report)))

(defun superchat-experience-cohort-deepseek-v6-finalize (run-index)
  "Finalize settled Cohort RUN-INDEX into Outcome and evidence records."
  (interactive "nCohort index (1-20): ")
  (let* ((assignment (superchat-experience-cohort-deepseek-v6--assignment run-index))
         (launch (or (superchat-experience-cohort-deepseek-v6--launch run-index)
                     (error "Cohort index not started: %s" run-index)))
         (_ (unless (superchat-experience-cohort-deepseek-v6--settled-p launch)
              (error "Cohort index is still running: %s" run-index)))
         (view (superchat-research-workflow-view
                (plist-get launch :instance_id)))
         (outcome
          (superchat-experience-cohort-deepseek-v6--evaluate assignment launch view))
         (evidence
          (superchat-experience-cohort-deepseek-v6--build-evidence
           assignment launch view outcome)))
    (superchat-experience-cohort-deepseek-v6--report)
    evidence))

(defun superchat-experience-cohort-deepseek-v6--evidence (assignment)
  "Return persisted evidence for ASSIGNMENT, or nil."
  (let ((path
         (expand-file-name
          "evidence-bundle.json"
          (superchat-experience-cohort-deepseek-v6--run-directory
           (plist-get assignment :assignment_id)))))
    (and (file-readable-p path)
         (superchat-experience-cohort-deepseek-v6--read-json path))))

(defun superchat-experience-cohort-deepseek-v6--calibration-pass-p ()
  "Return non-nil when all four Calibration evidence bundles pass."
  (cl-every
   (lambda (assignment)
     (let ((evidence
            (superchat-experience-cohort-deepseek-v6--evidence assignment)))
       (and evidence
            (equal "pass" (plist-get evidence :evidence_closure)))))
   (cl-subseq (superchat-experience-cohort-deepseek-v6--assignments) 0 4)))

(defun superchat-experience-cohort-deepseek-v6--wait (launch)
  "Wait until LAUNCH reaches a durable settlement point."
  (while (not (superchat-experience-cohort-deepseek-v6--settled-p launch))
    (accept-process-output nil 1)))

;;;###autoload
(defun superchat-experience-cohort-deepseek-v6-run-all ()
  "Run and finalize all twenty frozen first attempts sequentially."
  (interactive)
  (superchat-experience-cohort-deepseek-v6--verify-inputs)
  (when (and (null superchat-experience-cohort-deepseek-v6--launches)
             (file-exists-p
              (expand-file-name "superchat.db" superchat-data-directory)))
    (error "Cohort Runtime database exists without a launch record"))
  (dolist (assignment (superchat-experience-cohort-deepseek-v6--assignments))
    (let ((run-index (plist-get assignment :run_index)))
      (when (and (= run-index 5)
                 (not (superchat-experience-cohort-deepseek-v6--calibration-pass-p)))
        (superchat-experience-cohort-deepseek-v6--report)
        (error "Discovery blocked: Calibration did not close 4/4"))
      (unless (superchat-experience-cohort-deepseek-v6--evidence assignment)
        (let ((launch
               (or (superchat-experience-cohort-deepseek-v6--launch run-index)
                   (superchat-experience-cohort-deepseek-v6-start run-index))))
          (message "Cohort %02d/20 started: %s"
                   run-index (plist-get assignment :assignment_id))
          (superchat-experience-cohort-deepseek-v6--wait launch)
          (superchat-experience-cohort-deepseek-v6-finalize run-index)
          (message "Cohort %02d/20 finalized: %s"
                   run-index (plist-get assignment :assignment_id))))))
  (superchat-experience-cohort-deepseek-v6--report))

(defun superchat-experience-cohort-deepseek-v6-report ()
  "Return and persist current DeepSeek Experience Cohort status."
  (interactive)
  (superchat-experience-cohort-deepseek-v6--report))

(provide 'superchat-experience-cohort-deepseek-v6-evidence)

;;; evidence.el ends here
