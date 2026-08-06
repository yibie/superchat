;;; evidence.el --- Closed-loop correction evidence closure -*- lexical-binding: t; -*-

;;; Commentary:

;; Experiment-only evidence and blinded Outcome helpers.  This file does not
;; modify product Runtime, Authority, Memory, Workspace, or runner behavior.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'llm-ollama nil t)
(require 'superchat-closure)
(require 'superchat-runtime)
(require 'superchat-tape)
(require 'superchat-workspace)

(declare-function make-llm-ollama "llm-ollama")
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-reasoning)

(defconst superchat-closed-loop-experience-correction-v0--experiment-id
  "closed-loop-experience-correction-v0")

(defconst superchat-closed-loop-experience-correction-v0--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst superchat-closed-loop-experience-correction-v0--manifest-path
  (expand-file-name "manifest.json"
                    superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--assignments-path
  (expand-file-name "assignments.json"
                    superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--correction-rubric-path
  (expand-file-name
   "correction-fidelity-rubric-v0.json"
   superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--research-rubric-path
  (expand-file-name "../research-outcome-rubric-v0.json"
                    superchat-closed-loop-experience-correction-v0--directory))

(defconst superchat-closed-loop-experience-correction-v0--prompt-record-path
  "/private/tmp/superchat-closed-loop-experience-correction-v0/prompt-snapshots.json")

(defconst superchat-closed-loop-experience-correction-v0--forbidden-blind-keys
  '(:arm :claim_arm :claim_block :claim_source :claim_source_label
    :condition :cost :latency :run_cost :run_latency :prompt_hash
    :prompt_sha256 :model_prompt_sha256 :system_prompt_sha256))

(defun superchat-closed-loop-experience-correction-v0--read-json (path)
  "Read PATH as a plist-backed JSON object."
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'plist :array-type 'list
                       :null-object nil :false-object json-false)))

(defun superchat-closed-loop-experience-correction-v0--write-json (path value)
  "Write VALUE as JSON to PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert (json-encode value))
    (insert "\n")))

(defun superchat-closed-loop-experience-correction-v0--sha256-string (value)
  "Return protocol-prefixed SHA-256 for VALUE."
  (concat "sha256:" (secure-hash 'sha256 (or value ""))))

(defun superchat-closed-loop-experience-correction-v0--sha256-file (path)
  "Return protocol-prefixed SHA-256 for PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (concat "sha256:" (secure-hash 'sha256 (current-buffer)))))

(defun superchat-closed-loop-experience-correction-v0--run-directory
    (assignment-id)
  "Return evidence directory for ASSIGNMENT-ID."
  (expand-file-name
   (format "runs/%s/" assignment-id)
   superchat-closed-loop-experience-correction-v0--directory))

(defun superchat-closed-loop-experience-correction-v0--manifest ()
  "Return the frozen manifest when present."
  (and (file-readable-p superchat-closed-loop-experience-correction-v0--manifest-path)
       (superchat-closed-loop-experience-correction-v0--read-json
        superchat-closed-loop-experience-correction-v0--manifest-path)))

(defun superchat-closed-loop-experience-correction-v0--assignments ()
  "Return frozen assignments when present."
  (plist-get
   (superchat-closed-loop-experience-correction-v0--read-json
    superchat-closed-loop-experience-correction-v0--assignments-path)
   :assignments))

(unless (fboundp
         'superchat-closed-loop-experience-correction-v0--task-question)
  (error "Load closed-loop run.el before evidence.el"))

(defun superchat-closed-loop-experience-correction-v0--parse-evaluator-json
    (raw)
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

(defun superchat-closed-loop-experience-correction-v0--quality-decision
    (scores)
  "Return unchanged research-quality primary outcome for criterion SCORES."
  (cond ((cl-some #'zerop scores) "reject")
        ((cl-every (lambda (score) (= score 2)) scores) "accept")
        (t "revise")))

(defun superchat-closed-loop-experience-correction-v0--valid-quality-p
    (quality)
  "Return non-nil when QUALITY satisfies research-quality-v0."
  (let* ((criteria '(:evidence_completeness :citation_quality
                    :contradiction_handling))
         (items (mapcar (lambda (key) (plist-get quality key)) criteria))
         (scores (mapcar (lambda (item) (plist-get item :score)) items)))
    (and (equal "research-quality-v0" (plist-get quality :rubric_id))
         (cl-every (lambda (item)
                     (and (listp item)
                          (integerp (plist-get item :score))
                          (<= 0 (plist-get item :score) 2)
                          (stringp (plist-get item :evidence))))
                   items)
         (equal (superchat-closed-loop-experience-correction-v0--quality-decision
                 scores)
                (plist-get quality :primary_outcome))
         (listp (plist-get quality :missing_evidence)))))

(defun superchat-closed-loop-experience-correction-v0--valid-correction-p
    (correction)
  "Return non-nil when CORRECTION satisfies correction-fidelity-v0."
  (let* ((criterion (plist-get correction :correction_fidelity))
         (score (plist-get criterion :score)))
    (and (equal "correction-fidelity-v0" (plist-get correction :rubric_id))
         (integerp score)
         (<= 0 score 2)
         (stringp (plist-get criterion :evidence))
         (member (plist-get correction :primary_outcome)
                 '("fidelity-pass" "fidelity-partial" "fidelity-fail"))
         (equal (cond ((= score 2) "fidelity-pass")
                      ((= score 1) "fidelity-partial")
                      (t "fidelity-fail"))
                (plist-get correction :primary_outcome))
         (listp (plist-get correction :missing_evidence)))))

(defun superchat-closed-loop-experience-correction-v0--valid-evaluation-p
    (evaluation)
  "Return non-nil when EVALUATION has both frozen rubric results."
  (and (listp evaluation)
       (not (eq t (plist-get evaluation :abstained)))
       (superchat-closed-loop-experience-correction-v0--valid-correction-p
        (plist-get evaluation :correction_fidelity))
       (superchat-closed-loop-experience-correction-v0--valid-quality-p
        (plist-get evaluation :research_quality))))

(defun superchat-closed-loop-experience-correction-v0--artifact-references
    (steps)
  "Return blind Artifact references from Workflow STEPS."
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

(defun superchat-closed-loop-experience-correction-v0--reviewer-artifact
    (view)
  "Return the final deliverable reviewer Artifact from Workflow VIEW."
  (when-let* ((reviewer (cl-find 'reviewer (plist-get view :steps)
                                 :key (lambda (step)
                                        (plist-get step :role)))))
    (cl-find-if
     (lambda (artifact)
       (and (eq 'deliver (plist-get artifact :delivery-decision))
            (plist-get artifact :available)))
     (plist-get reviewer :artifacts))))

(defun superchat-closed-loop-experience-correction-v0--blinded-input
    (assignment view reviewer-content)
  "Return evaluator input for ASSIGNMENT and VIEW without arm leakage."
  (list :task_input
        (superchat-closed-loop-experience-correction-v0--task-question
         assignment)
        :source_fact_block
        (or (plist-get assignment :source_fact_block)
            (plist-get
             (plist-get
              (superchat-closed-loop-experience-correction-v0--manifest)
              :correction_source)
             :source_rows))
        :reviewer_artifact reviewer-content
        :artifact_evidence_references
        (superchat-closed-loop-experience-correction-v0--artifact-references
         (plist-get view :steps))))

(defun superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
    (value)
  "Return non-nil when VALUE contains an evaluator-blinding forbidden key."
  (cond
   ((and (listp value) (keywordp (car value)))
    (or (cl-loop for (key _item) on value by #'cddr
                 thereis (memq key
                               superchat-closed-loop-experience-correction-v0--forbidden-blind-keys))
        (cl-loop for (_key item) on value by #'cddr
                 thereis
                 (superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
                  item))))
   ((listp value)
    (cl-some
     #'superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
     value))
   ((vectorp value)
    (cl-some
     #'superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
     (append value nil)))))

(defun superchat-closed-loop-experience-correction-v0--missing-outcome
    (assignment launch reason)
  "Return missing-data Outcome for ASSIGNMENT and LAUNCH with REASON."
  (list :outcome_record_id
        (format "%s-outcome" (plist-get assignment :assignment_id))
        :experiment_id superchat-closed-loop-experience-correction-v0--experiment-id
        :assignment_id (plist-get assignment :assignment_id)
        :task_id (or (plist-get assignment :task_id)
                     (plist-get assignment :source_task_id))
        :first_attempt_id (plist-get launch :first_attempt_id)
        :research_instance_id (plist-get launch :instance_id)
        :evaluation_mode "preregistered-missing-data-rule"
        :evaluation_status "pass"
        :outcome_closure "pass"
        :correction_fidelity
        (list :rubric_id "correction-fidelity-v0"
              :correction_fidelity
              (list :score 0 :evidence reason)
              :primary_outcome "fidelity-fail"
              :missing_evidence (vector reason))
        :research_quality
        (list :rubric_id "research-quality-v0"
              :evidence_completeness (list :score 0 :evidence reason)
              :citation_quality (list :score 0 :evidence reason)
              :contradiction_handling (list :score 0 :evidence reason)
              :primary_outcome "reject"
              :missing_evidence (vector reason))
        :correction_fidelity_score 0
        :research_quality_outcome "reject"
        :abstained json-false
        :evaluated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z")))

(defun superchat-closed-loop-experience-correction-v0--outcome-from-raw
    (assignment launch raw blinded-input)
  "Return Outcome Record for ASSIGNMENT, LAUNCH, RAW and BLINDED-INPUT."
  (let* ((evaluation
          (superchat-closed-loop-experience-correction-v0--parse-evaluator-json
           raw))
         (valid
          (superchat-closed-loop-experience-correction-v0--valid-evaluation-p
           evaluation))
         (blind-pass
          (not
           (superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
            blinded-input))))
    (list :outcome_record_id
          (format "%s-outcome" (plist-get assignment :assignment_id))
          :experiment_id superchat-closed-loop-experience-correction-v0--experiment-id
          :assignment_id (plist-get assignment :assignment_id)
          :task_id (or (plist-get assignment :task_id)
                       (plist-get assignment :source_task_id))
          :first_attempt_id (plist-get launch :first_attempt_id)
          :research_instance_id (plist-get launch :instance_id)
          :evaluation_mode "blind-model"
          :evaluator_id "closed-loop-qwen3.6-blind-evaluator-v0"
          :evaluator_model_id "qwen3.6:latest"
          :evaluation_status (if valid "pass" "invalid")
          :outcome_closure (if blind-pass "pass" "blocked")
          :evaluator_input_sha256
          (superchat-closed-loop-experience-correction-v0--sha256-string
           (json-encode blinded-input))
          :blinding
          (list :arm_visible json-false
                :cost_visible json-false
                :latency_visible json-false
                :claim_source_visible json-false
                :forbidden_key_present
                (if (superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
                     blinded-input)
                    t
                  json-false))
          :raw_response raw
          :evaluation evaluation
          :correction_fidelity_score
          (if valid
              (plist-get
               (plist-get (plist-get evaluation :correction_fidelity)
                          :correction_fidelity)
               :score)
            0)
          :research_quality_outcome
          (if valid
              (plist-get (plist-get evaluation :research_quality)
                         :primary_outcome)
            "reject")
          :abstained (if (eq t (plist-get evaluation :abstained)) t json-false)
          :evaluated_at (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))

(defun superchat-closed-loop-experience-correction-v0--evaluator-system-prompt ()
  "Return the frozen combined evaluator instruction."
  (concat
   "Blindly evaluate one downstream Research Artifact. Return only JSON with "
   "this exact shape: {\"abstained\":false,\"correction_fidelity\":{"
   "\"rubric_id\":\"correction-fidelity-v0\",\"correction_fidelity\":{"
   "\"score\":0,\"evidence\":\"...\"},\"primary_outcome\":"
   "\"fidelity-fail\",\"missing_evidence\":[]},\"research_quality\":{"
   "\"rubric_id\":\"research-quality-v0\",\"evidence_completeness\":{"
   "\"score\":0,\"evidence\":\"...\"},\"citation_quality\":{"
   "\"score\":0,\"evidence\":\"...\"},\"contradiction_handling\":{"
   "\"score\":0,\"evidence\":\"...\"},\"primary_outcome\":\"reject\","
   "\"missing_evidence\":[]}}. Use only the supplied task, six Tape facts, "
   "Artifact, and Artifact references. Tape references are the citations for "
   "this bounded task. Do not infer the hidden comparison condition.\n\n"
   "Frozen correction rubric:\n"
   (with-temp-buffer
     (insert-file-contents
      superchat-closed-loop-experience-correction-v0--correction-rubric-path)
     (buffer-string))
   "\nFrozen research-quality rubric:\n"
   (with-temp-buffer
     (insert-file-contents
      superchat-closed-loop-experience-correction-v0--research-rubric-path)
     (buffer-string))))

(defun superchat-closed-loop-experience-correction-v0--evaluate
    (assignment launch view)
  "Write and return the blinded Outcome for ASSIGNMENT, LAUNCH and VIEW."
  (let* ((assignment-id (plist-get assignment :assignment_id))
         (directory
          (superchat-closed-loop-experience-correction-v0--run-directory
           assignment-id))
         (path (expand-file-name "outcome-record.json" directory)))
    (if (file-readable-p path)
        (superchat-closed-loop-experience-correction-v0--read-json path)
      (let* ((artifact
              (superchat-closed-loop-experience-correction-v0--reviewer-artifact
               view))
             (outcome
              (if (null artifact)
                  (superchat-closed-loop-experience-correction-v0--missing-outcome
                   assignment launch "final_reviewer_artifact_missing")
                (let* ((content
                        (superchat-workspace-read
                         (plist-get artifact :artifact-id)
                         (plist-get artifact :workspace-id)
                         (plist-get artifact :revision)))
                       (blind
                        (superchat-closed-loop-experience-correction-v0--blinded-input
                         assignment view content))
                       (session-id
                        (format "closed-loop-evaluator-%s" assignment-id))
                       raw)
                  (condition-case err
                      (progn
                        (unless (fboundp 'make-llm-ollama)
                          (error "llm-ollama is unavailable"))
                        (let ((superchat-llm-backend
                               (make-llm-ollama :chat-model "qwen3.6:latest"))
                              (superchat-llm-model nil)
                              (superchat-llm-reasoning 'none))
                          (setq raw
                                (superchat-runtime-submit-blocking
                                 (list :type :llm-query
                                       :session-id session-id
                                       :actor-id "blind-evaluator"
                                       :correlation-id
                                       (plist-get launch :instance_id)
                                       :input "Blind correction evaluation"
                                       :prompt (json-encode blind)
                                       :system-prompt
                                       (superchat-closed-loop-experience-correction-v0--evaluator-system-prompt)
                                       :tools 'none
                                       :record-conversation nil
                                       :authority-policy "default"
                                       :topology 'single)
                                 7200))))
                    (error
                     (setq raw
                           (format "[Evaluator error: %s]"
                                   (error-message-string err)))))
                  (superchat-closed-loop-experience-correction-v0--outcome-from-raw
                   assignment launch raw blind)))))
        (superchat-closed-loop-experience-correction-v0--write-json path outcome)
        outcome))))

(defun superchat-closed-loop-experience-correction-v0--prompt-snapshots
    (assignment-id)
  "Return persisted model-facing prompts for ASSIGNMENT-ID."
  (when (file-readable-p superchat-closed-loop-experience-correction-v0--prompt-record-path)
    (cl-remove-if-not
     (lambda (snapshot)
       (equal assignment-id (plist-get snapshot :assignment_id)))
     (plist-get
      (superchat-closed-loop-experience-correction-v0--read-json
       superchat-closed-loop-experience-correction-v0--prompt-record-path)
      :snapshots))))

(defun superchat-closed-loop-experience-correction-v0--prompt-arm-pass-p
    (assignment prompts)
  "Return non-nil when PROMPTS prove ASSIGNMENT's exact Claim arm."
  (let* ((arm (plist-get assignment :arm))
         (manifest
          (superchat-closed-loop-experience-correction-v0--manifest))
         (statements (plist-get (plist-get manifest :correction_source)
                                :statements))
         (stale (plist-get statements :superseded_claim))
         (current (plist-get statements :replacement_claim))
         (roles (mapcar (lambda (item) (plist-get item :role)) prompts)))
    (and (= 3 (length prompts))
         (equal '("analyst" "collector" "reviewer")
                (sort (copy-sequence roles) #'string<))
         (cl-every
          (lambda (snapshot)
            (let ((prompt (plist-get snapshot :model_prompt)))
              (and (equal arm (plist-get snapshot :arm))
                   (stringp prompt)
                   (string-match-p "BEGIN PRIOR CLAIM BLOCK" prompt)
                   (pcase arm
                     ("no-claim"
                      (and (not (string-match-p (regexp-quote stale) prompt))
                           (not (string-match-p (regexp-quote current) prompt))))
                     ("stale-claim"
                      (and (string-match-p (regexp-quote stale) prompt)
                           (not (string-match-p (regexp-quote current) prompt))))
                     ("corrected-current"
                      (and (string-match-p (regexp-quote current) prompt)
                           (not (string-match-p (regexp-quote stale) prompt))))
                     (_ nil)))))
          prompts))))

(defun superchat-closed-loop-experience-correction-v0--execution-summary
    (step)
  "Return model-free execution summary for Workflow STEP."
  (let* ((actor-id (plist-get step :actor-id))
         (run-id (plist-get step :run-id))
         (trace (and actor-id (superchat-tape-replay-events actor-id)))
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
             (and (equal actor-id (plist-get event :actor-id))
                  (stringp (plist-get event :turn-id))))
           tool-events)))
    (list :role (symbol-name (plist-get step :role))
          :actor_id actor-id
          :run_id run-id
          :trace_verdict
          (or (plist-get step :trace-verdict)
              (and well-formed (symbol-name (plist-get well-formed :verdict))))
          :terminal_verdict
          (or (plist-get step :terminal-verdict)
              (and terminal (symbol-name (plist-get terminal :verdict))))
          :tool_identity
          (or (plist-get step :tool-identity)
              (cond ((null tool-events) "not-applicable")
                ((zerop invalid-tool-identity) "pass")
                (t "invalid")))
          :run_decision
          (when-let* ((decision (plist-get step :run-decision)))
            (symbol-name decision))
          :resume_decision
          (when-let* ((decision (plist-get step :resume-decision)))
            (symbol-name decision))
          :terminal_kind
          (or (plist-get step :terminal-kind)
              (and (eq 'completed (plist-get step :resume-decision))
                   "completed"))
          :first_attempt_retained
          (if (plist-get step :replacement_attempt_id) json-false t))))

(defun superchat-closed-loop-experience-correction-v0--artifact-summary
    (step)
  "Return Artifact closure summary for Workflow STEP."
  (let* ((artifacts (plist-get step :artifacts))
         (deliverable
          (cl-remove-if-not
           (lambda (artifact)
             (and (eq 'deliver (plist-get artifact :delivery-decision))
                  (plist-get artifact :available)))
           artifacts)))
    (list :role (symbol-name (plist-get step :role))
          :classification
          (cond ((null artifacts) "artifact_absent")
                ((= 1 (length deliverable)) "artifact_closure_pass")
                (t "artifact_present_provenance_invalid"))
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

(defun superchat-closed-loop-experience-correction-v0--build-evidence
    (assignment launch view outcome)
  "Write and return evidence for ASSIGNMENT, LAUNCH, VIEW and OUTCOME."
  (let* ((assignment-id (plist-get assignment :assignment_id))
         (steps (plist-get view :steps))
         (prompts
          (superchat-closed-loop-experience-correction-v0--prompt-snapshots
           assignment-id))
         (execution
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--execution-summary
           steps))
         (artifacts
          (mapcar
           #'superchat-closed-loop-experience-correction-v0--artifact-summary
           steps))
         (prior-pass
          (and (stringp assignment-id)
               (superchat-closed-loop-experience-correction-v0--prompt-arm-pass-p
                assignment prompts)
               (not (plist-get launch :replacement_attempt_id))))
         (execution-pass
          (and (= 3 (length execution))
               (cl-every
                (lambda (item)
                  (and (plist-get item :run_id)
                       (equal "valid" (plist-get item :trace_verdict))
                       (equal "proved" (plist-get item :terminal_verdict))
                       (not (equal "invalid" (plist-get item :tool_identity)))
                       (not (eq json-false
                                (plist-get item :first_attempt_retained)))))
                execution)))
         (artifact-pass
          (and (= 3 (length artifacts))
               (cl-every
           (lambda (item)
             (equal "artifact_closure_pass"
                    (plist-get item :classification)))
           artifacts)))
         (outcome-pass (equal "pass" (plist-get outcome :outcome_closure)))
         (comparison-pass
          (and outcome-pass
               (integerp (plist-get outcome :correction_fidelity_score))
               (<= 0 (plist-get outcome :correction_fidelity_score) 2)
               (member (plist-get outcome :research_quality_outcome)
                       '("accept" "revise" "reject"))))
         (overall
          (and prior-pass execution-pass artifact-pass outcome-pass
               comparison-pass))
         (evidence
          (list
           :experiment_id superchat-closed-loop-experience-correction-v0--experiment-id
           :assignment_id assignment-id
           :task_id (or (plist-get assignment :task_id)
                        (plist-get assignment :source_task_id))
           :run_index (plist-get launch :run_index)
           :first_attempt_id (plist-get launch :first_attempt_id)
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
            (list :name "comparison-readiness"
                  :verdict (if comparison-pass "pass" "blocked")))
           :evidence_closure (if overall "pass" "blocked")
           :first_attempt_rule "failures-retained-no-rerun-substitution"
           :prompt_snapshots (vconcat prompts)
           :execution (vconcat execution)
           :artifacts (vconcat artifacts)
           :outcome_record
           (format "runs/%s/outcome-record.json" assignment-id)
           :correction_fidelity_score
           (plist-get outcome :correction_fidelity_score)
           :research_quality_outcome
           (plist-get outcome :research_quality_outcome)
           :effectiveness_claim "not_evaluated"
           :learning_claim "prohibited")))
    (superchat-closed-loop-experience-correction-v0--write-json
     (expand-file-name
      "evidence-bundle.json"
      (superchat-closed-loop-experience-correction-v0--run-directory
       assignment-id))
     evidence)
    evidence))

(defun superchat-closed-loop-experience-correction-v0--fixture-view
    (&optional missing-artifact)
  "Return a model-free Workflow view fixture.
When MISSING-ARTIFACT is non-nil, omit the deliverable Artifact."
  (list :status 'completed
        :steps
        (list
         (list :role 'collector :actor-id "actor-c" :run-id "run-c"
               :trace-verdict "valid" :terminal-verdict "proved"
               :tool-identity "not-applicable"
               :run-decision 'deliver :resume-decision 'advance
               :artifacts
               (list (list :artifact-id "artifact-c" :revision 1
                           :uri "workspace://artifact-c/1"
                           :delivery-decision 'deliver :available t)))
         (list :role 'analyst :actor-id "actor-a" :run-id "run-a"
               :trace-verdict "valid" :terminal-verdict "proved"
               :tool-identity "not-applicable"
               :run-decision 'deliver :resume-decision 'advance
               :artifacts
               (list (list :artifact-id "artifact-a" :revision 1
                           :uri "workspace://artifact-a/1"
                           :delivery-decision 'deliver :available t)))
         (list :role 'reviewer :actor-id "actor-r" :run-id "run-r"
               :trace-verdict "valid" :terminal-verdict "proved"
               :tool-identity "not-applicable"
               :run-decision 'deliver :resume-decision 'advance
               :artifacts
               (unless missing-artifact
                 (list (list :artifact-id "artifact-r" :revision 1
                             :uri "workspace://artifact-r/1"
                             :delivery-decision 'deliver :available t)))))))

(defun superchat-closed-loop-experience-correction-v0--fixture-raw
    (score)
  "Return evaluator JSON fixture for correction-fidelity SCORE."
  (json-encode
   (list :abstained json-false
         :correction_fidelity
         (list :rubric_id "correction-fidelity-v0"
               :correction_fidelity
               (list :score score :evidence "fixture evidence")
               :primary_outcome
               (cond ((= score 2) "fidelity-pass")
                     ((= score 1) "fidelity-partial")
                     (t "fidelity-fail"))
               :missing_evidence [])
         :research_quality
         (list :rubric_id "research-quality-v0"
               :evidence_completeness
               (list :score 2 :evidence "fixture evidence")
               :citation_quality
               (list :score 2 :evidence "fixture evidence")
               :contradiction_handling
               (list :score 2 :evidence "fixture evidence")
               :primary_outcome "accept"
               :missing_evidence []))))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v0-self-check ()
  "Run model-free validation fixtures before live execution."
  (interactive)
  (let* ((temp-directory
          (file-name-as-directory
           (make-temp-file
            "superchat-closed-loop-experience-correction-v0-" t)))
         (superchat-closed-loop-experience-correction-v0--directory
          temp-directory)
         (superchat-closed-loop-experience-correction-v0--prompt-record-path
          (expand-file-name "prompt-snapshots.json" temp-directory))
         (assignment
          (list :assignment_id "fixture-a1" :task_id "fixture-task"
                :arm "corrected-current" :question "Fixture question"))
         (launch
          (list :run_index 1 :first_attempt_id "attempt-1"
                :instance_id "instance-1"))
         (_prompt-snapshots
          (let* ((manifest
                  (superchat-closed-loop-experience-correction-v0--manifest))
                 (current
                  (plist-get
                   (plist-get (plist-get manifest :correction_source)
                              :statements)
                   :replacement_claim))
                 (model-prompt
                  (format "BEGIN PRIOR CLAIM BLOCK\n%s\nEND PRIOR CLAIM BLOCK"
                          current)))
            (superchat-closed-loop-experience-correction-v0--write-json
             superchat-closed-loop-experience-correction-v0--prompt-record-path
             (list :snapshots
                   (vconcat
                    (mapcar
                     (lambda (role)
                       (list :assignment_id "fixture-a1"
                             :role role
                             :arm "corrected-current"
                             :model_prompt model-prompt))
                     '("collector" "analyst" "reviewer")))))))
         (view
          (superchat-closed-loop-experience-correction-v0--fixture-view))
         (blind
          (superchat-closed-loop-experience-correction-v0--blinded-input
           assignment view "Reviewer artifact text"))
         (outcome-2
          (superchat-closed-loop-experience-correction-v0--outcome-from-raw
           assignment launch
           (superchat-closed-loop-experience-correction-v0--fixture-raw 2)
           blind))
         (outcome-1
          (superchat-closed-loop-experience-correction-v0--outcome-from-raw
           assignment launch
           (superchat-closed-loop-experience-correction-v0--fixture-raw 1)
           blind))
         (outcome-0
          (superchat-closed-loop-experience-correction-v0--outcome-from-raw
           assignment launch
           (superchat-closed-loop-experience-correction-v0--fixture-raw 0)
           blind))
         (missing
          (superchat-closed-loop-experience-correction-v0--missing-outcome
           assignment launch "final_reviewer_artifact_missing"))
         (missing-artifact-evidence
          (superchat-closed-loop-experience-correction-v0--build-evidence
           assignment launch
           (superchat-closed-loop-experience-correction-v0--fixture-view t)
           missing))
         (abstention
          (superchat-closed-loop-experience-correction-v0--outcome-from-raw
           assignment launch "{\"abstained\":true}" blind))
         (malformed
          (superchat-closed-loop-experience-correction-v0--outcome-from-raw
           assignment launch "not json" blind))
         (evidence
          (superchat-closed-loop-experience-correction-v0--build-evidence
           assignment launch view outcome-2)))
    (cl-assert (not
                (superchat-closed-loop-experience-correction-v0--contains-forbidden-key-p
                 blind)))
    (cl-assert (= 2 (plist-get outcome-2 :correction_fidelity_score)))
    (cl-assert (= 1 (plist-get outcome-1 :correction_fidelity_score)))
    (cl-assert (= 0 (plist-get outcome-0 :correction_fidelity_score)))
    (cl-assert (equal "accept"
                      (plist-get outcome-2 :research_quality_outcome)))
    (cl-assert (equal "pass" (plist-get missing :outcome_closure)))
    (cl-assert (= 0 (plist-get
                     (plist-get (plist-get missing :correction_fidelity)
                                :correction_fidelity)
                     :score)))
    (cl-assert (equal "blocked"
                      (plist-get missing-artifact-evidence :evidence_closure)))
    (cl-assert (equal "pass" (plist-get abstention :outcome_closure)))
    (cl-assert (= 0 (plist-get abstention :correction_fidelity_score)))
    (cl-assert (equal "pass" (plist-get malformed :outcome_closure)))
    (cl-assert (= 0 (plist-get malformed :correction_fidelity_score)))
    (cl-assert (equal "pass" (plist-get evidence :evidence_closure)))
    (message "closed-loop evidence self-check passed")
    t))

(provide 'superchat-closed-loop-experience-correction-v0-evidence)

;;; evidence.el ends here
