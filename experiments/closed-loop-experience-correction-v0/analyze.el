;;; analyze.el --- Closed-loop correction comparison -*- lexical-binding: t; -*-

;; Usage:
;;   emacs -Q --batch -l experiments/closed-loop-experience-correction-v0/analyze.el -- --self-check
;;   emacs -Q --batch -l experiments/closed-loop-experience-correction-v0/analyze.el

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst sc-clec--experiment-id "closed-loop-experience-correction-v0")
(defconst sc-clec--arms '("no-claim" "stale-claim" "corrected-current"))
(defconst sc-clec--task-count 8)
(defconst sc-clec--record-count 24)
(defconst sc-clec--primary-rubric-id "correction-fidelity-v0")
(defconst sc-clec--quality-rubric-id "research-quality-v0")
(defconst sc-clec--default-prohibited-claims
  '("Learning occurred."
    "The correction caused model recovery."
    "The correction was promoted to Memory."
    "The correction changed Policy or Authority."
    "The result generalizes beyond the eight preregistered tasks."
    "The stale arm is a product default."
    "A rerun may replace a failed first attempt."))

(defun sc-clec--die (format-string &rest args)
  (princ (apply #'format (concat "analyze.el: " format-string "\n") args))
  (kill-emacs 1))

(defun sc-clec--path (&rest parts)
  (mapconcat #'identity parts "/"))

(defun sc-clec--experiment-dir ()
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(defun sc-clec--read-json (path)
  (unless (file-readable-p path)
    (signal 'user-error (list (format "missing JSON file: %s" path))))
  (with-temp-buffer
    (insert-file-contents path)
    (json-parse-buffer :object-type 'alist :array-type 'list
                       :null-object nil :false-object :false)))

(defun sc-clec--json-key (key)
  (if (symbolp key) (symbol-name key) key))

(defun sc-clec--get (object &rest keys)
  (let ((value object))
    (dolist (key keys value)
      (setq value
            (cond
             ((hash-table-p value) (gethash (sc-clec--json-key key) value))
             ((and (listp value) (consp (car value)))
              (or (alist-get key value nil nil #'equal)
                  (alist-get (sc-clec--json-key key) value nil nil #'equal)
                  (alist-get (intern (sc-clec--json-key key)) value nil nil #'eq)))
             ((and (listp value) (keywordp (car value)))
              (plist-get value (intern (concat ":" (sc-clec--json-key key)))))
             (t nil))))))

(defun sc-clec--truthy-p (value)
  (and value (not (eq value :false))))

(defun sc-clec--string-value (value)
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (value (format "%s" value))
   (t nil)))

(defun sc-clec--bool-pass-p (value)
  (or (eq value t)
      (member (sc-clec--string-value value)
              '("pass" "passed" "proved" "valid" "complete" "completed" "deliver"))))

(defun sc-clec--proof-pass-p (object paths)
  (sc-clec--bool-pass-p (sc-clec--lookup-in object paths)))

(defun sc-clec--named-gate-pass-p (evidence name)
  (cl-some (lambda (gate)
             (and (equal name (sc-clec--string-value (sc-clec--get gate 'name)))
                  (sc-clec--bool-pass-p (or (sc-clec--get gate 'verdict)
                                            (sc-clec--get gate 'status)
                                            (sc-clec--get gate 'closure)))))
           (sc-clec--as-list (sc-clec--get evidence 'gates))))

(defun sc-clec--named-gate-present-p (evidence name)
  (cl-find-if (lambda (gate)
                (equal name (sc-clec--string-value
                             (sc-clec--get gate 'name))))
              (sc-clec--as-list (sc-clec--get evidence 'gates))))

(defun sc-clec--num (value field)
  (let ((number (cond
                 ((numberp value) value)
                 ((stringp value) (string-to-number value))
                 (t nil))))
    (unless (and number (<= 0 number))
      (signal 'user-error (list (format "missing numeric %s" field))))
    number))

(defun sc-clec--as-list (value)
  (cond
   ((null value) nil)
   ((vectorp value) (append value nil))
   ((listp value) value)
   (t (list value))))

(defun sc-clec--lookup-in (object keys)
  (catch 'found
    (dolist (path keys)
      (let ((value (apply #'sc-clec--get object path)))
        (when value (throw 'found value))))))

(defun sc-clec--load-assignments (dir)
  (let* ((path (sc-clec--path dir "assignments.json"))
         (json (sc-clec--read-json path))
         (items (or (sc-clec--get json 'assignments)
                    (sc-clec--get json 'records)
                    (sc-clec--get json 'items)
                    json)))
    (sc-clec--as-list items)))

(defun sc-clec--evidence-path (dir assignment-id)
  (sc-clec--path dir "runs" assignment-id "evidence-bundle.json"))

(defun sc-clec--outcome-path (dir assignment-id)
  (sc-clec--path dir "runs" assignment-id "outcome-record.json"))

(defun sc-clec--read-run-file (dir assignment-id file-kind)
  (condition-case err
      (pcase file-kind
        ('evidence (sc-clec--read-json (sc-clec--evidence-path dir assignment-id)))
        ('outcome (sc-clec--read-json (sc-clec--outcome-path dir assignment-id))))
    (user-error (signal 'user-error
                        (list (format "%s for %s" (cadr err) assignment-id))))))

(defun sc-clec--assignment-id (assignment)
  (or (sc-clec--string-value (sc-clec--get assignment 'assignment_id))
      (sc-clec--string-value (sc-clec--get assignment 'id))
      (signal 'user-error (list "assignment without assignment_id"))))

(defun sc-clec--task-id (assignment)
  (or (sc-clec--string-value (sc-clec--get assignment 'task_id))
      (sc-clec--string-value (sc-clec--get assignment 'source_task_id))
      (signal 'user-error (list (format "%s missing task_id" (sc-clec--assignment-id assignment))))))

(defun sc-clec--arm (assignment)
  (let ((arm (or (sc-clec--string-value (sc-clec--get assignment 'arm))
                 (sc-clec--string-value (sc-clec--get assignment 'claim_arm)))))
    (unless (member arm sc-clec--arms)
      (signal 'user-error (list (format "%s invalid arm %S"
                                        (sc-clec--assignment-id assignment) arm))))
    arm))

(defun sc-clec--first-attempt-p (assignment evidence outcome)
  (and (not (sc-clec--truthy-p (sc-clec--lookup-in assignment
                                                 '((rerun) (rerun_of) (substitutes_assignment_id)))))
       (not (sc-clec--truthy-p (sc-clec--lookup-in evidence
                                                 '((rerun) (rerun_of) (substitutes_assignment_id)))))
       (not (sc-clec--truthy-p (sc-clec--lookup-in outcome
                                                 '((rerun) (rerun_of) (substitutes_assignment_id)))))
       (let ((evidence-id
              (sc-clec--string-value
               (sc-clec--get evidence 'first_attempt_id)))
             (outcome-id
              (sc-clec--string-value
               (sc-clec--get outcome 'first_attempt_id))))
         (and evidence-id outcome-id (equal evidence-id outcome-id)))))

(defun sc-clec--closed-evidence-p (evidence outcome)
  (and (or (member (sc-clec--string-value
                    (sc-clec--get evidence 'evidence_closure))
                   '("pass" "blocked"))
           (cl-every (lambda (key) (sc-clec--get evidence key))
                     '(comparison_readiness prior_gate execution_gate
                       artifact_gate)))
       (or (cl-every (lambda (name)
                       (sc-clec--named-gate-present-p evidence name))
                     '("prior" "execution" "artifact" "outcome"
                       "comparison-readiness"))
           (cl-every (lambda (key) (sc-clec--get evidence key))
                     '(comparison_readiness prior_gate execution_gate
                       artifact_gate)))
       (sc-clec--proof-pass-p outcome
                              '((outcome_closure)
                                (evaluation_status)
                                (gates outcome)
                                (gates outcome status)))))

(defun sc-clec--prompt-arms (evidence)
  (let (arms)
    (dolist (snapshot (sc-clec--as-list (sc-clec--get evidence 'prompt_snapshots))
                      (nreverse arms))
      (when-let* ((arm (sc-clec--string-value (sc-clec--get snapshot 'arm))))
        (push arm arms)))))

(defun sc-clec--observed-arm (evidence outcome)
  (delete-dups
   (append (sc-clec--prompt-arms evidence)
           (delq nil
                 (mapcar #'sc-clec--string-value
                         (list (sc-clec--lookup-in evidence
                                                   '((arm) (claim_arm) (prompt_claim_arm)
                                                     (claim_block_arm)
                                                     (consumption claim_arm)
                                                     (consumption arm)))
                               (sc-clec--lookup-in outcome
                                                   '((arm) (claim_arm)))))))))

(defun sc-clec--arm-drift-p (arm evidence outcome)
  (cl-some (lambda (seen) (and seen (not (equal seen arm))))
           (sc-clec--observed-arm evidence outcome)))

(defun sc-clec--task-drift-p (task-id evidence outcome)
  (cl-some (lambda (seen) (and seen (not (equal seen task-id))))
           (delq nil
                 (mapcar #'sc-clec--string-value
                         (list (sc-clec--lookup-in evidence '((task_id) (source_task_id)))
                               (sc-clec--lookup-in outcome '((task_id) (source_task_id))))))))

(defun sc-clec--assignment-drift-p (assignment-id evidence outcome)
  (cl-some (lambda (seen) (and seen (not (equal seen assignment-id))))
           (delq nil
                 (mapcar #'sc-clec--string-value
                         (list (sc-clec--get evidence 'assignment_id)
                               (sc-clec--get outcome 'assignment_id))))))

(defun sc-clec--evaluator-id (outcome)
  (or (sc-clec--string-value (sc-clec--get outcome 'evaluator_id))
      (sc-clec--string-value (sc-clec--get outcome 'correction_fidelity_evaluator_id))
      (sc-clec--string-value (sc-clec--get outcome 'research_quality_evaluator_id))))

(defun sc-clec--primary-score (outcome)
  (let ((score (sc-clec--lookup-in
                outcome
                '((correction_fidelity score)
                  (correction_fidelity_score)
                  (primary_score)
                  (evaluation correction_fidelity score)
                  (evaluation correction_fidelity_score)))))
    (sc-clec--num score "correction-fidelity score")))

(defun sc-clec--quality-score (outcome)
  (let ((explicit (sc-clec--lookup-in
                   outcome
                   '((research_quality score)
                     (research_quality_score)
                     (evaluation research_quality score)
                     (evaluation research_quality_score)))))
    (if explicit
        (sc-clec--num explicit "research-quality score")
      (let ((outcome-label (sc-clec--string-value
                            (sc-clec--lookup-in
                             outcome
                             '((research_quality_outcome)
                               (research_quality primary_outcome)
                               (evaluation research_quality primary_outcome))))))
        (cond
         ((equal outcome-label "accept") 2)
         ((equal outcome-label "revise") 1)
         ((equal outcome-label "reject") 0)
         (t
          (+ (sc-clec--num (sc-clec--lookup-in
                            outcome
                            '((research_quality evidence_completeness score)
                              (evaluation evidence_completeness score)
                              (evaluation research_quality evidence_completeness score)))
                           "research-quality evidence_completeness")
             (sc-clec--num (sc-clec--lookup-in
                            outcome
                            '((research_quality citation_quality score)
                              (evaluation citation_quality score)
                              (evaluation research_quality citation_quality score)))
                           "research-quality citation_quality")
             (sc-clec--num (sc-clec--lookup-in
                            outcome
                            '((research_quality contradiction_handling score)
                              (evaluation contradiction_handling score)
                              (evaluation research_quality contradiction_handling score)))
                           "research-quality contradiction_handling"))))))))

(defun sc-clec--counterexample (task-id kind corrected other corrected-score other-score)
  `((task_id . ,task-id)
    (kind . ,kind)
    (corrected_current_assignment_id . ,(alist-get 'assignment_id corrected nil nil #'equal))
    (comparison_assignment_id . ,(alist-get 'assignment_id other nil nil #'equal))
    (corrected_current_score . ,corrected-score)
    (comparison_score . ,other-score)))

(defun sc-clec--record (dir assignment)
  (let* ((assignment-id (sc-clec--assignment-id assignment))
         (task-id (sc-clec--task-id assignment))
         (arm (sc-clec--arm assignment))
         (evidence (or (sc-clec--get assignment 'evidence)
                       (sc-clec--read-run-file dir assignment-id 'evidence)))
         (outcome (or (sc-clec--get assignment 'outcome)
                      (sc-clec--read-run-file dir assignment-id 'outcome))))
    (when (sc-clec--assignment-drift-p assignment-id evidence outcome)
      (signal 'user-error (list (format "%s assignment drift" assignment-id))))
    (when (sc-clec--task-drift-p task-id evidence outcome)
      (signal 'user-error (list (format "%s task drift" assignment-id))))
    (when (sc-clec--arm-drift-p arm evidence outcome)
      (signal 'user-error (list (format "%s arm drift" assignment-id))))
    (unless (member arm (sc-clec--observed-arm evidence outcome))
      (signal 'user-error (list (format "%s missing model-facing arm proof" assignment-id))))
    (unless (sc-clec--first-attempt-p assignment evidence outcome)
      (signal 'user-error (list (format "%s is rerun/substitution, not first attempt" assignment-id))))
    (unless (sc-clec--closed-evidence-p evidence outcome)
      (signal 'user-error (list (format "%s has incomplete evidence/outcome closure" assignment-id))))
    `((assignment_id . ,assignment-id)
      (task_id . ,task-id)
      (arm . ,arm)
      (evidence_closure . ,(sc-clec--string-value
                            (sc-clec--get evidence 'evidence_closure)))
      (consumption_complete
       . ,(and (= 3 (length (sc-clec--prompt-arms evidence)))
               (sc-clec--named-gate-pass-p evidence "prior")))
      (primary_score . ,(sc-clec--primary-score outcome))
      (research_quality_score . ,(sc-clec--quality-score outcome))
      (evaluator_id . ,(sc-clec--evaluator-id outcome))
      (counterexample_note . ,(or (sc-clec--string-value
                                   (sc-clec--lookup-in outcome
                                                       '((counterexample_note)
                                                         (correction_fidelity evidence)
                                                         (evaluation correction_fidelity evidence))))
                                  "")))))

(defun sc-clec--group-records (records)
  (let ((by-assignment (make-hash-table :test #'equal))
        (by-task (make-hash-table :test #'equal)))
    (dolist (record records)
      (let ((assignment-id (alist-get 'assignment_id record nil nil #'equal))
            (task-id (alist-get 'task_id record nil nil #'equal))
            (arm (alist-get 'arm record nil nil #'equal)))
        (when (gethash assignment-id by-assignment)
          (signal 'user-error (list (format "duplicate assignment_id %s" assignment-id))))
        (puthash assignment-id t by-assignment)
        (let ((arms (or (gethash task-id by-task) (make-hash-table :test #'equal))))
          (when (gethash arm arms)
            (signal 'user-error (list (format "duplicate arm %s for task %s" arm task-id))))
          (puthash arm record arms)
          (puthash task-id arms by-task))))
    (unless (= (length records) sc-clec--record-count)
      (signal 'user-error (list (format "expected %d records, got %d"
                                        sc-clec--record-count (length records)))))
    (unless (= (hash-table-count by-task) sc-clec--task-count)
      (signal 'user-error (list (format "expected %d tasks, got %d"
                                        sc-clec--task-count (hash-table-count by-task)))))
    (maphash (lambda (task-id arms)
               (dolist (arm sc-clec--arms)
                 (unless (gethash arm arms)
                   (signal 'user-error
                           (list (format "missing %s arm for task %s"
                                         arm task-id))))))
             by-task)
    by-task))

(defun sc-clec--manifest (dir)
  (let ((path (sc-clec--path dir "manifest.json")))
    (when (file-readable-p path) (sc-clec--read-json path))))

(defun sc-clec--manifest-pass-p (manifest keys)
  (and manifest
       (cl-every (lambda (key)
                   (sc-clec--bool-pass-p (sc-clec--lookup-in manifest (list key))))
                 keys)))

(defun sc-clec--boundary-pass-p (manifest)
  (or (null manifest)
      (and (not (sc-clec--truthy-p (sc-clec--lookup-in
                                    manifest '((product_semantics_changed)
                                               (memory_changed)
                                               (runtime_changed)
                                               (authority_changed)
                                               (policy_changed)
                                               (learning_claimed)
                                               (boundary_rules product_edits_observed)
                                               (boundary_rules memory_changed)
                                               (boundary_rules runtime_changed)
                                               (boundary_rules authority_changed)
                                               (boundary_rules policy_changed)))))
           (not (equal (sc-clec--lookup-in manifest '((held_out_accessed) (held_out_used)))
                       t)))))

(defun sc-clec--persistence-pass-p (record)
  (and record
       (sc-clec--truthy-p (sc-clec--get record 'history_retained))
       (equal "pass" (sc-clec--string-value
                      (sc-clec--get record 'restart_replay)))
       (equal "ok" (sc-clec--string-value
                    (sc-clec--get record 'sqlite_integrity_check)))
       (>= (length (sc-clec--as-list
                    (sc-clec--get record 'view 'claims))) 2)
       (>= (length (sc-clec--as-list
                    (sc-clec--get record 'view 'feedback))) 1)))

(defun sc-clec--correction-pass-p (record)
  (let ((original (sc-clec--string-value
                   (sc-clec--get record 'original_claim_id)))
        (replacement (sc-clec--string-value
                      (sc-clec--get record 'replacement_claim_id)))
        (current (sc-clec--string-value
                  (sc-clec--get record 'current_claim_id))))
    (and original replacement current
         (not (equal original replacement))
         (equal replacement current))))

(defun sc-clec--analyze-records (records &optional manifest correction-record)
  (let* ((by-task (sc-clec--group-records records))
         (evaluator nil)
         (stale-wins 0)
         (no-claim-wins 0)
         (quality-worse 0)
         (all-corrected-two t)
         (matrix nil)
         (counterexamples nil))
    (maphash
     (lambda (task-id arms)
       (let* ((no (gethash "no-claim" arms))
              (stale (gethash "stale-claim" arms))
              (corrected (gethash "corrected-current" arms))
              (cp (alist-get 'primary_score corrected nil nil #'equal))
              (sp (alist-get 'primary_score stale nil nil #'equal))
              (np (alist-get 'primary_score no nil nil #'equal))
              (cq (alist-get 'research_quality_score corrected nil nil #'equal))
              (nq (alist-get 'research_quality_score no nil nil #'equal)))
         (dolist (record (list no stale corrected))
           (let ((record-evaluator (alist-get 'evaluator_id record nil nil #'equal)))
             (when record-evaluator
               (if evaluator
                   (unless (equal evaluator record-evaluator)
                     (signal 'user-error
                             (list (format "evaluator identity drift: %s vs %s"
                                           evaluator record-evaluator))))
                 (setq evaluator record-evaluator)))))
         (unless (= cp 2)
           (setq all-corrected-two nil)
           (push (sc-clec--counterexample task-id "corrected-primary-not-2"
                                          corrected corrected cp 2)
                 counterexamples))
         (when (> cp sp) (cl-incf stale-wins))
         (unless (> cp sp)
           (push (sc-clec--counterexample task-id "not-strictly-better-than-stale"
                                          corrected stale cp sp)
                 counterexamples))
         (when (> cp np) (cl-incf no-claim-wins))
         (unless (> cp np)
           (push (sc-clec--counterexample task-id "not-strictly-better-than-no-claim"
                                          corrected no cp np)
                 counterexamples))
         (when (< cq nq)
           (cl-incf quality-worse)
           (push (sc-clec--counterexample task-id "research-quality-worse-than-no-claim"
                                          corrected no cq nq)
                 counterexamples))
         (push `((task_id . ,task-id)
                 (no_claim . ((assignment_id . ,(alist-get 'assignment_id no nil nil #'equal))
                              (primary_score . ,np)
                              (research_quality_score . ,nq)))
                 (stale_claim . ((assignment_id . ,(alist-get 'assignment_id stale nil nil #'equal))
                                 (primary_score . ,sp)
                                 (research_quality_score . ,(alist-get 'research_quality_score stale nil nil #'equal))))
                 (corrected_current . ((assignment_id . ,(alist-get 'assignment_id corrected nil nil #'equal))
                                       (primary_score . ,cp)
                                       (research_quality_score . ,cq))))
               matrix)))
     by-task)
    (let* ((effect-pass (and all-corrected-two
                             (>= stale-wins 6)
                             (>= no-claim-wins 3)
                             (<= quality-worse 1)))
           (persistence (if (sc-clec--persistence-pass-p correction-record)
                            "pass" "fail"))
           (correction (if (sc-clec--correction-pass-p correction-record)
                           "pass" "fail"))
           (consumption (if (cl-every
                             (lambda (record)
                               (alist-get 'consumption_complete record nil nil
                                          #'equal))
                             records)
                            "pass" "fail"))
           (closed-samples
            (cl-count "pass" records
                      :key (lambda (record)
                             (alist-get 'evidence_closure record nil nil #'equal))
                      :test #'equal))
           (boundary (if (sc-clec--boundary-pass-p manifest) "pass" "fail")))
      `((experiment_id . ,sc-clec--experiment-id)
        (status . ,(if effect-pass "effect_gate_pass" "effect_gate_fail"))
        (verdicts . ((persistence . ,persistence)
                     (correction . ,correction)
                     (consumption . ,consumption)
                     (effect . ,(if effect-pass "pass" "fail"))
                     (boundary . ,boundary)))
        (thresholds . ((corrected_primary_all_two
                        . ,(if all-corrected-two t json-false))
                       (evidence_closure_pass . ,closed-samples)
                       (evidence_closure_blocked . ,(- sc-clec--record-count
                                                      closed-samples))
                       (corrected_beats_stale_required . 6)
                       (corrected_beats_stale_actual . ,stale-wins)
                       (corrected_beats_no_claim_required . 3)
                       (corrected_beats_no_claim_actual . ,no-claim-wins)
                       (quality_worse_than_no_claim_allowed . 1)
                       (quality_worse_than_no_claim_actual . ,quality-worse)))
        (paired_score_matrix . ,(vconcat (sort matrix (lambda (a b)
                                                        (string< (alist-get 'task_id a nil nil #'equal)
                                                                 (alist-get 'task_id b nil nil #'equal))))))
        (counterexamples . ,(vconcat (nreverse counterexamples)))
        (prohibited_claims . ,(vconcat (or (sc-clec--get manifest 'prohibited_claims)
                                           sc-clec--default-prohibited-claims)))
        (allowed_effect_statement . ,(if effect-pass
                                         "In this preregistered eight-task set, corrected-current passed the correction-fidelity gate under the frozen paired comparison."
                                       "No effectiveness statement is allowed because the preregistered effect gate did not pass."))))))

(defun sc-clec--json-string (object)
  (let ((json-encoding-pretty-print t))
    (json-encode object)))

(defun sc-clec--write-json (path report)
  (with-temp-file path
    (insert (sc-clec--json-string report))
    (insert "\n")))

(defun sc-clec--apply-validity-erratum (report erratum)
  (if (not erratum)
      report
    (let* ((report (copy-tree report))
           (verdicts (alist-get 'verdicts report nil nil #'equal)))
      (setf (alist-get 'raw_status report nil nil #'equal)
            (alist-get 'status report nil nil #'equal)
            (alist-get 'status report nil nil #'equal) "invalid_input_contract"
            (alist-get 'consumption verdicts nil nil #'equal) "invalid"
            (alist-get 'effect verdicts nil nil #'equal) "invalid"
            (alist-get 'validity report nil nil #'equal) erratum
            (alist-get 'allowed_effect_statement report nil nil #'equal)
            "No consumption or effectiveness statement is allowed because the model-input contract was violated.")
      report)))

(defun sc-clec--write-markdown (path report)
  (let* ((verdicts (alist-get 'verdicts report nil nil #'equal))
         (thresholds (alist-get 'thresholds report nil nil #'equal))
         (validity (alist-get 'validity report nil nil #'equal))
         (counterexamples (append (alist-get 'counterexamples report nil nil #'equal) nil))
         (prohibited (append (alist-get 'prohibited_claims report nil nil #'equal) nil)))
    (with-temp-file path
      (insert "# Closed-loop Experience Correction v0 comparison\n\n")
      (insert (format "- Status: `%s`\n" (alist-get 'status report nil nil #'equal)))
      (dolist (name '(persistence correction consumption effect boundary))
        (insert (format "- %s verdict: `%s`\n"
                        (capitalize (symbol-name name))
                        (alist-get name verdicts nil nil #'equal))))
      (when validity
        (insert "\n## Validity\n\n")
        (insert (format "- Classification: `%s`\n"
                        (alist-get 'classification validity nil nil #'equal)))
        (insert (format "- %s\n"
                        (alist-get 'interpretation validity nil nil #'equal))))
      (insert "\n## Effect thresholds\n\n")
      (dolist (name '(corrected_primary_all_two
                      evidence_closure_pass
                      evidence_closure_blocked
                      corrected_beats_stale_actual
                      corrected_beats_no_claim_actual
                      quality_worse_than_no_claim_actual))
        (insert (format "- `%s`: `%s`\n" name
                        (alist-get name thresholds nil nil #'equal))))
      (insert "\n## Counterexamples\n\n")
      (if counterexamples
          (dolist (counterexample counterexamples)
            (insert (format "- `%s` `%s`: corrected `%s`, comparison `%s`\n"
                            (alist-get 'task_id counterexample nil nil #'equal)
                            (alist-get 'kind counterexample nil nil #'equal)
                            (alist-get 'corrected_current_score counterexample nil nil #'equal)
                            (alist-get 'comparison_score counterexample nil nil #'equal))))
        (insert "- None.\n"))
      (insert "\n## Prohibited claims\n\n")
      (dolist (claim prohibited)
        (insert (format "- %s\n" claim)))
      (insert "\n")
      (insert (alist-get 'allowed_effect_statement report nil nil #'equal))
      (insert "\n"))))

(defun sc-clec--run-analysis (dir)
  (let* ((manifest (sc-clec--manifest dir))
         (correction-record
          (let ((path (sc-clec--path dir "claim-correction-record.json")))
            (when (file-readable-p path) (sc-clec--read-json path))))
         (assignments (sc-clec--load-assignments dir))
         (records (mapcar (lambda (assignment) (sc-clec--record dir assignment))
                          assignments))
         (erratum-path (sc-clec--path dir "validity-erratum-v0.3.json"))
         (erratum (when (file-readable-p erratum-path)
                    (sc-clec--read-json erratum-path)))
         (report (sc-clec--apply-validity-erratum
                  (sc-clec--analyze-records records manifest correction-record)
                  erratum)))
    (sc-clec--write-json (sc-clec--path dir "comparison-report.json") report)
    (sc-clec--write-markdown (sc-clec--path dir "comparison-report.md") report)
    report))

(defun sc-clec--synthetic-assignment (task-index arm primary quality &optional fields)
  (let* ((task-id (format "task-%02d" task-index))
         (assignment-id (format "%s-%s" task-id arm)))
    (copy-tree
     `((assignment_id . ,assignment-id)
       (task_id . ,task-id)
       (arm . ,arm)
       (evidence . ((assignment_id . ,assignment-id)
                    (task_id . ,task-id)
                    (claim_arm . ,arm)
                    (first_attempt_id . ,(concat assignment-id "/attempt-1"))
                   (comparison_readiness . "pass")
                   (prior_gate . "pass")
                   (execution_gate . "pass")
                   (artifact_gate . "pass")
                   (consumption_gate . "pass")))
       (outcome . ((assignment_id . ,assignment-id)
                   (task_id . ,task-id)
                   (arm . ,arm)
                   (first_attempt_id . ,(concat assignment-id "/attempt-1"))
                   (outcome_closure . "pass")
                   (evaluator_id . "synthetic-evaluator-v0")
                   (correction_fidelity_score . ,primary)
                   (research_quality_score . ,quality)))
       ,@fields))))

(defun sc-clec--synthetic-assignments (&optional mutator)
  (let (assignments)
    (dotimes (i sc-clec--task-count)
      (let ((task (1+ i)))
        (dolist (spec '(("no-claim" 1 5) ("stale-claim" 0 4) ("corrected-current" 2 5)))
          (push (apply #'sc-clec--synthetic-assignment task spec) assignments))))
    (setq assignments (nreverse assignments))
    (if mutator (funcall mutator assignments) assignments)))

(defun sc-clec--synthetic-records (&optional mutator)
  (mapcar (lambda (assignment) (sc-clec--record nil assignment))
          (sc-clec--synthetic-assignments mutator)))

(defun sc-clec--expect-error (fn)
  (condition-case nil
      (progn (funcall fn) nil)
    (user-error t)))

(defun sc-clec--assert (condition message)
  (unless condition
    (signal 'user-error (list (format "self-check failed: %s" message)))))

(defun sc-clec--self-check ()
  (let ((pass (sc-clec--analyze-records (sc-clec--synthetic-records))))
    (sc-clec--assert (equal "effect_gate_pass" (alist-get 'status pass nil nil #'equal))
                     "pass fixture effect gate")
    (sc-clec--assert (= 8 (alist-get 'corrected_beats_stale_actual
                                     (alist-get 'thresholds pass nil nil #'equal)
                                     nil nil #'equal))
                     "exact stale paired count")
    (sc-clec--assert (= 8 (alist-get 'corrected_beats_no_claim_actual
                                     (alist-get 'thresholds pass nil nil #'equal)
                                     nil nil #'equal))
                     "exact no-claim paired count")
    (sc-clec--assert (string-match-p "\"effect_gate_pass\"" (sc-clec--json-string pass))
                     "report JSON serializes")
    (let* ((invalid
            (sc-clec--apply-validity-erratum
             pass
             '((classification . "experiment_input_contract_violation")
               (interpretation . "The raw matrix is descriptive only."))))
           (verdicts (alist-get 'verdicts invalid nil nil #'equal)))
      (sc-clec--assert
       (equal "invalid_input_contract"
              (alist-get 'status invalid nil nil #'equal))
       "input-contract erratum invalidates the report")
      (sc-clec--assert
       (equal "invalid" (alist-get 'consumption verdicts nil nil #'equal))
       "input-contract erratum invalidates consumption")
      (sc-clec--assert
       (equal "invalid" (alist-get 'effect verdicts nil nil #'equal))
       "input-contract erratum invalidates effect")
      (sc-clec--assert
       (equal "fail" (alist-get 'persistence verdicts nil nil #'equal))
       "input-contract erratum preserves unrelated verdicts"))
    (sc-clec--assert
     (equal "effect_gate_fail"
            (alist-get
             'status
             (sc-clec--analyze-records
              (sc-clec--synthetic-records
               (lambda (assignments)
                 (setcdr (assoc 'correction_fidelity_score
                                (alist-get 'outcome (nth 2 assignments) nil nil #'equal))
                         1)
                 assignments)))
             nil nil #'equal))
     "failure when corrected-current is not 2 for all tasks")
    (sc-clec--assert
     (equal "effect_gate_fail"
            (alist-get
             'status
             (sc-clec--analyze-records
              (sc-clec--synthetic-records
               (lambda (assignments)
                 (dotimes (i 3)
                   (setcdr (assoc 'correction_fidelity_score
                                  (alist-get 'outcome (nth (+ 2 (* i 3)) assignments)
                                             nil nil #'equal))
                           0))
                 assignments)))
             nil nil #'equal))
     "failure when corrected-current beats stale fewer than six times")
    (sc-clec--assert
     (equal "effect_gate_fail"
            (alist-get
             'status
             (sc-clec--analyze-records
              (sc-clec--synthetic-records
               (lambda (assignments)
                 (dotimes (i 6)
                   (setcdr (assoc 'correction_fidelity_score
                                  (alist-get 'outcome (nth (* i 3) assignments)
                                             nil nil #'equal))
                           2))
                 assignments)))
             nil nil #'equal))
     "failure when corrected-current beats no-claim fewer than three times")
    (sc-clec--assert
     (equal "effect_gate_fail"
            (alist-get
             'status
             (sc-clec--analyze-records
              (sc-clec--synthetic-records
               (lambda (assignments)
                 (dotimes (i 2)
                   (setcdr (assoc 'research_quality_score
                                  (alist-get 'outcome (nth (+ 2 (* i 3)) assignments)
                                             nil nil #'equal))
                           4))
                 assignments)))
             nil nil #'equal))
     "failure when research quality is worse than no-claim more than once")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--analyze-records
         (sc-clec--synthetic-records (lambda (assignments) (cdr assignments))))))
     "missing assignment rejected")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--analyze-records
         (sc-clec--synthetic-records
          (lambda (assignments)
            (cons (copy-tree (car assignments)) assignments))))))
     "duplicate assignment rejected")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--synthetic-records
         (lambda (assignments)
           (setcdr (assoc 'claim_arm (alist-get 'evidence (car assignments) nil nil #'equal))
                   "stale-claim")
           assignments))))
     "arm drift rejected")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--synthetic-records
         (lambda (assignments)
            (setcdr (assoc 'execution_gate
                          (alist-get 'evidence (car assignments) nil nil #'equal))
                   nil)
           assignments))))
     "incomplete evidence closure rejected")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--analyze-records
         (sc-clec--synthetic-records
          (lambda (assignments)
            (setcdr (assoc 'evaluator_id (alist-get 'outcome (car assignments) nil nil #'equal))
                    "different-evaluator")
            assignments)))))
     "evaluator drift rejected")
    (sc-clec--assert
     (sc-clec--expect-error
      (lambda ()
        (sc-clec--synthetic-records
         (lambda (assignments)
           (setcar assignments (cons '(rerun . t) (car assignments)))
           assignments))))
     "rerun substitution rejected")
    (princ "analyze.el self-check: pass\n")))

(defun sc-clec--main ()
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (condition-case err
        (cond
         ((member "--self-check" args)
          (sc-clec--self-check))
         (t
          (let* ((dir (or (cadr (member "--experiment-dir" args))
                          (sc-clec--experiment-dir)))
                 (report (sc-clec--run-analysis (directory-file-name dir))))
            (princ (format "Wrote %s and %s\n"
                           (sc-clec--path dir "comparison-report.json")
                           (sc-clec--path dir "comparison-report.md")))
            (princ (format "Status: %s\n" (alist-get 'status report nil nil #'equal))))))
      (user-error (sc-clec--die "%s" (cadr err))))))

(when noninteractive
  (sc-clec--main))

;;; analyze.el ends here
