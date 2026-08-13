;;; superchat-experience.el --- Tape-backed epistemic evidence -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; This module records post-Run evaluator observations, conditional relations,
;; and human Candidate dispositions.  Its task-specific View selects at most
;; one explicitly promoted Candidate without treating it as universal truth.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'superchat-closure)
(require 'superchat-projection)
(require 'superchat-tape)
(require 'superchat-tape-view)

(defconst superchat-experience-workspace-revision-recovery-scope
  "recovery-class/workspace_read/superchat-workspace-artifact-revision-error"
  "Exact Experience scope for stale Workspace Artifact reads.")

(defconst superchat-experience--verdicts
  '("passed" "failed" "not-identifiable"))

(defconst superchat-experience--relations
  '("applies-under" "fails-under"))

(defconst superchat-experience--dispositions
  '("promote" "reject" "revoke"))

(defun superchat-experience--required-string (value label)
  "Return trimmed VALUE, or signal that LABEL is required."
  (unless (stringp value)
    (error "%s must be a non-empty string" label))
  (let ((value (string-trim value)))
    (when (string-empty-p value)
      (error "%s must be a non-empty string" label))
    value))

(defun superchat-experience--plist-p (value)
  "Return non-nil when VALUE is a non-empty keyword plist."
  (and (consp value)
       (proper-list-p value)
       (cl-evenp (length value))
       (cl-loop for (key _item) on value by #'cddr
                always (keywordp key))))

(defun superchat-experience--normalize-json (value &optional path)
  "Return canonical JSON-domain VALUE, rejecting duplicate keys at PATH."
  (cond
   ((or (null value) (eq value t) (stringp value) (numberp value)) value)
   ((symbolp value) (symbol-name value))
   ((vectorp value)
    (apply #'vector
           (mapcar (lambda (item)
                     (superchat-experience--normalize-json item path))
                   (append value nil))))
   ((superchat-experience--plist-p value)
    (let (seen result)
      (while value
        (let ((key (pop value))
              (item (pop value)))
          (when (memq key seen)
            (error "Duplicate JSON key %s at %s" key (or path "payload")))
          (push key seen)
          (setq result
                (nconc result
                       (list key
                             (superchat-experience--normalize-json
                              item (format "%s/%s" (or path "payload") key)))))))
      result))
   ((and (listp value) (proper-list-p value))
    (apply #'vector
           (mapcar (lambda (item)
                     (superchat-experience--normalize-json item path))
                   value)))
   (t (error "Value at %s is not JSON data: %S" (or path "payload") value))))

(defun superchat-experience--closed-object
    (value label allowed required)
  "Return normalized VALUE after checking LABEL's ALLOWED and REQUIRED keys."
  (unless (superchat-experience--plist-p value)
    (error "%s must be a non-empty keyword plist" label))
  (let ((value (superchat-experience--normalize-json value label)))
    (cl-loop for (key _item) on value by #'cddr
             unless (memq key allowed)
             do (error "Unknown %s key: %s" label key))
    (dolist (key required)
      (unless (plist-member value key)
        (error "%s requires %s" label key)))
    value))

(defun superchat-experience--enum (value allowed label)
  "Return VALUE as a member of ALLOWED, or signal for LABEL."
  (let ((value (if (symbolp value) (symbol-name value)
                 (superchat-experience--required-string value label))))
    (unless (member value allowed)
      (error "Unknown %s: %s" label value))
    value))

(defun superchat-experience--reference (value &optional event-only)
  "Return canonical typed Tape reference VALUE.
When EVENT-ONLY is non-nil, reject storage references."
  (let* ((value
          (superchat-experience--closed-object
           value "Tape reference" '(:session-id :event-id :storage-id)
           '(:session-id)))
         (session-id
          (superchat-experience--required-string
           (plist-get value :session-id) "Tape reference session"))
         (event-id (plist-get value :event-id))
         (storage-id (plist-get value :storage-id)))
    (unless (xor event-id storage-id)
      (error "Tape reference needs exactly one event or storage ID: %S" value))
    (cond
     (event-id
      (list :session-id session-id
            :event-id
            (superchat-experience--required-string
             event-id "Tape reference event")))
     (event-only
      (error "Expected an event reference: %S" value))
     ((not (and (integerp storage-id) (> storage-id 0)))
      (error "Tape reference storage ID must be a positive integer"))
     (t (list :session-id session-id :storage-id storage-id)))))

(defun superchat-experience--reference-list
    (value label &optional event-only)
  "Return non-empty typed references from VALUE for LABEL."
  (setq value (if (vectorp value) (append value nil) value))
  (unless (and (proper-list-p value) value)
    (error "%s must be a non-empty list" label))
  (mapcar (lambda (reference)
            (superchat-experience--reference reference event-only))
          value))

(defun superchat-experience--events-for-reference (reference)
  "Return events matching typed event REFERENCE."
  (let ((event-id (plist-get reference :event-id)))
    (cl-remove-if-not
     (lambda (event) (equal event-id (plist-get event :event-id)))
     (superchat-tape-replay-events (plist-get reference :session-id)))))

(defun superchat-experience--resolve-event (reference)
  "Return the unique event named by typed REFERENCE, or signal."
  (let ((matches (superchat-experience--events-for-reference reference)))
    (unless (= 1 (length matches))
      (error "Tape event reference must resolve exactly once: %S" reference))
    (car matches)))

(defun superchat-experience--require-reference (reference)
  "Require REFERENCE to resolve and return its event or reference."
  (if (plist-get reference :event-id)
      (superchat-experience--resolve-event reference)
    (unless (superchat-tape-storage-reference-p
             (plist-get reference :session-id)
             (plist-get reference :storage-id))
      (error "Unknown Tape storage reference: %S" reference))
    reference))

(defun superchat-experience--event-ref (event)
  "Return the typed reference for EVENT."
  (list :session-id (plist-get event :session-id)
        :event-id (plist-get event :event-id)))

(defun superchat-experience--preflight-payload (payload)
  "Reject PAYLOAD unless its exact JSON round trip is stable."
  (let* ((receipt (superchat-tape-exact-json payload))
         (decoded
          (json-parse-string
           (plist-get receipt :json)
           :object-type 'plist :array-type 'array
           :null-object nil :false-object nil)))
    (unless (equal payload decoded)
      (error "Epistemic payload has an unstable JSON representation"))
    receipt))

(defun superchat-experience--event-node (event)
  "Return a compact graph node for EVENT."
  (let ((payload (plist-get event :payload)))
    (list :ref (superchat-experience--event-ref event)
          :type (plist-get event :type)
          :kind (or (plist-get payload :kind)
                    (plist-get payload :verdict))
          :scope (plist-get payload :scope)
          :actor-id (plist-get event :actor-id)
          :recorded-at (plist-get event :recorded-at)
          :payload payload)))

(defun superchat-experience--append-node (node nodes)
  "Add NODE to NODES once by typed reference."
  (if (cl-find (plist-get node :ref) nodes
               :key (lambda (candidate) (plist-get candidate :ref))
               :test #'equal)
      nodes
    (cons node nodes)))

(defun superchat-experience--terminal-event-p (event)
  "Return non-nil when EVENT is a Run terminal."
  (member (plist-get event :type)
          '("run.completed" "run.failed" "run.cancelled")))

(defun superchat-experience--reference-in-value-p (reference value)
  "Return non-nil when typed REFERENCE occurs in JSON-domain VALUE."
  (cond
   ((equal reference value) t)
   ((vectorp value)
    (cl-some (lambda (item)
               (superchat-experience--reference-in-value-p reference item))
             (append value nil)))
   ((consp value)
    (cl-some (lambda (item)
               (superchat-experience--reference-in-value-p reference item))
             value))))

(defun superchat-experience--event-invalid-in-graph-p (event graph)
  "Return non-nil when EVENT is missing or conflicted in GRAPH."
  (let ((reference (superchat-experience--event-ref event)))
    (or
     (cl-find-if
      (lambda (issue)
        (and (equal (plist-get event :session-id)
                    (plist-get issue :session-id))
             (equal (plist-get event :event-id)
                    (plist-get issue :event-id))))
      (plist-get graph :missing))
     (cl-find-if
      (lambda (conflict)
        (superchat-experience--reference-in-value-p
         reference (plist-get conflict :event-refs)))
      (plist-get graph :conflicts)))))

(defun superchat-experience--passed-evaluation-p
    (event candidate scope graph &optional before-storage-id)
  "Return non-nil when EVENT validly passed CANDIDATE in SCOPE.
When BEFORE-STORAGE-ID is non-nil, require the Evaluation to precede it."
  (let ((payload (plist-get event :payload)))
    (and (equal "exercise.evaluated" (plist-get event :type))
         (equal "passed" (plist-get payload :verdict))
         (equal scope (plist-get payload :scope))
         (superchat-experience--reference-in-value-p
          candidate (plist-get payload :evidence))
         (or (null before-storage-id)
             (< (plist-get event :storage-id) before-storage-id))
         (not (superchat-experience--event-invalid-in-graph-p event graph)))))

(defun superchat-experience--basis-sessions (candidate basis-events)
  "Return Session IDs needed to validate CANDIDATE and BASIS-EVENTS."
  (let ((sessions (list (plist-get candidate :session-id))))
    (dolist (event basis-events)
      (push (plist-get event :session-id) sessions)
      (let ((payload (plist-get event :payload)))
        (dolist (key '(:decision :terminal :evidence))
          (let ((value (plist-get payload key)))
            (when (vectorp value) (setq value (append value nil)))
            (dolist (reference (if (and (listp value)
                                        (not (keywordp (car-safe value))))
                                   value (list value)))
              (when (and (listp reference)
                         (plist-get reference :session-id))
                (push (plist-get reference :session-id) sessions)))))))
    (delete-dups (delq nil sessions))))

;;;###autoload
(defun superchat-experience-record-evaluation (spec)
  "Append one post-Run evaluator observation described by SPEC.
The Decision, terminal, and evidence fields are typed Tape references.  This
records an Outcome observation; it does not prove a Pattern or authorize a
future View."
  (let* ((spec
          (superchat-experience--closed-object
           spec "Evaluation"
           '(:session-id :actor-id :exercise-id :scope :run-id
             :decision :terminal :evaluator :evaluator-manifest-sha256
             :verdict :observations :metrics :evidence)
           '(:session-id :actor-id :exercise-id :scope :run-id
             :decision :terminal :evaluator :evaluator-manifest-sha256
             :verdict :observations :metrics :evidence)))
         (session-id
          (superchat-experience--required-string
           (plist-get spec :session-id) "Evaluation session"))
         (actor-id
          (superchat-experience--required-string
           (plist-get spec :actor-id) "Evaluation actor"))
         (exercise-id
          (superchat-experience--required-string
           (plist-get spec :exercise-id) "Exercise ID"))
         (scope
          (superchat-experience--required-string
           (plist-get spec :scope) "Evaluation scope"))
         (run-id
          (superchat-experience--required-string
           (plist-get spec :run-id) "Evaluation Run"))
         (decision-ref
          (superchat-experience--reference (plist-get spec :decision) t))
         (terminal-ref
          (superchat-experience--reference (plist-get spec :terminal) t))
         (evaluator
          (superchat-experience--closed-object
           (plist-get spec :evaluator) "Evaluator" '(:id :version)
           '(:id :version)))
         (evaluator-id
          (superchat-experience--required-string
           (plist-get evaluator :id) "Evaluator ID"))
         (evaluator-version (plist-get evaluator :version))
         (manifest-sha (plist-get spec :evaluator-manifest-sha256))
         (verdict
          (superchat-experience--enum
           (plist-get spec :verdict) superchat-experience--verdicts
           "Evaluation verdict"))
         (observations
          (superchat-experience--normalize-json
           (plist-get spec :observations) "Evaluation observations"))
         (metrics
          (superchat-experience--normalize-json
           (plist-get spec :metrics) "Evaluation metrics"))
         (evidence
          (superchat-experience--reference-list
           (plist-get spec :evidence) "Evaluation evidence"))
         (events (superchat-tape-replay-events session-id))
         (decision (superchat-experience--resolve-event decision-ref))
         (terminal (superchat-experience--resolve-event terminal-ref))
         (turn-id (plist-get decision :turn-id)))
    (unless (and (equal session-id (plist-get decision-ref :session-id))
                 (equal session-id (plist-get terminal-ref :session-id)))
      (error "Evaluation Decision and terminal must belong to its Session"))
    (unless (and (integerp evaluator-version) (> evaluator-version 0))
      (error "Evaluator version must be a positive integer"))
    (unless (and (stringp manifest-sha)
                 (string-match-p "\\`sha256:[[:xdigit:]]\\{64\\}\\'"
                                 manifest-sha))
      (error "Evaluator manifest must have a full SHA-256 digest"))
    (unless (equal "decision.snapshot.recorded" (plist-get decision :type))
      (error "Evaluation Decision is not a Decision Snapshot"))
    (unless (superchat-experience--terminal-event-p terminal)
      (error "Evaluation terminal is not a Run terminal"))
    (unless (and (equal run-id (plist-get decision :run-id))
                 (equal run-id (plist-get terminal :run-id))
                 (stringp turn-id) (not (string-empty-p turn-id))
                 (equal turn-id (plist-get terminal :turn-id)))
      (error "Evaluation Decision and terminal identity mismatch"))
    (unless (< (plist-get decision :storage-id)
               (plist-get terminal :storage-id))
      (error "Evaluation Decision must precede its terminal"))
    (unless (= 1 (cl-count-if
                  (lambda (event)
                    (and (equal run-id (plist-get event :run-id))
                         (superchat-experience--terminal-event-p event)))
                  events))
      (error "Evaluation Run must have exactly one terminal"))
    (let* ((judgment
            (superchat-closure-judge
             (list :kind 'durable-turn-terminal :run-id run-id
                   :turn-id turn-id)
             events))
           (witness-ids (plist-get (plist-get judgment :witness) :event-ids)))
      (unless (and (eq 'proved (plist-get judgment :verdict))
                   (not (plist-get (plist-get judgment :judgment)
                                   :audit-incomplete))
                   (member (plist-get decision :event-id) witness-ids)
                   (member (plist-get terminal :event-id) witness-ids))
        (error "Evaluation Run is not completely and durably closed")))
    (dolist (reference evidence)
      (superchat-experience--require-reference reference))
    (let ((payload
           (list :exercise-id exercise-id :scope scope :run-id run-id
                 :decision decision-ref :terminal terminal-ref
                 :evaluator (list :id evaluator-id :version evaluator-version)
                 :evaluator-manifest-sha256 manifest-sha
                 :verdict verdict :observations observations :metrics metrics
                 :evidence (vconcat evidence))))
      ;; A post-Run observation must not be silently redacted or downgraded.
      (superchat-experience--preflight-payload payload)
      (superchat-tape-append-event-record
       (list :type "exercise.evaluated" :schema 1
             :session-id session-id :turn-id turn-id :run-id run-id
             :actor-id actor-id
             :causation-id (plist-get terminal :event-id)
             :topic "learning/evaluation" :summary "exercise.evaluated"
             :sensitivity "confidential" :requires-payload t
             :payload payload)))))

;;;###autoload
(defun superchat-experience-record-relation (spec)
  "Append one scoped applicability relation described by SPEC.
The source must be a Pattern or Candidate Claim, the target a Context Claim,
and every endpoint and basis item must already exist in Tape."
  (let* ((spec
          (superchat-experience--closed-object
           spec "Applicability relation"
           '(:session-id :actor-id :correlation-id :causation-id
             :relation :from :to :scope :basis :reason)
           '(:session-id :actor-id :correlation-id
             :relation :from :to :scope :basis)))
         (session-id
          (superchat-experience--required-string
           (plist-get spec :session-id) "Relation session"))
         (actor-id
          (superchat-experience--required-string
           (plist-get spec :actor-id) "Relation actor"))
         (correlation-id
          (superchat-experience--required-string
           (plist-get spec :correlation-id) "Relation correlation"))
         (causation-id (plist-get spec :causation-id))
         (relation
          (superchat-experience--enum
           (plist-get spec :relation) superchat-experience--relations
           "applicability relation"))
         (from (superchat-experience--reference (plist-get spec :from) t))
         (to (superchat-experience--reference (plist-get spec :to) t))
         (scope
          (superchat-experience--required-string
           (plist-get spec :scope) "Relation scope"))
         (basis
          (superchat-experience--reference-list
           (plist-get spec :basis) "Relation basis"))
         (reason
          (when (plist-member spec :reason)
            (superchat-experience--required-string
             (plist-get spec :reason) "Relation reason")))
         (from-event (superchat-experience--resolve-event from))
         (to-event (superchat-experience--resolve-event to))
         (from-payload (plist-get from-event :payload))
         (to-payload (plist-get to-event :payload)))
    (when (equal from to)
      (error "Applicability relation cannot be a self-loop"))
    (unless (and (equal "claim.recorded" (plist-get from-event :type))
                 (member (or (plist-get from-payload :kind) "generic")
                         '("pattern" "candidate")))
      (error "Relation source must be a Pattern or Candidate Claim"))
    (unless (and (equal "claim.recorded" (plist-get to-event :type))
                 (equal "context" (plist-get to-payload :kind)))
      (error "Relation target must be a Context Claim"))
    (unless (and (equal scope (plist-get from-payload :scope))
                 (equal scope (plist-get to-payload :scope)))
      (error "Relation endpoints must share its scope"))
    (dolist (reference basis)
      (superchat-experience--require-reference reference))
    (when causation-id
      (setq causation-id
            (superchat-experience--required-string
             causation-id "Relation causation"))
      (superchat-experience--resolve-event
       (list :session-id session-id :event-id causation-id)))
    (let ((payload
           (append (list :relation relation :from from :to to
                         :scope scope :basis (vconcat basis))
                   (when reason (list :reason reason)))))
      (superchat-experience--preflight-payload payload)
      (superchat-tape-append-event-record
       (append
        (list :type "applicability.relation.recorded" :schema 1
              :session-id session-id :actor-id actor-id
              :correlation-id correlation-id
              :topic "learning/applicability"
              :summary "applicability.relation.recorded"
              :sensitivity "confidential" :requires-payload t
              :payload payload)
        (when causation-id (list :causation-id causation-id)))))))

;;;###autoload
(defun superchat-experience-record-disposition (spec)
  "Append one human Candidate disposition described by SPEC.
`promote' requires a valid passed Evaluation whose evidence names the
Candidate.  `revoke' must name the prior Promotion.  This host seam is not
registered as a model tool."
  (let* ((spec
          (superchat-experience--closed-object
           spec "Candidate disposition"
           '(:session-id :actor-id :candidate :scope :decision
             :basis :reason :prior)
           '(:session-id :actor-id :candidate :scope :decision
             :basis :reason)))
         (session-id
          (superchat-experience--required-string
           (plist-get spec :session-id) "Disposition session"))
         (actor-id
          (superchat-experience--required-string
           (plist-get spec :actor-id) "Disposition actor"))
         (candidate
          (superchat-experience--reference (plist-get spec :candidate) t))
         (scope
          (superchat-experience--required-string
           (plist-get spec :scope) "Disposition scope"))
         (decision
          (superchat-experience--enum
           (plist-get spec :decision) superchat-experience--dispositions
           "Candidate disposition"))
         (basis
          (superchat-experience--reference-list
           (plist-get spec :basis) "Disposition basis"))
         (reason
          (superchat-experience--required-string
           (plist-get spec :reason) "Disposition reason"))
         (prior
          (when (plist-member spec :prior)
            (superchat-experience--reference (plist-get spec :prior) t)))
         (candidate-event (superchat-experience--resolve-event candidate))
         (candidate-payload (plist-get candidate-event :payload))
         (basis-events
          (mapcar (lambda (reference)
                    (let ((event (superchat-experience--require-reference
                                  reference)))
                      (unless (plist-get event :event-id)
                        (error "Disposition basis must use event references"))
                      event))
                  basis)))
    (unless (equal actor-id "user")
      (error "Candidate dispositions require the user Actor"))
    (unless (and (equal "claim.recorded" (plist-get candidate-event :type))
                 (equal "candidate" (plist-get candidate-payload :kind))
                 (equal scope (plist-get candidate-payload :scope)))
      (error "Disposition target must be a Candidate Claim in its scope"))
    (if (equal decision "revoke")
        (let* ((prior-event
                (or (and prior (superchat-experience--resolve-event prior))
                    (error "Revocation requires its prior Promotion")))
               (prior-payload (plist-get prior-event :payload)))
          (unless (and (equal session-id (plist-get prior :session-id))
                       (equal "candidate.disposition.recorded"
                              (plist-get prior-event :type))
                       (equal "promote" (plist-get prior-payload :decision))
                       (equal candidate (plist-get prior-payload :candidate))
                       (equal scope (plist-get prior-payload :scope)))
            (error "Revocation prior is not this Candidate's Promotion")))
      (when prior
        (error "Only revocation accepts a prior Promotion")))
    (when (equal decision "promote")
      (let* ((sessions
              (delete-dups
               (cons session-id
                     (superchat-experience--basis-sessions
                      candidate basis-events))))
             (graph
              (superchat-experience-applicability-graph sessions scope)))
        (unless (cl-some
                 (lambda (event)
                   (superchat-experience--passed-evaluation-p
                    event candidate scope graph))
                 basis-events)
          (error "Promotion needs a valid passed Candidate Evaluation"))))
    (let ((payload
           (append
            (list :candidate candidate :scope scope :decision decision
                  :basis (vconcat basis) :reason reason)
            (when prior (list :prior prior)))))
      (superchat-experience--preflight-payload payload)
      (superchat-tape-append-event-record
       (append
        (list :type "candidate.disposition.recorded" :schema 1
              :session-id session-id :actor-id actor-id
              :topic "learning/promotion"
              :summary "candidate.disposition.recorded"
              :sensitivity "confidential" :requires-payload t
              :payload payload)
        (when prior (list :causation-id (plist-get prior :event-id))))))))

(defun superchat-experience--projection-apply (state event)
  "Fold relevant EVENT into applicability projection STATE."
  (pcase (plist-get event :type)
    ((or "claim.recorded" "feedback.recorded" "exercise.evaluated")
     (plist-put state :nodes (cons event (plist-get state :nodes))))
    ("applicability.relation.recorded"
     (plist-put state :relations
                (cons event (plist-get state :relations))))
    (_ state)))

(superchat-projection-register
 'experience-applicability
 :version 1
 :initial '(:nodes nil :relations nil)
 :apply #'superchat-experience--projection-apply)

(defun superchat-experience--events-matching-ref (reference events)
  "Return EVENTS matching typed event REFERENCE."
  (cl-remove-if-not
   (lambda (event)
     (and (equal (plist-get reference :session-id)
                 (plist-get event :session-id))
          (equal (plist-get reference :event-id)
                 (plist-get event :event-id))))
   events))

(defun superchat-experience--reference-issue
    (reference events session-ids)
  "Return REFERENCE's issue in EVENTS selected by SESSION-IDS, or nil."
  (if-let* ((event-id (plist-get reference :event-id)))
      (let ((matches
             (superchat-experience--events-matching-ref reference events)))
        (cond
         ((not (member (plist-get reference :session-id) session-ids))
          'session-not-in-view)
         ((null matches) 'dangling-event)
         ((cdr matches) 'ambiguous-event)))
    (cond
     ((not (member (plist-get reference :session-id) session-ids))
      'session-not-in-view)
     ((not (condition-case nil
               (superchat-tape-storage-reference-p
                (plist-get reference :session-id)
                (plist-get reference :storage-id))
             (error nil)))
      'dangling-storage))))

(defun superchat-experience--relation-record
    (event events session-ids nodes missing)
  "Normalize relation EVENT and return (RECORD NODES MISSING)."
  (let ((payload (plist-get event :payload))
        (original-missing missing)
        (record (list :event-id (plist-get event :event-id)
                      :session-id (plist-get event :session-id)))
        issues)
    (if (not (superchat-experience--plist-p payload))
        (push 'malformed-payload issues)
      (condition-case _err
          (let* ((relation
                  (superchat-experience--enum
                   (plist-get payload :relation)
                   superchat-experience--relations "applicability relation"))
                 (from (superchat-experience--reference
                        (plist-get payload :from) t))
                 (to (superchat-experience--reference
                      (plist-get payload :to) t))
                 (scope (plist-get payload :scope))
                 (basis (superchat-experience--reference-list
                         (plist-get payload :basis) "Relation basis")))
            (setq record
                  (append record
                          (list :relation relation :from from :to to
                                :scope scope :basis basis)))
            (dolist (entry `((from ,from) (to ,to)))
              (let* ((role (car entry))
                     (reference (cadr entry))
                     (issue (superchat-experience--reference-issue
                             reference events session-ids))
                     (matches
                      (superchat-experience--events-matching-ref
                       reference events)))
                (when issue
                  (push (list :event-id (plist-get event :event-id)
                              :session-id (plist-get event :session-id)
                              :role role :ref reference :reason issue)
                        missing))
                (when (= 1 (length matches))
                  (let* ((endpoint (car matches))
                         (endpoint-payload (plist-get endpoint :payload))
                         (kind (or (plist-get endpoint-payload :kind)
                                   "generic")))
                    (setq nodes
                          (superchat-experience--append-node
                           (superchat-experience--event-node endpoint) nodes))
                    (unless (and
                             (equal "claim.recorded"
                                    (plist-get endpoint :type))
                             (if (eq role 'from)
                                 (member kind '("pattern" "candidate"))
                               (equal kind "context"))
                             (equal scope (plist-get endpoint-payload :scope)))
                      (push (list :event-id (plist-get event :event-id)
                                  :session-id (plist-get event :session-id)
                                  :role role :ref reference
                                  :reason 'invalid-endpoint)
                            missing))))))
            (dolist (reference basis)
              (let ((issue (superchat-experience--reference-issue
                            reference events session-ids)))
                (when issue
                  (push (list :event-id (plist-get event :event-id)
                              :session-id (plist-get event :session-id)
                              :role 'basis :ref reference :reason issue)
                        missing))
                (if (plist-get reference :event-id)
                    (let ((matches
                           (superchat-experience--events-matching-ref
                            reference events)))
                      (when (= 1 (length matches))
                        (setq nodes
                              (superchat-experience--append-node
                               (superchat-experience--event-node (car matches))
                               nodes))))
                  (unless issue
                    (setq nodes
                          (superchat-experience--append-node
                           (list :ref reference :type "fact") nodes)))))))
        (error
         (push (list 'malformed-relation (error-message-string _err))
               issues))))
    (when issues
      (dolist (issue issues)
        (push (list :event-id (plist-get event :event-id)
                    :session-id (plist-get event :session-id) :reason issue)
              missing)))
    (when record
      (setq record (plist-put record :valid (eq original-missing missing))))
    (list record nodes missing)))

(defun superchat-experience--relation-conflicts (relations)
  "Return opposite valid assertions preserved in RELATIONS."
  (let (seen conflicts)
    ;; ponytail: Applicability graphs are replayed as small views; index pairs
    ;; only after measured O(n^2) conflict detection becomes visible.
    (dolist (relation relations)
      (when (plist-get relation :valid)
        (let* ((kind (plist-get relation :relation))
               (opposite (if (equal kind "applies-under")
                             "fails-under" "applies-under"))
               (key (list (plist-get relation :from)
                          (plist-get relation :to)
                          (plist-get relation :scope)))
               (other
                (cl-find-if
                 (lambda (candidate)
                   (and (plist-get candidate :valid)
                        (equal opposite (plist-get candidate :relation))
                        (equal (plist-get relation :from)
                               (plist-get candidate :from))
                        (equal (plist-get relation :to)
                               (plist-get candidate :to))
                        (equal (plist-get relation :scope)
                               (plist-get candidate :scope))))
                 relations)))
          (when (and other (not (member key seen)))
            (push key seen)
            (push (list :kind "opposite-applicability"
                        :from (plist-get relation :from)
                        :to (plist-get relation :to)
                        :scope (plist-get relation :scope)
                        :event-refs
                        (list
                         (list :session-id (plist-get relation :session-id)
                               :event-id (plist-get relation :event-id))
                         (list :session-id (plist-get other :session-id)
                               :event-id (plist-get other :event-id))))
                  conflicts)))))
    (nreverse conflicts)))

(defun superchat-experience--evaluation-missing-p (event missing)
  "Return non-nil when EVENT has an entry in MISSING."
  (cl-find-if
   (lambda (issue)
     (and (equal (plist-get event :session-id)
                 (plist-get issue :session-id))
          (equal (plist-get event :event-id)
                 (plist-get issue :event-id))))
   missing))

(defun superchat-experience--feedback-node
    (event events session-ids nodes missing)
  "Resolve Feedback EVENT and return (NODE-SCOPE NODES MISSING)."
  (let* ((payload (plist-get event :payload))
         (target (plist-get payload :target))
         (matches
          (and (stringp target)
               (superchat-experience--events-matching-ref
                (list :session-id (plist-get event :session-id)
                      :event-id target)
                events)))
         (basis (plist-get payload :basis))
         (contract-valid t)
         node-scope)
    (condition-case err
        (progn
          (superchat-experience--closed-object
           payload "Persisted Feedback"
           '(:target :kind :basis :note :replacement)
           '(:target :kind :basis :note))
          (superchat-experience--required-string target "Feedback target")
          (superchat-experience--enum
           (plist-get payload :kind) '("support" "challenge" "correct")
           "Feedback kind")
          (superchat-experience--required-string
           (plist-get payload :note) "Feedback note")
          (unless (or (null basis)
                      (and (listp basis) (proper-list-p basis))
                      (vectorp basis))
            (error "Feedback basis must be a list of Tape references"))
          (when (vectorp basis)
            (setq basis (append basis nil)))
          (when (plist-member payload :replacement)
            (superchat-experience--required-string
             (plist-get payload :replacement) "Feedback replacement")))
      (error
       (setq contract-valid nil)
       (push (list :event-id (plist-get event :event-id)
                   :session-id (plist-get event :session-id)
                   :reason 'malformed-feedback
                   :detail (error-message-string err))
             missing)))
    (when (and (= 1 (length matches))
               (equal "claim.recorded" (plist-get (car matches) :type)))
      (setq node-scope (plist-get (plist-get (car matches) :payload) :scope)))
    (if (not (and contract-valid node-scope))
        (when contract-valid
          (push (list :event-id (plist-get event :event-id)
                      :session-id (plist-get event :session-id)
                      :role 'target :reason 'invalid-feedback-target)
                missing))
      (let ((node (superchat-experience--event-node event)))
        (setq node (plist-put node :scope node-scope))
        (setq nodes (superchat-experience--append-node node nodes))))
    (dolist (reference (and contract-valid basis))
      (if (stringp reference)
          (push (list :event-id (plist-get event :event-id)
                      :session-id (plist-get event :session-id)
                      :role 'basis :ref reference
                      :reason 'legacy-unscoped-reference)
                missing)
        (condition-case err
            (let* ((reference (superchat-experience--reference reference))
                   (issue (superchat-experience--reference-issue
                           reference events session-ids)))
              (when issue
                (push (list :event-id (plist-get event :event-id)
                            :session-id (plist-get event :session-id)
                            :role 'basis :ref reference :reason issue)
                      missing))
              (if (plist-get reference :event-id)
                  (let ((sources
                         (superchat-experience--events-matching-ref
                          reference events)))
                    (when (= 1 (length sources))
                      (setq nodes
                            (superchat-experience--append-node
                             (superchat-experience--event-node (car sources))
                             nodes))))
                (unless issue
                  (setq nodes
                        (superchat-experience--append-node
                         (list :ref reference :type "fact") nodes)))))
          (error
           (push (list :event-id (plist-get event :event-id)
                       :session-id (plist-get event :session-id)
                       :role 'basis :reason 'malformed-feedback-basis
                       :detail (error-message-string err))
                 missing)))))
    (list node-scope nodes missing)))

(defun superchat-experience--evaluation-evidence
    (event events session-ids nodes missing)
  "Validate evaluator EVENT references and return (NODES MISSING)."
  (let ((payload (plist-get event :payload)))
    (condition-case err
        (let* ((payload
                (superchat-experience--closed-object
                 payload "Persisted Evaluation"
                 '(:exercise-id :scope :run-id :decision :terminal
                   :evaluator :evaluator-manifest-sha256 :verdict
                   :observations :metrics :evidence)
                 '(:exercise-id :scope :run-id :decision :terminal
                   :evaluator :evaluator-manifest-sha256 :verdict
                   :observations :metrics :evidence)))
               (evaluator
                (superchat-experience--closed-object
                 (plist-get payload :evaluator) "Persisted Evaluator"
                 '(:id :version) '(:id :version)))
               (decision
                (superchat-experience--reference
                 (plist-get payload :decision) t))
               (terminal
                (superchat-experience--reference
                 (plist-get payload :terminal) t))
               (evidence
                (superchat-experience--reference-list
                 (plist-get payload :evidence) "Evaluation evidence"))
               (decision-matches
                (superchat-experience--events-matching-ref decision events))
               (terminal-matches
                (superchat-experience--events-matching-ref terminal events)))
          (dolist (entry (append `((decision ,decision) (terminal ,terminal))
                                 (mapcar (lambda (reference)
                                           (list 'evidence reference))
                                         evidence)))
            (let* ((role (car entry))
                   (reference (cadr entry))
                   (issue (superchat-experience--reference-issue
                           reference events session-ids)))
              (when issue
                (push (list :event-id (plist-get event :event-id)
                            :session-id (plist-get event :session-id)
                            :role role :ref reference :reason issue)
                      missing))
              (if (plist-get reference :event-id)
                  (let ((matches
                         (superchat-experience--events-matching-ref
                          reference events)))
                    (when (= 1 (length matches))
                      (let ((source (car matches)))
                        (setq nodes
                              (superchat-experience--append-node
                               (superchat-experience--event-node source)
                               nodes))
                        (when (and
                               (memq role '(decision terminal))
                               (not
                                (if (eq role 'decision)
                                    (and
                                     (equal "decision.snapshot.recorded"
                                            (plist-get source :type))
                                     (equal (plist-get payload :run-id)
                                            (plist-get source :run-id)))
                                  (and
                                   (superchat-experience--terminal-event-p
                                    source)
                                   (equal (plist-get payload :run-id)
                                          (plist-get source :run-id))))))
                          (push (list :event-id (plist-get event :event-id)
                                      :session-id
                                      (plist-get event :session-id)
                                      :role role :ref reference
                                      :reason 'invalid-runtime-reference)
                                missing)))))
                (unless issue
                  (setq nodes
                        (superchat-experience--append-node
                         (list :ref reference :type "fact") nodes))))))
          (let* ((evaluation-session (plist-get event :session-id))
                 (run-id (plist-get payload :run-id))
                 (decision-event (car decision-matches))
                 (terminal-event (car terminal-matches))
                 (turn-id (and decision-event
                               (plist-get decision-event :turn-id)))
                 (session-events
                  (cl-remove-if-not
                   (lambda (candidate)
                     (equal evaluation-session
                            (plist-get candidate :session-id)))
                   events))
                 (judgment
                  (and (= 1 (length decision-matches))
                       (= 1 (length terminal-matches))
                       (stringp run-id) (stringp turn-id)
                       (superchat-closure-judge
                        (list :kind 'durable-turn-terminal
                              :run-id run-id :turn-id turn-id)
                        session-events)))
                 (witness-ids
                 (and judgment
                       (plist-get (plist-get judgment :witness) :event-ids))))
            (unless
                (and
                 (not (string-empty-p
                       (superchat-experience--required-string
                        (plist-get payload :exercise-id) "Exercise ID")))
                 (not (string-empty-p
                       (superchat-experience--required-string
                        (plist-get evaluator :id) "Evaluator ID")))
                 (and (integerp (plist-get evaluator :version))
                      (> (plist-get evaluator :version) 0))
                 (let ((digest
                        (plist-get payload :evaluator-manifest-sha256)))
                   (and (stringp digest)
                        (string-match-p
                         "\\`sha256:[[:xdigit:]]\\{64\\}\\'" digest)))
                 (member (plist-get payload :verdict)
                         superchat-experience--verdicts)
                 (stringp (plist-get payload :scope))
                 (not (string-empty-p (plist-get payload :scope)))
                 (equal evaluation-session (plist-get decision :session-id))
                 (equal evaluation-session (plist-get terminal :session-id))
                 (equal run-id (plist-get event :run-id))
                 (equal turn-id (plist-get event :turn-id))
                 (equal (plist-get terminal :event-id)
                        (plist-get event :causation-id))
                 (equal "decision.snapshot.recorded"
                        (plist-get decision-event :type))
                 (superchat-experience--terminal-event-p terminal-event)
                 (equal run-id (plist-get decision-event :run-id))
                 (equal run-id (plist-get terminal-event :run-id))
                 (equal turn-id (plist-get terminal-event :turn-id))
                 (< (plist-get decision-event :storage-id)
                    (plist-get terminal-event :storage-id))
                 (< (plist-get terminal-event :storage-id)
                    (plist-get event :storage-id))
                 (= 1 (cl-count-if
                       (lambda (candidate)
                         (and (equal run-id (plist-get candidate :run-id))
                              (superchat-experience--terminal-event-p
                               candidate)))
                       session-events))
                 (eq 'proved (plist-get judgment :verdict))
                 (not (plist-get (plist-get judgment :judgment)
                                 :audit-incomplete))
                 (member (plist-get decision-event :event-id) witness-ids)
                 (member (plist-get terminal-event :event-id) witness-ids))
              (push (list :event-id (plist-get event :event-id)
                          :session-id evaluation-session
                          :reason 'invalid-evaluation-contract)
                    missing))))
      (error
       (push (list :event-id (plist-get event :event-id)
                   :session-id (plist-get event :session-id)
                   :reason 'malformed-evaluation
                   :detail (error-message-string err))
             missing)))
    (list nodes missing)))

(defun superchat-experience--evaluation-conflicts (events scope missing)
  "Return divergent valid evaluator verdicts in EVENTS for SCOPE."
  (let (seen conflicts)
    (dolist (left events)
      (when (and (equal "exercise.evaluated" (plist-get left :type))
                 (not (superchat-experience--evaluation-missing-p
                       left missing))
                 (or (null scope)
                     (equal scope
                            (plist-get (plist-get left :payload) :scope))))
        (let* ((payload (plist-get left :payload))
               (key (list (plist-get left :session-id)
                          (plist-get payload :run-id)
                          (plist-get payload :exercise-id)
                          (plist-get payload :scope)))
               (right
                (cl-find-if
                 (lambda (candidate)
                   (let ((candidate-payload (plist-get candidate :payload)))
                     (and (not (equal (plist-get left :event-id)
                                      (plist-get candidate :event-id)))
                          (equal "exercise.evaluated"
                                 (plist-get candidate :type))
                          (not (superchat-experience--evaluation-missing-p
                                candidate missing))
                          (equal key
                                 (list (plist-get candidate :session-id)
                                       (plist-get candidate-payload :run-id)
                                       (plist-get candidate-payload :exercise-id)
                                       (plist-get candidate-payload :scope)))
                          (not (equal (plist-get payload :verdict)
                                      (plist-get candidate-payload
                                                 :verdict))))))
                 events)))
          (when (and right (not (member key seen)))
            (push key seen)
            (push (list :kind "divergent-evaluation"
                        :run-id (plist-get payload :run-id)
                        :exercise-id (plist-get payload :exercise-id)
                        :scope (plist-get payload :scope)
                        :event-refs
                        (list (superchat-experience--event-ref left)
                              (superchat-experience--event-ref right)))
                  conflicts)))))
    (nreverse conflicts)))

(defun superchat-experience--feedback-conflicts (nodes missing)
  "Return valid Claims with support and challenge Feedback in NODES."
  (let (seen conflicts)
    (dolist (left nodes)
      (when (and (equal "feedback.recorded" (plist-get left :type))
                 (not (cl-find-if
                       (lambda (issue)
                         (and (equal (plist-get (plist-get left :ref)
                                                :session-id)
                                     (plist-get issue :session-id))
                              (equal (plist-get (plist-get left :ref)
                                                :event-id)
                                     (plist-get issue :event-id))))
                       missing))
                 (member (plist-get (plist-get left :payload) :kind)
                         '("support" "challenge")))
        (let* ((payload (plist-get left :payload))
               (kind (plist-get payload :kind))
               (opposite (if (equal kind "support")
                             "challenge" "support"))
               (key (list (plist-get (plist-get left :ref) :session-id)
                          (plist-get payload :target)))
               (right
                (cl-find-if
                 (lambda (candidate)
                   (and (equal "feedback.recorded"
                               (plist-get candidate :type))
                        (not (cl-find-if
                              (lambda (issue)
                                (and
                                 (equal
                                  (plist-get (plist-get candidate :ref)
                                             :session-id)
                                  (plist-get issue :session-id))
                                 (equal
                                  (plist-get (plist-get candidate :ref)
                                             :event-id)
                                  (plist-get issue :event-id))))
                              missing))
                        (equal opposite
                               (plist-get (plist-get candidate :payload)
                                          :kind))
                        (equal key
                               (list
                                (plist-get (plist-get candidate :ref)
                                           :session-id)
                                (plist-get (plist-get candidate :payload)
                                           :target)))))
                 nodes)))
          (when (and right (not (member key seen)))
            (push key seen)
            (push (list :kind "support-challenge"
                        :target (plist-get payload :target)
                        :event-refs
                        (list (plist-get left :ref)
                              (plist-get right :ref)))
                  conflicts)))))
    (nreverse conflicts)))

;;;###autoload
(defun superchat-experience-applicability-graph (session-ids &optional scope)
  "Rebuild an applicability graph from explicit SESSION-IDS and SCOPE.
The result preserves every assertion and reports `:conflicts' and `:missing';
it never chooses a winning Claim."
  (unless (and (proper-list-p session-ids) session-ids)
    (error "Experience graph requires a non-empty Session list"))
  (setq session-ids
        (mapcar (lambda (session-id)
                  (superchat-experience--required-string
                   session-id "Experience graph session"))
                session-ids))
  (when (/= (length session-ids) (length (delete-dups (copy-sequence session-ids))))
    (error "Experience graph Session list contains duplicates"))
  (when scope
    (setq scope (superchat-experience--required-string
                 scope "Experience graph scope")))
  (let* ((events
          (sort
           (apply #'append
                  (mapcar #'superchat-tape-replay-events session-ids))
           (lambda (left right)
             (< (plist-get left :storage-id) (plist-get right :storage-id)))))
         (state
          (superchat-projection-replay 'experience-applicability events))
         (raw-nodes (nreverse (plist-get state :nodes)))
         (raw-relations (nreverse (plist-get state :relations)))
         nodes relations missing)
    (dolist (event raw-nodes)
      (let ((event-scope (plist-get (plist-get event :payload) :scope)))
        (pcase (plist-get event :type)
          ((or "claim.recorded" "exercise.evaluated")
           (when (or (null scope) (equal scope event-scope))
             (setq nodes
                   (superchat-experience--append-node
                    (superchat-experience--event-node event) nodes))
             (when (equal "exercise.evaluated" (plist-get event :type))
               (pcase-let ((`(,next-nodes ,next-missing)
                            (superchat-experience--evaluation-evidence
                             event events session-ids nodes missing)))
                 (setq nodes next-nodes
                       missing next-missing)))))
          ("feedback.recorded"
           (pcase-let ((`(,node-scope ,next-nodes ,next-missing)
                        (superchat-experience--feedback-node
                         event events session-ids nodes missing)))
             (when (or (null scope) (equal scope node-scope))
               (setq nodes next-nodes
                     missing next-missing)))))))
    (dolist (event raw-relations)
      (when (or (null scope)
                (equal scope (plist-get (plist-get event :payload) :scope)))
        (pcase-let ((`(,record ,next-nodes ,next-missing)
                     (superchat-experience--relation-record
                      event events session-ids nodes missing)))
          (setq nodes next-nodes
                missing next-missing)
          (when record (push record relations)))))
    (setq nodes (nreverse nodes)
          relations (nreverse relations)
          missing (nreverse missing))
    (list :sessions session-ids :scope scope
          :nodes nodes :relations relations
          :conflicts
          (append (superchat-experience--relation-conflicts relations)
                  (superchat-experience--evaluation-conflicts
                   raw-nodes scope missing)
                  (superchat-experience--feedback-conflicts nodes missing))
          :missing missing)))

(defun superchat-experience--view-events (session-ids)
  "Return ordered Tape events from explicit SESSION-IDS."
  (sort (apply #'append
               (mapcar #'superchat-tape-replay-events session-ids))
        (lambda (left right)
          (< (plist-get left :storage-id) (plist-get right :storage-id)))))

(defun superchat-experience--disposition-record
    (event events session-ids graph)
  "Return validated disposition EVENT, or a record with `:error'."
  (condition-case err
      (let* ((payload
              (superchat-experience--closed-object
               (plist-get event :payload) "Persisted Candidate disposition"
               '(:candidate :scope :decision :basis :reason :prior)
               '(:candidate :scope :decision :basis :reason)))
             (candidate
              (superchat-experience--reference
               (plist-get payload :candidate) t))
             (scope
              (superchat-experience--required-string
               (plist-get payload :scope) "Disposition scope"))
             (decision
              (superchat-experience--enum
               (plist-get payload :decision)
               superchat-experience--dispositions "Candidate disposition"))
             (basis
              (superchat-experience--reference-list
               (plist-get payload :basis) "Disposition basis" t))
             (prior
              (when (plist-member payload :prior)
                (superchat-experience--reference
                 (plist-get payload :prior) t)))
             (candidate-matches
              (superchat-experience--events-matching-ref candidate events))
             (candidate-event (car candidate-matches)))
        (superchat-experience--required-string
         (plist-get payload :reason) "Disposition reason")
        (unless (and (equal "user" (plist-get event :actor-id))
                     (member (plist-get event :session-id) session-ids)
                     (= 1 (length candidate-matches))
                     (equal "claim.recorded" (plist-get candidate-event :type))
                     (equal "candidate"
                            (plist-get (plist-get candidate-event :payload)
                                       :kind))
                     (equal scope
                            (plist-get (plist-get candidate-event :payload)
                                       :scope)))
          (error "Invalid Candidate disposition target"))
        (let ((basis-events
               (mapcar
                (lambda (reference)
                  (let ((matches
                         (superchat-experience--events-matching-ref
                          reference events)))
                    (unless (= 1 (length matches))
                      (error "Invalid disposition basis reference"))
                    (car matches)))
                basis)))
          (pcase decision
            ("promote"
             (when prior
               (error "Promotion cannot name a prior disposition"))
             (unless
                 (cl-some
                  (lambda (basis-event)
                    (superchat-experience--passed-evaluation-p
                     basis-event candidate scope graph
                     (plist-get event :storage-id)))
                  basis-events)
               (error "Promotion has no valid passed Candidate Evaluation")))
            ("reject"
             (when prior
               (error "Rejection cannot name a prior disposition")))
            ("revoke"
             (let* ((prior-matches
                     (and prior
                          (superchat-experience--events-matching-ref
                           prior events)))
                    (prior-event (car prior-matches))
                    (prior-payload (and prior-event
                                        (plist-get prior-event :payload))))
               (unless (and (= 1 (length prior-matches))
                            (equal (plist-get event :session-id)
                                   (plist-get prior :session-id))
                            (equal "candidate.disposition.recorded"
                                   (plist-get prior-event :type))
                            (equal "promote"
                                   (plist-get prior-payload :decision))
                            (equal candidate
                                   (plist-get prior-payload :candidate))
                            (equal scope (plist-get prior-payload :scope))
                            (equal (plist-get prior :event-id)
                                   (plist-get event :causation-id))
                            (< (plist-get prior-event :storage-id)
                               (plist-get event :storage-id)))
                 (error "Revocation does not follow its Promotion"))))))
        (list :valid t :ref (superchat-experience--event-ref event)
              :storage-id (plist-get event :storage-id)
              :candidate candidate :scope scope :decision decision
              :basis basis :prior prior))
    (error
     (list :valid nil :ref (superchat-experience--event-ref event)
           :scope (plist-get (plist-get event :payload) :scope)
           :error (error-message-string err)))))

(defun superchat-experience--session-heads (session-ids events)
  "Return stable per-Session Tape heads for SESSION-IDS from EVENTS."
  (vconcat
   (mapcar
    (lambda (session-id)
      (list :session-id session-id
            :storage-id
            (or (cl-loop for event in events
                         when (equal session-id (plist-get event :session-id))
                         maximize (plist-get event :storage-id))
                0)))
    session-ids)))

(defun superchat-experience--view-opportunity (scope context reason)
  "Return one read-only Opportunity for SCOPE, CONTEXT, and REASON."
  (list :kind "candidate-selection" :scope scope
        :context context :reason reason))

;;;###autoload
(defun superchat-experience-assemble-recovery-view (failure-event session-ids)
  "Return the reviewed View for FAILURE-EVENT, or nil.
FAILURE-EVENT is the exact typed `tool.failed' event that triggered recovery.
SESSION-IDS is the caller's explicit Experience lineage; this function never
discovers sessions by scanning the whole database."
  (unless (and (superchat-experience--plist-p failure-event)
               (equal "tool.failed" (plist-get failure-event :type)))
    (error "Recovery requires one typed tool.failed event"))
  (unless (and (proper-list-p session-ids)
               session-ids
               (cl-every (lambda (session-id)
                           (and (stringp session-id)
                                (not (string-empty-p session-id))))
                         session-ids)
               (= (length session-ids)
                  (length (delete-dups (copy-sequence session-ids)))))
    (error "Recovery requires an explicit non-empty Session lineage"))
  (let* ((payload (plist-get failure-event :payload))
         (tool-name (plist-get payload :tool-name))
         (error-type (plist-get payload :error-type)))
    (when (and (equal tool-name "workspace_read")
               (equal error-type
                      "superchat-workspace-artifact-revision-error"))
      (let* ((scope superchat-experience-workspace-revision-recovery-scope)
             (events (superchat-experience--view-events session-ids))
             (contexts
              (cl-remove-if-not
               (lambda (event)
                 (let ((event-payload (plist-get event :payload)))
                   (and (equal "claim.recorded" (plist-get event :type))
                        (equal "context" (plist-get event-payload :kind))
                        (equal scope (plist-get event-payload :scope)))))
               events)))
         (if (= 1 (length contexts))
             (let ((view
                    (superchat-experience-assemble-view
                     session-ids scope
                     (superchat-experience--event-ref (car contexts)))))
               ;; A blocked recovery View must also close the dependent action
               ;; gate.  Keep the dependency explicit: this seam knows the
               ;; failed action was `workspace_read'; it does not infer a
               ;; blanket policy from prose or Candidate text.
               (setq view
                     (plist-put view :failure-event
                                (superchat-experience--event-ref failure-event)))
               (if (eq 'blocked (plist-get view :status))
                   (plist-put view :experience-action-policy
                              '(:dependent-tools ("workspace_read")))
                 view))
           (let ((status (if contexts 'blocked 'empty))
                 (reason (if contexts "multiple-contexts" "no-context")))
             (append
              (list :contract "experience-view-v0" :status status :scope scope
                    :prompt nil :items []
                    :missing
                    (and contexts
                         (list (list :role 'context :reason 'multiple-contexts)))
                    :conflicts nil
                    :opportunity
                    (superchat-experience--view-opportunity scope nil reason)
                    :receipt
                    (list :projection "experience-applicability"
                          :projection-version 1)
                    :failure-event
                    (superchat-experience--event-ref failure-event))
              (when (eq 'blocked status)
                (list :experience-action-policy
                      '(:dependent-tools ("workspace_read")))))))))))

;;;###autoload
(defun superchat-experience-assemble-view (session-ids scope context)
  "Assemble one fail-closed Candidate View for CONTEXT in SCOPE.
SESSION-IDS is explicit.  The result never appends Tape data and selects at
most one active human-promoted Candidate with matching applicability."
  (setq scope (superchat-experience--required-string scope "Experience View scope")
        context (superchat-experience--reference context t))
  (let* ((before-events (superchat-experience--view-events session-ids))
         (before-heads
          (superchat-experience--session-heads session-ids before-events))
         (graph
          (superchat-experience-applicability-graph session-ids scope))
         (events (superchat-experience--view-events session-ids))
         (after-heads
          (superchat-experience--session-heads session-ids events))
         (context-matches
          (superchat-experience--events-matching-ref context events))
         (context-event (car context-matches))
         (missing (copy-tree (plist-get graph :missing) t))
         (conflicts (copy-tree (plist-get graph :conflicts) t))
         active)
    (unless (and (= 1 (length context-matches))
                 (equal "claim.recorded" (plist-get context-event :type))
                 (equal "context"
                        (plist-get (plist-get context-event :payload) :kind))
                 (equal scope
                        (plist-get (plist-get context-event :payload) :scope)))
      (push (list :role 'context :ref context :reason 'invalid-context)
            missing))
    (dolist (event events)
      (when (equal "candidate.disposition.recorded" (plist-get event :type))
        (let ((record
               (superchat-experience--disposition-record
                event events session-ids graph)))
          (when (or (null (plist-get record :scope))
                    (equal scope (plist-get record :scope)))
            (if (not (plist-get record :valid))
                (push (list :role 'disposition :ref (plist-get record :ref)
                            :reason 'invalid-disposition
                            :detail (plist-get record :error))
                      missing)
              (let* ((candidate (plist-get record :candidate))
                     (current (alist-get candidate active nil nil #'equal)))
                (pcase (plist-get record :decision)
                  ("promote"
                   (setf (alist-get candidate active nil nil #'equal) record))
                  ("reject"
                   (setq active (assoc-delete-all candidate active)))
                  ("revoke"
                   (if (equal (plist-get record :prior)
                              (plist-get current :ref))
                       (setq active (assoc-delete-all candidate active))
                     (push (list :role 'disposition
                                 :ref (plist-get record :ref)
                                 :reason 'inactive-promotion)
                           missing))))))))))
    (let (eligible)
      (dolist (entry active)
        (let* ((candidate (car entry))
               (promotion (cdr entry))
               (applies
                (cl-find-if
                 (lambda (relation)
                   (and (plist-get relation :valid)
                        (equal "applies-under"
                               (plist-get relation :relation))
                        (equal candidate (plist-get relation :from))
                        (equal context (plist-get relation :to))))
                 (plist-get graph :relations)))
               (fails
                (cl-find-if
                 (lambda (relation)
                   (and (plist-get relation :valid)
                        (equal "fails-under" (plist-get relation :relation))
                        (equal candidate (plist-get relation :from))
                        (equal context (plist-get relation :to))))
                 (plist-get graph :relations))))
          (when (and applies (not fails))
            (push (list :candidate candidate :promotion promotion
                        :relation applies)
                  eligible))))
      (setq missing (nreverse missing)
            eligible (nreverse eligible))
      (let* ((blocked (or missing conflicts (> (length eligible) 1)))
             (selected (and (not blocked) (car eligible)))
             (candidate-ref (plist-get selected :candidate))
             (candidate-event
              (and candidate-ref
                   (car (superchat-experience--events-matching-ref
                         candidate-ref events))))
             (statement
              (and candidate-event
                   (plist-get (plist-get candidate-event :payload)
                              :statement)))
             (promotion (plist-get selected :promotion))
             (relation (plist-get selected :relation))
             (status (cond (blocked 'blocked)
                           (selected 'satisfied)
                           (t 'empty)))
             (reason (cond (missing "missing-evidence")
                           (conflicts "conflicting-evidence")
                           ((> (length eligible) 1) "multiple-candidates")
                           ((null selected) "no-applicable-promotion")))
             (receipt
              (append
               (list :projection "experience-applicability"
                     :projection-version 1
                     :selection-policy "single-promoted-applicable-candidate"
                     :selection-policy-version 1
                     :session-heads
                     (superchat-experience--session-heads session-ids events)
                     :context context)
               (when selected
                 (list :candidate candidate-ref
                       :promotion (plist-get promotion :ref)
                       :relation
                       (list :session-id (plist-get relation :session-id)
                             :event-id (plist-get relation :event-id))))))
             (result
              (list :contract "experience-view-v0" :status status :scope scope
                    :context context
                    :prompt
                    (when selected
                      (format
                       "[Experience v0 | scope %s]\nUse this reviewed experience only when it fits the current context:\n%s"
                       scope statement))
                    :items
                    (if selected
                        (vector
                         (list :candidate candidate-ref
                               :promotion (plist-get promotion :ref)
                               :statement statement))
                      [])
                    :missing missing :conflicts conflicts
                    :opportunity
                    (unless selected
                      (superchat-experience--view-opportunity
                       scope context reason))
                    :receipt receipt)))
        (if (equal before-heads after-heads)
            result
          (list :contract "experience-view-v0" :status 'blocked :scope scope
                :context context :prompt nil :items []
                :missing '((:role snapshot :reason changed-during-assembly))
                :conflicts nil
                :opportunity
                (superchat-experience--view-opportunity
                 scope context "concurrent-evidence-update")
                :receipt
                (list :projection "experience-applicability"
                      :projection-version 1 :read-status 'unstable
                      :session-heads before-heads
                      :observed-session-heads after-heads
                      :context context)))))))

;;;###autoload
(defun superchat-experience-review-needs ()
  "Rebuild open recovery Review Needs from durable Tape evidence."
  ;; ponytail: global Tape replay; add an indexed Projection only if measured.
  (let* ((sessions (superchat-view-session-ids))
         (events (and sessions (superchat-experience--view-events sessions)))
         open blocked)
    (cl-labels
        ((ref (event)
           (superchat-experience--event-ref event))
         (after (event anchor)
           (> (plist-get event :storage-id) (plist-get anchor :storage-id)))
         (basis-covers-p (event evidence)
           (let ((basis
                  (append (plist-get (plist-get event :payload) :basis) nil)))
             (cl-every (lambda (item) (member item basis)) evidence)))
         (handled-p (outcome evidence candidate context promotion scope)
           (cl-some
            (lambda (event)
              (let ((payload (plist-get event :payload)))
                (and (after event outcome)
                     (basis-covers-p event evidence)
                     (or
                      (and
                       (equal "applicability.relation.recorded"
                              (plist-get event :type))
                       (equal candidate (plist-get payload :from))
                       (equal context (plist-get payload :to))
                       (equal scope (plist-get payload :scope)))
                      (and
                       (equal "candidate.disposition.recorded"
                              (plist-get event :type))
                       (equal "revoke" (plist-get payload :decision))
                       (equal candidate (plist-get payload :candidate))
                       (equal promotion (plist-get payload :prior))
                       (equal scope (plist-get payload :scope)))))))
            events))
         (open-need (intervention trigger action outcome receipt context scope)
           (let* ((candidate (plist-get receipt :candidate))
                  (promotion (plist-get receipt :promotion))
                  (evidence
                   (list trigger (ref intervention) (ref action) (ref outcome))))
             (unless (handled-p
                      outcome evidence candidate context promotion scope)
               (push
                (list :trigger trigger :intervention (ref intervention)
                      :action (ref action) :outcome (ref outcome)
                      :candidate candidate :promotion promotion
                      :relation (plist-get receipt :relation)
                      :context context :scope scope :basis evidence)
                open)))))
      (dolist (intervention events)
        (when (equal "experience.intervention.recorded"
                     (plist-get intervention :type))
          (let* ((payload (plist-get intervention :payload))
                 (receipt (plist-get payload :receipt))
                 (trigger (plist-get payload :trigger))
                 (context (plist-get payload :context))
                 (scope (plist-get payload :scope))
                 (candidate (plist-get receipt :candidate))
                 (promotion (plist-get receipt :promotion))
                 (relation (plist-get receipt :relation))
                 (required (list trigger context candidate promotion relation)))
            (if (not
                 (cl-every
                  (lambda (reference)
                    (= 1 (length
                          (superchat-experience--events-matching-ref
                           reference events))))
                  required))
                (push (list :intervention (ref intervention)
                            :reason 'invalid-receipt)
                      blocked)
              (let* ((run-id (plist-get intervention :run-id))
                     (action
                      (cl-find-if
                       (lambda (event)
                         (and (after event intervention)
                              (equal run-id (plist-get event :run-id))
                              (member (plist-get event :type)
                                      '("tool.requested"
                                        "response.completed"))))
                       events)))
                (cond
                 ((null action)
                  (push (list :intervention (ref intervention)
                              :reason 'missing-action)
                        blocked))
                 ((equal "tool.requested" (plist-get action :type))
                  (let* ((execution-id
                          (plist-get (plist-get action :payload)
                                     :execution-id))
                         (outcomes
                          (cl-remove-if-not
                           (lambda (event)
                             (and (after event action)
                                  (equal run-id (plist-get event :run-id))
                                  (member (plist-get event :type)
                                          '("tool.completed" "tool.failed"))
                                  (equal execution-id
                                         (plist-get
                                          (plist-get event :payload)
                                          :execution-id))))
                           events)))
                    (cond
                     ((/= 1 (length outcomes))
                      (push (list :intervention (ref intervention)
                                  :reason (if outcomes
                                              'ambiguous-outcome
                                            'missing-outcome))
                            blocked))
                     ((equal "tool.failed" (plist-get (car outcomes) :type))
                      (open-need intervention trigger action (car outcomes)
                                 receipt context scope)))))
                 (t
                  (let* ((action-ref (ref action))
                         (outcomes
                          (cl-remove-if-not
                           (lambda (event)
                             (let ((event-payload (plist-get event :payload)))
                               (and (after event action)
                                    (equal "exercise.evaluated"
                                           (plist-get event :type))
                                    (member
                                     action-ref
                                     (append
                                      (plist-get event-payload :evidence)
                                      nil)))))
                           events)))
                    (cond
                     ((/= 1 (length outcomes))
                      (push (list :intervention (ref intervention)
                                  :reason (if outcomes
                                              'ambiguous-outcome
                                            'missing-outcome))
                            blocked))
                     ((equal "failed"
                             (plist-get (plist-get (car outcomes) :payload)
                                        :verdict))
                      (open-need intervention trigger action (car outcomes)
                                 receipt context scope)))))))))))
    (list :contract "experience-review-v0"
          :open (vconcat (nreverse open))
          :blocked (vconcat (nreverse blocked))))))

;;;###autoload
(defun superchat-experience--cmd-review
    (_cmd args _input _lang _target-model)
  "Show or resolve the first open recovery Review Need."
  (let* ((view (superchat-experience-review-needs))
         (need (and (> (length (plist-get view :open)) 0)
                    (aref (plist-get view :open) 0)))
         (args (string-trim (or args ""))))
    (if (null need)
        '(:type :echo :content "No open recovery Review Need.")
      (if (string-empty-p args)
        (list
         :type :echo
         :content
         (concat
          (format
           (concat "Recovery Review Need\nCandidate: %S\nContext: %S\n"
                   "Action: %S\nOutcome: %S\n"
                   "Use /review retain REASON, /review fails-under REASON, "
                   "or /review revoke REASON.")
           (plist-get need :candidate) (plist-get need :context)
           (plist-get need :action) (plist-get need :outcome))
          (let ((hint (superchat-experience--structural-hint-text need)))
            (if hint (concat "\n" hint) ""))))
      (unless (string-match
               "\\`\\([^[:space:]]+\\)[[:space:]]+\\(.+\\)\\'" args)
        (error "Review requires a judgment and reason"))
      (let* ((judgment (match-string 1 args))
             (reason (string-trim (match-string 2 args)))
             (record
              (superchat-experience--record-review
               judgment need reason)))
        (list :type :echo
              :content
              (format "Review recorded: %s (%s)"
                      judgment (plist-get record :event-id))))))))

(defun superchat-experience--record-review (judgment need reason)
  "Record JUDGMENT (retain, fails-under, or revoke) for NEED.
Shared by the chat `/review' command and the interactive reviewer."
  (when (string-empty-p reason)
    (error "Review reason must not be empty"))
  (pcase judgment
    ((or "retain" "fails-under")
     (superchat-experience-record-relation
      (list :session-id
            (plist-get (plist-get need :intervention) :session-id)
            :actor-id "user"
            :correlation-id
            (format "review:%s"
                    (plist-get (plist-get need :intervention)
                               :event-id))
            :causation-id
            (plist-get (plist-get need :outcome) :event-id)
            :relation (if (equal judgment "retain")
                          'applies-under
                        'fails-under)
            :from (plist-get need :candidate)
            :to (plist-get need :context)
            :scope (plist-get need :scope)
            :basis (plist-get need :basis)
            :reason reason)))
    ("revoke"
     (superchat-experience-record-disposition
      (list :session-id
            (plist-get (plist-get need :promotion) :session-id)
            :actor-id "user" :candidate (plist-get need :candidate)
            :scope (plist-get need :scope) :decision 'revoke
            :prior (plist-get need :promotion)
            :basis (plist-get need :basis) :reason reason)))
    (_ (error "Unknown review judgment: %s" judgment))))

(defun superchat-experience--review-label (need)
  "Return a human-readable label for NEED, or nil."
  (let* ((candidate (plist-get need :candidate))
         (event (and candidate
                     (car (superchat-experience--events-for-reference
                           candidate))))
         (statement (and event
                         (plist-get (plist-get event :payload)
                                    :statement))))
    (and (stringp statement)
         (let ((text (string-trim (replace-regexp-in-string
                                   "[\n\r]+" " " statement))))
           (if (> (length text) 60)
               (concat (substring text 0 57) "...")
             text)))))

;;; Structural eye: a compression-based applicability ranker.
;; Read-only.  It never gates behavior; the reviewer sees the hint and
;; decides.  Based on the compression-prediction equivalence
;; (https://nathan.rs/posts/gzip-lm/) and validated by
;; experiments/experience-learning-minimal/compression-probe.el.

(defun superchat-experience--gzip-size (text)
  "Return DEFLATE compressed byte size of TEXT via the system gzip."
  (unless (executable-find "gzip")
    (error "gzip binary not found"))
  (let ((in (make-temp-file "scx-" nil ".txt")))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (write-region (point-min) (point-max) in nil 'quiet)
          (with-temp-buffer
            (call-process "gzip" nil t nil "-c" in)
            (buffer-size)))
      (delete-file in))))

(defun superchat-experience--run-skeletons ()
  "Fold tool events into per-Run behavioral skeletons.
Returns ((run-id steps recovered-p) ...) ordered by first event.
STEPS are ordered (tool-name . ok|failed) pairs.  RECOVERED-P is
non-nil when a tool that failed earlier in the Run later completed."
  (let* ((sessions (superchat-view-session-ids))
         (events (superchat-experience--view-events sessions))
         (runs nil))
    (dolist (event events)
      (when (member (plist-get event :type)
                    '("tool.completed" "tool.failed"))
        (let ((run-id (plist-get event :run-id))
              (tool-name
               (plist-get (plist-get event :payload) :tool-name)))
          (when (and run-id tool-name)
            (let ((cell (assoc run-id runs)))
              (unless cell
                (setq cell (cons run-id nil))
                (push cell runs))
              ;; Prepend; the skeleton is re-sorted by storage-id below.
              (setcdr cell
                      (cons (list (plist-get event :storage-id)
                                  tool-name
                                  (if (equal (plist-get event :type)
                                             "tool.completed")
                                      'ok 'failed))
                            (cdr cell))))))))
    (mapcar
     (lambda (cell)
       (let* ((ordered
               (mapcar (lambda (s) (cons (nth 1 s) (nth 2 s)))
                       (sort (cdr cell)
                             (lambda (a b)
                               (< (car a) (car b))))))
              (failed-tools nil)
              (recovered-p nil))
         (dolist (step ordered)
           (if (eq (cdr step) 'failed)
               (cl-pushnew (car step) failed-tools)
             (when (member (car step) failed-tools)
               (setq recovered-p t))))
         (list (car cell) ordered recovered-p)))
     runs)))

(defun superchat-experience--skeleton-text (skeleton)
  "Serialize SKELETON to the canonical behavioral text."
  (mapconcat
   (lambda (step)
     (format "ACTION %s %s" (car step)
             (if (eq (cdr step) 'ok) "OK" "FAILED")))
   (cadr skeleton) "\n"))

(defun superchat-experience--structural-hint (need)
  "Compression-based applicability hint for review NEED, or nil.
Read-only.  Returns (:margin N :predicted recovered|failed :successes
N :failures N) when the history holds at least 2 Runs per class.  The
margin is the incremental gzip cost of the need's Run skeleton against
the failure corpus minus its cost against the success corpus; a
positive margin means the behavior structurally matches successful
recoveries."
  (let* ((run-id (plist-get (plist-get need :intervention) :run-id))
         (skeletons (and (executable-find "gzip") run-id
                         (superchat-experience--run-skeletons)))
         (own (and skeletons (assoc run-id skeletons))))
    (when own
      (let* ((others (cl-remove own skeletons :test #'equal))
             (successes (cl-remove-if-not (lambda (s) (nth 2 s)) others))
             (failures (cl-remove-if (lambda (s) (nth 2 s)) others)))
        (when (and (>= (length successes) 2) (>= (length failures) 2))
          (let* ((own-text (superchat-experience--skeleton-text own))
                 (success-corpus
                  (mapconcat #'superchat-experience--skeleton-text
                             successes "\n"))
                 (failure-corpus
                  (mapconcat #'superchat-experience--skeleton-text
                             failures "\n"))
                 (success-score
                  (- (superchat-experience--gzip-size
                      (concat success-corpus "\n" own-text))
                     (superchat-experience--gzip-size success-corpus)))
                 (failure-score
                  (- (superchat-experience--gzip-size
                      (concat failure-corpus "\n" own-text))
                     (superchat-experience--gzip-size failure-corpus))))
            (list :margin (- failure-score success-score)
                  :predicted (if (< success-score failure-score)
                                 'recovered 'failed)
                  :successes (length successes)
                  :failures (length failures))))))))

(defun superchat-experience--structural-hint-text (need)
  "Human-readable form of the structural hint for NEED, or nil."
  (let ((hint (superchat-experience--structural-hint need)))
    (when hint
      (format
       (concat "Structural hint: behavior %s (margin %+d bytes; "
               "%d success / %d failure runs)")
       (if (eq (plist-get hint :predicted) 'recovered)
           "matches successful recovery patterns"
         "deviates from successful recovery patterns")
       (plist-get hint :margin)
       (plist-get hint :successes)
       (plist-get hint :failures)))))

(defun superchat-experience--structural-hint-label (need)
  "Compact structural-hint suffix for NEED labels, or the empty string."
  (let ((hint (and (executable-find "gzip")
                   (superchat-experience--structural-hint need))))
    (if hint
        (format " [%+d]" (plist-get hint :margin))
      "")))

;;;###autoload
(defun superchat-review ()
  "Review one open recovery Review Need through the minibuffer."
  (interactive)
  (let* ((view (superchat-experience-review-needs))
         (open (append (plist-get view :open) nil)))
    (if (null open)
        (message "No open recovery Review Need.")
      (let* ((labels
              (mapcar
               (lambda (need)
                 (let ((label (superchat-experience--review-label need)))
                   (concat
                    (format "%s (%s)"
                            (or label "(no statement)")
                            (plist-get (plist-get need :candidate)
                                       :session-id))
                    (superchat-experience--structural-hint-label need))))
               open))
             (choice
              (completing-read "Review need: " labels nil t))
             (need (nth (cl-position choice labels :test #'equal)
                        open))
             (judgment
              (completing-read "Judgment: "
                               '("retain" "fails-under" "revoke")
                               nil t))
             (reason (read-string
                      (format "%s reason: " judgment)))
             (record
              (superchat-experience--record-review
               judgment need reason)))
        (message "Review recorded: %s (%s)"
                 judgment (plist-get record :event-id))))))

(provide 'superchat-experience)

;;; superchat-experience.el ends here
