;;; superchat-closure.el --- Tape-derived Runtime judgments -*- lexical-binding: t; -*-

;;; Commentary:

;; This module exposes one pure Interface: `superchat-closure-judge'.  Tape is
;; authoritative; the validation state and Operational Boundary are private.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst superchat-closure--semantic-environment
  '(:closure-contract "closure-v0"
    :event-schema 1
    :authority-policy-contract "authority-v1")
  "Static semantic environment used by the v0 interpreter.")

(defconst superchat-closure--contract-version
  (plist-get superchat-closure--semantic-environment :closure-contract)
  "Semantic contract implemented by this interpreter.")

(defconst superchat-closure--run-event-types
  '("intent.received" "run.started" "response.completed"
    "tool.requested" "approval.requested" "approval.decided"
    "tool.execution.started" "tool.completed" "tool.failed"
    "decision.snapshot.recorded" "output.first.recorded"
    "run.completed" "run.failed" "run.cancelled" "turn.completed")
  "Contract-v0 event types that require Run and Turn identity.")

(defun superchat-closure--result (verdict query &rest properties)
  "Return a VERDICT for QUERY extended by PROPERTIES."
  (when-let* ((witness (plist-get properties :witness)))
    (setq properties
          (plist-put
           properties :witness
           (plist-put
            (copy-sequence witness) :semantic-environment
            (copy-tree superchat-closure--semantic-environment)))))
  (append (list :verdict verdict :judgment query) properties))

(defun superchat-closure--event-status (event)
  "Return EVENT's normalized payload status."
  (let ((status (plist-get (plist-get event :payload) :status)))
    (if (stringp status) (intern status) status)))

(defun superchat-closure--audit-incomplete-p (event)
  "Return non-nil when EVENT is an explicit audit-incomplete seal."
  (let ((payload (plist-get event :payload)))
    (and (eq 'audit-incomplete (superchat-closure--event-status event))
         (eq t (plist-get payload :audit-incomplete)))))

(defun superchat-closure--payload-symbol (event key)
  "Return EVENT payload KEY normalized to a symbol."
  (let ((value (plist-get (plist-get event :payload) key)))
    (if (stringp value) (intern value) value)))

(defun superchat-closure--entry (state key id)
  "Return ID from the alist stored at STATE KEY."
  (cdr (assoc id (plist-get state key))))

(defun superchat-closure--put-entry (state key id value)
  "Store VALUE under ID in the alist at STATE KEY."
  (plist-put state key
             (cons (cons id value)
                   (assoc-delete-all id (plist-get state key)))))

(defun superchat-closure--record-event (node event &optional disposition)
  "Return NODE updated by EVENT and optional DISPOSITION."
  (let ((node (copy-sequence node)))
    (setq node
          (plist-put node :event-ids
                     (append (plist-get node :event-ids)
                             (list (plist-get event :event-id)))))
    (if disposition
        (plist-put node :disposition disposition)
      node)))

(defun superchat-closure--approval-accounted-p (approval)
  "Return non-nil when APPROVAL has an allow or deny decision."
  (memq (plist-get approval :disposition) '(allow deny)))

(defun superchat-closure--effect-accounted-p (effect)
  "Return non-nil when EFFECT has an accounted disposition."
  (memq (plist-get effect :disposition)
        '(discharged denied blocked failed cancelled)))

(defun superchat-closure--pending-entries (state key predicate)
  "Return STATE entries at KEY for which PREDICATE is nil."
  (cl-remove-if (lambda (entry) (funcall predicate (cdr entry)))
                (plist-get state key)))

(defun superchat-closure--run-ready-p (state)
  "Return non-nil when STATE has no pending Effect or Approval."
  (and (null (superchat-closure--pending-entries
              state :effects #'superchat-closure--effect-accounted-p))
       (null (superchat-closure--pending-entries
              state :approvals #'superchat-closure--approval-accounted-p))))

(defun superchat-closure--terminal-ready-p (state kind)
  "Return non-nil when STATE is ready for terminal KIND."
  (and (memq kind '(completed failed timed-out cancelled))
       (plist-get state :started)
       (not (plist-get state :terminal))
       (superchat-closure--run-ready-p state)
       (or (eq kind 'cancelled)
           (eq kind
               (superchat-closure--event-status
                (plist-get state :response))))))

(defun superchat-closure--tool-name-matches-p (payload node)
  "Return non-nil when PAYLOAD names the Tool recorded by NODE."
  (let ((tool-name (plist-get payload :tool-name)))
    (and (stringp tool-name)
         (not (string-empty-p tool-name))
         (equal tool-name (plist-get node :tool-name)))))

(defun superchat-closure--authority-policy-version-supported-p (version)
  "Return non-nil when VERSION belongs to the current Authority contract."
  (let ((contract
         (plist-get superchat-closure--semantic-environment
                    :authority-policy-contract)))
    (and (stringp version)
         (or (equal version contract)
             (string-prefix-p (concat contract "/") version)))))

(defun superchat-closure--residual-node-ids (state)
  "Return IDs of unaccounted Effect and Approval nodes in STATE."
  (append
   (mapcar (lambda (entry) (plist-get (cdr entry) :id))
           (superchat-closure--pending-entries
            state :approvals #'superchat-closure--approval-accounted-p))
   (mapcar (lambda (entry) (plist-get (cdr entry) :id))
           (superchat-closure--pending-entries
            state :effects #'superchat-closure--effect-accounted-p))))

(defun superchat-closure--invalid (query event reason &optional residual)
  "Return an invalid result for QUERY at EVENT with REASON and RESIDUAL."
  (superchat-closure--result
   'invalid query
   :witness (list :contract superchat-closure--contract-version
                  :event-ids
                  (when-let* ((id (plist-get event :event-id))) (list id))
                  :reason reason
                  :residual-node-ids residual)))

(defun superchat-closure--unknown (query event reason)
  "Return an unknown judgment for QUERY at EVENT with REASON."
  (superchat-closure--result
   'unknown query
   :witness (list :contract superchat-closure--contract-version
                  :event-ids
                  (when-let* ((id (plist-get event :event-id))) (list id))
                  :reason reason)))

(defun superchat-closure--envelope-valid-p (event)
  "Return non-nil when EVENT has a stable, classifiable envelope."
  (and (stringp (plist-get event :type))
       (not (string-empty-p (plist-get event :type)))
       (stringp (plist-get event :event-id))
       (not (string-empty-p (plist-get event :event-id)))
       (integerp (plist-get event :schema))
       (> (plist-get event :schema) 0)
       (stringp (plist-get event :session-id))
       (not (string-empty-p (plist-get event :session-id)))))

(defun superchat-closure--apply-event (state event query)
  "Apply one Runtime EVENT to STATE for QUERY.
Return STATE or a final invalid/unknown result."
  (let ((type (plist-get event :type))
        (status (superchat-closure--event-status event)))
    (pcase type
      ((or "capability.activated"
           "topology.changed"
           "authority.policy.selected"
           "workspace.bound"
           "workspace.unbound"
           "attachment.added"
           "anchor.created"
           "extension.activated"
           "memory.captured"
           "memory.corrected"
           "memory.raw.added"
           "memory.rejected"
           "memory.reviewed"
           "plan.proposed"
           "plan.approved"
           "claim.recorded"
           "feedback.recorded"
           "exercise.evaluated"
           "applicability.relation.recorded"
           "candidate.disposition.recorded"
           "experience.intervention.recorded"
           "decision.snapshot.recorded"
           "output.first.recorded")
       (plist-put
        state :known-irrelevant-event-ids
        (append (plist-get state :known-irrelevant-event-ids)
                (list (plist-get event :event-id)))))
      ("intent.received"
       (if (plist-get state :intent)
           (superchat-closure--invalid query event 'duplicate-intent)
         (plist-put state :intent event)))
      ("run.started"
       (cond
        ((not (plist-get state :intent))
         (superchat-closure--invalid query event 'missing-intent))
        ((plist-get state :started)
         (superchat-closure--invalid query event 'duplicate-run-start))
        ((and (plist-get event :causation-id)
              (not (equal
                    (plist-get event :causation-id)
                    (plist-get (plist-get state :intent) :event-id))))
         (superchat-closure--invalid
          query event 'run-start-causation-mismatch))
        (t (plist-put state :started event))))
      ("response.completed"
       (if (or (not (memq status '(completed failed timed-out)))
               (not (plist-get state :started))
               (plist-get state :response)
               (plist-get state :terminal))
           (superchat-closure--invalid query event 'response-out-of-order)
         (plist-put state :response event)))
      ("tool.requested"
       (let* ((payload (plist-get event :payload))
              (tool-name (plist-get payload :tool-name))
              (execution-id (plist-get payload :execution-id))
              (request-hash (plist-get payload :request-hash)))
         (cond
          ((or (not (plist-get state :started))
               (plist-get state :terminal))
           (superchat-closure--invalid query event 'tool-request-out-of-order))
          ((or (not (stringp tool-name))
               (string-empty-p tool-name)
               (not (stringp execution-id))
               (not (stringp request-hash))
               (null (plist-get payload :effects)))
           (superchat-closure--invalid query event 'invalid-tool-request))
          ((superchat-closure--entry state :effects execution-id)
           (superchat-closure--invalid query event 'duplicate-execution-id))
          ((not (superchat-closure--authority-policy-version-supported-p
                 (plist-get payload :policy-version)))
           (superchat-closure--unknown
            query event 'unsupported-authority-policy-contract))
          (t
           (superchat-closure--put-entry
            state :effects execution-id
            (list :id (format "effect:%s" execution-id)
                  :tool-name tool-name
                  :execution-id execution-id
                  :request-hash request-hash
                  :policy-version (plist-get payload :policy-version)
                  :effects (copy-tree (plist-get payload :effects))
                  :decision (superchat-closure--payload-symbol event :decision)
                  :disposition 'requested
                  :event-ids (list (plist-get event :event-id))))))))
      ("approval.requested"
       (let* ((payload (plist-get event :payload))
              (execution-id (plist-get payload :execution-id))
              (request-hash (plist-get payload :request-hash))
              (effect (superchat-closure--entry
                       state :effects execution-id)))
         (cond
          ((or (not effect)
               (not (equal request-hash (plist-get effect :request-hash)))
               (not (eq 'ask (plist-get effect :decision))))
           (superchat-closure--invalid query event 'approval-not-requested))
          ((not (superchat-closure--tool-name-matches-p payload effect))
           (superchat-closure--invalid query event 'tool-identity-mismatch))
          ((superchat-closure--entry state :approvals request-hash)
           (superchat-closure--invalid query event 'duplicate-approval))
          ((not (superchat-closure--authority-policy-version-supported-p
                 (plist-get payload :policy-version)))
           (superchat-closure--unknown
            query event 'unsupported-authority-policy-contract))
          ((not (equal (plist-get payload :policy-version)
                       (plist-get effect :policy-version)))
           (superchat-closure--invalid
            query event 'authority-policy-version-mismatch))
          (t
           (superchat-closure--put-entry
            state :approvals request-hash
            (list :id (format "approval:%s" request-hash)
                  :tool-name (plist-get effect :tool-name)
                  :execution-id execution-id
                  :request-hash request-hash
                  :policy-version (plist-get payload :policy-version)
                  :disposition 'requested
                  :event-ids (list (plist-get event :event-id))))))))
      ("approval.decided"
       (let* ((payload (plist-get event :payload))
              (request-hash (plist-get payload :request-hash))
              (approval (superchat-closure--entry
                         state :approvals request-hash))
              (execution-id (plist-get payload :execution-id))
              (decision (superchat-closure--payload-symbol event :decision))
              (policy-version (plist-get payload :policy-version)))
         (cond
          ((or (not approval)
               (not (equal execution-id
                           (plist-get approval :execution-id)))
               (not (memq decision '(allow deny)))
               (superchat-closure--approval-accounted-p approval))
           (superchat-closure--invalid query event 'invalid-approval-decision))
          ((not (superchat-closure--tool-name-matches-p payload approval))
           (superchat-closure--invalid query event 'tool-identity-mismatch))
          ((not (superchat-closure--authority-policy-version-supported-p
                 policy-version))
           (superchat-closure--unknown
            query event 'unsupported-authority-policy-contract))
          ((not (equal policy-version
                       (plist-get approval :policy-version)))
           (superchat-closure--invalid
            query event 'authority-policy-version-mismatch))
          (t
           (superchat-closure--put-entry
            state :approvals request-hash
            (superchat-closure--record-event approval event decision))))))
      ("tool.execution.started"
       (let* ((payload (plist-get event :payload))
              (execution-id (plist-get payload :execution-id))
              (effect (superchat-closure--entry state :effects execution-id))
              (approval
               (and effect
                    (superchat-closure--entry
                     state :approvals (plist-get effect :request-hash))))
              (allowed
               (or (eq 'allow (plist-get effect :decision))
                   (eq 'allow (plist-get approval :disposition)))))
         (cond
          ((or (not effect)
               (not (equal (plist-get payload :request-hash)
                           (plist-get effect :request-hash)))
               (not allowed)
               (not (eq 'requested (plist-get effect :disposition))))
           (superchat-closure--invalid query event 'effect-not-ready))
          ((not (superchat-closure--tool-name-matches-p payload effect))
           (superchat-closure--invalid query event 'tool-identity-mismatch))
          (t
           (superchat-closure--put-entry
            state :effects execution-id
            (superchat-closure--record-event effect event 'executing))))))
      ("tool.completed"
       (let* ((payload (plist-get event :payload))
              (execution-id (plist-get payload :execution-id))
              (effect (superchat-closure--entry state :effects execution-id)))
         (cond
          ((or (not effect)
               (not (equal (plist-get payload :request-hash)
                           (plist-get effect :request-hash)))
               (not (eq 'executing (plist-get effect :disposition))))
           (superchat-closure--invalid query event 'effect-not-executing))
          ((not (superchat-closure--tool-name-matches-p payload effect))
           (superchat-closure--invalid query event 'tool-identity-mismatch))
          (t
           (superchat-closure--put-entry
            state :effects execution-id
            (superchat-closure--record-event effect event 'discharged))))))
      ("tool.failed"
       (let* ((payload (plist-get event :payload))
              (execution-id (plist-get payload :execution-id))
              (effect (superchat-closure--entry state :effects execution-id))
              (failure (superchat-closure--payload-symbol event :status))
              (approval
               (and effect
                    (superchat-closure--entry
                     state :approvals (plist-get effect :request-hash))))
              (valid-transition
               (pcase failure
                 ('failed
                  (eq 'executing (plist-get effect :disposition)))
                 ((or 'blocked 'denied)
                  (and (eq 'requested (plist-get effect :disposition))
                       (eq 'deny (plist-get effect :decision))))
                 ('cancelled
                  (and (eq 'requested (plist-get effect :disposition))
                       (eq 'ask (plist-get effect :decision))
                       (eq 'deny (plist-get approval :disposition)))))))
         (cond
          ((or (not effect)
               (not (equal (plist-get payload :request-hash)
                           (plist-get effect :request-hash)))
               (superchat-closure--effect-accounted-p effect)
               (not valid-transition))
           (superchat-closure--invalid query event 'invalid-effect-failure))
          ((not (superchat-closure--tool-name-matches-p payload effect))
           (superchat-closure--invalid query event 'tool-identity-mismatch))
          (t
           (superchat-closure--put-entry
            state :effects execution-id
            (superchat-closure--record-event effect event failure))))))
      ("run.completed"
       (if (or (not (eq status 'completed))
               (not (superchat-closure--terminal-ready-p
                     state 'completed)))
           (superchat-closure--invalid
            query event 'run-not-ready
            (superchat-closure--residual-node-ids state))
         (plist-put state :terminal event)))
      ("run.failed"
       (if (or (not (memq status '(failed timed-out audit-incomplete)))
               (if (superchat-closure--audit-incomplete-p event)
                   (plist-get state :terminal)
                 (not (superchat-closure--terminal-ready-p state status))))
           (superchat-closure--invalid
            query event 'run-not-ready
            (superchat-closure--residual-node-ids state))
         (plist-put state :terminal event)))
      ("run.cancelled"
       (if (or (not (eq status 'cancelled))
               (not (superchat-closure--terminal-ready-p
                     state 'cancelled)))
           (superchat-closure--invalid
            query event 'run-not-ready
            (superchat-closure--residual-node-ids state))
         (plist-put state :terminal event)))
      ("turn.completed"
       (if (or (not (plist-get state :terminal))
               (plist-get state :turn-terminal)
               (not (eq status
                        (superchat-closure--event-status
                         (plist-get state :terminal)))))
           (superchat-closure--invalid query event 'turn-not-ready)
         (plist-put state :turn-terminal event)))
      ((or "workspace.artifact.added"
           "workspace.artifact.changed"
           "workspace.artifact.removed")
       (let* ((workspace-id (plist-get event :workspace-id))
              (artifact-id (plist-get (plist-get event :payload) :artifact-id))
              (key (and workspace-id artifact-id
                        (format "%s/%s" workspace-id artifact-id)))
              (artifact (and key
                             (superchat-closure--entry
                              state :artifacts key))))
         (cond
          ((or (not (stringp workspace-id))
               (not (stringp artifact-id)))
           (superchat-closure--invalid query event 'invalid-artifact-identity))
          ((and (not (equal type "workspace.artifact.added"))
                (not artifact))
           (superchat-closure--invalid query event 'unknown-artifact))
          (t
           (setq state
                 (superchat-closure--put-entry
                  state :artifacts key
                  (if artifact
                      (superchat-closure--record-event
                       artifact event
                       (if (equal type "workspace.artifact.removed")
                           'removed 'present))
                    (list :id (format "artifact:%s" key)
                          :workspace-id workspace-id
                          :artifact-id artifact-id
                          :disposition 'present
                          :event-ids (list (plist-get event :event-id))))))
           (plist-put
            state :artifact-event-ids
            (append (plist-get state :artifact-event-ids)
                    (list (plist-get event :event-id))))))))
      (_ (superchat-closure--unknown query event 'unknown-event-type)))))

(defun superchat-closure--interpret (query trace)
  "Interpret TRACE for QUERY and return private state or a final result."
  ;; ponytail: v0 folds one explicit trace; add checkpoints only after shadow
  ;; replay shows this O(n) path is materially too slow.
  (let ((run-id (plist-get query :run-id))
        (seen (make-hash-table :test #'equal))
        (state (list :run-id (plist-get query :run-id)
                     :turn-id (plist-get query :turn-id))))
    (catch 'done
      (dolist (event trace)
        (unless (superchat-closure--envelope-valid-p event)
          (throw 'done
                 (superchat-closure--invalid
                  query event 'invalid-envelope)))
        (let ((event-id (plist-get event :event-id))
              (event-run-id (plist-get event :run-id))
              (event-turn-id (plist-get event :turn-id))
              (type (plist-get event :type)))
          (when (gethash event-id seen)
            (throw 'done
                   (superchat-closure--invalid
                    query event 'duplicate-event-id)))
          ;; Scope is decided from stable envelope identity before the event
          ;; schema is interpreted.
          (if (and run-id
                   (stringp event-run-id)
                   (not (string-empty-p event-run-id))
                   (not (equal run-id event-run-id)))
              (progn
                (puthash event-id t seen)
                (setq state
                      (plist-put
                       state :out-of-scope-event-ids
                       (append (plist-get state :out-of-scope-event-ids)
                               (list event-id)))))
            (unless (equal
                     (plist-get superchat-closure--semantic-environment
                                :event-schema)
                     (plist-get event :schema))
              (throw 'done
                     (superchat-closure--unknown
                      query event 'unsupported-schema)))
            (when (member type superchat-closure--run-event-types)
              (when (or (not (and (stringp event-run-id)
                                  (not (string-empty-p event-run-id))))
                        (not (and (stringp event-turn-id)
                                  (not (string-empty-p event-turn-id))))
                        (and (plist-get state :turn-id)
                             (not (equal (plist-get state :turn-id)
                                         event-turn-id))))
                (throw 'done
                       (superchat-closure--invalid
                        query event 'invalid-run-identity)))
              (unless (plist-get state :turn-id)
                (setq state (plist-put state :turn-id event-turn-id))))
            (let ((causation-id (plist-get event :causation-id)))
              (when (and causation-id (not (gethash causation-id seen)))
                (throw 'done
                       (superchat-closure--unknown
                        query event 'missing-causal-ancestor))))
            (puthash event-id t seen)
            (let ((next (superchat-closure--apply-event
                         state event query)))
              (when (plist-get next :verdict)
                (throw 'done next))
              (setq state next)
              (setq state
                    (plist-put
                     state :interpreted-event-ids
                     (append (plist-get state :interpreted-event-ids)
                             (list event-id))))
              (when (member type superchat-closure--run-event-types)
                (setq state
                      (plist-put
                       state :relevant-event-ids
                       (append (plist-get state :relevant-event-ids)
                               (list event-id)))))))))
      state)))

(defun superchat-closure--pending-effects (query state)
  "Answer pending Effect QUERY from interpreted STATE."
  (let* ((pending
          (superchat-closure--pending-entries
           state :effects #'superchat-closure--effect-accounted-p))
         (effect-ids (mapcar (lambda (entry) (plist-get (cdr entry) :id))
                             pending))
         (accounted-approvals
          (cl-remove-if-not
           (lambda (entry)
             (superchat-closure--approval-accounted-p (cdr entry)))
           (plist-get state :approvals)))
         (accounted-effects
          (cl-remove-if-not
           (lambda (entry)
             (superchat-closure--effect-accounted-p (cdr entry)))
           (plist-get state :effects))))
    (superchat-closure--result
     (if pending 'open 'proved)
     (list :kind 'pending-effects
           :run-id (plist-get query :run-id)
           :effects effect-ids)
     :witness
     (list :contract superchat-closure--contract-version
           :event-ids (plist-get state :relevant-event-ids)
           :rules '(approval-accounting effect-accounting)
           :accounted-node-ids
           (append
            (mapcar (lambda (entry) (plist-get (cdr entry) :id))
                    (reverse accounted-approvals))
            (mapcar (lambda (entry) (plist-get (cdr entry) :id))
                    (reverse accounted-effects)))
           :accounted
           (append
            (mapcar (lambda (entry)
                      (cons (plist-get (cdr entry) :id)
                            (plist-get (cdr entry) :disposition)))
                    (reverse accounted-approvals))
            (mapcar (lambda (entry)
                      (cons (plist-get (cdr entry) :id)
                            (plist-get (cdr entry) :disposition)))
                    (reverse accounted-effects)))
           :accounted-events
           (append
            (mapcar (lambda (entry)
                      (cons (plist-get (cdr entry) :id)
                            (plist-get (cdr entry) :event-ids)))
                    (reverse accounted-approvals))
            (mapcar (lambda (entry)
                      (cons (plist-get (cdr entry) :id)
                            (plist-get (cdr entry) :event-ids)))
                    (reverse accounted-effects)))))))

(defun superchat-closure--pending-approvals (query state)
  "Answer pending Approval QUERY from interpreted STATE."
  (let* ((pending
          (superchat-closure--pending-entries
           state :approvals #'superchat-closure--approval-accounted-p))
         (accounted
          (cl-remove-if-not
           (lambda (entry)
             (superchat-closure--approval-accounted-p (cdr entry)))
           (plist-get state :approvals))))
    (superchat-closure--result
     (if pending 'open 'proved)
     (list :kind 'pending-approvals
           :run-id (plist-get query :run-id)
           :approvals
           (mapcar (lambda (entry) (plist-get (cdr entry) :id)) pending))
     :witness
     (list :contract superchat-closure--contract-version
           :event-ids (plist-get state :relevant-event-ids)
           :rules '(approval-accounting)
           :accounted-node-ids
           (mapcar (lambda (entry) (plist-get (cdr entry) :id))
                   (reverse accounted))
           :accounted-events
           (mapcar (lambda (entry)
                     (cons (plist-get (cdr entry) :id)
                           (plist-get (cdr entry) :event-ids)))
                   (reverse accounted))))))

(defun superchat-closure--residual-boundary (query state)
  "Answer residual Boundary QUERY from interpreted STATE."
  (let ((ids (superchat-closure--residual-node-ids state)))
    (superchat-closure--result
     (if ids 'open 'proved)
     (list :kind 'residual-boundary
           :run-id (plist-get query :run-id)
           :nodes ids)
     :witness
     (list :contract superchat-closure--contract-version
           :event-ids (plist-get state :relevant-event-ids)
           :residual-node-ids ids
           :rules '(boundary-accounting)))))

(defun superchat-closure--relevant-event-ids-through (state event)
  "Return STATE's relevant event IDs through EVENT, inclusive."
  (let ((target (plist-get event :event-id)) result)
    (catch 'done
      (dolist (id (plist-get state :relevant-event-ids) (nreverse result))
        (push id result)
        (when (equal id target)
          (throw 'done (nreverse result)))))))

(defun superchat-closure--ready-terminal (query state)
  "Answer pre-terminal readiness QUERY from interpreted STATE."
  (let* ((kind (plist-get query :terminal-kind))
         (ready (superchat-closure--terminal-ready-p state kind)))
    (superchat-closure--result
     (if ready 'proved 'open)
     (list :kind 'ready-terminal
           :run-id (plist-get query :run-id)
           :terminal-kind kind)
     :witness
     (list :contract superchat-closure--contract-version
           :event-ids (plist-get state :relevant-event-ids)
           :residual-node-ids (superchat-closure--residual-node-ids state)
           :reason (unless ready 'terminal-preconditions-open)
           :rules
           (when ready
             (append
              (list 'run-started)
              (unless (eq kind 'cancelled) (list 'response-completed))
              (list (intern (format "ready-%s" kind)))))))))

(defun superchat-closure--durable-run-terminal (query state)
  "Answer durable Run terminal QUERY from interpreted STATE."
  (let ((terminal (plist-get state :terminal)))
    (if (not terminal)
        (superchat-closure--result
         'open query
         :witness (list :contract superchat-closure--contract-version
                        :event-ids
                        (plist-get state :relevant-event-ids)
                        :reason 'terminal-seal-absent))
      (let ((terminal-kind (superchat-closure--event-status terminal))
            (audit-incomplete
             (superchat-closure--audit-incomplete-p terminal)))
        (superchat-closure--result
         'proved
         (append
          (list :kind 'durable-run-terminal
                :run-id (plist-get query :run-id)
                :terminal-kind terminal-kind)
          (when audit-incomplete
            '(:audit-incomplete t)))
         :witness
         (list
          :contract superchat-closure--contract-version
          :event-ids
          (superchat-closure--relevant-event-ids-through state terminal)
          :node-ids
          (list (format "run:%s" (plist-get query :run-id))
                (format "obligation:%s:%s"
                        (plist-get query :run-id)
                        (if audit-incomplete "audit-terminal" "outcome")))
          :residual-node-ids
          (when audit-incomplete
            (superchat-closure--residual-node-ids state))
          :rules
          (if audit-incomplete
              '(audit-incomplete-terminal-seal)
            (append
             (list 'run-started)
             (unless (eq terminal-kind 'cancelled)
               (list 'response-completed))
             (list (intern (format "ready-%s" terminal-kind))
                   'terminal-seal)))))))))

(defun superchat-closure--durable-turn-terminal (query state)
  "Answer durable Turn terminal QUERY from interpreted STATE."
  (let ((terminal (plist-get state :terminal))
        (turn-terminal (plist-get state :turn-terminal)))
    (if (or (not terminal) (not turn-terminal))
        (superchat-closure--result
         'open query
         :witness (list :contract superchat-closure--contract-version
                        :event-ids
                        (plist-get state :relevant-event-ids)
                        :reason 'turn-terminal-seal-absent))
      (let ((kind (superchat-closure--event-status turn-terminal)))
        (superchat-closure--result
         'proved
         (append
          (list :kind 'durable-turn-terminal
                :turn-id (plist-get query :turn-id)
                :terminal-kind kind)
          (when (superchat-closure--audit-incomplete-p terminal)
            '(:audit-incomplete t)))
         :witness
         (list :contract superchat-closure--contract-version
               :event-ids
               (superchat-closure--relevant-event-ids-through
                state turn-terminal)
               :node-ids
               (list (format "obligation:%s:terminal"
                             (plist-get query :turn-id)))
               :rules '(durable-run-terminal turn-terminal-seal)))))))

(defun superchat-closure--trace-well-formed (query interpretation)
  "Return trace well-formedness for QUERY and INTERPRETATION."
  (let ((existing (plist-get interpretation :verdict)))
    (if existing
        (let ((result (copy-sequence interpretation)))
          (plist-put result :judgment
                     (append query (list :status existing))))
      (superchat-closure--result
       'valid (append query '(:status valid))
       :witness
       (list :contract superchat-closure--contract-version
             :event-ids
             (plist-get interpretation :interpreted-event-ids)
             :out-of-scope-event-ids
             (plist-get interpretation :out-of-scope-event-ids)
             :known-irrelevant-event-ids
             (plist-get interpretation :known-irrelevant-event-ids)
             :artifact-event-ids
             (plist-get interpretation :artifact-event-ids)
             :artifact-node-ids
             (mapcar (lambda (entry) (plist-get (cdr entry) :id))
                     (reverse (plist-get interpretation :artifacts)))
             :rules '(trace-envelope event-order transition-semantics))))))

;;;###autoload
(defun superchat-closure-judge (query trace)
  "Return a Tape-derived Runtime judgment for QUERY over TRACE.

QUERY is a plist containing `:kind' and the identity required by that kind.
TRACE is an ordered list of normalized Runtime event plists; in-scope events
must use contract-v0 schema 1.
The result contains `:verdict', `:judgment', and an auditable `:witness'.
Contract v0 never mutates TRACE or exposes its private Boundary state."
  (unless (and (listp query) (symbolp (plist-get query :kind)))
    (signal 'wrong-type-argument (list 'closure-query-p query)))
  (unless (listp trace)
    (signal 'wrong-type-argument (list 'listp trace)))
  (let ((kind (plist-get query :kind)))
    (when (memq kind '(trace-well-formed pending-effects pending-approvals
                       residual-boundary ready-terminal
                       durable-run-terminal durable-turn-terminal))
      (unless (and (stringp (plist-get query :run-id))
                   (not (string-empty-p (plist-get query :run-id))))
        (signal 'wrong-type-argument
                (list 'closure-run-id-p (plist-get query :run-id)))))
    (when (and (eq kind 'durable-turn-terminal)
               (not (and (stringp (plist-get query :turn-id))
                         (not (string-empty-p
                               (plist-get query :turn-id))))))
      (signal 'wrong-type-argument
              (list 'closure-turn-id-p (plist-get query :turn-id)))))
  (let ((state (superchat-closure--interpret query trace)))
    (if (eq 'trace-well-formed (plist-get query :kind))
        (superchat-closure--trace-well-formed query state)
      (if (plist-get state :verdict)
        (if (eq 'invalid (plist-get state :verdict))
            (plist-put (copy-sequence state) :verdict 'unknown)
          state)
        (pcase (plist-get query :kind)
          ('durable-run-terminal
           (superchat-closure--durable-run-terminal query state))
          ('durable-turn-terminal
           (superchat-closure--durable-turn-terminal query state))
          ('pending-effects
           (superchat-closure--pending-effects query state))
          ('pending-approvals
           (superchat-closure--pending-approvals query state))
          ('residual-boundary
           (superchat-closure--residual-boundary query state))
          ('ready-terminal
           (superchat-closure--ready-terminal query state))
          (_ (superchat-closure--result
              'unknown query
              :witness (list :contract superchat-closure--contract-version
                             :event-ids nil
                             :reason 'unsupported-query))))))))

(provide 'superchat-closure)

;;; superchat-closure.el ends here
