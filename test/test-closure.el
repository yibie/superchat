;;; test-closure.el --- Closure judgment trace tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'superchat-closure)

(defun superchat-closure-test--event (type id &rest properties)
  "Return a schema-v1 Runtime event of TYPE and ID with PROPERTIES."
  (append properties
          (list :type type
                :event-id id
                :schema 1
                :session-id "session-1"
                :turn-id "turn-1"
                :run-id "run-1"
                :actor-id "lead"
                :payload nil)))

(defun superchat-closure-test--successful-run ()
  "Return the canonical successful Run trace."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start"
    :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "response.completed" "evt-response"
    :causation-id "evt-start"
    :payload '(:status completed :content-size 4))
   (superchat-closure-test--event
    "run.completed" "evt-run-terminal"
    :causation-id "evt-response"
    :payload '(:status completed))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal"
    :payload '(:status completed))))

(ert-deftest closure/late-admission-does-not-legitimize-illegal-audit ()
  "Non-admission is not an interpretation exception for forged late audit."
  (let ((base (superchat-closure-test--successful-run)))
    (dolist (type '("tool.requested" "tool.execution.started" "tool.completed"
                    "approval.decided"))
      (let ((trace (append base (list (superchat-closure-test--event
                                      type "late-illegal"
                                      :payload '(:execution-id "unknown" :request-hash "hash"
                                                 :tool-name "read" :decision allow))))))
        (should (eq 'invalid (plist-get (superchat-closure-judge
                                        '(:kind trace-well-formed :run-id "run-1") trace) :verdict)))
        (should (eq 'unknown (plist-get (superchat-closure-judge
                                        '(:kind durable-run-terminal :run-id "run-1") trace) :verdict))))))
  (should (eq 'unknown (plist-get (superchat-closure-judge
                                  '(:kind trace-well-formed :run-id "run-1")
                                  (list (superchat-closure-test--event "tool.late" "unknown"))) :verdict))))

(ert-deftest closure/team-observations-exact-seven-and-combined ()
  "Every approved observation and their combined trace are explicitly known."
  (let* ((types '("team.task.created" "team.task.updated" "team.message.sent"
                  "team.decision.published" "subagent.started"
                  "subagent.completed" "subagent.failed"))
         (events (cl-loop for type in types for n from 0 collect
                          (superchat-closure-test--event type (format "obs-%s" n))))
         (base (superchat-closure-test--successful-run))
         (verdicts
          (mapcar (lambda (event)
                    (plist-get (superchat-closure-judge
                                '(:kind trace-well-formed :run-id "run-1")
                                (append base (list event))) :verdict)) events)))
    (should (equal '(valid valid valid valid valid valid valid) verdicts))
    (let ((result (superchat-closure-judge
                   '(:kind trace-well-formed :run-id "run-1")
                   (append base events))))
      (should (eq 'valid (plist-get result :verdict)))
      (should (equal (mapcar (lambda (event) (plist-get event :event-id)) events)
                     (plist-get (plist-get result :witness)
                                :known-irrelevant-event-ids))))))

(ert-deftest closure/team-observations-preserve-terminal-and-open-obligations ()
  "The finite Team observations explain work but never discharge it."
  (dolist (type '("team.task.created" "team.task.updated" "team.message.sent"
                  "team.decision.published" "subagent.started"
                  "subagent.completed" "subagent.failed"))
    (ert-info ((format "observation %s" type))
      (let* ((base (superchat-closure-test--successful-run))
             (observation (superchat-closure-test--event type "observation"))
             (trace (append base (list observation)))
             (well (superchat-closure-judge
                    '(:kind trace-well-formed :run-id "run-1") trace)))
        (should (eq 'valid (plist-get well :verdict)))
        (should (equal '("observation")
                       (plist-get (plist-get well :witness)
                                  :known-irrelevant-event-ids)))
        (should (eq 'proved (plist-get (superchat-closure-judge
                                       '(:kind durable-run-terminal :run-id "run-1")
                                       trace) :verdict)))
        (dolist (prefix (list nil (seq-take base 2)))
          (should-not
           (eq 'proved (plist-get (superchat-closure-judge
                                   '(:kind durable-run-terminal :run-id "run-1")
                                   (append prefix (list observation))) :verdict))))
        (dolist (decision '(allow ask))
          (let* ((request (superchat-closure-test--event
                           "tool.requested" "request"
                           :payload `(:tool-name "read" :execution-id "exec"
                                      :request-hash "hash" :policy-version "authority-v1"
                                      :effects ((:effect local-read :resource "fixture"))
                                      :decision ,decision)))
                 (next (superchat-closure-test--event
                        (if (eq decision 'ask) "approval.requested" "tool.execution.started")
                        "next" :payload '(:tool-name "read" :execution-id "exec"
                                          :request-hash "hash" :policy-version "authority-v1")))
                 (pending (append (seq-take base 2) (list request next (nth 2 base))))
                 (before (superchat-closure-judge
                          '(:kind ready-terminal :run-id "run-1" :terminal-kind completed)
                          pending))
                 (after (superchat-closure-judge
                         '(:kind ready-terminal :run-id "run-1" :terminal-kind completed)
                         (append pending (list observation)))))
            (should (eq 'valid (plist-get (superchat-closure-judge
                                           '(:kind trace-well-formed :run-id "run-1")
                                           pending) :verdict)))
            (should (eq 'open (plist-get before :verdict)))
            (should (eq (plist-get before :verdict) (plist-get after :verdict)))))))))

(ert-deftest closure/team-observations-do-not-bypass-validation ()
  "Unknown siblings and malformed or illegal traces remain fail-closed."
  (dolist (type '("team.task.deleted" "team.message.received"
                  "team.decision.revoked" "subagent.progress" "team.other"
                  "subagent.completed.extra"))
    (should (eq 'unknown
                (plist-get (superchat-closure-judge
                            '(:kind trace-well-formed :run-id "run-1")
                            (list (superchat-closure-test--event type "unknown")))
                           :verdict))))
  (dolist (properties '((:schema 99) (:event-id nil) (:session-id nil)
                         (:causation-id "missing")))
    (let ((event (apply #'superchat-closure-test--event
                        "team.task.created" "bad" properties)))
      (should (memq (plist-get (superchat-closure-judge
                               '(:kind trace-well-formed :run-id "run-1")
                               (list event)) :verdict) '(invalid unknown)))))
  (let ((observation (superchat-closure-test--event "subagent.completed" "obs")))
    (dolist (trace
             (list (list observation observation)
                   (list observation (superchat-closure-test--event
                                      "run.completed" "illegal"
                                      :payload '(:status completed)))
                   (list observation (superchat-closure-test--event
                                      "tool.completed" "illegal"
                                      :payload '(:call-id "missing")))
                   (list observation (superchat-closure-test--event
                                      "approval.decided" "illegal"
                                      :payload '(:approval-id "missing" :decision allow)))))
      (should (eq 'invalid (plist-get (superchat-closure-judge
                                      '(:kind trace-well-formed :run-id "run-1")
                                      trace) :verdict))))))

(ert-deftest closure/decision-evidence-is-known-but-lifecycle-irrelevant ()
  "Decision evidence must neither prove nor obstruct Runtime Closure."
  (let* ((base (superchat-closure-test--successful-run))
         (trace
          (append
           (list (nth 0 base) (nth 1 base))
           (list
            (superchat-closure-test--event
             "decision.snapshot.recorded" "evt-decision"
             :causation-id "evt-start"
             :payload '(:evidence-contract "epistemic-learning-v0"
                        :prompt "exact"))
            (superchat-closure-test--event
             "output.first.recorded" "evt-first"
             :causation-id "evt-decision"
             :payload '(:evidence-contract "epistemic-learning-v0"
                        :kind "text" :text "first")))
           (nthcdr 2 base)))
         (result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)))
    (should (eq 'valid (plist-get result :verdict)))
    (should
     (equal '("evt-decision" "evt-first")
            (plist-get (plist-get result :witness)
                       :known-irrelevant-event-ids)))))

(ert-deftest closure/steer-is-known-but-lifecycle-irrelevant ()
  "A recorded steer explains a Run without changing its terminal proof."
  (let* ((base (superchat-closure-test--successful-run))
         (trace
          (append
           (list (nth 0 base) (nth 1 base))
           (list
           (superchat-closure-test--event
             "run.steered" "evt-steer"
             :causation-id "evt-start"
             :payload '(:instruction "take another path"))
            (superchat-closure-test--event
             "run.steer-undelivered" "evt-undelivered"
             :causation-id "evt-start"
             :payload '(:count 1)))
           (nthcdr 2 base)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (equal '("evt-steer" "evt-undelivered")
                   (plist-get (plist-get well-formed :witness)
                              :known-irrelevant-event-ids)))
    (should (eq 'proved (plist-get terminal :verdict)))))

(ert-deftest closure/decision-evidence-requires-run-identity ()
  "Session-wide evidence cannot bypass Run/Turn identity checks."
  (let* ((event
          (superchat-closure-test--event
           "decision.snapshot.recorded" "evt-decision"
           :run-id nil :turn-id nil
           :payload '(:attempt 1)))
         (result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1")
           (list event))))
    (should (eq 'invalid (plist-get result :verdict)))
    (should (eq 'invalid-run-identity
                (plist-get (plist-get result :witness) :reason)))))

(ert-deftest closure/epistemic-events-are-known-but-never-prove-runtime ()
  "Post-Run evidence must not alter the operational terminal proof."
  (let* ((base (superchat-closure-test--successful-run))
         (evaluation
          (superchat-closure-test--event
           "exercise.evaluated" "evt-evaluation"
           :causation-id "evt-turn-terminal"
           :payload '(:scope "scope" :verdict "passed")))
         (relation
          '(:type "applicability.relation.recorded"
            :event-id "evt-relation" :schema 1
            :session-id "session-1" :actor-id "analyst"
            :causation-id "evt-evaluation"
            :payload (:scope "scope" :relation "applies-under")))
         (disposition
          '(:type "candidate.disposition.recorded"
            :event-id "evt-promotion" :schema 1
            :session-id "session-1" :actor-id "user"
            :payload (:scope "scope" :decision "promote")))
         (intervention
          (superchat-closure-test--event
           "experience.intervention.recorded" "evt-intervention"
           :payload '(:scope "scope")))
         (trace
          (append base (list evaluation relation disposition intervention)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (eq 'proved (plist-get terminal :verdict)))
    (should
     (equal '("evt-evaluation" "evt-relation" "evt-promotion"
              "evt-intervention")
            (plist-get (plist-get well-formed :witness)
                       :known-irrelevant-event-ids)))))

(ert-deftest closure/plan-events-are-known-but-lifecycle-irrelevant ()
  "Session plan state must not obstruct the Runtime terminal proof."
  (let* ((base (superchat-closure-test--successful-run))
         (proposed
          (superchat-closure-test--event
           "plan.proposed" "evt-plan-proposed"
           :run-id nil :turn-id nil
           :payload '(:summary "demo" :steps ((:id 1 :text "read")))))
         (approved
          (superchat-closure-test--event
           "plan.approved" "evt-plan-approved"
           :run-id nil :turn-id nil
           :payload '(:proposed (:session-id "session-1"
                                 :event-id "evt-plan-proposed"))))
         (trace
          (append (list (nth 0 base) (nth 1 base) proposed)
                  (nthcdr 2 base)
                  (list approved)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (eq 'proved (plist-get terminal :verdict)))
    (should
     (equal '("evt-plan-proposed" "evt-plan-approved")
            (plist-get (plist-get well-formed :witness)
                       :known-irrelevant-event-ids)))))

(ert-deftest closure/prior-run-evidence-is-out-of-scope-for-next-run ()
  "Decision evidence from an earlier Run cannot obstruct a later terminal."
  (let* ((prior
          (superchat-closure-test--event
           "decision.snapshot.recorded" "evt-prior-decision"
           :run-id "run-prior" :turn-id "turn-prior"
           :causation-id nil :payload '(:attempt 1)))
         (base (superchat-closure-test--successful-run))
         (next
          (mapcar
           (lambda (event)
             (let* ((copy (copy-sequence event))
                    (id (plist-get copy :event-id))
                    (cause (plist-get copy :causation-id)))
               (setq copy (plist-put copy :run-id "run-next"))
               (setq copy (plist-put copy :turn-id "turn-next"))
               (setq copy
                     (plist-put copy :event-id (concat id "-next")))
               (when cause
                 (setq copy
                       (plist-put copy :causation-id
                                  (concat cause "-next"))))
               copy))
           base))
         (trace (cons prior next))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-next") trace))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-next") trace)))
    (should (eq 'proved (plist-get terminal :verdict)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should
     (equal '("evt-prior-decision")
            (plist-get (plist-get well-formed :witness)
                       :out-of-scope-event-ids)))))

(defun superchat-closure-test--approved-tool-run ()
  "Return a successful Run containing one approved Tool execution."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "tool.requested" "evt-tool-request" :causation-id "evt-start"
    :payload '(:tool-name "send_email" :execution-id "exec-1"
               :request-hash "hash-1" :decision "ask"
               :policy-version "authority-v1"
               :effects ((:effect external-write :resource message
                          :scope "external://recipient/42"))))
   (superchat-closure-test--event
    "approval.requested" "evt-approval-request"
    :causation-id "evt-tool-request"
    :payload '(:tool-name "send_email" :execution-id "exec-1"
               :request-hash "hash-1" :policy-version "authority-v1"))
   (superchat-closure-test--event
    "approval.decided" "evt-approval-decision"
    :causation-id "evt-approval-request"
    :payload '(:tool-name "send_email" :execution-id "exec-1"
               :request-hash "hash-1" :decision "allow"
               :policy-version "authority-v1"))
   (superchat-closure-test--event
    "tool.execution.started" "evt-tool-start"
    :causation-id "evt-approval-decision"
    :payload '(:tool-name "send_email" :execution-id "exec-1"
               :request-hash "hash-1" :status "executing"))
   (superchat-closure-test--event
    "tool.completed" "evt-tool-complete" :causation-id "evt-tool-start"
    :payload '(:tool-name "send_email" :execution-id "exec-1"
               :request-hash "hash-1" :status "succeeded"
               :result-hash "sha256:result"))
   (superchat-closure-test--event
    "response.completed" "evt-response" :causation-id "evt-tool-complete"
    :payload '(:status completed :content-size 4))
   (superchat-closure-test--event
    "run.completed" "evt-run-terminal" :causation-id "evt-response"
    :payload '(:status completed))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal" :payload '(:status completed))))

(defun superchat-closure-test--denied-tool-run ()
  "Return a successful Run whose Tool request was denied before execution."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "tool.requested" "evt-tool-request" :causation-id "evt-start"
    :payload '(:tool-name "send_email" :execution-id "exec-denied"
               :request-hash "hash-denied" :decision "deny"
               :policy-version "authority-v1"
               :effects ((:effect external-write :resource message))))
   (superchat-closure-test--event
    "tool.failed" "evt-tool-denied" :causation-id "evt-tool-request"
    :payload '(:tool-name "send_email" :execution-id "exec-denied"
               :request-hash "hash-denied" :status "denied"))
   (superchat-closure-test--event
    "response.completed" "evt-response" :causation-id "evt-tool-denied"
    :payload '(:status completed :content-size 4))
   (superchat-closure-test--event
    "run.completed" "evt-run-terminal" :causation-id "evt-response"
    :payload '(:status completed))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal" :payload '(:status completed))))

(defun superchat-closure-test--failed-tool-run ()
  "Return a recovered Run containing one failed Tool execution."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "tool.requested" "evt-tool-request" :causation-id "evt-start"
    :payload '(:tool-name "read_api" :execution-id "exec-failed"
               :request-hash "hash-failed" :decision "allow"
               :policy-version "authority-v1"
               :effects ((:effect external-read :resource api))))
   (superchat-closure-test--event
    "tool.execution.started" "evt-tool-start"
    :causation-id "evt-tool-request"
    :payload '(:tool-name "read_api" :execution-id "exec-failed"
               :request-hash "hash-failed" :status "executing"))
   (superchat-closure-test--event
    "tool.failed" "evt-tool-failed" :causation-id "evt-tool-start"
    :payload '(:tool-name "read_api" :execution-id "exec-failed"
               :request-hash "hash-failed" :status "failed"))
   (superchat-closure-test--event
    "response.completed" "evt-response" :causation-id "evt-tool-failed"
    :payload '(:status failed :content-size 9))
   (superchat-closure-test--event
    "run.failed" "evt-run-terminal" :causation-id "evt-response"
    :payload '(:status failed))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal" :payload '(:status failed))))

(defun superchat-closure-test--timed-out-run ()
  "Return the canonical timed-out Run trace."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "response.completed" "evt-response" :causation-id "evt-start"
    :payload '(:status timed-out :content-size 16))
   (superchat-closure-test--event
    "run.failed" "evt-run-terminal" :causation-id "evt-response"
    :payload '(:status timed-out))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal" :payload '(:status timed-out))))

(defun superchat-closure-test--cancelled-run ()
  "Return the canonical clean cancellation trace."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "run.cancelled" "evt-run-terminal" :causation-id "evt-start"
    :payload '(:status cancelled))
   (superchat-closure-test--event
    "turn.completed" "evt-turn-terminal"
    :causation-id "evt-run-terminal" :payload '(:status cancelled))))

(defun superchat-closure-test--cancelled-run-with-pending-effect ()
  "Return a cancellation trace that leaves one Effect executing."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "tool.requested" "evt-tool-request" :causation-id "evt-start"
    :payload '(:tool-name "send_email" :execution-id "exec-pending"
               :request-hash "hash-pending" :decision "allow"
               :policy-version "authority-v1"
               :effects ((:effect external-write :resource message))))
   (superchat-closure-test--event
    "tool.execution.started" "evt-tool-start"
    :causation-id "evt-tool-request"
    :payload '(:tool-name "send_email" :execution-id "exec-pending"
               :request-hash "hash-pending" :status "executing"))
   (superchat-closure-test--event
    "run.cancelled" "evt-run-terminal" :causation-id "evt-tool-start"
    :payload '(:status cancelled))))

(defun superchat-closure-test--audit-gap ()
  "Return a completed response prefix without a durable terminal seal."
  (list
   (superchat-closure-test--event "intent.received" "evt-intent")
   (superchat-closure-test--event
    "run.started" "evt-start" :causation-id "evt-intent"
    :payload '(:status running-model :actor-id "lead" :capability "chat"))
   (superchat-closure-test--event
    "response.completed" "evt-response" :causation-id "evt-start"
    :payload '(:status completed :content-size 4))))

(ert-deftest closure/successful-run-has-durable-terminal-witness ()
  "A valid successful trace proves its Run terminal from prior events."
  (let* ((trace (superchat-closure-test--successful-run))
         (result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1")
           trace))
         (witness (plist-get result :witness))
         (turn
          (superchat-closure-judge
           '(:kind durable-turn-terminal :run-id "run-1"
             :turn-id "turn-1")
           trace)))
    (should (eq 'proved (plist-get result :verdict)))
    (should
     (equal '(:kind durable-run-terminal
              :run-id "run-1"
              :terminal-kind completed)
            (plist-get result :judgment)))
    (should
     (equal '("evt-intent" "evt-start" "evt-response"
              "evt-run-terminal")
            (plist-get witness :event-ids)))
    (should
     (equal '("run:run-1" "obligation:run-1:outcome")
            (plist-get witness :node-ids)))
    (should (eq 'proved (plist-get turn :verdict)))
    (should
     (equal '(:kind durable-turn-terminal :turn-id "turn-1"
              :terminal-kind completed)
            (plist-get turn :judgment)))))

(ert-deftest closure/witness-identifies-semantic-environment ()
  "Every judgment witness names the Γ used for interpretation."
  (let* ((result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1")
           (superchat-closure-test--successful-run)))
         (witness (plist-get result :witness)))
    (should
     (equal '(:closure-contract "closure-v0"
              :event-schema 1
              :authority-policy-contract "authority-v1")
            (plist-get witness :semantic-environment)))))

(ert-deftest closure/approved-tool-accounts-approval-and-effect ()
  "An approved, completed Tool leaves no pending Effect or Approval."
  (let* ((trace (superchat-closure-test--approved-tool-run))
         (result
          (superchat-closure-judge
           '(:kind pending-effects :run-id "run-1")
           trace))
         (witness (plist-get result :witness))
         (approvals
          (superchat-closure-judge
           '(:kind pending-approvals :run-id "run-1") trace)))
    (should (eq 'proved (plist-get result :verdict)))
    (should
     (equal '(:kind pending-effects :run-id "run-1" :effects nil)
            (plist-get result :judgment)))
    (should
     (equal '("approval:hash-1" "effect:exec-1")
            (plist-get witness :accounted-node-ids)))
    (should
     (equal '("evt-intent" "evt-start" "evt-tool-request"
              "evt-approval-request" "evt-approval-decision"
              "evt-tool-start" "evt-tool-complete" "evt-response"
              "evt-run-terminal" "evt-turn-terminal")
            (plist-get witness :event-ids)))
    (should (eq 'proved (plist-get approvals :verdict)))
    (should
     (equal '(:kind pending-approvals :run-id "run-1" :approvals nil)
            (plist-get approvals :judgment)))))

(ert-deftest closure/denied-tool-is-accounted-without-execution ()
  "A denied Tool request is accounted without an execution-start event."
  (let* ((result
          (superchat-closure-judge
           '(:kind pending-effects :run-id "run-1")
           (superchat-closure-test--denied-tool-run)))
         (witness (plist-get result :witness)))
    (should (eq 'proved (plist-get result :verdict)))
    (should-not
     (plist-get (plist-get result :judgment) :effects))
    (should
     (equal '(("effect:exec-denied" . denied))
            (plist-get witness :accounted)))))

(ert-deftest closure/failed-tool-keeps-its-accounting-witness ()
  "A Tool failure remains accounted and cites its full execution chain."
  (let* ((trace (superchat-closure-test--failed-tool-run))
         (result
          (superchat-closure-judge
           '(:kind pending-effects :run-id "run-1")
           trace))
         (witness (plist-get result :witness))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'proved (plist-get result :verdict)))
    (should
     (equal '(("effect:exec-failed"
               "evt-tool-request" "evt-tool-start" "evt-tool-failed"))
            (plist-get witness :accounted-events)))
    (should
     (eq 'failed
         (plist-get (plist-get terminal :judgment) :terminal-kind)))))

(ert-deftest closure/user-interrupted-tool-is-still-an-accounted-failure ()
  "Interruption metadata refines but does not reopen a failed Effect."
  (let* ((trace (superchat-closure-test--failed-tool-run))
         (failed (cl-find "tool.failed" trace
                          :key (lambda (event) (plist-get event :type))
                          :test #'equal)))
    (setf (plist-get failed :payload)
          (append (plist-get failed :payload)
                  '(:interruption "user-quit" :phase "implementation"
                    :effects-may-be-partial t)))
    (should
     (eq 'valid
         (plist-get
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)
          :verdict)))
    (should
     (eq 'proved
         (plist-get
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)
          :verdict)))))

(ert-deftest closure/timed-out-run-keeps-its-terminal-kind ()
  "A `run.failed' seal with timed-out payload proves timed-out, not failed."
  (let* ((result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1")
           (superchat-closure-test--timed-out-run)))
         (witness (plist-get result :witness)))
    (should (eq 'proved (plist-get result :verdict)))
    (should
     (eq 'timed-out
         (plist-get (plist-get result :judgment) :terminal-kind)))
    (should (memq 'ready-timed-out (plist-get witness :rules)))))

(ert-deftest closure/clean-cancellation-needs-no-response-event ()
  "A clean cancellation proves terminal without inventing a response."
  (let* ((result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1")
           (superchat-closure-test--cancelled-run)))
         (witness (plist-get result :witness)))
    (should (eq 'proved (plist-get result :verdict)))
    (should
     (eq 'cancelled
         (plist-get (plist-get result :judgment) :terminal-kind)))
    (should
     (equal '("evt-intent" "evt-start" "evt-run-terminal")
            (plist-get witness :event-ids)))
    (should-not (memq 'response-completed (plist-get witness :rules)))))

(ert-deftest closure/cancellation-readiness-allows-an-existing-response ()
  "Cancellation readiness and its durable seal use the same predicate."
  (let* ((prefix (superchat-closure-test--audit-gap))
         (ready
          (superchat-closure-judge
           '(:kind ready-terminal :run-id "run-1"
             :terminal-kind cancelled)
           prefix))
         (trace
          (append
           prefix
           (list
            (superchat-closure-test--event
             "run.cancelled" "evt-run-terminal"
             :causation-id "evt-response"
             :payload '(:status cancelled))
            (superchat-closure-test--event
             "turn.completed" "evt-turn-terminal"
             :causation-id "evt-run-terminal"
             :payload '(:status cancelled)))))
         (durable
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'proved (plist-get ready :verdict)))
    (should (eq 'proved (plist-get durable :verdict)))
    (should
     (eq 'cancelled
         (plist-get (plist-get durable :judgment) :terminal-kind)))))

(ert-deftest closure/cancellation-cannot-erase-a-pending-effect ()
  "A terminal event is invalid while an owned Effect remains executing."
  (let* ((trace
          (superchat-closure-test--cancelled-run-with-pending-effect))
         (result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1")
           trace))
         (witness (plist-get result :witness))
         (residual
          (superchat-closure-judge
           '(:kind residual-boundary :run-id "run-1")
           (butlast trace)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)))
    (should (eq 'unknown (plist-get result :verdict)))
    (should (eq 'run-not-ready (plist-get witness :reason)))
    (should (eq 'invalid (plist-get well-formed :verdict)))
    (should
     (equal '("effect:exec-pending")
            (plist-get witness :residual-node-ids)))
    (should (eq 'open (plist-get residual :verdict)))
    (should
     (equal '(:kind residual-boundary :run-id "run-1"
              :nodes ("effect:exec-pending"))
            (plist-get residual :judgment)))))

(ert-deftest closure/run-start-causation-must-match-intent ()
  "A known but unrelated parent cannot justify `run.started'."
  (let* ((successful (superchat-closure-test--successful-run))
         (unrelated
          (superchat-closure-test--event
           "memory.captured" "evt-unrelated"
           :run-id nil :turn-id nil
           :payload '(:status accepted)))
         (start (copy-sequence (nth 1 successful)))
         (_ (setq start
                  (plist-put start :causation-id "evt-unrelated")))
         (trace
          (append (list unrelated (car successful) start)
                  (nthcdr 2 successful)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (durable
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'invalid (plist-get well-formed :verdict)))
    (should
     (eq 'run-start-causation-mismatch
         (plist-get (plist-get well-formed :witness) :reason)))
    (should (eq 'unknown (plist-get durable :verdict)))
    (should
     (eq 'run-start-causation-mismatch
         (plist-get (plist-get durable :witness) :reason)))))

(ert-deftest closure/tool-name-is-required-and-stable ()
  "Tool lifecycle events carry one stable Tool identity."
  (let* ((missing
          (list
           (superchat-closure-test--event "intent.received" "evt-intent")
           (superchat-closure-test--event
            "run.started" "evt-start" :causation-id "evt-intent"
            :payload '(:status running-model :actor-id "lead"
                       :capability "chat"))
           (superchat-closure-test--event
            "tool.requested" "evt-tool-request"
            :causation-id "evt-start"
            :payload '(:execution-id "exec-1" :request-hash "hash-1"
                       :decision "allow"
                       :effects ((:effect external-read :resource api))))))
         (mismatch (superchat-closure-test--approved-tool-run))
         (completed (copy-sequence (nth 6 mismatch)))
         (payload (copy-sequence (plist-get completed :payload)))
         (_ (setq payload (plist-put payload :tool-name "other_tool")))
         (_ (setq completed (plist-put completed :payload payload)))
         (_ (setf (nth 6 mismatch) completed))
         (missing-result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") missing))
         (mismatch-result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") mismatch)))
    (should (eq 'invalid (plist-get missing-result :verdict)))
    (should
     (eq 'invalid-tool-request
         (plist-get (plist-get missing-result :witness) :reason)))
    (should (eq 'invalid (plist-get mismatch-result :verdict)))
    (should
     (eq 'tool-identity-mismatch
         (plist-get (plist-get mismatch-result :witness) :reason)))))

(ert-deftest closure/tool-request-requires-stable-identities ()
  "An Effect request without execution or request identity is invalid."
  (dolist (key '(:execution-id :request-hash))
    (let* ((trace (superchat-closure-test--approved-tool-run))
           (event (copy-sequence (nth 2 trace)))
           (payload (copy-sequence (plist-get event :payload))))
      (setq payload (plist-put payload key nil))
      (setf (nth 2 trace) (plist-put event :payload payload))
      (let ((result
             (superchat-closure-judge
              '(:kind trace-well-formed :run-id "run-1") trace)))
        (should (eq 'invalid (plist-get result :verdict)))
        (should
         (eq 'invalid-tool-request
             (plist-get (plist-get result :witness) :reason)))))))

(ert-deftest closure/normative-invalid-sequences-are-rejected ()
  "Contract section 14.8 examples remain invalid traces."
  (let* ((intent
          (superchat-closure-test--event "intent.received" "evt-intent"))
         (start
          (superchat-closure-test--event
           "run.started" "evt-start" :causation-id "evt-intent"
           :payload '(:status running-model :actor-id "lead"
                      :capability "chat")))
         (requested
          (superchat-closure-test--event
           "tool.requested" "evt-tool-request" :causation-id "evt-start"
           :payload '(:tool-name "read_api" :execution-id "exec-1"
                      :request-hash "hash-1" :decision "allow"
                      :policy-version "authority-v1"
                      :effects ((:effect external-read :resource api)))))
         (completed-without-request
          (append
           (list intent start)
           (list
            (superchat-closure-test--event
             "tool.completed" "evt-tool-complete"
             :causation-id "evt-start"
             :payload '(:tool-name "read_api" :execution-id "exec-1"
                        :request-hash "hash-1" :status "succeeded")))))
         (completed-before-execution
          (append
           (list intent start requested)
           (list
            (superchat-closure-test--event
             "tool.completed" "evt-tool-complete"
             :causation-id "evt-tool-request"
             :payload '(:tool-name "read_api" :execution-id "exec-1"
                        :request-hash "hash-1" :status "succeeded")))))
         (duplicate-event-id
          (let ((duplicate (copy-sequence start)))
            (setq duplicate (plist-put duplicate :event-id "evt-intent"))
            (list intent duplicate)))
         (denied-prefix
          (butlast (superchat-closure-test--denied-tool-run) 3))
         (completed-after-denial
          (append
           denied-prefix
           (list
            (superchat-closure-test--event
             "tool.completed" "evt-tool-complete"
             :causation-id "evt-tool-denied"
             :payload '(:tool-name "send_email"
                        :execution-id "exec-denied"
                        :request-hash "hash-denied"
                        :status "succeeded")))))
         (approval-without-request
          (append
           (list intent start)
           (list
            (superchat-closure-test--event
             "approval.decided" "evt-approval-decision"
             :causation-id "evt-start"
             :payload '(:tool-name "send_email" :execution-id "exec-1"
                        :request-hash "hash-1" :decision "allow")))))
         (run-without-readiness
          (append
           (list intent start)
           (list
            (superchat-closure-test--event
             "run.completed" "evt-run-terminal"
             :causation-id "evt-start" :payload '(:status completed)))))
         (turn-before-run
          (append
           (list intent start)
           (list
            (superchat-closure-test--event
             "turn.completed" "evt-turn-terminal"
             :causation-id "evt-start" :payload '(:status completed)))))
         (event-after-terminal
          (append
           (superchat-closure-test--successful-run)
           (list
            (superchat-closure-test--event
             "tool.requested" "evt-late-tool"
             :causation-id "evt-turn-terminal"
             :payload '(:tool-name "read_api" :execution-id "exec-late"
                        :request-hash "hash-late" :decision "allow"
                        :effects ((:effect external-read :resource api)))))))
         (cases
          (list completed-without-request completed-before-execution
                duplicate-event-id completed-after-denial
                approval-without-request run-without-readiness
                turn-before-run event-after-terminal)))
    (dolist (trace cases)
      (should
       (eq 'invalid
           (plist-get
            (superchat-closure-judge
             '(:kind trace-well-formed :run-id "run-1") trace)
            :verdict))))))

(ert-deftest closure/audit-gap-is-ready-but-not-durably-terminal ()
  "A completed response can be ready while its durable seal remains absent."
  (let* ((trace (superchat-closure-test--audit-gap))
         (ready
          (superchat-closure-judge
           '(:kind ready-terminal :run-id "run-1"
             :terminal-kind completed)
           trace))
         (durable
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'proved (plist-get ready :verdict)))
    (should
     (equal '("evt-intent" "evt-start" "evt-response")
            (plist-get (plist-get ready :witness) :event-ids)))
    (should (eq 'open (plist-get durable :verdict)))
    (should
         (eq 'terminal-seal-absent
             (plist-get (plist-get durable :witness) :reason)))))

(ert-deftest closure/abandoned-run-seal-proves-an-open-prefix ()
  "An explicit abandoned pair closes a crashed Run without a response."
  (let* ((prefix (superchat-closure-test--audit-gap))
         (trace
          (append
           prefix
           (list
            (superchat-closure-test--event
             "run.failed" "evt-abandoned"
             :causation-id "evt-response"
             :payload '(:status abandoned :abandoned t))
            (superchat-closure-test--event
             "turn.completed" "evt-abandoned-turn"
             :causation-id "evt-abandoned"
             :payload '(:status abandoned :abandoned t)))))
         (run
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace))
         (turn
          (superchat-closure-judge
           '(:kind durable-turn-terminal :run-id "run-1" :turn-id "turn-1")
           trace)))
    (should (eq 'proved (plist-get run :verdict)))
    (should (eq 'abandoned
                (plist-get (plist-get run :judgment) :terminal-kind)))
    (should (eq 'proved (plist-get turn :verdict)))
    (should (equal '(abandoned-terminal-seal)
                   (plist-get (plist-get run :witness) :rules)))))

(ert-deftest closure/unknown-in-scope-schema-never-validates ()
  "An unsupported schema in the selected Run makes well-formedness unknown."
  (let* ((trace
          (append
           (butlast (superchat-closure-test--audit-gap))
           (list
            (superchat-closure-test--event
             "tool.completed" "evt-unknown" :schema 99
             :causation-id "evt-start"
             :payload '(:execution-id "exec-unknown"
                        :request-hash "hash-unknown"
                        :status "succeeded")))))
         (result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)))
    (should (eq 'unknown (plist-get result :verdict)))
    (should
     (equal '(:kind trace-well-formed :run-id "run-1" :status unknown)
            (plist-get result :judgment)))
    (should
     (equal '("evt-unknown")
            (plist-get (plist-get result :witness) :event-ids)))
    (should
     (eq 'unsupported-schema
         (plist-get (plist-get result :witness) :reason)))))

(ert-deftest closure/unknown-out-of-scope-event-is-classified-not-interpreted ()
  "An understood foreign Run identity keeps unknown semantics out of scope."
  (let* ((successful (superchat-closure-test--successful-run))
         (artifact
          (superchat-closure-test--event
           "workspace.artifact.added" "evt-artifact"
           :run-id nil :turn-id nil :workspace-id "workspace-1"
           :payload '(:artifact-id "artifact-1"
                      :uri "buffer://artifact-1"
                      :revision "sha256:revision")))
         (memory
          (superchat-closure-test--event
           "memory.captured" "evt-memory"
           :run-id nil :turn-id nil
           :payload '(:title "Known fact" :status "accepted")))
         (foreign
          (superchat-closure-test--event
           "future.runtime.event" "evt-other" :schema 99
           :turn-id "turn-other" :run-id "run-other"
           :causation-id "evt-missing-other"))
         (trace
          (append (list memory (car successful) artifact foreign)
                  (cdr successful)))
         (result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)))
    (should (eq 'valid (plist-get result :verdict)))
    (should
     (equal '("evt-other")
            (plist-get (plist-get result :witness)
                       :out-of-scope-event-ids)))
    (should
     (equal '("evt-memory")
            (plist-get (plist-get result :witness)
                       :known-irrelevant-event-ids)))
    (should
     (equal '("evt-artifact")
            (plist-get (plist-get result :witness) :artifact-event-ids)))
    (should
     (equal '("artifact:workspace-1/artifact-1")
            (plist-get (plist-get result :witness) :artifact-node-ids)))
    (should
     (equal '("evt-memory" "evt-intent" "evt-artifact" "evt-start"
              "evt-response" "evt-run-terminal" "evt-turn-terminal")
            (plist-get (plist-get result :witness) :event-ids)))
    (let* ((malformed (copy-sequence foreign))
           (_ (setq malformed (plist-put malformed :event-id nil)))
           (invalid
            (superchat-closure-judge
             '(:kind trace-well-formed :run-id "run-1")
             (cons malformed successful))))
      (should (eq 'invalid (plist-get invalid :verdict)))
      (should
       (eq 'invalid-envelope
           (plist-get (plist-get invalid :witness) :reason))))))

(ert-deftest closure/unknown-authority-policy-contract-never-proves ()
  "An Effect chain from an unsupported Authority contract stays unknown."
  (dolist (version '(nil "authority-v99/future"))
    (dolist (index '(2 3 4))
      (let* ((trace (superchat-closure-test--approved-tool-run))
             (event (copy-sequence (nth index trace)))
             (payload (copy-sequence (plist-get event :payload))))
        (setq payload (plist-put payload :policy-version version))
        (setf (nth index trace) (plist-put event :payload payload))
        (let ((well-formed
               (superchat-closure-judge
                '(:kind trace-well-formed :run-id "run-1") trace))
              (terminal
               (superchat-closure-judge
                '(:kind durable-run-terminal :run-id "run-1") trace)))
          (should (eq 'unknown (plist-get well-formed :verdict)))
          (should (eq 'unknown (plist-get terminal :verdict)))
          (should
           (eq 'unsupported-authority-policy-contract
               (plist-get (plist-get terminal :witness) :reason))))))))

(ert-deftest closure/authority-policy-version-must-stay-consistent ()
  "One Effect/Approval chain cannot switch Authority policy identity."
  (let ((trace (superchat-closure-test--approved-tool-run)))
    (dolist (index '(2 3 4))
      (let* ((event (copy-sequence (nth index trace)))
             (payload (copy-sequence (plist-get event :payload))))
        (setq payload
              (plist-put payload :policy-version "authority-v1/default"))
        (setf (nth index trace) (plist-put event :payload payload))))
    (let* ((decision (copy-sequence (nth 4 trace)))
           (payload (copy-sequence (plist-get decision :payload))))
      (setq payload
            (plist-put payload :policy-version "authority-v1/cowork"))
      (setf (nth 4 trace) (plist-put decision :payload payload)))
    (let ((well-formed
           (superchat-closure-judge
            '(:kind trace-well-formed :run-id "run-1") trace))
          (terminal
           (superchat-closure-judge
            '(:kind durable-run-terminal :run-id "run-1") trace)))
      (should (eq 'invalid (plist-get well-formed :verdict)))
      (should (eq 'unknown (plist-get terminal :verdict)))
      (should
       (eq 'authority-policy-version-mismatch
           (plist-get (plist-get terminal :witness) :reason))))))

(ert-deftest closure/pre-execution-refusal-cannot-follow-execution ()
  "An executing Effect cannot later claim an Authority denial."
  (let* ((trace (superchat-closure-test--approved-tool-run))
         (event (copy-sequence (nth 6 trace)))
         (payload (copy-sequence (plist-get event :payload))))
    (setq event (plist-put event :type "tool.failed"))
    (setq payload (plist-put payload :status "denied"))
    (setf (nth 6 trace) (plist-put event :payload payload))
    (let ((well-formed
           (superchat-closure-judge
            '(:kind trace-well-formed :run-id "run-1") trace))
          (terminal
           (superchat-closure-judge
            '(:kind durable-run-terminal :run-id "run-1") trace)))
      (should (eq 'invalid (plist-get well-formed :verdict)))
      (should (eq 'unknown (plist-get terminal :verdict)))
      (should
       (eq 'invalid-effect-failure
           (plist-get (plist-get terminal :witness) :reason))))))

(ert-deftest closure/opaque-with-foreign-run-id-is-skipped ()
  "An opaque row from a different Run is out of scope, not interpreted."
  (let* ((base (superchat-closure-test--successful-run))
         (opaque
          (superchat-closure-test--event
           "legacy.unlisted" "evt-opaque"
           :run-id "run-other" :turn-id "turn-other"
           :historical-opaque t))
         (trace (append base (list opaque)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (equal '("evt-opaque")
                   (plist-get (plist-get well-formed :witness)
                              :out-of-scope-event-ids)))
    (should (eq 'proved (plist-get terminal :verdict)))))

(ert-deftest closure/opaque-with-matching-run-id-makes-claim-unknown ()
  "An opaque row with an explicit :run-id matching the queried Run, and no
:turn-id at all, still cannot be skipped."
  (let* ((base (superchat-closure-test--successful-run))
         (opaque
          (superchat-closure-test--event
           "legacy.unlisted" "evt-opaque"
           :run-id "run-1" :turn-id nil :historical-opaque t))
         (trace (append base (list opaque)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'unknown (plist-get well-formed :verdict)))
    (should (eq 'historical-opaque-in-scope
                (plist-get (plist-get well-formed :witness) :reason)))
    (should (eq 'unknown (plist-get terminal :verdict)))))

(ert-deftest closure/opaque-without-identity-makes-claim-unknown ()
  "An opaque row with no Run or Turn identity cannot be skipped either."
  (let* ((base (superchat-closure-test--successful-run))
         (opaque
          (superchat-closure-test--event
           "legacy.unlisted" "evt-opaque"
           :run-id nil :turn-id nil :historical-opaque t))
         (trace (append base (list opaque)))
         (result
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace)))
    (should (eq 'unknown (plist-get result :verdict)))
    (should (eq 'historical-opaque-in-scope
                (plist-get (plist-get result :witness) :reason)))))

(ert-deftest closure/opaque-with-only-current-turn-id-makes-claim-unknown-when-query-has-run-id-only ()
  "An opaque row's Turn, once learned from the trace, still blocks a skip."
  (let* ((base (superchat-closure-test--successful-run))
         (opaque
          (superchat-closure-test--event
           "legacy.unlisted" "evt-opaque" :turn-id "turn-1"
           :historical-opaque t))
         (trace (append (list (nth 0 base) (nth 1 base) opaque)
                        (nthcdr 2 base)))
         (result
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'unknown (plist-get result :verdict)))
    (should (eq 'historical-opaque-in-scope
                (plist-get (plist-get result :witness) :reason)))))

(ert-deftest closure/opaque-with-foreign-turn-id-after-turn-known-is-skipped ()
  "An opaque row is skipped once its Turn disagrees with the known Turn."
  (let* ((base (superchat-closure-test--successful-run))
         (opaque
          (superchat-closure-test--event
           "legacy.unlisted" "evt-opaque" :turn-id "turn-other"
           :historical-opaque t))
         (trace (append base (list opaque)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (terminal
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (equal '("evt-opaque")
                   (plist-get (plist-get well-formed :witness)
                              :out-of-scope-event-ids)))
    (should (eq 'proved (plist-get terminal :verdict)))))

(ert-deftest closure/delivery-failed-after-proved-seal-keeps-judgment ()
  "`delivery.failed' may follow `turn.completed' without changing Closure's
terminal judgment."
  (let* ((base (superchat-closure-test--successful-run))
         (delivery
          (superchat-closure-test--event
           "delivery.failed" "evt-delivery-failed"
           :causation-id "evt-turn-terminal"
           :payload '(:reason "buffer-killed")))
         (trace (append base (list delivery)))
         (well-formed
          (superchat-closure-judge
           '(:kind trace-well-formed :run-id "run-1") trace))
         (ready-before
          (superchat-closure-judge
           '(:kind ready-terminal :run-id "run-1" :terminal-kind completed)
           base))
         (ready-after
          (superchat-closure-judge
           '(:kind ready-terminal :run-id "run-1" :terminal-kind completed)
           trace))
         (durable-before
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") base))
         (durable-after
          (superchat-closure-judge
           '(:kind durable-run-terminal :run-id "run-1") trace)))
    (should (eq 'valid (plist-get well-formed :verdict)))
    (should (equal '("evt-delivery-failed")
                   (plist-get (plist-get well-formed :witness)
                              :known-irrelevant-event-ids)))
    (should (eq (plist-get ready-before :verdict)
                (plist-get ready-after :verdict)))
    (should (eq 'proved (plist-get durable-before :verdict)))
    (should (eq 'proved (plist-get durable-after :verdict)))))

(ert-deftest closure/artifact-event-types-exact-membership ()
  "The derived artifact subgroup is exactly the three workspace.artifact.*
types, not merely a superset or a subset of the union."
  (should (equal '("workspace.artifact.added" "workspace.artifact.changed"
                   "workspace.artifact.removed")
                 superchat-closure--artifact-event-types)))

(ert-deftest closure/run-annotation-event-types-exact-membership ()
  "The derived run-annotation subgroup is exactly the five Run-scoped
annotation types the contract names, in the contract's own order."
  (should (equal '("decision.snapshot.recorded" "output.first.recorded"
                   "run.steered" "run.steer-undelivered" "delivery.failed")
                 superchat-closure--run-annotation-event-types)))

(ert-deftest closure/known-irrelevant-event-types-excludes-lifecycle-and-artifacts ()
  "The known-irrelevant subgroup never admits a lifecycle or artifact type,
and every run-annotation and artifact type is exactly where the contract
puts it: irrelevant types carry annotations, never artifacts; the run
lifecycle and Authority types (which need a dedicated `--apply-event'
clause) are absent from both irrelevant lists."
  (dolist (type superchat-closure--run-annotation-event-types)
    (should (member type superchat-closure--known-irrelevant-event-types))
    (should-not (member type superchat-closure--artifact-event-types)))
  (dolist (type superchat-closure--artifact-event-types)
    (should-not (member type superchat-closure--known-irrelevant-event-types)))
  (dolist (type '("intent.received" "run.started" "response.completed"
                  "run.completed" "run.failed" "run.cancelled"
                  "turn.completed" "tool.requested" "approval.requested"
                  "approval.decided" "tool.execution.started"
                  "tool.completed" "tool.failed"))
    (should-not (member type superchat-closure--known-irrelevant-event-types))
    (should-not (member type superchat-closure--artifact-event-types))))

(provide 'test-closure)

;;; test-closure.el ends here
