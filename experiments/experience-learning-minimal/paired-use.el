;;; paired-use.el --- Thin real retrieval/use experiment -*- lexical-binding: t; -*-

;; This is deliberately not an acceptance harness.  It creates one reviewed
;; Candidate through the public Tape/Experience seams, then runs two real
;; target Runs: a matched control without the View and a treatment with it.
;; The result is evidence of retrieval/use, not proof that the model induced a
;; memory by itself.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'package)
(require 'superchat-preset)
(require 'superchat-editor-workspace)
(require 'subr-x)

(load (expand-file-name
       "../experience-view-recovery-trace/trace.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

;; These product knobs are dynamically scoped for one isolated Run.  Declare
;; them before the function bodies so lexical-binding does not turn the
;; experiment's bindings into unrelated lexical variables.
(defvar superchat-data-directory)
(defvar superchat-runtime--runs)
(defvar superchat-pre-topic-finalized-functions)
(defvar superchat-post-turn-functions)
(defvar superchat-post-topic-finalized-functions)
(defvar superchat-run-completed-functions)
(defvar superchat-turn-completed-functions)
(defvar superchat-llm-backend)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
(defvar superchat-llm-unknown-tool-retries)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-streaming)
(defvar superchat-response-timeout)
(defvar superchat-agent-max-tool-calls)
(defvar superchat-runtime-recovery-intervention-enabled)
(defvar superchat-closure-shadow-enabled)
(defvar superchat-closure-terminal-guard-enabled)
(defvar superchat-learning-minimal--wrong-action-fails)
(defvar superchat-learning-minimal--sampling-seed)

(defconst superchat-learning-minimal--model "qwen3.5:9b")
(defconst superchat-learning-minimal--scope "opaque-recovery/minimal-v1")
(defconst superchat-learning-minimal--code "E-R17")
(defconst superchat-learning-minimal--candidate-statement
  "For opaque code E-R17, call minimal_refresh exactly once; do not call minimal_uncertain.")
(defconst superchat-learning-minimal--tool-names
  '("read_probe" "minimal_refresh" "minimal_uncertain"))

;; The thin experiment is reusable: a new scenario changes only the opaque
;; code and three local tool names.  Product Runtime code stays untouched.
(defvar superchat-learning-minimal--scenario nil)

(defun superchat-learning-minimal--scenario-value (key default)
  "Return scenario KEY, or DEFAULT for the original experiment."
  (or (and superchat-learning-minimal--scenario
           (plist-get superchat-learning-minimal--scenario key))
      default))

(defun superchat-learning-minimal--scenario-code ()
  "Return the current opaque code."
  (superchat-learning-minimal--scenario-value
   :code superchat-learning-minimal--code))

(defun superchat-learning-minimal--scenario-scope ()
  "Return the current experience scope."
  (superchat-learning-minimal--scenario-value
   :scope superchat-learning-minimal--scope))

(defun superchat-learning-minimal--scenario-read-tool ()
  "Return the current probe tool name."
  (superchat-learning-minimal--scenario-value :read-tool "read_probe"))

(defun superchat-learning-minimal--scenario-target-tool ()
  "Return the current successful recovery tool name."
  (superchat-learning-minimal--scenario-value
   :target-tool "minimal_refresh"))

(defun superchat-learning-minimal--scenario-wrong-tool ()
  "Return the current failing recovery tool name."
  (superchat-learning-minimal--scenario-value
   :wrong-tool "minimal_uncertain"))

(defun superchat-learning-minimal--scenario-tool-names ()
  "Return the current ordered provider tool surface."
  (list (superchat-learning-minimal--scenario-read-tool)
        (superchat-learning-minimal--scenario-target-tool)
        (superchat-learning-minimal--scenario-wrong-tool)))

(defun superchat-learning-minimal--scenario-temperature ()
  "Return the optional fixed sampling temperature for this scenario."
  (plist-get superchat-learning-minimal--scenario :temperature))

(defun superchat-learning-minimal--scenario-seed ()
  "Return the optional fixed provider seed for this scenario."
  (plist-get superchat-learning-minimal--scenario :seed))

(defun superchat-learning-minimal--scenario-max-tool-rounds ()
  "Return the target Run tool-round ceiling for this scenario."
  (or (plist-get superchat-learning-minimal--scenario :max-tool-rounds) 3))

(defun superchat-learning-minimal--sampling-preset ()
  "Return the code preset for the current scenario."
  (superchat--builtin-capability-preset "code"))

(defun superchat-learning-minimal--sampling-preset-advice (orig &rest args)
  "Apply the scenario temperature after the product builds its preset."
  (let ((preset (apply orig args))
        (temperature (superchat-learning-minimal--scenario-temperature)))
    (when (and preset (numberp temperature))
      (setf (superchat-preset-temperature preset) temperature))
    preset))

(defun superchat-learning-minimal--sampling-prompt-advice (orig &rest args)
  "Add the experiment seed to the actual llm prompt when configured."
  (let ((prompt (apply orig args))
        (seed superchat-learning-minimal--sampling-seed))
    (when (and prompt seed (fboundp 'llm-chat-prompt-non-standard-params))
      (setf (llm-chat-prompt-non-standard-params prompt)
            (cons (cons :seed seed)
                  (assq-delete-all
                   :seed (llm-chat-prompt-non-standard-params prompt)))))
    prompt))

(define-error 'superchat-learning-minimal-opaque-error
  "Opaque probe failure")
(define-error 'superchat-learning-minimal-wrong-action-error
  "Opaque recovery action was incorrect")

(defun superchat-learning-minimal--ref (session event)
  "Return a typed reference to EVENT in SESSION."
  (list :session-id session :event-id (plist-get event :event-id)))

(defun superchat-learning-minimal--seed (&optional source-run induced)
  "Create one reviewed Candidate and an applicable Context Claim.
When SOURCE-RUN is supplied, create the Candidate only after that Run closes
with both a failure and a successful corrective action, and cite its Decision,
failure, success, and terminal events.  INDUCED is the parsed
model reflection that supplies the Candidate statement.  Without SOURCE-RUN,
keep the original deterministic setup path for the retrieval/use experiment."
  (let* ((scope (superchat-learning-minimal--scenario-scope))
         (source-events (and source-run (plist-get source-run :tape-events)))
         (source-session (and source-run (plist-get source-run :session)))
         (calibration
          (and (null source-run)
               (superchat-acceptance-v26--closed-run
                "minimal-evaluation" "minimal-calibration" #'ignore)))
         (evaluation-session (or source-session "minimal-evaluation"))
         (candidate-session "minimal-candidate")
         (context-session "minimal-context")
         (relation-session "minimal-relation")
         (promotion-session "minimal-promotion")
         (source-decision
          (and source-run
               (cl-find-if
                (lambda (event)
                  (equal "decision.snapshot.recorded" (plist-get event :type)))
                source-events)))
         (source-terminal
          (and source-run
               (cl-find-if
                (lambda (event)
                  (member (plist-get event :type)
                          '("run.completed" "run.failed"
                            "run.cancelled" "run.timed-out")))
                source-events)))
         (source-failure
          (and source-run
               (cl-find-if
                (lambda (event)
                  (equal "tool.failed" (plist-get event :type)))
                source-events)))
         (source-success
          (and source-run
               (cl-find-if
                (lambda (event)
                  (and (equal "tool.completed" (plist-get event :type))
                       (equal (superchat-learning-minimal--scenario-target-tool)
                              (plist-get (plist-get event :payload)
                                         :tool-name))))
                source-events)))
         (source-valid
          (when source-run
            (unless (and source-decision source-terminal source-failure
                         source-success induced)
              (error "Source Run did not produce a durable failure and correction before induction: %S"
                     (list :decision source-decision
                           :terminal source-terminal
                           :failure source-failure :success source-success
                           :induced induced)))))
         (run (or source-run calibration))
         (decision-ref
          (or (and source-decision
                   (superchat-learning-minimal--ref source-session
                                                    source-decision))
              (plist-get calibration :decision)))
         (terminal-ref
          (or (and source-terminal
                   (superchat-learning-minimal--ref source-session
                                                    source-terminal))
              (plist-get calibration :terminal)))
         (source-ref
          (or (and source-failure
                   (superchat-learning-minimal--ref source-session
                                                    source-failure))
              (plist-get calibration :output)))
         (source-success-ref
          (and source-success
               (superchat-learning-minimal--ref source-session
                                                source-success)))
         (induced-statement (and induced (plist-get induced :statement)))
         (induced-basis (and induced (plist-get induced :basis)))
         (candidate-statement
          (or induced-statement superchat-learning-minimal--candidate-statement))
         (candidate-basis
          (delq nil (append (list source-ref source-success-ref)
                            induced-basis)))
         (candidate
          (superchat-claim-record
           candidate-statement
           scope candidate-basis candidate-session
           (if induced "agent:inducer"
             (if source-run "agent:failure-reflection" "agent:seed")) nil
           'candidate))
         (candidate-ref
          (superchat-learning-minimal--ref candidate-session candidate))
         (evaluation
          (superchat-experience-record-evaluation
           (list :session-id evaluation-session
                 :actor-id "evaluator:minimal"
                 :exercise-id (if source-run
                                  "minimal-failure-reflection"
                                "minimal-seed")
                 :scope scope
                 :run-id (plist-get run :run-id)
                 :decision decision-ref
                 :terminal terminal-ref
                 :evaluator '(:id "minimal-seed-evaluator" :version 1)
                 :evaluator-manifest-sha256
                 (concat "sha256:" (secure-hash 'sha256
                                                   "minimal-seed-evaluator-v1"))
                 :verdict 'passed
                 :observations (if induced
                                  '(:source-run t :induction "model-reflection")
                                (if source-run
                                    '(:source-run t :induction "failure-reflection")
                                  '(:fixture t)))
                 :metrics '(:score 1)
                 :evidence (append (list source-ref candidate-ref)
                                   induced-basis))))
         (evaluation-ref
          (superchat-learning-minimal--ref evaluation-session evaluation))
         (context
          (superchat-claim-record
           (format "A fresh opaque recovery decision for code %s."
                   (superchat-learning-minimal--scenario-code))
           scope nil context-session "agent:target" nil 'context))
         (context-ref
          (superchat-learning-minimal--ref context-session context))
         (relation
          (superchat-experience-record-relation
           (list :session-id relation-session :actor-id "agent:seed"
                 :correlation-id "minimal-relation"
                 :relation 'applies-under :from candidate-ref :to context-ref
                 :scope scope :basis (list evaluation-ref)
                 :reason "Reviewed minimal seed.")))
         (relation-ref
          (superchat-learning-minimal--ref relation-session relation))
         (promotion
          (superchat-experience-record-disposition
           (list :session-id promotion-session :actor-id "user"
                 :candidate candidate-ref :scope scope :decision 'promote
                 :basis (list evaluation-ref)
                 :reason "User approved the minimal seed.")))
         (promotion-ref
          (superchat-learning-minimal--ref promotion-session promotion))
         (sessions
          (delete-dups
           (append (list evaluation-session candidate-session context-session
                         relation-session promotion-session)
                   (when induced
                     (list (plist-get induced :session))))))
         (view (superchat-experience-assemble-view
                sessions scope context-ref)))
    (unless (eq 'satisfied (plist-get view :status))
      (error "Minimal seed did not produce a satisfied View: %S" view))
    (list :scope scope :statement candidate-statement
          :candidate candidate-ref :evaluation evaluation-ref
          :context context-ref :relation relation-ref :promotion promotion-ref
          :sessions (vconcat sessions) :view view
          :induced induced
          :source-run (and source-run
                           (list :session source-session
                                 :run-id (plist-get source-run :run-id)
                                 :source source-ref)))))

(defun superchat-learning-minimal--register-tools ()
  "Register the three opaque local tools used by the target Runs."
  (let ((read-tool (superchat-learning-minimal--scenario-read-tool))
        (target-tool (superchat-learning-minimal--scenario-target-tool))
        (wrong-tool (superchat-learning-minimal--scenario-wrong-tool)))
    (superchat-register-tool
     read-tool
     :function
     (lambda (code)
       (unless (equal code (superchat-learning-minimal--scenario-code))
         (error "Unexpected probe code: %s" code))
       (signal 'superchat-learning-minimal-opaque-error (list code)))
     :description "Read the opaque probe for a recovery decision."
     :args '((:name "code" :type string :description "Opaque recovery code."))
     :effects '((:effect local-read :resource opaque-probe
                 :scope "minimal://probe")))
    (superchat-register-tool
     target-tool
     :function (lambda (_code) "recovery branch A recorded")
     :description "Perform the corrective recovery branch for the opaque code."
     :args '((:name "code" :type string :description "Opaque recovery code."))
     :effects '((:effect local-read :resource opaque-recovery
                 :scope "minimal://action")))
    (superchat-register-tool
     wrong-tool
     :function
     (lambda (code)
       (if superchat-learning-minimal--wrong-action-fails
           (signal 'superchat-learning-minimal-wrong-action-error (list code))
         "uncertain branch recorded"))
     :description "Report that no recovery branch can be selected."
     :args '((:name "code" :type string :description "Opaque recovery code."))
     :effects '((:effect local-read :resource opaque-recovery
                 :scope "minimal://action")))))

(defun superchat-learning-minimal--events-for-run (session run-id)
  "Return Session events belonging to RUN-ID."
  (cl-remove-if-not
   (lambda (event) (equal run-id (plist-get event :run-id)))
   (superchat-tape-replay-events session)))

(defun superchat-learning-minimal--first-action (events)
  "Return the first tool request after the first tool failure in EVENTS."
  (let* ((failure
          (cl-find-if (lambda (event)
                        (equal "tool.failed" (plist-get event :type)))
                      events))
         (failure-storage (and failure (plist-get failure :storage-id))))
    (and failure-storage
         (cl-find-if
          (lambda (event)
            (and (equal "tool.requested" (plist-get event :type))
                 (> (plist-get event :storage-id) failure-storage)))
          events))))

(defun superchat-learning-minimal--induction-summary (source)
  "Return the bounded source facts shown to the induction model.
The model sees the source trajectory, not a Candidate written by the runner."
  (list :status (plist-get source :status)
        :task (plist-get source :task)
        :events
        (vconcat
         (mapcar
          (lambda (event)
            (let ((payload (plist-get event :payload)))
              (list :type (plist-get event :type)
                    :tool-name (plist-get payload :tool-name)
                    :status (plist-get payload :status)
                    :error-type (plist-get payload :error-type)
                    :response (plist-get payload :status))))
          (plist-get source :tape-events)))))

(defun superchat-learning-minimal--induction-prompt (source)
  "Build the one-shot reflection prompt for SOURCE.
The correct answer is not embedded; only the observed trajectory and tool
surface are supplied."
  (format
   (concat
    "You are reflecting on your own completed training episode.\n"
    "Extract one reusable recovery rule from the observed trajectory below.\n"
    "Do not invent an outcome that is not recorded.  Return ONLY one JSON object "
    "with exactly these fields: lesson (string), action (one of %s or %s), "
    "avoid (array of strings).\n"
    "The rule must describe when the action applies, and avoid must name any "
    "observed wrong branch.  The recovery action must be serial; never say "
    "to call tools in parallel.  Do not include Markdown or commentary.\n\n"
    "Observed source trajectory:\n%s\n\n"
    "Available action tools: %s, %s")
   (superchat-learning-minimal--scenario-target-tool)
   (superchat-learning-minimal--scenario-wrong-tool)
   (prin1-to-string (superchat-learning-minimal--induction-summary source))
   (superchat-learning-minimal--scenario-target-tool)
   (superchat-learning-minimal--scenario-wrong-tool)))

(defun superchat-learning-minimal--parse-induction (response basis)
  "Validate model RESPONSE and return an induced Candidate description.
BASIS is a list of typed Tape references to the induction Run."
  (unless (and (stringp response) (not (string-empty-p (string-trim response))))
    (error "Induction Run returned no text"))
  (let* ((text (string-trim response))
         (start (string-match "{" text))
         (end (and start (string-match "}" text start)))
         (json-text (and start end (substring text start (1+ end))))
         (object
          (and json-text
               (json-parse-string json-text
                                  :object-type 'plist
                                  :array-type 'array
                                  :null-object nil
                                  :false-object nil)))
         (lesson (and object (plist-get object :lesson)))
         (action (and object (plist-get object :action)))
         (avoid (and object (plist-get object :avoid))))
    (unless (and (listp object)
                 (stringp lesson) (not (string-empty-p (string-trim lesson)))
                 (stringp action)
                 (member action
                         (list (superchat-learning-minimal--scenario-target-tool)
                               (superchat-learning-minimal--scenario-wrong-tool)))
                 (or (arrayp avoid) (listp avoid)))
      (error "Induction response did not match the closed schema: %S" response))
    (setq lesson (string-trim lesson))
    (when (> (length lesson) 400)
      (error "Induction lesson is too long"))
    (when (string-match-p "parallel" (downcase lesson))
      (error "Induction lesson permits parallel tool calls"))
    (list :statement
          (format "For opaque code %s, after the probe failure: use %s. %s"
                  (superchat-learning-minimal--scenario-code) action lesson)
          :action action
          :lesson lesson
          :avoid (if (arrayp avoid) (append avoid nil) avoid)
          :raw-response response
          :basis basis)))

(defun superchat-learning-minimal--induce (source)
  "Run one real model reflection over SOURCE and parse its Candidate.
This is the only source of the Candidate statement in the self-learning loop."
  (let* ((session "minimal-induction")
         (prompt (superchat-learning-minimal--induction-prompt source))
         (workspace "minimal-workspace-induction")
         (provider (make-llm-ollama
                    :chat-model superchat-learning-minimal--model
                    :host "localhost" :port 11434))
         (preset (superchat-learning-minimal--sampling-preset))
         (superchat-llm-backend provider)
         (superchat-llm-max-tool-rounds 1)
         (superchat-llm-round-limit-action 'stop)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-streaming t)
         (superchat-response-timeout 90)
         (superchat-agent-max-tool-calls 1)
         (superchat-runtime-recovery-intervention-enabled nil)
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-recovery-trace--llm-log nil)
         (superchat-recovery-trace--transport nil)
         handle submit-error)
    (advice-add 'llm--log :around
                #'superchat-recovery-trace--llm-log-advice)
    (advice-add 'plz-media-type-request :around
                #'superchat-recovery-trace--transport-advice)
    (unwind-protect
        (condition-case err
            (progn
              (setq handle
                    (superchat-runtime-submit
                     (list :type :llm-query :input prompt :clean-input prompt
                           :prompt prompt :session-id session
                           :workspace-id workspace
                           :buffer
                           (superchat-editor-workspace-validate-buffer
                            (superchat-editor-workspace-buffer
                             session 'minimal-induction-protocol t)
                            session)
                           :target-model superchat-learning-minimal--model
                           :tools 'none
                           :preset preset
                           :attendance 'attended :authority-policy "default"
                           :approval-function (lambda (_request) t)
                           :round-limit-function (lambda () 'stop)
                           :unknown-tool-retries 0 :record-conversation nil
                           :evidence-contract "epistemic-learning-v0"
                           :context-envelope
                           '(:contract "context-continuity-v0"
                             :status empty
                             :receipt (:tape-head 0)
                             :disclosure (:represented [])
                             :experience-view nil))))
              (let* ((run-id (plist-get handle :run-id))
                     (deadline (+ (float-time) 120)))
                (while (and (< (float-time) deadline)
                            (not (memq (plist-get
                                        (superchat-runtime-status run-id) :status)
                                       superchat-runtime--terminal-statuses)))
                  (accept-process-output nil 0.1))
                (unless (memq (plist-get (superchat-runtime-status run-id) :status)
                              superchat-runtime--terminal-statuses)
                  (superchat-runtime-cancel run-id "minimal induction timeout"))))
          (error (setq submit-error (error-message-string err))))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (let* ((run-id (and handle (plist-get handle :run-id)))
           (status (and run-id
                        (plist-get (superchat-runtime-status run-id) :status)))
           (entry (and run-id (superchat-runtime--entry run-id)))
           (response (and entry
                          (superchat-turn-llm-result
                           (plist-get entry :turn))))
           (events (and run-id
                        (superchat-learning-minimal--events-for-run
                         session run-id)))
           (response-event
            (and events
                 (cl-find-if
                  (lambda (event)
                    (equal "response.completed" (plist-get event :type)))
                  events)))
           (terminal-event
            (and events
                 (cl-find-if
                  (lambda (event)
                    (equal "run.completed" (plist-get event :type)))
                  events)))
           (basis
            (delq nil
                  (list (and response-event
                             (superchat-learning-minimal--ref
                              session response-event))
                        (and terminal-event
                             (superchat-learning-minimal--ref
                              session terminal-event))))))
      (unless (and (eq status 'completed) response-event terminal-event
                   (null submit-error))
        (error "Induction Run did not complete: %S"
               (list :status status :submit-error submit-error)))
      (append (superchat-learning-minimal--parse-induction response basis)
              (list :session session :run-id run-id :status status
                    :prompt prompt :response response
                    :response-ref (car basis)
                    :terminal-ref (cadr basis)
                    :tape-events events
                    :transport-requests
                    (nreverse superchat-recovery-trace--transport)
                    :llm-log (nreverse superchat-recovery-trace--llm-log))))))

(defun superchat-learning-minimal--run-arm
    (arm view &optional task-text experience-action-policy max-tool-rounds
         tool-names)
  "Run one real target ARM with VIEW or no View, retaining all boundaries.
When TASK-TEXT is non-nil, use it as the user task before adding VIEW text.
EXPERIENCE-ACTION-POLICY explicitly names tools denied by a blocked View."
  (let* ((session (format "minimal-target-%s" arm))
         (workspace (format "minimal-workspace-%s" arm))
         (view-prompt (and view (plist-get view :prompt)))
         (task
          (concat
           (or task-text
               (format
                (concat
                 "Perform one opaque recovery decision. First call %s exactly "
                 "once with code %s. It will fail with an opaque error. Then call "
                 "exactly one follow-up action with the same code. Do not answer with "
                 "prose, call tools in parallel, or invent another code.")
                (superchat-learning-minimal--scenario-read-tool)
                (superchat-learning-minimal--scenario-code)))
           (when (and view
                      (stringp view-prompt)
                      (not (string-empty-p view-prompt)))
             (concat "\n\n[Retrieved reviewed Experience]\n" view-prompt))))
         (context-envelope
          (append
           (list :contract "context-continuity-v0"
                 :status (or (plist-get view :status)
                             (if view 'satisfied 'empty))
                 :receipt (list :tape-head 0)
                 :disclosure (list :represented [])
                 :experience-view (copy-tree view t))
           (when experience-action-policy
             (list :experience-action-policy
                   (copy-tree experience-action-policy t)))))
         (provider (make-llm-ollama
                    :chat-model superchat-learning-minimal--model
                    :host "localhost" :port 11434))
         (preset (superchat-learning-minimal--sampling-preset))
         (superchat-llm-backend provider)
         (superchat-llm-max-tool-rounds (or max-tool-rounds 2))
         (superchat-llm-round-limit-action 'stop)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-streaming t)
         (superchat-response-timeout 90)
         (superchat-agent-max-tool-calls (or max-tool-rounds 2))
         (superchat-runtime-recovery-intervention-enabled nil)
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-recovery-trace--llm-log nil)
         (superchat-recovery-trace--transport nil)
         handle submit-error)
    (advice-add 'llm--log :around
                #'superchat-recovery-trace--llm-log-advice)
    (advice-add 'plz-media-type-request :around
                #'superchat-recovery-trace--transport-advice)
    (unwind-protect
        (condition-case err
            (progn
              (setq handle
                    (superchat-runtime-submit
                     (list :type :llm-query :input task :clean-input task
                           :prompt task :session-id session :workspace-id workspace
                           :buffer
                           (superchat-editor-workspace-validate-buffer
                            (superchat-editor-workspace-buffer
                             session
                             (format "minimal-target-%s-protocol" arm) t)
                            session)
                           :target-model superchat-learning-minimal--model
                           :tools (or tool-names
                                       (superchat-learning-minimal--scenario-tool-names))
                           :preset preset
                           :attendance 'attended :authority-policy "default"
                           :approval-function (lambda (_request) t)
                           :round-limit-function (lambda () 'stop)
                           :unknown-tool-retries 0 :record-conversation nil
                           :evidence-contract "epistemic-learning-v0"
                           :context-envelope context-envelope)))
              (let* ((run-id (plist-get handle :run-id))
                     (deadline (+ (float-time) 120)))
                (while (and (< (float-time) deadline)
                            (not (memq (plist-get
                                        (superchat-runtime-status run-id) :status)
                                       superchat-runtime--terminal-statuses)))
                  (accept-process-output nil 0.1))
                (unless (memq (plist-get (superchat-runtime-status run-id) :status)
                              superchat-runtime--terminal-statuses)
                  (superchat-runtime-cancel run-id "minimal experiment timeout"))))
          (error (setq submit-error (error-message-string err))))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (let* ((run-id (and handle (plist-get handle :run-id)))
           (status (and run-id
                        (plist-get (superchat-runtime-status run-id) :status)))
           (entry (and run-id (superchat-runtime--entry run-id)))
           (response (and entry
                          (superchat-turn-llm-result
                           (plist-get entry :turn))))
           (events (and run-id
                        (superchat-learning-minimal--events-for-run
                         session run-id)))
           (failure (and events
                         (cl-find-if
                          (lambda (event)
                            (equal "tool.failed" (plist-get event :type)))
                          events)))
           (action (and events
                        (superchat-learning-minimal--first-action events)))
           (action-tool (and action
                             (plist-get (plist-get action :payload) :tool-name))))
      (list :arm arm :session session :run-id run-id :status status
            :task task :response response
           :used-view (and view t) :view (copy-tree view t)
            :view-origin (and view (plist-get view :assembly-origin))
            :first-recovery-action action-tool
            :action-tool action-tool
            :action-matches
            (equal action-tool (superchat-learning-minimal--scenario-target-tool))
            :failure (and failure (plist-get (plist-get failure :payload)
                                             :error-type))
            :submit-error submit-error
            :transport-requests (nreverse superchat-recovery-trace--transport)
            :llm-log (nreverse superchat-recovery-trace--llm-log)
            :tape-events events))))

(defun superchat-learning-minimal--run ()
  "Run the minimal paired real-provider experiment exactly once."
  (let* ((output (getenv "SUPERCHAT_EXPERIENCE_MINIMAL_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (superchat-data-directory root)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         seed control treatment trace)
    (unless (and output root)
      (error "SUPERCHAT_EXPERIENCE_MINIMAL_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite existing minimal trace: %s" output))
    (make-directory root t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'llm)
    (require 'llm-ollama)
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (progn
          (setq seed (superchat-learning-minimal--seed))
          (superchat-learning-minimal--register-tools)
          (setq control (superchat-learning-minimal--run-arm 'control nil)
                treatment (superchat-learning-minimal--run-arm
                           'treatment (plist-get seed :view)))
          (let* ((control-match (plist-get control :action-matches))
                 (treatment-match (plist-get treatment :action-matches))
                 (contrast
                  (cond
                   ((and treatment-match (not control-match))
                    'treatment-only-success)
                   ((and control-match (not treatment-match))
                    'control-only-success)
                   (t 'no-identifiable-contrast))))
            (setq trace
                  (list :format "superchat-experience-learning-minimal-v1"
                        :claim "retrieval-use, not induction-from-scratch"
                        :source (list :emacs emacs-version
                                      :model superchat-learning-minimal--model
                                      :jj (superchat-acceptance-v26--jj-identity
                                           superchat-acceptance-v26--repo-root)
                                      :dependencies
                                      (superchat-recovery-trace--dependency-records))
                        :seed (list :scope (plist-get seed :scope)
                                     :statement (plist-get seed :statement)
                                     :candidate (plist-get seed :candidate)
                                     :evaluation (plist-get seed :evaluation)
                                     :context (plist-get seed :context)
                                     :relation (plist-get seed :relation)
                                     :promotion (plist-get seed :promotion)
                                     :view (plist-get seed :view))
                        :arms (list control treatment)
                        :contrast contrast
                        :provider-requests
                        (+ (length (plist-get control :transport-requests))
                           (length (plist-get treatment :transport-requests)))
                        :notes
                        '(:control "Matched task with no Experience View."
                          :treatment "Same task with a reviewed Candidate View."
                          :trace "Each arm retains exact provider bodies, decoded llm logs, and Tape events."
                          :interpretation "A treatment/control difference is evidence of View use; it is not proof of self-induced learning."
                          :default-remains-off t))))
            (superchat-recovery-trace--write output trace)
            (princ
             (format "minimal trace: %s\ncontrast=%S\ncontrol-action=%S\ntreatment-action=%S\n"
                     output (plist-get trace :contrast)
                     (plist-get (car (plist-get trace :arms)) :action-tool)
                     (plist-get (cadr (plist-get trace :arms)) :action-tool))))
      (ignore-errors (superchat-unregister-tool "read_probe"))
      (ignore-errors (superchat-unregister-tool "minimal_refresh"))
      (ignore-errors (superchat-unregister-tool "minimal_uncertain"))
      (superchat-db-close))
    nil))

(when (equal (getenv "SUPERCHAT_EXPERIENCE_MINIMAL_EXECUTE") "1")
  (condition-case err
      (progn (superchat-learning-minimal--run) (kill-emacs 0))
    (error
     (princ (format "minimal experiment infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-learning-minimal)

;;; paired-use.el ends here
