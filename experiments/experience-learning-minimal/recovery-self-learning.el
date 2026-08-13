;;; recovery-self-learning.el --- Model-induced rule in the recovery domain -*- lexical-binding: t; -*-
;;; Commentary:
;; Closed self-learning loop for the workspace revision-recovery domain:
;;   1. a source Run probes the stale Artifact without any Experience and
;;      must close with a typed failure plus a successful corrective action;
;;   2. a separate model Run reflects over the bounded source trajectory and
;;      emits the closed JSON shape lesson/action/avoid;
;;   3. the induced Candidate is admitted through the user promotion gate;
;;   4. two held-out control/treatment pairs measure whether the induced rule
;;      changes the first recovery action.
;; This is a diagnostic cohort, not a frozen acceptance contract.
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(load (expand-file-name "../experience-view-recovery-trace/durable-trace.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)
(load (expand-file-name "recovery-five-methods.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defvar superchat-data-directory)
(defvar superchat-workspace--registry)
(defvar superchat-workspace--artifact-counter)
(defvar superchat-runtime-recovery-intervention-enabled)
(defvar superchat-response-timeout)
(defvar superchat-tool-timeout-multiplier)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
(defvar superchat-llm-unknown-tool-retries)
(defvar superchat-agent-max-tool-calls)
(defvar superchat-agent-confirm-destructive)
(defvar superchat-lang)
(defvar superchat-llm-streaming)
(defvar superchat-llm-tools-enabled)
(defvar superchat-llm-backend)
(defvar superchat--session-id)
(defvar superchat-recovery-trace--llm-log)
(defvar superchat-recovery-trace--transport)
(defvar superchat-experience-workspace-revision-recovery-scope)
(defvar superchat-acceptance-v26--repo-root)
(defvar superchat-acceptance-v26--system-prompt)

(defconst superchat-recovery-self--model
  (or (getenv "SUPERCHAT_EXPERIMENT_MODEL") "qwen3.5:9b"))
(defconst superchat-recovery-self--base-url
  (getenv "SUPERCHAT_EXPERIMENT_URL"))

(defun superchat-recovery-self--setup (directory)
  "Seal a candidate-free workspace setup under DIRECTORY and return it."
  (let* ((superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (workspace "workspace-self")
         (producer-session "producer-self")
         (buffer-name " *self-producer*")
         (original-write (symbol-function 'superchat-workspace-write))
         artifact producer context)
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-workspace-add-region)
                   (lambda (_beg _end &optional role workspace-id)
                     (let ((meta
                            (superchat-workspace-publish-mutable
                             "sealed producer artifact\n" role workspace-id)))
                       (funcall original-write
                                "changed after recorded revision" nil
                                (plist-get meta :artifact-id)
                                (plist-get meta :revision) workspace-id)
                       meta))))
          (setq producer
                (superchat-acceptance-v26--closed-run
                 producer-session "producer-self"
                 (lambda (run)
                   (let ((superchat--tool-execution-context
                          (append run
                                  (list :actor-id "lead"
                                        :workspace-id workspace
                                        :causation-id
                                        (plist-get run :response-id)))))
                     (let ((superchat--session-id producer-session))
                       (with-current-buffer (get-buffer-create buffer-name)
                         (erase-buffer)
                         (insert "sealed producer artifact\n")
                         (superchat-workspace-bind workspace nil
                                                   producer-session)
                         (setq artifact
                               (superchat-workspace-add-region
                                (point-min) (point-max)
                                'workspace workspace))))))))
          (setq context
                (superchat-claim-record
                 (format "Cross-Run workspace_read failed with %s."
                         "superchat-workspace-artifact-revision-error")
                 superchat-experience-workspace-revision-recovery-scope
                 (list (plist-get producer :output))
                 "context-self" "agent:setup" nil 'context)))
      (superchat-db-close))
    (list :workspace-id workspace
          :artifact-id (plist-get artifact :artifact-id)
          :old-revision (plist-get artifact :revision)
          :producer producer
          :context (list :session-id "context-self"
                         :event-id (plist-get context :event-id))
          :sessions (vector producer-session "context-self"))))

(defun superchat-recovery-self--source-run (directory setup)
  "Run the source Run with no Experience and a generous round budget."
  (let* ((session "source-self")
         (workspace (plist-get setup :workspace-id))
         (artifact-id (plist-get setup :artifact-id))
         (old-revision (plist-get setup :old-revision))
         (prompt
          (format
           (concat
            "Your first tool call must be workspace_read, exactly once, for "
            "Artifact %s with expected_revision %s. This is a read-only task: "
            "never call workspace_write, EditBuffer, ReplaceBuffer, or "
            "eshell-command. It will fail; retrying the same call with the "
            "same revision will keep failing. Afterwards, use the available "
            "tools to reach a successful read of the same Artifact. Do not "
            "answer before that read succeeds.")
           artifact-id old-revision))
         (provider (if superchat-recovery-self--base-url
                       (make-llm-openai-compatible
                        :url superchat-recovery-self--base-url
                        :key "lm-studio"
                        :chat-model superchat-recovery-self--model)
                     (make-llm-ollama
                      :chat-model superchat-recovery-self--model
                      :host "localhost" :port 11434)))
         (superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat-runtime-recovery-intervention-enabled nil)
         (superchat-response-timeout 90)
         (superchat-tool-timeout-multiplier 1.0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-max-tool-rounds 8)
         (superchat-llm-round-limit-action 'finalize)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-agent-max-tool-calls 8)
         (superchat-agent-confirm-destructive t)
         (superchat-lang "English")
         (superchat-llm-streaming t)
         (superchat-llm-tools-enabled t)
         (superchat-llm-backend provider)
         handle status submit-error events)
    (advice-add 'llm--log :around
                #'superchat-recovery-trace--llm-log-advice)
    (advice-add 'plz-media-type-request :around
                #'superchat-recovery-trace--transport-advice)
    (unwind-protect
        (condition-case err
            (progn
              (with-current-buffer (get-buffer-create " *self-source*")
                (let ((superchat--session-id session))
                  (superchat-workspace-bind workspace nil session)))
              (setq handle
                    (superchat-runtime-submit
                     (list :prompt prompt :input prompt :clean-input prompt
                           :session-id session :workspace-id workspace
                           :buffer-name " *self-source*"
                           :preset (superchat--builtin-capability-preset "code")
                           :capability "code"
                           :target-model superchat-recovery-self--model
                           :tools '("workspace_read" "workspace_info")
                           :system-prompt superchat-acceptance-v26--system-prompt
                           :authority-policy "default" :attendance 'attended
                           :approval-function
                           #'superchat-acceptance-v26--approval-read-only-only
                           :round-limit-function (lambda () 'finalize)
                           :unknown-tool-retries 0 :record-conversation nil
                           :evidence-contract "epistemic-learning-v0"
                           :context-envelope
                           (list :contract "context-continuity-v0"
                                 :status 'empty
                                 :receipt '(:tape-head 0)
                                 :disclosure '(:represented [])))))
              (let* ((run-id (plist-get handle :run-id))
                     (deadline (+ (float-time) 240)))
                (while (and (< (float-time) deadline)
                            (not (memq
                                  (plist-get
                                   (superchat-runtime-status run-id) :status)
                                  '(completed failed cancelled timed-out))))
                  (accept-process-output nil 0.1))
                (setq status
                      (plist-get (superchat-runtime-status run-id) :status))
                (when (memq status '(accepted running-model delivering))
                  (superchat-runtime-cancel run-id "self-learning timeout"))))
          (error (setq submit-error (error-message-string err))))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (setq events (ignore-errors (superchat-tape-replay-events session)))
    (let* ((requested
            (cl-remove-if-not
             (lambda (event) (equal "tool.requested" (plist-get event :type)))
             events))
           (requested-names
            (mapcar (lambda (event)
                      (plist-get (plist-get event :payload) :tool-name))
                    requested))
           (failures
            (cl-remove-if-not
             (lambda (event)
               (and (equal "tool.failed" (plist-get event :type))
                    (stringp (plist-get (plist-get event :payload)
                                        :error-type))))
             events))
           (completed
            (cl-remove-if-not
             (lambda (event) (equal "tool.completed" (plist-get event :type)))
             events))
           (completed-names
            (mapcar (lambda (event)
                      (plist-get (plist-get event :payload) :tool-name))
                    completed))
           (terminals
            (cl-remove-if-not
             (lambda (event)
               (member (plist-get event :type)
                       '("run.completed" "run.failed" "run.cancelled"
                         "run.timed-out")))
             events))
           (decision
            (cl-find-if
             (lambda (event)
               (equal "decision.snapshot.recorded" (plist-get event :type)))
             events))
           (source-valid
            (and decision
                 (car (last terminals))
                 (cl-find-if
                  (lambda (event)
                    (and (equal "workspace_read"
                                (plist-get (plist-get event :payload)
                                           :tool-name))
                         (equal "superchat-workspace-artifact-revision-error"
                                (plist-get (plist-get event :payload)
                                           :error-type))))
                  failures)
                 (member "workspace_info" completed-names)
                 (cl-find-if
                  (lambda (event)
                    (equal "workspace_read"
                           (plist-get (plist-get event :payload) :tool-name)))
                  completed))))
      (list :session session
            :run-id (and handle (plist-get handle :run-id))
            :status status
            :task prompt
            :tool-requested requested-names
            :completed-names completed-names
            :typed-failure (and failures t)
            :corrective-success (and source-valid t)
            :source-valid (and source-valid t)
            :submit-error submit-error
            :transport-requests (nreverse superchat-recovery-trace--transport)
            :llm-log (nreverse superchat-recovery-trace--llm-log)
            :tape-events events))))


(defun superchat-recovery-self--induction-summary (source)
  "Return the bounded source trajectory shown to the induction model.
Consecutive identical outcomes collapse into one entry carrying `:repeats',
so the successful tail is not buried under repeated failed retries."
  (let ((collapsed nil))
    (dolist (event (plist-get source :tape-events))
      (let* ((payload (plist-get event :payload))
             (entry (list :type (plist-get event :type)
                          :tool-name (plist-get payload :tool-name)
                          :status (plist-get payload :status)
                          :error-type (plist-get payload :error-type)))
             (last (car collapsed))
             (last-entry (and last (plist-get last :event))))
        (if (equal entry last-entry)
            (setcar collapsed
                    (plist-put last :repeats
                               (1+ (or (plist-get last :repeats) 1))))
          (push (list :event entry :repeats 1) collapsed))))
    (list :status (plist-get source :status)
          :task (plist-get source :task)
          :events (vconcat (nreverse collapsed)))))

(defun superchat-recovery-self--induction-prompt (source)
  "Build the one-shot reflection prompt for SOURCE."
  (format
   (concat
    "You are reflecting on your own completed training episode.\n"
    "Extract one reusable recovery rule from the observed trajectory below.\n"
    "Do not invent an outcome that is not recorded.  Return ONLY one JSON "
    "object with exactly these fields: lesson (string), action (one of "
    "workspace_info or workspace_read), avoid (array of strings).\n"
    "The rule must describe when the action applies, and avoid must name any "
    "observed wrong branch.  The recovery action is the tool call that "
    "immediately precedes the first successful workspace_read; a repeated "
    "failing call is not a recovery action.  The recovery action must be "
    "serial; never say to call tools in parallel.  Do not include Markdown "
    "or commentary.\n\n"
    "Observed source trajectory:\n%s\n\n"
    "Available action tools: workspace_info, workspace_read")
   (prin1-to-string (superchat-recovery-self--induction-summary source))))

(defun superchat-recovery-self--parse-induction (response basis)
  "Validate model RESPONSE and return an induced Candidate description."
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
                 (member action '("workspace_info" "workspace_read"))
                 (or (arrayp avoid) (listp avoid)))
      (error "Induction response did not match the closed schema: %S" response))
    (setq lesson (string-trim lesson))
    (when (> (length lesson) 400)
      (error "Induction lesson is too long"))
    (when (string-match-p "parallel" (downcase lesson))
      (error "Induction lesson permits parallel tool calls"))
    (list :statement
          (format
           "When workspace_read fails with superchat-workspace-artifact-revision-error: use %s. %s"
           action lesson)
          :action action
          :lesson lesson
          :avoid (if (arrayp avoid) (append avoid nil) avoid)
          :raw-response response
          :basis basis)))

(defun superchat-recovery-self--induce (directory source)
  "Run one real model reflection over SOURCE and parse its Candidate."
  (let* ((session "induction-self")
         (prompt (superchat-recovery-self--induction-prompt source))
         (provider (if superchat-recovery-self--base-url
                       (make-llm-openai-compatible
                        :url superchat-recovery-self--base-url
                        :key "lm-studio"
                        :chat-model superchat-recovery-self--model)
                     (make-llm-ollama
                      :chat-model superchat-recovery-self--model
                      :host "localhost" :port 11434)))
         (superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat-runtime-recovery-intervention-enabled nil)
         (superchat-response-timeout 90)
         (superchat-llm-reasoning 'none)
         (superchat-llm-max-tool-rounds 1)
         (superchat-llm-round-limit-action 'stop)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-agent-max-tool-calls 1)
         (superchat-llm-streaming t)
         (superchat-llm-tools-enabled t)
         (superchat-llm-backend provider)
         handle status submit-error events)
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
                           :workspace-id (format "workspace-%s" session)
                           :buffer-name " *self-induction*"
                           :ephemeral-buffer t
                           :target-model superchat-recovery-self--model
                           :tools 'none
                           :preset (superchat--builtin-capability-preset "code")
                           :attendance 'attended :authority-policy "default"
                           :approval-function (lambda (_request) t)
                           :round-limit-function (lambda () 'stop)
                           :unknown-tool-retries 0 :record-conversation nil
                           :evidence-contract "epistemic-learning-v0"
                           :context-envelope
                           '(:contract "context-continuity-v0"
                             :status empty
                             :receipt (:tape-head 0)
                             :disclosure (:represented [])))))
              (let* ((run-id (plist-get handle :run-id))
                     (deadline (+ (float-time) 120)))
                (while (and (< (float-time) deadline)
                            (not (memq
                                  (plist-get
                                   (superchat-runtime-status run-id) :status)
                                  '(completed failed cancelled timed-out))))
                  (accept-process-output nil 0.1))
                (setq status
                      (plist-get (superchat-runtime-status run-id) :status))
                (unless (memq status '(completed failed cancelled timed-out))
                  (superchat-runtime-cancel run-id "induction timeout"))))
          (error (setq submit-error (error-message-string err))))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (let* ((run-id (and handle (plist-get handle :run-id)))
           (entry (and run-id (superchat-runtime--entry run-id)))
           (response (and entry
                          (superchat-turn-llm-result
                           (plist-get entry :turn))))
           (events (and run-id
                        (ignore-errors
                          (cl-remove-if-not
                           (lambda (event)
                             (equal run-id (plist-get event :run-id)))
                           (superchat-tape-replay-events session)))))
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
                             (list :session-id session
                                   :event-id (plist-get response-event
                                                        :event-id)))
                        (and terminal-event
                             (list :session-id session
                                   :event-id (plist-get terminal-event
                                                        :event-id)))))))
      (unless (and (eq status 'completed) response-event terminal-event
                   (null submit-error))
        (error "Induction Run did not complete: %S"
               (list :status status :submit-error submit-error)))
      (append (superchat-recovery-self--parse-induction response basis)
              (list :session session :run-id run-id :status status
                    :prompt prompt :response response
                    :response-ref (car basis)
                    :terminal-ref (cadr basis)
                    :tape-events events
                    :transport-requests
                    (nreverse superchat-recovery-trace--transport)
                    :llm-log (nreverse superchat-recovery-trace--llm-log))))))


(defun superchat-recovery-self--admit (directory source induced setup)
  "Admit the induced Candidate through the public Experience seams."
  (let* ((scope superchat-experience-workspace-revision-recovery-scope)
         (source-failure
          (cl-find-if
           (lambda (event)
             (equal "tool.failed" (plist-get event :type)))
           (plist-get source :tape-events)))
         (source-decision
          (cl-find-if
           (lambda (event)
             (equal "decision.snapshot.recorded" (plist-get event :type)))
           (plist-get source :tape-events)))
         (source-terminal
          (cl-find-if
           (lambda (event)
             (member (plist-get event :type)
                     '("run.completed" "run.failed" "run.cancelled"
                       "run.timed-out")))
           (plist-get source :tape-events)))
         (candidate
          (superchat-claim-record
           (plist-get induced :statement)
           scope
           (append
            (list (list :session-id (plist-get source :session)
                        :event-id (plist-get source-failure :event-id)))
            (plist-get induced :basis))
           "candidate-self" "agent:inducer" nil 'candidate))
         (candidate-ref
          (list :session-id "candidate-self"
                :event-id (plist-get candidate :event-id)))
         (evaluation
          (superchat-experience-record-evaluation
           (list :session-id (plist-get source :session)
                 :actor-id "evaluator:self-learning"
                 :exercise-id "recovery-self-learning-v1"
                 :scope scope
                 :run-id (plist-get source :run-id)
                 :decision (list :session-id (plist-get source :session)
                                 :event-id (plist-get source-decision
                                                      :event-id))
                 :terminal (list :session-id (plist-get source :session)
                                 :event-id (plist-get source-terminal
                                                      :event-id))
                 :evaluator '(:id "recovery-self-evaluator" :version 1)
                 :evaluator-manifest-sha256
                 (concat "sha256:"
                         (secure-hash 'sha256
                                      "recovery-self-evaluator-v1"))
                 :verdict 'passed
                 :observations '(:source-run t :induction "model-reflection")
                 :metrics '(:score 1)
                 :evidence (list candidate-ref))))
         (evaluation-ref
          (list :session-id (plist-get source :session)
                :event-id (plist-get evaluation :event-id)))
         (relation
          (superchat-experience-record-relation
           (list :session-id "relation-self" :actor-id "agent:inducer"
                 :correlation-id "recovery-self"
                 :relation 'applies-under
                 :from candidate-ref :to (plist-get setup :context)
                 :scope scope :basis (list evaluation-ref)
                 :reason "Induced from a closed source trajectory.")))
         (promotion
          (superchat-experience-record-disposition
           (list :session-id "promotion-self" :actor-id "user"
                 :candidate candidate-ref :scope scope :decision 'promote
                 :basis (list evaluation-ref)
                 :reason "User reviewed the induced recovery rule."))))
    (list :sessions
          (delete-dups
           (append (append (plist-get setup :sessions) nil)
                   (list (plist-get source :session)
                         "candidate-self" "relation-self"
                         "promotion-self" (plist-get induced :session))))
          :candidate candidate-ref
          :evaluation evaluation-ref
          :promotion promotion)))

(defun superchat-recovery-self--run ()
  "Run the closed self-learning loop once in the recovery domain."
  (let* ((output (getenv "SUPERCHAT_RECOVERY_SELF_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (directory (and root (expand-file-name "self-P" root)))
         setup source induced seed-sessions admission results)
    (unless (and output root)
      (error "SUPERCHAT_RECOVERY_SELF_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite self-learning trace: %s" output))
    (make-directory directory t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'superchat-workspace)
    (require 'llm)
    (if superchat-recovery-self--base-url
        (require 'llm-openai)
      (require 'llm-ollama))
    (setq setup (superchat-recovery-self--setup directory))
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0))
      (superchat-db-close)
      (superchat-db-open))
    (setq source (superchat-recovery-self--source-run directory setup))
    (unless (plist-get source :source-valid)
      (let ((trace
             (list :format "superchat-experience-recovery-self-learning-v1"
                   :status 'source-not-admissible
                   :source (list :emacs emacs-version
                                 :model superchat-recovery-self--model
                                 :jj (superchat-acceptance-v26--jj-identity
                                      superchat-acceptance-v26--repo-root)
                                 :dependencies
                                 (superchat-recovery-trace--dependency-records))
                   :setup setup :source-run source
                   :notes
                   '(:admission "Source Run must close with a typed failure and a successful corrective action before induction."
                     :default-remains-off t))))
        (superchat-recovery-trace--write output trace)
        (princ (format "self-learning: source not admissible (%S, tools=%S)\n"
                       (plist-get source :status)
                       (plist-get source :tool-requested)))
        (kill-emacs 0)))
    (setq induced (superchat-recovery-self--induce directory source))
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0))
      (superchat-db-close)
      (superchat-db-open)
      (setq admission
            (superchat-recovery-self--admit directory source induced setup))
      (setq seed-sessions (plist-get admission :sessions))
      (superchat-db-close))
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (sessions (append seed-sessions nil)))
      (superchat-db-close)
      (superchat-db-open)
      (dotimes (pair 2)
        (let ((treatment-first (cl-evenp pair)))
          (if treatment-first
              (progn
                (push (superchat-recovery-five--run-arm
                       directory (format "heldout-%d-treatment" pair)
                       setup sessions nil)
                      results)
                (push (superchat-recovery-five--run-arm
                       directory (format "heldout-%d-control" pair)
                       setup nil nil)
                      results))
            (progn
              (push (superchat-recovery-five--run-arm
                     directory (format "heldout-%d-control" pair)
                     setup nil nil)
                    results)
              (push (superchat-recovery-five--run-arm
                     directory (format "heldout-%d-treatment" pair)
                     setup sessions nil)
                    results)))))
      ;; Wording transfer: a semantically equivalent Context Claim with its
      ;; own relation, and a reworded task.  The induced rule must still
      ;; steer the first recovery action.
      (let ((superchat-data-directory directory))
        (let* ((transfer-context
                (superchat-claim-record
                 (concat "A stale cross-Run read surfaced a revision "
                         "mismatch for a shared Workspace Artifact.")
                 superchat-experience-workspace-revision-recovery-scope
                 nil
                 "context-self-transfer" "agent:target" nil 'context))
               (transfer-context-ref
                (list :session-id "context-self-transfer"
                      :event-id (plist-get transfer-context :event-id)))
               (transfer-relation
                (superchat-experience-record-relation
                 (list :session-id "relation-self-transfer"
                       :actor-id "agent:setup"
                       :correlation-id "recovery-self-transfer"
                       :relation 'applies-under
                       :from (plist-get admission :candidate)
                       :to transfer-context-ref
                       :scope superchat-experience-workspace-revision-recovery-scope
                       :basis (list (plist-get admission :evaluation))
                       :reason "Wording-transfer relation.")))
               (transfer-sessions
                (delete-dups
                 (append
                  (remove "relation-self"
                          (remove "context-self"
                                  (append seed-sessions nil)))
                  (list "context-self-transfer" "relation-self-transfer")))))
          (push (superchat-recovery-five--run-arm
                 directory "wording-transfer" setup transfer-sessions nil
                 (format
                  (concat
                   "A stale revision was reported for a shared Workspace "
                   "Artifact %s. Begin with one workspace_read carrying "
                   "expected_revision %s; expect a revision mismatch. Then "
                   "pick exactly one corrective tool call for the same "
                   "Artifact. Do not answer with prose or parallel tools.")
                  (plist-get setup :artifact-id)
                  (plist-get setup :old-revision)))
                results)))
      ;; Persistence: reopen the store, assemble the View fresh, run once.
      (superchat-db-close)
      (superchat-db-open)
      (push (superchat-recovery-five--run-arm
             directory "persistence-after-reopen" setup sessions nil)
            results)
      ;; Revocation: append the user revocation, rebuild, run once more.
      (let ((superchat-data-directory directory))
        (superchat-experience-record-disposition
         (list :session-id "promotion-self"
               :actor-id "user"
               :candidate (plist-get admission :candidate)
               :scope superchat-experience-workspace-revision-recovery-scope
               :decision 'revoke
               :basis (list (plist-get admission :evaluation))
               :reason "User revoked the induced rule after reopen."
               :prior (list :session-id "promotion-self"
                            :event-id
                            (plist-get (plist-get admission :promotion)
                                       :event-id)))))
      (superchat-db-close)
      (superchat-db-open)
      (push (superchat-recovery-five--run-arm
             directory "persistence-after-revocation" setup sessions nil)
            results))
    (setq results (nreverse results))
    (let* ((controls
            (cl-remove-if-not
             (lambda (arm) (string-suffix-p "control" (plist-get arm :arm)))
             results))
           (treatments
            (cl-remove-if-not
             (lambda (arm) (string-suffix-p "treatment" (plist-get arm :arm)))
             results))
           (target "workspace_info")
           (control-rate
            (/ (float (cl-count target controls
                               :test #'equal
                               :key (lambda (arm)
                                      (plist-get arm :first-recovery-action))))
               (max 1 (length controls))))
           (treatment-rate
            (/ (float (cl-count target treatments
                               :test #'equal
                               :key (lambda (arm)
                                      (plist-get arm :first-recovery-action))))
               (max 1 (length treatments))))
           (contrast
            (cond
             ((and (> treatment-rate control-rate)
                   (cl-every (lambda (arm)
                               (plist-get arm :intervention-recorded))
                             treatments))
              'induced-treatment-first-action-higher)
             ((and (> control-rate treatment-rate)) 'control-rate-higher)
             (t 'no-identifiable-contrast)))
           (trace
            (list :format "superchat-experience-recovery-self-learning-v1"
                  :claim "model-induced Candidate and held-out first-action effect"
                  :source (list :emacs emacs-version
                                :model superchat-recovery-self--model
                                :jj (superchat-acceptance-v26--jj-identity
                                     superchat-acceptance-v26--repo-root)
                                :dependencies
                                (superchat-recovery-trace--dependency-records))
                  :setup (list :workspace-id (plist-get setup :workspace-id)
                               :artifact-id (plist-get setup :artifact-id)
                               :old-revision (plist-get setup :old-revision)
                               :context (plist-get setup :context))
                  :source-run source
                  :induced induced
                  :candidate-statement (plist-get induced :statement)
                  :held-out-pairs 2
                  :target-action target
                  :control-rate control-rate
                  :treatment-rate treatment-rate
                  :rate-difference (- treatment-rate control-rate)
                  :contrast contrast
                  :wording-transfer (cl-find-if
                                    (lambda (arm)
                                      (equal "wording-transfer"
                                             (plist-get arm :arm)))
                                    results)
                  :persistence (cl-find-if
                                (lambda (arm)
                                  (equal "persistence-after-reopen"
                                         (plist-get arm :arm)))
                                results)
                  :revocation (cl-find-if
                               (lambda (arm)
                                 (equal "persistence-after-revocation"
                                        (plist-get arm :arm)))
                               results)
                  :arms results
                  :provider-requests
                  (apply #'+
                         (mapcar
                          (lambda (arm)
                            (length (plist-get arm :transport-requests)))
                          results))
                  :notes
                  '(:no-retry t
                    :admission "Source Run admitted only after a typed failure and a corrective success."
                    :induction "The Candidate statement comes from a real model reflection Run."
                    :human-gate "Promotion remains the user safety gate."
                    :interpretation "Held-out effect is evidence of induced-rule use, not of broad transfer."
                    :default-remains-off t))))
      (superchat-recovery-trace--write output trace)
      (princ
       (format
        (concat "self-learning trace: %s\n"
                "source-status=%S source-valid=%S induced-action=%S\n"
                "control-rate=%.2f treatment-rate=%.2f contrast=%S\n")
        output (plist-get source :status) (plist-get source :source-valid)
        (plist-get induced :action) control-rate treatment-rate contrast)))))

(when (equal (getenv "SUPERCHAT_RECOVERY_SELF_EXECUTE") "1")
  (condition-case err
      (progn (superchat-recovery-self--run) (kill-emacs 0))
    (error
     (princ (format "self-learning infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-recovery-self-learning)

;;; recovery-self-learning.el ends here
