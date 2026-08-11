;;; recovery-five-methods.el --- Five real-environment methods, workspace recovery domain
;;; Commentary:
;; This driver exercises the five thin real-environment methods against the
;; workspace_read revision-recovery domain with the real Ollama model and the
;; new minimal chain: typed failure -> intervention -> Runtime policy state
;; machine (awaiting-info -> awaiting-read -> satisfied) -> Authority gate.
;;
;; Methods:
;;   1. matched control/treatment  — no View vs satisfied View
;;   2. view-content ablation      — satisfied View with the instruction removed
;;   3. wording transfer           — new task wording, same reviewed rule
;;   4. negative transfer/safety   — conflicting relation, fail-closed disclosure
;;   5. persistence + revocation   — DB reopen, then user revocation
;;
;; Every arm retains exact provider bodies, decoded llm logs, and Tape events.
;; This is a diagnostic cohort, not a frozen acceptance contract.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "../experience-view-recovery-trace/durable-trace.el"
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

(defconst superchat-recovery-five--model
  (or (getenv "SUPERCHAT_EXPERIMENT_MODEL") "qwen3.5:9b"))
(defconst superchat-recovery-five--base-url
  (getenv "SUPERCHAT_EXPERIMENT_URL"))
(defconst superchat-recovery-five--cell
  '(:name "five-P" :positive t :kind stale :stale t))

(defun superchat-recovery-five--events (events type)
  "Return Tape EVENTS whose type is TYPE."
  (cl-remove-if-not (lambda (event)
                     (equal type (plist-get event :type)))
                   events))

(defun superchat-recovery-five--first-recovery-action (events)
  "Return the first tool request after the first typed tool failure."
  (let* ((failure
          (cl-find-if
           (lambda (event)
             (and (equal "tool.failed" (plist-get event :type))
                  (stringp (plist-get (plist-get event :payload) :error-type))))
           events))
         (failure-storage (and failure (plist-get failure :storage-id))))
    (and failure-storage
         (cl-find-if
          (lambda (event)
            (and (equal "tool.requested" (plist-get event :type))
                 (> (plist-get event :storage-id) failure-storage)))
          events))))

(defun superchat-recovery-five--envelope-mentions-candidate
    (events candidate-event-id)
  "Return non-nil when any prepared envelope mentions CANDIDATE-EVENT-ID."
  (cl-some
   (lambda (event)
     (and (equal "decision.snapshot.recorded" (plist-get event :type))
          (let ((json (plist-get (plist-get event :payload)
                                 :prepared-envelope-json)))
            (and (stringp json)
                 (string-match-p (regexp-quote candidate-event-id) json)))))
   events))

(defun superchat-recovery-five--attempt-2-experience-view (events)
  "Return the experience-view status seen in attempt 2's envelope, or nil."
  (let* ((snapshot
          (cl-find-if
           (lambda (event)
             (and (equal "decision.snapshot.recorded" (plist-get event :type))
                  (= 2 (plist-get (plist-get event :payload) :attempt))))
           events))
         (json (and snapshot
                    (plist-get (plist-get snapshot :payload)
                               :prepared-envelope-json))))
    (and json
         (string-match "\\\"experience-view\\\":{\\\"contract\\\":\\\"experience-view-v0\\\",\\\"status\\\":\\\"\\([a-z]+\\)\\\""
                       json)
         (match-string 1 json))))

(defun superchat-recovery-five--attempt-2-experience-prompt (events)
  "Return non-nil when attempt 2's envelope carries non-empty View prompt text."
  (let* ((snapshot
          (cl-find-if
           (lambda (event)
             (and (equal "decision.snapshot.recorded" (plist-get event :type))
                  (= 2 (plist-get (plist-get event :payload) :attempt))))
           events))
         (json (and snapshot
                    (plist-get (plist-get snapshot :payload)
                               :prepared-envelope-json))))
    (and json
         (string-match "\\\"prompt\\\":\\\"[Experience v0[^\"]+"
                       json)
         t)))

(defun superchat-recovery-five--evaluation-ref (setup)
  "Return the reviewed evaluation reference recorded in SETUP."
  (let ((evaluation-session
         (cl-find-if (lambda (session)
                       (string-prefix-p "evaluation-" session))
                     (append (plist-get setup :sessions) nil)))
        (inventory (plist-get setup :event-inventory)))
    (when evaluation-session
      (let* ((entry
              (cl-find-if
               (lambda (item)
                 (equal evaluation-session (plist-get item :session-id)))
               inventory))
             (events (and entry (plist-get entry :events))))
        (when-let ((evaluated
                    (cl-find-if
                     (lambda (event)
                       (equal "exercise.evaluated" (plist-get event :type)))
                     events)))
          (list :session-id evaluation-session
                :event-id (plist-get evaluated :event-id)))))))

(defun superchat-recovery-five--run-arm
    (directory arm setup sessions view-override &optional task-text policy
     max-tool-rounds)
  "Run one real recovery ARM against SETUP and return its full record.
SESSIONS are the Experience session ids the Runtime may read.  When
VIEW-OVERRIDE is a function it replaces the recovery assembler for this arm.
TASK-TEXT replaces the default stale-read task.  POLICY is an optional
experience-action-policy plist attached to the Run envelope."
  (let* ((name (format "five-%s" arm))
         (session (format "consumer-%s" name))
         (workspace (plist-get setup :workspace-id))
         (buffer-name (format " *recovery-%s*" name))
         (artifact-id (plist-get setup :artifact-id))
         (old-revision (plist-get setup :old-revision))
         (prompt
          (or task-text
              (format
               (concat
                "Your first tool call must be workspace_read, exactly once, "
                "for Artifact %s with expected_revision %s. Do not call "
                "workspace_info or any other tool before it. This is a "
                "read-only task: never call workspace_write, EditBuffer, "
                "ReplaceBuffer, or eshell-command. If it fails, make exactly "
                "one recovery Action using the available tools. Do not answer "
                "before that Action.")
               artifact-id old-revision)))
         (provider (if superchat-recovery-five--base-url
                       (make-llm-openai-compatible
                        :url superchat-recovery-five--base-url
                        :key "lm-studio"
                        :chat-model superchat-recovery-five--model)
                     (make-llm-ollama
                      :chat-model superchat-recovery-five--model
                      :host "localhost" :port 11434)))
         (superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat-runtime-recovery-intervention-enabled t)
         (superchat-response-timeout 90)
         (superchat-tool-timeout-multiplier 1.0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-max-tool-rounds (or max-tool-rounds 3))
         (superchat-llm-round-limit-action 'finalize)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-agent-max-tool-calls (or max-tool-rounds 3))
         (superchat-agent-confirm-destructive t)
         (superchat-lang "English")
         (superchat-llm-streaming t)
         (superchat-llm-tools-enabled t)
         (superchat-llm-backend provider)
         (envelope
          (append
           (list :contract "context-continuity-v0"
                 :status 'empty
                 :receipt '(:tape-head 0)
                 :disclosure '(:represented []))
           (when sessions
             (list :experience-session-ids (append sessions nil)))
           (when policy
             (list :experience-action-policy (copy-tree policy t)))))
         handle status submit-error events)
    (advice-add 'llm--log :around
                #'superchat-recovery-trace--llm-log-advice)
    (advice-add 'plz-media-type-request :around
                #'superchat-recovery-trace--transport-advice)
    (unwind-protect
        (condition-case err
            (progn
              (when (functionp view-override)
                (advice-add 'superchat-experience-assemble-recovery-view
                            :override view-override))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((superchat--session-id session))
                  (superchat-workspace-bind workspace nil session)))
              (setq handle
                    (superchat-runtime-submit
                     (list :prompt prompt :input prompt :clean-input prompt
                           :session-id session :workspace-id workspace
                           :buffer-name buffer-name
                           :preset (superchat--builtin-capability-preset "code")
                           :capability "code"
                           :target-model superchat-recovery-five--model
                           :tools '("workspace_read" "workspace_info")
                           :system-prompt superchat-acceptance-v26--system-prompt
                           :authority-policy "default" :attendance 'attended
                           :approval-function
                           #'superchat-acceptance-v26--approval-read-only-only
                           :round-limit-function (lambda () 'finalize)
                           :unknown-tool-retries 0 :record-conversation nil
                           :evidence-contract "epistemic-learning-v0"
                           :context-envelope envelope)))
              (let* ((run-id (plist-get handle :run-id))
                     (deadline (+ (float-time) 150)))
                (while (and (< (float-time) deadline)
                            (not (memq
                                  (plist-get
                                   (superchat-runtime-status run-id) :status)
                                  '(completed failed cancelled timed-out))))
                  (accept-process-output nil 0.1))
                (setq status
                      (plist-get (superchat-runtime-status run-id) :status))
                (when (memq status '(accepted running-model delivering))
                  (superchat-runtime-cancel run-id "five-method timeout"))))
          (error (setq submit-error (error-message-string err))))
      (when (functionp view-override)
        (advice-remove 'superchat-experience-assemble-recovery-view
                       view-override))
      (advice-remove 'llm--log #'superchat-recovery-trace--llm-log-advice)
      (advice-remove 'plz-media-type-request
                     #'superchat-recovery-trace--transport-advice))
    (setq events (ignore-errors (superchat-tape-replay-events session)))
    (let* ((requested
            (superchat-recovery-five--events events "tool.requested"))
           (requested-names
            (mapcar (lambda (event)
                      (plist-get (plist-get event :payload) :tool-name))
                    requested))
           (failed (superchat-recovery-five--events events "tool.failed"))
           (first-failure (car failed))
           (first-action (superchat-recovery-five--first-recovery-action events))
           (first-tool (and first-action
                            (plist-get (plist-get first-action :payload)
                                       :tool-name)))
           (interventions
            (superchat-recovery-five--events
             events "experience.intervention.recorded"))
           (candidate-event-id
            (plist-get
             (car (plist-get setup :candidates)) :event-id)))
      (list :arm arm :session session :run-id (and handle (plist-get handle :run-id))
            :status status
            :task prompt
            :status status
            :first-recovery-action first-tool
            :tool-requested requested-names
            :typed-failure
            (and first-failure
                 (equal "superchat-workspace-artifact-revision-error"
                        (plist-get (plist-get first-failure :payload)
                                   :error-type)))
            :intervention-recorded (and interventions t)
            :experience-view-status-attempt-2
            (superchat-recovery-five--attempt-2-experience-view events)
            :experience-prompt-attempt-2
            (superchat-recovery-five--attempt-2-experience-prompt events)
            :candidate-mentioned
            (and candidate-event-id
                 (superchat-recovery-five--envelope-mentions-candidate
                  events candidate-event-id))
            :submit-error submit-error
            :transport-requests
            (nreverse superchat-recovery-trace--transport)
            :llm-log (nreverse superchat-recovery-trace--llm-log)
            :tape-events events))))

(defun superchat-recovery-five--run ()
  "Run the five recovery-domain real-environment methods once."
  (let* ((output (getenv "SUPERCHAT_RECOVERY_FIVE_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (directory (and root (expand-file-name "five-P" root)))
         setup results)
    (unless (and output root)
      (error "SUPERCHAT_RECOVERY_FIVE_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite five-method trace: %s" output))
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
    (if superchat-recovery-five--base-url
        (require 'llm-openai)
      (require 'llm-ollama))
    ;; Seal one setup database before the first provider request.
    ;; The stale mutation is performed inside the producer Run so the
    ;; changed Artifact stays inside that Run's durable proof; the frozen
    ;; helper's post-terminal mutation is suppressed for the stale cell.
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (stale-p (eq (plist-get superchat-recovery-five--cell :kind) 'stale))
          (original-write (symbol-function 'superchat-workspace-write)))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (cl-letf (((symbol-function 'superchat-workspace-add-region)
                     (lambda (_beg _end &optional role workspace-id)
                       (let ((meta
                              (superchat-workspace-publish-mutable
                               "sealed producer artifact\n" role workspace-id)))
                         (when stale-p
                           (funcall original-write
                                    "changed after recorded revision" nil
                                    (plist-get meta :artifact-id)
                                    (plist-get meta :revision) workspace-id))
                         meta)))
                    ((symbol-function 'superchat-workspace-write)
                     (lambda (content &optional append artifact-id
                                      expected-revision workspace-id)
                       (unless stale-p
                         (funcall original-write content append artifact-id
                                  expected-revision workspace-id)))))
            (setq setup
                  (superchat-acceptance-v26--setup-experience
                   superchat-recovery-five--cell)))
        (superchat-db-close)))
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (scope superchat-experience-workspace-revision-recovery-scope)
          (sessions (append (plist-get setup :sessions) nil))
          (context (plist-get setup :context))
          (candidate-ref (car (plist-get setup :candidates)))
          (evaluation-ref (superchat-recovery-five--evaluation-ref setup))
          (promotion (car (plist-get setup :promotions))))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (progn
            ;; 1. Matched control/treatment.
            (push (superchat-recovery-five--run-arm
                   directory 'matched-control setup nil nil)
                  results)
            (push (superchat-recovery-five--run-arm
                   directory 'matched-treatment setup sessions nil)
                  results)
            ;; 2. View-content ablation: satisfied lineage, no instruction.
            (push (superchat-recovery-five--run-arm
                   directory 'ablation setup sessions
                   (lambda (_failure _session-ids)
                     (list :status 'satisfied
                           :scope scope
                           :context context
                           :prompt nil
                           :items []
                           :receipt (list :candidate candidate-ref
                                          :promotion promotion)))
                   nil)
                  results)
            ;; 3. Wording transfer: new wording, same reviewed rule.
            (push (superchat-recovery-five--run-arm
                   directory 'transfer setup sessions nil
                   (format
                    (concat
                     "A stale revision was detected for a shared Workspace "
                     "Artifact %s. Begin with one workspace_read carrying "
                     "expected_revision %s; expect a revision mismatch. Then "
                     "pick exactly one corrective tool call for the same "
                     "Artifact. Do not answer with prose or parallel tools.")
                    (plist-get setup :artifact-id)
                    (plist-get setup :old-revision)))
                  results)
            ;; 4. Negative transfer: conflicting relation must fail closed.
            (let* ((negative-relation
                    (superchat-experience-record-relation
                     (list :session-id "five-negative-relation"
                           :actor-id "agent:test"
                           :correlation-id "five-negative"
                           :relation 'fails-under
                           :from candidate-ref :to context :scope scope
                           :basis (list evaluation-ref)
                           :reason "Negative-transfer test.")))
                   (negative-sessions
                    (append sessions (list "five-negative-relation"))))
              (push (superchat-recovery-five--run-arm
                     directory 'negative-transfer setup negative-sessions nil nil
                     '(:kind "workspace-revision-recovery"
                       :phase "awaiting-info"
                       :required-tool "workspace_info"
                       :dependent-tools ("workspace_read")))
                    results))
            ;; 5. Persistence and revocation.
            (superchat-db-close)
            (superchat-db-open)
            (push (superchat-recovery-five--run-arm
                   directory 'persistence-after-reopen setup sessions nil)
                  results)
            (superchat-experience-record-disposition
             (list :session-id (plist-get promotion :session-id)
                   :actor-id "user"
                   :candidate candidate-ref :scope scope :decision 'revoke
                   :basis (list evaluation-ref)
                   :reason "User revoked the reviewed rule."
                   :prior promotion))
            (push (superchat-recovery-five--run-arm
                   directory 'persistence-after-revocation setup sessions nil)
                  results)
            (setq results (nreverse results)))
        (superchat-db-close)))
    (let ((trace
           (list :format "superchat-experience-recovery-five-methods-v1"
                 :claim "five real-environment methods around reviewed recovery"
                 :source
                 (list :emacs emacs-version
                       :model superchat-recovery-five--model
                       :jj (superchat-acceptance-v26--jj-identity
                            superchat-acceptance-v26--repo-root)
                       :dependencies
                       (superchat-recovery-trace--dependency-records))
                 :setup
                 (list :cell superchat-recovery-five--cell
                       :workspace-id (plist-get setup :workspace-id)
                       :artifact-id (plist-get setup :artifact-id)
                       :old-revision (plist-get setup :old-revision)
                       :scope superchat-experience-workspace-revision-recovery-scope
                       :candidates (plist-get setup :candidates)
                       :promotions (plist-get setup :promotions)
                       :relations (plist-get setup :relations)
                       :revocations (plist-get setup :revocations))
                 :methods results
                 :provider-requests
                 (apply #'+ (mapcar (lambda (result)
                                     (length (plist-get result
                                                        :transport-requests)))
                                   results))
                 :notes
                 '(:no-retry t
                   :control "No View; Runtime intervention finds no Candidate."
                   :treatment "Satisfied View assembled on the typed failure."
                   :ablation "Satisfied View with the instruction removed."
                   :transfer "New task wording, same reviewed abstract rule."
                   :negative "Conflicting relation must yield empty/blocked View."
                   :persistence "DB reopened before View assembly."
                   :revocation "User revocation appended before the final Run."
                   :interpretation "Each method is scored independently; one model call never proves learning."
                   :default-remains-off t))))
      (superchat-recovery-trace--write output trace)
      (princ (format "recovery five-method trace: %s\nrequests=%d\n"
                     output (plist-get trace :provider-requests))))))

(when (equal (getenv "SUPERCHAT_RECOVERY_FIVE_EXECUTE") "1")
  (condition-case err
      (progn (superchat-recovery-five--run) (kill-emacs 0))
    (error
     (princ (format "recovery five-method infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-recovery-five-methods)

;;; recovery-five-methods.el ends here
