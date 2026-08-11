;;; run.el --- Once-only Experience View internal acceptance -*- lexical-binding: t; -*-

;; This runner is deliberately separate from product code.  It executes the
;; frozen v1 cohort once when SUPERCHAT_ACCEPTANCE_V1_RUN is set by run.sh.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst superchat-acceptance-v1--runner-directory
  (file-name-directory (or load-file-name buffer-file-name)))

(add-to-list 'load-path (expand-file-name "../.." superchat-acceptance-v1--runner-directory))

(require 'superchat)
(require 'superchat-claims)
(require 'superchat-db)
(require 'superchat-experience)
(require 'superchat-runtime)
(require 'superchat-tape)
(require 'superchat-tools)
(require 'superchat-workspace)

(defconst superchat-acceptance-v1--contract-digest
  "4a3d69f38bfc7773a79c1e9b62cd6f10df7b09008cbbcd58278fa612a6e90a39")

(defconst superchat-acceptance-v1--candidate-text
  "Call workspace_info once, take the revision reported for the same artifact_id, then call workspace_read with that artifact_id and that exact expected_revision; never reuse or invent a revision.")

(defconst superchat-acceptance-v1--context-text
  "Cross-Run workspace_read failed with superchat-workspace-artifact-revision-error.")

(defconst superchat-acceptance-v1--tools
  '("read-file" "list-files" "search-text" "read_buffer" "EditBuffer"
    "ReplaceBuffer" "eshell-command" "workspace_read" "workspace_write"
    "workspace_info"))

(defconst superchat-acceptance-v1--source-digests
  '(("superchat.el" . "20807812e04e795946b9ef872885fa3b446a473597a401a74f532c7c04b9ead0")
    ("superchat-runtime.el" . "4f48e7fe43a469ec1aee2739524fba2b4d1daff14f6e4cb06ce3ca44d8dd556a")
    ("superchat-llm.el" . "71614821b7cf74ef6b047d841403b546fd29f20f28ca783f8e4433d57a79b462")
    ("superchat-authority.el" . "428a645ec3986c6fe0ac64dc4b2634ce0fa6f7c00a05378cc672efbdbab52937")
    ("superchat-workspace.el" . "c7fd05abc3d38d657daf3b94f3ba1ef54c6be6744df293601d436de5d0459634")
    ("superchat-experience.el" . "335305878437903fcfa36cd4e6cd64d03a3ff3a1d11d1751214a82ac56128531")
    ("superchat-closure.el" . "8e6e4bad3ce76b759794da84ea8c03d2f43d7bf4c165350fd92c4b99f086d198")
    ("superchat-tape.el" . "68b0d592c9c797f10e94b81e09155f515a9c2c64bd40038bc566ba9ec68fcf86")
    ("superchat-projection.el" . "4597d6623de642e12d7dc210a4a0c3fbbe650e96e9a7063b24d009073430867f")
    ("superchat-preset.el" . "09458c6356e30199e11ac0cd779e7be9d14ea59db73cf80de5b5a3bd032e878f")
    ("superchat-tools.el" . "45e6d459d8bcd168e1173f0e0092ba5e214e23bd2ca68b41fa582490cd8ff779")
    ("superchat-dispatcher.el" . "c8b6a8e91842f1d00131cf68f3d1238f194f80404298a7f111634319c922db48")
    ("superchat-prompt-hooks.el" . "362fec26a1dc776a095f687395f76e26c53b6e0d39f6fe3c18cdf1dfef8d4f0b")
    ("superchat-agent-loop.el" . "f2aae4b32cee04ca049e43286dd2cfeca316b952b2b8ce734b58418ea70aaab8")
    ("superchat-db.el" . "f4a76b0239f0041f7e895e3622df820ac1aaf85bec5aedfca7b59077fe0b1536")))

(defconst superchat-acceptance-v1--system-prompt
  (concat
   "Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools."
   "\n\n\n"
   "You can batch INDEPENDENT read-only tool calls (read-file, list-files, search-text, find-files, lsp-references, lsp-hover, read_buffer) into a single turn.\n"
   "Tools that depend on each other's output must wait for the previous result."))

(defun superchat-acceptance-v1--repo-root ()
  "Return the checkout root containing this runner."
  (expand-file-name "../.." superchat-acceptance-v1--runner-directory))

(defun superchat-acceptance-v1--file-hash (path)
  "Return SHA-256 for readable PATH, or nil."
  (when (file-readable-p path)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (secure-hash 'sha256 (current-buffer)))))

(defun superchat-acceptance-v1--write (path value)
  "Persist Lisp-readable VALUE at PATH."
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (let ((print-length nil) (print-level nil))
      (prin1 value (current-buffer))
      (insert "\n"))))

(defun superchat-acceptance-v1--events (events type)
  "Return EVENTS whose Tape TYPE matches TYPE."
  (cl-remove-if-not (lambda (event) (equal type (plist-get event :type))) events))

(defun superchat-acceptance-v1--position (event events)
  "Return EVENT's stable position in ordered EVENTS."
  (or (cl-position event events :test #'eq) -1))

(defun superchat-acceptance-v1--after (event events)
  "Return EVENTS strictly after EVENT in their retained order."
  (let ((position (superchat-acceptance-v1--position event events)))
    (and (>= position 0) (nthcdr (1+ position) events))))

(defun superchat-acceptance-v1--json-empty-arguments-p (json)
  "Return non-nil when exact authority argument JSON names no arguments."
  (condition-case nil
      (let* ((value (json-parse-string json :object-type 'plist :array-type 'list
                                       :null-object nil :false-object nil))
             (named (or (plist-get value :named-arguments)
                        (plist-get value :named_arguments)))
             (positional (or (plist-get value :positional-arguments)
                             (plist-get value :positional_arguments))))
        (and (null named) (null positional)))
    (error nil)))

(defun superchat-acceptance-v1--receipt-matches-p (receipt expected)
  "Return non-nil when RECEIPT names EXPECTED candidate reference.
When EXPECTED is nil, require a candidate receipt but leave identity checking to
the integration runner that owns the setup manifest."
  (let ((candidate (plist-get receipt :candidate)))
    (and candidate (or (null expected) (equal candidate expected)))))

(defun superchat-acceptance-v1--envelope-text (event)
  "Return EVENT's exact prepared envelope bytes, or an empty string."
  (or (plist-get (plist-get event :payload) :prepared-envelope-json) ""))

(defun superchat-acceptance-v1-verify-cell (cell events transport-count)
  "Verify one frozen acceptance CELL from ordered Tape EVENTS.
Return `(:passed BOOL :errors LIST)'.  This is intentionally a pure evidence
check: it neither invokes a model nor repairs a failed cell."
  (let* ((positive (plist-get cell :positive))
         (candidate-text (plist-get cell :candidate-text))
         (candidate-ref (plist-get cell :candidate-ref))
         (errors '())
         (snapshots (superchat-acceptance-v1--events events "decision.snapshot.recorded"))
         (interventions (superchat-acceptance-v1--events events "experience.intervention.recorded")))
    (unless (and (integerp transport-count) (<= 0 transport-count 2))
      (push "transport count is outside the 0..2 ceiling" errors))
    (unless (= (length snapshots) transport-count)
      (push "transport count and decision snapshots differ" errors))
    (if positive
        (let* ((failures
                (cl-remove-if-not
                 (lambda (event)
                   (let ((payload (plist-get event :payload)))
                     (and (equal "workspace_read" (plist-get payload :tool-name))
                          (equal "superchat-workspace-artifact-revision-error"
                                 (plist-get payload :error-type)))))
                 (superchat-acceptance-v1--events events "tool.failed")))
               (failure (car failures))
               (intervention (car interventions))
               (second-snapshots
                (cl-remove-if-not
                 (lambda (event) (= 2 (plist-get (plist-get event :payload) :attempt)))
                 snapshots)))
          (unless (= 1 (length failures))
            (push "positive cell lacks one typed workspace_read failure" errors))
          (unless (= 1 (length interventions))
            (push "positive cell lacks exactly one intervention" errors))
          (when intervention
            (unless (and failure
                         (equal (plist-get failure :event-id)
                                (plist-get intervention :causation-id)))
              (push "intervention is not caused by the typed failure" errors))
            (unless (superchat-acceptance-v1--receipt-matches-p
                     (plist-get (plist-get intervention :payload) :receipt)
                     candidate-ref)
              (push "intervention receipt names the wrong candidate" errors)))
          (unless (= 1 (length second-snapshots))
            (push "positive cell lacks exactly one attempt-2 snapshot" errors))
          (when-let* ((snapshot (car second-snapshots)))
            (let ((envelope (superchat-acceptance-v1--envelope-text snapshot)))
              (unless (and (stringp candidate-text)
                           (string-match-p (regexp-quote candidate-text) envelope))
                (push "attempt-2 envelope omits the Experience prompt" errors))
              (when candidate-ref
                (unless (string-match-p
                         (regexp-quote (plist-get candidate-ref :event-id)) envelope)
                  (push "attempt-2 envelope omits the Candidate receipt" errors)))))
          (when intervention
            (let* ((action
                    (car (superchat-acceptance-v1--events
                          (superchat-acceptance-v1--after intervention events)
                          "tool.requested")))
                   (payload (and action (plist-get action :payload)))
                   (execution-id (and payload (plist-get payload :execution-id)))
                   (outcomes
                    (cl-remove-if-not
                     (lambda (event)
                       (let ((outcome (plist-get event :payload)))
                         (and (equal "workspace_info" (plist-get outcome :tool-name))
                              (equal execution-id (plist-get outcome :execution-id)))))
                     (superchat-acceptance-v1--events events "tool.completed"))))
              (unless (and action
                           (equal "workspace_info" (plist-get payload :tool-name))
                           (superchat-acceptance-v1--json-empty-arguments-p
                            (plist-get payload :arguments-json)))
                (push "first Action after intervention is not no-argument workspace_info" errors))
              (unless (= 1 (length outcomes))
                (push "First Recovery Action has no unique completed Outcome" errors)))))
      (when interventions
        (push "negative cell recorded an intervention" errors))
      (dolist (snapshot snapshots)
        (when (and (stringp candidate-text)
                   (string-match-p
                    (regexp-quote candidate-text)
                    (superchat-acceptance-v1--envelope-text snapshot)))
          (push "negative envelope exposes blocked Candidate bytes" errors)))
      (dolist (event (superchat-acceptance-v1--events events "tool.requested"))
        (when (member (plist-get (plist-get event :payload) :tool-name)
                      '("workspace_write" "workspace_publish"))
          (push "negative cell requested a write tool" errors))))
    (list :passed (null errors) :errors (nreverse errors))))

(defun superchat-acceptance-v1--append (session turn run type event-id cause payload &optional workspace)
  "Append one deterministic setup event and return its record."
  (superchat-tape-append-event-record
   (append (list :type type :event-id event-id :schema 1
                 :session-id session :turn-id turn :run-id run :actor-id "lead"
                 :payload payload)
           (when cause (list :causation-id cause))
           (when workspace (list :workspace-id workspace)))))

(defun superchat-acceptance-v1--closed-run (session stem before-terminal)
  "Create a closed setup Run and execute BEFORE-TERMINAL within it."
  (let* ((turn (format "%s-turn" stem))
         (run (format "%s-run" stem))
         (intent (format "%s-intent" stem))
         (start (format "%s-start" stem))
         (decision (format "%s-decision" stem))
         (output (format "%s-output" stem))
         (response (format "%s-response" stem))
         (terminal (format "%s-terminal" stem)))
    (superchat-acceptance-v1--append session turn run "intent.received" intent nil nil)
    (superchat-acceptance-v1--append session turn run "run.started" start intent
                                     '(:status running-model :capability "code"))
    (superchat-acceptance-v1--append session turn run "decision.snapshot.recorded" decision start
                                     '(:evidence-contract "epistemic-learning-v0" :attempt 1))
    (superchat-acceptance-v1--append session turn run "output.first.recorded" output decision
                                     '(:evidence-contract "epistemic-learning-v0" :kind "text" :text "setup"))
    (superchat-acceptance-v1--append session turn run "response.completed" response output
                                     '(:status completed :content-size 5))
    (funcall before-terminal (list :session-id session :turn-id turn :run-id run
                                  :response-id response))
    (superchat-acceptance-v1--append session turn run "run.completed" terminal response
                                     '(:status completed))
    (superchat-acceptance-v1--append session turn run "turn.completed"
                                     (format "%s-turn-terminal" stem) terminal
                                     '(:status completed))
    (list :session-id session :turn-id turn :run-id run
          :decision (list :session-id session :event-id decision)
          :output (list :session-id session :event-id output)
          :terminal (list :session-id session :event-id terminal))))

(defun superchat-acceptance-v1--record-evaluation (scope candidate session)
  "Create a valid closed evaluator Run for CANDIDATE in SCOPE and SESSION."
  (let* ((run (superchat-acceptance-v1--closed-run
               session "evaluation" #'ignore))
         (spec (list :session-id session :actor-id "evaluator:acceptance"
                     :exercise-id "experience-view-internal-acceptance-v1"
                     :scope scope :run-id (plist-get run :run-id)
                     :decision (plist-get run :decision) :terminal (plist-get run :terminal)
                     :evaluator '(:id "frozen-acceptance-setup" :version 1)
                     :evaluator-manifest-sha256 (concat "sha256:" (make-string 64 ?a))
                     :verdict 'passed :observations '(:setup "reviewed")
                     :metrics '(:score 1)
                     :evidence (list (plist-get run :output) candidate))))
    (superchat-experience-record-evaluation spec)))

(defun superchat-acceptance-v1--setup-experience (name kind)
  "Install the exact Experience setup for cohort NAME and KIND.
Return setup manifest fields, including the one Artifact and Context."
  (let* ((scope superchat-experience-workspace-revision-recovery-scope)
         (workspace (format "workspace-%s" name))
         (producer-session (format "producer-%s" name))
         (artifact nil)
         (producer
          (superchat-acceptance-v1--closed-run
           producer-session "producer"
           (lambda (run)
             (let ((superchat--tool-execution-context
                    (append run (list :actor-id "lead" :workspace-id workspace
                                      :causation-id (plist-get run :response-id)))))
               (with-current-buffer (get-buffer-create (format " *acceptance-%s*" name))
                 (erase-buffer)
                 (insert "sealed producer artifact\n")
                 (superchat-workspace-bind workspace nil producer-session)
                 (setq artifact
                       (superchat-workspace-add-region (point-min) (point-max) 'workspace workspace)))))))
         (artifact-id (plist-get artifact :artifact-id))
         (old-revision (plist-get artifact :revision))
         (context
          (superchat-claim-record superchat-acceptance-v1--context-text scope
                                  (list (plist-get producer :output))
                                  (format "context-%s" name) "agent:setup" nil 'context))
         (context-ref (list :session-id (format "context-%s" name)
                            :event-id (plist-get context :event-id)))
         candidates promotions relations)
    (when (memq kind '(stale fails multiple))
      (let ((superchat--tool-execution-context
             (list :session-id producer-session :turn-id (plist-get producer :turn-id)
                   :run-id (plist-get producer :run-id) :actor-id "lead"
                   :workspace-id workspace :causation-id (plist-get (plist-get producer :terminal) :event-id))))
        (with-current-buffer (get-buffer (format " *acceptance-%s*" name))
          (superchat-workspace-write "changed after recorded revision" nil artifact-id old-revision workspace))))
    (unless (eq kind 'absent)
      (let ((count (if (eq kind 'multiple) 2 1)))
        (dotimes (index count)
          (let* ((candidate
                  (superchat-claim-record
                   superchat-acceptance-v1--candidate-text scope
                   (list (plist-get producer :output))
                   (format "candidate-%s-%d" name index) "agent:setup" nil 'candidate))
                 (candidate-ref
                  (list :session-id (format "candidate-%s-%d" name index)
                        :event-id (plist-get candidate :event-id)))
                 (evaluation (superchat-acceptance-v1--record-evaluation
                              scope candidate-ref (format "evaluation-%s-%d" name index)))
                 (evaluation-ref
                  (list :session-id (format "evaluation-%s-%d" name index)
                        :event-id (plist-get evaluation :event-id)))
                 (relation
                  (superchat-experience-record-relation
                   (list :session-id (format "relation-%s-%d" name index)
                         :actor-id "agent:setup" :correlation-id (format "%s-%d" name index)
                         :relation (if (eq kind 'fails) 'fails-under 'applies-under)
                         :from candidate-ref :to context-ref :scope scope
                         :basis (list evaluation-ref))))
                 (promotion
                  (superchat-experience-record-disposition
                   (list :session-id (format "promotion-%s-%d" name index)
                         :actor-id "user" :candidate candidate-ref :scope scope :decision 'promote
                         :basis (list evaluation-ref) :reason "Reviewed acceptance setup."))))
            (push candidate-ref candidates)
            (push (list :session-id (format "promotion-%s-%d" name index)
                        :event-id (plist-get promotion :event-id)) promotions)
            (push (list :session-id (format "relation-%s-%d" name index)
                        :event-id (plist-get relation :event-id)) relations)
            (when (eq kind 'conflict)
              (push
               (list :session-id (format "relation-%s-%d-conflict" name index)
                     :event-id
                     (plist-get
                      (superchat-experience-record-relation
                       (list :session-id (format "relation-%s-%d-conflict" name index)
                             :actor-id "agent:setup" :correlation-id (format "%s-%d-conflict" name index)
                             :relation 'fails-under :from candidate-ref :to context-ref :scope scope
                             :basis (list evaluation-ref)))
                      :event-id))
               relations))
            (when (eq kind 'revoked)
              (superchat-experience-record-disposition
               (list :session-id (format "promotion-%s-%d" name index)
                     :actor-id "user" :candidate candidate-ref :scope scope :decision 'revoke
                     :prior (car promotions) :basis (list evaluation-ref)
                     :reason "Revoked acceptance setup.")))))))
    (list :workspace-id workspace :producer producer :artifact artifact
          :artifact-id artifact-id :old-revision old-revision
          :context context-ref :candidates (nreverse candidates)
          :promotions (nreverse promotions) :relations (nreverse relations))))

(defun superchat-acceptance-v1--consumer-prompt (setup stale)
  "Return the frozen consumer prompt for SETUP and STALE."
  (if stale
      (format "Call workspace_read exactly once for Artifact %s with expected_revision %s. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action."
              (plist-get setup :artifact-id) (plist-get setup :old-revision))
    (format "Call workspace_read exactly once for Artifact %s without expected_revision. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action."
            (plist-get setup :artifact-id))))

(defun superchat-acceptance-v1--run-cell (cell)
  "Execute one CELL exactly once and return retained evidence summary."
  (let* ((name (plist-get cell :name))
         (directory (expand-file-name name superchat-acceptance-v1--evidence-root))
         (superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat-runtime-recovery-intervention-enabled t)
         (superchat-response-timeout 60)
         (superchat-tool-timeout-multiplier 1.0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-max-tool-rounds 2)
         (superchat-llm-round-limit-action 'stop)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-agent-max-tool-calls 2)
         (superchat-agent-confirm-destructive t)
         (setup nil) (handle nil) (events nil) (transport-count 0) (verdict nil))
    (make-directory directory t)
    (unwind-protect
        (condition-case err
            (progn
              (superchat-db-close)
              (superchat-db-open)
              (setq setup (superchat-acceptance-v1--setup-experience name (plist-get cell :kind)))
              (let* ((session (format "consumer-%s" name))
                     (buffer (get-buffer-create (format " *acceptance-consumer-%s*" name)))
                     (preset (superchat--builtin-capability-preset "code"))
                     (prompt (superchat-acceptance-v1--consumer-prompt
                              setup (plist-get cell :stale)))
                     (advice
                      (lambda (original &rest args)
                        (cl-incf transport-count)
                        (apply original args))))
                (with-current-buffer buffer
                  (superchat-workspace-bind (plist-get setup :workspace-id) nil session))
                (advice-add 'llm-chat-async :around advice)
                (advice-add 'llm-chat-streaming :around advice)
                (unwind-protect
                    (progn
                      (setq handle
                            (superchat-runtime-submit
                             (list :prompt prompt :input prompt :clean-input prompt
                                   :session-id session :workspace-id (plist-get setup :workspace-id)
                                   :buffer-name (buffer-name buffer) :preset preset :capability "code"
                                   :target-model "qwen3.5:9b" :tools superchat-acceptance-v1--tools
                                   :system-prompt superchat-acceptance-v1--system-prompt
                                   :authority-policy "default" :attendance 'attended
                                   :approval-function
                                   (lambda (request)
                                     (cl-every
                                      (lambda (effect)
                                        (eq (plist-get effect :effect) 'local-read))
                                      (plist-get request :effects)))
                                   :round-limit-function (lambda () 'stop)
                                   :unknown-tool-retries 0 :record-conversation nil
                                   :evidence-contract "epistemic-learning-v0"
                                   :context-envelope
                                   '(:contract "context-continuity-v0" :status empty
                                     :receipt (:tape-head 0) :disclosure (:represented [])))))
                      (let ((deadline (+ (float-time) 125)))
                        (while (and (< (float-time) deadline)
                                    (not (memq (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)
                                               '(completed failed cancelled timed-out))))
                          (accept-process-output nil 0.05))
                        (when (memq (plist-get (superchat-runtime-status (plist-get handle :run-id)) :status)
                                    '(accepted running-model delivering))
                          (superchat-runtime-cancel (plist-get handle :run-id) "Acceptance timeout"))))
                  (advice-remove 'llm-chat-async advice)
                  (advice-remove 'llm-chat-streaming advice))
                (setq events (superchat-tape-replay-events session))
                (setq verdict
                      (superchat-acceptance-v1-verify-cell
                       (append cell
                               (list :candidate-text superchat-acceptance-v1--candidate-text
                                     :candidate-ref (car (plist-get setup :candidates))))
                       events transport-count))))
          (error
           (setq verdict (list :passed nil :errors (list (error-message-string err))))))
      (superchat-acceptance-v1--write
       (expand-file-name "result.el" directory)
       (list :cell cell :setup setup :handle handle :events events
             :transport-count transport-count :verdict verdict))
      (superchat-db-close))
    (list :name name :transport-count transport-count :verdict verdict
          :evidence-directory directory)))

(defun superchat-acceptance-v1--command (program &rest args)
  "Return PROGRAM ARGS exit code and stdout without shell interpolation."
  (with-temp-buffer
    (let ((status (apply #'process-file program nil (current-buffer) nil args)))
      (list :exit status :output (buffer-string)))))

(defun superchat-acceptance-v1--dependency-record (feature)
  "Return the resolved record for required package FEATURE, or signal."
  (unless (require feature nil t)
    (error "Cannot require dependency %s" feature))
  (let* ((located (locate-library (symbol-name feature)))
         (path (if (and located (string-suffix-p ".elc" located)
                        (file-readable-p (concat (file-name-sans-extension located) ".el")))
                   (concat (file-name-sans-extension located) ".el")
                 located)))
    (unless (and path (file-readable-p path))
      (error "Cannot record dependency source for %s" feature))
    (list :feature (symbol-name feature)
          :version (with-temp-buffer
                     (insert-file-contents path nil 0 1000)
                     (when (re-search-forward "^;;[[:space:]]*Package-Version:[[:space:]]*\\(.+\\)$" nil t)
                       (string-trim (match-string 1))))
          :path (file-name-nondirectory path)
          :sha256 (superchat-acceptance-v1--file-hash path))))

(defun superchat-acceptance-v1--preflight ()
  "Seal and return the v1 launch manifest before any model request."
  (require 'llm)
  (require 'llm-ollama)
  (let* ((root (superchat-acceptance-v1--repo-root))
         (llm-root (or (getenv "SUPERCHAT_ACCEPTANCE_V1_LLM_DIR")
                       (error "v1 launch did not provide the private llm checkout")))
         (provider (make-llm-ollama :chat-model "qwen3.5:9b" :host "localhost" :port 11434))
         (product
          (mapcar
           (lambda (pair)
             (let ((actual (superchat-acceptance-v1--file-hash
                            (expand-file-name (car pair) root))))
               (unless (equal actual (cdr pair))
                 (error "Sealed product source mismatch: %s" (car pair)))
               (cons (car pair) actual)))
           superchat-acceptance-v1--source-digests))
         (llm-file (locate-library "llm"))
         (adapter (locate-library "llm-ollama"))
         (commit (string-trim (plist-get (superchat-acceptance-v1--command
                                          "git" "-C" llm-root "rev-parse" "HEAD") :output)))
         (ollama-version (superchat-acceptance-v1--command "ollama" "--version"))
         (model-list (superchat-acceptance-v1--command "ollama" "list"))
         (models-root (or (getenv "OLLAMA_MODELS")
                          (expand-file-name ".ollama/models" (getenv "HOME"))))
         (manifest-path (expand-file-name
                         "manifests/registry.ollama.ai/library/qwen3.5/9b" models-root))
         (manifest-digest (superchat-acceptance-v1--file-hash manifest-path))
         (manifest-content (when (file-readable-p manifest-path)
                             (with-temp-buffer
                               (insert-file-contents-literally manifest-path)
                               (buffer-string))))
         (tools (superchat-get-llm-tools superchat-acceptance-v1--tools))
         (schemas (and tools (superchat--llm-evidence-tool-records tools))))
    (unless (and (featurep 'llm) (featurep 'llm-ollama) (llm-ollama-p provider))
      (error "Pinned llm-ollama provider did not construct"))
    (unless (equal commit "34ac61091994c10bda45eed54433a1a87ca26af1")
      (error "Pinned llm checkout is at %s" commit))
    (unless (equal (superchat-acceptance-v1--file-hash adapter)
                   "96141fa9c58ada05684cc1f071e865986087cc01db640f41253f17aba581f17d")
      (error "llm-ollama adapter digest differs"))
    (unless (equal emacs-version "31.0.91") (error "Emacs version differs: %s" emacs-version))
    (unless (and (zerop (plist-get ollama-version :exit))
                 (string-match-p "0\\.32\\.6" (plist-get ollama-version :output)))
      (error "Ollama version differs"))
    (unless (and (zerop (plist-get model-list :exit))
                 (string-match-p "qwen3\\.5:9b[[:space:]]+6488c96fa5fa" (plist-get model-list :output))
                 (equal manifest-digest "6488c96fa5faab64bb65cbd30d4289e20e6130ef535a93ef9a49f42eda893ea7")
                 (string-match-p "sha256:dec52a44569a2a25341c4e4d3fee25846eed4f6f0b936278e3a3c900bb99d37c"
                                 manifest-content))
      (error "Ollama model identity differs"))
    (unless (= (length schemas) 10) (error "Profile tool schemas are incomplete"))
    (list :contract-sha256 superchat-acceptance-v1--contract-digest
          :emacs emacs-version :product-source product :llm-commit commit
          :llm-version (with-temp-buffer
                         (insert-file-contents llm-file nil 0 1000)
                         (when (re-search-forward "^;;[[:space:]]*Package-Version:[[:space:]]*\\(.+\\)$" nil t)
                           (string-trim (match-string 1))))
          :llm-ollama-sha256 (superchat-acceptance-v1--file-hash adapter)
          :dependencies (mapcar #'superchat-acceptance-v1--dependency-record
                                '(compat plz plz-event-source plz-media-type))
          :provider-class "llm-ollama" :provider-capabilities (llm-capabilities provider)
          :endpoint "http://localhost:11434" :model "qwen3.5:9b"
          :model-manifest-sha256 manifest-digest
          :model-layer-digest "sha256:dec52a44569a2a25341c4e4d3fee25846eed4f6f0b936278e3a3c900bb99d37c"
          :ollama ollama-version :profile "code" :tool-schemas schemas
          :system-prompt superchat-acceptance-v1--system-prompt
          :system-prompt-sha256 (secure-hash 'sha256 superchat-acceptance-v1--system-prompt)
          :topology "single" :language "English" :active-extensions []
          :reasoning "none" :max-tool-calls 2 :max-tool-rounds 2
          :unknown-tool-retries 0 :round-limit "stop" :timeout 60
          :timeout-multiplier 1.0 :authority-policy "authority-v1/default"
          :attendance "attended" :approval "read-only-only")))

(defvar superchat-acceptance-v1--evidence-root nil)

(defun superchat-acceptance-v1-run ()
  "Execute the frozen v1 cohort once and return its overall verdict."
  (let* ((root (or (getenv "SUPERCHAT_ACCEPTANCE_V1_EVIDENCE_DIR")
                   (error "v1 launch did not provide an evidence directory")))
         (superchat-acceptance-v1--evidence-root (file-name-as-directory root))
         (manifest (superchat-acceptance-v1--preflight))
         (superchat-llm-backend (make-llm-ollama :chat-model "qwen3.5:9b" :host "localhost" :port 11434))
         (cells '((:name "P-missing" :positive t :kind missing :stale nil)
                  (:name "P-stale" :positive t :kind stale :stale t)
                  (:name "N-absent" :positive nil :kind absent :stale nil)
                  (:name "N-fails" :positive nil :kind fails :stale t)
                  (:name "N-conflict" :positive nil :kind conflict :stale nil)
                  (:name "N-multiple" :positive nil :kind multiple :stale t)
                  (:name "N-revoked" :positive nil :kind revoked :stale nil))))
    (make-directory superchat-acceptance-v1--evidence-root t)
    ;; This is sealed before the first provider request.  The per-cell Tape
    ;; holds the request-specific prepared envelopes and their exact hashes.
    (superchat-acceptance-v1--write
     (expand-file-name "launch-manifest.el" superchat-acceptance-v1--evidence-root)
     manifest)
    (let* ((results (mapcar #'superchat-acceptance-v1--run-cell cells))
           (passed (cl-every (lambda (result) (plist-get (plist-get result :verdict) :passed)) results))
           (total (apply #'+ (mapcar (lambda (result) (plist-get result :transport-count)) results)))
           (summary (list :passed (and passed (<= total 14)) :transport-count total
                          :cells results :default-remains-off (not (and passed (<= total 14))))))
      (superchat-acceptance-v1--write
       (expand-file-name "cohort-verdict.el" superchat-acceptance-v1--evidence-root) summary)
      summary)))

(when (getenv "SUPERCHAT_ACCEPTANCE_V1_RUN")
  (condition-case err
      (if (getenv "SUPERCHAT_ACCEPTANCE_V1_PREFLIGHT_ONLY")
          (let* ((root (getenv "SUPERCHAT_ACCEPTANCE_V1_EVIDENCE_DIR"))
                 (superchat-acceptance-v1--evidence-root (file-name-as-directory root))
                 (manifest (superchat-acceptance-v1--preflight)))
            (make-directory superchat-acceptance-v1--evidence-root t)
            (superchat-acceptance-v1--write
             (expand-file-name "launch-manifest.el" superchat-acceptance-v1--evidence-root)
             manifest)
            (message "Experience acceptance v1 preflight: sealed")
            (kill-emacs 0))
        (let ((summary (superchat-acceptance-v1-run)))
          (message "Experience acceptance v1: %S" summary)
          (kill-emacs (if (plist-get summary :passed) 0 1))))
    (error
     (message "Experience acceptance v1 preflight failed: %s" (error-message-string err))
     (kill-emacs 1))))

(provide 'superchat-experience-acceptance-v1)

;;; run.el ends here
