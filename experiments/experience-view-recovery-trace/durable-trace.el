;;; durable-trace.el --- One durable-artifact recovery diagnostic -*- lexical-binding: t; -*-

;;; Commentary:
;; This is a fresh, one-cell diagnostic.  It deliberately does not claim or
;; modify the consumed v2.6 assignment.  The cell uses a generated-file
;; Artifact, which is the supported cross-Run boundary, and records the full
;; product-level Runtime/llm/Authority/Tape trace for the complete recovery
;; path.
;; A live buffer-region may still be shared across Runs while its markers stay
;; in this Emacs process; projected/restarted buffer-regions are unresolved.

;;; Code:

(require 'cl-lib)
(require 'package)
(require 'subr-x)

;; Reuse only the existing setup and trace capture helpers.  Loading these
;; files is side-effect free unless their explicit execute variables are set.
(load (expand-file-name "trace.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

;; These knobs are dynamically scoped for one isolated Runtime run.  Declare
;; them before the lexical function bodies; otherwise byte-compiling this
;; diagnostic turns the `let*' bindings below into unrelated lexical values
;; that the product modules cannot see.
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

;; `trace.el' loads the frozen setup helper at runtime.  Keep that loading
;; contract visible to the byte compiler instead of relying on load order.
(declare-function superchat-acceptance-v26--setup-experience "run.el" (cell))
(declare-function superchat-acceptance-v26--approval-read-only-only
                  "run.el" (request))
(declare-function superchat-acceptance-v26--jj-identity "run.el" (root))
(declare-function superchat-recovery-trace--transport-advice "trace.el"
                  (original method url &rest args))
(declare-function superchat-recovery-trace--llm-log-advice "trace.el"
                  (original type &rest args))
(declare-function superchat-recovery-trace--dependency-records "trace.el" ())
(declare-function superchat-recovery-trace--read "trace.el" (path))
(declare-function superchat-recovery-trace--write "trace.el" (path value))
(defvar superchat-acceptance-v26--repo-root)
(defvar superchat-acceptance-v26--llm-commit)
(defvar superchat-acceptance-v26--model)
(defvar superchat-acceptance-v26--system-prompt)

(defconst superchat-recovery-durable--cell
  '(:name "P-durable" :positive t :kind missing :stale nil))

(defvar superchat-recovery-durable--transport-count 0)
(defvar superchat-recovery-durable--transport-ceiling 4)

(defun superchat-recovery-durable--events (events type)
  "Return Tape EVENTS whose type is TYPE."
  (cl-remove-if-not (lambda (event)
                     (equal type (plist-get event :type)))
                   events))

(defun superchat-recovery-durable--completed-p (status)
  "Return non-nil when Runtime STATUS denotes completion."
  (or (eq status 'completed)
      (equal status "completed")))

(defun superchat-recovery-durable--payload (event)
  "Return EVENT's payload plist."
  (plist-get event :payload))

(defun superchat-recovery-durable--execution-id (event)
  "Return EVENT's Authority execution identity."
  (plist-get (superchat-recovery-durable--payload event) :execution-id))

(defun superchat-recovery-durable--arguments-json (event)
  "Return the exact named-argument JSON recorded for EVENT."
  (plist-get (superchat-recovery-durable--payload event) :arguments-json))

(defun superchat-recovery-durable--completed-for (events request)
  "Return the completion in EVENTS for REQUEST, if any."
  (let ((execution-id (superchat-recovery-durable--execution-id request)))
    (cl-find-if (lambda (event)
                  (and (equal "tool.completed" (plist-get event :type))
                       (equal execution-id
                              (superchat-recovery-durable--execution-id
                               event))))
                events)))

(defun superchat-recovery-durable--snapshot-for-attempt (events attempt)
  "Return the decision snapshot for ATTEMPT in EVENTS."
  (cl-find-if
   (lambda (event)
     (and (equal "decision.snapshot.recorded" (plist-get event :type))
          (= attempt (plist-get (superchat-recovery-durable--payload event)
                                :attempt))))
   events))

(defun superchat-recovery-durable--recovery-verdict (setup events)
  "Judge only the recovery action, independently of Run terminal status.

This is deliberately narrower than the full Run verdict: a successful
recovery action must remain visible even when the model later violates the
final-answer protocol."
  (let* ((requested
          (superchat-recovery-durable--events events "tool.requested"))
         (requested-names
          (mapcar (lambda (event)
                    (plist-get (superchat-recovery-durable--payload event)
                               :tool-name))
                  requested))
         (failed
          (superchat-recovery-durable--events events "tool.failed"))
         (completed
          (superchat-recovery-durable--events events "tool.completed"))
         (interventions
          (superchat-recovery-durable--events
           events "experience.intervention.recorded"))
         (first-request (nth 0 requested))
         (info-request (nth 1 requested))
         (corrected-request (nth 2 requested))
         (first-failure (car failed))
         (intervention (car interventions))
         (info (and info-request
                    (superchat-recovery-durable--completed-for
                     completed info-request)))
         (corrected-read (and corrected-request
                              (superchat-recovery-durable--completed-for
                               completed corrected-request)))
         (artifact-id (plist-get setup :artifact-id))
         (info-json (and info
                         (plist-get (superchat-recovery-durable--payload info)
                                    :result-json)))
         (info-revision
          (and info-json
               (string-match
                (format "artifact %s .* revision=\\([^ \\n]+\\)"
                        (regexp-quote artifact-id))
                info-json)
               (match-string 1 info-json)))
         (attempt-2
          (superchat-recovery-durable--snapshot-for-attempt events 2))
         (attempt-2-json
          (and attempt-2
               (plist-get (superchat-recovery-durable--payload attempt-2)
                          :prepared-envelope-json)))
         (candidate-event-id
          (and intervention
               (plist-get
                (plist-get
                 (plist-get (superchat-recovery-durable--payload intervention)
                            :receipt)
                 :candidate)
                :event-id)))
         (errors nil))
    (unless (equal requested-names
                   '("workspace_read" "workspace_info" "workspace_read"))
      (push (format "unexpected recovery tool sequence: %S" requested-names)
            errors))
    (unless (and (= 1 (length failed))
                 first-request
                 first-failure
                 (equal "workspace_read"
                        (plist-get (superchat-recovery-durable--payload
                                    first-failure)
                                   :tool-name))
                 (equal "superchat-workspace-artifact-revision-error"
                        (plist-get (superchat-recovery-durable--payload
                                    first-failure)
                                   :error-type)))
      (push "first recovery failure was not the typed revision error" errors))
    (unless (and (= 1 (length interventions))
                 intervention
                 first-failure
                 (equal (plist-get first-failure :event-id)
                        (plist-get intervention :causation-id))
                 (equal (plist-get first-failure :event-id)
                        (plist-get
                         (plist-get
                          (superchat-recovery-durable--payload intervention)
                          :trigger)
                         :event-id)))
      (push "recovery intervention was not caused by the typed failure"
            errors))
    (unless (and info
                 (equal "succeeded"
                        (plist-get (superchat-recovery-durable--payload info)
                                   :status)))
      (push "workspace_info did not complete successfully" errors))
    (unless (and corrected-read
                 (equal "succeeded"
                        (plist-get
                         (superchat-recovery-durable--payload corrected-read)
                         :status))
                 info-revision
                 (string-match-p
                  (format "\\\"expected_revision\\\":\\\"%s\\\""
                          (regexp-quote info-revision))
                  (or (superchat-recovery-durable--arguments-json
                       corrected-request)
                      "")))
      (push "corrected workspace_read did not use the reported revision"
            errors))
    (unless (and attempt-2
                 (stringp attempt-2-json)
                 (string-match-p
                  "\\\"attempt-reason\\\":\\\"tool-followup\\\""
                  attempt-2-json)
                 (string-match-p
                  "\\\"experience-view\\\":{\\\"contract\\\":\\\"experience-view-v0\\\",\\\"status\\\":\\\"satisfied\\\""
                  attempt-2-json)
                 (or (null candidate-event-id)
                     (string-match-p (regexp-quote candidate-event-id)
                                     attempt-2-json)))
      (push "tool-followup envelope did not contain the satisfied Experience View"
            errors))
    (list :passed (null errors)
          :errors (nreverse errors)
          :first-recovery-action (nth 1 requested-names)
          :tool-requested requested-names
          :typed-failure (and first-failure t)
          :intervention-recorded (and intervention t)
          :experience-injected (and attempt-2-json
                                    (string-match-p
                                     "\\\"experience-view\\\":{\\\"contract\\\":\\\"experience-view-v0\\\",\\\"status\\\":\\\"satisfied\\\""
                                     attempt-2-json)
                                    t)
          :corrected-read-succeeded
          (and corrected-read
               (equal "succeeded"
                      (plist-get
                       (superchat-recovery-durable--payload corrected-read)
                       :status))))))

(defun superchat-recovery-durable--setup (cell)
  "Install CELL's Experience setup with a durable generated-file Artifact."
  ;; The frozen helper owns the claim/evaluation/relation setup.  Replace only
  ;; its producer adapter; the product Workspace seam remains the code under
  ;; test.
  (cl-letf (((symbol-function 'superchat-workspace-add-region)
             (lambda (_beg _end &optional role workspace-id)
               (superchat-workspace-publish
                "sealed producer artifact\n" role workspace-id))))
    (superchat-acceptance-v26--setup-experience cell)))

(defun superchat-recovery-durable--transport-advice
    (original method url &rest args)
  "Capture and cap durable diagnostic provider requests."
  (when (and (eq method 'post)
             (string-suffix-p "/api/chat" url))
    (cl-incf superchat-recovery-durable--transport-count)
    (when (> superchat-recovery-durable--transport-count
             superchat-recovery-durable--transport-ceiling)
      (error "Durable diagnostic provider request ceiling exceeded")))
  (apply #'superchat-recovery-trace--transport-advice
         original method url args))

(defun superchat-recovery-durable--verify
    (setup events transport-count status &optional initial-revision)
  "Verify the complete durable recovery sequence in EVENTS.
When INITIAL-REVISION is non-nil, the first read must carry that stale
revision; otherwise it must omit `expected_revision'."
  (let* ((requested
          (superchat-recovery-durable--events events "tool.requested"))
         (requested-names
          (mapcar (lambda (event)
                    (plist-get (superchat-recovery-durable--payload event)
                               :tool-name))
                  requested))
         (failed
          (superchat-recovery-durable--events events "tool.failed"))
         (completed
          (superchat-recovery-durable--events events "tool.completed"))
         (interventions
          (superchat-recovery-durable--events
           events "experience.intervention.recorded"))
         (terminal-events
          (superchat-recovery-durable--events
           events "run.completed"))
         (terminal (car (last terminal-events)))
         (first-request (nth 0 requested))
         (info-request (nth 1 requested))
         (corrected-request (nth 2 requested))
         (first-failure (car failed))
         (intervention (car interventions))
         (info (and info-request
                    (superchat-recovery-durable--completed-for
                     completed info-request)))
         (corrected-read (and corrected-request
                              (superchat-recovery-durable--completed-for
                               completed corrected-request)))
         (artifact-id (plist-get setup :artifact-id))
         (info-json (and info
                         (plist-get (superchat-recovery-durable--payload info)
                                    :result-json)))
         (info-revision
          (and info-json
               (string-match
                (format "artifact %s .* revision=\\([^ \\n]+\\)"
                        (regexp-quote artifact-id))
                info-json)
               (match-string 1 info-json)))
         (errors nil))
    (unless (= transport-count 4)
      (push (format "expected 4 provider requests, got %s" transport-count)
            errors))
    (unless (equal requested-names '("workspace_read" "workspace_info"
                                     "workspace_read"))
      (push (format "unexpected tool sequence: %S" requested-names) errors))
    (unless (and (stringp (superchat-recovery-durable--execution-id
                          first-request))
                 (stringp (superchat-recovery-durable--execution-id
                           info-request))
                 (stringp (superchat-recovery-durable--execution-id
                           corrected-request)))
      (push "tool requests are missing Authority execution identities" errors))
    (unless (and (memq (plist-get (plist-get setup :artifact) :adapter)
                        '(generated-file durable-file))
                 first-request
                 (string-match-p
                  (format "\\\"named-arguments\\\":{\\\"artifact_id\\\":\\\"%s\\\""
                          (regexp-quote artifact-id))
                  (or (superchat-recovery-durable--arguments-json
                       first-request)
                      ""))
                 (if initial-revision
                     (string-match-p
                      (format "\\\"expected_revision\\\":\\\"%s\\\""
                              (regexp-quote initial-revision))
                      (or (superchat-recovery-durable--arguments-json
                           first-request)
                          ""))
                   (not (string-match-p "\\\"expected_revision\\\":"
                                        (or (superchat-recovery-durable--arguments-json
                                             first-request)
                                            "")))))
      (push "initial read did not match the file-adaptor revision contract"
            errors))
    (unless (and info-request
                 (string-match-p "\\\"named-arguments\\\":null"
                                 (or (superchat-recovery-durable--arguments-json
                                      info-request)
                                     ""))
                 (string-match-p "\\\"positional-arguments\\\":\\[\\]"
                                 (or (superchat-recovery-durable--arguments-json
                                      info-request)
                                     "")))
      (push "workspace_info was not called with empty arguments" errors))
    (unless (and (= 1 (length failed))
                 first-failure
                 (equal (superchat-recovery-durable--execution-id first-request)
                        (superchat-recovery-durable--execution-id first-failure))
                 (equal "superchat-workspace-artifact-revision-error"
                        (plist-get (superchat-recovery-durable--payload first-failure)
                                   :error-type)))
      (push "missing typed initial revision failure" errors))
    (unless (and (= 1 (length interventions))
                 intervention
                 (equal (plist-get first-failure :event-id)
                        (plist-get intervention :causation-id))
                 (equal (plist-get first-failure :event-id)
                        (plist-get (plist-get
                                    (superchat-recovery-durable--payload
                                     intervention)
                                    :trigger)
                                   :event-id)))
      (push "Experience intervention was not caused by the initial failure"
            errors))
    (unless (and (stringp info-json)
                 info-revision
                 (string-match-p (format "artifact %s "
                                         (regexp-quote artifact-id))
                                 info-json)
                 (string-match-p "live=t" info-json))
      (push "workspace_info did not expose a live durable Artifact" errors))
    (unless (and corrected-request
                 info-revision
                 (string-match-p
                  (format "\\\"artifact_id\\\":\\\"%s\\\""
                          (regexp-quote artifact-id))
                  (or (superchat-recovery-durable--arguments-json
                       corrected-request)
                      ""))
                 (string-match-p
                  (format "\\\"expected_revision\\\":\\\"%s\\\""
                          (regexp-quote info-revision))
                  (or (superchat-recovery-durable--arguments-json
                       corrected-request)
                      ""))
                 corrected-read
                 (equal "succeeded"
                        (plist-get (superchat-recovery-durable--payload
                                    corrected-read)
                                   :status)))
      (push "corrected workspace_read did not use the reported revision" errors))
    (unless (and (= 1 (length terminal-events))
                 (null (superchat-recovery-durable--events events "run.failed"))
                 (null (superchat-recovery-durable--events
                        events "response.failed"))
                 terminal
                 (equal "completed"
                        (plist-get (plist-get terminal :payload) :status)))
      (push "Runtime terminal was not exactly one successful run.completed"
            errors))
    (unless (superchat-recovery-durable--completed-p status)
      (push (format "Runtime status is %S" status) errors))
    (list :passed (null errors)
          :errors (nreverse errors)
          ;; Keep the narrow behavioral result separate from the full Run
          ;; terminal verdict.  A model can recover correctly and still fail
          ;; later on final-answer protocol.
          :recovery-verdict
          (superchat-recovery-durable--recovery-verdict setup events)
          :tool-requested requested-names
          :transport-count transport-count
          :terminal (and terminal
                         (list :type (plist-get terminal :type)
                               :status (plist-get (plist-get terminal :payload)
                                                  :status)))
          :artifact-adapter (plist-get (plist-get setup :artifact) :adapter)
          :initial-revision initial-revision
          :info-result-json info-json)))

(defun superchat-recovery-durable--run-cell
    (setup directory &optional cell max-tool-rounds request-ceiling
           round-limit-action verifier)
  "Run SETUP once and write its diagnostic evidence under DIRECTORY.
CELL, MAX-TOOL-ROUNDS, REQUEST-CEILING, ROUND-LIMIT-ACTION, and VERIFIER are
diagnostic-only overrides used by the seven-cell durable cohort."
  (let* ((cell (or cell superchat-recovery-durable--cell))
         (name (plist-get cell :name))
         (session (format "consumer-%s" name))
         (workspace (plist-get setup :workspace-id))
         (buffer-name (format " *recovery-consumer-%s*" name))
         (prompt
          (if (plist-get cell :stale)
              (format
               "Your first tool call must be workspace_read, exactly once, for Artifact %s with expected_revision %s. Do not call workspace_info or any other tool before it. This is a read-only task: never call workspace_write, EditBuffer, ReplaceBuffer, or eshell-command. If it fails, call workspace_info for the same artifact, then call workspace_read with the exact revision it reports. Do not answer before the corrected read succeeds."
               (plist-get setup :artifact-id)
               (plist-get setup :old-revision))
            (format
             "Your first tool call must be workspace_read, exactly once, for Artifact %s without expected_revision. Do not call workspace_info or any other tool before it. This is a read-only task: never call workspace_write, EditBuffer, ReplaceBuffer, or eshell-command. If it fails, call workspace_info for the same artifact, then call workspace_read with the exact revision it reports. Do not answer before the corrected read succeeds."
             (plist-get setup :artifact-id))))
         (handle nil)
         (events nil)
         (status nil)
         (failure nil)
         (result nil)
         (superchat-recovery-durable--transport-count 0)
         (superchat-recovery-durable--transport-ceiling
          (or request-ceiling 4))
         (superchat-data-directory directory)
         (superchat-workspace--registry nil)
         (superchat-workspace--artifact-counter 0)
         (superchat-runtime-recovery-intervention-enabled t)
         (superchat-response-timeout 90)
         (superchat-tool-timeout-multiplier 1.0)
         (superchat-llm-reasoning 'none)
         (superchat-llm-max-tool-rounds (or max-tool-rounds 3))
         (superchat-llm-round-limit-action (or round-limit-action 'finalize))
         (superchat-llm-unknown-tool-retries 0)
         (superchat-agent-max-tool-calls (or request-ceiling 4))
         (superchat-agent-confirm-destructive t)
         (superchat-lang "English")
         (superchat-llm-streaming t)
         (superchat-llm-tools-enabled t)
         (provider (make-llm-ollama
                    :chat-model superchat-acceptance-v26--model
                    :host "localhost" :port 11434))
         (superchat-llm-backend provider))
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create buffer-name)
            (let ((superchat--session-id session))
              (superchat-workspace-bind workspace nil session)))
          (advice-add 'llm--log :around
                      #'superchat-recovery-trace--llm-log-advice)
          (advice-add 'plz-media-type-request :around
                      #'superchat-recovery-durable--transport-advice)
          (unwind-protect
              (condition-case err
                  (progn
                    (setq handle
                          (superchat-runtime-submit
                           (list :prompt prompt :input prompt :clean-input prompt
                                 :session-id session :workspace-id workspace
                                 :buffer-name buffer-name
                                 :preset (superchat--builtin-capability-preset "code")
                                 :capability "code"
                                 :target-model superchat-acceptance-v26--model
                                 :tools '("workspace_read" "workspace_info")
                                 :system-prompt superchat-acceptance-v26--system-prompt
                                 :authority-policy "default" :attendance 'attended
                                 :approval-function
                                 #'superchat-acceptance-v26--approval-read-only-only
                                 :round-limit-function
                                 (lambda () (or round-limit-action 'finalize))
                                 :unknown-tool-retries 0 :record-conversation nil
                                 :evidence-contract "epistemic-learning-v0"
                                 :context-envelope
                                 (list :contract "context-continuity-v0"
                                       :status 'empty
                                       :receipt '(:tape-head 0)
                                       :disclosure '(:represented [])
                                       :experience-session-ids
                                       (append (plist-get setup :sessions)
                                               nil)))))
                    (let ((deadline (+ (float-time) 150)))
                      (while (and handle
                                  (< (float-time) deadline)
                                  (not (memq
                                        (plist-get
                                         (superchat-runtime-status
                                          (plist-get handle :run-id))
                                         :status)
                                        '(completed failed cancelled timed-out))))
                        (accept-process-output nil 0.1)))
                    (when handle
                      (setq status
                            (plist-get
                             (superchat-runtime-status
                              (plist-get handle :run-id))
                             :status))
                      (when (memq status '(accepted running-model delivering))
                        (superchat-runtime-cancel
                         (plist-get handle :run-id) "Durable diagnostic timeout"))))
                (error
                 (setq failure (error-message-string err))))
            (advice-remove 'llm--log
                           #'superchat-recovery-trace--llm-log-advice)
            (advice-remove 'plz-media-type-request
                           #'superchat-recovery-durable--transport-advice))
          (setq events (ignore-errors
                         (superchat-tape-replay-events session)))
          (setq status
                (or status
                    (and handle
                         (plist-get
                          (superchat-runtime-status (plist-get handle :run-id))
                          :status))))
          (setq result
                (condition-case err
                    (if verifier
                        (funcall verifier
                                 cell setup events
                                 superchat-recovery-durable--transport-count
                                 status)
                      (superchat-recovery-durable--verify
                       setup events superchat-recovery-durable--transport-count
                       status (and (plist-get cell :stale)
                                   (plist-get setup :old-revision))))
                  (error
                   (list :passed nil
                         :errors (list (error-message-string err)))))))
      (ignore-errors (superchat-db-close)))
    (superchat-recovery-trace--write
     (expand-file-name "result.el" directory)
     (list :cell cell
           :setup setup
           :handle handle
           :events events
           :transport-count superchat-recovery-durable--transport-count
           :runtime-status status
           :verdict result
           :failure failure))
    (list :result result :events events :status status :failure failure)))

(defun superchat-recovery-durable--run ()
  "Run the durable one-cell diagnostic and retain a complete trace."
  (let* ((output (getenv "SUPERCHAT_DURABLE_TRACE_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (cell-directory (and root (expand-file-name "P-durable" root)))
         setup run-result result-record events terminal status)
    (unless (and output root)
      (error "SUPERCHAT_DURABLE_TRACE_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite existing trace: %s" output))
    (make-directory cell-directory t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'superchat-workspace)
    (require 'llm)
    (require 'llm-ollama)
    (let ((superchat-data-directory cell-directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0))
      (superchat-db-close)
      (superchat-db-open)
      (unwind-protect
          (setq setup (superchat-recovery-durable--setup
                       superchat-recovery-durable--cell))
        (superchat-db-close)))
    (setq superchat-recovery-trace--llm-log nil
          superchat-recovery-trace--transport nil)
    (setq run-result
          (superchat-recovery-durable--run-cell setup cell-directory))
    (setq result-record
          (superchat-recovery-trace--read
           (expand-file-name "result.el" cell-directory)))
    (setq events (plist-get result-record :events)
          terminal (car (last (cl-remove-if-not
                               (lambda (event)
                                 (equal "run.completed" (plist-get event :type)))
                               events)))
          status (plist-get result-record :runtime-status))
    (superchat-recovery-trace--write
     output
     (list :format "superchat-experience-view-durable-recovery-trace-v0"
           :cell superchat-recovery-durable--cell
           :source
           (list :repo-root-role "<repo-root>"
                 :jj (superchat-acceptance-v26--jj-identity
                      superchat-acceptance-v26--repo-root)
                 :emacs emacs-version
                 :llm-commit superchat-acceptance-v26--llm-commit
                 :model superchat-acceptance-v26--model
                 :dependencies (superchat-recovery-trace--dependency-records))
           :setup setup
           :result result-record
           :summary run-result
           :runtime-status status
           :transport-requests (nreverse superchat-recovery-trace--transport)
           :llm-log (nreverse superchat-recovery-trace--llm-log)
           :tape-events events
           :notes
           '(:transport-body "Exact JSON request body captured in :transport-requests."
             :llm-response "Decoded llm log events captured; raw HTTP chunks and partial-callback counts are not captured."
             :tape "Runtime, Authority, Experience, and terminal events captured in :tape-events."
             :artifact "Cross-Run cell uses generated-file; buffer-region remains process-local."
             :redaction "Authorization headers are redacted; endpoint is local Ollama.")))
    (kill-emacs (if (and (plist-get (plist-get run-result :result) :passed)
                         (superchat-recovery-durable--completed-p status)
                         terminal)
                    0 1))))

(when (equal (getenv "SUPERCHAT_DURABLE_TRACE_EXECUTE") "1")
  (superchat-recovery-durable--run))

(provide 'superchat-experience-view-durable-recovery-trace)

;;; durable-trace.el ends here
