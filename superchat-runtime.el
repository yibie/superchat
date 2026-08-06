;;; superchat-runtime.el --- Main Turn/Run lifecycle for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Runtime owns main Chat/Agent execution.  Callers submit a prepared request;
;; the mutable Turn, provider request, lifecycle events, and delivery remain
;; behind `superchat-runtime-submit'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-core)
(require 'superchat-preset)
(require 'superchat-tape)
(require 'superchat-closure)
(require 'superchat-authority)

(declare-function llm-cancel-request "llm" (request))
(declare-function superchat--llm-generate-answer
                  "superchat-llm"
                  (prompt callback stream-callback &optional target-model
                          context-files tools system-prompt preset))
(declare-function superchat--stream-llm-result "superchat-render" (chunk))
(declare-function superchat--process-llm-result
                  "superchat-render" (answer &optional tape-id))
(declare-function superchat--update-status "superchat-render" (message))

(defvar superchat-buffer-name)
(defvar superchat--active-timeout-timer)
(defvar superchat--last-surface)
(defvar superchat-tools-default-directory)
(defvar superchat--llm-runtime-observer nil
  "Internal observer bound by Runtime around one transport invocation.")
(defvar superchat--llm-tools-prepared-p nil)
(defvar superchat--llm-prepared-tools nil)
(defvar superchat--llm-tool-wrapper nil)
(defvar superchat--llm-round-limit-function nil)
(defvar superchat--llm-unknown-tool-retry-limit nil)
(defvar superchat--llm-persist-synthesized-tools t)
(defvar superchat--llm-run-active-p nil)
(defvar superchat--tool-execution-context)

(defvar superchat-pre-topic-finalized-functions nil
  "Hook run with a Turn immediately before Runtime completes it.")

(defvar superchat-post-turn-functions nil
  "Hook run with a Turn after its response and before completion.")

(defvar superchat-post-topic-finalized-functions nil
  "Hook run with a Turn after Runtime finalization side effects.")

(defvar superchat-run-completed-functions nil
  "Hook run with a public Run status plist after successful completion.")

(defvar superchat-turn-completed-functions nil
  "Hook run with a public Run status plist after its Turn completes.")

(defvar superchat-closure-shadow-enabled nil
  "When non-nil, compare terminal Runtime state with Closure judgments.")

(defvar superchat-closure-terminal-guard-enabled t
  "When non-nil, require Closure readiness before a Run terminal seal.")

(defvar superchat-closure-shadow-functions nil
  "Hook run with each Closure shadow comparison report.
Reports are diagnostic only and never participate in Runtime transitions.")

(define-error 'superchat-closure-terminal-not-ready
  "Closure terminal guard rejected transition")

(defconst superchat-runtime--terminal-statuses
  '(completed failed cancelled timed-out audit-incomplete)
  "States after which a Run ignores further transport callbacks.")

(defvar superchat-runtime--runs (make-hash-table :test #'equal)
  "Process-local Run registry keyed by Run ID.")

(defconst superchat-runtime--closure-observation-limit 100
  "Maximum number of process-local Closure observations retained.")

(defvar superchat-runtime--closure-observations nil
  "Newest-first process-local Closure diagnostic reports.")

(defun superchat-runtime--closure-anomaly-p (observation)
  "Return non-nil when OBSERVATION is not a successful Closure result."
  (pcase (plist-get observation :source)
    ('shadow (not (eq 'agreed (plist-get observation :agreement))))
    ('guard (not (eq 'allowed (plist-get observation :decision))))
    (_ t)))

(defun superchat-runtime--record-closure-observation (report)
  "Retain and return one timestamped Closure REPORT."
  (let ((observation
         (append (list :observed-at (float-time)) (copy-tree report))))
    (push observation superchat-runtime--closure-observations)
    (when (> (length superchat-runtime--closure-observations)
             superchat-runtime--closure-observation-limit)
      (setcdr
       (nthcdr (1- superchat-runtime--closure-observation-limit)
               superchat-runtime--closure-observations)
       nil))
    observation))

(defun superchat-runtime-closure-observations (&optional anomalies-only)
  "Return retained Closure observations, newest first.
When ANOMALIES-ONLY is non-nil, omit agreed Shadow and allowed Guard reports.
The returned data is copied and may be mutated by the caller."
  (copy-tree
   (if anomalies-only
       (cl-remove-if-not #'superchat-runtime--closure-anomaly-p
                         superchat-runtime--closure-observations)
     superchat-runtime--closure-observations)))

(defun superchat-runtime-clear-closure-observations ()
  "Clear process-local Closure observations."
  (interactive)
  (setq superchat-runtime--closure-observations nil))

(defun superchat-runtime--new-id (prefix)
  "Return a compact identifier beginning with PREFIX."
  (format "%s-%s-%04x"
          prefix (format-time-string "%Y%m%d-%H%M%S-%6N") (random 65536)))

(defun superchat-runtime--terminal-p (status)
  "Return non-nil when STATUS is terminal."
  (memq status superchat-runtime--terminal-statuses))

(defun superchat-runtime--entry (run-id)
  "Return the private registry entry for RUN-ID."
  (gethash run-id superchat-runtime--runs))

(defun superchat-runtime--put (entry)
  "Store and return private Run ENTRY."
  (puthash (plist-get entry :run-id) entry superchat-runtime--runs)
  entry)

(defun superchat-runtime--update (run-id &rest properties)
  "Update RUN-ID with alternating PROPERTIES and return the entry."
  (let ((entry (superchat-runtime--entry run-id)))
    (unless entry
      (error "Unknown Superchat run: %s" run-id))
    (while properties
      (setq entry (plist-put entry (pop properties) (pop properties))))
    (superchat-runtime--put entry)))

(defun superchat-runtime--public-status (entry)
  "Return the caller-visible subset of private Run ENTRY."
  (when entry
    (let (status)
      (dolist (key '(:session-id :turn-id :run-id :actor-id :profile-id
                     :parent-run-id :workspace-id :correlation-id
                     :capability :topology :authority-policy :surface
                     :status :started-at :completed-at :error))
        (when (plist-member entry key)
          (setq status
                (plist-put status key (plist-get entry key)))))
      status)))

(defun superchat-runtime-status (run-id)
  "Return public status for RUN-ID, or nil when it is unknown."
  (superchat-runtime--public-status (superchat-runtime--entry run-id)))

(defun superchat-runtime--event (entry type payload summary causation-id)
  "Build a Runtime event for ENTRY of TYPE with PAYLOAD and SUMMARY."
  (let ((event
         (list :type type
               :session-id (plist-get entry :session-id)
               :turn-id (plist-get entry :turn-id)
               :run-id (plist-get entry :run-id)
               :actor-id (plist-get entry :actor-id)
               :correlation-id (plist-get entry :correlation-id)
               :summary summary
               :payload payload)))
    (when (plist-get entry :profile-id)
      (setq event
            (plist-put event :profile-id (plist-get entry :profile-id))))
    (when (plist-get entry :parent-run-id)
      (setq event
            (plist-put event :parent-run-id
                       (plist-get entry :parent-run-id))))
    (when causation-id
      (setq event (plist-put event :causation-id causation-id)))
    (when (plist-get entry :workspace-id)
      (setq event
            (plist-put event :workspace-id (plist-get entry :workspace-id))))
    event))

(defun superchat-runtime--append-event
    (entry type payload summary &optional causation-id)
  "Append a Runtime event for ENTRY and return its identity record."
  (superchat-tape-append-event-record
   (superchat-runtime--event
    entry type payload summary
    (or causation-id (plist-get entry :causation-id)))))

(defun superchat-runtime--content-metadata (content)
  "Return non-sensitive identity metadata for CONTENT."
  (let* ((text (or content ""))
         (encoded (encode-coding-string text 'utf-8)))
    (list :content-size (string-bytes encoded)
          :content-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-runtime--name (value fallback)
  "Return VALUE as a non-empty string, or FALLBACK."
  (let ((name (cond ((stringp value) value)
                    ((and value (symbolp value)) (symbol-name value))
                    (t nil))))
    (if (and name (not (string-empty-p name))) name fallback)))

(defun superchat-runtime--surface (request)
  "Return the request-local Surface identifier for REQUEST."
  (let ((origin (plist-get request :origin)))
    (or (plist-get request :surface)
        (and (listp origin) (plist-get origin :surface))
        (and (listp origin) (plist-get origin :major-mode))
        'superchat)))

(defun superchat-runtime--turn-from-request (request session-id)
  "Create the private execution Turn for prepared REQUEST in SESSION-ID."
  (let* ((input (or (plist-get request :input)
                    (plist-get request :user-message)
                    (plist-get request :prompt)
                    ""))
         (turn (superchat-turn-new input session-id)))
    (setf (superchat-turn-origin turn) (plist-get request :origin)
          (superchat-turn-clean-input turn)
          (or (plist-get request :clean-input)
              (plist-get request :user-message)
              input)
          (superchat-turn-target-model turn) (plist-get request :target-model)
          (superchat-turn-context-files turn) (plist-get request :context-files)
          (superchat-turn-system-prompt turn)
          (or (plist-get request :system-prompt) "")
          (superchat-turn-prompt turn) (or (plist-get request :prompt) "")
          (superchat-turn-tools turn) (plist-get request :tools)
          (superchat-turn-preset turn) (plist-get request :preset))
    turn))

(defun superchat-runtime--run-completion-hooks (entry)
  "Run terminal hooks for ENTRY.
Run hooks remain success-only.  Turn hooks observe every terminal status."
  (let ((status (superchat-runtime--public-status entry))
        (hooks (append
                (when (eq (plist-get entry :status) 'completed)
                  superchat-run-completed-functions)
                superchat-turn-completed-functions)))
    (dolist (fn hooks)
      (when (functionp fn)
        (ignore-errors (funcall fn status))))))

(defun superchat-runtime--guard-terminal (entry status)
  "Require a proved Closure readiness judgment for ENTRY and STATUS.
Return the judgment when the Guard is enabled and nil otherwise."
  (when superchat-closure-terminal-guard-enabled
    ;; ponytail: the opt-in Guard reuses canonical Session replay; add a
    ;; Run-scoped index only after this O(n) path is measurably too slow.
    (let* ((base
            (list :source 'guard
                  :session-id (plist-get entry :session-id)
                  :turn-id (plist-get entry :turn-id)
                  :run-id (plist-get entry :run-id)
                  :runtime-terminal status))
           (report
            (condition-case err
                (let* ((judgment
                        (superchat-closure-judge
                         (list :kind 'ready-terminal
                               :run-id (plist-get entry :run-id)
                               :terminal-kind status)
                         (superchat-tape-replay-events
                          (plist-get entry :session-id))))
                       (decision
                        (if (and
                             (eq 'proved (plist-get judgment :verdict))
                             (eq status
                                 (plist-get
                                  (plist-get judgment :judgment)
                                  :terminal-kind)))
                            'allowed
                          (if (eq 'unknown (plist-get judgment :verdict))
                              'unknown
                            'rejected))))
                  (append base
                          (list :decision decision :judgment judgment)))
              (error
               (append base
                       (list :decision 'unknown
                             :error (error-message-string err)))))))
      (superchat-runtime--record-closure-observation report)
      (unless (eq 'allowed (plist-get report :decision))
        (signal 'superchat-closure-terminal-not-ready
                (list status report)))
      (plist-get report :judgment))))

(defun superchat-runtime--run-closure-shadow (entry)
  "Compare terminal ENTRY with its Tape-derived Closure judgments."
  (when superchat-closure-shadow-enabled
    ;; ponytail: Replay one Session at terminal time while shadow mode is
    ;; experimental; add a Run-scoped index only after measured need.
    (let* ((status (plist-get entry :status))
           (run-id (plist-get entry :run-id))
           (report
            (condition-case err
                (let* ((trace
                        (superchat-tape-replay-events
                         (plist-get entry :session-id)))
                       (run-judgment
                        (superchat-closure-judge
                         (list :kind 'durable-run-terminal :run-id run-id)
                         trace))
                       (turn-judgment
                        (superchat-closure-judge
                         (list :kind 'durable-turn-terminal
                               :run-id run-id
                               :turn-id (plist-get entry :turn-id))
                         trace))
                       (run-verdict (plist-get run-judgment :verdict))
                       (turn-verdict (plist-get turn-judgment :verdict))
                       (agreement
                        (cond
                         ((and (eq run-verdict 'proved)
                               (eq turn-verdict 'proved)
                               (eq status
                                   (plist-get
                                    (plist-get run-judgment :judgment)
                                    :terminal-kind))
                               (eq status
                                   (plist-get
                                    (plist-get turn-judgment :judgment)
                                    :terminal-kind)))
                          'agreed)
                         ((or (eq run-verdict 'unknown)
                              (eq turn-verdict 'unknown))
                          'unknown)
                         (t 'disagreed))))
                  (list :agreement agreement
                        :run-id run-id
                        :runtime-terminal status
                        :run-judgment run-judgment
                        :turn-judgment turn-judgment))
              (error
               (list :agreement 'unknown
                     :run-id run-id
                     :runtime-terminal status
                     :error (error-message-string err))))))
      (setq report
            (append
             (list :source 'shadow
                   :session-id (plist-get entry :session-id)
                   :turn-id (plist-get entry :turn-id))
             report))
      (superchat-runtime--record-closure-observation report)
      (unless (eq 'agreed (plist-get report :agreement))
        (ignore-errors
          (display-warning
           'superchat-runtime
           (format "Closure shadow %s for Run %s (Runtime: %s)"
                   (plist-get report :agreement) run-id status)
           :warning)))
      (dolist (fn superchat-closure-shadow-functions)
        (when (functionp fn)
          (ignore-errors (funcall fn report)))))))

(defun superchat-runtime--deliver (entry response tape-id &optional status)
  "Deliver RESPONSE for ENTRY with STATUS and reusable TAPE-ID."
  (if-let* ((delivery (plist-get entry :delivery-function)))
      (funcall delivery response (or status (plist-get entry :status)) tape-id)
    (let ((superchat-buffer-name (plist-get entry :buffer-name)))
      (when (fboundp 'superchat--process-llm-result)
        (superchat--process-llm-result response tape-id)))))

(defun superchat-runtime--release (entry)
  "Release callbacks, provider handles, and ephemeral buffer from ENTRY."
  (when-let* (((plist-get entry :ephemeral-buffer))
              (buffer (get-buffer (plist-get entry :buffer-name))))
    (kill-buffer buffer))
  (superchat-runtime--update
   (plist-get entry :run-id)
   :delivery-function nil
   :stream-function nil
   :transport-request nil
   :ephemeral-buffer nil))

(defun superchat-runtime--run-turn-hooks (entry)
  "Run post-response Turn hooks for ENTRY exactly once."
  (let* ((turn (plist-get entry :turn))
         (name (plist-get entry :buffer-name))
         (buffer (get-buffer name))
         (superchat-buffer-name name))
    (unless (plist-get entry :turn-hooks-run)
      (cl-labels
          ((run-hooks ()
             (dolist (fn superchat-pre-topic-finalized-functions)
               (when (functionp fn)
                 (ignore-errors (funcall fn turn))))
             (dolist (fn superchat-post-turn-functions)
               (when (functionp fn)
                 (ignore-errors (funcall fn turn))))
             (dolist (fn superchat-post-topic-finalized-functions)
               (when (functionp fn)
                 (ignore-errors (funcall fn turn))))))
        (if (buffer-live-p buffer)
            (with-current-buffer buffer (run-hooks))
          (run-hooks)))
      (setq entry
            (superchat-runtime--update
             (plist-get entry :run-id) :turn-hooks-run t)))
    entry))

(defun superchat-runtime--complete-turn
    (entry status causation-id &optional unaudited)
  "Complete ENTRY's Turn once with STATUS caused by CAUSATION-ID.
When UNAUDITED is non-nil, mark only the in-memory lifecycle because Tape
persistence has already failed; do not invent a `turn.completed' event."
  (setq entry (superchat-runtime--run-turn-hooks entry))
  (unless (plist-get entry :turn-completed)
    (if unaudited
        (setq entry
              (superchat-runtime--update
               (plist-get entry :run-id) :turn-completed t))
      (let* ((record
              (superchat-runtime--append-event
               entry "turn.completed" (list :status status)
               (format "turn.completed: %s" (plist-get entry :turn-id))
               causation-id))
             (event-id (plist-get record :event-id)))
        (setq entry
              (superchat-runtime--update
               (plist-get entry :run-id)
               :turn-completed t
               :causation-id event-id)))))
  entry)

(defun superchat-runtime--audit-incomplete (run-id err response)
  "Mark RUN-ID audit-incomplete after ERR and show RESPONSE with a warning."
  (let* ((current (superchat-runtime--entry run-id))
         (request (plist-get current :transport-request))
         (_ (superchat-runtime--cancel-timeout current))
         (recovery-error
          (condition-case queue-error
              (progn
                (superchat-tape-queue-recovery-events
                 (list
                  (superchat-runtime--event
                   current "run.failed"
                   '(:status audit-incomplete :audit-incomplete t)
                   (format "run.failed: %s (audit incomplete)" run-id)
                   (plist-get current :causation-id))
                  (superchat-runtime--event
                   current "turn.completed"
                   '(:status audit-incomplete :audit-incomplete t)
                   (format "turn.completed: %s" (plist-get current :turn-id))
                   nil)))
                nil)
            (error (error-message-string queue-error))))
         (error-text
          (if recovery-error
              (format "%s; recovery journal failed: %s"
                      (error-message-string err) recovery-error)
            (error-message-string err)))
         (entry (superchat-runtime--update
                 run-id
                 :status 'audit-incomplete
                 :completed-at (float-time)
                 :error error-text))
         (warning (format "%s\n\n[Audit incomplete: %s]"
                          (or response "")
                          error-text)))
    (unwind-protect
        (progn
          (setq entry
                (superchat-runtime--complete-turn
                 entry 'audit-incomplete nil t))
          (ignore-errors
            (superchat-runtime--deliver
             entry warning nil 'audit-incomplete))
          (superchat-runtime--run-completion-hooks entry))
      (superchat-runtime--release entry))
    ;; Commit local closure before asking a provider to stop.  Some providers
    ;; invoke callbacks synchronously from cancellation.
    (when (and request (fboundp 'llm-cancel-request))
      (ignore-errors (llm-cancel-request request)))))

(defun superchat-runtime--terminal-event-type (status)
  "Return the Run terminal event type for STATUS."
  (pcase status
    ('completed "run.completed")
    ('cancelled "run.cancelled")
    (_ "run.failed")))

(defun superchat-runtime--finish (run-id status response)
  "Finish RUN-ID once with terminal STATUS and RESPONSE."
  (let ((entry (superchat-runtime--entry run-id)))
    (when (and entry
               (not (superchat-runtime--terminal-p
                     (plist-get entry :status))))
      (condition-case err
          (let* ((cancelled-p (eq status 'cancelled))
                 (entry (superchat-runtime--update
                         run-id :status 'delivering))
                 (response-record
                  (unless cancelled-p
                    (superchat-runtime--append-event
                     entry "response.completed"
                     (append (list :status status)
                             (superchat-runtime--content-metadata response))
                     (or response "") (plist-get entry :causation-id))))
                 (response-id
                  (or (plist-get response-record :event-id)
                      (plist-get entry :causation-id)))
                 (response-storage-id
                  (plist-get response-record :storage-id))
                 (_ (setf (superchat-turn-llm-result
                           (plist-get entry :turn))
                          (or response "")))
                 (_ (when (memq status '(failed timed-out))
                      (setf (superchat-turn-error
                             (plist-get entry :turn))
                            response)))
                 (terminal-type
                  (superchat-runtime--terminal-event-type status))
                 (cancel-record
                  (when cancelled-p
                    (superchat-runtime--guard-terminal entry status)
                    (superchat-runtime--append-event
                     entry terminal-type (list :status status)
                     (format "%s: %s" terminal-type run-id)
                     response-id)))
                 (entry (superchat-runtime--run-turn-hooks entry))
                 (terminal-record
                  (or cancel-record
                      (progn
                        (superchat-runtime--guard-terminal entry status)
                        (superchat-runtime--append-event
                         entry terminal-type
                         (list :status status)
                         (format "%s: %s" terminal-type run-id)
                         response-id))))
                 (terminal-id (plist-get terminal-record :event-id))
                 (entry
                  (superchat-runtime--update
                   run-id
                   :status status
                   :causation-id terminal-id
                   :completed-at (float-time)))
                 (entry
                  (superchat-runtime--complete-turn
                   entry status terminal-id)))
            ;; Delivery is a consumer handoff: it may observe, but must never
            ;; precede or undo the durable terminal facts it consumes.
            (condition-case delivery-error
                (superchat-runtime--deliver
                 entry response
                 (or response-storage-id
                     (plist-get cancel-record :storage-id))
                 status)
              (error
               (ignore-errors
                 (display-warning
                  'superchat-runtime
                  (format "Run %s closed, but delivery failed: %s"
                          run-id (error-message-string delivery-error))
                  :warning))))
            (superchat-runtime--run-completion-hooks entry)
            (superchat-runtime--release entry)
            ;; Shadow is deliberately last and double-isolated: even a bug in
            ;; its diagnostic hook cannot retroactively fail a closed Run.
            (ignore-errors
              (superchat-runtime--run-closure-shadow entry))
            entry)
        (error
         (superchat-runtime--audit-incomplete run-id err response))))))

(defun superchat-runtime--cancel-timeout (entry)
  "Cancel the UI timeout timer associated with ENTRY."
  (let ((buffer (get-buffer (plist-get entry :buffer-name))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (timerp superchat--active-timeout-timer)
          (cancel-timer superchat--active-timeout-timer))
        (setq superchat--active-timeout-timer nil)))))

(defun superchat-runtime-cancel (run-id &optional response)
  "Cancel RUN-ID locally and ask llm.el to stop its latest request.

Cancellation is best-effort at the provider layer.  Runtime immediately stops
accepting later callbacks, so a provider that cannot abort cannot deliver a
late result into the Session View.  RESPONSE customizes the delivered reason."
  (let ((entry (superchat-runtime--entry run-id)))
    (cond
     ((null entry) nil)
     ((superchat-runtime--terminal-p (plist-get entry :status)) nil)
     (t
      (let ((request (plist-get entry :transport-request)))
        ;; Commit local closure first.  Some providers invoke their callback
        ;; synchronously from `llm-cancel-request'; it must observe a terminal
        ;; Run rather than win the race with cancellation.
        (superchat-runtime--cancel-timeout entry)
        (superchat-runtime--finish
         run-id 'cancelled (or response "[Cancelled by user]"))
        (when (and request (fboundp 'llm-cancel-request))
          (ignore-errors (llm-cancel-request request))))
      t))))

(defun superchat-runtime--validate-request (request)
  "Validate prepared Runtime REQUEST."
  (unless (listp request)
    (signal 'wrong-type-argument (list 'listp request)))
  (unless (and (stringp (plist-get request :prompt))
               (not (string-empty-p
                     (string-trim (plist-get request :prompt)))))
    (error "Runtime request requires a non-empty :prompt"))
  (when (plist-member request :session-id)
    (unless (and (stringp (plist-get request :session-id))
                 (not (string-empty-p
                       (plist-get request :session-id))))
      (error "Runtime :session-id must be a non-empty string")))
  (when (plist-member request :parent-run-id)
    (unless (and (stringp (plist-get request :parent-run-id))
                 (not (string-empty-p
                       (plist-get request :parent-run-id))))
      (error "Runtime :parent-run-id must be a non-empty string")))
  (when (and (plist-member request :tool-validator)
             (not (functionp (plist-get request :tool-validator))))
    (error "Runtime :tool-validator must be a function"))
  (when (and (plist-member request :approval-function)
             (not (functionp (plist-get request :approval-function))))
    (error "Runtime :approval-function must be a function"))
  (let ((topology (or (plist-get request :topology) 'single)))
    (when (stringp topology)
      (setq topology (intern topology)))
    (unless (memq topology '(single team subagent workflow))
      (error "Unknown Runtime topology: %s" topology)))
  (let ((policy (or (plist-get request :authority-policy) "default")))
    (unless (superchat-authority-policy-known-p policy)
      (error "Unknown Runtime Authority policy: %s" policy)))
  (when-let* ((root (plist-get request :workspace-root)))
    (unless (and (stringp root) (file-directory-p root))
      (error "Runtime :workspace-root must be an existing directory")))
  request)

(defun superchat-runtime--validate-profile-capability
    (preset requested-capability profile-id)
  "Validate PRESET against REQUESTED-CAPABILITY for PROFILE-ID."
  (when (superchat-preset-agent-p preset)
    (let ((profile-capability
           (superchat-runtime--name
            (superchat-preset-capability preset) nil))
          (requested-capability
           (superchat-runtime--name requested-capability nil)))
      (cond
       ((null profile-capability)
        (display-warning
         'superchat-runtime
         (format "Agent Profile %s has no explicit capability; using %s for compatibility"
                 (or (superchat-preset-name preset) "<unnamed>")
                 (or requested-capability "chat"))
         :warning))
       ((and requested-capability
             (not (equal requested-capability profile-capability)))
        (error "Capability %s conflicts with Agent Profile %s capability %s"
               requested-capability profile-id profile-capability)))))
  preset)

(defun superchat-runtime-submit (request)
  "Submit prepared REQUEST and return an accepted Run handle.

REQUEST is a plist containing at least `:prompt'.  Runtime supplies an
ephemeral Session ID when `:session-id' is absent.  Runtime owns the mutable
execution Turn, transport request, Tape lifecycle, and result delivery;
callers retain only the returned IDs and query status separately."
  (superchat-runtime--validate-request request)
  (unless (fboundp 'superchat--llm-generate-answer)
    (require 'superchat-llm nil t))
  (unless (fboundp 'superchat--llm-generate-answer)
    (error "Superchat model transport is unavailable"))
  (let* ((session-id (or (plist-get request :session-id)
                         (superchat-runtime--new-id "session")))
         (turn (superchat-runtime--turn-from-request request session-id))
         (turn-id (superchat-turn-id turn))
         (run-id (superchat-runtime--new-id "run"))
         (actor-id (or (plist-get request :actor-id) "lead"))
         (parent-run-id (plist-get request :parent-run-id))
         (preset (plist-get request :preset))
         (profile-id
          (let ((profile-id
                 (and preset (superchat-preset-profile-id preset))))
            (superchat-runtime--validate-profile-capability
             preset (plist-get request :capability) profile-id)
            profile-id))
         (capability
          (superchat-runtime--name
           (or (plist-get request :capability)
               (and preset (superchat-preset-capability preset)))
           "chat"))
         (topology
          (let ((value (or (plist-get request :topology) 'single)))
            (if (stringp value) (intern value) value)))
         (authority-policy
          (symbol-name
           (superchat-authority-normalize-policy
            (plist-get request :authority-policy))))
         (surface (superchat-runtime--surface request))
         (correlation-id
          (or (plist-get request :correlation-id) run-id))
         (entry
          (list :session-id session-id
                :turn-id turn-id
                :run-id run-id
                :actor-id actor-id
                :profile-id profile-id
                :parent-run-id parent-run-id
                :workspace-id (plist-get request :workspace-id)
                :workspace-root (plist-get request :workspace-root)
                :correlation-id correlation-id
                :capability capability
                :topology topology
                :authority-policy authority-policy
                :surface surface
                :status 'accepted
                :started-at (float-time)
                :buffer-name
                (or (plist-get request :buffer-name)
                    (bound-and-true-p superchat-buffer-name)
                    "*superchat*")
                :delivery-function (plist-get request :delivery-function)
                :stream-function (plist-get request :stream-function)
                :ephemeral-buffer (plist-get request :ephemeral-buffer)
                :turn turn)))
    (superchat-runtime--put entry)
    (condition-case err
        (let* ((intent (or (plist-get request :input)
                           (plist-get request :user-message)
                           (plist-get request :clean-input)
                           ""))
               (intent-record
                (superchat-runtime--append-event
                 entry "intent.received"
                 (append
                  (superchat-runtime--content-metadata intent)
                  (list :capability capability
                        :topology (symbol-name topology)
                        :authority-policy authority-policy
                        :surface
                        (if (symbolp surface)
                            (symbol-name surface)
                          surface)
                        :workspace-id
                        (plist-get request :workspace-id))
                  (let ((uri
                         (and (listp (plist-get request :origin))
                              (plist-get (plist-get request :origin) :uri))))
                    (when (stringp uri)
                      (list :origin-uri uri))))
                 intent nil))
               (intent-id (plist-get intent-record :event-id))
               (intent-storage-id
                (plist-get intent-record :storage-id))
               (entry
                (superchat-runtime--update
                 run-id :causation-id intent-id))
               (started-record
                (superchat-runtime--append-event
                 entry "run.started"
                 (append
                  (list :status 'running-model
                        :actor-id actor-id
                        :capability capability)
                  (when profile-id (list :profile-id profile-id)))
                 (format "run.started: %s" run-id) intent-id))
               (started-id (plist-get started-record :event-id))
               (entry
                (superchat-runtime--update
                 run-id
                 :status 'running-model
                 :causation-id started-id))
               (turn-state
                (list
                 :runtime-managed t
                 :intent-event-id intent-id
                 :intent-storage-id intent-storage-id
                 :record-conversation
                 (if (plist-member request :record-conversation)
                     (plist-get request :record-conversation)
                   t))))
          (setf (superchat-turn-state turn) turn-state)
          (let ((superchat--tool-execution-context
                 (list :session-id session-id
                       :turn-id turn-id
                       :run-id run-id
                       :actor-id actor-id
                       :profile-id profile-id
                       :parent-run-id parent-run-id
                       :workspace-id (plist-get request :workspace-id)
                       :workspace-root (plist-get request :workspace-root)
                       :correlation-id correlation-id
                       :causation-id started-id
                       :capability capability
                       :topology topology
                       :authority-policy authority-policy
                       :surface surface
                       :attendance (or (plist-get request :attendance)
                                       'attended)
                       :validate (plist-get request :tool-validator)
                       ;; Request-scoped approval, e.g. a Surface granting
                       ;; a consented read scope for one batch Run.
                       :approval-function
                       (plist-get request :approval-function)
                       :audit-failure-function
                       (lambda (err result)
                         (let ((current
                                (superchat-runtime--entry run-id)))
                           (when (and current
                                      (not
                                       (superchat-runtime--terminal-p
                                        (plist-get current :status))))
                             (superchat-runtime--audit-incomplete
                              run-id err (format "%s" result)))))))
                (superchat--llm-runtime-observer
                 (lambda (event value)
                   (pcase event
                     ('request
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (superchat-runtime--update
                           run-id :transport-request value))))
                     ('terminal
                     (superchat-runtime--finish
                       run-id
                       (or (plist-get value :status) 'completed)
                       (or (plist-get value :response) ""))))))
                (superchat--llm-tools-prepared-p
                 (and (plist-member request :prepared-tools) t))
                (superchat--llm-prepared-tools
                 (plist-get request :prepared-tools))
                (superchat--llm-tool-wrapper
                 (plist-get request :tool-wrapper))
                (superchat--llm-round-limit-function
                 (plist-get request :round-limit-function))
                (superchat--llm-unknown-tool-retry-limit
                 (plist-get request :unknown-tool-retries))
                (superchat--llm-persist-synthesized-tools
                 (if (plist-member request :persist-synthesized-tools)
                     (plist-get request :persist-synthesized-tools)
                   t))
                (superchat--llm-run-active-p
                 (lambda ()
                   (let ((current (superchat-runtime--entry run-id)))
                     (and current
                          (not (superchat-runtime--terminal-p
                                (plist-get current :status))))))))
            (let* ((name (plist-get entry :buffer-name))
                   (buffer (get-buffer-create name))
                   (superchat-buffer-name name))
              (with-current-buffer buffer
                (setq-local superchat--last-surface surface)
                (when (plist-member request :workspace-root)
                  (setq-local superchat-tools-default-directory
                              (plist-get request :workspace-root)))
                (superchat--update-status "Assistant is thinking...")
                (superchat--llm-generate-answer
                 (plist-get request :prompt)
                 #'ignore
                 (lambda (chunk)
                   (let ((current (superchat-runtime--entry run-id)))
                     (when (and current
                                (not (superchat-runtime--terminal-p
                                      (plist-get current :status))))
                       (if-let* ((stream
                                  (plist-get current :stream-function)))
                           (funcall stream chunk)
                         (superchat--stream-llm-result chunk)))))
                 (plist-get request :target-model)
                 (plist-get request :context-files)
                 (plist-get request :tools)
                 (plist-get request :system-prompt)
                 preset)))))
      (error
       (let* ((message (error-message-string err))
              (current (superchat-runtime--entry run-id)))
         (unless (superchat-runtime--terminal-p
                  (plist-get current :status))
           (if (eq (plist-get current :status) 'running-model)
               (superchat-runtime--finish run-id 'failed message)
             ;; No provider effect occurred, but the failed audit still needs
             ;; a recoverable Run/Turn terminal seal.
             (superchat-runtime--audit-incomplete run-id err ""))))
       (signal (car err) (cdr err))))
    (list :turn-id turn-id
          :run-id run-id
          :session-id session-id
          :status 'accepted)))

(defun superchat-runtime-submit-blocking (request &optional timeout)
  "Submit REQUEST and return its response after the same Run completes.

TIMEOUT defaults to REQUEST's `:timeout', then
`superchat-response-timeout'.  Non-success terminal states signal an error."
  (let* ((request (copy-sequence request))
         (original-delivery (plist-get request :delivery-function))
         (limit (or timeout
                    (plist-get request :timeout)
                    (and (boundp 'superchat-response-timeout)
                         superchat-response-timeout)
                    120))
         response status done handle)
    (setq request
          (plist-put
           request :delivery-function
           (lambda (value terminal-status tape-id)
             (setq response value
                   status terminal-status
                   done t)
             (when (functionp original-delivery)
               (funcall original-delivery value terminal-status tape-id)))))
    (setq handle (superchat-runtime-submit request))
    (let ((deadline (and limit (+ (float-time) limit))))
      (while (and (not done)
                  (or (null deadline) (< (float-time) deadline)))
        (accept-process-output nil 0.05))
      (unless done
        (superchat-runtime-cancel
         (plist-get handle :run-id)
         (format "[Timed out after %ss]" limit))
        (error "Superchat Runtime timed out after %ss" limit)))
    (if (eq status 'completed)
        response
      (error "Superchat Runtime %s: %s" status response))))

(provide 'superchat-runtime)

;;; superchat-runtime.el ends here
