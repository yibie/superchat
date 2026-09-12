;;; superchat-runtime.el --- Main Turn/Run lifecycle for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Runtime owns main Chat/Agent execution.  Callers submit a prepared request;
;; the mutable Turn, provider request, lifecycle events, and delivery remain
;; behind `superchat-runtime-submit'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'superchat-core)
(require 'superchat-preset)
(require 'superchat-tape)
(require 'superchat-closure)
(require 'superchat-authority)
(require 'superchat-sink)
(require 'superchat-service)
(require 'superchat-invariant)

(declare-function llm-cancel-request "llm" (request))
(declare-function superchat-llm--input-pause-clock "superchat-llm" (owner handle))
(declare-function superchat-llm--transport-timer-owner-cancel
                  "superchat-llm" (owner))
(declare-function superchat--stream-llm-result "superchat-render" (chunk &optional owner))
(declare-function superchat--response-region-begin "superchat-render" (owner &optional initial-assistant))
(declare-function superchat--process-llm-result
                  "superchat-render" (answer &optional tape-id owner))
(declare-function superchat--response-region-insert-persistent
                  "superchat-render"
                  (content &optional owned-response-marker owner))
(declare-function superchat--update-status "superchat-render" (message &optional owner))
(declare-function superchat-experience-assemble-recovery-view
                  "superchat-experience" (failure-event session-ids))
(declare-function superchat-tape--new-event-id "superchat-tape" ())
(declare-function superchat-tape--event-state "superchat-tape" (event))
(declare-function superchat-tape--run-terminal-state "superchat-tape" (session-id run-id))
(declare-function superchat-tape--turn-terminal-state "superchat-tape" (session-id run-id turn-id))

(defvar superchat-buffer-name)
(defvar superchat--active-timeout-timer)
(defvar superchat--active-timeout-timer-owner)
(defvar superchat--session-id)
(defvar superchat--active-run-id)
(defvar superchat-authority-mode)
(defvar superchat-mode)
(defvar superchat--last-surface)
(defvar superchat-tools-default-directory)
(defvar superchat--llm-runtime-observer nil
  "Internal observer bound by Runtime around one transport invocation.")
(defvar superchat--llm-transcript-owner nil
  "Opaque presentation owner captured by the LLM request adapter.")
(defvar superchat--runtime-transcript-owner nil
  "Dynamic transcript owner retained for custom Run presentation callbacks.

Legacy Render entries use this value to select their response region.
Default Runtime handoffs pass an explicit owner.  This remains dynamic: custom
stream and delivery callbacks keep their existing calling convention while
still crossing the single Render region seam.")

(defvar superchat--llm-evidence-context nil
  "Internal evidence contract bound by Runtime around one transport invocation.")
(defvar superchat--llm-context-envelope nil
  "Context View bound by Runtime around one transport invocation.")
(defvar superchat--llm-request-max-tokens nil
  "Request-local output-token limit passed to the transport adapter.")
(defvar superchat--llm-tools-prepared-p nil)
(defvar superchat--llm-prepared-tools nil)
(defvar superchat--llm-tool-wrapper nil)
(defvar superchat--llm-round-limit-function nil)
(defvar superchat--llm-unknown-tool-retry-limit nil)
(defvar superchat--llm-persist-synthesized-tools t)
(defvar superchat--llm-run-active-p nil)
(defvar superchat--llm-transport-timer-owner-function nil)
(defvar superchat--llm-drain-steers-function nil)
(defvar superchat--llm-pending-steer-count-function nil)
(defvar superchat--tool-execution-context)
(defvar superchat-llm-tools-enabled)
(defvar superchat-runtime-recovery-intervention-enabled nil
  "Internal gate for same-Run reviewed recovery guidance.")
(defconst superchat-runtime--evidence-audit-error
  '(error "Exact evidence audit failed")
  "Public-safe error used when exact evidence persistence fails.")











(defvar superchat-closure-shadow-enabled nil
  "When non-nil, compare terminal Runtime state with Closure judgments.")

(defvar superchat-closure-terminal-guard-enabled t
  "When non-nil, require Closure readiness before a Run terminal seal.")



(define-error 'superchat-closure-terminal-not-ready
  "Closure terminal guard rejected transition")

(defconst superchat-runtime--terminal-statuses
  '(completed failed cancelled timed-out audit-incomplete abandoned)
  "States after which a Run ignores further transport callbacks.")

(defcustom superchat-runtime-default-deadline 1800
  "Default wall-clock deadline for a Runtime Run, in seconds.

The deadline covers the whole Run, including all model and tool rounds, and
therefore also applies to headless submissions.  Set this to nil or zero to
disable the default.  A larger value permits longer Runs but leaves them
unbounded for longer; a smaller value limits stuck Runs sooner but may time out
legitimate long workflows.  A Request's `:deadline' property overrides this
value for that Run."
  :type '(choice (const :tag "No deadline" nil)
                 (natnum :tag "Seconds"))
  :group 'superchat)

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

(defun superchat-runtime-active-runs ()
  "Return Run IDs still executing or retaining opted-in stop control, newest first.
Ordering comes from each entry's :started-at timestamp."
  (let ((entries nil))
    (maphash (lambda (_id entry) (push entry entries))
             superchat-runtime--runs)
    (mapcar
     (lambda (entry) (plist-get entry :run-id))
     (sort
      (cl-remove-if
       (lambda (entry)
         (and (superchat-runtime--terminal-p (plist-get entry :status))
              (not (plist-get entry :input-control-active))))
       entries)
      (lambda (a b)
        (> (or (plist-get a :started-at) 0)
           (or (plist-get b :started-at) 0)))))))

(defun superchat-runtime--entry (run-id)
  "Return the private registry entry for RUN-ID."
  (gethash run-id superchat-runtime--runs))

(defun superchat-runtime--execution-scope (entry)
  "Return ENTRY's borrowed execution Scope, or the global Scope.

The reference is process-local only: it is never copied into a Request,
Envelope, Tape event, Sink projection, or public Run status.  Runtime borrows
it to resume Stage/Event lookup after an async callback; it neither disposes
the Scope nor gives it lifecycle ownership over the Run."
  (or (plist-get entry :execution-scope) superchat-scope-global))

(defun superchat-runtime--assert-execution-scope-active (scope)
  "Reject new Runtime work through retired borrowed SCOPE.

This is an admission/continuation check only.  It does not dispose SCOPE or
make Runtime its owner."
  (when (superchat-execution-scope-retired-p scope)
    (error "Superchat execution Scope is retired"))
  scope)

(defun superchat-runtime--assert-parent-attach-active (parent-run-id)
  "Reject a child attachment whose PARENT-RUN-ID is no longer eligible.

This retains the existing parent attach requirements and additionally refuses
a parent whose borrowed execution Scope has retired.  It is intentionally not
a `running-model' test: accepted and controlled parent Runs retain their
existing attachment contract until their ordinary Runtime state says otherwise."
  (let ((parent (superchat-runtime--entry parent-run-id)))
    (unless (and parent
                 (not (superchat-runtime--terminal-p
                       (plist-get parent :status)))
                 (not (plist-get parent :terminalizing))
                 (superchat-runtime--transport-current-p parent)
                 (not (superchat-execution-scope-retired-p
                       (superchat-runtime--execution-scope parent))))
      (error "Parent Run unavailable: %s" parent-run-id))
    parent))

(defun superchat-runtime--entry-buffer (entry)
  "Return ENTRY's live authoritative View buffer.
Entries always carry a final exact object identity in `:buffer'; a dead object
resolves to nil and display names are never host locators."
  (let ((buffer (plist-get entry :buffer)))
    (and (buffer-live-p buffer) buffer)))

(defun superchat-runtime-transport-timer-owner (run-id)
  "Return RUN-ID's opaque transport timer owner, or nil."
  (plist-get (superchat-runtime--entry run-id) :transport-timer-owner))

(defun superchat-runtime--put (entry)
  "Store and return private Run ENTRY."
  (puthash (plist-get entry :run-id) entry superchat-runtime--runs)
  entry)

(defun superchat-runtime--publish-entry (entry)
  "Publish exact Run ENTRY atomically with respect to nonlocal exit."
  (let ((run-id (plist-get entry :run-id)) committed-p result)
    (unwind-protect
        (progn
          (setq result (superchat-runtime--put entry)
                committed-p t)
          result)
      (when (and (not committed-p)
                 (eq entry (gethash run-id superchat-runtime--runs)))
        (remhash run-id superchat-runtime--runs)))))

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
      (dolist (key '(:session-id :turn-id :run-id :actor-id :profile-id :model
                     :parent-run-id :workspace-id :correlation-id
                     :transcript-owner
                     :capability :topology :authority-policy :surface
                     :status :started-at :completed-at :error
                     :stop-requested :stop-notification-error :owner-stop-error :owner-cleanup-error
                     :attempt :attempt-reason :model-phase
                     :context-recovery
                     :context-used :context-limit :context-limit-source
                     :context-estimated-p
                     :input-tokens :output-tokens
                     :runtime-selected-tools :runtime-tool-decision))
        (when (plist-member entry key)
          (setq status (plist-put status key (plist-get entry key)))))
      (when (superchat-runtime--controlled-p entry)
        (setq status (plist-put status :control-active-p
                                (and (plist-get entry :input-control-active) t))))
      (when (and (superchat-runtime--terminal-p (plist-get entry :status))
                 (plist-get entry :input-continuation-blocked))
        (setq status (plist-put status :input-continuation-blocked
                                (plist-get entry :input-continuation-blocked))))
      (setq status
            (plist-put status :pending-steer-count
                       (length (plist-get entry :steer-queue))))
      (setq status
            (plist-put status :child-run-ids
                       (plist-get entry :children)))
      (when (plist-member entry :undelivered-steer-count)
        (setq status
              (plist-put status :undelivered-steer-count
                         (plist-get entry :undelivered-steer-count))))
      status)))

(defun superchat-runtime-status (run-id)
  "Return public status for RUN-ID, or nil when it is unknown.
Terminal :input-continuation-blocked is unanswered or answer-not-delivered
when an explicit input never entered its native consumer.  It is not an
expiry cause, cancellation, receipt judgment, or running permission flag."
  (superchat-runtime--public-status (superchat-runtime--entry run-id)))

(defun superchat-runtime-sink-run (run-id)
  "Return RUN-ID's callback-safe projection for its Sink.

Exactly the keys :run-id :turn-id :session-id :surface :status
:parent-run-id :started-at :completed-at :error :pending-steer-count
:sink-id, taken from `superchat-runtime--public-status' (plus :sink-id) and
passed through `copy-tree'.  It contains no buffer, marker, function, timer
or process."
  (let* ((entry (superchat-runtime--entry run-id))
         (status (superchat-runtime--public-status entry))
         projection)
    (when status
      (dolist (key '(:run-id :turn-id :session-id :surface :status
                     :parent-run-id :started-at :completed-at :error
                     :pending-steer-count))
        (setq projection (plist-put projection key (plist-get status key))))
      (setq projection
            (plist-put projection :sink-id
                       (superchat-sink-id (plist-get entry :sink))))
      (copy-tree projection))))

(defun superchat-runtime-run-in-buffer-p (run-id buffer)
  "Whether known RUN-ID's authoritative host is the exact live BUFFER object.
Names, unknown Runs and dead buffers do not match.  Renaming the same object
preserves the association.  This read-only query does not imply Run liveness."
  (and (bufferp buffer)
       (buffer-live-p buffer)
       (let ((entry (superchat-runtime--entry run-id)))
         (and entry (eq buffer (plist-get entry :buffer))))))

(defun superchat-runtime-status-projection (&optional current-run-id)
  "Return the public status projection for all known Runtime Runs.

The projection is the single UI-facing source for live Run state.  It contains
all public statuses, the active subset, and the active subset excluding
CURRENT-RUN-ID as `:background-runs'.  No UI state is stored here; callers may
derive their local phase or elapsed display from these status plists."
  (let (statuses)
    (maphash (lambda (_run-id entry)
               (when-let* ((status (superchat-runtime--public-status entry)))
                 (push status statuses)))
             superchat-runtime--runs)
    (setq statuses
          (sort statuses
                (lambda (left right)
                  (> (or (plist-get left :started-at) 0)
                     (or (plist-get right :started-at) 0)))))
    (let ((active-runs
           (cl-remove-if
            (lambda (status)
              (superchat-runtime--terminal-p (plist-get status :status)))
            statuses)))
      (list :runs statuses
            :active-runs active-runs
            :active-count (length active-runs)
            :background-runs
            (cl-remove-if
             (lambda (status)
               (equal current-run-id (plist-get status :run-id)))
             active-runs)
            :background-count
            (length
             (cl-remove-if
              (lambda (status)
                (equal current-run-id (plist-get status :run-id)))
              active-runs))))))

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

(defun superchat-runtime--reconciliation-entry (event)
  "Return the Runtime identity fields needed to seal EVENT's Run.
EVENT is a persisted `run.started' envelope, not a live registry entry."
  (let (entry)
    (dolist (key '(:session-id :turn-id :run-id :actor-id :profile-id
                   :parent-run-id :workspace-id :correlation-id))
      (when (plist-member event key)
        (setq entry (plist-put entry key (plist-get event key)))))
    entry))

(defun superchat-runtime--reconciliation-terminal-p (event)
  "Return non-nil when EVENT is a durable Run terminal event."
  (member (plist-get event :type)
          '("run.completed" "run.failed" "run.cancelled")))

(defun superchat-runtime--reconciliation-abandoned-p (event)
  "Return non-nil when EVENT is an explicit abandoned Run seal."
  (let* ((payload (plist-get event :payload))
         (status (plist-get payload :status)))
    (and (equal "run.failed" (plist-get event :type))
         (or (eq status 'abandoned) (equal status "abandoned"))
         (eq t (plist-get payload :abandoned)))))

(defun superchat-runtime-reconcile-session (session-id)
  "Seal open persisted Runs in SESSION-ID as `abandoned'.

This is restart reconciliation, not execution recovery: it never recreates a
prompt or provider request.  Tape and Closure are the only sources of truth;
the process-local Run registry is intentionally ignored.  Return the number
of Run/Turn pairs written.  An outer read or append failure emits a warning but
is contained so opening a Session View remains usable."
  (condition-case err
      (let ((trace (superchat-tape-replay-events session-id))
            (seen-runs nil)
            (sealed 0))
        (dolist (started
                 (cl-remove-if-not
                  (lambda (event)
                    (equal "run.started" (plist-get event :type)))
                  trace)
                 sealed)
          (let ((run-id (plist-get started :run-id)))
            (when (and (stringp run-id)
                       (not (string-empty-p run-id))
                       (not (member run-id seen-runs)))
              (push run-id seen-runs)
              (condition-case nil
                  (let* ((run-events
                          (cl-remove-if-not
                           (lambda (event)
                             (equal run-id (plist-get event :run-id)))
                           trace))
                         (terminal
                          (cl-find-if
                           #'superchat-runtime--reconciliation-terminal-p
                           run-events))
                         (turn-id (plist-get started :turn-id))
                         (turn-terminal
                          (cl-find-if
                           (lambda (event)
                             (and (equal "turn.completed"
                                         (plist-get event :type))
                                  (equal turn-id (plist-get event :turn-id))))
                           run-events))
                         (judgment
                          (and (not terminal)
                               (stringp turn-id)
                               (not (string-empty-p turn-id))
                               (superchat-closure-judge
                                (list :kind 'durable-run-terminal
                                      :run-id run-id)
                                trace))))
                    (cond
                     ;; A partially written recovery pair is completed on the
                     ;; next View rebuild, without duplicating its Run seal.
                     ((and terminal
                           (superchat-runtime--reconciliation-abandoned-p
                            terminal)
                           (not turn-terminal))
                      (let* ((entry
                              (superchat-runtime--reconciliation-entry started))
                             (record
                              (superchat-tape-append-event-record
                               (superchat-runtime--event
                                entry "turn.completed"
                                '(:status abandoned :abandoned t)
                                (format "turn.completed: %s (abandoned)"
                                        run-id)
                                (plist-get terminal :event-id)))))
                        (when record
                          (cl-incf sealed))))
                     ((and (null terminal)
                           (not turn-terminal)
                           (eq 'open (plist-get judgment :verdict)))
                      (let* ((entry
                              (superchat-runtime--reconciliation-entry started))
                             (last-event (car (last run-events)))
                             (run-record
                              (superchat-tape-append-event-record
                               (superchat-runtime--event
                                entry "run.failed"
                                '(:status abandoned :abandoned t)
                                (format "run.failed: %s (abandoned after restart)"
                                        run-id)
                                (plist-get last-event :event-id))))
                             (turn-record
                              (superchat-tape-append-event-record
                               (superchat-runtime--event
                                entry "turn.completed"
                                '(:status abandoned :abandoned t)
                                (format "turn.completed: %s (abandoned)"
                                        run-id)
                                (plist-get run-record :event-id)))))
                        (when (and run-record turn-record)
                          (cl-incf sealed))))))
                (error nil)))))
        sealed)
    (error
     (condition-case nil
         (display-warning
          'superchat-runtime
          (format
           "Session %s reconciliation failed: %s; open Runs were not claimed and remain open in Tape."
           session-id (error-message-string err))
          :warning)
       (error nil))
     0)))

(defun superchat-runtime--append-event
    (entry type payload summary &optional causation-id requires-payload event-id)
  "Append a Runtime event for ENTRY and return its identity record.
When REQUIRES-PAYLOAD is non-nil, fail instead of persisting hash-only data.
EVENT-ID, when non-nil, is the caller's stable identity for a seal whose
receipt might be lost after SQLite has already committed it."
  (let ((event
         (superchat-runtime--event
          entry type payload summary
          (or causation-id (plist-get entry :causation-id)))))
    (when requires-payload
      (setq event (plist-put event :requires-payload t))
      (setq event (plist-put event :sensitivity "restricted")))
    (when event-id
      (setq event (plist-put event :event-id event-id)))
    (prog1 (superchat-tape-append-event-record event)
      (when (equal type "run.started")
        (superchat-emit (superchat-runtime--execution-scope entry) 'run-started
                        (superchat-runtime-sink-run (plist-get entry :run-id))
                        (plist-get entry :request))))))

(defun superchat-runtime--recovery-intervention-view
    (run-id attempt-reason failure-event &optional failure-info)
  "Return and record one eligible recovery View before RUN-ID's next attempt.
FAILURE-EVENT is the exact Authority event that caused this follow-up.
FAILURE-INFO is request-local metadata supplied by Authority; it is never
persisted as raw tool arguments."
  (when (and superchat-runtime-recovery-intervention-enabled
             (eq attempt-reason 'tool-followup)
             failure-event
             (not (equal "user-quit"
                         (plist-get (plist-get failure-event :payload)
                                    :interruption)))
             (stringp
              (plist-get (plist-get failure-event :payload) :error-type))
             (superchat-service-get nil 'recovery))
    (let ((current (superchat-runtime--entry run-id)))
      (if (or (null current)
              (not (equal run-id (plist-get failure-event :run-id)))
              (not (equal "tool.failed"
                          (plist-get failure-event :type))))
          nil
        (if (plist-get current :experience-intervention-attempted)
            (copy-tree
             (plist-get (plist-get current :context-envelope)
                        :experience-view)
             t)
          (let* ((envelope (plist-get current :context-envelope))
               (session-ids
                (delete-dups
                 (cons (plist-get failure-event :session-id)
                       (copy-sequence
                        (plist-get envelope :experience-session-ids)))))
               (view
                (superchat-service-call nil 'recovery 'assemble-view
                                        failure-event session-ids)))
          (if (not (eq 'satisfied (plist-get view :status)))
              (progn
                ;; A blocked recovery View is itself an Authority input: the
                ;; failed dependent action stays closed while the model may
                ;; choose an information-gathering tool.  Persist the policy
                ;; on the Run so wrappers created before this attempt can
                ;; read the current context without a second global gate.
                (let ((policy
                       (plist-get view :experience-action-policy)))
                  (if policy
                      (let ((envelope
                             (copy-tree
                              (plist-get current :context-envelope) t)))
                        (setq envelope
                              (plist-put envelope
                                         :experience-action-policy
                                         (copy-tree policy t)))
                        (setq envelope
                              (plist-put envelope :experience-view
                                         (copy-tree view t)))
                        (superchat-runtime--update
                         run-id :context-envelope envelope
                         :experience-intervention-attempted t))
                    (superchat-runtime--update
                     run-id :experience-intervention-attempted t)))
                (and (eq 'blocked (plist-get view :status)) view))
            (let* ((trigger
                    (list :session-id (plist-get failure-event :session-id)
                          :event-id (plist-get failure-event :event-id)))
                   ;; The prompt remains a soft nudge.  This small policy is
                   ;; the hard Runtime/Authority seam: after a typed revision
                   ;; failure, repeat reads are blocked until workspace_info
                   ;; succeeds, then the corrected read is allowed.
                   (policy
                    (and (equal "workspace_read"
                                (plist-get (plist-get failure-event :payload)
                                           :tool-name))
                         (equal "superchat-workspace-artifact-revision-error"
                                (plist-get (plist-get failure-event :payload)
                                           :error-type))
                         (list :kind "workspace-revision-recovery"
                               :phase "awaiting-info"
                               :required-tool "workspace_info"
                               :dependent-tools (list "workspace_read")
                               :artifact-id
                               (let ((args (plist-get failure-info :args)))
                                 (and (listp args)
                                      (stringp (car args))
                                      (car args)))
                               :failure-event-id
                               (plist-get failure-event :event-id))))
                   (record
                    (superchat-runtime--append-event
                     current "experience.intervention.recorded"
                     (list :trigger trigger
                           :scope (plist-get view :scope)
                           :context (plist-get view :context)
                           :receipt (copy-tree (plist-get view :receipt) t))
                     "experience.intervention.recorded"
                     (plist-get failure-event :event-id) t)))
              (when policy
                (setq view
                      (plist-put view :experience-action-policy
                                 (copy-tree policy t))))
              (let ((next-envelope (copy-tree envelope t)))
                (when policy
                  (setq next-envelope
                        (plist-put next-envelope
                                   :experience-action-policy
                                   (copy-tree policy t))))
                (setq next-envelope
                      (plist-put next-envelope
                                 :experience-view
                                 (copy-tree view t)))
                (superchat-runtime--update
                 run-id :experience-intervention-attempted t
                 :experience-intervention-event-id
                 (plist-get record :event-id)
                 :causation-id (plist-get record :event-id)
                 :context-envelope next-envelope))
              view))))))))

(defun superchat-runtime--recovery-tool-completed
    (run-id completion)
  "Advance RUN-ID's typed revision recovery policy after COMPLETION.
COMPLETION is request-local metadata from Authority and is not a Tape event.
Only successful `workspace_info' and `workspace_read' calls advance the
policy; ordinary failures and terminal Runs leave it unchanged."
  (let* ((entry (superchat-runtime--entry run-id))
         (envelope (and entry (plist-get entry :context-envelope)))
         (policy (and (listp envelope)
                      (plist-get envelope :experience-action-policy)))
         (phase (and (listp policy) (plist-get policy :phase)))
         (tool-name (plist-get completion :tool-name))
         (args (plist-get completion :args))
         (result (format "%s" (or (plist-get completion :result) "")))
         (artifact-id (and (listp policy)
                           (plist-get policy :artifact-id)))
         (same-artifact
          (or (null artifact-id)
              (equal artifact-id (car args))
              (string-match-p (regexp-quote artifact-id) result))))
    (when (and entry
               (not (superchat-runtime--terminal-p (plist-get entry :status)))
               (equal "workspace-revision-recovery"
                      (and (listp policy) (plist-get policy :kind))))
      (cond
       ((and (equal "awaiting-info" phase)
             (equal "workspace_info" tool-name)
             (or (null artifact-id) same-artifact))
        (let ((next (copy-tree policy t))
              (next-envelope (copy-tree envelope t)))
          (setq next (plist-put next :phase "awaiting-read"))
          (setq next-envelope
                (plist-put next-envelope :experience-action-policy next))
          (when-let* ((view (plist-get next-envelope :experience-view)))
            (setq next-envelope
                  (plist-put next-envelope :experience-view
                             (plist-put (copy-tree view t)
                                        :experience-action-policy next))))
          (superchat-runtime--update
           run-id :context-envelope next-envelope)))
       ((and (equal "awaiting-read" phase)
             (equal "workspace_read" tool-name)
             same-artifact)
        (let ((next (copy-tree policy t))
              (next-envelope (copy-tree envelope t)))
          (setq next (plist-put next :phase "satisfied"))
          (setq next (plist-put next :satisfied-by
                                (list :tool tool-name
                                      :artifact-id (car args))))
          (setq next-envelope
                (plist-put next-envelope :experience-action-policy next))
          (when-let* ((view (plist-get next-envelope :experience-view)))
            (setq next-envelope
                  (plist-put next-envelope :experience-view
                             (plist-put (copy-tree view t)
                                        :experience-action-policy next))))
          (superchat-runtime--update
           run-id :context-envelope next-envelope)))))))

(defun superchat-runtime--content-metadata (content)
  "Return non-sensitive identity metadata for CONTENT."
  (let* ((text (or content ""))
         (encoded (encode-coding-string text 'utf-8)))
    (list :content-size (string-bytes encoded)
          :content-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-runtime--append-steer-undelivered (entry instruction)
  "Record that INSTRUCTION could not be queued because ENTRY refused it."
  (superchat-runtime--append-event
   entry "run.steer-undelivered"
   (list :count 1
         :instruction-hashes
         (list (plist-get (superchat-runtime--content-metadata instruction)
                           :content-hash)))
   (format "run.steer-undelivered: %s (1)" (plist-get entry :run-id))
   nil t))

(defun superchat-runtime-steer (run-id text)
  "Persist TEXT and queue it for RUN-ID's next model attempt.
Return the Tape identity record on acceptance, or nil when RUN-ID is
unknown, terminalizing, or terminal."
  (unless (and (stringp text) (not (string-empty-p (string-trim text))))
    (error "Runtime steer requires non-empty text"))
  (let* ((entry (superchat-runtime--entry run-id))
         (instruction (string-trim text))
         ;; `plist-put' extends ENTRY's plist with `nconc' even for a brand
         ;; new key, mutating this same list in place; capture the queue's
         ;; pre-push VALUE now, not by re-reading ENTRY after the push.
         (previous-queue (plist-get entry :steer-queue)))
    (if (and entry
             (superchat-runtime-effect-capable-p
              run-id))
        ;; Enqueue before persisting: when the append signals, the queue is
        ;; rolled back to exactly its pre-call shape (§7.1).
        (progn
          (superchat-runtime--update
           run-id :steer-queue
           (append previous-queue (list instruction)))
          (or (condition-case nil
                  (superchat-runtime--append-event
                   (superchat-runtime--entry run-id) "run.steered"
                   (append (superchat-runtime--content-metadata instruction)
                           (list :instruction instruction))
                   (format "run.steered: %s" run-id)
                   nil t)
                (error nil))
              (progn
                (superchat-runtime--update
                 run-id :steer-queue previous-queue)
                nil)))
      (when entry
        (superchat-runtime--append-steer-undelivered entry instruction)
        nil))))

(defun superchat-runtime-drain-steers (run-id)
  "Return and clear queued steers for active RUN-ID."
  (let ((entry (superchat-runtime--entry run-id)))
    (when (and entry
               (superchat-runtime-effect-capable-p
                run-id))
      (let ((queue (plist-get entry :steer-queue)))
        (when queue
          (superchat-runtime--update run-id :steer-queue nil))
        (copy-sequence queue)))))
(defalias 'superchat-runtime--drain-steers 'superchat-runtime-drain-steers
  "Deprecated alias for `superchat-runtime-drain-steers'.")
(make-obsolete 'superchat-runtime--drain-steers 'superchat-runtime-drain-steers "one release")

(defun superchat-runtime-pending-steer-count (run-id)
  "Return the number of steers queued for RUN-ID without consuming them."
  (length (plist-get (superchat-runtime--entry run-id) :steer-queue)))

(defun superchat-runtime--mark-undelivered-steers (entry causation-id)
  "Record and clear steers that cannot reach another model attempt.
ENTRY is terminally closing; CAUSATION-ID identifies that terminal path."
  (let ((queue (plist-get entry :steer-queue)))
    (if (null queue)
        entry
      (let ((count (length queue)))
        (superchat-runtime--append-event
         entry "run.steer-undelivered"
         (list :count count
               :instruction-hashes
               (mapcar
                (lambda (instruction)
                  (plist-get (superchat-runtime--content-metadata instruction)
                             :content-hash))
                queue))
         (format "run.steer-undelivered: %s (%d)"
                 (plist-get entry :run-id) count)
         causation-id t)
        (superchat-runtime--update
         (plist-get entry :run-id)
         :steer-queue nil
         :undelivered-steer-count count)))))

(defun superchat-runtime--plist-p (value)
  "Return non-nil when VALUE is a keyword property list."
  (and (listp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun superchat-runtime--record-decision-snapshot
    (run-id evidence-contract value)
  "Record one prepared provider-attempt VALUE for RUN-ID."
  (let ((current (superchat-runtime--entry run-id)))
    (when (and current
               (not (superchat-runtime--terminal-p
                     (plist-get current :status))))
      (condition-case err
          (let* ((attempt (plist-get value :attempt))
                 (attempt-reason (plist-get value :attempt-reason))
                 (prepared (plist-get value :prepared-envelope))
                 (records (plist-get current :decision-snapshot-events)))
            (unless (and (integerp attempt) (> attempt 0))
              (error "Decision snapshot needs an attempt ordinal"))
            (unless (member attempt-reason
                            '("initial" "tool-protocol-retry"
                              "tool-followup" "final-answer"))
              (error "Decision snapshot has an invalid attempt reason"))
            (unless (superchat-runtime--plist-p prepared)
              (error "Decision snapshot needs a prepared envelope"))
            (unless (and (equal "superchat-prepared-envelope-v0"
                                (plist-get prepared :format))
                         (equal attempt-reason
                                (plist-get prepared :attempt-reason)))
              (error "Decision snapshot envelope does not match its receipt"))
            (if-let* ((existing (assq attempt records)))
                (cdr existing)
              (unless (= attempt (1+ (length records)))
                (error "Decision snapshot attempt out of order: %s" attempt))
              (let* ((receipt (superchat-tape-exact-json prepared))
                     (payload
                      (list
                       :evidence-contract evidence-contract
                       :attempt attempt
                       :attempt-reason attempt-reason
                       :provider (plist-get value :provider)
                       :model (plist-get value :model)
                       :prepared-envelope-json (plist-get receipt :json)
                       :prepared-envelope-size (plist-get receipt :size)
                       :prepared-envelope-hash (plist-get receipt :hash)))
                     (record
                      (superchat-runtime--append-event
                       current "decision.snapshot.recorded" payload
                       (format "decision.snapshot.recorded: %s/%d"
                               run-id attempt)
                       nil t))
                     (event-id (plist-get record :event-id)))
                (superchat-runtime--update
                 run-id
                 :decision-snapshot-events
                 (append records (list (cons attempt event-id)))
                 :causation-id event-id)
                event-id)))
        (error
         (superchat-runtime--audit-incomplete
          run-id superchat-runtime--evidence-audit-error "")
         (signal (car superchat-runtime--evidence-audit-error)
                 (cdr superchat-runtime--evidence-audit-error)))))))

(defun superchat-runtime--record-first-output
    (run-id evidence-contract value)
  "Record the first observable VALUE of one RUN-ID provider attempt."
  (let ((current (superchat-runtime--entry run-id)))
    (when (and current
               (not (superchat-runtime--terminal-p
                     (plist-get current :status))))
      (condition-case err
          (let* ((attempt (plist-get value :attempt))
                 (records (plist-get current :first-output-events))
                 (snapshots
                  (plist-get current :decision-snapshot-events)))
            (unless (and (integerp attempt) (> attempt 0))
              (error "First output needs an attempt ordinal"))
            (unless (assq attempt records)
              (unless (assq attempt snapshots)
                (error "First output has no decision snapshot: %s" attempt))
              (let* ((receipt (superchat-tape-exact-json value))
                     (snapshot-id (cdr (assq attempt snapshots)))
                     (payload
                      (list
                       :evidence-contract evidence-contract
                       :attempt attempt
                       :kind (plist-get value :kind)
                       :observed-json (plist-get receipt :json)
                       :observed-size (plist-get receipt :size)
                       :observed-hash (plist-get receipt :hash)))
                     (record
                      (superchat-runtime--append-event
                       current "output.first.recorded" payload
                       (format "output.first.recorded: %s/%d"
                               run-id attempt)
                       snapshot-id t))
                     (event-id (plist-get record :event-id)))
                (if (= attempt (caar (last snapshots)))
                    (superchat-runtime--update
                     run-id
                     :first-output-events
                     (append records (list (cons attempt event-id)))
                     :causation-id event-id)
                  (superchat-runtime--update
                   run-id
                   :first-output-events
                   (append records (list (cons attempt event-id))))))))
        (error
         (superchat-runtime--audit-incomplete
          run-id superchat-runtime--evidence-audit-error "")
         (signal (car superchat-runtime--evidence-audit-error)
                 (cdr superchat-runtime--evidence-audit-error)))))))

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
        (scope (superchat-runtime--execution-scope entry)))
    (when (eq (plist-get entry :status) 'completed)
      (superchat-emit scope 'run-completed
                      (superchat-runtime-sink-run (plist-get entry :run-id)) status))
    (superchat-emit scope 'turn-completed
                    (superchat-runtime-sink-run (plist-get entry :run-id)) status)))

(defun superchat-runtime--guard-terminal (entry status)
  "Require a proved Closure readiness judgment for ENTRY and STATUS.
Return the judgment when the Guard is enabled and nil otherwise."
  (when superchat-closure-terminal-guard-enabled
    ;; ponytail: the default-enabled Guard reuses canonical Session replay; add a
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

(defun superchat-runtime--run-closure-shadow (entry &optional execution-scope)
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
      (superchat-emit (or execution-scope
                          (superchat-runtime--execution-scope entry))
                      'closure-shadow report))))

(defun superchat-runtime--sink-stop-requested-p (run)
  "Return non-nil when RUN's registry entry has an active stop request."
  (plist-get (superchat-runtime--entry (plist-get run :run-id)) :stop-requested))

(defun superchat-runtime--request-sink (request)
  "Return REQUEST's Sink: the explicit `:sink', which must be a `superchat-sink'.
Every producer submits through `superchat-pipeline-submit', which always
supplies one (contract §2.2).  The legacy `:stream-function' /
`:delivery-function' bridge and the default View-Sink fallback are gone;
their presence is an error, signalled before any Tape row."
  (when (or (plist-member request :stream-function)
            (plist-member request :delivery-function))
    (error "Runtime request must carry :sink; :stream-function / :delivery-function are no longer accepted"))
  (let ((sink (plist-get request :sink)))
    (unless (superchat-sink-p sink)
      (error "Runtime request :sink must be a superchat-sink"))
    sink))

(defun superchat-runtime--quit-condition (emit-result)
  "Return EMIT-RESULT's :condition when it is a `quit', else nil."
  (let ((condition (plist-get emit-result :condition)))
    (and (eq (car-safe condition) 'quit) condition)))

(defun superchat-runtime--record-delivery-failed (entry sink emits)
  "Append one `delivery.failed' Tape row for ENTRY when any of EMITS failed.
Also emits the same `display-warning' today's Runtime gave for a failed
delivery consumer, so a closed Run's failure stays visible even though it
cannot rewrite the durable Tape facts."
  (let ((failed (cl-remove-if-not (lambda (e) (plist-get e :condition)) emits)))
    (when failed
      (condition-case append-error
          (superchat-runtime--append-event
           entry "delivery.failed"
           (list :sink-id (superchat-sink-id sink)
                 :conditions
                 (mapcar (lambda (e)
                           (list :callback (plist-get e :callback)
                                 :condition (format "%S" (plist-get e :condition))))
                         failed))
           "delivery.failed")
        (error
         (superchat-sink-record-diagnostic
          (superchat-sink-id sink) 'delivery.failed append-error)))
      (ignore-errors
        (display-warning
         'superchat-runtime
         (format "Run %s closed, but delivery failed: %s"
                 (plist-get entry :run-id)
                 (error-message-string
                  ;; EMITS is (status-emit payload-emit); prefer the payload
                  ;; condition when both callbacks failed, matching today's
                  ;; single warning about the delivery consumer specifically.
                  (plist-get (car (last failed)) :condition)))
         :warning)))))

(defun superchat-runtime--deliver-terminal (entry sink-status payload-slot payload)
  "Emit ENTRY's terminal Sink SINK-STATUS then PAYLOAD-SLOT/PAYLOAD once.

Honors the `:input-core' stop-notification bracket; records `delivery.failed'
for any callback condition; returns a captured `quit' condition (from either
callback) for the caller to re-signal once completion hooks and quiesce have
run, or nil."
  (let* ((run-id (plist-get entry :run-id))
         (sink (plist-get entry :sink))
         (run (superchat-runtime-sink-run run-id))
         (controlled (superchat-runtime--controlled-p entry))
         (session-id (plist-get entry :session-id))
         status-emit payload-emit)
    (superchat-invariant-check 'exact-sink session-id run-id
                               (plist-get entry :sink-id) (superchat-sink-id sink))
    (when (plist-get entry :host-hash)
      (superchat-invariant-check
       'host-buffer-unchanged session-id run-id
       (plist-get entry :buffer) (plist-get entry :host-hash)
       ;; Evaluated only when the bytes differ.
       (lambda ()
         (cl-some (lambda (event)
                    (and (equal run-id (plist-get event :run-id))
                         (equal "edit.applied" (plist-get event :type))))
                  (superchat-tape-replay-events session-id)))))
    (when controlled
      (superchat-runtime--update run-id :input-delivery-entered t))
    ;; Sink predicates and callbacks are public reentry boundaries.  A terminal
    ;; callback belongs to the Run which produced it, not to whichever Scope
    ;; happens to be dynamically active when an async provider returns.
    (let ((superchat-execution-scope
           (superchat-runtime--execution-scope entry)))
      (unwind-protect
          (progn
            (setq status-emit
                  (superchat-sink-emit sink 'status run sink-status :capture-quit t))
            (setq payload-emit
                  (superchat-sink-emit sink payload-slot run payload :capture-quit t)))
        (when controlled
          (superchat-runtime--update run-id :input-delivery-entered nil)
          (superchat-runtime--input-notify-stop run-id))))
    (superchat-runtime--record-delivery-failed entry sink (list status-emit payload-emit))
    (or (superchat-runtime--quit-condition status-emit)
        (superchat-runtime--quit-condition payload-emit))))

(defun superchat-runtime--terminal-sink-args (status response tape-id)
  "Map internal terminal STATUS/RESPONSE/TAPE-ID to a Sink status/slot/payload."
  (pcase status
    ('completed
     (list 'ready 'final (list :text (or response "") :tape-id tape-id)))
    ('cancelled
     (list 'stopped 'error
           (list :status 'cancelled :text (or response "") :reason "user" :tape-id tape-id)))
    (_
     (list 'error 'error
           (list :status status :text (or response "") :reason nil :tape-id tape-id)))))

(defun superchat-runtime--run-turn-hooks (entry)
  "Run post-response Turn hooks for ENTRY exactly once."
  (let* ((turn (plist-get entry :turn))
         (buffer (superchat-runtime--entry-buffer entry))
         (name (or (and (buffer-live-p buffer) (buffer-name buffer))
                   (plist-get entry :buffer-name)))
         (superchat-buffer-name name))
    (unless (plist-get entry :turn-hooks-run)
      ;; Publish before the listener can synchronously cancel this Run.
      (setq entry (superchat-runtime--update (plist-get entry :run-id) :turn-hooks-run t))
      (cl-labels
          ((run-hooks ()
             (superchat-emit (superchat-runtime--execution-scope entry) 'run-sealing
                             (superchat-runtime-sink-run (plist-get entry :run-id)) turn)))
        (if (buffer-live-p buffer)
            (with-current-buffer buffer (run-hooks))
          (run-hooks)))
      (setq entry
            (superchat-runtime--update
             (plist-get entry :run-id) :turn-hooks-run t)))
    entry))

(defun superchat-runtime--complete-turn
    (entry status causation-id &optional unaudited event-id)
  "Complete ENTRY's Turn once with STATUS caused by CAUSATION-ID.
When UNAUDITED is non-nil, mark only the in-memory lifecycle because Tape
persistence has already failed; do not invent a `turn.completed' event.
EVENT-ID, when non-nil, is the pre-minted stable identity of this Turn seal."
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
               causation-id nil event-id))
             (event-id (plist-get record :event-id)))
        (setq entry
              (superchat-runtime--update
               (plist-get entry :run-id)
               :turn-completed t
               :causation-id event-id)))))
  entry)

(defun superchat-runtime--audit-incomplete
    (run-id err response &optional response-storage-id no-recovery)
  "Record audit failure without ending an outer input-control finish early."
  (superchat-runtime--input-finish-scope
   run-id (lambda ()
            (superchat-runtime--audit-incomplete-body
             run-id err response response-storage-id no-recovery))))

(defun superchat-runtime--audit-incomplete-body
    (run-id err response &optional response-storage-id no-recovery)
  "Mark RUN-ID audit-incomplete after ERR and deliver its durable result.
When RESPONSE-STORAGE-ID is non-nil, RESPONSE is already an immutable Tape
Entry and must be delivered unchanged with that identity.  The audit warning
is then a separate system material row and UI status.  When no response row
exists (for example, response persistence itself failed), the warning is the
new assistant material delivered without an existing identity.

When NO-RECOVERY is non-nil, Tape could not be read well enough to establish
that a Run terminal is absent.  The local audit result remains visible and is
quiesced, but no guessed audit recovery pair is queued."
  (let* ((current (superchat-runtime--entry run-id))
         (children (plist-get current :children))
         (recovery-error
          (unless no-recovery
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
              (error (error-message-string queue-error)))))
         (error-text
          (cond
           (no-recovery
            (format "%s; Tape terminal could not be confirmed; no recovery seal queued"
                    (error-message-string err)))
           (recovery-error
            (format "%s; recovery journal failed: %s"
                    (error-message-string err) recovery-error))
           (t (error-message-string err))))
         (entry (superchat-runtime--update
                 run-id
                 :status 'audit-incomplete
                 :context-recovery nil
                 :completed-at (float-time)
                 :steer-queue nil
                 :undelivered-steer-count
                 (length (plist-get current :steer-queue))
                 :error error-text))
         (audit-warning
          "[Audit incomplete: Run could not be fully closed; see *Messages* for details.]")
         (warning
          (format "%s\n\n[Audit incomplete: Run could not be fully closed; see *Messages* for details.]"
                  (or response ""))))
    (message "Superchat Run %s audit incomplete: %s" run-id error-text)
    (let (quit-condition)
      (unwind-protect
          (progn
            (setq entry
                  (superchat-runtime--complete-turn
                   entry 'audit-incomplete nil t))
            (when response-storage-id
              ;; Keep the warning separate from the immutable response Entry.
              ;; This is material rather than a Runtime event so it receives
              ;; its own Tape identity and remains visible after reopening.
              (ignore-errors
                (superchat-tape-append-material
                 (plist-get entry :session-id)
                 "system"
                 audit-warning
                 :topic "audit-incomplete")))
            (setq quit-condition
                  (superchat-runtime--deliver-terminal
                   entry 'error 'error
                   (list :status 'audit-incomplete
                         :text (if response-storage-id (or response "") warning)
                         :reason nil :tape-id response-storage-id)))
            (superchat-runtime--run-completion-hooks entry))
        (superchat-runtime--quiesce entry)
        ;; §7.4: same eventual cascade as the ordinary finish path.
        (when children
          (run-at-time 0 nil #'superchat-runtime--cancel-children
                       run-id children)))
      (when quit-condition
        (signal (car quit-condition) (cdr quit-condition))))))

(defun superchat-runtime--tape-event-status (event)
  "Return EVENT's persisted payload status as a symbol when possible."
  (let ((status (plist-get (plist-get event :payload) :status)))
    (if (stringp status) (intern status) status)))

(defun superchat-runtime--ordinary-terminal-status-of (event)
  "Return EVENT's ordinary terminal status, or nil for any other event."
  (let ((status (superchat-runtime--tape-event-status event)))
    (and (memq status '(completed cancelled failed timed-out))
         (equal (superchat-runtime--terminal-event-type status)
                (plist-get event :type))
         status)))

(defun superchat-runtime--matching-terminal-seal-p
    (entry status event-id event)
  "Return non-nil when EVENT is ENTRY's exact ordinary STATUS seal."
  (and event
       (equal event-id (plist-get event :event-id))
       (equal (plist-get entry :session-id) (plist-get event :session-id))
       (equal (plist-get entry :run-id) (plist-get event :run-id))
       (equal (plist-get entry :turn-id) (plist-get event :turn-id))
       (eq status (superchat-runtime--ordinary-terminal-status-of event))))

(defun superchat-runtime--matching-turn-seal-p
    (entry status causation-id event &optional event-id)
  "Return non-nil when EVENT is ENTRY's matching ordinary Turn seal."
  (and event
       (or (null event-id) (equal event-id (plist-get event :event-id)))
       (equal "turn.completed" (plist-get event :type))
       (equal (plist-get entry :session-id) (plist-get event :session-id))
       (equal (plist-get entry :run-id) (plist-get event :run-id))
       (equal (plist-get entry :turn-id) (plist-get event :turn-id))
       (equal causation-id (plist-get event :causation-id))
       (eq status (superchat-runtime--tape-event-status event))))

(defun superchat-runtime--record-finish-error (entry err &optional note)
  "Publish a post-seal ERR on ENTRY without changing its terminal status."
  (let* ((run-id (plist-get entry :run-id))
         (detail (or note
                     (format "post-seal completion failure: %s"
                             (error-message-string err))))
         (text (string-join (delq nil (list (plist-get entry :error) detail)) "; "))
         (updated (superchat-runtime--update run-id :error text)))
    (message "Superchat Run %s %s" run-id text)
    updated))

(cl-defun superchat-runtime--finish-tail
    (entry status response tape-id &key sink-lost-p children)
  "Deliver and release an already-sealed ordinary ENTRY exactly once.

Failures here are visible on the public Run but cannot earn another terminal
fact.  This is shared by the uninterrupted finish and the lost-receipt repair,
so both retain the same Sink, completion-hook, quiesce, child, and quit order."
  ;; `--quiesce' deliberately drops the process-private borrowed Scope.  Keep
  ;; this lexical snapshot only long enough for the final diagnostic Event:
  ;; the historical Run entry must not retain a caller Scope after it is
  ;; terminal, while `closure-shadow' still belongs to the Run that produced
  ;; it rather than to global execution.
  (let ((run-id (plist-get entry :run-id))
        (execution-scope (superchat-runtime--execution-scope entry))
        quit-condition)
    (unwind-protect
        (condition-case tail-error
            (progn
              (setq quit-condition
                    (if sink-lost-p
                        (superchat-runtime--deliver-terminal
                         entry 'orphaned 'orphaned 'sink-lost)
                      (pcase-let ((`(,sink-status ,payload-slot ,payload)
                                   (superchat-runtime--terminal-sink-args
                                    status response tape-id)))
                        (superchat-runtime--deliver-terminal
                         entry sink-status payload-slot payload))))
              (superchat-runtime--run-completion-hooks entry))
          (error
           (setq entry
                 (superchat-runtime--record-finish-error
                  (or (superchat-runtime--entry run-id) entry) tail-error))))
      (let ((latest (or (superchat-runtime--entry run-id) entry)))
        (unless (plist-get latest :quiesced)
          (condition-case cleanup-error
              (superchat-runtime--quiesce latest)
            (error
             (setq entry
                   (superchat-runtime--record-finish-error
                    (or (superchat-runtime--entry run-id) latest)
                    cleanup-error)))))
        ;; §7.4: defer descendants until the parent has released its finish
        ;; scope.  This helper runs once per owner, so no extra local flag is
        ;; needed to protect the schedule from post-seal recovery.
        (when children
          (condition-case child-error
              (run-at-time 0 nil #'superchat-runtime--cancel-children
                           run-id children)
            (error
             (setq entry
                   (superchat-runtime--record-finish-error
                    (or (superchat-runtime--entry run-id) latest)
                    child-error))))))
      ;; Shadow is diagnostic only: it must never retroactively alter a closed
      ;; Run or prevent the captured consumer quit from being re-signalled.
      (ignore-errors
        (superchat-runtime--run-closure-shadow
         (or (superchat-runtime--entry run-id) entry) execution-scope)))
    (when quit-condition
      (signal (car quit-condition) (cdr quit-condition)))
    (superchat-runtime--entry run-id)))

(cl-defun superchat-runtime--finish-postseal-failure
    (run-id status response err
            &key response-storage-id terminal-id terminal-record turn-event-id
            sink-lost-p children)
  "Finish cleanup after a known ordinary Run seal's later failure.

The durable Run fact remains STATUS.  A missing Turn can use only the existing
per-Run journal's single matching tail; unreadable Run/Turn state never
becomes a guessed recovery seal.  The shared terminal tail owns one Sink
delivery, completion-hook pass, and cleanup after either finish path."
  (let* ((current (superchat-runtime--entry run-id))
         (entry
          (and current
               (superchat-runtime--update
                run-id
                :status status
                :context-recovery nil
                :causation-id terminal-id
                :completed-at (or (plist-get current :completed-at) (float-time))
                :finish-seal-id terminal-id)))
         (entry (and entry (superchat-runtime--record-finish-error entry err)))
         (tape-id (or response-storage-id (plist-get terminal-record :storage-id)))
         turn-note
         turn-causation-id)
    (when entry
      ;; A direct Turn receipt failure can have inserted its row already.  Do
      ;; not retry it or fabricate an audit Run; first establish the three-way
      ;; state, then journal only a proved-absent ordinary tail.
      (condition-case turn-error
          (progn
            (let* ((exact-state
                    (and turn-event-id
                         (superchat-tape--event-state
                          (list :session-id (plist-get entry :session-id)
                                :event-id turn-event-id))))
                   ;; An exact receipt is enough unless it is readable absence.
                   ;; In that one case a full Run/Turn check prevents a second
                   ;; Turn seal with a different identity.
                   (turn-state
                    (if (or (null exact-state)
                            (eq 'absent (plist-get exact-state :state)))
                        (superchat-tape--turn-terminal-state
                         (plist-get entry :session-id)
                         (plist-get entry :run-id)
                         (plist-get entry :turn-id))
                      exact-state))
                   (state (plist-get turn-state :state))
                   (persisted (plist-get turn-state :event)))
              (pcase state
                ('present
                 (if (superchat-runtime--matching-turn-seal-p
                      entry status terminal-id persisted turn-event-id)
                     (setq turn-causation-id (plist-get persisted :event-id))
                   (setq turn-note
                         "existing Turn seal does not match the confirmed Run seal")))
                ('absent
                 (let* ((tail-id (or turn-event-id
                                     (superchat-tape--new-event-id)))
                        (tail
                         (plist-put
                          (superchat-runtime--event
                           entry "turn.completed" (list :status status)
                           (format "turn.completed: %s" (plist-get entry :turn-id))
                           terminal-id)
                          :event-id tail-id)))
                   ;; Queuing preserves a possible missing Turn suffix; it is
                   ;; not a durable Tape fact yet.  In particular, terminal
                   ;; Sink code may append `delivery.failed' before replay
                   ;; writes this sidecar.  Keep that intervening event caused
                   ;; by the confirmed Run seal, rather than by TAIL-ID which
                   ;; does not exist in Tape at this point.
                   (superchat-tape-queue-recovery-events (list tail))))
                ('unknown
                 (setq turn-note
                       "Tape Turn state is unreadable; no recovery Turn seal was guessed"))))
            ;; This is deliberately the existing unaudited completion path rather
            ;; than a direct `:turn-completed' update.  In particular, a
            ;; cancellation Run INSERT can lose its receipt before normal flow
            ;; reaches `run-sealing'; `--complete-turn' owns that exactly-once
            ;; hook and the local no-fabrication completion mark.
            (setq entry
                  (superchat-runtime--complete-turn
                   entry status (or turn-causation-id terminal-id) t))
            (when turn-causation-id
              (setq entry
                    (superchat-runtime--update
                     run-id :causation-id turn-causation-id))))
        (error
         ;; A sidecar write failure must not reopen or rewrite the confirmed
         ;; Run.  Complete local ownership so Sink delivery and resource
         ;; release still happen, while leaving the missing durable fact open.
         (setq turn-note
               (format "Turn recovery failed: %s"
                       (error-message-string turn-error)))
         (setq entry
               (superchat-runtime--complete-turn
                (or (superchat-runtime--entry run-id) entry)
                status terminal-id t))))
      (when turn-note
        (setq entry (superchat-runtime--record-finish-error entry err turn-note)))
      (setq entry
            (superchat-runtime--finish-tail
             entry status response tape-id
             :sink-lost-p sink-lost-p :children children)))
    (superchat-runtime--entry run-id)))

(defun superchat-runtime--terminal-event-type (status)
  "Return the Run terminal event type for STATUS."
  (pcase status
    ('completed "run.completed")
    ('cancelled "run.cancelled")
    (_ "run.failed")))

(defun superchat-runtime--finish-superseded-entry (run-id)
  "Return RUN-ID's terminal entry when a nested finish has sealed it.

Finish paths call public Sink predicates and Runtime Event listeners.  Either
may synchronously reenter Runtime and terminally seal RUN-ID.  An older
caller must then stop rather than write its stale nonterminal ENTRY back over
that seal."
  (let ((current (superchat-runtime--entry run-id)))
    (and current
         (superchat-runtime--terminal-p (plist-get current :status))
         current)))

(defun superchat-runtime--finish (run-id status response)
  "Finish RUN-ID once with terminal STATUS and RESPONSE."
  (let* ((entry (superchat-runtime--entry run-id))
         (scope-retired-p
          (and entry
               (superchat-execution-scope-retired-p
                (superchat-runtime--execution-scope entry)))))
    (cond
     ((and entry (not (eq status 'cancelled))
           (not (superchat-runtime--terminal-p (plist-get entry :status)))
           (or scope-retired-p
               (not (superchat-runtime--transport-current-p entry))))
      (superchat-runtime-cancel
       run-id
       (if scope-retired-p
           "[Execution Scope retired]"
         "[Model transport withdrawn]")))
     ((plist-get entry :owner-startup-active)
      ;; Startup may be inside an audited append.  Revoke permission now,
      ;; but never recursively seal and then let its outer let* revive us.
      (unless (plist-get entry :owner-startup-terminal)
        (superchat-runtime--update run-id :owner-startup-terminal (list status response)))
      nil)
     ((and (superchat-runtime--controlled-p entry)
           (> (or (plist-get entry :input-finish-depth) 0) 0)) nil)
     ((and (plist-get entry :input-core)
           (or (plist-get entry :input-question)
               (> (or (plist-get entry :input-settlement-depth) 0) 0)))
      (superchat-runtime--input-terminal-intent run-id status response)
      (superchat-runtime--input-drain run-id))
     (t
      (superchat-runtime--input-finish-scope
       run-id (lambda () (superchat-runtime--finish-body run-id status response)))))))

(cl-defun superchat-runtime--finish-body (run-id status response)
  "Commit frozen STATUS/RESPONSE through the existing Runtime finish order.

Every public callback boundary rechecks whether a nested finish has already
sealed RUN-ID.  That nested terminal fact wins; this older caller must not
restore its stale `delivering' entry or append another terminal event."
  (let* ((entry (superchat-runtime--entry run-id))
         (response-storage-id nil)
         ;; Set only after this invocation has durably sealed the Run and
         ;; published that exact seal to the registry.  The error handler uses
         ;; it to distinguish its own incomplete post-seal cleanup from an
         ;; entirely nested finish that already took over the Run.
         (finish-seal-id nil)
         ;; Stable seal identities cross SQLite's INSERT/receipt boundary.
         ;; All delivery and cleanup after a seal belongs to the shared tail.
         (terminal-id nil)
         (terminal-record nil)
         (turn-event-id nil)
         (children nil)
         (sink-lost-p nil))
    (when (and entry
               (not (superchat-runtime--terminal-p
                     (plist-get entry :status))))
      (superchat-runtime--update run-id :terminalizing t)
      (condition-case err
          (cl-macrolet
              ((return-if-superseded ()
                 `(when-let* ((sealed
                               (superchat-runtime--finish-superseded-entry run-id)))
                    (cl-return-from superchat-runtime--finish-body sealed))))
            (let (cancelled-p response-record response-id
                               terminal-type cancel-record tape-id)
              (setq entry (superchat-runtime--entry run-id)
                    children (plist-get entry :children)
                    ;; `live-p' is public user code and may synchronously
                    ;; unmount a transport, which cancels this Run.
                    sink-lost-p
                    (let ((superchat-execution-scope
                           (superchat-runtime--execution-scope entry)))
                      (not (superchat-sink-live-p (plist-get entry :sink)))))
              (return-if-superseded)
              (when sink-lost-p
                (setq status 'cancelled
                      response "[Runtime Sink lost]"))
              (setq cancelled-p (eq status 'cancelled)
                    entry (superchat-runtime--update run-id :status 'delivering))
              (unless cancelled-p
                (setq response-record
                      (superchat-runtime--append-event
                       entry "response.completed"
                       (append (list :status status)
                               (superchat-runtime--content-metadata response))
                       (or response "") (plist-get entry :causation-id)))
                (setq response-storage-id (plist-get response-record :storage-id))
                (return-if-superseded))
              (setq response-id
                    (or (plist-get response-record :event-id)
                        (plist-get entry :causation-id))
                    entry
                    (superchat-runtime--mark-undelivered-steers entry response-id))
              (return-if-superseded)
              (setf (superchat-turn-llm-result (plist-get entry :turn))
                    (or response ""))
              (when (memq status '(failed timed-out))
                (setf (superchat-turn-error (plist-get entry :turn)) response))
              (setq terminal-type (superchat-runtime--terminal-event-type status))
              (when cancelled-p
                (superchat-runtime--guard-terminal entry status)
                (return-if-superseded)
                ;; Mint before append.  SQLite INSERT may commit before its
                ;; last-rowid receipt fails; the handler can then ask Tape for
                ;; this exact identity rather than guessing that no seal ran.
                (setq terminal-id (superchat-tape--new-event-id)
                      cancel-record
                      (superchat-runtime--append-event
                       entry terminal-type
                       (if sink-lost-p
                           (list :status status :reason "sink-lost")
                         (list :status status))
                       (format "%s: %s" terminal-type run-id)
                       response-id nil terminal-id))
                (return-if-superseded)
                ;; The cancellation seal is durable before listeners run, so
                ;; a nested cancellation sees a terminal Run and is a no-op.
                (setq finish-seal-id terminal-id
                      entry
                      (superchat-runtime--update
                       run-id :status status
                       :causation-id finish-seal-id
                       :finish-seal-id finish-seal-id)))
              (setq entry (superchat-runtime--run-turn-hooks entry))
              ;; A cancellation record above is this invocation's own seal;
              ;; normal completion has no seal yet, so a nested one wins.
              (if cancel-record
                  (setq entry (superchat-runtime--entry run-id))
                (return-if-superseded))
              (unless cancel-record
                (superchat-runtime--guard-terminal entry status)
                (return-if-superseded)
                ;; As above, the stable identity crosses the append/receipt
                ;; boundary so a lost receipt is three-state, not absent.
                (setq terminal-id (superchat-tape--new-event-id)
                      terminal-record
                      (superchat-runtime--append-event
                       entry terminal-type (list :status status)
                       (format "%s: %s" terminal-type run-id)
                       response-id nil terminal-id))
                (return-if-superseded))
              (setq terminal-record (or terminal-record cancel-record))
              ;; Publish an ordinary terminal seal before the invariant seam,
              ;; just as cancellation publishes before `run-sealing'.  Once
              ;; this is visible, nested finish calls are no-ops rather than
              ;; competing durable seals.
              (unless cancel-record
                (setq finish-seal-id terminal-id
                      entry
                      (superchat-runtime--update
                       run-id
                       :status status
                       :context-recovery nil
                       :causation-id terminal-id
                       :completed-at (float-time)
                       :finish-seal-id finish-seal-id)))
              (superchat-invariant-check
               'terminal-on-tape (plist-get entry :session-id) run-id
               terminal-record run-id terminal-type)
              ;; Cancellation already published its status before
              ;; `run-sealing'; finish its ordinary lifecycle fields only
              ;; after that hook returns.  An error here is our own post-seal
              ;; cleanup failure, not proof that a nested finish completed it.
              (when cancel-record
                (setq entry
                      (superchat-runtime--update
                       run-id
                       :status status
                       :context-recovery nil
                       :causation-id terminal-id
                       :completed-at (float-time)
                       :finish-seal-id finish-seal-id)))
              (setq turn-event-id (superchat-tape--new-event-id)
                    entry
                    (superchat-runtime--complete-turn
                     entry status terminal-id nil turn-event-id)
                    tape-id (or response-storage-id
                                (plist-get cancel-record :storage-id)))
              ;; Delivery observes only after, and cannot undo, durable facts.
              (superchat-runtime--finish-tail
               entry status response tape-id
               :sink-lost-p sink-lost-p :children children)))
        (error
         (let ((current (superchat-runtime--entry run-id)))
           (cl-labels
               ((finish-known (known-status known-record known-id)
                  (superchat-runtime--finish-postseal-failure
                   run-id known-status response err
                   :response-storage-id response-storage-id
                   :terminal-id known-id :terminal-record known-record
                   :turn-event-id turn-event-id :sink-lost-p sink-lost-p
                   :children children))
                (audit-unknown ()
                  ;; A failed read cannot prove that this Run lacks a durable
                  ;; terminal.  Keep the local lifecycle observable and
                  ;; release it, but never queue a speculative audit pair.
                  (superchat-runtime--audit-incomplete
                   run-id err response response-storage-id t))
                (inspect-run ()
                  (let ((run-state
                         (and current
                              (superchat-tape--run-terminal-state
                               (plist-get current :session-id) run-id))))
                    (pcase (plist-get run-state :state)
                      ('present
                       (let* ((event (plist-get run-state :event))
                              (known-status
                               (superchat-runtime--ordinary-terminal-status-of
                                event)))
                         (if known-status
                             (finish-known known-status event
                                           (plist-get event :event-id))
                           (audit-unknown))))
                      ('absent
                       ;; This is the one path that retains the existing
                       ;; explicit audit pair: a readable Tape established
                       ;; that no Run terminal was written.
                       (superchat-runtime--audit-incomplete
                        run-id err response response-storage-id))
                      (_ (audit-unknown))))))
             (cond
              ;; Our own successfully received seal is authoritative even if
              ;; the next Turn, delivery, hook, or cleanup action errors.
              ((and finish-seal-id current
                    (equal finish-seal-id (plist-get current :finish-seal-id)))
               (finish-known status terminal-record finish-seal-id))
              ;; A public callback may have completed the Run through its own
              ;; Runtime path.  It published its terminal status before this
              ;; older frame resumes, so this frame must not deliver or seal.
              ((and current
                    (superchat-runtime--terminal-p (plist-get current :status)))
               current)
              ;; A receipt failure after terminal INSERT has a pre-minted
              ;; identity.  Exact present/absent/unknown avoids the former
              ;; "append audit pair on any error" duplicate-terminal bug.
              (terminal-id
               (let ((terminal-state
                      (superchat-tape--event-state
                       (list :session-id (plist-get current :session-id)
                             :event-id terminal-id))))
                 (pcase (plist-get terminal-state :state)
                   ('present
                    (let ((event (plist-get terminal-state :event)))
                      (if (superchat-runtime--matching-terminal-seal-p
                           current status terminal-id event)
                          (finish-known status event terminal-id)
                        (inspect-run))))
                   ('absent (inspect-run))
                   ('unknown (audit-unknown)))))
              ;; Guard/response failures before a terminal append still need
              ;; the same read-backed distinction; do not convert unreadable
              ;; storage into a durable audit guess.
              (t (inspect-run))))))))))

(defun superchat-runtime--deadline-expired (run-id)
  "Finish RUN-ID as `timed-out' when its Runtime deadline fires.

Timer callbacks can outlive their Run entry or race a terminal callback, so
all lookup and finish work is deliberately best-effort."
  (condition-case nil
      (let ((entry (superchat-runtime--entry run-id)))
        (when (and entry
                   (not (superchat-runtime--terminal-p
                         (plist-get entry :status))))
          ;; Finish first: late provider callbacks then observe the terminal
          ;; Runtime state.  Cancellation is only a best-effort resource hint.
          (superchat-runtime--finish
           run-id 'timed-out
           (format "[Runtime deadline exceeded; last phase: %s]"
                   (or (plist-get entry :model-phase) 'unknown)))))
    (error nil)))

(defun superchat-runtime--arm-deadline (entry)
  "Arm the wall-clock deadline stored in ENTRY and return its current value."
  (let ((deadline (plist-get entry :deadline)))
    (if (and (numberp deadline) (> deadline 0))
        (let ((timer (timer-create))
              (run-id (plist-get entry :run-id))
              committed current)
          (unwind-protect
              (progn
                (timer-set-time timer (max deadline (float-time)))
                (timer-set-function timer #'superchat-runtime--deadline-expired
                                    (list run-id))
                (superchat-runtime--update run-id :deadline-timer timer)
                (timer-activate timer)
                (setq current (superchat-runtime--entry run-id)
                      committed (eq timer (plist-get current :deadline-timer)))
                current)
            (unless committed
              (let ((inhibit-quit t))
                (cancel-timer timer)
                (let ((current (gethash run-id superchat-runtime--runs)))
                  (when (eq timer (plist-get current :deadline-timer))
                    (setf (plist-get current :deadline-timer) nil)))))))
      entry)))

(defun superchat-runtime--cancel-timeout (entry)
  "Cancel the transport and Runtime deadline timers owned by ENTRY."
  (when entry
    (let ((deadline-timer (plist-get entry :deadline-timer)))
      (when (timerp deadline-timer)
        ;; A broken deadline adapter must not prevent cancellation of the
        ;; independently owned per-attempt transport timers below.
        (ignore-errors
          (cancel-timer deadline-timer)))
      (when (and (plist-member entry :deadline-timer)
                 (superchat-runtime--entry (plist-get entry :run-id)))
        (superchat-runtime--update
         (plist-get entry :run-id) :deadline-timer nil)))
    (let ((transport-timer-owner
           (plist-get entry :transport-timer-owner)))
      (if (and transport-timer-owner
               (fboundp 'superchat-llm--transport-timer-owner-cancel))
          (ignore-errors
            (superchat-llm--transport-timer-owner-cancel
             transport-timer-owner))
        ;; Compatibility for callers that provide a transport double instead
        ;; of registering a per-call owner.
        (let ((buffer (superchat-runtime--entry-buffer entry)))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              (when (and (boundp 'superchat--active-timeout-timer)
                         (boundp 'superchat--active-timeout-timer-owner)
                         (boundp 'superchat--active-run-id)
                         (null superchat--active-timeout-timer-owner)
                         (equal (plist-get entry :run-id)
                                superchat--active-run-id))
                (when (timerp superchat--active-timeout-timer)
                  (cancel-timer superchat--active-timeout-timer))
                (setq superchat--active-timeout-timer nil
                      superchat--active-timeout-timer-owner nil)))))))))

(defvar superchat-runtime--transport-stopping nil
  "Non-nil while Runtime cancels a withdrawn provider's Runs.")

(defun superchat-runtime--transport-current-p (entry)
  "Whether ENTRY still owns the exact active transport provider."
  (and (not superchat-runtime--transport-stopping)
       (not (plist-get entry :transport-revoked))
       (plist-get entry :transport-provider-id)
       (eq (plist-get entry :transport-provider-id)
           (plist-get (superchat-service-get nil 'model-transport) :id))))

(defun superchat-runtime-retire-transport (provider-id)
  "Revoke PROVIDER-ID's old Runs together, then cancel each through Runtime.
Reentrant callbacks cannot start new work or let another old Run complete.
Consumer error/quit is diagnosed per Run and cannot interrupt the sweep.
No buffer ownership is acquired and no unfinished Effect is fabricated."
  (let ((superchat-runtime--transport-stopping t) runs)
    (maphash
     (lambda (id entry)
       (when (and (eq provider-id (plist-get entry :transport-provider-id))
                  (not (superchat-runtime--terminal-p (plist-get entry :status))))
         (superchat-runtime--update id :transport-revoked t)
         (push id runs)))
     superchat-runtime--runs)
    (dolist (id runs)
      (condition-case condition
          (superchat-runtime-cancel id "[Model transport withdrawn]")
        ((error quit)
         (superchat-stage-record-fault 'transport-stop id 'driver condition))))))

(defun superchat-runtime-retire-scope (scope &optional reason)
  "Retire execution through explicit non-global SCOPE and descendants.

The Scope latch is set before this function calls any cancellation callback.
Runtime then snapshots matching nonterminal Runs from its existing registry,
so reentrant terminal delivery cannot let a sibling old Run continue.  This
does not dispose SCOPE, unmount registrations, close a buffer, or retract an
external Effect already entered.  REASON is diagnostic text for consumers."
  (superchat-execution-scope-retire scope)
  (let ((response
         (if reason
             (format "[Execution Scope retired: %s]" reason)
           "[Execution Scope retired]"))
        runs queue-sessions)
    ;; No callback runs while this snapshot is built.  Match Scope identity in
    ;; the borrowed execution ancestry, never a display name or Session id.
    (maphash
     (lambda (run-id entry)
       (when (and (or (not (superchat-runtime--terminal-p
                            (plist-get entry :status)))
                      ;; Runtime cancellation deliberately handles opted-in
                      ;; control before its terminal test, so a terminal Sink
                      ;; still retaining control must receive this scope stop.
                      (and (superchat-runtime--controlled-p entry)
                           (plist-get entry :input-control-active)))
                  (memq scope
                        (superchat-scope-chain
                         (superchat-runtime--execution-scope entry))))
         (push run-id runs)))
     superchat-runtime--runs)
    ;; The FIFO remains the only queue.  Scheduling these sessions later lets
    ;; its normal dispatcher record a scope-retired drop before removal.
    (maphash
     (lambda (session-id queue)
       (when (cl-some
              (lambda (item)
                (memq scope
                      (superchat-scope-chain
                       (or (plist-get item :execution-scope)
                           superchat-scope-global))))
              queue)
         (cl-pushnew session-id queue-sessions :test #'equal)))
     superchat-runtime--session-queues)
    (dolist (run-id runs)
      (condition-case condition
          (superchat-runtime-cancel run-id response)
        ((error quit)
         ;; Consumer error/quit must not prevent the remaining old Runs from
         ;; being made terminal.  Keep the diagnostic in the existing ring.
         (condition-case nil
             (superchat-stage-record-fault
              'scope-retirement run-id 'driver condition)
           ((error quit) nil)))))
    ;; A transport gap leaves FIFO entries paused; the existing remount path
    ;; resumes them.  When available, dispatch only drops retired heads -- it
    ;; never creates a model request for this retirement operation itself.
    (dolist (session-id queue-sessions)
      (condition-case condition
          (superchat-runtime--schedule-dispatch-next session-id)
        ((error quit)
         (condition-case nil
             (superchat-stage-record-fault
              'scope-retirement session-id 'queue condition)
           ((error quit) nil)))))
    scope))

(defun superchat-runtime-cancel (run-id &optional response)
  "Cancel RUN-ID locally and ask llm.el to stop its latest request.

Cancellation is best-effort at the provider layer.  Runtime immediately stops
model continuation, so a provider that cannot abort cannot revive the Run or
start another model request.  This does not retract an already-entered tool:
its real late result may still account for its existing Tape Effect, without
reopening the Run or continuing the model loop.  RESPONSE customizes the
delivered reason."
  (let ((entry (superchat-runtime--entry run-id)))
    (cond
     ((null entry) nil)
     ((superchat-runtime--controlled-p entry)
      (when (plist-get entry :input-control-active)
        ;; Publish stop and detach the exact handle before abort or observers.
        ;; This permission fact does not rewrite a completed Tape seal.
        (unless (plist-get entry :stop-requested)
          (let ((request (plist-get entry :transport-request)))
            (superchat-runtime--update
             run-id :stop-requested (or response "[Stopped by user]")
             :transport-request nil)
            (superchat-runtime--notify-owner-stop run-id)
            (superchat-runtime--abort-transport request)))
        (unless (superchat-runtime--terminal-p (plist-get entry :status))
          (superchat-runtime--finish
           run-id 'cancelled (or response "[Stopped by user]")))
        t))
     ((superchat-runtime--terminal-p (plist-get entry :status)) nil)
     (t
      ;; Quiescence releases the captured request after publishing terminal
      ;; state, so synchronous cancellation callbacks cannot win this race.
      (superchat-runtime--finish
       run-id 'cancelled (or response "[Cancelled by user]"))
      t))))

(defun superchat-runtime--cancel-children (_parent-run-id children)
  "Cancel each still-live Run in CHILDREN after _PARENT-RUN-ID terminalized.

Runs from a timer-0 callback (§7.1): no guard is held while cancelling, each
child's own terminalization schedules its own children the same way, and no
ordering among CHILDREN is promised.  `superchat-runtime-cancel' already
no-ops for an unknown or already-terminal Run, so an already-finished child
is left untouched rather than re-terminated."
  (dolist (child-run-id children)
    (superchat-runtime-cancel child-run-id "[Cancelled with parent Run]")))

(defun superchat-runtime--validate-request (request)
  "Validate prepared Runtime REQUEST."
  (unless (listp request)
    (signal 'wrong-type-argument (list 'listp request)))
  (dolist (key '(:accepted-function :owner-stop-function))
    (unless (or (null (plist-get request key)) (functionp (plist-get request key)))
      (error "%s must be callable or nil" key)))
  (when-let* ((config (plist-get request :input-core)))
    ;; Internal NONSHIPPING fixture seam.  Validate before any Run/Tape/host
    ;; publication; no global switch and no implicit enabling by tool name.
    (unless (and (superchat-runtime--plist-p config)
                 (eq (plist-get config :enabled) t)
                 (cl-loop for (key _value) on config by #'cddr
                          always (memq key '(:enabled :stop-consumer :tool-name :presenter
                                           :operation :eligible-function))))
      (error "Invalid internal :input-core configuration"))
    (unless (or (null (plist-get config :stop-consumer))
                (functionp (plist-get config :stop-consumer)))
      (error "Internal input stop consumer must be callable or nil"))
    (when (plist-member config :tool-name)
      (unless (and (stringp (plist-get config :tool-name))
                   (not (string-empty-p (plist-get config :tool-name)))
                   (functionp (plist-get config :presenter)))
        (error "Internal input tool requires an exact name and presenter")))
    (when (and (plist-member config :presenter)
               (not (functionp (plist-get config :presenter))))
      (error "Internal input presenter must be callable"))
    (when (plist-member config :operation)
      (unless (and (eq (plist-get config :operation) 'ask-user)
                   (equal (plist-get config :tool-name) "ask_user")
                   (functionp (plist-get config :eligible-function)))
        (error "Trusted input requires ask-user and an eligibility adapter")))
    (when (and (plist-member config :eligible-function)
               (not (eq (plist-get config :operation) 'ask-user)))
      (error "Input eligibility requires the trusted operation")))
  (unless (and (stringp (plist-get request :prompt))
               (not (string-empty-p
                     (string-trim (plist-get request :prompt)))))
    (error "Runtime request requires a non-empty :prompt"))
  (when (plist-member request :session-id)
    (unless (and (stringp (plist-get request :session-id))
                 (not (string-empty-p
                       (plist-get request :session-id))))
      (error "Runtime :session-id must be a non-empty string")))
  (when (plist-member request :deadline)
    (let ((deadline (plist-get request :deadline)))
      (unless (or (null deadline)
                  (and (numberp deadline) (>= deadline 0)))
        (error "Runtime :deadline must be nil or a non-negative number of seconds"))))
  (when (plist-member request :max-tokens)
    (unless (and (integerp (plist-get request :max-tokens))
                 (> (plist-get request :max-tokens) 0))
      (error "Runtime :max-tokens must be a positive integer")))
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
  (when (plist-member request :context-envelope)
    (unless (superchat-runtime--plist-p
             (plist-get request :context-envelope))
      (error "Runtime :context-envelope must be a plist")))
  (when (and (plist-member request :context-envelope)
             (plist-member (plist-get request :context-envelope)
                           :experience-session-ids))
    (let ((session-ids
           (plist-get (plist-get request :context-envelope)
                      :experience-session-ids)))
      (unless (and (proper-list-p session-ids)
                   session-ids
                   (cl-every
                    (lambda (session-id)
                      (and (stringp session-id)
                           (not (string-empty-p session-id))))
                    session-ids)
                   (= (length session-ids)
                      (length (delete-dups (copy-sequence session-ids)))))
        (error "Runtime :experience-session-ids must be unique non-empty strings"))))
  (when (and (plist-member request :context-envelope)
             (plist-member (plist-get request :context-envelope)
                           :experience-action-policy))
    (let ((policy
           (plist-get (plist-get request :context-envelope)
                      :experience-action-policy)))
      (unless (superchat-runtime--plist-p policy)
        (error "Runtime :experience-action-policy must be a plist"))
      (let ((dependent-tools (plist-get policy :dependent-tools)))
        (unless (and (listp dependent-tools)
                     (cl-every
                      (lambda (tool)
                        (and (stringp tool)
                             (not (string-empty-p tool))))
                      dependent-tools))
          (error "Runtime :experience-action-policy dependent-tools must be non-empty strings")))
      (when (plist-member policy :kind)
        (unless (and (equal "workspace-revision-recovery"
                            (plist-get policy :kind))
                     (member (plist-get policy :phase)
                             '("awaiting-info" "awaiting-read" "satisfied")))
          (error "Unknown Runtime experience-action-policy kind or phase")))))
  (when-let* ((contract (plist-get request :evidence-contract)))
    (unless (equal contract "epistemic-learning-v0")
      (error "Unknown Runtime evidence contract: %s" contract))
    (let* ((envelope (plist-get request :context-envelope))
           (receipt (and (listp envelope)
                         (plist-get envelope :receipt))))
      (unless (and (superchat-runtime--plist-p envelope)
                   (equal "context-continuity-v0"
                          (plist-get envelope :contract))
                   (memq (plist-get envelope :status)
                         '(satisfied partial empty unknown blocked))
                   (plist-member envelope :receipt)
                   (consp receipt)
                   (superchat-runtime--plist-p receipt))
        (error "epistemic-learning-v0 requires a View context envelope")))
    (when (plist-member request :prepared-tools)
      (error "epistemic-learning-v0 does not support :prepared-tools"))
    (when (plist-member request :tool-wrapper)
      (error "epistemic-learning-v0 does not support :tool-wrapper"))
    (when (plist-get request :context-files)
      (error "epistemic-learning-v0 is text-only")))
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

(defun superchat-runtime--validate-host-admission (request)
  "Require REQUEST's exact borrowed editor host before any Runtime effect."
  (dolist (legacy '(:ephemeral-buffer :host-authority-mode))
    (when (plist-member request legacy)
      (error "Runtime no longer accepts legacy editor-host field %s" legacy)))
  (unless (plist-member request :buffer)
    (error "Runtime request requires an exact live :buffer"))
  (unless (buffer-live-p (plist-get request :buffer))
    (error "Runtime :buffer must be an exact live buffer"))
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

(defun superchat-runtime--prepare-input-request (request)
  "Detach and gate the trusted input pair without enabling any producer.
Explicit profiles retain precedence over the global tools switch."
  (let* ((request (copy-sequence request))
         (config (copy-sequence (plist-get request :input-core)))
         (tools (mapcar (lambda (name) (if (stringp name) (substring-no-properties name) name))
                        (if (listp (plist-get request :tools)) (plist-get request :tools) nil)))
         (selection (plist-get request :tools))
         (trusted (eq (plist-get config :operation) 'ask-user)))
    (when (plist-get config :tool-name)
      (setq config (plist-put config :tool-name
                              (substring-no-properties (plist-get config :tool-name)))))
    (when (and (member "ask_user" tools) (not trusted))
      (error "ask_user requires a supported paired input adapter"))
    (when trusted
      (when (listp selection) (setq request (plist-put request :tools tools)))
      (if (or (eq selection 'none)
              (and selection (not (member "ask_user" tools)))
              (and (null selection) (not (bound-and-true-p superchat-llm-tools-enabled))))
          (setq config nil)
        (unless (and (not (plist-member request :prepared-tools))
                     (null (plist-get request :tool-wrapper))
                     (null (plist-get request :parent-run-id))
                     (not (member (plist-get request :topology) '(subagent "subagent" team "team")))
                     (not (eq (plist-get request :attendance) 'unattended))
                     (funcall (plist-get config :eligible-function)))
          (error "Human input adapter is unavailable for this request"))))
    (if (plist-member request :input-core) (plist-put request :input-core config) request)))

(defun superchat-runtime-submit (request)
  "Submit prepared REQUEST and return an accepted Run handle.

REQUEST is a plist containing `:prompt' and a live exact `:buffer'.  Runtime
borrows that buffer object and never closes it.  Runtime supplies an ephemeral
Session ID when `:session-id' is absent.  Runtime owns the mutable execution
Turn, transport request, Tape lifecycle, and result delivery; callers retain
only the returned IDs and query status separately.

Optional `:accepted-function' receives a detached handle once after successful
unstopped startup, before model dispatch.  Its return value is not permission.
Optional `:owner-stop-function' receives fresh detached status immediately on
accepted stop, even inside terminal delivery; its error/quit is diagnostic.
Either option retains stop control through terminal consumers, without enabling
human input, minting an input token, or settling ordinary asynchronous tools.
Absent options preserve legacy behavior; result delivery retains three args."
  (superchat-runtime--validate-request request)
  ;; Capture producer callbacks before host admission can reenter/mutate its
  ;; Request.  Neither callback is a native success or a permission predicate.
  (let* ((execution-scope (superchat-execution-scope-resolve nil))
         (_scope-before-input
          (superchat-runtime--assert-execution-scope-active execution-scope))
         (request (superchat-runtime--prepare-input-request request))
         (_scope-after-input
          (superchat-runtime--assert-execution-scope-active execution-scope))
         (accepted-function (plist-get request :accepted-function))
         (owner-stop-function (plist-get request :owner-stop-function))
         (transport-provider-id (plist-get (superchat-service-get nil 'model-transport) :id))
         (_admission
          (progn
            (superchat-runtime--validate-host-admission request)
            (superchat-runtime--assert-execution-scope-active execution-scope)
            (unless (and transport-provider-id (not superchat-runtime--transport-stopping))
              (error "Superchat model-transport Service is unavailable"))))
         (session-id (or (plist-get request :session-id)
                         (superchat-runtime--new-id "session")))
         (turn (superchat-runtime--turn-from-request request session-id))
         (turn-id (superchat-turn-id turn))
         (run-id (superchat-runtime--new-id "run"))
         (actor-id (or (plist-get request :actor-id) "lead"))
         (parent-run-id (plist-get request :parent-run-id))
         ;; §7.4 attach: refused before any Tape row when the parent is
         ;; missing, terminal, already :terminalizing, or its execution Scope
         ;; has retired.
         (_parent-attach-validated
          (when parent-run-id
            (superchat-runtime--assert-parent-attach-active parent-run-id)))
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
         (evidence-contract (plist-get request :evidence-contract))
         (context-envelope (copy-tree (plist-get request :context-envelope)))
         (input-core
          (let ((config (copy-sequence (plist-get request :input-core))))
            (when-let* ((name (plist-get config :tool-name)))
              (setq config (plist-put config :tool-name (copy-sequence name))))
            config))
         (input-core-token (and input-core (make-symbol "input-core")))
         (input-operation
          (when (eq (plist-get input-core :operation) 'ask-user)
            (superchat-authority--make-human-input-operation run-id input-core-token)))
         (started-at (float-time))
         (deadline-seconds
          (if (plist-member request :deadline)
              (plist-get request :deadline)
            superchat-runtime-default-deadline))
         (buffer (plist-get request :buffer))
         (last-failure-event nil)
         (last-failure-info nil)
         ;; Built and checked before any registry publication: a dead Sink
         ;; at admission must signal with no Tape row and no accepted Run,
         ;; not enter the entry-published audit-incomplete recovery path.
         (sink
          (superchat-runtime--request-sink request))
         (_ (unless (superchat-sink-live-p sink)
              (error "Runtime Sink is not live at admission")))
         (_scope-after-sink
          (superchat-runtime--assert-execution-scope-active execution-scope))
         ;; Sink `live-p' is public admission code.  It may retire the parent
         ;; Scope while this child itself still runs through an open sibling.
         ;; Revalidate the exact parent before the child receives any Run row.
         (_parent-attach-after-sink
          (when parent-run-id
            (superchat-runtime--assert-parent-attach-active parent-run-id)))
         (publisher
          (lambda (buffer)
            ;; A public admission predicate may have retired this borrowed
            ;; Scope.  Do not publish a Run after its continuation latch shut.
            (superchat-runtime--assert-execution-scope-active execution-scope)
            ;; Keep the parent proof adjacent to registry publication too:
            ;; callers can only attach to the exact still-live, unretired
            ;; parent checked above, never merely to its Run ID.
            (when parent-run-id
              (superchat-runtime--assert-parent-attach-active parent-run-id))
            (unless (and (not superchat-runtime--transport-stopping)
                         (eq transport-provider-id (plist-get (superchat-service-get nil 'model-transport) :id)))
              (error "Model transport changed before Runtime acceptance"))
            (let ((entry
                   (list :transport-provider-id transport-provider-id
                         ;; This is the caller's borrowed Scope, not a Request
                         ;; field or a Run-owned Scope.  It stays private to the
                         ;; process registry so async lifecycle Events resume
                         ;; the same lookup layer without serializing its
                         ;; parent/children graph.
                         :execution-scope execution-scope
                         :session-id session-id
                         :turn-id turn-id
                         :run-id run-id
                         :actor-id actor-id
                         :profile-id profile-id
                         :model (or (plist-get request :target-model)
                                    "backend default")
                         :parent-run-id parent-run-id
                         :workspace-id (plist-get request :workspace-id)
                         :workspace-root (plist-get request :workspace-root)
                         :correlation-id correlation-id
                         :capability capability
                         :topology topology
                         :authority-policy authority-policy
                         :surface surface
                         :evidence-contract evidence-contract
                         :context-envelope context-envelope
                         :input-core input-core
                         :input-core-token input-core-token
                         :input-operation input-operation
                         :owner-control-p (and (or accepted-function owner-stop-function) t)
                         :accepted-function accepted-function
                         :owner-stop-function owner-stop-function
                         :owner-stop-notified nil
                         :owner-startup-active (and (or accepted-function owner-stop-function) t)
                         :input-control-active (and (or input-core accepted-function owner-stop-function) t)
                         :status 'accepted
                         :model-phase 'waiting-first-token
                         :started-at started-at
                         :transcript-owner
                         (or (plist-get request :transcript-owner) run-id)
                         :deadline
                         (and (numberp deadline-seconds)
                              (> deadline-seconds 0)
                              (+ started-at deadline-seconds))
                         :deadline-timer nil
                         :buffer buffer
                         :buffer-name (buffer-name buffer)
                         :delivery-function
                         (plist-get request :delivery-function)
                         :stream-function (plist-get request :stream-function)
                         :sink sink
                         :sink-id (superchat-sink-id sink)
                         :host-hash (plist-get request :host-hash)
                         :turn turn)))
              (superchat-runtime--publish-entry entry))))
         ;; Only the new owner option moves publication into protected
         ;; startup; retain the legacy no-option publication boundary.
         (entry (unless (or accepted-function owner-stop-function)
                  (funcall publisher buffer))))
    (condition-case err
        (catch 'superchat-owner-startup-stopped
        (let* ((entry (or entry (funcall publisher buffer)))
               ;; §7.4 attach: record the child on its parent's :children only
               ;; after the child entry itself is published.
               (_parent-attach-recorded
                (when parent-run-id
                  (superchat-runtime--update
                   parent-run-id :children
                   (cons run-id
                         (plist-get (superchat-runtime--entry parent-run-id)
                                    :children)))))
               (intent (or (plist-get request :input)
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
                        :capability capability
                        :model (plist-get entry :model))
                  (when profile-id (list :profile-id profile-id)))
                 (format "run.started: %s" run-id) intent-id))
               (started-id (plist-get started-record :event-id))
               (_stopped-during-start
                (when (superchat-runtime--terminal-p
                       (plist-get (superchat-runtime--entry run-id) :status))
                  (throw 'superchat-owner-startup-stopped nil)))
               (entry
                (superchat-runtime--update
                 run-id
                 :status (if (and (plist-get (superchat-runtime--entry run-id) :owner-control-p)
                                  (plist-get (superchat-runtime--entry run-id) :stop-requested))
                             'accepted 'running-model)
                 :causation-id started-id))
               (_ (when (plist-get entry :owner-control-p)
                    (superchat-runtime--owner-startup-checkpoint run-id)))
               (entry (superchat-runtime--entry run-id))
               (entry (superchat-runtime--arm-deadline entry))
               (turn-state
                (list
                 :runtime-managed t
                 :intent-event-id intent-id
                 :intent-storage-id intent-storage-id
                 :context-envelope context-envelope
                 :evidence-contract evidence-contract
                 :record-conversation
                 (if (plist-member request :record-conversation)
                     (plist-get request :record-conversation)
                   t))))
          (setf (superchat-turn-state turn) turn-state)
          (let ((superchat--tool-execution-context
                 (list :session-id session-id
                       :input-core-token input-core-token
                       :input-core-tool-name (plist-get input-core :tool-name)
                       :input-operation input-operation
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
                       :evidence-contract evidence-contract
                       :context-envelope (copy-tree context-envelope t)
                       :run-active-p
                       (lambda ()
                         ;; Runtime alone owns effect capability; active-runs
                         ;; deliberately includes terminal delivery cleanup.
                         ;; Tool authority closes before public Turn hooks.
                         (superchat-runtime-effect-capable-p
                         run-id))
                       :interrupt-function
                       (lambda ()
                         (superchat-runtime-cancel
                          run-id "[Interrupted by user]"))
                       :failure-recorded-function
                       (lambda (failure-event)
                         (when (and (equal run-id
                                           (plist-get failure-event :run-id))
                                    (equal "tool.failed"
                                           (plist-get failure-event :type))
                                    (stringp
                                     (plist-get
                                      (plist-get failure-event :payload)
                                      :error-type))
                                    (not (equal
                                          "user-quit"
                                          (plist-get
                                           (plist-get failure-event :payload)
                                           :interruption))))
                           (setq last-failure-event
                                 (copy-tree failure-event t))))
                       :recovery-failure-function
                       (lambda (failure-info)
                         (let ((failure-event
                                (plist-get failure-info :event)))
                           (when (and (equal run-id
                                             (plist-get failure-event :run-id))
                                      (equal "tool.failed"
                                             (plist-get failure-event :type))
                                      (stringp
                                       (plist-get
                                        (plist-get failure-event :payload)
                                        :error-type))
                                      (not (equal
                                            "user-quit"
                                            (plist-get
                                             (plist-get failure-event :payload)
                                             :interruption))))
                             (setq last-failure-event
                                   (copy-tree failure-event t)
                                   last-failure-info
                                   (copy-tree failure-info t)))))
                       :tool-completed-function
                       (lambda (completion)
                         (superchat-runtime--recovery-tool-completed
                          run-id completion))
                       :evidence-origin-validator
                       (lambda (attempt snapshot-id)
                         (let* ((current
                                 (superchat-runtime--entry run-id))
                                (latest
                                 (and current
                                      (car
                                       (last
                                        (plist-get
                                         current
                                         :decision-snapshot-events))))))
                           (and current latest
                                (not
                                 (superchat-runtime--terminal-p
                                  (plist-get current :status)))
                                (integerp attempt)
                                (> attempt 0)
                                (= attempt (car latest))
                                (equal snapshot-id (cdr latest)))))
                       :attendance (or (plist-get request :attendance)
                                       'attended)
                       :validate (plist-get request :tool-validator)
                       ;; Request-scoped approval, e.g. a Surface granting
                       ;; a consented read scope for one batch Run.
                       :approval-function
                       (plist-get request :approval-function)
                       :audit-failure-function
                       (lambda (_err result)
                         (let ((current
                                (superchat-runtime--entry run-id)))
                           (when (and current
                                      (not
                                       (superchat-runtime--terminal-p
                                        (plist-get current :status))))
                             (superchat-runtime--audit-incomplete
                              run-id
                              superchat-runtime--evidence-audit-error
                              (format "%s" result)))))))
                (superchat--llm-transcript-owner (plist-get entry :transcript-owner))
                (superchat--llm-runtime-observer
                 (lambda (event value)
                   (pcase event
                     ('before-provider-request
                      ;; The LLM adapter retains this observer across native
                      ;; async retries.  Looking up the live entry here, rather
                      ;; than relying on the original dynamic stack, keeps each
                      ;; provider attempt in the accepted Run's caller Scope.
                      ;; A terminal Run is effect-inert and has no new provider
                      ;; attempt, so an obsolete callback must not re-enter an
                      ;; Event layer after its Scope reference was released.
                      (let ((current (superchat-runtime--entry run-id)))
                        (if (and current
                                 (superchat-runtime-effect-capable-p run-id))
                            (superchat-emit
                             (superchat-runtime--execution-scope current)
                             'before-provider-request value)
                          value)))
                     ('before-input-dispatch
                      (and (superchat-runtime--input-check-deadline run-id)
                           (or (superchat-runtime--input-adapter-current-p run-id)
                               (progn
                                 (superchat-runtime--finish
                                  run-id 'failed "[Human input adapter is no longer available]")
                                 nil))
                           (not (superchat-runtime-pending-input run-id))))
                     ('input-clock-exhausted
                      (superchat-runtime--finish run-id 'timed-out "[Response hard timeout]"))
                    ('before-attempt
                      (superchat-runtime--recovery-intervention-view
                       run-id (plist-get value :attempt-reason)
                       last-failure-event last-failure-info))
                     ('decision-snapshot
                      (when evidence-contract
                        (superchat-runtime--record-decision-snapshot
                         run-id evidence-contract value)))
                     ('first-output
                      (when evidence-contract
                        (superchat-runtime--record-first-output
                         run-id evidence-contract value)))
                     ('audit-failure
                      (when evidence-contract
                        (let ((current (superchat-runtime--entry run-id)))
                          (when (and current
                                     (not
                                      (superchat-runtime--terminal-p
                                       (plist-get current :status))))
                            (superchat-runtime--audit-incomplete
                             run-id superchat-runtime--evidence-audit-error
                             "[Tool call blocked because exact audit failed]")))))
                     ('request
                      (let ((current (superchat-runtime--entry run-id)))
                        (when current
                          (if (or (superchat-runtime--terminal-p
                                   (plist-get current :status))
                                  (superchat-execution-scope-retired-p
                                   (superchat-runtime--execution-scope current)))
                              ;; A synchronous callback may finish the Run
                              ;; before the native transport returns its handle.
                              ;; Never install that late handle into a live slot.
                              (unless (and (eq (plist-get current :status) 'completed)
                                           (not (plist-get current :stop-requested))
                                           (not (superchat-execution-scope-retired-p
                                                 (superchat-runtime--execution-scope current))))
                                (superchat-runtime--abort-transport value))
                            (superchat-runtime--update
                             run-id :transport-request value)))))
                    ('attempt-started
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (superchat-runtime-effect-capable-p
                                    run-id))
                          (superchat-runtime--update
                           run-id
                           :status 'running-model
                           :model (or (plist-get value :model)
                                      (plist-get current :model))
                           :model-phase
                           (or (plist-get value :model-phase)
                               'waiting-first-token)
                           :attempt (plist-get value :attempt)
                           :attempt-reason (plist-get value :attempt-reason)
                           :context-recovery nil
                           :context-used (plist-get value :context-used)
                           :context-limit (plist-get value :context-limit)
                           :context-limit-source
                           (plist-get value :context-limit-source)
                           :context-estimated-p t
                           :capability-source
                           (plist-get value :capability-source)
                           ;; Contract §4.2's Run-status record requirement
                           ;; (S09-fix-1): which tool names this attempt's
                           ;; prompt actually carries, and why.
                           :runtime-selected-tools
                           (plist-get value :runtime-selected-tools)
                           :runtime-tool-decision
                           (plist-get value :runtime-tool-decision)))))
                     ('model-phase
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (superchat-runtime--update
                           run-id :model-phase value))))
                     ('context-budget-exceeded
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (superchat-runtime--update
                           run-id
                           :context-used (plist-get value :context-used)
                           :context-limit (plist-get value :context-limit)
                           :context-limit-source
                           (plist-get value :context-limit-source)
                           :context-estimated-p t))))
                     ('context-recovery-started
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (superchat-runtime--update
                           run-id :context-recovery 'compacting))))
                     ('context-recovery-ended
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (superchat-runtime--update
                           run-id :context-recovery nil))))
                     ('usage
                      (let ((current (superchat-runtime--entry run-id)))
                        (when (and current
                                   (not (superchat-runtime--terminal-p
                                         (plist-get current :status))))
                          (let ((input (plist-get value :input-tokens))
                                (output (plist-get value :output-tokens)))
                            (superchat-runtime--update
                             run-id
                             :input-tokens input
                             :output-tokens output
                             :context-used
                             (+ (or input 0) (or output 0))
                             :context-estimated-p
                             (not (numberp output)))))))
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
                   ;; Provider effects use the narrow Runtime predicate;
                   ;; delivering remains UI-active but is effect-inert.
                   (if (plist-get (superchat-runtime--entry run-id) :owner-control-p)
                       (superchat-runtime--input-check-deadline run-id)
                     (superchat-runtime-effect-capable-p run-id))))
                (superchat--llm-transport-timer-owner-function
                 (lambda (owner)
                   (let ((current (superchat-runtime--entry run-id)))
                     (when (and current
                                (not
                                 (superchat-runtime--terminal-p
                                  (plist-get current :status)))
                                (not (superchat-execution-scope-retired-p
                                      (superchat-runtime--execution-scope current))))
                       (superchat-runtime--update
                        run-id :transport-timer-owner owner)))
                   run-id))
                (superchat--llm-drain-steers-function
                 (lambda () (superchat-runtime-drain-steers run-id)))
                (superchat--llm-pending-steer-count-function
                 (lambda () (superchat-runtime-pending-steer-count run-id)))
                (superchat--llm-evidence-context
                 (when evidence-contract
                   (list :contract evidence-contract
                         :context-envelope context-envelope)))
                (superchat--llm-context-envelope
                 (copy-tree context-envelope t)))
            (let* ((buffer (or (superchat-runtime--entry-buffer entry)
                               (error "Runtime View buffer is no longer live")))
                   (name (buffer-name buffer))
                   (superchat-buffer-name name))
              (with-current-buffer buffer
                (setq-local superchat--last-surface surface)
                (when (plist-member request :workspace-root)
                  (setq-local superchat-tools-default-directory
                              (plist-get request :workspace-root)))
                (let ((superchat-execution-scope
                       (superchat-runtime--execution-scope entry)))
                  (when (superchat-sink-live-p (plist-get entry :sink))
                    (let ((status-emit
                           (superchat-sink-emit
                            (plist-get entry :sink) 'status
                            (superchat-runtime-sink-run run-id) 'waiting)))
                      (when (plist-get status-emit :condition)
                        (signal (car (plist-get status-emit :condition))
                                (cdr (plist-get status-emit :condition)))))))
                ;; `live-p' and the waiting status callback are public code.
                ;; A retirement from either path has already cancelled this
                ;; published Run; do not enter the first provider attempt.
                (unless (superchat-runtime-effect-capable-p run-id)
                  (throw 'superchat-owner-startup-stopped nil))
                (let ((superchat--llm-request-max-tokens
                       (plist-get request :max-tokens))
                      ;; Request evidence (M1): the identity and the message
                      ;; composition of this request travel to the LLM layer,
                      ;; where the provider-visible tool schemas are
                      ;; materialized.  The composition is the continuity
                      ;; envelope the prompt's history fragment was assembled
                      ;; from, so the record states the selection that
                      ;; happened instead of a second derivation of it.
                      (superchat--llm-request-context
                       (list :session-id session-id
                             :run-id run-id
                             :turn-id turn-id
                             :request-id (plist-get request :request-id)
                             :message-items
                             (superchat-runtime--envelope-items context-envelope)
                             :message-receipt
                             (plist-get context-envelope :receipt))))
                  (superchat-runtime--generate
                   (plist-get request :prompt)
                   #'ignore
                   (lambda (chunk)
                     (let ((current (superchat-runtime--entry run-id)))
                       (when current
                         ;; Native streaming callbacks no longer retain the
                         ;; original service-call stack.  Restore the private
                         ;; Run Scope for both `live-p' and the Sink callback.
                         (let ((superchat-execution-scope
                                (superchat-runtime--execution-scope current)))
                           (when (and (superchat-runtime-effect-capable-p run-id)
                                      (superchat-sink-live-p (plist-get current :sink)))
                             ;; Contract §2.3: a legacy `:stream-function' request
                             ;; still receives the provider's raw chunk unchanged.
                             ;; Every other Sink is contract §3.1's normalized
                             ;; `(:kind text|reasoning :text STRING)'; a chunk
                             ;; that normalizes to nil is not emitted.
                             (if (plist-get current :stream-function)
                                 (superchat-sink-emit
                                  (plist-get current :sink) 'stream
                                  (superchat-runtime-sink-run run-id) chunk)
                               (let ((payload
                                      (if (and (consp chunk) (plist-member chunk :kind))
                                          chunk
                                        (superchat-sink-normalize-chunk chunk))))
                                 (when payload
                                   (superchat-sink-emit
                                    (plist-get current :sink) 'stream
                                    (superchat-runtime-sink-run run-id) payload)))))))))
                   (plist-get request :target-model)
                   (plist-get request :context-files)
                   (plist-get request :tools)
                   (plist-get request :system-prompt)
                   preset
                   ;; Carries a `select-tools' decision `superchat-pipeline-
                   ;; submit' already made, so the LLM layer honors it as-is
                   ;; instead of re-deciding from `:tools' alone and
                   ;; misreporting a `stage'/`baseline' decision as
                   ;; `explicit' (contract §4.2, S09-fix-2 item 2).
                   (plist-get request :tool-decision))))))))
      ((error quit)
       (let* ((message
               (if evidence-contract
                   (cadr superchat-runtime--evidence-audit-error)
                 (error-message-string err)))
              (current (superchat-runtime--entry run-id)))
         (when (plist-get current :owner-control-p)
           (superchat-runtime--update run-id :owner-startup-active nil))
         (condition-case cleanup-error
         (unless (or (null current)
                     (superchat-runtime--terminal-p (plist-get current :status)))
           (if (and (eq (plist-get current :status) 'running-model)
                    (not (eq (car err)
                             'superchat-llm-evidence-recording-error)))
               (superchat-runtime--finish run-id 'failed message)
             ;; No provider effect occurred, but the failed audit still needs
             ;; a recoverable Run/Turn terminal seal.
             (superchat-runtime--audit-incomplete
              run-id superchat-runtime--evidence-audit-error "")))
           ((error quit)
            (if (plist-get current :owner-control-p)
                (progn
                  (superchat-runtime--update run-id :owner-cleanup-error cleanup-error)
                  ;; A secondary quit can escape the legacy finish body's
                  ;; error-only audit handler.  Recover truthfully before
                  ;; rethrowing the original acceptance condition below.
                  (unless (superchat-runtime--terminal-p
                           (plist-get (superchat-runtime--entry run-id) :status))
                    (condition-case recovery-error
                        (superchat-runtime--audit-incomplete run-id cleanup-error "")
                      ((error quit)
                       (superchat-runtime--update
                        run-id :status 'audit-incomplete
                        :owner-cleanup-error (list cleanup-error recovery-error)
                        :input-control-active nil :owner-stop-function nil)
                       (condition-case nil
                           (superchat-runtime--quiesce (superchat-runtime--entry run-id))
                         ((error quit) nil))))))
              (signal (car cleanup-error) (cdr cleanup-error))))))
       (if evidence-contract
           (signal 'superchat-llm-evidence-recording-error
                   (cdr superchat-runtime--evidence-audit-error))
         (signal (car err) (cdr err)))))
    (list :turn-id turn-id
          :run-id run-id
          :session-id session-id
          :status 'accepted)))

(defun superchat-runtime--controlled-p (entry)
  "Whether ENTRY retains opt-in stop control through terminal consumers."
  (or (plist-get entry :input-core) (plist-get entry :owner-control-p)))

(defun superchat-runtime--notify-owner-stop (run-id)
  "Propagate RUN-ID's stop to its producer immediately, once.
This callback owns no receipt or display settlement.  Its diagnostics cannot
replace a primary consumer condition or prevent exact transport release."
  (let ((entry (superchat-runtime--entry run-id)))
    (when (and (plist-get entry :owner-control-p)
               (plist-get entry :stop-requested)
               (not (plist-get entry :owner-stop-notified)))
      (let ((function (plist-get entry :owner-stop-function)))
        (superchat-runtime--update run-id :owner-stop-notified 'entered
                                   :owner-stop-function nil)
        (unwind-protect
            (when function
              (condition-case condition
                  (funcall function (superchat-runtime--copy-public-value
                                     (superchat-runtime-status run-id)))
                ((error quit)
                 (superchat-runtime--update run-id :owner-stop-error condition))))
          (superchat-runtime--update run-id :owner-stop-notified 'finished))))))

(defun superchat-runtime--owner-startup-checkpoint (run-id)
  "Drain startup stop after successful audited startup, then notify acceptance.
The notification's return value is not permission.  A stopped or expired Run
exits the submit startup catch without entering the model adapter."
  (let* ((entry (superchat-runtime--entry run-id))
         (intent (plist-get entry :owner-startup-terminal))
         (function (plist-get entry :accepted-function)))
    (superchat-runtime--update run-id :owner-startup-active nil
                               :owner-startup-terminal nil :accepted-function nil)
    (when (or intent (plist-get entry :stop-requested))
      (superchat-runtime--finish run-id (or (car intent) 'cancelled)
                                (or (cadr intent) (plist-get entry :stop-requested)))
      (throw 'superchat-owner-startup-stopped nil))
    (when (superchat-runtime--input-check-deadline run-id)
      (when function
        (superchat-runtime--update run-id :owner-accepted-notified t)
        (funcall function
                 (superchat-runtime--copy-public-value
                  (list :run-id run-id :turn-id (plist-get entry :turn-id)
                        :session-id (plist-get entry :session-id) :status 'accepted)))))
    (unless (superchat-runtime--input-check-deadline run-id)
      (throw 'superchat-owner-startup-stopped nil))))

(defun superchat-runtime-effect-capable-p (run-id)
  "Return non-nil when RUN-ID may still initiate a Runtime effect.

This is deliberately narrower than `superchat-runtime-active-runs'.  A Run
being terminally delivered remains active for UI cleanup and cancellation
fallbacks, but it may no longer steer, resume, execute a tool, or start a
provider attempt."
  (let ((entry (superchat-runtime--entry run-id)))
    (and (eq 'running-model (plist-get entry :status))
         (not (superchat-execution-scope-retired-p
               (superchat-runtime--execution-scope entry)))
         (superchat-runtime--transport-current-p entry)
         (not (plist-get entry :terminalizing))
         (not (plist-get entry :owner-startup-active))
         (not (and (superchat-runtime--controlled-p entry)
                   (plist-get entry :stop-requested))))))

(defun superchat-runtime--input-notify-stop (run-id)
  "Attempt opted-in stop cleanup once, without ending the outer finish."
  (let ((entry (superchat-runtime--entry run-id)))
    (when (and (plist-get entry :input-core)
               (plist-get entry :stop-requested)
               (not (plist-get entry :input-delivery-entered))
               (not (plist-get entry :input-stop-notified)))
      (let ((consumer (plist-get (plist-get entry :input-core) :stop-consumer)))
        (superchat-runtime--update run-id :input-stop-notified 'entered)
        (unwind-protect
            (when consumer
              (condition-case condition
                  (funcall consumer (superchat-runtime--copy-public-value
                                     (superchat-runtime-status run-id)))
                ((error quit)
                 (superchat-runtime--update
                  run-id :stop-notification-error condition))))
          (superchat-runtime--update run-id :input-stop-notified 'finished))))))

(defun superchat-runtime--input-check-deadline (run-id)
  "Check RUN-ID's absolute deadline synchronously before input continuation."
  (let ((entry (superchat-runtime--entry run-id)))
    (if (and (numberp (plist-get entry :deadline))
             (>= (float-time) (plist-get entry :deadline)))
        (progn (superchat-runtime--finish run-id 'timed-out "[Runtime deadline exceeded]") nil)
      (superchat-runtime-effect-capable-p run-id))))

(defun superchat-runtime--input-adapter-current-p (run-id)
  "Recheck the trusted adapter without changing legacy fixture behavior."
  (let* ((entry (superchat-runtime--entry run-id))
         (operation (plist-get entry :input-operation))
         (predicate (plist-get (plist-get entry :input-core) :eligible-function)))
    (or (null operation)
        (and (functionp predicate) (funcall predicate)
             (eq entry (superchat-runtime--entry run-id))
             (superchat-runtime-effect-capable-p run-id)))))

(defun superchat-runtime--trusted-input-operation-p (context function async)
  "Authenticate FUNCTION against the current accepted Run, never its name alone."
  (let* ((run (plist-get context :run-id))
         (entry (superchat-runtime--entry run)))
    (and async (functionp function)
         (eq function (plist-get entry :input-operation))
         (eq (plist-get context :input-core-token) (plist-get entry :input-core-token))
         (equal (plist-get context :session-id) (plist-get entry :session-id))
         (equal (plist-get context :input-core-tool-name) "ask_user")
         (eq (plist-get (plist-get entry :input-core) :operation) 'ask-user)
         (superchat-runtime-effect-capable-p run)
         (or (null (plist-get entry :deadline))
             (< (float-time) (plist-get entry :deadline)))
         (superchat-runtime--input-adapter-current-p run)
         (eq function (plist-get (superchat-runtime--entry run) :input-operation)))))

(defun superchat-runtime--request-input (capability question)
  "Publish one NONSHIPPING invocation to its configured controlled presenter."
  (unless (and (functionp capability) (listp question)
               (stringp (plist-get question :question)))
    (error "Invalid internal input request"))
  (let* ((identity (funcall capability 'snapshot))
         (run-id (plist-get identity :run-id))
         (entry (superchat-runtime--entry run-id))
         (old (plist-get entry :input-question))
         (old-state (and old (funcall (plist-get old :capability) 'snapshot)))
         (presenter (plist-get (plist-get entry :input-core) :presenter)))
    (unless (and (plist-get entry :input-core)
                 (eq (plist-get identity :token) (plist-get entry :input-core-token))
                 (equal (plist-get identity :session-id) (plist-get entry :session-id))
                 (superchat-runtime-effect-capable-p run-id)
                 (or (null old) (eq (plist-get old-state :delivery) 'entered)))
      (error "Input invocation is not eligible for this Run"))
    (let ((record (list :handle (make-symbol "input-request") :capability capability
                        :question (superchat-runtime--copy-public-value question))))
      (superchat-runtime--update run-id :input-question record)
      (condition-case condition
          (when (superchat-runtime--input-check-deadline run-id)
            (let ((state (superchat-llm--input-pause-clock
                          (superchat-runtime-transport-timer-owner run-id)
                          (plist-get record :handle))))
              (cond
               ((eq state 'exhausted)
                (superchat-runtime--finish run-id 'timed-out "[Response hard timeout]"))
               ((eq state 'closed)
                (superchat-runtime--finish run-id 'failed "[Input transport is closed]"))
               ((and (superchat-runtime--input-check-deadline run-id)
                     (eq record (plist-get (superchat-runtime--entry run-id) :input-question)))
                (funcall presenter (superchat-runtime-pending-input run-id))))))
        ((error quit)
         (superchat-runtime--input-terminal-intent run-id 'failed "[Input presenter failed]")
         (unwind-protect (superchat-runtime--input-drain run-id)
           (signal (car condition) (cdr condition)))))
      (plist-get record :handle))))

(defun superchat-runtime-pending-input (run-id)
  "Return a NONSHIPPING question snapshot, never its capability.
Conses, strings, vectors and bool-vectors in question data are detached;
other Lisp objects retain their existing identity.  HANDLE stays exact."
  (let* ((entry (superchat-runtime--entry run-id))
         (record (plist-get entry :input-question)))
    (when (and record (not (plist-get entry :stop-requested))
               (null (plist-get (funcall (plist-get record :capability) 'snapshot) :claim)))
      (append (list :handle (plist-get record :handle) :run-id (copy-sequence run-id)
                    :session-id (copy-sequence (plist-get entry :session-id))
                    :deadline (plist-get entry :deadline))
              (superchat-runtime--copy-public-value (plist-get record :question))))))

(defun superchat-runtime--input-blocked-reason (entry)
  "Return a blocker for ENTRY's current undelivered question, if any.
Only Authority's not-started delivery state proves no consumer entry.
This is not an expiry cause or a judgment about receipt commitment."
  (when-let* ((record (plist-get entry :input-question))
              (facts (funcall (plist-get record :capability) 'snapshot)))
    (when (eq (plist-get facts :delivery) 'not-started)
      (if (eq (plist-get facts :claim) 'answer)
          'answer-not-delivered
        'unanswered))))

(defun superchat-runtime--input-capture-blocker (run-id)
  "Latch only RUN-ID's CURRENT question before terminal cleanup retires it.
No unwinding record is a fallback.  Public readers see the scalar only once
the Run is terminal; ordinary successfully entered continuations do not latch."
  (when-let* ((reason (superchat-runtime--input-blocked-reason
                      (superchat-runtime--entry run-id))))
    (superchat-runtime--update run-id :input-continuation-blocked reason)))

(defun superchat-runtime--input-terminal-intent (run-id status response)
  "Merge pending input terminal intent; Runtime alone chooses Run outcomes."
  (let* ((entry (superchat-runtime--entry run-id))
         (old (plist-get entry :input-terminal-intent))
         (priority '(audit-incomplete timed-out cancelled failed completed)))
    (superchat-runtime--input-capture-blocker run-id)
    (when (or (null old)
              (< (cl-position status priority) (cl-position (car old) priority)))
      (superchat-runtime--update run-id :input-terminal-intent (list status response)))))

(defun superchat-runtime--input-drain (run-id)
  "Drain only outside input settlement; retain exact Q1/Q2 ownership."
  (let* ((entry (superchat-runtime--entry run-id))
         (intent (plist-get entry :input-terminal-intent))
         (record (plist-get entry :input-question)))
    (when (and intent (= 0 (or (plist-get entry :input-settlement-depth) 0)))
      (if (and record
               (null (plist-get (funcall (plist-get record :capability) 'snapshot) :claim)))
          (superchat-runtime--settle-input run-id record 'failed (cadr intent))
        (superchat-runtime--update run-id :input-terminal-intent nil)
        (superchat-runtime--input-finish-scope
         run-id (lambda () (superchat-runtime--finish-body run-id (car intent) (cadr intent))))))))

(defun superchat-runtime--settle-input (run-id record outcome value)
  "Account for RECORD once, then conditionally enter its native consumer."
  (let* ((entry (superchat-runtime--entry run-id))
         (depth (or (plist-get entry :input-settlement-depth) 0))
         (capability (plist-get record :capability)) accepted condition)
    (superchat-runtime--update run-id :input-settlement-depth (1+ depth))
    (unwind-protect
        (condition-case err
            (progn
              (setq accepted (funcall capability 'settle outcome value))
              (when (and accepted (eq outcome 'answer)
                         (superchat-runtime--input-check-deadline run-id)
                         (null (plist-get (superchat-runtime--entry run-id) :input-terminal-intent))
                         (superchat-runtime-effect-capable-p run-id))
                (funcall capability 'deliver)))
          ((error quit) (setq condition err)))
      (superchat-runtime--update run-id :input-settlement-depth depth)
      ;; Audit bypass and processing failure may go directly to terminal
      ;; handling after compare-retire.  Capture current Q2 (never stale Q1)
      ;; first, while its Authority delivery evidence remains reachable.
      (when (and condition
                 (let ((facts (funcall capability 'snapshot)))
                   (or (eq (plist-get facts :fault) 'audit)
                       (not (eq (plist-get facts :delivery) 'raised)))))
        (superchat-runtime--input-capture-blocker run-id))
      (when (eq record (plist-get (superchat-runtime--entry run-id) :input-question))
        (superchat-runtime--update run-id :input-question nil))
      (let ((facts (funcall capability 'snapshot)))
        (cond
         ((and condition (eq (plist-get facts :fault) 'audit))
          (superchat-runtime--update run-id :stop-requested "[Input audit failed]"
                                     :input-terminal-intent nil)
          (superchat-runtime--audit-incomplete run-id condition "[Input audit failed]"))
         ((and condition (not (eq (plist-get facts :delivery) 'raised)))
          (superchat-runtime--input-terminal-intent run-id 'failed "[Input processing failed]"))))
      (superchat-runtime--input-drain run-id))
    (when condition (signal (car condition) (cdr condition)))
    accepted))

(defun superchat-runtime-answer-input (run-id handle answer)
  "Answer the exact internal request HANDLE once, without starting a new Run."
  (let* ((entry (superchat-runtime--entry run-id))
         (record (plist-get entry :input-question)))
    (when (and record (eq handle (plist-get record :handle))
               (superchat-runtime-effect-capable-p run-id)
               (stringp answer) (not (string-empty-p (string-trim answer)))
               (null (plist-get (funcall (plist-get record :capability) 'snapshot) :claim)))
      (if (and (numberp (plist-get entry :deadline))
               (>= (float-time) (plist-get entry :deadline)))
          (progn (superchat-runtime--finish run-id 'timed-out "[Runtime deadline exceeded]") nil)
        (superchat-runtime--settle-input run-id record 'answer answer)))))

(defun superchat-runtime--input-finish-scope (run-id function)
  "Keep opted-in control reachable through FUNCTION and all terminal hooks."
  (let ((entry (superchat-runtime--entry run-id)))
    (if (not (superchat-runtime--controlled-p entry))
        (funcall function)
      (let ((depth (or (plist-get entry :input-finish-depth) 0)))
        (superchat-runtime--update run-id :input-finish-depth (1+ depth))
        (unwind-protect (funcall function)
          (superchat-runtime--update run-id :input-finish-depth depth)
          (when (and (= depth 0)
                     (superchat-runtime--terminal-p
                      (plist-get (superchat-runtime--entry run-id) :status)))
            (unwind-protect (superchat-runtime--input-notify-stop run-id)
              (superchat-runtime--update run-id :input-control-active nil
                                         :owner-stop-function nil))))))))

(defun superchat-runtime--copy-public-value (value)
  "Recursively detach mutable VALUE for a public Runtime projection."
  (cond
   ((consp value)
    (cons (superchat-runtime--copy-public-value (car value))
          (superchat-runtime--copy-public-value (cdr value))))
   ((stringp value) (copy-sequence value))
   ((bool-vector-p value) (copy-sequence value))
   ((vectorp value)
    (let ((copy (copy-sequence value))
          (index 0))
      (while (< index (length copy))
        (aset copy index
              (superchat-runtime--copy-public-value (aref copy index)))
        (setq index (1+ index)))
      copy))
   (t value)))

(defun superchat-runtime--envelope-items (envelope)
  "Return ENVELOPE's provider-visible message composition as a vector.
The continuity envelope the prompt's history fragment was assembled from
stores its selection as `:items', each carrying `:storage-id' and `:kind'.
Recording that selection instead of re-deriving one keeps a budget- or
supersession-driven divergence visible to the reconstruction rather than
hidden behind a second derivation that agrees with itself."
  (let ((items (and (listp envelope) (plist-get envelope :items))))
    (cond ((vectorp items) items)
          ((listp items) (vconcat items))
          (t []))))

(defun superchat-runtime--generate (&rest args)
  "Call the required mounted `model-transport' Service; never bypass it."
  (let* ((execution-scope (superchat-execution-scope-resolve nil))
         (scope-retired-p
          (superchat-execution-scope-retired-p execution-scope))
         (active-p
          (and (not scope-retired-p)
               (not superchat-runtime--transport-stopping)
               (or (null superchat--llm-run-active-p)
                   (funcall superchat--llm-run-active-p)))))
    (cond
     ;; An accepted Runtime Run can be synchronously retired by a public
     ;; lifecycle listener.  Its cancellation already owns the terminal fact;
     ;; returning inertly prevents the older startup frame from turning that
     ;; stop into a second failure.  Other inactive transport/error paths keep
     ;; their existing error contract, and an unwrapped internal caller still
     ;; gets a retired-Scope error rather than silently doing nothing.
     ((not active-p)
      (if (and superchat--llm-run-active-p scope-retired-p)
          nil
        (if scope-retired-p
            (error "Superchat execution Scope is retired")
          (error "Runtime Run is no longer active; model transport not entered"))))
     (t
      (let* (;; The Runtime observer is captured by `superchat--llm-generate-answer'
             ;; for retries after this dynamic call returns.  Route the first fold
             ;; through the same observer when present, so both paths use the Run's
             ;; private borrowed Scope.  Non-LLM adapters retain the explicit
             ;; caller-Scope fallback.
             (before-request (list :prompt (nth 0 args) :system (nth 6 args)))
             (spec (if (functionp superchat--llm-runtime-observer)
                       (funcall superchat--llm-runtime-observer
                                'before-provider-request before-request)
                     (superchat-emit (superchat-execution-scope-resolve nil)
                                     'before-provider-request before-request)))
             (args (let ((copy (copy-sequence args)))
                     (when (plist-member spec :prompt)
                       (setf (nth 0 copy) (plist-get spec :prompt)))
                     (when (and (> (length copy) 6) (plist-member spec :system))
                       (setf (nth 6 copy) (plist-get spec :system)))
                     copy)))
        ;; `before-provider-request' is public Event code.  It can retire this
        ;; Scope after the first check, so recheck immediately before the only
        ;; external provider entry.
        (setq scope-retired-p
              (superchat-execution-scope-retired-p execution-scope))
        (if (and (not scope-retired-p)
                 (not superchat-runtime--transport-stopping)
                 (or (null superchat--llm-run-active-p)
                     (funcall superchat--llm-run-active-p)))
            (apply #'superchat-service-call nil 'model-transport 'generate args)
          (if (and superchat--llm-run-active-p scope-retired-p)
              nil
            (if scope-retired-p
                (error "Superchat execution Scope is retired")
              (error "Runtime Run is no longer active; model transport not entered")))))))))

(defun superchat-runtime--abort-transport (request)
  "Ask the provider to release exact REQUEST, best-effort.
Only this cancellation adapter isolates both error and quit.  Run lifecycle
and Authority consumer conditions retain their existing semantics."
  (when (and request (fboundp 'llm-cancel-request))
    (condition-case nil
        (llm-cancel-request request)
      ((error quit) nil))))

;; Follow-up queue --------------------------------------------------------
;;
;; §7.5: Runtime owns an in-process, per-Session FIFO of requests to send
;; after the current Run ends.  It lives only in memory; nothing here is
;; persisted or revived after restart.

(defvar superchat-runtime--session-queues (make-hash-table :test #'equal)
  "Session-id -> FIFO list of pending follow-up items, oldest first.
Each item is a plist `(:queue-id STRING :request PLIST-WITH-LIVE-SINK-OR-BUFFER
:execution-scope SCOPE :enqueued-at TIME)'.  The Scope is a borrowed,
process-private reference only; no queue event or Request serializes it.")

(defvar superchat-runtime--queue-sequence 0
  "Monotonic counter recorded on every `queue.queued' Tape event.")

(defvar superchat-runtime--queue-timers (make-hash-table :test #'equal)
  "Session-id -> the single pending `superchat-runtime--dispatch-next' timer.")

(defun superchat-runtime--queue-event (session-id type payload summary)
  "Append a session-annotation Tape event of TYPE for SESSION-ID.
Follow-up queue events have no Run identity to attach to, so this calls
`superchat-tape-append-event-record' directly instead of going through
`superchat-runtime--append-event', which requires a registry entry."
  (superchat-tape-append-event-record
   (list :type type :session-id session-id :actor-id "runtime"
         :payload payload :summary summary)))

(defun superchat-runtime-enqueue-follow-up (session-id request)
  "Append REQUEST to SESSION-ID's in-process follow-up FIFO.
REQUEST is copied with `copy-sequence' (shallow; the Sink/buffer objects are
shared on purpose).  Appends a `queue.queued' Tape event and returns the
minted queue-id.

Per §7.1, the required `queue.queued' fact is appended *before* either the
FIFO or the sequence counter is mutated; if the append signals, neither is
touched and this returns `(:accepted nil :reason tape-failed)' instead of a
queue-id -- no compensating fact is invented.  A Scope already retired before
this call returns `(:accepted nil :reason scope-retired)' without a queue
fact.  If retirement reenters through the required append, the new item stays
on this one FIFO for its normal scope-retired drop, but is never accepted."
  (let* ((execution-scope (superchat-execution-scope-resolve nil))
         (queue-id (superchat-runtime--new-id "queue"))
         (sequence (1+ superchat-runtime--queue-sequence)))
    (if (superchat-execution-scope-retired-p execution-scope)
        (list :accepted nil :reason 'scope-retired)
      (condition-case nil
          (progn
            (superchat-runtime--queue-event
             session-id "queue.queued"
             (list :queue-id queue-id :sequence sequence)
             (format "Follow-up %s queued for session %s" queue-id session-id))
            (setq superchat-runtime--queue-sequence sequence)
            (let ((item
                   (list :queue-id queue-id
                         :request (copy-sequence request)
                         ;; Preserve the caller Scope across the timer/remount
                         ;; boundary without putting it in Envelope or Tape.
                         :execution-scope execution-scope
                         :enqueued-at (float-time))))
              ;; Read the FIFO after the append: a Tape listener may have
              ;; synchronously enqueued another item while this fact landed.
              (puthash session-id
                       (append (gethash session-id superchat-runtime--session-queues)
                               (list item))
                       superchat-runtime--session-queues)
              ;; `queue.queued' is a public append boundary.  Do not return an
              ;; accepted handle if it retired the caller Scope; schedule only
              ;; the existing dispatcher so it can record/drop this FIFO item.
              (if (superchat-execution-scope-retired-p execution-scope)
                  (progn
                    (condition-case nil
                        (superchat-runtime--schedule-dispatch-next session-id)
                      ((error quit) nil))
                    (list :accepted nil :reason 'scope-retired))
                queue-id)))
        (error (list :accepted nil :reason 'tape-failed))))))

(defun superchat-runtime-pending-follow-ups (session-id)
  "Return the number of follow-ups still queued for SESSION-ID."
  (length (gethash session-id superchat-runtime--session-queues)))

(defun superchat-runtime-clear-follow-ups (session-id reason)
  "Drop every follow-up queued for SESSION-ID, recording REASON.
Appends one `queue.dropped' Tape event per dropped item, committing that
item's removal only once its own fact lands (§7.1): a later item's append
failure can never resurrect an earlier item Tape already recorded dropped.
An item whose own `queue.dropped' append fails is left queued rather than
losing it or inventing a compensating fact.  Also cancels and removes
SESSION-ID's pending dispatch timer, if any."
  (dolist (item (gethash session-id superchat-runtime--session-queues))
    (condition-case nil
        (progn
          (superchat-runtime--queue-event
           session-id "queue.dropped"
           (list :queue-id (plist-get item :queue-id) :reason reason)
           (format "Follow-up %s dropped (%s)" (plist-get item :queue-id) reason))
          (puthash session-id
                   (delq item (gethash session-id superchat-runtime--session-queues))
                   superchat-runtime--session-queues))
      (error nil)))
  (unless (gethash session-id superchat-runtime--session-queues)
    (remhash session-id superchat-runtime--session-queues))
  (when-let* ((timer (gethash session-id superchat-runtime--queue-timers)))
    (cancel-timer timer)
    (remhash session-id superchat-runtime--queue-timers)))

(defun superchat-runtime--follow-up-request-live-p (request)
  "Return non-nil when REQUEST's delivery target is still live.
An envelope REQUEST carries an explicit `:sink'; a legacy REQUEST carries a
`:buffer' instead."
  (if (plist-member request :sink)
      (superchat-sink-live-p (plist-get request :sink))
    (buffer-live-p (plist-get request :buffer))))

(defun superchat-runtime--session-has-active-run-p (session-id)
  "Return non-nil when SESSION-ID has a Run in `superchat-runtime-active-runs'.
There is no per-session index; this scans the active-run list and compares
`:session-id' from `superchat-runtime-status', per §7.5."
  (cl-some
   (lambda (run-id)
     (equal session-id (plist-get (superchat-runtime-status run-id) :session-id)))
   (superchat-runtime-active-runs)))

(defun superchat-runtime--follow-up-dispatch-available-p ()
  "Whether the one existing FIFO may enter its Driver right now.

This is deliberately a lookup, not a paused-queue state.  A Sink `live-p'
predicate is public code and can synchronously withdraw either Service after
the dispatch loop first tested it."
  (and (not superchat-runtime--transport-stopping)
       (superchat-service-get nil 'driver)
       (superchat-service-get nil 'model-transport)))

(defun superchat-runtime--follow-up-dispatch-current-p (session-id item)
  "Whether ITEM is still SESSION-ID's dispatchable FIFO head.

Call this after yielding to public Sink code.  It prevents a stale dispatcher
from submitting an item after that callback withdrew transport, submitted a
Run, or otherwise changed the one canonical queue."
  (and (superchat-runtime--follow-up-dispatch-available-p)
       (not (superchat-runtime--session-has-active-run-p session-id))
       (eq item (car (gethash session-id superchat-runtime--session-queues)))))

(defun superchat-runtime--schedule-dispatch-next (session-id)
  "Schedule `superchat-runtime--dispatch-next' for SESSION-ID at timer-0.
Only one timer is ever pending per session (§7.1); an existing one is
cancelled and replaced."
  (when (and (stringp session-id)
             (superchat-runtime--follow-up-dispatch-available-p))
    (when-let* ((existing (gethash session-id superchat-runtime--queue-timers)))
      (cancel-timer existing))
    (puthash session-id
             (run-at-time 0 nil #'superchat-runtime--dispatch-next session-id)
             superchat-runtime--queue-timers)))

(defun superchat-runtime-resume-follow-ups ()
  "Schedule existing FIFOs after the driver and its transport are restored."
  (maphash (lambda (session-id queue)
             (when queue (superchat-runtime--schedule-dispatch-next session-id)))
           superchat-runtime--session-queues))

(defun superchat-runtime--dispatch-drop-item (session-id queue item reason &optional detail)
  "Best-effort record ITEM's drop for SESSION-ID/REASON and commit its removal.
QUEUE is the exact list ITEM currently heads.  Returns non-nil when the drop
fact was appended (removal committed); nil when the append itself failed --
ITEM is then left as SESSION-ID's queue head, unmutated, so a later
`superchat-runtime--dispatch-next' retries it instead of losing it."
  (let ((queue-id (plist-get item :queue-id)))
    (condition-case nil
        (progn
          (superchat-runtime--queue-event
           session-id "queue.dropped"
           (list :queue-id queue-id :reason reason)
           (if detail
               (format "Follow-up %s dropped (%s): %s" queue-id reason detail)
             (format "Follow-up %s dropped (%s)" queue-id reason)))
          (puthash session-id (cdr queue) superchat-runtime--session-queues)
          t)
      (error nil))))

(defun superchat-runtime--dispatch-next (session-id)
  "Submit at most one of SESSION-ID's queued follow-ups per call.
Per §7.1, this returns unconditionally after one successful
`superchat-runtime-submit' -- that Run's own quiesce schedules the next
call, so two Runs are never dispatched from the same timer/callback even
when a provider completes synchronously.  A dead delivery target is dropped
\(`sink-dead') and the loop continues to the next head, as does a
`superchat-runtime-submit' error (`submit-error'); neither creates a Run, so
nothing else would otherwise reconsider this session's queue.  A required
`queue.dropped'/`queue.dispatched' Tape fact that itself fails to append
never loses or duplicates the item: a not-yet-submitted item is left as the
head and a fresh timer is scheduled to retry it; an already-submitted item is
always consumed (resubmitting it would duplicate the Run), even when its
`queue.dispatched' fact could not be recorded.  If public Sink liveness code
withdraws the Driver or transport, the head stays queued without a drop or
busy retry; the restored Driver resumes this same FIFO.  A retired-Scope head
is dropped with reason `scope-retired'; if that required drop fact cannot be
written it stays inert until a later retirement/recovery scheduling attempt,
not a fresh busy timer.  The whole body is wrapped so one bad item cannot
wedge this session's dispatch chain."
  (remhash session-id superchat-runtime--queue-timers)
  (let (dispatched retry paused)
    (condition-case err
        (while (and (not dispatched) (not retry) (not paused)
                    (superchat-runtime--follow-up-dispatch-available-p)
                    (not (superchat-runtime--session-has-active-run-p session-id))
                    (gethash session-id superchat-runtime--session-queues))
          (let* ((queue (gethash session-id superchat-runtime--session-queues))
                 (item (car queue))
                 (request (plist-get item :request))
                 (queue-id (plist-get item :queue-id))
                 (execution-scope
                  (or (plist-get item :execution-scope)
                      superchat-scope-global)))
            ;; The timer has no dynamic caller.  Restore only the borrowed
            ;; enqueue Scope while running its public Sink liveness predicate
            ;; and the subsequent Driver submission; the queue remains the
            ;; one canonical FIFO and owns neither Scope nor Run lifecycle.
            (let ((superchat-execution-scope execution-scope))
              (cl-labels
                  ((drop-retired ()
                     ;; `live-p' may have reentered queue code.  Use the
                     ;; current FIFO cons chain so removing this head cannot
                     ;; discard a tail enqueued during that callback.
                     (let ((current-queue
                            (gethash session-id superchat-runtime--session-queues)))
                       (if (eq item (car current-queue))
                           (unless (superchat-runtime--dispatch-drop-item
                                    session-id current-queue item "scope-retired")
                             (setq paused t))
                         (setq paused t)))))
                (cond
                 ;; Do not invoke a retired head's Sink predicate at all.
                 ((superchat-execution-scope-retired-p execution-scope)
                  (drop-retired))
                 ((not (superchat-runtime--follow-up-request-live-p request))
                  (unless (superchat-runtime--dispatch-drop-item
                           session-id queue item "sink-dead")
                    (setq retry t)))
                 ;; `live-p' is public code.  Retirement wins over the
                 ;; transport-unmount pause path and earns an auditable drop.
                 ((superchat-execution-scope-retired-p execution-scope)
                  (drop-retired))
                 ;; Revalidate availability and this exact head before the
                 ;; Driver can consume it.  Transport loss remains a pause.
                 ((not (superchat-runtime--follow-up-dispatch-current-p
                        session-id item))
                  (setq paused t))
                 (t
                  (condition-case submit-err
                      (let ((handle (if (plist-member request :surface)
                                        ;; An Envelope (chat follow-up, step 3): the
                                        ;; driver Service runs the Stages; no active
                                        ;; Run exists here, so `admit' processes it.
                                        (superchat-service-call nil 'driver 'submit request)
                                      (superchat-runtime-submit request))))
                        (unless (plist-get handle :run-id)
                          (error "follow-up was not admitted: %S" handle))
                        ;; The Run now exists regardless of whether this fact
                        ;; lands, so the item is consumed either way.
                        (ignore-errors
                          (superchat-runtime--queue-event
                           session-id "queue.dispatched"
                           (list :queue-id queue-id :run-id (plist-get handle :run-id))
                           (format "Follow-up %s dispatched" queue-id)))
                        (puthash session-id (cdr queue) superchat-runtime--session-queues)
                        (setq dispatched t))
                    (error
                     ;; A Stage/Driver callback can remove transport or retire
                     ;; this Scope after the pre-submit check.  The former is a
                     ;; pause; the latter is a required, non-busy FIFO drop.
                     (cond
                      ((superchat-execution-scope-retired-p execution-scope)
                       (drop-retired))
                      ((not (superchat-runtime--follow-up-dispatch-current-p
                             session-id item))
                       (setq paused t))
                      ((not (superchat-runtime--dispatch-drop-item
                             session-id queue item "submit-error"
                             (error-message-string submit-err)))
                       (setq retry t)))))))))))
      (error
       (message "superchat-runtime: follow-up dispatch for session %s failed: %s"
                session-id (error-message-string err))
       (setq retry t)))
    (when (and retry (gethash session-id superchat-runtime--session-queues))
      (superchat-runtime--schedule-dispatch-next session-id))))

(defun superchat-runtime--drop-all-follow-ups ()
  "Best-effort: drop every session's follow-up queue, for `kill-emacs-hook'."
  (ignore-errors
    (let (session-ids)
      (maphash (lambda (session-id _queue) (push session-id session-ids))
               superchat-runtime--session-queues)
      (dolist (session-id session-ids)
        (ignore-errors
          (superchat-runtime-clear-follow-ups session-id "emacs-exit"))))))

(add-hook 'kill-emacs-hook #'superchat-runtime--drop-all-follow-ups)

(defun superchat-runtime--quiesce (entry)
  "Sever exact Run execution handles from current registry ENTRY once.

Its published `:quiesced' flag makes synchronous reentry a no-op.  Each
best-effort execution-handle release is isolated so one adapter cannot gate
the remaining releases.  Runtime never closes the borrowed editor buffer."
  (when-let* ((run-id (plist-get entry :run-id))
              (current (superchat-runtime--entry run-id)))
    (unless (plist-get current :quiesced)
      ;; Publish the one-shot guard and sever callback/provider reachability
      ;; before invoking timer or View adapters, which may call back
      ;; synchronously.  The execution Scope is likewise a borrowed private
      ;; handle: clearing the registry reference releases it but never disposes
      ;; its caller-owned Scope.  Keep CURRENT as the exact handle snapshot to
      ;; release.
      (setq current (copy-sequence current))
      (superchat-runtime--update
       run-id
       :quiesced t
       :deadline-timer nil
       :delivery-function nil
       :accepted-function nil
       :stream-function nil
       :sink nil
       :transport-request nil
       :transport-timer-owner nil
       :execution-scope nil)
      (when (and (superchat-runtime--terminal-p (plist-get current :status))
                 (not (eq (plist-get current :status) 'completed)))
        (superchat-runtime--abort-transport
         (plist-get current :transport-request)))
      (ignore-errors
        (superchat-runtime--cancel-timeout current))
      ;; §7.4: prune this quiescing child from its (still-live) parent's
      ;; index so :child-run-ids and a future cascade only ever see live
      ;; descendants.  --finish-body's own pre-quiesce :children snapshot,
      ;; used to schedule *this* Run's own cascade, is a separate local
      ;; value and is never affected by pruning the entry's live field here.
      (ignore-errors
        (when-let* ((parent-run-id (plist-get current :parent-run-id))
                    (parent (superchat-runtime--entry parent-run-id)))
          (superchat-runtime--update
           parent-run-id :children
           (delete run-id (copy-sequence (plist-get parent :children))))))
      (let ((view-buffer (superchat-runtime--entry-buffer current)))
        (when (and (buffer-live-p view-buffer)
                   (fboundp 'superchat--clear-pause-state))
          (with-current-buffer view-buffer
            (ignore-errors
              (superchat--clear-pause-state run-id))))))
    (superchat-runtime--schedule-dispatch-next (plist-get entry :session-id))
    (superchat-runtime--entry run-id)))

(provide 'superchat-runtime)

;;; superchat-runtime.el ends here
