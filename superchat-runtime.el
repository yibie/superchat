;;; superchat-runtime.el --- Main Turn/Run lifecycle for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Runtime owns main Chat/Agent execution.  Callers submit a prepared request;
;; the mutable Turn, provider request, lifecycle events, and delivery remain
;; behind `superchat-runtime-submit'.

;;; Code:

(require 'cl-lib)
(require 'superchat-select-tools)
(declare-function llm-tool-name "llm" (tool))
(require 'json)
(require 'subr-x)
(require 'superchat-core)
(require 'superchat-preset)
(require 'superchat-tape)
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
(defvar superchat--llm-request-context)
(defvar superchat--step-refused)
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
it to resume Event and Service lookup after an async callback; it neither
disposes the Scope nor gives it lifecycle ownership over the Run."
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

(defun superchat-runtime--open-authority-work-p (entry)
  "Return non-nil when ENTRY has unsettled Authority Effect or Approval work."
  (let ((work (plist-get entry :open-authority-work)))
    (and (hash-table-p work) (> (hash-table-count work) 0))))

(defun superchat-runtime--open-authority-work-summary (entry)
  "Return a compact summary of ENTRY's unsettled Authority work."
  (let ((work (plist-get entry :open-authority-work))
        (executions 0)
        (effects 0)
        (approvals 0))
    (when (hash-table-p work)
      (maphash
       (lambda (_execution marks)
         (cl-incf executions)
         (when (plist-get marks :effect) (cl-incf effects))
         (when (plist-get marks :approval) (cl-incf approvals)))
       work))
    (format "%d execution(s), %d effect(s), %d approval(s)"
            executions effects approvals)))

(defun superchat-runtime--note-authority-work (run-id execution-id action)
  "Account ACTION for EXECUTION-ID under RUN-ID.
ACTION is one of `effect-entered', `effect-settled', `approval-entered' and
`approval-settled'.  The helper is total: unknown or terminal Runs and
malformed identities are ignored without signalling.  A settlement removes
only its own mark; the execution row is dropped only when both marks are gone.
The operation never rewrites a terminal entry, calls finish, or appends Tape."
  (let ((entry (and (stringp run-id) (not (string-empty-p run-id))
                    (stringp execution-id) (not (string-empty-p execution-id))
                    (superchat-runtime--entry run-id))))
    (when (and entry
               (not (superchat-runtime--terminal-p (plist-get entry :status))))
      (let* ((work (or (plist-get entry :open-authority-work)
                       (make-hash-table :test #'equal)))
             (marks (and (hash-table-p work) (gethash execution-id work)))
             settle-key)
        (pcase action
          ('effect-entered
           (puthash execution-id (plist-put marks :effect t) work))
          ('approval-entered
           (puthash execution-id (plist-put marks :approval t) work))
          ('effect-settled
           (setq settle-key :effect))
          ('approval-settled
           (setq settle-key :approval))
          (_ (error "Unknown Authority work action: %S" action)))
        (when settle-key
          (let ((remaining
                 (cl-loop for (key value) on marks by #'cddr
                          unless (eq key settle-key)
                          append (list key value))))
            (if remaining
                (puthash execution-id remaining work)
              (remhash execution-id work))))
        (superchat-runtime--update run-id :open-authority-work work)
        t))))

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
prompt or provider request.  Tape is the only source of truth; the
process-local Run registry is intentionally ignored.  Return the number
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
                         (turn-id (plist-get started :turn-id))
                         (run-state
                          (superchat-tape--run-terminal-state
                           session-id run-id))
                         (turn-state
                          (and (stringp turn-id)
                               (not (string-empty-p turn-id))
                               (superchat-tape--turn-terminal-state
                                session-id run-id turn-id))))
                    (cond
                     ;; A partially written recovery pair is completed on the
                     ;; next View rebuild, without duplicating its Run seal.
                     ((and (eq 'present (plist-get run-state :state))
                           (superchat-runtime--reconciliation-abandoned-p
                            (plist-get run-state :event))
                           (eq 'absent (plist-get turn-state :state)))
                      (let* ((entry
                              (superchat-runtime--reconciliation-entry started))
                             (record
                              (superchat-tape-append-event-record
                               (superchat-runtime--event
                                entry "turn.completed"
                                '(:status abandoned :abandoned t)
                                (format "turn.completed: %s (abandoned)"
                                        run-id)
                                (plist-get (plist-get run-state :event)
                                           :event-id)))))
                        (when record
                          (cl-incf sealed))))
                     ((and (eq 'absent (plist-get run-state :state))
                           (eq 'absent (plist-get turn-state :state)))
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

;;; The inbox ---------------------------------------------------------------
;;
;; The inbox is the one place that owns "what input does this Session still
;; hold".  It unifies ownership rather than adding a second queue: `followup'
;; scheduling already lives in `superchat-runtime--session-queues' and steering
;; in a Run's `:steer-queue', so this section adds the one class neither
;; carries -- `inject', identified model material that is not a task -- and the
;; atomic claim the pre-step gate asks for.
;;
;; The accepted M3 record is explicit that this stays process-local and that no
;; durable inject queue or input-claim event family is introduced.  So an
;; injection is Runtime state, not a Tape fact, and the claim returns the
;; material rather than recording that it was claimed: an injection whose Turn
;; never arrives is discarded without a fact, exactly as a dropped draft is.

(defvar superchat-runtime--pending-injections (make-hash-table :test #'equal)
  "Session-id -> list of pending injections, oldest first.
Each entry is a plist `(:inject-id STRING :material PLIST :execution-scope
SCOPE :enqueued-at TIME)'.  The Scope is borrowed, never serialized, and is
what decides which Turn may claim the entry: an injection belongs to the
Preset and Scope that submitted it and must not attach to another.")

(defvar superchat-runtime--injection-sequence 0
  "Monotonic counter minting injection identities.
Process-local like the injections themselves, so no Tape fact carries it.")

(defun superchat-runtime--correct-injection-position (session-id injection)
  "Insert INJECTION into SESSION-ID's pending list, preserving insertion order."
  (puthash session-id
           (append (gethash session-id superchat-runtime--pending-injections)
                   (list injection))
           superchat-runtime--pending-injections))

(defun superchat-runtime--injectable-material-p (material)
  "Return non-nil when MATERIAL is material the model can actually receive.
M3 makes `inject' the one input class that carries identified model material,
so material the model cannot see is not injectable: delivery appends its text to
the dispatched prompt (`superchat-runtime--prompt-with-injected'), and a
material with no usable `:text' would otherwise be recorded as admitted while
staying invisible to the provider.  The predicate is shared by admission and by
what a pre-step gate may admit, so what is recorded and what is delivered are
decided by one rule rather than two."
  (and (listp material)
       (plist-get material :source)
       (let ((text (plist-get material :text)))
         (and (stringp text) (not (string-empty-p text))))))

;;;###autoload
(defun superchat-runtime-inject (session-id material &optional scope)
  "Queue identified MATERIAL for SESSION-ID under the exact caller SCOPE.
A live matching Run owns the material until its next Step or termination.
Otherwise the material waits for a matching Turn.  Global is a valid Scope,
not a wildcard.  Injection never starts a Run."
  (let ((execution-scope (superchat-execution-scope-resolve scope)))
    (cond
     ((or (not (stringp session-id)) (string-empty-p session-id)
          (not (superchat-runtime--injectable-material-p material)))
      (list :accepted nil :reason 'invalid-material))
     ((superchat-execution-scope-retired-p execution-scope)
      (list :accepted nil :reason 'scope-retired))
     (t
      (let* ((target
              (cl-find-if
               (lambda (run-id)
                 (let ((entry (superchat-runtime--entry run-id)))
                   (and (equal session-id (plist-get entry :session-id))
                        (eq execution-scope
                            (superchat-runtime--execution-scope entry)))))
               (superchat-runtime-active-runs)))
             (seq (cl-incf superchat-runtime--injection-sequence))
             (id (format "inject-%06x" seq))
             (identified (plist-put (copy-tree material) :inject-id id)))
        (superchat-runtime--correct-injection-position
         session-id
         (list :inject-id id :seq seq :material identified
               :target-run-id target :execution-scope execution-scope
               :enqueued-at (float-time)))
        id)))))

(defun superchat-runtime--injection-matches-p (injection entry)
  "Whether INJECTION can enter the live Run described by ENTRY."
  (and entry
       ;; A pending drop is an unresolved audit obligation, not fresh input.
       (not (plist-get injection :drop-event))
       (not (superchat-runtime--terminal-p (plist-get entry :status)))
       (eq (plist-get injection :execution-scope)
           (superchat-runtime--execution-scope entry))
       (or (null (plist-get injection :target-run-id))
           (equal (plist-get injection :target-run-id)
                  (plist-get entry :run-id)))))

(defun superchat-runtime--inbox-claim-injections (session-id run-id)
  "Claim matching pending injection entries for a live RUN-ID in SESSION-ID.
Matching uses exact Scope and, when present, exact Run identity.  A terminal
Run cannot acquire fresh material after releasing its borrowed Scope.  The
entries keep their arrival sequence, which is what lets the loop merge them
with steering into one ordered batch; `superchat-runtime-inbox-claim-step'
maps them to material for callers that want only the injections."
  (let* ((entry (superchat-runtime--entry run-id))
         (pending (gethash session-id superchat-runtime--pending-injections))
         claimed rest)
    (when (and entry (equal session-id (plist-get entry :session-id)))
      (dolist (injection pending)
        (if (superchat-runtime--injection-matches-p injection entry)
            (push injection claimed)
          (push injection rest)))
      (if rest
          (puthash session-id (nreverse rest) superchat-runtime--pending-injections)
        (remhash session-id superchat-runtime--pending-injections)))
    (nreverse claimed)))

(defun superchat-runtime-inbox-claim-step (session-id run-id)
  "Atomically claim matching injected material for a live RUN-ID.
This is the injection-only view of the claim; the loop opens a Step through
`superchat-runtime--claim-step-batch', which also carries steering, so the two
kinds share one insertion order."
  (mapcar (lambda (injection) (plist-get injection :material))
          (superchat-runtime--inbox-claim-injections session-id run-id)))

(defun superchat-runtime--claim-step-batch (run-id)
  "Claim RUN-ID's one ordered eligible batch of steering and injection.
The accepted record requires that within an eligible next-step batch steer and
inject preserve insertion order, that the loop alone atomically claims the
batch, and that contributors never drain the queue independently.  One arrival
counter orders both kinds, so this returns material plists oldest first with
steering rendered as identified material: it passes the same admission check,
reaches the same pre-step gate and is recorded by the same durable append as an
injection.  Claimed steering is cleared here, so a later independent drain of
the same Run finds nothing left to deliver outside the gate."
  (let* ((entry (superchat-runtime--entry run-id))
         (session-id (and entry (plist-get entry :session-id)))
         (steers (and entry (plist-get entry :steer-queue)))
         (injections (and entry
                          (superchat-runtime--inbox-claim-injections
                           session-id run-id))))
    (when (and entry steers)
      (superchat-runtime--update run-id :steer-queue nil))
    (mapcar
     #'cdr
     (sort (append
            (mapcar (lambda (injection)
                      (cons (or (plist-get injection :seq) 0)
                            (plist-get injection :material)))
                    injections)
            (mapcar (lambda (steer)
                      (cons (or (plist-get steer :seq) 0)
                            (list :source (list :kind 'steer :run-id run-id)
                                  :text (plist-get steer :text)
                                  :steer-id (plist-get steer :id))))
                    steers))
           (lambda (a b) (< (car a) (car b)))))))
(defun superchat-runtime-inbox-discard-step (session-id run-id)
  "Auditably discard pending material explicitly targeted at the ending RUN-ID.
Idle material for a future matching Turn is not owned by this Run, and material
targeted at another Run is left alone.

The record keeps the queue-drop obligation in force, so a discarded item is
settled the same way the follow-up FIFO settles its own: one `queue.dropped'
fact carrying the item's stable `:inject-id' and the reason, with the removal
committed only once that fact lands.  A failed append therefore leaves the item
pending rather than losing identified material without evidence, and a later
item's failure can never resurrect an earlier item the Tape already recorded
dropped -- which is what makes the drop recoverable from its own identity.
Returns the number of items actually dropped."
  (let ((dropped 0)
        (snapshot (gethash session-id superchat-runtime--pending-injections)))
    (dolist (injection snapshot)
      (when (equal run-id (plist-get injection :target-run-id))
        (condition-case nil
            (progn
              (superchat-runtime--record-queue-drop
               session-id injection "run ended without consuming the injection"
               (format "Injection %s dropped (Run %s ended without consuming it)"
                       (plist-get injection :inject-id) run-id))
              ;; Re-read the list: the append above is a public boundary and a
              ;; listener may have enqueued another injection while it landed.
              (puthash session-id
                       (delq injection
                             (gethash session-id superchat-runtime--pending-injections))
                       superchat-runtime--pending-injections)
              (cl-incf dropped))
          (error nil))))
    (unless (gethash session-id superchat-runtime--pending-injections)
      (remhash session-id superchat-runtime--pending-injections))
    dropped))

(defun superchat-runtime-session-scheduling-state (session-id)
  "Return SESSION-ID's Runtime-owned scheduling state as a read-only plist.
This is what an opaque driver handle reports and nothing more: how much work
the Session holds and which Run is doing it.  It carries no queue, no closure,
no borrowed Scope and no authority, so a holder of the state cannot dispatch,
drop or approve anything with it."
  (let* ((pending (superchat-runtime-pending-follow-ups session-id))
         (active
          (cl-find-if (lambda (run-id)
                        (let ((entry (superchat-runtime--entry run-id)))
                          (and entry
                               (equal session-id (plist-get entry :session-id)))))
                      (superchat-runtime-active-runs))))
    (list :session-id session-id
          :pending-follow-ups pending
          :pending-injections (superchat-runtime-pending-injections session-id)
          :active-run-id (and active (copy-sequence active))
          :active-p (and active t))))

(defun superchat-runtime--drop-pending-injections (session-id reason &optional scope)
  "Auditably drop SESSION-ID's pending injections, recording REASON.
With SCOPE, drop only the injections whose borrowed Scope is SCOPE or a
descendant of it -- the ones a retirement strands.  Without SCOPE, drop what
the Session holds, for the same two lifecycle points that drop its FIFO.

Injection state is process-local and has the same lifetime as the FIFO, so it
is settled the same way rather than left pending forever: one `queue.dropped'
Tape event per dropped item, and the item's removal is committed only once its
own fact lands.  A failed append leaves that item pending rather than losing
the material without evidence, and a later item's failure can never resurrect
an earlier item the Tape already recorded dropped.

Returns the number of injections actually dropped."
  (let ((dropped 0) (snapshot (gethash session-id superchat-runtime--pending-injections)))
    (dolist (injection snapshot)
      (when (or (null scope)
                (memq scope
                      (superchat-scope-chain
                       (or (plist-get injection :execution-scope)
                           superchat-scope-global))))
        (condition-case nil
            (progn
              (superchat-runtime--record-queue-drop
               session-id injection reason
               (format "Injection %s dropped (%s)"
                       (plist-get injection :inject-id) reason))
              ;; Re-read the list: the append above is a public boundary and a
              ;; listener may have enqueued another injection while it landed.
              (puthash session-id
                       (delq injection
                             (gethash session-id superchat-runtime--pending-injections))
                       superchat-runtime--pending-injections)
              (cl-incf dropped))
          (error nil))))
    (unless (gethash session-id superchat-runtime--pending-injections)
      (remhash session-id superchat-runtime--pending-injections))
    dropped))

(defun superchat-runtime-pending-injections (session-id)
  "Return how many injections SESSION-ID is holding."
  (length (gethash session-id superchat-runtime--pending-injections)))

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
         (previous-queue (plist-get entry :steer-queue))
         ;; Steering shares the inbox arrival counter with injection, so one
         ;; ordered claim preserves insertion order across both kinds.
         (seq (cl-incf superchat-runtime--injection-sequence))
         (queued (list :id (format "steer-%06x" seq) :seq seq
                       :text instruction)))
    (if (and entry
             (superchat-runtime-effect-capable-p
              run-id))
        ;; Enqueue before persisting: when the append signals, the queue is
        ;; rolled back to exactly its pre-call shape (§7.1).
        (progn
          (superchat-runtime--update
           run-id :steer-queue
           (append previous-queue (list queued)))
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
        (mapcar (lambda (queued) (plist-get queued :text)) queue)))))
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
                (lambda (queued)
                  (plist-get
                   (superchat-runtime--content-metadata
                    (plist-get queued :text))
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

(defun superchat-runtime--pending-step-input-count (run-id)
  "Count input eligible for the next Step of RUN-ID, excluding followups."
  (let ((entry (superchat-runtime--entry run-id)))
    (+ (superchat-runtime-pending-steer-count run-id)
       (cl-count-if
        (lambda (item) (superchat-runtime--injection-matches-p item entry))
        (gethash (plist-get entry :session-id)
                 superchat-runtime--pending-injections)))))

(defun superchat-runtime--turn-stopping (run-id)
  "Notify before natural closure and recheck input in the same live Run.
The notification has its own listeners; legacy run-sealing remains the
terminal state-contributor boundary.  Callback faults belong to this call,
not to an unrelated entry in the global diagnostic ring."
  (let ((entry (superchat-runtime--entry run-id)))
    (when (superchat-runtime-effect-capable-p run-id)
      (when (and (zerop (superchat-runtime--pending-step-input-count run-id))
                 (not (plist-get entry :turn-stopping-notified)))
        (superchat-runtime--update run-id :turn-stopping-notified t)
        (condition-case err
            (let ((superchat-execution-scope
                   (superchat-runtime--execution-scope entry)))
              (dolist (listener (superchat-event-listeners
                                superchat-execution-scope 'turn-stopping))
                (when (superchat-runtime-effect-capable-p run-id)
                  (funcall (cdr listener)
                           (superchat-runtime-sink-run run-id)
                           (plist-get entry :turn)))))
          ((error quit)
           (superchat-runtime--finish
            run-id 'failed
            (format "[Turn-stopping failed: %s]" (error-message-string err))))))
      (if (superchat-runtime-effect-capable-p run-id)
          (superchat-runtime--pending-step-input-count run-id)
        0))))

(defun superchat-runtime--run-turn-hooks (entry)
  "Run legacy terminal state contributors for ENTRY exactly once.
This is the run-sealing boundary, including cancellation cleanup.  Natural
turn-stopping and the pre-seal inbox check run separately while execution is
still live, through `superchat-runtime--turn-stopping'."
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
             (when-let* ((failures
                          (superchat-emit
                           (superchat-runtime--execution-scope entry) 'run-sealing
                           (superchat-runtime-sink-run (plist-get entry :run-id))
                           turn)))
               (superchat-runtime--update
                (plist-get entry :run-id) :turn-recording-failures failures)))
           (settle-injections ()
             ;; Natural closing has already rechecked eligible input.
             ;; Discard only material targeted at this terminal Run; pending
             ;; idle material remains available to its future matching Turn.
             (superchat-runtime-inbox-discard-step
              (plist-get entry :session-id) (plist-get entry :run-id))))
        (unwind-protect
            (if (buffer-live-p buffer)
                (with-current-buffer buffer (run-hooks))
              (run-hooks))
          (condition-case nil (settle-injections) (error nil))))
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
  ;; `--quiesce' deliberately drops the process-private borrowed Scope.
  ;; The historical Run entry must not retain a caller Scope after it is
  ;; terminal.
  (let ((run-id (plist-get entry :run-id))
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
                    child-error)))))))
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

(defun superchat-runtime--pre-step-decision (entry step batch)
  "Ask the pre-step waterfall whether ENTRY's STEP may open over BATCH.

Returns `(:enter BATCH\') when the Step may open, or `(:reject REASON)'.  The
default -- no middleware, or middleware that abstains -- enters, so an
unmanned seam never blocks work.  A middleware returns `(:reject REASON)` to
refuse, `(:enter BATCH)` to replace the material, or delegates with the
continuation.  A middleware fault is recorded as an Event fault and treated as
a refusal, because entering on a fault would open a Step whose gate ran
incorrectly.

The batch is what this Step proposes to make model-visible.  M3 keeps no
durable input queue, so a Step's batch is the input the accepted Run already
carries; the seam exists so a later slice can claim from an inbox without
moving the gate."
  (let* ((proposal (list :session-id (plist-get entry :session-id)
                         :run-id (plist-get entry :run-id)
                         :turn-id (plist-get entry :turn-id)
                         :step step
                         :batch batch
                         ;; The injections this Step claimed, so the gate
                         ;; decides on the material the Step would actually
                         ;; admit rather than on the input alone.
                         :injected (plist-get batch :injected)))
         (result (condition-case err
                     (superchat-emit
                      (superchat-runtime--execution-scope entry)
                      'pre-step proposal)
                   ((error quit)
                    (superchat-event-record-fault
                     'pre-step (plist-get entry :run-id) 'listener err)
                    (superchat-waterfall-refuse
                     (format "listener faulted: %s"
                             (error-message-string err)))))))
    (cond
     ;; The waterfall's own refusal sentinel, and a listener fault, both mean
     ;; "do not open a Step".
     ((superchat-waterfall-refused-p result)
      (list :reject (or (superchat-waterfall-refusal-reason result)
                        "pre-step refused")))
     ((and (listp result) (eq (car result) :reject))
      (list :reject (or (cadr result) "pre-step rejected")))
     ((and (listp result) (eq (car result) :enter))
      (list :enter (cadr result)))
     ((null result) (list :enter batch))
     (t (list :enter result)))))

(defun superchat-runtime--material-id (material)
  "Return the inbox identity carried by MATERIAL."
  (or (plist-get material :inject-id) (plist-get material :steer-id)))

(defun superchat-runtime--admitted-material (entered claimed)
  "Validate replacement material and retain its relationship to CLAIMED.
A one-to-one rewrite keeps its inbox identity and original source.
A split or merge supplies :input-ids naming claimed inputs; Runtime records
their actual sources.  An empty batch intentionally omits all claimed input."
  (let ((batch (if (and (listp entered) (plist-member entered :injected))
                   (plist-get entered :injected)
                 entered)))
    (unless (and (proper-list-p batch)
                 (cl-every #'superchat-runtime--injectable-material-p batch))
      (error "Pre-step returned invalid identified material"))
    (mapcar
     (lambda (material)
       (let* ((copy (copy-tree material))
              (ids (or (plist-get copy :input-ids)
                       (list (superchat-runtime--material-id copy))))
              (inputs
               (and (proper-list-p ids) ids
                    (mapcar
                     (lambda (id)
                       (or (and (stringp id)
                                (cl-find id claimed
                                         :key #'superchat-runtime--material-id :test #'equal))
                           (error "Pre-step material has an unknown input identity: %S" id)))
                     ids))))
         (unless inputs
           (error "Pre-step material must identify its claimed sources"))
         (if (plist-member copy :input-ids)
             (setq copy
                   (plist-put copy :source
                              (list :kind 'derived
                                    :inputs
                                    (mapcar
                                     (lambda (input)
                                       (list :id (superchat-runtime--material-id input)
                                             :source (copy-tree (plist-get input :source))))
                                     inputs))))
           (unless (equal (plist-get copy :source) (plist-get (car inputs) :source))
             (error "Pre-step rewrite changed its source relationship")))
         (plist-put copy :input-ids (copy-sequence ids))))
     batch)))

(defun superchat-runtime--discard-step-material (entry claimed admitted)
  "Audit CLAIMED inputs omitted by ADMITTED without returning them to the inbox."
  (let ((used (apply #'append (mapcar (lambda (item) (plist-get item :input-ids))
                                     admitted))))
    (dolist (item claimed)
      (unless (member (superchat-runtime--material-id item) used)
        (if (plist-get item :steer-id)
            (superchat-runtime--append-steer-undelivered entry (plist-get item :text))
          (superchat-runtime--record-queue-drop
           (plist-get entry :session-id) item "pre-step-not-admitted"
           "Injection omitted by pre-step"))))))

(cl-defun superchat-runtime--enter-step (run-id request context-envelope)
  "Run the pre-step gate for RUN-ID and open the Step when it enters.
Returns the new Step identity.  When the gate rejects the proposal it settles
the Run as failed with the gate's own reason and returns
`superchat--step-refused' instead: the caller has already been handed its
request, so the refusal cannot be reported as nil, and a sentinel keeps it
distinguishable from the nil that an unusable entry would produce."
  (let* ((entry (superchat-runtime--entry run-id))
         (step (1+ (or (plist-get entry :step-count) 0)))
         ;; The loop is the only claimant: it takes the whole eligible batch in
         ;; insertion order, once, here -- after this Step exists but before the
         ;; gate sees it, so the gate decides on the material this Step would
         ;; actually admit.  The batch is one list across steering and injection,
         ;; because the record requires those two kinds to preserve insertion
         ;; order instead of being drained separately.  Anything arriving after
         ;; this claim stays pending for the next Step, and a rejection discards
         ;; the claimed proposal rather than retrying it.
         (claimed (superchat-runtime--claim-step-batch run-id))
         (gate (superchat-runtime--pre-step-decision
                entry step
                (list :prompt (plist-get request :prompt)
                      :message-items
                      (superchat-runtime--envelope-items context-envelope)
                      :message-receipt
                      (plist-get context-envelope :receipt)
                      :injected (copy-tree claimed)))))
    (unless (superchat-runtime-effect-capable-p run-id)
      (superchat-runtime--discard-step-material entry claimed nil)
      (cl-return-from superchat-runtime--enter-step superchat--step-refused))
    (if (eq (car gate) :reject)
        (progn
          (superchat-runtime--discard-step-material entry claimed nil)
          (superchat-runtime--finish
           run-id 'failed
           (format "[Pre-step rejected: %s]" (cadr gate)))
          superchat--step-refused)
      (let* ((admitted
              (condition-case err
                  (superchat-runtime--admitted-material (cadr gate) claimed)
                ((error quit)
                 (superchat-runtime--discard-step-material entry claimed nil)
                 (signal (car err) (cdr err)))))
             (_ (superchat-runtime--discard-step-material entry claimed admitted))
             (step-id (superchat-runtime--step-started entry)))
        (unless (superchat-runtime-effect-capable-p run-id)
          (cl-return-from superchat-runtime--enter-step superchat--step-refused))
        (superchat-runtime--update run-id :step-material admitted
                                   :turn-stopping-notified nil)
        ;; The gate entered, so the material it admitted is what this Step
        ;; makes model-visible.  The record requires admitted material to be
        ;; durably recorded with its source relationship BEFORE provider
        ;; dispatch, and to enter history once rather than once per later Step
        ;; -- recording it here, at the one moment it is admitted, satisfies
        ;; both: it is on the Tape before the request goes out, and later
        ;; Turns derive it from history instead of claiming it again.  A
        ;; rejected proposal returns above and is never recorded, because a
        ;; rejected Step is not delivered material.
        (when admitted
          (condition-case err
              (superchat-runtime--record-injected-material entry admitted)
            (error
             (superchat-runtime--audit-incomplete
              run-id err "[Admitted material could not be durably recorded]")
             (cl-return-from superchat-runtime--enter-step superchat--step-refused)))
          ;; Make the same material model-visible in the Step that admitted
          ;; it.  The claim is one-shot, so this append happens once per
          ;; material and not once per later Step.  Continuations may carry
          ;; this prompt; later Turns read the material from the Tape.  The
          ;; augmented prompt is published on the Run for the dispatch site to
          ;; send, so what the provider receives and what the request evidence
          ;; records are the same string.
          (let ((augmented
                 (superchat-runtime--prompt-with-injected
                  (plist-get request :prompt) admitted)))
            (when augmented
              (superchat-runtime--update run-id :step-prompt augmented))))
        step-id))))

(defun superchat-runtime-step-advance (run-id request context-envelope &optional disposition)
  "Close RUN-ID's current Step and open the next through the pre-step gate.
This is the driver-owned half of an attempt boundary, and the seam a model
transport uses instead of knowing the native adapter's observer protocol: a
transport that returns to the driver for another model attempt calls it and
appends the returned material to that attempt's request.  Closing the prior
Step, claiming the one ordered inbox batch, running the gate and recording the
admitted material all happen here, so every transport gets the same Step
brackets and the same admission rule.

Returns identified material plists in arrival order, or
`superchat--step-refused' when the gate refused -- the Run is already settled as
failed then, and the caller must not dispatch."
  (when (superchat-runtime-effect-capable-p run-id)
    (unless (memq disposition '(nil completed failed cancelled))
      (error "Invalid prior Step disposition: %S" disposition))
    (superchat-runtime--step-ended (superchat-runtime--entry run-id)
                                   (or disposition 'completed))
    (let ((entered (superchat-runtime--enter-step
                    run-id (plist-put (copy-sequence request) :prompt "")
                    context-envelope)))
      ;; The prior Step closed with its real disposition either way; a refused
      ;; gate then opens nothing, so the caller is told not to dispatch rather
      ;; than handed the previous Step's material.
      (if (eq entered superchat--step-refused)
          superchat--step-refused
        (plist-get (superchat-runtime--entry run-id) :step-material)))))

(defun superchat-runtime-turn-stopping (run-id)
  "Run RUN-ID's turn-stopping notification and return its recheck count.
A transport calls this before natural closure: the notification's own listeners
run once, and the return value is how much eligible steer/inject input is
waiting, so the transport can make another attempt in the same Run instead of
closing.  Zero means close."
  (superchat-runtime--turn-stopping run-id))

(defconst superchat--step-refused
  (list 'superchat-step-refused)
  "Value `superchat-runtime--enter-step' returns when the gate rejected.
A named sentinel rather than nil, because nil already means that no Step
identity could be opened, and the two must not be conflated at the point of
use.")

(defun superchat-runtime--prompt-with-injected (prompt claimed)
  "Return PROMPT with CLAIMED material appended as model-visible input.
Returns nil when there is nothing to append, so the caller can leave the
original prompt untouched rather than send a rewritten copy of it."
  (when (and (stringp prompt) claimed)
    (let ((block
           (mapconcat (lambda (material)
                        (let ((text (plist-get material :text)))
                          (when (and (stringp text) (not (string-empty-p text)))
                            (format "[Injected material] %s" text))))
                      claimed "\n")))
      (when (and (stringp block) (not (string-empty-p block)))
        (concat prompt "\n\n" block)))))

(defun superchat-runtime--record-injected-material (entry claimed)
  "Persist admitted CLAIMED material before provider dispatch.
A missing append receipt is an audit failure, not permission to send input
whose durable source relationship cannot be established."
  (dolist (material claimed)
    (unless
        (superchat-tape-append-event
         (list :type "attachment.added"
               :session-id (plist-get entry :session-id)
               :turn-id (plist-get entry :turn-id)
               :run-id (plist-get entry :run-id)
               :actor-id "runtime" :topic "inject"
               :summary "Injected model material" :requires-payload t
               :payload (list :source (copy-tree (plist-get material :source) t)
                              :text (plist-get material :text)
                              :inject-id (plist-get material :inject-id)
                              :steer-id (plist-get material :steer-id)
                              :input-ids (plist-get material :input-ids)
                              :turn_id (plist-get entry :turn-id))))
      (error "Admitted material has no durable append receipt")))
  claimed)

(defun superchat-runtime--step-started (entry)
  "Open ENTRY's next Step and record `step.started'.
A Step is one driver-level model attempt and the tool calls it produces, so
the first bracket opens before transport generation and subsequent brackets
open when the adapter returns to the driver for another attempt.  ENTRY's
`:step' is the
Run-local stable identity; `:step-count' is the monotonic counter that mints
it, kept on the entry so a reader can tell an unopened Step from the first
one.  Returns the step identity, or nil when ENTRY is unusable."
  (when (and (listp entry) (plist-get entry :run-id))
    (let* ((run-id (plist-get entry :run-id))
           (step (1+ (or (plist-get entry :step-count) 0)))
           (event-id (superchat-tape--new-event-id)))
      (superchat-runtime--update run-id
                                 :step-count step
                                 :step step
                                 :step-event-id event-id)
      (superchat-runtime--append-event
       entry "step.started"
       (list :session-id (plist-get entry :session-id)
             :turn-id (plist-get entry :turn-id)
             :run-id run-id
             :step step
             :request-id (plist-get entry :request-id))
       (format "step.started: %s#%d" run-id step)
       nil nil event-id)
      step)))

(defun superchat-runtime--step-ended (entry &optional disposition)
  "Close ENTRY's open Step with DISPOSITION and record `step.ended'.
Does nothing when no Step is open, so a path that never reached the model
transport cannot invent a bracket.  A Step that already ended is not ended
twice: closing clears `:step' while `:step-count' stays monotonic.  Returns
the disposition recorded, or nil when there was no open Step."
  (when (and (listp entry)
             (plist-get entry :run-id)
             (plist-get entry :step))
    (let* ((run-id (plist-get entry :run-id))
           (step (plist-get entry :step))
           (disposition (or disposition 'completed)))
      (superchat-runtime--update run-id :step nil)
      (superchat-runtime--append-event
       entry "step.ended"
       (list :session-id (plist-get entry :session-id)
             :turn-id (plist-get entry :turn-id)
             :run-id run-id
             :step step
             :request-id (plist-get entry :request-id)
             :disposition (symbol-name disposition))
       (format "step.ended: %s#%d (%s)" run-id step disposition)
       nil nil (superchat-tape--new-event-id))
      disposition)))

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
              ;; ── M3 Step bracket: close the entered Step with the
              ;; disposition this seal is committing, before the Run's own
              ;; terminal rows.  Sink loss has already been folded into
              ;; STATUS above, so the Step cannot claim a normal completion
              ;; for a Run that is about to seal cancelled.  A Step that
              ;; never opened records nothing.
              (superchat-runtime--step-ended entry status)
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
                (let ((latest (superchat-runtime--entry run-id)))
                  (when (superchat-runtime--open-authority-work-p latest)
                    (error "Open Runtime Authority work at %s: %s"
                           status
                           (superchat-runtime--open-authority-work-summary
                            latest))))
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
                (let ((latest (superchat-runtime--entry run-id)))
                  (when (superchat-runtime--open-authority-work-p latest)
                    (error "Open Runtime Authority work at %s: %s"
                           status
                           (superchat-runtime--open-authority-work-summary
                            latest))))
                (return-if-superseded)
                ;; A recording gap can only weaken that completion, not
                ;; grant it.  Step/response facts already describe the
                ;; delivered answer.
                (when (and (eq status 'completed)
                           (plist-get entry :turn-recording-failures))
                  (setq status 'audit-incomplete
                        terminal-type
                        (superchat-runtime--terminal-event-type status)))
                ;; As above, the stable identity crosses the append/receipt
                ;; boundary so a lost receipt is three-state, not absent.
                (setq terminal-id (superchat-tape--new-event-id)
                      terminal-record
                      (superchat-runtime--append-event
                       entry terminal-type
                       (append (list :status status)
                               (when (and (eq status 'audit-incomplete)
                                          (plist-get entry :turn-recording-failures))
                                 '(:audit-incomplete t
                                   :reason "turn-recording-failed")))
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
         (superchat-event-record-fault 'transport-stop id 'driver condition))))))

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
             (superchat-event-record-fault
              'scope-retirement run-id 'driver condition)
           ((error quit) nil)))))
    ;; The inbox's inject class has no dispatcher of its own: an injection is
    ;; claimed only at a Step open, so a retirement that closes its Scope
    ;; would strand it with no later reader.  Settle it here through the same
    ;; audited drop the retired FIFO uses, and only when the latch really shut
    ;; -- retiring `superchat-scope-global' is a no-op that must not sweep
    ;; every Session's material.
    (when (superchat-execution-scope-retired-p scope)
      (let (injection-sessions)
        (maphash (lambda (session-id _pending) (push session-id injection-sessions))
                 superchat-runtime--pending-injections)
        (dolist (session-id injection-sessions)
          (condition-case condition
              (superchat-runtime--drop-pending-injections
               session-id "scope-retired" scope)
            ((error quit)
             (condition-case nil
                 (superchat-event-record-fault
                  'scope-retirement session-id 'inbox condition)
               ((error quit) nil)))))))
    ;; A transport gap leaves FIFO entries paused; the existing remount path
    ;; resumes them.  When available, dispatch only drops retired heads -- it
    ;; never creates a model request for this retirement operation itself.
    (dolist (session-id queue-sessions)
      (condition-case condition
          (superchat-runtime--schedule-dispatch-next session-id)
        ((error quit)
         (condition-case nil
             (superchat-event-record-fault
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
                         ;; The Request's identity, kept so the Step rows can
                         ;; state which request the Step belongs to.  Without
                         ;; it the Step payloads read a nil and the record's
                         ;; "provider request evidence links to the entered
                         ;; Step" held in neither direction.
                         :request-id (plist-get request :request-id)
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
                     ('step-id
                      (plist-get (superchat-runtime--entry run-id) :step))
                     ('turn-stopping
                      (superchat-runtime--turn-stopping run-id))
                     ('next-step
                      ;; The adapter is one caller of the driver-owned attempt
                      ;; boundary rather than the owner of it: a custom
                      ;; transport reaches the same behaviour through the
                      ;; driver Service's `advance-step' operation.
                      (let ((advanced
                             (superchat-runtime-step-advance
                              run-id request context-envelope
                              (if (eq value 'tool-protocol-retry) 'failed 'completed))))
                        ;; A refused gate already settled the Run; the adapter
                        ;; has nothing to append and its own active check
                        ;; reports the stop.
                        (unless (eq advanced superchat--step-refused)
                          advanced)))
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
                            (let* ((result
                                    (superchat--request-waterfall
                                     (superchat-runtime--execution-scope current)
                                     value
                                     (superchat--request-tool-restriction request)
                                     (plist-member request :prepared-tools)))
                                   (guidance
                                    (if (plist-member current :request-tool-guidance)
                                        (plist-get current :request-tool-guidance)
                                      (plist-get request :tool-guidance))))
                              (if (or (superchat-waterfall-refused-p result)
                                      (equal (plist-get value :tools)
                                             (plist-get result :tools))
                                      (null guidance))
                                  result
                                (let ((rendered
                                       (superchat--request-render-tool-guidance
                                        result guidance)))
                                  (superchat-runtime--update
                                   run-id :request-tool-guidance (cdr rendered))
                                  (car rendered))))
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
                ;; The record forbids a contributor draining the inbox independently:
                ;; the Step one ordered batch already carries steering, so the adapter
                ;; drain must find nothing to append outside the gate.
                ;; `superchat-runtime-drain-steers remains public for callers that do
                ;; not run through a Step.
                (superchat--llm-drain-steers-function nil)
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
                  ;; ── The pre-step waterfall gates the Step, and the Step
                  ;; opens only when the gate entered.  A rejection settles
                  ;; the Run as failed with the gate's own reason and opens no
                  ;; Step, so there is no bracket to close and no model
                  ;; material delivered.  A refused Step still reaches the
                  ;; transport call below, which returns inertly for an
                  ;; already terminal Run; nothing is dispatched.
                  (superchat-runtime--enter-step
                   run-id request context-envelope)
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
  "Call the required mounted `model-transport' Service; never bypass it.

The body is a `cl-block' so the request waterfall's refusal can leave this
call the same way a returned value does.  A `defun' has no implicit block, and
signalling instead would escape the transport rather than let the Run settle."
  (cl-block superchat-runtime--generate
  (let* ((execution-scope (superchat-execution-scope-resolve nil))
         (scope-retired-p
          (superchat-execution-scope-retired-p execution-scope))
         (active-p
          (and (not scope-retired-p)
               (not superchat-runtime--transport-stopping)
               (or (null superchat--llm-run-active-p)
                   (funcall superchat--llm-run-active-p)))))
    (cond
     ;; A Step the pre-step gate refused has already settled its Run.  This
     ;; call must then be inert rather than dispatching a request for a Run
     ;; that no longer accepts one; `--finish' has the terminal fact, so
     ;; returning nil here cannot lose it.
     ((let ((entry (superchat-runtime--entry
                    (plist-get superchat--llm-request-context :run-id))))
        (and entry (superchat-runtime--terminal-p (plist-get entry :status))))
      nil)
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
             ;; The entered Step may have published a prompt for this
             ;; attempt -- injected material is appended there.  Consume it
             ;; here, once: the value belongs to the Step that published it,
             ;; so clearing it stops a later Step in the same Run inheriting
             ;; it, and the string passed on is the one the provider receives
             ;; and the one request evidence records.
             (published (let ((entry (superchat-runtime--entry
                                      (plist-get superchat--llm-request-context
                                                 :run-id))))
                          (and entry (plist-get entry :step-prompt))))
             (_consume-published
              (when published
                (superchat-runtime--update
                 (plist-get superchat--llm-request-context :run-id)
                 :step-prompt nil)))
             (args (if published (cons published (cdr (append args nil))) args))
             (decision
              (unless superchat--llm-tools-prepared-p
                (or (nth 8 args)
                    (superchat-select-tools-decision
                     (list :tools (nth 5 args) :input (nth 0 args)
                           :surface (bound-and-true-p superchat--last-surface)
                           :input-core
                           (and (plist-get (bound-and-true-p superchat--tool-execution-context)
                                           :input-operation)
                                (list :operation
                                      (plist-get superchat--tool-execution-context :input-operation))))
                     nil))))
             (before-request
              (list :prompt (nth 0 args) :system (nth 6 args)
                    :tools (if superchat--llm-tools-prepared-p
                               (mapcar #'llm-tool-name superchat--llm-prepared-tools)
                             (plist-get decision :names))))
             (spec (if (functionp superchat--llm-runtime-observer)
                       (funcall superchat--llm-runtime-observer
                                'before-provider-request before-request)
                     (superchat--request-waterfall
                      (superchat-execution-scope-resolve nil) before-request
                      (superchat--request-tool-restriction
                       (list :tools (nth 5 args) :tool-decision decision))
                      superchat--llm-tools-prepared-p)))
             ;; ── The request waterfall may refuse the candidate outright.
             ;; A refusal is a decision, so the attempt must not be built from
             ;; it: signalling here routes it through this transport's
             ;; existing attempt-failure path, which settles the Run as a
             ;; failure rather than as a completion.  Without this check a
             ;; refusal would be read as a spec with no `:prompt' or `:system'
             ;; member and the refused request would be dispatched unchanged.
             (_refusal-check
              (when (superchat-waterfall-refused-p spec)
                ;; Settle the Run as failed here rather than signalling out of
                ;; the transport: a refusal is a decision about this request,
                ;; and the Run must reach a terminal fact instead of leaving
                ;; the caller to interpret an escaping error.  `--finish' is
                ;; once-only, so a refusal racing a cancellation cannot add a
                ;; second terminal fact.
                (superchat-runtime--finish
                 (plist-get superchat--llm-request-context :run-id)
                 'failed
                 (format "Provider request refused by the request waterfall: %s"
                         (or (superchat-waterfall-refusal-reason spec)
                             "no reason given")))
                (cl-return-from superchat-runtime--generate nil)))
             (args (let ((copy (append args (make-list (max 0 (- 9 (length args))) nil))))
                     (setf (nth 0 copy) (plist-get spec :prompt)
                           (nth 6 copy) (plist-get spec :system))
                     (unless superchat--llm-tools-prepared-p
                       (let* ((names (plist-get spec :tools))
                              (changed (not (equal names (plist-get decision :names)))))
                         (setf (nth 5 copy) names
                               (nth 8 copy)
                               (if changed
                                   (list :names (or names 'none) :reason 'waterfall)
                                 decision))))
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
              (error "Runtime Run is no longer active; model transport not entered"))))))))))

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

(defvar superchat-runtime--queue-append-event nil
  "Retained event for one dynamically scoped drop append, not a queue.")

(defun superchat-runtime--queue-event (session-id type payload summary)
  "Append a session-annotation Tape event of TYPE for SESSION-ID.
PAYLOAD and SUMMARY carry the annotation.  A drop's retained identity passes
through this existing four-argument seam; unrelated reentry cannot borrow it."
  (let* ((retained superchat-runtime--queue-append-event)
         (event (list :type type :session-id session-id :actor-id "runtime"
                      :payload payload :summary summary))
         (superchat-runtime--queue-append-event nil))
    (when (and retained
               (equal session-id (plist-get retained :session-id))
               (equal type (plist-get retained :type))
               (equal payload (plist-get retained :payload))
               (equal summary (plist-get retained :summary)))
      (setq event (plist-put event :event-id (plist-get retained :event-id))))
    (superchat-tape-append-event-record event)))

(defun superchat-runtime--record-queue-drop (session-id item reason summary)
  "Record ITEM's drop in SESSION-ID once, with REASON and SUMMARY.
Retain the first complete event on ITEM before any public append boundary.
An unreadable Tape is unknown, never permission to repeat a write."
  (let ((event (plist-get item :drop-event)))
    (unless event
      (setq event
            (list :type "queue.dropped" :session-id session-id
                  :event-id (superchat-tape--new-event-id) :actor-id "runtime"
                  :payload (list :queue-id (or (plist-get item :inject-id)
                                              (plist-get item :queue-id))
                                 :reason reason)
                  :summary summary))
      ;; ITEM is the exact pending plist; preserve it across failed receipts.
      (nconc item (list :drop-event event)))
    (cl-labels
        ((receipt (state)
           (let ((persisted (plist-get state :event)))
             (list :event-id (plist-get persisted :event-id)
                   :storage-id (plist-get persisted :storage-id))))
         (unknown (state)
           (let ((condition (plist-get state :condition)))
             (signal (car condition) (cdr condition)))))
      (let ((state (superchat-tape--event-state event)))
        (pcase (plist-get state :state)
          ('present (receipt state))
          ('unknown (unknown state))
          ('absent
           (condition-case err
               (let* ((superchat-runtime--queue-append-event event)
                      (record
                       (superchat-runtime--queue-event
                        session-id (plist-get event :type)
                        (plist-get event :payload) (plist-get event :summary))))
                 (unless (plist-get record :storage-id)
                   (error "Queue drop has no durable append receipt"))
                 record)
             (error
              (setq state (superchat-tape--event-state event))
              (pcase (plist-get state :state)
                ('present (receipt state))
                ('unknown (unknown state))
                (_ (signal (car err) (cdr err))))))))))))

(defvar superchat-runtime--dispatching-envelope nil
  "Exact FIFO Envelope currently entering the driver, or nil.")

(defun superchat-runtime-admit-envelope (session-id envelope)
  "Process ENVELOPE now or queue it in SESSION-ID's Runtime inbox.
Child Runs retain their independent admission.  A dispatched FIFO head may
enter without queuing itself again; unrelated reentry still joins the tail."
  (if (or (plist-get envelope :parent-run-id)
          (plist-get (plist-get envelope :runtime-extras) :parent-run-id)
          (eq envelope superchat-runtime--dispatching-envelope)
          (and (not (superchat-runtime--session-has-active-run-p session-id))
               (zerop (superchat-runtime-pending-follow-ups session-id))))
      '(:status process)
    (let ((result (superchat-runtime-enqueue-follow-up session-id envelope)))
      (if (stringp result)
          (progn
            (unless (superchat-runtime--session-has-active-run-p session-id)
              (superchat-runtime--schedule-dispatch-next session-id))
            (list :status 'queue :queue-id result :reason "session-busy"))
        (list :status 'drop :reason (plist-get result :reason))))))

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
          (superchat-runtime--record-queue-drop
           session-id item reason
           (format "Follow-up %s dropped (%s)" (plist-get item :queue-id) reason))
          (puthash session-id
                   (delq item (gethash session-id superchat-runtime--session-queues))
                   superchat-runtime--session-queues))
      (error nil)))
  (unless (gethash session-id superchat-runtime--session-queues)
    (remhash session-id superchat-runtime--session-queues))
  (when-let* ((timer (gethash session-id superchat-runtime--queue-timers)))
    (cancel-timer timer)
    (remhash session-id superchat-runtime--queue-timers))
  ;; The inbox's inject class is process-local state with the same lifetime as
  ;; the FIFO above, so this same close/exit point settles it through the same
  ;; audited drop instead of leaving the material stranded with no fact.
  (superchat-runtime--drop-pending-injections session-id reason))

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
     (let ((status (superchat-runtime-status run-id)))
       (and (equal session-id (plist-get status :session-id))
            ;; A completed workflow Step may retain stop control while its
            ;; consumer submits the next Step.  It is no longer executing.
            (not (superchat-runtime--terminal-p (plist-get status :status))))))
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
       (not (plist-get item :drop-event))
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

(defun superchat-runtime--dispatch-drop-item (session-id _queue item reason &optional detail)
  "Record ITEM's drop for SESSION-ID/REASON and commit its removal.
DETAIL adds diagnostic text.  Removal re-reads the FIFO after the append.
Return non-nil when the fact is confirmed; otherwise leave ITEM pending with
its retained event identity so a later dispatch can settle the same drop."
  (let ((queue-id (plist-get item :queue-id)))
    (condition-case nil
        (progn
          (superchat-runtime--record-queue-drop
           session-id item reason
           (if detail
               (format "Follow-up %s dropped (%s): %s" queue-id reason detail)
             (format "Follow-up %s dropped (%s)" queue-id reason)))
          (puthash session-id
                   (delq item (gethash session-id superchat-runtime--session-queues))
                   superchat-runtime--session-queues)
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
                 ;; A prior drop may have committed without a readable
                 ;; receipt.  Reconcile it; never resubmit its Envelope.
                 ((plist-get item :drop-event)
                  (unless (superchat-runtime--dispatch-drop-item
                           session-id queue item
                           (plist-get (plist-get (plist-get item :drop-event)
                                                 :payload)
                                      :reason))
                    (setq paused t)))
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
                      (let* ((superchat-runtime--dispatching-envelope request)
                             (handle (if (plist-member request :surface)
                                        ;; An Envelope (chat follow-up, step 3): the
                                        ;; driver Service prepares it; no active
                                        ;; Run exists here, so Runtime admits it.
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
                     ;; A Driver or Event listener callback can remove transport
                     ;; or retire this Scope after the pre-submit check.  The
                     ;; former is a pause; the latter is a required, non-busy
                     ;; FIFO drop.
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
  "Best-effort: drop every session's queued and injected input, for `kill-emacs-hook'.
Both classes are process-local pending input with the same lifetime, so a
session is named by either table: naming only the FIFO would leave a session
that holds nothing but injections with its material dropped silently at exit."
  (ignore-errors
    (let (session-ids)
      (maphash (lambda (session-id _queue) (push session-id session-ids))
               superchat-runtime--session-queues)
      (maphash (lambda (session-id _pending) (cl-pushnew session-id session-ids
                                                         :test #'equal))
               superchat-runtime--pending-injections)
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
       :request-tool-guidance nil
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
