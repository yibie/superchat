;;; superchat-tool-execution.el --- The tool execution pipeline -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The single model-invoked tool execution seam.  Callers provide a small
;; context plist; identity and liveness, validation, the block check, Tape
;; ordering and result delivery stay local to this module.  Who may run what
;; is not decided here: the permission level (`superchat-sandbox.el') decides
;; and the approval seam (`superchat-approval.el') asks.  Until 2026-09-20
;; this file was `superchat-authority.el'; the Authority decision machine it
;; once held was removed in M6.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-event)
(require 'superchat-invariant)
(require 'subr-x)
(require 'superchat-tape)

(declare-function superchat--agent-run-tool-hooks
                  "superchat-agent-loop" (hooks &rest args))
(declare-function superchat--extend-timeout "superchat"
                  (&optional transport-timer-owner))
(declare-function superchat-runtime-transport-timer-owner
                  "superchat-runtime" (run-id))
(declare-function superchat-runtime--request-input "superchat-runtime" (capability question))
(declare-function superchat-runtime--trusted-input-operation-p
                  "superchat-runtime" (context function async))
(declare-function superchat-runtime--note-open-work
                  "superchat-runtime" (run-id execution-id action))

(defvar superchat-tools-default-directory nil)
(declare-function superchat-permission-level-decision "superchat-sandbox"
                  (tool-name args context &optional scope))
(declare-function superchat-tools-row-builtin-owner "superchat-tools" (name))
(declare-function superchat-tools-builtin-descriptor-p "superchat-tools"
                  (name &optional scope))
(declare-function superchat-approval-request-snapshot "superchat-approval"
                  (request &optional scope))
(declare-function superchat-approval-effective-policy "superchat-approval"
                  (session-id))
(defvar superchat--tool-execution-context nil
  "Effective executor context visible to synchronously-created child runs.")

(defvar superchat-tool-mutation-target nil
  "Set by a structured buffer-editing tool body to the buffer it changed.
A plist (:buffer NAME :file PATH-OR-NIL).  The executor binds it to nil around
a synchronous body and copies it onto that call's `tool.completed' fact, which
is how the unsaved-changes list knows a Run's targets without a ledger of its
own.  Code run through `eval_elisp' does not set it: what that changes is not
attributed.")

(defconst superchat-tool-execution-transport-timer-owner-unavailable
  :superchat-transport-timer-owner-unavailable
  "Marker for a Runtime Run that no longer owns transport timers.")

(defun superchat-tool-execution-transport-timer-owner (&optional context)
  "Resolve CONTEXT's opaque transport timer owner.
Return `superchat-tool-execution-transport-timer-owner-unavailable' when CONTEXT
identifies a Runtime Run whose owner is absent.  Return nil only when there is
no Runtime Run identity, preserving the legacy non-Runtime hook behavior.
When CONTEXT is nil, use `superchat--tool-execution-context'."
  (let* ((context (or context superchat--tool-execution-context))
         (owner (and (listp context)
                     (plist-get context :transport-timer-owner)))
         (run-id (and (listp context) (plist-get context :run-id))))
    (cond
     (owner owner)
     (run-id
      (or (and (fboundp 'superchat-runtime-transport-timer-owner)
               (ignore-errors
                 (superchat-runtime-transport-timer-owner run-id)))
          superchat-tool-execution-transport-timer-owner-unavailable))
     (t nil))))

(defgroup superchat-tool-execution nil
  "The tool execution pipeline."
  :group 'superchat)

(defvar superchat-tool-execution--tool-installation nil
  "Private Scope installation captured from this materialized tool function.

It is process-local and dynamically bound only while Authority executes or
settles that exact tool invocation.  It is never copied into an Envelope,
Tape event, or public Run status.")

(defun superchat-tool-execution--installation-from-function (function)
  "Return FUNCTION's private materialized tool installation, if any.

Plugin tools use an uninterned callable symbol whose property carries the
installation selected when that descriptor was materialized.  A raw legacy
callable deliberately returns nil.  Reading the identity from FUNCTION (not
from TOOL-NAME and the current Scope) prevents an old descriptor from pairing
its implementation with a later same-name resolver or approval preview."
  (and (symbolp function)
       (get function 'superchat-tools--installation)))

(defun superchat-tool-execution--plist-p (value)
  "Return non-nil when VALUE looks like a plist."
  (and (consp value)
       (cl-evenp (length value))
       (cl-loop for (key _value) on value by #'cddr
                always (keywordp key))))

(defun superchat-tool-execution--alist-p (value)
  "Return non-nil when VALUE looks like an alist."
  (and (consp value)
       (cl-every (lambda (item) (and (consp item) (not (listp (car item)))))
                 value)))

(defun superchat-tool-execution--snapshot-value (value)
  "Return a detached copy of mutable JSON-like VALUE."
  (cond
   ((stringp value) (copy-sequence value))
   ((hash-table-p value)
    (let ((copy (make-hash-table :test (hash-table-test value))))
      (maphash
       (lambda (key item)
         (puthash (superchat-tool-execution--snapshot-value key)
                  (superchat-tool-execution--snapshot-value item)
                  copy))
       value)
      copy))
   ((vectorp value)
    (apply #'vector
           (mapcar #'superchat-tool-execution--snapshot-value (append value nil))))
   ((consp value)
    (cons (superchat-tool-execution--snapshot-value (car value))
          (superchat-tool-execution--snapshot-value (cdr value))))
   (t value)))

(defun superchat-tool-execution--visible-tool-installation (tool-name &optional scope)
  "Return live Scope tool installation TOOL-NAME visible from SCOPE."
  (let ((installation
         (superchat-scope-lookup
          (superchat-execution-scope-resolve scope) 'tool tool-name)))
    (and (plist-get installation :live) installation)))

(defun superchat-tool-execution--installation-active-p (tool-name)
  "Whether the captured materialized TOOL-NAME installation is still current.

Built-ins and direct legacy Authority callers carry no installation and retain
their established behavior.  A plugin descriptor must still be the exact
nearest Scope installation when it reaches Authority; an old descriptor after
withdrawal or replacement therefore cannot start a new tool body."
  (let ((installation superchat-tool-execution--tool-installation))
    (or (null installation)
        (and (equal tool-name (plist-get installation :name))
             (plist-get installation :live)
             (eq installation
                 (superchat-tool-execution--visible-tool-installation tool-name))))))

(defun superchat-tool-execution--execution-id ()
  "Return a request-local tool execution identifier."
  (format "tool-%s-%06x"
          (format-time-string "%Y%m%dT%H%M%S%3N" nil t)
          (random #x1000000)))

(defun superchat-tool-execution--experience-block-reason (tool-name context)
  "Return a denial reason when a blocked Experience View owns TOOL-NAME.

The dependency list is explicit request policy.  A blocked View never blocks
unrelated tools, and Authority does not infer dependencies from Candidate text
or from the model prompt."
  (let* ((envelope (plist-get context :context-envelope))
         (view (and (superchat-tool-execution--plist-p envelope)
                    (plist-get envelope :experience-view)))
         (policy (and (superchat-tool-execution--plist-p envelope)
                      (plist-get envelope :experience-action-policy)))
         (status (and (superchat-tool-execution--plist-p view)
                      (plist-get view :status)))
         (kind (and (superchat-tool-execution--plist-p policy)
                    (plist-get policy :kind)))
         (phase (and (superchat-tool-execution--plist-p policy)
                     (plist-get policy :phase)))
         (dependent-tools
          (and (superchat-tool-execution--plist-p policy)
               (plist-get policy :dependent-tools)))
         (name (if (symbolp tool-name)
                   (symbol-name tool-name)
                 tool-name)))
    (when (and (or (member status '(blocked "blocked"))
                   (and (equal "workspace-revision-recovery" kind)
                        (equal "awaiting-info" phase)))
               (listp dependent-tools)
               (cl-some (lambda (candidate)
                         (equal name
                                (if (symbolp candidate)
                                    (symbol-name candidate)
                                  candidate)))
                       dependent-tools))
      (if (and (equal "workspace-revision-recovery" kind)
               (equal "awaiting-info" phase))
          (format "Experience recovery requires workspace_info; dependent action %s is not authorized"
                  name)
        (format "Experience View is blocked; dependent action %s is not authorized"
                name)))))

(defun superchat-tool-execution--event-base (type context payload &optional summary)
  "Build a Runtime event of TYPE from CONTEXT and PAYLOAD."
  (let ((event
         (list :type type
               :session-id (plist-get context :session-id)
               :actor-id (or (plist-get context :actor-id) "lead")
               :summary summary
               :payload payload)))
    (dolist (key '(:turn-id :run-id :profile-id :workspace-id
                   :correlation-id :causation-id :parent-run-id :team-id
                   :audience :sensitivity))
      (when (plist-get context key)
        (setq event (plist-put event key (plist-get context key)))))
    event))

(defun superchat-tool-execution--material (key value)
  "Return (KEY TEXT) for tool material VALUE, or nil when not stored.
TEXT is the positional arguments JSON-encoded for `:arguments-text', and the
normalized delivered text for `:result-text'.  Bounding happens later in
`superchat-tape-fit-tool-material'."
  (when (and superchat-tape-store-tool-material value)
    (list key
          (pcase key
            (:arguments-text
             (condition-case nil
                 (json-encode (vconcat value))
               (error (prin1-to-string value))))
            (:result-text
             (let ((normalized
                    (superchat-tool-execution--normalize-tool-result value)))
               (if (stringp normalized)
                   normalized
                 (prin1-to-string normalized))))
            (_ (prin1-to-string value))))))

(defun superchat-tool-execution--append-event
    (type tool-name context payload &optional summary requires-payload
          return-record)
  "Append a tool event and return its logical event identity."
  (superchat-tool-execution--require-current-evidence-origin context)
  (let* ((payload (append (list :tool-name tool-name) payload))
         (payload (if (member type '("tool.requested" "tool.completed"
                                     "tool.failed"))
                      (superchat-tape-fit-tool-material payload)
                    payload))
         (event
          (superchat-tool-execution--event-base
           type context payload
           (or summary (format "%s: %s" type tool-name))))
         (event
          (if requires-payload
              (progn
                (setq event (plist-put event :requires-payload t))
                (plist-put event :sensitivity 'restricted))
            event))
         (record (superchat-tape-append-event-record event)))
    (if return-record
        (append event record)
      (plist-get record :event-id))))

(defun superchat-tool-execution--evidence-fields (context)
  "Return attempt provenance fields from eligible tool CONTEXT."
  (when (plist-get context :evidence-contract)
    (list :evidence-contract (plist-get context :evidence-contract)
          :evidence-attempt (plist-get context :evidence-attempt)
          :decision-snapshot-event-id
          (plist-get context :decision-snapshot-event-id)
          :tool-call-id (plist-get context :tool-call-id)
          :tool-call-ordinal (plist-get context :tool-call-ordinal))))

(defun superchat-tool-execution--exact-fields (label value)
  "Return exact JSON receipt fields for LABEL and VALUE."
  (let ((receipt (superchat-tape-exact-json value)))
    (when (> (plist-get receipt :size) superchat-tape-max-inline-bytes)
      (error "Exact %s evidence exceeds the Tape inline limit" label))
    (list (intern (format ":%s-json" label)) (plist-get receipt :json)
          (intern (format ":%s-size" label)) (plist-get receipt :size)
          (intern (format ":%s-hash" label)) (plist-get receipt :hash))))

(defun superchat-tool-execution--require-current-evidence-origin (context)
  "Signal when eligible CONTEXT no longer names the active provider attempt."
  (when (plist-get context :evidence-contract)
    (let ((attempt (plist-get context :evidence-attempt))
          (snapshot-id (plist-get context :decision-snapshot-event-id))
          (validator (plist-get context :evidence-origin-validator)))
      (unless (and (integerp attempt) (> attempt 0)
                   (stringp snapshot-id) (not (string-empty-p snapshot-id))
                   (functionp validator)
                   (funcall validator attempt snapshot-id))
        (error "Tool evidence origin is missing, stale, or mismatched")))))

(defun superchat-tool-execution--validated-evidence-action
    (tool-name args context)
  "Return exact action receipt for TOOL-NAME ARGS and eligible CONTEXT."
  (when-let* ((contract (plist-get context :evidence-contract)))
    (unless (equal contract "epistemic-learning-v0")
      (error "Unknown tool evidence contract: %s" contract))
    (let ((attempt (plist-get context :evidence-attempt))
          (snapshot-id (plist-get context :decision-snapshot-event-id))
          (call-id (plist-get context :tool-call-id))
          (ordinal (plist-get context :tool-call-ordinal))
          (call-name (plist-get context :tool-call-name))
          (call-args (plist-get context :tool-call-arguments)))
      (unless (and (integerp attempt) (> attempt 0)
                   (stringp snapshot-id) (not (string-empty-p snapshot-id))
                   (stringp call-id) (not (string-empty-p call-id))
                   (integerp ordinal) (> ordinal 0)
                   (equal tool-name call-name)
                   (listp call-args))
        (error "Tool evidence origin is missing, stale, or mismatched"))
      (superchat-tool-execution--require-current-evidence-origin context)
      (superchat-tool-execution--exact-fields
       "arguments"
       (list :tool-name tool-name
             :call-id call-id
             :named-arguments (copy-tree call-args t)
             :positional-arguments (vconcat (copy-tree args t)))))))

(defun superchat-tool-execution--result-fingerprint (result)
  "Return bounded identity metadata for RESULT."
  (let* ((printed (prin1-to-string result))
         (encoded (encode-coding-string printed 'utf-8)))
    (list :result-size (string-bytes encoded)
          :result-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-tool-execution--run-hooks (event &rest args)
  "Emit tool lifecycle EVENT with ARGS."
  (apply #'superchat-emit (superchat-execution-scope-resolve nil) event args))

(defun superchat-tool-execution--filter-result (tool-name args result)
  "Return RESULT as the post-execution stage leaves it.
That stage is the `tools/post-execute' waterfall: a
listener may replace a string result, or block it, which turns the listener's
feedback into an error result."
  (progn
    (let ((outcome (superchat-emit
                    (superchat-execution-scope-resolve nil) 'tools/post-execute
                    (list :tool-name tool-name
                          :args (superchat-tool-execution--snapshot-value args)
                          :result result :kind 'accept))))
      (cond
       ((superchat-waterfall-refused-p outcome)
        (format "Error: tool result blocked by post-execute policy: %s"
                (superchat-waterfall-refusal-reason outcome)))
       ((eq 'block (plist-get outcome :kind))
        (format "Error: %s"
                (or (plist-get outcome :feedback)
                    "tool result blocked by post-execute policy")))
       ((stringp (plist-get outcome :result)) (plist-get outcome :result))
       (t result)))))

(defun superchat-tool-execution--implementation-receipt (result)
  "Return bounded receipt metadata distinct from a delivered Result."
  (let* ((printed (prin1-to-string result))
         (encoded (encode-coding-string printed 'utf-8)))
    (list :implementation-result-size (string-bytes encoded)
          :implementation-result-hash
          (concat "sha256:" (secure-hash 'sha256 encoded)))))

(defun superchat-tool-execution--notify-failure (tool-name args err)
  "Notify failure hooks with detached ARGS and ERR."
  (superchat-tool-execution--run-hooks
   'tool-failed
   tool-name
   (superchat-tool-execution--snapshot-value args)
   (superchat-tool-execution--snapshot-value err)))

(defun superchat-tool-execution--deliver (async callback result)
  "Return or asynchronously deliver RESULT."
  (if async
      (progn (funcall callback result) nil)
    result))

(defun superchat-tool-execution--run-active-p (context)
  "Return whether the Runtime owning CONTEXT still accepts tool work.

Contexts from non-Runtime callers omit `:run-active-p' and retain the
historical behavior.  A Runtime context must provide a callable predicate;
missing or failing predicates fail closed so a late approval cannot reach the
tool implementation."
  (if (not (plist-member context :run-active-p))
      t
    (let ((predicate (plist-get context :run-active-p)))
      (and (functionp predicate)
           (condition-case nil
             (funcall predicate)
             (error nil))))))

(defun superchat-tool-execution--execution-active-p (tool-name context)
  "Return whether TOOL-NAME may still begin native execution for CONTEXT."
  (and (superchat-tool-execution--run-active-p context)
       (superchat-tool-execution--installation-active-p tool-name)))

(defun superchat-tool-execution--row-builtin-unmounted (tool-name)
  "Return the row owning TOOL-NAME when its built-in registration is absent.
A built-in tool a default row owns is selectable only while that row is
mounted; with the registration gone the row's name is returned so the call
can be refused before any body starts.  Nil for every other tool."
  (when (fboundp 'superchat-tools-row-builtin-owner)
    (let ((row (superchat-tools-row-builtin-owner tool-name)))
      (when (and row (not (superchat-tools-builtin-descriptor-p tool-name)))
        row))))

(defun superchat-tool-execution--note-runtime-work (context execution-id action)
  "Report ACTION for EXECUTION-ID to Runtime when Runtime is available.
The call is a no-op outside a Runtime-provided Run context, in any process
where `superchat-runtime.el' is not loaded, or with a malformed identity."
  (when (and (fboundp 'superchat-runtime--note-open-work)
             (stringp (plist-get context :run-id))
             (not (string-empty-p (plist-get context :run-id)))
             (stringp execution-id)
             (not (string-empty-p execution-id)))
    (funcall #'superchat-runtime--note-open-work
             (plist-get context :run-id) execution-id action)))

(defun superchat-tool-execution--inactive-result (tool-name)
  "Return the truthful refusal text for inactive TOOL-NAME."
  (if (superchat-tool-execution--installation-active-p tool-name)
      "[Tool call blocked: Run is no longer active]"
    "[Tool call blocked: Tool is no longer available]"))

(defun superchat-tool-execution--record-inactive-fault
    (tool-name context execution-id)
  "Diagnose an execution gate without fabricating an Effect disposition."
  (superchat-event-record-fault
   'tool-liveness execution-id 'tool-execution
   (list :tool tool-name :run-id (plist-get context :run-id)
         :reason (if (superchat-tool-execution--installation-active-p tool-name)
                     'run-inactive
                   'installation-inactive))))

(defun superchat-tool-execution--record-failure
    (tool-name context execution-id status
               &optional result exact-error metadata)
  "Record a failed or refused tool execution."
  (let ((record
         (superchat-tool-execution--append-event
          "tool.failed" tool-name context
          (append
           (list :execution-id execution-id
                 :status status)
           (superchat-tool-execution--material :result-text result)
           metadata
           (when-let* ((block-reason (plist-get context :block-reason)))
             (list :block-reason block-reason))
           (when-let* ((error-type (plist-get exact-error :error-type)))
             (list :error-type error-type))
           (superchat-tool-execution--evidence-fields context)
           (when (plist-get context :evidence-contract)
             (superchat-tool-execution--exact-fields
              "error" (or exact-error (list :status status :result result)))))
          nil (and (plist-get context :evidence-contract) t) t)))
    (superchat-tool-execution--note-runtime-work
     context execution-id 'effect-settled)
    (unless (equal "user-quit" (plist-get metadata :interruption))
      (when-let* ((callback (plist-get context :failure-recorded-function)))
        (when (functionp callback)
          (funcall callback record)))
      (when-let* ((callback (plist-get context :recovery-failure-function)))
        (when (functionp callback)
          (condition-case err
              (funcall callback
                       (list :event record
                             :tool-name tool-name
                             :args (copy-tree
                                    (plist-get context
                                               :recovery-failure-args)
                                    t)
                             :status status
                             :result result
                             :error exact-error))
            (error
             (display-warning
              'superchat-tool-execution
              (format "Tool failure observer failed: %s"
                      (error-message-string err))
              :warning))))))
    record))

(defun superchat-tool-execution--handle-quit
    (condition tool-name args context execution-id phase
               &optional implementation-returned-p implementation-result
               outcome-durable failure-status)
  "Account for CONDITION at PHASE, interrupt its Run, then re-signal it."
  (let ((audit-ok outcome-durable)
        (failure-record nil))
    (unless outcome-durable
      (condition-case audit-condition
          (progn
            (setq failure-record
                  (superchat-tool-execution--record-failure
                   tool-name context execution-id
                   (or failure-status "failed")
                   "[Tool call interrupted by user]"
                   (list :error-type "quit"
                         :error-data (copy-tree (cdr condition) t)
                         :message "Quit")
                   (append
                    (list :interruption "user-quit" :phase phase)
                    (when (equal phase "implementation")
                      (list :effects-may-be-partial t))
                    (when implementation-returned-p
                      (superchat-tool-execution--implementation-receipt
                       implementation-result)))))
            (setq audit-ok t))
        ((error quit)
         (when-let* ((audit-failure
                      (plist-get context :audit-failure-function)))
           (when (functionp audit-failure)
             (condition-case nil
                 (funcall audit-failure audit-condition
                          "[Tool interruption audit failed]")
               ((error quit) nil)))))))
    (when failure-record
      (condition-case nil
          (superchat-tool-execution--notify-failure tool-name args condition)
        ((error quit) nil)))
    (when audit-ok
      (when-let* ((interrupt (plist-get context :interrupt-function)))
        (when (functionp interrupt)
          (condition-case nil
              (funcall interrupt)
            ((error quit) nil)))))
    (signal (car condition) (cdr condition))))

(defun superchat-tool-execution--finish-success
    (tool-name args result context execution-id async callback)
  "Filter, audit, and deliver successful RESULT."
  (let* ((evidence (plist-get context :evidence-contract))
         (completed-record nil)
         (result (superchat-tool-execution--snapshot-value result))
         (audit
         (condition-case err
             (progn
               (superchat-tool-execution--require-current-evidence-origin context)
               ;; Reject sensitive raw results before compatibility hooks can
               ;; render or copy them outside the exact-evidence seam.
               (when evidence
                 (superchat-tool-execution--exact-fields
                  "result" (list :result result)))
               (setq result
                     (superchat-tool-execution--filter-result
                      tool-name args result))
               ;; Detach from filter-owned data before persistence and
               ;; provider delivery on both ordinary and exact paths.
               (setq result (superchat-tool-execution--snapshot-value result))
               (setq completed-record
                     (superchat-tool-execution--append-event
                      "tool.completed" tool-name context
                      (append
                       (list :execution-id execution-id
                             :status "succeeded")
                       (when-let* ((target (plist-get context :mutation-target))
                                   (name (plist-get target :buffer)))
                         (append (list :buffer name)
                                 (when (plist-get target :file)
                                   (list :buffer-file (plist-get target :file)))))
                       (superchat-tool-execution--result-fingerprint result)
                       (superchat-tool-execution--material :result-text result)
                       (superchat-tool-execution--evidence-fields context)
                       (when evidence
                         (superchat-tool-execution--exact-fields
                          "result" (list :result result))))
                      nil (and evidence t) t))
               (superchat-tool-execution--note-runtime-work
                context execution-id 'effect-settled)
               ;; Runtime owns recovery state.  Notify it only after the
               ;; durable success event exists; a bookkeeping failure must not
               ;; turn a successfully executed tool into an audit failure.
               (when-let* ((observer
                           (plist-get context :tool-completed-function)))
                 (when (functionp observer)
                   (condition-case err
                       (funcall
                        observer
                        (list :tool-name tool-name
                              :args (superchat-tool-execution--snapshot-value args)
                              :result (superchat-tool-execution--snapshot-value
                                       result)
                              :event completed-record
                              :execution-id execution-id))
                     (error
                      (display-warning
                       'superchat-tool-execution
                       (format "Tool completion observer failed: %s"
                               (error-message-string err))
                       :warning)))))
               (superchat-tool-execution--run-hooks
                 'tool-after
                  tool-name args
                  (superchat-tool-execution--snapshot-value result)
                  (and (listp context) (plist-get context :run-id)))
               (superchat-tool-execution--require-current-evidence-origin context)
               'complete)
           (error (cons 'failed err))
           (quit
            (superchat-tool-execution--handle-quit
             err tool-name args context execution-id
             "result-processing" t result (and completed-record t))))))
    (if (eq audit 'complete)
        (superchat-tool-execution--deliver async callback result)
      (let* ((evidence (plist-get context :evidence-contract))
             (err (cdr audit))
             (failure
              (if evidence
                  "[Tool result withheld because exact audit failed]"
                (format "[Tool completed but audit failed: %s]"
                        (error-message-string err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
        (if (functionp audit-failure)
            (progn
              (funcall audit-failure err failure)
              ;; Runtime has closed the Run and cancelled its provider request.
              ;; A delayed async callback would only reopen the transport path.
              (unless async failure))
          (superchat-tool-execution--deliver async callback failure))))))

(defun superchat-tool-execution--finish-error
    (tool-name args err context execution-id async callback)
  "Audit ERR from TOOL-NAME and deliver a model-readable failure."
  (let* ((message (error-message-string err))
         (result (format "[Tool call failed: %s]" message))
         (exact-error
          (list :error-type (format "%s" (car err))
                :error-data (copy-tree (cdr err) t)
                :message message))
         (audit
          (condition-case audit-err
              (progn
                (superchat-tool-execution--require-current-evidence-origin context)
                (superchat-tool-execution--record-failure
                 tool-name context execution-id
                 "failed" result exact-error)
                (superchat-tool-execution--notify-failure tool-name args err)
                (superchat-tool-execution--require-current-evidence-origin context)
                'complete)
            (quit
             (superchat-tool-execution--handle-quit
              audit-err tool-name args context execution-id
              "result-processing" nil nil t))
            (error (cons 'failed audit-err)))))
    (if (eq audit 'complete)
        (superchat-tool-execution--deliver async callback result)
      (let* ((audit-err (cdr audit))
             (failure
              (if (plist-get context :evidence-contract)
                  "[Tool error withheld because exact audit failed]"
                (format "%s [Audit also failed: %s]"
                        result (error-message-string audit-err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
        (if (functionp audit-failure)
            (progn
              (funcall
               audit-failure audit-err
               "[Tool error withheld because exact audit failed]")
              (unless async failure))
          (superchat-tool-execution--deliver async callback failure))))))

(defun superchat-tool-execution--normalize-tool-result (value)
  "Normalize textual tool results at the shared execution seam.
A byte-level read (e.g. coding-system-for-read bound to binary) or an
encoding artifact leaves raw bytes inside a tool result string, and
Emacs 31's json-serialize rejects that string — killing the next
streaming request with `wrong-type-argument json-value-p'.  Decode
those back to Unicode here, before the result forks to the Tape, the
renderer, and the provider.  Non-string values pass through."
  (if (not (stringp value))
      value
    (if (or (and (not (multibyte-string-p value))
                 (string-match-p "[^\000-\177]" value))
            (and (multibyte-string-p value)
                 (catch 'raw-byte
                   (mapc (lambda (c)
                           (when (> c #x3FFF7F) (throw 'raw-byte t)))
                         value))))
        ;; Decode the underlying bytes as UTF-8; a failed decode keeps
        ;; the original value so behavior degrades, never hard-fails.
        (condition-case nil
            (decode-coding-string (string-as-unibyte value) 'utf-8)
          (error value))
      value)))

(defun superchat-tool-execution--input-read-receipt (expected)
  "Read the exact stored EXPECTED input outcome, rejecting any conflict.
Ordinary results have a fingerprint, not durable raw result text."
  (let* ((events (superchat-tape-replay-events (plist-get expected :session-id)))
         (execution (plist-get (plist-get expected :payload) :execution-id))
         (matches (cl-remove-if-not
                   (lambda (event)
                     (or (equal (plist-get event :event-id) (plist-get expected :event-id))
                         (and (member (plist-get event :type) '("tool.completed" "tool.failed"))
                              (equal execution (plist-get (plist-get event :payload) :execution-id)))))
                   events)))
    (when matches
      (unless (= (length matches) 1) (error "Conflicting input outcome receipts"))
      (let ((actual (car matches)))
        (dolist (key (append '(:event-id :type :schema :session-id :payload)
                             (mapcar #'car superchat-tape--envelope-keys)))
          (unless (and (eq (not (null (plist-member actual key)))
                           (not (null (plist-member expected key))))
                       (equal (plist-get actual key) (plist-get expected key)))
            (error "Input receipt mismatch at %s" key)))
        (unless (and (integerp (plist-get actual :storage-id))
                     (> (plist-get actual :storage-id) 0)
                     (stringp (plist-get actual :recorded-at)))
          (error "Input receipt has no stored identity"))
        actual))))

(defun superchat-tool-execution--make-human-input-operation (run-id token)
  "Create the built-in operation for one accepted Run; no reverse require.
The native callback is intentionally not used to acknowledge waiting."
  (lambda (_done question)
    (let* ((context superchat--tool-execution-context)
           (capability (plist-get context :input-settlement))
           (identity (and (functionp capability) (funcall capability 'snapshot))))
      (unless (and (equal run-id (plist-get identity :run-id))
                   (eq token (plist-get identity :token))
                   (stringp question) (not (string-empty-p (string-trim question))))
        (error "Invalid trusted human input invocation"))
      (superchat-runtime--request-input capability (list :question question)))))

(defun superchat-tool-execution--make-input-settlement
    (tool-name args context execution-id callback)
  "Create the NONSHIPPING invocation's single settlement capability."
  (let (claim expected receipt (receipt-state 'not-attempted)
              (delivery 'not-started) result fault)
    (lambda (operation &rest values)
      (pcase operation
        ('snapshot
         (list :claim claim :receipt-state receipt-state
               :receipt (superchat-tool-execution--snapshot-value receipt)
               :delivery delivery :fault fault
               :run-id (plist-get context :run-id)
               :session-id (plist-get context :session-id)
               :token (plist-get context :input-core-token)))
        ('settle
         (unless claim
           ;; Claim is the only winner, and precedes every callback/append.
           (setq claim (car values) fault 'prepare)
           (unless (memq claim '(answer failed)) (error "Invalid input outcome"))
           (setq result (superchat-tool-execution--snapshot-value
                         (superchat-tool-execution--normalize-tool-result (cadr values))))
           (when (eq claim 'answer)
             (setq result (superchat-tool-execution--snapshot-value
                           (superchat-tool-execution--filter-result tool-name args result))))
           (let* ((event-payload
                   (append
                    (list :tool-name tool-name :execution-id execution-id
                          :status (if (eq claim 'answer) "succeeded" "failed"))
                    (if (eq claim 'answer)
                        (append (superchat-tool-execution--result-fingerprint result)
                                (superchat-tool-execution--material
                                 :result-text result))
                      (append (list :error-type "input-stopped" :reason result)
                              (superchat-tool-execution--material
                               :result-text result)))))
                  (event-payload (superchat-tape-fit-tool-material event-payload))
                  (event (superchat-tool-execution--event-base
                          (if (eq claim 'answer) "tool.completed" "tool.failed")
                          context event-payload))
                  (payload (superchat-tape--encoded-payload (plist-get event :payload))))
             (when (plist-get payload :omitted) (error "Input receipt payload omitted"))
             (setq event (plist-put event :schema 1)
                   event (plist-put event :event-id (superchat-tape--new-event-id)))
             ;; Compare the exact representation returned by Tape replay.
             (setq expected
                   (json-parse-string
                    (json-encode (plist-put (copy-tree event t) :payload payload))
                    :object-type 'plist :array-type 'list :null-object nil :false-object nil)
                   fault 'audit receipt-state 'attempting)
             (unwind-protect
                 (superchat-tape-append-event-record event)
               ;; A real append can commit and then signal.  Never infer
               ;; absence from its nonlocal exit or attempt another outcome.
               (condition-case nil
                   (setq receipt (superchat-tool-execution--input-read-receipt expected)
                         receipt-state (if receipt 'committed 'absent))
                 ((error quit) (setq receipt-state 'unknown))))
             (unless (eq receipt-state 'committed) (error "Input receipt not verifiable"))
             (superchat-tool-execution--note-runtime-work
              context execution-id 'effect-settled)
             (setq fault 'observer)
             (if (eq claim 'answer)
                 (progn
                   (when-let* ((observer (plist-get context :tool-completed-function)))
                     (funcall observer
                              (list :tool-name tool-name :args args :result result
                                    :event receipt :execution-id execution-id)))
                   (superchat-tool-execution--run-hooks
                    'tool-after
                    tool-name args result
                    (and (listp context) (plist-get context :run-id))))
               (when-let* ((observer (plist-get context :failure-recorded-function)))
                 (funcall observer receipt)))
             (setq fault nil)
             t)))
        ('deliver
         (when (and (eq claim 'answer) (eq receipt-state 'committed)
                    (eq delivery 'not-started)
                    (superchat-tool-execution--run-active-p context))
           (setq delivery 'entered)
           (condition-case condition
               (prog1 (funcall callback result) (setq delivery 'returned))
             ((error quit)
              (setq delivery 'raised)
              (signal (car condition) (cdr condition))))))
        (_ (error "Unknown input settlement operation"))))))

(defun superchat-tool-execution--invoke-async
    (tool-name original-fn args context execution-id callback)
  "Invoke asynchronous ORIGINAL-FN once and account for its outcome."
  (if (and (plist-get context :input-core-token)
           (equal tool-name (plist-get context :input-core-tool-name)))
      (let* ((execution-scope (superchat-execution-scope-resolve nil))
             (installation superchat-tool-execution--tool-installation)
             (capability (superchat-tool-execution--make-input-settlement
                          tool-name args context execution-id callback))
             (superchat--tool-execution-context
              (plist-put (superchat-tool-execution--snapshot-value context)
                         :input-settlement capability)))
        (let ((superchat-execution-scope execution-scope)
              (superchat-tool-execution--tool-installation installation))
          (apply original-fn
                 (lambda (value)
                   (let ((superchat-execution-scope execution-scope)
                         (superchat-tool-execution--tool-installation installation))
                     (when (funcall capability 'settle 'answer value)
                       (funcall capability 'deliver))))
                 args)))
    (superchat-tool-execution--invoke-async-default
     tool-name original-fn args context execution-id callback)))

(defun superchat-tool-execution--invoke-async-default
    (tool-name original-fn args context execution-id callback)
  "Preserve the ordinary async execution and consumer condition contract."
  (let ((completed nil)
        ;; Runtime releases its private Run reference at quiesce.  A native
        ;; callback must therefore carry the invocation's borrowed Scope now,
        ;; rather than attempt to recover it from a terminal Run later.
        (execution-scope (superchat-execution-scope-resolve nil))
        (installation superchat-tool-execution--tool-installation))
    (condition-case condition
        (let ((superchat-execution-scope execution-scope)
              (superchat-tool-execution--tool-installation installation)
              (superchat--tool-execution-context
               (superchat-tool-execution--snapshot-value context)))
          (apply
           original-fn
           (cons
            (lambda (result)
              (unless completed
                (setq completed t)
                (let ((superchat-execution-scope execution-scope)
                      (superchat-tool-execution--tool-installation installation))
                  ;; A genuine late result is a fact about an already-entered
                  ;; Effect.  Do not recheck installation liveness here: the
                  ;; invocation gate already prevented new work, while this
                  ;; completion must still settle truthful residual evidence.
                  (superchat-tool-execution--finish-success
                   tool-name args
                   (superchat-tool-execution--normalize-tool-result result)
                   context execution-id t callback))))
            args)))
      (error
       (unless completed
         (setq completed t)
         (superchat-tool-execution--finish-error
          tool-name args condition context execution-id
          t callback)))
      (quit
       (unless completed
         (setq completed t)
         (superchat-tool-execution--handle-quit
          condition tool-name args context execution-id
          "implementation"))
       (signal (car condition) (cdr condition))))))

(defun superchat-tool-execution--invoke-sync
    (tool-name original-fn args context execution-id )
  "Invoke synchronous ORIGINAL-FN once and account for its outcome."
  (let ((outcome
         (condition-case condition
             (let ((superchat-execution-scope
                    (superchat-execution-scope-resolve nil))
                   (superchat-tool-execution--tool-installation
                    superchat-tool-execution--tool-installation)
                   (superchat--tool-execution-context
                    (superchat-tool-execution--snapshot-value context))
                   (superchat-tool-mutation-target nil))
               (let ((value (superchat-tool-execution--normalize-tool-result
                             (apply original-fn args))))
                 (when superchat-tool-mutation-target
                   (setq context
                         (plist-put (copy-sequence context) :mutation-target
                                    superchat-tool-mutation-target)))
                 (cons 'ok value)))
           (error (cons 'error condition))
           (quit
            (superchat-tool-execution--handle-quit
             condition tool-name args context execution-id
             "implementation")))))
    (if (eq (car outcome) 'ok)
        (superchat-tool-execution--finish-success
         tool-name args (cdr outcome) context execution-id nil nil)
      (superchat-tool-execution--finish-error
       tool-name args (cdr outcome) context execution-id nil nil))))

(defun superchat-tool-execution--invoke
    (tool-name original-fn args context execution-id
               async callback &optional refuse)
  "Invoke ORIGINAL-FN after the authority and audit gates.
REFUSE, when supplied, delivers a preexecution liveness refusal.

The `tools/execute' waterfall wraps the whole
invocation of a live call: the chain ends in it, so the body runs at most
once and only when every listener delegates.  A listener that returns a
result instead ends the call with it, and a refusal or a fault blocks it;
the call is then recorded as blocked, never as started, and delivered once.
Trusted human input and a call whose Run is no longer live keep the plain
path."
  (if (or (and (plist-get context :input-core-token) (equal tool-name "ask_user"))
          (not (superchat-tool-execution--execution-active-p tool-name context)))
      (superchat-tool-execution--invoke-body
       tool-name original-fn args context execution-id
       async callback refuse)
    (let* ((started nil)
           (returned nil)
           (answer
            (let ((superchat-event-waterfall-terminal
                   (lambda (call)
                     (setq started t
                           returned (superchat-tool-execution--invoke-body
                                     tool-name original-fn args context
                                     execution-id
                                     async callback refuse))
                     (plist-put (copy-sequence call) :started t))))
              (superchat-emit
               (superchat-execution-scope-resolve nil) 'tools/execute
               (list :tool-name tool-name
                     :args (superchat-tool-execution--snapshot-value args)
                     :session-id (plist-get context :session-id)
                     :run-id (plist-get context :run-id))))))
      (if started
          returned
        (let ((result
               (cond
                ((superchat-waterfall-refused-p answer)
                 (format "[Tool call blocked: %s]"
                         (superchat-waterfall-refusal-reason answer)))
                ((stringp (plist-get answer :result)) (plist-get answer :result))
                (t "[Tool call blocked: a tools/execute listener ended the call without a result]"))))
          (superchat-tool-execution--record-failure
           tool-name context execution-id "blocked" result)
          (if refuse
              (funcall refuse result)
            (superchat-tool-execution--deliver async callback result)))))))

(defun superchat-tool-execution--invoke-body
    (tool-name original-fn args context execution-id
               async callback &optional refuse)
  "Invoke ORIGINAL-FN after the authority and audit gates.
REFUSE, when supplied, delivers a preexecution liveness refusal."
  (if (not (superchat-tool-execution--execution-active-p tool-name context))
      (let ((result (superchat-tool-execution--inactive-result tool-name)))
        ;; Losing execution authority is not an Effect disposition.  Preserve
        ;; unresolved Tape evidence and diagnose without fabricating a failure.
        (superchat-tool-execution--record-inactive-fault
         tool-name context execution-id)
        (if refuse
            (funcall refuse result)
          (superchat-tool-execution--deliver async callback result)))
    (progn
      ;; Persist before any compatibility hook or tool code can produce
      ;; effects.  An inactive Run returned above no longer needs an evidence
      ;; origin check because it cannot reach ORIGINAL-FN.
      (superchat-tool-execution--require-current-evidence-origin context)
      (superchat-tool-execution--note-runtime-work context execution-id 'effect-entered)
      (let* ((started-id
              (superchat-tool-execution--append-event
               "tool.execution.started" tool-name context
               (list :execution-id execution-id
                     :status "executing")))
             (context
              (plist-put (copy-sequence context) :causation-id started-id)))
      (condition-case condition
          (superchat-tool-execution--run-hooks
           'tool-before
           tool-name (superchat-tool-execution--snapshot-value args)
           (and (listp context) (plist-get context :run-id)))
        (quit
         (superchat-tool-execution--handle-quit
          condition tool-name args context execution-id "pre")))
      (superchat-tool-execution--require-current-evidence-origin context)
      (if (or (not (superchat-tool-execution--execution-active-p tool-name context))
              (and (plist-get context :input-core-token)
                   (equal tool-name "ask_user")
                   (not (superchat-runtime--trusted-input-operation-p
                         context original-fn async))))
          (let* ((human-input (and (equal tool-name "ask_user") (plist-get context :input-operation)))
                 (result (if human-input "[Human input adapter is no longer available]"
                           (superchat-tool-execution--inactive-result tool-name))))
            ;; The trusted input operation has not run.  Its admitted failure
            ;; is truthful, but a failed receipt must never reach native done.
            ;; Ordinary late-work accounting keeps its existing behavior.
            (let (audit-condition)
              (if (and human-input
                       (superchat-tool-execution--execution-active-p tool-name context))
                  (condition-case condition
                      (superchat-tool-execution--record-failure
                       tool-name context execution-id "failed" result
                       '(:error-type "input-adapter-unavailable"))
                    ((error quit) (setq audit-condition condition)))
                (superchat-tool-execution--record-inactive-fault
                 tool-name context execution-id))
              (if audit-condition
                  (unwind-protect
                      (when-let* ((failure (plist-get context :audit-failure-function)))
                        (funcall failure audit-condition "[Human input failure receipt is incomplete]"))
                    (when (eq (car audit-condition) 'quit)
                      (signal (car audit-condition) (cdr audit-condition))))
                (if refuse (funcall refuse result)
                  (superchat-tool-execution--deliver async callback result)))))
        (if async
            (superchat-tool-execution--invoke-async
             tool-name original-fn args context execution-id
             callback)
          (superchat-tool-execution--invoke-sync
           tool-name original-fn args context execution-id )))))))

;;;###autoload
(cl-defun superchat-tool-execute
    (tool-name original-fn args &key async callback context)
  "Execute TOOL-NAME through the one tool pipeline and its Tape audit.

ORIGINAL-FN receives normalized ARGS.  When ASYNC is non-nil it receives
CALLBACK as its first argument; otherwise the filtered result is returned.
CONTEXT must include `:session-id'.  Who may run what is decided by the
Session's permission level (`superchat-permission-level-decision'); a call
that must be asked about goes to the approval seam."
  ;; Bind this invocation's identity even for a raw legacy callable: a nested
  ;; `superchat-tool-execute' must not inherit its caller's installation.
  ;; A withdrawn proxy is intentionally no longer `functionp', but still
  ;; carries its revoked identity so the initial liveness gate can return the
  ;; ordinary refusal rather than accidentally treating it as legacy.
  (let* ((superchat-tool-execution--tool-installation
          (superchat-tool-execution--installation-from-function original-fn))
         (session-id (plist-get context :session-id))
         refusal-delivery-started
         (refuse (lambda (result)
                   (setq refusal-delivery-started t)
                   (superchat-tool-execution--deliver async callback result))))
    (unless (or (functionp original-fn)
                superchat-tool-execution--tool-installation)
      (error "Tool %s has no callable implementation" tool-name))
    (unless (listp args)
      (error "Tool %s arguments must be a list" tool-name))
    (when (and async (not (functionp callback)))
      (error "Async tool %s requires a callback" tool-name))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Tool execution requires context :session-id"))
    ;; A first arrival on a terminal Run, or through a revoked materialized
    ;; installation, was never admitted to execution audit.
    (unless (superchat-tool-execution--execution-active-p tool-name context)
      (cl-return-from superchat-tool-execute
        (superchat-tool-execution--deliver
         async callback (superchat-tool-execution--inactive-result tool-name))))
    ;; ── before the decision: normalize only, never decide ─────────────
    ;; Liveness, identity, argument normalization and the block check all
    ;; live here; none returns a verdict.
    (condition-case outer-err
        (let* ((provider-args (superchat-tool-execution--snapshot-value args))
               (evidence-action
                (superchat-tool-execution--validated-evidence-action
                 tool-name provider-args context))
               (context
                (if evidence-action
                    (plist-put
                     (copy-sequence context) :causation-id
                     (plist-get context :decision-snapshot-event-id))
                  context))
               (normalizer (plist-get context :normalize))
               (args (if (functionp normalizer)
                         (funcall normalizer args)
                       args))
               (args (superchat-tool-execution--snapshot-value args))
               (context
                (plist-put (copy-sequence context)
                           :recovery-failure-args
                           (copy-tree args t)))
               (_evidence-arguments
                (when (and evidence-action
                           (not (equal provider-args args)))
                  (error "Eligible tool arguments changed before execution")))
               (validator (plist-get context :validate))
               (_ (when (functionp validator)
                    (funcall validator
                             (superchat-tool-execution--snapshot-value args))))
               (input-candidate (equal tool-name "ask_user"))
               (trusted-input
                (and input-candidate
                     (fboundp 'superchat-runtime--trusted-input-operation-p)
                     (superchat-runtime--trusted-input-operation-p context original-fn async)))
               (experience-block
                (superchat-tool-execution--experience-block-reason
                 tool-name context))
               ;; ── blocks: reasons a call cannot run whatever the level ──
               (block-reason
                (or (and input-candidate (not trusted-input) "Untrusted human input operation")
                    (and trusted-input
                         (not (and (= (length args) 1) (stringp (car args))
                                   (not (string-empty-p (string-trim (car args))))))
                         "Human input requires one nonempty question string")
                    experience-block
                    (plist-get context :block)))
               (blocked (and block-reason t))
               ;; A row-owned built-in whose row was unmounted after this tool
               ;; was materialized must not start a body either.
               (row-builtin-unmounted
                (superchat-tool-execution--row-builtin-unmounted tool-name))
               (context
                (if block-reason
                    (plist-put (copy-sequence context)
                               :block-reason block-reason)
                  context))
               ;; ── the decision ─────────────────────────────────────────
               ;; A blocked call is denied.  Trusted human input is a
               ;; question to the person, not an effect to approve: it runs
               ;; unless the Session never asks anyone.  Everything else is
               ;; the level stage's: the `tools/pre-execute' waterfall, then
               ;; the level rule, the stronger of the two standing.
               (level
                (and (not blocked) (not trusted-input)
                     (superchat-permission-level-decision
                      tool-name args context
                      (superchat-execution-scope-resolve nil))))
               (decision
                (cond
                 (blocked 'deny)
                 (trusted-input
                  (if (eq 'never (superchat-approval-effective-policy session-id))
                      'deny
                    'allow))
                 (t (plist-get level :decision))))
               ;; Invariant 3: the decision is known and well-formed before
               ;; any tool body runs.
               (_pipeline-invariant
                (superchat-invariant-check
                 'tool-execution-passes-the-pipeline
                 (and (listp context) (plist-get context :session-id))
                 (and (listp context) (plist-get context :run-id))
                 tool-name decision))
               (disposition
                (when input-candidate (format "human-input-v1:%s" decision)))
               (execution-id (or (plist-get context :execution-id)
                                 (superchat-tool-execution--execution-id)))
               (_request-active
                (superchat-tool-execution--require-current-evidence-origin context))
               (_admission-active
                ;; Normalizers, validators and `tools/pre-execute' listeners
                ;; may cancel a Run or withdraw this materialized
                ;; installation.  Recheck before persisting/starting any new
                ;; native work.
                (unless (superchat-tool-execution--execution-active-p tool-name context)
                  (cl-return-from superchat-tool-execute
                    (funcall refuse
                             (superchat-tool-execution--inactive-result
                              tool-name)))))
               ;; ── body: entered only after the decision is well-formed ─
               ;; `tool.requested' is already on the Tape (invariant 3).
               (requested-id
                (superchat-tool-execution--append-event
                 "tool.requested" tool-name context
                 (append
                  (list :execution-id execution-id
                        :argument-count (length args)
                        :decision (symbol-name decision))
                  (when disposition (list :disposition disposition))
                  (when block-reason
                    (list :block-reason block-reason))
                  (superchat-tool-execution--material :arguments-text provider-args)
                  (superchat-tool-execution--evidence-fields context)
                  evidence-action)
                 nil (and evidence-action t)))
               (_
                (when-let* ((recorded
                             (plist-get context
                                        :evidence-action-recorded-function)))
                  (funcall recorded)))
               (_render-active
                (superchat-tool-execution--require-current-evidence-origin context))
               (context
                (plist-put
                 (copy-sequence context) :causation-id requested-id)))
          ;; ── finalize: every branch delivers through `refuse' or the ──
          ;; exact Sink, so a refusal reaches the consumer as a result does.
          (cond
           (row-builtin-unmounted
            (let ((result (format "[Tool call refused: the %s row is not mounted]"
                                  row-builtin-unmounted)))
              ;; The refused call is one audited fact, not an Effect: the same
              ;; shape as the blocked/denied refusals (the inactive gate path
              ;; records a diagnostic fault instead, or nothing at all).
              (superchat-tool-execution--record-failure
               tool-name context execution-id "blocked" result)
              (funcall refuse result)))
           (blocked
            (let ((result (format "[Tool call blocked: %s]" block-reason)))
              (superchat-tool-execution--record-failure
               tool-name context execution-id "blocked" result)
              (funcall refuse result)))
           ((eq decision 'deny)
            (let ((result (format "[Tool call denied: %s]"
                                  (or (plist-get level :reason)
                                      "the permission level does not grant it"))))
              (superchat-tool-execution--record-failure
               tool-name context execution-id "denied" result)
              (funcall refuse result)))
           ((eq decision 'ask)
            ;; The approval seam owns this round: the open-approval mark, the
            ;; `approval.requested' / `approval.decided' pair and the policy.
            ;; What stays here is the executor's part -- a quit interrupts
            ;; the Run, a refusal is recorded and delivered once, and the
            ;; body runs from the arguments the approval was given for.
            (let* ((answer
                    (if (not (and (stringp (plist-get context :run-id))
                                  (stringp (plist-get context :turn-id))))
                        (list :outcome 'unavailable)
                      (condition-case condition
                          (superchat-approval-request-snapshot
                           (list :session-id session-id
                                 :run-id (plist-get context :run-id)
                                 :turn-id (plist-get context :turn-id)
                                 :actor-id (plist-get context :actor-id)
                                 :execution-id execution-id
                                 :tool-name tool-name
                                 :args args
                                 :call-id (plist-get context :tool-call-id)
                                 :reason (plist-get level :reason))
                           (superchat-execution-scope-resolve nil))
                        (superchat-approval-outside-run
                         (list :outcome 'unavailable))
                        (quit
                         (superchat-tool-execution--handle-quit
                          condition tool-name args context
                          execution-id "approval"
                          nil nil nil "cancelled")))))
                   (outcome (plist-get answer :outcome)))
              (if (eq outcome 'allowed-once)
                  (superchat-tool-execution--invoke
                   tool-name original-fn (plist-get answer :args) context
                   execution-id async callback refuse)
                (let ((refusal
                       (pcase outcome
                         ('unavailable
                          "[Tool call refused: no approval provider is available]")
                         ('rejected
                          (if (eq 'never (superchat-approval-effective-policy session-id))
                              "[Tool call denied: approval prompts are disabled in this session]"
                            "[Tool call cancelled by user]"))
                         (_ "[Tool call cancelled by user]"))))
                  (superchat-tool-execution--record-failure
                   tool-name context execution-id
                   "cancelled" refusal)
                  (funcall refuse refusal)))))
           (t
            (superchat-tool-execution--invoke
             tool-name original-fn args context execution-id
             async callback refuse))))
      (error
       (when refusal-delivery-started
         (signal (car outer-err) (cdr outer-err)))
       ;; Most importantly, a failed pre-execution Tape write lands here before
       ;; ORIGINAL-FN is reached.
       (let* ((evidence (plist-get context :evidence-contract))
              (failure
               (if evidence
                   "[Tool call blocked because exact audit failed]"
                 (format "[Tool call blocked because audit failed: %s]"
                         (error-message-string outer-err))))
             (audit-failure
              (plist-get context :audit-failure-function)))
         (if (and evidence (functionp audit-failure))
             (progn
               (funcall audit-failure outer-err failure)
               (unless async failure))
           (superchat-tool-execution--deliver async callback failure)))))))

(provide 'superchat-tool-execution)

;;; superchat-tool-execution.el ends here
