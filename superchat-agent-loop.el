;;; superchat-agent-loop.el --- Agent execution loop for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agent mode coordinates multi-turn tool use for `type: agent' presets.
;; The actual multi-turn negotiation is handled by llm.el's native tool
;; support; this layer adds observability, tape logging, safety
;; guardrails, and per-tool lifecycle hooks.

;;; Code:

(require 'cl-lib)
(require 'superchat-event)
(require 'superchat-preset)
(require 'superchat-tool-execution)
(require 'superchat-runtime)
(require 'superchat-editor-workspace)

;; External function declarations
(declare-function org-escape-code-in-string "org" (s))
(declare-function superchat--response-region-close
                  "superchat-render" (content &optional final-p transcript-owner))
(declare-function superchat--response-region-insert-transcript
                  "superchat-render" (text owner &optional transcript-owner))
(declare-function superchat--response-region-update-transcript-line
                  "superchat-render" (marker replacement))
(declare-function superchat--response-region-track-pending-tool-call
                  "superchat-render"
                  (transcript-owner execution-id heading-marker end-marker tool-name call-time))
(declare-function superchat--response-region-untrack-pending-tool-call
                  "superchat-render" (transcript-owner execution-id))
(declare-function superchat--response-region-current-tail-marker
                  "superchat-render" (transcript-owner))
(declare-function superchat--response-region-insert-at-call-marker
                  "superchat-render" (transcript-owner marker text))
(declare-function superchat--response-region-pin-pending-end-markers
                  "superchat-render" (transcript-owner position))
(declare-function superchat--insert-system-message "superchat-render" (content))
(declare-function superchat--callback-in-view "superchat-llm" (buffer callback &optional dead-callback))
(defvar superchat--session-id)
(defvar superchat-mode)
(defvar superchat-buffer-name)
(defvar superchat--llm-tool-origin nil)

;; ═══════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-agent nil
  "Agent loop settings for Superchat."
  :group 'superchat)

(defcustom superchat-agent-max-tool-calls 120
  "Maximum number of tool calls a single agent turn may execute.
This is a safety guard against runaway agents.  120 since slice 11a
\(the user's ruling of 2026-10-20; it was 50)."
  :type 'integer
  :group 'superchat-agent)

(defun superchat--agent-effective-guardrails (&optional preset)
  "Return tighten-only guardrails for PRESET and the global policy.
An attempt to raise the tool budget is ignored with a warning.  Whether a
tool call is asked about is not a guardrail: the Session's permission level
decides that."
  (let ((profile-max (and preset (superchat-preset-max-tool-calls preset))))
    (when (and profile-max (> profile-max superchat-agent-max-tool-calls))
      (display-warning
       'superchat-agent
       (format "Preset %s cannot raise max_tool_calls above global limit %d"
               (superchat-preset-name preset)
               superchat-agent-max-tool-calls)
       :warning))
    (list :max-tool-calls
          (if profile-max
              (min superchat-agent-max-tool-calls profile-max)
            superchat-agent-max-tool-calls))))

;; ═══════════════════════════════════════════════════════════
;; State
;; ═══════════════════════════════════════════════════════════

(defvar-local superchat--agent-tool-call-count 0
  "Number of tool calls executed during the current agent run.")

(defun superchat--agent-apply-tool-origin (context)
  "Return CONTEXT extended by the current immutable llm tool origin."
  (if (null superchat--llm-tool-origin)
      context
    (unless (equal (plist-get context :evidence-contract)
                   (plist-get superchat--llm-tool-origin
                              :evidence-contract))
      (error "Tool evidence origin does not match its Runtime contract"))
    (let ((context (copy-sequence context)))
      (dolist (key '(:evidence-attempt :decision-snapshot-event-id
                     :tool-call-id :tool-call-name :tool-call-ordinal
                     :tool-call-arguments))
        (setq context
              (plist-put context key
                         (copy-tree
                          (plist-get superchat--llm-tool-origin key) t))))
      context)))

(defun superchat--agent-current-runtime-context (runtime-context)
  "Return RUNTIME-CONTEXT overlaid with the Run's latest envelope.

Recovery policy may be selected after the tool wrappers are created.  Read
that small mutable part from the Runtime entry at invocation time; keep all
other context fields captured at Run start."
  (let* ((context (copy-sequence runtime-context))
         (run-id (plist-get context :run-id))
         (entry (and run-id
                     (fboundp 'superchat-runtime--entry)
                     (superchat-runtime--entry run-id)))
         (envelope (and entry (plist-get entry :context-envelope))))
    (if (and envelope (listp envelope))
        (plist-put context :context-envelope (copy-tree envelope t))
      context)))

;; ═══════════════════════════════════════════════════════════
;; Per-tool lifecycle hooks
;; ═══════════════════════════════════════════════════════════

;; The per-tool lifecycle Events (spec §1.5): `tool-before', `tool-after',
;; `tool-failed' observe a call; `tools/pre-execute', `tools/execute' and
;; `tools/post-execute' are the waterfalls that decide, wrap and filter it
;; (see `superchat-tool-execution.el').  Register listeners with `superchat-on'.

(defun superchat--agent-run-tool-hooks (event &rest args)
  "Emit tool lifecycle EVENT with ARGS (listener errors are recorded, not raised)."
  (apply #'superchat-emit (superchat-execution-scope-resolve nil) event args))

;; ═══════════════════════════════════════════════════════════
;; Tool wrapping
;; ═══════════════════════════════════════════════════════════

(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function make-llm-tool "llm" (&rest args))

(defun superchat--agent-ensure-session-id ()
  "Return the already-bound `superchat--session-id', if any."
  (and (boundp 'superchat--session-id) superchat--session-id))

(defun superchat--agent-render-buffer (target-buffer &optional session-id)
  "Return exact TARGET-BUFFER when it is a chat View for SESSION-ID.
SESSION-ID is captured when wrapping, never derived from the current View.
A non-chat host buffer (inline, Org Babel, Magit) is never written to; its
tool calls are on the Tape."
  (and (bufferp target-buffer)
       (buffer-live-p target-buffer)
       (local-variable-p 'superchat-mode target-buffer)
       (buffer-local-value 'superchat-mode target-buffer)
       (or (null session-id)
           (not (local-variable-p 'superchat--session-id target-buffer))
           (equal session-id
                  (buffer-local-value 'superchat--session-id target-buffer)))
       target-buffer))

(defun superchat--agent-render-tool-call
    (tool-name args &optional target-buffer session-id transcript-owner
               execution-id)
  "Render a tool call in the chat buffer.
Closes any streamed text of the current round first: the model
usually narrates before it calls (\"Let me read that file first.\"),
and that text must be finalized *before* the tool record is appended
after it — otherwise the final rewrite, which deletes from the
assistant marker to the end of the buffer, takes the tool records
with it.

Renders one collapsible `*** ⏳ NAME' heading (S21); the running ->
done/failed update happens in place, see
`superchat--agent-render-tool-result'.

EXECUTION-ID is the call's real execution identity (S21-fix-1/fix-2): the
wrapper always passes the provider's own `:tool-call-id', or a freshly
minted id when there is none, so two overlapping same-name calls never
share it.  A direct caller that omits it also gets a freshly minted id
here, never TOOL-NAME (S21-fix-2, Spec 1): a name-keyed fallback let two
same-named calls collide, which is exactly the bug this exists to prevent,
so there is no caller-visible way to opt back into it.  The heading's
`superchat-tool-call-id' property is set to EXECUTION-ID and its pending
record -- the heading's own start marker AND the end-of-its-call-block
marker -- is tracked under it, in TRANSCRIPT-OWNER's own response-region
state (no global table).

The start marker cannot simply be captured before the insertion below and
trusted to stay put: `superchat--response-region-insert-transcript' inserts
via `insert-before-markers' whenever this owner's region is momentarily
inactive (the ordinary case between two tool calls with no narration in
between), which unconditionally pushes ANY marker already sitting at the
insertion point past the new text, regardless of that marker's own
insertion type.  Instead, the position just before the insertion is kept as
a plain integer (immune to marker relocation), the position just after is
read back via `superchat--response-region-current-tail-marker' (reliable:
it is read fresh, after the fact, from the owner's own current tail), and
the heading's actual start is located with a `text-property-any' search
bounded to exactly that narrow, this-call-only span and keyed on the
unique EXECUTION-ID -- never a whole-buffer scan, and never ambiguous with
another owner's identical heading text the way a backward text search from
`point-max' was (S21-fix-1's own defect)."
  (let ((buffer (superchat--agent-render-buffer target-buffer session-id)))
    (when buffer
      (with-current-buffer buffer
        (let* ((superchat-buffer-name (buffer-name buffer))
               (call-time (float-time))
               (execution-id (or execution-id (superchat-runtime--new-id "tool")))
               (heading (format "*** ⏳ %s" tool-name)))
          (when (fboundp 'superchat--response-region-close)
            (superchat--response-region-close nil nil transcript-owner))
          (let* ((before-position
                  (and (fboundp 'superchat--response-region-current-tail-marker)
                       (marker-position
                        (superchat--response-region-current-tail-marker transcript-owner))))
                 ;; S21-fix-2: an earlier, still-pending call's own
                 ;; end-of-call marker may coincide with BEFORE-POSITION
                 ;; (the ordinary case, no narration in between) -- pin it
                 ;; back afterward so it is not swept past THIS call's own
                 ;; new content by `insert-transcript's `insert-before-
                 ;; markers' insertion, which moves every marker at that
                 ;; position regardless of its own insertion type.
                 (boundary-markers
                  (and before-position
                       (fboundp 'superchat--response-region-pin-pending-end-markers)
                       (superchat--response-region-pin-pending-end-markers
                        transcript-owner before-position))))
            (superchat--response-region-insert-transcript
             (concat
              (propertize heading
			  'face 'font-lock-function-name-face
                          'superchat-tool-call-id execution-id)
              "\n#+begin_example\n"
              (format "Tool call: %s\n" tool-name)
              (org-escape-code-in-string (string-trim (format "%S" args)))
              "\n#+end_example\n")
             :current-run transcript-owner)
            (dolist (boundary boundary-markers) (set-marker boundary before-position))
            (when (and before-position
                       (fboundp 'superchat--response-region-current-tail-marker)
                       (fboundp 'superchat--response-region-track-pending-tool-call))
              (let* ((end-marker
                      (superchat--response-region-current-tail-marker transcript-owner))
                     (heading-position
                      (text-property-any before-position (marker-position end-marker)
                                         'superchat-tool-call-id execution-id)))
                (when heading-position
                  (superchat--response-region-track-pending-tool-call
                   transcript-owner execution-id (copy-marker heading-position nil)
                   end-marker tool-name call-time))))))))))

(defun superchat--agent-render-tool-result
    (tool-name result &optional target-buffer session-id transcript-owner
               execution-id)
  "Render a tool result in the chat buffer.

Updates the matching pending `*** ⏳ NAME' heading (S21) in place to
`*** ✓ NAME (D.Ds)' or `*** ✗ NAME (D.Ds)' -- a result starting with
\"[Tool call\" (the family of markers `superchat-tool-execution'/`superchat-
llm' use for a blocked/failed/denied/cancelled call) counts as failure --
and inserts the result block right after that same call's own args block,
before whatever comes next (S21-fix-2, Spec 1): out-of-order completion of
two overlapping calls must never land a result under the wrong heading, or
under the right heading but after a LATER call's own content.  Falls back
to inserting a fresh, unlinked heading (at the owner's current tail) when
no matching pending call was recorded (e.g. this function is ever called
without a prior `superchat--agent-render-tool-call' for the same execution
id).

EXECUTION-ID must match the id `superchat--agent-render-tool-call' used
for this same call; see that function's docstring for the fallback a
direct caller gets when it is omitted (S21-fix-2: a freshly minted id,
never TOOL-NAME).  The rewritten heading keeps the same
`superchat-tool-call-id' property value -- it is never replaced by a fresh
serial -- and the pending record for it, scoped to TRANSCRIPT-OWNER, is
removed either way."
  (let ((buffer (superchat--agent-render-buffer target-buffer session-id)))
    (when buffer
      (with-current-buffer buffer
        (let* ((superchat-buffer-name (buffer-name buffer))
               (execution-id (or execution-id (superchat-runtime--new-id "tool")))
               (text (string-trim (format "%s" result)))
               (pending
                (when (fboundp 'superchat--response-region-untrack-pending-tool-call)
                  (superchat--response-region-untrack-pending-tool-call
                   transcript-owner execution-id)))
               (heading-marker (nth 0 pending))
               (end-marker (nth 1 pending))
               (call-time (nth 3 pending))
               (failed-p (string-prefix-p "[Tool call" text))
               (duration (if (numberp call-time)
                             (max 0.0 (- (float-time) call-time))
                           0.0))
               (heading-text (format "*** %s %s (%.1fs)"
                                    (if failed-p "✗" "✓") tool-name duration))
               (heading (propertize heading-text
                                    'face (if failed-p
                                              'font-lock-warning-face
                                            'font-lock-function-name-face)
                                    'superchat-tool-call-id execution-id))
               (result-block
                (concat
                 "#+begin_example\n"
                 (format "Tool result: %s\n" tool-name)
                 (org-escape-code-in-string
                  (if (> (length text) 2000)
                      (concat (substring text 0 2000) "\n...")
                    text))
                 "\n#+end_example\n")))
          (if heading-marker
              (superchat--response-region-update-transcript-line heading-marker heading)
            (superchat--response-region-insert-transcript
             heading :current-run transcript-owner))
          (if (and end-marker (fboundp 'superchat--response-region-insert-at-call-marker))
              (superchat--response-region-insert-at-call-marker
               transcript-owner end-marker result-block)
            (superchat--response-region-insert-transcript
             result-block :current-run transcript-owner)))))))

(defun superchat--normalize-tool-result-text (value)
  "Normalize textual tool results for LLM transport.
The Authority seam already normalizes every executed result before it
forks to Tape/rendering/provider; this wrapper-level call remains as a
no-op safety net for results that pass through post-execution rewrite
hooks.  Non-string values pass through untouched."
  (superchat-tool-execution--normalize-tool-result value))

(defun superchat--agent-wrap-function
    (tool-name original-fn async &optional guardrails counter target-buffer
               runtime-context argument-names)
  "Wrap ORIGINAL-FN with accounting, rendering, and the shared executor.
ARGUMENT-NAMES, the tool's parameter names in order, lets the executor record
a call's arguments by name."
  (let* ((isolated-counter counter)
         (render-identity
          (condition-case nil
              (when runtime-context
                (let* ((session (plist-get runtime-context :session-id))
                       (run (plist-get runtime-context :run-id))
                       (status (and (stringp run) (not (string-empty-p run))
                                    (superchat-runtime-status run)))
                       (owner (plist-get status :transcript-owner)))
                  (when (and (stringp session) (not (string-empty-p session))
                             (equal session (plist-get status :session-id))
                             (equal run (plist-get status :run-id)) owner)
                    (cons session owner))))
            ((error quit) nil)))
         (render-session-id (car render-identity))
         (render-owner (cdr render-identity))
         (render-eligible (or (null runtime-context) render-identity))
         (render-warned nil)
         (render-allowed-p
          (lambda ()
            (unless (or render-eligible render-warned)
              ;; Commit before notifying: a warning handler may reenter this
              ;; wrapper.  Its failure must never interrupt execution/delivery.
              (setq render-warned t)
              (condition-case nil
                  (display-warning
                   'superchat-agent-loop
                   "Tool presentation withheld: unresolved Runtime context."
                   :warning)
                ((error quit) nil)))
            render-eligible))
         (render-call
          (lambda (args execution-id)
            (when (funcall render-allowed-p)
              (superchat--agent-render-tool-call
               tool-name args target-buffer render-session-id render-owner
               execution-id))))
         (render-result
          (lambda (result execution-id)
            (when (funcall render-allowed-p)
              (superchat--agent-render-tool-result
               tool-name result target-buffer render-session-id render-owner
               execution-id))))
         (counter (or counter (cons superchat--agent-tool-call-count nil)))
         (max-tool-calls (and guardrails
                              (plist-get guardrails :max-tool-calls))))
    (if async
        (lambda (callback &rest args)
          (let ((limit (or max-tool-calls superchat-agent-max-tool-calls)))
            (cl-incf (car counter))
            (unless isolated-counter
              (setq superchat--agent-tool-call-count (min (car counter) limit)))
            (let* ((context
                    (superchat--agent-apply-tool-origin
                     (superchat--agent-current-runtime-context
                      runtime-context)))
                 (context
                  (plist-put
                   context :session-id
                   (or (plist-get context :session-id)
                       (superchat--agent-ensure-session-id))))
                 (context
                  (plist-put context :actor-id
                             (or (plist-get context :actor-id) "lead")))
                 (context (plist-put context :guardrails guardrails))
                 (context (if argument-names
                              (plist-put context :argument-names argument-names)
                            context))
                 ;; S21-fix-1: the real provider tool-call id when this call
                 ;; came from a provider tool round, else a freshly minted id
                 ;; -- never the tool's name, so overlapping same-name calls
                 ;; never share an identity (Spec 1).
                 (execution-id
                  (or (plist-get context :tool-call-id)
                      (superchat-runtime--new-id "tool")))
                 (context
                  (if (plist-get context :evidence-contract)
                      (plist-put
                       context :evidence-action-recorded-function
                       (lambda ()
                         (funcall render-call args execution-id)))
                    (let ((session-id (plist-get context :session-id)))
                      (when (and (stringp session-id)
                                 (> (length session-id) 0))
                        (funcall render-call args execution-id)))
                    context))
                 (_counted
                  (superchat-runtime-note-tool-call
                   (plist-get context :run-id) (min (car counter) limit) limit))
                 (context
                  (if (> (car counter) limit)
                      (progn
                        (superchat-runtime-note-tool-cut-off
                         (plist-get context :run-id) limit limit)
                        (plist-put
                         context :block
                         (format "Agent stopped: exceeded maximum of %d tool calls"
                                 limit)))
                    context))
                 (delivered nil)
                 (deliver
                  (lambda (result render-p)
                    (unless delivered
                      (setq delivered t)
                      (when render-p
                        (condition-case render-error
                            (funcall render-result result execution-id)
                          (error
                           (ignore-errors
                             (display-warning
                              'superchat-agent-loop
                              (format "Tool result rendering failed: %s"
                                      (error-message-string render-error))
                              :warning)))))
                      (funcall callback
                               (superchat--normalize-tool-result-text result)))))
                 (live-done
                  (lambda (result) (funcall deliver result t)))
                 (dead-done
                  (lambda (result) (funcall deliver result nil)))
                 (done
                  (if (and target-buffer
                           (fboundp 'superchat--callback-in-view))
                      (superchat--callback-in-view
                       target-buffer live-done dead-done)
                    live-done)))
              (condition-case err
                  (superchat-tool-execute
                   tool-name original-fn args
                   :async t :callback done :context context)
                (error
                 (if delivered
                     (signal (car err) (cdr err))
                   (funcall done
                            (format "[Tool call failed: %s]"
                                    (error-message-string err)))))))))
      (lambda (&rest args)
        (let ((limit (or max-tool-calls superchat-agent-max-tool-calls)))
          (cl-incf (car counter))
          (unless isolated-counter
            (setq superchat--agent-tool-call-count (min (car counter) limit)))
          (let* ((context
                  (superchat--agent-apply-tool-origin
                   (superchat--agent-current-runtime-context
                    runtime-context)))
               (context
                (plist-put
                 context :session-id
                 (or (plist-get context :session-id)
                     (superchat--agent-ensure-session-id))))
               (context
                (plist-put context :actor-id
                           (or (plist-get context :actor-id) "lead")))
               (context (plist-put context :guardrails guardrails))
               (context (if argument-names
                            (plist-put context :argument-names argument-names)
                          context))
               ;; S21-fix-1: see the async branch's own comment above.
               (execution-id
                (or (plist-get context :tool-call-id)
                    (superchat-runtime--new-id "tool")))
               (context
                (if (plist-get context :evidence-contract)
                    (plist-put
                     context :evidence-action-recorded-function
                     (lambda ()
                       (funcall render-call args execution-id)))
                  (let ((session-id (plist-get context :session-id)))
                    (when (and (stringp session-id)
                               (> (length session-id) 0))
                      (funcall render-call args execution-id)))
                  context))
               (_counted
                (superchat-runtime-note-tool-call
                 (plist-get context :run-id) (min (car counter) limit) limit))
               (context
                (if (> (car counter) limit)
                    (progn
                      (superchat-runtime-note-tool-cut-off
                       (plist-get context :run-id) limit limit)
                      (plist-put
                       context :block
                       (format "Agent stopped: exceeded maximum of %d tool calls"
                               limit)))
                  context))
               (result
                (superchat-tool-execute
                 tool-name original-fn args :context context)))
            (funcall render-result result execution-id)
            (superchat--normalize-tool-result-text result)))))))

(defun superchat--agent-wrap-tool
  (tool &optional guardrails counter runtime-context)
  "Return a Runtime safety wrapper around an llm TOOL struct."
  (let* ((target-buffer (and superchat-buffer-name (current-buffer)))
         ;; Materialization happens while Runtime still carries the borrowed
         ;; caller Scope.  The provider invokes this wrapper later with no
         ;; dynamic stack, so retain this process-private reference.  The
         ;; exact plugin installation is intentionally *not* recovered here
         ;; by name: Authority reads it from the descriptor's own callable
         ;; proxy, selected at materialization, which survives replacement
         ;; without mixing OLD code with NEW resolver/preview state.
         (execution-scope (superchat-execution-scope-resolve nil))
         (wrapped (superchat--agent-wrap-function
                   (llm-tool-name tool)
                   (llm-tool-function tool)
                   (llm-tool-async tool)
                   guardrails counter target-buffer runtime-context
                   (mapcar (lambda (arg) (plist-get arg :name))
                           (llm-tool-args tool))))
         (function
          (if (llm-tool-async tool)
              (lambda (callback &rest args)
                (let ((superchat-execution-scope execution-scope))
                  (apply wrapped callback args)))
            (lambda (&rest args)
              (let ((superchat-execution-scope execution-scope))
                (apply wrapped args)))))
         (function (if (and target-buffer
                            (fboundp 'superchat--callback-in-view))
                       (superchat--callback-in-view target-buffer function function)
                     function)))
    (make-llm-tool
     :function function
     :name (llm-tool-name tool)
     :description (llm-tool-description tool)
     :args (llm-tool-args tool)
     :async (llm-tool-async tool))))

(defun superchat--agent-make-wrapper (&optional preset)
  "Return a closure that wraps one tool for this request.

All tools of a request share one guardrail set and one call counter, and
a tool the agent synthesizes mid-run has to join the same accounting —
so the wrapper is handed out as a closure rather than rebuilt per tool."
  (let ((execution-scope (superchat-execution-scope-resolve nil))
        (guardrails (superchat--agent-effective-guardrails preset))
        (counter (cons 0 nil))
        (runtime-context
         (copy-sequence
          (bound-and-true-p superchat--tool-execution-context))))
    (lambda (tool)
      ;; A synthesized descriptor can be wrapped on a later provider round,
      ;; after the original Runtime dynamic binding has unwound.  Keep the
      ;; request's borrowed Scope with this factory rather than resolving the
      ;; callback's ambient (usually global) Scope.
      (let ((superchat-execution-scope execution-scope))
        (superchat--agent-wrap-tool
         tool guardrails counter runtime-context)))))

(defun superchat--agent-wrap-tools (tools &optional preset)
  "Wrap all TOOLS for Runtime observability and guardrails."
  (mapcar (superchat--agent-make-wrapper preset) tools))


(provide 'superchat-agent-loop)

;;; superchat-agent-loop.el ends here
