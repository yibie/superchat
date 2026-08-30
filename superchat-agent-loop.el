;;; superchat-agent-loop.el --- Agent execution loop for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agent mode coordinates multi-turn tool use for `type: agent' presets.
;; The actual multi-turn negotiation is handled by llm.el's native tool
;; support; this layer adds observability, tape logging, safety
;; guardrails, and per-tool lifecycle hooks.

;;; Code:

(require 'cl-lib)
(require 'superchat-preset)
(require 'superchat-authority)
(require 'superchat-runtime)
(require 'superchat-editor-workspace)

;; External function declarations
(declare-function superchat--response-region-close
                  "superchat-render" (content &optional final-p transcript-owner))
(declare-function superchat--response-region-insert-transcript
                  "superchat-render" (text owner &optional transcript-owner))
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

(defcustom superchat-agent-max-tool-calls 50
  "Maximum number of tool calls a single agent turn may execute.
This is a safety guard against runaway agents."
  :type 'integer
  :group 'superchat-agent)

(defcustom superchat-agent-confirm-destructive t
  "If non-nil, ask for confirmation before destructive tool calls.
When nil, the agent may execute write/shell/edit tools without
prompting.  Read-only tools are never confirmed."
  :type 'boolean
  :group 'superchat-agent)

(defcustom superchat-agent-destructive-tools
  '("edit_buffer" "replace_buffer" "shell-command" "eshell-command" "bash"
    "eval_elisp")
  "Tool names considered destructive for confirmation purposes."
  :type '(repeat string)
  :group 'superchat-agent)

(defun superchat--agent-effective-guardrails (&optional preset)
  "Return tighten-only guardrails for PRESET and the global policy.
Attempts to raise the tool budget or disable required confirmation are
ignored with a warning."
  (let ((profile-max (and preset (superchat-preset-max-tool-calls preset)))
        (profile-confirm (and preset
                              (superchat-preset-confirm-destructive preset))))
    (when (and profile-max (> profile-max superchat-agent-max-tool-calls))
      (display-warning
       'superchat-agent
       (format "Preset %s cannot raise max_tool_calls above global limit %d"
               (superchat-preset-name preset)
               superchat-agent-max-tool-calls)
       :warning))
    (when (and preset superchat-agent-confirm-destructive
               (null profile-confirm))
      (display-warning
       'superchat-agent
       (format "Preset %s cannot disable global destructive-tool confirmation"
               (superchat-preset-name preset))
       :warning))
    (list :max-tool-calls
          (if profile-max
              (min superchat-agent-max-tool-calls profile-max)
            superchat-agent-max-tool-calls)
          :confirm-destructive
          (or superchat-agent-confirm-destructive
              (eq profile-confirm t)))))

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

(defvar superchat-agent-pre-tool-functions nil
  "Hook run before every tool call in agent mode.
Each function receives (TOOL-NAME ARGS).  Return value ignored.")

(defvar superchat-agent-permission-functions nil
  "Hook run before every tool call to gate execution.
Each function receives (TOOL-NAME ARGS) and should return:
  \\='allow  — skip user confirmation, allow the tool
  \\='deny   — deny the tool call (returns error string)
  nil     — defer to normal confirmation logic.
All hooks run; \\='deny wins over \\='allow.")

(defvar superchat-agent-post-tool-functions nil
  "Hook run after a successful tool call in agent mode.
Each function receives (TOOL-NAME ARGS RESULT).  Return value ignored.")

(defvar superchat-agent-post-tool-failure-functions nil
  "Hook run after a failed tool call in agent mode.
Each function receives (TOOL-NAME ARGS ERROR).  Return value ignored.")

(defun superchat--agent-run-tool-hooks (hooks &rest args)
  "Run HOOKS with ARGS, ignoring errors in individual hooks."
  (dolist (fn hooks)
    (when (functionp fn)
      (ignore-errors (apply fn args)))))

(defun superchat--agent-check-permission (tool-name args)
  "Check all permission hooks for TOOL-NAME with ARGS.
Returns \\='allow, \\='deny, or nil (defer to normal confirmation)."
  (let ((results (delq nil
                  (mapcar (lambda (fn)
                            (ignore-errors
                              (funcall fn tool-name args)))
                          superchat-agent-permission-functions))))
    (cond ((memq 'deny results) 'deny)
          ((memq 'allow results) 'allow)
          (t nil))))

;; ═══════════════════════════════════════════════════════════
;; Call interception — user-programmable fences (pi-style)
;; ═══════════════════════════════════════════════════════════

(defvar superchat-agent-tool-rewrite-functions nil
  "Abnormal-hook run before every agent tool call; may rewrite or block it.

Each function is called with (TOOL-NAME ARGS) — ARGS is the argument
list as the tool function will receive it — and returns one of:
  nil                     pass through unchanged
  (:args NEW-ARGS)        replace the argument list (later hooks see it)
  (:block REASON-STRING)  refuse the call; REASON reaches the model as
                          the tool result, so make it actionable

This is the ten-lines-of-elisp fence surface: path protection, command
gates, argument sanitising all live here, e.g.

  (add-hook \\='superchat-agent-tool-rewrite-functions
            (lambda (name args)
              (when (and (equal name \"edit_buffer\")
                         (string-match-p \"\\\\.env\\\\'\" (car args)))
                \\='(:block \"Editing .env buffers is not allowed.\"))))

Unlike `superchat-agent-permission-functions' (allow/deny/defer, fixed
message), these hooks compose, carry a custom reason, and can rewrite.")

(defvar superchat-agent-tool-result-filter-functions nil
  "Abnormal-hook run on every tool result before the model sees it.

Each function is called with (TOOL-NAME ARGS RESULT) and returns either
a replacement string or nil to leave RESULT unchanged.  Filters chain in
order; the filtered result is also what gets rendered and taped — the
transcript shows what the model saw.  Use for redaction and truncation.")

(defun superchat--agent-intercept-tool-call (tool-name args)
  "Run rewrite hooks over TOOL-NAME / ARGS.
Returns (:block REASON) from the first blocking hook, else (:args ARGS)
with every rewrite applied in hook order."
  (catch 'superchat--blocked
    (dolist (fn superchat-agent-tool-rewrite-functions)
      (let ((decision (ignore-errors (funcall fn tool-name args))))
        (cond
         ((and (consp decision) (plist-get decision :block))
          (throw 'superchat--blocked
                 (list :block (plist-get decision :block))))
         ((and (consp decision) (plist-member decision :args))
          (setq args (plist-get decision :args))))))
    (list :args args)))

(defun superchat--agent-filter-tool-result (tool-name args result)
  "Run result filters over RESULT for TOOL-NAME called with ARGS."
  (dolist (fn superchat-agent-tool-result-filter-functions)
    (let ((replacement (ignore-errors (funcall fn tool-name args result))))
      (when (stringp replacement)
        (setq result replacement))))
  result)

;; ═══════════════════════════════════════════════════════════
;; Tool wrapping
;; ═══════════════════════════════════════════════════════════

(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function make-llm-tool "llm" (&rest args))

(declare-function superchat--syn-tool-p "superchat-synthesis" (name))
(defvar superchat-agent-confirm-synthesized)
(defvar superchat--syn-call-confirmed)

(defun superchat--agent-destructive-p (tool-name _args)
  "Return non-nil if TOOL-NAME is considered destructive.

A tool the agent wrote for itself is always destructive: this predicate
is a name lookup, and a synthesized tool arrives with a brand-new name,
so without this clause it would fall straight through the confirmation
gate — even with a body of (delete-file \"~/\")."
  (or (member tool-name superchat-agent-destructive-tools)
      (and (fboundp 'superchat--syn-tool-p)
           (superchat--syn-tool-p tool-name))))

(defun superchat--agent-confirm-p (tool-name args &optional guardrails)
  "Return non-nil if the tool call should be confirmed.

Synthesized tools answer to `superchat-agent-confirm-synthesized' alone.
Turning off `superchat-agent-confirm-destructive' says \"I trust the
tools Superchat ships\"; it must not silently extend to code an LLM wrote
seconds ago.  Everything else respects the global switch (or the
preset's tightened value)."
  (if (and (fboundp 'superchat--syn-tool-p)
           (superchat--syn-tool-p tool-name))
      (bound-and-true-p superchat-agent-confirm-synthesized)
    (and (if guardrails
             (plist-get guardrails :confirm-destructive)
           superchat-agent-confirm-destructive)
         (superchat--agent-destructive-p tool-name args))))

(defun superchat--agent-ensure-session-id ()
  "Return the already-bound `superchat--session-id', if any."
  (and (boundp 'superchat--session-id) superchat--session-id))

(defun superchat--agent-render-buffer (target-buffer &optional session-id)
  "Return exact TARGET-BUFFER when its View still corresponds to SESSION-ID.
SESSION-ID is captured when wrapping, never derived from the current View.
Nil context and non-chat borrowed buffers keep their legacy presentation."
  (and (bufferp target-buffer)
       (buffer-live-p target-buffer)
       (or (null session-id)
           (not (local-variable-p 'superchat-mode target-buffer))
           (not (buffer-local-value 'superchat-mode target-buffer))
           (not (local-variable-p 'superchat--session-id target-buffer))
           (let ((current (buffer-local-value 'superchat--session-id target-buffer)))
             (or (null current) (equal session-id current))))
       target-buffer))

(defun superchat--agent-render-tool-call
    (tool-name args &optional target-buffer session-id transcript-owner)
  "Render a tool call in the chat buffer.
Closes any streamed text of the current round first: the model
usually narrates before it calls (\"Let me read that file first.\"),
and that text must be finalized *before* the tool record is appended
after it — otherwise the final rewrite, which deletes from the
assistant marker to the end of the buffer, takes the tool records
with it."
  (let ((buffer (superchat--agent-render-buffer target-buffer session-id)))
    (when buffer
      (with-current-buffer buffer
        (let ((superchat-buffer-name (buffer-name buffer)))
          (when (fboundp 'superchat--response-region-close)
            (superchat--response-region-close nil nil transcript-owner))
          (superchat--response-region-insert-transcript
           (concat
            (propertize (format "** Tool call: %s" tool-name)
			'face 'font-lock-function-name-face)
            "\n#+begin_example\n"
            (string-trim (format "%S" args))
            "\n#+end_example\n")
           :current-run transcript-owner))))))

(defun superchat--agent-render-tool-result
    (tool-name result &optional target-buffer session-id transcript-owner)
  "Render a tool result in the chat buffer."
  (let ((buffer (superchat--agent-render-buffer target-buffer session-id)))
    (when buffer
      (with-current-buffer buffer
        (let ((superchat-buffer-name (buffer-name buffer)))
          (let ((text (string-trim (format "%s" result))))
            (superchat--response-region-insert-transcript
             (concat
              (propertize (format "** Tool result: %s" tool-name)
                          'face 'font-lock-string-face)
              "\n#+begin_example\n"
              (if (> (length text) 2000)
                  (concat (substring text 0 2000) "\n...")
		text)
              "\n#+end_example\n")
             :current-run transcript-owner)))))))

(defun superchat--normalize-tool-result-text (value)
  "Normalize textual tool results for LLM transport.
The Authority seam already normalizes every executed result before it
forks to Tape/rendering/provider; this wrapper-level call remains as a
no-op safety net for results that pass through post-execution rewrite
hooks.  Non-string values pass through untouched."
  (superchat-authority--normalize-tool-result value))

(defun superchat--agent-wrap-function
    (tool-name original-fn async &optional guardrails counter target-buffer
               runtime-context)
  "Wrap ORIGINAL-FN with accounting, rendering, and the shared executor."
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
          (lambda (args)
            (when (funcall render-allowed-p)
              (superchat--agent-render-tool-call
               tool-name args target-buffer render-session-id render-owner))))
         (render-result
          (lambda (result)
            (when (funcall render-allowed-p)
              (superchat--agent-render-tool-result
               tool-name result target-buffer render-session-id render-owner))))
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
                  (plist-put context :attendance
                             (or (plist-get context :attendance)
                                 'attended)))
                 (context
                  (plist-put context :actor-id
                             (or (plist-get context :actor-id) "lead")))
                 (context (plist-put context :guardrails guardrails))
                 (context
                  (if (plist-get context :evidence-contract)
                      (plist-put
                       context :evidence-action-recorded-function
                       (lambda ()
                         (funcall render-call args)))
                    (let ((session-id (plist-get context :session-id)))
                      (when (and (stringp session-id)
                                 (> (length session-id) 0))
                        (funcall render-call args)))
                    context))
                 (context
                  (if (> (car counter) limit)
                      (plist-put
                       context :block
                       (format "Agent stopped: exceeded maximum of %d tool calls"
                               limit))
                    context))
                 (delivered nil)
                 (deliver
                  (lambda (result render-p)
                    (unless delivered
                      (setq delivered t)
                      (when render-p
                        (condition-case render-error
                            (funcall render-result result)
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
                (plist-put context :attendance
                           (or (plist-get context :attendance)
                               'attended)))
               (context
                (plist-put context :actor-id
                           (or (plist-get context :actor-id) "lead")))
               (context (plist-put context :guardrails guardrails))
               (context
                (if (plist-get context :evidence-contract)
                    (plist-put
                     context :evidence-action-recorded-function
                     (lambda ()
                       (funcall render-call args)))
                  (let ((session-id (plist-get context :session-id)))
                    (when (and (stringp session-id)
                               (> (length session-id) 0))
                      (funcall render-call args)))
                  context))
               (context
                (if (> (car counter) limit)
                    (plist-put
                     context :block
                     (format "Agent stopped: exceeded maximum of %d tool calls"
                             limit))
                  context))
               (result
                (superchat-tool-execute
                 tool-name original-fn args :context context)))
            (funcall render-result result)
            (superchat--normalize-tool-result-text result)))))))

(defun superchat--agent-wrap-tool
  (tool &optional guardrails counter runtime-context)
  "Return a Runtime safety wrapper around an llm TOOL struct."
  (let* ((target-buffer (and superchat-buffer-name (current-buffer)))
         (function (superchat--agent-wrap-function
                    (llm-tool-name tool)
                    (llm-tool-function tool)
                    (llm-tool-async tool)
                    guardrails counter target-buffer runtime-context))
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
  (let ((guardrails (superchat--agent-effective-guardrails preset))
        (counter (cons 0 nil))
        (runtime-context
         (copy-sequence
          (bound-and-true-p superchat--tool-execution-context))))
    (lambda (tool)
      (superchat--agent-wrap-tool
       tool guardrails counter runtime-context))))

(defun superchat--agent-wrap-tools (tools &optional preset)
  "Wrap all TOOLS for Runtime observability and guardrails."
  (mapcar (superchat--agent-make-wrapper preset) tools))

;; ═══════════════════════════════════════════════════════════
;; Entry point
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--request-plan-from-turn
                  "superchat-dispatcher"
                  (turn &optional template target-model finalized))

(defun superchat--agent-run (turn)
  "Submit compatibility TURN through the shared Runtime."
  (let* ((request (superchat--request-plan-from-turn turn))
         (session-id (plist-get request :session-id))
         (buffer (plist-get request :buffer)))
    (plist-put
     request :buffer
     (superchat-editor-workspace-validate-buffer buffer session-id))
    (superchat-runtime-submit request)))

(provide 'superchat-agent-loop)

;;; superchat-agent-loop.el ends here
