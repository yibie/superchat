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

;; External function declarations
(declare-function superchat--prepare-assistant-response-area "superchat-render" ())
(declare-function superchat--insert-system-message "superchat-render" (content))
(declare-function superchat--callback-in-view "superchat-llm" (buffer-name callback))
(defvar superchat--session-id)
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
  '("write-file" "append-file" "EditBuffer" "shell-command" "eshell-command" "bash"
    "eval-elisp")
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
              (when (and (equal name \"write-file\")
                         (string-match-p \"\\\\.env\\\\'\" (car args)))
                \\='(:block \"Writing .env files is not allowed.\"))))

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
  "Return `superchat--session-id', creating one if necessary."
  (or (and (boundp 'superchat--session-id) superchat--session-id)
      (setq superchat--session-id
            (format "%s%04x"
                    (format-time-string "%Y%m%d-%H%M%S-")
                    (random 65536)))))

(declare-function superchat--render-close-tool-round "superchat-render" ())

(defun superchat--agent-render-tool-call (tool-name args)
  "Render a tool call in the chat buffer.
Closes any streamed text of the current round first: the model
usually narrates before it calls (\"Let me read that file first.\"),
and that text must be finalized *before* the tool record is appended
after it — otherwise the final rewrite, which deletes from the
assistant marker to the end of the buffer, takes the tool records
with it."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (when (fboundp 'superchat--render-close-tool-round)
      (superchat--render-close-tool-round))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (format "** Tool call: %s" tool-name)
                            'face 'font-lock-function-name-face))
        (insert "\n#+begin_example\n")
        (insert (string-trim (format "%S" args)))
        (insert "\n#+end_example\n")))))

(defun superchat--agent-render-tool-result (tool-name result)
  "Render a tool result in the chat buffer."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (format "** Tool result: %s" tool-name)
                            'face 'font-lock-string-face))
        (insert "\n#+begin_example\n")
        (let ((text (string-trim (format "%s" result))))
          (insert (if (> (length text) 2000)
                      (concat (substring text 0 2000) "\n...")
                    text)))
        (insert "\n#+end_example\n")))))

(defun superchat--agent-wrap-function
    (tool-name original-fn async &optional guardrails counter target-buffer-name
               runtime-context)
  "Wrap ORIGINAL-FN with accounting, rendering, and the shared executor."
  (let* ((isolated-counter counter)
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
                     (copy-sequence runtime-context)))
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
                         (superchat--agent-render-tool-call tool-name args)))
                    (superchat--agent-render-tool-call tool-name args)
                    context))
                 (context
                  (if (> (car counter) limit)
                      (plist-put
                       context :block
                       (format "Agent stopped: exceeded maximum of %d tool calls"
                               limit))
                    context))
                 (done
                  (lambda (result)
                    (superchat--agent-render-tool-result tool-name result)
                    (funcall callback result)))
                 (done
                  (if (and target-buffer-name
                           (fboundp 'superchat--callback-in-view))
                      (superchat--callback-in-view target-buffer-name done)
                    done)))
              (superchat-tool-execute
               tool-name original-fn args
               :async t :callback done :context context))))
      (lambda (&rest args)
        (let ((limit (or max-tool-calls superchat-agent-max-tool-calls)))
          (cl-incf (car counter))
          (unless isolated-counter
            (setq superchat--agent-tool-call-count (min (car counter) limit)))
          (let* ((context
                  (superchat--agent-apply-tool-origin
                   (copy-sequence runtime-context)))
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
                       (superchat--agent-render-tool-call tool-name args)))
                  (superchat--agent-render-tool-call tool-name args)
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
            (superchat--agent-render-tool-result tool-name result)
            result))))))

(defun superchat--agent-wrap-tool
    (tool &optional guardrails counter runtime-context)
  "Return a Runtime safety wrapper around an llm TOOL struct."
  (let* ((target-buffer-name (bound-and-true-p superchat-buffer-name))
         (function (superchat--agent-wrap-function
                    (llm-tool-name tool)
                    (llm-tool-function tool)
                    (llm-tool-async tool)
                    guardrails counter target-buffer-name runtime-context))
         (function (if (and target-buffer-name
                            (fboundp 'superchat--callback-in-view))
                       (superchat--callback-in-view target-buffer-name function)
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
  (superchat-runtime-submit
   (superchat--request-plan-from-turn turn)))

(provide 'superchat-agent-loop)

;;; superchat-agent-loop.el ends here
