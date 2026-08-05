;;; superchat-llm.el --- LLM backend abstraction for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM provider abstraction, tool collection, prompt building, and
;; streaming answer generation.  Extracted from superchat.el monolith
;; (v0.9 split step 5).

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
;; The model port stays UI-free: streaming sinks arrive through the
;; Runtime request, and MCP tools are picked up behind `fboundp' when the
;; optional bridge is loaded.
(require 'superchat-preset)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-streaming)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-tools-enabled)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-unknown-tool-retries)
(defvar superchat-llm-round-limit-action)
(defvar superchat-response-timeout)
(defvar superchat-tool-timeout-multiplier)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat--ttft-start-time)
(defvar superchat-buffer-name)
(defvar superchat--active-timeout-timer)
(defvar superchat--conversation-history)
;; Per-run transport bindings owned by superchat-runtime.el.
(defvar superchat--llm-runtime-observer)
(defvar superchat--llm-tools-prepared-p)
(defvar superchat--llm-prepared-tools)
(defvar superchat--llm-tool-wrapper)
(defvar superchat--llm-round-limit-function)
(defvar superchat--llm-unknown-tool-retry-limit)
(defvar superchat--llm-persist-synthesized-tools)
(defvar superchat--llm-run-active-p)
(declare-function superchat--ttft-log "superchat" (label))
(declare-function superchat--annotate-ttft "superchat-render" (elapsed))
(declare-function superchat--is-ollama-backend-p "superchat-models" (&optional backend))
(declare-function superchat--detect-response-mode "superchat" (&optional tools tools-known-p))
(declare-function superchat--show-response-mode-indicator "superchat" (&optional mode))
(declare-function superchat--get-adjusted-timeout "superchat" (&optional mode))
(declare-function superchat-get-llm-tools "superchat-tools" (&optional tool-names))
(declare-function superchat-mcp-get-tools "superchat-mcp" ())
(declare-function llm-capabilities "llm" (provider))
(declare-function llm-chat-async "llm"
                  (provider prompt response-callback error-callback
                            &optional multi-output))
(declare-function llm-chat-streaming "llm"
                  (provider prompt partial-callback response-callback
                            error-callback &optional multi-output))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-chat-prompt-tools "llm" (prompt))
(declare-function llm-chat-prompt-p "llm" (prompt))
(declare-function llm-chat-prompt-interactions "llm" (prompt))
(declare-function llm-chat-prompt-interaction-role "llm" (interaction))
(declare-function llm-chat-prompt-interaction-content "llm" (interaction))
(declare-function llm-chat-prompt-append-response "llm"
                  (prompt response &optional role))
(declare-function make-llm-media "llm" (&rest args))
(declare-function llm-make-multipart "llm" (&rest parts))

(cl-defgeneric superchat--provider-name (provider)
  "Return a human-readable lowercase name for PROVIDER.")

(cl-defgeneric superchat--provider-chat-model (provider)
  "Return the chat-model of PROVIDER, or nil if not extractable.")

(cl-defgeneric superchat--provider-with-chat-model (provider new-model)
  "Return a copy of PROVIDER with chat-model set to NEW-MODEL.
Default: return PROVIDER unchanged (no override available).")

(defun superchat--copy-provider-with-chat-model
    (provider copy-function struct-type new-model)
  "Copy PROVIDER with COPY-FUNCTION and set its NEW-MODEL slot.
STRUCT-TYPE names the provider struct that owns the `chat-model' slot."
  (let ((copy (funcall copy-function provider)))
    (setf (cl-struct-slot-value struct-type 'chat-model copy) new-model)
    copy))

(defun superchat--effective-llm-backend (&optional target-model)
  "Return the effective llm backend, applying model override.
TARGET-MODEL is a one-shot model override; falls back to
`superchat-llm-model'.  When neither resolves, the raw backend is
returned.  If model override is unavailable for the provider type
\(only OpenAI, Claude, Ollama have copy-* methods installed), the
backend is returned unchanged and the override is silently dropped."
  (let* ((model (or target-model superchat-llm-model))
         (backend superchat-llm-backend))
    (cond
     ((null backend) nil)
     ((null model) backend)
     (t
      (condition-case nil
          (superchat--provider-with-chat-model backend model)
        (error backend))))))

(defun superchat--should-attach-tools-p (input)
  "Return non-nil when tools should be attached for INPUT.
Honors `superchat-llm-tools-enabled':
  nil          -> never
  `always'/t   -> always
  `on-demand'  -> only when INPUT shows a tool intent:
                  contains a `#'-style file reference, explicitly names
                  `render_ui', `eshell-command', or a team tool, or a slash
                  command is currently active."
  (cond
   ((null superchat-llm-tools-enabled) nil)
   ((memq superchat-llm-tools-enabled '(always t)) t)
   (t  ;; on-demand and anything else conservatively maps here
    (or (and (boundp 'superchat--current-command)
             superchat--current-command)
        (and (stringp input)
             (or (string-match-p superchat--file-ref-regexp input)
                 (superchat--render-ui-intent-p input)
                 (superchat--eshell-command-intent-p input)
                 (superchat--team-tool-intent-p input)))))))

(defun superchat--team-tool-intent-p (input)
  "Return non-nil when INPUT explicitly names a team tool."
  (and (stringp input)
       (string-match-p
        "\\_<\\(?:team_task\\|team_message\\|delegate_to_subagent\\(?:_parallel\\)?\\)\\_>"
        input)))

(defun superchat--eshell-command-intent-p (input)
  "Return non-nil when INPUT explicitly asks for the Eshell bridge."
  (and (stringp input)
       (or (string-match-p "\\_<eshell-command\\_>" input)
           (string-match-p "\\_<eshell command\\_>" input))))

(defun superchat--render-ui-intent-p (input)
  "Return non-nil when INPUT explicitly names the built-in `render_ui'."
  (and (stringp input)
       (string-match-p "\\_<render_ui\\_>" input)))

(defun superchat--collect-llm-tools (&optional input tool-names)
  "Collect built-in + MCP tools for the current request.
Returns nil when `superchat-llm-tools-enabled' (combined with INPUT
for the `on-demand' policy) forbids attaching tools — that lets the
caller bypass llm.el's multi-output / tool-calling mode entirely,
which is the main contributor to time-to-first-token.

When TOOL-NAMES is non-nil, only tools whose names are members of
that list are returned.  This allows presets to restrict the active
tool set without changing global allowlists.  The symbol `none'
means an explicitly empty tool set (preset declared `tools: []'):
no tools are attached at all."
  (unless (eq tool-names 'none)
    (when (or tool-names (superchat--should-attach-tools-p input))
      (let* ((render-ui-intent (superchat--render-ui-intent-p input))
             (llm-tools (when (fboundp 'superchat-get-llm-tools)
                          (superchat-get-llm-tools tool-names)))
             (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                          (superchat-mcp-get-tools)))
             (all (append llm-tools mcp-tools))
             ;; A direct render request has a single, audited UI path.
             ;; Do not let an older persisted synthesized tool, or the
             ;; self-extension primitive itself, compete with `render_ui'.
             (all (if render-ui-intent
                      (cl-remove-if
                       (lambda (tool)
                         (equal (llm-tool-name tool) "define_tool"))
                       all)
                    all)))
          (append
           (if tool-names
               (cl-remove-if-not (lambda (tool)
                                   (member (llm-tool-name tool) tool-names))
                                 all)
             all)
           ;; An explicit profile is an exact request-local surface.
           ;; Synthesized tools remain available only to unprofiled chat.
           (unless (or render-ui-intent tool-names)
             (bound-and-true-p superchat-llm-synthesized-tools)))))))

(defun superchat--llm-extract-text (result)
  "Extract the :text field from an llm.el multi-output RESULT.
For plain string results, returns the string unchanged."
  (cond
   ((null result) "")
   ((stringp result) result)
   ((and (consp result) (stringp (plist-get result :text)))
    (plist-get result :text))
   (t (format "%S" result))))

(defun superchat--llm-tool-round-p (result)
  "Return non-nil when RESULT asks the caller to continue after tools.
llm.el executes requested tools and appends their results to the mutable
chat prompt, but leaves sending the next model request to its caller.

The only signal that matters is whether this round called tools:
llm.el attaches `:tool-results' precisely when it executed tool uses
\(see `llm-provider-utils-execute-tool-uses'), and a bare alist of
tool-result conses is the non-multi-output form of the same thing.

Accompanying `:text' must NOT end the loop.  Models routinely narrate
the call they are about to make (\"Let me read that file first.\") in
the same round as the tool use; treating that narration as the final
answer strands the tool result unseen and hands the user the narration
instead of an answer.  The final answer is the round that returns text
and no tool calls."
  (and (consp result)
       (or (not (keywordp (car result)))
           (plist-get result :tool-results))
       t))

(defun superchat--llm-tool-result-entries (result)
  "Return RESULT's tool-result alist, or nil for ordinary text.

llm.el returns a plist with a `:tool-results' alist in multi-output mode
and the same alist directly in non-multi-output mode.  Keeping this shape
normalization in one place makes error classification consistent across the
sync, async, and sub-agent loops."
  (let ((tool-results
         (cond
          ((and (consp result) (plist-get result :tool-results))
           (plist-get result :tool-results))
          ((and (consp result) (not (keywordp (car result)))) result))))
    (and (listp tool-results) tool-results)))

(defun superchat--llm-tool-error-entries (result)
  "Return failed tool-result entries found in RESULT.

Tool functions deliberately return failures as readable strings so llm.el
can append them as tool results.  Only known failure markers are classified;
ordinary tool output such as a file whose first line is arbitrary text is
left alone."
  (cl-remove-if-not
   (lambda (entry)
     (and (consp entry)
          (stringp (cdr entry))
          (let ((message (string-trim (cdr entry))))
            (or (string-prefix-p "Error:" message)
                (string-prefix-p "genui:" message)
                (string-prefix-p "[tool-error" message)
                (string-prefix-p "[Tool error" message)
                (string-prefix-p "[Error:" message)
                (string-prefix-p "[ERROR:" message)
                (string-prefix-p "[Tool call failed" message)
                (string-prefix-p "[Tool call blocked" message)
                (string-prefix-p "[Tool call denied" message)
                (string-prefix-p "[Tool call cancelled" message)
                (string-prefix-p "[Tool call canceled" message)
                (string-prefix-p "[Tool definition declined" message)
                (string-prefix-p "❌" message)
                (string-prefix-p "[Synthesized tool" message)
                (string-prefix-p "[Call to synthesized tool" message)
                (string-prefix-p "[Delegation denied" message)))))
   (superchat--llm-tool-result-entries result)))

(defun superchat--llm-tool-error-non-retryable-p (result)
  "Return non-nil when RESULT contains a user/policy-blocked tool call."
  (cl-some
   (lambda (entry)
     (string-match-p
      "\\b\\(?:refused\\|declined\\|cancelled\\|canceled\\|denied\\|blocked\\)\\b"
      (cdr entry)))
   (superchat--llm-tool-error-entries result)))

(defun superchat--llm-tool-error-feedback (result)
  "Return corrective feedback when RESULT contains failed tool calls.

Add a short user-role instruction after tool results: providers otherwise
tend to narrate a fallback instead of repairing and retrying the requested
tool.  Return nil for successful tool rounds or ordinary text responses.
Callers can use `superchat--llm-tool-error-non-retryable-p' to stop rather
than retry a user or policy refusal."
  (let ((errors (superchat--llm-tool-error-entries result)))
    (when errors
      (concat
       "Tool execution failed. Do not claim success or silently substitute a fallback. "
       (if (superchat--llm-tool-error-non-retryable-p result)
           "The user or a policy prevented this call; do not retry it. Explain the limitation or continue without that tool."
         "Correct the arguments and retry the requested tool.")
       "\n"
       (mapconcat
        (lambda (entry)
          (format "- %s: %s" (car entry) (cdr entry)))
        errors
        "\n")))))

(defun superchat--llm-tool-protocol-error-p (type)
  "Return non-nil when TYPE is an llm.el tool-call validation error.

These errors are reported through the request error callback rather than
as a normal `:tool-results' response.  They are still model-correctable:
the preceding assistant tool-use must be removed before the prompt can be
sent again."
  (and (symbolp type)
       (string-prefix-p "llm-tool-" (symbol-name type))))

(defun superchat--llm-tool-protocol-error-feedback (type message)
  "Build corrective user feedback for an llm.el tool error TYPE/MESSAGE."
  (let ((detail (cond
                 ((stringp message) message)
                 ((and (consp message) (stringp (car message)))
                  (car message))
                 (t (format "%s" message)))))
    (format
     "%s\nOriginal llm.el error (%s): %s\nDo not claim success unless the corrected tool call succeeds."
     (pcase type
       ('llm-tool-unknown-tool
        "The requested tool name is not available. Call only an exact name from the tool list, or answer directly without tools.")
       ('llm-tool-missing-argument
        "The tool call is missing a required argument. Supply the required arguments and retry the tool.")
       ('llm-tool-unknown-argument
        "The tool call contains an unsupported argument. Remove it, use the declared argument names, and retry the tool.")
       ('llm-tool-call-error
        "The tool arguments were not complete valid JSON. Retry with shorter arguments that fit in one response.")
       (_
        "The tool call was rejected by llm.el. Correct its name or arguments and retry the tool."))
     type
     detail)))

(defun superchat--llm-round-limit-action ()
  "Return the action to take after exhausting the current tool budget."
  (pcase superchat-llm-round-limit-action
    ('ask (if noninteractive
              'finalize
            (if (y-or-n-p
                 (format "Tool round limit reached. Allow another %d rounds? "
                         superchat-llm-max-tool-rounds))
                'continue
              'finalize)))
    ((or 'finalize 'stop) superchat-llm-round-limit-action)
    (_ 'stop)))

(defun superchat--llm-disable-prompt-tools (prompt)
  "Remove tools from mutable llm.el PROMPT for its final request."
  (when (fboundp 'llm-chat-prompt-tools)
    ;; As with `superchat--llm-drop-dangling-tool-use', llm.el may load
    ;; after this file.  Resolve the struct slot only when the accessor is
    ;; live instead of baking an unavailable setf expander into this defun.
    (let ((slot (cl-struct-slot-offset 'llm-chat-prompt 'tools)))
      (if (eq (cl-struct-sequence-type 'llm-chat-prompt) 'list)
          (setf (nth slot prompt) nil)
        (aset prompt slot nil)))))

(defun superchat--llm-drop-dangling-tool-use (prompt)
  "Remove PROMPT's trailing assistant tool-use record, if present.

llm.el writes the assistant's tool calls into PROMPT before executing
them (`llm-provider-populate-tool-uses').  When a call names an unknown
tool, no tool-result interaction ever follows, and re-sending the prompt
with that dangling record violates the provider conversation contract
\(an assistant tool call must be answered by a tool result).  Returns
non-nil when a record was dropped."
  (when (and (fboundp 'llm-chat-prompt-interactions)
             (fboundp 'llm-chat-prompt-interaction-role))
    (let* ((interactions (llm-chat-prompt-interactions prompt))
           (last-interaction (car (last interactions))))
      (when (and last-interaction
                 (or (not (fboundp 'llm-chat-prompt-p))
                     (llm-chat-prompt-p prompt))
                 (eq (llm-chat-prompt-interaction-role last-interaction)
                     'assistant)
                 ;; Plain prose is a string; tool-use records are not.
                 (not (stringp (llm-chat-prompt-interaction-content
                                last-interaction))))
        ;; `superchat-llm.el' may load before optional `llm.el'.  Using the
        ;; accessor directly in `setf' then expands to a late-bound
        ;; `(setf llm-chat-prompt-interactions)' call, which does not exist.
        ;; Resolve the slot offset at runtime instead, after llm.el has made
        ;; the prompt struct available.
        (let ((slot (cl-struct-slot-offset 'llm-chat-prompt 'interactions))
              (value (butlast interactions)))
          (if (eq (cl-struct-sequence-type 'llm-chat-prompt) 'list)
              (setf (nth slot prompt) value)
            (aset prompt slot value)))
        t))))

(defun superchat--image-attachment-info (file)
  "Return supported image metadata for FILE, or nil."
  (when-let* ((mime-type
               (pcase (downcase (or (file-name-extension file) ""))
                 ((or "jpg" "jpeg") "image/jpeg")
                 ("png" "image/png")
                 ("webp" "image/webp"))))
    (list :path file :mime-type mime-type
          :size (file-attribute-size (file-attributes file)))))

(defun superchat--image-media (info)
  "Read image attachment INFO as native `llm-media'."
  (let ((path (plist-get info :path)))
    (unless (file-readable-p path)
      (user-error "Image attachment is not readable: %s" path))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally path)
      (make-llm-media :mime-type (plist-get info :mime-type)
                      :data (buffer-string)))))

(defun superchat--build-llm-prompt
    (text tools &optional context preset context-files)
  "Build an `llm-chat-prompt' struct from TEXT and TOOLS.
llm.el ≥ 0.7 requires a struct for `llm-chat-streaming' / `llm-chat',
even when no tools are attached.  TOOLS may be nil.

CONTEXT, when a non-empty string, is passed as the prompt's
`:context' — llm.el turns it into the system message.  This is how
turn system-prompts (preset persona, language instruction, tool
guidance) reach the provider.

PRESET may override temperature, max tokens, and reasoning for this
request.  Otherwise reasoning comes from `superchat-llm-reasoning' so that
reasoning-capable providers (Ollama qwen3.x, deepseek-r1, etc.) skip
thinking by default — thinking blocks streaming and inflates TTFT.

Supported image CONTEXT-FILES become native `llm-multipart' media."
  (let* ((preset-reasoning (and preset (superchat-preset-reasoning preset)))
         (reasoning (if (and preset-reasoning
                             (not (eq preset-reasoning 'inherit)))
                        preset-reasoning
                      superchat-llm-reasoning))
         (images (delq nil (mapcar #'superchat--image-attachment-info
                                   context-files)))
         (content (if images
                      (apply #'llm-make-multipart text
                             (mapcar #'superchat--image-media images))
                    text))
         (args (append (when tools (list :tools tools))
                       (when (and (stringp context)
                                  (not (string-empty-p (string-trim context))))
                         (list :context context))
                       (when (and preset (superchat-preset-temperature preset))
                         (list :temperature
                               (superchat-preset-temperature preset)))
                       (when (and preset (superchat-preset-max-tokens preset))
                         (list :max-tokens
                               (superchat-preset-max-tokens preset)))
                       (when reasoning
                         (list :reasoning (if (eq reasoning t)
                                             'medium
                                           reasoning))))))
    (apply #'llm-make-chat-prompt content args)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-chat-model ((provider llm-openai))
     (llm-openai-chat-model provider)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-openai) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-openai 'llm-openai new-model)))

(eval-after-load 'llm-claude
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-claude) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-claude 'llm-claude new-model)))

(eval-after-load 'llm-ollama
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-ollama) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-ollama 'llm-ollama new-model)))


;; --- LLM Backend (Extracted from supertag-rag.el) ---

;; Provider accessors. The real llm.el v0.7+ uses cl-defstruct providers
;; (e.g. `llm-openai'), not plists. These cl-defgenerics let us extract
;; the chat-model generically and override it (e.g. for `@model') per
;; provider type, with safe no-op defaults for providers we don't have
;; specialized methods for.

(cl-defmethod superchat--provider-name (provider)
  "Return a human-readable name for PROVIDER.
Default fallback when no specialized method matches.
Tries `llm-name' when fbound (handles both string and symbol returns),
otherwise returns a placeholder.  Specialized methods (installed via
`eval-after-load' for known provider structs) take precedence over
this one."
  (if (fboundp 'llm-name)
      (let ((name (ignore-errors (llm-name provider))))
        (cond
         ((null name) "unknown")
         ((stringp name) (downcase name))
         ((symbolp name) (downcase (symbol-name name)))
         (t (downcase (format "%s" name)))))
    "unknown"))

(cl-defmethod superchat--provider-chat-model (provider)
  "Return chat-model of PROVIDER, or nil.
Default: cannot extract chat-model from this provider type.
Specialized methods (installed via `eval-after-load' for known
provider structs) take precedence over this one."
  nil)

(cl-defmethod superchat--provider-with-chat-model (provider new-model)
  "Return a copy of PROVIDER with model set to NEW-MODEL.
Default: cannot override model on this provider type."
  provider)

;; --- Dispatchers ---

(defun superchat--callback-in-view (buffer-name callback)
  "Return a CALLBACK that runs in the Superchat View BUFFER-NAME."
  (lambda (&rest args)
    (let ((superchat-buffer-name buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
        (apply callback args)))))

(defun superchat--llm-request-plz-propagate-callback-errors
    (original url &rest args)
  "Call ORIGINAL for URL, routing async success callback errors to `:on-error'."
  (let ((on-success (plist-get args :on-success))
        (on-error (plist-get args :on-error)))
    (when (functionp on-success)
      (setq args
            (plist-put
             args :on-success
             (lambda (response)
               (condition-case err
                   (funcall on-success response)
                 (llm-error
                  (if (functionp on-error)
                      (funcall on-error (car err) (error-message-string err))
                    (signal (car err) (cdr err)))))))))
    (apply original url args)))

(with-eval-after-load 'llm-request-plz
  (unless (advice-member-p
           #'superchat--llm-request-plz-propagate-callback-errors
           'llm-request-plz-async)
    (advice-add 'llm-request-plz-async :around
                #'superchat--llm-request-plz-propagate-callback-errors)))

(defun superchat--llm-watch-plz-request (request active-p)
  "Finalize REQUEST when plz misses its asynchronous process sentinel.

ACTIVE-P returns non-nil while the originating Runtime request is live."
  (when (and (processp request)
             (eq (process-sentinel request) 'plz--sentinel))
    (let (timer)
      (setq timer
            (run-with-timer
             1 1
             (lambda ()
               (cond
                ((not (funcall active-p))
                 (cancel-timer timer))
                ((process-live-p request) nil)
                (t
                 (cancel-timer timer)
                 (when (and (buffer-live-p (process-buffer request))
                            (eq :plz-result
                                (process-get request :plz-result))
                            (not (eq 'buffer
                                     (process-get request :plz-as))))
                   (funcall (process-sentinel request)
                            request "workaround"))))))))))

(defun superchat--llm-generate-answer
    (prompt callback stream-callback
            &optional target-model context-files tools system-prompt preset)
  "Generate an answer for PROMPT using llm.el, handling streaming and tool use.
Optionally use TARGET-MODEL for this request only.
CONTEXT-FILES is an optional list of file paths to include as context.
TOOLS is an optional list of tool names to expose; when nil, tools are
collected from PROMPT and global settings as before.
SYSTEM-PROMPT, when a non-empty string, is sent as the system
message via the llm prompt's `:context' (turn system-prompts:
preset persona, language instruction, tool guidance).
CALLBACK is called with the final response string (or with
\(tool-call . ...) /\(tool-result . ...) markers for back-compat).
STREAM-CALLBACK is called with each text chunk during streaming.

llm.el executes requested tools and mutates its prompt with their
results.  Superchat reuses that prompt to issue subsequent model
requests until it receives text, bounded by
`superchat-llm-max-tool-rounds'."
  ;; The transport still reads distribution config (backend, timeouts,
  ;; TTFT and response-mode UI) owned by superchat.el.  A kernel-only
  ;; caller reaching the port directly loads the distribution once here;
  ;; extracting those seams from the monolith is the next deepening step.
  (unless (boundp 'superchat-llm-backend)
    (require 'superchat nil t))
  (if (null superchat-llm-backend)
      (let ((response
             "[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]"))
        (when (functionp superchat--llm-runtime-observer)
          (funcall superchat--llm-runtime-observer
                   'terminal (list :status 'failed :response response)))
        (when callback
          (funcall callback response)))
    ;; Collect tools and detect mode ONCE per request — both
    ;; `--show-response-mode-indicator' and `--get-adjusted-timeout'
    ;; used to recompute this independently, which meant 3 sync
    ;; MCP polls and 3 tool-set walks before the first byte left
    ;; the keyboard.
    (let* ((request-buffer-name superchat-buffer-name)
           (runtime-observer superchat--llm-runtime-observer)
           (tools-prepared-p superchat--llm-tools-prepared-p)
           (tools (if tools-prepared-p
                      superchat--llm-prepared-tools
                    (superchat--collect-llm-tools
                     (when (stringp prompt) prompt)
                     tools)))
           ;; One wrapper closure per request: the run's tools share a
           ;; guardrail set and a call counter, and a tool the agent
           ;; synthesizes mid-run must join the same accounting.
           (wrap-fn
            (or superchat--llm-tool-wrapper
                (and tools
                     (not tools-prepared-p)
                     (fboundp 'superchat--agent-make-wrapper)
                     (superchat--agent-make-wrapper preset))))
           (tools (if (and wrap-fn (not tools-prepared-p))
                      (mapcar wrap-fn tools)
                    tools))
           (response-mode (superchat--detect-response-mode tools t))
           (_ (superchat--show-response-mode-indicator response-mode))
           (adjusted-timeout (superchat--get-adjusted-timeout response-mode))
           (effective-backend (superchat--effective-llm-backend target-model))
           ;; Only stream tool calls when the provider explicitly promises
           ;; that its streaming decoder preserves them.  Plain chat remains
           ;; streaming; tool-capable providers without that capability use
           ;; llm.el's non-streaming async path instead.
           (streaming-request-p
            (or (null tools)
                (and (fboundp 'llm-capabilities)
                     (memq 'streaming-tool-use
                           (ignore-errors
                             (llm-capabilities effective-backend))))))
           (real-prompt (superchat--build-llm-prompt
                         prompt tools system-prompt preset context-files))
           ;; Bind `define_tool' to THIS prompt, so what it writes can be
           ;; injected into the request that is already in flight and used
           ;; on the next round.  Persist to the session registry: unlike a
           ;; sub-agent, the main session has later turns.
           (_ (when (fboundp 'superchat--syn-bind)
                (superchat--syn-bind
                 real-prompt wrap-fn
                 superchat--llm-persist-synthesized-tools)))
           (multi-output (and tools t))
           (response-parts '())
           (tool-rounds 0)
           (tool-round-limit superchat-llm-max-tool-rounds)
           (final-answer-requested nil)
           (completed nil)
           (timeout-timer nil)
           ;; ── TTFT measurement (meausred flag is lexical, closure-safe) ──
           (ttft-measured nil)
           ;; Tracks the cumulative streamed text as reported by
           ;; llm.el so we can compute the actual delta to forward
           ;; downstream.  llm.el's partial-callback semantics are the
           ;; same in both modes ("the entire response that has been
           ;; received" — see `llm-chat-streaming' and
           ;; `llm-provider-utils-streaming-accumulate'):
           ;;   - multi-output=nil  → chunk is the FULL text so far
           ;;   - multi-output=t    → chunk is a plist; :text is the
           ;;                         FULL text so far as well
           ;; We normalise both paths to deltas so `response-parts'
           ;; and `stream-callback' always see incremental text.
           (cumulative "")
           (tool-error-retries 0)
           (unknown-tool-retry-limit
            (or superchat--llm-unknown-tool-retry-limit
                superchat-llm-unknown-tool-retries))
           (round-limit-function superchat--llm-round-limit-function)
           (run-active-p (or superchat--llm-run-active-p (lambda () t)))
           (request-generation 0)
           (send-request nil)
           ;; ---- local helper: finalize exactly once ----
           (finalize
            (lambda (response &optional status)
              (unless completed
                (setq completed t)
                (when timeout-timer (cancel-timer timeout-timer))
                (with-current-buffer (get-buffer-create request-buffer-name)
                  (setq superchat--active-timeout-timer nil))
                (when (functionp runtime-observer)
                  (funcall runtime-observer
                           'terminal
                           (list :status (or status 'completed)
                                 :response response)))
                (when callback
                  (funcall callback response)))))
           ;; Holds elapsed seconds captured on the FIRST stream-cb call
           ;; (any chunk type) until we can annotate the Assistant header
           ;; on the first text chunk.  nil once consumed.
           (stream-cb
            (lambda (chunk)
              (let* ((text (cond
                            ((stringp chunk) chunk)
                            ((and (consp chunk)
                                  (stringp (plist-get chunk :text)))
                             (plist-get chunk :text)))))
                (cond
                 ;; text chunk — cumulative full text in BOTH payload shapes
                 ((stringp text)
                  (unless completed
                    (let ((delta
                           (cond
                            ((string-prefix-p cumulative text)
                             (substring text (length cumulative)))
                            ;; defensive: provider reset / out-of-order — emit full
                            (t text))))
                      (setq cumulative text)
                      (unless (string-empty-p delta)
                        ;; ── TTFT: measure on first non-empty text delta ──
                        ;; This is the ONLY reliable signal that the LLM has
                        ;; actually started generating.  Empty flushes, nil
                        ;; callbacks, reasoning blocks, and tool-call plists
                        ;; are all filtered out before we reach this point.
                        (when (and superchat-show-ttft
                                   (not ttft-measured))
                          (setq ttft-measured t)
                          (with-current-buffer (get-buffer-create request-buffer-name)
                            (when superchat--ttft-start-time
                              (let ((elapsed (- (float-time) superchat--ttft-start-time)))
                                (message "⚡ TTFT: %.2fs" elapsed)
                                (superchat--annotate-ttft elapsed)))))
                        (push delta response-parts)
                        (when stream-callback
                          (funcall stream-callback delta)))
                      (when (fboundp 'superchat--extend-timeout)
                        (superchat--extend-timeout)))))
                 ;; tool-call/tool-result markers — forward to caller for back-compat
                 ((and (consp chunk)
                       (memq (car chunk) '(tool-call tool-result)))
                  (when callback
                    (funcall callback chunk))
                  (when (fboundp 'superchat--extend-timeout)
                    (superchat--extend-timeout)))
                 ;; reasoning block — ignore
                 ((and (consp chunk) (eq (car chunk) 'reasoning))
                  nil)
                 (t
                  (message "superchat: unhandled stream payload: %S" chunk))))))
           (response-cb nil)
           ;; llm.el calls the error callback with TWO arguments
           ;; (type message) — see `llm-provider-utils-callback-in-buffer'
           ;; call sites, every one `error-callback 'error <msg>'.  A
           ;; one-argument lambda here crashes inside plz's timer with
           ;; `wrong-number-of-arguments', which then masks the real API
           ;; error.  The sub-agent path already takes (_type message);
           ;; `&rest' keeps this robust if llm.el ever changes the arity.
           ;; Bound via setq below so its body can reference `error-cb'
           ;; for the unknown-tool re-send (a let* self-reference
           ;; would be a free variable).
           (error-cb nil))

      ;; Safety-net timeout. Fires only when nothing else has finalised.
      (when adjusted-timeout
        (setq timeout-timer
              (run-with-timer
               adjusted-timeout nil
               (lambda ()
                 (funcall finalize
                          (if response-parts
                              (string-join (nreverse response-parts) "")
                            "[Response timeout. Try: (setq superchat-response-timeout 300)]")
                          'timed-out))))
        (with-current-buffer (get-buffer-create request-buffer-name)
          (setq superchat--active-timeout-timer timeout-timer)))

      (when target-model
        (message "Switching to model: %s" target-model))

      ;; ── TTFT measurement: reset per-request flag ──
      (when superchat-show-ttft
        (setq ttft-measured nil))

      (superchat--ttft-log "pre-llm-call")

      ;; ── Prompt size diagnostic ──
      (when superchat-show-ttft-breakdown
        (let* ((chars (length (or (and (stringp prompt) prompt) "")))
               (tools-chars (if tools (length (format "%S" tools)) 0))
               (history-len (length superchat--conversation-history))
               (est-tokens (/ (+ chars tools-chars) 4)))
          (message "⏱ TTFT [prompt-size]: %d chars + %d tools + %d history-msgs (~%d tokens)"
                   chars tools-chars history-len est-tokens)))

      (setq error-cb
            (lambda (type &rest more)
              (let ((err (if more (car more) type)))
                (cond
                 ;; Tool-call validation errors are model-correctable.  The
                 ;; provider has already appended an assistant tool-use
                 ;; record, so remove that dangling record, append the exact
                 ;; llm.el error as a user-role correction, and resend.
                 ((and (fboundp 'superchat--llm-tool-protocol-error-p)
                       (superchat--llm-tool-protocol-error-p type)
                       (not final-answer-requested)
                       (not completed)
                       (funcall run-active-p)
                       (< tool-error-retries
                          unknown-tool-retry-limit)
                       (fboundp 'superchat--llm-drop-dangling-tool-use)
                       (fboundp 'llm-chat-prompt-append-response))
                  (cl-incf tool-error-retries)
                  (superchat--llm-drop-dangling-tool-use real-prompt)
                  (llm-chat-prompt-append-response
                   real-prompt
                   (superchat--llm-tool-protocol-error-feedback type err)
                   'user)
                  (setq response-parts '() cumulative "")
                  (condition-case e
                      (funcall send-request)
                    (error
                     (funcall finalize
                              (format "[llm-chat-streaming error: %s]"
                                      (error-message-string e))
                              'failed))))
                 (t
                  (funcall finalize
                           (cond
                            (response-parts
                             (string-join (nreverse response-parts) ""))
                            (t
                             (format "[API error: %s]"
                                     (or (and (stringp err) err)
                                         (and (consp err) (format "%S" err))
                                         "unknown"))))
                           'failed))))))
      (setq response-cb
            (lambda (response)
              (when (funcall run-active-p)
                (cond
                 ((superchat--llm-tool-round-p response)
                  (cond
                   (final-answer-requested
                    (funcall finalize
                             "[Tool call returned after requesting a final answer]"
                             'failed))
                   (t
                    (let ((continue t))
                      (cl-incf tool-rounds)
                      (when (and (numberp tool-round-limit)
                                 (>= tool-rounds tool-round-limit))
                        (pcase (if (functionp round-limit-function)
                                   (funcall round-limit-function)
                                 (superchat--llm-round-limit-action))
                        ('continue
                         (if (and (numberp superchat-llm-max-tool-rounds)
                                  (> superchat-llm-max-tool-rounds 0))
                             (setq tool-round-limit
                                   (+ tool-round-limit
                                      superchat-llm-max-tool-rounds))
                           (setq final-answer-requested t
                                 multi-output nil)
                           (superchat--llm-disable-prompt-tools real-prompt)))
                        ('finalize
                         (setq final-answer-requested t
                               multi-output nil)
                         (superchat--llm-disable-prompt-tools real-prompt))
                        ('stop
                         (setq continue nil)
                         (funcall finalize
                                  (format "[Stopped after %d tool rounds]"
                                          tool-rounds)
                                  'failed))))
                      (when (and continue (not completed))
                        (when (not final-answer-requested)
                          (let ((feedback
                                 (superchat--llm-tool-error-feedback response))
                                (non-retryable
                                 (superchat--llm-tool-error-non-retryable-p
                                  response)))
                            (when feedback
                              (when (fboundp 'llm-chat-prompt-append-response)
                                (llm-chat-prompt-append-response
                                 real-prompt feedback 'user)))
                            ;; A user/policy refusal is terminal for this
                            ;; tool call; request one tool-free explanation
                            ;; rather than resending the blocked operation.
                            (when non-retryable
                              (setq final-answer-requested t
                                    multi-output nil)
                              (superchat--llm-disable-prompt-tools
                               real-prompt))))
                        ;; Close this round's rendered text before the next
                        ;; request: the transcript keeps the round's prose and
                        ;; the tool records, and the next round's text starts a
                        ;; fresh region after them.  Also drop this round's
                        ;; streamed parts so they cannot leak into the final
                        ;; answer's fallback join.
                        (when (fboundp 'superchat--render-close-tool-round)
                          (superchat--render-close-tool-round))
                        (setq response-parts '())
                        ;; Each round is a fresh llm request whose
                        ;; accumulated text restarts from empty.
                        (setq cumulative "")
                        (condition-case err
                            (funcall send-request)
                          (error
                           (funcall finalize
                                    (format "[llm-chat-streaming error: %s]"
                                            (error-message-string err))
                                    'failed))))))))
                  (t
                   (funcall finalize
                            (cond
                             ((and (consp response)
                                   (stringp (plist-get response :text))
                                   (not (string-empty-p
                                         (plist-get response :text))))
                              (plist-get response :text))
                             ((and (stringp response)
                                   (not (string-empty-p response)))
                              response)
                             (response-parts
                              (string-join (nreverse response-parts) ""))
                             (t "[Empty response from llm]"))))))))

      ;; llm.el may invoke these after the initiating dynamic binding has
      ;; unwound.  Route every round through the originating Session View.
      (setq finalize (superchat--callback-in-view request-buffer-name finalize)
            stream-cb (superchat--callback-in-view request-buffer-name stream-cb)
            response-cb (superchat--callback-in-view request-buffer-name response-cb)
            error-cb (superchat--callback-in-view request-buffer-name error-cb))

      (setq send-request
            (lambda ()
              (let* ((generation (cl-incf request-generation))
                     (request
                      (if streaming-request-p
                          (llm-chat-streaming effective-backend
                                              real-prompt
                                              stream-cb
                                              response-cb
                                              error-cb
                                              multi-output)
                        (llm-chat-async effective-backend
                                        real-prompt
                                        response-cb
                                        error-cb
                                        multi-output))))
                (when (and (= generation request-generation)
                           (functionp runtime-observer))
                  (funcall runtime-observer 'request request))
                (superchat--llm-watch-plz-request
                 request
                 (lambda ()
                   (and (not completed)
                        (funcall run-active-p))))
                request)))

      (condition-case err
          (funcall send-request)
        (error
         (funcall finalize
                  (format "[llm-chat-streaming error: %s]"
                          (error-message-string err))
                  'failed))))))

(provide 'superchat-llm)
;;; superchat-llm.el ends here
