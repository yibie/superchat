;;; superchat-llm.el --- LLM backend abstraction for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM provider abstraction, tool collection, prompt building, and
;; streaming answer generation.  Extracted from superchat.el monolith
;; (v0.9 split step 5).

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-event)
(require 'superchat-select-tools)
(require 'llm nil t)
;; The model port stays UI-free: streaming sinks arrive through the
;; Runtime request, and MCP tools are picked up behind `fboundp' when the
;; optional bridge is loaded.
(require 'superchat-preset)

;; ── Kernel-owned transport configuration ──
;; Keep each default and its Custom metadata in one declaration so saved
;; values are honored when this Kernel module is loaded after the init file.
(defgroup superchat nil
  "Configuration for superchat, a standalone AI chat client."
  :group 'external)

(defvar superchat-buffer-name)  ; the defcustom lives in superchat.el (S25)

(defcustom superchat-context-max-chars nil
  "Maximum rendered characters of recent conversation included in prompts.
Raw facts are included whole; an oversized newest fact is disclosed but not
forced into the prompt.  Set to nil to derive the budget from the trusted
model context window, or fall back to 40000 characters when no trusted window
is available."
  :type '(choice (const :tag "Derive from model window" nil)
                 (natnum :tag "Characters"))
  :group 'superchat)

(defcustom superchat-context-chars-per-token 2
  "Conservative character-per-token conversion used for context budgets.
The value is used in both directions: it converts measured prompt characters
into reserved tokens and converts the remaining model window back to the
character budget used by Context View."
  :type '(number :tag "Characters per token")
  :group 'superchat)

(defcustom superchat-context-fixed-reserve 4096
  "Fixed token reserve for experience, tool schemas, and reply headroom.
This reserve is added to the measured system and current-turn prompt tokens
when deriving a Context View character budget."
  :type '(natnum :tag "Reserved tokens")
  :group 'superchat)

(defcustom superchat-response-timeout 120
  "Maximum inactivity interval in seconds for each provider attempt.

The response-mode multiplier and current prompt size may raise this interval.
It starts immediately before dispatch, restarts on new answer, reasoning or
tool-call data, and ends when that attempt completes or fails.  Empty or
repeated cumulative chunks do not count as progress.  Tool continuations and
retries get a new interval even when an earlier attempt produced answer text.

This timer does not declare partial work complete.  Expiration reports a
timed-out response; the cumulative hard cap and Runtime deadline remain
independent.  Existing approval extensions affect only this inactivity wait.

Set to nil to disable both transport timers."
  :type '(choice (const :tag "No timeout" nil)
                 (integer :tag "Seconds"))
  :group 'superchat)

(defcustom superchat-response-timeout-hard-limit nil
  "Optional cumulative transport time limit, in seconds, for one tool conversation.

Nil, the default, means there is no such limit.  A Run is still bounded by
the per-attempt inactivity timeout (`superchat-response-timeout'), the tool
round budget and the Run deadline (`superchat-runtime-default-deadline'),
which together cover everything this limit did.  It used to default to 600:
under the default permission level a person reading each approval question
could use that up and lose the Run while a question was still open.

With a positive integer the old behaviour applies: the adaptive inactivity
interval never grows beyond this limit, neither progress nor a new provider
attempt resets it, a trusted ask_user wait freezes its remaining budget, and
ordinary tool-boundary pauses -- approval questions included -- consume it.

Set `superchat-response-timeout' to nil to disable both transport timers."
  :type '(choice (const :tag "No cumulative limit" nil)
                 (restricted-sexp
                  :tag "Seconds"
                  :match-alternatives ((lambda (value)
                                         (and (integerp value) (> value 0))))))
  :group 'superchat)

(defcustom superchat-tool-timeout-multiplier 1.5
  "Timeout multiplier for tool calling mode.
Default is 1.5, which adds 50% to the base timeout.
Tool calling (especially with cloud models or Ollama) may need more time."
  :type 'number
  :group 'superchat)

(defcustom superchat-llm-max-tool-rounds 12
  "Maximum consecutive tool rounds before Superchat stops the loop.

One round is a model response that invokes one or more tools.  The
limit bounds the depth of a tool conversation; agent tool wrappers
separately bound the total number of tool calls.  Set this to nil to
disable the round limit."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Rounds"))
  :group 'superchat)

(defcustom superchat-llm-finish-check t
  "Whether a tool-using chat Run gets one finish check.
When a text-only reply is empty or its last sentence announces the
model's own next action, one system-originated note is queued for the
Run's next Step and the Run takes one more request.  The check fires at
most once per Run and can cost one extra request."
  :type 'boolean
  :group 'superchat)

(defconst superchat-llm--finish-check-note
  (concat "Your last message has no tool call, so the task would end here. "
          "If a step you described is still undone, do it now with a tool; "
          "do not retry a call the user refused. "
          "If everything is done, give your final report: what you changed "
          "and where, what is unsaved, and what you did not verify.")
  "The one note the finish check queues for the Run's next Step.")

(defcustom superchat-llm-unknown-tool-retries 2
  "How many invalid tool calls the main loop recovers from.

llm.el reports unknown tool names and malformed arguments as fatal
`llm-tool-*' errors.  Up to this many times per response, the request is
repaired (the dangling tool-use record dropped, a corrective message
appended) and re-sent instead of ending the turn.  The sub-agent loop has
the same guard (`superchat-subagent-unknown-tool-retries').  Set to 0 to
keep the fatal behaviour."
  :type 'natnum
  :group 'superchat)

(defcustom superchat-llm-transient-retries 10
  "Maximum consecutive transient provider retries within one Run.
The count resets after a successful provider response.  Set to zero to
fail immediately on a retryable transport or HTTP error."
  :type 'natnum
  :group 'superchat)

(defcustom superchat-llm-transient-retry-interval 30
  "Fixed delay in seconds before each transient provider retry.
Every retry waits this interval; there is no backoff or jitter."
  :type 'number
  :group 'superchat)

(defvar superchat--llm-http-status nil
  "HTTP status of the failed provider request being reported, or nil.")

(declare-function plz-error-response "plz")
(declare-function plz-response-status "plz")

(defun superchat--llm-capture-http-status (original error on-error)
  "Call ORIGINAL with ERROR and ON-ERROR while retaining its HTTP status.
The binding spans llm.el's synchronous chat error callback and then ends."
  (let ((superchat--llm-http-status
         (condition-case nil
             (when (and (fboundp 'plz-error-p) (plz-error-p error))
               (let ((response (plz-error-response error)))
                 (when (and response
                            (fboundp 'plz-response-p)
                            (plz-response-p response))
                   (plz-response-status response))))
           (error nil))))
    (funcall original error on-error)))

(with-eval-after-load 'llm-request-plz
  (when (and (fboundp 'llm-request-plz--handle-error)
             (not (advice-member-p #'superchat--llm-capture-http-status
                                   'llm-request-plz--handle-error)))
    (advice-add 'llm-request-plz--handle-error :around
                #'superchat--llm-capture-http-status)))

(defun superchat--llm-transient-error-p (type err &optional http-status)
  "Return non-nil when TYPE, ERR and HTTP-STATUS identify a retryable error.
Only HTTP 429 and 5xx retry.  Without an HTTP status, recognize the pinned
llm transport strings; never infer HTTP status from a response body."
  (and (eq type 'error)
       (if (numberp http-status)
           (or (= http-status 429)
               (<= 500 http-status 599))
         (and (stringp err)
              (or (equal err "Operation timeout.")
                  (string-match-p
                   "\\`curl error code \\(6\\|7\\|35\\|52\\|55\\|56\\): " err))))))

(defcustom superchat-llm-round-limit-action 'ask
  "What to do when `superchat-llm-max-tool-rounds' is exhausted.

`ask' prompts in interactive Emacs.  Yes grants another full round
budget; no sends one last request with tools removed.  In batch mode,
`ask' behaves as `finalize' so no process waits for input.
`finalize' always sends that final tool-free request, while `stop'
ends the turn immediately."
  :type '(choice (const :tag "Ask, then finalize" ask)
                 (const :tag "Finalize without tools" finalize)
                 (const :tag "Stop immediately" stop))
  :group 'superchat)

(declare-function superchat--invalidate-model-cache "superchat-models" ())

(defcustom superchat-llm-backend nil
  "The llm provider struct for sending chat requests.
Set this to a provider struct created by `make-llm-openai',
`make-llm-claude', `make-llm-ollama', etc., e.g.:

  (setq superchat-llm-backend
        (make-llm-openai :key \"sk-...\" :chat-model \"gpt-4o-mini\"))

When nil, superchat will refuse to start a chat and prompt you to
configure this.  See the llm.el documentation for all supported
providers.  Each provider lives in its own autoloaded file
\(llm-openai, llm-claude, llm-ollama, llm-gemini, etc.)."
  :type '(choice (const :tag "Unconfigured (set me!)" nil)
                 (sexp :tag "Provider struct (make-llm-*)"))
  :set (lambda (sym val)
         (set-default sym val)
         (when (fboundp 'superchat--invalidate-model-cache)
           (superchat--invalidate-model-cache)))
  :group 'superchat)

(defcustom superchat-llm-model nil
  "Override the chat model name used for the next request.
When nil, the model's `:chat-model' from `superchat-llm-backend' is used.
Set this to switch models for a single request (e.g. via the @model
in-chat syntax) without reconfiguring the backend."
  :type '(choice (const :tag "Use backend default" nil)
                 (string :tag "Model name"))
  :group 'superchat)

(defcustom superchat-llm-context-window-overrides nil
  "Per-model context-window overrides in tokens.

Each entry is (MODEL . TOKENS).  Use this when a local runtime loads a
model with a smaller window than the model's advertised limit, for example:

  ((\"qwen/qwen3.8-27b\" . 8192))

Models without an override use the limit reported by llm.el."
  :type '(alist :key-type string :value-type integer)
  :group 'superchat)

(defcustom superchat-llm-output-token-reserve 2048
  "Output-token budget reserved when checking a provider context window."
  :type '(restricted-sexp
          :match-alternatives ((lambda (value)
                                 (and (integerp value) (> value 0)))))
  :group 'superchat)

(defcustom superchat-llm-streaming t
  "When non-nil, stream LLM responses into the chat buffer.
Disable for non-streaming backends or when debugging.
Uses `llm-chat-streaming-to-point'."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-llm-model-capability-overrides nil
  "Per-model streaming capability declarations.

Each entry is (MODEL :streaming BOOL :streaming-tool-use BOOL).  Set with
`superchat-llm-set-model-capabilities'.  Use this when a provider cannot
report a local or unrecognized model's true streaming capability, for
example a model on an OpenAI-compatible endpoint whose name llm.el does
not match against its known-model table.  An override takes precedence
over both the provider's reported capabilities and the adapter's
provider-type default; see `superchat-llm--resolve-streaming'."
  :type '(alist :key-type string :value-type sexp)
  :group 'superchat)

(defun superchat-llm-set-model-capabilities (model plist)
  "Declare streaming capabilities for MODEL.

MODEL must be a non-empty string.  PLIST must supply both :streaming and
:streaming-tool-use, each exactly `nil' or `t' — not merely non-nil, so a
typo such as a string or a number cannot silently enable a capability.
Stores the declaration into `superchat-llm-model-capability-overrides',
keyed by MODEL; `superchat-llm--resolve-streaming' consults it first,
ahead of the provider and adapter levels."
  (unless (and (stringp model) (not (string-empty-p model)))
    (user-error
     "superchat-llm-set-model-capabilities: MODEL must be a non-empty string, got %S"
     model))
  (dolist (key '(:streaming :streaming-tool-use))
    (unless (plist-member plist key)
      (user-error
       "superchat-llm-set-model-capabilities: PLIST is missing %s" key))
    (let ((value (plist-get plist key)))
      (unless (memq value '(nil t))
        (user-error
         "superchat-llm-set-model-capabilities: %s must be nil or t, got %S"
         key value))))
  (setf (alist-get model superchat-llm-model-capability-overrides
                    nil nil #'equal)
        (list :streaming (plist-get plist :streaming)
              :streaming-tool-use (plist-get plist :streaming-tool-use))))

(defun superchat-llm--resolve-streaming (backend model tools-p)
  "Resolve streaming capabilities for BACKEND/MODEL.

Returns (:streaming BOOL :streaming-tool-use BOOL :source SYMBOL).  Each
boolean is decided independently by the first level that has an opinion:
1. override (`superchat-llm-model-capability-overrides', keyed by MODEL);
2. provider (`llm-capabilities' contains `streaming' / `streaming-tool-use');
3. adapter (provider type `llm-openai-compatible', exact match, implies
   both capabilities for any model; every other type has no entry);
4. fallback (both nil).
SOURCE names the level that decided :streaming-tool-use when TOOLS-P is
non-nil, otherwise the level that decided :streaming."
  (let* ((override
          (and model
               (alist-get model superchat-llm-model-capability-overrides
                          nil nil #'equal)))
         (provider-capabilities
          (and (fboundp 'llm-capabilities)
               (ignore-errors (llm-capabilities backend))))
         ;; `llm-deepseek' subtypes `llm-openai-compatible' in llm.el's
         ;; `cl-defstruct' hierarchy; `type-of' (not `cl-typep') keeps the
         ;; adapter rule from misfiring on subtypes it was not written for.
         (adapter-p (eq (type-of backend) 'llm-openai-compatible))
         streaming streaming-source
         streaming-tool-use streaming-tool-use-source)
    (cond
     ((plist-member override :streaming)
      (setq streaming (and (plist-get override :streaming) t)
            streaming-source 'override))
     ((memq 'streaming provider-capabilities)
      (setq streaming t streaming-source 'provider))
     (adapter-p
      (setq streaming t streaming-source 'adapter))
     (t
      (setq streaming nil streaming-source 'fallback)))
    (cond
     ((plist-member override :streaming-tool-use)
      (setq streaming-tool-use (and (plist-get override :streaming-tool-use) t)
            streaming-tool-use-source 'override))
     ((memq 'streaming-tool-use provider-capabilities)
      (setq streaming-tool-use t streaming-tool-use-source 'provider))
     (adapter-p
      (setq streaming-tool-use t streaming-tool-use-source 'adapter))
     (t
      (setq streaming-tool-use nil streaming-tool-use-source 'fallback)))
    (list :streaming streaming
          :streaming-tool-use streaming-tool-use
          :source (if tools-p streaming-tool-use-source streaming-source))))

(defcustom superchat-show-ttft t
  "When non-nil, show Time-To-First-Token (TTFT) in the echo area.
Measures the end-to-end interval from `C-c C-c' (user send) to the
first chunk arriving from the LLM streaming response.
Displayed as \"⚡ TTFT: X.XXs\" in echo area and on the Assistant header."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-llm-reasoning-parameter-overrides nil
  "Explicit reasoning parameter mappings for OpenAI-compatible endpoints.
Each entry is ((PROVIDER-TYPE CHAT-URL MODEL) PARAMETER
               (none . VALUE) (light . VALUE) (medium . VALUE)
               (maximum . VALUE)).
CHAT-URL is the complete chat endpoint, as returned by
`llm-provider-chat-url'.  Matching is exact; model names and local ports
alone do not establish the endpoint's parameter protocol.  PARAMETER is a
top-level non-standard parameter name and each VALUE is a non-empty string.
An unsupported effort may be omitted, in which case requesting it errors.

Only the exact `llm-openai-compatible' provider type uses this adapter.
Native providers and specialized compatible subtypes retain llm.el's mapping.
Explicit reasoning parameters in the prompt or provider configuration take
precedence over this declaration and over Superchat's reasoning setting.
This declares request serialization, not proof that a server honors a value."
  :type 'sexp
  :group 'superchat)

(define-error 'superchat-llm-reasoning-unsupported
  "Superchat cannot map the requested reasoning setting" 'user-error)

(declare-function llm-standard-chat-provider-default-chat-non-standard-params
                  "llm-provider-utils" (provider))
(declare-function llm-provider-chat-url "llm-provider-utils" (provider))

(defun superchat--llm-parameter-name (key)
  "Return a comparable name for non-standard parameter KEY."
  (when (or (symbolp key) (stringp key))
    (string-remove-prefix ":" (if (symbolp key) (symbol-name key) key))))

(defun superchat--llm-reasoning-parameter-p (pair declared-name)
  "Return non-nil if PAIR explicitly controls reasoning.
DECLARED-NAME also counts as a reasoning control when configured."
  (when (consp pair)
    (let ((name (superchat--llm-parameter-name (car pair))))
      (or (member name '("reasoning_effort" "reasoning" "thinking"
                         "think" "enable_thinking"))
          (and declared-name (equal name declared-name))
          (and (equal name "chat_template_kwargs")
               (if (hash-table-p (cdr pair))
                   (let (present)
                     (maphash
                      (lambda (key _value)
                        (when (equal (superchat--llm-parameter-name key)
                                     "enable_thinking")
                          (setq present t)))
                      (cdr pair))
                     present)
                 (and (listp (cdr pair))
                      (cl-some
                       (lambda (setting)
                         (and (consp setting)
                              (equal (superchat--llm-parameter-name (car setting))
                                     "enable_thinking")))
                       (cdr pair)))))))))

(defun superchat--llm-prepare-reasoning (provider prompt)
  "Resolve PROMPT's reasoning setting for PROVIDER before dispatch.
Modify only this request's PROMPT and return it.  Never mutate PROVIDER,
its shared non-standard parameters, or another request's prompt.

For an exact OpenAI-compatible provider, an explicit raw reasoning
parameter keeps its existing llm.el precedence and disables automatic
reasoning serialization.  Otherwise apply the exact endpoint/model
declaration, or signal `superchat-llm-reasoning-unsupported'.
A nil prompt reasoning setting leaves the server/default parameters alone.
Other provider types retain llm.el's native behavior."
  (when (and (eq (type-of provider) 'llm-openai-compatible)
             (llm-chat-prompt-reasoning prompt))
    (let* ((reasoning (llm-chat-prompt-reasoning prompt))
           (url (llm-provider-chat-url provider))
           (model (superchat--provider-chat-model provider))
           (key (list (type-of provider) url model))
           (declaration
            (cdr (assoc key superchat-llm-reasoning-parameter-overrides)))
           (parameter (car declaration))
           (parameter-name (superchat--llm-parameter-name parameter))
           (params (llm-chat-prompt-non-standard-params prompt))
           (defaults
            (llm-standard-chat-provider-default-chat-non-standard-params
             provider))
           (explicit
            (cl-some
             (lambda (pair)
               (superchat--llm-reasoning-parameter-p pair parameter-name))
             (append params defaults))))
      (cond
       (explicit
        ;; Leave llm.el to merge raw prompt/provider parameters exactly as
        ;; before; only suppress the automatic setting that could conflict.
        (setf (cl-struct-slot-value 'llm-chat-prompt 'reasoning prompt) nil))
       (declaration
        (let ((mapping (assq reasoning (cdr declaration))))
          (unless (and (stringp parameter-name)
                       (string-match-p
                        "\\`[A-Za-z_][A-Za-z0-9_]*\\'" parameter-name)
                       mapping
                       (stringp (cdr mapping))
                       (not (string-empty-p (cdr mapping))))
            (signal 'superchat-llm-reasoning-unsupported
                    (list (format
                           "No valid %s reasoning mapping for %s at %s"
                           reasoning model url))))
          (setf (cl-struct-slot-value 'llm-chat-prompt 'non-standard-params prompt)
                (cons (cons parameter (cdr mapping)) (copy-tree params t))
                (cl-struct-slot-value 'llm-chat-prompt 'reasoning prompt) nil)))
       (t
        (signal
         'superchat-llm-reasoning-unsupported
         (list
          (format
           (concat "Reasoning %s has no declared mapping for %s at %s; "
                   "use Preset reasoning inherit and superchat-llm-reasoning nil "
                   "for server defaults, retain explicit provider "
                   "non-standard reasoning parameters, "
                   "or configure superchat-llm-reasoning-parameter-overrides")
           reasoning model url)))))))
  prompt)


(defcustom superchat-llm-reasoning nil
  "Reasoning (a.k.a. thinking) effort to request from the model.

Recognized values mirror llm.el's spec:

  nil       — provider default (Superchat's default)
  none      — request disabled thinking
  light     — minimal reasoning effort
  medium    — moderate reasoning effort
  maximum   — maximum reasoning effort
  t         — enable thinking (same as medium)

A Preset's explicit effort overrides this value; inherit uses it.
Native providers use llm.el's support for their model.  The exact
OpenAI-compatible base provider requires an explicit raw reasoning
parameter or a matching declaration in
`superchat-llm-reasoning-parameter-overrides' for non-nil efforts.
An unavailable mapping fails before dispatch instead of claiming that
thinking was disabled.  Explicit raw parameters retain their precedence.
Nil leaves provider/server settings alone and does not promise a mode."
  :type '(choice (const :tag "Disable thinking (fastest)" none)
                 (const :tag "Enable thinking" t)
                 (const :tag "Provider default" nil)
                 (const :tag "Light"   light)
                 (const :tag "Medium"  medium)
                 (const :tag "Maximum" maximum))
  :group 'superchat)

(defcustom superchat-show-ttft-breakdown nil
  "When non-nil, log per-stage timing to *Messages* at each pipeline step.
Shows cumulative elapsed from `C-c C-c' at: input-parse, memory-recall,
core-pipeline, dispatch, pre-llm-call, and first-token.
Use this to diagnose where the end-to-end latency is spent."
  :type 'boolean
  :group 'superchat)


(defvar superchat--ttft-start-time nil)
(defvar superchat--active-timeout-timer nil)
(defvar superchat--active-timeout-timer-owner nil)
(defvar superchat--transport-timer-owner-order 0)
(defvar superchat--conversation-history nil)
;; Per-run transport bindings owned by superchat-runtime.el.
(defvar superchat--llm-runtime-observer nil)
(defvar superchat--llm-transcript-owner nil
  "Request configuration captured for tool-round presentation boundaries.")
(defvar superchat--llm-evidence-context nil)
(defvar superchat--llm-context-envelope nil)
(defvar superchat--llm-request-max-tokens nil
  "Request-local output-token limit bound by Runtime.")
(defvar superchat--llm-raw-tool-uses
  (make-hash-table :test #'eq :weakness 'key)
  "Eligible prompts awaiting provider tool-use capture before normalization.")
(defvar superchat--tool-execution-context)
(defvar superchat--llm-input-batches
  (make-hash-table :test #'eq :weakness 'key)
  "Actual dispatched prompts with an opt-in, request-invariant input tool name.
Values are detached strings, never tools or callbacks retaining the prompt.
Unlike raw capture, this association survives callbacks until the key is GC'd;
ordinary prompt reuse does not make the key a generation identity.")
(defvar superchat--llm-callback-attempt nil
  "Provider-attempt ordinal bound around one transport callback.")
(defvar superchat--llm-preflight-streaming-p nil
  "Request-local streaming shape used while serializing preflight input.")
(defvar superchat--llm-tool-origin nil
  "Immutable evidence origin bound while llm.el invokes one tool call.")
(defvar superchat--llm-tools-prepared-p nil)
(defvar superchat--llm-prepared-tools nil)
(defvar superchat--llm-tool-wrapper nil)
(defvar superchat--llm-round-limit-function nil)
(defvar superchat--llm-unknown-tool-retry-limit nil)
(defvar superchat--llm-persist-synthesized-tools nil)
(defvar superchat--llm-run-active-p nil)
(defvar superchat--llm-transport-timer-owner-function nil
  "Run-local callback that receives one transport call's timer owner.")
(defvar superchat--llm-request-context nil
  "Runtime identity and composition of the request being generated.
Bound by Runtime around one `superchat--llm-generate-answer' call and captured
lexically at entry, like `superchat--llm-runtime-observer', because a retry
continuation runs after that dynamic binding unwinds.  It carries
`:session-id', `:run-id', `:turn-id' and `:message-items' -- the ordered
provider-visible message list as Tape composition -- which the request
evidence recorder needs.  nil means no Runtime is present.")
(defvar superchat--llm-drain-steers-function nil
  "Run-local callback returning and clearing queued steer texts, or nil.
Bound by Runtime around one `superchat--llm-generate-answer' call; nil means
no Runtime is present, so the steer list is always empty.")
(defvar superchat--llm-pending-steer-count-function nil
  "Run-local callback returning the queued-steer count without consuming it.
Bound by Runtime the same way as `superchat--llm-drain-steers-function'; nil
means 0.")
(defvar superchat--llm-ollama-response-p nil
  "Non-nil while starting a Superchat request through an Ollama provider.")
(defvar superchat-llm--distribution-adapters nil
  "Optional distribution callbacks installed by `superchat-llm-configure'.")

(defconst superchat-llm-provider-attempt-reasons
  '(initial tool-protocol-retry transient-retry tool-followup final-answer)
  "Provider attempt reasons accepted by the LLM transport.")

(defun superchat-llm-provider-attempt-reason-p (reason)
  "Return non-nil when REASON names a provider attempt kind."
  (memq reason superchat-llm-provider-attempt-reasons))

(defconst superchat-llm--adapter-keys
  '(:response-mode-function
    :response-mode-indicator-function
    :adjusted-timeout-function
    :stage-function
    :ttft-function
    :nonpartial-latency-function
    :activity-function)
  "Recognized keys for the distribution adapter port.")

(cl-defstruct
    (superchat-llm--transport-timer-owner
     (:constructor superchat-llm--make-transport-timer-owner))
  "Opaque ownership token for one transport call's timeout timers."
  buffer run-id
  publication-order
  wait-timer
  hard-timer
  ;; Only explicitly configured input Runs may freeze the common hard clock.
  input-clocks-p
  (hard-state 'disabled)
  hard-deadline hard-remaining hard-function
  clock-epoch input-handle wait-generation)

(cl-defstruct
    (superchat-llm--pending-pause
     (:constructor superchat-llm--make-pending-pause))
  "Identity-bound continuation captured at one Run's pause checkpoint."
  run-id
  transport-owner
  reason
  continuation)

(defun superchat-llm--next-transport-timer-owner-order (buffer)
  "Return the next transport timer publication order for BUFFER."
  (with-current-buffer buffer
    (setq-local superchat--transport-timer-owner-order
                (1+ superchat--transport-timer-owner-order))))

(defun superchat-llm--transport-timer-owner-publish-wait (owner timer)
  "Publish OWNER's TIMER unless a newer owner exists in the View.
The View's allocation order is a high-water mark: clearing the public slot
does not make an older owner eligible to publish again."
  (when (and (superchat-llm--transport-timer-owner-p owner)
             (timerp timer))
    (let ((buffer
           (superchat-llm--transport-timer-owner-buffer owner)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let* ((published-owner superchat--active-timeout-timer-owner)
                 (published-order
                  (and (superchat-llm--transport-timer-owner-p
                        published-owner)
                       (superchat-llm--transport-timer-owner-publication-order
                        published-owner)))
                 (owner-order
                  (superchat-llm--transport-timer-owner-publication-order
                   owner))
                 (latest-owner-order
                  superchat--transport-timer-owner-order))
            (when (and (numberp owner-order)
                       (= owner-order latest-owner-order)
                       (or (not (numberp published-order))
                           (<= published-order owner-order)))
              (setq-local superchat--active-timeout-timer timer)
              (setq-local superchat--active-timeout-timer-owner owner)
              t)))))))

(defun superchat-llm--transport-timer-owner-cancel-wait (owner)
  "Cancel OWNER's inactivity wait timer and compare-clear its publication."
  (when (superchat-llm--transport-timer-owner-p owner)
    (let ((timer
           (superchat-llm--transport-timer-owner-wait-timer owner))
          (buffer
           (superchat-llm--transport-timer-owner-buffer owner)))
      ;; Retire this acquisition before cancellation can reenter.  Never
      ;; clear a newer attempt's wait after returning from user code.
      (setf (superchat-llm--transport-timer-owner-wait-timer owner) nil)
      (when (timerp timer)
        (cancel-timer timer))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (eq superchat--active-timeout-timer timer)
                     (eq superchat--active-timeout-timer-owner owner))
            (setq superchat--active-timeout-timer nil
                  superchat--active-timeout-timer-owner nil)))))))

(defun superchat-llm--transport-timer-owner-cancel (owner)
  "Cancel both timeout timers held by transport OWNER, idempotently."
  (when (superchat-llm--transport-timer-owner-p owner)
    (setf (superchat-llm--transport-timer-owner-hard-state owner) 'closed
          (superchat-llm--transport-timer-owner-clock-epoch owner) nil
          (superchat-llm--transport-timer-owner-hard-deadline owner) nil
          (superchat-llm--transport-timer-owner-hard-function owner) nil
          (superchat-llm--transport-timer-owner-input-handle owner) nil)
    (superchat-llm--transport-timer-owner-cancel-wait owner)
    (let ((timer
           (superchat-llm--transport-timer-owner-hard-timer owner)))
      (when (timerp timer)
        (cancel-timer timer))
      (setf (superchat-llm--transport-timer-owner-hard-timer owner) nil))))

(defun superchat-llm--input-arm-hard (owner seconds function &optional epoch)
  "Arm OWNER with SECONDS remaining, publishing identity before activation."
  (let ((timer (timer-create))
        (epoch (or epoch (make-symbol "input-clock"))))
    (setf (superchat-llm--transport-timer-owner-clock-epoch owner) epoch
          (superchat-llm--transport-timer-owner-hard-state owner) 'running
          (superchat-llm--transport-timer-owner-hard-remaining owner) seconds
          (superchat-llm--transport-timer-owner-hard-deadline owner) (+ (float-time) seconds)
          (superchat-llm--transport-timer-owner-hard-function owner) function
          (superchat-llm--transport-timer-owner-hard-timer owner) timer)
    (timer-set-time timer (seconds-to-time
                           (superchat-llm--transport-timer-owner-hard-deadline owner)))
    (timer-set-function
     timer (lambda ()
             (when (and (eq epoch (superchat-llm--transport-timer-owner-clock-epoch owner))
                        (eq timer (superchat-llm--transport-timer-owner-hard-timer owner))
                        (eq 'running (superchat-llm--transport-timer-owner-hard-state owner)))
               (setf (superchat-llm--transport-timer-owner-hard-state owner) 'exhausted
                     (superchat-llm--transport-timer-owner-hard-remaining owner) 0)
               (funcall function))))
    (unwind-protect
        (timer-activate timer)
      ;; Activation itself may reenter before actually enqueueing TIMER.
      ;; Cancel this stale acquisition, never the newer owner's timer slot.
      (unless (and (eq epoch (superchat-llm--transport-timer-owner-clock-epoch owner))
                   (eq timer (superchat-llm--transport-timer-owner-hard-timer owner))
                   (eq 'running (superchat-llm--transport-timer-owner-hard-state owner)))
        (cancel-timer timer)))
    epoch))

(defun superchat-llm--input-pause-clock (owner handle)
  "Pause OWNER for newly accepted exact HANDLE; return its clock state.
The caller must have published and validated HANDLE as its current question."
  (when (and (superchat-llm--transport-timer-owner-p owner)
             (superchat-llm--transport-timer-owner-input-clocks-p owner))
    (unless (or (eq 'closed (superchat-llm--transport-timer-owner-hard-state owner))
                (eq handle (superchat-llm--transport-timer-owner-input-handle owner)))
      (setf (superchat-llm--transport-timer-owner-input-handle owner) handle)
      (superchat-llm--input-freeze-clock owner))
    (superchat-llm--transport-timer-owner-hard-state owner)))

(defun superchat-llm--input-freeze-clock (owner)
  "Freeze OWNER's current acquisition, charging its elapsed armed interval."
  (let* ((state (superchat-llm--transport-timer-owner-hard-state owner))
         (timer (superchat-llm--transport-timer-owner-hard-timer owner))
         (remaining
          (if (eq state 'running)
              (max 0 (min (superchat-llm--transport-timer-owner-hard-remaining owner)
                          (- (superchat-llm--transport-timer-owner-hard-deadline owner)
                             (float-time))))
            (superchat-llm--transport-timer-owner-hard-remaining owner))))
    (unless (eq state 'closed)
      (setf (superchat-llm--transport-timer-owner-clock-epoch owner) (make-symbol "input-paused")
            (superchat-llm--transport-timer-owner-hard-remaining owner) remaining
            (superchat-llm--transport-timer-owner-hard-deadline owner) nil
            (superchat-llm--transport-timer-owner-hard-timer owner) nil
            (superchat-llm--transport-timer-owner-hard-state owner)
            (cond ((eq state 'disabled) 'disabled)
                  ((or (eq state 'exhausted) (and remaining (<= remaining 0))) 'exhausted)
                  (t 'paused)))
      (superchat-llm--transport-timer-owner-cancel-wait owner)
      (when (timerp timer) (cancel-timer timer)))))

(defun superchat-llm--transport-timer-owner-extend-wait (owner seconds)
  "Move OWNER's inactivity wait timer later by SECONDS.
The timer is cancelled and reactivated so its position in `timer-list'
matches the new deadline.  OWNER's hard timer is never consulted."
  (when (superchat-llm--transport-timer-owner-p owner)
    (let ((timer
           (superchat-llm--transport-timer-owner-wait-timer owner)))
      (when (timerp timer)
        (let ((new-trigger-time
               (time-add (timer--time timer) (seconds-to-time seconds))))
          (cancel-timer timer)
          (timer-set-time timer new-trigger-time)
          (timer-activate timer)
          t)))))
;; A person deciding is not a stalled model.  While an approval question is
;; open the attempt's inactivity timer leaves the queue; after the decision
;; the same timer continues from what remained.  The `ask_user' protocol
;; already stops this timer (`superchat-llm--input-freeze-clock'); these two
;; are the same fact for the approval wait, which has no input handle.  See
;; docs/decisions/accepted/2026-09-18-approval-wait-pauses-the-inactivity-clock.md.
(defun superchat-llm--transport-timer-owner-suspend-wait (owner)
  "Take OWNER's inactivity wait timer out of the queue; return a resume token.
Return nil when OWNER has no running wait, including a wait that an outer
suspension already stopped.  OWNER's timer slot and hard timer are untouched."
  (when (superchat-llm--transport-timer-owner-p owner)
    (let ((timer (superchat-llm--transport-timer-owner-wait-timer owner)))
      (when (and (timerp timer) (memq timer timer-list))
        (let ((remaining
               (max 0 (float-time
                       (time-subtract (timer--time timer) (current-time))))))
          (cancel-timer timer)
          (cons timer remaining))))))

(defun superchat-llm--transport-timer-owner-resume-wait (owner token)
  "Continue the wait TOKEN stopped, with the time it had left.
Only while OWNER's slot still holds that very timer: a Run that was
cancelled, an attempt that ended or a newer attempt has emptied or replaced
the slot, and a late answer must not bring anything back."
  (when (and (superchat-llm--transport-timer-owner-p owner) (consp token))
    (let ((timer (car token)))
      (when (and (timerp timer)
                 (eq timer (superchat-llm--transport-timer-owner-wait-timer owner))
                 (not (memq timer timer-list)))
        (timer-set-time timer (time-add (current-time) (seconds-to-time (cdr token))))
        (timer-activate timer)
        t))))
(declare-function superchat--response-region-close
                  "superchat-render" (content &optional final-p transcript-owner))
(declare-function llm-ollama-p "llm-ollama" (object))
(declare-function superchat-get-llm-tools "superchat-tools" (&optional tool-names input-operation))
(declare-function superchat-event-record-fault "superchat-event" (site id owner condition))
(defvar superchat-event-faults)
(declare-function superchat-runtime--trusted-input-operation-p
                  "superchat-runtime" (context function async))
(declare-function superchat-runtime-status "superchat-runtime" (run-id))
(declare-function superchat-tools--trusted-ask-descriptor-p
                  "superchat-tools" (tool operation))
(declare-function superchat-tools--make-ask-user "superchat-tools" (operation))
(declare-function llm-capabilities "llm" (provider))
(declare-function llm-chat-async "llm"
                  (provider prompt response-callback error-callback
                            &optional multi-output))
(declare-function llm-chat-streaming "llm"
                  (provider prompt partial-callback response-callback
                            error-callback &optional multi-output))
(declare-function llm-chat-prompt-tools "llm" (prompt))
(declare-function llm-chat-prompt-p "llm" (prompt))
(declare-function llm-chat-prompt-interactions "llm" (prompt))
(declare-function llm-chat-prompt-interaction-role "llm" (interaction))
(declare-function llm-chat-prompt-interaction-content "llm" (interaction))
(declare-function llm-chat-prompt-interaction-tool-results "llm" (interaction))
(declare-function llm-chat-prompt-interaction-multi-turn-plist "llm" (interaction))
(declare-function llm-chat-prompt-tool-result-call-id "llm" (result))
(declare-function llm-chat-prompt-tool-result-tool-name "llm" (result))
(declare-function llm-chat-prompt-tool-result-result "llm" (result))
(declare-function llm-chat-prompt-context "llm" (prompt))
(declare-function llm-chat-prompt-temperature "llm" (prompt))
(declare-function llm-chat-prompt-max-tokens "llm" (prompt))
(declare-function llm-chat-prompt-response-format "llm" (prompt))
(declare-function llm-chat-prompt-reasoning "llm" (prompt))
(declare-function llm-chat-prompt-non-standard-params "llm" (prompt))
(declare-function llm-chat-prompt-tool-options "llm" (prompt))
(declare-function llm-multipart-p "llm" (value))
(declare-function llm-multipart-parts "llm" (multipart))
(declare-function llm-chat-token-limit "llm" (provider))
(declare-function llm-openai-compatible-p "llm-openai" (object))
(declare-function llm-openai-compatible-url "llm-openai" (provider))
(declare-function llm-tool-options-tool-choice "llm" (options))
(declare-function llm-provider-chat-request "llm-provider-utils"
                  (provider prompt streaming))
(declare-function llm-provider-utils-tool-use-p "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-id "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-name "llm-provider-utils" (value))
(declare-function llm-provider-utils-tool-use-args "llm-provider-utils" (value))
(declare-function llm-provider-utils--normalize-args
                  "llm-provider-utils" (args &optional false-value))
(declare-function llm-chat-prompt-append-response "llm"
                  (prompt response &optional role))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function superchat-invariant-check "superchat-invariant"
                  (name session-id context &optional record))
(declare-function llm-provider-utils-openai-tool-spec "llm-provider-utils" (tool))
(declare-function superchat-tape-request-record "superchat-tape"
                  (session-id request-id system tools model message-items receipt
                              &optional resumed execution))
(declare-function superchat-runtime-steer "superchat-runtime"
                  (run-id text &optional origin))
(declare-function llm-tool-function "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function make-llm-tool "llm" (&rest args))
(declare-function make-llm-media "llm" (&rest args))
(declare-function llm-make-multipart "llm" (&rest parts))

(define-error 'superchat-llm-evidence-recording-error
  "Superchat could not record required decision evidence")
(define-error 'superchat-llm-tool-wrapper-required
  "Superchat ordinary tools require an Authority wrapper")

(defun superchat--llm-lm-studio-p (provider)
  "Return non-nil when PROVIDER targets LM Studio's local API."
  (and (fboundp 'llm-openai-compatible-p)
       (llm-openai-compatible-p provider)
       (fboundp 'llm-openai-compatible-url)
       (let ((url (llm-openai-compatible-url provider)))
         (and (stringp url)
              (string-match-p
               "\\`https?://\\(?:localhost\\|127\\.0\\.0\\.1\\|\\[::1\\]\\):1234/"
               url)))))

(defun superchat--llm-lm-studio-context-window (provider model)
  "Return MODEL's active LM Studio context window, or nil.
This reads the loaded instance rather than the model's trained maximum."
  (when (and (stringp model)
             (superchat--llm-lm-studio-p provider)
             (fboundp 'json-parse-buffer))
    (when-let* ((program (executable-find "lms")))
      (condition-case nil
          (with-temp-buffer
            (when (zerop (call-process program nil t nil "ps" "--json"))
              (goto-char (point-min))
              (let* ((loaded
                      (json-parse-buffer
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil))
                     (entry
                      (cl-find-if
                       (lambda (item)
                         (cl-some
                          (lambda (key)
                            (equal model (alist-get key item)))
                          '(identifier modelKey indexedModelIdentifier)))
                       loaded)))
                (when-let* ((limit (alist-get 'contextLength entry)))
                  (and (integerp limit) (> limit 0) limit)))))
        (error nil)))))

(defun superchat--llm-context-capability (provider model)
  "Return MODEL's context limit and its provenance for PROVIDER."
  (let* ((override-entry
         (and (stringp model)
               (assoc-string model superchat-llm-context-window-overrides)))
         (override-limit
          (and (consp override-entry)
               (integerp (cdr override-entry))
               (> (cdr override-entry) 0)
               (cdr override-entry)))
         (runtime-limit
          (and (not override-limit)
               (superchat--llm-lm-studio-context-window provider model)))
         (llm-limit
          (and (not override-limit)
               (not runtime-limit)
               (fboundp 'llm-chat-token-limit)
               (ignore-errors (llm-chat-token-limit provider)))))
    (cond
     (override-limit
      (list :limit override-limit :source 'override))
     ((and (integerp runtime-limit) (> runtime-limit 0))
      (list :limit runtime-limit :source 'lm-studio-runtime))
     ((and (integerp llm-limit) (> llm-limit 0))
      (list :limit llm-limit :source 'llm)))))

(defconst superchat--default-context-budget-chars 40000
  "Fallback character budget when no trusted model window is available.")

(defun superchat--llm-context-budget-chars
    (&optional system-prompt turn-prompt target-model)
  "Return the effective Context View character budget.
An explicit `superchat-context-max-chars' wins.  Otherwise derive a budget
only from an override or a live LM Studio window; llm.el's static metadata is
not trusted for this calculation.  SYSTEM-PROMPT and TURN-PROMPT are measured
as prompt characters and reserved after conversion to tokens."
  (if (integerp superchat-context-max-chars)
      superchat-context-max-chars
    (let* ((chars-per-token
            (if (and (numberp superchat-context-chars-per-token)
                     (> superchat-context-chars-per-token 0))
                superchat-context-chars-per-token
              2))
           (fixed-reserve
            (if (and (numberp superchat-context-fixed-reserve)
                     (>= superchat-context-fixed-reserve 0))
                superchat-context-fixed-reserve
              4096))
           (backend (condition-case nil
                        (superchat--effective-llm-backend target-model)
                      (error nil)))
           (model (or target-model
                      superchat-llm-model
                      (and backend
                           (fboundp 'superchat--provider-chat-model)
                           (condition-case nil
                               (superchat--provider-chat-model backend)
                             (error nil)))
                      "unknown"))
           (capability (condition-case nil
                           (superchat--llm-context-capability backend model)
                         (error nil)))
           (limit (plist-get capability :limit))
           (source (plist-get capability :source)))
      (if (and (memq source '(override lm-studio-runtime))
               (integerp limit)
               (> limit 0))
          (let* ((measured-chars
                  (+ (if (stringp system-prompt) (length system-prompt) 0)
                     (if (stringp turn-prompt) (length turn-prompt) 0)))
                 (measured-tokens
                  (ceiling (/ measured-chars (float chars-per-token))))
                 (reserve (+ fixed-reserve measured-tokens))
                 (available-tokens (max 0 (- limit reserve)))
                 (window-chars (* limit chars-per-token)))
            (max 0
                 (floor
                  (min (* available-tokens chars-per-token)
                       (/ window-chars 2.0)))))
        superchat--default-context-budget-chars))))

(defconst superchat--llm-preflight-chars-per-token 2.0
  "Calibrated request characters per token for transport preflight.")

(defconst superchat--llm-preflight-safety-factor 1.10
  "Safety margin applied to transport preflight character estimates.")

(defun superchat--llm-serialized-attempt (provider prompt streaming)
  "Return the provider-visible serialization for one PROMPT attempt.
PROVIDER's own request builder is the primary source so messages, system
instructions, tool schemas, and provider options have the same shape as the
request body.  STREAMING must match the pending provider call."
  (or
   (when (fboundp 'llm-provider-chat-request)
     (condition-case nil
         (json-serialize
          (llm-provider-chat-request
           provider (copy-tree prompt t) streaming))
       (error nil)))
   (condition-case nil
       (json-serialize
        (list :system (llm-chat-prompt-context prompt)
              :messages (superchat--llm-evidence-messages prompt)
              :tools
              (superchat--llm-evidence-tool-records
               (llm-chat-prompt-tools prompt))))
     (error nil))
   (prin1-to-string prompt)))

(defun superchat--llm-estimate-context-tokens (provider prompt)
  "Conservatively estimate provider-visible PROMPT tokens.
The estimate is based on the JSON request body produced for PROVIDER, with a
fixed calibrated character ratio and safety margin.  llm.el's generic
word-count implementation is not a provider tokenizer and is deliberately
not used.  The pending attempt's streaming shape is dynamically bound by the
transport caller."
  (let* ((serialized
          (superchat--llm-serialized-attempt
           provider prompt superchat--llm-preflight-streaming-p))
         (character-estimate
          (ceiling
           (/ (length serialized)
              superchat--llm-preflight-chars-per-token))))
    (max 1
         (ceiling
          (* character-estimate superchat--llm-preflight-safety-factor)))))

(defun superchat-llm--adaptive-response-timeout (base-timeout estimated-tokens)
  "Scale BASE-TIMEOUT for ESTIMATED-TOKENS without crossing a set hard cap.
The first 4096 estimated tokens use the configured base.  Larger serialized
prompts add one second per 256 tokens, a deliberately simple prefill allowance
whose behavior is stable across providers."
  (when base-timeout
    (let* ((tokens (max 0 (or estimated-tokens 0)))
           (extra (if (> tokens 4096)
                      (ceiling (/ (- tokens 4096) 256.0))
                    0))
           (scaled (+ base-timeout extra)))
      (if superchat-response-timeout-hard-limit
          (min scaled (max 1 superchat-response-timeout-hard-limit))
        scaled))))

(defconst superchat--llm-context-overflow-patterns
  '("prompt is too long"
    "request_too_large"
    "exceeds the context window"
    "maximum context length"
    "maximum prompt length"
    "exceeds the available context size"
    "greater than the context length"
    "input token count.*exceeds the maximum"
    "reduce the length of the messages"
    "range of input length should be"
    "context[_ ]length[_ ]exceeded"
    "token limit exceeded")
  "Provider error fragments that identify a context overflow.")

(defun superchat--llm-context-overflow-error-p (value)
  "Return non-nil when provider error VALUE denotes context overflow."
  (let ((text (downcase (format "%s" value))))
    (and (not (string-match-p "rate limit\\|too many requests" text))
         (cl-some (lambda (pattern) (string-match-p pattern text))
                  superchat--llm-context-overflow-patterns))))

(defun superchat--llm-context-overflow-response-p
    (response context-limit &optional prior-output-p)
  "Return non-nil when RESPONSE indicates silent context overflow.
CONTEXT-LIMIT is the best known window for the current provider attempt.
PRIOR-OUTPUT-P is non-nil when streaming already produced visible text."
  (when (consp response)
    (let ((stop-reason (plist-get response :stop-reason))
          (input (plist-get response :input-tokens))
          (output (plist-get response :output-tokens))
          (text (plist-get response :text)))
      (or
       (and (eq stop-reason 'stop)
            (numberp context-limit)
            (numberp input)
            (> input context-limit))
       (and (eq stop-reason 'length)
            (numberp context-limit)
            (numberp input)
            (numberp output)
            (zerop output)
            (>= input (* context-limit 0.99)))
       ;; Some compatible providers omit usage on this failure shape.
       (and (eq stop-reason 'length)
            (not prior-output-p)
            (not (numberp input))
            (string-empty-p (or text "")))))))

(defun superchat--llm-replace-context-text (text old-context new-context)
  "Replace OLD-CONTEXT once in TEXT with NEW-CONTEXT, or return nil.
Uses a literal `string-search', not a regexp: a pattern `regexp-quote'
compiles from a large OLD-CONTEXT can exceed Emacs's regexp engine size
limit (empirically, somewhere between 30,000 and 50,000 characters,
signalling `(invalid-regexp \"Regular expression too big\")'), and this
replacement must work on the arbitrarily long raw history compaction
exists to summarize."
  (when (and (stringp text)
             (stringp old-context)
             (not (string-empty-p old-context)))
    (when-let* ((start (string-search old-context text)))
      (concat (substring text 0 start)
              new-context
              (substring text (+ start (length old-context)))))))

(defun superchat--llm-replace-prompt-context
    (prompt old-context new-context)
  "Replace OLD-CONTEXT in PROMPT's initial input with NEW-CONTEXT.
Return non-nil when the provider prompt was rewritten."
  (catch 'replaced
    (dolist (interaction (llm-chat-prompt-interactions prompt))
      (let ((content (llm-chat-prompt-interaction-content interaction)))
        (cond
         ((stringp content)
          (when-let* ((replacement
                       (superchat--llm-replace-context-text
                        content old-context new-context)))
            (setf (cl-struct-slot-value
                   'llm-chat-prompt-interaction 'content interaction)
                  replacement)
            (throw 'replaced t)))
         ((and (fboundp 'llm-multipart-p)
               (llm-multipart-p content)
               (fboundp 'llm-multipart-parts))
          (let ((parts (copy-sequence (llm-multipart-parts content))))
            (cl-loop for part in parts
                     for index from 0
                     for replacement =
                     (superchat--llm-replace-context-text
                      part old-context new-context)
                     when replacement
                     do (progn
                          (setf (nth index parts) replacement)
                          (setf (cl-struct-slot-value
                                 'llm-multipart 'parts content)
                                parts)
                          (throw 'replaced t))))))))
    nil))

(defun superchat--llm-replace-prompt-input (prompt old-input new-input)
  "Replace PROMPT's initial user input OLD-INPUT with NEW-INPUT once.

Prefer an exact initial-user replacement so a retry can also rewrite an empty
input.  Multipart input and assembled inputs which only contain OLD-INPUT use
the established context replacement fallback.  This mutates only the private
provider prompt retained by the active LLM invocation."
  (or
   (catch 'replaced
     (dolist (interaction (llm-chat-prompt-interactions prompt))
       (when (and (eq (llm-chat-prompt-interaction-role interaction) 'user)
                  (equal (llm-chat-prompt-interaction-content interaction)
                         old-input))
         (setf (cl-struct-slot-value
                'llm-chat-prompt-interaction 'content interaction)
               new-input)
         (throw 'replaced t)))
     nil)
   (superchat--llm-replace-prompt-context prompt old-input new-input)))

(defun superchat--llm-retry-before-provider-request
    (runtime-observer real-prompt prompt system-prompt)
  "Apply the Runtime's retry `before-provider-request' fold once.

The initial attempt is folded by Runtime before it enters this adapter.  LLM
retries occur after that dynamic call has unwound, so their captured
RUNTIME-OBSERVER is the only route back to the accepted Run's borrowed Scope.
Return the next (PROMPT SYSTEM-PROMPT) pair after mutating only REAL-PROMPT.
An Event result that changes input but cannot be reflected in the mutable
provider prompt is an error rather than a silently ignored policy rewrite.

A request waterfall MIDDLEWARE may also refuse the attempt outright.  That is
a decision, not an abstention and not a rewrite: this retry must not reach the
provider, so the refusal is signalled as an error for the caller's existing
attempt-failure path to settle.  Treating the refusal as a spec would send the
refused request anyway, which is the one outcome the refusal exists to stop."
  (if (not (functionp runtime-observer))
      (list prompt system-prompt
            (mapcar #'llm-tool-name (llm-chat-prompt-tools real-prompt)))
    (let* ((spec
            (funcall runtime-observer 'before-provider-request
                     (list :prompt prompt :system system-prompt
                           :tools (mapcar #'llm-tool-name
                                          (llm-chat-prompt-tools real-prompt)))))
           (_refusal-check
            (when (superchat-waterfall-refused-p spec)
              (error "Provider request refused by the request waterfall: %s"
                     (or (superchat-waterfall-refusal-reason spec)
                         "no reason given"))))
           (next-prompt
            (if (plist-member spec :prompt) (plist-get spec :prompt) prompt))
           (next-system
            (if (plist-member spec :system) (plist-get spec :system) system-prompt)))
      (unless (equal next-prompt prompt)
        (unless (superchat--llm-replace-prompt-input
                 real-prompt prompt next-prompt)
          (error "before-provider-request could not update retry prompt")))
      (unless (equal next-system system-prompt)
        ;; Resolve the llm struct slot only at run time: this file loads before
        ;; optional llm.el in the canonical package loader.
        (setf (cl-struct-slot-value 'llm-chat-prompt 'context real-prompt)
              next-system))
      (list next-prompt next-system (plist-get spec :tools)))))

(defun superchat--llm-canonical-evidence-value (value)
  "Return VALUE with llm.el false sentinels canonicalized for exact JSON."
  (cond
   ((eq value :false) :json-false)
   ((consp value)
    (cons (superchat--llm-canonical-evidence-value (car value))
          (superchat--llm-canonical-evidence-value (cdr value))))
   ((vectorp value)
    (vconcat (mapcar #'superchat--llm-canonical-evidence-value
                     (append value nil))))
   (t value)))

(defun superchat--llm-set-tool-use-args (use value)
  "Set USE's arguments without expanding an optional llm accessor early.

`superchat-llm.el' is loaded before optional llm.el in the canonical test
runner.  Calling the generated accessor's `setf' form directly would then
leave an unresolved `(setf ...)' function in the interpreted definition.
The struct slot API resolves the concrete llm object at call time."
  (setf (cl-struct-slot-value 'llm-provider-utils-tool-use 'args use) value)
  value)

(defun superchat--llm-dispatch-input-batch (prompt input-tool dispatch &optional prepare)
  "Register PROMPT immediately before DISPATCH under INPUT-TOOL's batch rule.
Native entry may hand off callbacks even when it throws.  Keep its policy;
only a pre-entry failure compare-restores the previous exact association.
Optional PREPARE runs after registration but before native entry.  Nil means
stop without dispatch; a condition propagates after registration rollback."
  (if (null input-tool)
      (funcall dispatch)
    (let ((previous (gethash prompt superchat--llm-input-batches))
          (policy (substring-no-properties input-tool)) entered)
      (unwind-protect
          (progn
            (puthash prompt policy superchat--llm-input-batches)
            ;; PREPARE may stop or throw before native entry.  In that case
            ;; the existing exact-association rollback must still run.
            (when (or (null prepare) (funcall prepare))
              (setq entered t)
              (funcall dispatch)))
        (when (and (not entered)
                   (eq policy (gethash prompt superchat--llm-input-batches)))
          (if previous
              (puthash prompt previous superchat--llm-input-batches)
            (remhash prompt superchat--llm-input-batches)))))))

(defun superchat--llm-capture-provider-tool-uses
    (original provider prompt tool-uses)
  "Call ORIGINAL after preserving PROMPT's raw provider TOOL-USES."
  (when-let* ((capture (and (hash-table-p superchat--llm-raw-tool-uses)
                            (gethash prompt superchat--llm-raw-tool-uses))))
    (let ((raw (copy-tree tool-uses t)))
      (dolist (use raw)
        (when (llm-provider-utils-tool-use-p use)
          (superchat--llm-set-tool-use-args
           use
           (superchat--llm-canonical-evidence-value
            (llm-provider-utils-tool-use-args use)))))
      (setcar capture raw))
    (remhash prompt superchat--llm-raw-tool-uses))
  (when-let* ((input-tool (gethash prompt superchat--llm-input-batches)))
    (let ((names (mapcar #'llm-provider-utils-tool-use-name tool-uses)))
      (when (and (member input-tool names) (not (equal names (list input-tool))))
        ;; The native scheduler has not entered any Authority wrapper yet.
        ;; llm-error follows the existing fatal path, not tool-protocol retry.
        (signal 'llm-error '("Required input must be the only tool in its batch")))))
  (funcall original provider prompt tool-uses))

(with-eval-after-load 'llm-provider-utils
  (unless (advice-member-p #'superchat--llm-capture-provider-tool-uses
                           'llm-provider-populate-tool-uses)
    (advice-add 'llm-provider-populate-tool-uses :around
                #'superchat--llm-capture-provider-tool-uses)))

(defun superchat--llm-require-accessors (names label)
  "Require every function in NAMES for exact evidence named LABEL."
  (dolist (name names)
    (unless (fboundp name)
      (error "llm.el %s accessor is unavailable: %s" label name))))

(defun superchat--llm-evidence-tool-use (use)
  "Return one llm.el tool USE as structured evidence."
  (superchat--llm-require-accessors
   '(llm-provider-utils-tool-use-p
     llm-provider-utils-tool-use-id
     llm-provider-utils-tool-use-name
     llm-provider-utils-tool-use-args)
   "tool-use")
  (unless (llm-provider-utils-tool-use-p use)
    (error "Unsupported llm.el interaction content: %S" (type-of use)))
  (list :call-id (llm-provider-utils-tool-use-id use)
        :tool-name (llm-provider-utils-tool-use-name use)
        :arguments
        (superchat--llm-canonical-evidence-value
         (copy-tree (llm-provider-utils-tool-use-args use) t))))

(defun superchat--llm-evidence-tool-result (result)
  "Return one llm.el tool RESULT as structured evidence."
  (list :call-id (llm-chat-prompt-tool-result-call-id result)
        :tool-name (llm-chat-prompt-tool-result-tool-name result)
        :result (copy-tree
                 (llm-chat-prompt-tool-result-result result) t)))

(defun superchat--llm-evidence-content (content)
  "Return llm.el interaction CONTENT and its structural format."
  (cond
   ((stringp content) (cons "text" content))
   ((null content) (cons "empty" nil))
   ((listp content)
    (cons "tool-uses"
          (vconcat (mapcar #'superchat--llm-evidence-tool-use content))))
   (t (error "Unsupported llm.el interaction content: %S"
             (type-of content)))))

(defun superchat--llm-evidence-messages (real-prompt)
  "Return every provider-visible interaction slot from REAL-PROMPT."
  (superchat--llm-require-accessors
   '(llm-chat-prompt-interactions
     llm-chat-prompt-interaction-role
     llm-chat-prompt-interaction-content
     llm-chat-prompt-interaction-tool-results
     llm-chat-prompt-interaction-multi-turn-plist
     llm-chat-prompt-tool-result-call-id
     llm-chat-prompt-tool-result-tool-name
     llm-chat-prompt-tool-result-result)
   "prepared-message")
  (vconcat
   (mapcar
    (lambda (interaction)
      (let* ((content
              (superchat--llm-evidence-content
               (llm-chat-prompt-interaction-content interaction)))
             (results
              (llm-chat-prompt-interaction-tool-results interaction)))
        (list
         :role (format "%s"
                       (llm-chat-prompt-interaction-role interaction))
         :content-format (car content)
         :content (cdr content)
         :tool-results
         (vconcat (mapcar #'superchat--llm-evidence-tool-result results))
         :multi-turn
         (copy-tree
          (llm-chat-prompt-interaction-multi-turn-plist interaction) t))))
    (llm-chat-prompt-interactions real-prompt))))

(defun superchat--llm-record-request-evidence (context prompt &optional
                                                       system-override)
  "Record CONTEXT's request and PROMPT's provider-visible form on the Tape.
CONTEXT is the Runtime-supplied `superchat--llm-request-context'.  The system
text and the tool schemas are read off PROMPT, so what is recorded is what the
provider layer is about to send rather than a second derivation of it.  Tool
schemas are recorded as the provider-shaped function specs when llm.el can
produce them, so the Tape states the descriptions and parameters the model was
offered even after the registry behind them changes.

SYSTEM-OVERRIDE, when a string, is recorded instead of reading PROMPT's own
`:context'.  The caller must supply the system text it is really sending,
because PROMPT is not a stable statement of it: llm.el's
`llm-provider-chat-request' folds `:context' into a system interaction and
clears the slot (`llm-provider-utils-combine-to-system-prompt'), so every
attempt after the first would otherwise read an empty system text and record a
spurious `request.header' whose only change is that erasure.  The caller's own
provider-facing system text is threaded through the same retry fold, so it
stays the value the provider receives.

Returns the header storage id, or nil when there is no Runtime context, no
Session, or the Tape refuses the append -- evidence recording never breaks the
request it describes."
  (when (and (listp context) (plist-get context :session-id))
    (condition-case nil
        (let* ((system (or system-override
                           (and (fboundp 'llm-chat-prompt-context)
                                (or (llm-chat-prompt-context prompt) ""))))
               (tools (and (fboundp 'llm-chat-prompt-tools)
                           (llm-chat-prompt-tools prompt)))
               (specs (and tools
                           (fboundp 'llm-provider-utils-openai-tool-spec)
                           (condition-case nil
                               (vconcat (mapcar
                                         #'llm-provider-utils-openai-tool-spec
                                         tools))
                             (error nil)))))
          (let ((header-id
                 (superchat-tape-request-record
                  (plist-get context :session-id)
                  (plist-get context :request-id)
                  system specs
                  (superchat--provider-chat-model superchat-llm-backend)
                  (or (plist-get context :message-items) [])
                  (plist-get context :message-receipt) nil
                  (list :run-id (plist-get context :run-id)
                        :turn-id (plist-get context :turn-id)
                        :step (plist-get context :step)))))
            ;; The invariant runs after both rows exist, so it rebuilds the
            ;; request instead of checking that a receipt was written.
            (when header-id
              ;; `superchat-invariant-check' is (NAME SESSION-ID RUN-ID &rest ARGS),
              ;; so the evidence plist is the first check argument.
              (superchat-invariant-check
               'prompt-reconstructable
               (plist-get context :session-id)
               nil
               (list :session-id (plist-get context :session-id)
                     :request-id (plist-get context :request-id))))
            header-id))
      (error nil))))

(defun superchat--llm-evidence-tool-records (tools)
  "Return TOOLS' provider-visible schemas as structured evidence."
  (superchat--llm-require-accessors
   '(llm-tool-name llm-tool-description llm-tool-args llm-tool-async)
   "prepared-tool")
  (vconcat
   (mapcar
    (lambda (tool)
      (list :name (llm-tool-name tool)
            :description (llm-tool-description tool)
            :args (vconcat (copy-tree (llm-tool-args tool) t))
            :async (and (llm-tool-async tool) t)))
    tools)))

(defun superchat--llm-validate-evidence-tools (tools)
  "Reject a tool surface that cannot carry exact attempt provenance."
  (condition-case nil
      (superchat--llm-require-accessors
       '(llm-tool-name llm-tool-description llm-tool-args llm-tool-function
         llm-tool-async make-llm-tool llm-provider-utils--normalize-args
         llm-provider-populate-tool-uses
         llm-provider-utils-tool-use-p llm-provider-utils-tool-use-id
         llm-provider-utils-tool-use-name llm-provider-utils-tool-use-args)
       "evidence-tool")
    (error
     (signal 'superchat-llm-evidence-recording-error
             '("Required llm.el evidence-tool accessor is unavailable"))))
  (unless (listp tools)
    (signal 'superchat-llm-evidence-recording-error
            '("epistemic-learning-v0 requires a tool list")))
  (let ((tool-names (make-hash-table :test #'equal)))
    (dolist (tool tools)
      (let ((name (llm-tool-name tool))
            (args (llm-tool-args tool)))
        (unless (and (stringp name) (not (string-empty-p name)))
          (signal 'superchat-llm-evidence-recording-error
                  '("epistemic-learning-v0 requires a named tool")))
        (when (gethash name tool-names)
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Duplicate evidence tool name: %s" name))))
        (puthash name t tool-names)
        (unless (functionp (llm-tool-function tool))
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Evidence tool is not callable: %s" name))))
        (unless (listp args)
          (signal 'superchat-llm-evidence-recording-error
                  (list (format "Evidence tool schema is invalid: %s" name))))
        (let ((arg-names (make-hash-table :test #'equal)))
          (dolist (spec args)
            (let ((arg-name (and (listp spec) (plist-get spec :name))))
              (unless (and (stringp arg-name)
                           (not (string-empty-p arg-name))
                           (not (gethash arg-name arg-names))
                           (memq (plist-get spec :type)
                                 '(string integer number boolean array)))
                (signal 'superchat-llm-evidence-recording-error
                        (list (format "Evidence tool argument is invalid: %s"
                                      name))))
              (puthash arg-name t arg-names))))
        (when (or (equal name "define_tool")
                  (and (fboundp 'superchat--syn-tool-p)
                       (superchat--syn-tool-p name))
                  (llm-tool-async tool))
          (signal
           'superchat-llm-evidence-recording-error
           (list
            (format "epistemic-learning-v0 excludes dynamic or async tool: %s"
                    name))))))))

(defun superchat--llm-evidence-options (prompt streaming multi-output)
  "Return actual Superchat-prepared options from PROMPT."
  (superchat--llm-require-accessors
   '(llm-chat-prompt-temperature llm-chat-prompt-max-tokens
     llm-chat-prompt-response-format llm-chat-prompt-reasoning
     llm-chat-prompt-non-standard-params llm-chat-prompt-tool-options
     llm-tool-options-tool-choice)
   "prepared-option")
  (let* ((tool-options (llm-chat-prompt-tool-options prompt))
         (tool-choice
          (and tool-options (llm-tool-options-tool-choice tool-options))))
    (list :temperature (llm-chat-prompt-temperature prompt)
          :max-tokens (llm-chat-prompt-max-tokens prompt)
          :response-format
          (copy-tree (llm-chat-prompt-response-format prompt) t)
          :reasoning (llm-chat-prompt-reasoning prompt)
          :non-standard-params
          (copy-tree (llm-chat-prompt-non-standard-params prompt) t)
          :tool-choice
          (cond ((null tool-choice) nil)
                ((symbolp tool-choice) (symbol-name tool-choice))
                (t tool-choice))
          :streaming (and streaming t)
          :multi-output (and multi-output t))))

(defun superchat--llm-set-prompt-tools (prompt tools)
  "Set PROMPT's TOOLS after llm.el is loaded."
  (setf (cl-struct-slot-value 'llm-chat-prompt 'tools prompt) tools))

(defun superchat--llm-tool-use-call-args (use tool)
  "Return (t . POSITIONAL-ARGS) when USE is valid for TOOL."
  (condition-case nil
      (let* ((arguments
              (copy-tree (llm-provider-utils-tool-use-args use) t))
             (schema (llm-tool-args tool))
             (valid (listp arguments))
             positional)
        (dolist (spec schema)
          (let* ((name (plist-get spec :name))
                 (key (and (stringp name) (intern name)))
                 (pair (and key (assq key arguments)))
                 (value (and pair (cdr pair)))
                 (type (plist-get spec :type)))
            (cond
             ((and pair
                   (pcase type
                     ('string (stringp value))
                     ('integer (integerp value))
                     ('number (numberp value))
                     ('boolean (eq value t))))
              (push value positional))
             ((and pair (eq type 'boolean)
                   (memq value '(:false :json-false)))
              (push nil positional))
             (pair (setq valid nil))
             ((plist-get spec :optional) (push nil positional))
             (t (setq valid nil)))))
        (when valid
          (dolist (argument arguments)
            (unless
                (and
                 (consp argument)
                 (cl-some
                  (lambda (spec)
                    (let ((name (plist-get spec :name)))
                      (and (stringp name)
                           (eq (intern name) (car argument)))))
                  schema))
              (setq valid nil))))
        (when valid
          (cons t (nreverse positional))))
    (error nil)))

(defun superchat--llm-attempt-call-origin
    (prompt tool received-args consumed base-origin &optional raw-uses)
  "Return the unused PROMPT tool use matching TOOL and RECEIVED-ARGS."
  (let* ((interactions (llm-chat-prompt-interactions prompt))
         (interaction (car (last interactions)))
         (uses (or (car-safe raw-uses)
                   (and interaction
                        (llm-chat-prompt-interaction-content interaction))))
         (expected-name (llm-tool-name tool))
         (match
          (and
           (listp uses)
           (cl-loop
            for use in uses
            for ordinal from 0
            for mapped =
            (and (llm-provider-utils-tool-use-p use)
                 (equal expected-name
                        (llm-provider-utils-tool-use-name use))
                 (superchat--llm-tool-use-call-args use tool))
            when (and mapped
                      (not (memq ordinal (car consumed)))
                      (equal received-args (cdr mapped)))
            return (cons ordinal use)))))
    (unless (and (stringp expected-name)
                 (not (string-empty-p expected-name))
                 match)
      (error "Tool call does not match its provider attempt"))
    (let* ((ordinal (car match))
           (use (cdr match))
           (call-id (llm-provider-utils-tool-use-id use)))
      (unless (and (stringp call-id)
                   (not (string-empty-p call-id))
                   (= 1
                      (cl-count
                       call-id uses :test #'equal
                       :key
                       (lambda (candidate)
                         (and (llm-provider-utils-tool-use-p candidate)
                              (llm-provider-utils-tool-use-id candidate))))))
        (error "Tool call ID is missing or duplicated"))
      (push ordinal (car consumed))
      (append
       (copy-tree base-origin t)
       (list :tool-call-id call-id
             :tool-call-name (llm-provider-utils-tool-use-name use)
             :tool-call-ordinal (1+ ordinal)
             :tool-call-arguments
             (copy-tree (llm-provider-utils-tool-use-args use) t))))))

(defun superchat--llm-restore-provider-tool-use (prompt origin)
  "Restore ORIGIN's arguments in PROMPT after llm.el normalization."
  (let* ((interactions (llm-chat-prompt-interactions prompt))
         (interaction (car (last interactions)))
         (uses (and interaction
                    (llm-chat-prompt-interaction-content interaction)))
         (use (and (listp uses)
                   (nth (1- (plist-get origin :tool-call-ordinal)) uses))))
    (unless (and (llm-provider-utils-tool-use-p use)
                 (equal (plist-get origin :tool-call-id)
                        (llm-provider-utils-tool-use-id use))
                 (equal (plist-get origin :tool-call-name)
                        (llm-provider-utils-tool-use-name use)))
      (error "Provider tool-use history diverged before execution"))
    (superchat--llm-set-tool-use-args
     use
     (llm-provider-utils--normalize-args
      (copy-tree (plist-get origin :tool-call-arguments) t)
      :false))))

(defun superchat--llm-attempt-tools (prompt tools base-origin execution-scope)
  "Clone TOOLS for PROMPT with BASE-ORIGIN and EXECUTION-SCOPE.

EXECUTION-SCOPE is captured when the Runtime enters this LLM request.  A
provider may invoke these functions after that dynamic call, including on a
retry, so it must not be re-resolved from whichever callback happens to be
current then."
  (let ((consumed (cons nil nil))
        (raw-uses (cons nil nil)))
    (puthash prompt raw-uses superchat--llm-raw-tool-uses)
    (mapcar
     (lambda (tool)
       (let ((name (llm-tool-name tool))
             (function (llm-tool-function tool))
             (async (llm-tool-async tool)))
         (make-llm-tool
          :name name
          :description (llm-tool-description tool)
          :args (copy-tree (llm-tool-args tool) t)
          :async async
          :function
          (lambda (&rest args)
            (let ((superchat-execution-scope execution-scope))
              (let* ((origin
                      (condition-case _err
                          (let ((origin
                                 (superchat--llm-attempt-call-origin
                                  prompt tool args consumed base-origin raw-uses)))
                            (superchat--llm-restore-provider-tool-use prompt origin)
                            origin)
                        (error
                         (when-let* ((observer
                                      (plist-get
                                       base-origin :audit-failure-observer)))
                           (ignore-errors
                             (funcall
                              observer
                              (list
                               :attempt
                               (plist-get base-origin :evidence-attempt)
                               :kind "tool-origin-invalid"
                               :tool-name name))))
                         nil)))
                     (active-p (plist-get base-origin :attempt-active-p))
                     (result
                      (if origin
                          "[Tool call blocked: stale provider attempt]"
                        "[Tool call blocked because exact audit failed]")))
                (if (and origin (functionp active-p) (funcall active-p))
                    (let ((superchat--llm-tool-origin origin))
                      (apply function args))
                  (progn
                    (when-let* ((observer
                                 (and origin
                                      (plist-get
                                       base-origin :stale-observer))))
                      (funcall observer origin result))
                    (if async
                        (progn
                          (when (functionp (car args))
                            (funcall (car args) result))
                          nil)
                      result)))))))))
     tools)))

(defun superchat--llm-bind-tools-to-execution-scope (tools execution-scope)
  "Return TOOLS whose native entry points restore EXECUTION-SCOPE.

This is the one LLM boundary for all descriptor shapes: ordinary Agent Loop
wrappers, request-local trusted wrappers, and prepared subagent wrappers.  It
keeps the borrowed Scope in a process-local closure; no Envelope, Tape fact,
or public Runtime value receives it."
  (mapcar
   (lambda (tool)
     (let ((function (llm-tool-function tool)))
       (make-llm-tool
        :name (llm-tool-name tool)
        :description (llm-tool-description tool)
        :args (copy-tree (llm-tool-args tool) t)
        :async (llm-tool-async tool)
        :function
        (lambda (&rest args)
          (let ((superchat-execution-scope execution-scope))
            (apply function args))))))
   tools))

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














(defun superchat--collect-llm-tools
    (&optional input tool-names input-operation precomputed-decision decision-cell)
  "Materialize the tools attached to the current request.

Whether to attach anything at all, and exactly which names, is decided
entirely by `superchat-select-tools-decision' (contract §4.2) -- this
function no longer re-derives that gate from INPUT itself via a keyword
heuristic, which is gone along with `superchat-llm-tools-enabled''s old
on-demand meaning (D14, approved decision 3).  This function is a pure
names-to-descriptors materializer (S09-fix-3 item 2): it neither adds nor
removes a name after the decision -- `report_plan_progress', every
session-synthesized tool, and the `render_ui'/`define_tool' exclusion are
all decided inside `superchat-select-tools-decision' (via
`superchat--select-tools-adjust-names') now, not appended or removed
here.  The final result keeps exactly the names decided, for every reason
\(`explicit', `stage', or `baseline') -- never a wider registry query
result, and never a narrower one either (S09-fix-2 item 1): a `baseline'
decision's own names are sourced from the identical registry query this
function performs (see `superchat--select-tools-custom-baseline-names'),
so filtering by them here is a safe no-op in the honest case and a real
restriction only when the decision and this query have genuinely
diverged.

`:surface' is read from the buffer-local `superchat--last-surface'
\(soft: this file does not require `superchat.el') so the surface-specific
rules (`inline-ask', and `none' for `inline-rewrite'/`org-babel'/`magit'/
`skills'/`compaction') are reachable from a real request through Runtime,
not only from a direct `superchat-select-tools' call -- Runtime sets that
buffer-local immediately before calling into this request's LLM path (its
own `superchat-runtime--surface' already computes it), and it is nil for
any caller not going through that path, which resolves to the same \"every
other surface\" baseline bucket this function used before `:surface'
existed.  `:input' carries INPUT itself, read only for the `render_ui'
keyword check inside the decision.

MCP tools are not collected here.  A connected MCP server registers its
tools with `superchat-register-tool', so they arrive inside
`superchat-get-llm-tools' like any other registered tool -- as `llm-tool'
structs, subject to the same allowlist, and wrapped by the same Authority
path.  Appending them separately, as this function used to, put them on
the surface outside that mechanism and as raw plists.

TOOL-NAMES is an explicit request-local restriction (contract §4.2
rule 1) threaded into the one `superchat-select-tools-decision' call,
which filters it to registered descriptors; the symbol `none' means an
explicitly empty tool set (preset declared `tools: []'): no tools are
attached at all.  INPUT-OPERATION, when non-nil, is read into
`:input-core''s `:operation' for `superchat-select-tools-decision'
(contract rule 3's `ask_user' addition).

PRECOMPUTED-DECISION, when non-nil, is used exactly as-is instead of
calling `superchat-select-tools-decision' again -- a caller (Runtime,
carrying a decision `superchat-pipeline-submit' already made through the
`select-tools' stage) that re-decided from TOOL-NAMES alone would
misreport a `stage'/`baseline' pipeline decision as `explicit' (S09-fix-2
item 2).  DECISION-CELL, when a cons cell, has its `cdr' set to the
decision actually used (precomputed or freshly decided): a caller-owned
lexical cell instead of the deleted global `superchat--llm-last-tool-decision',
so a nested/re-entrant collection during this call's own materialization
cannot overwrite an outer call's already-returned decision -- each call
gets its own cell."
  (let* ((decision (or precomputed-decision
                        (superchat-select-tools-decision
                         (list :tools tool-names
                               :surface (bound-and-true-p superchat--last-surface)
                               :input input
                               :input-core (and input-operation
                                                 (list :operation input-operation)))
                         nil)))
         (names (plist-get decision :names)))
    (when (consp decision-cell) (setcdr decision-cell decision))
    (unless (eq names 'none)
      (let* (;; A `baseline' decision's own `:materialized' structs are the
             ;; exact registry query that decided the baseline slice of
             ;; NAMES -- reuse them instead of querying the registry again
             ;; (S09-fix-2 items 1 and 2).  They never include
             ;; `"ask_user"' (the decision only *names* that addition
             ;; without validating it against a live Run), so it is built
             ;; here, separately and cheaply, with the same
             ;; `superchat-tools--make-ask-user' primitive
             ;; `superchat-get-llm-tools' itself would have called -- not
             ;; a second registry walk.
             (all (cond
                   ((plist-member decision :materialized)
                    (let ((base (plist-get decision :materialized)))
                      (if (and input-operation
                               (fboundp 'superchat-tools--make-ask-user)
                               (not (cl-find "ask_user" base
                                             :key #'llm-tool-name :test #'equal)))
                          (append base
                                  (list (superchat-tools--make-ask-user input-operation)))
                        base)))
                   ((fboundp 'superchat-get-llm-tools)
                    (if input-operation
                        (superchat-get-llm-tools tool-names input-operation)
                      (superchat-get-llm-tools tool-names)))))
             ;; NAMES already is the whole decision (S09-fix-3 item 2):
             ;; `report_plan_progress' and every synthesized tool's name
             ;; may be in NAMES even though neither is necessarily among
             ;; ALL's registry structs -- `report_plan_progress' is
             ;; disabled from the default registry allowlist, and
             ;; synthesized tools are deliberately kept out of the
             ;; registry entirely (`superchat-llm-synthesized-tools').
             ;; RESOLVABLE only ever supplies a struct for a name the
             ;; decision already listed; it can never add a name of its
             ;; own.
             ;; ALL, then the constructed `report_plan_progress' fallback,
             ;; then synthesized tools, in that order: the dedup pass below
             ;; keeps the first struct it sees for a given name, so a
             ;; synthesized tool sharing a name with a real plan-progress
             ;; or registry tool never wins over it (matching the pre-
             ;; S09-fix-3 append order, where `selected'/plan-progress
             ;; preceded synthesized tools in the same way).
             (resolvable
              (append
               all
               (when (and (member "report_plan_progress" names)
                          (not (cl-find "report_plan_progress" all
                                        :key #'llm-tool-name :test #'equal))
                          (fboundp 'llm-make-tool)
                          (fboundp 'superchat--report-plan-progress-spec))
                 (list
                  (apply #'llm-make-tool
                         :name "report_plan_progress"
                         (superchat--report-plan-progress-spec))))
               (bound-and-true-p superchat-llm-synthesized-tools))))
        (let ((seen nil))
          (cl-remove-if
           (lambda (tool)
             (if (or (not (member (llm-tool-name tool) names))
                     (member (llm-tool-name tool) seen))
                 t
               (push (llm-tool-name tool) seen)
               nil))
           resolvable))))))

(defun superchat--llm-check-selected-tools (names tools)
  "Require materialized TOOLS to match the waterfall's NAMES exactly."
  (let ((expected (sort (copy-sequence (unless (eq names 'none) names)) #'string<))
        (actual (sort (mapcar #'llm-tool-name tools) #'string<)))
    (unless (equal expected actual)
      (error "Request middleware tool selection cannot be materialized"))))

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

(defun superchat-llm--finish-check-cleaned (text)
  "Return TEXT without fenced code blocks, quoted passages and quoted lines."
  (let ((cleaned (or text "")))
    (setq cleaned (replace-regexp-in-string "```[^`]*```" "" cleaned))
    (setq cleaned (replace-regexp-in-string "\"[^\"]*\"" "" cleaned))
    (setq cleaned (replace-regexp-in-string "“[^”]*”" "" cleaned))
    (setq cleaned (replace-regexp-in-string "「[^」]*」" "" cleaned))
    (setq cleaned (replace-regexp-in-string "^[ \t]*>.*$" "" cleaned))
    cleaned))

(defun superchat-llm--finish-check-last-sentence (text)
  "Return TEXT's last sentence, trimmed."
  (let* ((trimmed (string-trim (or text "")))
         (length (length trimmed))
         (index (1- length))
         end)
    ;; Skip trailing terminators and whitespace so the last sentence keeps
    ;; its own final punctuation out of the walk-back below.
    (while (and (>= index 0)
                (memq (aref trimmed index)
                      '(?. ?! ?? ?？ ?！ ?。 ?\s ?\t ?\n)))
      (setq index (1- index)))
    (setq end (1+ index))
    ;; Walk back to the previous terminator; the sentence starts after it.
    (while (and (>= index 0)
                (not (memq (aref trimmed index)
                           '(?. ?! ?? ?？ ?！ ?。))))
      (setq index (1- index)))
    (string-trim (substring trimmed (1+ index) end))))

(defun superchat-llm--finish-check-announces-p (sentence)
  "Return non-nil when SENTENCE announces the model's own next action."
  (let ((lower (downcase sentence)))
    (and (not (string-prefix-p "let me know" lower))
         (not (string-prefix-p "you " lower))
         (not (string-prefix-p "please " lower))
         (not (string-prefix-p "请你" sentence))
         (not (string-prefix-p "你可以" sentence))
         ;; "让我知道" asks the user to tell; it announces nothing.
         (not (string-prefix-p "让我知道" sentence))
         (or (string-prefix-p "let me " lower)
             (string-prefix-p "i'll " lower)
             (string-prefix-p "i will " lower)
             (string-prefix-p "now i'll " lower)
             (string-prefix-p "now i will " lower)
             (string-prefix-p "next, i " lower)
             (string-prefix-p "next i " lower)
             (string-prefix-p "让我" sentence)
             (string-prefix-p "我先" sentence)
             (string-prefix-p "我再" sentence)
             (string-prefix-p "我来" sentence)
             (string-prefix-p "我将" sentence)
             (string-prefix-p "接下来我" sentence)))))

(defun superchat-llm--finish-check-trigger (text)
  "Return `empty', `announced', or nil for the text-only reply TEXT.
`empty' is an explicit empty string; `announced' is a last sentence in which
the model announces its own next action, after code and quotes are removed."
  (cond
   ((not (stringp text)) nil)
   ((string-empty-p text) 'empty)
   (t
    (let* ((cleaned (string-trim (superchat-llm--finish-check-cleaned text)))
           (sentence (and (not (string-empty-p cleaned))
                          (superchat-llm--finish-check-last-sentence cleaned))))
      (when (and sentence
                 (not (string-empty-p sentence))
                 (superchat-llm--finish-check-announces-p sentence))
        'announced)))))

(defun superchat-llm--finish-check-chat-run-p (request-context)
  "Return non-nil when REQUEST-CONTEXT's Run has the `chat' surface."
  (let ((run-id (plist-get request-context :run-id)))
    (and (stringp run-id)
         (eq 'chat (plist-get (superchat-runtime-status run-id) :surface)))))

(defun superchat-llm--finish-check-fires-p
    (request-context run-active tool-rounds tool-round-limit
                     final-answer-requested fired reply)
  "Return the trigger symbol when this Step fires its one finish check.
All of the conditions are stated here so a caller cannot half-apply them."
  (when (and superchat-llm-finish-check
             (not fired)
             (not final-answer-requested)
             (> tool-rounds 0)
             (or (not (numberp tool-round-limit))
                 (< tool-rounds tool-round-limit))
             (funcall run-active)
             (superchat-llm--finish-check-chat-run-p request-context))
    (superchat-llm--finish-check-trigger reply)))

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
     (let ((message (string-trim (cdr entry))))
       (and
        ;; An Elisp condition can name a variable such as cancelled.
        ;; Its text is not an Authority refusal.  Actual refusals use the
        ;; separate [Tool call ...] markers, even for eval_elisp.
        (not (and (equal (car entry) "eval_elisp")
                  (string-prefix-p "Error: evaluating expression:" message)))
        (string-match-p
         "\\b\\(?:refused\\|declined\\|cancelled\\|canceled\\|denied\\|blocked\\)\\b"
         message))))
   (superchat--llm-tool-error-entries result)))

(declare-function superchat-reference-pointer-for "superchat-reference")

(defun superchat--llm-reference-pointer-ids (result offered already)
  "Return ids of reference pages that are the known answer to RESULT's failures.
OFFERED is the tool names of the request the feedback is appended to: without
`read_reference' among them there is no entry to point at, so there is no
pointer.  ALREADY is the ids this tool loop has pointed at before; a page is
named once, not at every repetition of the error.  Only an error some page
claims gets a pointer; the error text itself stays in the tool result as it is.
docs/decisions/accepted/2026-09-20-reference-reading-on-demand.md"
  (when (and (member "read_reference" offered)
             (fboundp 'superchat-reference-pointer-for))
    (let (ids)
      (dolist (entry (superchat--llm-tool-error-entries result))
        (let ((id (superchat-reference-pointer-for (car entry) (cdr entry))))
          (when (and id (not (member id already)) (not (member id ids)))
            (push id ids))))
      (nreverse ids))))

(defun superchat--llm-tool-error-feedback (result &optional pointer-ids)
  "Return corrective feedback when RESULT contains failed tool calls.
POINTER-IDS, from `superchat--llm-reference-pointer-ids', are reference pages
to name after the instruction.

Add a short user-role instruction after tool results: providers otherwise
tend to narrate a fallback instead of repairing and retrying the requested
tool.  Return nil for successful tool rounds or ordinary text responses.
The feedback tells the model not to retry a user or policy refusal while
leaving the tool loop available for an alternative action.  Error details
already appear in the tool results; this reminder references them without
copying their potentially large contents into another prompt message.
"
  (let ((errors (superchat--llm-tool-error-entries result)))
    (when errors
      (concat
       "Tool execution failed. Do not claim success or silently substitute a fallback. "
       (if (superchat--llm-tool-error-non-retryable-p result)
           "The user or a policy prevented this call; do not retry it. Explain the limitation or continue without that tool."
         "Correct the arguments of the failed call and retry it. Once it succeeds, continue the user's original request.")
       "\nRead the error details in the preceding tool results. Failed tools:\n"
       (mapconcat (lambda (entry) (format "- %s" (car entry))) errors "\n")
       (mapconcat (lambda (id)
                    (format "\nThe reference page `%s' covers this kind of error; read_reference can fetch it before you retry."
                            id))
                  pointer-ids "")))))

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

(defun superchat--llm-record-tool-budget (observer rounds limit action)
  "Tell Runtime OBSERVER why tools ended at ROUNDS/LIMIT with ACTION.
This records a budget decision, not whether the user's task succeeded."
  (when (functionp observer)
    (funcall observer 'tool-budget
             (list :rounds rounds :limit limit :action action))))

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
request.  Otherwise reasoning comes from `superchat-llm-reasoning'.
The effective provider's mapping is resolved at dispatch; nil delegates
to provider/server defaults.

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
         (max-tokens (or superchat--llm-request-max-tokens
                         (and preset (superchat-preset-max-tokens preset))))
         (args (append (when tools (list :tools tools))
                       (when (and (stringp context)
                                  (not (string-empty-p (string-trim context))))
                         (list :context context))
                       (when (and preset (superchat-preset-temperature preset))
                         (list :temperature
                               (superchat-preset-temperature preset)))
                       (when max-tokens
                         (list :max-tokens max-tokens))
                       (when reasoning
                         (list :reasoning (if (eq reasoning t)
                                             'medium
                                           reasoning))))))
    (apply #'llm-make-chat-prompt content args)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-chat-model ((provider llm-openai))
     (llm-openai-chat-model provider)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-name ((_provider llm-openai-compatible))
     "open ai compatible"))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-openai) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-openai 'llm-openai new-model)))

(eval-after-load 'llm-claude
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-claude) new-model)
     (superchat--copy-provider-with-chat-model
      provider #'copy-llm-claude 'llm-claude new-model)))

(eval-after-load 'llm-ollama
  '(cl-defmethod superchat--provider-name ((_provider llm-ollama))
     "ollama"))

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

(defun superchat--callback-in-view (buffer callback &optional dead-callback)
  "Bind CALLBACK to View BUFFER; optionally run DEAD-CALLBACK after its death."
  (lambda (&rest args)
    (if (buffer-live-p buffer)
        (with-current-buffer buffer (let ((superchat-buffer-name (buffer-name buffer))) (apply callback args)))
      (when (functionp dead-callback) (let ((superchat-buffer-name nil)) (apply dead-callback args))))))

(defun superchat--llm-request-plz-propagate-callback-errors
    (original url &rest args)
  "Call ORIGINAL for URL, routing async success callback errors to `:on-error'.
For a Superchat Ollama request, decode curl output as UTF-8 so Emacs carries
incomplete characters across process-filter chunks."
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
    (let ((process (apply original url args)))
      (when (and superchat--llm-ollama-response-p
                 (processp process))
        (set-process-coding-system process 'utf-8-unix 'binary))
      process)))

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
            &optional target-model context-files tools system-prompt preset
            precomputed-tool-decision)
  "Generate an answer for PROMPT using llm.el, handling streaming and tool use.
Optionally use TARGET-MODEL for this request only.
CONTEXT-FILES is an optional list of file paths to include as context.
TOOLS is an optional list of tool names to expose; when nil, tools are
collected from PROMPT and global settings as before.
PRECOMPUTED-TOOL-DECISION, when non-nil, is a `(:names .. :reason ..)'
decision Runtime already carries on the Request (`:tool-decision', set by
`superchat-pipeline-submit' when the `select-tools' stage ran) -- it is
honored as-is instead of being re-decided from TOOLS alone, so a pipeline
`stage'/`baseline' decision is not misreported as `explicit' (contract
§4.2, S09-fix-2 item 2).
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
  ;; Transport values have Kernel defaults, while the standard distribution
  ;; installs optional response-mode, timeout, TTFT, and activity callbacks
  ;; through `superchat-llm-configure'.  A direct Kernel caller takes this
  ;; same request path without loading the chat distribution; absent UI
  ;; adapters are intentional no-ops.
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
    (let* ((request-buffer (current-buffer))
           ;; Provider tool invocations, including retry attempts, outlive the
           ;; Runtime's dynamic call.  Capture this Run's borrowed Scope once
           ;; at request entry and hand it to every native tool boundary.
           (execution-scope (superchat-execution-scope-resolve nil))
           (runtime-observer superchat--llm-runtime-observer)
           (tool-execution-context
            (copy-sequence (bound-and-true-p superchat--tool-execution-context)))
           ;; Same reasoning as `runtime-observer': captured once, lexically,
           ;; so a paused-then-resumed or otherwise async continuation still
           ;; sees Runtime's port after its dynamic `let*' has unwound.
           (drain-steers-function superchat--llm-drain-steers-function)
           (pending-steer-count-function superchat--llm-pending-steer-count-function)
           (transcript-owner superchat--llm-transcript-owner)
           ;; Same capture rule again: the request evidence recorder runs on
           ;; every attempt, including a retry continuation that outlives the
           ;; dynamic binding, so the identity and the message composition are
           ;; taken once here.
           (request-context superchat--llm-request-context)
           (input-tool
            (when (and (boundp 'superchat--tool-execution-context)
                       (plist-get superchat--tool-execution-context :input-core-token))
              (let ((name (plist-get superchat--tool-execution-context :input-core-tool-name)))
                (when (and (stringp name) (not (string-empty-p name)))
                  (substring-no-properties name)))))
           ;; The Runtime binding is dynamic and provider callbacks are
           ;; asynchronous.  Capture the request evidence once so a later
           ;; tool-followup does not fall back to the global nil value after
           ;; the Runtime's dynamic scope has unwound.
           (evidence-context
            (copy-tree superchat--llm-evidence-context t))
           (evidence-contract
            (plist-get evidence-context :contract))
           (active-experience-view
            (plist-get
             (plist-get evidence-context :context-envelope)
             :experience-view))
           (tools-prepared-p superchat--llm-tools-prepared-p)
           (input-operation (plist-get (bound-and-true-p superchat--tool-execution-context) :input-operation))
           (_input-operation
            (when input-operation
              (unless (and (not tools-prepared-p)
                           (fboundp 'superchat-runtime--trusted-input-operation-p)
                           (superchat-runtime--trusted-input-operation-p
                            superchat--tool-execution-context input-operation t))
                (error "Trusted human input is unavailable at dispatch"))))
           ;; A caller-owned lexical cell, not the deleted global
           ;; `superchat--llm-last-tool-decision': a nested or later
           ;; re-entrant tool collection elsewhere gets its own cell and
           ;; cannot overwrite this one before this attempt's async
           ;; continuation resumes and reads it (S09-fix-2 item 2).
           (tool-decision-cell (cons nil nil))
           (tools (if tools-prepared-p
                      superchat--llm-prepared-tools
                    (superchat--collect-llm-tools
                     (when (stringp prompt) prompt) tools input-operation
                     precomputed-tool-decision tool-decision-cell)))
           (_waterfall-materialization
            (when (eq (plist-get precomputed-tool-decision :reason) 'waterfall)
              (superchat--llm-check-selected-tools
               (plist-get precomputed-tool-decision :names) tools)))
           ;; Read right after the one materialization call above.
           ;; `tools-prepared-p' requests skip `superchat--collect-llm-tools'
           ;; entirely (a subagent/prepared path with its own,
           ;; already-decided surface), so there is no fresh decision to
           ;; report for them.
           (tool-decision-reason
            (and (not tools-prepared-p)
                 (plist-get (cdr tool-decision-cell) :reason)))
           (_prepared-input
            (when (and tools-prepared-p
                       (cl-some (lambda (tool) (equal "ask_user" (llm-tool-name tool))) tools))
              (error "Prepared tools cannot advertise ask_user")))
           (_input-descriptor
            (when input-operation
              (let ((selected (cl-remove-if-not
                               (lambda (tool) (equal "ask_user" (llm-tool-name tool))) tools)))
                (unless (and (= (length selected) 1)
                             (fboundp 'superchat-tools--trusted-ask-descriptor-p)
                             (superchat-tools--trusted-ask-descriptor-p (car selected) input-operation))
                  (error "Trusted human input descriptor was replaced")))))
           (_evidence-tools
            (when evidence-contract
              (superchat--llm-validate-evidence-tools tools)))
           ;; Use the request's topology-selected wrapper for tools created
           ;; inside this request.  Ordinary paths build the agent wrapper;
           ;; prepared subagent paths inject their subagent wrapper.
           (wrap-fn
            (or superchat--llm-tool-wrapper
                (and tools
                     (not tools-prepared-p)
                     (fboundp 'superchat--agent-make-wrapper)
                     (superchat--agent-make-wrapper preset))))
           (_evidence-wrapper
            (when (and evidence-contract tools (not (functionp wrap-fn)))
              (signal
               'superchat-llm-evidence-recording-error
               '("epistemic-learning-v0 requires the Authority tool wrapper"))))
           (_ordinary-wrapper
            (when (and tools
                       (not tools-prepared-p)
                       (not (functionp wrap-fn)))
              (signal
               'superchat-llm-tool-wrapper-required
               '("Ordinary tools require a callable Authority wrapper"))))
           (tools (if (and wrap-fn (not tools-prepared-p))
                      (mapcar wrap-fn tools)
                    tools))
           ;; Every provider-facing descriptor crosses this binder, including
           ;; a request-local trusted wrapper or a prepared subagent wrapper
           ;; that did not originate in Agent Loop.
           (tools (superchat--llm-bind-tools-to-execution-scope
                   tools execution-scope))
           (response-mode (superchat-llm--response-mode tools))
           (_ (superchat-llm--indicate-response-mode response-mode))
           (base-timeout (superchat-llm--adjusted-timeout response-mode))
           ;; Nil: no cumulative limit, and the owner's hard clock stays
           ;; `disabled' for the whole call.
           (hard-timeout-limit
            (and superchat-response-timeout-hard-limit
                 (max 1 superchat-response-timeout-hard-limit)))
           (effective-backend (superchat--effective-llm-backend target-model))
           (request-model
            (or (superchat--provider-chat-model effective-backend)
                target-model
                "unknown"))
           (context-capability
            (superchat--llm-context-capability
             effective-backend request-model))
           ;; Emacs must carry incomplete UTF-8 characters across the raw
           ;; process-filter chunks produced by plz for Ollama NDJSON.
           (superchat--llm-ollama-response-p
            (and (fboundp 'llm-ollama-p)
                 (llm-ollama-p effective-backend)))
           ;; §5.1: this is the request's *initial* resolution, against the
           ;; original request tools; it exists only to feed the timeout
           ;; preflight estimate just below, which runs once per request
           ;; before any attempt is sent.  `send-request' (below) shadows
           ;; both bindings per attempt, against that attempt's actual tool
           ;; surface — a round-limit `final-answer' attempt drops tools, so
           ;; its resolution (and the `:capability-source' it records) must
           ;; not be this one.  `superchat-llm--resolve-streaming' implements
           ;; the override > provider > adapter > fallback precedence.
           (streaming-capabilities
            (superchat-llm--resolve-streaming
             effective-backend request-model tools))
           (streaming-request-p
            (and superchat-llm-streaming
                 (if tools
                     (and (plist-get streaming-capabilities :streaming)
                          (plist-get streaming-capabilities :streaming-tool-use))
                   (plist-get streaming-capabilities :streaming))))
           (real-prompt (superchat--build-llm-prompt
                         prompt tools system-prompt preset context-files))
           ;; Runtime has already folded the initial request.  These lexical
           ;; values track the latest provider-facing inputs so an async retry
           ;; can apply its own Scope-correct fold without rebuilding or
           ;; serializing the Run's private execution Scope.
           (provider-prompt prompt)
           (provider-system-prompt system-prompt)
           (timeout-context-estimate
            (and base-timeout
                 (let ((superchat--llm-preflight-streaming-p
                        streaming-request-p))
                   (superchat--llm-estimate-context-tokens
                    effective-backend real-prompt))))
           (adjusted-timeout
            (superchat-llm--adaptive-response-timeout
             base-timeout timeout-context-estimate))
           (transport-timer-buffer
            request-buffer)
           (transport-timer-owner
            (superchat-llm--make-transport-timer-owner
             :buffer transport-timer-buffer
             :input-clocks-p (and input-tool t)
             :publication-order
             (superchat-llm--next-transport-timer-owner-order
              transport-timer-buffer)))
           (_transport-timer-owner-registration
            (when (functionp
                   superchat--llm-transport-timer-owner-function)
              (setf
               (superchat-llm--transport-timer-owner-run-id
                transport-timer-owner)
               (funcall superchat--llm-transport-timer-owner-function
                        transport-timer-owner))))
           (context-prefix
            (or (plist-get superchat--llm-context-envelope :prompt)
                (plist-get
                 (plist-get evidence-context :context-envelope)
                 :prompt)))
           ;; Keep the request-level functions unwrapped by provider attempt.
           ;; `real-prompt' adopts completed attempt history, including its
           ;; attempt wrappers, so using its tool objects here would nest old
           ;; provenance inside every later tool round.
           (evidence-base-tools
            (and evidence-contract (copy-tree tools t)))
           ;; Bind `define_tool' to THIS prompt, so what it writes can be
           ;; injected into the request that is already in flight and used
           ;; on the next round.  Persist to the session registry: unlike a
           ;; sub-agent, the main session has later turns.
           (_ (when (fboundp 'superchat--syn-bind)
                (superchat--syn-bind
                 real-prompt wrap-fn
                 superchat--llm-persist-synthesized-tools)))
           ;; Completion metadata (notably `:stop-reason') is only available
           ;; through llm.el's multi-output response.
           (multi-output t)
           (response-parts '())
           (tool-rounds 0)
           (tool-round-limit superchat-llm-max-tool-rounds)
           (final-answer-requested nil)
           ;; The finish check fires at most once per Run.
           (finish-check-fired nil)
           ;; Reference pages this loop has already pointed the model at.
           (reference-pointed nil)
           (completed nil)
           (arm-timeout nil)
           (recover-context nil)
           (context-recovery-attempted nil)
           (context-recovery-active nil)
           (last-attempt-details nil)
           (last-model-phase 'waiting-first-token)
           ;; ── TTFT measurement (meausred flag is lexical, closure-safe) ──
           (ttft-measured nil)
           (partial-answer-seen nil)
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
           ;; Reasoning partials use the same cumulative shape as text in
           ;; qwen/OpenAI-compatible streams.  Keep a separate cursor:
           ;; reasoning must not share the visible answer's cursor because
           ;; providers can interleave the two channels.
           (reasoning-cumulative "")
           (tool-progress-cumulative nil)
           (tool-marker-cumulative nil)
           (tool-error-retries 0)
           (transient-retries 0)
           (unknown-tool-retry-limit
            (or superchat--llm-unknown-tool-retry-limit
                superchat-llm-unknown-tool-retries))
           (round-limit-function superchat--llm-round-limit-function)
           (run-active-p (or superchat--llm-run-active-p (lambda () t)))
           (request-generation 0)
           (callback-active-p
            (lambda ()
              (and (not completed) (funcall run-active-p)
                   (or (null superchat--llm-callback-attempt)
                       (= superchat--llm-callback-attempt request-generation)))))
           (record-first-output
            (lambda (value)
              (let ((attempt (or superchat--llm-callback-attempt
                                 request-generation)))
                (when (and evidence-contract
                           (> attempt 0)
                           (functionp runtime-observer))
                  (condition-case err
                      (funcall runtime-observer 'first-output
                               (append (list :attempt attempt) value))
                    (error
                     (signal 'superchat-llm-evidence-recording-error
                             (list (error-message-string err)))))))))
           (record-usage
            (lambda (value)
              (when (and (consp value)
                         (numberp (plist-get value :input-tokens))
                         (functionp runtime-observer))
                (funcall runtime-observer
                         'usage
                         (list :attempt
                               (or superchat--llm-callback-attempt
                                   request-generation)
                               :input-tokens (plist-get value :input-tokens)
                               :output-tokens
                               (plist-get value :output-tokens))))))
           (send-request nil)
           (cancel-wait-timeout
            (lambda ()
              ;; A callback may reenter the next attempt after its entry
              ;; check.  The owner spans attempts, so compare generation too.
              (when (or (null superchat--llm-callback-attempt)
                        (eql superchat--llm-callback-attempt
                             (superchat-llm--transport-timer-owner-wait-generation
                              transport-timer-owner)))
                (superchat-llm--transport-timer-owner-cancel-wait
                 transport-timer-owner))))
           (refresh-wait-timeout
            (lambda ()
              (let ((generation (or superchat--llm-callback-attempt
                                    request-generation))
                    (timer (superchat-llm--transport-timer-owner-wait-timer
                            transport-timer-owner)))
                (when (and (not completed) (funcall run-active-p)
                           (= generation request-generation)
                           (or (null timer)
                               (eql generation
                                    (superchat-llm--transport-timer-owner-wait-generation
                                     transport-timer-owner))))
                  ;; Adapters may reenter, pause or advance the Run.  The
                  ;; transport owns inactivity: legacy +30s notifications
                  ;; cannot accumulate onto each streamed token's deadline.
                  (superchat-llm--notify-activity transport-timer-owner)
                  (when (and timer
                             (eq timer
                                 (superchat-llm--transport-timer-owner-wait-timer
                                  transport-timer-owner))
                             (= generation request-generation)
                             (not completed) (funcall run-active-p))
                    (funcall arm-timeout generation))))))
           (set-model-phase
            (lambda (phase)
              (unless (eq phase last-model-phase)
                (setq last-model-phase phase)
                (when (functionp runtime-observer)
                  (funcall runtime-observer 'model-phase phase)))))
           (call-stream-sink
            (lambda (value)
              (when stream-callback
                (condition-case sink-error
                    (funcall stream-callback value)
                  (error
                   (ignore-errors
                     (display-warning
                      'superchat-llm
                      (format "Stream sink failed: %s"
                              (error-message-string sink-error))
                      :warning)))))))
           ;; ---- local helper: finalize exactly once ----
           (finalize
            (lambda (response &optional status)
              (unless completed
                (setq completed t)
                (superchat-llm--transport-timer-owner-cancel
                 transport-timer-owner)
                (when (functionp runtime-observer)
                  (funcall runtime-observer
                           'terminal
                           (list :status (or status 'completed)
                                 :response response)))
                (when callback
                  (funcall callback response)))))
           ;; Multi-output plists may carry text, reasoning and tool data
           ;; together.  Every public callback can reenter a newer attempt;
           ;; recheck before writing shared cursors or delivering old output.
           (stream-cb
            (lambda (chunk)
              (when (funcall callback-active-p)
                (funcall record-usage chunk))
              (when (funcall callback-active-p)
                (let* ((text (cond
                              ((stringp chunk) chunk)
                              ((consp chunk) (plist-get chunk :text))))
                       (reasoning
                        (and (consp chunk)
                             (if (eq (car chunk) 'reasoning)
                                 (cadr chunk)
                               (plist-get chunk :reasoning))))
                       (tool-marker
                        (and (consp chunk)
                             (memq (car chunk) '(tool-call tool-result))))
                       (tool-progress
                        (and (consp chunk) (keywordp (car chunk))
                             (let ((parts
                                    (mapcar
                                     (lambda (key)
                                       (let ((value (plist-get chunk key)))
                                         ;; Empty arrays/strings are truthy
                                         ;; in Elisp, but carry no progress.
                                         (unless (or (null value)
                                                     (and (arrayp value)
                                                          (zerop (length value)))
                                                     (and (hash-table-p value)
                                                          (zerop (hash-table-count value))))
                                           value)))
                                     '(:tool-uses-raw :tool-uses :tool-results))))
                               (when (cl-some #'identity parts) parts)))))
                  (when (and (stringp text) (funcall callback-active-p))
                    (let ((delta
                           (if (string-prefix-p cumulative text)
                               (substring text (length cumulative))
                             text)))
                      (unless (string-empty-p delta)
                        (funcall refresh-wait-timeout)
                        (when (funcall callback-active-p)
                          (setq cumulative text)
                          (funcall set-model-phase 'answer-streaming))
                        (when (funcall callback-active-p)
                          (funcall record-first-output
                                   (list :kind "text" :text delta)))
                        (when (funcall callback-active-p)
                          (setq partial-answer-seen t)
                          ;; TTFT remains the first visible answer delta.
                          (when (and superchat-show-ttft (not ttft-measured))
                            (setq ttft-measured t)
                            (when (buffer-live-p request-buffer)
                              (with-current-buffer request-buffer
                                (when superchat--ttft-start-time
                                  (superchat-llm--report-ttft
                                   (- (float-time) superchat--ttft-start-time)))))))
                        (when (funcall callback-active-p)
                          (push delta response-parts)
                          (funcall call-stream-sink delta)))))
                  (when (and (stringp reasoning) (funcall callback-active-p))
                    (let ((delta
                           (if (string-prefix-p reasoning-cumulative reasoning)
                               (substring reasoning (length reasoning-cumulative))
                             reasoning)))
                      (unless (string-empty-p delta)
                        (funcall refresh-wait-timeout)
                        (when (funcall callback-active-p)
                          (setq reasoning-cumulative reasoning)
                          (funcall set-model-phase 'reasoning))
                        (when (funcall callback-active-p)
                          (funcall call-stream-sink (list :reasoning delta))))))
                  (when (and tool-marker (funcall callback-active-p)
                             (not (equal chunk tool-marker-cumulative)))
                    (funcall refresh-wait-timeout)
                    (when (funcall callback-active-p)
                      (funcall set-model-phase 'tool-streaming))
                    (when (funcall callback-active-p)
                      (setq tool-marker-cumulative (copy-tree chunk t))
                      (when callback (funcall callback chunk))))
                  (when (and tool-progress (funcall callback-active-p)
                             (not (equal tool-progress tool-progress-cumulative)))
                    (funcall refresh-wait-timeout)
                    (when (funcall callback-active-p)
                      (funcall set-model-phase 'tool-streaming))
                    (when (funcall callback-active-p)
                      (setq tool-progress-cumulative (copy-tree tool-progress t))))
                  (unless (or (stringp text) (stringp reasoning)
                              tool-marker tool-progress
                              (and (consp chunk)
                                   (or (numberp (plist-get chunk :input-tokens))
                                       (plist-member chunk :stop-reason)
                                       (plist-member chunk :tool-uses-raw)
                                       (plist-member chunk :tool-uses)
                                       (plist-member chunk :tool-results))))
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

      ;; One inactivity window belongs to one actual provider attempt.
      ;; Every acquisition carries both timer identity and generation, so a
      ;; callback already queued before cancel cannot affect its replacement.
      (setq arm-timeout
            (lambda (&optional wait-generation)
              (let* ((generation (or wait-generation request-generation))
                     (eligible-p
                      (lambda ()
                        (and adjusted-timeout (not completed)
                             (funcall run-active-p)
                             (= generation request-generation)
                             (memq (superchat-llm--transport-timer-owner-hard-state
                                    transport-timer-owner)
                                   '(running disabled))))))
                (when (funcall eligible-p)
                  (superchat-llm--transport-timer-owner-cancel-wait
                   transport-timer-owner)
                  ;; Cancellation can itself enter the next attempt.  Never
                  ;; publish an old acquisition after returning from it.
                  (when (funcall eligible-p)
                    (let ((timer (timer-create))
                          (seconds adjusted-timeout))
                      (timer-set-time timer (time-add (current-time) seconds))
                      (timer-set-function
                       timer
                       (lambda ()
                         (when (and (funcall eligible-p)
                                    (eql generation
                                         (superchat-llm--transport-timer-owner-wait-generation
                                          transport-timer-owner))
                                    (eq timer
                                        (superchat-llm--transport-timer-owner-wait-timer
                                         transport-timer-owner)))
                           (let ((notice
                                  (format "[Response timeout: no progress for %ss; attempt: %s; last phase: %s]"
                                          seconds generation last-model-phase)))
                             (funcall finalize
                                      (if response-parts
                                          (concat (string-join (reverse response-parts) "")
                                                  "\n\n" notice)
                                        notice)
                                      'timed-out)))))
                      (setf (superchat-llm--transport-timer-owner-wait-timer
                             transport-timer-owner) timer
                            (superchat-llm--transport-timer-owner-wait-generation
                             transport-timer-owner) generation)
                      (superchat-llm--transport-timer-owner-publish-wait
                       transport-timer-owner timer)
                      (unwind-protect
                          (when (and (funcall eligible-p)
                                     (eq timer
                                         (superchat-llm--transport-timer-owner-wait-timer
                                          transport-timer-owner)))
                            (timer-activate timer))
                        (unless (and (funcall eligible-p)
                                     (eq timer
                                         (superchat-llm--transport-timer-owner-wait-timer
                                          transport-timer-owner)))
                          (cancel-timer timer)))))))))
      (when (and adjusted-timeout hard-timeout-limit)
        (let ((expire
               (lambda ()
                 (let ((notice (format "[Response hard timeout after %ss; last phase: %s]"
                                       hard-timeout-limit last-model-phase)))
                   (funcall finalize
                            (if response-parts
                                (concat (string-join (reverse response-parts) "") "\n\n" notice)
                              notice)
                            'timed-out)))))
          ;; Ordinary and input Runs share the same identity-bound hard
          ;; clock; only the trusted input protocol can freeze its balance.
          (unless (eq 'closed
                      (superchat-llm--transport-timer-owner-hard-state
                       transport-timer-owner))
            (superchat-llm--input-arm-hard
             transport-timer-owner hard-timeout-limit expire))))

      (when target-model
        (message "Switching to model: %s" target-model))

      ;; ── TTFT measurement: reset per-request flag ──
      (when superchat-show-ttft
        (setq ttft-measured nil))

      (superchat-llm--report-stage "pre-llm-call")

      ;; ── Prompt size diagnostic ──
      (when superchat-show-ttft-breakdown
        (let* ((chars (length (or (and (stringp prompt) prompt) "")))
               (tools-chars (if tools (length (format "%S" tools)) 0))
               ;; M1: the count comes from the Tape derivation, not from the
               ;; per-View cache, so a diagnostic cannot disagree with the
               ;; history the prompt was actually built from.
               (history-len
                (length (if (fboundp 'superchat--derived-history)
                            (superchat--derived-history)
                          superchat--conversation-history)))
               (est-tokens (/ (+ chars tools-chars) 4)))
          (message "⏱ TTFT [prompt-size]: %d chars + %d tools + %d history-msgs (~%d tokens)"
                   chars tools-chars history-len est-tokens)))

      (setq error-cb
            (lambda (type &rest more)
              (let ((err (if more (car more) type)))
                (when (eq type 'superchat-llm-evidence-recording-error)
                  (signal type (if more more (list err))))
                (when (superchat--llm-tool-protocol-error-p type)
                  (funcall record-first-output
                           (list :kind "tool-protocol-error"
                                 :error-type (format "%s" type)
                                 :message (format "%s" err))))
                (cond
                 ;; Provider overflow is a context-management event, not a
                 ;; transient transport retry.  A loaded compaction plugin
                 ;; may rebuild the prompt and resume this same Run once.
                 ((and (not completed)
                       (funcall run-active-p)
                       (or (eq type 'llm-context-length-exceeded)
                           (superchat--llm-context-overflow-error-p err))
                       (functionp recover-context)
                       (funcall recover-context
                                'overflow
                                (or (plist-get last-attempt-details
                                               :attempt-reason)
                                    'initial)
                                (format "[API context overflow: %s]" err)
                                last-attempt-details))
                  nil)
                 ;; Tool-call validation errors are model-correctable.  The
                 ;; provider has already appended an assistant tool-use
                 ;; record, so remove that dangling record, append the exact
                 ;; llm.el error as a user-role correction, and resend.
                 ((and (fboundp 'superchat--llm-tool-protocol-error-p)
                       (superchat--llm-tool-protocol-error-p type)
                       (not final-answer-requested)
                       (not evidence-contract)
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
                  (setq response-parts '()
                        cumulative ""
                        reasoning-cumulative "")
                  (condition-case e
                      (funcall send-request 'tool-protocol-retry)
                    (superchat-llm-evidence-recording-error
                     (signal (car e) (cdr e)))
                    (error
                     (funcall finalize
                              (format "[llm-chat-streaming error: %s]"
                                      (error-message-string e))
                              'failed))))
                 ((and (not completed)
                       (funcall run-active-p)
                       (not response-parts)
                       (string-empty-p reasoning-cumulative)
                       (< transient-retries superchat-llm-transient-retries)
                       (superchat--llm-transient-error-p
                        type err superchat--llm-http-status))
                  (let ((delay superchat-llm-transient-retry-interval))
                    (cl-incf transient-retries)
                    (funcall cancel-wait-timeout)
                    (run-at-time
                     delay nil
                     (lambda ()
                       (when (and (not completed) (funcall run-active-p))
                         (condition-case e
                             (funcall send-request 'transient-retry)
                           (superchat-llm-evidence-recording-error
                            (signal (car e) (cdr e)))
                           (error
                            (funcall finalize
                                     (format "[llm-chat-streaming error: %s]"
                                             (error-message-string e))
                                     'failed))))))))
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
                (setq transient-retries 0)
                (funcall record-usage response)
                (let ((final-text
                       (cond
                        ((and (stringp response)
                              (not (string-empty-p response)))
                         response)
                        ((and (consp response)
                              (stringp (plist-get response :text))
                              (not (string-empty-p
                                    (plist-get response :text))))
                         (plist-get response :text)))))
                  (when final-text
                    (funcall set-model-phase 'answer-streaming)
                    (funcall cancel-wait-timeout)
                    ;; A callback-only provider exposes completion latency,
                    ;; not TTFT.  Keep that distinction visible in the
                    ;; Assistant header instead of inventing a first token.
                    (when (and superchat-show-ttft
                               (not partial-answer-seen)
                               (not ttft-measured))
                      (setq ttft-measured t)
                      (when (buffer-live-p request-buffer)
                        (with-current-buffer request-buffer
                          (when superchat--ttft-start-time
                            (superchat-llm--report-nonpartial-latency
                             (- (float-time) superchat--ttft-start-time))))))))
                (cond
                 ((and (stringp response)
                       (not (string-empty-p response)))
                  (funcall record-first-output
                           (list :kind "text" :text response)))
                 ((and (consp response)
                       (stringp (plist-get response :text))
                       (not (string-empty-p (plist-get response :text))))
                  (funcall record-first-output
                           (list :kind "text"
                                 :text (plist-get response :text)))))
                (cond
                 ;; Some OpenAI-compatible servers finish successfully with
                 ;; `length' and no text instead of returning an HTTP error.
                 ((and (superchat--llm-context-overflow-response-p
                        response
                        (plist-get last-attempt-details :context-limit)
                        response-parts)
                       (functionp recover-context)
                       (funcall recover-context
                                'overflow
                                (or (plist-get last-attempt-details
                                               :attempt-reason)
                                    'initial)
                                "[API context overflow: provider response exceeded the active window]"
                                last-attempt-details))
                  nil)
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
                             (setq final-answer-requested t)
                           (superchat--llm-record-tool-budget
                            runtime-observer tool-rounds tool-round-limit 'finalize)
                           (superchat--llm-disable-prompt-tools real-prompt)))
                        ('finalize
                         (setq final-answer-requested t)
                         (superchat--llm-record-tool-budget
                          runtime-observer tool-rounds tool-round-limit 'finalize)
                         (superchat--llm-disable-prompt-tools real-prompt))
                        ('stop
                         (setq continue nil)
                         (superchat--llm-record-tool-budget
                          runtime-observer tool-rounds tool-round-limit 'stop)
                         (funcall finalize
                                  (format "[Stopped after %d tool rounds]"
                                          tool-rounds)
                                  'failed))))
                      (when (and continue (not completed))
                        (when (not final-answer-requested)
                          ;; A user/policy refusal is terminal for that
                          ;; tool call, not for the whole tool loop.  Keep
                          ;; tools available so the model can choose an
                          ;; alternative; the feedback explicitly tells it
                          ;; not to retry the refused call.
                          (let* ((pointer-ids
                                  (superchat--llm-reference-pointer-ids
                                   response
                                   (mapcar #'llm-tool-name
                                           (ignore-errors
                                             (llm-chat-prompt-tools real-prompt)))
                                   reference-pointed))
                                 (feedback
                                  (superchat--llm-tool-error-feedback
                                   response pointer-ids)))
                            (setq reference-pointed
                                  (append reference-pointed pointer-ids))
                            (when feedback
                              (when (fboundp 'llm-chat-prompt-append-response)
                                (llm-chat-prompt-append-response
                                 real-prompt feedback 'user)))))
                        ;; Close this round's rendered text before the next
                        ;; request: the transcript keeps the round's prose and
                        ;; the tool records, and the next round's text
                        ;; continues the SAME assistant region after them --
                        ;; a non-final close (S21) never opens a second
                        ;; `** Assistant:' heading.  Also drop this round's
                        ;; streamed parts so they cannot leak into the final
                        ;; answer's fallback join.
                        (when (and (buffer-live-p request-buffer) (fboundp 'superchat--response-region-close))
                          (with-current-buffer request-buffer
                            (superchat--response-region-close nil nil transcript-owner)))
                        (setq response-parts '())
                        ;; Each round is a fresh llm request whose
                        ;; accumulated text restarts from empty.
                        (setq cumulative ""
                              reasoning-cumulative "")
                        (condition-case err
                            (funcall send-request
                                     (if final-answer-requested
                                         'final-answer
                                       'tool-followup))
                          (superchat-llm-evidence-recording-error
                           (signal (car err) (cdr err)))
                          (error
                           (funcall finalize
                                    (format "[llm-chat-streaming error: %s]"
                                            (error-message-string err))
                                    'failed))))))))
                  (t
                   (let ((pending
                          (if (functionp runtime-observer)
                              (or (funcall runtime-observer 'turn-stopping nil) 0)
                            (if pending-steer-count-function
                                (funcall pending-steer-count-function)
                              0))))
                     (if (and (> pending 0)
                              (or (not (numberp tool-round-limit))
                                  (< tool-rounds tool-round-limit)))
                         ;; A steer arrived after the model already finished
                         ;; with plain text: force one more attempt instead of
                         ;; finalizing, so the steer still reaches the model.
                         (progn
                           (cl-incf tool-rounds)
                           (when (and (buffer-live-p request-buffer)
                                      (fboundp 'superchat--response-region-close))
                             (with-current-buffer request-buffer
                               (superchat--response-region-close nil nil transcript-owner)))
                           (setq response-parts '())
                           (setq cumulative ""
                                 reasoning-cumulative "")
                           (condition-case err
                               (funcall send-request 'tool-followup)
                             (superchat-llm-evidence-recording-error
                              (signal (car err) (cdr err)))
                             (error
                              (funcall finalize
                                       (format "[llm-chat-streaming error: %s]"
                                               (error-message-string err))
                                       'failed))))
                       ;; Provider success may still omit an answer. Keep
                       ;; explicit empty strings and this attempt's text.
                       (if (or (stringp response)
                               (and (consp response)
                                    (stringp (plist-get response :text)))
                               response-parts)
                           (let* ((reply
                                   (cond
                                    ((and (consp response)
                                          (stringp (plist-get response :text)))
                                     (plist-get response :text))
                                    ((stringp response) response)
                                    (response-parts
                                     (string-join (reverse response-parts) ""))
                                    (t "")))
                                  (trigger
                                   (superchat-llm--finish-check-fires-p
                                    request-context run-active-p
                                    tool-rounds tool-round-limit
                                    final-answer-requested finish-check-fired
                                    reply)))
                             ;; One finish check: the note is queued through the
                             ;; existing steering path, so it reaches the model
                             ;; and the Tape like any steer, and the next reply
                             ;; either does the announced step or is final.
                             ;; A note that cannot be queued (the Run stopped
                             ;; accepting input between the checks and now) must
                             ;; not send a request with nothing new in it: the
                             ;; reply is then final, as without the check.
                             (if (and trigger
                                      (superchat-runtime-steer
                                       (plist-get request-context :run-id)
                                       superchat-llm--finish-check-note
                                       'finish-check))
                                 (progn
                                   (setq finish-check-fired t)
                                   (cl-incf tool-rounds)
                                   (when (and (buffer-live-p request-buffer)
                                              (fboundp 'superchat--response-region-close))
                                     (with-current-buffer request-buffer
                                       (superchat--response-region-close
                                        nil nil transcript-owner)))
                                   (setq response-parts '())
                                   (setq cumulative ""
                                         reasoning-cumulative "")
                                   (condition-case err
                                       (funcall send-request 'tool-followup)
                                     (superchat-llm-evidence-recording-error
                                      (signal (car err) (cdr err)))
                                     (error
                                      (funcall finalize
                                               (format "[llm-chat-streaming error: %s]"
                                                       (error-message-string err))
                                               'failed))))
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
                                         (t "[Empty response from llm]")))))
                         (funcall finalize
                                  "[Missing final response from llm]" 'failed)))))))))

      ;; llm.el may invoke these after the initiating dynamic binding has
      ;; unwound.  Bind View work to its object; preserve raw transport protocol.
      (cl-psetq finalize (superchat--callback-in-view request-buffer finalize finalize)
                stream-cb (superchat--callback-in-view request-buffer stream-cb stream-cb)
                response-cb (superchat--callback-in-view request-buffer response-cb response-cb)
                error-cb (superchat--callback-in-view request-buffer error-cb error-cb))

      (setq send-request
            (lambda (attempt-reason &optional recovery-wait-p)
              (unless (funcall run-active-p)
                (error "Runtime Run is no longer active; continuation refused"))
              (unless (superchat-llm-provider-attempt-reason-p
                       attempt-reason)
                (error "Unknown provider attempt reason: %s" attempt-reason))
              ;; ── Attempt checkpoint: a pause request defers this attempt
              ;; at the tool boundary.  The prelude above is idempotent-safe
              ;; to re-run on resume; the model call itself never starts
              ;; while paused.
              (if (and (boundp 'superchat--pause-requested)
                       superchat--pause-requested
                       (boundp 'superchat--pause-run-id)
                       (stringp superchat--pause-run-id)
                       (equal
                        superchat--pause-run-id
                        (superchat-llm--transport-timer-owner-run-id
                         transport-timer-owner))
                       (fboundp 'superchat--pause-boundary))
                  (progn
                    (setq superchat--pending-send-request
                          (superchat-llm--make-pending-pause
                           :run-id superchat--pause-run-id
                           :transport-owner transport-timer-owner
                           :reason attempt-reason
                           :continuation
                           (lambda (reason)
                             (funcall send-request reason recovery-wait-p))))
                    (superchat--pause-boundary
                     superchat--pause-run-id transport-timer-owner))
                (progn
              ;; A tool followup or driver-level retry opens a new Step.
              ;; Provider-internal retries never call this driver function.
              (when (and (> request-generation 0) (functionp runtime-observer))
                (dolist (material (funcall runtime-observer 'next-step attempt-reason))
                  (llm-chat-prompt-append-response
                   real-prompt (plist-get material :text) 'user))
                (unless (funcall run-active-p)
                  (error "Runtime stopped before entering the next Step")))
              (dolist (message-text
                       (and drain-steers-function
                            (funcall drain-steers-function)))
                (unless (fboundp 'llm-chat-prompt-append-response)
                  (error "llm.el prompt mutation is unavailable"))
                (llm-chat-prompt-append-response
                 real-prompt message-text 'user))
              ;; Experience is a recovery instruction for the next tool
              ;; action, not a final-answer instruction.  Clear the local
              ;; view before the tool-free final request so a model cannot
              ;; repeat `workspace_info' after the corrected read succeeded.
              (when (eq attempt-reason 'final-answer)
                (setq active-experience-view nil)
                (when (fboundp 'llm-chat-prompt-append-response)
                  (llm-chat-prompt-append-response
                   real-prompt
                   (format
                    "The tool round budget is exhausted after %d rounds (limit %d). No more tools are available in this Run. Give a final text answer that distinguishes completed work, incomplete work, and results that were not verified. Do not claim a change was applied, saved, or reloaded without supporting tool results. Explain any remaining next step. Do not call a tool or emit a tool-call block."
                    tool-rounds tool-round-limit)
                   'user)))
              (when (and (eq attempt-reason 'tool-followup)
                         (functionp runtime-observer))
                (when-let* ((view
                            (funcall runtime-observer 'before-attempt
                                     (list :attempt-reason attempt-reason))))
                  (when-let* ((text (plist-get view :prompt)))
                    (unless (fboundp 'llm-chat-prompt-append-response)
                      (error "llm.el prompt mutation is unavailable"))
                    (llm-chat-prompt-append-response real-prompt text 'user))
                  ;; A blocked View has no prompt, but it still carries the
                  ;; action gate that Authority must enforce for this round.
                  (setq active-experience-view view)))
              (setq last-model-phase 'waiting-first-token)
              (let* ((generation (1+ request-generation))
                     (_retry-before-provider-request
                      (when (> generation 1)
                        (let ((superchat-execution-scope execution-scope)
                              (superchat--tool-execution-context tool-execution-context))
                          (pcase-let
                              ((`(,next-prompt ,next-system ,next-names)
                                (superchat--llm-retry-before-provider-request
                                 runtime-observer real-prompt
                                 provider-prompt provider-system-prompt)))
                            (when (eq next-names 'none) (setq next-names nil))
                            (when (and (eq attempt-reason 'final-answer) next-names)
                              (error "Final-answer request cannot reacquire tools"))
                            (unless (equal next-names
                                           (mapcar #'llm-tool-name
                                                   (llm-chat-prompt-tools real-prompt)))
                              (when tools-prepared-p
                                (error "Request middleware cannot replace prepared tools"))
                              (let* ((selected
                                      (superchat--collect-llm-tools
                                       next-prompt next-names input-operation
                                       (list :names (or next-names 'none)
                                             :reason 'waterfall)))
                                     (wrapper
                                      (or wrap-fn
                                          (and selected
                                               (fboundp 'superchat--agent-make-wrapper)
                                               (superchat--agent-make-wrapper preset)))))
                                (superchat--llm-check-selected-tools next-names selected)
                                (when (and selected (not (functionp wrapper)))
                                  (signal 'superchat-llm-tool-wrapper-required
                                          '("Request middleware tools require a trusted wrapper")))
                                (when input-operation
                                  (let ((input (cl-remove-if-not
                                                (lambda (tool) (equal "ask_user" (llm-tool-name tool)))
                                                selected)))
                                    (unless (and (= (length input) 1)
                                                 (superchat-tools--trusted-ask-descriptor-p
                                                  (car input) input-operation))
                                      (error "Trusted human input descriptor was replaced"))))
                                (when evidence-contract
                                  (superchat--llm-validate-evidence-tools selected))
                                (let ((previous (llm-chat-prompt-tools real-prompt)))
                                  (setf (cl-struct-slot-value 'llm-chat-prompt 'tools real-prompt)
                                        (mapcar
                                         (lambda (tool)
                                           ;; Retained names retain their exact installation.
                                           ;; Only genuinely new tools acquire a wrapper.
                                           (or (cl-find (llm-tool-name tool) previous
                                                        :key #'llm-tool-name :test #'equal)
                                               (car (superchat--llm-bind-tools-to-execution-scope
                                                     (list (funcall wrapper tool)) execution-scope))))
                                         selected)))
                                (setq wrap-fn wrapper
                                      tool-decision-reason 'waterfall)))
                            (setq provider-prompt next-prompt
                                  provider-system-prompt next-system)))))
                     ;; A retry listener may synchronously cancel or retire the
                     ;; Run.  Do not even publish a new attempt after it did;
                     ;; the provider dispatch gate below remains the final
                     ;; protection across later public callbacks.
                     (_retry-still-active
                      (unless (funcall run-active-p)
                        (error "Runtime Run is no longer active; continuation refused")))
                     (attempt-prompt
                      (if evidence-contract
                          (copy-tree real-prompt t)
                        real-prompt))
                     (_reasoning
                      (superchat--llm-prepare-reasoning
                       effective-backend attempt-prompt))
                     ;; §5.1: capabilities are resolved per attempt against
                     ;; ITS actual tool surface, read directly off
                     ;; ATTEMPT-PROMPT rather than inferred from
                     ;; ATTEMPT-REASON.  A round-limit `final-answer'
                     ;; attempt has already had its tools removed in place
                     ;; by `superchat--llm-disable-prompt-tools' (just
                     ;; above, mutating REAL-PROMPT, which ATTEMPT-PROMPT
                     ;; aliases or copies); any future attempt policy that
                     ;; filters tools for some other reason is covered the
                     ;; same way, with no risk of the capability source and
                     ;; transport mode silently going stale against a
                     ;; reason-based proxy.  The streaming / streaming-tool-
                     ;; use / source resolved here shadow the request-wide
                     ;; bindings above for the rest of this attempt.
                     (attempt-has-tools
                      (and (llm-chat-prompt-tools attempt-prompt) t))
                     (attempt-tool-names
                      (and attempt-has-tools
                           (mapcar #'llm-tool-name
                                   (llm-chat-prompt-tools attempt-prompt))))
                     (streaming-capabilities
                      (superchat-llm--resolve-streaming
                       effective-backend request-model attempt-has-tools))
                     (streaming-request-p
                      (and superchat-llm-streaming
                           (if attempt-has-tools
                               (and (plist-get streaming-capabilities :streaming)
                                    (plist-get streaming-capabilities
                                               :streaming-tool-use))
                             (plist-get streaming-capabilities :streaming))))
                     (attempt-model request-model)
                     (context-limit (plist-get context-capability :limit))
                     (context-limit-source
                      (plist-get context-capability :source))
                     (context-used
                      (let ((superchat--llm-preflight-streaming-p
                             streaming-request-p))
                        (superchat--llm-estimate-context-tokens
                         effective-backend attempt-prompt)))
                     (prompt-max-tokens
                      (and (fboundp 'llm-chat-prompt-max-tokens)
                           (ignore-errors
                             (llm-chat-prompt-max-tokens attempt-prompt))))
                     (output-reserve
                      (if (and (numberp prompt-max-tokens)
                               (> prompt-max-tokens 0))
                          prompt-max-tokens
                        superchat-llm-output-token-reserve))
                     (details
                      (append
                       (list :attempt generation
                             :attempt-reason attempt-reason
                             :model attempt-model
                             :model-phase 'waiting-first-token
                             :context-used context-used
                             :output-reserve output-reserve
                             :context-limit context-limit
                             :streaming
                             (plist-get streaming-capabilities :streaming)
                             :streaming-tool-use
                             (plist-get streaming-capabilities
                                        :streaming-tool-use)
                             :capability-source
                             (plist-get streaming-capabilities :source)
                             :streaming-enabled
                             (and superchat-llm-streaming t)
                             ;; Contract §4.2's Run-status record
                             ;; requirement: which names this attempt's
                             ;; actual prompt carries, and why (`explicit',
                             ;; `stage', or `baseline' -- see
                             ;; `superchat-select-tools-decision').
                             ;; `:runtime-selected-tools' is nil, not
                             ;; omitted, when this attempt has no tools, so
                             ;; a caller can tell "no tools this attempt"
                             ;; from "this key was never recorded."
                             :runtime-selected-tools attempt-tool-names
                             :runtime-tool-decision tool-decision-reason)
                       (when context-limit-source
                         (list :context-limit-source
                               context-limit-source))))
                     (over-budget
                      (and (numberp context-used)
                           (numberp context-limit)
                           (numberp output-reserve)
                           ;; llm.el uses a restrictive fallback for unknown
                           ;; models.  A limit no larger than our reserve has
                           ;; no useful preflight signal; let the provider's
                           ;; exact limit and recovery path decide instead.
                           (> context-limit output-reserve)
                           (> (+ context-used output-reserve)
                              context-limit)))
                     (blocked nil)
                     snapshot-id)
                (setq last-attempt-details details)
                (when over-budget
                  (setq blocked t)
                  (when (functionp runtime-observer)
                    (funcall runtime-observer
                             'context-budget-exceeded details))
                  (let ((message
                         (format
                          "[文件过大：估算输入 %s tokens + 输出预留 %s > 模型上下文为 %s tokens；请求未发送，建议缩小或换模型。]"
                          context-used output-reserve context-limit)))
                    (unless (and (functionp recover-context)
                                 (funcall recover-context
                                          'threshold attempt-reason
                                          message details))
                      (funcall finalize message 'failed))))
                (unless (or blocked completed)
                  (setq request-generation generation)
                  (when (functionp runtime-observer)
                    (funcall runtime-observer
                             'attempt-started details))
                  (when (and evidence-contract
                           (functionp runtime-observer))
                  (condition-case err
                      (let* ((_ (unless
                                    (fboundp 'llm-provider-chat-request)
                                  (error
                                   "llm.el request serializer is unavailable")))
                             ;; Force llm.el's provider :before methods to
                             ;; materialize actual default options on this
                             ;; isolated attempt before the snapshot.
                             (_ (llm-provider-chat-request
                                 effective-backend attempt-prompt
                                 streaming-request-p))
                             (provider
                              (superchat--provider-name effective-backend))
                             (model
                              (or (superchat--provider-chat-model
                                   effective-backend)
                                  target-model
                                  "unknown"))
                             (prompt-tools
                              (progn
                                (unless (fboundp 'llm-chat-prompt-tools)
                                  (error
                                   "llm.el prepared-tool accessor is unavailable"))
                                (llm-chat-prompt-tools attempt-prompt)))
                             (current-tools
                              (and prompt-tools
                                   (copy-tree evidence-base-tools t)))
                             (prompt-tool-records
                              (superchat--llm-evidence-tool-records
                               prompt-tools))
                             (_
                              (unless
                                  (equal
                                   prompt-tool-records
                                   (superchat--llm-evidence-tool-records
                                    current-tools))
                                (error
                                 "Prepared and provider tool schemas diverged")))
                             (_ (unless (fboundp 'llm-chat-prompt-context)
                                  (error
                                   "llm.el system-prompt accessor is unavailable")))
                             (prepared-envelope
                              (list
                               :format "superchat-prepared-envelope-v0"
                               :attempt-reason (symbol-name attempt-reason)
                               :system-prompt
                               (or (llm-chat-prompt-context attempt-prompt) "")
                               :messages
                               (superchat--llm-evidence-messages attempt-prompt)
                               :tools
                               prompt-tool-records
                               :context-files (vconcat context-files)
                               :view-envelope
                               (let ((envelope
                                      (plist-get evidence-context
                                                 :context-envelope)))
                                 (list
                                  :contract (plist-get envelope :contract)
                                  :status (plist-get envelope :status)
                                  :receipt
                                  (copy-tree (plist-get envelope :receipt))
                                  :disclosure
                                  (copy-tree
                                   (plist-get envelope :disclosure))
                                  :experience-action-policy
                                  (copy-tree
                                   (or
                                    (plist-get envelope
                                               :experience-action-policy)
                                    (plist-get active-experience-view
                                               :experience-action-policy))
                                   t)
                                  :experience-view
                                  (copy-tree active-experience-view t)))
                               :options
                               (superchat--llm-evidence-options
                                attempt-prompt streaming-request-p
                                multi-output))))
                        ;; This is the structured Superchat-prepared envelope,
                        ;; not a claim about provider-specific HTTP wire bytes.
                        ;; Tape owns exact encoding, redaction refusal, size,
                        ;; and hash.
                        (setq
                         snapshot-id
                         (funcall
                          runtime-observer 'decision-snapshot
                          (list
                           :attempt generation
                           :attempt-reason (symbol-name attempt-reason)
                           :provider provider
                           :model model
                           :prepared-envelope prepared-envelope)))
                        (unless (and (stringp snapshot-id)
                                     (not (string-empty-p snapshot-id)))
                          (error "Runtime did not return a snapshot event ID"))
                        (superchat--llm-set-prompt-tools
                         attempt-prompt
                         (superchat--llm-attempt-tools
                          attempt-prompt current-tools
                          (list
                           :evidence-contract evidence-contract
                           :evidence-attempt generation
                           :decision-snapshot-event-id snapshot-id
                           :attempt-active-p
                           (lambda ()
                             (and (= generation request-generation)
                                  (not completed)
                                  (funcall run-active-p)))
                           :stale-observer
                           (lambda (origin result)
                             (let ((superchat--llm-callback-attempt generation))
                               (funcall
                                record-first-output
                                (list
                                 :kind "stale-tool-call-blocked"
                                 :tool-name
                                 (plist-get origin :tool-call-name)
                                 :call-id
                                 (plist-get origin :tool-call-id)
                                 :arguments
                                 (copy-tree
                                  (plist-get origin :tool-call-arguments) t)
                                 :result result))))
                           :audit-failure-observer
                           (lambda (value)
                             (let ((superchat--llm-callback-attempt generation))
                               (ignore-errors
                                 (funcall
                                  record-first-output
                                  (list
                                   :kind "tool-origin-invalid"
                                   :tool-name
                                   (plist-get value :tool-name))))
                               (when (functionp runtime-observer)
                                 (funcall
                                 runtime-observer
                                  'audit-failure value)))))
                          execution-scope)))
                    (superchat-llm-evidence-recording-error
                     (signal (car err) (cdr err)))
                    (error
                     (signal 'superchat-llm-evidence-recording-error
                             (list (error-message-string err))))))
                  (unless (funcall run-active-p)
                    (error
                     "Runtime Run is no longer active; provider request not sent"))
                  (let* ((attempt-stream-cb
                        (lambda (chunk)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (and (= generation request-generation)
                                       (funcall run-active-p))
                              (funcall stream-cb chunk)))))
                       (attempt-response-cb
                        (lambda (response)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (and (= generation request-generation)
                                       (funcall run-active-p))
                              (funcall cancel-wait-timeout)
                              (when (funcall callback-active-p)
                                (when evidence-contract
                                  (setq real-prompt attempt-prompt))
                                (funcall response-cb response))))))
                       (attempt-error-cb
                        (lambda (type &rest more)
                          (let ((superchat--llm-callback-attempt generation))
                            (when (and (= generation request-generation)
                                       (funcall run-active-p))
                              (funcall cancel-wait-timeout)
                              (when (funcall callback-active-p)
                                (when evidence-contract
                                  (setq real-prompt attempt-prompt))
                                (apply error-cb type more))))))
                     (request
                      (let ((epoch nil) (recovery-wait-epoch nil) (entered nil))
                        (unwind-protect
                            (superchat--llm-dispatch-input-batch
                             attempt-prompt input-tool
                             (lambda ()
                               ;; Batch preparation can reenter plugin teardown.
                               (unless (funcall run-active-p)
                                 (error "Runtime Run stopped before provider dispatch"))
                               (setq entered t)
                                ;; Request evidence (M1): record what this attempt asks the
                                ;; provider, before the bytes leave.  This is the one place the
                                ;; provider-visible system text and tool schemas exist together,
                                ;; and it runs on every attempt, so a rewrite and a retry each
                                ;; get their own message list.  A recording failure is swallowed
                                ;; by the recorder and never breaks the request.
                                ;;
                                ;; The system text comes from this layer's own
                                ;; PROVIDER-SYSTEM-PROMPT, not from ATTEMPT-PROMPT:
                                ;; llm.el's request builder clears the prompt's `:context'
                                ;; slot after folding it into a system interaction, so from
                                ;; the second attempt on the prompt no longer states the
                                ;; system text it is sent with.
                                (when (functionp runtime-observer)
                                  (setq request-context
                                        (plist-put request-context :step
                                                   (funcall runtime-observer 'step-id nil))))
                                (superchat--llm-record-request-evidence
                                 request-context attempt-prompt
                                 provider-system-prompt)
                               ;; Preparation and pause time are not provider
                               ;; silence.  Start only at this permitted send.
                               (setq adjusted-timeout
                                     (superchat-llm--adaptive-response-timeout
                                      base-timeout context-used)
                                     tool-progress-cumulative nil
                                     tool-marker-cumulative nil)
                               (funcall arm-timeout generation)
                               ;; Timer activation and activity hooks can
                               ;; reenter cancellation or a newer attempt.
                               (unless (and (not completed)
                                            (= generation request-generation)
                                            (funcall run-active-p))
                                 (error "Runtime Run stopped before provider dispatch"))
                               (if streaming-request-p
                                   (llm-chat-streaming effective-backend attempt-prompt
                                                       attempt-stream-cb attempt-response-cb
                                                       attempt-error-cb multi-output)
                                 (llm-chat-async effective-backend attempt-prompt
                                                 attempt-response-cb attempt-error-cb multi-output)))
                             (when input-tool
                               (lambda ()
                                 (when (and (funcall run-active-p)
                                            (funcall runtime-observer 'before-input-dispatch nil))
                                   (when (eq 'paused (superchat-llm--transport-timer-owner-hard-state
                                                      transport-timer-owner))
                                     ;; Set the expected epoch before timer activation, which
                                     ;; can reenter.  Rollback never touches a newer Q2 epoch.
                                     (let ((remaining (superchat-llm--transport-timer-owner-hard-remaining
                                                       transport-timer-owner)))
                                       (if (<= remaining 0)
                                           (setf (superchat-llm--transport-timer-owner-hard-state
                                                  transport-timer-owner) 'exhausted)
                                         (setq epoch (make-symbol "input-resume"))
                                         (superchat-llm--input-arm-hard
                                          transport-timer-owner remaining
                                          (superchat-llm--transport-timer-owner-hard-function
                                           transport-timer-owner) epoch))))
                                   ;; Retain the recovery epoch across public gates.
                                   ;; Inactivity is armed only at actual dispatch.
                                   (when (and recovery-wait-p (null epoch)
                                              (eq 'running (superchat-llm--transport-timer-owner-hard-state
                                                            transport-timer-owner)))
                                     (setq recovery-wait-epoch
                                           (superchat-llm--transport-timer-owner-clock-epoch
                                            transport-timer-owner)))
                                   (when (and (eq 'running (superchat-llm--transport-timer-owner-hard-state
                                                            transport-timer-owner))
                                              (<= (superchat-llm--transport-timer-owner-hard-deadline
                                                   transport-timer-owner) (float-time)))
                                     (setf (superchat-llm--transport-timer-owner-hard-state
                                            transport-timer-owner) 'exhausted))
                                   (if (eq 'exhausted (superchat-llm--transport-timer-owner-hard-state
                                                       transport-timer-owner))
                                       (progn (funcall runtime-observer 'input-clock-exhausted nil) nil)
                                     (and (memq (superchat-llm--transport-timer-owner-hard-state
                                                 transport-timer-owner) '(running disabled))
                                          (or (null epoch)
                                              (eq epoch (superchat-llm--transport-timer-owner-clock-epoch
                                                         transport-timer-owner)))
                                          (or (null recovery-wait-epoch)
                                              (eq recovery-wait-epoch
                                                  (superchat-llm--transport-timer-owner-clock-epoch
                                                   transport-timer-owner)))
                                          (= generation request-generation)
                                          (funcall run-active-p)
                                          (funcall runtime-observer 'before-input-dispatch nil)
                                          (progn
                                            (setf (superchat-llm--transport-timer-owner-wait-generation
                                                   transport-timer-owner) generation)
                                            t)))))))
                          ;; A wait-only recovery acquisition does not own the hard
                          ;; clock.  On pre-entry unwind cancel only its exact epoch
                          ;; and generation; Q2 and terminal cleanup always win.
                          (when (and recovery-wait-epoch (not entered)
                                     (eq recovery-wait-epoch
                                         (superchat-llm--transport-timer-owner-clock-epoch
                                          transport-timer-owner))
                                     (eql generation
                                          (superchat-llm--transport-timer-owner-wait-generation
                                           transport-timer-owner)))
                            (superchat-llm--transport-timer-owner-cancel-wait transport-timer-owner))
                          (when (and epoch (not entered)
                                     (eq epoch (superchat-llm--transport-timer-owner-clock-epoch
                                                transport-timer-owner))
                                     (eq 'running (superchat-llm--transport-timer-owner-hard-state
                                                   transport-timer-owner)))
                            (superchat-llm--input-freeze-clock transport-timer-owner)
                            (when (eq 'exhausted (superchat-llm--transport-timer-owner-hard-state
                                                  transport-timer-owner))
                              (funcall runtime-observer 'input-clock-exhausted nil)))))))
                (when (and (= generation request-generation)
                           (functionp runtime-observer))
                  (funcall runtime-observer 'request request))
                (superchat--llm-watch-plz-request
                 request
                 (lambda ()
                   (and (not completed)
                        (funcall run-active-p))))
                    request)))))))

      (setq recover-context
            (lambda (reason attempt-reason failure-message details)
              (when (and (not context-recovery-attempted)
                         (not completed)
                         (funcall run-active-p)
                         (buffer-live-p request-buffer)
                         (stringp context-prefix)
                         (not (string-empty-p context-prefix))
                         (superchat-event-listeners nil 'context-budget-exceeded))
                (setq context-recovery-attempted t
                      context-recovery-active t)
                (funcall cancel-wait-timeout)
                (when (functionp runtime-observer)
                  (funcall runtime-observer
                           'context-recovery-started
                           (append details (list :reason reason))))
                (let* ((view-dead
                        (lambda (&rest _args)
                          (when context-recovery-active
                            (setq context-recovery-active nil)
                            (when (and (not completed)
                                       (funcall run-active-p))
                              (funcall
                               finalize
                               "[Context recovery stopped because the originating View is no longer live]"
                               'failed)))))
                       (resume-body
                        (lambda (new-context)
                          ;; `error' anywhere below is swallowed (FINALIZE is
                          ;; the report); a `quit' still finalizes this Run
                          ;; the same way and is then re-signalled once that
                          ;; durable work is done (contract §3.3's terminal-
                          ;; `quit' rule).  One shared QUIT-CONDITION and one
                          ;; outer `unwind-protect' cover both nested
                          ;; `condition-case' forms below so the re-signal
                          ;; happens exactly once, from whichever one a quit
                          ;; actually reached.
                          (let (quit-condition)
                            (unwind-protect
                                (when context-recovery-active
                                  (setq context-recovery-active nil)
                                  (when (and (not completed)
                                             (funcall run-active-p))
                                    (if (not (and (stringp new-context)
                                                  (not (string-empty-p new-context))))
                                        (funcall
                                         finalize
                                         "[Context recovery could not rebuild the provider prompt]"
                                         'failed)
                                      ;; Rebuilding the provider prompt must never
                                      ;; leave this Run stalled: a signal here (the
                                      ;; oversized-context regexp defect this guards
                                      ;; against, or any future one) is caught and
                                      ;; finalized instead of propagating up into the
                                      ;; compaction Run's delivery path, where it was
                                      ;; previously swallowed as an unrelated
                                      ;; `delivery.failed' warning on that Run while
                                      ;; this Run never finalized at all.
                                      (condition-case rebuild-err
                                          (if (superchat--llm-replace-prompt-context
                                               real-prompt context-prefix new-context)
                                              (progn
                                                (setq context-prefix new-context
                                                      response-parts '()
                                                      cumulative ""
                                                      reasoning-cumulative "")
                                                (condition-case err
                                                    (funcall send-request attempt-reason (and input-tool t))
                                                  (error
                                                   (funcall
                                                    finalize
                                                    (format
                                                     "[Context recovery retry failed: %s]"
                                                     (error-message-string err))
                                                    'failed))
                                                  (quit
                                                   (setq quit-condition err)
                                                   (funcall
                                                    finalize
                                                    (format
                                                     "[Context recovery retry failed: %s]"
                                                     (error-message-string err))
                                                    'failed))))
                                            (funcall
                                             finalize
                                             "[Context recovery could not rebuild the provider prompt]"
                                             'failed))
                                        (error
                                         (funcall
                                          finalize
                                          (format
                                           "[Context recovery could not rebuild the provider prompt: %s]"
                                           (error-message-string rebuild-err))
                                          'failed))
                                        (quit
                                         (setq quit-condition rebuild-err)
                                         (funcall
                                          finalize
                                          (format
                                           "[Context recovery could not rebuild the provider prompt: %s]"
                                           (error-message-string rebuild-err))
                                          'failed))))))
                              (when quit-condition
                                (signal (car quit-condition) (cdr quit-condition)))))))
                       (resume
                        (superchat--callback-in-view
                         request-buffer resume-body view-dead))
                       (fail-body
                        (lambda (&optional message-text)
                          (when context-recovery-active
                            (setq context-recovery-active nil)
                            (when (and (not completed)
                                       (funcall run-active-p))
                              (funcall finalize
                                       (or message-text failure-message)
                                       'failed)))))
                       (fail
                        (superchat--callback-in-view
                         request-buffer fail-body view-dead))
                       (recovery-run-id
                        (superchat-llm--transport-timer-owner-run-id
                         transport-timer-owner))
                       (recovery-session-id
                        (and recovery-run-id
                             (fboundp 'superchat-runtime-status)
                             (plist-get (superchat-runtime-status recovery-run-id)
                                        :session-id)))
                       (hook-quit-condition nil)
                       (handled
                        ;; S19-fix-4: a hook implementation (typically
                        ;; `superchat-handoff--context-budget-exceeded')
                        ;; may itself signal `quit' from its own
                        ;; synchronous start, already finalized via FAIL
                        ;; by the time it reaches here (contract §3.3's
                        ;; terminal-`quit' rule).  `error' is swallowed
                        ;; and logged exactly as before; `quit' is
                        ;; logged the same way and both are treated as
                        ;; HANDLED = nil, so the existing `unless handled'
                        ;; bookkeeping just below runs uniformly for
                        ;; either -- the quit's own condition is only
                        ;; captured here, for the `unwind-protect' below
                        ;; to re-signal once that bookkeeping is done.
                        (condition-case hook-error
                            (superchat-emit
                             nil 'context-budget-exceeded
                             (append details
                                     (list :reason reason
                                           :resume-function resume
                                           :fail-function fail
                                           :session-id recovery-session-id)))
                          (error
                           (message "Context recovery hook failed: %s"
                                    (error-message-string hook-error))
                           nil)
                          (quit
                           (setq hook-quit-condition hook-error)
                           (message "Context recovery hook failed: %s"
                                    (error-message-string hook-error))
                           nil))))
                  (unwind-protect
                      (progn
                        (unless handled
                          (setq context-recovery-active nil)
                          (when (functionp runtime-observer)
                            (funcall runtime-observer
                                     'context-recovery-ended
                                     (append details (list :reason reason)))))
                        handled)
                    (when hook-quit-condition
                      (signal (car hook-quit-condition) (cdr hook-quit-condition))))))))

      (condition-case err
          (funcall send-request 'initial)
        (superchat-llm-evidence-recording-error
         (signal (car err) (cdr err)))
        (error
         (if evidence-contract
             (signal 'superchat-llm-evidence-recording-error
                     (list (error-message-string err)))
           (funcall finalize
                    (format "[llm-chat-streaming error: %s]"
                            (error-message-string err))
                    'failed)))))))

(defun superchat-llm-configure (&rest adapters)
  "Install optional distribution ADAPTERS for the transport port.

Recognized keys are `:response-mode-function' (TOOLS TOOLS-KNOWN-P),
`:response-mode-indicator-function' (MODE), `:adjusted-timeout-function'
(MODE), `:stage-function' (LABEL), `:ttft-function' (ELAPSED),
`:nonpartial-latency-function' (ELAPSED), and `:activity-function'
(TRANSPORT-TIMER-OWNER).  Values must be functions or nil.
Calling this function replaces the complete adapter set."
  (unless (zerop (% (length adapters) 2))
    (error "Superchat LLM adapters must be an even key/value plist: %S"
           adapters))
  (let ((remaining adapters)
        seen)
    (while remaining
      (let ((key (pop remaining))
            (value (pop remaining)))
        (unless (memq key superchat-llm--adapter-keys)
          (error "Unknown Superchat LLM adapter key: %S" key))
        (when (memq key seen)
          (error "Duplicate Superchat LLM adapter key: %S" key))
        (push key seen)
        (unless (or (null value) (functionp value))
          (error "Invalid Superchat LLM adapter %s: %S" key value)))))
  (setq superchat-llm--distribution-adapters (copy-sequence adapters)))

(defun superchat-llm--adapter (key)
  "Return the configured distribution adapter for KEY, or nil."
  (plist-get superchat-llm--distribution-adapters key))

(defun superchat-llm--response-mode (tools)
  "Return the response mode for already-collected TOOLS."
  (let ((adapter (superchat-llm--adapter :response-mode-function)))
    (if adapter
        (funcall adapter tools t)
      (cond
       ((and tools
             (fboundp 'llm-ollama-p)
             (ignore-errors (llm-ollama-p superchat-llm-backend)))
        'non-streaming)
       (tools 'tool-calling)
       (t 'streaming)))))

(defun superchat-llm--indicate-response-mode (mode)
  "Tell the optional distribution adapter about response MODE."
  (let ((adapter
         (superchat-llm--adapter :response-mode-indicator-function)))
    (when adapter
      (funcall adapter mode))))

(defun superchat-llm--adjusted-timeout (mode)
  "Return the configured timeout for response MODE."
  (let ((adapter (superchat-llm--adapter :adjusted-timeout-function)))
    (if adapter
        (funcall adapter mode)
      (when superchat-response-timeout
        (if (memq mode '(non-streaming tool-calling))
            (floor (* superchat-response-timeout
                      superchat-tool-timeout-multiplier))
          superchat-response-timeout)))))

(defun superchat-llm--report-stage (label)
  "Report transport stage LABEL through the optional distribution adapter."
  (let ((adapter (superchat-llm--adapter :stage-function)))
    (when adapter
      (funcall adapter label))))

(defun superchat-llm--report-ttft (elapsed)
  "Report ELAPSED time-to-first-token through the optional adapter."
  (let ((adapter (superchat-llm--adapter :ttft-function)))
    (when adapter
      (funcall adapter elapsed))))

(defun superchat-llm--report-nonpartial-latency (elapsed)
  "Report callback-only end-to-end ELAPSED time through the optional adapter."
  (let ((adapter
         (superchat-llm--adapter :nonpartial-latency-function)))
    (when adapter
      (funcall adapter elapsed))))

(defun superchat-llm--notify-activity (transport-timer-owner)
  "Notify the optional adapter of activity owned by TRANSPORT-TIMER-OWNER."
  (let ((adapter (superchat-llm--adapter :activity-function)))
    (when adapter
      (funcall adapter transport-timer-owner))))

;; ── Plugin (spec §3): the default provider defined next to its code ──

(provide 'superchat-llm)
;;; superchat-llm.el ends here
