;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;; Version: 1.3.2
;; Package-Requires: ((emacs "29.1") (llm "0.31.1"))

;; Author: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/superchat
;; Keywords: ai, chat, llm
;; License: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'llm nil t)
;; Kernel and the standard chat distribution load eagerly.
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-tape)
(require 'superchat-authority)
(require 'superchat-runtime)
(require 'superchat-tape-view)
(require 'superchat-memory)
(require 'superchat-tools)
(require 'superchat-executor)
(require 'superchat-skills)
(require 'superchat-preset)
(require 'superchat-parser)
(require 'superchat-models)
(require 'superchat-save)
(require 'superchat-mcp)
(require 'superchat-render)
(require 'superchat-llm)
(require 'superchat-extension)
(require 'superchat-dispatcher)
(require 'superchat-agent-loop)
(require 'superchat-workspace)
(require 'superchat-synthesis)
(require 'superchat-prompt-hooks)

;; Integration packages load on first use.  These explicit autoloads keep
;; their entry points reachable even without package.el-generated autoload
;; files; each package registers into the kernel seams when it loads.
(autoload 'superchat-cowork-telega
  "plugins/superchat-cowork-telega/superchat-cowork-telega" nil t)
(autoload 'superchat-rewrite-region "superchat-rewrite" nil t)
(autoload 'superchat-rewrite-apply-code-block-at-point "superchat-rewrite" nil t)
(autoload 'superchat-claim-capture "superchat-claims" nil t)
(autoload 'superchat-claim-correct "superchat-claims" nil t)
(autoload 'superchat-experience-record-evaluation "superchat-experience")
(autoload 'superchat-experience-record-relation "superchat-experience")
(autoload 'superchat-experience-applicability-graph "superchat-experience")
(autoload 'superchat-experience-assemble-recovery-view "superchat-experience")
(autoload 'superchat-experience--cmd-review "superchat-experience")
(autoload 'superchat--cmd-compact "superchat-compact")
(autoload 'superchat--cmd-expand "superchat-compact")
(autoload 'superchat--cmd-workflow "superchat-workflow")
(autoload 'superchat-research-workflow "superchat-workflow" nil t)
(autoload 'superchat-research-workflow-resume "superchat-workflow" nil t)
(autoload 'superchat-research-workflow-status "superchat-workflow" nil t)
(autoload 'superchat--cmd-subagent "superchat-subagent")
(autoload 'superchat--cmd-cancel-agent "superchat-subagent")
(autoload 'superchat--cmd-agents "superchat-agents-panel")
(autoload 'superchat-agents-panel-show "superchat-agents-panel" nil t)
(autoload 'superchat-sessions "superchat-tape-browser" nil t)
(autoload 'superchat-session-history "superchat-tape-browser" nil t)
(autoload 'superchat-emacsclient-list-runs "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-run-status "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-session-history "superchat-emacsclient" nil t)
(autoload 'superchat-emacsclient-run-cancel "superchat-emacsclient" nil t)

(defconst superchat-version "1.3.2"
  "Current Superchat package version.")

(declare-function superchat-memory-compose-title "superchat-memory" (content))
(declare-function superchat-memory-capture-explicit "superchat-memory" (content &optional title))
(declare-function superchat-memory-capture-conversation "superchat-memory" (content &rest options))
(declare-function superchat-memory-auto-capture "superchat-memory" (exchange))
(declare-function superchat-memory-retrieve "superchat-memory" (query-string))
(declare-function superchat-memory-summarize-session-history "superchat-memory" (history-content))

(declare-function superchat-view-search "superchat-tape-view" (query &optional session-id limit))
(declare-function superchat-view-row-to-memory-plist "superchat-tape-view" (row))
(declare-function superchat-view-entries "superchat-tape-view" (session-id &optional since-id kinds limit))
(declare-function superchat-view-session-history "superchat-tape-view" (session-id &optional max-id seen))
(declare-function superchat-view-session-ids "superchat-tape-view" ())
(declare-function superchat-view-session-configuration
                  "superchat-tape-view" (session-id))
(declare-function superchat-runtime-reconcile-session
                  "superchat-runtime" (session-id))
(declare-function superchat-sessions "superchat-tape-browser" ())
(declare-function superchat-authority-policy-known-p
                  "superchat-authority" (policy))

;; Declare llm functions if available.
;; The real llm.el v0.7+ API is:
;;   (make-llm-openai  :key ... :chat-model ...)
;;   llm-chat-streaming: provider prompt partial-cb response-cb error-cb &optional multi-output
;;   (llm-make-tool    :function ... :name ... :description ... :args ... :async ...)
;;   (llm-make-chat-prompt text :tools ... :context ... :temperature ...)
;;   (llm-name         provider)                    ; generic provider name
;;   (llm-models       provider)                    ; generic model list
(declare-function make-llm-openai "llm-openai" (&rest args))
(declare-function make-llm-claude "llm-claude" (&rest args))
(declare-function make-llm-ollama "llm-ollama" (&rest args))
(declare-function llm-openai-chat-model "llm-openai" (provider))
(declare-function llm-chat-streaming "llm" (provider prompt partial-cb response-cb error-cb &optional multi-output))
(declare-function llm-chat-streaming-to-point "llm" (provider prompt buffer point finish-cb &optional multi-output))
(declare-function llm-make-tool "llm" (&rest args))
(declare-function superchat--image-attachment-info "superchat-llm" (file))
(declare-function llm-make-chat-prompt "llm" (content &rest args))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-name "llm" (provider))
(declare-function llm-models "llm" (provider))
(declare-function llm-cancel-request "llm" (request))

;; Declare MCP functions if available
;; Note: mcp-hub-get-all-tool signature varies between versions
(declare-function mcp-hub-get-all-tool "mcp-hub" (asyncp categoryp errorHandle))
(declare-function mcp-hub-start-all-server "mcp-hub" (&optional callback servers syncp))
(declare-function mcp-hub "mcp-hub" (&optional force-refresh))

(declare-function superchat-mcp-check-all-health "superchat-mcp" ())
(declare-function superchat-mcp-start-server "superchat-mcp" (server-name))
(declare-function superchat-mcp-stop-server "superchat-mcp" (server-name))

(defgroup superchat nil
  "Configuration for superchat, a standalone AI chat client."
  :group 'external)

(defcustom superchat-closure-shadow-enabled nil
  "When non-nil, compare terminal Runtime state with Tape-derived Closure.
Shadow reports are observational: they never permit, reject, or rewrite a
Runtime transition.  Consumers may inspect reports through
`superchat-closure-shadow-functions' or
`superchat-runtime-closure-observations'."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-closure-terminal-guard-enabled t
  "When non-nil, require Closure proof before writing a Run terminal seal.
The Guard replays the current Session after `response.completed' when one is
required.  An open, unknown, or failed judgment prevents the requested seal
and leaves the process-local Run `audit-incomplete'.  Guard decisions are
retained by `superchat-runtime-closure-observations'."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-buffer-name "*superchat*"
  "The name of the buffer for the chat view."
  :type 'string
  :group 'superchat)

(defcustom superchat-lang "English"
  "The language used in chat view."
  :type 'string
  :group 'superchat)

(defcustom superchat-data-directory
  (expand-file-name "superchat/" user-emacs-directory)
  "Directory for saving chat conversations and commands."
  :type 'directory
  :group 'superchat)

(defun superchat--save-directory ()
  "Return the directory for saving Chat View conversations."
  (expand-file-name "chat-notes/" superchat-data-directory))

(defun superchat--command-dir ()
  "Return the directory for storing all custom command prompt files."
  (expand-file-name "command/" superchat-data-directory))

(defun superchat--ensure-directories ()
  "Ensure that the necessary directories exist.
Also initializes the SQLite database (tape + memory storage)."
  (let ((data-dir superchat-data-directory))
    (unless (file-directory-p data-dir)
      (make-directory data-dir t)))
  (let ((save-dir (superchat--save-directory)))
    (unless (file-directory-p save-dir)
      (make-directory save-dir t)))
  (let ((command-dir (superchat--command-dir)))
    (unless (file-directory-p command-dir)
      (make-directory command-dir t)))
  ;; Initialize SQLite database (lazy: schema created on first open)
  (when (fboundp 'superchat-db-open)
    (ignore-errors (superchat-db-open))))

(defcustom superchat-prompt-file-extensions
  '("prompt" "org" "txt" "md")
  "List of file extensions that are considered valid prompt files."
  :type '(repeat string)
  :group 'superchat)

(defcustom superchat-default-directories nil
  "List of default directories for file selection.
When set, users can choose files from these directories when adding references."
  :type '(repeat directory)
  :group 'superchat)

(defcustom superchat-default-file-extensions
  '("org" "md" "txt" "webp" "png" "jpg" "jpeg")
  "Allowed file extensions when listing files from default directories.
Only files with these extensions are shown in the file picker."
  :type '(repeat string)
  :group 'superchat)

(defcustom superchat-general-answer-prompt
  (string-join
   '("You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: $input")
   "\n")
  "The prompt template used for general questions."
  :type 'string
  :group 'superchat)

(defcustom superchat-display-single-window t
  "If non-nil, make the superchat window the only one in its frame."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-context-message-count nil
  "Maximum number of recent conversation messages to include when building prompts.
Set to nil to use the finite Context View default of 50 messages.
Set to 0 to disable including prior chat messages."
  :type '(choice (const :tag "Context View default" nil)
                 (natnum :tag "Messages"))
  :group 'superchat)

(defcustom superchat-context-max-chars 4000
  "Maximum rendered characters of recent conversation included in prompts.
Raw facts are included whole; an oversized newest fact is disclosed but not
forced into the prompt.  Set to nil to use the finite Context View default of
4000 characters."
  :type '(choice (const :tag "Context View default" nil)
                 (natnum :tag "Characters"))
  :group 'superchat)

(defcustom superchat-conversation-history-limit 50
  "Maximum number of conversation messages to retain in memory.
Set to nil to keep all messages."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'superchat)

;; --- Inline File Context Options ---
(defcustom superchat-inline-file-content t
  "Whether to inline file contents into the prompt in addition to gptel context."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-inline-text-extensions
  '("org" "md" "markdown" "txt"
    "el" "lisp" "cl" "clj" "cljc" "cljs"
    "py" "js" "jsx" "ts" "tsx"
    "c" "h" "cc" "cpp" "hpp" "rs" "go" "java"
    "rb" "php" "swift" "kt" "kts"
    "sh" "bash" "zsh" "fish"
    "json" "jsonl" "yaml" "yml" "toml" "ini" "cfg" "conf"
    "xml" "html" "htm" "css" "scss" "sass" "sql" "lua" "r")
  "File extensions whose contents may be inlined into a prompt.

Values are lowercase extensions without a leading dot.  Known binary and
image extensions are always rejected here so image references can continue
through the native attachment path.  Files without an extension are accepted
only when their leading bytes look like text."
  :type '(repeat string)
  :group 'superchat)

(defcustom superchat-inline-metadata-threshold 32768
  "Above this many bytes, a #file reference is NOT inlined into the prompt.

Instead the model receives constant-size metadata — path, size, line
count, a short head — plus instructions to ask for an explicit #file
line range.  This keeps the root context small regardless of file size.
Set to nil to always inline up to `superchat-inline-max-bytes'."
  :type '(choice (const :tag "Always inline" nil) natnum)
  :group 'superchat)

(defcustom superchat-inline-max-bytes 200000
  "Maximum number of bytes to inline from a referenced file."
  :type 'integer
  :group 'superchat)

(defcustom superchat-inline-context-template
  (concat
   "You are provided with the content of a local file for context.\n"
   "Use this context to answer the user's question.\n"
   "File: $path\n"
   "----- BEGIN FILE CONTENT -----\n"
   "$content\n"
   "----- END FILE CONTENT -----\n")
  "Template for embedding file content into the prompt."
  :type 'string
  :group 'superchat)

(defcustom superchat-response-timeout 120
  "Maximum time in seconds to wait for LLM response completion.

IMPORTANT: This timeout only triggers when NO data is received at all.
If the LLM is actively generating content (data is being received), the
response will be treated as complete even if it exceeds this timeout.

This prevents UI freezing in two scenarios:
1. Model is completely unresponsive (no data received)
2. Blocking/synchronous tools that freeze the connection

CRITICAL FOR TOOL USAGE:
When using tools that require user confirmation (web-search, web-fetch, etc.),
the confirmation dialogs consume time BEFORE the LLM starts responding.
Therefore, a longer timeout is essential:
- 60-90 seconds: Minimum for basic tool usage
- 120 seconds (default): Recommended for multiple tool calls
- 180+ seconds: For complex workflows with many confirmations

Set to nil to disable timeout protection (not recommended)."
  :type '(choice (const :tag "No timeout" nil)
                 (integer :tag "Seconds"))
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

(declare-function superchat--llm-drop-dangling-tool-use "superchat-llm" (prompt))
(declare-function superchat--llm-tool-error-feedback "superchat-llm" (result))
(declare-function superchat--llm-tool-error-non-retryable-p "superchat-llm" (result))
(declare-function llm-chat-prompt-append-response "llm" (prompt response &optional role))

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

(defcustom superchat-completion-check-delay 2
  "Time in seconds to wait for new data before considering response complete.

This is used for smart completion detection, primarily for non-streaming
responses (e.g., Ollama + tools mode). If no new data arrives within this
time, the response is considered complete.

Background:
- gptel disables streaming for Ollama + tools (see gptel-request.el:2019)
- Ollama's tool calling streaming has known issues (ollama/ollama#12557)
- This setting provides a balance between responsiveness and reliability

Recommended values:
- 1-2 seconds: Fast completion, suitable for local models
- 3-5 seconds: More conservative, for slower connections
- Higher values: For very slow networks or complex tool calls

Note: This only affects non-streaming responses. Normal streaming responses
complete immediately via the standard completion signal."
  :type 'integer
  :group 'superchat)

(defcustom superchat-show-response-mode t
  "Whether to display response mode indicator.
When enabled, shows the current response mode
\(streaming/non-streaming/tool-calling) before the response
starts.  Particularly useful for Ollama + tool scenarios
where non-streaming mode is used."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-tool-timeout-multiplier 1.5
  "Timeout multiplier for tool calling mode.
Default is 1.5, which adds 50% to the base timeout.
Tool calling (especially with cloud models or Ollama) may need more time."
  :type 'number
  :group 'superchat)

(defvaralias 'superchat-ollama-timeout-multiplier 'superchat-tool-timeout-multiplier
  "Alias for `superchat-tool-timeout-multiplier` for backward compatibility.")

;; ── Model-list cache (must be defined before `superchat-llm-backend'
;;     because its :set function calls `superchat--invalidate-model-cache') ──

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
         (superchat--invalidate-model-cache))
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

(defcustom superchat-show-ttft t
  "When non-nil, show Time-To-First-Token (TTFT) in the echo area.
Measures the end-to-end interval from `C-c C-c' (user send) to the
first chunk arriving from the LLM streaming response.
Displayed as \"⚡ TTFT: X.XXs\" in echo area and on the Assistant header."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-llm-reasoning 'none
  "Reasoning (a.k.a. thinking) effort to request from the model.

Passed to `llm-make-chat-prompt' as :reasoning.  Recognized values
mirror llm.el's spec:

  nil       — provider default (thinking-capable models will think)
  none      — disable thinking entirely (fastest, default)
  light     — minimal reasoning effort
  medium    — moderate reasoning effort
  maximum   — maximum reasoning effort
  t         — enable thinking (same as medium)

For reasoning-capable local models (qwen3.6, deepseek-r1, etc.),
the default `none' avoids long thinking phases that block streaming
output and inflate TTFT by 10-20x.  Set to t or medium when you
need the model to reason carefully.

Providers without reasoning support silently ignore this."
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

(defcustom superchat-llm-tools-enabled 'on-demand
  "Policy controlling when llm.el tools are attached to chat requests.

Attaching tools forces llm.el into multi-output / tool-calling mode,
which noticeably increases time-to-first-token because the model must
inspect every tool schema before responding.  Most casual chat does
not benefit from tools, so this defaults to `on-demand'.

Allowed values:
  nil          -- never attach tools (chat-only).
  `on-demand'  -- attach tools when the input shows a tool intent:
                  a `#'-style file reference, an explicit `render_ui' or
                  `eshell-command' request, or an active slash command
                  (`superchat--current-command' non-nil).
  `always' / t -- attach tools to every request (legacy behavior)."
  :type '(choice (const :tag "Never" nil)
                 (const :tag "On demand (default)" on-demand)
                 (const :tag "Always" always))
  :group 'superchat)

;; --- Faces ---
(defface superchat-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for labels like 'User:'."
  :group 'superchat)

(defface superchat-prompt-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the input prompt '>'."
  :group 'superchat)

(defface superchat-streaming-pending
  '((t :inherit shadow :italic t :extend t))
  "Face for text currently being streamed from the LLM.
Applied to each chunk as it arrives so the raw markdown (`**bold**',
backticks, code fences) doesn't get re-interpreted by font-lock
mid-stream — which is what caused the visible \"residue/ghosting\"
before the final rewrite.  Removed at stream end when
`superchat--process-llm-result' replaces the region with the formatted
Org version."
  :group 'superchat)

;; --- Buffer-Local Variables ---
(defvar-local superchat--conversation-history nil)
(defvar-local superchat--response-start-marker nil)
(defvar-local superchat--prompt-start nil)
(defvar-local superchat--queued-follow-up nil
  "Draft for the next input; it does not affect the current Run or steer it.")
(defvar-local superchat--active-run-id nil
  "Run ID of the Run most recently submitted from this chat buffer.")

(defvar superchat-handoff-auto-enabled nil
  "Non-nil when the optional compact plugin enables automatic handoff.")

(defvar-local superchat--saved-header-line-format nil
  "Header line that was active before `superchat-mode' was enabled.")

(defvar-local superchat--header-line-installed-p nil
  "Non-nil when `superchat-mode' owns this buffer's header line.")

(defvar-local superchat--pause-requested nil
  "Non-nil when the user asked to pause at the next tool boundary.")

(defvar-local superchat--pause-run-id nil
  "Run ID that owns the current pause continuation, if any.")

(defun superchat--mode-line-run-state ()
  "Return a compact, live summary of this buffer's Run and queued input."
  (let ((queued (and (stringp superchat--queued-follow-up)
                     (not (string-empty-p superchat--queued-follow-up))))
        (active (and superchat--active-run-id
                     (member superchat--active-run-id
                             (superchat-runtime-active-runs)))))
    (when (or active queued)
      (if active
          (let* ((status (superchat-runtime-status superchat--active-run-id))
                 (phase (if superchat--pause-requested
                            "PAUSED"
                          (or (plist-get status :status) 'active))))
            (format " · run %s · %s · %s%s · C-c C-g"
                    phase
                    (or (plist-get status :capability) "chat")
                    (or (plist-get status :model) "backend default")
                    (if queued " · next input queued" "")))
        " · next input queued · C-c C-g"))))

(defun superchat--format-token-count (tokens)
  "Return TOKENS as a compact human-readable count."
  (cond
   ((not (numberp tokens)) "?")
   ((>= tokens 1000000) (format "%.1fM" (/ tokens 1000000.0)))
   ((>= tokens 1000) (format "%.1fk" (/ tokens 1000.0)))
   (t (number-to-string tokens))))

(defun superchat--header-line-context (status)
  "Return the context-usage segment for Run STATUS."
  (let ((used (plist-get status :context-used))
        (limit (plist-get status :context-limit))
        (source
         (pcase (plist-get status :context-limit-source)
           ('lm-studio-runtime "runtime")
           ('override "override")
           ('llm "llm")
           (_ nil))))
    (when (and (numberp limit) (> limit 0))
      (if (numberp used)
          (let* ((percent (* 100.0 (/ used (float limit))))
                 (face (cond ((> percent 90) 'error)
                             ((> percent 70) 'warning)
                             (t 'shadow))))
            (propertize
             (format "context %s%s/%s %.1f%%%s"
                     (if (plist-get status :context-estimated-p) "~" "")
                     (superchat--format-token-count used)
                     (superchat--format-token-count limit)
                     percent
                     (if source (format " [%s]" source) ""))
             'face face
             'help-echo
             (and source (format "Context-window source: %s" source))))
        (format "context ?/%s%s"
                (superchat--format-token-count limit)
                (if source (format " [%s]" source) ""))))))

(defun superchat--header-line-run-state ()
  "Return the live Agent status projected into the chat header line."
  (let* ((run-id superchat--active-run-id)
         (status (and run-id (superchat-runtime-status run-id)))
         (active (and run-id
                      (member run-id (superchat-runtime-active-runs))))
         (model (or (plist-get status :model)
                    superchat-llm-model
                    (and superchat-llm-backend
                         (ignore-errors
                           (superchat--provider-chat-model
                            superchat-llm-backend)))
                    "backend default"))
         (phase (cond
                 (superchat--pause-requested "paused")
                 ((eq (plist-get status :context-recovery) 'compacting)
                  "compacting")
                 (active
                  (pcase (plist-get status :status)
                    ('running-model "thinking")
                    (value (format "%s" (or value "active")))))
                 ((memq (plist-get status :status)
                        '(failed cancelled timed-out audit-incomplete abandoned))
                  (format "%s" (plist-get status :status)))
                 (t "idle")))
         (attempt (and active (plist-get status :attempt)))
         (context (superchat--header-line-context status)))
    (concat
     (propertize (if active "●" "○")
                 'face (if active 'success 'shadow))
     " Superchat · " model " · " phase
     (if (numberp attempt) (format " · attempt %d" attempt) "")
     (if context (concat " │ " context) "")
     (if (bound-and-true-p superchat-handoff-auto-enabled)
         " · auto-compact"
       ""))))

(defun superchat-queue-follow-up ()
  "Queue a draft for the next user round without steering the current Run.
Repeated calls append another line.  The draft is inserted into the next
prompt by `superchat--insert-prompt' and is never sent automatically."
  (interactive)
  (let ((text (string-trim (read-string "Next input: "))))
    (unless (string-empty-p text)
      (setq-local superchat--queued-follow-up
                  (if (and (stringp superchat--queued-follow-up)
                           (not (string-empty-p superchat--queued-follow-up)))
                      (concat superchat--queued-follow-up "\n" text)
                    text))
      (force-mode-line-update t)
      (when (fboundp 'superchat-agents-panel-refresh)
        (superchat-agents-panel-refresh))
      (message "Next input queued."))))

(defvar-local superchat--pending-round-reason nil
  "Attempt reason stashed by the pause checkpoint.")

(defvar-local superchat--pending-send-request nil
  "Continuation stashed by the pause checkpoint; takes the attempt reason.")

(defvar-local superchat--resume-timeout-timer nil
  "Safety timer associated with the Run resumed in this View.")
(defvar-local superchat--resume-timeout-run-id nil
  "Run ID associated with `superchat--resume-timeout-timer'.")

(defun superchat--cancel-resume-timeout (&optional run-id)
  "Cancel the resume safety timer for RUN-ID in the current View.
When RUN-ID is nil, cancel the current View's timer unconditionally."
  (when (or (null run-id)
            (equal run-id superchat--resume-timeout-run-id))
    (when (timerp superchat--resume-timeout-timer)
      (cancel-timer superchat--resume-timeout-timer))
    (setq superchat--resume-timeout-timer nil
          superchat--resume-timeout-run-id nil)))

(defun superchat--clear-pause-state (&optional run-id)
  "Clear pause state owned by RUN-ID in the current View.
With nil RUN-ID, clear the current View's pause state unconditionally."
  (let ((owns-state
         (or (null run-id)
             (and (or (null superchat--pause-run-id)
                      (equal run-id superchat--pause-run-id))
                  (or (null superchat--active-run-id)
                      (equal run-id superchat--active-run-id))))))
    (superchat--cancel-resume-timeout run-id)
    (when owns-state
      (setq superchat--pause-requested nil
            superchat--pause-run-id nil
            superchat--pending-round-reason nil
            superchat--pending-send-request nil)
      (when (or (null run-id)
                (null superchat--active-run-id)
                (equal run-id superchat--active-run-id))
        (when (timerp superchat--active-timeout-timer)
          (cancel-timer superchat--active-timeout-timer))
        (setq superchat--active-timeout-timer nil)))))

(defun superchat--arm-resume-timeout (run-id)
  "Arm a safety timer for RUN-ID in the current View.
The timer checks both its captured Run ID and the current View binding before
it cancels anything, so an old timer cannot cancel a later Run."
  (superchat--cancel-resume-timeout)
  (let ((buffer (current-buffer))
        timer)
    (setq timer
          (run-with-timer
           (or superchat-response-timeout 120) nil
           (lambda ()
             (when (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (eq superchat--resume-timeout-timer timer)))
               (with-current-buffer buffer
                 (setq superchat--resume-timeout-timer nil
                       superchat--resume-timeout-run-id nil)
                 (when (and (equal superchat--active-run-id run-id)
                            (member run-id
                                    (superchat-runtime-active-runs)))
                   (ignore-errors
                     (superchat-runtime-cancel run-id
                                               "[Paused Run timed out]"))))))))
    (setq superchat--resume-timeout-timer timer
          superchat--resume-timeout-run-id run-id)))

(defun superchat--pause-boundary ()
  "Run at the tool boundary when a pause was requested.
Stops the safety-net timeout; the Run stays active and waits for
`superchat-resume'."
  (when (timerp superchat--active-timeout-timer)
    (cancel-timer superchat--active-timeout-timer)
    (setq superchat--active-timeout-timer nil))
  (message "Paused at the tool boundary. C-c C-r (or M-x superchat-resume) to continue with a steer message."))

(defun superchat-pause ()
  "Request a pause at the next tool boundary of the current Run.
The Run stays active; the model call for the next round never starts
until `superchat-resume'.  Pauses only take effect between model
rounds, never mid-stream."
  (interactive)
  (if (and (boundp 'superchat--active-run-id)
           superchat--active-run-id
           (fboundp 'superchat-runtime-active-runs)
           (member superchat--active-run-id
                   (superchat-runtime-active-runs)))
      (progn
        (setq superchat--pause-requested t
              superchat--pause-run-id superchat--active-run-id)
        (message "Pause requested — takes effect at the next tool boundary."))
    (message "No active Run to pause.")))

(defun superchat-resume ()
  "Resume a paused Run, optionally with a steer message.
The steer message is recorded and queued by Runtime before the stashed
round continues."
  (interactive)
  (cond
   ((not (and (boundp 'superchat--pause-requested)
              superchat--pause-requested))
    (message "Nothing paused."))
   (t
    (let* ((run-id (or superchat--pause-run-id
                       superchat--active-run-id))
           (steer (string-trim
                   (read-string
                    "Instruction for the model (empty to continue): ")))
           (pending superchat--pending-send-request)
           (reason superchat--pending-round-reason))
      (cond
       ;; An empty resume checks the Runtime registry directly.  A non-empty
       ;; steer uses Runtime's own accepting return value as the same live-Run
       ;; check while also persisting the instruction.
       ((and (string-empty-p steer)
             (not (and run-id
                       (member run-id
                               (superchat-runtime-active-runs)))))
        (superchat--clear-pause-state)
        (message "Paused Run is no longer active; cancel and resend instead."))
       ((and (not (string-empty-p steer))
             (not (condition-case nil
                      (superchat-runtime-steer run-id steer)
                    (error nil))))
        (superchat--clear-pause-state)
        (message "Paused Run is no longer active; cancel and resend instead."))
       ((not (functionp pending))
        (superchat--clear-pause-state)
        (message "Resume state lost — cancel and resend instead."))
       (t
        (setq superchat--pause-requested nil
              superchat--pending-round-reason nil)
        (superchat--arm-resume-timeout run-id)
        (condition-case err
            (progn
              (funcall pending reason)
              (message "Resumed."))
          (error
           (superchat--clear-pause-state)
           (message "Resume failed: %s" (error-message-string err))))
        ;; A synchronous re-pause may have installed a new continuation.
        (when (eq superchat--pending-send-request pending)
          (setq superchat--pending-send-request nil))))))))

(defvar-local superchat-authority-mode 'ask
  "Session permission mode for the chat buffer.
`explore' denies side-effecting tools (read-only); `ask' is the
interactive default with per-call and per-session approval; `execute'
allows classified local effects without asking.  Mode changes never
relax a registered Authority policy (tighten-only).")

(defvar-local superchat-authority--session-allowlist nil
  "Tool names approved for the rest of this session (the y/n/a `a').
Dangerous process-executing tools never enter this list.")

(defun superchat-authority-set-mode (mode)
  "Set the session permission MODE for the current chat buffer."
  (interactive
   (list (intern
          (completing-read "Permission mode: "
                           '("ask" "explore" "execute") nil t))))
  (unless (memq mode '(explore ask execute))
    (user-error "Mode must be explore, ask, or execute: %S" mode))
  (setq superchat-authority-mode mode)
  ;; A mode change also clears session approvals: what was granted under
  ;; one posture must not silently survive into a more permissive one.
  (setq superchat-authority--session-allowlist nil)
  (message "Permission mode: %s" mode))

(defun superchat--track-active-run (handle)
  "Record HANDLE as the chat buffer's active Run.  Returns HANDLE."
  (when (listp handle)
    (let ((buffer (if (bound-and-true-p superchat-mode)
                      (current-buffer)
                    (get-buffer-create superchat-buffer-name))))
      (with-current-buffer buffer
        (setq superchat--active-run-id (plist-get handle :run-id)))))
  handle)

(defun superchat--notify-run-terminal (status)
  "Message terminal Run STATUS when the user is not in its View.
STATUS is the Runtime public status plist passed by
`superchat-turn-completed-functions'.  `superchat-buffer-name' is bound
to the Run's own buffer during the hooks, so the selected window's
buffer is compared against it."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status))
             (plist-get status :run-id))
    (let ((undelivered (or (plist-get status :undelivered-steer-count) 0))
          (view-buffer (window-buffer (selected-window)))
          (run-buffer
           (and (boundp 'superchat-buffer-name)
                superchat-buffer-name
                (get-buffer superchat-buffer-name))))
      (cond
       ((> undelivered 0)
        (message "superchat: run %s ended; %d steer instruction%s not delivered"
                 (plist-get status :run-id)
                 undelivered
                 (if (= undelivered 1) "" "s")))
       ((not (eq view-buffer run-buffer))
        (message "superchat: run %s %s (session %s)"
                 (plist-get status :run-id)
                 (plist-get status :status)
                 (plist-get status :session-id)))))))

(add-hook 'superchat-turn-completed-functions
          #'superchat--notify-run-terminal)

(defun superchat-cancel ()
  "Interrupt the current Run and collect a redirect prompt.
The Run is cancelled first, then the minibuffer asks for the revised
  instruction; a non-empty answer is pre-filled into the next turn's
  input.  Reports when no Run is active."
  (interactive)
  (let ((buffer (if (bound-and-true-p superchat-mode)
                    (current-buffer)
                  (get-buffer-create superchat-buffer-name))))
    (with-current-buffer buffer
      (if (and (boundp 'superchat--active-run-id)
               superchat--active-run-id
               (fboundp 'superchat-runtime-active-runs)
               (member superchat--active-run-id
                       (superchat-runtime-active-runs)))
        (let ((run-id superchat--active-run-id))
          (unwind-protect
              (superchat-runtime-cancel run-id)
            (superchat--clear-pause-state))
          (let ((redirect
                 (string-trim
                  (read-string "Redirect instruction (empty to just cancel): "))))
            (when (not (string-empty-p redirect))
              (unless (and (markerp superchat--prompt-start)
                           (marker-position superchat--prompt-start))
                (superchat--insert-prompt))
              (let ((inhibit-read-only t))
                (goto-char (point-max))
                (insert redirect)))
            (message "Cancelled Run %s%s" run-id
                     (if (string-empty-p redirect)
                         ""
                       " — redirect ready"))))
        (superchat--clear-pause-state)
        (message "No active Run to cancel.")))))
(defvar superchat-mode)                ; defined by define-minor-mode below
(defvar-local superchat--assistant-response-start-marker nil)
(defvar-local superchat--current-command nil
  "Current active chat command (e.g. 'create-question', nil for default mode).")
(defvar-local superchat--current-response-parts nil
  "A list to accumulate response chunks in a streaming conversation.")
(defvar-local superchat--ttft-start-time nil
  "Timestamp (float-time) recorded at `superchat-send-input' entry.
Used to compute end-to-end Time-To-First-Token.
Read via `with-current-buffer' in the streaming callback.")
(defvar-local superchat--pending-recalled-memories nil
  "Memories pre-seeded by `/recall', to attach to the next user turn.
Each element is a row/plist as returned by `superchat-memory-retrieve' or
`superchat-db-memory-search-simple'.  Consumed by `superchat-send', which
moves them onto `superchat-turn-retrieved-memories' for the new turn.")

(defvar superchat--current-turn nil
  "The `superchat-turn' being dispatched.
Bound dynamically by `superchat-send' so prompt builders can read
turn-scoped data (retrieved memories, context files, etc.) without
threading the turn through every call site.")
(defvar-local superchat--active-timeout-timer nil
  "The currently active timeout timer for the ongoing LLM response.")
(defvar-local superchat--timeout-extension-amount 30
  "Number of seconds to extend timeout when user confirms a tool action.")

(defvar-local superchat--session-id nil
  "Unique identifier for the current conversation session.
Used as the session_id column in the SQLite tape table.
Generated on first use per buffer.")

(defvar-local superchat--active-preset nil
  "The active `superchat-preset' for the current session.
Used when the user does not explicitly specify a skill in the input.")

(defvar-local superchat--active-capability nil
  "Capability selected for the current Session.
This is separate from `superchat--active-preset', which may be an Agent
Profile whose identity differs from its Capability.")

(defvar-local superchat--active-topology 'single
  "Execution topology selected for the current Session.")

(defvar-local superchat--active-authority-policy "default"
  "Named Authority policy selected for the current Session.")

(defvar-local superchat--workspace-binding nil
  "Current Session Workspace binding plist, or nil.")

(defvar-local superchat--last-surface nil
  "Most recent request Surface reconstructed for this Session.")

(defvar-local superchat--session-name nil
  "Optional human-readable name for the current session.
Can be set via slash commands or UI interactions.")

(defconst superchat--team-topology-tool-names
  '("team_task" "team_message"
    "delegate_to_subagent" "delegate_to_subagent_parallel")
  "Coordination tools contributed by the team Topology overlay.")

(defconst superchat--team-topology-system-prompt
  (concat
   "[Topology: team]\n"
   "Coordinate bounded work through subagents. Use parallel delegation only "
   "for independent tasks, track shared work with team_task, and exchange "
   "relevant findings with team_message. When Workspace tools are available, "
   "coordinate shared Artifacts by ID and revision. Never simulate tool "
   "results.")
  "System instruction contributed by the team Topology overlay.")

(defun superchat--configuration-event (type payload)
  "Append a Session configuration event of TYPE with PAYLOAD."
  (when (and (stringp superchat--session-id)
             (not (string-empty-p superchat--session-id)))
    (superchat-tape-append-event
     (list :type type
           :session-id superchat--session-id
           :actor-id "user"
           :summary type
           :payload payload))))

(defun superchat--builtin-capability-preset (name)
  "Return the built-in Capability preset named NAME, or nil."
  (pcase name
    ("code"
     (superchat-preset-from-plist
      (list :name "code"
            :profile-id "code"
            :description "Coding in the current Emacs context"
            :type 'agent
            :capability "code"
            :tools '("list_buffers" "read_buffer" "edit_buffer" "replace_buffer"
                     "eval_elisp" "eshell-command"
                     "workspace_read" "workspace_write" "workspace_info")
            :body
            "Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools."
            :source 'builtin)))
    ("research"
     (superchat-preset-from-plist
      (list :name "research"
            :profile-id "research"
            :description "Investigation with durable Workspace delivery"
            :type 'agent
            :capability "research"
            :tools '("list_buffers" "read_buffer" "eval_elisp"
                     "sql" "memory_search" "tool_history" "file_history"
                     "recent_errors" "workspace_read" "workspace_publish"
                     "workspace_info")
            :body
            "Investigate the question from available sources, distinguish evidence from inference, and publish durable deliverables with workspace_publish when requested."
            :source 'builtin)))
    ("cowork"
     (superchat-preset-from-plist
      (list :name "cowork"
            :profile-id "cowork"
            :description "Organize work through available Emacs integrations"
            :type 'agent
            :capability "cowork"
            :tools '("read_buffer" "append_to_buffer"
                     "workspace_read" "workspace_write" "workspace_info"
                     "render_ui")
            :body
            "Help organize and produce work using the integrations currently available in Emacs. Do not claim access to external systems unless their tools are present."
            :source 'builtin)))
    ("plan"
     (superchat-preset-from-plist
      (list :name "plan"
            :description "Read-only planning mode"
            :type 'plan
            :capability "plan"
            :tools '("list_buffers" "read_buffer" "eval_elisp"
                     "workspace_read" "workspace_info" "emit_plan")
            :body
            (concat
             "Analyze the request and available context, then produce a "
             "clear step-by-step plan. You have read-only tools "
             "(list_buffers, read_buffer, eval_elisp): always "
             "inspect the actual code before planning, never claim you "
             "cannot access files. When your investigation is done, call "
             "emit_plan with steps as (text accept) pairs so the plan "
             "renders as a reviewable checklist.")
            :source 'builtin)))
    (_ nil)))

(defun superchat--resolve-capability-preset (name &optional source)
  "Resolve Capability NAME from SOURCE for Session restoration."
  (unless (equal name "chat")
    (if (equal source "builtin")
        (superchat--builtin-capability-preset name)
      (or (ignore-errors
            (and (superchat-skills-exists-p name)
                 (superchat-skills-load name)))
          (superchat--builtin-capability-preset name)))))

(defun superchat--resolve-profile-preset (profile-id capability source)
  "Resolve PROFILE-ID for Session restoration.
CAPABILITY and SOURCE provide the legacy lookup fallback."
  (let ((direct
         (superchat--resolve-capability-preset profile-id source)))
    (or (and direct
             (equal profile-id (superchat-preset-profile-id direct))
             direct)
        ;; ponytail: O(n) only while restoring a renamed Profile; add an
        ;; index if real Skill counts make this measurable.
        (and (not (equal source "builtin"))
             (fboundp 'superchat-skills-get-available)
             (cl-loop
              for name in (superchat-skills-get-available)
              for preset = (ignore-errors (superchat-skills-load name))
              when (and preset
                        (equal profile-id
                               (superchat-preset-profile-id preset)))
              return preset))
        (superchat--resolve-capability-preset capability source))))

(defun superchat--activate-capability (preset &optional name)
  "Activate PRESET as the Session Capability.
When PRESET is nil, activate ordinary chat under optional NAME.
NAME may restate PRESET's explicit capability but cannot override it."
  (let* ((explicit (and preset (superchat-preset-capability preset)))
         (legacy-name
          (and preset (null explicit)
               (format "%s" (superchat-preset-name preset))))
         (capability (or explicit name legacy-name "chat"))
         (profile-id (and preset (superchat-preset-profile-id preset)))
         (source
          (and preset (superchat-preset-source preset)
               (symbol-name (superchat-preset-source preset)))))
    (when (and explicit name (not (equal name explicit)))
      (error "Capability %s conflicts with Preset %s capability %s"
             name
             (or profile-id (superchat-preset-name preset))
             explicit))
    (when (and legacy-name (superchat-preset-agent-p preset))
      (display-warning
       'superchat
       (format
        "Agent Profile %s has no explicit capability; using its name for compatibility"
        legacy-name)
       :warning))
    (superchat--configuration-event
     "capability.activated"
     (append (list :name capability :source source)
             (when profile-id (list :profile-id profile-id))))
    (setq-local superchat--active-capability capability)
    (setq-local superchat--active-preset preset)
    preset))

(defun superchat--set-topology (topology)
  "Select TOPOLOGY for the current Session."
  (unless (memq topology '(single team))
    (error "Unknown Superchat topology: %s" topology))
  (superchat--configuration-event
   "topology.changed" (list :topology (symbol-name topology)))
  (setq-local superchat--active-topology topology))

(defun superchat--set-authority-policy (policy)
  "Select named Authority POLICY for the current Session."
  (let ((name (symbol-name
               (superchat-authority-normalize-policy policy))))
    (superchat--configuration-event
     "authority.policy.selected" (list :policy name))
    (setq-local superchat--active-authority-policy name)))

(defun superchat--workspace-binding-for-directory (directory)
  "Return a stable Workspace binding for DIRECTORY."
  (let* ((root (file-name-as-directory
                (file-truename (expand-file-name directory))))
         (id (concat "workspace-"
                     (substring (secure-hash 'sha256 root) 0 12))))
    (list :id id :root root)))

(defun superchat--bind-workspace (directory)
  "Bind the current Session Workspace to DIRECTORY, or clear it when nil."
  (let ((binding
         (when directory
           (unless (file-directory-p directory)
             (error "Workspace directory does not exist: %s" directory))
           (superchat--workspace-binding-for-directory directory))))
    (if binding
        (superchat--configuration-event
         "workspace.bound"
         (list :workspace-id (plist-get binding :id)
               :workspace-root (plist-get binding :root)))
      (superchat--configuration-event "workspace.unbound" nil))
    (when binding
      (superchat-workspace-bind
       (plist-get binding :id)
       (plist-get binding :root)
       superchat--session-id))
    (setq-local superchat--workspace-binding binding)
    (setq-local superchat-tools-default-directory
                (and binding (plist-get binding :root)))
    binding))

(defun superchat--session-runtime-configuration ()
  "Return the current Session configuration as Runtime Request fields."
  (let ((workspace superchat--workspace-binding))
    (append
     (list :capability
           (or superchat--active-capability
               (and superchat--active-preset
                    (superchat-preset-capability superchat--active-preset))
               "chat")
           :topology (or superchat--active-topology 'single)
           :authority-policy
           (or superchat--active-authority-policy "default"))
     (when workspace
       (list :workspace-id (plist-get workspace :id)
             :workspace-root (plist-get workspace :root))))))

(defun superchat--apply-topology-overlay (turn)
  "Apply the current Session Topology overlay to TURN."
  (when (eq superchat--active-topology 'team)
    (let* ((current-tools (superchat-turn-tools turn))
           (base-tools
            (cond
             ((eq current-tools 'none) nil)
             ((listp current-tools) current-tools)
             ((eq superchat-llm-tool-names 'all) nil)
             ((listp superchat-llm-tool-names)
              superchat-llm-tool-names)))
           (tools
            (unless (and (null current-tools)
                         (eq superchat-llm-tool-names 'all))
              (delete-dups
               (append base-tools superchat--team-topology-tool-names))))
           (system-prompt (or (superchat-turn-system-prompt turn) "")))
      (when tools
        (setf (superchat-turn-tools turn) tools))
      (unless (string-match-p
               (regexp-quote "[Topology: team]") system-prompt)
        (setf (superchat-turn-system-prompt turn)
              (if (string-empty-p system-prompt)
                  superchat--team-topology-system-prompt
                (concat system-prompt "\n\n"
                        superchat--team-topology-system-prompt))))))
  turn)

(defun superchat--restore-session-runtime-configuration (session-id)
  "Restore SESSION-ID configuration into the current View cache."
  (let* ((configuration
          (superchat-view-session-configuration session-id))
         (capability (plist-get configuration :capability))
         (capability (if (stringp capability) capability "chat"))
         (profile-id (plist-get configuration :profile-id))
         (profile-id (if (stringp profile-id) profile-id capability))
         (source (plist-get configuration :capability-source))
         (source (and (stringp source) source))
         (preset (ignore-errors
                   (superchat--resolve-profile-preset
                    profile-id capability source)))
         (topology (plist-get configuration :topology))
         (topology (if (stringp topology) (intern topology) topology))
         (authority-policy
          (or (plist-get configuration :authority-policy) "default"))
         (workspace-id (plist-get configuration :workspace-id))
         (workspace-root (plist-get configuration :workspace-root))
         (workspace-live-p
          (and (stringp workspace-id) (stringp workspace-root)
               (file-directory-p workspace-root))))
    (setq-local superchat--active-capability capability)
    (setq-local superchat--active-preset preset)
    (setq-local superchat--active-topology
                (if (memq topology '(single team)) topology 'single))
    (setq-local superchat--active-authority-policy
                (if (superchat-authority-policy-known-p authority-policy)
                    (symbol-name
                     (superchat-authority-normalize-policy
                      authority-policy))
                  "default"))
    (setq-local superchat--workspace-binding
                (and workspace-live-p
                     (list :id workspace-id :root workspace-root)))
    (when workspace-live-p
      (superchat-workspace-bind workspace-id workspace-root session-id))
    (setq-local superchat-tools-default-directory
                (and workspace-live-p workspace-root))
    (setq-local superchat--last-surface
                (plist-get configuration :surface))
    configuration))

;; --- Global Variables ---
(defvar superchat--builtin-commands
  '(("backend" . superchat-backend-show)
    ("models" . superchat-model-list)
    ("mcp" . superchat-mcp-status)
    ("mcp-start" . superchat-mcp-start-servers)
    ("mcp-start-server" . superchat-mcp-start-server)
    ("mcp-stop-server" . superchat-mcp-stop-server)
    ("mcp-health" . superchat-mcp-check-all-health)
    ("refresh-models" . superchat-refresh-models))
  "Alist of built-in commands and their prompt templates.")

(defvar superchat--user-commands (make-hash-table :test 'equal)
  "Hash table of user-defined commands and their prompt templates.")

(defun superchat--detect-response-mode (&optional tools tools-known-p)
  "Detect and return the expected response mode.

When TOOLS-KNOWN-P is non-nil, TOOLS is used as-is, and nil means
\"no tools\".  Otherwise this falls back to collecting the full tool set,
which is expensive and should not be used inside the request pipeline.

Returns:
  'streaming      - Streaming response (default)
  'non-streaming  - Non-streaming response (Ollama + tool calling)
  'tool-calling   - Tool calling mode (non-Ollama backend)"
  (let ((effective-tools
         (if tools-known-p tools
           (append (when (fboundp 'superchat-get-llm-tools)
                     (superchat-get-llm-tools))
                   (when (fboundp 'superchat-mcp-get-tools)
                     (superchat-mcp-get-tools))))))
    (cond
     ;; Ollama + tool calling = non-streaming
     ((and (superchat--is-ollama-backend-p)
           effective-tools
           (> (length effective-tools) 0))
      'non-streaming)

     ;; Has tools but not Ollama = tool calling mode (may be streaming)
     ((and effective-tools (> (length effective-tools) 0))
      'tool-calling)

     ;; Default streaming
     (t 'streaming))))

(defun superchat--get-adjusted-timeout (&optional mode)
  "Return adjusted timeout based on response MODE.
When MODE is nil, falls back to `superchat--detect-response-mode'
without precomputed tools (slow path — prefer passing MODE)."
  (let ((effective-mode (or mode (superchat--detect-response-mode)))
        (base-timeout (or superchat-response-timeout 120)))
    (pcase effective-mode
      ((or 'non-streaming 'tool-calling)
       (floor (* base-timeout superchat-tool-timeout-multiplier)))
      (_
       base-timeout))))

(defun superchat--get-adjusted-completion-delay (&optional mode)
  "Return adjusted completion check delay based on response MODE.
When MODE is nil, falls back to `superchat--detect-response-mode'
without precomputed tools (slow path — prefer passing MODE)."
  (let ((effective-mode (or mode (superchat--detect-response-mode)))
        (base-delay (or superchat-completion-check-delay 2)))
    (pcase effective-mode
      ('non-streaming
       (floor (* base-delay 2)))
      (_
       base-delay))))

(defun superchat--show-response-mode-indicator (&optional mode)
  "Display current response mode indicator in the response area.
Shows different messages based on detected MODE.  When MODE is nil,
falls back to `superchat--detect-response-mode'."
  (when superchat-show-response-mode
    (let ((mode (or mode (superchat--detect-response-mode))))
      (superchat--update-status
       (pcase mode
         ('streaming
          "🔄 Streaming Response Mode")
         ('non-streaming
          "⏳ Non-streaming Mode (Ollama + Tools, may be slow)")
         ('tool-calling
          "🔧 Tool Calling Mode"))))))

(defun superchat--extend-timeout ()
  "Extend the active timeout timer by the configured extension amount.
This is called automatically when user confirms a tool action."
  (when (and superchat--active-timeout-timer
             (timerp superchat--active-timeout-timer)
             superchat-response-timeout)
    (let* ((current-time (float-time))
           (timer-time (timer--time superchat--active-timeout-timer))
           (trigger-time (time-to-seconds timer-time))
           (new-trigger-time (+ trigger-time superchat--timeout-extension-amount)))
      ;; Update timer to fire later. Called on every stream chunk and tool
      ;; event, so do NOT emit a user-visible message here — it would spam
      ;; the echo area hundreds of times per response.
      (timer-set-time superchat--active-timeout-timer
                      (seconds-to-time new-trigger-time)))))

(defun superchat--record-message (role content &optional existing-tape-id)
  "Record a conversation message with ROLE and CONTENT into history.
Also appends to the SQLite tape for persistent storage.
The in-memory message plist includes :id with the tape row id.
When EXISTING-TAPE-ID is non-nil, reuse that Runtime event instead of
appending a duplicate legacy row."
  (let ((text (string-trim (or content "")))
        (tape-id existing-tape-id))
    (when (> (length text) 0)
      ;; Tape: append-only persistent material log.
      (when (null existing-tape-id)
        (ignore-errors
          ;; Lazy-init session id
          (unless superchat--session-id
            (setq superchat--session-id (superchat--new-session-id)))
          (setq tape-id (superchat-tape-append-material
                         superchat--session-id
                         (pcase role
                           ("user" "user")
                           ("assistant" "assistant")
                           (_ "system"))
                         text))))
      ;; In-memory history (kept after tape so :id is available)
      (setq superchat--conversation-history
            (cons (if tape-id
                      (list :role role :content text :id tape-id)
                    (list :role role :content text))
                  superchat--conversation-history))
      (when (and (integerp superchat-conversation-history-limit)
                 (> superchat-conversation-history-limit 0)
                 (> (length superchat--conversation-history)
                    superchat-conversation-history-limit))
        (setq superchat--conversation-history
              (cl-subseq superchat--conversation-history 0
                         superchat-conversation-history-limit))))))

(defun superchat--input-meets-memory-threshold-p (input)
  "Return non-nil when INPUT is long enough to trigger auto recall."
  (let* ((trimmed (string-trim (or input "")))
         (width (string-width trimmed)))
    (and (> width 0)
         (>= width superchat-memory-auto-recall-min-length))))

(defun superchat--recent-conversation-messages (&optional message-limit char-limit)
  "Return recent conversation messages constrained by MESSAGE-LIMIT and CHAR-LIMIT.
MESSAGE-LIMIT bounds how many recent messages are included; nil means unlimited and 0 disables history.
CHAR-LIMIT bounds the approximate number of characters contributed by the formatted history.
The newest message is always kept even if it exceeds the character limit."
  (let* ((message-limit (cond
                         ((null message-limit) most-positive-fixnum)
                         ((<= message-limit 0) 0)
                         (t message-limit)))
         (char-limit (cond
                      ((null char-limit) most-positive-fixnum)
                      ((<= char-limit 0) 0)
                      (t char-limit)))
         (history superchat--conversation-history)
         (collected '())
         (char-count 0)
         (message-count 0))
    (while (and history (< message-count message-limit))
      (let* ((message (car history))
             (role (plist-get message :role)))
        (setq history (cdr history))
        (when (superchat--role-matches role 'user)
          (let ((line (superchat--format-conversation-message message)))
            (when line
              (let ((new-count (+ char-count (length line) (if (null collected) 0 1))))
                (cond
                 ((> new-count char-limit)
                  (if collected
                      (setq history nil)
                    (push (cons message line) collected)
                    (setq char-count new-count)
                    (setq message-count (1+ message-count))))
                 (t
                  (push (cons message line) collected)
                  (setq char-count new-count)
                  (setq message-count (1+ message-count))))))))))
    (nreverse (mapcar #'car collected))))

(defun superchat--format-conversation-message (message)
  "Convert MESSAGE plist into a plain text line for prompt context."
  (let* ((role (pcase (plist-get message :role)
                 ((or "assistant" :assistant) "Assistant")
                 ((or "system" :system) "System")
                 (_ "User")))
         (content (string-trim (or (plist-get message :content) ""))))
    (when (> (length content) 0)
      (format "%s: %s" role content))))

(defun superchat--conversation-context-string (limit)
  "Build a conversation context string from recent history honoring LIMIT and character constraints."
  (let* ((messages (superchat--recent-conversation-messages limit superchat-context-max-chars))
         (lines (delq nil (mapcar #'superchat--format-conversation-message messages)))
         (no-history (and (integerp limit) (<= limit 0)))
         (assistant-msg (when (not no-history)
                          (cl-find-if (lambda (msg)
                                        (superchat--role-matches (plist-get msg :role) 'assistant))
                                      superchat--conversation-history)))
         (assistant-text (when assistant-msg
                           (string-trim (or (plist-get assistant-msg :content) ""))))
         (assistant-line (when (and assistant-text (> (length assistant-text) 0))
                           (format "Assistant (last reply): %s" assistant-text)))
         (merged-lines (append lines (when assistant-line (list assistant-line)))))
    (when merged-lines
      (concat "Previous conversation:\n"
              (mapconcat #'identity merged-lines "\n")))))
(defun superchat--get-prompt-file-path (prompt-name)
  "Get the full path for a prompt file by PROMPT-NAME.
Supports multiple file extensions defined in `superchat-prompt-file-extensions`.
Also searches for files matching 'PROMPT-NAME-*.ext' pattern (Title-Purpose format)."
  (when (and prompt-name (superchat--command-dir))
    (let ((command-dir (superchat--command-dir))
          (found-file nil))
      ;; 1. Try exact match first (e.g. "seo.prompt")
      (setq found-file
            (cl-find-if #'file-exists-p
                        (mapcar (lambda (ext)
                                  (expand-file-name (concat prompt-name "." ext)
                                                    command-dir))
                                superchat-prompt-file-extensions)))

      ;; 2. If not found, look for "prompt-name-*.ext" (e.g. "seo-audit.prompt")
      (unless found-file
        (let ((files (directory-files command-dir t)))
          (setq found-file
                (cl-find-if (lambda (f)
                              (and (file-regular-p f)
                                   (member (file-name-extension f) superchat-prompt-file-extensions)
                                   (let* ((base (file-name-base f))
                                          (parts (split-string base "-" t)))
                                     ;; Check if file starts with "prompt-name-"
                                     ;; AND the first part is exactly prompt-name
                                     (and (cdr parts) ; Must have at least two parts (title-purpose)
                                          (string= (car parts) prompt-name)))))
                            files))))
      found-file)))

(defun superchat--load-prompt-from-file (prompt-name)
  "Load prompt content from a file by PROMPT-NAME.
Supports multiple file extensions defined in `superchat-prompt-file-extensions`."
  (let ((prompt-file-path (superchat--get-prompt-file-path prompt-name)))
    ;; (message "=== DEBUG: LOAD-PROMPT-FROM-FILE === Prompt: %s, Path found: %s"
    ;;          prompt-name (if prompt-file-path prompt-file-path "NONE"))
    (when (and prompt-file-path (file-exists-p prompt-file-path))
      ;; (message "=== DEBUG: FILE-EXISTS === Reading file: %s" prompt-file-path)
      (with-temp-buffer
        (insert-file-contents prompt-file-path)
        (let ((content (buffer-string)))
          ;; (message "=== DEBUG: FILE-CONTENT === Length: %d, Preview: %s"
          ;;          (length content)
          ;;          (substring content 0 (min 50 (length content))))
          content)))))

(defun superchat--list-available-prompts ()
  "List all available prompt files in the prompt directory."
  (let ((command-dir (superchat--command-dir)))
    (when (file-directory-p command-dir)
      (let ((prompt-files '()))
        (dolist (file (directory-files command-dir t))
          (when (and (file-regular-p file)
                     (member (file-name-extension file) superchat-prompt-file-extensions))
            (push (file-name-base file) prompt-files)))
        (sort prompt-files 'string<)))))

;; --- Core Functions ---

(defun superchat--current-input ()
  "Return current prompt line user text."
  (when (and superchat--prompt-start (marker-position superchat--prompt-start))
    (let ((input-text (buffer-substring-no-properties superchat--prompt-start (point-max))))
      (string-trim input-text))))

;; --- Model Switching ---

;; --- Command System ---

(defun superchat--load-user-commands ()
  "Load all custom command prompt files from `superchat-command-dir`.
Parses filenames in format 'TITLE-PURPOSE.prompt' to register 'TITLE' as the command."
  (superchat--ensure-directories)
  ;; Ensure superchat--user-commands is a hash table before clearing
  (unless (and (boundp 'superchat--user-commands)
               (hash-table-p superchat--user-commands))
    (setq superchat--user-commands (make-hash-table :test 'equal)))
  (clrhash superchat--user-commands)
  (let ((command-dir (superchat--command-dir)))
    (when (file-directory-p command-dir)
      (dolist (file (directory-files command-dir t))
        (when (and (file-regular-p file)
                   (member (file-name-extension file) superchat-prompt-file-extensions))
          (let* ((base-name (file-name-base file))
                 (parts (split-string base-name "-" t))
                 ;; Use first part as command name if hyphen exists, else use full name
                 (command-name (if (cdr parts) (car parts) base-name)))
            (with-temp-buffer
              (insert-file-contents file)
              (puthash command-name (buffer-string) superchat--user-commands))))))))

(defun superchat--ensure-command-loaded (command)
  "Ensure COMMAND is loaded in the user commands hash table.
If not found, try to load it from a prompt file."
  ;; (message "=== DEBUG: ENSURE-COMMAND-LOADED === Command: %s" command)
  ;; (message "=== DEBUG: BUILTIN-COMMANDS === Available: %s" (mapcar #'car superchat--builtin-commands))
  ;; (message "=== DEBUG: USER-COMMANDS === Count: %d, Keys: %s"
  ;;          (hash-table-count superchat--user-commands)
  ;;          (hash-table-keys superchat--user-commands))
  (unless (or (assoc command superchat--builtin-commands)
              (gethash command superchat--user-commands))
    ;; (message "=== DEBUG: COMMAND-NOT-FOUND === Attempting to load from file: %s" command)
    ;; Try to load from a prompt file
    (let ((prompt-content (superchat--load-prompt-from-file command)))
      ;; (message "=== DEBUG: FILE-LOAD-RESULT === Content found: %s, Length: %s"
      ;;          (if prompt-content "YES" "NO")
      ;;          (if prompt-content (length prompt-content) "N/A"))
      (when prompt-content
        (puthash command prompt-content superchat--user-commands)
        ;; (message "=== DEBUG: COMMAND-LOADED === Command '%s' loaded into hash table" command)
        ))))

(defun superchat--define-command (name prompt)
  "Define a new command NAME, persist PROMPT to a file and load it."
  (superchat--ensure-directories)
  (let ((command-dir (superchat--command-dir)))
    (unless (file-directory-p command-dir)
      (make-directory command-dir t))
    (let ((file (expand-file-name (concat name ".prompt") command-dir)))
      (with-temp-buffer
        (insert prompt)
        (write-file file)))
    (puthash name prompt superchat--user-commands)
    (message "Defined command /%s" name)))

(defun superchat--lookup-command-template (command)
  "Return the template string associated with COMMAND, or nil."
  (or (cdr (assoc command superchat--builtin-commands))
      (gethash command superchat--user-commands)))

(defun superchat--list-commands-as-string ()
  "Return all available commands as a formatted string with smart alignment.
This separates built-in commands and user-defined prompt files into two sections."
  (let* ((built-in-functional-commands
          '(("commands" . "Show all available commands list")
            ("agent" . "Activate an agent preset")
            ("plan" . "Switch to read-only planning mode")
            ("code" . "Activate the built-in coding Capability")
            ("research" . "Activate the built-in research Capability")
            ("cowork" . "Activate the built-in cowork Capability")
            ("skill" . "Switch to a skill/preset")
            ("compact" . "Compact session history into an anchor")
            ("expand" . "Expand latest anchor back into history")
            ("team" . "Select team Topology, optionally binding a directory")
            ("single" . "Select single-Agent Topology")
            ("authority" . "Select or show the Session Authority policy")
            ("workspace" . "Bind, show, or clear the Session Workspace")
            ("subagent" . "Delegate a task to an isolated sub-agent")
            ("agents" . "List running sub-agents")
            ("cancel-agent" . "Cancel a running sub-agent by id")
            ("reset" . "Reset to default chat mode")
            ("clear-context" . "Clear all files from current session context")
            ("clear" . "Clear chat history and context")
            ("recall" . "Retrieve historical conversations or information from memory")
            ("remember" . "Save current chat or specific content to memory")))
         (built-in-dynamic-commands
          '(("backend" . "Display active llm backend, provider, and model")
            ("models" . "Display list of available language models")
            ("mcp" . "Display MCP (Model Context Protocol) status")
            ("mcp-start" . "Start all active MCP servers")
            ("mcp-start-server" . "Start a specific MCP server by name")
            ("mcp-stop-server" . "Stop a running MCP server by name")
            ("mcp-health" . "Check health of all active MCP servers")))
         (builtin-cmds '())
         (user-cmds '())
         (max-title-len 0)
         (command-dir (superchat--command-dir)))

    ;; 1. Collect Built-in Commands
    (dolist (cmd (append built-in-functional-commands built-in-dynamic-commands))
      (let* ((title (concat "/" (car cmd)))
             (purpose (cdr cmd)))
        (push (list title purpose) builtin-cmds)
        (setq max-title-len (max max-title-len (length title)))))

    ;; Sort built-in commands
    (setq builtin-cmds (sort builtin-cmds (lambda (a b) (string< (car a) (car b)))))

    ;; 2. Collect User-defined Commands
    (when (file-directory-p command-dir)
      (dolist (file (directory-files command-dir t))
        (let ((fname (file-name-nondirectory file)))
          (when (and (file-regular-p file)
                     (not (string-prefix-p "." fname))
                     (not (string-suffix-p "~" fname))
                     (member (file-name-extension file) superchat-prompt-file-extensions))
            (let* ((base-name (file-name-base file))
                   (parts (split-string base-name "-" t))
                   (command-name (car parts)))
              (when (and command-name (not (string-empty-p command-name)))
                (let* ((title (concat "/" command-name))
                       (purpose (if (cdr parts)
                                    (mapconcat #'identity (cdr parts) "-")
                                  "")))
                  (push (list title purpose) user-cmds)
                  (setq max-title-len (max max-title-len (length title))))))))))

    ;; Sort user commands
    (setq user-cmds (sort user-cmds (lambda (a b) (string< (car a) (car b)))))

    ;; 3. Format Output
    (with-temp-buffer
      (insert "Available Commands (type '/command_name' to use)\n\n")

      ;; Section: Built-in Commands
      (insert "-- System Commands --\n")
      (dolist (cmd builtin-cmds)
        (insert (format "%s  %s\n" (superchat--ljust (car cmd) max-title-len) (cadr cmd))))

      ;; Section: User-defined Commands (only if any exist)
      (when user-cmds
        (insert "\n-- User Commands --\n")
        (dolist (cmd user-cmds)
          (insert (format "%s  %s\n" (superchat--ljust (car cmd) max-title-len) (cadr cmd)))))

      (insert "\n-- Command Definition --\n")
      (insert (format "%s  Define a new command, or modify an existing one.\n" (superchat--ljust "/define <name> \"<prompt>\"" max-title-len)))

      (buffer-string))))

;; Helper function for left-justification (similar to Python's str.superchat--ljust)
(defun superchat--ljust (string width &optional padchar)
  "Left-justify STRING to WIDTH with PADCHAR (default is space)."
  (let ((len (length string))
        (padchar (or padchar ?\ )))
    (if (>= len width)
        string
      (concat string (make-string (- width len) padchar)))))

(defun superchat--role-matches (value symbol)
  "Return non-nil when VALUE represents SYMBOL (string, keyword or symbol)."
  (cond
   ((eq value symbol) t)
   ((and (symbolp value)
         (eq value (intern (concat ":" (symbol-name symbol))))))
   ((and (stringp value)
         (string= value (symbol-name symbol))))
   (t nil)))

(defun superchat--last-exchange-struct ()
  "Return plist describing the most recent user/assistant exchange."
  (let* ((history superchat--conversation-history)
         (assistant-msg (car history))
         (user-msg (cadr history)))
    (when (and assistant-msg user-msg
               (superchat--role-matches (plist-get assistant-msg :role) 'assistant)
               (superchat--role-matches (plist-get user-msg :role) 'user))
      (let* ((user-text (or (plist-get user-msg :content) ""))
             (assistant-text (or (plist-get assistant-msg :content) ""))
             (full-exchange (format "User: %s\n\nAssistant: %s" user-text assistant-text))
             (title (superchat-memory-compose-title user-text)))
        (list :user user-text
              :assistant assistant-text
              :content user-text
              :full-exchange full-exchange
              :title title
              :command superchat--current-command)))))

(defun superchat--get-last-exchange ()
  "Get the last user-assistant exchange as formatted text."
  (let ((exchange (superchat--last-exchange-struct)))
    (when exchange
      (or (plist-get exchange :full-exchange)
          (plist-get exchange :content)))))

(defun superchat--get-all-command-names ()
  "Return a list of all available command names, with the leading slash."
  (let ((cmds '("/define" "/commands" "/reset" "/clear-context" "/clear" "/remember" "/recall" "/agent" "/plan" "/team" "/workspace" "/skill" "/compact" "/expand" "/subagent" "/agents" "/cancel-agent" "/skill-install"))) ; Meta commands
    (dolist (cmd superchat--builtin-commands)
      (push (concat "/" (car cmd)) cmds))
    (maphash (lambda (k _v) (push (concat "/" k) cmds))
             superchat--user-commands)
    ;; Add available prompt files
    (let ((available-prompts (superchat--list-available-prompts)))
      (dolist (prompt available-prompts)
        (push (concat "/" prompt) cmds)))
    (sort (delete-dups (mapcar #'identity cmds)) 'string<)))

(defun superchat--replace-prompt-variables (prompt input &optional lang)
  "Replace $input and $lang variables in PROMPT with INPUT and LANG."
  (let ((result prompt))
    (setq result (replace-regexp-in-string "\\$input" (or input "") result))
    (when lang
      (setq result (replace-regexp-in-string "\\$lang" lang result)))
    result))

(defun superchat--completion-at-point ()
  "Provide completion for /commands, @models, >skills and #files at point.
Implements `completion-at-point-functions' for corfu/company integration."
  (let* ((end (point))
         (prompt-start (or superchat--prompt-start (point-min)))
         (text (buffer-substring-no-properties prompt-start end)))
    (cond
     ;; --- Agentic Skills completion (>skill-name) ---
     ((string-match "\\(>[a-zA-Z0-9_-]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start)))
        `(,completion-start ,end ,(when (fboundp 'superchat-skills-completion-list)
                                           (superchat-skills-completion-list))
          . (metadata (category . superchat-skills)))))
     ;; --- File path completion (#path) ---
     ((string-match "\\(#[^[:space:]]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             ;; completion-start after `#' so the hash stays in buffer;
             ;; the completion framework replaces only the path portion.
             ;; `completion-file-name-table' handles the 3-arg protocol
             ;; natively, and the path text does not include `#'.
             (completion-start (+ prompt-start symbol-start 1)))
        `(,completion-start ,end
          ,#'completion-file-name-table
          :exclusive no
          . (metadata (category . file)))))
     ;; --- 保留原有的 @ 和 / 分支 ---
     ;; Matches @word at the end of the string.
     ((string-match "\\(@[a-zA-Z0-9_./:-]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start))
             (models (mapcar (lambda (model)
                               (concat "@" model))
                             (superchat--get-available-models))))
        `(,completion-start ,end ,models
          . (metadata (category . superchat-model)))))
     ;; Matches /word at the end of the string.
     ((string-match "\\(/[^[:space:]]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start)))
        `(,completion-start ,end ,(superchat--get-all-command-names)
          . (metadata (category . superchat))))))))

(defun superchat--parse-define (input)
  "Parse /define command from INPUT."
  (superchat-parser-define input))

(defun superchat--parse-command (input)
  "Parse INPUT, return (command . args) or nil."
  (superchat-parser-command input))

(defun superchat--strip-leading-user-label (input)
  "Strip a leading \"User:\"/\"用户:\" label when it prefixes a slash command.

This supports pasting transcript-like input such as:
  User: /summarize\\n...content
  User:/summarize\\n...content

Returns INPUT unchanged when it doesn't look like a transcript-style label."
  (if (and (stringp input)
           (string-match "\\`\\(?:User\\|用户\\):\\s-*/" input))
      (replace-regexp-in-string "\\`\\(?:User\\|用户\\):\\s-*" "" input)
    input))

;; --- File Path Handling ---
;; Match a file reference introduced by `#`, allowing optional spaces after it,
;; then either a quoted path with spaces, or an unquoted path up to whitespace.
(defconst superchat--file-ref-regexp
  "#\\s-*\\(?:\"\\([^\"]+\\)\"\\|\\(\\(?:~\\|/\\)[^[:space:]#]+\\)\\)"
  "Regexp to capture a file path after a leading '#'.
Captures either a quoted path in group 1, or an unquoted absolute path in group 2.")

;; --- Main Send Logic ---

;; Helpers to inline file contents into the prompt

;; --- Backend introspection ---

(defun superchat-backend-show ()
  "Show active llm backend, provider, model, and tool counts."
  (interactive)
  (let* ((backend superchat-llm-backend)
         (provider-name (cond
                         ((null backend) "UNCONFIGURED")
                         ((fboundp 'llm-name) (superchat--provider-name backend))
                         (t "unknown")))
         (model (or (and (fboundp 'superchat--provider-chat-model)
                         (condition-case nil
                             (superchat--provider-chat-model backend)
                           (error nil)))
                    superchat-llm-model
                    "default"))
         (streaming superchat-llm-streaming)
         (tools (when (fboundp 'superchat-get-llm-tools)
                  (superchat-get-llm-tools)))
         (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                      (superchat-mcp-get-tools)))
         (content
          (concat
           "Backend: llm.el\n"
           (format "Provider: %s\n" provider-name)
           (format "Model: %s\n" model)
           (format "Streaming: %s\n" (if streaming "yes" "no"))
           (format "Tools: %d registered\n" (length tools))
           (format "MCP tools: %d registered\n" (length mcp-tools))
           "\n"
           (if (null backend)
               (concat
                "⚠ Backend not configured.\n"
                "Set `superchat-llm-backend' to a `make-llm-*' struct, e.g.:\n\n"
                "  (setq superchat-llm-backend\n"
                "        (make-llm-openai :key \"sk-...\" :chat-model \"gpt-4o-mini\"))\n")
             ""))))
    (when (called-interactively-p 'interactive)
      (with-help-window "*SuperChat Backend*"
        (with-current-buffer standard-output
          (insert content))))
    content))

(defun superchat-tools-status ()
  "Alias for `superchat-backend-show' (kept for /tools back-compat)."
  (interactive)
  (superchat-backend-show))

;; --- MCP Integration ---

;; The model transport (provider methods, `superchat--callback-in-view',
;; and `superchat--llm-generate-answer') lives in superchat-llm.el.

;; --- Save Conversation ---

(defun superchat--list-commands ()
  "Display available commands in the chat buffer."
  (interactive)
  (superchat--insert-system-message (superchat--list-commands-as-string)))

(defun superchat--smart-hash ()
  "Interactively select a file and insert its path at point, prefixed with '#'."
  (interactive)
  (let* ((prompt-start-pos (and superchat--prompt-start (marker-position superchat--prompt-start)))
         (p (point)))
    (if (or (and prompt-start-pos (>= p prompt-start-pos))
            (save-excursion
              (beginning-of-line)
              (looking-at "\\* User.*?: ")))
        (let ((file (if superchat-default-directories
                        (superchat--read-file-from-default-directories)
                      (read-file-name "Select file for context: "))))
          (when file
            (let* ((path (expand-file-name file))
                   (needs-quote (string-match-p "\\s-" path)))
              (insert (if needs-quote
                          (format "#\"%s\"" path)
                        (concat "#" path))))))
      (message "Smart hash '#' can only be used in the prompt area."))))

(defun superchat--point-in-prompt-p ()
  "Return non-nil when point is in the editable Superchat prompt area."
  (let ((prompt-start-pos (and superchat--prompt-start
                               (marker-position superchat--prompt-start))))
    (or (and prompt-start-pos (>= (point) prompt-start-pos))
        (save-excursion
          (beginning-of-line)
          (looking-at "\\* User.*?: ")))))

(defun superchat--insert-and-complete (char)
  "Insert CHAR and trigger completion in the prompt area."
  (interactive)
  (insert char)
  (when (superchat--point-in-prompt-p)
    (completion-at-point)))

(defun superchat--insert-slash-and-complete ()
  "Insert '/' and trigger Superchat command completion."
  (interactive)
  (superchat--insert-and-complete "/"))

(defun superchat--insert-at-and-complete ()
  "Insert '@' and trigger Superchat model completion."
  (interactive)
  (superchat--insert-and-complete "@"))

(defun superchat--read-file-from-default-directories ()
  "Read a file from user-defined default directories."
  (let* ((files (superchat--collect-files-from-directories superchat-default-directories)))
    (if files
        (let ((file (completing-read "Select file: " files)))
          (when file
            (expand-file-name file)))
      (progn
        (message "No files found in default directories")
        (read-file-name "Select file for context: ")))))

(defun superchat--collect-files-from-directories (directories)
  "Collect files from DIRECTORIES list without recursing into subdirectories.
Only include regular files at the top level of each directory whose
extension is in `superchat-default-file-extensions`. Hidden files are skipped."
  (let ((files '()))
    (dolist (dir directories)
      (let ((full-dir (if (file-directory-p dir) dir (expand-file-name dir))))
        (when (file-directory-p full-dir)
          (condition-case err
              (dolist (file (directory-files full-dir t nil t))
                (when (and (file-regular-p file)
                           (let ((name (file-name-nondirectory file)))
                             (and (not (string-prefix-p "." name))
                                  (let* ((ext (downcase (or (file-name-extension name) ""))))
                                    (member ext superchat-default-file-extensions)))))
                  (push file files)))
            (error
             (message "Warning: Error reading directory %s: %s" full-dir (error-message-string err)))))))
    (nreverse files)))

;; --- Context Management ---
(defvar superchat--current-context-files nil
  "List of files currently added to gptel context for this session.")

(defun superchat--add-file-to-context (file-path)
  "Add FILE-PATH to our session tracking list.
Note: We no longer call gptel-context-add-file directly due to gptel issue #572.
Instead, files are passed to gptel-request via the :context parameter."
  (message "superchat: Tracking file for context: %s" file-path)
  (when (and file-path (file-exists-p file-path))
    (cl-pushnew file-path superchat--current-context-files :test #'equal)))

(defun superchat--clear-session-context ()
  "Clear all files added to context during this session."
  (interactive)
  (when superchat--current-context-files
    ;; Clear our tracking list (no need to call gptel-context-remove since we use :context parameter)
    (setq superchat--current-context-files nil)
    (message "superchat: Session context cleared")))

(defun superchat--clear-chat-and-context ()
  "Clear the chat buffer, conversation history, and session context."
  (interactive)
  (let* ((primary-name superchat-buffer-name)
         (target (if (bound-and-true-p superchat-mode)
                     (current-buffer)
                   (get-buffer-create superchat-buffer-name)))
         (target-name (buffer-name target))
         (superchat-buffer-name target-name))
    (with-current-buffer target
      ;; 1. Summarize the primary session before clearing.
      (when (equal target-name primary-name)
        (superchat--summarize-session-on-exit))

      ;; A cleared View must not reassemble the old Tape context.  Keep the
      ;; old session durable (and cache it above), but start subsequent turns
      ;; on a fresh Session id with no implicit lineage.
      (setq superchat--session-id (superchat--new-session-id))

      ;; 2. Clear this View's file context.
      (superchat--clear-session-context)

      ;; 3. Clear UI, history, and command state in the View.
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert (propertize "#+TITLE: superchat\n" 'face 'font-lock-title-face))
        (setq superchat--conversation-history nil)
        (setq superchat--current-command nil)
        (setq superchat--pending-recalled-memories nil)
        (superchat--insert-prompt))))

  (message "Chat cleared."))

(defun superchat--summarize-session-on-exit ()
  "Get the entire buffer content and cache it for next startup processing."
  ;; The cache is a legacy handoff for the primary chat only.  Secondary
  ;; Session Views are already durable in Tape and must not overwrite it.
  (when (equal (buffer-name) superchat-buffer-name)
    (superchat--ensure-directories)
    (let ((history (buffer-substring-no-properties (point-min) (point-max)))
          (cache-file (expand-file-name "session-cache.org" superchat-data-directory)))
      (when (> (length history) 100)
        (with-temp-file cache-file
          (insert history))
        (message "Superchat session cached for next startup.")))))

(defun superchat--summarize-session-before-emacs-exit ()
  "Ensure the superchat session is cached when Emacs exits."
  (let ((buffer (get-buffer superchat-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (condition-case err
            (superchat--summarize-session-on-exit)
          (error
           (ignore-errors
             (let ((log-file (expand-file-name "session-cache-errors.log" superchat-data-directory)))
               (with-temp-buffer
                 (insert (format-time-string "[%Y-%m-%d %H:%M:%S] \n"))
                 (insert (format "Failed to cache session on exit: %s\n" (error-message-string err)))
                 (write-region (point-min) (point-max) log-file 'append 'silent))))
           (message "superchat: failed to cache session on exit (%s)"
                    (error-message-string err))))))))

(defun superchat--process-cached-session-on-startup ()
  "Check for and process a cached session file on startup."
  (let ((cache-file (expand-file-name "session-cache.org" superchat-data-directory)))
    (when (file-exists-p cache-file)
      (condition-case err
          (let ((history (with-temp-buffer
                           (insert-file-contents cache-file)
                           (buffer-string))))
            (message "Superchat: Processing cached session from last exit...")
            (let ((request (when (fboundp 'superchat-memory-summarize-session-history)
                              (superchat-memory-summarize-session-history history))))
              (if request
                  (progn
                    (delete-file cache-file)
                    (message "Superchat: Session handoff to memory summarizer queued."))
                (message "Superchat: Memory summarizer unavailable; cached session left on disk."))))
        (error
         (ignore-errors
           (let ((log-file (expand-file-name "session-cache-errors.log" superchat-data-directory)))
             (with-temp-buffer
               (insert (format-time-string "[%Y-%m-%d %H:%M:%S] \n"))
               (insert (format "Failed to process cached session: %s\n" (error-message-string err)))
               (write-region (point-min) (point-max) log-file 'append 'silent))))
         (message "superchat: failed to process cached session (%s); cache preserved."
                  (error-message-string err)))))))

;; --- Session Views ---

(defun superchat--new-session-id ()
  "Return a compact, time-sortable Session identifier."
  (format "%s%04x"
          (format-time-string "%Y%m%d-%H%M%S-")
          (random 65536)))

(defun superchat--session-buffer-name (session-id)
  "Return the View buffer name for SESSION-ID."
  (format "*superchat:%s*" session-id))

(defun superchat--ensure-chat-view (buffer-name &optional session-id)
  "Return an initialized chat View named BUFFER-NAME.
When SESSION-ID is non-nil, associate the View with that Session."
  (superchat--ensure-directories)
  (superchat--load-user-commands)
  (let ((buffer (get-buffer-create buffer-name))
        (superchat-buffer-name buffer-name))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (unless superchat-mode
          (superchat-mode 1))
        (when session-id
          (setq superchat--session-id session-id))
        (unless superchat--session-id
          (setq superchat--session-id (superchat--new-session-id)))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: superchat\n"
                              'face 'font-lock-title-face)))
        (unless (and superchat--prompt-start
                     (marker-position superchat--prompt-start))
          (superchat--insert-prompt))))
    buffer))

(defun superchat--display-chat-view (buffer)
  "Display BUFFER as a chat View and return it."
  (let ((window (display-buffer buffer)))
    (when (window-live-p window)
      (select-window window)
      (when superchat-display-single-window
        (delete-other-windows))))
  buffer)

(defun superchat--render-session-view (session-id)
  "Reconstruct SESSION-ID from Tape and return its View buffer."
  (let* ((buffer-name (superchat--session-buffer-name session-id))
         (buffer (superchat--ensure-chat-view buffer-name session-id))
         (rows (superchat-view-session-history session-id))
         (superchat-buffer-name buffer-name))
    (with-current-buffer buffer
      (superchat--restore-session-runtime-configuration session-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq superchat--conversation-history nil
              superchat--response-start-marker nil
              superchat--assistant-response-start-marker nil
              superchat--prompt-start nil
              superchat--current-command nil)
        (setq-local superchat--current-context-files nil)
        (insert (propertize
                 (format "#+TITLE: superchat — %s\n" session-id)
                 'face 'font-lock-title-face))
        (dolist (row rows)
          (let ((id (nth 0 row))
                (topic (nth 1 row))
                (kind (nth 2 row))
                (content (or (nth 3 row) "")))
            (pcase kind
              ("user"
               (insert (format "\n* User: %s\n" content))
               (push (list :role "user" :content content :id id)
                     superchat--conversation-history))
              ("assistant"
               (insert "\n** Assistant\n"
                       (superchat-render-markdown-to-org content)
                       "\n")
               (push (list :role "assistant" :content content :id id)
                     superchat--conversation-history))
              ("system"
               (insert (if (equal topic "attachment")
                           "\n** Attachment\n"
                         "\n** System\n")
                       content "\n"))
              ("anchor"
               (insert "\n** System\n" content "\n")))))
        (add-text-properties (point-min) (point-max) '(read-only t))
        (superchat--insert-prompt)
        ;; A View rebuild is the first point where this Session is observed
        ;; after a possible process crash.  Reconciliation is self-contained
        ;; and failure-safe; it never attempts to resume execution.
        (when (fboundp 'superchat-runtime-reconcile-session)
          (ignore-errors
            (superchat-runtime-reconcile-session session-id)))
        ;; A plan under review survives on the Tape while this rebuild
        ;; erased its inline review block; restore the review.
        (when (require 'superchat-plan nil t)
          (superchat-plan--restore-on-reopen session-id))))
    buffer))

(defun superchat-new-session ()
  "Create, display, and return a new independent Session View."
  (interactive)
  (let* ((session-id (superchat--new-session-id))
         (buffer (superchat--ensure-chat-view
                  (superchat--session-buffer-name session-id)
                  session-id)))
    (with-current-buffer buffer
      (setq-local superchat--current-context-files nil))
    (superchat--display-chat-view buffer)))

(defun superchat-open-session (session-id)
  "Open SESSION-ID as a reconstructed, independent chat View."
  (interactive
   (let ((session-ids (superchat-view-session-ids)))
     (unless session-ids
       (user-error "No persisted Superchat sessions"))
     (list (completing-read "Open Superchat session: "
                            session-ids nil t))))
  (let* ((buffer-name (superchat--session-buffer-name session-id))
         (buffer (or (get-buffer buffer-name)
                     (superchat--render-session-view session-id))))
    (superchat--display-chat-view buffer)))

(defun superchat--branch-turn-choices ()
  "Return completion choices for branchable Turns in the current Session."
  (unless superchat--session-id
    (user-error "This View has no Session"))
  (mapcar
   (lambda (row)
     (let ((id (nth 0 row))
           (kind (nth 2 row))
           (content (replace-regexp-in-string
                     "[\n\r]+" " " (or (nth 3 row) ""))))
       (cons (format "#%s %-9s %s"
                     id kind (truncate-string-to-width content 60 nil nil "…"))
             id)))
   (superchat-view-entries superchat--session-id nil
                           '("user" "assistant"))))

(defun superchat--read-branch-turn-id ()
  "Read a branch point from the current Session."
  (let ((choices (superchat--branch-turn-choices)))
    (unless choices
      (user-error "This Session has no Turns to branch from"))
    (cdr (assoc (completing-read "Branch after Turn: " choices nil t)
                choices))))

(defun superchat-branch-session (&optional parent-turn-id)
  "Branch the current Session after PARENT-TURN-ID and open the child View.
When called non-interactively with nil, branch after the latest Turn."
  (interactive (list (superchat--read-branch-turn-id)))
  (unless superchat--session-id
    (user-error "This View has no Session"))
  (let* ((parent-session-id superchat--session-id)
         (rows (superchat-view-entries parent-session-id nil
                                       '("user" "assistant")))
         (parent-turn-id (or parent-turn-id
                             (car (car (last rows)))))
         (parent-row (cl-find parent-turn-id rows :key #'car :test #'=)))
    (unless parent-row
      (user-error "Turn %s does not belong to Session %s"
                  parent-turn-id parent-session-id))
    (let* ((session-id (superchat--new-session-id))
           (source-ids (vconcat
                        (mapcar #'car
                                (cl-remove-if
                                 (lambda (row) (> (car row) parent-turn-id))
                                 rows)))))
      (superchat-tape-append-event
       (list :type "anchor.created"
             :session-id session-id
             :actor-id "user"
             :topic "branch"
             :summary (format "Branched from %s" parent-session-id)
             :requires-payload t
             :payload
             (list :parent_session_id parent-session-id
                   :parent_turn_id parent-turn-id
                   :source_ids source-ids)))
      (superchat-open-session session-id))))

;;;###autoload
(defun superchat (&optional preset-name)
  "Open or switch to the superchat buffer.
With a prefix argument (C-u), prompt for a skill/preset to activate
in the new or existing session.  PRESET-NAME may be supplied
programmatically to launch with a specific preset."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Superchat preset/skill: "
                            (superchat-skills-get-available)
                            nil t))))
  ;; Keep legacy prompt substitution aligned with the active language.
  (when (fboundp 'superchat-executor-initialize)
    (superchat-executor-initialize :lang superchat-lang))
  (superchat--ensure-directories)
  (superchat--load-user-commands)
  ;; Rehydrate hash-approved synthesized tools (Phase 2); silent when
  ;; everything on disk matches what the user once reviewed.
  (when (fboundp 'superchat-synthesis-rehydrate)
    (ignore-errors (superchat-synthesis-rehydrate)))
  (superchat--process-cached-session-on-startup)
  (let ((buffer (get-buffer-create superchat-buffer-name))
        (preset (when (and preset-name (not (string-empty-p preset-name)))
                  (superchat-skills-load preset-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (superchat-mode 1) ; Ensure the minor mode is turned on
        (unless superchat--session-id
          (setq superchat--session-id (superchat--new-session-id)))
        (when preset
          (superchat--activate-capability preset)
          (message "🎯 Active preset: %s (%s)"
                   (superchat-preset-name preset)
                   (superchat-preset-type preset)))
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: superchat\n" 'face 'font-lock-title-face)))
        (superchat--insert-prompt)))
    (let ((window (display-buffer buffer)))
      (select-window window)
      (when superchat-display-single-window
        (delete-other-windows)))))

;; ── Universal request entry + inspector ──

(defvar-local superchat--inspected-request-plan nil)
(defvar-local superchat--request-prompt-start nil)
(defvar-local superchat--request-prompt-end nil)
(defvar llm-log nil)

(defvar superchat-request-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map text-mode-map)
    (define-key map (kbd "C-c C-c") #'superchat-request-send)
    (define-key map (kbd "C-c C-w") #'superchat-request-copy)
    (define-key map (kbd "C-c C-t") #'superchat-request-toggle-log)
    (define-key map (kbd "C-c C-l") #'superchat-request-open-log)
    (define-key map (kbd "C-c C-k") #'superchat-request-cancel)
    map)
  "Keymap for `superchat-request-mode'.")

(define-derived-mode superchat-request-mode text-mode "Superchat-Request"
  "Mode for inspecting and editing one prepared Superchat request."
  (setq-local header-line-format
              "C-c C-c send; C-c C-w copy; C-c C-t toggle log; C-c C-l open log; C-c C-k cancel"))

(defun superchat--ensure-request-chat-buffer ()
  "Return an initialized chat buffer without changing selected windows."
  (superchat--ensure-chat-view superchat-buffer-name))

(defun superchat--capture-origin ()
  "Capture the current Emacs location as a stable request origin plist."
  (list :buffer (buffer-name)
        :file (buffer-file-name)
        :major-mode major-mode
        :point (point)
        :region (when (use-region-p)
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))

(defun superchat--request-tool-names (plan)
  "Return safe display names for tools in request PLAN."
  (mapcar
   (lambda (tool)
     (cond
      ((stringp tool) tool)
      ((fboundp 'llm-tool-name)
       (condition-case nil
           (llm-tool-name tool)
         (error (format "%S" tool))))
      (t (format "%S" tool))))
   (or (plist-get plan :tools) nil)))

(defun superchat--request-attachment-description (file)
  "Return a display description for attachment FILE."
  (let* ((image (superchat--image-attachment-info file))
         (size (or (plist-get image :size)
                   (file-attribute-size (file-attributes file)))))
    (format "%s (%s, %s)"
            file
            (or (plist-get image :mime-type) "context")
            (if size (format "%d bytes" size) "missing"))))

(defun superchat--request-plan-copy-text (plan)
  "Return a credential-free textual representation of PLAN."
  (format
   (concat "Session: %s\nModel: %s\nPreset: %s\nOrigin: %S\n"
           "Tools: %s\nAttachments: %s\n\n"
           "System prompt:\n%s\n\nPrompt:\n%s\n")
   (or (plist-get plan :session-id) "")
   (or (plist-get plan :target-model) "backend default")
   (let ((preset (plist-get plan :preset)))
     (if preset (superchat-preset-name preset) "none"))
   (plist-get plan :origin)
   (string-join (superchat--request-tool-names plan) ", ")
   (string-join
    (mapcar #'superchat--request-attachment-description
            (or (plist-get plan :context-files) nil))
    "\n             ")
   (or (plist-get plan :system-prompt) "")
   (or (plist-get plan :prompt) "")))

(defun superchat--display-request-plan (plan)
  "Display editable request PLAN and return its inspector buffer."
  (let ((buffer (get-buffer-create "*superchat-request*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (superchat-request-mode)
        (setq-local superchat--inspected-request-plan plan)
        (let ((start (point)))
          (insert "Superchat Request Plan\n======================\n\n")
          (insert (format "Session: %s\n"
                          (or (plist-get plan :session-id) "")))
          (insert (format "Model: %s\n"
                          (or (plist-get plan :target-model)
                              "backend default")))
          (insert (format "Origin: %S\n" (plist-get plan :origin)))
          (insert (format "Tools: %s\n"
                          (string-join
                           (superchat--request-tool-names plan) ", ")))
          (insert (format "llm.el log: %s (structured prompt/response, not provider HTTP JSON)\n"
                          (if llm-log "enabled" "disabled")))
          (insert "Attachments:\n")
          (dolist (file (plist-get plan :context-files))
            (insert "  " (superchat--request-attachment-description file) "\n"))
          (insert "\n")
          (insert "System prompt (read-only):\n")
          (insert (or (plist-get plan :system-prompt) ""))
          (insert "\n\nPrompt (editable):\n")
          (add-text-properties
           start (point) '(read-only t rear-nonsticky (read-only))))
        (setq-local superchat--request-prompt-start (point-marker))
        (insert (or (plist-get plan :prompt) ""))
        (setq-local superchat--request-prompt-end (point-marker))
        (set-marker-insertion-type superchat--request-prompt-end t)
        (let ((start (point)))
          (insert "\n\n")
          (insert-text-button
           "Send"
           'follow-link t
           'help-echo "Execute this exact request plan"
           'action (lambda (_button) (superchat-request-send)))
          (insert "   ")
          (insert-text-button
           "Copy"
           'follow-link t
           'help-echo "Copy a credential-free request description"
           'action (lambda (_button) (superchat-request-copy)))
          (insert "   ")
          (insert-text-button
           "Toggle Log"
           'follow-link t
           'help-echo "Toggle llm.el structured request logging"
           'action (lambda (_button) (superchat-request-toggle-log)))
          (insert "   ")
          (insert-text-button
           "Open Log"
           'follow-link t
           'help-echo "Open *llm log*"
           'action (lambda (_button) (superchat-request-open-log)))
          (insert "   ")
          (insert-text-button
           "Cancel"
           'follow-link t
           'help-echo "Discard this request plan"
           'action (lambda (_button) (superchat-request-cancel)))
          (insert "\n")
          (add-text-properties
           start (point)
           '(read-only t front-sticky nil rear-nonsticky (read-only))))
        (remove-text-properties
         superchat--request-prompt-start superchat--request-prompt-end
         '(read-only nil front-sticky nil rear-nonsticky nil))
        (goto-char superchat--request-prompt-start)))
    (display-buffer buffer)
    buffer))

(defun superchat--request-plan-with-edited-prompt ()
  "Update and return the request plan in the current inspector buffer."
  (unless (derived-mode-p 'superchat-request-mode)
    (user-error "Not in a Superchat request inspector"))
  (let ((prompt (buffer-substring-no-properties
                 superchat--request-prompt-start
                 superchat--request-prompt-end)))
    (when (string-empty-p (string-trim prompt))
      (user-error "Request prompt is empty"))
    (plist-put superchat--inspected-request-plan :prompt prompt)
    superchat--inspected-request-plan))

(defun superchat--execute-plan-in-chat (plan)
  "Render the user side of PLAN and execute it in the chat buffer."
  (let ((buffer (superchat--ensure-request-chat-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (input (or (plist-get plan :input) "")))
        (goto-char superchat--prompt-start)
        (delete-region (point) (point-max))
        (insert input)
        (put-text-property superchat--prompt-start (point-max) 'read-only t)
        (put-text-property superchat--prompt-start (point-max)
                           'superchat-role 'user)
        (setq superchat--prompt-start nil)
        (when superchat-show-ttft
          (setq superchat--ttft-start-time (float-time)))
        (superchat--prepare-for-response)))
    (display-buffer buffer)
    (with-current-buffer buffer
      (superchat-execute-request-plan plan))))

(defun superchat-submit (input &optional origin inspect)
  "Submit INPUT from ORIGIN, or display it when INSPECT is non-nil.
Returns the prepared Request Plan in both cases."
  (let* ((buffer (superchat--ensure-request-chat-buffer))
         (plan (with-current-buffer buffer
                 (superchat-build-request-plan
                  input superchat--session-id origin))))
    (if inspect
        (superchat--display-request-plan plan)
      (superchat--execute-plan-in-chat plan))
    plan))

(defun superchat--read-act-instruction ()
  "Read an instruction without nesting an active minibuffer."
  (if (minibufferp)
      (minibuffer-contents-no-properties)
    (read-string "Superchat instruction: ")))

;;;###autoload
(defun superchat-act (instruction &optional inspect)
  "Send INSTRUCTION from any Emacs buffer.
The active region is included as context.  With prefix argument INSPECT,
show the exact request plan before sending."
  (interactive
   (list (superchat--read-act-instruction) current-prefix-arg))
  (when (string-empty-p (string-trim (or instruction "")))
    (user-error "Instruction is empty"))
  (superchat-submit instruction (superchat--capture-origin) inspect))

;;;###autoload
(defun superchat-inspect-request (instruction)
  "Build and inspect INSTRUCTION without calling the LLM backend."
  (interactive (list (superchat--read-act-instruction)))
  (superchat-act instruction t))

(defun superchat-request-send ()
  "Send the edited request in the current inspector buffer."
  (interactive)
  (let ((plan (superchat--request-plan-with-edited-prompt))
        (inspector (current-buffer)))
    (superchat--execute-plan-in-chat plan)
    (when (buffer-live-p inspector)
      (kill-buffer inspector))))

(defun superchat-request-copy ()
  "Copy the edited request in the current inspector buffer."
  (interactive)
  (let ((plan (superchat--request-plan-with-edited-prompt)))
    (kill-new (superchat--request-plan-copy-text plan))
    (message "Superchat request copied without provider credentials.")))

(defun superchat-request-toggle-log ()
  "Toggle native `llm-log' and refresh the request inspector."
  (interactive)
  (when (derived-mode-p 'superchat-request-mode)
    (superchat--request-plan-with-edited-prompt))
  (setq llm-log (not llm-log))
  (when (derived-mode-p 'superchat-request-mode)
    (superchat--display-request-plan superchat--inspected-request-plan))
  (message "llm.el request logging %s%s"
           (if llm-log "enabled" "disabled")
           (if llm-log "; prompts and responses are stored in *llm log*" "")))

(defun superchat-request-open-log ()
  "Display llm.el's native request log."
  (interactive)
  (if-let* ((buffer (get-buffer "*llm log*")))
      (display-buffer buffer)
    (user-error "No llm.el log entries yet; enable logging before sending")))

(defun superchat-request-cancel ()
  "Discard the request in the current inspector buffer."
  (interactive)
  (quit-window t))

;; ── Region / Defun Context Sending ──

(defcustom superchat-send-include-diagnostics t
  "When non-nil, inject flymake/eglot diagnostics into sent context.
Applies to `superchat-send-region', `superchat-send-defun', and
`superchat-send-region-or-defun'.  Diagnostics are formatted as
`line:col severity message' and appended after the code block.
When the region has no diagnostics, nothing is appended."
  :type 'boolean
  :group 'superchat)

(declare-function project-current "project" (&optional maybe-prompt dir))
(declare-function project-root "project" (project))
(declare-function flymake-diagnostics "flymake" (beg end))
(declare-function flymake-diagnostic-beg "flymake" (diag))
(declare-function flymake-diagnostic-end "flymake" (diag))
(declare-function flymake-diagnostic-type "flymake" (diag))
(declare-function flymake-diagnostic-text "flymake" (diag))

(defun superchat--project-root ()
  "Return the project root directory for the current buffer.
Uses `project-current' (built-in); returns nil when the buffer is
not inside a recognised project."
  (when (fboundp 'project-current)
    (when-let ((proj (project-current)))
      (condition-case nil
          (project-root proj)
        (error nil)))))

(defun superchat--region-diagnostics (beg end)
  "Collect flymake diagnostics overlapping BEG..END.
Returns a formatted string suitable for appending after a code block,
or an empty string when there are none.  Format: `line:col severity message'.
Automatically covers eglot diagnostics (eglot reports through flymake)."
  (if (and (fboundp 'flymake-diagnostics)
           superchat-send-include-diagnostics
           beg end)
      (let* ((diags (condition-case nil
                        (flymake-diagnostics beg end)
                      (error nil)))
             (relevant (cl-remove-if-not
                        (lambda (d)
                          (let ((d-beg (flymake-diagnostic-beg d))
                                (d-end (flymake-diagnostic-end d)))
                            (and d-beg d-end
                                 (<= d-beg end)
                                 (>= d-end beg))))
                        diags)))
        (if (not relevant)
            ""
          (concat
           "\n;; ── Flymake diagnostics ──\n"
           (mapconcat
            (lambda (d)
              (let* ((line (line-number-at-pos
                            (flymake-diagnostic-beg d)))
                     (col (1+ (current-column)))
                     (severity (pcase (flymake-diagnostic-type d)
                                 (:error "ERROR")
                                 (:warning "WARNING")
                                 (:note "NOTE")
                                 (_ "DIAG")))
                     (msg (flymake-diagnostic-text d)))
                (save-excursion
                  (goto-char (flymake-diagnostic-beg d))
                  (setq col (1+ (- (point) (line-beginning-position)))))
                (format "%d:%d %s %s" line col severity msg)))
            relevant "\n")
           "\n")))
    ""))

(defun superchat--file-relative-to-project (file-path)
  "Return FILE-PATH relative to the current project root.
Returns FILE-PATH unchanged when there is no project root."
  (if-let ((root (superchat--project-root)))
      (file-relative-name file-path root)
    file-path))

;;;###autoload
(defun superchat-send-region (beg end &optional instruction)
  "Send the selected region (BEG to END) as context to the superchat buffer.
Prompts for an optional INSTRUCTION.  Inserts the region content
with the project root, relative file path, and flymake/eglot
diagnostics into the prompt area.  Locks tool search scope to the
source buffer's project root via `superchat-tools-default-directory'."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Instruction (optional): "))
     (user-error "No active region")))
  (let* ((code (buffer-substring-no-properties beg end))
         (source-buffer (buffer-name))
         (file-path (or (buffer-file-name) ""))
         (mode-name (symbol-name major-mode))
         (project-root (superchat--project-root))
         (rel-path (if file-path
                       (superchat--file-relative-to-project file-path)
                     source-buffer))
         (diag-text (superchat--region-diagnostics beg end))
         (header (concat
                  (when project-root
                    (format ";; project: %s\n" project-root))
                  (when file-path
                    (format ";; file: %s\n" rel-path))))
         (context-text
          (format "```%s\n;; from buffer: %s\n%s%s\n```%s\n"
                  mode-name source-buffer header code diag-text)))
    ;; Record source for apply-code-block-at-point.
    (when (fboundp 'superchat-rewrite--record-source)
      (superchat-rewrite--record-source source-buffer beg end))
    ;; Lock tool search scope to the source project.
    (when (fboundp 'superchat-tools-default-directory)
      (setq superchat-tools-default-directory
            (or project-root default-directory)))
    (superchat--insert-context-into-chat context-text instruction
                                         source-buffer mode-name)))

;;;###autoload
(defun superchat-send-defun (&optional instruction)
  "Send the enclosing function definition as context to the superchat buffer.
Uses `treesit' to find the enclosing function node when supported by
the current major-mode; falls back to `beginning-of-defun' /
`end-of-defun'.  Prompts for an optional INSTRUCTION.
Includes project root, relative file path, and flymake/eglot
diagnostics.  Locks tool search scope to the source buffer's
project root via `superchat-tools-default-directory'."
  (interactive "sInstruction (optional): ")
  (let* ((bounds (superchat--get-enclosing-defun-bounds))
         (code (if bounds
                   (buffer-substring-no-properties (car bounds) (cdr bounds))
                 (user-error "Could not determine enclosing defun bounds")))
         (source-buffer (buffer-name))
         (file-path (or (buffer-file-name) ""))
         (mode-name (symbol-name major-mode))
         (project-root (superchat--project-root))
         (rel-path (if file-path
                       (superchat--file-relative-to-project file-path)
                     source-buffer))
         (diag-text (if bounds
                        (superchat--region-diagnostics (car bounds) (cdr bounds))
                      ""))
         (header (concat
                  (when project-root
                    (format ";; project: %s\n" project-root))
                  (when file-path
                    (format ";; file: %s\n" rel-path))))
         (context-text
          (format "```%s\n;; from buffer: %s\n%s%s\n```%s\n"
                  mode-name source-buffer header code diag-text)))
    ;; Record source for apply-code-block-at-point.
    (when (and (fboundp 'superchat-rewrite--record-source)
               bounds)
      (superchat-rewrite--record-source source-buffer
                                        (car bounds) (cdr bounds)))
    ;; Lock tool search scope to the source project.
    (when (fboundp 'superchat-tools-default-directory)
      (setq superchat-tools-default-directory
            (or project-root default-directory)))
    (superchat--insert-context-into-chat context-text instruction
                                         source-buffer mode-name)))

;;;###autoload
(defun superchat-send-region-or-defun (&optional instruction)
  "Send active region or enclosing defun as context to superchat.
If region is active, behaves like `superchat-send-region'.
Otherwise, behaves like `superchat-send-defun'."
  (interactive
   (list (read-string "Instruction (optional): ")))
  (if (use-region-p)
      (superchat-send-region (region-beginning) (region-end) instruction)
    (superchat-send-defun instruction)))

(defun superchat--get-enclosing-defun-bounds ()
  "Return (BEG . END) of the enclosing function definition.
Tries treesit first for tree-sitter-enabled modes; falls back to
`beginning-of-defun' / `end-of-defun'."
  (or (superchat--treesit-defun-bounds)
      (superchat--traditional-defun-bounds)))

(defun superchat--treesit-defun-bounds ()
  "Return (BEG . END) for the enclosing function node via tree-sitter.
Returns nil when tree-sitter is unavailable or no function node is found."
  (when (and (fboundp 'treesit-node-at)
             (treesit-available-p))
    (let ((node (treesit-node-at (point))))
      (when node
        (let ((fn-node (treesit-parent-while
                        node
                        (lambda (n)
                          (not (member (treesit-node-type n)
                                       '("function_definition"
                                         "method_definition"
                                         "function_declaration"
                                         "method_declaration"
                                         "function"
                                         "arrow_function"
                                         "function_item"
                                         "impl_item"
                                         "defun")))))))
          (when fn-node
            (cons (treesit-node-start fn-node)
                  (treesit-node-end fn-node))))))))

(defun superchat--traditional-defun-bounds ()
  "Return (BEG . END) for the enclosing defun via traditional navigation.
Uses `beginning-of-defun' and `end-of-defun'."
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-defun)
          (let ((beg (point)))
            (end-of-defun)
            (cons beg (point))))
      (error nil))))

(defun superchat--insert-context-into-chat (context-text instruction
                                                         source-buffer src-mode-name)
  "Insert CONTEXT-TEXT with optional INSTRUCTION into the superchat prompt.
SOURCE-BUFFER and SRC-MODE-NAME are used for the header message."
  (let* ((inst (if (and instruction (not (string-empty-p instruction)))
                   instruction
                 "Please examine the following code:"))
         (buffer (get-buffer-create superchat-buffer-name)))
    ;; Ensure the buffer exists and has a prompt
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and superchat--prompt-start
                        (marker-position superchat--prompt-start))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (unless (derived-mode-p 'org-mode)
            (org-mode))
          (unless superchat-mode
            (superchat-mode 1))
          (goto-char (point-max))
          (when (= (point-min) (point-max))
            (insert (propertize "#+TITLE: superchat\n" 'face 'font-lock-title-face)))
          (superchat--insert-prompt))))
    (with-current-buffer buffer
      (when superchat--prompt-start
        (goto-char superchat--prompt-start)
        (insert (format "[Context from buffer %S (%S)]\n" source-buffer src-mode-name))
        (insert context-text)
        (insert inst)
        (goto-char (point-max))))
    ;; Display and select the buffer
    (let ((window (display-buffer buffer)))
      (select-window window)
      (goto-char (point-max)))
    ;; Auto-send: behave as if the user pressed C-c C-c, but only when a
    ;; backend is actually configured — otherwise leave the text in the
    ;; prompt for the user (and keep non-interactive callers side-effect free).
    (when superchat-llm-backend
      (with-current-buffer buffer
        (superchat-send-input)))))

;; --- UI Commands and Mode Definition ---

(defvar superchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-send-input)
    (define-key map (kbd "/") #'superchat--insert-slash-and-complete)
    (define-key map (kbd "@") #'superchat--insert-at-and-complete)
    (define-key map (kbd "#") #'superchat--smart-hash)
    (define-key map (kbd "C-c C-h") #'superchat--list-commands)
    (define-key map (kbd "C-c C-s") #'superchat--save-conversation)
    (define-key map (kbd "C-c C-d") #'superchat-dispatch)
    (define-key map (kbd "C-c C-k") #'superchat-cancel)
    (define-key map (kbd "C-c C-p") #'superchat-pause)
    (define-key map (kbd "C-c C-r") #'superchat-resume)
    (define-key map (kbd "C-c C-q") #'superchat-queue-follow-up)
    (define-key map (kbd "C-c C-g") #'superchat-agents-panel-show)
    (define-key map (kbd "C-c C-a") #'superchat-rewrite-apply-code-block-at-point)
    map)
  "Keymap for superchat-mode.")

(define-minor-mode superchat-mode
  "A minor mode to provide chat functionalities in an Org buffer."
  :init-value nil
  :lighter (:eval (format " SChat[%s%s]"
                          superchat-authority-mode
                          (or (superchat--mode-line-run-state) "")))
  :keymap superchat-mode-map
  (if superchat-mode
      ;; When turning the mode on
      (progn
        (unless superchat--header-line-installed-p
          (setq-local superchat--saved-header-line-format header-line-format)
          (setq-local superchat--header-line-installed-p t))
        (setq-local header-line-format
                    '(" " (:eval (superchat--header-line-run-state))))
        (setq-local completion-at-point-functions '(superchat--completion-at-point))
        ;; Configure corfu/company for popup completion on / @ > prefixes.
        ;; corfu reads completion-at-point-functions natively;
        ;; company uses company-capf backend to do the same.
        ;; Setting corfu-auto-prefix to 0 means corfu pops up even on
        ;; single-char input — good for /command, @model, >skill triggers.
        (when (fboundp 'corfu-mode)
          (setq-local corfu-auto-prefix 0)
          (setq-local corfu-auto t)
          (corfu-mode 1))
        (when (fboundp 'company-mode)
          (setq-local company-backends '((company-capf :with company-dabbrev-code)))
          (company-mode 1))
        (add-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit nil t)
        ;; Pre-fetch model list in background so first TAB completion is instant.
        ;; llm-models makes a synchronous HTTP call for real backends;
        ;; doing it now (while Emacs is idle) avoids blocking the UI later.
        (unless (or superchat-manual-models
                    superchat--model-list-cache)
          (run-with-idle-timer
           1 nil
           (lambda ()
             (when (and (buffer-live-p (get-buffer superchat-buffer-name))
                        (boundp 'superchat-llm-backend)
                        superchat-llm-backend
                        (fboundp 'llm-models))
               (condition-case nil
                   (superchat--get-available-models)
                 (error nil)))))))
    ;; When turning the mode off
    (when superchat--header-line-installed-p
      (setq-local header-line-format superchat--saved-header-line-format)
      (setq-local superchat--header-line-installed-p nil))
    (kill-local-variable 'completion-at-point-functions)
    (kill-local-variable 'corfu-auto-prefix)
    (kill-local-variable 'corfu-auto)
    (kill-local-variable 'company-backends)
    (when (fboundp 'corfu-mode) (corfu-mode -1))
    (when (fboundp 'company-mode) (company-mode -1))
    (remove-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit t)))

(add-hook 'kill-emacs-hook #'superchat--summarize-session-before-emacs-exit)

;; ── Transient Dispatch Menu ──

(declare-function transient-define-prefix "transient")
(declare-function transient-setup "transient")

(defun superchat-dispatch ()
  "Open the Superchat transient dispatch menu.
Uses the built-in `transient' library (Emacs 28+) when available;
falls back to a `completing-read' text menu otherwise."
  (interactive)
  (if (fboundp 'superchat--dispatch-menu-transient)
      (call-interactively #'superchat--dispatch-menu-transient)
    (superchat--fallback-dispatch)))

(defun superchat--dispatch-menu-transient ()
  "Stub replaced at runtime when transient is available."
  (interactive)
  (superchat--fallback-dispatch))

;;;###autoload (autoload 'superchat--dispatch-menu-transient "superchat" nil t)
(eval-after-load 'transient
  '(progn
     (transient-define-prefix superchat--dispatch-menu-transient ()
       "Superchat command dispatch menu."
       :value '("--")
       [["Model"
         ("m" "Switch model" superchat--transient-switch-model)
         ("e" "Reasoning effort" superchat--transient-set-reasoning)]
        ["Skill / Command"
         ("s" ">skill (Agentic Skill)" superchat--transient-pick-skill)
         ("c" "/command" superchat--transient-pick-command)]
        ["Context"
         ("f" "#file (attach)" superchat--smart-hash)
         ("r" "send-region-or-defun" superchat-send-region-or-defun)
         ("w" "rewrite-region" superchat-rewrite-region)]
        ["Session"
         ("n" "New session" superchat-new-session)
         ("o" "Open session" superchat-open-session)
         ("h" "Browse history" superchat-sessions)
         ("b" "Branch session" superchat-branch-session)
         ("v" "Save conversation" superchat--save-conversation)
         ("k" "Clear context" superchat--clear-chat-and-context)]])))

(defun superchat--fallback-dispatch ()
  "Fallback text menu when transient is not available."
  (interactive)
  (let ((choice
         (completing-read
          "Superchat action: "
          '("Switch model (m)"
            "Reasoning effort (e)"
            "Skill (s)"
            "Command (c)"
            "#file attach (f)"
            "send-region-or-defun (r)"
            "rewrite-region (w)"
            "New session (n)"
            "Open session (o)"
            "Browse history (h)"
            "Branch session (b)"
            "Save conversation (v)"
            "Clear context (k)")
          nil t)))
    (pcase choice
      ("Switch model (m)" (superchat--transient-switch-model))
      ("Reasoning effort (e)" (superchat--transient-set-reasoning))
      ("Skill (s)" (superchat--transient-pick-skill))
      ("Command (c)" (superchat--transient-pick-command))
      ("#file attach (f)" (call-interactively #'superchat--smart-hash))
      ("send-region-or-defun (r)" (call-interactively #'superchat-send-region-or-defun))
      ("rewrite-region (w)" (call-interactively #'superchat-rewrite-region))
      ("New session (n)" (call-interactively #'superchat-new-session))
      ("Open session (o)" (call-interactively #'superchat-open-session))
      ("Browse history (h)" (call-interactively #'superchat-sessions))
      ("Branch session (b)" (call-interactively #'superchat-branch-session))
      ("Save conversation (v)" (call-interactively #'superchat--save-conversation))
      ("Clear context (k)" (call-interactively #'superchat--clear-chat-and-context)))))

(defun superchat--transient-switch-model ()
  "Prompt for a model and switch to it."
  (interactive)
  (let ((models (and (fboundp 'superchat--get-available-models)
                     (superchat--get-available-models))))
    (if models
        (let ((model (completing-read "Switch to model: " models nil t)))
          (unless (string-empty-p model)
            (setq superchat-llm-model model)
            (message "Superchat model: %s" model)))
      (user-error "No models available — check superchat-llm-backend"))))

(defconst superchat--reasoning-effort-options
  '(("Provider default" . nil)
    ("Disable thinking (fastest)" . none)
    ("Light" . light)
    ("Medium" . medium)
    ("Maximum" . maximum))
  "Completion choices for the interactive reasoning-effort selector.")

(defun superchat--reasoning-effort-label (value)
  "Return the display label for reasoning VALUE."
  (or (car (rassoc value superchat--reasoning-effort-options))
      (format "%s" value)))

(defun superchat--transient-set-reasoning ()
  "Prompt for the global reasoning effort used by future requests.
An active preset with an explicit reasoning value still takes precedence
for turns that use that preset."
  (interactive)
  (let* ((preset (and (boundp 'superchat--active-preset)
                      superchat--active-preset))
         (preset-reasoning
          (and preset
               (superchat-preset-reasoning preset)))
         (preset-locked-p
          (and preset-reasoning
               (not (eq preset-reasoning 'inherit))))
         (current (if preset-locked-p
                      preset-reasoning
                    (if (eq superchat-llm-reasoning t)
                        'medium
                      superchat-llm-reasoning)))
         (current-label (superchat--reasoning-effort-label current))
         (choice (completing-read
                  (format "Reasoning effort (effective: %s): " current-label)
                  (mapcar #'car superchat--reasoning-effort-options)
                  nil t nil nil current-label))
         (reasoning (cdr (assoc choice superchat--reasoning-effort-options))))
    (setq superchat-llm-reasoning reasoning)
    (if preset-locked-p
        (message
         "Global reasoning: %s; active preset %s still uses %s"
         (superchat--reasoning-effort-label reasoning)
         (superchat-preset-name preset)
         (superchat--reasoning-effort-label preset-reasoning))
      (message "Superchat reasoning: %s"
               (superchat--reasoning-effort-label reasoning)))))

(defun superchat--transient-pick-skill ()
  "Prompt for an agentic skill and insert >skill-name."
  (interactive)
  (if-let ((skills (and (fboundp 'superchat-skills-completion-list)
                        (superchat-skills-completion-list))))
      (let ((skill (completing-read "Skill: " skills nil t)))
        (unless (string-empty-p skill)
          (insert ">" skill " ")))
    (user-error "No skills available")))

(defun superchat--transient-pick-command ()
  "Prompt for a slash command and insert /command."
  (interactive)
  (if-let ((commands (superchat--get-all-command-names)))
      (let ((cmd (completing-read "Command: " commands nil t)))
        (unless (string-empty-p cmd)
          (insert cmd " ")))
    (user-error "No commands available")))

;; ── Dired Integration ──

;;;###autoload
(defun superchat-dired-send ()
  "Add marked files in dired to superchat context and open the chat buffer.
When no files are marked, uses the file at point."
  (interactive)
  (unless (derived-mode-p 'dired-mode)
    (user-error "superchat-dired-send must be called from a dired buffer"))
  (when (fboundp 'dired-get-marked-files)
    (let ((files (dired-get-marked-files nil nil nil t)))
      (if files
          (progn
            (dolist (f files)
              (superchat--add-file-to-context f))
            (message "superchat: Added %d file(s) to context" (length files))
            (superchat))
        (message "superchat: No files marked")))))

;;;###autoload
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-c s") #'superchat-dired-send))

;; ── Embark Integration (soft dependency) ──

;;;###autoload
(eval-after-load 'embark
  '(progn
     (declare-function embark-define-action "embark")
     (when (fboundp 'embark-define-action)
       ;; File target: send file content to superchat as context.
       (embark-define-action
        'superchat-embark-file-send
        "s"                                 ; key in embark-act
        "Send to Superchat"
        (lambda (file)
          (superchat--add-file-to-context file)
          (superchat)
          (message "superchat: Added %s to context" file))
        'file)
       ;; Symbol target: send region-or-defun around symbol.
       (embark-define-action
        'superchat-embark-symbol-send
        "S"                                 ; capital S for symbol
        "Send symbol context to Superchat"
        (lambda (_symbol)
          (superchat-send-region-or-defun))
        'symbol))))

;; ── Bub Phase 1 / Migration D: post-turn tape hook ──

(defun superchat--turn-belongs-to-current-view-p (turn)
  "Return non-nil when TURN still belongs to this View's Session.
The nil case preserves the headless/pre-session behavior; once a View has a
Session ID, an older Run must not repopulate its cleared history."
  (or (null superchat--session-id)
      (equal (superchat-turn-session-id turn) superchat--session-id)))

(defun superchat--hook-record-user-message (turn)
  "Post-turn hook: append the user side of TURN to the SQLite tape.
Reads `superchat-turn-clean-input', not `inbound', so model/skill
prefixes and #file refs do not pollute the tape."
  (let ((state (superchat-turn-state turn)))
    (when (and (superchat--turn-belongs-to-current-view-p turn)
               (or (not (plist-member state :record-conversation))
                   (plist-get state :record-conversation)))
      (let ((text (string-trim (or (superchat-turn-clean-input turn) ""))))
        (when (> (length text) 0)
          (superchat--record-message
           "user" text
           (or (plist-get state :intent-storage-id)
               (plist-get state :intent-event-id)))))))
  nil)

(defun superchat--hook-record-attachments (turn)
  "Post-turn hook: append image attachment metadata from TURN to Tape."
  (let ((state (superchat-turn-state turn)))
    (when (and (superchat--turn-belongs-to-current-view-p turn)
               (or (not (plist-member state :record-conversation))
                   (plist-get state :record-conversation)))
      (dolist (file (superchat-turn-context-files turn))
        (when-let* ((info (superchat--image-attachment-info file)))
          (superchat-tape-append-event
           (list :type "attachment.added"
                 :session-id (superchat-turn-session-id turn)
                 :turn-id (superchat-turn-id turn)
                 :actor-id "user"
                 :topic "attachment"
                 :summary (superchat--request-attachment-description file)
                 :sensitivity "confidential"
                 :requires-payload t
                 :payload
                 (list :path file
                       :mime_type (plist-get info :mime-type)
                       :size (plist-get info :size)
                       :turn_id (superchat-turn-id turn))))))))
  nil)

(add-hook 'superchat-post-turn-functions
          #'superchat--hook-record-user-message)
(add-hook 'superchat-post-turn-functions
          #'superchat--hook-record-attachments t)

(provide 'superchat)

;; Command dispatch alist — one entry per slash command.
;; Much simpler than individual hook functions that all check (equal cmd "xxx").
(defvar superchat--command-alist
  '(("recall"        . superchat--cmd-recall)
    ("review"        . superchat-experience--cmd-review)
    ("remember"      . superchat--cmd-remember)
    ("skill-install" . superchat--cmd-skill-install)
    ("commands"      . superchat--cmd-commands)
    ("agent"         . superchat--cmd-agent)
    ("plan"          . superchat--cmd-plan)
    ("code"          . superchat--cmd-code)
    ("research"      . superchat--cmd-research)
    ("cowork"        . superchat--cmd-cowork)
    ("team"          . superchat--cmd-team)
    ("single"        . superchat--cmd-single)
    ("authority"     . superchat--cmd-authority)
    ("workspace"     . superchat--cmd-workspace)
    ("skill"         . superchat--cmd-skill)
    ("compact"       . superchat--cmd-compact)
    ("expand"        . superchat--cmd-expand)
    ("subagent"      . superchat--cmd-subagent)
    ("agents"        . superchat--cmd-agents)
    ("cancel-agent"  . superchat--cmd-cancel-agent)
    ("reset"         . superchat--cmd-reset)
    ("clear-context" . superchat--cmd-clear-context)
    ("clear"         . superchat--cmd-clear)
    ("define"        . superchat--cmd-define)
    ("workflow"      . superchat--cmd-workflow)
    ("extensions"    . superchat--cmd-extensions))
  "Alist of (command-name . handler-function) for slash commands.
Handler: (cmd args input lang target-model) → result-plist or nil.
Plugins register through `superchat-register-command'; treat this
variable itself as internal storage.")

(defun superchat-register-command (name handler)
  "Register slash command NAME to dispatch to HANDLER.

NAME is the command word without the leading slash.  HANDLER is called
as (NAME ARGS INPUT LANG TARGET-MODEL) and returns a result plist such
as \\='(:type :echo :content \"...\") — see `superchat--command-alist'
for the built-in examples.  HANDLER may be an autoloaded symbol, so a
plugin can register at load time and stay unloaded until first use.
Registering NAME again replaces the previous handler.  Returns NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "Command name must be a non-empty string"))
  (unless (or (functionp handler)
              (and (symbolp handler) handler))
    (error "Command handler must be a function or function symbol"))
  (setf (alist-get name superchat--command-alist nil nil #'equal) handler)
  name)

(defun superchat-unregister-command (name)
  "Remove the slash command NAME registered by a plugin.
Returns non-nil when a binding was removed."
  (when (assoc name superchat--command-alist)
    (setf (alist-get name superchat--command-alist nil 'remove #'equal) nil)
    t))

(defun superchat--cmd-recall (_cmd args _input _lang _target-model)
  "Recall relevant context from the tape (and legacy memory table as fallback).
ARGS is the search query."
  (if (and args (> (length args) 0))
      (let* ((trimmed (string-trim args))
             (session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (tape-rows (when (fboundp 'superchat-view-search)
                          (superchat-view-search trimmed session-id 20)))
             (tape-mems (mapcar #'superchat-view-row-to-memory-plist tape-rows))
             (legacy-mems (when (and (fboundp 'superchat-memory-retrieve)
                                     (< (length tape-mems) 20))
                            (superchat-memory-retrieve trimmed)))
             (memories (append tape-mems legacy-mems))
             (count (length memories)))
        (if (> count 0)
            (progn
              (setq superchat--pending-recalled-memories memories)
              `(:type :echo :content
                ,(format "Retrieved %d entries — will be attached to your next message." count)))
          '(:type :echo :content "No matching entries found.")))
    '(:type :echo :content "Usage: /recall <keywords>")))

(defun superchat--cmd-remember (_cmd args _input _lang _target-model)
  "Remember ARGS as an anchor-worthy entry on the tape.
Also updates the Memory projection when its compatibility bridge is enabled."
  (let ((trimmed (string-trim (or args ""))))
    (if (> (length trimmed) 0)
        (let ((title (superchat-memory-compose-title trimmed))
              (session-id (or (when (boundp 'superchat--session-id) superchat--session-id)
                              (format-time-string "%Y%m%d-%H%M%S-")))
              entry-id
              anchor-id)
          ;; Tape: append a user entry marking the explicit remember request.
          (setq entry-id (superchat-tape-append-material
                          session-id "user"
                          (format "[remember] %s" trimmed)))
          ;; Tape: append an anchor distilling the observation.
          (when entry-id
            (setq anchor-id (superchat-tape-append-material
                             session-id "anchor" trimmed
                             :meta `((source_ids . [,entry-id])
                                     (title . ,title))
                             :topic "remember")))
          ;; Compatibility Memory projection.
          (when (and (fboundp 'superchat-memory-capture-explicit)
                     (boundp 'superchat-memory-legacy-writes-enabled)
                     superchat-memory-legacy-writes-enabled)
            (superchat-memory-capture-explicit trimmed title))
          `(:type :echo :content ,(format "Anchor added: %s (tape ID: %s)" title anchor-id)))
      (if-let ((exchange (superchat--last-exchange-struct)))
          (let ((id (superchat-memory-capture-conversation exchange :tier :tier3)))
            `(:type :echo :content ,(format "Last exchange remembered (ID: %s)." id)))
        '(:type :echo :content "No recent exchange found.")))))

(defun superchat--cmd-skill-install (_cmd args _input _lang _target-model)
  (if (and args (> (length args) 0))
      (if (fboundp 'superchat-skills-install)
          (superchat-skills-install args)
        '(:type :echo :content "Skills system not loaded."))
    '(:type :echo :content "Usage: /skill-install user/repo[@branch]")))

(defun superchat--cmd-commands (_cmd _args _input _lang _target-model)
  `(:type :buffer :content ,(superchat--list-commands-as-string)))

(defun superchat--cmd-agent (_cmd args _input _lang _target-model)
  "Activate an agent preset.
With ARGS (a skill name), load and activate that skill as the session preset.
Without ARGS, report the current active preset if it is an agent."
  (let ((skill-name (string-trim (or args ""))))
    (if (string-empty-p skill-name)
        (if (and (boundp 'superchat--active-preset)
                 superchat--active-preset
                 (superchat-preset-agent-p superchat--active-preset))
            `(:type :echo
              :content ,(format "Already in agent mode: %s"
                                (superchat-preset-name superchat--active-preset)))
          '(:type :echo :content "Usage: /agent <skill-name>"))
      (if-let ((preset (superchat-skills-load skill-name)))
          (progn
            (superchat--activate-capability preset)
            (message "🎯 Active preset: %s (%s)"
                     (superchat-preset-name preset)
                     (superchat-preset-type preset))
            `(:type :echo
              :content ,(format "Switched to agent preset: %s" skill-name)))
        `(:type :echo :content ,(format "Skill not found: %s" skill-name))))))

(defun superchat--cmd-plan (_cmd args _input _lang _target-model)
  "Activate a read-only planning preset.
Tries to load a plan-type skill named `planning' or `plan'; falls back
to a built-in read-only preset.  A same-named skill without a plan
frontmatter (defaulting to prompt type) must not shadow the builtin —
planning without emit_plan and read-only tools is incoherent.  With
ARGS, the planning request runs in the new mode as one turn instead of
being swallowed by the switch."
  (let* ((request (string-trim (or args "")))
         (loaded (or (and (superchat-skills-exists-p "planning")
                          (superchat-skills-load "planning"))
                     (and (superchat-skills-exists-p "plan")
                          (superchat-skills-load "plan"))))
         (preset (or (and loaded
                          (superchat-preset-plan-p loaded)
                          loaded)
                     (superchat--builtin-capability-preset "plan"))))
    (superchat--activate-capability preset)
    (message "🎯 Active preset: %s (%s)"
             (superchat-preset-name preset)
             (superchat-preset-type preset))
    (if (string-empty-p request)
        `(:type :echo
          :content ,(format "Switched to planning mode: %s"
                            (superchat-preset-name preset)))
      `(:type :llm-query-and-mode-switch
        :args ,request :command "plan" :lang ,_lang))))

(defun superchat--cmd-builtin-capability (name)
  "Activate the built-in Capability NAME."
  (let ((preset (superchat--builtin-capability-preset name)))
    (unless preset
      (error "Unknown built-in Capability: %s" name))
    (superchat--activate-capability preset name)
    (message "🎯 Active Capability: %s" name)
    `(:type :echo :content ,(format "Capability: %s" name))))

(defun superchat--cmd-code (_cmd _args _input _lang _target-model)
  "Activate the built-in coding Capability."
  (superchat--cmd-builtin-capability "code"))

(defun superchat--cmd-research (_cmd _args _input _lang _target-model)
  "Activate the built-in research Capability."
  (superchat--cmd-builtin-capability "research"))

(defun superchat--cmd-cowork (_cmd _args _input _lang _target-model)
  "Activate the built-in cowork Capability."
  (superchat--cmd-builtin-capability "cowork"))

(defun superchat--cmd-team (_cmd args _input _lang _target-model)
  "Select team Topology for following turns.
When ARGS names a directory, also bind it as the Session Workspace."
  (let* ((arg (string-trim (or args "")))
         (directory (unless (string-empty-p arg)
                      (file-name-as-directory (expand-file-name arg)))))
    (if (and directory (not (file-directory-p directory)))
        '(:type :echo :content "Usage: /team [existing-directory]")
      (when directory (superchat--bind-workspace directory))
      (superchat--set-topology 'team)
      `(:type :echo
        :content ,(format "Topology: team%s."
                          (if directory
                              (format "; Workspace: %s" directory)
                            ""))))))

(defun superchat--cmd-single (_cmd _args _input _lang _target-model)
  "Select single-Agent Topology without changing Capability."
  (superchat--set-topology 'single)
  '(:type :echo :content "Topology: single"))

(defun superchat--cmd-authority (_cmd args _input _lang _target-model)
  "Select or report the Session Authority policy."
  (let ((policy (string-trim (or args ""))))
    (if (string-empty-p policy)
        `(:type :echo
          :content
          ,(format
            "Authority: %s (available: default, observer, developer-project, operator, cowork)"
            superchat--active-authority-policy))
      (if (superchat-authority-policy-known-p policy)
          (progn
            (superchat--set-authority-policy policy)
            `(:type :echo :content ,(format "Authority: %s" policy)))
        `(:type :echo
          :content ,(format "Unknown Authority policy: %s" policy))))))

(defun superchat--cmd-workspace (_cmd args _input _lang _target-model)
  "Bind, show, or clear the current Session Workspace."
  (let ((value (string-trim (or args ""))))
    (cond
     ((string-empty-p value)
      `(:type :echo
        :content
        ,(if superchat--workspace-binding
             (format "Workspace: %s (%s)"
                     (plist-get superchat--workspace-binding :id)
                     (plist-get superchat--workspace-binding :root))
           "Workspace: unbound")))
     ((string= value "clear")
      (superchat--bind-workspace nil)
      '(:type :echo :content "Workspace: unbound"))
     ((file-directory-p (expand-file-name value))
      (let ((binding
             (superchat--bind-workspace (expand-file-name value))))
        `(:type :echo
          :content ,(format "Workspace: %s (%s)"
                            (plist-get binding :id)
                            (plist-get binding :root)))))
     (t
      '(:type :echo
        :content "Usage: /workspace [existing-directory|clear]")))))

(defun superchat--cmd-skill (_cmd args _input _lang _target-model)
  "Switch to a skill/preset.
Without ARGS, list available skills.  With ARGS, load and activate that skill."
  (let ((skill-name (string-trim (or args ""))))
    (if (string-empty-p skill-name)
        `(:type :echo
          :content ,(format "Available skills: %s"
                            (string-join (superchat-skills-get-available) ", ")))
      (if-let ((preset (superchat-skills-load skill-name)))
          (progn
            (superchat--activate-capability preset)
            (message "🎯 Active preset: %s (%s)"
                     (superchat-preset-name preset)
                     (superchat-preset-type preset))
            `(:type :echo
              :content ,(format "Switched to skill: %s (%s)"
                                skill-name
                                (superchat-preset-type preset))))
        `(:type :echo :content ,(format "Skill not found: %s" skill-name))))))

(defun superchat--cmd-reset (_cmd _args _input _lang _target-model)
  (setq superchat--current-command nil)
  (superchat--activate-capability nil "chat")
  (superchat--set-topology 'single)
  (superchat--set-authority-policy "default")
  (superchat--bind-workspace nil)
  '(:type :echo :content "Reset Capability, Topology, Authority, and Workspace."))

(defun superchat--cmd-clear-context (_cmd _args _input _lang _target-model)
  (superchat--clear-session-context)
  '(:type :echo :content "Session context cleared."))

(defun superchat--cmd-clear (_cmd _args _input _lang _target-model)
  (superchat--clear-chat-and-context)
  '(:type :noop))

(defun superchat--cmd-define (_cmd args input _lang _target-model)
  (if-let ((define-pair (superchat--parse-define input)))
      (progn
        (superchat--define-command (car define-pair) (cdr define-pair))
        `(:type :echo :content ,(format "Command `/%s' defined." (car define-pair))))
    `(:type :buffer :content "Invalid /define syntax. Usage: /define <name> \"<prompt>\"")))

;;;###autoload
(defun superchat-ensure-directories ()
  "Ensure that the necessary directories exist."
  (interactive)
  (superchat--ensure-directories)
  (message "Superchat directories ensured to exist."))

;;; superchat.el ends here
