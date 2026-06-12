;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;; Version: 1.0.1
;; Package-Requires: ((emacs "28.1") (llm "0.24"))

;; Author: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/superchat
;; Keywords: ai, chat, llm
;; License: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'llm nil t)
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-memory)
(require 'superchat-tools)
(require 'superchat-executor)
(require 'superchat-skills)
(require 'superchat-parser)
(require 'superchat-models)
(require 'superchat-save)
(require 'superchat-mcp)
(require 'superchat-render)
(require 'superchat-llm)
(require 'superchat-dispatcher)
(require 'superchat-workflow)
(require 'superchat-prompt-hooks)

(defconst superchat-version "0.5"
  "Current Superchat package version.")

(declare-function superchat-memory-compose-title "superchat-memory" (content))
(declare-function superchat-memory-capture-explicit "superchat-memory" (content &optional title))
(declare-function superchat-memory-capture-conversation "superchat-memory" (content &rest options))
(declare-function superchat-memory-auto-capture "superchat-memory" (exchange))
(declare-function superchat-memory-retrieve "superchat-memory" (query-string))
(declare-function superchat-memory-summarize-session-history "superchat-memory" (history-content))

;; Declare llm functions if available.
;; The real llm.el v0.7+ API is:
;;   (make-llm-openai  :key ... :chat-model ...)
;;   (llm-chat         provider prompt &optional multi-output)
;;   (llm-chat-streaming provider prompt partial-cb response-cb error-cb &optional multi-output)
;;   (llm-make-tool    :function ... :name ... :description ... :args ... :async ...)
;;   (llm-make-chat-prompt text :tools ... :context ... :temperature ...)
;;   (llm-name         provider)                    ; generic provider name
;;   (llm-models       provider)                    ; generic model list
(declare-function make-llm-openai "llm-openai" (&rest args))
(declare-function make-llm-claude "llm-claude" (&rest args))
(declare-function make-llm-ollama "llm-ollama" (&rest args))
(declare-function llm-openai-chat-model "llm-openai" (provider))
(declare-function copy-llm-openai "llm-openai" (provider &rest args))
(declare-function llm-chat "llm" (provider prompt &optional multi-output))
(declare-function llm-chat-streaming "llm" (provider prompt partial-cb response-cb error-cb &optional multi-output))
(declare-function llm-chat-async "llm" (provider prompt response-cb error-cb &optional multi-output))
(declare-function llm-chat-streaming-to-point "llm" (provider prompt buffer point finish-cb &optional multi-output))
(declare-function llm-make-tool "llm" (&rest args))
(declare-function llm-make-chat-prompt "llm" (content &rest args))
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
Set to nil to let `superchat-context-max-chars` determine the rolling window.
Set to 0 to disable including prior chat messages."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Messages"))
  :group 'superchat)

(defcustom superchat-context-max-chars 4000
  "Approximate maximum chars of recent conversation to include in prompts.
The newest message is always kept even if it exceeds this limit.
Set to nil to disable character-based trimming."
  :type '(choice (const :tag "Unlimited" nil)
                 (integer :tag "Characters"))
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
                  a `#'-style file reference, or an active slash
                  command (`superchat--current-command' non-nil).
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

(defun superchat--record-message (role content)
  "Record a conversation message with ROLE and CONTENT into history.
Also appends to the SQLite tape for persistent storage."
  (let ((text (string-trim (or content ""))))
    (when (> (length text) 0)
      ;; In-memory history
      (setq superchat--conversation-history
            (cons (list :role role :content text)
                  superchat--conversation-history))
      (when (and (integerp superchat-conversation-history-limit)
                 (> superchat-conversation-history-limit 0)
                 (> (length superchat--conversation-history)
                    superchat-conversation-history-limit))
        (setq superchat--conversation-history
              (cl-subseq superchat--conversation-history 0
                         superchat-conversation-history-limit)))
      ;; SQLite tape: append-only persistent log
      (when (fboundp 'superchat-db-tape-append)
        (ignore-errors
          ;; Lazy-init session id
          (unless superchat--session-id
            (setq superchat--session-id
                  (format "%s%04x"
                          (format-time-string "%Y%m%d-%H%M%S-")
                          (random 65536))))
          (superchat-db-tape-append
           superchat--session-id
           (pcase role
             ("user" "user")
             ("assistant" "assistant")
             (_ "system"))
           text))))))

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
      (insert "【 System Commands 】\n")
      (dolist (cmd builtin-cmds)
        (insert (format "%s  %s\n" (superchat--ljust (car cmd) max-title-len) (cadr cmd))))

      ;; Section: User-defined Commands (only if any exist)
      (when user-cmds
        (insert "\n【 User Commands 】\n")
        (dolist (cmd user-cmds)
          (insert (format "%s  %s\n" (superchat--ljust (car cmd) max-title-len) (cadr cmd)))))

      (insert "\n【 Command Definition 】\n")
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
  (let ((cmds '("/define" "/commands" "/reset" "/clear-context" "/clear" "/remember" "/recall" "/agent" "/skill-install"))) ; Meta commands
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
  "Provide completion for /commands and @models at point for `completion-at-point-functions`."
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
     ;; --- 保留原有的 @ 和 / 分支 ---
     ;; Matches @word at the end of the string.
     ((string-match "\\(@[a-zA-Z0-9_.-]*\\)$" text)
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

(defun superchat--llm-generate-answer (prompt callback stream-callback &optional target-model context-files)
  "Generate an answer for PROMPT using llm.el, handling streaming and tool use.
Optionally use TARGET-MODEL for this request only.
CONTEXT-FILES is an optional list of file paths to include as context.
CALLBACK is called with the final response string (or with
\(tool-call . ...) /\(tool-result . ...) markers for back-compat).
STREAM-CALLBACK is called with each text chunk during streaming.

llm.el handles the multi-round tool loop internally; we hand it
the prompt (with tools embedded via `llm-make-chat-prompt' when
applicable) and three callbacks (partial-cb / response-cb / error-cb)."
  (if (null superchat-llm-backend)
      (when callback
        (funcall callback
                 "[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]"))
    ;; Collect tools and detect mode ONCE per request — both
    ;; `--show-response-mode-indicator' and `--get-adjusted-timeout'
    ;; used to recompute this independently, which meant 3 sync
    ;; MCP polls and 3 tool-set walks before the first byte left
    ;; the keyboard.
    (let* ((tools (superchat--collect-llm-tools
                   (when (stringp prompt) prompt)))
           (response-mode (superchat--detect-response-mode tools t))
           (_ (superchat--show-response-mode-indicator response-mode))
           (adjusted-timeout (superchat--get-adjusted-timeout response-mode))
           (effective-backend (superchat--effective-llm-backend target-model))
           (real-prompt (superchat--build-llm-prompt prompt tools))
           (multi-output (and tools t))
           (response-parts '())
           (completed nil)
           (timeout-timer nil)
           ;; ── TTFT measurement (meausred flag is lexical, closure-safe) ──
           (ttft-measured nil)
           ;; Tracks the cumulative streamed text as reported by
           ;; llm.el so we can compute the actual delta to forward
           ;; downstream.  llm.el's partial-callback semantics:
           ;;   - multi-output=nil  → chunk is the FULL text so far
           ;;   - multi-output=t    → chunk is a plist; :text is a delta
           ;; We normalise both paths to deltas so `response-parts'
           ;; and `stream-callback' always see incremental text.
           (cumulative "")
           ;; ---- local helper: finalize exactly once ----
           (finalize
            (lambda (response)
              (unless completed
                (setq completed t)
                (when timeout-timer (cancel-timer timeout-timer))
                (with-current-buffer (get-buffer-create superchat-buffer-name)
                  (setq superchat--active-timeout-timer nil))
                (when callback
                  (funcall callback response)))))
           ;; Holds elapsed seconds captured on the FIRST stream-cb call
           ;; (any chunk type) until we can annotate the Assistant header
           ;; on the first text chunk.  nil once consumed.
           (stream-cb
            (lambda (chunk)
              (let* ((cumulativep (stringp chunk))
                     (text (cond
                            (cumulativep chunk)
                            ((and (consp chunk)
                                  (stringp (plist-get chunk :text)))
                             (plist-get chunk :text)))))
                (cond
                 ;; text chunk (string=cumulative, or plist=delta)
                 ((stringp text)
                  (unless completed
                    (let ((delta
                           (if cumulativep
                               (cond
                                ((string-prefix-p cumulative text)
                                 (substring text (length cumulative)))
                                ;; defensive: provider reset / out-of-order — emit full
                                (t text))
                             text)))
                      (when cumulativep (setq cumulative text))
                      (unless (string-empty-p delta)
                        ;; ── TTFT: measure on first non-empty text delta ──
                        ;; This is the ONLY reliable signal that the LLM has
                        ;; actually started generating.  Empty flushes, nil
                        ;; callbacks, reasoning blocks, and tool-call plists
                        ;; are all filtered out before we reach this point.
                        (when (and superchat-show-ttft
                                   (not ttft-measured))
                          (setq ttft-measured t)
                          (with-current-buffer (get-buffer-create superchat-buffer-name)
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
           (response-cb
            (lambda (response)
              (funcall finalize
                       (cond
                        ((and (consp response)
                              (stringp (plist-get response :text))
                              (not (string-empty-p (plist-get response :text))))
                         (plist-get response :text))
                        ((and (stringp response)
                              (not (string-empty-p response)))
                         response)
                        (response-parts
                         (string-join (nreverse response-parts) ""))
                        (t "[Empty response from llm]")))))
           (error-cb
            (lambda (err)
              (funcall finalize
                       (cond
                        (response-parts
                         (string-join (nreverse response-parts) ""))
                        (t
                         (format "[API error: %s]"
                                 (or (and (stringp err) err)
                                     (and (consp err) (format "%S" err))
                                     "unknown"))))))))

      ;; Safety-net timeout. Fires only when nothing else has finalised.
      (when adjusted-timeout
        (setq timeout-timer
              (run-with-timer
               adjusted-timeout nil
               (lambda ()
                 (funcall finalize
                          (if response-parts
                              (string-join (nreverse response-parts) "")
                            "[Response timeout. Try: (setq superchat-response-timeout 300)]")))))
        (with-current-buffer (get-buffer-create superchat-buffer-name)
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

      (condition-case err
          (llm-chat-streaming effective-backend
                              real-prompt
                              stream-cb
                              response-cb
                              error-cb
                              multi-output)
        (error
         (funcall finalize
                  (format "[llm-chat-streaming error: %s]"
                          (error-message-string err))))))))

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
    (cl-pushnew file-path superchat--current-context-files :test #'equal)
    (message "superchat: File %s will be included in context" file-path)))

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
  ;; 1. Summarize the session before clearing.
  (superchat--summarize-session-on-exit)

  ;; 2. Clear file context (this is not buffer-local)
  (superchat--clear-session-context)

  ;; 3. Clear UI, history, and command state in the buffer
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (delete-region (point-min) (point-max))
      (insert (propertize "#+TITLE: superchat\n" 'face 'font-lock-title-face))
      (setq superchat--conversation-history nil)
      (setq superchat--current-command nil)
      (superchat--insert-prompt)))

  (message "Chat cleared."))

(defun superchat--summarize-session-on-exit ()
  "Get the entire buffer content and cache it for next startup processing."
  (superchat--ensure-directories)
  (let ((history (buffer-substring-no-properties (point-min) (point-max)))
        (cache-file (expand-file-name "session-cache.org" superchat-data-directory)))
    (when (> (length history) 100)
      (with-temp-file cache-file
        (insert history))
      (message "Superchat session cached for next startup."))))

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

;;;###autoload
(defun superchat ()
  "Open or switch to the superchat buffer."
  (interactive)
  ;; Initialize execution engine with dependency injection
  (when (fboundp 'superchat-executor-initialize)
    (superchat-executor-initialize :llm-executor 'superchat--llm-generate-answer-sync))
  (superchat--ensure-directories)
  (superchat--load-user-commands)
  (superchat--process-cached-session-on-startup)
  (let ((buffer (get-buffer-create superchat-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (superchat-mode 1) ; Ensure the minor mode is turned on
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: superchat\n" 'face 'font-lock-title-face)))
        (superchat--insert-prompt)))
    (let ((window (display-buffer buffer)))
      (select-window window)
      (when superchat-display-single-window
        (delete-other-windows)))))

;; --- UI Commands and Mode Definition ---

(defvar superchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-send-input)
    (define-key map (kbd "/") #'superchat--insert-slash-and-complete)
    (define-key map (kbd "@") #'superchat--insert-at-and-complete)
    (define-key map (kbd "#") #'superchat--smart-hash)
    (define-key map (kbd "C-c C-h") #'superchat--list-commands)
    (define-key map (kbd "C-c C-s") #'superchat--save-conversation)
    map)
  "Keymap for superchat-mode.")

(define-minor-mode superchat-mode
  "A minor mode to provide chat functionalities in an Org buffer."
  :init-value nil
  :lighter " SChat"
  :keymap superchat-mode-map
  (if superchat-mode
      ;; When turning the mode on
      (progn
        (setq-local completion-at-point-functions '(superchat--completion-at-point))
        ;; Disable third-party completion frameworks — superchat provides
        ;; its own command/context completion via `superchat--completion-at-point'
        ;; and company/corfu popups interfere with the chat input flow.
        (when (and (fboundp 'company-mode) (boundp 'company-mode))
          (company-mode -1))
        (when (fboundp 'corfu-mode)
          (corfu-mode -1))
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
    (kill-local-variable 'completion-at-point-functions)
    (remove-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit t)))

(add-hook 'kill-emacs-hook #'superchat--summarize-session-before-emacs-exit)

;; ── Bub Phase 1 / Migration D: post-turn tape hook ──

(defun superchat--hook-record-user-message (turn)
  "Post-turn hook: append the user side of TURN to the SQLite tape.
Reads `superchat-turn-clean-input', not `inbound', so model/skill
prefixes and #file refs do not pollute the tape."
  (let ((text (string-trim (or (superchat-turn-clean-input turn) ""))))
    (when (> (length text) 0)
      (superchat--record-message "user" text)))
  nil)

(add-hook 'superchat-post-turn-functions
          #'superchat--hook-record-user-message)

(provide 'superchat)

;; Command dispatch alist — one entry per slash command.
;; Much simpler than individual hook functions that all check (equal cmd "xxx").
(defvar superchat--command-alist
  '(("recall"        . superchat--cmd-recall)
    ("remember"      . superchat--cmd-remember)
    ("skill-install" . superchat--cmd-skill-install)
    ("commands"      . superchat--cmd-commands)
    ("reset"         . superchat--cmd-reset)
    ("clear-context" . superchat--cmd-clear-context)
    ("clear"         . superchat--cmd-clear)
    ("define"        . superchat--cmd-define))
  "Alist of (command-name . handler-function) for slash commands.
Handler: (args input lang target-model) → result-plist or nil.
Third-party commands register here with add-to-list.")

(defun superchat--cmd-recall (_cmd args _input _lang _target-model)
  (if (and args (> (length args) 0))
      (let* ((memories (cond
                        ((and (fboundp 'superchat-db-memory-search-simple)
                              (> (if (fboundp 'superchat-db-memory-count)
                                     (superchat-db-memory-count "accepted")
                                   0)
                                 0))
                         (superchat-db-memory-search-simple args 20))
                        ((fboundp 'superchat-memory-retrieve)
                         (superchat-memory-retrieve args))))
             (count (length memories)))
        (if (> count 0)
            (progn
              (setq superchat--pending-recalled-memories memories)
              `(:type :echo :content
                ,(format "Retrieved %d memories — will be attached to your next message." count)))
          '(:type :echo :content "No memories found.")))
    '(:type :echo :content "Usage: /recall <keywords>")))

(defun superchat--cmd-remember (_cmd args _input _lang _target-model)
  (let ((trimmed (string-trim (or args ""))))
    (if (> (length trimmed) 0)
        (let ((title (superchat-memory-compose-title trimmed)))
          (superchat-memory-capture-explicit trimmed title)
          `(:type :echo :content ,(format "Memory added: %s" title)))
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

(defun superchat--cmd-reset (_cmd _args _input _lang _target-model)
  (setq superchat--current-command nil)
  '(:type :echo :content "Switched to default chat mode."))

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
