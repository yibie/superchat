;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;; Version: 1.4.0
;; Package-Requires: ((emacs "29.1") (llm "0.31.1"))

;; Author: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/superchat
;; Keywords: ai, chat, llm
;; License: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'widget)
(require 'llm nil t)
;; Kernel and the standard chat distribution load eagerly.
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-invariant)
(require 'superchat-transport-llm)
(require 'superchat-transport-mock)
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-tape)
(require 'superchat-authority)
(require 'superchat-sandbox)
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
(require 'superchat-dispatcher)
(require 'superchat-agent-loop)
(require 'superchat-workspace)
(require 'superchat-editor-workspace)
(require 'superchat-change-set)
(require 'superchat-synthesis)
(require 'superchat-prompt-hooks)
(require 'superchat-default-plugins)

;; The surface rows load their own implementation files (inline ask/rewrite,
;; Org Babel, Magit, emacsclient) and each entry point guards on its row being
;; active.  The package autoload cookies remain for installed packages, but
;; this file no longer eagers or autoloads those symbols itself.
(autoload 'superchat-cowork-telega
  "plugins/superchat-cowork-telega/superchat-cowork-telega" nil t)
;; Experience and Generated UI remain core and autoload their files on first
;; use.
(autoload 'superchat-genui-render-form "superchat-genui")
(autoload 'superchat-genui-action-names "superchat-genui")
(autoload 'superchat-experience-record-evaluation "superchat-experience")
(autoload 'superchat-experience-record-relation "superchat-experience")
(autoload 'superchat-experience-applicability-graph "superchat-experience")
(autoload 'superchat-experience-assemble-recovery-view "superchat-experience")
(autoload 'superchat-experience-assemble-view "superchat-experience")
(autoload 'superchat-experience--run-completed-hook "superchat-experience")
(autoload 'superchat--cmd-compact "superchat-compact")
(autoload 'superchat--cmd-expand "superchat-compact")
(autoload 'superchat--cmd-compact-log "superchat-compact")
;; The overflow-recovery hook is armed the moment `superchat' loads (not only
;; once `superchat-compact' has been used): `run-hook-with-args-until-success'
;; funcalls this autoloaded symbol, so a real context overflow pulls the
;; plugin in on demand -- the plugin stays lazily loaded either way.
(autoload 'superchat-handoff--context-budget-exceeded "superchat-compact")
(autoload 'superchat-plan--nudge-on-completion "superchat-plan")
(autoload 'superchat-agents-panel--refresh-after-turn "superchat-agents-panel")
(autoload 'superchat-experience--projection-apply "superchat-experience")
(autoload 'superchat--team-projection-initial "superchat-subagent")
(autoload 'superchat--team-projection-apply "superchat-subagent")
(autoload 'superchat-workflow--instance-projection-apply "superchat-workflow")
(autoload 'superchat-workspace--projection-apply "superchat-workspace")
(autoload 'superchat--cmd-workflow "superchat-workflow")
(autoload 'superchat-workflow-execute-async "superchat-workflow" nil t)
(autoload 'superchat-workflow--exists-p "superchat-workflow")
(autoload 'superchat-workflow--completion-list "superchat-workflow")
(autoload 'superchat-workflow--parse-invocation "superchat-workflow")
(autoload 'superchat-workflow-edit "superchat-workflow" nil t)
(autoload 'superchat-research-workflow "superchat-workflow" nil t)
(autoload 'superchat-research-workflow-resume "superchat-workflow" nil t)
(autoload 'superchat-research-workflow-status "superchat-workflow" nil t)
(autoload 'superchat--cmd-subagent "superchat-subagent")
(autoload 'superchat--cmd-cancel-agent "superchat-subagent")
(autoload 'superchat--cmd-agents "superchat-agents-panel")
(autoload 'superchat-agents-panel-show "superchat-agents-panel" nil t)
(autoload 'superchat-sessions "superchat-tape-browser" nil t)
(autoload 'superchat-session-history "superchat-tape-browser" nil t)

(defconst superchat-version "1.4.0"
  "Current Superchat package version.")

(declare-function superchat-memory-compose-title "superchat-memory" (content))
(declare-function superchat-plan--capture-view-retirement "superchat-plan" ())
(declare-function superchat-memory-capture-explicit
                  "superchat-memory" (content &optional title &rest options))
(declare-function superchat-memory-verified-remember-anchor-id
                  "superchat-memory" (memory recall-session-id))
(declare-function superchat-memory-capture-conversation "superchat-memory" (content &rest options))
(declare-function superchat-memory-auto-capture "superchat-memory" (exchange))
(declare-function superchat-memory-retrieve "superchat-memory" (query-string))
(declare-function superchat-memory-summarize-session-history "superchat-memory" (history-content))
;; Loaded lazily by the `inline-rewrite' row; declared so keymap references
;; below do not become compile warnings and the row remains the loading owner.
(declare-function superchat-rewrite-region "superchat-rewrite" (beg end instruction))
(declare-function superchat-rewrite-apply-code-block-at-point "superchat-rewrite" ())

(declare-function superchat-view-search "superchat-tape-view" (query &optional session-id limit))
(declare-function superchat-view-row-to-memory-plist "superchat-tape-view" (row))
(declare-function superchat-view-entries "superchat-tape-view" (session-id &optional since-id kinds limit))
(declare-function superchat-view-session-history "superchat-tape-view" (session-id &optional max-id seen))
(declare-function superchat-view-session-ids "superchat-tape-view" ())
(declare-function superchat-view-session-configuration
                  "superchat-tape-view" (session-id))
(declare-function superchat-view-superseded-storage-ids
                  "superchat-tape-view" (session-id))
(declare-function superchat-view--continuity-record
                  "superchat-tape-view" (session-id row))
(declare-function superchat-view--continuity-material-record-p
                  "superchat-tape-view" (record))
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
;; The zero-argument entry points the built-in command surface calls.
(declare-function superchat-mcp-start-server-command "superchat-mcp" ())
(declare-function superchat-mcp-stop-server-command "superchat-mcp" ())
(declare-function superchat-mcp-health-report "superchat-mcp" ())
(declare-function superchat-mcp-registered-tool-names "superchat-mcp" (&optional server-name))

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

(defcustom superchat-buffer-name "*superchat*"
  "The name of the buffer for the chat view.
Owned by the UI layer; kernel files only `defvar' it (S25)."
  :type 'string
  :group 'superchat)

(defcustom superchat-context-message-count nil
  "Maximum number of recent conversation messages to include when building prompts.
Set to nil for no entry-count limit; character budget remains authoritative.
Set to 0 to disable including prior chat messages."
  :type '(choice (const :tag "Unlimited" nil)
                 (natnum :tag "Messages"))
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

(declare-function superchat--llm-drop-dangling-tool-use "superchat-llm" (prompt))
(declare-function superchat--llm-tool-error-feedback "superchat-llm" (result))
(declare-function superchat--llm-tool-error-non-retryable-p "superchat-llm" (result))
(declare-function llm-chat-prompt-append-response "llm" (prompt response &optional role))

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

(defvaralias 'superchat-ollama-timeout-multiplier 'superchat-tool-timeout-multiplier
  "Alias for `superchat-tool-timeout-multiplier` for backward compatibility.")

(defcustom superchat-reasoning-max-chars 20000
  "Maximum characters retained in one folded Thinking block.

When a provider emits a long cumulative reasoning partial, the renderer keeps
the newest tail and adds a truncation notice instead of allowing the View to
grow without bound.  Set to nil or a non-positive value to disable the cap."
  :type '(choice (integer :tag "Maximum characters")
                 (const :tag "Unlimited" nil))
  :group 'superchat)

;; --- Faces ---
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

(defface superchat-status-active-face
  '((t :inherit success :weight bold))
  "Face for an active Runtime status segment."
  :group 'superchat)

(defface superchat-status-warning-face
  '((t :inherit warning :weight bold))
  "Face for a degraded or nearly-full status segment."
  :group 'superchat)

(defface superchat-status-error-face
  '((t :inherit error :weight bold))
  "Face for a failed Runtime status segment."
  :group 'superchat)

(defface superchat-status-muted-face
  '((t :inherit shadow))
  "Face for an inactive or unavailable status segment."
  :group 'superchat)

(defface superchat-reasoning-face
  '((t :inherit shadow :italic t))
  "Face for model reasoning retained in a folded transcript block."
  :group 'superchat)

(defface superchat-superseded-face
  '((t :inherit shadow))
  "Face for a Turn replaced by regenerate or edit-and-resend."
  :group 'superchat)

;; --- Buffer-Local Variables ---
(defvar-local superchat--conversation-history nil)
(defvar-local superchat--response-start-marker nil)
(defvar-local superchat--prompt-start nil)
(defvar-local superchat--active-run-id nil
  "Run ID of the Run most recently submitted from this chat buffer.")
(defvar-local superchat--status-refresh-timer nil
  "Timer that refreshes the active Run's elapsed thinking status.")

(defvar-local superchat--saved-header-line-format nil
  "Header line that was active before `superchat-mode' was enabled.")

(defvar-local superchat--header-line-installed-p nil
  "Non-nil when `superchat-mode' owns this buffer's header line.")

(defvar-local superchat--pause-requested nil
  "Non-nil when the user asked to pause at the next tool boundary.")

(defvar-local superchat--pause-run-id nil
  "Run ID that owns the current pause continuation, if any.")

(defun superchat--runtime-status-projection ()
  "Return the Runtime projection used by both status-band surfaces.

The small compatibility branch keeps old headless tests and integrations that
stub the two public Runtime accessors working while the real Runtime
registry is empty.  A live Run always comes from
`superchat-runtime-status-projection'."
  (let ((projection
         (superchat-runtime-status-projection superchat--active-run-id)))
    (if (and superchat--active-run-id
             (null (superchat--runtime-status-for-current-run projection))
             (superchat-runtime-status superchat--active-run-id))
        (let* ((status (copy-sequence
                        (superchat-runtime-status superchat--active-run-id)))
               (active
                (member superchat--active-run-id
                        (superchat-runtime-active-runs))))
          (setq status (plist-put status :run-id superchat--active-run-id))
          (list :runs (list status)
                :active-runs (if active (list status) nil)
                :active-count (if active 1 0)
                :background-runs nil
                :background-count 0
                :compat-p t))
      projection)))

(defun superchat--runtime-status-for-current-run (projection)
  "Return this View's current Run status from PROJECTION."
  (and superchat--active-run-id
       (cl-find superchat--active-run-id
                (plist-get projection :runs)
                :key (lambda (status) (plist-get status :run-id))
                :test #'equal)))

(defun superchat--runtime-status-active-p (projection status)
  "Return non-nil when STATUS is active in PROJECTION."
  (and status
       (memq status (plist-get projection :active-runs))))

(defun superchat--model-phase-label (phase)
  "Return the user-facing label for model PHASE, or nil when unknown."
  (pcase phase
    ('waiting-first-token "prefill/waiting first token")
    ('reasoning "reasoning")
    ('tool-streaming "tool call streaming")
    ('answer-streaming "answer streaming")
    (_ nil)))

(defun superchat--runtime-phase (status active)
  "Return the display phase for STATUS and ACTIVE."
  (cond
   (superchat--pause-requested "paused")
   ((eq (plist-get status :context-recovery) 'compacting) "compacting")
   (active
    (pcase (plist-get status :status)
      ('accepted "starting")
      ('running-model
       (or (superchat--model-phase-label (plist-get status :model-phase))
           "thinking"))
      ('delivering "finishing")
      (value (format "%s" (or value "active")))))
   ((plist-get status :status)
    (let ((last-phase
           (superchat--model-phase-label (plist-get status :model-phase))))
      (concat (format "%s" (plist-get status :status))
              (if last-phase
                  (format " · last phase: %s" last-phase)
                ""))))
   (t "idle")))

(defun superchat--runtime-elapsed (status)
  "Return STATUS elapsed seconds, or nil when its clock is unavailable."
  (let ((started (plist-get status :started-at)))
    (when (numberp started)
      (max 0 (- (or (plist-get status :completed-at) (float-time))
                started)))))

(defun superchat--format-duration (seconds)
  "Return SECONDS as a compact, human-readable elapsed duration.
Fractional seconds are floored.  Show seconds below a minute, minutes and
seconds below an hour, hours and minutes below a day, and days and hours
thereafter."
  (when (numberp seconds)
    (let* ((seconds (max 0 (floor seconds)))
           (template (cond ((< seconds 60) "%ss")
                           ((< seconds 3600) "%mm%.2ss")
                           ((< seconds 86400) "%hh%.2mm")
                           (t "%dd%.2hh"))))
      (format-seconds template seconds))))

(defun superchat--runtime-elapsed-label (status)
  "Return a compact elapsed label for STATUS."
  (when-let* ((elapsed (superchat--runtime-elapsed status)))
    (superchat--format-duration elapsed)))

(defun superchat--mcp-status-segment ()
  "Return the live MCP health segment, or nil when no source is available."
  (when (fboundp 'superchat-mcp-health-summary)
    (when-let* ((summary (ignore-errors (superchat-mcp-health-summary)))
                (servers (plist-get summary :servers))
                (ok (plist-get summary :ok))
                (degraded (plist-get summary :degraded)))
      (propertize
       (format "MCP %d ok/%d degraded" ok degraded)
       'face (if (> degraded 0)
                 'superchat-status-warning-face
               'superchat-status-active-face)
               'help-echo (format "%d MCP server%s: %d healthy, %d degraded"
                         servers (if (= servers 1) "" "s")
                         ok degraded)))))

(defun superchat--mode-line-safe-string (value)
  "Escape literal percent signs in formatted mode-line VALUE.

This is the single outlet for dynamic header/mode-line strings.  The 8/30
regression let a status percentage pass through a bare `format' result and
`format-mode-line' consumed it; batch2 fixed that once, then a later status
move reintroduced it.  Keep this outlet central so the same lesson does not
become a third recurrence."
  (replace-regexp-in-string
   "%" "%%"
   (if (stringp value) value (format "%s" value))
   t t))

(defun superchat--background-run-segment (projection)
  "Return the active background Run count from PROJECTION."
  (let ((count (or (plist-get projection :background-count) 0)))
    (propertize
     (format "background %d" count)
     'face (if (> count 0)
               'superchat-status-active-face
             'superchat-status-muted-face))))

(defun superchat--mode-line-run-state ()
  "Return the Runtime and queue summary for the mode line."
  (let* ((queued-count (superchat-runtime-pending-follow-ups superchat--session-id))
         (queued (> queued-count 0))
         (queued-label (format "%d queued" queued-count))
         (projection (superchat--runtime-status-projection))
         (status (superchat--runtime-status-for-current-run projection))
         (active (superchat--runtime-status-active-p projection status))
         (mcp (superchat--mcp-status-segment)))
    (if (plist-get projection :compat-p)
        ;; Preserve the compact pre-projection display for legacy callers
        ;; that provide only the old accessor stubs.
        (when (or active queued)
          (if active
              (let ((phase (if superchat--pause-requested
                               "PAUSED"
                             (or (plist-get status :status) 'active))))
                (format " · run %s · %s · %s%s · C-c C-g"
                        phase
                        (or (plist-get status :capability) "chat")
                        (or (plist-get status :model) "backend default")
                        (if queued (concat " · " queued-label) "")))
            (format " · %s · C-c C-g" queued-label)))
      (when (or status active queued
                (> (or (plist-get projection :background-count) 0) 0)
                mcp)
        (concat
         (when status
           (format " · run %s · %s · %s%s"
                   (propertize (superchat--runtime-phase status active)
                               'face (if active
                                         'superchat-status-active-face
                                       'superchat-status-muted-face))
                   (or (plist-get status :capability) "chat")
                   (or (plist-get status :model) "backend default")
                   (if-let* ((elapsed (superchat--runtime-elapsed-label status)))
                       (concat " · " elapsed)
                     "")))
         (when queued (concat " · " queued-label))
         " · " (superchat--background-run-segment projection)
         (when mcp (concat " · " mcp))
         " · C-c C-g")))))

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
                 (face (cond ((> percent 90) 'superchat-status-error-face)
                             ((> percent 70) 'superchat-status-warning-face)
                             (t 'superchat-status-muted-face))))
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
  (let* ((projection (superchat--runtime-status-projection))
         (run-id superchat--active-run-id)
         (status (superchat--runtime-status-for-current-run projection))
         (active (superchat--runtime-status-active-p projection status))
         (model (or (plist-get status :model)
                    superchat-llm-model
                    (and superchat-llm-backend
                         (ignore-errors
                           (superchat--provider-chat-model
                            superchat-llm-backend)))
                    "backend default"))
         (phase (superchat--runtime-phase status active))
         (attempt (and active (plist-get status :attempt)))
         (elapsed (and status (superchat--runtime-elapsed-label status)))
         (context (superchat--header-line-context status)))
    (concat
     (propertize (if active "●" "○")
                 'face (if active
                           'superchat-status-active-face
                         'superchat-status-muted-face))
     " Superchat · " model " · " phase
     (if (numberp attempt) (format " · attempt %d" attempt) "")
     (if elapsed (concat " · " elapsed) "")
     (if context (concat " │ " context) "")
     (if (bound-and-true-p superchat-handoff-auto-enabled)
       " · auto-compact"
       ""))))

(defun superchat--runtime-status-for-owner (projection owner)
  "Return the active status in PROJECTION identified by OWNER."
  (or (and owner
           (cl-find owner (plist-get projection :runs)
                    :key (lambda (status)
                           (plist-get status :transcript-owner))
                    :test #'equal))
      (superchat--runtime-status-for-current-run projection)))

(defun superchat--thinking-status-message (status latest)
  "Return the dynamic waiting message for STATUS and latest reasoning line."
  (concat (pcase (plist-get status :model-phase)
            ('waiting-first-token "Prefill / waiting first token… ")
            ('reasoning "Reasoning… ")
            ('tool-streaming "Receiving tool call… ")
            ('answer-streaming "Answer streaming… ")
            (_ "Thinking… "))
          (or (superchat--runtime-elapsed-label status)
              (superchat--format-duration 0))
          (when (and (stringp latest)
                     (not (string-empty-p (string-trim latest))))
            (concat " · " (string-trim latest)))))

(defun superchat--refresh-thinking-status (&optional owner)
  "Refresh the current Run's waiting status, optionally selected by OWNER."
  (let* ((projection (superchat--runtime-status-projection))
         (status (superchat--runtime-status-for-owner projection owner))
         (active (superchat--runtime-status-active-p projection status)))
    (if (and status active
             (eq (plist-get status :status) 'running-model))
        (superchat--update-status
         (superchat--thinking-status-message
          status
          (and (fboundp 'superchat--response-region-reasoning-latest)
               (superchat--response-region-reasoning-latest
                (plist-get status :transcript-owner))))
         (plist-get status :transcript-owner))
      ;; A renderer-only caller can still receive a reasoning delta before
      ;; Runtime has published a status; keep the latest line visible without
      ;; inventing elapsed or cost data.
      (when (and (stringp owner)
                 (fboundp 'superchat--response-region-reasoning-latest))
        (let ((latest (superchat--response-region-reasoning-latest owner)))
          (when latest
            (superchat--update-status
             (superchat--thinking-status-message nil latest) owner)))))))

(defun superchat--cancel-status-refresh-timer (&optional timer buffer)
  "Cancel TIMER owned by BUFFER, without relying on BUFFER being live.
With no arguments, cancel the current View's timer.  TIMER is explicit in
timer callbacks so a dead-buffer branch can cancel the timer directly instead
of trying to read a buffer-local variable from the wrong current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (owned-timer
          (or timer
              (and (buffer-live-p buffer)
                   (buffer-local-value 'superchat--status-refresh-timer
                                       buffer)))))
    (when (timerp owned-timer)
      (cancel-timer owned-timer))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (or (null timer)
                  (eq owned-timer superchat--status-refresh-timer))
          (setq superchat--status-refresh-timer nil))))))

(defun superchat--status-refresh-tick (buffer &optional timer)
  "Refresh the status band and waiting line for BUFFER.
TIMER is captured by the repeating callback so cleanup still works after
BUFFER has been killed."
  (if (not (buffer-live-p buffer))
      (superchat--cancel-status-refresh-timer timer buffer)
    (with-current-buffer buffer
      (let* ((projection (superchat--runtime-status-projection))
             (status (superchat--runtime-status-for-current-run projection)))
        (when (and status
                   (memq status (plist-get projection :active-runs)))
          (superchat--refresh-thinking-status
           (plist-get status :transcript-owner)))
        (force-mode-line-update t)
        (unless (> (or (plist-get projection :active-count) 0) 0)
          (superchat--cancel-status-refresh-timer timer buffer))))))

(defvar-local superchat--pending-send-request nil
  "Identity-bound pending pause record stashed by the checkpoint.")

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
With nil RUN-ID, clear the current View's pause state unconditionally.
This function owns only pause and resume state.  Transport timer cleanup is
performed by the timer owner or Runtime's exact-Run legacy path."
  (let* ((pending superchat--pending-send-request)
         (pending-record-p
          (and (superchat-llm--pending-pause-p pending)
               (stringp
                (superchat-llm--pending-pause-run-id pending))))
         (pending-run-id
          (and pending-record-p
               (superchat-llm--pending-pause-run-id pending)))
         (target-match
          (and run-id (equal run-id superchat--pause-run-id)))
         (pending-match
          (and run-id (equal run-id pending-run-id))))
    (superchat--cancel-resume-timeout run-id)
    (if (null run-id)
        (setq superchat--pause-requested nil
              superchat--pause-run-id nil
              superchat--pending-send-request nil)
      (when pending-match
        (setq superchat--pending-send-request nil))
      (when target-match
        (if (and pending-record-p (not pending-match))
            ;; Preserve a foreign pending record and restore its own target.
            (setq superchat--pause-requested t
                  superchat--pause-run-id pending-run-id)
          (setq superchat--pause-requested nil
                superchat--pause-run-id nil)
          ;; A legacy/malformed closure has no identity to preserve safely.
          (unless pending-record-p
            (setq superchat--pending-send-request nil)))))))

(defun superchat--pending-pause-valid-p (pending)
  "Return non-nil when PENDING carries one coherent Run identity."
  (when (superchat-llm--pending-pause-p pending)
    (let ((run-id (superchat-llm--pending-pause-run-id pending))
          (owner
           (superchat-llm--pending-pause-transport-owner pending)))
      (and (stringp run-id)
           (superchat-llm--transport-timer-owner-p owner)
           (eq (current-buffer)
               (superchat-llm--transport-timer-owner-buffer owner))
           (equal run-id
                  (superchat-llm--transport-timer-owner-run-id owner))
           (superchat-llm-provider-attempt-reason-p
            (superchat-llm--pending-pause-reason pending))
           (functionp
            (superchat-llm--pending-pause-continuation pending))))))

(defun superchat--arm-resume-timeout (run-id)
  "Arm a safety timer for RUN-ID in the current View.
The timer checks its own slot identity and captured live Run ID before it
cancels anything, so changing the View's active Run cannot retarget it."
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
                 (when (superchat-runtime-effect-capable-p
                        run-id)
                   (ignore-errors
                     (superchat-runtime-cancel run-id
                                               "[Paused Run timed out]"))))))))
    (setq superchat--resume-timeout-timer timer
          superchat--resume-timeout-run-id run-id)))

(defun superchat--pause-boundary (run-id transport-timer-owner)
  "Pause RUN-ID at its tool boundary using TRANSPORT-TIMER-OWNER.
Only the requested Run's owner may cancel its first-answer wait; the owner
helper performs the timer-and-owner compare-clear of the public View slot."
  (let ((pending superchat--pending-send-request))
    (when (and (stringp run-id)
               (equal run-id superchat--pause-run-id)
               (superchat--pending-pause-valid-p pending)
               (equal run-id
                      (superchat-llm--pending-pause-run-id pending))
               (eq transport-timer-owner
                   (superchat-llm--pending-pause-transport-owner pending)))
      (superchat-llm--transport-timer-owner-cancel-wait
       transport-timer-owner)
      (message "Paused at the tool boundary. C-c C-r (or M-x superchat-resume) to continue with a steer message.")
      t)))

(defun superchat-pause ()
  "Request a pause at the next tool boundary of the current Run.
The Run stays active; the model call for the next round never starts
until `superchat-resume'.  Pauses only take effect between model
rounds, never mid-stream."
  (interactive)
  (let ((active-run-id
         (and (boundp 'superchat--active-run-id)
              superchat--active-run-id))
        (pending superchat--pending-send-request))
    (cond
     ((not (and active-run-id
                (fboundp 'superchat-runtime-active-runs)
                (member active-run-id
                        (superchat-runtime-active-runs))))
      (message "No active Run to pause."))
     ((and pending (not (superchat--pending-pause-valid-p pending)))
      (message "暂停恢复状态身份异常；先 resume 或 cancel 它。"))
     ((and pending
           (not (equal
                 active-run-id
                 (superchat-llm--pending-pause-run-id pending))))
      (message "另一 Run 已暂停等待恢复；先 resume 或 cancel 它。"))
     (t
      (setq superchat--pause-requested t
            superchat--pause-run-id active-run-id)
      (message "Pause requested — takes effect at the next tool boundary.")))))

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
    (let* ((steer (string-trim
                   (read-string
                    "Instruction for the model (empty to continue): ")))
           (run-id superchat--pause-run-id)
           (pending superchat--pending-send-request)
           (identity-matches
            (and (superchat--pending-pause-valid-p pending)
                 (equal run-id
                        (superchat-llm--pending-pause-run-id pending))))
           (live-run
            (and identity-matches
                 (superchat-runtime-effect-capable-p run-id)))
           (reason
            (and identity-matches
                 (superchat-llm--pending-pause-reason pending)))
           (continuation
            (and identity-matches
                 (superchat-llm--pending-pause-continuation pending))))
      (cond
       ((not identity-matches)
        (superchat--clear-pause-state)
        (message "恢复状态的 Run 身份不一致；已清理，请重新发送。"))
       ((not live-run)
        (superchat--clear-pause-state run-id)
        (message "Paused Run is no longer active; cancel and resend instead."))
       ((and (not (string-empty-p steer))
             (not (condition-case nil
                      (superchat-runtime-steer run-id steer)
                    (error nil))))
        (superchat--clear-pause-state run-id)
        (message "Paused Run is no longer active; cancel and resend instead."))
       ((not (superchat-runtime-effect-capable-p run-id))
        (superchat--clear-pause-state run-id)
        (message "Paused Run is no longer active; cancel and resend instead."))
       (t
        (setq superchat--pause-requested nil
              superchat--pause-run-id nil)
        (superchat--arm-resume-timeout run-id)
        (condition-case err
            (progn
              (funcall continuation reason)
              (message "Resumed."))
          (error
           (superchat--clear-pause-state run-id)
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
        (setq superchat--active-run-id (plist-get handle :run-id))
        (add-hook 'kill-buffer-hook #'superchat--cancel-status-refresh-timer
                  nil t)
        (unless (timerp superchat--status-refresh-timer)
          (let (timer)
            (setq timer
                  (run-with-timer
                   1 1
                   (lambda ()
                     (superchat--status-refresh-tick buffer timer))))
            (setq superchat--status-refresh-timer timer)))
        (force-mode-line-update t))))
  handle)

(defun superchat--notify-run-terminal (_run status)
  "Message terminal Run STATUS when the user is not in its View.
STATUS is the Runtime public status plist the `turn-completed' Event
carries.  `superchat-buffer-name' is bound to the Run's own buffer, so the selected window's
buffer is compared against it."
  (when (and (listp status)
             (fboundp 'superchat-runtime--terminal-p)
             (superchat-runtime--terminal-p (plist-get status :status))
             (plist-get status :run-id))
    (superchat--input-refresh-terminal-groups status)
    (force-mode-line-update t)
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
                 (plist-get status :session-id))))
      (when (and (boundp 'superchat--status-refresh-timer)
                 (not (> (length (superchat-runtime-active-runs)) 0)))
        (superchat--cancel-status-refresh-timer)))))

(superchat-define-plugin chat-run-notices
  :doc "Echo area notices when a Run ends outside the visible View."
  :apply (lambda (scope _)
           (superchat-on scope 'turn-completed 'chat-notify #'superchat--notify-run-terminal)))

(defvar-local superchat--input-view nil
  "Exact originating View of this nonshipping input panel.")
(defvar-local superchat--input-snapshot nil
  "Detached presentation snapshot; Runtime owns pending input and settlement.")

(defun superchat--input-view-p (view session)
  "Whether VIEW is still SESSION's exact owned Session View."
  (and (buffer-live-p view)
       (eq view (superchat-editor-workspace-buffer session 'session-view))
       (with-current-buffer view (equal session superchat--session-id))))

(defun superchat--input-current (view snapshot)
  "Return the current unclaimed snapshot only for this exact panel tuple."
  (when (superchat--input-view-p view (plist-get snapshot :session-id))
    (let ((pending (superchat-runtime-pending-input (plist-get snapshot :run-id))))
      (when (and pending
                 (equal (plist-get pending :session-id) (plist-get snapshot :session-id))
                 (equal (plist-get pending :run-id) (plist-get snapshot :run-id))
                 (eq (plist-get pending :handle) (plist-get snapshot :handle)))
        pending))))

(defun superchat--input-panel-find (view session run handle)
  "Find the existing native group for the exact tuple, without a registry."
  (cl-find-if
   (lambda (group)
     (let ((snapshot (widget-get group :input-snapshot)))
       (and (eq view (widget-get group :input-view))
            (equal session (plist-get snapshot :session-id))
            (equal run (plist-get snapshot :run-id))
            (eq handle (plist-get snapshot :handle)))))
   (superchat--input-groups view)))

(defun superchat--input-widget-live-p (group)
  "Whether GROUP still has its exact nonempty native presentation range."
  (and group (fboundp 'widget-get)
       (let ((from (widget-get group :from)) (to (widget-get group :to)))
         (and (markerp from) (markerp to) (marker-buffer from)
              (eq (marker-buffer from) (widget-get group :input-view))
              (eq (marker-buffer from) (marker-buffer to)) (< from to)))))

(defun superchat--input-groups (view)
  "Read native widget overlays in VIEW; Runtime remains pending authority."
  (when (and (buffer-live-p view) (fboundp 'widget-get))
    (with-current-buffer view
      (let (groups)
        (dolist (overlay (append (car (overlay-lists)) (cdr (overlay-lists))))
          (let ((widget (overlay-get overlay 'button)))
            (while (and widget (not (widget-get widget :input-snapshot)))
              (setq widget (widget-get widget :parent)))
            (when (superchat--input-widget-live-p widget) (cl-pushnew widget groups :test #'eq))))
        groups))))

(defun superchat--input-refresh-terminal-groups (status)
  "Refresh retained input groups for STATUS's exact current Run and View.
Terminal notification is not discovery of deleted questions or permission to
modify a replacement Session.  Each group retains its own pending handle."
  (let* ((run (plist-get status :run-id))
         (current (superchat-runtime-status run))
         (session (plist-get current :session-id))
         (view (and session (superchat-editor-workspace-buffer session 'session-view))))
    (when (and (equal session (plist-get status :session-id))
               (superchat--input-view-p view session)
               (superchat-runtime-run-in-buffer-p run view))
      (dolist (group (superchat--input-groups view))
        (let ((snapshot (widget-get group :input-snapshot)))
          (when (and (eq view (widget-get group :input-view))
                     (equal session (plist-get snapshot :session-id))
                     (equal run (plist-get snapshot :run-id)))
            (superchat--input-panel-refresh group)))))))

(defun superchat--input-widget-resources ()
  "Capture only native input groups in this View for clear rollback."
  (let (markers overlays)
    (cl-labels ((capture (widget)
                  (dolist (key '(:from :to))
                    (let ((marker (widget-get widget key)))
                      (when (markerp marker)
                        (push (list marker (marker-position marker) (marker-buffer marker)
                                    (marker-insertion-type marker)) markers))))
                  (dolist (key '(:button-overlay :inactive :input-hidden))
                    (let ((overlay (widget-get widget key)))
                      (when (and (overlayp overlay) (eq (overlay-buffer overlay) (current-buffer)))
                        (push (list overlay (overlay-start overlay) (overlay-end overlay)
                                    (overlay-buffer overlay) (copy-sequence (overlay-properties overlay))) overlays))))
                  (mapc #'capture (widget-get widget :children))))
      (mapc #'capture (superchat--input-groups (current-buffer))))
    (cons markers overlays)))

(defun superchat--welcome-widget-resources ()
  "Capture native welcome recent-session push-buttons in this View for clear rollback.
Restricted to `superchat--welcome-open-session' buttons (via their `:notify')
so unrelated widgets — input groups, tool-record buttons — are untouched."
  (let (markers overlays)
    (dolist (overlay (append (car (overlay-lists)) (cdr (overlay-lists))))
      (let ((widget (overlay-get overlay 'button)))
        (when (and widget
                   (eq (widget-get widget :notify) #'superchat--welcome-open-session))
          (dolist (key '(:from :to))
            (let ((marker (widget-get widget key)))
              (when (markerp marker)
                (push (list marker (marker-position marker) (marker-buffer marker)
                            (marker-insertion-type marker)) markers))))
          (push (list overlay (overlay-start overlay) (overlay-end overlay)
                      (overlay-buffer overlay) (copy-sequence (overlay-properties overlay)))
                overlays))))
    (cons markers overlays)))

(defun superchat--input-panel-action (panel action)
  "Apply ACTION only to PANEL's still-current question.
Hide also permits an ended question in its exact live View and Session.
Minibuffer errors and quit propagate.  They neither answer nor stop a Run."
  (when (superchat--input-widget-live-p panel)
    (let ((view (widget-get panel :input-view))
          (snapshot (widget-get panel :input-snapshot)))
      (prog1
          (if (eq action 'hide)
              (when (superchat--input-view-p view (plist-get snapshot :session-id))
                (with-current-buffer view
                  (let ((overlay (or (widget-get panel :input-hidden)
                                     (make-overlay (widget-get panel :from) (widget-get panel :to)))))
                    (add-to-invisibility-spec 'superchat-input-hidden)
                    (overlay-put overlay 'invisible 'superchat-input-hidden)
                    (overlay-put overlay 'evaporate t)
                    (widget-put panel :input-hidden overlay))))
            (if (not (superchat--input-current view snapshot))
              (message "This question is inactive; no action taken.")
            (pcase action
              ('answer
               (let ((answer (read-string "Answer: ")))
                 (cond ((not (superchat--input-current view snapshot))
                        (message "This question changed; answer not sent."))
                       ((string-empty-p (string-trim answer))
                        (message "Answer cannot be empty. Enter a response or stop this work."))
                       (t
                     (superchat-runtime-answer-input
                      (plist-get snapshot :run-id) (plist-get snapshot :handle) (string-trim answer))))))
              ('stop
               ;; No callback-capable UI work between the exact check and
               ;; public cancellation.  A stale Q1 control cannot stop Q2.
               (superchat-runtime-cancel (plist-get snapshot :run-id)
                                         (concat (widget-get panel :input-stop-label) " requested"))))))
        (superchat--input-panel-refresh panel)))))

(defun superchat--input-panel-refresh (panel)
  "Render public question state without changing its clock or lifecycle."
  (when (superchat--input-widget-live-p panel)
  (let* ((view (widget-get panel :input-view))
         (snapshot (widget-get panel :input-snapshot))
         (pending (superchat--input-current view snapshot))
         (status (superchat-runtime-status (plist-get snapshot :run-id)))
         (label (cond ((eq (plist-get status :status) 'timed-out) "Expired")
                      ((memq (plist-get status :status) '(failed audit-incomplete)) "Inactive")
                      ((plist-get status :stop-requested) "Stopped")
                      (pending "Waiting for your answer")
                      (t "Inactive")))
         (inhibit-read-only t))
    (when (superchat--input-view-p view (plist-get snapshot :session-id))
      (with-current-buffer view
        (save-excursion
          (let ((label-widget (nth 1 (widget-get panel :children))))
            (unless (equal label (widget-value label-widget))
              (widget-value-set label-widget label)))
          (dolist (control (cl-subseq (widget-get panel :children) 2 4))
            (widget-apply control (if pending :activate :deactivate)))))))))

(defun superchat--input-show (view session snapshot)
  "Present SNAPSHOT for an exact owned VIEW, without borrowing its draft."
  (unless (and (equal session (plist-get snapshot :session-id))
               (superchat--input-current view snapshot))
    (error "Input View is no longer eligible"))
  (require 'superchat-widgets)
  (let* ((status (superchat-runtime-status (plist-get snapshot :run-id)))
         (owner (plist-get status :transcript-owner))
         (scope (or (plist-get snapshot :presentation-scope) 'response))
         (stop-label (pcase scope ('workflow "Stop workflow")
                                    ('plan "Stop plan execution") (_ "Stop response")))
         (panel (superchat--input-panel-find view session (plist-get snapshot :run-id)
                                             (plist-get snapshot :handle))))
    (if panel
        (when-let* ((hidden (widget-get panel :input-hidden)))
          (delete-overlay hidden) (widget-put panel :input-hidden nil))
      (unless owner (error "Input has no published transcript owner"))
      (with-current-buffer view
        (let ((superchat-buffer-name (buffer-name view)))
          (superchat--response-region-close nil nil owner)
          (unless (superchat--input-current view snapshot) (error "Input View changed during close"))
          (superchat--response-region-insert-transcript
           "" :current-run owner
           (lambda ()
             (setq panel
                   (widget-create
                    'group :format "%v" :input-view view :input-snapshot snapshot
                    :input-stop-label stop-label
                    `(item :format "%v\n" ,(concat "*** Required input\n#+begin_example\n"
                                                   (org-escape-code-in-string (plist-get snapshot :question))
                                                   "\n#+end_example\n"
                                                   stop-label " stops this work; it does not undo completed effects."))
                    '(item :format "%v\n" "Waiting for your answer")
                    `(superchat-inline-button :tag "Answer" :format "%[%t%]  "
                                              :superchat-action ,(lambda () (superchat--input-panel-action panel 'answer)))
                    `(superchat-inline-button :tag ,stop-label :format "%[%t%]  "
                                              :superchat-action ,(lambda () (superchat--input-panel-action panel 'stop)))
                    `(superchat-inline-button :tag "Hide" :format "%[%t%]\n"
                                              :superchat-action ,(lambda () (superchat--input-panel-action panel 'hide))))))))))
    (superchat--input-panel-refresh panel)
    panel))

(defun superchat--input-presenter-config (view session tool-name)
  "Return presentation callbacks for exact owned VIEW and TOOL-NAME.
Trusted producer eligibility is supplied by the Request decorator separately.
There is no pending-question archive: reopening discovers current input only."
  (unless (superchat--input-view-p view session)
    (error "Input presenter requires an exact owned Session View"))
  (list :enabled t :tool-name tool-name
        :presenter (lambda (snapshot) (superchat--input-show view session snapshot))
        :stop-consumer
        (lambda (status)
          (dolist (panel (superchat--input-groups view))
            (when (equal (plist-get status :run-id)
                         (plist-get (widget-get panel :input-snapshot) :run-id))
              (superchat--input-panel-refresh panel))))))

(defun superchat--with-supported-input (request scope &optional current-p)
  "Decorate supported producer REQUEST with an exact owned input adapter.
SCOPE is response, workflow or plan.  CURRENT-P is the automatic producer's
pure execution permission predicate.  Existing explicit input adapters and
restrictive tool profiles are preserved.  Only explicit producer call sites
use this decorator; lower-level submission does not call it automatically."
  (let* ((view (plist-get request :buffer))
         (session (plist-get request :session-id))
         (tools (plist-get request :tools)))
    (if (or (plist-member request :input-core)
            (not (memq scope '(response workflow plan)))
            (not (superchat--input-view-p view session))
            (not (buffer-local-value 'superchat-mode view))
            (plist-member request :prepared-tools)
            (plist-get request :tool-wrapper)
            (plist-get request :parent-run-id)
            (member (plist-get request :topology) '(team "team" subagent "subagent"))
            (eq (plist-get request :attendance) 'unattended)
            ;; Route the ask_user attachment decision through the one
            ;; function that decides tool names (contract §4.2,
            ;; `superchat-select-tools'): bail unless the eventual
            ;; materialized set -- explicit TOOLS honored as-is (rule 1),
            ;; or the surface baseline with an input operation granted
            ;; (rule 3) -- would actually carry `"ask_user"'.
            (not (let ((decided
                        (superchat-select-tools
                         (list :tools tools :input-core (list :operation 'ask-user))
                         nil)))
                   (and (listp decided) (member "ask_user" decided)))))
        request
      (let* ((session (substring-no-properties session))
             (config (superchat--input-presenter-config view session (copy-sequence "ask_user")))
             (presenter (plist-get config :presenter)))
        (setq config (plist-put config :operation 'ask-user))
        (setq config
              (plist-put config :eligible-function
                         (lambda ()
                           (and (superchat--input-view-p view session)
                                (buffer-local-value 'superchat-mode view)
                                (or (null current-p) (funcall current-p))))))
        (setq config
              (plist-put config :presenter
                         (lambda (snapshot)
                           (funcall presenter
                                    (plist-put (copy-sequence snapshot)
                                               :presentation-scope scope)))))
        (plist-put (copy-sequence request) :input-core config)))))

(defun superchat-input-reopen ()
  "Reopen current pending input for this exact owned Session View.
Destroyed panels do not provide discovery of ended questions."
  (interactive)
  (let ((view (or superchat--input-view (current-buffer)))
        found)
    (dolist (run (superchat-runtime-active-runs))
      (when-let* ((pending (superchat-runtime-pending-input run)))
        (when (superchat--input-current view pending)
          (setq found t)
          (superchat--input-show view (plist-get pending :session-id) pending))))
    (unless found (message "No current pending question for this View."))))

(defun superchat--cancel-untracked-run (view active-runs)
  "Find the unique ordinary Run projected into exact VIEW before tracking.
Use public statuses, including opted-in control-active terminal Runs.  Never
choose by recency or Session alone.  Ambiguous ownership leaves all Runs alone."
  (when (and (bound-and-true-p superchat-mode)
             superchat--response-region-current-owner
             (superchat--input-view-p view superchat--session-id))
    (let ((candidates
           (cl-remove-if-not
            (lambda (status)
              (and (superchat-runtime-run-in-buffer-p (plist-get status :run-id) view)
                   (or (member (plist-get status :run-id) active-runs)
                       (plist-get status :control-active-p))
                   (equal superchat--session-id (plist-get status :session-id))
                   ;; S10: a chat Run's public status now carries the
                   ;; contract §2.1 producer surface `:surface 'chat'
                   ;; (`superchat-runtime--surface'), not the pre-S10
                   ;; implicit default `'superchat'; `superchat' is kept for
                   ;; any caller still on the legacy default.
                   (memq (plist-get status :surface) '(chat superchat))
                   (not (eq (plist-get status :topology) 'workflow))
                   (equal superchat--response-region-current-owner
                          (plist-get status :transcript-owner))))
            (plist-get (superchat-runtime-status-projection) :runs))))
      (when (cdr candidates)
        (user-error "Multiple Runs match this View; no Run was stopped"))
      (plist-get (car candidates) :run-id))))

(defun superchat-cancel ()
  "Interrupt the current Run and collect a redirect prompt.
The Run is cancelled first, then the minibuffer edits the existing visible
draft.  Accepting replaces that draft, including clearing it with an empty
answer; unchanged text retains its properties and point.  Nothing is sent.
An edit is discarded if cancellation or minibuffer callbacks replace the
original View, Session or prompt.  Editing errors and quit propagate after
cancellation; they do not undo it or change the draft.
When the latest Run is stale but a live paused Run owns the pending slot,
cancel that owner without retargeting its continuation.  Before a submission
returns its tracking handle, use only a unique ordinary Run matching this
owned View's Session and current opaque transcript owner."
  (interactive)
  (let ((buffer (if (bound-and-true-p superchat-mode)
                    (current-buffer)
                  (get-buffer-create superchat-buffer-name))))
    (with-current-buffer buffer
      (let* ((active-run-id
              (and (boundp 'superchat--active-run-id)
                   superchat--active-run-id))
             (active-runs
              (and (fboundp 'superchat-runtime-active-runs)
                   (superchat-runtime-active-runs)))
             (pending superchat--pending-send-request)
             (pending-owner-run-id
              (and (superchat--pending-pause-valid-p pending)
                   (superchat-llm--pending-pause-run-id pending)))
             (selected-run-id
              (cond ((member active-run-id active-runs) active-run-id)
                    ((member pending-owner-run-id active-runs) nil)
                    (t (superchat--cancel-untracked-run buffer active-runs)))))
        (cond
         (selected-run-id
          (let ((run-id selected-run-id)
                (session superchat--session-id)
                (prompt (superchat--response-region--view-prompt)))
            (unwind-protect
                (superchat-runtime-cancel run-id)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (superchat--clear-pause-state run-id))))
            (cl-labels ((same-draft-p ()
                          (and prompt (buffer-live-p buffer)
                               (with-current-buffer buffer
                                 (and (equal session superchat--session-id)
                                      (eq prompt (superchat--response-region--view-prompt)))))))
              (when (same-draft-p)
                (let ((redirect
                       (read-string
                        "Redirect instruction (edit draft, empty to clear): "
                        (with-current-buffer buffer
                          (buffer-substring-no-properties prompt (point-max))))))
                  (when (same-draft-p)
                    (with-current-buffer buffer
                      (unless (equal redirect (buffer-substring-no-properties prompt (point-max)))
                        (let ((inhibit-read-only t))
                          (delete-region prompt (point-max))
                          (goto-char prompt)
                          (insert redirect)))))))
              (message (if (plist-get (superchat-runtime-status run-id) :stop-requested)
                           "Stop accepted for Run %s"
                         "Cancelled Run %s")
                       run-id))))
         ((and pending-owner-run-id
               (member pending-owner-run-id active-runs))
          (if (superchat-runtime-cancel pending-owner-run-id)
              (message "已取消暂停中的 Run %s。" pending-owner-run-id)
            ;; The owner became terminal after the snapshot.  Its terminal
            ;; path normally compare-clears this slot; an unconditional clear
            ;; is now safe because the record has no live Runtime owner.
            (superchat--clear-pause-state)
            (message "暂停中的 Run %s 已结束；已清理暂停状态。"
                     pending-owner-run-id)))
         (t
          (superchat--clear-pause-state)
          (message "No active Run to cancel.")))))))

;;; ── Superseded turns: regenerate and edit-and-resend (C4) ──

(defun superchat--superseded-turn-rows (session-id)
  "Return the last non-superseded user row of SESSION-ID and its turn rows.
Turn rows are every later model-visible material row (the
`superchat-view--continuity-material-record-p' criterion) plus tool rows.
Session configuration annotations are non-material and therefore kept, so
configuration replay keeps seeing them."
  (let* ((superseded (superchat-view-superseded-storage-ids session-id))
         (rows (superchat-view-entries session-id))
         (start-id nil))
    (dolist (row rows)
      (when (and (equal (nth 2 row) "user")
                 (not (memql (nth 0 row) superseded)))
        (setq start-id (nth 0 row))))
    (when start-id
      (cl-remove-if-not
       (lambda (row)
         (and (>= (nth 0 row) start-id)
              (or (member (nth 2 row) '("tool_call" "tool_result"))
                  (superchat-view--continuity-material-record-p
                   (superchat-view--continuity-record session-id row)))))
       rows))))

(defun superchat--superseded-run-active-p ()
  "Return non-nil when a live Run is hosted by the current View."
  (let ((view (current-buffer)))
    (cl-some (lambda (run-id)
               (superchat-runtime-run-in-buffer-p run-id view))
             (superchat-runtime-active-runs))))

(defun superchat--superseded-prompt ()
  "Return this View's live input prompt marker, or signal."
  (let ((prompt (and (markerp superchat--prompt-start)
                     (eq (marker-buffer superchat--prompt-start)
                         (current-buffer))
                     (marker-position superchat--prompt-start)
                     superchat--prompt-start)))
    (unless prompt
      (user-error "This View has no input prompt"))
    prompt))

(defun superchat--superseded-user-line-p (position)
  "Return non-nil when the line at POSITION is a rendered user row header.
A rebuilt Session View protects its history rows read-only instead of
carrying the live prompt's `superchat-prompt-header' property."
  (and (eq (char-after position) ?*)
       (save-excursion
         (goto-char position)
         (looking-at "\\* User\\b"))
       (or (get-text-property position 'superchat-prompt-header)
           (eq (get-text-property position 'read-only) t))))

(defun superchat--superseded-previous-header (prompt)
  "Return the start of the last user row header line before PROMPT's line.
Return nil when this View shows no earlier user row."
  (let ((position (save-excursion (goto-char prompt) (line-beginning-position)))
        found)
    (while (and (not found) (> position (point-min)))
      (setq position
            (save-excursion (goto-char position) (forward-line -1) (point)))
      (when (superchat--superseded-user-line-p position)
        (setq found position)))
    found))

(defun superchat--superseded-dim (prompt include-user-row)
  "Dim the replaced turn before PROMPT in the current View.
Start after the previous user row's line, or at it when INCLUDE-USER-ROW is
non-nil, and stop before the newline that precedes PROMPT's line, so a later
insertion at the prompt cannot inherit the face."
  (let* ((header (superchat--superseded-previous-header prompt))
         (prompt-line (save-excursion (goto-char prompt)
                                      (line-beginning-position))))
    (when header
      (let ((start (if include-user-row
                       header
                     (save-excursion (goto-char header)
                                     (forward-line 1)
                                     (point))))
            (end (1- prompt-line)))
        (when (< start end)
          (let ((inhibit-read-only t))
            (add-face-text-property start end 'superchat-superseded-face)))))))

(defun superchat--superseded-append (session ids reason)
  "Append one `turn.superseded' event naming IDS for REASON."
  (superchat-tape-append-event
   (list :type "turn.superseded"
         :schema 1
         :session-id session
         :actor-id "chat"
         :payload (list :superseded-storage-ids (vconcat ids)
                        :reason reason)
         :summary (format "Superseded %d row(s) (%s)" (length ids) reason))))

(defun superchat--superseded-rows-for-command ()
  "Return the rows the supersede commands replace, or signal a refusal."
  (when (superchat--superseded-run-active-p)
    (user-error "A Run is active in this View; stop it first"))
  (let* ((rows (superchat--superseded-turn-rows superchat--session-id))
         (text (and rows (nth 3 (car rows)))))
    (unless (and (stringp text)
                 (not (string-empty-p (string-trim text))))
      (user-error "No previous user message to replace"))
    rows))

(defun superchat--superseded-resendable-text (rows)
  "Return ROWS' user text when regenerate may resend it, else signal.
A `/command' or `>skill' target is refused here, before any Tape or View
change; edit-and-resend still returns such text to the prompt and lets the
next ordinary send route it as usual."
  (let* ((text (nth 3 (car rows)))
         (parsed (superchat-core--parse-input
                  (superchat-turn-new text superchat--session-id)))
         (command (superchat-turn-command parsed))
         (skill (superchat-turn-skill parsed)))
    (when (or command skill)
      (user-error "Cannot regenerate %s; edit the message and send it instead"
                  (if command
                      (format "slash command /%s" command)
                    (format "skill >%s" skill))))
    text))

(defun superchat--superseded-resend (input)
  "Submit INPUT as a new ordinary Turn without touching the View prompt.
The ordinary chat request builder composes the request and the View Sink
renders the answer under a fresh assistant heading.  INPUT must already
have passed `superchat--superseded-resendable-text'.  Submits through
`superchat--submit-composed-chat-request' (S10), the single chat submission
path, rather than Runtime's low-level legacy entry point directly."
  (superchat--submit-composed-chat-request (superchat--build-chat-request input)))

;;;###autoload
(defun superchat-regenerate ()
  "Re-answer the last user message without adding a new `* User:' line.
The replaced question and answer leave the model context; the answer is
dimmed in the View.  The prompt and its draft are never touched.  A
`/command' or `>skill' target is refused before any Tape or View change."
  (interactive)
  (superchat-plugin-require-active-row 'chat)
  (let* ((session superchat--session-id)
         (prompt (superchat--superseded-prompt))
         (rows (superchat--superseded-rows-for-command))
         (text (superchat--superseded-resendable-text rows)))
    (superchat--superseded-append session (mapcar #'car rows) "regenerate")
    (superchat--superseded-dim prompt nil)
    (superchat--superseded-resend text)))

;;;###autoload
(defun superchat-edit-last-message ()
  "Put the last user message back into the prompt for editing.
The replaced question and answer leave the model context and are dimmed in
the View; the next ordinary send carries the edited text.  Refuses to
overwrite unsent prompt text."
  (interactive)
  (let* ((session superchat--session-id)
         (prompt (superchat--superseded-prompt)))
    (unless (string-empty-p
             (string-trim (buffer-substring-no-properties
                           prompt (point-max))))
      (user-error "The prompt has unsent text; send or clear it first"))
    (let ((rows (superchat--superseded-rows-for-command)))
      (superchat--superseded-append session (mapcar #'car rows) "edit")
      (superchat--superseded-dim prompt t)
      (goto-char prompt)
      (insert (nth 3 (car rows))))))

(defvar superchat-mode)                ; defined by define-minor-mode below
(defvar-local superchat--assistant-response-start-marker nil)
(defvar-local superchat--assistant-response-end-marker nil)
(defvar-local superchat--response-region-persistent-marker nil)
(defvar-local superchat--current-command nil
  "Current active chat command (e.g. 'create-question', nil for default mode).")
(defvar-local superchat--current-response-parts nil
  "A list to accumulate response chunks in a streaming conversation.")
(defvar-local superchat--ttft-start-time nil
  "Timestamp (float-time) recorded at `superchat-send-input' entry.
Used to compute end-to-end Time-To-First-Token.
Read via `with-current-buffer' in the streaming callback.")
(defvar-local superchat--ttft-elapsed nil
  "TTFT seconds captured for the current response, if available.")
(defvar-local superchat--nonpartial-e2e-elapsed nil
  "End-to-end seconds for a callback-only provider response, if available.")
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
(defvar-local superchat--active-timeout-timer-owner nil
  "Opaque owner of the timeout timer published for this View.")
(defvar-local superchat--timeout-extension-amount 30
  "Number of seconds to extend timeout when user confirms a tool action.")

(defvar-local superchat--session-id nil
  "Unique identifier for the current conversation session.
Used as the session_id column in the SQLite tape table.
Generated on first use per buffer.")

(defvar-local superchat--active-preset nil
  "The active `superchat-preset' for the current session.
Used when the user does not explicitly specify a skill in the input.")

(defvar-local superchat--active-preset-scope nil
  "Exact process-local Preset Scope owned by this chat View, or nil.

The Scope is intentionally not stored in the Preset, Session configuration,
Envelope, public Run status, or Tape.  It is a View-local owner reference;
its registrations are withdrawn only after Runtime has retired execution
through this exact object.")

(defvar-local superchat--preset-switching nil
  "Non-nil while this View is changing or deactivating its Preset Scope.")

(defvar-local superchat--preset-switch-aborted nil
  "Non-nil when this View was stopped while a Preset switch was in flight.")

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

(defun superchat--preset-scope-live-p (scope)
  "Return non-nil when SCOPE remains this process's direct global child."
  (and (superchat-scope-p scope)
       (eq (superchat-scope-parent scope) superchat-scope-global)
       (memq scope (superchat-scope-children superchat-scope-global))
       (not (superchat-execution-scope-retired-p scope))))

(defun superchat--preset-owner-live-p (owner require-mode-p expected-scope)
  "Return non-nil when OWNER can still publish EXPECTED-SCOPE's switch.

The exact buffer object, not a name or Session ID, owns this composition.
REQUIRE-MODE-P records whether activation began in a live chat View; a later
mode-off vetoes publication just like a killed owner does."
  (and (buffer-live-p owner)
       (with-current-buffer owner
         (and (not superchat--preset-switch-aborted)
              (or (not require-mode-p) (bound-and-true-p superchat-mode))
              (eq superchat--active-preset-scope expected-scope)))))

(defun superchat--assert-preset-owner (owner require-mode-p expected-scope)
  "Signal unless OWNER still owns EXPECTED-SCOPE for this activation."
  (unless (superchat--preset-owner-live-p
           owner require-mode-p expected-scope)
    (error "Preset activation owner is no longer live")))

(defun superchat--preset-scope-name (preset)
  "Return PRESET's exact display name for its process-local Scope."
  (let ((name (superchat-preset-name preset)))
    (unless (or (and (stringp name) (not (string-empty-p name)))
                (and (symbolp name) name))
      (error "Preset activation requires a non-empty Preset name: %S" name))
    (format "preset/%s" name)))

(defun superchat--preset-plugin-plan (preset)
  "Preflight PRESET's Plugin definitions and their configuration.

This does not mount anything.  It proves every referenced definition exists
and delegates per-Plugin config validation before candidate Scope creation,
so a bad later item cannot disturb the active Preset Scope."
  (mapcar
   (lambda (entry)
     (let ((name (car entry))
           (config (cdr entry)))
       (superchat-plugin-validate-config
        (superchat-plugin-definition name) config)
       entry))
   (superchat-preset-plugins preset)))

(defun superchat--mount-preset-candidate
    (preset owner owner-mode-p expected-old-scope)
  "Create and mount PRESET's validated composition in a new candidate Scope.

`pending' fibers are legal because their global Service requirements may
appear later.  A `failed' fiber is not a partial success: dispose the entire
candidate and signal before the caller writes configuration or retires the
old Scope."
  (let* ((plan (superchat--preset-plugin-plan preset))
         (scope (superchat-scope-create
                 (superchat--preset-scope-name preset) superchat-scope-global)))
    (condition-case condition
        (progn
          (dolist (entry plan)
            (superchat-plugin-mount (car entry) (cdr entry) scope)
            ;; Plugin apply/notify is public code and may change current
            ;; buffer, turn this View's mode off, or request its teardown.
            ;; The candidate has not become this View's owner yet.
            (superchat--assert-preset-owner
             owner owner-mode-p expected-old-scope))
          (dolist (entry plan)
            (let ((state (superchat-plugin-state (car entry) scope)))
              (unless (memq state '(active pending))
                (error "Preset plugin %s failed to apply (%s)"
                       (car entry) state))))
          (superchat--assert-preset-owner owner owner-mode-p expected-old-scope)
          scope)
      ((error quit)
       (superchat--retire-and-dispose-preset-scope
        scope "Preset candidate activation failed")
       (signal (car condition) (cdr condition))))))

(defun superchat--same-preset-activation-p (preset)
  "Return non-nil when PRESET already owns this View's live Scope."
  (and (equal preset superchat--active-preset)
       (if preset
           (superchat--preset-scope-live-p superchat--active-preset-scope)
         (null superchat--active-preset-scope))))

(defun superchat--record-capability-activation (capability source profile-id)
  "Durably record a requested capability activation before old Scope retirement.

No Session identity means there is no configuration fact to write.  When a
Session exists, a nil receipt is a failure just like a signaled Tape error;
the caller therefore leaves its old composition and Runs intact."
  (when (and (stringp superchat--session-id)
             (not (string-empty-p superchat--session-id)))
    (unless (superchat--configuration-event
             "capability.activated"
             (append (list :name capability :source source)
                     (when profile-id (list :profile-id profile-id))))
      (error "Could not record capability activation"))))

(defun superchat--retire-and-dispose-preset-scope (scope reason)
  "Retire exact PRESET SCOPE through Runtime, then withdraw its registrations."
  ;; Runtime owns cancellation; do not route through a potentially absent
  ;; Driver provider and do not make generic Scope disposal a cancellation
  ;; owner.  The old View pointer stays installed until both calls return, so
  ;; reentrant old callbacks are rejected by its retired Scope instead of
  ;; being silently redirected to a newly active Preset.
  ;; Scope disposal may run arbitrary plugin cleanup, and Runtime retirement
  ;; may deliver terminal Sink/Event callbacks.  Both owner paths use this one
  ;; unwind-protected sequence; callers perform their own exact View liveness
  ;; checks after it returns rather than threading a second lifecycle through
  ;; this helper.
  (unwind-protect
      (progn
        (superchat-runtime-retire-scope scope reason)
        scope)
    (superchat-scope-dispose scope)))

(defun superchat--clear-preset-owner (owner scope)
  "Clear OWNER's active Preset reference when it still names exact SCOPE.

This is deliberately identity based.  A callback may already have installed a
new View-local composition, which this cleanup must not erase."
  (when (buffer-live-p owner)
    (with-current-buffer owner
      (when (eq superchat--active-preset-scope scope)
        (setq-local superchat--active-preset-scope nil)
        (setq-local superchat--active-preset nil)
        (setq-local superchat--active-capability "chat")))))

(defun superchat--deactivate-active-preset (&optional reason)
  "Retire and dispose this View's active Preset Scope, if any.

This is idempotent and identity based.  It never infers a Scope from a
Session or a Preset name, and it does not close a View or another buffer."
  ;; Plugin unload and terminal Sink code are public callbacks and may call
  ;; `set-buffer'.  Preserve the caller's exact current buffer so the rest of
  ;; `superchat-mode' teardown cannot accidentally mutate a sibling View.
  (save-current-buffer
    (let ((owner (current-buffer)))
    ;; Keep this guard View-local rather than dynamically binding the special
    ;; variable: plugin/Tape callbacks may `set-buffer' to another View, whose
    ;; independent switch must neither abort nor inherit OWNER's transition.
    (if (with-current-buffer owner superchat--preset-switching)
        ;; A mode-off/kill hook can run inside candidate apply, Tape append, a
        ;; cancellation Sink, or unload.  Do not silently skip it: the outer
        ;; owner orchestrator sees this exact owner's abort marker, retires any
        ;; exact old/candidate Scope it still owns, and never publishes it.
        (with-current-buffer owner
          (setq-local superchat--preset-switch-aborted t))
      (let ((scope (with-current-buffer owner superchat--active-preset-scope)))
        (with-current-buffer owner
          (setq-local superchat--preset-switching t)
          (setq-local superchat--preset-switch-aborted nil))
        (unwind-protect
            (progn
              (when scope
                (superchat--retire-and-dispose-preset-scope
                 scope (or reason "Preset deactivated")))
              (superchat--clear-preset-owner owner scope))
          (when (buffer-live-p owner)
            (with-current-buffer owner
              (setq-local superchat--preset-switching nil)))))))))

(defun superchat--retire-preset-on-view-kill ()
  "Deactivate this exact chat View's Preset after kill has been approved.

Installed as a buffer-local `kill-buffer-hook', which runs after Emacs has
honored `kill-buffer-query-functions'.  It never guesses a Scope from a
Session or acts on another View."
  (superchat--deactivate-active-preset "Chat View killed"))

(defun superchat--retire-preset-on-major-mode-change ()
  "Deactivate this exact chat View before its local variables are cleared.

`change-major-mode-hook' is the Emacs lifecycle seam for an implicit minor
mode removal caused by changing the buffer's major mode; `superchat-mode' is
not called with -1 in that path."
  (superchat--deactivate-active-preset "Chat View major mode changed"))

(defun superchat--activate-capability (preset &optional name restore-p)
  "Activate PRESET as the current View's Capability and composition.

When PRESET is nil, activate ordinary chat under optional NAME.  NAME may
restate PRESET's explicit capability but cannot override it.  RESTORE-P
rehydrates a saved configuration without appending another
`capability.activated' fact.

Candidate definitions/configuration are fully checked and mounted before the
configuration fact is written.  Only then is the old exact Scope retired via
Runtime and disposed; this preserves its cancellation gates during reentrant
terminal delivery and makes activation failure leave the old Scope usable."
  ;; Candidate apply, Tape append, cancellation delivery, and plugin unload
  ;; can all legally switch Emacs's current buffer.  The initiating View
  ;; remains the owner, and callers must return to it rather than a callback's
  ;; incidental buffer.
  (save-current-buffer
    (let ((owner (current-buffer)))
    (when (with-current-buffer owner superchat--preset-switching)
      (error "Preset activation is already in progress for this View"))
    (let* ((explicit (and preset (superchat-preset-capability preset)))
           (legacy-name
            (and preset (null explicit)
                 (format "%s" (superchat-preset-name preset))))
           (capability (or explicit name legacy-name "chat"))
           (profile-id (and preset (superchat-preset-profile-id preset)))
           (source
            (and preset (superchat-preset-source preset)
                 (symbol-name (superchat-preset-source preset))))
           (owner-mode-p (with-current-buffer owner
                           (bound-and-true-p superchat-mode)))
           (old-scope (with-current-buffer owner superchat--active-preset-scope))
           (same-p (with-current-buffer owner
                     (superchat--same-preset-activation-p preset)))
           candidate committed old-released)
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
      ;; This is intentionally a buffer-local latch, not a dynamic binding:
      ;; public callbacks can move to another View without inheriting OWNER's
      ;; switch.  A callback in OWNER itself records an abort which each
      ;; publication boundary below verifies.
      (with-current-buffer owner
        (setq-local superchat--preset-switching t)
        (setq-local superchat--preset-switch-aborted nil))
      (unwind-protect
          (progn
            (superchat--assert-preset-owner owner owner-mode-p old-scope)
            (unless same-p
              (when preset
                (setq candidate
                      (superchat--mount-preset-candidate
                       preset owner owner-mode-p old-scope)))
              (superchat--assert-preset-owner owner owner-mode-p old-scope))
            ;; A failed write is before the irreversible old-Scope stop.  A
            ;; restore replays existing configuration rather than producing a
            ;; spurious new activation fact.
            (unless restore-p
              (with-current-buffer owner
                (superchat--record-capability-activation
                 capability source profile-id))
              (superchat--assert-preset-owner owner owner-mode-p old-scope))
            (unless same-p
              (when old-scope
                ;; Keep the old View pointer until this exact Runtime-owned
                ;; retirement and plugin withdrawal have returned.  Reentrant
                ;; old callbacks therefore meet the retired old Scope rather
                ;; than being redirected to CANDIDATE.
                (superchat--retire-and-dispose-preset-scope
                 old-scope "Preset replaced")
                (setq old-released t))
              (superchat--assert-preset-owner owner owner-mode-p old-scope)
              (with-current-buffer owner
                (setq-local superchat--active-preset-scope candidate)))
            ;; There is no public callback between this exact pointer publish
            ;; and the remaining local cache writes.  Thus a successful
            ;; candidate cannot be published into a View another callback just
            ;; killed or switched away from.
            (with-current-buffer owner
              (setq-local superchat--active-capability capability)
              (setq-local superchat--active-preset preset))
            (setq committed t)
            preset)
        (unless committed
          ;; Candidate cleanup is an execution owner release too: a plugin
          ;; apply may already have accepted a Run in its candidate Scope.
          (when candidate
            (superchat--retire-and-dispose-preset-scope
             candidate "Preset activation aborted"))
          ;; If owner callbacks killed/mode-disabled this View before commit,
          ;; deactivation was requested while the switch latch was set.  It
          ;; intentionally only marked the abort; complete its exact old Scope
          ;; release here rather than publishing a fresh candidate.  An
          ;; ordinary candidate/configuration failure leaves a live old Scope.
          (when (not (superchat--preset-owner-live-p
                      owner owner-mode-p old-scope))
            (when (and old-scope (not old-released))
              (superchat--retire-and-dispose-preset-scope
               old-scope "Preset View deactivated")
              (setq old-released t))
            ;; The old release may already have completed before a terminal
            ;; Sink/plugin cleanup disabled the mode.  It still needs its
            ;; exact View-local reference cleared; do not make that contingent
            ;; on whether this unwind path had to issue the retirement call.
            (superchat--clear-preset-owner owner old-scope)))
        (when (buffer-live-p owner)
          (with-current-buffer owner
            (setq-local superchat--preset-switching nil))))))))

(defun superchat--set-topology (topology)
  "Select TOPOLOGY for the current Session."
  (unless (memq topology '(single team))
    (error "Unknown Superchat topology: %s" topology))
  (superchat--configuration-event
   "topology.changed" (list :topology (symbol-name topology)))
  (setq-local superchat--active-topology topology))

(defun superchat--set-authority-policy (policy)
  "Select named Authority POLICY for the current Session."
  ;; Named custom policies are Scope registrations.  A direct UI call is not
  ;; already inside a Runtime Run, so resolve this View's active Preset Scope
  ;; explicitly rather than accidentally validating only the global layer.
  (let ((superchat-execution-scope (superchat--chat-execution-scope)))
    (let ((name (symbol-name
                 (superchat-authority-normalize-policy policy))))
      (superchat--configuration-event
       "authority.policy.selected" (list :policy name))
      (setq-local superchat--active-authority-policy name))))

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
         (preset (superchat--resolve-profile-preset
                  profile-id capability source))
         (topology (plist-get configuration :topology))
         (topology (if (stringp topology) (intern topology) topology))
         (authority-policy
          (or (plist-get configuration :authority-policy) "default"))
         (workspace-id (plist-get configuration :workspace-id))
         (workspace-root (plist-get configuration :workspace-root))
         (workspace-live-p
          (and (stringp workspace-id) (stringp workspace-root)
               (file-directory-p workspace-root))))
    ;; Reopen is a real composition activation, not merely a cache restore:
    ;; it creates this View's fresh Preset Scope (or reuses its exact current
    ;; one) without appending another `capability.activated' fact.  Do this
    ;; before mutating the remaining View cache, so an unknown/config/apply
    ;; failure leaves a still-live old Scope and configuration intact.
    (unless (or preset (equal capability "chat"))
      (error "Could not restore Preset %s" profile-id))
    (superchat--activate-capability preset capability t)
    (setq-local superchat--active-topology
                (if (memq topology '(single team)) topology 'single))
    (let ((superchat-execution-scope (superchat--chat-execution-scope)))
      (setq-local superchat--active-authority-policy
                  (if (superchat-authority-policy-known-p authority-policy)
                      (symbol-name
                       (superchat-authority-normalize-policy
                        authority-policy))
                    "default")))
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
    ("mcp-start-server" . superchat-mcp-start-server-command)
    ("mcp-stop-server" . superchat-mcp-stop-server-command)
    ("mcp-health" . superchat-mcp-health-report)
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
           ;; The registry is the whole tool surface: an MCP server's
           ;; tools are put on it by `superchat-mcp-register-server-tools'
           ;; when the server connects, so appending a fresh
           ;; `superchat-mcp-get-tools' here would count every one of
           ;; them twice -- and query the hub again to do it.
           (when (fboundp 'superchat-get-llm-tools)
             (superchat-get-llm-tools)))))
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
  (when superchat-response-timeout
    (let ((effective-mode (or mode (superchat--detect-response-mode)))
          (base-timeout superchat-response-timeout))
      (pcase effective-mode
        ((or 'non-streaming 'tool-calling)
         (floor (* base-timeout superchat-tool-timeout-multiplier)))
        (_
         base-timeout)))))

(defun superchat--show-response-mode-indicator (&optional mode)
  "Display current response mode indicator in the response area.
Shows different messages based on detected MODE.  When MODE is nil,
falls back to `superchat--detect-response-mode'.
`superchat-buffer-name' can be dynamically bound to a foreign target (for
example the rewrite command's source buffer) while the Runtime request it
names runs; the indicator writes only when that target is already a live
chat View, and never creates a buffer to write into."
  (when superchat-show-response-mode
    (let ((mode (or mode (superchat--detect-response-mode)))
          (target (get-buffer superchat-buffer-name)))
      (when (and (buffer-live-p target)
                 (buffer-local-value 'superchat-mode target))
        (superchat--update-status
         (pcase mode
           ('streaming
            "🔄 Streaming Response Mode")
           ('non-streaming
            "⏳ Non-streaming Mode (Ollama + Tools, may be slow)")
           ('tool-calling
            "🔧 Tool Calling Mode"))
         ;; The default indicator runs synchronously while Runtime's LLM
         ;; request configuration is bound.  Keep the registered callback's
         ;; one-argument contract; only Render receives the explicit owner.
         superchat--llm-transcript-owner)))))

(defun superchat--extend-timeout (&optional transport-timer-owner)
  "Extend one first-answer wait without changing its hard wall.
TRANSPORT-TIMER-OWNER identifies the transport call that reported activity.
The unavailable marker means a Runtime Run has already released its owner, so
late activity is ignored.  Nil alone retains the legacy View-local path for
hooks that have no Runtime Run identity."
  (cond
   ((superchat-llm--transport-timer-owner-p transport-timer-owner)
    (let ((buffer
           (superchat-llm--transport-timer-owner-buffer
            transport-timer-owner)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when superchat-response-timeout
            (superchat-llm--transport-timer-owner-extend-wait
             transport-timer-owner
             superchat--timeout-extension-amount))))))
   ((eq transport-timer-owner
        superchat-authority-transport-timer-owner-unavailable)
    nil)
   ((and superchat--active-timeout-timer
         (timerp superchat--active-timeout-timer)
         superchat-response-timeout)
    (let* ((timer superchat--active-timeout-timer)
           (new-trigger-time
            (time-add (timer--time timer)
                      (seconds-to-time
                       superchat--timeout-extension-amount))))
      ;; Direct tool hooks predate transport-owner tokens.  Keep their
      ;; legacy path reordered so an overdue wait cannot starve the hard
      ;; timer behind it.
      (cancel-timer timer)
      (timer-set-time timer new-trigger-time)
      (timer-activate timer)))))

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

(defun superchat--derived-history (&optional session-id)
  "Return this View's model-visible history, derived from the Tape.
M1: the Tape is the authority, so a reader asks the derivation instead of the
in-buffer list.  `superchat--conversation-history' remains a per-View cache of
the same rows -- it is never read as an authority.  SESSION-ID defaults to
this View's Session; nil returns nil rather than another Session's history."
  (let ((sid (or session-id (and (boundp 'superchat--session-id)
                                 superchat--session-id))))
    (when (and (stringp sid) (not (string-empty-p sid)))
      (let ((limit (and (integerp superchat-conversation-history-limit)
                        (> superchat-conversation-history-limit 0)
                        superchat-conversation-history-limit))
            (expanded (and (boundp 'superchat--expanded-compaction-anchor-ids)
                           (vconcat superchat--expanded-compaction-anchor-ids))))
        (superchat-view-derive-messages sid limit expanded)))))

(defun superchat--refresh-history-cache ()
  "Rebuild this View's in-buffer history cache from the Tape derivation.
M1: the cache is regenerated, never edited.  Compaction and Anchor expansion
change what the Session derivation returns, and this makes the cache follow it
instead of keeping a second opinion.  Returns the refreshed cache, or nil when
this View has no Session."
  (let ((derived (superchat--derived-history)))
    (when derived
      (setq superchat--conversation-history (reverse derived)))
    superchat--conversation-history))

(defun superchat--input-meets-memory-threshold-p (input)
  "Return non-nil when INPUT is long enough to trigger auto recall."
  (let* ((trimmed (string-trim (or input "")))
         (width (string-width trimmed)))
    (and (> width 0)
         (>= width superchat-memory-auto-recall-min-length))))

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
  ;; Prompt templates remain independent of command registrations.  A dormant
  ;; Scope entry while the commands provider is unmounted, or a live handler
  ;; that returned nil, must not hide a same-name user template.
  (unless (gethash command superchat--user-commands)
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
  ;; `superchat--builtin-commands' is installation metadata whose values are
  ;; handlers, not prompt templates.  Dispatch membership comes only from the
  ;; live commands Service; this hash remains the independent user-template
  ;; cache and fallback.
  (gethash command superchat--user-commands))

(defun superchat--list-commands-as-string ()
  "Return all available commands as a formatted string with smart alignment.
This separates built-in commands and user-defined prompt files into two sections."
  (let* ((command-descriptions
          ;; Descriptions are presentation metadata only.  Membership comes
          ;; from the live dispatch and builtin registries below.
          '(("commands" . "Show all available commands list")
            ("agent" . "Activate an agent preset")
            ("plan" . "Switch to read-only planning mode")
            ("code" . "Activate the built-in coding Capability")
            ("research" . "Activate the built-in research Capability")
            ("cowork" . "Activate the built-in cowork Capability")
            ("skill" . "Switch to a skill/preset")
            ("compact" . "Compact session history into an anchor")
            ("compact-log" . "Open the latest compaction summary")
            ("expand" . "Expand latest anchor back into history")
            ("team" . "Select team Topology, optionally binding a directory")
            ("single" . "Select single-Agent Topology")
            ("authority" . "Select or show the Session Authority policy")
            ("workspace" . "Bind, show, or clear the Session Workspace")
            ("permissions" . "Show or select the Session permission level")
            ("subagent" . "Delegate a task to an isolated sub-agent")
            ("agents" . "List running sub-agents")
            ("cancel-agent" . "Cancel a running sub-agent by id")
            ("reset" . "Reset to default chat mode")
            ("clear-context" . "Clear all files from current session context")
            ("clear" . "Clear chat history and context")
            ("recall" . "Retrieve historical conversations or information from memory")
            ("remember" . "Save current chat or specific content to memory")
            ("backend" . "Display active llm backend, provider, and model")
            ("models" . "Display list of available language models")
            ("mcp" . "Display MCP (Model Context Protocol) status")
            ("mcp-start" . "Start all active MCP servers")
            ("mcp-start-server" . "Start one MCP server, chosen from a prompt")
            ("mcp-stop-server" . "Stop one running MCP server, chosen from a prompt")
            ("mcp-health" . "Check health of all active MCP servers")
            ("refresh-models" . "Refresh available language models")
            ("skill-install" . "Install a skill from a repository")
            ("workflow" . "Run a .workflow recipe")))
         (live-command-names (superchat--commands-list))
         (builtin-cmds '())
         (user-cmds '())
         (max-title-len 0)
         (command-dir (superchat--command-dir))
         (seen-user-commands (make-hash-table :test #'equal)))

    ;; 1. Collect commands from the live dispatch registries.
    (dolist (command live-command-names)
      (let* ((title (concat "/" command))
             (purpose (or (cdr (assoc command command-descriptions))
                          "Registered slash command")))
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
              (when (and command-name (not (string-empty-p command-name))
                         (not (member command-name live-command-names))
                         (not (gethash command-name seen-user-commands)))
                (let* ((title (concat "/" command-name))
                       (purpose (if (cdr parts)
                                    (mapconcat #'identity (cdr parts) "-")
                                  "")))
                  (puthash command-name t seen-user-commands)
                  (push (list title purpose) user-cmds)
                  (setq max-title-len (max max-title-len (length title))))))))))

    ;; Hash entries can be populated without a prompt file (for example by a
    ;; test or an extension), so include those live user commands as well.
    (maphash
     (lambda (command _prompt)
       (unless (or (member command live-command-names)
                   (gethash command seen-user-commands))
         (puthash command t seen-user-commands)
         (let ((title (concat "/" command)))
           (push (list title "User-defined command") user-cmds)
           (setq max-title-len (max max-title-len (length title))))))
     superchat--user-commands)

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

      (when (member "define" live-command-names)
        (insert "\n-- Command Definition --\n")
        (insert (format "%s  Define a new command, or modify an existing one.\n"
                        (superchat--ljust "/define <name> \"<prompt>\""
                                           max-title-len))))

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
  (let* ((history (reverse (superchat--derived-history)))
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

(defun superchat--commands-list (&optional scope)
  "Return the commands Service's live view from SCOPE, or nil when absent."
  (let ((scope (superchat--chat-execution-scope scope)))
    (when (superchat-service-get scope 'commands)
      (superchat-service-call scope 'commands 'list))))

(defun superchat--get-all-command-names ()
  "Return a list of all available command names, with the leading slash."
  (let ((cmds (mapcar (lambda (cmd) (concat "/" cmd))
                      (superchat--commands-list))))
    (maphash (lambda (k _v) (push (concat "/" k) cmds))
             superchat--user-commands)
    ;; Add available prompt files
    (let ((available-prompts (superchat--list-available-prompts)))
      (dolist (prompt available-prompts)
        (push (concat "/" prompt) cmds)))
    (sort (delete-dups (mapcar #'identity cmds)) 'string<)))

(defun superchat--completion-at-point ()
  "Provide completion for /commands, @models, >skills and #files at point.
After /workflow, complete the recipe name before its task arguments.
Implements `completion-at-point-functions' for corfu/company integration."
  (let* ((end (point))
         (prompt-start (or superchat--prompt-start (point-min)))
         (text (buffer-substring-no-properties prompt-start end)))
    (cond
     ;; Workflow names use the same safe discovery as /workflow execution.
     ((when (and (>= end prompt-start)
                 (string-match "\\`[ \t]*/workflow[ \t]+\\([^\n]*\\)\\'" text))
        (let* ((start (+ prompt-start (match-beginning 1)))
               (prefix (match-string 1 text))
               (names (superchat-workflow--completion-list))
               (quoted (string-prefix-p "\"" prefix))
               (candidates (if quoted (mapcar #'prin1-to-string names) names)))
          (unless names
            (message "No workflows found. Create and save one with M-x superchat-workflow-edit."))
          (when (or (not (string-match-p "[[:space:]]" prefix))
                    (seq-some (lambda (name) (string-prefix-p prefix name)) candidates))
            (list start end candidates :exclusive 'no :company-prefix-length t
                  :exit-function
                  (lambda (name status)
                    (when (and (eq status 'finished) (not quoted))
                      (let ((replacement (superchat-workflow--quote-name name)))
                        (unless (equal name replacement)
                          (delete-region start (+ start (length name)))
                          (goto-char start)
                          (insert replacement))))))))))
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
  superchat-parser--file-ref-regexp
  "Regexp to capture a file path after a leading '#'.
The parser owns this syntax; prompt hooks and on-demand tool detection share
its exact command-head rule instead of maintaining a second regexp.")

;; --- Main Send Logic ---

;; Helpers to inline file contents into the prompt

;; --- Backend introspection ---

(defun superchat--llm-backend-setup-examples ()
  "Return example `superchat-llm-backend' forms as one string.
Shared by `/backend' (`superchat-backend-show') and the onboarding block
(`superchat--insert-onboarding') so their guidance cannot drift apart.  This
is a short form, not a byte-for-byte copy of README's canonical example: the
LM Studio form matches README's `:key \"lm-studio\"' and trailing-slash URL,
but uses the placeholder chat model \"your-model\" instead of naming one
specific model, since the model actually available on a reader's own LM
Studio server varies."
  (concat
   "  (setq superchat-llm-backend\n"
   "        (make-llm-openai-compatible :key \"lm-studio\"\n"
   "                                     :chat-model \"your-model\"\n"
   "                                     :url \"http://localhost:1234/v1/\"))  ;; LM Studio (local server)\n\n"
   "  (setq superchat-llm-backend\n"
   "        (make-llm-openai :key \"sk-...\" :chat-model \"gpt-4o-mini\"))\n"))

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
         ;; MCP tools are registered like any other tool, so they are
         ;; already inside TOOLS.  Counting them from the registry --
         ;; rather than collecting the hub a second time -- is what keeps
         ;; the report from presenting one tool as two.
         (mcp-tools (when (fboundp 'superchat-mcp-registered-tool-names)
                      (superchat-mcp-registered-tool-names)))
         (content
          (concat
           "Backend: llm.el\n"
           (format "Provider: %s\n" provider-name)
           (format "Model: %s\n" model)
           (format "Streaming: %s\n" (if streaming "yes" "no"))
           (format "Tools: %d registered (%d from MCP)\n"
                   (length tools) (length mcp-tools))
           "\n"
           (if (null backend)
               (concat
                "⚠ Backend not configured.\n"
                "Set `superchat-llm-backend' to a `make-llm-*' struct, e.g.:\n\n"
                (superchat--llm-backend-setup-examples))
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
  "Track FILE-PATH on the current buffer's `superchat--current-context-files',
but only when the current buffer is a live `superchat-mode' View, and
always buffer-locally there.  Returns non-nil only when it actually
tracked the file.
Note: We no longer call gptel-context-add-file directly due to gptel issue #572.
Instead, files are passed to gptel-request via the :context parameter.

Anywhere else — a caller outside any View (`superchat-dired-send' and
`superchat-embark-file-send' both `with-current-buffer' the destination
View before calling this, so they are unaffected), a headless
`superchat-prompt-collect'/`superchat-core-prepare-turn' invocation from
a plain buffer (the `file-inline' collector reaches this outside a View
when called that way), or a unit test isolating the variable with its own
`let' — this is a deliberate no-op on the tracking list: writing to
`superchat--current-context-files''s *global default* there would leak
one Session's tracked attachment into every other Session that has not
separately localized the variable, since `superchat--ensure-chat-view'
does not.  The current Turn's own hard `file:' prompt fragment is
unaffected either way — it comes from the Turn's own parsed file
reference, not from this tracking list, so this Turn's prompt still
receives the file's content regardless of where this function runs."
  (message "superchat: Tracking file for context: %s" file-path)
  (when (and file-path (file-exists-p file-path) (bound-and-true-p superchat-mode))
    (make-local-variable 'superchat--current-context-files)
    (cl-pushnew file-path superchat--current-context-files :test #'equal)))

(defun superchat--clear-session-context ()
  "Clear all files added to context during this session."
  (interactive)
  (when superchat--current-context-files
    ;; Clear our tracking list (no need to call gptel-context-remove since we use :context parameter)
    (setq superchat--current-context-files nil)
    (message "superchat: Session context cleared")))

(defun superchat--clear-view-transaction (publish cache-primary)
  "Reset this View and call PUBLISH within one text transaction.
CACHE-PRIMARY retains the existing primary cache behavior after consent."
  (let* ((variables
          '(superchat--session-id superchat--editor-workspace-session-id
            superchat--current-context-files superchat--conversation-history
            superchat--current-command superchat--pending-recalled-memories
            superchat--response-region-states superchat--response-region-current-owner
            superchat--current-response-parts superchat--ttft-elapsed
            superchat--nonpartial-e2e-elapsed superchat--response-start-marker
            superchat--assistant-response-start-marker
            superchat--assistant-response-end-marker
            superchat--response-region-persistent-marker superchat--prompt-start))
         (values (mapcar (lambda (symbol)
                           (cons symbol (copy-tree (symbol-value symbol))))
                         variables))
         (input-resources (superchat--input-widget-resources))
         (welcome-resources (superchat--welcome-widget-resources))
         (input-view (current-buffer))
         ;; `widget-create' (used by the recent-Sessions buttons) resets
         ;; `buffer-undo-list' to nil as a side effect of its own insertion,
         ;; which silently breaks `atomic-change-group''s own text rollback
         ;; for everything recorded before it in this same transaction --
         ;; confirmed directly: after a `widget-create' call inside an
         ;; `atomic-change-group', `cancel-change-group' signals "Undoing to
         ;; some unrelated state" instead of reverting.  Text restoration
         ;; below is therefore explicit and does not rely on that undo path.
         (saved-text (buffer-substring (point-min) (point-max)))
         markers overlays committed)
    ;; Render reset detaches markers and deletes reasoning overlays.  A text
    ;; change group alone cannot restore those resources.
    (cl-labels ((remember (value)
                  (cond
                   ((markerp value)
                    (push (list value (marker-position value)
                                (marker-buffer value)) markers))
                   ((overlayp value)
                    (push (list value (overlay-start value) (overlay-end value)
                                (overlay-buffer value)) overlays))
                   ((consp value)
                    (remember (car value)) (remember (cdr value))))))
      (mapc (lambda (entry) (remember (cdr entry))) values))
    (unwind-protect
        (atomic-change-group
          (when cache-primary (superchat--summarize-session-on-exit))
          (superchat--clear-session-context)
          (superchat--response-region-reset)
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))
            (insert (propertize "#+TITLE: superchat\n" 'face 'org-document-title))
            (setq superchat--conversation-history nil
                  superchat--current-command nil
                  superchat--pending-recalled-memories nil))
          ;; The new Session identity must exist before the fresh-View
          ;; presentation below: onboarding is unconditional on backend
          ;; only, and the recent-Sessions list must not include the
          ;; Session being cleared into.
          (funcall publish)
          (let ((inhibit-read-only t))
            (when (null (superchat-view-entries superchat--session-id nil nil 1))
              (superchat--insert-onboarding)
              (superchat--insert-welcome-recent-sessions))
            (superchat--insert-prompt))
          (setq committed t))
      (unless committed
        (let ((inhibit-quit t))
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))
            (insert saved-text))
          (dolist (entry values) (set (car entry) (cdr entry)))
          (dolist (entry markers)
            (set-marker (nth 0 entry) (nth 1 entry) (nth 2 entry)))
          (dolist (entry overlays)
            (when (nth 3 entry)
              (move-overlay (nth 0 entry) (nth 1 entry)
                            (nth 2 entry) (nth 3 entry))))
          ;; Text undo cannot resurrect evaporated native button overlays.
          ;; Restore exact captured objects only in the original live View;
          ;; cancellation has already happened, so their actions still query
          ;; Runtime and cannot regain a settled question's authority.
          (when (and (buffer-live-p input-view) (eq input-view (current-buffer)))
            (dolist (entry (car input-resources))
              (set-marker (nth 0 entry) (nth 1 entry) (nth 2 entry))
              (set-marker-insertion-type (nth 0 entry) (nth 3 entry)))
            (dolist (entry (cdr input-resources))
              (move-overlay (nth 0 entry) (nth 1 entry) (nth 2 entry) input-view)
              (let ((properties (nth 4 entry)))
                (while properties
                  (overlay-put (car entry) (pop properties) (pop properties)))))
            ;; Welcome recent-session buttons: `atomic-change-group' undoes
            ;; the buffer text a failed presentation inserted, but overlay
            ;; creation is never part of the undo list, so a widget's button
            ;; overlay would otherwise dangle at a stale position even after
            ;; its text is gone.  Reposition every pre-existing button back
            ;; onto the (now-restored) text exactly like the input widgets
            ;; above, then delete every button overlay the failed attempt
            ;; itself created -- including a stale re-creation of what looks
            ;; like the same row, which is a new, uncaptured object.
            (let ((kept (mapcar #'car (cdr welcome-resources))))
              (dolist (entry (car welcome-resources))
                (set-marker (nth 0 entry) (nth 1 entry) (nth 2 entry))
                (set-marker-insertion-type (nth 0 entry) (nth 3 entry)))
              (dolist (entry (cdr welcome-resources))
                (move-overlay (nth 0 entry) (nth 1 entry) (nth 2 entry) input-view)
                (let ((properties (nth 4 entry)))
                  (while properties
                    (overlay-put (car entry) (pop properties) (pop properties)))))
              (dolist (overlay (append (car (overlay-lists)) (cdr (overlay-lists))))
                (let ((widget (overlay-get overlay 'button)))
                  (when (and widget
                             (eq (widget-get widget :notify) #'superchat--welcome-open-session)
                             (not (memq overlay kept)))
                    (delete-overlay overlay)))))))))))

(defun superchat--clear-chat-and-context ()
  "Clear this chat View into a fresh Session, preserving its configuration."
  (interactive)
  (superchat-plugin-require-active-row 'chat)
  (let* ((primary-name superchat-buffer-name)
         (target (if (bound-and-true-p superchat-mode)
                     (current-buffer)
                   (or (get-buffer superchat-buffer-name)
                       (superchat--ensure-chat-view superchat-buffer-name))))
         (target-name (buffer-name target))
         (superchat-buffer-name target-name)
         result)
    (with-current-buffer target
      (unless (and (local-variable-p 'superchat-mode target) superchat-mode)
        (user-error "Clear requires a chat View; this named buffer is foreign"))
      (let ((new-session (superchat--new-session-id))
            (retire-plan (when (fboundp 'superchat-plan--capture-view-retirement)
                           (superchat-plan--capture-view-retirement))))
        (if (superchat-editor-workspace-owner-session target)
            (setq result
                  (superchat-editor-workspace-rollover-session-view
                   target superchat--session-id new-session
                   (lambda (publish)
                     (superchat--clear-view-transaction
                      publish (equal target-name primary-name)))))
          ;; Ordinary unowned chat buffers retain legacy borrowed-View behavior;
          ;; the Runtime borrowed-input validator remains unchanged.
          (superchat--clear-view-transaction
           (lambda () (setq superchat--session-id new-session))
           (equal target-name primary-name))
          (setq result 'cleared))
        ;; Outside the rollback-capable transaction: cancellation and any Plan
        ;; callback failure here cannot undo a successfully published Session.
        (when (and (eq result 'cleared) retire-plan)
          (funcall retire-plan new-session))))
    (when (eq result 'cleared) (message "Chat cleared."))
    result))

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

(defun superchat--clear-follow-ups-on-kill ()
  "Drop this View's queued follow-ups (D7): a closed View cannot receive them."
  (when (stringp superchat--session-id)
    (superchat-runtime-clear-follow-ups superchat--session-id 'view-closed)))

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

(defcustom superchat-welcome-recent-session-count 5
  "Number of recent persisted Sessions shown in a new welcome View."
  :type 'integer
  :group 'superchat)

(defun superchat--welcome-session-summary (rows)
  "Return the first useful one-line summary from session ROWS."
  (let* ((row (or (cl-find-if
                   (lambda (item)
                     (and (member (nth 2 item) '("user" "assistant"))
                          (not (string-empty-p (string-trim
                                                (or (nth 3 item) ""))))))
                  rows)
                 (cl-find-if
                  (lambda (item)
                    (not (string-empty-p (string-trim (or (nth 3 item) "")))))
                  rows)))
         (content (string-trim (or (nth 3 row) "")))
         (first-line (car (split-string content "[\r\n]+" t))))
    (truncate-string-to-width
     (or first-line "")
     72 nil nil "…")))

(defun superchat--welcome-recent-session-rows ()
  "Return recent Session summaries from the Tape View."
  (let ((limit (max 0 superchat-welcome-recent-session-count))
        summaries)
    (when (> limit 0)
      (dolist (session-id (seq-take (superchat-view-session-ids) limit))
        (let ((rows (ignore-errors
                      (superchat-view-session-history session-id))))
          (when rows
            (push (list :session-id session-id
                        :last-activity (nth 5 (car (last rows)))
                        :summary (superchat--welcome-session-summary rows))
                  summaries)))))
    (nreverse summaries)))

(defun superchat--session-row-event-type (row)
  "Return the Runtime event type recorded in Tape ROW, if present."
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (meta (ignore-errors (json-read-from-string (or (nth 4 row) ""))))
         (event (and (fboundp 'superchat-view--object-get)
                     (superchat-view--object-get
                      meta '(event_type event-type)))))
    (or event (nth 1 row))))

(defun superchat--session-row-runtime-audit-p (row)
  "Return non-nil when system Tape ROW is a Runtime/Kernel audit Fact
(run.*, turn.*, prompt.assembled, invariant.*), rendered collapsed."
  (and (equal (nth 2 row) "system")
       (let ((text (format "%s %s"
                          (or (nth 3 row) "")
                          (or (superchat--session-row-event-type row) ""))))
         (string-match-p
          "\\(?:\\`\\|[[:space:]]\\)\\(?:run\\|turn\\|prompt\\|invariant\\)\\."
          text))))

(defun superchat--insert-collapsed-runtime-audit-row (row)
  "Insert one read-only, initially folded Runtime audit ROW."
  (let* ((event (or (superchat--session-row-event-type row) "system"))
         (content (or (nth 3 row) "")))
    (insert "\n")
    (let ((heading (point)))
      (insert (format "** Runtime audit: %s\n%s\n" event content))
    ;; Org owns the fold state and TAB expansion; no second marker or
    ;; overlay protocol is needed for rebuilt Session Views.
      (save-excursion
        (goto-char heading)
        (org-cycle)))))

(defun superchat--welcome-open-session (button &rest _args)
  "Open the Session attached to welcome BUTTON."
  (superchat-open-session (widget-get button :session-id)))

(defun superchat--insert-welcome-recent-sessions ()
  "Insert the recent-session list into an otherwise empty welcome View.
The whole block is read-only (sticky start, nonsticky end, same convention
as `superchat--insert-onboarding'); its push-buttons remain active, since
button activation invokes their `:notify' rather than editing the buffer."
  (let ((rows (superchat--welcome-recent-session-rows))
        (start (point)))
    (when rows
      (insert "\n* Recent sessions\n")
      (insert (propertize "RET on a session id or choose its button to open it.\n"
                          'face 'superchat-status-muted-face))
      (dolist (row rows)
        (insert "\n  ")
        ;; `:keymap widget-keymap' is explicit so RET resolves reliably on
        ;; Emacs 31 in an Org buffer.
        (widget-create
         'push-button
         :tag (plist-get row :session-id)
         :keymap widget-keymap
         :session-id (plist-get row :session-id)
         :help-echo "Open this Superchat session"
         :notify #'superchat--welcome-open-session)
        (insert (format "  %s  %s\n"
                        (or (plist-get row :last-activity) "unknown time")
                        (or (plist-get row :summary) "(no summary)"))))
      (insert "\n")
      (add-text-properties
       start (point)
       '(read-only t front-sticky (read-only) rear-nonsticky t)))))

(defun superchat--insert-onboarding ()
  "Insert a read-only guidance block when no LLM backend is configured.
No-op once `superchat-llm-backend' is set.  Callers insert this once, into
a freshly created View, before `superchat--insert-welcome-recent-sessions'
and before the prompt line."
  (unless superchat-llm-backend
    (let ((start (point)))
      (insert
       "\n* System: superchat-llm-backend is not configured.\n"
       "Set it to a `make-llm-*' struct, e.g.:\n\n"
       (superchat--llm-backend-setup-examples)
       "\n"
       "Or M-x customize-group RET superchat RET.\n")
      (add-text-properties
       start (point)
       '(read-only t front-sticky (read-only) rear-nonsticky t)))))

(defun superchat--ensure-chat-view (buffer-name &optional session-id no-history)
  "Return an initialized chat View named BUFFER-NAME.
When SESSION-ID is non-nil, associate the View with that Session.  NO-HISTORY
skips rematerializing an existing Session's rows into a freshly created
buffer; `superchat--render-session-view' passes it because that caller
rematerializes the same View itself, exactly once."
  (superchat--ensure-directories)
  (superchat--load-user-commands)
  (let* ((named (get-buffer buffer-name))
         (owned-session
          (and named
               (superchat-editor-workspace-owner-session named)))
         (session-id (or session-id owned-session
                         (superchat--new-session-id)))
         (buffer (superchat-editor-workspace-buffer
                  session-id 'session-view t))
        (superchat-buffer-name buffer-name))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        ;; Never adopt a same-name foreign buffer.  The workspace creates the
        ;; exact owned object first; `rename-buffer' disambiguates if needed.
        (unless (equal (buffer-name) buffer-name)
          (rename-buffer buffer-name t))
        (unless (derived-mode-p 'org-mode)
          (org-mode))
        (unless superchat-mode
          (superchat-mode 1))
        (when session-id
          (setq superchat--session-id session-id))
        (unless superchat--session-id
          (setq superchat--session-id (superchat--new-session-id)))
        (let* ((fresh-buffer (= (point-min) (point-max)))
               ;; A freshly created View for a Session that already has Tape
               ;; rows must show that history (S22/C15), not the onboarding
               ;; presentation a rowless Session keeps (S24).  A caller that
               ;; rematerializes the View itself passes NO-HISTORY.
               (history-rows (and fresh-buffer
                                  (not no-history)
                                  session-id
                                  (superchat-view-session-history session-id))))
          (when fresh-buffer
            (cond
             (history-rows
              (superchat--rematerialize-session-view
               buffer session-id history-rows)
              ;; Restart reconciliation is a separate, live-Run-guarded step:
              ;; a killed View does not cancel its Run (S22-fix-1).
              (superchat--reconcile-session-on-reopen session-id))
             (t
              (insert (propertize "#+TITLE: superchat\n"
                                  'face 'org-document-title))
              ;; A brand-new buffer for a Session with no Tape rows yet is the
              ;; real "fresh View" entry point (`superchat', `superchat-new-session'
              ;; and this function's other callers all funnel through here); the
              ;; onboarding and recent-sessions blocks belong before the prompt
              ;; line, so insert them now, while point is still at point-max and
              ;; no prompt marker exists.
              (when (null (superchat-view-entries superchat--session-id nil nil 1))
                (superchat--insert-onboarding)
                (superchat--insert-welcome-recent-sessions))))))
        (unless (and superchat--prompt-start
                     (marker-position superchat--prompt-start))
          (let ((superchat-buffer-name (buffer-name buffer)))
            (superchat--response-region-ensure-view-prompt)))))
    buffer))

(defun superchat--display-chat-view (buffer)
  "Display BUFFER as a chat View and return it."
  (let ((window (display-buffer buffer)))
    (when (window-live-p window)
      (select-window window)
      (when superchat-display-single-window
        (delete-other-windows))))
  buffer)

(defun superchat--display-session-view (buffer session-id)
  "Select SESSION-ID's stable tab, display BUFFER there, and return BUFFER."
  (superchat-editor-workspace--select-session-tab session-id)
  (superchat--display-chat-view buffer))

(defun superchat--render-session-rows (buffer rows)
  "Render Tape ROWS into BUFFER as a rebuilt Session View.
The one row renderer shared by `superchat--render-session-view' and a
freshly created View whose Session already has history.  Tool rows render
as the live S21 headings -- `*** ⏳ NAME' updated to `*** ✓/✗ NAME' with
`Tool call:'/`Tool result:' example blocks -- except that a reopened call
has no recorded duration, so its heading carries `(—)' where the live View
shows `(D.Ds)'.  Reopened headings are already terminal and static, so they
carry no `superchat-tool-call-id' property: that identity belongs to the
live S21 renderer and its provider call id, and this renderer never
substitutes Authority's persisted `execution-id' for it.  Other Runtime
audit rows keep their collapsed line."
  (with-current-buffer buffer
    (let ((inhibit-read-only t)
          (pending (make-hash-table :test #'equal))
          (assistant-open nil))
      (cl-labels
          ((row-meta (row)
             (let ((json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'symbol))
               (ignore-errors
                 (json-read-from-string (or (nth 4 row) "")))))
           (payload-get (meta key)
             (and meta
                  (fboundp 'superchat-view--object-get)
                  (let ((payload (superchat-view--object-get meta '(payload))))
                    (and payload
                         (superchat-view--object-get payload (list key))))))
           (budget-notice (row)
             (when-let* ((budget (payload-get (row-meta row) 'tool-budget))
                         (notice
                          (superchat--tool-budget-notice
                           (list :rounds (superchat-view--object-get budget '(rounds))
                                 :limit (superchat-view--object-get budget '(limit))
                                 :action (superchat-view--object-get budget '(action))))))
               (insert (propertize (concat "* System: " notice) 'face 'italic)
                       "\n")))
           (open-assistant ()
             (unless assistant-open
               (insert (superchat--protected-chat-header "\n** Assistant:\n"))
               (setq assistant-open t)))
           (call-key (meta name)
             (or (payload-get meta 'execution-id)
                 (format "name:%s" name)))
           (block (first-line body)
             (concat "#+begin_example\n"
                     first-line "\n"
                     (when (and (stringp body) (not (string-empty-p body)))
                       (concat (org-escape-code-in-string body) "\n"))
                     "#+end_example\n"))
           (render-call (meta)
             (let ((name (or (payload-get meta 'tool-name) "tool")))
               (open-assistant)
               (let ((start (point)))
                 (insert (propertize (format "*** ⏳ %s" name)
                                     'face 'font-lock-function-name-face)
                         "\n"
                         (block (format "Tool call: %s" name)
                                (payload-get meta 'arguments-json)))
                 (puthash (call-key meta name)
                          (copy-marker start t)
                          pending))))
           (render-result (meta failed-p)
             (let* ((name (or (payload-get meta 'tool-name) "tool"))
                    (key (call-key meta name))
                    (heading (format "*** %s %s (—)"
                                     (if failed-p "✗" "✓") name))
                    (face (if failed-p
                              'font-lock-warning-face
                            'font-lock-function-name-face)))
               (open-assistant)
               (if-let* ((marker (gethash key pending)))
                   (progn
                     (superchat--response-region-update-transcript-line
                      marker (propertize heading 'face face))
                     (remhash key pending))
                 (insert (propertize heading 'face face) "\n"))
               (insert (block (format "Tool result: %s" name)
                              (or (payload-get meta 'result-json)
                                  (payload-get meta 'error-json)))))))
        (dolist (row rows)
          (let ((id (nth 0 row))
                (topic (nth 1 row))
                (kind (nth 2 row))
                (content (or (nth 3 row) "")))
            (pcase kind
              ("user"
               (insert (superchat--protected-chat-header "\n* User: ") content "\n")
               (push (list :role "user" :content content :id id)
                     superchat--conversation-history)
               (setq assistant-open nil))
              ("assistant"
               (open-assistant)
               (insert (superchat-render-markdown-to-org content) "\n")
               (budget-notice row)
               (push (list :role "assistant" :content content :id id)
                     superchat--conversation-history))
              ((or "tool_call" "tool_result")
               (let ((meta (row-meta row))
                     (event (superchat--session-row-event-type row)))
                 (pcase event
                   ("tool.requested" (render-call meta))
                   ("tool.execution.started" nil)
                   ("tool.failed" (render-result meta t))
                   ("tool.completed" (render-result meta nil))
                   (_ (insert "\n** System\n" content "\n")))))
              ("system"
               ;; Cancellation has no response.completed row to carry its
               ;; budget notice; other terminal rows share the assistant row.
               (when (equal (superchat--session-row-event-type row) "run.cancelled")
                 (budget-notice row))
               (if (superchat--session-row-runtime-audit-p row)
                   (superchat--insert-collapsed-runtime-audit-row row)
                 (insert (if (equal topic "attachment")
                             "\n** Attachment\n"
                           "\n** System\n")
                         content "\n")))
              ("anchor"
               (insert "\n** System\n" content "\n")))))
        ;; M1: the transcript above is a rendering of the Tape, and the list it
        ;; filled on the way is a cache of the same rows.  Settling it from the
        ;; derivation keeps the cache a regenerated view rather than a second
        ;; authority, so a compaction or an expansion is reflected here instead
        ;; of depending on which rows this walk happened to visit.
        (when (fboundp 'superchat--refresh-history-cache)
          (superchat--refresh-history-cache))))))

(defun superchat--session-active-run-p (session-id)
  "Return non-nil when SESSION-ID still has a live, process-local Run."
  (and (stringp session-id)
       (cl-some (lambda (status)
                  (equal session-id (plist-get status :session-id)))
                (plist-get (superchat-runtime-status-projection) :active-runs))))

(defun superchat--reconcile-session-on-reopen (session-id)
  "Run restart reconciliation for SESSION-ID only when no Run is live.
`superchat-runtime-reconcile-session' is restart reconciliation: it
deliberately ignores the process-local Run registry and seals unfinished
Runs `abandoned'.  Killing a View does not cancel its Run, so a Session
observed in the same Emacs while that Run is still current must be left
untouched -- otherwise the false seal could later be contradicted by the
Run's real terminal fact (S22-fix-1)."
  (when (and (stringp session-id)
             (fboundp 'superchat-runtime-reconcile-session)
             (not (superchat--session-active-run-p session-id)))
    (ignore-errors (superchat-runtime-reconcile-session session-id))))

(defun superchat--rematerialize-session-view (buffer session-id rows)
  "Replace BUFFER's contents with SESSION-ID's Tape ROWS.
The one rebuild operation behind `superchat--ensure-chat-view' and
`superchat--render-session-view': restore the Session's runtime
configuration, reset the response region, erase, render the title and rows,
make the history read-only, insert the prompt, and restore a plan review
that this erase removed.  Restart reconciliation is deliberately not part
of it: callers run `superchat--reconcile-session-on-reopen' as a separate,
live-Run-guarded step, exactly once."
  (with-current-buffer buffer
    (let ((superchat-buffer-name (buffer-name buffer)))
      (superchat--restore-session-runtime-configuration session-id)
      (superchat--response-region-reset)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq superchat--conversation-history nil
              superchat--current-command nil)
        (setq-local superchat--current-context-files nil)
        (insert (propertize
                 (format "#+TITLE: superchat — %s\n" session-id)
                 'face 'org-document-title))
        (superchat--render-session-rows buffer rows)
        (add-text-properties (point-min) (point-max) '(read-only t))
        (superchat--insert-prompt))
      ;; A plan under review survives on the Tape while this erase removed
      ;; its inline review block; restore the review.
      (when (require 'superchat-plan nil t)
        (superchat-plan--restore-on-reopen session-id))))
  buffer)

(defun superchat--render-session-view (session-id)
  "Reconstruct SESSION-ID from Tape and return its View buffer."
  (let* ((buffer-name (superchat--session-buffer-name session-id))
         ;; NO-HISTORY: this caller rematerializes the View itself, exactly
         ;; once, right below.
         (buffer (superchat--ensure-chat-view buffer-name session-id t))
         (rows (superchat-view-session-history session-id)))
    (superchat--rematerialize-session-view buffer session-id rows)
    (superchat--reconcile-session-on-reopen session-id)
    buffer))

(defun superchat-new-session ()
  "Create, display, and return a new independent Session View.
Invoked explicitly from a non-chat buffer, the exact buffer it was called
from becomes this View's associated editing source; a View created from
inside a chat surface, from Tape, or by branching starts with none."
  (interactive)
  (superchat-plugin-require-active-row 'chat)
  (let* ((source (current-buffer))
         (session-id (superchat--new-session-id))
         (buffer (superchat--ensure-chat-view
                  (superchat--session-buffer-name session-id)
                  session-id)))
    (with-current-buffer buffer
      (setq-local superchat--current-context-files nil))
    (superchat--origin-associate buffer source)
    (superchat--display-session-view buffer session-id)))

(defun superchat-open-session (session-id)
  "Open SESSION-ID as a reconstructed, independent chat View."
  (interactive
   (let ((session-ids (superchat-view-session-ids)))
     (unless session-ids
       (user-error "No persisted Superchat sessions"))
     (list (completing-read "Open Superchat session: "
                            session-ids nil t))))
  (superchat-plugin-require-active-row 'chat)
  (let* ((source (current-buffer))
         (buffer-name (superchat--session-buffer-name session-id))
         (buffer (or (superchat-editor-workspace-buffer
                      session-id 'session-view)
                     (superchat--render-session-view session-id))))
    ;; A View rebuilt from Tape has no association of its own; an explicit
    ;; call from a non-chat buffer is what gives it one.  Branching reaches
    ;; here from the parent's own View, so a branch inherits nothing.
    (superchat--origin-associate buffer source)
    (superchat--display-session-view buffer session-id)))

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
  (superchat-plugin-require-active-row 'chat)
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
  (superchat-plugin-require-active-row 'chat)
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
  (let* ((source (current-buffer))
         (primary (get-buffer superchat-buffer-name))
         (primary-session
          (and primary (superchat-editor-workspace-owner-session primary)))
         (current-tab (superchat-editor-workspace--current-tab))
         (tab-session
          (and current-tab (superchat-editor-workspace--tab-session-id current-tab)))
         (current-session
          ;; A rolled-over tab can retain old protocol hosts but no old View.
          ;; Keep routing to a different tab's existing View when it has one.
          (if (and primary-session
                   (or (null tab-session)
                       (null (superchat-editor-workspace-buffer
                              tab-session 'session-view))))
              primary-session
            tab-session))
         (buffer (superchat--ensure-chat-view
                  superchat-buffer-name current-session))
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
        ;; A fresh View's title, onboarding and recent-sessions blocks are
        ;; inserted by `superchat--ensure-chat-view' above, the shared entry
        ;; point for this command and `superchat-new-session'.
        (superchat--response-region-ensure-view-prompt)))
    ;; Called explicitly from a non-chat buffer, this View speaks for that
    ;; exact buffer from now on.  Called from a chat surface it changes
    ;; nothing -- in particular it never rebinds a View to itself.
    (superchat--origin-associate buffer source)
    (superchat--display-session-view
     buffer (buffer-local-value 'superchat--session-id buffer))))

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
  (let ((tab (superchat-editor-workspace--current-tab)))
    (superchat--ensure-chat-view
     superchat-buffer-name
     (and tab (superchat-editor-workspace--tab-session-id tab)))))

;; ── The View's associated editing source ──
;;
;; The chat window and `superchat-act' must hand the model the same facts
;; about the buffer the user came from.  Two different things are kept here,
;; deliberately not merged: the View's own ASSOCIATION (which exact buffer
;; later requests from this View speak about -- set only by an explicit entry
;; point, never guessed from focus, name or recency), and a REQUEST SNAPSHOT
;; (data captured from that buffer once per prepared request).  The
;; association is a live object on the View buffer; the snapshot is pure data.

(defvar-local superchat--origin-buffer nil
  "The exact editing source associated with this chat View, or nil.
Set by `superchat' / `superchat-new-session' / `superchat-open-session' when
one of them is invoked explicitly from a non-chat buffer, and by nothing
else: ordinary focus changes do not rebind it, no other-buffer, name or
recent-use heuristic may guess it, and a chat View never registers itself.
Death of the buffer invalidates the association; a new buffer reusing the
name never takes its place.")

(defvar-local superchat--origin-known nil
  "The last facts captured from this View's associated source, or nil.
Data only -- never the buffer object -- so it stays reportable after the
source dies.")

(defvar-local superchat--origin-buffer-name nil
  "The source's name as recorded when this View's association was made.
A killed buffer reports nil for `buffer-name', so this is the floor a request
falls back to when no full snapshot was ever recorded: the association's own
name, reported as facts plus `:dead' rather than as silence.")

(defun superchat--chat-view-p (&optional buffer)
  "Whether BUFFER (default the current buffer) is one of Superchat's own Views.
Both major modes count: the chat View and the request inspector are
Superchat-owned surfaces, never an editing source for themselves."
  (with-current-buffer (or buffer (current-buffer))
    (or (bound-and-true-p superchat-mode)
        (derived-mode-p 'superchat-request-mode))))

(defun superchat--origin-remember (view snapshot)
  "Record SNAPSHOT as VIEW's last known origin facts and return SNAPSHOT.
Only data is kept, and only while it describes a live source: a `:dead'
snapshot says nothing new about the source, so it never overwrites facts
captured from it earlier."
  (when (and (buffer-live-p view) snapshot (not (plist-get snapshot :dead)))
    (with-current-buffer view
      (setq superchat--origin-known snapshot)
      (setq superchat--origin-buffer-name (plist-get snapshot :name))))
  snapshot)

(defun superchat--origin-associate (buffer source)
  "Associate chat View BUFFER with SOURCE when SOURCE is a real editing source.
SOURCE is the buffer the explicit entry point was invoked from: a non-chat,
live buffer, which must not be BUFFER itself.  Anything else leaves the
association exactly as it was, so no entry point can clear or fake one by
being called from a chat surface.

The association also records SOURCE's facts at that moment, so a source that
dies before any request is prepared is still reported rather than silently
dropped."
  (when (and (buffer-live-p buffer) (buffer-live-p source)
             (not (eq buffer source))
             (not (superchat--chat-view-p source)))
    (with-current-buffer buffer
      (setq superchat--origin-buffer source)
      (setq superchat--origin-buffer-name (buffer-name source))
      (setq superchat--origin-known (superchat--origin-snapshot source))))
  buffer)

(defun superchat--origin-known-fallback (view source)
  "Return data-only facts about SOURCE for VIEW after SOURCE has died.
The facts recorded from SOURCE while it was alive, marked `:dead' so the
request reports the capture AND that its source is gone, or -- when no facts
were ever recorded -- at least the name kept when the association was made,
reported the same way rather than read off a same-named buffer.  A killed
buffer reports nil for every accessor, `buffer-name' included, so what the
View kept is the only honest answer left; \"not silent\" is the floor here,
not \"fully described\"."
  (let ((known (and (buffer-live-p view)
                    (buffer-local-value 'superchat--origin-known view)))
        (recorded (and (buffer-live-p view)
                       (buffer-local-value 'superchat--origin-buffer-name view))))
    (cond
     ((plist-get known :name) (append known (list :dead t)))
     (recorded (list :name recorded :dead t))
     ((buffer-name source) (list :name (buffer-name source) :dead t))
     (t (list :dead t)))))

(defun superchat--origin-snapshot (source)
  "Return a data-only snapshot of live SOURCE, or nil.
Names, paths, position, modification tick and the selection valid at capture
time; no live object and no buffer contents beyond the selection, so the
result may be recorded and replayed.

Nil when SOURCE is nil, when it is not live, or when it is live but has no
name: there are no facts to report about a buffer that is gone, and a killed
buffer answers `buffer-name' with nil rather than with its old name.  What a
dead source is reported from is `superchat--origin-known-fallback' -- the
facts kept while it was alive -- never this function."
  (when (and source (buffer-live-p source))
    (with-current-buffer source
      (let ((name (buffer-name)))
        (when name
          (list :name name
                :file (buffer-file-name)
                :major-mode (symbol-name major-mode)
                :tick (buffer-chars-modified-tick)
                :point (point)
                :selection (when (use-region-p)
                             (buffer-substring-no-properties
                              (region-beginning) (region-end)))))))))

(defun superchat--origin-render (snapshot)
  "Render data-only SNAPSHOT's facts as one readable block, or nil.
Every fact appears exactly once.  The user's selection is deliberately NOT
part of this text: it is user data with its own slot, rendered separately by
`superchat--origin-selection-render', so it appears in the request exactly
once and never twice.  A dead source is reported as the captured facts plus
that fact, never as a silently substituted same-named buffer."
  (when (and (listp snapshot) (plist-get snapshot :name))
    (let ((name (plist-get snapshot :name))
          (file (plist-get snapshot :file))
          (mode (plist-get snapshot :major-mode))
          (point (plist-get snapshot :point)))
      (string-trim-right
       (concat
        "[Emacs origin]\n"
        (format "Buffer: %s\n" name)
        (when (and file (not (string-empty-p file)))
          (format "File: %s\n" file))
        (when (and mode (not (string-empty-p mode)))
          (format "Major mode: %s\n" mode))
        ;; The contract forbids claiming this is where the user is looking
        ;; without window evidence, and a bare "Point: 1" reads as exactly
        ;; that claim, so the line says which buffer the position is in.
        (when point (format "Point in that buffer: %s\n" point))
        (when (plist-get snapshot :dead)
          "The source buffer no longer exists; these are the facts captured when the request was prepared.\n"))
       ;; The assembler joins fragments with a blank line; a trailing newline
       ;; here would make that three, which a byte-for-byte baseline freezes.
       "\n+"))))

(defun superchat--origin-selection-render (snapshot)
  "Render SNAPSHOT's explicitly selected text, or nil without one.
The selection is the user's own words, so it enters the request as data in
its own right, in the attachment slot the contract gives it: labelled here,
never treated as an instruction, and never folded into the facts block."
  (let ((selection (and (listp snapshot) (plist-get snapshot :selection))))
    (when (and (stringp selection) (not (string-empty-p selection)))
      (format "Selected text:\n%s" selection))))

(defun superchat--origin-selection (snapshot)
  "Return the user's explicitly selected text in SNAPSHOT, or nil.
Data only, reported separately from the surrounding facts because it enters
the request in its own slot."
  (let ((selection (and (listp snapshot) (plist-get snapshot :selection))))
    (and (stringp selection) (not (string-empty-p selection)) selection)))

(defun superchat--capture-origin ()
  "Capture the current Emacs location as a stable request origin plist.
The plist is the dispatching caller's own source; the modification tick is
carried so a request snapshot built from it can report the same facts an
association-time snapshot would."
  (list :buffer (buffer-name)
        :file (buffer-file-name)
        :major-mode major-mode
        :point (point)
        :tick (buffer-chars-modified-tick)
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
   (or (plist-get plan :preview-system-prompt)
       (plist-get plan :system-prompt) "")
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
          (insert (or (plist-get plan :preview-system-prompt)
                      (plist-get plan :system-prompt) ""))
          (insert "\n\nPrompt (editable):\n")
          (add-text-properties
           start (point) '(read-only t rear-nonsticky (read-only))))
        (setq-local superchat--request-prompt-start (point-marker))
        (insert (or (plist-get plan :prompt) ""))
        (setq-local superchat--request-prompt-end (point-marker))
        ;; The row below is chrome, not part of the request body.  The end
        ;; marker advances over insertions at its own position, which is what
        ;; makes text typed at the end of the prompt join the prompt; hold it
        ;; still while the chrome goes in, then arm it again so typing still
        ;; works.  Without the hold, `superchat-request-send' captures the
        ;; button labels as part of the user's message.
        (set-marker-insertion-type superchat--request-prompt-end nil)
        (let ((row-start (save-excursion (insert "\n\n") (point))))
          (save-excursion
            (goto-char row-start)
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
             row-start (point)
             '(read-only t front-sticky nil rear-nonsticky (read-only)))))
        (set-marker-insertion-type superchat--request-prompt-end t)
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

;; The dispatcher declares this variable special, but compiling this file
;; does not execute that declaration before the `let*' below is compiled.
;; A file-local declaration keeps that binding dynamic instead of lexical.
(defvar superchat--send-transaction)

(defun superchat--execute-plan-in-chat (plan)
  "Render the user side of PLAN and execute it in the chat buffer.
The submission is one more send from this View, so it is armed with the same
send transaction as an ordinary immediate chat send: a refused driver answer
puts the View back instead of leaving a user row with no Run."
  (let* ((buffer (superchat--ensure-request-chat-buffer))
         (input (or (plist-get plan :input) ""))
         (superchat--send-transaction
          (with-current-buffer buffer
            (superchat--send-transaction-arm input))))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
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
            (superchat-execute-request-plan plan)))
      (superchat--send-transaction-settle))))

(defun superchat-submit (input &optional origin inspect)
  "Submit INPUT from ORIGIN, or display it when INSPECT is non-nil.
Returns the prepared Request Plan in both cases."
  (superchat-plugin-require-active-row 'chat)
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
  "Send the edited request in the current inspector buffer.
The send is armed with the same transaction as an ordinary chat send, so a
refused submission rolls the chat View back.  The inspector itself stays open
on a refusal -- its edited body is what must survive -- and closes on any
accepted send, as before."
  (interactive)
  (superchat-plugin-require-active-row 'chat)
  (let ((plan (superchat--request-plan-with-edited-prompt))
        (inspector (current-buffer)))
    (superchat--execute-plan-in-chat plan)
    (unless (superchat--send-refusal-p (plist-get plan :run-handle))
      (when (buffer-live-p inspector)
        (kill-buffer inspector)))))

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
    (when-let* ((proj (project-current)))
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
  (if-let* ((root (superchat--project-root)))
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
  (superchat-plugin-require-active-row 'chat)
  (let* ((code (buffer-substring-no-properties beg end))
         (source-buffer (buffer-name))
         (file-path (buffer-file-name))
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
    (superchat--insert-context-into-chat context-text instruction
                                         source-buffer mode-name
                                         (or project-root default-directory))))

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
  (superchat-plugin-require-active-row 'chat)
  (let* ((bounds (superchat--get-enclosing-defun-bounds))
         (code (if bounds
                   (buffer-substring-no-properties (car bounds) (cdr bounds))
                 (user-error "Could not determine enclosing defun bounds")))
         (source-buffer (buffer-name))
         (file-path (buffer-file-name))
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
    (superchat--insert-context-into-chat context-text instruction
                                         source-buffer mode-name
                                         (or project-root default-directory))))

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
                                                         source-buffer src-mode-name
                                                         &optional tools-directory)
  "Insert CONTEXT-TEXT with optional INSTRUCTION into the superchat prompt.
SOURCE-BUFFER and SRC-MODE-NAME are used for the header message.
When TOOLS-DIRECTORY is non-nil, it is set as the destination chat
View's buffer-local `superchat-tools-default-directory', so tool
search scope follows the source buffer's project even when the chat
View is already open and has its own buffer-local value."
  (let* ((inst (if (and instruction (not (string-empty-p instruction)))
                   instruction
                 "Please examine the following code:"))
         (buffer (get-buffer superchat-buffer-name)))
    ;; Only the missing-or-promptless case needs creating.  An already-open
    ;; View (owned or not) is used as-is, unchanged, exactly as before —
    ;; routing it back through the initializer would silently create and
    ;; adopt a *second*, EditorWorkspace-owned buffer instead of reusing it,
    ;; since `superchat-editor-workspace-buffer' does not recognize this one
    ;; as already owned.  The shared initializer only creates the buffer
    ;; under EditorWorkspace ownership, inserts the title plus
    ;; onboarding/recent-Sessions exactly like any other fresh View, and
    ;; ensures the prompt marker.
    (unless (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (and superchat--prompt-start
                        (marker-position superchat--prompt-start))))
      (setq buffer (superchat--ensure-chat-view superchat-buffer-name)))
    (with-current-buffer buffer
      (when tools-directory
        (setq-local superchat-tools-default-directory tools-directory))
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
    (define-key map (kbd "C-c C-M-r") #'superchat-regenerate)
    (define-key map (kbd "C-c C-M-e") #'superchat-edit-last-message)
    (define-key map (kbd "C-c C-p") #'superchat-pause)
    (define-key map (kbd "C-c C-r") #'superchat-resume)
    (define-key map (kbd "C-c C-g") #'superchat-agents-panel-show)
    (define-key map (kbd "C-c C-a") #'superchat-rewrite-apply-code-block-at-point)
    map)
  "Keymap for superchat-mode.")

(define-minor-mode superchat-mode
  "A minor mode to provide chat functionalities in an Org buffer."
  :init-value nil
  :lighter (:eval
            (superchat--mode-line-safe-string
             (format " SChat[%s%s]"
                     superchat-authority-mode
                     (or (superchat--mode-line-run-state) ""))))
  :keymap superchat-mode-map
  (if superchat-mode
      ;; When turning the mode on
      (progn
        (unless superchat--header-line-installed-p
          (setq-local superchat--saved-header-line-format header-line-format)
          (setq-local superchat--header-line-installed-p t))
        (setq-local header-line-format
                    '(" " (:eval
                           (superchat--mode-line-safe-string
                            (superchat--header-line-run-state)))))
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
        (add-hook 'kill-buffer-hook #'superchat--clear-follow-ups-on-kill nil t)
        ;; `kill-buffer-hook' runs only after Emacs accepted its query
        ;; functions.  This exact View owner retires Runtime execution before
        ;; its Preset Scope is withdrawn; a vetoed kill leaves it untouched.
        (add-hook 'kill-buffer-hook #'superchat--retire-preset-on-view-kill nil t)
        ;; A major-mode change clears local minor modes without running this
        ;; mode's -1 body.  Retire while the exact owner references still
        ;; exist; this hook is local and cannot affect a sibling chat View.
        (add-hook 'change-major-mode-hook
                  #'superchat--retire-preset-on-major-mode-change nil t)
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
    ;; When turning the mode off.  This is an explicit Preset owner release,
    ;; not a generic Scope disposal rule and never affects global/plain Runs.
    (superchat--deactivate-active-preset "Chat View mode disabled")
    (superchat--cancel-status-refresh-timer)
    (remove-hook 'kill-buffer-hook #'superchat--cancel-status-refresh-timer t)
    (when superchat--header-line-installed-p
      (setq-local header-line-format superchat--saved-header-line-format)
      (setq-local superchat--header-line-installed-p nil))
    (kill-local-variable 'completion-at-point-functions)
    (kill-local-variable 'corfu-auto-prefix)
    (kill-local-variable 'corfu-auto)
    (kill-local-variable 'company-backends)
    (when (fboundp 'corfu-mode) (corfu-mode -1))
    (when (fboundp 'company-mode) (company-mode -1))
    (remove-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit t)
    (remove-hook 'kill-buffer-hook #'superchat--clear-follow-ups-on-kill t)
    (remove-hook 'kill-buffer-hook #'superchat--retire-preset-on-view-kill t)
    (remove-hook 'change-major-mode-hook
                 #'superchat--retire-preset-on-major-mode-change t)))

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
  (if-let* ((skills (and (fboundp 'superchat-skills-completion-list)
                        (superchat-skills-completion-list))))
      (let ((skill (completing-read "Skill: " skills nil t)))
        (unless (string-empty-p skill)
          (insert ">" skill " ")))
    (user-error "No skills available")))

(defun superchat--transient-pick-command ()
  "Prompt for a slash command and insert /command."
  (interactive)
  (if-let* ((commands (superchat--get-all-command-names)))
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
          ;; Open (or switch to) the destination View *first* and add the
          ;; files from within its own buffer, so
          ;; `superchat--add-file-to-context' localizes the write there —
          ;; calling it from this Dired buffer instead would push onto
          ;; `superchat--current-context-files''s global default, leaking
          ;; into every other Session.
          (let ((target (superchat)))
            (with-current-buffer target
              (dolist (f files)
                (superchat--add-file-to-context f)))
            (message "superchat: Added %d file(s) to context" (length files)))
        (message "superchat: No files marked")))))

;;;###autoload
(eval-after-load 'dired
  '(define-key dired-mode-map (kbd "C-c s") #'superchat-dired-send))

;; ── Embark Integration (soft dependency) ──

(defvar embark-file-map)
(defvar embark-region-map)

(defun superchat-embark-file-send (file)
  "Send FILE to Superchat as context from an Embark file target.
Opens (or switches to) the destination View *first* and adds FILE from
within its own buffer, so `superchat--add-file-to-context' localizes the
write there — calling it from the source buffer instead (as this used
to) pushes onto `superchat--current-context-files''s global default,
leaking the attachment into every other Session; see
`superchat-dired-send', fixed the same way."
  (interactive "fFile: ")
  (let ((target (superchat)))
    (with-current-buffer target
      (superchat--add-file-to-context file)))
  (message "superchat: Added %s to context" file))

;;;###autoload
(with-eval-after-load 'embark
  (keymap-set embark-file-map "s" #'superchat-embark-file-send)
  (keymap-set embark-region-map "S" #'superchat-send-region-or-defun))

;; ── Bub Phase 1 / Migration D: post-turn tape hook ──

(defun superchat--turn-belongs-to-current-view-p (turn)
  "Return non-nil when TURN still belongs to this View's Session.
The nil case preserves the headless/pre-session behavior; once a View has a
Session ID, an older Run must not repopulate its cleared history."
  (or (null superchat--session-id)
      (equal (superchat-turn-session-id turn) superchat--session-id)))

(defun superchat--hook-record-user-message (_run turn)
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

(superchat-define-plugin chat
  :doc "The chat window as a row: it owns the `chat' Surface name.
The code stays in superchat.el (eager `require'), so this row owns
registration and the entry guards, not file loading.  Its call-time needs
(view sink, commands, collectors, recorders) are met by the chat part rows
at call time, so there is no mount-time `:inject' -- an unmounted transport
must not read as a withdrawn Surface name."
  :apply (lambda (_scope _config)
           (superchat-register-surface 'chat)))

(superchat-define-plugin chat-turn-recorder
  :doc "Records the user's visible message row when a Run seals.
Attachment facts are not recorded here: Runtime writes them when it admits the
request, beside `intent.received', so that an image cannot reach the model with
nothing recording that it did when this row is unmounted mid-Run."
  :apply (lambda (scope _)
           (superchat-on scope 'run-sealing 'record-user-message
                         #'superchat--hook-record-user-message :required t)))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(defvar superchat--builtin-command-handlers)

(superchat-define-plugin chat-commands
  :doc "Slash command registry."
  :apply
  (lambda (scope _)
    (dolist (entry superchat--builtin-command-handlers)
      (superchat-register-command (car entry) (cdr entry)))
    ;; Permission levels replace /authority at the cutover; until then the
    ;; command would show a level that Authority does not yet obey.
    (when superchat--permission-levels-enabled
      (superchat-register-command "permissions" #'superchat--cmd-permissions))
    ;; The zero-argument built-ins (/backend, /models, /mcp*, /refresh-models)
    ;; go through the same registry: unmounting this plugin removes them.
    (dolist (entry superchat--builtin-commands)
      (let ((func (cdr entry)))
        (superchat-register-command
         (car entry)
         (lambda (&rest _)
           (if (fboundp func)
               (let ((result (funcall func)))
                 (if (stringp result) (list :type :buffer :content result) (list :type :noop)))
             (list :type :echo :content (format "Command `/%s' function not found." (car entry))))))))
    (superchat-service-provide
     scope 'commands
     (superchat-service-table
      register #'superchat-register-command
      lookup #'superchat--command-visible-handler
      list #'superchat--command-visible-names))))

(provide 'superchat)

;; Command dispatch alist — one entry per slash command.
;; Much simpler than individual hook functions that all check (equal cmd "xxx").
(defconst superchat--builtin-command-handlers
  '(("recall"        . superchat--cmd-recall)
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
    ("compact-log"   . superchat--cmd-compact-log)
    ("expand"        . superchat--cmd-expand)
    ("subagent"      . superchat--cmd-subagent)
    ("agents"        . superchat--cmd-agents)
    ("cancel-agent"  . superchat--cmd-cancel-agent)
    ("reset"         . superchat--cmd-reset)
    ("clear-context" . superchat--cmd-clear-context)
    ("clear"         . superchat--cmd-clear)
    ("define"        . superchat--cmd-define)
    ("workflow"      . superchat--cmd-workflow))
  "The built-in slash commands; registered by the `chat-commands' plugin.")

(defun superchat--command-visible-installation (name &optional scope)
  "Return the live command installation NAME visible from SCOPE.

This is the Scope-layer view used by the commands Service.  It deliberately
does not consult an obsolete process-global command table."
  (let ((installation
         (superchat-scope-lookup
          (superchat-execution-scope-resolve scope) 'command name)))
    installation))

(defun superchat--command-visible-handler (name &optional scope)
  "Return NAME's live command handler visible from SCOPE, or nil."
  (plist-get (superchat--command-visible-installation name scope) :handler))

(defun superchat--command-visible-names (&optional scope)
  "Return live slash command names visible from SCOPE in registry order."
  (delq nil
        (mapcar
         (lambda (entry)
           (and (cdr entry) (car entry)))
         (superchat-scope-merge
          (superchat-execution-scope-resolve scope) 'command))))

(defun superchat--command-local-installation (name &optional scope)
  "Return NAME's installation in SCOPE's own command layer only."
  (superchat-scope-local-lookup scope 'command name))

(cl-defun superchat-register-command (name handler &key scope)
  "Register slash command NAME to dispatch to HANDLER.

NAME is the command word without the leading slash.  HANDLER is called
as (NAME ARGS INPUT LANG TARGET-MODEL) and returns a result plist such
as \\='(:type :echo :content \"...\").  HANDLER may be an autoloaded symbol,
so a plugin can register at load time and stay unloaded until first use.

SCOPE explicitly selects the installation layer.  Without it, plugin effect
ownership wins over the current execution Scope, which in turn wins over the
global Scope.  A duplicate NAME in one layer is rejected atomically; replace
an existing command with an explicit `superchat-unregister-command' followed
by registration.  Returns NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "Command name must be a non-empty string"))
  (unless (or (functionp handler)
              (and (symbolp handler) handler))
    (error "Command handler must be a function or function symbol"))
  (let* ((target (superchat-registration-scope-resolve scope))
         (installation (list :handler handler :undo nil))
         ;; Scope rejects duplicates before adding either registration or its
         ;; cleanup.  The outer undo seals this exact installation before
         ;; removing its Scope entry, so a stale undo cannot erase a newer one.
         (withdraw (superchat-scope-register target 'command name installation))
         (undo
         (superchat-scope-effect
           target
           (lambda ()
             (plist-put installation :undo nil)
             (funcall withdraw)))))
    (plist-put installation :undo undo))
  name)

(cl-defun superchat-unregister-command (name &key scope)
  "Withdraw NAME from SCOPE's own command layer.

Without an explicit SCOPE, use the same registration target selection as
`superchat-register-command'.  This never withdraws an ancestor's command;
after local withdrawal a visible parent definition may reappear.  Returns
non-nil when this exact local installation was removed."
  (when-let* ((installation (superchat--command-local-installation name scope))
              (undo (plist-get installation :undo)))
    (funcall undo)
    t))

(defcustom superchat-memory-recall-scope 'all
  "Which sessions the user's own `/recall' command searches.
`all' (the default) searches every session's tape, with the current
session's own rows fetched first so they cannot be crowded out by
better-ranked rows from other sessions.  `session' restricts the search
to the current session only, matching Superchat's original recall
behaviour exactly.

This option governs `/recall' only -- the one lookup a human user
directly typed.  Automatic (silently attached) recall and the
model-initiated `memory_search' tool both instead follow
`superchat-memory-auto-recall-cross-session': the model, not the user,
decides when those run, so they follow the more conservative,
privacy-oriented cross-session policy rather than this option."
  :type '(choice (const :tag "All sessions" all)
                 (const :tag "Current session only" session))
  :group 'superchat)

(defcustom superchat-memory-auto-recall-cross-session 'remembered
  "What automatic indexed recall and `memory_search' may send from OTHER
sessions.  Rows from the *current* session are always eligible (any
kind, as before -- automatic recall additionally excludes its own
recent conversation-history window, which `memory_search' does not,
since it is an explicit model lookup rather than something silently
attached to every turn); this option controls only rows that come from
a session other than the one the user is chatting in right now.
Eligibility is enforced by the search itself, before any row-count
limit, so an eligible row is never crowded out by ineligible ones.

`remembered' (the default) admits only rows that are an explicit
`/remember' anchor -- kind \"anchor\", topic \"remember\" -- so a fact the
user deliberately asked Superchat to remember still reaches a later
session or a `memory_search' call, but an ordinary chat turn from an
unrelated past conversation is never silently sent to the model just
because it happens to share a word with the current input or query.

`all' restores Superchat's original cross-session behaviour: any row
from any session that matches the search is eligible, ranked by
relevance with the current session preferred.

`none' disables cross-session recall entirely for both automatic recall
and `memory_search': only the current session's own rows are ever
attached or returned.

The user's own `/recall' command is unaffected by this option; it keeps
consulting `superchat-memory-recall-scope' instead, since a human
explicitly typing `/recall' is not the same privacy question as content
the model decides on its own to attach or look up."
  :type '(choice (const :tag "Only explicitly remembered facts" remembered)
                 (const :tag "Any matching row from any session" all)
                 (const :tag "Current session only" none))
  :group 'superchat)

(defun superchat--cmd-recall (_cmd args _input _lang _target-model)
  "Recall relevant context from the tape (and legacy memory table as fallback).
ARGS is the search query."
  (if (and args (> (length args) 0))
      (let* ((trimmed (string-trim args))
             (session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (search-limit 20)
             (scope (if (boundp 'superchat-memory-recall-scope)
                        superchat-memory-recall-scope
                      'all))
             (tape-rows
              (when (fboundp 'superchat-view-search)
                (cond
                 ((eq scope 'session)
                  (superchat-view-search trimmed session-id search-limit))
                 ((fboundp 'superchat-db-tape-search-prioritized)
                  ;; Current-Session rows are fetched first, in the query
                  ;; itself, so they cannot be starved out of SEARCH-LIMIT
                  ;; by better-ranked rows from other Sessions.
                  (superchat-db-tape-search-prioritized
                   trimmed session-id search-limit nil))
                 (t (superchat-view-search trimmed nil search-limit)))))
             (tape-mems (mapcar #'superchat-view-row-to-memory-plist tape-rows))
             (selected-anchor-ids
              (mapcar #'car
                      (cl-remove-if-not
                       (lambda (row) (equal "anchor" (nth 3 row)))
                       tape-rows)))
             (legacy-mems
              (when (and (fboundp 'superchat-memory-retrieve)
                         (< (length tape-mems) search-limit))
                (cl-remove-if
                 (lambda (memory)
                   (let ((anchor-id
                          (and (fboundp
                                'superchat-memory-verified-remember-anchor-id)
                               (superchat-memory-verified-remember-anchor-id
                                memory session-id))))
                     (and anchor-id (member anchor-id selected-anchor-ids))))
                 (superchat-memory-retrieve trimmed))))
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
            (superchat-memory-capture-explicit
             trimmed title
             :remember-session-id session-id
             :remember-anchor-id anchor-id
             :remember-source-id entry-id))
          `(:type :echo :content ,(format "Anchor added: %s (tape ID: %s)" title anchor-id)))
      (if-let* ((exchange (superchat--last-exchange-struct)))
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
      (if-let* ((preset (superchat-skills-load skill-name)))
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
  (let ((superchat-execution-scope (superchat--chat-execution-scope)))
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
            :content ,(format "Unknown Authority policy: %s" policy)))))))

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

(defun superchat--cmd-permissions (_cmd args _input _lang _target-model)
  "Show or select the current Session's permission level."
  (let ((value (string-trim (or args ""))))
    (condition-case err
        (progn
          (unless (string-empty-p value)
            (superchat-permission-set superchat--session-id value))
          `(:type :echo
            :content ,(if (string-empty-p value)
                          (superchat-permission-describe superchat--session-id)
                        (format "Permissions: %s — %s" value
                                (plist-get (superchat-permission-level value)
                                           :description)))))
      (error `(:type :echo :content ,(error-message-string err))))))

(defun superchat--cmd-skill (_cmd args _input _lang _target-model)
  "Switch to a skill/preset.
Without ARGS, list available skills.  With ARGS, load and activate that skill."
  (let ((skill-name (string-trim (or args ""))))
    (if (string-empty-p skill-name)
        `(:type :echo
          :content ,(format "Available skills: %s"
                            (string-join (superchat-skills-get-available) ", ")))
      (if-let* ((preset (superchat-skills-load skill-name)))
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
  (if-let* ((define-pair (superchat--parse-define input)))
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

(defun superchat--llm-report-stage (stage)
  "Report transport STAGE through the distribution timing UI."
  (superchat--ttft-log stage))

(defun superchat--llm-report-ttft (elapsed)
  "Report ELAPSED time-to-first-token through the distribution UI."
  (message "⚡ TTFT: %.2fs" elapsed)
  (superchat--annotate-ttft elapsed))

(defun superchat--llm-report-nonpartial-latency (elapsed)
  "Report callback-only end-to-end ELAPSED time through the distribution UI."
  (message "⏱ E2E: %.2fs · TTFT unavailable (non-partial provider)" elapsed)
  (superchat--annotate-nonpartial-latency elapsed))

(superchat-llm-configure
 :response-mode-function #'superchat--detect-response-mode
 :response-mode-indicator-function #'superchat--show-response-mode-indicator
 :adjusted-timeout-function #'superchat--get-adjusted-timeout
 :stage-function #'superchat--llm-report-stage
 :ttft-function #'superchat--llm-report-ttft
 :nonpartial-latency-function #'superchat--llm-report-nonpartial-latency
 :activity-function #'superchat--extend-timeout)

;; Kernel: mount the composed profile (spec §2.1) once every function its rows
;; provide is defined -- this file keeps definitions after `provide'.
(superchat-profile-mount)

;;; superchat.el ends here
