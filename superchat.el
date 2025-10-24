;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))

;; Author: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/superchat
;; Keywords: ai, chat, gptel
;; License: GPL-3.0-or-later

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel nil t)
(require 'gptel-context nil t)
(require 'superchat-memory)
(require 'superchat-tools)
(require 'superchat-workflow)
(require 'superchat-parser)

(defconst superchat-version "0.4"
  "Current Superchat package version.")

(declare-function superchat-memory-compose-title "superchat-memory" (content))
(declare-function superchat-memory-capture-explicit "superchat-memory" (content &optional title))
(declare-function superchat-memory-capture-conversation "superchat-memory" (content &rest options))
(declare-function superchat-memory-auto-capture "superchat-memory" (exchange))
(declare-function superchat-memory-retrieve "superchat-memory" (query-string))
(declare-function superchat-memory-summarize-session-history "superchat-memory" (history-content))

;; Declare gptel functions if available
(declare-function gptel-tool-name "gptel" (tool))
(declare-function gptel-tool-description "gptel" (tool))
(declare-function gptel-backend-models "gptel" (backend))
(declare-function gptel-backend-name "gptel" (backend))

;; Declare MCP functions if available
;; Note: mcp-hub-get-all-tool signature varies between versions
(declare-function mcp-hub-get-all-tool "mcp-hub" (asyncp categoryp errorHandle))
(declare-function mcp-hub-start-all-server "mcp-hub" (&optional callback servers syncp))
(declare-function mcp-hub "mcp-hub" (&optional force-refresh))
(defvar mcp-hub-servers)
(defvar mcp-server-connections)

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

(defcustom superchat-default-save-method 'ask
  "Default save method for conversations."
  :type '(choice (const :tag "Ask each time" ask)
                 (const :tag "New file" new-file)
                 (const :tag "Append to node" append-node)
                 (const :tag "New subnode" new-node)
                 (const :tag "New heading" new-heading))
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
  "Ensure that the necessary directories exist."
  (let ((data-dir superchat-data-directory))
    (unless (file-directory-p data-dir)
      (make-directory data-dir t)))
  (let ((save-dir (superchat--save-directory)))
    (unless (file-directory-p save-dir)
      (make-directory save-dir t)))
  (let ((command-dir (superchat--command-dir)))
    (unless (file-directory-p command-dir)
      (make-directory command-dir t))))

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
  "Approximate maximum number of characters of recent conversation to include in prompts.
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


;; --- Faces ---
(defface superchat-label-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for labels like 'User:'."
  :group 'superchat)

(defface superchat-prompt-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for the input prompt '>'"
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
(defvar-local superchat--retrieved-memory-context nil
  "A string containing context from retrieved memories, to be used in the next query.")
(defvar-local superchat--active-timeout-timer nil
  "The currently active timeout timer for the ongoing LLM response.")
(defvar-local superchat--timeout-extension-amount 30
  "Number of seconds to extend timeout when user confirms a tool action.")

;; --- Global Variables ---
(defvar superchat--builtin-commands
  '(("tools" . superchat-tools-status)
    ("models" . superchat-model-list)
    ("mcp" . superchat-mcp-status)
    ("mcp-start" . superchat-mcp-start-servers))
  "Alist of built-in commands and their prompt templates.")

(defvar superchat--user-commands (make-hash-table :test 'equal)
  "Hash table of user-defined commands and their prompt templates.")

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
      ;; Update timer to fire later
      (timer-set-time superchat--active-timeout-timer
                      (seconds-to-time new-trigger-time))
      (message "⏱️  Timeout extended by %ds (user confirmed tool action)"
               superchat--timeout-extension-amount))))

(defun superchat--record-message (role content)
  "Record a conversation message with ROLE and CONTENT into history."
  (let ((text (string-trim (or content ""))))
    (when (> (length text) 0)
      (setq superchat--conversation-history
            (cons (list :role role :content text)
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
Supports multiple file extensions defined in `superchat-prompt-file-extensions`."
  (when (and prompt-name (superchat--command-dir))
    (cl-find-if #'file-exists-p
                (mapcar (lambda (ext)
                          (expand-file-name (concat prompt-name "." ext)
                                            (superchat--command-dir)))
                        superchat-prompt-file-extensions))))

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

(defun superchat--md-to-org (md-string)
  "Convert a Markdown string to Org-mode format.
This function is adapted from ollama-buddy's implementation,
using a robust two-pass approach to handle code blocks correctly.
This is a pure function that takes a string and returns a converted string."
  (if (string-empty-p md-string)
      ""
    (with-temp-buffer
      (insert md-string)
      (save-excursion
        (save-restriction
          (narrow-to-region (point-min) (point-max))
          (save-match-data
            ;; First, handle code blocks by temporarily protecting their content
            (goto-char (point-min))
            (let ((code-blocks nil)
                  (counter 0)
                  block-start block-end lang content placeholder)
              (while (re-search-forward "```\\(.*?\\)\\(?:\n\\|\\s-\\)\\(\\(?:.\\|\n\\)*?\\)```" nil t)
                (setq lang (match-string 1)
                      content (match-string 2)
                      block-start (match-beginning 0)
                      block-end (match-end 0)
                      placeholder (format "CODE_BLOCK_PLACEHOLDER_%d" counter))
                (push (list placeholder lang content) code-blocks)
                (delete-region block-start block-end)
                (goto-char block-start)
                (insert placeholder)
                (setq counter (1+ counter)))

              ;; Apply regular Markdown to Org transformations
              (goto-char (point-min))
              (while (re-search-forward "^\\([ \t]*\\)[*-+] \\(.*\\)$" nil t)
                (replace-match (concat (match-string 1) "- \\2")))

              (goto-char (point-min))
              (while (re-search-forward "\\*\\*\\([^ ]\\(.*?\\)[^ ]\\)\\*\\*" nil t)
                (replace-match "*\\1*"))

              (goto-char (point-min))
              (while (re-search-forward "\\([ \n]\\)_\\([^ ].*?[^ ]\\)_\\([ \n]\\)" nil t)
                (replace-match "\\1/\\2/\\3"))

              (goto-char (point-min))
              (while (re-search-forward "\\[\\(.*?\\)\\](\\(.*?\\))" nil t)
                (replace-match "[[\\2][\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "`\\(.*?\\)`" nil t)
                (replace-match "=\\1="))

              (goto-char (point-min))
              (while (re-search-forward "^\\(-{3,}\\|\\*{3,}\\)$" nil t)
                (replace-match "-----"))

              (goto-char (point-min))
              (while (re-search-forward "!\\[.*?\\](\\(.*?\\))" nil t)
                (replace-match "[[\\1]]"))

              (goto-char (point-min))
              (while (re-search-forward "^\\(#+\\) " nil t)
                (replace-match (make-string (length (match-string 1)) ?*) nil nil nil 1))

              ;; (goto-char (point-min))
              ;; (while (re-search-forward "—" nil t)
              ;;   (replace-match ", "))

              ;; Restore code blocks
              (dolist (block (nreverse code-blocks))
                (let ((placeholder (nth 0 block))
                      (lang (nth 1 block))
                      (content (nth 2 block)))
                  (goto-char (point-min))
                  (when (search-forward placeholder nil t)
                    (replace-match (format "#+begin_src %s\n%s\n#+end_src" (or lang "") content) t t))))))))
      (buffer-string))))

(defun superchat--insert-prompt ()
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n")  ; Always insert a blank line before the prompt
      (let ((headline
             (if superchat--current-command
                 (format "* User [%s mode]: " superchat--current-command)
               "* User: ")))  ; Removed leading newline for consistency
        (insert (propertize headline 'face 'superchat-prompt-face))
        (setq superchat--prompt-start (point-marker))))))

(defun superchat--prepare-for-response ()
  "Move to end of buffer, insert newline, and set marker for response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (setq superchat--response-start-marker (point-marker)))))

(defun superchat--update-status (message)
  "Update the status message in the chat buffer (e.g., 'Assistant is thinking...')."
  (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert (propertize message 'face 'italic))))))

(defun superchat--prepare-assistant-response-area ()
  "Prepare the buffer for the assistant's response.
This function should only be called once per response."
  (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        (setq superchat--assistant-response-start-marker (point-marker))
        (insert "\n** Assistant\n")
        (setq superchat--response-start-marker nil)))))

(defun superchat--stream-llm-result (chunk)
  "Process a chunk from the LLM streaming response, update UI and accumulate response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (push chunk superchat--current-response-parts)
      (unless superchat--assistant-response-start-marker
        (superchat--prepare-assistant-response-area))
      (goto-char (point-max))
      (insert chunk))))

(defun superchat--process-llm-result (answer)
  "Finalize the LLM response processing.
This function is called ONCE after the entire response has been streamed."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content (if (and answer (not (string-empty-p answer)))
                                answer
                              "Assistant did not provide a response.")))
      ;; If streaming was active, replace the raw content with the formatted version.
      (if (and superchat--assistant-response-start-marker (marker-position superchat--assistant-response-start-marker))
          (progn
            ;; Replace the entire assistant response block: delete the old, insert the new.
            (goto-char superchat--assistant-response-start-marker)
            (delete-region (point) (point-max))
            (insert "\n** Assistant\n")
            (insert (superchat--md-to-org response-content)))
        ;; Fallback for non-streaming or failed streaming.
        (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
          (superchat--prepare-assistant-response-area)
          (insert (superchat--md-to-org response-content))))

      ;; Apply the text property for the assistant's response
      (when (and superchat--assistant-response-start-marker (marker-position superchat--assistant-response-start-marker))
        (put-text-property superchat--assistant-response-start-marker (point-max) 'superchat-role 'assistant)
        (setq superchat--assistant-response-start-marker nil))

      ;; 1. Update conversation history with the full response
      (superchat--record-message "assistant" response-content)
      
      ;; 2. Insert the next prompt (it handles its own spacing)
      (superchat--insert-prompt))))

(defun superchat--current-input ()
  "Return current prompt line user text."
  (when (and superchat--prompt-start (marker-position superchat--prompt-start))
    (let ((input-text (buffer-substring-no-properties superchat--prompt-start (point-max))))
      (string-trim input-text))))

;; --- Model Switching ---

(defun superchat--parse-model-switch (input)
  "Parse input for @model syntax and return (clean-input . model) cons.
If no @model syntax is found, return nil."
  (superchat-parser-model-switch input))

(defcustom superchat-manual-models nil
  "Manually configured list of available models.
If set, this list will be used instead of trying to get models from gptel.
Example: (\"gpt-4\" \"gpt-3.5-turbo\" \"claude-3-opus\" \"qwen3-coder:30b-a3b-q8_0\")"
  :type '(repeat string)
  :group 'superchat)

(defun superchat--get-ollama-models ()
  "Get list of available Ollama models by running 'ollama list' command.
Returns a list of model names without @ prefix."
  (let ((output (shell-command-to-string "ollama list")))
    (when (and output (not (string-empty-p (string-trim output))))
      (let ((lines (split-string output "\n" t))
            (models '()))
        (dolist (line lines)
          ;; Skip header line
          (unless (string-match-p "^NAME" line)
            (when (string-match "^\\([a-zA-Z0-9_.:-]+\\)" line)
              (push (match-string 1 line) models))))
        (nreverse models)))))

(defun superchat-sync-ollama-models ()
  "Sync Ollama models to gptel backend configuration.
This function reads models from 'ollama list' and updates the gptel backend."
  (interactive)
  (let ((ollama-models (superchat--get-ollama-models)))
    (if ollama-models
        (progn
          (message "Found %d Ollama models: %s" 
                   (length ollama-models)
                   (mapconcat #'identity ollama-models ", "))
          (when (and (boundp 'gptel-backend) 
                     gptel-backend
                     (string-match-p "ollama" (format "%s" gptel-backend)))
            ;; Update the models slot of the backend
            (setf (gptel-backend-models gptel-backend)
                  (mapcar #'intern ollama-models))
            (message "✅ Synced %d models to gptel backend" (length ollama-models))
            (message "Available models: %s" 
                     (mapconcat (lambda (m) (format "@%s" m)) 
                               ollama-models ", ")))
          ollama-models)
      (message "⚠️ No Ollama models found or ollama command not available")
      nil)))

(defun superchat--get-available-models ()
  "Get list of available models, prefixed with @.
First tries manual configuration, then falls back to gptel, then defaults."
  (let ((models (cond
                 ;; 1. Use manually configured models if available
                 (superchat-manual-models superchat-manual-models)
                 
                 ;; 2. Try to get models from gptel backend
                 ((and (boundp 'gptel-backend) gptel-backend
                       (fboundp 'gptel-backend-models))
                  (gptel-backend-models gptel-backend))
                 
                 ;; 3. Fallback to default models
                 (t '("default")))))
    (mapcar (lambda (model)
              (concat "@" (if (symbolp model) (symbol-name model) model)))
            models)))

(defun superchat-model-list ()
  "Show available models for @ syntax."
  (interactive)
  (let ((models (superchat--get-available-models))
        (current-model (if (boundp 'gptel-model) gptel-model "unknown"))
        (ollama-models (superchat--get-ollama-models))
        (is-ollama-backend (and (boundp 'gptel-backend) 
                                gptel-backend
                                (string-match-p "ollama" (format "%s" gptel-backend))))
        (model-source (cond
                        (superchat-manual-models "Manual configuration")
                        ((and (boundp 'gptel-backend) gptel-backend) "gptel backend")
                        (t "Default fallback"))))
    (let ((content
           (concat
            (format "Available Models for @ Syntax\n\n")
            (format "Model source: %s\n" model-source)
            (format "Current model: %s\n\n" current-model)
            "Usage: @model_name\n"
            "Example: @qwen3-coder:30b-a3b-q8_0 Hello, how are you?\n\n"
            "Available models (from gptel backend):\n"
            (mapconcat (lambda (model)
                         (format "  %s" model))
                       models "\n")
            "\n\n"
            ;; Show sync info if using Ollama
            (if (and is-ollama-backend ollama-models)
                (concat
                 "💡 Tip: Your Ollama has these models:\n"
                 (mapconcat (lambda (m) (format "  - %s" m)) ollama-models "\n")
                 "\n\n"
                 "To sync Ollama models to gptel, run:\n"
                 "  M-x superchat-sync-ollama-models\n\n"
                 "Or add to your config:\n"
                 "  (with-eval-after-load 'superchat\n"
                 "    (superchat-sync-ollama-models))\n")
              (if superchat-manual-models
                  ""
                "Note: To configure custom models, set `superchat-manual-models` variable.\n"
                "Example: (setq superchat-manual-models '(\"qwen3-coder:30b-a3b-q8_0\" \"gemma3:12B\"))\n")))))
      ;; For interactive use, show help window
      (when (called-interactively-p 'interactive)
        (with-help-window "*SuperChat Models*"
          (with-current-buffer standard-output
            (insert content))))
      ;; Return content for display in chat
      content)))


;; --- Command System ---

(defun superchat--load-user-commands ()
  "Load all custom command prompt files from `superchat-command-dir`."
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
          (let ((name (file-name-base file)))
            (with-temp-buffer
              (insert-file-contents file)
              (puthash name (buffer-string) superchat--user-commands))))))))

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
  "Define a new command, persist the prompt to a file and load it."
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
  "Return all available commands as a formatted string."
  (with-temp-buffer
    (insert "Available Commands:\n\n")
    
    ;; Built-in functional commands
    (insert "  /commands: Show this list.\n"
            "  /reset: Reset to default chat mode.\n"
            "  /clear-context: Clear all files from current session context.\n"
            "  /clear: Clear the chat and context.\n")
    
    ;; Add built-in commands (tools, models, mcp, etc.)
    (dolist (cmd superchat--builtin-commands)
      (insert (format "  /%s\n" (car cmd))))
    
    ;; Add user-defined commands
    (when (> (hash-table-count superchat--user-commands) 0)
      (insert "\nUser-defined Commands:\n")
      (let ((cmds (sort (hash-table-keys superchat--user-commands) #'string<)))
        (dolist (cmd cmds)
          (insert (format "  /%s\n" cmd)))))
    
    (insert "\nCommand Definition:\n"
            "  /define <name> \"<prompt>\": Define a new command.\n")
    (buffer-string)))


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
  (let ((cmds '("/define" "/commands" "/reset" "/clear-context" "/clear" "/remember" "/recall"))) ; Meta commands
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
     ;; --- 新增这个分支 ---
     ((string-match "\\(>[a-zA-Z0-9_-]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start)))
        `(,completion-start ,end ,(when (fboundp 'superchat-workflow-completion-workflows)
                                           (superchat-workflow-completion-workflows))
          . (metadata (category . superchat-workflow)))))
     ;; --- 保留原有的 @ 和 / 分支 ---
     ;; Matches @word at the end of the string.
     ((string-match "\\(@[a-zA-Z0-9_.-]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start)))
        `(,completion-start ,end ,(superchat--get-available-models)
          . (metadata (category . superchat-model)))))
     ;; Matches /word at the end of the string.
     ((string-match "\\(/[a-zA-Z0-9_-]*\\)$" text)
      (let* ((symbol-start (match-beginning 0))
             (completion-start (+ prompt-start symbol-start)))
        `(,completion-start ,end ,(superchat--get-all-command-names)
          . (metadata (category . superchat))))))))

(defun superchat--parse-define (input)
  "Parse /define command input."
  (when (and input (stringp input))
    (cond
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-+\"\\(\\(?:.\\|\\n\\)*?\\)\"\\s-*$" input)
      (cons (match-string 1 input) (or (match-string 2 input) "")))
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-*$" input)
      (cons (match-string 1 input) ""))
     (t nil))))

(defun superchat--parse-command (input)
  "Parse command input, return (command . args) or nil."
  (superchat-parser-command input))

;; --- File Path Handling ---
;; Match a file reference introduced by `#`, allowing optional spaces after it,
;; then either a quoted path with spaces, or an unquoted path up to whitespace.
(defconst superchat--file-ref-regexp
  "#\\s-*\\(?:\"\\([^\"]+\\)\"\\|\\(\\(?:~\\|/\\)[^[:space:]#]+\\)\\)"
  "Regexp to capture a file path after a leading '#'.
Captures either a quoted path in group 1, or an unquoted absolute path in group 2.")

(defun superchat--normalize-file-path (file-path)
  "Normalize FILE-PATH: unescape spaces, strip quotes, expand, trim newline."
  (when (and file-path (stringp file-path))
    (let ((fp (replace-regexp-in-string "\\\\ " " " file-path)))
      (when (and (>= (length fp) 2)
                 (string= "\"" (substring fp 0 1))
                 (string= "\"" (substring fp -1)))
        (setq fp (substring fp 1 -1)))
      (setq fp (expand-file-name fp))
      (setq fp (replace-regexp-in-string "\n$" "" fp))
      fp)))

(defun superchat--extract-file-path (input)
  "Extract and normalize file path from INPUT string.
Handles various edge cases like spaces, parentheses, and quotes in file paths."
  (let ((file-path (superchat-parser-extract-file-path input)))
    (when file-path
      ;; --- DIAGNOSTIC MESSAGE ---
      (message "superchat: Extracted file path: %s" file-path)
      (message "superchat: File exists: %s" (file-exists-p file-path))
      ;; --- END DIAGNOSTIC ---
      file-path)))

;; --- Main Send Logic ---
(defun superchat--format-retrieved-memories (memories)
  "Format a list of retrieved memories into a string for the prompt."
  (if (not memories)
      ""
    (with-temp-buffer
      (insert "--- Retrieved Memories ---\n")
      (dolist (mem memories)
        (insert (format "* %s (ID: %s)\n"
                        (plist-get mem :title)
                        (plist-get mem :id)))
        (insert (format "  :PROPERTIES:\n  :TIMESTAMP: %s\n  :TYPE: %s\n  :TAGS: %s\n  :END:\n"
                        (plist-get mem :timestamp)
                        (plist-get mem :type)
                        (plist-get mem :tags)))
        (insert (plist-get mem :content))
        (unless (string-suffix-p "\n" (plist-get mem :content))
          (insert "\n")))
      (insert "--- End of Retrieved Memories ---\n\n")
      (buffer-string))))

(defun superchat--build-final-prompt (input &optional template lang)
  "Build the final prompt string, adding file and conversation context.
This function is rewritten to use a functional data flow, avoiding
in-place modification of variables to prevent subtle environment bugs.
Returns a plist containing :prompt and :user-message values."
  (let ((current-lang (or lang superchat-lang)))  ; 使用传入的lang或当前设置
    ;; (message "=== DEBUG: BUILD-FINAL-PROMPT === Input: '%s', Template provided: %s, Template length: %s, Lang: %s"
    ;;          input (if template "YES" "NO") (if template (length template) "N/A") current-lang)
    (let* ((prompt-template (or template superchat-general-answer-prompt))
           (lang-instruction
            (unless (or (string-empty-p current-lang)
                        (string= current-lang "English")
                        (string-match-p (regexp-quote "$lang") prompt-template))
              (format "Your response must be in %s." current-lang))))

    ;; (message "=== DEBUG: SUPERCHAT-GENERAL-ANSWER-PROMPT === '%s'" superchat-general-answer-prompt)

    ;; Phase 1: Prepend memory context if it exists, and then clear it.
  (let ((memory-context superchat--retrieved-memory-context))
    (setq superchat--retrieved-memory-context nil) ; Ensure it's used only once

    ; Phase 2: Parse input to separate user query from file path.
  ;; This phase avoids mutating 'user-query' by using different variables.
  (let* ((initial-query (string-trim (or input "")))
         (file-path
          (when (string-match superchat--file-ref-regexp initial-query)
            (superchat--normalize-file-path
             (or (match-string 1 initial-query)
                 (match-string 2 initial-query)))))
         (user-query
         (if file-path
           (let* ((path-start (match-beginning 0))
                  (path-end (match-end 0))
                  (text-before (substring initial-query 0 path-start))
                  (text-after (substring initial-query path-end)))
             (string-trim (concat (string-trim text-before) " " (string-trim text-after))))
         initial-query)))

    ;; (message "--- BUILD-PROMPT --- File path parsed as: '%s'" file-path)
    ;; (message "--- BUILD-PROMPT --- Final user query: '%s'" user-query)

    ;; Phase 2: Build context from the parsed file path.
    (let ((inline-context
           (when file-path
             (superchat--add-file-to-context file-path)
             (when superchat-inline-file-content
               (superchat--make-inline-context file-path)))))

      ;; Phase 4: Construct the final prompt string.
      (let* ((prompt-template
              (or template superchat-general-answer-prompt))
             (base-prompt (let ((processed-template prompt-template))
                            ;; First, replace $lang if present
                            (when (string-match-p (regexp-quote "$lang") processed-template)
                              ;; (message "=== DEBUG: LANG-REPLACEMENT === Found $lang in template, replacing with: '%s'" current-lang)
                              (setq processed-template
                                    (replace-regexp-in-string (regexp-quote "$lang")
                                                              current-lang
                                                              processed-template)))
                            ;; Then handle $input
                            (if (string-match-p (regexp-quote "$input") processed-template)
                                ;; If template contains $input, replace it
                                (replace-regexp-in-string (regexp-quote "$input") user-query processed-template)
                              ;; Otherwise, append user query to template
                              (concat processed-template "\n\nUser question: " user-query)))))
        ;; (message "=== DEBUG: USER-QUERY === '%s'" user-query)
        ;; (message "=== DEBUG: PROMPT-TEMPLATE === '%s'" prompt-template)
        ;; (message "=== DEBUG: BASE-PROMPT === '%s'" base-prompt)
        ;; (message "=== DEBUG: FINAL TEMPLATE === Using template: %s, Template source: %s"
        ;;          (if (eq prompt-template superchat-general-answer-prompt) "GENERAL-ANSWER-PROMPT" "CUSTOM-TEMPLATE")
        ;;          (substring prompt-template 0 (min 100 (length prompt-template))))
        (let* ((conversation-context (superchat--conversation-context-string superchat-context-message-count))
               (sections (delq nil (list lang-instruction memory-context inline-context conversation-context base-prompt)))
               (final-prompt-string (mapconcat #'identity sections "\n\n")))

          ;; Phase 4: Return the final plist.
          (list :prompt final-prompt-string
                :user-message (unless (string-empty-p user-query) user-query))))))))))

;; Helpers to inline file contents into the prompt
(defun superchat--textual-file-p (path)
  "Return non-nil if PATH looks like a textual file we can inline."
  (let* ((ext (downcase (or (file-name-extension path) "")))
         (text-exts '("org" "md" "markdown" "txt")))
    (member ext text-exts)))

(defun superchat--make-inline-context (file-path)
  "Build an inline context block from FILE-PATH if suitable.
Returns a string or nil if the file should not be inlined."
  (when (and (file-exists-p file-path)
             (superchat--textual-file-p file-path))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file-path nil 0 superchat-inline-max-bytes)
          (let* ((raw (buffer-string))
                 (content (string-trim raw))
                 (tpl superchat-inline-context-template)
                 (step1 (replace-regexp-in-string "$path" (or file-path "") tpl))
                 (rendered (replace-regexp-in-string "$content" (or content "") step1)))
            (message "superchat: Inlined %d characters from %s" (length content) file-path)
            rendered))
      (error
       (message "Warning: Failed to inline file %s: %s" file-path (error-message-string err))
       nil))))

(defun superchat--execute-llm-query (input &optional template lang target-model)
  "Build the final prompt from INPUT and return a result plist for the dispatcher."
  (let* ((prompt-data (superchat--build-final-prompt input template lang))
         (final-prompt (plist-get prompt-data :prompt))
         (user-message (plist-get prompt-data :user-message)))
    (append `(:type :llm-query :prompt ,final-prompt)
            (when target-model
              `(:target-model ,target-model))
            (when (and user-message (not (string-empty-p user-message)))
              `(:user-message ,user-message)))))

(defun superchat--handle-command (command args input &optional lang _target-model)
  "Handle all commands and return a result plist describing what to do next."
  ;; Ensure the command is loaded (for prompt files)
  ;; (message "=== DEBUG: HANDLE-COMMAND === Command: %s, Args: %s, Lang: %s" command args lang)
  (superchat--ensure-command-loaded command)

  (pcase command
       ("recall"
        (if (and args (> (length args) 0))
            (let* ((memories (superchat-memory-retrieve args))
                   (count (length memories)))
              (if (> count 0)
                  (progn
                    (setq superchat--retrieved-memory-context
                          (superchat--format-retrieved-memories memories))
                    `(:type :echo :content ,(format "Retrieved %d memories. They will be added to the context of your next query." count)))
                `(:type :echo :content "No memories found.")))
          `(:type :echo :content "Please provide keywords to recall. Usage: /recall <keywords>")))

       ("remember"
        (let ((args (string-trim args)))
          (if (and args (> (length args) 0))
              ;; Tier 1: Explicit memory with arguments
              (let ((title (superchat-memory-compose-title args)))
                (superchat-memory-capture-explicit args title)
                `(:type :echo :content ,(format "Explicit memory added: %s" title)))
            ;; Tier 3: Retroactive memory for the last exchange
            (if-let ((exchange (superchat--last-exchange-struct)))
                (let ((id (superchat-memory-capture-conversation exchange :tier :tier3)))
                  `(:type :echo :content ,(format "Last exchange remembered (ID: %s)." id)))
              `(:type :echo :content "Could not find a recent exchange to remember.")))))


       ("commands"
        `(:type :buffer :content ,(superchat--list-commands-as-string)))

       ("reset"
        (setq superchat--current-command nil)
        `(:type :echo :content "Switched to default chat mode."))

       ("clear-context"
        (superchat--clear-session-context)
        `(:type :echo :content "Session context cleared."))

       ("clear"
        ;; This action is handled directly, no further dispatch needed.
        (superchat--clear-chat-and-context)
        `(:type :noop))

       ("define"
        (if-let ((define-pair (superchat--parse-define input)))
            (progn
              (superchat--define-command (car define-pair) (cdr define-pair))
              `(:type :echo :content ,(format "Command `/%s` defined." (car define-pair))))
          `(:type :buffer :content ,(concat
                                      "Invalid `/define` syntax.\n\n"
                                      "Usage:\n"
                                      "  /define <command-name> \"<prompt-template>\"\n\n"
                                      "Example:\n"
                                      "  /define translate \"Translate to $lang: $input\"\n"
                                      "Available variables:\n"
                                      "  $input - User's input content\n"
                                      "  $lang  - Current language setting (currently: " superchat-lang ")\n\n"
                                      "Notes:\n"
                                      "  • Command name must contain only: a-z A-Z 0-9 _ -\n"
                                      "  • Prompt template must be enclosed in double quotes\n"
                                      "  • Use English double quotes (\"), not Chinese quotes ("")\n"))))

       ;; Default case for other commands
       (_ (cond
           ;; Normal prompt command
           ((or (assoc command superchat--builtin-commands) (gethash command superchat--user-commands))
            (let ((item (or (assoc command superchat--builtin-commands)
                           (gethash command superchat--user-commands))))
              ;; (message "=== DEBUG: COMMAND ITEM === Command: %s, Item: %s" command item)
              (cond
               ;; Built-in commands (functions) - execute immediately
               ((assoc command superchat--builtin-commands)
                ;; (message "=== DEBUG: EXECUTING BUILTIN COMMAND === %s" command)
                (let ((func (cdr item)))
                  (if (fboundp func)
                      (let ((result (funcall func)))
                        ;; If function returns content, display it
                        (if (and result (stringp result))
                            `(:type :buffer :content ,result)
                          `(:type :noop)))
                    `(:type :echo :content ,(format "Command `/%s` function not found." command)))))
               ;; User commands with args - enter mode
               ((and args (> (length args) 0))
                (setq superchat--current-command command)
                `(:type :llm-query-and-mode-switch :args ,args :template ,item :lang ,lang))
               ;; User commands without args - enter mode
               (t
                (setq superchat--current-command command)
                `(:type :echo :content ,(format "Switched to command mode: `/%s`." command))))))
           ;; Unknown command
           (command
            `(:type :echo :content ,(format "Unknown command: `/%s`." command)))
           ;; Not a command
           (t nil)))))

(defun superchat-send-input ()
  "Parse user input, get a result-plist from a handler, and render the result."
  (interactive)
  (let* ((input (superchat--current-input))
         (model-switch-info (superchat--parse-model-switch input))
         (clean-input (if model-switch-info 
                          (car model-switch-info) 
                        input))
         (target-model (if model-switch-info 
                           (cdr model-switch-info) 
                         nil))
         (lang superchat-lang))  ; 动态获取当前语言设置
    (when (and clean-input (> (length clean-input) 0))
      ;; (message "=== SUPERCHAT DEBUG === Processing input: '%s'" clean-input)
      ;; Auto-recall memories for non-command input
      (unless (string-prefix-p "/" clean-input)
        ;; (message "=== SUPERCHAT DEBUG === Not a command, checking memory recall...")
        ;; (message "superchat: checking memory recall for input: '%s'" clean-input)
        ;; (message "superchat: input meets threshold: %s" (superchat--input-meets-memory-threshold-p clean-input))
        (when (superchat--input-meets-memory-threshold-p clean-input)
          ;; (message "superchat: attempting memory retrieval...")
          ;; Try LLM-based retrieval first, fallback to local if LLM not available
          (if (and (featurep 'gptel) (fboundp 'gptel-request))
              (progn
                ;; (message "superchat: using LLM-based keyword extraction for memory retrieval")
                (superchat-memory-retrieve-async
                 clean-input
                 (lambda (memories)
                   ;; (message "superchat: LLM-based memory retrieval returned %d results" (length memories))
                   (when memories
                     ;; (message "superchat: recalled %d memory candidates" (length memories))
                     (setq superchat--retrieved-memory-context
                           (superchat--format-retrieved-memories memories))
                     ;; (message "Retrieved %d memories for context." (length memories))
                     ))))
            ;; Fallback to synchronous local retrieval
            (progn
              ;; (message "superchat: using local keyword extraction for memory retrieval")
              (let ((memories (superchat-memory-retrieve clean-input)))
                ;; (message "superchat: local memory retrieval returned %d results" (length memories))
                (when memories
                  ;; (message "superchat: recalled %d memory candidates" (length memories))
                  (setq superchat--retrieved-memory-context
                        (superchat--format-retrieved-memories memories))
                  ;; (message "Retrieved %d memories for context." (length memories))
                  ))))))

      ;; Finalize the user's input line.
      (let ((inhibit-read-only t)
            (end-of-input (point-max)))
        (put-text-property superchat--prompt-start end-of-input 'read-only t)
        (put-text-property superchat--prompt-start end-of-input 'superchat-role 'user)
        (setq superchat--prompt-start nil))
      ;; Prepare the area for any potential response.
      (superchat--prepare-for-response)

      (let* (;; 新增：优先解析工作流
            (workflow-info (when (fboundp 'superchat-workflow-parse-workflow-input)
                             (superchat-workflow-parse-workflow-input clean-input)))
            (workflow-name (car-safe workflow-info))
            (workflow-args (cdr-safe workflow-info))
            ;; 原有的命令解析
            (cmd-pair (superchat--parse-command clean-input))
            (command (car-safe cmd-pair))
            (args (cdr-safe cmd-pair))
            (result (cond
                     ;; 新增：如果解析到工作流，则处理
                     (workflow-name
                      (when (fboundp 'superchat-workflow-handle-workflow-command)
                        (superchat-workflow-handle-workflow-command workflow-name workflow-args clean-input)))
                     ;; 原有的cond逻辑
                     (command
                      (or (superchat--handle-command command args clean-input lang target-model)
                          (superchat--execute-llm-query clean-input nil lang target-model)))
                     ((and superchat--current-command
                           (not (string-empty-p (string-trim clean-input))))
                      (let ((template (superchat--lookup-command-template superchat--current-command)))
                        (if template
                            (progn
                              ;; (message "=== DEBUG: USING COMMAND TEMPLATE === %s" superchat--current-command)
                              (superchat--execute-llm-query clean-input template lang target-model))
                          (progn
                            (message "Warning: template for command %s not found; falling back to default." superchat--current-command)
                            (superchat--execute-llm-query clean-input nil lang target-model)))))
                     (t
                      (superchat--execute-llm-query clean-input nil lang target-model)))))

          (pcase (plist-get result :type)
          (:buffer
           ;; --- INLINED superchat--insert-system-message ---
           (let ((content (plist-get result :content)))
             ;;(message "ULTIMATE TEST: Matched :buffer. Content length: %d" (length content))
             (with-current-buffer (get-buffer-create superchat-buffer-name)
               (let ((inhibit-read-only t))
                 (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
                   (goto-char superchat--response-start-marker)
                   (delete-region (point) (point-max)))
                 (goto-char (point-max))
                 (insert "\n** System\n")
                 (insert content)
                 (insert "\n")
                 (superchat--insert-prompt)))))
          (:echo
           (message "%s" (plist-get result :content))
           (superchat--refresh-prompt))
          (:noop
           ;; The command has already done everything, including displaying
           ;; messages and refreshing the prompt if needed. Do nothing.
           nil)
          (:llm-query
           (let ((user-message (plist-get result :user-message)))
             (when (and user-message (not (string-empty-p user-message)))
               (superchat--record-message "user" user-message)))
           ;;(message "--- DEBUG PROMPT ---\n%s" (plist-get result :prompt))
           (superchat--update-status "Assistant is thinking...")
           (superchat--llm-generate-answer (plist-get result :prompt)
                                           #'superchat--process-llm-result
                                           #'superchat--stream-llm-result
                                           (plist-get result :target-model)))
          (:llm-query-and-mode-switch
           (superchat--update-status (format "Executing `/%s`..." command))
           (let* ((real-args (plist-get result :args))
                  (template (plist-get result :template))
                  (result-lang (plist-get result :lang))
                  (llm-result (superchat--execute-llm-query real-args template result-lang))
                  (user-message (plist-get llm-result :user-message)))
             (when (and user-message (not (string-empty-p user-message)))
               (superchat--record-message "user" user-message))
            ;;  (message "--- DEBUG PROMPT ---\n%s" (plist-get llm-result :prompt))
             (superchat--llm-generate-answer (plist-get llm-result :prompt)
                                             #'superchat--process-llm-result
                                             #'superchat--stream-llm-result
                                             (plist-get llm-result :target-model)))))))))

;; --- gptel Tools Integration ---

(defun superchat-get-gptel-tools ()
  "Get user's configured gptel tools."
  (when (boundp 'gptel-tools)
    gptel-tools))

(defun superchat-gptel-tools-enabled-p ()
  "Check if user has enabled gptel tools."
  (when (boundp 'gptel-use-tools)
    gptel-use-tools))

(defun superchat-tools-status ()
  "Display current gptel tools status and timeout protection."
  (interactive)
  (let ((enabled (superchat-gptel-tools-enabled-p))
        (tools (superchat-get-gptel-tools))
        (timeout superchat-response-timeout))
    (let ((content
           (concat
            (format "gptel Tools Status\n\n")
            (format "Tools enabled: %s\n" (if enabled "Yes" "No"))
            (format "Tools count: %d\n" (length tools))
            (format "Response timeout: %s\n\n" 
                    (if timeout 
                        (format "%d seconds (prevents UI freezing)" timeout)
                      "Disabled (not recommended)"))
            (when (and enabled tools)
              (concat "Available tools:\n"
                      (mapconcat (lambda (tool)
                                   (format "  • %s: %s" 
                                           (gptel-tool-name tool)
                                           (gptel-tool-description tool)))
                                 tools "\n")
                      "\n\n"))
            (when enabled
              (concat "Timeout Protection:\n"
                      "  If a tool blocks for more than " (number-to-string timeout) " seconds,\n"
                      "  the UI will automatically recover and show a timeout message.\n"
                      "  This prevents freezing from synchronous/blocking tools.\n\n"
                      "To adjust timeout: (setq superchat-response-timeout SECONDS)\n")))))
      ;; For interactive use, show help window
      (when (called-interactively-p 'interactive)
        (with-help-window "*SuperChat Tools Status*"
          (with-current-buffer standard-output
            (insert content))))
      ;; Return content for display in chat
      content)))

;; --- MCP Integration ---

(defun superchat-mcp-available-p ()
  "Check if MCP (Model Context Protocol) is available."
  (and (featurep 'mcp-hub)
       (boundp 'mcp-hub-servers)
       mcp-hub-servers))

(defun superchat-mcp-servers-running-p ()
  "Check if any MCP servers are running."
  (and (superchat-mcp-available-p)
       (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (> (hash-table-count mcp-server-connections) 0)))

(defun superchat-mcp-get-server-count ()
  "Get number of configured MCP servers."
  (if (superchat-mcp-available-p)
      (length mcp-hub-servers)
    0))

(defun superchat-mcp-get-running-server-count ()
  "Get number of running MCP servers."
  (if (superchat-mcp-servers-running-p)
      (hash-table-count mcp-server-connections)
    0))

(defun superchat-mcp-start-servers (&optional callback)
  "Start MCP servers if available.
CALLBACK is called when servers are started."
  (interactive)
  (if (not (superchat-mcp-available-p))
      (message "MCP not available. Please install and configure mcp.el package")
    (if (zerop (superchat-mcp-get-server-count))
        (message "No MCP servers configured. Please set `mcp-hub-servers'")
      (message "Starting %d MCP server(s)..." (superchat-mcp-get-server-count))
      (condition-case err
          (mcp-hub-start-all-server callback nil t)
        (error
         (message "Failed to start MCP servers: %s" (error-message-string err)))))))

(defun superchat-mcp-get-tools ()
  "Get MCP tools if available."
  (when (and (superchat-mcp-servers-running-p)
             (fboundp 'mcp-hub-get-all-tool))
    ;; Call with 3 positional args: asyncp categoryp errorHandle
    (mcp-hub-get-all-tool nil t nil)))

(defun superchat-mcp-status ()
  "Display MCP status and available tools."
  (interactive)
  (let ((mcp-available (superchat-mcp-available-p))
        (servers-configured (superchat-mcp-get-server-count))
        (servers-running (superchat-mcp-get-running-server-count))
        (mcp-tools (superchat-mcp-get-tools)))
    (let ((content
           (concat
            (format "SuperChat MCP (Model Context Protocol) Status\n\n")
            (format "Available: %s\n" (if mcp-available "Yes" "No"))
            (format "Servers configured: %d\n" servers-configured)
            (format "Servers running: %d\n\n" servers-running)
            
            (when mcp-available
              (concat
               (if (zerop servers-configured)
                   "No MCP servers configured.\n\n"
                 (concat
                  "Configured servers:\n"
                  (mapconcat (lambda (server)
                               (format "  • %s" (car server)))
                             mcp-hub-servers "\n")
                  "\n"))
               
               (when (and (> servers-running 0) mcp-tools)
                 (concat
                  (format "MCP Tools available: %d\n" (length mcp-tools))
                  (mapconcat (lambda (tool)
                               (format "  • %s: %s" 
                                       (plist-get tool :name)
                                       (or (plist-get tool :description) "No description")))
                             mcp-tools "\n")
                  "\n\n"
                  "Usage: Tools are automatically integrated with gptel.\n"
                  "MCP tools appear with 'mcp-' prefix in gptel's tool system.\n")))))))
      
      ;; For interactive use, show help window
      (when (called-interactively-p 'interactive)
        (with-help-window "*SuperChat MCP Status*"
          (with-current-buffer standard-output
            (insert content))))
      ;; Return content for display in chat
      content)))

;; --- LLM Backend (Extracted from supertag-rag.el) ---

(defun superchat--llm-generate-answer-sync (prompt &optional target-model)
  "Generate an answer using gptel synchronously and return the result.
This is a blocking call intended for internal systems like workflows.
This version SUPPORTS gptel tools, allowing workflows to use all available tools."
  (message "🤖 Synchronously generating answer with model %s%s..." 
           (or target-model gptel-model "default")
           (if (and (boundp 'gptel-use-tools) gptel-use-tools 
                    (boundp 'gptel-tools) gptel-tools)
               (format " (tools: %d)" (length gptel-tools))
             ""))
  (let ((original-model (when (boundp 'gptel-model) gptel-model))
        (response-parts '())
        (completed nil)
        (final-response nil))
    (unwind-protect
        (progn
          (when target-model
            (setq gptel-model target-model))

          ;; Use a promise-like approach to wait for completion
          (let ((response-finished nil))
            ;; Make the request and collect response synchronously
            ;; NOTE: We DON'T explicitly pass :tools parameter here because
            ;; gptel-request will automatically use the global gptel-use-tools
            ;; and gptel-tools variables if they are set
            (gptel-request prompt
                           :stream nil  ; Disable streaming for synchronous operation
                           :callback (lambda (response &rest _)
                                       (setq final-response response
                                             completed t
                                             response-finished t)))

            ;; Wait for completion with extended timeout for slower models
            ;; Tool calls can take longer, so we use a generous timeout
            (let ((timeout-count 0)
                  (max-timeout 180)) ; 180 second timeout to accommodate tool calls
              (while (and (not response-finished) (< timeout-count max-timeout))
                (sleep-for 0.1) ; Sleep for 100ms
                (cl-incf timeout-count)
                ;; Provide progress feedback every 10 seconds
                (when (= 0 (mod timeout-count 100))
                  (message "🤖 Still waiting for LLM response... %ds/%ds"
                           (* timeout-count 0.1) max-timeout)))

              (unless response-finished
                (message "⚠️ Synchronous LLM request timed out after %d seconds" max-timeout)
                (setq final-response (format "[ERROR: Request timeout after %ds - model may be too slow or offline]" max-timeout))))))

      ;; Cleanup: restore original model
      (when (and original-model (boundp 'gptel-model))
        (setq gptel-model original-model)))

    (message "✅ Synchronous generation complete.")
    final-response))

(defun superchat--llm-generate-answer (prompt callback stream-callback &optional target-model)
  "Generate an answer using gptel, correctly handling its streaming callback.
Optionally use TARGET-MODEL for this request only.

Includes timeout protection to prevent UI freezing from blocking tools.
Also includes smart completion detection for non-streaming responses (e.g. Ollama + tools)."
  (let ((response-parts '())
        (original-model (when (boundp 'gptel-model) gptel-model))
        (completed nil)
        (timeout-timer nil)  ; Ultimate safety net (30s default)
        (completion-check-timer nil)  ; Smart completion detection (5s)
        (prompt-copy prompt)) ; Used in gptel-request below
    
    ;; Temporarily set model if target-model is provided
    (when (and target-model (boundp 'gptel-model))
      (setq gptel-model target-model)
      (message "Switching to model: %s" target-model))
    
    ;; Set up timeout protection if configured (safety net)
    ;; This timer only triggers if BOTH conditions are met:
    ;; 1. No completion signal received (completed = nil)
    ;; 2. No data received at all (response-parts is empty)
    ;; If data was received, it means LLM is working and we should NOT show timeout error
    ;;
    ;; DYNAMIC TIMEOUT: The timer can be extended when user confirms tool actions
    (when superchat-response-timeout
      (setq timeout-timer
            (run-with-timer
             superchat-response-timeout nil
             (lambda ()
               (unless completed
                 (setq completed t)
                 ;; Cancel completion check timer if still active
                 (when completion-check-timer
                   (cancel-timer completion-check-timer))
                 ;; Clear the active timer reference
                 (with-current-buffer (get-buffer-create superchat-buffer-name)
                   (setq superchat--active-timeout-timer nil))
                 ;; Check if we received ANY response data
                 (if response-parts
                     ;; Data received - LLM is working, just didn't send completion signal
                     ;; This is NORMAL for long responses, treat as successful completion
                     (progn
                       (message "✅ Response completed after timeout (LLM still generating, treating as complete)")
                       ;; Restore model
                       (when (and original-model (boundp 'gptel-model))
                         (setq gptel-model original-model))
                       ;; Treat partial response as complete
                       (when callback
                         (let ((final-response (string-join (nreverse response-parts) "")))
                           (funcall callback final-response))))
                   ;; No data received at all - actual timeout/error
                   (message "⚠️  Response timeout. No data received - model may not support tools or is offline!")
                   ;; Restore model
                   (when (and original-model (boundp 'gptel-model))
                     (setq gptel-model original-model))
                  ;; Force UI recovery by calling callback with timeout message
                  (when callback
                    (let ((timeout-msg
                           (format "[Response timeout. Model (%s) may not support tools. Try: (setq gptel-use-tools nil)]"
                                   (or (boundp 'gptel-model) gptel-model "unknown"))))
                      (funcall callback timeout-msg))))))))
      ;; Store timer reference in buffer-local variable for dynamic extension
      (with-current-buffer (get-buffer-create superchat-buffer-name)
        (setq superchat--active-timeout-timer timeout-timer)))

   ;; Collect all available tools and wrap the request in a `let` binding
   ;; to temporarily set the gptel global variables.
   (let* ((gptel-tools-list (when (fboundp 'superchat-get-gptel-tools)
                              (superchat-get-gptel-tools)))
          (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                       (superchat-mcp-get-tools)))
          (all-tools (append gptel-tools-list mcp-tools)))
     ;; These global variables are what gptel-request actually uses.
     (setq gptel-use-tools (and all-tools t)
           gptel-tools all-tools)
     (gptel-request prompt-copy
                    :stream t
                    :callback (lambda (response-or-signal &rest _)
                                 "Handle streaming chunks, non-streaming responses, and completion signals."
                                 ;; (message "DEBUG: gptel callback received: %S" response-or-signal)
                                 (cond
                                  ;; Case 1: String response (streaming chunk or non-streaming full response)
                                  ((stringp response-or-signal)
                                   ;; (message "DEBUG: Processing chunk, length: %d" (length response-or-signal))
                                   (push response-or-signal response-parts)
                                   (when stream-callback
                                     (funcall stream-callback response-or-signal))
                                   ;; Smart completion detection: reset timer on new data
                                   ;; If no new data arrives within configured delay, assume response is complete
                                   (when completion-check-timer
                                     (cancel-timer completion-check-timer))
                                   (setq completion-check-timer
                                         (run-with-timer superchat-completion-check-delay nil
                                          (lambda ()
                                            (unless completed
                                              ;; (message "DEBUG: No new data for %ds, treating as complete" 
                                              ;;          superchat-completion-check-delay)
                                              (setq completed t)
                                               ;; Cancel timeout timer
                                               (when timeout-timer
                                                 (cancel-timer timeout-timer))
                                               ;; Clear the active timer reference
                                               (with-current-buffer (get-buffer-create superchat-buffer-name)
                                                 (setq superchat--active-timeout-timer nil))
                                               ;; Restore original model
                                               (when (and original-model (boundp 'gptel-model))
                                                 (setq gptel-model original-model))
                                               ;; Call completion callback
                                               (when callback
                                                 (let ((final-response (string-join (nreverse response-parts) "")))
                                                   ;; (message "DEBUG: Calling callback with final response, length: %d"
                                                   ;;          (length final-response))
                                                   (funcall callback final-response))))))))
                                  
                                  ;; Case 2: Completion signal 't' (normal streaming completion)
                                  ((eq response-or-signal t)
                                   (unless completed
                                     ;; (message "DEBUG: Received final t signal, completing response")
                                     (setq completed t)
                                     ;; Cancel all timers
                                     (when timeout-timer (cancel-timer timeout-timer))
                                     (when completion-check-timer (cancel-timer completion-check-timer))
                                     ;; Clear the active timer reference
                                     (with-current-buffer (get-buffer-create superchat-buffer-name)
                                       (setq superchat--active-timeout-timer nil))
                                     ;; Restore original model
                                     (when (and original-model (boundp 'gptel-model))
                                       (setq gptel-model original-model))
                                     ;; Call completion callback
                                     (when callback
                                       (let ((final-response (string-join (nreverse response-parts) "")))
                                         ;; (message "DEBUG: Calling callback with final response, length: %d"
                                         ;;          (length final-response))
                                         (funcall callback final-response))))))))

    ;; Restore original model in case of early termination
    (when (and original-model (boundp 'gptel-model))
      (setq gptel-model original-model)))))

;; --- Save Conversation ---
(defun superchat--format-conversation (conversation)
  "Format conversation for org-mode display."
  (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun superchat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (when (and conversation title (stringp conversation) (stringp title))
    (superchat--ensure-directories)
    (let ((save-dir (superchat--save-directory))
          (filename (expand-file-name (concat title ".org") (superchat--save-directory))))
      (make-directory save-dir t)
      (with-current-buffer (find-file-noselect filename)
        (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n" 
                        title (format-time-string "%Y-%m-%d")))
        (insert "* AI Conversation\n\n")
        (insert (superchat--format-conversation conversation))
        (save-buffer)
        ;;(message "Conversation saved to: %s" filename)
        ))))

(defun superchat--save-append-to-node (conversation)
  "Append conversation to current org headline."
  (when (and conversation (stringp conversation) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n\n* AI Assistant Conversation\n")
      (insert (format "#+CAPTION: Generated %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert (superchat--format-conversation conversation))
      ;;(message "Conversation appended to current node")
      )))

(defun superchat--save-as-subnode (conversation title)
  "Create new subnode under current headline."
  (when (and conversation title (stringp conversation) (stringp title) (org-at-heading-p))
    (save-excursion
      (org-end-of-subtree t)
      (insert "\n")
      (org-insert-heading)
      (insert title)
      (org-set-property "DATE" (format-time-string "%Y-%m-%d"))
      (org-set-property "TAGS" "ai-conversation")
      (insert "\n")
      (insert (superchat--format-conversation conversation))
      ;;(message "Conversation saved as subnode: %s" title)
      )))

(defun superchat--save-conversation ()
  "Save Chat View conversation with user choice of method."
  (interactive)
  (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
    (when (and conversation (stringp conversation))
      (let* ((title (read-string "Conversation title: " (format "Chat-%s" (format-time-string "%Y%m%d"))))
             (choice (completing-read "Save conversation as: "
                                      '("new file" "append to current node" "create new subnode")
                                      nil t)))
        (pcase choice
          ("new file" (superchat--save-as-new-file conversation title))
          ("append to current node" (superchat--save-append-to-node conversation))
          ("create new subnode" (superchat--save-as-subnode conversation title)))))))

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

(defun superchat--insert-system-message (content)
  "Insert a system message into the chat buffer and refresh the prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (propertize (format "* System: %s" content) 'face 'italic))
      (insert "\n")
      (superchat--refresh-prompt))))

(defun superchat--refresh-prompt ()
  "Clear any status message and insert a fresh prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (point-max)))
      (goto-char (point-max))
      (superchat--insert-prompt))))

;; --- Context Management ---
(defvar superchat--current-context-files nil
  "List of files currently added to gptel context for this session.")

(defun superchat--add-file-to-context (file-path)
  "Add FILE-PATH to gptel context and track it in our session."
  (message "superchat: Attempting to add file to context: %s" file-path)
  (when (and file-path (file-exists-p file-path))
    (condition-case err
        (progn
          (gptel-context-add-file file-path)
          (cl-pushnew file-path superchat--current-context-files :test #'equal)
          (message "superchat: File %s added to gptel context" file-path))
      (error
       (message "Warning: Error adding file to gptel context: %s" (error-message-string err))))))

(defun superchat--clear-session-context ()
  "Clear all files added to context during this session."
  (interactive)
  (when superchat--current-context-files
    ;; Remove each file from gptel context
    (dolist (file superchat--current-context-files)
      (gptel-context-remove file))
    ;; Clear our tracking list
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
  ;; Initialize workflow system with dependency injection
  (when (fboundp 'superchat-workflow-initialize)
    (superchat-workflow-initialize :llm-executor 'superchat--llm-generate-answer-sync))
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
        (add-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit nil t))
    ;; When turning the mode off
    (kill-local-variable 'completion-at-point-functions)
    (remove-hook 'kill-buffer-hook #'superchat--summarize-session-on-exit t)))

(add-hook 'kill-emacs-hook #'superchat--summarize-session-before-emacs-exit)

(provide 'superchat)

;;;###autoload
(defun superchat-ensure-directories ()
  "Ensure that the necessary directories exist."
  (interactive)
  (superchat--ensure-directories)
  (message "Superchat directories ensured to exist."))

;;; superchat.el ends here
