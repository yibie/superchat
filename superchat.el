;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'gptel-context)

(defgroup superchat nil
  "Configuration for Superchat, a standalone AI chat client."
  :group 'external)

(defcustom superchat-buffer-name "*Superchat*"
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

(defcustom superchat-save-directory
  (expand-file-name "chat-notes/" superchat-data-directory)
  "Directory for saving Chat View conversations."
  :type 'directory
  :group 'superchat)

(defcustom superchat-model nil
  "The model to use for chat queries. If nil, gptel's default is used."
  :type '(or nil string)
  :group 'superchat)

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
   '("You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: $input"
     "Format your answer using Markdown.")
   "\n")
  "The prompt template used for general questions."
  :type 'string
  :group 'superchat)

(defcustom superchat-display-single-window t
  "If non-nil, make the Superchat window the only one in its frame."
  :type 'boolean
  :group 'superchat)

(defcustom superchat-context-message-count 5
  "Number of most recent conversation messages to include in prompts.
Set to 0 to disable including prior chat messages."
  :type 'integer
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

(defun superchat--recent-conversation-messages (limit)
  "Return up to LIMIT most recent messages ordered from oldest to newest."
  (let* ((limit (or limit 0))
         (count 0)
         (history superchat--conversation-history)
         (result '()))
    (while (and history (< count limit))
      (push (car history) result)
      (setq history (cdr history))
      (setq count (1+ count)))
    (nreverse result)))

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
  "Build a conversation context string for the last LIMIT messages."
  (let ((messages (superchat--recent-conversation-messages limit)))
    (when messages
      (let ((lines (delq nil (mapcar #'superchat--format-conversation-message messages))))
        (when lines
          (concat "Previous conversation:\n"
                  (mapconcat #'identity lines "\n")))))))

;; --- Command System Variables ---
(defvar superchat--user-commands (make-hash-table :test 'equal)
  "Store all user-defined chat commands, command name -> prompt content.")

(defcustom superchat-command-dir
  (expand-file-name "command/" superchat-data-directory)
  "The single directory for storing all custom command prompt files."
  :type 'directory
  :group 'superchat)

(defconst superchat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input in $lang."))
  "Alist of built-in commands and their prompts. Use $input and $lang for variables.")

(defun superchat--get-prompt-file-path (prompt-name)
  "Get the full path for a prompt file by PROMPT-NAME.
Supports multiple file extensions defined in `superchat-prompt-file-extensions`."
  (when (and prompt-name superchat-command-dir)
    (cl-find-if #'file-exists-p
                (mapcar (lambda (ext)
                          (expand-file-name (concat prompt-name "." ext) superchat-command-dir))
                        superchat-prompt-file-extensions))))

(defun superchat--load-prompt-from-file (prompt-name)
  "Load prompt content from a file by PROMPT-NAME.
Supports multiple file extensions defined in `superchat-prompt-file-extensions`."
  (let ((prompt-file-path (superchat--get-prompt-file-path prompt-name)))
    (when (and prompt-file-path (file-exists-p prompt-file-path))
      (with-temp-buffer
        (insert-file-contents prompt-file-path)
        (buffer-string)))))

(defun superchat--list-available-prompts ()
  "List all available prompt files in the prompt directory."
  (when (file-directory-p superchat-command-dir)
    (let ((prompt-files '()))
      (dolist (file (directory-files superchat-command-dir t))
        (when (and (file-regular-p file)
                   (member (file-name-extension file) superchat-prompt-file-extensions))
          (push (file-name-base file) prompt-files)))
      (sort prompt-files 'string<))))

;; --- Core Functions ---
(defun superchat--md-to-org (md-string)
  "Convert a markdown string to an org-mode formatted string."
  (if (and md-string (not (string-empty-p md-string)))
      (with-temp-buffer
        (insert md-string)
        (goto-char (point-min))
        ;; Headers: # title -> * title, ## title -> ** title
        (while (re-search-forward "^\\(#+\\)\\s-+" nil t)
          (replace-match (concat (make-string (length (match-string 1)) ?*) " ")))
        ;; Bold: **bold** -> *bold*
        (goto-char (point-min))
        (while (re-search-forward "\\*\\*\\([^ \n*][^*]*[^ \n*]\\)\\*\\*" nil t)
          (replace-match (concat "*" (match-string 1) "*")))
        ;; Italics: *italic* or _italic_ -> /italic/
        (goto-char (point-min))
        (while (re-search-forward "\\(?:\\*\\|_\\)\\([^ \n*_][^*_]*[^ \n*_]\\)\\(?:\\*\\|_\\)" nil t)
          (replace-match (concat "/" (match-string 1) "/")))
        ;; Inline code: `code` -> =code=
        (goto-char (point-min))
        (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
          (replace-match (concat "=" (match-string 1) "=")))
        ;; Code blocks:
        (goto-char (point-min))
        (while (re-search-forward "^```\\([a-zA-Z0-9]+\\)?\n\\([\\s\\S]*?\\)\n```" nil t)
          (let ((lang (or (match-string 1) ""))
                (code (match-string 2)))
            (replace-match (concat "#+BEGIN_SRC " lang "\n" code "\n#+END_SRC"))))
        ;; Unordered lists: * item -> - item
        (goto-char (point-min))
        (while (re-search-forward "^\\s-*\\* " nil t)
          (replace-match "- "))
        ;; Links: [text](url) -> [[url][text]]
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([^\\]]+\\)\\](\\([^)]+\\))" nil t)
          (replace-match (concat "[[" (match-string 2) "][" (match-string 1) "]]")))
        ;; Horizontal rules: --- -> -----
        (goto-char (point-min))
        (while (re-search-forward "^---\\s-*$" nil t)
          (replace-match "-----"))
        (buffer-string))
    ""))

(defun superchat--insert-prompt ()
  "Insert an org headline as the input prompt at the end of the buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (let ((headline
             (if superchat--current-command
                 (format "* User [%s mode]: " superchat--current-command)
               "\n* User: ")))
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
      (superchat--prepare-assistant-response-area)
      (goto-char (point-max))
      (insert (superchat--md-to-org chunk)))))

(defun superchat--process-llm-result (answer)
  "Finalize the LLM response processing.
This function is called ONCE after the entire response has been streamed."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t)
          (response-content (if (and answer (not (string-empty-p answer)))
                                answer
                              "Assistant did not provide a response.")))
      ;; If streaming failed, prepare the area and insert the whole response.
      (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
        (superchat--prepare-assistant-response-area)
        (insert (superchat--md-to-org response-content)))

      ;; Apply the text property for the assistant's response
      (when (and superchat--assistant-response-start-marker (marker-position superchat--assistant-response-start-marker))
        (put-text-property superchat--assistant-response-start-marker (point-max) 'superchat-role 'assistant)
        (setq superchat--assistant-response-start-marker nil))

      ;; 1. Update conversation history with the full response
      (superchat--record-message "assistant" response-content)
      
      ;; 2. Insert a blank line for spacing before the next prompt
      (insert "\n")
      (superchat--insert-prompt))))

(defun superchat--current-input ()
  "Return current prompt line user text."
  (when (and superchat--prompt-start (marker-position superchat--prompt-start))
    (let ((input-text (buffer-substring-no-properties superchat--prompt-start (point-max))))
      (string-trim input-text))))

;; --- Command System ---

(defun superchat--load-user-commands ()
  "Load all custom command prompt files from `superchat-command-dir`."
  (clrhash superchat--user-commands)
  (when (file-directory-p superchat-command-dir)
    (dolist (file (directory-files superchat-command-dir t))
      (when (and (file-regular-p file)
                 (member (file-name-extension file) superchat-prompt-file-extensions))
        (let ((name (file-name-base file)))
          (with-temp-buffer
            (insert-file-contents file)
            (puthash name (buffer-string) superchat--user-commands)))))))

(defun superchat--ensure-command-loaded (command)
  "Ensure COMMAND is loaded in the user commands hash table.
If not found, try to load it from a prompt file."
  (unless (or (assoc command superchat--builtin-commands)
              (gethash command superchat--user-commands))
    ;; Try to load from a prompt file
    (let ((prompt-content (superchat--load-prompt-from-file command)))
      (when prompt-content
        (puthash command prompt-content superchat--user-commands)))))

(defun superchat--define-command (name prompt)
  "Define a new command, persist the prompt to a file and load it."
  (unless (file-directory-p superchat-command-dir)
    (make-directory superchat-command-dir t))
  (let ((file (expand-file-name (concat name ".prompt") superchat-command-dir)))
    (with-temp-buffer
      (insert prompt)
      (write-file file)))
  (puthash name prompt superchat--user-commands)
  (message "Defined command /%s" name))

(defun superchat--list-commands-as-string ()
  "Return all available commands as a formatted string."
  (with-temp-buffer
    (insert "Available Commands:\n\n"
            "- `/define <name> \"<prompt>`: Define a new command.\n"
            "- `/commands`: Show this list.\n"
            "- `/reset`: Reset to default chat mode.\n"
            "- `/clear-context`: Clear all files from current session context.\n\n"
            "**Built-in Prompts:**\n")
    (dolist (cmd superchat--builtin-commands)
      (insert (format "- `/%s`\n" (car cmd))))
    (when (> (hash-table-count superchat--user-commands) 0)
      (insert "\n**User-defined Prompts:**\n")
      (let ((cmds (sort (hash-table-keys superchat--user-commands) #'string<)))
        (dolist (cmd cmds)
          (insert (format "- `/%s`\n" cmd)))))
    (buffer-string)))

(defun superchat--get-all-command-names ()
  "Return a list of all available command names, without the leading slash."
  (let ((cmds '("define" "commands" "reset" "clear-context"))) ; Meta commands
    (dolist (cmd superchat--builtin-commands)
      (push (car cmd) cmds))
    (maphash (lambda (k _v) (push k cmds))
             superchat--user-commands)
    ;; Add available prompt files
    (let ((available-prompts (superchat--list-available-prompts)))
      (dolist (prompt available-prompts)
        (push prompt cmds)))
    (sort (delete-dups (append cmds '("reset"))) 'string<)))

(defun superchat--replace-prompt-variables (prompt input &optional lang)
  "Replace $input and $lang variables in PROMPT with INPUT and LANG."
  (let ((result prompt))
    (setq result (replace-regexp-in-string "\\$input" (or input "") result))
    (when lang
      (setq result (replace-regexp-in-string "\\$lang" lang result)))
    result))

(defun superchat--completion-at-point ()
  "Provide completion for /commands at point for `completion-at-point-functions`."
  (let* ((prompt-start-pos (and superchat--prompt-start (marker-position superchat--prompt-start)))
         (p (point)))
    (when (and prompt-start-pos (>= p prompt-start-pos))
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds)))
        (when (and start (string-prefix-p "/" (buffer-substring-no-properties start end)))
          ;; The completion target starts *after* the trigger character '/'.
          (list (1+ start) end (superchat--get-all-command-names)))))))

(defun superchat--parse-define (input)
  "Parse /define command input."
  (when (and input (stringp input))
    (cond
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-+\"\\(.*\\)\"$" input)
      (cons (match-string 1 input) (or (match-string 2 input) "")))
     ((string-match "^/define\\s-+\\([a-zA-Z0-9_-]+\\)\\s-*$" input)
      (cons (match-string 1 input) ""))
     (t nil))))

(defun superchat--parse-command (input)
  "Parse command input, return (command . args) or nil."
  (when (and input (stringp input))
    (if (string-match "^/\\([a-zA-Z0-9_-]+\\)\\(\\s-+.*\\)?" input)
        (cons (or (match-string 1 input) "")
              (string-trim (or (match-string 2 input) "")))
      nil)))

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
  (let (file-path)
    (when (and input (string-match superchat--file-ref-regexp input))
      (setq file-path (or (match-string 1 input)
                          (match-string 2 input)))
      (setq file-path (superchat--normalize-file-path file-path))
      ;; --- DIAGNOSTIC MESSAGE ---
      (message "Superchat: Extracted file path: %s" file-path)
      (message "Superchat: File exists: %s" (file-exists-p file-path))
      ;; --- END DIAGNOSTIC ---
      file-path)))

;; --- Main Send Logic ---
(defun superchat--build-final-prompt (input &optional template)
  "Build the final prompt string, adding file and conversation context.
This function is rewritten to use a functional data flow, avoiding
in-place modification of variables to prevent subtle environment bugs.
Returns a plist containing :prompt and :user-message values."
  (message "Superchat: Building final prompt with input: '%s'" input)

  ;; Phase 1: Parse input to separate user query from file path.
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

    (message "--- BUILD-PROMPT --- File path parsed as: '%s'" file-path)
    (message "--- BUILD-PROMPT --- Final user query: '%s'" user-query)

    ;; Phase 2: Build context from the parsed file path.
    (let ((inline-context
           (when file-path 
             (superchat--add-file-to-context file-path)
             (when superchat-inline-file-content
               (superchat--make-inline-context file-path)))))

      ;; Phase 3: Construct the final prompt string.
      (let* ((prompt-template
              (or template superchat-general-answer-prompt))
             (base-prompt (replace-regexp-in-string
                           (regexp-quote "$input")
                           user-query
                           prompt-template))
             (conversation-context (superchat--conversation-context-string superchat-context-message-count))
             (sections (delq nil (list inline-context conversation-context base-prompt))
             (final-prompt-string (mapconcat #'identity sections "\n\n")))

        ;; Phase 4: Return the final plist.
        (list :prompt final-prompt-string
              :user-message (unless (string-empty-p user-query) user-query))))))

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
            (message "Superchat: Inlined %d characters from %s" (length content) file-path)
            rendered))
      (error
       (message "Warning: Failed to inline file %s: %s" file-path (error-message-string err))
       nil))))

(defun superchat--execute-llm-query (input &optional template)
  "Build the final prompt from INPUT and return a result plist for the dispatcher."
  (let* ((prompt-data (superchat--build-final-prompt input template))
         (final-prompt (plist-get prompt-data :prompt))
         (user-message (plist-get prompt-data :user-message)))
    (append `(:type :llm-query :prompt ,final-prompt)
            (when (and user-message (not (string-empty-p user-message)))
              `(:user-message ,user-message)))))

(defun superchat--handle-command (command args input)
  "Handle all commands and return a result plist describing what to do next."
  ;; Ensure the command is loaded (for prompt files)
  (superchat--ensure-command-loaded command)

  (pcase command
       ("commands"
        `(:type :buffer :content ,(superchat--list-commands-as-string)))

       ("reset"
        (setq superchat--current-command nil)
        `(:type :echo :content "Switched to default chat mode."))

       ("clear-context"
        (superchat--clear-session-context)
        `(:type :echo :content "Session context cleared."))

       ("define"
        (if-let ((define-pair (superchat--parse-define input)))
            (progn
              (superchat--define-command (car define-pair) (cdr define-pair))
              `(:type :echo :content ,(format "Command `/%s` defined." (car define-pair))))
          `(:type :echo :content "Invalid `/define` syntax.")))

       ;; Default case for other commands
       (_ (cond
           ;; Normal prompt command
           ((or (assoc command superchat--builtin-commands) (gethash command superchat--user-commands))
            (let ((template (or (cdr (assoc command superchat--builtin-commands))
                                (gethash command superchat--user-commands))))
              (setq superchat--current-command command) ; Keep for UI prompt display
              (if (and args (> (length args) 0))
                  `(:type :llm-query-and-mode-switch :args ,args :template ,template)
                `(:type :echo :content ,(format "Switched to command mode: `/%s`." command)))))
           ;; Unknown command
           (command
            `(:type :echo :content ,(format "Unknown command: `/%s`." command)))
           ;; Not a command
           (t nil)))))

(defun superchat-send-input ()
  "Parse user input, get a result-plist from a handler, and render the result."
  (interactive)
  (let ((input (superchat--current-input)))
    (when (and input (> (length input) 0))
      ;; Finalize the user's input line.
      (let ((inhibit-read-only t)
            (end-of-input (point-max)))
        (put-text-property superchat--prompt-start end-of-input 'read-only t)
        (put-text-property superchat--prompt-start end-of-input 'superchat-role 'user)
        (setq superchat--prompt-start nil))
      ;; Prepare the area for any potential response.
      (superchat--prepare-for-response)

      (let* ((cmd-pair (superchat--parse-command input))
             (command (car-safe cmd-pair))
             (args (cdr-safe cmd-pair))
             (result (or (and command (superchat--handle-command command args input))
                         (superchat--execute-llm-query input))))

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
          (:llm-query
           (let ((user-message (plist-get result :user-message)))
             (when (and user-message (not (string-empty-p user-message)))
               (superchat--record-message "user" user-message)))
           (superchat--update-status "Assistant is thinking...")
           (superchat--llm-generate-answer (plist-get result :prompt)
                                           #'superchat--process-llm-result
                                           #'superchat--stream-llm-result))
          (:llm-query-and-mode-switch
           (superchat--update-status (format "Executing `/%s`..." command))
           (let* ((real-args (plist-get result :args))
                  (template (plist-get result :template))
                  (llm-result (superchat--execute-llm-query real-args template))
                  (user-message (plist-get llm-result :user-message)))
             (when (and user-message (not (string-empty-p user-message)))
               (superchat--record-message "user" user-message))
             (superchat--llm-generate-answer (plist-get llm-result :prompt)
                                             #'superchat--process-llm-result
                                             #'superchat--stream-llm-result))))))))

;; --- LLM Backend (Extracted from supertag-rag.el) ---

(defun superchat--llm-generate-answer (prompt callback stream-callback)
  "Generate an answer using gptel, correctly handling its streaming callback."
  (let ((response-parts '()))
    (apply #'gptel-request
           prompt ; Pass the prompt string directly as the first argument
           (append `(:stream t
                     :callback ,(lambda (response-or-signal &rest _)
                                 "Handle both string chunks from the stream and the final `t` signal from the sentinel."
                                 (if (stringp response-or-signal)
                                     ;; Case 1: It's a string chunk from the stream filter.
                                     (progn
                                       (when stream-callback (funcall stream-callback response-or-signal))
                                       (push response-or-signal response-parts))
                                   ;; Case 2: It's the final signal (t) from the stream sentinel.
                                   (when (eq response-or-signal t)
                                     (when callback
                                       (let ((final-response (string-join (nreverse response-parts) "")))
                                         (funcall callback final-response)))))))
                   (if superchat-model `(:model ,superchat-model) nil)))))

;; --- Save Conversation ---
(defun superchat--format-conversation (conversation)
  "Format conversation for org-mode display."
  (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
    (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

(defun superchat--save-as-new-file (conversation title)
  "Save conversation as new org file."
  (when (and conversation title (stringp conversation) (stringp title))
    (let ((filename (expand-file-name (concat title ".org") superchat-save-directory)))
      (make-directory superchat-save-directory t)
      (with-current-buffer (find-file-noselect filename)
        (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n" 
                        title (format-time-string "%Y-%m-%d")))
        (insert "* AI Conversation\n\n")
        (insert (superchat--format-conversation conversation))
        (save-buffer)
        ;;(message "Conversation saved to: %s" filename)
        ))))

;; (defun superchat--save-append-to-node (conversation)
;;   "Append conversation to current org headline."
;;   (when (and conversation (stringp conversation) (org-at-heading-p))
;;     (save-excursion
;;       (org-end-of-subtree t)
;;       (insert "\n\n* AI Assistant Conversation\n")
;;       (insert (format "#+CAPTION: Generated %s\n" (format-time-string "%Y-%m-%d %H:%M")))
;;       (insert (superchat--format-conversation conversation))
;;       ;;(message "Conversation appended to current node")
;;       )))

;; (defun superchat--save-as-subnode (conversation title)
;;   "Create new subnode under current headline."
;;   (when (and conversation title (stringp conversation) (stringp title) (org-at-heading-p))
;;     (save-excursion
;;       (org-end-of-subtree t)
;;       (insert "\n")
;;       (org-insert-heading)
;;       (insert title)
;;       (org-set-property "DATE" (format-time-string "%Y-%m-%d"))
;;       (org-set-property "TAGS" "ai-conversation")
;;       (insert "\n")
;;       (insert (superchat--format-conversation conversation))
;;       ;;(message "Conversation saved as subnode: %s" title)
;;       )))

;; (defun superchat--save-conversation ()
;;   "Save Chat View conversation with user choice of method."
;;   (interactive)
;;   (let ((conversation (buffer-substring-no-properties (point-min) (point-max))))
;;     (when (and conversation (stringp conversation))
;;       (let* ((title (read-string "Conversation title: " (format "Chat-%s" (format-time-string "%Y%m%d"))))
;;              (choice (completing-read "Save conversation as: "
;;                                       '("new file" "append to current node" "create new subnode")
;;                                       nil t)))
;;         (pcase choice
;;           ("new file" (superchat--save-as-new-file conversation title))
;;           ("append to current node" (superchat--save-append-to-node conversation))
;;           ("create new subnode" (superchat--save-as-subnode conversation title)))))))

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
  (message "Superchat: Attempting to add file to context: %s" file-path)
  (when (and file-path (file-exists-p file-path))
    (condition-case err
        (progn
          (gptel-context-add-file file-path)
          (cl-pushnew file-path superchat--current-context-files :test #'equal)
          (message "Superchat: File %s added to gptel context" file-path))
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
    (message "Superchat: Session context cleared")))


;;;###autoload
(defun superchat ()
  "Open or switch to the Superchat buffer."
  (interactive)
  (superchat--load-user-commands)
  (let ((buffer (get-buffer-create superchat-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'superchat-mode)
          (superchat-mode))
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: Welcome to Superchat\n" 'face 'font-lock-title-face)))
        (superchat--insert-prompt)))
    (let ((window (display-buffer buffer)))
      (select-window window)
      (when superchat-display-single-window
        (delete-other-windows)))))

(defun superchat-test-file-extraction ()
  "Test function to verify file path extraction."
  (interactive)
  (let ((test-cases (list 
                     (cons "#/Users/chenyibin/Documents/ref/【哥飞SEO教程】一个有效提升网页排名的方法.org Summarize this article" 
                           "/Users/chenyibin/Documents/ref/【哥飞SEO教程】一个有效提升网页排名的方法.org")
                     (cons "#/Users/chenyibin/Documents/ref/test.txt" 
                           "/Users/chenyibin/Documents/ref/test.txt")
                     (cons "# /Users/chenyibin/Documents/ref/test.txt" 
                           "/Users/chenyibin/Documents/ref/test.txt")
                     (cons "#  /Users/chenyibin/Documents/ref/test.txt" 
                           "/Users/chenyibin/Documents/ref/test.txt"))))
    (dolist (test-case test-cases)
      (let ((test-input (car test-case))
            (expected-path (cdr test-case)))
        (message "Testing with input: %s" test-input)
        (let ((extracted-path (superchat--extract-file-path test-input)))
          (message "Extracted path: '%s'" extracted-path)
          (if extracted-path
              (progn
                (message "File exists: %s" (file-exists-p extracted-path))
                (if (string= extracted-path expected-path)
                    (message "✓ Test passed")
                  (message "✗ Test failed: expected '%s', got '%s'" expected-path extracted-path)))
            (message "No file path extracted"))
          (message "---"))))))
;; --- UI Commands and Mode Definition ---

(defvar superchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-send-input)
    (define-key map (kbd "#") #'superchat--smart-hash)
    (define-key map (kbd "C-c C-h") #'superchat--list-commands)
    ;;(define-key map (kbd "C-c C-s") #'superchat--save-conversation)
    map)
  "Keymap for superchat-mode.")

(define-derived-mode superchat-mode org-mode "SChat"
  "Major mode for the chat view conversation."
  :group 'superchat
  (setq-local truncate-lines t)
  (setq-local org-hide-leading-stars t)
  (read-only-mode -1)
  (setq-local completion-at-point-functions '(superchat--completion-at-point))
  (use-local-map superchat-mode-map))

(provide 'superchat)

;;; superchat.el ends here
