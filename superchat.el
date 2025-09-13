;;; superchat.el --- A standalone AI chat client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a general-purpose, standalone chat view for interacting
;; with LLMs via gptel. It is completely independent of org-supertag.
;; It includes a command system for custom prompts and session-saving features.

;;; Code:

(require 'cl-lib)
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

(defcustom superchat-prompt-directory
  (expand-file-name "prompts/" superchat-data-directory)
  "Directory for saving and loading prompt files."
  :type 'directory
  :group 'superchat)

(defcustom superchat-prompt-file-extensions
  '("prompt" "org" "txt" "md")
  "List of file extensions that are considered valid prompt files."
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
(defvar-local superchat--current-command nil
  "Current active chat command (e.g. 'create-question', nil for default mode).")
(defvar-local superchat--current-response-parts nil
  "A list to accumulate response chunks in a streaming conversation.")

;; --- Command System Variables ---
(defvar superchat--user-commands (make-hash-table :test 'equal)
  "Store all user-defined chat commands, command name -> prompt content.")

(defconst superchat--command-dir
  (expand-file-name "command/" superchat-data-directory)
  "Path to the custom command prompt files. One .prompt file per command.")

(defconst superchat--builtin-commands
  '(("create-question" . "Please list all important questions related to $input in $lang."))
  "Alist of built-in commands and their prompts. Use $input and $lang for variables.")

(defun superchat--get-prompt-file-path (prompt-name)
  "Get the full path for a prompt file by PROMPT-NAME.
Supports multiple file extensions defined in `superchat-prompt-file-extensions`."
  (when (and prompt-name superchat-prompt-directory)
    (cl-find-if #'file-exists-p
                (mapcar (lambda (ext)
                          (expand-file-name (concat prompt-name "." ext) superchat-prompt-directory))
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
  (when (file-directory-p superchat-prompt-directory)
    (let ((prompt-files '()))
      (dolist (file (directory-files superchat-prompt-directory t))
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
               "* User: ")))
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

(defun superchat--stream-llm-result (chunk)
  "Process a chunk from the LLM streaming response, update UI and accumulate response."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (push chunk superchat--current-response-parts)
      (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert "\n** Assistant\n")
        (setq superchat--response-start-marker nil))
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
      ;; If streaming failed, clear the status message here
      (when (and superchat--response-start-marker (marker-position superchat--response-start-marker))
        (goto-char superchat--response-start-marker)
        (delete-region (point) (line-end-position))
        (insert "\n** Assistant\n")
        (insert (superchat--md-to-org response-content)))

      ;; 1. Update conversation history with the full response
      (setq superchat--conversation-history
            (cons (list :role "assistant" :content response-content)
                  superchat--conversation-history))

      ;; 2. Insert a newline for spacing before the next prompt
      (insert "\n")
      (superchat--insert-prompt))))

(defun superchat--current-input ()
  "Return current prompt line user text."
  (let ((txt (when (and superchat--prompt-start (marker-position superchat--prompt-start))
               (save-excursion
                 (goto-char superchat--prompt-start)
                 (string-trim (buffer-substring-no-properties (point) (line-end-position)))))))
    (if (and txt (not (string-empty-p txt)))
        txt
      (save-excursion
        (goto-char (point-max))
        (when (re-search-backward "^\* User.*?:[ ]*\(.*\)$" nil t)
          (string-trim (or (match-string 1) "")))))))

;; --- Command System ---

(defun superchat--load-user-commands ()
  "Load all custom command prompt files into hash-table."
  (clrhash superchat--user-commands)
  (when (file-directory-p superchat--command-dir)
    (dolist (file (directory-files superchat--command-dir t "\.prompt$"))
      (let ((name (file-name-base file)))
        (with-temp-buffer
          (insert-file-contents file)
          (puthash name (buffer-string) superchat--user-commands)))))
  ;; Also load prompts from the prompt directory
  (when (file-directory-p superchat-prompt-directory)
    (dolist (file (directory-files superchat-prompt-directory t))
      (when (and (file-regular-p file)
                 (member (file-name-extension file) superchat-prompt-file-extensions))
        (let ((name (file-name-base file)))
          (with-temp-buffer
            (insert-file-contents file)
            (puthash name (buffer-string) superchat--user-commands))))))

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
  (unless (file-directory-p superchat--command-dir)
    (make-directory superchat--command-dir t))
  (let ((file (expand-file-name (concat name ".prompt") superchat--command-dir)))
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
      (maphash (lambda (k v)
                 (insert (format "- `/%s`\n" k)))
               superchat--user-commands))
    (let ((available-prompts (superchat--list-available-prompts)))
      (when available-prompts
        (insert "\n**Available Prompt Files:**\n")
        (dolist (prompt available-prompts)
          (insert (format "- `%s`\n" prompt)))))
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
(defun superchat--extract-file-path (input)
  "Extract and normalize file path from INPUT string.
Handles various edge cases like spaces, parentheses, and quotes in file paths."
  (let ((file-path-regexp "#\\s-*\\(/[^#\n\r]*\\)")
        (file-path nil))
    (when (string-match file-path-regexp input)
      (setq file-path (match-string 1 input))
      ;; Handle escaped spaces
      (setq file-path (replace-regexp-in-string "\\\\ " " " file-path))
      ;; Remove surrounding quotes if present
      (when (and (>= (length file-path) 2)
                 (string= "\"" (substring file-path 0 1))
                 (string= "\"" (substring file-path -1)))
        (setq file-path (substring file-path 1 -1)))
      ;; Expand file name to handle ~ and other shortcuts
      (setq file-path (expand-file-name file-path))
      ;; --- DIAGNOSTIC MESSAGE ---
      ;;(message "Superchat: Extracted file path: %s" file-path)
      ;;(message "Superchat: File exists: %s" (file-exists-p file-path))
      ;; --- END DIAGNOSTIC ---
      file-path)))

;; --- Main Send Logic ---
(defun superchat--build-final-prompt (input)
  "Build the final prompt string, adding file content if specified."
  (let ((user-query input)
        (file-path nil))
    ;; 1. Check for and extract file path from the input.
    (setq file-path (superchat--extract-file-path user-query))
    (when file-path
      (let* ((path-start (match-beginning 0))
             (path-end (match-end 0))
             (text-before-path (string-trim (substring user-query 0 path-start)))
             (text-after-path (string-trim (substring user-query path-end))))
        (setq user-query (string-trim 
                         (cond
                          ((and (> (length text-before-path) 0) (> (length text-after-path) 0))
                           (concat text-before-path " " text-after-path))
                          ((> (length text-before-path) 0)
                           text-before-path)
                          ((> (length text-after-path) 0)
                           text-after-path)
                          (t
                           (progn 
                             (setq user-query (replace-match "" t t user-query 0))
                             (string-trim user-query))))))
        ;; --- DIAGNOSTIC MESSAGE ---
        ;; (message "Superchat: Text before path: '%s'" text-before-path)
        ;; (message "Superchat: Text after path: '%s'" text-after-path)
        ;; (message "Superchat: Combined user query: '%s'" user-query)
        ;; --- END DIAGNOSTIC ---
        
        ;; 使用我们新的上下文功能添加文件
        (superchat--add-file-to-context file-path)))

    ;; --- DIAGNOSTIC PROBE ---
    ;; (message "--- BUILD-PROMPT --- File path parsed as: '%s'" file-path)
    ;; (message "--- BUILD-PROMPT --- Final user query: '%s'" user-query)

    ;; 2. Get the base prompt template (from sticky command or default).
    (let* ((prompt-template
            (if superchat--current-command
                (or (cdr (assoc superchat--current-command superchat--builtin-commands))
                    (gethash superchat--current-command superchat--user-commands))
              superchat-general-answer-prompt))
           ;; 3. Substitute $input with the user's query.
           (base-prompt (replace-regexp-in-string "\\$input" user-query prompt-template)))
      
      base-prompt)))

(defun superchat--execute-llm-query (input)
  "Build the final prompt from INPUT and return a result plist for the dispatcher."
  (let ((final-prompt (superchat--build-final-prompt input)))
    `(:type :llm-query :prompt ,final-prompt)))

(defun superchat--handle-command (command args input)
  "Handle all commands and return a result plist describing what to do next."
  ;; Ensure the command is loaded (for prompt files)
  (superchat--ensure-command-loaded command)
  
  (cond
   ;; Meta-command: /commands
   ((equal command "commands")
    `(:type :buffer :content ,(superchat--list-commands-as-string)))
   ;; Meta-command: /reset
   ((equal command "reset")
    (setq superchat--current-command nil)
    `(:type :echo :content "Switched to default chat mode."))
   ;; Meta-command: /clear-context
   ((equal command "clear-context")
    (superchat--clear-session-context)
    `(:type :echo :content "Session context cleared."))
   ;; Meta-command: /define
   ((equal command "define")
    (if-let ((define-pair (superchat--parse-define input)))
        (progn
          (superchat--define-command (car define-pair) (cdr define-pair))
          `(:type :echo :content ,(format "Command `/%s` defined." (car define-pair))))
      `(:type :echo :content "Invalid `/define` syntax.")))
   ;; Normal prompt command
   ((or (assoc command superchat--builtin-commands) (gethash command superchat--user-commands))
    (setq superchat--current-command command)
    (if (and args (> (length args) 0))
        `(:type :llm-query-and-mode-switch :args ,args)
      `(:type :echo :content ,(format "Switched to command mode: `/%s`." command))))
   ;; Unknown command
   (command
    `(:type :echo :content ,(format "Unknown command: `/%s`." command)))
   ;; Default case: not a command
   (t nil)))

(defun superchat-send-input ()
  "Parse user input, get a result-plist from a handler, and render the result."
  (interactive)
  (let ((input (superchat--current-input)))
    (when (and input (> (length input) 0))
      ;; Finalize the user's input line.
      (let ((inhibit-read-only t))
        (put-text-property superchat--prompt-start (line-end-position) 'read-only t)
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
           ;;(message "%s" (plist-get result :content))
           (superchat--refresh-prompt))
          (:llm-query
           (superchat--update-status "Assistant is thinking...")
           (superchat--llm-generate-answer (plist-get result :prompt)
                                           #'superchat--process-llm-result
                                           #'superchat--stream-llm-result))
          (:llm-query-and-mode-switch
           (superchat--update-status (format "Executing `/%s`..." command))
           (let* ((real-args (plist-get result :args))
                  (llm-result (superchat--execute-llm-query real-args)))
             (superchat--llm-generate-answer (plist-get llm-result :prompt)
                                             #'superchat--process-llm-result
                                             #'superchat--stream-llm-result))))))))

;; --- LLM Backend (Extracted from supertag-rag.el) ---

(defun superchat--llm-generate-answer (prompt callback stream-callback)
  "Generate an answer using gptel, correctly handling its streaming callback."
  (let ((buffer (get-buffer-create "*superchat-temp-request*"))
        (response-parts '()))
    (with-current-buffer buffer
      (erase-buffer)
      (text-mode)
      (insert prompt))
    (apply #'gptel-request
           nil ; Prompt is nil, gptel reads from buffer
           (append `(:buffer ,buffer
                     :stream t
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
                                         (funcall callback final-response))) 
                                     (kill-buffer buffer)))))
                   (if superchat-model `(:model ,superchat-model) nil)))))

;; ;; --- Save Conversation ---
;; (defun superchat--format-conversation (conversation)
;;   "Format conversation for org-mode display."
;;   (let ((formatted (replace-regexp-in-string "^" "> " (or conversation ""))))
;;     (format "#+BEGIN_QUOTE\n%s\n#+END_QUOTE\n" formatted)))

;; (defun superchat--save-as-new-file (conversation title)
;;   "Save conversation as new org file."
;;   (when (and conversation title (stringp conversation) (stringp title))
;;     (let ((filename (expand-file-name (concat title ".org") superchat-save-directory)))
;;       (make-directory superchat-save-directory t)
;;       (with-current-buffer (find-file-noselect filename)
;;         (insert (format "#+TITLE: %s\n#+DATE: %s\n#+TAGS: ai-conversation\n\n" 
;;                         title (format-time-string "%Y-%m-%d")))
;;         (insert "* AI Conversation\n\n")
;;         (insert (superchat--format-conversation conversation))
;;         (save-buffer)
;;         ;;(message "Conversation saved to: %s" filename)
;;         ))))

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
        (let ((file (read-file-name "Select file for context: ")))
          (when file
            (insert "#" (expand-file-name file))))
      ;; (message "Smart hash debug: prompt-start-pos=%s, point=%s, bol-text=\"%s\"" 
      ;;          prompt-start-pos 
      ;;          p 
      ;;          (save-excursion 
      ;;            (beginning-of-line) 
      ;;            (buffer-substring (point) (min (+ (point) 20) (point-max)))))
      (message "Smart hash '#' can only be used in the prompt area."))))

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
  (superchat--load-user-commands) ; Load commands every time chat is opened
  (let* ((buffer (get-buffer-create superchat-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (unless (derived-mode-p 'superchat-mode)
          (superchat-mode))
        (goto-char (point-max))
        (when (= (point-min) (point-max))
          (insert (propertize "#+TITLE: Welcome to Superchat\n" 'face 'font-lock-title-face)))
        (superchat--insert-prompt))))
  (display-buffer superchat-buffer-name)
  (select-window (get-buffer-window superchat-buffer-name)))

;; --- UI Commands and Mode Definition ---

(defvar superchat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'superchat-send-input)
    (define-key map (kbd "#") #'superchat--smart-hash)
    (define-key map (kbd "RET") #'superchat-send-input)
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
