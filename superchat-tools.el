;;; superchat-tools.el --- Native tools for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:
;; This file defines a set of native Emacs Lisp tools for use with
;; Superchat and llm.el. Each tool follows a strict security model
;; requiring explicit user approval for any action that affects
;; the user's system.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-util)
(require 'auth-source)
(require 'subr-x)
(require 'llm nil t)

(declare-function superchat--subagent-run "superchat-subagent" (preset-name task &optional context))
(declare-function superchat--subagent-render-report "superchat-subagent" (preset-name report))

(declare-function superchat-db-tape-select "superchat-db" (sql &optional params))
(declare-function superchat-view-search "superchat-tape-view" (query &optional session-id limit))
(declare-function superchat-view-tool-history "superchat-tape-view" (tool-name &optional session-id limit))
(declare-function superchat-view-file-history "superchat-tape-view" (path &optional session-id limit))
(declare-function superchat-view-recent-errors "superchat-tape-view" (session-id &optional count))

(defvar superchat-tools-default-directory nil
  "When non-nil, overrides `default-directory' for tool operations.
Set by `superchat-send-region' / `superchat-send-defun' so
file-search tools operate in the source buffer's project root
instead of the chat buffer's directory.")

(defun superchat-tools--base-dir ()
  "Return the effective `default-directory' for tool operations.
Prefers `superchat-tools-default-directory' when set; falls back
to the buffer-local `default-directory'."
  (or superchat-tools-default-directory default-directory))

(declare-function llm-make-tool "llm")

(eval-when-compile
  (defvar gnutls-verify-error)
  (defvar gnutls-min-prime-bits))

(defconst superchat-tool--default-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  "Default user-agent used for outbound HTTP requests made by Superchat tools.")

(defun superchat--glob-to-regexp (glob)
  "Convert a glob pattern GLOB to a regular expression.
Handles `*' (zero or more chars), `?' (single char), and `.'
\(literal dot).  This is a local replacement for the built-in
`glob-to-regexp' (added in Emacs 30.1) so the package works on
the declared minimum Emacs 28.1."
  (mapconcat (lambda (c)
               (pcase c
                 (?* ".*")
                 (?? ".")
                 (?. "\\.")
                 (_ (char-to-string c))))
             glob
             ""))

;;;---------------------------------------------
;;; Helper Functions
;;;---------------------------------------------

(defun superchat-tool--confirm-diff (path new-content &optional mode)
  "Show a smart diff/preview and ask for user confirmation.
PATH: target file path
NEW-CONTENT: content to write
MODE: 'append, 'create, or nil (default replace)
Returns t if user approves, nil otherwise."
  (let* ((expanded-path (expand-file-name path))
         (file-exists (file-exists-p expanded-path))
         (old-content (if file-exists
                          (with-temp-buffer
                            (insert-file-contents expanded-path)
                            (buffer-string))
                        ""))
         (is-new-file (not file-exists))
         (is-append (eq mode 'append))
         (preview-content (if is-append
                              (concat old-content new-content)
                            new-content)))

    ;; Smart preview based on content size and operation
    (cond
     ;; New small file - just show basic info + preview
     ((and is-new-file (< (length new-content) 500))
      (message "📝 Creating new file: %s (%d chars)" expanded-path (length new-content))
      (with-temp-buffer
        (insert (format "=== Creating new file ===\n")
                (format "Path: %s\n" expanded-path)
                (format "Size: %d characters\n\n" (length new-content))
                (format "Preview (first 200 chars):\n%s%s"
                        (substring new-content 0 (min 200 (length new-content)))
                        (if (> (length new-content) 200) "..." "")))
        (let ((preview-buffer (current-buffer)))
          (with-help-window "*Superchat File Preview*"
            (with-current-buffer standard-output
              (insert-buffer-substring preview-buffer))))))

     ;; New large file - just show summary
     (is-new-file
      (message "📝 Creating new file: %s (%d chars)" expanded-path (length new-content)))

     ;; Append mode - show what will be added
     (is-append
      (message "➕ Appending to: %s (adding %d chars)" expanded-path (length new-content))
      (when (< (length new-content) 300)
        (with-temp-buffer
          (insert (format "=== Appending to file ===\n")
                  (format "Path: %s\n" expanded-path)
                  (format "Adding: %d characters\n\n" (length new-content))
                  (format "Content to append:\n%s" new-content))
          (let ((preview-buffer (current-buffer)))
            (with-help-window "*Superchat Append Preview*"
              (with-current-buffer standard-output
                (insert-buffer-substring preview-buffer)))))))

     ;; Existing file replacement - show smart diff
     (t
      (message "🔄 Replacing file: %s (old: %d chars, new: %d chars)"
               expanded-path (length old-content) (length new-content))
      (when (< (+ (length old-content) (length new-content)) 1000)
        (with-temp-buffer
          (insert (format "=== File Changes ===\n")
                  (format "Path: %s\n" expanded-path)
                  (format "Old size: %d chars, New size: %d chars\n\n"
                          (length old-content) (length new-content)))
          ;; Show a simple before/after for small files
          (when (< (length old-content) 300)
            (insert "--- Current content ---\n")
            (insert old-content)
            (insert "\n\n"))
          (when (< (length new-content) 300)
            (insert "+++ New content ---\n")
            (insert new-content))
          (let ((diff-buffer (current-buffer)))
            (with-help-window "*Superchat File Changes*"
              (with-current-buffer standard-output
                (insert-buffer-substring diff-buffer))))))))

    ;; Simplified confirmation with more options
    (let ((prompt (cond
                   (is-new-file "Create this new file?")
                   (is-append "Append this content?")
                   (t "Replace file with new content?"))))
      (prog1
          (y-or-n-p (format "%s (%s) " prompt expanded-path))
        ;; Extend timeout after user confirmation
        (when (fboundp 'superchat--extend-timeout)
          (superchat--extend-timeout))))))

;;;---------------------------------------------
;;; Tool Implementation Functions
;;;---------------------------------------------

(defun superchat-tool-shell-command (command)
  "Execute COMMAND and return its output. The user must approve every execution."
  (let ((confirmed (y-or-n-p (format "Superchat wants to execute shell command: %S. Allow?" command))))
    (when (and confirmed (fboundp 'superchat--extend-timeout))
      (superchat--extend-timeout))
    (if confirmed
        (shell-command-to-string command)
      "Error: User refused to execute the command.")))

(defun superchat-tool-write-file (path content)
  "Writes CONTENT to PATH with smart preview."
  (let ((expanded-path (expand-file-name path)))
    (if (superchat-tool--confirm-diff expanded-path content)
        (progn
          ;; Ensure directory exists
          (let ((dir (file-name-directory expanded-path)))
            (when (and dir (not (file-directory-p dir)))
              (make-directory dir t)))
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) expanded-path nil 'nomessage))
          (format "✅ File '%s' written successfully (%d chars)."
                  expanded-path (length content)))
      "❌ User refused to write to the file.")))

(defun superchat-tool-append-file (path content &optional newline)
  "Appends CONTENT to the end of PATH."
  (let* ((expanded-path (expand-file-name path))
         (add-newline (if (eq newline nil) t newline))  ; Default to true
         (final-content (if add-newline
                            (concat "\n" content)
                          content)))
    (if (superchat-tool--confirm-diff expanded-path final-content 'append)
        (progn
          ;; Ensure directory exists
          (let ((dir (file-name-directory expanded-path)))
            (when (and dir (not (file-directory-p dir)))
              (make-directory dir t)))
          ;; Append to file
          (with-temp-buffer
            (insert final-content)
            (write-region (point-min) (point-max) expanded-path t 'nomessage))
          (format "✅ Content appended to '%s' successfully (%d chars added)."
                  expanded-path (length final-content)))
      "❌ User refused to append to the file.")))

(defun superchat-tool-quick-write (path content)
  "Quickly writes CONTENT to PATH with minimal confirmation."
  (let* ((expanded-path (expand-file-name path))
         (content-length (length content))
         (file-exists (file-exists-p expanded-path))
         (action (if file-exists "replace" "create")))
    ;; Quick confirmation for small files
    (if (and (< content-length 1000)
             (y-or-n-p (format "Quick %s: %s (%d chars)? "
                               action expanded-path content-length)))
        (progn
          ;; Ensure directory exists
          (let ((dir (file-name-directory expanded-path)))
            (when (and dir (not (file-directory-p dir)))
              (make-directory dir t)))
          (with-temp-buffer
            (insert content)
            (write-region (point-min) (point-max) expanded-path nil 'nomessage))
          (format "⚡ Quick %s successful: '%s' (%d chars)"
                  action expanded-path content-length))
      (if (>= content-length 1000)
          "❌ Content too large for quick-write (use write-file for files >1000 chars)"
        "❌ User refused quick write operation."))))

(defun superchat-tool-read-file (path)
  "Read the entire content of PATH."
  (let ((expanded-path (expand-file-name path)))
    (if (not (file-exists-p expanded-path))
        (format "Error: File does not exist at path: %s" expanded-path)
      (let ((confirmed (y-or-n-p (format "Superchat wants to read file: %S. Allow?" expanded-path))))
        (when (and confirmed (fboundp 'superchat--extend-timeout))
          (superchat--extend-timeout))
        (if confirmed
            (with-temp-buffer
              (insert-file-contents expanded-path)
              (buffer-string))
          "Error: User refused to allow reading the file.")))))

(defun superchat-tool-list-files (&optional path)
  "Lists files and subdirectories in PATH directory."
  (let* ((target-path (or path (superchat-tools--base-dir)))
         (expanded-path (expand-file-name target-path)))
    (if (not (file-directory-p expanded-path))
        (format "Error: Path is not a valid directory: %s" expanded-path)
      (let ((confirmed (y-or-n-p (format "Superchat wants to list directory: %S. Allow?" expanded-path))))
        (when (and confirmed (fboundp 'superchat--extend-timeout))
          (superchat--extend-timeout))
        (if confirmed
            (let* ((files-and-attrs (directory-files-and-attributes expanded-path nil nil nil)))
              (if (not files-and-attrs)
                  (format "Directory is empty: %s" expanded-path)
                (mapconcat
                 (lambda (file-attr)
                   (let* ((filepath (car file-attr))
                          (attrs (cdr file-attr))
                          (is-dir (eq 'directory (nth 7 attrs)))
                          (size (nth 5 attrs))
                          (mod-time (nth 4 attrs))
                          (perms (nth 0 attrs))
                          (filename (file-name-nondirectory filepath)))
                     (format "%s %10d %s %s"
                             perms
                             size
                             (format-time-string "%Y-%m-%d %H:%M" mod-time)
                             (if is-dir (concat filename "/") filename))))
                 files-and-attrs
                 "\n")))
          "Error: User refused to list the directory.")))))

(defun superchat-tool-search-text (pattern &optional path)
  "Searches for PATTERN (literal or regex) in files under PATH."
  (let* ((search-path (or path (superchat-tools--base-dir)))
         ;; Using ripgrep (rg) is ideal. Fallback to standard grep if not available.
         (program (if (executable-find "rg") "rg" "grep"))
         (command
          (cond
           ((equal program "rg")
            (format "rg -n -- %s %s"
                    (shell-quote-argument pattern)
                    (shell-quote-argument search-path)))
           (t ; Fallback to grep
            (format "grep -r -n -- %s %s"
                    (shell-quote-argument pattern)
                    (shell-quote-argument search-path))))))
    (let ((confirmed (y-or-n-p (format "Superchat wants to search for pattern '%s' in '%s'. Allow?" pattern search-path))))
      (when (and confirmed (fboundp 'superchat--extend-timeout))
        (superchat--extend-timeout))
      (if confirmed
          (shell-command-to-string command)
        "Error: User refused to perform the search."))))

(defun superchat-tool-make-directory (parent name)
  "Create NAME directory inside PARENT directory."
  (condition-case nil
      (progn
        (make-directory (expand-file-name name parent) t)
        (format "Directory %s created/verified in %s" name parent))
    (error (format "Error creating directory %s in %s" name parent))))

(defun superchat-tool-find-files (pattern &optional path)
  "Recursively find files matching PATTERN glob under PATH."
  (let* ((search-path (or path (superchat-tools--base-dir)))
         (expanded-path (expand-file-name search-path)))
    (if (not (file-directory-p expanded-path))
        (format "Error: Path is not a valid directory: %s" expanded-path)
      (let ((confirmed (y-or-n-p (format "Superchat wants to find files matching '%s' in %S. Allow?" pattern expanded-path))))
        (when (and confirmed (fboundp 'superchat--extend-timeout))
          (superchat--extend-timeout))
        (if confirmed
            (let* ((regexp (superchat--glob-to-regexp pattern))
                   (files (directory-files-recursively expanded-path regexp)))
              (if files
                  (string-join files "\n")
                (format "No files found matching pattern '%s' in %s." pattern expanded-path)))
          "Error: User refused to perform the file search.")))))

(defun superchat-tool-read-buffer (buffer)
  "Return the contents of BUFFER."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Error: buffer %s is not live." buffer))
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun superchat-tool-append-to-buffer (buffer text)
  "Append TEXT to BUFFER."
  (with-current-buffer (get-buffer-create buffer)
    (save-excursion
      (goto-char (point-max))
      (insert text)))
  (format "Appended text to buffer %s" buffer))

(defun superchat-tool-edit-buffer (buffer-name old-string new-string)
  "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))  ;; Case-sensitive search
      (save-excursion
        (goto-char (point-min))
        (let ((count 0))
          (while (search-forward old-string nil t)
            (setq count (1+ count)))
          (if (= count 0)
              (format "Error: Could not find text to replace in buffer %s" buffer-name)
            (if (> count 1)
                (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
              (goto-char (point-min))
              (search-forward old-string)
              (replace-match new-string t t)
              (format "Successfully edited buffer %s" buffer-name))))))))

(defun superchat-tool-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

;;;---------------------------------------------
;;; Emacs introspection tools (for sub-agents)
;;;---------------------------------------------

(defun superchat-tool-describe-function (function-name)
  "Return the documentation for FUNCTION-NAME as a string."
  (let ((sym (intern-soft function-name)))
    (cond
     ((not sym)
      (format "No such function or variable: %s" function-name))
     ((fboundp sym)
      (format "Function: %s\n\n%s"
              function-name
              (or (documentation sym)
                  "(no documentation)")))
     (t
      (format "%s is not a function." function-name)))))

(defun superchat-tool-describe-variable (variable-name)
  "Return the documentation and value for VARIABLE-NAME as a string."
  (let ((sym (intern-soft variable-name)))
    (cond
     ((not sym)
      (format "No such variable: %s" variable-name))
     ((boundp sym)
      (format "Variable: %s\n\nValue: %S\n\n%s"
              variable-name
              (symbol-value sym)
              (or (documentation-property sym 'variable-documentation)
                  "(no documentation)")))
     (t
      (format "%s is not bound." variable-name)))))

(defun superchat-tool-eval-elisp (expression)
  "Evaluate EXPRESSION as Elisp and return the result as a string.
This tool is intended for safe introspection only."
  (condition-case err
      (let ((result (eval (read expression) t)))
        (format "Result: %S" result))
    (error (format "Error evaluating expression: %s" (error-message-string err)))))

(defun superchat-tool-delegate-to-subagent (preset task &optional context)
  "Delegate TASK to a sub-agent PRESET and return its report.
Optional CONTEXT is passed to the sub-agent so it knows the main
session's intent.  The report is also rendered in the main chat
buffer."
  (if (fboundp 'superchat--subagent-run)
      (let ((report (superchat--subagent-run preset task context)))
        (when (fboundp 'superchat--subagent-render-report)
          (superchat--subagent-render-report preset report))
        report)
    "Error: sub-agent module is not loaded."))

;;;---------------------------------------------
;;; Tape / memory retrieval tools
;;;---------------------------------------------

(defun superchat-tools--rows-to-json (rows)
  "Convert SQLite ROWS to a JSON string.
Each row becomes a JSON array so mixed-type columns encode reliably."
  (json-encode (mapcar (lambda (row) (vconcat row)) rows)))

(defun superchat-tool-sql (query)
  "Execute a read-only SELECT/WITH QUERY against the superchat SQLite DB.
Returns results as a JSON string.  Mutating statements are rejected
by signaling an error."
  (if (fboundp 'superchat-db-tape-select)
      (superchat-tools--rows-to-json (superchat-db-tape-select query))
    "Error: tape database is not available."))

(defun superchat-tool-memory-search (query)
  "Search the conversation tape for QUERY and return matches as JSON.
Searches the current session if one is active; otherwise all sessions."
  (if (fboundp 'superchat-view-search)
      (let* ((session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (rows (superchat-view-search query session-id 20)))
        (superchat-tools--rows-to-json rows))
    "Error: tape view layer is not available."))

(defun superchat-tool-tool-history (tool-name)
  "Return recent tool_call/tool_result entries for TOOL-NAME as JSON."
  (if (fboundp 'superchat-view-tool-history)
      (let* ((session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (rows (superchat-view-tool-history tool-name session-id 20)))
        (superchat-tools--rows-to-json rows))
    "Error: tape view layer is not available."))

(defun superchat-tool-file-history (path)
  "Return recent tool entries mentioning PATH as JSON."
  (if (fboundp 'superchat-view-file-history)
      (let* ((session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (rows (superchat-view-file-history path session-id 20)))
        (superchat-tools--rows-to-json rows))
    "Error: tape view layer is not available."))

(defun superchat-tool-recent-errors ()
  "Return recent error-like tool results as JSON."
  (if (fboundp 'superchat-view-recent-errors)
      (let* ((session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (rows (superchat-view-recent-errors session-id 10)))
        (superchat-tools--rows-to-json rows))
    "Error: tape view layer is not available."))

;;;---------------------------------------------
;;; Eglot / LSP Tools (soft dependency — fboundp guarded)
;;;---------------------------------------------

(declare-function eglot-current-server "eglot" (&optional buffer))
(declare-function eglot-find-references "eglot" (&optional pos))
(declare-function eglot-code-actions "eglot" (beg &optional end region-kind interactive))
(declare-function eglot-hover "eglot" (pos))

(defun superchat-tool-lsp-references (&optional buffer-name position)
  "Find references at POSITION in BUFFER-NAME via eglot.
Position defaults to the source buffer recorded by send-region/defun."
  (unless (fboundp 'eglot-current-server)
    (error "eglot is not active — start an LSP server first"))
  (let* ((buf (or (and buffer-name (get-buffer buffer-name))
                  (error "No source buffer available.  Send a region or defun first, or specify a BUFFER-NAME.")))
         (pos (or position
                  (with-current-buffer buf
                    (point)))))
    (with-current-buffer buf
      (unless (eglot-current-server)
        (error "No eglot server active in buffer %s" (buffer-name)))
      (let ((refs (condition-case nil
                      (eglot-find-references)
                    (error nil))))
        (if refs
            (mapconcat
             (lambda (loc)
               (let* ((file (plist-get loc :uri))
                      (range (plist-get loc :range))
                      (start (plist-get range :start)))
                 (format "%s:%d:%d"
                         (or file "unknown")
                         (1+ (plist-get start :line))
                         (1+ (plist-get start :character)))))
             refs "\n")
          "No references found")))))

(defun superchat-tool-lsp-code-actions (&optional buffer-name position)
  "Return available code actions at POSITION in BUFFER-NAME via eglot."
  (unless (fboundp 'eglot-code-actions)
    (error "eglot is not active — start an LSP server first"))
  (let* ((buf (or (and buffer-name (get-buffer buffer-name))
                  (error "No source buffer available")))
         (pos (or position
                  (with-current-buffer buf
                    (point)))))
    (with-current-buffer buf
      (unless (eglot-current-server)
        (error "No eglot server active in buffer %s" (buffer-name)))
      (let ((actions (condition-case nil
                         (eglot-code-actions pos)
                       (error nil))))
        (if actions
            (mapconcat
             (lambda (a)
               (format "%s — %s"
                       (or (plist-get a :title) "untitled")
                       (or (plist-get a :kind) "")))
             actions "\n")
          "No code actions available")))))

(defun superchat-tool-lsp-hover (&optional buffer-name position)
  "Return hover information at POSITION in BUFFER-NAME via eglot."
  (unless (fboundp 'eglot-hover)
    (error "eglot is not active — start an LSP server first"))
  (let* ((buf (or (and buffer-name (get-buffer buffer-name))
                  (error "No source buffer available")))
         (pos (or position
                  (with-current-buffer buf
                    (point)))))
    (with-current-buffer buf
      (unless (eglot-current-server)
        (error "No eglot server active in buffer %s" (buffer-name)))
      (let ((hover (condition-case nil
                       (eglot-hover pos)
                     (error nil))))
        (if hover
            (let ((contents (plist-get hover :contents)))
              (if (stringp contents)
                  contents
                ;; MarkedString / MarkupContent
                (if (plist-get contents :value)
                    (plist-get contents :value)
                  (format "%S" contents))))
          "No hover information")))))

;;;---------------------------------------------
;;; Tool List (llm.el) — replaces gptel-tools
;;;---------------------------------------------

(defvar superchat-llm-tools-list nil
  "List of llm.el tool structs registered by Superchat.
Populated lazily by `superchat-get-llm-tools' the first time it's called.
Refreshed by `superchat-llm-tools-reload'.")

(defcustom superchat-llm-tool-names
  '("read-file" "list-files" "search-text" "read_buffer"
    "sql" "memory_search" "tool_history" "file_history" "recent_errors")
  "Built-in llm.el tool names Superchat exposes by default.

The implementation still keeps the larger tool library available, but
only names in this allowlist are registered into
`superchat-llm-tools-list'.  This keeps normal chats out of heavyweight
tool-calling mode and keeps explicit tool use closer to Bub's small
read-only surface.

Set to `all' to expose every built-in tool.  Set to nil to expose no
built-in tools."
  :type '(choice (const :tag "No built-in tools" nil)
                 (const :tag "All built-in tools" all)
                 (repeat :tag "Allowlisted tool names" string))
  :group 'superchat)

(defun superchat--llm-tool-enabled-p (name)
  "Return non-nil when built-in tool NAME should be registered."
  (or (eq superchat-llm-tool-names 'all)
      (member name superchat-llm-tool-names)))

(defun superchat--maybe-make-llm-tool (name &rest args)
  "Build llm.el tool NAME with ARGS when it is enabled."
  (when (superchat--llm-tool-enabled-p name)
    (apply #'llm-make-tool :name name args)))

;;;###autoload
(defun superchat-llm-tools-reload ()
  "Rebuild `superchat-llm-tools-list' with the current tool implementations.
Useful after editing tool functions or adding new ones."
  (interactive)
  (unless (fboundp 'llm-make-tool)
    (user-error "llm.el is not loaded — cannot register tools"))
  (setq superchat-llm-tools-list
        (delq nil
              (list
         (superchat--maybe-make-llm-tool
          "shell-command"
          :description "Execute a shell command and return its output. The user must approve every execution."
          :args (list (list :name "command"
                            :type 'string
                            :description "The shell command to execute."))
          :function #'superchat-tool-shell-command)

         (superchat--maybe-make-llm-tool
          "write-file"
          :description "Writes content to a specified file with smart preview. Shows appropriate preview based on file size and operation type."
          :args (list (list :name "path"
                            :type 'string
                            :description "The path of the file to write to.")
                      (list :name "content"
                            :type 'string
                            :description "The content to write to the file."))
          :function #'superchat-tool-write-file)

         (superchat--maybe-make-llm-tool
          "append-file"
          :description "Appends content to the end of an existing file, or creates a new file if it doesn't exist. Much more convenient than write-file for adding content."
          :args (list (list :name "path"
                            :type 'string
                            :description "The path of the file to append to.")
                      (list :name "content"
                            :type 'string
                            :description "The content to append to the file.")
                      (list :name "newline"
                            :type 'boolean
                            :description "Whether to add a newline before the content (default: true)."
                            :optional t))
          :function #'superchat-tool-append-file)

         (superchat--maybe-make-llm-tool
          "quick-write"
          :description "Quickly writes small content to a file with minimal confirmation. Best for small files under 1000 characters. Only shows a simple confirmation."
          :args (list (list :name "path"
                            :type 'string
                            :description "The path of the file to write to.")
                      (list :name "content"
                            :type 'string
                            :description "The content to write (preferably under 1000 chars)."))
          :function #'superchat-tool-quick-write)

         (superchat--maybe-make-llm-tool
          "read-file"
          :description "Reads the entire content of a specified file. Requires user approval for each file access."
          :args (list (list :name "path"
                            :type 'string
                            :description "The path of the file to read."))
          :function #'superchat-tool-read-file)

         (superchat--maybe-make-llm-tool
          "list-files"
          :description "Lists files and subdirectories in a specified directory, similar to 'ls -l'. Requires user approval."
          :args (list (list :name "path"
                            :type 'string
                            :description "The path to the directory to list. Defaults to the current directory if not provided."
                            :optional t))
          :function #'superchat-tool-list-files)

         (superchat--maybe-make-llm-tool
          "search-text"
          :description "Searches for a textual pattern (or regular expression) in files. Requires user approval."
          :args (list (list :name "pattern"
                            :type 'string
                            :description "The text or regular expression to search for.")
                      (list :name "path"
                            :type 'string
                            :description "The specific file or directory to search in. Defaults to the current directory if not provided."
                            :optional t))
          :function #'superchat-tool-search-text)

         (superchat--maybe-make-llm-tool
          "make_directory"
          :description "Create a new directory with the given name in the specified parent directory"
          :args (list (list :name "parent"
                            :type 'string
                            :description "The parent directory where the new directory should be created, e.g. /tmp")
                      (list :name "name"
                            :type 'string
                            :description "The name of the new directory to create, e.g. testdir"))
          :function #'superchat-tool-make-directory)

         (superchat--maybe-make-llm-tool
          "find-files"
          :description "Recursively finds files matching a glob pattern. Requires user approval."
          :args (list (list :name "pattern"
                            :type 'string
                            :description "The glob pattern to match files against, e.g., '*.js' or 'src/**/*.py'.")
                      (list :name "path"
                            :type 'string
                            :description "The directory to start the search from. Defaults to the current directory."
                            :optional t))
          :function #'superchat-tool-find-files)

         (superchat--maybe-make-llm-tool
          "read_buffer"
          :description "Return the contents of an Emacs buffer"
          :args (list (list :name "buffer"
                            :type 'string
                            :description "The name of the buffer whose contents are to be retrieved"))
          :function #'superchat-tool-read-buffer)

         (superchat--maybe-make-llm-tool
          "append_to_buffer"
          :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
          :args (list (list :name "buffer"
                            :type 'string
                            :description "The name of the buffer to append text to.")
                      (list :name "text"
                            :type 'string
                            :description "The text to append to the buffer."))
          :function #'superchat-tool-append-to-buffer)

         (superchat--maybe-make-llm-tool
          "EditBuffer"
          :description "Edits Emacs buffers"
          :args (list (list :name "buffer_name"
                            :type 'string
                            :description "Name of the buffer to modify")
                      (list :name "old_string"
                            :type 'string
                            :description "Text to replace (must match exactly)")
                      (list :name "new_string"
                            :type 'string
                            :description "Text to replace old_string with"))
          :function #'superchat-tool-edit-buffer)

         (superchat--maybe-make-llm-tool
          "ReplaceBuffer"
          :description "Completely overwrites buffer contents"
          :args (list (list :name "buffer_name"
                            :type 'string
                            :description "Name of the buffer to overwrite")
                      (list :name "content"
                            :type 'string
                            :description "Content to write to the buffer"))
          :function #'superchat-tool-replace-buffer)

         ;; ── Emacs introspection tools (for sub-agents) ──

         (superchat--maybe-make-llm-tool
          "describe-function"
          :description "Return documentation for an Emacs Lisp function."
          :args (list (list :name "function_name"
                            :type 'string
                            :description "Name of the function to describe."))
          :function #'superchat-tool-describe-function)

         (superchat--maybe-make-llm-tool
          "describe-variable"
          :description "Return documentation and current value of an Emacs Lisp variable."
          :args (list (list :name "variable_name"
                            :type 'string
                            :description "Name of the variable to describe."))
          :function #'superchat-tool-describe-variable)

         (superchat--maybe-make-llm-tool
          "eval-elisp"
          :description "Evaluate an Emacs Lisp expression and return the result."
          :args (list (list :name "expression"
                            :type 'string
                            :description "Emacs Lisp expression to evaluate."))
          :function #'superchat-tool-eval-elisp)

         ;; ── Sub-agent delegation tool ──

         (superchat--maybe-make-llm-tool
          "delegate_to_subagent"
          :description "Delegate a task to an isolated sub-agent preset. \
Available presets: researcher (read-only investigation), executor (can modify files/run commands), \
introspector (Emacs introspection). Returns the sub-agent's report."
          :args (list (list :name "preset"
                            :type 'string
                            :description "Sub-agent preset name: researcher, executor, or introspector.")
                      (list :name "task"
                            :type 'string
                            :description "The task to delegate to the sub-agent.")
                      (list :name "context"
                            :type 'string
                            :description "Relevant context from the main session."
                            :optional t))
          :function #'superchat-tool-delegate-to-subagent)

         ;; ── Tape / memory retrieval tools ──

         (superchat--maybe-make-llm-tool
          "sql"
          :description "Run a read-only SELECT/WITH SQL query against the superchat SQLite database. \
Use this to compose complex views across the conversation tape."
          :args (list (list :name "query"
                            :type 'string
                            :description "SQL query (SELECT or WITH only)."))
          :function #'superchat-tool-sql)

         (superchat--maybe-make-llm-tool
          "memory_search"
          :description "Search the conversation tape for relevant entries. \
Returns matching tape rows as JSON."
          :args (list (list :name "query"
                            :type 'string
                            :description "Search query (FTS5 trigram search)."))
          :function #'superchat-tool-memory-search)

         (superchat--maybe-make-llm-tool
          "tool_history"
          :description "Return recent tool_call/tool_result entries for a specific tool name."
          :args (list (list :name "tool_name"
                            :type 'string
                            :description "Tool name, e.g. shell-command or write-file."))
          :function #'superchat-tool-tool-history)

         (superchat--maybe-make-llm-tool
          "file_history"
          :description "Return recent tool entries that mention a specific file path."
          :args (list (list :name "path"
                            :type 'string
                            :description "File path to look for in tool history."))
          :function #'superchat-tool-file-history)

         (superchat--maybe-make-llm-tool
          "recent_errors"
          :description "Return recent error-like tool results from the current session."
          :args nil
          :function #'superchat-tool-recent-errors)

         ;; ── Eglot/LSP tools (soft dep: only when eglot is loaded) ──

         (superchat--maybe-make-llm-tool
          "lsp-references"
          :description "Find all references of the symbol at point via eglot/LSP. \
Returns a list of file:line:col locations."
          :args (list (list :name "buffer-name"
                            :type 'string
                            :description "Source buffer name (from send-region/defun)."
                            :optional t)
                      (list :name "position"
                            :type 'number
                            :description "Buffer position (default: point)."
                            :optional t))
          :function #'superchat-tool-lsp-references)

         (superchat--maybe-make-llm-tool
          "lsp-code-actions"
          :description "Return available code actions at point via eglot/LSP \
(e.g., extract function, fix imports)."
          :args (list (list :name "buffer-name"
                            :type 'string
                            :description "Source buffer name."
                            :optional t)
                      (list :name "position"
                            :type 'number
                            :description "Buffer position."
                            :optional t))
          :function #'superchat-tool-lsp-code-actions)

         (superchat--maybe-make-llm-tool
          "lsp-hover"
          :description "Return hover/documentation info for the symbol at point \
via eglot/LSP."
          :args (list (list :name "buffer-name"
                            :type 'string
                            :description "Source buffer name."
                            :optional t)
                      (list :name "position"
                            :type 'number
                            :description "Buffer position."
                            :optional t))
          :function #'superchat-tool-lsp-hover))))
  superchat-llm-tools-list)

(defun superchat-get-llm-tools ()
  "Return the list of llm.el tools registered by Superchat.
Builds the list on first call (when `superchat-llm-tools-list' is nil).
Returns nil if llm.el is not loaded."
  (unless (fboundp 'llm-make-tool)
    (when (fboundp 'superchat--log)
      (superchat--log :warn "llm.el not loaded; tools disabled"))
    (setq superchat-llm-tools-list nil))
  (or superchat-llm-tools-list
      (and (fboundp 'llm-make-tool)
           (superchat-llm-tools-reload))))

(provide 'superchat-tools)

;;; superchat-tools.el ends here
