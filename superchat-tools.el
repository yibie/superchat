;;; superchat-tools.el --- Native tools for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:
;; This file defines a set of native Emacs Lisp tools for use with
;; Superchat and gptel. Each tool follows a strict security model
;; requiring explicit user approval for any action that affects
;; the user's system.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-util)
(require 'auth-source)
(require 'subr-x)

(eval-when-compile
  (defvar gnutls-verify-error)
  (defvar gnutls-min-prime-bits))

(defconst superchat-tool--default-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  "Default user-agent used for outbound HTTP requests made by Superchat tools.")

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
      (y-or-n-p (format "%s (%s) " prompt expanded-path)))))

;;;---------------------------------------------
;;; Tool: shell-command
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "shell-command"
              :description "Execute a shell command and return its output. The user must approve every execution."
              :args '((:name "command"
                             :type string
                             :description "The shell command to execute."
                             :required t))
              :function (lambda (command)
                          (if (y-or-n-p (format "Superchat wants to execute shell command: %S. Allow?" command))
                              (condition-case err
                                  (shell-command-to-string command)
                                (error (format "Error executing command: %s" (error-message-string err))))
                            "Error: User refused to execute the command."))
              :category "system"))

;;;---------------------------------------------
;;; Tool: write-file (Upgraded with diff-approval)
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "write-file"
              :description "Writes content to a specified file with smart preview. Shows appropriate preview based on file size and operation type."
              :args '((:name "path"
                             :type string
                             :description "The path of the file to write to."
                             :required t)
                      (:name "content"
                             :type string
                             :description "The content to write to the file."
                             :required t))
              :function (lambda (path content)
                          (let ((expanded-path (expand-file-name path)))
                            (if (superchat-tool--confirm-diff expanded-path content)
                                (condition-case err
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
                                  (error (format "❌ Error writing to file: %s" (error-message-string err))))
                              "❌ User refused to write to the file.")))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: append-file (New improved tool)
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "append-file"
              :description "Appends content to the end of an existing file, or creates a new file if it doesn't exist. Much more convenient than write-file for adding content."
              :args '((:name "path"
                             :type string
                             :description "The path of the file to append to."
                             :required t)
                      (:name "content"
                             :type string
                             :description "The content to append to the file."
                             :required t)
                      (:name "newline"
                             :type boolean
                             :description "Whether to add a newline before the content (default: true)."
                             :optional t))
              :function (lambda (path content &optional newline)
                          (let* ((expanded-path (expand-file-name path))
                                 (add-newline (if (eq newline nil) t newline))  ; Default to true
                                 (final-content (if add-newline 
                                                    (concat "\n" content)
                                                  content)))
                            (if (superchat-tool--confirm-diff expanded-path final-content 'append)
                                (condition-case err
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
                                  (error (format "❌ Error appending to file: %s" (error-message-string err))))
                              "❌ User refused to append to the file.")))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: quick-write (New fast tool for small files)
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "quick-write"
              :description "Quickly writes small content to a file with minimal confirmation. Best for small files under 1000 characters. Only shows a simple confirmation."
              :args '((:name "path"
                             :type string
                             :description "The path of the file to write to."
                             :required t)
                      (:name "content"
                             :type string
                             :description "The content to write (preferably under 1000 chars)."
                             :required t))
              :function (lambda (path content)
                          (let* ((expanded-path (expand-file-name path))
                                 (content-length (length content))
                                 (file-exists (file-exists-p expanded-path))
                                 (action (if file-exists "replace" "create")))
                            
                            ;; Quick confirmation for small files
                            (if (and (< content-length 1000)
                                     (y-or-n-p (format "Quick %s: %s (%d chars)? " 
                                                        action expanded-path content-length)))
                                (condition-case err
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
                                  (error (format "❌ Error in quick write: %s" (error-message-string err))))
                              (if (>= content-length 1000)
                                  "❌ Content too large for quick-write (use write-file for files >1000 chars)"
                                "❌ User refused quick write operation."))))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: read-file
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "read-file"
              :description "Reads the entire content of a specified file. Requires user approval for each file access."
              :args '((:name "path"
                             :type string
                             :description "The path of the file to read."
                             :required t))
              :function (lambda (path)
                          (let ((expanded-path (expand-file-name path)))
                            (if (not (file-exists-p expanded-path))
                                (format "Error: File does not exist at path: %s" expanded-path)
                              (if (y-or-n-p (format "Superchat wants to read file: %S. Allow?" expanded-path))
                                  (condition-case err
                                      (with-temp-buffer
                                        (insert-file-contents expanded-path)
                                        (buffer-string))
                                    (error (format "Error reading file: %s" (error-message-string err))))
                                "Error: User refused to allow reading the file."))))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: list-files
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "list-files"
              :description "Lists files and subdirectories in a specified directory, similar to 'ls -l'. Requires user approval."
              :args '((:name "path"
                             :type string
                             :description "The path to the directory to list. Defaults to the current directory if not provided."
                             :optional t))
              :function (lambda (&optional path)
                          (let* ((target-path (or path default-directory))
                                 (expanded-path (expand-file-name target-path)))
                            (if (not (file-directory-p expanded-path))
                                (format "Error: Path is not a valid directory: %s" expanded-path)
                              (if (y-or-n-p (format "Superchat wants to list directory: %S. Allow?" expanded-path))
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
                                "Error: User refused to list the directory."))))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: search-text
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "search-text"
              :description "Searches for a textual pattern (or regular expression) in files. Requires user approval."
              :args '((:name "pattern"
                             :type string
                             :description "The text or regular expression to search for."
                             :required t)
                      (:name "path"
                             :type string
                             :description "The specific file or directory to search in. Defaults to the current directory if not provided."
                             :optional t))
              :function (lambda (pattern &optional path)
                          (let* ((search-path (or path "."))
                                 ;; Using ripgrep (rg) is ideal. Fallback to standard grep if not available.
                                 (program (if (executable-find "rg") "rg" "grep"))
                                 (command
                                  (cond
                                   ((equal program "rg")
                                    ;; rg's --json output is great for machines, but text is fine for now.
                                    (format "rg -n -- %s %s"
                                            (shell-quote-argument pattern)
                                            (shell-quote-argument search-path)))
                                   (t ; Fallback to grep
                                    (format "grep -r -n -- %s %s"
                                            (shell-quote-argument pattern)
                                            (shell-quote-argument search-path))))))
                            (if (y-or-n-p (format "Superchat wants to search for pattern '%s' in '%s'. Allow?" pattern search-path))
                                (condition-case err
                                    (shell-command-to-string command)
                                  (error (format "Error executing search command: %s" (error-message-string err))))
                              "Error: User refused to perform the search.")))
              :category "filesystem"))


;;;---------------------------------------------
;;; Tool: edit-file
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "edit-file"
              :description "Precisely replace a block of text in a file. The 'old_string' must be an exact, unique match."
              :args '((:name "path"
                             :type string
                             :description "The path of the file to edit."
                             :required t)
                      (:name "old_string"
                             :type string
                             :description "The exact block of text to be replaced. Must be unique in the file."
                             :required t)
                      (:name "new_string"
                             :type string
                             :description "The new text to replace the 'old_string'."
                             :required t))
              :function (lambda (path old_string new_string)
                          (let ((expanded-path (expand-file-name path)))
                            (if (not (file-exists-p expanded-path))
                                (format "Error: File does not exist at path: %s" expanded-path)
                              (let* ((original-content (with-temp-buffer
                                                         (insert-file-contents expanded-path)
                                                         (buffer-string)))
                                     (match-count (how-many (regexp-quote old_string) original-content)))
                                (cond
                                 ((= match-count 0)
                                  "Error: The 'old_string' was not found in the file. No changes made.")
                                 ((> match-count 1)
                                  (format "Error: The 'old_string' is not unique; it appeared %d times. Aborting to prevent incorrect edits." match-count))
                                 (t ; Exactly one match, proceed
                                  (let ((new-content (replace-regexp-in-string (regexp-quote old_string) new_string original-content)))
                                    (if (superchat-tool--confirm-diff expanded-path new-content)
                                        (condition-case err
                                            (progn
                                              (write-region new-content nil expanded-path nil 'nomessage)
                                              (format "File '%s' edited successfully." expanded-path))
                                          (error (format "Error writing file: %s" (error-message-string err))))
                                      "Error: User refused the proposed edit."))))))))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: find-files
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "find-files"
              :description "Recursively finds files matching a glob pattern. Requires user approval."
              :args '((:name "pattern"
                             :type string
                             :description "The glob pattern to match files against, e.g., '*.js' or 'src/**/*.py'."
                             :required t)
                      (:name "path"
                             :type string
                             :description "The directory to start the search from. Defaults to the current directory."
                             :optional t))
              :function (lambda (pattern &optional path)
                          (let* ((search-path (or path default-directory))
                                 (expanded-path (expand-file-name search-path)))
                            (if (not (file-directory-p expanded-path))
                                (format "Error: Path is not a valid directory: %s" expanded-path)
                              (if (y-or-n-p (format "Superchat wants to find files matching '%s' in %S. Allow?" pattern expanded-path))
                                  (let* ((regexp (glob-to-regexp pattern))
                                         (files (directory-files-recursively expanded-path regexp)))
                                    (if files
                                        (string-join files "\n")
                                      (format "No files found matching pattern '%s' in %s." pattern expanded-path)))
                                "Error: User refused to perform the file search."))))
              :category "filesystem"))

(defun superchat-tool--brave-search--curl-fetch (url headers)
  "Fallback to curl when URL retrieval fails. Returns response body as string."
  (let ((curl-binary (executable-find "curl")))
    (unless curl-binary
      (error "No response received from Brave Search API. curl fallback unavailable."))
    (message "Brave Search: url-retrieve-synchronously failed, using curl fallback…")
    (with-temp-buffer
      (let* ((header-args (cl-mapcan (lambda (header)
                                       (list "-H" (format "%s: %s" (car header) (cdr header))))
                                     headers))
             (args (append '("--silent" "--show-error" "--fail" "--max-time" "30")
                           header-args
                           (list url)))
             (exit-code (apply #'call-process curl-binary nil (current-buffer) nil args)))
        (if (and (numberp exit-code) (zerop exit-code))
            (buffer-string)
          (error "curl fallback failed (exit %s): %s"
                 exit-code (buffer-string)))))))

(defun superchat-tool--brave-search--fetch (url headers)
  "Fetch URL using Emacs url.el; fallback to curl when necessary.
HEADERS should be an alist of header names to values."
  (let* ((url-request-method "GET")
         (url-request-extra-headers headers))
    (if-let ((response-buffer (url-retrieve-synchronously url nil nil 30)))
        (unwind-protect
            (with-current-buffer response-buffer
              (goto-char (point-min))
              (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                (error "API request failed - check your API key and network connection"))
              (unless (re-search-forward "\n\n" nil t)
                (error "Brave Search response missing body content."))
              (buffer-substring-no-properties (point) (point-max)))
          (when (buffer-live-p response-buffer)
            (kill-buffer response-buffer)))
      (superchat-tool--brave-search--curl-fetch url headers))))

;;;---------------------------------------------
;;; Tool: web-fetch
;;;---------------------------------------------

(defun superchat-tool--normalize-http-url (url)
  "Return trimmed URL ensuring it starts with http:// or https://.
Signal an error otherwise."
  (let ((trimmed (string-trim (or url ""))))
    (if (string-match-p "\\`https?://" trimmed)
        trimmed
      (error "Invalid URL %s (must start with http:// or https://)" url))))

(defun superchat-tool--http-get (url &optional headers timeout)
  "Fetch URL and return response body as string.
HEADERS is an alist of extra headers, TIMEOUT defaults to 30 seconds."
  (let* ((url-request-method "GET")
         (url-request-extra-headers headers)
         (fetch-timeout (or timeout 30))
         (gnutls-verify-error nil)
         (gnutls-min-prime-bits 1024)
         (buffer (url-retrieve-synchronously url nil nil fetch-timeout)))
    (unless buffer
      (error "No response received from %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (unless (re-search-forward "^HTTP/[0-9.]+ 2[0-9][0-9]" nil t)
            (error "HTTP request failed for %s" url))
          (unless (re-search-forward "\n\n" nil t)
            (error "Response from %s missing body content" url))
          (buffer-substring-no-properties (point) (point-max)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun superchat-tool--fetch-via-jina (url)
  "Fetch URL content via Jina reader proxy and return Markdown string.
Return nil if the proxy fetch fails."
  (condition-case err
      (let* ((jina-url (concat "https://r.jina.ai/" url))
             (headers `(("Accept" . "text/plain; charset=utf-8")
                        ("User-Agent" . ,superchat-tool--default-user-agent))))
        (superchat-tool--http-get jina-url headers))
    (error
     (message "Jina reader fallback failed for %s: %s" url (error-message-string err))
     nil)))

(defun superchat-tool--fetch-direct (url)
  "Fetch URL content directly and return raw body."
  (superchat-tool--http-get url `(("User-Agent" . ,superchat-tool--default-user-agent))))

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "web-fetch"
              :description "Fetches page content from a specific URL. Prefers Markdown via Jina Reader; falls back to raw HTML. Use this ONLY when you have an exact URL to fetch. For searching the web, use web-search instead. Requires user approval for network access."
              :args '((:name "url"
                             :type string
                             :description "The exact URL to fetch content from (must start with http:// or https://)."
                             :required t))
              :function (lambda (url)
                          (if (y-or-n-p (format "Superchat wants to fetch content from URL: %s. Allow?" url))
                              (condition-case err
                                  (let* ((normalized-url (superchat-tool--normalize-http-url url))
                                         (markdown (superchat-tool--fetch-via-jina normalized-url)))
                                    (or markdown
                                        (superchat-tool--fetch-direct normalized-url)))
                                (error (format "Error fetching URL %s: %s" url (error-message-string err))))
                            "Error: User refused the network request."))
              :category "web"))

;;;---------------------------------------------
;;; Tool: web-search (Brave Search API)
;;;---------------------------------------------

(defvar superchat-tool--brave-search-api-key
  (auth-source-pick-first-password :host "api.search.brave.com" :user "superchat")
  "API key for accessing the Brave Search API, retrieved from auth-source.")

(defun superchat-tool--brave-search-query (query)
  "Perform a web search using the Brave Search API with the given QUERY."
  (unless superchat-tool--brave-search-api-key
    (error "Brave Search API key not found. Configure it in your auth-source file (e.g., ~/.authinfo.gpg) with host 'api.search.brave.com' and user 'superchat'."))
  (let* ((url-request-method "GET")
         (url-request-extra-headers `(("X-Subscription-Token" . ,superchat-tool--brave-search-api-key)
                                      ("Accept" . "application/json")
                                      ("User-Agent" . "Emacs/superchat")))
         (request-url (format "https://api.search.brave.com/res/v1/web/search?q=%s"
                              (url-hexify-string query)))
         ;; Relax TLS requirements slightly to play nicer with older Emacs builds.
         (gnutls-verify-error nil)
         (gnutls-min-prime-bits 1024))
    (condition-case err
        (let* ((response-body (superchat-tool--brave-search--fetch request-url url-request-extra-headers))
               (json-object-type 'hash-table)
               (json-array-type 'vector)
               (json-key-type 'string))
          (condition-case json-err
              (json-parse-string response-body)
            ((json-parse-error json-error json-readtable-error)
             (error "Brave Search returned invalid JSON: %s"
                    (error-message-string json-err)))
            (error
             (error "Brave Search returned invalid data: %s"
                    (error-message-string json-err)))))
      (error
       (signal 'error
               (list (format "Brave Search API error: %s"
                             (error-message-string err))))))))


(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "web-search"
              :description "Performs a web search using the Brave Search API and returns a list of results. Requires user approval. Note: Requires valid Brave Search API key."
              :args '((:name "query"
                             :type string
                             :description "The search query."
                             :required t))
              :function (lambda (query)
                          (if (y-or-n-p (format "Superchat wants to perform a web search for: '%s'. Allow?" query))
                              (condition-case err
                                  (progn
                                    ;; Check if API key is configured
                                    (unless superchat-tool--brave-search-api-key
                                      (error "Brave Search API key not configured. Please set it in auth-source."))
                                    
                                    (let* ((json-result (superchat-tool--brave-search-query query))
                                           (web-results (gethash "web" json-result)))
                                      (if (not (and web-results (gethash "results" web-results)))
                                          "No web search results found."
                                        (let ((output "")
                                              (results-array (gethash "results" web-results)))
                                          (dotimes (i (min 5 (length results-array))) ; Limit to top 5 results
                                            (let* ((item (aref results-array i))
                                                   (title (gethash "title" item))
                                                   (url (gethash "url" item))
                                                   (description (gethash "description" item)))
                                              (setq output (concat output
                                                                   (format "### [%d] %s\n" (1+ i) title)
                                                                   (format "- URL: %s\n" url)
                                                                   (format "- Snippet: %s\n\n" description)))))
                                          output))))
                                (error (format "Web search failed: %s\n\nNote: If you see 'json-value-p' or 'HTML' errors, your Brave Search API key may be invalid or missing. Configure it with:\n  Host: api.search.brave.com\n  User: superchat" 
                                               (error-message-string err))))
                            "Error: User refused the web search request."))
              :category "web"))

(provide 'superchat-tools)

;;; superchat-tools.el ends here
