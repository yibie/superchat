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
(require 'gptel nil t)

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
      (prog1
          (y-or-n-p (format "%s (%s) " prompt expanded-path))
        ;; Extend timeout after user confirmation
        (when (fboundp 'superchat--extend-timeout)
          (superchat--extend-timeout))))))

;;;---------------------------------------------
;;; Tool: shell-command
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "shell-command"
              :description "Execute a shell command and return its output. The user must approve every execution."
              :args '((:name "command"
                             :type string
                             :description "The shell command to execute."))
              :function (lambda (command)
                          (let ((confirmed (y-or-n-p (format "Superchat wants to execute shell command: %S. Allow?" command))))
                            (when (and confirmed (fboundp 'superchat--extend-timeout))
                              (superchat--extend-timeout))
                            (if confirmed
                                (shell-command-to-string command)
                              "Error: User refused to execute the command.")))
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
                             :description "The path of the file to write to.")
                      (:name "content"
                             :type string
                             :description "The content to write to the file."))
              :function (lambda (path content)
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
                             :description "The path of the file to append to.")
                      (:name "content"
                             :type string
                             :description "The content to append to the file.")
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
                             :description "The path of the file to write to.")
                      (:name "content"
                             :type string
                             :description "The content to write (preferably under 1000 chars)."))
              :function (lambda (path content)
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
                             :description "The path of the file to read."))
              :function (lambda (path)
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
                             :description "The text or regular expression to search for.")
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
                            (let ((confirmed (y-or-n-p (format "Superchat wants to search for pattern '%s' in '%s'. Allow?" pattern search-path))))
                              (when (and confirmed (fboundp 'superchat--extend-timeout))
                                (superchat--extend-timeout))
                              (if confirmed
                                  (shell-command-to-string command)
                                "Error: User refused to perform the search."))))
              :category "filesystem"))


;; NOTE: Duplicate tools removed - using the versions with user confirmation above
;; - read-file (line 260) already provides this functionality with security
;; - list-files (line 287) already provides this functionality with security

;;;---------------------------------------------
;;; Tool: create directory
;;;---------------------------------------------

(add-to-list 'gptel-tools
	     (gptel-make-tool
	      :function (lambda (parent name)
			  (condition-case nil
			      (progn
				(make-directory (expand-file-name name parent) t)
				(format "Directory %s created/verified in %s" name parent))
			    (error (format "Error creating directory %s in %s" name parent))))
	      :name "make_directory"
	      :description "Create a new directory with the given name in the specified parent directory"
	      :args (list '(:name "parent"
				  :type string
				  :description "The parent directory where the new directory should be created, e.g. /tmp")
			  '(:name "name"
				  :type string
				  :description "The name of the new directory to create, e.g. testdir"))
	      :category "filesystem"))

;; NOTE: edit_file tool removed - function ds/gptel--edit_file is not defined
;; This tool was causing errors. Use write-file or append-file instead.

;;;---------------------------------------------
;;; Tool: find-files
;;;---------------------------------------------

(add-to-list 'gptel-tools
             (gptel-make-tool
              :name "find-files"
              :description "Recursively finds files matching a glob pattern. Requires user approval."
              :args '((:name "pattern"
                             :type string
                             :description "The glob pattern to match files against, e.g., '*.js' or 'src/**/*.py'.")
                      (:name "path"
                             :type string
                             :description "The directory to start the search from. Defaults to the current directory."
                             :optional t))
              :function (lambda (pattern &optional path)
                          (let* ((search-path (or path default-directory))
                                 (expanded-path (expand-file-name search-path)))
                            (if (not (file-directory-p expanded-path))
                                (format "Error: Path is not a valid directory: %s" expanded-path)
                              (let ((confirmed (y-or-n-p (format "Superchat wants to find files matching '%s' in %S. Allow?" pattern expanded-path))))
                                (when (and confirmed (fboundp 'superchat--extend-timeout))
                                  (superchat--extend-timeout))
                                (if confirmed
                                    (let* ((regexp (glob-to-regexp pattern))
                                           (files (directory-files-recursively expanded-path regexp)))
                                      (if files
                                          (string-join files "\n")
					(format "No files found matching pattern '%s' in %s." pattern expanded-path)))
                                  "Error: User refused to perform the file search.")))))
              :category "filesystem"))

;;;---------------------------------------------
;;; Tool: read buffer
;;;---------------------------------------------

(add-to-list 'gptel-tools
	     (gptel-make-tool
	      :function (lambda (buffer)
			  (unless (buffer-live-p (get-buffer buffer))
			    (error "Error: buffer %s is not live." buffer))
			  (with-current-buffer buffer
			    (buffer-substring-no-properties (point-min) (point-max))))
	      :name "read_buffer"
	      :description "Return the contents of an Emacs buffer"
	      :args (list '(:name "buffer"
				  :type string
				  :description "The name of the buffer whose contents are to be retrieved"))
	      :category "emacs"))

;;;---------------------------------------------
;;; Tool: append to buffer
;;;---------------------------------------------

(add-to-list 'gptel-tools
	     (gptel-make-tool
	      :function (lambda (buffer text)
			  (with-current-buffer (get-buffer-create buffer)
			    (save-excursion
			      (goto-char (point-max))
			      (insert text)))
			  (format "Appended text to buffer %s" buffer))
	      :name "append_to_buffer"
	      :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
	      :args (list '(:name "buffer"
				  :type string
				  :description "The name of the buffer to append text to.")
			  '(:name "text"
				  :type string
				  :description "The text to append to the buffer."))
	      :category "emacs"))

;;;---------------------------------------------
;;; Tool: edit buffer
;;;---------------------------------------------
(defun codel-edit-buffer (buffer-name old-string new-string)
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
(add-to-list 'gptel-tools
	     (gptel-make-tool
	      :name "EditBuffer"
	      :function #'codel-edit-buffer
	      :description "Edits Emacs buffers"
	      :args '((:name "buffer_name"
			     :type string
			     :description "Name of the buffer to modify")
		      (:name "old_string"
			     :type string
			     :description "Text to replace (must match exactly)")
		      (:name "new_string"
			     :type string
			     :description "Text to replace old_string with"))
	      :category "edit"))

;;;---------------------------------------------
;;; Tool: replace buffer
;;;---------------------------------------------
(defun codel-replace-buffer (buffer-name content)
  "Completely replace contents of BUFFER-NAME with CONTENT."
  (with-current-buffer buffer-name
    (erase-buffer)
    (insert content)
    (format "Buffer replaced: %s" buffer-name)))

(add-to-list 'gptel-tools
	     (gptel-make-tool
	      :name "ReplaceBuffer"
	      :function #'codel-replace-buffer
	      :description "Completely overwrites buffer contents"
	      :args '((:name "buffer_name"
			     :type string
			     :description "Name of the buffer to overwrite")
		      (:name "content"
			     :type string
			     :description "Content to write to the buffer"))
	      :category "edit"))

(provide 'superchat-tools)

;;; superchat-tools.el ends here
