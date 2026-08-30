;;; superchat-tools.el --- Native tools for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:
;; This file defines a set of native Emacs Lisp tools for use with
;; Superchat and llm.el.  Tools declare effects here; the shared Tool
;; Executor owns policy, approval, audit, and invocation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-util)
(require 'auth-source)
(require 'subr-x)
(require 'llm nil t)
;; Generated UI loads on first `render_ui' call; see `superchat-tool-render-ui'.
(defvar superchat-genui-buffer-name)
(declare-function superchat-genui-render-form "superchat-genui" (form))
(require 'superchat-authority)

(declare-function superchat--subagent-run-async "superchat-subagent" (preset-name task context callback &optional depth))
(declare-function superchat--subagent-run-parallel-async "superchat-subagent" (specs callback &optional depth placeholder))
(declare-function superchat--subagent-parse-specs "superchat-subagent" (tasks))
(declare-function superchat--subagent-begin-placeholder "superchat-subagent" (label))
(declare-function superchat--subagent-end-placeholder "superchat-subagent" (placeholder label report))
(declare-function superchat--subagent-set-placeholder "superchat-subagent" (id placeholder))
(declare-function superchat--subagent-registry "superchat-subagent" ())
(declare-function superchat-team-task "superchat-subagent"
                  (action &optional task-id title owner depends-on))
(declare-function superchat-team-message "superchat-subagent"
                  (action &optional to content since-id))
(declare-function superchat-preset-description "superchat-preset" (preset))
(declare-function superchat-workspace-write
                  "superchat-workspace"
                  (content &optional append artifact-id expected-revision
                           workspace-id))
(declare-function superchat-workspace-read
                  "superchat-workspace"
                  (&optional artifact-id workspace-id expected-revision))
(declare-function superchat-workspace-publish
                  "superchat-workspace"
                  (content &optional role workspace-id))
(declare-function superchat-workspace-info
                  "superchat-workspace" (&optional workspace-id))
(defvar read-eval)

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
(declare-function superchat--extend-timeout "superchat"
                  (&optional transport-timer-owner))
(defun superchat-tools--extend-active-transport-wait ()
  "Extend the transport wait owned by the current tool's Runtime Run."
  (when (fboundp 'superchat--extend-timeout)
    (superchat--extend-timeout (superchat-authority-transport-timer-owner))))

(defun superchat-tools--base-dir ()
  "Return the effective `default-directory' for tool operations.
Prefers `superchat-tools-default-directory' when set; falls back
to the buffer-local `default-directory'."
  (or superchat-tools-default-directory default-directory))

(defun superchat-tools--effect (kind resource &optional scope)
  "Return one tool effect descriptor."
  (append (list :effect kind :resource resource)
          (when scope (list :scope scope))))

(defun superchat-tools--effect-arg (args key index)
  "Return named KEY or positional INDEX from tool ARGS."
  (superchat-authority-effect-argument args key index))

(defconst superchat-tools--eval-elisp-read-only-functions
  '(buffer-list buffer-name point buffer-substring-no-properties
    symbol-value boundp fboundp length car cdr nth format concat)
  "Functions accepted by the conservative `eval_elisp' read classifier.

This is deliberately a small whitelist.  A function not in this list keeps
the expression at the broader `process-exec' effect, even when it looks
harmless to a human reader.")

(defconst superchat-tools--eval-elisp-structural-forms
  '(let let* if when unless progn cond lambda quote function and or)
  "Structural forms accepted by the conservative `eval_elisp' classifier.")

(defconst superchat-tools--eval-elisp-rejected-forms
  '(funcall apply eval macroexpand intern symbol-function fset set setq setf)
  "Forms that always keep an `eval_elisp' expression at `process-exec'.")

(defun superchat-tools--eval-elisp-safe-binding-p (binding)
  "Return non-nil when LET BINDING has a safe initializer.

Binding names are not calls; only their optional initializer needs analysis."
  (or (symbolp binding)
      (and (listp binding)
           (symbolp (car binding))
           (or (= (length binding) 1)
               (and (= (length binding) 2)
                    (superchat-tools--eval-elisp-form-read-only-p
                     (cadr binding)))))))

(defun superchat-tools--eval-elisp-form-read-only-p (form)
  "Return non-nil when FORM uses only the read-only Elisp whitelist.

Symbols in value positions are treated as variable/data references.  Only
function or special-form heads are resolved against the whitelist; this
keeps quoted data and ordinary LET bindings from being mistaken for calls."
  (cond
   ((atom form) t)
   ((not (symbolp (car form))) nil)
   (t
    (let ((head (car form))
          (args (cdr form)))
      (cond
       ((memq head superchat-tools--eval-elisp-rejected-forms) nil)
       ((eq head 'quote) (= (length args) 1))
       ((eq head 'function)
        (and (= (length args) 1)
             (or (and (symbolp (car args))
                      (memq (car args)
                            superchat-tools--eval-elisp-read-only-functions))
                 (and (consp (car args))
                      (eq (caar args) 'lambda)
                      (superchat-tools--eval-elisp-form-read-only-p
                       (car args))))))
       ((memq head '(let let*))
        (and (consp args)
             (listp (car args))
             (cl-every #'superchat-tools--eval-elisp-safe-binding-p
                       (car args))
             (cl-every #'superchat-tools--eval-elisp-form-read-only-p
                       (cdr args))))
       ((eq head 'lambda)
        (and (consp args)
             (listp (car args))
             (cl-every #'superchat-tools--eval-elisp-form-read-only-p
                       (cdr args))))
       ((memq head '(if when unless progn and or))
        (and (or (not (memq head '(if)))
                 (>= (length args) 2))
             (cl-every #'superchat-tools--eval-elisp-form-read-only-p args)))
       ((eq head 'cond)
        (cl-every (lambda (clause)
                    (and (consp clause)
                         (cl-every
                          #'superchat-tools--eval-elisp-form-read-only-p
                          clause)))
                  args))
       ((memq head superchat-tools--eval-elisp-read-only-functions)
        (cl-every #'superchat-tools--eval-elisp-form-read-only-p args))
       ((memq head superchat-tools--eval-elisp-structural-forms) nil)
       (t nil))))))

(defun superchat-tools--eval-elisp-read-only-p (expression)
  "Return non-nil when EXPRESSION is safely classifiable as read-only.

The reader never evaluates input.  Malformed input and every construct that
the whitelist cannot prove read-only return nil, which preserves the
broader `process-exec' effect."
  (and (stringp expression)
       (condition-case nil
           (let* ((parsed (let ((read-eval nil)
                                (read-circle nil))
                            (read-from-string expression)))
                  (rest (substring expression (cdr parsed))))
             (and (cl-loop for char across rest
                           always (memq char '(?\s ?\t ?\n ?\r ?\f ?\v)))
                  (superchat-tools--eval-elisp-form-read-only-p
                   (car parsed))))
         (error nil))))

(defun superchat-tools--register-effect-resolver (names resolver)
  "Register RESOLVER for every tool name in NAMES."
  (dolist (name names)
    (superchat-authority-register-effect-resolver name resolver)))

(defun superchat-tools--register-effect-resolvers ()
  "Declare effects for Superchat's built-in tools."
  (superchat-tools--register-effect-resolver
   '("list_buffers")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'buffer))))
  (superchat-tools--register-effect-resolver
   '("read_buffer")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-read 'buffer
       (superchat-tools--effect-arg args "buffer" 0)))))
  (superchat-tools--register-effect-resolver
   '("append_to_buffer" "edit_buffer" "replace_buffer" "render_ui")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-write 'buffer
       (or (superchat-tools--effect-arg args "buffer" 0)
           (superchat-tools--effect-arg args "buffer_name" 0)
           'generated)))))
  (superchat-tools--register-effect-resolver
   '("shell-command" "eshell-command")
   (lambda (args)
     (list
      (superchat-tools--effect
       'process-exec 'process
       (or (superchat-tools--effect-arg args "command" 0)
           'command)))))
  (superchat-tools--register-effect-resolver
   '("eval_elisp")
   (lambda (args)
     (let ((expression
            (superchat-tools--effect-arg args "expression" 0)))
       (list
        (superchat-tools--effect
         (if (superchat-tools--eval-elisp-read-only-p expression)
             'local-read
           'process-exec)
         'emacs expression)))))
  (superchat-tools--register-effect-resolver
   '("workspace_read")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-read 'workspace
       (or (superchat-tools--effect-arg args "artifact_id" 0)
           'current)))))
  (superchat-tools--register-effect-resolver
   '("workspace_info")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'workspace 'current))))
  (superchat-tools--register-effect-resolver
   '("workspace_write" "workspace_publish")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-write 'workspace
       (or (superchat-tools--effect-arg args "artifact_id" 2)
           'current)))))
  (superchat-tools--register-effect-resolver
   '("team_task")
   (lambda (args)
     (let ((action (superchat-tools--effect-arg args "action" 0)))
       (list
        (superchat-tools--effect
         (if (member action '("list" "read" list read))
             'local-read
           'local-write)
         'team 'task)))))
  (superchat-tools--register-effect-resolver
   '("team_message")
   (lambda (args)
     (let ((action (superchat-tools--effect-arg args "action" 0)))
       (list
        (superchat-tools--effect
         (if (member action '("list" "read" list read))
             'local-read
           'local-write)
         'team 'message)))))
  (superchat-tools--register-effect-resolver
   '("delegate_to_subagent" "delegate_to_subagent_parallel")
   (lambda (_args)
     (list (superchat-tools--effect 'runtime-exec 'team 'subagent))))
  (superchat-tools--register-effect-resolver
   '("define_tool")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-write 'tool-definition
       (superchat-tools--effect-arg args "name" 0)))))
  (superchat-tools--register-effect-resolver
   '("apropos" "describe-function" "describe-variable")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'emacs 'introspection))))
  (superchat-tools--register-effect-resolver
   '("sql")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'database 'superchat))))
  (superchat-tools--register-effect-resolver
   '("memory_search" "tool_history" "file_history" "recent_errors")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'tape 'current))))
  (superchat-tools--register-effect-resolver
   '("lsp-references" "lsp-code-actions" "lsp-hover")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-read 'language-server
       (or (superchat-tools--effect-arg args "buffer-name" 0)
           'current-buffer))))))

(superchat-tools--register-effect-resolvers)

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

(defun superchat-tool--append-content (content newline)
  "Return CONTENT exactly as the compatibility append helper writes it."
  (if (eq newline nil) (concat "\n" content) content))

(defun superchat-tool--preview-arguments (request)
  "Preview REQUEST by showing its full arguments, never truncated.
Used for process executions and as the safe fallback for other previews."
  (let* ((name (plist-get request :tool-name))
         (args (plist-get request :args))
         (text (format "%S" args)))
    (with-help-window (format "*Superchat %s Preview*" name)
      (with-current-buffer standard-output
        (insert (format "=== %s ===\n\n%s\n" name text))))))

(defun superchat-tool--buffer-diff-text (before after)
  "Return a full unified diff from BEFORE to AFTER, never truncated."
  (if (equal before after)
      "No textual changes.\n"
    (require 'diff)
    (let ((before-buffer (generate-new-buffer " *superchat-preview-before*"))
          (after-buffer (generate-new-buffer " *superchat-preview-after*"))
          (diff-buffer (generate-new-buffer " *superchat-preview-diff*")))
      (unwind-protect
          (progn
            (with-current-buffer before-buffer (insert before))
            (with-current-buffer after-buffer (insert after))
            (diff-no-select before-buffer after-buffer "-u" t diff-buffer)
            (with-current-buffer diff-buffer
              (buffer-substring-no-properties (point-min) (point-max))))
        (dolist (buffer (list before-buffer after-buffer diff-buffer))
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))))))

(defun superchat-tool--preview-buffer-edit (request)
  "Preview a buffer edit REQUEST as a readable diff, never blocking approval.
Malformed requests and diff failures fall back to the raw argument preview."
  (condition-case _err
      (let* ((name (plist-get request :tool-name))
             (args (plist-get request :args))
             (buffer-name
              (superchat-tools--effect-arg args "buffer_name" 0))
             (text
              (pcase name
                ("edit_buffer"
                 (let ((before
                        (superchat-tools--effect-arg args "old_string" 1))
                       (after
                        (superchat-tools--effect-arg args "new_string" 2)))
                   (unless (and (stringp buffer-name)
                                (stringp before)
                                (stringp after))
                     (error "Malformed buffer edit preview"))
                   (superchat-tool--buffer-diff-text before after)))
                ("replace_buffer"
                 (let ((after
                        (superchat-tools--effect-arg args "content" 1)))
                   (unless (and (stringp buffer-name) (stringp after))
                     (error "Malformed buffer replacement preview"))
                   (if-let* ((buffer (get-buffer buffer-name)))
                       (superchat-tool--buffer-diff-text
                        (with-current-buffer buffer
                          (buffer-substring-no-properties (point-min) (point-max)))
                        after)
                     (format "Buffer %s does not exist; replacement will create it.\n\n%s"
                             buffer-name after))))
                (_ (error "Unknown buffer edit tool")))))
        (with-help-window (format "*Superchat %s Preview: %s*" name buffer-name)
          (with-current-buffer standard-output
            (insert (format "=== %s: %s ===\n\n%s\n" name buffer-name text)))))
    (error
     (condition-case nil
         (superchat-tool--preview-arguments request)
       (error nil)))))

(dolist (name '("shell-command" "eshell-command" "eval_elisp"))
  (superchat-authority-register-approval-preview
   name #'superchat-tool--preview-arguments))

(dolist (name '("edit_buffer" "replace_buffer"))
  (superchat-authority-register-approval-preview
   name #'superchat-tool--preview-buffer-edit))

;;;---------------------------------------------
;;; Tool Implementation Functions
;;;---------------------------------------------

(defun superchat-tool-shell-command (command)
  "Execute COMMAND and return its output."
  (superchat-tools--extend-active-transport-wait)
  (shell-command-to-string command))

(defun superchat-tool-eshell-command (command)
  "Execute COMMAND through Emacs Eshell and return its result.
Eshell commands run from the effective Superchat tool directory, so
built-ins, pipelines, redirection, and external commands all see the same
working directory as the other file-oriented tools."
  (let ((base-dir (superchat-tools--base-dir)))
    (superchat-tools--extend-active-transport-wait)
    (condition-case err
        (progn
          (unless (fboundp 'eshell-command-result)
            (require 'eshell))
          (let* ((default-directory
                   (file-name-as-directory (expand-file-name base-dir)))
                 (status-var (make-symbol "superchat-eshell-status"))
                 (result (eshell-command-result command status-var))
                 (status (symbol-value status-var))
                 (output (cond
                          ((stringp result) (substring-no-properties result))
                          ((null result) "")
                          (t (format "%s" result)))))
            (if (zerop status)
                output
              (format "Error: Eshell command exited with status %s%s"
                      status
                      (if (string-empty-p output)
                          "."
                        (format ": %s" (string-trim-right output)))))))
      (error
       (format "Error: Eshell command failed: %s"
               (error-message-string err))))))

(defun superchat-tool--protected-path-p (path)
  "Return non-nil when PATH is a file no agent tool may write.
The synthesized-tool approval ledger anchors what the user reviewed; an
agent that could write it could stamp its own approvals."
  (and (fboundp 'superchat--syn-approvals-file)
       (string-equal (expand-file-name path)
                     (expand-file-name (superchat--syn-approvals-file)))))

(defun superchat-tool-write-file (path content)
  "Write CONTENT to PATH."
  (if (superchat-tool--protected-path-p path)
      "❌ This file records user approvals and cannot be written by tools."
    (let ((expanded-path (expand-file-name path)))
      (let ((dir (file-name-directory expanded-path)))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t)))
      (with-temp-buffer
        (insert content)
        (write-region (point-min) (point-max) expanded-path nil 'nomessage))
      (format "✅ File '%s' written successfully (%d chars)."
              expanded-path (length content)))))

(defun superchat-tool-append-file (path content &optional newline)
  "Appends CONTENT to the end of PATH."
  (if (superchat-tool--protected-path-p path)
      "❌ This file records user approvals and cannot be written by tools."
    (let* ((expanded-path (expand-file-name path))
           (final-content (superchat-tool--append-content content newline)))
      (let ((dir (file-name-directory expanded-path)))
        (when (and dir (not (file-directory-p dir)))
          (make-directory dir t)))
      (with-temp-buffer
        (insert final-content)
        (write-region (point-min) (point-max) expanded-path t 'nomessage))
      (format "✅ Content appended to '%s' successfully (%d chars added)."
              expanded-path (length final-content)))))

(defun superchat-tool-quick-write (path content)
  "Write small CONTENT to PATH."
  (if (superchat-tool--protected-path-p path)
      "❌ This file records user approvals and cannot be written by tools."
    (let* ((expanded-path (expand-file-name path))
           (content-length (length content))
           (file-exists (file-exists-p expanded-path))
           (action (if file-exists "replace" "create")))
      (if (>= content-length 1000)
          "❌ Content too large for quick-write (use write-file for files >1000 chars)"
        (let ((dir (file-name-directory expanded-path)))
          (when (and dir (not (file-directory-p dir)))
            (make-directory dir t)))
        (with-temp-buffer
          (insert content)
          (write-region (point-min) (point-max) expanded-path nil 'nomessage))
        (format "⚡ Quick %s successful: '%s' (%d chars)"
                action expanded-path content-length)))))

(defun superchat-tool-read-file (path &optional offset limit)
  "Read PATH, optionally a line slice.
OFFSET is the 1-based first line to return and LIMIT the number of
lines; both nil means the whole file.  Slicing lets the model read a
large file piece by piece instead of pulling it into the context."
  (let ((expanded-path (expand-file-name path)))
    (if (not (file-exists-p expanded-path))
        (format "Error: File does not exist at path: %s" expanded-path)
      (superchat-tools--extend-active-transport-wait)
      (if (or offset limit)
          (superchat--read-file-slice expanded-path (or offset 1) limit)
        (with-temp-buffer
          (insert-file-contents expanded-path)
          (buffer-string))))))

(defun superchat--read-file-slice (path start limit)
  "Return lines START..START+LIMIT-1 (1-based) of PATH.
A file larger than `superchat-inline-max-bytes' is streamed through
`sed' when available, so a small slice of a multi-gigabyte file does not
load the whole thing into the long-lived Emacs process.  Smaller files
take the simple in-buffer path, which also reports the total line count."
  (let* ((start (max 1 start))
         (size (or (file-attribute-size (file-attributes path)) 0))
         (big (and (boundp 'superchat-inline-max-bytes)
                   (> size superchat-inline-max-bytes)))
         (sed (and big (executable-find "sed"))))
    (if sed
        (let ((spec (format "%d,%sp" start
                            (if limit (number-to-string (+ (1- start) limit)) "$"))))
          (with-temp-buffer
            (if (zerop (call-process sed nil t nil "-n" spec
                                     (expand-file-name path)))
                (format "[lines %d..%s]\n%s"
                        start (if limit (number-to-string (+ (1- start) limit)) "end")
                        (buffer-string))
              "Error: could not read the requested slice.")))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((total (count-lines (point-min) (point-max))))
          (if (> start total)
              (format "Error: offset %d is beyond the file's %d lines."
                      start total)
            (goto-char (point-min))
            (forward-line (1- start))
            (let ((beg (point)))
              (if limit (forward-line limit) (goto-char (point-max)))
              (format "[lines %d..%d of %d]\n%s"
                      start
                      (min total (if limit (+ (1- start) limit) total))
                      total
                      (buffer-substring beg (point))))))))))

(defun superchat-tool-list-files (&optional path)
  "Lists files and subdirectories in PATH directory."
  (let* ((target-path (or path (superchat-tools--base-dir)))
         (expanded-path (expand-file-name target-path)))
    (if (not (file-directory-p expanded-path))
        (format "Error: Path is not a valid directory: %s" expanded-path)
      (superchat-tools--extend-active-transport-wait)
      (let ((files-and-attrs
             (directory-files-and-attributes expanded-path nil nil nil)))
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
           "\n"))))))

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
    (superchat-tools--extend-active-transport-wait)
    (shell-command-to-string command)))

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
      (superchat-tools--extend-active-transport-wait)
      (let* ((regexp (superchat--glob-to-regexp pattern))
             (files (directory-files-recursively expanded-path regexp)))
        (if files
            (string-join files "\n")
          (format "No files found matching pattern '%s' in %s."
                  pattern expanded-path))))))

(defun superchat-tool-list-buffers ()
  "Return live Emacs buffers as a readable list of lines.
Each line: NAME SIZE major-mode."
  (mapconcat
   (lambda (buffer)
     (with-current-buffer buffer
       (format "%s %d %s"
               (buffer-name buffer)
               (buffer-size buffer)
               (or major-mode "fundamental"))))
   (buffer-list)
   "\n"))

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

(defconst superchat-tool-apropos-max-results 40
  "Maximum number of symbols returned by `superchat-tool-apropos'.")

(defun superchat-tool--apropos-documentation (symbol type)
  "Return the first sentence of SYMBOL's documentation for TYPE."
  (let ((documentation
         (ignore-errors
           (if (equal type "variable")
               (documentation-property symbol 'variable-documentation)
             (documentation symbol t)))))
    (if (and (stringp documentation)
             (not (string-empty-p (string-trim documentation))))
        (let* ((line (car (split-string documentation "\n" t)))
               (sentence-end
                (and line
                     (string-match
                      "[.!?。！？]\\(?:[ \\t]\\|\\'\\)" line))))
          (string-trim
           (if sentence-end
               (substring line 0 (1+ sentence-end))
             line)))
      "(no documentation)")))

(defun superchat-tool-apropos (pattern &optional type)
  "List Emacs symbols matching PATTERN and filtered by TYPE.
TYPE is one of \"command\", \"function\", or \"variable\"; it
defaults to \"command\".  Results are sorted by symbol name and
bounded by `superchat-tool-apropos-max-results'."
  (cond
   ((not (stringp pattern))
    "Error: apropos pattern must be a string.")
   ((not (member (or type "command") '("command" "function" "variable")))
    (format "Error: invalid apropos type %S; use command, function, or variable."
            type))
   (t
    (let ((type (or type "command")))
      (condition-case err
          (progn
            ;; Validate PATTERN before walking the obarray so a malformed
            ;; regexp always returns a clean tool result.
            (string-match-p pattern "")
            (let (matches)
              (mapatoms
               (lambda (symbol)
                 (when (and (string-match-p pattern (symbol-name symbol))
                            (pcase type
                              ("command" (commandp symbol))
                              ("function" (fboundp symbol))
                              ("variable" (boundp symbol))))
                   (push symbol matches))))
              (setq matches
                    (sort matches
                          (lambda (left right)
                            (string< (symbol-name left)
                                     (symbol-name right)))))
              (if (null matches)
                  (format "No apropos matches for %S (type %s)."
                          pattern type)
                (let* ((total (length matches))
                       (shown (cl-subseq
                               matches 0
                               (min total superchat-tool-apropos-max-results)))
                       (result
                        (mapconcat
                         (lambda (symbol)
                           (format "%s — %s"
                                   (symbol-name symbol)
                                   (superchat-tool--apropos-documentation
                                    symbol type)))
                         shown "\n")))
                  (if (> total superchat-tool-apropos-max-results)
                      (concat
                       result
                       (format
                        "\n... %d more matches; narrow pattern to see the rest."
                        (- total superchat-tool-apropos-max-results)))
                    result)))))
        (invalid-regexp
         (format "Error: invalid apropos pattern %S: %s"
                 pattern (error-message-string err)))
        (error
         (format "Error: apropos search failed: %s"
                 (error-message-string err))))))))

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
The expression has full access to the Emacs process, so its registered
effect is `process-exec' and Authority decides whether it may run."
  (condition-case err
      (let ((result (eval (read expression) t)))
        (format "Result: %S" result))
    (error (format "Error evaluating expression: %s" (error-message-string err)))))

(defun superchat-tool-delegate-to-subagent-async (callback preset task &optional context)
  "Asynchronously delegate TASK to sub-agent PRESET.
CALLBACK receives the report string when the sub-agent completes.
A progress placeholder is inserted in the chat buffer immediately
and replaced by the report on completion.  Emacs stays interactive
throughout."
  (if (require 'superchat-subagent nil t)
      (let ((ph (and (fboundp 'superchat--subagent-begin-placeholder)
                     (superchat--subagent-begin-placeholder preset))))
        (let ((id (superchat--subagent-run-async
                   preset task context
                   (lambda (report)
                     (when (fboundp 'superchat--subagent-end-placeholder)
                       (superchat--subagent-end-placeholder ph preset report))
                     (funcall callback report)))))
          (when (fboundp 'superchat--subagent-set-placeholder)
            (superchat--subagent-set-placeholder id ph))))
    (funcall callback "Error: sub-agent module is not loaded.")))

(defun superchat-tool-delegate-to-subagent-parallel-async (callback tasks)
  "Asynchronously delegate multiple TASKS to sub-agents in parallel.
TASKS is a JSON array of objects with keys `preset', `task', and
optional `context'.  CALLBACK receives the aggregated report once
all sub-agents complete.  Requests run as concurrent llm.el calls,
so delegation is genuinely parallel and never blocks Emacs."
  (if (require 'superchat-subagent nil t)
      (let ((specs (superchat--subagent-parse-specs tasks)))
        (if (stringp specs)
            (funcall callback specs)
          (let ((ph (and (fboundp 'superchat--subagent-begin-placeholder)
                         (superchat--subagent-begin-placeholder
                          (format "parallel × %d" (length specs))))))
            (superchat--subagent-run-parallel-async
             specs
             (lambda (report)
               (when (fboundp 'superchat--subagent-end-placeholder)
                 (superchat--subagent-end-placeholder ph "parallel" report))
               (funcall callback report))
             nil ph))))
    (funcall callback "Error: parallel sub-agent module is not loaded.")))

(defun superchat-tool--format-team-task (task)
  "Format one projected team TASK for an LLM tool result."
  (format "%s [%s] owner=%s deps=%s — %s"
          (plist-get task :id)
          (plist-get task :status)
          (or (plist-get task :owner) "-")
          (if-let* ((dependencies (plist-get task :depends-on)))
              (mapconcat #'identity dependencies ",")
            "-")
          (plist-get task :title)))

(defun superchat-tool-team-task
    (action &optional task-id title owner depends-on)
  "Operate on the current team's Tape-backed task list."
  (condition-case err
      (let ((result
             (superchat-team-task
              action task-id title owner depends-on)))
        (if (equal action "list")
            (if result
                (mapconcat #'superchat-tool--format-team-task result "\n")
              "No team tasks.")
          (superchat-tool--format-team-task result)))
    (error (format "Error: %s" (error-message-string err)))))

(defun superchat-tool-team-message
    (action &optional to content since-id)
  "Use the current team's Tape-backed mailbox and decision log."
  (condition-case err
      (let ((result (superchat-team-message action to content since-id)))
        (pcase action
          ("list"
           (if result
               (mapconcat
                (lambda (message)
                  (format "%s %s -> %s: %s"
                          (plist-get message :id)
                          (plist-get message :from)
                          (plist-get message :to)
                          (plist-get message :content)))
                result "\n")
             "No team messages."))
          ("decisions"
           (if result
               (mapconcat
                (lambda (decision)
                  (format "%s %s: %s"
                          (plist-get decision :id)
                          (plist-get decision :from)
                          (plist-get decision :content)))
                result "\n")
             "No team decisions."))
          (_ result)))
    (error (format "Error: %s" (error-message-string err)))))

(defun superchat-tool-define-tool (_name _description _args _body)
  "Placeholder for `define_tool'.

The real function is bound per request by `superchat--syn-bind', which
closes over the in-flight prompt so the new tool can be injected into it.
Reaching this means the request was built without that binding — the tool
would have had nowhere to register what it created."
  "Error: define_tool is not available in this context.")

(defun superchat-tool--render-ui-form (form)
  "Render generated-UI FORM through the single trusted entry point.
`superchat-genui-render-form' validates, expands, and renders as one
unit; no agent code is ever evaluated."
  (superchat-genui-render-form form))

(defun superchat-tool--read-ui-forms (component)
  "Read every top-level form in COMPONENT with restricted reader settings.

Returns a list of forms.  Two model habits are tolerated here instead of
bounced back as errors: a sexp that arrives double-encoded as one string
literal is unwrapped and re-read, and multiple top-level forms are simply
all returned so the caller can stack them."
  (let ((read-eval nil)
        (read-circle nil)
        (pos 0)
        (len (length component))
        (forms nil))
    (while (progn
             (setq pos (or (string-match "[^ \t\n\r\f]" component pos) len))
             (< pos len))
      (let ((parsed (read-from-string component pos)))
        (push (car parsed) forms)
        (setq pos (cdr parsed))))
    (setq forms (nreverse forms))
    ;; Each unwrap consumes one quoting layer, so this terminates.
    (if (and (= (length forms) 1)
             (stringp (car forms))
             (string-prefix-p "(" (string-trim (car forms))))
        (superchat-tool--read-ui-forms (car forms))
      forms)))

(defun superchat-tool-emit-plan (steps &optional summary)
  "Propose STEPS as a reviewable structured plan.
STEPS is a list of (text accept) pairs; the plan is rendered as an org
subtree with checkboxes and recorded on the Tape."
  (if (not (require 'superchat-plan nil t))
      "Error: superchat-plan is not available"
    (superchat-plan-propose
     (if (or (listp steps) (vectorp steps))
         (mapcar (lambda (step)
                   (if (vectorp step) (append step nil) step))
                 steps)
       steps)
     summary)))

(defun superchat--report-plan-progress-spec ()
  "Return the property list describing the report_plan_progress tool.
Shared by the full-surface registry and the plan-scoped attachment in
`superchat--collect-llm-tools'."
  (list :description
        (concat
         "Report progress on the approved plan. updates is a list of "
         "(step-id status) pairs; status is \"in-progress\" when "
         "starting a step, \"done\" when its completion criterion "
         "holds, or \"pending\" to re-open a step. Only for steps in "
         "the approved plan. Example: [[1 \"done\"], [2 \"in-progress\"]].")
        :args (list (list :name "updates"
                          :type 'array
                          :items (list :type 'array
                                       :items (list :type 'string))
                          :description
                          "List of (step-id status) pairs."))
        :function #'superchat-tool-report-plan-progress))

(defun superchat-tool-report-plan-progress (updates)
  "Report progress UPDATES on the session's approved plan."
  (if (not (require 'superchat-plan nil t))
      "Error: superchat-plan is not available"
    (superchat-plan-report-progress
     (if (or (listp updates) (vectorp updates))
         (mapcar (lambda (u)
                   (if (vectorp u) (append u nil) u))
               updates)
       updates))))

(defun superchat-tool-render-ui (component)
  "Render a validated generated-UI COMPONENT in the TextUI buffer.

COMPONENT is a s-expression string supplied by the model.  The reader is
restricted, validation happens before action expansion, and every failure
is returned as a tool result instead of escaping into the chat turn.
Several top-level forms are auto-wrapped in a column flex rather than
rejected — weak models flatten trees far more often than they repair them
from error feedback."
  (cond
   ((not (stringp component))
    "Error: render_ui expects an s-expression string.")
   ((not (require 'superchat-genui nil t))
    "Error: superchat-genui is not available")
   (t
    (condition-case err
        (let* ((forms (superchat-tool--read-ui-forms component))
               (form (cond ((null forms) nil)
                           ((cdr forms)
                            (list :type :flex :direction :column
                                  :children forms))
                           (t (car forms)))))
          (if (null form)
              "Error: render_ui got an empty component string."
            (let ((result (superchat-tool--render-ui-form form)))
              (if (and (cdr forms) (stringp result)
                       (buffer-live-p (get-buffer result)))
                  (format (concat "%s (%d top-level forms were "
                                  "auto-wrapped in a column; emit "
                                  "one form next time)")
                          result (length forms))
                result))))
      (error (format "Error: %s" (error-message-string err)))))))

(defun superchat-tool-workspace-write
    (content &optional append artifact-id expected-revision)
  "Write CONTENT to an Artifact in the current Workspace.
When APPEND is non-nil, append instead of replacing.  ARTIFACT-ID selects
the target; EXPECTED-REVISION prevents stale writes."
  (if (fboundp 'superchat-workspace-write)
      (superchat-workspace-write
       content append artifact-id expected-revision)
    "Error: workspace module is not loaded."))

(defun superchat-tool-workspace-read (&optional artifact-id expected-revision)
  "Return content from ARTIFACT-ID at EXPECTED-REVISION."
  (if (fboundp 'superchat-workspace-read)
      (or (superchat-workspace-read artifact-id nil expected-revision)
          "(workspace is empty)")
    "Error: workspace module is not loaded."))

(defun superchat-tool-workspace-publish (content &optional role)
  "Publish immutable CONTENT as a durable Workspace Artifact with ROLE."
  (if (fboundp 'superchat-workspace-publish)
      (json-encode (superchat-workspace-publish content role))
    "Error: workspace module is not loaded."))

(defun superchat-tool-workspace-info ()
  "Return a summary of the current workspace."
  (if (fboundp 'superchat-workspace-info)
      (superchat-workspace-info)
    "Error: workspace module is not loaded."))

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

(defvar superchat--llm-tool-names-signature nil
  "Snapshot of `superchat-llm-tool-names' used for the cached tool list.")

;; Keep `apropos' in the default surface: it is read-only, capped at 40
;; matches, and discovery is unavailable to the default agent without it.
(defcustom superchat-llm-tool-names
  '("list_buffers" "apropos" "read_buffer" "edit_buffer" "eval_elisp")
  "Built-in llm.el tool names Superchat exposes by default.

The default surface is the five-verb core: enumerate identities,
discover capabilities, read live objects, edit live objects, and ask
the runtime.  Everything else attaches by scope instead of globally:
Capability presets carry their own
`:tools' lists (the code preset declares editing, workspace, and LSP
tools), the team Topology overlay appends the team tools, and
extensions declared with `superchat-define-extension' compose their
tools only into Requests they are active for.  The full library stays
available to any explicit tool list.

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

(defun superchat--subagent-registry-summary ()
  "Return a compact description of callable sub-agent presets."
  (let ((registry (and (superchat--llm-tool-enabled-p "delegate_to_subagent")
                       (require 'superchat-subagent nil t)
                       (superchat--subagent-registry))))
    (if registry
        (mapconcat
         (lambda (entry)
           (format "%s — %s" (car entry)
                   (string-replace
                    "\n" " " (superchat-preset-description (cdr entry)))))
         registry
         "; ")
      "researcher — read-only investigation; executor — autonomous execution; introspector — Emacs introspection")))

(defvar superchat--subagent-registry-signature nil
  "Registry summary captured when the llm.el tool cache was built.")

(defvar superchat--subagent-registry-tool-cache nil
  "Tool list associated with `superchat--subagent-registry-signature'.")

;;;###autoload
(defun superchat-llm-tools-reload ()
  "Rebuild `superchat-llm-tools-list' with the current tool implementations.
Useful after editing tool functions or adding new ones."
  (interactive)
  (unless (fboundp 'llm-make-tool)
    (user-error "llm.el is not loaded — cannot register tools"))
  (let ((subagent-summary (superchat--subagent-registry-summary)))
    (setq superchat--subagent-registry-signature subagent-summary)
    (setq superchat-llm-tools-list
          (delq nil
                (list
         (superchat--maybe-make-llm-tool
          "shell-command"
          :description "Execute a shell command and return its output."
         :args (list (list :name "command"
                            :type 'string
                            :description "The shell command to execute."))
          :function #'superchat-tool-shell-command)

         (superchat--maybe-make-llm-tool
          "eshell-command"
          :description "Execute an Emacs Eshell command and return its output. Use for commands such as pwd, ls, grep, find, git status, pipelines, and redirection; it supports Eshell built-ins, aliases, and external commands."
          :args (list (list :name "command"
                            :type 'string
                            :description "The Eshell command to execute."))
          :function #'superchat-tool-eshell-command)

         (superchat--maybe-make-llm-tool
          "list_buffers"
          :description "List live Emacs buffers with their names, sizes, and major modes. Use before read_buffer when the buffer name is unknown."
          :args nil
          :function #'superchat-tool-list-buffers)
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
          "edit_buffer"
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
          "replace_buffer"
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
          "apropos"
          :description "Find Emacs commands, functions, or variables by name pattern."
          :args (list (list :name "pattern"
                            :type 'string
                            :description "Regular expression or keyword to match.")
                      (list :name "type"
                            :type 'string
                            :description "One of command, function, or variable; defaults to command."
                            :optional t))
          :function #'superchat-tool-apropos)

         (superchat--maybe-make-llm-tool
          "describe-variable"
          :description "Return documentation and current value of an Emacs Lisp variable."
          :args (list (list :name "variable_name"
                            :type 'string
                            :description "Name of the variable to describe."))
          :function #'superchat-tool-describe-variable)

         (superchat--maybe-make-llm-tool
          "eval_elisp"
          :description "Evaluate an Emacs Lisp expression and return the result."
          :args (list (list :name "expression"
                            :type 'string
                            :description "Emacs Lisp expression to evaluate."))
          :function #'superchat-tool-eval-elisp)

         ;; ── Sub-agent delegation tools ──

         (superchat--maybe-make-llm-tool
          "team_task"
          :description "Coordinate shared team work stored on the session Tape. Actions: create (title, optional owner and depends_on JSON array), list, claim (task_id), complete (task_id). Delegated tasks are created and claimed automatically; use list to discover teammate owners and dependencies."
          :args (list (list :name "action"
                            :type 'string
                            :description "One of: create, list, claim, complete.")
                      (list :name "task_id"
                            :type 'string
                            :description "Task id such as T42."
                            :optional t)
                      (list :name "title"
                            :type 'string
                            :description "Task title for create."
                            :optional t)
                      (list :name "owner"
                            :type 'string
                            :description "Optional actor id assigned at create."
                            :optional t)
                      (list :name "depends_on"
                            :type 'string
                            :description "Optional JSON array of prerequisite task ids."
                            :optional t))
          :function #'superchat-tool-team-task)

         (superchat--maybe-make-llm-tool
          "team_message"
          :description "Use the team mailbox and decision log. Actions: send (to and content), list (optional since_id), publish (content), or decisions. Recipients are teammate actor ids shown by team_task, lead, or all."
          :args (list (list :name "action"
                            :type 'string
                            :description "One of: send, list, publish, decisions.")
                      (list :name "to"
                            :type 'string
                            :description "Recipient actor id, lead, or all."
                            :optional t)
                      (list :name "content"
                            :type 'string
                            :description "Message content for send."
                            :optional t)
                      (list :name "since_id"
                            :type 'integer
                            :description "Only list messages after this numeric Tape id."
                            :optional t))
          :function #'superchat-tool-team-message)

         (superchat--maybe-make-llm-tool
          "delegate_to_subagent"
          :description (format "Delegate a task to an isolated sub-agent preset. Available presets: %s. Returns the sub-agent's report."
                               subagent-summary)
          :args (list (list :name "preset"
                            :type 'string
                            :description (format "Sub-agent preset name. Available: %s."
                                                 subagent-summary))
                      (list :name "task"
                            :type 'string
                            :description "The task to delegate to the sub-agent.")
                      (list :name "context"
                            :type 'string
                            :description "Relevant context from the main session."
                            :optional t))
          :async t
          :function #'superchat-tool-delegate-to-subagent-async)

         (superchat--maybe-make-llm-tool
          "delegate_to_subagent_parallel"
          :description (format "Delegate multiple tasks to sub-agents in parallel. Available presets: %s. TASKS is a JSON array of objects with keys preset, task, and optional context. Max concurrent agents is controlled by superchat-subagent-parallel-max."
                               subagent-summary)
          :args (list (list :name "tasks"
                            :type 'string
                            :description "JSON array of {preset, task, context?} objects."))
          :async t
          :function #'superchat-tool-delegate-to-subagent-parallel-async)

         ;; ── Self-extension: the agent writes a tool and uses it ──
         ;; Off by default (not in `superchat-llm-tool-names'), like
         ;; shell-command, write-file and eval-elisp.  Every definition and
         ;; every call is confirmed; see superchat-synthesis.el.

         (superchat--maybe-make-llm-tool
          "define_tool"
          :description "Write a new tool in Emacs Lisp and register it for immediate use. \
Use this ONLY when no existing tool can do what the task needs — the tool becomes callable on \
your next step, in this same run. The body runs inside the user's Emacs and every call asks the \
user for confirmation, so keep it small, safe and specific. Returning a string is best. \
Do NOT use this to draw UI: call the render_ui tool directly for that. The body may call \
Superchat's own functions to compose capabilities — e.g. (superchat-subagent-map CHUNKS QUERY) \
fans a list of text chunks out to parallel sub-agents and returns their reports as a list, for \
data too large for this conversation."
          :args (list (list :name "name"
                            :type 'string
                            :description "Tool name: letters, digits, _ and - (e.g. parse-org-table).")
                      (list :name "description"
                            :type 'string
                            :description "What the tool does, written for you to read later when deciding whether to call it.")
                      (list :name "args"
                            :type 'string
                            :description "JSON array of arguments, e.g. [{\"name\":\"path\",\"type\":\"string\",\"description\":\"file to read\"}]. Types: string, number, integer, boolean. Add \"optional\":true for optional ones. Use [] for none.")
                      (list :name "body"
                            :type 'string
                            :description "Emacs Lisp body. The declared arguments are bound as variables. The last form's value is the result."))
          :function #'superchat-tool-define-tool)

         ;; ── Generative UI ──
         ;; The name is in the default allowlist, but `on-demand' requests
         ;; only attach it when the prompt explicitly names `render_ui'.

         (superchat--maybe-make-llm-tool
          "render_ui"
          :description "Render a validated interface tree in a TextUI buffer. The component must be one s-expression in the whitelisted grammar: (:type :text :value STR), (:type :flex :direction :row|:column :children (ELEMENT...)), (:type :grid :columns N :min-column-width N :children (ELEMENT...)), or (:type button :label STR :action (action NAME :key value...)). Buttons may only use registered actions: (action cancel-agent :id STR) and (action send-to-chat :text STR). Example: (:type :flex :direction :column :children ((:type :text :value \"Which option?\") (:type button :label \"Explain Redux\" :action (action send-to-chat :text \"Explain Redux\")))). Load the render-ui skill (\">render-ui\") for the complete grammar and layout examples. Do not use define_tool to draw UI."
          :args (list (list :name "component"
                            :type 'string
                            :description "Whitelisted interface s-expression to render."))
          :function #'superchat-tool-render-ui)

         ;; ── Structured plans (stage 4) ──

         (superchat--maybe-make-llm-tool
          "emit_plan"
          :description
          (concat
           "Propose a structured execution plan for review. steps must be "
           "a list of (text accept) PAIRS; each step is an array of "
           "exactly two strings: the first says what to do, the second "
           "is a verifiable completion criterion. "
           "Example steps value: "
           "[[\"Edit line 2\", \"line 2 equals the target text\"], "
           "[\"Verify\", \"read line 2 back and compare\"]]. "
           "At most 20 steps. The user reviews checkboxes and approves; "
           "do not start executing until the plan is approved.")
          :args (list (list :name "steps"
                            :type 'array
                            :items (list :type 'array
                                         :items (list :type 'string))
                            :description "List of (text accept) pairs, at most 20.")
                      (list :name "summary"
                            :type 'string
                            :description "One-line plan summary."
                            :optional t))
          :function #'superchat-tool-emit-plan)

         (apply #'superchat--maybe-make-llm-tool
                "report_plan_progress"
                (superchat--report-plan-progress-spec))

         ;; ── Workspace tools (shared region or fallback buffer for multi-agent state) ──

         (superchat--maybe-make-llm-tool
          "workspace_write"
          :description "Write to a buffer-region Artifact in the current Workspace. \
Call workspace_info first and pass its revision when the Artifact already exists; \
stale revisions are rejected. When APPEND is true, content is added at the end."
          :args (list (list :name "content"
                            :type 'string
                            :description "The content to write to the workspace.")
                      (list :name "append"
                            :type 'boolean
                            :description "Append to existing content instead of replacing."
                            :optional t)
                      (list :name "artifact_id"
                            :type 'string
                            :description "Target Artifact ID; omit for the active Artifact."
                            :optional t)
                      (list :name "expected_revision"
                            :type 'string
                            :description "Revision reported by workspace_info; required for an existing Artifact."
                            :optional t))
         :function #'superchat-tool-workspace-write)

         (superchat--maybe-make-llm-tool
          "workspace_publish"
          :description "Publish a new immutable, durable Artifact in the current Workspace. Use this for a child Run's final deliverable; the returned artifact_id and revision are required by later Runs."
          :args (list (list :name "content"
                            :type 'string
                            :description "Complete Artifact content.")
                      (list :name "role"
                            :type 'string
                            :description "Short role such as collector, analyst, or reviewer."
                            :optional t))
          :function #'superchat-tool-workspace-publish)

         (superchat--maybe-make-llm-tool
         "workspace_read"
          :description "Read one Workspace Artifact from the current Workspace. Cross-Run reads require a live resource and the revision reported by workspace_info; only durable Artifacts survive process restart, and the producer Run must have a durable Closure proof."
          :args (list (list :name "artifact_id"
                            :type 'string
                            :description "Artifact ID; omit for the active Artifact."
                            :optional t)
                      (list :name "expected_revision"
                            :type 'string
                            :description "Revision reported by workspace_info; required when consuming an Artifact produced by another Run."
                            :optional t))
          :function #'superchat-tool-workspace-read)

         (superchat--maybe-make-llm-tool
          "workspace_info"
          :description "List current Workspace Artifacts with URI, role, live status, and revision."
          :args nil
          :function #'superchat-tool-workspace-info)

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
    (setq superchat-llm-tools-list
          (append superchat-llm-tools-list
                  (superchat-tools--external-llm-tools)))
    (setq superchat--llm-tool-names-signature
          (copy-tree superchat-llm-tool-names))
    (setq superchat--subagent-registry-tool-cache superchat-llm-tools-list)
    superchat-llm-tools-list))

(defun superchat-get-llm-tools (&optional tool-names)
  "Return the list of llm.el tools registered by Superchat.
Builds the list on first call (when `superchat-llm-tools-list' is nil).
Returns nil if llm.el is not loaded.

When TOOL-NAMES is non-nil, build that request-local tool profile without
changing `superchat-llm-tool-names' or the global tool cache."
  (unless (fboundp 'llm-make-tool)
    (when (fboundp 'superchat--log)
      (superchat--log :warn "llm.el not loaded; tools disabled"))
    (setq superchat-llm-tools-list nil))
  (if tool-names
      (let ((superchat-llm-tool-names tool-names)
            (superchat-llm-tools-list nil)
            (superchat--llm-tool-names-signature nil)
            (superchat--subagent-registry-signature nil)
            (superchat--subagent-registry-tool-cache nil))
        (when (fboundp 'llm-make-tool)
          (let* ((registered (superchat-llm-tools-reload))
                 (resolved
                  (mapcar (lambda (name)
                            (cl-find-if (lambda (tool)
                                          (equal name (llm-tool-name tool)))
                                        registered))
                          tool-names))
                 (missing
                  (cl-loop for name in tool-names
                           for tool in resolved
                           unless tool collect name)))
            (when missing
              (error "Requested llm tool profile is not resolvable: %S"
                     missing))
            resolved)))
    (when (and superchat-llm-tools-list
               (not (equal superchat--llm-tool-names-signature
                           superchat-llm-tool-names)))
      (setq superchat-llm-tools-list nil))
    (when (and superchat-llm-tools-list
               superchat--subagent-registry-signature
               (eq superchat-llm-tools-list
                   superchat--subagent-registry-tool-cache)
               (superchat--llm-tool-enabled-p "delegate_to_subagent")
               (not (equal superchat--subagent-registry-signature
                           (superchat--subagent-registry-summary))))
      (setq superchat-llm-tools-list nil))
    (or superchat-llm-tools-list
        (and (fboundp 'llm-make-tool)
             (superchat-llm-tools-reload)))))

;; ── Plugin tool registration ──────────────────────────────────

(defconst superchat-tools--builtin-tool-names
  '("shell-command" "eshell-command" "list_buffers" "read_buffer"
    "append_to_buffer" "edit_buffer" "replace_buffer"
    "describe-function" "apropos" "describe-variable" "eval_elisp"
    "team_task" "team_message" "delegate_to_subagent"
    "delegate_to_subagent_parallel" "define_tool" "render_ui" "emit_plan"
    "report_plan_progress" "workspace_write" "workspace_publish"
    "workspace_read" "workspace_info" "sql" "memory_search" "tool_history"
    "file_history" "recent_errors" "lsp-references" "lsp-code-actions"
    "lsp-hover")
  "Built-in tool names reserved by `superchat-llm-tools-reload'.")

(defvar superchat-tools--external-registry nil
  "Alist of (NAME . SPEC) for tools contributed by plugins.
SPEC is the keyword plist accepted by `superchat-register-tool'.")

;;;###autoload
(cl-defun superchat-register-tool
    (name &key function description args async effects approval-preview)
  "Register plugin tool NAME on the model tool surface.

FUNCTION implements the tool; with ASYNC non-nil it receives a
callback as its first argument.  DESCRIPTION and ARGS follow
`llm-make-tool' conventions — ARGS is a list of
\(:name .. :type .. :description .. [:optional t]) plists.

EFFECTS is mandatory because the base Authority fold classifies resolved
Effects under Attendance instead of branching directly on the tool name.
Pass either a resolver function called with the normalized argument list
and returning effect descriptors
\((:effect KIND :resource RESOURCE [:scope SCOPE]) ...), or a static
list of such descriptors.  APPROVAL-PREVIEW, when given, is registered
with `superchat-authority-register-approval-preview'.

The name still selects the resolver and can affect later effective-policy
compatibility paths; requiring EFFECTS does not make the whole executor
name-blind.

NAME must not be one of Superchat's built-in tool names; plugin tools use a
separate namespace so unregistering one cannot remove a built-in Authority
resolver or approval preview.

Loading the plugin is the user's opt-in: a registered tool is exposed
whenever tools are attached, every execution still passes through
`superchat-tool-execute' (Authority, approval, Tape audit), and an
explicit request-local tool profile includes it only when it names it.
Registering NAME again replaces the previous definition.  Returns NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "Tool name must be a non-empty string"))
  (when (member name superchat-tools--builtin-tool-names)
    (error "Tool %s is built-in; plugin tools must use a distinct name" name))
  (unless (functionp function)
    (error "Tool %s requires a callable :function" name))
  (unless (and (stringp description) (not (string-empty-p description)))
    (error "Tool %s requires a :description" name))
  (unless effects
    (error "Tool %s must declare :effects for the base Authority fold" name))
  (superchat-authority-register-effect-resolver
   name
   (if (functionp effects) effects (lambda (_args) effects)))
  (when approval-preview
    (superchat-authority-register-approval-preview name approval-preview))
  (setf (alist-get name superchat-tools--external-registry nil nil #'equal)
        (list :function function :description description
              :args args :async async))
  ;; Drop the built tool list so the next collection sees NAME.
  (setq superchat-llm-tools-list nil)
  name)

(defun superchat-unregister-tool (name)
  "Remove plugin tool NAME and its Authority registrations.

Only a NAME present in `superchat-tools--external-registry' is treated as a
plugin tool.  Built-in tools use the same Authority resolver and preview
tables, so calling this function for a name with no plugin binding leaves
the built-in entries untouched.  Returns non-nil when a plugin binding was
removed."
  (when (assoc name superchat-tools--external-registry)
    (setf (alist-get name superchat-tools--external-registry
                     nil 'remove #'equal)
          nil)
    (superchat-authority-unregister-effect-resolver name)
    (superchat-authority-unregister-approval-preview name)
    (setq superchat-llm-tools-list nil)
    t))

(defun superchat-tools--external-llm-tools ()
  "Build llm.el tool structs for exposed plugin-registered tools.
A tool claimed by a declared extension stays off the surface unless the
current allowlist names it (an explicit request-local profile, or a
deliberate user opt-in) — activation composition is how it normally
arrives."
  (when (fboundp 'llm-make-tool)
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((name (car entry))
                   (spec (cdr entry)))
               (unless (and (not (eq superchat-llm-tool-names 'all))
                            (not (member name superchat-llm-tool-names))
                            (fboundp 'superchat-extension-claimed-tool-p)
                            (superchat-extension-claimed-tool-p name))
                 (llm-make-tool
                  :name name
                  :description (plist-get spec :description)
                  :args (plist-get spec :args)
                  :async (plist-get spec :async)
                  :function (plist-get spec :function)))))
           (reverse superchat-tools--external-registry)))))

(provide 'superchat-tools)

;;; superchat-tools.el ends here
