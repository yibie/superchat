;;; superchat-tools.el --- Native tools for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:
;; This file defines a set of native Emacs Lisp tools for use with
;; Superchat and llm.el.  Tools declare effects here; the shared Tool
;; Executor owns policy, approval, audit, and invocation.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'json)
(require 'url)
(require 'url-util)
(require 'auth-source)
(require 'subr-x)
(require 'llm nil t)
;; Generated UI loads on the first `render_ui' call; see `superchat-tool-render-ui'.
(defvar superchat-genui-buffer-name)
(declare-function superchat-genui-render-form "superchat-genui" (form))
(require 'superchat-authority)
(require 'superchat-sandbox)
(defvar superchat--tool-execution-context)
(declare-function superchat-runtime--trusted-input-operation-p
                  "superchat-runtime" (context function async))

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
(declare-function superchat-workspace-current-id "superchat-workspace" ())
(declare-function superchat-workspace--workspace "superchat-workspace"
                  (workspace-id &optional create))
(declare-function superchat-workspace--target-artifact "superchat-workspace"
                  (workspace artifact-id))
(declare-function superchat-workspace--fallback-buffer-name "superchat-workspace"
                  (workspace-id))
(declare-function superchat-workspace--file-adapter-p "superchat-workspace" (artifact))
(declare-function superchat-workspace--artifact-buffer "superchat-workspace" (artifact))
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
(declare-function superchat-db-tape-search-prioritized "superchat-db"
                  (query current-session-id limit &optional foreign-eligible-only))
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

Reader object syntax is rejected before reading: constructing a hash
table literal can call a user-defined hash test.  This intentionally
includes matching text inside strings or comments.  Malformed input and
every construct the whitelist cannot prove read-only return nil, keeping
the broader `process-exec' effect."
  (and (stringp expression)
       (not (string-match-p "#[sS]" expression))
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
     ;; Always include `:scope', even when nil: a nil scope means the
     ;; buffer argument was missing or malformed, which must ask like any
     ;; other unresolvable single-buffer read, not be mistaken for
     ;; `list_buffers''s deliberately scope-less "every buffer" effect.
     (list
      (list :effect 'local-read :resource 'buffer
            :scope (superchat-tools--effect-arg args "buffer" 0)))))
  (superchat-tools--register-effect-resolver
   '("append_to_buffer" "edit_buffer" "replace_buffer" "render_ui")
   (lambda (args)
     (list
      (superchat-tools--effect
       'local-write 'buffer
       (or (superchat-tools--effect-arg args "buffer" 0)
           (superchat-tools--effect-arg args "buffer_name" 0)
           'generated)))))
  ;; While Authority still defines which names are selectable, the two file
  ;; tools need a resolver of their own; they exist only with the levels on.
  (when superchat--permission-levels-enabled
    (superchat-tools--register-effect-resolver
     '("save_buffer")
     (lambda (args)
       (list (superchat-tools--effect
              'local-write 'buffer
              (or (superchat-tools--effect-arg args "buffer_name" 0) 'generated)))))
    (superchat-tools--register-effect-resolver
     '("write_file")
     (lambda (args)
       (list (superchat-tools--effect
              'local-write 'file
              (or (superchat-tools--effect-arg args "path" 0) 'generated))))))
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
   '("apropos" "describe-function")
   (lambda (_args)
     (list (superchat-tools--effect 'local-read 'emacs 'introspection))))
  (superchat-tools--register-effect-resolver
   '("describe-variable")
   ;; Not on `superchat-authority-unprompted-read-tools' by default: unlike
   ;; documentation lookup, this returns the variable's live value, so it
   ;; keeps its pre-S07 `ask' decision.
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


(declare-function llm-make-tool "llm")

(eval-when-compile
  (defvar gnutls-verify-error)
  (defvar gnutls-min-prime-bits))

(defconst superchat-tool--default-user-agent
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_0) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
  "Default user-agent used for outbound HTTP requests made by Superchat tools.")

;;;---------------------------------------------
;;; Helper Functions
;;;---------------------------------------------

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

(defun superchat-tool--preview-buffer-list (request)
  "Preview a `list_buffers' REQUEST, showing only the live buffer count.
Never prints a buffer name, so a sensitive buffer cannot leak into this
preview even when its own sensitivity match is why the call is asking."
  (let* ((name (plist-get request :tool-name))
         (count (length (buffer-list)))
         (sensitive
          (superchat-authority--sensitive-read-p
           (list (superchat-tools--effect 'local-read 'buffer)))))
    (with-help-window (format "*Superchat %s Preview*" name)
      (with-current-buffer standard-output
        (insert (format "=== %s ===\n\n%d live buffer%s%s\n"
                         name count (if (= count 1) "" "s")
                         (if sensitive
                             "\n(names withheld: sensitive buffers present)"
                           "")))))))

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

(defun superchat-tool-list-buffers ()
  "Return live buffers, one line per buffer: quoted NAME, SIZE, major mode.
NAME is an Emacs Lisp string; spaces are significant and newlines escaped.
Pass the decoded name to `superchat-tool-read-buffer'."
  (let ((print-escape-newlines t))
    (mapconcat
     (lambda (buffer)
       (with-current-buffer buffer
         (format "%S %d %s"
                 (buffer-name buffer)
                 (buffer-size buffer)
                 (or major-mode "fundamental"))))
     (buffer-list)
     "\n")))

(defun superchat-tool-read-buffer (buffer)
  "Return the contents of BUFFER."
  (unless (buffer-live-p (get-buffer buffer))
    (error "Error: buffer %s is not live." buffer))
  (with-current-buffer buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defun superchat-tools--write-evidence (target policy layer)
  "Return the evidence lines a direct write appends to its result.
TARGET is the resolved buffer target, POLICY the sandbox policy the write ran
under, and LAYER says how far the change went."
  (concat
   (format "\nTarget: buffer %s%s%s"
           (plist-get target :name)
           (if (equal (plist-get target :name) (plist-get target :given))
               ""
             (format " (base buffer of %s)" (plist-get target :given)))
           (if (plist-get target :file)
               (format ", visiting %s" (plist-get target :file))
             ", not visiting a file"))
   (format "\nLayer: %s" layer)
   (format "\nChecks: sandbox mode %s; the target was checked before the change. Nothing was read back."
           (plist-get policy :mode))))

(defun superchat-tools--edit-buffer-plan (buffer-name old-string new-string)
  "Return what replacing OLD-STRING by NEW-STRING in BUFFER-NAME would do.
The result is (:final TEXT) when nothing is to be written -- the text is the
tool's answer -- or (:ready t) when exactly one replacement is due.  Nothing
is modified."
  (if (string-empty-p old-string)
      (list :final "Error: old_string must be nonempty; read the buffer and supply unique text.")
    (with-current-buffer buffer-name
      (let ((case-fold-search nil)
            (count 0))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward old-string nil t)
            (setq count (1+ count))
            ;; Count overlapping occurrences too (e.g. ana in banana).
            (goto-char (1+ (match-beginning 0)))))
        (cond
         ((zerop count)
          (list :final (format "Error: Could not find text to replace in buffer %s" buffer-name)))
         ((> count 1)
          (list :final (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)))
         ((equal old-string new-string)
          (list :final (format "No change: old_string and new_string are identical in buffer %s." buffer-name)))
         (t (list :ready t)))))))

(defun superchat-tools--edit-buffer-apply (buffer-name old-string new-string)
  "Replace the one OLD-STRING in BUFFER-NAME by NEW-STRING; return the answer."
  (with-current-buffer buffer-name
    (let ((case-fold-search nil))
      (save-excursion
        (goto-char (point-min))
        (search-forward old-string)
        (replace-match new-string t t))))
  (format "Replaced 1 occurrence in buffer %s. This tool does not save or evaluate the buffer." buffer-name))

(defun superchat-tool-append-to-buffer (buffer text &optional sandbox-permissions justification)
  "Append TEXT to BUFFER, creating it when it does not exist.
SANDBOX-PERMISSIONS and JUSTIFICATION request a one-shot wider sandbox mode."
  (cl-flet ((append-text (buffer text)
              (with-current-buffer (get-buffer-create buffer)
                (save-excursion
                  (goto-char (point-max))
                  (insert text)))
              (format "Appended text to buffer %s" buffer)))
    (if (not superchat--permission-levels-enabled)
        (append-text buffer text)
      (superchat-sandbox-run-direct-write
       :tool-name "append_to_buffer" :args (list buffer text)
       :permissions sandbox-permissions :justification justification
       :resolve (lambda (args)
                  (list :target (superchat-sandbox-buffer-target (nth 0 args))))
       :perform (lambda (args _resolved policy)
                  (concat (append-text (nth 0 args) (nth 1 args))
                          (superchat-tools--write-evidence
                           (superchat-sandbox-buffer-target (nth 0 args)) policy
                           "in memory only; the buffer was not saved")))))))

(defun superchat-tool-edit-buffer (buffer-name old-string new-string
                                               &optional sandbox-permissions justification)
  "Replace one exact occurrence of OLD-STRING in BUFFER-NAME.
OLD-STRING must be nonempty and occur exactly once in the accessible
buffer text.  Identical replacement text returns a no-change result.
This edits the live buffer; it does not save or evaluate its contents.
SANDBOX-PERMISSIONS and JUSTIFICATION request a one-shot wider sandbox mode;
the sandbox is consulted only when a replacement is about to happen."
  (if (not superchat--permission-levels-enabled)
      (let ((plan (superchat-tools--edit-buffer-plan buffer-name old-string new-string)))
        (or (plist-get plan :final)
            (superchat-tools--edit-buffer-apply buffer-name old-string new-string)))
    (superchat-sandbox-run-direct-write
     :tool-name "edit_buffer" :args (list buffer-name old-string new-string)
     :permissions sandbox-permissions :justification justification
     :resolve (lambda (args)
                (let ((plan (apply #'superchat-tools--edit-buffer-plan args)))
                  (if (plist-get plan :final)
                      plan
                    (list :target (superchat-sandbox-buffer-target (nth 0 args))))))
     :perform (lambda (args resolved policy)
                (concat (apply #'superchat-tools--edit-buffer-apply args)
                        (superchat-tools--write-evidence
                         (plist-get resolved :target) policy
                         "in memory only; the buffer was not saved or evaluated"))))))

(defun superchat-tool-replace-buffer (buffer-name content
                                                  &optional sandbox-permissions justification)
  "Completely replace contents of BUFFER-NAME with CONTENT.
SANDBOX-PERMISSIONS and JUSTIFICATION request a one-shot wider sandbox mode."
  (cl-flet ((replace-all (buffer-name content)
              (with-current-buffer buffer-name
                (erase-buffer)
                (insert content)
                (format "Buffer replaced: %s" buffer-name))))
    (if (not superchat--permission-levels-enabled)
        (replace-all buffer-name content)
      (superchat-sandbox-run-direct-write
       :tool-name "replace_buffer" :args (list buffer-name content)
       :permissions sandbox-permissions :justification justification
       :resolve (lambda (args)
                  ;; A missing buffer signals here, before anybody is asked.
                  (with-current-buffer (nth 0 args)
                    (list :target (superchat-sandbox-buffer-target (nth 0 args)))))
       :perform (lambda (args resolved policy)
                  (concat (replace-all (nth 0 args) (nth 1 args))
                          (superchat-tools--write-evidence
                           (plist-get resolved :target) policy
                           "in memory only; the buffer was not saved")))))))

(defun superchat-tool-save-buffer (buffer-name &optional sandbox-permissions justification)
  "Save the file BUFFER-NAME visits and report where it went.
An indirect or narrowed buffer saves the whole file its base buffer visits.
A buffer without a file is an error: this tool never chooses a file name.
The whole buffer is saved, including modifications that were already unsaved
before this call.  SANDBOX-PERMISSIONS and JUSTIFICATION request a one-shot
wider sandbox mode.  The buffer's state is read again after an approval, so
what is saved and reported is the state at the time of the write."
  (superchat-sandbox-run-direct-write
   :tool-name "save_buffer" :args (list buffer-name)
   :permissions sandbox-permissions :justification justification
   :resolve
   (lambda (args)
     (let* ((target (superchat-sandbox-buffer-target (nth 0 args)))
            (base (plist-get target :buffer))
            (file (plist-get target :file)))
       (cond
        ((null base)
         (list :final (format "Error: buffer %s is not live." (nth 0 args))))
        ((null file)
         (list :final (format "Error: buffer %s is not visiting a file; save_buffer does not choose file names."
                              (plist-get target :name))))
        ((not (buffer-modified-p base))
         (list :final (format "No change: buffer %s has no unsaved modifications; %s was not written."
                              (plist-get target :name) file)))
        ((not (verify-visited-file-modtime base))
         (list :final (format "Error: %s changed on disk since buffer %s read it; nothing was saved."
                              file (plist-get target :name))))
        ;; The file path decides, and the evidence names the buffer.
        (t (list :path file :buffer-target target)))))
   :perform
   (lambda (_args resolved policy)
     (let* ((target (plist-get resolved :buffer-target))
            (file (plist-get resolved :path)))
       (with-current-buffer (plist-get target :buffer)
         (let ((inhibit-message t))
           (basic-save-buffer)))
       (concat
        (format "Saved buffer %s to %s (%d bytes)."
                (plist-get target :name) file
                (or (file-attribute-size (file-attributes file)) 0))
        " The whole buffer was saved, including any modifications that were already unsaved before this call."
        (superchat-tools--write-evidence
         target policy "written to disk; nothing was evaluated or reloaded"))))))

(defun superchat-tool-write-file (path content &optional sandbox-permissions justification)
  "Write CONTENT to the file PATH when no live buffer visits it.
A file some buffer visits is never written here: the buffer's name is
returned so the caller edits and saves that buffer instead.  The parent
directory must exist.  SANDBOX-PERMISSIONS and JUSTIFICATION request a
one-shot wider sandbox mode.  Whether a buffer visits the file is checked
again after an approval, immediately before the write."
  (superchat-sandbox-run-direct-write
   :tool-name "write_file" :args (list path content)
   :permissions sandbox-permissions :justification justification
   :resolve
   (lambda (args)
     (let* ((file (expand-file-name (nth 0 args) (superchat-tools--base-dir)))
            (visiting (find-buffer-visiting file)))
       (cond
        (visiting
         (list :final (format "Error: %s is visited by buffer %s; change it with edit_buffer and write it with save_buffer. Nothing was written."
                              file (buffer-name visiting))))
        ((file-directory-p file)
         (list :final (format "Error: %s is a directory." file)))
        ((not (file-directory-p (file-name-directory file)))
         (list :final (format "Error: directory %s does not exist; write_file does not create directories."
                              (file-name-directory file))))
        (t (list :path file)))))
   :perform
   (lambda (args resolved policy)
     (let* ((file (plist-get resolved :path))
            (existed (file-exists-p file))
            (coding-system-for-write 'utf-8-unix))
       (write-region (nth 1 args) nil file nil 'silent)
       (format "%s %s (%d bytes).\nTarget: file %s, visited by no buffer\nLayer: written to disk; nothing was loaded or evaluated\nChecks: sandbox mode %s; the path and its visitors were checked immediately before writing. Nothing was read back."
               (if existed "Replaced file" "Created file") file
               (or (file-attribute-size (file-attributes file)) 0)
               file
               (plist-get policy :mode))))))

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

(defun superchat-tools--read-one-expression (expression)
  "Read one complete Elisp EXPRESSION before calling eval on it.
Allow trailing whitespace and line comments.  Reject extra forms or
invalid trailing input before the caller can evaluate the first form.
Reading can construct objects and invoke their callbacks, so call this
only after Authority has allowed the expression."
  (let* ((parsed (let ((read-eval nil)
                       (read-circle nil))
                   (read-from-string expression)))
         (form (car parsed)))
    (with-temp-buffer
      (insert (substring expression (cdr parsed)))
      (goto-char (point-min))
      (skip-chars-forward " \t\n\r\f\v")
      (while (eq (char-after) ?\;)
        (forward-line 1)
        (skip-chars-forward " \t\n\r\f\v"))
      (unless (eobp)
        (error "Expected exactly one expression; wrap multiple forms in progn")))
    form))

(defconst superchat-tool-eval-error-max-chars 1024
  "Maximum characters in an eval error's details, including truncation text.
The fixed tool-error prefix is additional.  This bounds returned diagnostic
text, not execution, reader effects or successful evaluation values.")

(defun superchat-tools--eval-error-result (err)
  "Render ERR as a bounded, recognizable native eval tool error."
  (let* ((print-length 12)
         (print-level 4)
         (print-circle t)
         (details (substring-no-properties (error-message-string err)))
         (marker "\n[Error details truncated]"))
    (when (> (length details) superchat-tool-eval-error-max-chars)
      (setq details
            (concat (substring details 0
                               (- superchat-tool-eval-error-max-chars
                                  (length marker)))
                    marker)))
    (concat "Error: evaluating expression: " details)))

(defun superchat-tool-eval-elisp (expression)
  "Evaluate one complete Elisp EXPRESSION and return its value as a string.
Wrap multiple forms in progn or let.  Parsing finishes before eval runs;
an error does not roll back effects of reading or evaluating the expression.
The returned value is not verification of the user's requested outcome.
The expression has full access to the Emacs process.  Its registered
effect is `local-read' only when the conservative classifier proves the
expression read-only (it can still call `symbol-value' or
`buffer-substring-no-properties' and expose process data); every other
expression is `process-exec'.  Either way Authority decides whether it
may run, and it asks in `ask' mode regardless, because `eval_elisp' is
never in `superchat-authority-unprompted-read-tools'."
  (condition-case err
      (let ((result (eval (superchat-tools--read-one-expression expression) t)))
        (format "Result: %S" result))
    (error (superchat-tools--eval-error-result err))))

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
   (t
    (require 'superchat-genui)
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

(defun superchat-tools--workspace-write-target (args)
  "Resolve what `workspace_write' with ARGS would change, for the sandbox.
A file-backed Artifact is judged by its path and a region Artifact by the
buffer holding the region, which can be any buffer the user marked.  Without
an Artifact the write lands in the Workspace's fallback buffer, and the
buffer judged is the one that write will reuse: whatever live buffer already
carries the fallback's name.  That buffer is scratch space only while it
visits no file; a buffer that visits a file is judged by its file whatever
it is called."
  (let* ((workspace-id (superchat-workspace-current-id))
         (workspace (superchat-workspace--workspace workspace-id))
         (artifact (and workspace
                        (superchat-workspace--target-artifact workspace (nth 2 args))))
         (fallback (superchat-workspace--fallback-buffer-name workspace-id)))
    (cl-flet ((buffer-target (buffer)
                (let ((target (superchat-sandbox-buffer-target buffer)))
                  ;; The fallback is scratch although its name looks like
                  ;; Superchat's own state -- but only the name may be what
                  ;; made it look so.  A chat View or any other Session-owned
                  ;; buffer that was given the fallback's name stays what it
                  ;; is, and so does a buffer that visits a file.
                  (if (and (equal fallback (plist-get target :name))
                           (null (plist-get target :file))
                           (plist-get target :buffer)
                           (not (superchat-sandbox-owned-state-buffer-p
                                 (plist-get target :buffer))))
                      (plist-put target :kind 'scratch)
                    target))))
      (cond
       ((and (null artifact) (get-buffer fallback))
        (list :target (buffer-target (get-buffer fallback))))
       ((null artifact)
        (list :target (list :buffer nil :name fallback :given fallback :kind 'new)))
       ((superchat-workspace--file-adapter-p artifact)
        (list :path (plist-get artifact :path)))
       (t
        (list :target
              (buffer-target
               (or (superchat-workspace--artifact-buffer artifact)
                   (error "Workspace artifact is unresolved: %s"
                          (plist-get artifact :artifact-id))))))))))

(defun superchat-tool-workspace-write
    (content &optional append artifact-id expected-revision
             sandbox-permissions justification)
  "Write CONTENT to an Artifact in the current Workspace.
When APPEND is non-nil, append instead of replacing.  ARTIFACT-ID selects
the target; EXPECTED-REVISION prevents stale writes.  SANDBOX-PERMISSIONS and
JUSTIFICATION request a one-shot wider sandbox mode."
  (cond
   ((not (fboundp 'superchat-workspace-write))
    "Error: workspace module is not loaded.")
   ((not superchat--permission-levels-enabled)
    (superchat-workspace-write content append artifact-id expected-revision))
   (t
    (superchat-sandbox-run-direct-write
     :tool-name "workspace_write"
     :args (list content append artifact-id expected-revision)
     :permissions sandbox-permissions :justification justification
     :resolve #'superchat-tools--workspace-write-target
     :perform (lambda (args _resolved policy)
                (format "%s\nChecks: sandbox mode %s; the Artifact's buffer or file was checked before the change."
                        (apply #'superchat-workspace-write args)
                        (plist-get policy :mode)))))))

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
This is a model-initiated lookup, so -- like automatic recall, and unlike
the user's own `/recall' command -- it follows
`superchat-memory-auto-recall-cross-session': rows from the active
session (any kind) are always eligible; rows from another session are
eligible only when that option allows it (an explicit `/remember' anchor
under the default `remembered', any row under `all', none under `none').
Eligibility and session priority are both enforced by the search itself,
before any row-count limit, so an eligible row is never crowded out by
ineligible or merely better-ranked ones."
  (if (fboundp 'superchat-view-search)
      (let* ((session-id (when (boundp 'superchat--session-id) superchat--session-id))
             (cross-session
              (if (boundp 'superchat-memory-auto-recall-cross-session)
                  superchat-memory-auto-recall-cross-session
                'remembered))
             (rows
              (cond
               ((eq cross-session 'none)
                (superchat-view-search query session-id 20))
               ((fboundp 'superchat-db-tape-search-prioritized)
                (superchat-db-tape-search-prioritized
                 query session-id 20 (eq cross-session 'remembered)))
               (t (superchat-view-search query session-id 20)))))
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

;; Keep `apropos' and `describe-function' in the default surface: both are
;; read-only (apropos is capped at 40 matches, describe-function returns
;; documentation rather than a live value), and without them the default agent
;; cannot find out what exists or what it does.  `describe-variable' stays out:
;; it returns the variable's current value, which is why Authority prompts for
;; it while these two are on `superchat-authority-unprompted-read-tools'.
(defconst superchat-llm-default-tool-names
  '("list_buffers" "apropos" "describe-function" "read_buffer" "edit_buffer"
    "eval_elisp")
  "The default tool surface, in its one home.
`superchat-llm-tool-names' defaults to this list and the `select-tools'
surface baseline reads it, so the two can no longer drift apart the way two
hand-written copies of the same names did.")

(defcustom superchat-llm-tool-names
  superchat-llm-default-tool-names
  "Built-in llm.el tool names Superchat exposes by default.

The default surface is the core loop: enumerate identities, discover
capabilities, read what one does, read live objects, edit live objects,
and ask the runtime.  Everything else attaches by scope instead of globally:
Capability presets carry their own
`:tools' lists (the code preset declares editing, workspace, and LSP
tools), the team Topology overlay appends the team tools, and
extensions declared with `superchat-define-extension' compose their
tools only into Requests they are active for.  The full library stays
available to any explicit tool list.

Set to `all' to expose every built-in tool.  Set to nil to expose no
built-in tools.

This variable governs only Superchat's own **built-in** tools.  A tool a
plugin registered through `superchat-register-tool' is offered by default
regardless of this setting (contract §4.2 rule 3, as amended by user
decision): loading the plugin is the opt-in, and Authority still gates
every call.  Narrowing this list therefore never hides a plugin's own
tool; only an explicit request-local `:tools' profile, or the plugin's own
declared-extension activation, can do that."
  :type '(choice (const :tag "No built-in tools" nil)
                 (const :tag "All built-in tools" all)
                 (repeat :tag "Allowlisted tool names" string))
  :group 'superchat)

(defun superchat--llm-tool-enabled-p (name)
  "Return non-nil when built-in tool NAME should be registered."
  (or (eq superchat-llm-tool-names 'all)
      (member name superchat-llm-tool-names)))

(defconst superchat-tools--escalation-teaching
  " A write the sandbox denies returns `[sandbox: file access denied under <mode> mode]` — a policy denial, not a bug in the call; do not retry another way. When a wider mode would let it succeed, retry this exact call once with `sandbox_permissions` (the narrowest wider mode that suffices) plus a one-sentence `justification`; the approval prompt raised by that retry is how the user consents. If the session states approval prompts are disabled, a denial is final — do not set `sandbox_permissions`. A rejected escalation is final for that call — stop and explain, never work around it."
  "Model-facing teaching appended to the explicit-target write tools.")

(defun superchat-tools--escalation-args ()
  "Return the two escalation argument specs, or nil while levels are off.
Tool schemas are process-global, so the arguments are advertised whenever
permission levels are enabled, whatever a Session's mode; the strict-widening
check at execution is the boundary."
  (when superchat--permission-levels-enabled
    (list (list :name "sandbox_permissions"
                :type 'string
                :enum ["workspace-write" "danger-full-access"]
                :description "The wider sandbox mode this file operation needs. Only valid as a one-shot retry of an operation the sandbox just denied; requires justification and user approval."
                :optional t)
          (list :name "justification"
                :type 'string
                :description "Required with sandbox_permissions: one sentence for the user explaining why this exact file operation needs the wider access."
                :optional t))))

(defun superchat-tools--write-description (description)
  "Return DESCRIPTION, with the escalation teaching while levels are on."
  (if superchat--permission-levels-enabled
      (concat description superchat-tools--escalation-teaching)
    description))

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
          :description "List live Emacs buffers as rows: a quoted Emacs Lisp string name, size, and major mode. Names preserve leading spaces and escape newlines. Decode the name and pass its exact value to read_buffer."
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
          :description (superchat-tools--write-description "Append text to an Emacs buffer. If the buffer does not exist, it will be created.")
          :args (append (list (list :name "buffer"
                            :type 'string
                            :description "The name of the buffer to append text to.")
                      (list :name "text"
                            :type 'string
                            :description "The text to append to the buffer."))
                        (superchat-tools--escalation-args))
          :function #'superchat-tool-append-to-buffer)

         (superchat--maybe-make-llm-tool
          "edit_buffer"
          :description (superchat-tools--write-description "Replace one unique, exact, case-sensitive match in a live buffer. Read the target first. Empty, missing or ambiguous old_string fails; identical replacement reports no change. This edits buffer text only, without saving or evaluating it. Verify the requested result before reporting completion.")
          :args (append (list (list :name "buffer_name"
                            :type 'string
                            :description "Name of the buffer to modify")
                      (list :name "old_string"
                            :type 'string
                            :description "Nonempty text copied from the target buffer, with enough surrounding text to occur exactly once.")
                      (list :name "new_string"
                            :type 'string
                            :description "Text to replace old_string with"))
                        (superchat-tools--escalation-args))
          :function #'superchat-tool-edit-buffer)

         (superchat--maybe-make-llm-tool
          "replace_buffer"
          :description (superchat-tools--write-description "Completely overwrites buffer contents")
          :args (append (list (list :name "buffer_name"
                            :type 'string
                            :description "Name of the buffer to overwrite")
                      (list :name "content"
                            :type 'string
                            :description "Content to write to the buffer"))
                        (superchat-tools--escalation-args))
          :function #'superchat-tool-replace-buffer)

         ;; Saving and writing files are explicit-target writes of their own,
         ;; so the ordinary edit-then-save path needs no `eval_elisp'.  They
         ;; exist with permission levels and join the default surface at the
         ;; cutover.
         (when superchat--permission-levels-enabled
           (superchat--maybe-make-llm-tool
            "save_buffer"
            :description (superchat-tools--write-description "Save the file a live buffer visits and report the path and size. An indirect or narrowed buffer saves the whole file of its base buffer, including modifications that were already unsaved. A buffer without a file is an error; use write_file for a new file. Saving does not evaluate or reload anything.")
            :args (append (list (list :name "buffer_name"
                                      :type 'string
                                      :description "Name of the file-visiting buffer to save."))
                          (superchat-tools--escalation-args))
            :function #'superchat-tool-save-buffer))

         (when superchat--permission-levels-enabled
           (superchat--maybe-make-llm-tool
            "write_file"
            :description (superchat-tools--write-description "Create or replace a file that no live buffer visits. If a buffer visits the file this fails and names that buffer; change it with edit_buffer and save_buffer instead. The parent directory must exist. Writing does not load or evaluate the file.")
            :args (append (list (list :name "path"
                                      :type 'string
                                      :description "File path; a relative path is resolved against the Session's tool directory.")
                                (list :name "content"
                                      :type 'string
                                      :description "The complete new contents of the file."))
                          (superchat-tools--escalation-args))
            :function #'superchat-tool-write-file))

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
          :description "Evaluate one complete Emacs Lisp expression in the running Emacs and return its value. Use progn or let for multiple forms. For buffer operations, select the intended live buffer explicitly with with-current-buffer. Prefer edit_buffer for exact text replacements. After a change, read the effective value or key-binding in that buffer; a returned t or literal OK does not verify the change. For persistence, save the actual loaded configuration and verify in a fresh Emacs that loads it."
          :args (list (list :name "expression"
                            :type 'string
                            :description "One complete expression. Return observed state needed to verify the task; on an error, inspect any partial changes before retrying."))
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
          :args (append (list (list :name "content"
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
                        (superchat-tools--escalation-args))
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
          :description "Read one Workspace Artifact from the current Workspace. Cross-Run reads require a live resource and the revision reported by workspace_info; only durable Artifacts survive process restart, and the producer Run must have a recorded `run.completed` event."
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
    ;; This cache holds only global built-ins.  Plugin tools are resolved from
    ;; the caller's Scope below, on every materialization; caching them here
    ;; would make A's implementation visible to B and would retain Scope
    ;; installations in a process-global object.
    (setq superchat--llm-tool-names-signature
          (copy-tree superchat-llm-tool-names))
    (setq superchat--subagent-registry-tool-cache superchat-llm-tools-list)
    superchat-llm-tools-list))

(defconst superchat-tools--ask-user-args
  '((:name "question" :type string :description "One nonempty question requiring a text answer"))
  "The string-only trusted input schema; not a generic form schema.")

(defun superchat-tools--trusted-ask-descriptor-p (tool operation)
  "Whether TOOL retains the exact factory contract for OPERATION."
  (and (equal (llm-tool-name tool) "ask_user")
       (eq (llm-tool-function tool) operation)
       (eq (llm-tool-async tool) t)
       (equal (llm-tool-args tool) superchat-tools--ask-user-args)))

(defun superchat-tools--make-ask-user (operation)
  "Build the request-local native descriptor for accepted OPERATION.
This descriptor never enters the global tool cache."
  (unless (and (functionp operation)
               (fboundp 'superchat-runtime--trusted-input-operation-p)
               (superchat-runtime--trusted-input-operation-p
                superchat--tool-execution-context operation t))
    (error "ask_user requires an accepted operation"))
  (llm-make-tool
   :name "ask_user" :async t :function operation
   :description "Ask the user for required information and wait for their answer. Answering does not authorize other tools."
   ;; This string-only schema is a list of flat argument plists.  Detach
   ;; string leaves as well as conses: descriptor mutation must not rewrite
   ;; the canonical expectation used by `--trusted-ask-descriptor-p'.
   :args (mapcar (lambda (argument)
                   (mapcar (lambda (value)
                             (if (stringp value)
                                 (substring-no-properties value)
                               value))
                           argument))
                 superchat-tools--ask-user-args)))

(defun superchat-get-llm-tools (&optional tool-names input-operation)
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
          (let* ((registered (append (cl-remove-if
                                      (lambda (tool) (equal "ask_user" (llm-tool-name tool)))
                                      (superchat-llm-tools-reload))
                                     (superchat-tools--external-llm-tools)
                                     (when input-operation
                                       (list (superchat-tools--make-ask-user input-operation)))))
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
    (let ((base (append (or superchat-llm-tools-list
                            (and (fboundp 'llm-make-tool)
                                 (superchat-llm-tools-reload)))
                        (superchat-tools--external-llm-tools))))
      (when (cl-find "ask_user" base :key #'llm-tool-name :test #'equal)
        (setq base (cl-remove-if (lambda (tool) (equal "ask_user" (llm-tool-name tool))) base)))
      (if input-operation
          (append base (list (superchat-tools--make-ask-user input-operation)))
        base))))

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
    "lsp-hover" "save_buffer" "write_file")
  "Built-in tool names reserved by `superchat-llm-tools-reload'.")

(defconst superchat-tools--installation-property
  'superchat-tools--installation
  "Private property carried by an uninterned materialized tool function.

The property is instance data, not a registry: a descriptor keeps its own
uninterned callable symbol and Authority reads the paired installation from
the exact function it was given.  In particular, it must never be recovered
later by tool name after a nearer Scope has replaced that name.")

(defun superchat-tools--make-installation-proxy (name implementation)
  "Return an uninterned callable proxy for one NAME IMPLEMENTATION pair.

The caller installs the private installation property only after its Scope
registration has succeeded, preserving duplicate registration atomicity."
  (let ((proxy (make-symbol (format "superchat-tool-%s" name))))
    (fset proxy implementation)
    proxy))

(defun superchat-tools--registration-scope (&optional scope)
  "Return the target Scope for a plugin tool registration."
  (superchat-registration-scope-resolve scope))

(defun superchat-tools--visible-installation (name &optional scope)
  "Return live plugin tool installation NAME visible from SCOPE, or nil."
  (let ((installation
         (superchat-scope-lookup
          (superchat-execution-scope-resolve scope) 'tool name)))
    (and (plist-get installation :live) installation)))

(defun superchat-tools--visible-installations (&optional scope)
  "Return every live plugin tool installation visible from SCOPE.

This Scope-layer view is the only plugin-tool registry.  Nearer layers shadow
parents while siblings remain invisible."
  (delq nil
        (mapcar
         (lambda (entry)
           (and (plist-get (cdr entry) :live) entry))
         (superchat-scope-merge
          (superchat-execution-scope-resolve scope) 'tool))))

(defun superchat-tools--local-installation (name &optional scope)
  "Return NAME from SCOPE's own tool layer, never an ancestor."
  (superchat-scope-local-lookup scope 'tool name))

;;;###autoload
(cl-defun superchat-register-tool
    (name &key function description args async effects approval-preview surfaces scope)
  "Register plugin tool NAME on the model tool surface.

SURFACES, when given, is the list of Surfaces whose default tool baseline
may offer NAME; on any other Surface the tool is only reachable through an
explicit `:tools' list.  (This replaces the deleted extension layer's tool
claims: a Telega tool stays off the chat surface.)

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

Loading the plugin is the user's opt-in: a registered tool is offered by
default on the chat-like surfaces (`chat', `workflow', `subagent',
`external', a plugin surface) regardless of `superchat-llm-tool-names' --
that variable narrows only Superchat's own built-in tools (contract §4.2
rule 3, as amended: \"tools a plugin registered through
`superchat-register-tool' are offered by default ... loading the plugin
is the user's opt-in; Authority still gates every call\").  Every
execution still passes through `superchat-tool-execute' (Authority,
approval, Tape audit), and an explicit request-local tool profile
includes NAME only when it names it.  A tool claimed by a declared
extension (`superchat-define-extension') is the one exception: it stays
off the surface until that extension is active, per
`superchat-tools--external-llm-tools'.

SCOPE explicitly selects an installation layer.  Otherwise plugin effect
ownership wins over the current execution Scope, then global.  Function,
resolver, and approval preview stay together in one Scope installation; a
duplicate NAME in that layer is rejected before any registration writes.
Returns NAME."
  (unless (and (stringp name) (not (string-empty-p name)))
    (error "Tool name must be a non-empty string"))
  (when (or (equal name "ask_user") (member name superchat-tools--builtin-tool-names))
    (error "Tool %s is built-in; plugin tools must use a distinct name" name))
  (unless (functionp function)
    (error "Tool %s requires a callable :function" name))
  (unless (and (stringp description) (not (string-empty-p description)))
    (error "Tool %s requires a :description" name))
  ;; Effects feed the decision machine only.  Under permission levels a
  ;; registered tool is decided by its class, so none need be declared.
  (unless (or effects (bound-and-true-p superchat--permission-levels-enabled))
    (error "Tool %s must declare :effects for the base Authority fold" name))
  (when (and approval-preview (not (functionp approval-preview)))
    (error "Approval preview for %s must be callable" name))
  (let* ((target (superchat-tools--registration-scope scope))
         (resolver (if (functionp effects) effects (lambda (_args) effects)))
         ;; Each installation owns a distinct callable proxy.  Its identity
         ;; travels with every materialized descriptor through Agent Loop,
         ;; prepared subagent, and request-local wrappers without a global
         ;; name-to-installation side table.
         (proxy (superchat-tools--make-installation-proxy name function))
         (spec (list :name name :function proxy :description description
                     :args args :async async :surfaces surfaces
                     :resolver resolver :preview approval-preview
                     :proxy proxy :live t :undo nil))
         ;; Scope rejects a duplicate before it mutates its layer or records a
         ;; cleanup.  The outer undo seals this exact installation before the
         ;; layer removal, so stale materialized descriptors fail closed.
         (withdraw (superchat-scope-register target 'tool name spec))
         (undo
          (superchat-scope-effect
           target
           (lambda ()
             (when (plist-get spec :live)
               (plist-put spec :live nil)
               ;; A retained materialized descriptor needs only its exact
               ;; revoked identity.  Keep that minimal plist on the proxy so
               ;; Authority can fail it closed; release every closure and the
               ;; original function cell before withdrawing the Scope entry.
               (when-let* ((installed-proxy (plist-get spec :proxy)))
                 (fmakunbound installed-proxy))
               (plist-put spec :function nil)
               (plist-put spec :undo nil)
               (plist-put spec :resolver nil)
               (plist-put spec :preview nil)
               (plist-put spec :description nil)
               (plist-put spec :args nil)
               (plist-put spec :surfaces nil)
               (plist-put spec :proxy nil)
               (funcall withdraw)
               (setq superchat-llm-tools-list nil))))))
    ;; This happens only after `superchat-scope-register' accepted the new
    ;; layer entry.  A same-layer duplicate therefore leaves no observable
    ;; proxy installation behind.
    (put proxy superchat-tools--installation-property spec)
    (plist-put spec :undo undo))
  name)

(cl-defun superchat-unregister-tool (name &key scope)
  "Withdraw NAME from SCOPE's own plugin tool layer.

Without explicit SCOPE, use the same product registration precedence as
`superchat-register-tool'.  Ancestor and sibling definitions are never
removed; a parent becomes visible naturally after local withdrawal."
  (when-let* ((spec (superchat-tools--local-installation name scope))
              (undo (plist-get spec :undo)))
    (funcall undo)
    t))

(defun superchat-tools--external-llm-tools ()
  "Build llm.el tool structs for every plugin-registered tool.
Which of them a Request may offer is decided by `select-tools' (a tool's
`:surfaces' keeps it off other surfaces' baselines)."
  (when (fboundp 'llm-make-tool)
    (delq nil
          (mapcar
           (lambda (entry)
             (let ((name (car entry))
                   (spec (cdr entry)))
               (progn
                 (llm-make-tool
                  :name name
                  :description (plist-get spec :description)
                  :args (plist-get spec :args)
                  :async (plist-get spec :async)
                  :function (plist-get spec :function)))))
           (superchat-tools--visible-installations)))))

(declare-function superchat-authority-visible-effect-resolver-names
                  "superchat-authority" (&optional scope))
;; ── Plugin (spec §3): the default provider defined next to its code ──
(superchat-define-plugin tools
  :inject '(authority tape)
  :doc "The tool registry, materialization for the model, and the one execution seam."
  :apply
  (lambda (scope _)
    (superchat-service-provide
     scope 'tools
     (superchat-service-table
      register (lambda (&rest args) (apply #'superchat-register-tool args))
      descriptors #'superchat-authority-visible-effect-resolver-names
      materialize #'superchat-get-llm-tools
      execute (lambda (&rest args) (apply #'superchat-tool-execute args))))))


;; ── Plugin: the built-in tools' Authority declarations ──
(superchat-define-plugin tools-builtin
  :inject '(authority)
  :doc "Effect resolvers and approval previews of the built-in tools; without
it no built-in tool is a selectable descriptor."
  :apply
  (lambda (_scope _config)
    (superchat-tools--register-effect-resolvers)
    (dolist (name '("shell-command" "eshell-command" "eval_elisp"))
      (superchat-authority-register-approval-preview
       name #'superchat-tool--preview-arguments))

    (dolist (name '("edit_buffer" "replace_buffer"))
      (superchat-authority-register-approval-preview
       name #'superchat-tool--preview-buffer-edit))

    (superchat-authority-register-approval-preview
     "list_buffers" #'superchat-tool--preview-buffer-list)))

(provide 'superchat-tools)

;;; superchat-tools.el ends here
