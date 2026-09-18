;;; superchat-sandbox.el --- Permission levels: sandbox mode and approval policy -*- lexical-binding: t; -*-

;;; Commentary:
;; A Session has one permission level, which names a pair of knobs: a sandbox
;; mode (`read-only', `workspace-write', `danger-full-access') and an approval
;; policy (`ask', `never').  The vocabulary and the write-through order are
;; DeepSeek Harness's sandbox-policy and permission-presets packages.  See
;; docs/decisions/accepted/2026-09-17-permissions-the-dsh-way.md.
;;
;; This file owns the knobs, their Session events and the per-call policy
;; resolution.  Nothing consults them while
;; `superchat--permission-levels-enabled' is nil.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-tape)
(require 'superchat-approval)

(declare-function superchat-view-session-configuration "superchat-tape-view" (session-id))

(defconst superchat-sandbox-modes
  '(read-only workspace-write danger-full-access)
  "The closed sandbox modes, narrowest first.
`read-only' denies every direct write, `workspace-write' allows direct writes
under the Session workspace and the temporary directory, and
`danger-full-access' does not restrict them.")

(defconst superchat-sandbox-wider-modes
  '((read-only workspace-write danger-full-access)
    (workspace-write danger-full-access))
  "Alist of MODE to the modes strictly wider than it.")

(defcustom superchat-sandbox-default-mode 'workspace-write
  "Sandbox mode of a Session that has not selected one."
  :type '(choice (const read-only) (const workspace-write)
                 (const danger-full-access))
  :group 'superchat)

(defcustom superchat-permission-levels
  '((workspace-write
     :sandbox workspace-write :approval ask
     :description "Write inside the workspace and permitted temporary directories; wider retries require approval.")
    (danger-full-access
     :sandbox danger-full-access :approval never
     :description "Full file access without approval prompts.")
    (read-only
     :sandbox read-only :approval ask
     :description "Read only by default; a write needs a one-shot escalation."))
  "Permission levels a Session can select, in declaration order.
Each entry is (NAME :sandbox MODE :approval POLICY :description STRING).
The name `custom' is reserved for knob values that match no entry."
  :type '(alist :key-type symbol :value-type plist)
  :group 'superchat)

(defconst superchat-permission-custom 'custom
  "Derived level name for knob values matching no configured level.
It is reported, never selected.")

;;;; Sandbox mode

(defun superchat-sandbox--mode-symbol (value)
  "Return VALUE as a member of `superchat-sandbox-modes', or nil."
  (let ((symbol (cond ((symbolp value) value)
                      ((stringp value) (intern value)))))
    (car (memq symbol superchat-sandbox-modes))))

(defun superchat-sandbox--configuration (session-id)
  "Return SESSION-ID's projected configuration, or nil."
  (and (stringp session-id) (not (string-empty-p session-id))
       (fboundp 'superchat-view-session-configuration)
       (superchat-view-session-configuration session-id)))

(defun superchat-sandbox-override (session-id)
  "Return SESSION-ID's recorded sandbox mode, or nil without one."
  (superchat-sandbox--mode-symbol
   (plist-get (superchat-sandbox--configuration session-id) :sandbox-mode)))

(defun superchat-sandbox-effective-mode (session-id)
  "Return the sandbox mode in force for SESSION-ID."
  (or (superchat-sandbox-override session-id)
      (superchat-sandbox--mode-symbol superchat-sandbox-default-mode)
      'workspace-write))

(defun superchat-sandbox-set-mode (session-id mode)
  "Record MODE for SESSION-ID; return non-nil when an event was appended.
The single write path.  Selecting the effective mode again appends nothing."
  (let ((mode (or (superchat-sandbox--mode-symbol mode)
                  (error "Unknown sandbox mode: %S" mode))))
    (unless (eq mode (superchat-sandbox-effective-mode session-id))
      (superchat-tape-append-event
       (list :type "sandbox.mode" :session-id session-id :actor-id "user"
             :summary "sandbox.mode"
             :payload (list :mode (symbol-name mode))))
      t)))

(defun superchat-sandbox-wider-p (mode effective)
  "Return non-nil when MODE is strictly wider than EFFECTIVE."
  (and (memq mode (cdr (assq effective superchat-sandbox-wider-modes))) t))

(cl-defun superchat-sandbox-resolve (&key session-id mode workspace-root)
  "Return the complete sandbox policy for one capability call.
The result is (:mode MODE :workspace-root ROOT :session-id SESSION-ID).  An
explicit approved MODE outranks the Session's recorded mode, which outranks
`superchat-sandbox-default-mode'.  WORKSPACE-ROOT is canonicalized; without
one the root is `default-directory'."
  (list :mode (or (superchat-sandbox--mode-symbol mode)
                  (superchat-sandbox-effective-mode session-id))
        :workspace-root (file-name-as-directory
                         (file-truename
                          (expand-file-name (or workspace-root default-directory))))
        :session-id session-id))

;;;; Target checks
;;
;; A tool that knows its target checks it here, before it changes anything.
;; There is no global fence: arbitrary Lisp and external processes are not
;; confined by these checks, and hooks or file handlers the user configured
;; run as part of the trusted Emacs environment.

(defvar superchat--tool-execution-context)
(defvar superchat-tools-default-directory)
(defvar superchat-data-directory)
(declare-function superchat-editor-workspace-owner-session
                  "superchat-editor-workspace" (buffer))

(defun superchat-sandbox-denial-marker (mode)
  "Return the model-facing marker for a write denied under MODE."
  (format "[sandbox: file access denied under %s mode]" mode))

(defun superchat-sandbox-escalation-hint (subject)
  "Return the model-facing hint that SUBJECT may be retried with escalation."
  (format "[sandbox: escalation available — retry this exact %s once with sandbox_permissions (the narrowest wider mode that suffices) + justification; the approval prompt asks the user]"
          subject))

(defun superchat-sandbox--blank-p (value)
  "Return non-nil when VALUE is nil or an empty or whitespace-only string."
  (or (null value) (and (stringp value) (string-empty-p (string-trim value)))))

(defun superchat-sandbox-escalation-argument-error (permissions justification)
  "Return the pairing error text for PERMISSIONS and JUSTIFICATION, or nil."
  (cond
   ((and (superchat-sandbox--blank-p permissions)
         (superchat-sandbox--blank-p justification))
    nil)
   ((superchat-sandbox--blank-p permissions)
    "invalid escalation: justification is only valid together with sandbox_permissions")
   ((null justification)
    "invalid escalation: sandbox_permissions requires a justification")
   ((superchat-sandbox--blank-p justification)
    "invalid justification: expected a non-empty sentence")))

(defun superchat-sandbox-canonical-path (path)
  "Return PATH with its deepest existing ancestor resolved to a truename.
A path that does not exist yet is judged by where it would be created, so a
new file under a symlinked directory is contained by the link's target.  A
symbolic link counts as existing even when what it points to does not: a
write through a dangling link lands where the link points, and that is the
location judged."
  (let ((path (expand-file-name path))
        (tail nil))
    (while (and (not (file-exists-p path))
                (not (file-symlink-p path))
                (not (equal path (directory-file-name
                                  (or (file-name-directory path) path)))))
      (push (file-name-nondirectory (directory-file-name path)) tail)
      (setq path (directory-file-name (file-name-directory (directory-file-name path)))))
    (let ((root (file-truename path)))
      ;; A dangling link's truename may itself sit under links not yet
      ;; resolved because its last components do not exist.
      (unless (or (file-exists-p root) (equal root path))
        (setq root (superchat-sandbox-canonical-path root)))
      (dolist (segment tail)
        (setq root (expand-file-name segment root)))
      root)))

(defun superchat-sandbox--under-p (canonical root)
  "Return non-nil when CANONICAL is directory ROOT or lies beneath it."
  (or (string= (file-name-as-directory canonical) root)
      (string-prefix-p root canonical)))

(defun superchat-sandbox-data-root ()
  "Return Superchat's canonical data directory, or nil when it is unset.
The directory holds the Tape, published Artifacts and the approval ledger of
agent-written tools, so it is outside every workspace even when a workspace
root happens to contain it."
  (when (and (boundp 'superchat-data-directory)
             (stringp superchat-data-directory)
             (not (string-empty-p superchat-data-directory)))
    (file-name-as-directory
     (superchat-sandbox-canonical-path superchat-data-directory))))

(defun superchat-sandbox-writable-roots (policy)
  "Return the canonical directories POLICY's mode may write under.
Only `workspace-write' has any: the workspace root and the temporary
directory."
  (when (eq 'workspace-write (plist-get policy :mode))
    (delete-dups
     (mapcar (lambda (root)
               (file-name-as-directory (file-truename (expand-file-name root))))
             (list (plist-get policy :workspace-root)
                   temporary-file-directory)))))

(defun superchat-sandbox-path-writable-p (path policy)
  "Return non-nil when POLICY permits a direct write to PATH."
  (pcase (plist-get policy :mode)
    ('danger-full-access t)
    ('workspace-write
     (and (not (file-remote-p path))
          (let ((canonical (superchat-sandbox-canonical-path path))
                (data (superchat-sandbox-data-root)))
            (and (not (and data (superchat-sandbox--under-p canonical data)))
                 (cl-some (lambda (root)
                            (superchat-sandbox--under-p canonical root))
                          (superchat-sandbox-writable-roots policy))))))))

(defun superchat-sandbox-internal-buffer-p (buffer)
  "Return non-nil when BUFFER holds Superchat's own runtime state.
That is a Session-owned EditorWorkspace buffer such as a chat View, a buffer
in `superchat-mode', or a buffer named like Superchat's panels, previews and
temporaries.  Such a buffer is never inside a Session workspace."
  (with-current-buffer buffer
    (or (bound-and-true-p superchat-mode)
        (and (fboundp 'superchat-editor-workspace-owner-session)
             (ignore-errors (superchat-editor-workspace-owner-session buffer))
             t)
        (let ((case-fold-search nil))
          (string-match-p "\\` ?\\*[Ss]uperchat[-: *]" (buffer-name buffer))))))

(defun superchat-sandbox-buffer-target (buffer-or-name)
  "Resolve BUFFER-OR-NAME to the buffer a direct write would change.
Return (:buffer BASE :name NAME :given GIVEN :file PATH :kind KIND).  An
indirect buffer resolves to its base, whose text and file it shares.  KIND is
`file' for a buffer visiting a file, `internal' for Superchat's own state,
`scratch' for any other buffer, and `new' when no such buffer exists yet."
  (let* ((given (if (bufferp buffer-or-name) (buffer-name buffer-or-name) buffer-or-name))
         (live (get-buffer buffer-or-name))
         (base (and live (or (buffer-base-buffer live) live)))
         (file (and base (buffer-file-name base))))
    (list :buffer base
          :name (if base (buffer-name base) given)
          :given given
          :file file
          :kind (cond ((null base) 'new)
                      ((superchat-sandbox-internal-buffer-p base) 'internal)
                      (file 'file)
                      (t 'scratch)))))

(defun superchat-sandbox-target-writable-p (target policy)
  "Return non-nil when POLICY permits a direct write to buffer TARGET."
  (pcase (plist-get policy :mode)
    ('danger-full-access t)
    ('workspace-write
     (pcase (plist-get target :kind)
       ('file (superchat-sandbox-path-writable-p (plist-get target :file) policy))
       ((or 'scratch 'new) t)))))

(defun superchat-sandbox-call-policy (&optional mode)
  "Return the sandbox policy of the tool call in progress.
MODE, when non-nil, is an approved wider mode for this one call.  The Session
and workspace root come from the tool execution context; outside one, the
root is the tools' base directory and the mode is the configured default."
  (let ((context (and (boundp 'superchat--tool-execution-context)
                      (listp superchat--tool-execution-context)
                      superchat--tool-execution-context)))
    (superchat-sandbox-resolve
     :session-id (plist-get context :session-id)
     :mode mode
     :workspace-root (or (plist-get context :workspace-root)
                         (and (boundp 'superchat-tools-default-directory)
                              superchat-tools-default-directory)
                         default-directory))))

(defun superchat-sandbox--approve-escalation (requested justification effective subject tool-name args)
  "Return (MODE . APPROVED-ARGS) for a granted escalation, or an error string.
REQUESTED must be strictly wider than EFFECTIVE; a request that is not fails
with its own text and asks nobody.  SUBJECT, TOOL-NAME and ARGS describe the
call to the answerer."
  (let* ((mode (superchat-sandbox--mode-symbol requested))
         (context (and (boundp 'superchat--tool-execution-context)
                       (listp superchat--tool-execution-context)
                       superchat--tool-execution-context)))
    (cond
     ((not (and mode (superchat-sandbox-wider-p mode effective)))
      (format "sandbox escalation to \"%s\" is not strictly wider than this call's current \"%s\" mode"
              requested effective))
     ((not (and (stringp (plist-get context :run-id))
                (stringp (plist-get context :turn-id))
                (stringp (plist-get context :session-id))))
      (format "sandbox escalation to \"%s\" requires approval, but the call has no Run to route it through"
              mode))
     (t
      (let* ((answer
              (condition-case nil
                  (superchat-approval-request-snapshot
                   (list :session-id (plist-get context :session-id)
                         :run-id (plist-get context :run-id)
                         :turn-id (plist-get context :turn-id)
                         :execution-id
                         (format "%s-escalation"
                                 (or (plist-get context :execution-id)
                                     (format "tool-%06x" (random #x1000000))))
                         :tool-name tool-name
                         :args args
                         :call-id (plist-get context :tool-call-id)
                         :reason (format "escalate sandbox to %s: %s"
                                         mode (string-trim justification))))
                (superchat-approval-outside-run (list :outcome 'unavailable)))))
	(pcase (plist-get answer :outcome)
          ;; The grant is for the snapshot the seam took on entry: the caller
          ;; resolves its target and writes from these arguments, not from
          ;; whatever its own variables hold by now.
          ('allowed-once (cons mode (plist-get answer :args)))
          ('rejected (format "the user rejected escalating this %s to \"%s\"" subject mode))
          ('cancelled (format "approval for escalating to \"%s\" was cancelled" mode))
          (_ (format "sandbox escalation to \"%s\" requires approval, but no approval channel is available"
                     mode))))))))

(defun superchat-sandbox--resolved-writable-p (resolved policy)
  "Return non-nil when POLICY permits the write RESOLVED describes."
  (if (plist-get resolved :target)
      (superchat-sandbox-target-writable-p (plist-get resolved :target) policy)
    (superchat-sandbox-path-writable-p (plist-get resolved :path) policy)))

(cl-defun superchat-sandbox-run-direct-write
    (&key tool-name args permissions justification resolve perform
          (subject "operation"))
  "Run one explicit-target write under the sandbox; return its result text.
RESOLVE is called with the argument list and returns (:target TARGET) for a
buffer write, (:path PATH) for a file write, or (:final TEXT) when the call
ends without writing: a precondition failed or nothing would change.
PERFORM is called with the argument list, the resolution and the policy, and
makes the change.

A call without escalation arguments resolves, is checked and runs.  A call
with PERMISSIONS and JUSTIFICATION first resolves, so a call that would write
nothing asks nobody; then the pairing and strict widening are checked and the
user is asked.  After a grant everything is done again from the approved
argument snapshot: RESOLVE runs on it, so a file that became visited or
changed on disk while the question was open is seen, the target is checked
under the wider mode, and PERFORM writes those approved arguments.  A denial
is decided before anything changed."
  (let* ((argument-error
          (superchat-sandbox-escalation-argument-error permissions justification))
         (standing (superchat-sandbox-call-policy))
         (resolved (and (not argument-error) (funcall resolve args))))
    (cond
     (argument-error (concat "Error: " argument-error))
     ((plist-get resolved :final))
     (t
      (let ((granted
             (and (not (superchat-sandbox--blank-p permissions))
                  (superchat-sandbox--approve-escalation
                   permissions justification (plist-get standing :mode)
                   subject tool-name args))))
        (if (stringp granted)
            (concat "Error: " granted)
          (let* ((args (if granted (cdr granted) args))
                 (policy (if granted
                             (superchat-sandbox-call-policy (car granted))
                           standing))
                 (resolved (if granted (funcall resolve args) resolved)))
            (cond
             ((plist-get resolved :final))
             ((superchat-sandbox--resolved-writable-p resolved policy)
              (funcall perform args resolved policy))
             (t
              (concat (superchat-sandbox-denial-marker (plist-get policy :mode))
                      "\n"
                      (superchat-sandbox-escalation-hint subject)))))))))))

;;;; Permission levels

(defun superchat-permission-level-names ()
  "Return the selectable level names in declaration order."
  (mapcar #'car superchat-permission-levels))

(defun superchat-permission-level (name)
  "Return level NAME's (:sandbox :approval :description) plist, or signal."
  (let ((symbol (if (stringp name) (intern name) name)))
    (when (eq symbol superchat-permission-custom)
      (error "Permission level `custom' is derived and cannot be selected"))
    (or (cdr (assq symbol superchat-permission-levels))
        (error "Unknown permission level: %s (available: %s)" name
               (mapconcat #'symbol-name (superchat-permission-level-names) ", ")))))

(defun superchat-permission-current (session-id)
  "Return the level name matching SESSION-ID's effective knob values.
A still-matching recorded selection wins a tie between levels sharing one
bundle; otherwise the first matching level in declaration order; otherwise
`custom'."
  (let* ((sandbox (superchat-sandbox-effective-mode session-id))
         (approval (superchat-approval-effective-policy session-id))
         (recorded (plist-get (superchat-sandbox--configuration session-id)
                              :permission-level))
         (recorded (and recorded (if (stringp recorded) (intern recorded) recorded)))
         (matches
          (cl-remove-if-not
           (lambda (entry)
             (and (eq sandbox (plist-get (cdr entry) :sandbox))
                  (eq approval (plist-get (cdr entry) :approval))))
           superchat-permission-levels)))
    (cond ((assq recorded matches) recorded)
          (matches (caar matches))
          (t superchat-permission-custom))))

(defun superchat-permission-set (session-id name)
  "Select permission level NAME for SESSION-ID; return the level name.
Unknown names signal.  The selection is recorded first, and only when NAME is
not already the effective level; each knob is then written through its own
setter only when its effective value changes."
  (let* ((symbol (if (stringp name) (intern name) name))
         (level (superchat-permission-level symbol)))
    (unless (and (stringp session-id) (not (string-empty-p session-id)))
      (error "Selecting a permission level requires a Session"))
    (unless (eq symbol (superchat-permission-current session-id))
      (superchat-tape-append-event
       (list :type "permission.level" :session-id session-id :actor-id "user"
             :summary "permission.level"
             :payload (list :level (symbol-name symbol)))))
    (superchat-sandbox-set-mode session-id (plist-get level :sandbox))
    (superchat-approval-set-policy session-id (plist-get level :approval))
    symbol))

(defun superchat-permission-describe (session-id)
  "Return one line naming SESSION-ID's level and the selectable ones."
  (let ((current (superchat-permission-current session-id)))
    (format "Permissions: %s (available: %s)"
            current
            (mapconcat #'symbol-name (superchat-permission-level-names) ", "))))

(provide 'superchat-sandbox)
;;; superchat-sandbox.el ends here
