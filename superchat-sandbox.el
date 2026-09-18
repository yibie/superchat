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

(defun superchat-sandbox-set-mode (session-id mode &optional pin)
  "Record MODE for SESSION-ID; return non-nil when an event was appended.
The single write path.  Selecting the effective mode again appends nothing.
With PIN non-nil the mode is recorded unless the Session already recorded
it, even when it equals the configured default: a value fixed at a Session's
start must not drift when the default changes later."
  (let ((mode (or (superchat-sandbox--mode-symbol mode)
                  (error "Unknown sandbox mode: %S" mode))))
    (unless (eq mode (if pin
                         (superchat-sandbox-override session-id)
                       (superchat-sandbox-effective-mode session-id)))
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

(defun superchat-sandbox-owned-state-buffer-p (buffer)
  "Return non-nil when BUFFER is Superchat state by more than its name.
That is a buffer in `superchat-mode', such as a chat View, or a buffer some
Session's EditorWorkspace owns."
  (with-current-buffer buffer
    (or (bound-and-true-p superchat-mode)
        (and (fboundp 'superchat-editor-workspace-owner-session)
             (ignore-errors (superchat-editor-workspace-owner-session buffer))
             t))))

(defun superchat-sandbox-internal-buffer-p (buffer)
  "Return non-nil when BUFFER holds Superchat's own runtime state.
That is a Session-owned EditorWorkspace buffer such as a chat View, a buffer
in `superchat-mode', or a buffer named like Superchat's panels, previews and
temporaries.  Such a buffer is never inside a Session workspace."
  (or (superchat-sandbox-owned-state-buffer-p buffer)
      (let ((case-fold-search nil))
        (string-match-p "\\` ?\\*[Ss]uperchat[-: *]" (buffer-name buffer)))))

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
                                         mode (string-trim justification)))
                   ;; The same borrowed execution Scope the executor's own
                   ;; question uses, so an answerer registered for this Run's
                   ;; Scope hears an escalation too.
                   (superchat-execution-scope-resolve nil))
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

(declare-function superchat-runtime-effect-capable-p "superchat-runtime" (run-id))
(declare-function superchat-execution-scope-resolve "superchat-plugin" (&optional scope))

(defun superchat-sandbox--run-effect-capable-p ()
  "Return non-nil when the tool call in progress may still change anything.
A call outside any Run has nothing to outlive."
  (let ((run-id (and (boundp 'superchat--tool-execution-context)
                     (listp superchat--tool-execution-context)
                     (plist-get superchat--tool-execution-context :run-id))))
    (or (not (stringp run-id))
        (and (fboundp 'superchat-runtime-effect-capable-p)
             (superchat-runtime-effect-capable-p run-id)))))

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
             ;; The executor checked the Run before this body started, but a
             ;; question asked from inside the body can outlive the Run.  A
             ;; late grant is recorded by the seam and writes nothing.
             ((and granted (not (superchat-sandbox--run-effect-capable-p)))
              "[Tool call blocked: Run is no longer active]")
             ((plist-get resolved :final))
             ((superchat-sandbox--resolved-writable-p resolved policy)
              (funcall perform args resolved policy))
             (t
              (concat (superchat-sandbox-denial-marker (plist-get policy :mode))
                      "\n"
                      (superchat-sandbox-escalation-hint subject)))))))))))

;;;; The fixed level stage
;;
;; One decision per tool call, made where the executor decides.  Tools that
;; know their target are allowed here and check it themselves where they act.
;; Full Emacs control and tools from outside are decided here, because no
;; directory rule confines them.

(defconst superchat-sandbox-full-control-tools
  '("eval_elisp" "shell-command" "eshell-command" "bash" "define_tool")
  "Built-in tools with full control of Emacs.
Lisp can change any state and start any process, and a shell can write
anywhere, so these are never treated as confined by a workspace.")

(defvar superchat-mcp-trusted-servers)
(defvar superchat-tools--builtin-tool-names)
(declare-function superchat--syn-tool-p "superchat-synthesis" (name))
(declare-function superchat-mcp-tool-p "superchat-mcp" (name))
(declare-function superchat-mcp-tool-server "superchat-mcp" (name))

(defun superchat-sandbox-tool-class (tool-name)
  "Return TOOL-NAME's class for the level stage.
`full-control' for the tools in `superchat-sandbox-full-control-tools' and
every agent-written tool; `trusted' for an MCP tool of a server the user
listed in `superchat-mcp-trusted-servers'; `outside' for any other MCP or
third-party registered tool; `builtin' for everything else Superchat ships."
  (cond
   ((or (member tool-name superchat-sandbox-full-control-tools)
        (and (fboundp 'superchat--syn-tool-p) (superchat--syn-tool-p tool-name)))
    'full-control)
   ;; Trust is for the exact server a tool was registered for.  The name
   ;; alone cannot say: "mcp:notes:foreign:touch" belongs to the server
   ;; "notes:foreign", not to a trusted "notes".
   ((and (fboundp 'superchat-mcp-tool-p) (superchat-mcp-tool-p tool-name))
    (let ((server (superchat-mcp-tool-server tool-name)))
      (if (and (stringp server)
               (member server (bound-and-true-p superchat-mcp-trusted-servers)))
          'trusted
        'outside)))
   ((or (equal tool-name "ask_user")
        (not (boundp 'superchat-tools--builtin-tool-names))
        (member tool-name superchat-tools--builtin-tool-names))
    'builtin)
   (t 'outside)))

(defun superchat-sandbox--level-rule (tool-name mode)
  "Return (DECISION . REASON) the level stage gives TOOL-NAME under MODE."
  (pcase (superchat-sandbox-tool-class tool-name)
    ('builtin '(allow))
    ((and class (or 'full-control 'outside 'trusted))
     (pcase mode
       ('danger-full-access '(allow))
       ('read-only
        (cons 'deny
              (if (eq class 'full-control)
                  (format "%s is full Emacs control, which read-only mode does not grant; use /permissions to change the level"
                          tool-name)
                (format "%s runs outside Superchat, which read-only mode does not grant; use /permissions to change the level"
                        tool-name))))
       (_ (if (eq class 'trusted)
              '(allow)
            (cons 'ask
                  (if (eq class 'full-control)
                      (format "%s has full control of Emacs" tool-name)
                    (format "%s runs outside Superchat" tool-name)))))))))

(defconst superchat-sandbox--decision-rank '((deny . 2) (ask . 1) (allow . 0))
  "Strength of the three tool decisions; the stronger one stands.")

(defun superchat-permission-level-decision (tool-name args context &optional scope)
  "Return (:decision DECISION :reason REASON) for one tool call.
DECISION is `allow', `ask' or `deny'.  CONTEXT is the executor's context; its
`:session-id' selects the Session's sandbox mode.  The `tools/pre-execute'
waterfall in SCOPE runs first -- a listener answers by returning the call
with `:kind' set to `allow', `deny' or `ask' and an optional `:reason' -- and
the level rule is then applied as a fixed
stage: the stronger of the two stands, `deny' over `ask' over `allow'.  A
listener that answers `allow' without delegating ends the waterfall only, so
it cannot skip the level rule; a listener's refusal or fault is a denial."
  (let* ((session-id (plist-get context :session-id))
         (call (list :tool-name tool-name
                     :args (superchat-approval-snapshot-value args)
                     :session-id session-id
                     :run-id (plist-get context :run-id)
                     :kind 'allow))
         (answer (superchat-emit scope 'tools/pre-execute call))
         (listener
          (cond
           ((superchat-waterfall-refused-p answer)
            (cons 'deny (format "%s" (superchat-waterfall-refusal-reason answer))))
           ((and (listp answer)
                 (assq (plist-get answer :kind) superchat-sandbox--decision-rank))
            (cons (plist-get answer :kind) (plist-get answer :reason)))
           (t (cons 'deny "a tools/pre-execute listener returned no decision"))))
         (rule (superchat-sandbox--level-rule
                tool-name (superchat-sandbox-effective-mode session-id)))
         (winner (if (> (alist-get (car listener) superchat-sandbox--decision-rank)
                        (alist-get (car rule) superchat-sandbox--decision-rank))
                     listener
                   rule)))
    (list :decision (car winner) :reason (cdr winner))))

;;;; What the model is told

(defun superchat-sandbox-runtime-context-text (session-id)
  "Return the runtime-context snapshot stating SESSION-ID's current policy.
The stable system prompt never states the mode: a model told up front that
it is confined tends not to attempt the work at all.  This snapshot rides
with each request instead and supersedes the ones before it."
  (let ((policy (superchat-sandbox-resolve
                 :session-id session-id
                 :workspace-root (or (bound-and-true-p superchat-tools-default-directory)
                                     default-directory))))
    (concat
     "Current runtime context. This snapshot supersedes earlier runtime-context snapshots.\n\n"
     (pcase (plist-get policy :mode)
       ('read-only
        "Current Superchat file policy: read-only. Any available operation enforced by the Superchat file sandbox cannot modify files in the standing mode. Do not refuse a required modification from this policy alone: try an available tool normally and follow any denial and escalation guidance it returns.")
       ('workspace-write
        (format "Current Superchat file policy: workspace-write. Any available operation enforced by the Superchat file sandbox may modify files under the session workspace: %S. Some platform temporary areas may also be writable."
                (plist-get policy :workspace-root)))
       (_
        "Current Superchat file policy: danger-full-access. The Superchat file sandbox does not restrict file modifications by available operations."))
     "\n\n"
     (if (eq 'never (superchat-approval-effective-policy session-id))
         "Approval prompts are disabled in this session: actions that require approval are rejected automatically — do not request sandbox escalation (do not set `sandbox_permissions`)."
       "Approval policy: ask. Operations that require approval may ask through the configured answerers; without an available answerer, the request fails closed."))))

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
