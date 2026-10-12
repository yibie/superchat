;;; superchat-sandbox.el --- Permission levels and the buffer capability -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A Session runs at a permission level: a mode (`read-only', `edit-buffers',
;; `danger-full-access') plus an approval policy (`ask', `never', owned by
;; `superchat-approval.el').  A level decides how the tools that exist may
;; execute; it never decides which tools exist -- Preset and Request do.
;;
;; Buffer-first mutation (docs/decisions/implemented/2026-09-18-buffer-first-mutation.md):
;; the model's ordinary write capability is the Buffer.  There is no path
;; sandbox here any more -- no workspace containment, no write escalation.
;; What remains is one small capability boundary for the structured
;; buffer-editing tools: an ordinary buffer may be edited unless the mode is
;; `read-only'; Superchat's own buffers are never edited.  Persisting a buffer
;; is not a default capability: `save_buffer' exists only where a Preset or
;; Request offers it, and the level stage asks about it on every call.
;; `eval_elisp' and the other full-control tools are the escape hatch and are
;; not constrained by any of this once approved.
;;
;; The default mode used to be called `workspace-write'.  That name is accepted
;; wherever a mode or level is read -- user input, the user options, old Tape
;; rows -- and is never written again.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-event)
(require 'superchat-tape)
(require 'superchat-approval)

(declare-function superchat-view-session-configuration "superchat-tape-view" (session-id))

(defconst superchat-sandbox-modes
  '(read-only edit-buffers danger-full-access)
  "The closed modes, narrowest first.
`read-only' observes only: ordinary buffers are not edited.  `edit-buffers'
lets the structured tools edit ordinary buffers and asks about full Emacs
control.  `danger-full-access' asks about nothing except persisting.")

(defconst superchat-sandbox-mode-aliases '((workspace-write . edit-buffers))
  "Former names of modes and levels, accepted on input and never written.")

(defcustom superchat-sandbox-default-mode 'edit-buffers
  "Mode of a Session that has not selected one.
The former name `workspace-write' is still accepted."
  :type '(choice (const read-only) (const edit-buffers)
                 (const danger-full-access))
  :group 'superchat)

(defcustom superchat-permission-levels
  '((edit-buffers
     :sandbox edit-buffers :approval ask
     :description "Edit buffers freely; saving is yours.  Full Emacs control asks every time.")
    (auto
     :sandbox danger-full-access :approval ask
     :description "Full Emacs control without asking.  Saving a buffer still asks, and the model can still put a question to you.")
    (danger-full-access
     :sandbox danger-full-access :approval never
     :description "Full Emacs control without approval prompts.")
    (read-only
     :sandbox read-only :approval ask
     :description "Observe only: buffers are not edited."))
  "Permission levels a Session can select, in declaration order.
Each entry is (NAME :sandbox MODE :approval POLICY :description STRING).
The name `custom' is reserved for knob values that match no entry."
  :type '(alist :key-type symbol :value-type plist)
  :group 'superchat)

(defconst superchat-permission-custom 'custom
  "Derived level name for knob values matching no configured level.
It is reported, never selected.")

;;;; Mode

(defun superchat-sandbox--canonical-name (value)
  "Return VALUE as a symbol with any former name replaced by the current one."
  (let ((symbol (cond ((symbolp value) value)
                      ((stringp value) (intern value)))))
    (or (cdr (assq symbol superchat-sandbox-mode-aliases)) symbol)))

(defun superchat-sandbox-former-name-p (value)
  "Return non-nil when VALUE is a former name such as `workspace-write'."
  (and (assq (cond ((symbolp value) value) ((stringp value) (intern value)))
             superchat-sandbox-mode-aliases)
       t))

(defun superchat-sandbox--mode-symbol (value)
  "Return VALUE as a member of `superchat-sandbox-modes', or nil.
A former name reads as the mode it became, which is how old Tape rows and old
user settings keep meaning what they meant."
  (car (memq (superchat-sandbox--canonical-name value) superchat-sandbox-modes)))

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
  "Return the mode in force for SESSION-ID."
  (or (superchat-sandbox-override session-id)
      (superchat-sandbox--mode-symbol superchat-sandbox-default-mode)
      'edit-buffers))

(defun superchat-sandbox-set-mode (session-id mode &optional pin)
  "Record MODE for SESSION-ID; return non-nil when an event was appended.
The single write path.  Selecting the effective mode again appends nothing.
With PIN non-nil the mode is recorded unless the Session already recorded
it, even when it equals the configured default: a value fixed at a Session's
start must not drift when the default changes later.  The current name is
what is written, whatever name MODE was given by."
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

;;;; Paths Superchat keeps to itself

(defvar superchat--tool-execution-context)
(defvar superchat-data-directory)
(declare-function superchat-editor-workspace-owner-session
                  "superchat-editor-workspace" (buffer))

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

;;;; The buffer capability
;;
;; ordinary user buffer       -> may be edited (not under `read-only')
;; Superchat internal buffer  -> refused, under every mode
;;
;; This is not a sandbox and has no notion of a path.  It is the one place
;; where "Superchat's own buffers are never edited" is enforced.

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

(defun superchat-sandbox--context-session ()
  (let ((context (and (boundp 'superchat--tool-execution-context)
                      (listp superchat--tool-execution-context)
                      superchat--tool-execution-context)))
    (plist-get context :session-id)))

(defun superchat-sandbox-buffer-edit-refusal (target mode)
  "Return why TARGET may not be edited under MODE, or nil when it may.
TARGET is a `superchat-sandbox-buffer-target' plist."
  (cond
   ((eq 'internal (plist-get target :kind))
    (format "[Tool call denied: buffer %s is Superchat's own state, which tools never edit. Nothing was changed.]"
            (plist-get target :name)))
   ((eq mode 'read-only)
    (format "[Tool call denied: this Session is read-only, so %s was not edited. Nothing was changed; the user can change the level with /permissions.]"
            (if (plist-get target :name)
                (format "buffer %s" (plist-get target :name))
              "the target")))))

(cl-defun superchat-sandbox-run-buffer-edit (&key args resolve perform)
  "Run one structured buffer edit.
RESOLVE is called with ARGS and returns (:final TEXT) when there is nothing
to edit -- TEXT is the tool's answer -- or a plist whose `:target' is the
resolved buffer target.  The target is judged by
`superchat-sandbox-buffer-edit-refusal'; PERFORM is then called with ARGS and
the resolved plist and returns the tool's answer.  Nothing is asked: asking
belongs to the level stage and the approval seam."
  (let* ((resolved (funcall resolve args))
         (mode (superchat-sandbox-effective-mode (superchat-sandbox--context-session))))
    (or (plist-get resolved :final)
        (superchat-sandbox-buffer-edit-refusal (plist-get resolved :target) mode)
        (funcall perform args resolved))))

;;;; The fixed level stage

(defconst superchat-sandbox-full-control-tools
  '("eval_elisp" "shell-command" "eshell-command" "bash" "define_tool")
  "Built-in tools with full control of Emacs.
Lisp can change any state and start any process, and a shell can write
anywhere, so these are never treated as confined by a workspace.")

(defconst superchat-sandbox-persisting-tools '("save_buffer")
  "Built-in tools that write a user's buffer to disk.
Persisting is not a default mutation capability: such a tool exists only where
a Preset or Request offers it, and it is asked about on every call under every
mode that allows it at all -- `danger-full-access' included.")

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
  (if (member tool-name superchat-sandbox-persisting-tools)
      ;; "Ask" is the approval seam, so under the policy `never' -- the default
      ;; of `danger-full-access' -- the question cannot be put and the call is
      ;; refused there.  No second approval system exists for this tool.
      (if (eq mode 'read-only)
          (cons 'deny (format "%s writes a buffer to disk, which read-only mode does not grant; use /permissions to change the level"
                              tool-name))
        (cons 'ask (format "%s writes a buffer to disk; saving is normally the user's" tool-name)))
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
                      (format "%s runs outside Superchat" tool-name))))))))))

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
  (concat
   "Current runtime context. This snapshot supersedes earlier runtime-context snapshots.\n\n"
   (pcase (superchat-sandbox-effective-mode session-id)
     ('read-only
      "Current Superchat mode: read-only. You can read buffers and files and open files as buffers; buffers are not edited in this mode.")
     ('edit-buffers
      "Current Superchat mode: edit-buffers. You change things by editing Emacs buffers: open a file as a buffer with open_file and edit the buffer. Your edits stay in the buffer -- saving a buffer to disk is the user's decision, not yours, so say which buffers you changed and that they are unsaved.")
     (_
      "Current Superchat mode: danger-full-access. Tools run without approval prompts. You still change files by editing their buffers; saving a buffer to disk is the user's decision."))
   "\n\n"
   (if (eq 'never (superchat-approval-effective-policy session-id))
       "Approval prompts are disabled in this session: actions that require approval are rejected automatically."
     "Approval policy: ask. Operations that require approval may ask through the configured answerers; without an available answerer, the request fails closed.")))

;;;; Permission levels

(defun superchat-permission-level-names ()
  "Return the selectable level names in declaration order."
  (mapcar #'car superchat-permission-levels))

(defun superchat-permission-level (name)
  "Return level NAME's (:sandbox :approval :description) plist, or signal.
A former name such as `workspace-write' finds the level it became."
  (let ((symbol (superchat-sandbox--canonical-name name)))
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
         ;; An old Tape row may name the level by its former name.
         (recorded (and recorded (superchat-sandbox--canonical-name recorded)))
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
Unknown names signal.  A former name selects the level it became, and the
current name is what is recorded and returned.  The selection is recorded
first, and only when it is not already the effective level; each knob is then
written through its own setter only when its effective value changes."
  (let* ((symbol (superchat-sandbox--canonical-name name))
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
