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
