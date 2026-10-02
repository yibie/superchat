;;; superchat-plugin.el --- Kernel: plugins and Scopes -*- lexical-binding: t; -*-

;;; Commentary:
;; The Plugin / Scope half of the v2 Kernel (docs/design/plugin-architecture.md
;; §1.1-1.2).  A plugin is `name / :inject / :config / :apply'; `apply'
;; receives a Scope and registers everything through it; unmounting the
;; plugin (or disposing the Scope) withdraws every registration.
;;
;; Semantics (from the Cordis research and the ticket-006 prototype):
;; - A dependent plugin is a function of the set of active providers of the
;;   Services it injects (its "epoch").  Any change unwinds its effects and
;;   re-runs `apply'; PENDING is a silent, legal state.
;; - Registrations land in the Scope the plugin was mounted into; its
;;   effects unwind through a private child Scope (`layer-owner').
;; - Lookup from a Scope: global layer, then ancestors farthest -> nearest,
;;   then the Scope's own; the nearest definition wins.
;; - Config is validated on every activation; a change is validate, unload,
;;   re-apply.

;;; Code:

(require 'cl-lib)

;;;; Scope ---------------------------------------------------------------------

(cl-defstruct (superchat-scope (:constructor superchat-scope--make)
                               (:copier nil))
  name parent children
  effects       ; undo thunks, newest first
  layers        ; alist REGISTRY -> alist KEY -> VALUE (this Scope's layer)
  layer-owner   ; Scope whose layers receive this Scope's writes (nil = self)
  ;; A process-private execution admission latch.  It deliberately is not
  ;; Scope disposal state: an owner retires execution before it later chooses
  ;; whether to dispose registrations.
  execution-retired)

(defvar superchat-scope-global nil
  "The global Scope, created by `superchat-plugin-reset' at load.")

(defvar superchat-execution-scope nil
  "Borrowed Scope of the current execution, or nil outside one.

This is distinct from `superchat-plugin-current-scope': the latter is only
the effect Scope while a plugin applies and determines where a registration
lands.  This variable carries a caller's already-existing Scope through a
driver execution.  It never owns, disposes, or serializes that Scope.")

(defun superchat-execution-scope-resolve (&optional scope)
  "Return explicit execution SCOPE, the inherited one, or the global Scope.

An explicit `superchat-scope-global' deliberately resets a nested execution
to global.  A nil SCOPE inherits `superchat-execution-scope' when one is
bound, and means global only when there is no enclosing execution.  This is
the one Kernel source of execution-Scope inheritance; it is not a registry."
  (or scope superchat-execution-scope superchat-scope-global))

(defun superchat-scope-create (name &optional parent layer-owner)
  "Create Scope NAME under PARENT.  LAYER-OWNER, when given, receives the
registrations made through the new Scope (used for plugin effect Scopes)."
  (let ((scope (superchat-scope--make :name name :parent parent
                                      :layer-owner layer-owner)))
    (when parent (push scope (superchat-scope-children parent)))
    scope))

(defun superchat-scope-chain (scope)
  "SCOPE and its ancestors, farthest first."
  (let (chain)
    (while scope
      (push scope chain)
      (setq scope (superchat-scope-parent scope)))
    chain))

(defun superchat-execution-scope-retire (scope)
  "Permanently close explicit non-global SCOPE to new execution.

This is an execution latch only.  It neither disposes SCOPE nor grants Scope
disposal any Runtime ownership.  A fresh same-named Scope is a different
object and remains eligible."
  (unless (and (superchat-scope-p scope)
               (not (eq scope superchat-scope-global)))
    (error "Execution retirement requires an explicit non-global Scope"))
  (setf (superchat-scope-execution-retired scope) t)
  scope)

(defun superchat-execution-scope-retired-p (scope)
  "Return non-nil when SCOPE or an ancestor is execution-retired.

The query is identity/ancestry based and process-local; it does not infer a
Scope from a name, Session, Preset, or public Request data."
  (and (superchat-scope-p scope)
       (cl-some #'superchat-scope-execution-retired
                (superchat-scope-chain scope))))

(defun superchat-scope-effect (scope undo)
  "Record UNDO on SCOPE and return its idempotent, self-removing undo.
Manual invocation and Scope disposal use the same operation.  Detach and
release UNDO before calling it, so reentry cannot execute it twice and a
retained completed undo does not retain its original cleanup closure."
  (let (effect)
    (setq effect
          (lambda ()
            (when undo
              (let ((action undo))
                (setq undo nil)
                (setf (superchat-scope-effects scope)
                      (delq effect (superchat-scope-effects scope)))
                (funcall action)))))
    (push effect (superchat-scope-effects scope))
    effect))

(defun superchat-scope-dispose (scope)
  "Dispose SCOPE: children first, then its effects newest-first, then unlink."
  (dolist (child (copy-sequence (superchat-scope-children scope)))
    (superchat-scope-dispose child))
  (let ((effects (superchat-scope-effects scope)))
    (setf (superchat-scope-effects scope) nil)
    (dolist (undo effects)
      (condition-case err
          (funcall undo)
        (error (message "superchat-plugin: undo in scope %s failed: %s"
                        (superchat-scope-name scope) (error-message-string err))))))
  (when-let* ((parent (superchat-scope-parent scope)))
    (setf (superchat-scope-children parent)
          (delq scope (superchat-scope-children parent)))))

;;;; Scoped registries ---------------------------------------------------------

(defun superchat-scope-register (scope registry key value)
  "Register KEY -> VALUE in SCOPE's REGISTRY layer and record the undo.
REGISTRY is any `equal'-comparable key, e.g. `service' or `(event pre-step)'.
A duplicate KEY within one Scope is an error.  Returns the undo thunk."
  (let* ((owner (or (superchat-scope-layer-owner scope) scope))
         (layer (assoc registry (superchat-scope-layers owner))))
    (unless layer
      (setq layer (list registry))
      (push layer (superchat-scope-layers owner)))
    (when (assoc key (cdr layer))
      (error "superchat-plugin: %S already registers %S in scope %s"
             registry key (superchat-scope-name owner)))
    (let ((registration (cons key value)))
      (push registration (cdr layer))
      (superchat-scope-effect
       scope
       (lambda ()
         (setf (cdr layer) (delq registration (cdr layer)))
         (when (null (cdr layer))
           (setf (superchat-scope-layers owner)
                 (delq layer (superchat-scope-layers owner)))))))))

(defun superchat-scope-lookup (scope registry key)
  "Nearest-wins lookup of KEY in REGISTRY as seen from SCOPE."
  (let (found)
    (dolist (s (superchat-scope-chain scope))
      (when-let* ((layer (assoc registry (superchat-scope-layers s)))
                  (hit (assoc key (cdr layer))))
        (setq found hit)))
    (cdr found)))

(defun superchat-scope-merge (scope registry)
  "Every KEY -> VALUE of REGISTRY visible from SCOPE, nearest shadowing.
Within one layer the order is registration order."
  (let (acc)
    (dolist (s (superchat-scope-chain scope))
      (when-let* ((layer (assoc registry (superchat-scope-layers s))))
        (dolist (entry (reverse (cdr layer)))
          (setq acc (cons entry (cl-remove (car entry) acc :key #'car :test #'equal))))))
    (nreverse acc)))

(defun superchat-scope-empty-p (&optional scope)
  "Non-nil when SCOPE (default global) and its descendants hold no layer."
  (cl-labels ((walk (s) (and (null (superchat-scope-layers s))
                             (cl-every #'walk (superchat-scope-children s)))))
    (walk (or scope superchat-scope-global))))

;;;; Plugins -------------------------------------------------------------------

(defvar superchat-plugin-current-scope nil
  "The effect Scope of the plugin whose `apply' is running, or nil.
Registering functions that predate the Kernel (`superchat-register-tool',
`superchat-register-command',
`superchat-context-register', `superchat-on', …) consult it through
`superchat-plugin-record-undo' so a registration made while a plugin
applies is withdrawn when the plugin unmounts (migration step 1, Scope
accounting).")

(defun superchat-registration-scope-resolve (&optional scope)
  "Return the target Scope for a product registration.

An explicit SCOPE wins.  Otherwise the applying plugin's effect Scope owns
the registration, followed by the borrowed execution Scope and finally the
global Scope.  This is intentionally separate from Service provider lookup:
a global provider called while a plugin applies must not steal that plugin's
registration ownership."
  (or scope superchat-plugin-current-scope
      (superchat-execution-scope-resolve nil)))

(defun superchat-scope-local-lookup (scope registry key)
  "Return KEY in REGISTRY from SCOPE's own target layer only.

SCOPE is resolved by `superchat-registration-scope-resolve'.  Its
`layer-owner', when present, is the local layer; ancestors and siblings are
never searched.  Product unregister operations use this exact lookup so they
cannot withdraw a visible parent installation."
  (let* ((target (superchat-registration-scope-resolve scope))
         (owner (or (superchat-scope-layer-owner target) target))
         (layer (assoc registry (superchat-scope-layers owner))))
    (cdr (assoc key (cdr layer)))))

(defun superchat-plugin-record-undo (undo)
  "Record UNDO on the applying plugin's Scope, returning its one-shot undo.
Without an applying Scope, return UNDO unchanged."
  (if superchat-plugin-current-scope
      (superchat-scope-effect superchat-plugin-current-scope undo)
    undo))

(defun superchat-plugin-add-hook (hook function &optional depth local)
  "`add-hook' whose removal is recorded on the applying plugin's Scope."
  (add-hook hook function depth local)
  (superchat-plugin-record-undo (lambda () (remove-hook hook function local)))
  function)

(cl-defstruct (superchat-plugin (:copier nil))
  name inject config apply doc)

(cl-defstruct (superchat-plugin-fiber (:copier nil))
  plugin scope config state epoch child cleanup)
;; state: pending | active | failed.  child = the effect Scope of `apply'.
;; cleanup = the mount Scope's exact, self-removing lifetime undo.

(defvar superchat-plugin--definitions nil
  "alist NAME -> `superchat-plugin' (definitions are data, never state).")

(defvar superchat-plugin--fibers nil
  "Mounted plugin fibers, in mount order.")

(defcustom superchat-plugins
  '(transport-llm driver tape session-view tools tools-builtin
    tools-baseline prompt-assembler chat-collectors
    chat-commands chat-turn-recorder chat-run-notices changes chat
    editor-workspace change-set
    inline-ask inline-act inline-rewrite org-babel magit emacsclient
    memory experience plan agents-panel compact
    subagent workflow workspace workspace-resolution)
  "The `default' profile's one row layer, mounted when Superchat loads.
Each element is a plugin NAME or (NAME . CONFIG-PLIST).  Remove a row before
`(require \='superchat)' to disable a built-in; `superchat-plugin-mount' and
`superchat-plugin-unmount' do the same at runtime."
  :type '(repeat sexp)
  :group 'superchat)

(defcustom superchat-profiles
  '((headless (:inherit default)
              ((chat . :disabled)
               (chat-commands . :disabled)
               (chat-turn-recorder . :disabled)
               (chat-run-notices . :disabled)
               (chat-collectors . :disabled)
               (inline . :disabled)
               (inline-ask . :disabled)
               (inline-act . :disabled)
               (inline-rewrite . :disabled)
               (org-babel . :disabled)
               (magit . :disabled)
               (cowork-telega . :disabled))))
  "Alist mapping a profile name to an ordered list of layers.
Each layer is a list of entries.  An entry is NAME, (NAME . CONFIG), or
(NAME . :disabled).  A layer may also be (:inherit NAME), which appends the
resolved rows of NAME.  The `default' profile is always resolved from the
customisable `superchat-plugins' value, so existing configurations keep
working; other profiles layer over it with a `(:inherit default)' layer.
The `headless' profile keeps the batch/runtime rows and disables the
user-interface entry rows, including Telega when it is present."
  :type '(alist :key-type symbol :value-type (repeat sexp))
  :group 'superchat)

(defcustom superchat-profile 'default
  "The profile `(require \='superchat)' mounts.
Its default value is the built-in `default' profile."
  :type 'symbol
  :group 'superchat)

(defmacro superchat-define-plugin (name &rest props)
  "Define plugin NAME.  PROPS: `:inject' (list of Service names),
`:config' ((KEY PREDICATE DEFAULT [DOC]) …), `:apply' (lambda (scope config)),
`:doc' STRING.  Defining is data only; nothing runs until it is mounted."
  (declare (indent 1))
  `(progn
     (setf (alist-get ',name superchat-plugin--definitions)
           (make-superchat-plugin :name ',name
                                  :inject ,(plist-get props :inject)
                                  :config ,(plist-get props :config)
                                  :apply ,(plist-get props :apply)
                                  :doc ,(plist-get props :doc)))
     ',name))

(defun superchat-plugin-definition (name)
  (or (alist-get name superchat-plugin--definitions)
      (error "superchat-plugin: no plugin named %s" name)))

(defun superchat-plugin-validate-config (plugin config)
  "Return CONFIG filled with PLUGIN's defaults, or signal on a bad value.
Unknown keys are rejected."
  (let ((spec (superchat-plugin-config plugin))
        (out (copy-sequence config)))
    (cl-loop for (key _) on config by #'cddr
             unless (assq key spec)
             do (error "superchat-plugin: %s does not accept config key %s"
                       (superchat-plugin-name plugin) key))
    (dolist (entry spec out)
      (let* ((key (nth 0 entry)) (pred (nth 1 entry)) (default (nth 2 entry))
             (value (if (plist-member config key) (plist-get config key) default)))
        (unless (funcall pred value)
          (error "superchat-plugin: invalid config %s=%S for %s"
                 key value (superchat-plugin-name plugin)))
        (setq out (plist-put out key value))))))

(defun superchat-plugin--epoch (fiber)
  "Provider ids of FIBER's injected Services, or `inactive'."
  (let (ids)
    (catch 'inactive
      (dolist (name (superchat-plugin-inject (superchat-plugin-fiber-plugin fiber)))
        (let ((provider (superchat-scope-lookup (superchat-plugin-fiber-scope fiber)
                                                'service name)))
          (unless provider (throw 'inactive 'inactive))
          (push (plist-get provider :id) ids)))
      (nreverse ids))))

(defun superchat-plugin--load (fiber)
  (let ((plugin (superchat-plugin-fiber-plugin fiber)))
    (condition-case err
        (let* ((config (superchat-plugin-validate-config
                        plugin (superchat-plugin-fiber-config fiber)))
               (mount (superchat-plugin-fiber-scope fiber))
               (child (superchat-scope-create
                       (format "plugin/%s" (superchat-plugin-name plugin)) mount mount)))
          (setf (superchat-plugin-fiber-child fiber) child)
          (let ((superchat-plugin-current-scope child))
            (funcall (superchat-plugin-apply plugin) child config))
          (setf (superchat-plugin-fiber-state fiber) 'active))
      (error
       (setf (superchat-plugin-fiber-state fiber) 'failed)
       (message "superchat-plugin: %s failed: %s"
                (superchat-plugin-name plugin) (error-message-string err))
       (when (superchat-plugin-fiber-child fiber)
         (superchat-scope-dispose (superchat-plugin-fiber-child fiber))
         (setf (superchat-plugin-fiber-child fiber) nil))))))

(defun superchat-plugin--unload (fiber)
  (when (superchat-plugin-fiber-child fiber)
    (superchat-scope-dispose (superchat-plugin-fiber-child fiber))
    (setf (superchat-plugin-fiber-child fiber) nil))
  (setf (superchat-plugin-fiber-state fiber) 'pending))

(defvar superchat-plugin--notifying nil)

(defun superchat-plugin-notify ()
  "Re-check every fiber's epoch; unwind and re-apply the ones that changed.
Called by `superchat-service-provide' and its undo.  Synchronous fix-point
with a re-entrancy guard: a provider that appears during another plugin's
`apply' is picked up on the next pass."
  (unless superchat-plugin--notifying
    (let ((superchat-plugin--notifying t) changed)
      (while (progn
               (setq changed nil)
               (dolist (fiber (copy-sequence superchat-plugin--fibers))
                 (let ((new (superchat-plugin--epoch fiber)))
                   (unless (equal new (superchat-plugin-fiber-epoch fiber))
                     (setq changed t)
                     (setf (superchat-plugin-fiber-epoch fiber) new)
                     (when (superchat-plugin-fiber-child fiber)
                       (superchat-plugin--unload fiber))
                     (unless (eq new 'inactive)
                       (superchat-plugin--load fiber)))))
               changed)))))

(defun superchat-plugin-mount (name &optional config scope)
  "Mount plugin NAME with CONFIG into SCOPE (default global); return its fiber.
The fiber dies with SCOPE."
  (let* ((plugin (superchat-plugin-definition name))
         (scope (or scope superchat-scope-global))
         (fiber (make-superchat-plugin-fiber :plugin plugin :scope scope
                                             :config config :state 'pending
                                             :epoch 'unset)))
    (when (superchat-plugin-fiber name scope)
      (error "superchat-plugin: %s is already mounted in scope %s"
             name (superchat-scope-name scope)))
    (setq superchat-plugin--fibers (append superchat-plugin--fibers (list fiber)))
    ;; Keep the exact Scope effect that owns this fiber.  A manual unmount
    ;; invokes this same one-shot cleanup, rather than privately unloading the
    ;; fiber and leaving its lifetime closure on SCOPE.  Clear the fiber's
    ;; reference before the cleanup reaches unload/notify user code; an old
    ;; retained cleanup is then inert and cannot affect a later same-name
    ;; fiber.
    (setf (superchat-plugin-fiber-cleanup fiber)
          (superchat-scope-effect
           scope
           (lambda ()
             (setf (superchat-plugin-fiber-cleanup fiber) nil)
             (superchat-plugin--unmount fiber))))
    (superchat-plugin-notify)
    fiber))

(defun superchat-plugin-fiber (name &optional scope)
  "The fiber of plugin NAME mounted in SCOPE (any Scope when nil), or nil."
  (cl-find-if (lambda (f)
                (and (eq name (superchat-plugin-name (superchat-plugin-fiber-plugin f)))
                     (or (null scope) (eq scope (superchat-plugin-fiber-scope f)))))
              superchat-plugin--fibers))

(defun superchat-plugin--unmount (fiber)
  (when (memq fiber superchat-plugin--fibers)
    (if-let* ((cleanup (superchat-plugin-fiber-cleanup fiber)))
        ;; `superchat-scope-effect' detaches itself and clears its action
        ;; before it calls the lambda installed by `superchat-plugin-mount'.
        ;; Drop the fiber's reference first as well: unload and notify may
        ;; reenter plugin code.
        (progn
          (setf (superchat-plugin-fiber-cleanup fiber) nil)
          (funcall cleanup))
      (setq superchat-plugin--fibers (delq fiber superchat-plugin--fibers))
      (superchat-plugin--unload fiber)
      (superchat-plugin-notify))))

(defun superchat-plugin-unmount (name &optional scope)
  "Unmount plugin NAME (from SCOPE, or wherever it is mounted)."
  (let ((fiber (superchat-plugin-fiber name scope)))
    (unless fiber (error "superchat-plugin: %s is not mounted" name))
    (superchat-plugin--unmount fiber)
    name))

(defun superchat-plugin-reconfigure (name config &optional scope)
  "Validate CONFIG, then unwind and re-apply plugin NAME with it."
  (let ((fiber (or (superchat-plugin-fiber name scope)
                   (error "superchat-plugin: %s is not mounted" name))))
    (superchat-plugin-validate-config (superchat-plugin-fiber-plugin fiber) config)
    (setf (superchat-plugin-fiber-config fiber) config)
    (superchat-plugin--unload fiber)
    (unless (eq (superchat-plugin-fiber-epoch fiber) 'inactive)
      (superchat-plugin--load fiber))
    fiber))

(defun superchat-plugin-state (name &optional scope)
  "State of plugin NAME: `pending', `active', `failed', or nil when unmounted."
  (when-let* ((fiber (superchat-plugin-fiber name scope)))
    (superchat-plugin-fiber-state fiber)))

(define-error 'superchat-surface-unmounted
  "Superchat surface row is not mounted" 'user-error)

(defun superchat-plugin-row-active-p (name &optional scope)
  "Return non-nil when plugin NAME is mounted and active in SCOPE.
SCOPE defaults to the global Scope, because profile rows live there."
  (eq 'active (superchat-plugin-state name (or scope superchat-scope-global))))

(defun superchat-plugin-require-active-row (name)
  "Signal `superchat-surface-unmounted' unless row NAME is active globally.
Surface entry points call this before building an envelope or touching a
Service; the row may be unmounted or waiting for its own `:inject' deps."
  (let ((state (superchat-plugin-state name superchat-scope-global)))
    (unless (eq state 'active)
      (signal 'superchat-surface-unmounted
              (list (format "Superchat %s is not mounted (state: %s; active profile: %s)"
                            name (or state 'unmounted) superchat-profile))))))

(defun superchat-plugin-report ()
  "List of (NAME STATE SCOPE-NAME) for every mounted plugin."
  (mapcar (lambda (f)
            (list (superchat-plugin-name (superchat-plugin-fiber-plugin f))
                  (superchat-plugin-fiber-state f)
                  (superchat-scope-name (superchat-plugin-fiber-scope f))))
          superchat-plugin--fibers))

(defun superchat-plugin-mount-defaults ()
  "Compatibility entry point that mounts `superchat-profile'.

Kept for bundles that load after Superchat and append their own row before
calling it.  New code should call `superchat-profile-mount'."
  (superchat-profile-mount))

(defun superchat-plugin-reset ()
  "Dispose every Scope and fiber and create a fresh global Scope.
Definitions are kept."
  (when superchat-scope-global (superchat-scope-dispose superchat-scope-global))
  (setq superchat-plugin--fibers nil
        superchat-scope-global (superchat-scope-create "global")))

(unless superchat-scope-global (superchat-plugin-reset))

;;;; Profiles ------------------------------------------------------------------

(defvar superchat-profile--resolving nil
  "Profile names currently being resolved, innermost first.
Used only to turn an inheritance cycle into a clear error.")

(defun superchat-profile--entry-name (entry)
  "Return the row NAME of a profile ENTRY."
  (if (consp entry) (car entry) entry))

(defun superchat-profile--disabled-entry-p (entry)
  "Non-nil when ENTRY is the disable instruction (NAME . :disabled)."
  (and (consp entry) (eq (cdr entry) :disabled)))

(defun superchat-profile--apply-layer (rows layer)
  "Apply one profile LAYER over ROWS and return the new ordered row list.
LAYER is a list of entries, or (:inherit NAME).  Disable entries remove the
named row; every other entry appends or replaces that row in place, so a
later reordering of the base cannot silently resurrect a disabled row."
  (cond
   ((and (consp layer) (eq (car layer) :inherit))
    (let ((inherited (superchat-profile-rows (cadr layer))))
      (append
       rows
       (cl-remove-if
        (lambda (entry)
          (cl-find (superchat-profile--entry-name entry)
                   rows :key #'superchat-profile--entry-name :test #'eq))
        inherited))))
   ((null layer) rows)
   ((listp layer)
    (dolist (entry layer rows)
      (let ((name (superchat-profile--entry-name entry)))
        (if (superchat-profile--disabled-entry-p entry)
            (setq rows (cl-remove name rows
                                  :key #'superchat-profile--entry-name
                                  :test #'eq))
          ;; Patch in place: an existing row keeps its position when a
          ;; later layer changes its config or marks it disabled.
          (let ((cell rows) (found nil))
            (while (and cell (not found))
              (if (eq name (superchat-profile--entry-name (car cell)))
                  (progn (setcar cell entry) (setq found t))
                (setq cell (cdr cell))))
            (unless found
              (setq rows (append rows (list entry)))))))))
   (t
    (error "superchat-profile: invalid layer: %S" layer))))

(defun superchat-profile-rows (&optional name)
  "Return the resolved, ordered rows of profile NAME.

NAME defaults to `superchat-profile'.  The `default' profile begins from the
customisable `superchat-plugins' layer; other profiles apply their
`superchat-profiles' layers in order.  Rows disabled by a later layer are
absent from the result."
  (let ((name (or name superchat-profile 'default)))
    (when (memq name superchat-profile--resolving)
      (error "superchat-profile: inheritance cycle through %s" name))
    (let ((superchat-profile--resolving
           (cons name superchat-profile--resolving)))
      (cond
       ((eq name 'default)
        (superchat-profile--apply-layer nil superchat-plugins))
       ((not (assq name superchat-profiles))
        (error "superchat-profile: unknown profile %s" name))
       (t
        (let ((rows nil)
              (layers (cdr (assq name superchat-profiles))))
          (dolist (layer layers rows)
            (setq rows (superchat-profile--apply-layer rows layer)))))))))

(defun superchat-profile--global-fibers ()
  "Return the plugin fibers mounted directly in the global Scope, in mount order."
  (cl-remove-if-not
   (lambda (fiber)
     (eq (superchat-plugin-fiber-scope fiber) superchat-scope-global))
   superchat-plugin--fibers))

(defun superchat-profile-mount (&optional name)
  "Mount the resolved rows of profile NAME as the exact global composition.

NAME defaults to `superchat-profile'.  A row the profile does not name is
unmounted, not left over from the profile mounted before.  Extra rows are
unmounted in reverse mount order so later overlays come down before the base
rows they were layered on; rows the profile names are then mounted in
profile order.  Pending or failed rows are reported with one warning.  An
unmount failure is collected, the remaining rows are still processed, and a
final error names the rows whose cleanup failed rather than pretending the
composition is exact."
  (let* ((name (or name superchat-profile 'default))
         (rows (superchat-profile-rows name))
         (target (mapcar #'superchat-profile--entry-name rows))
         (mounted (mapcar (lambda (fiber)
                            (superchat-plugin-name
                             (superchat-plugin-fiber-plugin fiber)))
                          (superchat-profile--global-fibers)))
         (extras (cl-remove-if (lambda (row) (memq row target)) mounted))
         (failures nil))
    ;; Remove rows the target profile does not name, newest mount first.
    (dolist (row (reverse mounted))
      (when (memq row extras)
        (condition-case err
            (when (superchat-plugin-fiber row superchat-scope-global)
              (superchat-plugin-unmount row superchat-scope-global))
          (error (push (cons row err) failures)))))
    ;; Mount every target row that is absent, in profile order.
    (dolist (entry rows)
      (let ((row (superchat-profile--entry-name entry))
            (config (and (consp entry) (cdr entry))))
        (if (alist-get row superchat-plugin--definitions)
            (unless (superchat-plugin-fiber row superchat-scope-global)
              (condition-case err
                  (superchat-plugin-mount row config)
                (error (push (cons row err) failures))))
          (progn
            (message "superchat-profile: %s names undefined row %s" name row)
            (push (cons row 'undefined-row) failures)))))
    ;; One visible report after the whole switch, including a failed mount.
    (let ((not-active
           (cl-remove-if
            (lambda (entry) (eq 'active (nth 1 entry)))
            (superchat-profile-report name))))
      (when not-active
        (display-warning
         'superchat-profile
         (format "Profile %s has non-active rows: %s"
                 name
                 (mapconcat
                  (lambda (entry)
                    (format "%s (%s%s)"
                            (nth 0 entry)
                            (nth 1 entry)
                            (if (nth 2 entry)
                                (format " waiting for %s"
                                        (mapconcat #'symbol-name (nth 2 entry) ", "))
                              "")))
                  not-active "; "))
         :warning)))
    (when failures
      (error "Superchat profile %s could not be mounted exactly: %s"
             name
             (mapconcat (lambda (failure)
                          (format "%s (%s)" (car failure) (cdr failure)))
                        (nreverse failures) "; ")))
    name))

(defun superchat-profile-report (&optional name)
  "Return one (NAME STATE MISSING-SERVICES) entry per row of profile NAME.

NAME defaults to `superchat-profile'.  STATE is the mounted plugin state, or
`unmounted'; MISSING-SERVICES is the injected Services a pending row is still
waiting for, and nil otherwise."
  (let ((name (or name superchat-profile 'default)))
    (mapcar
     (lambda (entry)
       (let* ((row (superchat-profile--entry-name entry))
              (plugin (alist-get row superchat-plugin--definitions))
              (state (or (superchat-plugin-state row superchat-scope-global)
                         'unmounted))
              (missing
               (when (and plugin (eq state 'pending))
                 (cl-remove-if
                  (lambda (service)
                    (superchat-scope-lookup superchat-scope-global
                                            'service service))
                  (superchat-plugin-inject plugin)))))
         (list row state missing)))
     (superchat-profile-rows name))))

(provide 'superchat-plugin)
;;; superchat-plugin.el ends here
