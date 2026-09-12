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
  layer-owner)  ; Scope whose layers receive this Scope's writes (nil = self)

(defvar superchat-scope-global nil
  "The global Scope, created by `superchat-plugin-reset' at load.")

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

(defun superchat-scope-effect (scope undo)
  "Record thunk UNDO on SCOPE; it runs when SCOPE is disposed."
  (push undo (superchat-scope-effects scope))
  undo)

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
REGISTRY is any `equal'-comparable key, e.g. `service' or `(stage admit)'.
A duplicate KEY within one Scope is an error.  Returns the undo thunk."
  (let* ((owner (or (superchat-scope-layer-owner scope) scope))
         (layer (assoc registry (superchat-scope-layers owner))))
    (unless layer
      (setq layer (list registry))
      (push layer (superchat-scope-layers owner)))
    (when (assoc key (cdr layer))
      (error "superchat-plugin: %S already registers %S in scope %s"
             registry key (superchat-scope-name owner)))
    (push (cons key value) (cdr layer))
    (superchat-scope-effect
     scope
     (lambda ()
       (setf (cdr layer) (cl-remove key (cdr layer) :key #'car :test #'equal))
       (when (null (cdr layer))
         (setf (superchat-scope-layers owner)
               (delq layer (superchat-scope-layers owner))))))))

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
`superchat-register-command', `superchat-authority-register-*',
`superchat-stage-add', …) consult it through `superchat-plugin-record-undo'
so a registration made while a plugin applies is withdrawn when the plugin
unmounts (migration step 1, Scope accounting).")

(defun superchat-plugin-record-undo (undo)
  "Record thunk UNDO on the applying plugin's Scope, if any.  Returns UNDO."
  (when superchat-plugin-current-scope
    (superchat-scope-effect superchat-plugin-current-scope undo))
  undo)

(defun superchat-plugin-add-hook (hook function &optional depth local)
  "`add-hook' whose removal is recorded on the applying plugin's Scope."
  (add-hook hook function depth local)
  (superchat-plugin-record-undo (lambda () (remove-hook hook function local)))
  function)

(cl-defstruct (superchat-plugin (:copier nil))
  name inject config apply doc)

(cl-defstruct (superchat-plugin-fiber (:copier nil))
  plugin scope config state epoch child)
;; state: pending | active | failed.  child = the effect Scope of `apply'.

(defvar superchat-plugin--definitions nil
  "alist NAME -> `superchat-plugin' (definitions are data, never state).")

(defvar superchat-plugin--fibers nil
  "Mounted plugin fibers, in mount order.")

(defcustom superchat-plugins
  '(transport-llm driver tape session-view authority tools chat-view
    chat-commands chat-turn-recorder chat-run-notices closure editor-workspace
    change-set memory experience plan agents-panel compact)
  "Default plugins mounted into the global Scope when Superchat loads.
Each element is a plugin NAME or (NAME . CONFIG-PLIST).  Remove a row before
`(require \\='superchat)' to disable a built-in; `superchat-plugin-mount' and
`superchat-plugin-unmount' do the same at runtime."
  :type '(repeat sexp)
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
    (superchat-scope-effect scope (lambda () (superchat-plugin--unmount fiber)))
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
    (setq superchat-plugin--fibers (delq fiber superchat-plugin--fibers))
    (superchat-plugin--unload fiber)
    (superchat-plugin-notify)))

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

(defun superchat-plugin-report ()
  "List of (NAME STATE SCOPE-NAME) for every mounted plugin."
  (mapcar (lambda (f)
            (list (superchat-plugin-name (superchat-plugin-fiber-plugin f))
                  (superchat-plugin-fiber-state f)
                  (superchat-scope-name (superchat-plugin-fiber-scope f))))
          superchat-plugin--fibers))

(defun superchat-plugin-mount-defaults ()
  "Mount every entry of `superchat-plugins' not yet mounted into the global Scope."
  (dolist (entry superchat-plugins)
    (let ((name (if (consp entry) (car entry) entry))
          (config (and (consp entry) (cdr entry))))
      (unless (superchat-plugin-fiber name superchat-scope-global)
        (if (alist-get name superchat-plugin--definitions)
            (superchat-plugin-mount name config)
          (message "superchat-plugin: %s is listed in `superchat-plugins' but not defined" name))))))

(defun superchat-plugin-reset ()
  "Dispose every Scope and fiber and create a fresh global Scope.
Definitions are kept."
  (when superchat-scope-global (superchat-scope-dispose superchat-scope-global))
  (setq superchat-plugin--fibers nil
        superchat-scope-global (superchat-scope-create "global")))

(unless superchat-scope-global (superchat-plugin-reset))

(provide 'superchat-plugin)
;;; superchat-plugin.el ends here
