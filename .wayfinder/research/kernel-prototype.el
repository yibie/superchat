;;; sk-proto.el --- throwaway Kernel primitives prototype (ticket 006) -*- lexical-binding: t; -*-

;; Plugin / Scope / Service / Event / Stage with reversible registration.
;; Goal: mount -> register -> unmount leaves no trace, no `unload-feature'.
;; Semantics copied from .wayfinder/research/cordis-semantics.md:
;;  - dependent = function of the set of active providers (epoch); any change
;;    = unwind effects, then re-run `apply'; PENDING is a silent legal state.
;;  - scoped lookup: global, then farthest -> nearest ancestor layer, own last.
;;  - config validated on every activation; change = validate, unload, re-apply.

(require 'cl-lib)

;;;; Scope ----------------------------------------------------------------

(cl-defstruct (sk-scope (:constructor sk-scope--make))
  name parent children
  effects            ; list of undo thunks, newest first
  layers             ; alist REGISTRY-NAME -> alist of entries (this scope's layer)
  layer-owner)       ; Scope whose layers this one writes to (nil = itself).
;; A plugin's `apply' receives a private child Scope so its effects can be
;; unwound alone, but its *registrations* land in the Scope it was mounted
;; into (Cordis: a fiber's ctx carries the mount scope's key), otherwise
;; siblings and the driver could never see them.

(defvar sk--root nil "The global Scope.")
(defvar sk--fibers nil "All mounted plugin fibers (list of sk-fiber).")

(defun sk-scope-create (name &optional parent layer-owner)
  (let ((s (sk-scope--make :name name :parent parent :layer-owner layer-owner)))
    (when parent (push s (sk-scope-children parent)))
    s))

(defun sk-scope-chain (scope)
  "SCOPE and its ancestors, farthest first."
  (let (chain) (while scope (push scope chain) (setq scope (sk-scope-parent scope))) chain))

(defun sk-scope-effect (scope undo)
  "Record UNDO on SCOPE; it runs (in reverse order) when SCOPE is disposed."
  (push undo (sk-scope-effects scope)))

(defun sk-scope-dispose (scope)
  "Dispose SCOPE: children first, then its effects newest-first, then unlink."
  (dolist (child (copy-sequence (sk-scope-children scope))) (sk-scope-dispose child))
  (dolist (undo (sk-scope-effects scope)) (funcall undo))
  (setf (sk-scope-effects scope) nil)
  (when-let* ((p (sk-scope-parent scope)))
    (setf (sk-scope-children p) (delq scope (sk-scope-children p)))))

;;;; Scoped registries -------------------------------------------------------

(defun sk--layer-put (scope registry key value)
  "Register KEY->VALUE in SCOPE's REGISTRY layer; return an undo thunk.
The layer is created on first write and deleted when it becomes empty."
  (setq scope (or (sk-scope-layer-owner scope) scope))
  (let ((layer (assoc registry (sk-scope-layers scope))))
    (unless layer
      (setq layer (list registry))
      (push layer (sk-scope-layers scope)))
    (when (assoc key (cdr layer))
      (error "sk: %s already registers %S in scope %s" registry key (sk-scope-name scope)))
    (push (cons key value) (cdr layer))
    (lambda ()
      (setf (cdr layer) (cl-remove key (cdr layer) :key #'car :test #'equal))
      (when (null (cdr layer))
        (setf (sk-scope-layers scope) (delq layer (sk-scope-layers scope)))))))

(defun sk-registry-lookup (scope registry key)
  "Nearest-wins lookup of KEY in REGISTRY seen from SCOPE."
  (let (found)
    (dolist (s (sk-scope-chain scope))
      (when-let* ((layer (assoc registry (sk-scope-layers s)))
                  (hit (assoc key (cdr layer))))
        (setq found hit)))
    (cdr found)))

(defun sk-registry-merge (scope registry)
  "All KEY->VALUE pairs of REGISTRY visible from SCOPE, nearest shadowing."
  (let (acc)
    (dolist (s (sk-scope-chain scope))
      (when-let* ((layer (assoc registry (sk-scope-layers s))))
        (dolist (e (reverse (cdr layer)))
          (setq acc (cons e (cl-remove (car e) acc :key #'car :test #'equal))))))
    (nreverse acc)))

(defun sk-registry-empty-p ()
  "Non-nil when no scope in the tree holds any layer."
  (cl-labels ((walk (s) (and (null (sk-scope-layers s))
                             (cl-every #'walk (sk-scope-children s)))))
    (walk sk--root)))

;;;; Services -----------------------------------------------------------------

(defvar sk--service-defs nil "alist NAME -> required method list.")

(defun sk-define-service (name methods) (push (cons name methods) sk--service-defs))

(defun sk-provide (scope name table)
  "Provide service NAME with TABLE (alist METHOD -> FN) in SCOPE.
Missing methods are a mount-time error.  Notifies dependents."
  (let ((required (cdr (assq name sk--service-defs))))
    (unless required (error "sk: unknown service %s" name))
    (dolist (m required)
      (unless (functionp (cdr (assq m table)))
        (error "sk: provider of %s lacks method %s" name m))))
  (let ((undo (sk--layer-put scope 'service name (list :table table :id (gensym "prov")))))
    (sk-scope-effect scope (lambda () (funcall undo) (sk--notify)))
    (sk--notify)))

(defun sk-service-call (scope name method &rest args)
  (let ((prov (sk-registry-lookup scope 'service name)))
    (unless prov (error "sk: no provider for service %s" name))
    (apply (cdr (assq method (plist-get prov :table))) args)))

;;;; Events (4 combination rules) and Stages (fixed main line) --------------

(defvar sk--event-defs nil "alist NAME -> rule (first|collect|fold|observe).")
(defconst sk-stage-names '(admit collect-context assemble-prompt run-model render))
(defvar sk--stage-rules '((admit . first) (collect-context . collect)
                          (assemble-prompt . first) (run-model . first) (render . observe)))

(defun sk-define-event (name rule) (push (cons name rule) sk--event-defs))

(defun sk-on (scope event id fn)
  (unless (assq event sk--event-defs) (error "sk: unknown event %s" event))
  (sk-scope-effect scope (sk--layer-put scope (list 'event event) id fn)))

(defun sk-stage-add (scope stage id fn)
  (unless (memq stage sk-stage-names) (error "sk: unknown stage %s" stage))
  (sk-scope-effect scope (sk--layer-put scope (list 'stage stage) id fn)))

(defun sk--combine (rule impls args)
  (pcase rule
    ('first (cl-some (lambda (i) (apply (cdr i) args)) impls))
    ('collect (mapcar (lambda (i) (apply (cdr i) args)) impls))
    ('fold (let ((acc (car args)))
             (dolist (i impls acc) (setq acc (apply (cdr i) acc (cdr args))))))
    ('observe (dolist (i impls) (apply (cdr i) args)) nil)))

(defun sk-emit (scope event &rest args)
  (sk--combine (cdr (assq event sk--event-defs))
               (sk-registry-merge scope (list 'event event)) args))

(defun sk-stage-call (scope stage &rest args)
  (sk--combine (cdr (assq stage sk--stage-rules))
               (sk-registry-merge scope (list 'stage stage)) args))

;;;; Plugins and fibers ------------------------------------------------------

(cl-defstruct sk-plugin name inject config apply)
(cl-defstruct sk-fiber plugin scope config state epoch child)
;; state: pending | active | failed.  child = the Scope `apply' registered into.

(defvar sk-plugins nil "alist NAME -> sk-plugin (the definitions).")

(defmacro sk-define-plugin (name &rest props)
  `(setf (alist-get ',name sk-plugins)
         (make-sk-plugin :name ',name :inject ,(plist-get props :inject)
                         :config ,(plist-get props :config) :apply ,(plist-get props :apply))))

(defun sk--validate-config (plugin config)
  "CONFIG spec = alist KEY -> (PREDICATE . DEFAULT).  Returns filled config."
  (let ((out (copy-sequence config)))
    (dolist (spec (sk-plugin-config plugin) out)
      (let* ((key (car spec)) (pred (cadr spec)) (default (cddr spec))
             (val (if (plist-member config key) (plist-get config key) default)))
        (unless (funcall pred val)
          (error "sk: invalid config %s=%S for plugin %s" key val (sk-plugin-name plugin)))
        (setq out (plist-put out key val))))))

(defun sk--epoch (fiber)
  "List of provider ids for FIBER's injected services, or `inactive'."
  (let (ids)
    (catch 'inactive
      (dolist (name (sk-plugin-inject (sk-fiber-plugin fiber)))
        (let ((p (sk-registry-lookup (sk-fiber-scope fiber) 'service name)))
          (unless p (throw 'inactive 'inactive))
          (push (plist-get p :id) ids)))
      (nreverse ids))))

(defun sk--load (fiber)
  (condition-case err
      (let* ((plugin (sk-fiber-plugin fiber))
             (config (sk--validate-config plugin (sk-fiber-config fiber)))
             (child (sk-scope-create (format "%s" (sk-plugin-name plugin))
                                     (sk-fiber-scope fiber) (sk-fiber-scope fiber))))
        (setf (sk-fiber-child fiber) child)
        (funcall (sk-plugin-apply plugin) child config)
        (setf (sk-fiber-state fiber) 'active))
    (error (setf (sk-fiber-state fiber) 'failed)
           (message "sk: plugin %s failed: %s" (sk-plugin-name (sk-fiber-plugin fiber))
                    (error-message-string err))
           (when (sk-fiber-child fiber) (sk-scope-dispose (sk-fiber-child fiber))
                 (setf (sk-fiber-child fiber) nil)))))

(defun sk--unload (fiber)
  (when (sk-fiber-child fiber)
    (sk-scope-dispose (sk-fiber-child fiber))
    (setf (sk-fiber-child fiber) nil))
  (setf (sk-fiber-state fiber) 'pending))

(defvar sk--notifying nil)
(defun sk--notify ()
  "Re-check every fiber's epoch; unwind and re-apply the ones that changed."
  (unless sk--notifying
    (let ((sk--notifying t) changed)
      (while (progn
               (setq changed nil)
               (dolist (f sk--fibers)
                 (let ((new (sk--epoch f)))
                   (unless (equal new (sk-fiber-epoch f))
                     (setq changed t)
                     (setf (sk-fiber-epoch f) new)
                     (when (sk-fiber-child f) (sk--unload f))
                     (unless (eq new 'inactive) (sk--load f)))))
               changed)))))

(defun sk-mount (scope name &optional config)
  "Mount plugin NAME into SCOPE with CONFIG; returns the fiber."
  (let* ((plugin (or (alist-get name sk-plugins) (error "sk: no plugin %s" name)))
         (fiber (make-sk-fiber :plugin plugin :scope scope :config config
                               :state 'pending :epoch 'unset)))
    (setq sk--fibers (append sk--fibers (list fiber)))
    ;; The fiber dies with its scope.
    (sk-scope-effect scope (lambda () (sk--unmount fiber)))
    (sk--notify)
    fiber))

(defun sk--unmount (fiber)
  (when (memq fiber sk--fibers)
    (setq sk--fibers (delq fiber sk--fibers))
    (sk--unload fiber)
    (sk--notify)))

(defun sk-unmount (fiber)
  "Unmount FIBER now (its scope effect becomes a no-op)."
  (sk--unmount fiber))

(defun sk-reconfigure (fiber config)
  "Validate CONFIG, then unload and re-apply FIBER with it."
  (sk--validate-config (sk-fiber-plugin fiber) config)
  (setf (sk-fiber-config fiber) config)
  (sk--unload fiber)
  (unless (eq (sk-fiber-epoch fiber) 'inactive) (sk--load fiber)))

(defun sk-reset ()
  (setq sk--root (sk-scope-create "global") sk--fibers nil
        sk--service-defs nil sk--event-defs nil sk-plugins nil))

(provide 'sk-proto)
;;; sk-proto.el ends here
