;;; superchat-service.el --- Kernel: Services -*- lexical-binding: t; -*-

;;; Commentary:
;; A Service is a name with a required method list; a provider is a
;; method -> function table registered through a Scope and checked at mount.
;; Exactly one active provider per name; Services live in the global Scope
;; only (spec §1.3).  Consumers call `superchat-service-call' or probe with
;; `superchat-service-get'; they never `require' the provider's file.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)

(defvar superchat-service--definitions nil
  "alist NAME -> (:methods LIST :doc STRING).")

(defun superchat-define-service (name methods &optional doc)
  "Declare Service NAME with required METHODS (a list of symbols)."
  (setf (alist-get name superchat-service--definitions)
        (list :methods methods :doc doc))
  name)

(defun superchat-service-definition (name)
  (or (alist-get name superchat-service--definitions)
      (error "superchat-service: unknown service %s" name)))

(defun superchat-service-provide (scope name table)
  "Provide Service NAME with TABLE (alist METHOD -> FUNCTION) through SCOPE.
Missing required methods are a mount-time error.  Only the global layer may
hold providers: a plugin mounted in a Preset Scope may not shadow one.
Dependents are notified (unwound / re-applied) at once and again on undo."
  (let ((required (plist-get (superchat-service-definition name) :methods))
        (owner (or (superchat-scope-layer-owner scope) scope)))
    (unless (eq owner superchat-scope-global)
      (error "superchat-service: %s must be provided from the global Scope, not %s"
             name (superchat-scope-name owner)))
    (dolist (method required)
      (unless (functionp (cdr (assq method table)))
        (error "superchat-service: provider of %s lacks method %s" name method)))
    (let ((undo (superchat-scope-register
                 scope 'service name
                 (list :table table :id (gensym (format "%s-" name))))))
      ;; The registration undo already runs on dispose; add the notify step.
      (superchat-scope-effect scope #'superchat-plugin-notify)
      (superchat-plugin-notify)
      undo)))

(defun superchat-service-get (scope name)
  "The provider plist of Service NAME visible from SCOPE, or nil."
  (superchat-scope-lookup (or scope superchat-scope-global) 'service name))

(defun superchat-service-call (scope name method &rest args)
  "Call METHOD of Service NAME with ARGS; error when no provider is active."
  (let ((provider (superchat-service-get scope name)))
    (unless provider
      (error "superchat-service: no provider for %s" name))
    (let ((fn (cdr (assq method (plist-get provider :table)))))
      (unless fn
        (error "superchat-service: provider of %s has no method %s" name method))
      (apply fn args))))

(defun superchat-service-report ()
  "List of (NAME PROVIDED-P) for every declared Service."
  (mapcar (lambda (def)
            (list (car def) (and (superchat-service-get nil (car def)) t)))
          superchat-service--definitions))

;;;; Kernel service vocabulary ------------------------------------------------
;; Step 0 of the migration declares the one Service the driver can already
;; consume.  Steps 2 adds the other eleven (spec §1.3).

(superchat-define-service
 'model-transport '(generate capabilities)
 "generate PROMPT CALLBACK STREAM-CALLBACK &optional TARGET-MODEL CONTEXT-FILES
TOOLS SYSTEM-PROMPT PRESET TOOL-DECISION -- the model loop; must report
`request'/`terminal' to `superchat--llm-runtime-observer' when bound.
capabilities MODEL -> (:streaming BOOL :tools BOOL).")

(provide 'superchat-service)
;;; superchat-service.el ends here
