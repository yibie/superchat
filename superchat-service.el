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

(defmacro superchat-service-table (&rest pairs)
  "Build a provider method table from PAIRS of METHOD FUNCTION."
  `(list ,@(cl-loop for (method fn) on pairs by #'cddr
                    collect `(cons ',method ,fn))))

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
    (let* ((withdraw (superchat-scope-register
                      scope 'service name
                      (list :table table :id (gensym (format "%s-" name)))))
           (undo (superchat-scope-effect
                  scope (lambda () (funcall withdraw) (superchat-plugin-notify)))))
      ;; Only the combined, self-removing undo remains Scope-registered.
      ;; It invokes the detached registration undo before notifying dependents.
      (setf (superchat-scope-effects scope)
            (delq withdraw (superchat-scope-effects scope)))
      (superchat-plugin-notify)
      undo)))

(defun superchat-service-get (scope name)
  "The provider plist of Service NAME visible from SCOPE, or nil."
  (superchat-scope-lookup (or scope superchat-scope-global) 'service name))

(defun superchat-service-call (scope name method &rest args)
  "Call METHOD of Service NAME with ARGS; error when no provider is active.

SCOPE is the caller's execution Scope, never the provider's Scope: providers
remain global-only.  A nil SCOPE inherits an enclosing execution Scope; nil
outside an execution means global.  Pass `superchat-scope-global' explicitly
to reset a nested call to global."
  (let* ((execution-scope (superchat-execution-scope-resolve scope))
         (provider (superchat-service-get execution-scope name)))
    (unless provider
      (error "superchat-service: no provider for %s" name))
    (let ((fn (cdr (assq method (plist-get provider :table)))))
      (unless fn
        (error "superchat-service: provider of %s has no method %s" name method))
      ;; The provider is still selected from its global registration.  This
      ;; binding carries only the consumer's execution context to nested
      ;; driver/Stage/Event work; it never changes provider ownership.
      (let ((superchat-execution-scope execution-scope))
        (apply fn args)))))

(defun superchat-service-report ()
  "List of (NAME PROVIDED-P) for every declared Service."
  (mapcar (lambda (def)
            (list (car def) (and (superchat-service-get nil (car def)) t)))
          superchat-service--definitions))

;;;; Kernel service vocabulary (spec §1.3) -------------------------------------

(superchat-define-service 'driver
  '(submit submit-blocking cancel retire-scope steer status active-runs pending-input
    answer-input enqueue-follow-up pending-follow-ups clear-follow-ups
    terminal-p)
  "Envelope in, Run lifecycle out.")
(superchat-define-service 'tape
  '(append-event append-event-record append-material append-anchor replay
    register-projection replay-projection catch-up-projection
    storage-reference-p exact-json select)
  "The append-only history.")
(superchat-define-service 'session
  '(entries search assemble-view superseded-ids history configuration session-ids
    anchor-latest anchor-source-ids expand-anchor item-line)
  "Views over one Session's Tape.")
(superchat-define-service 'authority
  '(decide resolve-effects register-policy register-effect-resolver
    register-approval-preview transport-timer-owner)
  "Effect decisions and approvals.")
(superchat-define-service 'tools
  '(register descriptors materialize execute)
  "Tool registry and the single execution seam (Invariant 3).")
(superchat-define-service 'view '(sink-for markdown-to-org)
  "The chat surface's default Sink.")
(superchat-define-service 'commands '(register lookup list)
  "Slash commands.")
(superchat-define-service 'closure '(judge)
  "Closure judgments.")
(superchat-define-service 'editor-workspace
  '(validate-buffer buffer owner-session acquire-buffer rollback-acquisition close)
  "Session-owned editor buffers.")
(superchat-define-service 'change-set '(apply)
  "The only writer into a host buffer (Invariant 1).")
(superchat-define-service 'memory
  '(insert search search-prioritized get list-review set-review count reset-projection)
  "Memory rows.")
(superchat-define-service 'recovery '(assemble-view)
  "assemble-view FAILURE-EVENT SESSION-IDS -> Experience recovery View
\(provided by the experience plugin; optional for the driver).")

(superchat-define-service
 'model-transport '(generate capabilities context-budget)
 "generate PROMPT CALLBACK STREAM-CALLBACK &optional TARGET-MODEL CONTEXT-FILES
TOOLS SYSTEM-PROMPT PRESET TOOL-DECISION -- the model loop; must report
`request'/`terminal' to `superchat--llm-runtime-observer' when bound.
capabilities MODEL -> (:streaming BOOL :tools BOOL).
context-budget &optional SYSTEM PROMPT MODEL -> character budget for the prompt.")

(provide 'superchat-service)
;;; superchat-service.el ends here
