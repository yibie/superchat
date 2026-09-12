;;; superchat-stage.el --- Stage registry for the fixed turn pipeline -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; The stage registry (contract §4.1, `docs/design/turn-pipeline-contract.md'):
;; a small in-house pub/sub mechanism for the fixed thirteen-stage Turn
;; pipeline.  A stage is declared once at load with a fixed kind; plugins
;; compete only inside a stage by registering named implementations through
;; `superchat-stage-add'.  This file owns no pipeline behaviour of its own --
;; `superchat-pipeline.el' is the driver that decides what each stage means
;; and what happens when nothing is registered.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)

(define-error 'superchat-stage-required-failed
  "Superchat stage: a required collect-stage contributor failed")

(defconst superchat-stage-names
  '(parse resolve-session admit load-state select-tools collect-context
    assemble-prompt save-state)
  "The fixed Stage names of the main line (spec §1.4).  Calling the model
is the `model-transport' Service, the seal is driver code, delivery is the
Sink, and `stage-error' is an Event; those are not Stages.")

(defvar superchat-stage--registry (make-hash-table :test #'eq)
  "NAME -> (:kind KIND :validator FN-or-nil :core-only BOOL).

Stage declarations are global vocabulary.  Implementations live instead in
the existing Scope layers under `(stage NAME)', where their IDs receive the
normal same-layer duplicate rejection and nearest-layer shadowing.  Internal:
read visible implementations through `superchat-stage-report' or
`superchat-stage--implementations'; tests may call `superchat-stage--define'
directly to declare an ad-hoc temporary stage and must remove it afterward
with `remhash'.")

(defvar superchat-stage-faults nil
  "Bounded ring (100 entries) of stage-call and cross-cutting faults.
Each entry is a plist (:stage NAME :id ID :owner OWNER :condition CONDITION).")

(defun superchat-stage-record-fault (stage id owner condition)
  "Push one fault for STAGE/ID/OWNER/CONDITION, capped at 100 entries.
Exposed so callers outside the registry's own call machinery -- such as
`superchat-pipeline.el' validating a `load-state' delta's owner prefix --
can record into the same bounded ring, mirroring how
`superchat-sink-record-diagnostic' lets outside callers record a Sink
diagnostic."
  (push (list :stage stage :id id :owner owner :condition condition)
        superchat-stage-faults)
  (when (> (length superchat-stage-faults) 100)
    (setcdr (nthcdr 99 superchat-stage-faults) nil)))

(cl-defun superchat-stage--define (name &key kind validator core-only)
  "Declare stage NAME with KIND, optional VALIDATOR, and CORE-ONLY flag.
Internal: called for the thirteen fixed stages at load.  Plugins never call
this directly; `superchat-stage-add' rejects any NAME not already declared
here, which is what \"unknown names cannot be defined by plugins\" means."
  (puthash name (list :kind kind :validator validator :core-only core-only)
           superchat-stage--registry)
  name)

(defun superchat-stage--entry (name)
  "Return NAME's registry entry, or nil when NAME is not a declared stage."
  (gethash name superchat-stage--registry))

(defun superchat-stage--fragment-list-p (value)
  "Return non-nil when VALUE is a proper list of fragment plists.
Used as `collect-context''s VALIDATOR (contract §4.3): a `collect-context'
contribution must be a list of zero or more fragment plists -- never a
bare fragment plist itself, whose own keyword/value pairs `apply #'append'
would otherwise splice in as loose top-level elements.  A VALIDATOR only
ever sees a non-nil RESULT (`superchat-stage-call' skips it for nil), so
this need not accept nil itself; full per-fragment schema (required keys,
value types) stays the assembler's responsibility."
  (and (listp value)
       (proper-list-p value)
       (cl-every (lambda (element)
                   (and (listp element)
                        (proper-list-p element)
                        (cl-evenp (length element))
                        (cl-loop for (key _v) on element by #'cddr
                                 always (keywordp key))))
                 value)))

(dolist (spec '((parse . fold)
                (resolve-session . first-result)
                (admit . first-result)
                (load-state . collect)
                (select-tools . first-result)
                (collect-context . collect)
                (assemble-prompt . first-result)
                (save-state . collect)))
  (superchat-stage--define
   (car spec) :kind (cdr spec)
   :validator (and (eq (car spec) 'collect-context)
                    #'superchat-stage--fragment-list-p)))

(defun superchat-stage--registration-scope (&optional scope)
  "Return the Scope receiving a Stage registration.

Registration remains separate from execution lookup: an explicit SCOPE wins,
then the applying plugin's Scope, then global.  A Stage call running in an
execution Scope must not accidentally make a later registration local to that
Run."
  (or scope superchat-plugin-current-scope superchat-scope-global))

(defun superchat-stage--implementations (name &optional scope)
  "Return NAME's visible implementations in invocation order from SCOPE.

SCOPE defaults to the current execution Scope, then global.  Scope merge is
oldest-first for reporting, while Stage invocation retains its established
newest-first order; reversing the fresh merged list preserves that order and
makes a nearer Scope's surviving contributors run before inherited ones."
  (nreverse
   (mapcar #'cdr
           (superchat-scope-merge
            (superchat-execution-scope-resolve scope)
            (list 'stage name)))))

(cl-defun superchat-stage-add (name id function &key owner required scope)
  "Register FUNCTION as ID's implementation of stage NAME, owned by OWNER.
REQUIRED (collect stages only) marks a contributor whose execution or
validation failure is terminal for the Turn: `superchat-stage-call' signals
`superchat-stage-required-failed' with data (:stage NAME :id ID).
Validation failures also record a fault; nil remains no contribution.  Later
registration runs first.  Rejects an unknown NAME, a core-only stage
registered by an OWNER other than `core', a duplicate ID, a non-function
FUNCTION, and REQUIRED on a stage whose kind is not `collect' (contract
§4.1)."
  (let ((entry (superchat-stage--entry name)))
    (unless entry
      (error "Unknown stage: %S" name))
    (unless (and owner (symbolp owner))
      (error "superchat-stage-add requires a symbol :owner"))
    (when (and (plist-get entry :core-only) (not (eq owner 'core)))
      (error "Stage %S accepts registrations only from owner `core'" name))
    (unless (functionp function)
      (error "superchat-stage-add FUNCTION must be a function"))
    (unless (memq required '(nil t))
      (error "superchat-stage-add :required must be nil or t"))
    (when (and required (not (eq (plist-get entry :kind) 'collect)))
      (error "Stage %S is not a `collect' stage; :required is not allowed" name))
    (let* ((scope (superchat-stage--registration-scope scope))
           ;; Keep the one-shot Scope undo on the implementation itself so
           ;; explicit `superchat-stage-remove' can invoke the same cleanup.
           ;; The :undo slot exists before registration, so `setf' mutates the
           ;; exact value retained in the Scope layer rather than creating a
           ;; second registry or a name-only removal path.
           (implementation
            (list :id id :owner owner :function function :required required
                  :undo nil))
           (undo (superchat-scope-register
                  scope (list 'stage name) id implementation)))
      (setf (plist-get implementation :undo) undo)
      id)))

(defun superchat-stage-remove (name id &optional scope)
  "Remove this Scope's ID implementation of stage NAME, if any.

SCOPE defaults to the applying plugin's Scope, then global.  It invokes the
installation's own one-shot Scope undo, so removing an old registration cannot
withdraw a later same-ID registration and explicit removal does not retain a
stale effect closure."
  (let* ((scope (superchat-stage--registration-scope scope))
         (owner (or (superchat-scope-layer-owner scope) scope))
         (layer (assoc (list 'stage name) (superchat-scope-layers owner)))
         (implementation (and layer (cdr (assoc id (cdr layer)))))
         (undo (and implementation (plist-get implementation :undo))))
    (when (functionp undo)
      (funcall undo)))
  nil)

(defun superchat-stage--try (name impl args)
  "Call IMPL's function with ARGS, for stage NAME.
Return (:ok VALUE) on success.  An `error' is recorded as a fault and this
returns nil.  Collect-kind callers use `superchat-stage--try-collect'
instead, so a `:required' IMPL's error is not swallowed here."
  (condition-case condition
      (list :ok (apply (plist-get impl :function) args))
    (error
     (superchat-stage-record-fault
      name (plist-get impl :id) (plist-get impl :owner) condition)
     nil)))

(defun superchat-stage--valid-result-p (name impl validator result)
  "Return non-nil when RESULT passes VALIDATOR for NAME's IMPL.
No VALIDATOR always passes.  A VALIDATOR that returns nil, or that itself
signals an `error', counts as invalid and is recorded as a fault for NAME's
IMPL/OWNER (contract §4.1: \"validator failure counts as nil and is
recorded\") -- unlike an implementation's own `error' (handled by
`superchat-stage--try'/`superchat-stage--try-collect'), a signalling
VALIDATOR previously escaped uncaught, past this stage's fault ring."
  (or (null validator)
      (condition-case condition
          (let ((ok (funcall validator result)))
            (unless ok
              (superchat-stage-record-fault
               name (plist-get impl :id) (plist-get impl :owner)
               (list :validator-rejected result)))
            ok)
        (error
         (superchat-stage-record-fault
          name (plist-get impl :id) (plist-get impl :owner) condition)
         nil))))

(defun superchat-stage--try-collect (name impl args)
  "Like `superchat-stage--try', but a `:required t' IMPL's `error' signals
`superchat-stage-required-failed' with data (:stage NAME :id ID) instead of
being recorded as a fault."
  (condition-case condition
      (list :ok (apply (plist-get impl :function) args))
    (error
     (if (plist-get impl :required)
         (signal 'superchat-stage-required-failed
                 (list :stage name :id (plist-get impl :id)))
       (superchat-stage-record-fault
        name (plist-get impl :id) (plist-get impl :owner) condition)
       nil))))

(cl-defun superchat-stage-call (name &rest args)
  "Invoke stage NAME's implementation(s) with ARGS (contract §4.1).

`first-result': the first non-nil result that also passes the stage's
VALIDATOR (if any), newest registration first.  `collect': every such valid
non-nil result, in that same order.  `fold': ARGS' first element threads as
the accumulator through every implementation (newest first); an
implementation returning nil leaves the accumulator unchanged, and
`(:finish V)' stops early with V.  `observer': every implementation runs for
effect only; its own `error' is swallowed, not recorded.  `core-only': the
single core implementation is called directly, with no error swallowed -- a
signal such as `superchat-prompt-over-budget' propagates to the caller
unchanged, since there is no other implementation to fall back to."
  (let ((entry (superchat-stage--entry name)))
    (unless entry
      (error "Unknown stage: %S" name))
    (let ((kind (plist-get entry :kind))
          (validator (plist-get entry :validator))
          (implementations (superchat-stage--implementations name)))
      (pcase kind
        ('core-only
         (let ((impl (car implementations)))
           (and impl (apply (plist-get impl :function) args))))
        ('first-result
         (catch 'superchat-stage--found
           (dolist (impl implementations)
             (let ((outcome (superchat-stage--try name impl args)))
               (when outcome
                 (let ((result (cadr outcome)))
                   (when (and result (superchat-stage--valid-result-p name impl validator result))
                     (throw 'superchat-stage--found result))))))
           nil))
        ('collect
         (let (results)
           (dolist (impl implementations)
             (let ((outcome (superchat-stage--try-collect name impl args)))
               (when outcome
                 (let ((result (cadr outcome)))
                   (when result
                     (if (superchat-stage--valid-result-p name impl validator result)
                         (push result results)
                       (when (plist-get impl :required)
                         (signal 'superchat-stage-required-failed
                                 (list :stage name :id (plist-get impl :id))))))))))
           (nreverse results)))
        ('fold
         (let ((acc (car args)) (extra (cdr args)))
           (catch 'superchat-stage--finished
             (dolist (impl implementations)
               (let ((outcome (superchat-stage--try name impl (cons acc extra))))
                 (when outcome
                   (let ((result (cadr outcome)))
                     (cond
                      ((null result)) ;; identity: accumulator unchanged
                      ((and (consp result) (eq (car result) :finish))
                       (throw 'superchat-stage--finished (cadr result)))
                      (t (setq acc result)))))))
             acc)))
        ('observer
         (dolist (impl implementations)
           (condition-case nil (apply (plist-get impl :function) args) (error nil)))
         nil)
        (_ (error "Unknown stage kind: %S" kind))))))

(cl-defun superchat-stage-call-outcome (name &rest args)
  "Invoke `first-result' stage NAME's DIRECT implementations with ARGS.
Return `(:result R :faulted BOOL)': R is the first result that is non-nil
and passes the stage's VALIDATOR (if any), newest registration first --
identical to what `superchat-stage-call' would return for the same
NAME/ARGS.  FAULTED is non-nil exactly when at least one DIRECT
implementation tried during THIS call either signalled an `error' or had
its result rejected by the VALIDATOR.

This is computed inside this call's own loop, never by diffing
`superchat-stage-faults' -- a nested `superchat-stage-call' an
implementation happens to make internally records its own fault on the
shared ring but must never be attributed to this outer call (contract
§4's single-threaded re-entrancy requirement; the ring-diffing approach
`superchat-pipeline--stage-call-with-fault-p' still uses for `select-tools'
conflates the two).  Only `first-result' stages are supported -- `admit'
is this revision's only caller; signals an error for any other kind."
  (let ((entry (superchat-stage--entry name)))
    (unless entry
      (error "Unknown stage: %S" name))
    (unless (eq (plist-get entry :kind) 'first-result)
      (error "superchat-stage-call-outcome only supports `first-result' stages, got %S for %S"
             (plist-get entry :kind) name))
    (let ((validator (plist-get entry :validator))
          (implementations (superchat-stage--implementations name))
          (faulted nil))
      (catch 'superchat-stage--found
        (dolist (impl implementations)
          (let ((outcome (superchat-stage--try name impl args)))
            (if (null outcome)
                (setq faulted t)
              (let ((result (cadr outcome)))
                (when result
                  (if (superchat-stage--valid-result-p name impl validator result)
                      (throw 'superchat-stage--found (list :result result :faulted faulted))
                    (setq faulted t)))))))
        (list :result nil :faulted faulted)))))

(defun superchat-stage-report ()
  "Return the current registry state.

`(:stages ((:stage NAME :kind KIND :implementations ((:id ID :owner OWNER
:required BOOL) …)) …) :faults superchat-stage-faults)', stages in
`superchat-stage-names' order."
  (list :stages
        (mapcar
         (lambda (name)
           (let ((entry (superchat-stage--entry name)))
             (list :stage name :kind (plist-get entry :kind)
                   :implementations
                   (mapcar (lambda (impl)
                             (list :id (plist-get impl :id)
                                   :owner (plist-get impl :owner)
                                   :required (plist-get impl :required)))
                           (superchat-stage--implementations name)))))
         superchat-stage-names)
        :faults superchat-stage-faults))

;;; Events (spec §1.5) -------------------------------------------------------
;; A named extension point off the Stage main line, each with one combination
;; rule.  Listeners are Scope-registered (`superchat-on'), so a plugin's
;; listeners leave with it; `superchat-emit' merges the listeners visible
;; from SCOPE (nearest Scope last for `fold', so a Preset can override).

(defvar superchat-event--definitions nil
  "alist NAME -> (:rule RULE :doc DOC); RULE is observe|first|fold|collect.")

(defun superchat-define-event (name rule &optional doc)
  "Declare Event NAME with combination RULE."
  (unless (memq rule '(observe first fold collect))
    (error "superchat-define-event: unknown rule %S" rule))
  (setf (alist-get name superchat-event--definitions) (list :rule rule :doc doc))
  name)

(defun superchat-on (scope event id function)
  "Register FUNCTION as listener ID of EVENT through SCOPE.
SCOPE nil means the applying plugin's Scope when one is active, else the
global Scope.  Returns the undo thunk."
  (unless (assq event superchat-event--definitions)
    (error "superchat-on: unknown event %S" event))
  (unless (functionp function)
    (error "superchat-on: listener %S of %S is not a function" id event))
  (superchat-scope-register (or scope superchat-plugin-current-scope superchat-scope-global)
                            (list 'event event) id function))

(defun superchat-off (event id &optional scope)
  "Remove listener ID of EVENT from SCOPE (default global); return non-nil if found."
  (let* ((scope (or scope superchat-scope-global))
         (layer (assoc (list 'event event) (superchat-scope-layers scope))))
    (when (and layer (assoc id (cdr layer)))
      (setf (cdr layer) (cl-remove id (cdr layer) :key #'car :test #'equal))
      (when (null (cdr layer))
        (setf (superchat-scope-layers scope) (delq layer (superchat-scope-layers scope))))
      t)))

(defun superchat-event-listeners (scope event)
  "Listeners of EVENT visible from SCOPE (default global), as (ID . FN)."
  (superchat-scope-merge (or scope superchat-scope-global) (list 'event event)))

(defun superchat-emit (scope event &rest args)
  "Emit EVENT with ARGS to the listeners visible from SCOPE.
observe: every listener runs, its errors are recorded as faults; first: the
first non-nil result; fold: ARGS's first element is threaded through every
listener (a nil result keeps the accumulator); collect: every non-nil result."
  (let* ((execution-scope (or scope superchat-scope-global))
         (definition (or (assq event superchat-event--definitions)
                         (error "superchat-emit: unknown event %S" event)))
         (rule (plist-get (cdr definition) :rule))
         (listeners (superchat-event-listeners execution-scope event)))
    (cl-flet ((call (listener &rest call-args)
                (condition-case err
                    (apply (cdr listener) call-args)
                  ((error quit)
                   (superchat-stage-record-fault
                    (list 'event event) (car listener) 'listener err)
                   nil))))
      ;; Event callbacks are public reentry points.  Bind their borrowed
      ;; caller Scope while they run so a nested nil Service call inherits it;
      ;; the Scope itself is neither owned nor persisted here.
      (let ((superchat-execution-scope execution-scope))
        (pcase rule
          ('observe (dolist (l listeners) (apply #'call l args)) nil)
          ('first (cl-some (lambda (l) (apply #'call l args)) listeners))
          ('collect (delq nil (mapcar (lambda (l) (apply #'call l args)) listeners)))
          ('fold (let ((acc (car args)))
                   (dolist (l listeners acc)
                     (let ((next (apply #'call l acc (cdr args))))
                       (unless (null next) (setq acc next)))))))))))

(dolist (spec '((stage-error observe "stage condition envelope")
                (run-started observe "run request — after run.started is on the Tape")
                (run-sealing observe "run turn — before the seal")
                (run-completed observe "run status — completed Runs only")
                (turn-completed observe "run status — every terminal Turn")
                (tool-before observe "tool args run-id")
                (tool-permission collect "tool args → allow|deny|nil; deny wins")
                (tool-args-rewrite fold "tool args → (:args ARGS) | (:block REASON) | nil (listeners are called (TOOL ARGS))")
                (tool-after observe "tool args result run-id")
                (tool-failed observe "tool args error")
                (tool-result-filter fold "tool args result → replacement string or nil (listeners are called (TOOL ARGS RESULT))")
                (before-provider-request fold "(:prompt STRING :system STRING) → same shape")
                (context-budget-exceeded first "details → non-nil claims recovery")
                (closure-shadow observe "report")
                (session-started observe "session-id")
                (session-closed observe "session-id")))
  (superchat-define-event (nth 0 spec) (nth 1 spec) (nth 2 spec)))

(provide 'superchat-stage)

;;; superchat-stage.el ends here
