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

(define-error 'superchat-stage-required-failed
  "Superchat stage: a required collect-stage contributor failed")

(defconst superchat-stage-names
  '(resolve-session admit load-state select-tools collect-context
    assemble-prompt run-model save-state render dispatch
    on-error register-command register-surface)
  "The thirteen fixed stage names, in contract §4 order.")

(defvar superchat-stage--registry (make-hash-table :test #'eq)
  "NAME -> (:kind KIND :validator FN-or-nil :core-only BOOL :implementations LIST).
Each element of :implementations is (:id ID :owner OWNER :function FN
:required BOOL), newest registration first.  Internal: read it only through
`superchat-stage-report'; tests may call `superchat-stage--define' directly
to declare an ad-hoc temporary stage (for example a `fold'-kind stage, which
no fixed stage in this revision uses) and must remove it themselves
afterward with `remhash'.")

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
  (puthash name (list :kind kind :validator validator :core-only core-only
                       :implementations nil)
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

(dolist (spec '((resolve-session . first-result)
                (admit . first-result)
                (load-state . collect)
                (select-tools . first-result)
                (collect-context . collect)
                (assemble-prompt . core-only)
                (run-model . core-only)
                (save-state . collect-then-seal)
                (render . collect)
                (dispatch . core-only)
                (on-error . observer)
                (register-command . observer)
                (register-surface . observer)))
  (superchat-stage--define
   (car spec) :kind (cdr spec)
   :validator (and (eq (car spec) 'collect-context)
                    #'superchat-stage--fragment-list-p)
   :core-only (and (memq (car spec) '(assemble-prompt run-model dispatch)) t)))

(cl-defun superchat-stage-add (name id function &key owner required)
  "Register FUNCTION as ID's implementation of stage NAME, owned by OWNER.
REQUIRED (collect stages only) marks a contributor whose `error' is terminal
for the Turn: `superchat-stage-call' signals `superchat-stage-required-failed'
with data (:stage NAME :id ID) instead of recording a fault.  Later
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
    (when (cl-find id (plist-get entry :implementations)
                   :key (lambda (impl) (plist-get impl :id)) :test #'equal)
      (error "Stage %S already has an implementation with id %S" name id))
    (unless (functionp function)
      (error "superchat-stage-add FUNCTION must be a function"))
    (unless (memq required '(nil t))
      (error "superchat-stage-add :required must be nil or t"))
    (when (and required (not (eq (plist-get entry :kind) 'collect)))
      (error "Stage %S is not a `collect' stage; :required is not allowed" name))
    (puthash name
             (plist-put entry :implementations
                        (cons (list :id id :owner owner :function function
                                    :required required)
                              (plist-get entry :implementations)))
             superchat-stage--registry)
    id))

(defun superchat-stage-remove (name id)
  "Remove ID's implementation of stage NAME, if any.  Return nil."
  (let ((entry (superchat-stage--entry name)))
    (when entry
      (puthash name
               (plist-put entry :implementations
                          (cl-remove id (plist-get entry :implementations)
                                     :key (lambda (impl) (plist-get impl :id))
                                     :test #'equal))
               superchat-stage--registry)))
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
          (implementations (plist-get entry :implementations)))
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
                   (when (and result (superchat-stage--valid-result-p name impl validator result))
                     (push result results))))))
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
          (implementations (plist-get entry :implementations))
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
                           (plist-get entry :implementations)))))
         superchat-stage-names)
        :faults superchat-stage-faults))

;; Stages 8 and 10 stay inside `superchat-runtime.el' for this slice (the
;; non-replaceable seal, and Sink dispatch already built by S01).  These
;; registrations exist only so `superchat-stage-report' names a core owner
;; for them; calling them through the registry is a bug in this revision.
(superchat-stage-add
 'save-state 'core-seal
 (lambda (&rest _)
   (error "save-state's core seal is implemented by superchat-runtime.el, not the stage registry, in this revision"))
 :owner 'core)

(superchat-stage-add
 'dispatch 'core-sink
 (lambda (&rest _)
   (error "dispatch's core Sink emission is implemented by superchat-runtime.el, not the stage registry, in this revision"))
 :owner 'core)

(provide 'superchat-stage)

;;; superchat-stage.el ends here
