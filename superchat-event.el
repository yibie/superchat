;;; superchat-event.el --- Scoped Events, waterfalls and faults -*- lexical-binding: t; -*-

;; Shared Event substrate: declarations, Scope-registered listeners,
;; combination rules, two ordered-await waterfalls, additive context,
;; and the bounded listener fault ring.
(require 'cl-lib)
(require 'superchat-plugin)

(defvar superchat-event-faults nil
  "Bounded ring (100 entries) of listener and cross-cutting faults.
Each entry is a plist (:site NAME :id ID :owner OWNER :condition CONDITION).
SITE is an Event name such as `collect-context', an `(event waterfall)' pair,
or a driver step such as `transport-stop'.  This ring is diagnostic only:
a lifecycle decision uses the failure of its own invocation, not a scan of
unrelated entries in this bounded ring.")

(defun superchat-event-record-fault (site id owner condition)
  "Push one fault for SITE/ID/OWNER/CONDITION, capped at 100 entries.
Exposed so callers outside this module's own listener machinery -- such as
`superchat-pipeline.el' recording a failed context collection -- can record
into the same bounded ring, mirroring how
`superchat-sink-record-diagnostic' lets outside callers record a Sink
diagnostic."
  (push (list :site site :id id :owner owner :condition condition)
        superchat-event-faults)
  (when (> (length superchat-event-faults) 100)
    (setcdr (nthcdr 99 superchat-event-faults) nil)))

(defun superchat-context--fragment-list-p (value)
  "Return non-nil when VALUE is a proper list of fragment plists.
The shape a `collect-context' listener must return (contract §4.3): a list of
zero or more fragment plists -- never a bare fragment plist itself, whose own
keyword/value pairs `apply #'append' would otherwise splice in as loose
top-level elements.  Only a non-nil result is validated, because nil is the
Event rule's \"no contribution\"; full per-fragment schema (required keys,
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

;;; Events (spec §1.5) -------------------------------------------------------
;; A named extension point on the execution main line, each with one
;; combination rule.  Listeners are Scope-registered (`superchat-on'), so a
;; plugin's listeners leave with it; `superchat-emit' merges the listeners
;; visible from SCOPE (nearest Scope last for `fold', so a Preset can
;; override).

(defvar superchat-event--definitions nil
  "alist NAME -> (:rule RULE :doc DOC).
RULE is observe|first|fold|collect|waterfall.")

(defun superchat-define-event (name rule &optional doc)
  "Declare Event NAME with combination RULE.

A `waterfall' Event's listeners are ordered, awaited middleware: each receives
the candidate and a single-use continuation.  Only a `waterfall' Event may use
that protocol, so a listener cannot register a continuation shape against an
Event that will not provide one."
  (unless (memq rule '(observe first fold collect waterfall))
    (error "superchat-define-event: unknown rule %S" rule))
  (setf (alist-get name superchat-event--definitions) (list :rule rule :doc doc))
  name)

(define-error 'superchat-event-required-failed
  "A required Event observer failed")

(cl-defun superchat-on (scope event id function &key required)
  "Register FUNCTION as listener ID of EVENT through SCOPE.
SCOPE nil means the applying plugin's Scope when one is active, else the
global Scope.  Returns the undo thunk.
An observe listener may declare REQUIRED.  Its failure is recorded normally
and returned by that emission, without preventing the other observers from
running.  The event's caller decides what that failure means."
  (unless (assq event superchat-event--definitions)
    (error "superchat-on: unknown event %S" event))
  (unless (functionp function)
    (error "superchat-on: listener %S of %S is not a function" id event))
  (unless (and (memq required '(nil t))
               (or (not required)
                   (eq 'observe
                       (plist-get (cdr (assq event superchat-event--definitions))
                                  :rule))))
    (error "superchat-on: only observe listeners may be required"))
  (superchat-scope-register
   (or scope superchat-plugin-current-scope superchat-scope-global)
   (list 'event event) id
   (if required
       (lambda (&rest args)
         (condition-case err
             (apply function args)
           ((error quit)
            (signal 'superchat-event-required-failed (list err)))))
     function)))

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
observe: every listener runs, its errors are recorded as faults; return this
emission's required failures as (:id ID :condition CONDITION) plists, or nil.
first: the first non-nil result; fold: thread ARGS's first element through
every listener (a nil result keeps the accumulator); collect: every non-nil
result; waterfall: ARGS's first element is threaded through ordered, awaited
middleware, each receiving a single-use continuation."
  (let* ((execution-scope (or scope superchat-scope-global))
         (definition (or (assq event superchat-event--definitions)
                         (error "superchat-emit: unknown event %S" event)))
         (rule (plist-get (cdr definition) :rule))
         (listeners (superchat-event-listeners execution-scope event))
         (required-failures nil))
    (cl-flet ((call (listener &rest call-args)
                (condition-case err
                    (apply (cdr listener) call-args)
                  ((error quit)
                   (let ((condition
                          (if (eq (car err) 'superchat-event-required-failed)
                              (cadr err)
                            err)))
                     (when (eq (car err) 'superchat-event-required-failed)
                       (push (list :id (car listener) :condition condition)
                             required-failures))
                     (superchat-event-record-fault
                      (list 'event event) (car listener) 'listener condition))
                   nil))))
      ;; Event callbacks are public reentry points.  Bind their borrowed
      ;; caller Scope while they run so a nested nil Service call inherits it;
      ;; the Scope itself is neither owned nor persisted here.
      (let ((superchat-execution-scope execution-scope))
        (pcase rule
          ('observe
           (dolist (l listeners) (apply #'call l args))
           (nreverse required-failures))
          ('first (cl-some (lambda (l) (apply #'call l args)) listeners))
          ('collect (delq nil (mapcar (lambda (l) (apply #'call l args)) listeners)))
          ('fold (let ((acc (car args)))
                   (dolist (l listeners acc)
                     (let ((next (apply #'call l acc (cdr args))))
                       (unless (null next) (setq acc next))))))
          ('waterfall
           (superchat-emit--waterfall listeners args)))))))

(defconst superchat--waterfall-refusal
  :superchat-waterfall-refusal
  "Tag marking a waterfall refusal.
An interned keyword, not a freshly made symbol: this tag is compared with `eq'
from more than one loaded file, so it must print and read back to the same
object after any reload.  A `make-symbol' sentinel is fixed when its
`defconst' first runs, which makes the tag depend on load order rather than
on the value a caller wrote.")

(defun superchat-waterfall-refuse (&optional reason)
  "Return the refusal a request waterfall middleware returns, with REASON.
A refusal is a decision, not a failure: the caller refuses the candidate and
never treats it as an abstention, so middleware cannot refuse by accident."
  (list superchat--waterfall-refusal reason))

(defun superchat-waterfall-refused-p (value)
  "Return non-nil when VALUE is a waterfall refusal."
  (and (consp value) (eq (car value) superchat--waterfall-refusal)))

(defun superchat-waterfall-refusal-reason (value)
  "Return the reason carried by waterfall refusal VALUE, or nil."
  (and (superchat-waterfall-refused-p value) (cadr value)))

(defun superchat--listener-takes-continuation-p (function)
  "Return non-nil when FUNCTION declares the middleware shape (VALUE NEXT).

Reads the function's own argument list rather than calling it and interpreting
`wrong-number-of-arguments', which would confuse a listener's own arity bug
with its declared shape.

The test is on REQUIRED arguments, not on the printed list: a listener written
as (&rest _) has zero required arguments and can only be a fold step, while
counting a `&rest' marker as an argument would call it as middleware and hand
it the continuation where it expects the value."
  (let ((args (condition-case nil (help-function-arglist function t)
                (error nil))))
    (and args
         (>= (length (cl-remove-if (lambda (a)
                                     (memq a '(&optional &rest)))
                                   args))
             2))))

(defun superchat-emit--waterfall (listeners args)
  "Thread ARGS's first element through ordered LISTENERS, awaited.

The chain runs inside-out: the last listener sees the original candidate, and
each earlier listener receives everything downstream of it.  That is the
direction this Event's listeners were already written against, so ordering
survives the rule change.

Two listener shapes are accepted, and each has one unambiguous meaning:

- A listener declaring two arguments is middleware.  It receives the
  candidate and a single-use continuation; calling the continuation with a
  value passes that value downstream and returns the downstream result.
  Returning without calling it ends the chain there, and a refusal is a
  decision rather than abstention.
- A listener declaring one argument predates the continuation protocol.  It
  is a fold step: its result is passed downstream, and nil keeps the
  candidate, so an existing registration keeps its meaning.  A fold step
  always chains, because chaining is the only thing it was ever able to ask
  for."
  (let ((candidate (car args)))
    (cl-labels
        ((step
          (remaining value)
          (if (null remaining)
              value
            (let* ((listener (car remaining))
                   (function (cdr listener))
                   (middleware-p
                    (superchat--listener-takes-continuation-p function))
                   (delegated nil)
                   (active t)
                   (next
                    ;; `&rest' rather than `&optional' on purpose: with
                    ;; `&optional' a bare (funcall next) and an explicit
                    ;; (funcall next nil) are indistinguishable, and the two
                    ;; mean opposite things here -- "pass the candidate
                    ;; unchanged" versus "replace it with nil".
                    (lambda (&rest supplied)
                      (unless active
                        (error "Waterfall continuation used after middleware returned"))
                      (when delegated
                        (error "Request waterfall continuation used twice"))
                      (setq delegated t)
                      (step (cdr remaining)
                            (if supplied (car supplied) value))))
                   (result
                    (condition-case err
                        (unwind-protect
                            (if middleware-p
                                (funcall function value next)
                              (funcall next (or (funcall function value) value)))
                          (setq active nil))
                      ((error quit)
                       (superchat-event-record-fault
                        (list 'event 'waterfall) (car listener) 'listener err)
                       ;; A fault refuses, and a refusal is the (TAG REASON)
                       ;; pair -- never the bare tag.  Returning the tag made
                       ;; every consumer's `refused-p' fail, so a fault was
                       ;; read as permission.
                       (superchat-waterfall-refuse
                        (format "listener faulted: %s"
                                (error-message-string err)))))))
              (cond
               ((superchat-waterfall-refused-p result) result)
               ;; Middleware delegated: the continuation already produced the
               ;; final value.
               (delegated result)
               ;; Middleware returned without delegating, either to replace
               ;; the candidate or to abstain.  Both end the chain, and only
               ;; a non-nil result replaces anything.
               (middleware-p (or result value))
               ;; A fold step always chained, so RESULT is the downstream
               ;; value.
               (t result))))))
      (step listeners candidate))))

(dolist (spec '((collect-context collect "envelope state tools — additive context")
                (context-error observe "site condition envelope")
                (run-started observe "run request — after run.started is on the Tape")
                (run-sealing observe "run turn — before the seal")
                (turn-stopping observe "run turn — strict serial notification before natural closure; invoked by Runtime")
                (run-completed observe "run status — completed Runs only")
                (turn-completed observe "run status — every terminal Turn")
                (tool-before observe "tool args run-id")
                (tool-permission collect "tool args → allow|deny|nil; deny wins")
                (tool-args-rewrite fold "tool args → (:args ARGS) | (:block REASON) | nil (listeners are called (TOOL ARGS))")
                (tool-after observe "tool args result run-id")
                (tool-failed observe "tool args error")
                (tool-result-filter fold "tool args result → replacement string or nil (listeners are called (TOOL ARGS RESULT))")
                (before-provider-request waterfall "(:prompt STRING :system STRING) → same shape, or a refusal (listeners are called (VALUE NEXT))")
                (pre-step waterfall "(:session-id :run-id :turn-id :step :batch) → (:enter BATCH) | (:reject REASON) (listeners are called (PROPOSAL NEXT))")
                (context-budget-exceeded first "details → non-nil claims recovery")
                (closure-shadow observe "report")
                (session-started observe "session-id")
                (session-closed observe "session-id")))
  (superchat-define-event (nth 0 spec) (nth 1 spec) (nth 2 spec)))


(define-error 'superchat-context-required-failed
  "A required context contributor failed")

(cl-defun superchat-context-register (id function &key owner required scope)
  "Register additive context through the existing Event Scope layer.
FUNCTION receives envelope, preparation state and candidate tool names.
Required execution or validation errors terminate preparation."
  (unless (and (symbolp owner) owner (functionp function)
               (memq required '(nil t)))
    (error "Invalid context contributor"))
  (superchat-on
   scope 'collect-context id
   (lambda (&rest args)
     (condition-case err
         (let ((value (apply function args)))
           (when (and value (not (superchat-context--fragment-list-p value)))
             (error "Context contributor %S returned invalid fragments" id))
           (mapc #'superchat-prompt--validate-fragment value)
           value)
       ((error quit)
        (superchat-event-record-fault 'collect-context id owner err)
        (when required
          (signal 'superchat-context-required-failed (list :id id :condition err)))
        nil))))
  id)

(defun superchat-context-unregister (id &optional scope)
  "Withdraw ID from its registration Scope."
  (superchat-off 'collect-context id
                 (or scope superchat-plugin-current-scope superchat-scope-global)))

(declare-function superchat-prompt--validate-fragment "superchat-prompt" (fragment))

(defun superchat-context-collect (envelope state tools)
  "Collect identified fragments in the borrowed execution Scope.
Nearest/latest contributors run first; required failures propagate.
Collection prepares input and owns no Run lifecycle."
  (let ((superchat-execution-scope (superchat-execution-scope-resolve nil)))
    (delq nil
          (mapcar (lambda (listener) (funcall (cdr listener) envelope state tools))
                  (reverse (superchat-event-listeners
                            superchat-execution-scope 'collect-context))))))

(provide 'superchat-event)

;;; superchat-event.el ends here
