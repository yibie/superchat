;;; kernel-check.el --- exercise the Kernel primitives in-package -*- lexical-binding: t; -*-
;; emacs -Q --batch -L . -l scripts/kernel-check.el   (exit 1 on any FAIL)
(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(defvar kc--fails 0)
(defmacro check (name form)
  `(condition-case err
       (if ,form (princ (format "PASS %s\n" ,name))
         (cl-incf kc--fails) (princ (format "FAIL %s\n" ,name)))
     (error (cl-incf kc--fails) (princ (format "FAIL %s: %s\n" ,name (error-message-string err))))))
(defvar kc--log nil)
(defun kc--note (fmt &rest args) (push (apply #'format fmt args) kc--log))

(superchat-plugin-reset)
(superchat-define-service 'kc-transport '(generate))
(superchat-define-service 'kc-driver '(submit))
(superchat-define-plugin kc-transport-a
  :config '((:reply stringp "A"))
  :apply (lambda (scope config)
           (kc--note "transport-a apply %s" (plist-get config :reply))
           (superchat-service-provide scope 'kc-transport
             (list (cons 'generate (lambda (p) (concat (plist-get config :reply) ":" p)))))))
(superchat-define-plugin kc-transport-b
  :apply (lambda (scope _)
           (superchat-service-provide scope 'kc-transport
             (list (cons 'generate (lambda (p) (concat "B:" p)))))))
(superchat-define-plugin kc-driver
  :inject '(kc-transport)
  :apply (lambda (scope _)
           (kc--note "driver apply")
           (superchat-scope-register scope '(stage kc-collect) 'content (lambda (e) e))
           (superchat-service-provide scope 'kc-driver
             (list (cons 'submit (lambda (run-scope text)
                                   (let ((frags (mapcar (lambda (i) (funcall (cdr i) text))
                                                        (superchat-scope-merge run-scope '(stage kc-collect)))))
                                     (superchat-service-call run-scope 'kc-transport 'generate
                                                             (mapconcat #'identity frags "+")))))))))
(superchat-define-plugin kc-persona
  :apply (lambda (scope _)
           (superchat-scope-register scope '(stage kc-collect) 'persona (lambda (_e) "persona"))))

(superchat-plugin-mount 'kc-transport-a)
(superchat-plugin-mount 'kc-driver)
(defun kc--submit (scope text) (superchat-service-call scope 'kc-driver 'submit scope text))
(check "1 mount + service call end to end" (equal "A:hi" (kc--submit superchat-scope-global "hi")))
(let* ((preset (superchat-scope-create "preset" superchat-scope-global))
       (run (superchat-scope-create "run" preset)))
  (superchat-plugin-mount 'kc-persona nil preset)
  (check "2 preset scope adds a collector seen from the run scope" (equal "A:q+persona" (kc--submit run "q")))
  (check "2b global scope does not see it" (equal "A:g" (kc--submit superchat-scope-global "g")))
  (check "2c a preset may not provide a service"
         (condition-case _ (progn (superchat-plugin-mount 'kc-transport-b nil preset)
                                  (eq 'failed (superchat-plugin-state 'kc-transport-b preset)))
           (error t)))
  (superchat-scope-dispose preset)
  (check "2d disposing the preset unmounts its plugins and layers"
         (and (null (superchat-plugin-state 'kc-persona)) (null (superchat-scope-layers preset))
              (equal "A:z" (kc--submit superchat-scope-global "z")))))
(superchat-plugin-unmount 'kc-transport-a)
(check "3 provider loss suspends the driver"
       (and (eq 'pending (superchat-plugin-state 'kc-driver))
            (null (superchat-service-get nil 'kc-driver))))
(setq kc--log nil)
(superchat-plugin-mount 'kc-transport-b)
(check "3b another provider re-applies the driver"
       (and (eq 'active (superchat-plugin-state 'kc-driver)) (member "driver apply" kc--log)
            (equal "B:y" (kc--submit superchat-scope-global "y"))))
(superchat-plugin-unmount 'kc-transport-b)
(superchat-plugin-mount 'kc-transport-a '(:reply "v2"))
(check "4 config used" (equal "v2:c" (kc--submit superchat-scope-global "c")))
(check "4b unknown config key -> failed, nothing registered, good provider untouched"
       (progn (superchat-plugin-mount 'kc-transport-b '(:nope 1))
              (prog1 (and (eq 'failed (superchat-plugin-state 'kc-transport-b))
                          (equal "v2:c" (kc--submit superchat-scope-global "c")))
                (superchat-plugin-unmount 'kc-transport-b))))
(check "4b2 invalid value -> failed state"
       (progn (superchat-plugin-unmount 'kc-transport-a)
              (superchat-plugin-mount 'kc-transport-a '(:reply 42))
              (prog1 (and (eq 'failed (superchat-plugin-state 'kc-transport-a))
                          (null (superchat-service-get nil 'kc-transport)))
                (superchat-plugin-unmount 'kc-transport-a)
                (superchat-plugin-mount 'kc-transport-a '(:reply "v2")))))
(setq kc--log nil)
(superchat-plugin-reconfigure 'kc-transport-a '(:reply "v3"))
(check "4c reconfigure re-applies dependents" (and (member "driver apply" kc--log) (equal "v3:d" (kc--submit superchat-scope-global "d"))))
(superchat-plugin-unmount 'kc-transport-a)
(let ((scope (superchat-scope-create "direct-provider" superchat-scope-global superchat-scope-global)))
  (superchat-service-provide scope 'kc-transport (list (cons 'generate #'identity)))
  (check "4d direct Scope provider activates dependent" (eq 'active (superchat-plugin-state 'kc-driver)))
  (superchat-scope-dispose scope)
  (check "4e direct Scope disposal withdraws before notifying"
         (and (null (superchat-service-get nil 'kc-transport))
              (null (superchat-service-get nil 'kc-driver))
              (eq 'pending (superchat-plugin-state 'kc-driver))))
  (superchat-plugin-mount 'kc-transport-a)
  (check "4f provider return re-applies after direct disposal"
         (equal "A:restored" (kc--submit superchat-scope-global "restored"))))
(superchat-plugin-unmount 'kc-transport-a)
(let* ((scope (superchat-scope-create "returned-undo" superchat-scope-global superchat-scope-global))
       (undo (superchat-service-provide scope 'kc-transport (list (cons 'generate #'identity)))))
  (funcall undo)
  (check "4h returned Service undo notifies after withdrawal"
         (and (null (superchat-scope-effects scope))
              (null (superchat-service-get nil 'kc-transport))
              (null (superchat-service-get nil 'kc-driver))
              (eq 'pending (superchat-plugin-state 'kc-driver))))
  (superchat-plugin-mount 'kc-transport-a)
  (superchat-scope-dispose scope)
  (check "4i disposal after manual undo preserves returned provider"
         (equal "A:again" (kc--submit superchat-scope-global "again"))))
(let* ((scope (superchat-scope-create "identity-check" superchat-scope-global))
       (_ (superchat-scope-register scope 'identity 'keeper t))
       (undo (superchat-scope-register scope 'identity 'x 'old)))
  (funcall undo)
  (superchat-scope-register scope 'identity 'x 'new)
  (funcall undo)
  (check "4g old undo cannot withdraw a new registration"
         (eq 'new (superchat-scope-lookup scope 'identity 'x)))
  (superchat-scope-dispose scope))
(let* ((scope (superchat-scope-create "effect-order" superchat-scope-global))
       (child (superchat-scope-create "child" scope))
       (log nil)
       (manual (superchat-scope-effect scope (lambda () (push 'manual log)))))
  (funcall manual)
  (funcall manual)
  (check "4j manual effect undo detaches and runs exactly once"
         (and (null (superchat-scope-effects scope)) (equal log '(manual))))
  (superchat-scope-effect scope (lambda () (push 'older log)))
  (let ((newer (superchat-scope-effect scope (lambda () (push 'newer log)))))
    (superchat-scope-effect child (lambda () (push 'child log)))
    (superchat-scope-dispose scope)
    (funcall newer)
    (superchat-scope-dispose scope)
    (check "4k dispose is children-first, reverse registration, synchronous and idempotent"
           (equal (reverse log) '(manual child newer older)))))
(let* ((scope (superchat-scope-create "effect-reentry" superchat-scope-global))
       (calls 0) undo)
  (setq undo (superchat-scope-effect scope (lambda () (cl-incf calls) (funcall undo))))
  (check "4l undo detaches before reentrant invocation"
         (progn (funcall undo)
                (and (= calls 1) (null (superchat-scope-effects scope)))))
  (superchat-scope-dispose scope))
(let* ((scope (superchat-scope-create "plugin-effect" superchat-scope-global))
       (superchat-plugin-current-scope scope)
       (calls 0)
       (undo (superchat-plugin-record-undo (lambda () (cl-incf calls)))))
  (funcall undo)
  (funcall undo)
  (superchat-scope-dispose scope)
  (check "4m plugin undo adapter returns the same one-shot cleanup"
         (and (= calls 1) (null (superchat-scope-effects scope)))))
(defvar kc--fiber-active-unloads 0)
(defvar kc--fiber-consumer-applies 0)
(defvar kc--fiber-provider-cleanups 0)
(superchat-define-plugin kc-fiber-active
  :apply (lambda (scope _)
           (superchat-scope-effect
            scope (lambda () (cl-incf kc--fiber-active-unloads)))))
(superchat-define-plugin kc-fiber-pending
  :inject '(kc-fiber-absent)
  :apply (lambda (_scope _) (error "kc-fiber-pending unexpectedly applied")))
(superchat-define-plugin kc-fiber-failed
  :apply (lambda (_scope _) (error "kc-fiber-failed expected failure")))
;; A fiber's mount cleanup is the same Scope effect whether it is consumed by
;; a manual unmount or by Scope disposal.  Exercise active, pending, and
;; failed fibers because only the first has a live effect child.
(let* ((scope (superchat-scope-create "fiber-states" superchat-scope-global))
       (active (superchat-plugin-mount 'kc-fiber-active nil scope))
       (pending (superchat-plugin-mount 'kc-fiber-pending nil scope))
       (failed (superchat-plugin-mount 'kc-fiber-failed nil scope))
       (undos (mapcar #'superchat-plugin-fiber-cleanup
                      (list active pending failed))))
  (check "5a fibers retain one cleanup in active pending and failed states"
         (and (eq 'active (superchat-plugin-fiber-state active))
              (eq 'pending (superchat-plugin-fiber-state pending))
              (eq 'failed (superchat-plugin-fiber-state failed))
              (cl-every #'functionp undos)
              (= 3 (length (superchat-scope-effects scope)))))
  (superchat-scope-dispose scope)
  (mapc #'funcall undos)
  (check "5b Scope disposal consumes every fiber cleanup exactly once"
         (and (null (superchat-plugin-fiber 'kc-fiber-active scope))
              (null (superchat-plugin-fiber 'kc-fiber-pending scope))
              (null (superchat-plugin-fiber 'kc-fiber-failed scope))
              (null (superchat-scope-effects scope))
              (cl-every (lambda (fiber)
                          (null (superchat-plugin-fiber-cleanup fiber)))
                        (list active pending failed))
              (= 1 kc--fiber-active-unloads))))
;; The original retention bug grew this list by one on every manual
;; mount/unmount because manual unmount bypassed the Scope effect.
(let* ((scope (superchat-scope-create "fiber-retention" superchat-scope-global))
       (before (length (superchat-scope-effects scope))))
  (dotimes (_ 100)
    (superchat-plugin-mount 'kc-fiber-active nil scope)
    (superchat-plugin-unmount 'kc-fiber-active scope))
  (check "5c one hundred manual mount/unmount cycles retain no Scope effects"
         (and (= before (length (superchat-scope-effects scope)))
              (null (superchat-plugin-fiber 'kc-fiber-active scope))))
  (superchat-scope-dispose scope))
;; A completed old undo is safe to retain across a same-name remount.  Its
;; child cleanup may notify reentrantly, but the old fiber is already detached
;; before that user code runs.
(let* ((scope (superchat-scope-create "fiber-stale-cleanup" superchat-scope-global))
       (before kc--fiber-active-unloads)
       (old (superchat-plugin-mount 'kc-fiber-active nil scope))
       (old-undo (superchat-plugin-fiber-cleanup old))
       fresh fresh-undo)
  (superchat-plugin-unmount 'kc-fiber-active scope)
  (setq fresh (superchat-plugin-mount 'kc-fiber-active nil scope)
        fresh-undo (superchat-plugin-fiber-cleanup fresh))
  (funcall old-undo)
  (check "5d stale fiber cleanup cannot unmount a fresh same-name fiber"
         (and (eq fresh (superchat-plugin-fiber 'kc-fiber-active scope))
              (eq 'active (superchat-plugin-fiber-state fresh))
              (null (superchat-plugin-fiber-cleanup old))
              (= (1+ before) kc--fiber-active-unloads)))
  (superchat-scope-dispose scope)
  (funcall old-undo)
  (funcall fresh-undo)
  (check "5e manual then dispose and dispose then old undo stay one-shot"
         (and (null (superchat-plugin-fiber 'kc-fiber-active scope))
              (null (superchat-scope-effects scope))
              (null (superchat-plugin-fiber-cleanup fresh))
              (= (+ before 2) kc--fiber-active-unloads))))
(superchat-define-service 'kc-fiber-provider '(ping))
(superchat-define-plugin kc-fiber-provider
  :apply (lambda (scope _)
           (superchat-scope-effect
            scope (lambda () (cl-incf kc--fiber-provider-cleanups)))
           (superchat-service-provide
            scope 'kc-fiber-provider (list (cons 'ping #'identity)))))
(superchat-define-plugin kc-fiber-consumer
  :inject '(kc-fiber-provider)
  :apply (lambda (_scope _) (cl-incf kc--fiber-consumer-applies)))
;; Provider withdrawal notifies while the provider fiber is unloading.  This
;; is a real reentry path: its dependent must suspend once, resume once when a
;; fresh provider mounts, and an old retained cleanup must not perturb either.
(let* ((before-effects (length (superchat-scope-effects superchat-scope-global)))
       (before-applies kc--fiber-consumer-applies)
       provider old-undo fresh)
  (setq provider (superchat-plugin-mount 'kc-fiber-provider)
        old-undo (superchat-plugin-fiber-cleanup provider))
  (superchat-plugin-mount 'kc-fiber-consumer)
  (superchat-plugin-unmount 'kc-fiber-provider)
  (check "5f provider cleanup reentry suspends its dependent once"
         (and (null (superchat-service-get nil 'kc-fiber-provider))
              (eq 'pending (superchat-plugin-state 'kc-fiber-consumer))
              (= 1 kc--fiber-provider-cleanups)
              (= (1+ before-applies) kc--fiber-consumer-applies)))
  (setq fresh (superchat-plugin-mount 'kc-fiber-provider))
  (funcall old-undo)
  (check "5g provider return resumes once and an old cleanup is inert"
         (and (eq fresh (superchat-plugin-fiber 'kc-fiber-provider))
              (eq 'active (superchat-plugin-state 'kc-fiber-consumer))
              (= 1 kc--fiber-provider-cleanups)
              (= (+ before-applies 2) kc--fiber-consumer-applies)))
  (superchat-plugin-unmount 'kc-fiber-consumer)
  (superchat-plugin-unmount 'kc-fiber-provider)
  (check "5h provider reentry leaves the global effect baseline unchanged"
         (and (= 2 kc--fiber-provider-cleanups)
              (= before-effects (length (superchat-scope-effects superchat-scope-global))))))
;; Execution retirement is a distinct, monotonic admission latch.  It follows
;; object ancestry but does not turn ordinary Scope disposal into Runtime
;; cancellation or conflate same-named siblings.
(let* ((a (superchat-scope-create "retire-A" superchat-scope-global))
       (child (superchat-scope-create "retire-child" a))
       (same-name-sibling (superchat-scope-create "retire-A" superchat-scope-global))
       (ordinary (superchat-scope-create "ordinary-dispose" superchat-scope-global))
       (nil-error
        (condition-case _
            (progn (superchat-execution-scope-retire nil) nil)
          (error t)))
       (global-error
        (condition-case _
            (progn (superchat-execution-scope-retire superchat-scope-global) nil)
          (error t))))
  (superchat-scope-dispose ordinary)
  (superchat-execution-scope-retire a)
  (superchat-scope-dispose a)
  (check "5i execution retirement is ancestry-private and disposal remains separate"
         (and nil-error global-error
              (superchat-execution-scope-retired-p a)
              (superchat-execution-scope-retired-p child)
              (not (superchat-execution-scope-retired-p same-name-sibling))
              (not (superchat-execution-scope-retired-p superchat-scope-global))
              (not (superchat-execution-scope-retired-p ordinary))))
  (superchat-scope-dispose same-name-sibling))
(superchat-plugin-reset)
(check "6 reset leaves no fibers, no layers" (and (null superchat-plugin--fibers) (superchat-scope-empty-p)))
(princ (format "KERNEL-CHECK %s (%d fails)\n" (if (zerop kc--fails) "OK" "FAILED") kc--fails))
(kill-emacs (if (zerop kc--fails) 0 1))
