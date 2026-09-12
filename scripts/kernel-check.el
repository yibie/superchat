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
(superchat-plugin-reset)
(check "5 reset leaves no fibers, no layers" (and (null superchat-plugin--fibers) (superchat-scope-empty-p)))
(princ (format "KERNEL-CHECK %s (%d fails)\n" (if (zerop kc--fails) "OK" "FAILED") kc--fails))
(kill-emacs (if (zerop kc--fails) 0 1))
