;;; sk-drive.el --- drive the prototype with a fake transport -*- lexical-binding: t; -*-
(require 'cl-lib)
(load (expand-file-name "kernel-prototype.el" (file-name-directory load-file-name)))

(defvar log nil)
(defun note (fmt &rest args) (push (apply #'format fmt args) log))
(defmacro check (name form)
  `(condition-case err
       (if ,form (princ (format "PASS %s\n" ,name))
         (princ (format "FAIL %s\n" ,name)))
     (error (princ (format "FAIL %s: %s\n" ,name (error-message-string err))))))

(sk-reset)
;; Kernel vocabulary: services, events.
(sk-define-service 'model-transport '(generate))
(sk-define-service 'driver '(submit))
(sk-define-event 'run-completed 'observe)
(sk-define-event 'before-provider-request 'fold)

;; --- default plugins ------------------------------------------------------
(sk-define-plugin transport-fake
  :config '((:reply . (stringp . "fake answer")))
  :apply (lambda (scope config)
           (note "transport-fake apply reply=%s" (plist-get config :reply))
           (sk-provide scope 'model-transport
                       (list (cons 'generate
                                   (lambda (prompt)
                                     (note "generate <%s>" prompt)
                                     (plist-get config :reply)))))))

(sk-define-plugin transport-echo
  :apply (lambda (scope _)
           (note "transport-echo apply")
           (sk-provide scope 'model-transport
                       (list (cons 'generate (lambda (prompt) (concat "echo:" prompt)))))))

(sk-define-plugin driver
  :inject '(model-transport)
  :apply (lambda (scope _)
           (note "driver apply")
           (sk-provide scope 'driver
                       (list (cons 'submit
                                   (lambda (run-scope envelope)
                                     (let* ((admit (sk-stage-call run-scope 'admit envelope))
                                            (frags (apply #'append (sk-stage-call run-scope 'collect-context envelope)))
                                            (prompt (or (sk-stage-call run-scope 'assemble-prompt frags)
                                                        (mapconcat #'identity frags "\n")))
                                            (prompt (sk-emit run-scope 'before-provider-request prompt))
                                            (reply (if (eq admit 'drop) "[dropped]"
                                                     (sk-service-call run-scope 'model-transport 'generate prompt))))
                                       (sk-stage-call run-scope 'render envelope reply)
                                       (sk-emit run-scope 'run-completed envelope reply)
                                       reply)))))))

(sk-define-plugin chat-collectors
  :apply (lambda (scope _)
           (sk-stage-add scope 'collect-context 'content
                         (lambda (env) (list (plist-get env :content))))
           (sk-stage-add scope 'collect-context 'history
                         (lambda (_env) (list "history:none")))))

(sk-define-plugin chat-view
  :apply (lambda (scope _)
           (sk-stage-add scope 'render 'view (lambda (_env reply) (note "render <%s>" reply)))
           (sk-on scope 'run-completed 'view-status (lambda (_env _reply) (note "status ready")))))

;; A Preset-level plugin that shadows the history collector and folds the prompt.
(sk-define-plugin preset-persona
  :apply (lambda (scope _)
           (sk-stage-add scope 'collect-context 'history (lambda (_env) (list "history:persona-only")))
           (sk-on scope 'before-provider-request 'persona
                  (lambda (prompt) (concat "[persona] " prompt)))))

;; --- scenario -------------------------------------------------------------
(defvar sk-plugins-default '(transport-fake driver chat-collectors chat-view))
(dolist (p sk-plugins-default) (sk-mount sk--root p))

(defun submit (scope content)
  (sk-service-call scope 'driver 'submit scope (list :content content :surface 'chat)))

(check "1 default mount runs end to end"
       (equal "fake answer" (submit sk--root "hello")))
(check "1b collectors ran in registration order, nearest last"
       (member "generate <hello\nhistory:none>" log))
(check "1c render + event observed"
       (and (member "render <fake answer>" log) (member "status ready" log)))

;; Scope tree: global -> preset -> run
(let* ((preset (sk-scope-create "preset-A" sk--root))
       (run (sk-scope-create "run-1" preset)))
  (sk-mount preset 'preset-persona)
  (check "2 preset layer shadows the global history collector and folds the prompt"
         (progn (submit run "q") (member "generate <[persona] q\nhistory:persona-only>" log)))
  (check "2b global scope still sees the unshadowed collector"
         (progn (submit sk--root "g") (member "generate <g\nhistory:none>" log)))
  (sk-scope-dispose preset)
  (check "2c disposing the preset reclaims its layers and its fiber"
         (and (null (sk-scope-layers preset))
              (null (cl-find 'preset-persona sk--fibers :key (lambda (f) (sk-plugin-name (sk-fiber-plugin f)))))
              (progn (submit sk--root "after") (member "generate <after\nhistory:none>" log)))))

;; Provider loss suspends dependents; provider return re-applies them.
(let ((transport (cl-find 'transport-fake sk--fibers :key (lambda (f) (sk-plugin-name (sk-fiber-plugin f)))))
      (driver (cl-find 'driver sk--fibers :key (lambda (f) (sk-plugin-name (sk-fiber-plugin f))))))
  (sk-unmount transport)
  (check "3 losing model-transport suspends the driver (pending, no provider)"
         (and (eq 'pending (sk-fiber-state driver))
              (null (sk-registry-lookup sk--root 'service 'driver))
              (condition-case _ (progn (submit sk--root "x") nil) (error t))))
  (setq log nil)
  (sk-mount sk--root 'transport-echo)
  (check "3b a different provider re-applies the driver, which now uses it"
         (and (eq 'active (sk-fiber-state driver))
              (member "driver apply" log)
              (equal "echo:y\nhistory:none" (submit sk--root "y")))))

;; Config: validate-then-remount.
(let ((echo (cl-find 'transport-echo sk--fibers :key (lambda (f) (sk-plugin-name (sk-fiber-plugin f))))))
  (sk-unmount echo)
  (let ((fake (sk-mount sk--root 'transport-fake '(:reply "v2"))))
    (check "4 config default filled and used" (equal "v2" (submit sk--root "c")))
    (check "4b invalid config rejected at mount, plugin failed, nothing registered"
           (let ((bad (sk-mount sk--root 'transport-fake '(:reply 42))))
             (prog1 (and (eq 'failed (sk-fiber-state bad))
                         (equal "v2" (submit sk--root "still"))) ; the good one still answers
               (sk-unmount bad))))
    (setq log nil)
    (sk-reconfigure fake '(:reply "v3"))
    (check "4c reconfigure = unload + re-apply, dependents re-applied too"
           (and (member "transport-fake apply reply=v3" log)
                (member "driver apply" log)
                (equal "v3" (submit sk--root "d"))))))

;; No trace after unmounting everything.
(sk-scope-dispose sk--root)
(check "5 disposing the global scope leaves no fibers, no layers, no providers"
       (and (null sk--fibers) (sk-registry-empty-p)
            (null (sk-registry-lookup sk--root 'service 'driver))
            (condition-case _ (progn (submit sk--root "z") nil) (error t))))
(check "5b plugin *definitions* survive (they are data, not state)"
       (= 6 (length sk-plugins)))

(princ (format "prototype lines: %d\n"
               (with-temp-buffer
                 (insert-file-contents (expand-file-name "kernel-prototype.el" (file-name-directory load-file-name)))
                 (count-lines (point-min) (point-max)))))
