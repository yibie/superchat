;;; test-plugin-api.el --- Contract tests for the public plugin API -*- lexical-binding: t; -*-

;;; Commentary:

;; Third parties build on `superchat-register-command',
;; `superchat-register-tool', and `superchat-authority-register-policy'.
;; These tests are the contract: they exercise exactly what the plugin
;; guide promises and nothing internal.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-tools)
(require 'superchat-authority)
(require 'superchat-runtime)

(ert-deftest plugin/register-command-dispatches-replaces-unregisters ()
  "A registered command dispatches, can be replaced, and unregisters."
  (let ((superchat--command-alist (copy-alist superchat--command-alist)))
    (superchat-register-command
     "hello-plugin"
     (lambda (_cmd args _input _lang _model)
       (list :type :echo :content (concat "hi " args))))
    (should (equal '(:type :echo :content "hi world")
                   (funcall (cdr (assoc "hello-plugin"
                                        superchat--command-alist))
                            "hello-plugin" "world" nil "English" nil)))
    (superchat-register-command "hello-plugin"
                                (lambda (&rest _) '(:type :noop)))
    (should (equal '(:type :noop)
                   (funcall (cdr (assoc "hello-plugin"
                                        superchat--command-alist))
                            "hello-plugin" nil nil nil nil)))
    (should (superchat-unregister-command "hello-plugin"))
    (should-not (assoc "hello-plugin" superchat--command-alist))))

(ert-deftest plugin/register-command-validates-input ()
  "Command registration rejects empty names and non-functions."
  (should-error (superchat-register-command "" #'ignore))
  (should-error (superchat-register-command "x" nil)))

(ert-deftest plugin/register-tool-requires-an-effect-declaration ()
  "A plugin tool without :effects must be rejected outright."
  (should-error (superchat-register-tool
                 "plugin_tool_without_effects"
                 :function #'ignore
                 :description "No effects declared.")))

(ert-deftest plugin/registered-tool-flows-through-authority-and-tape ()
  "A registered tool resolves its declared effects and executes through
the shared Authority seam with the full audit chain."
  (let ((superchat-tools--external-registry nil)
        (superchat-llm-tools-list nil))
    (unwind-protect
        (progn
          (superchat-register-tool
           "plugin_greet"
           :function (lambda (name) (format "hello %s" name))
           :description "Greet someone by name."
           :args (list (list :name "name" :type 'string
                             :description "Who to greet."))
           :effects (lambda (args)
                      (list (list :effect 'external-read
                                  :resource 'greeting
                                  :scope (format "greet://%s" (car args))))))
          (let ((effects (superchat-authority-resolve-effects
                          "plugin_greet" '("ada"))))
            (should (eq 'external-read (plist-get (car effects) :effect)))
            (should (equal "greet://ada" (plist-get (car effects) :scope))))
          (let ((superchat-authority-approval-function (lambda (_request) t))
                (events '()))
            (cl-letf (((symbol-function 'superchat-tape-append-event)
                       (lambda (event)
                         (push (plist-get event :type) events)
                         1)))
              (should (equal "hello ada"
                             (superchat-tool-execute
                              "plugin_greet"
                              (lambda (name) (format "hello %s" name))
                              '("ada")
                              :context '(:session-id "plugin-test"
                                         :attendance attended))))
              (dolist (type '("tool.requested" "approval.requested"
                              "approval.decided" "tool.execution.started"
                              "tool.completed"))
                (should (member type events)))))
          (when (fboundp 'llm-make-tool)
            (should (cl-find "plugin_greet" (superchat-get-llm-tools)
                             :key #'llm-tool-name :test #'equal))))
      (superchat-unregister-tool "plugin_greet"))))

(ert-deftest plugin/unregister-tool-removes-its-authority-registrations ()
  "Removing a plugin tool removes the resolver and preview it installed."
  (let ((superchat-tools--external-registry nil)
        (superchat-authority-effect-resolvers (make-hash-table :test 'equal))
        (superchat-authority-approval-previews (make-hash-table :test 'equal))
        (superchat-llm-tools-list nil)
        (resolver (lambda (_args) '((:effect external-read :resource test))))
        (preview (lambda (_request) nil)))
    (unwind-protect
        (progn
          (superchat-register-tool
           "plugin_cleanup"
           :function #'ignore
           :description "Tool for cleanup testing."
           :effects resolver
           :approval-preview preview)
          (should (equal '((:effect external-read :resource test))
                         (superchat-authority-resolve-effects
                          "plugin_cleanup" nil)))
          (should (functionp (gethash "plugin_cleanup"
                                      superchat-authority-effect-resolvers)))
          (should (functionp (gethash "plugin_cleanup"
                                      superchat-authority-approval-previews)))
          (should (superchat-unregister-tool "plugin_cleanup"))
          (should-not (gethash "plugin_cleanup"
                               superchat-authority-effect-resolvers))
          (should-not (gethash "plugin_cleanup"
                               superchat-authority-approval-previews))
          (should (equal '((:effect unknown :resource dynamic
                             :scope "plugin_cleanup"))
                         (superchat-authority-resolve-effects
                          "plugin_cleanup" nil)))
          (should-not (superchat-unregister-tool "plugin_cleanup")))
      (superchat-unregister-tool "plugin_cleanup"))))

(ert-deftest plugin/unregister-tool-leaves-unclaimed-built-in-authority-entries ()
  "A built-in tool name without a plugin binding is not unregistered."
  (let ((superchat-tools--external-registry nil))
    (should-not (superchat-unregister-tool "edit_buffer"))
    (should (functionp (gethash "edit_buffer"
                                superchat-authority-effect-resolvers)))
    (should (functionp (gethash "edit_buffer"
                                superchat-authority-approval-previews)))))

(ert-deftest plugin/register-tool-rejects-built-in-name-and-preserves-authority ()
  "Plugin registration cannot shadow a built-in tool's Authority entries."
  (let ((superchat-tools--external-registry nil)
        (superchat-llm-tools-list nil))
    (let ((before-effects
           (superchat-authority-resolve-effects "edit_buffer"
                                                '("notes" "before" "after")))
          (before-preview (gethash "edit_buffer"
                                   superchat-authority-approval-previews)))
      (should-error
       (superchat-register-tool
        "edit_buffer"
        :function #'ignore
        :description "Must not shadow a built-in."
        :effects '((:effect external-write :resource message))))
      (should-not (assoc "edit_buffer" superchat-tools--external-registry))
      (should (equal before-effects
                     (superchat-authority-resolve-effects
                      "edit_buffer" '("notes" "before" "after"))))
      (should (eq before-preview
                  (gethash "edit_buffer"
                           superchat-authority-approval-previews))))))

(ert-deftest plugin/registered-policy-is-tighten-only ()
  "Registered policies compose tighten-only and cannot shadow built-ins."
  (let ((superchat-authority--policy-registry (make-hash-table :test 'eq)))
    (should-error (superchat-authority-register-policy 'cowork #'ignore))
    (superchat-authority-register-policy
     "notifier"
     (lambda (effects _context)
       (if (cl-some (lambda (fx)
                      (eq (plist-get fx :effect) 'external-write))
                    effects)
           'ask
         'allow)))
    (should (superchat-authority-policy-known-p 'notifier))
    (should (eq 'notifier (superchat-authority-normalize-policy "notifier")))
    ;; allow degrades to defer: a named policy may never widen the base.
    (should (eq 'defer (superchat-authority--policy-decision
                        'notifier
                        '((:effect local-read :resource file)) nil)))
    (should (eq 'ask (superchat-authority--policy-decision
                      'notifier
                      '((:effect external-write :resource message)) nil)))))

(ert-deftest plugin/registered-policy-is-accepted-by-runtime-requests ()
  "A registered policy name validates in a Runtime Request."
  (let ((superchat-authority--policy-registry (make-hash-table :test 'eq)))
    (superchat-authority-register-policy 'quiet (lambda (_fx _ctx) 'defer))
    (should (superchat-runtime--validate-request
             '(:prompt "hi" :authority-policy "quiet")))))

(provide 'test-plugin-api)

;;; test-plugin-api.el ends here
