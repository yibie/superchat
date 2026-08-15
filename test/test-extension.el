;;; test-extension.el --- Contract tests for declarative extensions -*- lexical-binding: t; -*-

;;; Commentary:

;; `superchat-define-extension' is the third-party contract for
;; activation-scoped contributions.  These tests pin the promises the
;; plugin guide makes: inactive extensions cost nothing, explicit tool
;; lists are never widened, composition is idempotent and audited.

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat)
(require 'superchat-extension)

(defun superchat-extension-test--request (&rest overrides)
  "Return a minimal Runtime Request merged with OVERRIDES."
  (append overrides
          (list :prompt "User question: hello"
                :input "hello"
                :session-id "extension-test"
                :origin '(:surface test-surface))))

(ert-deftest extension/define-validates-replaces-unregisters ()
  "Declarations validate, replace by name, and unregister."
  (let ((superchat-extension--registry nil))
    (should-error (superchat-define-extension 'bad-ext))
    (should-error (superchat-define-extension "not-a-symbol"
                                              :description "x"))
    (should-error (superchat-define-extension 'bad-ext
                                              :description "x"
                                              :active-when "not-a-fn"))
    (superchat-define-extension 'ext-a :description "first")
    (superchat-define-extension 'ext-a :description "second")
    (should (= 1 (length superchat-extension--registry)))
    (should (equal "second"
                   (plist-get (alist-get 'ext-a
                                         superchat-extension--registry)
                              :description)))
    (should (superchat-extension-unregister 'ext-a))
    (should-not superchat-extension--registry)))

(ert-deftest extension/unregister-removes-owned-command-and-policy ()
  "Unregistering an extension removes its command and policy seams."
  (let ((superchat-extension--registry nil)
        (superchat--command-alist (copy-alist superchat--command-alist))
        (superchat-authority--policy-registry (make-hash-table :test 'eq)))
    (superchat-define-extension
     'owned-extension
     :description "Owns public seams"
     :commands (list (cons "owned-command" #'ignore))
     :policies (list (cons 'owned-policy #'ignore)))
    (should (assoc "owned-command" superchat--command-alist))
    (should (superchat-authority-policy-known-p 'owned-policy))
    (should (superchat-extension-unregister 'owned-extension))
    (should-not (assoc "owned-command" superchat--command-alist))
    (should-not (superchat-authority-policy-known-p 'owned-policy))
    (should-error (superchat-authority-normalize-policy 'owned-policy))
    (should-error
     (superchat-authority-decide "read_buffer" nil 'attended
                                 'owned-policy))))

(ert-deftest extension/unregister-skips-contributions-owned-by-newer-extension ()
  "Unregistering an older extension leaves newer same-name bindings usable."
  (let ((superchat-extension--registry nil)
        (superchat--command-alist (copy-alist superchat--command-alist))
        (superchat-authority--policy-registry (make-hash-table :test 'eq))
        (command-a (lambda (&rest _args) 'from-a))
        (command-b (lambda (&rest _args) 'from-b))
        (policy-a (lambda (_effects _context) 'ask))
        (policy-b (lambda (_effects _context) 'defer)))
    (superchat-define-extension
     'older-extension
     :description "Older owner"
     :commands (list (cons "shared-extension-command" command-a))
     :policies (list (cons 'shared-extension-policy policy-a)))
    (superchat-define-extension
     'newer-extension
     :description "Newer owner"
     :commands (list (cons "shared-extension-command" command-b))
     :policies (list (cons 'shared-extension-policy policy-b)))
    (let ((older-spec (cdr (assq 'older-extension superchat-extension--registry))))
      (should (eq command-a
                  (cdr (assoc "shared-extension-command"
                              (plist-get older-spec :commands)))))
      (should (eq policy-a
                  (cdr (assoc 'shared-extension-policy
                              (plist-get older-spec :policies))))))
    (should (superchat-extension-unregister 'older-extension))
    (let ((handler (cdr (assoc "shared-extension-command"
                               superchat--command-alist))))
      (should (eq command-b handler))
      (should (equal 'from-b
                     (funcall handler "shared-extension-command"
                              nil nil nil nil))))
    (should (eq 'shared-extension-policy
                (superchat-authority-normalize-policy
                 'shared-extension-policy)))
    (should (eq policy-b
                (gethash 'shared-extension-policy
                         superchat-authority--policy-registry)))
    (should (superchat-extension-unregister 'newer-extension))
    (should-not (assoc "shared-extension-command" superchat--command-alist))
    (should-error
     (superchat-authority-normalize-policy 'shared-extension-policy))))

(ert-deftest extension/unregister-restores-overridden-built-in-command ()
  "Unregistering an extension does not erase an overridden built-in command."
  (let ((superchat-extension--registry nil)
        (superchat--command-alist (copy-alist superchat--command-alist))
        (plugin-handler (lambda (&rest _args) 'plugin-clear))
        (builtin-handler (cdr (assoc "clear" superchat--command-alist))))
    (superchat-define-extension
     'built-in-overrider
     :description "Temporarily overrides a command"
     :commands (list (cons "clear" plugin-handler)))
    (should (eq plugin-handler
                (cdr (assoc "clear" superchat--command-alist))))
    (should (superchat-extension-unregister 'built-in-overrider))
    (should (eq builtin-handler
                (cdr (assoc "clear" superchat--command-alist))))))

(ert-deftest extension/shadow-stack-restores-base-after-older-owner-unregister ()
  "Removing an older owner leaves the newer owner and then restores BASE."
  (let* ((superchat-extension--registry nil)
         (superchat-extension--command-shadows
          (make-hash-table :test 'equal))
         (superchat-extension--policy-shadows
          (make-hash-table :test 'eq))
         (superchat--command-alist (copy-alist superchat--command-alist))
         (superchat-authority--policy-registry (make-hash-table :test 'eq))
         (base-command (cdr (assoc "recall" superchat--command-alist)))
         (base-policy (lambda (&rest _args) 'base)))
    (puthash 'shadow-stack-policy base-policy
             superchat-authority--policy-registry)
    (let ((command-a (lambda (&rest _args) 'from-a))
          (command-b (lambda (&rest _args) 'from-b))
          (policy-a (lambda (&rest _args) 'from-a))
          (policy-b (lambda (&rest _args) 'from-b)))
      (superchat-define-extension
       'shadow-stack-a
       :description "Shadow stack A"
       :commands (list (cons "recall" command-a))
       :policies (list (cons 'shadow-stack-policy policy-a)))
      (superchat-define-extension
       'shadow-stack-b
       :description "Shadow stack B"
       :commands (list (cons "recall" command-b))
       :policies (list (cons 'shadow-stack-policy policy-b)))
      (should (eq command-b (cdr (assoc "recall" superchat--command-alist))))
      (should (eq policy-b
                  (gethash 'shadow-stack-policy
                           superchat-authority--policy-registry)))
      (should (superchat-extension-unregister 'shadow-stack-a))
      (should (eq command-b (cdr (assoc "recall" superchat--command-alist))))
      (should (eq policy-b
                  (gethash 'shadow-stack-policy
                           superchat-authority--policy-registry)))
      (should (superchat-extension-unregister 'shadow-stack-b))
      (should (eq base-command (cdr (assoc "recall" superchat--command-alist))))
      (let ((decider (gethash 'shadow-stack-policy
                              superchat-authority--policy-registry)))
        (should (eq base-policy decider))
        (should (eq 'base (funcall decider nil nil)))))))

(ert-deftest extension/shadow-stack-removes-stale-owner-on-redeclaration ()
  "Redeclaring an owner without a name removes its stale stack entry."
  (let* ((superchat-extension--registry nil)
         (superchat-extension--command-shadows
          (make-hash-table :test 'equal))
         (superchat-extension--policy-shadows
          (make-hash-table :test 'eq))
         (superchat--command-alist (copy-alist superchat--command-alist))
         (superchat-authority--policy-registry (make-hash-table :test 'eq))
         (base-command (cdr (assoc "recall" superchat--command-alist)))
         (base-policy (lambda (&rest _args) 'base)))
    (puthash 'shadow-redeclare-policy base-policy
             superchat-authority--policy-registry)
    (let ((command-a (lambda (&rest _args) 'from-a))
          (command-b (lambda (&rest _args) 'from-b))
          (policy-a (lambda (&rest _args) 'from-a))
          (policy-b (lambda (&rest _args) 'from-b)))
      (superchat-define-extension
       'shadow-redeclare-a
       :description "Shadow redeclaration A"
       :commands (list (cons "recall" command-a))
       :policies (list (cons 'shadow-redeclare-policy policy-a)))
      (superchat-define-extension
       'shadow-redeclare-b
       :description "Shadow redeclaration B"
       :commands (list (cons "recall" command-b))
       :policies (list (cons 'shadow-redeclare-policy policy-b)))
      (superchat-define-extension
       'shadow-redeclare-a
       :description "Shadow redeclaration A without seams")
      (should (eq command-b (cdr (assoc "recall" superchat--command-alist))))
      (should (eq policy-b
                  (gethash 'shadow-redeclare-policy
                           superchat-authority--policy-registry)))
      (should (superchat-extension-unregister 'shadow-redeclare-b))
      (should (eq base-command (cdr (assoc "recall" superchat--command-alist))))
      (let ((decider (gethash 'shadow-redeclare-policy
                              superchat-authority--policy-registry)))
        (should (eq base-policy decider))
        (should (eq 'base (funcall decider nil nil))))
      (should (superchat-extension-unregister 'shadow-redeclare-a)))))

(ert-deftest extension/shadow-stack-ownership-is-owner-not-handler ()
  "Identical handler objects do not make two extension owners identical."
  (let* ((superchat-extension--registry nil)
         (superchat-extension--command-shadows
          (make-hash-table :test 'equal))
         (superchat-extension--policy-shadows
          (make-hash-table :test 'eq))
         (superchat--command-alist (copy-alist superchat--command-alist))
         (superchat-authority--policy-registry (make-hash-table :test 'eq))
         (base-command (cdr (assoc "recall" superchat--command-alist)))
         (base-policy (lambda (&rest _args) 'base))
         (shared-handler (lambda (&rest _args) 'shared)))
    (puthash 'shadow-shared-policy base-policy
             superchat-authority--policy-registry)
    (superchat-define-extension
     'shadow-shared-a
     :description "Shared handler A"
     :commands (list (cons "recall" shared-handler))
     :policies (list (cons 'shadow-shared-policy shared-handler)))
    (superchat-define-extension
     'shadow-shared-b
     :description "Shared handler B"
     :commands (list (cons "recall" shared-handler))
     :policies (list (cons 'shadow-shared-policy shared-handler)))
    (should (superchat-extension-unregister 'shadow-shared-a))
    (let ((handler (cdr (assoc "recall" superchat--command-alist))))
      (should (eq shared-handler handler))
      (should (eq 'shared
                  (funcall handler "recall" nil nil nil nil))))
    (let ((decider (gethash 'shadow-shared-policy
                            superchat-authority--policy-registry)))
      (should (eq shared-handler decider))
      (should (eq 'shared (funcall decider nil nil))))
    (should (superchat-extension-unregister 'shadow-shared-b))
    (should (eq base-command (cdr (assoc "recall" superchat--command-alist))))
    (should (eq base-policy
                (gethash 'shadow-shared-policy
                         superchat-authority--policy-registry)))))

(ert-deftest extension/shadow-stack-resnapshots-after-base-owner-removed ()
  "A later direct registration becomes BASE after an old shadow is removed."
  (let* ((superchat-extension--registry nil)
         (superchat-extension--command-shadows
          (make-hash-table :test 'equal))
         (superchat-extension--policy-shadows
          (make-hash-table :test 'eq))
         (superchat--command-alist (copy-alist superchat--command-alist))
         (superchat-authority--policy-registry (make-hash-table :test 'eq))
         (base-command (cdr (assoc "recall" superchat--command-alist)))
         (base-policy (lambda (&rest _args) 'h0))
         (user-command (lambda (&rest _args) 'h9))
         (user-policy (lambda (&rest _args) 'h9)))
    (superchat-authority-register-policy
     'shadow-resnapshot-policy base-policy)
    (superchat-define-extension
     'shadow-resnapshot-a
     :description "Initial shadow owner"
     :commands (list (cons "recall" (lambda (&rest _args) 'a)))
     :policies (list (cons 'shadow-resnapshot-policy
                           (lambda (&rest _args) 'a))))
    (should (superchat-extension-unregister 'shadow-resnapshot-a))
    (should (eq base-command (cdr (assoc "recall" superchat--command-alist))))
    (should (eq base-policy
                (gethash 'shadow-resnapshot-policy
                         superchat-authority--policy-registry)))
    (should-not (gethash "recall" superchat-extension--command-shadows))
    (should-not (gethash 'shadow-resnapshot-policy
                         superchat-extension--policy-shadows))
    (superchat-register-command "recall" user-command)
    (superchat-authority-register-policy
     'shadow-resnapshot-policy user-policy)
    (superchat-define-extension
     'shadow-resnapshot-b
     :description "Second shadow owner"
     :commands (list (cons "recall" (lambda (&rest _args) 'b)))
     :policies (list (cons 'shadow-resnapshot-policy
                           (lambda (&rest _args) 'b))))
    (should (superchat-extension-unregister 'shadow-resnapshot-b))
    (let ((handler (cdr (assoc "recall" superchat--command-alist)))
          (decider (gethash 'shadow-resnapshot-policy
                            superchat-authority--policy-registry)))
      (should (eq user-command handler))
      (should (eq 'h9 (funcall handler "recall" nil nil nil nil)))
      (should (eq user-policy decider))
      (should (eq 'h9 (funcall decider nil nil))))))

(ert-deftest extension/replacement-removes-old-owned-command ()
  "Replacing an extension does not leave its old command registered."
  (let ((superchat-extension--registry nil)
        (superchat--command-alist (copy-alist superchat--command-alist))
        (superchat-authority--policy-registry (make-hash-table :test 'eq)))
    (superchat-define-extension
     'replaceable-extension
     :description "Old declaration"
     :commands (list (cons "old-extension-command" #'ignore)))
    (superchat-define-extension
     'replaceable-extension
     :description "New declaration"
     :commands (list (cons "new-extension-command" #'ignore)))
    (should-not (assoc "old-extension-command" superchat--command-alist))
    (should (assoc "new-extension-command" superchat--command-alist))))

(ert-deftest extension/replacement-with-invalid-handler-is-atomic ()
  "Invalid replacement contributions leave the old declaration untouched."
  (let ((superchat-extension--registry nil)
        (superchat--command-alist (copy-alist superchat--command-alist))
        (superchat-authority--policy-registry (make-hash-table :test 'eq))
        (old-handler (lambda (&rest _args) 'old)))
    (superchat-define-extension
     'transactional-extension
     :description "Original declaration"
     :commands (list (cons "transactional-command" old-handler)))
    (let ((old-spec (cdr (assq 'transactional-extension
                               superchat-extension--registry))))
      (should-error
       (superchat-define-extension
        'transactional-extension
        :description "Invalid handler"
        :commands (list (cons "new-transactional-command" 42))))
      (should (eq old-spec
                  (cdr (assq 'transactional-extension
                             superchat-extension--registry))))
      (let ((handler (cdr (assoc "transactional-command"
                                 superchat--command-alist))))
        (should (eq old-handler handler))
        (should (equal 'old
                       (funcall handler "transactional-command"
                                nil nil nil nil))))
      (should-not (assoc "new-transactional-command"
                         superchat--command-alist)))
    (should-error
     (superchat-define-extension
      'transactional-extension
      :description "Invalid policy"
      :commands (list (cons "new-transactional-command" #'ignore))
      :policies (list (cons 'default #'ignore))))
    (should (assoc "transactional-command" superchat--command-alist))
    (superchat-extension-unregister 'transactional-extension)))

(ert-deftest extension/inactive-extension-costs-nothing ()
  "A non-matching extension leaves the Request untouched."
  (let ((superchat-extension--registry nil))
    (superchat-define-extension
     'other-surface
     :description "Only for another surface"
     :active-when (lambda (request)
                    (eq (plist-get (plist-get request :origin) :surface)
                        'elsewhere))
     :tools '("some_tool")
     :context (lambda (_request) "should never appear")
     :knowledge "should never appear")
    (let ((composed (superchat-extension-compose-request
                     (superchat-extension-test--request))))
      (should (plist-member composed :superchat-extensions))
      (should (null (plist-get composed :superchat-extensions)))
      (should-not (plist-get composed :tools))
      (should (equal "User question: hello" (plist-get composed :prompt)))
      (should-not (plist-get composed :system-prompt)))))

(ert-deftest extension/active-extension-composes-contributions ()
  "An active extension contributes tools, context, and knowledge."
  (let ((superchat-extension--registry nil)
        (superchat-llm-tool-names '("read-file" "memory_search")))
    (superchat-define-extension
     'test-ext
     :description "Test extension for the test surface"
     :active-when (lambda (request)
                    (eq (plist-get (plist-get request :origin) :surface)
                        'test-surface))
     :tools '("test_tool")
     :context (lambda (request)
                (format "origin=%s"
                        (plist-get (plist-get request :origin) :surface)))
     :knowledge "Always draft before sending.")
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (_event) 1)))
      (let ((composed (superchat-extension-compose-request
                       (superchat-extension-test--request))))
        (should (equal '(test-ext)
                       (plist-get composed :superchat-extensions)))
        (should (equal '("read-file" "memory_search" "test_tool")
                       (plist-get composed :tools)))
        (should (string-match-p "\\[test-ext context\\]\norigin=test-surface"
                                (plist-get composed :prompt)))
        (let ((system (plist-get composed :system-prompt)))
          (should (string-match-p "Active Superchat extensions" system))
          (should (string-match-p "Test extension for the test surface"
                                  system))
          (should (string-match-p "Always draft before sending." system)))))))

(ert-deftest extension/explicit-tool-lists-are-never-widened ()
  "An explicit `:tools' value is a narrowing decision composition keeps."
  (let ((superchat-extension--registry nil))
    (superchat-define-extension
     'greedy :description "Wants its tool everywhere"
     :tools '("greedy_tool"))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (_event) 1)))
      (let ((explicit (superchat-extension-compose-request
                       (superchat-extension-test--request
                        :tools '("read-file"))))
            (disabled (superchat-extension-compose-request
                       (superchat-extension-test--request :tools 'none))))
        (should (equal '("read-file") (plist-get explicit :tools)))
        (should (eq 'none (plist-get disabled :tools)))))))

(ert-deftest extension/composition-is-idempotent ()
  "Composing an already-composed Request changes nothing."
  (let ((superchat-extension--registry nil)
        (superchat-llm-tool-names '("read-file")))
    (superchat-define-extension
     'once :description "Composes once"
     :tools '("once_tool")
     :context (lambda (_request) "injected"))
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (_event) 1)))
      (let* ((first (superchat-extension-compose-request
                     (superchat-extension-test--request)))
             (second (superchat-extension-compose-request first)))
        (should (eq first second))
        (should (= 1 (cl-count "once_tool"
                               (plist-get second :tools)
                               :test #'equal)))))))

(ert-deftest extension/claimed-tools-leave-the-global-surface ()
  "A claimed plugin tool is exposed only when the allowlist names it."
  (let ((superchat-extension--registry nil)
        (superchat-tools--external-registry nil)
        (superchat-llm-tools-list nil))
    (unwind-protect
        (progn
          (superchat-register-tool
           "claimed_tool"
           :function #'ignore
           :description "Claimed by an extension."
           :effects '((:effect external-read :resource test)))
          (superchat-define-extension
           'claimant :description "Claims claimed_tool"
           :tools '("claimed_tool"))
          (should (superchat-extension-claimed-tool-p "claimed_tool"))
          (skip-unless (fboundp 'llm-make-tool))
          (let ((superchat-llm-tool-names '("read-file")))
            (should-not (cl-find "claimed_tool" (superchat-get-llm-tools)
                                 :key #'llm-tool-name :test #'equal)))
          (should (cl-find "claimed_tool"
                           (superchat-get-llm-tools '("claimed_tool"))
                           :key #'llm-tool-name :test #'equal)))
      (superchat-unregister-tool "claimed_tool"))))

(ert-deftest extension/activation-is-audited-on-the-tape ()
  "Composition writes one extension.activated event for the Session."
  (let ((superchat-extension--registry nil)
        (events '()))
    (superchat-define-extension 'audited :description "Audited extension")
    (cl-letf (((symbol-function 'superchat-tape-append-event)
               (lambda (event) (push event events) 1)))
      (superchat-extension-compose-request
       (superchat-extension-test--request)))
    (should (= 1 (length events)))
    (let ((event (car events)))
      (should (equal "extension.activated" (plist-get event :type)))
      (should (equal "extension-test" (plist-get event :session-id)))
      (should (equal '("audited")
                     (plist-get (plist-get event :payload) :extensions))))))

(ert-deftest extension/listing-command-reports-contributions ()
  "/extensions lists names, conditionality, and tools."
  (let ((superchat-extension--registry nil))
    (should (string-match-p
             "No Superchat extensions"
             (plist-get (superchat--cmd-extensions nil nil nil nil nil)
                        :content)))
    (superchat-define-extension
     'listed :description "Listed extension"
     :active-when #'ignore
     :tools '("listed_tool"))
    (let ((content (plist-get (superchat--cmd-extensions nil nil nil nil nil)
                              :content)))
      (should (string-match-p "listed (conditional) — Listed extension"
                              content))
      (should (string-match-p "listed_tool" content)))))

(provide 'test-extension)

;;; test-extension.el ends here
