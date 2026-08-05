;;; test-extension.el --- Contract tests for declarative extensions -*- lexical-binding: t; -*-

;;; Commentary:

;; `superchat-define-extension' is the third-party contract for
;; activation-scoped contributions.  These tests pin the promises the
;; plugin guide makes: inactive extensions cost nothing, explicit tool
;; lists are never widened, composition is idempotent and audited.

;;; Code:

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
