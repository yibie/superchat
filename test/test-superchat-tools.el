;;; test-superchat-tools.el --- Test SuperChat gptel tools integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Test file for SuperChat gptel tools integration

;;; Code:

(require 'superchat-test-helper
         (expand-file-name "test-helper" (file-name-directory load-file-name)))
(require 'ert)
(require 'cl-lib)
(require 'superchat)

(ert-deftest superchat-tools/trusted-ask-schema-strings-detached ()
  "Returned schema strings cannot mutate the contract or another descriptor."
  (skip-unless (fboundp 'llm-tool-args))
  (dolist (field '(:name :description))
    ;; Use an independently allocated baseline so a RED cannot poison later
    ;; cases in this process.  Native regressions use the original constant.
    (let ((superchat-tools--ask-user-args
           (list (list :name (copy-sequence "question") :type 'string
                       :description
                       (copy-sequence "One nonempty question requiring a text answer"))))
          (operation (lambda (&rest _) nil)))
      (cl-letf (((symbol-function 'superchat-runtime--trusted-input-operation-p)
                 (lambda (&rest _) t)))
        (let* ((first (superchat-tools--make-ask-user operation))
               (second (superchat-tools--make-ask-user operation))
               (canonical (plist-get (car superchat-tools--ask-user-args) field))
               (expected (copy-sequence canonical))
               (returned (plist-get (car (llm-tool-args first)) field)))
          (should (eq (llm-tool-function first) operation))
          (should (eq (llm-tool-function second) operation))
          (should (superchat-tools--trusted-ask-descriptor-p first operation))
          (put-text-property 0 1 'trusted-ask-mutation t returned)
          (should-not (get-text-property 0 'trusted-ask-mutation canonical))
          (aset returned 0 ?X)
          (should (equal canonical expected))
          (should (equal (plist-get (car (llm-tool-args second)) field) expected))
          (should-not (superchat-tools--trusted-ask-descriptor-p first operation))
          (should (superchat-tools--trusted-ask-descriptor-p second operation)))))))

(ert-deftest superchat-tools/trusted-ask-schema-copies-without-text-properties ()
  "Schema copy strips properties without changing the canonical strings."
  (skip-unless (fboundp 'llm-tool-args))
  (let ((superchat-tools--ask-user-args
         (list (list :name (propertize "question" 'schema-source t)
                     :type 'string
                     :description (propertize "Question" 'schema-source t)))))
    (cl-letf (((symbol-function 'superchat-runtime--trusted-input-operation-p)
               (lambda (&rest _) t)))
      (let ((tool (superchat-tools--make-ask-user #'ignore)))
        (dolist (field '(:name :description))
          (should-not (text-properties-at
                       0 (plist-get (car (llm-tool-args tool)) field)))
          (should (get-text-property
                   0 'schema-source
                   (plist-get (car superchat-tools--ask-user-args) field))))))))

(ert-deftest superchat-tools/trusted-ask-never-global-or-spoof-profile ()
  "A stale/injected registry entry cannot advertise the reserved operation."
  (skip-unless (fboundp 'llm-tool-name))
  (let ((superchat-llm-tool-names 'all)
        (superchat-llm-tools-list nil)
        (superchat-tools--external-registry
         '(("ask_user" :function ignore :description "spoof" :args nil :async t))))
    (should-not (cl-find "ask_user" (superchat-get-llm-tools)
                         :key #'llm-tool-name :test #'equal))
    (should-error (superchat-get-llm-tools '("ask_user")))
    (should-error (superchat-get-llm-tools '("ask_user") #'ignore))
    (should-error (superchat-register-tool "ask_user" :function #'ignore
                                          :description "spoof" :async t
                                          :effects '((:effect human-interaction))))))

(ert-deftest superchat-tools/builtin-name-list-matches-reload ()
  "The reserved built-in name list matches the reload implementation."
  (let ((superchat-llm-tool-names 'all)
        (superchat-llm-tools-list nil)
        (superchat-tools--external-registry nil))
    (cl-letf (((symbol-function 'llm-make-tool)
               (lambda (&rest args)
                 (list :name (plist-get args :name))))
              ((symbol-function 'superchat--subagent-registry-summary)
               (lambda () "test sub-agent summary")))
      (let ((actual
             (sort
              (mapcar (lambda (tool) (plist-get tool :name))
                      (superchat-llm-tools-reload))
              #'string<))
            (expected (sort (copy-sequence
                             superchat-tools--builtin-tool-names)
                            #'string<)))
        (should (equal expected actual))))))

;; Mock gptel tools for testing
(defvar test-superchat--mock-tools
  (list (list :name "test_tool" 
              :description "A test tool"
              :function #'ignore))
  "Mock tools for testing.")

;; Mock gptel variables
(defvar test-superchat--gptel-use-tools t
  "Mock gptel-use-tools for testing.")

(defun test-superchat-tools-status ()
  "Test the tools status function."
  (interactive)
  ;; Setup mock environment
  (setq gptel-use-tools test-superchat--gptel-use-tools)
  (setq gptel-tools test-superchat--mock-tools)
  
  ;; Test the function
  (superchat-tools-status))

(defun test-superchat-get-gptel-tools ()
  "Test getting gptel tools."
  (interactive)
  ;; Setup mock environment
  (setq gptel-tools test-superchat--mock-tools)
  
  ;; Test the function
  (let ((tools (superchat-get-gptel-tools)))
    (message "Found %d tools: %s" (length tools) (mapcar (lambda (tool) (plist-get tool :name)) tools))))

(defun test-superchat-tools-enabled-p ()
  "Test checking if tools are enabled."
  (interactive)
  ;; Setup mock environment
  (setq gptel-use-tools test-superchat--gptel-use-tools)
  
  ;; Test the function
  (let ((enabled (superchat-gptel-tools-enabled-p)))
    (message "Tools enabled: %s" enabled)))

(provide 'test-superchat-tools)
;;; test-superchat-tools.el ends here
