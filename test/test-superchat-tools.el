;;; test-superchat-tools.el --- Test SuperChat gptel tools integration

;;; Commentary:
;; Test file for SuperChat gptel tools integration

;;; Code:

(require 'superchat)

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