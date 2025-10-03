;;; test-integration.el --- Integration test for SuperChat gptel tools

;;; Commentary:
;; This test verifies that SuperChat can start up and handle gptel tools configuration

;;; Code:

(require 'superchat)

;; Test 1: Verify SuperChat starts up
(defun test-superchat-startup ()
  "Test that SuperChat can start up successfully."
  (interactive)
  (condition-case err
      (progn
        (superchat)
        (message "✅ SuperChat startup test: PASSED"))
    (error
     (message "❌ SuperChat startup test: FAILED - %s" (error-message-string err)))))

;; Test 2: Verify tools functions exist
(defun test-tools-functions-exist ()
  "Test that tools integration functions are defined."
  (interactive)
  (let ((functions '(superchat-get-gptel-tools
                      superchat-gptel-tools-enabled-p
                      superchat-tools-status)))
    (dolist (func functions)
      (if (fboundp func)
          (message "✅ Function %s: EXISTS" func)
        (message "❌ Function %s: MISSING" func)))))

;; Test 3: Verify /tools command is registered
(defun test-tools-command-registered ()
  "Test that /tools command is registered in builtin commands."
  (interactive)
  (superchat--load-user-commands)
  (let ((tools-cmd (assoc "/tools" superchat--builtin-commands)))
    (if tools-cmd
        (message "✅ /tools command: REGISTERED")
      (message "❌ /tools command: NOT REGISTERED"))))

;; Run all tests
(defun test-superchat-tools-integration ()
  "Run all SuperChat tools integration tests."
  (interactive)
  (message "=== SuperChat gptel Tools Integration Test ===")
  (test-tools-functions-exist)
  (test-tools-command-registered)
  (message "=== Integration test completed ==="))

(provide 'test-integration)
;;; test-integration.el ends here