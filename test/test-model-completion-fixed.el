;;; test-model-completion.el --- Test @ model completion functionality

;;; Code:

;; Load required dependencies in correct order
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

;; Test the completion function
(defun test-model-completion-function ()
  "Test the @ model completion CAPF function."
  (interactive)
  (message "=== Testing @ Model Completion Function ===")
  
  ;; Create a temporary buffer to test completion
  (with-temp-buffer
    (let ((superchat-buffer-name (current-buffer))
          (superchat--prompt-start (point-marker)))
      
      ;; Test 1: No @ symbol
      (insert "hello world")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (message "❌ Test 1 failed: Expected nil for input without @, got %S" result)
          (message "✅ Test 1 passed: No completion for input without @")))
      
      ;; Test 2: @ symbol with no prefix
      (erase-buffer)
      (insert "@")
      (setq superchat--prompt-start (point-min))
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((start (nth 0 result))
                  (end (nth 1 result))
                  (candidates (nth 2 result)))
              (message "✅ Test 2 passed: @ completion available")
              (message "  Start: %d, End: %d" start end)
              (message "  Candidates: %S" candidates)
              (when candidates
                (message "✅ Test 2a passed: Found %d model candidates" (length candidates))))
          (message "❌ Test 2 failed: No completion for @ symbol"))))
      
      ;; Test 3: @ symbol with partial prefix
      (erase-buffer)
      (insert "@gp")
      (setq superchat--prompt-start (point-min))
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((candidates (nth 2 result)))
              (message "✅ Test 3 passed: @gp completion available")
              (message "  Candidates: %S" candidates))
          (message "❌ Test 3 failed: No completion for @gp")))))

;; Test available models function
(defun test-available-models-for-completion ()
  "Test the available models function for completion."
  (interactive)
  (message "=== Testing Available Models for Completion ===")
  
  (let ((models (superchat--get-available-models)))
    (message "✅ Found %d models for completion: %S" (length models) models)
    
    ;; Test that models are strings
    (let ((all-strings (cl-every #'stringp models)))
      (if all-strings
          (message "✅ All model names are strings")
        (message "❌ Some model names are not strings")))
    
    ;; Test that there are no empty model names
    (let ((no-empty (cl-notany #'string-empty-p models)))
      (if no-empty
          (message "✅ No empty model names")
        (message "❌ Found empty model names")))))

;; Test command completion still works
(defun test-command-completion-still-works ()
  "Test that command completion still works after adding @ completion."
  (interactive)
  (message "=== Testing Command Completion Still Works ===")
  
  (with-temp-buffer
    (let ((superchat-buffer-name (current-buffer))
          (superchat--prompt-start (point-min)))
      
      ;; Test command completion
      (insert "/to")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((candidates (nth 2 result)))
              (message "✅ Command completion still works")
              (message "  /to candidates: %S" candidates))
          (message "❌ Command completion broken"))))))

;; Run all tests
(defun test-superchat-completion ()
  "Run all SuperChat completion tests."
  (interactive)
  (message "=== SuperChat Completion Test Suite ===")
  (test-model-completion-function)
  (test-available-models-for-completion)
  (test-command-completion-still-works)
  (message "=== Completion Testing Complete ===")
  (message "\nUsage:")
  (message "• Type @ and use TAB or M-TAB to complete model names")
  (message "• Type / and use TAB or M-TAB to complete commands")
  (message "• Works with company-mode, corfu, or standard completion"))

(provide 'test-model-completion)
;;; test-model-completion.el ends here