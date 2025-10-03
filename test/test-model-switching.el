;;; test-model-switching.el --- Test SuperChat @ model switching functionality

;;; Commentary:
;; This test file verifies the @ model switching feature works correctly.

;;; Code:

;; Load required dependencies in correct order
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

;; Test 1: Model parsing function
(defun test-model-parsing ()
  "Test the @ model syntax parsing."
  (interactive)
  (message "=== Testing @ Model Syntax Parsing ===")
  
  (let ((test-cases '(
    ("@gpt-4o Hello world" . ("Hello world" . "gpt-4o"))
    ("@claude-3-5-sonnet How are you?" . ("How are you?" . "claude-3-5-sonnet"))
    ("No model syntax here" . nil)
    ("@gemini-pro Multiple words here" . ("Multiple words here" . "gemini-pro"))
    ("@model123 with-dashes and_underscores" . ("with-dashes and_underscores" . "model123"))
    ("Trailing spaces @gpt-4 " . ("Trailing spaces" . "gpt-4"))
    )))
    
    (dolist (test-case test-cases)
      (let* ((input (car test-case))
             (expected (cdr test-case))
             (result (superchat--parse-model-switch input)))
        (if (equal result expected)
            (message "✅ \"%s\" → %S" input result)
          (message "❌ \"%s\" → expected %S, got %S" input expected result))))))

;; Test 2: Available models function
(defun test-available-models ()
  "Test the available models function."
  (interactive)
  (message "=== Testing Available Models ===")
  
  (let ((models (superchat--get-available-models)))
    (message "✅ Found %d available models: %S" (length models) models)
    (when (member "default" models)
      (message "✅ Default model included"))
    (when (> (length models) 0)
      (message "✅ Models list is not empty"))))

;; Test 3: Model list function
(defun test-model-list ()
  "Test the model list display function."
  (interactive)
  (message "=== Testing Model List Function ===")
  (condition-case err
      (progn
        (superchat-model-list)
        (message "✅ Model list function executed successfully"))
    (error
     (message "❌ Model list function failed: %s" (error-message-string err)))))

;; Test 4: Integration test with mock scenarios
(defun test-model-switching-integration ()
  "Test complete model switching integration."
  (interactive)
  (message "=== Testing Model Switching Integration ===")
  
  ;; Test parsing and execution flow (without actually calling LLM)
  (let* ((input "@gpt-4o test message")
         (parsed (superchat--parse-model-switch input))
         (clean-input (when parsed (car parsed)))
         (target-model (when parsed (cdr parsed))))
    
    (if parsed
        (progn
          (message "✅ Parsed input: \"%s\"" input)
          (message "✅ Clean input: \"%s\"" clean-input)
          (message "✅ Target model: \"%s\"" target-model)
          
          ;; Test that the model is in available list
          (let ((available-models (superchat--get-available-models)))
            (if (member target-model available-models)
                (message "✅ Target model \"%s\" is in available models list" target-model)
              (message "⚠️  Target model \"%s\" not in available models list: %S" 
                       target-model available-models))))
      (message "❌ Failed to parse @ model syntax from: \"%s\"" input))))

;; Run all tests
(defun test-superchat-model-switching ()
  "Run all SuperChat model switching tests."
  (interactive)
  (message "=== SuperChat @ Model Switching Test Suite ===")
  (test-model-parsing)
  (test-available-models)
  (test-model-list)
  (test-model-switching-integration)
  (message "=== Model Switching Testing Complete ===")
  (message "\nUsage Summary:")
  (message "• Use @model_name syntax to switch models for a single query")
  (message "• Use /models command to see available models")
  (message "• Example: @gpt-4o What is Emacs?"))

(provide 'test-model-switching)
;;; test-model-switching.el ends here