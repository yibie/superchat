;;; test-simple-model-detection.el --- Simple test for real model detection

;;; Commentary:
;; Simple test to verify real model detection works

;;; Code:

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

(defun test-simple-model-detection ()
  "Simple test of real model detection."
  (interactive)
  (message "=== Simple Model Detection Test ===")
  
  ;; Test basic model detection
  (condition-case nil
      (let ((models (superchat--get-available-models)))
        (message "✅ Got %d models: %S" (length models) models))
    (error
     (message "❌ Error: %s" (error-message-string err))))
  
  (message "=== Simple Test Complete ==="))

(test-simple-model-detection)

;;; test-simple-model-detection.el ends here