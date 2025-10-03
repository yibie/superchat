;;; test-final-model-detection.el --- Final test for real model detection

;;; Commentary:
;; Clean test to verify real gptel model detection works

;;; Code:

(require 'cl-lib)

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

(defun test-final-model-detection ()
  "Test real model detection with gptel."
  (interactive)
  (message "=== Final Model Detection Test ===")
  
  ;; Check if gptel is available
  (if (require 'gptel nil t)
      (message "✅ gptel loaded")
    (message "❌ gptel not available"))
  
  ;; Test our function
  (let ((models (superchat--get-available-models)))
    (message "✅ SuperChat models: %S" models)
    (message "   Model count: %d" (length models)))
  
  ;; Check backend directly
  (when (and (boundp 'gptel-backend) 
             gptel-backend
             (fboundp 'gptel-backend-models))
    (condition-case err
        (let ((backend-models (gptel-backend-models gptel-backend)))
          (message "✅ Backend models: %S" backend-models))
      (error
       (message "❌ Backend error: %s" (error-message-string err)))))
  
  (message "=== Final Test Complete ==="))

(test-final-model-detection)

;;; test-final-model-detection.el ends here