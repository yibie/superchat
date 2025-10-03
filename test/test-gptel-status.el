;;; test-gptel-status.el --- Test gptel backend status

;;; Commentary:
;; Test to understand gptel backend configuration

;;; Code:

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

(defun test-gptel-status ()
  "Test gptel backend status and configuration."
  (interactive)
  (message "=== GPTel Status Test ===")
  
  ;; Check if gptel is available
  (if (require 'gptel nil t)
      (progn
        (message "✅ gptel package loaded")
        
        ;; Check gptel-backend variable
        (if (boundp 'gptel-backend)
            (progn
              (message "✅ gptel-backend is bound")
              (message "   Type: %s" (type-of gptel-backend))
              (message "   Value: %S" gptel-backend))
          (message "❌ gptel-backend is not bound"))
        
        ;; Check if gptel-backend is valid
        (when (and (boundp 'gptel-backend) gptel-backend)
          (if (gptel-backend-p gptel-backend)
              (message "✅ gptel-backend is valid")
            (message "❌ gptel-backend is not valid")))
        
        ;; Check gptel-backend-models function
        (if (fboundp 'gptel-backend-models)
            (message "✅ gptel-backend-models function exists")
          (message "❌ gptel-backend-models function missing"))
        
        ;; Check what gptel-backend-models returns
        (when (and (boundp 'gptel-backend) 
                   gptel-backend
                   (fboundp 'gptel-backend-models))
          (condition-case nil
              (let ((models (gptel-backend-models gptel-backend)))
                (message "✅ gptel-backend-models returned: %S" models)
                (message "   Type: %s" (type-of models))
                (message "   Length: %d" (if models (length models) 0)))
            (error
             (message "❌ Error calling gptel-backend-models: %s" (error-message-string err)))))
        
        ;; Check gptel--known-backends
        (if (boundp 'gptel--known-backends)
            (progn
              (message "✅ gptel--known-backends is bound")
              (message "   Length: %d" (if gptel--known-backends (length gptel--known-backends) 0)))
          (message "❌ gptel--known-backends is not bound"))
        
        ;; Check if we can find the OpenAI backend
        (when (boundp 'gptel--known-backends)
          (let ((openai-backend (alist-get "ChatGPT" gptel--known-backends nil nil #'equal)))
            (if openai-backend
                (progn
                  (message "✅ Found ChatGPT backend")
                  (when (fboundp 'gptel-backend-models)
                    (let ((models (gptel-backend-models openai-backend)))
                      (message "   ChatGPT models: %S" models))))
              (message "❌ ChatGPT backend not found"))))
        
        ;; Test our superchat function
        (let ((superchat-models (superchat--get-available-models)))
          (message "✅ SuperChat models: %S" superchat-models)))
    
    ;; gptel not available
    (message "❌ gptel package not available")))
  
  (message "=== GPTel Status Test Complete ==="))

(test-gptel-status)

;;; test-gptel-status.el ends here