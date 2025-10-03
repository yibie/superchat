;;; test-real-gptel-models.el --- Test with actual gptel package

;;; Commentary:
;; Test real model detection by loading actual gptel package

;;; Code:

(require 'cl-lib)

;; Add gptel to load path
(add-to-list 'load-path "/Users/chenyibin/.emacs.d/straight/repos/gptel")

;; Load gptel package
(load "/Users/chenyibin/.emacs.d/straight/repos/gptel/gptel.el")

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

(defun test-real-gptel-models ()
  "Test with actual gptel package loaded."
  (interactive)
  (message "=== Testing with Real GPTel Package ===")
  
  ;; Verify gptel is loaded
  (if (featurep 'gptel)
      (message "✅ gptel package loaded successfully")
    (message "❌ gptel package not loaded"))
  
  ;; Check gptel-backend
  (if (boundp 'gptel-backend)
      (progn
        (message "✅ gptel-backend is bound")
        (message "   Backend type: %s" (type-of gptel-backend))
        (when (fboundp 'gptel-backend-name)
          (let ((backend-name (gptel-backend-name gptel-backend)))
            (message "   Backend name: %s" backend-name))))
    (message "❌ gptel-backend not bound"))
  
  ;; Check gptel--known-backends
  (if (boundp 'gptel--known-backends)
      (progn
        (message "✅ gptel--known-backends available")
        (message "   Number of backends: %d" (length gptel--known-backends))
        (dolist (backend gptel--known-backends)
          (message "   - %s" (car backend))))
    (message "❌ gptel--known-backends not available"))
  
  ;; Test gptel-backend-models function
  (if (fboundp 'gptel-backend-models)
      (message "✅ gptel-backend-models function available")
    (message "❌ gptel-backend-models function not available"))
  
  ;; Get actual models from current backend
  (when (and (boundp 'gptel-backend) 
             gptel-backend
             (fboundp 'gptel-backend-models))
    (condition-case err
        (let ((backend-models (gptel-backend-models gptel-backend)))
          (message "✅ Real backend models:")
          (message "   Count: %d" (length backend-models))
          (dotimes (i (min 10 (length backend-models)))
            (let ((model (nth i backend-models)))
              (message "   %d. %s (%s)" (1+ i) 
                       (if (symbolp model) (symbol-name model) model)
                       (if (symbolp model) "symbol" "string")))))
      (error
       (message "❌ Error getting backend models: %s" (error-message-string err)))))
  
  ;; Test our SuperChat function with real gptel
  (message "")
  (message "SuperChat model detection results:")
  (let ((superchat-models (superchat--get-available-models)))
    (message "✅ SuperChat models (%d): %S" (length superchat-models) superchat-models)
    
    ;; Show if we got real models
    (if (not (equal superchat-models '("default")))
        (progn
          (message "🎉 SUCCESS: Got real models from gptel!")
          (message "First 5 models:")
          (dotimes (i (min 5 (length superchat-models)))
            (message "  %d. %s" (1+ i) (nth i superchat-models))))
      (message "⚠️  Still getting 'default' - investigating...")))
  
  ;; Test model completion
  (message "")
  (message "Testing model completion:")
  (with-temp-buffer
    (let ((superchat--prompt-start (point-min)))
      (erase-buffer)
      (insert "@")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((candidates (nth 2 result)))
              (message "✅ @ completion candidates: %d" (length candidates))
              (when (> (length candidates) 0)
                (message "First 3 candidates: %S" (cl-subseq candidates 0 3))))
          (message "❌ @ completion failed")))))
  
  (message "")
  (message "=== Real GPTel Test Complete ==="))

(test-real-gptel-models)

;;; test-real-gptel-models.el ends here