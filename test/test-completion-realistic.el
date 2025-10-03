;;; test-completion-realistic.el --- Realistic test for @ completion

;;; Code:

;; Load required dependencies
(load-file "../superchat-memory.el")
(load-file "../superchat.el")

;; Mock different backend scenarios
(defun test-with-different-backends ()
  "Test model completion with different backend configurations."
  (interactive)
  (message "=== Testing with Different Backend Scenarios ===")
  
  ;; Test 1: No backend configured
  (let ((gptel-backend nil))
    (let ((models (superchat--get-available-models)))
      (message "✅ No backend: %S" models)))
  
  ;; Test 2: Mock ChatGPT backend
  (cl-letf (((symbol-function 'gptel-backend-name)
             (lambda (_) "ChatGPT")))
    (let ((models (superchat--get-available-models)))
      (message "✅ ChatGPT backend: %S" models)))
  
  ;; Test 3: Mock Claude backend  
  (cl-letf (((symbol-function 'gptel-backend-name)
             (lambda (_) "Claude")))
    (let ((models (superchat--get-available-models)))
      (message "✅ Claude backend: %S" models))))

;; Test actual completion behavior
(defun test-completion-behavior ()
  "Test actual completion behavior in realistic scenarios."
  (interactive)
  (message "=== Testing Realistic Completion Behavior ===")
  
  (with-temp-buffer
    (let ((superchat--prompt-start (point-min)))
      
      ;; Test 1: @ at beginning of line
      (erase-buffer)
      (insert "@")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (message "✅ @ at beginning: completion available (%d candidates)" 
                     (length (nth 2 result)))
          (message "❌ @ at beginning: no completion")))
      
      ;; Test 2: @ with partial text
      (erase-buffer) 
      (insert "help me @gpt")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((start (nth 0 result))
                  (candidates (nth 2 result)))
              (message "✅ @ with partial: starts at %d, %d candidates" 
                       start (length candidates)))
          (message "❌ @ with partial: no completion")))
      
      ;; Test 3: Multiple @ symbols (should complete last one)
      (erase-buffer)
      (insert "email@example.com @claude")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (let ((start (nth 0 result)))
              (message "✅ Multiple @: completes at position %d (should be after space)" start)
              (if (> start 16)  ; After "email@example.com "
                  (message "✅ Multiple @: correct completion position")
                (message "❌ Multiple @: wrong completion position")))
          (message "❌ Multiple @: no completion")))
      
      ;; Test 4: No completion for regular text
      (erase-buffer)
      (insert "hello world")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if (null result)
            (message "✅ Regular text: no completion (correct)")
          (message "❌ Regular text: unexpected completion"))))))

;; Test edge cases
(defun test-edge-cases ()
  "Test edge cases for completion."
  (interactive)
  (message "=== Testing Edge Cases ===")
  
  (with-temp-buffer
    (let ((superchat--prompt-start (point-min)))
      
      ;; Test 1: Empty input
      (erase-buffer)
      (let ((result (superchat--completion-at-point)))
        (if (null result)
            (message "✅ Empty input: no completion")
          (message "❌ Empty input: unexpected completion")))
      
      ;; Test 2: Just @ symbol
      (erase-buffer)
      (insert "@")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (message "✅ Just @: completion available")
          (message "❌ Just @: no completion")))
      
      ;; Test 3: @ followed by space
      (erase-buffer)
      (insert "@ ")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if (null result)
            (message "✅ @ space: no completion (correct)")
          (message "❌ @ space: unexpected completion")))
      
      ;; Test 4: @ at end of sentence
      (erase-buffer)
      (insert "What do you think @")
      (goto-char (point-max))
      (let ((result (superchat--completion-at-point)))
        (if result
            (message "✅ @ at sentence end: completion available")
          (message "❌ @ at sentence end: no completion"))))))

;; Run comprehensive test
(defun test-completion-comprehensive ()
  "Run comprehensive completion tests."
  (interactive)
  (message "=== Comprehensive SuperChat Completion Test ===")
  (test-with-different-backends)
  (test-completion-behavior) 
  (test-edge-cases)
  (message "=== Comprehensive Testing Complete ===")
  (message "\nTo test interactively:")
  (message "1. M-x superchat")
  (message "2. Type @ and press TAB")
  (message "3. Type / and press TAB")
  (message "4. Verify both completion systems work"))

(provide 'test-completion-realistic)
;;; test-completion-realistic.el ends here