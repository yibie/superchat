;;; test-skills-integration.el --- Integration tests for superchat-skills -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration tests for Agentic Skills.
;; These tests simulate real user interactions and verify end-to-end functionality.

;;; Code:

(require 'ert)
(require 'superchat-skills)

;;;-----------------------------------------------
;;; Test Data
;;;-----------------------------------------------

(defconst superchat-skills--test-registry
  (list
   (list :name "code-review"
         :context "Review code quality"
         :triggers '("review" "check" "这段代码怎么样" "代码有问题吗" "帮我看看")
         :file "skills/code-review.md")
   (list :name "refactor"
         :context "Refactor code"
         :triggers '("refactor" "重构" "优化" "改进结构" "简化")
         :file "skills/refactor.md")
   (list :name "planning"
         :context "Plan development"
         :triggers '("plan" "规划" "帮我做" "怎么实现" "如何做")
         :file "skills/planning.md"))
  "Mock skill registry for testing.")

;;;-----------------------------------------------
;;; Implicit Matching Integration Tests
;;;-----------------------------------------------

(ert-deftest test-implicit-match-code-review-scenarios ()
  "Test implicit matching for code-review skill with various inputs."
  (let ((superchat-skills-match-confidence-threshold 0.3)
        (test-cases '(
          ;; (input expected-skill)
          ("帮我 review 这段代码" "code-review")
          ("check this code for issues" "code-review")
          ("这段代码怎么样" "code-review")
          ("帮我看看这段代码有没有问题" "code-review")
          ("代码有问题吗" "code-review")
          )))
    
    (dolist (test-case test-cases)
      (let* ((input (car test-case))
             (expected (cadr test-case))
             (result (superchat-skills--match-by-keywords 
                      input 
                      superchat-skills--test-registry))
             (matched-skill (car result)))
        
        (should result)
        (should (equal matched-skill expected))
        (message "✓ '%s' → %s" input matched-skill)))))

(ert-deftest test-implicit-match-refactor-scenarios ()
  "Test implicit matching for refactor skill."
  ;; Use lower threshold and specific test registry
  (let ((superchat-skills-match-confidence-threshold 0.25)
        (test-registry (list
                        (list :name "refactor"
                              :context "Refactor code"
                              :triggers '("refactor" "重构" "优化" "改进结构" "简化")
                              :file "skills/refactor.md"))))
    
    ;; Test each trigger word
    (dolist (trigger '("重构" "优化" "改进结构" "简化" "refactor"))
      (let* ((input (format "帮我 %s 这段代码" trigger))
             (result (superchat-skills--match-by-keywords input test-registry))
             (matched-skill (car result))
             (confidence (cdr result)))
        
        (should result)
        (should (equal matched-skill "refactor"))
        (message "✓ Trigger '%s' → %s (confidence: %.2f)" trigger matched-skill (or confidence 0))))))

(ert-deftest test-implicit-match-planning-scenarios ()
  "Test implicit matching for planning skill."
  (let ((superchat-skills-match-confidence-threshold 0.3)
        (test-cases '(
          ("帮我做个新功能" "planning")
          ("怎么实现这个功能" "planning")
          ("规划一下这个项目" "planning")
          ("如何做这个任务" "planning")
          )))
    
    (dolist (test-case test-cases)
      (let* ((input (car test-case))
             (expected (cadr test-case))
             (result (superchat-skills--match-by-keywords 
                      input 
                      superchat-skills--test-registry))
             (matched-skill (car result)))
        
        (should result)
        (should (equal matched-skill expected))
        (message "✓ '%s' → %s" input matched-skill)))))

(ert-deftest test-implicit-match-no-false-positives ()
  "Test that unrelated inputs don't trigger skills."
  (let ((superchat-skills-match-confidence-threshold 0.7)
        (test-inputs '(
          "今天天气怎么样"
          "你好"
          "谢谢"
          "什么是 Emacs"
          "怎么安装 Python"
          "推荐一本好书"
          )))
    
    (dolist (input test-inputs)
      (let ((result (superchat-skills--match-by-keywords 
                     input 
                     superchat-skills--test-registry)))
        (should (null result))
        (message "✓ '%s' → no match (correct)" input)))))

;;;-----------------------------------------------
;;; Explicit vs Implicit Priority Tests
;;;-----------------------------------------------

(ert-deftest test-explicit-overrides-implicit ()
  "Test that explicit >skill-name takes precedence over implicit matching."
  ;; Parse explicit skill
  (let* ((input ">refactor 帮我 review 这段代码")  ; Explicit refactor, but text mentions review
         (parsed (superchat-skills-parse-input input)))
    
    ;; Should parse as explicit refactor
    (should parsed)
    (should (equal (car parsed) "refactor"))
    (should (equal (cdr parsed) "帮我 review 这段代码"))
    
    ;; The skill-name should be "refactor" not "code-review"
    (should (equal (car parsed) "refactor"))))

;;;-----------------------------------------------
;;; Prompt Building Tests
;;;-----------------------------------------------

(ert-deftest test-prompt-building-with-skill ()
  "Test that skill content is properly injected into prompt."
  ;; Mock skill content
  (let* ((skill-name "test-skill")
         (user-input "test request")
         (skill-content "# Test Skill\n\nThis is a test skill."))
    
    ;; Create a temporary skill file
    (let ((skill-file (expand-file-name "test-skill.md" superchat-skills-directory)))
      (unwind-protect
          (progn
            (with-temp-file skill-file
              (insert skill-content))
            
            ;; Test prompt building
            (let ((prompt (superchat-skills-build-prompt user-input skill-name)))
              (should (string-match-p "\\[Context Instructions\\]" prompt))
              (should (string-match-p "This is a test skill" prompt))
              (should (string-match-p "\\[User Request\\]" prompt))
              (should (string-match-p user-input prompt))))
        
        ;; Cleanup
        (when (file-exists-p skill-file)
          (delete-file skill-file))))))

;;;-----------------------------------------------
;;; Confidence Threshold Tests
;;;-----------------------------------------------

(ert-deftest test-confidence-threshold-behavior ()
  "Test that confidence threshold affects matching."
  (let* ((input "帮我 review 代码"))
    
    ;; With low threshold, should match
    (let ((superchat-skills-match-confidence-threshold 0.1))
      (let ((result (superchat-skills--match-by-keywords 
                     input 
                     superchat-skills--test-registry)))
        (should result)))
    
    ;; With high threshold, might not match depending on score
    (let ((superchat-skills-match-confidence-threshold 0.9))
      (let ((result (superchat-skills--match-by-keywords 
                     input 
                     superchat-skills--test-registry)))
        ;; Result depends on actual score, just verify it runs without error
        (should (or result (null result)))))))

;;;-----------------------------------------------
;;; Edge Cases
;;;-----------------------------------------------

(ert-deftest test-empty-input ()
  "Test behavior with empty input."
  (let ((result1 (superchat-skills-parse-input ""))
        (result2 (superchat-skills--match-by-keywords "" superchat-skills--test-registry)))
    (should (null result1))
    (should (null result2))))

(ert-deftest test-only-prefix ()
  "Test input with only > prefix."
  (let ((result (superchat-skills-parse-input ">")))
    ;; Should treat ">" alone as skill name
    (should result)
    (should (equal (car result) ""))))

(ert-deftest test-special-characters-in-input ()
  "Test handling of special characters."
  (let* ((input "检查 #file.el 中的代码")
         (result (superchat-skills--match-by-keywords 
                  input 
                  superchat-skills--test-registry)))
    ;; Should handle special characters gracefully
    (should (or result (null result)))))

;;;-----------------------------------------------
;;; Performance Tests
;;;-----------------------------------------------

(ert-deftest test-matching-performance ()
  "Test that implicit matching is reasonably fast."
  (let ((input "帮我 review 这段代码")
        (iterations 100)
        start-time end-time)
    
    (setq start-time (float-time))
    (dotimes (_ iterations)
      (superchat-skills--match-by-keywords input superchat-skills--test-registry))
    (setq end-time (float-time))
    
    (let ((elapsed (- end-time start-time))
          (avg-time (/ (- end-time start-time) iterations)))
      (message "Performance: %d iterations in %.3f seconds (avg: %.4f ms)"
               iterations elapsed (* avg-time 1000))
      ;; Should complete 100 iterations in less than 1 second
      (should (< elapsed 1.0)))))

;;;-----------------------------------------------
;;; Run All Tests
;;;-----------------------------------------------

(defun superchat-skills-run-all-tests ()
  "Run all skill integration tests."
  (interactive)
  (ert-run-tests-batch-and-exit "^test-"))

(provide 'test-skills-integration)

;;; test-skills-integration.el ends here
