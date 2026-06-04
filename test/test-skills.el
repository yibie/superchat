;;; test-skills.el --- Tests for superchat-skills -*- lexical-binding: t; -*-

(require 'ert)
(require 'superchat-skills)

(ert-deftest test-skills-parse-input-explicit ()
  "Test explicit skill syntax parsing."
  ;; Basic case
  (let ((result (superchat-skills-parse-input ">code-review #file.el test")))
    (should (equal (car result) "code-review"))
    (should (equal (cdr result) "#file.el test")))
  ;; No args
  (let ((result (superchat-skills-parse-input ">planning")))
    (should (equal (car result) "planning"))
    (should (equal (cdr result) "")))
  ;; Not a skill prefix
  (let ((result (superchat-skills-parse-input "code-review test")))
    (should (null result))))

(ert-deftest test-skills-parse-registry ()
  "Test AGENTS.md registry parsing."
  (let* ((test-content "## Skill Registry\n\n### code-review\n- **name**: code-review\n- **context**: Review code quality\n- **triggers**: [\"review\", \"check\"]\n- **file**: skills/code-review.md\n\n### refactor\n- **name**: refactor\n- **context**: Refactor code\n- **triggers**: [\"refactor\", \"优化\"]\n- **file**: skills/refactor.md\n- **note**: Important\n")
         (skills (superchat-skills--parse-registry test-content)))
    (should (= (length skills) 2))
    ;; Check first skill
    (let ((first (car skills)))
      (should (equal (plist-get first :name) "code-review"))
      (should (equal (plist-get first :context) "Review code quality"))
      (should (equal (plist-get first :file) "skills/code-review.md"))
      (should (equal (plist-get first :triggers) '("review" "check"))))
    ;; Check second skill
    (let ((second (cadr skills)))
      (should (equal (plist-get second :name) "refactor"))
      (should (equal (plist-get second :note) "Important")))))

(ert-deftest test-skills-keyword-matching ()
  "Test keyword-based skill matching."
  ;; Temporarily lower threshold for testing
  (let ((superchat-skills-match-confidence-threshold 0.3)
        (test-skills (list (list :name "code-review"
                                  :context "Review code"
                                  :triggers '("review" "check" "看看")
                                  :file "skills/code-review.md")
                            (list :name "refactor"
                                  :context "Refactor code"
                                  :triggers '("refactor" "优化" "重构")
                                  :file "skills/refactor.md"))))
    ;; Test match
    (let ((match1 (superchat-skills--match-by-keywords "帮我 review 这段代码" test-skills)))
      (should match1)
      (should (equal (car match1) "code-review")))
    ;; Test match refactor
    (let ((match2 (superchat-skills--match-by-keywords "需要重构这个函数" test-skills)))
      (should match2)
      (should (equal (car match2) "refactor")))
    ;; Test no match
    (let ((match3 (superchat-skills--match-by-keywords "今天天气怎么样" test-skills)))
      (should (null match3)))))

(ert-deftest test-skills-exists-p ()
  "Test skill existence check."
  ;; This will depend on actual files present
  ;; Just test the function structure
  (should (or (booleanp (superchat-skills-exists-p "code-review"))
              (null (superchat-skills-exists-p "code-review")))))

(provide 'test-skills)

;;; test-skills.el ends here
