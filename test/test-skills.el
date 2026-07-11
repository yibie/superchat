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

(ert-deftest test-preset-from-frontmatter ()
  "Test parsing a SKILL.md frontmatter into a preset."
  (let ((content "---\nname: coder\ndescription: Autonomous coding agent\nversion: \"1.1\"\ntype: agent\ntools: [read-file, search-text, shell-command]\nmodel: claude-sonnet-4\n---\n\nYou are a coding assistant.\n")
        (preset (superchat-preset-from-frontmatter "---\nname: coder\ndescription: Autonomous coding agent\nversion: \"1.1\"\ntype: agent\ntools: [read-file, search-text, shell-command]\nmodel: claude-sonnet-4\n---\n\nYou are a coding assistant.\n")))
    (should preset)
    (should (string= (superchat-preset-name preset) "coder"))
    (should (eq (superchat-preset-type preset) 'agent))
    (should (equal (superchat-preset-tools preset)
                   '("read-file" "search-text" "shell-command")))
    (should (string= (superchat-preset-model preset) "claude-sonnet-4"))
    (should (string= (superchat-preset-version preset) "1.1"))
    (should (string-match-p "coding assistant" (superchat-preset-skill-body preset)))))

(ert-deftest test-preset-apply-to-turn ()
  "Test applying a preset to a superchat turn."
  (let ((preset (superchat-preset-from-plist
                 (list :name "coder"
                       :type 'agent
                       :tools '("read-file" "write-file")
                       :model "claude-sonnet-4")))
        (turn (superchat-turn-new ">coder write a function")))
    (superchat-preset-apply preset turn)
    (should (eq (superchat-preset-type (superchat-turn-preset turn)) 'agent))
    (should (string= (superchat-turn-target-model turn) "claude-sonnet-4"))
    (should (equal (superchat-turn-tools turn) '("read-file" "write-file")))))

(ert-deftest test-skill-load-returns-preset ()
  "Test that `superchat-skills-load' returns a preset struct."
  (let ((tmp-dir (make-temp-file "superchat-skills-test" t))
        (superchat-skills-directory nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "test-agent.md" tmp-dir)
            (insert "---\nname: test-agent\ndescription: Test agent\ntype: agent\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((preset (superchat-skills-load "test-agent")))
            (should (superchat-preset-p preset))
            (should (eq (superchat-preset-type preset) 'agent))
            (should (equal (superchat-preset-tools preset) '("read-file")))
            (should (string= (superchat-preset-skill-body preset) "Body.\n"))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-agent-activates-preset ()
  "Test that /agent activates a preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "coder.md" tmp-dir)
            (insert "---\nname: coder\ndescription: Coder agent\ntype: agent\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-agent "agent" "coder" nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'agent))
            (should (string= (superchat-preset-name superchat--active-preset) "coder"))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-plan-activates-preset ()
  "Test that /plan activates a planning preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "planning.md" tmp-dir)
            (insert "---\nname: planning\ndescription: Planning skill\ntype: plan\ntools: [read-file]\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-plan "plan" nil nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'plan))
            (should (member "read-file" (superchat-preset-tools superchat--active-preset)))))
      (delete-directory tmp-dir t))))

(ert-deftest test-cmd-skill-activates-preset ()
  "Test that /skill activates any preset."
  (let ((tmp-dir (make-temp-file "superchat-cmd-test" t))
        (superchat-skills-directory nil)
        (superchat--active-preset nil))
    (unwind-protect
        (progn
          (setq superchat-skills-directory tmp-dir)
          (with-temp-file (expand-file-name "review.md" tmp-dir)
            (insert "---\nname: review\ndescription: Review skill\ntype: prompt\n---\n\nBody.\n"))
          (let ((result (superchat--cmd-skill "skill" "review" nil nil nil)))
            (should (eq (plist-get result :type) :echo))
            (should (superchat-preset-p superchat--active-preset))
            (should (eq (superchat-preset-type superchat--active-preset) 'prompt))
            (should (string= (superchat-preset-name superchat--active-preset) "review"))))
      (delete-directory tmp-dir t))))

(provide 'test-skills)

;;; test-skills.el ends here
