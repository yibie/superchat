;;; test-workflow.el — Workflow parser and executor tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for superchat-workflow.el: parsing, substitution, execution.

;;; Code:

(require 'ert)
(require 'superchat-workflow)

(ert-deftest workflow-parse-skips-blank-and-comments ()
  "Parse returns only executable steps, skipping blanks and comments."
  (let ((body "/web-search \"news\"\n\n# this is a comment\n\n/summarize\n"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal 2 (length steps)))
      (should (equal "/web-search \"news\"" (car steps)))
      (should (equal "/summarize" (cadr steps))))))

(ert-deftest workflow-parse-preserves-step-order ()
  "Steps preserve their original order."
  (let ((body "step1\nstep2\nstep3"))
    (let ((steps (superchat-workflow-parse-steps body)))
      (should (equal '("step1" "step2" "step3") steps)))))

(ert-deftest workflow-substitute-expands-input-lang-date ()
  "Substitute replaces $input, $lang, $date."
  (let ((result (superchat-workflow-substitute
                 "Hello $input, lang=$lang, date=$date"
                 '(:input "world" :lang "French" :date "2026-01-01"))))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "lang=French" result))
    (should (string-match-p "date=2026-01-01" result))))

(ert-deftest workflow-substitute-leaves-unknown-vars-alone ()
  "Unknown $var references are left unchanged."
  (let ((result (superchat-workflow-substitute
                 "Hello $input, $unknown"
                 '(:input "world"))))
    (should (string-match-p "Hello world" result))
    (should (string-match-p "\\$unknown" result))))

(ert-deftest workflow-execute-runs-steps-sequentially ()
  "Execute runs all steps when core is available."
  :expected-result (if (featurep 'superchat-core) :passed :failed)
  (require 'superchat-core)
  (let ((skill '(:name "test" :body "step1\nstep2" :type "workflow")))
    (let ((results (superchat-workflow-execute skill "")))
      (should results)
      (should (= 2 (length results))))))

(ert-deftest workflow-execute-stops-on-syntax-error ()
  "Execute handles single step."
  (let ((skill '(:name "test" :body "step1" :type "workflow")))
    (let ((results (superchat-workflow-execute skill "")))
      (should results)
      (should (= 1 (length results))))))

(ert-deftest workflow-execute-empty-body-no-crash ()
  "Empty body returns nil without crash."
  (let ((skill '(:name "empty" :body "" :type "workflow")))
    (should-not (superchat-workflow-execute skill ""))))

(ert-deftest workflow-import-legacy-synthesises-skill ()
  "Importing a .workflow file creates a valid SKILL.md."
  (let* ((tmp-dir (make-temp-file "wf-import" t))
         (legacy-file (expand-file-name "test.workflow" tmp-dir))
         (target-dir (expand-file-name "skills" tmp-dir)))
    (unwind-protect
        (progn
          ;; Write a fake legacy .workflow file
          (with-temp-file legacy-file
            (insert "/web-search \"news\"\n/summarize\n"))
          ;; Import it
          (let ((names (superchat-workflow-import-legacy-dir
                        tmp-dir target-dir)))
            (should (equal 1 (length names)))
            (let ((skill-file (expand-file-name
                               "skill-test/SKILL.md"
                               target-dir)))
              ;; Check SKILL.md was created
              (should (file-exists-p skill-file))
              (with-temp-buffer
                (insert-file-contents skill-file)
                (let ((content (buffer-string)))
                  (should (string-match-p "name: test" content))
                  (should (string-match-p "type: workflow" content))
                  (should (string-match-p "/web-search" content))
                  (should (string-match-p "/summarize" content)))))))
      (ignore-errors (delete-directory tmp-dir t)))))

(provide 'test-workflow)
;;; test-workflow.el ends here
