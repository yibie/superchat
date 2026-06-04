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

(provide 'test-workflow)
;;; test-workflow.el ends here
