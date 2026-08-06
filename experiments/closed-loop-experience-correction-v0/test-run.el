;;; test-run.el --- Closed-loop runner contract tests -*- lexical-binding: t; -*-

(require 'ert)

(load-file
 (expand-file-name
  "run.el"
  (file-name-directory (or load-file-name buffer-file-name))))

(ert-deftest closed-loop-run/resolves-the-frozen-task-question ()
  "A real assignment renders the question stored in its referenced task."
  (let* ((assignment
          (car (superchat-closed-loop-experience-correction-v0--assignments)))
         (expected
          "Using only the six supplied Cohort v6 Tape facts and the assigned Claim block, synthesize what the two recovery episodes do and do not establish.")
         (query
          (superchat-closed-loop-experience-correction-v0--query assignment)))
    (should
     (equal expected
            (superchat-closed-loop-experience-correction-v0--task-question
             assignment)))
    (should (string-match-p (regexp-quote (concat "Question: " expected)) query))
    (should-not (string-match-p "Question: nil" query))))

(ert-deftest closed-loop-run/rejects-an-unknown-task-before-rendering ()
  "An assignment cannot silently render a missing task as `nil'."
  (let ((failure
         (should-error
          (superchat-closed-loop-experience-correction-v0--task-question
           '(:assignment_id "broken-assignment"
             :task_id "missing-task"
             :family "evidence-synthesis"
             :arm "no-claim"))
          :type 'error)))
    (should (string-match-p "unknown task missing-task"
                            (error-message-string failure)))))

(ert-deftest closed-loop-run/rejects-conflicting-inline-question ()
  "An inline question cannot disagree with the frozen task catalog."
  (let* ((assignment
          (copy-sequence
           (car (superchat-closed-loop-experience-correction-v0--assignments))))
         (assignment (plist-put assignment :question "Wrong question"))
         (failure
          (should-error
           (superchat-closed-loop-experience-correction-v0--task-question
            assignment)
           :type 'error)))
    (should (string-match-p "question disagrees with task"
                            (error-message-string failure)))))

(ert-deftest closed-loop-run/rejects-settlement-that-tape-cannot-prove ()
  "A cached settlement cannot override the durable Workflow view."
  (cl-letf (((symbol-function 'superchat-research-workflow-view)
             (lambda (_instance-id)
               '(:status incomplete
                 :steps ((:resume-decision wait))))))
    (let ((failure
           (should-error
            (superchat-closed-loop-experience-correction-v0--settled-p
             '(:instance_id "research-broken" :settled t))
            :type 'error)))
      (should
       (string-match-p "contradicts durable Workflow view"
                       (error-message-string failure))))))

(provide 'superchat-closed-loop-experience-correction-v0-test-run)

;;; test-run.el ends here
