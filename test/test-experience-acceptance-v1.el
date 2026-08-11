;;; test-experience-acceptance-v1.el --- Acceptance evidence checks -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load-file
 (expand-file-name
  "experiments/experience-view-internal-acceptance-v1/run.el"
  superchat-test--root))

(defun test-experience-acceptance-v1--event (id type &optional cause payload)
  "Return a small Tape-shaped event for acceptance-verifier tests."
  (append (list :event-id id :type type :run-id "consumer-run")
          (when cause (list :causation-id cause))
          (when payload (list :payload payload))))

(ert-deftest experience-acceptance-v1/positive-chain-requires-first-info-action ()
  "The verifier accepts only the frozen positive first-action chain."
  (let* ((candidate "Call workspace_info once, take the revision reported")
         (events
          (list
           (test-experience-acceptance-v1--event
            "snapshot-1" "decision.snapshot.recorded" nil
            '(:attempt 1 :prepared-envelope-json "initial request"))
           (test-experience-acceptance-v1--event
            "failure" "tool.failed" nil
            '(:tool-name "workspace_read"
              :error-type "superchat-workspace-artifact-revision-error"
              :execution-id "first"))
           (test-experience-acceptance-v1--event
            "intervention" "experience.intervention.recorded" "failure"
            '(:receipt (:candidate (:event-id "candidate"))))
           (test-experience-acceptance-v1--event
            "snapshot-2" "decision.snapshot.recorded" "intervention"
            (list :attempt 2 :prepared-envelope-json
                  (json-encode `(:prompt ,candidate))))
           (test-experience-acceptance-v1--event
            "info" "tool.requested" "intervention"
            '(:tool-name "workspace_info" :execution-id "recovery"
              :arguments-json "{\"named_arguments\":[]}"))
           (test-experience-acceptance-v1--event
            "outcome" "tool.completed" "info"
            '(:tool-name "workspace_info" :execution-id "recovery"))))
         (verdict
          (superchat-acceptance-v1-verify-cell
           (list :name "P-missing" :positive t :candidate-text candidate)
           events 2)))
    (should (plist-get verdict :passed))))

(ert-deftest experience-acceptance-v1/negative-rejects-candidate-leak ()
  "Blocked Candidate bytes in an envelope fail a negative cell."
  (let ((verdict
         (superchat-acceptance-v1-verify-cell
          '(:name "N-absent" :positive nil :candidate-text "secret Candidate")
          (list
           (test-experience-acceptance-v1--event
            "snapshot" "decision.snapshot.recorded" nil
            '(:attempt 1 :prepared-envelope-json "secret Candidate")))
          1)))
    (should-not (plist-get verdict :passed))))

;;; test-experience-acceptance-v1.el ends here
