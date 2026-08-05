;;; run-batch-v3.el --- Continue Cohort v0 after async identity repair -*- lexical-binding: t; -*-

;;; Commentary:

;; Preserves the v2 launch record, loads the repaired Subagent context, and
;; writes cumulative v3 launch evidence without reopening held-out tasks.

;;; Code:

(load "/Users/chenyibin/Documents/emacs/package/superchat/experiments/experience-cohort-v0/run-batch.el"
      nil nil t)

(setq superchat-experience-cohort-v0--batch-manifest-path
      (expand-file-name "manifest-v3.json"
                        superchat-experience-cohort-v0--experiment-directory))

(defconst superchat-experience-cohort-v0--v2-launch-path
  (expand-file-name "batch-launch-record-v2.json"
                    superchat-experience-cohort-v0--experiment-directory))

(when (null superchat-experience-cohort-v0-batch-launches)
  (setq superchat-experience-cohort-v0-batch-launches
        (reverse
         (plist-get
          (superchat-experience-cohort-v0--read-json
           superchat-experience-cohort-v0--v2-launch-path)
          :launches))))

(defun superchat-experience-cohort-v0--verify-v3-inputs ()
  "Fail unless the v3 repair and launch inputs remain frozen."
  (let* ((manifest
          (superchat-experience-cohort-v0--read-json
           superchat-experience-cohort-v0--batch-manifest-path))
         (repair (plist-get manifest :closure_repair))
         (execution (plist-get manifest :execution)))
    (unless (equal "batch-authorized" (plist-get manifest :status))
      (error "Cohort v0 v3 manifest is not authorized"))
    (dolist (pair
             (list
              (cons (plist-get repair :source_path)
                    (plist-get repair :source_sha256))
              (cons (plist-get repair :test_path)
                    (plist-get repair :test_sha256))
              (cons (plist-get execution :runner_path)
                    (plist-get execution :runner_sha256))))
      (let ((path (expand-file-name
                   (car pair)
                   "/Users/chenyibin/Documents/emacs/package/superchat/")))
        (unless (equal (cdr pair)
                       (superchat-experience-cohort-v0--sha256-file path))
          (error "v3 frozen input changed: %s" path))))))

(defun superchat-experience-cohort-v0--write-batch-launches ()
  "Persist cumulative v2/v3 launch identities without rewriting v2 evidence."
  (let* ((path (expand-file-name
                "batch-launch-record-v3.json"
                superchat-experience-cohort-v0--experiment-directory))
         (launches
          (mapcar
           (lambda (launch)
             (plist-put (copy-sequence launch) :manifest_revision
                        (if (= 2 (plist-get launch :run_index)) "v2" "v3")))
           (reverse superchat-experience-cohort-v0-batch-launches))))
    (with-temp-file path
      (insert
       (json-encode
        (list :experiment_id "experience-cohort-v0-20260803"
              :manifest_revision "v3"
              :manifest_sha256
              (superchat-experience-cohort-v0--sha256-file
               superchat-experience-cohort-v0--batch-manifest-path)
              :status "runs.started"
              :prior_launch_record "batch-launch-record-v2.json"
              :prior_launch_record_sha256
              (superchat-experience-cohort-v0--sha256-file
               superchat-experience-cohort-v0--v2-launch-path)
              :new_launch_count (length launches)
              :total_workflow_starts_including_v1 (1+ (length launches))
              :target_total_workflow_starts 20
              :launches (vconcat launches)
              :effectiveness_claim "not_evaluated"
              :learning_claim "prohibited")))
      (insert "\n"))))

(superchat-experience-cohort-v0--verify-v3-inputs)

(provide 'superchat-experience-cohort-v0-batch-v3)

;;; run-batch-v3.el ends here
