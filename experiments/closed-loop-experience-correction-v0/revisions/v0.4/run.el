;;; run.el --- Closed-loop correction manifest revision v0.4 -*- lexical-binding: t; -*-

;;; Commentary:

;; Thin revision wrapper over the remediated v0 experiment implementation.
;; Loading this file configures isolated inputs and outputs but starts no model
;; calls.  `superchat-closed-loop-experience-correction-v04-preflight' is the
;; mandatory model-free gate for every live entry point.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst superchat-closed-loop-experience-correction-v04--directory
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst superchat-closed-loop-experience-correction-v04--parent-directory
  (file-name-as-directory
   (expand-file-name "../.."
                     superchat-closed-loop-experience-correction-v04--directory)))

(defconst superchat-closed-loop-experience-correction-v04--root
  "/private/tmp/superchat-closed-loop-experience-correction-v0.4/")

(load (expand-file-name "run.el"
                        superchat-closed-loop-experience-correction-v04--parent-directory)
      nil nil t)
(load (expand-file-name "evidence.el"
                        superchat-closed-loop-experience-correction-v04--parent-directory)
      nil nil t)

(defun superchat-closed-loop-experience-correction-v04--configure ()
  "Select revision v0.4 inputs and isolated outputs."
  (superchat-db-close)
  (setq superchat-closed-loop-experience-correction-v0--directory
        superchat-closed-loop-experience-correction-v04--directory
        superchat-closed-loop-experience-correction-v0--root
        superchat-closed-loop-experience-correction-v04--root
        superchat-closed-loop-experience-correction-v0--runtime-directory
        (expand-file-name "runtime/"
                          superchat-closed-loop-experience-correction-v04--root)
        superchat-closed-loop-experience-correction-v0--working-db
        (expand-file-name
         "superchat.db"
         superchat-closed-loop-experience-correction-v0--runtime-directory)
        superchat-closed-loop-experience-correction-v0--manifest-path
        (expand-file-name
         "manifest.json"
         superchat-closed-loop-experience-correction-v04--directory)
        superchat-closed-loop-experience-correction-v0--assignments-path
        (expand-file-name
         "assignments.json"
         superchat-closed-loop-experience-correction-v04--directory)
        superchat-closed-loop-experience-correction-v0--correction-rubric-path
        (expand-file-name
         "correction-fidelity-rubric-v0.json"
         superchat-closed-loop-experience-correction-v04--directory)
        superchat-closed-loop-experience-correction-v0--claim-record-path
        (expand-file-name
         "claim-correction-record.json"
         superchat-closed-loop-experience-correction-v04--directory)
        superchat-closed-loop-experience-correction-v0--launch-record-path
        (expand-file-name
         "launch-record.json"
         superchat-closed-loop-experience-correction-v04--root)
        superchat-closed-loop-experience-correction-v0--prompt-record-path
        (expand-file-name
         "prompt-snapshots.json"
         superchat-closed-loop-experience-correction-v04--root)
        superchat-closed-loop-experience-correction-v0--preflight-record-path
        (expand-file-name
         "preflight-record.json"
         superchat-closed-loop-experience-correction-v04--directory)
        superchat-data-directory
        superchat-closed-loop-experience-correction-v0--runtime-directory
        superchat-closed-loop-experience-correction-v0--launches nil
        superchat-closed-loop-experience-correction-v0--prompt-snapshots nil
        superchat-closed-loop-experience-correction-v0--prepared-queries
        (make-hash-table :test #'equal))
  (make-directory superchat-closed-loop-experience-correction-v04--root t)
  (superchat-closed-loop-experience-correction-v0--load-records))

(defun superchat-closed-loop-experience-correction-v04--verify-input-contract ()
  "Prove every assignment reaches a stage prompt with its frozen question."
  (let* ((spec
          (superchat-closed-loop-experience-correction-v0--read-json
           superchat-closed-loop-experience-correction-v0--assignments-path))
         (tasks (plist-get spec :tasks))
         (assignments (plist-get spec :assignments)))
    (dolist (assignment assignments)
      (let* ((task-id
              (superchat-closed-loop-experience-correction-v0--assignment-task-id
               assignment))
             (matches
              (cl-remove-if-not
               (lambda (task) (equal task-id (plist-get task :task_id)))
               tasks))
             (question
              (superchat-closed-loop-experience-correction-v0--task-question
               assignment))
             (query
              (superchat-closed-loop-experience-correction-v0--query assignment)))
        (unless (= 1 (length matches))
          (error "Assignment %s maps to %d tasks"
                 (superchat-closed-loop-experience-correction-v0--assignment-id
                  assignment)
                 (length matches)))
        (unless (string-match-p
                 (regexp-quote (concat "Question: " question)) query)
          (error "Assignment %s query omitted its frozen question"
                 (superchat-closed-loop-experience-correction-v0--assignment-id
                  assignment)))
        (dolist (role '(collector analyst reviewer))
          (let ((stage
                 (superchat-research-workflow--stage-prompt
                  role (list :query query) '(:steps nil))))
            (when (string-match-p "Question: nil" stage)
              (error "Assignment %s produced Question: nil"
                     (superchat-closed-loop-experience-correction-v0--assignment-id
                      assignment)))
            (unless (string-match-p (regexp-quote question) stage)
              (error "Assignment %s omitted its question from %s stage"
                     (superchat-closed-loop-experience-correction-v0--assignment-id
                      assignment)
                     role)))))))
  t)

;;;###autoload
(defun superchat-closed-loop-experience-correction-v04-preflight ()
  "Run the complete model-free gate for manifest revision v0.4."
  (interactive)
  (superchat-closed-loop-experience-correction-v04--configure)
  (let ((record (superchat-closed-loop-experience-correction-v0-preflight)))
    (superchat-closed-loop-experience-correction-v04--verify-input-contract)
    (superchat-closed-loop-experience-correction-v0-self-check)
    (setq record
          (append record
                  (list :manifest_revision "v0.4"
                        :input_contract "pass"
                        :questions_verified 24
                        :stage_prompts_verified 72)))
    (superchat-closed-loop-experience-correction-v0--write-json
     superchat-closed-loop-experience-correction-v0--preflight-record-path
     record)
    record))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v04-start (run-index)
  "Start revision v0.4 assignment RUN-INDEX after its preflight."
  (interactive "nClosed-loop v0.4 run index (1-24): ")
  (superchat-closed-loop-experience-correction-v04-preflight)
  (superchat-closed-loop-experience-correction-v0-start run-index))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v04-finalize (run-index)
  "Finalize revision v0.4 assignment RUN-INDEX."
  (interactive "nClosed-loop v0.4 run index to finalize: ")
  (superchat-closed-loop-experience-correction-v04--configure)
  (superchat-closed-loop-experience-correction-v0-finalize run-index))

;;;###autoload
(defun superchat-closed-loop-experience-correction-v04-run-all ()
  "Execute all revision v0.4 first attempts after the mandatory preflight."
  (interactive)
  (superchat-closed-loop-experience-correction-v04-preflight)
  (superchat-closed-loop-experience-correction-v0-run-all))

(superchat-closed-loop-experience-correction-v04--configure)

(provide 'superchat-closed-loop-experience-correction-v04-run)

;;; run.el ends here
