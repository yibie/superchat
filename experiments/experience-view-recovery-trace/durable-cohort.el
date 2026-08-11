;;; durable-cohort.el --- Adapter-aware durable recovery diagnostic -*- lexical-binding: t; -*-

;;; Commentary:
;; This is an exploratory adapter-aware seven-cell cohort, not a replacement for
;; the consumed v2.6 assignment.  Every cell uses the restart-safe
;; durable-file adapter, which survives DB reopen and remains mutable through
;; `superchat-workspace-write' with revision tracking.  The durable slice keeps
;; P-missing, P-stale, and the five negative candidate states.  Positive cells
;; exercise the complete three-tool recovery path; negative cells stop after
;; their first action and cannot silently retry.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "durable-trace.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defconst superchat-recovery-durable-cohort--cells
  '((:name "P-missing" :positive t :kind missing :stale nil)
    (:name "P-stale" :positive t :kind stale :stale t)
    (:name "N-absent" :positive nil :kind absent :stale nil)
    (:name "N-fails" :positive nil :kind fails :stale t)
    (:name "N-conflict" :positive nil :kind conflict :stale nil)
    (:name "N-multiple" :positive nil :kind multiple :stale t)
    (:name "N-revoked" :positive nil :kind revoked :stale nil)))

(defun superchat-recovery-durable-cohort--setup (cell directory)
  "Create CELL's durable-file Experience setup under DIRECTORY.
For the stale cell the revision mutation is performed inside the producer
Run so the changed Artifact stays inside that Run's durable proof; the
frozen helper's post-terminal mutation is suppressed for that cell."
  (let ((superchat-data-directory directory)
        (superchat-workspace--registry nil)
        (superchat-workspace--artifact-counter 0)
        (stale-p (eq (plist-get cell :kind) 'stale))
        (original-write (symbol-function 'superchat-workspace-write)))
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (cl-letf (((symbol-function 'superchat-workspace-add-region)
                   (lambda (_beg _end &optional role workspace-id)
                     (let ((meta
                            (superchat-workspace-publish-mutable
                             "sealed producer artifact\n" role workspace-id)))
                       (when stale-p
                         (funcall original-write
                                  "changed after recorded revision" nil
                                  (plist-get meta :artifact-id)
                                  (plist-get meta :revision) workspace-id))
                       meta)))
                  ((symbol-function 'superchat-workspace-write)
                   (lambda (content &optional append artifact-id
                                    expected-revision workspace-id)
                     (unless stale-p
                       (funcall original-write content append artifact-id
                                expected-revision workspace-id)))))
          (superchat-acceptance-v26--setup-experience cell))
      (superchat-db-close))))

(defun superchat-recovery-durable-cohort--terminal-events (events)
  "Return Runtime terminal events from EVENTS."
  (cl-remove-if-not
   (lambda (event)
     (member (plist-get event :type)
             '("run.completed" "run.failed" "run.cancelled"
               "run.timed-out")))
   events))

(defun superchat-recovery-durable-cohort--verify
    (cell setup events transport-count status)
  "Verify one durable cohort CELL without changing the old v2.6 verifier."
  (if (plist-get cell :positive)
      (superchat-recovery-durable--verify
       setup events transport-count status
       (and (plist-get cell :stale)
            (plist-get setup :old-revision)))
    (let* ((base (superchat-acceptance-v26--verify-cell
                  cell setup events transport-count))
           (errors (copy-sequence (plist-get base :errors)))
           (terminals
            (superchat-recovery-durable-cohort--terminal-events events)))
      (unless (= 1 (length terminals))
        (push "negative cell lacks exactly one Runtime terminal" errors))
      (unless (memq status '(completed failed cancelled timed-out))
        (push (format "negative cell has non-terminal Runtime status %S" status)
              errors))
      (list :passed (null errors)
            :errors (nreverse errors)
            :base base
            :tool-requested
            (mapcar (lambda (event)
                      (plist-get (plist-get event :payload) :tool-name))
                    (superchat-recovery-durable--events
                     events "tool.requested"))
            :terminal-count (length terminals)))))

(defun superchat-recovery-durable-cohort--run ()
  "Run the six adapter-aware durable cells exactly once and retain traces."
  (let* ((output (getenv "SUPERCHAT_DURABLE_COHORT_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (setups nil)
         (records nil)
         (setup-failure nil)
         (total-requests 0))
    (unless (and output root)
      (error "SUPERCHAT_DURABLE_COHORT_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite existing cohort trace: %s" output))
    (make-directory root t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'superchat-workspace)
    (require 'llm)
    (require 'llm-ollama)
    ;; Seal all six setup databases before the first provider request.
    (dolist (cell superchat-recovery-durable-cohort--cells)
      (let ((directory (expand-file-name (plist-get cell :name) root)))
        (make-directory directory t)
        (condition-case err
            (push (list :cell cell :directory directory
                        :setup
                        (superchat-recovery-durable-cohort--setup
                         cell directory))
                  setups)
          (error
           (setq setup-failure (error-message-string err))))))
    (setq setups (nreverse setups))
    (unless setup-failure
      (dolist (entry setups)
        (let* ((cell (plist-get entry :cell))
               (setup (plist-get entry :setup))
               (directory (plist-get entry :directory))
               (positive (plist-get cell :positive))
               (max-rounds (if positive 3 1))
               (ceiling (if positive 4 1))
               (limit-action (if positive 'finalize 'stop)))
          (setq superchat-recovery-trace--llm-log nil
                superchat-recovery-trace--transport nil)
          (let* ((summary
                  (superchat-recovery-durable--run-cell
                   setup directory cell max-rounds ceiling limit-action
                   #'superchat-recovery-durable-cohort--verify))
                 (result-record
                  (superchat-recovery-trace--read
                   (expand-file-name "result.el" directory)))
                 (requests (nreverse superchat-recovery-trace--transport))
                 (logs (nreverse superchat-recovery-trace--llm-log)))
            (cl-incf total-requests (length requests))
            (push (list :cell cell
                        :setup setup
                        :result result-record
                        :summary summary
                        :runtime-status (plist-get result-record
                                                   :runtime-status)
                        :transport-requests requests
                        :llm-log logs
                        :tape-events (plist-get result-record :events))
                  records)))))
    (setq records (nreverse records))
    (let* ((all-results
            (mapcar (lambda (record)
                      (plist-get (plist-get record :result) :verdict))
                    records))
           (passed (and (null setup-failure)
                        (= 7 (length records))
                        (<= total-requests 13)
                        (cl-every (lambda (verdict)
                                    (plist-get verdict :passed))
                                  all-results)))
           (trace
            (list :format "superchat-experience-view-durable-cohort-v1"
                  :mode "adapter-aware-seven-cell"
                  :source
                  (list :repo-root-role "<repo-root>"
                        :jj (superchat-acceptance-v26--jj-identity
                             superchat-acceptance-v26--repo-root)
                        :emacs emacs-version
                        :llm-commit superchat-acceptance-v26--llm-commit
                        :model superchat-acceptance-v26--model
                        :dependencies
                        (superchat-recovery-trace--dependency-records))
                  :cells (vconcat superchat-recovery-durable-cohort--cells)
                  :records records
                  :setup-failure setup-failure
                  :transport-count total-requests
                  :cohort-verdict
                  (list :passed passed
                        :default-remains-off t
                        :request-ceiling 13
                        :positive-request-ceiling 4
                        :negative-request-ceiling 1)
                  :notes
                  '(:artifact "All cells use durable-file; generated-file stays immutable; buffer-region remains process-local."
                    :positive "P-missing requires workspace_read -> workspace_info -> workspace_read -> final response."
                    :p-stale "P-stale seeds a mutable durable Artifact at R1, mutates it to R2, and reads with R1 to produce the typed revision failure."
                    :negative "Negative cells stop after their first action and require no Experience intervention."
                    :transport-body "Exact JSON request bodies are retained per cell."
                    :llm-response "Decoded llm log events are retained; raw HTTP chunks are not."
                    :tape "Runtime, Authority, Experience, and terminal events are retained."
                    :redaction "Authorization headers are redacted; endpoint is local Ollama."))))
      (superchat-recovery-trace--write output trace)
      (kill-emacs (if passed 0 1)))))

(when (equal (getenv "SUPERCHAT_DURABLE_COHORT_EXECUTE") "1")
  (superchat-recovery-durable-cohort--run))

(provide 'superchat-experience-view-durable-cohort)

;;; durable-cohort.el ends here
