;;; review-flow-real.el --- Real-model verification of the user review path -*- lexical-binding: t; -*-
;;; Commentary:
;; The /review host seam exists and is covered by fixture ERT tests.  This
;; driver exercises it against a real model: a promoted Candidate steers the
;; recovery action, the action FAILS (injected), the intervention becomes an
;; open Review Need, and the three judgments (retain / fails-under / revoke)
;; land on Tape through the real command path.
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "../experience-view-recovery-trace/durable-trace.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)
(load (expand-file-name "recovery-five-methods.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defvar superchat-data-directory)
(defvar superchat-workspace--registry)
(defvar superchat-workspace--artifact-counter)
(defvar superchat-runtime-recovery-intervention-enabled)
(defvar superchat-response-timeout)
(defvar superchat-tool-timeout-multiplier)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
(defvar superchat-llm-unknown-tool-retries)
(defvar superchat-agent-max-tool-calls)
(defvar superchat-agent-confirm-destructive)
(defvar superchat-lang)
(defvar superchat-llm-streaming)
(defvar superchat-llm-tools-enabled)
(defvar superchat-llm-backend)
(defvar superchat--session-id)
(defvar superchat-recovery-trace--llm-log)
(defvar superchat-recovery-trace--transport)
(defvar superchat-experience-workspace-revision-recovery-scope)
(defvar superchat-acceptance-v26--repo-root)
(defvar superchat-acceptance-v26--system-prompt)
(defvar superchat-recovery-five--model)
(defvar superchat-recovery-five--base-url)

(defconst superchat-review-real--cell
  '(:name "review-P" :positive t :kind stale :stale t))

(defun superchat-review-real--run ()
  "Run one real recovery whose steered action fails, then review it."
  (let* ((output (getenv "SUPERCHAT_REVIEW_REAL_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (directory (and root (expand-file-name "review-P" root)))
         setup before-needs need retain-result needs-after-retain
         revoke-result needs-final trace results)
    (unless (and output root)
      (error "SUPERCHAT_REVIEW_REAL_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite review trace: %s" output))
    (make-directory directory t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tape-view)
    (require 'superchat-tools)
    (require 'superchat-workspace)
    (require 'llm)
    (if superchat-recovery-five--base-url
        (require 'llm-openai)
      (require 'llm-ollama))
    ;; Seal the stale setup (durable-file, in-run mutation).
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (stale-p t)
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
            (setq setup
                  (superchat-acceptance-v26--setup-experience
                   superchat-review-real--cell)))
        (superchat-db-close)))
    ;; Run the steered recovery with workspace_info sabotaged: the promoted
    ;; rule says "call workspace_info", the action fails, and the model then
    ;; answers.  That failed steered action is the Review Need.
    (let ((superchat-data-directory directory)
          (superchat-workspace--registry nil)
          (superchat-workspace--artifact-counter 0)
          (sessions (append (plist-get setup :sessions) nil)))
      (superchat-db-close)
      (superchat-db-open)
      (cl-letf (((symbol-function 'superchat-workspace-info)
                 (lambda (&optional _workspace-id)
                   (error "workspace_info unavailable in this environment"))))
        (push (superchat-recovery-five--run-arm
               directory "review-treatment" setup sessions nil)
              results))
      ;; The failed steered action must now be an open Review Need.
      (setq before-needs (superchat-experience-review-needs))
      (unless (> (length (plist-get before-needs :open)) 0)
        (error "No open Review Need after the failed steered action: %S"
               before-needs))
      (setq need (aref (plist-get before-needs :open) 0))
      ;; Judgment 1: retain (records the applies-under relation).
      (setq retain-result
            (superchat-experience--cmd-review
             "review" "retain retained after the real failed recovery"
             nil nil nil))
      (setq needs-after-retain (superchat-experience-review-needs))
      ;; Judgment 2: revoke (removes the promotion via the command path).
      (setq revoke-result
            (superchat-experience--cmd-review
             "review" "revoke withdrawn pending new evidence"
             nil nil nil))
      (setq needs-final (superchat-experience-review-needs))
      (superchat-db-close))
    (setq trace
          (list :format "superchat-experience-review-real-v1"
                :claim "real-model user review path"
                :source (list :emacs emacs-version
                              :model superchat-recovery-five--model
                              :jj (superchat-acceptance-v26--jj-identity
                                   superchat-acceptance-v26--repo-root)
                              :dependencies
                              (superchat-recovery-trace--dependency-records))
                :setup (list :workspace-id (plist-get setup :workspace-id)
                             :artifact-id (plist-get setup :artifact-id))
                :arms results
                :open-needs-before (length (plist-get before-needs :open))
                :need (plist-get need :candidate)
                :retain-result retain-result
                :open-needs-after-retain
                (length (plist-get needs-after-retain :open))
                :revoke-result revoke-result
                :notes
                '(:no-retry t
                  :steered-action-failed "workspace_info sabotaged via cl-letf"
                  :judgments "retain then revoke through the real command"
                  :default-remains-off t)))
    (superchat-recovery-trace--write output trace)
    (princ (format "review trace: %s\n" output))
    (princ (format "open-before=%d after-retain=%d\n"
                   (plist-get trace :open-needs-before)
                   (plist-get trace :open-needs-after-retain)))))

(when (equal (getenv "SUPERCHAT_REVIEW_REAL_EXECUTE") "1")
  (condition-case err
      (progn (superchat-review-real--run) (kill-emacs 0))
    (error
     (princ (format "review real infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-review-real)

;;; review-flow-real.el ends here
