;;; five-methods.el --- Thin five-method real-provider follow-up -*- lexical-binding: t; -*-

;; This driver intentionally stays outside the product.  It reuses the
;; minimal paired-use seams, changes one test variable at a time, and retains
;; the same complete per-Run trace.  It is a diagnostic cohort, not a frozen
;; acceptance contract.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "paired-use.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defvar superchat-data-directory)
(defvar superchat-runtime--runs)
(defvar superchat-pre-topic-finalized-functions)
(defvar superchat-post-turn-functions)
(defvar superchat-post-topic-finalized-functions)
(defvar superchat-run-completed-functions)
(defvar superchat-turn-completed-functions)

(defun superchat-learning-minimal-five--tag (result method expected)
  "Annotate RESULT with METHOD and the expected action for METHOD."
  (plist-put (plist-put result :method method) :expected-action expected))

(defun superchat-learning-minimal-five--run ()
  "Execute the five thin real-environment methods once, without retries."
  (let* ((output (getenv "SUPERCHAT_EXPERIENCE_FIVE_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (superchat-data-directory root)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         seed view control treatment ablation transfer persistence
         negative-relation negative-view negative-sessions revocation
         revoked-view persistence-revoked negative results)
    (unless (and output root)
      (error "SUPERCHAT_EXPERIENCE_FIVE_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite five-method trace: %s" output))
    (make-directory root t)
    (require 'superchat)
    (require 'superchat-claims)
    (require 'superchat-db)
    (require 'superchat-experience)
    (require 'superchat-runtime)
    (require 'superchat-tape)
    (require 'superchat-tools)
    (require 'llm)
    (require 'llm-ollama)
    (superchat-db-close)
    (superchat-db-open)
    (unwind-protect
        (progn
          (setq seed (superchat-learning-minimal--seed)
                view (plist-get seed :view))
          (superchat-learning-minimal--register-tools)

          ;; 1. Matched control/treatment.
          (setq control
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm 'five-control nil)
                 'matched-control "minimal_refresh")
                treatment
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm 'five-treatment view)
                 'matched-treatment "minimal_refresh"))

          ;; 2. Keep the lineage and receipt, remove the instruction text.
          (let ((receipt-only (copy-tree view t)))
            (setq receipt-only (plist-put receipt-only :prompt nil))
            (setq receipt-only (plist-put receipt-only :items []))
            (setq ablation
                  (superchat-learning-minimal-five--tag
                   (superchat-learning-minimal--run-arm
                    'five-ablation receipt-only)
                   'view-content-ablation "no-action-claim")))

          ;; 3. New wording, same abstract context and reviewed rule.
          (setq transfer
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm
                  'five-transfer view
                  (concat
                   "Handle a newly worded instance of an opaque recovery task. "
                   "Use read_probe once for E-R17, then choose exactly one "
                   "follow-up action with that same code. Do not answer with "
                   "prose or call tools in parallel."))
                 'wording-transfer "minimal_refresh"))

          ;; 5. Reopen the store before selecting the durable View.
          (superchat-db-close)
          (superchat-db-open)
          (setq persistence
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm
                  'five-persistence
                  (superchat-experience-assemble-view
                   (append (plist-get seed :sessions) nil)
                   (plist-get seed :scope)
                   (plist-get seed :context)))
                 'persistence-after-reopen "minimal_refresh"))

          ;; 4. Add a conflicting relation.  The real View assembler must
          ;; fail closed; the model receives no Candidate statement.
          (setq negative-relation
                (superchat-experience-record-relation
                 (list :session-id "minimal-negative-relation"
                       :actor-id "agent:test"
                       :correlation-id "minimal-negative"
                       :relation 'fails-under
                       :from (plist-get seed :candidate)
                       :to (plist-get seed :context)
                       :scope (plist-get seed :scope)
                       :basis (list (plist-get seed :evaluation))
                       :reason "Negative-transfer test.")))
          (setq negative-sessions
                (append (plist-get seed :sessions)
                        (list "minimal-negative-relation")))
          (setq negative-view
                (superchat-experience-assemble-view
                 negative-sessions (plist-get seed :scope)
                 (plist-get seed :context)))
          (setq negative
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm
                  'five-negative negative-view nil
                 '(:dependent-tools ("minimal_refresh")))
                 'negative-transfer "minimal_uncertain"))
          ;; Complete method 5: append the user's revocation, rebuild the
          ;; View, and run the same target task once more.  The promotion and
          ;; revocation share the promotion Session because the public writer
          ;; requires the prior reference to stay in that Session.
          (setq revocation
                (superchat-experience-record-disposition
                 (list :session-id "minimal-promotion" :actor-id "user"
                       :candidate (plist-get seed :candidate)
                       :scope (plist-get seed :scope) :decision 'revoke
                       :basis (list (plist-get seed :evaluation))
                       :reason "User revoked the minimal seed after reopen."
                       :prior (plist-get seed :promotion))))
          (setq revoked-view
                (superchat-experience-assemble-view
                 (append (plist-get seed :sessions) nil)
                 (plist-get seed :scope)
                 (plist-get seed :context)))
          (setq persistence-revoked
                (superchat-learning-minimal-five--tag
                 (superchat-learning-minimal--run-arm
                  'five-persistence-revoked revoked-view)
                 'persistence-after-revocation "minimal_refresh"))
          (setq results (list control treatment ablation transfer
                              negative persistence persistence-revoked)))
      (ignore-errors (superchat-unregister-tool "read_probe"))
      (ignore-errors (superchat-unregister-tool "minimal_refresh"))
      (ignore-errors (superchat-unregister-tool "minimal_uncertain"))
      (superchat-db-close))
    (let ((trace
           (list :format "superchat-experience-learning-five-methods-v1"
                 :claim "real-provider robustness around reviewed retrieval/use"
                 :source (list :emacs emacs-version
                               :model superchat-learning-minimal--model
                               :jj (superchat-acceptance-v26--jj-identity
                                    superchat-acceptance-v26--repo-root)
                               :dependencies
                               (superchat-recovery-trace--dependency-records))
                 :seed (list :scope (plist-get seed :scope)
                              :statement (plist-get seed :statement)
                              :candidate (plist-get seed :candidate)
                              :evaluation (plist-get seed :evaluation)
                              :context (plist-get seed :context)
                              :promotion (plist-get seed :promotion))
                 :negative-view-status (plist-get negative-view :status)
                 :negative-relation negative-relation
                 :revocation revocation
                 :revoked-view-status (plist-get revoked-view :status)
                 :methods results
                 :provider-requests
                 (apply #'+ (mapcar (lambda (result)
                                     (length (plist-get result
                                                        :transport-requests)))
                                   results))
                 :notes
                 '(:no-retry t
                   :control "No View."
                   :ablation "Receipt/lineage retained, candidate instruction removed."
                   :transfer "New wording, same reviewed abstract rule."
                   :negative "Conflicting relation must yield empty/blocked View."
                   :persistence "DB reopened before View assembly."
                   :revocation "User revocation appended, View rebuilt, and a second target Run executed."
                   :interpretation "Each method is scored independently; one model call never proves learning."
                   :default-remains-off t))))
      (superchat-recovery-trace--write output trace)
      (princ (format "five-method trace: %s\nrequests=%d\n"
                     output (plist-get trace :provider-requests)))))
    )

(when (equal (getenv "SUPERCHAT_EXPERIENCE_FIVE_EXECUTE") "1")
  (condition-case err
      (progn (superchat-learning-minimal-five--run) (kill-emacs 0))
    (error
     (princ (format "five-method infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-learning-five-methods)

;;; five-methods.el ends here
