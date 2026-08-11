;;; closed-loop.el --- Small temporal experience loop -*- lexical-binding: t; -*-

;; This is an experiment adapter, not a product module.  It keeps the temporal
;; seam explicit: a source Run closes before its Candidate is admitted, and
;; only then do held-out Runs receive a View.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(load (expand-file-name "paired-use.el"
                       (file-name-directory (or load-file-name buffer-file-name)))
      nil nil)

(defconst superchat-learning-minimal-closed--directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this experiment's runner files.")

(defun superchat-learning-minimal-closed--runner-files ()
  "Return hashes of the files that define this experiment and its gate."
  (mapcar
   (lambda (name)
     (let ((path (expand-file-name name
                                   superchat-learning-minimal-closed--directory)))
       (unless (file-readable-p path)
         (error "Experiment runner file is absent: %s" name))
       (list :file name :sha256 (secure-hash 'sha256 path))))
   '("closed-loop.el" "paired-use.el" "generalization.el" "verdict.el")))

;; The experiment deliberately binds these product globals around one
;; isolated run.  Declare them special before the lexical function body.
(defvar superchat-data-directory)
(defvar superchat-runtime--runs)
(defvar superchat-pre-topic-finalized-functions)
(defvar superchat-post-turn-functions)
(defvar superchat-post-topic-finalized-functions)
(defvar superchat-run-completed-functions)
(defvar superchat-turn-completed-functions)
(defvar superchat-learning-minimal--wrong-action-fails)

(defun superchat-learning-minimal-closed--tag (result method expected)
  "Annotate RESULT with verifier-facing METHOD and EXPECTED action."
  (plist-put (plist-put result :method method) :expected-action expected))

(defun superchat-learning-minimal-closed--context (seed ordinal)
  "Create a fresh Context/Relation and return its reviewed View.
ORDINAL keeps the two held-out contexts independent while reusing the same
Candidate and Promotion."
  (let* ((scope (plist-get seed :scope))
         (candidate (plist-get seed :candidate))
         (evaluation (plist-get seed :evaluation))
         (context-session (format "minimal-heldout-context-%d" ordinal))
         (relation-session (format "minimal-heldout-relation-%d" ordinal))
         (context
          (superchat-claim-record
           (format "A held-out opaque recovery decision %d for code %s."
                   ordinal (superchat-learning-minimal--scenario-code))
           scope nil context-session "agent:heldout" nil 'context))
         (context-ref
          (superchat-learning-minimal--ref context-session context))
         (relation
          (superchat-experience-record-relation
           (list :session-id relation-session :actor-id "agent:heldout"
                 :correlation-id (format "minimal-heldout-relation-%d" ordinal)
                 :relation 'applies-under :from candidate :to context-ref
                 :scope scope :basis (list evaluation)
                 :reason "Pre-registered held-out applicability.")))
         (relation-ref
          (superchat-learning-minimal--ref relation-session relation))
         (sessions (append (append (plist-get seed :sessions) nil)
                           (list context-session relation-session)))
         (view (superchat-experience-assemble-view
                sessions scope context-ref)))
    (unless (eq (plist-get view :status) 'satisfied)
      (error "Held-out View %d was not satisfied: %S" ordinal view))
    (list :ordinal ordinal :context context-ref :relation relation-ref
          :sessions (vconcat sessions) :view view)))

(defun superchat-learning-minimal-closed--task (ordinal)
  "Return wording for held-out task ORDINAL without naming the answer."
  (if (= ordinal 1)
      (format "Handle a new opaque recovery check. Call %s once with code %s. After its failure, choose exactly one follow-up action for that same code. Do not answer with prose or call tools in parallel."
              (superchat-learning-minimal--scenario-read-tool)
              (superchat-learning-minimal--scenario-code))
    (format "Complete a second opaque recovery check. Call %s once with code %s, then choose one corrective action for the same code. Use tools serially and do not answer with prose."
            (superchat-learning-minimal--scenario-read-tool)
            (superchat-learning-minimal--scenario-code))))

(defun superchat-learning-minimal-closed--run ()
  "Run one source→admit→held-out→reopen→revoke loop, without retries."
  (let* ((output (getenv "SUPERCHAT_EXPERIENCE_CLOSED_LOOP_OUTPUT"))
         (root (and output (file-name-directory (expand-file-name output))))
         (superchat-data-directory root)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-learning-minimal--wrong-action-fails t)
         (superchat-learning-minimal--sampling-seed
          (superchat-learning-minimal--scenario-seed))
         source seed heldout-1 heldout-2 control-1 treatment-1
         control-2 treatment-2 persistence persistence-view revoked-view revoked-run
         revocation induction results trace)
    (unless (and output root)
      (error "SUPERCHAT_EXPERIENCE_CLOSED_LOOP_OUTPUT is required"))
    (when (file-exists-p output)
      (error "Refusing to overwrite closed-loop trace: %s" output))
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
    (advice-add 'superchat--builtin-capability-preset :around
                #'superchat-learning-minimal--sampling-preset-advice)
    (when superchat-learning-minimal--sampling-seed
      (advice-add 'superchat--build-llm-prompt :around
                  #'superchat-learning-minimal--sampling-prompt-advice))
    (unwind-protect
        (progn
          (superchat-learning-minimal--register-tools)

          ;; The source episode must contain an observed failure followed by a
          ;; successful correction before the model may distill that trace.
          (setq source
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'induction-source nil
                  (format
                   (concat
                   "This is a training episode. Call %s exactly once with code %s; "
                   "it will fail. After the error, choose one "
                   "available recovery action for the same code. Do not answer "
                   "with prose, call tools in parallel, or assume an action "
                   "before observing the failure.")
                   (superchat-learning-minimal--scenario-read-tool)
                   (superchat-learning-minimal--scenario-code))
                  nil (superchat-learning-minimal--scenario-max-tool-rounds))
                 'induction-source
                 (superchat-learning-minimal--scenario-target-tool)))
          (setq induction (superchat-learning-minimal--induce source)
                seed (superchat-learning-minimal--seed source induction)
                heldout-1 (superchat-learning-minimal-closed--context seed 1)
                heldout-2 (superchat-learning-minimal-closed--context seed 2))

          ;; Two independent held-out pairs.  The only difference in each pair
          ;; is whether the admitted View is present.
          (setq control-1
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'holdout-control-1 nil
                  (superchat-learning-minimal-closed--task 1) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'holdout-control-1
                 (superchat-learning-minimal--scenario-target-tool))
                treatment-1
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'holdout-treatment-1 (plist-get heldout-1 :view)
                  (superchat-learning-minimal-closed--task 1) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'holdout-treatment-1
                 (superchat-learning-minimal--scenario-target-tool))
                control-2
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'holdout-control-2 nil
                  (superchat-learning-minimal-closed--task 2) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'holdout-control-2
                 (superchat-learning-minimal--scenario-target-tool))
                treatment-2
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'holdout-treatment-2 (plist-get heldout-2 :view)
                  (superchat-learning-minimal-closed--task 2) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'holdout-treatment-2
                 (superchat-learning-minimal--scenario-target-tool)))

          ;; Reopen the DB before the persistence check, then rebuild the View
          ;; from the persisted Candidate/Relation references.  Keep the origin
          ;; on the View so the verifier cannot confuse this with the old
          ;; in-memory object.
          (superchat-db-close)
          (superchat-db-open)
          (setq persistence-view
                (superchat-experience-assemble-view
                 (append (plist-get heldout-1 :sessions) nil)
                 (plist-get seed :scope)
                 (plist-get heldout-1 :context)))
          (setq persistence-view
                (plist-put persistence-view :assembly-origin 'after-db-reopen))
          (setq persistence
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'persistence-after-reopen persistence-view
                  (superchat-learning-minimal-closed--task 1) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'persistence-after-reopen
                 (superchat-learning-minimal--scenario-target-tool)))

          ;; Revocation is appended in the original Promotion Session; no
          ;; existing Tape fact or prompt is overwritten.  Reopen once more so
          ;; this check covers the persisted revocation, not just the current
          ;; process's projection cache.
          (setq revocation
                (superchat-experience-record-disposition
                 (list :session-id "minimal-promotion" :actor-id "user"
                       :candidate (plist-get seed :candidate)
                       :scope (plist-get seed :scope) :decision 'revoke
                       :basis (list (plist-get seed :evaluation))
                       :reason "User revoked the induced candidate."
                       :prior (plist-get seed :promotion))))
          (superchat-db-close)
          (superchat-db-open)
          (setq revoked-view
                (superchat-experience-assemble-view
                 (append (plist-get heldout-1 :sessions) nil)
                 (plist-get seed :scope)
                 (plist-get heldout-1 :context)))
          (setq revoked-view
                (plist-put revoked-view :assembly-origin 'after-revocation-reopen))
          (setq revoked-run
                (superchat-learning-minimal-closed--tag
                 (superchat-learning-minimal--run-arm
                  'persistence-after-revocation revoked-view
                  (superchat-learning-minimal-closed--task 1) nil
                  (superchat-learning-minimal--scenario-max-tool-rounds))
                 'persistence-after-revocation
                 (superchat-learning-minimal--scenario-target-tool)))
          (setq results (list source control-1 treatment-1 control-2 treatment-2
                              persistence revoked-run)))
      (ignore-errors
        (superchat-unregister-tool
         (superchat-learning-minimal--scenario-read-tool)))
      (ignore-errors
        (superchat-unregister-tool
         (superchat-learning-minimal--scenario-target-tool)))
      (ignore-errors
        (superchat-unregister-tool
         (superchat-learning-minimal--scenario-wrong-tool)))
      (when superchat-learning-minimal--sampling-seed
        (advice-remove 'superchat--build-llm-prompt
                       #'superchat-learning-minimal--sampling-prompt-advice))
      (advice-remove 'superchat--builtin-capability-preset
                     #'superchat-learning-minimal--sampling-preset-advice)
      (superchat-db-close))
    (setq trace
          (list :format "superchat-experience-self-learning-v1"
                :claim "model-induced Candidate and held-out Task Effect"
                :primary-metric 'first-recovery-action
                :sampling (list :temperature
                                (superchat-learning-minimal--scenario-temperature)
                                :seed (superchat-learning-minimal--scenario-seed))
                :scenario (list :code (superchat-learning-minimal--scenario-code)
                                :target-action
                                (superchat-learning-minimal--scenario-target-tool)
                                :wrong-action
                                (superchat-learning-minimal--scenario-wrong-tool)
                                :tool-surface
                                (superchat-learning-minimal--scenario-tool-names))
                :code (superchat-learning-minimal--scenario-code)
                :target-action
                (superchat-learning-minimal--scenario-target-tool)
                :tool-surface
                (superchat-learning-minimal--scenario-tool-names)
                :source (list :emacs emacs-version
                              :model superchat-learning-minimal--model
                              :runner-files
                              (superchat-learning-minimal-closed--runner-files)
                              :jj (superchat-acceptance-v26--jj-identity
                                   superchat-acceptance-v26--repo-root)
                              :dependencies
                              (superchat-recovery-trace--dependency-records))
                :induction induction
                :seed (list :scope (plist-get seed :scope)
                            :statement (plist-get seed :statement)
                            :candidate (plist-get seed :candidate)
                            :candidate-actor "agent:inducer"
                            :candidate-basis
                            (plist-get (plist-get seed :induced) :basis)
                            :evaluation (plist-get seed :evaluation)
                            :promotion (plist-get seed :promotion)
                            :induced (plist-get seed :induced))
                :held-out-contexts
                (list (list :ordinal 1 :context (plist-get heldout-1 :context)
                            :relation (plist-get heldout-1 :relation))
                      (list :ordinal 2 :context (plist-get heldout-2 :context)
                            :relation (plist-get heldout-2 :relation)))
                :held-out-pairs
                '((holdout-control-1 holdout-treatment-1)
                  (holdout-control-2 holdout-treatment-2))
                :revocation revocation
                :revoked-view-status (plist-get revoked-view :status)
                :persistence-view-origin
                (plist-get persistence-view :assembly-origin)
                :revoked-view-origin
                (plist-get revoked-view :assembly-origin)
                :methods results
                :provider-requests
                (+ (length (plist-get induction :transport-requests))
                   (apply #'+ (mapcar (lambda (result)
                                       (length (plist-get result
                                                          :transport-requests)))
                                     results)))
                :notes
                '(:no-retry t
                  :source "Run A contains a wrong branch and a correction; induction reads only its closed Tape trace."
                  :held-out "Two independent control/treatment pairs."
                  :persistence "DB reopened, then View was freshly assembled."
                  :revocation "User revocation was appended, DB reopened, then View was freshly assembled."
                  :candidate "Candidate statement comes from one real model reflection Run; user promotion remains the safety gate."
                  :human-gate t
                  :default-remains-off t)))
      (superchat-recovery-trace--write output trace)
      (princ (format "closed-loop trace: %s\nrequests=%d\nrevoked-view=%S\n"
                     output (plist-get trace :provider-requests)
                     (plist-get trace :revoked-view-status)))))

(when (equal (getenv "SUPERCHAT_EXPERIENCE_CLOSED_LOOP_EXECUTE") "1")
  (condition-case err
      (progn (superchat-learning-minimal-closed--run) (kill-emacs 0))
    (error
     (princ (format "closed-loop infrastructure error: %s\n"
                    (error-message-string err)))
     (kill-emacs 1))))

(provide 'superchat-experience-learning-closed-loop)

;;; closed-loop.el ends here
