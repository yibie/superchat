;;; heldout-cohort-v2a.el --- Frozen V2a provider cohort -*- lexical-binding: t; -*-

;; PROTOTYPE: On the 16 sealed assignments, does the real provider's first
;; post-error Action vary with the Candidate card and opaque error code?
;; This is a one-shot fixed-exercise observation, not evidence of Learning.

(require 'cl-lib)
(require 'json)
(require 'superchat)
(require 'superchat-tools)
(require 'llm-ollama)

(defvar semuse-v2a-harness-library-mode t)
(load (expand-file-name
       "product-harness-prototype-v2a.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defconst semuse-v2a-heldout--llm-fork-commit
  "f6a83bb05e45377d5166821b84aa178c9164b55b")
(defconst semuse-v2a-heldout--llm-ollama-sha256
  "74dcdad296f920ea7ee43e9411398f05e0079d044a34c6e2efa4d7d3aa107505")
(defconst semuse-v2a-heldout--provider "ollama")
(defconst semuse-v2a-heldout--model "qwen3.5:9b")
(defconst semuse-v2a-heldout--timeout 45)
(defconst semuse-v2a-heldout--max-tokens 64)
(defconst semuse-v2a-heldout--reasoning 'none)
(defconst semuse-v2a-heldout--provider-request-ceiling 32)
(defvar semuse-v2a-heldout--assignment-audits nil)

(defun semuse-v2a-heldout--file-sha256 (file)
  "Return FILE's exact SHA-256."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun semuse-v2a-heldout--make-backend ()
  "Return the frozen native Ollama backend."
  (make-llm-ollama
   :chat-model semuse-v2a-heldout--model
   :host "127.0.0.1" :port 11434
   :default-chat-max-tokens semuse-v2a-heldout--max-tokens
   :default-chat-non-standard-params nil))

(defun semuse-v2a-heldout--make-preset ()
  "Return the frozen cohort preset."
  (superchat-preset-from-plist
   (list :name "v2a-heldout-cohort" :type 'prompt
         :max-tokens semuse-v2a-heldout--max-tokens)))

(defun semuse-v2a-heldout--assert-contract ()
  "Fail unless source and reasoning controls match the frozen contract."
  (let* ((source (locate-library "llm-ollama"))
         (superchat-llm-reasoning semuse-v2a-heldout--reasoning)
         (prompt (superchat--build-llm-prompt
                  "contract-check" nil nil
                  (semuse-v2a-heldout--make-preset)))
         (request (llm-provider-chat-request
                   (semuse-v2a-heldout--make-backend) prompt t))
         (options (plist-get request :options)))
    (unless (and source
                 (equal semuse-v2a-heldout--llm-ollama-sha256
                        (semuse-v2a-heldout--file-sha256 source))
                 (eq :false (plist-get request :think))
                 (not (plist-member options :num_think_tokens)))
      (error "Held-out provider contract mismatch: source=%S request=%S"
             source request))))

(defun semuse-v2a-heldout--contract ()
  "Return the frozen held-out contract."
  (list
   :contract-revision "v2a-heldout-1"
   :evaluator-manifest-sha256 semuse-v2a-sealed-evaluator-sha256
   :llm-fork-commit semuse-v2a-heldout--llm-fork-commit
   :llm-ollama-sha256 semuse-v2a-heldout--llm-ollama-sha256
   :provider semuse-v2a-heldout--provider :model semuse-v2a-heldout--model
   :endpoint "http://127.0.0.1:11434/api/chat" :cost-ceiling-cny 0
   :assignments 16 :requests-per-assignment 2
   :provider-request-ceiling semuse-v2a-heldout--provider-request-ceiling
   :max-output-tokens semuse-v2a-heldout--max-tokens
   :timeout-seconds semuse-v2a-heldout--timeout
   :reasoning semuse-v2a-heldout--reasoning :non-standard-params nil
   :tools '("read_resource" "refresh_handle" "report_termination"
            "report_uncertain")
   :stop "One pass in sealed order. No retry or replacement. Stop each Run after its second tool round or any terminal response."))

(defun semuse-v2a-heldout--print-contract ()
  "Print the frozen held-out contract."
  (semuse-v2a-heldout--assert-contract)
  (let ((contract (semuse-v2a-heldout--contract)))
    (while contract
      (princ (format "%s=%S\n"
                     (substring (symbol-name (pop contract)) 1)
                     (pop contract))))))

(defun semuse-v2a-heldout--prompt (entry view)
  "Return the provider prompt for sealed manifest ENTRY and product VIEW."
  (concat
   (when-let* ((experience (plist-get view :prompt)))
     (concat experience "\n\n"))
   (format
    (concat
     "Exercise protocol. First call read_resource exactly once with code %s. "
     "It will return an opaque failure. After that failure, do not call "
     "read_resource again: make exactly one follow-up call. Apply the reviewed "
     "Experience only if its RULE matches this code; otherwise call "
     "report_uncertain. Pass the same code to the follow-up tool. "
     "Do not answer with plain text and do not call tools in parallel.")
    (plist-get entry :error-code))))

(defun semuse-v2a-heldout--events-of-type (events type)
  "Return EVENTS whose type equals TYPE."
  (cl-remove-if-not
   (lambda (event) (equal type (plist-get event :type))) events))

(defun semuse-v2a-heldout--run-consumer
    (entry workspace session context-envelope view)
  "Run one sealed assignment through the real provider exactly once."
  (let* ((position (plist-get entry :position))
         (prompt (semuse-v2a-heldout--prompt entry view))
         (started (float-time))
         handle audit)
    (condition-case err
        (progn
          (setq handle
                (superchat-runtime-submit
                 (list :type :llm-query :input prompt :clean-input prompt
                       :prompt prompt :session-id session
                       :workspace-id workspace
                       :buffer-name " *semuse-v2a-heldout*"
                       :target-model semuse-v2a-heldout--model
                       :tools '("read_resource" "refresh_handle"
                                "report_termination" "report_uncertain")
                       :preset (semuse-v2a-heldout--make-preset)
                       :attendance 'attended :authority-policy 'cowork
                       :approval-function (lambda (_request) t)
                       :round-limit-function (lambda () 'stop)
                       :unknown-tool-retries 0 :record-conversation nil
                       :evidence-contract "epistemic-learning-v0"
                       :context-envelope context-envelope)))
          (let* ((run-id (plist-get handle :run-id))
                 (deadline (+ (float-time)
                              (* 2 semuse-v2a-heldout--timeout) 5)))
            (while (and (< (float-time) deadline)
                        (not (memq
                              (plist-get (superchat-runtime-status run-id)
                                         :status)
                              superchat-runtime--terminal-statuses)))
              (accept-process-output nil 0.05))
            (unless (memq (plist-get (superchat-runtime-status run-id) :status)
                          superchat-runtime--terminal-statuses)
              (superchat-runtime-cancel run-id "Held-out wall timeout"))
            (let* ((status
                    (plist-get (superchat-runtime-status run-id) :status))
                   (events
                    (cl-remove-if-not
                     (lambda (event)
                       (equal run-id (plist-get event :run-id)))
                     (superchat-tape-replay-events session)))
                   (decisions
                    (semuse-v2a-heldout--events-of-type
                     events "decision.snapshot.recorded"))
                   (failures
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal "tool.failed" (plist-get event :type))
                            (equal "read_resource"
                                   (plist-get (plist-get event :payload)
                                              :tool-name))
                            (= 1 (or (plist-get (plist-get event :payload)
                                                :evidence-attempt)
                                     0))))
                     events))
                   (post-decisions
                    (cl-remove-if-not
                     (lambda (event)
                       (= 2 (or (plist-get (plist-get event :payload) :attempt)
                                0)))
                     decisions))
                   (post-actions
                    (cl-remove-if-not
                     (lambda (event)
                       (and (equal "tool.requested" (plist-get event :type))
                            (= 2 (or (plist-get (plist-get event :payload)
                                                :evidence-attempt)
                                     0))))
                     events))
                   (ok (and (= 2 (length decisions)) (= 1 (length failures))
                            (= 1 (length post-decisions))
                            (= 1 (length post-actions)))))
              (setq audit
                    (list :position position :assignment-id
                          (plist-get entry :assignment-id) :run-id run-id
                          :status status :ok ok
                          :duration-ms
                          (round (* 1000 (- (float-time) started)))
                          :provider-request-count (length decisions)
                          :trigger-count (length failures)
                          :post-decision-count (length post-decisions)
                          :post-action-count (length post-actions)
                          :post-action-tool
                          (and (= 1 (length post-actions))
                               (plist-get (plist-get (car post-actions) :payload)
                                          :tool-name))
                          :event-count (length events)))
              (push audit semuse-v2a-heldout--assignment-audits)
              (unless ok
                (error "Assignment %d did not produce one sealed post-error Action"
                       position))
              (list :trigger
                    (semuse-v2a-harness--ref session (car failures))
                    :decision
                    (semuse-v2a-harness--ref session (car post-decisions))
                    :action
                    (semuse-v2a-harness--ref session (car post-actions))))))
      (error
       (unless audit
         (push (list :position position :assignment-id
                     (plist-get entry :assignment-id) :ok nil
                     :duration-ms (round (* 1000 (- (float-time) started)))
                     :error (error-message-string err))
               semuse-v2a-heldout--assignment-audits))
       (signal (car err) (cdr err))))))

(defun semuse-v2a-heldout--register-tools ()
  "Register the frozen, local-effect cohort tools."
  (superchat-register-tool
   "read_resource" :function
   (lambda (code)
     (unless (member code '("E-R17" "E-N04"))
       (error "Unexpected opaque code: %s" code))
     (signal 'semuse-v2a-opaque-boundary (list :code code)))
   :description "Read one opaque resource before choosing a recovery Action."
   :args '((:name "code" :type string :description "Opaque recovery code."))
   :effects '((:effect local-read :resource opaque-resource
               :scope "heldout://fixed")))
  (dolist (spec '(("refresh_handle" "Refresh the handle for this code.")
                  ("report_termination" "Report a terminal boundary for this code.")
                  ("report_uncertain" "Report insufficient applicable guidance for this code.")))
    (superchat-register-tool
     (car spec) :function (lambda (_code) "recorded")
     :description (cadr spec)
     :args '((:name "code" :type string :description "Opaque recovery code."))
     :effects '((:effect local-read :resource recovery-record
                 :scope "heldout://fixed")))))

(defun semuse-v2a-heldout--write (directory name value)
  "Write VALUE readably to NAME under evidence DIRECTORY."
  (with-temp-file (expand-file-name name directory)
    (let ((print-length nil) (print-level nil))
      (prin1 value (current-buffer))
      (insert "\n"))))

(defun semuse-v2a-heldout-run ()
  "Run the frozen cohort once, preserving every assignment and Tape row."
  (semuse-v2a-heldout--assert-contract)
  (let* ((directory (make-temp-file "semuse-v2a-heldout-cohort-" t))
         (superchat-data-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-response-timeout semuse-v2a-heldout--timeout)
         (llm-ollama-chat-timeout semuse-v2a-heldout--timeout)
         (superchat-llm-max-tool-rounds 2)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-llm-reasoning semuse-v2a-heldout--reasoning)
         (superchat-llm-streaming t)
         (superchat-llm-backend (semuse-v2a-heldout--make-backend))
         (semuse-v2a-heldout--assignment-audits nil)
         records failures result request-count row-count)
    (unwind-protect
        (progn
          (superchat-db-close)
          (superchat-db-open)
          (semuse-v2a-heldout--register-tools)
          (cl-letf (((symbol-function 'semuse-v2a-harness--run-consumer)
                     #'semuse-v2a-heldout--run-consumer))
            (dolist (entry
                     (plist-get semuse-v2a-sealed-evaluator-manifest :schedule))
              (condition-case err
                  (push (semuse-v2a-harness--record entry) records)
                (error
                 (push (list :position (plist-get entry :position)
                             :assignment-id (plist-get entry :assignment-id)
                             :error (error-message-string err))
                       failures)))))
          (setq records (nreverse records)
                failures (nreverse failures)
                semuse-v2a-heldout--assignment-audits
                (nreverse semuse-v2a-heldout--assignment-audits)
                request-count
                (apply #'+
                       (mapcar
                        (lambda (audit)
                          (or (plist-get audit :provider-request-count) 0))
                        semuse-v2a-heldout--assignment-audits))
                row-count
                (caar (superchat-db-tape-select "SELECT COUNT(*) FROM tape"))
                result
                (semuse-v2a-score
                 records semuse-v2a-sealed-evaluator-manifest))
          (when (> request-count semuse-v2a-heldout--provider-request-ceiling)
            (error "Provider request ceiling exceeded: %d" request-count))
          (semuse-v2a-heldout--write
           directory "contract.el" (semuse-v2a-heldout--contract))
          (semuse-v2a-heldout--write directory "records.el" records)
          (semuse-v2a-heldout--write
           directory "result.el"
           (list :contract (semuse-v2a-heldout--contract)
                 :assignment-audits semuse-v2a-heldout--assignment-audits
                 :failures failures :record-count (length records)
                 :provider-request-count request-count :tape-row-count row-count
                 :result result)))
      (superchat-db-close)
      (when-let* ((buffer (get-buffer " *semuse-v2a-heldout*")))
        (kill-buffer buffer)))
    (list :ok (plist-get result :ok) :evidence-directory directory
          :record-count (length records) :failure-count (length failures)
          :provider-request-count request-count :tape-row-count row-count
          :result result :assignment-audits
          semuse-v2a-heldout--assignment-audits)))

(defun semuse-v2a-heldout--print-result (audit)
  "Print stable cohort AUDIT output."
  (let ((result (plist-get audit :result)))
    (princ (format "cohort=%s\n" (if (plist-get audit :ok) "SCORED" "NOT-IDENTIFIABLE")))
    (dolist (key '(:record-count :failure-count :provider-request-count
                   :tape-row-count :evidence-directory))
      (princ (format "%s=%S\n" (substring (symbol-name key) 1)
                     (plist-get audit key))))
    (princ (format "stage=%S\nstatus=%S\nerrors=%S\n"
                   (plist-get result :stage) (plist-get result :status)
                   (plist-get result :errors)))
    (dolist (assignment (plist-get audit :assignment-audits))
      (princ (format "assignment[%02d]=ok:%S requests:%S action:%S error:%S\n"
                     (plist-get assignment :position)
                     (plist-get assignment :ok)
                     (plist-get assignment :provider-request-count)
                     (plist-get assignment :post-action-tool)
                     (plist-get assignment :error))))))

(pcase command-line-args-left
  (`("contract")
   (semuse-v2a-heldout--print-contract)
   (kill-emacs 0))
  (`("run")
   (let ((audit (semuse-v2a-heldout-run)))
     (semuse-v2a-heldout--print-result audit)
     (kill-emacs (if (plist-get audit :ok) 0 1))))
  (_
   (princ "usage: heldout-cohort-v2a.el contract|run\n")
   (kill-emacs 2)))

;;; heldout-cohort-v2a.el ends here
