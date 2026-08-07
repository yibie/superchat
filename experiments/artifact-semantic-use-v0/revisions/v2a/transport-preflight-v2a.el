;;; transport-preflight-v2a.el --- One-request transport check -*- lexical-binding: t; -*-

;; TASK: Does one pinned local Ollama request produce the exact Decision
;; Snapshot and first Action provenance required by the V2a evaluator?

(require 'cl-lib)
(require 'json)
(require 'superchat)
(require 'superchat-tools)
(require 'llm-ollama)

(defconst semuse-v2a-preflight--llm-fork-commit
  "f6a83bb05e45377d5166821b84aa178c9164b55b")
(defconst semuse-v2a-preflight--provider "ollama")
(defconst semuse-v2a-preflight--model "qwen3.5:9b")
(defconst semuse-v2a-preflight--timeout 45)
(defconst semuse-v2a-preflight--max-tokens 64)
(defconst semuse-v2a-preflight--reasoning 'none)
(defconst semuse-v2a-preflight--non-standard-params nil)
(defconst semuse-v2a-preflight--prompt
  "Transport preflight: call refresh_handle exactly once with code E-R17. Do not call another tool and do not answer with plain text.")

(defun semuse-v2a-preflight--make-backend ()
  "Return the frozen native Ollama backend."
  (make-llm-ollama
   :chat-model semuse-v2a-preflight--model
   :host "127.0.0.1" :port 11434
   :default-chat-max-tokens semuse-v2a-preflight--max-tokens
   :default-chat-non-standard-params
   semuse-v2a-preflight--non-standard-params))

(defun semuse-v2a-preflight--make-preset ()
  "Return the frozen preflight preset."
  (superchat-preset-from-plist
   (list :name "v2a-transport-preflight"
         :type 'prompt :max-tokens semuse-v2a-preflight--max-tokens)))

(defun semuse-v2a-preflight--assert-reasoning-control ()
  "Fail unless the frozen request disables thinking with Ollama's API field."
  (let* ((superchat-llm-reasoning semuse-v2a-preflight--reasoning)
         (prompt (superchat--build-llm-prompt
                  semuse-v2a-preflight--prompt nil nil
                  (semuse-v2a-preflight--make-preset)))
         (request
          (llm-provider-chat-request
           (semuse-v2a-preflight--make-backend)
           prompt t))
         (options (plist-get request :options)))
    (unless (and (eq :false (plist-get request :think))
                 (not (plist-member options :num_think_tokens)))
      (error "Preflight does not disable Ollama thinking: %S" request))))

(defun semuse-v2a-preflight--events-of-type (events type)
  "Return EVENTS whose type equals TYPE."
  (cl-remove-if-not
   (lambda (event) (equal type (plist-get event :type))) events))

(defun semuse-v2a-preflight--prepared-envelope (decision)
  "Decode DECISION's exact prepared envelope."
  (json-parse-string
   (plist-get (plist-get decision :payload) :prepared-envelope-json)
   :object-type 'plist :array-type 'array :null-object nil :false-object nil))

(defun semuse-v2a-preflight--contract ()
  "Return the frozen preflight contract."
  (list :llm-fork-commit semuse-v2a-preflight--llm-fork-commit
        :provider semuse-v2a-preflight--provider
        :model semuse-v2a-preflight--model
        :endpoint "http://127.0.0.1:11434/api/chat"
        :cost-ceiling-cny 0
        :provider-request-ceiling 1
        :max-output-tokens semuse-v2a-preflight--max-tokens
        :timeout-seconds semuse-v2a-preflight--timeout
        :reasoning semuse-v2a-preflight--reasoning
        :non-standard-params semuse-v2a-preflight--non-standard-params
        :prompt semuse-v2a-preflight--prompt
        :tool "refresh_handle"
        :arguments '(:code "E-R17")
        :expected-events
        '("decision.snapshot.recorded" "tool.requested"
          "tool.execution.started" "tool.completed" "run.failed")
        :stop "No retry; stop after the first tool round or any failure."))

(defun semuse-v2a-preflight--print-contract ()
  "Print the frozen preflight contract."
  (semuse-v2a-preflight--assert-reasoning-control)
  (let ((contract (semuse-v2a-preflight--contract)))
    (dolist (key '(:llm-fork-commit :provider :model :endpoint :cost-ceiling-cny
                   :provider-request-ceiling :max-output-tokens
                   :timeout-seconds :reasoning :non-standard-params
                   :prompt :tool :arguments
                   :expected-events :stop))
      (princ (format "%s=%S\n" (substring (symbol-name key) 1)
                     (plist-get contract key))))))

(defun semuse-v2a-preflight--run ()
  "Run the single-request transport preflight and return its audit."
  (let* ((directory (make-temp-file "semuse-v2a-transport-preflight-" t))
         (superchat-data-directory directory)
         (superchat-runtime--runs (make-hash-table :test #'equal))
         (superchat-closure-shadow-enabled nil)
         (superchat-closure-terminal-guard-enabled nil)
         (superchat-pre-topic-finalized-functions nil)
         (superchat-post-turn-functions nil)
         (superchat-post-topic-finalized-functions nil)
         (superchat-run-completed-functions nil)
         (superchat-turn-completed-functions nil)
         (superchat-response-timeout semuse-v2a-preflight--timeout)
         (llm-ollama-chat-timeout semuse-v2a-preflight--timeout)
         (superchat-llm-max-tool-rounds 1)
         (superchat-llm-unknown-tool-retries 0)
         (superchat-llm-reasoning semuse-v2a-preflight--reasoning)
         (superchat-llm-streaming t)
         (superchat-llm-backend (semuse-v2a-preflight--make-backend))
         (preset (semuse-v2a-preflight--make-preset))
         (session-id "semuse-v2a-transport-preflight")
         (started (float-time))
         handle audit)
    (unwind-protect
        (condition-case err
            (progn
          (superchat-db-close)
          (superchat-db-open)
          (superchat-register-tool
           "refresh_handle"
           :function
           (lambda (code)
             (unless (equal code "E-R17")
               (error "Unexpected opaque code: %s" code))
             "handle-refreshed")
           :description "Refresh the handle for one opaque recovery code."
           :args '((:name "code" :type string
                    :description "Opaque recovery code."))
           :effects '((:effect local-read :resource opaque-handle
                       :scope "preflight://E-R17")))
          (setq handle
                (superchat-runtime-submit
                 (list :type :llm-query
                       :input semuse-v2a-preflight--prompt
                       :clean-input semuse-v2a-preflight--prompt
                       :prompt semuse-v2a-preflight--prompt
                       :session-id session-id
                       :workspace-id "workspace_transport_preflight"
                       :buffer-name " *semuse-v2a-transport-preflight*"
                       :target-model semuse-v2a-preflight--model
                       :tools '("refresh_handle")
                       :preset preset
                       :attendance 'attended
                       :authority-policy 'cowork
                       :approval-function (lambda (_request) t)
                       :round-limit-function (lambda () 'stop)
                       :unknown-tool-retries 0
                       :record-conversation nil
                       :evidence-contract "epistemic-learning-v0"
                       :context-envelope
                       '(:contract "context-continuity-v0" :status empty
                         :receipt (:tape-head 0)))))
          (let ((run-id (plist-get handle :run-id))
                deadline)
            (setq deadline (+ (float-time) semuse-v2a-preflight--timeout 2))
            (while (and (< (float-time) deadline)
                        (not (memq (plist-get (superchat-runtime-status run-id)
                                              :status)
                                   superchat-runtime--terminal-statuses)))
              (accept-process-output nil 0.05))
            (unless (memq (plist-get (superchat-runtime-status run-id) :status)
                          superchat-runtime--terminal-statuses)
              (superchat-runtime-cancel run-id "Preflight wall timeout"))
            (let* ((status (plist-get (superchat-runtime-status run-id) :status))
                   (events
                    (cl-remove-if-not
                     (lambda (event)
                       (equal run-id (plist-get event :run-id)))
                     (superchat-tape-replay-events session-id)))
                   (decisions
                    (semuse-v2a-preflight--events-of-type
                     events "decision.snapshot.recorded"))
                   (requests
                    (semuse-v2a-preflight--events-of-type events "tool.requested"))
                   (starts
                    (semuse-v2a-preflight--events-of-type
                     events "tool.execution.started"))
                   (completed
                    (semuse-v2a-preflight--events-of-type events "tool.completed"))
                   (origin-invalid
                    (cl-count-if
                     (lambda (event)
                       (and (equal "output.first.recorded"
                                   (plist-get event :type))
                            (equal "tool-origin-invalid"
                                   (plist-get (plist-get event :payload) :kind))))
                     events))
                   (decision (car decisions))
                   (envelope
                    (and decision
                         (semuse-v2a-preflight--prepared-envelope decision)))
                   (message
                    (and envelope (aref (plist-get envelope :messages) 0)))
                   (tools (and envelope (plist-get envelope :tools)))
                   (options (and envelope (plist-get envelope :options)))
                   (request (car requests))
                   (payload (and request (plist-get request :payload)))
                   (call-id (and payload (plist-get payload :tool-call-id)))
                   (ok
                    (and (= 1 (length decisions))
                         (equal "initial"
                                (plist-get (plist-get decision :payload)
                                           :attempt-reason))
                         (equal semuse-v2a-preflight--provider
                                (plist-get (plist-get decision :payload)
                                           :provider))
                         (equal semuse-v2a-preflight--model
                                (plist-get (plist-get decision :payload) :model))
                         (equal semuse-v2a-preflight--prompt
                                (plist-get message :content))
                         (= 1 (length tools))
                         (equal "refresh_handle"
                                (plist-get (aref tools 0) :name))
                         (= semuse-v2a-preflight--max-tokens
                            (plist-get options :max-tokens))
                         (equal "none" (plist-get options :reasoning))
                         (null (plist-get options :non-standard-params))
                         (= 1 (length requests))
                         (= 1 (length starts))
                         (= 1 (length completed))
                         (= 1 (plist-get payload :evidence-attempt))
                         (stringp call-id)
                         (not (string-empty-p call-id))
                         (equal "refresh_handle" (plist-get payload :tool-name))
                         (equal 'failed status))))
              (setq audit
                    (list :ok ok :status status
                          :duration-ms
                          (round (* 1000 (- (float-time) started)))
                          :tape-row-count (length events)
                          :decision-count (length decisions)
                          :tool-request-count (length requests)
                          :tool-start-count (length starts)
                          :tool-completed-count (length completed)
                          :tool-call-id call-id
                          :tool-origin-invalid-count origin-invalid
                          :event-types (mapcar (lambda (event)
                                                 (plist-get event :type))
                                               events))))))
          (error
           (setq audit
                 (list :ok nil :status 'audit-error
                       :audit-error (error-message-string err)))))
      (superchat-db-close)
      (when-let* ((buffer (get-buffer " *semuse-v2a-transport-preflight*")))
        (kill-buffer buffer)))
    (setq audit (plist-put audit :evidence-directory directory))
    audit))

(defun semuse-v2a-preflight--print-audit (audit)
  "Print stable AUDIT output."
  (princ (format "preflight=%s\n" (if (plist-get audit :ok) "PASS" "FAIL")))
  (dolist (key '(:status :duration-ms :tape-row-count :decision-count
                 :tool-request-count :tool-start-count :tool-completed-count
                 :tool-call-id :tool-origin-invalid-count :event-types :audit-error
                 :evidence-directory))
    (princ (format "%s=%S\n" (substring (symbol-name key) 1)
                   (plist-get audit key)))))

(pcase command-line-args-left
  (`("contract")
   (semuse-v2a-preflight--print-contract)
   (kill-emacs 0))
  (`("run")
   (let ((audit (semuse-v2a-preflight--run)))
     (semuse-v2a-preflight--print-audit audit)
     (kill-emacs (if (plist-get audit :ok) 0 1))))
  (_
   (princ "usage: transport-preflight-v2a.el contract|run\n")
   (kill-emacs 2)))

;;; transport-preflight-v2a.el ends here
