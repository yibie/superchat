;;; superchat-bridge-provider-deepseek-test.el --- Tests for superchat-bridge-provider-deepseek  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-provider-deepseek.el', its port of
;; `src/providers/deepseek.ts'.  Upstream has no test for the DeepSeek
;; provider alone; these follow its behaviour, from a Models collection
;; holding the provider to a stream: model lookup, API key resolution
;; (stored credential, then DEEPSEEK_API_KEY), and the call of the API
;; with the resolved auth and options.  The API is a stub that returns
;; a canned event stream; no test touches the network or a real key,
;; and each binds `process-environment'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-bridge-provider-deepseek)

(defvar superchat-bridge-provider-deepseek-test--calls nil
  "Calls (MODEL CONTEXT OPTIONS) the stub API received, oldest first.")

(defun superchat-bridge-provider-deepseek-test--message (model)
  "Return a finished assistant message of MODEL."
  (superchat-bridge-make-assistant-message
   :content (list (superchat-bridge-make-text-content :text "hello"))
   :api (superchat-bridge-model-api model)
   :provider (superchat-bridge-model-provider model)
   :model (superchat-bridge-model-id model)
   :usage (superchat-bridge-make-usage :input 10 :output 5 :total-tokens 15
                                :cost (superchat-bridge-make-cost))
   :stop-reason "stop"
   :timestamp (round (* 1000 (float-time)))))

(defun superchat-bridge-provider-deepseek-test--stream (model)
  "Return a complete canned stream for MODEL: start, text_delta, done."
  (let ((stream (superchat-bridge-create-assistant-message-event-stream))
        (message (superchat-bridge-provider-deepseek-test--message model)))
    (dolist (event (list (superchat-bridge-make-event :type "start" :partial message)
                         (superchat-bridge-make-event :type "text_delta"
                                               :content-index 0 :delta "hello"
                                               :partial message)
                         (superchat-bridge-make-event :type "done" :reason "stop"
                                               :message message)))
      (superchat-bridge-event-stream-push stream event))
    (superchat-bridge-event-stream-end stream message)
    stream))

(defun superchat-bridge-provider-deepseek-test--stub-api ()
  "Return a stub API that records its calls."
  (let ((respond (lambda (model context options)
                   (setq superchat-bridge-provider-deepseek-test--calls
                         (append superchat-bridge-provider-deepseek-test--calls
                                 (list (list model context options))))
                   (superchat-bridge-provider-deepseek-test--stream model))))
    (superchat-bridge-make-provider-streams :stream respond :stream-simple respond)))

(defun superchat-bridge-provider-deepseek-test--models (&optional credentials)
  "Return a Models collection holding the DeepSeek provider with the stub API."
  (let ((models (superchat-bridge-create-models :credentials credentials)))
    (superchat-bridge-models-set-provider
     models (superchat-bridge-deepseek-provider
             (superchat-bridge-provider-deepseek-test--stub-api)))
    models))

(defun superchat-bridge-provider-deepseek-test--context ()
  "Return a context with a system prompt and one user message."
  (superchat-bridge-make-context
   :system-prompt "be brief"
   :messages (list (superchat-bridge-make-user-message :content "hi" :timestamp 1))))

(defmacro superchat-bridge-provider-deepseek-test--with-env (vars &rest body)
  "Run BODY with a process environment of exactly VARS and no recorded calls."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence ,vars))
         (superchat-bridge-provider-deepseek-test--calls nil))
     ,@body))

;;; The provider

(ert-deftest superchat-bridge-provider-deepseek-test-provider-fields ()
  (let ((provider (superchat-bridge-deepseek-provider
                   (superchat-bridge-provider-deepseek-test--stub-api))))
    (should (equal (superchat-bridge-provider-id provider) "deepseek"))
    (should (equal (superchat-bridge-provider-name provider) "DeepSeek"))
    (should (equal (superchat-bridge-provider-base-url provider) "https://api.deepseek.com"))
    (should (equal (superchat-bridge-api-key-auth-name
                    (superchat-bridge-provider-auth-api-key
                     (superchat-bridge-provider-auth provider)))
                   "DeepSeek API key"))
    (should (equal (mapcar #'superchat-bridge-model-id
                           (funcall (superchat-bridge-provider-get-models provider)))
                   '("deepseek-flash" "deepseek-v4-pro")))))

(ert-deftest superchat-bridge-provider-deepseek-test-models-catalog ()
  (let ((models (superchat-bridge-deepseek-models)))
    (should (equal (mapcar #'car models) '("deepseek-flash" "deepseek-v4-pro")))
    ;; read once
    (should (eq models (superchat-bridge-deepseek-models)))))

(ert-deftest superchat-bridge-provider-deepseek-test-lookup-by-provider-and-id ()
  (let* ((models (superchat-bridge-provider-deepseek-test--models))
         (flash (superchat-bridge-models-get-model models "deepseek" "deepseek-flash")))
    (should (superchat-bridge-model-p flash))
    (should (equal (superchat-bridge-model-id flash) "deepseek-flash"))
    (should (superchat-bridge-has-api flash "openai-completions"))
    (should-not (superchat-bridge-models-get-model models "deepseek" "deepseek-v4-flash"))
    (should-not (superchat-bridge-models-get-model models "openai" "deepseek-flash"))))

(ert-deftest superchat-bridge-provider-deepseek-test-model-functions ()
  "Thinking levels and cost of the catalog's models, through the ported functions."
  (let* ((models (superchat-bridge-provider-deepseek-test--models))
         (flash (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
         (pro (superchat-bridge-models-get-model models "deepseek" "deepseek-v4-pro")))
    (should (equal (superchat-bridge-get-supported-thinking-levels flash)
                   '("off" "low" "high" "max")))
    (should (equal (superchat-bridge-clamp-thinking-level flash "medium") "high"))
    (should (equal (superchat-bridge-clamp-thinking-level flash "minimal") "low"))
    (should (equal (superchat-bridge-clamp-thinking-level flash "xhigh") "max"))
    (should (equal (superchat-bridge-get-supported-thinking-levels pro)
                   '("off" "high" "max")))
    (should (superchat-bridge-models-are-equal flash flash))
    (should-not (superchat-bridge-models-are-equal flash pro))
    ;; 1M input tokens at 0.3 dollars per million, 1M output at 1.2
    (let* ((usage (superchat-bridge-make-usage :input 1000000 :output 1000000
                                        :cost (superchat-bridge-make-cost)))
           (cost (superchat-bridge-calculate-cost flash usage)))
      (should (= (superchat-bridge-cost-input cost) 0.3))
      (should (= (superchat-bridge-cost-output cost) 1.2))
      (should (< (abs (- (superchat-bridge-cost-total cost) 1.5)) 1e-9)))))

;;; From the model to a stream

(ert-deftest superchat-bridge-provider-deepseek-test-stream-simple-with-env-key ()
  "No stored credential: DEEPSEEK_API_KEY resolves and reaches the API."
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
						     (stream (superchat-bridge-models-stream-simple
							      models model (superchat-bridge-provider-deepseek-test--context)
							      '(:reasoning "high"))))
						(should (equal (mapcar #'superchat-bridge-assistant-message-event-type
								       (superchat-bridge-event-stream-to-list stream))
							       '("start" "text_delta" "done")))
						(let ((message (superchat-bridge-event-stream-result stream)))
						  (should (equal (superchat-bridge-assistant-message-stop-reason message) "stop"))
						  (should (numberp (superchat-bridge-assistant-message-duration-ms message))))
						(should (= (length superchat-bridge-provider-deepseek-test--calls) 1))
						(pcase-let ((`(,call-model ,context ,options)
							     (car superchat-bridge-provider-deepseek-test--calls)))
						  (should (equal (superchat-bridge-model-id call-model) "deepseek-flash"))
						  (should (equal (superchat-bridge-model-base-url call-model)
								 "https://api.deepseek.com"))
						  (should (equal (plist-get options :api-key) "sk-env"))
						  (should (equal (plist-get options :reasoning) "high"))
						  ;; the API gets a transcript with the system prompt as first message
						  (should (equal (mapcar #'superchat-bridge-message-role
									 (superchat-bridge-context-messages context))
								 '("system" "user")))))))

(ert-deftest superchat-bridge-provider-deepseek-test-stream-with-env-key ()
  "`stream' calls the API's `stream' in the same way."
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-v4-pro"))
						     (stream (superchat-bridge-models-stream
							      models model (superchat-bridge-provider-deepseek-test--context))))
						(superchat-bridge-event-stream-to-list stream)
						(should (equal (superchat-bridge-assistant-message-stop-reason
								(superchat-bridge-event-stream-result stream))
							       "stop"))
						(should (equal (plist-get (nth 2 (car superchat-bridge-provider-deepseek-test--calls))
									  :api-key)
							       "sk-env")))))

(ert-deftest superchat-bridge-provider-deepseek-test-no-key-is-an-error-stream ()
  (superchat-bridge-provider-deepseek-test--with-env nil
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
						     (stream (superchat-bridge-models-stream-simple
							      models model (superchat-bridge-provider-deepseek-test--context)))
						     (events (superchat-bridge-event-stream-to-list stream))
						     (message (superchat-bridge-event-stream-result stream)))
						(should (equal (mapcar #'superchat-bridge-assistant-message-event-type events)
							       '("error")))
						(should (equal (superchat-bridge-assistant-message-error-message message)
							       "Provider is not configured: deepseek"))
						(should-not superchat-bridge-provider-deepseek-test--calls))))

(ert-deftest superchat-bridge-provider-deepseek-test-blank-env-key-is-no-key ()
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=   ")
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
						     (message (superchat-bridge-event-stream-result
							       (superchat-bridge-models-stream-simple
								models model (superchat-bridge-provider-deepseek-test--context)))))
						(should (equal (superchat-bridge-assistant-message-stop-reason message) "error")))))

(ert-deftest superchat-bridge-provider-deepseek-test-stored-credential-beats-env ()
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((credentials (superchat-bridge-make-in-memory-credential-store))
						     (models (superchat-bridge-provider-deepseek-test--models credentials))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash")))
						(funcall (superchat-bridge-credential-store-modify credentials)
							 "deepseek" (lambda (_c) '(:type api_key :key "sk-stored")) nil)
						(superchat-bridge-models-stream-simple
						 models model (superchat-bridge-provider-deepseek-test--context))
						(should (equal (plist-get (nth 2 (car superchat-bridge-provider-deepseek-test--calls))
									  :api-key)
							       "sk-stored"))
						(should (equal (plist-get (superchat-bridge-models-get-auth models "deepseek")
									  :source)
							       "stored credential")))))

(ert-deftest superchat-bridge-provider-deepseek-test-explicit-api-key-wins ()
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash")))
						(superchat-bridge-models-stream-simple
						 models model (superchat-bridge-provider-deepseek-test--context)
						 '(:api-key "sk-explicit"))
						(should (equal (plist-get (nth 2 (car superchat-bridge-provider-deepseek-test--calls))
									  :api-key)
							       "sk-explicit")))))

(ert-deftest superchat-bridge-provider-deepseek-test-env-source-is-the-variable ()
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (superchat-bridge-provider-deepseek-test--models))
						     (result (superchat-bridge-models-get-auth models "deepseek")))
						(should (equal (plist-get result :source) "DEEPSEEK_API_KEY"))
						(should (equal (plist-get (plist-get result :auth) :api-key) "sk-env")))))

;;; The default API: the plug-in point

(ert-deftest superchat-bridge-provider-deepseek-test-default-api-calls-the-openai-completions-functions ()
  "Without an API of its own the provider calls the two functions of
`superchat-bridge-api-openai-completions' after requiring that feature."
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let ((which nil)
						    (provided (featurep 'superchat-bridge-api-openai-completions)))
						;; the feature is provided here, so that `require' loads no file
						(unwind-protect
						    (progn
						      (provide 'superchat-bridge-api-openai-completions)
						      (cl-letf (((symbol-function 'superchat-bridge-api-openai-completions-stream)
								 (lambda (model _context _options)
								   (push 'stream which)
								   (superchat-bridge-provider-deepseek-test--stream model)))
								((symbol-function 'superchat-bridge-api-openai-completions-stream-simple)
								 (lambda (model _context options)
								   (push (list 'stream-simple (plist-get options :api-key)) which)
								   (superchat-bridge-provider-deepseek-test--stream model))))
							(let* ((models (superchat-bridge-create-models))
							       (model nil))
							  (superchat-bridge-models-set-provider models (superchat-bridge-deepseek-provider))
							  (setq model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
							  (let ((stream (superchat-bridge-models-stream-simple
									 models model (superchat-bridge-provider-deepseek-test--context))))
							    (should (equal (mapcar #'superchat-bridge-assistant-message-event-type
										   (superchat-bridge-event-stream-to-list stream))
									   '("start" "text_delta" "done"))))
							  (superchat-bridge-models-stream
							   models model (superchat-bridge-provider-deepseek-test--context))
							  (should (equal (nreverse which)
									 '((stream-simple "sk-env") stream))))))
						  (unless provided
						    (setq features (delq 'superchat-bridge-api-openai-completions features)))))))

(ert-deftest superchat-bridge-provider-deepseek-test-default-api-missing-is-an-error-stream ()
  "While the API file does not exist, a request ends in an error event."
  (skip-unless (not (locate-library "superchat-bridge-api-openai-completions")))
  (superchat-bridge-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (superchat-bridge-create-models))
						     (model nil))
						(superchat-bridge-models-set-provider models (superchat-bridge-deepseek-provider))
						(setq model (superchat-bridge-models-get-model models "deepseek" "deepseek-flash"))
						(let ((message (superchat-bridge-event-stream-result
								(superchat-bridge-models-stream-simple
								 models model (superchat-bridge-provider-deepseek-test--context)))))
						  (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
						  (should (string-search "superchat-bridge-api-openai-completions"
									 (superchat-bridge-assistant-message-error-message
									  message)))))))

(provide 'superchat-bridge-provider-deepseek-test)

;;; superchat-bridge-provider-deepseek-test.el ends here
