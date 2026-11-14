;;; llm-pi-ai-provider-deepseek-test.el --- Tests for llm-pi-ai-provider-deepseek  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-provider-deepseek.el', its port of
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
(require 'llm-pi-ai-provider-deepseek)

(defvar llm-pi-ai-provider-deepseek-test--calls nil
  "Calls (MODEL CONTEXT OPTIONS) the stub API received, oldest first.")

(defun llm-pi-ai-provider-deepseek-test--message (model)
  "Return a finished assistant message of MODEL."
  (llm-pi-ai-make-assistant-message
   :content (list (llm-pi-ai-make-text-content :text "hello"))
   :api (llm-pi-ai-model-api model)
   :provider (llm-pi-ai-model-provider model)
   :model (llm-pi-ai-model-id model)
   :usage (llm-pi-ai-make-usage :input 10 :output 5 :total-tokens 15
                                :cost (llm-pi-ai-make-cost))
   :stop-reason "stop"
   :timestamp (round (* 1000 (float-time)))))

(defun llm-pi-ai-provider-deepseek-test--stream (model)
  "Return a complete canned stream for MODEL: start, text_delta, done."
  (let ((stream (llm-pi-ai-create-assistant-message-event-stream))
        (message (llm-pi-ai-provider-deepseek-test--message model)))
    (dolist (event (list (llm-pi-ai-make-event :type "start" :partial message)
                         (llm-pi-ai-make-event :type "text_delta"
                                               :content-index 0 :delta "hello"
                                               :partial message)
                         (llm-pi-ai-make-event :type "done" :reason "stop"
                                               :message message)))
      (llm-pi-ai-event-stream-push stream event))
    (llm-pi-ai-event-stream-end stream message)
    stream))

(defun llm-pi-ai-provider-deepseek-test--stub-api ()
  "Return a stub API that records its calls."
  (let ((respond (lambda (model context options)
                   (setq llm-pi-ai-provider-deepseek-test--calls
                         (append llm-pi-ai-provider-deepseek-test--calls
                                 (list (list model context options))))
                   (llm-pi-ai-provider-deepseek-test--stream model))))
    (llm-pi-ai-make-provider-streams :stream respond :stream-simple respond)))

(defun llm-pi-ai-provider-deepseek-test--models (&optional credentials)
  "Return a Models collection holding the DeepSeek provider with the stub API."
  (let ((models (llm-pi-ai-create-models :credentials credentials)))
    (llm-pi-ai-models-set-provider
     models (llm-pi-ai-deepseek-provider
             (llm-pi-ai-provider-deepseek-test--stub-api)))
    models))

(defun llm-pi-ai-provider-deepseek-test--context ()
  "Return a context with a system prompt and one user message."
  (llm-pi-ai-make-context
   :system-prompt "be brief"
   :messages (list (llm-pi-ai-make-user-message :content "hi" :timestamp 1))))

(defmacro llm-pi-ai-provider-deepseek-test--with-env (vars &rest body)
  "Run BODY with a process environment of exactly VARS and no recorded calls."
  (declare (indent 1))
  `(let ((process-environment (copy-sequence ,vars))
         (llm-pi-ai-provider-deepseek-test--calls nil))
     ,@body))

;;; The provider

(ert-deftest llm-pi-ai-provider-deepseek-test-provider-fields ()
  (let ((provider (llm-pi-ai-deepseek-provider
                   (llm-pi-ai-provider-deepseek-test--stub-api))))
    (should (equal (llm-pi-ai-provider-id provider) "deepseek"))
    (should (equal (llm-pi-ai-provider-name provider) "DeepSeek"))
    (should (equal (llm-pi-ai-provider-base-url provider) "https://api.deepseek.com"))
    (should (equal (llm-pi-ai-api-key-auth-name
                    (llm-pi-ai-provider-auth-api-key
                     (llm-pi-ai-provider-auth provider)))
                   "DeepSeek API key"))
    (should (equal (mapcar #'llm-pi-ai-model-id
                           (funcall (llm-pi-ai-provider-get-models provider)))
                   '("deepseek-flash" "deepseek-v4-pro")))))

(ert-deftest llm-pi-ai-provider-deepseek-test-models-catalog ()
  (let ((models (llm-pi-ai-deepseek-models)))
    (should (equal (mapcar #'car models) '("deepseek-flash" "deepseek-v4-pro")))
    ;; read once
    (should (eq models (llm-pi-ai-deepseek-models)))))

(ert-deftest llm-pi-ai-provider-deepseek-test-lookup-by-provider-and-id ()
  (let* ((models (llm-pi-ai-provider-deepseek-test--models))
         (flash (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash")))
    (should (llm-pi-ai-model-p flash))
    (should (equal (llm-pi-ai-model-id flash) "deepseek-flash"))
    (should (llm-pi-ai-has-api flash "openai-completions"))
    (should-not (llm-pi-ai-models-get-model models "deepseek" "deepseek-v4-flash"))
    (should-not (llm-pi-ai-models-get-model models "openai" "deepseek-flash"))))

(ert-deftest llm-pi-ai-provider-deepseek-test-model-functions ()
  "Thinking levels and cost of the catalog's models, through the ported functions."
  (let* ((models (llm-pi-ai-provider-deepseek-test--models))
         (flash (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
         (pro (llm-pi-ai-models-get-model models "deepseek" "deepseek-v4-pro")))
    (should (equal (llm-pi-ai-get-supported-thinking-levels flash)
                   '("off" "low" "high" "max")))
    (should (equal (llm-pi-ai-clamp-thinking-level flash "medium") "high"))
    (should (equal (llm-pi-ai-clamp-thinking-level flash "minimal") "low"))
    (should (equal (llm-pi-ai-clamp-thinking-level flash "xhigh") "max"))
    (should (equal (llm-pi-ai-get-supported-thinking-levels pro)
                   '("off" "high" "max")))
    (should (llm-pi-ai-models-are-equal flash flash))
    (should-not (llm-pi-ai-models-are-equal flash pro))
    ;; 1M input tokens at 0.3 dollars per million, 1M output at 1.2
    (let* ((usage (llm-pi-ai-make-usage :input 1000000 :output 1000000
                                        :cost (llm-pi-ai-make-cost)))
           (cost (llm-pi-ai-calculate-cost flash usage)))
      (should (= (llm-pi-ai-cost-input cost) 0.3))
      (should (= (llm-pi-ai-cost-output cost) 1.2))
      (should (< (abs (- (llm-pi-ai-cost-total cost) 1.5)) 1e-9)))))

;;; From the model to a stream

(ert-deftest llm-pi-ai-provider-deepseek-test-stream-simple-with-env-key ()
  "No stored credential: DEEPSEEK_API_KEY resolves and reaches the API."
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
						     (stream (llm-pi-ai-models-stream-simple
							      models model (llm-pi-ai-provider-deepseek-test--context)
							      '(:reasoning "high"))))
						(should (equal (mapcar #'llm-pi-ai-assistant-message-event-type
								       (llm-pi-ai-event-stream-to-list stream))
							       '("start" "text_delta" "done")))
						(let ((message (llm-pi-ai-event-stream-result stream)))
						  (should (equal (llm-pi-ai-assistant-message-stop-reason message) "stop"))
						  (should (numberp (llm-pi-ai-assistant-message-duration-ms message))))
						(should (= (length llm-pi-ai-provider-deepseek-test--calls) 1))
						(pcase-let ((`(,call-model ,context ,options)
							     (car llm-pi-ai-provider-deepseek-test--calls)))
						  (should (equal (llm-pi-ai-model-id call-model) "deepseek-flash"))
						  (should (equal (llm-pi-ai-model-base-url call-model)
								 "https://api.deepseek.com"))
						  (should (equal (plist-get options :api-key) "sk-env"))
						  (should (equal (plist-get options :reasoning) "high"))
						  ;; the API gets a transcript with the system prompt as first message
						  (should (equal (mapcar #'llm-pi-ai-message-role
									 (llm-pi-ai-context-messages context))
								 '("system" "user")))))))

(ert-deftest llm-pi-ai-provider-deepseek-test-stream-with-env-key ()
  "`stream' calls the API's `stream' in the same way."
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-v4-pro"))
						     (stream (llm-pi-ai-models-stream
							      models model (llm-pi-ai-provider-deepseek-test--context))))
						(llm-pi-ai-event-stream-to-list stream)
						(should (equal (llm-pi-ai-assistant-message-stop-reason
								(llm-pi-ai-event-stream-result stream))
							       "stop"))
						(should (equal (plist-get (nth 2 (car llm-pi-ai-provider-deepseek-test--calls))
									  :api-key)
							       "sk-env")))))

(ert-deftest llm-pi-ai-provider-deepseek-test-no-key-is-an-error-stream ()
  (llm-pi-ai-provider-deepseek-test--with-env nil
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
						     (stream (llm-pi-ai-models-stream-simple
							      models model (llm-pi-ai-provider-deepseek-test--context)))
						     (events (llm-pi-ai-event-stream-to-list stream))
						     (message (llm-pi-ai-event-stream-result stream)))
						(should (equal (mapcar #'llm-pi-ai-assistant-message-event-type events)
							       '("error")))
						(should (equal (llm-pi-ai-assistant-message-error-message message)
							       "Provider is not configured: deepseek"))
						(should-not llm-pi-ai-provider-deepseek-test--calls))))

(ert-deftest llm-pi-ai-provider-deepseek-test-blank-env-key-is-no-key ()
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=   ")
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
						     (message (llm-pi-ai-event-stream-result
							       (llm-pi-ai-models-stream-simple
								models model (llm-pi-ai-provider-deepseek-test--context)))))
						(should (equal (llm-pi-ai-assistant-message-stop-reason message) "error")))))

(ert-deftest llm-pi-ai-provider-deepseek-test-stored-credential-beats-env ()
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((credentials (llm-pi-ai-make-in-memory-credential-store))
						     (models (llm-pi-ai-provider-deepseek-test--models credentials))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash")))
						(funcall (llm-pi-ai-credential-store-modify credentials)
							 "deepseek" (lambda (_c) '(:type api_key :key "sk-stored")) nil)
						(llm-pi-ai-models-stream-simple
						 models model (llm-pi-ai-provider-deepseek-test--context))
						(should (equal (plist-get (nth 2 (car llm-pi-ai-provider-deepseek-test--calls))
									  :api-key)
							       "sk-stored"))
						(should (equal (plist-get (llm-pi-ai-models-get-auth models "deepseek")
									  :source)
							       "stored credential")))))

(ert-deftest llm-pi-ai-provider-deepseek-test-explicit-api-key-wins ()
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash")))
						(llm-pi-ai-models-stream-simple
						 models model (llm-pi-ai-provider-deepseek-test--context)
						 '(:api-key "sk-explicit"))
						(should (equal (plist-get (nth 2 (car llm-pi-ai-provider-deepseek-test--calls))
									  :api-key)
							       "sk-explicit")))))

(ert-deftest llm-pi-ai-provider-deepseek-test-env-source-is-the-variable ()
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (llm-pi-ai-provider-deepseek-test--models))
						     (result (llm-pi-ai-models-get-auth models "deepseek")))
						(should (equal (plist-get result :source) "DEEPSEEK_API_KEY"))
						(should (equal (plist-get (plist-get result :auth) :api-key) "sk-env")))))

;;; The default API: the plug-in point

(ert-deftest llm-pi-ai-provider-deepseek-test-default-api-calls-the-openai-completions-functions ()
  "Without an API of its own the provider calls the two functions of
`llm-pi-ai-api-openai-completions' after requiring that feature."
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let ((which nil)
						    (provided (featurep 'llm-pi-ai-api-openai-completions)))
						;; the feature is provided here, so that `require' loads no file
						(unwind-protect
						    (progn
						      (provide 'llm-pi-ai-api-openai-completions)
						      (cl-letf (((symbol-function 'llm-pi-ai-api-openai-completions-stream)
								 (lambda (model _context _options)
								   (push 'stream which)
								   (llm-pi-ai-provider-deepseek-test--stream model)))
								((symbol-function 'llm-pi-ai-api-openai-completions-stream-simple)
								 (lambda (model _context options)
								   (push (list 'stream-simple (plist-get options :api-key)) which)
								   (llm-pi-ai-provider-deepseek-test--stream model))))
							(let* ((models (llm-pi-ai-create-models))
							       (model nil))
							  (llm-pi-ai-models-set-provider models (llm-pi-ai-deepseek-provider))
							  (setq model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
							  (let ((stream (llm-pi-ai-models-stream-simple
									 models model (llm-pi-ai-provider-deepseek-test--context))))
							    (should (equal (mapcar #'llm-pi-ai-assistant-message-event-type
										   (llm-pi-ai-event-stream-to-list stream))
									   '("start" "text_delta" "done"))))
							  (llm-pi-ai-models-stream
							   models model (llm-pi-ai-provider-deepseek-test--context))
							  (should (equal (nreverse which)
									 '((stream-simple "sk-env") stream))))))
						  (unless provided
						    (setq features (delq 'llm-pi-ai-api-openai-completions features)))))))

(ert-deftest llm-pi-ai-provider-deepseek-test-default-api-missing-is-an-error-stream ()
  "While the API file does not exist, a request ends in an error event."
  (skip-unless (not (locate-library "llm-pi-ai-api-openai-completions")))
  (llm-pi-ai-provider-deepseek-test--with-env '("DEEPSEEK_API_KEY=sk-env")
					      (let* ((models (llm-pi-ai-create-models))
						     (model nil))
						(llm-pi-ai-models-set-provider models (llm-pi-ai-deepseek-provider))
						(setq model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
						(let ((message (llm-pi-ai-event-stream-result
								(llm-pi-ai-models-stream-simple
								 models model (llm-pi-ai-provider-deepseek-test--context)))))
						  (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
						  (should (string-search "llm-pi-ai-api-openai-completions"
									 (llm-pi-ai-assistant-message-error-message
									  message)))))))

(provide 'llm-pi-ai-provider-deepseek-test)

;;; llm-pi-ai-provider-deepseek-test.el ends here
