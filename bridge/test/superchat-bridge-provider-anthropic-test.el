;;; superchat-bridge-provider-anthropic-test.el --- Tests for superchat-bridge-provider-anthropic  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-provider-anthropic.el': the auth resolution
;; (translated from upstream `test/anthropic-auth-token.test.ts') and an
;; end to end run through `superchat-bridge-models-stream-simple' with the
;; real provider and API over a fake curl (the rig of
;; `superchat-bridge-api-anthropic-messages-test'): a text answer, a tool
;; call and its result, and a 429 that ends as an error event.  No test
;; touches the network or reads a key.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-bridge)
(require 'superchat-bridge-provider-anthropic)
(require 'superchat-bridge-api-anthropic-messages-test)

(defun superchat-bridge-anthropic-test--ctx (env)
  "Return an auth context whose environment is the alist ENV."
  (superchat-bridge-make-auth-context
   :env (lambda (name) (cdr (assoc name env)))
   :file-exists (lambda (_path) nil)))

(defun superchat-bridge-anthropic-test--resolve (env &optional credential)
  "Resolve the provider's API key auth with ENV and CREDENTIAL."
  (funcall (superchat-bridge-api-key-auth-resolve
            (superchat-bridge-provider-auth-api-key
             (superchat-bridge-provider-auth (superchat-bridge-anthropic-provider))))
           (superchat-bridge-anthropic-test--ctx env) credential
           (superchat-bridge-make-abort-signal)))

(ert-deftest superchat-bridge-anthropic-test-auth-token-is-bearer-header ()
  "(upstream anthropic-auth-token) ANTHROPIC_AUTH_TOKEN wins, as a header."
  (should (equal (superchat-bridge-anthropic-test--resolve
                  '(("ANTHROPIC_AUTH_TOKEN" . "auth-token")
                    ("ANTHROPIC_OAUTH_TOKEN" . "oauth-token")
                    ("ANTHROPIC_API_KEY" . "api-key")))
                 '(:auth (:headers (("Authorization" . "Bearer auth-token")))
                   :source "ANTHROPIC_AUTH_TOKEN"))))

(ert-deftest superchat-bridge-anthropic-test-oauth-token-then-api-key ()
  "(upstream anthropic-auth-token) ANTHROPIC_OAUTH_TOKEN, then the API key."
  (should (equal (superchat-bridge-anthropic-test--resolve
                  '(("ANTHROPIC_OAUTH_TOKEN" . "oauth-token") ("ANTHROPIC_API_KEY" . "api-key")))
                 '(:auth (:api-key "oauth-token") :source "ANTHROPIC_OAUTH_TOKEN")))
  (should (equal (superchat-bridge-anthropic-test--resolve '(("ANTHROPIC_API_KEY" . "api-key")))
                 '(:auth (:api-key "api-key") :source "ANTHROPIC_API_KEY")))
  (should (equal (superchat-bridge-anthropic-test--resolve
                  '(("ANTHROPIC_API_KEY" . "api-key")) '(:type api_key :key "stored"))
                 '(:auth (:api-key "stored") :env nil :source "stored credential")))
  ;; federation is not ported: its variables alone configure nothing
  (should-not (superchat-bridge-anthropic-test--resolve
               '(("ANTHROPIC_FEDERATION_RULE_ID" . "r") ("ANTHROPIC_ORGANIZATION_ID" . "o")
                 ("ANTHROPIC_IDENTITY_TOKEN_FILE" . "/f")))))

(ert-deftest superchat-bridge-anthropic-test-provider-and-models ()
  "The provider's id, base URL and the models of data/anthropic.json."
  (let ((provider (superchat-bridge-anthropic-provider)))
    (should (equal (superchat-bridge-provider-id provider) "anthropic"))
    (should (equal (superchat-bridge-provider-base-url provider) "https://api.anthropic.com"))
    (let ((model (cdr (assoc "claude-opus-4-8" (superchat-bridge-anthropic-models)))))
      (should model)
      (should (equal (superchat-bridge-model-api model) "anthropic-messages"))
      (should (equal (superchat-bridge-model-provider model) "anthropic")))
    (should (>= (length (superchat-bridge-anthropic-models)) 17))))

;;; End to end through Models

(defun superchat-bridge-anthropic-test--models ()
  "Return a Models collection with the Anthropic provider on its real API."
  (let ((models (superchat-bridge-create-models)))
    (superchat-bridge-models-set-provider models (superchat-bridge-anthropic-provider))
    models))

(defun superchat-bridge-anthropic-test--weather-tool ()
  "Return a weather tool."
  (superchat-bridge-make-tool
   :name "get_weather" :description "Weather of a city"
   :parameters '((type . "object") (properties . ((city . ((type . "string")))))
                 (required . ["city"]))))

(ert-deftest superchat-bridge-anthropic-test-auth-token-reaches-request ()
  "(upstream anthropic-auth-token) The env's token is the request's header;
an explicit header overrides it."
  (superchat-bridge-amt-test--with-rig
    (let ((process-environment (list "HOME=/nonexistent" "ANTHROPIC_AUTH_TOKEN=ctx-token"))
          (models (superchat-bridge-anthropic-test--models))
          (model (superchat-bridge-amt-test--model :id "claude-test" :base-url
                                                   "https://api.anthropic.com")))
      (superchat-bridge-models-stream-simple
       models model (superchat-bridge-make-context
                     :system-prompt "System prompt."
                     :messages (list (superchat-bridge-amt-test--user "Hello"))))
      (should (equal (superchat-bridge-amt-test--header "Authorization" 0) "Bearer ctx-token"))
      (should-not (superchat-bridge-amt-test--header "X-Api-Key" 0))
      (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 0)
                                                     "system" 0 "text")
                     "System prompt."))
      (superchat-bridge-models-stream-simple
       models model (superchat-bridge-make-context
                     :messages (list (superchat-bridge-amt-test--user "Hello")))
       '(:headers (("Authorization" . "Bearer explicit-token"))))
      (should (equal (superchat-bridge-amt-test--header "Authorization" 1)
                     "Bearer explicit-token")))))

(ert-deftest superchat-bridge-anthropic-test-end-to-end-text-tool-and-429 ()
  "Through Models: text, a tool call, its result sent back, and a 429."
  (superchat-bridge-amt-test--with-rig
    (let* ((process-environment (list "HOME=/nonexistent" "ANTHROPIC_API_KEY=sk-test"))
           (models (superchat-bridge-anthropic-test--models))
           (model (superchat-bridge-models-get-model models "anthropic" "claude-sonnet-4-5"))
           (question (superchat-bridge-amt-test--user "Weather in Paris?"))
           (context (superchat-bridge-make-context
                     :system-prompt "Be brief."
                     :tools (list (superchat-bridge-anthropic-test--weather-tool))
                     :messages (list question)))
           (deltas nil)
           (stream (superchat-bridge-models-stream-simple models model context)))
      (superchat-bridge-event-stream-iterate
       stream (lambda (event)
                (when (equal (superchat-bridge-assistant-message-event-type event) "text_delta")
                  (push (superchat-bridge-assistant-message-event-delta event) deltas))))
      ;; 1. a text block, then a tool call in two pieces
      (superchat-bridge-amt-test--respond
       (superchat-bridge-amt-test--request 0)
       (superchat-bridge-amt-test--sse-ended
        (list
         (superchat-bridge-amt-test--e
          "message_start"
          '((type . "message_start")
            (message . ((id . "msg_1") (model . "claude-sonnet-4-5")
                        (usage . ((input_tokens . 20) (output_tokens . 0)))))))
         (superchat-bridge-amt-test--e
          "content_block_start"
          '((type . "content_block_start") (index . 0)
            (content_block . ((type . "text") (text . "")))))
         (superchat-bridge-amt-test--e
          "content_block_delta"
          '((type . "content_block_delta") (index . 0)
            (delta . ((type . "text_delta") (text . "Checking.")))))
         (superchat-bridge-amt-test--e
          "content_block_stop" '((type . "content_block_stop") (index . 0)))
         (superchat-bridge-amt-test--e
          "content_block_start"
          '((type . "content_block_start") (index . 1)
            (content_block . ((type . "tool_use") (id . "toolu_1") (name . "get_weather")
                              (input . nil)))))
         (superchat-bridge-amt-test--e
          "content_block_delta"
          '((type . "content_block_delta") (index . 1)
            (delta . ((type . "input_json_delta") (partial_json . "{\"city\":")))))
         (superchat-bridge-amt-test--e
          "content_block_delta"
          '((type . "content_block_delta") (index . 1)
            (delta . ((type . "input_json_delta") (partial_json . "\"Paris\"}")))))
         (superchat-bridge-amt-test--e
          "content_block_stop" '((type . "content_block_stop") (index . 1)))
         (superchat-bridge-amt-test--e
          "message_delta"
          '((type . "message_delta") (delta . ((stop_reason . "tool_use")))
            (usage . ((output_tokens . 9)))))
         (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop"))))))
      (let* ((first (superchat-bridge-event-stream-result stream))
             (call (nth 1 (superchat-bridge-message-content first)))
             (body (superchat-bridge-amt-test--body 0)))
        (should (equal (nreverse deltas) '("Checking.")))
        (should (equal (superchat-bridge-assistant-message-stop-reason first) "toolUse"))
        (should (equal (superchat-bridge-tool-call-id call) "toolu_1"))
        (should (equal (superchat-bridge-tool-call-name call) "get_weather"))
        (should (equal (gethash "city" (superchat-bridge-tool-call-arguments call)) "Paris"))
        (should (= (superchat-bridge-usage-total-tokens
                    (superchat-bridge-assistant-message-usage first))
                   29))
        (should (equal (superchat-bridge-amt-test--header "X-Api-Key" 0) "sk-test"))
        (should (equal (superchat-bridge-amt-test--get body "model") "claude-sonnet-4-5"))
        (should (equal (superchat-bridge-amt-test--get body "thinking") '(("type" . "disabled"))))
        (should (equal (superchat-bridge-amt-test--get body "tools" 0 "name") "get_weather"))
        ;; 2. the tool result goes back with the call
        (let* ((result (superchat-bridge-make-tool-result-message
                        :tool-call-id "toolu_1" :tool-name "get_weather"
                        :content (list (superchat-bridge-make-text-content :text "18C sunny"))
                        :timestamp 3))
               (second (superchat-bridge-models-stream-simple
                        models model
                        (superchat-bridge-make-context
                         :system-prompt "Be brief."
                         :tools (list (superchat-bridge-anthropic-test--weather-tool))
                         :messages (list question first result)))))
          (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1)
                                                         "messages")
                         `[(("content" . "Weather in Paris?") ("role" . "user"))
                           (("content"
                             . [(("text" . "Checking.") ("type" . "text"))
                                (("id" . "toolu_1") ("input" . (("city" . "Paris")))
                                 ("name" . "get_weather") ("type" . "tool_use"))])
                            ("role" . "assistant"))
                           (("content"
                             . [(("cache_control" . (("type" . "ephemeral")))
                                 ("content" . "18C sunny") ("is_error" . :false)
                                 ("tool_use_id" . "toolu_1") ("type" . "tool_result"))])
                            ("role" . "user"))]))
          ;; 3. that request is refused with a 429: an error event, the
          ;; status leading its text, retryable for the caller
          (superchat-bridge-amt-test--respond
           (superchat-bridge-amt-test--request 1)
           (concat "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\","
                   "\"message\":\"Number of requests has exceeded your rate limit\"}}")
           "HTTP/1.1 429 Too Many Requests\r\nretry-after: 0\r\nContent-Type: application/json\r\n\r\n")
          (let ((events (superchat-bridge-event-stream-to-list second))
                (message (superchat-bridge-event-stream-result second)))
            (should (equal (superchat-bridge-amt-test--types events) '("error")))
            (should (equal (superchat-bridge-assistant-message-event-reason (car events)) "error"))
            (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
            (should (string-match-p "\\`429[ :]"
                                    (superchat-bridge-assistant-message-error-message message)))
            (should (string-match-p "rate_limit_error"
                                    (superchat-bridge-assistant-message-error-message message)))
            (should (superchat-bridge-retryable-assistant-error-p message))
            (should (= (length superchat-bridge-amt-test--requests) 2))))))))

(provide 'superchat-bridge-provider-anthropic-test)

;;; superchat-bridge-provider-anthropic-test.el ends here
