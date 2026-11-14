;;; llm-pi-ai-utils-overflow-test.el --- Tests for llm-pi-ai-utils-overflow  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-overflow.el', translated from upstream
;; `test/overflow.test.ts', case by case, in order.  The `it.each'
;; tables become loops.  The cases after the last translated one, marked
;; "port", cover the regexp translation and the pattern list.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-overflow)

(defun llm-pi-ai-overflow-test--error (error-message &optional provider)
  "Make an assistant message that failed with ERROR-MESSAGE."
  (llm-pi-ai-make-assistant-message
   :content nil :api "openai-completions" :provider (or provider "ollama")
   :model "qwen3.5:35b" :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason "error" :error-message error-message :timestamp 0))

(defun llm-pi-ai-overflow-test--length-stop (input cache-read output
                                                   &optional cache-write)
  "Make an assistant message that stopped on `length'."
  (let ((cache-write (or cache-write 0)))
    (llm-pi-ai-make-assistant-message
     :content nil :api "openai-completions" :provider "test-provider"
     :model "test-model"
     :usage (llm-pi-ai-make-usage
             :input input :output output :cache-read cache-read
             :cache-write cache-write
             :total-tokens (+ input cache-read cache-write output)
             :cost (llm-pi-ai-make-cost))
     :stop-reason "length" :timestamp 0)))

(ert-deftest llm-pi-ai-utils-overflow-test-request-size ()
  "Provider-independent request-size overflow (HTTP 413)."
  (dolist (text '("github-copilot API error (413): {\"message\":\"failed to parse request\",\"code\":\"\"}"
                  "custom-provider API error (413): Payload Too Large"
                  "413 {\"error\":{\"message\":\"failed to parse request\",\"code\":\"\"}}"
                  "413"))
    (should (llm-pi-ai-context-overflow-p
             (llm-pi-ai-overflow-test--error text) 1000000))
    (should (llm-pi-ai-context-overflow-p
             (llm-pi-ai-overflow-test--error text)))))

(ert-deftest llm-pi-ai-utils-overflow-test-not-request-size ()
  "Unrelated errors are not request-size overflow."
  (dolist (text '("github-copilot API error (400): {\"message\":\"failed to parse request\",\"code\":\"\"}"
                  "custom-provider API error (429): Too many requests"
                  "github-copilot API error (400): {\"message\":\"failed to parse request: invalid value 413\"}"
                  "custom-provider API error (400): upstream mentioned OpenAI API error (413): invalid input"
                  "custom-provider API error (4130): invalid input"
                  "4130 failed to parse request"))
    (should-not (llm-pi-ai-context-overflow-p
                 (llm-pi-ai-overflow-test--error text)))))

(ert-deftest llm-pi-ai-utils-overflow-test-only-failed-responses ()
  "HTTP-status matching is for failed responses only."
  (let ((message (llm-pi-ai-overflow-test--error
                  "OpenAI API error (413): {\"message\":\"failed to parse request\"}")))
    (setf (llm-pi-ai-assistant-message-stop-reason message) "aborted")
    (should-not (llm-pi-ai-context-overflow-p message))
    (setf (llm-pi-ai-assistant-message-stop-reason message) "stop")
    (should-not (llm-pi-ai-context-overflow-p message))))

(ert-deftest llm-pi-ai-utils-overflow-test-provider-messages ()
  "The provider messages the source lists are overflow."
  (dolist (case '(("400 `prompt too long; exceeded max context length by 100918 tokens`" nil 32768)
                  ("400 {\"code\":\"1261\",\"message\":\"Prompt too long\"}" "zai" 1048576)
                  ("400 {\"code\":\"1261\",\"message\":\"Prompt exceeds max length\"}" "zai" 1048576)
                  ("400 The input (516368 tokens) is longer than the model's context length (262144 tokens)." nil 262144)
                  ("Error: 503 litellm.ServiceUnavailableError: litellm.MidStreamFallbackError: litellm.APIConnectionError: APIConnectionError: OpenAIException - Requested token count exceeds the model's maximum context length of 131072 tokens." nil 131072)
                  ("Error: 400 Input length (265330) exceeds model's maximum context length (262144)." nil 262144)
                  ("Provider returned error: Input length 131393 exceeds the maximum allowed input length of 131040 tokens." nil 131072)
                  ("400 Prompt has 256468 tokens, but the configured context size is 256000 tokens" nil 256000)
                  ("Prompt has 5,958,968 tokens, but the configured context size is 256,000 tokens" nil 256000)))
    (should (llm-pi-ai-context-overflow-p
             (llm-pi-ai-overflow-test--error (nth 0 case) (nth 1 case))
             (nth 2 case)))))

(ert-deftest llm-pi-ai-utils-overflow-test-generic-ollama-not-overflow ()
  "A generic Ollama error is not overflow."
  (should-not (llm-pi-ai-context-overflow-p
               (llm-pi-ai-overflow-test--error "500 `model runner crashed unexpectedly`")
               32768)))

(ert-deftest llm-pi-ai-utils-overflow-test-cerebras-bodyless ()
  "A bodyless 400 is overflow for Cerebras only."
  (should (llm-pi-ai-context-overflow-p
           (llm-pi-ai-overflow-test--error "400 status code (no body)" "cerebras")
           131072))
  (should-not (llm-pi-ai-context-overflow-p
               (llm-pi-ai-overflow-test--error "400 status code (no body)" "opencode-go")
               1000000)))

(ert-deftest llm-pi-ai-utils-overflow-test-throttling-not-overflow ()
  "Throttling, unavailability and rate limits are not overflow."
  (dolist (text '("Throttling error: Too many tokens, please wait before trying again."
                  "Service unavailable: The service is temporarily unavailable."
                  "Rate limit exceeded, please retry after 30 seconds."
                  "Too many requests. Please slow down."))
    (should-not (llm-pi-ai-context-overflow-p
                 (llm-pi-ai-overflow-test--error text) 200000))))

(ert-deftest llm-pi-ai-utils-overflow-test-length-stops ()
  "Length stops: Xiaomi-style overflow and recoverable lengths."
  ;; Xiaomi-style overflow
  (should (llm-pi-ai-context-overflow-p
           (llm-pi-ai-overflow-test--length-stop 58 1048512 0) 1048576))
  ;; a length stop below the desired output limit is recoverable
  (should (llm-pi-ai-recoverable-length-p
           (llm-pi-ai-overflow-test--length-stop 3 253584 16 25554) 128000))
  ;; one that reached the limit is not
  (should-not (llm-pi-ai-recoverable-length-p
               (llm-pi-ai-overflow-test--length-stop 4062 0 1024) 1024))
  ;; zero output is recoverable without context metadata
  (should (llm-pi-ai-recoverable-length-p
           (llm-pi-ai-overflow-test--length-stop 100 0 0) 128000))
  ;; a normal length stop with output is no overflow
  (should-not (llm-pi-ai-context-overflow-p
               (llm-pi-ai-overflow-test--length-stop 1000 0 4096) 200000))
  ;; nor is a zero-output one far below the context
  (should-not (llm-pi-ai-context-overflow-p
               (llm-pi-ai-overflow-test--length-stop 100 0 0) 200000)))

(ert-deftest llm-pi-ai-utils-overflow-test-port-silent-overflow ()
  "Port: case 2 of the source, usage over the window on a normal stop."
  (let ((message (llm-pi-ai-overflow-test--length-stop 1500 0 10)))
    (setf (llm-pi-ai-assistant-message-stop-reason message) "stop")
    (should (llm-pi-ai-context-overflow-p message 1000))
    (should-not (llm-pi-ai-context-overflow-p message 2000))
    (should-not (llm-pi-ai-context-overflow-p message))
    (should-not (llm-pi-ai-context-overflow-p message 0))))

(ert-deftest llm-pi-ai-utils-overflow-test-port-patterns ()
  "Port: every pattern compiles, matches ignoring case, and the list is a copy."
  (let ((patterns (llm-pi-ai-get-overflow-patterns)))
    (should (= (length patterns) 26))
    (dolist (pattern patterns)
      (should (stringp pattern))
      (string-match-p pattern ""))
    (setcar patterns "changed")
    (should-not (equal (car (llm-pi-ai-get-overflow-patterns)) "changed")))
  (should (llm-pi-ai-context-overflow-p
           (llm-pi-ai-overflow-test--error "PROMPT IS TOO LONG: 3 tokens"))))

(provide 'llm-pi-ai-utils-overflow-test)
;;; llm-pi-ai-utils-overflow-test.el ends here
