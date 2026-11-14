;;; llm-pi-ai-utils-error-body-test.el --- Tests for llm-pi-ai-utils-error-body  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-error-body.el', translated from
;; upstream `test/error-body.test.ts' (the describe blocks
;; `normalizeProviderError' and `formatProviderError', in order), with
;; the SDK-shaped errors written as the plists the file's commentary
;; defines, and a JSON body as a hash table.  The last tests, marked
;; "node", have expected values taken by running the source's
;; `truncateErrorText' and `safeJsonStringify' under node 22.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-error-body)

(defun llm-pi-ai-error-body-test--object (&rest pairs)
  "Return a hash table of PAIRS, key then value, in order."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (car pairs) (cadr pairs) table)
      (setq pairs (cddr pairs)))
    table))

(defmacro llm-pi-ai-error-body-test--norm (error)
  "Normalize ERROR and return its parts as a list."
  `(let ((norm (llm-pi-ai-normalize-provider-error ,error)))
     (list (llm-pi-ai-normalized-provider-error-status norm)
           (llm-pi-ai-normalized-provider-error-body norm)
           (llm-pi-ai-normalized-provider-error-message norm)
           (llm-pi-ai-normalized-provider-error-message-carries-body norm))))

(ert-deftest llm-pi-ai-utils-error-body-test-mistral-shape ()
  "Status and body from a Mistral-shaped error."
  (should (equal (llm-pi-ai-error-body-test--norm
                  '(:message "Mistral request failed" :status-code 403
                    :body "{\"error\":\"blocked by gateway WAF\"}"))
                 '(403 "{\"error\":\"blocked by gateway WAF\"}"
                       "Mistral request failed" nil))))

(ert-deftest llm-pi-ai-utils-error-body-test-openai-parsed-body ()
  "The parsed body off an openai APIError when the message is opaque."
  (should (equal (llm-pi-ai-error-body-test--norm
                  (list :message "403 status code (no body)" :status 403
                        :error (llm-pi-ai-error-body-test--object
                                "error" "blocked by gateway WAF")))
                 '(403 "{\"error\":\"blocked by gateway WAF\"}"
                       "403 status code (no body)" nil))))

(ert-deftest llm-pi-ai-utils-error-body-test-message-already-has-body ()
  "The message is kept when @google/genai folded the body into it."
  (let* ((body "{\"error\":{\"code\":403,\"message\":\"Permission denied\"}}")
         (result (llm-pi-ai-error-body-test--norm
                  (list :message body :status 403))))
    (should (equal (nth 0 result) 403))
    (should (eq (nth 3 result) t))
    (should (equal (nth 2 result) body))))

(ert-deftest llm-pi-ai-utils-error-body-test-bedrock-shape ()
  "Status and body from a Bedrock-shaped ServiceException."
  (should (equal (llm-pi-ai-error-body-test--norm
                  '(:message "UnknownError"
                    :metadata (:http-status-code 403)
                    :response (:status-code 403
                               :body "{\"message\":\"blocked by gateway WAF\"}")))
                 '(403 "{\"message\":\"blocked by gateway WAF\"}"
                       "UnknownError" nil))))

(ert-deftest llm-pi-ai-utils-error-body-test-ignores-stream ()
  "A Bedrock response stream is not serialized."
  (let* ((stream (llm-pi-ai-error-body-test--object
                  "pipe" #'ignore "_events" (llm-pi-ai-error-body-test--object)))
         (result (llm-pi-ai-error-body-test--norm
                  (list :message "Invocation of model ID x with on-demand throughput isn't supported."
                        :metadata '(:http-status-code 400)
                        :response (list :status-code 400 :body stream)))))
    (should (equal (nth 0 result) 400))
    (should (null (nth 1 result)))
    (should (string-search "on-demand throughput isn't supported" (nth 2 result)))
    (should (eq (nth 3 result) t))))

(ert-deftest llm-pi-ai-utils-error-body-test-ignores-class-instance-body ()
  "A body that is no plain object (a class instance) is not serialized."
  (let ((result (llm-pi-ai-error-body-test--norm
                 (list :message "Input is too long for requested model."
                       :metadata '(:http-status-code 400)
                       :response (list :status-code 400
                                       :body (record 'sdk-http-response-body nil))))))
    (should (equal (nth 0 result) 400))
    (should (null (nth 1 result)))
    (should (string-search "Input is too long" (nth 2 result)))
    (should (eq (nth 3 result) t)))
  (let ((result (llm-pi-ai-error-body-test--norm
                 (list :message "TLS handshake failed" :status 502
                       :error (record 'sdk-inner-error "EPROTO")))))
    (should (null (nth 1 result)))
    (should (equal (nth 2 result) "TLS handshake failed"))
    (should (eq (nth 3 result) t))))

(ert-deftest llm-pi-ai-utils-error-body-test-plain-parsed-body ()
  "A plain parsed JSON body object still surfaces."
  (let ((result (llm-pi-ai-error-body-test--norm
                 (list :message "400 status code (no body)" :status 400
                       :error (llm-pi-ai-error-body-test--object
                               "message" "schema validation failed"
                               "field" "tools[0]")))))
    (should (equal (nth 1 result)
                   "{\"message\":\"schema validation failed\",\"field\":\"tools[0]\"}"))
    (should (null (nth 3 result)))))

(ert-deftest llm-pi-ai-utils-error-body-test-non-error-value ()
  "A thrown value that is no error is shown as JSON."
  (should (equal (llm-pi-ai-error-body-test--norm
                  (llm-pi-ai-error-body-test--object "reason" "boom"))
                 '(nil nil "{\"reason\":\"boom\"}" nil)))
  (should (equal (llm-pi-ai-error-body-test--norm "just text")
                 '(nil nil "\"just text\"" nil))))

(ert-deftest llm-pi-ai-utils-error-body-test-lisp-error ()
  "A Lisp error condition is an Error with its message."
  (should (equal (llm-pi-ai-error-body-test--norm '(error "it broke"))
                 '(nil nil "it broke" t))))

(ert-deftest llm-pi-ai-utils-error-body-test-empty-parsed-body ()
  "An empty parsed body object is no body."
  (let ((result (llm-pi-ai-error-body-test--norm
                 (list :message "403 status code (no body)" :status 403
                       :error (llm-pi-ai-error-body-test--object)))))
    (should (null (nth 1 result)))
    (should (eq (nth 3 result) t))))

(ert-deftest llm-pi-ai-utils-error-body-test-truncates-body ()
  "The body is cut at the cap."
  (let* ((long (make-string (+ llm-pi-ai-max-provider-error-body-chars 50) ?x))
         (result (llm-pi-ai-error-body-test--norm
                  (list :message "failed" :status-code 500 :body long))))
    (should (string-search "... [truncated 50 chars]" (nth 1 result)))
    (should (< (length (nth 1 result)) (length long)))))

(ert-deftest llm-pi-ai-utils-error-body-test-message-carries-body ()
  "The flag is set when the message already contains the body."
  (should (eq (nth 3 (llm-pi-ai-error-body-test--norm
                      '(:message "500: upstream exploded" :status-code 500
                        :body "upstream exploded")))
              t)))

(ert-deftest llm-pi-ai-utils-error-body-test-format ()
  "`llm-pi-ai-format-provider-error' composes status, body and prefix."
  (let ((blocked (llm-pi-ai-normalize-provider-error
                  (list :message "403 status code (no body)" :status 403
                        :error (llm-pi-ai-error-body-test--object
                                "error" "blocked by gateway WAF")))))
    (let ((formatted (llm-pi-ai-format-provider-error blocked)))
      (should (string-search "403" formatted))
      (should (string-search "blocked by gateway WAF" formatted))
      (should-not (equal formatted "403 status code (no body)")))
    (should (equal (llm-pi-ai-format-provider-error blocked "OpenAI API error")
                   "OpenAI API error (403): {\"error\":\"blocked by gateway WAF\"}")))
  (let* ((body "{\"error\":{\"message\":\"Permission denied\"}}")
         (norm (llm-pi-ai-normalize-provider-error
                (list :message body :status 403))))
    (should (equal (llm-pi-ai-format-provider-error norm "OpenAI API error")
                   (format "OpenAI API error (403): %s" body))))
  (should (equal (llm-pi-ai-format-provider-error
                  (llm-pi-ai-normalize-provider-error
                   (llm-pi-ai-error-body-test--object "reason" "boom")))
                 "{\"reason\":\"boom\"}")))

(ert-deftest llm-pi-ai-utils-error-body-test-truncate-text-node ()
  "Truncation counts UTF-16 units, as the source does (node)."
  ;; truncateErrorText("abcdef", 4)
  (should (equal (llm-pi-ai-truncate-error-text "abcdef" 4)
                 "abcd... [truncated 2 chars]"))
  (should (equal (llm-pi-ai-truncate-error-text "abcd" 4) "abcd"))
  ;; truncateErrorText("a\u{1F648}b\u{1F648}cd", 3) => "a🙈... [truncated 5 chars]"
  (should (equal (llm-pi-ai-truncate-error-text "a\U0001F648b\U0001F648cd" 3)
                 "a\U0001F648... [truncated 5 chars]"))
  ;; truncateErrorText("a\u{1F648}b\u{1F648}cd", 2) => "a\ud83d... [truncated 6 chars]"
  (should (equal (llm-pi-ai-truncate-error-text "a\U0001F648b\U0001F648cd" 2)
                 (concat "a" (string #xD83D) "... [truncated 6 chars]")))
  ;; truncateErrorText("🙈🙈🙈", 6) => unchanged
  (should (equal (llm-pi-ai-truncate-error-text (make-string 3 #x1F648) 6)
                 (make-string 3 #x1F648)))
  ;; truncateErrorText("🙈🙈🙈", 5) => "🙈🙈\ud83d... [truncated 1 chars]"
  (should (equal (llm-pi-ai-truncate-error-text (make-string 3 #x1F648) 5)
                 (concat (make-string 2 #x1F648) (string #xD83D)
                         "... [truncated 1 chars]"))))

(ert-deftest llm-pi-ai-utils-error-body-test-safe-json-stringify-node ()
  "JSON text of values, as `JSON.stringify' writes them (node)."
  ;; safeJsonStringify("a\"b") => "\"a\\\"b\""
  (should (equal (llm-pi-ai-safe-json-stringify "a\"b") "\"a\\\"b\""))
  ;; safeJsonStringify(5) => "5"
  (should (equal (llm-pi-ai-safe-json-stringify 5) "5"))
  ;; safeJsonStringify(undefined) => "undefined"; null => "null"
  (should (equal (llm-pi-ai-safe-json-stringify nil) "undefined"))
  (should (equal (llm-pi-ai-safe-json-stringify :null) "null"))
  ;; safeJsonStringify([1,"x",{a:null,b:true}])
  (should (equal (llm-pi-ai-safe-json-stringify
                  (vector 1 "x" (llm-pi-ai-error-body-test--object
                                 "a" :null "b" t)))
                 "[1,\"x\",{\"a\":null,\"b\":true}]"))
  ;; safeJsonStringify({a:1.5,b:"é\n"})
  (should (equal (llm-pi-ai-safe-json-stringify
                  (llm-pi-ai-error-body-test--object "a" 1.5 "b" "é\n"))
                 "{\"a\":1.5,\"b\":\"é\\n\"}")))

(provide 'llm-pi-ai-utils-error-body-test)
;;; llm-pi-ai-utils-error-body-test.el ends here
