;;; superchat-bridge-utils-error-body-test.el --- Tests for superchat-bridge-utils-error-body  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-error-body.el', translated from
;; upstream `test/error-body.test.ts' (the describe blocks
;; `normalizeProviderError' and `formatProviderError', in order), with
;; the SDK-shaped errors written as the plists the file's commentary
;; defines, and a JSON body as a hash table.  The last tests, marked
;; "node", have expected values taken by running the source's
;; `truncateErrorText' and `safeJsonStringify' under node 22.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-error-body)

(defun superchat-bridge-error-body-test--object (&rest pairs)
  "Return a hash table of PAIRS, key then value, in order."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (car pairs) (cadr pairs) table)
      (setq pairs (cddr pairs)))
    table))

(defmacro superchat-bridge-error-body-test--norm (error)
  "Normalize ERROR and return its parts as a list."
  `(let ((norm (superchat-bridge-normalize-provider-error ,error)))
     (list (superchat-bridge-normalized-provider-error-status norm)
           (superchat-bridge-normalized-provider-error-body norm)
           (superchat-bridge-normalized-provider-error-message norm)
           (superchat-bridge-normalized-provider-error-message-carries-body norm))))

(ert-deftest superchat-bridge-utils-error-body-test-mistral-shape ()
  "Status and body from a Mistral-shaped error."
  (should (equal (superchat-bridge-error-body-test--norm
                  '(:message "Mistral request failed" :status-code 403
                    :body "{\"error\":\"blocked by gateway WAF\"}"))
                 '(403 "{\"error\":\"blocked by gateway WAF\"}"
                       "Mistral request failed" nil))))

(ert-deftest superchat-bridge-utils-error-body-test-openai-parsed-body ()
  "The parsed body off an openai APIError when the message is opaque."
  (should (equal (superchat-bridge-error-body-test--norm
                  (list :message "403 status code (no body)" :status 403
                        :error (superchat-bridge-error-body-test--object
                                "error" "blocked by gateway WAF")))
                 '(403 "{\"error\":\"blocked by gateway WAF\"}"
                       "403 status code (no body)" nil))))

(ert-deftest superchat-bridge-utils-error-body-test-message-already-has-body ()
  "The message is kept when @google/genai folded the body into it."
  (let* ((body "{\"error\":{\"code\":403,\"message\":\"Permission denied\"}}")
         (result (superchat-bridge-error-body-test--norm
                  (list :message body :status 403))))
    (should (equal (nth 0 result) 403))
    (should (eq (nth 3 result) t))
    (should (equal (nth 2 result) body))))

(ert-deftest superchat-bridge-utils-error-body-test-bedrock-shape ()
  "Status and body from a Bedrock-shaped ServiceException."
  (should (equal (superchat-bridge-error-body-test--norm
                  '(:message "UnknownError"
                    :metadata (:http-status-code 403)
                    :response (:status-code 403
                               :body "{\"message\":\"blocked by gateway WAF\"}")))
                 '(403 "{\"message\":\"blocked by gateway WAF\"}"
                       "UnknownError" nil))))

(ert-deftest superchat-bridge-utils-error-body-test-ignores-stream ()
  "A Bedrock response stream is not serialized."
  (let* ((stream (superchat-bridge-error-body-test--object
                  "pipe" #'ignore "_events" (superchat-bridge-error-body-test--object)))
         (result (superchat-bridge-error-body-test--norm
                  (list :message "Invocation of model ID x with on-demand throughput isn't supported."
                        :metadata '(:http-status-code 400)
                        :response (list :status-code 400 :body stream)))))
    (should (equal (nth 0 result) 400))
    (should (null (nth 1 result)))
    (should (string-search "on-demand throughput isn't supported" (nth 2 result)))
    (should (eq (nth 3 result) t))))

(ert-deftest superchat-bridge-utils-error-body-test-ignores-class-instance-body ()
  "A body that is no plain object (a class instance) is not serialized."
  (let ((result (superchat-bridge-error-body-test--norm
                 (list :message "Input is too long for requested model."
                       :metadata '(:http-status-code 400)
                       :response (list :status-code 400
                                       :body (record 'sdk-http-response-body nil))))))
    (should (equal (nth 0 result) 400))
    (should (null (nth 1 result)))
    (should (string-search "Input is too long" (nth 2 result)))
    (should (eq (nth 3 result) t)))
  (let ((result (superchat-bridge-error-body-test--norm
                 (list :message "TLS handshake failed" :status 502
                       :error (record 'sdk-inner-error "EPROTO")))))
    (should (null (nth 1 result)))
    (should (equal (nth 2 result) "TLS handshake failed"))
    (should (eq (nth 3 result) t))))

(ert-deftest superchat-bridge-utils-error-body-test-plain-parsed-body ()
  "A plain parsed JSON body object still surfaces."
  (let ((result (superchat-bridge-error-body-test--norm
                 (list :message "400 status code (no body)" :status 400
                       :error (superchat-bridge-error-body-test--object
                               "message" "schema validation failed"
                               "field" "tools[0]")))))
    (should (equal (nth 1 result)
                   "{\"message\":\"schema validation failed\",\"field\":\"tools[0]\"}"))
    (should (null (nth 3 result)))))

(ert-deftest superchat-bridge-utils-error-body-test-non-error-value ()
  "A thrown value that is no error is shown as JSON."
  (should (equal (superchat-bridge-error-body-test--norm
                  (superchat-bridge-error-body-test--object "reason" "boom"))
                 '(nil nil "{\"reason\":\"boom\"}" nil)))
  (should (equal (superchat-bridge-error-body-test--norm "just text")
                 '(nil nil "\"just text\"" nil))))

(ert-deftest superchat-bridge-utils-error-body-test-lisp-error ()
  "A Lisp error condition is an Error with its message."
  (should (equal (superchat-bridge-error-body-test--norm '(error "it broke"))
                 '(nil nil "it broke" t))))

(ert-deftest superchat-bridge-utils-error-body-test-empty-parsed-body ()
  "An empty parsed body object is no body."
  (let ((result (superchat-bridge-error-body-test--norm
                 (list :message "403 status code (no body)" :status 403
                       :error (superchat-bridge-error-body-test--object)))))
    (should (null (nth 1 result)))
    (should (eq (nth 3 result) t))))

(ert-deftest superchat-bridge-utils-error-body-test-truncates-body ()
  "The body is cut at the cap."
  (let* ((long (make-string (+ superchat-bridge-max-provider-error-body-chars 50) ?x))
         (result (superchat-bridge-error-body-test--norm
                  (list :message "failed" :status-code 500 :body long))))
    (should (string-search "... [truncated 50 chars]" (nth 1 result)))
    (should (< (length (nth 1 result)) (length long)))))

(ert-deftest superchat-bridge-utils-error-body-test-message-carries-body ()
  "The flag is set when the message already contains the body."
  (should (eq (nth 3 (superchat-bridge-error-body-test--norm
                      '(:message "500: upstream exploded" :status-code 500
                        :body "upstream exploded")))
              t)))

(ert-deftest superchat-bridge-utils-error-body-test-format ()
  "`superchat-bridge-format-provider-error' composes status, body and prefix."
  (let ((blocked (superchat-bridge-normalize-provider-error
                  (list :message "403 status code (no body)" :status 403
                        :error (superchat-bridge-error-body-test--object
                                "error" "blocked by gateway WAF")))))
    (let ((formatted (superchat-bridge-format-provider-error blocked)))
      (should (string-search "403" formatted))
      (should (string-search "blocked by gateway WAF" formatted))
      (should-not (equal formatted "403 status code (no body)")))
    (should (equal (superchat-bridge-format-provider-error blocked "OpenAI API error")
                   "OpenAI API error (403): {\"error\":\"blocked by gateway WAF\"}")))
  (let* ((body "{\"error\":{\"message\":\"Permission denied\"}}")
         (norm (superchat-bridge-normalize-provider-error
                (list :message body :status 403))))
    (should (equal (superchat-bridge-format-provider-error norm "OpenAI API error")
                   (format "OpenAI API error (403): %s" body))))
  (should (equal (superchat-bridge-format-provider-error
                  (superchat-bridge-normalize-provider-error
                   (superchat-bridge-error-body-test--object "reason" "boom")))
                 "{\"reason\":\"boom\"}")))

(ert-deftest superchat-bridge-utils-error-body-test-truncate-text-node ()
  "Truncation counts UTF-16 units, as the source does (node)."
  ;; truncateErrorText("abcdef", 4)
  (should (equal (superchat-bridge-truncate-error-text "abcdef" 4)
                 "abcd... [truncated 2 chars]"))
  (should (equal (superchat-bridge-truncate-error-text "abcd" 4) "abcd"))
  ;; truncateErrorText("a\u{1F648}b\u{1F648}cd", 3) => "a🙈... [truncated 5 chars]"
  (should (equal (superchat-bridge-truncate-error-text "a\U0001F648b\U0001F648cd" 3)
                 "a\U0001F648... [truncated 5 chars]"))
  ;; truncateErrorText("a\u{1F648}b\u{1F648}cd", 2) => "a\ud83d... [truncated 6 chars]"
  (should (equal (superchat-bridge-truncate-error-text "a\U0001F648b\U0001F648cd" 2)
                 (concat "a" (string #xD83D) "... [truncated 6 chars]")))
  ;; truncateErrorText("🙈🙈🙈", 6) => unchanged
  (should (equal (superchat-bridge-truncate-error-text (make-string 3 #x1F648) 6)
                 (make-string 3 #x1F648)))
  ;; truncateErrorText("🙈🙈🙈", 5) => "🙈🙈\ud83d... [truncated 1 chars]"
  (should (equal (superchat-bridge-truncate-error-text (make-string 3 #x1F648) 5)
                 (concat (make-string 2 #x1F648) (string #xD83D)
                         "... [truncated 1 chars]"))))

(ert-deftest superchat-bridge-utils-error-body-test-safe-json-stringify-node ()
  "JSON text of values, as `JSON.stringify' writes them (node)."
  ;; safeJsonStringify("a\"b") => "\"a\\\"b\""
  (should (equal (superchat-bridge-safe-json-stringify "a\"b") "\"a\\\"b\""))
  ;; safeJsonStringify(5) => "5"
  (should (equal (superchat-bridge-safe-json-stringify 5) "5"))
  ;; safeJsonStringify(undefined) => "undefined"; null => "null"
  (should (equal (superchat-bridge-safe-json-stringify nil) "undefined"))
  (should (equal (superchat-bridge-safe-json-stringify :null) "null"))
  ;; safeJsonStringify([1,"x",{a:null,b:true}])
  (should (equal (superchat-bridge-safe-json-stringify
                  (vector 1 "x" (superchat-bridge-error-body-test--object
                                 "a" :null "b" t)))
                 "[1,\"x\",{\"a\":null,\"b\":true}]"))
  ;; safeJsonStringify({a:1.5,b:"é\n"})
  (should (equal (superchat-bridge-safe-json-stringify
                  (superchat-bridge-error-body-test--object "a" 1.5 "b" "é\n"))
                 "{\"a\":1.5,\"b\":\"é\\n\"}")))

(provide 'superchat-bridge-utils-error-body-test)
;;; superchat-bridge-utils-error-body-test.el ends here
