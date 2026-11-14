;;; llm-pi-ai-api-openai-completions-test.el --- Tests for llm-pi-ai-api-openai-completions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-api-openai-completions.el'.  Upstream's
;; `test/openai-completions-*.test.ts' replace the `openai' client with a
;; fake; these replace curl with a fake start function
;; (`llm-pi-ai-http-start-function') that records the request and is fed
;; bytes in pieces.  Retry delays go through the fake timers of
;; `llm-pi-ai-utils-retry'.  No test touches the network or reads a key.
;;
;; The end-to-end tests go from the real DeepSeek provider through
;; `llm-pi-ai-models-stream-simple'.  Expected request bodies are written
;; as JSON text, key order included: the order is what a provider gets.
;; Tests marked "(upstream NAME)" are translated from that test file; the
;; others follow the source's behaviour.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'llm-pi-ai)
(require 'llm-pi-ai-provider-deepseek)
(require 'llm-pi-ai-api-openai-completions)

;;; Rig

(cl-defstruct (llm-pi-ai-occ-test--req (:copier nil))
  "A request the fake curl was started for."
  args config stdin output exit (killed 0))

(defvar llm-pi-ai-occ-test--requests nil
  "The requests made so far, oldest first.")

(defvar llm-pi-ai-occ-test--timers nil
  "Fake timers: a list of (SECONDS FUNCTION CANCELLED), oldest first.")

(defmacro llm-pi-ai-occ-test--with-rig (&rest body)
  "Run BODY with a fake curl and fake timers and a clean environment."
  (declare (indent 0))
  `(let* ((process-environment (list "HOME=/nonexistent"))
          (llm-pi-ai-occ-test--requests nil)
          (llm-pi-ai-occ-test--timers nil)
          (llm-pi-ai-retry-run-at-time-function
           (lambda (seconds _repeat function)
             (let ((timer (list seconds function nil)))
               (setq llm-pi-ai-occ-test--timers
                     (append llm-pi-ai-occ-test--timers (list timer)))
               timer)))
          (llm-pi-ai-retry-cancel-timer-function
           (lambda (timer) (setf (nth 2 timer) t)))
          (llm-pi-ai-http-start-function
           (lambda (args config stdin on-output on-exit)
             (let ((req (make-llm-pi-ai-occ-test--req
                         :args args :config config :stdin stdin
                         :output on-output :exit on-exit)))
               (setq llm-pi-ai-occ-test--requests
                     (append llm-pi-ai-occ-test--requests (list req)))
               (lambda () (cl-incf (llm-pi-ai-occ-test--req-killed req)))))))
     ,@body))

(defun llm-pi-ai-occ-test--fire-timer (index)
  "Run the fake timer INDEX (0-based) unless it was cancelled."
  (let ((timer (nth index llm-pi-ai-occ-test--timers)))
    (unless (nth 2 timer)
      (setf (nth 2 timer) t)
      (funcall (nth 1 timer)))))

(defun llm-pi-ai-occ-test--feed (req &rest strings)
  "Feed STRINGS to REQ as pieces of curl's stdout (UTF-8 bytes)."
  (dolist (string strings)
    (funcall (llm-pi-ai-occ-test--req-output req)
             (encode-coding-string string 'utf-8))))

(defun llm-pi-ai-occ-test--feed-pieces (req text)
  "Feed TEXT to REQ cut into pieces of 1 to 13 bytes, cycling."
  (let* ((bytes (encode-coding-string text 'utf-8))
         (start 0) (size 1))
    (while (< start (length bytes))
      (let ((end (min (length bytes) (+ start size))))
        (funcall (llm-pi-ai-occ-test--req-output req) (substring bytes start end))
        (setq start end size (if (>= size 13) 1 (1+ size)))))))

(defun llm-pi-ai-occ-test--exit (req &optional code stderr)
  "End REQ's curl with exit CODE (default 0) and STDERR text."
  (funcall (llm-pi-ai-occ-test--req-exit req) (or code 0) (or stderr "")))

(defconst llm-pi-ai-occ-test--head
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n")

(defun llm-pi-ai-occ-test--data (json)
  "Return one SSE event whose data is the JSON text."
  (concat "data: " json "\n\n"))

(defun llm-pi-ai-occ-test--request (&optional index)
  "Return request INDEX (default 0) of the rig."
  (nth (or index 0) llm-pi-ai-occ-test--requests))

(defun llm-pi-ai-occ-test--body (&optional index)
  "Return the JSON text sent by request INDEX of the rig."
  (llm-pi-ai-occ-test--req-stdin (llm-pi-ai-occ-test--request index)))

(defun llm-pi-ai-occ-test--json (text)
  "Parse the JSON TEXT into hash tables and vectors."
  (json-parse-string text :object-type 'hash-table :array-type 'array
                     :null-object :null :false-object :false))

(defun llm-pi-ai-occ-test--ref (value &rest keys)
  "Return the member of VALUE along KEYS: strings for objects, integers for arrays."
  (dolist (key keys value)
    (setq value (if (stringp key) (gethash key value) (aref value key)))))

(defun llm-pi-ai-occ-test--types (events)
  "Return the types of EVENTS."
  (mapcar #'llm-pi-ai-assistant-message-event-type events))

;;; Models and contexts

(defun llm-pi-ai-occ-test--models ()
  "Return a Models collection with the DeepSeek provider on its real API."
  (let ((models (llm-pi-ai-create-models)))
    (llm-pi-ai-models-set-provider models (llm-pi-ai-deepseek-provider))
    models))

(defun llm-pi-ai-occ-test--deepseek (&optional id)
  "Return the DeepSeek model ID (default \"deepseek-flash\")."
  (llm-pi-ai-models-get-model (llm-pi-ai-occ-test--models) "deepseek"
                              (or id "deepseek-flash")))

(defun llm-pi-ai-occ-test--model (&rest args)
  "Return a chat model on the completions API, ARGS over the defaults."
  (apply #'llm-pi-ai-make-model
         (append args
                 (list :id "test-model" :name "Test Model"
                       :api "openai-completions" :provider "openai"
                       :base-url "https://api.openai.com/v1" :reasoning nil
                       :input '("text") :cost (llm-pi-ai-make-model-cost)
                       :context-window 128000 :max-tokens 4096))))

(defun llm-pi-ai-occ-test--user (text)
  "Return a user message with TEXT."
  (llm-pi-ai-make-user-message :content text :timestamp 1))

(defun llm-pi-ai-occ-test--context (&rest messages)
  "Return a normalized context of MESSAGES, or of one user message."
  (llm-pi-ai-normalize-context
   (llm-pi-ai-make-context
    :messages (or messages (list (llm-pi-ai-occ-test--user "hi"))))))

(defun llm-pi-ai-occ-test--stream (model context options chunks &optional no-done)
  "Stream MODEL and CONTEXT with OPTIONS, feed CHUNKS and return the stream.
CHUNKS are JSON texts, sent as SSE events after the headers, then
`[DONE]' unless NO-DONE, then curl exits 0."
  (let* ((stream (llm-pi-ai-api-openai-completions-stream
                  model context (append (list :api-key "test-key") options)))
         (req (llm-pi-ai-occ-test--request)))
    (apply #'llm-pi-ai-occ-test--feed req llm-pi-ai-occ-test--head
           (append (mapcar #'llm-pi-ai-occ-test--data chunks)
                   (unless no-done (list "data: [DONE]\n\n"))))
    (llm-pi-ai-occ-test--exit req)
    stream))

(defun llm-pi-ai-occ-test--result (stream)
  "Return the final assistant message of STREAM."
  (llm-pi-ai-event-stream-result stream))

(defun llm-pi-ai-occ-test--chunk (delta &optional finish-reason)
  "Return the JSON text of a chunk with DELTA, a JSON text, and FINISH-REASON."
  (format "{\"id\":\"chatcmpl-test\",\"choices\":[{\"index\":0,\"delta\":%s%s}]}"
          delta
          (if finish-reason
              (format ",\"finish_reason\":\"%s\"" finish-reason)
            "")))

;;; Request body, end to end through the DeepSeek provider

(defun llm-pi-ai-occ-test--history-context ()
  "Return a context of the first scenario: prompt, tool, user, tool turn."
  (let ((assistant
         (llm-pi-ai-make-assistant-message
          :content (list (llm-pi-ai-make-thinking-content
                          :thinking "Need two lookups"
                          :thinking-signature "reasoning_content")
                         (llm-pi-ai-make-text-content :text "Checking both.")
                         (llm-pi-ai-make-tool-call
                          :id "call_1" :name "get_weather"
                          :arguments (llm-pi-ai-json-object "city" "Paris"))
                         (llm-pi-ai-make-tool-call
                          :id "call_2" :name "get_weather"
                          :arguments (llm-pi-ai-json-object "city" "Rome")))
          :api "openai-completions" :provider "deepseek" :model "deepseek-flash"
          :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
          :stop-reason "toolUse" :timestamp 2)))
    (llm-pi-ai-make-context
     :system-prompt "Be helpful."
     :tools (list (llm-pi-ai-make-tool
                   :name "get_weather" :description "Get weather"
                   :parameters '((type . "object")
                                 (properties . ((city . ((type . "string")))))
                                 (required . ["city"]))))
     :messages (list (llm-pi-ai-occ-test--user "What is the weather in Paris and Rome?")
                     assistant
                     (llm-pi-ai-make-tool-result-message
                      :tool-call-id "call_1" :tool-name "get_weather"
                      :content (list (llm-pi-ai-make-text-content :text "18C sunny"))
                      :timestamp 3)
                     (llm-pi-ai-make-tool-result-message
                      :tool-call-id "call_2" :tool-name "get_weather"
                      :content (list (llm-pi-ai-make-text-content :text "24C clear"))
                      :timestamp 4)))))

(defconst llm-pi-ai-occ-test--history-messages-json
  (concat
   "\"messages\":["
   "{\"role\":\"system\",\"content\":\"Be helpful.\"},"
   "{\"role\":\"user\",\"content\":\"What is the weather in Paris and Rome?\"},"
   "{\"role\":\"assistant\",\"content\":\"Checking both.\","
   "\"reasoning_content\":\"Need two lookups\","
   "\"tool_calls\":["
   "{\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\","
   "\"arguments\":\"{\\\"city\\\":\\\"Paris\\\"}\"}},"
   "{\"id\":\"call_2\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\","
   "\"arguments\":\"{\\\"city\\\":\\\"Rome\\\"}\"}}]},"
   "{\"role\":\"tool\",\"content\":\"18C sunny\",\"tool_call_id\":\"call_1\"},"
   "{\"role\":\"tool\",\"content\":\"24C clear\",\"tool_call_id\":\"call_2\"}],"
   "\"stream\":true,\"stream_options\":{\"include_usage\":true},\"max_tokens\":384000,"
   "\"tools\":[{\"type\":\"function\",\"function\":{\"name\":\"get_weather\","
   "\"description\":\"Get weather\",\"parameters\":{\"type\":\"object\","
   "\"properties\":{\"city\":{\"type\":\"string\"}},\"required\":[\"city\"]},"
   "\"strict\":false}}]")
  "The part of the request body that does not depend on thinking.")

(ert-deftest llm-pi-ai-occ-test-body-deepseek-reasoning-on ()
  "Exact body: reasoning_content sent back, one assistant message, max_tokens."
  (llm-pi-ai-occ-test--with-rig
    (let ((stream (llm-pi-ai-models-stream-simple
                   (llm-pi-ai-occ-test--models) (llm-pi-ai-occ-test--deepseek)
                   (llm-pi-ai-occ-test--history-context)
                   '(:api-key "test-key" :reasoning "high"))))
      (should (= (length llm-pi-ai-occ-test--requests) 1))
      (should (equal (llm-pi-ai-occ-test--body)
                     (concat "{\"model\":\"deepseek-flash\","
                             llm-pi-ai-occ-test--history-messages-json
                             ",\"thinking\":{\"type\":\"enabled\"},"
                             "\"reasoning_effort\":\"high\"}")))
      (should-not (llm-pi-ai-event-stream-done-p stream)))))

(ert-deftest llm-pi-ai-occ-test-body-deepseek-reasoning-off ()
  "Reasoning off: thinking disabled, no reasoning_effort, same messages."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-models-stream-simple
     (llm-pi-ai-occ-test--models) (llm-pi-ai-occ-test--deepseek)
     (llm-pi-ai-occ-test--history-context) '(:api-key "test-key"))
    (should (equal (llm-pi-ai-occ-test--body)
                   (concat "{\"model\":\"deepseek-flash\","
                           llm-pi-ai-occ-test--history-messages-json
                           ",\"thinking\":{\"type\":\"disabled\"}}")))))

(ert-deftest llm-pi-ai-occ-test-body-deepseek-level-clamped ()
  "\"medium\" is not a level of deepseek-flash: it clamps to \"high\"."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-models-stream-simple
     (llm-pi-ai-occ-test--models) (llm-pi-ai-occ-test--deepseek)
     (llm-pi-ai-occ-test--history-context) '(:api-key "test-key" :reasoning "medium"))
    (let ((body (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body))))
      (should (equal (llm-pi-ai-occ-test--ref body "reasoning_effort") "high")))))

(ert-deftest llm-pi-ai-occ-test-request-headers-and-url ()
  "POST to base-url/chat/completions with bearer auth, JSON content type."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-models-stream-simple
     (llm-pi-ai-occ-test--models) (llm-pi-ai-occ-test--deepseek)
     (llm-pi-ai-occ-test--context) '(:api-key "test-key"))
    (let ((config (llm-pi-ai-occ-test--req-config (llm-pi-ai-occ-test--request))))
      (should (string-match-p "url = \"https://api.deepseek.com/chat/completions\""
                              config))
      (should (string-match-p "request = \"POST\"" config))
      (should (string-match-p "header = \"Authorization: Bearer test-key\"" config))
      (should (string-match-p "header = \"Content-Type: application/json\"" config))
      (should (member "--data-binary" (llm-pi-ai-occ-test--req-args
                                       (llm-pi-ai-occ-test--request)))))))

(ert-deftest llm-pi-ai-occ-test-headers-override-and-remove ()
  "Option headers override defaults without regard to case; nil removes."
  (llm-pi-ai-occ-test--with-rig
    (let ((model (llm-pi-ai-occ-test--model :headers '(("X-Model" . "m")))))
      (llm-pi-ai-api-openai-completions-stream
       model (llm-pi-ai-occ-test--context)
       '(:api-key "k" :headers (("authorization" . "Bearer other")
                                ("accept" . nil) ("X-Extra" . "e")))))
    (let ((config (llm-pi-ai-occ-test--req-config (llm-pi-ai-occ-test--request))))
      (should (string-match-p "header = \"authorization: Bearer other\"" config))
      (should-not (string-match-p "Bearer k" config))
      (should-not (string-match-p "Accept" config))
      (should (string-match-p "header = \"X-Model: m\"" config))
      (should (string-match-p "header = \"X-Extra: e\"" config)))))

(ert-deftest llm-pi-ai-occ-test-authorization-header-replaces-api-key ()
  "No API key but an authorization header: the request is made."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-api-openai-completions-stream
     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context)
     '(:headers (("Authorization" . "Bearer from-header"))))
    (should (string-match-p "Authorization: Bearer from-header"
                            (llm-pi-ai-occ-test--req-config
                             (llm-pi-ai-occ-test--request))))))

(ert-deftest llm-pi-ai-occ-test-no-api-key ()
  "`stream' ends with an error event; `stream-simple' signals, as it throws."
  (llm-pi-ai-occ-test--with-rig
    (let* ((model (llm-pi-ai-occ-test--model :provider "acme"))
           (stream (llm-pi-ai-api-openai-completions-stream
                    model (llm-pi-ai-occ-test--context) nil))
           (message (llm-pi-ai-occ-test--result stream)))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "No API key for provider: acme"))
      (should-not llm-pi-ai-occ-test--requests)
      (should-error (llm-pi-ai-api-openai-completions-stream-simple
                     model (llm-pi-ai-occ-test--context) nil)))))

;;; A streamed response in awkward pieces

(defconst llm-pi-ai-occ-test--stream-chunks
  (list
   "{\"id\":\"chatcmpl-9\",\"model\":\"deepseek-flash-0101\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"reasoning_content\":\"Let me \"}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"reasoning_content\":\"think.\"}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"I\\u2019ll \"}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"content\":\"check 北京.\"}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_a\",\"type\":\"function\",\"function\":{\"name\":\"get_weather\",\"arguments\":\"{\\\"ci\"}}]}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":1,\"id\":\"call_b\",\"type\":\"function\",\"function\":{\"name\":\"get_time\",\"arguments\":\"{\\\"tz\\\":\"}}]}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"ty\\\":\\\"Pa\"}}]}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":1,\"function\":{\"arguments\":\"\\\"UTC\\\"}\"}}]}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{\"tool_calls\":[{\"index\":0,\"function\":{\"arguments\":\"ris\\\"}\"}}]}}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"tool_calls\"}]}"
   "{\"id\":\"chatcmpl-9\",\"choices\":[],\"usage\":{\"prompt_tokens\":100,\"completion_tokens\":50,\"prompt_cache_hit_tokens\":20,\"completion_tokens_details\":{\"reasoning_tokens\":10}}}")
  "Chunks of a response with reasoning, text, two interleaved tool calls, usage.")

(ert-deftest llm-pi-ai-occ-test-stream-awkward-pieces ()
  "Cut at every awkward place, the events and the message come out exact."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-models-stream-simple
                    (llm-pi-ai-occ-test--models) (llm-pi-ai-occ-test--deepseek)
                    (llm-pi-ai-occ-test--context)
                    '(:api-key "test-key" :reasoning "high")))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed-pieces
       req (concat llm-pi-ai-occ-test--head
                   (apply #'concat (mapcar #'llm-pi-ai-occ-test--data
                                           llm-pi-ai-occ-test--stream-chunks))
                   "data: [DONE]\n\n"))
      (llm-pi-ai-occ-test--exit req)
      (let* ((events (llm-pi-ai-event-stream-to-list stream))
             (message (llm-pi-ai-occ-test--result stream))
             (blocks (llm-pi-ai-message-content message)))
        (should (equal (llm-pi-ai-occ-test--types events)
                       '("start"
                         "thinking_start" "thinking_delta" "thinking_delta"
                         "text_start" "text_delta" "text_delta"
                         "toolcall_start" "toolcall_delta"
                         "toolcall_start" "toolcall_delta"
                         "toolcall_delta" "toolcall_delta" "toolcall_delta"
                         "thinking_end" "text_end" "toolcall_end" "toolcall_end"
                         "done")))
        (should (equal (mapcar #'llm-pi-ai-assistant-message-event-delta
                               (seq-filter
                                (lambda (event)
                                  (member (llm-pi-ai-assistant-message-event-type event)
                                          '("thinking_delta" "text_delta" "toolcall_delta")))
                                events))
                       '("Let me " "think." "I’ll " "check 北京."
                         "{\"ci" "{\"tz\":" "ty\":\"Pa" "\"UTC\"}" "ris\"}")))
        (should (equal (mapcar #'llm-pi-ai-assistant-message-event-content-index
                               (seq-filter
                                (lambda (event)
                                  (equal (llm-pi-ai-assistant-message-event-type event)
                                         "toolcall_delta"))
                                events))
                       '(2 3 2 3 2)))
        (should (equal (llm-pi-ai-assistant-message-event-content
                        (nth 14 events))
                       "Let me think."))
        ;; the last event is done, and its message is the result
        (should (eq (llm-pi-ai-assistant-message-event-message (car (last events)))
                    message))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "toolUse"))
        (should (equal (llm-pi-ai-assistant-message-raw-stop-reason message) "tool_calls"))
        (should (equal (llm-pi-ai-assistant-message-response-id message) "chatcmpl-9"))
        (should (equal (llm-pi-ai-assistant-message-response-model message)
                       "deepseek-flash-0101"))
        (should (= (length blocks) 4))
        (should (llm-pi-ai-thinking-content-p (nth 0 blocks)))
        (should (equal (llm-pi-ai-thinking-content-thinking (nth 0 blocks))
                       "Let me think."))
        (should (equal (llm-pi-ai-thinking-content-thinking-signature (nth 0 blocks))
                       "reasoning_content"))
        (should (equal (llm-pi-ai-text-content-text (nth 1 blocks))
                       "I’ll check 北京."))
        (let ((a (nth 2 blocks)) (b (nth 3 blocks)))
          (should (equal (list (llm-pi-ai-tool-call-id a) (llm-pi-ai-tool-call-name a))
                         '("call_a" "get_weather")))
          (should (equal (gethash "city" (llm-pi-ai-tool-call-arguments a)) "Paris"))
          (should (equal (list (llm-pi-ai-tool-call-id b) (llm-pi-ai-tool-call-name b))
                         '("call_b" "get_time")))
          (should (equal (gethash "tz" (llm-pi-ai-tool-call-arguments b)) "UTC"))
          (should-not (llm-pi-ai-tool-call-arguments-error a)))
        (let ((usage (llm-pi-ai-assistant-message-usage message)))
          (should (equal (list (llm-pi-ai-usage-input usage) (llm-pi-ai-usage-output usage)
                               (llm-pi-ai-usage-cache-read usage)
                               (llm-pi-ai-usage-cache-write usage)
                               (llm-pi-ai-usage-reasoning usage)
                               (llm-pi-ai-usage-total-tokens usage))
                         '(80 50 20 0 10 150)))
          (let ((cost (llm-pi-ai-usage-cost usage)))
            (should (< (abs (- (llm-pi-ai-cost-input cost) 2.4e-5)) 1e-12))
            (should (< (abs (- (llm-pi-ai-cost-output cost) 6e-5)) 1e-12))
            (should (< (abs (- (llm-pi-ai-cost-cache-read cost) 1.2e-7)) 1e-12))
            (should (< (abs (- (llm-pi-ai-cost-total cost) 8.412e-5)) 1e-12))))
        (should (numberp (llm-pi-ai-assistant-message-duration-ms message)))))))

(ert-deftest llm-pi-ai-occ-test-stream-one-byte-at-a-time ()
  "A response fed one byte at a time still parses."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k")))
           (req (llm-pi-ai-occ-test--request)))
      (dolist (byte (string-to-list
                     (encode-coding-string
                      (concat llm-pi-ai-occ-test--head
                              (llm-pi-ai-occ-test--data
                               (llm-pi-ai-occ-test--chunk "{\"content\":\"héllo 你好\"}"))
                              (llm-pi-ai-occ-test--data
                               (llm-pi-ai-occ-test--chunk "{}" "stop"))
                              "data: [DONE]\n\n")
                      'utf-8)))
        (funcall (llm-pi-ai-occ-test--req-output req) (unibyte-string byte)))
      (llm-pi-ai-occ-test--exit req)
      (should (equal (llm-pi-ai-text-content-text
                      (car (llm-pi-ai-message-content
                            (llm-pi-ai-occ-test--result stream))))
                     "héllo 你好")))))

;;; Stop reasons

(ert-deftest llm-pi-ai-occ-test-finish-reason-length-and-stop ()
  (llm-pi-ai-occ-test--with-rig
    (dolist (case '(("length" . "length") ("stop" . "stop") ("end" . "stop")
                    ("function_call" . "toolUse") ("tool_calls" . "toolUse")))
      (setq llm-pi-ai-occ-test--requests nil)
      (let* ((stream (llm-pi-ai-occ-test--stream
                      (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                      (list (llm-pi-ai-occ-test--chunk "{\"content\":\"x\"}")
                            (llm-pi-ai-occ-test--chunk "{}" (car case)))))
             (events (llm-pi-ai-event-stream-to-list stream))
             (message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) (cdr case)))
        (should (equal (llm-pi-ai-assistant-message-raw-stop-reason message) (car case)))
        (should (equal (llm-pi-ai-assistant-message-event-reason (car (last events)))
                       (cdr case)))
        (should (equal (llm-pi-ai-assistant-message-event-type (car (last events)))
                       "done"))))))

(ert-deftest llm-pi-ai-occ-test-raw-stop-reason-errors ()
  "(upstream openai-completions-raw-stop-reason) Error finish reasons."
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context)
                     '(:api-key "test")
                     (list (llm-pi-ai-occ-test--chunk "{}" "content_filter"))))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-raw-stop-reason message)
                     "content_filter"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Provider finish_reason: content_filter")))
    (setq llm-pi-ai-occ-test--requests nil)
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk "{}" "weird"))))))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Provider finish_reason: weird")))))

(ert-deftest llm-pi-ai-occ-test-raw-stop-reason-stop ()
  "(upstream openai-completions-raw-stop-reason) A plain stop keeps its raw."
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk "{}" "stop"))))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "stop"))
      (should (equal (llm-pi-ai-assistant-message-raw-stop-reason message) "stop"))
      (should-not (llm-pi-ai-assistant-message-error-message message)))))

(ert-deftest llm-pi-ai-occ-test-done-without-done-marker ()
  "The connection closes after a finish_reason with no [DONE]: still done."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-occ-test--stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                    (list (llm-pi-ai-occ-test--chunk "{\"content\":\"ok\"}")
                          (llm-pi-ai-occ-test--chunk "{}" "stop"))
                    t))
           (events (llm-pi-ai-event-stream-to-list stream)))
      (should (equal (llm-pi-ai-occ-test--types events)
                     '("start" "text_start" "text_delta" "text_end" "done")))
      (should (equal (llm-pi-ai-assistant-message-stop-reason
                      (llm-pi-ai-occ-test--result stream))
                     "stop")))))

(ert-deftest llm-pi-ai-occ-test-stream-ends-without-finish-reason ()
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk "{\"content\":\"cut off\"}"))))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Stream ended without finish_reason"))
      (should (equal (llm-pi-ai-text-content-text
                      (car (llm-pi-ai-message-content message)))
                     "cut off")))))

(ert-deftest llm-pi-ai-occ-test-no-finish-reason-support ()
  "Without `supportsFinishReason', the stop reason is inferred."
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model :compat '((supports-finish-reason)))
                     (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk "{\"content\":\"x\"}"))))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "stop")))
    (setq llm-pi-ai-occ-test--requests nil)
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model :compat '((supports-finish-reason)))
                     (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk
                            "{\"tool_calls\":[{\"index\":0,\"id\":\"c\",\"function\":{\"name\":\"f\",\"arguments\":\"{}\"}}]}"))))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "toolUse")))))

(ert-deftest llm-pi-ai-occ-test-response-model ()
  "(upstream openai-completions-response-model) Only a different model is kept."
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                     (list "{\"id\":\"r1\",\"model\":\"test-model\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}")))))
      (should-not (llm-pi-ai-assistant-message-response-model message))
      (should (equal (llm-pi-ai-assistant-message-response-id message) "r1")))
    (setq llm-pi-ai-occ-test--requests nil)
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                     (list "{\"id\":\"r2\",\"model\":\"routed-model\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}]}"
                           "{\"id\":\"r2\",\"model\":\"other\",\"choices\":[]}")))))
      (should (equal (llm-pi-ai-assistant-message-response-model message)
                     "routed-model")))))

(ert-deftest llm-pi-ai-occ-test-usage-variants ()
  "Cache read and write tokens, and usage in the choice (Moonshot)."
  (llm-pi-ai-occ-test--with-rig
    (let* ((model (llm-pi-ai-occ-test--model
                   :cost (llm-pi-ai-make-model-cost :input 1 :output 2
                                                    :cache-read 0.5 :cache-write 3)))
           (usage (llm-pi-ai-assistant-message-usage
                   (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     model (llm-pi-ai-occ-test--context) nil
                     (list "{\"id\":\"u\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\",\"usage\":{\"prompt_tokens\":1000,\"completion_tokens\":10,\"prompt_tokens_details\":{\"cached_tokens\":300,\"cache_write_tokens\":200}}}]}"))))))
      (should (equal (list (llm-pi-ai-usage-input usage) (llm-pi-ai-usage-cache-read usage)
                           (llm-pi-ai-usage-cache-write usage)
                           (llm-pi-ai-usage-total-tokens usage))
                     '(500 300 200 1010)))
      (should (< (abs (- (llm-pi-ai-cost-total (llm-pi-ai-usage-cost usage))
                         (+ 5e-4 2e-5 1.5e-4 6e-4)))
                 1e-12)))))

;;; Errors

(ert-deftest llm-pi-ai-occ-test-http-400-json-error ()
  "A 400 with a JSON body: an error event whose message has the provider's text."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k")))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed
       req "HTTP/1.1 400 Bad Request\r\nContent-Type: application/json\r\n\r\n"
       "{\"error\":{\"message\":\"Invalid parameter: messages\","
       "\"type\":\"invalid_request_error\"}}")
      (llm-pi-ai-occ-test--exit req)
      (let ((events (llm-pi-ai-event-stream-to-list stream))
            (message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-occ-test--types events) '("error")))
        (should (equal (llm-pi-ai-assistant-message-event-reason (car events)) "error"))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       (concat "400: {\"message\":\"Invalid parameter: messages\","
                               "\"type\":\"invalid_request_error\"}")))
        (should (= (length llm-pi-ai-occ-test--requests) 1))))))

(ert-deftest llm-pi-ai-occ-test-http-error-texts ()
  "Non-JSON and body-less errors read as the openai SDK's message does."
  (llm-pi-ai-occ-test--with-rig
    (dolist (case '(("502 Bad Gateway" "upstream down" "502 upstream down")
                    ("500 Internal Server Error" "" "500 status code (no body)")
                    ("401 Unauthorized" "{\"detail\":\"no\"}" "401 status code (no body)")
                    ("403 Forbidden" "{\"error\":\"denied\"}" "403 \"denied\"")))
      (setq llm-pi-ai-occ-test--requests nil)
      (let* ((stream (llm-pi-ai-api-openai-completions-stream
                      (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                      '(:api-key "k")))
             (req (llm-pi-ai-occ-test--request)))
        (llm-pi-ai-occ-test--feed
         req (format "HTTP/1.1 %s\r\n\r\n%s" (nth 0 case) (nth 1 case)))
        (llm-pi-ai-occ-test--exit req)
        (should (equal (llm-pi-ai-assistant-message-error-message
                        (llm-pi-ai-occ-test--result stream))
                       (nth 2 case)))))))

(ert-deftest llm-pi-ai-occ-test-http-429-retry-after-then-success ()
  "(upstream openai-completions-retry) One stream, no duplicate events."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :max-retries 2)))
           (first (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed
       first "HTTP/1.1 429 Too Many Requests\r\nretry-after: 2\r\n\r\n"
       "{\"error\":{\"message\":\"slow down\"}}")
      (llm-pi-ai-occ-test--exit first)
      ;; waiting: one backoff timer of 2 s, no second request, no event yet
      (should (= (length llm-pi-ai-occ-test--requests) 1))
      (should (= (length llm-pi-ai-occ-test--timers) 1))
      (should (= (car (nth 0 llm-pi-ai-occ-test--timers)) 2.0))
      (should (= (llm-pi-ai-fifo-queue-length (llm-pi-ai-event-stream-queue stream)) 0))
      (llm-pi-ai-occ-test--fire-timer 0)
      (should (= (length llm-pi-ai-occ-test--requests) 2))
      (let ((second (llm-pi-ai-occ-test--request 1)))
        (llm-pi-ai-occ-test--feed
         second llm-pi-ai-occ-test--head
         (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"ok\"}"))
         (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{}" "stop"))
         "data: [DONE]\n\n")
        (llm-pi-ai-occ-test--exit second))
      (should (equal (llm-pi-ai-occ-test--types (llm-pi-ai-event-stream-to-list stream))
                     '("start" "text_start" "text_delta" "text_end" "done")))
      (should (equal (llm-pi-ai-occ-test--body 0) (llm-pi-ai-occ-test--body 1))))))

(ert-deftest llm-pi-ai-occ-test-retry-delay-above-limit-fails-at-once ()
  "(upstream openai-completions-retry) A long retry-after is not waited for."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :max-retries 2 :max-retry-delay-ms 1000)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed
       req "HTTP/1.1 429 Too Many Requests\r\nretry-after: 277403\r\n\r\n"
       "{\"error\":{\"message\":\"rate limited\"}}")
      (llm-pi-ai-occ-test--exit req)
      (let ((message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
        (should (string-match-p "Server requested 277403s retry delay (max: 1s)"
                                (llm-pi-ai-assistant-message-error-message message)))
        (should (string-match-p "rate limited"
                                (llm-pi-ai-assistant-message-error-message message)))
        (should (= (length llm-pi-ai-occ-test--requests) 1))))))

(ert-deftest llm-pi-ai-occ-test-no-retry-by-default ()
  "A 429 is final when `:max-retries' is not given."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k")))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed req "HTTP/1.1 429 Too Many Requests\r\n\r\n{}")
      (llm-pi-ai-occ-test--exit req)
      (should (equal (llm-pi-ai-assistant-message-stop-reason
                      (llm-pi-ai-occ-test--result stream))
                     "error"))
      (should-not llm-pi-ai-occ-test--timers))))

(ert-deftest llm-pi-ai-occ-test-no-retry-once-a-chunk-was-delivered ()
  "A failure after the response began ends the stream; nothing is retried."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :max-retries 3)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed
       req llm-pi-ai-occ-test--head
       (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"par\"}")))
      (llm-pi-ai-occ-test--exit req 56 "curl: (56) Recv failure: Connection reset")
      (let ((events (llm-pi-ai-event-stream-to-list stream))
            (message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-occ-test--types events)
                       '("start" "text_start" "text_delta" "error")))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       "Connection error: curl: (56) Recv failure: Connection reset"))
        (should (equal (llm-pi-ai-text-content-text
                        (car (llm-pi-ai-message-content message)))
                       "par"))
        (should (= (length llm-pi-ai-occ-test--requests) 1))
        (should-not llm-pi-ai-occ-test--timers)))))

(ert-deftest llm-pi-ai-occ-test-network-error ()
  "curl fails before any response: an error event; it is retried when asked."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k")))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--exit req 7 "curl: (7) Failed to connect to api.deepseek.com")
      (let ((message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       "Connection error: curl: (7) Failed to connect to api.deepseek.com"))
        (should (equal (llm-pi-ai-occ-test--types
                        (llm-pi-ai-event-stream-to-list stream))
                       '("error")))))
    ;; with retries allowed, a connection error is retried
    (setq llm-pi-ai-occ-test--requests nil llm-pi-ai-occ-test--timers nil)
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :max-retries 1)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--exit req 7 "curl: (7) refused")
      (should (= (length llm-pi-ai-occ-test--timers) 1))
      (llm-pi-ai-occ-test--fire-timer 0)
      (should (= (length llm-pi-ai-occ-test--requests) 2))
      (llm-pi-ai-occ-test--exit (llm-pi-ai-occ-test--request 1) 7 "curl: (7) refused again")
      (should (equal (llm-pi-ai-assistant-message-error-message
                      (llm-pi-ai-occ-test--result stream))
                     "Connection error: curl: (7) refused again")))))

(ert-deftest llm-pi-ai-occ-test-timeout-until-headers ()
  "`:timeout-ms' ends a request with no headers; curl is killed."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :timeout-ms 5000)))
           (req (llm-pi-ai-occ-test--request)))
      (should (= (length llm-pi-ai-occ-test--timers) 1))
      (should (= (car (nth 0 llm-pi-ai-occ-test--timers)) 5.0))
      (llm-pi-ai-occ-test--fire-timer 0)
      (let ((message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       "Request timed out."))
        (should (= (llm-pi-ai-occ-test--req-killed req) 1))))))

(ert-deftest llm-pi-ai-occ-test-timeout-stops-at-headers ()
  "Once the headers arrive the timeout timer is cancelled, a long stream lives."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k" :timeout-ms 5000)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed req llm-pi-ai-occ-test--head)
      (should (nth 2 (nth 0 llm-pi-ai-occ-test--timers)))
      (llm-pi-ai-occ-test--feed
       req (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{}" "stop"))
       "data: [DONE]\n\n")
      (llm-pi-ai-occ-test--exit req)
      (should (equal (llm-pi-ai-assistant-message-stop-reason
                      (llm-pi-ai-occ-test--result stream))
                     "stop")))))

(ert-deftest llm-pi-ai-occ-test-curl-timeout-exit ()
  "The seam's `timeout' (curl exit 28) reads \"Request timed out.\"."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    '(:api-key "k")))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--exit req 28 "curl: (28) Operation timed out")
      (should (equal (llm-pi-ai-assistant-message-error-message
                      (llm-pi-ai-occ-test--result stream))
                     "Request timed out.")))))

(ert-deftest llm-pi-ai-occ-test-error-chunk-mid-stream ()
  "The SDK throws an APIError for a chunk with an `error' member."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-occ-test--stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                    (list (llm-pi-ai-occ-test--chunk "{\"content\":\"a\"}")
                          "{\"error\":{\"message\":\"model overloaded\",\"code\":503}}"
                          (llm-pi-ai-occ-test--chunk "{\"content\":\"b\"}"))))
           (message (llm-pi-ai-occ-test--result stream)))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "model overloaded"))
      (should (equal (llm-pi-ai-text-content-text
                      (car (llm-pi-ai-message-content message)))
                     "a")))))

(ert-deftest llm-pi-ai-occ-test-invalid-json-chunk ()
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-occ-test--stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                    (list "{not json")))
           (message (llm-pi-ai-occ-test--result stream)))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (string-match-p "JSON" (llm-pi-ai-assistant-message-error-message message))))))

(ert-deftest llm-pi-ai-occ-test-error-metadata-raw ()
  "OpenRouter's `metadata.raw' is appended unless the message has it."
  (llm-pi-ai-occ-test--with-rig
    (let* ((stream (llm-pi-ai-occ-test--stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context) nil
                    (list "{\"error\":{\"message\":\"Provider returned error\",\"metadata\":{\"raw\":\"upstream said no\"}}}")))
           (message (llm-pi-ai-occ-test--result stream)))
      (should (equal (llm-pi-ai-assistant-message-error-message message)
                     "Provider returned error\nupstream said no")))))

;;; Abort

(ert-deftest llm-pi-ai-occ-test-abort-before-first-chunk ()
  "Aborted while waiting for the response: aborted, curl killed, nothing after."
  (llm-pi-ai-occ-test--with-rig
    (let* ((signal (llm-pi-ai-make-abort-signal))
           (stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    (list :api-key "k" :signal signal)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-abort-signal-abort signal)
      (should (= (llm-pi-ai-occ-test--req-killed req) 1))
      (let ((events (llm-pi-ai-event-stream-to-list stream))
            (message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-occ-test--types events) '("error")))
        (should (equal (llm-pi-ai-assistant-message-event-reason (car events)) "aborted"))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "aborted"))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       "Request aborted")))
      ;; late bytes and exit change nothing
      (llm-pi-ai-occ-test--feed
       req llm-pi-ai-occ-test--head
       (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"late\"}")))
      (llm-pi-ai-occ-test--exit req)
      (should-not (llm-pi-ai-event-stream-to-list stream)))))

(ert-deftest llm-pi-ai-occ-test-abort-mid-stream ()
  "Aborted after some text: aborted, the text kept, no event after it."
  (llm-pi-ai-occ-test--with-rig
    (let* ((signal (llm-pi-ai-make-abort-signal))
           (stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    (list :api-key "k" :signal signal :max-retries 3)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed
       req llm-pi-ai-occ-test--head
       (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"Hel\"}")))
      (llm-pi-ai-abort-signal-abort signal)
      (should (= (llm-pi-ai-occ-test--req-killed req) 1))
      (llm-pi-ai-occ-test--feed
       req (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"lo\"}")))
      (llm-pi-ai-occ-test--exit req)
      (let* ((events (llm-pi-ai-event-stream-to-list stream))
             (message (llm-pi-ai-occ-test--result stream)))
        (should (equal (llm-pi-ai-occ-test--types events)
                       '("start" "text_start" "text_delta" "error")))
        (should (equal (llm-pi-ai-assistant-message-event-reason (car (last events)))
                       "aborted"))
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "aborted"))
        (should (equal (llm-pi-ai-assistant-message-error-message message)
                       "Request was aborted."))
        (should (equal (llm-pi-ai-text-content-text
                        (car (llm-pi-ai-message-content message)))
                       "Hel"))
        (should (= (length llm-pi-ai-occ-test--requests) 1))))))

(ert-deftest llm-pi-ai-occ-test-abort-already-aborted ()
  "A signal aborted before the call: no request is made."
  (llm-pi-ai-occ-test--with-rig
    (let* ((signal (llm-pi-ai-make-abort-signal)))
      (llm-pi-ai-abort-signal-abort signal)
      (let* ((stream (llm-pi-ai-api-openai-completions-stream
                      (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                      (list :api-key "k" :signal signal)))
             (message (llm-pi-ai-occ-test--result stream)))
        (should-not llm-pi-ai-occ-test--requests)
        (should (equal (llm-pi-ai-assistant-message-stop-reason message) "aborted"))))))

(ert-deftest llm-pi-ai-occ-test-abort-during-retry-wait ()
  "Aborted while waiting to retry: aborted, no second request."
  (llm-pi-ai-occ-test--with-rig
    (let* ((signal (llm-pi-ai-make-abort-signal))
           (stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    (list :api-key "k" :signal signal :max-retries 2)))
           (req (llm-pi-ai-occ-test--request)))
      (llm-pi-ai-occ-test--feed req "HTTP/1.1 500 Oops\r\n\r\nx")
      (llm-pi-ai-occ-test--exit req)
      (should (= (length llm-pi-ai-occ-test--timers) 1))
      (llm-pi-ai-abort-signal-abort signal)
      (should (equal (llm-pi-ai-assistant-message-stop-reason
                      (llm-pi-ai-occ-test--result stream))
                     "aborted"))
      (should (= (length llm-pi-ai-occ-test--requests) 1)))))

;;; Hooks

(ert-deftest llm-pi-ai-occ-test-on-payload-and-on-response ()
  "`:on-payload' may replace the body; `:on-response' gets status and headers."
  (llm-pi-ai-occ-test--with-rig
    (let* ((seen-payload nil) (seen-response nil) (seen-chunks nil)
           (stream (llm-pi-ai-api-openai-completions-stream
                    (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
                    (list :api-key "k"
                          :on-payload (lambda (params model)
                                        (setq seen-payload (list params (llm-pi-ai-model-id model)))
                                        '((custom . "body") (list . [1 2])))
                          :on-response (lambda (response _model)
                                         (setq seen-response response))
                          :on-provider-stream-event (lambda (chunk _model)
                                                      (push chunk seen-chunks)))))
           (req (llm-pi-ai-occ-test--request)))
      (should (equal (llm-pi-ai-occ-test--body) "{\"custom\":\"body\",\"list\":[1,2]}"))
      (should (equal (alist-get 'model (car seen-payload)) "deepseek-flash"))
      (llm-pi-ai-occ-test--feed
       req "HTTP/1.1 200 OK\r\nX-Request-Id: abc\r\n\r\n"
       (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{}" "stop")))
      (llm-pi-ai-occ-test--exit req)
      (should (= (llm-pi-ai-provider-response-status seen-response) 200))
      (should (equal (assoc "x-request-id" (llm-pi-ai-provider-response-headers seen-response))
                     '("x-request-id" . "abc")))
      (should (= (length seen-chunks) 1))
      (should (hash-table-p (car seen-chunks)))
      (should (equal (llm-pi-ai-assistant-message-stop-reason
                      (llm-pi-ai-occ-test--result stream))
                     "stop")))))

(ert-deftest llm-pi-ai-occ-test-on-payload-nil-keeps-body ()
  "(upstream openai-thinking-token-budget) A hook that returns nil changes nothing."
  (llm-pi-ai-occ-test--with-rig
    (let ((payload nil))
      (llm-pi-ai-api-openai-completions-stream
       (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-occ-test--context)
       (list :api-key "k" :on-payload (lambda (params _m) (setq payload params) nil)))
      (should (equal (llm-pi-ai-oc--serialize payload) (llm-pi-ai-occ-test--body))))))

;;; Request parameters (buildParams)

(defun llm-pi-ai-occ-test--params (model options &optional context)
  "Return the parsed request body for MODEL, OPTIONS and CONTEXT."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-api-openai-completions-stream
     model (or context (llm-pi-ai-occ-test--context))
     (append (list :api-key "k") options))
    (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body))))

(defun llm-pi-ai-occ-test--vllm-model (&optional compat)
  "Return a local vLLM reasoning model with COMPAT."
  (llm-pi-ai-occ-test--model :id "zai-org/glm-5.2" :provider "local-vllm"
                             :base-url "http://localhost:8000/v1" :reasoning t
                             :context-window 262144 :max-tokens 16384
                             :compat (or compat '((thinking-format . "zai")
                                                  (supports-thinking-token-budget . t)))))

(ert-deftest llm-pi-ai-occ-test-thinking-token-budget ()
  "(upstream openai-completions-thinking-token-budget) Budget per level."
  (let ((params (llm-pi-ai-occ-test--params
                 (llm-pi-ai-occ-test--vllm-model)
                 (list :reasoning-effort "medium"
                       :thinking-budgets (llm-pi-ai-make-thinking-budgets :medium 4096)))))
    (should (= (gethash "thinking_token_budget" params) 4096))
    (should (equal (llm-pi-ai-occ-test--ref params "thinking" "type") "enabled"))))

(ert-deftest llm-pi-ai-occ-test-thinking-token-budget-omitted ()
  "(upstream openai-completions-thinking-token-budget) Omitted without the field."
  (let ((params (llm-pi-ai-occ-test--params
                 (llm-pi-ai-occ-test--vllm-model '((thinking-format . "zai")))
                 (list :reasoning-effort "medium"))))
    (should-not (gethash "thinking_token_budget" params))
    (should-not (gethash "thinking_budget" params)))
  ;; off: no budget
  (let ((params (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--vllm-model) nil)))
    (should-not (gethash "thinking_token_budget" params))
    (should (equal (llm-pi-ai-occ-test--ref params "thinking" "type") "disabled"))))

(ert-deftest llm-pi-ai-occ-test-thinking-token-budget-field-and-default ()
  "A named field, and the default budget of the level; clamped to leave room."
  (let ((params (llm-pi-ai-occ-test--params
                 (llm-pi-ai-occ-test--vllm-model
                  '((thinking-format . "zai") (thinking-token-budget-field . "thinking_budget")))
                 (list :reasoning-effort "low"))))
    (should (= (gethash "thinking_budget" params) 2048)))
  ;; max tokens 2000 leaves 976 for thinking
  (let ((params (llm-pi-ai-occ-test--params
                 (llm-pi-ai-occ-test--vllm-model)
                 (list :reasoning-effort "high" :max-tokens 2000))))
    (should (= (gethash "thinking_token_budget" params) 976))))

(ert-deftest llm-pi-ai-occ-test-thinking-formats ()
  "The body of each thinking format, reasoning on and off."
  (cl-flet ((params (format effort &optional extra)
              (llm-pi-ai-occ-test--params
               (llm-pi-ai-occ-test--model
                :reasoning t :compat (append (list (cons 'thinking-format format)) extra))
               (and effort (list :reasoning-effort effort)))))
    (let ((p (params "zai" "high")))
      (should (equal (llm-pi-ai-oc--serialize
                      (list (cons 'thinking (llm-pi-ai-occ-test--ref p "thinking"))))
                     "{\"thinking\":{\"type\":\"enabled\",\"clear_thinking\":false}}"))
      (should (equal (gethash "reasoning_effort" p) "high")))
    (should (equal (llm-pi-ai-occ-test--ref (params "zai" nil) "thinking" "type")
                   "disabled"))
    (should (eq (gethash "enable_thinking" (params "qwen" "low")) t))
    (should (eq (gethash "enable_thinking" (params "qwen" nil)) :false))
    (let ((p (params "qwen-chat-template" "low")))
      (should (eq (llm-pi-ai-occ-test--ref p "chat_template_kwargs" "enable_thinking") t))
      (should (eq (llm-pi-ai-occ-test--ref p "chat_template_kwargs" "preserve_thinking") t)))
    (should (equal (llm-pi-ai-occ-test--ref (params "openrouter" "high") "reasoning" "effort")
                   "high"))
    (should (equal (llm-pi-ai-occ-test--ref (params "openrouter" nil) "reasoning" "effort")
                   "none"))
    (should (eq (llm-pi-ai-occ-test--ref (params "together" "high") "reasoning" "enabled") t))
    (should (equal (gethash "thinking" (params "string-thinking" "high")) "high"))
    (should (equal (gethash "thinking" (params "string-thinking" nil)) "none"))
    (should (equal (gethash "reasoning_effort" (params "openai" "high")) "high"))
    (should-not (gethash "reasoning_effort" (params "openai" nil)))
    (should-not (gethash "thinking" (params "ant-ling" nil)))))

(ert-deftest llm-pi-ai-occ-test-chat-template-values ()
  "Chat template kwargs: literals, $var entries, omitWhenOff."
  (let* ((kwargs '(("enable_thinking" ("$var" . "thinking.enabled"))
                   ("budget" ("$var" . "thinking.budget") ("omitWhenOff" . t))
                   ("effort" ("$var" . "thinking.effort"))
                   ("flag" . t) ("name" . "x") ("nothing" . :null) ("no" . nil)))
         (model (llm-pi-ai-occ-test--model
                 :reasoning t
                 :compat (list (cons 'thinking-format "chat-template")
                               (cons 'supports-thinking-token-budget t)
                               (cons 'chat-template-kwargs kwargs))))
         (on (llm-pi-ai-occ-test--params
              model (list :reasoning-effort "low" :max-tokens 5000)))
         (off (llm-pi-ai-occ-test--params model nil)))
    (should (equal (llm-pi-ai-oc--serialize (llm-pi-ai-occ-test--ref on "chat_template_kwargs"))
                   (concat "{\"enable_thinking\":true,\"budget\":2048,\"effort\":\"low\","
                           "\"flag\":true,\"name\":\"x\",\"nothing\":null,\"no\":false}")))
    (should (equal (llm-pi-ai-oc--serialize (llm-pi-ai-occ-test--ref off "chat_template_kwargs"))
                   (concat "{\"enable_thinking\":false,"
                           "\"flag\":true,\"name\":\"x\",\"nothing\":null,\"no\":false}")))))

(ert-deftest llm-pi-ai-occ-test-sampling-params-and-temperature ()
  "Sampling params go last and override named fields; model, level, request."
  (let ((params (llm-pi-ai-occ-test--params
                 (llm-pi-ai-occ-test--model
                  :reasoning t :compat '((thinking-format . "openai"))
                  :sampling-params '(("top_p" . 0.9) ("temperature" . 0.1))
                  :sampling-params-by-thinking-level '(("high" ("top_p" . 0.5) ("top_k" . 40))))
                 (list :temperature 0.7 :reasoning-effort "high"
                       :sampling-params '(("top_k" . 20) ("seed" . 7))))))
    (should (= (gethash "temperature" params) 0.1))
    (should (= (gethash "top_p" params) 0.5))
    (should (= (gethash "top_k" params) 20))
    (should (= (gethash "seed" params) 7)))
  (should (= (gethash "temperature"
                      (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model)
                                                  '(:temperature 0.7)))
             0.7)))

(ert-deftest llm-pi-ai-occ-test-max-tokens-fields-and-store ()
  (let ((p (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model) '(:max-tokens 100))))
    (should (= (gethash "max_completion_tokens" p) 100))
    (should-not (gethash "max_tokens" p))
    (should (eq (gethash "store" p) :false))
    (should (equal (llm-pi-ai-occ-test--ref p "stream_options" "include_usage") t))
    (should-not (gethash "max_completion_tokens"
                         (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model) nil))))
  (let ((p (llm-pi-ai-occ-test--params
            (llm-pi-ai-occ-test--model :compat '((max-tokens-field . "max_tokens")
                                                 (supports-usage-in-streaming)
                                                 (supports-store)))
            '(:max-tokens 100))))
    (should (= (gethash "max_tokens" p) 100))
    (should-not (gethash "stream_options" p))
    (should (eq (gethash "store" p) nil))))

(ert-deftest llm-pi-ai-occ-test-tool-choice-and-priority ()
  (let ((p (llm-pi-ai-occ-test--params
            (llm-pi-ai-occ-test--model :compat '((vllm-priority . 5)))
            '(:tool-choice ((type . "function") (function . ((name . "get_weather"))))))))
    (should (equal (llm-pi-ai-oc--serialize (llm-pi-ai-occ-test--ref p "tool_choice"))
                   "{\"type\":\"function\",\"function\":{\"name\":\"get_weather\"}}"))
    (should (= (gethash "priority" p) 5)))
  (should (equal (gethash "tool_choice"
                          (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model)
                                                      '(:tool-choice "none")))
                 "none")))

(ert-deftest llm-pi-ai-occ-test-empty-tools-with-tool-history ()
  "(upstream openai-completions-empty-tools) `tools: []' for a history with tool calls."
  (let* ((model (llm-pi-ai-occ-test--model))
         (assistant (llm-pi-ai-make-assistant-message
                     :content (list (llm-pi-ai-make-tool-call
                                     :id "t1" :name "read"
                                     :arguments (llm-pi-ai-json-object "path" "a")))
                     :api "openai-completions" :provider "openai" :model "test-model"
                     :stop-reason "toolUse" :timestamp 2))
         (result (llm-pi-ai-make-tool-result-message
                  :tool-call-id "t1" :tool-name "read"
                  :content (list (llm-pi-ai-make-text-content :text "ok")) :timestamp 3))
         (p (llm-pi-ai-occ-test--params
             model nil (llm-pi-ai-occ-test--context
                        (llm-pi-ai-occ-test--user "go") assistant result))))
    (should (equal (llm-pi-ai-occ-test--ref p "tools") []))
    (should-not (gethash "tools" (llm-pi-ai-occ-test--params model nil)))))

(ert-deftest llm-pi-ai-occ-test-prompt-cache-key ()
  "(upstream openai-completions-prompt-cache) Key and retention fields."
  (let ((p (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model)
                                       '(:session-id "sess-1"))))
    (should (equal (gethash "prompt_cache_key" p) "sess-1"))
    (should-not (gethash "prompt_cache_retention" p)))
  (should-not (gethash "prompt_cache_key"
                       (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model)
                                                   '(:session-id "sess-1"
                                                     :cache-retention "none"))))
  (let ((p (llm-pi-ai-occ-test--params (llm-pi-ai-occ-test--model)
                                       (list :session-id (make-string 70 ?x)
                                             :cache-retention "long"))))
    (should (equal (gethash "prompt_cache_key" p) (make-string 64 ?x)))
    (should (equal (gethash "prompt_cache_retention" p) "24h")))
  ;; a model that is not OpenAI gets a key only for long retention
  (let ((deepseek (llm-pi-ai-occ-test--deepseek)))
    (should-not (gethash "prompt_cache_key"
                         (llm-pi-ai-occ-test--params deepseek '(:session-id "s"))))
    (should (equal (gethash "prompt_cache_key"
                            (llm-pi-ai-occ-test--params
                             deepseek '(:session-id "s" :cache-retention "long")))
                   "s")))
  ;; PI_CACHE_RETENTION=long from the options' env
  (should (equal (gethash "prompt_cache_retention"
                          (llm-pi-ai-occ-test--params
                           (llm-pi-ai-occ-test--model)
                           '(:session-id "s" :env (("PI_CACHE_RETENTION" . "long")))))
                 "24h")))

(ert-deftest llm-pi-ai-occ-test-clamp-prompt-cache-key ()
  (should-not (llm-pi-ai-clamp-openai-prompt-cache-key nil))
  (should (equal (llm-pi-ai-clamp-openai-prompt-cache-key "abc") "abc"))
  (should (equal (llm-pi-ai-clamp-openai-prompt-cache-key (make-string 65 ?中))
                 (make-string 64 ?中))))

(ert-deftest llm-pi-ai-occ-test-session-affinity-headers ()
  "OpenRouter gets x-session-id; others the openai-style pair; none: no header."
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-api-openai-completions-stream
     (llm-pi-ai-occ-test--model :provider "openrouter" :id "x/y"
                                :base-url "https://openrouter.ai/api/v1")
     (llm-pi-ai-occ-test--context) '(:api-key "k" :session-id "s1"))
    (let ((config (llm-pi-ai-occ-test--req-config (llm-pi-ai-occ-test--request))))
      (should (string-match-p "x-session-id: s1" config))
      (should-not (string-match-p "x-session-affinity" config))))
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-api-openai-completions-stream
     (llm-pi-ai-occ-test--model :compat '((send-session-affinity-headers . t)))
     (llm-pi-ai-occ-test--context) '(:api-key "k" :session-id "s2"))
    (let ((config (llm-pi-ai-occ-test--req-config (llm-pi-ai-occ-test--request))))
      (should (string-match-p "session_id: s2" config))
      (should (string-match-p "x-client-request-id: s2" config))
      (should (string-match-p "x-session-affinity: s2" config))))
  (llm-pi-ai-occ-test--with-rig
    (llm-pi-ai-api-openai-completions-stream
     (llm-pi-ai-occ-test--model :compat '((send-session-affinity-headers . t)))
     (llm-pi-ai-occ-test--context) '(:api-key "k" :session-id "s2" :cache-retention "none"))
    (should-not (string-match-p "s2" (llm-pi-ai-occ-test--req-config
                                      (llm-pi-ai-occ-test--request))))))

(ert-deftest llm-pi-ai-occ-test-routing-preferences ()
  (let ((p (llm-pi-ai-occ-test--params
            (llm-pi-ai-occ-test--model
             :compat '((open-router-routing ("only" "anthropic" "openai")
                                            ("allow_fallbacks")
                                            ("sort" . "price"))
                       (vercel-gateway-routing ("only" "bedrock") ("order" "a" "b"))))
            nil)))
    (should (equal (llm-pi-ai-oc--serialize (llm-pi-ai-occ-test--ref p "provider"))
                   "{\"only\":[\"anthropic\",\"openai\"],\"allow_fallbacks\":false,\"sort\":\"price\"}"))
    (should (equal (llm-pi-ai-oc--serialize (llm-pi-ai-occ-test--ref p "providerOptions"))
                   "{\"gateway\":{\"only\":[\"bedrock\"],\"order\":[\"a\",\"b\"]}}"))))

(ert-deftest llm-pi-ai-occ-test-cache-control-anthropic ()
  "(upstream openai-completions-cache-control-format) Marks prompt, tool, last message."
  (let* ((model (llm-pi-ai-occ-test--model :provider "openrouter" :id "anthropic/claude"
                                           :base-url "https://openrouter.ai/api/v1"))
         (context (llm-pi-ai-make-context
                   :system-prompt "sys"
                   :tools (list (llm-pi-ai-make-tool :name "a" :description "A"
                                                     :parameters '((type . "object")))
                                (llm-pi-ai-make-tool :name "b" :description "B"
                                                     :parameters '((type . "object"))))
                   :messages (list (llm-pi-ai-occ-test--user "first")
                                   (llm-pi-ai-make-assistant-message
                                    :content (list (llm-pi-ai-make-text-content :text "second"))
                                    :api "openai-completions" :provider "openrouter"
                                    :model "anthropic/claude" :stop-reason "stop" :timestamp 2)
                                   (llm-pi-ai-occ-test--user "third"))))
         (p (llm-pi-ai-occ-test--params model nil (llm-pi-ai-normalize-context context)))
         (messages (gethash "messages" p))
         (marker "\"cache_control\":{\"type\":\"ephemeral\"}"))
    (should (equal (llm-pi-ai-oc--serialize (aref messages 0))
                   (concat "{\"role\":\"system\",\"content\":[{\"type\":\"text\",\"text\":\"sys\","
                           marker "}]}")))
    (should (equal (gethash "content" (aref messages 1)) "first"))
    (should (equal (gethash "content" (aref messages 2)) "second"))
    (should (equal (llm-pi-ai-oc--serialize (aref messages 3))
                   (concat "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"third\","
                           marker "}]}")))
    (let ((tools (gethash "tools" p)))
      (should-not (gethash "cache_control" (aref tools 0)))
      (should (gethash "cache_control" (aref tools 1))))
    ;; long retention asks for a one hour ttl
    (let ((p (llm-pi-ai-occ-test--params model '(:cache-retention "long")
                                         (llm-pi-ai-normalize-context context))))
      (should (equal (llm-pi-ai-oc--serialize
                      (llm-pi-ai-occ-test--ref (gethash "tools" p) 1 "cache_control"))
                     "{\"type\":\"ephemeral\",\"ttl\":\"1h\"}")))
    ;; none: nothing is marked
    (let ((p (llm-pi-ai-occ-test--params model '(:cache-retention "none")
                                         (llm-pi-ai-normalize-context context))))
      (should-not (string-match-p "cache_control" (llm-pi-ai-oc--serialize p))))))

(ert-deftest llm-pi-ai-occ-test-constrained-sampling-not-ported ()
  "A tool with constrained sampling ends the stream with a plain error."
  (llm-pi-ai-occ-test--with-rig
    (let* ((context (llm-pi-ai-normalize-context
                     (llm-pi-ai-make-context
                      :tools (list (llm-pi-ai-make-tool :name "g" :description "G"
                                                        :parameters '((type . "object"))
                                                        :constrained-sampling t))
                      :messages (list (llm-pi-ai-occ-test--user "hi")))))
           (message (llm-pi-ai-occ-test--result
                     (llm-pi-ai-api-openai-completions-stream
                      (llm-pi-ai-occ-test--model) context '(:api-key "k")))))
      (should (equal (llm-pi-ai-assistant-message-stop-reason message) "error"))
      (should (string-match-p "constrained sampling" (llm-pi-ai-assistant-message-error-message message))))))

;;; convertMessages

(defun llm-pi-ai-occ-test--convert (model &rest messages)
  "Return the serialized messages `convert-messages' makes of MESSAGES for MODEL."
  (mapcar #'llm-pi-ai-oc--serialize
          (llm-pi-ai-api-openai-completions-convert-messages
           model (apply #'llm-pi-ai-occ-test--context messages)
           (llm-pi-ai-api-openai-completions-get-compat model))))

(defun llm-pi-ai-occ-test--assistant (model &rest content)
  "Return an assistant message of MODEL, a model, with CONTENT blocks."
  (llm-pi-ai-make-assistant-message
   :content content :api (llm-pi-ai-model-api model)
   :provider (llm-pi-ai-model-provider model) :model (llm-pi-ai-model-id model)
   :stop-reason "stop" :timestamp 2))

(ert-deftest llm-pi-ai-occ-test-convert-user-content-parts ()
  "(upstream openai-completions-tool-result-images) Empty text parts are dropped."
  (let ((model (llm-pi-ai-occ-test--model :input '("text" "image"))))
    (should (equal (llm-pi-ai-occ-test--convert
                    model
                    (llm-pi-ai-make-user-message
                     :content (list (llm-pi-ai-make-text-content :text "")
                                    (llm-pi-ai-make-image-content :data "ZmFrZQ==" :mime-type "image/png"))
                     :timestamp 1))
                   '("{\"role\":\"user\",\"content\":[{\"type\":\"image_url\",\"image_url\":{\"url\":\"data:image/png;base64,ZmFrZQ==\"}}]}")))
    ;; nothing left: the message is skipped
    (should-not (llm-pi-ai-occ-test--convert
                 model (llm-pi-ai-make-user-message
                        :content (list (llm-pi-ai-make-text-content :text ""))
                        :timestamp 1)))
    (should (equal (llm-pi-ai-occ-test--convert
                    model (llm-pi-ai-make-user-message
                           :content (list (llm-pi-ai-make-text-content :text "a"))
                           :timestamp 1))
                   '("{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"a\"}]}")))))

(ert-deftest llm-pi-ai-occ-test-convert-instruction-role ()
  "A reasoning model with developer role support gets `developer'."
  (cl-flet ((roles (model)
              (mapcar (lambda (m) (alist-get 'role m))
                      (llm-pi-ai-api-openai-completions-convert-messages
                       model
                       (llm-pi-ai-normalize-context
                        (llm-pi-ai-make-context :system-prompt "s"
                                                :messages (list (llm-pi-ai-occ-test--user "u"))))
                       (llm-pi-ai-api-openai-completions-get-compat model)))))
    (should (equal (roles (llm-pi-ai-occ-test--model :reasoning t)) '("developer" "user")))
    (should (equal (roles (llm-pi-ai-occ-test--model :reasoning nil)) '("system" "user")))
    (should (equal (roles (llm-pi-ai-occ-test--deepseek)) '("system" "user")))))

(ert-deftest llm-pi-ai-occ-test-convert-tool-result-images-batched ()
  "(upstream openai-completions-tool-result-images) Images follow all results."
  (let* ((model (llm-pi-ai-occ-test--model :input '("text" "image")))
         (image-result (lambda (id)
                         (llm-pi-ai-make-tool-result-message
                          :tool-call-id id :tool-name "read"
                          :content (list (llm-pi-ai-make-text-content :text "Read image file [image/png]")
                                         (llm-pi-ai-make-image-content :data "ZmFrZQ==" :mime-type "image/png"))
                          :timestamp 3)))
         (messages (llm-pi-ai-api-openai-completions-convert-messages
                    model
                    (llm-pi-ai-occ-test--context
                     (llm-pi-ai-occ-test--user "Read the images")
                     (llm-pi-ai-occ-test--assistant
                      model
                      (llm-pi-ai-make-tool-call :id "tool-1" :name "read"
                                                :arguments (llm-pi-ai-json-object "path" "img-1.png"))
                      (llm-pi-ai-make-tool-call :id "tool-2" :name "read"
                                                :arguments (llm-pi-ai-json-object "path" "img-2.png")))
                     (funcall image-result "tool-1") (funcall image-result "tool-2"))
                    (llm-pi-ai-api-openai-completions-get-compat model))))
    (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                   '("user" "assistant" "tool" "tool" "user")))
    (let ((parts (alist-get 'content (car (last messages)))))
      (should (equal (alist-get 'text (aref parts 0)) "Attached image(s) from tool result:"))
      (should (= (length (seq-filter (lambda (p) (equal (alist-get 'type p) "image_url"))
                                     parts))
                 2)))))

(ert-deftest llm-pi-ai-occ-test-convert-empty-tool-output ()
  "(upstream openai-completions-tool-result-images) Placeholders for no text."
  (let ((model (llm-pi-ai-occ-test--model :input '("text" "image"))))
    (dolist (text '("" "\r\n" " \n\t"))
      (let ((messages (llm-pi-ai-api-openai-completions-convert-messages
                       model
                       (llm-pi-ai-occ-test--context
                        (llm-pi-ai-occ-test--user "run")
                        (llm-pi-ai-occ-test--assistant
                         model (llm-pi-ai-make-tool-call :id "t" :name "bash"
                                                         :arguments (llm-pi-ai-json-object)))
                        (llm-pi-ai-make-tool-result-message
                         :tool-call-id "t" :tool-name "bash"
                         :content (list (llm-pi-ai-make-text-content :text text))
                         :timestamp 3))
                       (llm-pi-ai-api-openai-completions-get-compat model))))
        (should (equal (alist-get 'content (nth 2 messages)) "(no tool output)"))))
    ;; only an image: "(see attached image)", and the image follows
    (let ((messages (llm-pi-ai-api-openai-completions-convert-messages
                     model
                     (llm-pi-ai-occ-test--context
                      (llm-pi-ai-occ-test--user "run")
                      (llm-pi-ai-occ-test--assistant
                       model (llm-pi-ai-make-tool-call :id "t" :name "bash"
                                                       :arguments (llm-pi-ai-json-object)))
                      (llm-pi-ai-make-tool-result-message
                       :tool-call-id "t" :tool-name "bash"
                       :content (list (llm-pi-ai-make-image-content :data "QQ==" :mime-type "image/png"))
                       :timestamp 3))
                     (llm-pi-ai-api-openai-completions-get-compat model))))
      (should (equal (alist-get 'content (nth 2 messages)) "(see attached image)"))
      (should (equal (alist-get 'role (nth 3 messages)) "user")))))

(ert-deftest llm-pi-ai-occ-test-convert-compat-switches ()
  "requiresToolResultName, requiresAssistantAfterToolResult, thinking as text."
  (let* ((model (llm-pi-ai-occ-test--model
                 :reasoning t
                 :compat '((requires-tool-result-name . t)
                           (requires-assistant-after-tool-result . t)
                           (requires-thinking-as-text . t))))
         (messages (llm-pi-ai-occ-test--convert
                    model
                    (llm-pi-ai-occ-test--user "q")
                    (llm-pi-ai-occ-test--assistant
                     model
                     (llm-pi-ai-make-thinking-content :thinking "pondering"
                                                      :thinking-signature "reasoning")
                     (llm-pi-ai-make-text-content :text "answer")
                     (llm-pi-ai-make-tool-call :id "c1" :name "f"
                                               :arguments (llm-pi-ai-json-object "a" 1)))
                    (llm-pi-ai-make-tool-result-message
                     :tool-call-id "c1" :tool-name "f"
                     :content (list (llm-pi-ai-make-text-content :text "r")) :timestamp 3)
                    (llm-pi-ai-occ-test--user "next"))))
    (should (equal (nth 1 messages)
                   (concat "{\"role\":\"assistant\",\"content\":[{\"type\":\"text\",\"text\":\"pondering\"},"
                           "{\"type\":\"text\",\"text\":\"answer\"}],"
                           "\"tool_calls\":[{\"id\":\"c1\",\"type\":\"function\",\"function\":"
                           "{\"name\":\"f\",\"arguments\":\"{\\\"a\\\":1}\"}}]}")))
    (should (equal (nth 2 messages)
                   "{\"role\":\"tool\",\"content\":\"r\",\"tool_call_id\":\"c1\",\"name\":\"f\"}"))
    (should (equal (nth 3 messages)
                   "{\"role\":\"assistant\",\"content\":\"I have processed the tool results.\"}"))
    (should (equal (nth 4 messages) "{\"role\":\"user\",\"content\":\"next\"}"))))

(ert-deftest llm-pi-ai-occ-test-convert-assistant-reasoning-fields ()
  "The thinking signature picks the field; text-less thinking-only turns are kept
with an empty content only when they have tool calls."
  (let ((model (llm-pi-ai-occ-test--model :reasoning t)))
    (should (equal (llm-pi-ai-occ-test--convert
                    model (llm-pi-ai-occ-test--assistant
                           model
                           (llm-pi-ai-make-thinking-content :thinking "a" :thinking-signature "reasoning")
                           (llm-pi-ai-make-thinking-content :thinking "b" :thinking-signature "reasoning")
                           (llm-pi-ai-make-text-content :text "t")))
                   '("{\"role\":\"assistant\",\"content\":\"t\",\"reasoning\":\"a\\nb\"}")))
    ;; nothing to send: skipped
    (should-not (llm-pi-ai-occ-test--convert
                 model (llm-pi-ai-occ-test--assistant
                        model (llm-pi-ai-make-text-content :text "  "))))
    ;; a signature that is no known field: thinking is not sent
    (should (equal (llm-pi-ai-occ-test--convert
                    model (llm-pi-ai-occ-test--assistant
                           model
                           (llm-pi-ai-make-thinking-content :thinking "a" :thinking-signature "sig")
                           (llm-pi-ai-make-text-content :text "t")))
                   '("{\"role\":\"assistant\",\"content\":\"t\"}")))
    ;; requiresReasoningContent adds an empty reasoning_content
    (should (equal (llm-pi-ai-occ-test--convert
                    (llm-pi-ai-occ-test--deepseek)
                    (llm-pi-ai-occ-test--assistant
                     (llm-pi-ai-occ-test--deepseek) (llm-pi-ai-make-text-content :text "t")))
                   '("{\"role\":\"assistant\",\"content\":\"t\",\"reasoning_content\":\"\"}")))
    ;; opencode-go maps `reasoning' to `reasoning_content'
    (let ((go (llm-pi-ai-occ-test--model :provider "opencode-go" :reasoning t
                                         :base-url "https://opencode.ai/zen/go/v1")))
      (should (equal (llm-pi-ai-occ-test--convert
                      go (llm-pi-ai-occ-test--assistant
                          go
                          (llm-pi-ai-make-thinking-content :thinking "a" :thinking-signature "reasoning")
                          (llm-pi-ai-make-text-content :text "t")))
                     '("{\"role\":\"assistant\",\"content\":\"t\",\"reasoning_content\":\"a\"}"))))))

(ert-deftest llm-pi-ai-occ-test-convert-pipe-tool-call-ids ()
  "Ids of the Responses API form are made fit for Chat Completions."
  (should (equal (llm-pi-ai-oc--normalize-tool-call-id
                  (llm-pi-ai-occ-test--model) "call_1|item+/=2")
                 "call_1_item___2"))
  (should (equal (llm-pi-ai-oc--normalize-tool-call-id (llm-pi-ai-occ-test--model) "call|")
                 "call"))
  (let ((long (llm-pi-ai-oc--normalize-tool-call-id
               (llm-pi-ai-occ-test--model) (concat "call_1|" (make-string 400 ?a)))))
    (should (= (length long) 15))
    (should (string-prefix-p "call_1_" long))
    (let ((hash (llm-pi-ai-short-hash (concat "call_1|" (make-string 400 ?a)))))
      (should (equal long (concat "call_1_" (substring hash 0 (min 8 (length hash))))))))
  (should (equal (llm-pi-ai-oc--normalize-tool-call-id (llm-pi-ai-occ-test--model)
                                                       (make-string 50 ?b))
                 (make-string 40 ?b)))
  (should (equal (llm-pi-ai-oc--normalize-tool-call-id
                  (llm-pi-ai-occ-test--model :provider "acme") (make-string 50 ?b))
                 (make-string 50 ?b))))

(ert-deftest llm-pi-ai-occ-test-convert-kimi-tool-system-message ()
  "With mid-conversation tool additions, tools go in a system message."
  (let* ((model (llm-pi-ai-occ-test--model
                 :compat '((supports-mid-convo-system-messages . t)
                           (supports-mid-convo-tool-additions . t))))
         (tool (llm-pi-ai-make-tool :name "later" :description "L"
                                    :parameters '((type . "object"))))
         (messages (llm-pi-ai-occ-test--convert
                    model
                    (llm-pi-ai-make-system-message :content "base" :timestamp 0)
                    (llm-pi-ai-occ-test--user "hi")
                    (llm-pi-ai-make-system-message :content "now with a tool"
                                                   :tools-added (list tool) :timestamp 5))))
    (should (equal messages
                   (list "{\"role\":\"system\",\"content\":\"base\"}"
                         "{\"role\":\"user\",\"content\":\"hi\"}"
                         (concat "{\"role\":\"system\",\"tools\":[{\"type\":\"function\",\"function\":"
                                 "{\"name\":\"later\",\"description\":\"L\",\"parameters\":{\"type\":\"object\"}}}]}")
                         "{\"role\":\"system\",\"content\":\"now with a tool\"}")))))

;;; Reasoning details

(defconst llm-pi-ai-occ-test--encrypted-detail
  "{\"type\":\"reasoning.encrypted\",\"id\":\"call_1\",\"data\":\"enc==\",\"index\":0}"
  "A reasoning_details entry.")

(defun llm-pi-ai-occ-test--rd-model ()
  "Return the OpenRouter Gemini model of upstream's reasoning details tests."
  (llm-pi-ai-occ-test--model :id "google/gemini-test" :provider "openrouter"
                             :base-url "https://openrouter.ai/api/v1"
                             :reasoning t :context-window 100000))

(defun llm-pi-ai-occ-test--rd-run (chunks &optional messages)
  "Stream the reasoning details model over CHUNKS, after MESSAGES; return the message."
  (llm-pi-ai-occ-test--result
   (llm-pi-ai-occ-test--stream
    (llm-pi-ai-occ-test--rd-model)
    (llm-pi-ai-normalize-context
     (llm-pi-ai-make-context
      :messages (or messages (list (llm-pi-ai-occ-test--user "hi")))
      :tools (list (llm-pi-ai-make-tool :name "read" :description "Read"
                                        :parameters '((type . "object"))))))
    nil chunks)))

(defconst llm-pi-ai-occ-test--rd-tool-call-chunk
  (llm-pi-ai-occ-test--chunk
   "{\"tool_calls\":[{\"index\":0,\"id\":\"call_1\",\"type\":\"function\",\"function\":{\"name\":\"read\",\"arguments\":\"{\\\"path\\\":\\\"README.md\\\"}\"}}]}"))

(ert-deftest llm-pi-ai-occ-test-reasoning-details-preserved-and-replayed ()
  "(upstream openai-completions-reasoning-details) Kept in the signature, replayed."
  (llm-pi-ai-occ-test--with-rig
    (let* ((detail llm-pi-ai-occ-test--encrypted-detail)
           (message (llm-pi-ai-occ-test--rd-run
                     (list (llm-pi-ai-occ-test--chunk
                            (format "{\"reasoning_details\":[%s]}" detail))
                           llm-pi-ai-occ-test--rd-tool-call-chunk
                           (llm-pi-ai-occ-test--chunk "{}" "tool_calls"))))
           (thinking (car (llm-pi-ai-message-content message)))
           (tool-call (cadr (llm-pi-ai-message-content message))))
      (should (llm-pi-ai-thinking-content-p thinking))
      (should (equal (llm-pi-ai-thinking-content-thinking thinking) ""))
      (should (equal (llm-pi-ai-thinking-content-thinking-signature thinking)
                     (format "[%s]" detail)))
      (should (equal (llm-pi-ai-tool-call-name tool-call) "read"))
      (should (equal (gethash "path" (llm-pi-ai-tool-call-arguments tool-call)) "README.md"))
      ;; replay
      (setq llm-pi-ai-occ-test--requests nil)
      (llm-pi-ai-occ-test--rd-run
       (list (llm-pi-ai-occ-test--chunk "{\"content\":\"ok\"}")
             (llm-pi-ai-occ-test--chunk "{}" "stop"))
       (list (llm-pi-ai-occ-test--user "hi") message
             (llm-pi-ai-make-tool-result-message
              :tool-call-id "call_1" :tool-name "read"
              :content (list (llm-pi-ai-make-text-content :text "text")) :timestamp 9)))
      (let* ((body (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body)))
             (assistant (seq-find (lambda (m) (equal (gethash "role" m) "assistant"))
                                  (gethash "messages" body))))
        (should (equal (llm-pi-ai-oc--serialize (gethash "reasoning_details" assistant))
                       (format "[%s]" detail)))
        (should-not (gethash "reasoning" assistant))
        (should-not (gethash "reasoning_content" assistant))))))

(ert-deftest llm-pi-ai-occ-test-reasoning-details-legacy-tool-call-signature ()
  "(upstream openai-completions-reasoning-details) Older messages: from the tool call."
  (llm-pi-ai-occ-test--with-rig
    (let* ((tool-call (llm-pi-ai-make-tool-call
                       :id "call_1" :name "read"
                       :arguments (llm-pi-ai-json-object "path" "x")
                       :thought-signature llm-pi-ai-occ-test--encrypted-detail))
           (assistant (llm-pi-ai-make-assistant-message
                       :content (list tool-call)
                       :api "openai-completions" :provider "openrouter"
                       :model "google/gemini-test" :stop-reason "toolUse" :timestamp 2)))
      (llm-pi-ai-occ-test--rd-run
       (list (llm-pi-ai-occ-test--chunk "{}" "stop"))
       (list (llm-pi-ai-occ-test--user "hi") assistant
             (llm-pi-ai-make-tool-result-message
              :tool-call-id "call_1" :tool-name "read"
              :content (list (llm-pi-ai-make-text-content :text "t")) :timestamp 9)))
      (let* ((body (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body)))
             (message (seq-find (lambda (m) (equal (gethash "role" m) "assistant"))
                                (gethash "messages" body))))
        (should (equal (llm-pi-ai-oc--serialize (gethash "reasoning_details" message))
                       (format "[%s]" llm-pi-ai-occ-test--encrypted-detail)))))))

(ert-deftest llm-pi-ai-occ-test-reasoning-details-merge-deltas ()
  "(upstream openai-completions-reasoning-details) Text and summary deltas merge."
  (llm-pi-ai-occ-test--with-rig
    (let* ((text-delta "{\"type\":\"reasoning.text\",\"text\":\"The\",\"index\":0}")
           (text-delta-2 (concat "{\"type\":\"reasoning.text\",\"text\":\" user wants the time.\","
                                 "\"signature\":\"sha256:text-signature\","
                                 "\"format\":\"openai-responses-v1\",\"index\":0}"))
           (summary "{\"type\":\"reasoning.summary\",\"summary\":\"Looked\",\"index\":0}")
           (summary-2 (concat "{\"type\":\"reasoning.summary\",\"summary\":\" up time.\","
                              "\"format\":\"openai-responses-v1\",\"index\":0}"))
           (later (concat "{\"type\":\"reasoning.summary\",\"summary\":\"After encrypted block.\","
                          "\"format\":\"openai-responses-v1\",\"index\":0}"))
           (details (list text-delta text-delta-2 summary summary-2
                          llm-pi-ai-occ-test--encrypted-detail later))
           (message (llm-pi-ai-occ-test--rd-run
                     (append (mapcar (lambda (d)
                                       (llm-pi-ai-occ-test--chunk
                                        (format "{\"reasoning_details\":[%s]}" d)))
                                     details)
                             (list llm-pi-ai-occ-test--rd-tool-call-chunk
                                   (llm-pi-ai-occ-test--chunk "{}" "tool_calls")))))
           (thinking (car (llm-pi-ai-message-content message))))
      (should (equal (llm-pi-ai-thinking-content-thinking-signature thinking)
                     (concat "["
                             "{\"type\":\"reasoning.text\",\"text\":\"The user wants the time.\","
                             "\"index\":0,\"signature\":\"sha256:text-signature\","
                             "\"format\":\"openai-responses-v1\"},"
                             "{\"type\":\"reasoning.summary\",\"summary\":\"Looked up time.\","
                             "\"index\":0,\"format\":\"openai-responses-v1\"},"
                             llm-pi-ai-occ-test--encrypted-detail ","
                             later "]"))))))

(ert-deftest llm-pi-ai-occ-test-reasoning-and-content-in-one-delta ()
  "(upstream openai-completions-reasoning-details) Thinking comes before text."
  (llm-pi-ai-occ-test--with-rig
    (let* ((message (llm-pi-ai-occ-test--rd-run
                     (list (llm-pi-ai-occ-test--chunk
                            "{\"role\":\"assistant\",\"reasoning_content\":\"thinking\",\"content\":\"answer\"}"
                            "stop"))))
           (blocks (llm-pi-ai-message-content message)))
      (should (llm-pi-ai-thinking-content-p (nth 0 blocks)))
      (should (equal (llm-pi-ai-thinking-content-thinking (nth 0 blocks)) "thinking"))
      (should (equal (llm-pi-ai-thinking-content-thinking-signature (nth 0 blocks))
                     "reasoning_content"))
      (should (equal (llm-pi-ai-text-content-text (nth 1 blocks)) "answer")))))

(ert-deftest llm-pi-ai-occ-test-reasoning-field-precedence ()
  "The first non-empty reasoning field wins; `opencode-go' renames `reasoning'."
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--rd-run
                    (list (llm-pi-ai-occ-test--chunk
                           "{\"reasoning_content\":\"\",\"reasoning\":\"r1\",\"reasoning_text\":\"r2\"}"
                           "stop")))))
      (should (equal (llm-pi-ai-thinking-content-thinking
                      (car (llm-pi-ai-message-content message)))
                     "r1"))
      (should (equal (llm-pi-ai-thinking-content-thinking-signature
                      (car (llm-pi-ai-message-content message)))
                     "reasoning"))))
  (llm-pi-ai-occ-test--with-rig
    (let ((message (llm-pi-ai-occ-test--result
                    (llm-pi-ai-occ-test--stream
                     (llm-pi-ai-occ-test--model :provider "opencode-go")
                     (llm-pi-ai-occ-test--context) nil
                     (list (llm-pi-ai-occ-test--chunk "{\"reasoning\":\"r\"}" "stop"))))))
      (should (equal (llm-pi-ai-thinking-content-thinking-signature
                      (car (llm-pi-ai-message-content message)))
                     "reasoning_content")))))

(ert-deftest llm-pi-ai-occ-test-tool-call-arguments-error ()
  "Arguments that are not JSON end up empty, with the error recorded."
  (llm-pi-ai-occ-test--with-rig
    (let* ((message (llm-pi-ai-occ-test--result
                     (llm-pi-ai-occ-test--stream
                      (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                      (list (llm-pi-ai-occ-test--chunk
                             "{\"tool_calls\":[{\"index\":0,\"id\":\"c\",\"function\":{\"name\":\"f\",\"arguments\":\"[1,2]\"}}]}")
                            (llm-pi-ai-occ-test--chunk "{}" "tool_calls")))))
           (tool-call (car (llm-pi-ai-message-content message))))
      (should (= (hash-table-count (llm-pi-ai-tool-call-arguments tool-call)) 0))
      (should (string-prefix-p "Tool arguments must be a JSON object"
                               (llm-pi-ai-tool-call-arguments-error tool-call))))))

(ert-deftest llm-pi-ai-occ-test-tool-call-matched-by-id-without-index ()
  "A delta with an id and no index continues the block of that id."
  (llm-pi-ai-occ-test--with-rig
    (let* ((message (llm-pi-ai-occ-test--result
                     (llm-pi-ai-occ-test--stream
                      (llm-pi-ai-occ-test--model) (llm-pi-ai-occ-test--context) nil
                      (list (llm-pi-ai-occ-test--chunk
                             "{\"tool_calls\":[{\"id\":\"c1\",\"function\":{\"name\":\"f\",\"arguments\":\"{\\\"a\\\":\"}}]}")
                            (llm-pi-ai-occ-test--chunk
                             "{\"tool_calls\":[{\"id\":\"c1\",\"function\":{\"arguments\":\"1}\"}}]}")
                            (llm-pi-ai-occ-test--chunk "{}" "tool_calls"))))))
      (should (= (length (llm-pi-ai-message-content message)) 1))
      (should (= (gethash "a" (llm-pi-ai-tool-call-arguments
                               (car (llm-pi-ai-message-content message))))
                 1)))))

;;; Compat

(ert-deftest llm-pi-ai-occ-test-detect-compat-deepseek ()
  (let ((compat (llm-pi-ai-api-openai-completions-detect-compat
                 (llm-pi-ai-occ-test--model :provider "deepseek"
                                            :base-url "https://api.deepseek.com"))))
    (should-not (plist-get compat :supports-store))
    (should-not (plist-get compat :supports-developer-role))
    (should (plist-get compat :supports-reasoning-effort))
    (should (equal (plist-get compat :max-tokens-field) "max_tokens"))
    (should (plist-get compat :requires-reasoning-content-on-assistant-messages))
    (should (equal (plist-get compat :thinking-format) "deepseek"))
    (should (plist-get compat :supports-long-cache-retention))))

(ert-deftest llm-pi-ai-occ-test-detect-compat-providers ()
  "Each provider branch of `detectCompat'."
  (cl-flet ((compat (provider url &optional id)
              (llm-pi-ai-api-openai-completions-detect-compat
               (llm-pi-ai-occ-test--model :provider provider :base-url url
                                          :id (or id "m")))))
    ;; plain OpenAI
    (let ((c (compat "openai" "https://api.openai.com/v1")))
      (should (plist-get c :supports-store))
      (should (plist-get c :supports-developer-role))
      (should (equal (plist-get c :max-tokens-field) "max_completion_tokens"))
      (should (equal (plist-get c :thinking-format) "openai"))
      (should-not (plist-get c :requires-reasoning-content-on-assistant-messages)))
    ;; z.ai
    (let ((c (compat "zai" "https://api.z.ai/api/paas/v4")))
      (should (equal (plist-get c :thinking-format) "zai"))
      (should-not (plist-get c :supports-reasoning-effort))
      (should (equal (plist-get c :max-tokens-field) "max_tokens")))
    (should (equal (plist-get (compat "x" "https://open.bigmodel.cn/api") :thinking-format) "zai"))
    ;; Together
    (let ((c (compat "together" "https://api.together.xyz/v1")))
      (should (equal (plist-get c :thinking-format) "together"))
      (should-not (plist-get c :supports-long-cache-retention)))
    ;; Moonshot
    (let ((c (compat "moonshotai" "https://api.moonshot.ai/v1")))
      (should-not (plist-get c :supports-reasoning-effort))
      (should-not (plist-get c :supports-store)))
    ;; OpenRouter, with and without the Anthropic cache format
    (let ((c (compat "openrouter" "https://openrouter.ai/api/v1" "anthropic/claude")))
      (should (equal (plist-get c :thinking-format) "openrouter"))
      (should (plist-get c :supports-developer-role))
      (should (equal (plist-get c :cache-control-format) "anthropic"))
      (should (plist-get c :send-session-affinity-headers))
      (should (equal (plist-get c :session-affinity-format) "openrouter")))
    (let ((c (compat "openrouter" "https://openrouter.ai/api/v1" "meta/llama")))
      (should-not (plist-get c :supports-developer-role))
      (should-not (plist-get c :cache-control-format)))
    (should (plist-get (compat "openrouter" "https://openrouter.ai/api/v1" "openai/gpt")
                       :supports-developer-role))
    ;; Nvidia, Cerebras, xAI, Cloudflare, Ant Ling, opencode, chutes
    (let ((c (compat "nvidia" "https://integrate.api.nvidia.com/v1")))
      (should (equal (plist-get c :max-tokens-field) "max_tokens"))
      (should-not (plist-get c :supports-long-cache-retention))
      (should-not (plist-get c :supports-reasoning-effort)))
    (let ((c (compat "cerebras" "https://api.cerebras.ai/v1")))
      (should-not (plist-get c :supports-store))
      (should (equal (plist-get c :max-tokens-field) "max_completion_tokens")))
    (let ((c (compat "xai" "https://api.x.ai/v1")))
      (should-not (plist-get c :supports-reasoning-effort))
      (should-not (plist-get c :supports-developer-role)))
    (let ((c (compat "cloudflare-ai-gateway" "https://gateway.ai.cloudflare.com/v1")))
      (should (equal (plist-get c :max-tokens-field) "max_tokens"))
      (should-not (plist-get c :supports-long-cache-retention)))
    (let ((c (compat "cloudflare-workers-ai" "https://api.cloudflare.com/client/v4")))
      (should-not (plist-get c :supports-store))
      (should-not (plist-get c :supports-long-cache-retention))
      (should (equal (plist-get c :max-tokens-field) "max_completion_tokens")))
    (let ((c (compat "ant-ling" "https://api.ant-ling.com/v1")))
      (should (equal (plist-get c :thinking-format) "ant-ling"))
      (should-not (plist-get c :supports-reasoning-effort)))
    (should-not (plist-get (compat "opencode" "https://opencode.ai/zen/v1") :supports-store))
    (should (equal (plist-get (compat "x" "https://llm.chutes.ai/v1") :max-tokens-field)
                   "max_tokens"))
    ;; fixed fields
    (let ((c (compat "openai" "https://api.openai.com/v1")))
      (should (plist-get c :supports-usage-in-streaming))
      (should (plist-get c :supports-finish-reason))
      (should-not (plist-get c :supports-strict-mode)))))

(ert-deftest llm-pi-ai-occ-test-get-compat-overrides ()
  "Model compat entries win, a nil being false; `:null' falls back."
  (let* ((model (llm-pi-ai-occ-test--model
                 :compat '((supports-store) (max-tokens-field . "max_tokens")
                           (supports-developer-role . :null)
                           (thinking-format . "qwen")
                           (supports-strict-mode . t)
                           (open-router-routing ("only" "a"))
                           (vllm-priority . 0)
                           (chat-template-kwargs ("k" . t)))))
         (c (llm-pi-ai-api-openai-completions-get-compat model)))
    (should-not (plist-get c :supports-store))
    (should (equal (plist-get c :max-tokens-field) "max_tokens"))
    (should (plist-get c :supports-developer-role))
    (should (equal (plist-get c :thinking-format) "qwen"))
    (should (plist-get c :supports-strict-mode))
    (should (equal (plist-get c :open-router-routing) '(("only" "a"))))
    (should (eql (plist-get c :vllm-priority) 0))
    (should (equal (plist-get c :chat-template-kwargs) '(("k" . t)))))
  ;; no model compat: the detected one
  (let ((model (llm-pi-ai-occ-test--model)))
    (should (equal (llm-pi-ai-api-openai-completions-get-compat model)
                   (llm-pi-ai-api-openai-completions-detect-compat model)))))

(ert-deftest llm-pi-ai-occ-test-strict-mode-in-tools ()
  "`strict' is sent only when the provider supports strict mode."
  (let ((p (llm-pi-ai-occ-test--params
            (llm-pi-ai-occ-test--model :compat '((supports-strict-mode . t)))
            nil (llm-pi-ai-normalize-context
                 (llm-pi-ai-make-context
                  :tools (list (llm-pi-ai-make-tool :name "f" :description "F"
                                                    :parameters '((type . "object"))))
                  :messages (list (llm-pi-ai-occ-test--user "x")))))))
    (should (eq (llm-pi-ai-occ-test--ref p "tools" 0 "function" "strict") :false)))
  (let ((p (llm-pi-ai-occ-test--params
            (llm-pi-ai-occ-test--model)
            nil (llm-pi-ai-normalize-context
                 (llm-pi-ai-make-context
                  :tools (list (llm-pi-ai-make-tool :name "f" :description "F"
                                                    :parameters '((type . "object"))))
                  :messages (list (llm-pi-ai-occ-test--user "x")))))))
    (should-not (gethash "strict" (llm-pi-ai-occ-test--ref p "tools" 0 "function")))))

;;; Transcript and token estimate helpers

(ert-deftest llm-pi-ai-occ-test-collapse-system-messages ()
  "A model without mid-conversation system messages gets one leading prompt."
  (let* ((context (llm-pi-ai-make-context
                   :messages (list (llm-pi-ai-make-system-message :content "base" :timestamp 0)
                                   (llm-pi-ai-occ-test--user "hi")
                                   (llm-pi-ai-make-system-message
                                    :content "more" :timestamp 5
                                    :sections '(("a" . "A") ("b" . nil))))))
         (collapsed (llm-pi-ai-oc--resolve-transcript context nil)))
    (should (equal (mapcar #'llm-pi-ai-message-role (llm-pi-ai-context-messages collapsed))
                   '("system" "user")))
    (should (equal (llm-pi-ai-system-message-text
                    (car (llm-pi-ai-context-messages collapsed)))
                   "base\n\nmore\n\nA"))
    (should (eq (llm-pi-ai-oc--resolve-transcript context t) context))))

(ert-deftest llm-pi-ai-occ-test-simple-options-clamp-max-tokens ()
  "`stream-simple' holds max tokens to what the window has left."
  (llm-pi-ai-occ-test--with-rig
    (let ((model (llm-pi-ai-occ-test--model :context-window 6000 :max-tokens 5000)))
      (llm-pi-ai-api-openai-completions-stream-simple
       model (llm-pi-ai-occ-test--context (llm-pi-ai-occ-test--user (make-string 3500 ?x)))
       '(:api-key "k"))
      ;; 3500 chars / 3.5 = 1000 tokens; 6000 - 1000 - 4096 = 904
      (should (= (gethash "max_completion_tokens"
                          (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body)))
                 904)))))

(ert-deftest llm-pi-ai-occ-test-stream-simple-reasoning-levels ()
  "`:reasoning' is clamped: off sends no effort; unsupported levels move up."
  (let ((model (llm-pi-ai-occ-test--deepseek)))
    (should (equal (gethash "reasoning_effort"
                            (llm-pi-ai-occ-test--with-rig
                              (llm-pi-ai-api-openai-completions-stream-simple
                               model (llm-pi-ai-occ-test--context) '(:api-key "k" :reasoning "minimal"))
                              (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body))))
                   "low"))
    (should-not (gethash "reasoning_effort"
                         (llm-pi-ai-occ-test--with-rig
                           (llm-pi-ai-api-openai-completions-stream-simple
                            model (llm-pi-ai-occ-test--context) '(:api-key "k" :reasoning "off"))
                           (llm-pi-ai-occ-test--json (llm-pi-ai-occ-test--body)))))))

;;; The caller's view

(ert-deftest llm-pi-ai-occ-test-caller-example ()
  "The example of how a caller builds a context and reads the events."
  (llm-pi-ai-occ-test--with-rig
    (let ((process-environment (list "DEEPSEEK_API_KEY=sk-test"))
          (deltas nil) (final nil))
      (let* ((models (llm-pi-ai-create-models)))
        (llm-pi-ai-models-set-provider models (llm-pi-ai-deepseek-provider))
        (let* ((model (llm-pi-ai-models-get-model models "deepseek" "deepseek-flash"))
               (context (llm-pi-ai-make-context
                         :system-prompt "Be brief."
                         :tools (list (llm-pi-ai-make-tool
                                       :name "get_weather" :description "Weather of a city"
                                       :parameters '((type . "object")
                                                     (properties . ((city . ((type . "string")))))
                                                     (required . ["city"]))))
                         :messages (list (llm-pi-ai-make-user-message
                                          :content "Weather in Paris?"
                                          :timestamp (round (* 1000 (float-time)))))))
               (stream (llm-pi-ai-models-stream-simple models model context
                                                       '(:reasoning "high"))))
          (llm-pi-ai-event-stream-iterate
           stream
           (lambda (event)
             (when (equal (llm-pi-ai-assistant-message-event-type event) "text_delta")
               (push (llm-pi-ai-assistant-message-event-delta event) deltas)))
           (lambda () (setq final (llm-pi-ai-event-stream-result stream))))
          (let ((req (llm-pi-ai-occ-test--request)))
            (llm-pi-ai-occ-test--feed
             req llm-pi-ai-occ-test--head
             (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\"Sunny\"}"))
             (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{\"content\":\" in Paris\"}"))
             (llm-pi-ai-occ-test--data (llm-pi-ai-occ-test--chunk "{}" "stop"))
             "data: [DONE]\n\n")
            (llm-pi-ai-occ-test--exit req))))
      (should (equal (nreverse deltas) '("Sunny" " in Paris")))
      (should (equal (llm-pi-ai-assistant-message-stop-reason final) "stop")))))

;;; Entry file

(ert-deftest llm-pi-ai-occ-test-entry-file-loads ()
  (should (featurep 'llm-pi-ai))
  (should (fboundp 'llm-pi-ai-models-stream-simple))
  (should (fboundp 'llm-pi-ai-create-assistant-message-event-stream))
  (should (fboundp 'llm-pi-ai-parse-streaming-json)))

(provide 'llm-pi-ai-api-openai-completions-test)

;;; llm-pi-ai-api-openai-completions-test.el ends here
