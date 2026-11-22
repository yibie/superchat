;;; superchat-bridge-api-anthropic-messages-test.el --- Tests for superchat-bridge-api-anthropic-messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-api-anthropic-messages.el'.  Upstream's
;; `test/anthropic-*.test.ts' replace the SDK client with a fake or capture
;; the payload with an `onPayload' that throws; these replace curl with a
;; fake start function (`superchat-bridge-http-start-function') fed bytes
;; in pieces, and capture the payload the same way.  No test touches the
;; network or reads a key.  Tests marked "(upstream NAME)" are translated
;; from `test/NAME.test.ts'; the others follow the source's behaviour.
;; Response bodies are built as upstream's `createSseResponse' builds
;; them: events joined by a newline, the last one not ended by a blank
;; line.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'superchat-bridge)
(require 'superchat-bridge-provider-anthropic)
(require 'superchat-bridge-api-anthropic-messages)

;;; Rig

(cl-defstruct (superchat-bridge-amt-test--req (:copier nil))
  "A request the fake curl was started for."
  args config stdin output exit (killed 0))

(defvar superchat-bridge-amt-test--requests nil
  "The requests made so far, oldest first.")

(defvar superchat-bridge-amt-test--timers nil
  "Fake timers: a list of (SECONDS FUNCTION CANCELLED), oldest first.")

(defmacro superchat-bridge-amt-test--with-rig (&rest body)
  "Run BODY with a fake curl and fake timers and a clean environment."
  (declare (indent 0))
  `(let* ((process-environment (list "HOME=/nonexistent"))
          (superchat-bridge-amt-test--requests nil)
          (superchat-bridge-amt-test--timers nil)
          (superchat-bridge-retry-run-at-time-function
           (lambda (seconds _repeat function)
             (let ((timer (list seconds function nil)))
               (setq superchat-bridge-amt-test--timers
                     (append superchat-bridge-amt-test--timers (list timer)))
               timer)))
          (superchat-bridge-retry-cancel-timer-function
           (lambda (timer) (setf (nth 2 timer) t)))
          (superchat-bridge-http-start-function
           (lambda (args config stdin on-output on-exit)
             (let ((req (make-superchat-bridge-amt-test--req
                         :args args :config config :stdin stdin
                         :output on-output :exit on-exit)))
               (setq superchat-bridge-amt-test--requests
                     (append superchat-bridge-amt-test--requests (list req)))
               (lambda () (cl-incf (superchat-bridge-amt-test--req-killed req)))))))
     ,@body))

(defun superchat-bridge-amt-test--request (&optional index)
  "Return request INDEX (default 0) of the rig."
  (nth (or index 0) superchat-bridge-amt-test--requests))

(defun superchat-bridge-amt-test--feed-pieces (req text)
  "Feed TEXT to REQ cut into pieces of 1 to 13 bytes, cycling."
  (let* ((bytes (encode-coding-string text 'utf-8))
         (start 0) (size 1))
    (while (< start (length bytes))
      (let ((end (min (length bytes) (+ start size))))
        (funcall (superchat-bridge-amt-test--req-output req) (substring bytes start end))
        (setq start end size (if (>= size 13) 1 (1+ size)))))))

(defun superchat-bridge-amt-test--exit (req &optional code stderr)
  "End REQ's curl with exit CODE (default 0) and STDERR text."
  (funcall (superchat-bridge-amt-test--req-exit req) (or code 0) (or stderr "")))

(defconst superchat-bridge-amt-test--head
  "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n")

(defun superchat-bridge-amt-test--respond (req text &optional head)
  "Answer REQ with HEAD (default 200 SSE) and TEXT in pieces, then exit 0."
  (superchat-bridge-amt-test--feed-pieces
   req (concat (or head superchat-bridge-amt-test--head) text))
  (superchat-bridge-amt-test--exit req))

(defun superchat-bridge-amt-test--e (event data)
  "Return the SSE event EVENT whose DATA is Lisp JSON data or a string."
  (cons event (if (stringp data) data (superchat-bridge-am--serialize data))))

(defun superchat-bridge-amt-test--sse (events)
  "Return EVENTS as upstream's `createSseResponse' writes them."
  (mapconcat (lambda (e) (format "event: %s\ndata: %s\n" (car e) (cdr e))) events "\n"))

(defun superchat-bridge-amt-test--sse-ended (events)
  "Return EVENTS each ended by a blank line, as a live server sends them."
  (mapconcat (lambda (e) (format "event: %s\ndata: %s\n\n" (car e) (cdr e))) events ""))

(defun superchat-bridge-amt-test--minimal-events ()
  "Return upstream's `minimalAnthropicEvents'."
  (list
   (superchat-bridge-amt-test--e
    "message_start"
    '((type . "message_start")
      (message . ((id . "msg_test")
                  (usage . ((input_tokens . 12) (output_tokens . 0)
                            (cache_read_input_tokens . 0)
                            (cache_creation_input_tokens . 0)))))))
   (superchat-bridge-amt-test--e
    "content_block_start"
    '((type . "content_block_start") (index . 0)
      (content_block . ((type . "text") (text . "")))))
   (superchat-bridge-amt-test--e
    "content_block_delta"
    '((type . "content_block_delta") (index . 0)
      (delta . ((type . "text_delta") (text . "Hello")))))
   (superchat-bridge-amt-test--e
    "content_block_stop" '((type . "content_block_stop") (index . 0)))
   (superchat-bridge-amt-test--e
    "message_delta"
    '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
      (usage . ((input_tokens . 12) (output_tokens . 5)
                (cache_read_input_tokens . 0) (cache_creation_input_tokens . 0)))))
   (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop")))))

(defun superchat-bridge-amt-test--norm (value)
  "Return parsed JSON VALUE with objects as alists sorted by key."
  (cond ((hash-table-p value)
         (let (entries)
           (maphash (lambda (k v) (push (cons k (superchat-bridge-amt-test--norm v)) entries))
                    value)
           (sort entries (lambda (a b) (string< (car a) (car b))))))
        ((vectorp value) (vconcat (mapcar #'superchat-bridge-amt-test--norm value)))
        (t value)))

(defun superchat-bridge-amt-test--parse (text)
  "Parse the JSON TEXT and normalize it for comparison."
  (superchat-bridge-amt-test--norm
   (json-parse-string text :object-type 'hash-table :array-type 'array
                      :null-object :null :false-object :false)))

(defun superchat-bridge-amt-test--js (data)
  "Return Lisp JSON DATA normalized for comparison."
  (superchat-bridge-amt-test--parse (superchat-bridge-am--serialize data)))

(defun superchat-bridge-amt-test--get (value &rest keys)
  "Return the member of normalized VALUE along KEYS (strings or integers)."
  (dolist (key keys value)
    (setq value (if (stringp key) (cdr (assoc key value)) (aref value key)))))

(defun superchat-bridge-amt-test--body (&optional index)
  "Return the JSON body of request INDEX, normalized."
  (superchat-bridge-amt-test--parse
   (superchat-bridge-amt-test--req-stdin (superchat-bridge-amt-test--request index))))

(defun superchat-bridge-amt-test--header (name &optional index)
  "Return the value of header NAME of request INDEX, or nil."
  (let ((config (superchat-bridge-amt-test--req-config
                 (superchat-bridge-amt-test--request index)))
        (case-fold-search t))
    (and (string-match (format "^header = \"%s: \\(.*\\)\"$" (regexp-quote name)) config)
         (match-string 1 config))))

(defun superchat-bridge-amt-test--types (events)
  "Return the types of EVENTS."
  (mapcar #'superchat-bridge-assistant-message-event-type events))

;;; Models and contexts

(defun superchat-bridge-amt-test--catalog (id)
  "Return the Anthropic catalog model ID."
  (or (cdr (assoc id (superchat-bridge-anthropic-models)))
      (error "No model %s" id)))

(defun superchat-bridge-amt-test--model (&rest args)
  "Return an anthropic-messages model, ARGS over the defaults."
  (apply #'superchat-bridge-make-model
         (append args
                 (list :id "claude-test" :name "Claude Test"
                       :api "anthropic-messages" :provider "anthropic"
                       :base-url "http://127.0.0.1:9" :reasoning nil
                       :input '("text") :cost (superchat-bridge-make-model-cost)
                       :context-window 200000 :max-tokens 1024))))

(defun superchat-bridge-amt-test--with-compat (model compat)
  "Return a copy of MODEL whose compat is COMPAT."
  (let ((copy (copy-sequence model)))
    (setf (superchat-bridge-model-compat copy) compat)
    copy))

(defun superchat-bridge-amt-test--user (text &optional timestamp)
  "Return a user message with TEXT."
  (superchat-bridge-make-user-message :content text :timestamp (or timestamp 1)))

(defun superchat-bridge-amt-test--context (&rest messages)
  "Return a normalized context of MESSAGES, or of one user message."
  (superchat-bridge-normalize-context
   (superchat-bridge-make-context
    :messages (or messages (list (superchat-bridge-amt-test--user "Hello"))))))

(defun superchat-bridge-amt-test--usage ()
  "Return a zero usage."
  (superchat-bridge-make-usage :cost (superchat-bridge-make-cost)))

(defun superchat-bridge-amt-test--stream (model context options events)
  "Stream MODEL and CONTEXT with OPTIONS, answer with EVENTS, return the result."
  (let* ((stream (superchat-bridge-api-anthropic-messages-stream
                  model context (append options (list :api-key "test-key"))))
         (req (superchat-bridge-amt-test--request
               (1- (length superchat-bridge-amt-test--requests)))))
    (superchat-bridge-amt-test--respond req (superchat-bridge-amt-test--sse events))
    (superchat-bridge-event-stream-result stream)))

(defun superchat-bridge-amt-test--capture (model context options &optional simple)
  "Return (PAYLOAD . MESSAGE): the body `:on-payload' saw, and the result.
The hook signals after capturing, as upstream's `PayloadCaptured'.
SIMPLE uses `stream-simple'."
  (let* ((payload nil)
         (stream (funcall (if simple
                              #'superchat-bridge-api-anthropic-messages-stream-simple
                            #'superchat-bridge-api-anthropic-messages-stream)
                          model context
                          (append (list :api-key "fake-key"
                                        :on-payload (lambda (params _model)
                                                      (setq payload params)
                                                      (error "payload captured")))
                                  options)))
         (message (superchat-bridge-event-stream-result stream)))
    (should payload)
    (should (equal (superchat-bridge-assistant-message-error-message message)
                   "payload captured"))
    (should-not superchat-bridge-amt-test--requests)
    (cons (superchat-bridge-amt-test--js payload) message)))

(defun superchat-bridge-amt-test--simple-payload (model &optional options context)
  "Return the payload `stream-simple' builds for MODEL with OPTIONS."
  (superchat-bridge-amt-test--with-rig
    (car (superchat-bridge-amt-test--capture
          model (if context
                    (superchat-bridge-normalize-context context)
                  (superchat-bridge-amt-test--context))
          options t))))

;;; SSE parsing (upstream anthropic-sse-parsing)

(ert-deftest superchat-bridge-amt-test-provider-events-in-order ()
  "(upstream anthropic-sse-parsing) Parsed events go to the hook in order."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--catalog "claude-haiku-4-5"))
           (seen nil) (models nil)
           (result (superchat-bridge-amt-test--stream
                    model (superchat-bridge-amt-test--context)
                    (list :on-provider-stream-event
                          (lambda (event event-model)
                            (push (gethash "type" event) seen)
                            (push event-model models)))
                    (superchat-bridge-amt-test--minimal-events))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
      (should (equal (nreverse seen)
                     '("message_start" "content_block_start" "content_block_delta"
                       "content_block_stop" "message_delta" "message_stop")))
      (should (cl-every (lambda (m) (eq m model)) models))
      (should (= (length models) 6)))))

(defun superchat-bridge-amt-test--response-model-events (response-model block)
  "Return upstream's `createResponseModelSseResponse' events."
  (list
   (superchat-bridge-amt-test--e
    "message_start"
    `((type . "message_start")
      (message . ((id . "msg_response_model") (model . ,response-model)
                  (usage . ((input_tokens . 100) (output_tokens . 0)))))))
   (superchat-bridge-amt-test--e
    "content_block_start"
    `((type . "content_block_start") (index . 0) (content_block . ,block)))
   (superchat-bridge-amt-test--e
    "content_block_stop" '((type . "content_block_stop") (index . 0)))
   (superchat-bridge-amt-test--e
    "message_delta"
    '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
      (usage . ((input_tokens . 100) (output_tokens . 20)))))
   (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop")))))

(ert-deftest superchat-bridge-amt-test-relabelled-model-keeps-signed-thinking ()
  "(upstream anthropic-sse-parsing) A proxy's model name does not drop thinking."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--catalog "claude-opus-5"))
           (context (superchat-bridge-amt-test--context))
           (first (superchat-bridge-amt-test--stream
                   model context nil
                   (superchat-bridge-amt-test--response-model-events
                    "kimi-for-coding"
                    '((type . "thinking") (thinking . "reasoning")
                      (signature . "signature")))))
           (replayed (seq-find #'superchat-bridge-assistant-message-p
                               (superchat-bridge-transform-messages
                                (append (superchat-bridge-context-messages context)
                                        (list first))
                                model)))
           (block (car (superchat-bridge-message-content replayed))))
      (should (equal (superchat-bridge-assistant-message-model first) "claude-opus-5"))
      (should (equal (superchat-bridge-assistant-message-response-model first)
                     "kimi-for-coding"))
      (should (= (length (superchat-bridge-message-content replayed)) 1))
      (should (superchat-bridge-thinking-content-p block))
      (should (equal (superchat-bridge-thinking-content-thinking block) "reasoning"))
      (should (equal (superchat-bridge-thinking-content-thinking-signature block)
                     "signature")))))

(ert-deftest superchat-bridge-amt-test-fallback-model-prices-usage ()
  "(upstream anthropic-sse-parsing) A returned fallback model prices the usage."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--with-compat
                   (superchat-bridge-amt-test--catalog "claude-opus-5")
                   '((allowed-fallback-models
                      . ((("provider" . "anthropic") ("model" . "fallback-model")
                          ("cost" . (("input" . 3) ("output" . 5)
                                     ("cacheRead" . 0) ("cacheWrite" . 0)))))))))
           (result (superchat-bridge-amt-test--stream
                    model (superchat-bridge-amt-test--context) nil
                    (superchat-bridge-amt-test--response-model-events
                     "fallback-model" '((type . "text") (text . "done")))))
           (cost (superchat-bridge-usage-cost (superchat-bridge-assistant-message-usage result))))
      (should (equal (superchat-bridge-assistant-message-model result) "claude-opus-5"))
      (should (equal (superchat-bridge-assistant-message-response-model result)
                     "fallback-model"))
      (should (< (abs (- (superchat-bridge-cost-input cost) 0.0003)) 1e-10))
      (should (< (abs (- (superchat-bridge-cost-output cost) 0.0001)) 1e-10))
      ;; the request named the fallbacks and its beta
      (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body)
                                                     "fallbacks")
                     (vector '(("model" . "fallback-model")))))
      (should (string-match-p "server-side-fallback-2026-07-01"
                              (superchat-bridge-amt-test--header "anthropic-beta"))))))

(ert-deftest superchat-bridge-amt-test-fallback-after-output-fails ()
  "(upstream anthropic-sse-parsing) A fallback after output began is an error."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-opus-5")
                   (superchat-bridge-amt-test--context) nil
                   (list
                    (superchat-bridge-amt-test--e
                     "message_start"
                     '((type . "message_start")
                       (message . ((id . "msg_fallback") (model . "claude-opus-5")
                                   (usage . ((input_tokens . 1) (output_tokens . 0)))))))
                    (superchat-bridge-amt-test--e
                     "content_block_start"
                     '((type . "content_block_start") (index . 0)
                       (content_block . ((type . "text") (text . "partial")))))
                    (superchat-bridge-amt-test--e
                     "content_block_stop" '((type . "content_block_stop") (index . 0)))
                    (superchat-bridge-amt-test--e
                     "content_block_start"
                     '((type . "content_block_start") (index . 1)
                       (content_block . ((type . "fallback")
                                         (from . ((model . "claude-opus-5")))
                                         (to . ((model . "claude-opus-4-8")))))))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
      (should (string-match-p "unsupported mid-output model fallback"
                              (superchat-bridge-assistant-message-error-message result))))))

(ert-deftest superchat-bridge-amt-test-payload-replacement-keeps-streaming ()
  "(upstream anthropic-sse-parsing) An `onPayload' replacement still streams."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-fable-5-1")
                   (superchat-bridge-amt-test--context)
                   (list :on-payload (lambda (params _model)
                                       (let ((copy (copy-tree params)))
                                         (setf (alist-get 'stream copy) :false)
                                         copy)))
                   (superchat-bridge-amt-test--minimal-events))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
      (should (eq (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body) "stream")
                  t))
      ;; betas left the body and went into the header
      (should-not (assoc "betas" (superchat-bridge-amt-test--body)))
      (should (superchat-bridge-amt-test--header "anthropic-beta")))))

(ert-deftest superchat-bridge-amt-test-no-interleaved-beta-without-thinking ()
  "(upstream anthropic-sse-parsing) Thinking disabled: no interleaved beta."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--model
                   :id "anthropic/claude-haiku-4.5" :provider "openrouter"
                   :base-url "https://openrouter.ai/api" :reasoning t))
           (result (superchat-bridge-amt-test--stream
                    model (superchat-bridge-amt-test--context) '(:thinking-enabled :false)
                    (superchat-bridge-amt-test--minimal-events))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
      (should-not (string-match-p "interleaved"
                                  (or (superchat-bridge-amt-test--header "anthropic-beta")
                                      ""))))))

(ert-deftest superchat-bridge-amt-test-managed-beta-features ()
  "(upstream anthropic-sse-parsing) Mid-conversation effort betas are sent."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-fable-5-1")
                   (superchat-bridge-amt-test--context) nil
                   (superchat-bridge-amt-test--minimal-events)))
          (beta (superchat-bridge-amt-test--header "anthropic-beta")))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
      (should (string-match-p "mid-conversation-output-config-2026-07-01" beta))
      (should (string-match-p "thinking-binding-controls-2026-08-01" beta)))))

(ert-deftest superchat-bridge-amt-test-input-transformations-diagnostic ()
  "(upstream anthropic-sse-parsing) The final event's transformations are kept."
  (superchat-bridge-amt-test--with-rig
    (let* ((events (superchat-bridge-amt-test--minimal-events))
           (result nil))
      (setf (nth 0 events)
            (superchat-bridge-amt-test--e
             "message_start"
             '((type . "message_start")
               (message . ((id . "msg_transformations") (model . "claude-fable-5-1")
                           (usage . ((input_tokens . 12) (output_tokens . 0)))
                           (input_transformations
                            . [((type . "thinking_dropped") (path . "messages.1.content.0")
                                (reason . "prefix_binding_mismatch"))]))))))
      (setf (nth 4 events)
            (superchat-bridge-amt-test--e
             "message_delta"
             '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
               (usage . ((input_tokens . 12) (output_tokens . 5)))
               (input_transformations
                . [((type . "thinking_dropped") (path . "messages.3.content.0")
                    (reason . "model_binding_mismatch"))]))))
      (setq result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-fable-5-1")
                    (superchat-bridge-amt-test--context) nil events))
      (let ((diagnostics (superchat-bridge-assistant-message-diagnostics result)))
        (should (= (length diagnostics) 1))
        (should (equal (plist-get (car diagnostics) :type) "anthropic_input_transformations"))
        (should (integerp (plist-get (car diagnostics) :timestamp)))
        (should (equal (plist-get (car diagnostics) :details)
                       '(:transformations ((:type "thinking_dropped"
                                            :path "messages.3.content.0"
                                            :reason "model_binding_mismatch")))))))))

(ert-deftest superchat-bridge-amt-test-repairs-malformed-json ()
  "(upstream anthropic-sse-parsing) Malformed event and tool JSON are repaired."
  (superchat-bridge-amt-test--with-rig
    (let* ((context (superchat-bridge-normalize-context
                     (superchat-bridge-make-context
                      :messages (list (superchat-bridge-amt-test--user "Use the edit tool."))
                      :tools (list (superchat-bridge-make-tool
                                    :name "edit" :description "Edit a file."
                                    :parameters '((type . "object")
                                                  (properties . ((path . ((type . "string")))
                                                                 (text . ((type . "string")))))
                                                  (required . ["path" "text"])))))))
           (malformed (concat "{\"type\":\"content_block_delta\",\"index\":0,"
                              "\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":"
                              "\"{\\\"path\\\":\\\"A\\H\\\",\\\"text\\\":\\\"col1\tcol2\\\"}\"}}"))
           (result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-haiku-4-5") context nil
                    (list
                     (superchat-bridge-amt-test--e
                      "message_start"
                      '((type . "message_start")
                        (message . ((id . "msg_test")
                                    (usage . ((input_tokens . 12) (output_tokens . 0)))))))
                     (superchat-bridge-amt-test--e
                      "content_block_start"
                      '((type . "content_block_start") (index . 0)
                        (content_block . ((type . "tool_use") (id . "toolu_test")
                                          (name . "edit") (input . nil)))))
                     (cons "content_block_delta" malformed)
                     (superchat-bridge-amt-test--e
                      "content_block_stop" '((type . "content_block_stop") (index . 0)))
                     (superchat-bridge-amt-test--e
                      "message_delta"
                      '((type . "message_delta") (delta . ((stop_reason . "tool_use")))
                        (usage . ((input_tokens . 12) (output_tokens . 5)))))
                     (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop"))))))
           (call (seq-find #'superchat-bridge-tool-call-p
                           (superchat-bridge-message-content result))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "toolUse"))
      (should-not (superchat-bridge-assistant-message-error-message result))
      (should call)
      (should (equal (superchat-bridge-amt-test--norm (superchat-bridge-tool-call-arguments call))
                     '(("path" . "A\\H") ("text" . "col1\tcol2")))))))

(ert-deftest superchat-bridge-amt-test-block-start-content-kept ()
  "(upstream anthropic-sse-parsing) Text, thinking and signature of a start."
  (superchat-bridge-amt-test--with-rig
    (let* ((result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                    (superchat-bridge-amt-test--context) nil
                    (list
                     (superchat-bridge-amt-test--e
                      "message_start"
                      '((message . ((id . "msg_initial_content")
                                    (usage . ((input_tokens . 12) (output_tokens . 0)))))))
                     (superchat-bridge-amt-test--e
                      "content_block_start"
                      '((type . "content_block_start") (index . 0)
                        (content_block . ((type . "text") (text . "Initial text")))))
                     (superchat-bridge-amt-test--e
                      "content_block_delta"
                      '((type . "content_block_delta") (index . 0)
                        (delta . ((type . "text_delta") (text . " plus delta")))))
                     (superchat-bridge-amt-test--e
                      "content_block_stop" '((type . "content_block_stop") (index . 0)))
                     (superchat-bridge-amt-test--e
                      "content_block_start"
                      '((type . "content_block_start") (index . 1)
                        (content_block . ((type . "thinking") (thinking . "Initial thinking")
                                          (signature . "initial signature")))))
                     (superchat-bridge-amt-test--e
                      "content_block_delta"
                      '((type . "content_block_delta") (index . 1)
                        (delta . ((type . "thinking_delta") (thinking . " plus delta")))))
                     (superchat-bridge-amt-test--e
                      "content_block_delta"
                      '((type . "content_block_delta") (index . 1)
                        (delta . ((type . "signature_delta") (signature . " plus delta")))))
                     (superchat-bridge-amt-test--e
                      "content_block_stop" '((type . "content_block_stop") (index . 1)))
                     (superchat-bridge-amt-test--e
                      "message_delta"
                      '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
                        (usage . ((input_tokens . 12) (output_tokens . 5)))))
                     (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop"))))))
           (content (superchat-bridge-message-content result)))
      (should (= (length content) 2))
      (should (equal (superchat-bridge-text-content-text (nth 0 content))
                     "Initial text plus delta"))
      (should (equal (superchat-bridge-thinking-content-thinking (nth 1 content))
                     "Initial thinking plus delta"))
      (should (equal (superchat-bridge-thinking-content-thinking-signature (nth 1 content))
                     "initial signature plus delta"))
      (should-not (superchat-bridge-thinking-content-redacted (nth 1 content))))))

(defun superchat-bridge-amt-test--stop-events (delta)
  "Return message_start, a message_delta with DELTA, message_stop."
  (list
   (superchat-bridge-amt-test--e
    "message_start"
    '((message . ((id . "msg_x") (usage . ((input_tokens . 412) (output_tokens . 0)))))))
   (superchat-bridge-amt-test--e
    "message_delta"
    `((type . "message_delta") (delta . ,delta)
      (usage . ((input_tokens . 412) (output_tokens . 0)))))
   (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop")))))

(ert-deftest superchat-bridge-amt-test-refusal-stop-details ()
  "(upstream anthropic-sse-parsing) A refusal's explanation is the error."
  (superchat-bridge-amt-test--with-rig
    (let* ((explanation "This request triggered restrictions and was blocked.")
           (result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-fable-5")
                    (superchat-bridge-amt-test--context) nil
                    (superchat-bridge-amt-test--stop-events
                     `((stop_reason . "refusal")
                       (stop_details . ((type . "refusal") (category . "cyber")
                                        (explanation . ,explanation))))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
      (should (equal (superchat-bridge-assistant-message-raw-stop-reason result) "refusal"))
      (should (equal (superchat-bridge-assistant-message-error-message result) explanation)))))

(ert-deftest superchat-bridge-amt-test-sensitive-stop-reason ()
  "(upstream anthropic-sse-parsing) A sensitive stop has a descriptive message."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                   (superchat-bridge-amt-test--context) nil
                   (superchat-bridge-amt-test--stop-events '((stop_reason . "sensitive"))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
      (should (equal (superchat-bridge-assistant-message-raw-stop-reason result) "sensitive"))
      (should (equal (superchat-bridge-assistant-message-error-message result)
                     "Provider stopped with: sensitive")))))

(ert-deftest superchat-bridge-amt-test-unknown-stop-reason ()
  "An unknown stop reason ends the stream with an error, as `mapStopReason' throws."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                   (superchat-bridge-amt-test--context) nil
                   (superchat-bridge-amt-test--stop-events '((stop_reason . "new_thing"))))))
      (should (equal (superchat-bridge-assistant-message-error-message result)
                     "Unhandled stop reason: new_thing")))))

(ert-deftest superchat-bridge-amt-test-delta-without-usage ()
  "(upstream anthropic-sse-parsing) A message_delta without usage keeps usage."
  (superchat-bridge-amt-test--with-rig
    (let* ((events (superchat-bridge-amt-test--minimal-events))
           (result nil))
      (setf (nth 4 events)
            (superchat-bridge-amt-test--e
             "message_delta" '((type . "message_delta") (delta . ((stop_reason . "end_turn"))))))
      (setq result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                    (superchat-bridge-amt-test--context) nil events))
      (let ((usage (superchat-bridge-assistant-message-usage result)))
        (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
        (should-not (superchat-bridge-assistant-message-error-message result))
        (should (equal (superchat-bridge-text-content-text
                        (car (superchat-bridge-message-content result)))
                       "Hello"))
        (should (= (superchat-bridge-usage-input usage) 12))
        (should (= (superchat-bridge-usage-total-tokens usage) 12))))))

(ert-deftest superchat-bridge-amt-test-unknown-events-ignored ()
  "(upstream anthropic-sse-parsing) Unknown SSE events after message_stop."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                   (superchat-bridge-amt-test--context) nil
                   (append (superchat-bridge-amt-test--minimal-events)
                           (list (cons "done" "[DONE]")
                                 (cons "proxy.stats" "not json"))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
      (should-not (superchat-bridge-assistant-message-error-message result)))))

(ert-deftest superchat-bridge-amt-test-events-and-usage ()
  "The events of a text answer, and the usage of message_delta."
  (superchat-bridge-amt-test--with-rig
    (let* ((stream (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                    (superchat-bridge-amt-test--context) '(:api-key "k")))
           (req (superchat-bridge-amt-test--request)))
      (superchat-bridge-amt-test--respond
       req (superchat-bridge-amt-test--sse-ended (superchat-bridge-amt-test--minimal-events)))
      (let ((events (superchat-bridge-event-stream-to-list stream))
            (usage (superchat-bridge-assistant-message-usage
                    (superchat-bridge-event-stream-result stream))))
        (should (equal (superchat-bridge-amt-test--types events)
                       '("start" "text_start" "text_delta" "text_end" "done")))
        (should (= (superchat-bridge-usage-output usage) 5))
        (should (= (superchat-bridge-usage-total-tokens usage) 17))
        (should (equal (superchat-bridge-assistant-message-response-id
                        (superchat-bridge-event-stream-result stream))
                       "msg_test"))))))

(ert-deftest superchat-bridge-amt-test-stream-ends-before-message-stop ()
  "A body that ends after message_start but before message_stop is an error."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                   (superchat-bridge-amt-test--context) nil
                   (seq-take (superchat-bridge-amt-test--minimal-events) 3))))
      (should (equal (superchat-bridge-assistant-message-error-message result)
                     "Anthropic stream ended before message_stop")))))

(ert-deftest superchat-bridge-amt-test-sse-error-event ()
  "An `error' event ends the stream with its data as the message."
  (superchat-bridge-amt-test--with-rig
    (let* ((data "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\",\"message\":\"Overloaded\"}}")
           (result (superchat-bridge-amt-test--stream
                    (superchat-bridge-amt-test--catalog "claude-haiku-4-5")
                    (superchat-bridge-amt-test--context) nil
                    (list (car (superchat-bridge-amt-test--minimal-events))
                          (cons "error" data)))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
      (should (equal (superchat-bridge-assistant-message-error-message result) data)))))

;;; Thinking, temperature and effort payloads

(defun superchat-bridge-amt-test--custom (id compat)
  "Return upstream's `makeCustomModel' with ID and COMPAT."
  (superchat-bridge-amt-test--model
   :id id :provider "vendor-proxy" :reasoning t
   :context-window 200000 :max-tokens 32000 :compat compat))

(ert-deftest superchat-bridge-amt-test-thinking-disable-payloads ()
  "(upstream anthropic-thinking-disable) Off, adaptive and xhigh payloads."
  (dolist (case '(("claude-sonnet-4-5" nil (("type" . "disabled")) nil)
                  ("claude-opus-4-6" nil (("type" . "disabled")) nil)
                  ("claude-opus-4-8" nil (("type" . "disabled")) nil)
                  ("claude-fable-5" nil nil nil)
                  ("claude-opus-4-8" "high" (("display" . "summarized") ("type" . "adaptive"))
                   (("effort" . "high")))
                  ("claude-sonnet-5" "high" (("display" . "summarized") ("type" . "adaptive"))
                   (("effort" . "high")))
                  ("claude-opus-4-8" "xhigh" (("display" . "summarized") ("type" . "adaptive"))
                   (("effort" . "xhigh")))))
    (pcase-let* ((`(,id ,reasoning ,thinking ,output-config) case)
                 (payload (superchat-bridge-amt-test--simple-payload
                           (superchat-bridge-amt-test--catalog id)
                           (and reasoning (list :reasoning reasoning)))))
      (should (equal (cons id (superchat-bridge-amt-test--get payload "thinking"))
                     (cons id thinking)))
      (should (equal (cons id (superchat-bridge-amt-test--get payload "output_config"))
                     (cons id output-config))))))

(ert-deftest superchat-bridge-amt-test-temperature-compat ()
  "(upstream anthropic-temperature-compat) Temperature only where supported."
  (dolist (case `((,(superchat-bridge-amt-test--catalog "claude-opus-4-7") 0 nil)
                  (,(superchat-bridge-amt-test--catalog "claude-opus-4-8") 0 nil)
                  (,(superchat-bridge-amt-test--catalog "claude-opus-4-7") 1 nil)
                  (,(superchat-bridge-amt-test--catalog "claude-opus-4-6") 0 0)
                  (,(superchat-bridge-amt-test--catalog "claude-sonnet-4-6") 0 0)
                  (,(superchat-bridge-amt-test--custom "vendor--claude-opus-4-7"
                                                       '((supports-temperature)))
                   0 nil)))
    (let ((payload (superchat-bridge-amt-test--simple-payload
                    (nth 0 case) (list :temperature (nth 1 case)))))
      (should (equal (superchat-bridge-amt-test--get payload "temperature") (nth 2 case))))))

(ert-deftest superchat-bridge-amt-test-force-adaptive-thinking ()
  "(upstream anthropic-force-adaptive-thinking) The compat override."
  (let ((legacy (superchat-bridge-amt-test--simple-payload
                 (superchat-bridge-amt-test--custom "vendor--claude-opus-latest" nil)
                 '(:reasoning "medium")))
        (forced (superchat-bridge-amt-test--simple-payload
                 (superchat-bridge-amt-test--custom "vendor--claude-opus-latest"
                                                    '((force-adaptive-thinking . t)))
                 '(:reasoning "medium")))
        (fable (superchat-bridge-amt-test--simple-payload
                (superchat-bridge-amt-test--catalog "claude-fable-5") '(:reasoning "xhigh")))
        (opted-out (superchat-bridge-amt-test--simple-payload
                    (superchat-bridge-amt-test--with-compat
                     (superchat-bridge-amt-test--catalog "claude-opus-4-8")
                     '((force-adaptive-thinking)))
                    '(:reasoning "medium")))
        (off (superchat-bridge-amt-test--simple-payload
              (superchat-bridge-amt-test--custom "vendor--claude-opus-latest"
                                                 '((force-adaptive-thinking . t))))))
    (should (equal (superchat-bridge-amt-test--get legacy "thinking" "type") "enabled"))
    (should-not (superchat-bridge-amt-test--get legacy "output_config"))
    (should (equal (superchat-bridge-amt-test--get forced "thinking")
                   '(("display" . "summarized") ("type" . "adaptive"))))
    (should (equal (superchat-bridge-amt-test--get forced "output_config")
                   '(("effort" . "medium"))))
    (should (equal (superchat-bridge-amt-test--get fable "output_config")
                   '(("effort" . "xhigh"))))
    (should (equal (superchat-bridge-amt-test--get opted-out "thinking" "type") "enabled"))
    (should-not (superchat-bridge-amt-test--get opted-out "output_config"))
    (should (equal (superchat-bridge-amt-test--get off "thinking") '(("type" . "disabled"))))
    (should-not (superchat-bridge-amt-test--get off "output_config"))))

(ert-deftest superchat-bridge-amt-test-budget-thinking-and-interleaved-beta ()
  "Budget thinking: max_tokens grows by the budget; the interleaved beta."
  (let ((payload (superchat-bridge-amt-test--simple-payload
                  (superchat-bridge-amt-test--catalog "claude-sonnet-4-5")
                  '(:reasoning "medium" :max-tokens 1000))))
    ;; 1000 + 8192 (medium); the budget leaves 1024 for the answer
    (should (= (superchat-bridge-amt-test--get payload "max_tokens") 9192))
    (should (equal (superchat-bridge-amt-test--get payload "thinking")
                   '(("budget_tokens" . 8168) ("display" . "summarized")
                     ("type" . "enabled"))))
    (should (equal (superchat-bridge-amt-test--get payload "betas")
                   ["interleaved-thinking-2025-05-14"]))))

(ert-deftest superchat-bridge-amt-test-adaptive-thinking-models ()
  "(upstream anthropic-adaptive-thinking-models) The catalog's adaptive models."
  (let ((flagged (sort (mapcar #'car
                               (seq-filter (lambda (entry)
                                             (eq (alist-get 'force-adaptive-thinking
                                                            (superchat-bridge-model-compat
                                                             (cdr entry)))
                                                 t))
                                           (superchat-bridge-anthropic-models)))
                       #'string<)))
    (dolist (id '("claude-fable-5" "claude-haiku-5-5" "claude-opus-4-8" "claude-opus-5"
                  "claude-sonnet-5"))
      (should (member id flagged)))
    (dolist (id flagged)
      (should (string-match-p
               "opus[-.]\\(4[-.][678]\\|5\\)\\|sonnet[-.]4[-.]6\\|sonnet[-.]5\\|haiku[-.]5[-.]5\\|fable[-.]5"
               id)))))

(defun superchat-bridge-amt-test--managed (&optional provider)
  "Return upstream's `managedModel' of PROVIDER (default anthropic)."
  (superchat-bridge-amt-test--model
   :id "claude-fable-5-1" :provider (or provider "anthropic") :reasoning t
   :thinking-level-map '(("off" . :null) ("minimal" . "low") ("low" . "low")
                         ("medium" . "medium") ("high" . "high") ("max" . "max"))
   :context-window 200000 :max-tokens 32000
   :compat '((force-adaptive-thinking . t) (supports-mid-convo-effort . t))))

(defun superchat-bridge-amt-test--assistant (model &optional level)
  "Return upstream's `assistant' of MODEL with provider thinking LEVEL."
  (superchat-bridge-make-assistant-message
   :content (list (superchat-bridge-make-thinking-content
                   :thinking "reasoning" :thinking-signature "signature")
                  (superchat-bridge-make-text-content :text "answer"))
   :api "anthropic-messages" :provider (superchat-bridge-model-provider model)
   :model (superchat-bridge-model-id model) :provider-thinking-level level
   :usage (superchat-bridge-amt-test--usage) :stop-reason "stop" :timestamp 1))

(defun superchat-bridge-amt-test--effort (model messages &rest options)
  "Return (PAYLOAD . MESSAGE) of `stream' for MODEL, MESSAGES and OPTIONS.
Upstream's `capture': thinking enabled, cache retention none."
  (superchat-bridge-amt-test--with-rig
    (superchat-bridge-amt-test--capture
     model (apply #'superchat-bridge-amt-test--context messages)
     (append options (list :cache-retention "none" :thinking-enabled t)))))

(defun superchat-bridge-amt-test--markers (payload)
  "Return the system messages of PAYLOAD."
  (seq-filter (lambda (m) (equal (cdr (assoc "role" m)) "system"))
              (append (superchat-bridge-amt-test--get payload "messages") nil)))

(defun superchat-bridge-amt-test--marker (effort)
  "Return the normalized effort marker of EFFORT."
  `(("content" . []) ("output_config" . (("effort" . ,effort))) ("role" . "system")))

(ert-deftest superchat-bridge-amt-test-effort-marker-prefix ()
  "(upstream anthropic-mid-conversation-effort) Historical markers and drop_block."
  (let* ((model (superchat-bridge-amt-test--managed))
         (u1 (superchat-bridge-amt-test--user "one" 1))
         (first (superchat-bridge-amt-test--effort model (list u1) :effort "low"))
         (second (superchat-bridge-amt-test--effort
                  model (list u1 (superchat-bridge-amt-test--assistant model "low")
                              (superchat-bridge-amt-test--user "two" 2))
                  :effort "high"))
         (first-messages (superchat-bridge-amt-test--get (car first) "messages"))
         (second-messages (superchat-bridge-amt-test--get (car second) "messages")))
    (should (equal first-messages
                   (vector '(("content" . "one") ("role" . "user"))
                           (superchat-bridge-amt-test--marker "low"))))
    (should (equal (seq-take (append second-messages nil) 2) (append first-messages nil)))
    (should (equal (aref second-messages (1- (length second-messages)))
                   (superchat-bridge-amt-test--marker "high")))
    (should (equal (superchat-bridge-amt-test--get (car first) "output_config")
                   '(("effort" . "high"))))
    (should (equal (superchat-bridge-amt-test--get (car second) "thinking")
                   '(("block_binding" . (("prefix_mismatch_behavior" . "drop_block")))
                     ("display" . "summarized") ("type" . "adaptive"))))
    (should (equal (superchat-bridge-assistant-message-provider-thinking-level (cdr first))
                   "low"))))

(ert-deftest superchat-bridge-amt-test-effort-native-levels ()
  "(upstream anthropic-mid-conversation-effort) Native efforts are kept."
  (dolist (effort '("low" "medium" "high" "xhigh" "max"))
    (let ((result (superchat-bridge-amt-test--effort
                   (superchat-bridge-amt-test--managed)
                   (list (superchat-bridge-amt-test--user "one")) :effort effort)))
      (should (equal (superchat-bridge-amt-test--markers (car result))
                     (list (superchat-bridge-amt-test--marker effort))))
      (should (equal (superchat-bridge-assistant-message-provider-thinking-level (cdr result))
                     effort)))))

(ert-deftest superchat-bridge-amt-test-effort-defaults-to-high ()
  "(upstream anthropic-mid-conversation-effort) No effort is high."
  (let ((result (superchat-bridge-amt-test--effort
                 (superchat-bridge-amt-test--managed)
                 (list (superchat-bridge-amt-test--user "one")))))
    (should (equal (superchat-bridge-amt-test--markers (car result))
                   (list (superchat-bridge-amt-test--marker "high"))))
    (should (equal (superchat-bridge-amt-test--get (car result) "thinking" "block_binding"
                                                   "prefix_mismatch_behavior")
                   "drop_block"))
    (should (equal (superchat-bridge-assistant-message-provider-thinking-level (cdr result))
                   "high"))))

(ert-deftest superchat-bridge-amt-test-effort-no-invented-markers ()
  "(upstream anthropic-mid-conversation-effort) Legacy and other providers."
  (let* ((model (superchat-bridge-amt-test--managed))
         (other (superchat-bridge-amt-test--assistant model "low")))
    (setf (superchat-bridge-assistant-message-provider other) "other-provider")
    (let ((result (superchat-bridge-amt-test--effort
                   model (list (superchat-bridge-amt-test--user "one" 1)
                               (superchat-bridge-amt-test--assistant model)
                               (superchat-bridge-amt-test--user "two" 2)
                               other
                               (superchat-bridge-amt-test--user "three" 3))
                   :effort "medium")))
      (should (equal (superchat-bridge-amt-test--markers (car result))
                     (list (superchat-bridge-amt-test--marker "medium")))))))

(ert-deftest superchat-bridge-amt-test-effort-off-supported ()
  "(upstream anthropic-mid-conversation-effort) Off where the model has it."
  (let ((model (superchat-bridge-amt-test--managed)))
    (setf (superchat-bridge-model-thinking-level-map model)
          '(("minimal" . :null) ("low" . "low") ("medium" . "medium") ("high" . "high")
            ("max" . "max")))
    (let ((result (superchat-bridge-amt-test--effort
                   model (list (superchat-bridge-amt-test--user "one" 1)
                               (superchat-bridge-amt-test--assistant model "low")
                               (superchat-bridge-amt-test--user "two" 2)
                               (superchat-bridge-amt-test--assistant model "medium")
                               (superchat-bridge-amt-test--user "three" 3))
                   :thinking-enabled :false)))
      (should (equal (superchat-bridge-amt-test--get (car result) "thinking")
                     '(("type" . "disabled"))))
      (should-not (superchat-bridge-amt-test--get (car result) "output_config"))
      (should-not (superchat-bridge-amt-test--markers (car result)))
      (should-not (superchat-bridge-assistant-message-provider-thinking-level (cdr result))))))

(ert-deftest superchat-bridge-amt-test-effort-off-unsupported ()
  "(upstream anthropic-mid-conversation-effort) Off unsupported stays adaptive."
  (let ((result (superchat-bridge-amt-test--effort
                 (superchat-bridge-amt-test--managed)
                 (list (superchat-bridge-amt-test--user "one")) :thinking-enabled :false)))
    (should (equal (superchat-bridge-amt-test--get (car result) "thinking" "type") "adaptive"))
    (should (equal (superchat-bridge-amt-test--markers (car result))
                   (list (superchat-bridge-amt-test--marker "high"))))
    (should (equal (superchat-bridge-assistant-message-provider-thinking-level (cdr result))
                   "high"))))

(ert-deftest superchat-bridge-amt-test-effort-unsupported-model ()
  "(upstream anthropic-mid-conversation-effort) Top-level effort otherwise."
  (let ((model (superchat-bridge-amt-test--managed)))
    (setf (superchat-bridge-model-compat model) '((force-adaptive-thinking . t)))
    (let ((result (superchat-bridge-amt-test--effort
                   model (list (superchat-bridge-amt-test--user "one")) :effort "low")))
      (should (equal (superchat-bridge-amt-test--get (car result) "messages")
                     (vector '(("content" . "one") ("role" . "user")))))
      (should (equal (superchat-bridge-amt-test--get (car result) "output_config")
                     '(("effort" . "low"))))
      (should (equal (superchat-bridge-amt-test--get (car result) "thinking")
                     '(("display" . "summarized") ("type" . "adaptive"))))
      (should-not (superchat-bridge-assistant-message-provider-thinking-level
                   (cdr result))))))

(ert-deftest superchat-bridge-amt-test-effort-catalog-gates ()
  "(upstream anthropic-mid-conversation-effort) The catalog's effort gates."
  (let ((direct (superchat-bridge-amt-test--catalog "claude-fable-5-1")))
    (should (eq (alist-get 'supports-mid-convo-effort (superchat-bridge-model-compat direct)) t))
    (should (eq (cdr (assoc "off" (superchat-bridge-model-thinking-level-map direct))) :null))
    (should-not (assq 'supports-mid-convo-effort
                      (superchat-bridge-model-compat
                       (superchat-bridge-amt-test--catalog "claude-opus-4-8"))))
    (should-not (assq 'allowed-fallback-models
                      (superchat-bridge-model-compat
                       (superchat-bridge-amt-test--catalog "claude-opus-5"))))
    (dolist (id '("claude-opus-5" "claude-haiku-5-5"))
      (let ((model (superchat-bridge-amt-test--catalog id)))
        (should (eq (alist-get 'supports-mid-convo-effort (superchat-bridge-model-compat model))
                    t))
        (should-not (eq (cdr (assoc "off" (superchat-bridge-model-thinking-level-map model)))
                        :null))))
    (dolist (id '("claude-opus-5-5" "claude-sonnet-5-5" "claude-fable-5-1"))
      (should (eq (cdr (assoc "off" (superchat-bridge-model-thinking-level-map
                                     (superchat-bridge-amt-test--catalog id))))
                  :null)))))

;;; Messages (upstream anthropic-empty-thinking-signature-compat,
;;; anthropic-error-tool-result-images)

(defun superchat-bridge-amt-test--signature-context (signature &optional thinking)
  "Return upstream's `makeContext' with SIGNATURE and THINKING."
  (superchat-bridge-make-context
   :messages
   (list (superchat-bridge-amt-test--user "first")
         (superchat-bridge-make-assistant-message
          :content (list (superchat-bridge-make-thinking-content
                          :thinking (or thinking "internal reasoning")
                          :thinking-signature signature))
          :provider "xiaomi-token-plan-ams" :api "anthropic-messages"
          :model "mimo-v2.6-pro" :timestamp 1
          :usage (superchat-bridge-amt-test--usage) :stop-reason "stop")
         (superchat-bridge-amt-test--user "second"))))

(defun superchat-bridge-amt-test--mimo (&optional compat)
  "Return upstream's `makeModel' with COMPAT."
  (superchat-bridge-amt-test--model
   :id "mimo-v2.6-pro" :provider "xiaomi-token-plan-ams"
   :base-url "http://127.0.0.1:9/anthropic" :reasoning t
   :context-window 1048576 :max-tokens 1024 :compat compat))

(defun superchat-bridge-amt-test--assistant-content (payload)
  "Return the content of the assistant message of PAYLOAD."
  (cdr (assoc "content"
              (seq-find (lambda (m) (equal (cdr (assoc "role" m)) "assistant"))
                        (superchat-bridge-amt-test--get payload "messages")))))

(ert-deftest superchat-bridge-amt-test-empty-thinking-signature ()
  "(upstream anthropic-empty-thinking-signature-compat) Unsigned thinking."
  (should (equal (superchat-bridge-amt-test--assistant-content
                  (superchat-bridge-amt-test--simple-payload
                   (superchat-bridge-amt-test--mimo) nil
                   (superchat-bridge-amt-test--signature-context "")))
                 (vector '(("text" . "internal reasoning") ("type" . "text")))))
  (should (equal (superchat-bridge-amt-test--assistant-content
                  (superchat-bridge-amt-test--simple-payload
                   (superchat-bridge-amt-test--mimo) nil
                   (superchat-bridge-amt-test--signature-context "signed-thinking" "")))
                 (vector '(("signature" . "signed-thinking") ("thinking" . "")
                           ("type" . "thinking")))))
  (should (equal (superchat-bridge-amt-test--assistant-content
                  (superchat-bridge-amt-test--simple-payload
                   (superchat-bridge-amt-test--mimo '((allow-empty-signature . t))) nil
                   (superchat-bridge-amt-test--signature-context " ")))
                 (vector '(("signature" . "") ("thinking" . "internal reasoning")
                           ("type" . "thinking"))))))

(defun superchat-bridge-amt-test--image (data)
  "Return the normalized image block of DATA."
  `(("source" . (("data" . ,data) ("media_type" . "image/png") ("type" . "base64")))
    ("type" . "image")))

(defun superchat-bridge-amt-test--tool-turn (results)
  "Return the context of upstream's error tool result test with RESULTS."
  (superchat-bridge-make-context
   :messages
   (append
    (list (superchat-bridge-amt-test--user "Show the screenshots")
          (superchat-bridge-make-assistant-message
           :content (list (superchat-bridge-make-tool-call
                           :id "call-1" :name "codemode" :arguments (superchat-bridge-json-object))
                          (superchat-bridge-make-tool-call
                           :id "call-2" :name "read" :arguments (superchat-bridge-json-object)))
           :api "anthropic-messages" :provider "anthropic" :model "claude-test"
           :usage (superchat-bridge-amt-test--usage) :stop-reason "toolUse" :timestamp 1))
    results)))

(defun superchat-bridge-amt-test--png (data)
  "Return a PNG image content of DATA."
  (superchat-bridge-make-image-content :data data :mime-type "image/png"))

(ert-deftest superchat-bridge-amt-test-error-tool-result-images ()
  "(upstream anthropic-error-tool-result-images) Images after the results."
  (let* ((model (superchat-bridge-amt-test--model :input '("text" "image")))
         (payload (superchat-bridge-amt-test--simple-payload
                   model '(:cache-retention "none")
                   (superchat-bridge-amt-test--tool-turn
                    (list (superchat-bridge-make-tool-result-message
                           :tool-call-id "call-1" :tool-name "codemode"
                           :content (list (superchat-bridge-make-text-content :text "Script failed")
                                          (superchat-bridge-amt-test--png "AAAA")
                                          (superchat-bridge-amt-test--png "CCCC")
                                          (superchat-bridge-make-text-content :text "Script error"))
                           :is-error t :timestamp 2)
                          (superchat-bridge-make-tool-result-message
                           :tool-call-id "call-2" :tool-name "read"
                           :content (list (superchat-bridge-amt-test--png "BBBB"))
                           :is-error nil :timestamp 3))))))
    (should (equal (superchat-bridge-amt-test--get payload "messages" 2)
                   `(("content"
                      . [(("content"
                           . "Script failed\n(image attached after the tool results)\nScript error")
                          ("is_error" . t) ("tool_use_id" . "call-1") ("type" . "tool_result"))
                         (("content" . [(("text" . "(see attached image)") ("type" . "text"))
                                        ,(superchat-bridge-amt-test--image "BBBB")])
                          ("is_error" . :false) ("tool_use_id" . "call-2")
                          ("type" . "tool_result"))
                         (("text" . "Images from the error result of tool call call-1 (codemode):")
                          ("type" . "text"))
                         ,(superchat-bridge-amt-test--image "AAAA")
                         ,(superchat-bridge-amt-test--image "CCCC")])
                     ("role" . "user"))))))

(ert-deftest superchat-bridge-amt-test-error-tool-result-without-images ()
  "(upstream anthropic-error-tool-result-images) No images: unchanged."
  (let* ((model (superchat-bridge-amt-test--model :input '("text" "image")))
         (context (superchat-bridge-amt-test--tool-turn
                   (list (superchat-bridge-make-tool-result-message
                          :tool-call-id "call-1" :tool-name "codemode"
                          :content (list (superchat-bridge-make-text-content :text "Script failed"))
                          :is-error t :timestamp 2)))))
    ;; upstream's assistant has only the first call here
    (setf (superchat-bridge-message-content (nth 1 (superchat-bridge-context-messages context)))
          (list (car (superchat-bridge-message-content
                      (nth 1 (superchat-bridge-context-messages context))))))
    (should (equal (superchat-bridge-amt-test--get
                    (superchat-bridge-amt-test--simple-payload
                     model '(:cache-retention "none") context)
                    "messages" 2)
                   '(("content" . [(("content" . "Script failed") ("is_error" . t)
                                    ("tool_use_id" . "call-1") ("type" . "tool_result"))])
                     ("role" . "user"))))))

;;; Cache control

(ert-deftest superchat-bridge-amt-test-cache-control-short-and-long ()
  "System, last tool and last user block carry cache_control; long is 1h."
  (let* ((context (superchat-bridge-make-context
                   :system-prompt "Be brief."
                   :tools (list (superchat-bridge-make-tool
                                 :name "a" :parameters '((type . "object")))
                                (superchat-bridge-make-tool
                                 :name "b" :parameters '((type . "object"))))
                   :messages (list (superchat-bridge-amt-test--user "hi"))))
         (short (superchat-bridge-amt-test--simple-payload
                 (superchat-bridge-amt-test--model) nil context))
         (long (superchat-bridge-amt-test--simple-payload
                (superchat-bridge-amt-test--model) '(:cache-retention "long") context))
         (none (superchat-bridge-amt-test--simple-payload
                (superchat-bridge-amt-test--model) '(:cache-retention "none") context))
         (ephemeral '(("type" . "ephemeral"))))
    (should (equal (superchat-bridge-amt-test--get short "system")
                   `[(("cache_control" . ,ephemeral) ("text" . "Be brief.") ("type" . "text"))]))
    (should-not (assoc "cache_control" (superchat-bridge-amt-test--get short "tools" 0)))
    (should (equal (superchat-bridge-amt-test--get short "tools" 1 "cache_control") ephemeral))
    (should (equal (superchat-bridge-amt-test--get short "tools" 1 "eager_input_streaming") t))
    (should (equal (superchat-bridge-amt-test--get short "tools" 1 "input_schema")
                   '(("properties") ("required" . []) ("type" . "object"))))
    (should (equal (superchat-bridge-amt-test--get short "messages" 0)
                   `(("content" . [(("cache_control" . ,ephemeral) ("text" . "hi")
                                    ("type" . "text"))])
                     ("role" . "user"))))
    (should (equal (superchat-bridge-amt-test--get long "system" 0 "cache_control")
                   '(("ttl" . "1h") ("type" . "ephemeral"))))
    (should-not (assoc "cache_control" (superchat-bridge-amt-test--get none "system" 0)))
    (should (equal (superchat-bridge-amt-test--get none "messages" 0 "content") "hi"))))

(ert-deftest superchat-bridge-amt-test-cache-retention-from-env ()
  "PI_CACHE_RETENTION=long in the options' env is the long retention."
  (let ((payload (superchat-bridge-amt-test--simple-payload
                  (superchat-bridge-amt-test--model)
                  '(:env (("PI_CACHE_RETENTION" . "long"))))))
    (should (equal (superchat-bridge-amt-test--get payload "messages" 0 "content" 0
                                                   "cache_control" "ttl")
                   "1h"))))

;;; 1h cache write cost (upstream anthropic-cache-write-1h-cost)

(defun superchat-bridge-amt-test--cache-events (cache-creation)
  "Return upstream's `eventsWithCacheCreation' of CACHE-CREATION."
  (let ((usage `((input_tokens . 100) (output_tokens . 0) (cache_read_input_tokens . 0)
                 (cache_creation_input_tokens . 1000000)
                 ,@(and cache-creation (list (cons 'cache_creation cache-creation))))))
    (list
     (superchat-bridge-amt-test--e
      "message_start" `((type . "message_start") (message . ((id . "msg_test") (usage . ,usage)))))
     (superchat-bridge-amt-test--e
      "content_block_start"
      '((type . "content_block_start") (index . 0) (content_block . ((type . "text") (text . "")))))
     (superchat-bridge-amt-test--e
      "content_block_delta"
      '((type . "content_block_delta") (index . 0) (delta . ((type . "text_delta") (text . "Hi")))))
     (superchat-bridge-amt-test--e
      "content_block_stop" '((type . "content_block_stop") (index . 0)))
     (superchat-bridge-amt-test--e
      "message_delta"
      '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
        (usage . ((input_tokens . 100) (output_tokens . 5) (cache_read_input_tokens . 0)
                  (cache_creation_input_tokens . 1000000)))))
     (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop"))))))

(ert-deftest superchat-bridge-amt-test-cache-write-1h-cost ()
  "(upstream anthropic-cache-write-1h-cost) 1h writes cost twice the input."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--catalog "claude-opus-4-8"))
           (usage (superchat-bridge-assistant-message-usage
                   (superchat-bridge-amt-test--stream
                    model (superchat-bridge-amt-test--context) nil
                    (superchat-bridge-amt-test--cache-events
                     '((ephemeral_5m_input_tokens . 600000)
                       (ephemeral_1h_input_tokens . 400000)))))))
      (should (= (superchat-bridge-usage-cache-write usage) 1000000))
      (should (= (superchat-bridge-usage-cache-write1h usage) 400000))
      ;; 600k * 6.25/Mtok + 400k * 10/Mtok = 7.75
      (should (< (abs (- (superchat-bridge-cost-cache-write (superchat-bridge-usage-cost usage))
                         7.75))
                 1e-10)))))

(ert-deftest superchat-bridge-amt-test-cache-write-1h-in-delta ()
  "(upstream anthropic-cache-write-1h-cost) 1h writes reported in message_delta."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--model
                   :id "anthropic/claude-haiku-4.5" :provider "vercel-ai-gateway"
                   :cost (superchat-bridge-make-model-cost :input 1 :output 5
                                                           :cache-read 0.1 :cache-write 1.25)))
           (usage (superchat-bridge-assistant-message-usage
                   (superchat-bridge-amt-test--stream
                    model (superchat-bridge-amt-test--context) nil
                    (list
                     (superchat-bridge-amt-test--e
                      "message_start"
                      '((type . "message_start")
                        (message . ((id . "msg_test")
                                    (usage . ((input_tokens . 0) (output_tokens . 0)))))))
                     (superchat-bridge-amt-test--e
                      "message_delta"
                      '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
                        (usage . ((input_tokens . 3) (output_tokens . 4)
                                  (cache_creation_input_tokens . 6535)
                                  (cache_creation . ((ephemeral_5m_input_tokens . 0)
                                                     (ephemeral_1h_input_tokens . 6535)))))))
                     (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop"))))))))
      (should (= (superchat-bridge-usage-cache-write usage) 6535))
      (should (= (superchat-bridge-usage-cache-write1h usage) 6535))
      (should (< (abs (- (superchat-bridge-cost-cache-write (superchat-bridge-usage-cost usage))
                         (/ (* 6535 1 2) 1000000.0)))
                 1e-10)))))

(ert-deftest superchat-bridge-amt-test-cache-write-without-breakdown ()
  "(upstream anthropic-cache-write-1h-cost) No breakdown: the 5m rate."
  (superchat-bridge-amt-test--with-rig
    (let ((usage (superchat-bridge-assistant-message-usage
                  (superchat-bridge-amt-test--stream
                   (superchat-bridge-amt-test--catalog "claude-opus-4-8")
                   (superchat-bridge-amt-test--context) nil
                   (superchat-bridge-amt-test--cache-events nil)))))
      (should (= (superchat-bridge-usage-cache-write usage) 1000000))
      (should (= (or (superchat-bridge-usage-cache-write1h usage) 0) 0))
      (should (< (abs (- (superchat-bridge-cost-cache-write (superchat-bridge-usage-cost usage))
                         6.25))
                 1e-10)))))

;;; Auth and headers (upstream anthropic-auth-token, eager-tool-input-compat)

(ert-deftest superchat-bridge-amt-test-request-url-and-headers ()
  "POST to /v1/messages?beta=true with the SDK's headers and X-Api-Key."
  (superchat-bridge-amt-test--with-rig
    (superchat-bridge-api-anthropic-messages-stream
     (superchat-bridge-amt-test--model :base-url "https://api.anthropic.com/")
     (superchat-bridge-amt-test--context) '(:api-key "anthropic-key"))
    (let ((config (superchat-bridge-amt-test--req-config (superchat-bridge-amt-test--request))))
      (should (string-match-p "url = \"https://api.anthropic.com/v1/messages\\?beta=true\""
                              config))
      (should (string-match-p "request = \"POST\"" config))
      (should (equal (superchat-bridge-amt-test--header "X-Api-Key") "anthropic-key"))
      (should (equal (superchat-bridge-amt-test--header "anthropic-version") "2023-06-01"))
      (should (equal (superchat-bridge-amt-test--header "Content-Type") "application/json"))
      (should (equal (superchat-bridge-amt-test--header "accept") "application/json"))
      (should-not (superchat-bridge-amt-test--header "Authorization"))
      (should-not (superchat-bridge-amt-test--header "anthropic-beta")))))

(ert-deftest superchat-bridge-amt-test-authorization-header-without-key ()
  "(upstream anthropic-auth-token) A header's auth: no key, no OAuth shaping."
  (superchat-bridge-amt-test--with-rig
    (superchat-bridge-api-anthropic-messages-stream
     (superchat-bridge-amt-test--model)
     (superchat-bridge-normalize-context
      (superchat-bridge-make-context
       :system-prompt "System prompt."
       :messages (list (superchat-bridge-amt-test--user "Hello"))))
     '(:headers (("Authorization" . "Bearer gateway-token"))))
    (should (equal (superchat-bridge-amt-test--header "Authorization") "Bearer gateway-token"))
    (should-not (superchat-bridge-amt-test--header "X-Api-Key"))
    (should-not (superchat-bridge-amt-test--header "anthropic-beta"))
    (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body)
                                                   "system" 0 "text")
                   "System prompt."))))

(ert-deftest superchat-bridge-amt-test-beta-header-replacement-and-suppression ()
  "(upstream anthropic-auth-token) An anthropic-beta header replaces or removes."
  (let ((replaced (superchat-bridge-amt-test--simple-payload
                   (superchat-bridge-amt-test--model :reasoning t)
                   '(:reasoning "low" :headers (("anthropic-beta" . "custom-beta")))))
        (suppressed (superchat-bridge-amt-test--simple-payload
                     (superchat-bridge-amt-test--model :reasoning t)
                     '(:reasoning "low" :headers (("anthropic-beta" . nil))))))
    (should (equal (superchat-bridge-amt-test--get replaced "betas") ["custom-beta"]))
    (should-not (assoc "betas" suppressed))))

(ert-deftest superchat-bridge-amt-test-no-api-key ()
  "`stream' ends with an error event; `stream-simple' signals, as it throws."
  (superchat-bridge-amt-test--with-rig
    (let* ((model (superchat-bridge-amt-test--model :provider "acme"))
           (message (superchat-bridge-event-stream-result
                     (superchat-bridge-api-anthropic-messages-stream
                      model (superchat-bridge-amt-test--context) nil))))
      (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     "No API key for provider: acme"))
      (should-not superchat-bridge-amt-test--requests)
      (should-error (superchat-bridge-api-anthropic-messages-stream-simple
                     model (superchat-bridge-amt-test--context) nil)))))

(ert-deftest superchat-bridge-amt-test-oauth-and-copilot-left-out ()
  "An OAuth token and a Copilot model end with an error: not ported."
  (superchat-bridge-amt-test--with-rig
    (should (string-match-p
             "OAuth tokens are not supported"
             (superchat-bridge-assistant-message-error-message
              (superchat-bridge-event-stream-result
               (superchat-bridge-api-anthropic-messages-stream
                (superchat-bridge-amt-test--model) (superchat-bridge-amt-test--context)
                '(:api-key "sk-ant-oat-test"))))))
    (should (string-match-p
             "GitHub Copilot is not supported"
             (superchat-bridge-assistant-message-error-message
              (superchat-bridge-event-stream-result
               (superchat-bridge-api-anthropic-messages-stream
                (superchat-bridge-amt-test--model :provider "github-copilot")
                (superchat-bridge-amt-test--context) '(:api-key "k"))))))
    (should-not superchat-bridge-amt-test--requests)))

(ert-deftest superchat-bridge-amt-test-session-affinity-openrouter ()
  "OpenRouter gets x-session-id; none when cache retention is none."
  (superchat-bridge-amt-test--with-rig
    (let ((model (superchat-bridge-amt-test--model :provider "openrouter")))
      (superchat-bridge-api-anthropic-messages-stream
       model (superchat-bridge-amt-test--context) '(:api-key "k" :session-id "s1"))
      (should (equal (superchat-bridge-amt-test--header "x-session-id" 0) "s1"))
      (superchat-bridge-api-anthropic-messages-stream
       model (superchat-bridge-amt-test--context)
       '(:api-key "k" :session-id "s1" :cache-retention "none"))
      (should-not (superchat-bridge-amt-test--header "x-session-id" 1)))))

(defun superchat-bridge-amt-test--lookup-tool ()
  "Return upstream's `lookup' tool."
  (superchat-bridge-make-tool
   :name "lookup" :description "Look up a value"
   :parameters '((type . "object") (properties . ((value . ((type . "string")))))
                 (required . ["value"]))))

(ert-deftest superchat-bridge-amt-test-eager-tool-input-compat ()
  "(upstream anthropic-eager-tool-input-compat) Eager input or the old beta."
  (superchat-bridge-amt-test--with-rig
    (let ((with-tool (superchat-bridge-normalize-context
                      (superchat-bridge-make-context
                       :messages (list (superchat-bridge-amt-test--user "Use the tool"))
                       :tools (list (superchat-bridge-amt-test--lookup-tool)))))
          (base (superchat-bridge-amt-test--custom "claude-opus-4-8"
                                                   '((force-adaptive-thinking . t)))))
      (superchat-bridge-api-anthropic-messages-stream
       base with-tool '(:api-key "test-key" :cache-retention "none"))
      (should (eq (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 0)
                                                  "tools" 0 "eager_input_streaming")
                  t))
      (should-not (superchat-bridge-amt-test--header "anthropic-beta" 0))
      (superchat-bridge-api-anthropic-messages-stream
       (superchat-bridge-amt-test--with-compat
        base '((force-adaptive-thinking . t) (supports-eager-tool-input-streaming)))
       with-tool '(:api-key "test-key" :cache-retention "none"))
      (should-not (assoc "eager_input_streaming"
                         (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1)
                                                         "tools" 0)))
      (should (equal (superchat-bridge-amt-test--header "anthropic-beta" 1)
                     "fine-grained-tool-streaming-2025-05-14"))
      (superchat-bridge-api-anthropic-messages-stream
       (superchat-bridge-amt-test--with-compat
        base '((force-adaptive-thinking . t) (supports-eager-tool-input-streaming)))
       (superchat-bridge-amt-test--context (superchat-bridge-amt-test--user "Use the tool"))
       '(:api-key "test-key" :cache-retention "none"))
      (should-not (assoc "tools" (superchat-bridge-amt-test--body 2)))
      (should-not (superchat-bridge-amt-test--header "anthropic-beta" 2)))))

;;; Strict tool schemas (upstream anthropic-strict-tool-schema)

(defun superchat-bridge-amt-test--first-tool (parameters &optional strict)
  "Return the first tool of the payload for a tool of PARAMETERS.
STRICT, when given, is its `constrained-sampling' strictness."
  (let ((model (superchat-bridge-amt-test--model
                :id "claude-opus-4-8" :provider "test-anthropic" :reasoning t
                :max-tokens 32000
                :compat '((force-adaptive-thinking . t) (supports-strict-tools . t))))
        (tool (superchat-bridge-make-tool
               :name "lookup" :description "Look up a value" :parameters parameters
               :constrained-sampling (and strict (list :type "json_schema" :strict strict)))))
    (superchat-bridge-amt-test--with-rig
      (superchat-bridge-amt-test--get
       (car (superchat-bridge-amt-test--capture
             model (superchat-bridge-normalize-context
                    (superchat-bridge-make-context
                     :messages (list (superchat-bridge-amt-test--user "Use the tool"))
                     :tools (list tool)))
             '(:api-key "test-key" :cache-retention "none")))
       "tools" 0))))

(ert-deftest superchat-bridge-amt-test-strict-schema-only-for-strict-tools ()
  "(upstream anthropic-strict-tool-schema) The full schema only when strict."
  (let ((legacy (superchat-bridge-amt-test--first-tool
                 '((type . "object") (properties . ((value . ((type . "string")))))
                   (required . ["value"]) (additionalProperties . :false)
                   (title . "LookupInput"))))
        (strict (superchat-bridge-amt-test--first-tool
                 '((type . "object") (title . "StrictLookupInput")
                   (properties . ((value . ((type . "string")))
                                  (optional . ((type . "number")))))
                   (required . ["value"]))
                 "prefer")))
    (should-not (assoc "strict" legacy))
    (should (equal (cdr (assoc "input_schema" legacy))
                   '(("properties" . (("value" . (("type" . "string")))))
                     ("required" . ["value"]) ("type" . "object"))))
    (should (eq (cdr (assoc "strict" strict)) t))
    (should (equal (cdr (assoc "input_schema" strict))
                   '(("additionalProperties" . :false)
                     ("properties"
                      . (("optional" . (("anyOf" . [(("type" . "number")) (("type" . "null"))])))
                         ("value" . (("type" . "string")))))
                     ("required" . ["value" "optional"])
                     ("title" . "StrictLookupInput")
                     ("type" . "object"))))))

(ert-deftest superchat-bridge-amt-test-strict-unsupported-keywords ()
  "(upstream anthropic-strict-tool-schema) Rejected keywords fall back (#9953)."
  (dolist (parameters
           '(((type . "object")
              (properties . ((timeoutMs . ((type . "integer") (minimum . 1)
                                           (maximum . 300000))))))
             ((type . "object")
              (properties . ((options . ((type . "object")
                                         (properties . ((tags . ((type . "array")
                                                                 (items . ((type . "string")))
                                                                 (minItems . 2)))))))))
              (required . ["options"]))
             ((type . "object")
              (properties . ((expression . ((type . "string") (format . "regex")))))
              (required . ["expression"]))))
    (should-not (assoc "strict" (superchat-bridge-amt-test--first-tool parameters "prefer"))))
  (should (eq (cdr (assoc "strict"
                          (superchat-bridge-amt-test--first-tool
                           '((type . "object")
                             (properties . ((code . ((type . "string") (minLength . 1)
                                                     (maxLength . 1000)
                                                     (pattern . "^[a-z]+$")))
                                            (url . ((type . "string") (format . "uri")))
                                            (tags . ((type . "array")
                                                     (items . ((type . "string")))
                                                     (minItems . 1)))))
                             (required . ["code" "url" "tags"]))
                           "prefer")))
              t)))

(ert-deftest superchat-bridge-amt-test-strict-require-unsupported ()
  "A `require' tool that cannot be strict ends the stream with an error."
  (superchat-bridge-amt-test--with-rig
    (let ((message (superchat-bridge-event-stream-result
                    (superchat-bridge-api-anthropic-messages-stream
                     (superchat-bridge-amt-test--model)
                     (superchat-bridge-normalize-context
                      (superchat-bridge-make-context
                       :messages (list (superchat-bridge-amt-test--user "x"))
                       :tools (list (superchat-bridge-make-tool
                                     :name "lookup" :parameters '((type . "object"))
                                     :constrained-sampling '(:type "json_schema"
                                                             :strict "require")))))
                     '(:api-key "k")))))
      (should (equal (superchat-bridge-assistant-message-error-message message)
                     (concat "Tool \"lookup\" requires JSON-schema constrained sampling, "
                             "but strict tools are unsupported."))))))

;;; Strict tool fallback (upstream anthropic-strict-tool-fallback)

(defconst superchat-bridge-amt-test--grammar-error
  (concat "The compiled grammar is too large, which would cause performance issues. "
          "Simplify your tool schemas or reduce the number of strict tools."))

(defun superchat-bridge-amt-test--fallback-model (&optional compat)
  "Return the fallback test's model, COMPAT added to its own."
  (superchat-bridge-amt-test--model
   :id "claude-haiku-4-5" :name "Test Claude" :base-url "https://anthropic.example"
   :max-tokens 100 :compat (append compat '((supports-strict-tools . t)))))

(defun superchat-bridge-amt-test--fallback-tool (name &optional strict)
  "Return the fallback test's tool NAME with STRICT sampling."
  (superchat-bridge-make-tool
   :name name :description "Look up a value"
   :parameters '((type . "object")
                 (properties . ((value . ((type . "string"))) (optional . ((type . "number")))))
                 (required . ["value"]))
   :constrained-sampling (and strict (list :type "json_schema" :strict strict))))

(defun superchat-bridge-amt-test--tools-context (tools)
  "Return a normalized context with TOOLS and one user message."
  (superchat-bridge-normalize-context
   (superchat-bridge-make-context :tools tools
                                  :messages (list (superchat-bridge-amt-test--user "Hello")))))

(defun superchat-bridge-amt-test--error-response (req &optional message status type)
  "Answer REQ with an Anthropic error body of MESSAGE, STATUS and TYPE."
  (let ((status (or status 400)))
    (superchat-bridge-amt-test--respond
     req (superchat-bridge-am--serialize
          `((type . "error")
            (error . ((type . ,(or type "invalid_request_error"))
                      (message . ,(or message superchat-bridge-amt-test--grammar-error))))))
     (format "HTTP/1.1 %d Error\r\nContent-Type: application/json\r\n\r\n" status))))

(defun superchat-bridge-amt-test--success-response (req)
  "Answer REQ with upstream's `successResponse'."
  (superchat-bridge-amt-test--respond
   req (superchat-bridge-amt-test--sse-ended
        (list (superchat-bridge-amt-test--e
               "message_start"
               '((type . "message_start")
                 (message . ((id . "msg_test") (model . "claude-haiku-4-5")
                             (usage . ((input_tokens . 10) (output_tokens . 0)))))))
              (superchat-bridge-amt-test--e
               "message_delta"
               '((type . "message_delta") (delta . ((stop_reason . "end_turn")))
                 (usage . ((output_tokens . 1)))))
              (superchat-bridge-amt-test--e "message_stop" '((type . "message_stop")))))))

(defun superchat-bridge-amt-test--answer (respond)
  "Answer each request the rig has not answered with (RESPOND REQ ATTEMPT)."
  (let ((answered 0))
    (while (< answered (length superchat-bridge-amt-test--requests))
      (setq answered (1+ answered))
      (funcall respond (superchat-bridge-amt-test--request (1- answered)) answered))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-retries-non-strict ()
  "(upstream anthropic-strict-tool-fallback) Each limit message retries once."
  (dolist (message (list superchat-bridge-amt-test--grammar-error
                         "Too many strict tools (32). The maximum number of strict tools supported is 20."
                         "Schemas contains too many parameters with union types (480 parameters)."))
    (superchat-bridge-amt-test--with-rig
      (let* ((payloads 0) (responses 0)
             (transcript (superchat-bridge-amt-test--tools-context
                          (list (superchat-bridge-amt-test--fallback-tool "preferred" "prefer")
                                (superchat-bridge-amt-test--fallback-tool "required" "require")
                                (superchat-bridge-amt-test--fallback-tool "ordinary"))))
             (stream (superchat-bridge-api-anthropic-messages-stream
                      (superchat-bridge-amt-test--fallback-model) transcript
                      (list :api-key "test-key" :cache-retention "long"
                            :on-payload (lambda (params _model)
                                          (setq payloads (1+ payloads))
                                          (let ((copy (copy-tree params)))
                                            (setf (alist-get 'stream copy) :false)
                                            (append copy (list (cons 'inference_geo "us")))))
                            :on-response (lambda (_response _model)
                                           (setq responses (1+ responses)))))))
        (superchat-bridge-amt-test--answer
         (lambda (req attempt)
           (if (= attempt 1)
               (superchat-bridge-amt-test--error-response req message)
             (superchat-bridge-amt-test--success-response req))))
        (let* ((result (superchat-bridge-event-stream-result stream))
               (first (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 0) "tools"))
               (second (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1) "tools")))
          (should (equal (superchat-bridge-assistant-message-stop-reason result) "stop"))
          (should (equal (superchat-bridge-amt-test--types
                          (superchat-bridge-event-stream-to-list stream))
                         '("start" "done")))
          (should (= (length superchat-bridge-amt-test--requests) 2))
          (should (= payloads 2))
          (should (= responses 1))
          (should (eq (superchat-bridge-amt-test--get first 0 "strict") t))
          (should (equal (superchat-bridge-amt-test--get first 0 "input_schema" "required")
                         ["value" "optional"]))
          (should-not (assoc "strict" (aref second 0)))
          (should (equal (superchat-bridge-amt-test--get second 0 "input_schema")
                         '(("properties" . (("optional" . (("type" . "number")))
                                            ("value" . (("type" . "string")))))
                           ("required" . ["value"]) ("type" . "object"))))
          (should (equal (aref second 1) (aref first 1)))
          (should (eq (superchat-bridge-amt-test--get second 1 "strict") t))
          (should (equal (aref second 2) (aref first 2)))
          (should (equal (superchat-bridge-amt-test--get second 2 "cache_control")
                         '(("ttl" . "1h") ("type" . "ephemeral"))))
          (should (eq (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 0) "stream") t))
          (should (eq (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1) "stream") t))
          (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1)
                                                         "inference_geo")
                         "us"))
          ;; the transcript's tool is unchanged
          (should (equal (superchat-bridge-tool-parameters
                          (car (superchat-bridge-system-message-tools-added
                                (car (superchat-bridge-context-messages transcript)))))
                         '((type . "object")
                           (properties . ((value . ((type . "string")))
                                          (optional . ((type . "number")))))
                           (required . ["value"]))))
          (let ((diagnostic (car (superchat-bridge-assistant-message-diagnostics result))))
            (should (equal (plist-get diagnostic :type) "anthropic_strict_tools_fallback"))
            (should (equal (plist-get diagnostic :details)
                           '(:reason "strict_tool_limits_exceeded")))))))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-inline-additions ()
  "(upstream anthropic-strict-tool-fallback) Inline additions downgrade too.
Upstream uses an OAuth key here; OAuth is not ported, so a plain key."
  (superchat-bridge-amt-test--with-rig
    (let* ((transcript (superchat-bridge-amt-test--tools-context
                        (list (superchat-bridge-amt-test--fallback-tool "lookup" "require"))))
           (messages (superchat-bridge-context-messages transcript)))
      (setf (superchat-bridge-context-messages transcript)
            (append messages
                    (list (superchat-bridge-make-system-message
                           :content "" :timestamp 2
                           :tools-added (list (superchat-bridge-amt-test--fallback-tool
                                               "lookup" "prefer")))
                          (superchat-bridge-make-system-message
                           :content "" :timestamp 3
                           :tools-added (list (superchat-bridge-amt-test--fallback-tool
                                               "lookup" "require"))))))
      (let ((stream (superchat-bridge-api-anthropic-messages-stream
                     (superchat-bridge-amt-test--fallback-model
                      '((supports-mid-convo-system-messages . t)
                        (supports-mid-convo-tool-changes . t)))
                     transcript '(:api-key "test-key"))))
        (superchat-bridge-amt-test--answer
         (lambda (req attempt)
           (if (= attempt 1)
               (superchat-bridge-amt-test--error-response req)
             (superchat-bridge-amt-test--success-response req))))
        (should (equal (superchat-bridge-assistant-message-stop-reason
                        (superchat-bridge-event-stream-result stream))
                       "stop"))
        (should (= (length superchat-bridge-amt-test--requests) 2))
        (should (equal (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 0) "tools")
                       (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1) "tools")))
        (should (equal (superchat-bridge-amt-test--header "anthropic-beta" 0)
                       "inline-tools-2026-09-15"))
        (let ((additions
               (mapcar
                (lambda (index)
                  (let (found)
                    (seq-doseq (message (superchat-bridge-amt-test--get
                                         (superchat-bridge-amt-test--body index) "messages"))
                      (let ((content (cdr (assoc "content" message))))
                        (when (vectorp content)
                          (seq-doseq (block content)
                            (when (equal (cdr (assoc "type" block)) "tool_addition")
                              (push (superchat-bridge-amt-test--get block "tool" "definition")
                                    found))))))
                    (nreverse found)))
                '(0 1))))
          (should (equal (mapcar (lambda (d) (cdr (assoc "strict" d))) (nth 0 additions))
                         '(t t)))
          (should (equal (mapcar (lambda (d) (cdr (assoc "strict" d))) (nth 1 additions))
                         '(nil t)))
          (should (equal (superchat-bridge-amt-test--get (car (nth 1 additions))
                                                         "input_schema" "required")
                         ["value"]))
          (should (equal (nth 1 (nth 1 additions)) (nth 1 (nth 0 additions)))))))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-not-for-other-tool-sets ()
  "(upstream anthropic-strict-tool-fallback) No retry without preferred tools."
  (dolist (tools (list (list (superchat-bridge-amt-test--fallback-tool "lookup" "require"))
                       (list (superchat-bridge-amt-test--fallback-tool "lookup"))
                       (list (superchat-bridge-make-tool
                              :name "lookup"
                              :parameters '((type . "object")
                                            (properties . ((value . ((type . "number")
                                                                     (minimum . 1)))))
                                            (required . ["value"]))
                              :constrained-sampling '(:type "json_schema" :strict "prefer")))))
    (superchat-bridge-amt-test--with-rig
      (let ((stream (superchat-bridge-api-anthropic-messages-stream
                     (superchat-bridge-amt-test--fallback-model)
                     (superchat-bridge-amt-test--tools-context tools) '(:api-key "test-key"))))
        (superchat-bridge-amt-test--answer
         (lambda (req _attempt) (superchat-bridge-amt-test--error-response req)))
        (let ((result (superchat-bridge-event-stream-result stream)))
          (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
          (should (string-search superchat-bridge-amt-test--grammar-error
                                 (superchat-bridge-assistant-message-error-message result)))
          (should (= (length superchat-bridge-amt-test--requests) 1)))))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-not-for-other-errors ()
  "(upstream anthropic-strict-tool-fallback) Other errors are not downgraded."
  (dolist (case `(("Invalid tool schema" 400 "invalid_request_error")
                  (,superchat-bridge-amt-test--grammar-error 400 "authentication_error")
                  (,superchat-bridge-amt-test--grammar-error 500 "invalid_request_error")))
    (superchat-bridge-amt-test--with-rig
      (let ((stream (superchat-bridge-api-anthropic-messages-stream
                     (superchat-bridge-amt-test--fallback-model)
                     (superchat-bridge-amt-test--tools-context
                      (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")))
                     '(:api-key "test-key"))))
        (superchat-bridge-amt-test--answer
         (lambda (req _attempt)
           (apply #'superchat-bridge-amt-test--error-response req case)))
        (should (equal (superchat-bridge-assistant-message-stop-reason
                        (superchat-bridge-event-stream-result stream))
                       "error"))
        (should (= (length superchat-bridge-amt-test--requests) 1))))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-second-rejection ()
  "(upstream anthropic-strict-tool-fallback) A second rejection is final."
  (superchat-bridge-amt-test--with-rig
    (let ((stream (superchat-bridge-api-anthropic-messages-stream
                   (superchat-bridge-amt-test--fallback-model)
                   (superchat-bridge-amt-test--tools-context
                    (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")
                          (superchat-bridge-amt-test--fallback-tool "required" "require")))
                   '(:api-key "test-key"))))
      (superchat-bridge-amt-test--answer
       (lambda (req attempt)
         (superchat-bridge-amt-test--error-response
          req (if (= attempt 1) superchat-bridge-amt-test--grammar-error
                "Required grammar is still too large"))))
      (let ((result (superchat-bridge-event-stream-result stream)))
        (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
        (should (string-search "Required grammar is still too large"
                               (superchat-bridge-assistant-message-error-message result)))
        (should (= (length superchat-bridge-amt-test--requests) 2))
        (should (eq (superchat-bridge-amt-test--get (superchat-bridge-amt-test--body 1)
                                                    "tools" 1 "strict")
                    t))))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-not-for-hook-errors ()
  "(upstream anthropic-strict-tool-fallback) A hook's error is not downgraded."
  (superchat-bridge-amt-test--with-rig
    (let ((result (superchat-bridge-event-stream-result
                   (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--fallback-model)
                    (superchat-bridge-amt-test--tools-context
                     (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")))
                    (list :api-key "test-key"
                          :on-payload (lambda (_params _model)
                                        (error "400 %s"
                                               superchat-bridge-amt-test--grammar-error)))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason result) "error"))
      (should-not superchat-bridge-amt-test--requests)
      (should-not (superchat-bridge-assistant-message-diagnostics result)))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-honours-abort ()
  "(upstream anthropic-strict-tool-fallback) An abort stops the fallback."
  (superchat-bridge-amt-test--with-rig
    (let* ((signal (superchat-bridge-make-abort-signal))
           (stream (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--fallback-model)
                    (superchat-bridge-amt-test--tools-context
                     (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")))
                    (list :api-key "test-key" :signal signal))))
      (superchat-bridge-abort-signal-abort signal)
      (superchat-bridge-amt-test--error-response (superchat-bridge-amt-test--request))
      (should (equal (superchat-bridge-assistant-message-stop-reason
                      (superchat-bridge-event-stream-result stream))
                     "aborted"))
      (should (= (length superchat-bridge-amt-test--requests) 1)))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-not-after-headers ()
  "(upstream anthropic-strict-tool-fallback) An SSE error is not retried."
  (superchat-bridge-amt-test--with-rig
    (let ((stream (superchat-bridge-api-anthropic-messages-stream
                   (superchat-bridge-amt-test--fallback-model)
                   (superchat-bridge-amt-test--tools-context
                    (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")))
                   '(:api-key "test-key"))))
      (superchat-bridge-amt-test--respond
       (superchat-bridge-amt-test--request)
       (superchat-bridge-amt-test--sse-ended
        (list (superchat-bridge-amt-test--e
               "error"
               `((type . "error")
                 (error . ((type . "invalid_request_error")
                           (message . ,superchat-bridge-amt-test--grammar-error))))))))
      (should (equal (superchat-bridge-assistant-message-stop-reason
                      (superchat-bridge-event-stream-result stream))
                     "error"))
      (should (= (length superchat-bridge-amt-test--requests) 1)))))

(ert-deftest superchat-bridge-amt-test-strict-fallback-per-request ()
  "(upstream anthropic-strict-tool-fallback) Later requests try strict again."
  (superchat-bridge-amt-test--with-rig
    (let ((transcript (superchat-bridge-amt-test--tools-context
                       (list (superchat-bridge-amt-test--fallback-tool "lookup" "prefer")))))
      (superchat-bridge-api-anthropic-messages-stream
       (superchat-bridge-amt-test--fallback-model) transcript '(:api-key "test-key"))
      (superchat-bridge-amt-test--error-response (superchat-bridge-amt-test--request 0))
      (superchat-bridge-amt-test--success-response (superchat-bridge-amt-test--request 1))
      (superchat-bridge-api-anthropic-messages-stream
       (superchat-bridge-amt-test--fallback-model) transcript '(:api-key "test-key"))
      (superchat-bridge-amt-test--success-response (superchat-bridge-amt-test--request 2))
      (should (equal (mapcar (lambda (index)
                               (cdr (assoc "strict"
                                           (superchat-bridge-amt-test--get
                                            (superchat-bridge-amt-test--body index) "tools" 0))))
                             '(0 1 2))
                     '(t nil t))))))

;;; Retries and errors over HTTP

(ert-deftest superchat-bridge-amt-test-http-error-text ()
  "An HTTP error reads as the Anthropic SDK's message: status, then the body."
  (superchat-bridge-amt-test--with-rig
    (dolist (case '(("429 Too Many Requests"
                     "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow\"}}"
                     "429 {\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow\"}}")
                    ("502 Bad Gateway" "upstream down" "502 upstream down")
                    ("500 Internal Server Error" "" "500 status code (no body)")))
      (setq superchat-bridge-amt-test--requests nil)
      (let* ((stream (superchat-bridge-api-anthropic-messages-stream
                      (superchat-bridge-amt-test--model) (superchat-bridge-amt-test--context)
                      '(:api-key "k")))
             (req (superchat-bridge-amt-test--request)))
        (superchat-bridge-amt-test--respond
         req (nth 1 case) (format "HTTP/1.1 %s\r\n\r\n" (nth 0 case)))
        (should (equal (superchat-bridge-assistant-message-error-message
                        (superchat-bridge-event-stream-result stream))
                       (nth 2 case)))))))

(ert-deftest superchat-bridge-amt-test-429-retry-then-success ()
  "A 429 with retry-after is retried when `:max-retries' allows it."
  (superchat-bridge-amt-test--with-rig
    (let* ((stream (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--model) (superchat-bridge-amt-test--context)
                    '(:api-key "k" :max-retries 2)))
           (first (superchat-bridge-amt-test--request)))
      (superchat-bridge-amt-test--respond
       first "{\"type\":\"error\",\"error\":{\"type\":\"rate_limit_error\",\"message\":\"slow\"}}"
       "HTTP/1.1 429 Too Many Requests\r\nretry-after: 2\r\n\r\n")
      (should (= (length superchat-bridge-amt-test--timers) 1))
      (should (= (car (nth 0 superchat-bridge-amt-test--timers)) 2.0))
      (funcall (nth 1 (nth 0 superchat-bridge-amt-test--timers)))
      (should (= (length superchat-bridge-amt-test--requests) 2))
      (superchat-bridge-amt-test--respond
       (superchat-bridge-amt-test--request 1)
       (superchat-bridge-amt-test--sse (superchat-bridge-amt-test--minimal-events)))
      (should (equal (superchat-bridge-amt-test--types (superchat-bridge-event-stream-to-list stream))
                     '("start" "text_start" "text_delta" "text_end" "done"))))))

(ert-deftest superchat-bridge-amt-test-connection-error-and-abort ()
  "A curl failure and an abort mid-stream end the stream with their kinds."
  (superchat-bridge-amt-test--with-rig
    (let* ((stream (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--model) (superchat-bridge-amt-test--context)
                    '(:api-key "k")))
           (req (superchat-bridge-amt-test--request)))
      (superchat-bridge-amt-test--exit req 7 "curl: (7) Failed to connect")
      (let ((message (superchat-bridge-event-stream-result stream)))
        (should (equal (superchat-bridge-assistant-message-stop-reason message) "error"))
        (should (string-prefix-p "Connection error: "
                                 (superchat-bridge-assistant-message-error-message message))))))
  (superchat-bridge-amt-test--with-rig
    (let* ((signal (superchat-bridge-make-abort-signal))
           (stream (superchat-bridge-api-anthropic-messages-stream
                    (superchat-bridge-amt-test--model) (superchat-bridge-amt-test--context)
                    (list :api-key "k" :signal signal)))
           (req (superchat-bridge-amt-test--request)))
      (superchat-bridge-amt-test--feed-pieces
       req (concat superchat-bridge-amt-test--head
                   (superchat-bridge-amt-test--sse-ended
                    (seq-take (superchat-bridge-amt-test--minimal-events) 3))))
      (superchat-bridge-abort-signal-abort signal)
      (should (= (superchat-bridge-amt-test--req-killed req) 1))
      (superchat-bridge-amt-test--exit req 143 "")
      (let ((message (superchat-bridge-event-stream-result stream)))
        (should (equal (superchat-bridge-assistant-message-stop-reason message) "aborted"))
        (should (equal (superchat-bridge-text-content-text
                        (car (superchat-bridge-message-content message)))
                       "Hello"))))))

(provide 'superchat-bridge-api-anthropic-messages-test)

;;; superchat-bridge-api-anthropic-messages-test.el ends here
