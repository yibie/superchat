;;; llm-pi-ai-utils-retry.el --- Retry of an assistant call, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/retry.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `llm-pi-ai-retry-assistant-call'      the source's `retryAssistantCall'
;;   `llm-pi-ai-retryable-assistant-error-p'  `isRetryableAssistantError'
;;   `llm-pi-ai-retry-delay-ms'            `retryDelayMs'
;;   `llm-pi-ai-retry-policy'              the `RetryPolicy' interface
;;   `llm-pi-ai-default-max-agent-retry-delay-ms'
;;                                         `DEFAULT_MAX_AGENT_RETRY_DELAY_MS'
;;
;; How a call that awaits becomes a call with callbacks
;; ----------------------------------------------------
;;
;; The source `await's the producer and `sleep's between tries.
;; Emacs has no await, so:
;;
;;  - PRODUCE is a function of one argument, DONE.  It starts one
;;    call and, when that call ends, calls DONE once with a result:
;;    (:ok MESSAGE) for the AssistantMessage, (:error ERROR) where the
;;    source's promise rejects.
;;  - The sleep is a timer, started through
;;    `llm-pi-ai-retry-run-at-time-function' (`run-at-time' unless a test
;;    changes it) and stopped, when the call is aborted, through
;;    `llm-pi-ai-retry-cancel-timer-function'.  Nothing blocks.
;;  - The caller's FINISH function is called exactly once, with the
;;    same two shapes: (:ok MESSAGE) where the source's promise
;;    resolves, (:error ERROR) where it rejects.  ERROR is whatever
;;    PRODUCE reported, or a plist (:message TEXT :lisp-error CONDITION)
;;    when PRODUCE or a callback signalled a Lisp error: an error
;;    reaches the caller, it is not lost in a timer.  An aborted call
;;    is not an error: FINISH gets (:ok MESSAGE) with an `aborted'
;;    message, as the source returns one.
;;  - The callbacks of the source may return promises that it
;;    awaits; the Lisp ones are plain functions and their return
;;    value is ignored.
;;
;; The abort signal
;; ----------------
;;
;; The source's `AbortSignal' is `llm-pi-ai-abort-signal' of
;; `llm-pi-ai-utils-abort'.  A SIGNAL argument of nil means that the
;; call cannot be aborted, as `undefined' does.

;;; Code:

(require 'cl-lib)
(require 'llm-pi-ai-types)
(require 'llm-pi-ai-utils-abort)

;;; Abort signal

(defun llm-pi-ai--signal-aborted-p (signal)
  "Return non-nil when SIGNAL is a signal that has been aborted.
nil, standing for no signal, is never aborted: the source's
`signal?.aborted'."
  (and signal (llm-pi-ai-abort-signal-aborted-p signal)))

;;; Timers

(defvar llm-pi-ai-retry-run-at-time-function #'run-at-time
  "Function that starts the backoff timer: (SECONDS nil FUNCTION).
Tests bind it to a function that records FUNCTION instead of
waiting, and returns something `llm-pi-ai-retry-cancel-timer-function'
can take.")

(defvar llm-pi-ai-retry-cancel-timer-function #'cancel-timer
  "Function that stops a timer `llm-pi-ai-retry-run-at-time-function' made.")

(defun llm-pi-ai--start-timer (delay-ms function)
  "Call FUNCTION after DELAY-MS milliseconds, through the timer variable.
Return the timer."
  (funcall llm-pi-ai-retry-run-at-time-function
           (/ (max 0 delay-ms) 1000.0) nil function))

;;; Patterns

(defconst llm-pi-ai--non-retryable-provider-limit-patterns
  '(;; OpenCode Go/free-tier limits returned as 429 JSON error types by OpenCode's
    ;; Zen API.  These are subscription/account limits, not transient throttles.
    "GoUsageLimitError"
    "FreeUsageLimitError"
    ;; OpenCode Go subscription-limit text asks users to enable available-balance
    ;; usage after rolling/weekly/monthly limits are reached.
    "Monthly usage limit reached"
    "available balance"
    ;; Generic quota/budget/billing exhaustion.  `insufficient_quota' is OpenAI's
    ;; quota/billing error code; the other strings cover common gateway wording.
    "insufficient_quota"
    "out of budget"
    "quota exceeded"
    "billing"
    ;; Sign in with ChatGPT: the subscription's shared usage limit, which resets
    ;; after hours rather than seconds.
    "subscription_sharing_usage_limit_exceeded"
    ;; Sign in with ChatGPT on a ChatGPT Business plan: a usage limit set by the
    ;; workspace admin.  Only the admin can raise it.
    "subscription_sharing_workplace_individual_usage_limit_exceeded")
  "The source's `NON_RETRYABLE_PROVIDER_LIMIT_ERROR_PATTERN' parts.
Emacs regexps, matched without regard to case.")

(defconst llm-pi-ai--retryable-provider-error-patterns
  '(;; Generic provider load, HTTP status, and server-side transient failures.
    "overloaded"
    "server_busy"
    "servers are currently busy"
    "currently experiencing high demand"
    "model is at capacity"
    "rate.?limit"
    "too many requests"
    ;; The status codes (429 500 502 503 504 520 524) are not here: Emacs
    ;; regexps have no lookbehind, see `llm-pi-ai--status-code-in-text-p'.
    ;; Also matches "The service is temporarily unavailable" (#10965).
    "service.?\\(?:is.?\\)?\\(?:temporarily.?\\)?unavailable"
    "server.?error"
    "internal.?error"
    ;; Wrapper/provider text for transient upstream failures, including OpenRouter
    ;; "Provider returned error" responses (#2264) and gateway wording such as
    ;; "The upstream provider returned an error" (#10842).
    "provider.?returned.?\\(?:an.?\\)?error"
    "exceeded request buffer limit while retrying upstream"
    ;; Gateway failure relaying the upstream response stream, sent as an OpenAI
    ;; Responses `response.failed' event without an error code (#10920).
    "unable to forward upstream"
    ;; Other gateway upstream failures relayed as `response.failed' error codes (#10920).
    "upstream.?request.?failed"
    "upstream_stream_read_error"
    "stream was interrupted"
    "http/?2 stream failed"
    ;; Network, proxy, and fetch transport failures.  This includes OpenAI Codex
    ;; raw-fetch failures such as "upstream connect", "connection refused", and
    ;; "reset before headers" (#733), plus OpenRouter connection drops (#3317).
    "network.?error"
    "connection.?error"
    "connection.?refused"
    "connection.?lost"
    "other side closed"
    "fetch failed"
    ;; node-fetch system errors, e.g. the Google ADC OAuth token refresh used by
    ;; google-vertex.  The reason is often empty (#10996).
    "request to [^[:space:]]+ failed, reason:"
    "getaddrinfo"
    "ENOTFOUND"
    "EAI_AGAIN"
    "upstream.?connect"
    "reset before headers"
    "socket hang up"
    "socket connection was closed"
    "timed? out"
    "timeout"
    "terminated"
    ;; WebSocket transports can report close/error text instead of HTTP/fetch text.
    "websocket.?closed"
    "websocket.?error"
    ;; Premature stream endings from SDKs and transports.  Anthropic can throw
    ;; "stream ended without ..." and "Anthropic stream ended before message_stop"
    ;; (#4433); Bedrock/Smithy can throw an HTTP/2 no-response error (#3594).
    "ended without"
    "stream ended before message_stop"
    "stream ended before a terminal response event"
    "http2 request did not get a response"
    ;; Codex CLI mid-stream disconnect, relayed by proxies that wrap the Codex CLI,
    ;; e.g. "stream disconnected before completion: stream closed before
    ;; response.completed" (#11057).
    "stream disconnected before completion"
    ;; Node ERR_HTTP2_STREAM_CANCEL: the HTTP/2 session died before the request was
    ;; sent, e.g. after the Bedrock SDK's 5-minute session timeout (#10379).
    "pending stream has been canceled"
    ;; Provider-requested retry delay cap failures should flow through the outer
    ;; retry policy so callers can surface/abort the backoff (#1123).
    "retry delay"
    ;; Explicit retry guidance emitted mid-stream by OpenAI Responses and Bedrock
    ;; stream exceptions (#6019).
    "you can retry your request"
    "try your request again"
    "please retry your request"
    ;; gRPC based providers (e.g. NVIDIA NIM)
    "ResourceExhausted"
    ;; Sign in with ChatGPT: usage or user data temporarily unavailable.  Usage
    ;; failures can arrive mid-stream without an HTTP 503 in the message.
    "subscription_sharing_usage_unavailable"
    "subscription_sharing_user_unavailable")
  "The source's `RETRYABLE_PROVIDER_ERROR_PATTERN' parts, but one.
Emacs regexps, matched without regard to case.  The part left out
is the status codes, which `llm-pi-ai--status-code-in-text-p' tests.")

(defun llm-pi-ai--status-code-in-text-p (text)
  "Return non-nil when TEXT holds a retryable HTTP status code by itself.
The source's `(?<![a-z0-9])(?:429|500|502|503|504|520|524)(?![a-z0-9])':
the code must not be part of a longer word or number, e.g. a request
id or a byte count (#11057).  Emacs regexps have no lookaround, so
each match of the code is checked for its neighbours."
  (let ((case-fold-search t)
        (start 0)
        (found nil))
    (while (and (not found)
                (string-match "\\(?:429\\|50[0234]\\|52[04]\\)" text start))
      (let ((from (match-beginning 0))
            (to (match-end 0)))
        (if (and (or (= from 0)
                     (not (string-match-p "[a-z0-9]" (string (aref text (1- from))))))
                 (or (= to (length text))
                     (not (string-match-p "[a-z0-9]" (string (aref text to))))))
            (setq found t)
          (setq start (1+ from)))))
    found))

;;; Policy

(llm-pi-ai-defstruct (llm-pi-ai-retry-policy
                (:constructor llm-pi-ai-make-retry-policy)
                (:copier llm-pi-ai-copy-retry-policy))
  "A `RetryPolicy': bounded attempts with exponential backoff.
`enabled' turns retries on.  `max-retries' is the most retries (0
means none; the first call never counts).  `base-delay-ms' is the
base delay: the delay before retry N is `base-delay-ms' times 2 to
the N-1, before jitter.  `max-agent-delay-ms' caps each delay, and
is 60 seconds when nil."
  (enabled nil)
  (max-retries 0)
  (base-delay-ms 0)
  (max-agent-delay-ms nil))

(defconst llm-pi-ai-default-max-agent-retry-delay-ms 60000
  "`DEFAULT_MAX_AGENT_RETRY_DELAY_MS' of the source.")

(defconst llm-pi-ai--max-safe-integer 9007199254740991
  "JavaScript's `Number.MAX_SAFE_INTEGER'.")

(defun llm-pi-ai-retry-delay-ms (policy attempt)
  "Return the delay in ms before retry number ATTEMPT (1 for the first).
The source's `retryDelayMs'.  POLICY is a `llm-pi-ai-retry-policy'."
  (let* ((power (max 0 (1- attempt)))
         ;; 2**1024 is Infinity in JavaScript: no safe integer comes of it.
         (delay (and (< power 1024)
                     (* (llm-pi-ai-retry-policy-base-delay-ms policy)
                        (expt 2 power))))
         (safe-delay (if (and delay
                              (not (and (floatp delay) (isnan delay)))
                              (or (integerp delay)
                                  (and (floatp delay) (= delay (ffloor delay))))
                              (<= (abs delay) llm-pi-ai--max-safe-integer))
                         delay
                       llm-pi-ai--max-safe-integer)))
    (min safe-delay
         (or (llm-pi-ai-retry-policy-max-agent-delay-ms policy)
             llm-pi-ai-default-max-agent-retry-delay-ms))))

;;; Classification

(defun llm-pi-ai-retryable-assistant-error-p (message)
  "Return non-nil when assistant MESSAGE failed in a way worth a retry.
The source's `isRetryableAssistantError': MESSAGE must have the stop
reason `error' and an error message, which must not match a provider
limit (quota, billing: those do not clear by waiting) and must match
a transient provider or transport pattern.  This is a classifier,
not a retry policy; callers handle context overflow first."
  (let ((error-message (llm-pi-ai-assistant-message-error-message message)))
    (and (equal (llm-pi-ai-assistant-message-stop-reason message) "error")
         error-message
         (not (equal error-message ""))
         (let ((case-fold-search t))
           (and (not (cl-some (lambda (pattern)
                                (string-match-p pattern error-message))
                              llm-pi-ai--non-retryable-provider-limit-patterns))
                (or (cl-some (lambda (pattern)
                               (string-match-p pattern error-message))
                             llm-pi-ai--retryable-provider-error-patterns)
                    (llm-pi-ai--status-code-in-text-p error-message))))
         t)))

;;; The retry loop

(defun llm-pi-ai-retry-assistant-call (produce policy signal callbacks finish)
  "Run PRODUCE with bounded retry on transient errors.
The source's `retryAssistantCall'.  Everything is by callback; see the
file commentary for the shapes.

PRODUCE is called with one argument, DONE, and starts one assistant
call.  POLICY is a `llm-pi-ai-retry-policy', or nil for no retry.
SIGNAL is an `llm-pi-ai-abort-signal' or nil.  CALLBACKS is a plist
of three optional functions, as the source's `RetryCallbacks':

  :on-retry-scheduled (ATTEMPT MAX-ATTEMPTS DELAY-MS ERROR-MESSAGE)
      before the backoff of each retry (ATTEMPT from 1);
  :on-retry-attempt-start ()
      after the backoff, just before the retried call starts;
  :on-retry-finished (SUCCESS ATTEMPT &optional FINAL-ERROR)
      once, when the loop ends after at least one retry was
      scheduled: SUCCESS if a later call completed normally.

FINISH is called once with (:ok MESSAGE) or (:error ERROR).  A
successful message is returned at once; so is an aborted one, never
retried; so is an error that is not retryable, or one after
`max-retries' retries.  Otherwise the call is retried after
`llm-pi-ai-retry-delay-ms'.  An abort during that wait stops the
timer and finishes with the last message as an `aborted' one with
no error message."
  (let* ((max-attempts (if (and policy (llm-pi-ai-retry-policy-enabled policy))
                           (llm-pi-ai-retry-policy-max-retries policy)
                         0))
         (attempt 0)
         (last-retry nil)               ; (ATTEMPT . ERROR-MESSAGE)
         (finished nil))
    (cl-labels
        ((callback (name &rest args)
           (let ((function (plist-get callbacks name)))
             (when function (apply function args))))
         (conclude (result)
           (setq finished t)
           (funcall finish result))
         (guarded (thunk)
           ;; An error of THUNK, our own or a callback's, goes to FINISH.
           (condition-case err
               (funcall thunk)
             (error
              (if finished
                  (signal (car err) (cdr err))
                (conclude (list :error
                                (list :message (error-message-string err)
                                      :lisp-error err)))))))
         (start ()
           (guarded
            (lambda ()
              (funcall produce
                       (lambda (result) (guarded (lambda () (respond result))))))))
         (respond (result)
           (if (eq (car result) :error)
               (conclude result)
             (let* ((response (cadr result))
                    (stop-reason (llm-pi-ai-assistant-message-stop-reason response)))
               (cond
                ;; Abort: terminal but not successful.
                ((equal stop-reason "aborted")
                 (when last-retry
                   (callback :on-retry-finished nil (car last-retry)))
                 (conclude result))
                ;; Success: non-error, non-abort responses return as they are.
                ((not (equal stop-reason "error"))
                 (when last-retry
                   (callback :on-retry-finished t (car last-retry)))
                 (conclude result))
                ;; Non-retryable, or budget exhausted: the final error message.
                ((or (>= attempt max-attempts)
                     (not (llm-pi-ai-retryable-assistant-error-p response)))
                 (when last-retry
                   (callback :on-retry-finished nil (car last-retry)
                             (llm-pi-ai-assistant-message-error-message response)))
                 (conclude result))
                (t (schedule response))))))
         (schedule (response)
           (setq attempt (1+ attempt))
           (let* ((error-message
                   (let ((text (llm-pi-ai-assistant-message-error-message response)))
                     (if (or (null text) (equal text "")) "Unknown error" text)))
                  (delay-ms (llm-pi-ai-retry-delay-ms policy attempt))
                  (attempt-number attempt)
                  (timer nil)
                  (on-abort nil))
             (setq last-retry (cons attempt error-message))
             (callback :on-retry-scheduled attempt max-attempts delay-ms
                       error-message)
             (cl-flet ((aborted-during-backoff ()
                         ;; Same AssistantMessage shape as a provider
                         ;; stream abort, so callers need not care
                         ;; when the cancellation came.
                         (callback :on-retry-finished nil attempt-number
                                   error-message)
                         (let ((aborted (copy-sequence response)))
                           (setf (llm-pi-ai-assistant-message-error-message aborted) nil
                                 (llm-pi-ai-assistant-message-stop-reason aborted)
                                 "aborted")
                           (conclude (list :ok aborted)))))
               (if (llm-pi-ai--signal-aborted-p signal)
                   (guarded #'aborted-during-backoff)
                 (setq on-abort
                       (lambda ()
                         (when timer
                           (funcall llm-pi-ai-retry-cancel-timer-function timer))
                         (guarded #'aborted-during-backoff)))
                 (when signal
                   (llm-pi-ai-abort-signal-add-listener signal on-abort))
                 (setq timer
                       (llm-pi-ai--start-timer
                        delay-ms
                        (lambda ()
                          (when signal
                            (llm-pi-ai-abort-signal-remove-listener signal on-abort))
                          (guarded
                           (lambda ()
                             (callback :on-retry-attempt-start)
                             (funcall produce
                                      (lambda (result)
                                        (guarded (lambda () (respond result)))))))))))))))
      (start))))

(provide 'llm-pi-ai-utils-retry)

;;; llm-pi-ai-utils-retry.el ends here
