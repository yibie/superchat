;;; llm-pi-ai-utils-retry-test.el --- Tests for llm-pi-ai-utils-retry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-retry.el', translated from upstream
;; `test/retry.test.ts' (the describe blocks `provider retry
;; classification', `retryDelayMs' and `retryAssistantCall', in order).
;; `fauxAssistantMessage' is not ported yet; `llm-pi-ai-retry-test--message'
;; stands in for it.  No test waits: the timer is replaced by a list of
;; pending (DELAY-MS . FUNCTION) pairs that a test runs by hand, the
;; "fake timers" of the source's tests.  The cases marked "port" test
;; the Lisp shapes (finish results, errors) and the Emacs regexps.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-retry)
(require 'llm-pi-ai-utils-abort)

(defvar llm-pi-ai-retry-test--timers nil
  "Pending fake timers: a list of (ID DELAY-MS FUNCTION), oldest last.")

(defvar llm-pi-ai-retry-test--cancelled nil
  "IDs of fake timers that were cancelled.")

(defun llm-pi-ai-retry-test--message (&optional stop-reason error-message text)
  "Make an assistant message: STOP-REASON (default stop), ERROR-MESSAGE, TEXT."
  (llm-pi-ai-make-assistant-message
   :content (list (llm-pi-ai-make-text-content :text (or text "")))
   :api "faux:api" :provider "faux" :model "faux-1"
   :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason (or stop-reason "stop") :error-message error-message
   :timestamp 0))

(defmacro llm-pi-ai-retry-test--with-fake-timers (&rest body)
  "Run BODY with the retry timer replaced by a hand-run list."
  `(let* ((llm-pi-ai-retry-test--timers nil)
         (llm-pi-ai-retry-test--cancelled nil)
         (counter 0)
         (llm-pi-ai-retry-run-at-time-function
          (lambda (seconds _repeat function)
            (setq counter (1+ counter))
            (push (list counter (round (* seconds 1000)) function)
                  llm-pi-ai-retry-test--timers)
            counter))
         (llm-pi-ai-retry-cancel-timer-function
          (lambda (id)
            (push id llm-pi-ai-retry-test--cancelled)
            (setq llm-pi-ai-retry-test--timers
                  (seq-remove (lambda (timer) (eql (car timer) id))
                              llm-pi-ai-retry-test--timers)))))
     ,@body))

(defun llm-pi-ai-retry-test--fire-oldest ()
  "Run the oldest pending fake timer; return its delay in ms."
  (let ((timer (car (last llm-pi-ai-retry-test--timers))))
    (setq llm-pi-ai-retry-test--timers (butlast llm-pi-ai-retry-test--timers))
    (funcall (nth 2 timer))
    (nth 1 timer)))

(defun llm-pi-ai-retry-test--retryable-p (error-message)
  (llm-pi-ai-retryable-assistant-error-p
   (llm-pi-ai-retry-test--message "error" error-message)))

;;; Classification

(ert-deftest llm-pi-ai-utils-retry-test-explicit-retry-guidance ()
  "Explicit provider retry guidance matches."
  (dolist (text '("An error occurred while processing your request. You can retry your request, or contact us through our help center at help.openai.com if the error persists. Please include the request ID req_******** in your message."
                  "{\"message\":\"The system encountered an unexpected error during processing. Try your request again.\"}"
                  "ResourceExhausted: Worker local total request limit reached (288/48)"
                  ;; Bun fetch socket drop wording
                  "The socket connection was closed unexpectedly. For more information, pass `verbose: true` in the second argument to fetch()"
                  "Error: exceeded request buffer limit while retrying upstream"))
    (should (llm-pi-ai-retry-test--retryable-p text))))

(ert-deftest llm-pi-ai-utils-retry-test-transport-wording ()
  "DNS, stream cancellation, service-unavailable and gateway wording match."
  (dolist (text '("The pending stream has been canceled (caused by: getaddrinfo ENOTFOUND bedrock-runtime.us-east-1.amazonaws.com)"
                  "connect ENOTFOUND api.example.com"
                  "EAI_AGAIN api.example.com"
                  "getaddrinfo failed for api.example.com"
                  "The pending stream has been canceled"
                  "The pending stream has been canceled (caused by: socket closed)"
                  "The service is temporarily unavailable. Please retry later."
                  "Service unavailable: The service is temporarily unavailable."
                  "request to https://oauth2.googleapis.com/token failed, reason:"
                  "request to https://oauth2.googleapis.com/token failed, reason: connect ECONNREFUSED 127.0.0.1:9"
                  "unknown: Unable to forward upstream stream"
                  "upstream_error: Upstream request failed"
                  "upstream_stream_read_error: Upstream response stream was interrupted"
                  "Upstream HTTP/2 stream failed"
                  "OpenAI Responses stream ended before a terminal response event"
                  "The system is currently experiencing high demand and cannot process your request. Your request exceeds the maximum usage size allowed during peak load. For improved capacity reliability, consider switching to Provisioned Throughput."))
    (should (llm-pi-ai-retry-test--retryable-p text))))

(ert-deftest llm-pi-ai-utils-retry-test-limit-errors-not-retryable ()
  "Provider limit errors are not retried."
  (dolist (text '("429 quota exceeded"
                  "OpenAI API error (429): {\"code\":\"subscription_sharing_usage_limit_exceeded\",\"message\":\"Usage limit reached.\"}"
                  "OpenAI API error (429): {\"code\":\"subscription_sharing_workplace_individual_usage_limit_exceeded\",\"message\":\"Usage limit reached.\"}"))
    (should-not (llm-pi-ai-retry-test--retryable-p text)))
  (dolist (text '("subscription_sharing_usage_unavailable: Usage cannot be checked."
                  "subscription_sharing_user_unavailable: User cannot be loaded."))
    (should (llm-pi-ai-retry-test--retryable-p text))))

(ert-deftest llm-pi-ai-utils-retry-test-classifies-error-messages ()
  "Assistant error messages are classified."
  (should (llm-pi-ai-retry-test--retryable-p "overloaded_error"))
  (should (llm-pi-ai-retry-test--retryable-p "520 status code (no body)"))
  (should (llm-pi-ai-retry-test--retryable-p "524 status code (no body)"))
  (should-not (llm-pi-ai-retryable-assistant-error-p
               (llm-pi-ai-retry-test--message))))

(ert-deftest llm-pi-ai-utils-retry-test-status-codes-in-longer-numbers ()
  "Status codes inside ids or longer numbers are not status codes (#11057)."
  (dolist (text '("401 {\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"},\"request_id\":\"req_011CU503xyz\"}"
                  "400 image exceeds 5 MB maximum: 5316784 bytes > 5242880 bytes"))
    (should-not (llm-pi-ai-retry-test--retryable-p text))))

(ert-deftest llm-pi-ai-utils-retry-test-port-classification-node ()
  "Port: classifications of the source's regexps, taken with node 22."
  ;; isRetryableAssistantError({stopReason:"error",errorMessage:X}) for each X
  (dolist (case '(("error 503" t) ("req 5030" nil) ("x429y" nil) ("429" t)
                  ("a 504." t) ("HTTP/2 stream failed" t)
                  ("http2 request did not get a response" t)
                  ("Rate-Limit" t) ("Timed out" t) ("time out" t)
                  ("SERVER_BUSY" t) ("bad request 400" nil)
                  ("request to http://x failed, reason: boom" t)
                  ("quota exceeded 429" nil) ("internal\nerror" nil)
                  ("rate\nlimit" nil) ("503abc" nil) ("abc503" nil)
                  ("id_503" t) ("a503" nil)))
    (should (eq (llm-pi-ai-retry-test--retryable-p (car case))
                (cadr case)))))

(ert-deftest llm-pi-ai-utils-retry-test-port-not-an-error ()
  "Port: only a message with the stop reason error and a message counts."
  (should-not (llm-pi-ai-retryable-assistant-error-p
               (llm-pi-ai-retry-test--message "stop" "overloaded")))
  (should-not (llm-pi-ai-retryable-assistant-error-p
               (llm-pi-ai-retry-test--message "error" nil)))
  (should-not (llm-pi-ai-retryable-assistant-error-p
               (llm-pi-ai-retry-test--message "error" ""))))

;;; Delay

(ert-deftest llm-pi-ai-utils-retry-test-delay-caps ()
  "Agent retry delay is capped (#8826)."
  (should (= (llm-pi-ai-retry-delay-ms
              (llm-pi-ai-make-retry-policy :base-delay-ms 2000) 6)
             60000))
  (should (= (llm-pi-ai-retry-delay-ms
              (llm-pi-ai-make-retry-policy :base-delay-ms 2000
                                           :max-agent-delay-ms 5000)
              5)
             5000))
  (should (= (llm-pi-ai-retry-delay-ms
              (llm-pi-ai-make-retry-policy :base-delay-ms 2000
                                           :max-agent-delay-ms 0)
              5)
             0)))

(ert-deftest llm-pi-ai-utils-retry-test-port-delay-node ()
  "Port: delays as the source computes them (node 22)."
  (let ((delay (lambda (base attempt &optional cap)
                 (llm-pi-ai-retry-delay-ms
                  (llm-pi-ai-make-retry-policy :base-delay-ms base
                                               :max-agent-delay-ms cap)
                  attempt))))
    ;; retryDelayMs({baseDelayMs:1000},1) 1000; 3 -> 4000; 0 -> 1000
    (should (= (funcall delay 1000 1) 1000))
    (should (= (funcall delay 1000 3) 4000))
    (should (= (funcall delay 1000 0) 1000))
    ;; {baseDelayMs:0.5}, 2 -> 1
    (should (= (funcall delay 0.5 2) 1))
    ;; {baseDelayMs:100,maxAgentDelayMs:1e9}, 60 -> 1e9 (not safe: capped)
    (should (= (funcall delay 100 60 1000000000) 1000000000))
    ;; {baseDelayMs:0}, 2000 -> 60000; {baseDelayMs:2}, 2000 -> 60000
    (should (= (funcall delay 0 2000) 60000))
    (should (= (funcall delay 2 2000) 60000))))

;;; retryAssistantCall

(defun llm-pi-ai-retry-test--run (messages policy &optional signal callbacks)
  "Run `llm-pi-ai-retry-assistant-call' over MESSAGES, a list produced in turn.
Each element is a message, or a function of no argument that returns
one, called when the call starts.  Return (RESULT CALLS), where
RESULT is what FINISH got (nil if not yet), CALLS the number of
produce calls so far, as a cons cell the caller may read later."
  (let* ((calls (list 0))
         (result (list nil)))
    (llm-pi-ai-retry-assistant-call
     (lambda (done)
       (let ((next (nth (car calls) messages)))
         (setcar calls (1+ (car calls)))
         (funcall done (list :ok (if (functionp next) (funcall next) next)))))
     policy signal callbacks
     (lambda (r) (setcar result r)))
    (list result calls)))

(defconst llm-pi-ai-retry-test--enabled
  (llm-pi-ai-make-retry-policy :enabled t :max-retries 3 :base-delay-ms 0))

(defun llm-pi-ai-retry-test--drain ()
  "Fire fake timers until none is left."
  (while llm-pi-ai-retry-test--timers
    (llm-pi-ai-retry-test--fire-oldest)))

(ert-deftest llm-pi-ai-utils-retry-test-success-immediate ()
  "A successful response is returned at once, no retry."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message nil nil "ok"))
                llm-pi-ai-retry-test--enabled))
          (result (car (car out))))
     (should (eq (car result) :ok))
     (should (equal (llm-pi-ai-text-content-text
                     (car (llm-pi-ai-message-content (cadr result))))
                    "ok"))
     (should (= (car (cadr out)) 1))
     (should (null llm-pi-ai-retry-test--timers)))))

(ert-deftest llm-pi-ai-utils-retry-test-aborted-not-retried ()
  "An aborted message is not retried."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((scheduled nil)
          (out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message "aborted"))
                llm-pi-ai-retry-test--enabled nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))))))
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled))))

(ert-deftest llm-pi-ai-utils-retry-test-non-retryable-error ()
  "A non-retryable error (quota/billing) is returned at once."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message "error" "insufficient_quota"))
                llm-pi-ai-retry-test--enabled nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))
                      :on-retry-finished (lambda (&rest _) (setq finished t))))))
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled)
     (should-not finished))))

(ert-deftest llm-pi-ai-utils-retry-test-retries-up-to-max ()
  "A transient error is retried up to max-retries, then the error returns."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (messages (make-list 10 (llm-pi-ai-retry-test--message "error" "terminated")))
          (out (llm-pi-ai-retry-test--run
                messages llm-pi-ai-retry-test--enabled nil
                (list :on-retry-scheduled
                      (lambda (&rest args) (push args scheduled))
                      :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (llm-pi-ai-retry-test--drain)
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 4))    ; 1 initial + 3 retries
     (should (= (length scheduled) 3))
     (should (equal finished '((nil 3 "terminated")))))))

(ert-deftest llm-pi-ai-utils-retry-test-reports-capped-delays ()
  "Capped retry delays are reported (#8826)."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((policy (llm-pi-ai-make-retry-policy :enabled t :max-retries 4
                                               :base-delay-ms 10
                                               :max-agent-delay-ms 15))
          (delays nil)
          (error-message (llm-pi-ai-retry-test--message "error" "terminated"))
          (out (llm-pi-ai-retry-test--run
                (list error-message error-message error-message error-message
                      (llm-pi-ai-retry-test--message nil nil "recovered"))
                policy nil
                (list :on-retry-scheduled
                      (lambda (_a _m delay _e) (push delay delays))))))
     (llm-pi-ai-retry-test--drain)
     (should (equal (nreverse delays) '(10 15 15 15)))
     (should (eq (car (car (car out))) :ok)))))

(ert-deftest llm-pi-ai-utils-retry-test-stops-once-succeeded ()
  "Retrying stops once a call succeeds."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((finished nil)
          (error-message (llm-pi-ai-retry-test--message "error" "terminated"))
          (out (llm-pi-ai-retry-test--run
                (list error-message error-message
                      (llm-pi-ai-retry-test--message nil nil "recovered"))
                llm-pi-ai-retry-test--enabled nil
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (llm-pi-ai-retry-test--drain)
     (should (equal (llm-pi-ai-text-content-text
                     (car (llm-pi-ai-message-content (cadr (car (car out))))))
                    "recovered"))
     (should (= (car (cadr out)) 3))
     (should (equal finished '((t 2)))))))

(ert-deftest llm-pi-ai-utils-retry-test-aborted-retried-call-unsuccessful ()
  "An aborted retried call is reported as unsuccessful."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((finished nil)
          (out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message "error" "terminated")
                      (llm-pi-ai-retry-test--message "aborted"))
                llm-pi-ai-retry-test--enabled nil
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (llm-pi-ai-retry-test--drain)
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (= (car (cadr out)) 2))
     (should (equal finished '((nil 1)))))))

(ert-deftest llm-pi-ai-utils-retry-test-disabled-policy ()
  "With the policy disabled the first response is returned as it is."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message "error" "terminated"))
                (llm-pi-ai-make-retry-policy :enabled nil :max-retries 3)
                nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))
                      :on-retry-finished (lambda (&rest _) (setq finished t))))))
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled)
     (should-not finished))
   (let ((out (llm-pi-ai-retry-test--run
               (list (llm-pi-ai-retry-test--message "error" "terminated"))
               nil)))
     (should (= (car (cadr out)) 1)))))

(ert-deftest llm-pi-ai-utils-retry-test-attempt-start-after-backoff ()
  "on-retry-attempt-start comes after the backoff, before each retried call."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((events nil)
          (n 0)
          (calls 0)
          (result nil))
     (llm-pi-ai-retry-assistant-call
      (lambda (done)
        (push (format "produce:%d" n) events)
        (setq n (1+ n) calls (1+ calls))
        (funcall done
                 (list :ok (if (< n 3)
                               (llm-pi-ai-retry-test--message "error" "terminated")
                             (llm-pi-ai-retry-test--message nil nil "recovered")))))
      llm-pi-ai-retry-test--enabled nil
      (list :on-retry-scheduled (lambda (attempt &rest _)
                                  (push (format "retry:%d" attempt) events))
            :on-retry-attempt-start (lambda () (push "attempt-start" events)))
      (lambda (r) (setq result r)))
     (llm-pi-ai-retry-test--drain)
     (should (equal (llm-pi-ai-text-content-text
                     (car (llm-pi-ai-message-content (cadr result))))
                    "recovered"))
     (should (equal (nreverse events)
                    '("produce:0" "retry:1" "attempt-start" "produce:1"
                      "retry:2" "attempt-start" "produce:2"))))))

(ert-deftest llm-pi-ai-utils-retry-test-abort-during-backoff ()
  "Aborting the backoff stops the timer and returns an aborted message."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((signal (llm-pi-ai-make-abort-signal))
          (finished nil)
          (policy (llm-pi-ai-make-retry-policy :enabled t :max-retries 5
                                               :base-delay-ms 10000))
          (out (llm-pi-ai-retry-test--run
                (list (llm-pi-ai-retry-test--message "error" "terminated"))
                policy signal
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     ;; one produce call, one pending timer of 10 s
     (should (= (car (cadr out)) 1))
     (should (equal (mapcar #'cadr llm-pi-ai-retry-test--timers) '(10000)))
     (should (null (car (car out))))
     (llm-pi-ai-abort-signal-abort signal)
     (let ((message (cadr (car (car out)))))
       (should (eq (car (car (car out))) :ok))
       (should (equal (llm-pi-ai-assistant-message-stop-reason message) "aborted"))
       (should (null (llm-pi-ai-assistant-message-error-message message))))
     (should (= (car (cadr out)) 1))
     (should (null llm-pi-ai-retry-test--timers))
     (should (equal llm-pi-ai-retry-test--cancelled '(1)))
     (should (equal finished '((nil 1 "terminated")))))))

(ert-deftest llm-pi-ai-utils-retry-test-port-already-aborted ()
  "Port: a signal aborted before the backoff ends the call without a timer."
  (llm-pi-ai-retry-test--with-fake-timers
   (let* ((signal (llm-pi-ai-make-abort-signal))
          (out (llm-pi-ai-retry-test--run
                (list (lambda ()
                        (llm-pi-ai-abort-signal-abort signal)
                        (llm-pi-ai-retry-test--message "error" "terminated")))
                llm-pi-ai-retry-test--enabled signal)))
     (should (equal (llm-pi-ai-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (null llm-pi-ai-retry-test--timers)))))

(ert-deftest llm-pi-ai-utils-retry-test-port-errors-reach-finish ()
  "Port: an error of PRODUCE or of a callback is reported, not lost."
  (llm-pi-ai-retry-test--with-fake-timers
   ;; produce reports an error
   (let (result)
     (llm-pi-ai-retry-assistant-call
      (lambda (done) (funcall done (list :error '(:message "boom"))))
      llm-pi-ai-retry-test--enabled nil nil (lambda (r) (setq result r)))
     (should (equal result '(:error (:message "boom")))))
   ;; produce signals
   (let (result)
     (llm-pi-ai-retry-assistant-call
      (lambda (_done) (error "it broke"))
      llm-pi-ai-retry-test--enabled nil nil (lambda (r) (setq result r)))
     (should (eq (car result) :error))
     (should (equal (plist-get (cadr result) :message) "it broke")))
   ;; a callback signals during a timer
   (let (result (n 0))
     (llm-pi-ai-retry-assistant-call
      (lambda (done)
        (setq n (1+ n))
        (funcall done (list :ok (llm-pi-ai-retry-test--message "error" "terminated"))))
      llm-pi-ai-retry-test--enabled nil
      (list :on-retry-attempt-start (lambda () (error "callback broke")))
      (lambda (r) (setq result r)))
     (llm-pi-ai-retry-test--drain)
     (should (= n 1))
     (should (eq (car result) :error))
     (should (equal (plist-get (cadr result) :message) "callback broke")))))

(ert-deftest llm-pi-ai-utils-retry-test-port-timer-delay-in-seconds ()
  "Port: the timer is started with the delay in seconds."
  (let* ((seconds nil)
        (llm-pi-ai-retry-run-at-time-function
         (lambda (s _repeat _function) (setq seconds s) 'timer)))
    (llm-pi-ai-retry-assistant-call
     (lambda (done)
       (funcall done (list :ok (llm-pi-ai-retry-test--message "error" "terminated"))))
     (llm-pi-ai-make-retry-policy :enabled t :max-retries 1 :base-delay-ms 1500)
     nil nil #'ignore)
    (should (= seconds 1.5))))

(provide 'llm-pi-ai-utils-retry-test)
;;; llm-pi-ai-utils-retry-test.el ends here
