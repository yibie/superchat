;;; superchat-bridge-utils-retry-test.el --- Tests for superchat-bridge-utils-retry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-retry.el', translated from upstream
;; `test/retry.test.ts' (the describe blocks `provider retry
;; classification', `retryDelayMs' and `retryAssistantCall', in order).
;; `fauxAssistantMessage' is not ported yet; `superchat-bridge-retry-test--message'
;; stands in for it.  No test waits: the timer is replaced by a list of
;; pending (DELAY-MS . FUNCTION) pairs that a test runs by hand, the
;; "fake timers" of the source's tests.  The cases marked "port" test
;; the Lisp shapes (finish results, errors) and the Emacs regexps.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-retry)
(require 'superchat-bridge-utils-abort)

(defvar superchat-bridge-retry-test--timers nil
  "Pending fake timers: a list of (ID DELAY-MS FUNCTION), oldest last.")

(defvar superchat-bridge-retry-test--cancelled nil
  "IDs of fake timers that were cancelled.")

(defun superchat-bridge-retry-test--message (&optional stop-reason error-message text)
  "Make an assistant message: STOP-REASON (default stop), ERROR-MESSAGE, TEXT."
  (superchat-bridge-make-assistant-message
   :content (list (superchat-bridge-make-text-content :text (or text "")))
   :api "faux:api" :provider "faux" :model "faux-1"
   :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
   :stop-reason (or stop-reason "stop") :error-message error-message
   :timestamp 0))

(defmacro superchat-bridge-retry-test--with-fake-timers (&rest body)
  "Run BODY with the retry timer replaced by a hand-run list."
  `(let* ((superchat-bridge-retry-test--timers nil)
         (superchat-bridge-retry-test--cancelled nil)
         (counter 0)
         (superchat-bridge-retry-run-at-time-function
          (lambda (seconds _repeat function)
            (setq counter (1+ counter))
            (push (list counter (round (* seconds 1000)) function)
                  superchat-bridge-retry-test--timers)
            counter))
         (superchat-bridge-retry-cancel-timer-function
          (lambda (id)
            (push id superchat-bridge-retry-test--cancelled)
            (setq superchat-bridge-retry-test--timers
                  (seq-remove (lambda (timer) (eql (car timer) id))
                              superchat-bridge-retry-test--timers)))))
     ,@body))

(defun superchat-bridge-retry-test--fire-oldest ()
  "Run the oldest pending fake timer; return its delay in ms."
  (let ((timer (car (last superchat-bridge-retry-test--timers))))
    (setq superchat-bridge-retry-test--timers (butlast superchat-bridge-retry-test--timers))
    (funcall (nth 2 timer))
    (nth 1 timer)))

(defun superchat-bridge-retry-test--retryable-p (error-message)
  (superchat-bridge-retryable-assistant-error-p
   (superchat-bridge-retry-test--message "error" error-message)))

;;; Classification

(ert-deftest superchat-bridge-utils-retry-test-explicit-retry-guidance ()
  "Explicit provider retry guidance matches."
  (dolist (text '("An error occurred while processing your request. You can retry your request, or contact us through our help center at help.openai.com if the error persists. Please include the request ID req_******** in your message."
                  "{\"message\":\"The system encountered an unexpected error during processing. Try your request again.\"}"
                  "ResourceExhausted: Worker local total request limit reached (288/48)"
                  ;; Bun fetch socket drop wording
                  "The socket connection was closed unexpectedly. For more information, pass `verbose: true` in the second argument to fetch()"
                  "Error: exceeded request buffer limit while retrying upstream"))
    (should (superchat-bridge-retry-test--retryable-p text))))

(ert-deftest superchat-bridge-utils-retry-test-transport-wording ()
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
    (should (superchat-bridge-retry-test--retryable-p text))))

(ert-deftest superchat-bridge-utils-retry-test-limit-errors-not-retryable ()
  "Provider limit errors are not retried."
  (dolist (text '("429 quota exceeded"
                  "OpenAI API error (429): {\"code\":\"subscription_sharing_usage_limit_exceeded\",\"message\":\"Usage limit reached.\"}"
                  "OpenAI API error (429): {\"code\":\"subscription_sharing_workplace_individual_usage_limit_exceeded\",\"message\":\"Usage limit reached.\"}"))
    (should-not (superchat-bridge-retry-test--retryable-p text)))
  (dolist (text '("subscription_sharing_usage_unavailable: Usage cannot be checked."
                  "subscription_sharing_user_unavailable: User cannot be loaded."))
    (should (superchat-bridge-retry-test--retryable-p text))))

(ert-deftest superchat-bridge-utils-retry-test-classifies-error-messages ()
  "Assistant error messages are classified."
  (should (superchat-bridge-retry-test--retryable-p "overloaded_error"))
  (should (superchat-bridge-retry-test--retryable-p "520 status code (no body)"))
  (should (superchat-bridge-retry-test--retryable-p "524 status code (no body)"))
  (should-not (superchat-bridge-retryable-assistant-error-p
               (superchat-bridge-retry-test--message))))

(ert-deftest superchat-bridge-utils-retry-test-status-codes-in-longer-numbers ()
  "Status codes inside ids or longer numbers are not status codes (#11057)."
  (dolist (text '("401 {\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"},\"request_id\":\"req_011CU503xyz\"}"
                  "400 image exceeds 5 MB maximum: 5316784 bytes > 5242880 bytes"))
    (should-not (superchat-bridge-retry-test--retryable-p text))))

(ert-deftest superchat-bridge-utils-retry-test-port-classification-node ()
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
    (should (eq (superchat-bridge-retry-test--retryable-p (car case))
                (cadr case)))))

(ert-deftest superchat-bridge-utils-retry-test-port-not-an-error ()
  "Port: only a message with the stop reason error and a message counts."
  (should-not (superchat-bridge-retryable-assistant-error-p
               (superchat-bridge-retry-test--message "stop" "overloaded")))
  (should-not (superchat-bridge-retryable-assistant-error-p
               (superchat-bridge-retry-test--message "error" nil)))
  (should-not (superchat-bridge-retryable-assistant-error-p
               (superchat-bridge-retry-test--message "error" ""))))

;;; Delay

(ert-deftest superchat-bridge-utils-retry-test-delay-caps ()
  "Agent retry delay is capped (#8826)."
  (should (= (superchat-bridge-retry-delay-ms
              (superchat-bridge-make-retry-policy :base-delay-ms 2000) 6)
             60000))
  (should (= (superchat-bridge-retry-delay-ms
              (superchat-bridge-make-retry-policy :base-delay-ms 2000
                                           :max-agent-delay-ms 5000)
              5)
             5000))
  (should (= (superchat-bridge-retry-delay-ms
              (superchat-bridge-make-retry-policy :base-delay-ms 2000
                                           :max-agent-delay-ms 0)
              5)
             0)))

(ert-deftest superchat-bridge-utils-retry-test-port-delay-node ()
  "Port: delays as the source computes them (node 22)."
  (let ((delay (lambda (base attempt &optional cap)
                 (superchat-bridge-retry-delay-ms
                  (superchat-bridge-make-retry-policy :base-delay-ms base
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

(defun superchat-bridge-retry-test--run (messages policy &optional signal callbacks)
  "Run `superchat-bridge-retry-assistant-call' over MESSAGES, a list produced in turn.
Each element is a message, or a function of no argument that returns
one, called when the call starts.  Return (RESULT CALLS), where
RESULT is what FINISH got (nil if not yet), CALLS the number of
produce calls so far, as a cons cell the caller may read later."
  (let* ((calls (list 0))
         (result (list nil)))
    (superchat-bridge-retry-assistant-call
     (lambda (done)
       (let ((next (nth (car calls) messages)))
         (setcar calls (1+ (car calls)))
         (funcall done (list :ok (if (functionp next) (funcall next) next)))))
     policy signal callbacks
     (lambda (r) (setcar result r)))
    (list result calls)))

(defconst superchat-bridge-retry-test--enabled
  (superchat-bridge-make-retry-policy :enabled t :max-retries 3 :base-delay-ms 0))

(defun superchat-bridge-retry-test--drain ()
  "Fire fake timers until none is left."
  (while superchat-bridge-retry-test--timers
    (superchat-bridge-retry-test--fire-oldest)))

(ert-deftest superchat-bridge-utils-retry-test-success-immediate ()
  "A successful response is returned at once, no retry."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message nil nil "ok"))
                superchat-bridge-retry-test--enabled))
          (result (car (car out))))
     (should (eq (car result) :ok))
     (should (equal (superchat-bridge-text-content-text
                     (car (superchat-bridge-message-content (cadr result))))
                    "ok"))
     (should (= (car (cadr out)) 1))
     (should (null superchat-bridge-retry-test--timers)))))

(ert-deftest superchat-bridge-utils-retry-test-aborted-not-retried ()
  "An aborted message is not retried."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((scheduled nil)
          (out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message "aborted"))
                superchat-bridge-retry-test--enabled nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))))))
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled))))

(ert-deftest superchat-bridge-utils-retry-test-non-retryable-error ()
  "A non-retryable error (quota/billing) is returned at once."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message "error" "insufficient_quota"))
                superchat-bridge-retry-test--enabled nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))
                      :on-retry-finished (lambda (&rest _) (setq finished t))))))
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled)
     (should-not finished))))

(ert-deftest superchat-bridge-utils-retry-test-retries-up-to-max ()
  "A transient error is retried up to max-retries, then the error returns."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (messages (make-list 10 (superchat-bridge-retry-test--message "error" "terminated")))
          (out (superchat-bridge-retry-test--run
                messages superchat-bridge-retry-test--enabled nil
                (list :on-retry-scheduled
                      (lambda (&rest args) (push args scheduled))
                      :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (superchat-bridge-retry-test--drain)
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 4))    ; 1 initial + 3 retries
     (should (= (length scheduled) 3))
     (should (equal finished '((nil 3 "terminated")))))))

(ert-deftest superchat-bridge-utils-retry-test-reports-capped-delays ()
  "Capped retry delays are reported (#8826)."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((policy (superchat-bridge-make-retry-policy :enabled t :max-retries 4
                                               :base-delay-ms 10
                                               :max-agent-delay-ms 15))
          (delays nil)
          (error-message (superchat-bridge-retry-test--message "error" "terminated"))
          (out (superchat-bridge-retry-test--run
                (list error-message error-message error-message error-message
                      (superchat-bridge-retry-test--message nil nil "recovered"))
                policy nil
                (list :on-retry-scheduled
                      (lambda (_a _m delay _e) (push delay delays))))))
     (superchat-bridge-retry-test--drain)
     (should (equal (nreverse delays) '(10 15 15 15)))
     (should (eq (car (car (car out))) :ok)))))

(ert-deftest superchat-bridge-utils-retry-test-stops-once-succeeded ()
  "Retrying stops once a call succeeds."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((finished nil)
          (error-message (superchat-bridge-retry-test--message "error" "terminated"))
          (out (superchat-bridge-retry-test--run
                (list error-message error-message
                      (superchat-bridge-retry-test--message nil nil "recovered"))
                superchat-bridge-retry-test--enabled nil
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (superchat-bridge-retry-test--drain)
     (should (equal (superchat-bridge-text-content-text
                     (car (superchat-bridge-message-content (cadr (car (car out))))))
                    "recovered"))
     (should (= (car (cadr out)) 3))
     (should (equal finished '((t 2)))))))

(ert-deftest superchat-bridge-utils-retry-test-aborted-retried-call-unsuccessful ()
  "An aborted retried call is reported as unsuccessful."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((finished nil)
          (out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message "error" "terminated")
                      (superchat-bridge-retry-test--message "aborted"))
                superchat-bridge-retry-test--enabled nil
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     (superchat-bridge-retry-test--drain)
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (= (car (cadr out)) 2))
     (should (equal finished '((nil 1)))))))

(ert-deftest superchat-bridge-utils-retry-test-disabled-policy ()
  "With the policy disabled the first response is returned as it is."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((scheduled nil) (finished nil)
          (out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message "error" "terminated"))
                (superchat-bridge-make-retry-policy :enabled nil :max-retries 3)
                nil
                (list :on-retry-scheduled (lambda (&rest _) (setq scheduled t))
                      :on-retry-finished (lambda (&rest _) (setq finished t))))))
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "error"))
     (should (= (car (cadr out)) 1))
     (should-not scheduled)
     (should-not finished))
   (let ((out (superchat-bridge-retry-test--run
               (list (superchat-bridge-retry-test--message "error" "terminated"))
               nil)))
     (should (= (car (cadr out)) 1)))))

(ert-deftest superchat-bridge-utils-retry-test-attempt-start-after-backoff ()
  "on-retry-attempt-start comes after the backoff, before each retried call."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((events nil)
          (n 0)
          (calls 0)
          (result nil))
     (superchat-bridge-retry-assistant-call
      (lambda (done)
        (push (format "produce:%d" n) events)
        (setq n (1+ n) calls (1+ calls))
        (funcall done
                 (list :ok (if (< n 3)
                               (superchat-bridge-retry-test--message "error" "terminated")
                             (superchat-bridge-retry-test--message nil nil "recovered")))))
      superchat-bridge-retry-test--enabled nil
      (list :on-retry-scheduled (lambda (attempt &rest _)
                                  (push (format "retry:%d" attempt) events))
            :on-retry-attempt-start (lambda () (push "attempt-start" events)))
      (lambda (r) (setq result r)))
     (superchat-bridge-retry-test--drain)
     (should (equal (superchat-bridge-text-content-text
                     (car (superchat-bridge-message-content (cadr result))))
                    "recovered"))
     (should (equal (nreverse events)
                    '("produce:0" "retry:1" "attempt-start" "produce:1"
                      "retry:2" "attempt-start" "produce:2"))))))

(ert-deftest superchat-bridge-utils-retry-test-abort-during-backoff ()
  "Aborting the backoff stops the timer and returns an aborted message."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((signal (superchat-bridge-make-abort-signal))
          (finished nil)
          (policy (superchat-bridge-make-retry-policy :enabled t :max-retries 5
                                               :base-delay-ms 10000))
          (out (superchat-bridge-retry-test--run
                (list (superchat-bridge-retry-test--message "error" "terminated"))
                policy signal
                (list :on-retry-finished
                      (lambda (&rest args) (push args finished))))))
     ;; one produce call, one pending timer of 10 s
     (should (= (car (cadr out)) 1))
     (should (equal (mapcar #'cadr superchat-bridge-retry-test--timers) '(10000)))
     (should (null (car (car out))))
     (superchat-bridge-abort-signal-abort signal)
     (let ((message (cadr (car (car out)))))
       (should (eq (car (car (car out))) :ok))
       (should (equal (superchat-bridge-assistant-message-stop-reason message) "aborted"))
       (should (null (superchat-bridge-assistant-message-error-message message))))
     (should (= (car (cadr out)) 1))
     (should (null superchat-bridge-retry-test--timers))
     (should (equal superchat-bridge-retry-test--cancelled '(1)))
     (should (equal finished '((nil 1 "terminated")))))))

(ert-deftest superchat-bridge-utils-retry-test-port-already-aborted ()
  "Port: a signal aborted before the backoff ends the call without a timer."
  (superchat-bridge-retry-test--with-fake-timers
   (let* ((signal (superchat-bridge-make-abort-signal))
          (out (superchat-bridge-retry-test--run
                (list (lambda ()
                        (superchat-bridge-abort-signal-abort signal)
                        (superchat-bridge-retry-test--message "error" "terminated")))
                superchat-bridge-retry-test--enabled signal)))
     (should (equal (superchat-bridge-assistant-message-stop-reason
                     (cadr (car (car out))))
                    "aborted"))
     (should (null superchat-bridge-retry-test--timers)))))

(ert-deftest superchat-bridge-utils-retry-test-port-errors-reach-finish ()
  "Port: an error of PRODUCE or of a callback is reported, not lost."
  (superchat-bridge-retry-test--with-fake-timers
   ;; produce reports an error
   (let (result)
     (superchat-bridge-retry-assistant-call
      (lambda (done) (funcall done (list :error '(:message "boom"))))
      superchat-bridge-retry-test--enabled nil nil (lambda (r) (setq result r)))
     (should (equal result '(:error (:message "boom")))))
   ;; produce signals
   (let (result)
     (superchat-bridge-retry-assistant-call
      (lambda (_done) (error "it broke"))
      superchat-bridge-retry-test--enabled nil nil (lambda (r) (setq result r)))
     (should (eq (car result) :error))
     (should (equal (plist-get (cadr result) :message) "it broke")))
   ;; a callback signals during a timer
   (let (result (n 0))
     (superchat-bridge-retry-assistant-call
      (lambda (done)
        (setq n (1+ n))
        (funcall done (list :ok (superchat-bridge-retry-test--message "error" "terminated"))))
      superchat-bridge-retry-test--enabled nil
      (list :on-retry-attempt-start (lambda () (error "callback broke")))
      (lambda (r) (setq result r)))
     (superchat-bridge-retry-test--drain)
     (should (= n 1))
     (should (eq (car result) :error))
     (should (equal (plist-get (cadr result) :message) "callback broke")))))

(ert-deftest superchat-bridge-utils-retry-test-port-timer-delay-in-seconds ()
  "Port: the timer is started with the delay in seconds."
  (let* ((seconds nil)
        (superchat-bridge-retry-run-at-time-function
         (lambda (s _repeat _function) (setq seconds s) 'timer)))
    (superchat-bridge-retry-assistant-call
     (lambda (done)
       (funcall done (list :ok (superchat-bridge-retry-test--message "error" "terminated"))))
     (superchat-bridge-make-retry-policy :enabled t :max-retries 1 :base-delay-ms 1500)
     nil nil #'ignore)
    (should (= seconds 1.5))))

(provide 'superchat-bridge-utils-retry-test)
;;; superchat-bridge-utils-retry-test.el ends here
