;;; superchat-bridge-utils-provider-retry.el --- Retry of a provider request, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/provider-retry.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `superchat-bridge-retry-provider-request'   the source's `retryProviderRequest'
;;   `superchat-bridge-abort-error-p'            true for the error an abort makes
;;
;; The source reproduces the retry policy of the OpenAI and Anthropic
;; SDKs around a request, with a backoff sleep that an abort can end.
;; It `await's the request and sleeps; here it is callbacks and a
;; timer.  The timer and the abort signal are those of
;; `superchat-bridge-utils-retry' (`superchat-bridge-retry-run-at-time-function',
;; `superchat-bridge-abort-signal' of `superchat-bridge-utils-abort'); read the commentary
;; there.
;;
;; The request
;; -----------
;;
;; REQUEST is a function of one argument, DONE.  It starts one fresh
;; request and, when that ends, calls DONE once with
;;
;;   (:ok VALUE)       the request's value, which is the final
;;                     result: no retry; or
;;   (:error ERROR)    the request failed.
;;
;; ERROR is a plist with a `:message'.  It is a provider error, which
;; may be retried, when it has the keys `:status' (an integer or nil)
;; and `:headers' (an alist of (NAME . VALUE), names in lower case as
;; the HTTP layer gives them, or nil), both present even if nil;
;; the source's `isProviderError' asks the same of an error object.
;; Any other error is final at once.  A failure with no HTTP response
;; (the HTTP layer's `(:error KIND :message S)': `network', `timeout')
;; is a provider error whose `:status' and `:headers' are nil, which is
;; retried, as the SDKs retry a connection error.  A response with
;; status 400 or more is the caller's to turn into a provider error
;; (:message, :status, :headers) before calling DONE.  Other keys of
;; ERROR are the caller's and are passed through untouched.
;;
;; The result
;; ----------
;;
;; FINISH is called once, with (:ok VALUE) or (:error ERROR).  ERROR is
;; the last error of REQUEST as it came; or, when REQUEST signalled a
;; Lisp error, (:message TEXT :lisp-error CONDITION); or
;; (:message "Server requested Ns retry delay (max: Ms). TEXT") when
;; the provider asked for a wait above the limit; or the abort error,
;; (:message "Request aborted" :name "AbortError"), recognised by
;; `superchat-bridge-abort-error-p', when SIGNAL was aborted.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'superchat-bridge-utils-retry)
(require 'superchat-bridge-utils-abort)
(require 'superchat-bridge-utils-sanitize-unicode)

(defconst superchat-bridge--default-max-retry-delay-ms 60000
  "`DEFAULT_MAX_RETRY_DELAY_MS' of the source.")

(defvar superchat-bridge-retry-random-function (lambda () (/ (random 1000000) 1000000.0))
  "Function of no argument giving a number in [0, 1): `Math.random'.
Tests bind it to get a delay they can predict.")

(define-error 'superchat-bridge-retry-delay-error
  "Provider retry delay above the limit")

(defun superchat-bridge--create-abort-error ()
  "Return the error of an aborted request: the source's `createAbortError'."
  (list :message "Request aborted" :name "AbortError"))

(defun superchat-bridge-abort-error-p (error)
  "Return non-nil when ERROR is the error that an abort makes."
  (and (consp error) (keywordp (car error))
       (equal (plist-get error :name) "AbortError")))

(defun superchat-bridge--provider-error-p (error)
  "Return non-nil when ERROR is a provider error, see the file commentary.
The source's `isProviderError'."
  (and (consp error) (keywordp (car error))
       (plist-member error :message)
       (plist-member error :status)
       (plist-member error :headers)
       (let ((status (plist-get error :status)))
         (or (null status) (numberp status)))))

(defun superchat-bridge--header (headers name)
  "Return the value of header NAME in HEADERS, an alist, or nil.
`Headers.get': names are compared without regard to case."
  (cdr (assoc-string name headers t)))

(defun superchat-bridge--retryable-provider-error-p (error)
  "Return non-nil when provider ERROR is worth a retry.
The source's `isRetryableProviderError': it mirrors the retry policy of
the pinned OpenAI and Anthropic SDKs, so review it when either one is
upgraded.  The `x-should-retry' header decides when present; else a
missing status (no response) and the statuses 408, 409, 429 and 500 and
above are retried."
  (let ((should-retry (superchat-bridge--header (plist-get error :headers)
                                         "x-should-retry"))
        (status (plist-get error :status)))
    (cond
     ((equal should-retry "true") t)
     ((equal should-retry "false") nil)
     ((null status) t)
     (t (or (memql status '(408 409 429)) (>= status 500))))))

(defun superchat-bridge--parse-float (text)
  "Return the number `parseFloat' reads at the start of TEXT.
nil stands for NaN, and 1.0e+INF or -1.0e+INF for Infinity."
  (let ((trimmed (string-trim-left text superchat-bridge--js-whitespace)))
    (cond
     ((string-match "\\`[+-]?Infinity" trimmed)
      (if (eq (aref trimmed 0) ?-) -1.0e+INF 1.0e+INF))
     ((string-match
       "\\`[+-]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?"
       trimmed)
      (let ((number (string-to-number (match-string 0 trimmed))))
        ;; "5" is 5, not 5.0, in a number written without a fraction.
        number))
     (t nil))))

(defun superchat-bridge--finite-p (number)
  "Return non-nil when NUMBER is a finite number (not nil, NaN, infinite)."
  (and (numberp number)
       (or (integerp number)
           (not (or (isnan number) (= (abs number) 1.0e+INF))))))

(defun superchat-bridge--parse-date-ms (text)
  "Return the time TEXT names, as ms since the epoch, or nil.
`Date.parse', with nil for NaN.  Which date formats are read is
`date-to-time's affair; the HTTP date of a `retry-after' header is."
  (condition-case nil
      (* 1000.0 (float-time (date-to-time text)))
    (error nil)))

(defun superchat-bridge--validate-server-retry-delay-ms (delay-ms max-retry-delay-ms
                                                           provider-error-message)
  "Return DELAY-MS, or signal an error if it exceeds the limit.
The source's `validateServerRetryDelayMs'.  The limit is
MAX-RETRY-DELAY-MS, or 60 seconds when that is nil; 0 or less removes
it.  The error is a `superchat-bridge-retry-delay-error' carrying the message
\"Server requested Ns retry delay (max: Ms). PROVIDER-ERROR-MESSAGE\"."
  (let ((max-delay-ms (or max-retry-delay-ms
                          superchat-bridge--default-max-retry-delay-ms)))
    (when (and (> max-delay-ms 0) (> delay-ms max-delay-ms))
      (signal 'superchat-bridge-retry-delay-error
              (list (format "Server requested %ds retry delay (max: %ds). %s"
                            (ceiling (/ delay-ms 1000.0))
                            (ceiling (/ max-delay-ms 1000.0))
                            provider-error-message))))
    delay-ms))

(defun superchat-bridge--retry-delay-ms (error retry-index max-retry-delay-ms)
  "Return the wait before the retry of provider ERROR, in ms.
The source's `getRetryDelayMs': the `retry-after-ms' header, else
`retry-after' (seconds, or an HTTP date), each held to
MAX-RETRY-DELAY-MS (see `superchat-bridge--validate-server-retry-delay-ms'),
else exponential, 0.5 s doubled for each RETRY-INDEX up to 8 s, less
a random 0 to 25 per cent."
  (let* ((headers (plist-get error :headers))
         (message (plist-get error :message))
         (retry-after-ms (superchat-bridge--header headers "retry-after-ms"))
         (retry-after (superchat-bridge--header headers "retry-after")))
    (or
     (when (and retry-after-ms (not (equal retry-after-ms "")))
       (let ((value (superchat-bridge--parse-float retry-after-ms)))
         (when (superchat-bridge--finite-p value)
           (superchat-bridge--validate-server-retry-delay-ms
            value max-retry-delay-ms message))))
     (when (and retry-after (not (equal retry-after "")))
       (let* ((seconds (superchat-bridge--parse-float retry-after))
              (delay-ms (if (and seconds (not (and (floatp seconds) (isnan seconds))))
                            (* seconds 1000)
                          (let ((date-ms (superchat-bridge--parse-date-ms retry-after)))
                            (and date-ms (- date-ms (* 1000.0 (float-time))))))))
         (when (superchat-bridge--finite-p delay-ms)
           (superchat-bridge--validate-server-retry-delay-ms
            delay-ms max-retry-delay-ms message))))
     (let ((exponential-delay (* (min (* 0.5 (expt 2 retry-index)) 8) 1000)))
       (* exponential-delay
          (- 1 (* (funcall superchat-bridge-retry-random-function) 0.25)))))))

(cl-defun superchat-bridge-retry-provider-request (request finish
                                                    &key (max-retries 0)
                                                    max-retry-delay-ms
                                                    signal no-retry-statuses)
  "Run REQUEST, retrying it as the OpenAI and Anthropic SDKs would.
The source's `retryProviderRequest'.  REQUEST, FINISH and the shapes of
what they pass are in the file commentary.  The SDK is to be used with
its own retries off, and REQUEST wrapped with this.

MAX-RETRIES (default 0) is the most retries.  MAX-RETRY-DELAY-MS: a
delay the provider asks for with `retry-after-ms' or `retry-after',
when above this (60000 if nil), fails at once; 0 removes the limit.
SIGNAL is an `superchat-bridge-abort-signal' or nil: when it is aborted, FINISH
gets the abort error, wherever the request was; a backoff wait ends
at once and its timer is stopped.  NO-RETRY-STATUSES is a list of
HTTP statuses that fail at once although the policy would retry them.

Each retry is a fresh REQUEST call.  Return nil; the result goes to
FINISH."
  (let ((retries-remaining max-retries)
        (finished nil))
    (cl-labels
        ((conclude (result)
           (setq finished t)
           (funcall finish result))
         (guarded (thunk)
           (condition-case err
               (funcall thunk)
             (error
              (if finished
                  (signal (car err) (cdr err))
                (conclude (list :error
                                (list :message (error-message-string err)
                                      :lisp-error err)))))))
         (attempt ()
           (guarded
            (lambda ()
              (funcall request
                       (lambda (result) (guarded (lambda () (respond result))))))))
         (respond (result)
           ;; Each retry is a fresh request.
           (if (eq (car result) :ok)
               (conclude result)
             (let ((error (cadr result)))
               (cond
                ((superchat-bridge--signal-aborted-p signal)
                 (conclude (list :error (superchat-bridge--create-abort-error))))
                ((or (<= retries-remaining 0)
                     (not (superchat-bridge--provider-error-p error))
                     (not (superchat-bridge--retryable-provider-error-p error))
                     (memql (plist-get error :status) no-retry-statuses))
                 (conclude result))
                (t
                 (let* ((retry-index (- max-retries retries-remaining))
                        (delay-ms
                         (condition-case delay-error
                             (superchat-bridge--retry-delay-ms
                              error retry-index max-retry-delay-ms)
                           (superchat-bridge-retry-delay-error
                            (conclude
                             (list :error
                                   (list :message (cadr delay-error))))
                            nil))))
                   (setq retries-remaining (1- retries-remaining))
                   (when delay-ms
                     (sleep-then delay-ms))))))))
         (sleep-then (delay-ms)
           (if (superchat-bridge--signal-aborted-p signal)
               (conclude (list :error (superchat-bridge--create-abort-error)))
             (let ((timer nil)
                   (on-abort nil))
               (setq on-abort
                     (lambda ()
                       (when timer
                         (funcall superchat-bridge-retry-cancel-timer-function timer))
                       (guarded
                        (lambda ()
                          (conclude (list :error
                                          (superchat-bridge--create-abort-error)))))))
               (when signal
                 (superchat-bridge-abort-signal-add-listener signal on-abort))
               (setq timer
                     (superchat-bridge--start-timer
                      delay-ms
                      (lambda ()
                        (when signal
                          (superchat-bridge-abort-signal-remove-listener signal on-abort))
                        (attempt))))))))
      (attempt)
      nil)))

(provide 'superchat-bridge-utils-provider-retry)

;;; superchat-bridge-utils-provider-retry.el ends here
