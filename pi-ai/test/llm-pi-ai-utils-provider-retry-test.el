;;; llm-pi-ai-utils-provider-retry-test.el --- Tests for llm-pi-ai-utils-provider-retry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-provider-retry.el', translated from
;; upstream `test/provider-retry.test.ts', in order.  The source's
;; fake timers (`vi.useFakeTimers', `advanceTimersByTimeAsync') are a list
;; of pending (ID DELAY-MS FUNCTION) timers that the test fires by hand:
;; `llm-pi-ai-retry-run-at-time-function' is bound to a function that
;; records and does not wait.  The cases marked "port" test what
;; the Lisp shapes add: the result plists, a Lisp error in the request,
;; retry-after parsing, jitter.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-provider-retry)

(defvar llm-pi-ai-pretry-test--timers nil
  "Pending fake timers, newest first: (ID DELAY-MS FUNCTION).")

(defvar llm-pi-ai-pretry-test--cancelled nil)

(defmacro llm-pi-ai-pretry-test--with-fake-timers (&rest body)
  "Run BODY with the retry timer a hand-run list."
  `(let* ((llm-pi-ai-pretry-test--timers nil)
          (llm-pi-ai-pretry-test--cancelled nil)
          (counter 0)
          (llm-pi-ai-retry-run-at-time-function
           (lambda (seconds _repeat function)
             (setq counter (1+ counter))
             (push (list counter (round (* seconds 1000)) function)
                   llm-pi-ai-pretry-test--timers)
             counter))
          (llm-pi-ai-retry-cancel-timer-function
           (lambda (id)
             (push id llm-pi-ai-pretry-test--cancelled)
             (setq llm-pi-ai-pretry-test--timers
                   (seq-remove (lambda (timer) (eql (car timer) id))
                               llm-pi-ai-pretry-test--timers)))))
     ,@body))

(defun llm-pi-ai-pretry-test--fire-oldest ()
  "Run the oldest fake timer, and return its delay in ms."
  (let ((timer (car (last llm-pi-ai-pretry-test--timers))))
    (setq llm-pi-ai-pretry-test--timers (butlast llm-pi-ai-pretry-test--timers))
    (funcall (nth 2 timer))
    (nth 1 timer)))

(defun llm-pi-ai-pretry-test--error (status &optional headers)
  "A provider error with STATUS and HEADERS, an alist."
  (list :message (format "Provider error: %s" status)
        :status status :headers headers))

(defun llm-pi-ai-pretry-test--run (outcomes &rest options)
  "Run a provider request whose calls end with OUTCOMES, in order.
Each is a result plist, (:ok V) or (:error E); the last one repeats.
OPTIONS go to `llm-pi-ai-retry-provider-request'.  Return
\(RESULT-CELL CALLS-CELL): the cells hold what FINISH got, and the number
of request calls."
  (let ((calls (list 0)) (result (list nil)))
    (apply #'llm-pi-ai-retry-provider-request
           (lambda (done)
             (let ((outcome (nth (min (car calls) (1- (length outcomes)))
                                 outcomes)))
               (setcar calls (1+ (car calls)))
               (funcall done outcome)))
           (lambda (r) (setcar result r))
           options)
    (list result calls)))

(ert-deftest llm-pi-ai-utils-provider-retry-test-retries-retryable ()
  "Retryable provider errors are retried, after the server's delay."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let ((out (llm-pi-ai-pretry-test--run
               (list (list :error (llm-pi-ai-pretry-test--error
                                   429 '(("retry-after-ms" . "1000")))
                           )
                     '(:ok "ok"))
               :max-retries 1)))
     (should (= (car (cadr out)) 1))
     (should (equal (mapcar #'cadr llm-pi-ai-pretry-test--timers) '(1000)))
     (should (null (car (car out))))
     (llm-pi-ai-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok")))
     (should (= (car (cadr out)) 2)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-x-should-retry-false ()
  "Errors the provider marks as non-retryable are not retried."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((error (llm-pi-ai-pretry-test--error 429 '(("x-should-retry" . "false"))))
          (out (llm-pi-ai-pretry-test--run (list (list :error error))
                                           :max-retries 2)))
     (should (eq (cadr (car (car out))) error))
     (should (= (car (cadr out)) 1))
     (should (null llm-pi-ai-pretry-test--timers)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-no-retry-statuses ()
  "Statuses in no-retry-statuses are not retried."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((error (llm-pi-ai-pretry-test--error 504 '(("retry-after-ms" . "0"))))
          (out (llm-pi-ai-pretry-test--run (list (list :error error))
                                           :max-retries 2
                                           :no-retry-statuses '(504))))
     (should (eq (cadr (car (car out))) error))
     (should (= (car (cadr out)) 1)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-delay-above-limit ()
  "A provider-requested retry delay above the limit fails at once."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let ((out (llm-pi-ai-pretry-test--run
               (list (list :error (llm-pi-ai-pretry-test--error
                                   429 '(("retry-after" . "277403")))))
               :max-retries 1 :max-retry-delay-ms 1000)))
     (should (eq (car (car (car out))) :error))
     (should (string-prefix-p "Server requested 277403s retry delay (max: 1s)"
                              (plist-get (cadr (car (car out))) :message)))
     (should (= (car (cadr out)) 1))
     (should (null llm-pi-ai-pretry-test--timers)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-delay-cap-disabled ()
  "The provider-requested delay cap can be disabled."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let ((out (llm-pi-ai-pretry-test--run
               (list (list :error (llm-pi-ai-pretry-test--error
                                   429 '(("retry-after" . "2")))
                           )
                     '(:ok "ok"))
               :max-retries 1 :max-retry-delay-ms 0)))
     (should (equal (mapcar #'cadr llm-pi-ai-pretry-test--timers) '(2000)))
     (llm-pi-ai-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok")))
     (should (= (car (cadr out)) 2)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-abort-delay ()
  "An abort ends a provider-requested retry delay, and stops its timer."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((signal (llm-pi-ai-make-abort-signal))
          (out (llm-pi-ai-pretry-test--run
                (list (list :error (llm-pi-ai-pretry-test--error
                                    429 '(("retry-after" . "277403"))))
                      )
                :max-retries 2 :max-retry-delay-ms 0 :signal signal)))
     (should (= (car (cadr out)) 1))
     (should (= (length llm-pi-ai-pretry-test--timers) 1))
     (llm-pi-ai-abort-signal-abort signal)
     (should (eq (car (car (car out))) :error))
     (should (llm-pi-ai-abort-error-p (cadr (car (car out)))))
     (should (equal (plist-get (cadr (car (car out))) :name) "AbortError"))
     (should (= (car (cadr out)) 1))
     (should (null llm-pi-ai-pretry-test--timers)))))

;;; Port

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-default-no-retry ()
  "Port: without max-retries the first error is final, an ok is returned."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((error (llm-pi-ai-pretry-test--error 500))
          (out (llm-pi-ai-pretry-test--run (list (list :error error)))))
     (should (equal (car (car out)) (list :error error)))
     (should (= (car (cadr out)) 1)))
   (let ((out (llm-pi-ai-pretry-test--run '((:ok 42)))))
     (should (equal (car (car out)) '(:ok 42))))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-not-a-provider-error ()
  "Port: an error without :status and :headers keys is final."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((error '(:message "TypeError: x"))
          (out (llm-pi-ai-pretry-test--run (list (list :error error))
                                           :max-retries 3)))
     (should (equal (car (car out)) (list :error error)))
     (should (= (car (cadr out)) 1)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-which-statuses ()
  "Port: the statuses that are retried, as the SDK policy says."
  (dolist (case '((408 t) (409 t) (429 t) (500 t) (503 t) (400 nil) (401 nil)
                  (404 nil) (422 nil) (nil t)))
    (llm-pi-ai-pretry-test--with-fake-timers
     (let ((out (llm-pi-ai-pretry-test--run
                 (list (list :error (llm-pi-ai-pretry-test--error (car case)))
                       '(:ok "ok"))
                 :max-retries 1)))
       (should (eq (and llm-pi-ai-pretry-test--timers t) (cadr case)))
       (should (= (car (cadr out)) 1))))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-x-should-retry-true ()
  "Port: x-should-retry true retries even a 400; the header name's case is free."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let ((out (llm-pi-ai-pretry-test--run
               (list (list :error (llm-pi-ai-pretry-test--error
                                   400 '(("X-Should-Retry" . "true"))))
                     '(:ok "ok"))
               :max-retries 1)))
     (should (= (length llm-pi-ai-pretry-test--timers) 1))
     (llm-pi-ai-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok"))))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-exhausted-and-count ()
  "Port: after max-retries retries the last error is returned."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((llm-pi-ai-retry-random-function (lambda () 0.0))
          (error (llm-pi-ai-pretry-test--error 500))
          (out (llm-pi-ai-pretry-test--run (list (list :error error))
                                           :max-retries 3))
          (delays nil))
     (while llm-pi-ai-pretry-test--timers
       (push (llm-pi-ai-pretry-test--fire-oldest) delays))
     (should (= (car (cadr out)) 4))
     (should (equal (car (car out)) (list :error error)))
     ;; no header: 0.5 s doubled each retry, no jitter
     (should (equal (nreverse delays) '(500 1000 2000))))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-exponential-and-jitter ()
  "Port: the exponential delay is capped at 8 s, less up to 25 per cent."
  (let ((delay (lambda (retry-index random)
                 (let ((llm-pi-ai-retry-random-function (lambda () random)))
                   (llm-pi-ai--retry-delay-ms
                    (llm-pi-ai-pretry-test--error 500) retry-index nil)))))
    (should (= (funcall delay 0 0.0) 500.0))
    (should (= (funcall delay 3 0.0) 4000.0))
    (should (= (funcall delay 10 0.0) 8000.0))
    (should (= (funcall delay 4 0.5) (* 8000 0.875)))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-retry-after-forms ()
  "Port: retry-after-ms, retry-after in seconds, and as an HTTP date (node 22)."
  (let ((delay (lambda (headers &optional max)
                 (llm-pi-ai--retry-delay-ms
                  (llm-pi-ai-pretry-test--error 429 headers) 0 max))))
    ;; retry-after-ms wins over retry-after
    (should (= (funcall delay '(("retry-after-ms" . "1500") ("retry-after" . "9")))
               1500))
    ;; parseFloat("5abc") = 5 ; ".5" ; "1e3"
    (should (= (funcall delay '(("retry-after" . "5abc"))) 5000))
    (should (= (funcall delay '(("retry-after" . ".5"))) 500.0))
    (should (= (funcall delay '(("retry-after" . "1e3")) 0) 1000000.0))
    ;; an unusable retry-after-ms falls to retry-after
    (should (= (funcall delay '(("retry-after-ms" . "soon") ("retry-after" . "2")))
               2000))
    ;; Date.parse("garbage") is NaN: exponential delay (500 ms less jitter)
    (let ((llm-pi-ai-retry-random-function (lambda () 0.0)))
      (should (= (funcall delay '(("retry-after" . "garbage"))) 500.0)))
    ;; a date in the past gives a negative delay, which is not capped
    ;; Date.parse("Wed, 21 Oct 2015 07:28:00 GMT") = 1445412480000
    (should (= (llm-pi-ai--parse-date-ms "Wed, 21 Oct 2015 07:28:00 GMT")
               1445412480000.0))
    (should (< (funcall delay '(("retry-after" . "Wed, 21 Oct 2015 07:28:00 GMT")))
               0))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-lisp-error-in-request ()
  "Port: a Lisp error in the request is reported to FINISH."
  (let (result)
    (llm-pi-ai-retry-provider-request
     (lambda (_done) (error "it broke"))
     (lambda (r) (setq result r))
     :max-retries 2)
    (should (eq (car result) :error))
    (should (equal (plist-get (cadr result) :message) "it broke"))))

(ert-deftest llm-pi-ai-utils-provider-retry-test-port-abort-during-request ()
  "Port: a signal aborted while the request ran gives the abort error."
  (llm-pi-ai-pretry-test--with-fake-timers
   (let* ((signal (llm-pi-ai-make-abort-signal))
          (result nil))
     (llm-pi-ai-retry-provider-request
      (lambda (done)
        (llm-pi-ai-abort-signal-abort signal)
        (funcall done (list :error (llm-pi-ai-pretry-test--error 500))))
      (lambda (r) (setq result r))
      :max-retries 2 :signal signal)
     (should (llm-pi-ai-abort-error-p (cadr result)))
     (should (null llm-pi-ai-pretry-test--timers)))))

(provide 'llm-pi-ai-utils-provider-retry-test)
;;; llm-pi-ai-utils-provider-retry-test.el ends here
