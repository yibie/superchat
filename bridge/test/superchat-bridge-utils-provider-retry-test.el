;;; superchat-bridge-utils-provider-retry-test.el --- Tests for superchat-bridge-utils-provider-retry  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-provider-retry.el', translated from
;; upstream `test/provider-retry.test.ts', in order.  The source's
;; fake timers (`vi.useFakeTimers', `advanceTimersByTimeAsync') are a list
;; of pending (ID DELAY-MS FUNCTION) timers that the test fires by hand:
;; `superchat-bridge-retry-run-at-time-function' is bound to a function that
;; records and does not wait.  The cases marked "port" test what
;; the Lisp shapes add: the result plists, a Lisp error in the request,
;; retry-after parsing, jitter.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-provider-retry)

(defvar superchat-bridge-pretry-test--timers nil
  "Pending fake timers, newest first: (ID DELAY-MS FUNCTION).")

(defvar superchat-bridge-pretry-test--cancelled nil)

(defmacro superchat-bridge-pretry-test--with-fake-timers (&rest body)
  "Run BODY with the retry timer a hand-run list."
  `(let* ((superchat-bridge-pretry-test--timers nil)
          (superchat-bridge-pretry-test--cancelled nil)
          (counter 0)
          (superchat-bridge-retry-run-at-time-function
           (lambda (seconds _repeat function)
             (setq counter (1+ counter))
             (push (list counter (round (* seconds 1000)) function)
                   superchat-bridge-pretry-test--timers)
             counter))
          (superchat-bridge-retry-cancel-timer-function
           (lambda (id)
             (push id superchat-bridge-pretry-test--cancelled)
             (setq superchat-bridge-pretry-test--timers
                   (seq-remove (lambda (timer) (eql (car timer) id))
                               superchat-bridge-pretry-test--timers)))))
     ,@body))

(defun superchat-bridge-pretry-test--fire-oldest ()
  "Run the oldest fake timer, and return its delay in ms."
  (let ((timer (car (last superchat-bridge-pretry-test--timers))))
    (setq superchat-bridge-pretry-test--timers (butlast superchat-bridge-pretry-test--timers))
    (funcall (nth 2 timer))
    (nth 1 timer)))

(defun superchat-bridge-pretry-test--error (status &optional headers)
  "A provider error with STATUS and HEADERS, an alist."
  (list :message (format "Provider error: %s" status)
        :status status :headers headers))

(defun superchat-bridge-pretry-test--run (outcomes &rest options)
  "Run a provider request whose calls end with OUTCOMES, in order.
Each is a result plist, (:ok V) or (:error E); the last one repeats.
OPTIONS go to `superchat-bridge-retry-provider-request'.  Return
\(RESULT-CELL CALLS-CELL): the cells hold what FINISH got, and the number
of request calls."
  (let ((calls (list 0)) (result (list nil)))
    (apply #'superchat-bridge-retry-provider-request
           (lambda (done)
             (let ((outcome (nth (min (car calls) (1- (length outcomes)))
                                 outcomes)))
               (setcar calls (1+ (car calls)))
               (funcall done outcome)))
           (lambda (r) (setcar result r))
           options)
    (list result calls)))

(ert-deftest superchat-bridge-utils-provider-retry-test-retries-retryable ()
  "Retryable provider errors are retried, after the server's delay."
  (superchat-bridge-pretry-test--with-fake-timers
   (let ((out (superchat-bridge-pretry-test--run
               (list (list :error (superchat-bridge-pretry-test--error
                                   429 '(("retry-after-ms" . "1000")))
                           )
                     '(:ok "ok"))
               :max-retries 1)))
     (should (= (car (cadr out)) 1))
     (should (equal (mapcar #'cadr superchat-bridge-pretry-test--timers) '(1000)))
     (should (null (car (car out))))
     (superchat-bridge-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok")))
     (should (= (car (cadr out)) 2)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-x-should-retry-false ()
  "Errors the provider marks as non-retryable are not retried."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((error (superchat-bridge-pretry-test--error 429 '(("x-should-retry" . "false"))))
          (out (superchat-bridge-pretry-test--run (list (list :error error))
                                           :max-retries 2)))
     (should (eq (cadr (car (car out))) error))
     (should (= (car (cadr out)) 1))
     (should (null superchat-bridge-pretry-test--timers)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-no-retry-statuses ()
  "Statuses in no-retry-statuses are not retried."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((error (superchat-bridge-pretry-test--error 504 '(("retry-after-ms" . "0"))))
          (out (superchat-bridge-pretry-test--run (list (list :error error))
                                           :max-retries 2
                                           :no-retry-statuses '(504))))
     (should (eq (cadr (car (car out))) error))
     (should (= (car (cadr out)) 1)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-delay-above-limit ()
  "A provider-requested retry delay above the limit fails at once."
  (superchat-bridge-pretry-test--with-fake-timers
   (let ((out (superchat-bridge-pretry-test--run
               (list (list :error (superchat-bridge-pretry-test--error
                                   429 '(("retry-after" . "277403")))))
               :max-retries 1 :max-retry-delay-ms 1000)))
     (should (eq (car (car (car out))) :error))
     (should (string-prefix-p "Server requested 277403s retry delay (max: 1s)"
                              (plist-get (cadr (car (car out))) :message)))
     (should (= (car (cadr out)) 1))
     (should (null superchat-bridge-pretry-test--timers)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-delay-cap-disabled ()
  "The provider-requested delay cap can be disabled."
  (superchat-bridge-pretry-test--with-fake-timers
   (let ((out (superchat-bridge-pretry-test--run
               (list (list :error (superchat-bridge-pretry-test--error
                                   429 '(("retry-after" . "2")))
                           )
                     '(:ok "ok"))
               :max-retries 1 :max-retry-delay-ms 0)))
     (should (equal (mapcar #'cadr superchat-bridge-pretry-test--timers) '(2000)))
     (superchat-bridge-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok")))
     (should (= (car (cadr out)) 2)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-abort-delay ()
  "An abort ends a provider-requested retry delay, and stops its timer."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((signal (superchat-bridge-make-abort-signal))
          (out (superchat-bridge-pretry-test--run
                (list (list :error (superchat-bridge-pretry-test--error
                                    429 '(("retry-after" . "277403"))))
                      )
                :max-retries 2 :max-retry-delay-ms 0 :signal signal)))
     (should (= (car (cadr out)) 1))
     (should (= (length superchat-bridge-pretry-test--timers) 1))
     (superchat-bridge-abort-signal-abort signal)
     (should (eq (car (car (car out))) :error))
     (should (superchat-bridge-abort-error-p (cadr (car (car out)))))
     (should (equal (plist-get (cadr (car (car out))) :name) "AbortError"))
     (should (= (car (cadr out)) 1))
     (should (null superchat-bridge-pretry-test--timers)))))

;;; Port

(ert-deftest superchat-bridge-utils-provider-retry-test-port-default-no-retry ()
  "Port: without max-retries the first error is final, an ok is returned."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((error (superchat-bridge-pretry-test--error 500))
          (out (superchat-bridge-pretry-test--run (list (list :error error)))))
     (should (equal (car (car out)) (list :error error)))
     (should (= (car (cadr out)) 1)))
   (let ((out (superchat-bridge-pretry-test--run '((:ok 42)))))
     (should (equal (car (car out)) '(:ok 42))))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-not-a-provider-error ()
  "Port: an error without :status and :headers keys is final."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((error '(:message "TypeError: x"))
          (out (superchat-bridge-pretry-test--run (list (list :error error))
                                           :max-retries 3)))
     (should (equal (car (car out)) (list :error error)))
     (should (= (car (cadr out)) 1)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-which-statuses ()
  "Port: the statuses that are retried, as the SDK policy says."
  (dolist (case '((408 t) (409 t) (429 t) (500 t) (503 t) (400 nil) (401 nil)
                  (404 nil) (422 nil) (nil t)))
    (superchat-bridge-pretry-test--with-fake-timers
     (let ((out (superchat-bridge-pretry-test--run
                 (list (list :error (superchat-bridge-pretry-test--error (car case)))
                       '(:ok "ok"))
                 :max-retries 1)))
       (should (eq (and superchat-bridge-pretry-test--timers t) (cadr case)))
       (should (= (car (cadr out)) 1))))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-x-should-retry-true ()
  "Port: x-should-retry true retries even a 400; the header name's case is free."
  (superchat-bridge-pretry-test--with-fake-timers
   (let ((out (superchat-bridge-pretry-test--run
               (list (list :error (superchat-bridge-pretry-test--error
                                   400 '(("X-Should-Retry" . "true"))))
                     '(:ok "ok"))
               :max-retries 1)))
     (should (= (length superchat-bridge-pretry-test--timers) 1))
     (superchat-bridge-pretry-test--fire-oldest)
     (should (equal (car (car out)) '(:ok "ok"))))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-exhausted-and-count ()
  "Port: after max-retries retries the last error is returned."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((superchat-bridge-retry-random-function (lambda () 0.0))
          (error (superchat-bridge-pretry-test--error 500))
          (out (superchat-bridge-pretry-test--run (list (list :error error))
                                           :max-retries 3))
          (delays nil))
     (while superchat-bridge-pretry-test--timers
       (push (superchat-bridge-pretry-test--fire-oldest) delays))
     (should (= (car (cadr out)) 4))
     (should (equal (car (car out)) (list :error error)))
     ;; no header: 0.5 s doubled each retry, no jitter
     (should (equal (nreverse delays) '(500 1000 2000))))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-exponential-and-jitter ()
  "Port: the exponential delay is capped at 8 s, less up to 25 per cent."
  (let ((delay (lambda (retry-index random)
                 (let ((superchat-bridge-retry-random-function (lambda () random)))
                   (superchat-bridge--retry-delay-ms
                    (superchat-bridge-pretry-test--error 500) retry-index nil)))))
    (should (= (funcall delay 0 0.0) 500.0))
    (should (= (funcall delay 3 0.0) 4000.0))
    (should (= (funcall delay 10 0.0) 8000.0))
    (should (= (funcall delay 4 0.5) (* 8000 0.875)))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-retry-after-forms ()
  "Port: retry-after-ms, retry-after in seconds, and as an HTTP date (node 22)."
  (let ((delay (lambda (headers &optional max)
                 (superchat-bridge--retry-delay-ms
                  (superchat-bridge-pretry-test--error 429 headers) 0 max))))
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
    (let ((superchat-bridge-retry-random-function (lambda () 0.0)))
      (should (= (funcall delay '(("retry-after" . "garbage"))) 500.0)))
    ;; a date in the past gives a negative delay, which is not capped
    ;; Date.parse("Wed, 21 Oct 2015 07:28:00 GMT") = 1445412480000
    (should (= (superchat-bridge--parse-date-ms "Wed, 21 Oct 2015 07:28:00 GMT")
               1445412480000.0))
    (should (< (funcall delay '(("retry-after" . "Wed, 21 Oct 2015 07:28:00 GMT")))
               0))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-lisp-error-in-request ()
  "Port: a Lisp error in the request is reported to FINISH."
  (let (result)
    (superchat-bridge-retry-provider-request
     (lambda (_done) (error "it broke"))
     (lambda (r) (setq result r))
     :max-retries 2)
    (should (eq (car result) :error))
    (should (equal (plist-get (cadr result) :message) "it broke"))))

(ert-deftest superchat-bridge-utils-provider-retry-test-port-abort-during-request ()
  "Port: a signal aborted while the request ran gives the abort error."
  (superchat-bridge-pretry-test--with-fake-timers
   (let* ((signal (superchat-bridge-make-abort-signal))
          (result nil))
     (superchat-bridge-retry-provider-request
      (lambda (done)
        (superchat-bridge-abort-signal-abort signal)
        (funcall done (list :error (superchat-bridge-pretry-test--error 500))))
      (lambda (r) (setq result r))
      :max-retries 2 :signal signal)
     (should (superchat-bridge-abort-error-p (cadr result)))
     (should (null superchat-bridge-pretry-test--timers)))))

(provide 'superchat-bridge-utils-provider-retry-test)
;;; superchat-bridge-utils-provider-retry-test.el ends here
