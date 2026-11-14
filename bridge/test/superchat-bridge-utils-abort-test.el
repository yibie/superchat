;;; superchat-bridge-utils-abort-test.el --- Tests for superchat-bridge-utils-abort  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-abort.el', its port of `src/utils/abort.ts'.
;; Upstream has `test/abort.test.ts' for `AbortSignal' interplay with
;; providers; it needs promises and is not translated.  These test the
;; source's functions from their behaviour.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-abort)

(ert-deftest superchat-bridge-utils-abort-test-signal-starts-unaborted ()
  (let ((signal (superchat-bridge-make-abort-signal)))
    (should-not (superchat-bridge-abort-signal-aborted-p signal))
    (should-not (superchat-bridge-abort-signal-throw-if-aborted signal))))

(ert-deftest superchat-bridge-utils-abort-test-abort-default-reason ()
  "Without a reason, abort gives an AbortError with the DOM message."
  (let ((signal (superchat-bridge-make-abort-signal)))
    (superchat-bridge-abort-signal-abort signal)
    (should (superchat-bridge-abort-signal-aborted-p signal))
    (let ((err (should-error (superchat-bridge-abort-signal-throw-if-aborted signal)
                             :type 'superchat-bridge-abort-error)))
      (should (equal (cadr err) "The operation was aborted")))))

(ert-deftest superchat-bridge-utils-abort-test-abort-custom-reason ()
  "An error object reason is signalled as it is; another value is wrapped."
  (let ((signal (superchat-bridge-make-abort-signal)))
    (superchat-bridge-abort-signal-abort signal '(user-error "stop now"))
    (let ((err (should-error (superchat-bridge-abort-signal-throw-if-aborted signal)
                             :type 'user-error)))
      (should (equal (cadr err) "stop now"))))
  (let ((signal (superchat-bridge-make-abort-signal)))
    (superchat-bridge-abort-signal-abort signal "why")
    (let ((err (should-error (superchat-bridge-abort-signal-throw-if-aborted signal)
                             :type 'superchat-bridge-abort-error)))
      (should (equal (nth 2 err) "why")))))

(ert-deftest superchat-bridge-utils-abort-test-listeners ()
  "Listeners run once, in order, only on the first abort; removed ones do not."
  (let ((signal (superchat-bridge-make-abort-signal))
        (log nil))
    (let ((removed (lambda () (push 'removed log))))
      (superchat-bridge-abort-signal-add-listener signal (lambda () (push 'a log)))
      (superchat-bridge-abort-signal-add-listener signal removed)
      (superchat-bridge-abort-signal-add-listener signal (lambda () (push 'b log)))
      (superchat-bridge-abort-signal-remove-listener signal removed))
    (superchat-bridge-abort-signal-abort signal)
    (superchat-bridge-abort-signal-abort signal "again")
    (should (equal (nreverse log) '(a b)))
    (should (equal (superchat-bridge-abort-reason signal)
                   '(superchat-bridge-abort-error "The operation was aborted")))))

(ert-deftest superchat-bridge-utils-abort-test-operation-signal ()
  (let ((signal (superchat-bridge-make-abort-signal)))
    (should (eq (superchat-bridge-operation-signal signal) signal)))
  (should (superchat-bridge-abort-signal-p (superchat-bridge-operation-signal)))
  (should-not (superchat-bridge-abort-signal-aborted-p (superchat-bridge-operation-signal))))

(ert-deftest superchat-bridge-utils-abort-test-race-returns-value ()
  (should (equal (superchat-bridge-race-with-abort-signal
                  (lambda () 42) (superchat-bridge-make-abort-signal))
                 42)))

(ert-deftest superchat-bridge-utils-abort-test-race-aborted-before-start ()
  "An aborted signal stops the operation from running at all."
  (let ((signal (superchat-bridge-make-abort-signal))
        (ran nil))
    (superchat-bridge-abort-signal-abort signal)
    (should-error (superchat-bridge-race-with-abort-signal
                   (lambda () (setq ran t)) signal)
                  :type 'superchat-bridge-abort-error)
    (should-not ran)))

(ert-deftest superchat-bridge-utils-abort-test-race-abort-wins-over-value-and-error ()
  "A signal aborted during the operation wins over its value or its error."
  (let ((signal (superchat-bridge-make-abort-signal)))
    (should-error (superchat-bridge-race-with-abort-signal
                   (lambda () (superchat-bridge-abort-signal-abort signal) 'late)
                   signal)
                  :type 'superchat-bridge-abort-error))
  (let ((signal (superchat-bridge-make-abort-signal)))
    (should-error (superchat-bridge-race-with-abort-signal
                   (lambda ()
                     (superchat-bridge-abort-signal-abort signal)
                     (error "boom"))
                   signal)
                  :type 'superchat-bridge-abort-error)))

(ert-deftest superchat-bridge-utils-abort-test-race-passes-errors ()
  (let ((err (should-error (superchat-bridge-race-with-abort-signal
                            (lambda () (error "boom"))
                            (superchat-bridge-make-abort-signal)))))
    (should (equal (cadr err) "boom"))))

(provide 'superchat-bridge-utils-abort-test)

;;; superchat-bridge-utils-abort-test.el ends here
