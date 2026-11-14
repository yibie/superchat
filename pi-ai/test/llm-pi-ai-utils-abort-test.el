;;; llm-pi-ai-utils-abort-test.el --- Tests for llm-pi-ai-utils-abort  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-abort.el', its port of `src/utils/abort.ts'.
;; Upstream has `test/abort.test.ts' for `AbortSignal' interplay with
;; providers; it needs promises and is not translated.  These test the
;; source's functions from their behaviour.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-abort)

(ert-deftest llm-pi-ai-utils-abort-test-signal-starts-unaborted ()
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (should-not (llm-pi-ai-abort-signal-aborted-p signal))
    (should-not (llm-pi-ai-abort-signal-throw-if-aborted signal))))

(ert-deftest llm-pi-ai-utils-abort-test-abort-default-reason ()
  "Without a reason, abort gives an AbortError with the DOM message."
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (llm-pi-ai-abort-signal-abort signal)
    (should (llm-pi-ai-abort-signal-aborted-p signal))
    (let ((err (should-error (llm-pi-ai-abort-signal-throw-if-aborted signal)
                             :type 'llm-pi-ai-abort-error)))
      (should (equal (cadr err) "The operation was aborted")))))

(ert-deftest llm-pi-ai-utils-abort-test-abort-custom-reason ()
  "An error object reason is signalled as it is; another value is wrapped."
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (llm-pi-ai-abort-signal-abort signal '(user-error "stop now"))
    (let ((err (should-error (llm-pi-ai-abort-signal-throw-if-aborted signal)
                             :type 'user-error)))
      (should (equal (cadr err) "stop now"))))
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (llm-pi-ai-abort-signal-abort signal "why")
    (let ((err (should-error (llm-pi-ai-abort-signal-throw-if-aborted signal)
                             :type 'llm-pi-ai-abort-error)))
      (should (equal (nth 2 err) "why")))))

(ert-deftest llm-pi-ai-utils-abort-test-listeners ()
  "Listeners run once, in order, only on the first abort; removed ones do not."
  (let ((signal (llm-pi-ai-make-abort-signal))
        (log nil))
    (let ((removed (lambda () (push 'removed log))))
      (llm-pi-ai-abort-signal-add-listener signal (lambda () (push 'a log)))
      (llm-pi-ai-abort-signal-add-listener signal removed)
      (llm-pi-ai-abort-signal-add-listener signal (lambda () (push 'b log)))
      (llm-pi-ai-abort-signal-remove-listener signal removed))
    (llm-pi-ai-abort-signal-abort signal)
    (llm-pi-ai-abort-signal-abort signal "again")
    (should (equal (nreverse log) '(a b)))
    (should (equal (llm-pi-ai-abort-reason signal)
                   '(llm-pi-ai-abort-error "The operation was aborted")))))

(ert-deftest llm-pi-ai-utils-abort-test-operation-signal ()
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (should (eq (llm-pi-ai-operation-signal signal) signal)))
  (should (llm-pi-ai-abort-signal-p (llm-pi-ai-operation-signal)))
  (should-not (llm-pi-ai-abort-signal-aborted-p (llm-pi-ai-operation-signal))))

(ert-deftest llm-pi-ai-utils-abort-test-race-returns-value ()
  (should (equal (llm-pi-ai-race-with-abort-signal
                  (lambda () 42) (llm-pi-ai-make-abort-signal))
                 42)))

(ert-deftest llm-pi-ai-utils-abort-test-race-aborted-before-start ()
  "An aborted signal stops the operation from running at all."
  (let ((signal (llm-pi-ai-make-abort-signal))
        (ran nil))
    (llm-pi-ai-abort-signal-abort signal)
    (should-error (llm-pi-ai-race-with-abort-signal
                   (lambda () (setq ran t)) signal)
                  :type 'llm-pi-ai-abort-error)
    (should-not ran)))

(ert-deftest llm-pi-ai-utils-abort-test-race-abort-wins-over-value-and-error ()
  "A signal aborted during the operation wins over its value or its error."
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (should-error (llm-pi-ai-race-with-abort-signal
                   (lambda () (llm-pi-ai-abort-signal-abort signal) 'late)
                   signal)
                  :type 'llm-pi-ai-abort-error))
  (let ((signal (llm-pi-ai-make-abort-signal)))
    (should-error (llm-pi-ai-race-with-abort-signal
                   (lambda ()
                     (llm-pi-ai-abort-signal-abort signal)
                     (error "boom"))
                   signal)
                  :type 'llm-pi-ai-abort-error)))

(ert-deftest llm-pi-ai-utils-abort-test-race-passes-errors ()
  (let ((err (should-error (llm-pi-ai-race-with-abort-signal
                            (lambda () (error "boom"))
                            (llm-pi-ai-make-abort-signal)))))
    (should (equal (cadr err) "boom"))))

(provide 'llm-pi-ai-utils-abort-test)

;;; llm-pi-ai-utils-abort-test.el ends here
