;;; superchat-bridge-utils-models-error-test.el --- Tests for superchat-bridge-utils-models-error  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-models-error.el', its port of
;; `src/utils/models-error.ts'.  Upstream has no test file for it; the
;; cases come from the source's behaviour (`withCauseDetail', the code).

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-models-error)

(ert-deftest superchat-bridge-utils-models-error-test-signal-and-read ()
  (condition-case err
      (superchat-bridge-signal-models-error 'provider "no such provider")
    (superchat-bridge-models-error
     (should (superchat-bridge-models-error-p err))
     (should (eq (superchat-bridge-models-error-code err) 'provider))
     (should (equal (superchat-bridge-error-message-text err) "no such provider"))
     (should-not (superchat-bridge-models-error-cause err)))))

(ert-deftest superchat-bridge-utils-models-error-test-cause-detail-in-message ()
  "The cause's text is kept in the message: callers surface only it."
  (let ((err (superchat-bridge-models-error
              'auth "API key auth failed for provider p1" '(error "nope"))))
    (should (equal (superchat-bridge-error-message-text err)
                   "API key auth failed for provider p1: nope"))
    (should (equal (superchat-bridge-models-error-cause err) '(error "nope")))
    (should (eq (superchat-bridge-models-error-code err) 'auth))))

(ert-deftest superchat-bridge-utils-models-error-test-cause-detail-not-repeated ()
  "A cause whose text is already in the message, or empty, adds nothing."
  (should (equal (superchat-bridge-error-message-text
                  (superchat-bridge-models-error 'oauth "refresh failed: nope"
                                          '(error "nope")))
                 "refresh failed: nope"))
  (should (equal (superchat-bridge-error-message-text
                  (superchat-bridge-models-error 'oauth "refresh failed" '(error "  ")))
                 "refresh failed")))

(ert-deftest superchat-bridge-utils-models-error-test-error-message-text ()
  "Plain errors give their message, as `error.message'; other values print."
  (should (equal (superchat-bridge-error-message-text '(error "boom")) "boom"))
  (should (equal (superchat-bridge-error-message-text "text") "text"))
  (should (equal (superchat-bridge-error-message-text 42) "42"))
  (should-not (superchat-bridge-models-error-p '(error "boom"))))

(ert-deftest superchat-bridge-utils-models-error-test-is-an-error ()
  "A ModelsError is caught by `error' handlers."
  (should (condition-case nil
              (superchat-bridge-signal-models-error 'stream "x")
            (error t))))

(provide 'superchat-bridge-utils-models-error-test)

;;; superchat-bridge-utils-models-error-test.el ends here
