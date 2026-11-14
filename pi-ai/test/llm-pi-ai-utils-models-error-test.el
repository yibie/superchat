;;; llm-pi-ai-utils-models-error-test.el --- Tests for llm-pi-ai-utils-models-error  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-models-error.el', its port of
;; `src/utils/models-error.ts'.  Upstream has no test file for it; the
;; cases come from the source's behaviour (`withCauseDetail', the code).

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-models-error)

(ert-deftest llm-pi-ai-utils-models-error-test-signal-and-read ()
  (condition-case err
      (llm-pi-ai-signal-models-error 'provider "no such provider")
    (llm-pi-ai-models-error
     (should (llm-pi-ai-models-error-p err))
     (should (eq (llm-pi-ai-models-error-code err) 'provider))
     (should (equal (llm-pi-ai-error-message-text err) "no such provider"))
     (should-not (llm-pi-ai-models-error-cause err)))))

(ert-deftest llm-pi-ai-utils-models-error-test-cause-detail-in-message ()
  "The cause's text is kept in the message: callers surface only it."
  (let ((err (llm-pi-ai-models-error
              'auth "API key auth failed for provider p1" '(error "nope"))))
    (should (equal (llm-pi-ai-error-message-text err)
                   "API key auth failed for provider p1: nope"))
    (should (equal (llm-pi-ai-models-error-cause err) '(error "nope")))
    (should (eq (llm-pi-ai-models-error-code err) 'auth))))

(ert-deftest llm-pi-ai-utils-models-error-test-cause-detail-not-repeated ()
  "A cause whose text is already in the message, or empty, adds nothing."
  (should (equal (llm-pi-ai-error-message-text
                  (llm-pi-ai-models-error 'oauth "refresh failed: nope"
                                          '(error "nope")))
                 "refresh failed: nope"))
  (should (equal (llm-pi-ai-error-message-text
                  (llm-pi-ai-models-error 'oauth "refresh failed" '(error "  ")))
                 "refresh failed")))

(ert-deftest llm-pi-ai-utils-models-error-test-error-message-text ()
  "Plain errors give their message, as `error.message'; other values print."
  (should (equal (llm-pi-ai-error-message-text '(error "boom")) "boom"))
  (should (equal (llm-pi-ai-error-message-text "text") "text"))
  (should (equal (llm-pi-ai-error-message-text 42) "42"))
  (should-not (llm-pi-ai-models-error-p '(error "boom"))))

(ert-deftest llm-pi-ai-utils-models-error-test-is-an-error ()
  "A ModelsError is caught by `error' handlers."
  (should (condition-case nil
              (llm-pi-ai-signal-models-error 'stream "x")
            (error t))))

(provide 'llm-pi-ai-utils-models-error-test)

;;; llm-pi-ai-utils-models-error-test.el ends here
