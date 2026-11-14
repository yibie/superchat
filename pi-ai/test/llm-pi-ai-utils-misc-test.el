;;; llm-pi-ai-utils-misc-test.el --- Tests for provider-env, model-operations, transcript  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-utils-provider-env.el', `llm-pi-ai-utils-model-operations.el'
;; and the ported part of `llm-pi-ai-utils-transcript.el'.  Upstream has
;; no test files for these three; the cases come from the source.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-utils-provider-env)
(require 'llm-pi-ai-utils-model-operations)
(require 'llm-pi-ai-utils-transcript)

;;; provider-env.ts

(ert-deftest llm-pi-ai-utils-misc-test-provider-env-order ()
  "Scoped env first, then the process environment; empty is unset."
  (let ((process-environment '("PI_T_A=process" "PI_T_EMPTY=")))
    (should (equal (llm-pi-ai-get-provider-env-value "PI_T_A") "process"))
    (should (equal (llm-pi-ai-get-provider-env-value
                    "PI_T_A" '(("PI_T_A" . "scoped")))
                   "scoped"))
    (should (equal (llm-pi-ai-get-provider-env-value
                    "PI_T_A" '(("PI_T_A" . "")))
                   "process"))
    (should-not (llm-pi-ai-get-provider-env-value "PI_T_EMPTY"))
    (should-not (llm-pi-ai-get-provider-env-value "PI_T_NOPE"))
    (should (equal (llm-pi-ai-get-provider-env-value
                    "PI_T_NOPE" '(("PI_T_NOPE" . "x")))
                   "x"))))

;;; model-operations.ts

(ert-deftest llm-pi-ai-utils-misc-test-assert-chat-model ()
  (let ((chat (llm-pi-ai-make-model :id "m" :provider "p"))
        (typed (llm-pi-ai-make-model :id "m" :provider "p" :type 'chat))
        (image (llm-pi-ai-make-image-model :id "i" :provider "p")))
    ;; models without type are chat models
    (should (eq (llm-pi-ai-assert-chat-model chat) chat))
    (should (eq (llm-pi-ai-assert-chat-model typed) typed))
    (let ((err (should-error (llm-pi-ai-assert-chat-model image)
                             :type 'llm-pi-ai-models-error)))
      (should (eq (llm-pi-ai-models-error-code err) 'provider))
      (should (equal (llm-pi-ai-error-message-text err)
                     "Model p/i is not a chat model")))))

;;; transcript.ts

(ert-deftest llm-pi-ai-utils-misc-test-create-initial-system-message ()
  "Nil when prompt and tools are both empty or absent."
  (should-not (llm-pi-ai-create-initial-system-message nil nil))
  (should-not (llm-pi-ai-create-initial-system-message "" nil))
  (let ((message (llm-pi-ai-create-initial-system-message "be brief" nil)))
    (should (llm-pi-ai-system-message-p message))
    (should (equal (llm-pi-ai-message-role message) "system"))
    (should (equal (llm-pi-ai-message-content message) "be brief"))
    (should (equal (llm-pi-ai-message-timestamp message) 0))
    (should-not (llm-pi-ai-system-message-tools-added message)))
  ;; tools alone: the content is the empty string
  (let* ((tool (llm-pi-ai-make-tool :name "t"))
         (message (llm-pi-ai-create-initial-system-message nil (list tool))))
    (should (equal (llm-pi-ai-message-content message) ""))
    (should (equal (llm-pi-ai-system-message-tools-added message) (list tool)))))

(ert-deftest llm-pi-ai-utils-misc-test-normalize-context ()
  "The system prompt and tools move into a leading system message."
  (let* ((user (llm-pi-ai-make-user-message :content "hi" :timestamp 1))
         (context (llm-pi-ai-make-context :system-prompt "sys"
                                          :messages (list user)))
         (transcript (llm-pi-ai-normalize-context context)))
    (should-not (llm-pi-ai-context-system-prompt transcript))
    (should-not (llm-pi-ai-context-tools transcript))
    (should (= (length (llm-pi-ai-context-messages transcript)) 2))
    (should (equal (llm-pi-ai-message-content
                    (car (llm-pi-ai-context-messages transcript)))
                   "sys"))
    (should (eq (cadr (llm-pi-ai-context-messages transcript)) user))
    (should (llm-pi-ai-transcript-context-p transcript)))
  ;; nothing to fold: the messages are as they were
  (let* ((user (llm-pi-ai-make-user-message :content "hi" :timestamp 1))
         (transcript (llm-pi-ai-normalize-context
                      (llm-pi-ai-make-context :messages (list user)))))
    (should (equal (llm-pi-ai-context-messages transcript) (list user)))))

(provide 'llm-pi-ai-utils-misc-test)

;;; llm-pi-ai-utils-misc-test.el ends here
