;;; superchat-bridge-utils-misc-test.el --- Tests for provider-env, model-operations, transcript  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-utils-provider-env.el', `superchat-bridge-utils-model-operations.el'
;; and the ported part of `superchat-bridge-utils-transcript.el'.  Upstream has
;; no test files for these three; the cases come from the source.

;;; Code:

(require 'ert)
(require 'superchat-bridge-utils-provider-env)
(require 'superchat-bridge-utils-model-operations)
(require 'superchat-bridge-utils-transcript)

;;; provider-env.ts

(ert-deftest superchat-bridge-utils-misc-test-provider-env-order ()
  "Scoped env first, then the process environment; empty is unset."
  (let ((process-environment '("PI_T_A=process" "PI_T_EMPTY=")))
    (should (equal (superchat-bridge-get-provider-env-value "PI_T_A") "process"))
    (should (equal (superchat-bridge-get-provider-env-value
                    "PI_T_A" '(("PI_T_A" . "scoped")))
                   "scoped"))
    (should (equal (superchat-bridge-get-provider-env-value
                    "PI_T_A" '(("PI_T_A" . "")))
                   "process"))
    (should-not (superchat-bridge-get-provider-env-value "PI_T_EMPTY"))
    (should-not (superchat-bridge-get-provider-env-value "PI_T_NOPE"))
    (should (equal (superchat-bridge-get-provider-env-value
                    "PI_T_NOPE" '(("PI_T_NOPE" . "x")))
                   "x"))))

;;; model-operations.ts

(ert-deftest superchat-bridge-utils-misc-test-assert-chat-model ()
  (let ((chat (superchat-bridge-make-model :id "m" :provider "p"))
        (typed (superchat-bridge-make-model :id "m" :provider "p" :type 'chat))
        (image (superchat-bridge-make-image-model :id "i" :provider "p")))
    ;; models without type are chat models
    (should (eq (superchat-bridge-assert-chat-model chat) chat))
    (should (eq (superchat-bridge-assert-chat-model typed) typed))
    (let ((err (should-error (superchat-bridge-assert-chat-model image)
                             :type 'superchat-bridge-models-error)))
      (should (eq (superchat-bridge-models-error-code err) 'provider))
      (should (equal (superchat-bridge-error-message-text err)
                     "Model p/i is not a chat model")))))

;;; transcript.ts

(ert-deftest superchat-bridge-utils-misc-test-create-initial-system-message ()
  "Nil when prompt and tools are both empty or absent."
  (should-not (superchat-bridge-create-initial-system-message nil nil))
  (should-not (superchat-bridge-create-initial-system-message "" nil))
  (let ((message (superchat-bridge-create-initial-system-message "be brief" nil)))
    (should (superchat-bridge-system-message-p message))
    (should (equal (superchat-bridge-message-role message) "system"))
    (should (equal (superchat-bridge-message-content message) "be brief"))
    (should (equal (superchat-bridge-message-timestamp message) 0))
    (should-not (superchat-bridge-system-message-tools-added message)))
  ;; tools alone: the content is the empty string
  (let* ((tool (superchat-bridge-make-tool :name "t"))
         (message (superchat-bridge-create-initial-system-message nil (list tool))))
    (should (equal (superchat-bridge-message-content message) ""))
    (should (equal (superchat-bridge-system-message-tools-added message) (list tool)))))

(ert-deftest superchat-bridge-utils-misc-test-normalize-context ()
  "The system prompt and tools move into a leading system message."
  (let* ((user (superchat-bridge-make-user-message :content "hi" :timestamp 1))
         (context (superchat-bridge-make-context :system-prompt "sys"
                                          :messages (list user)))
         (transcript (superchat-bridge-normalize-context context)))
    (should-not (superchat-bridge-context-system-prompt transcript))
    (should-not (superchat-bridge-context-tools transcript))
    (should (= (length (superchat-bridge-context-messages transcript)) 2))
    (should (equal (superchat-bridge-message-content
                    (car (superchat-bridge-context-messages transcript)))
                   "sys"))
    (should (eq (cadr (superchat-bridge-context-messages transcript)) user))
    (should (superchat-bridge-transcript-context-p transcript)))
  ;; nothing to fold: the messages are as they were
  (let* ((user (superchat-bridge-make-user-message :content "hi" :timestamp 1))
         (transcript (superchat-bridge-normalize-context
                      (superchat-bridge-make-context :messages (list user)))))
    (should (equal (superchat-bridge-context-messages transcript) (list user)))))

(provide 'superchat-bridge-utils-misc-test)

;;; superchat-bridge-utils-misc-test.el ends here
