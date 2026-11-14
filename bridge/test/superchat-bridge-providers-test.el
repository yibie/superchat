;;; superchat-bridge-providers-test.el --- Tests for superchat-bridge-providers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-providers.el'.  Upstream has no test for these
;; providers; the literal values asserted are copied from
;; `src/providers/groq.ts' and `moonshotai.ts'.  Requests go through the
;; real openai-completions API to a fake curl; no test touches the network.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'superchat-bridge-providers)
(require 'superchat-bridge-api-openai-completions)
(require 'superchat-bridge-http)
(require 'superchat-bridge-utils-retry)

(defun superchat-bridge-providers-test--ask (provider model-id env)
  "Stream one request to MODEL-ID of PROVIDER under process environment ENV.
Return (CONFIG STDIN MESSAGE): what the fake curl got and the final message.
The fake curl answers a one-chunk stream of the text \"ok\"."
  (let* ((process-environment (append env (list "HOME=/nonexistent")))
         (superchat-bridge-retry-run-at-time-function (lambda (&rest _) nil))
         (superchat-bridge-retry-cancel-timer-function #'ignore)
         (got nil)
         (superchat-bridge-http-start-function
          (lambda (_args config stdin on-output on-exit)
            (setq got (list config stdin))
            (funcall on-output
                     (concat "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n\r\n"
                             "data: {\"id\":\"c\",\"choices\":[{\"index\":0,\"delta\":"
                             "{\"role\":\"assistant\",\"content\":\"ok\"},"
                             "\"finish_reason\":\"stop\"}]}\n\ndata: [DONE]\n\n"))
            (funcall on-exit 0 "")
            #'ignore))
         (models (superchat-bridge-create-models)))
    (superchat-bridge-models-set-provider models provider)
    (let* ((model (superchat-bridge-models-get-model
                   models (superchat-bridge-provider-id provider) model-id))
           (stream (superchat-bridge-models-stream-simple
                    models model
                    (superchat-bridge-make-context
                     :messages (list (superchat-bridge-make-user-message
                                      :content "hi" :timestamp 1))))))
      (superchat-bridge-event-stream-to-list stream)
      (list (car got) (cadr got) (superchat-bridge-event-stream-result stream)))))

(ert-deftest superchat-bridge-providers-test-every-id-builds ()
  (should (= (length (superchat-bridge-provider-ids)) 18))
  (dolist (id (superchat-bridge-provider-ids))
    (let* ((provider (superchat-bridge-provider id))
           (models (funcall (superchat-bridge-provider-get-models provider))))
      (should (equal (superchat-bridge-provider-id provider) id))
      (should models)
      (should (string-prefix-p "https://" (superchat-bridge-provider-base-url provider)))
      (should (superchat-bridge-provider-env-vars provider))
      (dolist (model models)
        (should (equal (superchat-bridge-model-api model) "openai-completions"))
        (should (equal (superchat-bridge-model-provider model) id))))))

(ert-deftest superchat-bridge-providers-test-literal-values-of-the-ts-files ()
  (let ((groq (superchat-bridge-provider "groq"))
        (moon (superchat-bridge-provider "moonshotai-cn")))
    (should (equal (superchat-bridge-provider-name groq) "Groq"))
    (should (equal (superchat-bridge-provider-base-url groq)
                   "https://api.groq.com/openai/v1"))
    (should (equal (superchat-bridge-provider-env-vars groq) '("GROQ_API_KEY")))
    (should (equal (superchat-bridge-api-key-auth-name
                    (superchat-bridge-provider-auth-api-key
                     (superchat-bridge-provider-auth groq)))
                   "Groq API key"))
    (should (equal (superchat-bridge-provider-name moon) "Moonshot AI CN"))
    (should (equal (superchat-bridge-provider-base-url moon) "https://api.moonshot.cn/v1"))
    (should (equal (superchat-bridge-provider-env-vars moon) '("MOONSHOT_API_KEY")))
    (should (equal (superchat-bridge-provider-env-vars (superchat-bridge-provider "huggingface"))
                   '("HF_TOKEN")))))

(ert-deftest superchat-bridge-providers-test-unknown-id-is-an-error ()
  (should-error (superchat-bridge-provider "nope")))

(ert-deftest superchat-bridge-providers-test-request-to-a-table-provider ()
  (pcase-let ((`(,config ,stdin ,message)
               (superchat-bridge-providers-test--ask
                (superchat-bridge-provider "groq") "llama-3.3-70b-versatile"
                '("GROQ_API_KEY=sk-groq"))))
    (should (string-match-p "https://api.groq.com/openai/v1/chat/completions" config))
    (should (string-match-p "Authorization: Bearer sk-groq" config))
    (should (string-match-p "llama-3.3-70b-versatile" stdin))
    (should (equal (superchat-bridge-assistant-message-stop-reason message) "stop"))))

(ert-deftest superchat-bridge-providers-test-keyless-compatible-provider ()
  (pcase-let ((`(,config ,_ ,message)
               (superchat-bridge-providers-test--ask
                (superchat-bridge-openai-compatible-provider
                 :id "lmstudio" :name "LM Studio"
                 :base-url "http://localhost:1234/v1" :models '("qwen"))
                "qwen" nil)))
    (should (string-match-p "http://localhost:1234/v1/chat/completions" config))
    (should (string-match-p "Authorization: Bearer none" config))
    (should (equal (superchat-bridge-assistant-message-stop-reason message) "stop"))))

(ert-deftest superchat-bridge-providers-test-compatible-with-key-env ()
  (let ((make (lambda ()
                (superchat-bridge-openai-compatible-provider
                 :id "gw" :base-url "https://gw.example/v1" :models '("m")
                 :api-key-env '("GW_KEY")))))
    (should (equal (superchat-bridge-provider-env-vars (funcall make)) '("GW_KEY")))
    (pcase-let ((`(,config ,_ ,_)
                 (superchat-bridge-providers-test--ask (funcall make) "m" '("GW_KEY=sk-gw"))))
      (should (string-match-p "Bearer sk-gw" config)))
    (pcase-let ((`(,config ,_ ,message)
                 (superchat-bridge-providers-test--ask (funcall make) "m" nil)))
      (should-not config)
      (should (equal (superchat-bridge-assistant-message-stop-reason message) "error")))))

(ert-deftest superchat-bridge-providers-test-compatible-model-defaults-and-overrides ()
  (let* ((provider (superchat-bridge-openai-compatible-provider
                    :id "x" :name "X" :base-url "http://h/v1"
                    :models '("plain" (:id "big" :context-window 4096
                                           :max-tokens 512 :reasoning t))))
         (models (funcall (superchat-bridge-provider-get-models provider)))
         (plain (car models)) (big (cadr models)))
    (should (equal (superchat-bridge-provider-name provider) "X"))
    (should (equal (mapcar #'superchat-bridge-model-id models) '("plain" "big")))
    (should (equal (list (superchat-bridge-model-context-window plain)
                         (superchat-bridge-model-max-tokens plain)
                         (superchat-bridge-model-reasoning plain)
                         (superchat-bridge-model-input plain)
                         (superchat-bridge-model-base-url plain)
                         (superchat-bridge-model-api plain)
                         (superchat-bridge-model-cost-rates-input
                          (superchat-bridge-model-cost plain)))
                   '(128000 16384 nil ("text") "http://h/v1" "openai-completions" 0)))
    (should (equal (list (superchat-bridge-model-context-window big)
                         (superchat-bridge-model-max-tokens big)
                         (superchat-bridge-model-reasoning big))
                   '(4096 512 t)))))

(provide 'superchat-bridge-providers-test)

;;; superchat-bridge-providers-test.el ends here
