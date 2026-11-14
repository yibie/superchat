;;; llm-pi-ai-api-transform-messages-test.el --- Tests for transform-messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-api-transform-messages.el'.  Translated from
;; upstream `test/transform-messages-cross-model-thinking.test.ts' (two
;; cases), `test/transform-messages-copilot-openai-to-anthropic.test.ts'
;; (three cases) and `test/lax-message-content.test.ts' (one case),
;; each in the source's order.  The cases marked "port" are written
;; from the behaviour of `src/api/transform-messages.ts': image notes,
;; error and aborted turns, held system messages, and the split of an
;; error tool result.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-api-transform-messages)

(defun llm-pi-ai-tm-test--model (api provider id &optional input)
  "Make a model of API, PROVIDER and ID that takes INPUT (default text, image)."
  (llm-pi-ai-make-model :id id :name id :api api :provider provider
                        :base-url "https://example.invalid"
                        :input (or input '("text" "image"))
                        :context-window 128000 :max-tokens 16000))

(defun llm-pi-ai-tm-test--assistant (api provider model content
                                         &optional stop-reason)
  "Make an assistant message of API, PROVIDER and MODEL with CONTENT."
  (llm-pi-ai-make-assistant-message
   :content content :api api :provider provider :model model
   :usage (llm-pi-ai-make-usage :cost (llm-pi-ai-make-cost))
   :stop-reason (or stop-reason "toolUse") :timestamp 0))

(defun llm-pi-ai-tm-test--user (content)
  (llm-pi-ai-make-user-message :content content :timestamp 0))

(defun llm-pi-ai-tm-test--result (id name text &optional is-error)
  (llm-pi-ai-make-tool-result-message
   :tool-call-id id :tool-name name
   :content (list (llm-pi-ai-make-text-content :text text))
   :is-error is-error :timestamp 0))

(defun llm-pi-ai-tm-test--call (id name &rest args)
  (apply #'llm-pi-ai-make-tool-call :id id :name name :arguments nil args))

(defun llm-pi-ai-tm-test--text (text)
  (llm-pi-ai-make-text-content :text text))

(defun llm-pi-ai-tm-test--thinking (text)
  (llm-pi-ai-make-thinking-content
   :thinking text :thinking-signature "reasoning_content"))

(defun llm-pi-ai-tm-test--roles (messages)
  (mapcar #'llm-pi-ai-message-role messages))

(defun llm-pi-ai-tm-test--normalize-anthropic (id _model _source)
  "ID as anthropic.ts normalises it."
  (let ((clean (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" id)))
    (substring clean 0 (min 64 (length clean)))))

;;; Cross-model thinking

(defconst llm-pi-ai-tm-test--kimi
  (llm-pi-ai-tm-test--model "openai-completions" "sference" "moonshotai/Kimi-K3"))

(ert-deftest llm-pi-ai-api-transform-messages-test-other-models-thinking-dropped ()
  "Another model's readable thinking is not sent as text."
  (let* ((messages
          (list (llm-pi-ai-tm-test--user "Why does curl work but node fail?")
                (llm-pi-ai-tm-test--assistant
                 "openai-completions" "sference" "zai-org/GLM-5.3"
                 (list (llm-pi-ai-tm-test--thinking
                        "The user asks about ECONNRESET. Candidates: proxy, IPv6, TLS.")
                       (llm-pi-ai-tm-test--text "Let me check the TLS versions.")
                       (llm-pi-ai-tm-test--call "call_1" "bash")))
                (llm-pi-ai-tm-test--result "call_1" "bash" "TLSv1.2")))
         (result (llm-pi-ai-transform-messages messages llm-pi-ai-tm-test--kimi))
         (assistant (nth 1 result))
         (content (llm-pi-ai-message-content assistant)))
    (should (= (length content) 2))
    (should (llm-pi-ai-text-content-p (nth 0 content)))
    (should (equal (llm-pi-ai-text-content-text (nth 0 content))
                   "Let me check the TLS versions."))
    (should (llm-pi-ai-tool-call-p (nth 1 content)))
    (should (equal (llm-pi-ai-tool-call-id (nth 1 content)) "call_1"))))

(ert-deftest llm-pi-ai-api-transform-messages-test-alternating-models ()
  "Thinking is kept only from the target model when models alternate."
  (let* ((kimi "moonshotai/Kimi-K3") (glm "zai-org/GLM-5.3")
         (assistant (lambda (model thinking text)
                      (llm-pi-ai-tm-test--assistant
                       "openai-completions" "sference" model
                       (list (llm-pi-ai-tm-test--thinking thinking)
                             (llm-pi-ai-tm-test--text text))
                       "stop")))
         (messages (list (llm-pi-ai-tm-test--user "1")
                         (funcall assistant kimi "kimi 1" "a")
                         (llm-pi-ai-tm-test--user "2")
                         (funcall assistant glm "glm 1" "b")
                         (llm-pi-ai-tm-test--user "3")
                         (funcall assistant kimi "kimi 2" "c")
                         (llm-pi-ai-tm-test--user "4")
                         (funcall assistant glm "glm 2" "d")
                         (llm-pi-ai-tm-test--user "hi")))
         (result (llm-pi-ai-transform-messages messages llm-pi-ai-tm-test--kimi))
         (contents (mapcar (lambda (message)
                             (mapcar (lambda (block)
                                       (if (llm-pi-ai-thinking-content-p block)
                                           (llm-pi-ai-thinking-content-thinking block)
                                         (llm-pi-ai-text-content-text block)))
                                     (llm-pi-ai-message-content message)))
                           (seq-filter #'llm-pi-ai-assistant-message-p result))))
    (should (equal contents '(("kimi 1" "a") ("b") ("kimi 2" "c") ("d"))))))

;;; Copilot, OpenAI to Anthropic

(defconst llm-pi-ai-tm-test--claude
  (llm-pi-ai-tm-test--model "anthropic-messages" "github-copilot"
                            "claude-sonnet-4.6"))

(ert-deftest llm-pi-ai-api-transform-messages-test-thought-signature-removed ()
  "thoughtSignature goes from tool calls when migrating between models."
  (let* ((messages
          (list (llm-pi-ai-tm-test--user "run a command")
                (llm-pi-ai-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (llm-pi-ai-tm-test--call
                        "call_123" "bash"
                        :thought-signature "{\"type\":\"reasoning.encrypted\"}")))
                (llm-pi-ai-tm-test--result "call_123" "bash" "output")))
         (result (llm-pi-ai-transform-messages
                  messages llm-pi-ai-tm-test--claude
                  #'llm-pi-ai-tm-test--normalize-anthropic))
         (tool-call (car (llm-pi-ai-message-content (nth 1 result)))))
    (should (null (llm-pi-ai-tool-call-thought-signature tool-call)))
    ;; the input message is not changed
    (should (llm-pi-ai-tool-call-thought-signature
             (car (llm-pi-ai-message-content (nth 1 messages)))))))

(ert-deftest llm-pi-ai-api-transform-messages-test-trailing-orphaned-call ()
  "Synthetic tool results for trailing orphaned tool calls."
  (let* ((messages
          (list (llm-pi-ai-tm-test--user "read the file")
                (llm-pi-ai-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (llm-pi-ai-tm-test--call "call_123|fc_123" "read")))))
         (result (llm-pi-ai-transform-messages
                  messages llm-pi-ai-tm-test--claude
                  #'llm-pi-ai-tm-test--normalize-anthropic))
         (last-message (car (last result))))
    (should (llm-pi-ai-tool-result-message-p last-message))
    (should (equal (llm-pi-ai-tool-result-message-tool-call-id last-message)
                   "call_123_fc_123"))
    (should (equal (llm-pi-ai-tool-result-message-tool-name last-message) "read"))
    (should (eq (llm-pi-ai-tool-result-message-is-error last-message) t))
    (should (equal (llm-pi-ai-text-content-text
                    (car (llm-pi-ai-message-content last-message)))
                   "No result provided"))))

(ert-deftest llm-pi-ai-api-transform-messages-test-only-missing-results ()
  "Synthetic results only for trailing calls still missing one."
  (let* ((messages
          (list (llm-pi-ai-tm-test--user "run commands")
                (llm-pi-ai-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (llm-pi-ai-tm-test--call "call_1|fc_1" "read")
                       (llm-pi-ai-tm-test--call "call_2|fc_2" "bash")))
                (llm-pi-ai-tm-test--result "call_1|fc_1" "read" "done")))
         (result (llm-pi-ai-transform-messages
                  messages llm-pi-ai-tm-test--claude
                  #'llm-pi-ai-tm-test--normalize-anthropic))
         (synthetic (seq-filter (lambda (message)
                                  (and (llm-pi-ai-tool-result-message-p message)
                                       (llm-pi-ai-tool-result-message-is-error
                                        message)))
                                result)))
    (should (= (length synthetic) 1))
    (should (equal (llm-pi-ai-tool-result-message-tool-call-id (car synthetic))
                   "call_2_fc_2"))
    (should (equal (llm-pi-ai-tool-result-message-tool-name (car synthetic))
                   "bash"))
    ;; the real result follows its call under the normalised id
    (should (equal (llm-pi-ai-tool-result-message-tool-call-id (nth 2 result))
                   "call_1_fc_1"))))

;;; Lax content

(ert-deftest llm-pi-ai-api-transform-messages-test-null-content ()
  "Null or missing content becomes the empty list."
  (let* ((model (llm-pi-ai-tm-test--model "openai-completions" "openai"
                                          "test-model" '("text")))
         (messages (list (llm-pi-ai-make-user-message :content :null :timestamp 0)
                         (llm-pi-ai-tm-test--assistant
                          "openai-completions" "openai" "test-model" :null "stop")
                         (llm-pi-ai-make-tool-result-message
                          :tool-call-id "call_1" :tool-name "web_search"
                          :is-error nil :timestamp 0)))
         (result (llm-pi-ai-transform-messages messages model)))
    (should (= (length result) 3))
    (dolist (message result)
      (should (equal (llm-pi-ai-message-content message) nil)))))

;;; Port

(ert-deftest llm-pi-ai-api-transform-messages-test-port-image-notes ()
  "A model without image input gets notes for runs of images."
  (let* ((text-model (llm-pi-ai-tm-test--model "openai-completions" "p" "m" '("text")))
         (image (llm-pi-ai-make-image-content :data "x" :mime-type "image/png"))
         (messages
          (list (llm-pi-ai-tm-test--user
                 (list (llm-pi-ai-tm-test--text "look") image image
                       (llm-pi-ai-tm-test--text "done")))
                (llm-pi-ai-tm-test--user "a string stays")
                (llm-pi-ai-make-tool-result-message
                 :tool-call-id "c" :tool-name "t" :content (list image)
                 :timestamp 0)))
         (result (llm-pi-ai-transform-messages messages text-model))
         (texts (lambda (message)
                  (mapcar #'llm-pi-ai-text-content-text
                          (llm-pi-ai-message-content message)))))
    (should (equal (funcall texts (nth 0 result))
                   '("look" "(image omitted: model does not support images)" "done")))
    (should (equal (llm-pi-ai-message-content (nth 1 result)) "a string stays"))
    (should (equal (funcall texts (nth 2 result))
                   '("(tool image omitted: model does not support images)")))
    ;; a model with image input keeps them
    (let ((same (llm-pi-ai-transform-messages
                 messages llm-pi-ai-tm-test--kimi)))
      (should (eq (nth 0 same) (nth 0 messages))))))

(ert-deftest llm-pi-ai-api-transform-messages-test-port-error-and-aborted-skipped ()
  "Assistant messages that ended with error or aborted are dropped."
  (let* ((same (lambda (content stop)
                 (llm-pi-ai-tm-test--assistant
                  "openai-completions" "sference" "moonshotai/Kimi-K3"
                  content stop)))
         (messages (list (llm-pi-ai-tm-test--user "a")
                         (funcall same (list (llm-pi-ai-tm-test--call "x" "t")) "error")
                         (llm-pi-ai-tm-test--user "b")
                         (funcall same nil "aborted")
                         (funcall same (list (llm-pi-ai-tm-test--text "ok")) "stop")))
         (result (llm-pi-ai-transform-messages messages llm-pi-ai-tm-test--kimi)))
    ;; the errored turn's call gets no synthetic result either: its
    ;; assistant message is skipped before its calls are tracked
    (should (equal (llm-pi-ai-tm-test--roles result)
                   '("user" "user" "assistant")))))

(ert-deftest llm-pi-ai-api-transform-messages-test-port-held-system-message ()
  "A system message between a call and its result waits for the results."
  (let* ((system (llm-pi-ai-make-system-message :content "later" :timestamp 0))
         (messages
          (list (llm-pi-ai-tm-test--user "go")
                (llm-pi-ai-tm-test--assistant
                 "openai-completions" "sference" "moonshotai/Kimi-K3"
                 (list (llm-pi-ai-tm-test--call "c1" "a")
                       (llm-pi-ai-tm-test--call "c2" "b")))
                system
                (llm-pi-ai-tm-test--result "c1" "a" "r1")
                (llm-pi-ai-tm-test--user "next")))
         (result (llm-pi-ai-transform-messages messages llm-pi-ai-tm-test--kimi)))
    ;; c2 gets a synthetic result when the user turn comes; the system
    ;; message comes after both results.
    (should (equal (llm-pi-ai-tm-test--roles result)
                   '("user" "assistant" "toolResult" "toolResult" "system" "user")))
    (should (equal (llm-pi-ai-tool-result-message-tool-call-id (nth 3 result)) "c2"))
    (should (eq (nth 4 result) system))))

(ert-deftest llm-pi-ai-api-transform-messages-test-port-same-model-keeps-blocks ()
  "The target model's own blocks are kept, signed and redacted thinking too."
  (let* ((signed (llm-pi-ai-make-thinking-content :thinking "" :thinking-signature "sig"))
         (redacted (llm-pi-ai-make-thinking-content :thinking "" :redacted t))
         (empty (llm-pi-ai-make-thinking-content :thinking "  \n "))
         (text (llm-pi-ai-make-text-content :text "t" :text-signature "ts"))
         (messages
          (list (llm-pi-ai-tm-test--assistant
                 "openai-completions" "sference" "moonshotai/Kimi-K3"
                 (list signed redacted empty text) "stop")))
         (result (llm-pi-ai-transform-messages messages llm-pi-ai-tm-test--kimi))
         (content (llm-pi-ai-message-content (car result))))
    (should (equal content (list signed redacted text)))
    ;; another model's text loses its signature
    (let* ((other (llm-pi-ai-tm-test--assistant
                   "openai-completions" "sference" "other" (list text) "stop"))
           (out (car (llm-pi-ai-message-content
                      (car (llm-pi-ai-transform-messages
                            (list other) llm-pi-ai-tm-test--kimi))))))
      (should (equal (llm-pi-ai-text-content-text out) "t"))
      (should (null (llm-pi-ai-text-content-text-signature out))))))

(ert-deftest llm-pi-ai-api-transform-messages-test-port-split-error-result ()
  "The images of an error tool result are split out."
  (let* ((image (llm-pi-ai-make-image-content :data "x" :mime-type "image/png"))
         (message (llm-pi-ai-make-tool-result-message
                   :tool-call-id "call-1" :tool-name "codemode" :is-error t
                   :content (list (llm-pi-ai-tm-test--text "failed") image image)
                   :timestamp 0))
         (split (llm-pi-ai-split-error-tool-result-images message)))
    (should (equal (mapcar #'llm-pi-ai-text-content-text
                           (llm-pi-ai-error-tool-result-split-content split))
                   '("failed" "(image attached after the tool results)")))
    (should (equal (llm-pi-ai-error-tool-result-split-caption split)
                   "Images from the error result of tool call call-1 (codemode):"))
    (should (equal (llm-pi-ai-error-tool-result-split-images split)
                   (list image image)))
    ;; no split when it is no error, or has no image
    (setf (llm-pi-ai-tool-result-message-is-error message) nil)
    (should (null (llm-pi-ai-split-error-tool-result-images message)))
    (should (null (llm-pi-ai-split-error-tool-result-images
                   (llm-pi-ai-tm-test--result "c" "t" "x" t))))))

(provide 'llm-pi-ai-api-transform-messages-test)
;;; llm-pi-ai-api-transform-messages-test.el ends here
