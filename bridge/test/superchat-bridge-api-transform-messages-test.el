;;; superchat-bridge-api-transform-messages-test.el --- Tests for transform-messages  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-api-transform-messages.el'.  Translated from
;; upstream `test/transform-messages-cross-model-thinking.test.ts' (two
;; cases), `test/transform-messages-copilot-openai-to-anthropic.test.ts'
;; (three cases) and `test/lax-message-content.test.ts' (one case),
;; each in the source's order.  The cases marked "port" are written
;; from the behaviour of `src/api/transform-messages.ts': image notes,
;; error and aborted turns, held system messages, and the split of an
;; error tool result.

;;; Code:

(require 'ert)
(require 'superchat-bridge-api-transform-messages)

(defun superchat-bridge-tm-test--model (api provider id &optional input)
  "Make a model of API, PROVIDER and ID that takes INPUT (default text, image)."
  (superchat-bridge-make-model :id id :name id :api api :provider provider
                        :base-url "https://example.invalid"
                        :input (or input '("text" "image"))
                        :context-window 128000 :max-tokens 16000))

(defun superchat-bridge-tm-test--assistant (api provider model content
                                         &optional stop-reason)
  "Make an assistant message of API, PROVIDER and MODEL with CONTENT."
  (superchat-bridge-make-assistant-message
   :content content :api api :provider provider :model model
   :usage (superchat-bridge-make-usage :cost (superchat-bridge-make-cost))
   :stop-reason (or stop-reason "toolUse") :timestamp 0))

(defun superchat-bridge-tm-test--user (content)
  (superchat-bridge-make-user-message :content content :timestamp 0))

(defun superchat-bridge-tm-test--result (id name text &optional is-error)
  (superchat-bridge-make-tool-result-message
   :tool-call-id id :tool-name name
   :content (list (superchat-bridge-make-text-content :text text))
   :is-error is-error :timestamp 0))

(defun superchat-bridge-tm-test--call (id name &rest args)
  (apply #'superchat-bridge-make-tool-call :id id :name name :arguments nil args))

(defun superchat-bridge-tm-test--text (text)
  (superchat-bridge-make-text-content :text text))

(defun superchat-bridge-tm-test--thinking (text)
  (superchat-bridge-make-thinking-content
   :thinking text :thinking-signature "reasoning_content"))

(defun superchat-bridge-tm-test--roles (messages)
  (mapcar #'superchat-bridge-message-role messages))

(defun superchat-bridge-tm-test--normalize-anthropic (id _model _source)
  "ID as anthropic.ts normalises it."
  (let ((clean (replace-regexp-in-string "[^a-zA-Z0-9_-]" "_" id)))
    (substring clean 0 (min 64 (length clean)))))

;;; Cross-model thinking

(defconst superchat-bridge-tm-test--kimi
  (superchat-bridge-tm-test--model "openai-completions" "sference" "moonshotai/Kimi-K3"))

(ert-deftest superchat-bridge-api-transform-messages-test-other-models-thinking-dropped ()
  "Another model's readable thinking is not sent as text."
  (let* ((messages
          (list (superchat-bridge-tm-test--user "Why does curl work but node fail?")
                (superchat-bridge-tm-test--assistant
                 "openai-completions" "sference" "zai-org/GLM-5.3"
                 (list (superchat-bridge-tm-test--thinking
                        "The user asks about ECONNRESET. Candidates: proxy, IPv6, TLS.")
                       (superchat-bridge-tm-test--text "Let me check the TLS versions.")
                       (superchat-bridge-tm-test--call "call_1" "bash")))
                (superchat-bridge-tm-test--result "call_1" "bash" "TLSv1.2")))
         (result (superchat-bridge-transform-messages messages superchat-bridge-tm-test--kimi))
         (assistant (nth 1 result))
         (content (superchat-bridge-message-content assistant)))
    (should (= (length content) 2))
    (should (superchat-bridge-text-content-p (nth 0 content)))
    (should (equal (superchat-bridge-text-content-text (nth 0 content))
                   "Let me check the TLS versions."))
    (should (superchat-bridge-tool-call-p (nth 1 content)))
    (should (equal (superchat-bridge-tool-call-id (nth 1 content)) "call_1"))))

(ert-deftest superchat-bridge-api-transform-messages-test-alternating-models ()
  "Thinking is kept only from the target model when models alternate."
  (let* ((kimi "moonshotai/Kimi-K3") (glm "zai-org/GLM-5.3")
         (assistant (lambda (model thinking text)
                      (superchat-bridge-tm-test--assistant
                       "openai-completions" "sference" model
                       (list (superchat-bridge-tm-test--thinking thinking)
                             (superchat-bridge-tm-test--text text))
                       "stop")))
         (messages (list (superchat-bridge-tm-test--user "1")
                         (funcall assistant kimi "kimi 1" "a")
                         (superchat-bridge-tm-test--user "2")
                         (funcall assistant glm "glm 1" "b")
                         (superchat-bridge-tm-test--user "3")
                         (funcall assistant kimi "kimi 2" "c")
                         (superchat-bridge-tm-test--user "4")
                         (funcall assistant glm "glm 2" "d")
                         (superchat-bridge-tm-test--user "hi")))
         (result (superchat-bridge-transform-messages messages superchat-bridge-tm-test--kimi))
         (contents (mapcar (lambda (message)
                             (mapcar (lambda (block)
                                       (if (superchat-bridge-thinking-content-p block)
                                           (superchat-bridge-thinking-content-thinking block)
                                         (superchat-bridge-text-content-text block)))
                                     (superchat-bridge-message-content message)))
                           (seq-filter #'superchat-bridge-assistant-message-p result))))
    (should (equal contents '(("kimi 1" "a") ("b") ("kimi 2" "c") ("d"))))))

;;; Copilot, OpenAI to Anthropic

(defconst superchat-bridge-tm-test--claude
  (superchat-bridge-tm-test--model "anthropic-messages" "github-copilot"
                            "claude-sonnet-4.6"))

(ert-deftest superchat-bridge-api-transform-messages-test-thought-signature-removed ()
  "thoughtSignature goes from tool calls when migrating between models."
  (let* ((messages
          (list (superchat-bridge-tm-test--user "run a command")
                (superchat-bridge-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (superchat-bridge-tm-test--call
                        "call_123" "bash"
                        :thought-signature "{\"type\":\"reasoning.encrypted\"}")))
                (superchat-bridge-tm-test--result "call_123" "bash" "output")))
         (result (superchat-bridge-transform-messages
                  messages superchat-bridge-tm-test--claude
                  #'superchat-bridge-tm-test--normalize-anthropic))
         (tool-call (car (superchat-bridge-message-content (nth 1 result)))))
    (should (null (superchat-bridge-tool-call-thought-signature tool-call)))
    ;; the input message is not changed
    (should (superchat-bridge-tool-call-thought-signature
             (car (superchat-bridge-message-content (nth 1 messages)))))))

(ert-deftest superchat-bridge-api-transform-messages-test-trailing-orphaned-call ()
  "Synthetic tool results for trailing orphaned tool calls."
  (let* ((messages
          (list (superchat-bridge-tm-test--user "read the file")
                (superchat-bridge-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (superchat-bridge-tm-test--call "call_123|fc_123" "read")))))
         (result (superchat-bridge-transform-messages
                  messages superchat-bridge-tm-test--claude
                  #'superchat-bridge-tm-test--normalize-anthropic))
         (last-message (car (last result))))
    (should (superchat-bridge-tool-result-message-p last-message))
    (should (equal (superchat-bridge-tool-result-message-tool-call-id last-message)
                   "call_123_fc_123"))
    (should (equal (superchat-bridge-tool-result-message-tool-name last-message) "read"))
    (should (eq (superchat-bridge-tool-result-message-is-error last-message) t))
    (should (equal (superchat-bridge-text-content-text
                    (car (superchat-bridge-message-content last-message)))
                   "No result provided"))))

(ert-deftest superchat-bridge-api-transform-messages-test-only-missing-results ()
  "Synthetic results only for trailing calls still missing one."
  (let* ((messages
          (list (superchat-bridge-tm-test--user "run commands")
                (superchat-bridge-tm-test--assistant
                 "openai-responses" "github-copilot" "gpt-5"
                 (list (superchat-bridge-tm-test--call "call_1|fc_1" "read")
                       (superchat-bridge-tm-test--call "call_2|fc_2" "bash")))
                (superchat-bridge-tm-test--result "call_1|fc_1" "read" "done")))
         (result (superchat-bridge-transform-messages
                  messages superchat-bridge-tm-test--claude
                  #'superchat-bridge-tm-test--normalize-anthropic))
         (synthetic (seq-filter (lambda (message)
                                  (and (superchat-bridge-tool-result-message-p message)
                                       (superchat-bridge-tool-result-message-is-error
                                        message)))
                                result)))
    (should (= (length synthetic) 1))
    (should (equal (superchat-bridge-tool-result-message-tool-call-id (car synthetic))
                   "call_2_fc_2"))
    (should (equal (superchat-bridge-tool-result-message-tool-name (car synthetic))
                   "bash"))
    ;; the real result follows its call under the normalised id
    (should (equal (superchat-bridge-tool-result-message-tool-call-id (nth 2 result))
                   "call_1_fc_1"))))

;;; Lax content

(ert-deftest superchat-bridge-api-transform-messages-test-null-content ()
  "Null or missing content becomes the empty list."
  (let* ((model (superchat-bridge-tm-test--model "openai-completions" "openai"
                                          "test-model" '("text")))
         (messages (list (superchat-bridge-make-user-message :content :null :timestamp 0)
                         (superchat-bridge-tm-test--assistant
                          "openai-completions" "openai" "test-model" :null "stop")
                         (superchat-bridge-make-tool-result-message
                          :tool-call-id "call_1" :tool-name "web_search"
                          :is-error nil :timestamp 0)))
         (result (superchat-bridge-transform-messages messages model)))
    (should (= (length result) 3))
    (dolist (message result)
      (should (equal (superchat-bridge-message-content message) nil)))))

;;; Port

(ert-deftest superchat-bridge-api-transform-messages-test-port-image-notes ()
  "A model without image input gets notes for runs of images."
  (let* ((text-model (superchat-bridge-tm-test--model "openai-completions" "p" "m" '("text")))
         (image (superchat-bridge-make-image-content :data "x" :mime-type "image/png"))
         (messages
          (list (superchat-bridge-tm-test--user
                 (list (superchat-bridge-tm-test--text "look") image image
                       (superchat-bridge-tm-test--text "done")))
                (superchat-bridge-tm-test--user "a string stays")
                (superchat-bridge-make-tool-result-message
                 :tool-call-id "c" :tool-name "t" :content (list image)
                 :timestamp 0)))
         (result (superchat-bridge-transform-messages messages text-model))
         (texts (lambda (message)
                  (mapcar #'superchat-bridge-text-content-text
                          (superchat-bridge-message-content message)))))
    (should (equal (funcall texts (nth 0 result))
                   '("look" "(image omitted: model does not support images)" "done")))
    (should (equal (superchat-bridge-message-content (nth 1 result)) "a string stays"))
    (should (equal (funcall texts (nth 2 result))
                   '("(tool image omitted: model does not support images)")))
    ;; a model with image input keeps them
    (let ((same (superchat-bridge-transform-messages
                 messages superchat-bridge-tm-test--kimi)))
      (should (eq (nth 0 same) (nth 0 messages))))))

(ert-deftest superchat-bridge-api-transform-messages-test-port-error-and-aborted-skipped ()
  "Assistant messages that ended with error or aborted are dropped."
  (let* ((same (lambda (content stop)
                 (superchat-bridge-tm-test--assistant
                  "openai-completions" "sference" "moonshotai/Kimi-K3"
                  content stop)))
         (messages (list (superchat-bridge-tm-test--user "a")
                         (funcall same (list (superchat-bridge-tm-test--call "x" "t")) "error")
                         (superchat-bridge-tm-test--user "b")
                         (funcall same nil "aborted")
                         (funcall same (list (superchat-bridge-tm-test--text "ok")) "stop")))
         (result (superchat-bridge-transform-messages messages superchat-bridge-tm-test--kimi)))
    ;; the errored turn's call gets no synthetic result either: its
    ;; assistant message is skipped before its calls are tracked
    (should (equal (superchat-bridge-tm-test--roles result)
                   '("user" "user" "assistant")))))

(ert-deftest superchat-bridge-api-transform-messages-test-port-held-system-message ()
  "A system message between a call and its result waits for the results."
  (let* ((system (superchat-bridge-make-system-message :content "later" :timestamp 0))
         (messages
          (list (superchat-bridge-tm-test--user "go")
                (superchat-bridge-tm-test--assistant
                 "openai-completions" "sference" "moonshotai/Kimi-K3"
                 (list (superchat-bridge-tm-test--call "c1" "a")
                       (superchat-bridge-tm-test--call "c2" "b")))
                system
                (superchat-bridge-tm-test--result "c1" "a" "r1")
                (superchat-bridge-tm-test--user "next")))
         (result (superchat-bridge-transform-messages messages superchat-bridge-tm-test--kimi)))
    ;; c2 gets a synthetic result when the user turn comes; the system
    ;; message comes after both results.
    (should (equal (superchat-bridge-tm-test--roles result)
                   '("user" "assistant" "toolResult" "toolResult" "system" "user")))
    (should (equal (superchat-bridge-tool-result-message-tool-call-id (nth 3 result)) "c2"))
    (should (eq (nth 4 result) system))))

(ert-deftest superchat-bridge-api-transform-messages-test-port-same-model-keeps-blocks ()
  "The target model's own blocks are kept, signed and redacted thinking too."
  (let* ((signed (superchat-bridge-make-thinking-content :thinking "" :thinking-signature "sig"))
         (redacted (superchat-bridge-make-thinking-content :thinking "" :redacted t))
         (empty (superchat-bridge-make-thinking-content :thinking "  \n "))
         (text (superchat-bridge-make-text-content :text "t" :text-signature "ts"))
         (messages
          (list (superchat-bridge-tm-test--assistant
                 "openai-completions" "sference" "moonshotai/Kimi-K3"
                 (list signed redacted empty text) "stop")))
         (result (superchat-bridge-transform-messages messages superchat-bridge-tm-test--kimi))
         (content (superchat-bridge-message-content (car result))))
    (should (equal content (list signed redacted text)))
    ;; another model's text loses its signature
    (let* ((other (superchat-bridge-tm-test--assistant
                   "openai-completions" "sference" "other" (list text) "stop"))
           (out (car (superchat-bridge-message-content
                      (car (superchat-bridge-transform-messages
                            (list other) superchat-bridge-tm-test--kimi))))))
      (should (equal (superchat-bridge-text-content-text out) "t"))
      (should (null (superchat-bridge-text-content-text-signature out))))))

(ert-deftest superchat-bridge-api-transform-messages-test-port-split-error-result ()
  "The images of an error tool result are split out."
  (let* ((image (superchat-bridge-make-image-content :data "x" :mime-type "image/png"))
         (message (superchat-bridge-make-tool-result-message
                   :tool-call-id "call-1" :tool-name "codemode" :is-error t
                   :content (list (superchat-bridge-tm-test--text "failed") image image)
                   :timestamp 0))
         (split (superchat-bridge-split-error-tool-result-images message)))
    (should (equal (mapcar #'superchat-bridge-text-content-text
                           (superchat-bridge-error-tool-result-split-content split))
                   '("failed" "(image attached after the tool results)")))
    (should (equal (superchat-bridge-error-tool-result-split-caption split)
                   "Images from the error result of tool call call-1 (codemode):"))
    (should (equal (superchat-bridge-error-tool-result-split-images split)
                   (list image image)))
    ;; no split when it is no error, or has no image
    (setf (superchat-bridge-tool-result-message-is-error message) nil)
    (should (null (superchat-bridge-split-error-tool-result-images message)))
    (should (null (superchat-bridge-split-error-tool-result-images
                   (superchat-bridge-tm-test--result "c" "t" "x" t))))))

(provide 'superchat-bridge-api-transform-messages-test)
;;; superchat-bridge-api-transform-messages-test.el ends here
