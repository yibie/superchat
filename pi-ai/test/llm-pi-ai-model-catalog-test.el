;;; llm-pi-ai-model-catalog-test.el --- Tests for llm-pi-ai-model-catalog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `llm-pi-ai-model-catalog.el', its port of
;; `src/model-catalog.ts'.  Upstream's `test/model-catalog-types.test.ts'
;; checks TypeScript types only and cannot be translated; the cases here
;; are from the behaviour of `flattenModelCatalog' (filter by `type',
;; `Object.fromEntries' on id) and of reading `data/deepseek.json'.

;;; Code:

(require 'ert)
(require 'llm-pi-ai-model-catalog)

(defun llm-pi-ai-model-catalog-test--parse (json)
  "Return the groups of the catalog JSON, a string."
  (llm-pi-ai-model-catalog-from-json
   (json-parse-string json :object-type 'alist :array-type 'list
                      :null-object :null :false-object :false)))

(defconst llm-pi-ai-model-catalog-test--json
  "{\"openai-completions\":{
     \"chat:a\":{\"type\":\"chat\",\"id\":\"a\",\"name\":\"A\",\"api\":\"openai-completions\",
                 \"provider\":\"p\",\"baseUrl\":\"https://x\",\"input\":[\"text\"],
                 \"cost\":{\"input\":1,\"output\":2,\"cacheRead\":0,\"cacheWrite\":0},
                 \"contextWindow\":100,\"maxTokens\":10,\"reasoning\":false},
     \"image:i\":{\"type\":\"image\",\"id\":\"i\",\"name\":\"I\",\"api\":\"openrouter-images\",
                 \"provider\":\"p\",\"baseUrl\":\"https://x\",\"input\":[\"text\"],
                 \"output\":[\"image\",\"text\"],
                 \"cost\":{\"input\":1,\"output\":2,\"cacheRead\":0,\"cacheWrite\":0}}},
    \"anthropic-messages\":{
     \"chat:b\":{\"type\":\"chat\",\"id\":\"b\",\"name\":\"B\",\"api\":\"anthropic-messages\",
                 \"provider\":\"p\",\"baseUrl\":\"https://y\",\"input\":[\"text\",\"image\"],
                 \"cost\":{\"input\":1,\"output\":2,\"cacheRead\":0,\"cacheWrite\":0},
                 \"contextWindow\":100,\"maxTokens\":10,\"reasoning\":true},
     \"classifier:c\":{\"type\":\"classifier\",\"id\":\"c\",\"name\":\"C\",\"api\":\"llama-cpp-classify\",
                 \"provider\":\"p\",\"baseUrl\":\"https://y\",\"input\":[\"text\"],
                 \"cost\":{\"input\":0,\"output\":0,\"cacheRead\":0,\"cacheWrite\":0},
                 \"contextWindow\":100},
     \"chat:a\":{\"type\":\"chat\",\"id\":\"a\",\"name\":\"A again\",\"api\":\"anthropic-messages\",
                 \"provider\":\"p\",\"baseUrl\":\"https://y\",\"input\":[\"text\"],
                 \"cost\":{\"input\":1,\"output\":2,\"cacheRead\":0,\"cacheWrite\":0},
                 \"contextWindow\":100,\"maxTokens\":10,\"reasoning\":false}}}"
  "A catalog of two apis with chat, image and classifier models, and a
chat id that appears in both apis.")

(ert-deftest llm-pi-ai-model-catalog-test-flatten-chat ()
  "Chat models of every api, by id; a later one with the same id replaces the
value of the earlier one and keeps its place."
  (let* ((groups (llm-pi-ai-model-catalog-test--parse
                  llm-pi-ai-model-catalog-test--json))
         (flat (llm-pi-ai-flatten-chat-model-catalog "p" groups)))
    (should (equal (mapcar #'car flat) '("a" "b")))
    (should (equal (llm-pi-ai-model-name (cdr (assoc "a" flat))) "A again"))
    (should (equal (llm-pi-ai-model-api (cdr (assoc "a" flat)))
                   "anthropic-messages"))
    (should (eq (llm-pi-ai-model-type (cdr (assoc "b" flat))) 'chat))))

(ert-deftest llm-pi-ai-model-catalog-test-flatten-other-types ()
  (let* ((groups (llm-pi-ai-model-catalog-test--parse
                  llm-pi-ai-model-catalog-test--json))
         (images (llm-pi-ai-flatten-image-model-catalog "p" groups))
         (classifiers (llm-pi-ai-flatten-classifier-model-catalog "p" groups)))
    (should (equal (mapcar #'car images) '("i")))
    (should (llm-pi-ai-image-model-p (cdr (assoc "i" images))))
    (should (equal (llm-pi-ai-image-model-output (cdr (assoc "i" images)))
                   '("image" "text")))
    (should (equal (mapcar #'car classifiers) '("c")))
    (should (llm-pi-ai-classifier-model-p (cdr (assoc "c" classifiers))))))

(ert-deftest llm-pi-ai-model-catalog-test-flatten-needs-explicit-type ()
  "A model whose `type' is unset is in no catalog: the source compares `type'."
  (let ((groups (list (cons "api" (list (cons "chat:x" (llm-pi-ai-make-model :id "x")))))))
    (should-not (llm-pi-ai-flatten-chat-model-catalog "p" groups))))

(ert-deftest llm-pi-ai-model-catalog-test-flatten-empty ()
  (should-not (llm-pi-ai-flatten-chat-model-catalog "p" nil))
  (should-not (llm-pi-ai-flatten-chat-model-catalog "p" '(("api")))))

(ert-deftest llm-pi-ai-model-catalog-test-kebab ()
  (should (equal (llm-pi-ai-model-catalog--kebab "supportsStrictMode")
                 "supports-strict-mode"))
  (should (equal (llm-pi-ai-model-catalog--kebab "supportsOpenAIGrammarTools")
                 "supports-open-ai-grammar-tools"))
  (should (equal (llm-pi-ai-model-catalog--kebab "zaiToolStream") "zai-tool-stream"))
  (should (equal (llm-pi-ai-model-catalog--kebab "thinkingFormat") "thinking-format")))

;;; data/deepseek.json

(ert-deftest llm-pi-ai-model-catalog-test-read-deepseek ()
  "The shipped DeepSeek catalog flattens to its two chat models."
  (let ((flat (llm-pi-ai-flatten-chat-model-catalog
               "deepseek" (llm-pi-ai-model-catalog-read-provider "deepseek"))))
    (should (equal (mapcar #'car flat) '("deepseek-flash" "deepseek-v4-pro")))
    (let ((flash (cdr (assoc "deepseek-flash" flat)))
          (pro (cdr (assoc "deepseek-v4-pro" flat))))
      (should (equal (llm-pi-ai-model-name flash) "DeepSeek V4.1 Flash"))
      (should (equal (llm-pi-ai-model-api flash) "openai-completions"))
      (should (equal (llm-pi-ai-model-provider flash) "deepseek"))
      (should (equal (llm-pi-ai-model-base-url flash) "https://api.deepseek.com"))
      (should (eq (llm-pi-ai-model-reasoning flash) t))
      (should (eq (llm-pi-ai-model-type flash) 'chat))
      (should (equal (llm-pi-ai-model-input flash) '("text" "image")))
      (should (equal (llm-pi-ai-model-input pro) '("text")))
      (should (= (llm-pi-ai-model-context-window flash) 1000000))
      (should (= (llm-pi-ai-model-max-tokens flash) 384000))
      ;; cost
      (let ((cost (llm-pi-ai-model-cost flash)))
        (should (llm-pi-ai-model-cost-p cost))
        (should (= (llm-pi-ai-model-cost-input cost) 0.3))
        (should (= (llm-pi-ai-model-cost-output cost) 1.2))
        (should (= (llm-pi-ai-model-cost-cache-read cost) 0.006))
        (should (= (llm-pi-ai-model-cost-cache-write cost) 0))
        (should-not (llm-pi-ai-model-cost-tiers cost)))
      ;; thinking level map: null is :null, absent stays absent
      (should (equal (llm-pi-ai-model-thinking-level-map flash)
                     '(("minimal" . :null) ("low" . "low") ("medium" . :null)
                       ("high" . "high") ("max" . "max"))))
      ;; compat: kebab-case symbol keys, false present as nil
      (let ((compat (llm-pi-ai-model-compat flash)))
        (should (equal (alist-get 'thinking-format compat) "deepseek"))
        (should (equal (alist-get 'max-tokens-field compat) "max_tokens"))
        (should (eq (alist-get 'supports-strict-mode compat) t))
        (should (eq (alist-get 'requires-reasoning-content-on-assistant-messages
                               compat)
                    t))
        ;; JSON false is nil, but the key is there: `assq' tells it from absent
        (should (assq 'supports-store compat))
        (should-not (alist-get 'supports-store compat))
        (should-not (assq 'supports-temperature compat)))
      ;; image input limits
      (let ((resize (llm-pi-ai-model-image-input-limits-resize
                     (llm-pi-ai-model-input-limits-images
                      (llm-pi-ai-model-input-limits flash)))))
        (should (= (llm-pi-ai-model-image-resize-options-max-width resize) 2000))
        (should (= (llm-pi-ai-model-image-resize-options-jpeg-quality resize) 80)))
      (should-not (llm-pi-ai-model-input-limits pro)))))

(provide 'llm-pi-ai-model-catalog-test)

;;; llm-pi-ai-model-catalog-test.el ends here
