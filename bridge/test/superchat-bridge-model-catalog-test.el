;;; superchat-bridge-model-catalog-test.el --- Tests for superchat-bridge-model-catalog  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Tests for `superchat-bridge-model-catalog.el', its port of
;; `src/model-catalog.ts'.  Upstream's `test/model-catalog-types.test.ts'
;; checks TypeScript types only and cannot be translated; the cases here
;; are from the behaviour of `flattenModelCatalog' (filter by `type',
;; `Object.fromEntries' on id) and of reading `data/deepseek.json'.

;;; Code:

(require 'ert)
(require 'superchat-bridge-model-catalog)

(defun superchat-bridge-model-catalog-test--parse (json)
  "Return the groups of the catalog JSON, a string."
  (superchat-bridge-model-catalog-from-json
   (json-parse-string json :object-type 'alist :array-type 'list
                      :null-object :null :false-object :false)))

(defconst superchat-bridge-model-catalog-test--json
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

(ert-deftest superchat-bridge-model-catalog-test-flatten-chat ()
  "Chat models of every api, by id; a later one with the same id replaces the
value of the earlier one and keeps its place."
  (let* ((groups (superchat-bridge-model-catalog-test--parse
                  superchat-bridge-model-catalog-test--json))
         (flat (superchat-bridge-flatten-chat-model-catalog "p" groups)))
    (should (equal (mapcar #'car flat) '("a" "b")))
    (should (equal (superchat-bridge-model-name (cdr (assoc "a" flat))) "A again"))
    (should (equal (superchat-bridge-model-api (cdr (assoc "a" flat)))
                   "anthropic-messages"))
    (should (eq (superchat-bridge-model-type (cdr (assoc "b" flat))) 'chat))))

(ert-deftest superchat-bridge-model-catalog-test-flatten-other-types ()
  (let* ((groups (superchat-bridge-model-catalog-test--parse
                  superchat-bridge-model-catalog-test--json))
         (images (superchat-bridge-flatten-image-model-catalog "p" groups))
         (classifiers (superchat-bridge-flatten-classifier-model-catalog "p" groups)))
    (should (equal (mapcar #'car images) '("i")))
    (should (superchat-bridge-image-model-p (cdr (assoc "i" images))))
    (should (equal (superchat-bridge-image-model-output (cdr (assoc "i" images)))
                   '("image" "text")))
    (should (equal (mapcar #'car classifiers) '("c")))
    (should (superchat-bridge-classifier-model-p (cdr (assoc "c" classifiers))))))

(ert-deftest superchat-bridge-model-catalog-test-flatten-needs-explicit-type ()
  "A model whose `type' is unset is in no catalog: the source compares `type'."
  (let ((groups (list (cons "api" (list (cons "chat:x" (superchat-bridge-make-model :id "x")))))))
    (should-not (superchat-bridge-flatten-chat-model-catalog "p" groups))))

(ert-deftest superchat-bridge-model-catalog-test-flatten-empty ()
  (should-not (superchat-bridge-flatten-chat-model-catalog "p" nil))
  (should-not (superchat-bridge-flatten-chat-model-catalog "p" '(("api")))))

(ert-deftest superchat-bridge-model-catalog-test-kebab ()
  (should (equal (superchat-bridge-model-catalog--kebab "supportsStrictMode")
                 "supports-strict-mode"))
  (should (equal (superchat-bridge-model-catalog--kebab "supportsOpenAIGrammarTools")
                 "supports-open-ai-grammar-tools"))
  (should (equal (superchat-bridge-model-catalog--kebab "zaiToolStream") "zai-tool-stream"))
  (should (equal (superchat-bridge-model-catalog--kebab "thinkingFormat") "thinking-format")))

;;; data/deepseek.json

(ert-deftest superchat-bridge-model-catalog-test-read-deepseek ()
  "The shipped DeepSeek catalog flattens to its two chat models."
  (let ((flat (superchat-bridge-flatten-chat-model-catalog
               "deepseek" (superchat-bridge-model-catalog-read-provider "deepseek"))))
    (should (equal (mapcar #'car flat) '("deepseek-flash" "deepseek-v4-pro")))
    (let ((flash (cdr (assoc "deepseek-flash" flat)))
          (pro (cdr (assoc "deepseek-v4-pro" flat))))
      (should (equal (superchat-bridge-model-name flash) "DeepSeek V4.1 Flash"))
      (should (equal (superchat-bridge-model-api flash) "openai-completions"))
      (should (equal (superchat-bridge-model-provider flash) "deepseek"))
      (should (equal (superchat-bridge-model-base-url flash) "https://api.deepseek.com"))
      (should (eq (superchat-bridge-model-reasoning flash) t))
      (should (eq (superchat-bridge-model-type flash) 'chat))
      (should (equal (superchat-bridge-model-input flash) '("text" "image")))
      (should (equal (superchat-bridge-model-input pro) '("text")))
      (should (= (superchat-bridge-model-context-window flash) 1000000))
      (should (= (superchat-bridge-model-max-tokens flash) 384000))
      ;; cost
      (let ((cost (superchat-bridge-model-cost flash)))
        (should (superchat-bridge-model-cost-p cost))
        (should (= (superchat-bridge-model-cost-input cost) 0.3))
        (should (= (superchat-bridge-model-cost-output cost) 1.2))
        (should (= (superchat-bridge-model-cost-cache-read cost) 0.006))
        (should (= (superchat-bridge-model-cost-cache-write cost) 0))
        (should-not (superchat-bridge-model-cost-tiers cost)))
      ;; thinking level map: null is :null, absent stays absent
      (should (equal (superchat-bridge-model-thinking-level-map flash)
                     '(("minimal" . :null) ("low" . "low") ("medium" . :null)
                       ("high" . "high") ("max" . "max"))))
      ;; compat: kebab-case symbol keys, false present as nil
      (let ((compat (superchat-bridge-model-compat flash)))
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
      (let ((resize (superchat-bridge-model-image-input-limits-resize
                     (superchat-bridge-model-input-limits-images
                      (superchat-bridge-model-input-limits flash)))))
        (should (= (superchat-bridge-model-image-resize-options-max-width resize) 2000))
        (should (= (superchat-bridge-model-image-resize-options-jpeg-quality resize) 80)))
      (should-not (superchat-bridge-model-input-limits pro)))))

(provide 'superchat-bridge-model-catalog-test)

;;; superchat-bridge-model-catalog-test.el ends here
