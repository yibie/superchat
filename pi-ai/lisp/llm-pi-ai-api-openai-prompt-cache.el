;;; llm-pi-ai-api-openai-prompt-cache.el --- OpenAI prompt cache keys, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The llm-pi-ai contributors

;; This file is part of llm-pi-ai, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/openai-prompt-cache.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;;   `llm-pi-ai-clamp-openai-prompt-cache-key'  the source's `clampOpenAIPromptCacheKey'
;;   `llm-pi-ai-openai-prompt-cache-key-max-length'
;;                                              `OPENAI_PROMPT_CACHE_KEY_MAX_LENGTH'
;;
;; The source counts code points (`Array.from(key)'); an Emacs string is
;; a sequence of code points, so `length' and `substring' count the same.

;;; Code:

(defconst llm-pi-ai-openai-prompt-cache-key-max-length 64
  "Longest prompt cache key OpenAI takes, in code points.")

(defun llm-pi-ai-clamp-openai-prompt-cache-key (key)
  "Return KEY, a string or nil, cut to the longest key OpenAI takes.
The source's `clampOpenAIPromptCacheKey'.  nil, the source's
`undefined', stays nil."
  (cond ((null key) nil)
        ((<= (length key) llm-pi-ai-openai-prompt-cache-key-max-length) key)
        (t (substring key 0 llm-pi-ai-openai-prompt-cache-key-max-length))))

(provide 'llm-pi-ai-api-openai-prompt-cache)

;;; llm-pi-ai-api-openai-prompt-cache.el ends here
