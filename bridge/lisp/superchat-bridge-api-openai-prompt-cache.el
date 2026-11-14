;;; superchat-bridge-api-openai-prompt-cache.el --- OpenAI prompt cache keys, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/api/openai-prompt-cache.ts' (@earendil-works/pi-ai@1.4.0,
;; commit 58c0b02ce552adab76014e7ed712ab7981359aa8), below `packages/ai/'.
;;
;;   `superchat-bridge-clamp-openai-prompt-cache-key'  the source's `clampOpenAIPromptCacheKey'
;;   `superchat-bridge-openai-prompt-cache-key-max-length'
;;                                              `OPENAI_PROMPT_CACHE_KEY_MAX_LENGTH'
;;
;; The source counts code points (`Array.from(key)'); an Emacs string is
;; a sequence of code points, so `length' and `substring' count the same.

;;; Code:

(defconst superchat-bridge-openai-prompt-cache-key-max-length 64
  "Longest prompt cache key OpenAI takes, in code points.")

(defun superchat-bridge-clamp-openai-prompt-cache-key (key)
  "Return KEY, a string or nil, cut to the longest key OpenAI takes.
The source's `clampOpenAIPromptCacheKey'.  nil, the source's
`undefined', stays nil."
  (cond ((null key) nil)
        ((<= (length key) superchat-bridge-openai-prompt-cache-key-max-length) key)
        (t (substring key 0 superchat-bridge-openai-prompt-cache-key-max-length))))

(provide 'superchat-bridge-api-openai-prompt-cache)

;;; superchat-bridge-api-openai-prompt-cache.el ends here
