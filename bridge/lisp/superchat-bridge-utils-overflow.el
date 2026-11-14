;;; superchat-bridge-utils-overflow.el --- Context overflow detection, ported  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  The superchat-bridge contributors

;; This file is part of superchat-bridge, a port of @earendil-works/pi-ai.

;;; Commentary:

;; Port of `src/utils/overflow.ts' (@earendil-works/pi-ai@1.4.0, commit
;; 58c0b02ce552adab76014e7ed712ab7981359aa8).
;;
;;   `superchat-bridge-context-overflow-p'     the source's `isContextOverflow'
;;   `superchat-bridge-recoverable-length-p'   `isRecoverableLength'
;;   `superchat-bridge-get-overflow-patterns'  `getOverflowPatterns'
;;
;; The source tests error messages with regular expressions.  Each
;; is written here as an Emacs regexp, matched with `case-fold-search'
;; on, as the source's `i' flag does.  The translations change no
;; meaning, except that JavaScript's `\w' and `\s' are ASCII letters,
;; digits and underscore, and `[:space:]', where JavaScript's `\s'
;; takes in a few more Unicode spaces.  The source's `^' and `$' are
;; the start and end of the whole string (no `m' flag), `\\`' and
;; `\\'' here.

;;; Code:

(require 'cl-lib)
(require 'superchat-bridge-types)

(defconst superchat-bridge--overflow-patterns
  '("prompt \\(?:is \\)?too long"      ; Anthropic and z.ai token overflow
    "prompt exceeds max length"        ; z.ai CN endpoint token overflow
    "request_too_large"                ; Anthropic request byte-size overflow (HTTP 413)
    "input is too long for requested model" ; Amazon Bedrock
    "exceeds the context window"       ; OpenAI (Completions & Responses API)
    ;; OpenAI-compatible proxies (LiteLLM)
    "exceeds \\(?:the \\)?\\(?:model'?s \\)?maximum context length\\(?: of [0-9,]+ tokens?\\|[[:space:]]*([0-9,]+)\\)"
    "input token count.*exceeds the maximum" ; Google (Gemini)
    "maximum prompt length is [0-9]+"  ; xAI (Grok)
    "reduce the length of the messages" ; Groq
    "maximum context length is [0-9]+ tokens" ; OpenRouter (most backends)
    "exceeds \\(?:the \\)?maximum allowed input length of [0-9,]+ tokens?" ; OpenRouter/Poolside
    ;; Together AI
    "input ([0-9]+ tokens) is longer than the model'?s context length ([0-9]+ tokens)"
    "exceeds the limit of [0-9]+"      ; GitHub Copilot
    "exceeds the available context size" ; llama.cpp server
    "greater than the context length"  ; LM Studio
    "context window exceeds limit"     ; MiniMax
    "exceeded model token limit"       ; Kimi For Coding
    "too large for model with [0-9]+ maximum context length" ; Mistral
    ;; DS4 server
    "prompt has [0-9,]+ tokens?, but the configured context size is [0-9,]+ tokens?"
    "model_context_window_exceeded"    ; z.ai non-standard finish_reason as error text
    "prompt too long; exceeded \\(?:max \\)?context length" ; Ollama explicit overflow error
    "range of input length should be"  ; DashScope / Qwen Token Plan
    "context[_ ]length[_ ]exceeded"    ; Generic fallback
    "too many tokens"                  ; Generic fallback
    "token limit exceeded"             ; Generic fallback
    ;; HTTP 413 means request-size overflow regardless of provider or body text.
    ;; Match raw SDK errors and formatProviderError's prefix, not a status
    ;; mentioned in the body.
    "\\`\\(?:[A-Za-z0-9_ -]+ API error (413):\\|413\\(?:[[:space:]]\\|:\\|\\'\\)\\)")
  "The source's `OVERFLOW_PATTERNS', as Emacs regexps.")

(defconst superchat-bridge--cerebras-bodyless-overflow-pattern
  "\\`4\\(?:00\\|13\\)[[:space:]]*\\(?:status code\\)?[[:space:]]*(no body)"
  "The source's `CEREBRAS_BODYLESS_OVERFLOW_PATTERN'.")

(defconst superchat-bridge--non-overflow-patterns
  '("\\`\\(?:Throttling error\\|Service unavailable\\):" ; AWS Bedrock non-overflow errors
    "rate limit"                       ; Generic rate limiting
    "too many requests")               ; Generic HTTP 429 style
  "The source's `NON_OVERFLOW_PATTERNS'.
An error message matching one of these is no overflow even when it
also matches an overflow pattern, as Bedrock's \"Throttling error:
Too many tokens\" does.")

(defun superchat-bridge--matches-any-p (patterns text)
  "Return non-nil when any of PATTERNS matches TEXT, ignoring case."
  (let ((case-fold-search t))
    (cl-some (lambda (pattern) (string-match-p pattern text)) patterns)))

(defun superchat-bridge-context-overflow-p (message &optional context-window)
  "Return non-nil when assistant MESSAGE is a context overflow.
The source's `isContextOverflow'.  Three cases are told apart: an
error whose text matches a known overflow pattern; a successful reply
whose input (plus cache reads) exceeds CONTEXT-WINDOW, for providers
that take an oversized request silently; and a `length' stop with no
output whose input fills 99% of CONTEXT-WINDOW (Xiaomi MiMo).  The
last two apply only when CONTEXT-WINDOW is a non-zero number."
  (let ((stop-reason (superchat-bridge-assistant-message-stop-reason message))
        (error-message (superchat-bridge-assistant-message-error-message message))
        (usage (superchat-bridge-assistant-message-usage message))
        (window (and context-window (not (eql context-window 0)) context-window)))
    (or
     ;; Case 1: error message patterns
     (and (equal stop-reason "error")
          error-message
          (not (equal error-message ""))
          (not (superchat-bridge--matches-any-p superchat-bridge--non-overflow-patterns
                                         error-message))
          (or (superchat-bridge--matches-any-p superchat-bridge--overflow-patterns
                                        error-message)
              (and (equal (superchat-bridge-assistant-message-provider message)
                          "cerebras")
                   (superchat-bridge--matches-any-p
                    (list superchat-bridge--cerebras-bodyless-overflow-pattern)
                    error-message)))
          t)
     ;; Case 2: silent overflow (z.ai style)
     (and window
          (equal stop-reason "stop")
          (> (+ (superchat-bridge-usage-input usage) (superchat-bridge-usage-cache-read usage))
             window))
     ;; Case 3: length-stop overflow (Xiaomi MiMo style)
     (and window
          (equal stop-reason "length")
          (eql (superchat-bridge-usage-output usage) 0)
          (>= (+ (superchat-bridge-usage-input usage) (superchat-bridge-usage-cache-read usage))
              (* window 0.99))))))

(defun superchat-bridge-recoverable-length-p (message desired-max-output)
  "Return non-nil when MESSAGE's `length' stop is below DESIRED-MAX-OUTPUT.
The source's `isRecoverableLength'.  Such a stop may come from context
pressure or provider-side truncation, so a caller can try one bounded
compact-and-retry.  DESIRED-MAX-OUTPUT is the original limit, before
any clamping to the context."
  (and (equal (superchat-bridge-assistant-message-stop-reason message) "length")
       (> desired-max-output 0)
       (< (superchat-bridge-usage-output (superchat-bridge-assistant-message-usage message))
          desired-max-output)))

(defun superchat-bridge-get-overflow-patterns ()
  "Return a copy of the overflow patterns, as Emacs regexps.
The source's `getOverflowPatterns'."
  (copy-sequence superchat-bridge--overflow-patterns))

(provide 'superchat-bridge-utils-overflow)

;;; superchat-bridge-utils-overflow.el ends here
