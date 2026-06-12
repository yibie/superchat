;;; superchat-llm.el --- LLM backend abstraction for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; LLM provider abstraction, tool collection, prompt building, and
;; streaming answer generation.  Extracted from superchat.el monolith
;; (v0.9 split step 5).

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
(require 'superchat-render)
(require 'superchat-mcp)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-streaming)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-tools-enabled)
(defvar superchat-response-timeout)
(defvar superchat-tool-timeout-multiplier)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat--ttft-start-time)
(defvar superchat-buffer-name)
(defvar superchat--active-timeout-timer)
(declare-function superchat--is-ollama-backend-p "superchat-models" (&optional backend))
(declare-function superchat--detect-response-mode "superchat" (&optional tools tools-known-p))
(declare-function superchat--show-response-mode-indicator "superchat" (&optional mode))
(declare-function superchat--get-adjusted-timeout "superchat" (&optional mode))
(declare-function superchat-get-llm-tools "superchat-tools" ())
(declare-function superchat-mcp-get-tools "superchat-mcp" ())

(cl-defgeneric superchat--provider-name (provider)
  "Return a human-readable lowercase name for PROVIDER.")

(cl-defgeneric superchat--provider-chat-model (provider)
  "Return the chat-model of PROVIDER, or nil if not extractable.")

(cl-defgeneric superchat--provider-with-chat-model (provider new-model)
  "Return a copy of PROVIDER with chat-model set to NEW-MODEL.
Default: return PROVIDER unchanged (no override available).")

(defun superchat--effective-llm-backend (&optional target-model)
  "Return the effective llm backend, applying model override.
TARGET-MODEL is a one-shot model override; falls back to
`superchat-llm-model'.  When neither resolves, the raw backend is
returned.  If model override is unavailable for the provider type
\(only OpenAI, Claude, Ollama have copy-* methods installed), the
backend is returned unchanged and the override is silently dropped."
  (let* ((model (or target-model superchat-llm-model))
         (backend superchat-llm-backend))
    (cond
     ((null backend) nil)
     ((null model) backend)
     (t
      (condition-case nil
          (superchat--provider-with-chat-model backend model)
        (error backend))))))

(defun superchat--should-attach-tools-p (input)
  "Return non-nil when tools should be attached for INPUT.
Honors `superchat-llm-tools-enabled':
  nil          -> never
  `always'/t   -> always
  `on-demand'  -> only when INPUT shows a tool intent:
                  contains a `#'-style file reference, or a slash
                  command is currently active."
  (cond
   ((null superchat-llm-tools-enabled) nil)
   ((memq superchat-llm-tools-enabled '(always t)) t)
   (t  ;; on-demand and anything else conservatively maps here
    (or (and (boundp 'superchat--current-command)
             superchat--current-command)
        (and (stringp input)
             (string-match-p superchat--file-ref-regexp input))))))

(defun superchat--collect-llm-tools (&optional input)
  "Collect built-in + MCP tools for the current request.
Returns nil when `superchat-llm-tools-enabled' (combined with INPUT
for the `on-demand' policy) forbids attaching tools — that lets the
caller bypass llm.el's multi-output / tool-calling mode entirely,
which is the main contributor to time-to-first-token."
  (when (superchat--should-attach-tools-p input)
    (let ((llm-tools (when (fboundp 'superchat-get-llm-tools)
                       (superchat-get-llm-tools)))
          (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                       (superchat-mcp-get-tools))))
      (append llm-tools mcp-tools))))

(defun superchat--llm-extract-text (result)
  "Extract the :text field from an llm.el multi-output RESULT.
For plain string results, returns the string unchanged."
  (cond
   ((null result) "")
   ((stringp result) result)
   ((and (consp result) (stringp (plist-get result :text)))
    (plist-get result :text))
   (t (format "%S" result))))

(defun superchat--build-llm-prompt (text tools)
  "Build an `llm-chat-prompt' struct from TEXT and TOOLS.
llm.el ≥ 0.7 requires a struct for `llm-chat-streaming' / `llm-chat',
even when no tools are attached.  TOOLS may be nil.

The `:reasoning' key is set from `superchat-llm-reasoning' so that
reasoning-capable providers (Ollama qwen3.x, deepseek-r1, etc.) skip
thinking by default — thinking blocks streaming and inflates TTFT."
  (let ((args (append (when tools (list :tools tools))
                     (when superchat-llm-reasoning
                       (list :reasoning (if (eq superchat-llm-reasoning t)
                                           'medium
                                         superchat-llm-reasoning))))))
    (apply #'llm-make-chat-prompt text args)))

(defun superchat--llm-generate-answer-sync (prompt &optional target-model)
  "Generate an answer for PROMPT using llm.el synchronously.
This is a blocking call intended for internal systems like workflows.
Supports llm.el tools. Optionally use TARGET-MODEL for this request only."
  (unless superchat-llm-backend
    (error "superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...))."))
  (let* ((effective-backend (superchat--effective-llm-backend target-model))
         (tools (superchat--collect-llm-tools (when (stringp prompt) prompt)))
         (real-prompt (superchat--build-llm-prompt prompt tools))
         (multi-output (and tools t)))
    (message "🤖 Synchronously generating answer%s..."
             (if tools (format " (tools: %d)" (length tools)) ""))
    (message "✅ Synchronous generation complete.")
    (condition-case err
        (superchat--llm-extract-text
         (llm-chat effective-backend real-prompt multi-output))
      (error
       (format "[llm-chat error: %s]" (error-message-string err))))))

(defun superchat--llm-generate-answer-sync (prompt &optional target-model)
  "Generate an answer for PROMPT using llm.el synchronously.
This is a blocking call intended for internal systems like workflows.
Supports llm.el tools. Optionally use TARGET-MODEL for this request only."
  (unless superchat-llm-backend
    (error "superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...))."))
  (let* ((effective-backend (superchat--effective-llm-backend target-model))
         (tools (superchat--collect-llm-tools (when (stringp prompt) prompt)))
         (real-prompt (superchat--build-llm-prompt prompt tools))
         (multi-output (and tools t)))
    (message "🤖 Synchronously generating answer%s..."
             (if tools (format " (tools: %d)" (length tools)) ""))
    (message "✅ Synchronous generation complete.")
    (condition-case err
        (superchat--llm-extract-text
         (llm-chat effective-backend real-prompt multi-output))
      (error
       (format "[llm-chat error: %s]" (error-message-string err))))))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-chat-model ((provider llm-openai))
     (llm-openai-chat-model provider)))

(eval-after-load 'llm-openai
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-openai) new-model)
     (copy-llm-openai provider :chat-model new-model)))

(eval-after-load 'llm-claude
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-claude) new-model)
     (copy-llm-claude provider :chat-model new-model)))

(eval-after-load 'llm-ollama
  '(cl-defmethod superchat--provider-with-chat-model ((provider llm-ollama) new-model)
     (copy-llm-ollama provider :chat-model new-model)))


(provide 'superchat-llm)
;;; superchat-llm.el ends here
