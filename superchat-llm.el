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
(require 'superchat-preset)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(defvar superchat-llm-streaming)
(defvar superchat-llm-reasoning)
(defvar superchat-llm-tools-enabled)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
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
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-chat-prompt-tools "llm" (prompt))

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

(defun superchat--collect-llm-tools (&optional input tool-names)
  "Collect built-in + MCP tools for the current request.
Returns nil when `superchat-llm-tools-enabled' (combined with INPUT
for the `on-demand' policy) forbids attaching tools — that lets the
caller bypass llm.el's multi-output / tool-calling mode entirely,
which is the main contributor to time-to-first-token.

When TOOL-NAMES is non-nil, only tools whose names are members of
that list are returned.  This allows presets to restrict the active
tool set without changing global allowlists.  The symbol `none'
means an explicitly empty tool set (preset declared `tools: []'):
no tools are attached at all."
  (unless (eq tool-names 'none)
    (when (or tool-names (superchat--should-attach-tools-p input))
      (let ((llm-tools (when (fboundp 'superchat-get-llm-tools)
                         (superchat-get-llm-tools)))
            (mcp-tools (when (fboundp 'superchat-mcp-get-tools)
                         (superchat-mcp-get-tools))))
        (let ((all (append llm-tools mcp-tools)))
          (if tool-names
              (cl-remove-if-not (lambda (tool)
                                  (member (llm-tool-name tool) tool-names))
                                all)
            all))))))

(defun superchat--llm-extract-text (result)
  "Extract the :text field from an llm.el multi-output RESULT.
For plain string results, returns the string unchanged."
  (cond
   ((null result) "")
   ((stringp result) result)
   ((and (consp result) (stringp (plist-get result :text)))
    (plist-get result :text))
   (t (format "%S" result))))

(defun superchat--llm-tool-round-p (result)
  "Return non-nil when RESULT asks the caller to continue after tools.
llm.el executes requested tools and appends their results to the mutable
chat prompt, but leaves sending the next model request to its caller.
Tool-only results are either an alist of tool-result conses or a
multi-output plist with `:tool-results'.  A non-empty `:text' always
means the model has supplied its final answer instead."
  (let ((text (and (consp result) (plist-get result :text))))
    (and (not (and (stringp text) (not (string-empty-p text))))
         (or (and (consp result)
                  (not (keywordp (car result))))
             (and (consp result) (plist-get result :tool-results))))))

(defun superchat--llm-round-limit-action ()
  "Return the action to take after exhausting the current tool budget."
  (pcase superchat-llm-round-limit-action
    ('ask (if noninteractive
              'finalize
            (if (y-or-n-p
                 (format "Tool round limit reached. Allow another %d rounds? "
                         superchat-llm-max-tool-rounds))
                'continue
              'finalize)))
    ((or 'finalize 'stop) superchat-llm-round-limit-action)
    (_ 'stop)))

(defun superchat--llm-disable-prompt-tools (prompt)
  "Remove tools from mutable llm.el PROMPT for its final request."
  (when (fboundp 'llm-chat-prompt-tools)
    (setf (llm-chat-prompt-tools prompt) nil)))

(defun superchat--build-llm-prompt (text tools &optional context preset)
  "Build an `llm-chat-prompt' struct from TEXT and TOOLS.
llm.el ≥ 0.7 requires a struct for `llm-chat-streaming' / `llm-chat',
even when no tools are attached.  TOOLS may be nil.

CONTEXT, when a non-empty string, is passed as the prompt's
`:context' — llm.el turns it into the system message.  This is how
turn system-prompts (preset persona, language instruction, tool
guidance) reach the provider.

PRESET may override temperature, max tokens, and reasoning for this
request.  Otherwise reasoning comes from `superchat-llm-reasoning' so that
reasoning-capable providers (Ollama qwen3.x, deepseek-r1, etc.) skip
thinking by default — thinking blocks streaming and inflates TTFT."
  (let* ((preset-reasoning (and preset (superchat-preset-reasoning preset)))
         (reasoning (if (and preset-reasoning
                             (not (eq preset-reasoning 'inherit)))
                        preset-reasoning
                      superchat-llm-reasoning))
         (args (append (when tools (list :tools tools))
                       (when (and (stringp context)
                                  (not (string-empty-p (string-trim context))))
                         (list :context context))
                       (when (and preset (superchat-preset-temperature preset))
                         (list :temperature
                               (superchat-preset-temperature preset)))
                       (when (and preset (superchat-preset-max-tokens preset))
                         (list :max-tokens
                               (superchat-preset-max-tokens preset)))
                       (when reasoning
                         (list :reasoning (if (eq reasoning t)
                                             'medium
                                           reasoning))))))
    (apply #'llm-make-chat-prompt text args)))

(defun superchat--llm-generate-answer-sync (prompt &optional target-model tools agent-mode system-prompt preset)
  "Generate an answer for PROMPT using llm.el synchronously.
This is a blocking call intended for internal systems like workflows.
Supports llm.el tools. Optionally use TARGET-MODEL for this request only.
TOOLS is an optional list of tool names to expose; when nil, tools are
collected from PROMPT and global settings as before.
AGENT-MODE, when non-nil, wraps tools with agent observability and
safety guardrails from `superchat-agent-loop'.
SYSTEM-PROMPT, when a non-empty string, is sent as the system
message via the prompt's `:context'."
  (unless superchat-llm-backend
    (error "superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...))."))
  (let* ((effective-backend (superchat--effective-llm-backend target-model))
         (tools (superchat--collect-llm-tools
                 (when (stringp prompt) prompt)
                 tools))
         (tools (if (and agent-mode tools (fboundp 'superchat--agent-wrap-tools))
                    (superchat--agent-wrap-tools tools preset)
                  tools))
         (real-prompt (superchat--build-llm-prompt
                       prompt tools system-prompt preset))
         (multi-output (and tools t)))
    (message "🤖 Synchronously generating answer%s..."
             (if tools (format " (tools: %d)" (length tools)) ""))
    (condition-case err
        (let ((response (llm-chat effective-backend real-prompt multi-output))
              (tool-rounds 0)
              (tool-round-limit superchat-llm-max-tool-rounds)
              (final-answer-requested nil))
          (while (superchat--llm-tool-round-p response)
            (cond
             (final-answer-requested
              (setq response
                    "[Tool call returned after requesting a final answer]"))
             (t
              (cl-incf tool-rounds)
              (when (and (numberp tool-round-limit)
                         (>= tool-rounds tool-round-limit))
                (pcase (superchat--llm-round-limit-action)
                  ('continue
                   (if (and (numberp superchat-llm-max-tool-rounds)
                            (> superchat-llm-max-tool-rounds 0))
                       (setq tool-round-limit
                             (+ tool-round-limit superchat-llm-max-tool-rounds))
                     (setq final-answer-requested t
                           multi-output nil)
                     (superchat--llm-disable-prompt-tools real-prompt)))
                  ('finalize
                   (setq final-answer-requested t
                         multi-output nil)
                   (superchat--llm-disable-prompt-tools real-prompt))
                  ('stop
                   (setq response
                         (format "[Stopped after %d tool rounds]"
                                 tool-rounds)))))
              (when (superchat--llm-tool-round-p response)
                (setq response
                      (llm-chat effective-backend real-prompt multi-output))))))
          (message "✅ Synchronous generation complete.")
          (superchat--llm-extract-text response))
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
