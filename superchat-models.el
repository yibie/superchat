;;; superchat-models.el --- Model listing and switching for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Model discovery, caching, listing, and @model switching.
;; Extracted from superchat.el monolith (v0.9 split step 1).

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
(require 'superchat-parser)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-llm-backend)
(defvar superchat-llm-model)
(declare-function superchat--provider-chat-model "superchat" (provider))

(defvar superchat--model-list-cache nil
  "Cached list of available models from llm-models.
Bound to (models . timestamp) where timestamp is (float-time).
Cleared when superchat-llm-backend or superchat-manual-models changes.")

(defvar superchat--model-list-cache-ttl 300
  "Cache TTL for model list in seconds (default: 300 = 5 minutes).
Set to nil to disable expiry.")

(defun superchat--invalidate-model-cache ()
  "Clear the cached model list.
Call this when the backend or manual model configuration changes."
  (setq superchat--model-list-cache nil))

(defun superchat--is-ollama-backend-p (&optional backend)
  "Check if BACKEND is an Ollama provider.
If BACKEND is nil, check the current `superchat-llm-backend'."
  (let ((backend (or backend superchat-llm-backend)))
    (and backend
         (or (and (fboundp 'llm-name)
                  (string-match-p "ollama"
                                  (downcase (format "%s"
                                                    (ignore-errors (llm-name backend))))))
             (string-match-p "ollama"
                             (downcase (format "%s" backend)))))))

(defun superchat--parse-model-switch (input)
  "Parse input for @model syntax and return (clean-input . model) cons.
If no @model syntax is found, return nil."
  (superchat-parser-model-switch input))

(defcustom superchat-manual-models nil
  "Manually configured list of available models.
If set, this list will be used instead of trying to get models from gptel.
Example: (\"gpt-4\" \"gpt-3.5-turbo\" \"claude-3-opus\" \"qwen3-coder:30b-a3b-q8_0\")"
  :type '(repeat string)
  :group 'superchat
  :set (lambda (sym val)
         (set-default sym val)
         (superchat--invalidate-model-cache)))

(defun superchat--get-ollama-models ()
  "Get list of available Ollama models by running 'ollama list' command.
Returns a list of model names without @ prefix."
  (let ((output (shell-command-to-string "ollama list")))
    (when (and output (not (string-empty-p (string-trim output))))
      (let ((lines (split-string output "\n" t))
            (models '()))
        (dolist (line lines)
          ;; Skip header line
          (unless (string-match-p "^NAME" line)
            (when (string-match "^\\([a-zA-Z0-9_.:-]+\\)" line)
              (push (match-string 1 line) models))))
        (nreverse models)))))

(defun superchat-sync-ollama-models ()
  "List locally available Ollama models.
In v0.5+ llm.el handles Ollama model discovery automatically; this
command is kept for users who want to verify which local models the
`ollama list' command reports. It no longer mutates backend state."
  (interactive)
  (let ((ollama-models (superchat--get-ollama-models)))
    (if ollama-models
        (progn
          (message "Found %d Ollama models: %s"
                   (length ollama-models)
                   (mapconcat #'identity ollama-models ", "))
          (message "Available models: %s"
                   (mapconcat (lambda (m) (format "@%s" m))
                              ollama-models ", "))
          ollama-models)
      (message "⚠️ No Ollama models found or ollama command not available")
      nil)))

(defun superchat--get-available-models ()
  "Get list of available model IDs (without @ prefix).
First tries manual configuration, then `llm-models' generic on the
backend, then the configured chat-model via the
`superchat--provider-chat-model' accessor, then nil.

Results are cached for `superchat--model-list-cache-ttl' seconds
(default 300 = 5 minutes) to avoid repeated blocking network calls."
  (cond
   ;; 1. Manual override (fast path)
   (superchat-manual-models
    (copy-sequence superchat-manual-models))
   ;; 2. Check cache first
   ((and superchat--model-list-cache
         (consp superchat--model-list-cache)
         (or (null superchat--model-list-cache-ttl)
             (< (- (float-time) (cdr superchat--model-list-cache))
                superchat--model-list-cache-ttl)))
    (copy-sequence (car superchat--model-list-cache)))
   ;; 3. Backend's `:chat-model' via `llm-models' cl-defgeneric
   ;;    NOTE: llm-models makes a synchronous HTTP request for real backends.
   ;;    We call it ONCE (not twice as before) and cache the result.
   ((and superchat-llm-backend
         (fboundp 'llm-models))
    (let ((models (condition-case nil
                      (llm-models superchat-llm-backend)
                    (error nil))))
      (when (listp models)
        (setq superchat--model-list-cache
              (cons (copy-sequence models) (float-time))))
      (and (listp models) (copy-sequence models))))
   ;; 4. Fall back to the configured chat-model via the accessor
   ((and superchat-llm-backend
         (fboundp 'superchat--provider-chat-model)
         (condition-case nil
             (superchat--provider-chat-model superchat-llm-backend)
           (error nil)))
    (let ((models (list (superchat--provider-chat-model superchat-llm-backend))))
      (setq superchat--model-list-cache
            (cons (copy-sequence models) (float-time)))
      models))
   ;; 5. Empty
   (t nil)))

(defun superchat-refresh-models ()
  "Invalidate the model list cache and re-fetch available models.
Use this when you've changed backends, installed new models locally,
or want to force a fresh model list from the provider API."
  (interactive)
  (superchat--invalidate-model-cache)
  (let ((models (superchat--get-available-models)))
    (message "Superchat: model list refreshed (%d models)"
             (if models (length models) 0))
    models))

(defun superchat-model-list ()
  "Show available models for @ syntax, sourced from `superchat-llm-backend'."
  (interactive)
  (let* ((backend superchat-llm-backend)
         (configured-model (and (fboundp 'superchat--provider-chat-model)
                                (condition-case nil
                                    (superchat--provider-chat-model backend)
                                  (error nil))))
         (override-model superchat-llm-model)
         (current-model (or override-model configured-model "unknown"))
         (is-ollama (and backend
                         (string-match-p "ollama"
                                         (downcase (or (and (fboundp 'llm-name)
                                                             (let ((name (ignore-errors (llm-name backend))))
                                                               (cond
                                                                ((stringp name) name)
                                                                ((symbolp name) (symbol-name name))
                                                                (t (format "%s" name)))))
                                                        (format "%s" backend))))))
         (raw-models (or superchat-manual-models
                         (and (fboundp 'superchat--get-available-models)
                              (superchat--get-available-models))
                         '()))
         (models (mapcar (lambda (model)
                           (concat "@" (if (symbolp model) (symbol-name model) model)))
                         raw-models))
         (model-source (cond
                         (superchat-manual-models "Manual configuration")
                         ((and backend (fboundp 'llm-models)
                               (ignore-errors (llm-models backend)))
                          "llm.el `llm-models' generic")
                         (backend "Configured llm backend chat-model")
                         (t "Default fallback"))))
    (let ((content
           (concat
            (format "Available Models for @ Syntax\n\n")
            (format "Model source: %s\n" model-source)
            (format "Current model: %s\n" current-model)
            (when is-ollama
              (format "Ollama detected: %s\n" "yes"))
            "\nUsage: @model_name\n"
            "Example: @gpt-4o-mini Hello, how are you?\n\n"
            (if models
                (concat "Available models:\n"
                        (mapconcat (lambda (model)
                                     (format "  %s" model))
                                   models "\n")
                        "\n\n")
              (concat
               "No models available. Either:\n"
               "  - Set `superchat-manual-models' to a list of model IDs, or\n"
               "  - Configure a backend that supports `(llm-models PROVIDER)'.\n\n")))))
      (when (called-interactively-p 'interactive)
        (with-help-window "*SuperChat Models*"
          (with-current-buffer standard-output
            (insert content))))
      content)))


(provide 'superchat-models)
;;; superchat-models.el ends here
