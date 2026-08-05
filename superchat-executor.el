;;; superchat-executor.el --- Prompt Execution Engine for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Core execution engine for Superchat.
;;
;; Provides:
;; - Prompt variable substitution ($input, $lang)
;; - Context management and injection
;;
;; Execution itself lives behind `superchat-runtime-submit'; this module
;; deliberately has no model transport or blocking Runtime entry.
;;
;; This module is used by:
;; - superchat-skills.el (Agentic Skills)
;; - superchat.el (main chat interface)

;;; Code:

(require 'cl-lib)

(defvar superchat-executor--current-lang "English"
  "Current language setting for prompts.")

(cl-defun superchat-executor-initialize (&key lang &allow-other-keys)
  "Initialize the execution engine.
LANG is the default language for variable substitution.
Unknown keys are accepted for compatibility with the former LLM injector."
  (when lang
    (setq superchat-executor--current-lang lang)))

;;;-----------------------------------------------
;;; Variable Substitution
;;;-----------------------------------------------

(defun superchat-executor-replace-variables (prompt &optional input lang context)
  "Replace template variables in PROMPT.

Supported variables:
  $input - Replaced with INPUT or extracted from CONTEXT
  $lang  - Replaced with LANG or current language setting

Return the processed prompt string."
  (let* ((current-lang (or lang superchat-executor--current-lang "English"))
         (actual-input (or input
                           (when (superchat-executor-context-p context)
                             (superchat-executor-context-user-input context))
                           ""))
         (result prompt))
    ;; Replace $input
    (setq result (replace-regexp-in-string "\\$input" actual-input result))
    ;; Replace $lang
    (setq result (replace-regexp-in-string "\\$lang" current-lang result))
    result))

;;;-----------------------------------------------
;;; Context Management
;;;-----------------------------------------------

(cl-defstruct superchat-executor-context
  "Execution context for a conversation or skill invocation."
  (id "")
  (user-input ""))

(defun superchat-executor-context-create (&optional id user-input)
  "Create a new execution context.
ID: Optional identifier for this context.
USER-INPUT: The original user input."
  (make-superchat-executor-context
   :id (or id (format "ctx-%d" (random 10000)))
   :user-input (or user-input "")))

;;;-----------------------------------------------
;;; Context Injection
;;;-----------------------------------------------

(defun superchat-executor-wrap-with-context (prompt context-content)
  "Wrap PROMPT with CONTEXT-CONTENT.

The context content is prepended with a clear delimiter.
Return the combined prompt."
  (if (and context-content (not (string-empty-p context-content)))
      (concat "[Context Instructions]\n"
              "=======================\n"
              context-content
              "\n=======================\n\n"
              "[User Request]\n"
              prompt)
    prompt))

(provide 'superchat-executor)

;;; superchat-executor.el ends here
