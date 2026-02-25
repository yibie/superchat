;;; superchat-executor.el --- Prompt Execution Engine for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Core execution engine for Superchat.
;;
;; Provides:
;; - Prompt variable substitution ($input, $lang)
;; - Context management and injection
;; - LLM execution wrapper
;; - Result protocol (unified success/failure handling)
;; - File context processing
;;
;; This module is used by:
;; - superchat-skills.el (Agentic Skills)
;; - superchat.el (main chat interface)

;;; Code:

(require 'cl-lib)

;;;-----------------------------------------------
;;; Dependency Injection
;;;-----------------------------------------------

(defvar superchat-executor--llm-executor nil
  "Function to execute LLM prompts.
Passed from superchat.el. Accepts prompt string and optional target-model.")

(defvar superchat-executor--current-lang "English"
  "Current language setting for prompts.")

(cl-defun superchat-executor-initialize (&key llm-executor lang)
  "Initialize the execution engine.
LLM-EXECUTOR: Function to execute LLM prompts.
LANG: Default language for variable substitution."
  (setq superchat-executor--llm-executor llm-executor)
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

Returns the processed prompt string."
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
  (id "")                      ; Unique identifier
  (user-input "")              ; Original user input
  (variables (make-hash-table :test 'equal))  ; Variable storage
  (results '())                ; List of results
  (metadata '()))              ; Additional metadata

(defun superchat-executor-context-create (&optional id user-input)
  "Create a new execution context.
ID: Optional identifier for this context.
USER-INPUT: The original user input."
  (make-superchat-executor-context
   :id (or id (format "ctx-%d" (random 10000)))
   :user-input (or user-input "")
   :variables (make-hash-table :test 'equal)
   :results '()
   :metadata '()))

(defun superchat-executor-context-set-variable (context name value)
  "Set a variable in CONTEXT."
  (puthash name value (superchat-executor-context-variables context)))

(defun superchat-executor-context-get-variable (context name)
  "Get a variable from CONTEXT."
  (gethash name (superchat-executor-context-variables context)))

;;;-----------------------------------------------
;;; Result Protocol
;;;-----------------------------------------------

(cl-defstruct superchat-executor-result
  "Unified result structure for execution operations."
  (success t)                  ; Boolean success status
  (data "")                    ; Result data on success
  (error nil)                  ; Error message on failure
  (elapsed 0.0)                ; Execution time in seconds
  (source nil))                ; Error source (:model :tool :network :system)

(defun superchat-executor-result-success (data &optional elapsed)
  "Create a successful result."
  (make-superchat-executor-result
   :success t
   :data data
   :elapsed (or elapsed 0.0)))

(defun superchat-executor-result-failure (error-msg &optional source elapsed)
  "Create a failure result."
  (make-superchat-executor-result
   :success nil
   :error error-msg
   :source (or source :system)
   :elapsed (or elapsed 0.0)))

;;;-----------------------------------------------
;;; LLM Execution
;;;-----------------------------------------------

(defun superchat-executor-call-llm (prompt &optional target-model context)
  "Execute LLM with given prompt.

PROMPT: The prompt string to send
TARGET-MODEL: Optional model override
CONTEXT: Optional execution context for variable substitution

Returns the LLM response string or nil on failure."
  (when superchat-executor--llm-executor
    (let ((processed-prompt
           (if context
               (superchat-executor-replace-variables
                prompt
                (superchat-executor-context-user-input context)
                superchat-executor--current-lang
                context)
             prompt)))
      (funcall superchat-executor--llm-executor processed-prompt target-model))))

;;;-----------------------------------------------
;;; Context Injection
;;;-----------------------------------------------

(defun superchat-executor-wrap-with-context (prompt context-content)
  "Wrap PROMPT with CONTEXT-CONTENT.

The context content is prepended with a clear delimiter.
Returns the combined prompt."
  (if (and context-content (not (string-empty-p context-content)))
      (concat "[Context Instructions]\n"
              "=======================\n"
              context-content
              "\n=======================\n\n"
              "[User Request]\n"
              prompt)
    prompt))

;;;-----------------------------------------------
;;; Smart Context Building (from workflow heritage)
;;;-----------------------------------------------

(cl-defun superchat-executor-build-context-summary (context &key max-length)
  "Build a smart context summary to avoid LLM overload.

CONTEXT: Execution context
MAX-LENGTH: Maximum length (default 2000 chars)

Returns optimized context summary string."
  (let* ((max-len (or max-length 2000))
         (ctx context)
         (parts '()))
    ;; Add basic context info
    (push (format "Context: %s" (superchat-executor-context-id ctx)) parts)
    ;; Add user input if available
    (let ((user-input (superchat-executor-context-user-input ctx)))
      (when (and user-input (not (string-empty-p user-input)))
        (push (format "User Input: %s"
                      (if (> (length user-input) 200)
                          (concat (substring user-input 0 200) "...")
                        user-input))
              parts)))
    ;; Combine and limit
    (let* ((full (string-join (nreverse parts) "\n"))
           (trimmed (if (> (length full) max-len)
                        (concat (substring full 0 max-len) "...")
                      full)))
      trimmed)))

;;;-----------------------------------------------
;;; Utility Functions
;;;-----------------------------------------------

(defun superchat-executor-sanitize-string (string)
  "Sanitize STRING for safe display.
Removes or replaces problematic control characters."
  (if (stringp string)
      (let ((clean string))
        ;; Replace null bytes
        (setq clean (replace-regexp-in-string "\x00" "" clean))
        ;; Replace other control chars except newlines and tabs
        (setq clean (replace-regexp-in-string "[\x01-\x08\x0b\x0c\x0e-\x1f]" "" clean))
        clean)
    ""))

(defun superchat-executor-extract-content (result)
  "Extract string content from various result types.
Handles strings, executor-result structs, and other types."
  (cond
   ;; Already a string
   ((stringp result) result)
   ;; Executor result struct
   ((superchat-executor-result-p result)
    (superchat-executor-result-data result))
   ;; Fallback: convert to string
   (t (format "%s" result))))

(provide 'superchat-executor)

;;; superchat-executor.el ends here
