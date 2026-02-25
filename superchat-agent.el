;;; superchat-agent.el --- Bridge to gptel-agent agentic mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This module bridges superchat with gptel-agent, providing a modal
;; architecture where a superchat buffer can switch between normal mode
;; (superchat's own tools) and agent mode (gptel-agent's full toolset
;; including async Bash, diff/patch Edit, Grep, sub-agents, TodoWrite,
;; and overlay previews).

;;; Code:

(require 'cl-lib)

;; Forward declarations -- these are defined in gptel / gptel-agent
(defvar gptel-agent--agents)
(defvar gptel-tools)
(defvar gptel--system-message)

(declare-function gptel-agent-update "gptel-agent" ())
(declare-function gptel-tool-category "gptel-request" (tool))

;; Forward declarations for superchat functions used in prompt building
(declare-function superchat--normalize-file-path "superchat" (path))
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--read-inline-file-content "superchat" (file-path))
(declare-function superchat--render-inline-context "superchat" (file-path content))
(declare-function superchat--textual-file-p "superchat" (path))
(declare-function superchat--conversation-context-string "superchat" (n))
(declare-function superchat--format-retrieved-memories "superchat" (memories))

(defvar superchat--retrieved-memory-context)
(defvar superchat--file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-context-message-count)
(defvar superchat-lang)

;;; --- Buffer-local state ---

(defvar-local superchat-agent-mode nil
  "Non-nil when this superchat buffer is in agent mode.")

(defvar-local superchat-agent--saved-system-message nil
  "Saved system message from before entering agent mode.")

;;; --- Lazy initialization ---

(defvar superchat-agent--initialized nil
  "Non-nil after gptel-agent has been loaded and initialized.")

(defun superchat-agent-initialize ()
  "Load gptel-agent, update agents/skills, register tools.
This is called once on first use and is idempotent."
  (unless superchat-agent--initialized
    (require 'gptel-agent)
    (require 'gptel-agent-tools)
    (require 'gptel-agent-tools-introspection nil t)
    (gptel-agent-update)
    (setq superchat-agent--initialized t)))

;;; --- Tool collection ---

(defun superchat-agent--get-tools ()
  "Return tools for agent mode.
Includes gptel-agent tools, introspection tools, and superchat's
emacs/edit category tools.  Excludes superchat's system/filesystem
category tools (replaced by gptel-agent's Bash, Read, Write, Grep etc)."
  (when (boundp 'gptel-tools)
    (let ((excluded-categories '("system" "filesystem")))
      (cl-remove-if
       (lambda (tool)
         (let ((cat (gptel-tool-category tool)))
           (member cat excluded-categories)))
       gptel-tools))))

;;; --- System prompt ---

(defun superchat-agent--get-system-prompt ()
  "Return gptel-agent's system prompt from the parsed agent definitions.
Falls back to nil if no agent definition is found."
  (when (boundp 'gptel-agent--agents)
    (plist-get (cdr (assoc "gptel-agent" gptel-agent--agents)) :system)))

;;; --- Prompt building ---

(defun superchat-agent--build-prompt (input &optional _lang)
  "Build prompt for agent mode from INPUT.
Reuses superchat's file extraction and memory/conversation context
logic, but does NOT wrap with the `superchat-general-answer-prompt'
template (the agent system prompt already contains role instructions).
Returns a plist (:prompt PROMPT :user-message USER-MSG)."
  (let* ((memory-context superchat--retrieved-memory-context)
         (_ (setq superchat--retrieved-memory-context nil))
         (initial-query (string-trim (or input "")))
         ;; Extract file reference if present
         (file-path
          (when (string-match superchat--file-ref-regexp initial-query)
            (superchat--normalize-file-path
             (or (match-string 1 initial-query)
                 (match-string 2 initial-query)))))
         (user-query
          (if file-path
              (let* ((path-start (match-beginning 0))
                     (path-end (match-end 0))
                     (text-before (substring initial-query 0 path-start))
                     (text-after (substring initial-query path-end)))
                (string-trim (concat (string-trim text-before) " " (string-trim text-after))))
            initial-query))
         (file-content
          (when file-path
            (when (and (file-exists-p file-path)
                       (superchat--textual-file-p file-path))
              (superchat--read-inline-file-content file-path))))
         (effective-user-query
          (cond
           ((and (string-empty-p user-query) file-content)
            (format "File: %s\n\n%s" file-path file-content))
           (t user-query)))
         (inline-context
          (when file-path
            (superchat--add-file-to-context file-path)
            (when (and superchat-inline-file-content
                       (not (and (string-empty-p user-query) file-content)))
              (superchat--render-inline-context file-path file-content))))
         (conversation-context
          (superchat--conversation-context-string superchat-context-message-count))
         ;; Build sections without template wrapping
         (sections (delq nil (list (unless (string-empty-p (or memory-context "")) memory-context)
                                   inline-context
                                   conversation-context
                                   effective-user-query)))
         (final-prompt-string (mapconcat #'identity sections "\n\n")))
    (when (and file-path (not (file-exists-p file-path)))
      (message "Warning: Referenced file does not exist: %s" file-path))
    (list :prompt final-prompt-string
          :user-message (unless (string-empty-p user-query) user-query))))

;;; --- Toggle command ---

(defun superchat-agent-toggle ()
  "Toggle agent mode in the current superchat buffer.
Returns a superchat result plist for display."
  (superchat-agent-initialize)
  (if superchat-agent-mode
      (superchat-agent--deactivate)
    (superchat-agent--activate)))

(defun superchat-agent--activate ()
  "Activate agent mode: swap system prompt, return echo plist."
  (setq superchat-agent-mode t)
  (setq superchat-agent--saved-system-message
        (buffer-local-value 'gptel--system-message (current-buffer)))
  (let ((agent-system (superchat-agent--get-system-prompt)))
    (when agent-system
      (setq-local gptel--system-message agent-system)))
  (setq-local superchat--timeout-extension-amount 120)
  '(:type :echo :content "Agent mode ON. Tools: gptel-agent (Bash, Read, Write, Edit, Grep, Glob, Agent, TodoWrite). Sub-agents: researcher/introspector."))

(defun superchat-agent--deactivate ()
  "Deactivate agent mode: restore system prompt, return echo plist."
  (setq superchat-agent-mode nil)
  (setq-local gptel--system-message superchat-agent--saved-system-message)
  (setq-local superchat--timeout-extension-amount 30)
  '(:type :echo :content "Agent mode OFF. Using default superchat tools."))

(provide 'superchat-agent)
;;; superchat-agent.el ends here
