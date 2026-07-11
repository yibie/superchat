;;; superchat-magit.el --- Magit integration for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit hook: generate conventional-commit messages from staged diff
;; via a deterministic blocking LLM call (no tools).  Soft-depends on
;; Magit — the command is only registered when Magit is loaded.

;;; Code:

(require 'cl-lib)

;; ── Forward declarations ──
(defvar superchat-llm-backend)
(declare-function magit-staged-files "magit" ())
(declare-function magit-diff-staged "magit" (&optional files))
(declare-function magit-git-string "magit" (&rest args))
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function llm-make-chat-prompt "llm" (text &rest args))
(declare-function llm-chat "llm" (provider prompt &optional multi-output))

(defgroup superchat-magit nil
  "Magit integration for Superchat."
  :group 'superchat)

(defcustom superchat-magit-commit-style 'conventional
  "Style of commit message to generate.
`conventional' — Conventional Commits (feat:, fix:, etc.).
`freeform'   — free-form one-liner."
  :type '(choice (const :tag "Conventional Commits" conventional)
                 (const :tag "Free-form" freeform))
  :group 'superchat-magit)

(defcustom superchat-magit-commit-system-prompt
  "You are an expert commit message writer. Generate exactly one commit \
message from the provided git diff. Return ONLY the commit message \
text, no explanations, no markdown fences, no commentary."
  "System prompt for commit message generation."
  :type 'string
  :group 'superchat-magit)

;;;###autoload
(defun superchat-magit-commit-message ()
  "Generate a commit message from the staged git diff.
Uses the LLM (no tool-calling) to produce a conventional-commit
message.  If inside a `git-commit-mode' buffer, inserts the
message at point; otherwise echoes it to the echo area for review.

Requires Magit to be loaded and `superchat-llm-backend' to be
configured."
  (interactive)
  (unless (fboundp 'magit-staged-files)
    (user-error "Magit is not loaded — cannot read staged diff"))
  (unless superchat-llm-backend
    (user-error "superchat-llm-backend is not configured"))
  (let* ((staged (magit-staged-files))
         (diff-text
          (if staged
              (condition-case nil
                  (if (fboundp 'magit-diff-staged)
                      (magit-diff-staged staged)
                    ;; Fallback: magit-git-string
                    (apply #'magit-git-string "diff" "--cached" staged))
                (error nil))
            (user-error "No staged changes")))
         (prompt (superchat-magit--build-prompt diff-text))
         (message-text (superchat-magit--call-llm prompt)))
    (if (and message-text (not (string-empty-p message-text)))
        (superchat-magit--deliver message-text)
      (user-error "LLM returned no commit message"))))

(defun superchat-magit--build-prompt (diff)
  "Build the LLM prompt from DIFF."
  (let ((style-hint
         (if (eq superchat-magit-commit-style 'conventional)
             "Write a conventional-commit message (e.g., `feat: ...`, `fix: ...`, `refactor: ...`). \
Use the imperative mood. Keep the subject line under 72 characters."
           "Write a concise, imperative-mood commit message under 72 characters.")))
    (format "%s\n\nGit diff:\n%s"
            style-hint diff)))

(defun superchat-magit--call-llm (prompt)
  "Send PROMPT to the LLM (blocking, no tools) and return the text.
Returns the trimmed response, or nil on failure."
  (unless (fboundp 'llm-make-chat-prompt)
    (error "llm.el is not available"))
  (let* ((effective-backend
          (if (fboundp 'superchat--effective-llm-backend)
              (funcall 'superchat--effective-llm-backend)
            superchat-llm-backend))
         (system-prompt superchat-magit-commit-system-prompt)
         ;; system prompt goes through `:context'; never overwrite
         ;; `interactions' with a plist (see superchat-rewrite.el).
         (full-prompt (llm-make-chat-prompt prompt :context system-prompt))
         (result-text nil))
    (condition-case err
        (let ((result (llm-chat effective-backend full-prompt)))
          (setq result-text
                (cond
                 ((stringp result) result)
                 ((and (consp result) (stringp (plist-get result :text)))
                  (plist-get result :text))
                 (t (format "%S" result)))))
      (error
       (user-error "LLM call failed: %s" (error-message-string err))))
    (when (stringp result-text)
      (string-trim result-text))))

(defun superchat-magit--deliver (message-text)
  "Deliver MESSAGE-TEXT to the appropriate location.
If in a `git-commit-mode' buffer, insert at point (undo-able).
Otherwise, echo in the echo area."
  (if (and (fboundp 'git-commit-mode)
           (derived-mode-p 'git-commit-mode))
      (progn
        (atomic-change-group
          (insert message-text "\n"))
        (message "Commit message inserted (undo to revert)."))
    (message "Superchat commit message:\n%s" message-text)))

(provide 'superchat-magit)
;;; superchat-magit.el ends here
