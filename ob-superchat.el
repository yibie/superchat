;;; ob-superchat.el --- Org-babel integration for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Enables `#+begin_src superchat ... #+end_src' in any Org buffer.
;; The body is sent as a query to the LLM and the result is written
;; to `#+RESULTS:'.
;;
;; Supported header-args:
;;   :model  — one-shot model override (maps to @model in chat)
;;   :skill  — skill name to inject as context (maps to >skill in chat)
;;   :system — custom system prompt for this block

;;; Code:

(require 'cl-lib)
(require 'ob-core nil t)

;; ── Forward declarations ──
(defvar superchat-llm-backend)
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function superchat--build-llm-prompt "superchat-llm" (text tools))
(declare-function llm-make-chat-prompt "llm" (text &rest args))
(declare-function llm-make-context "llm" (&rest args))
(declare-function llm-chat "llm" (provider prompt &optional multi-output))
(declare-function llm-chat-prompt-context "llm" (prompt))
(declare-function llm-chat-prompt-interactions "llm" (prompt))
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional processed-params))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))

(defgroup ob-superchat nil
  "Org-babel integration for Superchat."
  :group 'org-babel
  :group 'superchat)

(defcustom ob-superchat-system-prompt ""
  "Default system prompt for `#+begin_src superchat' blocks.
Can be overridden per-block with the :system header-arg."
  :type 'string
  :group 'ob-superchat)

(defcustom ob-superchat-timeout 120
  "Maximum seconds to wait for an LLM response from an org-babel block."
  :type 'integer
  :group 'ob-superchat)

;;;###autoload
(defun org-babel-execute:superchat (body params)
  "Execute a superchat org-babel source block.
BODY is the query text.  PARAMS is a plist of header-args:
  :model   — model override
  :skill   — skill name (loads context from skills directory)
  :system  — custom system prompt

Returns the LLM response as a string for `#+RESULTS:'."
  (unless superchat-llm-backend
    (user-error "superchat-llm-backend is not configured"))
  (unless (fboundp 'llm-make-chat-prompt)
    (user-error "llm.el is not loaded — cannot execute superchat block"))
  (let* ((model (cdr (assq :model params)))
         (skill (cdr (assq :skill params)))
         (system-prompt (or (cdr (assq :system params))
                            ob-superchat-system-prompt))
         ;; Expand header-vars and noweb references in the body
         (expanded-body (if (fboundp 'org-babel-expand-body:generic)
                            (org-babel-expand-body:generic body params)
                          body))
         ;; If a skill is specified, load its content and prepend.
         (query (ob-superchat--build-query expanded-body skill))
         (result (ob-superchat--call-llm query system-prompt model)))
    (if result
        (string-trim result)
      "[Superchat: no response]")))

(defun ob-superchat--build-query (body skill)
  "Build the full prompt by prepending skill context to BODY when SKILL is set."
  (if (and skill (fboundp 'superchat-skills-load))
      (let ((skill-plist (condition-case nil
                             (superchat-skills-load (symbol-name skill))
                           (error nil))))
        (if skill-plist
            (let ((skill-body (plist-get skill-plist :body)))
              (if (stringp skill-body)
                  (concat skill-body "\n\n" body)
                body))
          body))
    body))

(defun ob-superchat--call-llm (query system-prompt &optional model)
  "Send QUERY to the LLM (blocking, no tools) and return the response.
MODEL is an optional one-shot model override."
  (let* ((effective-backend
          (if (and model (fboundp 'superchat--effective-llm-backend))
              (funcall 'superchat--effective-llm-backend model)
            (if (fboundp 'superchat--effective-llm-backend)
                (funcall 'superchat--effective-llm-backend)
              superchat-llm-backend)))
         (full-prompt
          (if (fboundp 'superchat--build-llm-prompt)
              (funcall 'superchat--build-llm-prompt query nil)
            (llm-make-chat-prompt query)))
         (context (llm-make-context))
         (result-text nil))
    (setf (llm-chat-prompt-context full-prompt) context)
    (when (and system-prompt (not (string-empty-p system-prompt)))
      (setf (llm-chat-prompt-interactions full-prompt)
            `((:role system :content ,system-prompt))))
    (condition-case err
        (progn
          (let ((result (llm-chat effective-backend full-prompt)))
            (setq result-text
                  (cond
                   ((stringp result) result)
                   ((and (consp result) (stringp (plist-get result :text)))
                    (plist-get result :text))
                   (t (format "%S" result))))
            result-text))
      (error
       (format "[Superchat error: %s]" (error-message-string err))))))

(provide 'ob-superchat)
;;; ob-superchat.el ends here
