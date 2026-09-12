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
(require 'superchat-plugin)
(require 'superchat-service)
(require 'ob-core nil t)

;; ── Forward declarations ──
(defvar superchat-llm-backend)
(declare-function org-babel-expand-body:generic "ob-core" (body params &optional processed-params))
(declare-function org-entry-get "org" (pom property &optional inherit literal-nil))
(declare-function superchat-skills-load "superchat-skills" (skill-name))
(declare-function superchat-preset-skill-body "superchat-preset" (preset))

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
      (let ((preset (condition-case nil
                        (superchat-skills-load
                         (if (symbolp skill) (symbol-name skill) skill))
                      (error nil))))
        (if preset
            (let ((skill-body (superchat-preset-skill-body preset)))
              (if (stringp skill-body)
                  (concat skill-body "\n\n" body)
                body))
          body))
    body))

(defun ob-superchat--call-llm (query system-prompt &optional model)
  "Send QUERY to the LLM (blocking, no tools) and return the response.
MODEL is an optional one-shot model override."
  (let* ((buffer (current-buffer))
         (session-id
          (or (superchat-service-call nil 'editor-workspace 'owner-session buffer)
              (format "org-babel-%s" (gensym)))))
    (superchat-service-call nil 'editor-workspace 'validate-buffer buffer session-id)
    (condition-case err
      (superchat-service-call nil 'driver 'submit-blocking
       (append
        (list :surface 'org-babel
              :content query
              :session-id session-id
              :context (list :host-buffer buffer)
              :tools 'none
              :runtime-extras '(:record-conversation nil)
              :origin (list :surface 'org-babel
                            :buffer (buffer-name)
                            :file buffer-file-name
                            :major-mode major-mode)
              :timeout ob-superchat-timeout)
        (when (and system-prompt (not (string-empty-p system-prompt)))
          (list :system-prompt system-prompt))
        (when model
          (list :target-model model))))
      (error
       (format "[Superchat error: %s]" (error-message-string err))))))

(provide 'ob-superchat)
;;; ob-superchat.el ends here
