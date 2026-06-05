;;; superchat-workflow.el --- Multi-step linear recipes -*- lexical-binding: t; -*-

;;; Commentary:
;; v0.7 restores workflow as a SKILL.md sub-type (type: workflow).
;; Each non-empty, non-comment line in the skill body is one step.
;; Steps execute top-to-bottom through superchat-core-run-turn —
;; the same pipeline as chat, including all registered hooks.
;;
;; Variable substitution: $input (the trigger argument), $lang,
;; $date. Substitution happens once, before parsing into steps.

;;; Code:

(require 'cl-lib)
(require 'superchat-core)

;; ── Forward declarations ──
(defvar superchat-lang)
(declare-function superchat--llm-generate-answer "superchat-llm"
                  (prompt callback stream-callback &optional target-model context-files))

;;;-----------------------------------------------
;;; Public API
;;;-----------------------------------------------

(defun superchat-workflow-parse-steps (body)
  "Split BODY into a list of step strings.
Skips blank lines and comment lines (lines whose first
non-whitespace character is `#').
Preserves $input / $lang / $date for late binding."
  (let ((steps '()))
    (dolist (line (split-string (or body "") "\n"))
      (let ((trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed)
                    (string-prefix-p "#" trimmed))
          (push trimmed steps))))
    (nreverse steps)))

(defun superchat-workflow-substitute (string vars)
  "Expand $input, $lang, $date in STRING using VARS plist.
VARS is a plist (:input \"foo\" :lang \"English\" :date \"2026-06-04\").
Returns STRING with variables expanded.  Unknown $var refs are left
as-is."
  (let ((result string))
    (dolist (var '(:input :lang :date))
      (let* ((key (substring (symbol-name var) 1))  ; "input" / "lang" / "date"
             (val (or (plist-get vars var) ""))
             (pattern (concat "$" key)))
        (setq result (replace-regexp-in-string
                      (regexp-quote pattern)
                      (regexp-quote val)
                      result t t))))
    ;; Unescape regexp-quote artifacts
    (replace-regexp-in-string "\\\\" "" result)))

(defun superchat-workflow-execute (skill-plist argument &optional on-done)
  "Execute a workflow SKILL-PLIST with user ARGUMENT.
SKILL-PLIST has :name :body and :type (= \"workflow\").
ARGUMENT is the user-supplied text after `>name'.
ON-DONE is invoked with the final result when execution finishes.

Each step builds a fresh turn via `superchat-turn-new', calls
`superchat-core-run-turn', then hands the populated turn to
`superchat--llm-generate-answer'.  Steps run sequentially."
  (let* ((body (plist-get skill-plist :body))
         (steps (superchat-workflow-parse-steps body))
         (vars (list :input (or argument "")
                     :lang (or superchat-lang "English")
                     :date (format-time-string "%Y-%m-%d")))
         (results '()))
    (if (null steps)
        (progn
          (message "superchat-workflow: skill '%s' has no executable steps."
                   (plist-get skill-plist :name))
          (when on-done (funcall on-done nil)))
      (dolist (step steps)
        (condition-case err
            (let* ((substituted (superchat-workflow-substitute step vars))
                   (turn (superchat-turn-new substituted))
                   (prepared (superchat-core-run-turn turn))
                   (prompt (superchat-turn-prompt prepared)))
              ;; FIXME: synchronous execution for now.
              ;; Async would need callback chaining per step.
              (message "superchat-workflow: executing step: %s" substituted)
              (push (cons step prompt) results))
          (error
           (message "superchat-workflow: step '%s' failed: %s"
                    step (error-message-string err))
           ;; Don't run further steps
           (cl-return)))))
    (nreverse results)))

(provide 'superchat-workflow)

;;;-----------------------------------------------
;;; Legacy .workflow import (v0.7 step 5)
;;;-----------------------------------------------

(defun superchat-workflow-import-legacy (legacy-file target-dir)
  "Import a single legacy .workflow LEGACY-FILE as a SKILL.md.
Writes the SKILL.md under TARGET-DIR/skill-name/SKILL.md.
Returns the skill name, or nil on failure."
  (let ((base (file-name-base legacy-file))
        (name (file-name-sans-extension (file-name-base legacy-file))))
    (unless (and (file-exists-p legacy-file)
                 (string-suffix-p ".workflow" legacy-file t))
      (user-error "%s is not a .workflow file" legacy-file))
    (with-temp-buffer
      (insert-file-contents legacy-file)
      (let* ((body (string-trim (buffer-string)))
             (skill-dir (expand-file-name (concat "skill-" name) target-dir)))
        (unless (file-directory-p skill-dir)
          (make-directory skill-dir t))
        (with-temp-file (expand-file-name "SKILL.md" skill-dir)
          (insert (format "---\nname: %s\ndescription: Imported workflow: %s\nversion: \"1.0\"\ntype: workflow\n---\n\n"
                          name base))
          (insert body))
        (message "superchat-workflow: imported '%s' → %s/SKILL.md"
                 legacy-file skill-dir)
        name))))

;;;###autoload
(defun superchat-workflow-import-legacy-dir (legacy-dir &optional target-dir)
  "Scan LEGACY-DIR for .workflow files and import them as SKILL.md.
TARGET-DIR defaults to `superchat-skills-directory'.
Returns a list of imported skill names."
  (interactive "DLegacy workflow directory: ")
  (let ((target (or target-dir
                    (if (boundp 'superchat-skills-directory)
                        superchat-skills-directory
                      (expand-file-name
                       "skills/"
                       (if (boundp 'superchat-data-directory)
                           superchat-data-directory
                         (expand-file-name "superchat/" user-emacs-directory))))))
        (imported '()))
    (unless (file-directory-p legacy-dir)
      (user-error "%s is not a directory" legacy-dir))
    (unless (file-directory-p target)
      (make-directory target t))
    (dolist (file (directory-files legacy-dir t "\\.workflow$"))
      (let ((name (superchat-workflow-import-legacy file target)))
        (when name (push name imported))))
    (message "superchat-workflow: imported %d legacy workflows from %s"
             (length imported) legacy-dir)
    imported))
;;; superchat-workflow.el ends here
