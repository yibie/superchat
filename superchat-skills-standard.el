;;; superchat-skills-standard.el --- Standard Skill Format Support -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Support for standard skill formats (OpenAI Codex, Claude, etc.)
;;
;; Converts standard skill directories to superchat's internal format.
;;
;; Standard skill structure:
;;   skill-name/
;;   ├── SKILL.md          (required)
;;   ├── scripts/          (optional)
;;   └── references/       (optional)

;;; Code:

(require 'cl-lib)
(require 'superchat-skills)
(require 'superchat-preset)

;;;-----------------------------------------------
;;; Configuration
;;;-----------------------------------------------

(defcustom superchat-skills-standard-directories
  (list (expand-file-name ".agents/skills/" default-directory)
        (expand-file-name "~/.agents/skills/"))
  "List of directories to search for standard-format skills."
  :type '(repeat directory)
  :group 'superchat-skills)

(defcustom superchat-skills-support-standard-formats t
  "Whether to support standard skill formats (OpenAI, Claude, etc.)."
  :type 'boolean
  :group 'superchat-skills)

;;;-----------------------------------------------
;;; Standard Skill Detection
;;;-----------------------------------------------

(defun superchat-skills-standard--find-skills (directory)
  "Find all standard-format skills in DIRECTORY.
Returns list of skill directory paths."
  (when (file-directory-p directory)
    (let (skills)
      (dolist (entry (directory-files directory t "^[^.]"))
        (when (and (file-directory-p entry)
                   (file-exists-p (expand-file-name "SKILL.md" entry)))
          (push entry skills)))
      (nreverse skills))))

(defun superchat-skills-standard--load-metadata (skill-dir)
  "Load and validate metadata from SKILL-DIR's SKILL.md frontmatter.
Return a plist with :name :description :type :version :triggers
plus :directory and :file, or nil if validation fails (with warning)."
  (let* ((skill-file (expand-file-name "SKILL.md" skill-dir))
         (content (when (file-exists-p skill-file)
                    (with-temp-buffer
                      (insert-file-contents skill-file)
                      (buffer-string)))))
    (when content
      (let ((alist (superchat-preset-parse-frontmatter content)))
        (if (null alist)
            ;; No frontmatter — use directory basename as name, empty description
            (list :name (file-name-nondirectory skill-dir)
                  :description ""
                  :type "prompt"
                  :version "1.0"
                  :triggers '()
                  :reasoning 'inherit
                  :confirm-destructive 'inherit
                  :directory skill-dir
                  :file skill-file)
          (let ((validated (superchat-skills-standard--validate alist)))
            (if (car validated)
                (append (cdr validated)
                        (list :directory skill-dir :file skill-file))
              (display-warning 'superchat-skills
                               (format "Skipping %s: %s" skill-dir (cdr validated))
                               :warning)
              nil)))))))

;;;-----------------------------------------------
;;; Frontmatter Parser
;;;-----------------------------------------------

;; Fields must be emitted in this order for round-trip fidelity.
;; See docs/handoff-v0.7-skills-workflow.md step 2.

(defconst superchat-skills-standard--field-order
  '("name" "description" "version" "type" "tools" "model"
    "temperature" "max_tokens" "reasoning" "max_tool_calls"
    "confirm_destructive" "pre" "triggers")
  "Canonical YAML key order for SKILL.md export.")

(defun superchat-skills-standard--validate (alist)
  "Validate a frontmatter ALIST for required fields.
Returns (t . plist) on success or (nil . error-string) on failure.
Required: `name', `description'.  `type' defaults to `prompt',
`version' defaults to `\"1.0\"'.  Valid types are `prompt', `agent',
`plan', and `workflow'."
  (let ((name (cdr (assoc "name" alist)))
        (desc (cdr (assoc "description" alist)))
        (ver  (or (cdr (assoc "version" alist)) "1.0"))
        (type (or (cdr (assoc "type" alist)) "prompt"))
        (trig (cdr (assoc "triggers" alist)))
        (tools (cdr (assoc "tools" alist)))
        (model (cdr (assoc "model" alist)))
        (temperature (cdr (assoc "temperature" alist)))
        (max-tokens (or (cdr (assoc "max_tokens" alist))
                        (cdr (assoc "max-tokens" alist))))
        (reasoning (or (cdr (assoc "reasoning" alist)) 'inherit))
        (max-tool-calls (or (cdr (assoc "max_tool_calls" alist))
                            (cdr (assoc "max-tool-calls" alist))))
        (confirm-destructive
         (if-let* ((entry (assoc "confirm_destructive" alist)))
             (cdr entry)
           'inherit))
        (pre (cdr (assoc "pre" alist))))
    (cond
     ((or (null name) (string-empty-p name))
      (cons nil "missing required field: name"))
     ((or (null desc) (string-empty-p desc))
      (cons nil "missing required field: description"))
     ((not (member (downcase type) '("prompt" "agent" "plan" "workflow")))
      (cons nil (format "invalid type: %s (must be prompt, agent, plan, or workflow)" type)))
     (t
      (cons t (list :name name :description desc :version ver
                    :type (downcase type)
                    :tools tools :model model
                    :temperature temperature :max-tokens max-tokens
                    :reasoning reasoning :max-tool-calls max-tool-calls
                    :confirm-destructive confirm-destructive :pre pre
                    :triggers trig))))))

;;;-----------------------------------------------
;;; Content Extraction
;;;-----------------------------------------------

(defun superchat-skills-standard--extract-content (skill-file)
  "Extract skill content from SKILL-FILE, skipping frontmatter."
  (when (file-exists-p skill-file)
    (with-temp-buffer
      (insert-file-contents skill-file)
      (let ((content (buffer-string)))
        ;; Remove YAML frontmatter if present
        (when (string-match "^---\\s-*\n\\(\\(.\\|\n\\)*\\)---\\s-*\n" content)
          (setq content (substring content (match-end 0))))
        ;; Remove HTML comments
        (setq content (replace-regexp-in-string "<!--.*?-->" "" content t t))
        ;; Clean up
        (string-trim content)))))

(defun superchat-skills-standard--load-references (skill-dir)
  "Load all reference files from SKILL-DIR's references/ directory."
  (let ((ref-dir (expand-file-name "references/" skill-dir))
        (refs '()))
    (when (file-directory-p ref-dir)
      (dolist (file (directory-files ref-dir t "\\.\\(md\\|org\\|txt\\)$"))
        (push (cons (file-name-nondirectory file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
              refs)))
    (nreverse refs)))

;;;-----------------------------------------------
;;; Conversion to Superchat Format
;;;-----------------------------------------------

(defun superchat-skills-standard--convert (standard-skill)
  "Convert STANDARD-SKILL to a `superchat-preset'.
Return plist compatible with superchat-skills functions."
  (let* ((metadata (superchat-skills-standard--load-metadata standard-skill))
         (name (plist-get metadata :name))
         (content (superchat-skills-standard--extract-content
                  (plist-get metadata :file)))
         (references (superchat-skills-standard--load-references standard-skill)))
    (superchat-preset-from-plist
     (list :name name
           :description (plist-get metadata :description)
           :type (plist-get metadata :type)
           :body (concat content
                         (when references
                           (concat "\n\n### References\n\n"
                                   (mapconcat (lambda (ref)
                                                (format "#### %s\n%s"
                                                        (car ref) (cdr ref)))
                                              references "\n\n"))))
           :tools (plist-get metadata :tools)
           :model (plist-get metadata :model)
           :temperature (plist-get metadata :temperature)
           :max-tokens (plist-get metadata :max-tokens)
           :reasoning (plist-get metadata :reasoning)
           :max-tool-calls (plist-get metadata :max-tool-calls)
           :confirm-destructive (plist-get metadata :confirm-destructive)
           :pre (plist-get metadata :pre)
           :version (plist-get metadata :version)
           :triggers (plist-get metadata :triggers)
           :source 'standard
           :source-file (plist-get metadata :file)))))

;;;-----------------------------------------------
;;; Integration with Superchat Skills
;;;-----------------------------------------------

(defun superchat-skills-standard--discover-all ()
  "Discover all standard-format skills from configured directories.
Returns list of converted skill definitions."
  (let (all-skills)
    (dolist (dir superchat-skills-standard-directories)
      (let ((skills (superchat-skills-standard--find-skills dir)))
        (dolist (skill-dir skills)
          (let ((converted (superchat-skills-standard--convert skill-dir)))
            (when converted
              (push converted all-skills))))))
    (nreverse all-skills)))

(defun superchat-skills-standard-merge-registry ()
  "Merge standard skills into superchat's skill registry.
Returns merged registry."
  (let* ((standard-skills (superchat-skills-standard--discover-all))
         (native-skills (superchat-skills--get-registry))
         (merged native-skills))
    ;; Add standard skills, preferring native skills on name conflict
    (dolist (skill standard-skills)
      (unless (assoc (if (superchat-preset-p skill)
                         (superchat-preset-name skill)
                       (plist-get skill :name))
                     merged)
        (push skill merged)))
    merged))

;;;-----------------------------------------------
;;; Export to Standard Format
;;;-----------------------------------------------

;;;###autoload
(defun superchat-skills-standard-export (skill-name target-dir)
  "Export a superchat skill to standard format.
SKILL-NAME: Name of the skill to export
TARGET-DIR: Directory to create the standard skill in"
  (interactive (list (completing-read "Skill to export: " 
                                     (superchat-skills-get-available))
                    (read-directory-name "Target directory: ")))
  (let* ((preset (superchat-skills-load skill-name))
         (body (when preset (superchat-preset-skill-body preset)))
         (desc (if preset
                   (superchat-preset-description preset)
                 (format "Auto-exported from superchat skill %s" skill-name)))
         (type (if preset
                   (symbol-name (superchat-preset-type preset))
                 "prompt"))
         (version (or (when preset (superchat-preset-version preset)) "1.0"))
         (triggers (when preset (superchat-preset-triggers preset)))
         (tools (when preset (superchat-preset-tools preset)))
         (model (when preset (superchat-preset-model preset)))
         (temperature (when preset (superchat-preset-temperature preset)))
         (max-tokens (when preset (superchat-preset-max-tokens preset)))
         (reasoning (when preset (superchat-preset-reasoning preset)))
         (max-tool-calls (when preset
                           (superchat-preset-max-tool-calls preset)))
         (confirm-destructive (when preset
                                (superchat-preset-confirm-destructive preset)))
         (skill-dir (expand-file-name skill-name target-dir))
         (skill-file (expand-file-name "SKILL.md" skill-dir)))
    ;; Create directory
    (unless (file-directory-p skill-dir)
      (make-directory skill-dir t))
    ;; Write SKILL.md with frontmatter in canonical order
    (with-temp-file skill-file
      (insert "---\n")
      (insert (format "name: %s\n" skill-name))
      (insert (format "description: %s\n" desc))
      (insert (format "version: \"%s\"\n" version))
      (insert (format "type: %s\n" type))
      (when tools
        (insert (format "tools: [%s]\n"
                        (mapconcat (lambda (s) (format "\"%s\"" s))
                                   tools ", "))))
      (when model
        (insert (format "model: \"%s\"\n" model)))
      (when temperature
        (insert (format "temperature: %s\n" temperature)))
      (when max-tokens
        (insert (format "max_tokens: %s\n" max-tokens)))
      (when (and reasoning (not (eq reasoning 'inherit)))
        (insert (format "reasoning: %s\n" reasoning)))
      (when max-tool-calls
        (insert (format "max_tool_calls: %s\n" max-tool-calls)))
      (when (and preset (not (eq confirm-destructive 'inherit)))
        (insert (format "confirm_destructive: %s\n"
                        (if confirm-destructive "true" "false"))))
      (when triggers
        (insert (format "triggers: [%s]\n"
                        (mapconcat (lambda (s) (format "\"%s\"" s))
                                   triggers ", "))))
      (insert "---\n\n")
      (insert (or body "")))
    (message "Exported skill '%s' to %s" skill-name skill-dir)))

;;;-----------------------------------------------
;;; Commands
;;;-----------------------------------------------

;;;###autoload
(defun superchat-skills-standard-import (source-dir)
  "Import standard-format skills from SOURCE-DIR."
  (interactive "DSource directory: ")
  (let ((skills (superchat-skills-standard--find-skills source-dir))
        (imported 0))
    (dolist (skill-dir skills)
      (let* ((metadata (superchat-skills-standard--load-metadata skill-dir))
             (name (plist-get metadata :name))
             (content (superchat-skills-standard--extract-content 
                      (plist-get metadata :file)))
             (target-file (expand-file-name (concat name ".md")
                                           superchat-skills-directory)))
        (with-temp-file target-file
          (insert content))
        (setq imported (1+ imported))))
    (message "Imported %d skills from %s" imported source-dir)))

(defun superchat-skills-standard-list ()
  "List all discovered standard-format skills."
  (interactive)
  (let ((skills (superchat-skills-standard--discover-all)))
    (with-output-to-temp-buffer "*Standard Skills*"
      (princ (format "Standard Skills (%d found):\n\n" (length skills)))
      (dolist (skill skills)
        (princ (format "• %s\n" (if (superchat-preset-p skill)
                                     (superchat-preset-name skill)
                                   (plist-get skill :name))))
        (princ (format "  Description: %s\n" (if (superchat-preset-p skill)
                                                  (superchat-preset-description skill)
                                                (plist-get skill :context))))
        (princ (format "  Directory: %s\n\n" (if (superchat-preset-p skill)
                                                  (superchat-preset-source-file skill)
                                                (plist-get skill :directory))))))))

;;;-----------------------------------------------
;;; Initialization
;;;-----------------------------------------------

(defun superchat-skills-standard-initialize ()
  "Initialize standard skill format support."
  (when superchat-skills-support-standard-formats
    (message "Standard skill format support enabled")
    ;; Hook into skill discovery
    (advice-add 'superchat-skills--get-registry :around
               (lambda (orig-fun)
                 (superchat-skills-standard-merge-registry)))))

(provide 'superchat-skills-standard)

;;; superchat-skills-standard.el ends here
