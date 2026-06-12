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
      (let ((alist (superchat-skills-standard--parse-frontmatter content)))
        (if (null alist)
            ;; No frontmatter — use directory basename as name, empty description
            (list :name (file-name-nondirectory skill-dir)
                  :description ""
                  :type "prompt"
                  :version "1.0"
                  :triggers '()
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
  '("name" "description" "version" "type" "triggers")
  "Canonical YAML key order for SKILL.md export.")

(defun superchat-skills-standard--parse-frontmatter (content)
  "Parse YAML frontmatter from CONTENT string.
Returns an alist of (KEY . VALUE) pairs, or nil if no frontmatter block.
Trims values, supports quoted strings, and supports
`key: [\"a\", \"b\"]' list shorthand for `triggers'."
  (when (and (stringp content)
             (string-match "^---\\s-*\n\\(\\(.\\|\n\\)*\\)---\\s-*\n" content))
    (let ((raw (match-string 1 content))
          (alist '()))
      (dolist (line (split-string raw "\n"))
        (when (string-match "^\\([a-z]+\\):\\s-*\\(.+\\)$" line)
          (let* ((key (match-string 1 line))
                 (val (match-string 2 line)))
            ;; List shorthand: triggers: ["a", "b"]
            (cond
             ((string-match "^\\[\\(.*\\)\\]$" val)
              (let ((items (mapcar (lambda (s) (string-trim s "\"" "\""))
                                  (split-string (match-string 1 val) "," t "\\s*"))))
                (push (cons key items) alist)))
             ;; Quoted string: "value"
             ((string-match "^\"\\(.*\\)\"$" val)
              (push (cons key (match-string 1 val)) alist))
             ;; Plain string
             (t
              (push (cons key (string-trim val)) alist))))))
      (nreverse alist))))

(defun superchat-skills-standard--validate (alist)
  "Validate a frontmatter ALIST for required fields.
Returns (t . plist) on success or (nil . error-string) on failure.
Required: `name', `description'.  `type' defaults to `prompt',
`version' defaults to `\"1.0\"'.  Type must be `prompt' or `workflow'."
  (let ((name (cdr (assoc "name" alist)))
        (desc (cdr (assoc "description" alist)))
        (ver  (or (cdr (assoc "version" alist)) "1.0"))
        (type (or (cdr (assoc "type" alist)) "prompt"))
        (trig (cdr (assoc "triggers" alist))))
    (cond
     ((or (null name) (string-empty-p name))
      (cons nil "missing required field: name"))
     ((or (null desc) (string-empty-p desc))
      (cons nil "missing required field: description"))
     ((not (member (downcase type) '("prompt" "workflow")))
      (cons nil (format "invalid type: %s (must be prompt or workflow)" type)))
     (t
      (cons t (list :name name :description desc :version ver
                    :type (downcase type) :triggers trig))))))

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
  "Convert STANDARD-SKILL to superchat internal format.
Return plist compatible with superchat-skills functions."
  (let* ((metadata (superchat-skills-standard--load-metadata standard-skill))
         (name (plist-get metadata :name))
         (content (superchat-skills-standard--extract-content 
                  (plist-get metadata :file)))
         (references (superchat-skills-standard--load-references standard-skill)))
    (list :name name
          :context (plist-get metadata :description)
          :triggers (plist-get metadata :triggers)
          :content content
          :references references
          :source :standard
          :directory standard-skill)))

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
      (unless (assoc (plist-get skill :name) merged)
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
  (let* ((skill (superchat-skills-load skill-name))
         (body (plist-get skill :body))
         (desc (or (plist-get skill :description)
                   (format "Auto-exported from superchat skill %s" skill-name)))
         (type (or (plist-get skill :type) "prompt"))
         (version (or (plist-get skill :version) "1.0"))
         (triggers (plist-get skill :triggers))
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
      (when triggers
        (insert (format "triggers: [%s]\n"
                        (mapconcat (lambda (s) (format "\"%s\"" s))
                                   triggers ", "))))
      (insert "---\n\n")
      (insert body))
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
        (princ (format "• %s\n" (plist-get skill :name)))
        (princ (format "  Description: %s\n" (plist-get skill :context)))
        (princ (format "  Directory: %s\n\n" (plist-get skill :directory)))))))

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
