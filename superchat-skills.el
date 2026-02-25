;;; superchat-skills.el --- Agentic Skills for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agentic Skills implementation for Superchat.
;;
;; Design principles:
;; - Skills are simple text files (markdown/org) describing capabilities
;; - AGENTS.md serves as the index describing when to use each skill
;; - Skills are loaded on-demand and injected into prompts as context
;; - Supports both explicit (>skill-name) and implicit (context-matched) invocation
;;
;; Uses superchat-executor.el for:
;; - Variable substitution ($input, $lang)
;; - Context management
;; - LLM execution
;;
;; Directory structure:
;;   ~/.emacs.d/superchat/
;;   ├── AGENTS.md          # Skill index with usage contexts
;;   └── skills/
;;       ├── code-review.md
;;       ├── refactor.md
;;       └── planning.md

;;; Code:

(require 'cl-lib)
(require 'superchat-executor)

;;;-----------------------------------------------
;;; Configuration
;;;-----------------------------------------------

(defgroup superchat-skills nil
  "Agentic Skills for Superchat."
  :group 'superchat)

(defcustom superchat-skills-directory
  (expand-file-name "skills/"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      (expand-file-name "superchat/" user-emacs-directory)))
  "Directory containing skill files."
  :type 'directory
  :group 'superchat-skills)

(defcustom superchat-skills-agents-md-file
  (expand-file-name "AGENTS.md"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      (expand-file-name "superchat/" user-emacs-directory)))
  "Path to AGENTS.md file containing skill index."
  :type 'file
  :group 'superchat-skills)

(defcustom superchat-skills-file-extensions '("md" "org" "txt" "el")
  "Supported skill file extensions."
  :type '(repeat string)
  :group 'superchat-skills)

(defcustom superchat-skills-implicit-match-p t
  "Whether to enable implicit skill matching.
When enabled, superchat will try to automatically detect
which skill to use based on user input."
  :type 'boolean
  :group 'superchat-skills)

(defcustom superchat-skills-match-confidence-threshold 0.7
  "Minimum confidence threshold for implicit skill matching.
Value between 0 and 1. Skills with lower confidence will not be auto-selected."
  :type 'float
  :group 'superchat-skills)

;;;-----------------------------------------------
;;; Core Functions
;;;-----------------------------------------------

(defun superchat-skills--package-skills-directory ()
  "Return the skills directory bundled with the superchat package."
  (let ((pkg-dir (file-name-directory (or load-file-name
                                          (locate-library "superchat-skills")
                                          ""))))
    (when pkg-dir
      (expand-file-name "skills/" pkg-dir))))

(defun superchat-skills--ensure-directory ()
  "Ensure the skills directory exists and has default skills.
On first run, copies bundled skill files from the package directory
into the user's skills directory."
  (unless (file-directory-p superchat-skills-directory)
    (make-directory superchat-skills-directory t))
  ;; Seed default skills from the package if user directory is empty
  (when (and (file-directory-p superchat-skills-directory)
             (null (directory-files superchat-skills-directory nil
                                   "\\.\\(md\\|org\\|txt\\)$")))
    (let ((pkg-dir (superchat-skills--package-skills-directory)))
      (when (and pkg-dir (file-directory-p pkg-dir))
        (dolist (file (directory-files pkg-dir t "\\.\\(md\\|org\\|txt\\)$"))
          (let ((dest (expand-file-name (file-name-nondirectory file)
                                        superchat-skills-directory)))
            (unless (file-exists-p dest)
              (copy-file file dest)
              (message "superchat: installed default skill %s"
                       (file-name-nondirectory file))))))))
  superchat-skills-directory)

(defun superchat-skills-get-available ()
  "Get list of available skill names."
  (when (file-directory-p superchat-skills-directory)
    (let (skills)
      (dolist (ext superchat-skills-file-extensions)
        (setq skills
              (append skills
                      (mapcar #'file-name-base
                              (directory-files superchat-skills-directory nil
                                               (concat "^.*\\." (regexp-quote ext) "$"))))))
      (delete-dups skills))))

(defun superchat-skills-exists-p (skill-name)
  "Check if a skill exists."
  (member skill-name (superchat-skills-get-available)))

(defun superchat-skills--find-file (skill-name)
  "Find the skill file path for SKILL-NAME.
Returns the first matching file with supported extension."
  (catch 'found
    (dolist (ext superchat-skills-file-extensions)
      (let ((file (expand-file-name (concat skill-name "." ext)
                                    superchat-skills-directory)))
        (when (file-exists-p file)
          (throw 'found file))))
    nil))

(defun superchat-skills-load (skill-name)
  "Load the content of a skill file."
  (let ((skill-file (superchat-skills--find-file skill-name)))
    (when skill-file
      (with-temp-buffer
        (insert-file-contents skill-file)
        (buffer-string)))))

;;;-----------------------------------------------
;;; Input Parsing
;;;-----------------------------------------------

(defun superchat-skills-parse-input (input)
  "Parse >skill-name syntax from input.
Returns (skill-name . user-input) cons or nil.

Syntax: >skill-name optional-user-input

Examples:
  >code-review #file.el check this
  => (\"code-review\" . \"#file.el check this\")

  >planning
  => (\"planning\" . \"\")"
  (when (string-prefix-p ">" input)
    (let ((clean-input (string-trim (substring input 1))))
      (if (string-match "^\\([a-zA-Z0-9_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?$" clean-input)
          (cons (match-string 1 clean-input)
                (string-trim (or (match-string 2 clean-input) "")))
        ;; If no match, treat entire input as skill name
        (cons clean-input "")))))

;;;-----------------------------------------------
;;; AGENTS.md Parsing for Implicit Matching
;;;-----------------------------------------------

(defun superchat-skills--load-agents-md ()
  "Load the AGENTS.md file content if it exists."
  (when (and superchat-skills-agents-md-file
             (file-exists-p superchat-skills-agents-md-file))
    (with-temp-buffer
      (insert-file-contents superchat-skills-agents-md-file)
      (buffer-string))))

(defun superchat-skills--parse-registry (content)
  "Parse skill registry from AGENTS.md CONTENT.
Returns a list of plists with :name :context :triggers :file keys."
  (let ((skills '())
        (pos 0))
    ;; Match skill definitions: ### skill-name followed by bullet points
    (while (string-match "^###\\s-+\\([a-zA-Z0-9_-]+\\)\\s-*\n" content pos)
      (let* ((name (match-string 1 content))
             (section-start (match-end 0))
             (section-end (or (string-match "^###\\s-+" content section-start)
                             (string-match "^##\\s-+" content section-start)
                             (length content)))
             (section (substring content section-start section-end)))
        ;; Parse fields from the section
        (let ((context "")
              (triggers '())
              (file "")
              (note ""))
          ;; Extract context
          (when (string-match "^-\\s-+\\*\\*context\\*\\*:\\s-*\\(.+\\)$" section)
            (setq context (string-trim (match-string 1 section))))
          ;; Extract triggers (handle bracketed list format)
          (when (string-match "^-\\s-+\\*\\*triggers\\*\\*:\\s-*\\[\\(.+?\\)\\]" section)
            (let ((triggers-str (match-string 1 section)))
              (setq triggers 
                    (mapcar (lambda (s) 
                              (string-trim (string-trim s) "\"" "\""))
                            (split-string triggers-str "," t "\\s*")))))
          ;; Extract file
          (when (string-match "^-\\s-+\\*\\*file\\*\\*:\\s-*\\(.+\\)$" section)
            (setq file (string-trim (match-string 1 section))))
          ;; Extract note (optional)
          (when (string-match "^-\\s-+\\*\\*note\\*\\*:\\s-*\\(.+\\)$" section)
            (setq note (string-trim (match-string 1 section))))
          ;; Add to skills list
          (when name
            (push (list :name name
                        :context context
                        :triggers triggers
                        :file (or file (format "skills/%s.md" name))
                        :note note)
                  skills))
          (setq pos section-start))))
    (nreverse skills)))

(defun superchat-skills--get-registry ()
  "Get parsed skill registry from AGENTS.md.
Returns list of skill definitions."
  (let ((content (superchat-skills--load-agents-md)))
    (if content
        (superchat-skills--parse-registry content)
      ;; Fallback: build registry from available skills
      (mapcar (lambda (name)
                (list :name name
                      :context ""
                      :triggers '()
                      :file (format "skills/%s.md" name)))
              (superchat-skills-get-available)))))

;;;-----------------------------------------------
;;; Implicit Skill Matching
;;;-----------------------------------------------

(defvar superchat-skills--last-match-result nil
  "Stores the last implicit match result for debugging.
Contains plist with :skill :confidence :reasoning")

(defun superchat-skills--build-match-prompt (user-input skills)
  "Build prompt for LLM to match user input with skills.

USER-INPUT: The user's request
SKILLS: List of skill definitions

Returns a prompt string for the LLM."
  (concat "You are a skill matcher. Analyze the user's request and determine which skill (if any) should be used.

Available skills:\n\n"
          (mapconcat (lambda (skill)
                       (format "Skill: %s\nContext: %s\nTriggers: %s\n---"
                               (plist-get skill :name)
                               (plist-get skill :context)
                               (string-join (plist-get skill :triggers) ", ")))
                     skills "\n")
          "\n\nUser request: \"" user-input "\"\n\n"
          "Respond in this exact format:\n"
          "MATCH: <skill-name or NONE>\n"
          "CONFIDENCE: <0.0-1.0>\n"
          "REASONING: <one sentence explaining why>\n\n"
          "Rules:\n"
          "- Use NONE if no skill matches well\n"
          "- Only match if the request clearly fits the skill's context\n"
          "- Be conservative: prefer NONE over weak matches"))

(defun superchat-skills--parse-match-response (response)
  "Parse LLM match response.
Returns plist with :skill :confidence :reasoning, or nil."
  (when response
    (let ((skill nil)
          (confidence 0.0)
          (reasoning ""))
      ;; Parse MATCH line
      (when (string-match "^MATCH:\\s-*\\(.+\\)$" response)
        (setq skill (string-trim (match-string 1 response)))
        (when (string= (downcase skill) "none")
          (setq skill nil)))
      ;; Parse CONFIDENCE line
      (when (string-match "^CONFIDENCE:\\s-*\\([0-9.]+\\)$" response)
        (setq confidence (string-to-number (match-string 1 response))))
      ;; Parse REASONING line
      (when (string-match "^REASONING:\\s-*\\(.+\\)$" response)
        (setq reasoning (string-trim (match-string 1 response))))
      
      (when skill
        (list :skill skill
              :confidence confidence
              :reasoning reasoning)))))

(defun superchat-skills-match (user-input &optional callback)
  "Match user input to a skill using LLM.

USER-INPUT: The user's request text
CALLBACK: Optional callback function for async operation

If CALLBACK is provided, calls it with (skill-name confidence reasoning).
Otherwise returns (skill-name . confidence) cons or nil synchronously.

Note: Synchronous mode requires `superchat-executor--llm-executor` to be set."
  (let* ((registry (superchat-skills--get-registry))
         (match-prompt (superchat-skills--build-match-prompt user-input registry)))
    
    (if callback
        ;; Async mode
        (when superchat-executor--llm-executor
          (funcall superchat-executor--llm-executor
                   match-prompt
                   (lambda (response)
                     (let ((result (superchat-skills--parse-match-response response)))
                       (setq superchat-skills--last-match-result result)
                       (funcall callback
                                (plist-get result :skill)
                                (plist-get result :confidence)
                                (plist-get result :reasoning))))))
      
      ;; Sync mode - simplified: use keyword matching as fallback
      (superchat-skills--match-by-keywords user-input registry))))

(defun superchat-skills--match-by-keywords (user-input skills)
  "Fallback keyword matching for synchronous operation.
Returns (skill-name . confidence) or nil."
  (let ((input-lower (downcase user-input))
        (best-match nil)
        (best-score 0))
    (dolist (skill skills)
      (let ((score 0)
            (triggers (plist-get skill :triggers)))
        ;; Score based on trigger matches
        (dolist (trigger triggers)
          (when (string-match-p (regexp-quote (downcase trigger)) input-lower)
            (setq score (+ score 0.3))))
        ;; Score based on name match
        (when (string-match-p (regexp-quote (downcase (plist-get skill :name))) input-lower)
          (setq score (+ score 0.2)))
        ;; Check for file references (common in code-related skills)
        (when (and (string-match-p "#" input-lower)
                   (member (plist-get skill :name) '("code-review" "refactor")))
          (setq score (+ score 0.2)))
        ;; Update best match
        (when (> score best-score)
          (setq best-score score)
          (setq best-match (plist-get skill :name)))))
    
    (when (and best-match (>= best-score superchat-skills-match-confidence-threshold))
      (cons best-match best-score))))

(defun superchat-skills-try-implicit (user-input)
  "Try to implicitly match and invoke a skill for USER-INPUT.

Returns:
- If matched: Result plist from `superchat-skills-invoke`
- If not matched: nil (caller should handle as normal chat)"
  (when superchat-skills-implicit-match-p
    (let ((match (superchat-skills--match-by-keywords 
                  user-input 
                  (superchat-skills--get-registry))))
      (when match
        (let ((skill-name (car match))
              (confidence (cdr match)))
          (message "🔍 Implicit skill match: %s (confidence: %.2f)" 
                   skill-name confidence)
          (superchat-skills-invoke skill-name user-input))))))

;;;-----------------------------------------------
;;; Prompt Integration
;;;-----------------------------------------------

(defun superchat-skills-build-prompt (user-input skill-name)
  "Build the final prompt by combining user input with skill content.

USER-INPUT: The user's request text
SKILL-NAME: Name of the skill to apply

Returns the combined prompt string with variable substitution applied."
  (let* ((skill-content (superchat-skills-load skill-name))
         ;; Create a temporary context for variable substitution
         (ctx (superchat-executor-context-create nil user-input)))
    ;; Apply variable substitution to skill content
    (when skill-content
      (setq skill-content
            (superchat-executor-replace-variables
             skill-content
             user-input
             superchat-executor--current-lang
             ctx)))
    ;; Wrap with context using executor
    (superchat-executor-wrap-with-context user-input skill-content)))

;;;-----------------------------------------------
;;; Invocation
;;;-----------------------------------------------

(defun superchat-skills-invoke (skill-name &optional user-input)
  "Invoke a skill with optional user input.

This function prepares the prompt with skill context and returns
a result plist compatible with superchat's command system.

Returns plist with:
  :type - :llm-query or :echo
  :prompt - The combined prompt with skill context
  :content - Echo message (if applicable)
  :skill - The skill name used"
  (cond
   ((superchat-skills-exists-p skill-name)
    (let ((combined-prompt (superchat-skills-build-prompt
                            (or user-input "")
                            skill-name)))
      (message "🎯 Using skill: %s" skill-name)
      `(:type :llm-query
        :prompt ,combined-prompt
        :skill ,skill-name)))
   
   (t
    `(:type :echo
      :content ,(format "Skill '%s' not found. Available: %s"
                        skill-name
                        (mapconcat #'identity
                                   (superchat-skills-get-available)
                                   ", "))))))

;;;-----------------------------------------------
;;; Completion Support
;;;-----------------------------------------------

(defun superchat-skills-completion-list ()
  "Return list of skill names with > prefix for completion."
  (mapcar (lambda (skill) (concat ">" skill))
          (superchat-skills-get-available)))

;;;-----------------------------------------------
;;; Debugging & Diagnostics
;;;-----------------------------------------------

(defun superchat-skills-diagnose ()
  "Run diagnostics on the skills system.
Displays configuration, available skills, and registry status."
  (interactive)
  (with-output-to-temp-buffer "*Skills Diagnostics*"
    (princ "=== Agentic Skills Diagnostics ===\n\n")
    
    ;; Configuration
    (princ "Configuration:\n")
    (princ (format "  Skills directory: %s\n" superchat-skills-directory))
    (princ (format "  Directory exists: %s\n" (file-directory-p superchat-skills-directory)))
    (princ (format "  AGENTS.md file: %s\n" superchat-skills-agents-md-file))
    (princ (format "  AGENTS.md exists: %s\n" (file-exists-p superchat-skills-agents-md-file)))
    (princ (format "  Implicit match: %s\n" superchat-skills-implicit-match-p))
    (princ (format "  Confidence threshold: %.2f\n\n" superchat-skills-match-confidence-threshold))
    
    ;; Available skills
    (let ((available (superchat-skills-get-available)))
      (princ (format "Available Skills (%d):\n" (length available)))
      (dolist (skill available)
        (princ (format "  - %s\n" skill))))
    (princ "\n")
    
    ;; Registry
    (let ((registry (superchat-skills--get-registry)))
      (princ (format "Registered Skills (%d):\n" (length registry)))
      (dolist (skill registry)
        (princ (format "  - %s\n" (plist-get skill :name)))
        (princ (format "    Context: %s\n" (plist-get skill :context)))
        (princ (format "    Triggers: %s\n" (plist-get skill :triggers)))
        (princ (format "    File: %s\n" (plist-get skill :file)))))
    (princ "\n")
    
    ;; Last match
    (if superchat-skills--last-match-result
        (progn
          (princ "Last Match Result:\n")
          (princ (format "  Skill: %s\n" (plist-get superchat-skills--last-match-result :skill)))
          (princ (format "  Confidence: %.2f\n" (plist-get superchat-skills--last-match-result :confidence)))
          (princ (format "  Reasoning: %s\n" (plist-get superchat-skills--last-match-result :reasoning))))
      (princ "No recent match result.\n"))
    
    (princ "\n=== End of Diagnostics ===\n")))

(defun superchat-skills-test-match (input)
  "Test implicit matching for INPUT.
Shows detailed matching information."
  (interactive "sTest input: ")
  (let* ((registry (superchat-skills--get-registry))
         (result (superchat-skills--match-by-keywords input registry))
         (skill-name (car result))
         (confidence (cdr result)))
    
    (message "Input: %s" input)
    (message "Registry skills: %d" (length registry))
    
    (if result
        (progn
          (message "✓ MATCHED: %s (confidence: %.2f)" skill-name confidence)
          (when (superchat-skills-exists-p skill-name)
            (let ((skill-result (superchat-skills-invoke skill-name input)))
              (message "Skill invocation: %s" (plist-get skill-result :type))))
          (list :matched skill-name :confidence confidence))
      
      (message "✗ No match found")
      (let ((threshold superchat-skills-match-confidence-threshold))
        (message "  (Threshold: %.2f)" threshold))
      nil)))

(defun superchat-skills-inspect (skill-name)
  "Inspect a skill's details."
  (interactive (list (completing-read "Skill: " (superchat-skills-get-available))))
  (let ((content (superchat-skills-load skill-name))
        (file (superchat-skills--find-file skill-name)))
    (with-output-to-temp-buffer "*Skill Inspector*"
      (princ (format "=== Skill: %s ===\n\n" skill-name))
      (princ (format "File: %s\n" file))
      (princ (format "Exists: %s\n\n" (superchat-skills-exists-p skill-name)))
      (princ "Content:\n")
      (princ (or content "[Not found]"))
      (princ "\n"))))

;;;-----------------------------------------------
;;; GitHub Installation
;;;-----------------------------------------------

(defun superchat-skills--parse-install-spec (spec)
  "Parse SPEC into (user repo branch).
SPEC is \"user/repo\" or \"user/repo@branch\".
Returns a list (USER REPO BRANCH) where BRANCH may be nil."
  (when (string-match "^\\([^/]+\\)/\\([^@]+\\)\\(?:@\\(.+\\)\\)?$" spec)
    (list (match-string 1 spec)
          (match-string 2 spec)
          (match-string 3 spec))))

(defun superchat-skills--github-api-get (url)
  "Fetch URL synchronously and return parsed JSON.
Returns nil on error."
  (require 'url)
  (require 'json)
  (let ((url-request-extra-headers
         '(("Accept" . "application/vnd.github.v3+json")
           ("User-Agent" . "superchat-emacs"))))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t nil 30)
          (goto-char (point-min))
          (when (re-search-forward "^$" nil t)
            (let ((json-object-type 'alist)
                  (json-array-type 'list))
              (json-read))))
      (error
       (message "superchat: GitHub API error: %s" (error-message-string err))
       nil))))

(defun superchat-skills--github-fetch-file-list (user repo &optional branch)
  "Fetch skill file list from GitHub repo USER/REPO.
If BRANCH is non-nil, use that ref.  Auto-detects skills/ subdirectory.
Returns alist of (NAME . DOWNLOAD-URL)."
  (let* ((ref-param (if branch (format "?ref=%s" branch) ""))
         (skills-url (format "https://api.github.com/repos/%s/%s/contents/skills%s"
                             user repo ref-param))
         (root-url (format "https://api.github.com/repos/%s/%s/contents/%s"
                           user repo ref-param))
         ;; Try skills/ directory first
         (skills-response (superchat-skills--github-api-get skills-url))
         (entries (if (and (listp skills-response)
                           (not (assq 'message skills-response)))
                      skills-response
                    ;; Fall back to root directory
                    (superchat-skills--github-api-get root-url)))
         (ext-regexp "\\.\\(md\\|org\\|txt\\)$"))
    (when (listp entries)
      (cl-loop for entry in entries
               when (and (string= (alist-get 'type entry) "file")
                         (string-match-p ext-regexp (alist-get 'name entry "")))
               collect (cons (alist-get 'name entry)
                             (alist-get 'download_url entry))))))

(defun superchat-skills--github-download-file (url dest-path)
  "Download file from URL and write to DEST-PATH.
Returns t on success, nil on error."
  (require 'url)
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url t nil 30)
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t)
          (forward-char 1)
          (let ((content (buffer-substring-no-properties (point) (point-max))))
            (with-temp-file dest-path
              (insert content))
            t)))
    (error
     (message "superchat: download error for %s: %s" url (error-message-string err))
     nil)))

(defun superchat-skills-install (spec)
  "Install skills from GitHub repository SPEC.
SPEC is \"user/repo\" or \"user/repo@branch\".
Downloads skill files (.md, .org, .txt) into `superchat-skills-directory'.
Returns a superchat result plist for display."
  (let ((parsed (superchat-skills--parse-install-spec (string-trim spec))))
    (unless parsed
      (cl-return-from superchat-skills-install
        `(:type :echo :content ,(format "Invalid spec: %s\nUsage: /skill-install user/repo[@branch]" spec))))
    (let* ((user (nth 0 parsed))
           (repo (nth 1 parsed))
           (branch (nth 2 parsed))
           (file-list (superchat-skills--github-fetch-file-list user repo branch)))
      (unless file-list
        (cl-return-from superchat-skills-install
          `(:type :echo :content ,(format "No skill files found in %s/%s%s"
                                          user repo
                                          (if branch (format " (branch: %s)" branch) "")))))
      (superchat-skills--ensure-directory)
      (let ((installed '())
            (failed '()))
        (dolist (entry file-list)
          (let* ((name (car entry))
                 (url (cdr entry))
                 (dest (expand-file-name name superchat-skills-directory)))
            (if (superchat-skills--github-download-file url dest)
                (push name installed)
              (push name failed))))
        (let ((msg (format "Installed %d skill(s) from %s/%s: %s"
                           (length installed) user repo
                           (string-join (nreverse installed) ", "))))
          (when failed
            (setq msg (concat msg (format "\nFailed: %s" (string-join (nreverse failed) ", ")))))
          `(:type :echo :content ,msg))))))

;;;-----------------------------------------------
;;; Initialization
;;;-----------------------------------------------

(defun superchat-skills-initialize ()
  "Initialize the skills system."
  (superchat-skills--ensure-directory)
  (message "Agentic Skills initialized. Directory: %s" superchat-skills-directory))

;; Auto-initialize when loaded
(superchat-skills-initialize)

(provide 'superchat-skills)

;;; superchat-skills.el ends here
