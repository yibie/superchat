;;; superchat-skills.el --- Agentic Skills for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agentic Skills implementation for Superchat.
;;
;; Design principles:
;; - Skills are simple text files (markdown/org) describing capabilities
;; - AGENTS.md serves as the index describing when to use each skill
;; - Skills are loaded on-demand and injected into prompts as context
;; - Explicit (>skill-name) invocation; the model-driven `superchat-skills-match'
;;   async matcher is available to callers that want it
;;
;; Uses superchat-executor.el for:
;; - Variable substitution ($input, $lang)
;; - Context management
;; Semantic matching submits through the shared Runtime.
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
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-executor)
(require 'superchat-preset)
(declare-function superchat-prompt--substitute-template "superchat-prompt-hooks" (user-query))

(declare-function superchat-sink-callback "superchat-sink")

;;;-----------------------------------------------
;;; Configuration
;;;-----------------------------------------------

(defgroup superchat-skills nil
  "Agentic Skills for Superchat."
  :group 'superchat)

(defcustom superchat-skills-github-token nil
  "GitHub Personal Access Token for authenticated API requests.
When non-nil, used for Authorization header to raise rate limits.
If nil, falls back to the GITHUB_TOKEN environment variable."
  :type '(choice (const nil) string)
  :group 'superchat-skills)

(defcustom superchat-skills-directory
  (expand-file-name "skills/"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      (expand-file-name "superchat/" user-emacs-directory)))
  "Directory containing skill files."
  :type 'directory
  :group 'superchat-skills)

(defcustom superchat-skills-extra-directories nil
  "Other folders whose skills join the skill catalog.
Each folder is searched, at any depth, for folders holding a `SKILL.md'
(the Agent Skills layout that Pi, Claude Code and Codex use), for example
\"~/.agents/skills\".  `superchat-skills-directory' comes first: when two
skills share a name, the first one found wins."
  :type '(repeat directory)
  :group 'superchat-skills)

(defconst superchat-skills-description-max-chars 1024
  "Longest description a skill shows in the catalog (the Agent Skills limit).")

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

(defcustom superchat-skills-match-confidence-threshold 0.7
  "Minimum confidence threshold for implicit skill matching.
Value between 0 and 1. Skills with lower confidence will not be auto-selected."
  :type 'float
  :group 'superchat-skills)

;;;-----------------------------------------------
;;; Core Functions
;;;-----------------------------------------------

(defvar superchat-skills--initialized-directory nil
  "Expanded skills directory initialized in this Emacs process, if any.")

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
  (superchat-skills-initialize)
  (when (file-directory-p superchat-skills-directory)
    (let (skills)
      (dolist (ext superchat-skills-file-extensions)
        (setq skills
              (append skills
                      (mapcar #'file-name-base
                              (directory-files superchat-skills-directory nil
                                               (concat "^.*\\." (regexp-quote ext) "$"))))))
      ;; Folders holding a SKILL.md, by their own name.
      (dolist (dir (directory-files superchat-skills-directory t "\\`[^.]"))
        (when (file-exists-p (expand-file-name "SKILL.md" dir))
          (setq skills (append skills (list (file-name-nondirectory dir))))))
      (delete-dups skills))))

(defun superchat-skills-exists-p (skill-name)
  "Check if SKILL-NAME exists as a skill."
  (and (superchat-skills--find-file skill-name) t))

(defun superchat-skills--find-file (skill-name)
  "Find the skill file path for SKILL-NAME.
A single file NAME.EXT in `superchat-skills-directory' comes first, then a
folder NAME holding `SKILL.md', then any `SKILL.md' there whose frontmatter
name is SKILL-NAME."
  (superchat-skills-initialize)
  (catch 'found
    (dolist (ext superchat-skills-file-extensions)
      (let ((file (expand-file-name (concat skill-name "." ext)
                                    superchat-skills-directory)))
        (when (file-exists-p file)
          (throw 'found file))))
    (let ((folder (expand-file-name (concat skill-name "/SKILL.md")
                                    superchat-skills-directory)))
      (when (file-exists-p folder)
        (throw 'found folder)))
    (dolist (file (superchat-skills--skill-files superchat-skills-directory nil))
      (when (equal skill-name
                   (cdr (assoc "name" (ignore-errors (superchat-skills--frontmatter file)))))
        (throw 'found file)))
    nil))

;;;; The skill catalog: skills the model finds itself
;; docs/decisions/accepted/2026-09-27-skills-the-model-finds-itself.md

(defun superchat-skills--unquote (value)
  "VALUE without one pair of surrounding quotes."
  (if (and (>= (length value) 2)
           (memq (aref value 0) '(?\" ?'))
           (eq (aref value 0) (aref value (1- (length value)))))
      (substring value 1 -1)
    value))

(defun superchat-skills--frontmatter (file)
  "FILE's frontmatter as an alist of lower-case keys to strings, or nil.
Only `key: value' lines are read; a YAML block scalar (`|' or `>' followed
by indented lines) is read whole, `|' keeping line breaks and `>' folding
them into spaces."
  (with-temp-buffer
    (insert-file-contents file nil 0 65536)
    (goto-char (point-min))
    (when (looking-at "---[ \t]*$")
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^---[ \t]*$" nil t)
          (let ((end (copy-marker (match-beginning 0))) alist)
            (goto-char start)
            (while (< (point) end)
              (if (looking-at "\\([A-Za-z_][A-Za-z0-9_-]*\\):[ \t]*\\(.*\\)$")
                  (let ((key (downcase (match-string 1)))
                        (value (string-trim (match-string 2))))
                    (forward-line 1)
                    (if (string-match-p "\\`[|>][-+]?\\'" value)
                        (let ((fold (string-prefix-p ">" value)) lines)
                          (while (and (< (point) end)
                                      (looking-at "\\(?:[ \t]+\\(.*\\)\\|[ \t]*\\)$"))
                            (push (string-trim (or (match-string 1) "")) lines)
                            (forward-line 1))
                          (setq lines (nreverse lines))
                          (while (and lines (string-empty-p (car (last lines))))
                            (setq lines (butlast lines)))
                          (setq value (mapconcat #'identity lines (if fold " " "\n"))))
                      (setq value (superchat-skills--unquote value)))
                    (push (cons key value) alist))
                (forward-line 1)))
            (nreverse alist)))))))

(defun superchat-skills--skill-files (directory root-files)
  "Skill files in DIRECTORY: every `SKILL.md' at any depth, then, when
ROOT-FILES, DIRECTORY's own .md and .org files.  Hidden folders are skipped."
  (when (and (stringp directory) (file-directory-p directory))
    (let ((directory (expand-file-name directory)))
      (append
       (sort (directory-files-recursively
              directory "\\`SKILL\\.md\\'" nil
              (lambda (dir) (not (string-prefix-p "." (file-name-nondirectory dir))))
              t)
             #'string<)
       (when root-files
         (cl-remove-if (lambda (file) (equal (file-name-nondirectory file) "SKILL.md"))
                       (directory-files directory t "\\`[^.].*\\.\\(?:md\\|org\\)\\'")))))))

(defun superchat-skills--catalog-entry (file)
  "FILE's catalog entry (:name :description :location), or nil when it is
not listed: no description, a mode skill (type agent or plan), or marked
`disable-model-invocation: true'."
  (let* ((front (superchat-skills--frontmatter file))
         (description (cdr (assoc "description" front)))
         (type (cdr (assoc "type" front))))
    (when (and (stringp description)
               (not (string-empty-p (string-trim description)))
               (not (member type '("agent" "plan")))
               (not (member (cdr (assoc "disable-model-invocation" front)) '("true" "yes"))))
      (list :name (or (cdr (assoc "name" front))
                      (if (equal (file-name-nondirectory file) "SKILL.md")
                          (file-name-nondirectory
                           (directory-file-name (file-name-directory file)))
                        (file-name-base file)))
            :description (if (> (length description) superchat-skills-description-max-chars)
                             (substring description 0 superchat-skills-description-max-chars)
                           description)
            :location (expand-file-name file)))))

(defun superchat-skills-catalog ()
  "The skills the model may read: a list of (:name :description :location).
`superchat-skills-directory' (its SKILL.md folders and described files)
comes first, then `superchat-skills-extra-directories' (SKILL.md folders);
the first skill of a name wins.  Reading the folders has no side effect."
  (let (seen catalog)
    (dolist (source (cons (cons superchat-skills-directory t)
                          (mapcar (lambda (dir) (cons dir nil))
                                  superchat-skills-extra-directories)))
      (dolist (file (superchat-skills--skill-files (car source) (cdr source)))
        (when-let* ((entry (ignore-errors (superchat-skills--catalog-entry file))))
          (unless (member (plist-get entry :name) seen)
            (push (plist-get entry :name) seen)
            (push entry catalog)))))
    (nreverse catalog)))

(defun superchat-skills--xml (text)
  "TEXT with the XML special characters escaped."
  (replace-regexp-in-string
   "[&<>\"]"
   (lambda (c) (pcase c ("&" "&amp;") ("<" "&lt;") (">" "&gt;") (_ "&quot;")))
   text t t))

(defun superchat-skills-catalog-text ()
  "The skill catalog as system text, in Pi's shape, or nil with no skills."
  (when-let* ((skills (superchat-skills-catalog)))
    (concat
     "The following skills provide specialized instructions for specific tasks. "
     "Use read_file to load a skill's file when the task matches its description, "
     "or when the user names the skill. When a skill file refers to a relative path, "
     "resolve it against the skill's directory (the directory of its file) and read "
     "it with read_file when you need it.\n\n<available_skills>\n"
     (mapconcat (lambda (skill)
                  (format "  <skill>\n    <name>%s</name>\n    <description>%s</description>\n    <location>%s</location>\n  </skill>\n"
                          (superchat-skills--xml (plist-get skill :name))
                          (superchat-skills--xml (plist-get skill :description))
                          (superchat-skills--xml (plist-get skill :location))))
                skills "")
     "</available_skills>")))

(defun superchat-skills-load (skill-name)
  "Load SKILL-NAME and return a `superchat-preset'.
Parses YAML frontmatter if present.  Returns nil if skill not found."
  (let ((skill-file (superchat-skills--find-file skill-name)))
    (when skill-file
      (with-temp-buffer
        (insert-file-contents skill-file)
        (let* ((raw (buffer-string))
               (body-start (when (string-match "^---\\s-*\n\\(\\(.\\|\n\\)*\\)---\\s-*\n" raw)
                             (match-end 0)))
               (alist (when body-start (superchat-preset-parse-frontmatter raw)))
               (body (substring raw body-start))
               (name (or (cdr (assoc "name" alist)) skill-name))
               (profile-id (or (cdr (assoc "profile_id" alist))
                               (cdr (assoc "profile-id" alist))))
               (desc (or (cdr (assoc "description" alist)) ""))
               (type (or (cdr (assoc "type" alist)) "prompt"))
               (capability (cdr (assoc "capability" alist)))
               (version (or (cdr (assoc "version" alist)) "1.0"))
               (triggers (cdr (assoc "triggers" alist)))
               ;; tools: [] → `none' (explicitly no tools); absent → nil
               ;; (inherit global tools).  Mirrors
               ;; `superchat-preset-from-frontmatter'.
               (tools (let ((entry (assoc "tools" alist)))
                        (cond ((null entry) nil)
                              ((null (cdr entry)) 'none)
                              (t (cdr entry)))))
               (plugins (cdr (assoc "plugins" alist)))
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
               (timeout (cdr (assoc "timeout" alist)))
               (pre (cdr (assoc "pre" alist))))
          (superchat-preset-from-plist
           (list :name name
                 :profile-id profile-id
                 :description desc
                 :type type
                 :capability capability
                 :body body
                 :tools tools
                 :plugins plugins
                 :model model
                 :temperature temperature
                 :max-tokens max-tokens
                 :reasoning reasoning
                 :max-tool-calls max-tool-calls
                 :confirm-destructive confirm-destructive
                 :timeout timeout
                 :pre pre
                 :version version
                 :triggers triggers
                 :source 'skill
                 :source-file skill-file)))))))

;;;-----------------------------------------------
;;; Input Parsing
;;;-----------------------------------------------

(defun superchat-skills-parse-input (input)
  "Parse >skill-name syntax from INPUT.
Return (skill-name . user-input) cons or nil.

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
  "Parse the LLM match RESPONSE.
Return plist with :skill :confidence :reasoning, or nil."
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

Synchronous mode uses keyword matching."
  (let* ((registry (superchat-skills--get-registry))
         (match-prompt (superchat-skills--build-match-prompt user-input registry)))
    
    (if callback
        ;; Async mode
        (let* ((session-id (format "skills-match-%s" (gensym)))
               (acquisition
                (superchat-service-call nil 'editor-workspace 'acquire-buffer
                 session-id 'skills-match-protocol))
               (buffer (plist-get acquisition :buffer))
               delivery-entered
               close-attempted)
          (superchat-service-call nil 'editor-workspace 'validate-buffer buffer session-id)
          (cl-labels
              ((close-child ()
                 (let (close-condition close-result)
                   (condition-case condition
                       (setq close-result
                             (superchat-service-call nil 'editor-workspace 'close session-id))
                     ((error quit) (setq close-condition condition)))
                   (when (or close-condition
                             (not (eq close-result 'closed)))
                     ;; Diagnostics are secondary to any provider, callback,
                     ;; or submission condition already in flight.
                     (condition-case nil
                         (display-warning
                          'superchat
                          (format
                           (concat
                            "Skills match Session %s retained; retry with "
                            "M-x superchat-close-session-workspace")
                           session-id))
                       ((error quit) nil))))))
            (condition-case condition
                (let ((deliver
                       (lambda (response)
                         (unless close-attempted
                           ;; This single claim owns both callback delivery
                           ;; and the later close.  Publish it before either
                           ;; untrusted seam.
                           (setq delivery-entered t
                                 close-attempted t)
                           (unwind-protect
                               (let ((result
                                      (superchat-skills--parse-match-response
                                       response)))
                                 (setq superchat-skills--last-match-result result)
                                 (funcall callback
                                          (plist-get result :skill)
                                          (plist-get result :confidence)
                                          (plist-get result :reasoning)))
                             (close-child))))))
                  ;; S13a: an envelope with a callback Sink (contract §2.1
                  ;; Skills row: ephemeral Session, no tools).  The prepared
                  ;; match prompt travels as `:context :prepared-prompt'; the
                  ;; raw user input is what `intent.received' records.
                  (superchat-service-call nil 'driver 'submit
                   (list
                    :surface 'skills
                    :content user-input
                    :session-id session-id
                    :tools 'none
                    :sink (superchat-sink-callback
                           :id (format "skills-%08x" (random #x100000000))
                           :stream #'ignore
                           :final (lambda (_run result)
                                    (funcall deliver (or (plist-get result :text) "")))
                           :error (lambda (_run failure)
                                    (funcall deliver (or (plist-get failure :text) ""))))
                    :context (list :host-buffer buffer
                                   :prepared-prompt match-prompt)
                    :runtime-extras (list :record-conversation nil))))
              ((error quit)
               (unless delivery-entered
                 (unless close-attempted
                   (setq close-attempted t)
                   (close-child)))
               (signal (car condition) (cdr condition))))))
      
      ;; Sync mode - simplified: use keyword matching as fallback
      (superchat-skills--match-by-keywords user-input registry))))

(defun superchat-skills--match-by-keywords (user-input skills)
  "Fallback keyword matching of USER-INPUT against SKILLS list.
Return (skill-name . confidence) or nil."
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

;;;-----------------------------------------------
;;; Prompt Integration
;;;-----------------------------------------------

(defun superchat-skills-build-prompt
    (user-input skill-name &optional prepared-prompt)
  "Build the final prompt by combining user input with skill content.

USER-INPUT: The user's request text
SKILL-NAME: Name of the skill to apply
PREPARED-PROMPT: Optional prompt already built by the Turn pipeline

Returns the combined prompt string with variable substitution applied."
  (let* ((preset (superchat-skills-load skill-name))
         (skill-content (when preset (superchat-preset-skill-body preset)))
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
    (superchat-executor-wrap-with-context
     (or prepared-prompt user-input) skill-content)))

;;;-----------------------------------------------
;;; Invocation
;;;-----------------------------------------------

(defun superchat-skills-invoke (skill-name &optional user-input turn)
  "Invoke SKILL-NAME with optional USER-INPUT and TURN.

When TURN is provided, the skill's preset is applied to it (model,
tools, pre-hook).  This function prepares the prompt with skill
context and returns a result plist compatible with superchat's
command system.

Returns plist with:
  :type - :llm-query or :echo
  :prompt - The combined prompt with skill context
  :content - Echo message (if applicable)
  :skill - The skill name used
  :preset - The `superchat-preset' applied (when TURN is provided)"
  (cond
   ((superchat-skills-exists-p skill-name)
    (let* ((preset (superchat-skills-load skill-name))
           (prepared-prompt
            (and turn
                 (stringp (superchat-turn-prompt turn))
                 (not (string-empty-p (superchat-turn-prompt turn)))
                 (superchat-turn-prompt turn)))
           ;; Since the driver assembles prompts (v2 step 3) the Turn's
           ;; prompt slot is empty here; apply the general template to the
           ;; raw input ourselves so agent/plan skills keep it (review F5).
           (request-prompt (or prepared-prompt
                               (and (fboundp 'superchat-prompt--substitute-template)
                                    (superchat-prompt--substitute-template (or user-input "")))
                               user-input ""))
           ;; For agent/plan presets the body is a persona and travels
           ;; as the system prompt (written into the turn by
           ;; `superchat-preset-apply' below) — embedding it in the
           ;; user prompt as well would send it twice.  Prompt-type
           ;; skills keep the template-combining behavior.
           (combined-prompt
            (if (and preset
                     (memq (superchat-preset-type preset) '(agent plan)))
                request-prompt
              (superchat-skills-build-prompt
               (or user-input "")
               skill-name
               request-prompt))))
      (message "🎯 Using skill: %s" skill-name)
      (when (and turn preset)
        (superchat-preset-apply preset turn))
      `(:type :llm-query
        :prompt ,combined-prompt
        :skill ,skill-name
        ,@(when preset `(:preset ,preset)))))

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

(defun superchat-skills-inspect (skill-name)
  "Inspect SKILL-NAME's details."
  (interactive (list (completing-read "Skill: " (superchat-skills-get-available))))
  (let* ((preset (superchat-skills-load skill-name))
         (content (when preset (superchat-preset-skill-body preset)))
         (file (superchat-skills--find-file skill-name)))
    (with-output-to-temp-buffer "*Skill Inspector*"
      (princ (format "=== Skill: %s ===\n\n" skill-name))
      (princ (format "File: %s\n" file))
      (princ (format "Exists: %s\n\n" (superchat-skills-exists-p skill-name)))
      (when preset
        (princ (format "Type: %s\n" (superchat-preset-type preset)))
        (princ (format "Tools: %s\n" (or (superchat-preset-tools preset) "none")))
        (princ (format "Model: %s\n\n" (or (superchat-preset-model preset) "default"))))
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

(defun superchat-skills--github-token ()
  "Return GitHub token from config or environment variable, or nil."
  (or superchat-skills-github-token
      (getenv "GITHUB_TOKEN")))

(defun superchat-skills--github-api-get (url)
  "Fetch URL synchronously and return parsed JSON.
Returns nil on error."
  (require 'url)
  (require 'json)
  (let* ((token (superchat-skills--github-token))
         (url-request-extra-headers
          (append '(("Accept" . "application/vnd.github.v3+json")
                    ("User-Agent" . "superchat-emacs"))
                  (when token
                    (list (cons "Authorization" (concat "Bearer " token)))))))
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

(defun superchat-skills--github-response-ok-p (response)
  "Return non-nil if RESPONSE is a valid GitHub API file list (not an error)."
  (and (listp response)
       (not (null response))
       (listp (car response))   ; entries are alists, errors are cons pairs
       (condition-case nil
           (not (assq 'message response))
         (error nil))))

(defun superchat-skills--github-response-error (response)
  "Return error message string from RESPONSE, or nil if not an error."
  (cond
   ((and (consp response) (eq (car response) 'message))
    (cdr response))
   ((and (listp response)
         (condition-case nil (assq 'message response) (error nil)))
    (cdr (assq 'message response)))))

(defun superchat-skills--strip-yaml-frontmatter (content)
  "Strip YAML frontmatter from CONTENT if present.
Frontmatter is a block delimited by --- lines at the start of the file."
  (if (string-match "\\`[ \t\n]*---[ \t]*\n" content)
      (let ((body-start (match-end 0)))
        (if (string-match "^---[ \t]*\n" content body-start)
            (string-trim (substring content (match-end 0)))
          content))
    content))

(cl-defun superchat-skills--github-collect-files (entries ext-regexp &optional parent-name)
  "Recursively collect skill files from ENTRIES matching EXT-REGEXP.
PARENT-NAME is the containing directory name when recursing.

Supports two layouts:
- Claude Code format: skill-name/SKILL.md  → skill saved as \"skill-name.md\"
- Flat format: skill-name.md               → skill saved as-is

Returns alist of (SAVE-NAME . (DOWNLOAD-URL . STRIP-FRONTMATTER-P))."
  ;; Claude Code format: directory containing SKILL.md
  (when parent-name
    (let ((skill-md (cl-find-if (lambda (e)
                                  (and (string= (alist-get 'type e "") "file")
                                       (string= (alist-get 'name e "") "SKILL.md")))
                                entries)))
      (when skill-md
        (cl-return-from superchat-skills--github-collect-files
          (list (cons (concat parent-name ".md")
                      (cons (alist-get 'download_url skill-md) t)))))))
  ;; Flat / recursive collection
  (let (result)
    (dolist (entry entries)
      (let ((type (alist-get 'type entry ""))
            (name (alist-get 'name entry ""))
            (url  (alist-get 'url  entry))
            (dl   (alist-get 'download_url entry)))
        (cond
         ((and (string= type "file")
               (string-match-p ext-regexp name))
          (push (cons name (cons dl nil)) result))
         ((string= type "dir")
          (let ((sub (superchat-skills--github-api-get url)))
            (when (superchat-skills--github-response-ok-p sub)
              (setq result (append result
                                   (superchat-skills--github-collect-files
                                    sub ext-regexp name)))))))))
    result))

(defun superchat-skills--github-fetch-file-list (user repo &optional branch)
  "Fetch skill file list from GitHub repo USER/REPO.
If BRANCH is non-nil, use that ref.  Auto-detects skills/ subdirectory.
Recurses into subdirectories to find skill files.
Returns alist of (NAME . DOWNLOAD-URL), or signals an error string."
  (let* ((ref-param (if branch (format "?ref=%s" branch) ""))
         (skills-url (format "https://api.github.com/repos/%s/%s/contents/skills%s"
                             user repo ref-param))
         (root-url (format "https://api.github.com/repos/%s/%s/contents/%s"
                           user repo ref-param))
         (skills-response (superchat-skills--github-api-get skills-url))
         (entries (if (superchat-skills--github-response-ok-p skills-response)
                      skills-response
                    (superchat-skills--github-api-get root-url)))
         (ext-regexp "\\.\\(md\\|org\\|txt\\)$"))
    (let ((err (superchat-skills--github-response-error entries)))
      (when err
        (error "GitHub API error: %s" err)))
    (when (superchat-skills--github-response-ok-p entries)
      (superchat-skills--github-collect-files entries ext-regexp))))

(defun superchat-skills--github-download-file (url dest-path &optional strip-frontmatter)
  "Download file from URL and write to DEST-PATH.
When STRIP-FRONTMATTER is non-nil, remove YAML frontmatter from content.
Returns t on success, nil on error."
  (require 'url)
  (condition-case err
      (with-current-buffer (url-retrieve-synchronously url t nil 30)
        (goto-char (point-min))
        (when (re-search-forward "^$" nil t)
          (forward-char 1)
          (let ((content (buffer-substring-no-properties (point) (point-max))))
            (when strip-frontmatter
              (setq content (superchat-skills--strip-yaml-frontmatter content)))
            (with-temp-file dest-path
              (insert content))
            t)))
    (error
     (message "superchat: download error for %s: %s" url (error-message-string err))
     nil)))

(cl-defun superchat-skills-install (spec)
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
           (file-list (condition-case err
                          (superchat-skills--github-fetch-file-list user repo branch)
                        (error
                         (cl-return-from superchat-skills-install
                           `(:type :echo :content ,(error-message-string err)))))))
      (unless file-list
        (cl-return-from superchat-skills-install
          `(:type :echo :content ,(format "No skill files found in %s/%s%s"
                                          user repo
                                          (if branch (format " (branch: %s)" branch) "")))))
      (superchat-skills-initialize)
      (let ((installed '())
            (failed '()))
        (dolist (entry file-list)
          (let* ((name            (car entry))
                 (url             (cadr entry))
                 (strip-frontmatter (cddr entry))
                 (dest (expand-file-name name superchat-skills-directory)))
            (if (superchat-skills--github-download-file url dest strip-frontmatter)
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
  "Initialize the skills system on first use for its configured directory."
  (let ((directory
         (file-name-as-directory
          (expand-file-name superchat-skills-directory))))
    (unless (equal directory superchat-skills--initialized-directory)
      (superchat-skills--ensure-directory)
      (setq superchat-skills--initialized-directory directory))
    superchat-skills-directory))

(provide 'superchat-skills)

;;; superchat-skills.el ends here
