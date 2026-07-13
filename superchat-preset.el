;;; superchat-preset.el --- Preset abstraction for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; A "preset" bundles a system prompt, tool set, model override, and
;; execution type (prompt / agent / plan).  Skills are one source of
;; presets; built-in presets (researcher, executor, etc.) can be defined
;; here as well.

;;; Code:

(require 'cl-lib)
(require 'superchat-core)

;; ═══════════════════════════════════════════════════════════
;; Preset struct
;; ═══════════════════════════════════════════════════════════

(cl-defstruct (superchat-preset
               (:constructor superchat-preset--create)
               (:copier superchat-preset-copy))
  "A bundle of configuration for a Superchat session or turn.

Slots:
  :name           Symbol or string identifying the preset.
  :description    Human-readable description.
  :type           One of `prompt', `agent', `plan', `workflow'.
  :skill-body     The prompt text / skill content.
  :tools          List of tool names to expose to the LLM; nil to
                  inherit the global tool set; the symbol `none' for
                  an explicitly empty tool set (tools: [] in YAML).
  :model          Optional model override string.
  :pre            Optional function to run before the preset is applied.
  :version        Preset version string.
  :triggers       Optional list of trigger strings.
  :source         Where the preset came from (`skill', `builtin', etc.).
  :source-file    File path when loaded from disk."
  (name nil :read-only t)
  (description "")
  (type 'prompt)
  (skill-body "")
  (tools nil)
  (model nil)
  (pre nil)
  (version "1.0")
  (triggers nil)
  (source nil)
  (source-file nil))

;; ═══════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════

(defun superchat-preset-type-p (preset type)
  "Return non-nil if PRESET's type equals TYPE (compared as symbols)."
  (and preset
       (superchat-preset-p preset)
       (eq (superchat-preset-type preset) type)))

(defun superchat-preset-agent-p (preset)
  "Return non-nil if PRESET is an agent preset."
  (superchat-preset-type-p preset 'agent))

(defun superchat-preset-plan-p (preset)
  "Return non-nil if PRESET is a planning preset."
  (superchat-preset-type-p preset 'plan))

(defun superchat-preset-workflow-p (preset)
  "Return non-nil if PRESET is a workflow preset."
  (superchat-preset-type-p preset 'workflow))

(defun superchat-preset--resolve-pre (pre-spec)
  "Resolve PRE-SPEC to a callable function or nil.
PRE-SPEC may be a function, a symbol, or a string naming a function.
Lambdas read from strings are rejected unless they are already
functions; only named functions are accepted for safety."
  (cond
   ((functionp pre-spec) pre-spec)
   ((symbolp pre-spec) (when (fboundp pre-spec) pre-spec))
   ((stringp pre-spec)
    (let ((sym (intern-soft pre-spec)))
      (when (and sym (fboundp sym)) sym)))
   (t nil)))

(defun superchat-preset-run-pre (preset)
  "Run PRE hook for PRESET if present and callable.
Returns the hook's return value, or nil if no hook or not callable.
Signals a warning when a hook is specified but cannot be resolved."
  (when-let* ((pre (superchat-preset-pre preset)))
    (let ((fn (superchat-preset--resolve-pre pre)))
      (cond
       (fn (funcall fn))
       (t
        (display-warning 'superchat-preset
                         (format "Preset %s has unresolved pre hook: %S"
                                 (superchat-preset-name preset) pre)
                         :warning)
        nil)))))

(defun superchat-preset-apply (preset turn)
  "Apply PRESET to TURN, mutating turn slots.
Sets :preset, :target-model, :tools, and runs the :pre hook.
Returns the modified TURN.

Model precedence: an explicit per-turn @model (already parsed into
TURN by the core pipeline) wins over the preset's model — the preset
only fills the slot when it is still empty.

For agent and plan presets, the skill body is the persona: it is
prepended to TURN's system-prompt so it reaches the LLM as system
context on every turn, not only on explicit skill invocation.
Prompt-type presets keep their body out of the system prompt — for
them the body is a prompt template handled by the skills layer."
  (when preset
    (setf (superchat-turn-preset turn) preset)
    (when (and (superchat-preset-model preset)
               (null (superchat-turn-target-model turn)))
      (setf (superchat-turn-target-model turn)
            (superchat-preset-model preset)))
    (when (superchat-preset-tools preset)
      (setf (superchat-turn-tools turn)
            (superchat-preset-tools preset)))
    (when (memq (superchat-preset-type preset) '(agent plan))
      (let ((body (string-trim (or (superchat-preset-skill-body preset) ""))))
        (unless (string-empty-p body)
          (let ((existing (or (superchat-turn-system-prompt turn) "")))
            (setf (superchat-turn-system-prompt turn)
                  (if (string-empty-p existing)
                      body
                    (concat body "\n\n" existing)))))))
    (superchat-preset-run-pre preset))
  turn)

(defun superchat-preset-make-system-prompt (preset)
  "Build a system prompt string from PRESET's skill body.
Returns the skill body as-is; callers may prepend/append further
instructions."
  (or (superchat-preset-skill-body preset) ""))

;; ═══════════════════════════════════════════════════════════
;; Frontmatter parser (shared by skill loaders)
;; ═══════════════════════════════════════════════════════════

(defconst superchat-preset-frontmatter-field-order
  '("name" "description" "version" "type" "tools" "model" "pre" "triggers")
  "Canonical YAML key order for SKILL.md export.")

(defun superchat-preset-parse-frontmatter (content)
  "Parse YAML frontmatter from CONTENT string.
Returns an alist of (KEY . VALUE) pairs, or nil if no frontmatter block.
Trims values, supports quoted strings, and supports
`key: [\"a\", \"b\"]' list shorthand for list-valued keys."
  (when (and (stringp content)
             (string-match "^---\\s-*\n\\(\\(.\\|\n\\)*\\)---\\s-*\n" content))
    (let ((raw (match-string 1 content))
          (alist '()))
      (dolist (line (split-string raw "\n"))
        (when (string-match "^\\([a-zA-Z0-9_-]+\\):\\s-*\\(.+\\)$" line)
          (let* ((key (match-string 1 line))
                 (val (match-string 2 line)))
            ;; List shorthand: triggers: ["a", "b"]
            (cond
             ((string-match "^\\[\\(.*\\)\\]$" val)
              (let ((items (mapcar (lambda (s)
                                     (string-trim (string-trim s "\"" "\"")))
                                   (split-string (match-string 1 val) "," t))))
                (push (cons key items) alist)))
             ;; Quoted string: "value"
             ((string-match "^\"\\(.*\\)\"$" val)
              (push (cons key (match-string 1 val)) alist))
             ;; Plain string
             (t
              (push (cons key (string-trim val)) alist))))))
      (nreverse alist))))

(defun superchat-preset-from-frontmatter (content &optional source-file)
  "Create a `superchat-preset' from SKILL.md CONTENT.
Optional SOURCE-FILE records the file path.  Returns nil if no
frontmatter block is present."
  (when (string-match "^---\\s-*\n\\(\\(.\\|\n\\)*\\)---\\s-*\n" content)
    (let ((body-start (match-end 0))
          (alist (superchat-preset-parse-frontmatter content)))
      (when alist
        (superchat-preset-from-plist
         (list :name (cdr (assoc "name" alist))
               :description (cdr (assoc "description" alist))
               :type (or (cdr (assoc "type" alist)) "prompt")
               :body (substring content body-start)
               ;; Distinguish `tools: []' (explicitly no tools → the
               ;; symbol `none') from an absent tools key (nil →
               ;; inherit the global tool set).  The parser collapses
               ;; an empty list literal to nil, so recover the
               ;; distinction from key presence.
               :tools (let ((entry (assoc "tools" alist)))
                        (cond ((null entry) nil)
                              ((null (cdr entry)) 'none)
                              (t (cdr entry))))
               :pre (cdr (assoc "pre" alist))
               :model (cdr (assoc "model" alist))
               :version (or (cdr (assoc "version" alist)) "1.0")
               :triggers (cdr (assoc "triggers" alist))
               :source 'skill
               :source-file source-file))))))

(defun superchat-preset-from-plist (plist)
  "Create a `superchat-preset' from PLIST.
Accepts keys :name :description :type :body :tools :model
:pre :version :triggers :source :source-file.
:tools may be the symbol `none' for an explicitly empty tool set."
  (let* ((type-str (plist-get plist :type))
         (type (cond
                ((symbolp type-str) type-str)
                ((stringp type-str) (intern (downcase type-str)))
                (t 'prompt)))
         (tools (plist-get plist :tools))
         (pre (plist-get plist :pre)))
    (superchat-preset--create
     :name (plist-get plist :name)
     :description (or (plist-get plist :description) "")
     :type type
     :skill-body (or (plist-get plist :body)
                     (plist-get plist :skill-body)
                     "")
     :tools (cond
             ((null tools) nil)
             ((eq tools 'none) 'none)
             ((stringp tools) (mapcar #'string-trim
                                      (split-string tools "[, ]" t "\\s*")))
             ((listp tools) (mapcar (lambda (x)
                                      (string-trim
                                       (if (symbolp x) (symbol-name x) x)))
                                    tools))
             (t nil))
     :model (plist-get plist :model)
     :pre pre
     :version (or (plist-get plist :version) "1.0")
     :triggers (plist-get plist :triggers)
     :source (plist-get plist :source)
     :source-file (plist-get plist :source-file))))

(provide 'superchat-preset)

;;; superchat-preset.el ends here
