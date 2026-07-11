;;; superchat-workflow.el --- Multi-step linear recipes (async engine) -*- lexical-binding: t; -*-

;;; Commentary:
;; v1.2 restores independent .workflow files with an async linear
;; execution engine.  Each .workflow file is a list of steps executed
;; sequentially through `superchat--llm-generate-answer', chained via
;; completion callbacks so Emacs never blocks.
;;
;; Directory: <superchat-data-directory>/workflow/
;; Format: one step per line, blank lines and # comments skipped
;; Variables: $input $lang $date $result $stepN
;; Per-step annotations:
;;   @model    — one-shot model override; must be the first token of the line
;;   /command  — user-defined command (prompt template); must start the line
;;               (after an optional @model); its args bind to $input
;;   #path     — context file; the token must look like a path (contain
;;               `.' or `/'), so prose like "issue #42" is left alone
;;
;; Async engine replaces the v0.7 synchronous placeholder that never
;; actually called the LLM.  The old `superchat-workflow-execute'
;; (SKILL.md sub-type path) is removed.  Invocation: `>>name [args]'
;; (`>' is reserved for skills, `>>' for workflows) or
;; `/workflow <name> [args]'.

;;; Code:

(require 'cl-lib)
(require 'superchat-core)

;; ── Forward declarations ──
(defvar superchat-lang)
(defvar superchat-data-directory)
(defvar superchat-buffer-name)
(declare-function superchat--llm-generate-answer "superchat"
                  (prompt callback stream-callback &optional target-model context-files))
(declare-function superchat--insert-prompt "superchat-render" ())
(declare-function superchat--record-message "superchat" (role content))
(declare-function superchat--lookup-command-template "superchat" (command))

;;;-----------------------------------------------
;;; Part A — .workflow file loading
;;;-----------------------------------------------

(defcustom superchat-workflow-directory
  (expand-file-name "workflow/"
                    (if (boundp 'superchat-data-directory)
                        superchat-data-directory
                      (expand-file-name "superchat/" user-emacs-directory)))
  "Directory containing .workflow recipe files.
Each file is a linear list of steps, one per line.
Blank lines and `#' comment lines are skipped.
See `superchat-workflow-execute-async' for execution."
  :type 'directory
  :group 'superchat)

(defun superchat-workflow--ensure-directory ()
  "Create the workflow directory if it does not exist."
  (unless (file-directory-p superchat-workflow-directory)
    (make-directory superchat-workflow-directory t)))

(defun superchat-workflow--list ()
  "Return list of available workflow names (without .workflow extension)."
  (superchat-workflow--ensure-directory)
  (when (file-directory-p superchat-workflow-directory)
    (mapcar #'file-name-base
            (directory-files superchat-workflow-directory t "\\.workflow\\'"))))

(defun superchat-workflow--exists-p (name)
  "Return non-nil if a .workflow file named NAME exists."
  (member name (superchat-workflow--list)))

(defun superchat-workflow--file-path (name)
  "Return the full path for .workflow file NAME."
  (expand-file-name (concat name ".workflow") superchat-workflow-directory))

(defun superchat-workflow--load (name)
  "Load the .workflow file for NAME as a string, or nil if not found."
  (let ((file (superchat-workflow--file-path name)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun superchat-workflow--completion-list ()
  "Return list of workflow names for completion (used by /workflow command)."
  (superchat-workflow--list))

;;;-----------------------------------------------
;;; Part B — File format & parsing
;;;-----------------------------------------------

(defun superchat-workflow-parse-steps (body)
  "Split BODY into a list of step strings.
Skips blank lines and comment lines (lines whose first
non-whitespace character is `#').
Preserves $input / $lang / $date / $result / $stepN for late binding."
  (let ((steps '()))
    (dolist (line (split-string (or body "") "\n"))
      (let ((trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed)
                    (string-prefix-p "#" trimmed))
          (push trimmed steps))))
    (nreverse steps)))

(defun superchat-workflow--parse-line (line)
  "Parse a single step LINE into a plist (:model :command :contexts :prompt).

Extracts annotations:
  @model-name   → :model (string or nil)
  /command args → :command (string or nil)
  #file-path    → :contexts (list of strings)

Everything else becomes :prompt.
When :prompt is empty and :command is set, :prompt defaults to :command."
  (let ((model nil)
        (command nil)
        (contexts nil)
        (remaining line))
    ;; Extract @model — only as the first token of the line, so inline
    ;; mentions ("feedback from @john.doe") stay in the prompt.
    (when (string-match "\\`@\\([a-zA-Z0-9_.-]+\\)\\s-*" remaining)
      (setq model (match-string 1 remaining))
      (setq remaining (replace-match "" t t remaining 0)))
    ;; Extract /command args — only at the start of the line (after an
    ;; optional @model), so prose containing "/" is untouched.  Args
    ;; stop at `#' so #context refs survive.
    (when (string-match "\\`\\s-*/\\([a-zA-Z0-9_-]+\\)\\(?:\\s-+\\([^#]*\\)\\)?" remaining)
      (let ((cmd (match-string 1 remaining))
            (args (string-trim (or (match-string 2 remaining) ""))))
        (setq command (if (string-empty-p args) cmd (concat cmd " " args)))
        (setq remaining (replace-match "" t t remaining 0))))
    ;; Extract #context references — the token must look like a file
    ;; path (contain `.' or `/') so "issue #42" stays in the prompt.
    (while (string-match "\\(?:^\\|\\s-\\)#\"?\\([^[:space:]#\"]*[./][^[:space:]#\"]*\\)\"?" remaining)
      (push (match-string 1 remaining) contexts)
      (setq remaining (replace-match "" t t remaining 0)))
    ;; Trim remaining text
    (setq remaining (string-trim remaining))
    (list :model model
          :command command
          :contexts (nreverse contexts)
          :prompt (cond
                   ((not (string-empty-p remaining)) remaining)
                   (command command)
                   (t "")))))

(defun superchat-workflow--substitute-vars (text vars step-results)
  "Substitute $input, $lang, $date, $result, $stepN in TEXT.

VARS is a plist (:input ... :lang ... :date ...).
STEP-RESULTS is a list of strings, 0-indexed
\(step-results[0] = step 1 output).

Returns TEXT with all recognized variables expanded."
  (let ((result text))
    ;; Built-in vars: $input $lang $date
    (dolist (var '(:input :lang :date))
      (let* ((key (substring (symbol-name var) 1))
             (val (or (plist-get vars var) "")))
        (setq result (replace-regexp-in-string
                      (concat "\\$" (regexp-quote key))
                      val result t t))))
    ;; $result = most recent step output
    (let ((last-result (or (car (last step-results)) "")))
      (setq result (replace-regexp-in-string
                    "\\$result" last-result result t t)))
    ;; $stepN = step N output (1-indexed).  Replace from the highest N
    ;; down so "$step1" cannot clobber the prefix of "$step10".
    (cl-loop for i downfrom (length step-results) to 1
             do (let ((step-key (format "$step%d" i))
                      (step-val (or (nth (1- i) step-results) "")))
                  (setq result (replace-regexp-in-string
                                (regexp-quote step-key)
                                step-val result t t))))
    result))

;; Backward-compat alias (used by test-workflow.el)
(defalias 'superchat-workflow-substitute 'superchat-workflow--substitute-vars)

;;;-----------------------------------------------
;;; Part C — Async linear executor
;;;-----------------------------------------------

(defun superchat-workflow-execute-async (workflow-name &optional argument)
  "Execute WORKFLOW-NAME asynchronously with optional ARGUMENT.

Each step chains into the next via the completion callback of
`superchat--llm-generate-answer'.  Emacs remains interactive
throughout execution.  Outputs render directly into the superchat
buffer.

Interactively, prompts for workflow name with completion.
ARGUMENT is bound to $input in workflow steps."
  (interactive
   (list (completing-read "Workflow: " (superchat-workflow--list))
         (read-string "Argument (optional): ")))
  (let* ((body (superchat-workflow--load workflow-name)))
    (unless body
      (user-error "Workflow '%s' not found in %s"
                  workflow-name superchat-workflow-directory))
    (let* ((raw-steps (superchat-workflow-parse-steps body))
           (base-dir default-directory)
           ;; Resolve #context paths once, against the invocation
           ;; directory — `default-directory' inside async callbacks
           ;; is whatever buffer happens to be current at that moment.
           (parsed-steps
            (mapcar (lambda (raw)
                      (let ((step (superchat-workflow--parse-line raw)))
                        (plist-put step :contexts
                                   (mapcar (lambda (c)
                                             (expand-file-name c base-dir))
                                           (plist-get step :contexts)))))
                    raw-steps))
           (vars (list :input (or argument "")
                       :lang (or superchat-lang "English")
                       :date (format-time-string "%Y-%m-%d"))))
      (unless parsed-steps
        (user-error "Workflow '%s' has no executable steps" workflow-name))
      ;; Write workflow header to buffer
      (superchat-workflow--render-workflow-header workflow-name
                                                  (length parsed-steps))
      ;; Kick off the callback chain
      (superchat-workflow--run-step 0 parsed-steps vars nil))))

(defun superchat-workflow--render-workflow-header (name step-count)
  "Render the workflow start header into the superchat buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "** Workflow: %s (%d steps)\n" name step-count)))))

(defun superchat-workflow--run-step (i steps vars step-results)
  "Execute step I of STEPS with VARS and STEP-RESULTS.

When i >= (length steps), finalize the workflow.
Otherwise prepare the step prompt, render its header, and chain
the next step via the completion callback of
`superchat--llm-generate-answer'.

STEP-RESULTS accumulates each step's LLM output as a string,
accessible to later steps via $result and $stepN."
  (if (>= i (length steps))
      ;; All steps done
      (superchat-workflow--finalize steps step-results)
    (let* ((step (nth i steps))
           (model (plist-get step :model))
           (command (plist-get step :command))
           (contexts (plist-get step :contexts))
           (raw-prompt (plist-get step :prompt))
           (substituted (superchat-workflow--resolve-prompt
                         command raw-prompt vars step-results))
           (total (length steps))
           (step-num (1+ i)))
      ;; Render step header
      (superchat-workflow--render-step-header step-num total)
      (condition-case err
          (superchat--llm-generate-answer
           substituted
           ;; Completion callback — chain to next step
           (lambda (answer)
             (let ((answer-str (if (stringp answer)
                                   answer
                                 (format "%s" answer))))
               ;; `superchat--llm-generate-answer' reports failures as
               ;; "[Error: ...]" strings through this same callback —
               ;; stop the chain instead of feeding them into $result.
               (if (string-prefix-p "[Error" answer-str)
                   (superchat-workflow--fail-step step-num total answer-str
                                                  step-results)
                 ;; Record in conversation history for downstream
                 ;; context continuity across steps.
                 (with-current-buffer (get-buffer-create superchat-buffer-name)
                   (superchat--record-message "assistant" answer-str))
                 ;; Chain recursively
                 (superchat-workflow--run-step
                  (1+ i) steps vars
                  (append step-results (list answer-str))))))
           ;; Stream callback — custom lightweight renderer for workflow
           #'superchat-workflow--stream-chunk
           model
           ;; context-files: hook for future llm.el support
           contexts)
        (error
         ;; Step failure — render error and stop
         (superchat-workflow--fail-step step-num total
                                        (error-message-string err)
                                        step-results))))))

(defun superchat-workflow--resolve-prompt (command raw-prompt vars step-results)
  "Return the final prompt for a step.

When COMMAND (\"name args...\") names a command with a prompt
template (see `superchat--lookup-command-template'), the template
is expanded with $input bound to the command args.  Otherwise
RAW-PROMPT is used.  Workflow variables are expanded either way."
  (let* ((cmd-name (and command
                        (car (split-string command "[[:space:]]+" t))))
         (template (and cmd-name
                        (fboundp 'superchat--lookup-command-template)
                        (superchat--lookup-command-template cmd-name))))
    ;; Built-in commands map to function symbols, not templates —
    ;; only a string template can be expanded here.
    (if (stringp template)
        (let ((args (superchat-workflow--substitute-vars
                     (string-trim (substring command (length cmd-name)))
                     vars step-results)))
          (replace-regexp-in-string "\\$input" args template t t))
      (superchat-workflow--substitute-vars raw-prompt vars step-results))))

(defun superchat-workflow--stream-chunk (chunk)
  "Append CHUNK into the superchat buffer after the step header.
Uses `superchat-streaming-pending' face for raw markdown
mid-stream so font-lock does not re-interpret it."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert chunk)
        (put-text-property start (point) 'face 'superchat-streaming-pending)))))

(defun superchat-workflow--render-step-header (step-num total)
  "Write \"** Workflow: step N/T\" header into the superchat buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "** Step %d/%d\n" step-num total)))))

(defun superchat-workflow--fail-step (step-num total error-msg _step-results)
  "Render a failure notice for step STEP-NUM/TOTAL with ERROR-MSG.
Writes the error into the buffer, then inserts a fresh prompt."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "** Workflow FAILED at step %d/%d\n" step-num total))
      (insert (format "Error: %s\n" error-msg))))
  (superchat--insert-prompt))

(defun superchat-workflow--finalize (steps _step-results)
  "Render workflow completion summary into the buffer.
Inserts a completion headline and a fresh prompt."
  (let ((total (length steps)))
    (with-current-buffer (get-buffer-create superchat-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "** Workflow complete (%d/%d steps)\n" total total)))))
  (superchat--insert-prompt))

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
    (dolist (file (directory-files legacy-dir t "\\.workflow\\'"))
      (let ((name (superchat-workflow-import-legacy file target)))
        (when name (push name imported))))
    (message "superchat-workflow: imported %d legacy workflows from %s"
             (length imported) legacy-dir)
    imported))

(provide 'superchat-workflow)
;;; superchat-workflow.el ends here
