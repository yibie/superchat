;;; superchat-workflow.el --- Multi-step linear recipes (async engine) -*- lexical-binding: t; -*-

;;; Commentary:
;; v1.2 restores independent .workflow files with an async linear
;; execution engine.  Each .workflow file is a list of steps executed
;; sequentially through `superchat-runtime-submit', chained via delivery
;; callbacks so Emacs never blocks.
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
(require 'json)
(require 'superchat-core)
(require 'superchat-runtime)
(require 'superchat-closure)
(require 'superchat-projection)
(require 'superchat-tape)

;; ── Forward declarations ──
(defvar superchat-lang)
(defvar superchat-data-directory)
(defvar superchat-buffer-name)
(defvar superchat-mode)
(defvar superchat--session-id nil)
(defvar superchat--active-authority-policy nil)
(defvar superchat--team-id nil)
(defvar superchat--team-actor nil)
(defvar superchat--tool-execution-context nil)
(defvar superchat--workspace-binding nil)
(declare-function superchat--insert-prompt "superchat-render" ())
(declare-function superchat--record-message
                  "superchat" (role content &optional existing-tape-id))
(declare-function superchat--lookup-command-template "superchat" (command))
(declare-function superchat--session-runtime-configuration "superchat" ())
(declare-function superchat--subagent-run-async
                  "superchat-subagent"
                  (preset-name task context callback &optional depth))
(declare-function superchat--team-state "superchat-subagent" (&optional team-id))
(declare-function superchat-team-message
                  "superchat-subagent"
                  (action &optional to content since-id))
(declare-function superchat-team-tasks "superchat-subagent" (&optional team-id))
(declare-function superchat-subagent-handoff-decision
                  "superchat-subagent" (session-id run-id &optional trace))
(declare-function superchat-workspace-bind
                  "superchat-workspace" (workspace-id &optional root session-id))
(declare-function superchat-workspace-artifacts
                  "superchat-workspace" (&optional workspace-id replay))
(declare-function superchat-workspace-artifact-delivery-decision
                  "superchat-workspace"
                  (session-id run-id reference &optional trace))
(declare-function superchat-workspace-read
                  "superchat-workspace"
                  (&optional artifact-id workspace-id expected-revision))
(declare-function superchat-view-session-ids "superchat-tape-view" ())

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

(defun superchat-workflow--step-number (actor-id)
  "Return the one-based workflow step number encoded in ACTOR-ID."
  (when (and (stringp actor-id)
             (string-match
              "\\`workflow-step-\\([1-9][0-9]*\\)\\'" actor-id))
    (string-to-number (match-string 1 actor-id))))

(defun superchat-workflow--instance-projection-apply (state event)
  "Reduce a Runtime EVENT into Workflow Instance STATE."
  (let* ((actor-id (plist-get event :actor-id))
         (step-id (superchat-workflow--step-number actor-id))
         (instance-id (plist-get event :correlation-id))
         (run-id (plist-get event :run-id)))
    (if (and (equal "run.started" (plist-get event :type))
             step-id
             (stringp instance-id)
             (stringp run-id))
        ;; ponytail: linear workflows are small; keep a flat replay view until
        ;; measured history size justifies an indexed materialized projection.
        (cons (list :instance-id instance-id
                    :step-id step-id
                    :actor-id actor-id
                    :run-id run-id
                    :turn-id (plist-get event :turn-id))
              (cl-remove-if
               (lambda (step)
                 (and (equal instance-id (plist-get step :instance-id))
                      (= step-id (plist-get step :step-id))))
               state))
      state)))

(superchat-projection-register
 'workflow-instance
 :version 1
 :initial nil
 :apply #'superchat-workflow--instance-projection-apply)

(defun superchat-workflow--resume-decision (session-id run-id &optional trace)
  "Return the Closure-backed resume decision for RUN-ID in SESSION-ID.
The result's `:action' is one of `advance', `wait', `manual-review', or
`stop'.  TRACE is accepted for callers that already replayed the Session."
  (if (not (and (stringp run-id) (not (string-empty-p run-id))))
      (list :action 'manual-review :run-id run-id :reason 'run-not-found)
    (let* ((trace (or trace (superchat-tape-replay-events session-id)))
           (well-formed
            (superchat-closure-judge
             (list :kind 'trace-well-formed :run-id run-id) trace))
           (well-verdict (plist-get well-formed :verdict))
           terminal action)
      (cond
       ((eq well-verdict 'invalid)
        (setq action 'stop))
       ((not (eq well-verdict 'valid))
        (setq action 'manual-review))
       (t
        (setq terminal
              (superchat-closure-judge
               (list :kind 'durable-run-terminal :run-id run-id) trace))
        (setq action
              (pcase (plist-get terminal :verdict)
                ('proved
                 (if (eq 'completed
                         (plist-get (plist-get terminal :judgment)
                                    :terminal-kind))
                     'advance
                   'stop))
                ('open 'wait)
                (_ 'manual-review)))))
      (list :action action
            :run-id run-id
            :trace-judgment well-formed
            :terminal-judgment terminal))))

(defun superchat-workflow-instance-view (session-id instance-id)
  "Return the Tape-derived resume view for workflow INSTANCE-ID.
Each step maps to its latest Run, Closure judgment, and resume decision."
  (let* ((trace (superchat-tape-replay-events session-id))
         (projected
          (superchat-projection-replay 'workflow-instance trace))
         (steps
          (sort
           (cl-remove-if-not
            (lambda (step)
              (equal instance-id (plist-get step :instance-id)))
            projected)
           (lambda (left right)
             (< (plist-get left :step-id)
                (plist-get right :step-id))))))
    (list
     :instance-id instance-id
     :session-id session-id
     :steps
     (mapcar
      (lambda (step)
        (let* ((decision
                (superchat-workflow--resume-decision
                 session-id (plist-get step :run-id) trace))
               (judgment
                (or (plist-get decision :terminal-judgment)
                    (plist-get decision :trace-judgment))))
          (append step
                  (list :closure-judgment judgment
                        :resume-decision (plist-get decision :action)
                        :resume decision))))
      steps))))

(defun superchat-workflow--resume-decision-for-step
    (session-id correlation-id actor-id)
  "Return the resume decision for ACTOR-ID in CORRELATION-ID.
The latest matching Run is selected from SESSION-ID's ordered Tape."
  (let* ((view
          (superchat-workflow-instance-view session-id correlation-id))
         (step
          (cl-find actor-id (plist-get view :steps)
                   :key (lambda (item) (plist-get item :actor-id))
                   :test #'equal)))
    (or (plist-get step :resume)
        (list :action 'manual-review
              :run-id nil
              :reason 'run-not-found))))

(defun superchat-workflow-execute-async (workflow-name &optional argument)
  "Execute WORKFLOW-NAME asynchronously with optional ARGUMENT.

Each step chains into the next via Runtime delivery callbacks.
Emacs remains interactive throughout execution.  Outputs render
directly into the superchat buffer.

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
      (let ((superchat-buffer-name
             (if (bound-and-true-p superchat-mode)
                 (buffer-name)
               superchat-buffer-name)))
        (let ((correlation-id
               (format "workflow-%s-%s-%04x"
                       workflow-name
                       (format-time-string "%Y%m%d-%H%M%S-%6N")
                       (random 65536))))
          ;; Write workflow header to buffer and kick off the callback chain.
          (superchat-workflow--render-workflow-header workflow-name
                                                      (length parsed-steps))
          (superchat-workflow--run-step
           0 parsed-steps vars nil correlation-id))))))

(defun superchat-workflow--render-workflow-header (name step-count)
  "Render the workflow start header into the superchat buffer."
  (with-current-buffer (get-buffer-create superchat-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert (format "** Workflow: %s (%d steps)\n" name step-count)))))

(defun superchat-workflow--run-step (i steps vars step-results
                                       &optional correlation-id)
  "Execute step I of STEPS with VARS and STEP-RESULTS.

When i >= (length steps), finalize the workflow.
Otherwise prepare the step prompt, render its header, and chain the
next step via the Runtime delivery callback.

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
           (step-num (1+ i))
           (correlation-id
            (or correlation-id
                (format "workflow-%s-%04x"
                        (format-time-string "%Y%m%d-%H%M%S-%6N")
                        (random 65536))))
           (session-id
            (when-let* ((buffer (get-buffer superchat-buffer-name)))
              (buffer-local-value 'superchat--session-id buffer)))
           (runtime-session-id (or session-id correlation-id))
           (actor-id (format "workflow-step-%d" step-num))
           (session-configuration
            (when-let* ((buffer (get-buffer superchat-buffer-name))
                        ((fboundp
                          'superchat--session-runtime-configuration)))
              (with-current-buffer buffer
                (superchat--session-runtime-configuration)))))
      ;; Render step header
      (superchat-workflow--render-step-header step-num total)
      (condition-case err
          (superchat-runtime-submit
           (list
            :type :llm-query
            :prompt substituted
            :clean-input substituted
            :buffer-name superchat-buffer-name
            :target-model model
            :context-files contexts
            :session-id runtime-session-id
            :actor-id actor-id
            :correlation-id correlation-id
            :capability "workflow"
            :topology 'workflow
            :authority-policy
            (or (plist-get session-configuration :authority-policy)
                "default")
            :workspace-id
            (plist-get session-configuration :workspace-id)
            :workspace-root
            (plist-get session-configuration :workspace-root)
            :surface 'workflow
            :origin '(:surface workflow)
            :record-conversation nil
            :stream-function #'superchat-workflow--stream-chunk
            :delivery-function
            (lambda (response status tape-id)
              (let ((answer-str (if (stringp response)
                                    response
                                  (format "%s" response))))
                (if (eq status 'completed)
                    (let ((decision
                           (superchat-workflow--resume-decision-for-step
                            runtime-session-id correlation-id actor-id)))
                      (if (eq 'advance (plist-get decision :action))
                          (progn
                            ;; Keep workflow step outputs available as ordinary
                            ;; conversation context without duplicating Tape rows.
                            (with-current-buffer
                                (get-buffer-create superchat-buffer-name)
                              (superchat--record-message
                               "assistant" answer-str tape-id))
                            (superchat-workflow--run-step
                             (1+ i) steps vars
                             (append step-results (list answer-str))
                             correlation-id))
                        (superchat-workflow--fail-step
                         step-num total
                         (format "resume gate %s for Run %s"
                                 (plist-get decision :action)
                                 (or (plist-get decision :run-id) "unknown"))
                         step-results)))
                  (superchat-workflow--fail-step step-num total answer-str
                                                 step-results))))))
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

;;;###autoload
(defun superchat--cmd-workflow (_cmd args _input _lang _target-model)
  "Execute a .workflow recipe.  /workflow <name> [argument].
With no arguments, lists available workflows."
  (let ((name (car (split-string (or args "") "[[:space:]]+" t))))
    (if (and name (not (string-empty-p name)))
        (if (superchat-workflow--exists-p name)
            (let ((argument (string-trim
                             (replace-regexp-in-string
                              (concat "^" (regexp-quote name) "[[:space:]]*")
                              "" (or args "")))))
              (superchat-workflow-execute-async name argument)
              '(:type :noop))
          `(:type :echo :content
            ,(format "Workflow '%s' not found. Available: %s"
                     name
                     (string-join (or (superchat-workflow--list) '()) ", "))))
      ;; No name: list available workflows
      (let ((available (superchat-workflow--list)))
        (if available
            `(:type :echo :content
              ,(format "Available workflows: %s"
                       (string-join available ", ")))
          '(:type :echo :content "No workflows found in superchat-workflow-directory."))))))

;; ═══════════════════════════════════════════════════════
;; Research Workflow vertical slice
;; ═══════════════════════════════════════════════════════

(defconst superchat-research-workflow--roles
  '(collector analyst reviewer)
  "Ordered roles in the first durable Research Workflow.")

(defun superchat-research-workflow--manifest (instance-id)
  "Return the persisted Research manifest for INSTANCE-ID."
  (require 'superchat-subagent)
  (catch 'manifest
    (dolist (decision (plist-get (superchat--team-state instance-id)
                                 :decisions))
      (when-let* ((content (plist-get decision :content))
                  (manifest
                   (ignore-errors
                     (json-parse-string content
                                        :object-type 'plist
                                        :array-type 'list))))
        (when (equal "research-workflow-v0" (plist-get manifest :kind))
          (throw 'manifest manifest))))))

(defun superchat-research-workflow--read-instance-id ()
  "Read a persisted Research Workflow instance id."
  (require 'superchat-tape-view)
  ;; ponytail: scan Session ids at this interactive boundary; add a projection
  ;; only after measured picker latency warrants persistent indexing.
  (let ((ids (cl-remove-if-not #'superchat-research-workflow--manifest
                               (superchat-view-session-ids))))
    (unless ids
      (user-error "No persisted Research Workflows"))
    (completing-read "Research Workflow: " ids nil t)))

(defun superchat-research-workflow--task-role (task)
  "Return TASK's Research role, or nil."
  (when (string-match
         "\\`\\[research/\\(collector\\|analyst\\|reviewer\\)\\]"
         (or (plist-get task :title) ""))
    (intern (match-string 1 (plist-get task :title)))))

(defun superchat-research-workflow--artifact-view (artifact)
  "Return delivery and availability facts for projected ARTIFACT."
  (let* ((reference
          (list :workspace-id (plist-get artifact :workspace-id)
                :artifact-id (plist-get artifact :artifact-id)
                :revision (plist-get artifact :revision)))
         (decision
          (superchat-workspace-artifact-delivery-decision
           (plist-get artifact :session-id)
           (plist-get artifact :run-id)
           reference))
         (available
          (and (eq 'deliver (plist-get decision :action))
               (condition-case nil
                   (let ((content
                          (superchat-workspace-read
                           (plist-get artifact :artifact-id)
                           (plist-get artifact :workspace-id)
                           (plist-get artifact :revision))))
                     (equal (secure-hash 'sha256 content)
                            (plist-get artifact :revision)))
                 (error nil)))))
    (append reference
            (list :uri (plist-get artifact :uri)
                  :role (plist-get artifact :role)
                  :delivery-decision (plist-get decision :action)
                  :available available))))

(defun superchat-research-workflow-view (instance-id)
  "Rebuild Research INSTANCE-ID and its resume decisions from durable facts."
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((manifest (or (superchat-research-workflow--manifest instance-id)
                       (error "Unknown Research Workflow: %s" instance-id)))
         (workspace-id (plist-get manifest :workspace_id))
         (_workspace
          (superchat-workspace-bind
           workspace-id (plist-get manifest :workspace_root) instance-id))
         (state (superchat--team-state instance-id))
         (tasks (superchat-team-tasks instance-id))
         (members (plist-get state :members))
         (artifacts (superchat-workspace-artifacts workspace-id t))
         (previous-ready t)
         steps)
    (dolist (role superchat-research-workflow--roles)
      (let* ((task
              (cl-find role tasks
                       :key #'superchat-research-workflow--task-role))
             (member
              (and task
                   (cl-find (plist-get task :id) members
                            :key (lambda (item) (plist-get item :task-id))
                            :test #'equal)))
             (run-id (plist-get member :run-id))
             (actor-id (plist-get member :actor-id))
             (handoff
              (and run-id actor-id
                   (superchat-subagent-handoff-decision actor-id run-id)))
             (run-decision (plist-get handoff :action))
             (artifact-views
              (mapcar
               #'superchat-research-workflow--artifact-view
               (cl-remove-if-not
                (lambda (artifact)
                  (equal run-id (plist-get artifact :run-id)))
                artifacts)))
             (deliverable
              (cl-some
               (lambda (artifact)
                 (and (eq 'deliver
                          (plist-get artifact :delivery-decision))
                      (plist-get artifact :available)))
               artifact-views))
             (resume
              (cond
               ((null task) (if previous-ready 'start 'blocked))
               ((null member) 'wait)
               ((equal "failed" (plist-get member :status)) 'stop)
               ((eq run-decision 'wait) 'wait)
               ((eq run-decision 'manual-review) 'manual-review)
               ((eq run-decision 'reject) 'stop)
               ((and (eq run-decision 'deliver) deliverable) 'advance)
               ((eq run-decision 'deliver) 'manual-review)
               (t 'wait))))
        (push (list :role role
                    :task-id (plist-get task :id)
                    :actor-id actor-id
                    :run-id run-id
                    :run-decision run-decision
                    :artifacts artifact-views
                    :resume-decision resume)
              steps)
        (setq previous-ready (eq resume 'advance))))
    (setq steps (nreverse steps))
    (list :instance-id instance-id
          :query (plist-get manifest :query)
          :workspace-id workspace-id
          :status (if (cl-every
                       (lambda (step)
                         (eq 'advance (plist-get step :resume-decision)))
                       steps)
                      'completed
                    'incomplete)
          :steps steps)))

(defun superchat-research-workflow--stage-prompt (role manifest view)
  "Return ROLE's task prompt from MANIFEST and prior VIEW."
  (let ((references
         (apply #'append
                (mapcar
                 (lambda (step)
                   (when (eq 'advance (plist-get step :resume-decision))
                     (plist-get step :artifacts)))
                 (plist-get view :steps)))))
    (format
     (concat "[research/%s] Question: %s\n"
             "Work only as the %s. Inspect prior delivered Artifacts with "
             "workspace_read when present, then publish exactly one complete "
             "durable Artifact with workspace_publish using role %s.\n"
             "Prior Artifact references: %s")
     role (plist-get manifest :query) role role
     (json-encode (vconcat references)))))

(defun superchat-research-workflow--continue (instance-id)
  "Launch the next provably resumable stage of INSTANCE-ID."
  (let* ((manifest (superchat-research-workflow--manifest instance-id))
         (view (superchat-research-workflow-view instance-id))
         (next
          (cl-find 'start (plist-get view :steps)
                   :key (lambda (step) (plist-get step :resume-decision)))))
    (if (null next)
        view
      (let* ((role (plist-get next :role))
             (workspace-id (plist-get manifest :workspace_id))
             (root (plist-get manifest :workspace_root))
             (policy (or (plist-get manifest :authority_policy) "default"))
             (task (superchat-research-workflow--stage-prompt
                    role manifest view))
             (superchat--team-id instance-id)
             (superchat--team-actor "lead")
             (superchat--workspace-binding
              (list :id workspace-id :root root))
             (superchat--tool-execution-context
              (list :session-id instance-id
                    :team-id instance-id
                    :correlation-id instance-id
                    :attendance 'attended
                    :authority-policy policy
                    :workspace-id workspace-id
                    :workspace-root root)))
        (superchat--subagent-run-async
         'researcher task nil
         (lambda (report)
           (if (and (stringp report)
                    (string-prefix-p "[Error:" report))
               (message "Research Workflow %s stopped: %s"
                        instance-id report)
             (superchat-research-workflow--continue instance-id))))
        (superchat-research-workflow-view instance-id)))))

;;;###autoload
(defun superchat-research-workflow (query)
  "Start the durable collector → analyst → reviewer workflow for QUERY."
  (interactive "sResearch question: ")
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (user-error "Research question cannot be empty"))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((instance-id
          (format "research-%s-%04x"
                  (format-time-string "%Y%m%d-%H%M%S-%6N")
                  (random 65536)))
         (root (expand-file-name default-directory))
         (policy (or superchat--active-authority-policy "default"))
         (manifest
          (json-encode
           (list :kind "research-workflow-v0"
                 :query query
                 :workspace_id instance-id
                 :workspace_root root
                 :authority_policy policy))))
    (superchat-workspace-bind instance-id root instance-id)
    (let ((superchat--team-id instance-id)
          (superchat--team-actor "lead"))
      (superchat-team-message "publish" nil manifest))
    (superchat-research-workflow--continue instance-id)
    (message "Research Workflow started: %s" instance-id)
    instance-id))

;;;###autoload
(defun superchat-research-workflow-resume (instance-id)
  "Resume the next Closure-approved stage of Research INSTANCE-ID."
  (interactive (list (superchat-research-workflow--read-instance-id)))
  (superchat-research-workflow--continue instance-id)
  (let ((view (superchat-research-workflow-view instance-id)))
    (when (called-interactively-p 'interactive)
      (message "Research Workflow %s: %s"
               instance-id (plist-get view :status)))
    view))

;;;###autoload
(defun superchat-research-workflow-status (instance-id)
  "Display the Tape-derived recovery view for Research INSTANCE-ID."
  (interactive (list (superchat-research-workflow--read-instance-id)))
  (let ((view (superchat-research-workflow-view instance-id)))
    (with-output-to-temp-buffer "*Superchat Research Workflow*"
      (princ (format "Research Workflow: %s\nQuestion: %s\nStatus: %s\n\n"
                     instance-id (plist-get view :query)
                     (plist-get view :status)))
      (dolist (step (plist-get view :steps))
        (princ
         (format "%s  run=%s trust=%s resume=%s\n"
                 (plist-get step :role)
                 (or (plist-get step :run-id) "-")
                 (or (plist-get step :run-decision) "-")
                 (plist-get step :resume-decision)))
        (dolist (artifact (plist-get step :artifacts))
          (princ
           (format "  %s  delivery=%s available=%s\n"
                   (plist-get artifact :artifact-id)
                   (plist-get artifact :delivery-decision)
                   (if (plist-get artifact :available) "yes" "no"))))))
    view))

(provide 'superchat-workflow)
;;; superchat-workflow.el ends here
