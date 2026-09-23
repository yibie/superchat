;;; superchat-workflow.el --- Multi-step linear recipes (async engine) -*- lexical-binding: t; -*-

;;; Commentary:
;; v1.2 restores independent .workflow files with an async linear
;; execution engine.  Each .workflow file is a list of steps executed
;; sequentially as Request envelopes through `superchat-pipeline-submit',
;; chained via the callback Sink's delivery
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
;; Workflow names are direct-directory basenames.  Discovery compares actual
;; `.workflow' basenames in NFC form, but returns the checked candidate's
;; canonical path for loading.  The outer `@model' parser runs before `>>'
;; routing, so an @-prefixed token is reserved at this surface rather than
;; an ordinary workflow name.
;;
;; Async engine replaces the v0.7 synchronous placeholder that never
;; actually called the LLM.  The old `superchat-workflow-execute'
;; (SKILL.md sub-type path) is removed.  Invocation: `>>name [args]'
;; (`>' is reserved for skills, `>>' for workflows) or
;; `/workflow <name> [args]'.

;;; Code:

(declare-function superchat-tape-recorded-run-completed-p "superchat-tape" (event))
(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'superchat-projection)
(require 'json)
(require 'ucs-normalize)
(require 'superchat-core)
(require 'superchat-sink)

;; ── Forward declarations ──
(defvar superchat-lang)
(defvar superchat-data-directory)
(defvar superchat-buffer-name)
(defvar superchat-mode)
(defvar superchat--session-id nil)
(defvar superchat--team-id nil)
(defvar superchat--team-actor nil)
(defvar superchat--tool-execution-context nil)
(defvar superchat--workspace-binding nil)
(defvar superchat--prompt-start nil)
(declare-function superchat--response-region-begin "superchat-render" (owner))
(declare-function superchat--response-region-ensure-view-prompt "superchat-render" (&optional independent))
(declare-function superchat--response-region-insert-transcript
                  "superchat-render" (text owner &optional transcript-owner))
(declare-function superchat--response-region-close
                  "superchat-render" (content &optional final-p transcript-owner))
(declare-function superchat--response-region--prompt-line-position
                  "superchat-render" (&optional state))
(declare-function superchat--stream-llm-result "superchat-render" (chunk &optional owner))
(declare-function superchat--track-active-run "superchat" (handle))
(declare-function superchat--with-supported-input "superchat" (request scope &optional current-p))
(declare-function superchat--record-message
                  "superchat" (role content &optional existing-tape-id))
(declare-function superchat--lookup-command-template "superchat" (command))
(declare-function superchat--session-runtime-configuration "superchat" ())
(declare-function superchat--callback-in-view
                  "superchat-llm" (buffer callback &optional dead-callback))
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

(defun superchat-workflow--ensure-view-prompt ()
  "Keep an independent editable prompt at the end of the workflow View."
  (superchat--response-region-ensure-view-prompt t))

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

(defun superchat-workflow--safe-candidate-p (candidate root-truename)
  "Return non-nil when CANDIDATE is an ordinary file below ROOT-TRUENAME.
Reject symlinks directly, then verify the truename containment boundary."
  (and (not (file-symlink-p candidate))
       (file-regular-p candidate)
       (file-in-directory-p (file-truename candidate) root-truename)))

(defun superchat-workflow--list ()
  "Return list of available workflow names (without .workflow extension)."
  (superchat-workflow--ensure-directory)
  (when (file-directory-p superchat-workflow-directory)
    (let ((root-truename (file-truename superchat-workflow-directory)))
      (mapcar #'file-name-base
              (cl-remove-if-not
               (lambda (candidate)
                 (superchat-workflow--safe-candidate-p
                  candidate root-truename))
               (directory-files superchat-workflow-directory t
                                "\\.workflow\\'"))))))

(defun superchat-workflow--basename-p (name)
  "Return non-nil when NAME is a non-empty direct-directory basename."
  (and (stringp name)
       (> (length name) 0)
       (string= name (file-name-nondirectory name))))

(defun superchat-workflow--select-candidate (name candidates)
  "Classify NFC-equivalent CANDIDATES for NAME.
CANDIDATES is a list of plists with `:basename' and `:path' keys.  Return
a pure result with `:status' of `missing', `unique', or `ambiguous'; a
unique result carries `:candidate', and an ambiguous result carries
`:candidates'."
  (let* ((normalized-name (ucs-normalize-NFC-string name))
         (matches
          (cl-remove-if-not
           (lambda (candidate)
             (string= normalized-name
                      (ucs-normalize-NFC-string
                       (plist-get candidate :basename))))
           candidates)))
    (cond
     ((null matches)
      (list :status 'missing))
     ((cdr matches)
      (list :status 'ambiguous :candidates matches))
     (t
      (list :status 'unique :candidate (car matches))))))

(defun superchat-workflow--resolve (name)
  "Return the canonical .workflow path matching NAME, or nil.

NAME must be a basename.  Candidate basenames are enumerated from
`superchat-workflow-directory' and compared in NFC form so a composed
and decomposed spelling resolve to the same checked candidate.  No matching
candidate returns nil; multiple safe matches signal a `user-error'."
  (superchat-workflow--ensure-directory)
  (when (and (superchat-workflow--basename-p name)
             (file-directory-p superchat-workflow-directory))
    (let* ((root-truename (file-truename superchat-workflow-directory))
           (candidates
            (cl-loop for candidate
                     in (directory-files superchat-workflow-directory t
                                         "\\.workflow\\'")
                     for candidate-name = (file-name-base candidate)
                     when (superchat-workflow--safe-candidate-p
                           candidate root-truename)
                     collect (list :basename candidate-name
                                   :path (file-truename candidate))))
           (selection
            (superchat-workflow--select-candidate name candidates)))
      (pcase (plist-get selection :status)
        ('unique
         (plist-get (plist-get selection :candidate) :path))
        ('ambiguous
         (user-error "Ambiguous workflow '%s': %s"
                     name
                     (mapconcat
                      (lambda (candidate)
                        (plist-get candidate :path))
                      (plist-get selection :candidates)
                      ", ")))
        (_ nil)))))

;;;###autoload
(defun superchat-workflow--exists-p (name)
  "Return the canonical .workflow path matching NAME, or nil."
  (superchat-workflow--resolve name))

(defun superchat-workflow--file-path (name)
  "Return the actual or safe candidate path for .workflow NAME.
Existing files are resolved through `superchat-workflow--resolve'; a
non-existing basename gets the path where such a file would live.  An
existing but unsafe path is not returned."
  (or (superchat-workflow--resolve name)
      (let ((path (when (superchat-workflow--basename-p name)
                    (expand-file-name (concat name ".workflow")
                                      superchat-workflow-directory))))
        (when (and path
                   (not (file-exists-p path))
                   (not (file-symlink-p path)))
          path))))

(defun superchat-workflow--load (name &optional candidate)
  "Load NAME as a string, optionally consuming canonical CANDIDATE.
When CANDIDATE is nil, resolve NAME.  A supplied candidate is validated
without resolving NAME again."
  (let ((file (or candidate (superchat-workflow--resolve name)))
        (root-truename
         (when (file-directory-p superchat-workflow-directory)
           (file-truename superchat-workflow-directory))))
    (when (and (stringp name)
               file
               root-truename
               (string-suffix-p ".workflow" file t)
               (string= (ucs-normalize-NFC-string name)
                        (ucs-normalize-NFC-string (file-name-base file)))
               (superchat-workflow--safe-candidate-p file root-truename))
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun superchat-workflow--completion-list ()
  "Return list of workflow names for completion (used by /workflow command)."
  (superchat-workflow--list))

;;;###autoload
(defun superchat-workflow--parse-invocation (input)
  "Return (NAME . ARGUMENT) from INPUT, accepting a double-quoted NAME.
Bare names end at whitespace.  Quoted names use Lisp string escaping;
only the string is read, never evaluated.  Arguments keep internal spacing."
  (let ((text (string-trim (or input ""))))
    (cond
     ((string-empty-p text) (cons nil ""))
     ((string-prefix-p "\"" text)
      (let* ((parsed (condition-case nil (read-from-string text)
                       (error (user-error "Unclosed or invalid quoted workflow name"))))
             (name (car parsed))
             (end (cdr parsed)))
        (unless (and (stringp name) (not (string-empty-p name))
                     (or (= end (length text))
                         (string-match-p "\\`[[:space:]]" (substring text end))))
          (user-error "Use a nonempty quoted workflow name followed by whitespace"))
        (cons name (string-trim (substring text end)))))
     ((string-match "\\`\\([^[:space:]]+\\)" text)
      (cons (match-string 1 text)
            (string-trim (substring text (match-end 1))))))))

(defun superchat-workflow--quote-name (name)
  "Quote NAME for command insertion when it needs string escaping."
  (if (string-match-p "[[:space:]\"\\\\]" name)
      (let ((print-escape-newlines t)) (prin1-to-string name))
    name))

;;;###autoload
(defun superchat-workflow-edit (name)
  "Visit the existing or prospective Workflow recipe named NAME.
Completion offers existing recipes but also accepts a new direct-directory
basename.  Existing recipes are opened at their resolved actual path.  A new
recipe receives an unsaved minimal template; ordinary `save-buffer' is the
only persistence step."
  (interactive
   (list (completing-read "Workflow name (steps go in the file): "
                          (superchat-workflow--completion-list)
                          nil nil)))
  (let* ((existing (superchat-workflow--resolve name))
         (normalized-name (ucs-normalize-NFC-string name))
         (target (or existing
                     (superchat-workflow--file-path normalized-name))))
    (unless target
      (user-error "Invalid or unsafe workflow name: %s" name))
    (let ((visiting-buffer (find-buffer-visiting target))
          (new-target (not existing)))
      (find-file target)
      (when (and new-target
                 (not visiting-buffer))
        (insert "# Superchat workflow: one executable step per non-comment line.\n"
                "Describe this workflow step.\n")))))

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
  (let ((last-result (or (car (last step-results)) ""))
        (step-count (length step-results)))
    ;; A single pass over TEXT is intentional.  Step output is user/model
    ;; content, so a literal variable marker in it must remain literal rather
    ;; than becoming a second substitution pass.
    (replace-regexp-in-string
     "\\$\\(?:input\\|lang\\|date\\|result\\|step[0-9]+\\)"
     (lambda (match)
       (cond
        ((equal match "$input") (or (plist-get vars :input) ""))
        ((equal match "$lang") (or (plist-get vars :lang) ""))
        ((equal match "$date") (or (plist-get vars :date) ""))
        ((equal match "$result") last-result)
        (t
         (let ((step-number (string-to-number (substring match 5))))
           (if (and (> step-number 0) (<= step-number step-count))
               (or (nth (1- step-number) step-results) "")
             match)))))
     text t t)))

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


(cl-defun superchat-workflow--resume-decision
    (session-id run-id &optional (trace nil trace-supplied-p))
  "Return the recorded-terminal resume decision for RUN-ID in SESSION-ID.
The result's `:action' is one of `advance', `wait', or `stop'.  TRACE, when
supplied, is the caller's Tape-ordered snapshot; nil is an empty snapshot."
  (if (not (and (stringp run-id) (not (string-empty-p run-id))))
      (list :action 'stop :run-id run-id :reason 'run-not-found)
    (condition-case err
        (let* ((events (if trace-supplied-p
                           trace
                         (superchat-service-call nil 'tape 'replay session-id)))
               (terminal
                (superchat-service-call nil 'tape 'recorded-run-terminal
                                        session-id run-id events))
               (state (plist-get terminal :state))
               action reason)
          (cond
           ((eq state 'absent)
            (setq action 'wait reason 'run-open))
           ((eq state 'present)
            (if (superchat-tape-recorded-run-completed-p
                 (plist-get terminal :event))
                (setq action 'advance reason 'proved)
              (setq action 'stop reason 'run-not-successful)))
           (t
            (setq action 'stop
                  reason (or (plist-get terminal :reason)
                             'unknown-terminal))))
          (list :action action
                :run-id run-id
                :reason reason
                :terminal-state terminal))
      (error
       (list :action 'stop
             :run-id run-id
             :reason (list 'tape-read-failed (error-message-string err))
             :terminal-state (list :state 'unknown
                                   :reason 'tape-read-failed))))))


(defun superchat-workflow-instance-view (session-id instance-id)
  "Return the Tape-derived resume view for workflow INSTANCE-ID.
Each step maps to its latest Run, recorded terminal state, and resume
decision."
  (let* ((trace (superchat-service-call nil 'tape 'replay session-id))
         (projected
          (superchat-service-call nil 'tape 'replay-projection 'workflow-instance trace))
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
        (let ((decision
               (superchat-workflow--resume-decision
                session-id (plist-get step :run-id) trace)))
          (append step
                  (list :terminal-state (plist-get decision :terminal-state)
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
        (list :action 'stop
              :run-id nil
              :reason 'run-not-found))))

;;;###autoload
(defun superchat-workflow-execute-async
    (workflow-name &optional argument resolved-file)
  "Execute WORKFLOW-NAME asynchronously with optional ARGUMENT.

Each step chains into the next via Runtime delivery callbacks.
Emacs remains interactive throughout execution.  Outputs render
directly into the superchat buffer.

Interactively, prompts for workflow name with completion.
ARGUMENT is bound to $input in workflow steps.  RESOLVED-FILE, when
non-nil, is the candidate already validated by the dispatcher and is
consumed without resolving WORKFLOW-NAME again."
  (interactive
   (list (completing-read "Workflow: " (superchat-workflow--list))
         (read-string "Argument (optional): ")))
  (superchat-plugin-require-active-row 'workflow)
  (let* ((body (if resolved-file
                   (superchat-workflow--load workflow-name resolved-file)
                 (superchat-workflow--load workflow-name))))
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
      (let* ((superchat-buffer-name
              (if (bound-and-true-p superchat-mode)
                  (buffer-name)
                superchat-buffer-name))
             ;; Resolve the View name at the public boundary.  Later callbacks
             ;; retain this object; a same-name buffer is not its successor.
             (view-buffer
              (and superchat-buffer-name
                   (get-buffer superchat-buffer-name)))
             (session-id
              (or (and (buffer-live-p view-buffer)
                       (buffer-local-value 'superchat--session-id view-buffer))
                  (and (boundp 'superchat--session-id)
                       superchat--session-id))))
        (unless (and (stringp session-id) (not (string-empty-p session-id)))
          (user-error "Workflow requires an existing Session"))
        (let ((correlation-id
               (format "workflow-%s-%s-%04x"
                       workflow-name
                       (format-time-string "%Y%m%d-%H%M%S-%6N")
                       (random 65536))))
          ;; Write workflow header to buffer and kick off the callback chain.
          ;; Initialize the concrete owner before the prompt exists: an ordinary
          ;; request may consume that prompt before this step's first stream
          ;; callback arrives.
          (when (buffer-live-p view-buffer)
            (with-current-buffer view-buffer
              (when (fboundp 'superchat--response-region-begin)
                (superchat--response-region-begin correlation-id))
              (superchat-workflow--render-workflow-header
               workflow-name (length parsed-steps) correlation-id)))
          (superchat-workflow--run-step
           0 parsed-steps vars nil view-buffer correlation-id
           nil session-id))))))

(defun superchat-workflow--render-workflow-header (name step-count &optional owner)
  "Render the workflow start header into the superchat buffer."
  (let ((text (format "** Workflow: %s (%d steps)\n" name step-count)))
    (if (fboundp 'superchat--response-region-insert-transcript)
        (superchat--response-region-insert-transcript text :current-run owner)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert text)))))

(defun superchat-workflow--run-step (i steps vars step-results view-buffer
                                       &optional correlation-id
                                       _reserved session-id control)
  "Execute step I of STEPS with VARS and STEP-RESULTS.

When i >= (length steps), finalize the workflow.
Otherwise prepare the step prompt, render its header, and chain the
next step via the Runtime delivery callback.

STEP-RESULTS accumulates each step's LLM output as a string,
accessible to later steps via $result and $stepN.  VIEW-BUFFER is the
target View object captured by the public workflow boundary.
The seventh positional argument is ignored and retained only to preserve the
private helper's existing arity and SESSION-ID position.
SESSION-ID is captured once by the public workflow boundary and remains the
Runtime Session after VIEW-BUFFER dies.  CONTROL is one execution-local owner
shared by recursive steps; acceptance, never submit return, publishes its Run."
  (cl-block step
    (let* ((control (or control (list :token nil :run-id nil :stopped nil)))
           (token (list i))
           (session-id (or session-id
                           (and (buffer-live-p view-buffer)
                                (buffer-local-value 'superchat--session-id view-buffer))))
           run-id)
      (when (plist-get control :stopped) (cl-return-from step nil))
      (setf (plist-get control :token) token
            (plist-get control :run-id) nil)
      (cl-labels
          ((current-p ()
             (and (eq token (plist-get control :token))
                  (not (plist-get control :stopped))
                  (or (null run-id)
                      (and (equal run-id (plist-get control :run-id))
                           (not (plist-get (superchat-service-call nil 'driver 'status run-id)
                                           :stop-requested))))))
           (view-p ()
             (and (buffer-live-p view-buffer)
                  (equal session-id
                         (buffer-local-value 'superchat--session-id view-buffer))))
           (decorate (request)
             (if (fboundp 'superchat--with-supported-input)
                 (superchat--with-supported-input
                  request 'workflow (lambda () (and (current-p) (view-p))))
               request)))
        (if (>= i (length steps))
            ;; All steps done
            (when (and (current-p) (view-p))
              (with-current-buffer view-buffer
                (superchat-workflow--finalize steps step-results correlation-id)))
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
                 (runtime-session-id
                  (or session-id
                      (and (buffer-live-p view-buffer)
                           (buffer-local-value 'superchat--session-id view-buffer))))
                 (_ (unless (and (stringp runtime-session-id)
                                 (not (string-empty-p runtime-session-id)))
                      (error "Workflow step requires a stable Session ID")))
                 (runtime-buffer
                  (if (buffer-live-p view-buffer)
                      view-buffer
                    (superchat-service-call nil 'editor-workspace 'buffer
                     runtime-session-id 'workflow-protocol t)))
                 (runtime-buffer
                  (superchat-service-call nil 'editor-workspace 'validate-buffer
                   runtime-buffer runtime-session-id))
                 (actor-id (format "workflow-step-%d" step-num))
                 (stream-function
                  (if (and (buffer-live-p view-buffer)
                           (fboundp 'superchat--callback-in-view))
                      (superchat--callback-in-view
                       view-buffer (lambda (chunk)
                                     (when (and (current-p) (view-p))
                                       (superchat-workflow--stream-chunk chunk correlation-id))))
                    #'ignore))
                 (session-configuration
                  (when (and (buffer-live-p view-buffer)
                             (fboundp 'superchat--session-runtime-configuration))
                    (with-current-buffer view-buffer
                      (superchat--session-runtime-configuration)))))
            (unless (current-p) (cl-return-from step nil))
            ;; Render the step into the concrete workflow state and reopen an
            ;; independent editable prompt before the provider starts streaming.
            ;; An ordinary send consumes only that pending prompt through Render's
            ;; response-region API and gets its own Run state.
            (when (and (current-p) (view-p))
              (with-current-buffer view-buffer
                (superchat-workflow--render-step-header step-num total correlation-id)
                (when (and (current-p) (view-p))
                  (superchat-workflow--ensure-view-prompt))))
            (unless (current-p) (cl-return-from step nil))
            ;; A replaced Session is not this execution's View.  Do not let
            ;; initial model presentation enter it after a reentrant header.
            ;; A dead View still follows the existing protocol-host path.
            (when (and (buffer-live-p view-buffer) (not (view-p)))
              (cl-return-from step nil))
            (condition-case err
                (let ((handle
                       (superchat-service-call nil 'driver 'submit
                        (superchat-workflow--envelope
                        (decorate
                        (append
                         (list
                          :type :llm-query
                          :prompt substituted
                          :clean-input substituted
                          :buffer runtime-buffer
                          :buffer-name (buffer-name runtime-buffer)
                          :target-model model
                          :context-files contexts
                          :session-id runtime-session-id
                          :actor-id actor-id
                          :correlation-id correlation-id
                          :capability "workflow"
                          :topology 'workflow
                          :workspace-id
                          (plist-get session-configuration :workspace-id)
                          :workspace-root
                          (plist-get session-configuration :workspace-root)
                          :surface 'workflow
                          :origin '(:surface workflow)
                          :record-conversation nil
                          :transcript-owner correlation-id
                          :stream-function stream-function
                          :accepted-function
                          (lambda (handle)
                            (let ((accepted (plist-get handle :run-id)))
                              (if (not (and (current-p)
                                            (or (not (buffer-live-p view-buffer))
                                                (view-p))))
                                  (superchat-service-call nil 'driver 'cancel accepted "Workflow stopped")
                                (setq run-id accepted)
                                (setf (plist-get control :run-id) accepted)
                                (when (and (buffer-live-p view-buffer)
                                           (buffer-local-value 'superchat-mode view-buffer)
                                           (fboundp 'superchat--track-active-run))
                                  (with-current-buffer view-buffer
                                    (superchat--track-active-run handle)))
                                (unless (and (current-p)
                                             (or (not (buffer-live-p view-buffer))
                                                 (view-p)))
                                  (superchat-service-call nil 'driver 'cancel accepted "Workflow stopped")))))
                          :owner-stop-function
                          (lambda (snapshot)
                            (unless (plist-get control :stopped)
                              (setf (plist-get control :stopped)
                                    (or (plist-get snapshot :stop-requested) t))
                              (let ((current (plist-get control :run-id)))
                                (when (and current
                                           (not (equal current (plist-get snapshot :run-id))))
                                  (superchat-service-call nil 'driver 'cancel current "Workflow stopped"))))
                            ;; Runtime guards this notification once.  Publish
                            ;; and propagate stop before fallible presentation;
                            ;; only the current step may close its own region.
                            (when (and (eq token (plist-get control :token)) (view-p))
                              (with-current-buffer view-buffer
                                (when (fboundp 'superchat--response-region-close)
                                  ;; Keep useful partial output, not a synthetic
                                  ;; cancelled result or a normal delivery retry.
                                  (superchat--response-region-close nil nil correlation-id))
                                (when (and (eq token (plist-get control :token)) (view-p))
                                  (superchat--response-region-insert-transcript
                                   (format "** Workflow stopped at step %d/%d\n%s\n"
                                           step-num total
                                           (or (plist-get snapshot :stop-requested)
                                               "Workflow stopped"))
                                   :current-run correlation-id)
                                  (when (and (eq token (plist-get control :token)) (view-p))
                                    (superchat-workflow--ensure-view-prompt))))))
                          :delivery-function
                          (lambda (response status tape-id)
                            (when (current-p)
                              (let ((answer-str (if (stringp response)
                                                    response
                                                  (format "%s" response))))
                                (when (and (current-p) (view-p)
                                           (fboundp 'superchat--response-region-close))
                                  (with-current-buffer view-buffer
                                    (superchat--response-region-close answer-str nil correlation-id)))
                                (if (and (current-p) (eq status 'completed))
                                    (let ((decision
                                           (superchat-workflow--resume-decision-for-step
                                            runtime-session-id correlation-id actor-id)))
                                      (if (and (current-p)
                                               (eq 'advance (plist-get decision :action))
                                               (or (null run-id)
                                                   (equal run-id (plist-get decision :run-id))))
                                          (progn
                                            ;; Keep workflow step outputs available as ordinary
                                            ;; conversation context without duplicating Tape rows.
                                            (when (and (current-p) (view-p))
                                              (with-current-buffer view-buffer
                                                (superchat--record-message
                                                 "assistant" answer-str tape-id)))
                                            (when (current-p)
                                              (superchat-workflow--run-step
                                               (1+ i) steps vars
                                               (append step-results (list answer-str))
                                               view-buffer correlation-id
                                               nil runtime-session-id control)))
                                        (when (and (current-p) (view-p))
                                          (with-current-buffer view-buffer
                                            (superchat-workflow--fail-step
                                             step-num total
                                             (format "resume gate %s for Run %s"
                                                     (plist-get decision :action)
                                                     (or (plist-get decision :run-id)
                                                         "unknown"))
                                             step-results correlation-id)))))
                                  (when (and (current-p) (view-p))
                                    (with-current-buffer view-buffer
                                      (superchat-workflow--fail-step
                                       step-num total answer-str step-results correlation-id))))))))))))))
                  handle)
              (error
               (when (and (current-p) (view-p))
                 (with-current-buffer view-buffer
                   (superchat-workflow--fail-step
                    step-num total (error-message-string err) step-results correlation-id)))))))))))

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

(defconst superchat-workflow--envelope-keys
  '(:session-id :target-model :capability :topology
    :origin :parent-run-id :deadline :system-prompt)
  "Legacy Request keys that map onto the envelope's own fields unchanged.")

(defun superchat-workflow--envelope (request)
  "Translate a workflow step's legacy REQUEST into a Request envelope (S13b).
The prepared step prompt rides in `:context :prepared-prompt' (sent
unchanged), the host in `:context :host-buffer', `:context-files' as
`:media', and `:stream-function' / `:delivery-function' become a callback
Sink; every other legacy key (the owner protocol's `:accepted-function' /
`:owner-stop-function' / `:transcript-owner', `:input-core', workspace,
actor and correlation ids, `:record-conversation') travels in
`:runtime-extras'.  Contract §2.1 Workflow row, §2.2."
  (let* ((stream-function (plist-get request :stream-function))
         (delivery (plist-get request :delivery-function))
         (prompt (plist-get request :prompt))
         (envelope
          (list :surface 'workflow
                :content (or (plist-get request :clean-input) prompt)
                :tools (plist-get request :tools)
                :media (plist-get request :context-files)
                :sink (superchat-sink-callback
                       :id (format "workflow-%08x" (random #x100000000))
                       :stream (lambda (_run chunk)
                                 (when (functionp stream-function)
                                   (let* ((payload (if (and (listp chunk)
                                                            (plist-member chunk :kind))
                                                       chunk
                                                     (superchat-sink-normalize-chunk chunk)))
                                          (text (plist-get payload :text)))
                                     (when (stringp text)
                                       (funcall stream-function
                                                (if (eq (plist-get payload :kind) 'reasoning)
                                                    (list :reasoning text)
                                                  text))))))
                       :final (lambda (_run result)
                                (when (functionp delivery)
                                  (funcall delivery (or (plist-get result :text) "")
                                           'completed (plist-get result :tape-id))))
                       :error (lambda (_run failure)
                                (when (functionp delivery)
                                  (funcall delivery (or (plist-get failure :text) "")
                                           (plist-get failure :status)
                                           (plist-get failure :tape-id)))))
                :context (list :host-buffer (plist-get request :buffer)
                               :prepared-prompt prompt)))
         extras)
    (dolist (key superchat-workflow--envelope-keys)
      (when (plist-member request key)
        (setq envelope (plist-put envelope key (plist-get request key)))))
    ;; Like the chat translation: `superchat-select-tools-decision' reads
    ;; `:input-core' off the envelope itself (for `ask_user' on the baseline),
    ;; so it travels at the top level as well as in the extras.
    (when (plist-get request :input-core)
      (setq envelope (plist-put envelope :input-core (plist-get request :input-core))))
    (cl-loop for (key value) on request by #'cddr
             unless (or (memq key superchat-workflow--envelope-keys)
                        (memq key '(:type :prompt :input :buffer :buffer-name
                                    :stream-function :delivery-function
                                    :context-files :tools :surface)))
             do (setq extras (plist-put extras key value)))
    (plist-put envelope :runtime-extras extras)))

(defun superchat-workflow--stream-chunk (chunk &optional owner)
  "Append CHUNK into the superchat buffer after the step header.
Uses `superchat-streaming-pending' face for raw markdown
mid-stream so font-lock does not re-interpret it."
  (if (fboundp 'superchat--stream-llm-result)
      (superchat--stream-llm-result chunk owner)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert chunk)
        (put-text-property start (point)
                           'face 'superchat-streaming-pending)))))

(defun superchat-workflow--render-step-header (step-num total &optional owner)
  "Write \"** Workflow: step N/T\" header into the superchat buffer."
  (let ((text (format "** Step %d/%d\n" step-num total)))
    (if (fboundp 'superchat--response-region-insert-transcript)
        (superchat--response-region-insert-transcript text :current-run owner)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert text)))))

(defun superchat-workflow--fail-step (step-num total error-msg _step-results &optional owner)
  "Render a failure notice for step STEP-NUM/TOTAL with ERROR-MSG.
Writes the error into the buffer, then inserts a fresh prompt."
  (let ((text (format "** Workflow FAILED at step %d/%d\nError: %s\n"
                     step-num total error-msg)))
    (if (fboundp 'superchat--response-region-insert-transcript)
        (superchat--response-region-insert-transcript text :current-run owner)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert text))))
  (superchat-workflow--ensure-view-prompt))

(defun superchat-workflow--finalize (steps _step-results &optional owner)
  "Render workflow completion summary into the buffer.
Inserts a completion headline and a fresh prompt."
  (let* ((total (length steps))
         (text (format "** Workflow complete (%d/%d steps)\n" total total)))
    (if (fboundp 'superchat--response-region-insert-transcript)
        (superchat--response-region-insert-transcript text :current-run owner)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert text))))
  (superchat-workflow--ensure-view-prompt))

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
Double-quote names containing whitespace.  With no arguments, list recipes.
Listing is read-only and stays available with the workflow row unmounted;
running a recipe is refused by `superchat-workflow-execute-async'."
  (pcase-let ((`(,name . ,argument) (superchat-workflow--parse-invocation args)))
    (if (and name (not (string-empty-p name)))
        (let ((resolved-file (superchat-workflow--exists-p name)))
          (if resolved-file
              (progn
                (superchat-workflow-execute-async
                 name argument resolved-file)
                '(:type :noop))
            `(:type :echo :content
              ,(format "Workflow '%s' not found. Available: %s"
                       name
                       (string-join (or (superchat-workflow--list) '()) ", ")))))
      ;; No name: list available workflows
      (let ((available (superchat-workflow--list)))
        (if available
            `(:type :echo :content
              ,(format "Available workflows: %s"
                       (string-join available ", ")))
          '(:type :echo :content "No workflows found in superchat-workflow-directory. Create and save one with M-x superchat-workflow-edit."))))))

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
                               (superchat-service-call nil 'session 'session-ids))))
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
  (superchat-plugin-require-active-row 'workflow)
  (unless (and (stringp query) (not (string-empty-p (string-trim query))))
    (user-error "Research question cannot be empty"))
  (require 'superchat-subagent)
  (require 'superchat-workspace)
  (let* ((instance-id
          (format "research-%s-%04x"
                  (format-time-string "%Y%m%d-%H%M%S-%6N")
                  (random 65536)))
         (root (expand-file-name default-directory))
         (manifest
          (json-encode
           (list :kind "research-workflow-v0"
                 :query query
                 :workspace_id instance-id
                 :workspace_root root))))
    (superchat-workspace-bind instance-id root instance-id)
    (let ((superchat--team-id instance-id)
          (superchat--team-actor "lead"))
      (superchat-team-message "publish" nil manifest))
    (superchat-research-workflow--continue instance-id)
    (message "Research Workflow started: %s" instance-id)
    instance-id))

;;;###autoload
(defun superchat-research-workflow-resume (instance-id)
  "Resume the next advance-approved stage of Research INSTANCE-ID."
  (interactive (list (superchat-research-workflow--read-instance-id)))
  (superchat-plugin-require-active-row 'workflow)
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
