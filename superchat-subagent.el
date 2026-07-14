;;; superchat-subagent.el --- Isolated sub-agents for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Sub-agents let the main session delegate isolated tasks to
;; specialized presets (researcher, executor, introspector).  They run
;; with their own session_id and tape, and return a concise report that
;; is rendered in the main chat buffer.

;;; Code:

(require 'cl-lib)
(require 'superchat-preset)

;; External declarations
(declare-function superchat-preset-apply "superchat-preset" (preset turn))
(declare-function superchat--llm-generate-answer-sync "superchat-llm" (prompt &optional target-model tools agent-mode system-prompt preset))
(declare-function superchat--execute-llm-query "superchat-dispatcher" (turn &optional template target-model))
(declare-function superchat--agent-wrap-tools "superchat-agent-loop" (tools &optional preset))
(declare-function superchat-get-llm-tools "superchat-tools" ())
(declare-function superchat--collect-llm-tools "superchat-llm" (&optional input tool-names))

(defvar superchat--session-id)
(defvar superchat--conversation-history)
(defvar superchat--active-preset)
(defvar superchat-buffer-name)

(declare-function superchat-skills-get-available "superchat-skills" ())
(declare-function superchat-skills-load "superchat-skills" (skill-name))

;; ═══════════════════════════════════════════════════════════
;; Built-in sub-agent presets
;; ═══════════════════════════════════════════════════════════

(defun superchat--subagent-make-preset (name description type tools body)
  "Create a built-in sub-agent preset."
  (superchat-preset-from-plist
   (list :name name
         :description description
         :type type
         :tools tools
         :body body
         :source 'builtin
         :version "1.0")))

(defun superchat--subagent-preset-researcher ()
  "Return the built-in `researcher' sub-agent preset."
  (superchat--subagent-make-preset
   "researcher"
   "Read-only research sub-agent"
   'agent
   '("read-file" "search-text" "list-files" "read_buffer")
   "You are a research sub-agent. Your job is to investigate the user's request using only read-only tools. Produce a concise, factual report with sources. Do not modify files or execute commands."))

(defun superchat--subagent-preset-executor ()
  "Return the built-in `executor' sub-agent preset."
  (superchat--subagent-make-preset
   "executor"
   "Autonomous execution sub-agent"
   'agent
   '("read-file" "search-text" "list-files" "read_buffer" "shell-command" "write-file" "append-file" "EditBuffer")
   "You are an execution sub-agent. Carry out the user's task autonomously using the available tools. You may read, write, edit, and run shell commands. Produce a concise final report summarizing what you did and the outcome."))

(defun superchat--subagent-preset-introspector ()
  "Return the built-in `introspector' sub-agent preset."
  (superchat--subagent-make-preset
   "introspector"
   "Emacs introspection sub-agent"
   'agent
   '("read_buffer" "describe-function" "describe-variable" "eval-elisp")
   "You are an Emacs introspection sub-agent. Use the available tools to inspect Emacs state, documentation, and Elisp. Produce a concise report with the requested information."))

(defconst superchat--subagent-builtin-presets
  '(("researcher" . superchat--subagent-preset-researcher)
    ("executor" . superchat--subagent-preset-executor)
    ("introspector" . superchat--subagent-preset-introspector))
  "Built-in sub-agent names and their preset constructors.")

(defun superchat--subagent-custom-name (name)
  "Return the on-disk skill name matching NAME case-insensitively."
  (when (fboundp 'superchat-skills-get-available)
    (let ((key (downcase name)))
      (cl-find-if (lambda (candidate)
                    (string= key (downcase candidate)))
                  (superchat-skills-get-available)))))

(defun superchat--subagent-preset (name)
  "Return a sub-agent preset by NAME (symbol or string)."
  (let* ((key (downcase (if (symbolp name) (symbol-name name) name)))
         (builtin (assoc key superchat--subagent-builtin-presets)))
    (if builtin
        (funcall (cdr builtin))
      (let* ((custom-name (superchat--subagent-custom-name key))
             (skill (and custom-name
                         (fboundp 'superchat-skills-load)
                         (superchat-skills-load custom-name))))
        (when (and skill (superchat-preset-agent-p skill))
          skill)))))

(defun superchat--subagent-registry ()
  "Return callable sub-agent names paired with their presets.
Built-ins come first and win name collisions; custom agents are sorted
by the skill filename used to invoke them."
  (let* ((builtins (mapcar (lambda (entry)
                             (cons (car entry) (funcall (cdr entry))))
                           superchat--subagent-builtin-presets))
         (seen-names (mapcar #'car builtins))
         (custom-names (if (fboundp 'superchat-skills-get-available)
                           (sort (copy-sequence
                                  (superchat-skills-get-available))
                                 #'string-lessp)
                         nil)))
    (append
     builtins
     (delq nil
           (mapcar (lambda (source-name)
                     (let ((name (downcase source-name)))
                       (unless (member name seen-names)
                         (push name seen-names)
                         (let ((preset (superchat-skills-load source-name)))
                         (when (and preset (superchat-preset-agent-p preset))
                             (cons name preset))))))
                   custom-names)))))

;; ═══════════════════════════════════════════════════════════
;; Sub-agent runner
;; ═══════════════════════════════════════════════════════════

(defcustom superchat-subagent-parallel-max 3
  "Maximum number of sub-agents in flight at once.
In the async engine this is a launch window: additional specs
queue and start as running sub-agents complete."
  :type 'integer
  :group 'superchat)

(defcustom superchat-subagent-max-depth 1
  "Maximum delegation depth for nested sub-agents.
The main session delegates at depth 1.  A sub-agent may itself
delegate only while its depth is below this limit, so the default
of 1 means sub-agents cannot delegate further."
  :type 'integer
  :group 'superchat)

(defun superchat--subagent-run (preset-name task &optional context)
  "Run a sub-agent PRESET-NAME on TASK with optional CONTEXT — BLOCKING.
Returns the final report string.  The sub-agent has an isolated
session_id and tape; its tool calls are not rendered in the main
chat buffer.

Legacy synchronous path: Emacs freezes for the duration.  New code
should use `superchat--subagent-run-async' instead."
  (let* ((preset (superchat--subagent-preset preset-name))
         (session-id (format "subagent-%s-%s-%04x"
                             (superchat-preset-name preset)
                             (format-time-string "%Y%m%d-%H%M%S")
                             (random 65536)))
         (temp-buffer (generate-new-buffer (format " *superchat-subagent:%s*" session-id)))
         (turn (superchat-turn-new task))
         (superchat-buffer-name (buffer-name temp-buffer))
         (superchat--session-id session-id)
         (superchat--conversation-history nil)
         (superchat--active-preset preset))
    (unwind-protect
        (with-current-buffer temp-buffer
          (setq-local superchat--agent-tool-call-count 0)
          (unless preset
            (error "Unknown sub-agent preset: %s" preset-name))
          (superchat-preset-apply preset turn)
          (setf (superchat-turn-prompt turn)
                (concat (when (and context (not (string-empty-p context)))
                          (format "Context from main session:\n%s\n\n" context))
                        task))
          (let* ((result (superchat--execute-llm-query turn))
                 (tools (plist-get result :tools)))
            (superchat--llm-generate-answer-sync
             (plist-get result :prompt)
             (plist-get result :target-model)
             tools
             t
             (plist-get result :system-prompt)
             preset)))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

;; ═══════════════════════════════════════════════════════════
;; Async engine — explicit context (v1.3)
;; ═══════════════════════════════════════════════════════════
;;
;; The sync runner above fakes isolation with dynamic `let' bindings.
;; Those bindings do not survive into async callbacks, so the async
;; engine threads an explicit context plist through closures instead:
;; tape writes and depth checks read the context, never the dynamic
;; environment.  All async callbacks run on the main thread, so there
;; is no data race — only completion-order nondeterminism, which the
;; parallel aggregator resolves with spec-indexed result slots.

(defvar superchat-llm-backend)
(defvar superchat-llm-max-tool-rounds)
(defvar superchat-llm-round-limit-action)
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function superchat--build-llm-prompt "superchat-llm" (text tools &optional context preset))
(declare-function superchat--llm-extract-text "superchat-llm" (result))
(declare-function superchat--llm-tool-round-p "superchat-llm" (result))
(declare-function superchat--llm-round-limit-action "superchat-llm" ())
(declare-function superchat--llm-disable-prompt-tools "superchat-llm" (prompt))
(declare-function llm-chat-async "llm")
(declare-function llm-cancel-request "llm" (request))
(declare-function make-llm-tool "llm")
(declare-function json-read-from-string "json" (string))
(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function superchat-db-tape-append "superchat-db")
(declare-function superchat--agent-run-tool-hooks "superchat-agent-loop" (hooks &rest args))
(declare-function superchat--agent-check-permission "superchat-agent-loop" (tool-name args))
(declare-function superchat--agent-confirm-p "superchat-agent-loop" (tool-name args &optional guardrails))
(declare-function superchat--agent-ask-confirm "superchat-agent-loop" (tool-name args))
(declare-function superchat--agent-effective-guardrails "superchat-agent-loop" (&optional preset))
(defvar superchat-agent-max-tool-calls)
(defvar superchat-agent-confirm-destructive)
(defvar superchat-agent-pre-tool-functions)
(defvar superchat-agent-post-tool-functions)
(defvar superchat-agent-post-tool-failure-functions)

(defcustom superchat-subagent-timeout 300
  "Global upper bound for a sub-agent run, in seconds.
Set to nil to disable the global bound.  A preset `timeout' may set a
shorter limit, but cannot extend this one."
  :type '(choice (const :tag "No timeout" nil) number)
  :group 'superchat)

(defvar superchat--subagent-running nil
  "Alist of active sub-agent ids and their runtime metadata.")

(defun superchat--subagent-entry (id)
  "Return the active control-plane entry for ID."
  (cdr (assoc id superchat--subagent-running)))

(defun superchat--subagent-finish (id report)
  "Finish active sub-agent ID once and deliver REPORT."
  (when-let* ((entry (superchat--subagent-entry id)))
    (when-let* ((timer (plist-get entry :timer)))
      (cancel-timer timer))
    (setq superchat--subagent-running
          (assoc-delete-all id superchat--subagent-running))
    (funcall (plist-get entry :callback) report)))

(defun superchat-subagent-cancel (id &optional reason)
  "Cancel running sub-agent ID and return non-nil when it existed.
REASON defaults to a user-cancellation message."
  (interactive
   (list (completing-read "Cancel sub-agent: "
                          (mapcar #'car superchat--subagent-running)
                          nil t)))
  (when-let* ((entry (superchat--subagent-entry id)))
    (when-let* ((request (plist-get entry :request)))
      (when (fboundp 'llm-cancel-request)
        (ignore-errors (llm-cancel-request request))))
    (superchat--subagent-finish id (or reason "[Cancelled by user]"))
    t))

(defun superchat-subagent-running ()
  "Return active sub-agent metadata, newest first."
  (mapcar #'cdr superchat--subagent-running))

(defun superchat-subagent-list-running ()
  "Display and return a summary of running sub-agents."
  (interactive)
  (let ((summary
         (if (null superchat--subagent-running)
             "No running sub-agents."
           (mapconcat
            (lambda (pair)
              (let* ((entry (cdr pair))
                     (placeholder (plist-get entry :placeholder))
                     (position (and (consp placeholder)
                                    (markerp (car placeholder))
                                    (marker-position (car placeholder)))))
                (format "%s  %s  depth=%d  %.1fs%s"
                        (car pair) (plist-get entry :preset)
                        (plist-get entry :depth)
                        (- (float-time) (plist-get entry :started-at))
                        (if position (format "  placeholder=%d" position) ""))))
            superchat--subagent-running "\n"))))
    (when (called-interactively-p 'interactive) (message "%s" summary))
    summary))

(defun superchat--subagent-set-placeholder (id placeholder)
  "Associate PLACEHOLDER markers with running sub-agent ID."
  (when-let* ((entry (superchat--subagent-entry id)))
    (setcdr (assoc id superchat--subagent-running)
            (plist-put entry :placeholder placeholder))))

(defun superchat--subagent-set-request (id request)
  "Record REQUEST as the current cancellable request for sub-agent ID."
  (when-let* ((entry (superchat--subagent-entry id)))
    (setcdr (assoc id superchat--subagent-running)
            (plist-put entry :request request))))

(defun superchat--subagent-arm-timeout (id delay timeout)
  "Arm sub-agent ID's timeout after DELAY seconds.
TIMEOUT is retained for the user-facing error message when a paused
timer is resumed with less than its original delay."
  (when-let* ((entry (superchat--subagent-entry id)))
    (let ((timer
           (run-at-time
            delay nil
            (lambda ()
              (superchat-subagent-cancel
               id (format "[Timed out after %ss]" timeout))))))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :timer timer)))))

(defun superchat--subagent-pause-timeout (id)
  "Pause ID's timer and retain its remaining duration, if any."
  (when-let* ((entry (superchat--subagent-entry id))
              (timer (plist-get entry :timer)))
    (let ((remaining (max 0
                          (- (time-to-seconds (timer--time timer))
                             (float-time)))))
      (cancel-timer timer)
      (setq entry (plist-put entry :timer nil))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :paused-timeout remaining)))))

(defun superchat--subagent-resume-timeout (id)
  "Resume ID's timeout after an interactive pause, if it was paused."
  (when-let* ((entry (superchat--subagent-entry id))
              (remaining (plist-get entry :paused-timeout)))
    (let ((timeout (plist-get entry :timeout)))
      (setcdr (assoc id superchat--subagent-running)
              (plist-put entry :paused-timeout nil))
      (superchat--subagent-arm-timeout id remaining timeout))))

(defun superchat--subagent-round-limit-action (ctx)
  "Choose a round-limit action for CTX without charging user input time."
  (let ((id (plist-get ctx :session-id)))
    (if (and (eq superchat-llm-round-limit-action 'ask)
             (not noninteractive))
        (unwind-protect
            (progn
              (superchat--subagent-pause-timeout id)
              (superchat--llm-round-limit-action))
          (superchat--subagent-resume-timeout id))
      (superchat--llm-round-limit-action))))

(defconst superchat--subagent-delegate-tool-names
  '("delegate_to_subagent" "delegate_to_subagent_parallel")
  "Tool names that re-enter the sub-agent engine (depth-guarded).")

(defun superchat--subagent-make-context (preset depth)
  "Create an explicit execution context for PRESET at DEPTH.
The context replaces the dynamic bindings of the sync runner: async
callbacks close over it, so session identity, depth, and the tool
call counter survive across the event loop."
  (list :session-id (format "subagent-%s-%s-%04x"
                            (superchat-preset-name preset)
                            (format-time-string "%Y%m%d-%H%M%S")
                            (random 65536))
        :preset preset
        :depth depth
        :guardrails
        (if (fboundp 'superchat--agent-effective-guardrails)
            (superchat--agent-effective-guardrails preset)
          (list :max-tool-calls
                (if (boundp 'superchat-agent-max-tool-calls)
                    superchat-agent-max-tool-calls
                  50)
                :confirm-destructive
                (bound-and-true-p superchat-agent-confirm-destructive)))
        ;; Mutable tool-call counter: the car of this cons is
        ;; incremented in place by the tool wrappers.
        :tool-calls (cons 0 nil)))

(defun superchat--subagent-effective-timeout (preset)
  "Return the effective timeout for PRESET."
  (let ((global (and (numberp superchat-subagent-timeout)
                     (> superchat-subagent-timeout 0)
                     superchat-subagent-timeout))
        (profile (superchat-preset-timeout preset)))
    (cond ((and global profile) (min global profile))
          (profile profile)
          (t global))))

(defun superchat--subagent-tape (ctx kind content)
  "Append a KIND/CONTENT event to the tape under CTX's session id."
  (when (fboundp 'superchat-db-tape-append)
    (ignore-errors
      (superchat-db-tape-append (plist-get ctx :session-id) kind content))))

(defun superchat--subagent-truncate (obj)
  "Format OBJ as a string, truncated for tape logging."
  (let ((s (format "%s" obj)))
    (if (> (length s) 1000)
        (concat (substring s 0 1000) "…(truncated)")
      s)))

(defun superchat--subagent-guarded-call (guardrails tool-name original-fn args)
  "Run ORIGINAL-FN with ARGS under hooks, permission, and confirm gates.
Returns the tool result, or a bracketed status string when the call
is denied, cancelled, or errors.  Unlike the main agent loop, tool
errors are returned as strings instead of re-signaled: a `signal'
inside llm.el's async machinery would abort the whole sub-agent."
  (superchat--agent-run-tool-hooks
   superchat-agent-pre-tool-functions tool-name args)
  (let ((permission (superchat--agent-check-permission tool-name args)))
    (cond
     ((eq permission 'deny)
      "[Tool call denied by permission hook]")
     ((and (not (eq permission 'allow))
           (superchat--agent-confirm-p tool-name args guardrails)
           (not (superchat--agent-ask-confirm tool-name args)))
      "[Tool call cancelled by user]")
     (t
      (condition-case err
          (let ((result (apply original-fn args)))
            (superchat--agent-run-tool-hooks
             superchat-agent-post-tool-functions tool-name args result)
            result)
        (error
         (superchat--agent-run-tool-hooks
          superchat-agent-post-tool-failure-functions tool-name args err)
         (format "[Tool error: %s]" (error-message-string err))))))))

(defun superchat--subagent-wrap-function (ctx tool-name original-fn async)
  "Wrap ORIGINAL-FN for sub-agent CTX: counting, gating, tape logging.
ASYNC non-nil means ORIGINAL-FN takes a callback as first argument."
  (let ((counter (plist-get ctx :tool-calls)))
    (let* ((guardrails (plist-get ctx :guardrails))
           (max-tool-calls (plist-get guardrails :max-tool-calls)))
    (if async
        (lambda (callback &rest args)
          (cl-incf (car counter))
          (if (> (car counter) max-tool-calls)
              (funcall callback
                       (format "Error: sub-agent exceeded maximum tool calls (%d)."
                               max-tool-calls))
            (superchat--subagent-tape ctx "tool_call"
                                      (format "%s(%S)" tool-name args))
            (superchat--agent-run-tool-hooks
             superchat-agent-pre-tool-functions tool-name args)
            (let ((permission (superchat--agent-check-permission tool-name args)))
              (cond
               ((eq permission 'deny)
                (funcall callback "[Tool call denied by permission hook]"))
               ((and (not (eq permission 'allow))
                     (superchat--agent-confirm-p tool-name args guardrails)
                     (not (superchat--agent-ask-confirm tool-name args)))
                (funcall callback "[Tool call cancelled by user]"))
               (t
                (condition-case err
                    (apply original-fn
                           (lambda (result)
                             (superchat--agent-run-tool-hooks
                              superchat-agent-post-tool-functions
                              tool-name args result)
                             (superchat--subagent-tape
                              ctx "tool_result"
                              (format "%s -> %s" tool-name
                                      (superchat--subagent-truncate result)))
                             (funcall callback result))
                           args)
                  (error
                   (superchat--agent-run-tool-hooks
                    superchat-agent-post-tool-failure-functions
                    tool-name args err)
                   (funcall callback (format "[Tool error: %s]"
                                             (error-message-string err))))))))))
      (lambda (&rest args)
        (cl-incf (car counter))
        (if (> (car counter) max-tool-calls)
            (format "Error: sub-agent exceeded maximum tool calls (%d)."
                    max-tool-calls)
          (superchat--subagent-tape ctx "tool_call"
                                    (format "%s(%S)" tool-name args))
          (let ((result (superchat--subagent-guarded-call
                         guardrails tool-name original-fn args)))
            (superchat--subagent-tape
             ctx "tool_result"
             (format "%s -> %s" tool-name (superchat--subagent-truncate result)))
            result)))))))

(defun superchat--subagent-wrap-tool (ctx tool)
  "Return TOOL wrapped for sub-agent CTX.
Delegate tools are special-cased: below `superchat-subagent-max-depth'
they re-enter the async engine at depth + 1; at or beyond the limit
they degrade to a sync stub returning a denial string."
  (let ((name (llm-tool-name tool))
        (depth (plist-get ctx :depth)))
    (cond
     ((not (member name superchat--subagent-delegate-tool-names))
      (make-llm-tool
       :name name
       :description (llm-tool-description tool)
       :args (llm-tool-args tool)
       :async (llm-tool-async tool)
       :function (superchat--subagent-wrap-function
                  ctx name (llm-tool-function tool) (llm-tool-async tool))))
     ((>= depth superchat-subagent-max-depth)
      (make-llm-tool
       :name name
       :description (llm-tool-description tool)
       :args (llm-tool-args tool)
       :async nil
       :function (lambda (&rest _args)
                   (format "[Delegation denied: sub-agent depth limit %d reached]"
                           superchat-subagent-max-depth))))
     (t
      (make-llm-tool
       :name name
       :description (llm-tool-description tool)
       :args (llm-tool-args tool)
       :async t
       :function
       (if (string= name "delegate_to_subagent")
           (lambda (callback preset task &optional context)
             (superchat--subagent-run-async preset task context callback
                                            (1+ depth)))
         (lambda (callback tasks)
           (let ((specs (superchat--subagent-parse-specs tasks)))
             (if (stringp specs)
                 (funcall callback specs)
               (superchat--subagent-run-parallel-async
                specs callback (1+ depth)))))))))))

(defun superchat--subagent-wrap-tools (ctx tools)
  "Wrap all TOOLS for sub-agent CTX."
  (mapcar (lambda (tool) (superchat--subagent-wrap-tool ctx tool)) tools))

(defun superchat--subagent-llm-async (ctx prompt tools target-model system-prompt callback)
  "Fire an async llm.el request for sub-agent CTX.
SYSTEM-PROMPT, when a non-empty string, becomes the system message
\(the preset persona travels here).  CALLBACK receives the final
answer text, or an \"[Error: ...]\" string.  Never signals; Emacs
stays interactive."
  (if (null superchat-llm-backend)
      (funcall callback "[Error: superchat-llm-backend is not configured]")
    (condition-case err
        (let* ((backend (superchat--effective-llm-backend target-model))
               (real-prompt (superchat--build-llm-prompt
                             prompt tools system-prompt
                             (plist-get ctx :preset)))
               ;; A sub-agent may synthesize a tool, but only into its own
               ;; in-flight prompt (persist = nil).  The prompt dies with
               ;; the run, so nothing reaches the session registry: a
               ;; shareable sub-agent skill must not be able to push a tool
               ;; into the main agent's toolbox.
               (_ (when (fboundp 'superchat--syn-bind)
                    (superchat--syn-bind
                     real-prompt
                     (lambda (tool) (superchat--subagent-wrap-tool ctx tool))
                     nil)))
               (multi-output (and tools t))
               (tool-rounds 0)
               (tool-round-limit superchat-llm-max-tool-rounds)
               (final-answer-requested nil)
               (request-generation 0)
               (response-cb nil)
               (error-cb (lambda (_type message)
                           (funcall callback (format "[Error: %s]" message)))))
          (cl-labels
              ((send-request ()
                 (condition-case request-err
                     (let* ((generation (cl-incf request-generation))
                            (request (llm-chat-async backend real-prompt
                                                     response-cb error-cb
                                                     multi-output)))
                       ;; A provider may invoke RESPONSE-CB synchronously.
                       ;; In that case its nested request is newer.
                       (when (= generation request-generation)
                         (superchat--subagent-set-request
                          (plist-get ctx :session-id) request))
                       request)
                   (error
                    (funcall callback
                             (format "[Error: %s]"
                                     (error-message-string request-err)))))))
            (superchat--subagent-tape ctx "user" prompt)
            (setq response-cb
                  (lambda (response)
                    (if (superchat--llm-tool-round-p response)
                        (cond
                         (final-answer-requested
                          (funcall callback
                                   "[Tool call returned after requesting a final answer]"))
                         (t
                          (let ((continue t))
                            (cl-incf tool-rounds)
                            (when (and (numberp tool-round-limit)
                                       (>= tool-rounds tool-round-limit))
                              (pcase (superchat--subagent-round-limit-action ctx)
                                ('continue
                                 (if (and (numberp superchat-llm-max-tool-rounds)
                                          (> superchat-llm-max-tool-rounds 0))
                                     (setq tool-round-limit
                                           (+ tool-round-limit
                                              superchat-llm-max-tool-rounds))
                                   (setq final-answer-requested t
                                         multi-output nil)
                                   (superchat--llm-disable-prompt-tools real-prompt)))
                                ('finalize
                                 (setq final-answer-requested t
                                       multi-output nil)
                                 (superchat--llm-disable-prompt-tools real-prompt))
                                ('stop
                                 (setq continue nil)
                                 (funcall callback
                                          (format "[Stopped after %d tool rounds]"
                                                  tool-rounds)))))
                            (when continue
                              (send-request)))))
                      (let ((text (superchat--llm-extract-text response)))
                        (superchat--subagent-tape ctx "assistant" text)
                        (funcall callback text)))))
            (send-request)))
      (error
       (funcall callback (format "[Error: %s]" (error-message-string err)))))))

(defun superchat--subagent-run-async (preset-name task context callback &optional depth)
  "Run sub-agent PRESET-NAME on TASK asynchronously.
CONTEXT is optional background text from the caller.  CALLBACK
receives the final report string; failures are delivered as
\"[Error: ...]\" strings — this function never signals.  DEPTH is
the delegation depth of the new sub-agent (default 1: delegated
from the main session).

Unlike `superchat--subagent-run', isolation comes from the explicit
context object, not from dynamic bindings, and Emacs stays
interactive while the sub-agent runs."
  (let ((preset (superchat--subagent-preset preset-name)))
    (if (null preset)
        (funcall callback (format "[Error: unknown sub-agent preset: %s]"
                                  preset-name))
      (let ((id nil))
        (condition-case err
          (let* ((ctx (superchat--subagent-make-context preset (or depth 1)))
                 (turn (superchat-turn-new task)))
            (setq id (plist-get ctx :session-id))
            (push (cons id (list :id id
                                 :preset (superchat-preset-name preset)
                                 :depth (or depth 1)
                                 :started-at (float-time)
                                 :callback callback))
                  superchat--subagent-running)
            (superchat-preset-apply preset turn)
            (setf (superchat-turn-prompt turn)
                  (concat (when (and context (not (string-empty-p context)))
                            (format "Context from main session:\n%s\n\n" context))
                          task))
            ;; Everything up to the llm-chat-async call runs
            ;; synchronously, so dynamic bindings are still safe for
            ;; PROMPT BUILDING; the async callbacks rely only on `ctx'.
            (let* ((result (let ((superchat--active-preset preset)
                                 (superchat--conversation-history nil)
                                 (superchat--session-id (plist-get ctx :session-id)))
                             (superchat--execute-llm-query turn)))
                   (prompt (plist-get result :prompt))
                   (tools (superchat--subagent-wrap-tools
                           ctx
                           (superchat--collect-llm-tools
                            (when (stringp prompt) prompt)
                            (plist-get result :tools)))))
              (let* ((request
                      (superchat--subagent-llm-async
                       ctx prompt tools (plist-get result :target-model)
                       (plist-get result :system-prompt)
                       (lambda (report)
                         (superchat--subagent-finish id report))))
                     (entry (superchat--subagent-entry id))
                     (timeout (superchat--subagent-effective-timeout preset)))
                (when entry
                  (unless (plist-member entry :request)
                    (setq entry (plist-put entry :request request)))
                  (setq entry (plist-put entry :timeout timeout))
                  (setcdr (assoc id superchat--subagent-running) entry)
                  (when timeout
                    (superchat--subagent-arm-timeout id timeout timeout)))
                id)))
          (error
           (if id
               (superchat--subagent-finish
                id (format "[Error: %s]" (error-message-string err)))
             (funcall callback (format "[Error: %s]"
                                       (error-message-string err))))))))))

(defun superchat--subagent-parse-specs (tasks)
  "Parse TASKS (a JSON array string) into a list of spec plists.
Each element becomes (:preset ... :task ... :context ...).
Returns an error string when the JSON is invalid."
  (condition-case err
      (let ((items (if (fboundp 'json-parse-string)
                       (json-parse-string tasks :object-type 'plist
                                          :array-type 'list)
                     (json-read-from-string tasks))))
        (mapcar (lambda (item)
                  (list :preset (plist-get item :preset)
                        :task (plist-get item :task)
                        :context (or (plist-get item :context) "")))
                items))
    (error (format "[Error: invalid JSON tasks: %s]"
                   (error-message-string err)))))

(defun superchat--subagent-run-parallel-async (specs callback &optional depth placeholder)
  "Run sub-agent SPECS concurrently; CALLBACK gets the aggregated report.
Each spec is a plist (:preset :task :context).  At most
`superchat-subagent-parallel-max' sub-agents are in flight at once;
the rest queue and launch as slots free up.  Results aggregate in
spec order regardless of completion order.  DEPTH is passed through
to `superchat--subagent-run-async'.

Replaces the v1.2 `make-thread' implementation: llm.el requests run
on separate curl processes, so delegation is genuinely concurrent
and the UI never blocks on `thread-join'."
  (let* ((n (length specs))
         (results (make-vector n nil))
         (pending n)
         (next 0)
         (active 0)
         (window (max 1 superchat-subagent-parallel-max)))
    (if (zerop n)
        (funcall callback "[Error: no sub-agent specs given]")
      (cl-labels
          ((finish (idx spec report)
             (aset results idx (list :preset (plist-get spec :preset)
                                     :task (plist-get spec :task)
                                     :report report))
             (cl-decf pending)
             (cl-decf active)
             (if (zerop pending)
                 (funcall callback
                          (superchat--subagent-aggregate-reports
                           (append results nil)))
               (launch)))
           (launch ()
             (while (and (< next n) (< active window))
               (let ((idx next)
                     (spec (nth next specs)))
                 (cl-incf next)
                 (cl-incf active)
                 (let ((id (superchat--subagent-run-async
                            (plist-get spec :preset)
                            (plist-get spec :task)
                            (plist-get spec :context)
                            (lambda (report) (finish idx spec report))
                            depth)))
                   (superchat--subagent-set-placeholder id placeholder))))))
        (launch)))))

(defun superchat--subagent-aggregate-reports (results)
  "Format a list of sub-agent RESULTS into a single report string.
Each result is a plist with :preset, :report, and optionally :error."
  (mapconcat
   (lambda (r)
     (format "** Sub-agent report: %s\n#+begin_quote\n%s\n#+end_quote"
             (plist-get r :preset)
             (or (plist-get r :report)
                 (format "[ERROR: %s]" (plist-get r :error)))))
   results
   "\n\n"))

;; ═══════════════════════════════════════════════════════════
;; Main-buffer rendering
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--insert-system-message "superchat-render" (content))

(defun superchat--subagent-format-report (preset-name report)
  "Format a sub-agent REPORT block attributed to PRESET-NAME."
  (concat (propertize (format "** Sub-agent report: %s" preset-name)
                      'face 'font-lock-keyword-face)
          "\n#+begin_quote\n"
          (string-trim (or report ""))
          "\n#+end_quote\n"))

(defun superchat--subagent-render-report (preset-name report)
  "Render a sub-agent REPORT in the main superchat buffer.
PRESET-NAME is the name of the sub-agent that produced the report."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (superchat--subagent-format-report preset-name report))))))

;; ── Progress placeholders (async delegation) ──

(defun superchat--subagent-begin-placeholder (label)
  "Insert a running placeholder for LABEL in the main chat buffer.
Returns a (BEG-MARKER . END-MARKER) cons for
`superchat--subagent-end-placeholder', or nil when the chat buffer
does not exist.  Concurrent placeholders coexist: markers track
buffer edits, so replacing one does not disturb the others."
  (let ((buf (get-buffer (bound-and-true-p superchat-buffer-name))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (let ((beg (point-marker)))
              (insert (propertize (format "⏳ Sub-agent %s: running…" label)
                                  'face 'font-lock-comment-face)
                      "\n")
              (cons beg (point-marker)))))))))

(defun superchat--subagent-end-placeholder (placeholder label report)
  "Replace PLACEHOLDER with the final REPORT block for LABEL.
Falls back to appending at the end of the buffer when PLACEHOLDER
is nil or its buffer was killed."
  (if (and (consp placeholder)
           (markerp (car placeholder))
           (marker-buffer (car placeholder)))
      (with-current-buffer (marker-buffer (car placeholder))
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (car placeholder))
            (delete-region (car placeholder) (cdr placeholder))
            (insert (superchat--subagent-format-report label report))))
        (set-marker (car placeholder) nil)
        (set-marker (cdr placeholder) nil))
    (superchat--subagent-render-report label report)))

(provide 'superchat-subagent)

;;; superchat-subagent.el ends here
