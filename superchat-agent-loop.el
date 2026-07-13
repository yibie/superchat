;;; superchat-agent-loop.el --- Agent execution loop for Superchat -*- lexical-binding: t; -*-

;; This file is in the public domain.

;;; Commentary:

;; Agent mode coordinates multi-turn tool use for `type: agent' presets.
;; The actual multi-turn negotiation is handled by llm.el's native tool
;; support; this layer adds observability, tape logging, safety
;; guardrails, and per-tool lifecycle hooks.

;;; Code:

(require 'cl-lib)
(require 'superchat-preset)

;; External function declarations
(declare-function superchat--md-to-org "superchat-render" (text))
(declare-function superchat--prepare-assistant-response-area "superchat-render" ())
(declare-function superchat--insert-system-message "superchat-render" (content))
(declare-function superchat-db-tape-append "superchat-db" (session-id kind content &optional metadata))
(defvar superchat--session-id)
(defvar superchat-buffer-name)

;; ═══════════════════════════════════════════════════════════
;; Configuration
;; ═══════════════════════════════════════════════════════════

(defgroup superchat-agent nil
  "Agent-mode settings for superchat."
  :group 'superchat)

(defcustom superchat-agent-max-tool-calls 50
  "Maximum number of tool calls a single agent turn may execute.
This is a safety guard against runaway agents."
  :type 'integer
  :group 'superchat-agent)

(defcustom superchat-agent-confirm-destructive t
  "If non-nil, ask for confirmation before destructive tool calls.
When nil, the agent may execute write/shell/edit tools without
prompting.  Read-only tools are never confirmed."
  :type 'boolean
  :group 'superchat-agent)

(defcustom superchat-agent-destructive-tools
  '("write-file" "append-file" "EditBuffer" "shell-command" "bash"
    "eval-elisp")
  "Tool names considered destructive for confirmation purposes."
  :type '(repeat string)
  :group 'superchat-agent)

(defun superchat--agent-effective-guardrails (&optional preset)
  "Return tighten-only guardrails for PRESET and the global policy.
Attempts to raise the tool budget or disable required confirmation are
ignored with a warning."
  (let ((profile-max (and preset (superchat-preset-max-tool-calls preset)))
        (profile-confirm (and preset
                              (superchat-preset-confirm-destructive preset))))
    (when (and profile-max (> profile-max superchat-agent-max-tool-calls))
      (display-warning
       'superchat-agent
       (format "Preset %s cannot raise max_tool_calls above global limit %d"
               (superchat-preset-name preset)
               superchat-agent-max-tool-calls)
       :warning))
    (when (and preset superchat-agent-confirm-destructive
               (null profile-confirm))
      (display-warning
       'superchat-agent
       (format "Preset %s cannot disable global destructive-tool confirmation"
               (superchat-preset-name preset))
       :warning))
    (list :max-tool-calls
          (if profile-max
              (min superchat-agent-max-tool-calls profile-max)
            superchat-agent-max-tool-calls)
          :confirm-destructive
          (or superchat-agent-confirm-destructive
              (eq profile-confirm t)))))

;; ═══════════════════════════════════════════════════════════
;; State
;; ═══════════════════════════════════════════════════════════

(defvar-local superchat--agent-tool-call-count 0
  "Number of tool calls executed during the current agent run.")

(defvar-local superchat--agent-running nil
  "Non-nil when an agent loop is currently active in the buffer.")

;; ═══════════════════════════════════════════════════════════
;; Per-tool lifecycle hooks
;; ═══════════════════════════════════════════════════════════

(defvar superchat-agent-pre-tool-functions nil
  "Hook run before every tool call in agent mode.
Each function receives (TOOL-NAME ARGS).  Return value ignored.")

(defvar superchat-agent-permission-functions nil
  "Hook run before every tool call to gate execution.
Each function receives (TOOL-NAME ARGS) and should return:
  \\='allow  — skip user confirmation, allow the tool
  \\='deny   — deny the tool call (returns error string)
  nil     — defer to normal confirmation logic.
All hooks run; \\='deny wins over \\='allow.")

(defvar superchat-agent-post-tool-functions nil
  "Hook run after a successful tool call in agent mode.
Each function receives (TOOL-NAME ARGS RESULT).  Return value ignored.")

(defvar superchat-agent-post-tool-failure-functions nil
  "Hook run after a failed tool call in agent mode.
Each function receives (TOOL-NAME ARGS ERROR).  Return value ignored.")

(defun superchat--agent-run-tool-hooks (hooks &rest args)
  "Run HOOKS with ARGS, ignoring errors in individual hooks."
  (dolist (fn hooks)
    (when (functionp fn)
      (ignore-errors (apply fn args)))))

(defun superchat--agent-check-permission (tool-name args)
  "Check all permission hooks for TOOL-NAME with ARGS.
Returns \\='allow, \\='deny, or nil (defer to normal confirmation)."
  (let ((results (delq nil
                  (mapcar (lambda (fn)
                            (ignore-errors
                              (funcall fn tool-name args)))
                          superchat-agent-permission-functions))))
    (cond ((memq 'deny results) 'deny)
          ((memq 'allow results) 'allow)
          (t nil))))

;; ═══════════════════════════════════════════════════════════
;; Tool wrapping
;; ═══════════════════════════════════════════════════════════

(declare-function llm-tool-name "llm" (tool))
(declare-function llm-tool-description "llm" (tool))
(declare-function llm-tool-args "llm" (tool))
(declare-function llm-tool-async "llm" (tool))
(declare-function llm-tool-function "llm" (tool))
(declare-function make-llm-tool "llm" (&rest args))

(defun superchat--agent-destructive-p (tool-name _args)
  "Return non-nil if TOOL-NAME is considered destructive."
  (member tool-name superchat-agent-destructive-tools))

(defun superchat--agent-confirm-p (tool-name args &optional guardrails)
  "Return non-nil if the tool call should be confirmed.
Respects `superchat-agent-confirm-destructive'."
  (and (if guardrails
           (plist-get guardrails :confirm-destructive)
         superchat-agent-confirm-destructive)
       (superchat--agent-destructive-p tool-name args)))

(defun superchat--agent-ask-confirm (tool-name args)
  "Ask user to confirm TOOL-NAME with ARGS.
Returns t if confirmed, nil otherwise."
  (y-or-n-p
   (format "Agent wants to call `%s' with args %S. Allow? "
           tool-name (if (> (length (format "%S" args)) 200)
                         "(see details in buffer)"
                       args))))

(defun superchat--agent-ensure-session-id ()
  "Return `superchat--session-id', creating one if necessary."
  (or (and (boundp 'superchat--session-id) superchat--session-id)
      (setq superchat--session-id
            (format "%s%04x"
                    (format-time-string "%Y%m%d-%H%M%S-")
                    (random 65536)))))

(defun superchat--agent-log-tool-call (tool-name args)
  "Write a `tool_call' row to the tape."
  (when (fboundp 'superchat-db-tape-append)
    (ignore-errors
      (superchat-db-tape-append
       (superchat--agent-ensure-session-id)
       "tool_call"
       (format "%s(%S)" tool-name args)
       :meta `(:tool_name ,tool-name :args ,args)))))

(defun superchat--agent-log-tool-result (tool-name result)
  "Write a `tool_result' row to the tape."
  (when (fboundp 'superchat-db-tape-append)
    (ignore-errors
      (superchat-db-tape-append
       (superchat--agent-ensure-session-id)
       "tool_result"
       (format "%s -> %s" tool-name
               (if (> (length (format "%S" result)) 1000)
                   "(truncated)"
                 result))
       :meta `(:tool_name ,tool-name :result ,result)))))

(defun superchat--agent-render-tool-call (tool-name args)
  "Render a tool call in the chat buffer."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (format "** Tool call: %s" tool-name)
                            'face 'font-lock-function-name-face))
        (insert "\n#+begin_example\n")
        (insert (string-trim (format "%S" args)))
        (insert "\n#+end_example\n")))))

(defun superchat--agent-render-tool-result (tool-name result)
  "Render a tool result in the chat buffer."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (format "** Tool result: %s" tool-name)
                            'face 'font-lock-string-face))
        (insert "\n#+begin_example\n")
        (let ((text (string-trim (format "%s" result))))
          (insert (if (> (length text) 2000)
                      (concat (substring text 0 2000) "\n...")
                    text)))
        (insert "\n#+end_example\n")))))

;; ── Permission-aware sync execution ──

(defun superchat--agent-execute-sync (tool-name args original-fn guardrails)
  "Execute ORIGINAL-FN with hooks, permission check, and error wrapping.
Returns the tool result."
  (cl-labels ((do-call ()
                (condition-case err
                    (let ((r (apply original-fn args)))
                      (superchat--agent-run-tool-hooks
                       superchat-agent-post-tool-functions
                       tool-name args r)
                      r)
                  (error
                   (superchat--agent-run-tool-hooks
                    superchat-agent-post-tool-failure-functions
                    tool-name args err)
                   (signal (car err) (cdr err))))))
    (let ((permission (superchat--agent-check-permission tool-name args)))
      (cond
       ((eq permission 'deny)
        "[Tool call denied by permission hook]")
       ((eq permission 'allow)
        (do-call))
       (t
        (if (superchat--agent-confirm-p tool-name args guardrails)
            (if (superchat--agent-ask-confirm tool-name args)
                (do-call)
              "[Tool call cancelled by user]")
          (do-call)))))))

(defun superchat--agent-wrap-function (tool-name original-fn async &optional guardrails counter)
  "Return a wrapped version of ORIGINAL-FN for agent mode.
The wrapper renders and logs each call/result, runs per-tool lifecycle
hooks, and enforces the max-tool-calls limit.  ASYNC is non-nil if the
tool is asynchronous."
  (let* ((isolated-counter counter)
         (counter (or counter (cons superchat--agent-tool-call-count nil)))
         (max-tool-calls (and guardrails
                              (plist-get guardrails :max-tool-calls))))
    (if async
      (lambda (callback &rest args)
        (cl-incf (car counter))
        (unless isolated-counter
          (setq superchat--agent-tool-call-count (car counter)))
        (if (> (car counter)
               (or max-tool-calls superchat-agent-max-tool-calls))
            (progn
              (cl-decf (car counter))
              (unless isolated-counter
                (setq superchat--agent-tool-call-count (car counter)))
              (funcall callback
                       (format "[Agent stopped: exceeded maximum of %d tool calls]"
                               (or max-tool-calls
                                   superchat-agent-max-tool-calls))))
          (superchat--agent-render-tool-call tool-name args)
          (superchat--agent-log-tool-call tool-name args)
          (superchat--agent-run-tool-hooks
           superchat-agent-pre-tool-functions tool-name args)
          ;; Wrap done-cb to inject post hooks.  Apply is wrapped in
          ;; condition-case so sync errors from the async tool setup
          ;; also trigger failure hooks.
          (let* ((done-cb (lambda (result)
                            (superchat--agent-render-tool-result tool-name result)
                            (superchat--agent-log-tool-result tool-name result)
                            (funcall callback result)))
                 (wrapped-done-cb (lambda (result)
                                   (superchat--agent-run-tool-hooks
                                    superchat-agent-post-tool-functions
                                    tool-name args result)
                                   (funcall done-cb result)))
                 (cancelled "[Tool call cancelled by user]")
                 (permission (superchat--agent-check-permission tool-name args))
                 (do-apply
                  (lambda ()
                    (condition-case err
                        (apply original-fn wrapped-done-cb args)
                      (error
                       (superchat--agent-run-tool-hooks
                        superchat-agent-post-tool-failure-functions
                        tool-name args err)
                       (signal (car err) (cdr err)))))))
            (cond
             ((eq permission 'deny)
              (superchat--agent-render-tool-result
               tool-name "[Tool call denied by permission hook]")
              (superchat--agent-log-tool-result
               tool-name "[Tool call denied by permission hook]")
              (funcall callback "[Tool call denied by permission hook]"))
             ((eq permission 'allow)
              (funcall do-apply))
             (t
              (if (superchat--agent-confirm-p tool-name args guardrails)
                  (if (superchat--agent-ask-confirm tool-name args)
                      (funcall do-apply)
                    (superchat--agent-render-tool-result tool-name cancelled)
                    (superchat--agent-log-tool-result tool-name cancelled)
                    (funcall callback cancelled))
                (funcall do-apply)))))))
    (lambda (&rest args)
      (cl-incf (car counter))
      (unless isolated-counter
        (setq superchat--agent-tool-call-count (car counter)))
      (if (> (car counter)
             (or max-tool-calls superchat-agent-max-tool-calls))
          (progn
            (cl-decf (car counter))
            (unless isolated-counter
              (setq superchat--agent-tool-call-count (car counter)))
            (format "[Agent stopped: exceeded maximum of %d tool calls]"
                    (or max-tool-calls superchat-agent-max-tool-calls)))
        (superchat--agent-render-tool-call tool-name args)
        (superchat--agent-log-tool-call tool-name args)
        (superchat--agent-run-tool-hooks
         superchat-agent-pre-tool-functions tool-name args)
        (let ((result (superchat--agent-execute-sync
                       tool-name args original-fn guardrails)))
          (superchat--agent-render-tool-result tool-name result)
          (superchat--agent-log-tool-result tool-name result)
          result))))))

(defun superchat--agent-wrap-tool (tool &optional guardrails counter)
  "Return an agent-mode wrapper around an llm TOOL struct."
  (make-llm-tool
   :function (superchat--agent-wrap-function
              (llm-tool-name tool)
              (llm-tool-function tool)
              (llm-tool-async tool)
              guardrails counter)
   :name (llm-tool-name tool)
   :description (llm-tool-description tool)
   :args (llm-tool-args tool)
   :async (llm-tool-async tool)))

(defun superchat--agent-wrap-tools (tools &optional preset)
  "Wrap all TOOLS for agent-mode observability and guardrails."
  (let ((guardrails (superchat--agent-effective-guardrails preset)))
    (let ((counter (cons 0 nil)))
      (mapcar (lambda (tool)
                (superchat--agent-wrap-tool tool guardrails counter))
              tools))))

;; ═══════════════════════════════════════════════════════════
;; Entry point
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--execute-llm-query "superchat-dispatcher" (turn &optional template target-model))
(declare-function superchat--llm-generate-answer "superchat" (prompt callback stream-callback &optional target-model context-files tools agent-mode system-prompt preset))
(declare-function superchat--process-llm-result "superchat-render" (answer))
(declare-function superchat--stream-llm-result "superchat-render" (chunk))

(defun superchat--agent-run (turn)
  "Execute TURN in agent mode.
Sets up agent state, wraps tools, and delegates the multi-turn
call to `superchat--llm-generate-answer'."
  (let* ((result (superchat--execute-llm-query turn))
         (tools (plist-get result :tools))
         (finish-callback
          (lambda (response)
            (unwind-protect
                (when (fboundp 'superchat--process-llm-result)
                  (superchat--process-llm-result response))
              (superchat--agent-finish)))))
    (with-current-buffer (get-buffer-create (bound-and-true-p superchat-buffer-name))
      (setq superchat--agent-tool-call-count 0
            superchat--agent-running t))
    (superchat--llm-generate-answer
     (plist-get result :prompt)
     finish-callback
     #'superchat--stream-llm-result
     (plist-get result :target-model)
     (bound-and-true-p superchat--current-context-files)
     tools
     t
     (plist-get result :system-prompt)
     (plist-get result :preset))))

(defun superchat--agent-finish ()
  "Clean up agent state after a run completes."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (setq superchat--agent-running nil))))

(provide 'superchat-agent-loop)

;;; superchat-agent-loop.el ends here
