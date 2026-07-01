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
(declare-function superchat--llm-generate-answer-sync "superchat-llm" (prompt &optional target-model tools agent-mode))
(declare-function superchat--execute-llm-query "superchat-dispatcher" (turn &optional template target-model))
(declare-function superchat--agent-wrap-tools "superchat-agent-loop" (tools))
(declare-function superchat-get-llm-tools "superchat-tools" ())
(declare-function superchat--collect-llm-tools "superchat-llm" (&optional input tool-names))

(defvar superchat--session-id)
(defvar superchat--conversation-history)
(defvar superchat--active-preset)
(defvar superchat-buffer-name)

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

(defun superchat--subagent-preset (name)
  "Return a sub-agent preset by NAME (symbol or string)."
  (let ((sym (if (symbolp name) name (intern (downcase name)))))
    (pcase sym
      ('researcher (superchat--subagent-preset-researcher))
      ('executor (superchat--subagent-preset-executor))
      ('introspector (superchat--subagent-preset-introspector))
      (_ (let ((skill (and (fboundp 'superchat-skills-load)
                           (superchat-skills-load (symbol-name sym)))))
           (when (and skill (superchat-preset-agent-p skill))
             skill))))))

;; ═══════════════════════════════════════════════════════════
;; Sub-agent runner
;; ═══════════════════════════════════════════════════════════

(defcustom superchat-subagent-parallel-max 3
  "Maximum number of sub-agents to run concurrently."
  :type 'integer
  :group 'superchat)

(defun superchat--subagent-run (preset-name task &optional context)
  "Run a sub-agent PRESET-NAME on TASK with optional CONTEXT.
Returns the final report string.  The sub-agent has an isolated
session_id and tape; its tool calls are not rendered in the main
chat buffer."
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
        (progn
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
             t)))
      (when (buffer-live-p temp-buffer)
        (kill-buffer temp-buffer)))))

(defun superchat--subagent-run-in-thread (spec results)
  "Run a single sub-agent SPEC in a thread, storing the result in RESULTS.
SPEC is a plist with :preset, :task, :context, :index.
RESULTS is a mutable vector."
  (make-thread
   (lambda ()
     (let* ((preset (plist-get spec :preset))
            (task (plist-get spec :task))
            (context (plist-get spec :context))
            (idx (plist-get spec :index))
            (result (condition-case err
                        (list :preset preset
                              :task task
                              :report (superchat--subagent-run preset task context))
                      (error (list :preset preset
                                   :task task
                                   :error (error-message-string err))))))
       (aset results idx result)))
   (format "superchat-subagent:%s" (plist-get spec :preset))))

(defun superchat--subagent-run-parallel (specs)
  "Run multiple sub-agent SPECS in parallel and return aggregated report string.
Each spec is a plist (:preset :task :context).  Concurrency is capped
by `superchat-subagent-parallel-max'."
  (let* ((n (length specs))
         (results (make-vector n nil))
         (max (max 1 superchat-subagent-parallel-max))
         (idx 0))
    (while (< idx n)
      (let* ((chunk-end (min (+ idx max) n))
             (threads nil))
        (dotimes (i (- chunk-end idx))
          (let* ((spec-idx (+ idx i))
                 (spec (plist-put (copy-sequence (nth spec-idx specs)) :index spec-idx)))
            (push (superchat--subagent-run-in-thread spec results) threads)))
        (dolist (thread threads)
          (thread-join thread)))
      (setq idx (+ idx max)))
    (superchat--subagent-aggregate-reports (append results nil))))

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

(defun superchat--subagent-render-report (preset-name report)
  "Render a sub-agent REPORT in the main superchat buffer.
PRESET-NAME is the name of the sub-agent that produced the report."
  (when (buffer-live-p (get-buffer (bound-and-true-p superchat-buffer-name)))
    (with-current-buffer (get-buffer (bound-and-true-p superchat-buffer-name))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (propertize (format "** Sub-agent report: %s" preset-name)
                            'face 'font-lock-keyword-face))
        (insert "\n#+begin_quote\n")
        (insert (string-trim report))
        (insert "\n#+end_quote\n")))))

(provide 'superchat-subagent)

;;; superchat-subagent.el ends here
