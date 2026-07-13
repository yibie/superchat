;;; superchat-dispatcher.el --- Command dispatch and prompt building for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; User input dispatch, prompt building, and the main send-input pipeline.
;; Extracted from superchat.el monolith (v0.9 split step 6 — pure move only,
;; hook registration deferred to follow-up).
;;
;; NOTE: The bub-alignment fix (registering prompt-build logic as
;; superchat-system-prompt-functions / superchat-build-prompt-functions
;; hooks) is deferred to a follow-up because it changes execution flow.

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
(require 'superchat-core)
(require 'superchat-db)
(require 'superchat-models)
(require 'superchat-memory)
(require 'superchat-skills)
(require 'superchat-preset)
(require 'superchat-parser)
(require 'superchat-tools)
(require 'superchat-render)
(require 'superchat-llm)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-buffer-name)
(defvar superchat-lang)
(defvar superchat--prompt-start)
(defvar superchat--response-start-marker)
(defvar superchat--current-command)
(defvar superchat--active-preset)
(defvar superchat--current-context-files)
(defvar superchat--conversation-history)
(defvar superchat--pending-recalled-memories)
(defvar superchat--session-id)
(defvar superchat--current-turn)
(defvar superchat--ttft-start-time)
(defvar superchat-show-ttft)
(defvar superchat-show-ttft-breakdown)
(defvar superchat-file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-inline-context-template)
(defvar superchat-inline-max-bytes)
(defvar superchat-prompt-file-extensions)
(defvar superchat-general-answer-prompt)
(defvar superchat--user-commands)
(defvar superchat--builtin-commands)
(defvar superchat-context-message-count)
(defvar superchat-context-max-chars)
(defvar superchat--file-ref-regexp)
(defvar superchat--current-response-parts)
(defvar superchat--assistant-response-start-marker)
(declare-function superchat--strip-leading-user-label "superchat" (input))
(declare-function superchat--input-meets-memory-threshold-p "superchat" (input))
(declare-function superchat--lookup-command-template "superchat" (command))
(declare-function superchat--record-message "superchat" (role content))
(declare-function superchat--update-status "superchat-render" (message))
(declare-function superchat--insert-system-message "superchat-render" (text))
(declare-function superchat--refresh-prompt "superchat-render" ())
(declare-function superchat--insert-prompt "superchat-render" ())
(declare-function superchat--prepare-for-response "superchat-render" ())
(declare-function superchat--ttft-log "superchat-render" (stage))
(declare-function superchat--conversation-context-string "superchat" (limit))
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--get-available-models "superchat-models" ())
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function superchat-view-search "superchat-tape-view" (query &optional session-id limit))
(declare-function superchat-view-row-to-memory-plist "superchat-tape-view" (row))

(defun superchat--normalize-file-path (file-path)
  "Normalize FILE-PATH: unescape spaces, strip quotes, expand, trim newline."
  (when (and file-path (stringp file-path))
    (let ((fp (replace-regexp-in-string "\\\\ " " " file-path)))
      (when (and (>= (length fp) 2)
                 (string= "\"" (substring fp 0 1))
                 (string= "\"" (substring fp -1)))
        (setq fp (substring fp 1 -1)))
      (setq fp (expand-file-name fp))
      (setq fp (replace-regexp-in-string "\n$" "" fp))
      fp)))

(defun superchat--extract-file-path (input)
  "Extract and normalize file path from INPUT string.
Handles various edge cases like spaces, parentheses, and quotes in file paths."
  (let ((file-path (superchat-parser-extract-file-path input)))
    (when file-path
      ;; --- DIAGNOSTIC MESSAGE ---
      (message "superchat: Extracted file path: %s" file-path)
      (message "superchat: File exists: %s" (file-exists-p file-path))
      ;; --- END DIAGNOSTIC ---
      file-path)))

(defun superchat--format-retrieved-memories (memories)
  "Format MEMORIES into a string for the prompt.
Handles both plist (org-based) and list-of-lists (SQLite DB) formats."
  (if (not memories)
      ""
    (with-temp-buffer
      (insert "--- Retrieved Memories ---\n")
      (dolist (mem memories)
        (let* (;; SQLite row = (id title content keywords mood status created_at);
               ;; org plist = (:title ... :content ... :id ... :timestamp ... :tags ...).
               ;; Discriminate on (car mem): integer id for DB rows, keyword for plists.
               (entry (if (integerp (car-safe mem))
                          (list :id (nth 0 mem)
                                :title (or (nth 1 mem) "")
                                :content (nth 2 mem)
                                :timestamp (nth 6 mem)
                                :tags (nth 3 mem))
                        mem))
               (title (plist-get entry :title))
               (content (plist-get entry :content))
               (id (plist-get entry :id))
               (timestamp (plist-get entry :timestamp))
               (tags (plist-get entry :tags)))
          (insert (format "* %s (ID: %s)\n" (or title "Untitled") id))
          (when (or timestamp tags)
            (insert (format "  :TIMESTAMP: %s  :TAGS: %s\n"
                            (or timestamp "") (or tags ""))))
          (when content
            (insert content)
            (unless (string-suffix-p "\n" content)
              (insert "\n")))))
      (insert "--- End of Retrieved Memories ---\n\n")
      (buffer-string))))

(defun superchat--textual-file-p (path)
  "Return non-nil if PATH looks like a textual file we can inline."
  (let* ((ext (downcase (or (file-name-extension path) "")))
         (text-exts '("org" "md" "markdown" "txt")))
    (member ext text-exts)))

(defun superchat--read-inline-file-content (file-path)
  "Read and trim FILE-PATH for use in prompts.
Returns the file content as a string, or nil on failure."
  (when (and (stringp file-path)
             (file-exists-p file-path))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file-path nil 0 superchat-inline-max-bytes)
          (string-trim (buffer-string)))
      (error
       (message "Warning: Failed to read file %s: %s" file-path (error-message-string err))
       nil))))

(defun superchat--render-inline-context (file-path content)
  "Render an inline context block from FILE-PATH and CONTENT.
Returns a string or nil if CONTENT is empty."
  (when (and content (not (string-empty-p content)))
    (let* ((tpl superchat-inline-context-template)
           (step1 (replace-regexp-in-string (regexp-quote "$path") (or file-path "") tpl t t))
           (rendered (replace-regexp-in-string (regexp-quote "$content") content step1 t t)))
      (message "superchat: Inlined %d characters from %s" (length content) file-path)
      rendered)))

(defun superchat--execute-llm-query (turn &optional template target-model)
  "Return a result plist for the dispatcher from a processed TURN.
TURN must have already been run through `superchat-core-run-turn'
so its `prompt' and `system-prompt' slots are populated by hooks.

When TEMPLATE is non-nil, re-runs the build-prompt hooks with
`superchat-general-answer-prompt' dynamically bound to TEMPLATE.
TARGET-MODEL is an optional one-shot model override; when nil, the
turn's own target-model is used — that is how a preset's model
(applied after the dispatcher captured its binding) and the
sub-agent path (which calls with no explicit model) reach the
effective backend."
  (setq target-model (or target-model (superchat-turn-target-model turn)))
  (when template
    (let ((superchat-general-answer-prompt template))
      (setf (superchat-turn-prompt turn) "")
      (setq turn (superchat-core--run-hook-chain
                  turn 'superchat-build-prompt-functions))))
  ;; Fallback: if hooks are unregistered / produced nothing, build a
  ;; minimal prompt from clean-input so tests without hooks still pass.
  (when (string-empty-p (superchat-turn-prompt turn))
    (let ((query (or (superchat-turn-clean-input turn) "")))
      (setf (superchat-turn-prompt turn)
            (concat "User question: " query))))
  `(:type :llm-query :prompt ,(superchat-turn-prompt turn)
    :user-message ,(unless (string-empty-p (superchat-turn-clean-input turn))
                     (superchat-turn-clean-input turn))
    ,@(when (superchat-turn-preset turn)
        `(:preset ,(superchat-turn-preset turn)))
    ,@(when target-model `(:target-model ,target-model))
    ,@(let ((sys (superchat-turn-system-prompt turn)))
        (when (and (stringp sys) (not (string-empty-p (string-trim sys))))
          `(:system-prompt ,sys)))
    ,@(when (superchat-turn-tools turn)
        `(:tools ,(superchat-turn-tools turn)))))

(defun superchat--handle-command (command args input &optional lang _target-model)
  "Dispatch COMMAND with ARGS, INPUT, and LANG via alist, hook chain, then builtins."
  (superchat--ensure-command-loaded command)
  (or
   ;; 1. Alist lookup (fast path for registered commands)
   (when-let ((handler (cdr (assoc command superchat--command-alist))))
     (funcall handler command args input lang nil))
   ;; 2. Hook chain (third-party commands)
   (run-hook-with-args-until-success
    'superchat-command-hooks command args input lang nil)
   ;; 3. Default: builtin/user commands
   (superchat--handle-default-command command args input lang)))

(defun superchat--handle-default-command (command args input lang)
  "Handle COMMAND with ARGS, INPUT, and LANG that wasn't covered by hooks.
This is the catch-all fallback after the hook chain."
  (cond
   ;; Builtin commands (from superchat--builtin-commands alist)
   ((assoc command superchat--builtin-commands)
    (let ((func (cdr (assoc command superchat--builtin-commands))))
      (if (and func (fboundp func))
          (let ((result (funcall func)))
            (if (and result (stringp result))
                `(:type :buffer :content ,result)
              `(:type :noop)))
        `(:type :echo :content
          ,(format "Command `/%s' function not found." command)))))
   ;; User-defined commands (loaded from prompt files)
   ((gethash command superchat--user-commands)
    (let ((item (gethash command superchat--user-commands)))
      (if (and args (> (length args) 0))
          (progn
            (setq superchat--current-command command)
            `(:type :llm-query-and-mode-switch :args ,args :template ,item :lang ,lang))
        (progn
          (setq superchat--current-command command)
          `(:type :echo :content ,(format "Switched to command mode: `/%s'." command))))))
   ;; Unknown command
   (t
    `(:type :echo :content ,(format "Unknown command: `/%s'." command)))))

;;;###autoload
(defun superchat-send-input ()
  "Parse user input, run through hook pipeline, dispatch, and render result."
  (interactive)
  ;; ── TTFT end-to-end: start clock on C-c C-c ──
  (when superchat-show-ttft
    (setq superchat--ttft-start-time (float-time)))
  (let* ((raw-input (when (and superchat--prompt-start
                                (marker-position superchat--prompt-start))
                      (buffer-substring-no-properties
                       superchat--prompt-start (point-max))))
         (input (string-trim
                 (superchat--strip-leading-user-label (or raw-input ""))))
         ;; Fix buffer if label was stripped
         (_ (when (and raw-input
                       (not (equal raw-input
                                   (superchat--strip-leading-user-label raw-input))))
              (with-current-buffer (get-buffer-create superchat-buffer-name)
                (let ((inhibit-read-only t))
                  (goto-char superchat--prompt-start)
                  (delete-region (point) (point-max))
                  (insert input))))))
    (when (> (length input) 0)
      (let* ((turn (superchat-turn-new input superchat--session-id))
             (lang superchat-lang))
        (superchat--ttft-log "input-parsed")
        ;; Auto-recall — fills turn.retrieved-memories.
        ;;
        ;; 1. If `/recall' pre-seeded `superchat--pending-recalled-memories',
        ;;    move that list onto the turn and clear it so it doesn't
        ;;    bleed into later turns.
        ;; 2. Otherwise, when the input meets the threshold and the SQLite
        ;;    memory table has accepted rows, run the indexed search
        ;;    synchronously — single LIKE on small tables is sub-ms.
        (cond
         (superchat--pending-recalled-memories
          (setf (superchat-turn-retrieved-memories turn)
                superchat--pending-recalled-memories)
          (setq superchat--pending-recalled-memories nil))
         ((and (superchat--input-meets-memory-threshold-p input)
               (fboundp 'superchat-view-search)
               (boundp 'superchat--session-id)
               superchat--session-id)
          (when-let ((rows (superchat-view-search input superchat--session-id 5)))
            (let ((memories (mapcar #'superchat-view-row-to-memory-plist rows)))
              (when memories
                (setf (superchat-turn-retrieved-memories turn) memories))))))
        (superchat--ttft-log "memory-recall")
        (let ((inhibit-read-only t)
              (end-of-input (point-max)))
          (put-text-property superchat--prompt-start end-of-input 'read-only t)
          (put-text-property superchat--prompt-start end-of-input
                             'superchat-role 'user)
          (setq superchat--prompt-start nil))
        (superchat--prepare-for-response)
        ;; ── Step D: Topic lifecycle hooks + core pipeline ──
        (run-hook-with-args 'superchat-pre-topic-functions turn)
        (let* ((prepared (superchat-core-run-turn turn)))
          (when superchat-post-topic-functions
            (setq prepared (superchat-core--run-hook-chain
                            prepared 'superchat-post-topic-functions)))
          (let* ((command (superchat-turn-command prepared))
                 (skill (superchat-turn-skill prepared))
                 (target-model (superchat-turn-target-model prepared))
                 (clean-input (superchat-turn-clean-input prepared))
                 (superchat--current-turn prepared))
            (superchat--ttft-log "core-pipeline")
          ;; Inherit active preset when no explicit skill was given.
          (when (and (null skill)
                     (boundp 'superchat--active-preset)
                     superchat--active-preset)
            (superchat-preset-apply superchat--active-preset prepared))
          (cond
           ;; Workflow invocation: >>name [args] (or >workflow <name> [args])
           ;; Core parsing rewrote either form to the reserved `workflow'
           ;; skill token with "<name> [args]" left in clean-input.  The
           ;; first token is the .workflow file name, the rest is passed
           ;; as $input.  The async executor renders its own output, so
           ;; nothing is returned to dispatch.
           ((and skill (string= skill "workflow")
                 (fboundp 'superchat-workflow-execute-async))
            (let* ((rest (string-trim (or clean-input "")))
                   (name (when (string-match "^\\([a-zA-Z0-9_-]+\\)" rest)
                           (match-string 1 rest)))
                   (args (if name
                             (string-trim (substring rest (match-end 1)))
                           "")))
              (if (and name
                       (fboundp 'superchat-workflow--exists-p)
                       (superchat-workflow--exists-p name))
                  (superchat-workflow-execute-async name args)
                (superchat--dispatch-result
                 (list :type :buffer
                       :content (if name
                                    (format "No such workflow: %s" name)
                                  "Usage: >>name [args]"))
                 lang target-model))))
           ;; Skill invocation (>skill-name)
           (skill
            (let ((result (when (fboundp 'superchat-skills-invoke)
                            (superchat-skills-invoke skill input prepared))))
              ;; If the invoked skill is an agent/plan preset, make it the
              ;; active preset for the rest of the session.
              (when-let* ((preset (and result (plist-get result :preset)))
                          ((or (superchat-preset-agent-p preset)
                               (superchat-preset-plan-p preset)))
                          ((boundp 'superchat--active-preset)))
                (setq superchat--active-preset preset)
                (message "🎯 Active preset: %s (%s)"
                         (superchat-preset-name preset)
                         (superchat-preset-type preset)))
              (if (and (consp result)
                       (memq (plist-get result :type)
                             '(:llm-query :llm-query-and-mode-switch)))
                  (progn
                    ;; Use the skill-combined prompt for downstream execution.
                    (setf (superchat-turn-prompt prepared)
                          (plist-get result :prompt))
                    (superchat--dispatch-llm-or-agent prepared lang target-model))
                (superchat--dispatch-llm-or-agent prepared lang target-model))))
           ;; Command dispatch (keeps existing handler)
           (command
            (superchat--dispatch-result
             (or (superchat--handle-command
                  command (superchat-turn-command-args prepared)
                  clean-input lang target-model)
                 (superchat--execute-llm-query prepared nil target-model))
             lang target-model))
           ;; Active command mode
           (superchat--current-command
            (let ((template (superchat--lookup-command-template
                             superchat--current-command)))
              (superchat--dispatch-llm-or-agent prepared lang target-model template)))
           ;; Default: plain LLM query
           (t
            (superchat--dispatch-llm-or-agent prepared lang target-model)))))))))

(defun superchat--dispatch-llm-or-agent (turn lang target-model &optional template)
  "Dispatch TURN to either agent loop or normal LLM query.
When TURN has an agent preset and `superchat--agent-run' is
available, run the agent loop; otherwise dispatch a normal query
using optional TEMPLATE."
  (if (and (superchat-turn-preset turn)
           (superchat-preset-agent-p (superchat-turn-preset turn))
           (fboundp 'superchat--agent-run))
      (superchat--agent-run turn)
    (superchat--dispatch-result
     (superchat--execute-llm-query turn template target-model)
     lang target-model)))

(defun superchat--dispatch-result (result lang target-model)
  "Handle a RESULT plist from command dispatch, using LANG and TARGET-MODEL.
Extracted from old superchat-send-input for reuse in command handlers."
  (pcase (plist-get result :type)
    (:buffer
     (let ((content (plist-get result :content)))
       (with-current-buffer (get-buffer-create superchat-buffer-name)
         (let ((inhibit-read-only t))
           (when (and superchat--response-start-marker
                      (marker-position superchat--response-start-marker))
             (goto-char superchat--response-start-marker)
             (delete-region (point) (point-max)))
           (goto-char (point-max))
           (insert "\n** System\n")
           (insert content)
           (insert "\n")
           (superchat--insert-prompt)))))
    (:echo
     (message "%s" (plist-get result :content))
     (superchat--refresh-prompt))
    (:noop nil)
    (:llm-query
     (superchat--update-status "Assistant is thinking...")
     (superchat--ttft-log "dispatch")
     (superchat--llm-generate-answer
      (plist-get result :prompt)
      #'superchat--process-llm-result
      #'superchat--stream-llm-result
      (plist-get result :target-model)
      superchat--current-context-files
      (plist-get result :tools)
      nil
      (plist-get result :system-prompt)
      (plist-get result :preset)))
    (:llm-query-and-mode-switch
     (superchat--update-status
      (format "Executing `/%s'..."
              (or (plist-get result :command) "?")))
     (let* ((real-args (plist-get result :args))
            (template (plist-get result :template))
            (target-model (plist-get result :target-model))
            (turn (superchat-turn-new real-args))
            (prepared (superchat-core-run-turn turn))
            (llm-result (superchat--execute-llm-query
                         prepared template target-model)))
       (superchat--llm-generate-answer
        (plist-get llm-result :prompt)
        #'superchat--process-llm-result
        #'superchat--stream-llm-result
        (plist-get llm-result :target-model)
        superchat--current-context-files
        (plist-get llm-result :tools)
        nil
        (plist-get llm-result :system-prompt)
        (plist-get llm-result :preset))))))


(provide 'superchat-dispatcher)
;;; superchat-dispatcher.el ends here
