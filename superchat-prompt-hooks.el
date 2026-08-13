;;; superchat-prompt-hooks.el — Prompt-building hooks for superchat-core pipeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Five focused hook functions that replace the monolithic
;; `superchat--build-final-prompt'.  Each hook receives a
;; `superchat-turn' struct, reads/writes its slots, and returns
;; the modified turn.
;;
;; Hook signatures: (turn) → modified-turn or nil
;; nil signals "skip me" — the core runner discards it.

;;; Code:

(require 'cl-lib)
(require 'superchat-core)
(require 'superchat-tape-view)

;; ── Forward declarations (owned by other modules) ──
(defvar superchat-lang)
(defvar superchat-general-answer-prompt)
(defvar superchat-context-message-count)
(defvar superchat-context-max-chars)
(defvar superchat--file-ref-regexp)
(defvar superchat-inline-file-content)
(defvar superchat-inline-context-template)
(defvar superchat-inline-max-bytes)
(defvar superchat-llm-tools-enabled)

(declare-function superchat-genui-action-names "superchat-genui" ())
(declare-function superchat-experience-assemble-view
                  "superchat-experience" (session-ids scope context))

(declare-function superchat--normalize-file-path "superchat-dispatcher" (file-path))
(declare-function superchat--textual-file-p "superchat-dispatcher" (path))
(declare-function superchat--read-inline-file-content "superchat-dispatcher" (file-path))
(declare-function superchat--render-inline-context "superchat-dispatcher" (file-path content))
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--format-retrieved-memories "superchat-dispatcher" (memories))

;; ═══════════════════════════════════════════════════════════
;; System-prompt hooks
;; ═══════════════════════════════════════════════════════════

(defun superchat-prompt-hook--language-instruction (turn)
  "Append a language instruction to TURN's system-prompt if needed.
When `superchat-lang' is not English and the template does not
already reference `$lang', set a language directive."
  (let* ((current-lang (or superchat-lang "English"))
         (template (or superchat-general-answer-prompt "")))
    (unless (or (string-empty-p current-lang)
                (string= current-lang "English")
                (string-match-p (regexp-quote "$lang") template))
      (setf (superchat-turn-system-prompt turn)
            (concat (superchat-turn-system-prompt turn)
                    (unless (string-empty-p (superchat-turn-system-prompt turn))
                      "\n")
                    (format "Your response must be in %s." current-lang)))))
  turn)

(defun superchat-prompt-hook--self-extension (turn)
  "Tell the model it may write a tool, when `define_tool' is available.

Without this the capability is built and never triggered: faced with a
task no tool covers, a model answers from memory rather than wondering
whether it could build the tool it lacks.  Injected only when the tool is
actually in the active set, so nobody pays for it who has not enabled it."
  (when (and (boundp 'superchat-llm-tool-names)
             (or (eq superchat-llm-tool-names 'all)
                 (member "define_tool" superchat-llm-tool-names))
             (let ((preset (bound-and-true-p superchat--active-preset)))
               (or (null preset)
                   (not (fboundp 'superchat-preset-tools))
                   (let ((tools (superchat-preset-tools preset)))
                     (or (null tools)
                         (and (listp tools)
                              (member "define_tool" tools)))))))
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn)
                  "\n"
                  "If no available tool can do what a task needs, you may write one: "
                  "call define_tool with a name, a description, an argument schema and "
                  "an Emacs Lisp body. It becomes callable on your next step, in this "
                  "same run. The user confirms the code before it is defined and again "
                  "on every call, so keep it small and specific.")))
  turn)

(defun superchat-prompt-hook--generative-ui-reachable-p ()
  "Return non-nil when `render_ui' will be attached to the coming request.

The global allowlist is necessary but not sufficient: an active preset
narrows the request's tool set AFTER the system-prompt hooks run
(`superchat-preset-apply' is called later in the dispatcher), so honour
its `:tools' restriction here.  Advertising a surface the model cannot
reach sends it hunting for workarounds."
  (and (boundp 'superchat-llm-tool-names)
       (or (eq superchat-llm-tool-names 'all)
           (member "render_ui" superchat-llm-tool-names))
       (let ((preset (bound-and-true-p superchat--active-preset)))
         (or (null preset)
             (not (fboundp 'superchat-preset-tools))
             (let ((tools (superchat-preset-tools preset)))
               (cond ((null tools) t)          ; inherit global set
                     ((eq tools 'none) nil)    ; explicitly empty
                     (t (and (listp tools)
                             (member "render_ui" tools)))))))))

(defun superchat-prompt-hook--generative-ui (turn)
  "Describe the opt-in `render_ui' surface when it is active."
  (when (superchat-prompt-hook--generative-ui-reachable-p)
    (let* ((actions
            (if (fboundp 'superchat-genui-action-names)
                (mapconcat #'identity (superchat-genui-action-names) ", ")
              "cancel-agent")))
      (setf (superchat-turn-system-prompt turn)
            (concat (superchat-turn-system-prompt turn)
                    "\n"
                    (format (concat "render_ui is available for an interactive "
                                    "TextUI buffer. If the user asks for a UI or "
                                    "table, call render_ui directly. Do not "
                                    "define_tool or use an agent-written tool "
                                    "to render it. The component is DATA, never "
                                    "code: elements are (:type :text :value STR), "
                                    "(:type :flex :direction :row|:column "
                                    ":children (ELEMENT...)), (:type :grid "
                                    ":columns N :min-column-width N :children "
                                    "(ELEMENT...)), or (:type button :label STR "
                                    ":action (action NAME :key value...)). "
                                    "Registered actions: %s. Example: (:type :flex "
                                    ":direction :column :children "
                                    "((:type :text :value \"Which option?\") "
                                    "(:type button :label \"Ask\" :action "
                                    "(action send-to-chat :text \"Explain Redux\")))). "
                                    "Prefer the >render-ui skill for the full grammar.")
                              actions)))))
  turn)

(defun superchat-prompt-hook--parallel-tool-calls (turn)
  "Add a brief guidance block encouraging parallel read-only tool calls.
This is only injected when `superchat-llm-tools-enabled' is non-nil.
Fewer round-trips = less context resend, which compounds over long chats."
  (when superchat-llm-tools-enabled
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn)
                  "\n"
                  "You can batch INDEPENDENT read-only tool calls "
                  "(read-file, list-files, search-text, find-files, "
                  "lsp-references, lsp-hover, read_buffer) into a single turn.\n"
                  "Tools that depend on each other's output must wait for the "
                  "previous result.")))
  turn)

;; ═══════════════════════════════════════════════════════════
;; Build-prompt hooks (order matters — see add-hook calls below)
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--file-metadata-block "superchat-dispatcher" (file-path))

(defun superchat-prompt-hook--file-inline (turn)
  "Parse #file ref from TURN's clean-input, inline file content into prompt.
Strips the file ref from `turn.clean-input' so downstream hooks
see the plain query.  When no file ref is present, returns turn unchanged."
  (let* ((initial-query (string-trim (or (superchat-turn-clean-input turn) "")))
         (file-path
          (when (string-match superchat--file-ref-regexp initial-query)
            (superchat--normalize-file-path
             (or (match-string 1 initial-query)
                 (match-string 2 initial-query))))))
    (if (null file-path)
        turn
      ;; Strip file ref from clean-input
      (let* ((path-start (match-beginning 0))
             (path-end (match-end 0))
             (text-before (substring initial-query 0 path-start))
             (text-after (substring initial-query path-end))
             (user-query (string-trim (concat (string-trim text-before) " "
                                              (string-trim text-after))))
             (file-readable (and (file-exists-p file-path)
                                 (superchat--textual-file-p file-path)))
             ;; RLM-style: a large file never enters the root context.
             ;; The model gets its shape and reads slices on demand.
             (metadata-only
              (and file-readable
                   (natnump (bound-and-true-p superchat-inline-metadata-threshold))
                   (> (or (file-attribute-size (file-attributes file-path)) 0)
                      superchat-inline-metadata-threshold)))
             (file-content
              (when (and file-readable (not metadata-only))
                (superchat--read-inline-file-content file-path)))
             (inline-context
              (when file-path
                (superchat--add-file-to-context file-path)
                (cond
                 (metadata-only
                  (superchat--file-metadata-block file-path))
                 ((and superchat-inline-file-content
                       (not (and (string-empty-p user-query) file-content)))
                  (superchat--render-inline-context file-path file-content))))))
        ;; When user only supplied a file ref, inject file content as $input
        (when (and (string-empty-p user-query) file-content)
          (setq user-query (format "File: %s\n\n%s" file-path file-content)))
        ;; Warn on missing file
        (when (and file-path (not (file-exists-p file-path)))
          (message "Warning: Referenced file does not exist: %s" file-path))
        ;; Update turn slots
        (setf (superchat-turn-clean-input turn) user-query)
        (when inline-context
          (setf (superchat-turn-prompt turn)
                (concat inline-context
                        (unless (string-empty-p (superchat-turn-prompt turn))
                          "\n\n")
                        (superchat-turn-prompt turn)))))))
  turn)

(defun superchat-prompt-hook--template-substitution (turn)
  "Substitute `$input' and `$lang' in TURN's template, append to prompt.
Uses `turn.clean-input' as the effective user query."
  (let* ((current-lang (or superchat-lang "English"))
         (template (or superchat-general-answer-prompt ""))
         (user-query (string-trim (or (superchat-turn-clean-input turn) "")))
         (processed-template template))
    ;; $lang substitution
    (when (string-match-p (regexp-quote "$lang") processed-template)
      (setq processed-template
            (replace-regexp-in-string (regexp-quote "$lang")
                                      current-lang
                                      processed-template
                                      t t)))
    ;; $input substitution
    (let ((base-prompt
           (if (string-match-p (regexp-quote "$input") processed-template)
               (replace-regexp-in-string (regexp-quote "$input")
                                         user-query
                                         processed-template
                                         t t)
             (concat processed-template "\n\nUser question: " user-query))))
      (setf (superchat-turn-prompt turn)
            (concat (superchat-turn-prompt turn)
                    (unless (string-empty-p (superchat-turn-prompt turn))
                      "\n\n")
                    base-prompt))))
  turn)

(defun superchat-prompt-hook--memory-context (turn)
  "Prepend formatted memories from TURN's retrieved-memories to prompt."
  (when-let* ((mems (superchat-turn-retrieved-memories turn))
              (formatted (superchat--format-retrieved-memories mems))
              ((not (string-empty-p formatted))))
    (setf (superchat-turn-prompt turn)
          (concat formatted "\n\n" (superchat-turn-prompt turn))))
  turn)

(defun superchat-prompt-hook--experience-context (turn)
  "Prepend an explicitly requested, satisfied Experience View to TURN."
  (when-let* ((selection
               (plist-get (superchat-turn-state turn) :experience-selection)))
    (unless (and (listp selection) (proper-list-p selection)
                 (cl-evenp (length selection)))
      (error "Experience selection must be a plist"))
    (let (seen)
      (cl-loop for (key _value) on selection by #'cddr
               unless (memq key '(:session-ids :scope :context))
               do (error "Unknown Experience selection key: %s" key)
               when (memq key seen)
               do (error "Duplicate Experience selection key: %s" key)
               do (push key seen))
      (dolist (key '(:session-ids :scope :context))
        (unless (plist-member selection key)
          (error "Experience selection requires %s" key))))
    (unless (fboundp 'superchat-experience-assemble-view)
      (require 'superchat-experience))
    (let* ((view
            (superchat-experience-assemble-view
             (plist-get selection :session-ids)
             (plist-get selection :scope)
             (plist-get selection :context)))
           (prompt (and (eq 'satisfied (plist-get view :status))
                        (plist-get view :prompt))))
      (setf (superchat-turn-state turn)
            (plist-put (superchat-turn-state turn) :experience-view view))
      (when (and (stringp prompt) (not (string-empty-p prompt)))
        (setf (superchat-turn-prompt turn)
              (concat prompt "\n\n" (superchat-turn-prompt turn))))))
  turn)

(defun superchat-prompt-hook--conversation-history (turn)
  "Prepend a bounded Tape-backed Context View to TURN's prompt.
The complete envelope remains attached to TURN state for audit."
  (let* ((session-id (superchat-turn-session-id turn))
         (envelope
          (when (and (stringp session-id)
                     (not (string-empty-p session-id)))
            (superchat-view-assemble
             session-id
             :max-entries
             (and (boundp 'superchat-context-message-count)
                  superchat-context-message-count)
             :max-chars
             (and (boundp 'superchat-context-max-chars)
                  superchat-context-max-chars))))
         (status (and envelope (plist-get envelope :status)))
         (context (and (memq status '(satisfied partial))
                       (plist-get envelope :prompt))))
    (when envelope
      (when-let* ((experience
                   (plist-get (superchat-turn-state turn) :experience-view)))
        (setq envelope
              (plist-put envelope :experience-view (copy-tree experience t))))
      (setf (superchat-turn-state turn)
            (plist-put (superchat-turn-state turn)
                       :context-envelope envelope)))
    (when (and (stringp context) (not (string-empty-p context)))
      (setf (superchat-turn-prompt turn)
            (concat context "\n\n" (superchat-turn-prompt turn)))))
  turn)

;; ═══════════════════════════════════════════════════════════
;; Registration — runs at load time
;; ═══════════════════════════════════════════════════════════

(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--language-instruction)
(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--parallel-tool-calls)
(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--self-extension)
(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--generative-ui)

;; build-prompt order matters — later hooks see earlier hooks' work
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--file-inline)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--template-substitution)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--memory-context)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--conversation-history)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--experience-context)

(provide 'superchat-prompt-hooks)
;;; superchat-prompt-hooks.el ends here
