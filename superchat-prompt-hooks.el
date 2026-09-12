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
(require 'superchat-plugin)
(require 'superchat-core)
(require 'superchat-llm)
(require 'superchat-tape-view)
(require 'superchat-prompt)

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
(defvar superchat--current-context-files)

(declare-function superchat-genui-action-names "superchat-genui" ())
(declare-function superchat-experience-assemble-view
                  "superchat-experience" (session-ids scope context))

(declare-function superchat--normalize-file-path "superchat-dispatcher" (file-path))
(declare-function superchat--textual-file-p "superchat-dispatcher" (path))
(declare-function superchat--read-inline-file-content "superchat-dispatcher" (file-path))
(declare-function superchat--render-inline-context "superchat-dispatcher" (file-path content))
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--image-attachment-info "superchat-llm" (file))
(declare-function superchat--format-retrieved-memories "superchat-dispatcher" (memories))
(declare-function superchat--llm-context-budget-chars
                  "superchat-llm"
                  (&optional system-prompt turn-prompt target-model))

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
                  "Operate available tools yourself; never tell the user to "
                  "call one. You can batch INDEPENDENT read-only tool calls "
                  "into a single turn.\n"
                  "Tools that depend on each other's output must wait for the "
                  "previous result.")))
  turn)

;; ═══════════════════════════════════════════════════════════
;; Build-prompt hooks (order matters — see add-hook calls below)
;; ═══════════════════════════════════════════════════════════

(declare-function superchat--file-metadata-block "superchat-dispatcher" (file-path))
(declare-function superchat--extract-file-path "superchat-dispatcher" (input))

(defun superchat-prompt--substitute-template (user-query)
  "Return USER-QUERY run through `superchat-general-answer-prompt'
substitution: `$lang' becomes `superchat-lang' (or \"English\"); `$input'
becomes USER-QUERY when the template names it, else USER-QUERY is
appended as \"User question: …\".  Pure — no Turn is read or written."
  (let* ((current-lang (or superchat-lang "English"))
         (template (or superchat-general-answer-prompt ""))
         (processed-template template))
    ;; $lang substitution
    (when (string-match-p (regexp-quote "$lang") processed-template)
      (setq processed-template
            (replace-regexp-in-string (regexp-quote "$lang")
                                      current-lang
                                      processed-template
                                      t t)))
    ;; $input substitution
    (if (string-match-p (regexp-quote "$input") processed-template)
        (replace-regexp-in-string (regexp-quote "$input")
                                  user-query
                                  processed-template
                                  t t)
      (concat processed-template "\n\nUser question: " user-query))))

;; ═══════════════════════════════════════════════════════════
;; Registration — runs at load time
;; ═══════════════════════════════════════════════════════════
;;
;; The four system-prompt hooks above are folded into the `system-instructions'
;; collector; the old hook lists are gone (v2).

(defun superchat-prompt-collector--plan (env _state _tools)
  "Return the session's approved plan as a `plan'-slot fragment (chat only)."
  (when (and (eq (plist-get env :surface) 'chat)
             (boundp 'superchat--active-plan)
             superchat--active-plan
             (fboundp 'superchat-plan--context-string))
    (list (superchat-prompt-fragment
           :owner 'chat :id "plan" :slot 'plan
           :text (superchat-plan--context-string superchat--active-plan)
           :priority 0 :budget-class 'hard))))

;; ═══════════════════════════════════════════════════════════
;; Prompt fragment collectors (`collect-context' stage, contract §4.3)
;; ═══════════════════════════════════════════════════════════

(defconst superchat-prompt--system-instruction-specs
  (list (list "language-instruction"
              #'superchat-prompt-hook--language-instruction 40)
        (list "parallel-tool-calls"
              #'superchat-prompt-hook--parallel-tool-calls 30)
        (list "self-extension"
              #'superchat-prompt-hook--self-extension 20)
        (list "generative-ui"
              #'superchat-prompt-hook--generative-ui 10))
  "System-prompt sub-blocks folded into the `system-instructions' collector.
Each entry is (ID HOOK-FUNCTION PRIORITY); PRIORITY orders the resulting
`:slot system' fragments so their joined text matches the order the
retired `add-hook' chain produced: language instruction, the
parallel-tool-calls blurb, the `define_tool' blurb, the `render_ui' blurb.")

(defun superchat-prompt-collector--system-instructions (env _state _tools)
  "Return one `:slot system' fragment per non-empty block in
`superchat-prompt--system-instruction-specs', computed by running each
block's hook on its own empty scratch turn — none of the four read
anything from the turn itself, only from global tool/preset state.
Chat and inline-ask only: producers that prepare their own prompt
\(subagent, workflow, skills, compaction) never received these blocks."
  (let (fragments)
    (dolist (spec (and (memq (plist-get env :surface) '(chat inline-ask))
                       superchat-prompt--system-instruction-specs))
      (cl-destructuring-bind (id fn priority) spec
        (let* ((scratch (superchat-turn-new ""))
               (result (funcall fn scratch))
               (text (string-trim (or (superchat-turn-system-prompt result) ""))))
          (unless (string-empty-p text)
            (push (superchat-prompt-fragment
                   :owner 'core :id id :slot 'system :text text
                   :priority priority :budget-class 'hard)
                  fragments)))))
    (nreverse fragments)))

(defun superchat-prompt-collector--file-inline (env _state _tools)
  "Return the `#file' attachment fragment, if ENV's Turn referenced one.
`superchat-prompt-collect' already stripped the ref from clean-input and
recorded the parsed path as ENV's `:file-ref'; this collector owns only
the read, the user-visible `message' calls, and the `:attachment-notices'
turn-state write the retired `--file-inline' hook made.  A no-op,
including on all its side effects, when `:file-ref' is nil — which is
also true the second time `superchat-prompt-collect' runs on the same
Turn (the dispatcher's template-path re-collection), matching what the
retired hook did once the ref was already stripped from clean-input."
  (let* ((context (plist-get env :context))
         (turn (plist-get context :chat-turn))
         (file-path (plist-get context :chat-file-ref)))
    (when file-path
      (let* ((file-exists (file-exists-p file-path))
             (image-info (and file-exists
                              (fboundp 'superchat--image-attachment-info)
                              (ignore-errors
                                (superchat--image-attachment-info file-path))))
             (textual-file (and file-exists
                                (superchat--textual-file-p file-path)))
             (metadata-only
              (and textual-file
                   (natnump (bound-and-true-p superchat-inline-metadata-threshold))
                   (> (or (file-attribute-size (file-attributes file-path)) 0)
                      superchat-inline-metadata-threshold)))
             (file-content
              (when (and textual-file (not metadata-only)
                         superchat-inline-file-content)
                (superchat--read-inline-file-content file-path)))
             (inline-context
              (cond
               (metadata-only (superchat--file-metadata-block file-path))
               ((and (stringp file-content) (not (string-empty-p file-content)))
                (superchat--render-inline-context file-path file-content)))))
        ;; `superchat--add-file-to-context' (superchat.el) localizes
        ;; `superchat--current-context-files' to the current View itself
        ;; now, so this collector needs no guard of its own.
        (superchat--add-file-to-context file-path)
        (cond
         ((not file-exists)
          (message "Warning: Referenced file does not exist: %s" file-path))
         (image-info
          (message "superchat: Image %s will be attached to the request"
                   file-path))
         ((not textual-file)
          (message "superchat: File %s is not text or a supported image; it will not be included in context"
                   file-path))
         (metadata-only
          (message "superchat: File %s metadata will be included; content is not inlined"
                   file-path)
          (setf (superchat-turn-state turn)
                (plist-put
                 (superchat-turn-state turn)
                 :attachment-notices
                 (append
                  (plist-get (superchat-turn-state turn) :attachment-notices)
                  (list
                   (format
                    "附件 %s 仅发送 metadata，文件正文未全文注入；请用 #路径:START-END 缩小范围。"
                    file-path))))))
         ((and (stringp file-content) (not (string-empty-p file-content)))
          nil) ; `superchat--render-inline-context' already messaged
         ((not superchat-inline-file-content)
          (message "superchat: File %s was not inlined (inline file content is disabled)"
                   file-path))
         (t
          (message "superchat: File %s was not inlined (empty or unreadable)"
                   file-path)))
        (when inline-context
          (list (superchat-prompt-fragment
                 :owner 'chat :id (format "file:%s" file-path) :slot 'attachment
                 :text inline-context :priority 0 :budget-class 'hard)))))))

(defun superchat-prompt-collector--context-files (env _state _tools)
  "Return one soft `:attachment' fragment per textual dired/embark file
in `superchat--current-context-files', except the path ENV's current
Turn explicitly references via `#file' — the `file-inline' collector's
hard fragment owns that path for this Turn, so re-emitting it here would
send its body twice.  The path stays in `superchat--current-context-files'
for later Turns that do not explicitly reference it.  Images are excluded
by `superchat--textual-file-p' and stay on the `:context-files' transport
`superchat--execute-llm-query' already builds; only text content was
being silently dropped (N5) before this collector existed."
  (let ((current-ref (plist-get (plist-get env :context) :chat-file-ref))
        fragments)
    (dolist (path superchat--current-context-files)
      (when (and (superchat--textual-file-p path)
                 (not (equal path current-ref)))
        (let ((content (superchat--read-inline-file-content path)))
          (when (and (stringp content) (not (string-empty-p content)))
            (push (superchat-prompt-fragment
                   :owner 'chat :id (format "ctx:%s" path) :slot 'attachment
                   :text (superchat--render-inline-context path content)
                   :priority 0 :budget-class 'soft)
                  fragments)))))
    (nreverse fragments)))

(defun superchat-prompt-collector--memory (env _state _tools)
  "Return the retrieved-memories fragment, when ENV's Turn has any."
  (let* ((turn (plist-get (plist-get env :context) :chat-turn))
         (mems (superchat-turn-retrieved-memories turn))
         (formatted (and mems (superchat--format-retrieved-memories mems))))
    (when (and (stringp formatted) (not (string-empty-p formatted)))
      (list (superchat-prompt-fragment
             :owner 'chat :id "memory" :slot 'memory :text formatted
             :priority 0 :budget-class 'soft)))))

(defun superchat-prompt-collector--experience (env _state _tools)
  "Return an explicitly requested, satisfied Experience View fragment.
Still writes `:experience-view' into Turn state so the `history'
collector can fold it into the Context envelope."
  (let* ((turn (plist-get (plist-get env :context) :chat-turn))
         (selection (plist-get (superchat-turn-state turn) :experience-selection)))
    (when selection
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
      (let* ((view (superchat-experience-assemble-view
                    (plist-get selection :session-ids)
                    (plist-get selection :scope)
                    (plist-get selection :context)))
             (prompt (and (eq 'satisfied (plist-get view :status))
                          (plist-get view :prompt))))
        (setf (superchat-turn-state turn)
              (plist-put (superchat-turn-state turn) :experience-view view))
        (when (and (stringp prompt) (not (string-empty-p prompt)))
          (list (superchat-prompt-fragment
                 :owner 'chat :id "experience" :slot 'experience :text prompt
                 :priority 0 :budget-class 'soft)))))))

(defun superchat-prompt-collector--history (env _state _tools)
  "Return the bounded Context View fragment, refit at assemble time.
Stores `:context-envelope'/`:context-max-chars' in Turn state twice: once
here with a first-pass budget estimate, so Runtime's overflow recovery
always finds one even when a fragment ordered before `history' exhausts
the budget and `:refit' is never called; again inside `:refit' with the
exact remaining budget `superchat-prompt-assemble' computed, which is
strictly more accurate and overwrites the first.  The Session's
replaced-row ids come from the single Tape View supersession Projection
and are computed once here for both assemblies.  When that projection
signals, this Turn gets no history fragment at all -- the current input
still goes out -- and a visible notice is recorded: history is never
assembled without its exclusions."
  (let* ((turn (or (plist-get (plist-get env :context) :chat-turn)
                   ;; S18b: an inline ask has no chat Turn; its history is
                   ;; the inline project Session's own rows, so a scratch
                   ;; Turn keyed by the envelope's `:session-id' carries the
                   ;; bookkeeping instead.  Other surfaces without a Turn
                   ;; (rewrite, skills, compaction) get no history.
                   (and (eq (plist-get env :surface) 'inline-ask)
                        (stringp (plist-get env :session-id))
                        (superchat-turn-new (or (plist-get env :content) "")
                                            (plist-get env :session-id)))))
         (session-id (and turn (superchat-turn-session-id turn)))
         (history-usable-p t)
         (excluded-storage-ids
          (when (and (stringp session-id)
                     (not (string-empty-p session-id)))
            (condition-case err
                (superchat-view-superseded-storage-ids session-id)
              (error
               (setq history-usable-p nil)
               (message
                "Superchat: replaced-turn index unreadable; history omitted for this turn (%s)"
                (error-message-string err))
               nil)))))
    (when (and history-usable-p
               (stringp session-id)
               (not (string-empty-p session-id)))
      (cl-flet
          ((assemble-and-store
            (max-chars)
            ;; `superchat-context-max-chars', when the user set it, still
            ;; means "cap the history budget" only (its documented
            ;; meaning) — `superchat-core-prepare-turn'/
            ;; `superchat--execute-llm-query' deliberately no longer feed
            ;; it into the whole-prompt budget, so MAX-CHARS reaching this
            ;; function (whether the eager initial estimate or `:refit''s
            ;; real remaining budget) can be far larger than the user's
            ;; configured history limit; clamp it here, the one place both
            ;; callers go through.
            (let* ((max-chars
                    (if (integerp superchat-context-max-chars)
                        (min max-chars superchat-context-max-chars)
                      max-chars))
                   (envelope
                    (superchat-view-assemble
                     session-id
                     :max-entries (and (boundp 'superchat-context-message-count)
                                        superchat-context-message-count)
                     :max-chars max-chars
                     :excluded-storage-ids excluded-storage-ids
                     :expanded-anchor-ids
                     (and (boundp 'superchat--expanded-compaction-anchor-ids)
                          (vconcat superchat--expanded-compaction-anchor-ids)))))
              (when-let* ((experience
                           (plist-get (superchat-turn-state turn) :experience-view)))
                (setq envelope (plist-put envelope :experience-view
                                          (copy-tree experience t))))
              (setf (superchat-turn-state turn)
                    (plist-put (superchat-turn-state turn) :context-envelope envelope))
              (setf (superchat-turn-state turn)
                    (plist-put (superchat-turn-state turn) :context-max-chars max-chars))
              (when-let* ((notice (plist-get envelope :notice)))
                (message "%s" notice))
              envelope)))
        (let* ((initial-envelope
                (assemble-and-store
                 (superchat--llm-context-budget-chars
                  "" "" (superchat-turn-target-model turn))))
               (initial-status (plist-get initial-envelope :status))
               (initial-text (if (memq initial-status '(satisfied partial))
                                  (or (plist-get initial-envelope :prompt) "")
                                "")))
          (list (superchat-prompt-fragment
                 :owner 'chat :id "history" :slot 'history :text initial-text
                 :priority 0 :budget-class 'soft
                 :refit
                 (lambda (chars)
                   (let* ((envelope (assemble-and-store chars))
                          (status (plist-get envelope :status)))
                     (if (memq status '(satisfied partial))
                         (or (plist-get envelope :prompt) "")
                       ""))))))))))

(defun superchat-prompt-collector--chat-input (env _state _tools)
  "Return the chat user message as the required `content' fragment.
Only for an Envelope carrying `:context :chat-turn' (the chat surface).
The text is `:chat-prompt-override' when the dispatcher fixed one (a
prompt-type skill's combined text), else the ref-free clean input run
through `superchat-general-answer-prompt' -- or through `:chat-template'
\(an active `/command' template) when the Envelope names one.  Returning
the id \"content\" tells the driver to leave out its own raw content
fragment (spec §1.4)."
  (let* ((context (plist-get env :context))
         (turn (plist-get context :chat-turn)))
    (when turn
      (let* ((override (plist-get context :chat-prompt-override))
             (template (plist-get context :chat-template))
             (clean (string-trim (or (superchat-turn-clean-input turn) "")))
             (text (cond
                    ((and (stringp override) (not (string-empty-p override))) override)
                    (t (let ((superchat-general-answer-prompt
                              (or template superchat-general-answer-prompt)))
                         (superchat-prompt--substitute-template clean))))))
        (list (superchat-prompt-fragment
               :owner 'chat :id "content" :slot 'template :text text
               :priority 0 :budget-class 'hard :required t))))))

(defun superchat-prompt-collector--prepared (env _state _tools)
  "Return the fragments of a producer that prepared its whole user message.
Subagent, workflow, skills-match and compaction hand the driver
`:context (:prepared-prompt TEXT :prepared-system TEXT)'; TEXT becomes the
required `content' fragment (replacing the driver's raw one) and the
system text a `system' fragment unless the Envelope already carries its
own `:system-prompt'.  This is the plugin-shaped successor of the driver's
former prepared-prompt bypass (migration step 3)."
  (let* ((context (plist-get env :context))
         (prompt (plist-get context :prepared-prompt))
         (system (plist-get context :prepared-system))
         (envelope-system (plist-get env :system-prompt)))
    (when (and (stringp prompt) (not (string-empty-p prompt)))
      (append
       (list (superchat-prompt-fragment
              :owner 'core :id "content" :slot 'template :text prompt
              :priority 0 :budget-class 'hard :required t))
       (when (and (stringp system) (not (string-empty-p system))
                  (not (and (stringp envelope-system)
                            (not (string-empty-p envelope-system)))))
         (list (superchat-prompt-fragment
                :owner 'core :id "prepared-system" :slot 'system :text system
                :priority 0 :budget-class 'hard)))))))

;; ── Plugin: the default collectors ──
;; Registration order below is reversed at call time ("later registration
;; runs first"): experience, history, memory, context-files, file-inline,
;; system-instructions, chat-input, plan, prepared.

(superchat-define-plugin chat-collectors
  :doc "The default collect-context contributors: chat input, system
instructions, #file and context files, memory, history, experience, plan,
and a producer's prepared text."
  :apply
  (lambda (_scope _config)
    (superchat-stage-add 'collect-context 'prepared
                          #'superchat-prompt-collector--prepared :owner 'core)
    (superchat-stage-add 'collect-context 'plan
                          #'superchat-prompt-collector--plan :owner 'chat)
    (superchat-stage-add 'collect-context 'chat-input
                          #'superchat-prompt-collector--chat-input :owner 'chat)
    (superchat-stage-add 'collect-context 'system-instructions
                          #'superchat-prompt-collector--system-instructions :owner 'chat)
    (superchat-stage-add 'collect-context 'file-inline
                          #'superchat-prompt-collector--file-inline :owner 'chat)
    (superchat-stage-add 'collect-context 'context-files
                          #'superchat-prompt-collector--context-files :owner 'chat)
    (superchat-stage-add 'collect-context 'memory
                          #'superchat-prompt-collector--memory :owner 'chat)
    (superchat-stage-add 'collect-context 'history
                          #'superchat-prompt-collector--history :owner 'chat)
    (superchat-stage-add 'collect-context 'experience
                          #'superchat-prompt-collector--experience :owner 'chat)))

(provide 'superchat-prompt-hooks)
;;; superchat-prompt-hooks.el ends here
