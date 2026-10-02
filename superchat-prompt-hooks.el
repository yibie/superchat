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
;; Owned by superchat.el: the View's associated editing source, and the
;; data-only origin helpers the origin collector renders.
(defvar superchat--origin-buffer)
(declare-function superchat--origin-snapshot "superchat" (source))
(declare-function superchat--origin-selection "superchat" (snapshot))
(declare-function superchat--origin-render "superchat" (snapshot))
(declare-function superchat--origin-selection-render "superchat" (snapshot))

(declare-function superchat-genui-action-names "superchat-genui" ())
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

(declare-function superchat-sandbox-runtime-context-text "superchat-sandbox" (session-id))

(defun superchat-prompt-collector--permission-context (env _state tools)
  "Return the current permission policy as the last user-message fragment.
Only for a Session whose request offers tools.
The text is the same for every request while the policy is unchanged, and
the next request after a change states the new policy."
  (when (and tools (not (eq tools 'none))
             (stringp (plist-get env :session-id))
             (fboundp 'superchat-sandbox-runtime-context-text))
    (list (superchat-prompt-fragment
           :owner 'core :id "permission-context" :slot 'continuation
           :text (superchat-sandbox-runtime-context-text (plist-get env :session-id))
           :priority 0 :budget-class 'hard))))

(defconst superchat-prompt--permission-context-regexp
  "^Current runtime context\\. This snapshot supersedes earlier runtime-context snapshots\\.\n\n[^\n]*\n\n[^\n]*"
  "Matches one permission-context snapshot: its heading and two paragraphs.")

(defun superchat-prompt--refresh-permission-context (candidate next)
  "Request middleware: restate the current policy in CANDIDATE's prompt.
The snapshot is collected when a Turn is submitted, and the same prompt is
sent again after every tool round.  A level changed in between -- during an
approval, say -- must be what the next request states, so the last snapshot
in the prompt is rewritten from the Session's policy now.  Nothing else in
the prompt is collected again."
  (let* ((context (and (boundp 'superchat--tool-execution-context)
                       (listp superchat--tool-execution-context)
                       superchat--tool-execution-context))
         (session-id (plist-get context :session-id))
         (prompt (plist-get candidate :prompt)))
    (when (and (stringp session-id) (stringp prompt)
               (fboundp 'superchat-sandbox-runtime-context-text))
      (let ((start nil) (end nil) (from 0))
        (while (string-match superchat-prompt--permission-context-regexp prompt from)
          (setq start (match-beginning 0) end (match-end 0) from (match-end 0)))
        (when start
          (setq candidate
                (plist-put (copy-sequence candidate) :prompt
                           (concat (substring prompt 0 start)
                                   (superchat-sandbox-runtime-context-text session-id)
                                   (substring prompt end)))))))
    (funcall next candidate)))

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
;; Prompt fragment collectors (the `collect-context' Event, contract §4.3)
;; ═══════════════════════════════════════════════════════════

;;;; The Emacs operating note ---------------------------------------------------
;; docs/decisions/implemented/2026-09-20-emacs-operating-note.md
;; Written from mistakes models were observed to repeat while configuring Emacs
;; under buffer-first mutation.  It is text: it grants and restricts nothing.
;; That a real model does better for it has NOT been verified.

(defcustom superchat-emacs-operating-note t
  "Whether requests that can change Emacs carry a short operating note.
t sends the built-in `superchat-emacs-operating-note-text'; a string sends
that string instead; nil sends nothing.  The note is part of the system text
and appears only when `eval_elisp' or `edit_buffer' is on the request's final
tool surface.  It costs about 310 tokens on each such request."
  :type '(choice (const :tag "Built-in note" t)
                 (const :tag "No note" nil)
                 (string :tag "My own note"))
  :group 'superchat)

(defconst superchat-emacs-operating-note-text
  "Working in the user's Emacs:
- Ask the running Emacs, do not guess: `user-init-file' is the loaded init file. Check symbols with fboundp, commandp, boundp; never run a command to test it. fundamental-mode has no keymap variable.
- Keys: write (kbd \"C-c t h\"), never a bare string. A number from lookup-key means that map has no binding for it; key-binding in the target buffer is the effective binding.
- Change a file with open_file, then edit_buffer; until the user saves, (load FILE) reads the old disk text. To make it live, evaluate only the forms you changed, not the whole buffer (no eval-buffer).
- eval_elisp takes one expression; wrap several in progn.
- To undo, restore what you observed (not \"unbound\"), only what this task changed.
- The origin buffer is where the user is, not the task's limit. Their agenda, feeds, notes, files and repositories live in this Emacs: ask it with eval_elisp (the packages and variables they configured) before saying you cannot reach something.
- Never change global settings, other buffers or Superchat's windows to get past an obstacle; say what blocks you.
- Report what you checked: the buffer you edited and whether it is unsaved (buffer-modified-p); saving is the user's. You did not restart Emacs; claim no restart."
  "The built-in Emacs operating note: at most 1280 characters.
It must never tell the model to save or persist anything.")

(defconst superchat-prompt--operating-note-tools '("eval_elisp" "edit_buffer")
  "The note is rendered when any of these is on the final tool surface.")

(defun superchat-prompt-hook--emacs-operating-note (turn)
  "Add the Emacs operating note chosen by `superchat-emacs-operating-note'."
  (when-let* ((text (cond ((stringp superchat-emacs-operating-note)
                           superchat-emacs-operating-note)
                          (superchat-emacs-operating-note
                           superchat-emacs-operating-note-text))))
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn) "\n" text)))
  ;; Like every block function: the Turn is what is returned.
  turn)

;; docs/decisions/accepted/2026-09-20-reference-reading-on-demand.md
(defconst superchat-prompt--reference-index-tools '("read_reference")
  "The index is rendered only when the reading tool is on the final surface.
It never advertises an entry the request does not have.")

(declare-function superchat-reference-index-text "superchat-reference")

(defun superchat-prompt-hook--reference-index (turn)
  "Add the short index of the bundled reference pages."
  (when (and (require 'superchat-reference nil t)
             (fboundp 'superchat-reference-index-text))
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn) "\n"
                  (superchat-reference-index-text))))
  turn)

;; docs/decisions/accepted/2026-09-27-skills-the-model-finds-itself.md
(defconst superchat-prompt--skill-catalog-tools '("read_file")
  "The skill catalog is rendered only when the model can read a skill.")

(declare-function superchat-skills-catalog-text "superchat-skills")

(defun superchat-prompt-hook--skill-catalog (turn)
  "Add the skill catalog: the skills the model may read with read_file."
  (when-let* ((text (and (require 'superchat-skills nil t)
                         (fboundp 'superchat-skills-catalog-text)
                         (ignore-errors (superchat-skills-catalog-text)))))
    (setf (superchat-turn-system-prompt turn)
          (concat (superchat-turn-system-prompt turn) "\n" text)))
  turn)

(defun superchat-prompt--block-tools (fn)
  "Return the tool names block function FN is about, or nil.
A block with tool names is rendered only when one of them is on the final tool
surface; nil is an unconditional block."
  (cond ((eq fn #'superchat-prompt-hook--self-extension) '("define_tool"))
        ((eq fn #'superchat-prompt-hook--generative-ui) '("render_ui"))
        ((eq fn #'superchat-prompt-hook--emacs-operating-note)
         superchat-prompt--operating-note-tools)
        ((eq fn #'superchat-prompt-hook--reference-index)
         superchat-prompt--reference-index-tools)
        ((eq fn #'superchat-prompt-hook--skill-catalog)
         superchat-prompt--skill-catalog-tools)))

(defconst superchat-prompt--system-instruction-specs
  (list (list "language-instruction"
              #'superchat-prompt-hook--language-instruction 40)
        (list "parallel-tool-calls"
              #'superchat-prompt-hook--parallel-tool-calls 30)
        (list "self-extension"
              #'superchat-prompt-hook--self-extension 20)
        (list "generative-ui"
              #'superchat-prompt-hook--generative-ui 10)
        (list "emacs-operating-note"
              #'superchat-prompt-hook--emacs-operating-note 5)
        (list "reference-index"
              #'superchat-prompt-hook--reference-index 4)
        (list "skill-catalog"
              #'superchat-prompt-hook--skill-catalog 3))
  "System-prompt sub-blocks folded into the `system-instructions' collector.
Each entry is (ID HOOK-FUNCTION PRIORITY); PRIORITY orders the resulting
`:slot system' fragments so their joined text matches the order the
retired `add-hook' chain produced: language instruction, the
parallel-tool-calls blurb, the `define_tool' blurb, the `render_ui' blurb.")

(defun superchat-prompt-collector--system-instructions (env _state tools)
  "Return one `:slot system' fragment per non-empty block in
`superchat-prompt--system-instruction-specs', computed by running each
block's hook on its own empty scratch turn — none of the four read
anything from the turn itself, only from global tool/preset state.
Chat, inline-ask and inline-act only (inline-act was left out when it was
added; its record gives it the chat window's operating note and tool text):
producers that prepare their own prompt \(subagent, workflow, skills,
compaction) never received these blocks."
  (let (fragments)
    (dolist (spec (and (memq (plist-get env :surface) '(chat inline-ask inline-act))
                       superchat-prompt--system-instruction-specs))
      (cl-destructuring-bind (id fn priority) spec
        (let* ((scratch (superchat-turn-new ""))
               (result (funcall fn scratch))
               (text (string-trim (or (superchat-turn-system-prompt result) ""))))
          (unless (or (string-empty-p text)
                      ;; The operating note is about tools that change Emacs:
                      ;; a request whose decided tool names hold none of them
                      ;; -- an explicit list without them, or `none' -- does
                      ;; not get it.  nil is "not decided yet" (the current-
                      ;; buffer entry collects before its tools are chosen),
                      ;; which means the default surface, so it does.  A later
                      ;; rewrite of the tools is handled by `:render-tools'.
                      ;; The reference index follows the same rule for the
                      ;; reading tool.
                      ;; "Not decided yet" means the user's tool list, which is
                      ;; what the default surface is drawn from -- not the
                      ;; built-in default: a user list without the tool gets no
                      ;; block that talks about it.
                      (let ((decided (or tools
                                         (bound-and-true-p superchat-llm-tool-names))))
                        (and (memq fn (list #'superchat-prompt-hook--emacs-operating-note
                                            #'superchat-prompt-hook--reference-index
                                            #'superchat-prompt-hook--skill-catalog))
                             decided
                             (not (and (listp decided)
                                       (cl-intersection
                                        (superchat-prompt--block-tools fn) decided
                                        :test #'equal))))))
            (push (let ((fragment
                         (superchat-prompt-fragment
                          :owner 'core :id id :slot 'system :text text
                          :priority priority :budget-class 'hard))
                        ;; The block is rendered only when one of these is on
                        ;; the final tool surface.
                        (tool-names (superchat-prompt--block-tools fn)))
                    (if tool-names
                        (plist-put fragment :render-tools
                                   (lambda (names)
                                     (if (cl-intersection tool-names names :test #'equal)
                                         text
                                       "")))
                      fragment))
                  fragments)))))
    (nreverse fragments)))

(declare-function superchat-workspace-rules-text "superchat-workspace" (root))

(defun superchat-prompt-collector--workspace-rules (env _state _tools)
  "Return the rules of the Workspace ENV's Session is bound to, as system text.
The root comes from the Session's own Tape (`workspace.bound'), so the rules
follow the conversation, not the buffer that happens to be current.  Chat,
inline-ask and inline-act only, like the other system blocks.  Nothing for a
Session with no Workspace root (a temporary conversation) or no rules file."
  (let* ((session-id (plist-get env :session-id))
         (root (and (memq (plist-get env :surface) '(chat inline-ask inline-act))
                    (stringp session-id)
                    (plist-get (superchat-view-session-configuration session-id)
                               :workspace-root)))
         (text (and (stringp root)
                    (fboundp 'superchat-workspace-rules-text)
                    (superchat-workspace-rules-text root))))
    (when text
      (list (superchat-prompt-fragment
             :owner 'core :id "workspace-rules" :slot 'system :text text
             :priority 2 :budget-class 'hard)))))

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
         ;; A non-chat Envelope legitimately has no chat Turn.  Skip it
         ;; instead of reading a slot from nil; a present but wrongly typed
         ;; `:chat-turn' still signals from `superchat-turn-retrieved-memories'.
         (mems (and turn (superchat-turn-retrieved-memories turn)))
         (formatted (and mems (superchat--format-retrieved-memories mems))))
    (when (and (stringp formatted) (not (string-empty-p formatted)))
      (list (superchat-prompt-fragment
             :owner 'chat :id "memory" :slot 'memory :text formatted
             :priority 0 :budget-class 'soft)))))

(defun superchat-prompt--outside-thread (events thread)
  "Return the storage ids among EVENTS an in-place follow-up leaves out.
THREAD is its overlay's Runs.  It keeps their rows and the notes of what the
user saved or discarded, after the thread began, of a buffer those Runs
edited (M10 record: the follow-up carries its own exchange and those notes)."
  (let* ((first (cl-some (lambda (event)
                           (and (member (plist-get event :run-id) thread)
                                (plist-get event :storage-id)))
                         events))
         (edited (delq nil
                       (mapcar (lambda (event)
                                 (and (member (plist-get event :run-id) thread)
                                      (equal "tool.completed" (plist-get event :type))
                                      (plist-get (plist-get event :payload) :buffer)))
                               events))))
    (delq nil
          (mapcar
           (lambda (event)
             (let ((source (plist-get (plist-get event :payload) :source)))
               (unless (or (member (plist-get event :run-id) thread)
                           (and (equal "attachment.added" (plist-get event :type))
                                (equal "user-save" (format "%s" (plist-get source :kind)))
                                (member (plist-get source :buffer) edited)
                                first (integerp (plist-get event :storage-id))
                                (> (plist-get event :storage-id) first)))
                 (plist-get event :storage-id))))
           events))))

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
  (let* ((context (plist-get env :context))
         ;; An in-place request (M10 record, amended 2026-09-25: "1d") carries
         ;; no earlier turn of the conversation it joins; a follow-up in the
         ;; same overlay carries that overlay's own earlier Runs, and the
         ;; in-place code passes the Turn and those Runs only then.  Other
         ;; surfaces without a Turn (rewrite, skills, compaction) get none.
         (thread (plist-get context :inline-thread-runs))
         (turn (or (plist-get context :chat-turn)
                   (and thread (plist-get context :inline-turn))))
         (session-id (and turn (superchat-turn-session-id turn)))
         (history-usable-p t)
         (excluded-storage-ids
          (when (and (stringp session-id)
                     (not (string-empty-p session-id)))
            (condition-case err
                (append
                 (superchat-view-superseded-storage-ids session-id)
                 (and thread
                      (superchat-prompt--outside-thread
                       (superchat-tape-replay-events session-id) thread)))
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

(defun superchat-prompt-collector--origin (env _state _tools)
  "Return the request's Emacs origin: dynamic facts and the explicit selection.

Two fragments at most, in a fixed order:

- the facts about the buffer this request speaks about (name, file, major
  mode, point) into the `context' slot, `hard';
- the text the user had explicitly selected into the `attachment' slot,
  `hard' and `required'.

Both are ordinary budgeted user-message data: neither is an instruction and
neither may be moved into a system slot to dodge the character budget.  A
request with no source contributes nothing at all -- that is normal data, not
an error.  A source that exists but yields no usable facts is a collection
error the Event layer records, which is why the render is allowed to say so
rather than silently dropping the facts.

The source is the Envelope's frozen snapshot when the dispatcher captured one
\(a plan that previewed and then sent shares exactly that instant).  Only a
request that was never prepared that way resolves the View's own association
here, and captures it once, at this collection.  Nothing is guessed from
focus, buffer names or recent use, and a request that already carries an
assembled prompt (`:chat-prompt-override': the previewed or user-edited final
message) is NOT rendered into again: that text is the exact body the user
reviewed and confirmed, so re-rendering would both duplicate the facts and
restore a selection the user deleted in the inspector.

The live associated object is reported through the Envelope's
`:emacs-origin-buffer', the data-only facts through `:emacs-origin-snapshot';
neither is appended to the user's own text."
  (let* ((context (plist-get env :context))
         (override (plist-get context :chat-prompt-override))
         (frozen (plist-get context :emacs-origin-snapshot)))
    (unless (and (stringp override) (not (string-empty-p override)))
      (let* ((view (plist-get context :chat-buffer))
             (source (or (plist-get context :emacs-origin-buffer)
                         (and (buffer-live-p view)
                              (buffer-local-value 'superchat--origin-buffer view))))
             (live (and (buffer-live-p source) source))
             (snapshot (or frozen (superchat--origin-snapshot source)))
             (facts (superchat--origin-render snapshot))
             (selection (superchat--origin-selection-render snapshot))
             ;; A source that exists but cannot be described is a collection
             ;; error, not a silently empty request: the Event layer records
             ;; the condition and this contributor reports it.
             (_ (when (and source (null facts))
                  (error "Emacs origin source %S yielded no usable snapshot"
                         (buffer-name source)))))
        (when facts
          ;; Report the live object and the frozen facts on the Envelope so
          ;; the request keeps the provenance the rendered text no longer
          ;; uniquely carries.
          (when live
            (setcdr (last context 1) (list :emacs-origin-buffer live)))
          (unless frozen
            (setcdr (last context 1)
                    (list :emacs-origin-snapshot snapshot)))
          (append
           (list (superchat-prompt-fragment
                  :owner 'chat :id "emacs-origin" :slot 'context :text facts
                  :priority 0 :budget-class 'hard
                  :source (list :emacs-origin (plist-get snapshot :name))))
           (when selection
             (list (superchat-prompt-fragment
                    :owner 'chat :id "emacs-origin-selection" :slot 'attachment
                    :text selection
                    :priority 0 :budget-class 'hard :required t
                    :source (list :emacs-origin (plist-get snapshot :name)
                                  :selection t))))))))))


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
;; Each registration is a `collect-context' Event listener, and the Event's
;; listeners run newest-first: history, memory, context-files,
;; file-inline, workspace-rules, system-instructions, chat-input, plan, origin, prepared.
;; Collection order is not the assembly contract -- the fragments carry
;; their own `:priority'/`:budget-class'/`:required', which the assembler
;; sorts -- but it is preserved so a nearer Scope still shadows an
;; inherited contributor first, preserving the established collection order.

(superchat-define-plugin chat-collectors
  :doc "The default collect-context contributors: chat input, system
instructions, #file and context files, the request's Emacs origin and
selection, memory, history, plan, and a producer's prepared text."
  :apply
  (lambda (_scope _config)
    (superchat-context-register 'prepared
                          #'superchat-prompt-collector--prepared :owner 'core)
    (superchat-context-register 'origin
                          #'superchat-prompt-collector--origin :owner 'chat)
    (superchat-context-register 'plan
                          #'superchat-prompt-collector--plan :owner 'chat)
    ;; Registered at the cutover; until then the collector list is as it was.
    (progn
      (superchat-context-register 'permission-context
                            #'superchat-prompt-collector--permission-context
                            :owner 'core)
      (superchat-on nil 'before-provider-request 'permission-context
                    #'superchat-prompt--refresh-permission-context))
    (superchat-context-register 'chat-input
                          #'superchat-prompt-collector--chat-input :owner 'chat)
    (superchat-context-register 'system-instructions
                          #'superchat-prompt-collector--system-instructions :owner 'chat)
    (superchat-context-register 'workspace-rules
                          #'superchat-prompt-collector--workspace-rules :owner 'chat)
    (superchat-context-register 'file-inline
                          #'superchat-prompt-collector--file-inline :owner 'chat)
    (superchat-context-register 'context-files
                          #'superchat-prompt-collector--context-files :owner 'chat)
    (superchat-context-register 'memory
                          #'superchat-prompt-collector--memory :owner 'chat)
    (superchat-context-register 'history
                          #'superchat-prompt-collector--history :owner 'chat)))

(provide 'superchat-prompt-hooks)
;;; superchat-prompt-hooks.el ends here
