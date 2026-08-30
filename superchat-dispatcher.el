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
(require 'superchat-runtime)
(require 'superchat-extension)
(require 'superchat-editor-workspace)

;; ── Forward declarations (owned by superchat.el) ──
(defvar superchat-buffer-name)
(defvar superchat-lang)
(defvar superchat--prompt-start)
(defvar superchat--current-command)
(defvar superchat--active-preset)
(defvar superchat--active-topology)
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
(defvar superchat-inline-text-extensions
  '("org" "md" "markdown" "txt"
    "el" "lisp" "cl" "clj" "cljc" "cljs"
    "py" "js" "jsx" "ts" "tsx"
    "c" "h" "cc" "cpp" "hpp" "rs" "go" "java"
    "rb" "php" "swift" "kt" "kts"
    "sh" "bash" "zsh" "fish"
    "json" "jsonl" "yaml" "yml" "toml" "ini" "cfg" "conf"
    "xml" "html" "htm" "css" "scss" "sass" "sql" "lua" "r")
  "Default extensions eligible for inline text context.

The user option is declared by `superchat.el'; this value also makes the
dispatcher safe to load and call on its own.")
(defvar superchat-prompt-file-extensions)
(defvar superchat-general-answer-prompt)
(defvar superchat--user-commands)
(defvar superchat--builtin-commands)
(defvar superchat-context-message-count)
(defvar superchat-context-max-chars)
(defvar superchat--file-ref-regexp)
(declare-function superchat--strip-leading-user-label "superchat" (input))
(declare-function superchat--input-meets-memory-threshold-p "superchat" (input))
(declare-function superchat--lookup-command-template "superchat" (command))
(declare-function superchat--record-message "superchat" (role content))
(declare-function superchat--update-status "superchat-render" (message))
(declare-function superchat--insert-system-message "superchat-render" (text))
(declare-function superchat--insert-persistent-system-message
                  "superchat-render" (text &optional owned-response-marker))
(declare-function superchat--response-region-insert-transcript
                  "superchat-render" (text owner))
(declare-function superchat--refresh-prompt "superchat-render" ())
(declare-function superchat--insert-prompt "superchat-render" ())
(declare-function superchat--prepare-for-response "superchat-render" ())
(declare-function superchat--response-region-clear-status "superchat-render" ())
(declare-function superchat--ttft-log "superchat-render" (stage))
(declare-function superchat--activate-capability "superchat" (preset &optional name))
(declare-function superchat--apply-topology-overlay "superchat" (turn))
(declare-function superchat--session-runtime-configuration "superchat" ())
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

(defvar superchat--context-file-ranges nil
  "Alist of (path . (start . end)) line ranges for #-attached files.
Populated by `superchat--extract-file-path' and consumed by
`superchat--read-inline-file-content'.")

(defun superchat--extract-file-path (input)
  "Extract and normalize file path from INPUT string.
Handles various edge cases like spaces, parentheses, and quotes in
file paths.  An optional `:START' or `:START-END' suffix records the
line range for inline reading."
  (let ((file-path (superchat-parser-extract-file-path input)))
    (when file-path
      (when (fboundp 'superchat-parser-extract-file-range)
        (let ((range (superchat-parser-extract-file-range input)))
          (if range
              (setf (alist-get file-path superchat--context-file-ranges
                               nil nil #'equal)
                    range)
            (setq superchat--context-file-ranges
                  (assoc-delete-all file-path
                                    superchat--context-file-ranges
                                    #'equal)))))
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

(defconst superchat--binary-file-extensions
  '("7z" "a" "avi" "bin" "bmp" "bz2" "class" "db" "dll" "doc"
    "docx" "dylib" "elc" "exe" "flac" "gif" "gz" "ico" "jpeg"
    "jpg" "m4a" "mkv" "mov" "mp3" "mp4" "o" "ogg" "pdf" "png" "ppt"
    "pptx" "rar" "so" "sqlite" "sqlite3" "tar" "tif" "tiff" "wav"
    "wasm" "webp" "xls" "xlsx" "xz" "zip")
  "Extensions that must never be treated as inline text.

This includes image types handled by `superchat--image-attachment-info'.")

(defconst superchat--text-file-probe-bytes 8192
  "Maximum number of leading bytes used to recognize unlisted text files.")

(defun superchat--textual-file-prefix-p (path)
  "Return non-nil when PATH's bounded literal prefix looks textual."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally
         path nil 0 superchat--text-file-probe-bytes)
        (let ((prefix (buffer-string)))
          (or (cl-some (lambda (bom)
                         (string-prefix-p bom prefix))
                       (list (unibyte-string #xff #xfe)
                             (unibyte-string #xfe #xff)
                             (unibyte-string #xff #xfe 0 0)
                             (unibyte-string 0 0 #xfe #xff)))
              (not
               (cl-loop for char across prefix
                        thereis
                        (or (= char 127)
                            (and (< char 32)
                                 (not (memq char '(9 10 13))))))))))
    (error nil)))

(defun superchat--textual-file-p (path)
  "Return non-nil if PATH looks like a textual file we can inline.

Configured extensions are the no-content-I/O fast path.  Other regular files
are accepted when a bounded literal prefix contains no binary control bytes,
or starts with a UTF-16/UTF-32 byte-order mark."
  (when (stringp path)
    (let* ((ext (downcase (or (file-name-extension path) "")))
           (attributes
            (condition-case nil
                (file-attributes (file-truename path))
              (error nil))))
      (and attributes
           (eq (file-attribute-type attributes) nil)
           (file-regular-p path)
           (file-readable-p path)
           (not (member ext superchat--binary-file-extensions))
           (or (member ext superchat-inline-text-extensions)
               (superchat--textual-file-prefix-p path))))))

(defun superchat--read-inline-file-content (file-path)
  "Read and trim FILE-PATH for use in prompts.
Honors the `:START' / `:START-END' range recorded by the `#' parser:
only those lines enter the prompt.  Returns the content string, or nil
on failure."
  (when (and (stringp file-path)
             (file-exists-p file-path))
    (condition-case err
        (let ((content
               (with-temp-buffer
                 (insert-file-contents file-path nil 0
                                       superchat-inline-max-bytes)
                 (buffer-string)))
              (range (cdr (assoc file-path
                                 superchat--context-file-ranges))))
          (if (null range)
              (string-trim content)
            (let* ((lines (split-string content "\n"))
                   (start (max 1 (car range)))
                   (end (min (length lines) (cdr range))))
              (if (> start (length lines))
                  ""
                (string-trim
                 (mapconcat #'identity
                            (cl-subseq lines (1- start) end)
                            "\n"))))))
      (error
       (message "Warning: Failed to read file %s: %s"
                file-path (error-message-string err))
       nil))))

(defun superchat--file-metadata-block (file-path)
  "Return a constant-size metadata block describing FILE-PATH.

Injected in place of the file body when it exceeds
`superchat-inline-metadata-threshold': the model learns the file's
shape and how to read slices, without the content entering the root
context."
  (let* ((size (file-attribute-size (file-attributes file-path)))
         (head (with-temp-buffer
                 (insert-file-contents file-path nil 0 (min size 500))
                 (buffer-string)))
         ;; Counting lines means reading the whole file; only do it when
         ;; the file is small enough that loading it is cheap.  For a huge
         ;; log the exact total is not worth freezing Emacs over.
         (line-count (when (<= size superchat-inline-max-bytes)
                       (with-temp-buffer
                         (insert-file-contents file-path)
                         (count-lines (point-min) (point-max))))))
    (format "[Referenced file — NOT inlined]
Path: %s
Size: %d bytes%s.
First %d characters:
%s
...
The full content is on disk, not in this conversation.  Read only the
slices you need by asking the user to attach #%S:START-END, and delegate
per-slice analysis to sub-agents rather than reading the whole file into
this context."
            file-path size
            (if line-count (format ", %d lines" line-count) "")
            (length head) head file-path)))

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
  (let ((context-files
         (delete-dups
          (append (superchat-turn-context-files turn)
                  superchat--current-context-files))))
    `(:type :llm-query :prompt ,(superchat-turn-prompt turn)
      :user-message ,(unless (string-empty-p (superchat-turn-clean-input turn))
                       (superchat-turn-clean-input turn))
      ,@(when context-files `(:context-files ,context-files))
      ,@(when (superchat-turn-preset turn)
          `(:preset ,(superchat-turn-preset turn)))
      ,@(when target-model `(:target-model ,target-model))
      ,@(let ((sys (superchat-turn-system-prompt turn)))
          (when (and (stringp sys) (not (string-empty-p (string-trim sys))))
            `(:system-prompt ,sys)))
      ,@(when (superchat-turn-tools turn)
          `(:tools ,(superchat-turn-tools turn))))))

(defun superchat--input-with-origin (input origin)
  "Return INPUT with a readable Emacs ORIGIN block appended."
  (if (not origin)
      input
    (let ((buffer (plist-get origin :buffer))
          (file (plist-get origin :file))
          (mode (plist-get origin :major-mode))
          (point (plist-get origin :point))
          (region (plist-get origin :region)))
      (concat
       input
       "\n\n[Emacs origin]\n"
       (when buffer (format "Buffer: %s\n" buffer))
       (when file (format "File: %s\n" file))
       (when mode (format "Major mode: %s\n" mode))
       (when point (format "Point: %s\n" point))
       (when region (format "\nSelected text:\n%s\n" region))))))

(defun superchat--attach-turn-memories (turn input session-id &optional consume)
  "Attach relevant memories to TURN for INPUT in SESSION-ID.
When CONSUME is non-nil, clear memories queued by `/recall'."
  (cond
   (superchat--pending-recalled-memories
    (setf (superchat-turn-retrieved-memories turn)
          superchat--pending-recalled-memories)
    (when consume
      (setq superchat--pending-recalled-memories nil)))
   ((and (superchat--input-meets-memory-threshold-p input)
         (fboundp 'superchat-view-search)
         session-id)
    (when-let* ((rows (superchat-view-search input session-id 5))
                (memories (mapcar #'superchat-view-row-to-memory-plist rows)))
      (setf (superchat-turn-retrieved-memories turn) memories))))
  turn)

(defun superchat--request-plan-from-turn
    (turn &optional template target-model _finalized)
  "Build an inspectable request plan from prepared TURN.
TEMPLATE and TARGET-MODEL have the same meaning as in
`superchat--execute-llm-query'.  The mutable TURN stays internal."
  (setq turn (superchat--apply-topology-overlay turn))
  (let ((plan (superchat--execute-llm-query turn template target-model)))
    (setq plan (plist-put plan :input (superchat-turn-inbound turn)))
    (setq plan
          (plist-put plan :clean-input (superchat-turn-clean-input turn)))
    (setq plan (plist-put plan :origin (superchat-turn-origin turn)))
    (setq plan (plist-put plan :session-id (superchat-turn-session-id turn)))
    (setq plan (plist-put plan :buffer (current-buffer)))
    (setq plan (plist-put plan :buffer-name (buffer-name)))
    (setq plan (plist-put plan :lang superchat-lang))
    (when-let* ((envelope
                 (plist-get (superchat-turn-state turn) :context-envelope)))
      (setq plan (plist-put plan :context-envelope (copy-tree envelope))))
    (let ((configuration (superchat--session-runtime-configuration)))
      (while configuration
        (setq plan
              (plist-put plan (pop configuration) (pop configuration)))))
    plan))

(defun superchat-build-request-plan (input &optional session-id origin)
  "Build a dry-run request plan for INPUT, SESSION-ID, and ORIGIN.
No backend call or post-turn side effect occurs.  The first slice
supports plain requests and active prompt command mode; slash commands,
skills, workflows, and agent presets continue to use the chat buffer."
  (let* ((raw-input (string-trim (or input "")))
         (full-input (superchat--input-with-origin raw-input origin))
         (turn (superchat-turn-new full-input session-id)))
    (when (string-empty-p (superchat-turn-inbound turn))
      (user-error "Request input is empty"))
    (setf (superchat-turn-origin turn) origin)
    (superchat--attach-turn-memories turn full-input session-id)
    (let* ((prepared (superchat-core-prepare-turn turn))
           (skill (superchat-turn-skill prepared))
           (command (superchat-turn-command prepared)))
      (when (and (null skill)
                 (boundp 'superchat--active-preset)
                 superchat--active-preset)
        (superchat-preset-apply superchat--active-preset prepared))
      (when (or skill command
                (and (superchat-turn-preset prepared)
                     (superchat-preset-agent-p
                      (superchat-turn-preset prepared))))
        (user-error
         "Request inspection currently supports plain and prompt-mode turns"))
      (let* ((template (when superchat--current-command
                         (superchat--lookup-command-template
                          superchat--current-command)))
             (plan (superchat--request-plan-from-turn
                    prepared template
                    (superchat-turn-target-model prepared))))
        (setq plan (plist-put plan :input raw-input))
        (setq plan (plist-put plan :clean-input raw-input))
        (setq plan (plist-put plan :user-message raw-input))
        (setq plan
              (plist-put plan :consume-pending-memories
                         (and superchat--pending-recalled-memories t)))
        plan))))

(defun superchat--submit-chat-request (request)
  "Compose and submit producer-approved Chat REQUEST.

The producer's exact buffer and Session identity are fixed before Extension
composition.  Extensions may enrich the Request, but cannot retarget either
identity.  Both the producer pair and the final pair cross the single
EditorWorkspace validation authority before Runtime effects begin."
  (let* ((producer-buffer (or (plist-get request :buffer) (current-buffer)))
         (producer-session-id (plist-get request :session-id)))
    (superchat-editor-workspace-validate-buffer
     producer-buffer producer-session-id)
    ;; Materialize the producer's implicit current View before composition so
    ;; omission cannot be interpreted later as Extension-selected identity.
    (plist-put request :buffer producer-buffer)
    (let* ((identity-guard
            (lambda (candidate)
              (unless (and (plist-member candidate :buffer)
                           (eq (plist-get candidate :buffer)
                               producer-buffer))
                (error "Extension cannot retarget or remove the Chat Request buffer"))
              (unless (and (plist-member candidate :session-id)
                           (equal (plist-get candidate :session-id)
                                  producer-session-id))
                (error "Extension cannot retarget or remove the Chat Request Session"))
              (superchat-editor-workspace-validate-buffer
               producer-buffer producer-session-id)))
           (composed
            (superchat-extension-compose-request request identity-guard)))
      (plist-put
       composed :buffer
       producer-buffer)
      (superchat-runtime-submit composed))))

(defun superchat-execute-request-plan (plan)
  "Execute the exact inspectable request PLAN and return PLAN."
  (unless (eq (plist-get plan :type) :llm-query)
    (user-error "Not an executable LLM request plan"))
  (when (plist-get plan :consume-pending-memories)
    (setq superchat--pending-recalled-memories nil))
  (plist-put plan :run-handle
             (superchat--submit-chat-request plan))
  plan)

(defun superchat--handle-command (command args input &optional lang _target-model)
  "Dispatch COMMAND with ARGS, INPUT, and LANG via alist, hook chain, then builtins."
  (superchat--ensure-command-loaded command)
  (or
   ;; 1. Alist lookup (fast path for registered commands)
   (when-let* ((handler (cdr (assoc command superchat--command-alist))))
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
  "Send the visible draft, or retain it while ordinary chat is active."
  (interactive)
  (let* ((view (current-buffer))
         (session superchat--session-id)
         (superchat-buffer-name (buffer-name view))
         ;; Keep the existing borrowed-input marker contract.  Render owns
         ;; header validity/association; the dispatcher reads its projection.
         (prompt (and (markerp superchat--prompt-start)
                      (eq (marker-buffer superchat--prompt-start) view)
                      (marker-position superchat--prompt-start)
                      superchat--prompt-start))
         (input (and prompt
                     (string-trim
                      (superchat--strip-leading-user-label
                       (buffer-substring-no-properties prompt (point-max)))))))
    (when (and input (not (string-empty-p input)))
      (let* ((parsed (superchat-core--parse-input
                      (superchat-turn-new input session)))
             (command (superchat-turn-command parsed)))
        (cond
         ;; This command owns its confirmation and exact View transaction.
         ;; Dispatch it before any prompt, recall, timing or text mutation.
         ((equal command "clear")
          (superchat--dispatch-result
           (superchat--handle-command command (superchat-turn-command-args parsed)
                                      (superchat-turn-clean-input parsed)
                                      superchat-lang (superchat-turn-target-model parsed))
           superchat-lang (superchat-turn-target-model parsed)))
         ((and (bound-and-true-p superchat-mode)
               (null command) (null (superchat-turn-skill parsed))
               (cl-some
                (lambda (status)
                  (and (equal session (plist-get status :session-id))
                       (eq (plist-get status :surface) 'superchat)
                       ;; The current projection also covers synchronous UI
                       ;; reentry before submit returns its tracking handle.
                       (or (equal (plist-get status :run-id)
                                  (bound-and-true-p superchat--active-run-id))
                           (equal (plist-get status :transcript-owner)
                                  superchat--response-region-current-owner))
                       (not (eq (plist-get status :topology) 'workflow))))
                (plist-get (superchat-runtime-status-projection) :active-runs)))
          (message "Draft retained. Edit and send it after the current response finishes."))
         (t
          (unwind-protect
              (superchat--send-input-now)
            ;; Runtime has adopted the consumed response state before returning.
            ;; Synchronous final delivery may already have supplied this prompt.
            (when (and (buffer-live-p view)
                       (equal session (buffer-local-value 'superchat--session-id view)))
              (with-current-buffer view
                (let ((superchat-buffer-name (buffer-name view)))
                  (superchat--response-region-ensure-view-prompt)))))))))))

(defun superchat--send-input-now ()
  "Parse user input, run through hook pipeline, dispatch, and render result."
  (let ((superchat-buffer-name (buffer-name)))
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
        (superchat--attach-turn-memories
         turn input superchat--session-id t)
        (superchat--ttft-log "memory-recall")
        (let ((inhibit-read-only t)
              (end-of-input (point-max)))
          (put-text-property superchat--prompt-start end-of-input 'read-only t)
          (put-text-property superchat--prompt-start end-of-input
                             'superchat-role 'user)
          (if (fboundp 'superchat--response-region-consume-prompt)
              (superchat--response-region-consume-prompt)
            (setq superchat--prompt-start nil)))
        (superchat--prepare-for-response)
        ;; ── Step D: Topic lifecycle hooks + core pipeline ──
        (run-hook-with-args 'superchat-pre-topic-functions turn)
        (let* ((prepared (superchat-core-prepare-turn turn)))
          (when superchat-post-topic-functions
            (setq prepared (superchat-core--run-hook-chain
                            prepared 'superchat-post-topic-functions)))
          (let* ((command (superchat-turn-command prepared))
                 (skill (superchat-turn-skill prepared))
                 (target-model (superchat-turn-target-model prepared))
                 (clean-input (superchat-turn-clean-input prepared))
                 (superchat--current-turn prepared))
            (superchat--ttft-log "core-pipeline")
          (dolist (notice
                   (plist-get (superchat-turn-state prepared)
                              :attachment-notices))
            (superchat--insert-persistent-system-message notice))
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
            ;; The coordinator is a local command; each executable workflow
            ;; step owns its lifecycle through Runtime.
            (let* ((rest (string-trim (or clean-input "")))
                   (name (when (string-match "^\\([^[:space:]]+\\)" rest)
                           (match-string 1 rest)))
                   (args (if name
                             (string-trim (substring rest (match-end 1)))
                           "")))
              (let ((workflow-file
                     (and name
                          (fboundp 'superchat-workflow--exists-p)
                          (superchat-workflow--exists-p name))))
                (if workflow-file
                    (superchat-workflow-execute-async
                     name args workflow-file)
                (superchat--dispatch-result
                 (list :type :buffer
                       :content (cond
                                 (name
                                  (format "No such workflow: %s" name))
                                 ((string-empty-p rest)
                                  "Usage: >>name [args]")
                                 (t
                                  (format "Invalid workflow name: %s" rest))))
                 lang target-model)))))
           ;; Skill invocation (>skill-name)
           (skill
            (let* ((skipped-command
                    (and (fboundp 'superchat-parser-command)
                         (superchat-parser-command (or clean-input ""))))
                   (result (when (fboundp 'superchat-skills-invoke)
                             (superchat-skills-invoke skill clean-input prepared))))
              (when skipped-command
                (message "/%s 未作为命令执行，已当作普通文字"
                         (car skipped-command)))
              ;; If the invoked skill is an agent/plan preset, make it the
              ;; active preset for the rest of the session.
              (when-let* ((preset (and result (plist-get result :preset)))
                          ((or (superchat-preset-agent-p preset)
                               (superchat-preset-plan-p preset)))
                          ((boundp 'superchat--active-preset)))
                (superchat--activate-capability preset)
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
            (let ((result
                   (or (superchat--handle-command
                        command (superchat-turn-command-args prepared)
                        clean-input lang target-model)
                       (superchat--execute-llm-query
                        prepared nil target-model))))
              (superchat--dispatch-result result lang target-model)))
           ;; Active command mode
           (superchat--current-command
            (let ((template (superchat--lookup-command-template
                             superchat--current-command)))
              (superchat--dispatch-llm-or-agent prepared lang target-model template)))
           ;; Default: plain LLM query
           (t
            (superchat--dispatch-llm-or-agent prepared lang target-model))))))))))

(defun superchat--dispatch-llm-or-agent (turn lang target-model &optional template)
  "Submit TURN through Runtime using optional TEMPLATE."
  (let ((plan (superchat--request-plan-from-turn
               turn template target-model)))
    (plist-put plan :lang lang)
    (let ((executed (superchat-execute-request-plan plan)))
      (when (fboundp 'superchat--track-active-run)
        (superchat--track-active-run (plist-get executed :run-handle)))
      executed)))

(defun superchat--dispatch-result (result lang target-model)
  "Handle a RESULT plist from command dispatch, using LANG and TARGET-MODEL.
Extracted from old superchat-send-input for reuse in command handlers."
  (pcase (plist-get result :type)
    (:buffer
     (let ((content (plist-get result :content)))
       (with-current-buffer (get-buffer-create superchat-buffer-name)
         (let ((inhibit-read-only t))
           (superchat--response-region-insert-transcript
            (concat "\n** System\n" content "\n")
            :independent-turn)
           (superchat--refresh-prompt)))))
    (:echo
     (message "%s" (plist-get result :content))
     (superchat--refresh-prompt))
    (:noop nil)
    (:llm-query
     (unless (plist-get result :session-id)
       (plist-put result :session-id superchat--session-id))
     (unless (plist-get result :buffer-name)
       (plist-put result :buffer-name superchat-buffer-name))
     (unless (plist-get result :lang)
       (plist-put result :lang lang))
     (plist-put
      result :buffer
      (superchat-editor-workspace-validate-buffer
       (current-buffer) (plist-get result :session-id)))
     (superchat--ttft-log "dispatch")
     (let ((run-handle (superchat--submit-chat-request result)))
       (if (fboundp 'superchat--track-active-run)
           (superchat--track-active-run run-handle)
         run-handle)))
    (:llm-query-and-mode-switch
     (superchat--update-status
      (format "Executing `/%s'..."
              (or (plist-get result :command) "?")))
     (let* ((real-args (plist-get result :args))
            (template (plist-get result :template))
            (target-model (plist-get result :target-model))
            (turn (superchat-turn-new real-args superchat--session-id))
            (prepared (superchat-core-prepare-turn turn))
            (_ (when (and (boundp 'superchat--active-preset)
                          superchat--active-preset)
                 (superchat-preset-apply
                  superchat--active-preset prepared)))
            (plan (superchat--request-plan-from-turn
                   prepared template target-model)))
       (plist-put plan :buffer-name (buffer-name))
       (plist-put plan :lang lang)
       (let ((executed (superchat-execute-request-plan plan)))
         (when (fboundp 'superchat--track-active-run)
           (superchat--track-active-run
            (plist-get executed :run-handle)))
         executed)))))


(provide 'superchat-dispatcher)
;;; superchat-dispatcher.el ends here
