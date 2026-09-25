;;; superchat-dispatcher.el --- Command dispatch and prompt building for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; User input dispatch, prompt building, and the main send-input pipeline.
;; Extracted from superchat.el monolith (v0.9 split step 6 — pure move only,
;; hook registration deferred to follow-up).

;;; Code:

(require 'cl-lib)
(require 'llm nil t)
(require 'superchat-plugin)
(require 'superchat-service)
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
(require 'superchat-editor-workspace)
(require 'superchat-pipeline)

;; ── Forward declarations (owned by superchat.el) ──
(declare-function superchat--with-supported-input "superchat" (request scope &optional current-p))
(declare-function superchat--request-attachment-description "superchat" (file))
(declare-function superchat--origin-remember "superchat" (view snapshot))
(declare-function superchat--origin-known-fallback "superchat" (view source))
(declare-function superchat--origin-snapshot "superchat" (source))
(defvar superchat--origin-buffer)
(defvar superchat-buffer-name)
(defvar superchat-lang)
(defvar superchat--prompt-start)
(defvar superchat--current-command)
(defvar superchat--active-preset)
(defvar superchat--active-preset-scope)
(defvar superchat--active-topology)
(defvar superchat--current-context-files)
(defvar superchat--conversation-history)
(defvar superchat--pending-recalled-memories)
;; Bound by `superchat--send-input-now' around one immediate send, so the
;; submission seam can record the driver's answer and the send can put a
;; refused draft back.  Declared here because a `defvar' after the first `let'
;; that binds it would make that binding lexical once byte-compiled.
(defvar superchat--send-transaction)
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
(defvar superchat-memory-recall-scope)
(defvar superchat-memory-auto-recall-cross-session)
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
(declare-function superchat--activate-capability "superchat" (preset &optional name restore-p))
(declare-function superchat--apply-topology-overlay "superchat" (turn))
(declare-function superchat--session-runtime-configuration "superchat" ())
(declare-function superchat--add-file-to-context "superchat" (file-path))
(declare-function superchat--get-available-models "superchat-models" ())
(declare-function superchat--effective-llm-backend "superchat-llm" (&optional target-model))
(declare-function superchat-view-search "superchat-tape-view" (query &optional session-id limit))
(declare-function superchat-db-tape-search-prioritized "superchat-db"
                  (query current-session-id limit &optional foreign-eligible-only
                         exclude-ids current-session-only))
(declare-function superchat-view-row-to-memory-plist "superchat-tape-view" (row))
(declare-function superchat-tape-append-event-record "superchat-tape" (event))

(define-error 'superchat-preset-request-scope-retired
  "Preset Scope retired while preparing this request")

(defun superchat--chat-execution-scope (&optional explicit-scope producer-buffer)
  "Return this producer's Scope without putting it into public request data.

EXPLICIT-SCOPE wins, as does an enclosing `superchat-execution-scope' binding
(including an explicit global binding).  Only an otherwise unscoped chat View
selects its locally owned active Preset Scope.  When PRODUCER-BUFFER is live,
its View-local Scope is used instead of the ambient buffer's; this preserves
the identity of a previously composed request submitted from another View.
Callers outside a View retain the global default.  This helper never infers a
Scope from a Session or a buffer name."
  (or explicit-scope
      superchat-execution-scope
      ;; Supplying an exact producer buffer is meaningful even when it has no
      ;; Preset Scope: that plain View deliberately resolves to global, rather
      ;; than falling through to an ambient caller View's private composition.
      (and producer-buffer
           (if (and (buffer-live-p producer-buffer)
                    (boundp 'superchat--active-preset-scope))
               (or (buffer-local-value 'superchat--active-preset-scope
                                       producer-buffer)
                   superchat-scope-global)
             superchat-scope-global))
      superchat--active-preset-scope
      superchat-scope-global))

(defmacro superchat--with-chat-execution-scope (&rest body)
  "Evaluate BODY with the current chat producer Scope dynamically bound."
  (declare (indent 0) (debug t))
  `(let ((superchat-execution-scope (superchat--chat-execution-scope)))
     ,@body))

(defun superchat--apply-active-preset (turn)
  "Apply this View's active Preset to TURN under its captured Scope.

This is the UI producer seam, not a change to the pure
`superchat-preset-apply' contract.  An ordinary request has already acquired
its persona and tool profile before a Preset's `:pre' hook can reenter and
activate another Preset.  Keep that hook in the old Scope, and reject the
prepared request if the hook retired it; it must not silently submit old Turn
data through the replacement Scope.  Callers bind the returned Scope while
they finish that ordinary request.  Explicit skill routing deliberately does
not use this helper, because its selected skill may intentionally activate a
new Preset before submission."
  (when (and (boundp 'superchat--active-preset)
             superchat--active-preset)
    (let ((view (current-buffer))
          (preset superchat--active-preset)
          (scope (superchat--chat-execution-scope)))
      ;; `:pre' is user code.  It may select another buffer as well as reenter
      ;; Preset activation, so preserve the producer View while retaining the
      ;; exact Scope it observed.
      (save-current-buffer
        (with-current-buffer view
          (let ((superchat-execution-scope scope))
            (superchat-preset-apply preset turn))))
      (when (superchat-execution-scope-retired-p scope)
        (signal 'superchat-preset-request-scope-retired nil))
      scope)))

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
  "Return a result plist for the dispatcher from a parsed TURN.
TURN must have been run through `superchat-core-prepare-turn'.  `:prompt'
is TURN's prompt slot, which is empty unless a skill fixed a combined text;
TEMPLATE (an active `/command' template) travels as `:chat-template'.
TARGET-MODEL is an optional one-shot model override; when nil, the
turn's own target-model is used — that is how a preset's model
(applied after the dispatcher captured its binding) and the
sub-agent path (which calls with no explicit model) reach the
effective backend."
  (setq target-model (or target-model (superchat-turn-target-model turn)))
  ;; Step 3: no re-collection here; TEMPLATE travels to the chat-input
  ;; collector as `:chat-template' and the driver assembles once.
  (let ((context-files
         (delete-dups
          (append (superchat-turn-context-files turn)
                  superchat--current-context-files))))
    `(:type :llm-query :prompt ,(superchat-turn-prompt turn)
      ,@(when template `(:chat-template ,template))
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

(defun superchat--origin-request-data (origin)
  "Return ORIGIN's request-carried data, or nil without an ORIGIN.
ORIGIN is the dispatching caller's plist (`superchat--capture-origin').
The result keeps the two things the contract separates: the live object
under `:buffer' (never recorded) and the captured facts under `:snapshot'
(data only).  This is the act entry's one and only capture: the snapshot
rides the plan and is reused by the preview and by the eventual send."
  (when origin
    (list :buffer (let ((named (get-buffer (or (plist-get origin :buffer) ""))))
                    (and (buffer-live-p named) named))
          :snapshot
          (let ((name (plist-get origin :buffer))
                (file (plist-get origin :file))
                (mode (plist-get origin :major-mode))
                (point (plist-get origin :point))
                (tick (plist-get origin :tick))
                (region (plist-get origin :region)))
            (when name
              (append
               (list :name name)
               (when file (list :file file))
               (when mode
                 (list :major-mode (if (symbolp mode) (symbol-name mode) mode)))
               (when tick (list :tick tick))
               (when point (list :point point))
               (when (and (stringp region) (not (string-empty-p region)))
                 (list :selection region))))))))

(defun superchat--resolve-origin-data (view &optional origin)
  "Return the origin data a request prepared for VIEW carries.
ORIGIN, when given, is this call's own source and wins: it is captured now
and frozen.  Otherwise VIEW's associated editing source supplies it, and no
association is an ordinary answer with no origin data at all.

A live source is captured fresh, and those facts are remembered on VIEW as
its last known facts -- the only thing left to report once the source dies,
because a killed buffer answers every accessor, `buffer-name' included, with
nil.  A source that has already died is reported from those kept facts plus
`:dead', never from a same-named buffer."
  (or (superchat--origin-request-data origin)
      (let ((source (and (buffer-live-p view)
                         (buffer-local-value 'superchat--origin-buffer view))))
        (cond
         ((null source) nil)
         ((buffer-live-p source)
          (list :buffer source
                :snapshot (superchat--origin-remember
                           view (superchat--origin-snapshot source))))
         (t (list :snapshot (superchat--origin-known-fallback view source)))))))

(defun superchat--origin-plan-carrier (turn)
  "Return the View a request prepared from TURN may take its source from.
Nil for a request no View prepared: a producer's own generated Turn carries
`(:producer plan)' as its origin marker and speaks for no editing source, so
it never inherits whatever View happens to be current."
  (unless (plist-get (superchat-turn-origin turn) :producer)
    (current-buffer)))

(defun superchat--freeze-origin-data (plan view)
  "Fill PLAN's frozen origin data and return PLAN.
A request carries the source facts as they were when it was PREPARED: this is
called once per prepared request -- ordinary chat, queued chat and the
inspector's plan alike -- so a later rename, rebind or death changes what the
next request says, never what this one already says.  A plan that resolved
its own source (the act entry) is left exactly as it is, and a nil VIEW (a
producer's generated request) adds nothing at all."
  (when (and view (not (plist-get plan :emacs-origin-data)))
    (plist-put plan :emacs-origin-data
               (superchat--resolve-origin-data
                view (and (boundp 'superchat--preview-origin-data)
                          superchat--preview-origin-data))))
  plan)

(defun superchat--origin-context (plan)
  "Return PLAN's owner-prefixed origin `:context' keys, or nil.
The live object and the data-only snapshot travel separately, and only when
PLAN actually carries one: an ordinary request with no source adds no keys at
all.  The live object is process-local -- it is never part of the recorded
`:context-envelope', which the Turn's continuity state owns."
  (when-let* ((data (plist-get plan :emacs-origin-data))
              ((plist-get data :snapshot)))
    (append (when-let* ((buffer (plist-get data :buffer))
                        ((buffer-live-p buffer)))
              (list :emacs-origin-buffer buffer))
            (list :emacs-origin-snapshot (plist-get data :snapshot)))))

(defvar superchat--preview-origin-data nil
  "Origin data `superchat--preview-chat-prompt' must assemble with.
Bound by `superchat-build-request-plan' around the preview so the assembled
text and the request that later reuses it are built from the same frozen
snapshot: the preview is the request's one moment of capture, and the send
only re-renders what it already holds.")

(defun superchat--attach-indexed-memories (turn input session-id)
  "Attach indexed recall to TURN for INPUT in SESSION-ID, never the user queue.
Rows from SESSION-ID are eligible regardless of kind, as before, except
those inside its own recent `superchat-context-message-count' window
(already present as ordinary conversation history) -- excluded in the
search itself (see `superchat-db-tape-search-prioritized''s EXCLUDE-IDS),
before LIMIT is applied, so a Session with enough matching rows to fill
the whole fetch cannot starve out a foreign eligible row the way a
post-fetch filter would.  This exclusion applies uniformly across every
`superchat-memory-auto-recall-cross-session' setting, `none' included
(see CURRENT-SESSION-ONLY below) -- a row already visible as ordinary
history must never be re-injected as a \"memory\" no matter how
cross-session eligibility is configured.  Whether a row from a DIFFERENT
session is eligible is controlled by
`superchat-memory-auto-recall-cross-session': `remembered' (the default)
admits only explicit `/remember' anchors (kind \"anchor\", topic
\"remember\"); `all' admits any matching row from any session
(Superchat's original behaviour); `none' admits no cross-session rows at
all, via `superchat-db-tape-search-prioritized''s CURRENT-SESSION-ONLY
mode.  Eligibility, session priority, and the own-session recency
exclusion are all expressed in the search itself (see
`superchat-db-tape-search-prioritized'), so an eligible row can never be
starved out of the result by unrelated, ineligible, or excluded rows that
merely rank better by relevance or recency.  `superchat-tool-memory-search'
(the model's `memory_search' tool) follows this same
`superchat-memory-auto-recall-cross-session' option, through the same
shared `superchat-db-tape-search-prioritized' search, since it is a
model-initiated lookup rather than something the user directly typed.
Only the user's own `/recall' command is unaffected -- it keeps
consulting `superchat-memory-recall-scope' instead."
  (when (and (superchat--input-meets-memory-threshold-p input)
             (fboundp 'superchat-view-search)
             session-id)
    (let* ((limit 5)
           (cross-session
            (if (boundp 'superchat-memory-auto-recall-cross-session)
                superchat-memory-auto-recall-cross-session
              'remembered))
           (none-p (eq cross-session 'none))
           (window-size (and (boundp 'superchat-context-message-count)
                              superchat-context-message-count))
           (session-entries (and (fboundp 'superchat-view-entries)
                                  (superchat-view-entries session-id)))
           (recent-window (if (natnump window-size)
                               (last session-entries window-size)
                             session-entries))
           (recent-ids (mapcar #'car recent-window))
           (rows
            (cond
             ((fboundp 'superchat-db-tape-search-prioritized)
              (superchat-db-tape-search-prioritized
               input session-id limit (eq cross-session 'remembered)
               recent-ids none-p))
             (none-p
              ;; Defensive fallback only, for when the shared search is
              ;; unavailable; it has no exclude-ids support of its own, so
              ;; the own-session recency exclusion still needs a post-fetch
              ;; pass here.
              (cl-remove-if
               (lambda (row) (memql (nth 0 row) recent-ids))
               (superchat-view-search input session-id (* limit 2))))
             (t
              (cl-remove-if
               (lambda (row)
                 (and (equal (nth 1 row) session-id)
                      (memql (nth 0 row) recent-ids)))
               (superchat-view-search input session-id (* limit 2))))))
           (kept (cl-subseq rows 0 (min limit (length rows))))
           (memories (mapcar #'superchat-view-row-to-memory-plist kept)))
      (when memories
        (setf (superchat-turn-retrieved-memories turn) memories))))
  turn)

(defun superchat--attach-turn-memories (turn input session-id &optional consume)
  "Attach relevant memories to TURN for INPUT in SESSION-ID.
When CONSUME is non-nil, clear memories queued by `/recall'."
  (cond
   (superchat--pending-recalled-memories
    (setf (superchat-turn-retrieved-memories turn)
          superchat--pending-recalled-memories)
    (when consume
      (setq superchat--pending-recalled-memories nil)))
   (t (superchat--attach-indexed-memories turn input session-id)))
  turn)

(defun superchat--request-plan-from-turn
    (turn &optional template target-model _finalized)
  "Build an inspectable request plan from prepared TURN.
TEMPLATE and TARGET-MODEL have the same meaning as in
`superchat--execute-llm-query'.  The mutable TURN stays internal."
  (setq turn (superchat--apply-topology-overlay turn))
  (let ((plan (superchat--execute-llm-query turn template target-model)))
    ;; `:input' is what Runtime's `intent.received' records as the Tape's
    ;; user-message content (`superchat-runtime.el' prefers `:input' over
    ;; `:clean-input'/`:user-message'), which later renders as this Turn's
    ;; line in conversation history.  Using the ref-free clean-input here
    ;; (already stripped by `superchat-prompt-collect' inside
    ;; `superchat-core-prepare-turn') keeps a `#file' token out of every
    ;; later Turn's history, not just this one's own prompt.
    (setq plan (plist-put plan :input (superchat-turn-clean-input turn)))
    (setq plan
          (plist-put plan :clean-input (superchat-turn-clean-input turn)))
    ;; Step 3: the parsed Turn rides to the chat Envelope's `:chat-turn'.
    (setq plan (plist-put plan :chat-turn turn))
    (setq plan (plist-put plan :origin (superchat-turn-origin turn)))
    (setq plan (plist-put plan :session-id (superchat-turn-session-id turn)))
    (setq plan (plist-put plan :buffer (current-buffer)))
    (setq plan (plist-put plan :buffer-name (buffer-name)))
    (setq plan (plist-put plan :lang superchat-lang))
    ;; The request's source facts are frozen here, at the one moment a request
    ;; is prepared -- ordinary chat, queued chat and the inspector's plan all
    ;; pass through it.  A request that waits in the queue, or whose envelope
    ;; is assembled later, therefore still says what was true when the user
    ;; sent it; only the NEXT request re-reads the View.
    (setq plan (superchat--freeze-origin-data plan (superchat--origin-plan-carrier turn)))
    (when-let* ((envelope
                 (plist-get (superchat-turn-state turn) :context-envelope)))
      (setq plan (plist-put plan :context-envelope (copy-tree envelope))))
    (let ((configuration (superchat--session-runtime-configuration)))
      (while configuration
        (setq plan
              (plist-put plan (pop configuration) (pop configuration)))))
    plan))

(defun superchat--preview-chat-prompt (turn &optional template)
  "Assemble parsed TURN for an act request or its optional inspector.
Return the display system, editable body and prepared context for sending.
Use the same collection boundary and base fragments as the driver, without
submitting.  Required content over budget signals before a plan is produced.
`superchat--preview-origin-data' supplies the frozen origin snapshot."
  (superchat--with-chat-execution-scope
    (let* ((state (superchat-turn-state turn))
           (env (list :content (or (superchat-turn-clean-input turn) "")
                      :surface 'chat
                      :session-id (superchat-turn-session-id turn)
                      :target-model (superchat-turn-target-model turn)
                      :system-prompt (superchat-turn-system-prompt turn)
                      :context
                      (append
                       (list :chat-turn turn
                             :chat-file-ref (plist-get state :file-ref)
                             :chat-template template)
                       (superchat--origin-context
                        (list :emacs-origin-data superchat--preview-origin-data)))))
           (collected
            (superchat-pipeline--collect-context
             env nil (superchat-turn-tools turn)))
           (result
            (superchat-prompt-assemble
             (superchat-pipeline--context-fragments env collected)
             (superchat-pipeline--budget (superchat-turn-target-model turn)))))
      (list :system (or (plist-get result :system) "")
            :prompt (plist-get (car (plist-get result :messages)) :content)
            :prepared-context
            (list :prompt
                  (plist-get (car (plist-get result :messages)) :content)
                  :system-fragments
                  (cl-remove-if-not
                   (lambda (fragment)
                     (memq (plist-get fragment :slot) '(system instruction)))
                   collected)
                  ;; History refitting during assembly fixes this selection.
                  :context-envelope
                  (copy-tree (plist-get (superchat-turn-state turn)
                                        :context-envelope)))))))

(defun superchat-build-request-plan (input &optional session-id origin)
  "Build a dry-run request plan for INPUT, SESSION-ID, and ORIGIN.
No backend call or post-turn side effect occurs.  The first slice
supports plain requests and active command mode; slash commands,
skills, workflows, and agent presets continue to use the chat buffer.

This is the one moment a request prepared here captures its source: ORIGIN
is this call's own source (the act entry), otherwise the View's associated
editing source is read.  The captured snapshot is frozen on the plan, so the
preview, the inspector's confirmed send and the submitted request all speak
about the same instant -- the source buffer may change afterwards and the
request still reports what was true when it was prepared.  INPUT itself
stays the user's own text; the facts travel as request data, never appended
to what the user typed and never as instructions."
  (let* ((raw-input (string-trim (or input "")))
         (full-input raw-input)
         (turn (superchat-turn-new full-input session-id))
         (view (current-buffer))
         (origin-data (superchat--resolve-origin-data view origin)))
    (when (string-empty-p (superchat-turn-inbound turn))
      (user-error "Request input is empty"))
    (setf (superchat-turn-origin turn) origin)
    (superchat--attach-turn-memories turn full-input session-id)
    (let* ((prepared (superchat--with-chat-execution-scope
                       (superchat-core-prepare-turn turn)))
           (skill (superchat-turn-skill prepared))
           (command (superchat-turn-command prepared))
           (superchat-execution-scope
            (if (and (null skill)
                     (boundp 'superchat--active-preset)
                     superchat--active-preset)
                (superchat--apply-active-preset prepared)
              superchat-execution-scope)))
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
                    (superchat-turn-target-model prepared)))
             ;; Capture once, including history and attachments.  Sending
             ;; reuses the system fragments and the exact final body instead
             ;; of collecting into an already assembled body a second time.
             (preview (let ((superchat--preview-origin-data origin-data))
                        (superchat--preview-chat-prompt prepared template))))
        ;; The body is the inspector's exact editable text.  The assembled
        ;; system is display-only: sending it as the base system would add
        ;; every system collector a second time.  Keep :system-prompt as the
        ;; original Turn/Preset contribution for ordinary pipeline assembly.
        (setq plan (plist-put plan :prompt (plist-get preview :prompt)))
        (setq plan (plist-put plan :preview-system-prompt
                              (plist-get preview :system)))
        (setq plan (plist-put plan :chat-prepared-context
                              (plist-get preview :prepared-context)))
        (setq plan (plist-put plan :input raw-input))
        (setq plan (plist-put plan :clean-input raw-input))
        (setq plan (plist-put plan :user-message raw-input))
        ;; The frozen source snapshot the preview above was assembled with.
        ;; The send that follows this inspection reuses it verbatim rather
        ;; than capturing again.
        (setq plan (plist-put plan :emacs-origin-data origin-data))
        (setq plan
              (plist-put plan :consume-pending-memories
                         (and superchat--pending-recalled-memories t)))
        plan))))

(defun superchat--prepare-generated-request (instruction)
  "Prepare an explicit producer INSTRUCTION without borrowing the user draft.
Reuse the ordinary Turn preparation and request builder, but leave queued
manual recall and prompt ownership untouched.  Submission is separate."
  (let ((turn (superchat-turn-new instruction superchat--session-id)))
    (setf (superchat-turn-origin turn) '(:producer plan))
    ;; Exclude the queue for this Turn only.  Hooks/search may reenter ordinary
    ;; sending, whose access to and consumption of its queue must remain real.
    (superchat--attach-indexed-memories turn instruction superchat--session-id)
    (let ((prepared (superchat--with-chat-execution-scope
                     (superchat-core-prepare-turn turn))))
      (let ((superchat-execution-scope
             (if (and (null (superchat-turn-skill prepared))
                      (bound-and-true-p superchat--active-preset))
                 (superchat--apply-active-preset prepared)
               superchat-execution-scope)))
        (superchat--request-plan-from-turn
         prepared
         (when superchat--current-command
           (superchat--lookup-command-template superchat--current-command))
         (superchat-turn-target-model prepared))))))

;; ── A refused immediate send gives the draft back ─────────────────────────
;;
;; A command or a skill always takes `superchat--send-input-now', which
;; consumes the visible draft -- read-only properties, the prompt boundary,
;; the response region -- before the driver has decided anything.  When
;; Runtime refuses that submission (`:status drop'), the request lifecycle
;; still requires the producer's draft to stay available, so the send records
;; what it consumed, the submission seam records the driver's answer, and one
;; settle after the dispatch puts the draft back.  Plain input never arms a
;; transaction: its own queueing branch consumes only after an accepted
;; enqueue.

(defun superchat--send-transaction-arm (input)
  "Record what a refused send of INPUT must restore.
Called before the `let*' that binds `superchat--send-transaction' consumes
the draft -- by `superchat--send-input-now' or by
`superchat--execute-plan-in-chat'.  The prompt boundary and the end of the
draft are copies, so the busy Run's own streaming -- which lands above the
prompt, never in the draft -- moves them with the text they point at."
  (let* ((live superchat--prompt-start)
         (prompt (and (markerp live) (copy-marker live nil)))
         (owner (if (bound-and-true-p superchat--runtime-transcript-owner)
                    superchat--runtime-transcript-owner
                  :pending))
         (response-state (cdr (assq owner superchat--response-region-states)))
         ;; `superchat--response-region-consume-prompt' detaches the prompt
         ;; from the state that owns the live boundary, falling back to its
         ;; owner-derived state; mirror both so the restore re-attaches the
         ;; boundary to exactly that state.
         (prompt-state
          (or (cl-loop for entry in superchat--response-region-states
                       for candidate = (cdr entry)
                       when (eq live (plist-get candidate :prompt-marker))
                       return candidate)
              response-state)))
    (list :view (current-buffer)
          :input input
          :prompt prompt
          :pre-max (copy-marker (point-max) nil)
          :prompt-state prompt-state
          :assistant-end (let ((marker (plist-get prompt-state :assistant-end)))
                           (and (markerp marker) (copy-marker marker nil)))
          :response-state response-state
          :status-marker (plist-get response-state :status-marker)
          :recalled superchat--pending-recalled-memories
          :command (and (boundp 'superchat--current-command)
                        superchat--current-command)
          :submissions nil)))

(defun superchat--send-transaction-record (result)
  "Record driver RESULT for the armed send, then return RESULT unchanged.
`superchat--send-input-now' and `superchat--execute-plan-in-chat' arm a
transaction around the submission each is about to make; every other
producer that crosses this seam may have none, so this is a no-op for it.
A deferred submission (nil) is recorded like any other answer: only a
`:status drop' result is a refusal, and treating the seam's own documented
nil as one would undo a send that was never refused.

`superchat--send-transaction' is a bare `defvar' that is unbound outside
those arms, so \"no transaction armed\" reaches here as an UNBOUND special,
not as nil -- and reading it would signal `void-variable' for producers
outside both paths (the plan producer's automatic submission, for example).
Asking whether one is armed has to tolerate its absence."
  (when (bound-and-true-p superchat--send-transaction)
    (setq superchat--send-transaction
          (plist-put superchat--send-transaction :submissions
                     (cons result (plist-get superchat--send-transaction
                                             :submissions)))))
  result)

(defun superchat--send-refusal-p (result)
  "Return RESULT when the driver refused it: `:status drop' and no Run.
A Run handle, a `queue'/`steer'/`process' admission, a deferred nil and a
local result are all not refusals."
  (and (listp result)
       (eq (plist-get result :status) 'drop)
       (null (plist-get result :run-id))
       result))

(defun superchat--send-transaction-restore (transaction refusals)
  "Put TRANSACTION's View back the way it was before its refused send.
REFUSALS are that send's `:status drop' answers, newest first; the newest
one's `:reason' picks the notice, so a retired Scope keeps the wording the
queueing branch already uses."
  (let ((view (plist-get transaction :view))
        (prompt (plist-get transaction :prompt))
        (pre-max (plist-get transaction :pre-max))
        (state (plist-get transaction :prompt-state))
        (response-state (plist-get transaction :response-state)))
    (when (buffer-live-p view)
      (with-current-buffer view
        (let ((inhibit-read-only t)
              (superchat-buffer-name (buffer-name view))
              (prompt-pos (and (markerp prompt) (marker-position prompt)))
              (pre-max-pos (and (markerp pre-max) (marker-position pre-max))))
          ;; 1. Everything the attempt appended after the draft: the newline
          ;; `superchat--prepare-for-response' adds, the status line a
          ;; mode-switch command writes, an attachment notice, and any prompt
          ;; header inserted while the attempt ran.
          (when (and pre-max-pos (> (point-max) pre-max-pos))
            (delete-region pre-max-pos (point-max)))
          ;; 2. The draft is a draft again, not a sent read-only user row.
          (when (and prompt-pos pre-max-pos (< prompt-pos pre-max-pos))
            (remove-text-properties prompt-pos pre-max-pos
                                    '(read-only nil superchat-role nil)))
          ;; 3. Re-attach the boundary the attempt detached.  The original
          ;; marker was destroyed, so a fresh marker at the recorded position
          ;; stands in before `superchat-send-input''s `unwind-protect' can
          ;; insert a second prompt.
          (when prompt-pos
            (let ((marker (copy-marker prompt-pos nil)))
              (when state (plist-put state :prompt-marker marker))
              (setq superchat--prompt-start marker)))
          ;; 4. The response-region boundary and status the attempt replaced.
          (let ((end (plist-get transaction :assistant-end)))
            (when (and state (markerp end)
                       (markerp (plist-get state :assistant-end)))
              (set-marker (plist-get state :assistant-end)
                          (marker-position end) nil)))
          (if response-state
              (progn
                (superchat--response-region--detach-state-marker
                 response-state :status-marker)
                (plist-put response-state :status-marker
                           (plist-get transaction :status-marker)))
            ;; No response state existed when the send was armed, so the one
            ;; `superchat--prepare-for-response' created is entirely the
            ;; attempt's and its status marker sits at or after the draft's
            ;; old end.
            (dolist (entry superchat--response-region-states)
              (let ((marker (plist-get (cdr entry) :status-marker)))
                (when (and (markerp marker) (marker-position marker)
                           pre-max-pos (>= (marker-position marker) pre-max-pos))
                  (superchat--response-region--detach-state-marker
                   (cdr entry) :status-marker)))))
          (when state
            (superchat--response-region--select-state state))
          ;; 5. The View-local state the attempt changed.
          (setq superchat--pending-recalled-memories
                (plist-get transaction :recalled))
          (when (boundp 'superchat--current-command)
            (setq superchat--current-command (plist-get transaction :command)))
          (message
           "%s"
           (if (eq (plist-get (car refusals) :reason) 'scope-retired)
               "Preset changed while preparing the message; it is still in the prompt."
             "Could not queue the message; it is still in the prompt.")))))))

(defun superchat--send-transaction-settle ()
  "Undo the armed send when the driver refused every submission it made.
Called once, after the dispatch `cond' returned, so every marker and buffer
write the dispatch makes is already in place.  A send that started a Run is
never undone; a send that both started one and was refused is reported rather
than guessed at."
  (let ((transaction superchat--send-transaction))
    (when transaction
      (let* ((submissions (plist-get transaction :submissions))
             (refusals (delq nil (mapcar #'superchat--send-refusal-p
                                         submissions)))
             (started (cl-remove-if #'superchat--send-refusal-p submissions)))
        (cond
         ((null refusals) nil)
         ((null started)
          (superchat--send-transaction-restore transaction refusals))
         (t
          (message "superchat: draft left sent; one send both started a Run and was refused: %S"
                   submissions)))))))

(defun superchat--submit-composed-chat-request (composed)
  "Submit fully-composed chat COMPOSED through the driver (contract §4).

In the absence of an explicit enclosing execution Scope, COMPOSED's exact
producer buffer chooses the View-local Preset Scope.  A caller in another
chat View therefore cannot accidentally submit A's already-built request
through B's active Preset."
  (superchat-plugin-require-active-row 'chat)
  (let ((superchat-execution-scope
         (superchat--chat-execution-scope
          nil (plist-get composed :buffer))))
    (superchat-pipeline-submit (superchat--chat-envelope composed))))

(defun superchat--chat-envelope (composed)
  "Translate fully-composed chat COMPOSED into a Request envelope.
Since migration step 3 the chat prompt is assembled by the driver: the
envelope carries the parsed Turn (`:chat-turn'), the stripped `#file'
path (`:chat-file-ref'), an active command template (`:chat-template')
and a skill-fixed prompt (`:chat-prompt-override') for the chat
collectors instead of a pre-assembled prompt.  COMPOSED must
already have passed `superchat-extension-compose-request' with its Chat
Request buffer/Session identity fixed -- either `superchat--submit-chat-request'
own composition step (the ordinary immediate-submit path), or
`superchat--build-chat-request''s deferred composition being submitted now
by a caller other than the S05a follow-up queue (`superchat-regenerate' via
`superchat--superseded-resend').  This is the single chat submission path:
every chat Request that is submitted from the dispatcher, immediately or
after being built ahead of time, ends here.  The one exception is a queued
follow-up itself: Runtime's own `dispatch-next' (S05a) resubmits that frozen
Request verbatim through Runtime's low-level legacy entry point, not through
this function, because it cannot reach the pipeline without a Runtime
change.  ;; S13 removes that gap when other producers stop needing the
legacy bridge.

`:content' prefers `:clean-input' over `:input' (the same value today, kept
as two tiers for when a future Turn distinguishes them again), falling back
to a description of the attached files when a message is only a `#file'
reference -- clean-input is empty after stripping the reference, and this
codebase's `superchat--request-plan-from-turn' sets both `:input' and
`:clean-input' from the identical clean value, so neither alone can satisfy
the envelope's non-empty `:content' invariant for that case (caught by
`journey/c5-attach-file-inlines-and-removes-token').  This fallback never
reaches the model: the `:prepared-prompt' bypass (`superchat-pipeline.el')
replaces `:content''s own fragment whenever it is present, which it always
is on this path (built by `superchat-core-prepare-turn' before this
function runs); `:content' only still has to be a real, non-empty string to
pass validation and to seed the legacy Request's `:input' (Tape's
`intent.received' content).

`:media' is only the image subset of `:context-files' --
`superchat--image-attachment-info' already filters every current consumer of
that key (the model call's image attachment and the Tape attachment-recording
hook) down to exactly this subset, so the non-image files' *paths* are
carried informationally in `:context (:chat-context-files …)' while their
*content* is already inlined into `:prepared-prompt' by the existing
pre-S10 Turn-preparation flow -- nothing observable is lost.

`:input-core' is carried BOTH at the envelope's own top level and inside
`:runtime-extras': `superchat-select-tools-decision' (`superchat-llm.el',
out of this slice's write scope) reads `(plist-get REQUEST :input-core)'
off whatever it is called with, and the pipeline's `select-tools' stage
calls it with the ENVELOPE itself (`superchat-pipeline.el' line ~369) --
before `:runtime-extras' has been merged onto a legacy Request at all -- so
omitting the top-level copy silently drops the `ask_user' tool attachment
for a human-input Turn (caught by `journey/d14-policy-visible-small-tool-set').
`:input-core' is not a documented envelope field (contract §2.2's table),
but is not forbidden either; the top-level copy is redundant with the
`:runtime-extras' one only in the sense that both must currently exist for
their two different readers.

The View Sink's OWNER cannot be the eventual Run's id: that id does not
exist until Runtime allocates it inside its own low-level legacy submit
entry point, which this function calls only indirectly through the
pipeline, after the Sink already had to be built.  A fresh id is
pre-allocated here and passed both as the Sink's OWNER and, via
`:runtime-extras', as the legacy Request's `:transcript-owner' -- Runtime's
own default is `(or (plist-get request :transcript-owner) run-id)'
(`superchat-runtime.el:2002,2038'), so supplying
it explicitly keeps the Sink's response-region owner and the Run entry's
`:transcript-owner' in agreement, which `superchat-send-input''s admit/
active-Run match (C12/C12b) depends on."
  (let* ((buffer (plist-get composed :buffer))
         (session-id (plist-get composed :session-id))
         (clean-input (plist-get composed :clean-input))
         (context-files (plist-get composed :context-files))
         (content (cond
                   ((and (stringp clean-input) (not (string-empty-p clean-input)))
                    clean-input)
                   ((let ((input (plist-get composed :input)))
                      (and (stringp input) (not (string-empty-p input)) input)))
                   (context-files
                    (format "[attached: %s]"
                            (mapconcat #'file-name-nondirectory context-files ", ")))
                   (t "(empty)")))
         (images (cl-remove-if-not #'superchat--image-attachment-info context-files))
         (attachments
          (mapcar
           (lambda (file)
             (let ((info (superchat--image-attachment-info file)))
               (list :path file
                     :mime-type (plist-get info :mime-type)
                     :size (plist-get info :size)
                     :summary (superchat--request-attachment-description file))))
           images))
         (text-files (cl-set-difference context-files images :test #'equal))
         ;; A caller-supplied `:transcript-owner' on COMPOSED (a legacy-
         ;; shaped producer, e.g. a test driving `superchat--submit-chat-
         ;; request' directly with its own owner, or a future non-turn
         ;; producer) must win over the fresh id this function would
         ;; otherwise mint -- overriding it unconditionally silently
         ;; discarded any owner a caller already fixed (caught by
         ;; `output-p1'/`output-p2'/`output-context''s `:custom-owner'
         ;; cases in `test/test-llm-backend.el').
         (owner (or (plist-get composed :transcript-owner)
                    (format "chat-owner-%08x" (random #x100000000))))
         (input-core (plist-get composed :input-core))
         (extras
          (superchat-pipeline--plist-extend
           nil
           :input-core input-core
           ;; `superchat-runtime--turn-from-request' reads `:clean-input'
           ;; straight off the legacy Request to rebuild the private Turn
           ;; (falls back to `:user-message'/`:input' only when absent);
           ;; `--dispatch-to-runtime''s own fixed field list does not carry
           ;; it, so it must ride along as an extra or the reconstructed
           ;; Turn's `clean-input' silently reverts to the unstripped
           ;; `:input' value (caught by
           ;; `skill/mixed-input-uses-clean-prompt-and-echoes-skipped-command').
           :clean-input clean-input
           :workspace-id (plist-get composed :workspace-id)
           :workspace-root (plist-get composed :workspace-root)
           ;; A prepared (inspected) request's selection is the one its plan
           ;; saved, read by `superchat-pipeline--context-envelope'; the
           ;; legacy extra carries none for it.
           :context-envelope
           (unless (plist-get composed :chat-prepared-context)
             (plist-get composed :context-envelope))
           :lang (plist-get composed :lang)
           :accepted-function (plist-get composed :accepted-function)
           :owner-stop-function (plist-get composed :owner-stop-function)
           :actor-id (plist-get composed :actor-id)
           :correlation-id (plist-get composed :correlation-id)
           ;; Runtime's legacy submit path reads `:preset' to resolve
           ;; capability/topology naming and to pass the Preset object
           ;; itself to `superchat--llm-generate-answer' (max-tokens,
           ;; reasoning, temperature overrides); `composed :preset' is set
           ;; by `superchat--execute-llm-query' whenever the Turn carries
           ;; one but was never forwarded here (caught by
           ;; `test-send-input-inherits-active-preset-tools').
           :preset (plist-get composed :preset)
           :max-tokens (plist-get composed :max-tokens)
           :transcript-owner owner)))
    (superchat-pipeline--plist-extend
      (list :content content
            :surface 'chat
            :sink (superchat-render-view-sink buffer session-id owner)
            :session-id session-id
            ;; Preparation sees the contract default, never an explicit nil
            ;; (S10-fix-2 finding 4).
            :tools (plist-get composed :tools)
            :media images
            :context
            (append
             (list :chat-buffer buffer
                            ;; S12: the chat composed its legacy Request
                            ;; already; marking the envelope keeps the
                            ;; driver's composition idempotent.
                            :chat-lang (plist-get composed :lang)
                            :chat-context-files text-files
                            :chat-turn (plist-get composed :chat-turn)
                            :chat-file-ref (let ((turn (plist-get composed :chat-turn)))
                                             (and turn (plist-get (superchat-turn-state turn) :file-ref)))
                            :chat-template (plist-get composed :chat-template)
                            :chat-prepared-context
                            (plist-get composed :chat-prepared-context)
                            :chat-prompt-override
                            (let ((prompt (plist-get composed :prompt)))
                              (and (stringp prompt) (not (string-empty-p prompt)) prompt)))
             ;; The source this request speaks about: the associated live
             ;; object plus the data-only snapshot, when the plan carries
             ;; one.  Absent -- not empty -- for a request with no source.
             ;; The collector renders it, so it is never appended to the
             ;; user's own text and never reaches a system slot.
             (superchat--origin-context composed))
            :capability (plist-get composed :capability)
            :topology (plist-get composed :topology)
            :origin (plist-get composed :origin)
            :target-model (plist-get composed :target-model)
            :system-prompt (plist-get composed :system-prompt)
            :deadline (plist-get composed :deadline)
            :attachments attachments
            :runtime-extras extras)
      :input-core input-core)))

(defun superchat--submit-chat-request (request &optional submit-p)
  "Compose and submit producer-approved Chat REQUEST.

The producer's exact buffer and Session identity are fixed before Extension
composition.  Extensions may enrich the Request, but cannot retarget either
identity.  Both the producer pair and the final pair cross the single
EditorWorkspace validation authority before Runtime effects begin.
For automatic producers SUBMIT-P is a final, side-effect-free permission
check after composition (including activation recording); nil defers submission."
  (let* ((producer-buffer (or (plist-get request :buffer) (current-buffer)))
         (producer-session-id (let ((session (plist-get request :session-id)))
                                (if (stringp session) (substring-no-properties session) session)))
         (accepted-present (and (plist-member request :accepted-function) t))
         (stop-present (and (plist-member request :owner-stop-function) t))
         (owner-p (or accepted-present stop-present))
         (accepted (plist-get request :accepted-function))
         (stop (plist-get request :owner-stop-function))
         (input-present (and (plist-member request :input-core) t))
         (input-config (mapcar (lambda (value)
                                (if (stringp value) (substring-no-properties value) value))
                              (plist-get request :input-core)))
         (original-tools (mapcar (lambda (value)
                                  (if (stringp value) (substring-no-properties value) value))
                                (if (listp (plist-get request :tools))
                                    (plist-get request :tools))))
         (original-selection (or original-tools (plist-get request :tools)))
         (default-ask (and (eq (plist-get input-config :operation) 'ask-user)
                           (null original-selection) superchat-llm-tools-enabled))
         tools-admitted admitted-tools)
    (superchat-editor-workspace-validate-buffer
     producer-buffer producer-session-id)
    ;; Materialize the producer's implicit current View before composition so
    ;; omission cannot be interpreted later as Extension-selected identity.
    (plist-put request :buffer producer-buffer)
    (let* ((identity-guard
            (lambda (candidate)
              (unless (and (eq input-present (and (plist-member candidate :input-core) t))
                           (equal input-config (plist-get candidate :input-core))
                           (cl-every
                            (lambda (key)
                              (eq (plist-get input-config key)
                                  (plist-get (plist-get candidate :input-core) key)))
                            '(:presenter :stop-consumer :eligible-function)))
                (error "Extension cannot replace the producer input adapter"))
              (when input-present
                (if tools-admitted
                    (unless (equal admitted-tools (plist-get candidate :tools))
                      (error "Extension changed admitted input tool selection"))
                  (when (and original-selection
                             (not (equal original-selection (plist-get candidate :tools))))
                    (error "Extension cannot widen an explicit input tool profile"))
                  ;; Extension may materialize an originally unprofiled list.
                  ;; Add default ask before activation evidence, never to a
                  ;; user's restrictive profile or a globally disabled path.
                  (when (and default-ask (consp (plist-get candidate :tools)))
                    (plist-put candidate :tools
                               (delete-dups (append (plist-get candidate :tools) '("ask_user")))))
                  (setq admitted-tools
                        (if (listp (plist-get candidate :tools))
                            (mapcar (lambda (value) (substring-no-properties value))
                                    (plist-get candidate :tools))
                          (plist-get candidate :tools))
                        tools-admitted t)))
              (when owner-p
                (unless (and (eq accepted-present
                                 (and (plist-member candidate :accepted-function) t))
                             (eq stop-present
                                 (and (plist-member candidate :owner-stop-function) t))
                             (eq accepted (plist-get candidate :accepted-function))
                             (eq stop (plist-get candidate :owner-stop-function)))
                  (error "Extension cannot replace or remove producer owner callbacks")))
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
           (composed (progn (funcall identity-guard request) request)))
      (plist-put
       composed :buffer
       producer-buffer)
      ;; Activation recording can reenter after composition's acceptance gate.
      ;; Only opted-in producers acquire the extra check; ordinary composition
      ;; retains its existing behavior.
      (when (or owner-p input-present (plist-member composed :input-core))
        (funcall identity-guard composed))
      (if (or (null submit-p) (funcall submit-p))
          (superchat--send-transaction-record
           (superchat--submit-composed-chat-request composed))
        ;; A deferring SUBMIT-P (S05b's follow-up queue) still needs the
        ;; fully composed request to enqueue; no current caller inspects
        ;; this branch's previous nil return.
        composed))))

(defun superchat--build-chat-request (input)
  "Compose, but do not submit, a chat request plist for INPUT.
Mirrors the plain-LLM-query path of `superchat--send-input-now' (turn
creation, memory attach, pre/post-topic hooks, `superchat-core-prepare-turn',
active-command-mode template, preset inheritance, then
`superchat--request-plan-from-turn') and composes it through
`superchat--submit-chat-request' with a SUBMIT-P that always defers, so the
returned plist is the fully composed request, never submitted.  Used by
`superchat-send-input' to build the item Runtime's follow-up queue later
submits verbatim through Runtime's own low-level legacy submit entry point
(S05a) -- that queued path is the one chat submission Runtime resubmits
directly rather than through `superchat--submit-composed-chat-request' (S10);
see that function's docstring.

Because that later, automatic submission happens outside this buffer's
dynamic scope, the composed request's `:accepted-function' closes over this
View so the eventual Run is still tracked by `superchat--track-active-run'
\(documented here since Runtime's legacy submit entry point invokes
`:accepted-function' generically for every submission, immediate or queued --
no Runtime change was needed to carry this handle back to the View)."
  (let* ((view (current-buffer))
         (session superchat--session-id)
         (lang superchat-lang)
         (turn (superchat-turn-new input session)))
    (superchat--attach-turn-memories turn input session t)
    (let* ((prepared (superchat--with-chat-execution-scope
                       (superchat-core-prepare-turn turn))))
      (let* ((target-model (superchat-turn-target-model prepared))
             (superchat-execution-scope
              (if (and (null (superchat-turn-skill prepared))
                       (boundp 'superchat--active-preset)
                       superchat--active-preset)
                  (superchat--apply-active-preset prepared)
                superchat-execution-scope)))
        (let* ((template (when superchat--current-command
                           (superchat--lookup-command-template
                            superchat--current-command)))
               (plan (superchat--request-plan-from-turn
                      prepared template target-model)))
          (plist-put plan :lang lang)
          (plist-put plan :accepted-function
                     (lambda (handle)
                       (when (buffer-live-p view)
                         (with-current-buffer view
                           (superchat--track-active-run handle)))))
          (superchat--submit-chat-request
           (superchat--with-supported-input plan 'response)
           (lambda () nil)))))))

(defun superchat-execute-request-plan (plan)
  "Execute the exact inspectable request PLAN and return PLAN."
  (unless (eq (plist-get plan :type) :llm-query)
    (user-error "Not an executable LLM request plan"))
  (when (plist-get plan :consume-pending-memories)
    (setq superchat--pending-recalled-memories nil))
  (plist-put plan :run-handle
             (superchat--submit-chat-request
              (superchat--with-supported-input plan 'response)))
  plan)

(defun superchat--handle-command (command args input &optional lang _target-model)
  "Dispatch COMMAND with ARGS, INPUT, and LANG through commands Service.

When the provider is absent, do not bypass it through a stale process-global
table: a user prompt template remains the intentional graceful fallback."
  (superchat--with-chat-execution-scope
    (let* ((scope (superchat-execution-scope-resolve nil))
           ;; Do not take a UI helper dependency here: this public dispatcher
           ;; crosses the Kernel Service seam directly and treats provider
           ;; absence as an ordinary route to the user-template fallback.
           (provider (superchat-service-get scope 'commands))
           (handler (and provider
                         (superchat-service-call scope 'commands 'lookup command))))
      (or
       ;; A handler may intentionally return nil, preserving the original
       ;; `or'-default contract.  Only after it declined do we load a same-name
       ;; prompt file and ask the independent template fallback to handle it.
       (and handler (funcall handler command args input lang nil))
       (progn
         (superchat--ensure-command-loaded command)
         (superchat--handle-default-command command args input lang))))))

(defun superchat--handle-default-command (command args input lang)
  "Handle COMMAND with ARGS, INPUT, and LANG that wasn't covered by hooks.
This is the catch-all fallback after the hook chain."
  (cond
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
  "Send the visible draft, or queue it while ordinary chat is active."
  (interactive)
  (superchat-plugin-require-active-row 'chat)
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
      (let* ((parsed (superchat--with-chat-execution-scope
                       (superchat-core--parse-input
                        (superchat-turn-new input session))))
             (command (superchat-turn-command parsed))
             ;; Same match rule as before: the plain-input (no command, no
             ;; skill) gate against an active Run in this Session/View.
             (active-status
              (and (bound-and-true-p superchat-mode)
                   (null command) (null (superchat-turn-skill parsed))
                   (cl-find-if
                    (lambda (status)
                      (and (equal session (plist-get status :session-id))
                           ;; S10: the chat envelope now sets the contract
                           ;; §2.1 producer surface `chat' explicitly, so a
                           ;; chat Run's public status carries `:surface
                           ;; 'chat', not the pre-S10 implicit default
                           ;; `'superchat' (`superchat-runtime--surface').
                           ;; `superchat' is kept for any caller still on
                           ;; the legacy default.
                           (memq (plist-get status :surface) '(chat superchat))
                           ;; The current projection also covers synchronous
                           ;; UI reentry before submit returns its tracking
                           ;; handle.
                           (or (equal (plist-get status :run-id)
                                      (bound-and-true-p superchat--active-run-id))
                               (equal (plist-get status :transcript-owner)
                                      superchat--response-region-current-owner))
                           (not (eq (plist-get status :topology) 'workflow))))
                    (plist-get (superchat-runtime-status-projection) :active-runs))))
             (queue-p (and active-status t)))
        (cond
         ;; This command owns its confirmation and exact View transaction.
         ;; Dispatch it before any prompt, recall, timing or text mutation.
         ((equal command "clear")
          (superchat--dispatch-result
           (superchat--handle-command command (superchat-turn-command-args parsed)
                                      (superchat-turn-clean-input parsed)
                                      superchat-lang (superchat-turn-target-model parsed))
           superchat-lang (superchat-turn-target-model parsed)))
         (queue-p
          ;; The recalled-memory payload `superchat--build-chat-request'
          ;; would otherwise consume unconditionally must survive a
          ;; rejected enqueue exactly as captured -- restored below, never
          ;; touched at all on success (S05b-fix-2 item 1).
          (let ((superchat-execution-scope
                 ;; A queue transaction captures one Scope before any parse/
                 ;; `pre' contribution can reenter and activate a different
                 ;; Preset.  The built Envelope and FIFO item must remain one
                 ;; request under that old Scope; retirement then rejects it
                 ;; rather than silently retargeting it to the replacement.
                 (superchat--chat-execution-scope nil view)))
            (let* ((recalled-memories superchat--pending-recalled-memories)
                   ;; The queued item is the same Envelope an immediate send
                   ;; would submit; Runtime's `dispatch-next' hands it to the
                   ;; driver Service (one prompt path, step 3).
                   (request
                    (condition-case nil
                        (superchat--chat-envelope
                         (superchat--build-chat-request input))
                      ;; A normal producer pre hook may activate a replacement
                      ;; Preset.  Its old Turn cannot join this FIFO under the
                      ;; replacement Scope, but this is a rejected enqueue,
                      ;; not an error which escapes after the user pressed send.
                      (superchat-preset-request-scope-retired
                       :scope-retired)))
                   (enqueue-result
                    (unless (eq request :scope-retired)
                      (superchat-runtime-enqueue-follow-up session request))))
              (if (stringp enqueue-result)
                  (progn
                    (let ((inhibit-read-only t))
                      (put-text-property prompt (point-max) 'read-only t)
                      (put-text-property prompt (point-max) 'superchat-role 'user))
                    (superchat--response-region-consume-prompt)
                    (overlay-put (make-overlay (point-max) (point-max))
                                 'after-string
                                 (propertize " ⏳ queued" 'face 'superchat-status-muted-face))
                    (superchat--response-region-ensure-view-prompt)
                    (message "Queued (%d pending); it will be sent when the current response finishes."
                             (superchat-runtime-pending-follow-ups session)))
                ;; Rejected (Tape append failed or the captured Scope was
                ;; retired): the original prompt text and marker are untouched
                ;; above -- nothing was locked, consumed, or marked queued --
                ;; so simply restore the recalled-memory payload and report
                ;; the failure; the draft is still there.
                (setq superchat--pending-recalled-memories recalled-memories)
                (message (if (eq request :scope-retired)
                             "Preset changed while preparing the message; it is still in the prompt."
                           "Could not queue the message; it is still in the prompt."))))))
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
      ;; The immediate path consumes the visible draft below, before the
      ;; driver has decided anything.  Record what it consumes so a refused
      ;; submission (`:status drop') can put the draft back; the settle at the
      ;; end of this `let*' runs it.
      (let* ((superchat--send-transaction (superchat--send-transaction-arm input))
             (turn (superchat-turn-new input superchat--session-id))
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
        (let* ((prepared (superchat--with-chat-execution-scope
                           (superchat-core-prepare-turn turn))))
          (let* ((command (superchat-turn-command prepared))
                 (skill (superchat-turn-skill prepared))
                 (target-model (superchat-turn-target-model prepared))
                 (clean-input (superchat-turn-clean-input prepared))
                 (superchat--current-turn prepared)
                 ;; A caller that explicitly supplied an execution Scope keeps
                 ;; it even when a command changes this View's Preset.  With
                 ;; no explicit binding, a mode-switch result is deliberately
                 ;; routed again below through the View's newly active Scope.
                 (caller-execution-scope superchat-execution-scope)
                 (superchat-execution-scope
                  (if (and (null skill)
                           (boundp 'superchat--active-preset)
                           superchat--active-preset)
                      (superchat--apply-active-preset prepared)
                    caller-execution-scope)))
            (superchat--ttft-log "core-pipeline")
          (dolist (notice
                   (plist-get (superchat-turn-state prepared)
                              :attachment-notices))
            (superchat--insert-persistent-system-message notice))
          (cond
           ;; Workflow invocation: >>name [args] (or >workflow <name> [args])
           ;; Core parsing rewrote either form to the reserved `workflow'
           ;; skill token with "<name> [args]" left in clean-input.  The
           ;; name may be quoted; the rest is passed as $input using the
           ;; same parser as /workflow.  The async executor renders output, so
           ;; nothing is returned to dispatch.
           ((and skill (string= skill "workflow")
                 (fboundp 'superchat-workflow-execute-async))
            ;; The row owns the feature: a `>>` message with the workflow
            ;; row unmounted is refused before parsing, existence checks or
            ;; any Run.
            (superchat-plugin-require-active-row 'workflow)
            ;; The coordinator is a local command; each executable workflow
            ;; step owns its lifecycle through Runtime.
            (let* ((rest (string-trim (or clean-input "")))
                   (invocation (superchat-workflow--parse-invocation rest))
                   (name (car invocation))
                   (args (cdr invocation)))
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
                   (result
                    (when (fboundp 'superchat-skills-invoke)
                      (superchat--with-chat-execution-scope
                        (superchat-skills-invoke skill clean-input prepared)))))
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
              ;; A command such as `/plan ARGS' intentionally installs its
              ;; selected Preset before returning this mode-switch request.
              ;; Its lookup/handler above still ran in the old Scope, but the
              ;; new request must not inherit that retired Scope.  A caller's
              ;; explicit Scope remains authoritative, including explicit
              ;; global, so only an otherwise unscoped View is refreshed.
              (if (eq (plist-get result :type) :llm-query-and-mode-switch)
                  (let ((superchat-execution-scope caller-execution-scope))
                    (superchat--dispatch-result result lang target-model))
                (superchat--dispatch-result result lang target-model))))
           ;; Active command mode
           (superchat--current-command
            (let ((template (superchat--lookup-command-template
                             superchat--current-command)))
              (superchat--dispatch-llm-or-agent prepared lang target-model template)))
           ;; Default: plain LLM query
           (t
            (superchat--dispatch-llm-or-agent prepared lang target-model)))))
        ;; The dispatch returned: undo a send the driver refused.  Settling
        ;; here sees every marker and buffer write the dispatch made.
        (superchat--send-transaction-settle))))))

(defun superchat--dispatch-llm-or-agent (turn lang target-model &optional template)
  "Submit TURN through Runtime using optional TEMPLATE."
  ;; Deliberately resolve at this final producer seam rather than around the
  ;; whole input dispatch: a `>agent' skill may have installed a new Preset
  ;; after parse, and its request must not inherit the now-retired old Scope.
  (superchat--with-chat-execution-scope
    (let ((plan (superchat--request-plan-from-turn
                 turn template target-model)))
      (plist-put plan :lang lang)
      (let ((executed (superchat-execute-request-plan plan)))
        ;; Only a real Run handle may become the View's tracked Run: a refused
        ;; submission answers with an admission plist that carries no
        ;; `:run-id', and recording that would forget the Run still running.
        (when (and (fboundp 'superchat--track-active-run)
                   (stringp (plist-get (plist-get executed :run-handle) :run-id)))
          (superchat--track-active-run (plist-get executed :run-handle)))
        executed))))

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
     (let ((run-handle (superchat--submit-chat-request
                        (superchat--with-supported-input result 'response))))
       ;; A refusal (`:status drop') has no `:run-id': it must not clear the
       ;; View's record of the Run that is still busy.
       (if (and (fboundp 'superchat--track-active-run)
                (stringp (plist-get run-handle :run-id)))
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
            (prepared (superchat--with-chat-execution-scope
                        (superchat-core-prepare-turn turn)))
            (superchat-execution-scope
             (if (and (boundp 'superchat--active-preset)
                      superchat--active-preset)
                 (superchat--apply-active-preset prepared)
               superchat-execution-scope))
            (plan (superchat--request-plan-from-turn
                   prepared template target-model)))
       (plist-put plan :buffer-name (buffer-name))
       (plist-put plan :lang lang)
       (let ((executed (superchat-execute-request-plan plan)))
         ;; Same guard as the other two sites: a refused submission's handle
         ;; carries no `:run-id' and must not clear the tracked busy Run.
         (when (and (fboundp 'superchat--track-active-run)
                    (stringp (plist-get (plist-get executed :run-handle)
                                        :run-id)))
           (superchat--track-active-run
            (plist-get executed :run-handle)))
         executed)))))

(provide 'superchat-dispatcher)
;;; superchat-dispatcher.el ends here
