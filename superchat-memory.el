;;; superchat-memory.el --- SQLite-backed memory facade for Superchat -*- lexical-binding: t; -*-

;; Author: Superchat contributors
;; Keywords: ai, chat, memory

;;; Commentary:
;; This module owns the materialized Memory projection and its compatibility
;; facade.  Capture and review facts enter Tape first, then the projection is
;; updated through `superchat-projection-catch-up'.
;;
;; Storage moved from org-mode (`memory.org', `soul.org') to SQLite in
;; v0.8.  The org-only complexity — org-ql search, RELATED multi-hop
;; BFS, keyword LLM enrichment, contradiction pairing, mood taxonomy,
;; ACCESS_COUNT decay, soul synthesis — was deleted.  External
;; consumers see the same function names and (compatible) call sites.
;;
;; Public API preserved:
;;   superchat-memory-compose-title
;;   superchat-memory-capture-explicit
;;   superchat-memory-capture-conversation
;;   superchat-memory-auto-capture
;;   superchat-memory-review
;;   superchat-memory-correct
;;   superchat-memory-retrieve
;;   superchat-memory-summarize-session-history
;;   superchat-memory-prune
;;   superchat-memory-add-raw         (kept as a thin tape-append shim)
;;   superchat-memory-import-from-org (one-shot migration helper)
;;
;; Anything else from v0.7 is gone.  If you depended on `-retrieve-async',
;; `-retrieve-with-context', `-discover-keyword-associations',
;; `-expand-with-related', `-synthesize-soul', `-llm-keyword-enricher',
;; the mood taxonomy, or `superchat-memory-soul-file', update your code
;; to the surviving API.

;;; Code:

(require 'cl-lib)
(require 'superchat-plugin)
(require 'superchat-service)
(require 'subr-x)
(require 'org nil t)             ; org-with-wide-buffer etc (optional)
(require 'superchat-db)
(require 'superchat-projection)
(require 'superchat-tape)

(defvar superchat-data-directory)        ; from superchat.el
(defvar superchat-llm-backend)           ; from superchat.el
(defvar superchat--session-id)            ; buffer-local in superchat.el
(declare-function superchat-db-open                 "superchat-db" ())
(declare-function superchat-db-memory-insert        "superchat-db" (content &rest args))
(declare-function superchat-db-memory-get-by-id      "superchat-db" (memory-id))
(declare-function superchat-db-memory-get-by-source-event "superchat-db" (event-id))
(declare-function superchat-db-memory-set-review     "superchat-db" (memory-id status &optional replaced-by-id))
(declare-function superchat-db-memory-reset-projection "superchat-db" ())
(declare-function superchat-db-memory-search        "superchat-db" (query &optional limit))
(declare-function superchat-db-memory-search-simple "superchat-db" (query &optional limit))
(declare-function superchat-db-memory-count         "superchat-db" (&optional review-status))
(declare-function superchat-db-memory-delete-rejected-older-than "superchat-db" (days))
(declare-function superchat-db-tape-select          "superchat-db" (sql &optional params))
(declare-function superchat-projection-catch-up     "superchat-projection" (name &optional scope))
(declare-function superchat-tape-append-event       "superchat-tape" (event))

(defgroup superchat-memory nil
  "SQLite-backed long-term memory for Superchat."
  :group 'superchat)

(defcustom superchat-memory-auto-capture-enabled t
  "When non-nil, `superchat-memory-auto-capture' attempts implicit captures.

The current implementation is conservative: implicit memory writes that
used to call the LLM on the response hot path have been removed.
Implicit capture now only runs when the input matches one of
`superchat-memory-explicit-trigger-patterns'.  Anything else must be
captured by the user via `/remember' or by registering a custom
capture hook."
  :type 'boolean
  :group 'superchat-memory)

(defcustom superchat-memory-auto-capture-minimum-length 40
  "Minimum trimmed character count before auto-capture considers a turn."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-explicit-trigger-patterns
  '("\\`记住"
    "\\`记下"
    "\\`remember\\b"
    "\\`note this"
    "请记住"
    "请记下")
  "Regexps that mark a user turn as an explicit memory request.
Matched case-insensitively against the trimmed user input.  When any
pattern matches, `superchat-memory-auto-capture' stores the turn."
  :type '(repeat regexp)
  :group 'superchat-memory)

(defcustom superchat-memory-title-max-length 78
  "Maximum length for synthesized titles."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-prune-rejected-days 30
  "Delete rejected memory rows older than this many days during prune.
Set to 0 or negative to disable the rejected-row sweep."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-legacy-writes-enabled t
  "When non-nil, `/remember' also updates the Memory projection.
The command always writes its Tape anchor.  Set this to nil when Tape search
alone is preferred over the compatibility Memory recall path."
  :type 'boolean
  :group 'superchat-memory)

(defcustom superchat-memory-retrieve-limit 5
  "Default upper bound on rows returned by `superchat-memory-retrieve'."
  :type 'integer
  :group 'superchat-memory)

(defcustom superchat-memory-auto-recall-min-length 8
  "Minimum trimmed input length before auto-recall is attempted.
Below this width, the user message is treated as too terse to benefit
from memory retrieval (e.g. \"ok\", \"thanks\")."
  :type 'integer
  :group 'superchat-memory)

;;;; ── Helpers ────────────────────────────────────────────────────────

(defun superchat-memory--sanitize (s)
  "Trim S and collapse internal whitespace."
  (when (and s (stringp s))
    (let ((trimmed (string-trim s)))
      (replace-regexp-in-string "[ \t\n\r]+" " " trimmed))))

(defun superchat-memory--truncate (s n)
  "Truncate string S to at most N chars, with ellipsis."
  (if (and s (> (length s) n))
      (concat (substring s 0 (max 1 (- n 1))) "…")
    s))

(defun superchat-memory--row-to-plist (row)
  "Normalize a SQLite row to a plist matching the v0.7 retrieval shape.
ROW layout (from `superchat-db-memory-search' / `-search-simple'):
  (id title content keywords mood review_status created_at . _rest)."
  (when row
    (list :id         (nth 0 row)
          :title      (or (nth 1 row) "")
          :content    (or (nth 2 row) "")
          :keywords   (or (nth 3 row) "")
          :mood       (or (nth 4 row) "")
          :tags       ""
          :type       "memory"
          :timestamp  (or (nth 6 row) ""))))

(defun superchat-memory--extract-text (exchange)
  "Pick the most informative text from EXCHANGE.
EXCHANGE may be a string or a plist with :input / :user / :content."
  (cond
   ((stringp exchange) exchange)
   ((and (consp exchange) (plist-member exchange :input))
    (or (plist-get exchange :input) ""))
   ((and (consp exchange) (plist-member exchange :user))
    (or (plist-get exchange :user) ""))
   ((and (consp exchange) (plist-member exchange :content))
    (or (plist-get exchange :content) ""))
   (t (format "%S" exchange))))

(defun superchat-memory--session-id ()
  "Return the current Session id for a Memory event."
  (or (and (boundp 'superchat--session-id) superchat--session-id)
      "memory"))

(defun superchat-memory--event-meta (row)
  "Decode the event envelope from materialization ROW."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string (or (nth 5 row) "{}"))))

(defun superchat-memory--target-row (payload)
  "Resolve the Memory row identified by review event PAYLOAD."
  (or (when-let* ((event-id (alist-get 'target_event_id payload)))
        (superchat-db-memory-get-by-source-event event-id))
      (when-let* ((memory-id (alist-get 'target_memory_id payload)))
        (superchat-db-memory-get-by-id memory-id))
      (error "Memory review target no longer exists")))

(defun superchat-memory--projection-select (since-id _scope)
  "Return Memory events newer than SINCE-ID."
  (superchat-db-tape-select
   "SELECT id, session_id, topic, kind, content, meta, created_at
    FROM tape
    WHERE topic = 'memory' AND id > ?
    ORDER BY id ASC"
   (list since-id)))

(defun superchat-memory--projection-apply (_state row)
  "Apply Memory event ROW to the materialized Memory table."
  (let* ((storage-id (car row))
         (meta (superchat-memory--event-meta row))
         (event-id (or (alist-get 'event_id meta) storage-id))
         (type (alist-get 'event_type meta))
         (payload (alist-get 'payload meta)))
    (pcase type
      ("memory.captured"
       (unless (superchat-db-memory-get-by-source-event event-id)
         (superchat-db-memory-insert
          (or (alist-get 'content payload) "")
          :title (alist-get 'title payload)
          :keywords (alist-get 'keywords payload)
          :source-tape-ids (list storage-id)
          :source-event-ids (list event-id)
          :mood (alist-get 'mood payload)
          :status (or (alist-get 'status payload) "pending")
          :projection-version 1
          :model-id (alist-get 'model_id payload)
          :prompt-hash (alist-get 'prompt_hash payload))))
      ((or "memory.reviewed" "memory.rejected")
       (let ((target (superchat-memory--target-row payload)))
         (superchat-db-memory-set-review
          (car target)
          (if (equal type "memory.rejected") "rejected" "accepted"))))
      ("memory.corrected"
       (let* ((target (superchat-memory--target-row payload))
              (replacement
               (or (superchat-db-memory-get-by-source-event event-id)
                   (progn
                     (superchat-db-memory-insert
                      (or (alist-get 'content payload) "")
                      :title (alist-get 'title payload)
                      :keywords (alist-get 'keywords payload)
                      :source-tape-ids (list storage-id)
                      :source-event-ids (list event-id)
                      :mood (alist-get 'mood payload)
                      :status "accepted"
                      :projection-version 1
                      :model-id (alist-get 'model_id payload)
                      :prompt-hash (alist-get 'prompt_hash payload))
                     (superchat-db-memory-get-by-source-event event-id)))))
         (superchat-db-memory-set-review
          (car target) "accepted" (car replacement)))))
    nil))

(defun superchat-memory--projection-reset (_scope)
  "Reset only rows owned by the Memory projection."
  (superchat-db-memory-reset-projection))


(cl-defun superchat-memory--capture
    (content title keywords mood status
             &key model-id prompt-hash session-id remember-bridge)
  "Record and materialize one Memory fact."
  (let* ((record
          (superchat-tape-append-event-record
           (list :type "memory.captured"
                 :session-id (or session-id (superchat-memory--session-id))
                 :topic "memory"
                 :summary "memory.captured"
                 :sensitivity "confidential"
                 :requires-payload t
                 :payload
                 (append
                  (list :content content
                        :title title
                        :keywords keywords
                        :mood mood
                        :status status
                        :model_id model-id
                        :prompt_hash prompt-hash)
                  (when remember-bridge
                    (list :remember_bridge remember-bridge))))))
         (event-id (plist-get record :event-id))
         (_checkpoint (superchat-projection-catch-up 'memory))
         (row (superchat-db-memory-get-by-source-event event-id)))
    (or (car row)
        (error "Memory event %s was not materialized" event-id))))

(defun superchat-memory--review-target (row)
  "Return a durable target plist for Memory ROW."
  (let* ((projection-version (or (nth 10 row) 0))
         (source-events
          (and (> projection-version 0)
               (ignore-errors
                 (let ((json-array-type 'list))
                   (json-read-from-string (or (nth 9 row) "[]"))))))
         (source-event (car source-events)))
    (if source-event
        (list :target_event_id source-event)
      (list :target_memory_id (car row)))))

;;;; ── Public API: title / write paths ───────────────────────────────

(defun superchat-memory-compose-title (content)
  "Compose a one-line title from CONTENT.
Returns a string bounded by `superchat-memory-title-max-length'."
  (let ((normalized (superchat-memory--sanitize content)))
    (or (and normalized
             (superchat-memory--truncate
              normalized superchat-memory-title-max-length))
        "Untitled")))

(cl-defun superchat-memory-capture-explicit
    (content &optional title
             &key tags type keywords related remember-session-id
             remember-anchor-id remember-source-id)
  "Store CONTENT as an accepted memory.  Return the new row id.
TITLE defaults to `superchat-memory-compose-title'.
TAGS, TYPE, RELATED are accepted for back-compat but only TAGS / TYPE
land in the keywords column.  KEYWORDS is honoured directly.
The three REMEMBER-* values record a bridge only when they form a complete
Session, Anchor storage id, and source storage id relation."
  (ignore related)
  (let* ((clean (or (superchat-memory--sanitize content) ""))
         (title (or title (superchat-memory-compose-title clean)))
         (kw (cond
              ((stringp keywords) keywords)
              ((listp keywords)   (mapconcat #'identity keywords ","))
              (t                  "")))
         (tag-str (cond
                   ((stringp tags) tags)
                   ((listp tags)   (mapconcat #'identity tags ","))
                   (t              "")))
         (combined (if (and (not (string-empty-p kw))
                            (not (string-empty-p tag-str)))
                       (concat kw "," tag-str)
                     (concat kw tag-str))))
    (when (> (length clean) 0)
      (superchat-memory--capture
       clean title combined (or type "") "accepted"
       :session-id remember-session-id
       :remember-bridge
       (when (and (stringp remember-session-id)
                  (not (string-empty-p remember-session-id))
                  (integerp remember-anchor-id) (> remember-anchor-id 0)
                  (integerp remember-source-id) (> remember-source-id 0))
         (list :relation_type "remember-anchor-v1"
               :session_id remember-session-id
               :anchor_storage_id remember-anchor-id
               :source_storage_id remember-source-id))))))

(cl-defun superchat-memory-capture-conversation (exchange &key tier tags type
                                                          keywords related
                                                          access-count)
  "Store EXCHANGE as an accepted memory.
TIER is accepted for back-compat (was used by the v0.6 dual-track flow)
but no longer changes behaviour; both Tier 1 and Tier 3 land in the
same memory table.  TAGS / TYPE / KEYWORDS / RELATED forwarded to
`superchat-memory-capture-explicit'.  ACCESS-COUNT is ignored — the
decay model that depended on it was removed."
  (ignore tier access-count)
  (let ((content (superchat-memory--extract-text exchange)))
    (when (and content (> (length (string-trim content)) 0))
      (superchat-memory-capture-explicit
       content nil
       :tags tags :type type :keywords keywords :related related))))

(defun superchat-memory-auto-capture (exchange)
  "Optionally capture EXCHANGE based on explicit-trigger patterns.
Honors `superchat-memory-auto-capture-enabled' and the length floor.
Returns the new row id on capture, nil otherwise."
  (when superchat-memory-auto-capture-enabled
    (let* ((text (superchat-memory--extract-text exchange))
           (trimmed (and text (string-trim text))))
      (when (and trimmed
                 (>= (length trimmed)
                     superchat-memory-auto-capture-minimum-length)
                 (let ((case-fold-search t))
                   (cl-some (lambda (rx) (string-match-p rx trimmed))
                            superchat-memory-explicit-trigger-patterns)))
        (superchat-memory-capture-explicit trimmed)))))

(cl-defun superchat-memory-add-raw (content &key mood context verbatim tags type)
  "Append CONTENT with CONTEXT and optional MOOD, TAGS, TYPE as raw Tape event.
The v0.6 `soul.org' raw-event store is gone.  Callers that used to
write \"raw\" events now feed them into the Runtime event contract.
Return the Tape row id."
  (ignore verbatim)
  (when (and content
             (> (length (string-trim content)) 0))
    (superchat-tape-append-event
     (list :type "memory.raw.added"
           :session-id (superchat-memory--session-id)
           :topic "memory/raw"
           :summary "memory.raw.added"
           :sensitivity "confidential"
           :requires-payload t
           :payload
           (list :content content
                 :mood (or mood "")
                 :context (or context "")
                 :tags (cond
                        ((stringp tags) tags)
                        ((listp tags) (mapconcat #'identity tags ","))
                        (t ""))
                 :type (or type ""))))))

(defun superchat-memory-review (memory-id status)
  "Accept or reject MEMORY-ID through an auditable Tape event.
STATUS must be `accepted' or `rejected'.  Return MEMORY-ID."
  (let* ((status (if (symbolp status) (symbol-name status) status))
         (row (superchat-db-memory-get-by-id memory-id)))
    (unless row
      (error "Unknown Memory id: %s" memory-id))
    (unless (member status '("accepted" "rejected"))
      (error "Memory review status must be accepted or rejected"))
    (superchat-tape-append-event
     (list :type (if (equal status "rejected")
                     "memory.rejected"
                   "memory.reviewed")
           :session-id (superchat-memory--session-id)
           :topic "memory"
           :summary (format "memory.%s: %s" status memory-id)
           :payload (superchat-memory--review-target row)))
    (superchat-projection-catch-up 'memory)
    memory-id))

(defun superchat-memory-correct (memory-id content &optional title)
  "Replace MEMORY-ID with corrected CONTENT through a Tape event.
Return the new materialized Memory id."
  (let* ((row (superchat-db-memory-get-by-id memory-id))
         (clean (or (superchat-memory--sanitize content) "")))
    (unless row
      (error "Unknown Memory id: %s" memory-id))
    (when (string-empty-p clean)
      (error "Corrected Memory content must not be empty"))
    (let* ((record
            (superchat-tape-append-event-record
             (list :type "memory.corrected"
                   :session-id (superchat-memory--session-id)
                   :topic "memory"
                   :summary (format "memory.corrected: %s" memory-id)
                   :sensitivity "confidential"
                   :requires-payload t
                   :payload
                   (append
                    (superchat-memory--review-target row)
                    (list :content clean
                          :title (or title
                                     (superchat-memory-compose-title clean))
                          :keywords (or (nth 3 row) "")
                          :mood (or (nth 4 row) "")
                          :model_id (or (nth 11 row) "")
                          :prompt_hash (or (nth 12 row) ""))))))
           (event-id (plist-get record :event-id))
           (_checkpoint (superchat-projection-catch-up 'memory))
           (replacement
            (superchat-db-memory-get-by-source-event event-id)))
      (or (car replacement)
          (error "Memory correction %s was not materialized" event-id)))))

;;;; ── Public API: read paths ────────────────────────────────────────

(defun superchat-memory-retrieve (query-string)
  "Return accepted memories matching QUERY-STRING as a list of plists.
Uses FTS5 first; falls back to LIKE when FTS5 returns nothing
\(common for very short queries or non-ASCII tokens)."
  (when (and (stringp query-string)
             (not (string-empty-p (string-trim query-string))))
    (superchat-projection-catch-up 'memory)
    (let* ((limit superchat-memory-retrieve-limit)
           (rows (or (ignore-errors
                       (superchat-db-memory-search query-string limit))
                     (superchat-db-memory-search-simple query-string limit))))
      (delq nil (mapcar #'superchat-memory--row-to-plist rows)))))

(defun superchat-memory--json-value (text)
  "Decode JSON TEXT into symbol-keyed alists and lists."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (json-read-from-string text)))

(defun superchat-memory--tape-row-at (storage-id)
  "Return the full Tape row at positive STORAGE-ID, or nil."
  (when (and (integerp storage-id) (> storage-id 0))
    (car
     (superchat-db-tape-select
      "SELECT id, session_id, topic, kind, content, meta, created_at
       FROM tape WHERE id = ?"
      (list storage-id)))))

(defun superchat-memory-verified-remember-anchor-id (memory recall-session-id)
  "Return MEMORY's verified remember Anchor id for RECALL-SESSION-ID.
Any absent, malformed, inconsistent, or unreadable provenance returns nil."
  (condition-case nil
      (let* ((memory-id (plist-get memory :id))
             (memory-row
              (and (integerp memory-id) (> memory-id 0)
                   (superchat-db-memory-get-by-id memory-id)))
             (capture-ids
              (and memory-row
                   (superchat-memory--json-value (or (nth 8 memory-row) "[]"))))
             (event-ids
              (and memory-row
                   (superchat-memory--json-value (or (nth 9 memory-row) "[]"))))
             (capture-id (and (= 1 (length capture-ids)) (car capture-ids)))
             (event-id (and (= 1 (length event-ids)) (car event-ids)))
             (capture-row (superchat-memory--tape-row-at capture-id))
             (capture-meta
              (and capture-row
                   (superchat-memory--json-value (or (nth 5 capture-row) "{}"))))
             (payload (and capture-meta (alist-get 'payload capture-meta)))
             (relation (and payload (alist-get 'remember_bridge payload)))
             (relation-session (and relation (alist-get 'session_id relation)))
             (anchor-id (and relation (alist-get 'anchor_storage_id relation)))
             (source-id (and relation (alist-get 'source_storage_id relation)))
             (anchor-row (superchat-memory--tape-row-at anchor-id))
             (source-row (superchat-memory--tape-row-at source-id))
             (anchor-meta
              (and anchor-row
                   (superchat-memory--json-value (or (nth 5 anchor-row) "{}"))))
             (anchor-sources (and anchor-meta
                                  (alist-get 'source_ids anchor-meta))))
        (when (and memory-row
                   (= memory-id (car memory-row))
                   (integerp capture-id) (> capture-id 0)
                   (stringp event-id) (not (string-empty-p event-id))
                   (equal "memory.captured" (alist-get 'event_type capture-meta))
                   (equal event-id (alist-get 'event_id capture-meta))
                   (equal "memory" (nth 2 capture-row))
                   (equal "remember-anchor-v1"
                          (alist-get 'relation_type relation))
                   (stringp recall-session-id)
                   (not (string-empty-p recall-session-id))
                   (equal relation-session recall-session-id)
                   (equal relation-session (nth 1 capture-row))
                   (equal relation-session (alist-get 'session_id capture-meta))
                   (integerp anchor-id) (> anchor-id 0)
                   (integerp source-id) (> source-id 0)
                   (< source-id anchor-id) (< anchor-id capture-id)
                   anchor-row source-row
                   (equal relation-session (nth 1 anchor-row))
                   (equal relation-session (nth 1 source-row))
                   (equal "anchor" (nth 3 anchor-row))
                   (equal "remember" (nth 2 anchor-row))
                   (listp anchor-sources)
                   (member source-id anchor-sources))
          anchor-id))
    (error nil)))

;;;; ── Maintenance ───────────────────────────────────────────────────

(defun superchat-memory-prune (&optional silent)
  "Delete legacy rejected Memory rows older than the configured horizon.
Projected rows remain available for deterministic rebuild.
Runs SILENTly (no output) when SILENT is non-nil.
Honours `superchat-memory-prune-rejected-days'."
  (interactive)
  (let ((days superchat-memory-prune-rejected-days))
    (when (and (integerp days) (> days 0))
      (ignore-errors
        (superchat-db-memory-delete-rejected-older-than days)))
    (unless silent
      (message "superchat-memory: pruned legacy rejected rows older than %d days."
               days))))

(defun superchat-memory-summarize-session-history (history-content)
  "Compatibility shim — stash HISTORY-CONTENT as a pending memory row.

In v0.7 this returned an llm.el request that summarized the session on
restart.  That LLM call was on the startup hot path and blocked the
chat buffer.  v0.8 simply persists the raw history as a pending-review
memory so the user can decide later whether to keep it.

Returns a truthy value (the new row id) on success so existing callers
that check the return as a boolean continue to work."
  (when (and history-content
             (stringp history-content)
             (> (length (string-trim history-content)) 0))
    (ignore-errors
      (superchat-memory--capture
       history-content
       (superchat-memory-compose-title
        (concat "Session " (format-time-string "%Y-%m-%d %H:%M")))
       "" "" "pending"))))

;;;; ── One-shot org → sqlite import ──────────────────────────────────

;;;###autoload
(defun superchat-memory-import-from-org (&optional org-file)
  "Import legacy `memory.org' entries into the SQLite memory table.
ORG-FILE defaults to memory.org under `superchat-data-directory'.
Each top-level heading becomes one accepted memory row with its
title, body, and any KEYWORDS / TAGS property preserved as keywords.
Idempotent only at the file level — re-running on the same file
will create duplicates.  Return the number of rows inserted."
  (interactive)
  (let* ((file (or org-file
                   (expand-file-name "memory.org" superchat-data-directory)))
         (count 0))
    (unless (file-exists-p file)
      (user-error "No file at %s" file))
    (require 'org)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^\\* " nil t)
         (let* ((title (org-get-heading t t t t))
                (props (org-entry-properties))
                (content (save-excursion
                           (org-back-to-heading t)
                           (org-end-of-meta-data t)
                           (buffer-substring-no-properties (point)
                                                           (or (outline-next-heading)
                                                               (point-max)))))
                (keywords (or (cdr (assoc "KEYWORDS" props)) ""))
                (tags     (or (cdr (assoc "TAGS" props))     "")))
           (when (and content (> (length (string-trim content)) 0))
             (superchat-memory--capture
              (string-trim content)
              title
              (if (string-empty-p tags) keywords
                (concat keywords "," tags))
              ""
              "accepted")
             (cl-incf count))))))
    (when (called-interactively-p 'interactive)
      (message "superchat-memory: imported %d row(s) from %s" count file))
    count))

;;;###autoload
(defun superchat-memory-migrate-to-tape ()
  "Migrate accepted rows from the legacy `memory' table to tape anchors.
Each accepted memory becomes an `anchor' entry on the tape with the
memory's title, keywords, and creation timestamp preserved in meta.
Returns the number of anchors created."
  (interactive)
  (superchat-projection-catch-up 'memory)
  (let ((memories (superchat-db-memory-list-review "accepted" 10000))
        (count 0)
        (sid "memory-migration"))
    (dolist (mem memories)
      (let* ((id (nth 0 mem))
             (title (or (nth 1 mem) ""))
             (content (or (nth 2 mem) ""))
             (keywords (or (nth 3 mem) ""))
             (created (or (nth 6 mem) "")))
        (when (and content (not (string-empty-p content)))
          (superchat-tape-append-material
           sid "anchor" content
           :meta `((source . "memory")
                   (memory_id . ,id)
                   (title . ,title)
                   (keywords . ,keywords)
                   (created_at . ,created))
           :topic "migrated-memory")
          (cl-incf count))))
    (when (called-interactively-p 'interactive)
      (message "superchat-memory: migrated %d row(s) to tape anchors" count))
    count))

;; ── Plugin (spec §3): the default provider defined next to its code ──
(declare-function superchat-db-tape-search-prioritized "superchat-db")
(declare-function superchat-db-memory-list-review "superchat-db")
(superchat-define-plugin memory
  :inject '(tape)
  :doc "Memory rows: insert, search, review."
  :apply
  (lambda (scope _)
    (superchat-projection-register
     'memory
     :version 1
     :apply #'superchat-memory--projection-apply
     :select #'superchat-memory--projection-select
     :reset #'superchat-memory--projection-reset)
    (superchat-service-provide
     scope 'memory
     (superchat-service-table
      insert (lambda (&rest args) (apply #'superchat-db-memory-insert args))
      search #'superchat-db-memory-search
      search-prioritized #'superchat-db-tape-search-prioritized
      get #'superchat-db-memory-get-by-id
      list-review #'superchat-db-memory-list-review
      set-review #'superchat-db-memory-set-review
      count #'superchat-db-memory-count
      reset-projection #'superchat-db-memory-reset-projection))))

(provide 'superchat-memory)

;;; superchat-memory.el ends here
