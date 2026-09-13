;;; superchat-db.el — SQLite storage backend for Superchat -*- lexical-binding: t; -*-

;;; Commentary:
;; Replaces org-mode file storage for tape (raw events) and memory
;; (extracted facts) with SQLite + FTS5 full-text search.
;;
;; Soul (identity document) stays as org-mode file — it is human-authored.
;;
;; Requires Emacs 29+ (built-in sqlite support — no package needed).
;;
;; Schema:
;;   tape    — append-only conversation event log
;;   memory  — extracted facts with FTS5 index
;;   Soul    — org file (not in this module)

;;; Code:

(require 'cl-lib)
(require 'json)

(defvar superchat-data-directory
  (expand-file-name "superchat/" user-emacs-directory))

;; ═══════════════════════════════════════════════════════════
;; Database path & connection
;; ═══════════════════════════════════════════════════════════

(defvar superchat-db--connection nil
  "SQLite database connection for tape + memory storage.")

(defvar superchat-db--path nil
  "Filesystem path to the SQLite database file.")

(defcustom superchat-db-busy-timeout 5000
  "Milliseconds SQLite waits for a busy database before failing.
Larger values reduce transient concurrent-write failures but can block
longer; smaller values fail faster but expose more `SQLITE_BUSY' errors."
  :type 'natnum
  :group 'superchat)

(defun superchat-db--path ()
  "Return the default SQLite database path."
  (expand-file-name "superchat.db" superchat-data-directory))

(defun superchat-db-open (&optional db-path)
  "Open (or return existing) SQLite connection at DB-PATH.
When called without argument, uses the default path under
`superchat-data-directory'.  Idempotent — returns the same
connection on repeated calls.  Ensures schema on first open."
  (let ((path (or db-path (superchat-db--path))))
    (when (or (not superchat-db--connection)
              (not (equal superchat-db--path path)))
      (when superchat-db--connection
        (ignore-errors (sqlite-close superchat-db--connection)))
      (let ((dir (file-name-directory path)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (setq superchat-db--connection (sqlite-open path))
      (setq superchat-db--path path)
      ;; Enable WAL for concurrent reads
      (sqlite-execute superchat-db--connection "PRAGMA journal_mode=WAL")
      (sqlite-execute
       superchat-db--connection
       (format "PRAGMA busy_timeout = %d" superchat-db-busy-timeout))
      (sqlite-execute superchat-db--connection "PRAGMA foreign_keys=ON")
      ;; Create schema on first open
      (superchat-db--ensure-schema))
    superchat-db--connection))

(defun superchat-db-close ()
  "Close the SQLite connection and clean up."
  (when superchat-db--connection
    (ignore-errors (sqlite-close superchat-db--connection))
    (setq superchat-db--connection nil)
    (setq superchat-db--path nil)))

;; ═══════════════════════════════════════════════════════════
;; Schema migration
;; ═══════════════════════════════════════════════════════════

(defconst superchat-db--schema-version 4
  "Current schema version.  Used for auto-migration.

History:
  1 — initial tape + memory + FTS5 (unicode61 tokenizer; broken for CJK).
  2 — FTS5 switched to trigram tokenizer.  Trigram indexes any
      3+ character substring, which lets CJK queries hit without
      explicit word segmentation.  Caveat: queries shorter than
      3 characters fall through to LIKE.
  3 — Added `topic' column to tape and a tape_fts full-text index
      over tape.content.  Tape is now searchable without relying on
      the separate memory table.
  4 — Added projection checkpoints and Memory projection provenance.")

(defun superchat-db--current-schema-version (db)
  "Read the recorded schema version from DB, or 0 if the table is missing/empty."
  (or (caar (ignore-errors
              (sqlite-select db "SELECT version FROM _schema_version LIMIT 1")))
      0))

(defun superchat-db--ensure-schema ()
  "Create tables and indexes if they don't exist.
The whole body is wrapped in BEGIN/COMMIT because Emacs's
`sqlite-execute' does not durably commit DDL by default — without
the explicit transaction, CREATE/DROP statements run inside the open
connection but are lost on close.  Observed reliably on Emacs 29 +
sqlite3 3.47.

This function is called from `superchat-db-open' after a fresh
connection is established.  It must NOT call `superchat-db-open'
back to fetch a handle — at the point we run, `superchat-db--connection'
is set but `superchat-db--path' may differ from the default path
\(e.g. tests that open a custom DB file), and re-entering
`superchat-db-open' with the default path would reopen against the
wrong file and migrate the wrong DB."
  (let ((db superchat-db--connection))
    (sqlite-execute db "BEGIN")
    (condition-case err
        (progn
          (superchat-db--ensure-schema-1 db)
          (sqlite-execute db "COMMIT"))
      (error
       (ignore-errors (sqlite-execute db "ROLLBACK"))
       (signal (car err) (cdr err))))))

(defun superchat-db--ensure-schema-1 (db)
  "Inner schema-creation body for DB — must run inside a transaction."
  ;; ── Base tables (must exist before migrations reference them) ──
  ;; ponytail: Keep event-envelope fields in `meta'; promote and index them
  ;; only when measured query volume makes JSON extraction a bottleneck.
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS tape (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      session_id TEXT    NOT NULL,
      topic      TEXT,
      kind       TEXT    NOT NULL CHECK(kind IN ('user','assistant','tool_call','tool_result','anchor','system')),
      content    TEXT    NOT NULL,
      meta       TEXT    DEFAULT '{}',
      created_at TEXT    NOT NULL DEFAULT (datetime('now'))
    )")
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS memory (
      id              INTEGER PRIMARY KEY AUTOINCREMENT,
      title           TEXT,
      content         TEXT    NOT NULL,
      keywords        TEXT    DEFAULT '',
      source_tape_ids TEXT    DEFAULT '[]',
      source_event_ids TEXT   DEFAULT '[]',
      mood            TEXT    DEFAULT '',
      replaced_by     INTEGER REFERENCES memory(id),
      review_status   TEXT    DEFAULT 'pending'
                         CHECK(review_status IN ('pending','accepted','rejected')),
      projection_version INTEGER NOT NULL DEFAULT 0,
      model_id        TEXT    DEFAULT '',
      prompt_hash     TEXT    DEFAULT '',
      created_at      TEXT    NOT NULL DEFAULT (datetime('now')),
      updated_at      TEXT    NOT NULL DEFAULT (datetime('now'))
    )")
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS _schema_version (version INTEGER PRIMARY KEY)")
  (sqlite-execute
   db
   "CREATE TABLE IF NOT EXISTS projection_checkpoint (
      projection_name TEXT    NOT NULL,
      scope           TEXT    NOT NULL DEFAULT '',
      version         INTEGER NOT NULL,
      last_event_id   INTEGER NOT NULL DEFAULT 0,
      updated_at      TEXT    NOT NULL DEFAULT (datetime('now')),
      PRIMARY KEY (projection_name, scope)
    )")

  ;; ── Schema version + targeted migrations ──
  ;; Run migrations BEFORE creating indexes/FTS tables that depend on
  ;; migrated columns (e.g. v2→v3 adds `topic' and `tape_fts').  Without
  ;; this ordering, an existing v2 database hits "no such column: topic"
  ;; when creating idx_tape_topic, aborts the transaction, and never
  ;; creates tape_fts, leaving later queries to fail with "no such table:
  ;; tape_fts".
  (let ((current (superchat-db--current-schema-version db)))
    (unless (equal current superchat-db--schema-version)
      ;; v1 → v2: rebuild memory_fts with the trigram tokenizer.
      ;; The CREATE VIRTUAL TABLE IF NOT EXISTS below is skipped when an old
      ;; unicode61-tokenized table already exists, so we drop and
      ;; recreate explicitly, then reindex from the authoritative
      ;; `memory' table.  Persistence comes from the BEGIN/COMMIT
      ;; in the caller (`superchat-db--ensure-schema').
      (when (< current 2)
        (sqlite-execute db "DROP TABLE IF EXISTS memory_fts")
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE memory_fts USING fts5(
            content, keywords, title,
            content=memory, content_rowid=id,
            tokenize='trigram'
          )")
        (sqlite-execute
         db
         "INSERT INTO memory_fts(rowid, content, keywords, title)
          SELECT id, content, keywords, title FROM memory"))
      ;; v2 → v3: add topic column to existing tape tables and create tape_fts.
      (when (< current 3)
        (ignore-errors
          (sqlite-execute db "ALTER TABLE tape ADD COLUMN topic TEXT"))
        (ignore-errors
          (sqlite-execute db "DROP TABLE IF EXISTS tape_fts"))
        (sqlite-execute
         db
         "CREATE VIRTUAL TABLE tape_fts USING fts5(
            content,
            content=tape, content_rowid=id,
            tokenize='trigram'
          )")
        (sqlite-execute
         db
         "INSERT INTO tape_fts(rowid, content)
          SELECT id, content FROM tape"))
      ;; v3 → v4: projection provenance and durable checkpoints.
      (when (< current 4)
        (dolist
            (statement
             '("ALTER TABLE memory ADD COLUMN source_event_ids TEXT DEFAULT '[]'"
               "ALTER TABLE memory ADD COLUMN projection_version INTEGER NOT NULL DEFAULT 0"
               "ALTER TABLE memory ADD COLUMN model_id TEXT DEFAULT ''"
               "ALTER TABLE memory ADD COLUMN prompt_hash TEXT DEFAULT ''"
               "ALTER TABLE memory ADD COLUMN updated_at TEXT"))
          ;; Fresh databases already received these columns from CREATE TABLE.
          (ignore-errors (sqlite-execute db statement)))
        (sqlite-execute
         db
         "UPDATE memory SET updated_at = COALESCE(updated_at, created_at)"))
      ;; DELETE + INSERT guarantees a single authoritative row.  `INSERT OR
      ;; REPLACE' would add a new row when the existing version differs
      ;; from the target version, leaving stale rows behind; `LIMIT 1'
      ;; could then return the old version and re-trigger migrations.
      (sqlite-execute db "DELETE FROM _schema_version")
      (sqlite-execute
       db "INSERT INTO _schema_version (version) VALUES (?)"
       (list superchat-db--schema-version))))

  ;; ── Indexes (depend on migrated columns) ──
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_session ON tape(session_id)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_topic ON tape(topic)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_kind ON tape(kind)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_created ON tape(created_at)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_mood ON memory(mood)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_review ON memory(review_status)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_created ON memory(created_at)")
  (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_projection ON memory(projection_version)")

  ;; ── FTS5 full-text indexes — trigram tokenizer.  This is the only
  ;; tokenizer in shipping SQLite that works for CJK without
  ;; per-language word segmentation: it indexes every 3-character
  ;; substring of the columns, so `MATCH '编辑器'` hits anywhere the
  ;; substring appears.  Tradeoff: queries shorter than 3 characters
  ;; can't be served from FTS5 and must fall back to LIKE.
  (sqlite-execute
   db
   "CREATE VIRTUAL TABLE IF NOT EXISTS memory_fts USING fts5(
      content, keywords, title,
      content=memory, content_rowid=id,
      tokenize='trigram'
    )")

  ;; Triggers to keep FTS5 in sync
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS memory_ai AFTER INSERT ON memory BEGIN
      INSERT INTO memory_fts(rowid, content, keywords, title)
      VALUES (new.id, new.content, new.keywords, new.title);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS memory_ad AFTER DELETE ON memory BEGIN
      INSERT INTO memory_fts(memory_fts, rowid, content, keywords, title)
      VALUES('delete', old.id, old.content, old.keywords, old.title);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS memory_au AFTER UPDATE ON memory BEGIN
      INSERT INTO memory_fts(memory_fts, rowid, content, keywords, title)
      VALUES('delete', old.id, old.content, old.keywords, old.title);
      INSERT INTO memory_fts(rowid, content, keywords, title)
      VALUES (new.id, new.content, new.keywords, new.title);
    END")

  ;; ── Tape FTS5 full-text index ──
  (sqlite-execute
   db
   "CREATE VIRTUAL TABLE IF NOT EXISTS tape_fts USING fts5(
      content,
      content=tape, content_rowid=id,
      tokenize='trigram'
    )")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS tape_ai AFTER INSERT ON tape BEGIN
      INSERT INTO tape_fts(rowid, content) VALUES (new.id, new.content);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS tape_ad AFTER DELETE ON tape BEGIN
      INSERT INTO tape_fts(tape_fts, rowid, content) VALUES('delete', old.id, old.content);
    END")
  (sqlite-execute
   db
   "CREATE TRIGGER IF NOT EXISTS tape_au AFTER UPDATE ON tape BEGIN
      INSERT INTO tape_fts(tape_fts, rowid, content) VALUES('delete', old.id, old.content);
      INSERT INTO tape_fts(rowid, content) VALUES (new.id, new.content);
    END"))

;; ═══════════════════════════════════════════════════════════
;; Projection transactions and checkpoints
;; ═══════════════════════════════════════════════════════════

(defun superchat-db-call-with-transaction (function)
  "Call FUNCTION inside one immediate SQLite transaction.
Roll back and re-signal when FUNCTION fails."
  (let ((db (superchat-db-open))
        committed)
    (sqlite-execute db "BEGIN IMMEDIATE")
    (unwind-protect
        (prog1 (funcall function)
          (sqlite-execute db "COMMIT")
          (setq committed t))
      (unless committed
        (ignore-errors (sqlite-execute db "ROLLBACK"))))))

(defun superchat-db-projection-checkpoint (name &optional scope)
  "Return NAME checkpoint for SCOPE as (VERSION LAST-STORAGE-ID)."
  (car
   (sqlite-select
    (superchat-db-open)
    "SELECT version, last_event_id
     FROM projection_checkpoint
     WHERE projection_name = ? AND scope = ?"
    (list (format "%s" name) (or scope "")))))

(defun superchat-db-projection-set-checkpoint
    (name version last-storage-id &optional scope)
  "Set projection NAME VERSION and LAST-STORAGE-ID for SCOPE."
  (sqlite-execute
   (superchat-db-open)
   "INSERT INTO projection_checkpoint
      (projection_name, scope, version, last_event_id, updated_at)
    VALUES (?, ?, ?, ?, datetime('now'))
    ON CONFLICT(projection_name, scope) DO UPDATE SET
      version = excluded.version,
      last_event_id = excluded.last_event_id,
      updated_at = excluded.updated_at"
   (list (format "%s" name) (or scope "") version last-storage-id)))

(defun superchat-db-projection-reset-checkpoint (name &optional scope)
  "Delete projection NAME checkpoint for SCOPE."
  (sqlite-execute
   (superchat-db-open)
   "DELETE FROM projection_checkpoint
    WHERE projection_name = ? AND scope = ?"
   (list (format "%s" name) (or scope ""))))

;; ═══════════════════════════════════════════════════════════
;; Tape operations
;; ═══════════════════════════════════════════════════════════

(cl-defun superchat-db-tape-append (session-id kind content &key (meta "{}") (topic nil))
  "Append an event to the tape.  Returns the new row id.
Optional TOPIC tags the entry for view grouping.  META is the row's metadata
already serialized as JSON text by the caller (`superchat-tape' owns the
payload rules the text must satisfy); this module stores the bytes it is
given and never re-encodes them."
  (let ((db (superchat-db-open)))
    (sqlite-execute
     db
     "INSERT INTO tape (session_id, topic, kind, content, meta) VALUES (?, ?, ?, ?, ?)"
     (list session-id (or topic "") kind content (or meta "{}")))
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun superchat-db-tape-replay (session-id &optional since-id limit)
  "Return tape entries for SESSION-ID, ordered by id ascending.
If SINCE-ID is given, only entries with id > SINCE-ID.
If LIMIT is given, return at most LIMIT entries."
  (let* ((db (superchat-db-open))
         (sql (if since-id
                  "SELECT id, topic, kind, content, meta, created_at FROM tape
                   WHERE session_id = ? AND id > ?
                   ORDER BY id ASC"
                "SELECT id, topic, kind, content, meta, created_at FROM tape
                 WHERE session_id = ?
                 ORDER BY id ASC"))
         (params (if since-id (list session-id since-id) (list session-id)))
         (rows (sqlite-select db sql params)))
    (if limit
        (cl-subseq rows 0 (min limit (length rows)))
      rows)))

(defun superchat-db-tape-last-anchor (session-id)
  "Return the last anchor entry for SESSION-ID, or nil."
  (let* ((db (superchat-db-open))
         (rows (sqlite-select
                db
                "SELECT id, topic, content, meta, created_at FROM tape
                 WHERE session_id = ? AND kind = 'anchor'
                 ORDER BY id DESC LIMIT 1"
                (list session-id))))
    (car rows)))

(defun superchat-db-tape-select (sql &optional params)
  "Run a parameterized SELECT/WITH SQL against the tape database.
PARAMS is a list of positional arguments.  Only read-only statements
starting with SELECT or WITH are allowed."
  (let* ((db (superchat-db-open))
         (stmt (string-trim (string-remove-suffix ";" (string-trim sql))))
         (upcased (upcase stmt)))
    (unless (string-match-p "\\`\\s-*\\(SELECT\\|WITH\\)\\b" upcased)
      (error "Only SELECT/WITH queries are allowed"))
    ;; A statement may START with WITH and still mutate ("WITH x AS
    ;; (...) INSERT INTO ..."), and sqlite-select would happily run it.
    ;; Word-scan for mutating verbs; a false positive on a quoted
    ;; keyword just means rephrasing the query.
    (when (string-match-p
           "\\b\\(INSERT\\|UPDATE\\|DELETE\\|DROP\\|CREATE\\|ALTER\\|REPLACE\\|ATTACH\\|DETACH\\|PRAGMA\\|VACUUM\\|REINDEX\\)\\b"
           upcased)
      (error "Only read-only SELECT/WITH queries are allowed"))
    (when (string-match-p ";" stmt)
      (error "Multi-statement queries are not allowed"))
    (sqlite-select db stmt params)))

(defun superchat-db--tape-search-match-query (query)
  "Return an FTS5 MATCH string OR-joining QUERY's word tokens, or nil.
Tokens shorter than 3 characters are dropped: `tape_fts' is indexed with
the `trigram' tokenizer (chosen so CJK content, which has no spaces,
still gets substring matches — see the schema comment above), and a
phrase shorter than 3 characters can never match anything under that
tokenizer.  Each surviving token is quoted as its own FTS5 phrase and
doubled-quote-escaped, so reserved FTS5 syntax inside QUERY (`NEAR',
`AND', `OR', `*', parentheses, quotes) is always treated as literal text,
never as query syntax.  A QUERY with no surviving tokens (e.g. pure
punctuation) returns nil, so the caller skips the query entirely instead
of asking FTS5 to match nothing."
  (let (tokens)
    (with-temp-buffer
      (insert query)
      (goto-char (point-min))
      (while (re-search-forward "\\w+" nil t)
        (let ((token (match-string 0)))
          (when (>= (length token) 3)
            (push token tokens)))))
    (when tokens
      (mapconcat
       (lambda (token)
         (concat "\"" (replace-regexp-in-string "\"" "\"\"" token) "\""))
       (nreverse tokens)
       " OR "))))

(defun superchat-db-tape-search (query &optional session-id limit)
  "Full-text search across tape entries using FTS5.
If SESSION-ID is non-nil, restrict to that session.  LIMIT defaults to 20.
QUERY is split into word tokens and OR-joined (see
`superchat-db--tape-search-match-query'), so a full sentence matches any
row that shares at least one word with it, ranked by FTS5 relevance
(`bm25'), most-recent first among equally relevant rows."
  (let* ((db (superchat-db-open))
         (limit (or limit 20))
         (match-query (superchat-db--tape-search-match-query query)))
    (when match-query
      (if session-id
          (sqlite-select
           db
           "SELECT t.id, t.session_id, t.topic, t.kind, t.content, t.created_at
            FROM tape t
            JOIN tape_fts fts ON t.id = fts.rowid
            WHERE t.session_id = ? AND tape_fts MATCH ?
            ORDER BY bm25(tape_fts) ASC, t.id DESC
            LIMIT ?"
           (list session-id match-query limit))
        (sqlite-select
         db
         "SELECT t.id, t.session_id, t.topic, t.kind, t.content, t.created_at
          FROM tape t
          JOIN tape_fts fts ON t.id = fts.rowid
          WHERE tape_fts MATCH ?
          ORDER BY bm25(tape_fts) ASC, t.id DESC
          LIMIT ?"
         (list match-query limit))))))

(defun superchat-db-tape-search-prioritized
    (query current-session-id limit &optional foreign-eligible-only exclude-ids
           current-session-only)
  "Full-text search QUERY across every session, CURRENT-SESSION-ID first.
Unlike `superchat-db-tape-search' with a nil SESSION-ID, session
priority, (when requested) foreign-row eligibility, and the
CURRENT-SESSION-ID recency exclusion (EXCLUDE-IDS) are all expressed in
the query itself, before LIMIT is applied -- so neither a current-Session
match nor an eligible foreign row can be starved out of the result by
unrelated foreign rows that merely rank better by relevance or recency.

Every row from CURRENT-SESSION-ID is eligible regardless of kind.  When
CURRENT-SESSION-ONLY is non-nil, no other session's rows are considered
at all (FOREIGN-ELIGIBLE-ONLY is ignored) -- this is the mode for
`superchat-memory-auto-recall-cross-session' `none', which still needs
its own-session EXCLUDE-IDS applied before LIMIT like every other mode.
Otherwise, when FOREIGN-ELIGIBLE-ONLY is non-nil, a row from any OTHER
session is eligible only when it is an explicit `/remember' anchor
(`kind' = \"anchor\", `topic' = \"remember\"); when nil, any matching
foreign row is eligible (Superchat's original, unrestricted
cross-session behaviour).

EXCLUDE-IDS, when non-nil, is a list of tape row ids to drop from the
result before LIMIT is applied (the caller's own-session recent-window
rows, already visible as ordinary conversation history).  It is passed
as a single JSON array bound parameter and matched with `t.id NOT IN
(SELECT value FROM json_each(?))', so an arbitrarily large window (a
whole long Session, thousands of ids) never hits SQLite's per-statement
bound-parameter limit the way one placeholder per id would.

Rows are ordered by session match first (CURRENT-SESSION-ID's own rows
before every other session's; moot when CURRENT-SESSION-ONLY is
non-nil), then by FTS5 relevance (`bm25'), then by recency.  Returns nil
when QUERY has no usable token (see
`superchat-db--tape-search-match-query')."
  (let* ((db (superchat-db-open))
         (limit (or limit 20))
         (match-query (superchat-db--tape-search-match-query query))
         (exclude-json (json-encode (or exclude-ids []))))
    (when match-query
      (cond
       (current-session-only
        (sqlite-select
         db
         "SELECT t.id, t.session_id, t.topic, t.kind, t.content, t.created_at
          FROM tape t
          JOIN tape_fts fts ON t.id = fts.rowid
          WHERE tape_fts MATCH ?
            AND t.session_id = ?
            AND t.id NOT IN (SELECT value FROM json_each(?))
          ORDER BY bm25(tape_fts) ASC, t.id DESC
          LIMIT ?"
         (list match-query current-session-id exclude-json limit)))
       (foreign-eligible-only
        (sqlite-select
         db
         "SELECT t.id, t.session_id, t.topic, t.kind, t.content, t.created_at
          FROM tape t
          JOIN tape_fts fts ON t.id = fts.rowid
          WHERE tape_fts MATCH ?
            AND (t.session_id = ? OR (t.kind = 'anchor' AND t.topic = 'remember'))
            AND t.id NOT IN (SELECT value FROM json_each(?))
          ORDER BY (t.session_id = ?) DESC, bm25(tape_fts) ASC, t.id DESC
          LIMIT ?"
         (list match-query current-session-id exclude-json current-session-id limit)))
       (t
        (sqlite-select
         db
         "SELECT t.id, t.session_id, t.topic, t.kind, t.content, t.created_at
          FROM tape t
          JOIN tape_fts fts ON t.id = fts.rowid
          WHERE tape_fts MATCH ?
            AND t.id NOT IN (SELECT value FROM json_each(?))
          ORDER BY (t.session_id = ?) DESC, bm25(tape_fts) ASC, t.id DESC
          LIMIT ?"
         (list match-query exclude-json current-session-id limit)))))))

;; ═══════════════════════════════════════════════════════════
;; Memory operations (extracted facts)
;; ═══════════════════════════════════════════════════════════

(cl-defun superchat-db-memory-insert
    (content &key title keywords source-tape-ids source-event-ids mood status
             (projection-version 0) model-id prompt-hash)
  "Insert CONTENT as a new memory entry.  Return the new row id.
STATUS defaults to \"accepted\" — direct user-driven inserts
\(`/remember', `capture-explicit') are trusted and should be
immediately retrievable.  Background / LLM-derived inserts can
pass STATUS \"pending\" to keep them out of recall until the
user reviews them.  SOURCE-EVENT-IDS and PROJECTION-VERSION identify
rows derived from Tape; the older SOURCE-TAPE-IDS field remains for
compatibility."
  (let ((db (superchat-db-open))
        (effective-status (or status "accepted"))
        (source-tape-json
         (if source-tape-ids
             (condition-case nil
                 (json-encode source-tape-ids)
               (error "[]"))
           "[]"))
        (source-event-json
         (if source-event-ids
             (condition-case nil
                 (json-encode source-event-ids)
               (error "[]"))
           "[]")))
    (sqlite-execute
     db
     "INSERT INTO memory
        (title, content, keywords, source_tape_ids, source_event_ids,
         mood, review_status, projection_version, model_id, prompt_hash,
         updated_at)
      VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime('now'))"
     (list (or title "")
           content
           (or keywords "")
           source-tape-json
           source-event-json
           (or mood "")
           effective-status
           projection-version
           (or model-id "")
           (or prompt-hash "")))
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun superchat-db-memory-delete-rejected-older-than (days)
  "Delete legacy rejected memory entries older than DAYS days.
Projected rows remain replayable audit material."
  (sqlite-execute (superchat-db-open)
                  "DELETE FROM memory
                   WHERE review_status = 'rejected'
                     AND projection_version = 0
                     AND created_at < datetime('now', ?)"
                  (list (format "-%d days" days))))

(defun superchat-db-memory-search (query &optional limit)
  "Search memory for QUERY using FTS5 full-text index.
Return entries ranked by relevance, most relevant first."
  (let* ((db (superchat-db-open))
         (limit (or limit 10)))
    (sqlite-select
     db
     "SELECT m.id, m.title, m.content, m.keywords, m.mood,
             m.review_status, m.created_at, m.replaced_by
      FROM memory m
      JOIN memory_fts fts ON m.id = fts.rowid
      WHERE memory_fts MATCH ?
        AND m.review_status = 'accepted'
        AND m.replaced_by IS NULL
      ORDER BY rank
      LIMIT ?"
     (list query limit))))

(defun superchat-db-memory-search-simple (query &optional limit)
  "Search for QUERY without FTS5 (fallback or broader matching).
Searches content, keywords, and title fields with LIKE."
  (let* ((db (superchat-db-open))
         (limit (or limit 10))
         (like-pat (concat "%" query "%")))
    (sqlite-select
     db
     "SELECT id, title, content, keywords, mood, review_status, created_at
      FROM memory
      WHERE (content LIKE ? OR keywords LIKE ? OR title LIKE ?)
        AND review_status = 'accepted'
        AND replaced_by IS NULL
      ORDER BY created_at DESC
      LIMIT ?"
     (list like-pat like-pat like-pat limit))))

(defun superchat-db-memory-get-by-id (memory-id)
  "Get a single memory entry by MEMORY-ID."
  (let* ((db (superchat-db-open))
         (rows (sqlite-select
                db
                "SELECT id, title, content, keywords, mood, review_status,
                        created_at, replaced_by, source_tape_ids,
                        source_event_ids, projection_version, model_id,
                        prompt_hash, updated_at
                 FROM memory WHERE id = ?"
                (list memory-id))))
    (car rows)))

(defun superchat-db-memory-get-by-source-event (event-id)
  "Return the Memory row projected from EVENT-ID, or nil."
  (let ((rows
         (sqlite-select
          (superchat-db-open)
          "SELECT id, title, content, keywords, mood, review_status,
                  created_at, replaced_by, source_tape_ids,
                  source_event_ids, projection_version, model_id,
                  prompt_hash, updated_at
           FROM memory WHERE source_event_ids = ?"
          (list (json-encode (list event-id))))))
    (car rows)))

(defun superchat-db-memory-list-review (status &optional limit)
  "List up to LIMIT memories with a given review STATUS ('pending', 'accepted', 'rejected')."
  (let* ((db (superchat-db-open))
         (limit (or limit 50)))
    (sqlite-select
     db
     "SELECT id, title, content, keywords, mood, created_at
      FROM memory
      WHERE review_status = ? AND replaced_by IS NULL
      ORDER BY created_at DESC
      LIMIT ?"
     (list status limit))))

(defun superchat-db-memory-set-review (memory-id status &optional replaced-by-id)
  "Update review status of MEMORY-ID."
  (let ((db (superchat-db-open)))
    (if replaced-by-id
        (sqlite-execute
         db
         "UPDATE memory
          SET review_status = ?, replaced_by = ?, updated_at = datetime('now')
          WHERE id = ?"
         (list status replaced-by-id memory-id))
      (sqlite-execute
       db
       "UPDATE memory
        SET review_status = ?, updated_at = datetime('now')
        WHERE id = ?"
       (list status memory-id)))))

(defun superchat-db-memory-count (&optional review-status)
  "Count memory entries.  If REVIEW-STATUS given, filter by it."
  (let* ((db (superchat-db-open))
         (rows (if review-status
                   (sqlite-select db "SELECT COUNT(*) FROM memory WHERE review_status = ?"
                                   (list review-status))
                 (sqlite-select db "SELECT COUNT(*) FROM memory"))))
    (caar rows)))

(defun superchat-db-memory-reset-projection ()
  "Delete materialized Memory rows while preserving legacy rows."
  (let ((db (superchat-db-open)))
    (sqlite-execute
     db
     "UPDATE memory SET replaced_by = NULL
      WHERE replaced_by IN
        (SELECT id FROM memory WHERE projection_version > 0)")
    (sqlite-execute db "DELETE FROM memory WHERE projection_version > 0")))

;; ═══════════════════════════════════════════════════════════
;; Stats & maintenance
;; ═══════════════════════════════════════════════════════════

(defun superchat-db-stats ()
  "Return a plist with row counts for each table."
  (let* ((db (superchat-db-open)))
    (list :tape-count
          (caar (sqlite-select db "SELECT COUNT(*) FROM tape"))
          :memory-count
          (caar (sqlite-select db "SELECT COUNT(*) FROM memory"))
          :memory-pending
          (caar (sqlite-select db "SELECT COUNT(*) FROM memory WHERE review_status = 'pending'"))
          :memory-accepted
          (caar (sqlite-select db "SELECT COUNT(*) FROM memory WHERE review_status = 'accepted'")))))

;;;###autoload
(defun superchat-db-vacuum ()
  "Run VACUUM on the database to reclaim space."
  (interactive)
  (sqlite-execute (superchat-db-open) "VACUUM")
  (message "Superchat DB: VACUUM complete."))

(provide 'superchat-db)

;;; superchat-db.el ends here
