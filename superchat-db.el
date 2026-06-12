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

;; ═══════════════════════════════════════════════════════════
;; Database path & connection
;; ═══════════════════════════════════════════════════════════

(defvar superchat-db--connection nil
  "SQLite database connection for tape + memory storage.")

(defvar superchat-db--path nil
  "Filesystem path to the SQLite database file.")

(defun superchat-db--path ()
  "Return the default SQLite database path."
  (expand-file-name "superchat.db"
                     (if (boundp 'superchat-data-directory)
                         superchat-data-directory
                       (expand-file-name "superchat/" user-emacs-directory))))

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

(defconst superchat-db--schema-version 2
  "Current schema version.  Used for auto-migration.

History:
  1 — initial tape + memory + FTS5 (unicode61 tokenizer; broken for CJK).
  2 — FTS5 switched to trigram tokenizer.  Trigram indexes any
      3+ character substring, which lets CJK queries hit without
      explicit word segmentation.  Caveat: queries shorter than
      3 characters fall through to LIKE.")

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
    ;; ── Tape: append-only event log ──
    (sqlite-execute
     db
     "CREATE TABLE IF NOT EXISTS tape (
        id         INTEGER PRIMARY KEY AUTOINCREMENT,
        session_id TEXT    NOT NULL,
        kind       TEXT    NOT NULL CHECK(kind IN ('user','assistant','tool_call','tool_result','anchor','system')),
        content    TEXT    NOT NULL,
        meta       TEXT    DEFAULT '{}',
        created_at TEXT    NOT NULL DEFAULT (datetime('now'))
      )")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_session ON tape(session_id)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_kind ON tape(kind)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_tape_created ON tape(created_at)")

    ;; ── Memory: extracted facts ──
    (sqlite-execute
     db
     "CREATE TABLE IF NOT EXISTS memory (
        id              INTEGER PRIMARY KEY AUTOINCREMENT,
        title           TEXT,
        content         TEXT    NOT NULL,
        keywords        TEXT    DEFAULT '',
        source_tape_ids TEXT    DEFAULT '[]',
        mood            TEXT    DEFAULT '',
        replaced_by     INTEGER REFERENCES memory(id),
        review_status   TEXT    DEFAULT 'pending'
                           CHECK(review_status IN ('pending','accepted','rejected')),
        created_at      TEXT    NOT NULL DEFAULT (datetime('now'))
      )")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_mood ON memory(mood)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_review ON memory(review_status)")
    (sqlite-execute db "CREATE INDEX IF NOT EXISTS idx_memory_created ON memory(created_at)")

    ;; FTS5 full-text index — trigram tokenizer.  This is the only
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

    ;; ── Schema version + targeted migrations ──
    (sqlite-execute
     db
     "CREATE TABLE IF NOT EXISTS _schema_version (version INTEGER PRIMARY KEY)")
    (let ((current (superchat-db--current-schema-version db)))
      (unless (equal current superchat-db--schema-version)
        ;; v1 → v2: rebuild memory_fts with the trigram tokenizer.
        ;; The CREATE TABLE IF NOT EXISTS above is skipped when an old
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
        (sqlite-execute
         db "INSERT OR REPLACE INTO _schema_version (version) VALUES (?)"
         (list superchat-db--schema-version)))))

;; ═══════════════════════════════════════════════════════════
;; Tape operations
;; ═══════════════════════════════════════════════════════════

(cl-defun superchat-db-tape-append (session-id kind content &key (meta nil))
  "Append an event to the tape.  Returns the new row id."
  (let ((db (superchat-db-open))
        (meta-json (if meta
                       (condition-case nil
                           (json-encode meta)
                         (error "{}"))
                     "{}")))
    (sqlite-execute
     db
     "INSERT INTO tape (session_id, kind, content, meta) VALUES (?, ?, ?, ?)"
     (list session-id kind content meta-json))
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun superchat-db-tape-replay (session-id &optional since-id limit)
  "Return tape entries for SESSION-ID, ordered by id ascending.
If SINCE-ID is given, only entries with id > SINCE-ID.
If LIMIT is given, return at most LIMIT entries."
  (let* ((db (superchat-db-open))
         (sql (if since-id
                  "SELECT id, kind, content, meta, created_at FROM tape
                   WHERE session_id = ? AND id > ?
                   ORDER BY id ASC"
                "SELECT id, kind, content, meta, created_at FROM tape
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
                "SELECT id, content FROM tape
                 WHERE session_id = ? AND kind = 'anchor'
                 ORDER BY id DESC LIMIT 1"
                (list session-id))))
    (car rows)))

(defun superchat-db-tape-search (query &optional limit)
  "Full-text search across all tape entries.  Used by /recall."
  (let* ((db (superchat-db-open))
         (limit (or limit 20)))
    ;; Simple LIKE-based search (tape table doesn't have FTS5 —
    ;; the volume is smaller and we want exact substring matches)
    (sqlite-select
     db
     "SELECT id, session_id, kind, content, created_at
      FROM tape
      WHERE content LIKE '%' || ? || '%'
      ORDER BY id DESC
      LIMIT ?"
     (list query limit))))

;; ═══════════════════════════════════════════════════════════
;; Memory operations (extracted facts)
;; ═══════════════════════════════════════════════════════════

(cl-defun superchat-db-memory-insert (content &key title keywords source-tape-ids mood status)
  "Insert CONTENT as a new memory entry.  Return the new row id.
STATUS defaults to \"accepted\" — direct user-driven inserts
\(`/remember', `capture-explicit') are trusted and should be
immediately retrievable.  Background / LLM-derived inserts can
pass STATUS \"pending\" to keep them out of recall until the
user reviews them."
  (let ((db (superchat-db-open))
        (effective-status (or status "accepted")))
    (sqlite-execute
     db
     "INSERT INTO memory (title, content, keywords, source_tape_ids, mood, review_status)
      VALUES (?, ?, ?, ?, ?, ?)"
     (list (or title "")
           content
           (or keywords "")
           (if source-tape-ids
               (condition-case nil
                   (json-encode source-tape-ids)
                 (error "[]"))
             "[]")
           (or mood "")
           effective-status))
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun superchat-db-memory-delete-by-id (memory-id)
  "Hard-delete MEMORY-ID.  Use sparingly — prefer review_status='rejected'."
  (sqlite-execute (superchat-db-open)
                  "DELETE FROM memory WHERE id = ?"
                  (list memory-id)))

(defun superchat-db-memory-delete-rejected-older-than (days)
  "Delete rejected memory entries older than DAYS days.  Return the row count
before deletion (best-effort — sqlite-execute does not return affected rows)."
  (sqlite-execute (superchat-db-open)
                  "DELETE FROM memory
                   WHERE review_status = 'rejected'
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
      WHERE memory_fts MATCH ? AND m.review_status != 'rejected'
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
        AND review_status != 'rejected'
      ORDER BY created_at DESC
      LIMIT ?"
     (list like-pat like-pat like-pat limit))))

(defun superchat-db-memory-get-by-id (memory-id)
  "Get a single memory entry by MEMORY-ID."
  (let* ((db (superchat-db-open))
         (rows (sqlite-select
                db
                "SELECT id, title, content, keywords, mood, review_status,
                        created_at, replaced_by, source_tape_ids
                 FROM memory WHERE id = ?"
                (list memory-id))))
    (car rows)))

(defun superchat-db-memory-list-review (status &optional limit)
  "List up to LIMIT memories with a given review STATUS ('pending', 'accepted', 'rejected')."
  (let* ((db (superchat-db-open))
         (limit (or limit 50)))
    (sqlite-select
     db
     "SELECT id, title, content, keywords, mood, created_at
      FROM memory
      WHERE review_status = ?
      ORDER BY created_at DESC
      LIMIT ?"
     (list status limit))))

(defun superchat-db-memory-set-review (memory-id status &optional replaced-by-id)
  "Update review status of MEMORY-ID."
  (let ((db (superchat-db-open)))
    (if replaced-by-id
        (sqlite-execute
         db
         "UPDATE memory SET review_status = ?, replaced_by = ? WHERE id = ?"
         (list status replaced-by-id memory-id))
      (sqlite-execute
       db
       "UPDATE memory SET review_status = ? WHERE id = ?"
       (list status memory-id)))))

(defun superchat-db-memory-set-replaced (memory-id new-id)
  "Mark MEMORY-ID as superseded by NEW-ID."
  (superchat-db-memory-set-review memory-id "accepted" new-id))

(defun superchat-db-memory-count (&optional review-status)
  "Count memory entries.  If REVIEW-STATUS given, filter by it."
  (let* ((db (superchat-db-open))
         (rows (if review-status
                   (sqlite-select db "SELECT COUNT(*) FROM memory WHERE review_status = ?"
                                   (list review-status))
                 (sqlite-select db "SELECT COUNT(*) FROM memory"))))
    (caar rows)))

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
