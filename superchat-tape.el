;;; superchat-tape.el --- The Tape: an append-only SQLite log -*- lexical-binding: t; -*-

;;; Commentary:
;; One table of entries and its full-text index.  The database itself refuses
;; UPDATE and DELETE; no Lisp here checks that.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'superchat)

(defconst superchat-tape--kinds '("user" "assistant" "tool" "anchor" "note" "event"))

(defconst superchat-tape--schema
  '("CREATE TABLE IF NOT EXISTS entry (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tape TEXT NOT NULL, kind TEXT NOT NULL, text TEXT NOT NULL DEFAULT '',
  data TEXT NOT NULL DEFAULT '{}', refs TEXT NOT NULL DEFAULT '[]',
  scope TEXT NOT NULL, at REAL NOT NULL)"
    "CREATE INDEX IF NOT EXISTS entry_tape ON entry(tape, id)"
    "CREATE INDEX IF NOT EXISTS entry_kind ON entry(kind, scope, id)"
    "CREATE VIRTUAL TABLE IF NOT EXISTS entry_fts
  USING fts5(text, content='entry', content_rowid='id', tokenize='trigram')"
    "CREATE TRIGGER IF NOT EXISTS entry_ai AFTER INSERT ON entry BEGIN
  INSERT INTO entry_fts(rowid, text) VALUES (new.id, new.text); END"
    "CREATE TRIGGER IF NOT EXISTS entry_no_update BEFORE UPDATE ON entry BEGIN
  SELECT RAISE(ABORT, 'the tape is append-only'); END"
    "CREATE TRIGGER IF NOT EXISTS entry_no_delete BEFORE DELETE ON entry BEGIN
  SELECT RAISE(ABORT, 'the tape is append-only'); END")
  "Statements that create the schema.")

(defconst superchat-tape--columns "id, tape, kind, text, data, refs, scope, at")

(defvar superchat-tape--connections (make-hash-table :test #'equal)
  "File name to its open connection.")

(defvar superchat-tape--readers (make-hash-table :test #'equal)
  "File name to its read-only connection, for `superchat-tape-select'.")

(defun superchat-tape--file (file)
  "Absolute name of FILE, or of the configured Tape file."
  (expand-file-name (or file superchat-tape-file
                        (expand-file-name "tape.sqlite" superchat-directory))))

(defun superchat-tape-open (&optional file)
  "Open FILE (default the configured Tape file), create the schema, return it."
  (let ((name (superchat-tape--file file)))
    (or (gethash name superchat-tape--connections)
        (progn
          (make-directory (file-name-directory name) t)
          (let ((db (sqlite-open name)))
            (dolist (sql superchat-tape--schema) (sqlite-execute db sql))
            (puthash name db superchat-tape--connections))))))

(defun superchat-tape-close ()
  "Close every connection, the read-only ones too."
  (dolist (table (list superchat-tape--connections superchat-tape--readers))
    (maphash (lambda (_ db) (sqlite-close db)) table)
    (clrhash table)))

(defun superchat-tape--json (value)
  "VALUE ready for `json-serialize'.
A plist becomes an object, any other list an array."
  (cond ((not (consp value)) value)
        ((keywordp (car value))
         (cl-loop for (k v) on value by #'cddr collect k collect (superchat-tape--json v)))
        (t (vconcat (mapcar #'superchat-tape--json value)))))

(defun superchat-tape--parse (json)
  "The Lisp value of the stored JSON string."
  (json-parse-string json :object-type 'plist :array-type 'list
                     :null-object nil :false-object nil))

(defun superchat-tape--entry (row)
  "The entry plist of database ROW."
  (pcase-let ((`(,id ,tape ,kind ,text ,data ,refs ,scope ,at) row))
    (list :id id :tape tape :kind (intern kind) :text text
          :data (superchat-tape--parse data) :refs (superchat-tape--parse refs)
          :scope scope :at at)))

(defun superchat-tape--rows (sql args)
  "Entries answering SQL with ARGS."
  (mapcar #'superchat-tape--entry (sqlite-select (superchat-tape-open) sql args)))

(defun superchat-tape--where (conds)
  "The WHERE clause for CONDS, a list of SQL strings."
  (if conds (concat " WHERE " (string-join conds " AND ")) ""))

(defun superchat-tape--filters (scope kinds)
  "Conditions and arguments for SCOPE and KINDS: (CONDS . ARGS)."
  (cons (append (and scope '("scope = ?"))
                (and kinds (list (format "kind IN (%s)"
                                         (string-join (mapcar (lambda (_) "?") kinds) ",")))))
        (append (and scope (list scope)) (mapcar #'symbol-name kinds))))

(cl-defun superchat-tape-append (tape kind text &key data refs scope at)
  "Append an entry to timeline TAPE and return it.
SCOPE defaults to that of TAPE's latest entry."
  (unless (and (symbolp kind) (member (symbol-name kind) superchat-tape--kinds))
    (error "Unknown entry kind: %S" kind))
  (let ((scope (or scope
                   (caar (sqlite-select (superchat-tape-open)
                                        "SELECT scope FROM entry WHERE tape = ? ORDER BY id DESC LIMIT 1"
                                        (list tape)))
                   (error "The first entry of timeline %s needs a scope" tape)))
        (db (superchat-tape-open)))
    (sqlite-execute db "INSERT INTO entry (tape, kind, text, data, refs, scope, at)
 VALUES (?, ?, ?, ?, ?, ?, ?)"
                    (list tape (symbol-name kind) text
                          (json-serialize (superchat-tape--json data) :null-object nil)
                          (json-serialize (vconcat refs))
                          scope (float (or at (float-time)))))
    (superchat-tape-get (caar (sqlite-select db "SELECT last_insert_rowid()")))))

(defun superchat-tape-get (id)
  "The entry with ID, or nil."
  (car (superchat-tape--rows
        (format "SELECT %s FROM entry WHERE id = ?" superchat-tape--columns) (list id))))

(cl-defun superchat-tape-entries (&key tape scope kinds after before limit newest)
  "Entries in id order, filtered; AFTER and BEFORE are exclusive ids.
With NEWEST and LIMIT, the last LIMIT entries."
  (let* ((filters (superchat-tape--filters scope kinds))
         (conds (append (and tape '("tape = ?")) (and after '("id > ?"))
                        (and before '("id < ?")) (car filters)))
         (args (append (and tape (list tape)) (and after (list after))
                       (and before (list before)) (cdr filters)))
         (sql (format "SELECT %s FROM entry%s ORDER BY id %s%s" superchat-tape--columns
                      (superchat-tape--where conds) (if newest "DESC" "ASC")
                      (if limit " LIMIT ?" "")))
         (rows (superchat-tape--rows sql (append args (and limit (list limit))))))
    (if newest (nreverse rows) rows)))

(cl-defun superchat-tape-search (query &key scope kinds limit)
  "Entries whose text matches the plain text QUERY, best first."
  (let* ((filters (superchat-tape--filters scope kinds))
         (short (< (length query) 3))
         (conds (cons (if short "text LIKE ? ESCAPE '\\'" "entry_fts MATCH ?") (car filters)))
         (match (if short
                    (concat "%" (replace-regexp-in-string "[%_\\]" "\\\\\\&" query) "%")
                  (concat "\"" (string-replace "\"" "\"\"" query) "\"")))
         (sql (format "SELECT %s FROM %s%s ORDER BY %s%s"
                      (if short superchat-tape--columns
                        "entry.id, tape, kind, entry.text, data, refs, scope, at")
                      (if short "entry" "entry_fts JOIN entry ON entry.id = entry_fts.rowid")
                      (superchat-tape--where conds) (if short "id DESC" "rank")
                      (if limit " LIMIT ?" ""))))
    (superchat-tape--rows sql (append (list match) (cdr filters) (and limit (list limit))))))

(defun superchat-tape-select (sql &optional args)
  "Rows answering the SELECT or WITH statement SQL, read-only.
The connection is a second one, kept per file.  `sqlite-execute' is used
because `sqlite-select' hides a refused write."
  (unless (string-match-p "\\`[ \t\n]*\\(select\\|with\\)\\_>" (downcase sql))
    (error "Only SELECT and WITH statements are allowed"))
  (let* ((name (superchat-tape--file nil))
         (db (or (gethash name superchat-tape--readers)
                 (progn (superchat-tape-open)
                        (let ((reader (sqlite-open name)))
                          (sqlite-execute reader "PRAGMA query_only=ON")
                          (puthash name reader superchat-tape--readers)))))
         ;; `sqlite-execute' answers a count, not nil, when no row matches.
         (rows (sqlite-execute db sql args)))
    (and (listp rows) rows)))

(defun superchat-tape-new-name ()
  "A fresh timeline name, YYYYMMDD-HHMMSS-xxxx."
  (format "%s-%04x" (format-time-string "%Y%m%d-%H%M%S") (random 65536)))

(defun superchat-tape-scope-of (directory)
  "The scope for DIRECTORY: its project root, or \"home\"."
  (if-let* ((project (project-current nil directory)))
      (file-name-as-directory (expand-file-name (project-root project)))
    "home"))

(provide 'superchat-tape)
;;; superchat-tape.el ends here
