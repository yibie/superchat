# L1 memory: the Tape and its views

Part of the [specification](../SPEC.md). Modules: `superchat-tape.el`,
`superchat-view.el`. This layer uses only Emacs (`sqlite`, `project`) and
`superchat.el`. Everything the other layers keep or recall goes through the
public functions here.

## superchat-tape.el

One table and its full-text index.

```sql
CREATE TABLE IF NOT EXISTS entry (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  tape TEXT NOT NULL, kind TEXT NOT NULL, text TEXT NOT NULL DEFAULT '',
  data TEXT NOT NULL DEFAULT '{}', refs TEXT NOT NULL DEFAULT '[]',
  scope TEXT NOT NULL, at REAL NOT NULL);
CREATE INDEX IF NOT EXISTS entry_tape ON entry(tape, id);
CREATE INDEX IF NOT EXISTS entry_kind ON entry(kind, scope, id);
CREATE VIRTUAL TABLE IF NOT EXISTS entry_fts
  USING fts5(text, content='entry', content_rowid='id', tokenize='trigram');
```

Triggers: one `AFTER INSERT` trigger keeps `entry_fts` in step. Two triggers,
`BEFORE UPDATE` and `BEFORE DELETE` on `entry`, `RAISE(ABORT, 'the tape is
append-only')`. The database enforces the invariant; no Lisp code checks it.

`:data` is stored as JSON. A plist is a JSON object: keys lose the colon and
come back as keywords, at every depth. Any other list is a JSON array, of
numbers, strings, plists or lists. Nil comes back as nil. `:refs` is a JSON array of integers.

Public functions, and no others:

- `(superchat-tape-open &optional FILE)`: open (creating schema) and return
  the connection. With no FILE, `superchat-tape-file`. One connection per
  file, kept. `(superchat-tape-close)` closes it.
- `(superchat-tape-append TAPE KIND TEXT &key data refs scope at)` → the new
  entry. SCOPE defaults to the scope of the latest entry of TAPE; it is an
  error to give none for the first entry of a timeline. KIND is one of the
  six kind symbols.
- `(superchat-tape-get ID)` → entry or nil.
- `(superchat-tape-entries &key tape scope kinds after before limit newest)`
  → entries in id order. AFTER and BEFORE are ids, exclusive. With NEWEST
  non-nil and LIMIT, the last LIMIT entries, still returned in id order.
- `(superchat-tape-search QUERY &key scope kinds limit)` → entries matching
  the text, best first. A QUERY shorter than three characters uses `LIKE`,
  because the trigram index cannot answer it. QUERY is plain text: quote it
  for FTS so that punctuation in it is never read as FTS syntax.
- `(superchat-tape-select SQL &optional ARGS)` → rows, for `SELECT` and
  `WITH` statements only. It opens the file a second time read-only
  (`file:...?mode=ro` is not available in Emacs: use `PRAGMA query_only=ON`
  on a second connection) so that a statement cannot write. It opens the
  Tape first when it is not open, so it works before anything was appended.
  The read-only connection is kept and used again, not opened each time.
- `(superchat-tape-new-name)` → a fresh timeline name, `YYYYMMDD-HHMMSS-xxxx`.
- `(superchat-tape-scope-of DIRECTORY)` → the scope for DIRECTORY, by
  `project-current`.

There is no function that updates or deletes.

## superchat-view.el

- `(superchat-view-define NAME FUNCTION DOC)`: FUNCTION takes a plist of
  arguments and returns a list of entries. NAME is a symbol.
- `(superchat-view-remove NAME)`.
- `(superchat-view NAME &rest ARGS)` → the entries. Unknown NAME is an error.
- `(superchat-view-names)` → alist of `(NAME . DOC)`.
- `(superchat-view-render ENTRIES &optional MAX-CHARS)` → text for a model or
  a person: one block per entry, `#ID kind time` then the text, tool results
  cut to 400 characters. When MAX-CHARS is exceeded the oldest entries are
  dropped and the first line says how many.

Built-in views:

| name | arguments | returns |
|---|---|---|
| `conversation` | `:tape` | the timeline from its latest anchor, inclusive, to its end; the whole timeline when it has no anchor |
| `topic` | `:name`, `:scope` optional | for every anchor with that name: the entries from that anchor up to the next anchor of the same timeline |
| `search` | `:query`, `:scope`, `:kinds`, `:limit` (default 8), `:around` (default 1) | each hit with AROUND neighbours before and after it in its timeline, no entry twice, in id order |
| `notes` | `:scope` optional, `:query` optional, `:limit` (default 20) | note entries, newest first; with `:scope`, that scope and `"home"` |
| `timelines` | `:scope` optional, `:limit` (default 30) | for each timeline that has no `user` entry with `:ephemeral t`, most recently active first: its latest anchor if it has one, else its first `user` entry |
| `sources` | `:id` | the entries named by the `:refs` of entry ID |

`conversation` is what a session is. `timelines` is the list of sessions.
`notes`, `search`, `topic` and `sources` are what memory is.
