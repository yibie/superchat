# L1 memory: the Tape and its views

Part of the [specification](../SPEC.md). Modules: `superchat-tape.el`,
`superchat-view.el`. This layer uses only Emacs (`sqlite`, `project`,
`cl-lib`, `subr-x`, `json`) and `superchat.el`. Everything the other layers keep or recall goes through the
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

`:data` is nil or a plist. Its values are strings, numbers, t, nil, plists and
lists of these; anything else is an error. It is stored as JSON, nil as `{}`.
A plist is a JSON object: keys lose the colon and come back as keywords, at
every depth. Any other list is a JSON array. t comes back as t, nil as nil.
`:refs` is a JSON array of integers.

Public functions, and no others:

- `(superchat-tape-open)`: open the Tape (creating its schema) and return
  the connection. The Tape is the file `superchat-tape-file` names when the
  function is called, so every other function works on the file the options
  name now. One connection per file it has opened, kept.
  `(superchat-tape-close)` closes every connection, the read-only ones of
  `superchat-tape-select` too.
- `(superchat-tape-append TAPE KIND TEXT &key data refs scope at)` → the new
  entry. A timeline has one scope, set by its first entry: SCOPE defaults to
  it, giving another one is an error, and so is giving none for the first
  entry. KIND is one of the eight kind symbols.
- `(superchat-tape-get ID)` → entry or nil.
- `(superchat-tape-trace ID)` → `(:sources IDS :derived IDS)`: the ids in
  entry ID's `:refs`, and the later entries whose `:refs` name ID, in id
  order; nil when there is no entry ID.
- `(superchat-tape-entries &key tape scope kinds after before limit newest)`
  → entries in id order. AFTER and BEFORE are ids, exclusive. With NEWEST
  non-nil and LIMIT, the last LIMIT entries, still returned in id order.
- `(superchat-tape-search QUERY &key scope kinds limit)` → entries matching
  the text, best first by the full-text rank. A QUERY shorter than three
  characters uses `LIKE`, because the trigram index cannot answer it, and
  its entries come newest first. QUERY is plain text: quote it
  for FTS so that punctuation in it is never read as FTS syntax.
- `(superchat-tape-select SQL &optional ARGS)` → rows, for `SELECT` and
  `WITH` statements only. It reads through a second connection to the file
  with `PRAGMA query_only=ON`, so that a statement cannot write. It opens the
  Tape first when it is not open, so it works before anything was appended.
  The read-only connection is kept and used again, not opened each time.
- `(superchat-tape-new-name)` → a fresh timeline name, `YYYYMMDD-HHMMSS-xxxx`.
- `(superchat-tape-scope-of DIRECTORY)` → the scope for DIRECTORY, by
  `project-current`.
- `(superchat-tape-directory TAPE)` → the timeline's working directory: its
  scope when a project root, else the `:directory` of its first `user`
  entry, else nil.

There is no function that updates or deletes.

## superchat-view.el

- `(superchat-view-define NAME FUNCTION DOC)`: FUNCTION takes a plist of
  arguments and returns a list of entries. NAME is a symbol.
- `(superchat-view-remove NAME)`.
- `(superchat-view NAME &rest ARGS)` → the entries. Unknown NAME is an error.
- `(superchat-view-names)` → alist of `(NAME . DOC)`.
- `(superchat-view-render ENTRIES &optional MAX-CHARS)` → text for a model or
  a person: one block per entry, in the order given, separated by a blank
  line. A block is the header `#ID KIND YYYY-MM-DD HH:MM` (local time), then
  on the next line the text, if it is not empty. A tool result is cut to 400
  characters by `superchat-view-clip`. When MAX-CHARS is exceeded, entries are
  dropped smallest `:id` first, whatever the order of ENTRIES, until the text
  fits or one block is left, and the text starts with the line
  `[N earlier entries left out]` and a blank line. When the one block left is
  still too long, its header stays whole and its text is cut by
  `superchat-view-clip`, or left out when not even the cut mark fits. So the
  text is longer than MAX-CHARS only when that first line and the one header
  left are already longer.
- `(superchat-view-clip TEXT MAX)` → TEXT when it has at most MAX
  characters, else its start, ending in `... [N characters cut]`, at most
  MAX characters in all; the mark alone when MAX is shorter than the mark.

Built-in views:

| name | arguments | returns |
|---|---|---|
| `conversation` | `:tape` | the timeline from its latest anchor, inclusive, to its end; the whole timeline when it has no anchor. When that anchor has `:refs` and an empty `:text` (a fork), the entries it refers to that exist come first, in id order. No `:tape` is an error |
| `topic` | `:name`, `:scope` optional | for every anchor with that name: the entries from that anchor up to the next anchor of the same timeline |
| `search` | `:query`, `:scope`, `:kinds`, `:limit` (default 8), `:around` (default 1) | each hit with AROUND neighbours before and after it in its timeline, no entry twice, in id order. SCOPE and KINDS filter the hits only; a neighbour is of any kind. A nil or empty `:query` is an error |
| `notes` | `:scope` optional, `:query` optional, `:limit` (default 20) | note entries, newest first; with `:scope`, that scope and `"home"`; with `:query`, the newest LIMIT of the notes that match it. A note that a later note has in `:refs` is corrected (`SPEC.md`) and left out |
| `timelines` | `:scope` optional, `:limit` (default 30) | for each timeline that has no `user` entry with `:ephemeral t`, most recently active first: its latest anchor if it has one, else its first `user` entry; a timeline with neither is left out |
| `sources` | `:id` | the entries named by the `:refs` of entry ID |
| `trace` | `:id` | entry ID, the entries it rests on and the later ones resting on it, in id order |
| `recipes` | `:name`, `:scope` opt, `:limit` opt (400) | `eval_elisp` results that did not fail, cited in the `:refs` of an answer among the newest LIMIT, whose call's code contains NAME; each code once, newest first, the code added to the entry's data as `:code` |

`conversation` is what a session is. `timelines` is the list of sessions.
`notes`, `search`, `topic` and `sources` are what memory is.
