# Superchat — specification

This file is the whole contract. If something is not here, it is not part of
Superchat. Do not add it.

## What Superchat is

An Emacs package with three uses:

1. **The AI layer of Emacs.** Any buffer, command or package can ask a model
   to do something through one function, `superchat-run`. The model acts on
   Emacs through tools, the most general being `eval_elisp`.
2. **A coding assistant.** It reads and edits buffers and files of a project
   under a permission level the user sets.
3. **A personal agent.** It keeps everything that happened on the Tape and
   recalls it across conversations, in and outside projects.

## Hard limits

- Product code: **6,000 lines in total**, counted by `wc -l` over the `.el`
  files at the repository root. Tests are not counted. Each file has its own
  cap in the module table. `scripts/check.sh` fails when a cap is exceeded.
- Emacs 30.1 or later. Dependencies: `llm` 0.32.1 and what Emacs ships with
  (`sqlite`, `project`, `cl-lib`, `subr-x`, `json`). Nothing else.
- Every file starts with `;;; NAME.el --- ... -*- lexical-binding: t; -*-` and
  byte-compiles with no warnings.

## Rules against overflow

These are checked in review. Code that breaks one is removed.

1. No feature, option, command, tool, hook, keyword argument or event that
   this file does not name. If you believe something is missing, do not build
   it: write one line in `NOTES.md` (what, why) and go on.
2. No `defcustom` other than those listed under "User options".
3. No compatibility code, no migration code, no "legacy" branch, no
   deprecated alias. There is no earlier version.
4. No check for whether Superchat's own functions or variables exist
   (`fboundp`, `boundp`, `bound-and-true-p`, `featurep`). `require` the file.
5. No `ignore-errors`, and no `condition-case` that discards an error. An
   error is either signalled to the caller or recorded and shown to the user.
   Catch an error only where this file says what happens to it.
6. No extension point that has no user in this repository: no hook variable,
   no generic function, no registry beyond the three named here (tools,
   views, commands).
7. No abstraction with one implementation: no struct wrapping a plist that is
   used in one file, no "manager", "service", "provider" or "adapter" layer
   other than `superchat-llm.el`.
8. Plists and alists are the data. Entries are plists. No `cl-defstruct` for
   data that crosses a module boundary.
9. Comments say why, not what. No section banners longer than one line. No
   commented-out code.
10. One way to do each thing. If two call paths reach the same effect, one of
    them is deleted.

## Vocabulary

- **Tape**: the one SQLite database. It holds entries. It is append-only.
- **Entry**: one immutable fact. A plist
  `(:id N :tape NAME :kind KIND :text STRING :data PLIST :refs IDS :scope SCOPE :at TIME)`.
  KIND is a symbol. TIME is seconds since the epoch, a float.
- **Timeline**: the entries that share a `:tape` name, in id order. A
  conversation is a timeline. Timeline names are strings made by
  `superchat-tape-new-name`.
- **Scope**: the absolute project root of the buffer a timeline was started
  from (with a trailing slash), or the string `"home"` when that buffer is in
  no project.
- **Anchor**: an entry of kind `anchor`. It marks a point from which context
  is rebuilt. Its `:data` is `(:name STRING :summary STRING :next STRING)`,
  and its `:refs` are the entries the summary was made from. An anchor
  deletes nothing.
- **View**: a named function from arguments to a list of entries. Context
  for the model is always built from a view. Nothing is carried over
  implicitly.
- **Note**: an entry of kind `note`: a conclusion worth keeping, with `:refs`
  to the entries it came from. Notes are a cache of conclusions. They never
  replace the entries they cite.
- **Run**: one call of `superchat-run`: a user request answered by the model
  through zero or more tool rounds.

There is no "session" object and no "memory" store. A session is the
`conversation` view of a timeline. Memory is what views return from the Tape.

## Entry kinds

| kind | `:text` | `:data` |
|---|---|---|
| `user` | what the user said | `(:origin PLIST)` optional: `(:buffer NAME :file PATH :mode NAME :line N)`, the mode's name a string |
| `assistant` | the model's text, may be empty | `(:model STRING :calls LIST)`, each call `(:id STRING :name STRING :args PLIST)` |
| `tool` | the tool's result as text | `(:call-id STRING :name STRING :error BOOL)` |
| `anchor` | the summary | `(:name STRING :summary STRING :next STRING)` |
| `note` | the conclusion | `(:title STRING)` |
| `event` | one line a person can read | `(:type STRING ...)`; types: `permission`, `cancelled`, `error`, `level` |

A correction is a new entry whose `:refs` names the entry it corrects.

The arguments of a call are a plist with keyword keys, `(:path "a.el" :count 5)`,
everywhere: from `superchat-llm-request`, on the Tape, and into a tool's
function. A JSON false is nil.

## Modules

| file | cap | owns |
|---|---|---|
| `superchat-tape.el` | 550 | the database, append, read, search |
| `superchat-view.el` | 500 | the view registry and the built-in views |
| `superchat-llm.el` | 450 | the only file that knows `llm.el` |
| `superchat-tools.el` | 800 | the tool registry, running a tool, the built-in tools |
| `superchat-permission.el` | 250 | levels, the decision for one tool call, asking |
| `superchat-agent.el` | 700 | `superchat-run`: the loop, cancel, handoff |
| `superchat-context.el` | 250 | the system prompt: rules files, skills catalog, notes, origin |
| `superchat-recall.el` | 200 | the three tools the model uses on the Tape |
| `superchat-chat.el` | 1100 | the chat buffer and slash commands |
| `superchat-inline.el` | 600 | rewrite a region in place; ask about a region |
| `superchat-plugin.el` | 150 | define, enable, disable a plugin |
| `superchat.el` | 120 | package header, group, user options, version |

Dependencies go one way only:

```
inline → chat
chat → agent, plugin
agent → llm, tools, view, context
recall → tools, view
context → view
tools → permission
permission, view → tape
every file → superchat.el
```

`superchat.el` requires no other Superchat file. Every other file requires it
for the user options. A file requires only what the arrows give it, and
`superchat-llm.el` and `superchat-plugin.el` require only `superchat.el`.

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
come back as keywords, at every depth. A list that is not a plist is a JSON
array. Nil comes back as nil. `:refs` is a JSON array of integers.

Public functions, and no others:

- `(superchat-tape-open &optional FILE)`: open (creating schema) and return
  the connection. With no FILE, `superchat-tape-file`. One connection per
  file, kept. `(superchat-tape-close)` closes it.
- `(superchat-tape-append TAPE KIND TEXT &key data refs scope at)` → the new
  entry. SCOPE defaults to the scope of the latest entry of TAPE; it is an
  error to give none for the first entry of a timeline. KIND must be one of
  the six kinds.
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
  on a second connection) so that a statement cannot write.
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
| `timelines` | `:scope` optional, `:limit` (default 30) | for each timeline, most recently active first: its latest anchor if it has one, else its first `user` entry |
| `sources` | `:id` | the entries named by the `:refs` of entry ID |

`conversation` is what a session is. `timelines` is the list of sessions.
`notes`, `search`, `topic` and `sources` are what memory is.

## superchat-llm.el

The only file that requires `llm`. It translates between neutral messages and
`llm.el`, and never runs a tool.

- `(superchat-llm-request MESSAGES &key system tools model on-text on-done on-error)`
  → a handle.
  - MESSAGES: a list of `(:role user :text S)`,
    `(:role assistant :text S :calls CALLS)` and
    `(:role tool :call-id ID :name NAME :text S)`.
  - TOOLS: a list of `(:name S :description S :args ARGS)`, each of ARGS
    `(:name S :type string|number|boolean :description S :required BOOL)`.
  - ON-TEXT is called with each piece of streamed text. ON-DONE is called
    once with `(:text S :calls CALLS :model S)`. ON-ERROR is called once
    with a string. Exactly one of ON-DONE and ON-ERROR is called, unless the
    request is cancelled, and then neither.
  - One request is one model response. When the model asks for tools the
    calls come back in ON-DONE; the caller runs them and makes another
    request with the results appended to MESSAGES.
- It uses only what `llm.el` documents for its callers: no advice on, and no
  call to, a function of `llm.el` whose name has `--` or that lives in
  `llm-provider-utils`, with one exception: `make-llm-provider-utils-tool-use`,
  the only way to write an earlier tool call into a prompt. The tool functions
  given to `llm.el` only record that they were called and with what. Call ids
  are made here, so a call and its result agree when they are replayed.
- `(superchat-llm-cancel HANDLE)`.
- `(superchat-llm-models)` → names for completion, from the provider when it
  can list them, else the configured model alone.

The provider is `superchat-llm-provider`, an `llm.el` provider object the
user sets. Tests never touch the network: `superchat-llm-request-function`
(a `defvar`, default `#'superchat-llm--request`) is what `superchat-llm-request`
calls, and tests bind it to a scripted function.

## superchat-permission.el

Three levels: `read-only`, `ask` (the default), `auto`. A tool has a class:
`read`, `write` or `exec`.

| | read | write | exec |
|---|---|---|---|
| `read-only` | allow | deny | deny |
| `ask` | allow | ask | ask |
| `auto` | allow | allow | allow |

- `(superchat-permission-level TAPE)` → the level of a timeline: the level of
  its latest `event` entry of type `level`, else `superchat-default-level`.
- `(superchat-permission-set TAPE LEVEL)`: append that event.
- `(superchat-permission-decide TAPE CLASS)` → `allow`, `deny` or `ask`.
- `(superchat-permission-ask TOOL-NAME SUMMARY)` → `allow`, `deny` or
  `always`, by calling `superchat-permission-ask-function` (a `defvar`). The
  default reads one key in the minibuffer: y, n, or a for "always in this
  conversation". `superchat-tool-call` remembers `always` for that tool name
  in that timeline until Emacs exits, in a hash table, not on the Tape.

## superchat-tools.el

- `(superchat-tool-define NAME &key description args class function)`:
  ARGS has the shape `superchat-llm-request` takes. DESCRIPTION is a string,
  or a function of no arguments returning one, called each time the specs
  are asked for. FUNCTION is called with one plist of arguments and returns
  a string, or signals an error. Tools are synchronous.
- `superchat-tool-tape` (a `defvar`): bound to the timeline name while a
  tool's function runs, for the tools that need it.
- `(superchat-tool-remove NAME)`, `(superchat-tool-specs &optional NAMES)` →
  the list `superchat-llm-request` takes.
- `(superchat-tool-call TAPE CALL)` → the `tool` entry it appended. It decides
  permission, asks when needed, runs the function with `default-directory`
  set to the timeline's scope (the user's home directory for `"home"`),
  appends the result, and for an `ask` or `deny` decision appends a
  `permission` event first. A denied call, an unknown tool, a missing
  required argument and an error signalled by the function all become a
  `tool` entry with `:error t` whose text says what happened. Nothing is
  swallowed and nothing is signalled to the loop. A quit (`C-g` while the
  tool runs or while the user is asked) is such an entry too, with the text
  `Interrupted by the user.`
- Results longer than `superchat-tool-max-chars` (20000) are cut, and the
  text says how many characters were left out.

Built-in tools, and no others:

| name | class | arguments | does |
|---|---|---|---|
| `read_file` | read | `path`, `start` opt, `count` opt | lines of a file with line numbers; at most 400 lines a call |
| `read_buffer` | read | `buffer`, `start` opt, `count` opt | the same for a buffer |
| `list_buffers` | read | none | name, mode, file and size of the user's buffers (no names starting with a space) |
| `search_files` | read | `pattern`, `path` opt | `grep -rn` under the scope, at most 100 matching lines |
| `edit_buffer` | write | `buffer`, `old`, `new` | replace OLD by NEW in a buffer; OLD must occur exactly once; does not save |
| `write_file` | write | `path`, `content` | create or replace a file, through a buffer, and save it |
| `save_buffer` | write | `buffer` | save |
| `eval_elisp` | exec | `code` | read every form in CODE, evaluate in order, return the last value printed, with what was printed to `*Messages*` meanwhile |
| `shell` | exec | `command` | run through `shell-file-name`, at most `superchat-shell-seconds` (60), return output and exit status |

A path is relative to the scope. A file a tool edits is edited through its
buffer, so the user can undo it.

## superchat-recall.el

Three tools, defined with `superchat-tool-define`, all of class `read`:
`note` only appends to the Tape, so it never asks.

| name | arguments | does |
|---|---|---|
| `recall` | `view`, `query` opt, `name` opt, `id` opt, `limit` opt | run a built-in or plugin view for the current scope and return `superchat-view-render` of it, at most 6000 characters |
| `tape_query` | `sql` | `superchat-tape-select`, rows as JSON lines, at most 200 rows |
| `note` | `title`, `text`, `sources` (comma-separated entry ids) | append a `note` entry to the current timeline |

The description of `recall` lists the views and their arguments from
`superchat-view-names`, each time the specs are asked for. The description of
`tape_query` gives the table definition.

## superchat-context.el

- `(superchat-context-system TAPE &optional EXTRA)` → the system prompt, built
  fresh for each request, in this order:
  1. the base instructions, a constant of at most 40 lines;
  2. the rules file of the scope: `AGENTS.md` at the project root, or
     `AGENTS.md` in `superchat-directory` for `"home"`, when it exists;
  3. the skills catalog: for each `NAME/SKILL.md` under
     `superchat-directory`/`skills/`, its name and the `description:` line of
     its frontmatter, with the path, so the model reads the file with
     `read_file` when it wants it;
  4. the `notes` view of the scope, rendered, at most 2000 characters;
  5. the origin of the latest `user` entry, when it has one;
  6. EXTRA, a string, when given.
- Frontmatter is only at the very start of a file and ends at the first line
  that is exactly `---`.

## superchat-agent.el

- `(superchat-run TAPE TEXT &key origin tools system model on-text on-entry on-done)`
  → the run, a plist. The first entry of a new timeline takes the scope of
  `default-directory`. It appends the `user` entry, then loops:
  1. messages = the `conversation` view of TAPE turned into neutral
     messages. An anchor becomes a `user` message that begins
     `Summary of the conversation so far:`. `event` and `note` entries are
     left out.
  2. `superchat-llm-request` with `superchat-context-system` and the tool
     specs. TOOLS is a list of names, or nil for every tool, or `none`.
  3. On done: append the `assistant` entry. If it has calls, run each with
     `superchat-tool-call` in order, then go to 1. If it has none, the run
     is over.
  ON-TEXT gets streamed text. ON-ENTRY gets each entry as it is appended.
  ON-DONE gets the run once, with `:status` `done`, `cancelled` or `error`.
- At most one run per timeline. `superchat-run` on a timeline that has one is
  an error.
- `(superchat-run-active TAPE)` → the run or nil.
- `(superchat-cancel TAPE)`: cancel the request, append a `cancelled` event,
  call ON-DONE with `cancelled`. Tool calls of the current response that
  have not started are not run. So that the timeline stays well formed, a
  `tool` entry with `:error t` and the text `Cancelled.` is appended for
  each call that has no result.
- After `superchat-max-rounds` (40) model responses in one run, the run ends
  with status `error` and an `error` event that says so.
- An error from the model appends an `error` event and ends the run with
  status `error`.
- `(superchat-handoff TAPE &optional NAME on-done)`: ask the model, with no
  tools, for a summary of the `conversation` view and for what comes next,
  and append an anchor with them, its `:refs` the ids summarised. NAME is the
  topic name, default `"handoff"`. ON-DONE is called with the anchor, or with
  nil when the model failed (an `error` event is appended then).
- When `superchat-run` is called and the timeline's messages are already
  longer than `superchat-context-chars`, a handoff is made first, and the
  `user` entry is appended after the anchor, so the request is never behind
  the anchor. There is no handoff in the middle of a run.
- `(superchat-fork TAPE ENTRY-ID)` → a new timeline name whose first entry is
  an anchor: summary empty, `:refs` the ids of TAPE's `conversation` view up
  to ENTRY-ID, and when that entry is an `assistant` entry with calls, the
  `tool` entries that answer them as well. The `conversation` view of a
  timeline whose first anchor has refs and an empty summary begins with
  those referred entries.

## superchat-chat.el

One buffer per timeline, named `*superchat: TITLE*`, TITLE being the first
40 characters of the first `user` entry, or `new`. Major mode
`superchat-chat-mode`, derived from `text-mode`.

- The buffer is rendered from the `conversation` view. Text above the input
  area is read-only. Entries look like:
  `▸ You`, `▸ Assistant`, a tool call as one line `⚙ name(args cut to 60
  chars) → first line of the result`, an anchor as `── summary: NAME ──` and
  its text, an event as a dimmed line.
- The input area is the editable text after the last entry. `RET` sends when
  point is in the last line of the input, otherwise inserts a newline;
  `C-c C-c` always sends. `C-c C-k` cancels the run.
- While a run is active the mode line shows `[running]`, streamed text
  appears as it comes, and the input stays editable but sending is refused
  with a message.
- `superchat` (an autoloaded command): open the chat buffer of the most
  recent timeline of the current buffer's scope, or a new one. With a prefix
  argument, always a new one. It enables the plugins of `superchat-plugins`
  that are not enabled yet.
- `(superchat-chat-open TAPE)` → the chat buffer of timeline TAPE, made and
  rendered when there is none. It does not display it.
- `(superchat-chat-send TEXT &optional ORIGIN)`: in a chat buffer, send TEXT
  as the user's request. Sending from the input area calls this.
- `(superchat-chat-recent-tape SCOPE)` → the name of the most recently active
  timeline of SCOPE whose name does not start with `inline:`, or nil.
- A line starting with `/` is a command. `(superchat-command-define NAME
  FUNCTION DOC)` and `(superchat-command-remove NAME)`: NAME is written
  without the slash; FUNCTION is called with the rest of the line, in the
  chat buffer.

Built-in commands, and no others:

| command | does |
|---|---|
| `/new` | a new timeline in the same scope, in a new buffer |
| `/resume` | choose a timeline from the `timelines` view with `completing-read` and open it |
| `/fork` | fork at the entry at point, open the new timeline |
| `/handoff [name]` | `superchat-handoff`; refused while a run is active |
| `/topic name` | open the `topic` view of that name in a read-only buffer |
| `/recall query` | open the `search` view in a read-only buffer |
| `/notes` | open the `notes` view in a read-only buffer |
| `/level [read-only\|ask\|auto]` | show or set the permission level |
| `/model [name]` | show or set the model for this buffer |
| `/help` | list commands |

## superchat-inline.el

- `superchat-rewrite` (a command): with a region, read an instruction, run on
  the timeline `inline:` + scope with tools `none`, and show the model's
  replacement over the region in an overlay as it streams. `C-c C-c` on the
  overlay replaces the region (one undo step), `C-c C-k` discards it. The
  request tells the model to answer with the replacement text only; a
  fenced code block around the whole answer is unwrapped.
- `superchat-ask` (a command): read a question; the origin is the current
  buffer, and the region when active is quoted in the request. It opens the
  chat buffer of `superchat-chat-recent-tape` for the scope (a new timeline
  when there is none) beside the current window and sends the question
  there with `superchat-chat-send`.

## superchat-plugin.el

- `(superchat-plugin-define NAME &key doc enable disable)`: ENABLE and
  DISABLE are functions of no arguments. A plugin adds tools, views and
  commands in ENABLE and removes them in DISABLE.
- `(superchat-plugin-enable NAME)`, `(superchat-plugin-disable NAME)`,
  `(superchat-plugin-enabled-p NAME)`.
- `(superchat-plugin-enable-configured)`: enable each name in the user
  option `superchat-plugins` that is not enabled. A name with no definition
  is an error that names it.

Plugins live in another repository. This one ships none.

## superchat.el

Package header, `defgroup superchat`, the user options and the constant
`superchat-version` (`"2.0.0"`). Nothing else.

## User options

`superchat-directory` (`~/.superchat/`), `superchat-tape-file`
(`tape.sqlite` in it), `superchat-llm-provider` (nil; using Superchat without
one is an error that says how to set it), `superchat-llm-model` (nil: the
provider's own), `superchat-default-level` (`ask`), `superchat-context-chars`
(120000), `superchat-max-rounds` (40), `superchat-tool-max-chars` (20000),
`superchat-shell-seconds` (60), `superchat-plugins` (nil).

## Tests

ERT, one file per module in `test/`, named `test-NAME.el`. `test/helper.el`
gives `superchat-test-with-tape` (a fresh database in a temporary directory,
closed and deleted afterwards) and `superchat-test-script` (bind
`superchat-llm-request-function` to answer from a list of scripted
responses, synchronously by default). Tests do not use the network, the
user's home directory or timers longer than 0.1 s. Test what this file
promises, by behaviour. Do not test private functions.

A test never deletes a directory it did not make itself with
`make-temp-file`, never binds `superchat-directory` or `default-directory`
to the repository, and never lets a scripted model run the `shell` or
`eval_elisp` tool on a path outside a temporary directory.

`scripts/check.sh` compiles every file with warnings as errors, runs every
test in a copy of the repository under the temporary directory, and checks
the caps. It must pass before work is handed back.
