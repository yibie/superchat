# L2 capabilities: the model, the tools and permission

Part of the [specification](../SPEC.md). Modules: `superchat-llm.el`,
`superchat-permission.el`, `superchat-tools.el`, `superchat-recall.el`. This
layer uses L1 and the bridge; it never runs a loop of requests.

## superchat-llm.el

The only file that requires `superchat-bridge` (the port of pi's AI package kept in
`bridge/`; its own plan and rules are in `bridge/`). It translates between
neutral messages and the bridge, and never runs a tool. Nothing in Superchat
requires `llm.el`.

- `(superchat-llm-request MESSAGES &key system tools model on-text on-done on-error)`
  → a handle.
  - MESSAGES: a list of `(:role user :text S)`,
    `(:role assistant :text S :calls CALLS)` and
    `(:role tool :call-id ID :name NAME :text S)`.
  - TOOLS: a list of `(:name S :description S :args ARGS)`, each of ARGS
    `(:name S :type string|number|boolean :description S :required BOOL)`.
    They become the bridge tools with a JSON-schema object as parameters.
  - ON-TEXT is called with each piece of streamed text. ON-DONE is called
    once with `(:text S :calls CALLS :model S)`. ON-ERROR is called once
    with `(:message S :kind KIND :status N)`, `:status` only when the
    provider gave one. Exactly one of ON-DONE and ON-ERROR is called, unless
    the request is cancelled, and then neither.
  - KIND says whether waiting may help: `rate-limit` (status 429 or a rate
    limit named), `timeout`, `server` (a status of 500 and above, an
    overloaded or unavailable server), `network` (the connection failed, or
    the stream ended with no response), `empty` (a response with neither
    text nor calls) and `other`: everything else, a quota or a billing limit
    among them. The kind is taken from the bridge's error message by the
    bridge's own classifier of passing failures; nothing above this module
    reads error text.
  - The `:model` of ON-DONE is the id of the model that was asked.
  - Every call the model made comes back in CALLS with the id the provider
    gave it and `:args` as a plist with keyword keys, also a call that names
    no known tool or lacks an argument: the caller judges. A call whose
    arguments the provider sent as text that is not a JSON object comes back
    with `:args` nil. A call the provider gave no id, or an id that a kept
    message, an assistant message of this request's MESSAGES or an earlier
    call of the same response already has, gets one made here.
  - A request waits ten minutes for the response to start and is tried once;
    trying again is the run's (L3).
  - A response that ends with the bridge's stop reason `error` goes to ON-ERROR
    with its error message; `aborted` calls neither; every other stop reason
    goes to ON-DONE, but a response with neither text nor calls, which goes
    to ON-ERROR with kind `empty`.
  - A provider that wants a tool it cannot call may write its own call
    markup as text, `<｜｜DSML｜｜ invoke ...>` for DeepSeek. No answer carries
    it: the text is cut where the markup starts, and a response left with
    nothing goes to ON-ERROR, kind `other`, its message saying the model
    wrote a call as text.
  - A signal raised while the request is started is delivered through
    ON-ERROR, kind `other`; `superchat-llm-request` itself signals a `user-error` only when
    `superchat-llm-provider` or the model is not set or not known, when
    `superchat-llm-reasoning` is not one of its values, or when the provider
    has no API key; the message says what to set.
  - When ON-TEXT signals, the request is aborted and ON-ERROR is called once
    with the error's message, kind `other`. An error raised by ON-DONE or ON-ERROR is the
    caller's to catch: it is not delivered anywhere.
  - The model's reasoning is never on the Tape and never in ON-DONE. This
    module keeps, in memory, the whole assistant message the bridge gave for a response
    that made calls, by the id and name of every call. When MESSAGES holds an
    assistant message with exactly those calls, the kept message is sent in
    its place, so the provider gets its reasoning back as the bridge sends it. A
    request whose last message is from the user drops what is kept for the
    calls in its MESSAGES: no provider wants the reasoning of an earlier
    turn, and another conversation's turn in progress keeps its own. At most
    64 messages are kept.
  - An assistant message with no kept message is rebuilt from the Tape: a
    plist argument is a JSON object, any other list an array. The Tape has
    nil for false, null and anything empty alike; nil is sent as false, and
    as an empty object when it is the whole argument list.
  - One request is one model response. When the model asks for tools the
    calls come back in ON-DONE; the caller runs them and makes another
    request with the results appended to MESSAGES.
- `(superchat-llm-cancel HANDLE)`: aborts the request through the bridge's abort
  signal.
- `(superchat-llm-model-ids)` → the model ids of `superchat-llm-provider`. A
  `user-error` says what to set when the provider is not set or not known,
  and names the entry of `superchat-llm-providers` whose function
  signalled.
- `(superchat-llm-context-window &optional MODEL)` → the context window, in
  tokens, of MODEL (default `superchat-llm-model`) of the provider, as the
  bridge's model data gives it; nil when the data has none, or when the
  provider or the model is not set or not known (the request reports
  those).
- `superchat-llm-providers` (a `defvar`): an alist from a provider name to a
  function of no arguments that returns a bridge provider. When a name is
  there twice the first entry counts, and a changed entry is seen at the
  next request. The model data is there whenever it is asked for: an entry
  whose function signalled is tried again, once, at the start of the next
  `superchat-llm-request`, `superchat-llm-model-ids` or
  `superchat-llm-context-window`, so a fixed entry is seen by `/model` too.
  It starts with
  one entry for each provider the bridge has (`"deepseek"`, `"groq"`,
  `"moonshotai"` ...); a plugin or the user adds to it. The bridge models
  collection is made from it on first use.
- A server on the user's own machine (LM Studio, Ollama) or any other
  OpenAI-compatible endpoint has no entry of its own. The user adds one:
  `(push (cons "ollama" (lambda () (superchat-bridge-openai-compatible-provider
  :id "ollama" :base-url "http://localhost:11434/v1" :models '("qwen3.5:9b"))))
  superchat-llm-providers)`. Such a provider needs no API key unless it is
  given `:api-key-env`.
- It calls the bridge only through what `bridge/lisp` files provide as public
  functions (no `--` name).

The provider is `superchat-llm-provider`, a name in `superchat-llm-providers`;
the model is `superchat-llm-model`, one of that provider's model ids; the API
key is found by the bridge (for DeepSeek, the environment variable
`DEEPSEEK_API_KEY`). Tests never touch the network: `superchat-llm-request-function`
(a `defvar`, default `#'superchat-llm--request`) is what `superchat-llm-request`
calls, and `test/helper.el` replaces it. The tests of this file give
`superchat-llm-providers` a provider whose API is a stub that returns scripted
bridge event streams.

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
- `(superchat-permission-set TAPE LEVEL &optional SCOPE)`: append that
  event; SCOPE is that of a timeline with no entry yet. A LEVEL that is not
  one of the three is an error.
- `(superchat-permission-decide TAPE CLASS)` → `allow`, `deny` or `ask`.
- `(superchat-permission-ask TOOL-NAME SUMMARY &optional TAPE)` → `allow`,
  `deny`, `always` or `later`, by calling `superchat-permission-ask-function`
  (a `defvar`) with TOOL-NAME and SUMMARY. The default reads one key in the
  minibuffer: y, n, or a for "always in this conversation". `later`: nobody
  can answer now.
- `(superchat-permission-answer TAPE CALL-ID ANSWER)` appends the `event`
  `(:type "answer" :call-id ID :answer NAME)`; ANSWER is `allow`, `deny` or
  `always`. `(superchat-permission-answered TAPE CALL-ID)` → the latest, a
  symbol, or nil.
- `superchat-permission-asking` (a `defvar`): while that function runs,
  `(:tape TAPE :tool TOOL-NAME :summary SUMMARY)`, else nil, also after it
  signalled; so what shows a run can say that it waits for the person's
  answer, not for the tool. `superchat-tool-call` gives its TAPE.

## superchat-tools.el

- `(superchat-tool-define NAME &key description args class function async deferred)`:
  ARGS has the shape `superchat-llm-request` takes. DESCRIPTION is a string,
  or a function of no arguments returning one, called each time the specs
  are asked for. FUNCTION is called with one plist of arguments and returns
  a string, or signals an error.
- A tool that waits for something outside Emacs (a process) is defined with
  `:async t`. Its FUNCTION is then called with the plist and a function
  DONE, and returns at once: either nothing, or a function of no arguments
  that stops the work. It calls DONE exactly once, later or before it
  returns, with the result string, or for an error with nil and the
  message: `(funcall DONE nil "what went wrong")`. A signal raised
  before it returns counts as an error result, also when DONE was already
  called: the signal is the result. Emacs stays usable while an
  asynchronous tool runs. `shell` and `grep` are asynchronous; the
  others are plain Lisp and return at once (`eval_elisp` runs Lisp, which
  cannot be made asynchronous: `C-g` interrupts it).
- `superchat-tool-tape` (a `defvar`): bound to the timeline name while a
  tool's function runs, for the tools that need it.
- A tool defined `:deferred t` is offered only on a timeline where
  `load_tools` loaded it: a `load` event `(:type "load" :tools NAMES)`, for
  good. `:deferred` defaults to `superchat-tool-defer-default` (a `defvar`,
  nil). `(superchat-tool-loaded TAPE)` → those names;
  `(superchat-tool-offered TAPE &optional EXCEPT)` → the tools not
  deferred and the loaded ones, but EXCEPT; `(superchat-tool-catalog TAPE)`
  → `More tools, offered once you load them with load_tools:` and a line
  `- NAME: FIRST-SENTENCE` per deferred tool not loaded, or nil.
- A tool `load_tools` (`names`, separated by commas or spaces), class
  `read`, not deferred, appends that event and answers `Loaded NAMES;
  offered from your next request.`; a name of no deferred tool is an error.
- `(superchat-tool-remove NAME)`, `(superchat-tool-specs &optional NAMES)` →
  the list `superchat-llm-request` takes.
- `(superchat-tool-call TAPE CALL ON-RESULT &optional CANCELLED OFFERED)` →
  nil, or a function of no arguments that stops a tool still running. With
  OFFERED, the names the request offered, a call of another known tool is an
  error result: `NAME is not loaded: call load_tools with its name first`
  for a deferred tool, else `NAME is not offered in this conversation`.
  CANCELLED is
  a function of no arguments, asked once permission is decided (after the
  user answered, when asked); when it returns non-nil, the tool does not run
  and its result is `Cancelled.`. It decides
  permission, asks when needed, runs the function with `default-directory`
  set to `(superchat-tape-directory TAPE)`, or, when that is nil,
  `(superchat-tools-scratch TAPE)`: a directory named after TAPE under
  `temporary-file-directory`, made when missing. With no working directory
  a relative path, or no path where one defaults to `.`, is an error: `This
  conversation has no working directory: give an absolute path`; a call that
  names a buffer and no path reads no default path. It appends the result as a `tool` entry and calls ON-RESULT with that entry,
  exactly once: before it returns for a synchronous tool, later for an
  asynchronous one. A call that names no known tool or lacks a required
  argument is answered first, with no question and no event. An `ask`
  decision is taken from the call's answer on the Tape when there is one.
  When the ask function returns `later`, nothing runs or is recorded and
  ON-RESULT gets the `event` `(:type "question" :call-id ID :tool NAME
  :summary S)` instead of a `tool` entry. For an `ask`
  or `deny` decision it appends a `permission` event once the decision is
  known, before the result, when the user was asked now or the call is
  denied; it says whether the call was allowed. A call that an earlier
  `always` or an answer on the Tape allows leaves no event. A quit
  while the user is asked leaves no `permission` event, only the result. When the returned function is called before the
  result came, the tool is stopped, nothing is appended and ON-RESULT is
  never called; a result that comes after that is dropped. A denied call, an unknown tool, a missing
  required argument and an error signalled by the function all become a
  `tool` entry with `:error t` whose text says what happened; so does a
  function that returns something that is not a string. A required argument
  is present when its key is in the plist, whatever its value. Nothing is
  swallowed and nothing is signalled to the loop. The function and the
  question to the user run with `inhibit-quit` bound to nil, because the
  callbacks of a real model run in a timer, where quitting is otherwise off;
  the entry is appended with quitting off again. A quit (`C-g` while the
  tool runs or while the user is asked) is such an entry too, with the text
  `Interrupted by the user.`
- `superchat-tool-call` remembers an `always` answer for that tool name in
  that timeline until Emacs exits, in a hash table, not on the Tape.
- Results longer than `superchat-tool-max-chars` (51200) are cut, and the
  text says how many characters were left out. A tool that cuts its own
  output keeps its text within that limit less a quarter of it, at most 1000 characters, the room
  left for its notices, so that this cut never takes a notice.
- A notice is one line in brackets. A tool's notices follow its text after
  a blank line, one a line; with no text they are the whole result. A
  notice of a cut names the call that gets what was left out.
- A tool that reads from a process never holds more of its output than a
  few times `superchat-tool-max-chars`: past that `grep` is stopped and
  `shell` writes what lies before the tail it keeps to its file. A process
  buffer is killed whatever happens, also when the process cannot be
  started.

Built-in tools, and no others:

| name | class | arguments | does |
|---|---|---|---|
| `read` | read | `path` or `buffer`, `offset` opt, `limit` opt | lines of a file or a buffer, or the entries of a directory, as Reading says |
| `grep` | read | `pattern`, `path` opt, `glob` opt, `ignore_case` opt, `literal` opt, `context` opt, `limit` opt | matching lines of files, as Searching says; asynchronous |
| `find` | read | `pattern`, `path` opt, `limit` opt | files whose path matches the glob PATTERN, as Searching says |
| `edit` | write | `path` or `buffer`, `old`, `new` | replace OLD by NEW and save, as Changing says |
| `write` | write | `path`, `content` | CONTENT becomes the whole text of the file PATH, saved |
| `eval_elisp` | exec | `code` | read every form in CODE, evaluate in order with `debug-on-error`, `debug-on-signal` and `debug-on-quit` nil, return what was printed to `*Messages*` meanwhile, then the value of each form printed, one line each in order, a newline inside a value written `\n`; an error is the call's result, never the debugger, which would hold the run until the person leaves it |
| `shell` | exec | `command`, `timeout` opt | run through `shell-file-name` with no input, at most TIMEOUT seconds (default `superchat-shell-seconds`), return the end of the output and the exit status, as Running says; asynchronous |

The descriptions of the tools state the division: files through `read`,
`grep`, `find`, `edit` and `write`, and that `edit` and `write` save the
file; outside programs through `shell`; the running Emacs through
`eval_elisp`. The description of `shell` says never to read, list or
search files with it (`cat`, `head`, `tail`, `sed -n`, `ls`, `find`,
`grep`) and names the tool for each. The description of `grep` says its
PATTERN is POSIX extended syntax, with no inline flags such as `(?i)`
(IGNORE_CASE is the way), and that CONTEXT shows the lines around a match
without another `read`; that of `find` says a name pattern such as
`*diary*` finds a file at any depth, so no directory needs walking; that
of `read` says a read with LIMIT 1 gives a file's line count
in its notice. A boolean argument is false when it is
nil (a JSON false).

A path is relative to the scope; `~` and absolute paths work. A call of a
`read` tool that reads outside the scope and outside `superchat-directory`
(where the skills are) is decided as a `write` call. What it reads is the
file or directory its `path` argument names, the project when a tool that
takes a `path` is given none, and the file a buffer named by its `buffer`
argument visits. A path is checked as the tool will open it, with symbolic
links resolved, and given to the tool in that form, so a link, a `..` or a
leading `-` cannot make the tool read another file than the one checked. A
buffer that visits no file is Emacs's own and inside. Such a call: at `ask` the user is asked, at `read-only` it is
denied, at `auto` it runs. `"home"` is no project, so there only
`superchat-directory` is inside. The `permission` event and the denial say that
the call was outside the project.

### Reading

- Files are read and changed through Emacs buffers. `read` reads the buffer
  visiting the file when there is one, so unsaved edits are seen, else the
  file in a temporary buffer: a read leaves no buffer open. `read` takes
  exactly one of PATH and BUFFER.
- `read` returns the lines from OFFSET (1-indexed, default
  1), with no line numbers, at most LIMIT lines, 2000 lines and the room of
  a result, whichever comes first. A line is never cut, except a single
  line longer than the room, of which the room's worth is shown with a
  notice. The lines of a text are as `count-lines` counts them: a final
  newline does not start another line.
- With lines left after those shown, one notice names the next offset:
  `[Showing lines X-Y of Z. Use offset=N to continue.]` when 2000 lines
  stopped it; `[Showing lines X-Y of Z (N characters limit). Use offset=N to continue.]`
  when the room did; `[K more lines in file. Use offset=N to continue.]`
  (`in buffer` for a buffer) when LIMIT did.
- An OFFSET past the last line is an error,
  `Offset N is beyond end of file (Z lines total)` (`of buffer`); offset 1
  of an empty text is the empty result.

### Searching

- `grep` and `find` take PATH, default the project. The files
  `grep` and `find` look at are the project's files as `project-files`
  gives them (so `.gitignore` counts) when PATH is the project or inside
  it, else every file under PATH by `directory-files-recursively`; nothing
  under a `.git`. A GLOB without `/` matches a file's name at any depth,
  one with `/` its path relative to PATH; `*` and `?` stay within a
  directory, `**` crosses them, `{a,b}` is either, `[...]` a class.
- `grep` filters those files by GLOB in Lisp, leaves out symbolic links
  (which it would follow out of PATH, as `grep -r` does not), and runs the
  `grep` program on what is left, after `--`, a batch at a time so that no
  command line is too long, with an extended regular expression, or a
  fixed string with LITERAL, case ignored with IGNORE_CASE, CONTEXT lines
  before and after; binary files are skipped. A PATH that is a file is
  searched alone. A match is `PATH:LINE: TEXT`, a context line
  `PATH-LINE- TEXT`, PATH relative to the searched directory; a line longer
  than 500 characters is cut. At most LIMIT matches (default 100), with
  their context, and the room of a result, in whole lines.
- The notices of `grep`:
  `[N matches limit reached. Use limit=2N for more, or refine pattern]`,
  `[N characters limit reached]`,
  `[Some lines truncated to 500 chars. Use read to see full lines]`; no
  match is `No matches found`. Listing and searching stop after 10
  seconds, and a notice says so and to narrow PATH or GLOB. A PATH that
  does not exist is an error.
- `find` lists the files PATTERN matches, as a GLOB, relative to PATH and
  sorted, one a line, a directory with `/` where the listing has it; at
  most LIMIT (default 1000):
  `[N results limit reached. Use limit=2N for more, or refine pattern]`;
  none is `No files found matching pattern`. Its listing stops after 10
  seconds too, with the same notice.
- `read` of a directory lists its entries, dotfiles included, sorted
  alphabetically ignoring case, `/` after a directory, at most LIMIT
  (default 500): `[N entries limit reached. Use limit=2N for more]`; an empty
  one is `(empty directory)`.
- A list that passes the room of a result keeps whole lines, with
  `[N characters limit reached]`.

### Changing

- `edit` and `write` change the buffer visiting the file, visiting it when
  none does, then save it, so one call changes the file and the person can
  undo the change in that buffer. `edit` takes exactly one of PATH and
  BUFFER; OLD must occur exactly once, case and overlapping occurrences
  counted; a PATH that names no file and no buffer is an error, and a buffer
  visited for an edit that fails is killed again. Its result is
  `Replaced 1 block in PATH.`, BUFFER in place of PATH for a buffer.
  `write` creates a file that does not exist; its result is
  `Wrote N characters to PATH.`. A file a tool visits gets only the local
  variables the person already trusts (`enable-local-variables` `:safe`):
  a question there would hold Emacs.
- The save makes the file's directory when needed, writes a file that does
  not exist even when the buffer is empty, adds no final newline, leaves
  no backup file, whatever the person's Emacs says, and shows no message. A buffer that visits no
  file is not saved. A buffer that held unsaved changes before the call is
  changed and not saved, with the notice
  `Not saved: the buffer held unsaved changes of the person's; they save it`.
  When the save changes the buffer's text (a hook such as a formatter), the
  notice `Saving changed the text (a hook of the person's, such as a
  formatter); read it before the next edit` follows the result.

### Running

- `shell` keeps the end of the output, where the errors are: at most 2000
  lines or the room of a result, whole lines but for a single line longer
  than the room, of which the end is kept. When it cuts, the whole output
  is in a file made by `make-temp-file "superchat-shell-"`, and the notice
  is `[Showing lines X-Y of Z. Full output: FILE]`. Once the output held
  passes four times `superchat-tool-max-chars`, what lies before its last
  `superchat-tool-max-chars` characters is appended to that file as it
  comes. When that file passes 200 times `superchat-tool-max-chars`, the
  command is stopped. The last notice is `[exit status N]`,
  `[timed out after N seconds and killed]`, or
  `[output passed N characters; the command was stopped]`.

## superchat-lint.el

After `edit` or `write` saves a file, the checks run in its buffer and what
they find follows the tool's result as notices, `[Lint: …]`, so the model
fixes it in the same round. A lint never refuses a change.

- `(superchat-lint-check FILE)` → the lines found, each `Lint: …`: built
  in, an Emacs Lisp buffer whose brackets or strings do not balance
  (`check-parens`, with the line), and up to three lines holding a control,
  private-use or replacement character; then each function of
  `superchat-lint-functions` (a `defvar`), called with FILE, returning nil or
  lines. A check that signals is reported as a line, not raised.
- Before those functions, the buffer's `superchat-lint-project-command` (a
  `defvar`, nil), set by a project's `.dir-locals.el` and not safe as a
  local variable, runs as `superchat-lint-command` would for every file, in
  the directory of that `.dir-locals.el`.
- `(superchat-lint-command PATTERN COMMAND)` → such a function: for a file
  matching the regexp PATTERN, run the shell COMMAND, `%s` in it the quoted
  file name; an exit other than 0 is a line with the command, its status and
  the last 10 lines of its output. It is stopped after
  `superchat-lint-seconds` (a `defvar`, 20).

## superchat-recall.el

Three tools, defined with `superchat-tool-define`, all of class `read` and
deferred:
`note` only appends to the Tape, so it never asks.

| name | arguments | does |
|---|---|---|
| `recall` | `view`, `query` opt, `name` opt, `id` opt, `limit` opt | run a built-in or plugin view for the current scope and return `superchat-view-render` of it, at most 6000 characters |
| `tape_query` | `sql` | `superchat-tape-select`, rows as JSON lines, at most 200 rows; the limit is part of the statement that runs, so a query that would return a million rows does not fetch them |
| `note` | `title`, `text`, `sources` (comma-separated entry ids) | append a `note` entry to the current timeline; a note among its sources is corrected by it (L1, `notes`), as its description says |

- `(superchat-recall-note TAPE TITLE TEXT &optional REFS SCOPE)` → the
  `note` entry appended to TAPE; SCOPE is needed only for a timeline with no
  entry yet. The `note` tool and the person (`superchat-note`, `/note`)
  write notes through this one function.

The description of `recall` lists the views and their arguments from
`superchat-view-names`, each time the specs are asked for. The description of
`tape_query` gives the table definition.
