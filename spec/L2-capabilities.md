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
    with a string. Exactly one of ON-DONE and ON-ERROR is called, unless the
    request is cancelled, and then neither.
  - The `:model` of ON-DONE is the id of the model that was asked.
  - Every call the model made comes back in CALLS with the id the provider
    gave it and `:args` as a plist with keyword keys, also a call that names
    no known tool or lacks an argument: the caller judges. A call whose
    arguments the provider sent as text that is not a JSON object comes back
    with `:args` nil. A call the provider gave no id, or an id that a kept
    message, an assistant message of this request's MESSAGES or an earlier
    call of the same response already has, gets one made here.
  - A request waits ten minutes for the response to start and is tried up to
    two more times when the provider says to try again, as pi's own client
    does.
  - A response that ends with the bridge's stop reason `error` goes to ON-ERROR
    with its error message; `aborted` calls neither; every other stop reason
    goes to ON-DONE.
  - A signal raised while the request is started is delivered through
    ON-ERROR; `superchat-llm-request` itself signals a `user-error` only when
    `superchat-llm-provider` or the model is not set or not known, when
    `superchat-llm-reasoning` is not one of its values, or when the provider
    has no API key; the message says what to set.
  - When ON-TEXT signals, the request is aborted and ON-ERROR is called once
    with the error's message. An error raised by ON-DONE or ON-ERROR is the
    caller's to catch: it is not delivered anywhere.
  - The model's reasoning is never on the Tape and never in ON-DONE. This
    module keeps, in memory, the whole the bridge assistant message of a response
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
  bridge's model data gives it; nil when the data has none.
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
The bridge event streams.

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
  a string, or signals an error.
- A tool that waits for something outside Emacs (a process) is defined with
  `:async t`. Its FUNCTION is then called with the plist and a function
  DONE, and returns at once: either nothing, or a function of no arguments
  that stops the work. It calls DONE exactly once, later or before it
  returns, with the result string, or for an error with nil and the
  message: `(funcall DONE nil "what went wrong")`. A signal raised
  before it returns counts as an error result, also when DONE was already
  called: the signal is the result. Emacs stays usable while an
  asynchronous tool runs. `shell` and `search_files` are asynchronous; the
  others are plain Lisp and return at once (`eval_elisp` runs Lisp, which
  cannot be made asynchronous: `C-g` interrupts it).
- `superchat-tool-tape` (a `defvar`): bound to the timeline name while a
  tool's function runs, for the tools that need it.
- `(superchat-tool-remove NAME)`, `(superchat-tool-specs &optional NAMES)` →
  the list `superchat-llm-request` takes.
- `(superchat-tool-call TAPE CALL ON-RESULT &optional CANCELLED)` → nil, or
  a function of no arguments that stops a tool still running. CANCELLED is
  a function of no arguments, asked once permission is decided (after the
  user answered, when asked); when it returns non-nil, the tool does not run
  and its result is `Cancelled.`. It decides
  permission, asks when needed, runs the function with `default-directory`
  set to the timeline's scope (the user's home directory for `"home"`),
  appends the result as a `tool` entry and calls ON-RESULT with that entry,
  exactly once: before it returns for a synchronous tool, later for an
  asynchronous one. A call that names no known tool or lacks a required
  argument is answered first, with no question and no event. For an `ask`
  or `deny` decision it appends a `permission` event once the decision is
  known, before the result; it says whether the call was allowed. A quit
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
- Results longer than `superchat-tool-max-chars` (20000) are cut, and the
  text says how many characters were left out.
- A tool that reads from a process never holds more of its output than a
  few times `superchat-tool-max-chars`: past that the process is stopped
  and the result says the output was cut there. A process buffer is killed
  whatever happens, also when the process cannot be started.

Built-in tools, and no others:

| name | class | arguments | does |
|---|---|---|---|
| `read_file` | read | `path`, `start` opt, `count` opt | lines of a file with line numbers; at most 400 lines a call |
| `read_buffer` | read | `buffer`, `start` opt, `count` opt | the same for a buffer |
| `list_buffers` | read | none | name, mode, file and size of the user's buffers (no names starting with a space) |
| `search_files` | read | `pattern`, `path` opt | `grep -rn` under the scope, skipping `.git` and binary files; it is stopped at 100 matching lines or after 10 seconds, and the result says which; asynchronous |
| `edit_buffer` | write | `buffer`, `old`, `new` | replace OLD by NEW in a buffer; OLD must occur exactly once, case and overlapping occurrences counted; does not save |
| `write_file` | write | `path`, `content` | create or replace a file, through a buffer, and save it |
| `save_buffer` | write | `buffer` | save |
| `eval_elisp` | exec | `code` | read every form in CODE, evaluate in order, return the last value printed, with what was printed to `*Messages*` meanwhile |
| `shell` | exec | `command` | run through `shell-file-name` with no input, at most `superchat-shell-seconds` (60), return the command's output and exit status and nothing else; asynchronous |

A path is relative to the scope. A call of a `read` tool whose `path`
argument names a file outside the scope, once `..` and symbolic links are
resolved, is decided as a `write` call: at `ask` the user is asked, at
`read-only` it is denied, at `auto` it runs. `"home"` is no project, so
there every path is outside. The `permission` event and the denial say that
the call was outside the project. A file a tool edits is edited through its
buffer, so the user can undo it.

## superchat-recall.el

Three tools, defined with `superchat-tool-define`, all of class `read`:
`note` only appends to the Tape, so it never asks.

| name | arguments | does |
|---|---|---|
| `recall` | `view`, `query` opt, `name` opt, `id` opt, `limit` opt | run a built-in or plugin view for the current scope and return `superchat-view-render` of it, at most 6000 characters |
| `tape_query` | `sql` | `superchat-tape-select`, rows as JSON lines, at most 200 rows; the limit is part of the statement that runs, so a query that would return a million rows does not fetch them |
| `note` | `title`, `text`, `sources` (comma-separated entry ids) | append a `note` entry to the current timeline |

- `(superchat-recall-note TAPE TITLE TEXT &optional REFS SCOPE)` → the
  `note` entry appended to TAPE; SCOPE is needed only for a timeline with no
  entry yet. The `note` tool and the person (`superchat-note`, `/note`)
  write notes through this one function.

A number argument may arrive as a string. The description of `recall` lists the views and their arguments from
`superchat-view-names`, each time the specs are asked for. The description of
`tape_query` gives the table definition.
