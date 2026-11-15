# Superchat — specification

This file and the layer specifications it links are the whole contract. If
something is not in them, it is not part of Superchat. Do not add it. Each
fact has one home: a rule that holds for every layer is here, a rule of one
module is in the specification of its layer.

## What Superchat is

An Emacs package with three uses:

1. **The AI layer of Emacs.** Any buffer, command or package can ask a model
   to do something through one function, `superchat-run`. The model acts on
   Emacs through tools, the most general being `eval_elisp`.
2. **A coding assistant.** It reads and edits buffers and files of a project
   under a permission level the user sets.
3. **A personal agent.** It keeps everything that happened on the Tape and
   recalls it across conversations, in and outside projects.

## Layers

Superchat is built one layer at a time. A layer uses only the public
functions and variables of the layers below it, never one of their private
(`--`) names; a layer above it never reaches into it either.

| layer | name | specification | serves |
|---|---|---|---|
| L0 | base | this file, `superchat.el`; [`bridge/README.md`](bridge/README.md) for the bridge | every layer |
| L1 | memory | [`spec/L1-memory.md`](spec/L1-memory.md) | use 3, the personal agent |
| L2 | capabilities | [`spec/L2-capabilities.md`](spec/L2-capabilities.md) | use 2, the coding assistant |
| L3 | agent | [`spec/L3-agent.md`](spec/L3-agent.md) | use 1, `superchat-run` |
| L4 | interface | [`spec/L4-interface.md`](spec/L4-interface.md) | the person at the keyboard |

Work in one layer reads this file and that layer's specification, all of
them, and of the layers below only the public functions it calls. It edits
only the files of that layer, their tests and its specification. A need in
another layer is one line in `NOTES.md`, starting with that layer's name.

## Modules

Each module belongs to one layer. `scripts/check.sh` reads this table.

| layer | file | cap | owns |
|---|---|---|---|
| L0 | `superchat.el` | 120 | package header, group, user options, version |
| L1 | `superchat-tape.el` | 550 | the database, append, read, search |
| L1 | `superchat-view.el` | 500 | the view registry and the built-in views |
| L2 | `superchat-llm.el` | 450 | the only file that knows the bridge |
| L2 | `superchat-permission.el` | 250 | levels, the decision for one tool call, asking |
| L2 | `superchat-tools.el` | 800 | the tool registry, running a tool, the built-in tools |
| L2 | `superchat-recall.el` | 200 | the three tools the model uses on the Tape |
| L3 | `superchat-context.el` | 250 | the system prompt: rules files, skills catalog, notes, origin |
| L3 | `superchat-agent.el` | 700 | `superchat-run`: the loop, cancel, handoff |
| L4 | `superchat-chat.el` | 1100 | the chat buffer and slash commands |
| L4 | `superchat-inline.el` | 600 | rewrite a region in place; ask about a region |
| L4 | `superchat-plugin.el` | 150 | define, enable, disable a plugin |

Dependencies go one way only:

```
inline → chat
chat → agent, plugin, recall
agent → llm, tools, view, context
recall → tools, view
context → view
tools → permission
permission, view → tape
every file → superchat.el
```

`superchat.el` requires no other Superchat file. Every other file requires it
for the user options. Arrows carry on: a file may require what the files it
points to require. A file requires nothing else, and
`superchat-llm.el` and `superchat-plugin.el` require only `superchat.el`;
`superchat-llm.el` is the only file that requires a `superchat-bridge` file.

## Hard limits

- Product code: **6,000 lines in total**, counted by `wc -l` over the `.el`
  files at the repository root. Tests are not counted. Each file has its own
  cap in the layer table. `scripts/check.sh` fails when a cap is exceeded.
- Emacs 30.1 or later. Dependencies: `curl`, the `bridge/` directory of this repository, and what Emacs ships with
  (`sqlite`, `project`, `cl-lib`, `subr-x`, `seq`, `json`). Nothing else.
- Every file starts with `;;; NAME.el --- ... -*- lexical-binding: t; -*-` and
  byte-compiles with no warnings.

## Rules against overflow

These are checked in review. Code that breaks one is removed.

1. No feature, option, command, tool, hook, keyword argument or event that
   this specification does not name. If you believe something is missing, do not build
   it: write one line in `NOTES.md` (what, why) and go on.
2. No `defcustom` other than those listed under "User options".
3. No compatibility code, no migration code, no "legacy" branch, no
   deprecated alias. There is no earlier version.
4. No check for whether Superchat's own functions or variables exist
   (`fboundp`, `boundp`, `bound-and-true-p`, `featurep`). `require` the file.
5. No `ignore-errors`, and no `condition-case` that discards an error. An
   error is either signalled to the caller or recorded and shown to the user.
   Catch an error only where this specification says what happens to it.
6. No extension point that has no user in this repository: no hook variable,
   no generic function, no registry beyond the three it names (tools,
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

## superchat.el

Package header, `defgroup superchat`, the user options and the constant
`superchat-version` (`"2.0.0"`). Nothing else.

## User options

`superchat-directory` (`superchat/` in the user's Emacs directory),
`superchat-tape-file` (nil, meaning `tape.sqlite` in that directory), `superchat-llm-provider` (nil; a name such as `"deepseek"`; using Superchat
without one is an error that says how to set it), `superchat-llm-model` (nil;
a model id of that provider such as `"deepseek-flash"`; required),
`superchat-llm-reasoning` (nil: the model does not reason; else one of the bridge's
thinking levels `minimal`, `low`, `medium`, `high`, `xhigh`, `max`, a symbol,
which the bridge clamps to what the model has), `superchat-default-level` (`ask`), `superchat-context-chars`
(nil: follow the model's context window; or a number of characters), `superchat-max-rounds` (40), `superchat-tool-max-chars` (20000),
`superchat-shell-seconds` (60), `superchat-plugins` (nil).

## Tests

ERT, one file per module in `test/`, named `test-NAME.el`. `test/helper.el`
gives `superchat-test-with-tape` (a fresh database in a temporary directory,
closed and deleted afterwards) and `superchat-test-script` (bind
`superchat-llm-request-function` to answer from a list of scripted
responses, synchronously by default; with `superchat-test-async` bound to t
each response is delivered from a timer, as a real model's is, and
`superchat-test-drain` waits for the timers). Tests do not use the network, the
user's home directory or timers longer than 0.1 s. Test what the specification
promises, by behaviour. Do not test private functions.

A test never deletes a directory it did not make itself with
`make-temp-file`, never binds `superchat-directory` or `default-directory`
to the repository, and never lets a scripted model run the `shell` or
`eval_elisp` tool on a path outside a temporary directory.

`scripts/check.sh` compiles every file with warnings as errors, runs every
test in a copy of the repository under the temporary directory, and checks
the caps. It must pass before work is handed back.

## Gates

`scripts/check.sh [L1|module ...]` runs, with no argument, everything:
`scripts/gates.el`, then compiling, then the tests; with a layer, the tests
of that layer's modules. The gates check what can be checked by reading the
files: the caps and the total, that every `.el` file at the root is a module
of the table, the dependency arrows and the layer order, that no file uses a
private name of a module of another layer, the banned forms of the rules
against overflow, and that every link in the specification files leads to a
file. A test may use a private name of a module of its own layer. A use
across layers that is not yet removed is listed, with its reason, in
`scripts/gate-exceptions`; a line there that no longer matches anything is
an error too, so the list only shrinks. Before it checks the repository,
each gate is shown an invalid case and must reject it.
