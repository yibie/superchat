# L3 agent: `superchat-run`

Part of the [specification](../SPEC.md). Modules: `superchat-context.el`,
`superchat-agent.el`, `superchat-request.el`. This layer is the AI layer of Emacs: any package asks
a model through `superchat-run`. It uses L1 and L2. A package that requires
`superchat-agent` and nothing else gets every built-in tool, `recall`,
`tape_query` and `note` among them.

## superchat-context.el

- `(superchat-context-system TAPE &optional EXTRA)` → the system prompt, built
  fresh for each request, in this order:
  1. the base instructions, a constant of at most 40 lines;
  2. the working directory: the scope, or the home directory for
     `"home"`, with the line that a tool's relative path is against it and
     that `shell` runs there;
  3. the rules file of the scope: `AGENTS.md` at the project root, or
     `AGENTS.md` in `superchat-directory` for `"home"`, when it exists;
  4. the skills catalog: for each `NAME/SKILL.md` under
     `superchat-directory`/`skills/`, its name and the `description:` line of
     its frontmatter, with the path, so the model reads the file with
     `read` when it wants it. A file whose first `---` line is never
     closed has no frontmatter;
  5. the `notes` view of the scope, newest first, rendered, at most 2000
     characters;
  6. the origin of the latest `user` entry, when it has one;
  7. EXTRA, a string, when given.
- A timeline with no entry has no scope: its prompt has no working
  directory line, no rules file and no notes section.
- Frontmatter is only at the very start of a file and ends at the first line
  that is exactly `---`.

## superchat-agent.el

### A run

- `(superchat-run TAPE TEXT &key scope origin tools system model ephemeral unattended refs on-text on-entry on-done)`
  → the run, a plist. It appends TEXT as a `user` entry of timeline TAPE and
  answers it.
  - SCOPE: the scope of the first entry of a new timeline; the scope of
    `default-directory` when not given. A timeline that has entries keeps
    its scope.
  - ORIGIN: becomes `:origin` of the `user` entry's data.
  - TOOLS: a list of tool names, nil for every tool, or `none`.
  - SYSTEM: the EXTRA of `superchat-context-system`.
  - MODEL: the model of every request of the run and of its handoff; nil
    for `superchat-llm-model`.
  - EPHEMERAL: when non-nil, `:ephemeral t` in the `user` entry's data.
  - REFS: the `:refs` of the `user` entry.
  - UNATTENDED: when non-nil, nobody is asked: a call that needs asking
    comes back as a `question` (L2) and the run ends `waiting`.
  - ON-TEXT: gets each piece of streamed text.
  - ON-ENTRY: gets each entry of the run as it is appended.
  - ON-DONE: gets the run once, with `:status` `done`, `cancelled`,
    `waiting` or `error`, `:text` the text of the last `assistant` entry of this run (nil
    when it made none) and, for `error`, `:error` the text of the error
    event.
- First, if the timeline's last run never ended (Emacs stopped during it),
  the run appends, for each of its calls with no result, a `tool` entry with
  `:error t` and the text
  `Interrupted: Emacs stopped while this call ran; it may have had effects.`,
  then an `interrupted` event; nothing is run or requested again. A run never
  ended when, after the latest `user` entry, no `error` or `cancelled` event
  comes and the last entry but events and notes is a `tool` entry, an
  `assistant` entry with calls, or that `user` entry, unless an `error` event
  comes right before it (a failed handoff).
- After the `user` entry the run loops:
  1. it builds the messages (`superchat-request.el`), or ends with the refusal;
  2. `superchat-llm-request` with the messages, `superchat-context-system`
     and the tool specs of TOOLS;
  3. on done it appends the `assistant` entry. If it has calls, it runs each
     with `superchat-tool-call`, one after the other in order, each started
     when the one before has its result, then goes to 1. If it has none,
     the run ends with status `done`.
- The calls of one response are run in a loop, not by recursion: a response
  with hundreds of calls is no deeper than one with two. The run does not
  block Emacs while an asynchronous tool works: the user can type, switch
  buffers and cancel.
- An entry `superchat-tool-call` appended (a permission event, a result) is
  given to ON-ENTRY as soon as that call returns, also when the tool is
  asynchronous and its result comes later.
- When `superchat-max-rounds` is a number (nil sets no limit), after that
  many model responses in one run, the run asks the
  model once more, its tools offered, a system line saying the limit is
  reached, to call none, to begin with `Status: finished`, `blocked` or
  `continue`, and to look back over the run's calls: what is done, what
  failed and why, what is left, the plan for the next step; then the facts
  `superchat-request-digest` counts over the run's entries. It records the
  answer, less its status line, as an `assistant` entry with no calls.
  `finished` ends the run `done`. Else, at the `auto`
  level, not `blocked`, at most `superchat-agent-continues` (a `defvar`, 2)
  times a run, the run goes on: an `event` `(:type "continued" :count N :status S)`,
  the count starts again, and the next request's system prompt
  says to follow the plan, with the facts. Otherwise it ends `error`, the event counting the calls the run made
  on the Tape, so the account is there even when the model says nothing. The
  calls of the last response run first.
- A failed request of kind `rate-limit`, `server`, `timeout`, `network` or
  `empty` is retried: the run appends an `event` with `:data`
  `(:type "retry" :kind :status :wait :attempt :of)` (KIND a string) and a
  line, waits, and goes back to step 1. `superchat-agent-retry-waits` (a
  `defvar`, `(2 4 8 16 32)`) holds a wait in seconds per retry; the count
  restarts when a request answers. Tool calls and handoffs are never
  retried. A run that waits is running.
- Any other failure, or one with no wait left, appends an `error` event with
  its message and ends the run with status `error`.
- A run that ends `waiting` runs no more calls and gives none a result.
  `(superchat-question TAPE)` → the `question` event still without an
  `answer` event or result for its call, or nil; `superchat-run` on such a
  timeline is an error. `(superchat-answer TAPE ANSWER &key unattended
  on-text on-entry on-done)` → a run: it appends the answer
  (`superchat-permission-answer`), runs that call and the rest of its
  response, then goes on as any run, with no new `user` entry. With no
  question waiting, or a run going, it is an error.
- At most one run per timeline. `superchat-run` on a timeline that has one is
  an error. A handoff occupies its timeline like a run: while it is pending,
  `superchat-run` and a second handoff are errors, and `superchat-handoff`
  on a timeline with a run is an error.
- `(superchat-run-active TAPE)` → nil when the timeline is free; while a
  handoff is pending, a plist whose `:status` is `handoff`; while a run is
  going, the run, whose `:status` is `running` (`cancelled` while a cancelled
  run waits for a call to return from `superchat-tool-call`).

### How a run ends

- A run always ends, and there is one way it ends, whatever ends it: done,
  the refusal, an error, a cancel, or a callback that signals. Whatever is
  signalled while a request is started or inside a callback of the run
  becomes an `error` event and the run ends with status `error`.
- A signal inside `superchat-tool-call` is recorded as that `error` event
  and the run ends with status `error`; it is not raised to the caller or the
  timer. A throw or a quit out of `superchat-tool-call` ends the run with
  status `error` (`cancelled` when it was cancelled meanwhile), the event
  saying that the call was interrupted, and goes on to where it is caught.
- When a run ends, in this order: a wait for a retry is stopped; a tool of
  it that has not answered is stopped; every call of its responses gets a result, a `tool` entry with
  `:error t` and the text `Cancelled.` for one that has none; the `user`
  entry is appended if it was not (before the `cancelled` event, after the
  `error` event); the `cancelled` or `error` event is appended; ON-ENTRY gets
  the entries the ending appended; the timeline is freed; ON-DONE is called
  exactly once. ON-DONE may start the next run, ON-ENTRY may not. After
  that nothing of the run is appended and nothing is given to its ON-ENTRY.
  A run only ever adds results for calls of its own responses. A step of the
  ending that signals (stopping the request or a tool) is recorded as an
  `error` event and the ending goes on, with the status it had.
- When ON-ENTRY signals on an entry the ending appended, the ending goes on,
  and after ON-DONE the first such signal is raised to whatever ended the
  run: the caller of `superchat-run` or `superchat-cancel`, or the timer or
  process that delivered a response or a tool's result. A later signal of
  the same ending is shown with `message`. An error raised by ON-DONE goes
  to the same place; when an ON-ENTRY error of the ending is pending then,
  ON-DONE's error is raised and the ON-ENTRY error is shown with `message`.
- ON-ENTRY may cancel the run: nothing more is requested or run, and ON-DONE
  is still called exactly once. A cancel while the run is ending (ON-ENTRY
  given an entry the ending appended) does nothing: the status stays what
  the ending set.
- `(superchat-cancel TAPE)` for a run: cancel its request or its wait, stop a tool that
  is running through the function `superchat-tool-call` returned, run no
  call of the current response that has not started, and end the run with
  status `cancelled`. The request is cancelled only while it has not
  answered: its handle is forgotten when its ON-DONE or ON-ERROR comes. A
  tool is stopped only while it has not answered. When the run was cancelled
  during the handoff that comes first, its `user` entry is appended before
  the `cancelled` event.
- A run cancelled while one of its calls is inside `superchat-tool-call`
  (the user is being asked, or a plain tool is running) keeps its timeline
  until that call returns; only then does it end, so nothing of a cancelled
  run is ever appended after a later run's entries. A tool whose permission
  question is answered after the cancel does not run; its result is
  `Cancelled.`.
- `(superchat-cancel TAPE)` for a pending handoff: cancel its request, append
  a `cancelled` event and free the timeline. The handoff's ON-DONE is not
  called, and an answer that comes later is dropped.

### Handoff and fork

- `(superchat-handoff TAPE &optional NAME on-done)` → the handle of the
  request. It asks the model, with no tools, for a summary of the
  `conversation` view and for what comes next, and appends an anchor named
  NAME (default `"handoff"`) with them. The request is one `user` message: the
  text given to a handoff, then the question, which asks for a summary of at
  most a quarter of the budget and a line that starts `NEXT:` saying what
  comes next. ON-DONE is called with the anchor, or with nil when the model
  failed; an `error` event is appended then. When starting the request
  signals, the timeline is freed and the signal goes to the caller.
- The text given to a handoff begins with the conversation's current anchor
  message, when there is one, cut to the budget. Then comes
  `superchat-view-render` of the entries after the anchor, to what is left of
  the budget. The anchor's `:refs` are that anchor and the entries the render
  kept; the render's `[N earlier entries left out]` line says how many of
  the oldest it left out.
- In the answer, the text before the first line that starts with `NEXT:` is
  the summary and the rest is the next step. Each is cut to a quarter of the
  budget. An answer with no summary is a failure: only `superchat-fork`
  writes an anchor with refs and an empty text.
- When `superchat-run` is called and the messages of the timeline are
  already larger than the budget, a handoff is made first, with the run's
  MODEL, and the `user` entry is appended after the anchor. When that handoff
  fails, or starting its request signals, the `user` entry is appended after
  the error event and the run ends with status `error`. There is no handoff
  in the middle of a run.
- `(superchat-fork TAPE ENTRY-ID)` → the name of a new timeline of TAPE's
  scope. ENTRY-ID must be the id of an entry of TAPE's `conversation` view,
  else it is an error. The new timeline's first entry is an anchor named
  `"fork"` with an empty summary and next step; its `:refs` are the ids of
  the entries of that view with an id up to ENTRY-ID (id ≤ ENTRY-ID), and of
  the `tool` entries that answer the calls of every `assistant` entry among
  them.

## superchat-request.el

The messages of a request, built from entries; the run of
`superchat-agent.el` calls it for every request, the refusal and the
handoff. `(superchat-request-message ENTRY)` → an entry's message or nil;
`(superchat-request-budget &optional MODEL)` → the budget;
`(superchat-request-messages ENTRIES)` → all their messages;
`(superchat-request-size MESSAGES)` → their size;
`(superchat-request-fit ENTRIES USER-ID BUDGET)` → the messages that fit;
`(superchat-request-kept-size ENTRIES USER-ID)` → the size of what is never
left out; `(superchat-request-digest ENTRIES)` → lines counting their
calls: per tool, failed, files changed, repeated calls, last 3 failures.

- The messages of a request are the `conversation` view of TAPE turned into
  neutral messages:
  - a `user` entry is a `user` message; an `assistant` entry an `assistant`
    message with its text and calls; a `tool` entry a `tool` message with its
    call id and name;
  - an anchor with an empty text gives no message; any other anchor is a
    `user` message, `Summary of the conversation so far:\n` and its summary,
    then, when its `:next` is not empty, `\n\nWhat comes next:\n` and that;
    an entry of any other kind gives no message;
  - a `tool` entry that does not answer a call of the `assistant` entry
    before it is not sent; a call with no `tool` entry answering it is not
    sent, and an `assistant` message left with no text and no calls is not
    sent.
- The size of a message is its text plus the printed arguments of its calls.
- The budget of a run, in characters, is `superchat-context-chars` when that
  is a number. When it is nil, it is half of `superchat-llm-context-window`
  of the run's model, one token taken as one character; 120000 when the
  model has no window in the data. The handoff before a run, the text given
  to a handoff and the leaving out within a run use this one budget. It is
  worked out each time a request is built, so a change of the model's data
  or of `superchat-context-chars` during a run is seen at its next request.
- When the messages together are larger than the budget, the oldest are left
  out of this request until the rest fits: never the anchor message, never
  the `user` entry of this run, never the latest `assistant` entry and its
  results. An `assistant` entry is left out together with the `tool` entries
  that answer it and the entries among them that give no message. One `user`
  message, `[N earlier entries of this conversation are left out of this
  request; recall and tape_query can read them.]`, stands at the place of the
  oldest entry left out, also when the entries left out are not contiguous;
  N counts the entries left out, of any kind. When leaving entries out would
  not make the request smaller than sending all of them, all are sent, so a
  request may pass the budget by the length of that message. Nothing is
  removed from the Tape and no event is appended.
- The refusal: when the messages of the entries that are never left out,
  without that message, are larger than twice the budget, no request is
  made. The run ends with status `error` and an event that gives the size of
  the request and the budget, names the model or says that none is set, and
  says to start a new conversation; it advises raising
  `superchat-context-chars` too only when that is a number.
