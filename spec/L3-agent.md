# L3 agent: `superchat-run`

Part of the [specification](../SPEC.md). Modules: `superchat-context.el`,
`superchat-agent.el`. This layer is the AI layer of Emacs: any package asks
a model through `superchat-run`. It uses L1 and L2.

## superchat-context.el

- `(superchat-context-system TAPE &optional EXTRA)` → the system prompt, built
  fresh for each request, in this order:
  1. the base instructions, a constant of at most 40 lines;
  2. the rules file of the scope: `AGENTS.md` at the project root, or
     `AGENTS.md` in `superchat-directory` for `"home"`, when it exists;
  3. the skills catalog: for each `NAME/SKILL.md` under
     `superchat-directory`/`skills/`, its name and the `description:` line of
     its frontmatter, with the path, so the model reads the file with
     `read_file` when it wants it. A file whose first `---` line is never
     closed has no frontmatter;
  4. the `notes` view of the scope, rendered, at most 2000 characters;
  5. the origin of the latest `user` entry, when it has one;
  6. EXTRA, a string, when given.
- Frontmatter is only at the very start of a file and ends at the first line
  that is exactly `---`.

## superchat-agent.el

- `(superchat-run TAPE TEXT &key scope origin tools system model ephemeral on-text on-entry on-done)`
  → the run, a plist. It opens the Tape itself. The first entry of a new
  timeline takes SCOPE, or the scope of `default-directory` when none is
  given. With EPHEMERAL the `user` entry it appends has `:ephemeral t` in
  its data: a timeline with such an entry is one question another package
  asked, not a conversation, and the `timelines` view and
  `superchat-chat-recent-tape` leave it out. It stays on the Tape and the
  `search` view finds it. It appends the `user` entry, then loops:
  1. messages = the `conversation` view of TAPE turned into neutral
     messages. An anchor becomes a `user` message that begins
     `Summary of the conversation so far:`. `event` and `note` entries are
     left out. The size of a message is its text plus the printed
     arguments of its calls. When the messages together are larger than
     the budget, the oldest are left out of this request
     until the rest fits: never the anchor message, never the `user` entry
     of this run, never the latest `assistant` entry and its results; an
     `assistant` entry goes together with the `tool` entries that answer
     it. In their place stands one `user` message, `[N earlier entries of
     this conversation are left out of this request; recall and tape_query
     can read them.]`. When leaving entries out would not make the request
     smaller than sending all of them, all are sent, so a request may pass
     the budget by the length of that sentence. Nothing is removed from
     the Tape and no event is appended. When what is left is still more
     than twice the budget (the entries that are never left out are that
     large), no request is made: the run ends with status `error` and an
     event that gives the size of the request, the budget, the model, and
     says to raise `superchat-context-chars` or start a new conversation.
  2. `superchat-llm-request` with `superchat-context-system` and the tool
     specs. TOOLS is a list of names, or nil for every tool, or `none`.
  3. On done: append the `assistant` entry. If it has calls, run each with
     `superchat-tool-call`, one after the other in order, each started when
     the one before has its result, then go to 1. If it has none, the run
     is over. The run does not block Emacs while an asynchronous tool
     works: the user can type, switch buffers and cancel.
  ON-TEXT gets streamed text. ON-ENTRY gets each entry as it is appended.
  ON-DONE gets the run once, with `:status` `done`, `cancelled` or `error`,
  `:text` the text of the last `assistant` entry of this run (nil when it
  made none) and, for `error`, `:error` the text of the error event.
- At most one run per timeline. `superchat-run` on a timeline that has one is
  an error. A handoff occupies its timeline like a run: while it is pending,
  `superchat-run` and a second handoff are errors, and `superchat-handoff`
  on a timeline with a run is an error.
- A run always ends. Whatever is signalled while a request is started or
  inside a callback of the run becomes an `error` event and the run ends
  with status `error`; the timeline is free again.
- That holds when a callback the caller gave (ON-ENTRY, ON-TEXT) signals
  while the error event is being recorded: the run is ended first.
- That also holds for `superchat-cancel`: when stopping the request or a
  tool signals, the run still ends, with status `cancelled`.
- A run that is cancelled while one of its calls is inside
  `superchat-tool-call` (the user is being asked, or a plain tool is
  running) keeps its timeline until that call returns: only then are the
  missing results appended, the `cancelled` event appended and ON-DONE
  called, so nothing of a cancelled run is ever appended after a later
  run's entries. A tool whose permission question is answered after the
  cancel does not run; its result is `Cancelled.`.
- However a run ends (done, error, cancelled, a callback that signals at any
  entry): a tool of it that is still running is stopped; every call of its
  responses has a result on the Tape, `Cancelled.` for one that got none,
  before the run's last event; what the user wrote is on the Tape; ON-DONE
  is called exactly once; and nothing of the run is appended and nothing is
  given to its ON-ENTRY afterwards. A run only ever adds results for calls
  of its own responses. There is one way a run ends, used by all of these.
- ON-ENTRY may cancel the run. Nothing more is requested or run after that,
  and ON-DONE is still called exactly once. A cancel while the run is
  ending (ON-ENTRY given an entry the ending wrote) does nothing: the
  status stays what the ending set.
- When ON-ENTRY signals on an entry the ending wrote, the ending goes on:
  ON-ENTRY gets the entries after that one, the timeline is freed, ON-DONE
  is called, and then the first such signal is raised to whatever ended the
  run: the caller of `superchat-run` or `superchat-cancel`, or the timer or
  process that delivered a response or a tool's result, where Emacs
  reports it. An error raised by ON-DONE is the caller's in the same way.
- The calls of one response are run in a loop, not by recursion: a response
  with hundreds of calls is no deeper than one with two.
- An entry `superchat-tool-call` appended (a permission event, a result) is
  given to ON-ENTRY as soon as that call returns, also when the tool is
  asynchronous and its result comes later.
- What a provider is sent is well formed by how the messages are built, not
  only by what endings wrote: a `tool` entry that does not answer a call of
  the `assistant` entry before it is left out of every request.
- A summary is never forgotten and never too large: the text given to a
  handoff always begins with the conversation's current anchor message
  when there is one, the entries after it are cut to the budget oldest
  first, and when a single entry alone is larger than what is left its
  text is cut, so the run after a refusal makes a handoff that fits. The
  summary and the next step a handoff appends are each cut to a quarter of
  the budget, so an anchor message stays under half of it.
- The timeline of a run that ends is freed after its last entries were
  given to ON-ENTRY, just before ON-DONE: ON-DONE may start the next run,
  ON-ENTRY may not.
- The refusal compares the entries that are never left out, without the
  stand-in sentence, with twice the budget. Its text names the model, or
  says that none is set, and advises raising `superchat-context-chars`
  only when that is a number.
- The budget of a run, in characters, is `superchat-context-chars` when
  that is a number. When it is nil, the budget follows the run's model:
  half of `superchat-llm-context-window`, taking one token as one
  character (about right for Chinese, cautious for English), the other
  half being left for the system prompt, the tool specs, reasoning sent
  back and the answer; 120000 when the model has no window in the data.
  Twice the budget is then the whole window. The handoff before a run, the
  text given to a handoff and the leaving out within a run all use this
  one budget. It is worked out each time a request is built, so a change of
  the model's data or of `superchat-context-chars` during a run is seen at
  its next request.
- `(superchat-run-active TAPE)` → the run or nil.
- `(superchat-cancel TAPE)`: for a pending handoff, cancel its request,
  append a `cancelled` event and free the timeline. For a run: cancel the
  request, append a `cancelled` event,
  call ON-DONE with `cancelled`. A tool that is running is stopped through
  the function `superchat-tool-call` returned. Tool calls of the current
  response that have not started are not run. So that the timeline stays well formed, a
  `tool` entry with `:error t` and the text `Cancelled.` is appended for
  each call that has no result. When the run was cancelled before its `user`
  entry was appended (during the handoff that comes first), the `user`
  entry is appended before the `cancelled` event: what the user wrote is
  never lost.
- After `superchat-max-rounds` (40) model responses in one run, the run ends
  with status `error` and an `error` event that says so.
- An error from the model appends an `error` event and ends the run with
  status `error`.
- `(superchat-handoff TAPE &optional NAME on-done)`: ask the model, with no
  tools, for a summary of the `conversation` view and for what comes next,
  and append an anchor with them, its `:refs` the ids summarised. The
  request is one `user` message holding `superchat-view-render` of the view,
  cut to the budget, and the question, so it has no tool turns a provider could refuse. It
  returns the handle of the request. NAME is the
  topic name, default `"handoff"`. ON-DONE is called with the anchor, or with
  nil when the model failed (an `error` event is appended then).
- When `superchat-run` is called and the timeline's messages are already
  larger than the budget, a handoff is made first, and the
  `user` entry is appended after the anchor, so the request is never behind
  the anchor. When that handoff fails, the `user` entry is appended all the
  same, after the error event, and the run ends with status `error`: what
  the user wrote is never lost. That holds too when starting the handoff
  request signals. The handoff of a run uses the run's MODEL. There is no handoff in the middle of a run.
- `(superchat-fork TAPE ENTRY-ID)` → a new timeline name whose first entry is
  an anchor: summary empty, `:refs` the ids of TAPE's `conversation` view up
  to ENTRY-ID, and the `tool` entries that answer the calls of every
  `assistant` entry among them, so that no call is left without its result.
