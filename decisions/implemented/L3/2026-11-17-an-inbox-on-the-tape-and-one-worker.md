# Decision: An inbox on the Tape and one worker

Status: implemented

## Problem

A package that wanted the agent to work called `superchat-run` directly.
When the timeline was busy that was an error, so the caller had to keep the
request and try again; two sources that called at once raced, and a request
made while Emacs was going down was lost.

Anthropic's article "Building always-on agents" names these failures of
direct calls ("a crash can lose an event entirely") and answers them with a
durable inbox that is the source of truth, read by one worker, with source
ids to drop duplicates and several pending events folded into one run.

## Decision

- A new L3 module, `superchat-inbox.el`, cap 250 lines, with the arrow
  `inbox → agent`. It owns the inbox and its worker; the timers of a later
  decision belong here too.
- An item is an entry of the new kind `inbox` on the timeline it is for,
  `:data` `(:source :id :unattended)`. `superchat-inbox-put` appends one,
  unless an item with the same source and id is already on the Tape, and
  wakes the worker.
- An item is pending until a `user` entry of its timeline has its id in
  `:refs`; `superchat-run` takes `:refs` for that. Nothing else marks an
  item: the Tape stays append-only, and an item is consumed exactly when the
  run that answers it has written its request, also when that run ends in
  an error.
- One worker, `superchat-inbox-wake`, from a timer of 0 seconds: for each
  timeline with pending items and no run, no handoff and no question, one
  run whose text is the items' texts joined by blank lines, oldest first,
  unattended when any item is. It wakes again when such a run ends. A
  timeline whose run cannot start is reported with `message` and keeps its
  items.

The chat buffer's queue stays as it was, against the proposal. Its rules
are for a person at the keyboard: a queued text goes back to the input when
the run is cancelled or fails, and `C-u C-c C-c` forces it. An inbox item,
once on the Tape, is always answered. Folding the queue into the inbox
would have reversed those rules, which no one asked for; the inbox serves
packages, and the timers to come.

## Alternatives considered

- Keep direct calls to `superchat-run`. Not taken: the article "Building
  always-on agents" shows how they race and lose events.
- A table of its own with a status column, as the article suggests
  ("typically a database table"). Not taken: the Tape is the one database
  and is append-only; a status that changes would be the first thing on it
  that is edited. The `:refs` of the `user` entry says the same and cannot
  disagree with what was sent.
- Interrupt the running run when a new item comes, one of the article's
  three choices. Not taken: queuing it for the next run is the article's
  simplest choice and keeps one way a run ends.
- Fold the chat buffer's queue into the inbox, as proposed. Not taken now,
  for the reason above.

## Consequences

- Several items put while a timeline is busy are answered by one run, and
  an item put twice from the same source and id is on the Tape once.
- What was pending when Emacs exited is answered after
  `superchat-inbox-wake` is called again; nothing calls it at start yet.
  The timers of the next decision will poll, which wakes it.
- A chat buffer open on a timeline the worker runs is not told of the
  run's entries as they come; it shows them when it is drawn again.
- An item that comes while its timeline waits for an answer stays pending;
  the run that `superchat-answer` starts does not wake the worker, so the
  item waits for the next wake.
- `superchat-agent.el` is at 699 of 700 lines. `spec/L3-agent.md` may hold
  2810 words, up from 2460: the module is new and its rules have no other
  home.
