# Decision: An inbox on the Tape and one worker

Status: proposed

## Problem

A package, a timer or the chat buffer starts work by calling `superchat-run`
directly. When the timeline is busy that is an error, so the caller must
keep the request and try again; the chat buffer does, in a queue that lives
only in its memory and is lost when Emacs exits. Two sources that call at
once race, and a request made while Emacs is going down is lost.

Anthropic's article "Building always-on agents" names these failures of
direct calls ("a crash can lose an event entirely") and answers them with a
durable inbox that is the source of truth, read by one worker, with source
ids to drop duplicates and several pending events folded into one run.

## Proposal

- A new L3 module `superchat-inbox.el`, cap 250 lines, between the agent
  and the interface: `inbox → agent`, `chat → inbox`. It owns the inbox,
  its worker and, in a later decision, the timers.
- An inbox item is an entry of the new kind `inbox` on the timeline it is
  for: `:text` the message, `:data` `(:source S :id ID :unattended BOOL)`.
  `(superchat-inbox-put TAPE TEXT &key source id unattended scope)` appends
  one, unless an item with the same SOURCE and ID is already on the Tape,
  and wakes the worker.
- An item is pending until a `user` entry has it in its `:refs`.
  `superchat-run` gets the key `:refs` for that. Nothing else marks it: the
  Tape stays append-only, and an item is consumed exactly when the run that
  answers it has written its request.
- One worker per Emacs, on an idle timer: for each timeline with pending
  items and no run, no handoff and no open question, it starts one run whose
  text is the pending items' texts joined by blank lines, oldest first, with
  their ids as `:refs`, unattended when any of them is. When a run ends, the
  worker looks again. On start, Emacs finds what was pending when it last
  exited.
- **L4.** The chat buffer's queue goes: input sent while the timeline is
  busy becomes an inbox item, shown dimmed as `⏸ queued` from the Tape.
  `superchat-run` on a busy timeline stays an error, for callers that want
  one.
- **L0.** Entry kind `inbox`; the module row and its arrows in `SPEC.md`.

## Alternatives considered

- Keep direct calls to `superchat-run`. Not taken: the article "Building
  always-on agents" shows how they race and lose events, and the chat
  buffer's in-memory queue is lost when Emacs exits.
- A table of its own with a status column, as the article suggests
  ("typically a database table"). Not taken: the Tape is the one database
  and is append-only; a status that changes would be the first thing on it
  that is edited. The `:refs` of the `user` entry says the same and cannot
  disagree with what was sent.
- Interrupt the running run when a new item comes, one of the article's
  three choices. Not taken now: queuing it for the next run is the
  article's simplest choice and keeps one way a run ends.

## Acceptance criteria

- Three items put while a run goes are answered by one run whose `user`
  entry holds their three texts and refs their three ids.
- An item put twice with the same source and id is on the Tape once.
- Emacs exiting with an item pending, then starting again: the worker
  answers it.
- Text sent in the chat buffer while a run goes is an `inbox` entry, and
  after the run the worker answers it.
- No two runs ever go on one timeline.

## Risks

- Joining several messages into one `user` entry loses which came when; the
  refs keep the items, and the joined text keeps their order.
- The worker runs only while Emacs runs; an item put from outside Emacs has
  no way in yet. This proposal has no outside source.
- A new module costs lines out of the 6000; the inbox must stay small.
