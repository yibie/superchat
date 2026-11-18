# L4 background: the work nobody watches

Part of the [specification](../SPEC.md). Module: `superchat-inbox.el`. This
layer is the personal agent at work while nobody watches: it puts work in
an inbox, starts runs for it, keeps timers and lets a run tell the person
something. It uses L1, L2 and L3 through their public functions; L5 uses it.

## superchat-inbox.el

Work for a timeline that nobody is typing into goes through the inbox: it
waits while the timeline is busy and survives Emacs exiting.

- `(superchat-inbox-put TAPE TEXT &key source id unattended scope)` → the
  `inbox` entry it appends to TAPE, `:data` `(:source :id :unattended)`,
  and wakes the worker; nil, appending nothing, when an `inbox` entry with
  that SOURCE and ID is on the Tape. SCOPE is that of a new timeline, by
  default that of `default-directory`.
- An item is pending until a `user` entry of its timeline has its id in
  `:refs`. `(superchat-inbox-pending &optional TAPE)` → the pending items,
  oldest first, of TAPE or of every timeline.
- `(superchat-inbox-wake)`: the worker looks, from a timer of 0 seconds,
  once for any number of wakes. It is woken by a put, by the end of a run it
  started, and by the person or a package when Emacs starts. For each
  timeline with pending items, no run, no handoff and no question waiting,
  it starts one `superchat-run`: TEXT the items' texts, oldest first,
  joined by a blank line; REFS their ids; UNATTENDED when any item is,
  and then SYSTEM a few lines saying that nobody watches, that the person
  sees only what is sent and the questions, and to send only what they
  need to act on or asked for. A
  timeline whose run cannot start is shown with `message` and keeps its
  items; the others start.
- A `timer` (`SPEC.md`) may still fire unless a `:cancel t` timer refs it
  or, a follow-up (`:at`), it fired.
  `(superchat-inbox-timers &optional TAPE)` → those, oldest first. A timer
  is due at the latest time, at or before now and, for a schedule, not
  before its own `:at`, that its `:at`, `:every` from `:from`, or local
  `:daily` names. A due timer puts its text, unattended, as source
  `"timer"`, id `ID@SECONDS`: once per time, and once for all times missed
  while Emacs was closed.
- `(superchat-inbox-start)`, a command: put what is due and wake the worker
  now and every `superchat-inbox-poll-seconds` (a `defvar`, 60).
  `(superchat-inbox-cancel-timer ID)` cancels.
- Tools, class `read`: `follow_up` (`in` seconds, `text`); `schedule`
  (`every` ≥ 60 or `daily` `"HH:MM"`, `text`), first due one period after
  it is set; `cancel_timer` (`id`).
- A tool `send` (`text`), class `read`, appends an `event` of type `sent`
  with TEXT, shows its first line with `message`, and answers `Sent.`.
