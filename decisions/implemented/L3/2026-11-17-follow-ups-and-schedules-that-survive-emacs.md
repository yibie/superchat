# Decision: Follow-ups and schedules that survive Emacs exiting

Status: implemented

## Problem

The agent could act only when someone asked. It could not say "look again
in an hour whether the build passed", nor do something every morning, and
Emacs timers do not survive Emacs exiting. Anthropic's article "Building
always-on agents" separates follow-ups (a deadline the agent sets itself)
from schedules (recurring work), keeps both apart from the inbox, and has a
scheduler that polls, updates the timer and enqueues an event: "A timer
doesn't initiate a run on its own."

## Decision

- A timer is an entry of the new kind `timer` on the timeline it is for:
  `:text` the task, `:data` `(:at TIME)` for a follow-up, or
  `(:every SECONDS :from TIME)` or `(:daily "HH:MM")` for a schedule. A
  `timer` entry `(:cancel t)` whose `:refs` name a timer cancels it.
- `superchat-inbox.el` owns them. Three tools, class `read` because they
  only append: `follow_up` (`in` seconds, `text`), `schedule` (`every` of 60
  seconds or more, or `daily`; `text`), first due one period after it is
  set, and `cancel_timer` (`id`). The person lists and cancels them with
  `/timers` and `/timers cancel ID` in the chat buffer, through
  `superchat-inbox-timers` and `superchat-inbox-cancel-timer`.
- A timer is due at the latest time, at or before now and, for a schedule,
  not before the timer was set, that its data names. A due timer becomes an
  unattended `inbox` item with source `"timer"` and id `ID@SECONDS`. The
  inbox puts one item per source and id, so a timer fires once per time
  with no state of its own, and a schedule that missed several times while
  Emacs was closed fires once.
- `superchat-inbox-start`, a command, puts what is due and wakes the worker
  at once and every `superchat-inbox-poll-seconds` (60).

Three things differ from the proposal. Whether a timer fired is read from
the inbox item's source and id, not from an item that refs the timer: the
inbox's own check of source and id then keeps a timer from firing twice
for one time. `follow_up` takes only `in`, not `at`: one way to say when.
And nothing calls `superchat-inbox-start` when Emacs starts; the person
puts it in the init file. Starting a timer when a file is loaded would be a
side effect of `require`, which no other file here has.

## Alternatives considered

- Emacs's `run-at-time` alone. Not taken: it is lost when Emacs exits.
- A durable execution engine, the article's other choice. Not taken: it is
  a dependency outside Emacs, and the dependencies are only what Emacs
  ships, `curl` and `bridge/`.
- A status on the timer that the scheduler updates, as the article
  describes. Not taken: whether a timer fired is read from the inbox, so
  the Tape stays append-only.

## Consequences

- Nothing fires while Emacs is closed: "always on" here means "while Emacs
  runs and `superchat-inbox-start` was called". The fire at start keeps work
  from being lost, not from being late.
- Each firing is a model request; the tools' descriptions ask the model to
  schedule only what the person asked for. `every` is at least 60 seconds.
- `superchat-inbox-start` also wakes the inbox worker, so items left
  pending when Emacs exited are answered once it runs.
- `superchat-inbox.el` is at 194 of 250 lines, `superchat-chat.el` at 1086
  of 1100.
