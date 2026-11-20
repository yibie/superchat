# Decision: The person steers a run, and waiting input drives the next

Status: implemented

## Problem

While a run went, what the person typed waited in the chat's queue until the
run ended, however long that took. A correction ("not that file", "wrong
direction") came too late to matter. And at the round limit a run at the
`auto` level went on after its look-back even when new input waited for the
timeline: it spent its next rounds on the old plan while the person's or the
inbox's newer input sat unread. dim (`crates/dim-agent-capabilities`,
`input_queue.rs`, `agent_loop.rs`) keeps one engine that only input drives,
and its Goal plugin makes no continuation decision while input waits
(`docs/design/plugins/Goal.md`, condition C5).

## Decision

- `superchat-steer TAPE TEXT` gives a running run the person's text. It is
  held on the run and goes in as a `user` entry with `:steer t` at the start
  of the run's next request, after every call of the response before has its
  result: a `user` entry produces a message, and one between a call and its
  result breaks the grouping of the request.
- Text a run never took (it ended, was cancelled, failed, waited, paused) is
  its `:steers` in ON-DONE, in order; the caller decides what to do with it.
- `superchat-agent-pending-functions`, called with the timeline's name, says
  whether input waits outside the run; the layer above adds to it. With
  steered text or such input, a run at the round limit pauses at once, with
  no look-back, and does not go on after a look-back.
- The round-limit code moved to `superchat-limit.el` first, to make room in
  `superchat-agent.el`.

## Alternatives considered

- Write steered text to the Tape at once. Not taken: written while a call is
  out, it lands between the call and its result, the bug a timer entry once
  caused (`superchat-agent--groups`).
- Cancel the run and start one with the new text, as `C-u C-c C-c` does.
  Kept as the forced choice, not the default: it throws away the calls in
  flight and the run's round of work.
- dim's two modes, Steer (drives) and Append (only context). Not taken now:
  everything the person types should be read; Append has no user here yet.
- Let L3 read pending inbox entries itself. Not taken: L3 must not know the
  `inbox` kind of the layer above; the hook keeps the dependency one way.

## Consequences

- A correction reaches the model at its next request, not after the run.
- The chat must steer instead of queue, and send `:steers` a run left behind
  (L5).
- A run with pending input never spends a look-back or a continuation on it.
