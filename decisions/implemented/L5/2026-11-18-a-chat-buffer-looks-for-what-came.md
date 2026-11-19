# Decision: A chat buffer looks for what came while it sat still

Status: implemented

## Problem

A chat buffer learned of entries only from the callbacks of a run it started
itself. Open on a timeline the inbox worker runs, it showed nothing of that
run: its entries came, and the buffer sat still. Its mode line did not even
say the timeline was busy, because that came from the same `--status` call.
The person saw a conversation that was not moving while it was. `NOTES.md`
recorded it.

## Decision

The mode line looks for entries that came while the buffer sat still: a
form `(:eval (superchat-chat--watch))` at the end of `mode-line-format`,
which Emacs evaluates whenever it draws the mode line. The look is at most
once a second (`superchat-chat--looked`), calls the same `--add` the
callbacks call, and so starts the buffer's timer as soon as the timeline is
busy. `spec/L5-interface.md` says so in the rule of what the buffer shows.

The mode line is the only place a buffer is told it is on screen, and it
costs no timer and no polling when the buffer is not: a hidden buffer is not
drawn, so it does not look. The second is a rate, not a delay: the entries
appear within a second of coming, and a person reading the buffer cannot
tell.

## Alternatives considered

- A timer per chat buffer, always running. That is a timer per open
  conversation, forever, for a case that matters only while one is on
  screen; and the buffer already has a timer, which this starts exactly when
  it is useful.
- A hook in L1 that a chat buffer adds itself to, so a write tells the
  buffers of its timeline. The clean answer, and it needs a new extension
  point in a lower layer, which `SPEC.md` forbids without a user in the
  repository; the user would be here, so it would be one — but it costs a
  hook variable, a call on every append, and the same work in L1 that L5 can
  do alone. The look in the mode line needs nothing of L1.
- `post-command-hook`. It runs on typing, not on display, so a buffer shown
  and not touched would stay still — the very case `NOTES.md` names.

## Consequences

- A chat buffer on a timeline another run is busy with shows that run's
  entries, its header line gains ` · running`, and the rest of the run
  arrives by the buffer's own timer.
- A buffer that is not displayed does no work, and a displayed one does one
  `superchat-tape-entries` query a second while it sits still.
- The callbacks keep their job: an entry from a run this buffer started is
  shown at once, not within a second.
- `test/test-chat.el` holds it, in two parts: a run started outside the
  buffer is shown once the mode line's form is called, and that form is
  where the mode line has it. Batch Emacs has no redisplay, so the test can
  not simply display the buffer: it asks the mode line for its form and
  calls it.
