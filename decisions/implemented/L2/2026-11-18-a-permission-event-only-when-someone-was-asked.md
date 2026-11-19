# Decision: A permission event only when someone was asked

Status: implemented

## Problem

In the same session as the decision "Fewer, coarser tools", 596 of 2206 entries were
`permission` events. 580 of them came within a second of the call: an
`always` given earlier allowed the call, and nobody was asked, yet each call
left an event. They crowded the Tape, the views built from it, and what
`recall` searches.

## Decision

`superchat-tool-call` appends a `permission` event when the user was asked
during the call, or when the call is denied. A call that an earlier
`always`, or an answer already on the Tape, allows leaves none.

## Alternatives considered

- Keep every event and hide them in the views. Not taken: the Tape would
  still hold them, and every reader would have to know to skip them.
- Record the `always` once, when it is given. That is what remains: the
  event of the question where the person answered `always`.

## Consequences

- The Tape records each decision a person made, once.
- That a later call ran under an `always` is no longer written down next to
  it; the earlier event and the level say it.
