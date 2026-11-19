# Decision: A cut-off call has no duration

Status: implemented

## Problem

When Emacs exited while a tool call ran, the next run marks that call with a
`tool` entry whose `:error` is t and whose text begins `Interrupted: Emacs
stopped while this call ran; it may have had effects.` The chat buffer took
the entry's `:at` as the end of the call, so the heading showed a duration
such as `(97.6s)`: the time from the cut-off to the start of the next run,
not how long the call ran. `NOTES.md` recorded it.

## Decision

A call's heading has no DURATION when its `tool` entry's text begins
`Interrupted:`, the one sign the Tape carries that the call was cut off
rather than run. Any other call with a `tool` entry keeps its duration, also
when that entry has `:error`: a call that failed did run that long.
`spec/L5-interface.md` states it in the rule of DURATION, and
`superchat-turn--call` does it.

## Alternatives considered

The record weighs none: `NOTES.md` says only that such a call "may need no
duration", and the request that it show none. These were weighed while doing
it and not taken:

- `(0.0s)`. It is a number that never happened.
- `:error t` as the sign. A call that failed by itself has a duration that is
  true, and would lose it.
- Having L3 record the elapsed time in the `tool` entry it appends. L5 may
  not change another layer's entry, and the entry already says what happened.
- Reading the turn's `interrupted` event. A timeline may hold several, and
  none of them says which calls it marks.

## Consequences

- A cut-off call's heading is `*** ✗ NAME  ARGS`, with no duration.
- L5 knows one string of L3's wording. A change to the text L3 writes for a
  cut-off call changes this display with it, and `test/test-turn.el` fails.
- A call cut off by a quit keeps its duration: its text is `Interrupted by
  the user.`, so it did run until the quit.
