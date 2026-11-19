# Decision: A turn that is over shows no hourglass

Status: implemented

## Problem

`superchat-turn--progress` put `⏳` before `Progress` whenever a call of the
turn had no `tool` entry. A call cut off by Emacs exiting has none, so a
timeline reopened after the cut-off showed `** ⏳ Progress  ▮  1 call` as if
the call were still running. The call was not: Emacs was gone, and the next
run marks the call `Interrupted:`. The heading was already folded, which is
the other sign of a turn that is over; only the hourglass said otherwise.
`NOTES.md` recorded it.

## Decision

The hourglass is there only while the turn still runs: whatever
`superchat-turn-text` is given as OVER, and whether a final answer is there,
folds Progress and takes the hourglass with it. The sign is the one DONE
already is, so the hourglass and the fold say the same thing.
`spec/L5-interface.md` says so in the rule of Progress, and
`superchat-turn--progress` does it (folded `(and done (= failed 0))`, the
mark `(and waiting (not done) "⏳ ")`).

A call with no result in a turn that is over is not running in either case:
either Emacs stopped, and the next run marks the call, or the run ended
`waiting` for the person's answer, which the line above the separator
already says. The call's own heading keeps its `⏳`: it says the Tape holds
no result, which stays true.

## Alternatives considered

The record weighs none: `NOTES.md` names the symptom and one repair, and
the request asks for the sign DONE already is. These were weighed while
doing it and not taken:

- Leave the hourglass. It claims a run that is not there, and the fold
  below it contradicts it.
- Read the Tape the way `superchat-run` does, and show a cut-off turn as cut
  off. That is more than a display fix: it repeats L3's rule of when a run
  never ended in L5, on every draw of a turn, and it answers a question the
  heading was not asked. The next run already appends the marks, and the
  turn then says `Interrupted:` by itself.
- Key it on the turn's `interrupted` event. The event comes only when the
  next run starts, so a timeline opened before it would still show an
  hourglass; and a turn that ended `waiting` has no such event and no run
  going either. The event says which calls were marked, not whether a run
  is going now.

## Consequences

- A turn drawn with OVER t, or with its final answer, shows `** Progress`,
  whatever its calls' results.
- A running turn is unchanged: `** ⏳ Progress  ▮  1 call` while its call
  has no result.
- The call's heading still shows `⏳` for a call with no `tool` entry, so a
  cut-off call is told apart from a failed one by its `✗` and its missing
  duration, not by the Progress line.
- `test/test-turn.el` holds both cases; the test of the running turn is
  unchanged.
