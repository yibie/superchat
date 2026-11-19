# Decision: The inbox may grow to 265 lines

Status: implemented

## Problem

`superchat-inbox.el` stood at 250 lines against a cap of 250. The inbox has
one thing left to say that it cannot (`NOTES.md`): how many messages a run
answers. That takes about ten lines — a function that builds the text, and
its docstring — and there was no room for them.

The cap is a rule of `SPEC.md`: `scripts/check.sh` reads the table and stops
the work of a layer at the number in it. Raising it is therefore a change to
a rule, and this note says why it is the right rule.

## Decision

The cap of `superchat-inbox.el` is 265.

The file stays one concern: the `inbox` entry kind, the worker that answers
it, and the timers that put one. A timer's only effect is to put an item and
wake the worker; the worker is what every timer waits for, and the tool
`follow_up` is the same worker seen from a run. Two of them in two files
would be one subject with a dependency arrow between its halves.

## Alternatives considered

- Split the timers into a new module, `superchat-timers.el`. This was
  weighed first, and it has the better reason: the timer part is 118 lines
  of a different subject — due times, missed while Emacs was closed,
  repeating — and a cap is a design signal that the file has grown into
  two. It was not taken because the need is ten lines: the split changes
  the module table, the dependency arrows and the sections of
  `spec/L4-background.md`, and either moves the public names
  (`superchat-inbox-timers`, `-start`, `-cancel-timer`, `-dream`) or keeps
  names in one file that speak of another. The person, asked which, chose
  the cap.
- Fold the fix into the lines that are already there. The run's TEXT is one
  argument on one line; marking the items needs a count and a place per
  item, which is four lines wherever they are written, and the argument is
  already the longest in the call. It does not fit in 250 with or without a
  name for it.

## Consequences

- The file is 260 lines, five under the new cap. The five lines are all the
  room there is; the next need of this layer has to be paid for, not
  added — by moving the timers out or by a rule that needs less code.
- Nothing else changes: no module, no dependency arrow, no public name.
- `SPEC.md` holds the number; `scripts/check.sh` enforces it.
