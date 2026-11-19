# Decision: Eighty rounds, then a look-back

Status: implemented

## Problem

"No round limit by default" (2026-11-19) followed Pi, which has none. The
person then asked for a limit after all, 80 or 120, reached by a run that
looks back over its calls, says why it has not finished, and at the `auto`
level goes on by itself: a limit as a point to reflect, not a stop. In the
person's own Tape, 74 of 111 runs took at most 10 responses, 105 at most 40;
every run past 40 was one the old limit of 40 stopped.

## Decision

`superchat-max-rounds` defaults to 80; nil still sets no limit. The event
types gain `continued`, written when a run goes on after its look-back
(L3, "A run looks back at the limit and goes on").

## Alternatives considered

- 120. Not taken: twice the 40 that most runs stay under is room enough, and
  a run going round is made to look back sooner.
- No limit, the decision this replaces. Not taken any more: the person
  wants the look-back that a limit brings.

## Consequences

- This replaces "No round limit by default" of the same day.
- A run that goes round looks back after 80 responses instead of going on
  until the person cancels.
