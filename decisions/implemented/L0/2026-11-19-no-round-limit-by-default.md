# Decision: No round limit by default

Status: implemented

## Problem

`superchat-max-rounds` was 40. In the person's own use (timeline
`20261118-165024-f2af`) runs stopped at it twelve times, and the person
typed "继续" after each stop. A limit that a working run reaches is a stop
the person has to undo by hand. Pi, which the bridge is ported from, sets no
limit: its loop (`packages/agent/src/agent-loop.ts`) ends when a response
holds no tool call, or when the person aborts.

## Decision

`superchat-max-rounds` defaults to nil, as in Pi: no limit. A number still
limits a run, as the L3 rule for the limit says. The person asked for the
default to match Pi.

## Alternatives considered

- Raise it to 80 or 100. Not taken: the person asked for Pi's behaviour, and
  any number is reached by some run that is still making progress.
- Keep 40 for runs nobody watches (the inbox, timers) and none for the rest.
  Not taken now: it needs a run to carry its own limit; it is the first
  thing to add if an unattended run is seen going round.

## Consequences

- A run in a loop goes on until the person cancels it (`C-c C-k`). A run
  nobody watches has no one to cancel it; setting a number protects it.
- The answer the limit asks for, and its account on the Tape, only happen
  when the person sets a number.
