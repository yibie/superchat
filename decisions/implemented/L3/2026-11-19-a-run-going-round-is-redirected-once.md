# Decision: A run going round is redirected once

Status: implemented

## Problem

The person asked for Microsoft's TokenEconomizer in the ReAct loop, to make
the agent waste fewer rounds. "A run going round is told once" was rejected
on 2026-11-19 in favour of a look-back at the round limit; the person now
wants both. TokenEconomizer's own evidence is about cost, not ability: on
SWE-Bench, 18-31% lower mean cost and Pass@1 from -1.0 to +2.6; this was
said to the person before they chose.

## Decision

`superchat-guard.el` counts, over a run's entries, its calls, failed calls,
failures that repeat an earlier one (first failing line, numbers and paths
masked), and the most changes to one file. Past 18 calls, a repeat, 8
failures or 4 changes to one file mean going round. The next request then
carries, once a run, the run's own counts and a line to change the approach,
never to stop; a `redirect` event records it. The look-back at the round
limit stays.

Replayed offline over the person's Tape (48 runs of superchat-next, 13 that
the old limit of 40 stopped): this trigger fires in 10 of the 13 stopped runs
and in 4 of the 35 others. Failure signals alone fire in 5 of the 13: the run
that rewrote `test/test-turn.el` seven times made no failing call, and only
the changes to one file showed it.

## Alternatives considered

- TokenEconomizer's strict path, which also waits until a run passes 1.25
  times the 90th percentile of this scope's good runs. Not taken now: in the
  replay it fired in 1 run, as the old limit cut runs before they passed it;
  it is the first refinement once runs have room under the limit of 80.
- Its judge, a model call on a digest returning JSON. Not taken yet: a
  request each time the gate opens; the template redirect is free.
- The thresholds of the rejected proposal (4 failures in the last 12 calls).
  Not taken: they act before a loop has had time to form.
- Only the look-back at the limit (the rejected choice). Not taken alone: it
  comes after 80 responses; the redirect comes when the signals do.

## Consequences

- Most long runs that went round get one line telling them so, with their
  numbers; about one in nine others gets it needlessly, which costs a line.
- Work that goes round without failing and without changing one file again
  is not seen.
- The thresholds are guesses checked on one person's Tape; they are
  `defvar`s to be tuned.

This reverses "A run going round is told once, and dreaming learns from it"
(`decisions/rejected/L3/`) for its in-run half; its dreaming half is the
next step, "facts of the project learned in dreaming".
