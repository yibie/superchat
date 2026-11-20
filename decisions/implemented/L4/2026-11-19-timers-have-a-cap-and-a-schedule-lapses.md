# Decision: Timers have a cap, and a schedule the model set lapses

Status: implemented

## Problem

A run could set any number of timers, and a schedule fired forever. On
2026-11-19 a test run chained `follow_up` timers on the person's own Tape: 11
background runs and 11 notifications before anyone noticed. Making the timer
tools `write` stopped the chain at `ask`, but at `auto` nothing bounded how
many timers a conversation keeps or for how long. dim's scheduler caps loops
per session at 8, lets them live 7 days and makes them visible and stoppable
(`crates/dim-agent-scheduler/src/loops.rs`, `docs/design/plugins/Scheduler.md`).

## Decision

- `follow_up` and `schedule` refuse when the timeline has
  `superchat-timer-max` (8) timers that may still fire; the refusal says to
  cancel one first.
- A `schedule` (`:every` or `:daily`) the model sets gets `:until`,
  `superchat-timer-days` (7) days on; past it, the timer may no longer fire
  and puts nothing, missed times included.
- Dreaming, which `/dream` sets from code, has no `:until`: a lapse would
  turn it off with nobody told.
- `/timers` shows `until DATE`.

## Alternatives considered

- A cap only. Not taken: one schedule alone fires without end.
- Lapse every timer, dreaming too. Not taken: dreaming is the person's own
  standing order, and nothing would tell them it stopped.
- Make the model re-arm a schedule each time, as dim's self-paced loops do.
  Not taken now: a schedule here is what the person asked for; a lapse the
  person can see does the same with less machinery.

## Consequences

- A schedule asked for "every day" stops after a week; the person sets it
  again, and `/timers` says when it ends.
- The two numbers are `defvar`s, for the person to change.
