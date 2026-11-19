# Decision: A run looks back at the limit and goes on

Status: implemented

## Problem

A run that reached `superchat-max-rounds` stopped, and the person typed
"继续"; in one session, twelve times. The person asked that a run at the
limit look back over its calls, find why it has not finished, and at the
`auto` level go on by itself, so that the next stretch works better than
the last.

## Decision

- The last request at the limit asks for a look-back: what is done, what
  failed and why, what is left, and the plan for the next step. It is
  recorded as an `assistant` entry, as before.
- At the `auto` level the run then goes on, at most
  `superchat-agent-continues` (2) times: a `continued` event, its count of
  responses from zero, and one system line on the next request to follow the
  plan and change what went wrong.
- At other levels, and after the last time, the run ends `error` with the
  account on the Tape, as before.

## Alternatives considered

- Tell a run going round once, from counts of failures on the Tape, with no
  limit (the proposal "A run going round is told once", rejected the same
  day). Not taken: the person chose the limit as the point to look back.
- A summary written by L3 from the entries (B, declined on 2026-11-18). Still
  not taken: the look-back is the model's own, which knows what it tried.
- Go on at every level. Not taken: at `ask` the person is at hand and
  decides; at `auto` they asked the run to work alone.
- Go on without a limit on the times. Not taken: a run that cannot finish
  would never end; with 80 rounds and 2 times it takes at most 240
  responses plus its look-backs.

## Consequences

- At `auto` a run can take three times `superchat-max-rounds` responses;
  the cost is that of a run the person would have continued by hand.
- The look-back is as good as the model's account of itself; it can be
  wrong, and the calls on the Tape remain the facts.
