# Decision: A look-back says its status and sees the facts

Status: implemented

## Problem

"A run looks back at the limit and goes on" was tried with `deepseek-flash`
at a limit of 3: one look-back was only "f3: value 3.", and one already held
the final answer, yet the run went on and spent a request saying it again.
The look-back relied on a cheap model reflecting on a long context, and the
run could not tell a finished look-back from an unfinished one.

## Decision

- The look-back begins with `Status: finished`, `Status: blocked` or
  `Status: continue`, and the run reads it: `finished` ends the run `done`
  with the look-back as its answer; `blocked` stops it at every level;
  `continue`, or no status, goes on at `auto` as before. The status line is
  not kept on the Tape. Markdown around it and case do not matter.
- The look-back request and the first request after it carry facts counted
  from the Tape by `superchat-request-digest`: each tool's calls and
  failures, the files changed most, calls made more than once, the last
  three failures. The model looks back at numbers it did not have to recall.

The shape follows Microsoft's TokenEconomizer (`tokeneconomizer/loopguard/`):
a digest of recent actions computed by code, a structured verdict instead of
prose, and a redirect that mirrors the agent's own counts back at it.

## Alternatives considered

- A separate judge request on the digest, returning JSON, as
  TokenEconomizer's `judge.py`. Not taken yet: a request each time the limit
  is reached; the status asks the model that holds the context, for free.
- Decide from the look-back's prose whether it is finished. Not taken: a
  guess at a sentence; the status line is one the run can test.
- A summary for the person written by L3 (B, declined on 2026-11-18). This
  is not one: the digest is counts for the model, and the person still reads
  the calls themselves.

## Consequences

- A run whose look-back finishes the task ends there, with no extra request.
- A model that ignores the status line gets the old behaviour.
- The facts take a few lines of each look-back request.
