# Decision: A run going round is told once, and dreaming learns from it

Status: rejected — the person chose a look-back at the round limit instead (L3, "A run looks back at the limit and goes on"); learning from such runs in dreaming may come back on its own

## Problem

With no round limit (L0, "No round limit by default") nothing stops a run
that goes round, and nothing makes the next run cheaper. In the person's own
use (timeline `20261118-165024-f2af`) one stretch spent about ten responses
shortening one sentence of a specification at its word ceiling, an `edit`, a
save and a `wc -w` each; eleven edits failed on text a formatter had
changed. The person asked that a run which has not finished look back at why,
so that the work gets cheaper. Microsoft's TokenEconomizer does this for
other agents: a loop guard that redirects a turn once when it churns, and
"micro-skills", repository facts learned from past sessions and stated at
the start of the next.

## Proposal

- **L3, the redirect.** A run is going round when, in its last 12 calls, at
  least 4 failed (a `tool` entry with `:error`), or one file was changed 4
  times, or the same call (name and arguments) was made 3 times. The check
  reads the run's own entries on the Tape; no model judges it. The first
  time it holds, the next request carries one system line: how many
  responses the run took, the failures or the repeated calls in short, and
  "Stop and say in one line why this is not working, then change the
  approach." An `event` of type `redirect` records it. Once a run; it never
  stops the run.
- **L4, the learning.** Dreaming reads, beside the notes, the runs of the
  scope since its last review that had a `redirect` or more than 30
  responses, and writes a note for a fact of the project that would have
  saved the rounds (the command that checks a word ceiling, the build order,
  a formatter that runs on save), citing the entries. Notes reach every
  system prompt already, and a later note corrects one that is wrong.

## Alternatives considered

- A model that judges whether a run is stuck, as TokenEconomizer's judge.
  Not taken first: it costs a request per check and can be wrong in either
  direction; the Tape's own counts are free and can be tested.
- Stop the run at the point it goes round. Not taken: TokenEconomizer
  "never stops a turn itself", and the person chose no limit.
- A summary of the run in prose at a stop (B in the decision of 2026-11-18,
  `7afeda2`). Already declined: it judges on top of the facts; the redirect
  asks the model, which knows what it tried, and records only that it asked.
- Learn facts in the run itself, at once. Not taken: a fact seen once is
  often wrong; dreaming sees several runs and can correct a note.

## Acceptance criteria

- A scripted run with 4 failing calls in a row gets the system line on its
  next request, once, and a `redirect` event; a run with no failures never
  does.
- The same file changed 4 times, and the same call made 3 times, each
  trigger it.
- The run goes on after the redirect; with `superchat-max-rounds` nil it ends
  only when the model answers or the person cancels.
- Dreaming's text names the runs with a `redirect` or more than 30
  responses; a scripted dream run that reads such a run writes a note citing
  it.

## Risks

- The thresholds are guesses; a long healthy run that edits one file many
  times on purpose gets one needless line.
- A note learned from one bad run can mislead later runs until a later note
  corrects it.
- `superchat-agent.el` must hold the check within its cap.
