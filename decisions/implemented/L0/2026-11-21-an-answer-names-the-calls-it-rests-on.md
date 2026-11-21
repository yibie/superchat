# Decision: An answer names the calls it rests on

Status: implemented

## Problem

Gathering recipes from the Tape found only the debris of exploring
(see [2026-11-20-what-this-emacs-can-do.md](../../implemented/L0/2026-11-20-what-this-emacs-can-do.md)).
The Tape keeps every call, its arguments, its result and whether it
failed: 223 `eval_elisp` calls succeeded and 59 failed on the person's
Tape, and none of that was lost. What it lacks is which calls an answer
rests on. Of seven telega calls that succeeded, one gave the result the
answer used; the rest were probes, and nothing tells them apart. The
facts replay failed the same way.

DeepSeek Harness keeps this as a relation, not a guess: its
`SessionEventTrace` gives each event's `sourceEventSeqs`, the earlier
events it cites, and `derivedEventSeqs`, the later ones citing it
(`docs/subsystems/session-query.md`). It records only what is declared;
the declaring is what Superchat's answers never do.

## Decision

- **Citable results (L3).** `superchat-request.el` sends each tool result
  with its entry id first, `#ID `. The id is the Tape's, so a request's
  text does not change from one request to the next.
- **Declared sources (L3).** The base instructions ask an answer that
  rests on results to end with one line `Sources: #ID, #ID`, probes and
  failed tries left out. When the answer is kept, `superchat-request-sources`
  takes the line out of its text; its ids that name a `tool` entry of
  this run that did not fail become the entry's `:refs`, and the count of
  the others its `:refs-dropped`.
- **The meaning of `:refs` (L0).** "`:refs` names the entries an entry
  rests on: an anchor's summarised entries, a note's sources, an
  answer's tool results. A note among a note's sources is corrected by
  it." This replaces "a correction is a new entry whose `:refs` names
  the entry it corrects".
- **Trace (L1).** `(superchat-tape-trace ID)` gives `(:sources IDS
  :derived IDS)`; the view `trace :id N` shows the entry with both.
- **Caps.** `superchat-request.el` goes from 200 to 250 lines, by the
  person's choice, so the citation scheme stays in one module; the L1
  word ceiling from 1070 to 1170. `request → llm, tape` is a new arrow.

## Alternatives considered

- Infer which calls mattered from the order of calls (the last
  successful call before the answer). Not taken: an answer often rests
  on a call made early, after which the run only checked things.
- A `cite` tool the model calls before answering. Not taken: it costs a
  request each answer, and a model that forgets the line would forget
  the tool as well.
- Cite by the provider's call ids. Not taken: models do not see them
  reliably, and they mean nothing on the Tape.
- Leave it as it is and only gather from the current conversation
  (`/learn` right after a success). Not taken alone: it narrows the
  source but still cannot tell probes from the call that worked.

## Consequences

- Recipes of the earlier proposal's step 2 can come from cited calls,
  not from every call that succeeded.
- What is recorded is what the model declares: code checks that a cited
  result exists in the run and succeeded, not that the answer rests on
  it.

## Risks

- A model may cite too little or too much; code checks only that what it
  cites exists in the run and succeeded, not that the answer truly rests
  on it. The trace records declarations, as DeepSeek Harness's does.
- `#ID ` on every result costs a few tokens per result.
- A model that writes `Sources:` in the middle of prose is not parsed;
  only a final line is.
