# Decision: An answer names the calls it rests on

Status: proposed

## Problem

Gathering recipes from the Tape found only the debris of exploring
(see [2026-11-20-what-this-emacs-can-do.md](2026-11-20-what-this-emacs-can-do.md)).
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

## Proposal

- **Citable results (L3).** Each tool result is sent to the model with
  its entry id first, `#ID `, so the model has a number to cite; the id
  is the Tape's, so the text of a request does not change from one
  request to the next.
- **Declared sources (L3).** The system prompt asks that an answer which
  rests on tool results end with one line `Sources: #ID, #ID`. When the
  answer is kept, the line is taken out of its text and the ids become
  the assistant entry's `:refs`, keeping only ids of `tool` entries of
  this run that did not fail. An id that names anything else is dropped
  and counted in the entry's data as `:refs-dropped N`.
- **The meaning of `:refs` (L0).** The sentence "A correction is a new
  entry whose `:refs` names the entry it corrects" becomes: "`:refs`
  names the entries an entry rests on: an anchor's summarised entries,
  a note's sources, an answer's tool results. A note among a note's
  sources is corrected by it."
- **Trace (L1).** `(superchat-tape-trace ID)` → `(:sources IDS :derived
  IDS)`: the entries ID names in `:refs`, and the later entries naming
  ID. A view `trace :id N` shows both with their first lines.
- **What uses it.** The recipes of step 2 of the earlier proposal come
  from calls an answer cites, in runs the person did not correct next;
  calls no answer cites are exploring and are left out.

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

## Acceptance criteria

- A DeepSeek run that reads a buffer, probes two names, then answers from
  the buffer's text ends with an assistant entry whose `:refs` holds the
  read and not the probes, and whose text has no `Sources:` line.
- An answer citing a failed call, a call of another run or a made-up id
  keeps none of them, and its data counts them.
- `superchat-tape-trace` of that read gives the answer among `:derived`.
- Requests of a timeline stay byte-identical from one request to the
  next apart from what is appended.

## Risks

- A model may cite too little or too much; code checks only that what it
  cites exists in the run and succeeded, not that the answer truly rests
  on it. The trace records declarations, as DeepSeek Harness's does.
- `#ID ` on every result costs a few tokens per result.
- A model that writes `Sources:` in the middle of prose is not parsed;
  only a final line is.
