# Decision: Dreaming, a scheduled review of the notes

Status: implemented

## Problem

Notes pile up. A note that a later one corrected was still shown by the
`notes` view and still reached every system prompt, inside its 2000
characters. Anthropic's article "Building always-on agents" names an
optional "dreaming" step that reviews past sessions to curate memory,
replacing stale entries and finding patterns across runs.

## Decision

- **L1.** The `notes` view leaves out a note that a later note has in its
  `:refs`: by `SPEC.md`, a new entry whose `:refs` names an entry corrects
  it. It leaves them out before it takes the newest LIMIT, so a corrected
  note does not make the view shorter. The `search` view still finds both.
- **L2.** The `note` tool's description says that a note among its sources
  is replaced, and to cite a note only to correct or merge it.
- **L4.** `superchat-inbox-dream` turns dreaming on for a scope: a `daily`
  `"03:00"` timer on a timeline of its own, whose text asks the model to
  read the notes and what happened since its last review with `recall`,
  to replace with `note` the notes that repeat or are wrong, citing them,
  to delete nothing, to send nothing unless a note contradicts what the
  person said, and to write nothing when nothing is new. One such timer
  per scope; turning it off cancels it. Its run is unattended and quiet,
  as every timer's.
- **L5.** `/dream` and `/dream off` in a chat buffer, for its scope.

One thing differs from the proposal: the run is made each day whether or
not the scope has new notes, and the model is told to write nothing when
nothing is new. Telling new from old would need a mark of the last review
that the timer's own inbox items already are; the cost is one request a
day per scope with dreaming on.

## Alternatives considered

- Compaction of long sessions alone. Not taken as the answer: the article
  says compaction and summaries both "lose detail", and the facts worth
  keeping belong in memory.
- Let the person tidy the notes by hand. Not taken as the only way: it is
  the work the person gives an agent; `/note` stays.

## Consequences

- A corrected note leaves the system prompt and the `notes` view; it stays
  on the Tape and in `search`, so a wrong correction can be corrected in
  turn.
- A note that cites a note only as a source now hides it; the tool's
  description says so, and that is all that guards it.
- Dreaming costs one unattended request a day per scope that has it on,
  and makes up one missed night at start.
