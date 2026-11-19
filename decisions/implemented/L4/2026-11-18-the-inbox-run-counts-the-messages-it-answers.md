# Decision: The inbox run counts the messages it answers

Status: implemented

## Problem

The worker answers every pending item of a timeline in one run, joining
their texts with a blank line and nothing else. The model could not tell
whether it was given one message or three: asked how many had come, DeepSeek
answered "one message" for two. Each item is its own message from its own
source — a timer, a package, the person — and the run's text said otherwise
by being flat. `NOTES.md` recorded it.

## Decision

One item's text goes into the run as it was said. Two or more, each item's
text is marked `[I/N] ` with its place in the batch, oldest first, and the
texts are joined by a blank line as before. `spec/L4-background.md` says so
in the rule of the worker, and `superchat-inbox--text` builds it.

A mark at each boundary is what makes the count unambiguous: the model reads
`[2/3]` and knows both how many there are and where the one it is reading
stands, without counting blank lines, which a message may contain itself.

## Alternatives considered

- Mark every item, a single one too. Then a run of one item reads `[1/1] …`,
  and that case is the common one: most runs are one timer firing. A count
  of one says nothing the caller did not already say, and the mark is
  something a model may repeat back to the person.
- Number the items with `seq-position`. It returns the index of the first
  item equal to the one it is given, so two timers with the same text would
  both be marked `[1/3]`. The first attempt did this and was wrong; an index
  loop counts places, not values.

The record weighs nothing else: `NOTES.md` names the fault and one repair.

## Consequences

- A run of several items is read as several messages, each with its source's
  own text untouched after the mark.
- The text of a run of one item is exactly what it was, so every existing
  behaviour of the worker is unchanged, and the timers, the dreaming run and
  the wake on Emacs starting all go on as they did.
- `test/test-inbox.el` holds both cases: three items marked `[1/3]`…`[3/3]`,
  and one item's text as it was said.
