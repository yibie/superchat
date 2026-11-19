# Decision: The interface specification may hold 3780 words

Status: implemented

## Problem

`spec/L5-interface.md` stood at 3598 words against a ceiling of 3600. Two
rules had to go in: that a chat buffer shows entries made while it sat
still, and that a `question` and its `answer` show at the call they waited
for. The first was paid for by compressing a dozen sentences elsewhere in
the file; the second had nowhere to go.

The ceiling is right in general: it is a signal that a layer's words have
outgrown what a person can hold, and the file had reached it by growing
honestly. But paying for one new rule by rewording a dozen old ones is a bad
trade, and it is expensive: every reworded sentence is an edit, a save and a
word count, and the meaning of a rule nobody meant to change is at risk.

## Decision

The ceiling of `spec/L5-interface.md` is 3780 words, five percent more.
`SPEC.md` holds the number.

The file is where the whole interface layer is described — the chat buffer,
the turn's text, the inline commands, the plugins, the messages buffer — and
it is the layer the person meets. Its rules are long because the display is
detailed, not because it repeats itself: the compression that paid for the
last rule found no paragraph saying the same thing twice, only sentences
that could be shorter.

## Alternatives considered

- Compress further. It was done once, for the first rule, and the second
  showed what it costs: a dozen edits for forty words, and rules restated in
  words their author did not choose. A third and fourth rule would keep
  buying room the same way, at the same price, in the file least worth
  risking a misstatement in.
- Move the detail to its one home. `SPEC.md` gives the entry kinds and the
  layer table; the display of every heading, cell and face has no other
  home. The parts that could move — the plugin and messages sections, which
  are small — would leave this file talking about a layer it no longer
  covers.
- Leave the ceiling and the rule out. The note in `NOTES.md` is a real
  fault: a question shown after the answer, away from the call that waited
  for it.

## Consequences

- The file is 3645 words, 135 under the new ceiling; the next two rules of
  this layer have room and need no rewording of others.
- No other ceiling changes. The rule that a file more than a tenth under its
  ceiling has it lowered to five percent above its count stays, so this one
  can come down again when the file earns its shorter text.
- The commit that raised it says why, as `SPEC.md` requires.
