# Decision: The agent specification may hold 2730 words

Status: implemented

## Problem

`spec/L3-agent.md` stood at 2593 words against a ceiling of 2600. The rule of
the round limit had to change twice in one evening — the last request keeps
its tools, and the `error` event counts the calls the run made — and each
change had to be paid for by rewording the same paragraph, to keep its count
under the ceiling.

That is the trade the note of the interface ceiling already refused, for the
same reason: paying for a rule with a rewording of a dozen others costs many
edits and risks restating a rule nobody meant to change. Here the paragraph
had become shorter than its own meaning twice over.

## Decision

The ceiling of `spec/L3-agent.md` is 2730 words, five percent more.
`SPEC.md` holds the number.

## Alternatives considered

- Keep compressing. The paragraph of the round limit now says what it says in
  as few words as it can without losing a rule — that the tools stay offered,
  that the answer is recorded with no calls, that the event counts the calls
  — and the compression of the first change had already cut the sentence
  which stopped the next request.
- Move the rule to another file. The round limit is the loop of
  `superchat-run`, and `spec/L3-agent.md` is the only place the loop is
  described. `SPEC.md` holds entry kinds and options, not the order of a
  run's requests.
- Leave the change out. The count is what makes the stop an account of the
  work; without it a run that ends at the limit says only that it ended.

## Consequences

- The file is 2607 words, 123 under the ceiling.
- No other ceiling changes. The interface ceiling went up by the same
  proportion in the same evening, for the same reason, and `SPEC.md` now says
  of both that a ceiling comes down again when the file earns shorter text.
