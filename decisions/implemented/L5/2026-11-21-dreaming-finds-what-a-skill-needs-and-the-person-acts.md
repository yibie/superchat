# Decision: Dreaming finds what a skill needs, and the person acts

Status: implemented

## Problem

Skills grown by `/learn` drift: two interface entries can differ only by a
`require`, a package's version moves past the skill's `version:`, and two
recipes can overlap. Dreaming runs nightly with nobody watching, but adding
or dropping a tool or recipe is the person's call.

## Decision

- **At save**, an interface entry whose code, without `progn` and top-level
  `require` forms, is that of one already there is not added.
- **Dreaming only finds and says.** L4 gains `superchat-timer-dream-functions`,
  called with the scope when a dreaming timer puts its item; each text a
  function returns is sent from the dreaming timeline (`superchat-inbox-sent`,
  also what `send` uses). `/learn` adds `superchat-learn-dream`: when code
  finds repeated entries or a changed version in any skill it made, it
  returns `N skill(s) to review: /skills review`, at most once a day.
- **`/skills review`** lists those findings; `d` drops a repeated entry and
  `RET` opens the SKILL.md. A model with no tools lists overlapping recipes
  as suggestions; the person merges them by hand or leaves them.

## Alternatives considered

- **Dreaming merges and drops on its own.** Rejected in this session: an
  unattended run would add or drop the person's tools.
- **Dreaming handles the `require` duplicates.** Proposed by the person; the
  repeat is mechanical, so it is stopped at save, and dreaming keeps what
  code alone cannot judge.
- **The dreaming model reads the skills itself and calls `send`.** Its
  finding cannot be checked; code finds the mechanical cases and the model
  only suggests, in front of the person.

## Consequences

- One message a day at most, however many scopes dream.
- Recipes unused for long are not found yet: evidence holds Tape ids, not
  last use.
- `superchat-learn.el` goes to an 880-line cap, the person's standing choice
  for this module; L4 and L5 word ceilings rise to 740 and 5120.
