# Decision: A skill grows by the forms it lacks

Status: implemented

## Problem

[2026-11-21-a-skill-is-drafted-from-cited-code-that-ran.md](2026-11-21-a-skill-is-drafted-from-cited-code-that-ran.md)
drafted a whole SKILL.md each time and replaced the file on save. Every
`/learn` had the model rewrite every recipe, the descriptions changed
from one draft to the next, and what the person edited or deleted came
back on the next save.

## Decision

- A form is known when its trimmed code is an `elisp` block of the
  existing SKILL.md or is in `rejected.eld` beside it. Known forms are
  not sent; with nothing new, `/learn` says so and drafts nothing.
- For an existing skill the run writes only the new sections, no
  frontmatter. `C-c C-c` appends them, leaving the rest of the file as
  the person left it, and sets `version:` to the package's.
- A block the draft showed and the person deleted before saving goes to
  `rejected.eld`, a list of code strings read with `read`.
- When the package's version differs from the skill's, the header line
  shows both; older forms are not changed.

## Alternatives considered

- Keep replacing the whole file. Not taken: it loses the person's edits
  and spends a request on forms already written.
- Let the model merge the old file with the new forms. Not taken: the
  model would rewrite text the person wrote, and its merge could not be
  checked mechanically.
- Forget deleted forms. Not taken: every later `/learn` would offer them
  again.

## Consequences

- `superchat-learn.el` grew to 216 lines; its cap went from 150 to 250,
  the person's choice, since growing a skill is part of drafting one.
- A form the model left out as a probe is not recorded as dropped, so it
  can be offered again; only what the person deleted is remembered.
- Forms are compared byte for byte after trimming: a reformatted copy of
  a known form counts as new.
