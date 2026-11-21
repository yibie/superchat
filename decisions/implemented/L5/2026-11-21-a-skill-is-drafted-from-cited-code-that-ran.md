# Decision: A skill is drafted from cited code that ran

Status: implemented

## Problem

Step 2 of
[../../proposed/L0/2026-11-20-what-this-emacs-can-do.md](../../proposed/L0/2026-11-20-what-this-emacs-can-do.md):
what a run learns about a package is lost when it ends. Gathering every
`eval_elisp` call that succeeded found mostly probes; since
[../L0/2026-11-21-an-answer-names-the-calls-it-rests-on.md](../L0/2026-11-21-an-answer-names-the-calls-it-rests-on.md)
an answer names the results it rests on, and those can be told apart.

## Decision

- An L1 view `recipes :name PREFIX` gives the `eval_elisp` results that
  did not fail, that an answer cites, and whose code contains PREFIX, each
  code once.
- `superchat-learn` (`/learn PREFIX`), in a new L5 module: a run with no
  tools titles each form and writes the description; code then replaces
  every block that is not, trimmed, one of the forms that ran, and adds
  `version:` from the library header.
- The draft waits in a buffer; `C-c C-c` writes
  `superchat-directory`/`skills/PREFIX/SKILL.md`, asking before it
  replaces one; `C-c C-k` drops it. The person's key writes the file.
- The L5 word ceiling goes from 4200 to 4420 for the new module's section.

## Alternatives considered

- Every successful call as a source. Not taken: on the person's Tape no
  code had run twice and most of it only probed.
- The model's own account of what it learned. Not taken: it repeated a
  wrong fact in 7 runs in the facts trial.
- Gather from the current conversation only. Not taken: citations are
  kept on the Tape across conversations, and a package is learned over
  many.
- Leave out runs the person corrected next. Not built: what counts as a
  correction is not yet defined; the person reviews the draft instead.

## Consequences

- Only answers made after citations existed feed the view; older runs
  give nothing.
- Recipes are not yet marked when the package's version changes, and
  dreaming does not draft skills; both remain from the proposal, with its
  step 3, a skill's declared interface.
