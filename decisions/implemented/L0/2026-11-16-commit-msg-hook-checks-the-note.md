# Decision: A commit-msg hook checks that a specification change brings a note

Status: implemented

## Problem

The rule that a commit changing a specification adds a decision note was
checked only in review. One person commits straight to `main` here, so no
review stands between a commit and the history, and most commits change a
`spec/` file: a rule with nothing to stop its breach would not hold.

## Decision

`scripts/hooks/commit-msg` refuses a commit whose staged files include
`SPEC.md` or a `spec/*.md` file, unless they also include a file under
`decisions/` or the message has a line `Decision: none — WHY`. The waiver is
written, not guessed: the hook cannot tell a wording fix from a new rule,
so the person committing says which it is, and the reason stays in the
history where it can be read. `git config core.hooksPath scripts/hooks`
turns the hook on. `scripts/check.sh` runs the hook with `--self-test`,
which shows it a specification change with no note and a waiver with no
reason, both of which it must refuse, and a change with its note and a
waiver with its reason, both of which it must accept.

## Alternatives considered

- Leave the rule to review, as
  [the earlier decision](2026-11-16-decision-notes-and-word-ceilings.md)
  did, because the gates read files and not history. Replaced: the hook
  reads the staged files and the message, which is where the commit is,
  and `scripts/gates.el` stays a reader of files.
- A hook that refuses every specification change without a note, with no
  waiver. Not taken: a wording fix or a fix that makes the code follow the
  specification would need an empty note.
- A `pre-commit` hook. Not taken: it cannot read the commit message, so it
  cannot see a waiver.

## Consequences

A commit that changes a specification fails until it brings a note or a
waiver with a reason. The hook checks only that one of the two is there;
whether a waiver's reason is true is still for review. The hook is off in a
fresh clone until `core.hooksPath` is set. `git commit --amend` checks only
what is newly staged, not the files the commit already had, and
`git commit --no-verify` skips the hook.
