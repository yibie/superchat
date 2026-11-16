# Decision: Decision notes and word ceilings

Status: implemented

## Problem

The specification states each rule in the present tense and keeps no reason
for it. The reason lives in a commit message or a conversation, where the
next person to change the rule does not look, so a settled choice can be
argued again from the start. The specification files also have no limit on
their size, while every module has a line cap.

## Decision

A commit that adds or changes a rule of a specification adds or updates a
decision note in `decisions/STATE/LAYER/`, in the same commit. The note
says what the problem was, what was decided, which alternatives it beat and
what follows from it. `scripts/gates.el` checks each note's path, status
line and sections, and rejects an invalid note before it checks the
repository. `AGENTS.md`, `SPEC.md` and each `spec/L*.md` have a word
ceiling in `SPEC.md`; the gates fail a file over its ceiling, a file more
than a tenth under it, and a file with none.

The way of working was taken from another project's practice of agent notes
with a lifecycle, a checked shape and documents under budget, and fitted to
this repository: the layer is the class of a note, and the gates are the
existing `scripts/gates.el`.

## Alternatives considered

- Fill in notes for the decisions already made, from the 81 commits of the
  history. Not done: the history rarely records what a choice beat, and an
  invented alternative is worse than none.
- Move the reasons already written in `SPEC.md` and `spec/L*.md` into
  notes, so that the specification holds only rules. Not done now: the
  specification works as it is, and the move would touch every layer at
  once.
- An `archived/` state frozen by a hash manifest. Not taken: with no notes
  yet, nothing is stale; a reversal deletes the old note and git keeps it.
- A note in two languages kept in step by recorded hashes, a pull request
  template, and approvals weighted by reviewer. Not taken: one person works
  here and commits to `main`; these guard a team's process.
- Checking the same-commit rule by a gate that compares the commit's diff.
  Not taken: the gates read the files of the repository, not its history,
  and "a rule changed" cannot be told from a diff by reading it mechanically.

## Consequences

Most commits that touch a `spec/` file now also touch `decisions/`. A rule
in a specification and its reason are in two places on purpose: the rule
in the specification, the reason in its note, each said once. Raising a
word ceiling is a visible act in a commit, after moving and compressing
have been tried. The gates check a note's shape, not whether its reasons
are right; that stays with review.
