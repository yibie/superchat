# Decision: A project names its own lint command

Status: implemented

## Problem

`edit` and `write` lint a file right after saving it (`superchat-lint.el`),
and `superchat-lint-functions` takes more checks. A check that belongs to a
project, such as this repository's gates (`scripts/gates.el`, 0.2 s: word
ceilings, line caps, a private name of another layer), could only be added
in the person's own configuration. So it lived on one machine, had to name
the repository's path, and nobody else who works on the repository got it.
In the person's session `20261118-165024-f2af` about ten rounds went to
shortening wording until a word ceiling held, found only by running the
check by hand.

## Decision

- `superchat-lint-project-command`, nil by default, is a shell command run
  after `edit` or `write` saves a file, in the directory of the
  `.dir-locals.el` that set it, as `superchat-lint-command` runs one: an
  exit other than 0 adds the command, its status and its last 10 lines.
- A project sets it in `.dir-locals.el`, a string. It is not marked safe:
  it runs the project's code on every save, so the person accepts the value
  once, when Emacs asks as they open a file there, and may keep the answer.
- A file a tool visits gets only the local variables already trusted
  (`enable-local-variables` `:safe`). Without that, an untrusted
  `.dir-locals.el` would ask its question from inside a tool call, holding
  Emacs, with nobody watching in a background run.
- This repository's `.dir-locals.el` runs the gates and reports any line
  but `ok` and the total.

## Alternatives considered

- Leave it to the person's configuration with `superchat-lint-command`.
  Not taken alone: it stays on one machine and names a path.
- A function in `.dir-locals.el` (`eval:`). Not taken: Emacs flags `eval`
  as risky every time, and a string is enough.
- Mark the variable safe for any string. Not taken: a cloned repository
  could then run any command on every save without being asked.
- A Superchat file of its own in the project root (`.superchat/lint`). Not
  taken: `.dir-locals.el` is Emacs's own place for per-project settings and
  already carries the trust question.
- Let tools visit files as Emacs does by default. Not taken: the question
  would come from inside a tool call, as said above.

## Consequences

- Until the person accepts the value, no project command runs; Superchat
  never asks for it.
- A tool no longer applies a file's untrusted local variables, of any
  project; the person's own visits are unchanged.
- A slow project command slows every save of the model's; the time bound
  and the choice of command are the guard.
