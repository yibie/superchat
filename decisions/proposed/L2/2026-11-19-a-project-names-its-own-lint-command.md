# Decision: A project names its own lint command

Status: proposed

## Problem

`edit` and `write` lint a file right after saving it (`superchat-lint.el`),
and `superchat-lint-functions` takes more checks. A check that belongs to a
project, such as this repository's gates (`scripts/gates.el`, 0.23 s: word
ceilings, line caps, a private name of another layer), can only be added in
the person's own configuration. So it lives on one machine, has to name the
repository's path, and nobody else who works on the repository gets it. In
the person's session `20261118-165024-f2af` about ten rounds went to
shortening wording until a word ceiling held, found only by running the
check by hand.

## Proposal

- A variable `superchat-lint-project-command`, nil by default: a shell
  command run in the project root after `edit` or `write` saves a file of
  that project. On a nonzero exit its last 10 lines join the result as lint
  lines, as `superchat-lint-command` does; `superchat-lint-seconds` bounds it.
- Lint already runs in the saved file's buffer, so a project sets the
  variable in its `.dir-locals.el`, a string, not a function:
  `((nil . ((superchat-lint-project-command . "emacs -Q --batch -l scripts/gates.el . | grep -v '^ok'"))))`.
- It is not marked safe: Emacs asks the person once whether to accept that
  value, since it runs the project's own code on every save; they may keep
  the answer.
- This repository gets such a `.dir-locals.el`.

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

## Acceptance criteria

- With the variable set for a directory, an `edit` there whose check fails
  shows the command's last lines as `Lint:` lines; one that passes adds
  nothing; a file outside the project runs no command.
- A command that runs past `superchat-lint-seconds` is stopped and said so.
- Opening this repository asks once about the value; after accepting, an
  edit that pushes a specification over its ceiling shows `WORDS …` in
  that same tool result.

## Risks

- A slow project command slows every save of the model's; the time bound
  and the person's choice of command are the guard.
- `superchat-lint.el` and the L2 specification grow by a variable.
