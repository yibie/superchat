# Decision: A saved file is linted at once

Status: implemented

## Problem

Mistakes in a file cost rounds when they are found late. In the person's own
use the model wrote U+ABCD into a test through an escaping slip and only saw
it rounds later; it shortened a specification ten times, checking its word
ceiling each time with a `shell` call; a paren it closed twice showed up only
at `scripts/check.sh`. The person asked for a lint rule, a check that gives
the error back at once, and said dreaming is no such immediate feedback.

## Decision

`superchat-lint.el` runs checks in the buffer of every file `edit` or
`write` saves, and their findings follow the tool's result as `[Lint: …]`
notices. Built in: unbalanced Emacs Lisp, and characters rarely meant (a
control, private-use or replacement character). A person or a project adds
checks to `superchat-lint-functions`; `superchat-lint-command` makes one of a
shell command, for files matching a regexp, reporting a non-zero exit with
the end of its output, stopped after 20 seconds. A lint reports; it never
refuses or undoes a change.

## Alternatives considered

- Learn such errors in dreaming as facts. Not taken for this: the person said
  dreaming is not immediate feedback; a fact is read the next day.
- Refuse an edit that fails a lint. Not taken: a change may pass through a
  broken state on its way, as an edit of one of two parens does.
- Run the project's whole check after each save. Not taken as built in: it
  can take minutes; a person picks the commands fast enough to run each time.
- Rules in `.dir-locals.el`. Not taken as the way: a command from a file in
  the project would run when the model saves; it is the person's
  configuration that names the commands.

## Consequences

- The model sees a broken paren or a stray character in the round it made it.
- A lint command runs on every save of a matching file, synchronously: a
  slow one slows each edit, up to its limit.
- U+ABCD itself is a real script's letter and is not caught by the
  character check; the paren check or a project's tests are what catch such
  a slip.
