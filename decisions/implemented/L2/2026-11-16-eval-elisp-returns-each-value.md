# Decision: eval_elisp returns the value of each form

Status: implemented

## Problem

`eval_elisp` evaluates every form of its code but returned only the last
value. In a run against DeepSeek, a model asked for `(* 6 7)` and
`emacs-major-version` sent both forms in one call, got back only `32`, and
had to call the tool again with the two forms in a list.

## Decision

`eval_elisp` returns what was printed to `*Messages*` while it ran, then the
value of each form, printed, one line each in the order of the forms. A
value is printed with `print-escape-newlines`, so a newline inside a string
is written `\n` and every value stays on its line. One form gives one line,
as before.

## Alternatives considered

- Keep returning the last value and let the description tell the model to
  wrap several values in a list. Not taken: the model sends several forms
  anyway, and a second call costs a round and, at level `ask`, a question
  to the person.

## Consequences

A form evaluated only for its effect, such as a `setq` or a `require`,
adds a line to the result. A string with a newline is shown escaped, also
when it is the only form. When a form signals, the result is the error and
the values of the forms before it are not shown.
