# Decision: Commands and models complete as typed

Status: implemented

## Problem

The person wanted the input to offer completions as soon as `/` or `@`
is typed. Only `@MODEL` had completion, and only when asked for with a
key; slash commands had none, so their names had to be remembered or
read from `/help`.

## Decision

- One completion function serves the start of the input: after `/` it
  offers the slash commands, each annotated with its line of help; after
  `@` the model ids.
- Typing `/` or `@` as the input's first character calls
  `completion-at-point` at once, so the list shows without a key. Any
  completion interface the person uses (the default list, Corfu,
  Company) shows it.

## Alternatives considered

- Leave the popup to the person's completion package (Corfu's
  `corfu-auto`). Not taken: Corfu waits for two typed characters by
  default, so a bare `/` shows nothing, and the chat would then behave
  differently under each package.
- Ask for completions after every character. Not taken: the completion
  package already follows the typing once the list is open.

## Consequences

- A `/` or `@` typed later in the text offers nothing; only the first
  character of the input does.
