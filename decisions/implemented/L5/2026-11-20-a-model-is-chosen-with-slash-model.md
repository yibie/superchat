# Decision: A model is chosen with /model

Status: implemented

## Problem

`@MODEL` before a message chose its model for that one message
([2026-11-20-at-model-for-one-message.md](2026-11-20-at-model-for-one-message.md)).
Since then `/model` completes its argument, noting each model's window,
input cost and image support, and `@` is wanted for referring to files
and buffers. Two ways to choose a model, one of them taking `@`, is one
too many.

## Decision

- `/model NAME` is the one way to choose the model; it holds for the
  chat buffer until changed.
- `@` starts nothing: a message that starts with it is sent as written,
  and the input neither completes nor styles it.

## Alternatives considered

- Keep `@MODEL` beside `/model`. Not taken: it holds `@`, which file and
  buffer references need, and gives the same choice two syntaxes.
- `/model NAME TEXT` to answer one message with NAME. Not taken: the
  person asked for `/model [name]` completion only.

## Consequences

- Asking one question of another model takes `/model` before it and
  `/model` back after.
- `@` is free for the proposed file and buffer references.
