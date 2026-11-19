# Decision: A chat run has no send

Status: implemented

## Problem

`send` tells a person who is not reading the conversation. In the person's
own use the model called it in a chat buffer the person was reading: once to
"tell" them `f1.txt: 1`, and in a later test the echo area showed
`Superchat: 1`. Each call also went to the notification hook, so a person at
the keyboard got macOS notifications of what was on their screen.

## Decision

A run sent from the chat input is given every tool but `send`. Runs nobody
watches (the inbox worker's) keep it; a question of such a run answered
from the chat goes on with that run's tools.

## Alternatives considered

- Say so only in `send`'s description. Not taken alone: the model used it in
  the chat against the words it already had.
- Let `send` know whether anyone watches. Not taken: the tool would have to
  know of the chat, a layer above it; the chat knows who reads its runs.

## Consequences

- A model in the chat answers in the chat.
- A plugin tool added later is offered to chat runs as before; only `send`
  is left out.
