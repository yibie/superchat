# Decision: @MODEL answers one message

Status: implemented

## Problem

`/model` sets the model of a whole chat buffer. The person often wants one
question answered by another model, then to go on as before; the earlier
Superchat let them write `@MODEL` before the question.

## Decision

- A message that starts with `@MODEL` and more text, MODEL one of the
  provider's model ids, runs with that model for that request only; the
  text after it is what is sent and kept. The next message uses the
  buffer's model again.
- An `@` that names no model is sent as written.
- After an `@` at the start of the input, `completion-at-point` offers the
  model ids.
- The model that answered is already kept on each assistant entry.

## Alternatives considered

- Wait for profiles and their `@NAME`
  ([a profile is who answers](../../proposed/L0/2026-11-20-a-profile-is-who-answers.md)).
  Not taken: the person asked for this now; a profile's `@NAME` can take
  the same place later, a model id and a profile name told apart then.
- Make `@MODEL` stick for the rest of the conversation. Not taken: that is
  `/model`.

## Consequences

- A message meant to start with an `@word` that happens to be a model id
  goes to that model; write a word before it to send it as text.
- Reversed by
  [2026-11-20-a-model-is-chosen-with-slash-model.md](2026-11-20-a-model-is-chosen-with-slash-model.md):
  `@MODEL` is gone, and `/model` chooses the model.
