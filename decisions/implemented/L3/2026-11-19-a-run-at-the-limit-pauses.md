# Decision: A run at the limit pauses

Status: implemented

## Problem

A run that reached the round limit, looked back and did not go on ended
`error`, with the line "Stopped: 3 model responses without an answer; above
is what it did: 3 tool calls." shown in the error face, and the chat said
"Request failed". The person read it as something broken, each time; nothing
had failed: the run had worked, looked back and handed over.

## Decision

Such a run ends `done`, its look-back its text, and its event has the type
`paused`, shown like any other event, not in the error face: "Paused at the
round limit: N model responses, K tool calls. Send a message to go on."
This holds after a `blocked` look-back, at a level other than `auto`, and
after the last time a run went on.

## Alternatives considered

- Keep `error` and soften the words. Not taken: the face and the chat's
  "Request failed" come from the status, so words alone do not help.
- A new run status, `paused`. Not taken: every caller of `superchat-run`
  would have to learn it; the look-back is an answer, so `done` is true.

## Consequences

- No red line and no "Request failed" at the limit; text queued in the chat
  is sent after it, as after any answer.
- A caller that told the limit by the status `error` now sees `done`; the
  `paused` event tells it apart.
