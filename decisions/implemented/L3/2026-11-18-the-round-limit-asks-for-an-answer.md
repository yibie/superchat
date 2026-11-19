# Decision: The round limit asks for an answer, and ends as stopped

Status: implemented

## Problem

`superchat-max-rounds` stopped a run dead: the fortieth model response ran its
calls, and the run ended with an `error` event reading `Stopped: 40 model
responses without an answer.` The person got a chat buffer ending in an error
with nothing said about the work, and the model was never asked. That is what
happened to a run dispatched in this repository: it edited
`superchat-inbox.el` halfway, reindented a test file, and ended there.

The limit is right. A run that keeps calling without answering is looping,
and something must stop it; a run of 40 responses is also a run that has cost
the person a great deal of money. What was wrong is the shape of the stop: it
threw away the one thing the run had, an in-memory picture of what it did,
instead of asking for the cheapest useful thing left.

## Decision

When the count reaches the limit, the run makes one last request: no tools,
and a system line (`superchat-agent--limit-note`) saying the limit is reached
and to answer with what it has, saying what it did, what is left, and what
the person should check. The model cannot call a tool, so it can only write.

That answer is recorded as an `assistant` entry with no `:calls`, whatever
calls the response claims — the Tape then holds a true fact, and the chat
buffer does not show a call with no result. The run keeps its status `error`
and its `Stopped: N model responses without an answer.` event after the
answer, so nothing that reads the status changes and the person is told what
happened.

The calls of the last ordinary response still run before this last request:
the run does not abandon a call it has already started, and the request that
follows reads their results.

`spec/L3-agent.md` says this in the rule of the round limit.

## Alternatives considered

- Raise `superchat-max-rounds`. It does not fix the stop, it moves it; a run
  that loops loops for more rounds and costs more. The default stays 40.
- End with status `done` and the answer. The run did not do what was asked,
  and `done` says it did: the inbox worker, the chat buffer and the person
  all read the status. A run that hit the ceiling is a failure that has
  something to say, not a success.
- Say nothing and stop as before, but keep the model's text of the last
  response. It is usually a sentence announcing calls to come, and it is
  already on the Tape; more of the same tells the person nothing new.
- Ask the last request for a summary through an extra tool, or a second
  short request after the stop. Both cost more than one request with no
  tools, and the second has no picture of the run that the first lacks.

## Consequences

- A stopped run ends with: the last response's calls and their results, an
  `assistant` entry with the model's answer and no calls, then the
  `Stopped:` error event.
- One more request is made at the limit, never with tools, so the run cannot
  grow past it.
- `test/test-agent.el` holds it: `superchat-agent-max-rounds` checks three
  requests, that the third has no tools and carries the note, that the
  answer is on the Tape with no calls, and that the run ends `error` with the
  `Stopped:` text.
