# Decision: The messages of a request in a module of their own

Status: implemented

## Problem

`superchat-agent.el` was at 700 lines, its cap: the next change to the run
loop could not be made without first finding lines. The file held two
kinds of code. About 120 lines built the messages of a request from the
`conversation` view: an entry's message, groups of a call and its results,
the budget, the size, what is never left out, the stand-in, leaving out to
fit. They are pure functions of entries and a budget, with no state, and
`spec/L3-agent.md` already gave them a section of their own, "Messages".
The rest (the run, its ending, cancel, retry, questions, handoff, fork)
keeps state in `superchat-agent--runs`.

## Decision

- A new L3 module, `superchat-request.el`, cap 200 lines, with the arrows
  `agent → request` and `request → llm`, owns the messages of a request.
- Its public functions are the ones the run calls:
  `superchat-request-message`, `-budget`, `-messages`, `-size`, `-fit`, and
  `-kept-size`, which the refusal measures against twice the budget and
  which took the place of a loop the run wrote out itself. The groups, the
  answered calls, what is never left out and the stand-in stay private to
  the module.
- The section "Messages" of `spec/L3-agent.md` is the section
  `superchat-request.el`, its rules unchanged, with the public functions
  listed.
- `superchat-agent.el` went from 700 to 577 lines and keeps its cap of 700.
  `test/test-request.el` tests the public functions; every test of
  `test/test-agent.el` passed unchanged, so no request sends anything
  different.
- `SPEC.md` may hold 2550 words, up from 2400: each module adds a row and
  an arrow that have no other home, and the sentences that said a rule
  twice were already gone.

## Alternatives considered

- Raise the cap of `superchat-agent.el`. Not taken: `AGENTS.md` says that
  code which does not fit means the design is wrong, and the file held two
  things that already had two sections of the specification.
- Move handoff and fork out instead, about 130 lines. Not taken: a run
  starts with a handoff when the timeline is over the budget, and a handoff
  must see the runs table to keep the timeline busy, so the two modules
  would need each other.

## Consequences

- The run loop has 123 lines to grow in before its cap.
- What a request sends can be read and tested without a run or a Tape.
