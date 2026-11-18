# Decision: The messages of a request in a module of their own

Status: proposed

## Problem

`superchat-agent.el` is at 700 lines, its cap: the next change to the run
loop cannot be made without first finding lines. The file holds two kinds
of code. Lines 30 to 150 build the messages of a request from the
`conversation` view: an entry's message, groups of a call and its results,
the budget, the size, what is never left out, the stand-in, leaving out to
fit. They are pure functions of entries and a budget, with no state, and
`spec/L3-agent.md` already gives them a section of their own, "Messages".
The rest (the run, its ending, cancel, retry, questions, handoff, fork)
keeps state in `superchat-agent--runs` and calls them through four entry
points: the budget, the size, the fitted messages and one entry's message.

## Proposal

- A new L3 module, `superchat-request.el`, cap 200 lines, owning the
  messages of a request: what is now lines 30 to 150 of
  `superchat-agent.el`. Its public functions are those four entry points,
  under the prefix `superchat-request-`.
- Arrows: `agent → request`, `request → llm, view`. `superchat-request.el`
  requires nothing that keeps state.
- The section "Messages" of `spec/L3-agent.md` becomes the section
  `superchat-request.el`, its rules unchanged.
- `superchat-agent.el` keeps the cap 700 and is about 120 lines under it.

## Alternatives considered

- Raise the cap of `superchat-agent.el`. Not taken: `AGENTS.md` says that
  code which does not fit means the design is wrong, and the file holds two
  things that already have two sections of the specification.
- Move handoff and fork out instead, about 130 lines. Not taken: a run
  starts with a handoff when the timeline is over the budget, and a handoff
  must see the runs table to keep the timeline busy, so the two modules
  would need each other.

## Acceptance criteria

- Every test of `test/test-agent.el` passes unchanged but for the names of
  the private functions it no longer uses; the tests of the messages move
  to `test/test-request.el`.
- `superchat-agent.el` defines none of the moved functions and is under 600
  lines.
- The gates find no use of a private name across modules of different
  layers, and no new exception.

## Risks

- Private names used by tests change; a test of the agent that called a
  moved private function must call the public one or move.
- The moved code is the part every request goes through; the move must not
  change what any request sends.
