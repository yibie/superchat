# Decision: A layer of its own for the work nobody watches

Status: implemented

## Problem

`superchat-inbox.el` (the inbox, its worker, the timers and the tools
`follow_up`, `schedule`, `cancel_timer` and `send`) was a module of L3, but
it used L3 only through public functions (`superchat-run`,
`superchat-question`, `superchat-run-active`) and nothing in L3 required
it: it was a user of the agent layer, as the interface is. It served a use
L3 does not name: L3 is "the AI layer of Emacs", `superchat-run` for any
package; the inbox is the personal agent working while nobody watches.

Its rules made `spec/L3-agent.md` outgrow its word ceiling twice in one
day (2460 to 2810, 2810 to 3010), though its section was 393 of 2865
words, and dreaming would add a schedule of its own there. And L3 named the
kinds `timer` and `inbox` in `superchat-agent--groups`: the agent knew the
data of the module that used it.

## Decision

- Layer **L4, background**, between the agent and the interface, serves
  use 3 while nobody watches. Its specification is
  `spec/L4-background.md` (ceiling 480 words) and its module
  `superchat-inbox.el`; the inbox section left `spec/L3-agent.md`, whose
  ceiling went down to 2600.
- The interface is **L5**: `spec/L5-interface.md`, with the layer table, the
  module table, the ceilings, the exception for the Org display in
  `AGENTS.md`, the `L5:` lines of `NOTES.md` and the layer parts of the
  decision notes brought up to date. `superchat-messages.el` is in L5.
- `superchat-question` and `superchat-answer` stay in L3: they say how a
  run stops and goes on.
- `superchat-agent--groups` keeps in a call's group every entry that gives
  no message, by that rule; L3's code and specification name no kind of the
  layer above. The Messages rule says that an entry of a kind it does not
  name gives no message.
- The arrows did not change. The gates compare layers with `string<` and
  `scripts/check.sh` takes `L[0-9]`, so `L5` needed nothing of them;
  `scripts/check.sh L4` runs the inbox's tests and `L5` the interface's.
- The notes of the inbox and the timers moved to `decisions/implemented/L4/`.
  The inbox's tests no longer use L3's private `superchat-agent--runs`,
  which the gates forbid across layers; they ask `superchat-run-active`.
- `SPEC.md` dropped two sentences that said a rule twice, to stay under its
  ceiling with the new rows.

## Alternatives considered

- Keep the inbox in L3 and give it a specification file of its own,
  `spec/L3-inbox.md`. Not taken: the layer table names one specification
  per layer, and the inbox would still be a user of L3 inside it.
- Number the new layer L3b and keep the interface at L4. Not taken by the
  person, who agreed to L4 background and L5 interface.
- Leave it as it was. Not taken: each new background feature would raise
  L3's ceiling again.

## Consequences

- A new background feature is written in L4 and measured against L4's
  ceiling; L3 grows only with the run itself.
- Commit messages before this decision say `L4` for the interface; notes
  and specifications say `L5`.
- `superchat-agent.el` is still at 700 of 700 lines: this decision moved no
  code. `2026-11-17-the-messages-of-a-request-in-a-module-of-their-own`
  proposes the move that makes room.
