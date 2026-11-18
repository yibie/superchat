# Decision: A layer of its own for the work nobody watches

Status: proposed

## Problem

`superchat-inbox.el` (the inbox, its worker, the timers and the tools
`follow_up`, `schedule`, `cancel_timer` and `send`) is a module of L3, but
it uses L3 only through public functions (`superchat-run`,
`superchat-question`, `superchat-run-active`) and nothing in L3 requires
it: it is a user of the agent layer, as the interface is. It serves a use
L3 does not name: L3 is "the AI layer of Emacs", `superchat-run` for any
package; the inbox is the personal agent working while nobody watches.

Its rules made `spec/L3-agent.md` outgrow its word ceiling twice in one
day (2460 to 2810, 2810 to 3010), though its section is 393 of 2865 words;
the next decision, dreaming, adds a schedule of its own there. And L3 now
names the kinds `timer` and `inbox` in `superchat-agent--groups`: the agent
knows the data of the module that uses it.

## Proposal

- A new layer **L4 background** between the agent and the interface, with
  the specification `spec/L4-background.md` and the module
  `superchat-inbox.el`. Its section of `spec/L3-agent.md` moves there.
- The interface becomes **L5**: `spec/L4-interface.md` is renamed
  `spec/L5-interface.md`, and the layer table, the module table, the links,
  the exception for the Org display in `AGENTS.md` and the `L4:` lines of
  `NOTES.md` follow. `superchat-messages.el` stays in the interface.
- `superchat-question` and `superchat-answer` stay in L3: they say how a
  run stops and goes on.
- `superchat-agent--groups` keeps in a call's group every entry that gives
  no message, by that rule, not by a list of kinds; L3 no longer names
  `timer` or `inbox`.
- The arrows do not change. The gates compare layers with `string<` and
  `scripts/check.sh` takes `L[0-9]`, so `L5` needs no change to them.
- Decision notes under `decisions/*/L4/` move to the layer they concern
  now; past commit messages stay as they are.

## Alternatives considered

- Keep the inbox in L3 and give it a specification file of its own,
  `spec/L3-inbox.md`. Not taken: the layer table names one specification
  per layer, and the inbox would still be a user of L3 inside it.
- Number the new layer L3b and keep the interface at L4. Not taken by the
  person, who agreed to L4 background and L5 interface.
- Leave it as it is. Not taken: each new background feature would raise
  L3's ceiling again.

## Acceptance criteria

- `spec/L3-agent.md` holds no rule of the inbox, and its ceiling is
  lowered to 5% above what is left.
- `scripts/check.sh L4` runs the inbox's tests and `scripts/check.sh L5`
  the interface's.
- No file of L3 names the kinds `timer` or `inbox`.
- Every link in the specification files and decision notes still leads to
  a file.

## Risks

- A rename touches many files at once; one commit per kind of change
  (layer table, file rename, notes) keeps each reviewable.
- Notes written before the rename say `L4` for the interface; their
  history is in git, and the moved notes are rewritten to the new numbers.
