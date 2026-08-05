# Terminal-state Closure & Runtime Boundary Hardening

## Goal

Close every Runtime terminal path without adding a second lifecycle abstraction.

## Invariants

1. A transport callback or synchronous transport error after `run.started`
   reaches the same Runtime terminal protocol.
2. Successful, failed, cancelled, and timed-out Runs persist a Run terminal
   event followed by `turn.completed`.
3. `superchat-turn-completed-functions` runs once for every terminal status.
   `superchat-run-completed-functions` remains success-only.
4. `audit-incomplete` cannot promise a persisted terminal event, but it still
   closes in-memory Turn state, runs terminal hooks, and releases resources.
5. Product entry points do not invoke model transport directly.

## Changes

1. Add regression tests for synchronous transport failure, terminal hooks, and
   Surface bypasses.
2. Route synchronous transport errors through Runtime finishing after the
   pre-execution audit succeeds.
3. Reuse Runtime finishing for cancellation while preserving provider
   cancellation and delivery behavior.
4. Remove the unused synchronous model loop.
5. Remove the obsolete `agent-mode` request/transport/UI flag.

## Verification

- Focused Runtime, ecosystem, workflow, rewrite, subagent, and backend tests.
- Canonical `test/run-tests.el` suite.
- Repository search for direct model calls, `agent-mode`, and legacy
  completion owners.

## Deferred

- No Interaction Mode abstraction.
- No Tool DSL or new Runtime interface.
- No Telega/Trigger implementation in this phase.
