# Explicit Claim Capture

## Decision

Add one explicit Emacs command that records a contestable Claim as an
append-only `claim.recorded` Tape event.  Replay normalizes that durable event
for the existing Human Correction contract.  Do not add a Current View,
materialized projection, table, slash command, Memory bridge, or Runtime
semantics.

## RALPLAN-DR

### Principles

1. Tape remains the only durable source of truth for this surface.
2. Claim is contestable domain data, not Memory or Fact.
3. Capture is explicit; no model extraction or prompt injection.
4. Replay is sufficient until a real Current View consumer exists.
5. One command and one event shape precede broader UI or learning machinery.

### Drivers

1. A captured Claim must survive database close/reopen.
2. Its shape must feed the proven Claim -> Feedback -> replacement replay.
3. Foundational Runtime boundaries and the dirty worktree must remain intact.

### Options

- **Dedicated tiny module + Tape replay (chosen):** clear domain owner, no
  migration, minimal coupling.  Revisit only if replay latency or a first-class
  Current View consumer appears.
- **Write from `superchat-tape-view.el`:** one fewer file, but rejected because
  that module explicitly owns read-only views.
- **Use Memory or a materialized Claim projection:** query-friendly, but
  rejected because it changes ownership and adds storage before a consumer
  proves the need.

## Implementation

1. Add `superchat-claims.el` with:
   - `superchat-claim-record`: validate non-empty statement, scope, and session;
     allow empty basis; require each supplied basis reference to match
     `tape:<positive integer>` and resolve to an existing Tape row; append
     `claim.recorded` through `superchat-tape-append-event-record`.
   - The Tape logical `event-id` is the Claim ID.  Do not store a second ID.
   - Payload: `:statement`, `:scope`, `:basis`, optional `:supersedes`.
     Envelope: schema 1, current session, actor `user` by default, topic
     `experience/claim`, confidential required payload.
   - `superchat-claims-replay`: replay one session, select Claim events,
     normalize envelope identity/time/actor plus payload to top-level Claim
     plists, and optionally filter by scope.  No registered projection.
   - Autoloaded `superchat-claim-capture`: use active region or `read-string`
     for the statement; prompt for scope and comma/space-separated basis refs;
     append only, with no LLM request.
2. Add an explicit autoload in `superchat.el`; no keybinding or slash command.
3. Add `test/test-claims.el` and load it from `test/run-tests.el`.
4. Update the design-contract status without broadening Feedback or Current
   View scope.

## Tests and acceptance

1. Interactive capture writes a Claim with an existing `tape:N` basis; after
   DB close/reopen, replay returns the same Claim ID, statement, scope, basis,
   actor, and recorded time.
2. Empty basis is accepted.  Empty statement/scope/session, malformed refs,
   and missing Tape rows are rejected before append.
3. A replay-normalized captured Claim composes with a Feedback record and an
   explicitly superseding replacement in the existing pure correction reducer:
   original and feedback remain, replacement becomes current.
4. Focused ERT and the canonical full ERT suite pass.

Commands:

```sh
emacs -Q --batch -L . -l test/test-claims.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
```

## ADR

- **Decision:** dedicated capture/replay module over existing Tape.
- **Drivers:** restart durability, correction compatibility, minimum diff.
- **Alternatives:** Tape View write, Memory reuse, materialized projection.
- **Why:** it is the smallest owner that preserves existing boundaries.
- **Consequences:** reads scan one session; basis syntax is deliberately only
  `tape:N`; there is no derived truth/status surface yet.
- **Follow-ups:** add Feedback capture or Current View only when a real user
  workflow consumes them; materialize only after measured replay cost.

## Execution roster

- Planner: decision and test shape (complete).
- Architect: boundary and over-engineering review (complete).
- Critic: consensus approval (complete).
- Executor/verifier: one solo owner for the focused diff and ERT verification.

Staffing guidance: stay solo; the diff is coupled and too small to justify a
team.  Launch hint: implement test-first, then run focused and full ERT.  If a
failure is unrelated to the focused diff, preserve it and report exact evidence
rather than modifying unrelated dirty files.
