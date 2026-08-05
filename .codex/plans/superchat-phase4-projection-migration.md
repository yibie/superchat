# Superchat Phase 4 — Projection Migration

## TL;DR

- **What you'll get:** Session configuration, Memory, and Team state are
  explicitly replayable Tape projections. Materialized Memory gains a durable
  versioned checkpoint and can be dropped and rebuilt without losing review
  decisions.
- **Approach:** Reuse the existing Tape and SQLite connection. Add one small
  projector registry, one checkpoint table, and event-first Memory/Team writes.
  Keep Session and Team as on-demand folds; only Memory is materialized.
- **Not included:** A second event store, per-event envelope columns, a Team
  table, artifact projections, background projector processes, snapshots, or
  distributed-consumer semantics.
- **Decisions:** Projection application and checkpoint advancement share one
  SQLite transaction. Existing Memory rows remain legacy durable material and
  are not deleted by rebuild. New projected rows retain stable source event
  references. No commit is created unless explicitly requested.

SQLite transactions provide the required atomic boundary; the logical
projection checkpoint is separate from SQLite WAL checkpointing:

- <https://www.sqlite.org/lang_transaction.html>
- <https://www.sqlite.org/lang_upsert.html>
- <https://www.sqlite.org/wal.html>
- <https://www.gnu.org/software/emacs/manual/html_node/elisp/Database.html>

## Scope

### In

- A minimal projection registry with version, initial state, reducer/apply,
  event selector, reset, replay, incremental catch-up, and rebuild.
- A durable `(projection, scope) → version/last-event-id` checkpoint.
- Atomic projection application plus checkpoint advancement.
- Session configuration replay through the registered Session reducer.
- Event-first Memory capture, review, rejection, and correction.
- Memory projection metadata and compatibility-preserving schema migration.
- Memory rebuild that preserves legacy rows and replays user review events.
- Team task, message, decision, and member events through the Tape service.
- A registered on-demand Team projection preserving old Tape rows.
- Focused replay/idempotency/rebuild tests and canonical verification.

### Out

- Promoting envelope fields into dedicated Tape columns; current queries do
  not need them.
- Materializing Session or Team into new tables; their current data volume and
  access paths do not justify duplicate storage.
- Background workers, snapshotting, or cross-process projector coordination.
- Memory review UI; this phase provides the event-first API and projection
  semantics used by a future UI.
- Workspace, Trigger, Workflow-resume, or Cowork integration changes.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Projection/checkpoint seam | None | 2, 3, 4 | One replay and materialization contract |
| 2. Session reducer registration | 1 | 5 | Proves pure on-demand projections |
| 3. Memory event-first materialization | 1 | 5 | Exercises checkpoints and rebuild |
| 4. Team event/reducer migration | 1 | 5 | Removes direct Tape writes and proves replay |
| 5. Compatibility and verification | 2, 3, 4 | None | Locks migration and failure semantics |

## Execution Order

```text
1. Projection registry + DB checkpoint/schema migration
2. Session registered reducer
3. Memory event-first projector + review/correction replay
4. Team registered reducer + Tape-service writes
5. Focused tests → canonical ERT → structural/static checks
```

## Tasks

### Task 1: Projection and checkpoint seam

- **What:** Add `superchat-projection.el` with registration, pure replay,
  incremental catch-up, and rebuild. Add a schema-v4 checkpoint table and a
  small DB transaction/checkpoint API.
- **Files:** `superchat-projection.el`, `superchat-db.el`,
  `test/test-projection.el`, `test/test-tape-view.el`.
- **Acceptance:** Applying one event and advancing its checkpoint are atomic;
  repeated catch-up is a no-op; a version change rebuilds through the
  registered reset path.

### Task 2: Session projection

- **What:** Move the existing Session configuration fold behind the registered
  Session projector without changing its public View API or branch lineage.
- **Files:** `superchat-tape-view.el`, `test/test-tape-view.el`.
- **Acceptance:** Capability, Topology, Authority, Workspace, Surface, and
  branch inheritance produce the same logical configuration after replay.

### Task 3: Memory projection

- **What:** Record capture/review/reject/correct facts in Tape before mutating
  the materialized Memory table. Add source event IDs, projection version,
  model/prompt provenance, and update time while preserving old columns and
  direct DB compatibility methods. Rebuild deletes only projected rows.
- **Files:** `superchat-memory.el`, `superchat-db.el`,
  `test/test-memory-sqlite-facade.el`.
- **Acceptance:** Pending rows are not recalled; rejected/superseded rows are
  not recalled; correction and review survive a full projection rebuild;
  legacy rows remain untouched.

### Task 4: Team projection

- **What:** Register the Team fold; write task/message/decision/member events
  through `superchat-tape-append-event`; read both new envelopes and existing
  legacy topic rows.
- **Files:** `superchat-subagent.el`, `superchat-tools.el`,
  `test/test-subagent.el`.
- **Acceptance:** Existing task and mailbox behavior remains compatible;
  decisions are publishable/listable; replaying the same Tape yields the same
  Team state without a new Team table.

### Task 5: Verification

- **What:** Add the focused projection and Memory suites to the canonical
  runner, test schema-v2→v4 migration, then run all repository checks.
- **Files:** `test/run-tests.el` plus evidence-driven fixes only.
- **Acceptance:** Focused suites and canonical ERT have zero unexpected
  failures; touched files pass `check-parens`, byte compilation introduces no
  new warnings, static searches show no new direct Team/Memory Tape bypass,
  and `git diff --check` is clean.

## Success Criteria

- [x] Session configuration is produced by a registered Tape projection.
- [x] Memory catch-up is checkpointed and idempotent.
- [x] Memory can be reset and rebuilt from Tape.
- [x] User review, rejection, and correction survive rebuild.
- [x] Legacy Memory rows and old Tape rows remain readable.
- [x] Team task/mailbox/decision state is replayable from one registered fold.
- [x] Team writes enter through `superchat-tape-append-event`.
- [x] No second event store or unnecessary materialized Team/Session table is
      introduced.
- [x] Canonical ERT has zero unexpected failures.
- [x] Touched modules pass structural and byte-compile verification.
