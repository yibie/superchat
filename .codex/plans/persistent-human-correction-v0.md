# Persistent Human Correction v0 — Work Plan

## TL;DR

- **Outcome:** one real Cohort v6 Claim is corrected durably without deleting
  the original, then recovered from Tape after a database close/reopen.
- **Approach:** extend the existing Claim module with durable Feedback, an
  atomic correction command, and a pure replay Current View; run the same
  production API on a temporary copy of the frozen Cohort v6 database.
- **Not included:** Memory promotion, Learning, automatic extraction,
  materialized Projection, new table, or Runtime/Closure/Authority changes.

## RALPLAN-DR

### Principles

1. Correction appends; it never rewrites history.
2. Basis references remain evidence pointers, not truth or Memory.
3. Current View is a pure interpretation of Tape.
4. Cross-Session evidence is valid only inside a named, hashed database.
5. The smallest verified surface wins.

### Decision drivers

1. The real recovery counterexample spans Tape rows in two Sessions.
2. The user-visible correction must not leave a half-written pair.
3. The frozen 5MB database must not be duplicated in source control.

### Options considered

- **Database-local `tape:N` basis (chosen):** smallest change; report must bind
  references to the source DB hash.
- **Qualified cross-Session reference syntax:** more portable, but adds a new
  grammar and UI surface before a second consumer exists.
- **Copy source facts into one Session:** preserves current validation but
  stops citing the real frozen rows and obscures provenance.

## Scope

### In

- Database-local existence validation for Claim/Feedback `tape:N` basis.
- Same-Session Feedback target and same-Session/same-scope supersession.
- Durable `feedback.recorded` API.
- `M-x superchat-claim-correct`, committing replacement Claim and Feedback in
  one existing SQLite transaction.
- Pure `superchat-claims-current-view` for one Session and scope.
- Focused ERT coverage and one real Cohort v6 experiment report.

### Out

- New schema, table, materialized Projection, dependency, slash command, or
  automatic retry.
- Memory, Workflow, Runtime, Closure, Authority, policy, capability, or
  Learning changes.
- Effectiveness, causality, promotion, or learning claims.

## Dependency graph

| Task | Depends on | Blocks |
| --- | --- | --- |
| 1. Lock correction behavior with tests | None | 2 |
| 2. Implement Feedback, atomic correction, Current View | 1 | 3 |
| 3. Run real Cohort v6 correction experiment | 2 | 4 |
| 4. Verify and document the completed boundary | 3 | None |

## Execution waves

```text
Wave 1: Task 1
Wave 2: Task 2
Wave 3: Task 3
Wave 4: Task 4
```

The tasks are sequential because each downstream proof consumes the exact
production surface established by the previous task.

## Tasks

### Task 1 — Regression contract

- **Files:** `test/test-claims.el`
- **Agent:** `executor` or solo owner, high reasoning.
- **What:** add tests for cross-Session basis, Feedback validation, transaction
  rollback, both event orderings, explicit supersession, and close/reopen.
- **Acceptance:** tests fail because the new production functions do not yet
  exist, while existing Claim tests remain intact.
- **QA:**
  `emacs -Q --batch -L . -l test/test-claims.el -f ert-run-tests-batch-and-exit`
- **Change description:** `Make human corrections durable without erasing the
  claims they replace` using the repository Lore trailers.

### Task 2 — Minimum production surface

- **Files:** `superchat-claims.el`, `superchat.el` only if an explicit autoload
  is required.
- **Agent:** `executor`, high reasoning.
- **What:**
  - validate `tape:N` against any existing row in the current DB;
  - append validated `feedback.recorded` events;
  - add `superchat-claim-correct`, writing replacement first and Feedback
    second inside `superchat-db-call-with-transaction` so Feedback can point to
    the real replacement event ID without inventing an ID generator;
  - add pure `superchat-claims-current-view(session-id, scope)` returning only
    `:claims`, `:feedback`, and `:current`.
- **Acceptance:** rollback removes the replacement when Feedback append fails;
  replay handles both original→Feedback→replacement and
  original→replacement→Feedback; competing Claims do not replace current.
- **QA:** focused ERT passes.

### Task 3 — Real persistent correction experiment

- **Files:**
  `experiments/experience-feedback-v0/run-human-correction-v0.el` and generated
  `experiments/experience-feedback-v0/human-correction-v0-report.json`.
- **Agent:** solo executor; no external model call.
- **What:** copy the frozen v6 DB to a temporary directory, verify rows
  `961/962/964/1577/1578/1580`, call the production correction API, close and
  reopen SQLite, replay Current View, assert all links, then write the report
  and delete the temporary copy.
- **Acceptance:** report contains source path/hash, unchanged source post-hash,
  temp pre/post hashes, SQLite integrity result, source row Session identities,
  original/replacement/Feedback event and storage IDs, replayed records,
  selected current Claim, and prohibited claims.
- **QA:**
  `emacs -Q --batch -L . -l experiments/experience-feedback-v0/run-human-correction-v0.el`

### Task 4 — Boundary and full verification

- **Files:** `docs/design/experience-feedback-contract-v0.md`.
- **Agent:** solo owner, then independent verifier if needed.
- **What:** record that durable Feedback and pure Current View are implemented;
  distinguish DB-local basis from Session-local supersession; link the real
  experiment report.
- **Acceptance:** source v6 DB SHA-256 remains
  `dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c`;
  focused and canonical suites pass; focused diff contains no forbidden layer.
- **QA:**
  `emacs -Q --batch -L . -l test/run-tests.el` plus source hash and SQLite
  integrity checks.

## Verification strategy

- **Unit/integration:** focused Claim ERT tests.
- **End-to-end:** production API against a copied real Cohort v6 database,
  including actual DB close/reopen.
- **Regression:** canonical ERT suite.
- **Evidence audit:** source hash unchanged, no committed database duplicate,
  report links each derived record to real Tape rows.
- **Boundary audit:** no Memory write, new table, materialized Projection,
  Runtime/Closure/Authority semantic change, effectiveness claim, or Learning
  claim.

## ADR

### Decision

Use DB-local `tape:N` evidence references for Claim/Feedback while keeping
targets and supersession Session-local. Commit each correction pair in one
existing SQLite transaction and derive Current View directly from replay.

### Why

Cohort-level observations legitimately span Sessions. Qualified references are
premature; copying facts would weaken provenance. A transaction prevents the
operator command from manufacturing half a correction. Direct replay is
enough for the only current consumer.

### Consequences

- A report must name the database hash that gives `tape:N` meaning.
- Current View is linear in Session event count.
- Replacement precedes Feedback in storage order, but they become visible in
  one commit and the reducer supports both orderings.

### Follow-ups

- Add qualified references only when a second cross-database consumer exists.
- Materialize only after measured replay cost justifies it.
- Add broader UI only after real correction usage proves the need.

## Staffing guidance

Available agent types include `executor`, `test-engineer`, `verifier`,
`architect`, `critic`, `debugger`, and `code-reviewer`.

- **Ralph path:** one `executor` at high reasoning owns all four sequential
  tasks; a `verifier` at high reasoning independently checks the report and
  full suite.
- **Team path:** `executor` owns production code, `test-engineer` owns focused
  tests, `writer` owns the contract update, and `verifier` runs the merged
  experiment and suite. Suggested launch: `$team execute
  .codex/plans/persistent-human-correction-v0.md` or
  `omx team ...` inside an OMX CLI runtime.
- **Team verification:** verifier reruns focused ERT, experiment script, source
  hash/integrity checks, full ERT, and a focused diff audit after all lanes
  converge.

## Completion checklist

- [x] Original Claim, replacement Claim, and Feedback all persist.
- [x] Original remains addressable; replacement is current after reopen.
- [x] Feedback points to both target and replacement.
- [x] Six real Cohort v6 rows are present and span the disclosed Sessions.
- [x] Source DB hash is unchanged; no copied DB is committed.
- [x] Focused and canonical tests pass.
- [x] Report prohibits causality, effectiveness, Memory, policy, and Learning
      claims.
