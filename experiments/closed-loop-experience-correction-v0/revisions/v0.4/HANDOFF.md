# Closed-loop Experience Correction v0.4 — Handoff

Date: 2026-08-07

## Result

The corrected input contract was valid and all 24 preregistered first attempts
were retained. Persistence, correction replay, prompt consumption, and the
experiment boundary passed. The preregistered effect gate failed.

| Subclaim | Verdict | Evidence |
|---|---|---|
| Persistence | pass | The append-only correction survived database reopen; SQLite integrity is `ok`. |
| Correction | pass | Current View selects the replacement Claim while retaining the superseded Claim and Feedback. |
| Consumption | pass | 24 assignment questions and 72 stage prompts passed the exact-question contract; no prompt contained `Question: nil`. |
| Effect | fail | Corrected-current beat stale on `0/8` tasks and no-claim on `1/8`; the frozen thresholds required `6/8` and `3/8`. |
| Boundary | pass | The execution change contains only this v0.4 experiment revision; no product, Memory, Authority, Policy, schema, or held-out change occurred. |

The strongest supported conclusion is:

> Superchat preserved, replayed, and consumed an append-only human correction.
> The preregistered effect gate did not pass, so no effectiveness statement is
> allowed.

No Learning, causal recovery, Memory promotion, Policy change, or
generalization claim is allowed.

## Execution

- Launches/finalized first attempts: `24/24`.
- Prompt snapshots: `72/72`; exact input contract: `pass`.
- Durable workflow status: `23 completed`, `1 incomplete`.
- Evidence Closure: `23 pass`, `1 blocked`.
- Outcome Records: `24/24`.
- Blind evaluator: `15 parse-valid`, `9 invalid` under the frozen missing-data rule.
- Primary correction fidelity: `14 score-2`, `10 score-0`.
- No historical v0.3 run was imported and no first attempt was rerun or replaced.

The single incomplete workflow was
`clf-audit-critique-01-corrected-current`: collector and analyst artifacts were
deliverable, but the reviewer artifact was rejected by Closure as provenance
invalid. Its original attempt remains in the dataset and received score 0
under the preregistered missing-target rule.

## Effect gate

- Corrected-current score 2 on all 8 tasks: `fail` (`3/8`).
- Corrected-current strictly beat stale-claim: `0/8` (required `6/8`).
- Corrected-current strictly beat no-claim: `1/8` (required `3/8`).
- Corrected-current research quality worse than no-claim: `3/8` (maximum `1/8`).

The exact paired matrix and counterexamples are in `comparison-report.json`.

## Preserved launch incident

The first v0.4 process stopped before an accepted Runtime Run because batch
Emacs did not load Nova Emacs's installed `llm-deepseek` package directory.
No model call started and no assignment attempt was consumed. The incident is
retained in `attempt-2-launch-infrastructure.json`; the same frozen experiment
then ran with that deterministic package directory initialized.

## Verification

- Source Cohort database SHA-256 remained
  `dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c`.
- Final working database SHA-256 is
  `fc36ccb8cc440a8f2325f05d25be15220abd9cd9d9c09c241b1d16dd8d7d3d02`.
- SQLite `PRAGMA integrity_check`: `ok`.
- Frozen input hashes: `pass`.
- Analyzer self-check: `pass`.
- Canonical ERT: `592 tests`, `576 expected`, `0 unexpected`, `16 skipped`.
- The external handoff archive contains the working database, 72 runtime
  artifacts, launch/prompt records, all run records, and verified
  `SHA256SUMS`.
