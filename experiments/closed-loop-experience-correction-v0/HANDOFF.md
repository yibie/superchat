# Closed-loop Experience Correction v0 — Handoff

Date: 2026-08-06

## Result

The preregistered 24-assignment run finished without substituting any Agent
first attempt.  A post-run audit found a fatal model-input contract violation:
all 67 captured model prompts contained `Question: nil`, while the blind
evaluator received the real task question.  The raw run is retained, but it
cannot support Consumption, Effect, or Learning claims.

| Subclaim | Verdict | Evidence |
|---|---|---|
| Persistence | pass | After database close/reopen, two Claims and one Feedback event remain; SQLite integrity is `ok`. |
| Correction | pass | Current View selects the replacement Claim while the superseded Claim remains addressable. |
| Consumption | invalid | All 67 delivered prompts matched their assigned arm but omitted the task question; three assignments also did not reach all three stages. |
| Effect | invalid | The evaluator graded against task questions the model never received, so the raw scores are not interpretable as treatment effects. |
| Boundary | pass | The jj diff contains only this experiment and its plan; no Runtime, Authority, Memory, Workflow, Workspace, schema, or materialized Projection code changed. |

The strongest supported conclusion is:

> Superchat preserved and replayed an append-only correction.  Downstream
> Consumption and Effect were not validly evaluated in this cohort.

## Executions and outcomes

- Assignments launched/finalized: `24/24`.
- Evidence Closure: `19 pass`, `5 blocked`.
- Model-facing prompt snapshots: `67/72` planned; `67/67` observed snapshots
  matched their assigned arm, and `67/67` also contained `Question: nil`.
- Outcome Closure: `24/24 pass` (the evaluation process was recorded even when
  the result was reject/missing).
- Evaluator output: `18 parse-valid`, `6 invalid` under the frozen missing-data
  rule.
- Primary outcome: `14 score-2/accept`, `10 score-0/reject`.
- The score and quality counts below are retained as descriptive audit data
  only; the input-contract violation makes treatment comparisons invalid.

Five first attempts did not close all evidence gates:

- `clf-evidence-synthesis-01-stale-claim`: its persisted launch record claimed
  settlement, but replayed durable Tape left the reviewer Run open.
- `clf-incident-diagnosis-02-corrected-current`: analyst transport failed with
  curl error 35; reviewer was blocked.
- `clf-incident-diagnosis-02-no-claim`: collector transport failed with curl
  error 35.
- `clf-audit-critique-01-corrected-current`: reviewer transport failed with
  curl error 35.
- `clf-audit-critique-02-stale-claim`: collector transport failed with curl
  error 35.

These attempts remain in the dataset and retain their original scores.  They
were not deleted, retried, or replaced.  Four failures cluster into two TLS
handshake windows; they are infrastructure failures, not semantic treatment
outcomes.

## Paired primary scores

Columns are `no-claim / stale-claim / corrected-current`.

| Task | Scores |
|---|---|
| audit-critique-01 | `0 / 2 / 0` |
| audit-critique-02 | `2 / 0 / 2` |
| evidence-synthesis-01 | `2 / 2 / 2` |
| evidence-synthesis-02 | `0 / 2 / 2` |
| incident-diagnosis-01 | `2 / 2 / 0` |
| incident-diagnosis-02 | `0 / 0 / 0` |
| operational-recommendation-01 | `0 / 2 / 0` |
| operational-recommendation-02 | `2 / 2 / 2` |

The exact raw matrix and all counterexamples are in `comparison-report.json`.
It is retained for audit only.

## Preserved experiment incidents

No incident was rewritten as a successful sample:

1. v0 and v0.1 stopped before an accepted Runtime Run because the batch launch
   surface was incomplete.  Both records remain as
   `attempt-0-launch-infrastructure.json` and
   `attempt-1-launch-infrastructure.json`.
2. The frozen evidence consumer expected `proved` for
   `trace-well-formed`; Closure returns `valid`.  Runs completed under the
   frozen consumer, then the same Tape/Artifact/Outcome records were replayed
   once with the documented decoder erratum.  No model was called again.
3. The launch record retained `settled=true`, but replayed durable Tape did not
   prove the reviewer Run terminal.  The v0.3 runner incorrectly trusted the
   cached launch field over Tape.  The corrected runner now rejects that
   contradiction; it does not reinterpret the historical Run as settled.
4. The analyzer converted JSON null to the string `nil` and conflated a
   complete failure record with a missing Evidence Bundle.  The deterministic
   consumer was corrected after all model runs; Outcome Records were not
   reevaluated.
5. Assignments reference questions through `task_id`, but the runner read the
   nonexistent inline `assignment.question`.  Its synthetic preflight fixture
   contained that inline field and masked the mismatch.  Consequently all 67
   model prompts contained `Question: nil`, while the evaluator correctly
   joined the task question.  The runner now validates the real frozen join
   before any model call.  Existing records remain unchanged; any rerun needs
   a new preregistered revision.

The manifest's earlier correction event IDs identify the source correction
report.  The retained working database necessarily generated new event IDs
when that frozen correction was appended to its copied Cohort v6 Tape.  The
working Current View and model-facing prompts are therefore proved by the new
working IDs and exact statement content, not by equality with the source
report's event IDs.

## Verification

- Source Cohort v6 database SHA-256 stayed
  `dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c`.
- The packaged `source/runtime-snapshot.db` was recovered from the previously
  sealed Cohort v6 handoff archive and matches that frozen hash.  The live
  temporary v6 database changed later and was not substituted for the source
  snapshot.
- Retained working database SHA-256 after the run is
  `d1f38a819785de70410b6188fc8054a1d81b159ff9d20907e8444cf84be869aa`.
- Independent restart replay: `pass`; current Claim is the replacement;
  history count `2`, feedback count `1`.
- SQLite `PRAGMA integrity_check`: `ok`.
- Claim-focused ERT: `7/7`, `0 unexpected`.
- Runner regression ERT: `4/4`, `0 unexpected`.
- Canonical ERT: `590 tests`, `574 expected`, `0 unexpected`, `16 skipped`.
- Analyzer self-check: `pass`.
- The frozen and post-run-remediated SHA-256 values for `run.el`,
  `evidence.el`, and `analyze.el` are recorded in
  `validity-erratum-v0.3.json`; the frozen manifest hashes were not rewritten.

## Boundaries

This result does not prove Learning, a causal recovery strategy, automatic
Memory promotion, Workflow extraction, or an Authority/Policy change.  No
Consumption or effectiveness statement is permitted because the model-input
contract was violated.  The raw effect gate is reported only for audit.
