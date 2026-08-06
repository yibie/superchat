# Closed-loop Experience Correction v0 — Work Plan

## TL;DR (For humans)

- **What you'll get:** one preregistered 24-Run experiment that tests whether a
  real, append-only human correction survives restart, reaches later Research
  Workflow prompts, improves correction fidelity, and leaves Runtime,
  Authority, Memory, and historical Facts unchanged.
- **Approach:** run eight frozen downstream tasks in three matched conditions:
  no Claim, the superseded Claim, and the replay-derived current Claim.  Archive
  the actual post-correction database, prompt snapshots, Run evidence,
  Artifacts, blinded outcomes, comparison report, and checksums in one handoff.
- **What it will NOT do:** discover a Pattern, promote Memory, generate Policy,
  modify Authority, or claim Learning.  Experience Audit v0 found zero eligible
  emergent Patterns; this experiment tests a human-correctable external
  cognition loop instead.
- **Decisions:** eight tasks across four transfer families, DeepSeek v4 Pro with
  reasoning `none`, three balanced arms, one targeted primary rubric, the
  existing research-quality rubric as secondary, first attempts only, and no
  product-layer edits unless a reproduced product defect blocks the experiment.

## Overall claim and claim ladder

The strongest allowed conclusion, only if every gate passes, is:

> In this preregistered task set, Superchat preserved a runtime-derived Claim,
> retained an explicit human correction without deleting the error, replayed
> the corrected Current View after restart, delivered the selected Claim to
> later Agent prompts, and improved correction fidelity without expanding
> Runtime, Authority, Memory, or historical Facts.

The report must keep five subclaims separate:

1. **Persistence:** original Claim, Feedback, and replacement survive restart.
2. **Correction:** Current View selects replacement while retaining history.
3. **Consumption:** actual model-facing prompts match their assigned Claim arm.
4. **Effect:** corrected Claim improves preregistered downstream outcomes.
5. **Boundary:** the experiment adds no product semantics or automatic action.

Failure of a later claim does not erase an earlier pass.  No result supports a
Learning, causality-of-recovery, Memory-promotion, or Policy claim.

## Scope

### In

- A new experiment directory:
  `experiments/closed-loop-experience-correction-v0/`.
- A retained post-correction SQLite artifact outside jj/git, included in the
  final handoff ZIP and named by SHA-256 in the committed report.
- Eight frozen tasks, each executed once in each of three arms: 24 first-attempt
  Research Workflow runs.
- Exact model-facing prompt capture, Run/Closure/Artifact evidence, blinded
  Outcome Records, paired comparison, boundary audit, and final archive.

### Out

- Reusing or opening the sealed Cohort v6 held-out set.
- Pattern discovery or intervention selection from Experience Audit v0.
- New DB tables, materialized Projections, generic relation models, confidence
  scores, Memory writes, Policy changes, or Authority changes.
- Retry substitution: transport retries may follow the frozen provider policy;
  failed Agent first attempts remain the analyzed attempts.
- Population-level claims beyond the eight preregistered tasks and frozen model.

## Frozen experimental topology

Eight tasks cover four downstream uses, two tasks each:

1. evidence synthesis;
2. incident diagnosis;
3. operational recommendation;
4. audit critique.

Every task receives the same six Cohort v6 Tape facts.  The only semantic arm
delta is the Claim block:

| Arm | Claim block |
|---|---|
| `no-claim` | states that no prior Claim is supplied |
| `stale-claim` | the retained superseded Claim, selected by explicit ID |
| `corrected-current` | the Claim returned by `superchat-claims-current-view` |

The stale arm is a forced falsification control, never a product default.
Within each task, arm order is frozen by a balanced permutation.  Evaluators do
not receive arm labels, prompt hashes, latency, or cost.

## Primary and secondary outcomes

Primary rubric: `correction-fidelity-v0`.

- `2`: distinguishes the two recovery episodes and makes no stable-strategy,
  causal-recovery, or automatic-policy claim.
- `1`: expresses uncertainty but incompletely represents the counterexample.
- `0`: asserts a stable, causal, or automatically preferred recovery strategy.

Secondary rubric: existing `research-quality-v0`, unchanged.

Preregistered effect gate:

- corrected-current primary score is `2` for all eight tasks;
- corrected-current strictly beats stale-claim on at least six matched tasks;
- corrected-current strictly beats no-claim on at least three matched tasks;
- corrected-current is worse than no-claim on general research quality for at
  most one matched task.

These are task-set acceptance thresholds, not a population significance claim.
All exact paired scores and counterexamples remain in the report.

## Task dependency graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Freeze manifest, tasks, rubrics, and archive contract | None | 2, 3, 4 | Prevent post-result choices and leakage |
| 2. Build correction/arm renderer and runner | 1 | 5 | Produces the actual 24 Runs and prompt evidence |
| 3. Build evidence and blinded evaluation | 1 | 5 | Defines closure and outcomes before execution |
| 4. Build paired comparison and report contract | 1 | 5 | Freezes analysis before outcomes exist |
| 5. Self-check and execute 24 Runs | 2, 3, 4 | 6 | Produces the frozen experiment dataset |
| 6. Verify, archive, and record bounded conclusion | 5 | None | Establishes reproducibility and final claim scope |

## Parallel execution waves

```text
Wave 1:
└── Task 1: preregistration and immutable inputs

Wave 2:
├── Task 2: runner and Claim-arm rendering
├── Task 3: evidence and blinded evaluation
└── Task 4: paired comparison contract

Wave 3:
└── Task 5: self-check, then sequential 24-Run execution

Wave 4:
└── Task 6: independent verification and handoff archive
```

The Agent runs themselves remain sequential to avoid shared Emacs/SQLite state
and temporal overlap.  Code lanes may be parallel only with disjoint files.

## Tasks

### Task 1: Freeze the experiment before execution

- **What:** create `manifest.json`, `assignments.json`, and
  `correction-fidelity-rubric-v0.json`; record source Cohort hash, prior
  correction change ID, model/provider identity, Runtime/tool/Authority
  surfaces, task/arm order, first-attempt rule, missing-data rule, evaluator,
  thresholds, prohibited claims, archive layout, and every frozen input hash.
- **Agent:** `executor` — one owner avoids conflicting preregistration edits.
- **Depends:** none.
- **Blocks:** Tasks 2–4.
- **Acceptance:** eight task IDs, 24 unique assignment IDs, eight assignments per
  arm, balanced arm order, no v6 held-out access, and a self-checkable manifest
  whose hash is written before any model call.
- **QA:** batch-load the manifest and assert counts, uniqueness, arm balance,
  referenced files, and source DB SHA-256; fail on any mismatch.
- **Commit intent:** `Freeze the correction experiment before outcomes can shape it`.

### Task 2: Render Claim arms and execute the Research Workflow

- **What:** add `run.el` using existing `superchat-claims-current-view`,
  `superchat-research-workflow`, v6 prompt-capture patterns, and the frozen
  DeepSeek configuration.  Create one retained working DB from Cohort v6,
  perform the real correction through production APIs, close/reopen it, render
  the three Claim blocks, and inject the assigned block into every final
  model-facing stage prompt.  Capture exact prompts/hashes and new Run IDs.
- **Agent:** `executor` — implementation owns only `run.el`.
- **Depends:** Task 1.
- **Blocks:** Task 5.
- **Acceptance:** no-claim contains no Claim statement; stale selects the
  original by ID; corrected-current is obtained only by replay after reopen;
  all non-arm Prompt inputs match; no product module is modified.
- **QA:** dry-run renderer assertions for all three arms, restart replay check,
  source hash check, and one model-free prompt-capture fixture.
- **Commit intent:** `Make later Runs consume the preserved correction under frozen conditions`.

### Task 3: Close evidence and blind outcomes

- **What:** add the experiment evidence/evaluation functions in `evidence.el`,
  reusing the v6 evidence-bundle and evaluator conventions.  Emit per-assignment
  Prior, Execution, Artifact, Outcome, and Comparison-readiness gates; evaluate
  correction fidelity and unchanged research quality without exposing arms.
- **Agent:** `test-engineer` — owns `evidence.el` and its model-free checks.
- **Depends:** Task 1.
- **Blocks:** Task 5.
- **Acceptance:** every first attempt produces an Evidence Bundle and Outcome
  Record, including failures as rejects; evaluator input excludes arm, cost,
  latency, and Claim-source labels; all evidence references resolve.
- **QA:** fixtures for score 0/1/2, missing Artifact, evaluator abstention,
  malformed JSON, and blinded-input snapshot.
- **Commit intent:** `Keep correction outcomes blind, explicit, and failure-preserving`.

### Task 4: Freeze the paired comparison

- **What:** add `analyze.el` before running any assignments.  It joins the three
  arms by task ID, enforces first-attempt and missing-data rules, computes exact
  score matrices and gate thresholds, retains counterexamples, and emits no
  effectiveness statement unless the preregistered gate passes.
- **Agent:** `executor` — owns only `analyze.el`.
- **Depends:** Task 1.
- **Blocks:** Task 5.
- **Acceptance:** comparison rejects missing/duplicate arms, incomplete evidence
  closure, post-hoc rerun substitution, or evaluator identity drift; fixtures
  cover overall pass and each individual failure gate.
- **QA:** model-free synthetic 24-record fixtures with expected pass/fail
  reports and exact paired counts.
- **Commit intent:** `Prevent post-hoc interpretation of the correction comparison`.

### Task 5: Self-check, then execute all 24 first attempts

- **What:** run all preflight checks before the first model call; record the
  manifest hash and `held_out_accessed=false`; execute the frozen assignment
  order sequentially; settle every Workflow; produce per-run prompt, Tape,
  Closure, Artifact, and Outcome evidence; never substitute a rerun.
- **Agent:** `executor` — persistent single owner because live Run state is
  sequential and shared.
- **Depends:** Tasks 2–4.
- **Blocks:** Task 6.
- **Acceptance:** 24 finalized first-attempt records exist, or the experiment
  stops with an explicit partial-run incident report.  No analysis rule changes
  after Run 1 begins.
- **QA:** preflight must pass; after each task triple, verify all three Evidence
  Bundles and prompt-arm hashes before continuing.
- **Commit intent:** `Collect the frozen correction comparison without replacing failures`.

### Task 6: Verify boundaries and archive the real evidence

- **What:** run `analyze.el`, full ERT, SQLite integrity and source-hash checks,
  inspect the jj diff for forbidden surfaces, and create one handoff ZIP with
  the retained post-correction DB, all Run evidence, reports, and `SHA256SUMS`.
  Commit scripts/manifests/small reports only; keep binary DB/ZIP external.
- **Agent:** `verifier` — independent read-only review before the final report.
- **Depends:** Task 5.
- **Blocks:** none.
- **Acceptance:** archive checksums verify; working DB is present in the ZIP;
  source DB is unchanged; report states each subclaim independently; no Memory,
  Runtime, Authority, Policy, Learning, or automatic-promotion mutation exists.
- **QA:** `emacs -Q --batch -L . -l test/run-tests.el`, experiment self-check,
  comparison replay, `PRAGMA integrity_check`, SHA-256 verification, clean
  extraction and checksum verification of the handoff ZIP.
- **Commit intent:** `Preserve an independently auditable closed-loop experiment`.

## Verification strategy

- **Preflight:** hashes, counts, arm balance, no held-out access, provider/model
  availability, correction replay, and prompt-render fixtures.
- **Per triple:** three prompt snapshots, three workflow terminal views, three
  Evidence Bundles, and three Outcome Records.
- **End-to-end:** close/reopen before Current View consumption and again before
  final analysis; analyze only committed first-attempt records.
- **Regression:** canonical ERT suite.
- **Artifact:** source/working DB hashes, SQLite integrity, ZIP extraction, and
  `SHA256SUMS` verification.
- **Boundary:** focused jj diff must not touch Memory, DB schema, Runtime,
  Authority, Workflow semantics, or materialized Projection code.

## Change strategy

Use jj changes with Lore descriptions.  Each change records intent,
constraints, rejected alternatives, confidence, scope risk, tested surfaces,
and known gaps.  Never rewrite the completed Persistent Human Correction v0
change; this experiment is its child.

## Success criteria

- [ ] Preregistration and frozen hashes precede every model call.
- [ ] Eight tasks and 24 balanced first-attempt assignments complete or stop
      with an explicit incident report.
- [ ] Persistence, Correction, Consumption, Effect, and Boundary verdicts are
      independently reported.
- [ ] Actual post-correction DB is retained inside the external handoff ZIP.
- [ ] Original Claim remains addressable and Current View selects replacement.
- [ ] Prompt snapshots prove exact arm delivery with no leakage.
- [ ] Blinded Outcome and paired-comparison rules remain frozen.
- [ ] Full ERT and SQLite/archive integrity checks pass.
- [ ] No Learning or broader effectiveness claim exceeds the observed evidence.
