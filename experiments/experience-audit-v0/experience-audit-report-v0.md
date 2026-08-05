# Experience Audit v0 — Read-only Audit Report

Generated: `2026-08-05T17:56:58+0800`

Dataset: `experience-cohort-deepseek-v6`  
Dataset lock: `e815b9c2953daee73de64f79a15f854d07b34475e2216c39b078d6c26d0583ae`

## Verdict

**Experience Audit v0 completed with zero eligible Pattern Observations.**

This is the preregistered zero-result, not a failed Audit. The frozen Cohort
contains reliable Runtime evidence, but the three allowed lanes did not expose
a repeated structure that is both outside Designer Prior and eligible for a
Learning-oriented intervention.

| Result | Value |
|---|---|
| Pattern Observations | 0 |
| Candidates | 0 |
| Candidate gate | not entered |
| Held-out | sealed and unused |
| Effectiveness | not evaluated |
| Promotion / Learning | prohibited |

The machine-readable results are in
[`experience-audit-report-v0.json`](experience-audit-report-v0.json) and
[`pattern-observation-report-v0.json`](pattern-observation-report-v0.json).

## Frozen boundary

The Audit checked all 52 files named by `dataset-lock.json` before and after
querying the snapshot; every hash matched. All five set commitments (Evidence,
Outcome, Artifact revision, archived Artifact bytes and Runtime Run IDs) also
matched. Only the 12 Discovery Assignments could increase an occurrence count.
Four Calibration and four Replicate Assignments remained context only, and no
held-out task was read or executed.

The interpreter performed read-only joins across the frozen SQLite Tape,
launch records, evidence bundles and Outcome Records. It wrote no Runtime
state and introduced no Runtime event, Projection, Contract, table or learning
module.

## A. Artifact Lineage Audit

Verdict: **no eligible Pattern Observation**.

- 60 producer Artifacts were present.
- 62 explicit `workspace_read` requests produced 61 completed reads and one
  failed read.
- The completed reads formed 61 unique Artifact/consumer-Run edges; all
  Artifact references resolved.
- There were zero cross-Assignment and zero cross-Workspace consumer edges.
- Discovery contributed 37 completed edges. Eleven Assignments had the three
  expected handoff edges; `disc-systems-03` had four.

The normal Collector-to-Analyst/Reviewer lineage is `forced`: the frozen
Prompts explicitly require downstream stages to inspect prior delivered
Artifacts. It cannot count as learned or emergent structure.

The one additional edge was a Collector self-read in `disc-systems-03`:

```text
event 1035  workspace.artifact.added  artifact-19cf0d68-19
event 1037  workspace_read requested  same producer/consumer Run
event 1039  workspace_read completed
```

It occurred once, stayed within the same Assignment and Workspace, and had no
comparable recurrence. It is retained as a non-pattern finding with Prior
classification `unknown`.

Canonical edge-set digests:

- all 61 edges: `00fa4ec9e41a44053c8abf362c4743732642c687d9a6936d9cd2eca24bb076ae`;
- 37 Discovery edges: `377395f1bbb3e65da8cf22d588f6756d81f9d0c83917417582caba4c1bc401a7`.

Observed absence: v6 contains no cross-task Artifact reuse. A completed read
also does not prove semantic use, usefulness, or Outcome contribution.

## B. Recovery Episode Audit

Verdict: **insufficient evidence; no Pattern Observation**.

The Tape contains four failure episodes, but only two are occurrence-eligible:

| Assignment | Split | Failure | First subsequent request | Terminal | Run terminal | Class |
|---|---|---:|---:|---:|---:|---|
| `cal-science-01` | Calibration | 374 `workspace_read` | 375 `workspace_read` | 377 completed | 414 completed | same-tool follow-up |
| `disc-systems-02` | Discovery | 961 `agent_reach_read` | 962 `agent_reach_read` | 964 completed | 982 completed | same-tool follow-up |
| `disc-security-03` | Discovery | 1577 `agent_reach_read` | 1578 `agent_reach_search` | 1580 completed | 1605 completed | different-tool follow-up |
| `rep-disc-standards-01-01` | Replicate | 3286 `agent_reach_read` | 3287 `agent_reach_read` | 3289 completed | 3308 completed | same-tool follow-up |

The two eligible episodes have different sequence classes, so neither repeats
across Discovery Assignments. The dataset also lacks a structured failure
class, the durable error payload presented to the model, an explicit
retry/fallback link and comparable failed-recovery exposure. Therefore the
correct Prior classification is `unknown`; `retry`, `fallback` and causal
recovery claims are not justified.

## C. Boundary Transition Audit

Verdict: **no eligible Pattern Observation**.

The only durable Workspace event type is `workspace.artifact.added`: 60 events
for 60 distinct Artifact IDs. There are no changed or removed events and no
Artifact ID with multiple revisions.

Workflow order, Artifact delivery to the next stage, required prior-Artifact
inspection, publish-before-completion, Authority execution order and Outcome
evaluation are all forced structures. The three different role Artifacts in an
Assignment cannot be relabeled as one Artifact's `draft -> revision -> final`
lifecycle. No non-forced repeated boundary transition remains after exclusion.

## Outcome boundary

All 20 Outcome Records have Outcome Closure pass: 15 `accept`, four `revise`
and one `reject`. The 12 Discovery Assignments contain nine `accept`, two
`revise` and one `reject`.

These labels are Assignment-level descriptive context only. Because no Pattern
was frozen and no Pattern-specific baseline/intervention exists, causal status
is `not-identifiable`; Outcome labels cannot rescue an ineligible Observation.

## Stop decision

All three preregistered lanes ended in `forced`, `observed-absence` or
`insufficient-evidence`. The preregistered stop condition is therefore met:

- do not create a Candidate Structure;
- do not design an intervention from Cohort v6;
- do not expand or repair the frozen dataset;
- do not access held-out;
- do not make effectiveness, promotion or Learning claims.

If this research question continues, it requires a new prospective cohort with
a separately frozen Designer Prior and evidence plan. Cohort v6 remains a valid
zero-observation dataset.
