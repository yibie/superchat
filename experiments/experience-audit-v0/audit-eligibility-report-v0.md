# Experience Audit v0 — Audit Eligibility Report

Generated: `2026-08-05T17:43:27+0800`

Dataset: `experience-cohort-deepseek-v6`  
Dataset lock: `e815b9c2953daee73de64f79a15f854d07b34475e2216c39b078d6c26d0583ae`

## Verdict

**Limited read-only audit eligible.**

The frozen cohort is eligible for constrained descriptive inspection. It is
not eligible for emergent classification, causal association, candidate
promotion, held-out execution, or a learning claim.

Pattern Observation remains `not_started`.

| Claim / activity | Verdict |
|---|---|
| Dataset integrity | pass |
| Descriptive Observation | eligible with lane restrictions |
| Explicit forced classification | eligible |
| Suggested classification | partial |
| Emergent classification | blocked |
| Pattern-specific causal association | blocked |
| Promotion | prohibited |

## Analysis denominator

The dataset has 20 Assignments, 60 Research stage Runs, 20 evaluator Runs,
60 Artifacts and 20 isolated Workspaces. The 60 stage Runs are nested in the
20 Assignments and are not independent samples.

- Calibration: 4, excluded from occurrence counts.
- Discovery: 12, the only occurrence-eligible split.
- Replicate: 4, excluded from occurrence thresholds.
- Held-out: sealed and unused.

## Designer Prior

Observed Prior includes 60 actual model-facing Prompt snapshots, 60 System
Prompt snapshots, the frozen model/provider settings, Workflow topology,
deliverable guard, seven-tool name surface, Authority events, source hashes,
task assignments and blind evaluator protocol.

The following behaviors are explicitly forced and must be excluded:

- Collector -> Analyst -> Reviewer order;
- search/read of official or primary sources;
- downstream inspection of prior delivered Artifacts;
- one `workspace_publish` before each stage completes;
- the seven-tool allowlist and frozen Outcome evaluation.

Exact serialized provider-facing Tool schemas were not snapshotted directly.
Tool names and relevant source hashes exist, but behavior potentially suggested
by Tool descriptions cannot be classified `emergent` in v6. The provider also
does not expose an immutable internal model build beyond the observed provider
name and `deepseek-v4-pro` model ID.

## Artifact Lineage eligibility

Artifact producer and explicit same-Workspace consumer lineage are observable:

- 60 unique producer Artifacts in 20 Workspaces;
- 62 explicit `workspace_read` requests;
- 61 completed reads and one failed read;
- 61 unique Artifact/consumer-Run edges;
- zero unresolved Artifact references;
- zero cross-Workspace consumer edges.

Discovery contributes 37 completed explicit consumer edges, but all remain
inside their source Assignment. Standard stage handoff is forced by Prompt and
Workflow Prior.

Therefore this lane may report lineage coverage and the observed absence of
cross-task reuse. It cannot test whether Artifact reuse reduces search or
improves quality.

## Recovery eligibility

The Tape contains four `tool.failed` events, but only two belong to the
occurrence-eligible Discovery split:

| Assignment | Failed tool | Next tool | Durable description |
|---|---|---|---|
| `disc-systems-02` | `agent_reach_read` | `agent_reach_read` | same-tool follow-up |
| `disc-security-03` | `agent_reach_read` | `agent_reach_search` | different-tool follow-up |

One Calibration episode and one Replicate episode remain context only. The
dataset has no structured failure class, durable error payload shown to the
model, explicit retry/fallback link, or failed-recovery comparison.

This lane may produce an episode inventory. Evidence is insufficient for a
stable recovery Pattern, its Prior classification is `unknown`, and causality
is prohibited.

## Boundary Transition eligibility

The only durable Workspace transition type is `workspace.artifact.added`:

- 60 added events;
- zero changed events;
- zero removed events;
- 60 unique Artifact IDs;
- zero same-Artifact multi-revision sequences.

Stage order and publish-before-completion are forced. Three role-specific
Artifacts cannot be relabeled as draft -> revision -> final. This lane may
report forced exclusions and observed absence, but no natural boundary
candidate is established.

## Outcome boundary

All 20 Outcome Records have Outcome Closure pass: 15 accept, 4 revise and one
reject. These are Assignment-level evaluations. No candidate Pattern has a
preregistered baseline, intervention or Comparison Closure.

Outcome association is therefore `descriptive-only`; causal status is
`not-identifiable`.

## Missingness and next action

The eligibility gaps require no Runtime modification. They must not be repaired
after the fact:

- exact serialized Tool schemas: partial Prior;
- recovery failure class and fallback relation: missing;
- cross-task Artifact consumer edge: observed absence, not missing data;
- Pattern comparison: not applicable until an eligible candidate exists.

The next allowed action is review of this report, followed by the three
preregistered read-only audits. Each lane may legally return `forced`,
`unknown`, `observed-absence` or `insufficient-evidence` without creating a
Pattern Observation.

Held-out remains sealed. Effectiveness is not evaluated. Learning and
promotion claims remain prohibited.
