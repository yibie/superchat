# Superchat DeepSeek Experience Cohort v6 — Handoff

Completed and archived on 2026-08-05. The dedicated batch Emacs process exited
normally after finalizing all 20 assignments; no experiment process remains
running.

## Frozen scope

- 20 sequential Research Workflow assignments: 4 Calibration, 12 Discovery,
  and 4 preregistered Replicates.
- DeepSeek provider `deepseek v4 pro`, model `deepseek-v4-pro`, reasoning
  `none`, and `llm` 0.31.1.
- Frozen Collector -> Analyst -> Reviewer topology and exact seven-tool
  surface.
- Blind Outcome evaluation used local `qwen3.6:latest` and the frozen
  `research-quality-v0` rubric.
- Historical attempts and Artifacts were not imported.
- Held-out tasks remained sealed and were never authorized or launched.

## Final result

The cohort ran from `2026-08-05T11:29:29+0800` through
`2026-08-05T14:15:56+0800` (2h 46m 27s).

- 20/20 assignments finalized.
- 20/20 Evidence Closure pass.
- 20/20 Workflow Contract satisfied.
- Calibration Gate pass: 4/4.
- 20/20 Outcome Closure pass.
- Outcome results: 15 accept, 4 revise, 1 reject.
- 60/60 stage Artifacts have Artifact Closure pass and Delivery decision
  `deliver`.
- 60/60 archived Artifact files match their recorded revision SHA-256.

The single reject is valid experimental data: Outcome Closure proves that the
frozen evaluation completed, not that the evaluator accepted the Artifact.

## Runtime and evidence integrity

- SQLite `PRAGMA integrity_check`: `ok`.
- Tape events: 3,315.
- Run lifecycle: 80 started, 80 completed, 0 failed, 0 cancelled.
- Tool lifecycle: 841 completed and 4 failed events.
- Workspace: 60 `workspace.artifact.added` events.
- Missing Run identity on checked Runtime events: 0.
- Missing Actor identity on checked Runtime events: 0.
- Invalid tool identities in the 20 evidence bundles: 0.
- Non-valid terminal traces in the 20 evidence bundles: 0.

Four tool failures remain explicit in Tape and in their evidence bundles:

- `cal-science-01`, Analyst: 1.
- `disc-systems-02`, Reviewer: 1.
- `disc-security-03`, Analyst: 1.
- `rep-disc-standards-01-01`, Reviewer: 1.

Each occurred inside an otherwise valid, durably completed trace. They were
not deleted, rewritten, or used as automatic evidence-gate failures.

## Archived outputs

- `manifest.json`, `assignments.json`, and `prompt-snapshots.json`: frozen
  experiment prior.
- `launch-record.json`: all 20 new execution identities.
- `runs/*/evidence-bundle.json`: 20 machine-readable evidence bundles.
- `runs/*/outcome-record.json`: 20 machine-readable Outcome records.
- `cohort-report.json` and `cohort-report.md`: generated cohort summary.
- `verification-report.json`: independent completion and integrity checks.
- `runtime-snapshot.db`: standalone SQLite backup, SHA-256
  `dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c`.
- `runtime-artifacts/`: exact copy of all 60 durable Artifact files.

The frozen Markdown report generator retained the display title “DeepSeek
Experience Cohort v3.” This is a display-only naming residue. The authoritative
identity is `experience-cohort-deepseek-v6`; the generated evidence was not
rewritten after execution.

## Claim boundary

This run proves that the frozen Runtime -> Evidence path produced a complete,
auditable 20-Run cohort on the tested surfaces. It does not prove an
intervention effect, an emergent pattern, or learning.

- Held-out: sealed and unused.
- Pattern Observation: not started.
- Effectiveness claim: not evaluated.
- Learning claim: prohibited.

The next authorized activity is read-only evidence inspection. Any Pattern
Observation must classify Designer Prior and preserve counterexamples before
an intervention or held-out experiment is proposed.
