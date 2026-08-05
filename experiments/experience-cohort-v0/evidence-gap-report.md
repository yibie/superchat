# Experience Cohort v0 — Evidence Gap Report

Date: 2026-08-03  
Manifest: `experience-cohort-v0-20260803`  
Verdict: `design closure = pass`; `execution = blocked`; `runs started = 0`

## Frozen design

- 24 unique tasks across four families.
- Calibration / discovery / held-out split: 4 / 12 / 8.
- Held-out task bank is sealed after freeze validation.
- Designer Prior includes the forced Collector → Analyst → Reviewer topology, Research Profile, task-specific prompt priors, tool surface, and Authority surface.
- Output is limited to `Pattern Observation`; causal claims and promotion are prohibited.

## Blocking evidence gaps

| Evidence | Status | Impact | Minimum resolution |
|---|---|---|---|
| Real provider, model, version and generation settings | missing | Real Prior Closure cannot be reproduced | Choose them and freeze a new manifest revision |
| External source-retrieval tools and schemas | missing | The tasks require primary or official sources but cannot retrieve them reproducibly | Configure and freeze the tool surface and Authority effects |
| Blind evaluator identity, version and instructions | missing | Real Outcome Closure cannot be reproduced | Choose a human or model evaluator and freeze the blinding procedure |

All three gaps are experimental configuration gaps. None currently justifies a Tape event, Projection, Contract, database table, or other Runtime change.

## Stop condition

The cohort did not start. After the three inputs are frozen, the next allowed execution is the four-task calibration set—not discovery, held-out evaluation, Pattern Mining, or promotion.
