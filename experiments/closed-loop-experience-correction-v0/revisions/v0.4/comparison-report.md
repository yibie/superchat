# Closed-loop Experience Correction v0 comparison

- Status: `effect_gate_fail`
- Persistence verdict: `pass`
- Correction verdict: `pass`
- Consumption verdict: `pass`
- Effect verdict: `fail`
- Boundary verdict: `pass`

## Effect thresholds

- `corrected_primary_all_two`: `:json-false`
- `evidence_closure_pass`: `23`
- `evidence_closure_blocked`: `1`
- `corrected_beats_stale_actual`: `0`
- `corrected_beats_no_claim_actual`: `1`
- `quality_worse_than_no_claim_actual`: `3`

## Counterexamples

- `clf-evidence-synthesis-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-evidence-synthesis-01` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-evidence-synthesis-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `0`
- `clf-evidence-synthesis-02` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-evidence-synthesis-02` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-evidence-synthesis-02` `not-strictly-better-than-no-claim`: corrected `0`, comparison `2`
- `clf-evidence-synthesis-02` `research-quality-worse-than-no-claim`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-01` `not-strictly-better-than-stale`: corrected `0`, comparison `0`
- `clf-incident-diagnosis-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `0`
- `clf-incident-diagnosis-02` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-operational-recommendation-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-operational-recommendation-01` `not-strictly-better-than-stale`: corrected `0`, comparison `0`
- `clf-operational-recommendation-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `2`
- `clf-operational-recommendation-01` `research-quality-worse-than-no-claim`: corrected `0`, comparison `2`
- `clf-operational-recommendation-02` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-operational-recommendation-02` `not-strictly-better-than-no-claim`: corrected `2`, comparison `2`
- `clf-audit-critique-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-audit-critique-01` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-audit-critique-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `2`
- `clf-audit-critique-01` `research-quality-worse-than-no-claim`: corrected `0`, comparison `2`
- `clf-audit-critique-02` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-audit-critique-02` `not-strictly-better-than-no-claim`: corrected `2`, comparison `2`

## Prohibited claims

- Learning occurred.
- The correction caused model recovery.
- The correction was promoted to Memory.
- The correction changed Policy or Authority.
- The result generalizes beyond the eight preregistered tasks.
- A rerun may replace a failed first attempt.

No effectiveness statement is allowed because the preregistered effect gate did not pass.
