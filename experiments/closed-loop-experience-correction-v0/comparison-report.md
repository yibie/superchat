# Closed-loop Experience Correction v0 comparison

- Status: `invalid_input_contract`
- Persistence verdict: `pass`
- Correction verdict: `pass`
- Consumption verdict: `invalid`
- Effect verdict: `invalid`
- Boundary verdict: `pass`

## Validity

- Classification: `experiment_input_contract_violation`
- The raw score matrix is retained for audit only and cannot support Consumption or Effect claims.

## Effect thresholds

- `corrected_primary_all_two`: `:json-false`
- `evidence_closure_pass`: `19`
- `evidence_closure_blocked`: `5`
- `corrected_beats_stale_actual`: `1`
- `corrected_beats_no_claim_actual`: `1`
- `quality_worse_than_no_claim_actual`: `1`

## Counterexamples

- `clf-evidence-synthesis-01` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-evidence-synthesis-01` `not-strictly-better-than-no-claim`: corrected `2`, comparison `2`
- `clf-evidence-synthesis-02` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-incident-diagnosis-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-01` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-01` `research-quality-worse-than-no-claim`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-02` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-incident-diagnosis-02` `not-strictly-better-than-stale`: corrected `0`, comparison `0`
- `clf-incident-diagnosis-02` `not-strictly-better-than-no-claim`: corrected `0`, comparison `0`
- `clf-operational-recommendation-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-operational-recommendation-01` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-operational-recommendation-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `0`
- `clf-operational-recommendation-02` `not-strictly-better-than-stale`: corrected `2`, comparison `2`
- `clf-operational-recommendation-02` `not-strictly-better-than-no-claim`: corrected `2`, comparison `2`
- `clf-audit-critique-01` `corrected-primary-not-2`: corrected `0`, comparison `2`
- `clf-audit-critique-01` `not-strictly-better-than-stale`: corrected `0`, comparison `2`
- `clf-audit-critique-01` `not-strictly-better-than-no-claim`: corrected `0`, comparison `0`
- `clf-audit-critique-02` `not-strictly-better-than-no-claim`: corrected `2`, comparison `2`

## Prohibited claims

- A completed launch count proves Learning.
- The correction proves a causal recovery mechanism.
- The correction authorizes Memory promotion.
- The correction authorizes Policy or Authority changes.
- The stale arm is a product default.
- Held-out Cohort v6 tasks were inspected or executed.
- The eight preregistered tasks support population-level effectiveness.

No consumption or effectiveness statement is allowed because the model-input contract was violated.
