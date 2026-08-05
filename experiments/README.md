# Experiment status

Current evidence surfaces:

- `experience-cohort-deepseek-v6/` — frozen 20-Run dataset.
- `experience-audit-v0/` — read-only audit over v6.
- `research-outcome-rubric-v0.json` — frozen outcome rubric.

`experience-cohort-v0/` is the frozen runner dependency used by v6.
`experience-cohort-deepseek-v5/` retains the predecessor incident report and
Runtime snapshot referenced by v6. They are not active cohorts; their
transitive predecessor histories are in the archive below.

Superseded cohorts, canaries, calibration outputs, and historical debug tests
are preserved in `archive/superchat-experiment-history-through-2026-08-05.zip`.
Verify it against `archive/SHA256SUMS` before extraction.
