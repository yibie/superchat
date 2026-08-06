# Experiment status

Current evidence surfaces:

- `experience-cohort-deepseek-v6/` — frozen 20-Run dataset.
- `experience-audit-v0/` — read-only audit over v6.
- `experience-feedback-v0/` — real append-only human correction and restart
  replay over a temporary copy of the frozen v6 database.
- `research-outcome-rubric-v0.json` — frozen outcome rubric.

`experience-cohort-v0/` is the frozen runner dependency used by v6.
`experience-cohort-deepseek-v5/` retains the predecessor incident report and
Runtime snapshot referenced by v6. They are not active cohorts; their
transitive predecessor histories are in the archive below.

Binary companions are deliberately not versioned in Git.  Keep them in the
project's external artifact storage and restore them at the paths below before
rerunning an audit:

| Path | SHA-256 |
|---|---|
| `archive/superchat-experiment-history-through-2026-08-05.zip` | `2ef15226e366b86f730ca6786af736ccddc5500b9f9dfb90550bde4bacbd6caf` |
| `experience-cohort-deepseek-v5/runtime-snapshot.db` | `7d66b10c5f032377e4b6314ffc826656e6de64cfd64c2edb8d124af4c7b9b25f` |
| `experience-cohort-deepseek-v6/runtime-snapshot.db` | `dedd4538fb7d99a2c2d942bbee5c398e074f617dbb73b47af2516bcbc949266c` |

The ZIP contains superseded cohorts, canaries, calibration outputs, and
historical debug tests.  Its checksum is also retained in
`archive/SHA256SUMS`.
