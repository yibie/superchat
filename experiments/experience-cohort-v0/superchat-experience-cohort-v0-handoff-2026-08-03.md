# Superchat Experience Cohort v0 — Handoff

Stopped and archived on 2026-08-03 after writing the final evidence snapshot.
The dedicated `superchat-cohort-v0` Emacs daemon and its experiment child
processes were terminated; the shared Ollama service was not stopped.

## Frozen scope

- 20 Research Workflow starts: 4 calibration, 12 discovery, 4 replicate.
- 20 unique Collector Run IDs and 20 unique Workflow instance IDs.
- Held-out tasks remained sealed and were not launched.
- Failed and incomplete v1/v2 attempts remain in the evidence set.
- Agent Reach execution used the frozen Exa search and Jina Reader surfaces.

## Final snapshot

The final report was written at `2026-08-03T15:54:26+0800`.

- Collector Runtime status: 9 completed, 10 running-model, 1 audit-incomplete.
- Durable Collector terminal witnesses: 9.
- Collector indexes 3 and 9 reached `deliver -> advance`.
- Collector indexes 10, 11, 12, 13, 17, 18, and 19 completed without a
  deliverable Artifact and therefore remain `manual-review`.
- No complete Collector -> Analyst -> Reviewer Workflow was claimed.
- No effectiveness, Pattern, emergent-capability, or learning claim was made.

See `evidence-execution-inspection.md` for the readable report and
`evidence-execution-inspection.json` for the complete machine-readable rows.

## Integrity and verification

- Manifest v1/v2/v3 hashes and all seven v3 frozen input hashes matched.
- Held-out usage was `false`.
- SQLite `PRAGMA integrity_check` returned `ok` before packaging.
- `runtime/superchat-snapshot.db` is a consistent standalone SQLite backup of
  the stopped DB/WAL state: 476 Tape rows across 42 Sessions.
- Full ERT: 575 tests, 560 expected passes, 15 skipped, 0 unexpected.
- `git diff --check` passed.

## Runtime repair included in the archive

`superchat-subagent.el` now copies the accepted Runtime handle's Run and Turn
identity into the lexical context captured by asynchronous tool wrappers.
`test/test-subagent.el` contains the regression test
`test-subagent-runtime-identity-reaches-wrapped-tools`.
