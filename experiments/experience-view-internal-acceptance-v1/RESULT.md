# Experience View Internal Acceptance v1 — Result

Status: **keep off**  
Executed: 2026-08-11 13:43 CST  
Provider requests: **0 / 14**

The v1 bootstrap constructed the pinned llm.el/Ollama transport and checked the
frozen product, adapter, model-manifest, and model-layer hashes. But it did
**not** pass the contract's launch boundary: its retained manifest records the
versions of all four private dependencies as `nil`. The contract requires
resolved dependency names, versions, paths, and source digests. It also omits
the required redacted role for the private llm source. It records neither the
`jj` working-copy identity nor the required schema JSON hashes and complete
Profile/request-setting fields. The contract says an unrecordable dependency
must stop the run before a provider request. The runner failed to enforce that
rule.

Independently, the incorrectly admitted seven cells all stopped before their
first provider request. Each retained a `run.failed` / audit-incomplete record
and reports:

```text
Superchat could not record required decision evidence: "Exact evidence audit failed"
```

## Proven cause

The built-in `code` profile's frozen ten-tool surface includes `workspace_write`.
That tool has the boolean `append` argument. In the same frozen source,
`superchat--llm-validate-evidence-tools` in `superchat-llm.el` accepts only
`string`, `integer`, and `number` argument types for
`epistemic-learning-v0`. It therefore rejects the profile before a prepared
decision snapshot can be persisted, and Runtime correctly prevents the model
request.

This proves neither a successful nor an unsuccessful model First Recovery
Action. The v1 record is not an admissible acceptance result: the bootstrap
manifest is incomplete, and the reference cell cannot begin the required audit
trail. The default remains off:
`superchat-runtime-recovery-intervention-enabled` stays `nil`.

## Retained evidence

The launch manifest, seven isolated SQLite Tapes, per-cell result records, and
the cohort verdict are retained locally at:

`experiments/experience-view-internal-acceptance-v1/evidence-20260811T054341Z/`

## Required repairs before a new contract

1. Freeze a new contract and write a runner that records and rejects on every
   required launch field: package versions, redacted private-source role, `jj`
   identity, schema JSON/hashes, complete Profile, hooks, and materialized
   request settings.
2. Decide and test the exact-evidence contract for boolean tool arguments (the
   minimal affected case is `workspace_write.append`).

The second repair changes sealed product source. Freeze a new acceptance
contract only after both repairs; then run a new once-only cohort. v1 must not
be rerun.
