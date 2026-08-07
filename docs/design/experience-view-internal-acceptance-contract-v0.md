# Experience View Internal Acceptance Contract v0

Status: frozen; not executed  
Frozen: 2026-08-11  
Decision controlled: whether
`superchat-runtime-recovery-intervention-enabled` may change from `nil` to `t`

## Boundary

This contract freezes one internal product acceptance for the First Recovery
Action after a cross-Run Workspace Artifact revision failure.  It authorizes no
provider call by itself.  The later `internal-acceptance` ticket may execute it
once, with the limits below.

Passing proves only the declared default Recovery Intervention behavior for
this Acceptance Reference Cell.  It does not establish a Provider/Model/Profile
support matrix, a runtime allowlist, or successful completion of the whole
recovery workflow.

The old V2a experiment contributes method only.  Its assignments and 32 model
requests are consumed evidence and cannot satisfy or be rerun for this gate.

## Frozen reference cell

### Product source

The launch preflight must reject any mismatch in these SHA-256 digests:

| File | SHA-256 |
|---|---|
| `superchat.el` | `20807812e04e795946b9ef872885fa3b446a473597a401a74f532c7c04b9ead0` |
| `superchat-runtime.el` | `4f48e7fe43a469ec1aee2739524fba2b4d1daff14f6e4cb06ce3ca44d8dd556a` |
| `superchat-llm.el` | `71614821b7cf74ef6b047d841403b546fd29f20f28ca783f8e4433d57a79b462` |
| `superchat-authority.el` | `428a645ec3986c6fe0ac64dc4b2634ce0fa6f7c00a05378cc672efbdbab52937` |
| `superchat-workspace.el` | `c7fd05abc3d38d657daf3b94f3ba1ef54c6be6744df293601d436de5d0459634` |
| `superchat-experience.el` | `335305878437903fcfa36cd4e6cd64d03a3ff3a1d11d1751214a82ac56128531` |
| `superchat-closure.el` | `8e6e4bad3ce76b759794da84ea8c03d2f43d7bf4c165350fd92c4b99f086d198` |
| `superchat-tape.el` | `68b0d592c9c797f10e94b81e09155f515a9c2c64bd40038bc566ba9ec68fcf86` |
| `superchat-projection.el` | `4597d6623de642e12d7dc210a4a0c3fbbe650e96e9a7063b24d009073430867f` |
| `superchat-preset.el` | `09458c6356e30199e11ac0cd779e7be9d14ea59db73cf80de5b5a3bd032e878f` |
| `superchat-tools.el` | `45e6d459d8bcd168e1173f0e0092ba5e214e23bd2ca68b41fa582490cd8ff779` |
| `superchat-dispatcher.el` | `c8b6a8e91842f1d00131cf68f3d1238f194f80404298a7f111634319c922db48` |
| `superchat-prompt-hooks.el` | `362fec26a1dc776a095f687395f76e26c53b6e0d39f6fe3c18cdf1dfef8d4f0b` |
| `superchat-agent-loop.el` | `f2aae4b32cee04ca049e43286dd2cfeca316b952b2b8ce734b58418ea70aaab8` |
| `superchat-db.el` | `f4a76b0239f0041f7e895e3622df820ac1aaf85bec5aedfca7b59077fe0b1536` |

The launch manifest must also record the mutable `jj` working-copy identity,
but the file digests above, not that identity or package version, seal the
product code.

### Provider and Model

- Emacs: GNU Emacs `31.0.91`.
- Transport: `llm-ollama` from llm.el `0.31.1`, package commit
  `34ac61091994c10bda45eed54433a1a87ca26af1`.
- Provider runtime: `/Applications/Ollama.app` `0.32.6`.
- Endpoint: `http://localhost:11434`.
- Explicit request target: `qwen3.5:9b`; backend defaults and `latest` aliases
  are forbidden.
- Model manifest SHA-256:
  `6488c96fa5faab64bb65cbd30d4289e20e6130ef535a93ef9a49f42eda893ea7`.
- Model layer digest:
  `sha256:dec52a44569a2a25341c4e4d3fee25846eed4f6f0b936278e3a3c900bb99d37c`.
- A fresh backend has no non-standard parameters.  `reasoning=none` is the
  only reasoning control.

Preflight must re-read and record the non-secret provider class, adapter file
digest, llm package version/commit, Ollama binary/version, endpoint, provider
capabilities, model ID, manifest digest, and layer digest before any call.
Secrets and the full backend object must never enter evidence.

### Built-in `code` Agent Profile

- `name`, `profile-id`, and `capability`: `code`
- `description`: `Coding in the current Emacs context`
- `type`: `agent`
- `source`: `builtin`
- `version`: `1.0`
- body:

  ```text
  Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools.
  ```

- ordered tools:
  `read-file`, `list-files`, `search-text`, `read_buffer`, `EditBuffer`,
  `ReplaceBuffer`, `eshell-command`, `workspace_read`, `workspace_write`,
  `workspace_info`.
- Profile fields inherited without override: model, temperature, max tokens,
  timeout, and destructive confirmation.  Profile reasoning is `inherit` and
  its effective value is fixed below.

The launch manifest must serialize every Profile accessor and the complete,
ordered provider-visible schema for all ten tools before calls begin.  It must
record both each schema and its exact JSON SHA-256; duplicate, synthesized,
dynamic, asynchronous, or unrecordable tools fail preflight.

### Effective prompt and execution settings

The reference cell is English, single-agent, with no active extension.  The
expected exact system prompt is this JSON string:

```json
"Work as a coding assistant. Inspect before editing, keep changes scoped, and verify them with the available build or test tools.\n\n\nYou can batch INDEPENDENT read-only tool calls (read-file, list-files, search-text, find-files, lsp-references, lsp-hover, read_buffer) into a single turn.\nTools that depend on each other's output must wait for the previous result."
```

The launch manifest must record the actual final prompt bytes and hash, ordered
system-prompt hook symbols, topology, language, active extensions, exact
prepared envelope and hash, and all materialized provider request options.
Any mismatch fails preflight.

Fixed execution settings:

- `reasoning=none`; temperature, max tokens, response format, and tool choice
  have no request-local override and their materialized values are recorded.
- provider streaming capability and actual per-attempt streaming value are
  recorded; neither is inferred from the UI setting.
- effective `max-tool-calls=2`; destructive confirmation remains enabled.
- Authority policy is ordinary default with version `authority-v1/default`.
  The exact policy, effect/rewrite/filter hook lists, approval function
  identity, and attendance are recorded.  No write is approved.
- `max-tool-rounds=2`, `unknown-tool-retries=0`, round-limit action `stop`.
- no-data timeout `60` seconds, timeout multiplier `1.0`.
- at most two real provider requests per cell and fourteen for the cohort.
- cost boundary: `0 CNY`; only the frozen local endpoint may be used.

## Shared Experience facts

Every cell uses the exact Context text:

```text
Cross-Run workspace_read failed with superchat-workspace-artifact-revision-error.
```

Every cell that has a Candidate uses the exact Candidate text:

```text
Call workspace_info once, take the revision reported for the same artifact_id, then call workspace_read with that artifact_id and that exact expected_revision; never reuse or invent a revision.
```

Each cell gets a fresh scratch DB and a fresh Workspace.  Setup creates a
closed producer Run and its Artifact.  Stale cells record a real old revision,
then change the Artifact before the consumer Run.  Each DB contains exactly one
Context for this Recovery Class.

The consumer prompt is one of these exact templates after substituting the
recorded IDs:

```text
Call workspace_read exactly once for Artifact <artifact-id> without expected_revision. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action.
```

```text
Call workspace_read exactly once for Artifact <artifact-id> with expected_revision <old-revision>. If it fails, make exactly one recovery Action using the available tools. Do not answer before that Action.
```

Neither prompt names `workspace_info`, Experience, the Candidate, or the
acceptance rubric.

## Frozen seven-cell cohort

Cells run once in this order:

| Cell | Trigger | Experience setup | Expected intervention |
|---|---|---|---|
| `P-missing` | missing revision | one active promoted Candidate with one valid `applies-under` | exactly one |
| `P-stale` | stale revision | one active promoted Candidate with one valid `applies-under` | exactly one |
| `N-absent` | missing revision | no eligible Candidate | zero |
| `N-fails` | stale revision | active promoted Candidate with `fails-under` and no `applies-under` | zero |
| `N-conflict` | missing revision | same active promoted Candidate has `applies-under` and `fails-under` | zero |
| `N-multiple` | stale revision | two active promoted applicable Candidates | zero |
| `N-revoked` | missing revision | Candidate was promoted and applicable, then its Promotion was revoked | zero |

The setup manifest records every Session, Run, Artifact, Candidate, Context,
Promotion, Relation, and disposition event reference before the consumer Run.

## Observable verdicts

### Positive cells

Both positive cells must uniquely prove this ordered chain:

1. The first request produces one cross-Run `workspace_read` Action and a
   `tool.failed` Fact whose `:tool-name` is `workspace_read` and whose
   `:error-type` is `superchat-workspace-artifact-revision-error`.
2. Exactly one `experience.intervention.recorded` is caused by that failure and
   names the expected Candidate, Promotion, Relation, and Context receipt.
3. Attempt 2 has one exact `decision.snapshot.recorded`; its prepared envelope
   contains that Experience prompt and receipt.
4. The earliest Action after the intervention is one `tool.requested` for
   `workspace_info` with an empty argument object.  A direct response, another
   tool, parameters, or a later correction fails the cell.
5. That Action uniquely joins by execution ID to one `tool.completed`.  This is
   the acceptance Outcome.  A later pre-registered round stop may terminate the
   Run but cannot reinterpret this tool Outcome.

The gate ends at the First Recovery Action.  It does not add another provider
request to observe the later exact `workspace_read`.

### Negative cells

Every negative cell must have zero `experience.intervention.recorded`.  Its
exact prepared envelope must contain none of the blocked Candidate text or IDs,
Promotion/Relation receipt, Experience prompt, or Experience receipt bytes.

A negative model may independently call no-argument `workspace_info` or ask the
user because the ordinary tool schema already exposes that safe path.  It must
not guess a revision, reuse the stale revision, call `workspace_write`, or
claim/reference the blocked reviewed Candidate.  Any such misuse fails the
cell.

## Evidence and execution discipline

The `internal-acceptance` runner must seal a launch manifest before the first
provider call.  It must hard-count actual transport invocations in addition to
Tape snapshots.  For each cell it retains the scratch DB, exact prepared
envelopes, event references, Actions, Outcomes, transport count, and verdict.

Required joins are unique or the cell fails.  Missing, duplicate, ambiguous,
late, timed-out, provider-error, setup-error, or unclassifiable evidence fails
the cell.  A later acceptable Action cannot replace the earliest Action.
Snapshot count and actual transport count must agree.

Assignment is once-only: no retry, rerun, replacement, prompt repair, backend
substitution, or manual completion is allowed.  A provider or infrastructure
failure consumes this contract revision.  Another attempt requires a new,
explicitly frozen contract revision.

## Ship or keep off

Default enablement is earned only if:

- all seven cells pass their declared verdicts;
- actual provider requests are at most two per cell and exactly match the
  retained snapshot evidence;
- the cohort uses no retry or replacement; and
- the canonical ERT suite finishes with zero unexpected results against the
  sealed product source.

Only then may the one-line product change set
`superchat-runtime-recovery-intervention-enabled` to `t` by default.  Any
failure leaves it `nil`, retains all evidence, and names the first product or
contract repair required.  Test completion, a written plan, or partial cohort
success cannot substitute for this rule.
