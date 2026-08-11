# Experience View Internal Acceptance Contract v2.5

Status: frozen; one execution authorized; v2.4 consumed after N-conflict write attempt  
Frozen: 2026-08-11  
Canonical self-digest: c7ade8112e9120a77aff5bca67b49d88f7ef898bc26c292f726c6edad745da17  
Decision controlled: whether
`superchat-runtime-recovery-intervention-enabled` may change from `nil` to `t`

The canonical self-digest is the SHA-256 of this file after replacing the
64 hexadecimal characters on the `Canonical self-digest` line and the 64
characters in the assignment ID below with zeroes.  This avoids a circular
self-reference while keeping the frozen bytes auditable.

Final runner: `experiments/experience-view-internal-acceptance-v2/run.el`  
Final runner SHA-256: `1844695d1ca6657fb5ce9278b8225f0c644a16a9d674d1bb76aff43482d3feb5`  
Verifier: embedded in the final runner; deterministic accept/reject checks are
executed before the cohort verdict is written.

## Inheritance and replacements

This contract incorporates the boundary, frozen Provider/Model cell, built-in
`code` Agent Profile, exact prompt, shared Experience facts, seven-cell cohort,
observable verdicts, request ceiling, evidence discipline, and ship/keep-off
rule from
[`experience-view-internal-acceptance-contract-v0.md`](experience-view-internal-acceptance-contract-v0.md),
whose SHA-256 is
`85179efbb728fbda148cf8b95dc1b12d9263055fa9888a8c42d23bccfddfe2ab`.

It does **not** change the v0 ordered ten-tool surface.  In particular, the
provider-visible order remains:

```text
read-file, list-files, search-text, read_buffer, EditBuffer, ReplaceBuffer,
eshell-command, workspace_read, workspace_write, workspace_info
```

The following clauses replace v0/v1 only where they conflict:

1. product-source digests are re-frozen after the boolean provenance repair;
2. the v1 private `llm.el` bootstrap is retained, with the fail-closed manifest
   rules below;
3. exact-evidence tool schemas support `boolean` in addition to v0's already
   supported `string`, `integer`, and `number` types; and
4. one immutable assignment is claimed before launch and cannot be retried.

The rejected v0 and v1 assignments and their evidence are consumed.  They must
not be rerun, merged into v2, or counted toward a v2 verdict.

Revision v2.1 superseded the consumed v2 assignment whose contract digest was
`d80f2d2e4117110fd3a52e9bb123ce5d288e83927157a18845f3b16bb8d5e9db`.  That
assignment created its claim but failed after no-chat provider construction and
before the first cohort dispatch because the runner's two-argument evidence
writer replaced the preflight's one-argument writer.

Revision v2.2 supersedes the consumed v2.1 assignment whose contract digest was
`cc10942d8221b3d2abca7670aa1307061e404f911df82cdaf0f1f930497c415e`.  Its
preflight found that the frozen `ReplaceBuffer` schema digest had one transposed
hex character (`...be...` instead of the actual `...ea...`).  v2.2 corrects that
single lock value and does not authorize a retry of v2.1.

Revision v2.3 supersedes the consumed v2.2 assignment.  That cohort made seven
real `/api/chat` requests with no retry: all five negative cells passed, while
both positive cells failed before a typed `workspace_read` failure because the
frozen upstream `llm-ollama` extractor discarded Ollama's provider tool-call
IDs.  Superchat correctly rejected the resulting unidentifiable tool origin.
v2.3 changes only the private transport pin to the audited fork commit below,
which preserves the response and streaming tool-call IDs.  It does not relax
the origin check, change the tool surface, or authorize a rerun of v2.2.

Revision v2.4 supersedes the consumed v2.3 assignment whose contract digest was
`d899fcdc43704e7fe39e8adbc18f1fe387bdc292d43562d02bbc1be76fe75916`.  v2.3
made the full fourteen allowed requests: P-stale and all five negative cells
passed, but P-missing's model called `workspace_info` before the required
initial `workspace_read`, then called `read_buffer` on the non-live setup
buffer.  The verifier correctly rejected that cell because no typed initial
`workspace_read` failure or recovery intervention existed.  v2.4 changes only
the consumer prompt to state that `workspace_read` must be the first tool call
and explicitly forbids `workspace_info` or another tool before it.  The tool
surface, validator, Runtime behavior, and fail-closed verdict are unchanged.

Revision v2.5 supersedes the consumed v2.4 assignment whose contract digest was
`3bbc5c609ff7d2b7630fd8501220235ed1ea94bf3c8aa8cc5ccf5ccbb85c5ba3`.  v2.4
made all fourteen allowed requests: both positive cells and four negative cells
passed, but N-conflict's model requested `workspace_write` after the initial
`workspace_read` failure.  Authority cancelled the write because the frozen
approval predicate allows only `local-read`, and the verifier correctly kept
the cohort failed.  v2.5 changes only the consumer prompt to mark the task
read-only and forbid `workspace_write`, `EditBuffer`, `ReplaceBuffer`, and
`eshell-command`.  Authority and the negative prohibition remain unchanged.

## Freeze audit

The contract is frozen only after all of the following are true:

- every product, runner, dependency, schema, prompt, and verifier value below
  is exact;
- the repaired product and canonical ERT suite are complete;
- this contract and the final runner have recorded SHA-256 digests;
- all fifteen v0 product files are re-hashed from the same working copy; and
- a read-only dry audit proves that every required manifest field has a
  fail-closed predicate in the runner.

The frozen product digest table is:

| File | v2.5 SHA-256 |
|---|---|
| `superchat.el` | `20807812e04e795946b9ef872885fa3b446a473597a401a74f532c7c04b9ead0` |
| `superchat-runtime.el` | `4f48e7fe43a469ec1aee2739524fba2b4d1daff14f6e4cb06ce3ca44d8dd556a` |
| `superchat-llm.el` | `1430b708ee308b3953673c1b8f76ce1bd0f670d7f7ce1e3fccbe99f9de5d1733` |
| `superchat-authority.el` | `428a645ec3986c6fe0ac64dc4b2634ce0fa6f7c00a05378cc672efbdbab52937` |
| `superchat-workspace.el` | `c7fd05abc3d38d657daf3b94f3ba1ef54c6be6744df293601d436de5d0459634` |
| `superchat-experience.el` | `335305878437903fcfa36cd4e6cd64d03a3ff3a1d11d1751214a82ac56128531` |
| `superchat-closure.el` | `8e6e4bad3ce76b759794da84ea8c03d2f43d7bf4c165350fd92c4b99f086d198` |
| `superchat-tape.el` | `68b0d592c9c797f10e94b81e09155f515a9c2c64bd40038bc566ba9ec68fcf86` |
| `superchat-projection.el` | `4597d6623de642e12d7dc210a4a0c3fbbe650e96e9a7063b24d009073430867f` |
| `superchat-preset.el` | `09458c6356e30199e11ac0cd779e7be9d14ea59db73cf80de5b5a3bd032e878f` |
| `superchat-tools.el` | `166187aa91888e76d65f787d6121c2e9a1bce3301d67be95fcd1b63a2261a2d1` |
| `superchat-dispatcher.el` | `c8b6a8e91842f1d00131cf68f3d1238f194f80404298a7f111634319c922db48` |
| `superchat-prompt-hooks.el` | `362fec26a1dc776a095f687395f76e26c53b6e0d39f6fe3c18cdf1dfef8d4f0b` |
| `superchat-agent-loop.el` | `f2aae4b32cee04ca049e43286dd2cfeca316b952b2b8ce734b58418ea70aaab8` |
| `superchat-db.el` | `f4a76b0239f0041f7e895e3622df820ac1aaf85bec5aedfca7b59077fe0b1536` |

`superchat-llm.el` must change because the sealed v1 validator rejected
`workspace_write.append`.  If the repair also changes boolean preservation in
Authority or Tape, the corresponding `superchat-authority.el` or
`superchat-tape.el` digest changes too.  Every table entry is nevertheless
recomputed; unchanged v0 values are not assumed.

## Repaired boolean provenance

The repair is an exact-evidence correction, not a Profile or tool-order change.
Canonical ERT tests must prove these predicates before the contract is frozen;
the claimed assignment's static preflight must prove them again before any
provider call:

1. `superchat--llm-validate-evidence-tools` accepts only named, callable,
   unique, synchronous, non-dynamic tools whose argument types are one of
   `string`, `integer`, `number`, or `boolean`.
2. The resolved `workspace_write.append` provider schema has type `boolean`, is
   optional, and otherwise byte-for-byte matches the frozen schema.
3. The ordered resolved schemas exactly match the frozen ten-tool order.  A
   resolver that returns registry order instead of Profile order fails.
4. Exact action provenance distinguishes an omitted `append`, JSON `true`, and
   JSON `false`.  `false` must not become absent, `null`, or an omitted member.
5. For each of those three cases, normalized arguments, exact JSON bytes, byte
   size, and SHA-256 are asserted by a deterministic ERT test.

The launch manifest records the test names and the zero-unexpected ERT result,
plus the exact `workspace_write` schema JSON and hash.  A passing test name
without its retained result is not evidence.

The frozen exact-evidence schema hashes are:

| Tool | Schema SHA-256 |
|---|---|
| `read-file` | `9f7367cd4d1cd0b5a595d9f4a2fe90e777bf77e72abfefd7abbdc93d308ec319` |
| `list-files` | `7cd051be9d00298374cad0c90ac48a357dba78def800929bfb99e8cc32aeeb1d` |
| `search-text` | `d8bcb617cb458d616bc8abba19e9d621a47543d05a0f0bcc60182b601ae89186` |
| `read_buffer` | `eea7cce265b686073290f10fc2dc3fff7ffc480a2f184083ee56d787caea6140` |
| `EditBuffer` | `d0b2ec281383ca039bd1fc533fb68c08b305de609063758fae9381f7b034c7c8` |
| `ReplaceBuffer` | `650459ea2397f0ec1d9aa811f463d413a7745d9484bc718c33ac15a7d80778de` |
| `eshell-command` | `c680c39df8639c728a8896271800038609c5b23eb2c64dbdc997782f08cf6f40` |
| `workspace_read` | `955c6c05c0ad647c58ffb8992e79833209248600603b0f2f66b91f979897aac1` |
| `workspace_write` | `c5da0eaad012ad332db1792db6f28f3e2be6d5a6df1d85acfd096cd2c8e777d4` |
| `workspace_info` | `237a2ace10f88329883585f93165aa756513bb0929dc08cc9dbd5a11e6e5a8da` |

The ordered ten-schema vector SHA-256 is
`5df69e025360a06bc1eac4d875af6a3fb28b0d874ffe0b836bac55cca5807e64`.
The exact Profile JSON SHA-256 is
`ae4caa7e7931f3c508d542eae60e51028c67792289fd65de416580945bd9bc00`.
The exact final system-prompt SHA-256 is
`ddb4f26e82110eba8735b43c59c44e1ff99ecb301eb704b2db7269c7d646b067`.

## Private bootstrap

The runner uses one fresh private scratch root.  It clones
`https://github.com/yibie/llm.git`, checks out
`f6a83bb05e45377d5166821b84aa178c9164b55b`, and creates a private `elpa/`
containing exactly the required transport dependencies `compat`, `plz`,
`plz-event-source`, and `plz-media-type` plus their transitive package
requirements.

The acceptance process starts under `emacs -Q --batch`, initializes only that
private package directory, adds the pinned checkout to `load-path`, and loads
the frozen v2 runner.  Both substituted paths must be absolute.  The runner
then requires `llm` and `llm-ollama`, constructs

```elisp
(make-llm-ollama :chat-model "qwen3.5:9b" :host "localhost" :port 11434)
```

and requires both features and `llm-ollama-p` to succeed.  Construction and
preflight must not call `/api/chat`.

Private absolute paths and the backend object never enter evidence.  Paths are
recorded as resolved role paths such as `<private-llm>/llm-ollama.el` and
`<private-elpa>/compat-<version>/compat.el`; a basename alone is not a resolved
path.

## Once-only assignment

Freezing assigns one identifier:

```text
experience-view-internal-acceptance-v2/c7ade8112e9120a77aff5bca67b49d88f7ef898bc26c292f726c6edad745da17
```

The evidence root is deterministically derived from that identifier, not from
the clock.  The launcher must atomically create it and persist
`assignment-claim.el` before bootstrap or any provider call.  Existing claim or
evidence paths reject launch.  The claim is never deleted by cleanup.

The claim records the assignment ID, contract hash, runner hash, observed `jj`
working-copy identity, UTC start time, and status `claimed`.  Any bootstrap,
preflight, setup, provider, timeout, or evidence failure consumes the
assignment.  The runner writes the terminal failure if possible and exits; it
must not retry, resume, repair, replace a cell, create a second evidence root,
or offer a preflight-only mode for the frozen assignment.  A later attempt
requires a newly frozen contract revision.

Within the one assignment, the seven cells execute once in v0 order.  No cell,
provider request, or prompt is automatically retried.  The only second request
allowed in a cell is the contract-directed tool follow-up within the v0
two-request ceiling.

## Complete launch manifest

`launch-manifest.el` is sealed before the first provider request.  Every field
below is required.  A missing value, `nil` outside an explicitly frozen
inherited `nil`, an empty string, a basename where a resolved role path is
required, an anonymous function where identity is required, a digest mismatch,
or an unrecordable value rejects the whole assignment before a call.

### Contract, source, and launch identity

| Required field | Acceptance predicate |
|---|---|
| assignment ID and claim digest | exact frozen ID; claim exists once |
| v0 and v2 contract paths and SHA-256 | exact frozen hashes |
| runner role path and SHA-256 | exact frozen runner hash |
| product source | all fifteen ordered path/digest pairs match the v2 table |
| working-copy identity | complete mutable `jj` identity is recorded |
| Emacs | exactly GNU Emacs `31.0.91` |
| cost and request ceiling | `0 CNY`, local-only, `2` per cell, `14` cohort |

### Bootstrap and dependencies

| Required field | Acceptance predicate |
|---|---|
| private source role | `<private-llm>`; no absolute path |
| llm origin and commit | frozen Git URL and commit |
| llm version | exactly `0.31.1` |
| adapter role path and SHA-256 | resolved role path; hash `74dcdad296f920ea7ee43e9411398f05e0079d044a34c6e2efa4d7d3aa107505` |
| dependencies | for every resolved package: feature, package name, non-empty version, resolved role path, source SHA-256 |
| feature/predicate results | `llm`, `llm-ollama`, and `llm-ollama-p` are all true |
| bootstrap transport count | exactly zero `/api/chat` requests |

The expected version and source digest of every resolved private dependency are
frozen below.  Preflight compares rather than merely records them.

| Feature | Package | Version | Source SHA-256 |
|---|---|---|---|
| `compat` | `compat` | `31.0.0.2` | `e7c2ddc4b6d7af6e6b038c86a0d11bf23907d5b6018c406683bcef41d1518ce3` |
| `plz` | `plz` | `0.9.1` | `e1b60b4b3634c6760b5d66a24d81b296ee67eeaf76f4b7d216fb47b766741637` |
| `plz-event-source` | `plz-event-source` | `0.1.3` | `2a2a5b73cb35e091389a31fc01da70959d85b95a4f079b3b7751f4db507d290e` |
| `plz-media-type` | `plz-media-type` | `0.2.4` | `d19a8df85cd22f5cd40cce740c5dba9d4894bfee5cd8668e664916d0deeba27b` |

### Provider and model

| Required field | Acceptance predicate |
|---|---|
| provider class | `llm-ollama` |
| Ollama executable/app role, digest, and version | resolved and exactly `0.32.6` |
| endpoint | exactly `http://localhost:11434`; no alias or fallback |
| provider capabilities | exact ordered materialized value |
| explicit model | exactly `qwen3.5:9b` |
| model manifest and layer digests | exact v0 values |
| backend parameters | complete non-secret accessor record; non-standard parameters empty |
| reasoning | Profile `inherit`; effective request value `none`; no other reasoning control |
| streaming | provider capability plus actual value for every attempt |

Secrets and the full backend object are forbidden.

### Complete built-in `code` Profile

The manifest records every accessor by name and value: `name`, `profile-id`,
`description`, `type`, `capability`, `skill-body`, `tools`, `model`,
`temperature`, `max-tokens`, `reasoning`, `max-tool-calls`,
`confirm-destructive`, `timeout`, `pre`, `version`, `triggers`, `source`, and
`source-file`.  Values must match v0, including inherited `nil`/`inherit`
values, source `builtin`, version `1.0`, and the exact body.

The manifest also retains, in Profile order:

- all ten resolved provider-visible schema structures;
- the exact JSON bytes, byte size, and `sha256:` hash for each schema;
- the exact JSON bytes, size, and hash for the ordered schema vector; and
- validator results proving no duplicate, synthesized, dynamic, asynchronous,
  missing, or unrecordable tool.

Length alone is not a schema check.

### Prompt, options, Authority, and hooks

The manifest records and compares:

- actual final system-prompt bytes and SHA-256, not only the requested
  constant;
- ordered system-prompt, effect, rewrite, result-filter, and other material
  hook symbols; every required list is present even when empty;
- topology `single`, language `English`, no active extension, and single Actor;
- exact consumer prompt bytes for all seven recorded Artifact/revision IDs;
- exact initial prepared-envelope JSON, byte size, and hash for every cell;
- materialized temperature, max tokens, response format, tool choice,
  reasoning, non-standard parameters, multi-output, and streaming values;
- effective `max-tool-calls=2`, `max-tool-rounds=2`,
  `unknown-tool-retries=0`, round-limit action `stop`, no-data timeout `60`, and
  timeout multiplier `1.0`;
- destructive confirmation enabled;
- Authority policy name and version `authority-v1/default`, exact policy
  record, attendance, and the stable named approval-function identity and
  source digest; and
- proof that the approval predicate rejects every non-`local-read` effect.

Anonymous approval or round-limit lambdas fail because their identity cannot be
retained.  Recording the intended setting without reading the materialized
request fails.

## Setup sealed before the first call

All seven scratch DBs and Workspaces are created and checked empty before the
first provider call.  Each cell's setup manifest records every setup event by
`session-id` and `event-id`, including:

- producer Session, Turn, Run, decision, output, Artifact-add, revision-change
  when applicable, terminal Run, and terminal Turn;
- the Artifact ID, original revision, current revision, Workspace ID, and
  producer identity;
- the one Context and its exact text;
- every Candidate and its exact text;
- every evaluator Session, Turn, Run, decision, output, evaluation, terminal
  Run, and terminal Turn;
- every `applies-under` and `fails-under` Relation;
- every promote and revoke disposition, including the revoked Promotion's
  prior reference; and
- the consumer Session, prompt, expected positive/negative classification, and
  expected Candidate/Promotion/Relation/Context receipts.

Counts and relations must match the v0 seven-cell table exactly.  The runner
must query the sealed DB state to prove one Recovery-Class Context per cell,
fresh isolation, closed producers/evaluators, and a genuinely changed current
revision in stale cells.  A setup plist that omits an event reference fails.

## Attempt evidence and transport counting

The actual transport counter sits at the concrete Ollama `/api/chat` dispatch
boundary.  Incrementing a high-level `llm-chat-*` entry before dispatch is not
proof of a request.  Each retained invocation record has cohort and cell
ordinal, attempt ordinal, local endpoint, model, start/result status, and the
unique decision-snapshot event ID.  It contains no secret request headers.

Immediately before every actual dispatch, Tape must already contain exactly one
`decision.snapshot.recorded` with exact prepared-envelope JSON, size, and hash.
The invocation and snapshot join one-to-one.  Per-cell and cohort counters,
invocation records, and snapshots must agree; otherwise the cell fails.

Attempt 2 in a positive cell is dynamic and therefore is not fabricated during
launch preflight.  It is recorded after the typed failure and intervention but
before its dispatch, and must contain the exact Experience prompt and complete
Candidate, Promotion, Relation, and Context receipt.

## Verdict implementation

The v0 positive and negative observable verdicts remain normative.  The v2
verifier must fail closed on every clause, not sample a subset.

For each positive cell it additionally makes the required joins explicit:

1. the first request has exactly one cross-Run `workspace_read`
   `tool.requested`, with the frozen Artifact and missing/stale arguments, joined
   by execution ID to exactly one typed `tool.failed`;
2. exactly one intervention is caused by that failure and its receipt exactly
   names the expected Candidate, Promotion, Relation, and Context;
3. exactly one attempt-2 snapshot follows the intervention and contains the
   exact prompt and receipt;
4. no response or other tool request occurs between the intervention and the
   earliest Recovery Action;
5. that Action is exactly no-argument `workspace_info`; extra argument members
   fail; and
6. its execution ID joins to exactly one `tool.completed` Outcome.

For every negative cell the verifier scans every prepared envelope and Action.
It requires zero intervention; absence of the blocked Candidate text and all
Candidate/Promotion/Relation/Context IDs and receipt bytes; no guessed or stale
revision; no `workspace_write`/publish Action; and no claim that cites the
blocked reviewed Candidate.  An independently chosen no-argument
`workspace_info` or request for user help remains allowed by v0.

The runner replays Tape in `unwind-protect` even when submit/setup fails.  Once a
consumer Run is accepted, its handle, events, terminal/audit-incomplete record,
transport count, and verdict cannot be saved as `nil`.  Missing, duplicate,
ambiguous, late, timed-out, provider-error, setup-error, or unclassifiable
evidence fails and is retained.

Deterministic verifier ERT tests must contain at least one accept case and one
reject case for every numbered positive rule and every negative prohibition.

## Evidence set and final gate

The consumed assignment retains:

- assignment claim and terminal launch status;
- complete launch and seven setup manifests;
- seven SQLite Tapes and recovery files;
- every exact prepared envelope, Action, Outcome, invocation record, and cell
  verdict;
- cohort verdict and request totals;
- canonical ERT command, complete output, exit status, and source hashes; and
- an inventory of every evidence file with byte size and SHA-256.

Default enablement is earned only when all seven v0 verdicts pass, every count
and join above agrees, the cohort used no retry or replacement, transport use is
at most two requests per cell and fourteen total, the canonical ERT suite has
zero unexpected results against the sealed source, and a terminal re-hash still
matches this contract.

Any failure consumes v2, leaves
`superchat-runtime-recovery-intervention-enabled` at `nil`, and names the first
required product or contract repair.  Only a fully admissible pass authorizes
the separate one-line default change.
