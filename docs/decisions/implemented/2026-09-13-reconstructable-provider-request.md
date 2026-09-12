# Decision: A request header makes the actual provider request reconstructable

Status: implemented

## Problem

M1 requires that an independent reconstruction from the Tape reproduce the provider-visible system text, messages and tool schemas of every request, including each rewrite and retry, and that missing evidence report `unknown` rather than a pass. The Tape does not carry what that acceptance needs. Three facts are checkable in the tree today:

- `superchat-pipeline.el` appends `prompt.assembled` with `:request-id`, the budget `:receipt`, `:system-chars` and `:message-chars`. The system prompt and the assembled message list are not in the payload.
- `superchat-invariant.el`'s `prompt-reconstructable` check tests only that the receipt record is non-nil and returns a message when it is missing. It reconstructs nothing, so its name promises a property it does not test.
- No Tape row carries a tool schema. `tool.requested` carries `:tool-name`, `:request-hash` and `:effects`; the name, description and `:parameters` the provider receives are built at request time.

An executable verification of the M1 acceptance measures the gap rather than assuming it: a two-turn chat is driven through the real product entry points, the request is captured at the `llm-chat-streaming` and `llm-chat-async` seams, and a reconstruction built only from the Tape is compared field by field. Message text is recovered, because the chat recorder appends material rows whose `kind` is the conversation role and whose `content` is the full text; the system text, the provider-visible rendering of each message and the tool schemas are not. The instrument and its raw output are `m1-request-reconstruction.el` and the `evidence-*.txt` files under `.scratch/m1-verification/` (gitignored local material, not part of this record).

Two properties of the current implementation bound the decision. The provider-visible message is not the Tape row: the history fragment is rendered with a presentation receipt (`[Context v0 | raw …#2, …#4 | represented none | omitted 4 | recall available]`) and the current turn's text is folded into the same message, so equal text does not imply an equal message list. And the in-buffer `superchat--conversation-history` already holds a copy of the same material rows: after two turns its four entries match the Tape rows of the same storage ids. The buffer list is therefore a regenerable view of the Tape, not a second history.

## Decision

The user accepts this mechanism. The user's words, given after reviewing the record and reproducing the verification: "这份记录可以接受。它的核心判断是对的：Tape 现在只记了 system 的字符数、消息字符数和工具名哈希，没有正文，所以 M1 的验收在补上'请求头'事件之前不可能通过；写入时机按四种理由（首次、恢复、变化、新系列）控制，不会把 Tape 撑大" and "接受后 M1 才动代码，顺序仍按 roadmap". The user also reproduced canonical smoke case A under `SMOKE_REAL=1` against `qwen/qwen3.8-27b` and observed it complete through the product path, which settles the real-provider question this record raises as a risk below.

## Design

One new session-annotation Tape event type, the **request header**, written once per series of provider requests and referenced by every request that belongs to it.

### What the header records

| Field | Content |
|---|---|
| `:system` | the complete system text sent to the provider |
| `:tools` | the tool schemas as the provider receives them: name, description, `:parameters`, and the tool-choice option |
| `:model` | the target model and the call parameters that shape the request (temperature, max tokens, response format) |
| `:series-id` | the identity of the series this header opens |
| `:reason` | why a new header is written — `first`, `resume`, `change` or `new-series` |
| `:request-id` | the request that caused the write |

Each request in the series appends its own lighter event, the **request message list**, carrying `:request-id`, `:series-id` and the composition rather than the content: which Tape storage ids contributed, in which order, at which role, under which template and template version, together with the presentation receipt the provider receives. Message bodies stay in the material rows; the list states only its source relation. Rewrites and retries each append their own list, so every attempt has an owner. Because the two events name a `:series-id` and a `:request-id`, a reconstruction walks the requests of a run, resolves each to its series header, and rebuilds that request's message list from the storage ids it names, without a second inventory.

### When a header is written

A header is written only on one of four reasons, so that a long session or a tool-call loop does not restate the system text and six schemas on every request:

| Reason | Written when |
|---|---|
| `first` | the run's first request, and no header for the series exists |
| `resume` | the series is continued in a later run or after a persistence boundary, so the reader cannot be assumed to hold the previous header |
| `change` | the system text, the tool schema set, or the call parameters differ from the header in force |
| `new-series` | the caller starts a request series whose inputs are independent of the previous one |

Every other request in the series references the header in force. The reference is by `:series-id` and the storage id of its row, so a reader that cannot resolve the header reports `unknown` instead of falling back to a neighbour's header.

### The invariant

`prompt-reconstructable` either performs the reconstruction and comparison this record describes, reporting `unknown` when the header or a named storage id cannot be resolved, or it is renamed to state what it tests (that the receipt record exists). Keeping its present name over a check that only tests for a non-nil record reports a property the system does not have.

### The buffer cache

The buffer list is demoted to a view regenerated from the Tape and is never read as an authority, which is M1's stated goal; `derive-messages` becomes the single derivation over the Tape Context View. Compaction and the chat window read it. This is a demotion of an existing derived cache rather than a rebuilt history: the measured equality between the buffer entries and the Tape material rows of the same storage ids establishes that the cache already carries the same rows, and the check that the buffer list and the Tape-derived history agree runs before the demotion.

## Cross-references

- [The roadmap's milestones](../../roadmap.md#milestones) own M1, its acceptance and the order of work; this record owns the mechanism those criteria need. The acceptance is stated there and is not restated here.
- [The spine and extension-point proposal](../proposed/2026-09-12-restructure-on-deepseek-harness.md) moves `derive-messages` into the session module and names the in-buffer history list among what that module takes over.
- Source anchors: `superchat-pipeline.el` (the `prompt.assembled` payload), `superchat-invariant.el` (the `prompt-reconstructable` check), `superchat-tape-view.el` (the Context View and its presentation receipt), `superchat-tape.el` (payload redaction and bound, the event-type inventory).

## Alternatives considered

**Record the whole request on every request.** One event per request carrying system text, the message list and all schemas is the simplest shape and needs no series identity. It loses because it multiplies Tape volume by the number of requests in a series: a tool-call loop resends an unchanged 209-character system text and six schemas on every step, and a long session pays that repeatedly. The header form keeps the same evidence at one write per series.

**Reconstruct from the existing `prompt.assembled` receipt plus the material rows.** No new event is added and the Tape keeps its present shape. It loses because the receipt carries character counts, not content: the system text is built at request time and stored nowhere, and the presentation receipt and template wrapping mean the material rows do not determine the message list. This is the alternative the measurement refutes, which is why the acceptance asks for reconstruction rather than for a receipt.

**Store tool schemas once in the tool registry and reference the registry from the request.** The schema body has one home and the Tape stays small. It loses because the registry is live, scoped and mutable: a schema set is only meaningful relative to the scope, plugin state and moment of the request, so a later read reconstructs what the registry holds then, not what the model is offered. A reconstruction that is correct only while nothing has changed does not meet the acceptance.

**Give the invariant a new name and leave reconstruction to a separate verifier.** The naming defect is fixed with a one-line change and no runtime cost. It loses as the whole answer because the acceptance is a property of the Tape, not of a script: without recorded evidence the verifier can only report `unknown` for every request, and the milestone's purpose is that the evidence exists. It remains the fallback for the invariant if the header is rejected.

## Consequences

- The header and the message list are two new event types in `superchat-tape-event-type-specs`, both session-annotation, and the message list names storage ids, so it depends on the Tape's existing storage identity rather than a second one.
- Payload volume and redaction. `superchat-tape--bound-payload` redacts conventionally secret fields and bounds payload size; the system text and the schema set are long, so a bound that truncates them leaves evidence that exists but cannot reconstruct. The bound for these two fields is part of the mechanism, and a truncated field is recorded as truncated so the reconstruction reports `unknown` rather than a partial pass.
- Series identity across runs. `resume` exists because a reader cannot be assumed to hold a header across a restart; an identity derived from something a restart does not preserve misses the `resume` header and leaves every later request reporting `unknown`. The identity is chosen so that a missing header is detectable.
- Schema shape drift. The recorded schemas are provider-shaped; a change to the provider layer's tool serialization does not invalidate past rows, but a reconstruction that re-derives schemas from the live registry compares against the wrong shape. The reconstruction reads the recorded schemas and never re-derives them.
- Two histories during the transition. Until the buffer list is demoted, both it and the Tape-derived history exist; the agreement check runs first, and a divergence is a finding rather than something to reconcile by writing the buffer value into the Tape.
- Real-provider fixture. On 2026-09-13 the canonical smoke case A under `SMOKE_REAL=1` against `qwen/qwen3.8-27b` completes through the product path (`response.completed`, `run.completed`, `turn.completed`, status `completed`); its only mismatches are the scripted answer text and the fake-network prompt capture, which real mode cannot produce. The fixture warms the model with one direct request before the timed run, because LM Studio's first load of the 27B model exceeds the fixture's wait window and produces a failure unrelated to the product.
- The record authorizes no implementation by itself. M1's code lands under [the roadmap](../../roadmap.md), which also owns the two acceptance items that no run exercises yet: the inline real-provider turn and the compaction-anchor comparison.
