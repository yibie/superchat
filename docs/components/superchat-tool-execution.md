---
description: "The tool execution pipeline: the one seam every model-invoked tool call crosses. It checks identity and liveness, asks the Session's permission level for a decision, sends an `ask` to the approval seam, wraps and filters the call with the three `tools/*` waterfalls, and records every step on the Tape. Until 2026-09-20 the file was `superchat-authority.el`; the Authority decision machine it used to hold was removed in M6."
kind: "reference"
---

# superchat-tool-execution

## Summary

Use this component to run a model-invoked tool call. `superchat-tool-execute` is the one execution seam: it refuses a call whose Run or installation is no longer live, asks the Session's permission level whether the call may run, sends an `ask` to the approval seam, runs the body at most once inside the `tools/execute` waterfall, filters the result through `tools/post-execute`, and appends the audit rows. It does **not** decide who may run what: [`superchat-sandbox.el`](../../superchat-sandbox.el) decides (permission levels) and [`superchat-approval.el`](../../superchat-approval.el) asks. Until M6 this file also held Authority — effect resolvers, named policies, an approval hash and preview registry; all of that is deleted ([decision record](../decisions/implemented/2026-09-17-permissions-the-dsh-way.md)). Until 2026-09-20 the file was named `superchat-authority.el`.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

Register a tool with [`superchat-tools`](superchat-tools.md) and let the pipeline run it. Nothing else may execute a model-invoked call.

### When to choose it

Choose this seam whenever code runs a tool on the model's behalf. Choose a `tools/pre-execute` listener to make your own tool stricter, an `approval/request` answerer to answer (or preview) a question yourself, `tools/execute` to wrap a call and `tools/post-execute` to replace or block a result. Choose a Command when a human is the caller.

### Minimal configuration

None. A Session starts at `superchat-sandbox-default-mode` (`edit-buffers`; the former name `workspace-write` is still accepted) and `superchat-approval-default-policy` (`ask`); `/permissions` changes the level for one Session. `superchat-approval-function` is the human answerer the approval chain ends in (default: `superchat-approval-ask`. With `superchat-approval-prompt` at `publish`, the default, the question is published in the conversation and listed in `superchat-pending`, Emacs goes on, and it is answered there; at `minibuffer` it is the former `y`/`n`/`?` question).

### The decision

| Tool class | `edit-buffers` (default) | `read-only` | `danger-full-access` |
|---|---|---|---|
| Built-in tools (`read_buffer`, `read_file`, `open_file`, `edit_buffer`, …) | allowed; a buffer edit is refused by the tool only when the target is one of Superchat's own buffers | allowed; buffer edits are refused by the tool, finally — there is nothing to retry with | allowed; Superchat's own buffers are still refused |
| `save_buffer` — the one persisting tool, off the default surface, offered only by an explicit Preset or Request list | asked about **per call** | denied | asked about per call — and this level's approval policy is `never`, so it is refused |
| Full Emacs control: `eval_elisp`, shell tools, `define_tool`, every agent-written tool | asked about **per call** | denied | allowed |
| MCP tool of a server in `superchat-mcp-trusted-servers` | allowed | denied | allowed |
| Any other registered (plugin / MCP) tool | asked about per call | denied | allowed |

The `tools/pre-execute` waterfall runs first; a listener answers by returning the call with `:kind` `allow`, `deny` or `ask` (and an optional `:reason`). The level rule is then applied as a fixed stage and **the stronger of the two stands** (`deny` > `ask` > `allow`). A listener that allows without delegating ends the waterfall only — it cannot spare a call the level's question. A listener's refusal or fault is a denial.

Two calls are decided before the level is consulted: a **blocked** call (an untrusted `ask_user`, a malformed trusted one, a request-local `:block`, or a dependent tool named by a blocked Experience recovery View) is denied whatever the level — `danger-full-access` does not skip it; and **trusted human input** (`ask_user` through Runtime's paired adapter) is a question to the person, not an effect: it runs unless the Session's approval policy is `never`. Such a Session asks nobody, so the input adapter is not attached and `ask_user` is not offered at all — this includes `danger-full-access` with its default `never`, and every delegated child.

### Executing through the pipeline

`superchat-tool-execute TOOL-NAME ORIGINAL-FN ARGS &key async callback context`. `CONTEXT` must carry `:session-id`; the execution identity the Tape audit checks travels with the call from Runtime's accepted execution context.

| Stage | What it receives | What it may change | What it hands on |
|---|---|---|---|
| **admission** | The call and its context | Nothing | The ordinary refusal when the Run is terminal or the materialized installation was withdrawn |
| **normalize** | The arguments | A request-local `:normalize` / `:validate` may normalize or reject them | A deep snapshot of the arguments, and a block reason when a check refuses |
| **decide** | Tool name, arguments, Session | `tools/pre-execute` listeners may tighten | `allow`, `ask` or `deny`, recorded in `tool.requested` before anything acts on it |
| **approve** (only `ask`) | The argument snapshot | The approval seam's answerers | `allowed-once` with the snapshot that was approved, or a refusal |
| **execute** | The approved arguments | `tools/execute` listeners wrap the whole invocation; the chain ends in the body, which runs at most once | The result; a listener that returns instead of delegating ends the call as `blocked`, never as started |
| **post** | The result | `tools/post-execute` may replace a string result or block it (the feedback becomes an error result) | The delivered result |
| **finalize** | Every branch's outcome | Nothing | Delivery through the exact Sink or the single refusal path |

The refusals a consumer can receive are ordinary strings: `[Tool call denied: <reason>]`, `[Tool call blocked: <reason>]`, `[Tool call cancelled by user]`, `[Tool call denied: approval prompts are disabled in this session]` (policy `never`), `[Tool call refused: no approval provider is available]`, and a call whose audit cannot be recorded is blocked instead of running.

### Tool-loop hooks (slice 11b, 2026-10-20)

`superchat-tool-loop-hooks` (default `(superchat-tool-failure-brake-hook)`, nil off) are asked about every call that reaches its body: in phase `before`, just before the body runs, and in phase `after`, once `tool.completed` or `tool.failed` holds the outcome, with `:result` and `:failed`. A hook answers nil (let it be), a string, or `(:stop REASON)`; the first one that does not answer nil decides. A string before the body means the body does not run and the string is the result; after it, the model is given the string while the original stays in `tool.completed`/`tool.failed`, and a `tool.rewritten` fact (`:execution-id`, `:phase`, `:result-text`) records it; the history later requests are built from the Tape still shows the original. A stop before the body records the call as `tool.failed` with status `stopped`, which the failure count does not count. Any stop goes to `superchat-runtime-stop-tools`: the Run's `:tool-budget` becomes `(:action stopped :reason REASON)` unless it already had one, the remaining calls of the round are refused (`Tool calls stopped: REASON`), and the request loop ends the Run with `[REASON]` as failed before its next request, so the ending facts, the notice under the answer and the pending row carry REASON. A hook that signals, or answers anything else, is a fault (`tool-loop-hook` in `superchat-event-faults`) and counts as nil. The hooks are a seam of their own, called directly by the executor, not listeners on the `tools/execute` or `tools/post-execute` waterfalls, and no event type was added for them. The reason: `tools/post-execute` runs only for a call that succeeded, and before `tool.completed` is written, so it can neither see a failed call (the failure brake needs exactly those) nor leave the original result in `tool.completed` while the model is given a rewrite. The failure brake (11c) stops after `superchat-tool-failure-brake` (3) failed calls in a row, counted by Runtime from status `failed` only; it replaces 9c's sentence.

### Cancellation, reentrancy and late results

This interface inherits the request lifecycle and ADR-0006 rather than restating them; what that inheritance obliges this pipeline to do:

- **Running identity comes from Runtime.** An approval answer cannot reopen a terminal or retired Run: a call whose Run is no longer active refuses instead of proceeding, and a Run cancelled while the question is open records the late answer and starts nothing. A Run that ends with a question still undecided seals `audit-incomplete`.
- **An approval is for the arguments that were shown.** The body runs from the approval seam's snapshot, not from the live argument objects.
- **An approval return cannot rebind the call.** The installation was fixed when the descriptor was materialized, so a same-name tool withdrawn and re-registered during approval cannot be executed by the pending call.
- **An asynchronous refusal is delivered once, with its original condition.**
- **A genuine late external result records its match and stops there.**
- **A tool result callback is not a Run terminator.** Runtime alone completes a Run.

### Tape event types

| Type | Meaning |
|---|---|
| `tool.requested` | The call entered the audit, with its `:decision`. This records that a request was made, **not** that it will run |
| `approval.requested`, `approval.decided` | The approval round (written by the approval seam), Run-scoped, with the outcome |
| `tool.execution.started` | The body is about to run |
| `tool.completed` | The body returned |
| `tool.failed` | The call was refused, blocked, or the body signalled, with its disposition |

Rows written before M6 also carry `:effects`, `:policy`, `:policy-version`, `:attendance` and `:request-hash`; nothing reads them, and new rows do not have them. Old `authority.policy.selected` Session events stay on the Tape as inert history.

### Stored tool material (accepted 2026-09-23)

Decision: [the next turn sees previous tool results](../decisions/accepted/2026-09-23-the-next-turn-sees-previous-tool-results.md). While `superchat-tape-store-tool-material` is non-nil (the default), ordinary tool events keep what the model sent and what it got back, in the event payload:

| Event | Fields |
|---|---|
| `tool.requested` | `:arguments-text`: the positional arguments as the model supplied them (the provider arguments before normalization), JSON-encoded as an array |
| `tool.completed` | `:result-text`: the result as delivered to the model, after the post-execute stage |
| `tool.failed` | `:result-text`: the refusal or failure text delivered to the model (for example `[Tool call denied: …]`) |

Each text keeps at most `superchat-tape-tool-material-max-chars` characters (default 2000). A cut text carries `:arguments-truncated t` / `:result-truncated t` and the original length in characters as `:arguments-chars` / `:result-chars`; the stored text itself has no marker. The writer then checks the whole encoded payload against `superchat-tape-max-inline-bytes` and shortens the text further until it fits, so the new fields never cause the Tape to replace the payload with its size and hash, and never make a `requires-payload` event fail. Payloads that were already too large without the new fields behave as before.

What is and is not protected: the texts pass through the Tape's keyed-field redaction like every payload, which redacts sensitive *keys*; a secret inside a string (file text a tool read, a Lisp expression, an evaluated value) is stored as it is. The texts are never written to the row's `content`, so the 500-column summary and full-text search do not see them. They are readable wherever a payload is readable: the `tool_history`, `file_history`, `recent_errors` and `sql` tools (off by default) return whole rows. With the option nil, events carry the same fields as before. Rows written before this change have no texts. A chat View rebuilt from the Tape (`superchat-open-session`) shows them in its `Tool call:` / `Tool result:` blocks, with `… [cut: N more characters]` under a cut text (2026-09-23; before, those blocks were empty for ordinary calls).

### Absent dependencies

Without the `tape` plugin there is no audit, and a call is blocked rather than executed without its evidence. Without `tools-builtin` the built-in tools that row installs are not selectable descriptors; a built-in tool another row owns (`work_record` of `work-records`, and the tools of the memory, sub-agent and workspace rows) is installed by its own row and stays selectable while that row is mounted. Without any answerer (a batch Emacs with the default answerer) an `ask` is `unavailable` and the call is refused.

## Understand the implementation

<details><summary>Implementation internals</summary>

`superchat-tool-execute` is the seam. Its stages are marked in the source, and a refusal and a result leave through the same delivery path, so a consumer cannot tell them apart by shape. The decision itself is one call, `superchat-permission-level-decision` (in `superchat-sandbox.el`); the question is one call, `superchat-approval-request-snapshot` (in `superchat-approval.el`), which owns the open-approval mark and the audit pair.

| Section | Role |
|---|---|
| Timer owner | The Run's transport clock, with its absent case (also the `tools` Service's `transport-timer-owner` method) |
| Installation identity | The exact materialized installation a call belongs to, and its liveness |
| Snapshot | The deep copy used on every seam path |
| Experience guard | `--experience-block-reason`: a blocked recovery View blocks the dependent tools it names |
| Evidence and events | Tape appends, evidence origin, result fingerprinting |
| Settlement | `--record-failure`, `--handle-quit`, `--finish-success`, `--finish-error` and Runtime's open-work marks |
| Trusted human input | `--make-human-input-operation` and its single-settlement capability |
| Invocation | `--invoke` (the `tools/execute` wrapper), `--invoke-body`, the sync and async bodies |
| Execution | `superchat-tool-execute` |

</details>

## Further exploration

- [superchat-tools](superchat-tools.md) — the registry whose tools this pipeline runs; prerequisite.
- [Permissions the DeepSeek Harness way](../decisions/implemented/2026-09-17-permissions-the-dsh-way.md) — the contract for levels and approval. Its write-target checks and one-shot escalation were removed by [Buffer-first mutation](../decisions/implemented/2026-09-18-buffer-first-mutation.md), which is the contract for what a built-in tool may change.
- [ADR-0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md) — why a missing trusted wrapper fails closed.
- [ADR-0006](../adr/0006-authority-refusal-consumer-delivery.md) — why a refusal reaches the consumer as a result does.

## Model experience

Indirectly: the permission level decides which calls the model may make, and this pipeline writes the text of a refusal into the conversation the model reads.

#### What the model sees

A refusal is delivered into the tool-result slot the model reads, so a denied call is visible to the model as its tool's answer rather than as silence. A refusal is final: no tool result offers a retry with wider permissions. Every request that offers tools ends with a runtime-context snapshot stating the Session's current mode (the model changes things by editing buffers; saving is the user's) and approval policy.

#### Token effect

The pipeline adds no material of its own; a refusal's text and the tool results it delivers cost what their strings cost in the tool-result slot. The runtime-context snapshot is a few sentences per request.

#### KV cache effect

Indirectly: a refusal or a tool result is appended to the conversation, so it extends the request from that point on. The runtime-context snapshot sits after everything else in the request so that a level change does not disturb the cached prefix before it.

## Known limitations

- The target checks live in Superchat's own write tools. Code the model runs through `eval_elisp` or a shell is not fenced in; that is why those tools are asked about on every call rather than classified.
- A listener that answers without delegating ends the `tools/pre-execute` chain, so a listener after it is never consulted: there is no deny-beats-allow rule *between listeners*. Listener order follows the Scope merge; only the fixed level stage is guaranteed to run.
- The Experience guard blocks by tool name from a request-local View; it is not a permission rule and no level relaxes it.
- The file and its symbols were renamed from `superchat-authority` on 2026-09-20; no compatibility aliases were kept, because nothing outside this repository used them.

### Dev note

<details><summary>Working context for maintainers</summary>

The invariant `tool-execution-passes-the-pipeline` checks that a well-formed decision exists before any body runs. The part most likely to drift is the split of responsibilities: keep decisions in `superchat-sandbox.el`, questions in `superchat-approval.el`, and only identity, liveness, audit and delivery here.

</details>
