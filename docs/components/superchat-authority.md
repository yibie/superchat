---
description: "The tool execution pipeline: the one seam every model-invoked tool call crosses. It checks identity and liveness, asks the Session's permission level for a decision, sends an `ask` to the approval seam, wraps and filters the call with the three `tools/*` waterfalls, and records every step on the Tape. The file keeps its historical name; the Authority decision machine it used to hold was removed in M6."
kind: "reference"
---

# superchat-authority

## Summary

Use this component to run a model-invoked tool call. `superchat-tool-execute` is the one execution seam: it refuses a call whose Run or installation is no longer live, asks the Session's permission level whether the call may run, sends an `ask` to the approval seam, runs the body at most once inside the `tools/execute` waterfall, filters the result through `tools/post-execute`, and appends the audit rows. It does **not** decide who may run what: [`superchat-sandbox.el`](../../superchat-sandbox.el) decides (permission levels) and [`superchat-approval.el`](../../superchat-approval.el) asks. Until M6 this file also held Authority — effect resolvers, named policies, an approval hash and preview registry; all of that is deleted ([decision record](../decisions/accepted/2026-09-17-permissions-the-dsh-way.md)). The file name is historical.

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

None. A Session starts at `superchat-sandbox-default-mode` (`workspace-write`) and `superchat-approval-default-policy` (`ask`); `/permissions` changes the level for one Session. `superchat-approval-function` is the human answerer the approval chain ends in (default: `superchat-approval-confirm`, `y`/`n`/`?`).

### The decision

| Tool class | `workspace-write` (default) | `read-only` | `danger-full-access` |
|---|---|---|---|
| Built-in tools (`read_buffer`, `edit_buffer`, `save_buffer`, `write_file`, …) | allowed; a tool that writes to an explicit target checks that target itself, where the write happens | allowed; writes are denied by the tool, with a one-shot escalation offer | allowed |
| Full Emacs control: `eval_elisp`, shell tools, `define_tool`, every agent-written tool | asked about **per call** | denied | allowed |
| MCP tool of a server in `superchat-mcp-trusted-servers` | allowed | denied | allowed |
| Any other registered (plugin / MCP) tool | asked about per call | denied | allowed |

The `tools/pre-execute` waterfall runs first; a listener answers by returning the call with `:kind` `allow`, `deny` or `ask` (and an optional `:reason`). The level rule is then applied as a fixed stage and **the stronger of the two stands** (`deny` > `ask` > `allow`). A listener that allows without delegating ends the waterfall only — it cannot spare a call the level's question. A listener's refusal or fault is a denial.

Two calls are decided before the level is consulted: a **blocked** call (an untrusted `ask_user`, a malformed trusted one, a request-local `:block`, or a dependent tool named by a blocked Experience recovery View) is denied whatever the level — `danger-full-access` does not skip it; and **trusted human input** (`ask_user` through Runtime's paired adapter) is a question to the person, not an effect: it runs unless the Session's approval policy is `never`.

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

### Absent dependencies

Without the `tape` plugin there is no audit, and a call is blocked rather than executed without its evidence. Without `tools-builtin` no built-in tool is a selectable descriptor. Without any answerer (a batch Emacs with the default minibuffer answerer) an `ask` is `unavailable` and the call is refused.

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
- [Permissions the DeepSeek Harness way](../decisions/accepted/2026-09-17-permissions-the-dsh-way.md) — the contract for levels, target checks, escalation and approval.
- [ADR-0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md) — why a missing trusted wrapper fails closed.
- [ADR-0006](../adr/0006-authority-refusal-consumer-delivery.md) — why a refusal reaches the consumer as a result does.

## Model experience

Indirectly: the permission level decides which calls the model may make, and this pipeline writes the text of a refusal into the conversation the model reads.

#### What the model sees

A refusal is delivered into the tool-result slot the model reads, so a denied call is visible to the model as its tool's answer rather than as silence. A sandbox denial from a write tool carries two marker lines that teach the one sanctioned retry (`sandbox_permissions` + `justification`). Every request that offers tools ends with a runtime-context snapshot stating the Session's current file policy and approval policy.

#### Token effect

The pipeline adds no material of its own; a refusal's text and the tool results it delivers cost what their strings cost in the tool-result slot. The runtime-context snapshot is a few sentences per request.

#### KV cache effect

Indirectly: a refusal or a tool result is appended to the conversation, so it extends the request from that point on. The runtime-context snapshot sits after everything else in the request so that a level change does not disturb the cached prefix before it.

## Known limitations

- The target checks live in Superchat's own write tools. Code the model runs through `eval_elisp` or a shell is not fenced in; that is why those tools are asked about on every call rather than classified.
- A listener that answers without delegating ends the `tools/pre-execute` chain, so a listener after it is never consulted: there is no deny-beats-allow rule *between listeners*. Listener order follows the Scope merge; only the fixed level stage is guaranteed to run.
- The Experience guard blocks by tool name from a request-local View; it is not a permission rule and no level relaxes it.
- The file and its `superchat-authority--` prefix are historical; renaming them is a separate change.

### Dev note

<details><summary>Working context for maintainers</summary>

The invariant `tool-execution-passes-the-pipeline` checks that a well-formed decision exists before any body runs. The part most likely to drift is the split of responsibilities: keep decisions in `superchat-sandbox.el`, questions in `superchat-approval.el`, and only identity, liveness, audit and delivery here.

</details>
