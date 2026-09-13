---
description: "Decide whether a tool call may run: resolve what it would affect, apply the built-in policy, the named policy layers and the compatibility hooks, compose them tighten-only, and take approval. The single authority every model-invoked tool crosses."
kind: "reference"
---

# superchat-authority

## Summary

Use this component to answer one question: may this tool call run, and under what conditions? It resolves a call's arguments into effect descriptors, composes a decision from the built-in policy, every named policy layer in the Scope chain and the compatibility hooks, takes approval when the decision is `ask`, and delivers either the body's result or a refusal. Choose it as the only place a decision is made -- a caller that decides for itself produces a second authority. Its main cost is that the decision is not one function: the base fold reads effects alone, the composed decision reads policy and hooks, and monotonicity is a property of how the layers combine, so a reader who stops at any one of them gets a different answer. Its most important boundary is direction: a named layer may tighten a decision and never loosen it, so a plugin cannot grant what the built-in policy withholds.

## Table of contents

- [Use this component](#use-this-component)
- [Understand the implementation](#understand-the-implementation)
- [Further exploration](#further-exploration)
- [Model experience](#model-experience)
- [Known limitations](#known-limitations)
- [Dev note](#dev-note)

## Use this component

Register the effects a tool has, then let the pipeline decide: `superchat-tool-execute` is the one execution seam, and nothing else may authorize a model-invoked call.

### When to choose it

Choose Authority for any decision about whether a model-invoked tool may run. Choose [`superchat-tools`](superchat-tools.md) to put the tool in front of the model in the first place. Choose a Command when a human is the caller.

### Minimal configuration

The plugin depends on the Tape Service, because every decision is audited:

```elisp
(superchat-plugin-mount 'authority)
```

It declares `:inject '(tape)` -- supplied here by the default `tape` provider -- and takes no configuration keys. Composition and lifetime follow the [composition contract](../design/plugin-architecture.md). Effect resolvers and approval previews for the built-in tools come from the separate `tools-builtin` plugin in [`superchat-tools`](superchat-tools.md); without it no built-in tool is selectable.

### The `authority` Service

| Method | Underlying entry point |
|---|---|
| `decide` | `superchat-authority-decide` |
| `resolve-effects` | `superchat-authority-resolve-effects` |
| `register-policy` | `superchat-authority-register-policy` |
| `register-effect-resolver` | `superchat-authority-register-effect-resolver` |
| `register-approval-preview` | `superchat-authority-register-approval-preview` |
| `transport-timer-owner` | `superchat-authority-transport-timer-owner` |

The Service exposes registration but not unregistration; the `superchat-authority-unregister-*` functions are the public way to withdraw an installation.

### Resolving what a call affects

`superchat-authority-resolve-effects TOOL-NAME ARGS` returns a list of effect descriptors, `(:effect KIND :resource RESOURCE [:scope SCOPE])`. The kinds are `local-read`, `local-write`, `process-exec`, `runtime-exec`, `external-read`, `external-write`, `human-interaction` and `unknown`.

A name with a visible resolver uses it. A name without one resolves to a single `unknown` effect scoped to the tool name, so an unclassified call is not silently treated as harmless.

### Asking for a base decision

`superchat-authority-decide TOOL-NAME ARGS ATTENDANCE &optional POLICY EFFECTS` returns a plist: `:tool`, `:attendance`, `:policy`, `:policy-version`, `:decision`, `:args` (normalized) and `:effects`.

`ATTENDANCE` must be exactly `attended` or `unattended`; anything else, including nil, signals. The base decision is composed from the effects alone:

| Effect kind | Attended | Unattended |
|---|---|---|
| `human-interaction` | `deny` | `deny` |
| `local-read`, `local-write`, `process-exec`, `runtime-exec`, `external-read`, `external-write`, `unknown` | `ask` | `deny` |

**This entry point does not read `POLICY`.** It records the policy and its version for the audit trail, and its decision is the same whatever policy is passed. The named policy is applied one level up, in the composed decision. A caller that treats `decide`'s answer as the final one has skipped the policy.

**The base decision is not a permit.** It is one input to a composition that continues through the named layers, the compatibility hooks and approval. In the pipeline the composed `defer` is then resolved by attendance -- `ask` when attended, `deny` when not -- and nothing runs until every later stage has had its say.

### Registering a policy

`superchat-authority-register-policy NAME DECIDE &key scope` registers a tighten-only layer and returns the normalized name. `DECIDE` is called with the resolved effects and the execution context and returns `deny`, `ask` or `defer`; `allow` is treated as `defer`.

| Rule | Behaviour |
|---|---|
| Built-in names | `default`, `observer`, `developer-project`, `operator` and `cowork` cannot be replaced; registering one signals |
| Callability | A non-callable `DECIDE` signals |
| Duplicates | A duplicate `NAME` in one Scope is rejected before its layer or cleanup changes |
| Combining | Every layer in the Scope chain contributes, not just the nearest |

`superchat-authority-unregister-policy NAME &key scope` withdraws that layer's own entry.

### Registering an effect resolver

`superchat-authority-register-effect-resolver TOOL-NAME RESOLVER &key scope` returns `RESOLVER`; a non-callable one signals. The resolver receives the normalized argument list and returns effect descriptors. Resolution follows the Scope chain, and the registration is what makes a name selectable in [`superchat-tools`](superchat-tools.md), because `descriptors` answers from this registry.

### Registering an approval preview

`superchat-authority-register-approval-preview TOOL-NAME PREVIEW &key scope` returns `PREVIEW`; a non-callable one signals. A preview renders the presentation a human is shown; it answers nothing. The seam that decides whether an `ask` call proceeds is the approval provider below, and registering a preview does not make a tool approvable.

### Composing decisions

`superchat-authority-compose-decisions &rest DECISIONS` returns the strongest of them, ranked `deny > ask > allow > defer`. It accepts the decisions either as separate arguments or as one list. This is the function that makes a layer's `allow` harmless beside a `deny`, and the reason the layers are described as tighten-only.

### The approval hash

`superchat-authority-approval-hash TOOL-NAME ARGS EFFECTS POLICY-VERSION &optional EXECUTION-ID DISPOSITION` returns the canonical hash an approval is bound to. It covers the tool, its normalized arguments, its resolved effects and the policy version, so a changed argument list produces a different hash and an approval taken for one call cannot be replayed against another.

### The transport timer owner

`superchat-authority-transport-timer-owner &optional CONTEXT` resolves the opaque owner of the transport clock for `CONTEXT`, defaulting to the current tool-execution context. It returns the owner when the context carries one, the sentinel `:superchat-transport-timer-owner-unavailable` when the context names a Run whose owner is absent, and nil when the context names no Run at all -- so "no Run" and "a Run whose clock is gone" stay distinguishable.

### The agent loop's contributions

The agent loop contributes four optional behaviours, declared once in `superchat-authority--contributors` rather than probed at the call site. Each row states what an absent contributor means:

| Seam | Absent means |
|---|---|
| `intercept` | Arguments pass unchanged |
| `permission` | `defer`, so the composed decision stands |
| `confirmation` | A classified destructive effect still requires confirmation under guardrails |
| `result-filter` | The result is delivered unchanged |

### The approval provider

Approval answers one question: **this exact call has been decided `ask` -- may it proceed?** A provider is not a preview. `register-approval-preview` supplies the text a human is shown; the provider is what actually answers.

**How a provider is supplied.** Authority resolves exactly one approval function: a request-local value when it is callable, otherwise the default, otherwise the absent case. That ladder is the whole of it -- two function sources and an absent case, not three:

| Source | Where it comes from | Reported as |
|---|---|---|
| Request-local | `:approval-function` on the call's context | `request` |
| Default | `superchat-authority-approval-function` | `default` |
| Neither callable | -- | `absent` |

The request-local value wins over the default. A plugin that wants its own approver for its requests supplies the request-local value, so what a plugin does with its own channel, its granted resource ranges and its fallback is composition *inside* the chosen function, not a further source for Authority to rank. When neither is callable the source is `absent`, which is not a user decision: the refusal reports that no approval provider is available, distinct from a user's cancellation.

**What a provider receives.** One plist, snapshotted so a provider cannot mutate the call:

| Key | Meaning |
|---|---|
| `:tool-name` | The tool being called |
| `:args` | The normalized arguments |
| `:effects` | The resolved effect descriptors |
| `:decision` | The final decision, which is `ask` whenever a provider is consulted at all: a refusal decided before this point never reaches an approver |
| `:attendance` | `attended` or `unattended` |
| `:policy`, `:policy-version` | The named policy and its version |
| `:execution-id`, `:request-hash` | This call's identity and the hash an approval is bound to |

**What a provider returns.** Non-nil approves this exact request; nil denies it. A `quit` during approval is recorded as its own interruption and cancels the call rather than denying it.

**How the answer is consumed.**

- `allow` runs the body, but only after every other constraint already held: a decision that was `deny` never reaches a provider, because the `ask` branch is what consults one. **An approval therefore cannot override a policy denial** -- there is no path from an approval to a call the policy refused.
- `deny` refuses the call and the body does not run.
- `absent` refuses the call with the no-approver refusal.
- An approval is bound to `:request-hash`, which covers the tool, its normalized arguments, its effects and the policy version. A rewritten argument list produces a different hash, so an approval taken for one call cannot be reused for another; a provider that wants to reuse a decision must re-answer for the new request.
- **A selected approver's refusal is a result, not an absent source.** Once the answerer has answered, the call is refused and nothing goes looking for a second answerer willing to approve.
- **Authority chooses who answers; the answerer decides what to consult.** A plugin's channel, the resource ranges it has already been granted and any fallback it wants are composed inside the function Authority selected, by that function. They are not sources Authority ranks against one another, and there is no general rule that a registration Scope outranks the request-local value.

Both outcomes append `approval.decided` before the call proceeds or is refused, so the Tape states which source answered and what it said.

### Executing through the pipeline

`superchat-tool-execute TOOL-NAME ORIGINAL-FN ARGS &key async callback context` is the one seam. `CONTEXT` must carry `:session-id` and an explicit `:attendance`; the execution identity the Tape audit checks travels with the call from Runtime's accepted execution context, not from this argument.

The stages run in this order:

| Stage | What it receives | What it may change | What it hands on |
|---|---|---|---|
| **pre-policy** | The arguments and the context | An `intercept` contributor may rewrite or veto the arguments; effects are resolved from the result | A block reason when a check refuses, and the candidate that goes on to be decided |
| **approval** | The decision, the effects and the policy version | Nothing but the approval answer | `allow`, or a refusal |
| **monotonic guards** | Every named layer's answer and the built-in policy's | A layer may tighten; `allow` from a layer is read as `defer` | The effective policy decision |
| **around** | The effective policy decision, plus the agent loop's permission and confirmation contributions and the session allowlist | A contributor may tighten; confirmation adds a requirement and nothing loosens | The final decision, with `defer` resolved to `ask` when attended and to `deny` when not, and a `deny` from any contributor taken as final |
| **body** | The normalized arguments | Effects, through `ORIGINAL-FN` | The result, or a signalled condition |
| **post** | The result | A `result-filter` contributor may redact or replace it | The delivered result |
| **finalize** | Every branch's outcome | Nothing | Delivery through the exact Sink or the refusal path |

The pre-policy stage and the guards after approval are not the same check done twice, and one does not replace the other. Pre-policy decides whether there is a well-formed call to ask about at all: it rewrites or vetoes arguments, resolves effects, and blocks a candidate whose execution identity or evidence origin is already unusable, so a call that cannot be recorded is never offered for approval. The guards after approval are what an approval cannot loosen. A refusal known before approval short-circuits the approval round, but that is an outcome of the order -- it does not move the guards earlier, and **no stage after approval may widen the decision**, so an approver cannot authorize a call the effective policy denies.

`tool.requested` is appended once the decision is well formed and before the branch that acts on it, so a refused call still carries that row.

The refusals a consumer can receive are ordinary strings: a denial is `[Tool call denied by authority policy]`, a blocked call names its reason, an unapprovable call reports that no approval provider is available, and a call whose audit cannot be recorded is blocked instead of running.

### Cancellation, reentrancy and late results

This interface inherits the request lifecycle and ADR-0006 rather than restating them; what follows is what that inheritance obliges this pipeline to do:

- **Running identity comes from Runtime.** An approval callback cannot reopen a terminal or retired Run: a call whose Run is no longer active refuses instead of proceeding, and the identity that decides this is the accepted execution context, not anything a provider returns.
- **An approval return cannot rebind the call.** The installation was fixed when the descriptor was materialized, so a same-name tool withdrawn and re-registered during approval cannot be executed by the pending call.
- **An asynchronous refusal is delivered once, with its original condition.** ADR-0006's refusal path carries the consumer's own error or quit rather than reclassifying it as an audit failure and delivering a second time.
- **A genuine late external result records its match and stops there.** It may append the one matching fact for an Effect that was already open; it cannot restore model execution or revive the Run.
- **A tool result callback is not a Run terminator.** Runtime alone completes a Run; the pipeline's callback delivers a tool result and never becomes a second completer.

### Tape event types

This component defines the pipeline's event table; no other component restates it.

| Type | Meaning |
|---|---|
| `tool.requested` | The call entered the audit. This records that a request was made, **not** that it will run: a refused call still has this row |
| `approval.requested`, `approval.decided` | The approval round, with its effects, policy version, source and answer |
| `tool.execution.started` | The body is about to run |
| `tool.completed` | The body returned |
| `tool.failed` | The call was refused, blocked, or the body signalled, with its disposition |

### Absent dependencies

Without the `tape` plugin there is no audit, and a call is blocked rather than executed without its evidence. Without `tools-builtin` no built-in tool has an effect resolver, so every built-in call resolves to `unknown` and, attended, asks; unattended, it denies.

## Understand the implementation

<details><summary>Implementation internals</summary>

Three registries hold the declarations -- `authority-policy`, `authority-effect-resolver` and `authority-approval-preview` -- and all three live in Scope layers, so a registration is visible through the Scope chain rather than globally. `superchat-authority--scoped-values` collects a key from every layer in the chain instead of taking the nearest, which is what lets policies combine monotonically.

A decision is composed in `superchat-authority--effective-decision`, and the order of its `cond` is the safety property: unattended side effects deny first, then a policy or legacy denial, then a confirmation requirement. `superchat-authority--policy-decision` builds the policy half by combining the built-in policy's answer with every registered layer's, mapping a layer's `allow` to `defer` on the way in.

`superchat-tool-execute` is the seam. Its stages are marked in the source, and a refusal and a result leave through the same delivery path, so a consumer cannot tell them apart by shape.

| Section | Lines | Role |
|---|---|---|
| Timer owner | 48–60 | The Run's transport clock, with its absent case |
| Built-in policies | 117–120 | The five names that cannot be replaced |
| Scope registries | 155–226 | Register, unregister and collect across the chain |
| Decision ranks and normalization | 228–330 | Attendance checking, policy normalization, argument normalization |
| Effects | 371–457 | Effect descriptors, resolvers and their lookup |
| Approval | 461–495 | The provider over three sources and the refusal text |
| Base decision | 510–593 | `resolve-effects`, `decide` and the effect-to-decision table |
| Approval hash | 595–620 | The canonical binding an approval is tied to |
| Agent-loop contributions | 622–711 | The declared seams and their absent-case answers |
| Effect classification | 713–808 | Read-only, side-effect, sensitive and workspace classification |
| Policy composition | 810–946 | `--policy-decision` and `--effective-decision` |
| Evidence and events | 948–1108 | Tape appends, evidence origin, result fingerprinting |
| Execution | 1110–2093 | The pipeline, its refusals and its delivery |
| Plugin | 2095–2107 | The `authority` Service table |

</details>

## Further exploration

- [superchat-tools](superchat-tools.md) — the registry whose names this component decides on; prerequisite.
- [ADR-0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md) — why a missing trusted wrapper fails closed.
- [ADR-0006](../adr/0006-authority-refusal-consumer-delivery.md) — why a refusal reaches the consumer as a result does.
- [plugin-architecture.md](../design/plugin-architecture.md) — Scope chains and why policies are collected rather than shadowed.

## Model experience

Indirectly: this component decides which calls the model may make, and writes the text of a refusal into the conversation the model reads.

#### What the model sees

A refusal is delivered into the tool-result slot the model reads, so a denied call is visible to the model as its tool's answer rather than as silence. The approval path is visible to a human, not to the model.

#### Token effect

The pipeline adds no material of its own; a refusal's text and the tool results it delivers cost what their strings cost in the tool-result slot.

#### KV cache effect

Indirectly: a refusal or a tool result is appended to the conversation, so it extends the request from that point on. Whether a provider reuses a cached prefix is the provider's own behaviour and is not something this component can promise; what it guarantees is that the appended material is deterministic for the same call and outcome.

## Known limitations

- The base decision entry point is policy-blind, so `superchat-authority-decide` answers the same for every policy. A caller that needs the policy's effect must go through the execution path; reading `decide` alone under-reports what the policy will do.
- A registered effect resolver without a matching tool makes a name selectable that has no function behind it. Authority refuses the call, but the name appears in [`superchat-tools`](superchat-tools.md)'s descriptor list.
- Policy layers combine across the whole Scope chain and cannot be shadowed, so a child Scope cannot opt out of an ancestor's policy -- which is the intended direction, and also means an over-broad ancestor policy can only be tightened, never exempted.
- The four agent-loop seams are read by name from a `defconst` table; a contributor that renames its function without updating the table changes the absent-case answer instead of failing loudly.
- `human-interaction` effects deny unconditionally, so a trusted-input path that needs a different disposition must be built outside this fold rather than by adding a resolver.

### Dev note

<details><summary>Working context for maintainers</summary>

The seam table's absent-case answers are the part most likely to drift: they are declared, so a missing contributor reads as a deliberate answer rather than an accident, but nothing checks that a declared answer still matches what the contributor would do.

</details>
