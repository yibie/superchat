# Decision: One tool pipeline with Authority as its only decision point

Status: accepted

## Problem

Every tool call already crosses Authority and already passes one function, `superchat-tool-execute` (`superchat-authority.el`), which normalizes context, applies a block check, resolves effects, asks `superchat-authority-decide`, checks `tool-execution-passes-authority`, resolves `defer`, classifies the request, rechecks that the execution is still active, appends `tool.requested`, and only then branches to refuse, deny or ask before running a body. What M2 adds is not a pipeline where none exists, and this record states the three properties it does deliver before it states what changes.

Three properties already hold, and the refactor must keep them:

- **A contributed `allow` cannot relax an inherited `deny`.** `superchat-authority--policy-decision` composes the built-in policy decision with every registered named policy and clamps a named layer's `allow` to `defer`; `superchat-authority--effective-decision` then orders a policy `deny` and `ask` ahead of the legacy, sensitive-read, allow-list and session-allowlist branches. Monotonicity is delivered, not pending.
- **A missing approval provider already fails closed.** When no `:approval-function` is present and `superchat-authority-approval-function` is not callable, `approved` is nil, `approval.decided` records `deny`, and the `refuse` branch runs without the body. The mechanism is correct.
- **A denial already reaches the consumer, and the body does not run.** `refuse` carries the result for the block, deny and cancelled outcomes (ADR-0006).

What does not hold is the shape, and each item below is a defect this record bounds.

- **The pipeline is a function, not an order.** Pre-policy, the decision, the active-execution recheck and the request record are `let*` bindings in one body. Their order is real but unnamed, so moving one binding changes the order silently and no review can see it.
- **Approval has no declared provider.** It is a `defcustom` plus a request-local plist key plus a per-plugin scope table, and `superchat-authority-register-approval-preview` registers only preview text. There is no single place that answers "who approves this, and what is the answer when nobody can", so the absent case is expressed as a nil that each branch interprets.
- **The absent case is misreported to the consumer.** With no approval provider the run records `approval.decided :decision "deny"` and then refuses with `"[Tool call cancelled by user]"`. "Nobody could approve" reaches the user as "you cancelled", which is exactly the distinction ADR-0006 requires a refusal to carry.
- **A denial crosses the pipeline, and its sequence differs from an approval's.** A denied `read_buffer` and a denied `edit_buffer` both reach `superchat-tool-execute`, both record `tool.requested` followed by `tool.failed`, both leave the body unrun, and both deliver `"[Tool call denied by authority policy]"` into the model's tool-result slot on the next request. An approved call records `tool.requested`, `tool.execution.started` and `tool.completed` instead. The denied path's difference from the approved one is therefore the contract the acceptance below pins, and it holds for a read and a write alike.

- **Authority reaches the agent loop by presence probe.** `superchat-authority.el` guards `superchat--agent-intercept-tool-call`, `superchat--agent-check-permission`, `superchat--agent-confirm-p`, `superchat--agent-filter-tool-result` and `superchat--extend-timeout` with `fboundp`, and declares them without requiring their file. Authority cannot distinguish a contributor that declined to object from one that is not there, and `superchat--agent-make-wrapper` is probed the same way from `superchat-llm.el`, where absence fails closed (ADR-0005).

The four removal candidates the 2026-09-12 audit listed were re-confirmed against this tree. All four are still present, and two were mis-described by the audit, so the corrected forms are what this record bounds:

| Candidate | Re-checked finding |
|---|---|
| a duplicated default tool list | Present. `superchat--select-tools-baseline-names` (`superchat-select-tools.el`) and the default value of `superchat-llm-tool-names` (`superchat-tools.el`) hold the same five names in different order; `superchat-tools--builtin-tool-names` is a third, larger list |
| a second tool-selection pass on chat input | Present in a narrower form than described. The decision is not recomputed: the pipeline's `:tool-decision` reaches the LLM layer and is honored. The redundancy is a second registry query in `superchat--collect-llm-tools`, which its own docstring calls a no-op in the honest case and a real restriction only when the decision and the query have diverged |
| `fboundp` probes between Authority and the agent-loop wrappers | Present, seven probes. `superchat--extend-timeout` is defined (`superchat.el`), so the probes are load-order tolerance, not dead code |
| the `on-demand` selection alias | Present and inert by its own documentation: it is kept for saved custom values and behaves exactly like `always` |

## Decision

The user accepts this record. The user's word, given after reviewing it: "接受".

## Design

**Name the order, declare the approval provider, give absence a consequence.**

The pipeline order becomes the contract, in this order, with each step's absence defined:

    pre-policy → approval → monotonic guards (deny-only) → around → body → post → finalize

- **pre-policy** normalizes the request: tool name, arguments, attendance, effects, context. It cannot decide. The block check, the effect resolution and the active-execution recheck belong to it.
- **approval** becomes a declared provider on the Authority service, with today's `defcustom`, request-local key and plugin scope table read through it. Its absent answer is the deny that already happens; what changes is that the seam states the deny, and the refusal it produces says that no approver was available rather than that the user cancelled.
- **monotonic guards** are the named Authority policies. The contract states what `superchat-authority--policy-decision` already does — a guard returns `deny`, `ask` or `nil`, an `allow` is read as no tightening, and `superchat-authority-compose-decisions`' ranking makes relaxation impossible — and the existing behaviour becomes a gate on the refactor rather than a change in it.
- **around** is the interception and permission contribution the agent loop supplies today through probes. It moves to one declared seam whose absent case is written down: the call proceeds where the contributor could only object, and is refused where the contributor was the only thing that could authorize.
- **body** runs only after the decision is known and well-formed (invariant 3, unchanged).
- **post** is result filtering and rendering.
- **finalize** is accounting, the terminal event and delivery to the exact Sink.

**The duplicated default list collapses to one source.** One of the two five-name lists becomes the definition and the other reads it. Which one is the definition is an implementation choice this record leaves open, with the constraint that the surviving list keeps the default surface exactly (the five verbs, `apropos` included), so no tool appears in or disappears from the default set.

**The second registry query is either removed or made to state its purpose.** If the decision already names the tools, the query re-derives what the decision holds. The implementation either drops it or keeps it with a reason that survives review; what it must not do is keep it while its own docstring calls it a no-op.

## Alternatives considered

**Leave the probes and document them.** The probes are deliberate load-order tolerance and the system works. It loses because presence probing cannot express the absent case, which is what the milestone asks one level down: what a contributed `allow` may do to an inherited `deny`, and what a call may do when its only possible authorizer is not mounted.

**Reuse `superchat-authority-register-approval-preview` as the approval seam.** The service table already carries an `authority-approval-preview` key, so approval could be declared without a new mechanism. It loses because that registry holds presentation text for one tool name, while approval needs a per-request answer with a request hash; widening the preview registry would make one name mean two things.

**Rewrite the `cond` in `superchat-authority--effective-decision` as an ordered guard list without changing what it reads.** This keeps behaviour identical and makes the order visible. It loses as the whole answer because the milestone's difficulty is not the `cond`'s readability but that approval has no declared provider and absence has no stated consequence; reordering the same reads leaves both.

**Make every named policy a full decision participant.** Letting a policy return `allow` as well as `deny` would make policies symmetrical with the built-in decision. It loses because it gives up the property `superchat-authority--policy-decision` already provides: monotonicity.

**Delete the second registry query and trust the decision.** Fewer moving parts, and the decision is already carried on the request. It loses as the whole answer because the query also decides what the surface contains, and a prepared path may legitimately hold tools the decision did not name; removing it without that distinction risks widening a prepared surface.

**Keep the `on-demand` alias for saved values.** Removing an accepted value from a `defcustom` breaks a user's saved configuration. It loses because the alias is documented as identical to `always`, so the value carries no behaviour to preserve; the record keeps the alias until a decision removes it from the accepted set, and this record does not remove it.

## Consequences

The acceptance this decision is measured by lives in [the roadmap](../../roadmap.md#milestones); the record states what the change costs and what it must not break.

- **The pipeline's order becomes reviewable, and `superchat-tool-execute` is the seam every tool crosses.** A mistake in it is not local, so each step keeps its existing behaviour until the step that replaces it is verified, and the canonical smoke set plus the denied-write check are the gate.
- **The refusal wording becomes observable.** The absent-provider case stops reporting "[Tool call cancelled by user]". That string appears only where it is produced and nowhere in the smoke expectations, so the change reaches a consumer without moving a baseline; a consumer outside this tree that matches on it sees new text.
- **The monotonic rule bounds direction, not judgement.** A guard that returns `deny` for a reason the user did not expect is indistinguishable from one that returns it correctly; the audit trail keeps carrying policy and policy version per decision.
- **The second registry query may be load-bearing on a prepared path.** Removing it is safe only once the prepared and ordinary surfaces are shown to differ in a way the record names; until then the query stays and the reason is written down.
- **The approval provider is a declaration over three existing sources.** The request-local key keeps winning over the default, and the plugin scope table keeps winning over both where it applies; a declaration that shadows one of them changes behaviour.
- **`on-demand` stays.** The record does not remove the alias from the accepted set, and nothing in the default tool surface changes: the same five baseline names remain.
