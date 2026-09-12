# Request and Run lifecycle

This is the implemented cross-module contract for submission, cancellation and delivery. The [architecture map](../architecture.md) shows the call path; the [composition contract](../design/plugin-architecture.md) defines execution Scope lookup. The earlier [fixed-pipeline specification](../design/turn-pipeline-contract.md) is historical, not a second current contract.

## Submission and preparation

A public producer calls `(superchat-service-call scope 'driver 'submit envelope)`. An Envelope must contain nonempty string `:content`, a registered symbol `:surface` and a valid `:sink`. It may carry `:session-id`, `:tools`, `:media`, owner-specific `:context`, model and execution metadata. Validation and the accepted field forms are implemented by `superchat-pipeline-validate-envelope`.

Top-level legacy buffer-routing, callback and prompt keys are rejected. `:runtime-extras` is a compatibility escape for admitted metadata, not a way to replace the Sink or bypass forbidden routing keys. Producers use the exact host object where a host is needed; a buffer name is not a delivery locator.

`parse` acts on a chat preparation Turn before the Envelope. The driver then resolves the Session, admits input, loads state, selects tools, collects fragments and assembles a prompt. A required collect contributor's execution or validation failure terminates preparation rather than silently omitting required input. Runtime, not the preparation Turn, owns acceptance and execution completion.

The assembler produces one prompt and its receipt. Prepared producer text follows the prepared collector, rather than a second public submission API. Preserving prepared text does not prove complete prompt reconstruction: the receipt-append Invariant and provider evidence do not perform an independent end-to-end comparison.

## Execution and tools

Runtime is the single owner of public Run status, model continuation, steering, parent/child admission and terminal handling. The model provider is the injected `model-transport` Service; unavailable transport is not permission to use a hidden LLM fallback.

Every model-invoked tool crosses Authority, including request-local wrappers and internal prepared Subagent tools. Ordinary nonempty tool surfaces without a trusted wrapper fail before provider work. Exact invocation identity, current liveness, Effect resolution, policy and approval constrain execution; a tool list cached by the model does not grant execution rights. See [wrapper admission](../adr/0005-fail-closed-ordinary-llm-tool-admission.md) and [refusal delivery](../adr/0006-authority-refusal-consumer-delivery.md).

Approval callbacks and observers can reenter Emacs. They cannot reopen a terminal or retired Run, admit work to an unavailable parent, or select a replacement installation for an already materialized tool. Effects already entered outside Emacs are not generically revocable.

## Cancellation and terminal truth

Runtime cancellation prohibits continuation before callbacks can resume model or tool work. Transport withdrawal retires the matching provider epoch. Preset retirement follows exact Scope ancestry, not a Preset name or Session-wide buffer search. Only the relevant owner combines Runtime retirement with plugin disposal.

When the trace can prove cancellation, Runtime records `run.cancelled`. If an external Effect or required audit obligation is unresolved, the Run can terminate with status `audit-incomplete`; no fake tool completion or approval settlement is appended. A genuine late tool result may append its one completion fact for the existing Effect. The Run remains terminal and cannot start another provider request.

Run and Turn appends carry stable event identities across SQLite insertion and receipt handling. A confirmed Run terminal survives a lost receipt or later delivery failure; it is not replaced by a competing audit terminal. Recovery can append only a causally matching missing Turn suffix when readable evidence supports it. An unreadable lookup is `unknown`, not proof of absence and not permission to invent a terminal pair.

Closure interprets the evidence; it is not another lifecycle writer or state store. Its current handoff and delivery consumers retain their proof requirements. Re-hosting Closure as an official plugin does not, by itself, remove those requirements.

## Sink delivery and synchronous adapters

A Run retains the exact accepted Sink. Streaming, status and terminal callbacks belong to that Run and execute in its original execution Scope. Public Run status and the View's display status are different interfaces; a UI label is not another Runtime terminal.

A dead target or callback failure cannot redirect output to another buffer. Terminal handling uses one delivery owner and quiesces; reentrant or late callbacks do not produce a second terminal outcome. Consumer errors and quits retain the applicable propagation contract rather than being relabelled as new model outcomes.

`driver.submit-blocking` is reserved for Org Babel and Magit, whose host interfaces demand a synchronous result. These adapters use the same Envelope pipeline and accepted Sink. Timeout or caller quit cancels only the adapter's accepted Run; a queued request whose blocking capture has expired cannot execute later through it. Other integrations are asynchronous.

## Queued and steered input

Runtime owns the in-process Session FIFO and live Run steering. Queue audit facts are durable; queued closures and borrowed Scope references are not a restartable queue. There is no second plugin queue for transport suspension or Scope retirement.

- A queued item retains its original Envelope, Sink and borrowed execution Scope.
- Transport absence pauses dispatch without busy retry or pretending the item was dropped; provider restoration resumes eligible input.
- A retired Scope makes new enqueues non-accepted. Already queued items are auditably dropped as `scope-retired`, never retargeted to global or the next Preset.
- A required queue/drop append failure cannot make the item execute without its evidence. An inert retained item can be handled by a later scheduling attempt.
- Rejected queueing leaves the producer's draft available. Steering targets an eligible live Run rather than allocating a replacement Run.

Any future inbox must preserve these observable distinctions before replacing this implementation.

## Editor resources and source changes

Runtime and registration Scope cleanup borrow editor resources; neither acquires general buffer-close authority. The [Session EditorWorkspace close protocol](session-editor-workspace-close.md) validates ownership on the exact live object. User buffers, files and Artifact buffers are not owned merely because a Run uses them.

Inline rewrite keeps previews outside source text. Accepting a Change Set applies edits atomically and appends `edit.applied`; reject and cancellation do not apply the preview. This does not prohibit an independently authorized editing tool from changing a Resource through its own Authority-gated operation.

## Verification

Use [the canonical checks](../agents/development.md). R1–R9 cover withdrawal, submission, registration identity, terminal recovery and scoped lifecycle contracts alongside the product smoke journeys. A new architecture must preserve those contracts or name and obtain approval for a deliberate behavior change; passing an older static baseline alone does not prove a new interface.
