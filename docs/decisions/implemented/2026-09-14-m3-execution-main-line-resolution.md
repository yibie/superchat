# Decision: M3 execution main line and bounded Tape changes

Status: implemented — leader decision under the user's explicit delegation on 2026-09-14; delivered under M3 and accepted by the user on 2026-09-15

## Problem

The [M3 draft](../proposed/2026-09-13-one-execution-main-line.md) leaves six coupled mechanism choices open and conflates turn-stopping with a new graceful-stop command. The accepted Stage retirement needs one executable direction, not another round of independent option selection.

## Decision

The user delegates the decision to the leader: “别没完没了的了，你是 leader agent，你来做决策”. Under that delegation the leader selects the mechanism below. This records delegated authority, not a claim that the user separately selected every option. It supersedes the M3 draft's recommendations and open questions; the draft is historical design input, not an implementation contract. [The roadmap](../../roadmap.md) owns delivery status and acceptance.

### Ownership and handle

Runtime remains the only owner of Run acceptance, completion, cancellation and quiescence. The loop is Runtime's execution driver, not a second lifecycle authority. Session, Turn and Run retain their [domain meanings](../../../CONTEXT.md); a Step is work inside one Run, not another Run or a synonym for Turn. Child Runs retain their existing identity and admission contracts.

The existing driver Service provides an opaque, process-local handle resolved for a Session. It addresses Runtime-owned scheduling state, exposes neither a queue nor a writable loop, and grants no Authority. Producers retain `driver submit` as the public request submission path; steer/inject operations resolve through the same driver owner, not a new registry or parallel submission service. One Session's top-level scheduling has one inbox owner even when multiple Views refer to it. Each item separately retains its original execution Scope, Sink where applicable, and source identity; a handle does not own those resources. Child execution is not forced into its parent's top-level FIFO.

Chat parsing and sufficient Session routing occur before inbox insertion. Enqueue is not Run acceptance. Only Runtime dispatch of an eligible followup accepts a Request and creates its Turn/Run. An immediate idle submission does not acquire synthetic queue events merely to pass through the same scheduler.

### Inbox targeting, claims and durability

The inbox unifies state and scheduling ownership, not all-category head-of-line blocking.

- `followup` targets the next eligible Turn in its Session. Followups preserve FIFO order. Idle submission wakes dispatch; active execution leaves the item pending.
- `steer` targets the exact live Run supplied at submission and its next eligible Step. It bypasses followups for future Turns. Idle/no-live-target steering is refused rather than starting a Turn; this intentionally preserves Superchat's contract rather than adopting DSH's idle-steer wake behavior. Unconsumed steering cannot migrate to a later Run.
- `inject` carries identified model material, not a new task, and never wakes an idle driver. Injection during a live Run targets that Run's next eligible Step; if that Run ends without consumption, it is discarded rather than silently moved to a later Turn. Idle injection targets the next eligible Turn under its original execution Scope; it cannot attach to a different Preset/Scope. A matching head followup claims it; a nonmatching Turn leaves it pending until its owner retires or a matching Turn arrives.

Within an eligible next-step batch, steer and inject preserve insertion order. The loop alone atomically claims a batch; contributors receive the same claimed batch and never drain the queue independently. Entries arriving after the claim remain pending. Pre-step rejection consumes/discards that claimed proposal with an explicit non-entry outcome; it does not silently retry it. A thrown pre-step failure ends the accepted Run through Runtime's failure path and settles the claim without reinsertion. Input that never enters a Step is not represented as delivered model material. Existing steer-undelivered and queue-drop obligations remain in force.

Pending scheduling is process-local, as the FIFO is; there are no restartable jobs and no serialized closures, Sinks or Scopes. Material admitted by pre-step is durably recorded with its source relationship before provider dispatch. It enters history once, not once per subsequent Step: within a Run a later Step continues the provider prompt that carries it, and later Turns derive it from the Tape through the Context View ([closing ruling 4](2026-09-15-m3-closing-rulings.md)). There is no separate durable inject queue or input-claim event family.

Transport withdrawal pauses pending dispatch without busy retry. Dead Sinks and retired Scopes retain their exact drop reasons. Retirement blocks new admissions before callbacks can reenter, never reroutes work to global, and settles pending items through the existing audited drop paths. Audit errors distinguish an uncommitted append from a committed append with a lost receipt; existing stable event identities prevent duplicate dispatch or facts.

### Two waterfalls and one closing notification

Both waterfalls use ordered, awaited middleware with a single-use `next` continuation, explicit input/output values and explicit refusal. Returning without delegation short-circuits; errors are not interpreted as abstention. Middleware receives the proposal or candidate and its continuation, and cannot replace execution identity. The promise of a cancellation context is withdrawn by [closing ruling 5](2026-09-15-m3-closing-rulings.md): after cancellation Runtime's execution boundaries keep a middleware result from opening a Step or entering the model transport, but middleware is not told of the cancellation and its own work is not stopped. A narrow composition helper implements both waterfalls; there is no general registry and no copied Stage registry.

1. **Pre-step waterfall:** receives the claimed identified message batch and the proposed Run/Step coordinates. Its result is `enter` with the complete identified material batch, or `reject`. The default enters the claimed batch. It may transform admitted material while retaining source relationships; it cannot replace Run, Session, Scope, Sink or tool authorization. A rejection opens no Step. Enter opens the Step and records admitted material before history/request derivation.
2. **Request waterfall:** receives the candidate provider request derived after entry and returns a coherent replacement candidate or explicit refusal. It may alter model-facing request fields, not accepted execution identity, Sink, effective Authority or cancellation ownership. Final validation and [M1 provider request evidence](../implemented/2026-09-13-reconstructable-provider-request.md) apply after rewriting and before dispatch. Ordinary/prepared tool admission retains [ADR-0005](../../adr/0005-fail-closed-ordinary-llm-tool-admission.md); middleware cannot turn raw tools into a trusted prepared path.
3. **Serial turn-stopping:** runs only when a Turn is about to close because no tool continuation or eligible next-step input remains. Runtime awaits the ordered listeners and rechecks the inbox before committing the terminal boundary. Eligible input added during notification continues execution; otherwise Runtime seals. This is not a stop vote or an every-step hook. Listener failure follows the Run failure path, not successful completion.

No graceful-stop public API is added. Immediate interruption uses Runtime cancellation. The [Authority tool pipeline](../implemented/2026-09-13-one-tool-pipeline.md) remains distinct from these model-loop waterfalls and retains its accepted order. [Tools](../../components/superchat-tools.md), [Authority](../../components/superchat-tool-execution.md) and [Session](../../components/superchat-session.md) retain their own responsibilities; migrating collectors does not authorize repeated turn-only side effects on every Step.

### Explicit Step facts: option B

An entered Step has a stable identity within its Run and encompasses one driver-level model attempt plus the tool calls it produces. Provider-internal transport retries remain inside that attempt; a retry that returns to the driver and rebuilds a request enters a new Step. Pre-step rejection opens no Step. Request-waterfall refusal or failure after entry closes that Step with its actual disposition even when no provider request occurs.

Tape gains exactly two lifecycle event types, `step.started` and `step.ended`, carrying Session, Turn, Run and Step identity; the ending states its disposition. Provider request evidence links to the entered Step without replacing request/series identity. Tool facts from an attempt, including concurrent calls, fall inside that Step's bracket and carry no Step identity of their own; child Runs retain their own identities. These rows are structural, not model-visible messages.

Runtime closes an entered Step on success, failure or cancellation before the enclosing Run's terminal sequence, unless durable recording itself cannot be established. A cancellation ending does not assert that an entered external Effect is resolved. Late matching Effect facts remain valid after the Step and Run end but cannot produce another Step end or continue execution. Crash or unreadable evidence may leave an open bracket; recovery never invents success or an unproven end merely to produce a pair.

Turn boundaries reuse existing Run/Turn facts; there is no Turn event family. `request.header` remains a reusable series record, not a Step boundary.

### Termination, delivery and disposal

The [request lifecycle](../../protocols/request-lifecycle.md) and [composition lifetime](../../design/plugin-architecture.md#retirement-withdrawal-and-evidence) govern termination and retirement. Runtime retains terminal append ownership and orders state contributors according to its existing finalization contract; moving Stage registration does not reorder that contract. Required contributor failure cannot be reported as successful completion: the `chat-turn-recorder` observers are the required recording contributors of `run-sealing`, and their failure seals an otherwise completed Run `audit-incomplete` ([closing ruling 1](2026-09-15-m3-closing-rulings.md)).

Provider continuation after termination is prohibited. A genuine late external Effect result may append its one matching fact without reopening the Run. A consumer error/quit is not a tool-body failure: [ADR-0006](../../adr/0006-authority-refusal-consumer-delivery.md) retains the one-attempt refusal-delivery and original-condition propagation rules. Tool callbacks do not become Run terminal Sinks. Quiescence releases borrowed execution references; the owning Preset retires Runtime work before disposing registrations. Neither loop nor Runtime gains buffer-close ownership.

## Alternatives considered

**A: derive Steps solely from provider request rows.** Rejected because the proposed derivation does not describe entered Steps that fail before dispatch or explicitly bracket their tool execution. Avoiding a baseline edit is not sufficient reason to weaken lifecycle evidence.

**C: process-local Steps only.** Rejected because input targeting and entered-step outcomes need durable reconstruction. Process-local pending scheduling is compatible with durable facts for work that actually enters execution.

**One strict FIFO across all message classes.** Rejected because a followup for a future Turn must not block steering for the current Run. One owner with eligibility-based claims provides one coherent ordering without that blockage.

**Use fold/observe names as the complete waterfall/serial design.** Rejected because names alone do not specify awaited delegation, short-circuit, refusal, cancellation or final inbox reread. A narrow helper can implement the explicit protocol without broad Event redesign.

**Add graceful stop and a second cancellation-like path.** Rejected for M3: turn-stopping is a closing notification, and no independent product requirement justifies another public termination operation in this migration.

**Expose mutable loop state or introduce an independently replaceable inbox Service.** Rejected because scheduling and accepted execution must share Runtime ownership. An opaque handle through the existing driver gives addressability without a second authority.

## Consequences

The superseded draft's six questions are not approval gates. Concrete Elisp representations, helper placement and call signatures are implementation choices within these constraints; the [closing rulings](2026-09-15-m3-closing-rulings.md) settle the questions that remained at delivery. M3 includes no M3a extraction and no default-plugin removal.

One bounded baseline exception applies: insertion of the two explicit Step lifecycle event types and assertions for their linkage/dispositions. Existing events retain relative order and established meanings; deleting or reordering an existing event, synthesizing queue events for immediate submissions, or weakening cancellation/refusal assertions is outside this exception. Review both the full new sequence and the old-event subsequence: excluding only the two declared Step types must reproduce the existing sequence. This does not replace checks that every entered Step has correct identity, placement and outcome, including pre-dispatch failure, retries and cancellation. Additional facts beyond this allowance require leader review, not an implementation-local expected-file rewrite. The user separately accepted one assertion replacement in smoke case V (`chat-parser-direct`, `assembler-works-without-its-plugin`), bounded as the roadmap records.

Acceptance rests on the full canonical smoke set including R1–R9 in both readings, the M3 regression set `scripts/m3-regression.el`, and real-provider normal and cancelled turns, as recorded under M3 in the roadmap.
