# Decision: One execution main line

Status: proposed

> Historical design input: superseded by the [accepted leader resolution](../accepted/2026-09-14-m3-execution-main-line-resolution.md). Its recommendations and open questions are not implementation gates.

## Problem

The user has decided to retire the fixed Stage registry ([retire-fixed-stages](../accepted/2026-09-12-retire-fixed-stages.md)) and the blueprint record already names an agent module with an inbox, turn and step bracketing, and interception points ([spine components](../proposed/2026-09-12-restructure-on-deepseek-harness.md)). What is not designed is the mechanism: the API, the combination rules, and the lifetime it implies.

Today a turn's work is spread across four unrelated mechanisms, and nothing states what work an agent holds or what happens to it between steps:

- **Eight positional Stages with four combination rules.** `parse` (`fold`), `resolve-session`, `admit`, `select-tools`, `assemble-prompt` (`first-result`), `load-state`, `collect-context`, `save-state` (`collect`). A plugin that wants to intercept a turn must first work out which owner runs the Stage it cares about.
- **A per-Session process-local FIFO** in Runtime, with three handled outcomes: dispatch, drop with a reason, or pause.
- **A per-Run steer queue** in Runtime, drained by the model loop.
- **Runtime notifications** as a separate Event surface.

Two consequences follow, and they are the reason this is a milestone rather than a cleanup. First, "send while running" is not one behaviour: a chat follow-up becomes a queued item, an instruction to a live Run becomes a steered queue entry, and neither mechanism knows about the other's lifetime rules. Second, there is no serial point at which a stop request is observed, so "stop this turn" and "cancel this Run" are the same operation, and a caller that wants the first has to accept the second.

## Proposal

One execution loop per agent handle. A Runtime Run becomes the loop's active turn; the loop owns three things and nothing else does.

### The handle and its inbox

An agent handle is the process-local thing a producer submits to. Its inbox holds items of three classes, and the class is the wake rule:

| Class | Meaning | Wake |
|---|---|---|
| `followup` | A next turn: new input for the same Session | Wakes the loop to start a turn when the loop is idle |
| `steer` | An instruction for the *next step* of the running turn | Wakes the loop if it is waiting; otherwise it waits for the next step boundary. Applies to the turn that was running when it was accepted, never to a later one |
| `inject` | Material without an instruction: context for whatever happens next | Does not wake. Consumed by the next step that builds a request |

An item belongs to a **Session** by its identity and to a **turn** by the moment it is consumed. Only a `followup` can create a turn; `steer` and `inject` never do, and an item that no turn consumes is not a turn waiting to happen.

**Enqueueing is not acceptance.** `driver submit` stays the public submission path and translates to an inbox item; the item is *pending* until the loop takes it up as the head of a turn. Only then does the Run exist and the accepted facts land. This is the distinction the current FIFO already makes by returning `(:accepted nil :reason …)` instead of a queue-id, and it becomes the inbox's own statement rather than an accident of two mechanisms.

### What replaces the eight Stages

The Stage registry and its positional order are gone. Their work is subsumed by the loop's own phases, and the extension surface is three domains: durable session events (Tape facts), live loop events, and capability-seam events.

| Phase | What it does | Replaces |
|---|---|---|
| **Turn setup** | Derive the input's Session and turn identity; decide whether this item starts a turn | `parse`, `resolve-session`, `admit` |
| **Step preparation** | Gather state and context; build the candidate request | `load-state`, `collect-context`, `assemble-prompt`, `select-tools` |
| **Request waterfall** | The one interception point for the candidate request: contributors may rewrite it, and one may refuse it | the Stage `first-result`/`fold` over these names |
| **Model transport** | One provider attempt, through the `model-transport` Service | unchanged |
| **Tool rounds** | Every tool call crosses Authority's pipeline | unchanged |
| **Turn seal** | Record the turn's terminal fact, run `save-state` contributors, deliver through the exact Sink, then quiesce | `save-state`, the seal in Runtime |

Two extension points remain where the Stage registry had one, and they are the blueprint's:

- **A request waterfall** (the blueprint's "request waterfall"). Combination rule: a candidate is threaded through every contributor, and any contributor may refuse it. This is where request rewriting lives.
- **A serial turn-stopping hook** (the blueprint's "turn-stopping serial"). Every listener runs, in order, at each step boundary, and any may request a stop. Serial because a stop decision must not depend on which listener ran first.

### Stop, seal, quiesce and retire are four different things

This is the separation the mechanism exists to make explicit, and each verb has one owner:

| Verb | What it means | What it must not do |
|---|---|---|
| **Stop request** | "Finish the turn in progress, start no new work." Observed at the next step boundary by the serial hook | Interrupt a step in flight, or drop already-accepted work silently |
| **Seal** | Append the one terminal fact for the turn, having already run the state contributors | Be skipped, repeated, or produced by anything other than the loop |
| **Quiesce** | Release the Run's borrowed execution Scope and the callback reachability Runtime held for it | Dispose the caller's Scope, close a buffer, or retract an external Effect already entered |
| **Retire / dispose** | `retire-scope` prohibits admission and continuation through one Scope chain; the owner then disposes registrations | Act as a Run cancellation authority on its own, per the [composition contract](../../design/plugin-architecture.md) |

The delivered order is unchanged: seal and deliver first, quiesce second, and only then may the owner dispose. A stop request and a cancellation are different inputs to this machine: the first ends the turn normally and seals it; the second is a terminal fact of its own (`run.cancelled`), and a turn whose external Effect is still unresolved seals `audit-incomplete` with the Effect left open, exactly as delivered.

### The queue semantics move; they are not replaced

The [request lifecycle](../../protocols/request-lifecycle.md) fixes the observable distinctions the current FIFO provides, and its last clause is binding on this record: *any future inbox must preserve these observable distinctions before replacing this implementation*. Each becomes an inbox rule with its own fact:

| Distinction (delivered) | Inbox rule | Fact |
|---|---|---|
| A queued item retains its original Envelope, Sink and borrowed Scope | The item holds them; nothing re-derives them at dispatch | — |
| Queued while a turn runs | A `followup` arriving during an active turn waits without disturbing it | `queue.queued` |
| Dispatch | The item becomes the head of a turn | `queue.dispatched` (with the Run it became) |
| Transport absence pauses, without busy retry and without pretending a drop | The head stays pending; provider restoration resumes it | — (no fact; absence is not an outcome) |
| A retired Scope makes new enqueues non-accepted | The inbox returns not-accepted before any fact | — (the caller keeps its draft) |
| Already-queued items in a retired Scope drop as `scope-retired`, never retargeted to global or the next Preset | Same reason, same rule | `queue.dropped` `:reason "scope-retired"` |
| A dead Sink drops the head | Same | `queue.dropped` `:reason "sink-dead"` |
| A required append failure must not let the item execute without its evidence | Facts land before the inbox state changes; a failed append leaves the item where it was rather than loses it | — |
| Steering targets an eligible live Run, never allocating a replacement | A `steer` against a terminal or non-effect-capable turn is refused and recorded | `run.steer-undelivered` |

## Turn and step brackets on the Tape

**The turn bracket adds no fact.** `run.started` already carries the turn identity, and every terminal fact carries it too, so a turn is identified by what is already recorded. This is why the canonical sequences can stay untouched for the turn half.

**The step bracket does not exist today.** No row in the tree carries a step identity, and the acceptance in the roadmap asks for Tape sequences unchanged from the M3 baseline. Those two cannot both hold if a step bracket is written as new rows. This record does not decide it by wording; it states the options and asks.

| Option | What a step is | Baseline | Cost |
|---|---|---|---|
| **A. Derive, add no fact** (recommended) | The span of one provider attempt, replayed from the existing `request.header` and `request.message-list` rows, which already carry a request identity | Unchanged | No row states the step's own boundary; a reader must know the derivation, and a step that never reached a provider has no rows |
| **B. Explicit rows** | A new pair appended around every step | Changes: every canonical case gains rows | The bracket is explicit and reconstructable, and the baseline change is an accepted, documented edit rather than a discovery during implementation |
| **C. Live only** | A process-local span the loop uses for `steer` and `inject` targeting, with no Tape row | Unchanged | Fails the blueprint's "step bracketing on the log": step boundaries are not durable, so a steered instruction's target step cannot be reconstructed after a restart |

The recommendation is **A**, because it satisfies the blueprint's purpose -- a `steer` names the step it applies to, and that step is reconstructable -- without spending a baseline change on a fact that is already derivable. It is a recommendation, not a decision this record may take alone: see the open questions.

## Responsibility migration

Every row refers to behaviour that exists today; nothing here is invented.

| Today | Owner today | Moves to | Note |
|---|---|---|---|
| `parse` (`fold`) | Stage, invoked by the chat dispatcher over a preparation Turn | Turn setup | Stays chat-specific; the loop does not parse chat syntax |
| `resolve-session`, `admit` (`first-result`) | Stage, invoked by `superchat-pipeline-submit` | The loop's turn setup | Admission becomes the inbox's own decision |
| `load-state`, `collect-context` (`collect`) | Stages | Step preparation | Same contributors, called at a different point |
| `select-tools`, `assemble-prompt` (`first-result`) | Stages | Step preparation, ending at the request waterfall | The request waterfall is where a contributor rewrites the candidate |
| `save-state` (`collect`) | Stage, invoked by Runtime | Turn seal | Same contributors, called by the loop |
| Per-Session FIFO | Runtime | The inbox | Rules preserved row for row above |
| Per-Run steer queue | Runtime | The inbox | A `steer` item targets the running turn's next step |
| `superchat-runtime-cancel` | Runtime | Unchanged | Stays the cancellation authority; stop is a new, separate input |
| `superchat-runtime-retire-scope` | Runtime | Unchanged | Its contract does not change; it composes with the loop's seal and quiesce |
| Quiesce and borrowed-Scope release | Runtime | The loop, under Runtime's existing rules | The order seal → deliver → quiesce → dispose is not changed |

## Expected outcomes for the key scenarios

Each row is a completion condition for the implementation, not a claim about today.

| Scenario | Expected |
|---|---|
| `followup` while idle | A turn starts; `queue.queued` then `queue.dispatched` then the run's facts, in that order |
| `followup` while running | The item waits; the running turn's facts are uninterrupted; on the running turn's terminal fact the item becomes the head of the next turn |
| `steer` while running, before the next step | The instruction reaches the next request; `run.steered` records acceptance |
| `steer` after the turn became terminal, or against a non-effect-capable turn | Refused, and `run.steer-undelivered` records it; no new Run is allocated |
| `inject` while running | No wake; the next request that is built includes it. If no further step happens, nothing consumes it |
| `inject` while idle | Held, not a turn. It is consumed by the next turn's first step |
| Transport withdrawn with a head pending | The head stays pending, no retry loop, no drop fact; restoration resumes it |
| Scope retired with items queued | New enqueues are refused with no fact; already-queued items drop as `scope-retired`; none is retargeted |
| Stop requested during a step | The step completes; the serial hook observes the request at the next boundary; no new step starts; the turn seals normally and delivers |
| Stop requested while waiting on a provider | The loop does not interrupt the attempt in flight. Whether this stops before or after the current attempt is a decision below |
| Cancelled turn | `run.cancelled` seals once; late provider and tool callbacks cannot reopen or continue it |
| Turn whose external Effect is unresolved at cancellation | `audit-incomplete` seals with the Effect open; a genuine late result records its one matching fact and does not resume execution |
| Callback arriving after quiesce | Refused for lack of an active Run, per the [plugin architecture](../../design/plugin-architecture.md), rather than delivering or sealing again |
| Slow or signalled tool callback | A signalled tool callback records its failure once (`tool.failed`, as the denied-write evidence already shows) and cannot produce a second terminal fact, because the seal is the loop's and not a callback's |
| Two concurrent producers on one Session | Their items are ordered by the inbox; no second queue exists, so no second ordering exists either |

## Alternatives considered

**Keep the Stage registry and rename its names.** Rejected by the accepted record: the dispersed ownership survives, and the registry stays one of several mechanisms a plugin must choose between.

**Keep the FIFO and add an inbox beside it.** Rejected: two mechanisms would own "what work does this Session have", which is the defect this milestone exists to remove, and the request lifecycle already requires one.

**Make the inbox a new Service rather than the loop's own state.** Rejected as the starting point: a Service would make the inbox a provider other code can replace, but no second provider is wanted, and the queue's semantics are bound to the Run lifetime the loop already owns. A Service can be introduced later without changing the rules.

**Model `stop` as a second cancellation.** Rejected: cancellation is a terminal fact about a Run, while a stop request is an input the turn survives long enough to seal normally. Conflating them is what forces a caller to accept a cancellation to end a turn.

**Add explicit step rows now.** Kept as option B above, not rejected. It is the honest way to get a durable step bracket if the derivation in option A is judged too implicit; it is not the recommendation because it spends a baseline change the acceptance criteria otherwise forbid.

## Open questions this record does not decide

1. **Step bracket identity.** Option A (derive from the existing request rows), B (explicit rows, baseline changes), or C (live only)? The recommendation is A.
2. **`inject` durability and consumption.** Is an `inject` item process-local with a Tape fact, or process-local only? Who consumes it when several contributors could, and is it consumed once or visible to every step until the turn ends?
3. **Ordering across classes.** Within one Session, is the inbox strictly first-in-first-out across `followup`, `steer` and `inject`, or do `steer` items pass queued `followup` items because they belong to the running turn?
4. **The waterfall's combination rule.** "Waterfall" appears in three documents and is not in the Event rule set, which has `observe`, `first`, `fold` and `collect`. Does the request waterfall reuse `fold` (threaded, nil keeps the accumulator) or need a distinct rule where a contributor's result replaces rather than threads? Does the serial hook reuse `observe`, or need a rule whose listeners run in a defined order and can stop?
5. **Stop-request latency.** Is a stop observed only at a step boundary -- so a turn waiting on a slow provider finishes that attempt -- or may it also abandon an in-flight attempt? The first is simpler and keeps the seal single; the second needs a rule for an attempt whose result arrives afterwards.
6. **Where the handle lives.** Is the agent handle a first-class object a producer can hold and address, or an internal name for the loop a Session already has? This changes whether `followup`/`steer`/`inject` are new public API or internal verbs behind `driver submit`.

## Acceptance

The [roadmap's M3 acceptance](../../roadmap.md#milestones) governs, with one dependency this record makes explicit: it requires the complete canonical smoke set to pass with Tape sequences unchanged from the M3 baseline. That holds under option A of the step bracket and is a *documented, accepted* baseline change under option B -- it must not be absorbed at implementation time by editing the expected files.

The behavioural assertions are: sending during a Run still queues; cancel still seals `run.cancelled`; a Run with an entered external Effect still terminates `audit-incomplete` with the Effect open; one normal turn and one cancelled turn against a real provider complete with the expected terminal facts, without demanding a fixed number of model events from a non-deterministic model. A static check that a contract "passed" does not replace these.

## Risks

- **The loop becomes the new god object.** The eight Stages were dispersed; one loop is central by construction. Mitigation is that the loop owns sequencing and lifetime only -- context, tools, prompts, transport and delivery keep their own owners and are called by it.
- **A baseline change hides a regression.** If option B is chosen, the new rows must be justified as a fact the design needs, not as a way to make the suite green.
- **Admission semantics drift.** Moving `admit` into the inbox changes who answers "was this accepted"; the four refusal reasons and the fact-before-state rule above are the guard.
- **Two domain words.** "Waterfall" and "serial" are used in the blueprint and the roadmap but are not defined in the Event vocabulary. Leaving them undefined is what would let two implementers build two different rules; open question 4 exists to close that before implementation.
