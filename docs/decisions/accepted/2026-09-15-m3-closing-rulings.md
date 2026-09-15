# Decision: M3 closing rulings — turn recording failure, Stage vocabulary, inbox stores

Status: accepted — user decision on 2026-09-15 ("都按建议") on three leader recommendations; implementation state is in the roadmap

## Problem

Three questions blocked closing M3 in [the roadmap](../../roadmap.md). Each rests on a measurement taken against the tree at commit `92265435`.

The [M3 execution main line record](2026-09-14-m3-execution-main-line-resolution.md) states that "required contributor failure cannot be reported as successful completion" but does not say which `run-sealing` listeners are required. The product registers two: the `chat-turn-recorder` plugin's `record-user-message` and `record-attachments`, which append the user's message and attachment rows to the Tape. A `run-sealing` listener that signals is recorded as a fault in `superchat-event-faults` and the Run seals `completed` (measured with a throwing listener registered on `run-sealing`); a recorder that signals before its append therefore leaves a Run reported as completed whose Tape lacks that row.

The roadmap requires that no Stage name survive M3, while the [request lifecycle protocol](../../protocols/request-lifecycle.md) names the `stage-error` Event. Today the Event `stage-error`, the fault record key `:stage` and the file `superchat-stage.el` (feature `superchat-stage`, ten `require` edges) remain. Only test scripts listen to `stage-error`, and no canonical smoke baseline contains the word.

The roadmap's M3 removal list names "the per-session FIFO and the steer queue as mechanisms separate from the inbox". Runtime holds follow-ups in `superchat-runtime--session-queues`, steering in each Run entry's `:steer-queue`, and injections in `superchat-runtime--pending-injections`. All three are Runtime-owned, and steering and injection are claimed as one ordered batch by `superchat-runtime--claim-step-batch`.

## Decision

The user accepted all three recommendations with "都按建议".

### 1. A failed turn recording is not successful completion

`record-user-message` and `record-attachments` are required recording contributors of the `run-sealing` notification. When either signals while the Run is about to seal `completed`, the Run seals `audit-incomplete` instead, through the existing terminal fact for that status (`run.failed` carrying `:status audit-incomplete`, as `superchat-runtime--terminal-event-type` maps it). The fault is still recorded in `superchat-event-faults`.

A Run that seals `cancelled`, `failed`, `timed-out` or `audit-incomplete` for another reason keeps that disposition. A failure of any other `run-sealing` listener is recorded as a fault and does not change the disposition. No Tape fact type is added, and no existing fact is removed or reordered: on the ordinary completion path `superchat-runtime--run-turn-hooks` runs before the terminal append, so the change is which terminal status is appended, not where. The Step bracket and `response.completed` appended before sealing keep their recorded values.

### 2. No Stage name survives M3

The Event `stage-error` is named `context-error`, with the same `observe` rule and arguments, emitted by the same collection error path in `superchat-pipeline.el`. The fault record key `:stage` is named `:site`; its values are unchanged. `superchat-stage.el` is named `superchat-event.el`, providing the feature `superchat-event`, and every `require` edge follows it. No `superchat-stage` compatibility feature is kept.

Historical records keep their wording: `.wayfinder/`, [the fixed-stage retirement record](2026-09-12-retire-fixed-stages.md) and [the fixed-pipeline specification](../../design/turn-pipeline-contract.md).

### 3. One inbox owner, three stores

The inbox requirement is one owner and one scheduling authority, as the [M3 record](2026-09-14-m3-execution-main-line-resolution.md#inbox-targeting-claims-and-durability) states, not one data structure. Follow-ups, steering and injection stay in their three Runtime stores, with steering and injection claimed as one ordered batch. The roadmap's removal list names the dispersed ownership, not the stores.

## Implementation constraints

These are leader decisions under the same delegation, recorded so the implementation does not re-open them.

- Whether a `run-sealing` listener is required is declared by the plugin that registers it, at registration. Runtime does not hold a list of other plugins' listener ids.
- A required recording failure is detected from that listener's own failure, not inferred from unrelated entries in the global fault ring, matching the rule the `turn-stopping` notification already follows.
- The rename moves names only. Event arguments, fault record values, Tape facts, smoke baselines and the default plugin set are unchanged.

## Alternatives considered

**Seal `failed` when a turn recording fails.** Rejected because the model's answer has already been delivered to the View and `response.completed` is on the Tape; calling the Run failed contradicts what the user saw. The gap is in the durable record of the turn, which is what `audit-incomplete` states.

**Keep `completed` and document recording failure as an observer fault.** Rejected because the Tape then silently omits a message the user sent while the Run reports success, which the M3 record's required-contributor sentence forbids.

**Treat every `run-sealing` listener as required.** Rejected because any observer, including a third-party plugin's, could then decide a Run's terminal status, while the M3 record keeps Runtime the only owner of Run completion; a recording contributor the product declares is part of that completion, an arbitrary observer is not.

**Keep `stage-error` and the file name, amending the roadmap instead.** Rejected because the names describe a registry that no longer exists, the only listeners are test scripts, and no baseline depends on them, so the cost of the rename is mechanical.

**Keep a `superchat-stage` compatibility feature.** Rejected because the Stage API such a feature would serve is already removed, so any external code requiring it is already broken and a shim would only hide that.

**Merge follow-ups, steering and injection into one store.** Rejected because the M3 record requires unified ownership and ordered claiming, both already met and covered by the regression set, while a storage merge would rewrite the FIFO's audited queue and drop paths for no additional guarantee.

## Consequences

The [request lifecycle protocol](../../protocols/request-lifecycle.md), [architecture map](../../architecture.md), [domain vocabulary](../../../CONTEXT.md), [plugin guide](../../plugin-guide.md) and root `AGENTS.md` change in the same commits as the code they describe. Plugins that listened to `stage-error` or required `superchat-stage` must use `context-error` and `superchat-event`. A turn whose recording fails appears as an `audit-incomplete` Run in status and Tape readers. The [roadmap](../../roadmap.md) tracks the remaining M3 items and acceptance.
