# Request and Run lifecycle

This is the implemented cross-module contract for submission, cancellation and delivery. The [architecture map](../architecture.md) shows the call path; the [composition contract](../design/plugin-architecture.md) defines execution Scope lookup. The earlier [fixed-pipeline specification](../design/turn-pipeline-contract.md) is historical, not a second current contract.

## Submission and preparation

A public producer calls `(superchat-service-call scope 'driver 'submit envelope)`. An Envelope must contain nonempty string `:content`, a registered symbol `:surface` and a valid `:sink`. It may carry `:session-id`, `:tools`, `:media`, owner-specific `:context`, model and execution metadata. Validation and the accepted field forms are implemented by `superchat-pipeline-validate-envelope`.

Top-level legacy buffer-routing, callback and prompt keys are rejected. `:runtime-extras` is a compatibility escape for admitted metadata, not a way to replace the Sink or bypass forbidden routing keys. Producers use the exact host object where a host is needed; a buffer name is not a delivery locator.

`parse` acts on a chat preparation Turn before the Envelope, as the chat surface's own direct call to `superchat-core-prepare-turn`. The driver then resolves the Session, admits input through Runtime, merges core state, selects tools, collects fragments through the `collect-context` Event and assembles a prompt. A `:required t` context listener's execution or validation failure terminates preparation rather than silently omitting required input; an optional listener's failure is recorded in the fault ring and contributes nothing. Runtime, not the preparation Turn, owns acceptance and execution completion.

The assembler produces one prompt and its receipt. Prepared producer text follows the prepared collector, rather than a second public submission API. Preserving prepared text does not prove complete prompt reconstruction: the receipt-append Invariant and provider evidence do not perform an independent end-to-end comparison.

## Request rewriting and tool selection

The fixed Stage registry is retired. `select-tools`, `assemble-prompt`,
`resolve-session`, `admit`, `load-state` and `save-state` are core calls, and
`collect-context` is an Event. Core builds the explicit or surface-baseline
tool candidate and calls `superchat-prompt-assemble` directly. The default
`tools-baseline` and `prompt-assembler` plugins remain mounted as those
capabilities' declarations. Context contributors register through
`superchat-context-register`, which validates the returned fragment list and
reports a required listener's failure as `context-error` before re-signalling.
The Event substrate is `superchat-event.el`; fault records use `:site`.

The Runtime-owned `before-provider-request` waterfall accepts a candidate
with `:prompt`, `:system` and `:tools` (unique tool names, nil or `none`).
Middleware receives a private copy and a single-use continuation. Omitted
fields retain their input values; explicit nil tools means no tools. Unknown
fields, invalid text or tool names, and widening an explicit tool ceiling fail
before dispatch. A refusal also prevents dispatch. Trusted prepared tools
cannot be replaced. The middleware cannot set prepared status or execution
identity. The original restriction stays with Runtime even after an earlier
attempt narrowed or replaced the candidate.

Initial and native continuation requests use this same validation. Only the
final selection is materialized; ordinary tools still need the trusted
Authority wrapper. Tools newly added on a continuation inherit the accepted
execution context and the same wrapper accounting. Retained tools keep their
existing installation-bound descriptors. Final-answer attempts cannot acquire
tools again. Provider evidence records the final prompt and tool schemas.

A system fragment may carry an internal `:render-tools` pure function of
final tool names. Preparation retains its current text and renderer as
process-local request data; after a tool rewrite Runtime refreshes these
blocks without invoking collectors again. Telega workflow guidance and
existing tool-specific system instructions use this path. Middleware that
removes an entire nonempty block does not have that block silently restored.
The assembly receipt describes initial assembly; provider evidence describes
the rewritten request.

## Execution and tools

Runtime is the single owner of public Run status, model continuation, steering, parent/child admission and terminal handling. The model provider is the injected `model-transport` Service; unavailable transport is not permission to use a hidden LLM fallback.

Every model-invoked tool crosses Authority, including request-local wrappers and internal prepared Subagent tools. Ordinary nonempty tool surfaces without a trusted wrapper fail before provider work. Exact invocation identity, current liveness, Effect resolution, policy and approval constrain execution; a tool list cached by the model does not grant execution rights. See [wrapper admission](../adr/0005-fail-closed-ordinary-llm-tool-admission.md) and [refusal delivery](../adr/0006-authority-refusal-consumer-delivery.md).

Approval callbacks and observers can reenter Emacs. They cannot reopen a terminal or retired Run, admit work to an unavailable parent, or select a replacement installation for an already materialized tool. Effects already entered outside Emacs are not generically revocable.

## Cancellation and terminal truth

Runtime cancellation prohibits continuation before callbacks can resume model or tool work. Transport withdrawal retires the matching provider epoch. Preset retirement follows exact Scope ancestry, not a Preset name or Session-wide buffer search. Only the relevant owner combines Runtime retirement with plugin disposal.

Runtime tracks each Run's entered-but-unsettled Effects and requested-but-undecided Approvals by execution identity. Authority records entry before the entered/requested append can reach user code, and clears only the corresponding mark after its settlement is durable. If any work remains open at sealing, a requested `completed`, `failed`, `timed-out` or `cancelled` outcome uses the `audit-incomplete` path. Otherwise the requested outcome is preserved, subject to required recording and storage failures; ordinary cancellation records `run.cancelled`. Runtime does not judge the full trace. A genuine late settlement may append its one fact without reopening the Run, changing its terminal outcome or starting another provider request; no tool result or approval decision is fabricated.

The `chat-turn-recorder` plugin declares its `record-user-message` and `record-attachments` observers required at registration (`superchat-on ... :required t`). Every `run-sealing` observer still runs. Event emission records faults and returns only that invocation's required failures; Runtime never infers them from the global fault ring. If execution would complete but a required recording fails, Runtime preserves the Step ending and `response.completed`, then seals `run.failed` with `:status audit-incomplete`, `:audit-incomplete t` and reason `turn-recording-failed`. Other terminal statuses and ordinary observer failures retain their dispositions.

Run and Turn appends carry stable event identities across SQLite insertion and receipt handling. A confirmed Run terminal survives a lost receipt or later delivery failure; it is not replaced by a competing audit terminal. Recovery can append only a causally matching missing Turn suffix when readable evidence supports it, including a Turn whose confirmed Run seal is `audit-incomplete` after recording failure. An unreadable lookup is `unknown`, not proof of absence and not permission to invent a terminal pair.

### Reading a recorded Run outcome

The `tape` Service's `recorded-run-terminal` method receives Session identity, Run identity and an already-read Tape-ordered snapshot. It returns `present` with one identified terminal, `absent` when there is none, or `unknown` for a conflicting or unusable terminal reading. It does not perform I/O or validate the full trace. Only `run.completed` with payload status `completed` is a successful Run outcome. Reading storage unsuccessfully is not an empty snapshot.

| Recorded Run outcome | Subagent handoff | Workflow resume | Cross-Run Artifact delivery |
|---|---|---|---|
| No terminal recorded | `wait` | `wait` | `wait` |
| Unique `run.completed` with status `completed` | `deliver` | `advance` | Apply the Artifact checks, then `deliver` |
| Any other terminal | `reject` | `stop` | `reject` |
| Conflicting terminal records | `reject` | `stop` | `reject` |
| Record cannot be read | `reject` with reason | `stop` with reason | `manual-review` with the delivery-check reason |

Artifact checks retain producer identity, expected revision, provenance and the requirement that the Artifact event precede its producing Run's terminal in Tape order. Unknown Artifact provenance keeps its own `manual-review` outcome. Subagent and ordinary workflow Run decisions contain `:terminal-state` rather than Closure judgments. Research workflow may still need manual review for an unavailable or undeliverable Artifact; that is not a Run-trace verdict.

### Repair on Session reopen

The chat reopen boundary skips restart repair while the Session has a live process-local Run. Reconcile first replays the Session, which may apply existing recovery journals, then uses raw persisted Run/Turn terminal queries to decide what it may append. These identity queries do not trigger another replay. For an identified `run.started` whose Run and matching Turn terminals are confirmed `absent`, Runtime appends `run.failed` and `turn.completed` with status `abandoned`. An existing `abandoned` Run seal with a confirmed missing Turn receives only that Turn suffix. Unknown storage state or conflicting seals do not authorize another pair. Repair never fabricates a tool outcome or `step.ended`, and does not resume the provider request.

### Experience evidence and recovery

Recording an Experience evaluation and reading it back as evidence both require the uniquely recorded successful Run terminal and its matching, later, non-`audit-incomplete` `turn.completed`. Existing identity, event order, terminal causation and evaluation-reference checks remain binding. A `failed`, `timed-out`, `cancelled`, `audit-incomplete` or `abandoned` Run is not accepted as successful evaluation evidence. Experience's `recovery` Service, request-time `:experience-view` and Authority restrictions remain available; the standalone context collector, history's copy from a preparation Turn, and the two review command forms are absent.

## Sink delivery and synchronous adapters

A Run retains the exact accepted Sink. Streaming, status and terminal callbacks belong to that Run and execute in its original execution Scope. Public Run status and the View's display status are different interfaces; a UI label is not another Runtime terminal.

A dead target or callback failure cannot redirect output to another buffer. Terminal handling uses one delivery owner and quiesces; reentrant or late callbacks do not produce a second terminal outcome. Consumer errors and quits retain the applicable propagation contract rather than being relabelled as new model outcomes.

`driver.submit-blocking` is reserved for Org Babel and Magit, whose host interfaces demand a synchronous result. These adapters use the same Envelope pipeline and accepted Sink. Timeout or caller quit cancels only the adapter's accepted Run; a queued request whose blocking capture has expired cannot execute later through it. Other integrations are asynchronous.

## Queued and steered input

Runtime owns the in-process Session FIFO, live Run steering, and the inbox's `inject` class. Queue and drop audit facts are durable; queued closures and borrowed Scope references are not a restartable queue. There is no second plugin queue for transport suspension or Scope retirement.

- A queued item retains its original Envelope, Sink and borrowed execution Scope.
- Transport absence pauses dispatch without busy retry or pretending the item was dropped; provider restoration resumes eligible input.
- A retired Scope makes new enqueues non-accepted. Already queued items are auditably dropped as `scope-retired`, never retargeted to global or the next Preset.
- A required queue/drop append failure cannot make the item execute without its evidence. An inert retained item can be handled by a later scheduling attempt.
- Rejected queueing leaves the producer's draft available. Steering targets an eligible live Run rather than allocating a replacement Run.
- `inject` carries identified model material rather than a task: it never wakes an idle driver and never starts a Turn. Material must state its source relationship and nonempty text, because that text is what reaches the provider; material the model could not receive is refused as `invalid-material` rather than recorded as admitted.
- An injection belongs to the Scope that submitted it, and matching is exact: a Run claims an injection only when its borrowed execution Scope is the same Scope, never merely an ancestor or descendant. The global Scope is a valid Scope here, not a wildcard — a global injection can only be claimed by a Run that executes in the global Scope, so it cannot reach a Preset's Run. When a live matching Run exists at submission, the injection also carries that Run's identity, and a terminal Run can never acquire fresh material after releasing its borrowed Scope. An earlier revision instead refused a global-resolved injection as `unscoped-material`; that refusal is history, not the current rule.
- The loop is the only claimant. It takes the whole eligible batch — steering and injection together — in one insertion order once, before the pre-step gate decides whether to open the Step; entries arriving after the claim stay pending. Contributors never drain independently: steering is queued with the same arrival sequence as an injection and reaches the model through that one claimed batch, so a steer that arrived first is delivered first even though injections are claimed last. Material a Run ends without consuming is not carried to a later Turn and is not dropped silently: each item lands one `queue.dropped` fact carrying its stable identity and the reason, and its removal is committed only once that fact exists.
- Pending injections settle through the same audited `queue.dropped` path as the FIFO when their Scope retires or the Session's view closes, and their removal is committed only once that fact lands.

- A model transport that is not the native llm.el loop reaches the same behaviour through two driver operations rather than through one adapter's callback protocol: `advance-step` closes the current Step, claims that batch, runs the pre-step gate and opens the next Step, returning the material the transport must append (or a refusal sentinel once the Run is settled as failed), and `turn-stopping` runs the natural-closing notification and returns how much eligible input is waiting, so the transport makes another attempt in the same Run instead of closing. Step brackets, admission and closing therefore do not depend on which transport drives the Run.

Before a drop append, Runtime retains one complete drop event on the pending item. All injection and follow-up disposal paths reuse it. A lost receipt is recovered by its exact event identity, without another write; an unreadable lookup keeps the item pending rather than treating the event as absent. The first reason remains attached to that drop. While its receipt remains unresolved, the item is not eligible for injection admission or follow-up dispatch; later scheduling reconciles the drop instead of resubmitting the input.

Admitted material is recorded once as Runtime's `attachment.added` event with topic `inject`, before dispatch. Within a Run, later Steps may continue the existing provider prompt. Later Turns read the recorded text through the Tape Context View as user material, with its storage/event identity and source relationship. The existing lineage, supersession, cancellation, anchor and budget selectors still apply. Ordinary attachment metadata and Runtime summaries are not prompt messages. An unavailable recorded payload produces an unknown View, not text reconstructed from live state.

Any future inbox must preserve these observable distinctions before replacing this implementation.

## Editor resources and source changes

Runtime and registration Scope cleanup borrow editor resources; neither acquires general buffer-close authority. The [Session EditorWorkspace close protocol](session-editor-workspace-close.md) validates ownership on the exact live object. User buffers, files and Artifact buffers are not owned merely because a Run uses them.

Inline rewrite keeps previews outside source text. Accepting a Change Set applies edits atomically and appends `edit.applied`; reject and cancellation do not apply the preview. This does not prohibit an independently authorized editing tool from changing a Resource through its own Authority-gated operation.

## Verification

Use [the canonical checks](../agents/development.md). R1–R9 cover withdrawal, submission, registration identity, terminal recovery and scoped lifecycle contracts alongside the product smoke journeys. A new architecture must preserve those contracts or name and obtain approval for a deliberate behavior change; passing an older static baseline alone does not prove a new interface.
