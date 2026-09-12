# Decision: Spine components and extension points after the Stage retirement

Status: proposed

## Problem

The user has decided the blueprint, the retirement of the fixed Stages, and the plugin status of Experience, Claims and Closure (see `docs/decisions/accepted/`). What remains open is the concrete shape of the spine those decisions imply: which logical modules exist, what each owns, which interception points replace the Stages, and how Runs and Presets relate to Scopes. This record proposes that shape. The order of work is in [the roadmap](../../roadmap.md); nothing here is implemented or approved.

## Proposal

Six logical modules, each with its own spec once delivered. The split is by ownership, not by file: a module may span or share files where that is simpler in Emacs Lisp.

| Module | Owns | Takes over from (current) |
|---|---|---|
| kernel | Scope, plugin fiber, Service, Event with four dispatch modes (emit, serial, waterfall, first), Invariant registry | `superchat-plugin.el`, `-service.el`, the Event half of `-stage.el`, `-invariant.el` |
| session | the append-only log, `derive-messages` as the only model history, projections, resume and repair, a persistence seam with the SQLite provider | `superchat-tape.el`, `-tape-view.el`, the Tape half of `-db.el`, the seal logic in `-runtime.el`, the in-buffer history list |
| prompt | ordered sections, contexts, tool schemas, one complete override, persona as a section | `superchat-prompt.el`, `-prompt-hooks.el`, the preset persona path |
| tools | scoped registry, per-scope restriction, model presentation, the execution pipeline pre → approval → monotonic guards → around → body → post → finalize | the registry in `superchat-tools.el`, `-select-tools.el`, the execute path of `-authority.el`, the wrappers in `-agent-loop.el` |
| agent and agent-loop | a live agent handle with an inbox (`followup` next turn, `steer` next step, `inject` without wake), turn and step bracketing on the log, request building, stream settlement, tool scheduling, cancel and retire-before-dispose | `superchat-runtime.el`, `-pipeline.el`, the model path of `-dispatcher.el`, the loop in `-llm.el`, the FIFO and steer queue |
| llm | the adapter seam over `llm.el` (`llm-chat-streaming`, `llm-chat-async`); retry as a plugin | the transport part of `superchat-llm.el`, `-models.el` |

Three extension domains replace the Stage registry: durable session events (facts on the Tape), live agent events (pre-step waterfall, request waterfall, request-error waterfall, turn-stopping serial), and capability-seam events (`tools/*`). Authority's decision stays the single point every tool passes; approval is a seam that fails closed when absent.

Scopes: the per-View Preset Scope delivered in 4C-2 stays. A Run borrows its caller's Scope; Run lifetime and the retire-then-dispose contract are designed inside the agent module rather than as a separate automatic Run Scope. A standing Preset mount that several Views join by re-parenting is an independent, unscheduled proposal; it must keep the delivered per-View isolation, and two Views choosing the same Preset name are not, by that fact, joined.

Composition: profiles (`chat`, `inline`, `headless`) as ordered row lists, with the default profile reproducing today's `superchat-plugins` exactly before that variable is retired.

Kept and moved rather than rewritten: Tape append and replay, Authority decide and audit including `audit-incomplete`, the exact Sink, retire-scope-before-dispose.

## Alternatives considered

**Keep the eight Stages and only split files.** Rejected: the user retired the Stages; a split alone leaves the dispatcher-owned `parse` and the separate FIFO and steer queue in place.

**One file per module as a rule.** Not required: Emacs Lisp load order and `declare-function` make a strict one-file rule costly; the spec, not the file, is the unit.

**Port dsh's seams as they are (filesystem, shell, subprocess, sandbox).** Rejected: superchat's world is Emacs objects; the host filesystem and shell are escape hatches. The seam list is a later item gated on the undecided tool surface.

**An automatic Run Scope as its own batch (4C-3).** Deferred into the agent module design so that no empty Scope layer is added before its owner exists.

## Acceptance criteria

- Each delivered module has a spec under `docs/components/` and the smoke cases named in the roadmap pass on a real model and on the scripted transport.
- Model-visible history is reconstructed independently from the Tape and compared with the actual provider-visible request, per [the roadmap's M1 acceptance](../../roadmap.md#milestones); a recorded `prompt.assembled` receipt alone does not count.
- Every tool execution passes the one pipeline; a contributed `allow` cannot relax an inherited `deny`.
- Removing any row of the default profile leaves the rest loading.

## Risks

- The inbox and waterfall API is undesigned; the current FIFO semantics (queue while running, drop on dead Sink, pause on withdrawn transport) must be carried over and each has a smoke case.
- Feature loss during the cut-over: no plugin leaves the default set without a recorded user decision.
- Batch 4C-2 is frozen and the present work is documentation only; this record is rewritten when a milestone lands so it never describes an unbuilt module as `current`.
