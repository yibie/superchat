---
title: Stage main line v2
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: []
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
With chat as a plain producer (no prepared-prompt bypass) and the driver as a replaceable Service, which Stages remain, in what order, with what inputs/outputs and absent-behaviour? Today: resolve-session admit load-state select-tools collect-context assemble-prompt run-model save-state render dispatch (+ on-error, register-*). Decide which are deleted, which become Events.

## Resolution (2026-09-11, sc-leader)

**Plain language.** The main line shrinks from 13 names to 8 real steps. Every
step is a Stage with a registered *default plugin* implementation (so a third
party can shadow it in a Preset Scope or replace it globally), except the two
things that are not steps at all: calling the model (a Service call) and the
seal (driver-internal, an Invariant). Five names are deleted; `on-error`
becomes an Event.

| # | Stage / rule | Input → result | Default plugin | Absent |
|---:|---|---|---|---|
| 1 | `parse` / fold | envelope → envelope (may rewrite `:content`, set `:target-model`, `:tools`, `:context :chat-skill`/`:chat-command`/`:chat-file-ref`) | `chat-parse` (today `superchat-core--parse-input`: `@model`, `>skill`, `>>workflow`, `/command`, `#file[:A-B]`), applies only to `:surface chat`/`inline-ask` | envelope unchanged |
| 2 | `resolve-session` / first | envelope → session-id | `chat-session` (host View's session) | explicit `:session-id`, else ephemeral |
| 3 | `admit` / first | session-id, envelope, snapshot → `(:action process\|queue\|steer\|drop\|handled :reason :target-run-id)` — **new action `handled`**: a plugin consumed the Request (slash command, `>>workflow` coordinator) and no Run is created, no drop Fact | `chat-active-run` (queue), `chat-commands` (handled), `workflow` (handled) | process |
| 4 | `load-state` / collect | envelope, session-id → owner-prefixed deltas | `chat-state` (authority mode/policy from the View) | authority defaults only |
| 5 | `select-tools` / first | envelope, state → `none` or names | `tools-baseline` (today's rules 3–4 from `superchat-llm.el:1663-1750`, moved out of llm.el); rule 1 (explicit `:tools`) stays in the driver as Envelope contract | `none` |
| 6 | `collect-context` / collect | envelope, state, tools → fragments | the seven collectors of `superchat-prompt-hooks.el` minus `legacy-prompt-hooks`, plus `plan` (today's post-topic hook), `extension-*` deleted with `superchat-define-extension` (Telega registers its own collector, ticket 008) | content fragment only |
| 7 | `assemble-prompt` / first | fragments, budget → `(:system :messages :receipt)`; over-budget → terminal `prompt-over-budget` | `core-assembler` (`superchat-prompt.el:311`) — now replaceable, no longer `core-only` | driver error `no-assembler` (no Run) |
| — | *(Service call)* `model-transport.generate` | prompt-spec, tools, Sink → outcome | `transport-llm` | driver PENDING |
| 8 | `save-state` / collect | outcome, state → `(:owner :receipts)` per contributor | `tape-user-message` (`superchat.el:4312-4314`), `experience-observer` (`superchat-experience.el:2340`), `plan-progress`, `agents-panel`, `workspace-artifacts` — the erroring stub `core-seal` (`superchat-stage.el:319-323`) is deleted; the seal is driver code guarded by Invariant 2 | seal only |

**Deleted names:** `run-model` (Service), `render` (the Sink *is* the seam; the
chat default Sink comes from the `view` Service, so "replace the chat
renderer" = provide `view`), `dispatch` (driver emits to the exact Sink;
`superchat-stage.el:325-329` stub deleted), `register-command`,
`register-surface` (never called; commands are a Service, surfaces are
Envelope vocabulary). `on-error` → Event `stage-error` (ticket 003).

**One prompt path (the substance of C1).** `superchat-core-prepare-turn`,
`superchat-prompt-collect`, the dispatcher's template re-collection
(`superchat-dispatcher.el:302-335`) and the `:prepared-prompt` bypass
(`superchat-pipeline.el:536-582`) are deleted. Chat submits
`(:content RAW :surface chat :sink … :context (:chat-buffer B :chat-template T …))`;
`parse` and the collectors do the rest inside the driver. The four hand-built
producers (subagent, workflow, skills-match, compaction) stop sending
`:prepared-prompt`: each registers *its own* collector (owner `subagent` etc.)
that emits its prepared text as a hard `template` fragment for its surface, so
third-party collectors run for them too and the budget rule applies uniformly.
Preset persona/skill text = a `collect-context` contributor mounted in the
Preset Scope (slot `system`), which is exactly what the Scope tree is for.

**Kinds:** `first`, `collect`, `fold` remain; `core-only` is deleted (nothing
uses it after 7 becomes replaceable). "Later registration wins" stays *within*
a Scope; across Scopes nearest wins (006).
