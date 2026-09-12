---
title: Migration order and per-step verification
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: [007-default-plugin-inventory]
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
Confirm the six-step order (scope accounting → services → one prompt path → built-ins as plugins → file split → docs) against the inventory, and design the per-step smoke check against LM Studio (localhost:1234) that replaces a test suite: which journeys, which Tape event sequences are compared.

## Resolution (2026-09-11, sc-leader)

**Plain language.** Six steps, each one either "add without changing
behaviour" or "one behaviour change with a named check". Because there is no
test suite, every step ends with the same 20-line smoke script against LM
Studio plus a mock transport, and the check is "the Tape says the same thing
as before the step". Order confirmed, with one change: the mock transport
comes *first* (step 0), because it is what makes every later step checkable.

| Step | What changes | Behaviour change? | Check |
|---:|---|---|---|
| 0 | **Kernel primitives + mock transport.** Add `superchat-plugin/service/stage(rewrite)/invariant/envelope`; wrap today's `superchat--llm-generate-answer` as the `transport-llm` provider and add `transport-mock` (scripted replies, tool calls, hangs) | none — nothing calls the new Kernel yet | prototype 006 drive passes inside the package; `(superchat-plugin-mount 'transport-mock)` then one chat turn answers with the script |
| 1 | **Scope accounting.** `superchat-register-tool/command/surface`, `superchat-authority-register-*`, `superchat-stage-add`, every `add-hook` on a Superchat list → go through a Scope (default plugins get a Scope each, still one big file) | none | unmount `cowork-telega`: no `telega*` in any registry; remount: Telega digest smoke passes |
| 2 | **Services.** Twelve Services provided; every cross-file call in 001's table becomes `superchat-service-call`; `fboundp` probes → `superchat-service-get`; `superchat-runtime.el:2292` calls `model-transport` | none | Smoke A–F identical Tape sequences with `transport-llm`; Smoke A with `transport-mock` |
| 3 | **One prompt path.** Delete `superchat-core-prepare-turn`, `:prepared-prompt`, the template re-collection; chat and the four producers become plain producers; `parse` Stage; Events replace hook lists | **yes** (the only one) | Smoke A/B/C: the *model-visible prompt* (Invariant 4 receipt) equals the pre-step capture byte-for-byte for the same script; Smoke D/E for subagent/workflow |
| 4 | **Built-ins as plugins.** `tools-baseline`, `save-state` contributors, `view` Service, `commands` Service; delete stubs, decorative stages, legacy hook lists, `superchat-extension.el`; Telega port (008) | none intended | Smoke A–F unchanged; `superchat-plugins` minus `memory` still loads and Smoke A passes with no `memory` fragment in the receipt |
| 5 | **File split** along 007's table, one plugin per file, no plugin `require`s another | none | `emacs -Q --batch -L . --eval '(require (quote superchat))'` loads; byte-compile clean; Smoke A–F unchanged |
| 6 | **Docs.** `docs/plugin-guide.md` rewritten around the four pieces, `AGENTS.md` Kernel table, `CONTEXT.md` | — | plugin guide's example plugin actually mounts |

**The smoke script** (`scripts/smoke.el`, ~20 lines per case, run with
`emacs -Q --batch -L . -l scripts/smoke.el CASE`, model = whatever
`/v1/models` on `localhost:1234` lists; `transport-mock` for the
deterministic cases):

| Case | Journey (from the deleted C/B tests) | Tape sequence compared |
|---|---|---|
| A | chat: one plain turn | `intent.received prompt.assembled response.completed run.completed turn.completed` |
| B | chat: turn with one approved tool call (`read_buffer`) | A + `tool.execution.started approval.decided tool.completed` in that order between `prompt.assembled` and `response.completed` |
| C | chat: send while a Run is active, then cancel | `queue.queued run.cancelled queue.dispatched run.completed` |
| D | inline ask on a scratch buffer | A with `:surface inline-ask`; buffer hash unchanged (Invariant 1) |
| E | inline rewrite → accept, and → reject | accept: `change-set.accepted` + hash changed; reject: no accept Fact, hash unchanged |
| F | Telega digest with `transport-mock` | `intent.received … run.completed` with `:surface telega-digest` and the two policies recorded in `approval.decided` |

Comparison = `(equal (mapcar #'type events) EXPECTED)` plus the receipt text
for step 3; the expected sequences are captured once *before* step 0 from
the current code and committed under `scripts/smoke-expected/`. A step is
done when all six cases match; a mismatch is investigated, never re-baselined
without a note in the map.
