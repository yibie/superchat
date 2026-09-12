---
title: Default plugin inventory
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: [001-service-catalogue, 002-stage-main-line]
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
Which plugins does v2 ship, one per file, each with its `inject` list and what it registers? Assign every current top-level file (superchat.el, llm, runtime, authority, tools, render, dispatcher, experience, memory, plan, genui, subagent, workflow, skills, compact, mcp, inline, rewrite, change-set…) to a plugin or to the Kernel; name what is deleted.

## Resolution (2026-09-11, sc-leader)

**Plain language.** Everything that is not the Kernel becomes a plugin — one
file, one `superchat-define-plugin`, an `:inject` list of Services, and a
list of what it registers. Kernel ≈ 1.5k lines (six files). Thirty-one
default plugins ship in `superchat-plugins`; the current 46 top-level files
map onto them as below (sizes are today's `wc -l`, for orientation only).

**Kernel (not plugins)** — `superchat-plugin.el` (Scope/fiber/mount, from
prototype 006), `superchat-service.el`, `superchat-stage.el` (Stage + Event
registries; rewritten, 333 → ~150), `superchat-invariant.el`, `superchat-sink.el`
(135), `superchat-envelope.el` (validation + surface list, out of
`superchat-pipeline.el:36-120`), plus `superchat.el` reduced to the loader:
`superchat-plugins` defcustom, mount at load, autoload cookies. Target ≤ 1.5k.

| Plugin | inject | Registers | From today's file(s) |
|---|---|---|---|
| `driver` | tape, closure | Service `driver`; Invariants 2/4/5 seams live here | `superchat-pipeline.el` (591) + Run lifecycle of `superchat-runtime.el` (~1.8k of 2995) |
| `transport-llm` | tools, tape | Service `model-transport`; Events `before-provider-request`, `context-budget-exceeded` emitter | `superchat-llm.el` (3781) minus tool selection/materialize; `superchat-models.el` (204); `superchat-agent-loop.el` (704) tool-round loop |
| `tape` | — | Service `tape` | `superchat-tape.el` (639), `superchat-projection.el` (103), tape half of `superchat-db.el` (760), `superchat-save.el` (88) |
| `session-view` | tape | Service `session` | `superchat-tape-view.el` (1190) |
| `closure` | tape | Service `closure`; Event `closure-shadow` emitter | `superchat-closure.el` (884), closure-guard half of runtime |
| `authority` | tape | Service `authority`; approval UI (y/n/a, diff preview) | `superchat-authority.el` (1825) minus `superchat-tool-execute` |
| `tools` | authority, tape | Service `tools` (`register/materialize/execute`); Invariant 3 seam; six tool-lifecycle Events | registry + `superchat-tool-execute` + `superchat-select-tools-decision` rule 1 |
| `tools-builtin` | tools, memory, session | the built-in tools (list_buffers, read_buffer, edit_buffer, eval_elisp, apropos, memory_search, ask_user, define_tool, render_ui…) | `superchat-tools.el` bodies (1748), `superchat-executor.el` (95), `superchat-synthesis.el` (667, `define_tool`) |
| `tools-baseline` | tools | Stage `select-tools` default (rules 3–4) | `superchat-llm.el:1663-1750` |
| `prompt-assembler` | — | Stage `assemble-prompt` default | `superchat-prompt.el` (317) |
| `chat-parse` | — | Stage `parse` | `superchat-core.el` (178), `superchat-parser.el` (94) |
| `chat-collectors` | session, memory | Stage `collect-context`: system-instructions, file-inline, context-files, memory, history, experience, plan | `superchat-prompt-hooks.el` (590) |
| `chat-view` | driver, session, editor-workspace, view | Service `view`; the chat window (mode, prompt region, status line, org rendering, response region) | `superchat-render.el` (1678), the View half of `superchat.el`, `superchat-widgets.el` (81) |
| `chat-commands` | driver, session, commands | Service `commands`; Stage `admit` (`handled` for `/…`); the built-in slash commands | command alist + handlers of `superchat.el` (~2k) , `superchat-dispatcher.el` (1266) |
| `chat-session` | driver, editor-workspace | Stages `resolve-session`, `load-state`, `admit` (`chat-active-run` queue); follow-up dispatch | `superchat-dispatcher.el:985-1010, 1251-1263`, queue half of runtime |
| `editor-workspace` | tape | Service `editor-workspace`; Events `session-started/closed` | `superchat-editor-workspace.el` (693) |
| `change-set` | tape | Service `change-set`; Invariant 1 writer | `superchat-change-set.el` (98) |
| `inline` | driver, change-set, view | surfaces `inline-ask`/`inline-rewrite`; collector `inline-reference`; overlay preview | `superchat-inline.el` (569), `superchat-rewrite.el` (735) |
| `memory` | tape | Service `memory`; `/remember`, `/recall` | `superchat-memory.el` (666), memory half of `superchat-db.el` |
| `compact` | driver, session, tape | `/compact`, `/expand`; Event `context-budget-exceeded` listener; its own `collect-context` contributor (replaces `:prepared-prompt`) | `superchat-compact.el` (1013) |
| `experience` | tape, session, closure | Event `run-completed` listener; `/review`; recovery view for the driver via `superchat-service-get` | `superchat-experience.el` (2345) |
| `claims` | tape | `/claim` capture/correct | `superchat-claims.el` (350) |
| `plan` | driver, tools | collector `plan`; Event `turn-completed`; `emit_plan` tool; `/plan` | `superchat-plan.el` (895) |
| `subagent` | driver, tools | `delegate_to_subagent` tool; surface `subagent`; own collector | `superchat-subagent.el` (1503) |
| `agents-panel` | driver | Event `turn-completed`; the live panel | `superchat-agents-panel.el` (250) |
| `workflow` | driver | Stage `admit` (`handled` for `>>`); surface `workflow`; own collector | `superchat-workflow.el` (1312) |
| `skills` | driver, commands | `>skill` resolution; surface `skills`; own collector | `superchat-skills.el` (804) |
| `preset` | commands | Preset loading/activation, Preset Scope creation (005) | `superchat-preset.el` (431) + `superchat--activate-capability` |
| `genui` | tools | `render_ui` tool; TextUI renderer | `superchat-genui.el` (443) |
| `mcp` | tools, authority | MCP servers as registered tools | `superchat-mcp.el` (1068) |
| `workspace` | tools, tape | artifact tools; `save-state` contributor | `superchat-workspace.el` (1159) |
| `tape-browser` | session | the Tape browser UI | `superchat-tape-browser.el` (209) |
| `emacsclient` | driver, session | JSON Run surface | `superchat-emacsclient.el` (226) |
| `org-babel` | driver | `ob-superchat` blocking adapter | `ob-superchat.el` (115) |
| `magit` | driver | commit-message surface | `superchat-magit.el` (122) |

**Deleted outright:** `superchat-extension.el` (531; `superchat-define-extension`,
its composition seam and the `extension-*` collectors — Telega registers
directly, ticket 008), `superchat-core.el`'s Turn struct and
`superchat-core-prepare-turn` (parse becomes a Stage, the Turn's other slots
are Envelope context), `superchat-runtime-submit` and the legacy request
shape (every producer submits an Envelope), `superchat-pipeline-envelope-from-request`
(only Telega-legacy used it), the `legacy-prompt-hooks` collector, the two
erroring stage stubs, `superchat-stage-report`.

**Honest size expectation:** this is a re-partition, not a diet. The behaviour
that exists stays; the deletions above are ≈ 1.5–2k lines; the Kernel gets
~1.5k *new* lines. Net effect on the 42.8k total is roughly −1k; the win is
that `superchat-plugins` can drop any row and the rest still loads.
