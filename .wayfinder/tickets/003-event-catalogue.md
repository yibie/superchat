---
title: Event catalogue
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: []
---
## Question
Which named Events exist, each with its combination rule? Map today's hook variables (`superchat-*-functions` in core, runtime, agent-loop, llm) and pi/dsh's event lists (tool pre/post-execute, turn start/end, session start, before-provider-request…) onto one catalogue; decide which old variables are deleted.

## Resolution (2026-09-11, sc-leader)

**Plain language.** An Event is a named extension point off the main line,
each with one combination rule that is part of its contract. Fifteen Events
replace the eighteen `superchat-*-functions` variables; nine variables are
deleted outright. Listeners register with `superchat-on SCOPE EVENT ID FN`
(Scope-reversible); no `add-hook` on a Superchat variable remains.

| Event | Rule | Args | Replaces (today) | Registrants today |
|---|---|---|---|---|
| `stage-error` | observe | stage condition envelope | `on-error` stage (`superchat-pipeline.el:152-158`) | none |
| `run-started` | observe | run envelope | — (new; pi `turn_start`) | — |
| `run-sealing` | observe | run turn | `superchat-pre-topic-finalized-functions`, `superchat-post-turn-functions`, `superchat-post-topic-finalized-functions` (run back-to-back, identically, `superchat-runtime.el:1169-1200`) | `superchat.el:4312,4314` (user message / attachments → become `save-state` contributors instead) |
| `run-completed` | observe | run status | `superchat-run-completed-functions` | `superchat-experience.el:2340` |
| `turn-completed` | observe | turn | `superchat-turn-completed-functions` | `superchat.el:1046`, `superchat-plan.el:609`, `superchat-agents-panel.el:207` |
| `tool-before` | observe | tool args run | `superchat-agent-pre-tool-functions` | — |
| `tool-permission` | fold (acc ∈ nil/allow/deny; deny wins, never overwritten) | tool args run | `superchat-agent-permission-functions` | — |
| `tool-args-rewrite` | fold | args tool run | `superchat-agent-tool-rewrite-functions` | — |
| `tool-after` | observe | tool args result run | `superchat-agent-post-tool-functions` | — |
| `tool-failed` | observe | tool args error run | `superchat-agent-post-tool-failure-functions` | — |
| `tool-result-filter` | fold | result tool args run | `superchat-agent-tool-result-filter-functions` | — |
| `before-provider-request` | fold | prompt-spec run | — (new; pi/dsh `before-provider-request`) — the `parse`-then-collect path makes the old build-prompt hooks unnecessary; this is the last-look seam | — |
| `context-budget-exceeded` | first | details `(:reason :resume-function :fail-function)` | `superchat-llm-context-budget-exceeded-functions` (`superchat-llm.el:350`) | `superchat-compact.el` overflow recovery |
| `closure-shadow` | observe | report | `superchat-closure-shadow-functions` (`superchat-runtime.el:109`) | — |
| `session-started` / `session-closed` | observe | session-id | — (new; editor-workspace open/close) | — |

**Deleted, no replacement:** `superchat-system-prompt-functions`,
`superchat-build-prompt-functions` (and the `legacy-prompt-hooks` collector —
v2 may break the API once; third parties write a `collect-context`
contributor), `superchat-pre-topic-functions` (no registrant anywhere),
`superchat-post-topic-functions` (its one registrant, `active-plan`, becomes
the `plan` collector), `superchat-command-hooks` (commands are a Service).

**Rules.** `observe` listeners' errors are recorded as `stage-error`-style
Facts and never abort the Run; `fold` listeners run nearest-Scope-last so a
Preset can override; `first` stops at the first non-nil. Event names are
declared by the Kernel list above plus `superchat-define-event` for plugins
that want their own (declared, with a rule, before any `superchat-on`).
