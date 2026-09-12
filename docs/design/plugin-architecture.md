# Superchat v2 plugin architecture

Status: specification, 2026-09-11. Supersedes the "everything else is a plugin
attached to a stage" sentence of `turn-pipeline-contract.md`; that contract's
Envelope, Sink, fragment and assembly sections remain in force where this
document does not say otherwise. Decisions and evidence: `.wayfinder/map.md`
and `.wayfinder/tickets/`. Vocabulary: `CONTEXT.md` § Composition.

The idea in one sentence: **the Kernel knows five registries and two data
shapes; everything Superchat does — the driver, the model loop, Authority,
the Tape, the chat window — is a plugin mounted into a Scope, and unmounting
it withdraws everything it registered.**

## 1. Kernel

The Kernel is six files (~1.5k lines) plus the loader in `superchat.el`.

| File | Owns |
|---|---|
| `superchat-plugin.el` | `superchat-define-plugin`, Scope, fiber (pending/active/failed), mount/unmount/reconfigure, `superchat-plugins` |
| `superchat-service.el` | Service definitions (name → method list), providers, `superchat-service-call`, `superchat-service-get` |
| `superchat-stage.el` | the eight Stages and the Event registry, four combination rules |
| `superchat-invariant.el` | the six Invariants and their seam checks |
| `superchat-sink.el` | the Sink struct and `superchat-sink-emit` (unchanged) |
| `superchat-envelope.el` | Envelope validation and the Surface list |

The Kernel never requires a plugin file, never opens a window, never calls a
model. It loads plugins named in `superchat-plugins` and stops.

### 1.1 Plugin shape

```elisp
(superchat-define-plugin NAME
  :inject  (SERVICE …)                       ; services this plugin needs
  :config  ((KEY PREDICATE DEFAULT [DOC]) …) ; validated on every activation
  :apply   (lambda (scope config) …))        ; registers through SCOPE only
```

- `:inject` — the plugin is **pending** until every named Service has an active
  provider; it is applied when they exist, unwound when any of them goes away,
  and re-applied when a provider (the same or another) returns. This is the
  "epoch" rule: a dependent is a function of the set of active providers.
- `:config` — defaults are filled, then each `PREDICATE` runs; unknown keys are
  errors. Bad config = the plugin is **failed** and registered nothing.
  `superchat-plugin-reconfigure NAME CONFIG` validates, unwinds, re-applies.
- `:apply` — receives an *effect Scope* whose registrations land in the Scope
  the plugin was mounted into (§1.2). Everything it does must go through
  `scope`; a plugin that `defun`s or `setq`s globals leaks (Invariant 6).

Prototype: `.wayfinder/research/kernel-prototype.el` (250 lines, 13 checks).

### 1.2 Scope

A Scope is `(name parent children effects layers layer-owner)`.

- **Registration** — `superchat-stage-add`, `superchat-on`,
  `superchat-service-call scope 'tools 'register …` and every other
  registering call take a Scope and record an undo on it.
- **Lookup** — from a Scope: the global layer, then each ancestor's layer
  farthest → nearest, then the Scope's own; the nearest definition of a name
  wins. Within one Scope a duplicate id is an error; "later registration
  wins" holds only across Scopes.
- **Disposal** — children first, then undo thunks newest-first, then unlink.
  A layer is created on first write and deleted when it becomes empty, so a
  disposed Scope leaves no trace.
- **Tree** — `global` (created at `(require 'superchat)`) → `preset/<name>`
  (created when a Preset is activated, disposed when deactivated) →
  `run/<run-id>` (created by `driver.submit` before `parse`, passed to every
  Stage and Event, disposed after the terminal Sink emission). Session is
  not a Scope level: Sessions are Tape identities.

### 1.3 Services

A Service is a required-method list; a provider is a method → function table
checked at mount (missing method = mount error). Exactly one active provider
per name; Services are global-Scope only (a Preset may not shadow one, because
a provider swap restarts every dependent). Consumers call
`(superchat-service-call SCOPE NAME METHOD &rest ARGS)`; optional use is
`(superchat-service-get SCOPE NAME)` → provider or nil, which replaces every
`fboundp` / `with-eval-after-load` probe in the code base.

| Service | Methods | Default provider | Absent |
|---|---|---|---|
| `driver` | `submit ENVELOPE→handle` `cancel RUN &optional REASON` `steer RUN TEXT` `status RUN` `active-runs` `pending-input RUN` `answer-input RUN HANDLE ANSWER` `enqueue-follow-up SESSION ENVELOPE` `pending-follow-ups SESSION` `clear-follow-ups SESSION REASON` | `driver` | no Surface can submit |
| `model-transport` | `generate PROMPT-SPEC TOOLS SINK→outcome` `capabilities MODEL→(:streaming :tools)` | `transport-llm` (`transport-mock` for verification) | driver pending |
| `tape` | `append-event` `append-material` `append-anchor` `replay SESSION &optional SINCE LIMIT` `register-projection` `replay-projection` | `tape` | nothing runs (driver injects it) |
| `session` | `entries` `search` `assemble-view` `superseded-ids` `history` `configuration` `session-ids` | `session-view` | history/memory collectors contribute nothing |
| `authority` | `decide TOOL ARGS ATTENDANCE &optional POLICY EFFECTS` `resolve-effects TOOL ARGS` `register-policy` `register-effect-resolver` `register-approval-preview` (Scope-reversible) | `authority` | `tools.execute` denies everything |
| `tools` | `register NAME &key …` (Scope-reversible) `descriptors` `materialize NAMES→llm tools` `execute NAME ARGS ATTENDANCE` | `tools` | model sees no tools |
| `view` | `sink-for SURFACE CONTEXT→sink` `markdown-to-org TEXT` | `chat-view` | chat cannot open; other producers pass their own Sink |
| `commands` | `register NAME HANDLER &optional DOC` (Scope-reversible) `lookup` `list` | `chat-commands` | `/name` is plain text |
| `closure` | `judge QUERY TRACE→verdict` | `closure` | driver seals and records `closure.absent` |
| `editor-workspace` | `validate-buffer` `buffer SESSION ROLE &optional CREATE` `owner-session` `acquire-buffer` `rollback-acquisition` `close` | `editor-workspace` | chat cannot host a View |
| `change-set` | `propose HOST EDITS→id` `accept ID` `reject ID` `apply ID` | `change-set` | inline rewrite cannot be accepted |
| `memory` | `insert` `search` `search-prioritized` `get` `list-review` `set-review` `count` `reset-projection` | `memory` | memory collector and `memory_search` contribute nothing |

Not Services: artifacts (`superchat-workspace.el` → a plugin registering tools
and a `save-state` contributor); Surfaces (Envelope vocabulary, §1.6).

### 1.4 Stages — the main line

Eight Stages in a fixed order. Every Stage has a *default plugin*
implementation and can be shadowed in a Preset Scope or replaced globally.
Calling the model is a Service call between 7 and 8, not a Stage; the seal
after 8 is driver code guarded by Invariant 2.

| # | Stage / rule | Input → result | Default plugin | Absent |
|---:|---|---|---|---|
| 1 | `parse` / fold | envelope → envelope (`:content` rewritten; `:target-model`, `:tools`, `:context :chat-skill / :chat-command / :chat-file-ref` set) | `chat-parse` (`@model`, `>skill`, `>>workflow`, `/command`, `#file[:A-B]`), only for `chat` / `inline-ask` | unchanged |
| 2 | `resolve-session` / first | envelope → session-id | `chat-session` | explicit `:session-id`, else `ephemeral-<request-id>` |
| 3 | `admit` / first | session-id, envelope, snapshot → `(:action process\|queue\|steer\|drop\|handled :reason :target-run-id)`; **`handled`** = a plugin consumed the Request, no Run, no drop Fact | `chat-active-run` (queue), `chat-commands` and `workflow` (handled) | `process` |
| 4 | `load-state` / collect | envelope, session-id → owner-prefixed deltas | `chat-state` | authority defaults only |
| 5 | `select-tools` / first | envelope, state → `none` or names; explicit `:tools` on the Envelope is decided by the driver before this Stage | `tools-baseline` | `none` |
| 6 | `collect-context` / collect | envelope, state, tools → fragments | `chat-collectors` (system-instructions, file-inline, context-files, memory, history, experience, plan), `inline-reference`, each producer's own collector | content fragment only |
| 7 | `assemble-prompt` / first | fragments, budget → `(:system :messages :receipt)`; hard set over budget → terminal `prompt-over-budget` | `prompt-assembler` | driver error `no-assembler`, no Run |
| — | `model-transport.generate` | prompt-spec, tools, Sink → outcome | `transport-llm` | driver pending |
| 8 | `save-state` / collect | outcome, state → `(:owner :receipts)` | `tape-user-message`, `experience-observer`, `plan-progress`, `agents-panel`, `workspace-artifacts` | seal only |

**One prompt path.** Every producer submits an Envelope with raw `:content`;
`parse` and the collectors do the rest inside the driver. `superchat-core-prepare-turn`,
`superchat-prompt-collect`, the dispatcher's template re-collection and the
`:prepared-prompt` bypass are deleted. Producers that used to hand-build a
prompt (subagent, workflow, skills, compaction) register their own
`collect-context` contributor emitting their text as a hard `template`
fragment for their Surface; a Preset's persona/skill text is a `system`
contributor mounted in the Preset Scope.

Deleted names: `run-model`, `render` (the Sink is the seam; the chat default
Sink comes from `view`), `dispatch`, `register-command`, `register-surface`,
the `core-only` kind, the two erroring stubs, `superchat-stage-report`.

### 1.5 Events

An Event is a named extension point off the main line with one rule.
Listeners register with `(superchat-on SCOPE EVENT ID FN)`; no `add-hook` on a
Superchat variable remains. Plugins may declare their own with
`superchat-define-event NAME RULE` before any `superchat-on`.

| Event | Rule | Args | Replaces |
|---|---|---|---|
| `stage-error` | observe | stage condition envelope | `on-error` stage |
| `run-started` | observe | run envelope | new |
| `run-sealing` | observe | run turn | `superchat-pre-topic-finalized-functions`, `-post-turn-functions`, `-post-topic-finalized-functions` |
| `run-completed` | observe | run status | `superchat-run-completed-functions` |
| `turn-completed` | observe | turn | `superchat-turn-completed-functions` |
| `tool-before` | observe | tool args run | `superchat-agent-pre-tool-functions` |
| `tool-permission` | fold, deny wins | tool args run | `superchat-agent-permission-functions` |
| `tool-args-rewrite` | fold | args tool run | `superchat-agent-tool-rewrite-functions` |
| `tool-after` | observe | tool args result run | `superchat-agent-post-tool-functions` |
| `tool-failed` | observe | tool args error run | `superchat-agent-post-tool-failure-functions` |
| `tool-result-filter` | fold | result tool args run | `superchat-agent-tool-result-filter-functions` |
| `before-provider-request` | fold | prompt-spec run | new (last look before transport) |
| `context-budget-exceeded` | first | details | `superchat-llm-context-budget-exceeded-functions` |
| `closure-shadow` | observe | report | `superchat-closure-shadow-functions` |
| `session-started` / `session-closed` | observe | session-id | new |

Rules: `observe` errors are recorded, never abort the Run; `fold` runs
nearest-Scope-last; `first` stops at the first non-nil. Deleted without
replacement: `superchat-system-prompt-functions`, `superchat-build-prompt-functions`
(and the legacy bridge collector), `superchat-pre-topic-functions`,
`superchat-post-topic-functions`, `superchat-command-hooks`.

### 1.6 Envelope and Sink

Unchanged from `turn-pipeline-contract.md` §2.2 / §3: an Envelope is
`(:content :surface :sink :session-id :context :tools :media …)` with
owner-prefixed context keys; a Sink is the exact delivery target. Two
changes: `superchat-surface-declare SCOPE NAME` replaces
`superchat-register-surface` (Scope-reversible; an Envelope naming an
undeclared Surface fails validation, no Run); and the Envelope's optional
`:context (:preset P)` carries the active Preset for plugins that read its
slots (`tools`, `model`, `temperature`, `max-tokens`, `reasoning`).

### 1.7 Invariants

Kernel data, not plugin registrations. Checked synchronously at a seam the
Kernel owns; a violation is the Tape Fact `invariant.violated`, never hidden;
a check that itself errors is `invariant.check-failed`. Replacing the `driver`
Service does not exempt anyone: the Kernel wraps every driver's `submit`.
`superchat-invariant-report` lists them.

| # | Invariant | Seam | Check |
|---:|---|---|---|
| 1 | host buffer unchanged until its Change Set is accepted | `driver.submit` for `inline-*` (hash recorded in the Run Scope), re-checked at terminal emission and Run Scope disposal | hash equal or `change-set.accepted` for this Run |
| 2 | every terminal Run state is on the Tape | the seal | exactly one of `run.completed/failed/cancelled` before any terminal emission; Tape failure → `error (:status audit-incomplete)` |
| 3 | every tool execution passes Authority | `tools.execute` (only path to a tool body; `materialize` hands out wrappers) | `authority.decide` → allow and `approval.decided` recorded before the body ran |
| 4 | model-visible text reconstructable from the Tape | driver, between `assemble-prompt` and `generate` | assembler `:receipt` appended as `prompt.assembled`; `intent.received` holds raw content |
| 5 | a Run delivers to exactly its Envelope's Sink | driver emission | every emission carries the Sink id captured at submit; `delivery.failed` on signal |
| 6 | a plugin contributes only through its Scope | mount (review; optional `superchat-plugin-audit` obarray diff) | advisory `plugin.leaked` |

## 2. Composition

### 2.1 `superchat-plugins`

A `defcustom` list of `NAME` or `(NAME . CONFIG-PLIST)`, mounted into the
global Scope at load in list order (order is the tie-break for same-Stage
contributors of equal Scope; dependencies are order-independent because
pending plugins wait). Remove a row before `(require 'superchat)` to disable a
built-in; `superchat-plugin-mount` / `-unmount` / `-reconfigure` do the same at
runtime. Third-party plugins append themselves.

### 2.2 Presets

`superchat-preset` gains a `:plugins` slot with the same element shape.
Activating a Preset creates `preset/<name>` under global and mounts its
plugins there, so a Preset can shadow Stage/Event contributors, tools,
commands and policies — never Services. Its other slots are consumed by
plugins through `:context (:preset P)`; a non-empty `skill-body` mounts a
`preset-persona` `system` collector automatically. Preset files declare
`plugins:` as names or `{name, config}` maps.

## 3. Default plugin inventory

Thirty-one plugins, one file each, no plugin `require`s another
(only `:inject`). Sizes are today's, for orientation.

| Plugin | inject | Registers | From today |
|---|---|---|---|
| `driver` | tape, closure | Service `driver`; Invariant 2/4/5 seams | `superchat-pipeline.el`, Run lifecycle of `superchat-runtime.el` |
| `transport-llm` | tools, tape | Service `model-transport`; emits `before-provider-request`, `context-budget-exceeded` | `superchat-llm.el` (minus tool selection), `superchat-models.el`, `superchat-agent-loop.el` |
| `transport-mock` | — | Service `model-transport` (scripted) | new (verification) |
| `tape` | — | Service `tape` | `superchat-tape.el`, `superchat-projection.el`, tape half of `superchat-db.el`, `superchat-save.el` |
| `session-view` | tape | Service `session` | `superchat-tape-view.el` |
| `closure` | tape | Service `closure`; emits `closure-shadow` | `superchat-closure.el`, closure guard of runtime |
| `authority` | tape | Service `authority`; approval UI | `superchat-authority.el` minus `superchat-tool-execute` |
| `tools` | authority, tape | Service `tools`; Invariant 3 seam; tool Events | registry + `superchat-tool-execute` + selection rule 1 |
| `tools-builtin` | tools, memory, session | built-in tools incl. `define_tool` | `superchat-tools.el` bodies, `superchat-executor.el`, `superchat-synthesis.el` |
| `tools-baseline` | tools | Stage `select-tools` | `superchat-llm.el:1663-1750` |
| `prompt-assembler` | — | Stage `assemble-prompt` | `superchat-prompt.el` |
| `chat-parse` | — | Stage `parse` | `superchat-core.el`, `superchat-parser.el` |
| `chat-collectors` | session, memory | seven `collect-context` contributors | `superchat-prompt-hooks.el` |
| `chat-view` | driver, session, editor-workspace, view | Service `view`; the chat window | `superchat-render.el`, View half of `superchat.el`, `superchat-widgets.el` |
| `chat-commands` | driver, session, commands | Service `commands`; `admit` (`handled`); built-in slash commands | command alist + handlers of `superchat.el`, `superchat-dispatcher.el` |
| `chat-session` | driver, editor-workspace | `resolve-session`, `load-state`, `admit` (queue); follow-up dispatch | dispatcher queue branch, queue half of runtime |
| `editor-workspace` | tape | Service `editor-workspace`; `session-started/closed` | `superchat-editor-workspace.el` |
| `change-set` | tape | Service `change-set`; Invariant 1 writer | `superchat-change-set.el` |
| `inline` | driver, change-set, view | Surfaces `inline-ask`/`inline-rewrite`; `inline-reference` collector; overlay preview | `superchat-inline.el`, `superchat-rewrite.el` |
| `memory` | tape | Service `memory`; `/remember` `/recall` | `superchat-memory.el`, memory half of `superchat-db.el` |
| `compact` | driver, session, tape | `/compact` `/expand`; `context-budget-exceeded` listener; own collector | `superchat-compact.el` |
| `experience` | tape, session, closure | `run-completed` listener; `/review`; recovery view via `superchat-service-get` | `superchat-experience.el` |
| `claims` | tape | claim capture/correct | `superchat-claims.el` |
| `plan` | driver, tools | `plan` collector; `turn-completed`; `emit_plan`; `/plan` | `superchat-plan.el` |
| `subagent` | driver, tools | `delegate_to_subagent`; Surface `subagent`; own collector | `superchat-subagent.el` |
| `agents-panel` | driver | `turn-completed`; the panel | `superchat-agents-panel.el` |
| `workflow` | driver | `admit` (`handled` for `>>`); Surface `workflow`; own collector | `superchat-workflow.el` |
| `skills` | driver, commands | `>skill`; Surface `skills`; own collector | `superchat-skills.el` |
| `preset` | commands | Preset loading/activation and Preset Scopes | `superchat-preset.el`, `superchat--activate-capability` |
| `genui` | tools | `render_ui`; TextUI | `superchat-genui.el` |
| `mcp` | tools, authority | MCP servers as tools | `superchat-mcp.el` |
| `workspace` | tools, tape | artifact tools; `save-state` contributor | `superchat-workspace.el` |
| `tape-browser` | session | the Tape browser | `superchat-tape-browser.el` |
| `emacsclient` | driver, session | JSON Run surface | `superchat-emacsclient.el` |
| `org-babel` | driver | blocking adapter | `ob-superchat.el` |
| `magit` | driver | commit-message surface | `superchat-magit.el` |

Deleted outright: `superchat-extension.el` and `superchat-define-extension`;
the Turn struct and `superchat-core-prepare-turn`; `superchat-runtime-submit`
and the legacy request shape; `superchat-pipeline-envelope-from-request`; the
legacy prompt bridge; the stage stubs; `superchat-stage-report`. Experience,
Claims, GenUI and emacsclient stay in the default list (user ruling).

Honest expectation: this is a re-partition, not a diet — deletions ≈ 1.5–2k
lines, Kernel ≈ 1.5k new; the gain is that any row of `superchat-plugins`
can be dropped and the rest still loads.

## 4. Reference plugin: Telega

One file. `:inject (driver tools authority commands session tape)`; mounted
in the global Scope. In `:apply`: declare the three Surfaces; register three
tools, two policies and one command through the Services; register a
`collect-context` contributor and a `select-tools` contributor that act only
when `(plist-get env :surface)` is one of its Surfaces — which is everything
`:active-when` used to express. It submits Envelopes directly
(`superchat-service-call scope 'driver 'submit …`) with its own Sink and
`:context (:telega-chat-id … :telega-autonomy …)`. Its per-autonomy Agent
Profiles are data in the Envelope context, not Preset Scopes. Unmounting it
must leave no `telega*` tool, policy, command, Surface or contributor, and a
Telega Envelope then fails validation with `unknown surface`. Full sketch:
`.wayfinder/tickets/008-telega-reference-port.md`.

## 5. Migration and verification

Seven steps; each is "add without changing behaviour" except step 3. Every
step ends with the smoke script. Order is fixed: no file is split before the
Services exist, or the `require` cycles just become `declare-function`s.

| Step | Change | Behaviour change | Check |
|---:|---|---|---|
| 0 | Kernel primitives + `transport-llm` wrapper + `transport-mock` | none | prototype checks pass in-package; one chat turn answers from the mock script |
| 1 | Scope accounting for every registering call and `add-hook` | none | unmount `cowork-telega` → no `telega*` anywhere; remount → Smoke F |
| 2 | Twelve Services; every cross-file call → `superchat-service-call`; probes → `superchat-service-get`; runtime calls `model-transport` | none | Smoke A–F identical; A with the mock |
| 3 | One prompt path; `parse` Stage; Events replace hook lists | **yes** | receipt (Invariant 4) byte-equal to pre-step capture for the same script; D/E for subagent/workflow |
| 4 | Built-ins as plugins; delete stubs, decorative stages, legacy lists, `superchat-extension.el`; Telega port | none | Smoke A–F; `superchat-plugins` minus `memory` loads and A passes without a `memory` fragment |
| 5 | File split per §3 | none | batch load + byte-compile clean; Smoke A–F |
| 6 | Docs (`plugin-guide.md`, `AGENTS.md`, `CONTEXT.md`) | — | the guide's example plugin mounts |

**Smoke script** — `scripts/smoke.el`, ~20 lines per case,
`emacs -Q --batch -L . -l scripts/smoke.el CASE`; model = what LM Studio
(`localhost:1234/v1/models`) lists, `transport-mock` for deterministic cases.
Baselines are captured from the current code *before* step 0 into
`scripts/smoke-expected/` and are never re-baselined without a note in
`.wayfinder/map.md`.

| Case | Journey | Tape sequence compared |
|---|---|---|
| A | chat, one plain turn | `intent.received prompt.assembled response.completed run.completed turn.completed` |
| B | chat, one approved `read_buffer` call | A with `tool.execution.started approval.decided tool.completed` between `prompt.assembled` and `response.completed` |
| C | chat, send during a Run, then cancel | `queue.queued run.cancelled queue.dispatched run.completed` |
| D | inline ask on a scratch buffer | A with `:surface inline-ask`; buffer hash unchanged |
| E | inline rewrite → accept / → reject | accept: `change-set.accepted` + hash changed; reject: no accept Fact, hash unchanged |
| F | Telega digest with the mock | A with `:surface telega-digest`; both policies in `approval.decided` |

## 6. Open items (not blocking implementation)

- MELPA packaging: one package, `superchat-plugins` as the only switch;
  third-party plugins are ordinary packages that `(add-to-list 'superchat-plugins …)`.
- `superchat-plugin-audit` (Invariant 6) is optional tooling; ship it only if
  the review burden proves real.
- pi-style hot reload of plugin files and a marketplace remain out of scope.
