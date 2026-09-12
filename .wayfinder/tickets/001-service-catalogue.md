---
title: Service catalogue and interfaces
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: []
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
Which Services exist in v2, what is each one's required method list, and which default plugin provides it? Candidates from the review: model-transport, authority, tape, session-store, tools, view, driver (Q8 b), commands, surfaces. Decide names, methods, and what a consumer may assume when a provider is absent.

## Resolution (2026-09-11, sc-leader)

**Plain language.** A Service is a named capability with a fixed list of
verbs; exactly one mounted plugin provides it at a time; anyone else calls it
by name and never `require`s the provider's file. Twelve Services cover what
the top-level files call across file boundaries today (measured: the public
functions each file exports that *other* files actually use).

| Service | Required methods (verbs) | Default provider plugin (today's code) | If absent |
|---|---|---|---|
| `driver` | `submit ENVELOPE → handle`, `cancel RUN-ID &optional REASON`, `steer RUN-ID TEXT`, `status RUN-ID`, `active-runs`, `pending-input RUN-ID`, `answer-input RUN-ID HANDLE ANSWER`, `enqueue-follow-up SESSION ENVELOPE`, `pending-follow-ups SESSION`, `clear-follow-ups SESSION REASON` | `driver` (`superchat-pipeline.el` + the Run lifecycle half of `superchat-runtime.el`) | no Surface can submit; producers stay PENDING |
| `model-transport` | `generate PROMPT-SPEC TOOLS SINK → outcome` (PROMPT-SPEC = the assembler result: `:system`, `:messages`, `:receipt`; outcome per contract §5), `capabilities MODEL → (:streaming BOOL :tools BOOL)` | `transport-llm` (`superchat--llm-generate-answer` loop, `superchat-llm.el:2281`, provider structs `:132`) | driver PENDING; a mock transport plugin is the no-test-suite verification tool (ticket 009) |
| `tape` | `append-event EVENT`, `append-material SESSION KIND TEXT`, `append-anchor …`, `replay SESSION &optional SINCE LIMIT`, `register-projection NAME FN`, `replay-projection NAME` | `tape` (`superchat-tape.el`, `superchat-projection.el`, tape half of `superchat-db.el`) | nothing runs — `driver` injects it (Invariant 2) |
| `session` | `entries SESSION &optional SINCE KINDS LIMIT`, `search QUERY &optional SESSION LIMIT`, `assemble-view SESSION &key …` (bounded Context View), `superseded-ids SESSION`, `history SESSION`, `configuration SESSION`, `session-ids` | `session-view` (`superchat-tape-view.el`) | history/memory collectors contribute nothing; driver still runs |
| `authority` | `decide TOOL ARGS ATTENDANCE &optional POLICY EFFECTS → decision`, `resolve-effects TOOL ARGS`, `register-policy NAME FN` / `register-effect-resolver TOOL FN` / `register-approval-preview TOOL FN` (all Scope-reversible; the `unregister-*` trio is deleted) | `authority` (`superchat-authority.el`) | `tools.execute` denies everything (default-deny, Invariant 3) |
| `tools` | `register NAME &key function description args async effects approval-preview` (Scope-reversible; `superchat-unregister-tool` deleted), `descriptors → names`, `materialize NAMES → llm-tool list`, `execute NAME ARGS ATTENDANCE → result` (the only path to a tool function; injects `authority`, `tape`) | `tools` (`superchat-tools.el` registry + `superchat-tool-execute` from `superchat-authority.el:` moved here; built-in tool bodies become their own plugin `tools-builtin`) | model sees no tools |
| `view` | `sink-for SURFACE CONTEXT → sink`, `markdown-to-org TEXT` | `chat-view` (`superchat-render-view-sink` `superchat-render.el:1558`, `superchat-render-markdown-to-org`) | producers that pass their own Sink are unaffected; chat cannot open |
| `commands` | `register NAME HANDLER &optional DOC` (Scope-reversible), `lookup NAME`, `list` | `chat-commands` (the alist at `superchat.el:4321-4368` + `superchat-register-command`) | `/name` input is plain text |
| `closure` | `judge QUERY TRACE → verdict` | `closure` (`superchat-closure.el`) | driver seals without judgment and records `closure.absent` (a Fact, not a failure) |
| `editor-workspace` | `validate-buffer BUFFER SESSION`, `buffer SESSION ROLE &optional CREATE`, `owner-session BUFFER`, `acquire-buffer …`, `rollback-acquisition …`, `close SESSION` | `editor-workspace` (`superchat-editor-workspace.el`) | chat surface cannot host a View; inline surfaces unaffected |
| `change-set` | `propose HOST-BUFFER EDITS → id`, `accept ID`, `reject ID`, `apply ID` (the only writer into a host buffer, Invariant 1) | `change-set` (`superchat-change-set.el`) | inline rewrite cannot be accepted; nothing is written |
| `memory` | `insert`, `search QUERY …`, `search-prioritized …`, `get ID`, `list-review`, `set-review`, `count`, `reset-projection` | `memory` (`superchat-memory.el` + memory tables of `superchat-db.el`) | memory collector and `memory_search` tool contribute nothing |

**Rules.**
- `superchat-workspace.el` (artifacts: bind/publish/read/write/artifacts) is
  *not* a Service; it becomes a plugin that registers tools and a
  `save-state` contributor. `superchat-register-surface` is Kernel Envelope
  vocabulary (a name list validated by the driver), not a Service.
- A consumer declares `:inject (SERVICES…)` and is PENDING until every one is
  provided (ticket 006). An *optional* use goes through
  `superchat-service-get SCOPE NAME → provider-or-nil`; the `fboundp` /
  `with-eval-after-load` probes at `superchat-llm.el:1443`,
  `superchat-pipeline.el:266`, `superchat-skills.el:384`,
  `superchat-runtime.el:536` are all replaced by one of these two forms.
- Provider tables are checked at mount against the method list (missing method
  = mount error, plugin `failed`). Extra methods are allowed but invisible to
  consumers (they are not part of the interface).
- Services are global-Scope only in v2 (a Preset may *not* shadow a Service;
  Stages/Events/tools/commands/policies may). Reason: provider swap = every
  dependent unwinds and re-applies (Cordis epoch); doing that per Preset
  switch would restart the driver mid-session.

**Overturnable by the user:** whether `closure` is a Service or a `save-state`
contributor (kept a Service because the driver *awaits* its verdict before
sealing); whether `editor-workspace` deserves Service status (kept because 10
files call it).
