---
label: wayfinder:map
title: Superchat architecture migration — historical record
created: 2026-09-11
tracker: local-markdown (.wayfinder/tickets/*.md; frontmatter status/type/assignee/blocked-by)
---

## Reading this historical record

This file preserves the original specification decisions, migration observations and accepted repair-batch records. It is not the remaining task list. The only active route is [docs/roadmap.md](../docs/roadmap.md); implemented interfaces are described by the [architecture map](../docs/architecture.md) and [composition contract](../docs/design/plugin-architecture.md). Future-tense notes below retain their meaning at the time they were written, not authority over the current plan. Some referenced scratch reports have been removed; their names are historical references, not prerequisites to recreate.

## Original specification destination

A spec at `docs/design/plugin-architecture.md` that fixes, for superchat v2:
the Kernel vocabulary (Envelope, Sink) and its five registries (Plugin/Scope,
Service, Stage, Event, Invariant); the plugin shape `name / inject / config /
apply`; the Service catalogue with interfaces; the Stage main line; the Event
catalogue; the Invariant list; the default-plugin inventory and Preset
composition; and the six-step migration order with a per-step verification
that needs no test suite. Telega is designed as the reference plugin. When the
spec answers every question an implementer would ask, the map is done.

## Notes

- Domain: Emacs Lisp; the repo's glossary is `CONTEXT.md` (Composition section
  added 2026-09-11). Every session should consult `mattpocock-skills:grilling`
  and `mattpocock-skills:domain-modeling`; plan rounds must be plain language
  (one-word answers possible).
- Reference models read from local sources: Cordis / DeepSeek Harness
  (`~/Documents/reference-code/deepseek-harness`, docs/cordis-primer.md,
  packages/core/scope), pi (`~/Documents/playground/pi-mono`), bub.
- Standing rulings: v2 may break the public plugin API once; Experience,
  Claims, GenUI, emacsclient stay shipped-by-default; no test suite is being
  rebuilt; JJ is the VCS (`jj status`).
- Evidence for the current state: `.scratch/arch-review-fable.md`,
  `.scratch/modularization-path.md`.

## Decisions so far

- [Destination and scope (chart session)](tickets/000-charting.md): spec
  deliverable; all six migration steps in scope; API may break once.
- [Plugin model](tickets/000-charting.md): Cordis-style plugin
  `name/inject/config/apply`; Scope with reversible registrations, runtime
  unmount required; `superchat-define-extension` deleted, activation =
  composition + event listening.
- [Vocabulary](tickets/000-charting.md): Stage = fixed main line, Event =
  declared extension point with 4 combination rules (names unchanged);
  Preset also names plugins; Scope tree = global → Preset → Run.
- [Kernel boundary](tickets/000-charting.md): driver is a Service (replaceable);
  Invariant registry holds the non-negotiable rules; Kernel = loader + five
  registries + default mount list + Envelope/Sink vocabulary.
- [Service representation](tickets/000-charting.md): definition = required
  method list; provider = method→function table, checked at mount.
- [Composition](tickets/000-charting.md): `superchat-plugins` list variable,
  runtime mount/unmount.
- [Cordis/dsh semantics research](tickets/010-cordis-semantics-research.md):
  losing a Service provider suspends dependents (effects unwound) and
  re-applies them when a provider returns; scoped lookup is global then
  nearest-scope-wins with empty layers reclaimed at once; config changes are
  validate-then-remount, never in place. Findings in
  `.wayfinder/research/cordis-semantics.md`.
- [Kernel primitives prototype](tickets/006-kernel-prototype.md): feasible in
  250 lines of Elisp with no `unload-feature`; 13 checks pass
  (`.wayfinder/research/kernel-prototype.el` + `-drive.el`). Spec must say:
  registrations land in the mount Scope while effects unwind through a
  private child (`layer-owner`); registry keys compare with `equal`; notify
  is a synchronous fix-point; "contribute only through the Scope" is an
  Invariant checked by review, not enforceable in Elisp.
- [Service catalogue](tickets/001-service-catalogue.md): twelve Services —
  driver, model-transport, tape, session, authority, tools, view, commands,
  closure, editor-workspace, change-set, memory — each a required-method list
  with a named default provider plugin and a stated absent-behaviour;
  Services are global-Scope only (no Preset shadowing); optional use goes
  through `superchat-service-get`, replacing every `fboundp` probe.
- [Stage main line v2](tickets/002-stage-main-line.md): eight Stages —
  parse, resolve-session, admit (new action `handled`), load-state,
  select-tools, collect-context, assemble-prompt (replaceable), save-state —
  each with a named default plugin; `run-model` is the `model-transport`
  Service call, `render`/`dispatch`/`register-*` deleted, `on-error` → Event;
  one prompt path: `superchat-core-prepare-turn` and the `:prepared-prompt`
  bypass are deleted, chat and the four hand-built producers become plain
  producers with their own collectors.
- [Event catalogue](tickets/003-event-catalogue.md): fifteen Events with
  fixed rules (stage-error, run-started, run-sealing, run-completed,
  turn-completed, six tool-lifecycle Events, before-provider-request,
  context-budget-exceeded, closure-shadow, session-started/closed) replace
  all eighteen `superchat-*-functions` variables; the two legacy prompt hook
  lists, pre/post-topic and command-hooks are deleted.
- [Config and Preset](tickets/005-config-and-preset.md): `:config ((KEY
  PREDICATE DEFAULT [DOC])…)` validated on every activation; `superchat-plugins`
  = list of NAME or (NAME . CONFIG); Preset gains `:plugins`; Scope lifetimes
  global (require) → preset/<name> (activate/deactivate) → run/<id>
  (submit/terminal); Session is not a Scope level.
- [Invariants](tickets/004-invariants.md): six Kernel-checked rules —
  host buffer unchanged until Change Set accepted, terminal Run state on
  Tape, tool execution passes Authority, model-visible text reconstructable
  from the Tape, exact-Sink delivery, contribute-only-through-Scope
  (advisory) — each with its seam; violations are `invariant.violated` Tape
  Facts; `superchat-invariant-report` replaces `superchat-stage-report`.
- [Default plugin inventory](tickets/007-default-plugin-inventory.md):
  Kernel = six files (~1.5k lines: plugin, service, stage+event, invariant,
  sink, envelope) + the loader; thirty-one default plugins, one file each,
  mapped from today's 46 files; deleted outright: `superchat-extension.el`,
  the Turn struct and `superchat-core-prepare-turn`, `superchat-runtime-submit`,
  `superchat-pipeline-envelope-from-request`, the legacy prompt bridge, the
  stage stubs and `superchat-stage-report`. Net line change ≈ −1k; the gain is
  droppability, not size.
- [Telega reference plugin](tickets/008-telega-reference-port.md): one file,
  `:inject (driver tools authority commands session tape)`, mounted in the
  global Scope; three tools, two policies, one command, a surface-guarded
  collector and a `select-tools` contributor registered through the Scope;
  `:active-when` disappears (surface checks replace it); it submits
  Envelopes directly, so `superchat-pipeline-envelope-from-request` and
  `--compose-request` are deleted; unmount must leave no `telega*` entry.
- [Migration and verification](tickets/009-migration-and-verification.md):
  seven steps (0 mock transport + Kernel primitives, 1 Scope accounting, 2
  Services, 3 one prompt path — the only behaviour change, 4 built-ins as
  plugins + Telega port, 5 file split, 6 docs); six smoke cases A–F against
  LM Studio / `transport-mock` comparing Tape event sequences (and the
  prompt receipt for step 3) with baselines captured before step 0.

## Not yet specified

- Rewrite of `docs/plugin-guide.md` and `AGENTS.md` Kernel table — migration
  step 6, after the code moves.

## Resolved along the way

- Experience/Claims attach as Event listeners (`run-completed`) plus
  `superchat-service-get` for the recovery view; GenUI is a `tools` plugin;
  the `view` Service is provided by `chat-view` (ticket 007).
- org-babel, magit, MCP, emacsclient are plugins injecting `driver`/`tools`
  (ticket 007).
- Kernel file naming: six files (§1 of the spec); packaging: one package,
  `superchat-plugins` the only switch (spec §6).

## Implementation log

- Step 0 (c0bf109d): Kernel primitives, transport-llm/transport-mock, smoke
  baselines A-F (+M). Step 1 (e85f7aad): Scope accounting; Telega is a
  plugin; smoke U. Step 2: the twelve Services declared with default
  provider plugins in `superchat-default-plugins.el` (one file until step
  5); the named `fboundp`/`with-eval-after-load` probes replaced. Decision:
  the remaining direct cross-file calls (≈90 sites) are switched to
  `superchat-service-call` per plugin during step 5's file split, when each
  file's `require's are cut — doing it earlier adds verbosity without
  removing a dependency.
- Step 3 (one prompt path): `superchat-core-prepare-turn` only parses; the
  chat Envelope carries `:chat-turn/:chat-file-ref/:chat-template/
  :chat-prompt-override` and the driver's collectors assemble (`chat-input`
  supplies the `content` fragment); the driver's prepared-prompt bypass is
  gone, replaced by the `prepared` collector; queued follow-ups are
  Envelopes submitted through the driver Service. Smoke A's model-visible
  prompt is byte-identical. Behaviour changes accepted: prompt-type skills
  now wrap only the template part (history/memory are separate fragments),
  prepared prompts are budgeted, `system-instructions` are chat/inline-ask
  only (what those producers received before).
- Step 4 (378e9f91, 7aaff543, +4e): Events replace the thirteen hook
  lists; tools-baseline is the select-tools default plugin; the Stage table
  is the seven-Stage main line; the legacy prompt hook chain is deleted;
  `superchat-extension.el` is deleted and Telega registers directly (tools
  carry `:surfaces`, a surface-guarded collector replaces `:active-when`).
  **Re-baselined:** smoke F no longer records `extension.activated` (the
  extension layer is gone; the event type stays declared for old Tapes);
  smoke U counts the Telega collector instead of the extension entry.
- Step 5a/5b: every default provider plugin is defined in the file that
  owns its code (`superchat-default-plugins.el` keeps only the lazily
  loaded experience/plan/agents-panel/compact); `superchat-service-table`
  builds provider tables; the driver Service gained `submit-blocking` and
  tape `append-event-record`; ob-superchat, magit, tape-browser,
  emacsclient, agents-panel, change-set, inline and rewrite reach other
  plugins only through Services (their cross-plugin `require's are gone).
- Step 5c: compact, workflow, subagent, skills, plan, workspace, experience
  and claims reach driver/tape/session/editor-workspace/closure/authority
  through Services (methods added: driver `terminal-p`, tape
  `storage-reference-p exact-json select`, session `anchor-latest
  anchor-source-ids expand-anchor item-line`, authority
  `transport-timer-owner`). Load-time projection registration stays a
  direct `superchat-projection-register` call (it runs before plugins
  mount). Not done: renaming files one-plugin-per-file (superchat.el,
  superchat-llm.el, superchat-runtime.el, superchat-dispatcher.el keep
  their names and sizes) — a pure move with no seam value; left for a
  later session if the user wants it.
- Step 6: `docs/plugin-guide.md` rewritten around the four pieces (with a
  fortune example that mounts and unmounts), `AGENTS.md` layering section,
  `docs/architecture.md`, README/README_cn, CHANGELOG. **Not implemented in
  code:** the Invariant registry (§1.7) and the `parse` Stage; both recorded
  in the spec's status line.
- Review (sc-fable, `.scratch/migration-review-fable.md`) after step 6:
  fixed F1 (prepared prompts are no longer re-budgeted — compaction's input
  is by design a large slice of the window), F5 (agent/plan skills keep the
  general template), F7 (driver injects closure), F4 (README/architecture
  and stale docstrings). Recorded, not fixed: F2 (16 of 36 plugins are not
  yet defined; built-in registrations are load-time, so only Telega's
  unmount is trace-free), F3 (four Events declared but never emitted; Event
  argument shapes are narrower than spec §1.5), F6 (compact→llm,
  rewrite→inline, agents-panel→subagent, subagent→closure/authority,
  workspace→closure/tape requires remain). F7's autoload point was wrong:
  `superchat.el:77-87` autoloads those listeners explicitly.
- Smoke coverage extended (user: "同意"): K compaction beyond the budget
  (fails without the F1 fix), S prompt- and agent-type skills (template and
  persona both present), T user command template, W two-step workflow, L a
  `tool-permission` listener denying a tool, O org-babel, I emacsclient.
  Baselines captured on the reviewed code.
- Review F2 follow-up: the chat collectors, prompt assembler, chat admit,
  inline collector and the built-in tools' effect resolvers/previews now
  register from default plugins (chat-collectors, prompt-assembler,
  chat-session, inline, tools-builtin); smoke V proves unmounting each
  withdraws its registrations and remounting restores them. Still
  load-time: the command alist seed, the Surface seed list (Kernel
  vocabulary), projections, and subagent/workflow/skills/mcp/genui/
  preset/workspace entry points (autoloaded commands and tools already
  covered by tools-builtin).
- Code-level follow-ups (user: "把代码层面需要修改的全部修了"): Events
  run-started/session-started/session-closed/before-provider-request are
  emitted with spec-shaped arguments and listener faults are recorded;
  compact and workspace lost their cross-plugin requires (model-transport
  gained `context-budget`); projections register from plugins
  (`superchat-projection-unregister`, Scope-accounted) and subagent,
  workflow, workspace, experience own theirs; the built-in slash commands
  are registered by chat-commands from `superchat--builtin-command-handlers`;
  `parse` is the first Stage (fold over the chat Turn, `chat-parse` plugin).
  33 of the 36 inventory plugins exist (missing: skills, mcp, genui, preset —
  their tools are covered by tools-builtin; their entry points autoload).
- File split (item 6, partial): tool selection (contract §4.2, ~430 lines)
  moved from `superchat-llm.el` into `superchat-select-tools.el` together
  with the `tools-baseline` plugin, so the model transport no longer owns
  tool policy. `superchat.el` (4.7k) has no internal section structure
  (one heading in 4,700 lines) and the chat UI has no smoke coverage of
  its own; splitting it blind was judged too risky for no seam gain —
  left for a session that starts by giving the UI its own checks.
- Invariant registry (item 1): `superchat-invariant.el` defines the six
  Invariants; checks run at the driver (`prompt.assembled` receipt), the
  seal (terminal on Tape), terminal delivery (exact Sink; inline host hash
  vs `edit.applied`) and `superchat-tool-execute` (decision before body);
  violations are `invariant.violated` Tape Facts, check errors
  `invariant.check-failed`. **Re-baselined:** every case whose Run goes
  through the driver now records `prompt.assembled` before
  `intent.received`; no case records a violation.
- Review 2 (sc-fable, `.scratch/migration-review-2-fable.md`): fixed —
  Session Views collapse `prompt.assembled`/`invariant.*` rows as audit
  rows; Invariant 1 checks `inline-rewrite` only; no full-Tape replay at
  the seal (the append's own record is checked) and the `edit.applied`
  lookup runs only when bytes differ; Invariant 3 validates the effective
  decision; the zero-argument built-ins (/backend, /models, /mcp*,
  /refresh-models) go through the command registry; smoke P sends a
  120k-char prepared prompt through the driver with the mock transport (the
  real F1 scenario) and completes. Recorded: Stage/Event implementations
  are global (no Preset-Scope isolation); user `.md` commands remain a
  template table outside the registry; Invariant 5 is tautological today.

## Specification milestone completed (historical, 2026-09-11)

Tickets 001–009 closed the specification-writing milestone. This was not acceptance of the complete implementation. Subsequent observations and repair batches are recorded below; remaining work belongs only to [the current roadmap](../docs/roadmap.md).

## Out of scope

- pi-style hot reload of plugin files from disk and a plugin marketplace /
  installer.
- Moving Experience, Claims, GenUI or emacsclient out of the default set.
- Rebuilding the deleted test suite.


## Architecture review repair — batch 1 (2026-09-12)

- Fixed the required `model-transport` dependency and removed Runtime's LLM
  fallback; Service disposal now withdraws before notifying dependents.
- Moved public synchronous submission to the Envelope pipeline and migrated
  Org/Magit. Blocking capture handles orphaning and expires on exit, so a
  rejected queued Envelope cannot execute later. Original terminal Sink
  error/quit conditions and timeout cancellation have focused checks.
- Tools use existing Scope registration ids and installation-owned cleanup;
  same-layer duplicates fail before Authority writes. Required collect-stage
  validator rejection/error now terminates just like execution failure.
- Added positive smoke contracts R1–R4 (all four first failed against the
  pre-repair production code), plus direct-disposal/old-undo Kernel checks
  (both failed against the base Kernel). No old baseline was changed or
  re-captured. O still matches because its baseline records result, provider
  count and active Run count, not event types; its now-real pipeline appends
  `prompt.assembled`, as covered by R2's collection/receipt checks.
- Full Scope/Preset routing and prompt evidence reconstruction remain deferred.
  See `docs/plugin-guide.md` for the explicit API compatibility and in-flight
  unmount boundaries. No Scope cleanup closes editor buffers.

- Review follow-up: manual Service undo and Scope disposal now share the same
  notify-after-withdraw operation. Blocking caller quit cancels only its own
  accepted active Run without changing the original condition. Both added
  checks failed before their fixes. Authorized MCP refresh compatibility is
  included in R3: validate before replacing own tools, retain invalid updates,
  retain all tools on collection failure, and remove stale tools on success.
- The following in-flight observation described the pre-batch-2 behavior only:
  transport-llm could complete after unmount while transport-mock silently
  cleared its timer.  It is superseded by the approved withdrawal contract
  below, not a current provider-specific lifecycle guarantee.

- Independent review caught a refresh-retention regression: unregister removed
  the tool but left its two cleanup closures on global Scope.effects (28 to
  228 after 100 refreshes). Scope effect undo is now self-removing and one-shot;
  tool explicit unregister invokes its tracked cleanup, and Service/plugin undo
  adapters use the same mechanism. The original probe now reports 28 to 28.
  R3 adds 100 actually changing descriptors/implementations and return-to-baseline
  assertions; Kernel adds manual/dispose, order, reentry and adapter checks.

## Architecture review repair — batch 2 (2026-09-12)

- Transport withdrawal/replacement is now a Runtime epoch boundary.  Provider
  plugin unmount, direct provider Scope disposal, a returned provider undo, and
  replacement revoke every matching nonterminal Run together before the unique
  Runtime cancel path visits any one of them.  The driver Service is withdrawn
  first, so reentrant notifications cannot submit through the retiring epoch;
  only the provider `:id` is retained in each historical Run, not its provider
  table/closures.
- This deliberately changes R1's old in-flight observation: an old
  `transport-llm` or `transport-mock` Run now terminates immediately rather
  than being allowed to complete (or silently lose only its mock timer).  A
  closure-ready Run records `run.cancelled`; an unresolved Effect/Approval
  preserves its real residual and uses the existing `audit-incomplete`
  exception.  No Closure rule was loosened and no fake tool/approval settlement
  is written.  The static R1 baseline remains `contract: passed`; its focused
  assertions changed, and none of the A–P baselines was re-captured.
- An already-entered external async tool cannot be revoked generically.  At
  withdrawal its Run becomes effect-incapable and its residual stays open; if a
  genuine late result later arrives, its normal one-time `tool.completed` Fact
  accounts that existing Effect (the pending Effect goes `open` → `proved`)
  while the terminal Run remains `audit-incomplete`.  It cannot restart the
  model loop, issue a new provider request, or claim that the external side
  effect was undone.
- R1 now covers old-provider cancellation, mock cancellation, direct Scope and
  returned-undo retirement, late model callbacks/retries/tool calls, ordinary
  and async tool boundaries, FIFO pause/resume, and reentrant startup,
  run-sealing, Sink-liveness, and post-seal cleanup failures.  The FIFO remains
  the sole queue: unavailable transport preserves `queue.queued` without a
  busy retry or `queue.dropped`, then resumes it after remount.
- The terminal finish path has one reentrant owner: a nested terminal seal wins
  over an older outer finish.  The former post-seal audit fallback is superseded
  by batch 3 below.  No Scope or Runtime teardown closes a user or
  Session-owned buffer.

## Architecture review repair — batch 3 (2026-09-12)

- A stable event ID now crosses each Runtime Run/Turn append's SQLite
  INSERT/receipt boundary.  If the ordinary Run terminal is confirmed after a
  lost receipt, Runtime preserves that terminal status, reports later work in
  the public Run error, delivers once through its original Sink slot, and
  quiesces; it never appends a competing `audit-incomplete` Run terminal.
- A readable, unique ordinary Run seal may recover only one matching ordinary
  `turn.completed` suffix through the existing protected per-Run sidecar.
  Reconciliation rechecks the Run, Turn, status, causation, and stable identity
  before append.  Read failure is an explicit `unknown`: it queues neither an
  audit pair nor a guessed Turn tail.  The audit pair remains the narrow
  fallback only when readable Tape proves no Run terminal of any kind.
- R5 adds real SQLite pre-INSERT and post-INSERT/lost-receipt checks for
  completed/cancelled Run and Turn seals, plus unreadable lookup cases.  It
  asserts a single Run seal, at most one Turn seal where evidence is unreadable,
  Closure validity after readable replay, exactly-once `run-sealing` and Sink
  delivery, and quiesce.  Its blocking Turn-write fault cases also assert that
  terminal consumer `error`/`quit` is propagated once while a later recovered
  Turn suffix leaves the trace causally valid.  R1's focused post-seal
  assertion now expects the preserved `cancelled` status rather than
  `audit-incomplete`; the static R1 expected line remains `contract: passed`.
  No A–P baseline was re-captured.

## Architecture review repair — batch 4A (2026-09-12)

- Stage implementations now live in their existing `(stage NAME)` Scope layer
  rather than in the global declaration table.  This makes a Telega
  `collect-context` contributor visible to the Stage trace used by smoke U;
  consequently, and only consequently, U's mounted/remounted registration
  counts change from 12 to 13.  Its post-unmount trace remains nil.  No other
  pre-existing smoke baseline was re-captured.
- An explicitly supplied caller Scope now flows through pipeline Stage lookup,
  Runtime lifecycle/Event and Sink callbacks, first/retry provider-request
  folds, and queued follow-up dispatch.  A nested `superchat-service-call nil`
  inherits that execution Scope; explicit `superchat-scope-global` escapes to
  global, while Service providers themselves remain global-only. Runs and FIFO
  items borrow the Scope only in process memory; terminal quiesce clears the
  historical Run reference without disposing it.
- This is a routing slice, not the promised Preset/Run lifecycle: it creates no
  Scope, does not give Scope disposal Runtime cancellation or buffer ownership,
  and deliberately leaves Authority tool Events plus tools/commands/policies
  on their current product-global paths. R6 covers Stage shadowing/siblings,
  async lifecycle and retry, nested requests, FIFO restoration, public-data
  exclusion, and repeat-safe cleanup.

## Architecture review repair — batch 4B (2026-09-12)

- Product registrations now use the existing Scope layers as their only truth:
  tools, commands, Authority policies, standalone effect resolvers, and
  approval previews target explicit `:scope` first, then applying plugin effect
  Scope, current execution Scope, and finally global.  Service providers remain
  global-only.  All product reads (including command dispatch/list/help and
  tool descriptors) use the visible Scope view; no writable global mirror or
  second installation registry remains.
- Same-layer duplicate registration rejects before any paired product write.
  Parent-to-child shadowing remains valid; explicit unregister plus register is
  the intentional compatibility tightening for same-layer replacement.  Exact,
  self-removing undo means an old command/tool/Authority cleanup cannot remove a
  later local installation or an ancestor.
- A plugin tool descriptor carries an installation-private callable proxy from
  materialization into Authority.  Authority binds that exact identity per
  invocation, so an old materialized descriptor fails closed after revoke or
  replacement rather than mixing OLD implementation with NEW resolver/preview.
  This does not claim reversal of an external Effect: an invocation already
  entered may append its genuine late completion Fact, while a terminal Run
  stays terminal and never resumes its model loop.
- Authority policy constraints fold at execution time through the caller Scope
  chain (`deny` > `ask` > `defer`; contributed `allow` is `defer`).  The common
  LLM native boundary restores the accepted execution Scope for ordinary,
  request-local, prepared-Subagent, and retry tool callbacks and `tool-*`
  Events, without serializing Scope into Envelope/Tape/public status.  Commands
  retain file-template fallback when a handler declines or the provider is
  absent.  An unchanged MCP discovery is now a no-op on the owned installation;
  a changed descriptor still refreshes explicitly and server-effects remain
  dynamically resolved.
- R7 is a new focused expected contract; it covers the core Scope pairing,
  revocation, command fallback, policy, native callback, exact-evidence, and
  MCP no-op paths and is repeat-safe.  No existing A–P or R1–R6 baseline was
  re-captured.  This remains 4B only: automatic Preset/Run Scope creation,
  prompt-evidence reconstruction, and Scope resource ownership are deferred.
  The design has no Session Scope layer; it is not deferred future work.
- Kernel mount-cleanup repair: a plugin fiber retains the exact self-removing
  Scope effect that owns its mount.  Manual unmount clears that held reference
  and consumes the same effect before unload/notify can reenter; Scope disposal
  takes the identical path.  A retained old cleanup is inert after a same-name
  remount.  The Kernel 100-cycle check and repeated R7 now require the global
  `Scope.effects` baseline to remain stable, including `chat-commands`.
  This is intentionally limited to fiber cleanup ownership: generic
  children-first Scope disposal and resource ownership are unchanged.

## Architecture review repair — batch 4C-1 (2026-09-12)

- Execution Scope retirement is now a Runtime-owned, process-private latch on
  one explicit non-global Scope.  It follows object ancestry to descendants,
  not Scope/Preset/Session names.  Retirement neither withdraws registrations
  nor closes buffers.  Generic Scope disposal retains its existing
  registration-withdrawal behavior, but does not gain Runtime cancellation or
  buffer ownership.  The Driver Service exposes `retire-scope` for the future
  Preset owner.
- Runtime latches first, snapshots matching live and existing controlled-stop
  entries, then uses the ordinary cancel path so reentrant Sink/Event code
  cannot continue an old sibling, retry a provider/tool operation, or attach a
  new child to an old retired parent through an otherwise open sibling Scope.
  Existing controlled stop/terminal separation is deliberately preserved.
  Entered external Effects still retain truthful residual/late-result behavior;
  retirement neither fabricates settlement nor reopens a terminal Run.
- The existing FIFO retains each item's private borrowed Scope.  New retired
  enqueues are explicitly non-accepted; an append-time retirement remains
  non-accepted and is later auditably dropped as `scope-retired` in FIFO order.
  Required drop-write faults leave that item inert for a later scheduling
  attempt; transport withdrawal keeps its established pause/resume behavior.
- R8 is the new focused expected contract for latch ancestry, reentrant
  cancellation/parent admission, pipeline and direct-provider gates, and FIFO
  handling. No prior A–P or R1–R7 baseline was re-captured. Batch 4C-2 below
  supplies the Preset activation/switch owner; automatic Run-created Scope
  lifecycles remain future work. There is no Session Scope layer to implement.

## Architecture review repair — batch 4C-2 (2026-09-12)

- A Preset now carries a validated native `:plugins` composition: `NAME` or
  `(NAME . CONFIG-PLIST)`, with unique names and validated config before an old
  View composition changes. Markdown accepts only the existing inline list
  `plugins: [foo, bar]` (including `[]`); nested, map, and multiline forms are
  rejected rather than silently dropped. Full YAML config maps remain deferred.
- Each Chat View owns an exact direct-global-child `preset/<name>` Scope.
  Same-named Presets in different Views are distinct objects; Preset data itself
  never retains that Scope. Candidate validation/mount precedes configuration
  persistence and publication. A failed candidate preserves the old active Scope
  only while its owner View remains live; owner kill/mode-off/abort retires both
  old and candidate Scopes and publishes no replacement. A successfully appended
  configuration fact remains append-only rather than being rolled back. A pending
  Service dependency is a legal activation state. Restore of the same live Preset
  does not remount plugins or rebuild its Scope (though View text may render
  again) and creates no extra activation fact.
- A View switch, explicit stop, mode-off, approved kill, or major-mode exit
  calls Runtime retirement on its old Scope before generic Scope disposal
  withdraws plugin registrations. The owner path is identity- and buffer-based,
  not Preset-name or Session based; it closes no user or Session-owned buffer.
  Candidate work accepted before a failed activation is likewise retired before
  cleanup. An entered external Effect remains open while its Run is terminal
  `audit-incomplete`; a genuine one-time late completion may prove that Effect,
  but cannot alter the Run terminal or resume it.
- Ordinary chat captures one active View Scope through `preset-apply`, request
  construction, and FIFO enqueue/submit. A `:pre` callback that switches
  Preset cannot route its old prepared Turn through the new Scope: retirement
  rejects it. Explicit `>skill` and command mode-switch results intentionally
  resolve the fresh View Scope after their handlers return. FIFO retains its
  original borrowed Scope and records `scope-retired` rather than retargeting.
  A View without an active Preset uses global, does not retire global, and does
  not sweep unrelated Runs by Session identity. Scope/latches remain
  process-only, absent from Envelope, Tape, public status, and Sink data.
- R9 is the new compact repeatable contract for composition/shadowing, sibling
  Views, switch cancellation and FIFO routing, late external truth, candidate
  rollback/pending dependency/Driver absence, restore, mode-off/kill/major-mode
  ownership, ordinary `:pre` and `/plan` switch routing, and cleanup stability.
  No older baseline was re-captured. Full YAML plugin maps, automatic
  Run-created Scopes, prompt-evidence reconstruction, and a dedicated persona
  collector remain later slices; there is no Session Scope layer planned.
