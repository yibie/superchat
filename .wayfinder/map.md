---
label: wayfinder:map
title: Superchat plugin architecture v2
created: 2026-09-11
tracker: local-markdown (.wayfinder/tickets/*.md; frontmatter status/type/assignee/blocked-by)
---

## Destination

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

## Destination reached (2026-09-11)

`docs/design/plugin-architecture.md` written from tickets 001–009; every
ticket closed. Next step is code: migration step 0 (spec §5).

## Out of scope

- pi-style hot reload of plugin files from disk and a plugin marketplace /
  installer.
- Moving Experience, Claims, GenUI or emacsclient out of the default set.
- Rebuilding the deleted test suite.
