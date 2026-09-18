# AGENTS.md — Superchat

Superchat is an Emacs-native chat and Agent Runtime, packaged as one Emacs Lisp package over `llm.el`. This file is a navigation guide, not a second specification.

## Start here

- Read [CONTEXT.md](CONTEXT.md) and relevant [accepted ADRs](docs/adr/) before changing domain behavior; follow [domain navigation](docs/agents/domain.md).
- Use [docs/architecture.md](docs/architecture.md) for the current implementation, [docs/roadmap.md](docs/roadmap.md) for the only remaining plan, and [docs/README.md](docs/README.md) to place documentation. Accepted direction is not proof of implementation.
- The [composition contract](docs/design/plugin-architecture.md) and [request lifecycle contract](docs/protocols/request-lifecycle.md) describe current interfaces. `.wayfinder/` and the old fixed-pipeline specification are historical, not work queues.
- Work locally in the existing worktree. Preserve unrelated edits. Use jj when `.jj/` exists; do not rename modules merely to satisfy a file count.

## Locate the work

- UI and producers: `superchat.el`, `superchat-dispatcher.el`, `superchat-inline.el`, `superchat-rewrite.el`; the full ownership map is in [architecture](docs/architecture.md#composition-and-ownership).
- Execution: `superchat-runtime.el`; model adapter: `superchat-llm.el`; tool execution pipeline: `superchat-authority.el` (historical file name); permission levels: `superchat-sandbox.el`; approval: `superchat-approval.el`; selection: `superchat-select-tools.el`.
- Composition: `superchat-plugin.el`, `superchat-service.el`, `superchat-event.el`, `superchat-invariant.el`, `superchat-sink.el`. `superchat-pipeline.el` also contains the product driver; it is not a product-free registry file.
- Persistence/read models: `superchat-tape.el`, `superchat-db.el`, `superchat-projection.el`, `superchat-tape-view.el`. Keep DB storage, Tape facts and derived views distinct; recorded Run outcomes are consumed under the [request lifecycle contract](docs/protocols/request-lifecycle.md).
- Find public commands with `rg -n ';;;###autoload' --glob '*.el'`; inspect Service and Event declarations in source rather than copying an inventory.

## Preserve these contracts

- Runtime alone accepts, completes and cancels Runs. Plugins use the driver Service and the exact accepted Sink; see [request lifecycle](docs/protocols/request-lifecycle.md). Only Org Babel and Magit use `submit-blocking`.
- Every model-invoked tool crosses the one execution pipeline (`superchat-tool-execute`). The Session's permission level decides, the approval seam asks, a `tools/pre-execute` listener can only tighten. Missing trusted wrappers fail closed; a delegated child inherits its parent's sandbox mode and never asks. See [tool admission](docs/adr/0005-fail-closed-ordinary-llm-tool-admission.md) and [refusal delivery](docs/adr/0006-authority-refusal-consumer-delivery.md).
- Preset teardown calls Runtime retirement before plugin disposal. An unresolved external Effect may leave a terminal `audit-incomplete` Run; a genuine late fact cannot restart it. See [composition lifetime](docs/design/plugin-architecture.md#preset-composition-and-lifetime).
- Scope is registration visibility, not Session identity or buffer ownership. Only the Session EditorWorkspace closes its exact owned buffers under the [close protocol](docs/protocols/session-editor-workspace-close.md); Runtime borrows them.
- Tape is append-only. Anchor, View and Projection change interpretation or selection, not past Entries; a representation must retain its source relationship. See [Tape vocabulary](CONTEXT.md#tape-and-context) and the [Context View contract](docs/design/context-continuity-contract-v0.md).

## Develop and verify

- Follow [development verification](docs/agents/development.md): focused behavior, canonical smoke, Kernel check, package load and byte-compilation as appropriate. The deleted test suite is not a requirement to recreate. Report untested boundaries.
- **Do not edit the M3 regression set.** `scripts/m3-regression.el`, `.scratch/m3-verification/regression-mutants/` and `.scratch/m3-verification/REGRESSION-SET.md` are owned by the leader agent the user designated; no other agent or session may modify them, even when fixing a defect a test found. Run the set, and report a test you believe is wrong, or a known-failure marker your fix should flip, to the leader with evidence.
- Keep `lexical-binding: t`; use `superchat--name` for internal helpers and `superchat-name` for public functions; match surrounding code. Optional dependencies and package metadata rules are in the development guide.
- When editing inline widget.el controls, read the `emacs-widget-controls` skill. Plan review/progress belong in their side window, not inline in chat; see [UI conventions](docs/agents/development.md#ui-and-editor-conventions).
- Product `skills/` contains in-Emacs prompt text for `>skill`, not coding-agent skills. Use the [Skills guide](docs/SKILLS_QUICKSTART.md) for the actual discovery layout and format limitations.
- Write local issues under `.scratch/<feature>/` using the [issue tracker conventions](docs/agents/issue-tracker.md) and [triage labels](docs/agents/triage-labels.md). No separate publication, evidence-ledger or plan/capture ceremony is required.
- Keep README.md and README_cn.md aligned when changing user-facing guidance. Documentation changes should distinguish current behavior, accepted decisions, proposals and history; they do not authorize production changes.
