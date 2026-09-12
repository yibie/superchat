---
title: Cordis/dsh semantics research
type: research
status: closed
assignee: fable-subagent
blocked-by: []
---

> Historical specification ticket. “Closed” records the design discussion, not current implementation completion. Use [the remaining plan](../../docs/roadmap.md); do not execute this ticket as a pending task.

## Question
From the vendored Cordis source and dsh packages/core/scope + agent-presets: (1) what happens to dependents when a Service provider is unmounted (do `inject` consumers unmount and remount?); (2) how scoped registries shadow parent entries and reclaim layers; (3) how `Config` schemas are validated at mount and on patch reload. Findings go to `.wayfinder/research/cordis-semantics.md` (no branch: this is a colocated JJ repo).

## Resolution (2026-09-11, research subagent)
Findings: `.wayfinder/research/cordis-semantics.md`.
1. When a provider unmounts, dependents are not destroyed: their effects are unwound and they go PENDING; `apply` re-runs when any provider of that service returns (even a different one). No ordering among sibling dependents; config order is irrelevant.
2. Scoped registries: one global table plus lazily created per-scope tables with a parent chain; lookup is global, then chain farthest to nearest, nearest name wins; a scope layer is deleted the moment its last entry is undone; registrations are owned by the scope so `dispose` unwinds them.
3. Config is validated on every activation; failure marks the plugin FAILED. A live config change is validate-first then full unmount + remount, never an in-place update.
Copy for Elisp: provider-epoch dependency tracking, PENDING state, global+chain overlay lookup, delete-when-empty layers, validate-then-reapply. Skip: microtask deferral, async disposers, schemastery, file watching.
