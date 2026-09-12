---
title: Invariant list and seams
type: grilling
status: closed
assignee: sc-leader (Claude Fable 5.1, session 8cfe6859)
blocked-by: [001-service-catalogue]
---
## Question
Which Invariants does the Kernel enforce, and at which Service seam is each checked so it holds even when the driver is replaced? Start from: buffer unchanged until Change Set accepted; terminal Run state written to Tape; tool execution passes Authority; model-visible text is reconstructable from Tape.

## Resolution (2026-09-11, sc-leader)

**Plain language.** An Invariant is a rule that must hold no matter which
plugins are mounted, so it cannot live inside any one plugin. Each is checked
at a seam the Kernel owns (the driver, or a Service method the Kernel wraps),
and a violation is *recorded on the Tape* as `invariant.violated` — never
silently repaired, never hidden. Six Invariants; the registry is
`superchat-invariant-define NAME :seam SEAM :check FN :doc STRING`, and
`superchat-invariant-report` lists them (the honest replacement for today's
`superchat-stage-report`, which advertises seams that do not exist).

| # | Invariant | Seam (where the Kernel checks it) | Check |
|---:|---|---|---|
| 1 | **Source buffer unchanged until its Change Set is accepted.** | `driver.submit` for `inline-*` surfaces records `(buffer-hash HOST)` in the Run Scope; the driver re-checks at the terminal Sink emission and again when the Run Scope is disposed. | hash equal, or a `change-set.accepted` Fact for this Run exists on the Tape. Today's writers: `superchat-change-set-apply` (`superchat-rewrite.el:383,466,680`) — the only sanctioned path; the raw `insert`s at `:436,572,590` write into *preview/diff* buffers, not the host. |
| 2 | **Every terminal Run state is on the Tape.** | the driver's seal, immediately after `save-state` contributors return. | exactly one of `run.completed` / `run.failed` / `run.cancelled` (today `superchat-runtime.el:1315-1317`) appended for this run-id before any terminal Sink emission; if `tape.append-event` itself fails the driver emits `error (:status audit-incomplete)` and marks the Run unaudited (kept from today's `unaudited` path). |
| 3 | **Every tool execution passes Authority.** | `tools.execute` (the only path to a tool body; the `materialize` method hands the model *wrappers* that call `execute`). | `authority.decide` was called for this (run, tool, args) and returned `allow` before the body ran; `approval.decided` is on the Tape; a wrapper that reaches a body without a decision is a violation. Today the seam is `superchat-tool-execute` (`superchat-authority.el:1538`, callers `agent-loop:603,657`, `subagent:965,980`, MCP). |
| 4 | **Model-visible text is reconstructable from the Tape.** | the driver, between `assemble-prompt` and `model-transport.generate`. | the assembler's `:receipt` (fragment ids, owners, budget class, chars kept/dropped) is appended as `prompt.assembled` for this run-id; `intent.received` (`superchat-runtime.el:1931`) carries the raw `:content`. Transport receives only what the receipt describes. |
| 5 | **A Run delivers to exactly its Envelope's Sink.** | driver emission (`superchat-runtime.el:1108-1117` today). | every emission for run-id carries the Sink id captured at submit; `delivery.failed` is recorded when the Sink signals. |
| 6 | **A plugin contributes only through its Scope** (ticket 006: not enforceable in Elisp). | mount, by review; optionally `superchat-plugin-audit` diffs `obarray` and hook variables around `apply`. | advisory: a difference is reported as `plugin.leaked` with the symbols, the plugin still mounts. |

**Rules.** Invariants are Kernel data, not plugin registrations: a plugin
cannot add or remove one. Checks run synchronously at the seam and cost one
Tape read at most; a check that *itself* errors is recorded as
`invariant.check-failed` and does not abort the Run. Replacing the `driver`
Service does not exempt anyone: the Kernel wraps every `driver` provider's
`submit` with the same seam checks (that is what "checked at a seam the
Kernel owns" means when the driver is a plugin).

**Dropped candidates:** "streaming chunks arrive in order" (a transport
property, tested by the mock), "Preset switch leaves no registrations" (that is
Scope disposal, covered by 006's checks, not a per-Run rule).
