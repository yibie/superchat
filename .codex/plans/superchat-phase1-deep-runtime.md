# Superchat Phase 1 — Deep Runtime Work Plan

## TL;DR

- **What you'll get:** Main Chat and Agent submissions will enter one
  `superchat-runtime-submit` lifecycle with stable Turn/Run IDs, Runtime events,
  queryable status, best-effort cancellation, and completion hooks that run
  only after the model response ends.
- **Approach:** Keep the current prompt builder and `llm.el` transport, place a
  deep Runtime module between the dispatcher and transport, then make the
  Request Inspector hold only immutable request data—not a mutable
  `superchat-turn`.
- **What it will not do:** No Workflow/Subagent/Rewrite/Magit/Org migration,
  Projection framework, Workspace 2.0, Capability/Topology commands, new
  SQLite columns, or transport rewrite.
- **Decisions:** Phase 1 accepts an already-prepared Request so Surface context
  collection can move independently in Phase 2. Runtime alone creates the
  execution Turn. Cancellation uses the request object returned by
  `llm-chat-streaming`; every tool-round resend replaces the stored handle.
  Terminal Runs remain in a process-local registry so status stays queryable.
  No new dependency or commit is created.

The cancellation contract is grounded in the official `llm.el` interface:
both async and streaming calls return an object accepted by
`llm-cancel-request`.

- <https://github.com/ahyatt/llm/blob/main/llm.el>
- <https://github.com/ahyatt/llm>

## Scope

### In

- `superchat-runtime-submit`, `superchat-runtime-status`, and
  `superchat-runtime-cancel`.
- Runtime-owned execution Turn and root Run registry.
- Stable `session-id`, `turn-id`, `run-id`, `actor-id`, `correlation-id`, and
  causation links.
- `intent.received → run.started → response.completed → run.completed` events,
  plus failed, timed-out, cancelled, and audit-incomplete terminal states.
- Runtime context propagated to the existing Tool Executor.
- Main plain Chat and Agent dispatch through Runtime.
- Request Inspector without a mutable Turn or manual finalization.
- True post-turn and completion-hook timing.
- Latest `llm.el` request handle tracking across tool rounds.

### Out

- Child Run creation and Subagent control-plane consolidation.
- Workflow step state, Trigger idempotency, or unattended execution.
- Runtime preparation of raw Surface context.
- Persistent Session/Run projection or cross-restart status.
- Provider-specific hard cancellation guarantees beyond
  `llm-cancel-request`.
- Changing the synchronous workflow/rewrite transport.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Runtime interface and events | None | 2, 3, 4 | Defines the lifecycle seam |
| 2. Transport observation | 1 | 3, 4 | Runtime needs terminal outcomes and latest request handle |
| 3. Main Chat/Agent migration | 1, 2 | 4, 5 | Replaces the direct model path |
| 4. Inspector and completion semantics | 1, 2, 3 | 5 | Removes mutable Turn and pre-response finalization |
| 5. Verification and review | 1–4 | None | Proves lifecycle, compatibility, and boundaries |

## Parallel Execution Waves

```text
Wave 1:
└── Task 1: Runtime interface and focused tests

Wave 2:
└── Task 2: Transport observation and cancellation handle

Wave 3:
├── Task 3: Main Chat/Agent migration
└── Task 4: Inspector/completion migration

Wave 4:
└── Task 5: Full verification and review
```

## Tasks

### Task 1: Add the deep Runtime module

- **What:** Add `superchat-runtime.el` with a process-local Run registry, ID
  generation, sanitized status views, terminal-state guards, Runtime event
  append helpers, and the three public functions. Runtime reconstructs its
  internal Turn from a prepared Request and never returns it.
- **Files:** `superchat-runtime.el`, `superchat.el`,
  `test/test-runtime.el`, `test/run-tests.el`.
- **Agent:** `executor` — implementation and ERT at the new interface.
- **Skills:** `codebase-design` — keep the external seam to submit/status/cancel.
- **Depends:** None.
- **Blocks:** Tasks 2–4.
- **Acceptance:** Submit returns the documented handle; status exposes no
  internal Turn or provider request; duplicate terminal callbacks have no
  effect; pre-model Tape failure prevents transport execution.
- **QA:** Focused ERT with fake Tape and transport, including a failing append.
- **Suggested commit intent:** `Give each model run one observable lifecycle`

### Task 2: Expose transport lifecycle without rewriting it

- **What:** Extend `superchat--llm-generate-answer` with one optional observer.
  Notify it when a streaming request object changes and once on terminal
  completion/failure/timeout. Route every initial/retry request through one
  local send helper so handle tracking cannot diverge.
- **Files:** `superchat.el`, `test/test-llm-backend.el`,
  `test/test-runtime.el`.
- **Agent:** `executor` — small transport edit with callback-sensitive tests.
- **Skills:** None.
- **Depends:** Task 1.
- **Blocks:** Tasks 3–4.
- **Acceptance:** Initial and tool-round request handles are observed in order;
  final/error/timeout is observed once; old callers and streaming behavior are
  unchanged.
- **QA:** Fake `llm-chat-streaming` returns distinct handles across two rounds;
  failure test proves one terminal notification.
- **Suggested commit intent:** `Let Runtime observe transport without owning provider code`

### Task 3: Route main Chat and Agent through Runtime

- **What:** Replace direct `superchat--llm-generate-answer` dispatch for
  `:llm-query` with `superchat-runtime-submit`. Runtime binds the Phase 0 Tool
  Executor context, renders streams/results in the originating View, and
  preserves Agent running-state cleanup. Keep `superchat--agent-run` as a thin
  compatibility adapter.
- **Files:** `superchat-dispatcher.el`, `superchat-runtime.el`,
  `superchat-agent-loop.el`, `test/test-llm-backend.el`,
  `test/test-agent-loop.el`, `test/test-runtime.el`.
- **Agent:** `executor` — owns the primary execution path.
- **Skills:** `codebase-design` — replace the shallow alternate Agent path.
- **Depends:** Tasks 1–2.
- **Blocks:** Tasks 4–5.
- **Acceptance:** Plain and Agent requests both produce one Run; Tool events
  carry the Run/Turn/correlation IDs; callbacks render in the originating View;
  no main product entry calls the transport directly.
- **QA:** Runtime integration ERT plus existing Chat/Agent/tool-round tests.
- **Suggested commit intent:** `Make Runtime the owner of main model execution`

### Task 4: Make completion and the Request Inspector truthful

- **What:** Remove `:turn` and `:finalized` from inspectable Request values.
  `superchat-execute-request-plan` submits the exact edited Request to Runtime.
  Record `intent.received` before execution; run real post-turn hooks only
  after terminal response; reuse those event IDs when updating in-memory
  history so legacy Tape rows are not duplicated.
- **Files:** `superchat-core.el`, `superchat-dispatcher.el`,
  `superchat-runtime.el`, `superchat-render.el`, `superchat.el`,
  `test/test-native-ui.el`, `test/test-runtime.el`.
- **Agent:** `executor` — lifecycle migration with compatibility tests.
- **Skills:** None.
- **Depends:** Tasks 1–3.
- **Blocks:** Task 5.
- **Acceptance:** Inspector values contain no Turn; building/inspecting does not
  run completion hooks; submitting runs them once after response; user and
  assistant each have one legacy-compatible Tape row.
- **QA:** Delayed callback ERT checks ordering before/after response; duplicate
  callback and cancellation paths remain idempotent.
- **Suggested commit intent:** `Make post-turn mean after the response`

### Task 5: Verify the Phase 1 boundary

- **What:** Run targeted and canonical ERT, byte-compile touched modules, inspect
  the diff, and grep for direct main-path transport calls or mutable Turn in
  inspector values. Repair regressions without extending Phase 2 scope.
- **Files:** Only fixes required by evidence.
- **Agent:** `verifier` — independent completion evidence.
- **Skills:** None.
- **Depends:** Tasks 1–4.
- **Blocks:** None.
- **Acceptance:** Canonical suite has zero unexpected failures; touched files
  byte-compile without new warnings; status/cancel/events/completion ordering
  pass; Workflow/Subagent/Rewrite/Magit/Org remain intentionally unmigrated.
- **QA:** `emacs -Q -l test/run-tests.el`; targeted Runtime ERT; repository
  searches for direct main-path transport and inspector `:turn`.
- **Suggested commit intent:** `Prove the main Runtime lifecycle before expanding it`

## Verification Strategy

- Unit: Runtime submit/status/cancel, event ordering, terminal idempotency,
  prewrite failure, request-handle replacement.
- Integration: plain Chat, Agent preset, streamed response, tool round, request
  inspector, origin View restoration.
- Compatibility: legacy Tape kinds and existing in-memory history ordering.
- Static checks:
  - Request Inspector data contains no `superchat-turn`;
  - main `:llm-query` dispatch enters Runtime;
  - only transport/internal adapters call `llm-chat-streaming`;
  - Tool Executor receives Runtime correlation fields.
- Build: byte-compile touched `.el` files with edited source preferred.

## Success Criteria

- [x] Each main submit has stable Turn/Run IDs and one terminal status.
- [x] `superchat-runtime-cancel` cancels the latest provider request and ignores
      later callbacks.
- [x] Runtime events form the expected causation chain.
- [x] Post-turn hooks run once and only after terminal response.
- [x] Inspector execution uses the edited Request without exposing a Turn.
- [x] Plain Chat and Agent retain streaming/tool behavior.
- [x] Canonical ERT passes with zero unexpected failures.
- [x] No Phase 2+ framework or dependency is added.
