# Superchat Phase 2 — Execution Path Convergence

## TL;DR

- **What you'll get:** Workflow, Subagent, Rewrite, Magit, and Org Babel will
  submit model work through the same Runtime lifecycle as Chat and Agent.
- **Approach:** Add one blocking wrapper and private delivery/tool-loop request
  options to the existing deep Runtime, then replace each direct transport
  path without moving Surface-specific prompt or rendering logic.
- **Already done:** `superchat-act` already reaches
  `superchat-runtime-submit`; Phase 2 only locks that path with regression and
  static checks.
- **Not included:** Capability/Topology orthogonalization, child-Run APIs,
  Workflow recovery, Workspace 2.0, Projections, Trigger/Cowork integrations,
  or migration of the internal compaction service.
- **Decisions:** Blocking callers wait on the exact Run returned by
  `superchat-runtime-submit`. Subagent keeps its current depth/team/timeout
  control plane but delegates provider execution and terminal lifecycle to
  Runtime. No new dependency, schema, framework, or commit is created.

The blocking wrapper preserves Org Babel's synchronous execution contract,
while Runtime cancellation continues to use the request handle returned by
`llm.el`.

- <https://www.gnu.org/software/emacs/manual/html_node/org/Evaluating-Code-Blocks.html>
- <https://www.gnu.org/software/emacs/manual/html_node/org/Results-of-Evaluation.html>
- <https://github.com/ahyatt/llm/blob/main/llm.el>

## Scope

### In

- `superchat-runtime-submit-blocking`, implemented as submit + wait.
- Request-local delivery and stream callbacks.
- Request-local prepared tools, tool wrapper, round-limit callback, retry
  limit, timeout, and conversation-recording flag needed by existing paths.
- Workflow step submission through Runtime.
- Subagent sync/async provider execution through Runtime.
- Rewrite, Magit, and Org Babel through the blocking Runtime adapter.
- Removal of duplicate direct transport/lifecycle ownership.
- Regression, focused ERT, canonical ERT, byte compilation, and static
  architecture checks.

### Out

- Redesigning Workflow into a resumable program.
- Introducing `superchat-runtime-submit-child`.
- Changing Subagent task/team semantics.
- Making Rewrite/Magit/Org streaming.
- Moving prompt construction out of existing Surface adapters.
- Migrating `superchat-compact.el`; compaction remains an internal projection
  service until the Projection phase.
- New public Surface registry or Configuration commands.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Runtime compatibility seam | None | 2–4 | All remaining callers need one lifecycle |
| 2. Workflow migration | 1 | 5 | Removes one direct async path |
| 3. Blocking Surface migration | 1 | 5 | Removes three direct sync paths |
| 4. Subagent migration | 1 | 5 | Removes the duplicated provider/tool loop |
| 5. Boundary cleanup and verification | 2–4 | None | Proves no product entry bypass remains |

## Parallel Execution Waves

```text
Wave 1:
└── Task 1: Runtime compatibility seam

Wave 2:
├── Task 2: Workflow
├── Task 3: Rewrite, Magit, Org Babel
└── Task 4: Subagent

Wave 3:
└── Task 5: cleanup, regression, full verification
```

## Tasks

### Task 1: Deepen Runtime without adding another execution service

- **What:** Add a thin blocking wrapper around `superchat-runtime-submit`.
  Store callbacks and existing transport controls only in private Run entries;
  never expose them from status. Generate an ephemeral Session ID when a
  stateless Surface omits one. Let callers suppress only the legacy
  conversation-recording hooks, not Runtime events or arbitrary completion
  hooks.
- **Files:** `superchat-runtime.el`, `superchat.el`, `test/test-runtime.el`.
- **Acceptance:** Blocking execution returns the completed response from the
  same Run, cancels on timeout, signals non-success terminal states, supports
  custom delivery/stream callbacks, and records the full Runtime event chain.
- **QA:** Focused ERT for success, timeout/cancel, custom delivery, generated
  Session ID, and no duplicate conversation history.

### Task 2: Route Workflow steps through Runtime

- **What:** Keep current sequential step orchestration and rendering. Each step
  constructs a Request, supplies the existing stream callback, and starts the
  next step from Runtime delivery. Reuse one correlation ID across the
  workflow.
- **Files:** `superchat-workflow.el`, `test/test-workflow.el`.
- **Acceptance:** Workflow contains no direct model call; success/failure is
  decided from Runtime status; streaming and step chaining remain unchanged.
- **QA:** Existing Workflow ERT rewritten at the Runtime boundary.

### Task 3: Route blocking Surfaces through Runtime

- **What:** Replace direct `llm-chat` calls in Rewrite, Magit, and Org Babel
  with `superchat-runtime-submit-blocking`. Preserve each caller's current
  prompt, model selection, error presentation, and synchronous return shape.
- **Files:** `superchat-rewrite.el`, `superchat-magit.el`, `ob-superchat.el`,
  related ecosystem tests.
- **Acceptance:** No direct model API remains in those files; Rewrite still
  replaces the selected region, Magit still returns commit text, and Org Babel
  still returns a synchronous result.
- **QA:** Focused Surface tests mock only the blocking Runtime interface.

### Task 4: Remove the Subagent provider loop

- **What:** Keep Subagent depth, team, aggregation, timer, and prepared tool
  wrappers. Replace its sync generator and copied `llm-chat-async` retry/tool
  loop with Runtime Requests. Runtime receives the already wrapped tools and
  existing round-limit/retry behavior; Subagent cancellation targets the Run
  rather than a provider request.
- **Files:** `superchat-runtime.el`, `superchat.el`,
  `superchat-subagent.el`, `test/test-subagent.el`,
  `test/test-llm-backend.el`.
- **Acceptance:** Subagent contains no direct provider/model call, child
  Authority remains tighten-only through existing wrappers, cancellation
  ignores late results, and team/depth behavior is unchanged.
- **QA:** Existing Subagent suite plus focused sync, async, timeout, cancel,
  prepared-tool, and round-limit tests.

### Task 5: Enforce and verify the architecture boundary

- **What:** Delete obsolete declarations/helpers and update only tests broken
  by the moved seam. Run focused and canonical ERT, byte-compile touched
  modules, inspect the diff, and grep product entry files for direct model
  calls.
- **Files:** Only evidence-driven fixes.
- **Acceptance:** Product entry files contain no `llm-chat`,
  `llm-chat-async`, `superchat--llm-generate-answer`, or synchronous generator
  calls. Allowed calls remain inside Runtime/transport and the explicitly
  deferred compaction service.
- **QA:** Targeted suites,
  `emacs -Q --batch -L . -l test/run-tests.el`, byte compilation, and
  repository searches.

## Success Criteria

- [x] Blocking callers wait on the same accepted Run, not a second model path.
- [x] Workflow steps submit through Runtime and preserve streaming/chaining.
- [x] Subagent sync/async execution submits through Runtime.
- [x] Rewrite, Magit, and Org Babel submit through Runtime.
- [x] `superchat-act` remains Runtime-owned.
- [x] Runtime IDs and Tape causation exist for every migrated path.
- [x] Existing Authority/Approval/Tool Executor remains the only tool seam.
- [x] Canonical ERT reports zero unexpected failures.
- [x] Touched modules byte-compile without new warnings.
- [x] Static checks find no product-entry transport bypass.
