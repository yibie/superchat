# Superchat Phase 0 — Event and Authority Work Plan

## TL;DR

- **What you'll get:** Every model-invoked tool path will enter one audited
  `Validate → Effects → Authority → Approval → Execute → Tape` seam, while
  preserving the current `llm-tool`, Preset, Tape schema, and user commands.
- **Approach:** Add two deep modules (`superchat-tape.el` and
  `superchat-authority.el`), prove their interfaces with ERT, then replace the
  duplicated main-agent and subagent wrappers with adapters over that seam.
- **What it will not do:** No Runtime submit interface, Session projection,
  Workspace 2.0, Topology overlay, external integrations, schema migration, or
  OS sandbox claims.
- **Decisions:** Existing `tape.kind` remains unchanged; event envelopes live
  in `meta`. Existing permission/rewrite/result hooks remain compatibility
  inputs. Unknown effects ask in attended runs and deny in unattended runs.
  The current dirty worktree is preserved and no commit is created.

## Scope

### In

- `superchat-tape-append-event` with validation, legacy-kind mapping,
  redaction, inline-size limiting, stable timestamps, and propagated failures.
- Tool effect descriptors/resolvers for current built-ins and conservative
  fallbacks for MCP and synthesized tools.
- Authority composition with `deny | ask | allow | defer`.
- Approval bound to normalized arguments, resolved effects, policy version,
  and a stable SHA-256 request hash.
- One sync/async Tool Executor used by normal chat, agent, subagent, workflow,
  MCP, and synthesized tools.
- Pre-execution Tape durability for model-invoked side effects.
- Regression and failure-path ERT coverage.

### Out

- `superchat-runtime-submit`, Turn/Run state machines, cancellation handles.
- Projection registry, Workspace artifacts, Trigger/Workflow persistence.
- Persistent approval grants or unattended external-write policies.
- SQLite column promotion or a second event store.
- Removing legacy public hooks or tool-specific confirmations where they are
  still used outside the model tool path.

## Task Dependency Graph

| Task | Depends on | Blocks | Reason |
|---|---|---|---|
| 1. Tape event module | None | 3, 4 | Executor audit depends on a durable event seam |
| 2. Authority/effect module | None | 3, 4 | Executor decisions depend on normalized effects |
| 3. Unified executor | 1, 2 | 4, 5 | Callers need one proven execution interface |
| 4. Main/normal tool migration | 3 | 5, 6 | Establish the canonical request path first |
| 5. Subagent/dynamic migration | 3, 4 | 6 | Remove the remaining duplicate wrappers |
| 6. Verification and review | 1–5 | None | Prove behavior and catch bypasses |

## Parallel Execution Waves

```text
Wave 1:
├── Task 1: Tape event module
└── Task 2: Authority/effect module

Wave 2:
└── Task 3: Unified executor

Wave 3:
├── Task 4: Main and normal-chat migration
└── Task 5: Subagent and dynamic-tool migration

Wave 4:
└── Task 6: Full verification and review
```

## Tasks

### Task 1: Introduce the Tape event seam

- **What:** Add `superchat-tape.el`; validate event plists; assign schema and
  `recorded-at`; map event types to legacy kinds; redact sensitive keys; replace
  oversized payloads with hash/size metadata; call
  `superchat-db-tape-append` without swallowing errors. Migrate the existing
  main-agent tool log helpers so the module has a real caller immediately.
- **Files:** `superchat-tape.el`, `superchat-agent-loop.el`,
  `test/test-tape-events.el`, `test/run-tests.el`, `superchat.el`.
- **Agent:** `executor` — isolated module plus tests.
- **Acceptance:** Event rows retain old kinds; full envelope is in `meta`;
  secrets and oversized bodies are absent; DB failure is observable and stops
  the pre-execution path.
- **QA:** Targeted ERT for mapping/redaction/size/failure, then canonical suite.
- **Suggested commit intent:** `Make tool audit durable before execution`

### Task 2: Define effects and Authority decisions

- **What:** Add `superchat-authority.el`; normalize tool arguments; resolve
  `Effect × Resource × Scope`; adapt current permission hooks and destructive
  guardrails; compose `deny > ask > allow > defer`; define explicit
  `attended | unattended`; compute the canonical approval request hash.
- **Files:** `superchat-authority.el`, `superchat-tools.el`,
  `test/test-authority.el`, `test/run-tests.el`.
- **Agent:** `executor` — pure decision logic and focused tests.
- **Acceptance:** Representative read/write/process tools resolve correct
  scopes; unknown attended asks; unknown unattended denies; deny wins; child
  guardrails cannot broaden parent policy; hashes change when normalized args,
  effects, or policy version change.
- **QA:** Targeted ERT with no model or real filesystem mutation.
- **Suggested commit intent:** `Make tool authority depend on effects, not names`

### Task 3: Implement one Tool Executor

- **What:** Implement `superchat-tool-execute` and the llm-tool wrapper adapter.
  Enforce rewrite/normalize, validation, effect resolution, Authority,
  approval, pre-execution events, sync/async invocation, result filtering,
  completion/failure events, and exact normalized-argument execution.
- **Files:** `superchat-authority.el`, `superchat-tape.el`,
  `test/test-authority.el`.
- **Agent:** `executor` — security-sensitive shared seam.
- **Acceptance:** Denied/unapproved calls never invoke the function; allow does
  not double-confirm; Tape prewrite failure prevents invocation; sync and async
  yield equivalent decisions/events; changed arguments invalidate approval.
- **QA:** Fake sync/async tools and an injected failing Tape append.
- **Suggested commit intent:** `Prevent tool paths from bypassing one execution seam`

### Task 4: Route main and normal-chat tools through the executor

- **What:** Replace agent-only security wrapping with unconditional execution
  wrapping at request-time collection. Keep agent rendering, counters, current
  hooks, timeout behavior, and synthesized-tool confirmation compatibility.
  Tool bodies must not repeat approval after the executor approved the exact
  request.
- **Files:** `superchat.el`, `superchat-agent-loop.el`,
  `superchat-tools.el`, `test/test-agent-loop.el`,
  `test/test-llm-backend.el`.
- **Agent:** `executor` — owns the primary user path.
- **Acceptance:** Normal chat and agent mode both use the executor; current
  agent tests remain green; mutating buffer/workspace tools no longer bypass
  Authority; no duplicate prompt after executor approval.
- **QA:** Targeted agent/LLM tests plus grep proving the main collection path
  applies the wrapper regardless of `agent-mode`.
- **Suggested commit intent:** `Apply the same tool safety to every chat mode`

### Task 5: Route subagents and dynamic tools through the executor

- **What:** Replace the subagent permission/confirmation/Tape implementation
  with the same executor; preserve counters, team dynamic bindings, callbacks,
  isolation, and retries. Ensure MCP and synthesized tools receive explicit or
  conservative descriptors.
- **Files:** `superchat-subagent.el`, `superchat-mcp.el`,
  `superchat-synthesis.el`, `test/test-subagent.el`,
  `test/test-synthesis.el`.
- **Agent:** `executor` — owns secondary execution paths.
- **Acceptance:** Subagents cannot widen guardrails; MCP unknown effects ask or
  deny according to attendance; synthesized tools remain confirmation-safe;
  no duplicate permission/confirmation implementation remains in subagent code.
- **QA:** Targeted subagent/synthesis tests plus sync/async failure tests.
- **Suggested commit intent:** `Keep delegated and dynamic tools inside the same trust boundary`

### Task 6: Verify the Phase 0 invariant

- **What:** Run targeted and full ERT, inspect the diff, search for remaining
  model-invoked bypasses, and repair regressions. Keep legacy direct Tape
  writes that are not tool execution; they are outside this phase.
- **Files:** No planned production additions; only necessary fixes.
- **Agent:** `verifier` — independent evidence pass.
- **Acceptance:** Canonical suite has zero unexpected failures; targeted safety
  tests pass; tool paths from chat/agent/subagent/workflow/MCP/synthesis all
  cross `superchat-tool-execute`; no secrets are written by new event paths.
- **QA:**
  `emacs -Q --batch -l test/run-tests.el` with data/skills paths sandboxed;
  repository grep for direct wrapper execution and direct tool Tape logging.
- **Suggested commit intent:** `Prove the Phase 0 execution invariant`

## Verification Strategy

- Unit: focused ERT for Tape, Authority, executor sync/async, and failure paths.
- Integration: existing agent, subagent, workflow, synthesis, MCP collection,
  and Tape view suites.
- Architecture checks:
  - model-invoked tools are wrapped once at the shared collection seam;
  - subagent code contains no independent permission/approval implementation;
  - no new event store or schema fork exists;
  - old Tape rows remain readable.
- Full suite:

  ```sh
  emacs -Q --batch \
    --eval '(setq superchat-data-directory "/private/tmp/superchat-codex-test-skills/" superchat-skills-directory "/private/tmp/superchat-codex-test-skills/")' \
    -l test/run-tests.el
  ```

## Commit Strategy

No commits will be created in this run because the worktree already contains
large user-owned changes. If committed later, use one Lore commit per task,
including `Confidence`, `Scope-risk`, `Directive`, `Tested`, and `Not-tested`
trailers.

## Success Criteria

- [ ] `superchat-tape-append-event` is a real, tested caller-facing seam.
- [ ] `superchat-tool-execute` is the only model-invoked execution seam.
- [ ] All effects are explicit or conservatively unknown.
- [ ] Unknown unattended effects deny.
- [ ] Pre-execution audit failure prevents side effects.
- [ ] Main and subagent sync/async behavior remains compatible.
- [ ] Existing Presets, hooks, commands, and Tape rows remain readable.
- [ ] Canonical ERT suite reports zero unexpected failures.
