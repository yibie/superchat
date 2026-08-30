# 06 迁移 Chat、Workflow 与 Compact 到 Session EditorWorkspace

status: done
blocked_by: 05
writer:
authority: marker/owner 查询与 close — `superchat-editor-workspace.el` | owned；Chat producer identity — `superchat-dispatcher.el` | owned；Extension activation publication — `superchat-extension.el` | owned；Run publication/terminal — `superchat-runtime.el` | owned；Compact 只协调 operation-scoped child closure
spec_ref: `.specs/session-editor-workspace/SPEC.md` Revision 4 — AC 2/2a/3/7；Candidate C；Issue map S5
context_ref: `KERNEL.md` — Buffer-backed Session identity、Workflow Instance、compaction child Session
evidence_ref: E-20260904-004

## Goal

交互 Chat、Workflow 和 Compact 不再依赖 Runtime 的隐式 `get-buffer-create` 或
per-Run ephemeral host；View 死亡不丢 Session identity，child compaction Session 不
借用 parent-owned host。

## Scope

- `superchat.el`、`superchat-dispatcher.el`、`superchat-agent-loop.el`、
  `superchat-editor-workspace.el`。
- `superchat-workflow.el`、`superchat-compact.el`、`superchat-extension.el`、
  `superchat-runtime.el`。
- `test/test-cmd-dispatch.el`、`test/test-workflow.el`、`test/test-compact.el` 及必要的
  `test/test-editor-workspace.el`、`test/test-extension.el`、`test/test-runtime.el`、
  `test/test-llm-backend.el` producer tests。
- 不修改 Runtime legacy removal、Subagent、Skills 或外部插件；Runtime 变更仅覆盖
  synchronous submission nonlocal-exit 的既有 terminal/atomic publication seam。

## Acceptance

1. chat Request 传 exact Session-owned View/host；Runtime 仅借用。
2. Workflow 启动时独立捕获稳定 Session ID；View 死亡后不以 correlation ID 冒充。
3. 同 Session workflow steps 复用 role host，不按 Run 创建 ephemeral buffer。
4. workflow test mock 不再按 buffer name kill 资源。
5. compaction child Session 使用自己的 headless role host，不借 parent-owned buffer。
6. ordinary View rendering、dead-View no-retarget、workflow delivery 和 compaction
   append-only行为保持。
7. Chat submission 在 Extension compose 前冻结并验证 producer 的 exact buffer /
   Session identity；compose 后任何一项被替换都在 Runtime effect 前拒绝，最终 pair
   再经 EditorWorkspace validation。inspectable-plan 与 ordinary dispatch 复用同一
   submission seam。
8. `superchat.el` 不直接读取或解释 EditorWorkspace 私有 ownership marker；同名
   View 复用只通过公共 owner-Session 查询，malformed ownership 被 authority 拒绝。
9. 每个已 materialize 的 operation-scoped compaction child EditorWorkspace 在
   terminal processing 之后或 Runtime submission failure 时通过公共 close authority
   显式关闭；不得直接 kill buffer。重复 compaction 不累积隐藏 live host，关闭失败
   仍保留 `retained` 可发现/重试语义。
10. ownership 与 Session marker 必须同时是 exact buffer-local 且合法才授予 close
    authority；default value、dynamic binding 或 partial metadata 不进入 owner query、
    enumeration、role lookup、ibuffer 或 close set，malformed query 必须拒绝。
11. Extension callback 原地或复制后修改/删除 buffer、Session 任一 identity，都在
    `extension.activated` Tape、Runtime 和 provider effect 前拒绝；合法 composition
    仍恰好记录一次 activation。
12. Runtime provider submission 同步 `error` 或 `quit` 时，任何已发布 Run 都先通过
    既有 terminal/delivery/quiescence seam 密封，再传播原始 condition；Compact 的
    parent in-flight 同时清除，child workspace 关闭或进入可恢复 retained，不遗留
    `running-model` Run。

## Verification

运行 Dispatcher、Workflow、Compact、Agent Loop、EditorWorkspace、Extension、
Runtime focused tests，producer-shape/marker-authority search 与 canonical suite。

## Evidence

## Verification summary (do)

- Compatibility red phase: the first Workflow focused run exposed 13 tests
  that still required correlation-ID Session fallback, per-Run ephemeral
  hosts, or mock name-based cleanup.  Those assertions were replaced with
  stable Session and exact owned-buffer behavior; the mock now captures the
  Request and invokes delivery only.
- Chat Request construction now carries the current exact View object.
  Both final Dispatcher submission seams and Agent Loop validate the exact
  buffer/Session pair through `superchat-editor-workspace-validate-buffer`
  before Runtime submission.  Extension-composed Requests are validated in
  their final shape.  Runtime remains a borrower.
- Workflow captures its stable Session ID at the public invocation boundary
  and threads it through every step.  A live View is validated and borrowed;
  after that exact View dies, later steps use and reuse the Session's
  `workflow-protocol` role host.  Correlation ID remains correlation identity
  only.  The producer emits neither `:ephemeral-buffer` nor
  `:host-authority-mode`, and completion/rejection/cancellation do not kill
  the Session-owned host.
- Compact derives the child compaction Session first, allocates that child's
  `compaction-protocol` role host, validates it, and passes the exact object.
  The parent chat View remains only the delivery/Anchor target; append-only
  compaction behavior is unchanged.
- Focused ERT: Dispatcher 24/24 expected; Workflow 50 total, 49 expected,
  0 unexpected, 1 skipped; Compact 32/32 expected; Agent Loop 27 total,
  26 expected, 0 unexpected, 1 skipped.  Additional Runtime regression:
  107/107 expected.  LLM backend producer coverage: 192 total, 187 expected,
  0 unexpected, 5 skipped.
- Canonical ERT: 1208 total, 1191 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.
- Producer-shape search across `superchat.el`, Dispatcher, Agent Loop,
  Workflow, and Compact found zero `:ephemeral-buffer` or
  `:host-authority-mode` matches.  The four submitting modules each reach
  EditorWorkspace validation before their submit seam.  Dispatcher, Agent
  Loop, Workflow, and Compact contain zero direct ownership-marker symbol
  references.  The Workflow Runtime mock contains zero buffer-name or
  kill-buffer logic.
- `check-parens` passed for all five production files and four focused test
  files.  Scratch byte compilation produced all five `.elc` files under
  `/tmp` only.  It reported existing Superchat/UI free-variable, docstring,
  and optional-function warnings; Agent Loop, Workflow, and Compact added no
  compile warnings.  No repository `.elc` was produced.
- Focused `git diff --check` passed.  JJ was used read-only; no Git/JJ write
  operation was performed.
- Read-only Kernel citation check after the source/test movement reports
  1031 references, 150 errors, and 0 warnings; the authority-map checker is
  `ok`.  Re-anchoring and any durable semantic promotion belong to
  `check`/`learn`, so this Slice leaves `KERNEL.md` and `evidence_ref`
  unchanged.

## Correction verification summary (do, Revision 3)

- Added one shared Dispatcher Chat submission seam used by both inspectable
  plans and ordinary dispatch.  It captures and validates the producer's exact
  buffer/Session pair before Extension composition, rejects either identity
  changing before Runtime is called, validates the unchanged final pair, and
  then submits.  Agent Loop remains its separate compatibility entry and did
  not gain a second Extension identity policy.
- Added the narrow public EditorWorkspace owner-Session query.  It returns the
  Session ID for a live valid owned buffer, nil for an ordinary unowned buffer,
  and signals for dead or malformed ownership.  `superchat.el` now uses only
  this interface for same-name View discovery and contains no private marker
  read.
- Compaction child Sessions remain unique and operation-scoped.  Terminal
  delivery closes the child EditorWorkspace after all terminal processing,
  including an `on-finish` error or quit; synchronous submission failure also
  closes it.  Close errors are warned without replacing the original terminal
  result, duplicate delivery is inert, and refused kills remain visible as
  EditorWorkspace `retained` state for manual retry.  Compact never directly
  kills its host and changes no durable Tape fact.
- Focused ERT: Dispatcher 27/27 expected; Workflow 50 total, 49 expected,
  0 unexpected, 1 skipped; Compact 37/37 expected; Agent Loop 27 total,
  26 expected, 0 unexpected, 1 skipped; EditorWorkspace 29/29 expected.
  Runtime regression 107/107 expected; LLM backend 192 total, 187 expected,
  0 unexpected, 5 skipped.  The independently reported inspectable-plan
  reproduction also passes 1/1.
- Canonical ERT initially exposed one stale native-UI expectation for an
  implicit plan buffer; the shared seam was corrected to materialize the
  producer's current exact View before composition while preserving the
  inspected plan object.  Final canonical ERT: 1218 total, 1201 expected,
  0 unexpected, 17 skipped; `RUNNER_GATE_OK`.
- Producer and authority searches found zero private marker references in
  `superchat.el`, Dispatcher, Agent Loop, Workflow, or Compact; zero
  `:ephemeral-buffer`/`:host-authority-mode` fields in those producers; one
  Extension compose call, inside the shared Dispatcher seam; and zero direct
  `kill-buffer` calls in Compact.  `check-parens` passed for all six production
  files and five focused test files.  Scratch byte compilation emitted all six
  `.elc` files under `/tmp`; only the existing `superchat.el`/Dispatcher
  docstring, free-variable, optional-function, and unused-binding warnings
  appeared.  Focused `git diff --check` passed.
- Read-only Kernel citation check now reports 1031 references, 166 errors, and
  0 warnings; the authority-map checker is `ok`.  The additional drift is
  mechanical source/test movement plus Revision 3 semantics awaiting
  `check`/`learn`; this `do` did not modify `KERNEL.md` or `evidence_ref`.
- Git/JJ write operations: none.

## Atomic correction verification summary (do, Revision 4)

- EditorWorkspace now has one private live-buffer classification seam.  Both
  ownership and Session markers must be exact buffer-local metadata before a
  buffer is classified as owned; inherited/default/dynamic values are ignored,
  partial or malformed local metadata is rejected by the public owner query,
  and enumeration, role lookup, ibuffer projection, validation, and close all
  consume the same classification rather than restating marker conditions.
- Extension composition accepts an optional acceptance function and invokes it
  after all contributions but before `extension.activated` publication.  The
  Dispatcher supplies the frozen producer buffer/Session guard; in-place
  mutation, copied replacement, simultaneous retarget, and removal of either
  identity now fail before Extension Tape, Runtime, or provider effects.
  Accepted enrichment still records one activation and submits once.
- Runtime's existing synchronous submission failure handler now covers both
  `error` and `quit`.  A published Run is terminally delivered and quiesced
  through the existing finish/release path before the original condition is
  re-signalled; delivery-side `quit` is isolated without reopening the Run.
  Compact uses an `unwind-protect` acceptance guard so every nonlocal failed
  submission exit clears the parent View's in-flight state, while child close
  continues through public EditorWorkspace authority and retained semantics.
- Seven focused ERT groups: Dispatcher 30/30; Workflow 50 total, 49 expected,
  0 unexpected, 1 skipped; Compact 39/39; Agent Loop 27 total, 26 expected,
  0 unexpected, 1 skipped; EditorWorkspace 31/31; Extension 17 total,
  16 expected, 0 unexpected, 1 skipped; Runtime 108/108.  LLM backend:
  192 total, 187 expected, 0 unexpected, 5 skipped.
- The real Runtime + Compact provider-quit test proves one failure delivery,
  terminal `failed`, released Cleanup/quiescence, parent in-flight false, and
  closed operation child.  Canonical ERT: 1226 total, 1209 expected,
  0 unexpected, 17 skipped; `RUNNER_GATE_OK`.
- Marker searches find private ownership representation only in
  `superchat-editor-workspace.el` definitions, the classifier, and atomic
  publication.  Producer searches find no direct `kill-buffer`,
  `:ephemeral-buffer`, or `:host-authority-mode` in Chat, Workflow, Agent Loop,
  or Compact.  `check-parens` and focused `git diff --check` pass.  Scratch
  byte compilation emitted all five changed production modules under `/tmp`;
  only pre-existing Dispatcher/Runtime warnings were reported and no repository
  `.elc` was produced.
- Read-only Kernel check reports 1031 references, 170 errors, and 0 warnings;
  the authority-map checker is `ok`.  This is citation drift plus Revision 4
  semantics for `check`/`learn`; `do` did not modify `KERNEL.md` or
  `evidence_ref`.
- Git/JJ write operations: none.

## Terminal hook correction verification summary (do, Revision 4 final blocker)

- Tests-first reproduction added one real Runtime submission in which the
  provider synchronously signals `(quit provider-quit)`, a pre-terminal Turn
  hook signals `(quit turn-hook-quit)`, and a completion hook also signals
  `quit`.  Before the fix the hook condition replaced the provider condition
  and interrupted the terminal transaction; the focused test failed with
  `(quit turn-hook-quit)` instead of `(quit provider-quit)`.
- The existing Turn-hook and completion-hook seams now isolate both `error`
  and `quit` around each individual untrusted hook.  Later hooks still run;
  Runtime's `--finish`, `--complete-turn`, delivery, `--quiesce`, and Cleanup
  seams remain the only lifecycle owners.  Closure shadow hooks were inspected
  and intentionally left unchanged because they run after `--release` and
  cannot leave a Run unsealed or unquiesced.
- The green regression proves the original provider condition is re-signalled
  after one failure delivery; the Run is `failed`, its Turn is completed,
  `:quiesced` is non-nil, Cleanup is `released`, and the registry contains no
  `running-model` or `delivering` Run.
- Focused ERT: Runtime 109/109 expected, 0 unexpected; Compact 39/39 expected,
  0 unexpected.  The real Runtime + Compact provider-quit joint regression
  remains green.
- Canonical ERT: 1227 total, 1210 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.
- `check-parens` passed for both owned Emacs Lisp files.  Scratch byte
  compilation of `superchat-runtime.el` passed under `/tmp` with only the two
  pre-existing unused-lexical-variable warnings.  Focused `git diff --check`
  passed; `test/test-compact.el` required no modification.
- Read-only Kernel citation check against the concurrent dirty tree reports
  1031 references, 268 errors, and 0 warnings; the authority-map checker is
  `ok`.  The drift is not repaired by `do`; KERNEL and all other long-term
  semantic files remain untouched.
- Slice status remains `done`, `evidence_ref` remains empty, and no Git/JJ
  write operation was performed.

## Verification summary (learn)

- The preceding empty-`evidence_ref` statement is the historical `do` handoff.
  Learn appended `E-20260904-004` first and then populated this Slice's
  `evidence_ref` without rewriting that history.
- K16 promotes exact Chat producer identity and pre-activation zero-effect
  Extension rejection, exact local-only ownership classification, Workflow
  and Compact Session-owned host selection, operation-scoped child
  close/retained/retry behavior, and synchronous provider `error`/`quit`
  terminal publication-delivery-quiescence-cleanup with per-hook
  `error`/`quit` isolation.  K10/K11 legacy HostLease/Cleanup remains enacted
  for unmigrated callers.
- Learn reran Runtime 109/109 and Compact 39/39, both with 0 unexpected.  It
  reused the final canonical result already checked against the same product
  and test tree: 1227 total, 1210 expected, 0 unexpected, 17 skipped, and
  `RUNNER_GATE_OK`; `slice06_final_check` was PASS.
- Mechanical citation drift was re-anchored by construct.  Final strict Kernel
  gate: 1035 references, 0 errors, 0 warnings.  Authority-map checker: `ok`.
  Final pairs: 81 entries, 514 shared groups, 201 universal sentences, 137
  narrowed in 36 entries.
- `CONTEXT.md` and `STATUS.md` now reflect the enacted K16 boundary and route
  the active SPEC to `arrange` for Slice 07, the next issue in the current
  order.  No production code, test, SPEC, Protocol, ADR, runbook, lesson, or
  other Slice was modified; no Git/JJ write operation was performed.
