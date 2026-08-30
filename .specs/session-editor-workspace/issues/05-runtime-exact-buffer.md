# 05 为 Runtime 建立 exact-buffer 兼容入口

status: done
blocked_by: 01, 04
writer:
authority: exact-buffer Request identity — `superchat-runtime.el` | owned; EditorWorkspace marker validation — `superchat-editor-workspace.el` | owned
spec_ref: `.specs/session-editor-workspace/SPEC.md` — AC 7；Candidate B；Compatibility and migration
context_ref: `KERNEL.md` — Request Plan、Submit Request、atomic publication；SPEC planned EditorWorkspace authority
evidence_ref: E-20260904-003

## Goal

建立两个不反向依赖的接口：EditorWorkspace 验证 owned marker 与 Session，
Runtime 接受并借用 exact live buffer 进行 context/rendering。Runtime 不理解
EditorWorkspace marker，任何终态都不 kill modern buffer；兼容期保留 legacy
ephemeral 输入，但两种 Request shape 严格互斥。

## Scope

- `superchat-runtime.el`。
- `superchat-editor-workspace.el`：只提供 ownership 查询/验证接口，不提交 Request。
- `test/test-runtime.el`、`test/test-editor-workspace.el`、必要的
  `test/test-llm-backend.el`。
- 不迁移具体 producer，不删除 legacy 字段。

## Acceptance

1. exact `:buffer` 必须 live；Runtime 不从 `:buffer-name` recreate/retarget modern entry。
2. EditorWorkspace 提供一个小而完整的 validation interface：它读取唯一的 owned/
   Session markers，拒绝 malformed ownership 和跨 Session owned buffer，并返回同一
   exact live buffer。Runtime 不读取这些 marker。
3. new exact-buffer Request 不创建 HostLease/Cleanup host，所有终态零 kill effect。
4. legacy ephemeral Request 暂时走原路径并创建自己的 fresh unmarked host；Runtime
   不把 legacy host 转移或标记为 EditorWorkspace-owned。
5. Runtime 在任何 host/Run/provider effect 前拒绝同时携带 exact `:buffer` 与
   `:ephemeral-buffer t` 的 mixed Request。直接 Runtime caller 可以提交普通 borrowed
   buffer；若提交 EditorWorkspace-owned buffer，先通过 EditorWorkspace validation
   interface 是 caller precondition。所有 shipped producer 必经该接口由 Slice06–08
   分别验证，本 Slice 不虚报 producer migration 已完成。
6. Run entry 的 exact buffer 是 borrowed use pointer；buffer name 仅诊断文本。
7. atomic Run publication和普通 caller-owned buffer rollback protection 保持成立。
8. 结构测试证明 Kernel Runtime 不 require/call EditorWorkspace module，也不引用其
   marker symbols；ownership validation 没有第二落点。

## Verification

focused Runtime tests覆盖 success/cancel/rejection/error、dead exact buffer、mixed
Request、same-name replacement 和 rollback；focused EditorWorkspace tests覆盖 owned
marker/Session mismatch、malformed ownership 与 borrowed buffer。随后运行必要的 LLM
backend tests、canonical suite、check-parens、scratch byte compile 和 focused diff
check；只读报告 KERNEL citation drift。

## Evidence

## Planning resolution

- The first `do` attempt correctly stopped before implementation because the
  old Acceptance made Kernel Runtime responsible for a middle-tier ownership
  judgment it is forbidden to perform.
- Confirmed `plan` resolution: keep the SPEC's dependency direction.  Runtime
  validates only its generic exact-buffer Request shape and borrowed identity;
  EditorWorkspace alone validates its ownership markers.  Direct Runtime
  callers have a documented precondition, while Slices06–08 migrate and prove
  the shipped producer paths.
- This is a compatible Slice correction, not a SPEC/KERNEL change.  It adds no
  global hook, validator registry, attestation token, callback field, or second
  ownership ledger.

## Plan-after-check classification correction

- The final sentence above is preserved as the historical pre-implementation
  classification; independent `check` proved that its “not a SPEC/KERNEL
  change” clause was incorrect.
- Post-check `plan` classified the delivered behavior as the compatible Runtime
  Request Action Contract delta already approved by SPEC AC7: `:buffer` is a
  validated input, the Request/Run keeps exact borrowed object identity, the
  object must be live, and it is mutually exclusive with
  `:ephemeral-buffer t`.
- This correction does not move ownership authority.  Runtime still knows only
  generic Request shape and borrowed identity; EditorWorkspace validation is
  the sole owner-marker and Session authority.  Producer migration remains in
  Slices06–08.  The durable correction is K15, sourced by
  `E-20260904-003`.

## Verification summary (do)

- TDD red phase: focused Runtime tests exposed three intended failures before
  implementation: a dead exact buffer was accepted, a mixed exact/ephemeral
  Request reached host acquisition, and Runtime ignored the exact object in
  favor of its name.  EditorWorkspace validation tests initially failed
  because the interface did not exist.
- Runtime focused ERT: 107/107 expected, 0 unexpected.
- EditorWorkspace focused ERT: 27/27 expected, 0 unexpected.
- LLM backend focused ERT: 192 total, 187 expected, 0 unexpected, 5 skipped.
- Canonical suite: 1206 total, 1189 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.
- Runtime validates only generic shape: `:buffer` is an exact live object and
  is mutually exclusive with `:ephemeral-buffer t` before host acquisition,
  Run publication, Tape writes, or provider effects.  The modern entry borrows
  that same object, records its actual name only for diagnostics, creates no
  HostLease, and success/cancel/provider-error terminal paths make zero buffer
  release calls.  Same-name replacement cannot retarget a dead exact object.
- Legacy ephemeral acquisition remains fresh and HostLease-owned.  Legacy
  ordinary name-based entries retain their prior terminal reference cleanup;
  exact-buffer publication and ordinary publication rollback both preserve
  caller-owned buffers while removing only an exact partial Run entry.
- `superchat-editor-workspace-validate-buffer` returns the same live object,
  accepts ordinary borrowed buffers, and rejects malformed owned metadata,
  dead objects, and cross-Session ownership.  It creates no resource and does
  not submit a Request.
- Structural ERT proves `superchat-runtime.el` neither names/requires
  `superchat-editor-workspace` nor references its marker symbols.  No global
  hook, registry, attestation token, Request callback, or second ownership
  ledger was added.  Shipped producer migration remains explicitly deferred
  to Slices06–08.
- `check-parens`: passed for both production files and both focused test files.
- Scratch byte compilation: both production files passed in `/tmp`; only the
  two pre-existing unused lexical-variable warnings in Runtime remained.  No
  repository `.elc` was produced.
- Focused `git diff --check`: passed.  Git was used read-only; no Git/JJ write
  operation was performed.
- Read-only Kernel citation check: 1026 references, 76 line/construct errors,
  0 warnings in the current dirty tree.  Expected drift includes the new
  EditorWorkspace interface and shifted Runtime/tests; re-anchoring belongs to
  `learn`, so `KERNEL.md` was intentionally not modified.
- `evidence_ref` remains empty for `check`/`learn`.

## Verification summary (learn)

- The earlier “`evidence_ref` remains empty” bullet records the handoff state
  before `learn`; this section records the subsequent promotion and backfill
  rather than rewriting that history.
- Evidence `E-20260904-003` was appended before this `evidence_ref` was
  populated and records observation, interpretation, recommended action,
  verification, and references separately.
- K15 revises the existing Submit Request Action Contract while preserving its
  `since: K3`; it explicitly supersedes the K14 seventeen-key wording, records
  all eighteen validation keys, exact borrowed identity, the live/mutual-
  exclusion invariants, the EditorWorkspace authority boundary, and the
  unchanged legacy HostLease path.
- Kernel strict check: 1031 references, 0 errors, 0 warnings.  Authority-map
  checker: `ok`.  Pairs: 81 entries, 494 shared groups, 198 universal
  sentences, 136 narrowed in 37 entries.
- Independent learn reruns: Runtime 107/107; EditorWorkspace 27/27; LLM backend
  192 total, 187 expected, 0 unexpected, 5 skipped; canonical 1206 total, 1189
  expected, 0 unexpected, 17 skipped, `RUNNER_GATE_OK`.
- No production code, tests, SPEC, Protocol, CONTEXT, or other Slice was
  modified by `learn`; no Git/JJ write operation was performed.
