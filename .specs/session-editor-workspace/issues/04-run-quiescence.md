# 04 将 Run quiescence 从 editor Cleanup 中独立出来

status: done
blocked_by: 01
writer:
authority: `superchat-runtime.el` | owned — KERNEL 的 Request/Turn/Run、terminal、cancellation 与执行句柄生命周期权威
spec_ref: `.specs/session-editor-workspace/SPEC.md` — Model delta；Candidate A；Runtime and structural tests；Issue map S4
context_ref: `KERNEL.md` — Run、HostLease and Cleanup、Cleanup lifecycle、Submit Request
evidence_ref: E-20260904-002

## Goal

建立一个 private、幂等且不可被 editor ownership 失败跳过的 Run-quiescence seam。
本切片暂不删除 legacy HostLease release，只解除它对 timer/provider/callback/pause
释放的控制权。

## Scope

- `superchat-runtime.el`。
- `test/test-runtime.el`、`test/test-llm-backend.el` 中 focused lifecycle tests。
- 不修改 Request producer、EditorWorkspace ownership、KERNEL 或公开插件文档。

## Acceptance

1. completed/failed/cancelled/timed-out/audit-incomplete 都执行 quiescence exactly once。
2. deadline timer、transport timer owner、provider request、delivery/stream closures 和
   exact Run pause continuation 被 cancel 或 sever。
3. foreign owner、unresolved、retained 或 buffer releaser error 不能跳过 quiescence。
4. retry editor release 不重复 terminal hooks、delivery、provider cancellation 或
   quiescence side effects。
5. borrowed View buffer 保持 live；legacy ephemeral release 行为暂时兼容。
6. whole-Run deadline 与 per-attempt transport timer 保持两个独立保证。

## Verification

运行 focused Runtime/LLM backend ERT、canonical suite、check-parens 和 scratch byte
compile；报告 KERNEL citation drift，不修改 KERNEL。

## Verification summary (do)

- TDD red phase: the two focused regression tests failed as intended before
  implementation (foreign-owner deadline cancellation `0 != 1`; retained
  retry quiescence calls `2 != 1`).
- Adversarial repair TDD red phase: after the deadline `cancel-timer` adapter
  signalled, exact transport-owner cancellation was not attempted (`0 != 1`).
  The focused regression is green after isolating the two physical timer
  effects.
- Runtime focused ERT: 99/99 expected, 0 unexpected.
- LLM backend focused ERT: 192 total, 187 expected, 0 unexpected, 5 skipped.
- Canonical suite: 1193 total, 1176 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.
- Terminal matrix covers `completed`, `failed`, `cancelled`, `timed-out`, and
  `audit-incomplete`: one quiescence seam call and one exact transport-owner
  cancellation per live Run; callback/provider handles are severed and the
  borrowed View remains live.
- Cleanup regressions cover foreign owner/unresolved, retained releaser error
  plus successful retry, synchronous HostLease substitution, and a legacy
  pre-Cleanup ephemeral entry.  Retry does not repeat timer/provider-handle,
  callback, delivery, stream, or pause quiescence.
- Existing real-timer coverage for whole-Run deadline and per-attempt
  wait/hard timers remains green in the focused LLM suite.
- Adversarial finding closed: the deadline handle, transport owner, provider
  request, and callbacks are atomically severed before physical effects; a
  signalling deadline cancellation and a signalling transport-owner
  cancellation are each attempted exactly once, neither blocks exact-Run
  pause clear, and release reentry remains a no-op.
- `check-parens`: passed for all three owned Emacs Lisp files.
- Scratch byte compile: `superchat-runtime.el` passed in `/tmp`; two existing
  unused-lexical-variable warnings remain outside the changed seam.  The test
  files' repository-relative helper form is not batch-byte-compilable from a
  copied path because byte compilation binds `load-file-name` nil; ordinary
  source loading is covered by both focused suites and canonical ERT.
- Focused `git diff --check`: passed.  No repository `.elc` was created.
- Read-only strict Kernel citation check reports 1016 references, 80 line/
  construct errors, and 0 warnings in the current dirty tree.  The report
  includes the expected Slice04 shifts at Runtime release/cleanup forms and
  the inserted Runtime/LLM tests; re-anchoring belongs to `learn`, so KERNEL
  was intentionally left unchanged.
- No semantic conflict found.  KERNEL/CONTEXT/STATUS/EVIDENCE/SPEC/Protocol
  were not modified; `evidence_ref` remains empty for `check`/`learn`.

## Evidence
