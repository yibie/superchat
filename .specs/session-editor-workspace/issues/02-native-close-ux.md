# 02 用 tab 与手动命令关闭 Session EditorWorkspace

status: done
blocked_by: 01
writer:
authority: `superchat-editor-workspace.el` | planned — 唯一拥有 close orchestration；`superchat-runtime.el` 继续独占单个 Run 的 cancellation 语义
spec_ref: `.specs/session-editor-workspace/SPEC.md` — AC 3/4；Close behavior tests；Issue map S2
context_ref: `KERNEL.md` — Runtime status projection、Run cancellation、Session identity
evidence_ref: `E-20260903-003`

## Goal

把原生 tab close 和 `M-x superchat-close-session-workspace` 接到同一个关闭动作：
有活跃 Run 时只确认一次；确认则取消目标 Session 的全部活跃 Run 后关闭，拒绝则
零修改并 veto tab close；失败资源进入可见的 `retained`。

## Scope

- `superchat-editor-workspace.el`：close orchestration、tab parameter/hooks、消息。
- `superchat.el`：Session tab/open 集成与命令可达性。
- `test/test-editor-workspace.el`、必要时 `test/test-native-ui.el`。
- 不修改 Runtime cancellation 实现、project/ibuffer 或 legacy HostLease。

## Acceptance

1. tab 参数保存稳定 Session ID，不从 tab 名、窗口或当前 buffer 猜测。
2. active Runs 只从 Runtime public status projection 按 exact Session ID 选择。
3. 确认后先冻结分配，取消全部且仅目标 Session active Runs，再关 owned buffers，
   最后关 tab。
4. 拒绝后 Run/buffer/tab 均不变，native close 被 veto，消息明确给出手动命令。
5. 手动命令与 tab hook 调用同一 close authority，并在 active Runs 仍在时重问。
6. kill 失败保留 tab 和 survivor，状态为 `retained`；无 force-kill bypass。
7. headless manual/programmatic close 不依赖 tab。

## Verification

运行 EditorWorkspace/native UI focused ERT 与 canonical suite；用真实 batch tab
证明 Emacs 原生关 tab 本身不会被误当作 kill-buffer ownership。

## Resolved decision (plan)

The implementation and focused tests exposed an unplanned atomicity choice in
Emacs's shared `tab-bar-tab-prevent-close-functions` chain.  Superchat must
cancel Runs and close owned buffers before allowing its Session tab to close,
because a normal buffer kill may be retained.  But a later third-party prevent
hook can still veto after those irreversible effects, leaving a tagged tab for
an already closed Workspace.  Waiting until native deletion succeeds reverses
the inconsistency: a later retained buffer would have no Session tab to keep.

The user confirmed the exclusive Session-tab close policy: existing prevent
hooks ordered before Superchat may veto with zero Superchat effects; once the
event reaches Superchat, EditorWorkspace consumes the close transaction and
later prevent hooks do not run.  The shared programmatic authority performs
tab closure itself rather than relying on a caller-bound private switch.

A second stop-ship finding was local and is now closed: direct calls to
`superchat-session-workspace-close` perform exact tagged-tab closure without a
caller-bound private protocol.

Kernel's Architecture boundaries also have no Session EditorWorkspace row yet;
this is a `semantic` finding for `learn` and the controlled Kernel update, not
a reason to move the authority out of `superchat-editor-workspace.el`.

## Verification summary (do)

- EditorWorkspace focused ERT: 18/18 passed, 0 unexpected.
- Native UI focused ERT: 51/51 passed, 0 unexpected.
- Combined focused coverage: 69/69 passed, 0 unexpected.
- Canonical suite: 1184 tests, 1167 expected, 0 unexpected, 17 skipped.
- Runner architecture gate: `RUNNER_GATE_OK`.
- `check-parens`, focused `git diff --check`, and isolated byte compilation:
  passed; no repository `.elc` retained.
- Adversarial stop-ship review: no remaining High or Medium findings.
- Regression coverage includes exact active-Run filtering, one confirmation,
  freeze-before-cancel, cancellation failure/retry, normal kill retention,
  direct/manual/native shared authority, pre- and post-Superchat prevent-hook
  ordering, pre-close `error`/`quit`, sole-tab neutralization, exact tab
  parameters, and cross-frame lookup/closure.

## Evidence
