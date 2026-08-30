# 01 建立 Session EditorWorkspace 的最小资源权威

status: done
blocked_by:
writer: Codex team (`kernel_host_review`, `adversarial_arch_review`, root integration)
authority: `superchat-editor-workspace.el` | planned — confirmed SPEC 指定的 Session editor-resource membership 与 close-state 唯一权威；当前 KERNEL 尚未列出，交由 `learn` 补图
spec_ref: `.specs/session-editor-workspace/SPEC.md` — Decision and boundaries；AC 1/2；Core state and ownership tests；Issue map S1
context_ref: `KERNEL.md` — Session、Run、Buffer-backed Session identity、Architecture boundaries；`CONTEXT.md` — Session/Run
evidence_ref: `E-20260903-003`

## Goal

提供一个 headless 可用的中层模块：按稳定 Session ID 惰性得到一个
EditorWorkspace，创建/复用带精确 buffer-local owned marker 的 role buffer，并从
实时 `buffer-list` 枚举成员；没有活跃 Run 时可通过同一 authority 正常关闭。

## Scope

- 新建 `superchat-editor-workspace.el`。
- 新建 `test/test-editor-workspace.el`。
- `test/run-tests.el`：接入新测试。
- 不修改 Runtime、Workflow、Subagent、tab-bar、project、ibuffer 或现有 Session UI。

## Acceptance

1. 相同 Session ID 返回同一 live projection；不同 Session 不共享 owned buffer。
2. lookup 本身不创建 buffer、tab、目录或 project 记录。
3. `(session-id, role)` 创建后复用 exact live buffer；buffer 死亡后重新创建。
4. ownership 仅来自 exact buffer-local Session marker；枚举每次扫描 live
   `buffer-list`，不维护第二份 buffer ledger。
5. 一个 live buffer 最多由一个 Session owned；未标记/borrowed buffer 永不进入
   close set。
6. `open -> closing -> closed` 的无活跃 Run close 是幂等的；close 期间禁止新分配。
7. 普通 kill query 拒绝时进入 `retained`，幸存 buffer 保持 live；retry 重新扫描
   live buffers，成功后进入 `closed`。
8. headless 路径不依赖 tab、selected window 或 project。
9. 新模块是 middle tier；kernel require gate 保持通过。

## Verification

```sh
emacs -Q --batch -L . -l test/test-editor-workspace.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
```

另运行 `check-parens`/byte compilation；若本切片导致 KERNEL 引用漂移，只报告给
`learn`，不修改 KERNEL。

## Verification summary (do)

- Focused ERT: 15/15 passed, 0 unexpected.
- Canonical suite: 1166 tests, 1149 expected, 0 unexpected, 17 skipped.
- Runner architecture gate: `RUNNER_GATE_OK`.
- `check-parens`: passed.
- Isolated byte compilation in `/tmp`: passed; no repository `.elc` retained.
- Adversarial stop-ship review: no remaining High or Medium findings within
  the confirmed Slice 01 trust model.
- Regression coverage includes exact marker-derived membership, borrowed
  survival, `closing`/`retained` allocation freeze for existing and missing
  roles, valid-directory publication, synchronous close/acquisition reentry,
  rollback kill refusal, retained retry, and defensive rescan of a closed
  state with an exact owned survivor.

## Evidence
