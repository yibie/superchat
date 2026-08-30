# 03 用 transient project 与 ibuffer 投影 EditorWorkspace

status: done
blocked_by: 01
writer:
authority: `superchat-editor-workspace.el` | planned — project/ibuffer 只读取该模块的 membership，不建立第二权威
spec_ref: `.specs/session-editor-workspace/SPEC.md` — AC 5/6；Native adapter tests；Issue map S3
context_ref: `KERNEL.md` — Workspace identity 与 Artifact Workspace；`docs/adr/0002-lazy-buffer-derived-workspaces.md`
evidence_ref: `E-20260904-001`

## Goal

在存在 authoritative Workspace root 时提供内存 transient project，并提供按
Session 标记的 ibuffer group；两者只做发现和展示，绝不扩大 owned close set。

## Scope

- `superchat-editor-workspace.el`：内置 project/ibuffer adapters。
- `test/test-editor-workspace.el`。
- 不修改 durable `superchat-workspace.el` 模型，不新增 plugin。

## Acceptance

1. 有 root 时返回 Emacs 支持的 transient project；无 root 时返回无 project，
   不制造临时目录。
2. 不调用 `project-remember-project`，不改 `project-list-file`，不触发交互发现。
3. `project-buffers`、root 下文件和 ibuffer group 都不能写 ownership marker 或
   扩大 close set。
4. ibuffer group 从 live marker 实时计算并显示 retained survivors。
5. 可被多个 Session 借用的普通 file buffer 不被伪装为单 Session owned。

## Verification

运行 focused ERT；用临时 sentinel `project-list-file` 证明内容未改变；随后运行
canonical suite。

## Evidence

## Verification summary (do)

- Added three read-only adapters in `superchat-editor-workspace.el`:
  `superchat-editor-workspace-project`,
  `superchat-editor-workspace-project-buffers`, and
  `superchat-editor-workspace-ibuffer-group`.
- Focused EditorWorkspace ERT: 22/22 passed, 0 unexpected.
- Canonical suite: 1188 tests, 1171 expected, 0 unexpected, 17 skipped.
- Runner architecture gate: `RUNNER_GATE_OK`.
- `check-parens` and focused `git diff --check`: passed.
- Independent adversarial review: no remaining High or Medium finding.
- Regression coverage proves nil root creates no project or directory; empty
  and nonexistent roots are rejected; transient project construction and
  buffer projection call neither interactive discovery nor project
  persistence and leave a sentinel `project-list-file` unchanged; two
  Sessions may project the same borrowed file buffer without either close set
  acquiring it; and a real ibuffer predicate projection remains Session-exact
  and keeps a retained survivor visible until successful retry.
- `do` moved the K12 citations into `superchat-editor-workspace.el` and
  `test/test-editor-workspace.el`; non-strict Kernel checking now reports 15
  construct-anchor errors.  This is a re-anchor-only handoff for the next
  `learn`, not a semantic Kernel conflict.
