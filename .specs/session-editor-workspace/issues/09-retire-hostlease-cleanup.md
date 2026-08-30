# 09 删除 legacy HostLease 与 editor Cleanup 契约

status: done
blocked_by: 02, 06, 07, 08
writer: do
authority: `superchat-runtime.el` | owned — 退休 Runtime 的旧 Request/Run/Cleanup Action Contract；`superchat-editor-workspace.el` 继续独占 editor ownership/close，本 Slice 不修改或复制该权威
spec_ref: `.specs/session-editor-workspace/SPEC.md` Revision 5 — Removed model；Candidates A/B/D；Compatibility and migration；Issue map S6
context_ref: `KERNEL.md` K10/K11（待退休 baseline）与 K15-K17（须保留的 exact-buffer、terminal、EditorWorkspace 合同）
evidence_ref: E-20260905-001

## Goal

完成 Revision 5 已批准的 breaking retirement：Runtime 删除 HostLease、editor
Cleanup 和旧 host-selection 字段入口，同时保持 exact borrowed buffer、无条件 Run
quiescence、atomic publication、at-most-once delivery、timer/pause 清理与 dead View
不按同名 buffer retarget。EditorWorkspace 仍是唯一 editor owner/close authority；不得
把旧 registry、lease 或 classifier 换名复制。

这是 K10/K11 public Action Contract 的删除，不是“无语义变化”。只在 do/check
完成、learn 记录证据后更新 KERNEL；do 不写知识文件。

## Dependencies

- Slice02：`E-20260903-003`；Slice06：`E-20260904-004`；
  Slice07：`E-20260904-005`；Slice08：`E-20260904-006`。

四项均已验收。`ready` 还取决于下列完整分类、精确 Scope 和可达 Acceptance，
并非依赖满足即自动推出。

## Arrange-time classification

| Class | Finding and treatment |
| --- | --- |
| shipped/core | 旧 structs、acquire/reconstruct/release/retry/abandon、旧字段分支及 `:cleanup-failure` 只在 `superchat-runtime.el`。已迁移 producer 的 `:buffer-name` 仅可作诊断；Runtime 不得用 name locate/create/retarget/authorize kill，entry name 从 exact object 派生。 |
| bundled-plugin | Telega 四 Request shapes 已由 Slice08 改为 exact Session host；两个 producer 文件零旧字段。production 只读，standalone test/guide 纳入验证。 |
| test | Runtime/LLM backend 有旧表示测试；native UI/Compact 有旧 fixture/cleanup 断言；dispatcher/workflow/subagent/plugin 的负向门保护新 producer。只改下列精确 owners，不能机械删除所有命中。 |
| experiment-active | 两个 learning experiments 已迁移且零旧 producer；不改，分别结构检查和 env-unset smoke。 |
| docs-current | `docs/plugin-guide.md` 仍把旧字段列为迁移期 compatibility inputs；改成已移除的 breaking boundary，保留公共 exact-buffer + Session EditorWorkspace 合同。 |
| archive/historical | EVIDENCE、KERNEL 和历史材料合法命中；do 不改、不清历史来制造 zero，交 learn 分类。 |
| external-unknown | 仓库不能证明第三方 Lisp consumer 已迁移；明确 unknown。包含删除的发行版是 breaking，数字 release 未定，发布不在 scope。 |

全树 `:buffer-name` 还服务诊断/展示，不能据字符串命中无限扩张权限。删除的是
Runtime 的 name-based editor-host fallback，而非所有诊断字段。

## Scope and ownership

Do worker 只可修改：

- `superchat-runtime.el`：唯一 production owner；保留 private、无条件、幂等 Run
  quiescence，删除旧 editor lifecycle surface。
- `test/test-runtime.el`：admission、exact identity、publication rollback、结构门。
- `test/test-llm-backend.el`：先迁移 terminal/quiescence/delivery/timer/pause 保障，再删
  lease/state-machine 表示测试。
- `test/test-native-ui.el`：移除旧 nil fixture，保留 View/terminal 行为。
- `test/test-compact.el`：Cleanup-state 断言改为 public terminal/quiescence 与 host
  survival；保留 producer 零旧字段门。
- `plugins/superchat-cowork-telega/test-cowork-telega.el`：standalone plugin、guide、
  experiment、shipped-zero 结构合同；Telega production 文件只读。
- `docs/plugin-guide.md`：public removal/breaking 文案与 current field table。

该精确列表修复了旧 Acceptance 6 与 Scope 的冲突，且均属 Revision 5 已确认的最终
兼容移除 surface。不得借“孤儿文件”修改其他文件。EditorWorkspace、其他 producers/
plugins/experiments、SPEC/KERNEL/EVIDENCE/STATUS/CONTEXT、Protocol/ADR/Runbook/Lesson
均只读。Git/JJ 写、publish/provider 操作均为 none。

## Tests-first order

1. **红灯 admission/identity。** Request 必须有 live exact `:buffer`；missing、nil、
   dead object，以及显式 `:ephemeral-buffer`/`:host-authority-mode`，均须在 host/Run/
   Tape/provider effect 前拒绝。证明 exact object 入 entry、name 从它派生；同名替身、
   foreign callback、legacy field 均不能 retarget。
2. **先迁移真实保障。** 为 success/cancel/provider error/deadline/audit-incomplete 覆盖
   quiescence exactly once、deadline/transport timer 与 provider request severing、
   delivery/stream closure、exact pause、atomic terminal publication、at-most-once
   delivery；覆盖 publication failure/partial-put rollback。所有结果与 rollback 都让
   borrowed exact buffer 存活。然后才删除仅证明 HostLease ID、substitution、
   unresolved/retained/abandoned/retry representation 的测试。
3. **最小 retirement。** 抽出或保留 private idempotent Run-quiescence seam；删除
   HostLease/Cleanup structs、ephemeral acquisition、lazy reconstruction、editor
   releaser、public retry/abandon、`:cleanup-failure` 和旧字段接受/存储/分支。保留
   `superchat-runtime--publish-entry` 与独立 Run registry；不得新增 registry/classifier、
   automatic child-close、durable identity 或 Runtime ownership inspection。
4. **邻接合同。** native UI/Compact 使用 exact buffer 与 public Runtime observations；
   plugin standalone 把 guide 旧字段从“仍接受”改为“已删除、必须迁移”，继续验证
   Telega 四 shapes 和 active experiments。Guide 保留 public exact-buffer + Session
   EditorWorkspace 示例、ordinary borrowed/owned close 边界和 external-unknown 限定；
   不承诺 `2.0.0` 或整个 `1.x` compatibility。
5. **全绿后分类搜索。** executable tests 的旧名仅可处于 rejection/absence/structural
   assertions；current docs 仅可在 removal warning；history 单列。

## Acceptance

1. Runtime 无 HostLease/editor Cleanup、lazy reconstruction、editor releaser、public
   retry/abandon 或 `:cleanup-failure` projection，且无同功能换皮副本。
2. Runtime 不接受/存储/分支 `:ephemeral-buffer`、`:host-authority-mode`，不从
   `:buffer-name` 选择 host；missing/nil/dead exact buffer 和旧字段均 effect-before
   failure。
3. EditorWorkspace 仍唯一 owner/closer；borrowed exact buffer 在 success/cancel/error/
   audit/publication rollback 后存活。Runtime 不从 name/callback/legacy field 推断权威。
4. 所有 terminal paths 仍幂等 quiesce；timers/provider/delivery/stream/pause 正确释放，
   terminal atomic publish、delivery at most once，dead View 不 retarget。
5. 真实保障迁移后才删除纯表示测试；focused suites 单独证明 admission、rollback、
   host survival 和 terminal responsibility，不靠删断言过绿。
6. 分类搜索证明 shipped/core 与 bundled-plugin production 零旧 ownership producers/
   APIs；tests 旧名只用于拒绝/缺失门，guide 只用于 removal warning，两个 active
   experiments 零且分别 smoke。history/external-unknown 单列，不声称外部已迁移。
7. Guide 明确 breaking removal、数字版本未指定且发布不在 scope；示例仅用 public
   EditorWorkspace + exact buffer，并区分 ordinary borrowed 与 owned close 责任。
8. Focused、plugin standalone、canonical、syntax/byte-compile、architecture gates
   通过。Runtime citation drift 是批准的 Action Contract retirement；非本次 movement
   的 drift 为失败并阻塞 learn。

## Verification

```sh
emacs -Q --batch -L . -l test/test-runtime.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-llm-backend.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-native-ui.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-compact.el -f ert-run-tests-batch-and-exit

# canonical 不含 plugin standalone，必须另跑
emacs -Q --batch -L . -l plugins/superchat-cowork-telega/test-cowork-telega.el \
  -f ert-run-tests-batch-and-exit

emacs -Q --batch -L . -l test/run-tests.el
```

对全部 changed `.el` 跑 `check-parens` 和仓库既有 byte-compile 命令。运行 plugin
test 中 guide/experiment 结构断言及两个 active experiment header 指定的 env-unset
smoke。用 `rg` 按七类分别报告旧字段/函数，不混合 pattern/历史计数。

最后运行轻量 Authority gate 与 `docs/runbooks/kernel-check.md` 的非 strict citation
procedure。仅由 K10/K11、K15-K17 Runtime source 删除/移动造成的 non-strict failure
是 **expected approved semantic delta**，不是 do 修改 KERNEL 的许可；其他 authority、
layering 或 citation drift 均失败。do/check 通过后由 learn 记录 Evidence、退休
K10/K11 并协调 K15-K17，再跑 strict Kernel gate。

## Hard stops

出现以下任一项即把 Slice 标 `blocked` 回 `plan`，不得猜：

- shipped Request 无法在不发明 durable Session identity 或修改 Runtime/
  EditorWorkspace public API 的情况下提供 stable live exact buffer；
- 保留 quiescence/publication/delivery 需要 editor Cleanup、automatic child-close、
  新 ownership registry/classifier 或 Runtime ownership inspection；
- 出现分类外 non-test retry/abandon/旧 ownership caller，或 Acceptance 必须写出精确
  Scope；
- Revision 5 与 K17 对须存活的 authority/terminal contract 真冲突，而非已批准退休
  所导致的记录漂移。

数字 release 未定和 repository-external consumers unknown 是需记录的风险，不是本地
arrange/do blocker；版本选择与发布在 scope 外。

## Evidence

`evidence_ref` 保持为空，直到 check 通过且 learn 追加稳定 Evidence。

## Do verification summary (2026-09-04)

Tests-first admission red was 2/2 unexpected (missing exact `:buffer` and an
explicit nil legacy field were accepted); the focused green was 2/2 expected.
Runtime now requires one live exact buffer before Run/Tape/provider effects,
derives its diagnostic name from that object, rejects both retired fields, and
keeps only idempotent private Run quiescence. HostLease/editor Cleanup,
ephemeral acquisition/name fallback, editor releaser, retry/abandon, and the
cleanup-failure projection were removed after terminal/publication guarantees
were migrated to exact borrowed-host assertions.

Focused greens: Runtime 103/0/0; LLM backend 173 expected, 0 unexpected, 5
skipped; native UI 51/0/0; Compact 39/0/0; Telega standalone 45 expected, 0
unexpected, 1 skipped. Both env-unset experiment load smokes exited 0.
Canonical: 1203 expected, 0 unexpected, 17 skipped, `RUNNER_GATE_OK`.
Changed-file `check-parens` passed; production scratch byte-compile passed with
two pre-existing unused-`err` warnings, while direct test compilation retains
the repository's batch-load-context limitations. Authority map is `ok`.
Non-strict Kernel citation check reports 159 errors, all expected handoff drift
from the approved Runtime/test retirement and line movement; KERNEL remained
read-only for learn. Classification: shipped/core has zero legacy producer/API
and one explicit admission-rejection list; bundled-plugin production zero;
tests contain rejection/absence assertions only; both active experiments zero;
current guide has one removal-warning row; 10 historical files contain archived
references; repository-external consumers remain unknown. Existing experiment
`.elc` files were observed and left untouched; this run emitted compile output
only under `/tmp`.

## Required correction (2026-09-05)

The public `superchat-runtime-submit` docstring now matches admission: `:prompt`
and a live exact `:buffer` are required, Runtime borrows and never closes that
object, and an absent `:session-id` is still generated by Runtime.  The prior
159-error citation summary was over-attributed: the checker classified it as a
mix of approved K10/K11 retirement, K15-K17 citation movement, and unrelated
mechanical drift accumulated in the shared dirty worktree.  No KERNEL update is
part of this correction; `evidence_ref` remains empty.  Verification reuses the
independent full-suite results recorded above and adds loaded documentation,
focused admission, `check-parens`, and focused-diff checks for this docstring-only
production change.
