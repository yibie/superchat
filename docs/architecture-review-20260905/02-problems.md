# 当前结构问题：已复现缺陷、维护负担与未知边界

日期：2026-09-05。状态：保留审计时问题证据；当前合同为 K26。唯一当前状态总表
见 §1，其余三篇只引用本表，避免维护四份进度台账。
关联：[演进](01-evolution.md) · [优化方案](03-refactoring.md) · [目标架构](04-target-architecture.md)。

收尾证据：[EVIDENCE.md](../../EVIDENCE.md) `E-20260905-002`、`E-20260905-003`、
`E-20260905-004`、`E-20260905-005`、`E-20260905-006`、`E-20260905-007`、
`E-20260905-008`、`E-20260905-009`、`E-20260905-010`、`E-20260905-011`、
`E-20260905-012`。P1/P2 下文代码描述、
输出及附录是修复前的复现记录；P7/P8 各节也明确区分审计时与当前。

## 1. 结论与证据等级

系统的主干分工值得保留。当前最具体的问题不是缺少抽象，而是已选定的身份、
作用域和策略在穿越模块时，又被名称、字符串拼接或隐式上下文重新解释。

| ID | 当前状态 | 已验收证据或剩余边界 |
| --- | --- | --- |
| P1 | 完成 | 同步初始化/重建修复已验收（`E-20260905-002`）；仓库异步生产入口经组合回归确认为 no-change（`E-20260905-007`），不等于全 Render 私有接口迁移 |
| P2 | 完成 | Tape kinds 参数化与 Session/cursor/limit 合同已独立验收（`E-20260905-002`，当时 K19） |
| P3 | 完成 | 三条隐式 seam 定向调查为有界 no-change；Tool Quit accounting（`E-20260905-006`）与 Runtime→Render headless 缺席兼容（`E-20260905-008`）已验收；普通 raw tool Request 无 callable wrapper 时由 LLM 在 provider 前 fail closed（`E-20260905-011`，[ADR 0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md)/K25） |
| P4 | 完成 | 当前唯一 controls producer 与 callback 模型经双 Run 工具轮次、取消和 retry 交错验证为有界 no-change（`E-20260905-009`）；未来触发条件不在本结论内 |
| P5 | 完成 | Memory/Workspace/Team/Claims 的 scope、legacy、坏数据与 checkpoint 差异经真实 SQLite 矩阵确认为有界 no-change（`E-20260905-010`） |
| P6 | 完成 | future-only typed provenance 由 Memory 严格验证；显式 recall 仅在 exact Anchor 已选中时过滤对应 bridge Memory，不回填、不回溯（`E-20260905-012`，K26） |
| P7 | 完成 | Workflow authority 无效读取/重算/传播已清理（`E-20260905-004`）；兼容槽保留 |
| P8 | 完成 | 指南已校准并独立验收（`E-20260905-005`） |

**验证范围（不改变上述完成状态）。** Workflow edit 的两个真实 NFC ambiguity
测试因当前文件系统不能保留两个物理上不同但 NFC 等价的名称而 capability-skip；
现有纯分类以及真实 resolver、Unicode、symlink 路径通过，但不能据此宣称所有平台的
真实 NFC 歧义路径都已执行（`E-20260905-003`）。P5 的 Workspace standalone 为
30 total / 29 expected / 1 unexpected：该历史 fixture 在 lexical binding 下未将
`superchat-buffer-name` 声明为 special，使局部 `let` 对产品 `boundp` 不可见并走到
legacy fallback；canonical 的加载顺序会声明该变量并通过。这不是产品同名 buffer
漏洞，也不代表全部 standalone 测试为绿；该 fixture 未在 P5 范围外顺手修复
（`E-20260905-010`）。

这些结构问题不等于已发现远程权限提升；特别是能执行任意 Emacs Lisp 的调用方
本来就有广泛权限。不能把接口范围错误夸大成未知外部攻击已经成立。

## 2. P1：所有权正确，写入对象却错了

**证据链。** [superchat.el](../../superchat.el) `:2365@superchat--ensure-chat-view`
取得 EditorWorkspace 的 exact buffer，`rename-buffer ... t` 在重名时正确消歧。
但是局部 `superchat-buffer-name` 仍是请求名称。
[Render](../../superchat-render.el) `:139@superchat--insert-prompt` 又执行
`(get-buffer-create superchat-buffer-name)`，不使用已选定的 object。

隔离 Emacs 中，先创建内容为 `FOREIGN` 的普通 buffer 占用请求名称，再调用真实
`superchat--ensure-chat-view`。加载 `test/test-native-ui.el` 使用 test-helper 临时
数据目录，并设置 `superchat-manual-models=t`，未运行 provider。结果：

```text
distinct=t
foreign="FOREIGN\n\n* User: "
owned="#+TITLE: superchat\n"
owned-prompt=nil
```

**影响。** 非所属 buffer 被修改，返回的 Session View 无正确输入标记。已证的是
prompt 误写，尚未证明秘密泄漏、后续模型执行或所有 UI 命令都受影响。

R1 独立审查追加：`:2416@superchat--render-session-view` 在 ensure 返回后再次绑定
请求名称。临时模拟 ensure-only 修复，再执行真实重建，foreign 仍追加 prompt，
即使 owned prompt 已正确。因此首步必须同时覆盖初始化与同步重建。

**结构根因。** 名称既是展示标签又是重新定位键；exact identity 仅在资源分配和
Runtime admission 处成立，没有贯穿渲染链。Render 的 reset/prepare/stream 等
入口也多处使用名称，属于同一审计面，不代表每处都已复现错误。

**最小切法。** 创建 View 后只以返回 object 执行初始化。Render 修改已有 View 时
使用该 exact object/current buffer；选择或创建对象留在用户入口。不能新造 marker
classifier，不能杀掉同名 foreign buffer，也不能把“更新名称”当长期身份合同。

**反证/验收。** 重名、重命名、目标死亡后同名替身、两 Session 交错渲染都必须证明
foreign 内容与 marker 不变；返回 View 的 prompt marker 属于它自己。已有单纯
ownership 测试和 Runtime no-retarget 测试不能替代这条完整 UI 路径。

## 3. P2：参数化的 Session 被非参数化的 kinds 绕过

[Tape View](../../superchat-tape-view.el) `:25@superchat-view-entries` 对 Session、
cursor 使用参数，但把每个 kind 直接用 `'%s'` 拼到 `IN (...)`。
[DB](../../superchat-db.el) `:411@superchat-db-tape-select` 的只读限制不检查查询
是否仍局限于请求的 Session。

在 test-helper 的临时 SQLite 库，用 `superchat-tape-append-material` 分别向
`arch-A`、`arch-B` 写入 `A`、`B`，调用：

```elisp
(superchat-view-entries "arch-A" nil '("user"))
;; content => ("A")
(superchat-view-entries "arch-A" nil '("user') OR 1=1 --"))
;; content => ("A" "B")
```

**影响边界。** 已证查询 Interface 的 Session 过滤失效；未发现真实外部输入能直接
控制 kinds，也未发生数据库修改。只读 SQL 不等于范围正确。

**最小切法。** 为 kinds 生成 `?` 占位符，值按顺序绑定。nil/空列表保持原义，
不借此禁用公开 SQL 工具或重做 Tape schema。

**反证/验收。** 真实 SQLite 测试覆盖引号、注释、多个 kind、空列表、since-id 与
limit 组合；恶意字符串只能成为字面 kind 值，不能返回别的 Session。

## 4. P3：物理分层门与实际 Interface 不一致

已读证据：

- [Runtime](../../superchat-runtime.el) 声明 Render/Experience 符号，
  `superchat-runtime--deliver` 与 `superchat-runtime--recovery-intervention-view`
  调用中层行为。
- [Authority](../../superchat-authority.el) `superchat-authority--run-hooks`、
  `superchat-authority--filter-result` 通过 `fboundp` 发现 agent-loop 行为。
- [LLM](../../superchat-llm.el) `superchat-llm-configure` 已有显式 adapter
  Interface，但其他路径仍按全局符号发现 wrapper/synthesis。
- [结构测试](../../test/test-runtime.el) `:661@runtime/kernel-requires-stay-inside-kernel`
  扫描 require；它不证明不存在跨层符号调用。

这是已证的隐藏 Interface 和加载顺序负担，不直接宣称违反现有“require 不跨层”
合同，也不证明 headless load 当前失败。

**建议。** 先列出真实跨层调用及缺席语义，再逐个利用现有 adapter 机制收口。
若新增 Interface 或改变 authority map，必须单独确认；不要为了漂亮依赖图直接
禁掉全部 `fboundp`，也不要引入全局 service locator。

**当前校准。** Authority→Agent Loop 的 hook/filter seam 已完成定向调查，加载前后
的缺席行为与现有 headless 兼容有真实用途，因此结论为 no-change；跨层符号调用也
不等同于违反 require layering。该调查随后暴露的 Tool Quit 记账问题已经按独立
Action Contract 修复验收（`E-20260905-006`，K21）。后续定向调查确认
Runtime→Experience 与 LLM→Synthesis 无需改动，并修复了 Runtime 在 Render
缺席时的 headless status/stream 崩溃（`E-20260905-008`，K23）。普通、未 prepared
且携带工具的 Request 若没有可信 callable wrapper，现在由
`superchat--llm-generate-answer` 在 prompt/provider/effect 前拒绝
（`E-20260905-011`，[ADR 0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md)/K25）。prepared 工具是仓库内部可信边界，不是安全
认证，也不是外部 caller 绕过迁移的开关；未知外部 direct caller 仍未知。

## 5. P4：动态参数总线与开放 Request 的维护成本

Runtime 的 transport controls 声明在文件前部，`superchat-runtime-submit` 绑定 prepared
tools、wrapper、round/retry、run-active、timer owner、evidence/context 等变量。
LLM 再声明、读取并把异步所需部分捕获到 lexical closure。
`runtime/forwards-request-local-transport-controls` 直接验证这些 special variables。

同一 Request 还分别投影成 Turn、Run entry、Tape payload、Authority context、
LLM 输入。不同投影有必要，但字段的新增、缺省和拷贝责任分散。

**不是已证 bug。** 现有 callback 隔离测试不能被忽略，动态绑定本身也符合 Emacs
习惯。负担在于开发者必须知道整条传播链，而非调用一个较深 Interface。

**可选收口。** 私有 transport controls 在入口统一捕获；保留 `nil` 与 absent 的
区分、callable identity 和必要的防御拷贝。Authority context 不与它合并。
公共 Request plist 暂不换类型，也不立即拒绝未知 key：未知外部 Extension 可能
使用额外字段。先记录字段消费者矩阵，再决定是否值得引入描述表。

## 6. P5/P6：存储兼容面与记忆表示

P5 的消费者包括 [Memory](../../superchat-memory.el)
`superchat-memory--projection-select`、[Claims](../../superchat-claims.el)
`superchat-claims--normalize-basis`、[Workspace](../../superchat-workspace.el)
`superchat-workspace--projected-artifacts`、[Subagent](../../superchat-subagent.el)
`superchat--team-state`。它们接触 SQL、列位置或 meta JSON。

Tape View 本来就是查询 Adapter，其直接依赖 DB 不应被算作违规。问题是其他业务
模块需要同时理解自己的领域与物理行格式。公开 `superchat-tool-sql` 又令 raw schema
成为外部可见面；不能用新 query helper 伪称消除了这一兼容义务。

P6 的直接证据在 [superchat.el](../../superchat.el)
`superchat--cmd-remember`：写 user material、Anchor，再按默认兼容开关调用
`superchat-memory-capture-explicit`。Memory 的 FTS/review/correction 有独立价值；
不能删掉 Memory，只因它和 Anchor 记了相近文字。

**当前校准。** P5 的不同读者合同经真实 SQLite 矩阵确认为有界 no-change
（`E-20260905-010`），没有建立统一 DB facade。P6 已采用 future-only strict typed
provenance（`E-20260905-012`）：Memory 独占关系恢复与验证；只有对应 exact Anchor
实际出现在本次已限额的 Tape 结果时，显式 `/recall` 才过滤原 bridge Memory。
raw user 与 Anchor 仍保留；普通、同文、历史无关系、跨 Session、修正 Memory 都不
按文本或裸 ID 去重。没有 backfill、refill、Anchor 递归或 Continuity
`:represented` 变化，旧 Tape 不删除。

## 7. P7：低风险简化不必升级成新框架

审计时，[Workflow](../../superchat-workflow.el) `:520` 捕获
`last-known-authority-mode`，`:556` 接收，`:588` 重算，`:683` 递归传递。
实际 Request 的 authority policy 来自 Session configuration。该值没有行为消费。

当前无效读取、重算和递归传播已经移除（`E-20260905-004`）；第七位置保留为明确
忽略的兼容槽，第八位置仍是 Session。policy 来源和 Kernel 合同不变。

审计时曾把 `superchat-workflow--file-path` 记为只有测试引用，并提出删除候选；该
建议已经撤回。当前 `superchat-workflow-edit` 使用它计算安全的新文件目标，并以
既有 resolver 打开已有实际路径（`E-20260905-003`，K20）。它不是待删的死 helper。

**保留边界。** 外部私有 arity 依赖仍未知，因此兼容槽不左移。安全 resolver、
live/dead View 的真实 authority 测试继续保留；Workflow edit 不新增保存层、执行
路径或 provider 调用。

## 8. P8：给开发者的旧架构说明仍在制造错觉

审计时，[docs/architecture.md](../architecture.md) 第 7 行起写“每个 Turn 经过
core-run-turn，包含 post-turn effects”；Core 的 `superchat-core-prepare-turn` 和
兼容名 `superchat-core-run-turn` 只做准备，completion 属于 Runtime。

这会让后续代码在错误的 owner 加 hook。指南现已按当前 owner 校准，并通过
独立 docs-only 检查（`E-20260905-005`）；不重写 CHANGELOG 和历史 ADR。本次四篇是
审计材料，不自动晋升为另一份相互竞争的 Kernel。

## 9. 合并调查结果与拒绝的简化

三个分区报告已合并为上面 P1–P8：UI 问题以 lead 的真实复现为准；SQL 字符串
风险由静态线索升级为本地行为证明；Request 宽度与动态总线合并为 P4；其余候选
保持证据等级，不按发现数量计功。

明确拒绝：合并 Runtime 与 EditorWorkspace、合并所有读模型、删除 Authority
snapshot/验证、统一 Skills 与 Subagent 的关闭生命周期、把 MCP 连接与工具可见性
合为一张真值表、为了减少文件而拆 Experience、未测量就加 replay 缓存。

## 10. 覆盖与盲点

已覆盖：Core/Runtime/LLM/Authority；DB/Tape/Projection/Closure/Memory/Claims/
Experience/Workspace；Dispatcher/Extension/EditorWorkspace/Workflow/Subagent/Skills/
MCP/Tools；UI/Render/Plan/Widgets；Org/Magit/Rewrite/Telega 的主要调用链。
Models/Save/GenUI/Emacsclient、具体每个工具和所有配置只做入口级抽查，未全审。

未知：外部私有调用与 advice、真实 provider/MCP/Telega、GUI 多 frame、实际数据库
升级与 crash 恢复、大库性能和长时间运行。前序 canonical 全绿是历史独立验证，
不是审计轮重跑，也不能否定当时 P1/P2 的新增反例。后续修复验收见顶部 Evidence。

## 附录：两个最小反例的执行入口

在仓库根目录，用 `emacs -Q --batch -L . -l test/test-native-ui.el --eval EXPR`
运行下面完整 Lisp（EXPR 是该表达式，不是字面占位符）。test-helper 使用临时数据目录；
`superchat-manual-models` 禁止模型预取。只清理本探针创建的两个 buffer。

```elisp
(let* ((name (generate-new-buffer-name " *arch-foreign*"))
       (foreign (get-buffer-create name))
       (superchat-manual-models t) owned)
  (unwind-protect
      (progn
        (with-current-buffer foreign (insert "FOREIGN"))
        (setq owned (superchat--ensure-chat-view name "arch-audit-collision"))
        (princ (list :distinct (not (eq owned foreign))
                     :foreign (with-current-buffer foreign (buffer-string))
                     :owned (with-current-buffer owned (buffer-string)))))
    (dolist (buffer (list owned foreign))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions nil kill-buffer-hook nil))
        (kill-buffer buffer)))))
```

P2 改用 `emacs -Q --batch -L . -l test/test-helper.el -l superchat-tape.el
-l superchat-tape-view.el --eval EXPR`，表达式如下。该 fixture 只写临时库；不操作用户库。

```elisp
(progn
  (superchat-tape-append-material "arch-A" "user" "A")
  (superchat-tape-append-material "arch-B" "user" "B")
  (princ
   (list :normal
         (mapcar (lambda (row) (nth 3 row))
                 (superchat-view-entries "arch-A" nil '("user")))
         :injected
         (mapcar (lambda (row) (nth 3 row))
                 (superchat-view-entries "arch-A" nil '("user') OR 1=1 --"))))))
```

这两个入口是修复前缺陷探针，不是完整验收矩阵。当前代码运行时不应再产生所列
错误结果。真实 reopen 的额外反例由 R1 独立 reviewer 执行；实现时已补入该入口
及 foreign 既有 Render marker 的真实回归，不能只凭 ensure 探针证明全部路径。
