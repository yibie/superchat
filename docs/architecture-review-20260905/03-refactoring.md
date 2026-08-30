# 重构与优化：先修对象与作用域，再减少隐式义务

日期：2026-09-05。状态以[第二篇 §1 总表](02-problems.md#1-结论与证据等级)为准；
本篇保留方案及验收边界，不维护第二份进度表。当前 Kernel 为 K26。
输入：[演进](01-evolution.md)、[问题与证据](02-problems.md)。综合决策见
[目标架构](04-target-architecture.md)。

收尾记录：[EVIDENCE.md](../../EVIDENCE.md) `E-20260905-002`、`E-20260905-003`、
`E-20260905-004`。以下保留设计与验收方法；R1 的最小同步首步、R2 和 R3 的
authority 参数清理已完成；Tool Quit accounting 的衍生合同另由
`E-20260905-006` 收口。R1 的仓库异步生产入口由 `E-20260905-007` 以
no-change 收口；全 Render 私有接口迁移不在该结论内。P3–P6 的最终处置以
[第二篇唯一状态表](02-problems.md#1-结论与证据等级)为准。

## 1. 总体策略

不做“大重写”，也不以拆文件为主要目标。把每项改动限定为一个可证明的义务：

```text
已选定对象不再重查名称 → 查询值不再解释成 SQL
             ↓
识别并删掉无行为工作 → 显式化真正的跨模块变化点
             ↓
有测量和产品决定后，再考虑存储/记忆策略迁移
```

前两项互相独立，可以分别验收；后续不是它们的前置条件。所有提案保留 K18 的
append-only、tighten-only Authority、Runtime terminal 与 EditorWorkspace close 合同。

## 2. R1：exact View 贯穿 UI 写入（对应 P1）

### 选择的模型

buffer object 是目标，buffer name 是标签。用户入口允许按名称选择/创建；选定以后，
初始化、渲染和异步 callback 不重新解析名称。Render 管理 transcript region，不管理
Session membership，也不创建资源。

### 最小首步与后续收口

1. 在 `superchat--ensure-chat-view` 和 `superchat--render-session-view` 已取得 exact object 后，初始化与同步重建均作用于该
   object。ensure 必须在 rename、mode 初始化完成后，紧邻同步 Render 调用读取
   `(buffer-name buffer)` 并绑定旧入口；不能只替换外层 let* 的 RHS，否则捕获的是
   rename 前的废弃分配名。reopen 同样在 reset/prompt 各入口前取返回 object 的
   当前名，不用请求名。这只是名称稳定的同步调用段修复，不承诺任意 advice、
   timer 重入或 Render 调用内部的改名/杀 buffer；这些属于后续 exact-object 收口。
2. 后续将 Render 内部写入改为 current-buffer 操作；调用方先以捕获的 object 进入
   `with-current-buffer`。需要跨 callback 的调用显式捕获 object，入口先判断存活。
3. 只有确认所有内部调用迁移后才移除名称兼容适配。对无法确认的外部私有使用，
   明确记录兼容政策，不永久保留两套 owner 状态。

首步预计文件：`superchat.el`、`test/test-native-ui.el`；完整 Render 收口需要
`superchat-render.el` 及真实调用者，必须另列精确范围，不能挤入首步。

### 首步新增验收与后续边界

- 分别执行真实 ensure 初始化和 render-session-view 重建，预置同名 foreign buffer：内容、marker、mode、modified 状态均不被污染；重建历史正确。
- owned View 的 prompt marker 指向 owned object；Request 使用该 object。
- 碰撞 fixture 区分初始分配名、请求名、最终消歧名；不能重新创建废弃初始名 buffer。
- 既有回归继续保持：单次 delivery、dead View no-retarget、owner transcript；不宣称本次已经迁移全部异步 Render 入口。
- 后续完整 Render 收口另证：rename、两个 Session 并行、View 死后同名替身不被晚到 callback 修改。
- 保留 owner-keyed region、Thinking、tool transcript 与独立 prompt；不是删掉
  Render registry 以简化状态。
- 真实 EditorWorkspace/Render/UI；只 stub 外部 provider、显示和临时存储边界。

回退：仅撤销该修复的精确改动，不回滚共享工作区。没有数据/schema 迁移。

## 3. R2：查询作用域由参数保证（对应 P2）

不新增 query language。保留 `superchat-view-entries` 的 Interface 和返回行形，
只用参数表达 kind：

```elisp
;; 设计示意，不是当前实现
;; WHERE session_id = ? [AND id > ?] [AND kind IN (?, ...)]
;; params 顺序：session-id → since-id（若有）→ kinds
```

nil/空 kinds 继续表示不限制 kind；传入字符串作为字面值，不解释 SQL。保持
since-id、limit、排序、material/event 行选择等现有行为。不要顺手重写整个 SQL
只读过滤器；后者有独立公开兼容面。

范围：`superchat-tape-view.el`、`test/test-tape-view.el`。真实临时 SQLite 验证
多 Session、恶意 kind、含引号值、多 kind、cursor/limit 组合；现有 Context View
来源/预算测试作为回归。失败可回退代码，无 schema 和历史 Entry 变化。

实现提示：修复前 since-id 分支会重置 params；先完成 Session/cursor 参数，再追加
kinds，或一次性按占位顺序构造。测试至少一例同时含 since-id、两个 kinds 与 limit，
且正常 kind 必须实际命中，不能让“恒返回 nil”通过负向测试。

## 4. R3：删除 Workflow 无效工作（对应 P7）

已移除 `last-known-authority-mode` 的无效读取、重算和递归传播；私有
`superchat-workflow--run-step` 保留明确忽略的第七位置参数，第八位置仍为 Session，
所以没有把旧调用者的 Session 左移。实际 Request policy 仍来自 Session configuration。

`superchat-workflow--file-path` 的删除建议已经撤回。它现在由
`superchat-workflow-edit` 用于新 recipe 的安全目标路径；已有 recipe 仍由 resolver
返回实际路径。该入口采用单一 Workflow directory、新名称 NFC、已有 NFD 文件不
改名、普通保存且不自动执行（K20，`E-20260905-003`）。

范围：Workflow 与其测试；验证实际 Request policy 不变、死 View 仍保留 Session、
同 role host 复用、unsafe resolver 仍拒绝。收益是少一条“貌似授权来源”的参数链，
不是引入通用 workflow context 对象。

## 5. R4：显式化 Kernel 的实际扩展 Interface（对应 P3/P4）

以下保留审计时的候选设计门。实际处置没有采用一个统一 adapter/control 重构：
P3 逐 seam 验证并只修复 headless 缺席、Tool Quit 与 ordinary tool-wrapper admission；
P4 对当前唯一 controls producer 得到有界 no-change。证据见第二篇唯一状态表。

### 先分三种输入

| 输入 | owner / 生命周期 | 不可混淆 |
| --- | --- | --- |
| Runtime Request | producer 提交；Runtime admission 与执行归属 | 不变成 UI 全局配置 |
| transport controls | Kernel 内部一次调用的配置；异步捕获 | 不承载有效 Authority |
| Authority context | 执行许可、effect、审计与 snapshot | 不能为了少字段而取消防御拷贝 |

### 再选最小设计

- 先盘点每个 `fboundp` 的消费者、缺席结果、失败传播与测试，不机械禁止动态 Lisp。
- 优先复用 `superchat-llm-configure` 的已有 seam；不要为每个调用新建 adapter 类。
- Runtime 的 Request 已有 stream/delivery callback，应优先从 composition root
  提供它们，避免再新增同义配置入口。其他能力逐项定义缺席语义。
- 若引入私有 transport-control 值，入口只捕获一次；callback 读取 lexical
  snapshot，不回查全局默认。按字段区分 absent/nil、函数身份、可变容器。
- 先保持公共 Request plist；字段消费矩阵用于审查，不立即生成 schema/codegen。

### 实施门

每个 adapter 必须有实际生产与 headless/test 使用场景，证明变化点真实存在。
两 Run 交错、迟到工具结果、cancel/quit、加载顺序和 adapter 缺席测试全部可执行。
新 seam 必须减少调用者义务；若只是把十一项变量装进一项但仍到处拆装，拒绝提案。

## 6. R5：读模型 Interface 与记忆策略（对应 P5/P6）

审计时曾建议从一个业务读者试点命名结果。P5 的真实矩阵随后证明各 reader 的
scope、legacy、坏数据与 checkpoint 合同不同，因此没有建立统一 DB facade；若未来
出现第二个相同消费者或测得查询负担，再重新验证复用价值。不要强制 material
rows、FTS、跨 Session 索引都通过 Runtime event replay。

公开 SQL 工具仍是 raw schema 兼容面。收窄它需要单独迁移决定；新增 helper
不能自动消除这个承诺。

`/remember` 已完成召回差异和兼容矩阵（`E-20260905-012`）：未来显式 remember
写窄 typed provenance，Memory 严格验证；显式 recall 只在 exact Anchor 已被选中时
过滤原 bridge Memory。raw user/Anchor、旧无关系 Memory 与修正事实保留；不做历史
backfill、文本去重、refill 或 Continuity representation。历史保持 append-only。

## 7. 文档与测试的优化

`docs/architecture.md` 已校准为 Core 准备 pipeline 与 Runtime completion 的真实
职责，并保留必要扩展示例；已通过独立 docs-only 验收（`E-20260905-005`），
不将静态示例检查当作 ERT 或真实 provider 验证。

测试分四层：纯输入/查询测试、真实模块 Interface 测试、跨模块生命周期测试、
可选真实环境 canary。结构 grep 只检查结构，不能替代行为证据；同样，真实数据库
测试不证明 GUI 正确，stub transport 测试不证明真实模型服务可用。

不按测试数量衡量质量。旧表示测试退役前，先指出其保护的行为是否仍有测试。

## 8. 何时值得做性能优化

目前没有大库 replay 或长会话 Render 延迟测量，不把猜测包装成瓶颈。
下一步测量应覆盖固定规模的 Tape、Context assemble、Projection rebuild、并发 Run
和 UI stream；记录 p50/p95、内存、数据库读取次数及错误条件，之后才决定批事务、
索引或缓存。任何缓存都必须有 owner、失效规则和重建策略。

## 9. 不变合同与执行许可

初次审计仅产出文档与隔离诊断；之后用户授权的 R1 同步首步、R2、Workflow edit
和 R3 authority 清理均已完成 do → 独立 check → learn。P1–P8 已按第二篇证据表
收口；有界 no-change 不是把未实施候选伪装成交付。未知外部/private/advice、真实
GUI/provider/C-g 与未来触发条件仍需各自 plan，既有完成记录不是自动执行许可。
