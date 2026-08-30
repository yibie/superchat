# 从聊天入口到可审计 Runtime：Superchat 的抽象演进

日期：2026-09-05。性质：架构审计稿，不是新 SPEC 或 enacted Kernel。
审计时基线：共享工作区，K18；JJ 已记录快照 `6021ca2b778c` 及 parent
`dd160620213e` 只用于定位历史，不能代表所有当前未提交实现。

关联：[结构问题](02-problems.md) · [优化方案](03-refactoring.md) · [综合架构](04-target-architecture.md)。

当前校准（2026-09-05）：仓库合同为 K26。P1–P8 的唯一当前状态与证据映射见
[第二篇总表](02-problems.md#1-结论与证据等级)。首批 A/B 已独立验收
（[EVIDENCE.md](../../EVIDENCE.md) `E-20260905-002`，当时由 K18 重锚到 K19）；
Workflow 编辑入口已交付并晋升 K20（`E-20260905-003`）；无效 authority 参数线程
已清理且没有 Kernel 语义升级（`E-20260905-004`）；其后衍生的 Tool Quit 合同已
验收并晋升 K21（`E-20260905-006`）。P1 异步生产入口与 P4 controls 已分别以
有界 no-change 验收（`E-20260905-007`、`E-20260905-009`）；P3 headless 与普通
tool-wrapper admission、P5 reader 分类、P6 remember provenance 分别由
`E-20260905-008`、`E-20260905-011`、`E-20260905-010`、`E-20260905-012` 收口。
下文保留历史
和审计时观察，不伪造“当时已正确”。

## 1. 核心判断

Superchat 不是靠一种万能 Agent 抽象长大的。它逐渐把几个原本混在一起的问题分开：

1. 用户想做什么，与模型如何执行分开。
2. 一次运行何时结束，与编辑器资源何时关闭分开。
3. 发生过什么，与当前怎么显示、怎么检索分开。
4. 可以提出什么工具，与实际有权执行什么效果分开。
5. 流程如何组织参与者，与每个参与者的执行机制分开。

这些分离形成可复用模块；复杂功能应该是它们的组合，而不是再复制一套 Runtime。
这条方向基本正确，但 Interface 仍有隐式的加载顺序、动态变量和 buffer 名称义务。
文件已经分开，不表示这些义务也已经被封装。

## 2. 历史证据与解释性重建分开

以下版本是仓库的历史记录，不据此声称所有条目曾以正式版本发布。

| 历史证据 | 当时增加或改变的能力 | 抽象上的意义 |
| --- | --- | --- |
| `CHANGELOG.md:357`，0.2 | 持久记忆、检索与评分 | 聊天开始需要跨轮保留信息，而非只画一段文本 |
| `CHANGELOG.md:349`，0.3 | MCP、工具和模型选择 | 模型连接与工具扩展开始成为可变部分 |
| `CHANGELOG.md:331`，0.5 | gptel → llm.el 硬切换 | 传输实现可以替换；不应让上层绑定旧 sentinel workaround |
| `CHANGELOG.md:281`，0.8 | Org Memory → SQLite/FTS facade | 删除自制检索和存储责任，利用现成机制 |
| `CHANGELOG.md:253`，0.9 | 从单文件抽出 Models/Save/MCP/Render/LLM/Dispatcher | 首先形成文件级模块，尚不能据此证明高内聚 |
| `CHANGELOG.md:163`，1.2.0 | 异步 Workflow、共享 Artifact Workspace | 同一执行能力开始服务更长的组合过程 |
| `CHANGELOG.md:132`，1.2.1，明确标注 untagged | 异步 Subagent、并发窗口与取消 | 参与者组织必须与 transport 生命周期解耦 |
| `EVIDENCE.md` 的 K10–K18 演进记录 | 临时 HostLease/Cleanup 被 Session EditorWorkspace 替代并退休 | 资源生命周期从 Run 中移走，减少冲突的关闭权威 |

可交叉核查的只读历史：`2336396ac904` 抽出 Dispatcher；
`fe363cb8985b` 抽出 MCP/Render/LLM；`1a8e8d495c55` 增加 Tape schema/View；
`181c0aca33dd` 保留 compact 来源与 expand；`8017b48d8e8b` 集中 steer、重启
reconciliation 与 deadline。以上由 `jj --ignore-working-copy log` 获取。

后文的“第一层、第二层”是解释依赖关系，不是伪造各模块的准确创建时间。
旧 CHANGELOG 对 CI、最低 Emacs 版本等描述不能替代当前 AGENTS 和包头。

## 3. 第一层：把输入准备成可检查的数据

简单思想：先弄清请求，再产生效果。

- [Core](../../superchat-core.el) `superchat-turn` 用 struct 承载输入和准备结果。
- `superchat-core-prepare-turn` 顺序执行解析、system prompt、build prompt。
- `superchat-core-run-turn` 现在只是兼容名称，不执行 completion 副作用。
- [UI](../../superchat.el) `superchat-submit` 先构建 Request Plan；inspect
  路径显示计划，执行路径才进入 `superchat--execute-plan-in-chat`。

价值是“检查”和“执行”可以共享准备过程，而不需要让查看请求触发一次模型调用。
限制也很重要：注入的 hook 可以任意执行 Lisp，因此“准备 pipeline”不能被泛称为
数学意义上的纯函数；它是一个有约定的可扩展准备过程。

## 4. 第二层：统一执行，不让每个功能自己收尾

简单思想：所有模型工作经过同一 Run 生命周期。

主路径是：

```text
交互入口 / Workflow / Subagent / blocking adapter
  → 构建 Request（固定目的、策略和 exact buffer）
  → Runtime.submit
  → LLM transport → Authority 工具执行 → 后续模型轮次
  → Runtime terminal publication → delivery → quiescence
```

[Runtime](../../superchat-runtime.el) 的关键落点是
`superchat-runtime-submit`、`superchat-runtime--finish` 和
`superchat-runtime--quiesce`。submit 必须有 live exact `:buffer`；buffer 只被借用，
不随 Run 结束而关闭。显式退役字段即使值为 nil 也不能继续进入旧路径。

深度（Depth）不在于 Runtime 有多长，而在于调用者不必再分别实现取消、超时、
晚到回调排除、publication 回滚和单次 delivery。
真实保障见 [Runtime tests](../../test/test-runtime.el) 的
`runtime/every-live-terminal-path-quiesces-exactly-once`、
`runtime/exact-buffer-publication-rollback-preserves-caller-object`，以及
[LLM tests](../../test/test-llm-backend.el) 的
`test-runtime-quiescence-is-idempotent-and-preserves-borrowed-host`。

## 5. 第三层：历史、当前状态与判断各有归属

简单思想：事实不重写，视图可以重建。

| 模块 | Interface 隐藏的复杂性 | 不能替代它的邻居 |
| --- | --- | --- |
| [DB](../../superchat-db.el) | SQLite schema、事务、索引、存储游标 | Tape 不是 SQL 引擎 |
| [Tape](../../superchat-tape.el) | 事件身份、来源、顺序、redaction 与 append | DB 行不自动等于合法 Runtime 事实 |
| [Projection](../../superchat-projection.el) | reducer 注册、catch-up、checkpoint 与 rebuild | 物化当前状态不是新的历史权威 |
| [Closure](../../superchat-closure.el) | 用轨迹解释 Run/Turn 的可证明状态 | 不控制 Run，不存第二份运行状态 |

具体组合不是口号：`superchat-projection-register`（第 24 行）接收 reducer 与
selector；`superchat-projection-catch-up`（第 64 行）把每个事件应用和 checkpoint
更新放进同一个 DB 事务。`superchat-closure-judge`（第 783 行）接收 query 和 trace，
返回判断而不是自己结束 Run。

Memory、Artifact Workspace、Claims、Experience 应分别消费这些机制，不能因为
都使用 Tape 就合并成一个万能“知识库”。Anchor 在 View 中代表来源，不删除原 Entry。

## 6. 第四层：复用 Emacs，而非模拟一个编辑器

简单思想：Session 是逻辑身份，buffer 是资源，tab/project/ibuffer 是组织与呈现。

EditorWorkspace 按 Session 与 role 创建、复用 exact buffer，并独占 membership
与 close。tab 只能指向 Session，不证明它显示过的所有 buffer 都属于该 Session。
这保留了 Emacs 中同一 buffer 可在多个窗口、tab 出现的事实。

[Widgets](../../superchat-widgets.el) 是一个小而有效的例子：
`superchat-side-window-show` 把 panel kind 映射到原生 side-window slot；
`superchat-widgets-button` 统一必要的 widget keymap。
它复用 `display-buffer-in-side-window`，没有自建窗口管理器。

[Plan](../../superchat-plan.el) `superchat-plan-propose` 先保存 Tape 与 plan
数据，再尝试显示；TextUI 失败不能反向否定已记录的 plan。
`superchat-plan-approve` 从 plan 数据读取选择，而不是解析屏幕上的文字。
这是“状态是源、界面是投影”的直接代码证据。

不过 exact identity 还没有贯穿所有 UI 写入。审计复现的同名 foreign buffer
写入问题见第二篇 P1，其同步初始化/重建路径现已修复；不能据此推导全部异步 UI 已正确。

## 7. 什么抽象值得保留

一个完整组合例子是 Workflow 的一步。`superchat-workflow--run-step` 准备
替换后的 prompt、Session、exact buffer 和 delivery callback，并调用
`superchat-runtime-submit`。Runtime 调用 LLM transport；若模型请求工具，执行最终
经过 `superchat-tool-execute`，而不是因为 Workflow 选了工具就获得授权。
Authority 的 `superchat-authority--append-event` 将工具证据交给
Tape；Runtime 拥有 Run 的完成和交付。收到 completed 也不是 Workflow 无条件
继续的许可：callback 调用 `superchat-workflow--resume-decision-for-step`，经
`superchat-workflow--resume-decision` 重放 Tape 并调用 `superchat-closure-judge`。只有
durable terminal 被证明且为 completed，才递归下一步；不完整证据转 manual-review，
无效 trace 停止。这不是一个万能循环，而是“顺序组织、执行、权限、事实、证明”
五种小职责通过明确结果组合起来。没有工具请求时自然跳过工具分支。

- Runtime / EditorWorkspace：两种不同生命周期，合并会重新引入关闭冲突。
- Tape / Projection / Closure：事实、派生状态和证明是不同问题。
- tool catalog / Authority：可发现不等于有执行权。
- Plan data / panel：渲染失败不应成为业务失败。
- Workflow / Runtime：前者组织顺序，后者拥有每次执行；应审查重复状态，而非直接合并。

衡量模块的标准是“删除它，复杂性会不会重新分散到调用者”，不是行数或文件数。
真正的下一步是缩小隐式 Interface，而不是再次大规模拆文件。

## 8. 证据范围

本稿基于当前源代码、仓库历史与前序独立验收，并增加一次隔离 Emacs 的 UI
碰撞复现。未访问外部消费者、未执行真实 provider、未做 GUI 多窗口长期使用或
大数据性能测量。同模型串行审查与修订记录集中在第四篇，不把分区调查冒充三轮审查。
