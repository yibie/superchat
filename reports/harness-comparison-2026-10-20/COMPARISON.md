# 四个 harness 的横向比较

日期 2026-10-20。执行者：pi（sc-architect）。只读研究。

版本：Superchat HEAD `01e3d47a`；DeepSeek Harness `c389f96`；pi `a13d35a`；Harness² `1cfe254`（`/private/tmp/harness2`）。

记号：路径相对各仓库根。Superchat 是 Emacs Lisp，其余是 TypeScript（Harness² 是 Python）。每个单元格要么带 `file:line`，要么标"无此机制"并给出说明它没有的那处代码。**代码事实**与**推论**分开标注；"未核对"指该点没在本次通读中落到行号。

---

## 1. 执行单位与停止规则

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 一次执行是什么 | Run + Steps；Run 注册表在 `superchat-runtime.el:123`，终态集 `(completed failed cancelled timed-out audit-incomplete abandoned)` 在 `superchat-runtime.el:93`。 | Session 内 turn→step；`turn()` 在 `packages/core/agent-loop/src/agent.ts:257`，step 内一次模型调用+工具调用。 | 一次 agent loop（外层 follow-up、内层工具/steering）；`packages/agent/src/agent-loop.ts:161-315`。 | 一次 solver **rollout**（substrate 会话）+ propose/compose 阶段；`harness2/core/recursion.py:343-478`。 |
| 什么让它结束 | 终态由 Runtime 封口（成功/取消/超时/审计不完整）；工具轮预算 `superchat-llm-max-tool-rounds` 默认 nil、`superchat-llm-round-limit-action` 默认 `ask`（`superchat-llm.el:103,268`）。 | step 结束：纯文本 `completed`（`agent.ts:470`）、`max-tokens`（`:467`，粘性）；turn 结束：blocked/completed/max-tokens/aborted/error（`:279-321`）。 | `stopReason`：error/aborted/length（token 截断）；`finishTurn` 决策 end/continue（`agent-loop.ts:245,286-291`）。 | ChannelSpec `steps=120, timeout=3600`（`spec.py:43-44`）；k 决定阶段数（R=4k rollouts, P=3k sessions，`ledger.py:28-34`）。 |
| 纯文字回复算不算结束 | 算（Run completed），但 finish-check 可在 chat/inline-act 上补一次 steer 再续一轮（`superchat-llm.el:2128`）。 | 算：`toolCalls.length === 0 → completed`（`agent.ts:469-470`）。 | 算：无工具调用时 `hasMoreToolCalls=false`，`finishTurn` 返回 end 或自然停（`agent-loop.ts:263,286-291`）。 | 算（solver 正常返回即 `mark_complete`，`runmark.py:91-104`）。 |
| 有没有目标循环和上限 | 有：工具轮上限（nil=不限）+ 轮限动作；finish-check 只发一次（`superchat-llm.el:103,268,2128`）。 | 无显式工具轮上限；`max-tokens` 是模型侧上限，`DEFAULT_MAX_PARALLEL_TOOL_CALLS=10`（`constants.ts:6`）。 | 无显式轮上限；`length` 截断时**拒绝执行**这批工具调用（`agent-loop.ts:263-270`）。 | 有：递归预算 k（宽度 agg / 深度 seq），`PHASE_WIDTH=2` 定并行度（`recursion.py:343-478`, `config.py:176`）。 |

**同**：四个系统都把"纯文字回复"当作一次自然结束；都用 step/turn 或 Run 作为执行单位，且都有某种上限（工具轮/token/递归预算）。

**异**：上限的性质不同——Superchat 是**可配置工具轮预算+轮限动作**（默认不限），DeepSeek 是模型侧 `max-tokens`（粘性），pi 是 token 截断时**拒绝执行工具**而非继续，Harness² 是显式递归预算 k。只有 Harness² 把"预算"当研究装置的核心旋钮；只有 pi 把"被截断"直接升级为"这批工具调用失败"。

---

## 2. 工具面与发现

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 常驻几个 | 默认清单 13 个名字（含 timer_*/work_record），`superchat-llm-default-tool-names`（`superchat-tools.el:1706`）；另有 row 专有工具 `work_record`（`superchat-tools.el:1782`）、`delegate_to_subagent` 等（`:2678`）。 | 无固定数字；工具由 Scope 分层注册，registry 按 agent/scope 链解析（`packages/core/tools/src/index.ts:707,804`）。 | `context.tools` 数组，由宿主装配；无仓库级默认清单（`packages/agent/src/types.ts`）。 | 无"工具面"概念——可编辑面是 harness 文件树 `SurfaceSpec`（`harness2/core/tree.py:41`）；solver 是外部 agent（codex/opencode）。 |
| 按名单还是按包 | 按名单（`superchat-llm-tool-names` 只窄化内建集）+ 按 row/Scope 注册；row 专有工具**不受该名单压制**（`superchat-tools.el:1782`）。 | 按 Scope 层注册 + `restrict()`/`guards` 窄化（`packages/core/tools/src/index.ts:1047,1083,1101`）。 | 按工具对象 + `executionMode`（sequential/parallel）逐工具声明（`packages/agent/src/types.ts:309-311`）。 | 按 `SurfaceSpec` 的路径+校验器（`tree.py:92` `is_allowed` 谓词）。 |
| 模型能否自己启用 | 能，但有边界：`define_tool` 合成工具（需审批、Session 级注册）——这正是既有矛盾 C1/C2/C7 的来源（`superchat-tools.el:2104-2112`, `superchat-synthesis.el:72-88`）。 | 不能直接；registry 解析决定哪些可见，模型只能调用已解析名字（`packages/core/tools/src/index.ts:482` 未注册名报错）。 | 不能；工具集变化通过系统消息 `toolsAdded/toolsRemoved` **向模型宣告**，不靠模型自启（`agent-loop.ts:324-362`）。 | 不适用：proposer 编辑的是 harness 文件，不是"启用工具"（`recursion.py` + `tree.py`）。 |
| 插件工具怎么进来 | `superchat-register-tool` 注册进 Scope（`superchat-tools.el`）；MCP/插件工具同一条 Tape 审计包装。 | `ctx.tools.register()` 进 Scope 层（`packages/core/tools/src/index.ts:1047`）。 | 宿主把工具对象放进 `context.tools`（无注册表，直接传参）。 | 不适用（substrate 是 codex/opencode 两个实现，见 `substrate/{codex,opencode}.py`）。 |

**同**：Superchat 与 DeepSeek 都是"Scope/层注册 + 可见性解析"，工具存在与工具可选是两件事；pi 最简（直接数组）；Harness² 根本不是工具式 harness，而是**文件面**。

**异**：只有 Superchat 给模型一个 `define_tool` 入口（合成新工具），这正是它 C1/C2/C7 的根——DeepSeek/pi 都没有模型自造工具的通路。DeepSeek 的分层 `restrict()`/`guards` 比 Superchat 的"名单窄化 + row 专有豁免"更细粒度。

---

## 3. 硬约束与软提示

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 工具循环内跳不过的钩子 | 权限层判定（`superchat-sandbox.el`）、审批 seam（`approval/request`，`superchat-approval.el:588`）、`tools/pre-execute` 只能收紧、Runtime 活性门（`superchat-tool-execution.el`）。 | 沙箱策略（fs 边界，`packages/sandbox/sandbox-policy/src/index.ts`）、审批 waterfall、工具 guards 的单调拒绝（`packages/core/tools/src/index.ts:1108-1114`）。 | `beforeToolCall` 返回 `{block:true}` 阻止执行（`packages/agent/src/agent-loop.ts:727-747`, `types.ts:61-73`）；工具输出截断是硬上限（`truncate.ts:11-12`）。 | `SurfaceSpec.is_allowed`（`tree.py:92`）、`validate`/`check_hooks`（`tree.py:216,336`）、`assert_no_grader_verdicts`（`context.py:454-504`）、账本恒等式 `assert_identity`（`ledger.py:88-91`）。 |
| 哪些只是提示词 | persona/role/safety、operating note、权限上下文叙述。 | `renderPolicyContext` 把政策说给模型（`packages/sandbox/sandbox-policy/src/index.ts:46-58`）、审批政策句（`user-approval/src/index.ts:67-68`）。 | 系统提示 + `toolsAdded/toolsRemoved` 声明（`agent-loop.ts:324-362`）。 | 全部 propose/compose/select 提示词（`harness2/core/prompts.py`，1163 行）；`ANSWER_FREE` 是软提示。 |

**同**：四者都严格区分"代码强制"与"提示词劝导"；Superchat 与 DeepSeek 几乎同构（都有一条审批 waterfall + 沙箱模式 + 政策叙述），pi 把"硬约束"收敛到单点 `beforeToolCall` 钩子，Harness² 把硬约束放在**文件面校验 + 账本恒等式 + 证据不泄题**上。

**异**：pi 没有任何"问用户"的缝——它把权限完全外包给宿主的 `beforeToolCall` 钩子；Superchat/DeepSeek 把审批做成运行时瀑布。Harness² 的硬约束是"研究正确性"导向（防篡改、防泄题、防账本漂移），没有面向交互的权限。

---

## 4. 失败怎么记

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 工具报错是失败还是成功字符串 | 失败是 Tape `tool.failed`（结构化），不是成功字符串；执行管线统一记（`superchat-tool-execution.el`）。 | `tool/result` 带结构化错误文本（不抛断 loop）；`tool/result` 事件（`packages/core/tools/src/index.ts`）。 | 工具错误 → `createErrorToolResult`（错误字符串）+ `isError`，loop 继续不崩（`agent-loop.ts:772,845`）。 | stage 失败 → `fell_back` 标记 + 回退到 base（`recursion.py:170-226`）。 |
| 连续失败有没有处理 | 工具循环无退避；只有 wake/timer 重试有次数+间隔（`superchat-work.el:877-898`, `superchat-timer.el:447-477`）。 | 有：`agent/request-error` waterfall + `retryPolicy`（`agent.ts:414-431`），`llm/retry*` 事件。 | 无显式退避；失败是普通工具结果，交给模型决定下一步。 | 有：`TRANSPORT_ATTEMPTS=2`（`config.py:177`），重试单独计 attempts（`ledger.py:63-64`）。 |
| 拒绝/取消/超时各记成什么 | 拒绝→`approval.decided` deny；取消→`run.cancelled`；超时→`run.failed` payload timed-out；未结清→audit-incomplete（`superchat-runtime.el:93`, `superchat-approval.el`）。 | 拒绝→`approval/decided` outcome rejected/cancelled/unavailable（`user-approval/src/types.ts:32`）；取消→turn aborted；错误→turn error。 | 无审批概念；aborted→"Operation aborted" 错误结果（`agent-loop.ts:623,740`）。 | judge 失败→None 绝不记 0（`judge.py:17,52-81`）；selector 失败→回退已交付 base（`selection.py:279`）。 |

**同**：四者都不把"工具失败"当"成功"，且都区分失败/取消/超时（或拒绝）等终态；Superchat 与 DeepSeek 共享 `approval/decided` 的拒绝词汇。

**异**：Superchat 的连续失败处理只在**无人看守路径**（wake/timer）有退避，工具循环内没有；DeepSeek 把重试做成一等 `request-error` 瀑布；pi 最薄（失败就是普通结果）；Harness² 把失败预算化（重试单独计 attempts，账本有分母）。

---

## 5. 持久化与重建

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 什么写盘 | Tape（append-only SQLite）+ 投影 + checkpoint；工具材料截断后存（`superchat-tape.el:25,40`）。 | append-only 会话日志 + seq + `ignorable` 标记 + 已知事件集（`packages/core/session/src/index.ts`, `known-event-types.ts`）。 | JSONL append-only 会话文件（`packages/coding-agent/src/core/session-manager.ts:977,1187`）。 | runs 目录 + `.adapt_complete` 标记 + harness 指纹 + ledger + library（`runmark.py:91-104`, `ledger.py`）。 |
| 重启后能重建什么 | Run 终态可读（`recorded-run-terminal`，`superchat-tape.el:515`）；投影重放。 | 崩溃尾 turn 合成关闭事件（`interruptedTurnClosers`, `TOOL_OUTCOME_UNKNOWN`，`packages/core/session/src/repair.ts:20-33`）。 | 会话从 JSONL 重建；fork 带 `parentSession`（`session-manager.ts:1069`）。 | 已完成 rollout 按指纹复用（`runmark.py:113-120`）；depth 模式 checkpoint 继承。 |
| 什么只在内存 | 重试计数/armed 计时器/continuing 集合/FIFO 队列（`superchat-timer.el`, `superchat-work.el`, `superchat-runtime.el:4671-4680`）。 | 进程内 agent loop phase/abort（`agent.ts:41-48`）。 | in-memory 会话（`session-manager.ts` `inMemory`）；steering/followUp 队列（`agent.ts`）。 | 无持久进程内状态概念——但"重试次数/等待"这类在研究装置里同样不跨进程（按文档 K 条目）。 |

**同**：四者都是"事件日志/事实"写盘 + "进程内工作状态"只在内存；都支持某种崩溃恢复或续跑。

**异**：Superchat 用 SQLite Tape + 投影 + 显式 checkpoint；DeepSeek 用带 `ignorable` 前向兼容标记的 JSONL 日志 + `known-event-types` 拒绝未知必需事件；pi 用最朴素的 JSONL + fork 树；Harness² 的持久化核心是**指纹化可复用 + 账本恒等式**，为实验可审计而非产品恢复。

---

## 6. 无人看守路径

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 定时 | timer 触发先写 `timer.fired`，task 启动**需要**活 View（可重建），提醒只需 bell/echo（`superchat-timer.el:318-383,579-619`）。 | 有 schedule 包（`packages/schedule/schedule`）；未逐行核对。 | 核心 loop 无定时器；宿主驱动。 | 无定时器——rollout 是提交即跑的 substrate 会话。 |
| 唤醒 | work wake 新建对话 View + 报告回主会话；主 View 不必活，`work.reported` 仍写（`superchat-work.el:1006-1063`）。 | 未核对。 | 无 wake 概念。 | 无 wake——由 sweep 顺序推进。 |
| 子代理 | child 需要活 parent Run + 借用 Scope，不需要窗口（`superchat-subagent.el:1116-1175`）；parent 结束调度取消 children（`superchat-runtime.el:3139-3150`）。 | agent-team/subagent 包存在（`packages/experimental/agent-team`）；未逐行核对。 | fork 会话（`session-manager.ts:1069`）；无独立子代理运行时。 | proposer/selector/solver 都是独立模型会话，headless（`spec.py`, `substrate/`）。 |

**同**：无人看守路径的共性结论是——**事实先写盘，宿主（窗口/buffer）消失不一定取消执行**；Superchat 的 timer/wake/subagent 都遵循这一点。

**异**：只有 Superchat 把"窗口/buffer 死活"当作显式设计约束（timer 启动要 View、goal-round 要 View、subagent 不要窗口）；DeepSeek/pi/Harness² 都是服务端/无头模型，天然无窗口依赖。Harness² 的"无人看守"= 整批 sweep，靠 preflight + 指纹续跑而非窗口。

---

## 7. 审批与权限

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 谁来问、问在哪 | Session 权限层 + `approval/request` 瀑布 + `ask_user` 工具；问题发布到会话/Pending（`superchat-approval.el:588,597-603`）。 | `approval/request` 瀑布 + `approval/asked`/`decided` 审计对（`user-approval/src/index.ts:142,207,273-282`）。 | **无审批缝**；唯一门是 `beforeToolCall` 钩子（`types.ts:322-341`, `agent-loop.ts:804` 注释明示权限检查在钩子里）。 | **无审批**；唯一门是预算（steps/timeout，`spec.py:32-35`）。 |
| 有没有"全自动"档 | 有：`danger-full-access`（sandbox danger + approval never）；`auto` 是 full-access + ask（`superchat-sandbox.el:53-66`）。 | 有：`danger-full-access` sandbox + `never` policy（`permission-presets/src/index.ts:101-104`）。 | 无概念（非交互宿主即全自动）。 | 无（默认无人）。 |
| 全自动下还有什么会问 | `save_buffer` 仍在 ask 档询问（save 例外，`superchat-sandbox.el:266`）；`auto` 档 save 也问（C6）。 | `never` 政策下所有 ask 直接 reject，无例外（`user-approval/src/index.ts:54-56`）。 | 不适用。 | 不适用。 |

**同**：Superchat 与 DeepSeek 的审批/权限模型几乎同构——都是沙箱模式 + 审批政策两旋钮 + `approval/request` 瀑布 + `asked/decided` 审计对。两者都有 `danger-full-access` + `never` 的全自动档。

**异**：Superchat 多一个 `auto` 层（full-access 但保留 ask），且**全自动下 `save_buffer` 仍是例外**（这是 C6 的根）；DeepSeek 的 `never` 是绝对静默拒绝，无例外。pi 把权限整个外包给宿主钩子，Harness² 根本没有交互权限。默认档也不同：Superchat `edit-buffers`（`superchat-sandbox.el:47`），DeepSeek `read-only`（`sandbox-policy/src/index.ts` Config default）。

---

## 8. 证据纪律

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 轨迹怎么记 | Tape 记 tool.requested/execution.started/completed/failed；工具材料截断 2000 字符、内联字节 16384（`superchat-tape.el:25,40`）。 | append-only 日志：turn/step、tool/call、tool/result、assistant/attempt（`known-event-types.ts`）。 | JSONL 条目：user/assistant/tool-result/reset/compaction（`packages/durable/src/entries.ts:15-34`）。 | 证据写成**文件树**，mission 只给短指针，不做 prompt-budget 截断（`context.py:25,105-129`）。 |
| 截断规则 | read 工具 2000 行/50KiB（`superchat-tools.el:1875,1893`）；工具材料逐字符截断+标记。 | `output-retention` 行/字节上限 + `truncated` 标志（`packages/util/output-retention/src/index.ts:52-82`）。 | 工具输出 2000 行/50KB，绝不返回半行（`truncate.ts:11-12,109-110`）。 | `obs_trunc` 逐 observation 生效，trajectory 整体写全（`context.py:119`）；`JB_OBS_TRUNC=50000`（`config.py:156`）。 |
| 判定和任务是否分离 | 是：`recorded-run-terminal` 只读终态、不判全 trace（`superchat-tape.el:515`）。 | 是：judge/projection 与生成分离（未逐行核）。 | 是：`ToolResultEntry` 的 diagnostics 与 content 分离（`entries.ts:24`）。 | **最强**：judge 模块被方法**零导入**（`judge.py:17`），judge 失败= None 绝不 0（`:52-81`）。 |
| 运行能否复现（指纹/种子） | 无指纹/种子追踪（Run 无 seed 记录）。 | 有：`replayState`（`llm/src/assembler.ts:91,196`）+ `request/header` canonicalHeader + `session/end-seed`。 | 有：session fork/seed lineage（`session-manager.ts`）；未见模型 seed 指纹。 | **最强**：harness 树 sha1 指纹 `[:16]`（`runmark.py:32-47`），`is_complete` 按指纹复用（`:113-120`）。 |

**同**：四者都做轨迹持久化与截断标记；Superchat/DeepSeek/pi 的"任务 vs 判定"分离都是同一模式（生成与终态读取/诊断分离）。

**异**：证据纪律的深度差异明显——Harness² 把"证据写成文件、不被 prompt 截断伪造、judge 零导入、指纹防污染"当成研究正确性的生命线；DeepSeek 有 `replayState`/`end-seed` 的复现元数据；Superchat 和 pi **都没有模型级指纹/种子**，复现只能靠重建历史消息，不能证明同 seed 同结果。Superchat 的工具材料截断（2000 字符）比 pi 的 50KB 更激进。

---

## 9. 实验与改进循环

| 问题 | Superchat | DeepSeek Harness | pi | Harness² |
|---|---|---|---|---|
| 有没有轨迹→harness 改动的循环 | **无**。Experience 是只读监控（`superchat-experience.el:2126,2140`），不回头改产品。 | **无**（产品；有 benchmarks 但无自我改进，`packages/benchmarks/`）。 | **无**（产品；有 evals + 统计脚本，无自改，`packages/evals/`, `scripts/*-stats`）。 | **有**：Propose→Probe→Compose RSI 循环（`recursion.py:343-478`, `proposer.py`, `library.py`）。 |
| 成本 | 无实验成本概念。 | benchmark 计时/轮廓（`benchmarks/`）。 | telemetry spans + 统计脚本（`packages/telemetry/`）。 | 账本记 usd/attempts（`ledger.py:63-64,92-101`），R=4k/P=3k 恒等式（`ledger.py:28-34`）。 |
| 分母是什么 | 无。 | 无统一分母。 | 无统一分母。 | 账本 `assert_identity`（R=4k, P=3k）是硬分母（`ledger.py:88-91`）；但选择/反思**不进账本**（`ledger.py:8-10`，对应分析文档 K 条目 6 的 20-30% 成本低估）。 |

**同**：Superchat、DeepSeek、pi 都是**产品/运行时**，没有"把真实轨迹喂回 harness 改自己"的循环；三者至多有评测或遥测。

**异**：只有 Harness² 是**研究装置**——它的存在意义就是 RSI 循环，账本恒等式是它的分母。但它的分母也自认不完整（选择/反思不计入），这是它作为"可复现研究"而非"产品"的代价。

---

## 一句话总览

Superchat 与 DeepSeek Harness 是同源架构（沙箱模式 + 审批瀑布 + append-only 事实日志 + 投影），pi 是最薄的"工具数组 + 单钩子 + JSONL"，Harness² 是唯一以"证据纪律 + 账本分母 + 指纹可复现"为核心的**研究装置**而非产品运行时。四者最硬的分野在**第 8、9 维**：Superchat/pi 没有指纹/种子与自我改进循环，Harness² 没有交互审批。
