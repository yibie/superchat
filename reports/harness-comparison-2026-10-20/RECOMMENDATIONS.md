# Harness 横向比较 — 给 Superchat 的建议

日期 2026-10-20。执行者：pi（sc-architect）。只读研究。

纪律：每条建议必须指回一个 Superchat 事实（代码行，或 `reports/` 下已有测量）。没有事实支撑的建议不写。版本基准 Superchat HEAD `01e3d47a`。

---

## 1. 执行单位与停止规则

**借**：pi 的"`length` 截断 → 拒绝执行这批工具调用"（`pi/packages/agent/src/agent-loop.ts:263-270`）。Superchat 事实：工具轮上限默认 nil（`superchat-llm.el:103`），意味着一个反复触发工具且被截断的回复可能继续烧轮次；目前 Superchat 只在 finish-check 用一次性 steer 兜底（`superchat-llm.el:2128`）。把"token 截断时的工具调用一律失败、不执行"作为硬规则，能堵住截断参数被半执行的风险。

**不借**：Harness² 的显式递归预算 k 作为产品旋钮。Superchat 已有 `superchat-llm-max-tool-rounds` + `round-limit-action`（`superchat-llm.el:103,268`），再加一层 k 是重复；且 Harness² 的 k 是研究装置专用（`recursion.py:343-478`），与交互产品的心智模型不符。

---

## 2. 工具面与发现

**借**：DeepSeek 的分层 `restrict()`/`guards`（`deepseek-harness/packages/core/tools/src/index.ts:1083,1101`）。Superchat 事实：`superchat-llm-tool-names` 只窄化内建集、row 专有工具豁免（`superchat-tools.el:1782`），且 C2/C7 显示工具可见性有进程级泄漏（`reports/architecture-map-2026-10-13/CONTRADICTIONS.md` C2/C7）。给工具一个 per-Scope 的 `restrict` 而非"名单 + 豁免"，能把"模型能看见什么"收敛到一层解析。

**不借**：pi 的"工具集变化通过系统消息宣告"（`pi/packages/agent/src/agent-loop.ts:324-362`）。Superchat 的工具面由 Preset/Request + row 注册决定（`superchat-sandbox.el:7-11` 明确"level 决定怎么执行、不决定有哪些工具"），向模型宣告工具增删是提示词层的事，不是权限层，且会引入新的软提示漂移面。

---

## 3. 硬约束与软提示

**借**：Harness² 的"证据不泄题"硬断言（`harness2/core/context.py:454-504` `assert_no_grader_verdicts`）的思想——在**写盘前**对材料做一次确定性拒绝，而不是靠提示词。Superchat 事实：工具材料截断后落盘（`superchat-tape.el:25,40`），但没有"某些内容绝不该进证据"的硬断言；C5 显示"账本在 SQLite 外"不是安全边界（CONTRADICTIONS.md C5）。给 Tape 加一个可配置的"禁止落盘的模式"校验，比事后提示词更硬。

**不借**：把权限收敛到单一 `beforeToolCall` 钩子（pi 模式）。Superchat 的权限层 + 审批 seam + `tools/pre-execute` 收紧是已被多轮核过的正确分层（`superchat-approval.el:588`, `superchat-sandbox.el`）；pi 的"无审批缝"会让 Superchat 退回没有 `ask_user`/save 例外的状态（C6 依赖这个例外存在）。

---

## 4. 失败怎么记

**借**：DeepSeek 的 `request-error` 瀑布 + retryPolicy（`deepseek-harness/packages/core/agent-loop/src/agent.ts:414-431`）。Superchat 事实：工具循环内**没有**连续失败退避，只有 wake/timer 有（`superchat-work.el:877-898`, `superchat-timer.el:447-477`）。给工具循环一个可配置的"连续工具失败 → 停止/降级"策略，能避免模型在同一错误上烧轮次（当前靠 `max-tool-rounds` 默认 nil 不兜底）。

**不借**：Harness² 把 judge 失败记 None 绝不 0（`harness2/core/judge.py:17,52-81`）的粒度直接搬进来——Superchat 的失败已是结构化 Tape 事件（`tool.failed`、`run.failed` 带 payload status，`superchat-runtime.el:93`），不需要再发明一套 judge 层；只需补连续失败策略。

---

## 5. 持久化与重建

**借**：DeepSeek 的 `ignorable` 前向兼容标记（`deepseek-harness/packages/core/session/src/known-event-types.ts:20-25` + `types.ts:471`）。Superchat 事实：Tape 是 append-only（`superchat-tape.el`），但投影升级时旧事件类型的兼容靠每个 reducer 各自兜底（`reports/architecture-map-2026-10-13/MAP.md` Read models 层）。给事实加"可忽略/必需"分类，能让新版本读旧库时明确哪些事件能安全跳过，而不是靠 `ignore-errors`。

**不借**：Harness² 的 sha1 指纹复用整套（`harness2/core/runmark.py:32-47`）作为产品恢复机制。Superchat 事实：Run 无 seed/指纹，但它的重建语义是"终态可读 + 投影重放"（`superchat-tape.el:515`），不是"跳过已跑过的 rollout"。指纹复用是实验缓存语义，与交互产品的 Tape 追加重放不兼容，且会引入 C1 式的"过期事实被复用"风险。

---

## 6. 无人看守路径

**借**：DeepSeek/Harness² 的"服务端无窗口依赖"作为**方向性目标**，但以 Superchat 已有事实落地：Superchat 已经让 subagent 不需要窗口（`superchat-subagent.el:1116-1175`）、wake 不依赖主 View（`superchat-work.el:1006-1063`），只剩 timer 启动 task 和 goal-round 还要活 View（`superchat-timer.el:354-383`, `superchat-work.el:1314-1356`）。把这两处的"要 View"改成"无 View 时用协议 host 或降级"，就是收敛到服务端无头模型。

**不借**：pi 的"无定时/唤醒"（宿主驱动）作为架构——Superchat 的产品价值之一就是 timer/wake/round 这些无人看守路径（`superchat-timer.el`, `superchat-work.el`），砍掉等于退功能。

---

## 7. 审批与权限

**借**：DeepSeek 的 `never` 政策是"绝对静默拒绝、无例外"的清晰语义（`deepseek-harness/packages/interaction/user-approval/src/index.ts:54-56`）。Superchat 事实：`danger-full-access` 是 sandbox danger + approval never（`superchat-sandbox.el:53-66`），但 C6 显示"allow all"切到的 `auto` 层**仍问 save**（`superchat-sandbox.el:266`），语义被拆成例外。把"全自动 = never 无例外"与"auto = full-access 但仍问 save"的边界写死并同步 UI 文案，能消除 C6 的误导。

**不借**：把权限外包给宿主钩子（pi 模式）。Superchat 事实：审批缝 + `ask_user` + save 例外是已被验证的设计（`superchat-approval.el:588,597-603`, `superchat-sandbox.el:266`）；外包会丢掉"问在哪"（会话/Pending，`superchat-approval.el`）的既定语义。

---

## 8. 证据纪律

**借**：Harness² 的"证据写成文件、prompt 只给短指针、不做 prompt-budget 截断"（`harness2/core/context.py:25,105-129`）作为**长轨迹**的取法。Superchat 事实：工具材料被截到 2000 字符（`superchat-tape.el:40`），read 工具 2000 行/50KiB（`superchat-tools.el:1875,1893`）；对超长轨迹，Superchat 目前是"截断后存"，Harness² 是"文件化 + 只截 observation 不截整体"。给长轨迹一个"文件化证据 + 短指针"的通道，能避免重放时二次截断伪造。

**不借**：要求模型级指纹/种子（Harness² 的 sha1、DeepSeek 的 `replayState`）。Superchat 事实：Run 无 seed，且这是个**成本-收益**问题——Superchat 没有"同 seed 复现"的实验需求（它是交互产品，非评测装置），加指纹属于未证实收益的工程量；当前 C1-C25 里也没有一条用户可见成本是指向"缺指纹"的。等有证据显示需要复现再说。

---

## 9. 实验与改进循环

**借**：Harness² 的"账本分母 + 恒等式校验"（`harness2/core/ledger.py:28-34,88-91`）**思想**，但要落到 Superchat 已有的测量上：`reports/architecture-map-2026-10-13/` 里已有"wake 重试次数先读后试""settle 扫描有欠账门控"等测量（Round 3 核对项）。给 Superchat 的无人看守路径加一个"预算恒等式"（例如 wake 重试次数在 Tape 上对账），能暴露类似 Harness² K 条目 6 的"账本口径偏乐观"型缺口。

**不借**：RSI 自我改进循环（Harness² 的 Propose→Probe→Compose）。Superchat 事实：Experience 是只读监控、不回头改产品（`superchat-experience.el:2126,2140`），且没有把"改进"这件事做成带分母的实验基础设施。把一个交互式 Emacs 产品变成自改进研究装置，与 Superchat 的定位（`superchat-sandbox.el:7-11` 权限分层、append-only Tape）冲突，且成本无证据支撑——不写没有事实支撑的建议。

---

## 不写建议的地方

以下维度本次**不产建议**，因为没有 Superchat 事实能支撑"该借/不借"的判断：

- 工具面"常驻几个"的具体数字对齐——Superchat 的默认清单（13 个，`superchat-tools.el:1706`）是产品决策，无测量表明该增该减。
- pi 的 JSONL/fork 树 vs Superchat 的 SQLite Tape——两者都满足"append-only + 重建"，无 `reports/` 证据显示 Superchat 的 Tape 有重建缺口需要换存储。
- Harness² 的 `PHASE_WIDTH=2` 并行度——Superchat 的无人看守路径没有"并行 rollout"概念，无可比事实。
