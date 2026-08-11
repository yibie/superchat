# Architectural Diagnosis: Why Experience Injection Is Unreliable

> 综合四份研究 (`agent-loop-control-theory`, `entropy-trajectory-detection`, `trace-design-research`, `event-sourcing-agent-state`) 的诊断报告。
> 面向 Superchat 代码库 (`superchat-llm.el`, `superchat-authority.el`, `superchat-runtime.el`, `superchat-tape.el`)。

---

## 1. 根因：经验注入是"建议"，不是"控制信号"

### 一句话根因

**体验注入发生在错误的架构层：它被当做一条自然语言的 user-role 消息塞进对话上下文（prompt 层），而不是作为结构化的控制约束施加在 Authority 执行缝（execution 层）。**

### 证据链

| 证据 | 来源 | 代码事实 |
|------|------|---------|
| 注入点是 `llm-chat-prompt-append-response real-prompt text 'user` | Control Theory §Injection Point Analysis | `superchat-llm.el` send-request λ, ~L1250 |
| 这是 append-only 的对话文本，没有任何 `tool_choice`、logit bias、constrained decoding | Control Theory §Weakness 1 | `superchat-llm.el` 无 provider-specific control |
| Recovery View 由 `superchat-experience-assemble-recovery-view` 组装，输出 `:prompt` 纯文本 | Control Theory §Control Flow Diagram | `superchat-runtime.el` recovery-intervention-view |
| 被忽略后没有升级机制——同一个注入方式重试，same gain, same failure probability | Control Theory §Weakness 3 | Runtime observer loop 复用相同注入路径 |
| `temperature=0` 时有效是因为 greedy decoding 必然选最高概率 token，把"建议"变成"事实约束" | Control Theory §Temperature sensitivity | Greedy decoding 的性质，非 Superchat 控制 |

### 不是模型层面的问题

根因不涉及：
- 模型能力不足（模型能理解和执行恢复指令）
- Provider 选择不当（问题出现在所有 provider 上）
- 经验质量差（promoted candidate 本身是正确的恢复步骤）

根因在前一层：**把一条正确的指令以错误的形式交给了模型。**

---

## 2. 断在哪一层：为什么 temperature=0 仅仅"掩盖"了症状

### 控制论映射：哪个部件坏了

完整控制回路是 **Sensor → Controller → Actuator → Plant**：

| 部件 | 控制论定义 | Superchat 实现 | 工作正常？ |
|------|-----------|---------------|----------|
| **Sensor** | 测量输出与预期的偏差 | Authority 观察者：检测 `tool.failed`，触发 `recovery-intervention-view` | ✅ 正常 |
| **Controller** | 根据偏差计算控制量 | `superchat-experience-assemble-recovery-view`：查询适用图、选择被 promoted 的 candidate、生成恢复指令 | ✅ 正常（产生正确的恢复步骤） |
| **Actuator** | 把控制量施加到被控对象 | `llm-chat-prompt-append-response` → 一条 user-role 文本消息 | **❌ 断裂点** |
| **Plant** | 被控系统 | LLM provider + prompt → 概率分布 → 采样输出 | ⚠️ 受 temperature 干扰 |

**Actuator 是断裂点。** Controller 已经产生了正确的控制量（恢复指令），但 Actuator 的"执行方式"没有控制力：它在 token 概率分布上只是增加了一个文本前缀，不能保证期望工具必然被调用。

### 为什么 temperature=0 掩盖了问题

`temperature=0` 实质上是**移除 Plant 层的随机性（disturbance）**。它让系统的行为变成了：

```
确定性的 prompt → 确定性的最高概率 token → 确定性的输出
```

这让"建议"（suggestion）在效果上等同于"控制信号"（control signal），**但机制上仍然是 suggestion**。一旦 temperature 恢复非零，这种等价就破裂了：

- 每个 token 采样有 `p = 1 - p_compliance` 的概率偏离恢复路径
- 多步恢复序列的总体合规率是 `(p_compliance)^n`——随步数指数衰减
- 例：若单步合规率 = 0.85，三步恢复序列的端到端成功率 = 0.61

**结论：temperature=0 不是修复，是以"冻结 Plant 随机性"为代价绕过 Actuator 缺陷。** 真正的修复必须在 Actuator 层——让注入机制有足够的 **gain** 来克服 Plant 的随机性（disturbance rejection）。

---

## 3. 三个架构修复方案（按影响/成本排序）

### 方案 A：Authority-seam 结构化注入（高影响，高成本）

**核心思路**：不让经验通过 prompt text 注入，而是让经验向 Authority 注册结构化的执行约束。

```
当前流：
  经验 → 文本注入 → LLM 可能忽略 → tool 执行 → Authority 验收（被动）

改造后：
  经验 → 向 Authority 注册 :tool-preconditions / :tool-blocklist
       → LLM 仍然被告知（soft nudge，保留）
       → Authority 在 pre-execution hook 验证：要调用的 tool 是否符合经验约束
       → 不符合 → 阻止执行 + 针对性错误反馈给 LLM
```

**关键改动**：

1. **扩展 Recovery View 结构**：从纯 `{:prompt "text"}` 变为 `{:prompt "text" :preconditions ({:before-tool "workspace_read" :require-tool "workspace_info"} {:block-tool "bash"})}`。

2. **Authority 新增 `:experience-precondition` hook**：在 `superchat-authority--execute` 的 validate 阶段，检查即将执行的 tool 是否需要前置条件满足。不满足则拒绝执行，返回 `tool.blocked`（带明确原因），不消耗 LLM 调用。

3. **保留文本注入作为 soft nudge**：LLM 仍然收到提示，让它在大多数情况下直接走正确路径；Authority 的 precondition 是 safety net——它只在 LLM 偏离时介入。

**为什么这解决了根因**：控制信号从"影响 token 概率"变成了"在工具执行前做结构化的 gate check"。这是从软约束（prompt）变成硬约束（API gate）。

**与 Tape 的关系**：Authority 的 precondition 检查产生新的 Tape 事件类型 `experience.precondition.enforced`（或 `tool.blocked` with `:block-reason :experience-precondition`）。这让经验是否起效成为**可审计的 Tape 事实**，而不是藏在 LLM 黑盒里的概率事件。

**成本**：高。需要修改 Authority 执行管道、Recovery View 数据结构、candidate 解析/结构化翻译、Experience 模块的注册接口。

**影响**：最高——根因级修复。

---

### 方案 B：Per-attempt 控制增益叠加（中影响，中成本）

**核心思路**：保持 prompt 注入路径，但通过多点注入 + 升级 + temperature override 大幅提高控制增益。

**三层改造**：

**B1. 多点注入（增加 gain）**

不再只在 user-role 位置注入一条消息，而是同时注入三个位置：

```
1. System message 前端：在本次 attempt 的 system prompt 前插入恢复指令
2. User message：保持现有位置
3. 目标工具描述：动态修改 tool description，如 workspace_info 的 description
   前面加上 "[REQUIRED: Call this tool before any workspace_read]"
```

**B2. 升级机制（integral action）**

```
Attempt 1: soft injection（当前方式）
Attempt 2: strong injection + system-message placement + 显式 "DO NOT call X. Call Y."
Attempt 3+: temperature=0 for this attempt + tool_choice forced to recovery tool
```

升级级别和触发条件写入 Tape（`experience.escalation.recorded` 事件）。

**B3. Per-attempt temperature override**

在 Recovery View 中增加 `:temperature` 字段。`send-request` lambda 在注入恢复 View 时临时覆盖 provider 请求的 temperature 为 0（或 0.1），恢复 attempt 结束后复原。

**成本**：中。主要在 `superchat-llm.el` 的 `send-request` 中改造注入逻辑；不涉及 Authority 管道变更。

**影响**：中高。gain 提升显著（三点注入 ≈ 3× gain），但仍然是 prompt 层的软约束，极端情况下仍可能被忽略。

---

### 方案 C：Entropy-based 经验有效性监控（低影响/检测，低中成本）

**核心思路**：不改变注入机制，而是增加一个独立的监控层，用信息论度量持续检测经验是否正在改变行为。作为"先检测、后修复"的第一步。

**具体做法**：

1. **实现 Trace Design 报告的立即推荐字段**：`:duration-ms`、`:attempt`、`:error-code`、token counts。这些让 trace 从"能记录"变成"能分析"（Trace Design 报告 §3.1）。

2. **注册一个 `experience-effectiveness` Projection**（基于 Event Sourcing 报告的诊断 Projection 框架）：从 Tape 事件中按实验组别计算 action 分布的 JSD（Jensen-Shannon Divergence），输出 per-arm 的行为指纹（Entropy 报告 §4 Phase 3）。

3. **仪表盘指标**：
   - `JSD(treatment_arm, control_arm)` 随日历时间的趋势 → 经验起效的早期信号
   - `H(Action | Arm)` / `I(Arm; Action)` → 实验组身份对行为的解释力
   - Bootstrap CI → 区分噪声和真实分离

4. **告警**：如果 JSD(treatment, control) 持续接近零且没有上升趋势 → **经验没有起效**——触发升级（切到方案 B 或 A）。

**成本**：低中。不改变注入机制，纯读路径（Tape → Projection → metrics）。

**影响**：低（不直接修复注入可靠性），但高价值（提供第一个可量化、可告警的经验有效性信号，替代当前的纯主观判断）。

---

### 推荐实施顺序

```
第 1 步（立即）：方案 C — 先有眼睛，再动手
  ├── 加 :duration-ms, :attempt, :error-code, token counts（不改 schema，纯增量字段）
  └── 实现 JSD-based 监控 Projection

第 2 步（一周内）：方案 B — 快速止血
  ├── 多点注入（system + user + tool description）
  ├── 升级机制（3-tier escalation）
  └── Per-attempt temperature override

第 3 步（两周内）：方案 A — 根因修复
  ├── Recovery View 结构化（preconditions + blocklist）
  ├── Authority experience-precondition hook
  └── Candidate → 结构化翻译（natural language → tool constraint）
```

---

## 4. 推荐的持续监控指标

### 主指标：JSD(treatment, control) 随时间趋势

| 维度 | 内容 |
|------|------|
| **指标** | Jensen-Shannon Divergence between treatment arm action distribution and control arm |
| **计算方式** | 在滑动时间窗口内（如最近 50 runs），对每条 treatment arm 分别 vs control 计算 JSD，使用 bootstrap 95% CI |
| **上升趋势** | ✅ 经验正在使行为分布与基线分离——经验在起效 |
| **平稳/下降** | ❌ 经验没有产生可检测的行为变化——经验无效或已过时 |
| **文献支持** | Arigbabu (2026) entropy-based observability；Entropy-Triggered Retraining (2026) 漂移检测框架 |

### 辅助指标

| 指标 | 含义 | 来源 |
|------|------|------|
| `I(Arm; Action)` — 互信息 | 实验组 identity 对 action 的解释力（bits） | Entropy 报告 §1.4 |
| `H(Action \| Arm)` — 条件熵 | 知道实验组后，行为还有多少不确定性 | Entropy 报告 §1.2 |
| `Escalation rate` | 升级到 Tier 2/3 注入的频率（每 100 runs） | 方案 B 升级机制 |
| `Experience precondition enforcement rate` | Authority 阻止了多少次"偏离经验"的工具调用 | 方案 A Authority hook |
| `Recovery success rate` per intervention tier | 每级干预后的恢复成功率 | Trace Design 报告 `:attempt` ordinal |

### 不推荐的指标

- **纯 action entropy（H）**：不结合结果指标时，无法区分"探索性收敛"（好）和"随机乱撞"（坏）——Entropy 报告 §3.3 已有详述。
- **单一成功率**：不能区分"经验让 agent 变好了"和"这批任务本来就更简单"——需要对照组。

---

## 5. Tape.system 与修复方案的关系

### Tape 不是被修复的对象，是修复的"骨架"

四份研究在这一点上高度一致：**Tape + Projection + Closure 组合已经是正确的存储/验证架构**（Event Sourcing 报告 §2）。问题不在 Tape——问题在 **注入点（Actuator）** 和 **缺少诊断性 Projection（分析层）**。

### Tape 如何支撑三个修复方案

| 修复方案 | Tape 的角色 | 新增的 Tape 事件/字段 |
|---------|------------|---------------------|
| **方案 A** (Authority 结构化注入) | 将经验约束的 enforcement 记录为不可变事实 | `experience.precondition.enforced`（包含 `:blocked-tool`、`:block-reason`、`:source-candidate-id`） |
| **方案 B** (Per-attempt 增益叠加) | 记录升级过程，让每一次重试都可追溯 | `experience.intervention.recorded`（已有），新增 `:tier`（1/2/3）、`:temperature-override` 字段 |
| **方案 C** (Entropy 监控) | 作为监控 Projection 的数据源 | 不需要新事件类型；纯读路径（Tape → Projection → metrics），但需要新增 `:duration-ms`、`:attempt`、`:error-code` 等元数据字段 |

### 为什么 Tape 的 append-only 合同是修复的基础，而非障碍

1. **不可变性保证监控的可靠性。** Entropy 监控依赖 JSD 趋势——如果历史数据可被修改，趋势就不可信。Tape 的 append-only 性质确保每个时间点的行为指纹是基于不可变事实计算的（Event Sourcing 报告 §2.2）。

2. **Causation chain 让诊断"回到根因"。** 当 Authority 阻止一次偏离经验的工具调用时，`tool.blocked` 事件的 `:causation-id` 指向触发它的 Recovery View 组装事件，后者又指向原始 `candidate.disposition.recorded`（promote）事件。完整链条：**"这条经验是什么时候谁 promoted 的 → 它被哪次失败触发 → 它在哪次 recovery 中被 enforce"** 全部可追溯（Event Sourcing 报告 §3.2）。

3. **Closure 可验证"经验是否改变了结局"。** 如果方案 A 的 precondition 阻止了一次错误的工具调用并让 agent 走上正确路径，Closure 可以验证这个 Run 的最终状态是否从 `run.failed` 变为 `run.completed`——即经验是否**因果地**改变了运行结果。这是 Trace Design 报告 §4.1 提到的"形式化生命周期验证"的延伸应用。

### Tape 还需要什么

从 Trace Design 报告（§3.1, §3.2）和 Event Sourcing 报告（§3.5, §5.1）：

| 需求 | Tape 现状 | 必要性 |
|------|---------|--------|
| `:duration-ms` per event | ❌ 缺失 | 高——所有诊断 Projection 的基础 |
| `:attempt` ordinal on tool/LLM events | ❌ 缺失 | 高——方案 B 升级机制的计数基础 |
| `:error-code` / `:error-type` | ❌ 缺失 | 高——Health Projection 错误分类 |
| `:parent-event-id` / `:span-kind` | ❌ 缺失 | 中——层级 trace 可视化 |
| `:token-counts` on LLM events | ❌ 缺失 | 中——成本归因 |
| Causation chain 跨 Session | ⚠️ 存在但需应用层解析 | 中——方案 C 跨 Session 子代理追踪 |

**所有这些是增量字段——不需要改变 append-only 合同，不需要迁移已有事件。**

---

## 6. 总结

| 问题 | 答案 |
|------|------|
| **根因** | 经验注入在 prompt 层（自然语言 user 消息），不在 execution 层（Authority structural constraint）。注入是 suggestion，不是 control signal。 |
| **断裂层** | Actuator（`superchat-llm.el` `send-request` 的 prompt injection 点）。Controller 和 Sensor 工作正常。 |
| **temperature=0 的真相** | 以冻结 Plant 随机性为代价绕过 Actuator 缺陷——不是修复，是特定条件下的巧合等价。 |
| **短期修复** | 方案 C（监控）→ 方案 B（多点注入 + 升级 + temperature override） |
| **根因修复** | 方案 A（Authority-seam 结构化注入：preconditions + blocklist） |
| **持续指标** | JSD(treatment, control) 随时间趋势 + bootstrap CI |
| **Tape 角色** | 不可变事件存储是修复的骨架——每一次经验 enforcement、升级、和最终结果变更都写入 Tape，形成可审计的完整证据链。Tape 不需要改合同，只需要增量字段。 |
