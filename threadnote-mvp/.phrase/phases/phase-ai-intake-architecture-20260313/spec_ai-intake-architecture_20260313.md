# Spec — phase-ai-intake-architecture-20260313

## Summary

这个 phase 重新定义 Threadnote 的 AI 主边界：**先理解输入，再决定归类，再整理 thread 状态，再准备输出**。

系统仍保留 Retrieval / Memory 基础设施，但它们回到支撑层，不再充当 intake 主引擎。

## Locked Decisions

| 决策 | 结论 |
|------|------|
| 产品主承诺 | 用户只需要输入，不需要手动整理 |
| Intake 优先级 | Capture Intelligence 先于 Retrieval |
| Routing 主逻辑 | `capture semantics + thread signature + retrieval support` |
| Retrieval / Memory 角色 | 支撑 thread state / prepare / routing 证据，不直接替代输入理解 |
| LLM 角色 | 语义增强、表达增强、关系增强；不是高风险路由决策的唯一来源 |
| 无 backend 约束 | 核心路径必须在 deterministic fallback 下工作 |
| 历史记录边界 | 不自动改写旧 note，不自动大量创建 thread |

## Goals

- 让新 note 在无显式 `#tag` / `@object` 的情况下，仍能得到合理的 item type 和对象抽取
- 将 Capture Intelligence 抽成独立层，脱离 `Store` 内部零散逻辑
- 引入 `ThreadSignature` 作为 thread routing 的主匹配对象
- 让 auto-route 主要由输入语义和 thread signature 决定，retrieval 只做辅助证据
- 保持 Restart Note / current judgment / open loops / next action 的 thread-state 产出
- 保持 Prepare 输出能力，但输入边界更清晰

## Non-Goals

- 自动写完整文章并覆盖用户思考
- 自动改写历史记录
- 自动大量创建 thread
- 自动替用户做高置信决策
- 构建通用 agent / autonomous workflow 系统

## Product Contract

### 1. Capture Intelligence

每条新 note 在进入 inbox / thread 之前，先经过解释层，输出：

- `normalizedText`
- `detectedItemType`
- `detectedObjects`
- `candidateClaims`
- `routingSignals`
- `confidence`

### 2. Thread Signature

每个 active thread 暴露紧凑签名，至少包含：

- `title`
- `goalStatement`
- `coreObjects`
- `activeClaims`
- `latestAnchorSummary`
- `openLoops`
- `lastActiveAt`

### 3. Routing Decision

路由决策只允许三种结果：

- `route(threadID, score, reason)`
- `suggest([threadIDs])`
- `stayInInbox(reason)`

### 4. Thread State

thread 打开时，系统继续生成：

- `Restart Note`
- `current judgment`
- `judgment basis`
- `open loops`
- `next action`
- `thread presentation`

这层可以继续使用 Retrieval / Memory / LLM 增强，但不能反向定义 capture 语义。
并且必须先产出 deterministic `thread state snapshot`，再允许 LLM 做表达增强；LLM 不得直接改写核心状态判断。

### 4.1 Generative UI Contract

thread 页面不再只显示一段 restart prose，而是基于 thread state 生成受限的工作界面：

- `headline`
- `blocks`
- `primary action`

block 只能来自受限集合，例如：

- `judgment`
- `basis`
- `gap`
- `nextMove`
- `evidence`
- `sources`
- `resolved`

### 5. Prepare Output

Prepare 只从当前 thread 构造可写上下文，不替用户做最终内容决定。

## User Flows

### Flow A — Raw Capture

用户输入一句自然语言 note，不加 `#tag`，不加 `@object`。

系统行为：
- 推断 item type
- 抽取对象
- 尝试 route 到现有 thread
- 若不够确定，保留在 inbox 并显示原因/建议

### Flow B — Explicit Capture

用户显式写 `#claim` 或 `@OpenAI`。

系统行为：
- 保留用户显式意图为最高优先级
- 其余语义信号继续参与对象补全、candidate claim 提炼和 routing

### Flow C — Thread Resume

用户打开 thread。

系统行为：
- 基于 thread 当前内容生成 Restart Note / current judgment / open loops / next action
- 输出应代表 thread 当前工作状态，而不是原始 archive 摘录

### Flow D — Prepare

用户点击 Prepare。

系统行为：
- 将 thread 内容整理成受控草稿上下文
- 输出 key claims、evidence、gaps、recommended next steps

## Edge Cases

- note 同时命中多个 thread，但缺少明确差异：停留 inbox，给 suggestion，不自动路由
- note 语义过短或过模糊：不自动路由
- 用户显式 tag 与系统推断冲突：用户显式 tag 优先
- 无 backend provider：仍需完成基础类型推断、对象抽取、路由和 thread state
- retrieval 命中强但 capture semantics 弱：不允许 retrieval 单独决定高置信 auto-route

## Acceptance Criteria

- [ ] 无显式 tag 的普通输入可以得到非全量 `.note` 的基础 item type 推断
- [ ] 无显式 `@` 的普通输入可以抽取至少一部分核心对象
- [ ] auto-route 主要依赖 `capture semantics + thread signature`，而不是纯 retrieval 文档聚合
- [ ] retrieval / memory 仍能支撑 thread state 与 prepare，不退化
- [ ] Restart Note 先建立 thread-specific state snapshot，再渲染为 summary + structured lines
- [ ] Thread 页面可由 thread state 生成 headline + blocks 的受限工作界面，而不是固定 prose+bullet 模板
- [ ] 无 backend provider 时全链路可工作
- [ ] ambiguous note 不会被强制错误归类
