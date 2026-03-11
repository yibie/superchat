# Spec: Thread Input Language

## Summary

Threadnote 使用一套最小输入语言来描述 note 在工作流中的意义，并将其作为后续 parser、ledger、object aggregation 与 note focus view 的统一事实来源。

这套语言只包含 3 个语法层：

- `#role`：这条 note 在 thread 推进里扮演什么角色
- `@object`：这条 note 正在谈谁或什么
- `[[reference]]`：这条 note 明确引用另一个 note 或 thread

一句话定义：

`Threadnote lets people keep thinking continuity by writing notes whose role, object, and references are explicit at capture time.`

## Goals

- 让用户在写 note 时，用极少的语法表达这条 note 的工作角色
- 让 `Settled So Far` 直接从 notes 的稳定语义生成，而不是从平行 schema 拼接
- 让对象追踪和 note 角色分离，避免 `#tag` 膨胀为混乱分类系统
- 让 thread 内部引用显式化，支持恢复现场和回到上次讨论点

## Non-goals

- 不做开放式任意 hashtag 系统
- 不要求用户在 capture 时先选择对象类型
- 不引入完整 wiki/markdown 语法宇宙
- 不让 `[[reference]]` 取代 reply 层级
- 不把对象系统做成通用知识库 ontology

## Core Concepts

- `Note`：用户写下的一条记录
- `Thread`：长期推进的问题空间
- `Role`：一条 note 在工作流中的角色，由 `#role` 表达
- `Object`：被持续提及和追踪的实体，由 `@object` 表达
- `Reference`：一条 note 对另一条 note 或 thread 的显式引用，由 `[[reference]]` 表达

## Syntax

### 1. `#role`

`#role` 表示这条 note 在 thread 推进里扮演的角色。

规则：
- v1 使用受控词典，不支持用户自定义语义 tag
- 一条 note 默认只有 1 个主 role
- 如果没有写 `#role`，系统默认视为 `#note`
- 如果写了多个已识别 `#role`，v1 只取第一个作为主 role，其余保留为普通文本，不进入语义层
- 未被系统识别的 `#tag` 不进入语义层

#### v1 Role 词典

核心 discourse units：
- `#question`：一个需要被回答或推进的研究问题
- `#claim`：一个判断、主张或阶段性结论
- `#evidence`：支撑或反驳某个 claim 的观察、事实或数据
- `#source`：外部引用、材料或出处

宽松 capture roles：
- `#note`：未进入明确 discourse 角色的普通记录
- `#idea`：一个还未收敛成 claim 的想法或方向

推进 roles：
- `#comparison`：两个对象、样本或判断的比较
- `#pattern`：跨样本归纳出的模式
- `#plan`：下一步计划或推进方案

稳定 roles：
- `#decided`：已经做出的决定
- `#solved`：已经解决的问题
- `#verified`：已经验证成立的判断
- `#dropped`：已经放弃或排除的方向

### 2. `@object`

`@object` 表示这条 note 正在谈谁或什么。

规则：
- `@object` 是开放对象，不是固定词典
- 用户写的是对象名，不是对象类型
- 一条 note 可以关联多个对象
- 对象类型由系统推断或后续编辑确认，不要求用户在 capture 时先选 `person / company / film / contact`
- v1 支持自然对象聚合，但不要求完整对象详情页

例子：
- `@Hitchcock`
- `@RearWindow`
- `@OpenClaw`
- `@Alice`

对象类型属于系统内部元数据，可选包括：
- `person`
- `contact`
- `company`
- `product`
- `book`
- `film`
- `paper`
- `event`
- `place`
- `generic`

### 3. `[[reference]]`

`[[reference]]` 表示这条 note 明确引用另一个 note 或 thread。

规则：
- `[[` 触发引用候选面板
- v1 支持引用 `note` 和 `thread`
- 显示层按标题显示，存储层按稳定 ID 绑定
- 被引用对象改名后，引用关系仍应保持有效
- archived thread 仍可被引用

例子：
- `[[Resume should restart work in 10 seconds]]`
- `[[The default Resume should show only three recovery lines]]`

## Semantics

## Typed Relationships

Threadnote 的基础 discourse graph 使用少量 typed relationships 连接 notes。

### Relationship Set

- `supports`：Evidence 或 Claim 支撑一个 Claim
- `opposes`：Evidence 或 Claim 反驳一个 Claim
- `informs`：提供背景、上下文或来源指向；v1 主要用于 `Evidence -> Source`
- `answers`：Claim 回答一个 Question

### Recommended Pairings

- `Evidence -> Claim`：`supports / opposes`
- `Claim -> Claim`：`supports / opposes`
- `Claim -> Question`：`answers`
- `Evidence -> Source`：`informs`

### Product Rules

- typed relationships 是系统的一等语义，但不要求用户在 capture 时手动声明每条边
- v1 允许系统根据 `#role`、`[[reference]]` 和现有 discourse 关系做推断
- reply 不是 typed relationship；reply 是会话层级，relationship 是推理层级
- `@object` 不构成 discourse edge；它只是对象指向

### `#role` 影响什么

- note 的视觉样式
- Resume 的摘要优先级
- `Settled So Far` 是否吸收这条 note
- 以后可能的筛选、排序与 prepared views
- discourse graph 中的节点类型

### `@object` 影响什么

- 对象聚合
- 对象反向引用
- 以后对象页或对象过滤

`@object` 默认不进入右侧 ledger。

### `[[reference]]` 影响什么

- 显式链接关系
- backlinks
- note 与 thread 之间的可追溯上下文
- 恢复现场时的“从哪里接着看”

`[[reference]]` 是思考引用，不等同于 reply。

## Ledger Rules

右侧 `Settled So Far` 是 notes 的一个稳定语义视图，不是独立对象。

只有以下 role 会进入 ledger：
- `#decided` -> `Decision Made`
- `#solved` -> `Problem Solved`
- `#verified` -> `Verified`
- `#dropped` -> `Dropped`

规则：
- ledger 默认按时间倒序
- 每条 item 显示 note 文本摘要 + 日期
- `#claim`、`#question`、`#evidence`、`#source` 默认不进入 ledger
- `@object` 和 `[[reference]]` 可以附着在 ledger note 上，但不会改变 ledger 类型

## User Flows

### Flow 1: 记录一个决定

输入：
`#decided Default Resume should only show three recovery lines.`

结果：
- note 出现在 notes 区
- 右侧 `Settled So Far` 新增一条 `Decision Made`

### Flow 2: 记录一个对象相关证据

输入：
`#evidence @RearWindow Rear Window traps the viewer in one observation point.`

结果：
- note 出现在 notes 区
- `@RearWindow` 对象聚合该 note
- 不进入 ledger

### Flow 3: 用显式引用推进已有判断

输入：
`#verified [[Suspense often comes from controlled access to information]] This pattern holds across at least three films.`

结果：
- note 出现在 notes 区
- 与被引用 note 建立显式连接
- 右侧 ledger 新增 `Verified`

### Flow 4: 写普通 note 不使用语法

输入：
`Need to compare Rear Window and Psycho more carefully.`

结果：
- 系统默认 role 为 `#note`
- 不进入 ledger
- 不创建对象和显式引用

## Reply vs Reference

- `reply`：表示会话层级上的从属关系
- `[[reference]]`：表示思考层级上的显式引用关系

这两者可以同时存在。

## Edge Cases

- 多个 `#role`：v1 只吃第一个识别到的 role
- 未知 `#tag`：保留原文，不进入语义层
- 多个 `@object`：允许
- 引用目标重命名：保持 ID 绑定，显示文本更新
- 引用目标删除：保留失效引用壳，提示目标不可用
- archived thread：可以被 `[[reference]]` 引用，但不回到左栏 active thread 列表

## Acceptance Criteria

- 用户能只用 `# / @ / [[ ]]` 这 3 套语法表达 note 的最小语义
- `#role` 不再承担对象分类职责
- `@object` 不再污染 ledger 语义
- `Settled So Far` 可以明确解释为“从带稳定 role 的 notes 过滤而来”
- `[[reference]]` 与 reply 的区别对产品团队和实现层都清楚
- 核心 discourse units 与 typed relationships 可以被实现层直接映射为 graph 节点与边

## Cutting Floor

| Feature | Why It Is Tempting | Why We Cut It for v1 |
|---------|--------------------|----------------------|
| 任意自定义 `#tag` | 看起来灵活 | 会把 Threadnote 拉回开放标签系统，破坏恢复语义 |
| capture 时强制选择对象类型 | 数据更整齐 | 概念税过高，打断记录 |
| 让 `@object` 直接进入 ledger | 看起来更完整 | 对象不是稳定推进项 |
| 用 `[[reference]]` 取代 reply | 结构统一 | 会混淆会话关系与思考关系 |
| 让所有 typed relationships 都由用户手动输入 | 关系更精确 | capture 成本过高，应优先由系统推断或轻量确认 |
