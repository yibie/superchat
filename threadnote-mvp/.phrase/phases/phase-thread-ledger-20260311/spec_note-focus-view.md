# Spec: Note Focus View

## Summary

Threadnote 需要一个二级 note 视图，用来展示单条 note 在 discourse graph 里的位置。

它不是默认 thread 首页，也不是完整 timeline，而是一个围绕单条 note 展开的 argument view。

一句话定义：

`Note Focus View helps someone inspect one note as a reasoning unit: what it answers, what supports it, what opposes it, and what informs it.`

## Goals

- 让用户查看单条 note 时，能直接看到它在推理结构中的位置
- 让 typed relationships 变得可见，而不是只存在于后台推断里
- 为 `Question / Claim / Evidence / Source` 提供更强的结构化阅读入口
- 不把这种高密度结构直接塞进 thread 首页

## Non-goals

- 不把 thread 主页面改成 discourse graph 浏览器
- 不做完整图谱画布或节点拖拽
- 不暴露内部 ID、property key 等工程信息
- 不要求所有 note 默认展开所有关系

## Placement

Note Focus View 是 notes 区的二级视图。

进入方式：
- 从 notes 列表点击一条 note
- 从 `[[reference]]` 点击被引用 note
- 从 ledger item 跳到对应 note

退出方式：
- 返回 thread 主页面
- 关闭 sheet / panel

## Core Layout

### 1. Note Header

显示：
- note role（Question / Claim / Evidence / Source）
- note 主文本
- 创建时间
- 关联对象（`@object`）

不显示：
- 内部 ID
- 原始 parser 元数据

### 2. Relationship Blocks

按语义显示少量固定关系区块：
- `Answers`
- `Supported By`
- `Opposed By`
- `Informed By`

规则：
- 只显示当前 note 实际拥有的关系区块
- 没有对应关系时不显示空块
- 区块顺序固定，减少理解成本

### 3. Related Replies

如果当前 note 有 reply，会在 focus view 中作为会话层内容显示。

规则：
- reply 单独归类
- reply 不与 typed relationships 混为一谈

## Relationship Semantics

### `Answers`

当前 note 回答了哪些 `Question`。

典型类型：
- `Claim -> Question`

### `Supported By`

有哪些 `Evidence` 或 `Claim` 在支撑当前 note。

典型类型：
- `Evidence -> Claim`
- `Claim -> Claim`

### `Opposed By`

有哪些 `Evidence` 或 `Claim` 在反驳当前 note。

典型类型：
- `Evidence -> Claim`
- `Claim -> Claim`

### `Informed By`

有哪些 `Source` 或上下文引用在为当前 note 提供出处或背景。

典型类型：
- `Evidence -> Source`
- `Claim -> Source`（如果实现层允许）

## Product Rules

- Note Focus View 展示 typed relationships，但不要求用户手动维护整张 graph
- 默认 thread 首页保持恢复导向，不展示完整关系树
- Note Focus View 是“深入理解一条 note”的地方，不是“回到现场”的第一屏
- 关系词必须是自然语言：`Answers / Supported By / Opposed By / Informed By`

## Data Dependencies

该视图依赖：
- note 的 `#role`
- typed relationships：`supports / opposes / informs / answers`
- `@object` 关联
- `[[reference]]` 显式引用
- reply 层级

## Acceptance Criteria

- 用户从 notes 区进入一条 note 后，能看懂它在推理结构中的位置
- `Question / Claim / Evidence / Source` 在该视图中的语义差异是明确的
- typed relationships 对用户可见，但不会把 thread 首页重新做成结构信息墙
- reply 与 relationships 在该视图中不混淆

## Cutting Floor

| Feature | Why It Is Tempting | Why We Cut It for v1 |
|---------|--------------------|----------------------|
| 节点拖拽图谱画布 | 看起来强大 | 会把产品重心拉向图谱工具 |
| 默认首页展示关系树 | 信息更完整 | 会破坏恢复首页的简洁性 |
| 暴露内部 note id / graph id | 方便调试 | 对用户没有意义，只会增加噪音 |
| 让 reply 和 graph edge 共用同一块 UI | 看起来统一 | 会混淆会话关系与推理关系 |
