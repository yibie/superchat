# Spec: Plastic Resume

## Purpose

将 `Resume` 从固定摘要卡升级为围绕 thread `goal layer` 自动塑形的工作恢复面板。

## Phase Goal

让用户在重新打开一个 thread 时，能围绕明确目标快速回答四个问题：

1. 我在做什么
2. 现在到哪了
3. 还缺什么
4. 下一步最值得做什么

## In Scope

- 在 `Thread` 中加入内嵌 `goal layer`
- 创建 thread 时收集 `goal statement / goal type / success condition / current stage`
- 通过 AI 建议 `goal type`
- 将 `Resume` 改为组件驱动的塑形面板
- 支持 `Build / Study / Research` 三种目标类型

## Out of Scope

- 新增独立 `Goal` 一级对象
- AI 自由生成全新 UI 组件
- 真实云模型 provider 接入
- 多 goal thread
- goal 的复杂项目管理字段

## Product Principles

- `Thread` 仍是主对象；`goal` 只是 thread 的定向层。
- 顶层意图由用户确认，不能由 AI 代替。
- `Resume` 必须先恢复工作状态，再考虑信息完整性。
- 可塑化不等于任意生成；组件库必须稳定。

## Success Criteria

- 新 thread 必须显式带有 goal layer
- 旧 thread 可以向后兼容显示默认 goal layer
- `Resume` 始终保留 `Goal Focus / Current State / Next Best Move`
- 不同 `goal type` 的 `Resume` 组合明显不同
- 主流程构建通过，且 UI 审查确认不退化为 AI dashboard
