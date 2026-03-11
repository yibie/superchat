# Spec: Thread Ledger

## Purpose

将 thread 右侧栏收敛为轻量进展账本，并把 goal setting 简化到只需要确定类别。

## Phase Goal

让 thread 页面右侧只回答一件事：这条 thread 哪些问题、判断和决定已经稳定下来。

## In Scope

- 将 thread 创建与编辑时的 goal setting 简化为 `title + goal type`
- 让 thread 标题承担原本的 `goal statement`
- 将右侧栏收敛为 `Settled So Far`
- 将 `Resolved / Since You Left / Key Anchors` 合并成统一 ledger
- 为 ledger 定义 4 类稳定项：`Problem Solved / Decision Made / Confirmed / Ruled Out`
- 将 thread tools 从右侧主视野降级
- 将 thread 状态收敛为 `active / archived`

## Out of Scope

- 新增 goal type
- 引入复杂 thread lifecycle 状态机
- 重做 Resume AI 推断逻辑
- 重做 stream 数据模型
- 引入完整 task/project 管理系统

## Product Principles

- 右侧栏是 progress ledger，不是 dashboard。
- thread 标题承载意图，goal type 只负责定类。
- 用户先恢复“哪些已经稳定”，再决定是否展开更多工具。
- tools 是次级能力，不该压过 thread 的推进记录。

## Success Criteria

- 新建或编辑 thread 时，用户不再填写独立 goal statement
- 右侧栏默认只显示 `Settled So Far`
- ledger 条目能用统一类型表达已解决问题、已作决定、已确认事实和已排除方向
- `Since You Left` 和 `Key Anchors` 不再以独立卡片占据右栏
