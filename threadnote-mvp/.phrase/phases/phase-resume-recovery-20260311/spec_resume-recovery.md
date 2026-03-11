# Spec: Resume Recovery

## Purpose

将 `Resume` 从“结构化信息面板”进一步收敛为“最小工作恢复包”。

## Phase Goal

让用户重新打开一个 thread 时，在 10-30 秒内完成恢复，而不是进入阅读任务。

## In Scope

- 将默认 Resume 收敛为 3 行可塑化恢复信息
- 为 `Build / Study / Research` 定义各自的 3 行语义
- 引入统一的 `Resolved So Far` 二级层
- 将 `Goal` 元信息从主视图降级到轻量层或 inspector
- 明确哪些信息绝对不能进入默认 Resume
- 将 thread 页面收敛为 `Memory Rail + Working Surface + Tools` 的恢复工作台
- 将 `Resolved / Since You Left / Key Anchors` 下沉到记忆侧栏，而不是继续塞进默认 Resume

## Out of Scope

- 新增 goal type
- 重新设计 stream / list / source 体系
- AI provider 扩展
- timeline / resources 的深层交互重构

## Product Principles

- Resume 不是问题档案；Resume 是重新开工的点火器。
- 默认态只显示最少可行动信息。
- 复杂度必须隐藏在简洁之后，而不是直接展示。
- “已解决问题”是用户控制信息量的边界。

## Success Criteria

- 默认 Resume 只显示 3 行
- 这 3 行会按 goal type 塑形，而不是统一模板
- `Resolved So Far` 成为唯一默认展开层
- 用户不需要阅读大段结构化内容就能继续 thread
- thread 页面读起来更像 restart workbench，而不是 summary/dashboard
