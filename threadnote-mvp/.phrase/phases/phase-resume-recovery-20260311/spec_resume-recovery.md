# Spec: Resume Recovery

## Purpose

将 `Resume` 从“结构化信息面板”进一步收敛为“最小工作恢复包”。

## Phase Goal

让用户重新打开一个 thread 时，在 10-30 秒内完成恢复，而不是进入阅读任务。

## In Scope

- 将默认 Resume 收敛为 3 行可塑化恢复信息
- 为 `Build / Study / Research` 定义各自的 3 行语义
- 引入统一的 settled memory 边界，并保留二级查看路径
- 将 `Goal` 元信息从主视图降级到轻量层或 inspector
- 明确哪些信息绝对不能进入默认 Resume
- 将 thread 页面收敛为单栏恢复工作面，二级工具通过弹层按需打开
- 将 `Thread Memory` 收敛为 thread 标题上方的容器级工具，而不是正文 section 内按钮
- 将 sidebar 中的全局二级入口定义为 `Resources`，它汇总所有 thread 的资源
- 将 thread 内 `Resources` 与全局 `Resources` 统一为同一套派生资源定义
- 将 `Resources` 收敛为 `Links / Media / Mentions` 三类派生资源
- 将 thread 内的 `Resources / Timeline` 收进按需弹出的右侧 inspector，而不是固定 rail 或独立 sheet
- 让右侧 inspector 在收起时仍保留外露 tabs，保持二级工具可发现性，并避免在主工作面重复渲染 `Resources / Timeline`

## Out of Scope

- 新增 goal type
- 重新设计 stream / list / source 体系
- AI provider 扩展
- timeline / resources 的深层交互重构

## Product Principles

- Resume 不是问题档案；Resume 是重新开工的点火器。
- 默认态只显示最少可行动信息。
- 复杂度必须隐藏在简洁之后，而不是直接展示。
- “已解决问题”是用户控制信息量的边界，但不必默认占据 thread 主页面。
- 二级工具入口属于容器 chrome，不属于正文 section。
- 同一语义入口不要在主工作面和 inspector 上重复出现。
- `@` 继续表示 object mention，不退化成 note 引用语法。
- `Resources` 不是 note/thread 容器，而是从 thread 中派生出的 `Links / Media / Mentions` 资源层。
- 右侧 inspector 只承载按需查看的二级工具，不抢占 thread 主恢复路径。

## Success Criteria

- 默认 Resume 只显示 3 行
- 这 3 行会按 goal type 塑形，而不是统一模板
- stable memory 通过二级路径保留，但不默认占据 thread 主页面
- `Resources` 在全局和 thread 内都按 `Links / Media / Mentions` 呈现
- `Resources / Timeline` 通过按需弹出的右侧 inspector 打开，而不是固定显示或在主工作面重复出现
- inspector 在收起时仍保留可点击的外露 tabs，而不是完全消失
- 用户不需要阅读大段结构化内容就能继续 thread
- thread 页面读起来更像 restart workbench，而不是 summary/dashboard
