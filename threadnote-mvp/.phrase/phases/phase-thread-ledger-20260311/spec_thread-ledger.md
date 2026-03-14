# Spec: Thread Ledger

## Purpose

将 thread 的稳定推进记录收敛为轻量进展账本，并把 goal setting 简化到只需要确定类别。

## Phase Goal

让 thread 页面始终只回答一件事：这条 thread 哪些问题、判断和决定已经稳定下来；右侧 inspector 只在按需打开时承载二级工具，不固定占位。

## In Scope

- 将 thread 创建与编辑时的 goal setting 简化为 `title + goal type`
- 让 thread 标题承担原本的 `goal statement`
- 将稳定推进记录收敛为 `Settled So Far` 语义层，而不是 thread 主页面里的固定 section
- 将 `Resolved / Since You Left / Key Anchors` 合并成统一 ledger
- 为 ledger 定义 4 类稳定项：`Problem Solved / Decision Made / Confirmed / Ruled Out`
- 将 thread tools 从右侧主视野降级
- 允许右侧 inspector 作为按需弹出的二级工具层存在，但不重复 thread 主上下文
- 允许右侧 inspector 在收起时保留外露 tabs，避免二级工具完全失踪
- `Restart Note / Resources` 只在右侧 inspector dock 上出现，不在主工作面重复出现
- 将 thread 状态收敛为 `active / archived`
- 允许 entry row 将本地附件渲染为图片、视频、音频、文档卡片，而不是暴露原始 `attachments/...` 路径
- 允许 entry row 用 badge + 时间戳 + 分隔 action row 的卡片容器表达 note，而不是继续依赖单行内联文本

## Out of Scope

- 新增 goal type
- 引入复杂 thread lifecycle 状态机
- 重做 Resume AI 推断逻辑
- 重做 stream 数据模型
- 引入完整 task/project 管理系统

## Product Principles

- `Settled So Far` 是 progress ledger 语义，不要求它以固定 section 形式常驻。
- thread 标题承载意图，goal type 只负责定类。
- 用户先恢复“哪些已经稳定”，再决定是否展开更多工具。
- tools 是次级能力，不该压过 thread 的推进记录。
- tools 应该挂在容器 chrome 上，而不是混进正文 section。
- 同一工具不要同时占据 chrome 和 dock 两个入口。
- `Restart Note` 属于恢复入口，不该长期压住主编辑区。
- entry card 应该优先暴露 note 的语义与附件预览，不该把工作区相对路径直接泄露给用户。
- `@object / [[reference]]` 是 note 语义的一部分，capture 时丢掉就是数据模型坏了，不是渲染问题。

## Success Criteria

- 新建或编辑 thread 时，用户不再填写独立 goal statement
- 默认主路径不再强制显示 `Settled So Far` section；稳定记忆走二级入口
- 右侧 inspector 按需打开，不固定显示
- 右侧 inspector 收起后仍保留可发现的 tabs
- `Restart Note / Resources` 不再在 thread 标题上方或主工作面重复渲染
- ledger 条目能用统一类型表达已解决问题、已作决定、已确认事实和已排除方向
- `Since You Left` 和 `Key Anchors` 不再以独立卡片占据主视图或固定右栏
- 主工作面默认只保留 `Continue` 与 `Working Stream`
- 本地附件 entry 会直接渲染对应预览卡片，不再显示裸 `attachments/...` 文本
- capture 生成的 entry 能稳定带上 `objectMentions` 与 `references`，供 Resources 与后续图谱使用
