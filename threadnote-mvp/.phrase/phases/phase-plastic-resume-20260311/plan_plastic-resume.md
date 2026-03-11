# Plan: Plastic Resume

## Milestone 0: Phase Gate

- 固化 spec / plan / task / change 文档
- 锁定本阶段只做 `goal layer + plastic resume + creation flow`

## Milestone 1: Goal Layer

- 为 `ThreadRecord` 增加 `goal layer`
- 保证旧 snapshot 解码兼容
- 增加 thread goal 的创建与编辑入口

## Milestone 2: AI Suggestion + Resume Synthesis

- 增加 `goal type` suggestion workflow
- 扩展 `resume synthesis` 输出为组件集合
- 保持 AI 只做建议、选配、排序与填充

## Milestone 3: UI Integration

- 在创建 thread 时收集 goal 数据
- 在 thread inspector 暴露 goal context
- 将 `ResumeCard` 改为组件驱动渲染

## Exit Criteria

- 新旧 thread 都能进入新的 plastic resume 路径
- `Build / Study / Research` 具备稳定差异化面板
- 构建通过且 phase 文档回写完成
