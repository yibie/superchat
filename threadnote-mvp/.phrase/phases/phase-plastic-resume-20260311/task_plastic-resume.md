# Tasks: Plastic Resume

## Phase Status

- 当前阶段状态: active
- 当前开放顶层任务: task001

task001 [doing] 场景:让 thread resume 围绕 goal layer 自动塑形 | Given:当前 thread 只有固定摘要式 resume，无法适配 build/study/research 三类目标 | When:为 thread 增加 goal layer、goal type suggestion 与组件驱动 resume | Then:用户重新进入 thread 时能快速恢复工作状态而不是阅读通用摘要 | 验证:人工场景审查 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` | 备注:实现与构建已完成，待人工场景审查收口

## task001 breakdown

- task001-01 [done] 为 `ThreadRecord` 增加 `goal layer` 并保证旧 snapshot 向后兼容
- task001-02 [done] 在 AI 协议层增加 `goal type suggestion` 与组件化 `resume synthesis`
- task001-03 [done] 在创建 thread 时要求填写 goal statement / type / success condition
- task001-04 [done] 将 `ResumeCard` 改为组件驱动渲染，稳定保留 3 个核心块
- task001-05 [done] 在 thread inspector 显示 goal context，并允许手动修正
- task001-06 [doing] 以 `Build / Study / Research` 三类场景做人工审查并回写文档
