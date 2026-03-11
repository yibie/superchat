# Tasks: Resume Recovery

## Phase Status

- 当前阶段状态: active
- 当前开放顶层任务: task001 task002 task003

task001 [doing] 场景:将默认 Resume 收敛为可塑化三行恢复入口 | Given:当前 Resume 仍像结构化面板，用户重开 thread 时信息负担过大 | When:把默认 Resume 改为按 goal type 塑形的 3 行恢复槽位 | Then:用户能快速知道当前状态、关键缺口和下一步动作 | 验证:人工场景审查 + 主流程截图对比 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` | 备注:实现与构建已完成，待人工主流程审查
task002 [doing] 场景:为 Resume 建立“已解决问题”边界 | Given:用户需要确认哪些内容已 settled，但又不能回到信息墙模式 | When:引入统一的 `Resolved So Far` 二级层并限定其内容 | Then:用户可以主动展开已解决问题，而默认态仍保持极简 | 验证:人工场景审查 + 文案审查 + `swift build` | 备注:已接入第一版 `Resolved So Far`，仍需继续收紧内容边界

task003 [doing] 场景:将 thread 页面重构为恢复工作台 | Given:默认 Resume 已收敛为三行，但 thread 主页面仍然像摘要卡片与 conversation stream 组合 | When:将页面拆成 `Memory Rail + Working Surface + Tools` 三层 | Then:用户先看到 restart note 与当前工作段，再按需查看已解决问题和工具层 | 验证:人工场景审查 + 主流程截图对比 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` | 备注:结构实现与构建已完成，待人工主流程审查

task004 [done] 场景:开发时日志系统初始化超时不再反复干扰调试 | Given:Xcode 在运行共享 scheme 时出现 `Failed to initialize logging system due to time out` 警告 | When:为共享 scheme 配置 `IDEPreferLogStreaming=YES` | Then:Run/Test/Profile 都使用 log streaming 偏好，减少 IDE 侧日志初始化超时警告 | 验证:检查 `Threadnote.xcscheme` 环境变量 + `xcodegen generate` + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`

## task001 breakdown

- task001-01 [done] 定义 `Build / Study / Research` 的三行恢复语义
- task001-02 [done] 定义三行生成与降级规则
- task001-03 [done] 收敛默认 Resume 的可见信息，只保留三行
- task001-04 [done] 将 goal 元信息降级出默认主视图

## task002 breakdown

- task002-01 [done] 定义 `Resolved So Far` 允许出现的内容类型
- task002-02 [doing] 定义默认不允许进入 Resume 的信息类型
- task002-03 [done] 设计用户可控展开路径，不回退成历史/资源墙

## task003 breakdown

- task003-01 [done] 将 thread 页面拆成 `Memory Rail + Working Surface` 主结构
- task003-02 [done] 左侧引入 `Goal / Resolved / Since You Left / Key Anchors` 记忆侧栏
- task003-03 [done] 中间收敛为 `Restart Note / Current Working Segment / Continue` 工作面
- task003-04 [done] 右侧 inspector 收敛为工具层，不再重复 thread 上下文
- task003-05 [doing] 进行人工场景审查，确认页面首先让用户回到现场而不是进入阅读模式

## task004 breakdown

- task004-01 [done] 在 `project.yml` 的共享 scheme 上声明 `IDEPreferLogStreaming=YES`
- task004-02 [done] 重新生成 `Threadnote.xcodeproj` 并确认 `Threadnote.xcscheme` 写入环境变量
- task004-03 [done] 重新运行构建，确认项目配置未回归
