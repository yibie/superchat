# Tasks: Resume Recovery

## Phase Status

- 当前阶段状态: active
- 当前开放顶层任务: task001 task002 task003

task001 [doing] 场景:将默认 Resume 收敛为可塑化三行恢复入口 | Given:当前 Resume 仍像结构化面板，用户重开 thread 时信息负担过大 | When:把默认 Resume 改为按 goal type 塑形的 3 行恢复槽位 | Then:用户能快速知道当前状态、关键缺口和下一步动作 | 验证:人工场景审查 + 主流程截图对比 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` | 备注:实现与构建已完成，待人工主流程审查
task002 [doing] 场景:为 Resume 建立“已解决问题”边界 | Given:用户需要确认哪些内容已 settled，但又不能回到信息墙模式 | When:将 stable memory 收敛为二级路径并限定其内容，而不是默认塞进 thread 主页面 | Then:用户可以按需确认已 settled 的内容，而默认态仍保持极简 | 验证:人工场景审查 + 文案审查 + `swift build` | 备注:已接入第一版 stable memory 边界，仍需继续收紧内容边界

task003 [doing] 场景:将 thread 页面收敛回单栏恢复工作面，并统一 Resources 定义 | Given:默认 Resume 已收敛为三行，但 thread 页面被额外 rail/tool 改造打断了自上而下扫描路径，且 list/resource 语义仍然分裂 | When:保留 `Restart Note / Continue / Working Stream` 的单栏流，将 `Resources / Timeline` 收进按需弹出的右侧 inspector，让它们通过外露 dock tabs 进入，只把 `Thread Memory` 保留在 thread 标题上方，并在 inspector 收起时保留外露 tabs | Then:用户先回到现场继续工作，需要时再通过上方工具和右侧 inspector 浏览二级信息，同时不会因为入口完全消失或入口重复而打乱心智模型 | 验证:人工场景审查 + 主流程截图对比 + `swift test` + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` | 备注:删除底层 list 主动维护 API，不保留手动收纳能力

task004 [done] 场景:开发时日志系统初始化超时不再反复干扰调试 | Given:Xcode 在运行共享 scheme 时出现 `Failed to initialize logging system due to time out` 警告 | When:为共享 scheme 配置 `IDEPreferLogStreaming=YES` | Then:Run/Test/Profile 都使用 log streaming 偏好，减少 IDE 侧日志初始化超时警告 | 验证:检查 `Threadnote.xcscheme` 环境变量 + `xcodegen generate` + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`

## task001 breakdown

- task001-01 [done] 定义 `Build / Study / Research` 的三行恢复语义
- task001-02 [done] 定义三行生成与降级规则
- task001-03 [done] 收敛默认 Resume 的可见信息，只保留三行
- task001-04 [done] 将 goal 元信息降级出默认主视图

## task002 breakdown

- task002-01 [done] 定义 stable memory 允许出现的内容类型
- task002-02 [doing] 定义默认不允许进入 Resume 的信息类型
- task002-03 [done] 设计用户可控展开路径，不回退成历史/资源墙

## task003 breakdown

- task003-01 [done] 将 thread 页面恢复为单栏文档流
- task003-02 [done] 保留 `Restart Note / Continue / Working Stream` 的自上而下恢复路径
- task003-03 [done] 将 `Resources / Timeline / Thread Memory` 收敛为标题上方的二级动作
- task003-04 [done] 将 sidebar 的二级入口改为全局 `Resources`，汇总所有 thread 资源
- task003-05 [done] 保持 `@` 为 object mention 语义，不将其改写为 note 引用解析
- task003-06 [done] 删除底层 `List` 主动维护 API，不再保留 `create/add/remove/pin` 语义
- task003-07 [doing] 进行人工场景审查，确认页面首先让用户回到现场而不是进入阅读模式
- task003-08 [done] 将资源聚合从 attachment/object-note 收敛为 `Links / Media / Mentions`
- task003-09 [done] 将 thread 内 `Resources / Timeline` 从独立 sheet 收敛为按需弹出的右侧 inspector
- task003-10 [done] 移除 thread 主页面中的 `Settled So Far` section，仅保留 `Thread Memory` 作为 settled 记忆入口
- task003-11 [done] 将 thread 顶部二级动作改成容器级 chrome 工具带，而不是正文里的按钮组
- task003-12 [done] 让右侧 inspector 在收起时仍保留外露 tabs，提升二级工具的可发现性
- task003-13 [done] 将 inspector 内的 `Resources / Timeline` 改成更偏 distill/workbench 的卡片层次，而不是简单列表
- task003-14 [done] 移除主工作面里重复的 `Resources / Timeline` 按钮，只保留右侧 dock tabs 作为该二级工具入口

## task004 breakdown

- task004-01 [done] 在 `project.yml` 的共享 scheme 上声明 `IDEPreferLogStreaming=YES`
- task004-02 [done] 重新生成 `Threadnote.xcodeproj` 并确认 `Threadnote.xcscheme` 写入环境变量
- task004-03 [done] 重新运行构建，确认项目配置未回归
