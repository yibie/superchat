# Tasks: Native App Foundation

## Phase Status

- 当前阶段状态: all scoped tasks done
- 当前开放顶层任务: none
- 当前开放子任务: none
- 本阶段到此为止，不再默认追加“下一步实现”

## Phase Boundary

- 本阶段已完成范围:
  - 正式 macOS app 工程壳与构建链
  - capture/editor 输入可靠性与显式 tag 输入
  - thread/list/source 职责边界收敛
  - AI role / provider boundary / heuristic runtime 接线
  - Craft-like workspace 三栏结构与主路径视觉收口
- 本阶段明确不包含:
  - 真实云模型 provider 接入
  - `discourseAnalysis` 生产级实现
  - 新 phase 的功能扩张或额外 AI 能力探索
- 后续若继续开发，必须新建 task 或新 phase，再开始实现

task001 [done] 场景:将当前 demo 升级为正式 macOS app 工程 | Given:项目仍是 SwiftPM executable + 轻量 app 壳 | When:建立正式 app target 并梳理窗口/命令/菜单/面板边界 | Then:Threadnote 可以作为标准 `.app` 构建与启动 | 验证:`xcodegen generate` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build` + `open .xcodebuild/DerivedData/Build/Products/Debug/Threadnote.app`
task002 [done] 场景:修复 capture 输入链的可靠性 | Given:当前 capture 输入框仍存在焦点/输入异常 | When:将主 capture、thread continue、quick capture 统一到可靠的 AppKit-backed editor | Then:三个输入入口都可稳定输入、提交与切换焦点 | 验证:手动输入测试 + 中文输入法测试 + quick capture 焦点测试
task003 [done] 场景:让 capture 类型可显式输入 | Given:用户需要快速声明 capture 的语义类型 | When:实现 `#question / #claim / #evidence / #source` 标注与输入内补全 | Then:显式 tag 覆盖自动推断且 UI 可稳定操作 | 验证:手动键盘测试 + 提交后 kind 检查
task004 [done] 场景:重新定义 thread/list/source 的职责边界 | Given:当前 thread 承担过多职责且 list/resource 语义仍不稳 | When:完成对象边界文档与对应 UI 主路径收敛 | Then:thread 只负责问题现场，list 只负责资源视角，source 成为一等资源对象 | 验证:设计审查（截图） + 文档对齐检查 + 构建通过
task005 [done] 场景:定义 AI 在产品中的角色与集成边界 | Given:当前尚未明确 AI 的职责和 provider strategy | When:产出 AI role 与 integration strategy 文档并建立代码接口骨架 | Then:AI 只作为结构化助手进入主流程，provider 可替换 | 验证:文档审查 + 接口编译通过
task006 [done] 场景:把主界面设计语言收敛到 Craft-like workspace | Given:当前 UI 仍带有 demo/dashboard 感 | When:重构主界面的 sidebar/canvas/inspector 结构和核心组件层级 | Then:主路径更平静、更直接、更像原生工作区 | 验证:人工设计审查 + 主流程截图对比 + 构建通过

## task004 breakdown

task004-01 [done] 场景:让 source 从普通 entry 中被明确拉出来 | Given:source 仍只在 entry 组件里顺带出现 | When:为 source 建立独立打开路径与详情面板 | Then:source 成为一等资源对象而不是附属字段 | 验证:stream/thread/list 均可打开 source detail
task004-02 [done] 场景:让 thread 主页面回到问题现场 | Given:resume 内联过多 claims/evidence/source 细节 | When:将资源浏览下沉到 `Thread Resources` 次级路径 | Then:thread 主路径保持 `Resume -> Conversation Stream -> Continue` | 验证:thread 页面人工审查
task004-03 [done] 场景:让 thread 资源可以一键进入 list | Given:资源只能逐条右键加入 list | When:在 `Thread Resources` 中提供批量收纳入口 | Then:list 真正成为 thread 的资源视角下游 | 验证:从 thread resources 收纳后跳转到目标 list
task004-04 [done] 场景:让 list 更像资源区而不是分类清单 | Given:list detail 只是按类型分 section 纵向堆叠 | When:重排为 `Thread Focus + Resource Shelf` | Then:list 更像资源工作区 | 验证:list 页面人工审查
task004-05 [done] 场景:让 note/source 在 list 里读起来就不一样 | Given:资源卡片视觉语气过于统一 | When:分化 note/source 卡片内容层级和视觉语气 | Then:source 更像引用资源，note 更像思考碎片 | 验证:list 页面人工审查
task004-06 [done] 场景:让 list 至少具备一个真实的资源视角操作 | Given:list 仍缺最小排序能力 | When:为 list 增加 `saved/recent/active` 排序 | Then:list 不只是静态资源架，而是可按视角组织资源 | 验证:手动切换排序并检查 thread/note/source 顺序变化
task004-07 [done] 场景:让 list 有最小焦点管理能力 | Given:resource shelf 还缺 pin/focus 一类轻量操作 | When:为 list 引入最小 pin 或 focus 能力 | Then:list 更接近 Arc 式资源区而不是只读陈列架 | 验证:手动 pin/focus 操作（截图审查）
task004-08 [done] 场景:结束 task004 | Given:thread/list/source 边界已基本落地 | When:做一次设计审查并修最后的概念外露 | Then:task004 可从 doing 进入 done | 验证:设计审查 + 文档回写 + 构建通过

## task004 remaining checklist

- task004-07.1 [done] 数据层补 `ListItem.isPinned`，并保证旧快照兼容
- task004-07.2 [done] store 补 `togglePinned(_:)`
- task004-07.3 [done] list 视图将 pinned 项从普通 shelf/focus 中分离
- task004-07.4 [done] thread/note/source 卡片都提供 pin/unpin 入口
- task004-07.5 [done] 手动验证 pin 后的层级、排序与去重行为（基于最新截图审查：pinned 项已从普通 shelf/focus 中提升并保持去重）
- task004-08.1 [done] 审查 thread 页是否仍有资源浏览外露
- task004-08.2 [done] 审查 list 页是否还像分类清单而不是资源视角
- task004-08.3 [done] 回写 change/task 文档并关闭 task004 的实现阶段

## task004 closure notes

- task004-close.1 [done] thread 主路径保持 `Resume -> Conversation Stream -> Continue`，资源浏览停留在 inspector/sheet 次级路径
- task004-close.2 [done] list 保持 `Pinned / Thread Focus / Resource Shelf` 资源视角，不再回退成 typed section dump
- task004-close.3 [done] source 继续作为独立资源对象存在，可从 stream/thread/list 打开 dedicated detail

## task005 deliverables

- task005-01 [done] 在 `tech-refer` 中明确 AI role、workflow mapping、provider priority 与 hard rule
- task005-02 [done] 新增 `adr_ai-integration-boundary.md`，固定“先协议层、后 provider”决策
- task005-03 [done] 建立 `AIIntegration.swift`，定义 `AIRole / AIWorkflow / ThreadnoteAIProvider / AIRequest / AIResponse`
- task005-04 [done] 在 `ThreadnoteStore` 挂接默认 `ThreadnoteAIConfiguration`，为后续 provider 接入预留稳定边界
- task005-05 [done] 将 `suggestedThreads / resume / prepareView` 现有启发式改为先走 `ThreadnoteAIRuntime` + `HeuristicAIProvider`，并保留 `Store` fallback 防止行为回退
- task005-close.1 [done] 当前 phase 内 AI 集成范围到 `capture classification / thread suggestion / resume synthesis / draft preparation / list curation` 为止
- task005-close.2 [done] `discourseAnalysis`、真实 provider 接入、远程推理链路明确留到后续新 task / 新 phase

## task006 breakdown

task006-01 [done] 场景:让工作区拥有持久导航而不是临时返回链路 | Given:thread/list 仍通过 header 里的 `Stream` 返回，页面像几个孤立场景 | When:把 sidebar 提升成全局导航，并让 stream/thread/list 共用同一 canvas 容器 | Then:工作区更像一个连续的 Mac app，而不是来回切页的原型 | 验证:人工导航审查 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`
task006-02 [done] 场景:让 header 不再承担导航补丁 | Given:header 同时承担标题、返回和过多控制 | When:将 header 收敛成页面标题 + 当前上下文动作 | Then:顶部更平静，主操作更清楚 | 验证:人工审查 + 截图对比
task006-03 [done] 场景:让 canvas 更像 Craft 式工作区而不是卡片堆栈 | Given:stream/list/thread 画面仍有较强 dashboard 感 | When:收敛 section 密度、空白与卡片层级 | Then:主阅读区更稳定、更像文档工作面 | 验证:人工设计审查（截图）+ `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`
task006-04 [done] 场景:给 inspector 留出清晰边界 | Given:thread resources、draft、source detail 还在分散生长 | When:明确哪些能力应该进入 inspector/sheet，哪些应该留在 canvas | Then:信息和动作边界更稳定 | 验证:设计审查（截图）+ 文档回写 + 构建通过

## task006 progress notes

- task006-04.1 [done] 工作区改为 `Sidebar / Canvas / Inspector` 三栏常驻结构
- task006-04.2 [done] 线程上下文动作（resources / timeline / prepare）迁移到 inspector
- task006-04.3 [done] 边界定稿：thread actions/support pack 进 inspector；thread resources/source detail 保持 sheet；continue draft 保持 canvas

## task006-03 progress notes

- task006-03.1 [done] stream/thread/list 主内容统一到固定宽度列，减少“满宽卡片墙”观感
- task006-03.2 [done] section 卡片与条目卡片改为低噪音文档表皮（浅底 + 细边框）
- task006-03.3 [done] list 顶部资源概览从三张指标卡收敛为一条轻量信息条
- task006-03.4 [done] 人工截图对比完成，并修正两项暴露问题（stream 输入区高度、resume 文案路径）
