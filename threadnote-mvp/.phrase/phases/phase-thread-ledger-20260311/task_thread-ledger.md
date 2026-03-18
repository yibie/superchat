# Tasks: Thread Ledger

## Phase Status

- 当前阶段状态: active
- 当前开放顶层任务: none
- 当前开放子任务: none

task001 [x] 场景:用户创建或编辑 thread 时不再被复杂 goal 字段打断 | Given:当前 goal layer 仍要求独立 statement、stage 等字段 | When:将 goal setting 简化为 thread 标题承载意图 + 用户只选择类别 | Then:用户能更快创建和理解 thread，而不会先进入配置流程 | 验证:人工主流程审查 + 文案审查 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`
task002 [x] 场景:用户在 thread 右侧只看到轻量进展账本 | Given:当前右侧栏仍由 tools、goal、resolved、since-you-left、anchors 组成多张卡片 | When:将右侧栏收敛为 `Goal + Settled So Far`，并把 thread tools 降级 | Then:右侧栏更像 progress ledger，而不是 dashboard | 验证:人工场景审查 + 主流程截图对比 + `swift build` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`

## task001 breakdown

- task001-01 [x] 明确 thread 标题如何替代独立 goal statement
- task001-02 [x] 将新建/编辑 thread 流程收敛为只选 goal type
- task001-03 [x] 决定旧 `success condition / stage` 的降级策略

## task002 breakdown

- task002-01 [x] 定义 `Settled So Far` 的 ledger item 类型和排序
- task002-02 [x] 合并 `Resolved / Since You Left / Key Anchors` 为统一右栏结构
- task002-03 [x] 将 tools 从右侧主视野降级，并验证视觉层级


## Progress Notes

- 2026-03-11: 已完成 thread 创建/编辑流程简化；标题承载意图，用户只选择类别。
- 2026-03-11: 已完成右栏收敛为 `Goal + Settled So Far`，并把 tools 挪到中间工作区顶部的紧凑菜单。
- 2026-03-11: 仍待收口两项边界：旧 `success condition / stage` 在长期模型中的最终降级策略，以及 `Settled So Far` 的 item 语义是否要扩成完整 4 类。
- 2026-03-11: 已将 thread 状态收敛为 `active / archived`，左栏默认只呈现 active threads，并提供最小 archive / restore 动作。
- 2026-03-13: 已将 `ThreadnoteStore` 的 repository、routing/runtime 句柄与派生缓存从 Observation 中摘除，避免 thread 页面读取缓存时自触发重绘回环。
- 2026-03-14: 已将 note entry 卡片升级为本地附件可渲染的富媒体卡片，并修复 `@object / [[reference]]` 在 capture 路径中的丢失问题。
- 2026-03-14: 已补阅读态 `@mention / [[reference]]` 高亮、URL-only 文本去重与本地附件缺失占位，并收紧路由 pulse 动画的可见生命周期以降低空转开销。
- 2026-03-14: 已完成 EntryCard 第二轮修复，移除 relation 截断并支持目标 thread 跳转，修复 mixed URL 重复文本、阅读态双击编辑、换行丢失与 mention 子串重复。
- 2026-03-16: Electron 版 reference 已恢复为可读正文语法，稳定 `targetID` 只保存在 `references_json`；capture completion 提交结构化 reference 元数据，resolver/backlink 继续只按 `targetID` 建图，正文显式 relation 改为前缀图标。

task003 [x] 场景:产品团队需要一份正式输入语言规格 | Given:当前 notes、ledger、object tracking 的语义边界仍停留在讨论里 | When:将 `#role / @object / [[reference]]` 写成正式 spec | Then:后续 parser、UI、ledger 和 object aggregation 都有统一事实来源 | 验证:规格文档审查

## task003 breakdown

- task003-01 [x] 定义 `#role` 受控词典与默认行为
- task003-02 [x] 定义 `@object` 的开放对象语义
- task003-03 [x] 定义 `[[reference]]` 的显式引用边界
- task003-04 [x] 定义哪些 role 会进入 `Settled So Far`
- task003-05 [x] 定义核心 discourse units 与 typed relationships


task004 [x] 场景:产品团队需要一份 note 关系视图规格 | Given:typed relationships 已进入输入语言 spec，但还没有对应的查看视图定义 | When:将单条 note 的 focus view 写成正式 spec | Then:后续可在不污染 thread 首页的前提下实现 argument view | 验证:规格文档审查

## task004 breakdown

- task004-01 [x] 定义 Note Focus View 的位置与进入方式
- task004-02 [x] 定义 Answers / Supported By / Opposed By / Informed By 区块
- task004-03 [x] 定义 reply 与 typed relationships 的显示边界

task005 [x] 场景:用户长时间停留在 thread 工作面时应用不会因内部状态回写而持续飙升 CPU 与内存 | Given:thread 页面新增了 entries/resource/threadState 等派生缓存且 store 使用 Observation 驱动 UI | When:将运行时依赖与派生缓存从 Observation 跟踪中移除 | Then:读取缓存不会再次触发渲染回环，thread 页面长时间停留时主线程负载保持稳定 | 验证:`swift test` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath /tmp/threadnote-deriveddata-codex CODE_SIGNING_ALLOWED=NO build`
task006 [x] 场景:用户在 thread 工作面中更容易看到恢复信息与路由反馈 | Given:Restart Note 仍占据主面板且 sidecar 仍保留 Timeline tab，note 卡片正文和引用补全也混入多余信号 | When:将 sidecar 收敛为 `Restart Note + Resources` 并提升未路由/stream 的视觉反馈，同时收紧 `[[reference]]` 候选 | Then:主面板只保留 Continue 与 Working Stream，Restart Note 走右侧恢复入口，卡片正文更干净且引用补全只指向 notes | 验证:人工场景审查 + `swift test` + `xcodebuild -scheme Threadnote build`

## task005 breakdown

- task005-01 [x] 识别 thread 工作面中会在读取时写入 cache 的 store 路径
- task005-02 [x] 将派生缓存与运行时句柄标记为 `@ObservationIgnored`
- task005-03 [x] 验证 SwiftPM 测试与 Xcode app target 构建

## task006 breakdown

- task006-01 [x] 将 `Restart Note` 从 `ThreadDocument` 抽离到右侧 sidecar，并替换 `Timeline` tab
- task006-02 [x] 清理主工作面的重复恢复入口，只保留 `Continue + Working Stream`
- task006-03 [x] 增强未路由 note 与 stream 时间线的视觉权重反馈
- task006-04 [x] 收紧 note 正文与 `[[reference]]` 补全的信号边界

task007 [x] 场景:用户在 thread 时间线中看到附件与对象语义的完整 note 卡片 | Given:当前 capture 路径会丢失 `@object / [[reference]]`，本地附件只以 `attachments/...` 文本显示且 entry 卡片仍是扁平内联样式 | When:修复 capture 数据写入、为本地附件建立 `EntryBody` 分类并将 entry 卡片升级为富媒体预览样式 | Then:Mentions/References 能被资源侧栏收集，PNG/PDF/MP4 等本地附件渲染为对应预览卡片，时间线条目读起来更接近成型卡片而不是原始文本流 | 验证:`swift test --filter URLBodyMigrationTests` + `swift test --filter StoreURLBodyMigrationTests` + `swift test --filter StoreRoutingTests`

## task007 breakdown

- task007-01 [x] 修复 capture 创建 entry 时遗漏的 `objectMentions / references`
- task007-02 [x] 为 `attachments/...` 建立图片/视频/音频/文档 body 分类并补迁移测试
- task007-03 [x] 让 `RichBodyView` 解析 workspace 相对附件路径并渲染本地图片、音频、视频、文档卡片
- task007-04 [x] 将 entry 正文改成卡片布局，隐藏纯附件原始路径并提升 badge / 时间 / action row 层级

task008 [x] 场景:用户在 thread 时间线中阅读和编辑 note 时不会再被关系截断、重复 URL、失效双击和错误 mention 打断 | Given:EntryCard 首轮改版后 RelationBadge、mixed body、阅读态编辑与 mention merge 仍残留边界缺陷 | When:收敛 relation badge 展示与跳转、mixed 文本来源、阅读态 hover/双击行为，以及 migration/mention 去重规则 | Then:关系目标完整可见且可跳转，mixed note 不再重复显示 URL，双击稳定进入编辑，历史换行被保留，`@J.K.罗琳` 不再派生出孤立 `@罗琳` | 验证:`swift test --filter URLBodyMigrationTests` + `swift test --filter CaptureInterpreterTests` + `swift test --filter SharedComponentsTests` + `xcodebuild -project Threadnote.xcodeproj -scheme Threadnote -configuration Debug -derivedDataPath .xcodebuild/DerivedData CODE_SIGNING_ALLOWED=NO build`
task009 [x] 场景:用户在正文中以内联 reference 阅读和跳转，而不会看到暴露的 targetID 或因目标改文而丢失 backlink | Given:reference 一度把 `targetID` 写进正文语法，导致正文分段、显示污染与绑定不稳定的心智负担 | When:恢复 `[[Reference]] / [[supports|Reference]]` 可读语法，并将稳定 target 绑定只保存在 `references_json`，正文显式 relation 改为前缀小图标 | Then:reference 继续以内联文本流显示，source/target 文案统一只取第一行，backlink 始终挂到真实 target 上，目标正文修改后引用关系仍稳定 | 验证:`npm test` + `npm run build:renderer`
task010 [x] 场景:用户在 thread 内查看和修正工作结果状态，而不会把 `decided/solved/verified/dropped` 混进 entry kind 或主卡片 | Given:当前 Electron clean-room 仍缺少结构化 `entry.status` 与 thread 内聚合展示 | When:新增 `entry.status/statusMetadata`、后台状态复判链、手动 `updateEntryStatus` 调用链，并在 `ThreadInspector` 增加 `Status` tab 聚合非 open 条目 | Then:thread 可在 inspector 内聚合展示 Decisions/Solved/Verified/Dropped，用户可在该处手动修正状态，EntryCard 继续只展示 kind | 验证:`node --test tests/clean-room/threadnoteAIService.test.js tests/clean-room/applicationService.test.js` + `npx vitest run tests/renderer/useWorkbench.test.jsx tests/renderer/threadNavigation.test.jsx`
