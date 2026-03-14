# Plan: Thread Ledger

## Milestone 0: Phase Gate

- 初始化 `spec / plan / task / change` 文档
- 明确本阶段是 thread 恢复工作台的收敛迭代
- 跳过 PR/FAQ；原因是这不是新产品或新方向，而是当前 thread 页面信息边界的继续收敛

## Milestone 1: Goal Simplification

- 把 thread 标题收敛为主意图载体
- 将 goal setting 收敛为只选 `Build / Study / Research`
- 明确哪些旧 goal 字段需要降级为系统维护或隐藏字段

## Milestone 2: Settled Ledger Model

- 定义 `Settled So Far` 的统一数据结构
- 为 `Problem Solved / Decision Made / Confirmed / Ruled Out` 建立映射规则
- 决定 ledger 条目的排序、数量上限和日期语义

## Milestone 3: Right Rail Simplification

- 移除右侧栏中的独立 `Since You Left` 和 `Key Anchors` 卡片
- 将 tools 从右侧主视野降级
- 将二级工具收敛为可按需展开的 inspector dock，并在收起时保留外露 tabs
- 避免在主工作面和 dock 上重复渲染同一组工具入口
- 验证二级工具读起来像辅助工作面，而不是小型 dashboard

## Exit Criteria

- 右侧栏不再是多卡片堆叠的信息墙
- 用户能在右侧快速看到当前 goal 类别和已经稳定的推进记录
- thread 创建和编辑流程的概念税明显下降


## Milestone 4: Thread Input Language

- 定义 `#role / @object / [[reference]]` 的最小输入语言
- 明确哪些 note role 会进入 `Settled So Far`
- 明确 reply 与 explicit reference 的边界


## Milestone 5: Note Focus View

- 定义单条 note 的二级 argument 视图
- 约束哪些 typed relationships 在该视图中可见
- 明确 thread 首页与 note 深入视图的边界

## Milestone 6: Runtime Stability

- 收敛 thread 工作面的派生状态缓存，避免读取路径在渲染期间反向触发新一轮 observation
- 验证 thread 长时间停留时不会因内部 cache 写入导致主线程持续重绘

## Milestone 7: Recovery-First Sidecar Cleanup

- 将右侧 sidecar 固定为 `Restart Note + Resources`，默认先打开 `Restart Note`
- 将 `Restart Note` 从主面板移出，避免与 `Continue / Working Stream` 重复争抢主路径
- 提高未路由 note 与 stream 时间线的反馈权重，但不增加新的控制复杂度
- 将 `[[reference]]` 补全限制为 notes，保持输入语言的引用边界清晰

## Milestone 8: Entry Rich Media Cards

- 修复 capture 路径对 `@object / [[reference]]` 的写入遗漏，避免资源聚合与显式引用在入口处丢数据
- 将 `attachments/...` 相对路径提升为正式 `EntryBody`，而不是继续依赖 URL detector 的 scheme 识别
- 让 entry 卡片能直接预览本地图片、音频、视频和文档，避免时间线退化成原始路径列表
- 将 entry row 布局升级为带 badge、时间戳、分隔 action row 的卡片样式，但不引入额外控制面板
