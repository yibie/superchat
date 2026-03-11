# Tech Reference: Native App Foundation

## App Architecture Direction

- UI 编排：SwiftUI
- 编辑器/焦点/窗口关键行为：AppKit
- 工程形态：正式 macOS app target
- 目标：保留当前模型与服务代码，但不继续依赖 demo 级运行壳

## Preferred Stack

### App shell

- Xcode macOS app target 作为主入口
- 现有模型、store、服务可继续保留为本地模块
- 后续可评估将核心逻辑拆分为 package/framework，但不是本阶段前置条件

### Editor

- Capture 与 Continue 优先使用 AppKit-backed editor
- 输入、补全、焦点、快捷键优先考虑可靠性，不强求纯 SwiftUI

### Persistence

- 当前 `snapshot.json` 方案仅作为过渡
- 本阶段先不强行切完整数据库，但要为更稳的存储层留出边界

## AI Integration Direction

### Product role

- Input Assistant：分类、tag 建议、thread 推荐
- Structure Assistant：discourse、resume、claim/evidence/source 关系
- Draft Assistant：从 thread 派生输出视图
- Librarian：来源、资源区、list 的归类和整理

### Provider strategy

- 应用内部定义统一协议，而不是让产品模型直接依赖 provider SDK
- 首选支持：
  - Apple Foundation Models（本地能力）
  - OpenAI（云模型）
  - Anthropic（云模型）
- 默认 provider 优先级：
  - capture 分类 / thread 推荐 / list 整理：`Heuristics -> Apple Foundation Models -> OpenAI`
  - discourse / resume / draft：`Apple Foundation Models -> OpenAI -> Anthropic`

### App protocol boundary

- 统一入口：`ThreadnoteAIProvider`
- 统一请求模型：`AIRequest`
- 统一响应模型：`AIResponse`
- 应用层只按 workflow 调度 provider，不直接在 `Store` / `View` 中引用具体 SDK 类型

### Workflow mapping

- `captureClassification`
  - role: `Input Assistant`
  - job: 判断 `question / claim / evidence / source`
- `threadSuggestion`
  - role: `Input Assistant`
  - job: 给未归档 capture 推荐 thread
- `discourseAnalysis`
  - role: `Structure Assistant`
  - job: 识别 note 之间的 supports / challenges / cites 等关系
- `resumeSynthesis`
  - role: `Structure Assistant`
  - job: 产出 thread 的 current judgment / open loops / next action
- `draftPreparation`
  - role: `Draft Assistant`
  - job: 从 thread 派生 writing / meeting / decision 视图
- `listCuration`
  - role: `Librarian`
  - job: 对 list 中资源做提升、归并与排序建议

### Hard rule

- AI 默认不代写，不成为主工作区的中心对象
- AI 输出必须服务恢复现场与结构化
- provider 失败时允许回退，不允许阻塞 capture / thread / list 主路径
- 主工作区优先显示结果，不显示 provider 品牌或聊天式中间过程

## Design Language Direction

- 对齐 Craft 的优点：低噪音、稳定导航、文档/卡片优先、内容可继续工作
- 不照搬其完整产品模型
- 主工作区优先采用：
  - Sidebar
  - Canvas
  - Inspector

## Open Questions

- 正式 app target 与现有 Swift Package 代码应如何拆边界
- capture editor 是否需要直接采用 `NSTextView` 深度包装
- quick capture 是独立 panel 还是辅助窗口
