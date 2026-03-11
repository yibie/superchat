# Spec: Native App Foundation

## Purpose

本阶段将 `Threadnote` 从 demo 级原型推进为可持续开发的原生 macOS 应用基础。核心目标不是增加更多 feature，而是解决当前基础层的不可靠与概念外露问题。

## Phase Goal

构建一个正式的 macOS 工作区基础，让用户能够：

1. 稳定地输入 capture，并明确标注 capture 语义。
2. 在 Thread 中直观看到“当前问题到了哪里”，并继续推进。
3. 用 List 作为资源视角组织线程、笔记与来源。
4. 在 AI 帮助下完成结构化，而不是被 AI 代写。

## In Scope

- 正式 macOS app 工程结构
- AppKit-backed editor / capture 输入链
- `Thread / List / Source / Draft` 的职责边界
- `question / claim / evidence / source` capture 语义
- AI 角色定义与 provider abstraction
- Craft-like 设计语言与布局体系

## Out of Scope

- 实时协作
- 跨设备同步
- 通用数据库式条目管理
- 大规模知识图谱可视化
- AI 自动长文写作
- Web/iOS 客户端

## Core Product Principles

- Thread 是问题空间，不是文件夹。
- List 是资源视角，不是状态容器。
- AI 负责结构化与恢复现场，不替代作者。
- Capture 优先级高于管理；继续优先级高于归档。
- 默认界面必须简约，复杂关系退到后台。

## User Scenarios

### Scenario 1: Reliable capture

用户在首页或浮动 capture 面板输入一句话、一个链接、或一条来源。输入框稳定可用，支持显式 `#question / #claim / #evidence / #source` 标注，提交后内容立刻进入系统。

### Scenario 2: Return to a thread

用户打开一个 thread，不需要看仪表盘，而是直接看到当前判断、未解问题、最近动态和继续输入区。

### Scenario 3: Organize resources

用户把 thread、entry、source 加入一个 list，用某个资源视角集中查看，而不把它们变成另一个问题空间。

### Scenario 4: Use AI without losing control

用户看到的是 AI 提供的分类、归并、恢复、关系提示；用户不会被推着进入“AI 代写”流程。

## Object Boundaries

### Thread

- 表达一个持续推进的问题空间
- 拥有状态、resume、open loops、next action
- 接收 capture 并组织 conversation stream

### List

- 表达一个资源视角
- 可收纳 thread、entry、source
- 不拥有 claim、anchor、current state

### Source

- 表达外部来源对象
- 可以由 URL、文档、图片、书籍等载体组成
- 既能在 thread 中作为来源出现，也能在 list 中作为资源出现

### Draft

- 表达从 thread 派生出的任务视图
- 不是产品一级对象，不常驻

### Entry

- 继续作为底层原子记录
- UI 中默认叫 note/capture，不直接暴露 entry 术语

## Architecture Requirements

- 工程形态必须从 SwiftPM demo 可执行程序升级为正式 macOS app target。
- 编辑器与焦点管理必须允许 AppKit 级控制。
- UI 可继续使用 SwiftUI，但关键输入与窗口行为不能继续依赖 demo 级拼接。
- AI 接入必须先经过应用内协议层，不能直接把产品绑死在某个 provider 框架上。

## Design Requirements

- 信息架构遵循 `Sidebar / Canvas / Inspector` 的清晰分层。
- 默认页面不使用“控制台式仪表盘”语言。
- 视觉层级向 Craft 靠齐：低噪音、卡片/文档优先、稳定导航。
- 需要高频输入的区域必须优先交互可靠性，再讨论视觉细节。

## Phase Success Criteria

- capture 在主工作区与 quick capture 中都稳定可输入
- thread 页被收敛成 `Resume + Stream + Continue`
- list 明确成为资源视角，而不是 thread 替身
- AI 角色文档与集成接口定稿
- 工程结构允许继续进入正式开发，而不是继续在 demo 外壳上堆叠
