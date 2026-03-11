# Plan: Native App Foundation

## Milestone 0: Phase Gate

- 固化 PR/FAQ、spec、plan、task 文档
- 明确本阶段不直接继续在 demo 形态上堆功能
- 锁定四个主目标：正式 app、稳定输入、AI 结构化角色、设计语言重构

## Milestone 1: Formal macOS App Shell

- 创建正式 macOS app target
- 保留现有模型/服务代码，收敛工程边界
- 梳理窗口、命令、快捷键、菜单栏与 quick capture 面板的责任
- 明确 `SwiftUI + AppKit` 混合架构

## Milestone 2: Capture and Editor Reliability

- 重做 capture/editor 输入链
- 让主 capture、thread continue、quick capture 都稳定可输入
- 固化 `#question / #claim / #evidence / #source` 语义标注与补全
- 把输入可靠性当成正式验收项

## Milestone 3: Object Model Clarification

- 明确 `Thread / List / Source / Draft / Entry` 的职责边界
- 梳理来源、资源、问题空间三者关系
- 收敛 thread 表面复杂度，复杂关系退到后台

## Milestone 4: AI Role and Integration Layer

- 定义 AI 在产品中的 4 层角色：
  - Input Assistant
  - Structure Assistant
  - Draft Assistant
  - Librarian
- 设计 `AIProvider` 抽象与本地/云模型双通道
- 优先接入分类、结构化、resume，而非代写

## Milestone 5: Craft-like Design Language

- 统一 `Sidebar / Canvas / Inspector` 结构
- 设计线程、资源、来源、草稿的视觉对象层级
- 收口当前过重的 dashboard/card stack 风格

## Exit Criteria

- 工程可作为正式 `.app` 启动并继续迭代
- capture 输入稳定可用
- 线程与资源区的边界清晰
- AI 接口与角色已文档化
- 新设计语言在主路径上落地
