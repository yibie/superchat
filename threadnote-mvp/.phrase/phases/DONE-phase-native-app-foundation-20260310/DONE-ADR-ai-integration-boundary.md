# ADR: AI Integration Boundary

## Context

当前 `Threadnote` 已经有一些本地启发式能力，例如 capture 分类、thread 推荐、resume/draft 派生逻辑，但这些能力仍散落在 `Store` 语义里，还没有一个稳定的 AI 协议层。

如果后续直接把产品模型绑定到某个 provider SDK，会带来三个直接问题：

- provider 切换会穿透 `Store` 与 UI
- 本地模型与云模型无法按 workflow 独立切换
- AI 失败路径会污染 capture / thread / list 主流程

## Decision

在应用内部先定义一层稳定协议，再决定接哪个 provider。

- 用 `AIRole` 固化 4 类产品角色：
  - `Input Assistant`
  - `Structure Assistant`
  - `Draft Assistant`
  - `Librarian`
- 用 `AIWorkflow` 固化可调度的工作流：
  - `captureClassification`
  - `threadSuggestion`
  - `discourseAnalysis`
  - `resumeSynthesis`
  - `draftPreparation`
  - `listCuration`
- 用 `ThreadnoteAIProvider` 作为唯一 provider 协议入口
- 用 `AIRequest` / `AIResponse` 作为统一边界模型
- 用 `ThreadnoteAIConfiguration.default` 明确默认 provider 优先级：
  - 输入与整理类 workflow 优先本地启发式
  - 结构化与 draft 类 workflow 优先本地模型，再回退云模型

## Alternatives

1. 直接在 `Store` 里调用某个云 provider SDK

- 好处：接入快
- 问题：后续 provider 替换成本高，测试困难，失败路径会泄露到产品主路径

2. 暂时完全不建协议层，继续沿用本地启发式

- 好处：短期最省事
- 问题：task005 无法完成，后续接 Apple Foundation Models 或云模型时会返工

## Consequences

- 正面：
  - provider 能按 workflow 单独切换
  - 产品层可以先对齐角色与边界，再决定模型能力
  - capture / thread / list 主路径不再依赖单一 SDK
- 代价：
  - 现在需要维护一组请求/响应模型
  - 现有启发式能力后续需要逐步挂接到协议层实现

## Rollback

如果这层协议被证明过度设计，可回退为：

- 保留 `AIRole` / `AIWorkflow` 命名
- 删除 provider 抽象实现
- 让 `Store` 继续直接使用本地启发式

但不建议回退到直接依赖云 provider SDK 的做法。
