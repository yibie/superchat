# Tasks: Thread Input Language

## Phase Status

- 当前阶段状态: active
- 当前开放顶层任务: 无
- 当前开放子任务: 无

task001 [x] 场景:产品团队需要一套稳定的 `#role` 词典 | Given:当前 note role 仍分散在旧 `EntryKind`、讨论稿和 UI 假设里 | When:将 `#role` 固定成正式词典并定义默认行为 | Then:后续 parser 与 UI 有统一 role 语义 | 验证:规格文档审查+swift build
task002 [x] 场景:产品团队需要明确 `@object` 与 `[[reference]]` 的边界 | Given:对象追踪、显式引用与 reply 仍可能混淆 | When:定义 `@object` 和 `[[reference]]` 的规则与边界并将其接入 parser 与显示层 | Then:对象聚合、引用跳转与 reply 层能各自独立演进 | 验证:规格文档审查+swift build
task003 [x] 场景:产品团队需要把输入语言映射到 discourse graph | Given:基础记录单元和 typed relationships 刚被提出 | When:定义节点、边和 ledger 的映射规则 | Then:实现层可以直接将 notes 解释为 graph 节点与边 | 验证:规格文档审查+swift build
task004 [x] 场景:用户在输入框连续输入时不应触发系统级不稳定 | Given:自定义 NSTextView 桥接触发 ViewBridge 断连日志且补全面板被输入框裁切 | When:将输入内核改为 SwiftUI TextEditor 并将补全面板挪到输入框下方 | Then:输入过程稳定且补全内容完整可见 | 验证:手动测试+swift build

## task001 breakdown

- task001-01 [x] 固定 core discourse units 与 loose capture roles
- task001-02 [x] 固定 stable roles 与 ledger 入口规则
- task001-03 [x] 定义默认 role 与多 role 冲突规则

## task002 breakdown

- task002-01 [x] 定义 `@object` 的开放对象语义
- task002-02 [x] 定义 `[[reference]]` 的 note/thread 引用规则
- task002-03 [x] 定义 reply 与 explicit reference 的边界

## task003 breakdown

- task003-01 [x] 固定 supports / opposes / informs / answers
- task003-02 [x] 定义 typed relationships 与 `#role` 的推荐配对
- task003-03 [x] 定义 `Settled So Far` 从 stable-role notes 的过滤规则
