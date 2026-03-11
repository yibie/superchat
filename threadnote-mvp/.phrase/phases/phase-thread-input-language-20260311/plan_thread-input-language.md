# Plan: Thread Input Language

## Milestone 0: Phase Gate

- 从 `spec_note-language.md` 提炼并独立出输入语言 phase
- 锁定本阶段只处理 note 输入语法与语义，不混入 thread 页面布局收敛
- 将 parser、ledger、object aggregation、note focus view 视为后续实现目标

## Milestone 1: Role Dictionary

- 固定 `#role` 的受控词典
- 区分 core discourse units、loose capture roles、stable roles
- 明确默认 role 与多 role 冲突规则

## Milestone 2: Object Layer

- 定义 `@object` 的开放对象语义
- 定义对象聚合与对象类型推断边界
- 明确对象不进入 ledger

## Milestone 3: Reference Layer

- 定义 `[[reference]]` 的显示语义与存储语义
- 明确 note/thread 引用与 reply 的边界
- 明确 rename / delete / archived target 的行为

## Milestone 4: Discourse Graph

- 固定核心节点：Question / Claim / Evidence / Source
- 固定核心关系：supports / opposes / informs / answers
- 定义 typed relationships 与 `#role`、`[[reference]]` 的关系

## Milestone 5: Downstream Mapping

- 定义哪些 stable roles 会进入 `Settled So Far`
- 为 parser、UI、note focus view 提供实现映射原则
- 明确哪些关系可由系统推断，哪些需要用户显式输入或确认

## Exit Criteria

- `# / @ / [[ ]]` 的职责边界清楚且互不重叠
- `Settled So Far` 的来源可以明确解释为 notes 的稳定语义视图
- 基础 discourse units 与 typed relationships 可以被实现层直接映射
- 后续 parser/UI 实现不需要再反复发明输入语义
