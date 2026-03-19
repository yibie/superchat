# Plan: Electron AI Cockpit Productization

## Summary

Swift parity 已完成，下一阶段重点转向产品化。

方向不是“再补字段”，而是把现有 AI 结果变成用户可推进工作的界面。

## Scope

### In Scope

- Restart Note 的行动化和结构强化
- Prepare 工作台的类型分化和回写机制
- planner block 与 thread 实体的映射
- 最小闭环：synthesize -> inspect -> act -> re-synthesize

### Out of Scope

- 全量品牌化视觉改版
- 新 provider / 新模型接入
- 新 memory / embedding 基础设施
- 超出 thread 工作台范围的 agent 系统

## Milestones

### M1. Cockpit Action Model

- 定义哪些 planner blocks 可操作
- 明确 block 到 claim/evidence/source/task 的映射
- 决定哪些动作属于 P0

### M2. Prepare Workspace Differentiation

- 为 `writing/review/summary` 设计不同结构
- 为每种 Prepare 定义输出格式和回写路径

### M3. Write-Back Loop

- 从 Prepare / cockpit 直接生成 note/task/anchor
- 回写后触发 thread state invalidate + re-synthesize

### M4. UX Tightening

- 收敛按钮、入口和状态提示
- 保证 desktop/mobile 下都可用

## Priorities

### P0

- 可操作 cockpit block
- Prepare 类型分化
- 回写 thread 的闭环

### P1

- 交互层级优化
- discourse 结果在 UI 中的最小显化

### P2

- 更细的视觉 polish
- richer debug / provenance 展示

## Risks

- 一旦 block 行为设计错，容易把 cockpit 做成第二套 thread editor
- Prepare 回写如果太重，会让用户失去对 thread 数据结构的理解
- 交互入口过多，会把当前简单界面拖回复杂工具面板

## Decision

- 优先做“少而硬”的行动入口，不追求大而全
- 每个新入口都必须有明确 thread 数据落点
- 以工作流闭环作为完成标准，而不是 UI 丰富度
