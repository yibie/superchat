# Spec: Electron AI Cockpit Productization

## Summary

本阶段目标不是继续做 Swift parity，而是把已经补齐的 Electron AI 能力产品化，收敛成一个真正可推进线程工作的 AI cockpit。

本阶段属于既有产品线内的体验深化和工作流产品化，不是全新产品方向，因此跳过 PR/FAQ。

## Goals

- 把 `Restart Note` 从“AI 摘要面板”升级成“线程工作驾驶舱”
- 把 `Prepare` 从“标题+列表”升级成面向具体任务的工作台
- 把 planner blocks 从只读信息升级为可操作入口
- 把 synthesize -> inspect -> prepare -> write back 变成闭环，而不是分散按钮
- 在不破坏当前稳定性的前提下，提升 AI 结果的可操作性、可追踪性和使用频次

## Non-goals

- 不重新打开 Swift parity 阶段
- 不做全量视觉重设计
- 不在本阶段引入新的底层 AI 基础设施
- 不为了“聪明”而扩大模型自由度

## Product Contract

### 1. Restart Note Cockpit

- headline 必须对应当前线程的单一工作判断
- planner blocks 必须可映射到真实线程对象，如 claim / evidence / source / question
- block 不只是展示，还要支持最小行动

### 2. Prepare Workspace

- writing / review / summary 必须有明显不同的结构，而不是同一模板换标题
- Prepare 结果应可回写 thread，而不是停留在临时面板
- Prepare 必须保持 deterministic fallback 和 AI state 可见性

### 3. Actionability

- 用户能直接从 AI block 执行动作
- 动作至少包括：promote claim、create task、mark resolved、open related note/source
- 所有动作都要可追溯到现有 thread 数据结构

### 4. Flow Closure

- synthesize 后应该自然引导到下一步，而不是只显示信息
- prepare 生成的结果应能反哺 thread state
- 线程从 capture 容器收敛为工作单元

## User Flows

### Flow A: Resume And Decide

用户打开 thread。

系统行为：
- 展示 current judgment 和 main gap
- 用户可直接点击 block 进入相关 note/source
- 用户能从 cockpit 直接采取下一步动作

### Flow B: Prepare And Write Back

用户选择 `writing` / `review` / `summary`。

系统行为：
- 渲染该 prepare 类型的专属结构
- 用户可以把生成内容转成 note / task / anchor
- 回写后 thread state 自动失效并重新 synthesize

### Flow C: Close Gaps

用户在 cockpit 看到 `gap` / `questions` / `risks` block。

系统行为：
- block 提供最小行动入口
- 行动完成后 block 所反映的状态发生变化
- 用户看到 thread 向前推进，而不是仅多一条笔记

## Edge Cases

- planner block 对应不到真实数据对象时，必须回退为只读
- Prepare 回写失败时，不得污染已有 thread state
- 连续触发 synthesize 和 prepare 时，仍要遵守现有 request token / stale result 规则
- renderer 强化后，不得牺牲当前简单线程的可读性

## Acceptance Criteria

- [ ] Restart Note 至少有一类 block 具备真实可操作入口
- [ ] `writing/review/summary` 三种 Prepare 在结构和用途上明显分化
- [ ] Prepare 结果至少支持一种回写 thread 的路径
- [ ] cockpit 与 prepare 形成闭环，而不是分离视图
- [ ] 新交互不会破坏现有 AI state/debug/persistence 边界
