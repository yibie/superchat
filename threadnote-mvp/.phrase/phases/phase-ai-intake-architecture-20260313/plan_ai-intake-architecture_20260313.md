# Plan — phase-ai-intake-architecture-20260313

## Milestones

### M1 — Capture Intelligence Boundary

把新 note 的理解逻辑从 `Store` 中抽出，建立独立 intake 层。

**交付物**：
- `CaptureInterpretation` 数据结构
- `CaptureInterpreter` 模块
- 显式 tag 保留 + 无 tag 的基础 item type 推断
- 基础对象抽取入口
- candidate claim 提炼入口

**验收**：capture 解释结果可单测，不依赖 `Store` orchestration 才能验证

---

### M2 — Thread Signature Engine

为每个 active thread 生成紧凑签名，作为 routing 主输入。

**交付物**：
- `ThreadSignature` 数据结构
- `ThreadSignatureEngine`
- 由 title / goal / active claims / latest anchor / open loops / top objects 派生签名

**验收**：thread signature 可解释，可在无 retrieval 命中时仍提供基础匹配信号

---

### M3 — Routing Engine Rework

将 route 主逻辑改为 `capture semantics + thread signature + retrieval support`。

**交付物**：
- RoutingFeatures 模型
- ThreadRoutingEngine 内部评分模型重写
- `route / suggest / stayInInbox` 决策边界
- recency 仅用于 suggestion，不用于高置信 auto-route

**验收**：ambiguous note 不再被 retrieval-only 强制归类；清晰 note 可稳定 auto-route

---

### M4 — Thread State / Prepare Boundary Cleanup

让 thread-state 和 prepare 明确建立在 thread 内容整理之上，而不是 intake 逻辑的延伸。

**交付物**：
- ThreadStateInputBuilder
- deterministic ThreadStateSynthesizer
- ThreadPresentation / ThreadBlock 受限生成式 UI 数据结构
- Prepare composer 清晰化
- Retrieval / Memory 输入边界收紧
- Restart 区域的 block-based cockpit 渲染
- deterministic fallback 与 LLM enrichment 职责清晰

**验收**：Restart Note / Prepare 输入来源可解释，且不再承担 intake 推断职责；LLM 不再直接改写核心 thread state；thread 页面可根据状态渲染不同 block 组合

---

### M5 — Product Fit Validation

用用户承诺校验实现，不让系统滑向“全文检索归档器”。

**交付物**：
- raw capture 场景测试
- route confidence / ambiguity 测试
- no-backend regression 测试
- 与 v1 承诺对齐的验收清单

**验收**：用户只输入，不整理，仍能感受到 item type / object / route / thread state 的自动化价值

## Priorities

| 优先级 | 里程碑 | 理由 |
|--------|--------|------|
| P0 | M1 | 没有 capture intelligence，就没有正确的 AI 主边界 |
| P0 | M2 | routing 不该直接依赖 retrieval 文档堆积 |
| P0 | M3 | route 是最直接的用户信任问题 |
| P1 | M4 | thread state / prepare 已经有产出，优先做边界清理而非推倒重来 |
| P1 | M5 | 需要用产品承诺校验，而不是只看工程完成度 |

## Scope

**In scope**：
- Capture Intelligence 模块化
- Thread Signature 模型与派生逻辑
- Routing Engine 重写
- Thread state / Prepare 输入边界清理
- 对应测试与回归

**Out of scope**：
- 新 UI 大改
- 自动创建 thread
- 历史数据回写重分类
- 通用 agent / autonomous task system

## Risks & Dependencies

| 风险 | 缓解 |
|------|------|
| 与现有 ai-memory-rag phase 职责重叠 | 明确本 phase 依赖 retrieval / memory 基础层，但不重做其 schema |
| Capture Intelligence 做成另一个大杂烩 | 先锁定输出结构，再加实现细节 |
| Routing 改动破坏现有 suggestion 路径 | 保留统一入口，先替换内部评分，不拆 UI contract |
| LLM 与 deterministic fallback 再次漂移 | 强制 deterministic 先产出，LLM 仅做可选增强 |
| 目标过大，phase 失控 | 以“用户只输入，不整理”作为唯一收敛标准 |

## Rollback

- M1 / M2 新模块可并存于现有实现后面，通过开关切回旧路径
- M3 保留 route 决策入口，若新评分异常，可临时回退到旧评分实现
- M4 不改变现有 UI 结构，必要时仅回退输入构造层
