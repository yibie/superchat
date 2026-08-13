# 经验自学习系统：完整实验报告

> 时间跨度：2026-08-12 至 2026-08-13
> 模型链：qwen3.5:9b（Ollama）→ LFM2.5-1.2B（LM Studio）→ Qwen3-30B-A3B（LM Studio）
> 结论先行：可靠的是架构，学会的是 30B；证据完整，默认保持关闭。

---

## 1. 问题与根因

### 现象

经验注入在 temperature=0 时 100% 有效，默认温度下不可靠——treatment 组有时完全无视注入的经验。

### 根因（架构诊断，非模型问题）

完整的控制回路是 Sensor → Controller → Actuator → Plant：

| 部件 | 实现 | 状态 |
|------|------|------|
| Sensor | Authority 观察者检测 `tool.failed` | ✅ |
| Controller | Recovery View 组装（选择 promoted 候选） | ✅ |
| **Actuator** | **经验作为一条 user-role 文本消息注入 prompt** | ❌ 断裂点 |
| Plant | LLM token 采样 | ⚠️ 随机性 |

经验注入是"建议"（suggestion），不是"控制信号"（control signal）。temperature=0 只是冻结了 Plant 的随机性，让建议碰巧等同于指令——机制上仍是建议。

## 2. 架构修复：最小链路

三层配合，全部落在现有模块边界内：

```
workspace_read(stale)
  → typed tool.failed（Authority 记录，不被阻断覆盖）
  → experience.intervention.recorded（Runtime 组装 View）
  → Runtime 策略状态机：awaiting-info → awaiting-read → satisfied
  → Authority 执行闸门：awaiting-info 阶段拦截重复 workspace_read，
    放行 workspace_info 和无关工具
  → workspace_info 成功 → awaiting-read
  → 正确 revision 的 workspace_read 成功 → satisfied
```

产品改动：
- `superchat-workspace.el`：新增 `durable-file` adapter（持久且可变），generated-file 保持不可变
- `superchat-runtime.el`：恢复策略状态机 + 两个时序修复（阻断失败不覆盖 typed failure；每轮策略状态刷新）
- `superchat-authority.el`：经验前置条件闸门（只拦 `dependent-tools`，不拦信息工具）
- 完整 ERT：734 tests，0 unexpected

## 3. 证据链

### 阶段 1：七格 durable cohort（真实 Ollama）

P-missing、P-stale、N-absent、N-fails、N-conflict、N-multiple、N-revoked 全部通过。正面格完整走完三工具恢复路径；负面格单动作停止。

### 阶段 2：五种真实环境方法

配对对照、内容消融、负迁移+安全（Authority 拦截真实触发）、持久化、撤销——全部按预期行为。

### 阶段 3：重复配对（核心量化证据）

| 模型 | control 首次选对 | treatment 首次选对 | JSD |
|------|-----------------|-------------------|-----|
| qwen3.5:9b | 0/4 | 4/4 | ln 2（理论上限） |
| LFM2.5-1.2B | 0/4 | 4/4 | ln 2 |
| Qwen3-30B | 0/4 | 4/4 | ln 2 |

行为分离由架构决定，与模型规模无关——1.2B 模型同样完美分离。

### 阶段 4：恢复域自我学习闭环（Qwen3-30B）

```
源 Run（无经验，清洁任务）
  → 模型自发发现两步恢复（read 失败 ×N → workspace_info → 修正 read）
  → 准入闸门（typed failure + corrective success 才放行）
  → 反思 Run 诱导规则（闭 JSON schema，无答案泄漏）
  → 用户 promotion 门槛
  → 留出对照：treatment 1.0 vs control 0.0
  → 持久化：重开后 View 仍 satisfied、仍 steering
  → 撤销：revoke 后 View 空、模型回到无引导
  → 措辞迁移：新 context claim + 新 relation + 新措辞，规则仍生效
```

诱导出的规则（3/3 一致）：
> "When a workspace_read fails due to a revision mismatch, use workspace_info to inspect the current state before retrying the read."

### 阶段 5：正式判定

`superchat-recovery-self-verdict`（3/3）：

```
status: supported-with-human-gate
model-induced-candidate: observed
held-out-task-effect: observed
learning: supported
default-remains-off: t
```

## 4. 三模型对照总结

| 能力 | qwen3.5:9b | LFM2.5-1.2B | Qwen3-30B |
|------|-----------|-------------|-----------|
| 经验注入后行为分离 | ✓ 4/4 vs 0/4 | ✓ 4/4 vs 0/4 | ✓ 4/4 vs 0/4 |
| 自发发现两步恢复 | ✗ 从不 | 未测 | ✓ 3/4 |
| 诱导正确规则 | 未达标 | — | ✓ 3/3（清洁 prompt 后） |
| 留出效应 | — | — | ✓ 1.0 vs 0.0 |

规模只在"自发发现 + 诱导"这步起作用；执行层的行为差异是架构的。

## 5. 已证实 / 未证实

### 已证实

- 经验注入（软提示）+ Authority 闸门（硬约束）的组合在 3 个模型上可靠工作
- 30B 模型能自发发现两步恢复并诱导出正确规则
- 诱导规则不绑定字面措辞（迁移 3/3）
- 经验持久化与撤销在诱导候选上工作正常
- 负迁移时 fail-closed（候选文本零泄漏）

### 未证实（诚实的边界）

- 所有证据在单一任务类别内（workspace revision recovery）
- 跨任务泛化未做——且当前架构无 scope 泛化机制，属产品决策
- 真实用户场景未验证（promotion 是 harness 里的 user actor）
- `default-remains-off` 保持：这是证据，不是启用依据

## 6. 下一步

1. 产品化：用户审核经验的实际交互流程（真实 promotion 路径）
2. 熵监控 Projection（方案 C"先有眼睛"）：JSD 产品化为持续监控
3. 跨任务泛化：待产品积累真实多任务经验后，作为下一阶段研究问题

## 附：关键文件

- 产品：`superchat-workspace.el`（durable-file）、`superchat-runtime.el`（状态机）、`superchat-authority.el`（闸门）
- 实验：`experiments/experience-learning-minimal/recovery-self-learning.el`（闭环驱动）、`verdict.el`（判定）
- 设计：`docs/design/experience-learning-real-tests-v0.md`（五方法 + 恢复域闭环记录）
- 架构诊断：`docs/research/synthesis-architectural-diagnosis.md`
