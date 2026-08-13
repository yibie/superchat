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

## 6. 用户审查流程（产品化第一步）

`/review` 命令与 `superchat-experience-review-needs` 在真实模型下验证通过：

- 经验引导的动作失败时（真实模型 + 破坏的 workspace_info），产生 1 个 open Review Need
- `/review retain` 落库 applies-under relation，need 关闭
- revoke / fails-under 路径由 ERT fixture 覆盖（同一命令路径）
- 成功的引导恢复不产生 need（设计如此——只有失败才需要人审查）
- `M-x superchat-review`：minibuffer 审查，显示规则可读文本，与 `/review` 共享同一落库 seam

## 7. 压缩探针：正确/错误的信息结构

参考 <https://nathan.rs/posts/gzip-lm/>（压缩-预测等价）。问题：正确与错误
在信息上是否有可发现的结构？

**结论：有。** 52 个真实恢复 episode（45 recovered / 7 failed，9 个
实验库），行为骨架（动作序列，剥离经验注入文本、scope、id、revision）
留一法压缩分类 51/52 正确；预测性前缀（只看失败后的第一个恢复动作）
同样 51/52。加入任务文本反而掉到 63%——**行为是结构，措辞是噪声**。

- 唯一错分：review-treatment（模式与成功类相同但环境折断），margin 仅
  7 字节。错误有两种：结构外（模式不同，可分）与结构内（模式相同但
  环境折断，不可分）；后者是压缩方法的天花板。
- 30B 无协助自愈的证据：同一 control arm 设计，30B 的 episode 被正确
  分进成功类，9B 全在失败类——它分的是行为，不是 harness 分组。
- 负面结果：规则文本的压缩相似度被词汇频率劫持，不能直接当适用性闸门。
- 外部实践：NEC/NAIST ICSME 2018 NCD 故障克隆检测（召回 1.0、精确率
  0.01——排名工具而非分类器）；ICSE 2025 LLM 熵重排故障定位（熵做重排
  信号，不单独分类）。详见 `docs/design/compression-probe-verdict.md`。

产品落地（先看，不动手）：`/review` 与 `M-x superchat-review` 显示
结构提示——需要中的 Run 骨架对成功/失败历史语料的增量压缩代价差。
margin > 0 提示“模式符合成功史，环境折断”（倾向 retain），< 0 提示
“模型选错路”（倾向 fails-under/revoke）；历史不足（LOO 需要每类 ≥ 3 Run）时
保持沉默。只显示，不自动判定。

## 8. 结构监控与自动层（眼睛已装，手仍在人）

### 8.1 结构监控

`M-x superchat-experience-monitor`：语料平衡（recovered/failed Run 数）、
类别分离健康度（最近 Run 的 margin 均值）、观察名单（失败但行为结构
匹配成功史的 Run——结构内错误的漏网之鱼）。薄历史（每类 < 3 Run）
保持沉默；有界折叠（全量语料 + 最近 10 个 Run 的 margin）。

### 8.2 自动层第一级（默认关闭）

机器只排序和标注，不判定：

- `superchat-experience-auto-annotation-enabled`（nil）：每个 Run 完成后
  追加 `experience.structure.recorded` 事件（骨架 margin + 预测），
  actor `experience-auto`，挂在 `superchat-run-completed-functions` 上
- 审查队列自动排序：`/review` 与 `M-x superchat-review` 按 margin 排序，
  正 margin（模式对、环境折断）排最前
- `superchat-experience-auto-watchlist-threshold`（3）：观察名单累计
  过阈值时提醒看 monitor；nil 关闭告警

### 8.3 自动层第二级：候选结构选择（默认关闭）

`superchat-experience-auto-candidate-selection`（nil）：多个候选竞争同一
上下文时（原本 fail-closed），按当前 Run 行为前缀对每个候选的
成功干预历史语料的增量压缩代价排序；严格胜者被选中注入，平局/无语料
保持 blocked。排名以 `experience.candidate-ranking.recorded` 落 Tape
（actor `experience-auto`，含逐候选代价）。诚实的退化边界：只观察到
失败那一步时（单步任务）前缀无法区分，行为不变。

### 8.4 交叉模型审查与接缝修复

Kimi 独立审查测试文件抓到一个致命接缝：`review-needs` 产出的 need 里
`:intervention` 只有规范引用（无 `:run-id`），而结构提示靠 `:run-id` 找
骨架——提示在真实审查路径上恒为 nil，ERT 因手工构造 need 恰好绕过了
断点。已修复（从 intervention 事件本身解析 run-id）并补真实接缝测试。
同时修正：`--run-skeletons` 顺序文档（最新在前，monitor 依赖）、
separation 行 float 格式化（truncate）、2/2 边界自相矛盾输出、空 reason
交互。

## 9. 下一步

1. 第三级自动 revoke：等真实历史校准“连续失败”阈值（不对称安全、
   Tape 可逆，但 52 个实验 episode 定不了阈值）
2. monitor checkpoint 物化：等数据量大到折叠变慢
3. 跨任务泛化：待产品积累真实多任务经验

边界不变：所有自动开关默认关闭；机器只排序、标注、选择，retain/
revoke 判定权仍在人；Tape append-only，自动动作以 `experience-auto`
actor 可审计。

## 附：关键文件

- 产品：`superchat-workspace.el`（durable-file）、`superchat-runtime.el`（状态机）、`superchat-authority.el`（闸门）
- 实验：`experiments/experience-learning-minimal/recovery-self-learning.el`（闭环驱动）、`verdict.el`（判定）、`compression-probe.el`（压缩探针）、`review-flow-real.el`（真实审查路径）
- 设计：`docs/design/experience-learning-real-tests-v0.md`（五方法 + 恢复域闭环记录）
- 架构诊断：`docs/research/synthesis-architectural-diagnosis.md`
