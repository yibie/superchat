# Experience Audit v0 Preregistration

- 状态：`preregistered; Pattern Observation not started`
- 日期：2026-08-05
- 数据集：[DeepSeek Experience Cohort v6](../../experiments/experience-cohort-deepseek-v6/)
- 数据锁：[dataset-lock.json](../../experiments/experience-cohort-deepseek-v6/dataset-lock.json)，SHA-256 `e815b9c2953daee73de64f79a15f854d07b34475e2216c39b078d6c26d0583ae`
- 前置协议：[Auditability Completeness Protocol v0](auditability-completeness-protocol-v0.md)
- 前置盘点：[Experience Auditability Inventory v0](experience-auditability-inventory.md)
- Eligibility：[Audit Eligibility Report v0](../../experiments/experience-audit-v0/audit-eligibility-report-v0.md)

## 1. 目的与零假设

Experience Audit v0 只回答：

> Cohort v6 的冻结历史中，是否存在没有被 Designer Prior 解释、能够跨 Discovery task 重复定位的结构性行为？

零假设：

> 在排除 Workflow、Prompt、Profile、Tool surface、Authority 和实验 guard 注入的结构后，Cohort v6 不包含有资格进入 Learning-oriented intervention 的重复结构。

本协议允许零个 Pattern Observation。没有发现候选是有效结果，不触发补样、放宽分类或修改 Runtime。

## 2. 边界

本阶段是冻结数据上的只读实验分析，不是 Runtime 功能：

- 不修改 Runtime、Tape、Closure、Workflow、Workspace 或 Authority；
- 不新增 Tape event、Projection、数据库表或 Runtime Contract；
- 不读取或执行 held-out tasks；
- 不记录、恢复或推断 hidden chain of thought；
- 不用当前配置补写运行时缺失的 Prior；
- 不把 Observation 提升为 Workflow、Memory、Policy 或 Capability；
- 不做 effectiveness、causal improvement 或 learning claim。

可观察对象是 decision antecedents、Runtime facts、Artifact lineage 和 Outcome Record，不是模型内部动机。

## 3. 冻结数据与分析单位

Cohort v6 包含：

| 对象 | 数量 | 用途 |
|---|---:|---|
| Assignment / Workflow instance | 20 | 实验分配与 Outcome 的主单位 |
| Research stage Run | 60 | Collector、Analyst、Reviewer 的嵌套执行单位 |
| Evaluator Run | 20 | Outcome Closure；不作为行为 Pattern 样本 |
| Runtime Run | 80 | 执行事实总数 |
| Artifact | 60 | producer/consumer lineage 单位 |
| Workspace | 20 | 每个 Assignment 一个隔离 Workspace |
| Tool failure episode | 4 | Recovery episode 候选单位 |

Primary analysis unit 是 Assignment。Stage Run、Artifact edge 和 Tool episode 嵌套在 Assignment 中，不能被当作相互独立的 60 或 80 个样本。

### Split 规则

| Split | 数量 | Audit 用途 |
|---|---:|---|
| Calibration | 4 | 只证明数据生成机制；排除 occurrence 计数 |
| Discovery | 12 | 唯一允许增加 Pattern occurrence 的主样本 |
| Replicate | 4 | 只描述同任务执行稳定性；不得增加 occurrence threshold |
| Held-out | 0 used | 保持 sealed；本阶段禁止读取与执行 |

任何报告都必须同时写出 unit、eligible split、eligible count 和 excluded count。不得使用模糊的 `eligible_runs: 20`。

## 4. Audit 输入

```text
Experience Dataset
    =
Run Evidence Bundles
+ Designer Prior Envelope
+ Tape / Authority facts
+ Artifact producer-consumer lineage
+ Outcome Records
```

权威来源：

- Dataset bytes 与 hash：`dataset-lock.json`；
- 执行事实：`runtime-snapshot.db`；
- 实验条件：`manifest.json`、`assignments.json`、`prompt-snapshots.json`；
- Run/Artifact/Closure join：20 个 `evidence-bundle.json`；
- 评价：20 个 `outcome-record.json`；
- Artifact bytes：`runtime-artifacts/`。

Derived 结果必须标明 interpreter/query 与 source；Derived 不能伪装成运行时持久化事实。

## 5. Designer Prior 先于 Pattern

Prior classification 作用于一个具体 behavior/pattern，不作用于整个 Run。一个 Run 可以同时包含 forced、suggested 和 unknown 行为。

| Classification | 判定 | Learning candidate eligibility |
|---|---|---|
| `forced` | Workflow、hard Prompt、Authority 或 guard 明确要求 | 排除 |
| `suggested` | soft Prompt、Profile、名称或 Tool description 暗示 | 仅可成为 application-design hypothesis |
| `emergent` | 相关 Prior 完整，未要求或暗示，且跨 Discovery task 重复 | 可以进入候选 gate |
| `unknown` | Prior 不完整、证据冲突、样本不足或来源不能排除 | 阻塞 |

找不到注入来源不等于证明没有注入来源。缺少相关 Prompt、Workflow、Profile 或 model-facing Tool schema 任一关键 Prior 时，不得标记 `emergent`。

### v6 已知 forced 结构

以下行为由冻结 Prior 明确要求，Audit 必须预先排除：

- Collector -> Analyst -> Reviewer topology；
- 使用 `agent_reach_search` 搜索官方/主要来源；
- 使用 `agent_reach_read` 读取重要来源；
- Analyst/Reviewer 检查前序 delivered Artifacts；
- 每个 stage 在结束前调用 `workspace_publish`；
- 每个 stage 正好交付一个不超过 2,000 words 的 Artifact；
- Authority allowlist 和七工具 surface；
- blind evaluator 与 `research-quality-v0` rubric。

## 6. Audit lane A：Artifact Lineage

### 问题

> 是否存在跨 Assignment 的显式 Artifact consumer edge，或未被 Prior 强制的重复 lineage？

### Query 规则

1. Producer 来自 `workspace.artifact.added` 的 Artifact ID、revision、Run、Actor 和 Workspace；
2. Consumer 只接受 `workspace_read` Authority effects 中的显式 Artifact ID；
3. Consumer terminal 必须是相同 execution ID 的 `tool.completed`；
4. `producer_workspace != consumer_workspace` 才是 cross-task edge；
5. failed read 不作为成功 consumer edge，但保留为事件；
6. 读取完成只证明访问，不证明语义使用或帮助；
7. Prompt 强制的同一 Workflow 内 handoff 标为 `forced`。

允许输出 producer/consumer coverage、cross-workspace edge count、缺失和反例。禁止从读取次数推断 Memory eligibility 或 Outcome improvement。

## 7. Audit lane B：Recovery Episode

### 问题

> Discovery split 中的 `tool.failed` 后是否存在可定位、重复但非预设的后续动作序列？

### Query 规则

1. Episode 从一个 `tool.failed` 开始；
2. 记录后续第一个 `tool.requested`、其 terminal 和所属 Run terminal；
3. 相同工具再次调用只能描述为 `same-tool-follow-up`，不能自动命名为 retry；
4. 不同工具调用只能描述为 `different-tool-follow-up`，不能自动命名为 fallback；
5. 没有 durable failure class 或 fallback link 时，Prior classification 保持 `unknown`；
6. Calibration 与 Replicate episode 保留为 context，但不增加 occurrence；
7. 没有失败暴露的 Assignment 不是 recovery counterexample。

只有同一 sequence class 在多个 eligible Discovery assignments 中重复，且 failure/selection antecedents 足够完整时，才允许创建 Pattern Observation。完成 Run 不能证明后续动作导致恢复。

## 8. Audit lane C：Boundary Transition

### 问题

> 排除冻结 Workflow 与 stage guard 后，是否存在跨 Discovery task 重复的非预设状态转换？

### 排除规则

以下不是候选：

- Workflow stage order；
- Artifact delivery -> next stage；
- Prompt 要求的 prior Artifact read；
- Prompt 要求的 publish-before-finish；
- Authority validate -> allow -> execute；
- evaluator start -> Outcome Record。

Artifact revision transition 只有在同一 Artifact ID 存在多个有序 revision 时才成立。不同 role 发布三个不同 Artifact ID 不能被改写为 draft -> revision -> final。

## 9. Outcome association

Outcome 是 Assignment-level 描述，不是 Tool、episode 或单个 Artifact 的因果标签。

本阶段只允许：

```json
{
  "status": "descriptive-only",
  "metric": "frozen outcome label",
  "causal_status": "not-identifiable",
  "causal_claim": "prohibited"
}
```

没有 baseline、intervention 和 Comparison Closure 时，不得用 accept/revise/reject、Tool count 或 approximate latency 证明 Pattern 有效。

## 10. Missingness 与反例

- Missing 必须显式记录 source、impact 和 blocked claim；
- `unknown` 是合法终态，不要求自动消除；
- Counterexample 必须与 Pattern 有相同 exposure 和 eligibility；
- 没有 exposure 的 Assignment 不能作为反例；
- 没有符合条件的 counterexample 时，必须写明原因；
- observed absence 与 missing evidence 分开记录。

## 11. Pattern Observation 最小格式

旧的 [`pattern-observation.schema.json`](../../experiments/experience-cohort-v0/pattern-observation.schema.json) 保留为历史输入。任何 v6 Observation 在生成前必须包含下列额外字段；本预注册不生成 Observation：

```json
{
  "observation_id": "obs-...",
  "status": "observed",
  "pattern_type": "artifact-lineage | recovery | boundary-transition",
  "unit_of_analysis": "assignment | stage-run | artifact-edge | failure-episode",
  "eligible_split": "discovery",
  "eligible_count": 12,
  "excluded_counts": {
    "calibration": 4,
    "replicate": 4
  },
  "prior_classification": "forced | suggested | emergent | unknown",
  "prior_evidence_status": "complete | partial | missing",
  "occurrences": 1,
  "task_ids": [],
  "task_families": [],
  "evidence_ranges": [],
  "counter_examples": [],
  "observed_absence": [],
  "missing_evidence": [],
  "outcome_association": {
    "status": "descriptive-only",
    "causal_status": "not-identifiable",
    "causal_claim": "prohibited"
  },
  "promotion_status": "prohibited"
}
```

No-observation 结果写入 Audit Report，不伪造 `occurrences: 0` 的 Pattern Observation。

## 12. Candidate gate

最多选择一个候选，但只有同时满足以下条件才进入 Learning-oriented intervention preregistration：

1. Dataset lock 验证通过；
2. unit 与 eligible denominator 明确；
3. occurrence 来自多个 Discovery assignments，不能由 Replicate 增加；
4. Prior evidence 完整且分类为 `emergent`；
5. 有相同 exposure 的 counterexample，或明确说明为什么不可获得；
6. Outcome 仅用于描述与优先级，不作为因果证明；
7. Candidate specification 在任何 held-out 访问前冻结。

`suggested` 可以进入应用层 Prompt/Workflow 实验，但不能作为 learned structure。`unknown` 在补齐 Prior 前不得 promotion。

## 13. Stop conditions

出现以下任一情况，Experience Audit v0 停止在 Observation/Eligibility 层：

- Dataset lock 不匹配；
- 只有 forced 或 suggested 结构；
- Prior 不足以排除设计者注入；
- occurrence 只来自 Calibration/Replicate；
- 没有可比较的 exposure 或 counterexample；
- Outcome association 需要未经注册的因果解释；
- 三个 lane 均返回 observed absence 或 insufficient evidence。

停止不是失败，也不授权扩大 Cohort、解封 held-out 或增加 Runtime evidence。

## 14. 阶段顺序

```text
Dataset Lock
    ↓
Audit Eligibility
    ↓
Designer Prior classification
    ↓
Read-only Pattern Observation
    ↓
Candidate gate (zero or one)
    ↓
Hypothesis + intervention preregistration
    ↓
Held-out comparison
    ↓
Promotion review
```

Intervention 只是验证候选，不等于 Learning。只有 validated improvement 经 Promotion 并被安全复用后，才讨论 Learning Runtime。

