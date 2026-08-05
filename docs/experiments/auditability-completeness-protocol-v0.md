# Auditability Completeness Protocol v0

- 状态：experimental, structurally validated
- 日期：2026-08-03
- 适用对象：进入 Experience Audit 之前的 prospective Agent experiments
- 前置盘点：[Experience Auditability Inventory v0](experience-auditability-inventory.md)
- 验证状态：Canary A v2 已验证单 Run 结构闭合；Canary B v2 已验证 paired Comparison Closure（五个 Gate `pass`）
- 历史验证证据：Canary A v2 与 Canary B v2 已移入[实验历史归档](../../experiments/archive/superchat-experiment-history-through-2026-08-05.zip)。
- 声明边界：`comparison-infrastructure-only`；effectiveness `not_evaluated`；learning claim `prohibited`
- 冻结评价：[Research Outcome Rubric v0](../../experiments/research-outcome-rubric-v0.json)（SHA-256 `8159d93103f8375aa1024226b3b1af6d18bfae02ff5d3a1d3febfd2bfc847b45`）

## 1. 目的

本协议判断一次 Agent 实验是否留下了足以被解释、比较和证伪的证据。它不判断 Agent 是否已经学习，也不从历史行为生成能力。

只有通过本协议全部适用 gate 的实验，才有资格进入 Experience Audit：

```text
Runtime Foundation
        ↓
Auditability Canary
        ↓
Evidence Gap Report
        ↓
Audit-ready Cohort
        ↓
Experience Audit
```

本协议是实验基础设施，不是 Runtime Kernel：

- 不新增 Tape event、Projection、数据库表或 Runtime Contract；
- 不修改 Closure、Authority、Workflow 或 Workspace semantics；
- 不记录或推断 hidden chain of thought；
- 不把 Observation 自动提升为 Workflow、Memory 或 Policy；
- 不用当前配置补写历史 Run 的缺失证据。

## 2. 可观察因果边界

五类问题必须由不同证据面回答：

| 问题 | 负责对象 | 可观察证据 | 不能声称 |
|---|---|---|---|
| Why selected? | Prompt / Profile / Workflow / offered Tools / Model output | 冻结的输入条件与实际 tool call | 模型内部为什么“想”这样做 |
| Why permitted? | Authority | policy、effects、scope、approval、decision | Agent 为什么选择该动作 |
| What happened? | Tape | ordered events、identity、causation、terminal facts | 未记录的执行状态 |
| What changed? | Workspace | Artifact、revision、producer、explicit consumer | Artifact 的业务价值 |
| Did it help? | Outcome Evaluation | frozen rubric、evaluator、measurement、comparison | 没有对照时的因果改善 |

表中的 `Why selected?` 只表示 **selection antecedents**：在什么可观察条件下产生了什么调用。它不是 reasoning trace，也不是对模型内部动机的解释。

Authority 只证明执行裁决。例如：

```text
effect = external-write
policy = requires-approval
approval = user-approved
decision = execute
```

它可以解释为什么该调用被允许执行，不能解释为什么模型选择了该调用。

## 3. 协议原则

### 3.1 Provenance first

```text
State without provenance   = unknown
Pattern without prior      = unknown
Outcome without comparison = unknown
```

找不到设计者注入来源，不等于证明没有注入来源。

### 3.2 Decision antecedents, not hidden reasoning

允许记录：

- Prompt、Profile、Workflow 和 Tool surface 的冻结版本；
- Model/provider 的可观察身份与设置；
- model output 产生的 tool call；
- Authority decision 和执行结果。

禁止要求、保存或事后生成：

- hidden chain of thought；
- 对“模型内心动机”的叙事；
- 无证据支持的选择理由。

### 3.3 Absence is data

缺失证据必须被显式记录，不能用默认值、当前配置或推断值填充：

```json
{
  "field": "model_version",
  "value": null,
  "status": "missing",
  "source": null,
  "impact": "cannot_compare_runs"
}
```

Evidence status 使用：

| Status | 含义 |
|---|---|
| `observed` | 有直接、可定位的证据 |
| `derived` | 由声明的 interpreter/version 从事实计算得到 |
| `missing` | 需要但不存在，影响必须写明 |
| `not-applicable` | 协议明确允许不适用，并记录理由 |
| `invalid` | 证据冲突、hash 不匹配或引用断裂 |

`missing` 不自动变成 `derived`；`derived` 也不能伪装成执行当时持久化的事实。

### 3.4 Freeze before execution

所有可能影响比较的实验条件必须在第一个 Run 开始前冻结。运行后修改 manifest 会使 Comparison Closure 失效；修正只能创建新的 manifest revision 和新的 experiment ID。

### 3.5 No weighted completeness

Gate 不打总分，也不允许用多个强证据抵消一个关键缺口。一个 required evidence 缺失，就保留对应 gate 的 `blocked`。

### 3.6 Consume facts, do not redefine semantics

> Experimental infrastructure must consume Runtime facts, not redefine Runtime semantics.

实验基础设施可以 snapshot、连接或用声明版本的 interpreter 派生 Runtime 事实，但必须保留 source 以及 `observed` / `derived` 区别。实验层 decoder 可以规范化可证明等价的 transport shape，不能改变事件含义、补造缺失状态，也不能让实验 label 反向成为 Runtime fact。

## 4. 实验记录对象

协议只要求三个外部实验对象；v0 不要求新的 Runtime 对象。

### 4.1 Experiment Manifest

在运行前冻结，描述可比较条件和评价方法。

最小字段：

| Category | Required evidence |
|---|---|
| Experiment | experiment ID、manifest revision/hash、freeze time |
| Task | task ID/family、input snapshot/hash、discovery/held-out assignment |
| Workflow | definition snapshot/URI/hash、stage order、stage prompt snapshots/hashes |
| Prompt | system prompt、composed prompt、context manifest 的 snapshot/URI/hash |
| Profile | profile ID、definition snapshot/URI/hash |
| Model | provider、model ID、可获得的 version、关键 generation settings |
| Tools | offered tool names、schema snapshots/hashes |
| Authority | policy name、policy version、policy snapshot/URI/hash |
| Extensions | activated set、definition snapshots/hashes |
| Runtime | package version 或 commit、event schema、Closure contract |
| Outcome | rubric snapshot/hash、evaluator identity/version、missing-data rule |
| Comparison | baseline、intervention、assignment、exclusion rule、sample plan |

只有 hash 而没有可恢复的内容，不足以区分 `forced`、`suggested` 和 `emergent`。敏感内容应放在受控存储中并在 manifest 中保存不可变引用与 hash；不得为完整性把 secret 写入 Tape 或公开报告。

Provider 不暴露精确 model version 时，必须记录实际可观察的 provider/model ID，并把 version 标为 `missing`，不能猜测。

### 4.2 Run Evidence Bundle

运行后生成，把外部 manifest 与现有 Runtime 事实连接起来。

最小 join keys：

```text
experiment-id
manifest-hash
task-id
session-id
turn-id
run-id
actor-id
workspace-id
artifact-id / revision（存在 Artifact 时）
outcome-record-id
```

Bundle 只保存引用、状态与必要的实验 metadata；Tape 和 Workspace 仍是 Runtime 事实来源，不复制成第二套事件日志。

### 4.3 Outcome Record

Outcome Record 至少包含：

- experiment、task 和 Run identity；
- rubric identity/hash；
- evaluator identity/version 或 human reviewer identity；
- evaluation time；
- 每项 label/score 及其 evidence reference；
- missing-data 与 abstention；
- 在盲评时记录 blinded protocol，不泄漏 assignment 给 evaluator；
- 人工更正时保留 revision，而不是覆盖旧结果。

Outcome Closure 证明“评价过程完整”，不等于证明“该结构有效”。有效性仍需要 Comparison Closure 和 held-out 结果。

## 5. 五个 Closure Gate

Gate verdict 只有：

| Verdict | 含义 |
|---|---|
| `pass` | 所有 required evidence 存在、可定位且一致 |
| `blocked` | 证据缺失或当前实验设计不足，不能得出目标结论 |
| `invalid` | 证据冲突、身份错连、hash 不匹配或发生未登记的事后变更 |

### Gate 1: Prior Closure

问题：Run 当时的可观察执行条件能否被重建？

`pass` 条件：

- Experiment Manifest 在 Run 前冻结；
- Workflow、Prompt、Profile、Tool schemas、Authority policy 和 Extensions 均有可恢复 snapshot/immutable URI 与 hash；
- model/provider 和可观察 generation settings 已记录；
- Runtime、event schema 和 Closure contract identity 已记录；
- Run Evidence Bundle 的 manifest hash 与冻结版本一致。

以下任一情况产生 `blocked`：

- 用当前 Prompt/Profile/Workflow 解释旧 Run；
- 只有名称，没有定义版本或内容；
- 只有 hash，但内容已不可恢复；
- offered tool surface 不明；
- model/provider identity 缺失。

### Gate 2: Execution Closure

问题：Run 的关键执行事实能否从 Tape 重建？

`pass` 条件：

- Session、Turn、Run、Actor identity 可连接；
- 关键 event ordering、causation 和 parent relation 可重建；
- 每个审计范围内的 tool call 都能连接 requested → Authority decision → execution/terminal；
- Run terminal claim 能被指定版本的 Closure interpreter 证明；
- 没有 invalid trace、身份冲突或未声明的 Tape 缺口。

`run.completed` 只证明执行闭合，不证明 Artifact 可用或 Outcome 良好。

### Gate 3: Artifact Closure

问题：实验相关的状态变化和 Artifact lineage 能否重建？

`pass` 条件：

- 每个声明产物都有 artifact ID、URI、revision/hash 和 producer Run/Actor；
- 当前验证需要读取内容时，bytes 可获得且 revision 匹配；
- 声明 reuse 时，consumer Run 使用显式 artifact ID，且读取执行完成；
- changed/removed revision 不被误当作原始产物；
- 预期无 Artifact 的任务以 `not-applicable` 记录实验设计理由。

隐式 `current` 读取不能单独证明 precise consumer lineage。

### Gate 4: Outcome Closure

问题：结果是否按运行前定义的方式被评价？

`pass` 条件：

- rubric 和 missing-data rule 在 Run 前冻结；
- evaluator identity/version 或 human reviewer 已记录；
- Outcome Record 与正确 task/Run/Artifact 相连；
- score、label、abstention 和 missingness 均保留；
- 评价所依据的 evidence references 可定位；
- 评价 revision 不覆盖历史。

Artifact 被创建、读取或交付都不能替代 Outcome Record。

### Gate 5: Comparison Closure

问题：baseline 与 intervention 是否足以支持预先声明的比较？

`pass` 条件：

- baseline、intervention 和目标 metric 在执行前登记；
- discovery/held-out split、assignment 和 exclusion rule 已冻结；
- 除 intervention 外的关键 Prior 可比较，差异被显式记录；
- 缺失样本、失败 Run、人工介入和重跑按预设规则处理；
- 比较使用冻结的 rubric/evaluator protocol；
- 候选 Pattern 在 discovery 后冻结，不能根据 held-out 结果改写。

单个 Run 可以通过前四个 gate，但 Comparison Closure 必然是 `blocked`，因此不能进入因果或 learning claim。

## 6. Canary Protocol

Canary 的目标是验证证据闭环，不是发现 Pattern 或比较能力强弱。

### Canary A: Structural closure

运行 1 个预先登记的 Research Run，验证：

```text
Manifest
   ↓
Run / Tape / Authority
   ↓
Artifact producer + explicit consumer
   ↓
Outcome Record
```

验收：Prior、Execution、Artifact、Outcome 四个 gate 为 `pass`。Comparison 必须明确记录为 `blocked`，不能省略。

### Canary B: Comparison closure

运行 2 个预先登记、只在 intervention 上不同的 Run，验证：

- assignment 在执行前冻结；
- 两个 Run 都能连接到各自 manifest/task identity；
- outcome protocol 相同；
- failure、missingness、人工介入与重跑没有被隐藏；
- 五个 gate 均为 `pass`。

Canary B 通过只说明“这组证据可以比较”，不说明 intervention 有效。

### Canary stop conditions

出现以下任一情况，停止扩大样本并输出 Evidence Gap Report：

- required evidence 只能通过当前配置事后推断；
- manifest 与 Run 无稳定 join key；
- Artifact producer/consumer 指向不唯一；
- evaluator 无法定位其 rubric 或输入证据；
- baseline/intervention 在冻结后发生未登记变化；
- secret 或 hidden reasoning 被要求作为通过条件。

## 7. Evidence Gap Report

每轮 Canary 必须输出一份报告，即使全部 gate 通过。

报告首先记录 experiment/manifest/Run identity、五个 gate 的 verdict 与 evidence references，然后包含零个或多个 gap entry。全部通过时 gap 列表为空，不能省略 gate verdict。

每个 gap entry 的最小字段：

| Field | 内容 |
|---|---|
| Evidence | 缺失或冲突的具体证据 |
| Status | `missing` 或 `invalid` |
| Expected source | Manifest、Tape、Authority、Workspace 或 Outcome |
| Affected gate | Prior / Execution / Artifact / Outcome / Comparison |
| Impact | 哪些声明因此被禁止 |
| Workaround | 是否能在实验层可靠获得 |
| Runtime change needed | `no`、`unknown` 或附带证据的 `yes` |
| Minimal addition | 仅在确需 Runtime 修改时描述最小事实 |

示例：

```json
{
  "evidence": "offered_tool_schema_hash",
  "status": "missing",
  "expected_source": "experiment_manifest",
  "affected_gate": "prior",
  "impact": "cannot_classify_pattern_as_emergent",
  "workaround": "snapshot schemas before run",
  "runtime_change_needed": "no"
}
```

## 8. Runtime evidence addition gate

Canary 发现缺口后，默认先在实验层解决。只有同时满足以下条件，才评估 Runtime 修改：

1. 缺口实际阻塞了一个 required gate；
2. 证据无法在 Run 前后由实验层可靠采集和连接；
3. 该事实属于 Runtime 自己发生的行为，而不是评价者结论；
4. 多个 Canary 重复出现同一缺口；
5. 可以只增加最小 durable fact，不引入新的综合状态；
6. 有测试证明记录、重放、缺失和兼容行为。

即使满足这些条件，也不得把以下内容加入 Runtime evidence：

- hidden chain of thought；
- 事后生成的行为动机；
- `artifact.useful=true` 一类混合存在性、正确性和价值的字段；
- 自动扩大 Authority 的 learned policy。

## 9. Audit readiness decision

```text
if any applicable gate == invalid:
    experiment = invalid

else if any applicable gate == blocked:
    experiment = not-audit-ready

else:
    experiment = audit-ready
```

`audit-ready` 只表示证据完整，绝不等于：

- Pattern 已被发现；
- Pattern 是 emergent；
- intervention 有效；
- 能力可以 promotion。

这些结论必须依次经过：

```text
Observation
    ↓
Hypothesis freeze
    ↓
Held-out intervention
    ↓
Measured improvement
    ↓
Promotion review
```

## 10. v0 完成条件

Auditability Completeness Protocol v0 在以下条件满足后结束：

1. Canary A 完成并产出四个 gate 的证据与 Comparison `blocked` 记录；
2. Canary B 完成并对五个 gate 给出 verdict；
3. Evidence Gap Report 明确区分实验层缺口与 Runtime 缺口；
4. 所有 `missing` 都有 impact，没有被推断值覆盖；
5. 未新增任何没有 Canary 消费证据的 Runtime 机制。

完成 v0 后，才定义正式 Cohort 的任务集、样本规模和 Pattern Observation 格式。

截至 2026-08-03，Canary A v2 与 Canary B v2 已满足以上结构性完成条件。这里的完成只证明单 Run 可解释且 paired evidence 可比较；它不证明 intervention 有效，也不授权 Experience Audit、Learning 或 Promotion claim。
