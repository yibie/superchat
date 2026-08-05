# Experience Auditability Inventory v0

- 状态：`partial`
- 日期：2026-08-03
- 范围：盘点现有 Runtime 历史是否足以支持 Experience Audit；本文不定义学习器，也不修改 Runtime。

## 结论

Superchat 当前保存的历史足以重建一部分执行事实、Authority 决策和 Artifact producer lineage，但不足以把重复行为可靠地分类为 `forced`、`suggested` 或 `emergent`，也不足以证明某个结构改善了结果质量。

因此，现有历史只适合生成带证据边界的 **Pattern Observation**，不适合生成 Candidate Capability，更不能支持 promotion。

核心判断：

| 问题 | 结论 | 置信度 |
|---|---|---|
| Run、Tool、Artifact 的事实能否重建？ | 大部分可以 | high |
| 当时的 immutable execution envelope 能否重建？ | 不可以，仅有部分字段 | high |
| Artifact producer lineage 能否重建？ | 可以 | high |
| Artifact consumer lineage 能否重建？ | 部分可以 | medium |
| 效率变化能否测量？ | 部分可以 | medium |
| 质量改善能否测量？ | 通常不可以 | high |
| 历史 Pattern 能否证明为 emergent？ | 不可以 | high |

这意味着当前最严格、也最诚实的结论是：

> Tape 中存在可供审计的行为证据，但历史 Prior 和 Outcome 不完整；任何无法排除设计者注入的 Pattern 都必须保持 `unknown`。

## 边界与零假设

Experience Audit 的输入不是单独的 Tape，而是：

```text
Experience = (Tape, Designer Prior Envelope, Outcome Evidence)
```

本阶段采用零假设：

> 在排除设计者预先注入的结构，并控制 Runtime 环境差异之后，Tape 中不存在能够在 held-out 任务上产生可测改进的重复结构。

本文只回答证据是否存在，不执行以下工作：

- 不做 sequence mining 或 Pattern Detection；
- 不创建 `experience-audit.el`；
- 不增加 Tape event、Projection、数据库表或 Runtime Contract；
- 不把 Observation 提升为 Workflow、Memory 或 Policy；
- 不用当前配置反向解释历史 Run。

## 评级规则

可用性：

| 评级 | 含义 |
|---|---|
| `yes` | 历史记录中存在直接、可回放的证据 |
| `partial` | 只能覆盖部分路径，或需要受约束的推断 |
| `derived` | 可由当前 Projection/Closure 重新计算，但不是当时持久化的决定 |
| `no` | 当前历史没有足够证据 |

可靠性：

| 评级 | 含义 |
|---|---|
| `high` | 直接来自稳定 ID、required payload 或明确的终态事件 |
| `medium` | 可连接，但存在隐式目标、缺失版本或解释依赖 |
| `low` | 依赖启发式或当前配置，不应用于 hard claim |

## 1. Tape 与执行身份

Tape 已经提供可靠的事实底座，但它不是完整的执行快照。

| Evidence | Exists | Source | Reliability | 限制 |
|---|---|---|---|---|
| Event identity | yes | Tape envelope / metadata | high | 无 |
| Session / Turn / Run identity | yes | Tape envelope | high | 无 |
| Actor / parent Run / correlation | yes | Tape envelope | high | 仅在调用上下文提供时存在 |
| Event ordering / recorded time | yes | Tape + DB | high | 可用于近似耗时，不等同于 provider latency |
| Event schema | yes | event metadata | high | 表示事件 schema，不表示完整 Runtime 版本 |
| Payload integrity | partial | inline payload 或 omitted hash/size | high | 大 payload 可能只保留 hash/size，不能重建内容 |
| Runtime semantic version at execution | no | — | — | Closure 的 semantic environment 是当前解释器信息，不是 per-Run snapshot |

允许的结论：某个事件在某个 Run 中发生、何时记录、与哪些 Actor/Run 有因果或父子关系。

禁止的结论：仅凭 event schema 或当前 Closure 版本，断言历史 Run 使用了相同的 Runtime semantics。

## 2. Designer Prior / Execution Prior

这是当前最主要的缺口。`intent.received` 保存了 intent 的 content hash/size、Capability、Topology、Authority policy 名称、Surface 和 Workspace 等信息，但没有保存实际提交给模型的完整 execution envelope。

| Evidence | Exists | Source | Reliability | 审计结论 |
|---|---|---|---|---|
| Run identity | yes | Runtime / Tape | high | 可作为 cohort join key |
| Actor identity | yes | Runtime / Tape | high | 可区分主 Agent 与 Subagent |
| Profile ID | partial | Run envelope / events | high | 有 Profile 时可见；没有定义版本或内容 hash |
| Capability | yes | `intent.received`, `run.started` | high | 只能说明声明值，不是完整行为约束 |
| Topology | yes | `intent.received` | high | 不等于 immutable Workflow definition |
| Surface | yes | `intent.received` | high | 仅是 surface 名称/类别，不是完整 tool surface |
| Authority policy name | yes | `intent.received` | high | policy version 只在相关 tool/approval 事件中出现 |
| Authority policy version per Run | partial | Authority events | high | 无 tool/approval 事件时无法证明该 Run 的版本 |
| User intent hash | yes | `intent.received` | high | 只覆盖 intent content，不是最终 composed prompt |
| Final prompt hash / snapshot | no | — | — | 无法排除 prompt 注入的结构 |
| System prompt hash / snapshot | no | — | — | 无法区分 forced 与 emergent |
| Context file list / content hash | no | — | — | origin URI 不能替代实际 context snapshot |
| Profile definition version / hash | no | — | — | 当前 Profile 不能用于解释旧 Run |
| Model / provider / model version | no | — | — | 无法控制模型差异 |
| Full offered tool surface | no | — | — | 只知道实际调用；Extension 事件只覆盖已激活贡献 |
| Tool schema / definition hash | no | — | — | 无法判断工具语义是否改变 |
| Extension definition hash | no | — | — | 只能部分看到 activation 名称与贡献 |
| Workflow definition / hash / version | no | — | — | correlation 或 step 名称不能替代定义快照 |
| Runtime/package commit | no | — | — | 无法严格控制实现版本 |

Research Workflow 的 durable manifest 提供 `query`、Workspace、root 和 Authority policy 等运行输入，但不包含角色定义、stage prompt、Workflow hash、模型、Profile 或 tool surface，因此不能作为完整 Prior Envelope。

### Prior 判定

```text
Result: partial

Available:
  ✓ Run / Actor / Profile identity（Profile 存在时）
  ✓ Capability / Topology / Surface
  ✓ Authority policy name
  ✓ Intent content hash

Missing:
  ✗ Final prompt / system prompt
  ✗ Workflow definition hash
  ✗ Profile definition version
  ✗ Model / provider version
  ✗ Complete tool surface and schemas
  ✗ Runtime implementation version

Consequence:
  Historical runs cannot prove emergent classification.
```

## 3. Authority 与动作证据

Authority 提供了目前最完整的行为证据链之一。

| Evidence | Exists | Source | Reliability | 限制 |
|---|---|---|---|---|
| Tool requested | yes | `tool.requested` | high | 只覆盖实际请求的工具 |
| Tool name / execution ID | yes | Authority events | high | 无 |
| Request commitment | yes | request hash | high | hash 提交参数，但不能恢复参数内容 |
| Declared effects | yes | Authority events | high | process/external 等敏感 scope 可能被 hash 化 |
| Policy / version / decision | yes | Authority events | high | 仅在发生相应事件时存在 |
| Approval requested / decided | yes | approval events | high | 无 |
| Execution started / terminal | yes | tool events | high | 无 |
| Tool result hash / size | yes | `tool.completed` | high | 不保存 result content |
| Failure status | yes | `tool.failed` | medium | 失败分类较粗，不能证明语义 failure class |
| Retry / fallback relation | no | — | — | 重复调用可见，但其恢复语义不是 durable fact |

可以观察“工具 X 失败后工具 Y 被调用且 Run 完成”，但在没有显式 failure class、fallback link 和受控对照时，只能写成相关性 Observation：

```text
X failure was followed by Y and a completed Run.
```

不能写成：

```text
Y is the learned recovery policy for failure class X.
```

## 4. Artifact provenance 与 lineage

Workspace 是当前最适合启动 Experience Audit 的证据面。

| Evidence | Exists | Source | Reliability | 限制 |
|---|---|---|---|---|
| Artifact ID / URI / type / adapter / role | yes | Workspace events | high | role 只是粗粒度语义 |
| Revision / content hash | yes | Workspace events | high | 可验证当前 bytes，但不证明业务正确性 |
| Producer Run / Actor / Profile | yes | event envelope | high | 依赖发布时存在 Runtime context |
| Added / changed / removed history | yes | Workspace events | high | 无 |
| Durable generated file | yes | `workspace_publish` + `file://` | high | 文件仍可能被外部删除或篡改 |
| Current availability / revision match | derived | Workspace Projection / adapter | high | 是读取时判断，不是历史质量结论 |
| Explicit consumer Run | partial | `workspace_read` Authority effects | medium | 仅显式 `artifact_id` 可直接连接 |
| Implicit `current` consumer target | partial | Workspace state + effects | low | 需要推断，不能作为 hard lineage |
| Successful read | partial | `tool.completed` | medium | 证明调用完成，不证明语义使用或帮助 |
| Artifact delivery decision event | no | — | — | 当前决定由 Closure + Workspace 状态派生 |
| Semantic purpose / usefulness | no | — | — | 不能从 read 或 role 自动推断 |

### Artifact 判定

```text
Producer lineage: yes
Consumer lineage: partial
Semantic reuse: no
Quality contribution: no
```

因此，现有历史可以回答“谁生产了什么、哪个明确读取动作访问了它”，但不能回答“它为什么被使用、是否改善了下游结果”。

## 5. Outcome evidence

Event、Closure 和 Outcome 必须分开：

```text
artifact.read                 = event
Run proved completed          = closure judgment
artifact improved review      = outcome claim
```

### 自动可观测

| Metric / Label | Exists | Source | Reliability | 说明 |
|---|---|---|---|---|
| Run terminal status | yes | Run terminal events | high | completed / failed / cancelled / timed-out 等 |
| Turn terminal status | yes | Turn events | high | 无 |
| Tool / execution count | yes | Authority events | high | 可按 Run 聚合 |
| Tool failure / denial / block | yes | Authority events | high | 失败语义较粗 |
| Approval count / decision | yes | Authority events | high | 无 |
| Artifact publish/change/remove | yes | Workspace events | high | 无 |
| Approximate Run / tool latency | partial | event `recorded_at` | medium | 不是 provider 原生 latency |
| Retry count | partial | 重复 execution 序列 | low | 无显式 retry relation |
| Token usage | no | — | — | 当前 Tape 无记录 |
| Provider/model request count | no | — | — | 缺少 transport-level durable evidence |
| Time to first token | no | — | — | 当前 UI annotation 不是 durable evidence |

### 派生判断

以下结果可以由当前解释器重新计算，但不是当时持久化的 Outcome：

- Closure verdict；
- Workflow resume action；
- Subagent handoff action；
- Artifact delivery decision；
- Research Workflow 的 advance / stop / manual-review / reject。

它们适合验证“当前 Contract 如何解释历史”，不适合证明“历史运行时当时使用了同一 Contract”。

### 需要评价者的结果

| Outcome | Exists | 审计结论 |
|---|---|---|
| Structured reviewer acceptance | no | 需要未来 cohort 明确定义 label |
| Evidence completeness | no | 不能从 Artifact 存在性推断 |
| Insight / report quality | no | 必须由 human/evaluator 评价 |
| Contradiction resolved | no | 当前没有结构化标签 |
| Rework caused by prior artifact | no | 缺少显式因果 link |
| Downstream improvement | no | 需要 baseline / intervention / held-out 对照 |

### Outcome 判定

```text
Efficiency proxies: partial
Runtime correctness: yes
Task quality: no
Causal improvement: no
```

## 6. Pattern provenance 的可判定性

未来 Observation 可以使用四种来源分类，但当前历史不能完整支持它们：

| Classification | 所需证据 | 当前可判定性 |
|---|---|---|
| `forced` | Pattern 被 immutable Workflow、Prompt 或 hard Policy 明确要求 | partial |
| `suggested` | Pattern 被 soft instruction、Profile 或命名暗示 | partial |
| `emergent` | 完整 Prior 中没有要求，且跨任务重复出现 | no |
| `unknown` | Prior 不完整、证据冲突或样本不足 | yes |

判定规则：

1. 缺少相关 Prompt、Workflow、Profile 或 Tool Surface 任一关键 Prior 时，不得标记 `emergent`。
2. 当前配置不能补写历史 Prior。
3. Workflow topology 或 step 名称的重复只能标记 `forced`/`suggested`，不能作为学习信号。
4. 找不到注入来源不等于证明没有注入来源。
5. `unknown` 是有效结论，不是待自动消除的错误状态。

## 7. Experience Audit gate

| Gate | 当前状态 | 能否开始正式 Audit |
|---|---|---|
| Durable runtime facts | pass | 是 |
| Artifact producer provenance | pass | 是 |
| Explicit artifact consumer provenance | partial | 仅限受约束 Observation |
| Immutable Designer Prior | fail | 否，不能做 emergent claim |
| Comparable execution environment | fail | 否，不能做跨历史因果比较 |
| Structured quality outcome | fail | 否，不能证明质量改善 |
| Held-out intervention protocol | not defined | 否，不能 promotion |

总门禁：

```text
Historical audit:
  allowed  → descriptive observations with explicit unknowns
  blocked  → emergent classification, causal claims, promotion

Prospective cohort:
  allowed  → after Prior and Outcome manifests are frozen
```

## 8. Experience Audit Cohort v0

下一步不是扩 Runtime，而是在实验目录中定义一个 prospective cohort manifest。Manifest 应固定并保存：

| Category | Required field |
|---|---|
| Task | task ID、task family、输入 hash、discovery/held-out split |
| Workflow | definition file、content hash、stage order、stage prompt hashes |
| Prompt | system prompt hash、composed prompt hash、context manifest hash |
| Profile | profile ID、definition version/hash |
| Model | provider、model ID/version、关键 transport settings |
| Tools | offered tool names、schemas/definition hashes |
| Authority | policy name、policy version |
| Extensions | activated set、definition hashes |
| Runtime | package version 或 commit、event schema、Closure contract |
| Outcome | reviewer protocol、labels、cost/latency metrics、missing-data rule |
| Experiment | baseline、intervention、freeze time、sample size、exclusion rule |

这个 manifest 首先应作为实验输入保存在 Runtime 外部。只有真实 cohort 证明某个字段必须被每个 Run 自动记录时，才评估是否把最小 evidence 加入 Runtime。

建议最小实验顺序：

1. 定义 cohort manifest 和 outcome rubric；
2. 冻结 discovery 与 held-out task split；
3. 运行 20–30 个受控任务；
4. 只输出 Pattern Observation，包含 occurrences、task IDs、event ranges、counter-examples 和 provenance status；
5. 冻结候选假设后，在 held-out 上做 baseline / intervention；
6. 只有出现可重复、代价可接受的改善，才讨论 promotion。

## 9. 允许与禁止的声明

### 当前允许

- “Artifact A 由 Run 12 发布，revision 为 H。”
- “Run 19 显式请求读取 Artifact A，读取工具完成。”
- “工具 X 失败后，工具 Y 被调用，随后 Run 完成。”
- “在当前 Closure/Workflow 解释器下，这段 Tape 的判断是 manual-review。”
- “该 Pattern 的 Prior provenance 不完整，因此状态为 unknown。”

### 当前禁止

- “Artifact A 被重复读取，所以它值得进入长期 Memory。”
- “Y 是 Agent 学会的 recovery policy。”
- “cross-check 是 emergent primitive。”
- “该 Artifact 提升了 Reviewer 质量。”
- “当前 Profile/Workflow 定义与历史 Run 使用的版本相同。”
- “当前 Projection 的判断就是历史 Runtime 当时的决定。”

## 10. Evidence source index

本盘点以当前代码为事实来源：

- Tape envelope、payload 和 replay：[`superchat-tape.el`](../../superchat-tape.el)
- SQLite 物理存储与 schema：[`superchat-db.el`](../../superchat-db.el)
- Run lifecycle 与 Runtime events：[`superchat-runtime.el`](../../superchat-runtime.el)
- Authority request/approval/execution chain：[`superchat-authority.el`](../../superchat-authority.el)
- Closure 当前语义环境与判断：[`superchat-closure.el`](../../superchat-closure.el)
- Workspace Artifact events、publish/read/delivery：[`superchat-workspace.el`](../../superchat-workspace.el)
- Workflow 与 Research manifest/recovery：[`superchat-workflow.el`](../../superchat-workflow.el)
- Extension activation evidence：[`superchat-extension.el`](../../superchat-extension.el)
- Memory-specific review fields：[`superchat-memory.el`](../../superchat-memory.el)

相关边界说明：

- [`architecture.md`](../architecture.md)
- [`agent-architecture.md`](../agent-architecture.md)
- [`closure-projection-contract-v0.md`](../design/closure-projection-contract-v0.md)

## 决策记录

Experience Audit v0 暂不实现。先用 prospective cohort manifest 补齐实验所需的 Prior 和 Outcome 控制，再决定 Runtime 是否需要新增最小证据。

这保持了现有边界：Tape 记录事实，Projection/Closure 解释事实，Audit 产生 Observation，实验产生评价；任何 Promotion 都发生在这些步骤之后。
