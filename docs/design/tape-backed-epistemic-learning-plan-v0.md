# Tape-backed Epistemic Learning：实施计划 v0

状态：P1、P1.1、P2、P3a 已通过 canonical ERT；V1 离线 scorer 已通过三轴复审；下一步是 V2a local/fake-provider harness，provider cohort 未获授权
日期：2026-08-10
范围：冻结模型权重的 Agent Runtime
依据：[ADR 0001](../adr/0001-tape-backed-epistemic-self-learning.md)、当前源码、现有实验与 2026-08-10 ERT 基线

## 一句话结论

Superchat 不需要先造一个庞大的“Learning Layer”。最短可行路径是：先改 Runtime，让它精确记录一次决策真正看见了什么、做了什么；再用不会泄漏答案的实验攻击和验收这项能力；最后才从后验 Outcome 追加可质疑的 Pattern Claim，并用一个窄小、先验中没有答案的练习检验它能否改变未来行为。

## 1. 已经确定的问题模型

我们要实现的不是“成功以后，倒过来说 Agent 当时早就知道正确答案”，而是下面这条可审计链：

```text
Decision Snapshot（先验）
  → Execution / Action
  → Outcome（后验）
  → Attribution Claim（哪一步可能有贡献）
  → Pattern Claim（在什么条件下可能适用）
  → Boundary-targeted Exercise（专门练适用边界）
  → Held-out Evaluation
  → Human-gated Promotion / Rejection / Revocation
  → future View 改变
```

其中：

- Outcome 只能在行动发生后追加，不能回填进之前的 Decision Snapshot。
- 一次成功不是整个轨迹每一步都正确，也不是 Pattern 的普遍真值。
- Pattern、因果归因、适用范围都是 Claim，必须保留来源、反例和缺失证据。
- Graph 负责表达“哪些条件、经验和 Claim 有关系”，不负责把相似、连通或中心性解释成真理。
- Exercise 负责区分竞争 Claim 和寻找适用边界；Evaluator 的答案必须封存在 Actor 的 View 之外。
- 只有经过 held-out 对照并实际改变未来 View/Action 的外部状态，才有资格称为本项目里的 Learning。

## 2. 当前代码已经具备什么

| 能力 | 当前所有者 | 可以直接复用的部分 | 当前不能证明的部分 |
|---|---|---|---|
| Run/Turn 生命周期 | `superchat-runtime.el` | 创建、完成、取消、terminal-before-delivery | 任务是否成功、经验是否有效 |
| 不可覆盖的历史 | `superchat-tape.el` + `superchat-db.el` | 稳定事件身份、因果字段、追加、重放、脱敏；opt-in Run 可保存每次 provider attempt 的 prepared envelope；固定同步工具可保存 exact Action/Result/Error | provider-specific HTTP wire bytes、异步/动态工具轨迹、任务 Outcome 与因果贡献 |
| 工具副作用 | `superchat-authority.el` | validate → policy → approval → execute → Tape | 工具调用是否对最终 Outcome 有因果贡献 |
| Runtime 完备性 | `superchat-closure.el` | 纯解释 Tape，判断生命周期证据是否闭合 | 语义正确、任务成功、已经学习 |
| 派生读模型 | `superchat-projection.el` | reducer、replay、checkpoint | 现成的 Applicability Graph |
| Claim/Feedback | `superchat-claims.el` | statement、scope、basis、supersession、support/challenge/correction | Pattern 的系统评估和 Promotion 状态机 |
| 当前上下文 | `superchat-tape-view.el` | 从 Tape 临时选择和组合 View | 学习候选的 task-specific View |
| Anchor | `superchat-compact.el`、branch anchor | 追加式提炼/回溯标记 | 统一、typed 的 graph index 或可靠 handoff state |
| Artifact | `superchat-workspace.el` | revision、provenance、跨 Run 交付 | 自动说明某 revision 导致成功 |

结论：底座已经有了。缺的不是另一套数据库或记忆系统，而是三条窄缝：

1. 先验证据：模型调用前，Actor 实际看见的完整输入没有进入 durable Tape。
2. 后验证据：任务 Outcome 与第一次关键 Action 没有精确记录。
3. 学习判定：没有用 sealed exercise 将 Pattern Claim 与 withheld/sham/contradictory 对照分开。

## 3. 真实执行链与证据断点

当前主链是：

```text
surface input
  → superchat-core-prepare-turn
  → prompt hooks / superchat-view-assemble
  → dispatcher 构造 Runtime Request
  → superchat-runtime-submit
  → superchat--llm-generate-answer
  → llm.el provider
  ↔ Authority tool execution
  → Runtime terminal events
  → Closure guard
  → delivery
```

已核实的断点：

1. `superchat-prompt-hook--conversation-history` 已把完整 `:context-envelope` 放进 prepared Turn state。
2. dispatcher 构造的 Request 没有携带这份 Turn state；Runtime 又创建自己的 Turn，因此 durable Runtime 看不到生成 prompt 的 View receipt。
3. `superchat--llm-generate-answer` 在实际 tools、wrappers 和 `real-prompt` 已确定后才真正知道 provider 会看到什么，但目前 observer 只报告 request handle/terminal response。
4. `intent.received`、`response.completed` 和 Authority 工具事件主要保存 size/hash/count；Tape summary 最多 500 字，不能重建长输入、第一次输出、精确 tool args/result/error。
5. `run.completed` 证明生命周期完成，不等于任务成功。

因此，第一条产品改动应该接在现有 LLM observer → Runtime → Tape 路径上，而不是让 `superchat-llm.el` 直接写数据库或另造一套 trace store。

## 4. 架构决策

### 4.1 保持现有所有权

- LLM adapter 只观察并上报 plain data，不持久化。
- Runtime 是 Decision Snapshot 和 first output Runtime 证据的唯一写入者。
- Authority 继续是所有 model-invoked tool 的唯一执行缝。
- Closure 仍只判断 Runtime 生命周期，不吸收“任务成功”或“学习成功”语义。
- Projection 从 Tape 重放 Applicability Graph；Tape 本身不变成 graph engine。
- Evaluator 在 Run terminal 后追加 Outcome；它不能改写先前事件。

### 4.2 第一版是 opt-in evidence contract

普通聊天不需要立刻承担完整 trajectory 的存储成本。第一条纵切使用显式 Request 合同，例如：

```elisp
(:evidence-contract "epistemic-learning-v0")
```

只有进入受控实验的 opt-in Run 才要求精确 payload 且 fail closed。P1 先交付 text-only/tool-free 路径；P1.1 再加入固定同步工具。两者都受 Tape 当前 16 KiB payload 上限约束；超限时失败，不降级成只有哈希的“伪精确”证据。

### 4.3 新学习事件对 Runtime Closure 是已知但无关

Closure 遇到未知 event type 会返回 `unknown`，而 terminal guard 会重放同 Session 的事件。因此每个新事件必须同时被明确加入 Closure 的 known-irrelevant 列表，否则后续正常 Run 也可能无法封口。

这不是说事件不重要，而是说：

- Runtime Closure 只回答“这次执行是否闭合”；
- Epistemic Evaluation 另行回答“这个 Outcome/Claim 是否有证据”。

两种证明不能混在一起。

### 4.4 Graph 是 Projection，不是新数据库

v0 使用 Tape 中的事件作为 node，以一个窄的 relation event 表达现有 Claim/Feedback 合同不能表达的边。调用者显式给出 Session 集合，Projection 按 event type/scope 纯重放；当前 normalized Tape replay 不保留 topic，因此不能宣称按 topic 选择。实现不做 SQLite schema migration，不引入 graph/vector database，也不先做物化表。

### 4.5 v0 不引入 graph Anchor

当前 Anchor 有 compact、branch 等多种语义，latest-anchor 选择也没有 typed isolation。过早让它兼任图 checkpoint 会污染 `/expand` 和 Continuity。

v0 直接从 Tape 重放小规模 graph。只有 replay 成本被测量为问题后，才考虑 deterministic graph Anchor；届时也必须先实现 typed anchor query。

## 5. 最小事件合同

下面的前两个事件已在 P1/P1.1 中进入代码；后两个事件与读模型已在 P2 中进入代码。

### 5.1 `decision.snapshot.recorded`

发生时间：实际 prompt、system、messages、model/options/tool surface 已确定之后，provider 请求之前。每个 provider attempt 各有 ordinal。

当前持久化内容：

```elisp
(:evidence-contract "epistemic-learning-v0"
 :attempt 1
 :attempt-reason "initial"
 :provider "..."
 :model "..."
 :prepared-envelope-json "{...}"
 :prepared-envelope-size 1234
 :prepared-envelope-hash "sha256:...")
```

约束：

- JSON 是 Superchat 在每次 provider attempt 前准备的语义 envelope，不宣称等于 provider-specific HTTP wire bytes。
- envelope 包含当次 system prompt、messages、options、tool schema 和结构化 View receipt/disclosure；P1 的 `:tools` 为空，P1.1 允许固定同步工具。
- P1 只接受带有效 View envelope 的 text-only、tool-free Request；P1.1 仍拒绝 `:context-files`、已预包装的 `:prepared-tools`、request-local `:tool-wrapper`、动态/合成/异步工具，以及缺少默认 Authority wrapper 的工具请求。
- 不含 assignment arm、expected answer、rubric 或 held-out 标签；这些仍由实验构造负责封存。
- `:requires-payload t`；超过 Tape inline 上限时在 provider 调用前 fail closed，不降级成 hash-only。
- payload 标记为 `restricted`，但当前 SQLite 没有字段级加密；因此 v0 只用于明确 opt-in、已控制敏感输入的 Run。

### 5.2 `output.first.recorded`

发生时间：一次 tool-free 决策后的首个非空 assistant text，或无工具 prompt 意外收到的 `llm-tool-*` 协议错误；每个 attempt 只追加一次。协议错误只作为终止观察保存，P1 不据此重试，也不把它解释成已发生的工具调用。

后续或晚到输出不能覆盖它；晚到的旧 attempt 输出仍因果指向自己的 Decision Snapshot，也不能把 Run 当前因果头倒退。工具调用、tool result/error 和 post-error Action 不属于 P1 基础合同；P1.1 用 `tool.requested` → `tool.completed|tool.failed` 记录固定同步工具链。

### 5.3 `exercise.evaluated`

发生时间：Run 与 Turn 都已 durable terminal，具名 evaluator 完成检查之后。写入者必须显式调用；Runtime terminal 不会自动生成 Outcome。

最小内容：

```elisp
(:exercise-id "..."
 :scope "..."
 :run-id "..."
 :decision (:session-id "..." :event-id "evt_...")
 :terminal (:session-id "..." :event-id "evt_...")
 :evaluator (:id "..." :version 1)
 :evaluator-manifest-sha256 "sha256:..."
 :verdict "passed|failed|not-identifiable"
 :observations (...)
 :metrics (...)
 :evidence [(:session-id "..." :event-id "evt_...")])
```

这里记录的是“某 evaluator 对某 exercise 的观察”，不是 Pattern 在所有任务上都为真。manifest digest 只证明“这条记录声明了哪个摘要”，不能单独证明 manifest 已在 Run 前封存或 evaluator 值得信任。

### 5.4 `applicability.relation.recorded`

只补充 Claims/Feedback 不能直接表达的有向关系。最小关系集合：

- `applies-under`
- `fails-under`

`tested-by` 不单独持久化：Claim/Feedback 的 typed basis 已能指向 `exercise.evaluated`，重复写一条边会把同一份证据伪装成两份。

示例：

```elisp
(:type "applicability.relation.recorded"
 :schema 1
 :session-id "..."
 :actor-id "..."
 :correlation-id "experience-thread-..."
 :causation-id "evt_immediate_writer_parent"
 :topic "learning/applicability"
 :requires-payload t
 :payload
 (:relation "applies-under"
  :from (:session-id "..." :event-id "evt_pattern_claim")
  :to (:session-id "..." :event-id "evt_context_claim")
  :scope "opaque-tool-recovery/v0"
  :basis [(:session-id "..." :storage-id 123)]))
```

`causation-id` 只表示写入链，不冒充语义因果。跨 Session 引用必须带 `session-id`；不能只保存数据库本地的 `tape:N`。

### 5.5 Promotion 事件

在 held-out Evaluation 通过以后才引入：

- `promotion.decided`
- `promotion.rejected`
- `promotion.revoked`

Promotion 是显式的人类决策。撤销通过追加 `promotion.revoked` 实现，不删除旧 Candidate、Claim、Evaluation 或 Promotion。

## 6. Applicability Graph v0

### 6.1 Node 复用

不创建一套重复实体：

- Fact：`session-id + storage-id`
- Claim / Feedback / Outcome / Exercise：逻辑 `event-id`
- Artifact：`workspace-id + artifact-id + revision + producing-event-id`
- Candidate：优先是带明确 kind/scope/basis 的 Claim；需要完整正文时引用 Workspace revision
- Promotion：Promotion event-id

### 6.2 Graph 语义

它是 directed typed multigraph：同一对节点可以有多条来源不同、结论冲突的边。Projection 必须保留冲突，不做“最新一条覆盖旧结论”。

查询只允许返回三类结果：

- 有 scope 匹配的证据断言；
- 有冲突，需要 Exercise；
- 证据缺失，不注入 Candidate。

P2 不解释 Promotion；“证据存在且已获人工晋升”到 P3 才成立。

不存在“图连通，所以 Pattern 为真”的第四种结果。

### 6.3 写入边界

`superchat-tape-append-event` 接受任意 payload，但 DB 对无法 JSON 编码的 metadata 可能退化为 `{}`。因此 P2 graph writer 在 append 前验证：

- plist shape 与 key 唯一；
- typed reference 完整；
- relation 属于闭集；
- payload 可 JSON round-trip；
- basis 指向真实可重放事件。

Tape 不负责全图一致性；Projection 将 dangling reference 暴露为 missing evidence。

## 7. 分阶段实施

产品改动是主线，实验是对应能力的验收装置。实施顺序是：

```text
P1 Decision Evidence 产品纵切
  → P2 Epistemic records + Graph Projection
  → P1.1 Tool-bearing Decision Evidence
  → V1 tool-bearing measurement/scorer 验收
  → P3a Human-gated Promotion + scoped Experience View
  → V2a local/fake-provider harness closure
  → T1 explicitly authorized transport preflight
  → V2 held-out 因果实验
```

每个产品阶段都必须留下最小 ERT；每个验证阶段负责攻击刚完成的能力。验证失败就回到对应产品接缝修正，而不是继续堆更多实验。

### V1（P1.1 后的下一步）：修复 tool-bearing measurement/scorer

状态：2026-08-10 已完成离线 scorer remediation，并通过 source-facts、
code-correctness、experimental-validity 三轴复审。该状态不授权 provider
cohort，也不表示已观察到 Semantic Use 或 Learning。

目标：用已交付的固定同步工具证据，让实验基础设施攻击 tool-bearing evidence 并拒绝已知假阳性。fixture 不以“修实验”代替产品实现；它不是 P1/P1.1 的验收门槛。

前置条件已经满足到固定同步工具边界；V1 已修复当前已知的
scorer/oracle/grammar 假阳性。是否运行 provider cohort 仍由
`later-authorization` 单独决定。

文件范围：

- 新建 `experiments/artifact-semantic-use-v0/revisions/v0.3/`
- 保持 rejected v0.2 原样冻结

工作：

1. 把 actor manifest 与 sealed evaluator/oracle manifest 分离。Actor 只拿 opaque assignment ID 和 visible inputs。
2. 在任何 `plist-get`、hash 或 join 前验证 record/envelope：偶数长度、keyword key、key 唯一、必需字段、字段类型。
3. 把 model-visible envelope 定义成闭合 schema；递归检查完整 envelope，而不是只重建五个已知字段扫描。
4. content/sham 的 prompt、messages、tool schema、Artifact 必须存在且非空；withheld 的 Artifact 必须在整个 visible envelope 中缺席。
5. 从 exact Artifact bytes 重新计算 length/SHA，并与 condition/family/revision schedule join。
6. 把 structure validation 纳入唯一 scorer closure；禁止 audit 与 scorer 分别读取不同全局 fixture。
7. 把每个攻击 fixture 接到进程退出码。

必须被拒绝的 fixture：

- 顶层或嵌套 `:answer-key`
- nil/missing visible input
- 顶层、嵌套、schedule duplicate key
- actual record 中的 BAD grammar Artifact
- Artifact bytes/hash/revision 不一致
- schedule/truth join 不一致
- baseline structure 本身失败

完成标准：

- 合法 fixture 全部通过；每个攻击 fixture 独立 fail closed。
- structure/content/oracle/first-action/control gates 由同一个 scorer 调用。
- source-facts、code-adversary、experimental-validity 三轴复审不再给出 false positive。
- 在 scorer 和攻击 fixture 全部 fail closed 前不运行 provider，不声称 Learning。

### P1：Decision Evidence 产品纵切

状态：2026-08-10 已实现并通过 ERT；该状态只表示产品证据接缝交付，不表示 self-learning 已交付。

目标：在受控 Run 中证明“provider 看见的东西”和“Tape 记录的先验”完全一致。

最小文件范围：

- `superchat-dispatcher.el`
- `superchat-llm.el`
- `superchat-runtime.el`
- `superchat-closure.el`
- 现有对应 ERT 文件

工作：

1. dispatcher 将 prepared Turn 的 context envelope 传入 Runtime Request。
2. 添加 opt-in `epistemic-learning-v0` evidence contract。
3. LLM adapter 从 `real-prompt` 读取实际消息和 tool surface；tool surface 非空或 accessor 失败时阻止 provider，否则在请求前上报 structured Decision Snapshot。
4. Runtime 同步追加 `decision.snapshot.recorded`；成功持久化后才允许 provider 请求。
5. Tape 对同一个结构化值执行敏感键检查、JSON 编码、size/hash；需要脱敏或超过 inline 上限时 fail closed。
6. LLM adapter 上报首个非空文本；若无工具 prompt 意外收到 `llm-tool-*` 协议错误，则记录该终止观察但不重试。Runtime 每个 attempt 只追加一次 `output.first.recorded`，并绑定同 attempt 的 snapshot 因果。
7. Closure 将两个新事件列为 known-irrelevant，但仍要求合法 Run/Turn 身份。

最小 ERT：

- snapshot 已持久化后 fake provider 才被调用；
- fake provider 收到的 prompt/system/messages/options 与 snapshot exact equal；
- actual prompt 带任何工具、accessor 失败、需要敏感键脱敏或 payload 超限时 provider 未被调用；
- first output 只记录一次；晚到旧输出不串因果也不倒退当前因果头；
- snapshot 不含 sealed oracle；
- 普通非 eligible Run 的工具行为不变；
- `run.completed` 不会自动生成 Outcome；
- 同 Session 后续 Run 的 Closure 仍可证明 terminal。

完成标准：这只证明可审计 evidence seam，不证明经验被使用或任务改善。

### P2：最小 Epistemic records 与纯 Graph Projection

状态：2026-08-10 已实现；聚焦 ERT 37/37、canonical ERT 674 项（657 pass、17 skip、0 unexpected），独立复审 ACCEPT。该状态只说明后验证据和条件关系可被安全追加/重放，不说明经验已改变未来 View。

目标：能追加 Outcome/关系，能从 Tape 回答“某 scope 下有哪些证据、冲突和缺口”。

默认实现：一个窄模块 `superchat-experience.el`，而不是通用 Learning framework。它只拥有：

- validated epistemic event writers；
- pure applicability reducer/query；
- 不拥有模型执行、自动评分、Authority 或 Promotion。

工作：

1. 追加 `exercise.evaluated`；只允许引用已 terminal 的 Run。
2. 复用 Claims/Feedback 写 Attribution/Pattern Claim、support/challenge/correction。
3. Claim 增加闭集 kind，legacy basis 保持兼容，新 basis 可使用跨 Session typed reference。
4. 追加只有 `applies-under` / `fails-under` 的窄 relation event；验证 JSON round-trip 和 typed refs。
5. 通过 `superchat-projection-replay` 对显式 Session 集合按 event type/scope 重放 graph。
6. missing/dangling/conflicting evidence 必须作为查询结果保留。
7. 所有新事件加入 Closure known-irrelevant。

最小 ERT：

- real SQLite append/replay 后 node/edge identity 不丢；
- 跨 Session ref 不会因相同 event ID 或 storage ID 混淆；
- correction/supersession 不删除旧 Claim；
- 两条相反 relation 同时存在；
- malformed/unencodable payload 在 append 前失败；
- Projection 从空库重放可重建相同结果。

完成标准：Graph 能表示证据状态，但还不会自动改变 View。

### P1.1：Tool-bearing Decision Evidence（V1/V2 前置）

目标：在不串 attempt、不遗漏 `llm.el` 独立 tool-results/multi-turn 字段的前提下，把工具调用纳入同一条先验/行动证据链。

已实现的窄合同只覆盖固定同步工具：

- 每个 attempt 深拷贝 prompt，并从 request-level base tools 重新建立 wrapper；后续 attempt 不会套用旧 wrapper。
- Snapshot 保存 interactions、tool-results、multi-turn、tool schema 和实际 `tool-choice`；Snapshot schema 与随后交给 provider 的 schema 不同则 fail closed。
- wrapper 绑定不可变的 attempt、snapshot、call ID、ordinal 和 named args；它按 tool name、schema 映射后的实际参数和未消费位置匹配 provider tool use，晚到旧 attempt 在 Authority 前被拦截。
- Runtime 拒绝 request-local `:tool-wrapper`；默认 Agent/Authority wrapper 不可用时，eligible 请求在 provider 前失败。
- exact Action/provenance/sensitive-argument validation 和 rewrite/normalize 等值检查发生在 effects、policy 与 approval 之前；参数被改变即转为 audit-incomplete。
- Authority 作出 policy decision 后追加 durable `tool.requested`；只有追加成功后才允许 UI render、approval、pre-tool hook 和工具副作用。
- `tool.completed` / `tool.failed` 保存 exact Result/Error。它们在信息时序上只能发生于工具返回或抛错之后；若持久化失败，Run 变为 `audit-incomplete`，原始结果/错误被固定占位替代，不能回传 provider 或 UI。
- result/filter/post-hook 分别使用隔离副本；compatibility hook 不能在 durable Result 后篡改 provider 可见结果。
- 工具参数 schema 只接受 `string`、`integer`、`number` 标量。`llm` 0.31.1 会把 JSON false/null 原地归一化为同一个 `nil`；P1.1 对这种已无法区分的显式值拒绝执行，不写伪 exact Action。
- `define_tool`、session synthesized tools、`llm-tool-async`、`:prepared-tools`、`:tool-wrapper` 和 context files 不在此合同内，均在对应副作用前拒绝；非 eligible 路径保持原行为。

最小要求：

1. 每个 provider attempt 使用隔离的 prompt/tool surface；旧响应不能读取新 attempt 的 wrapper。
2. Decision Snapshot 结构化记录 provider-visible interactions、tool schema，以及独立的 tool-results/multi-turn 数据。
3. Authority 收到不可变的 origin attempt；tool args/result/error 明确因果指向该 attempt 的 snapshot。
4. Snapshot/accessor/schema 失败时在 provider 前 fail closed；Action/provenance/敏感参数失败时在 policy、approval、render 和工具副作用前 fail closed；Result/Error 失败只能在副作用后转为 audit-incomplete。
5. 普通非 eligible 工具路径保持兼容。

完成标准：晚到旧响应不会串到新 attempt；固定同步工具的 exact args/results/error 可重放；独立攻击测试通过。异步和运行中扩展工具面需要新的版本/排空合同，不由 P1.1 暗中承诺。

### V2：第一条可归因的练习纵切

顺序修正：V2 必须在下面的 P3a 产品纵切和 V2a local harness 之后。
否则 Candidate 只能由实验代码手工塞入，既没有历史 Promotion，也无法证明
未来 View 是产品逻辑选择的。

目标：证明一个来自早先经验、经 Promotion 的窄 Candidate，在新 held-out Exercise 中改变了第一次 Action。

不使用现成 missing-argument tool recovery。当前 `superchat-llm.el` 已明确提示“修参数后重试”，withheld 组也有答案，无法区分 Runtime designer prior 与 learned Candidate。

第一条 synthetic read-only tool 练习使用 opaque error code，工具描述和错误文本不泄漏恢复策略：

```text
E-R17 → 先调用 refresh_handle，再用新 handle 重试一次
E-N04 → 不重试，报告终止
```

映射只能来自较早 Experience Thread 形成的 Candidate。工具必须无副作用并走 Authority。

最小数据集：两类边界各两个 held-out 实例，共 4 个 Exercise；每个 Exercise 交叉四路，共 16 个 assignment：

| Arm | Actor 可见内容 | 用途 |
|---|---|---|
| real | 正确 Candidate | 测适用行为 |
| withheld | Candidate 缺席 | 基线 |
| same-length sham | 等长等格式、只描述不会出现的 code | 控制“多一张卡片”和长度效应 |
| contradictory | 等长反向映射 | 检查内容依赖和 evaluator 拒错能力 |

每个 assignment 使用独立 Session/Workspace，冻结 model/provider/config/tool schema/budget/schedule。provider replacement retry 与 Candidate 规定的 tool retry 分开计数。

sealed evaluator 必须检查：

- exact actor-visible snapshot 中没有 arm/target/rubric；
- 第一次 post-error Action；
- exact tool name/args/result/error bytes；
- provider retry、tool retry、replacement count；
- eligible error 是否 refresh→retry 一次；
- terminal error 是否停止；
- tool count、成本和额外 Effect；
- Candidate 的 Experience lineage 和 Promotion ID。

任一发生即 `not-identifiable`：

- oracle 泄漏；
- first Action 被后续结果覆盖；
- exact bytes/hash/join 缺失；
- real 与 withheld/sham 没有预注册方向差异；
- contradictory 与 real 行为无内容依赖差异；
- real 对 terminal error 重试或产生额外 Effect；
- Candidate 只是本轮手工塞入，没有历史 lineage/Promotion；
- 相关测试被 skip。

完成标准：只能报告这些固定 exercise unit 的证据；不外推为普遍学习能力。

运行真实 cohort 前还有两个独立门：

1. V2a 用 deterministic local actor + fake transport 跑同一 16-assignment
   结构，只验证产品 Promotion/View、Decision Snapshot、scorer 与退出码闭合；
   不报告模型 Semantic Use。
2. T1 由用户另行明确授权一个 pinned provider/model/config 的只读、无秘密、
   有成本上限的 transport preflight。它只检查当前 transport path，不自动
   启动 cohort。

### P3a：Human-gated Promotion 进入未来 View（V2 前置）

目标：让通过 Evaluation 的 Candidate 以可撤销方式改变后续 task-specific View。

工作：

1. 在现有 `superchat-experience.el` 深化模块，不新增 Promotion framework：
   一个 host disposition writer 统一追加 promote/reject/revoke，只接受 `user`
   Actor，且不注册为模型工具。该 Actor 字段是 host contract，不是身份认证。
2. 一个 task-specific Experience View Interface 显式接收 Session IDs、scope
   和 Context Claim reference；不从自然语言或相似度猜 scope。
3. 只选择 active promoted、scope 精确匹配且存在有效 `applies-under`、没有
   `fails-under`/missing/conflict 的 Candidate；v0 出现多个 eligible
   Candidate 时直接 `blocked`。
4. View 返回 exact Candidate/Promotion/Context event refs、projection version、
   per-Session tape heads、selection policy/version 和 rendered prompt。
5. opt-in prompt adapter 把该 View 放入未来 Turn；Decision Snapshot 同时保存
   exact rendered prompt 与 Experience View receipt，严格先于 first Action。
6. missing/conflict 时不注入，只返回 Exercise Opportunity descriptor；View
   assembler 本身保持只读。
7. 撤销通过追加 event 生效，旧历史仍可重放。

约束：

- 不修改 Continuity 的 `:represented []` 合同。
- 不把 Candidate 伪装成普通对话历史或 compact Anchor。
- 不自动写 Skill，不直接修改 Memory，不修改任意工具 policy。
- 不改 DB schema；继续复用 Tape、Projection、Claims/Evaluation/Graph。
- 不默认接入普通聊天；只有显式 Experience selection spec 才启用。

完成标准：同一 future Exercise 的 Request/View 在 Promotion 前后可比较；
Promotion 后的 Decision Snapshot 能指向 exact Candidate 与 Promotion，
Revocation 后恢复空 View。模型行为差异留给 V2，不用 ERT 冒充。

### Deferred：只有被需求证明后才做的扩展

以下全部 deferred：

- deterministic graph Anchor / graph checkpoint；
- graph materialized table 或专用 graph DB；
- async tool 的 outstanding Action 排空与 post-terminal audit-gap 合同；
- `define_tool` / session synthesized tool 的版本化 next-attempt surface；
- entropy 驱动的 observation/exercise proposal；
- 自动 Candidate synthesis；
- 自动 Promotion；
- general ontology；
- 跨模型/跨 provider 普遍性结论；
- 权重训练或在线 fine-tuning。

升级触发条件：小图纯 replay 已有测量瓶颈、人工练习设计成为主要吞吐瓶颈，或已有至少一个通过 V2/P3 的真实纵切。

## 8. 第一阶段的文件落点

| 阶段 | 修改/新增 | 不修改 |
|---|---|---|
| P1 | dispatcher/llm/runtime/tape/closure + 现有测试 | DB schema、Authority、Continuity 语义 |
| P1.1（implemented） | `llm`/Agent wrapper/Authority/Runtime + 现有测试；每-attempt prompt isolation、固定同步 tool trajectory evidence | async、`define_tool`、session synthesized tool |
| V1（implemented） | v0.3 scorer、37 个 fixtures、README | v0.2、产品 Runtime |
| P2 | 一个窄 `superchat-experience.el` + 一个测试文件；必要时 registration/load | 新数据库、graph Anchor、通用 framework |
| P3a（implemented） | deepen `superchat-experience.el` + Closure/prompt/LLM receipt + 现有测试 | 新 DB、自动 Promotion、普通聊天默认注入 |
| V2a | 一个隔离 local/fake-provider harness | 真实 provider、行为效果声明 |
| T1 | 一个显式授权的 pinned transport preflight | cohort、泛化结论 |
| V2 | 一个隔离 experiment 目录 + synthetic read-only tool fixture | live user tools、现成 protocol recovery 策略 |

## 9. 验证基线与测试策略

2026-08-10 P1 完成后的 dirty worktree canonical ERT：

```text
666 tests
649 expected passed
17 skipped
0 unexpected
```

2026-08-10 P2 完成后的 dirty worktree canonical ERT：

```text
674 tests
657 expected passed
17 skipped
0 unexpected
```

2026-08-10 P1.1 完成后的 dirty worktree canonical ERT：

```text
699 tests
682 expected passed
17 skipped
0 unexpected
```

2026-08-10 V1 离线验收：

```text
v0.3 focused ERT: 4 / 4 passed
v0.3 audit: 37 / 37 verdicts matched, exit 0
independent review: source-facts ACCEPT; code-correctness ACCEPT;
                    experimental-validity ACCEPT
provider/model calls: 0
```

2026-08-10 P3a 完成后的 dirty worktree canonical ERT：

```text
705 tests
688 expected passed
17 skipped
0 unexpected
provider/model calls: 0
```

命令：

```bash
emacs -Q --batch -L . -l test/run-tests.el
```

P1/P1.1 结果证明 opt-in evidence 合同、durability-before-provider 顺序、每-attempt prompt/tool 隔离与因果、固定同步工具的 Action→Result/Error 证据，以及敏感键/accessor/tool-surface fail-closed 和 Closure 边界在 fake provider 路径上成立。另有聚焦测试使用本机 `llm` 0.31.1 的真实 struct/accessor 通过，但仍 mock 了 provider transport。

P3a 结果证明：只有显式引用 Candidate 的 `passed` Evaluation 才能写入
Promotion；任务 View 在 Promotion 前为空、Promotion 后包含 exact
Candidate/Promotion receipt、revocation 后重新为空；conflict、missing、多个候选
和绕过 writer 的 Agent disposition 都不注入；普通 chat 保持 opt-out；receipt
进入 Decision Snapshot。它没有调用真实 provider，且不能证明模型发生 Semantic
Use、Task Effect 或 Learning。

每个阶段的验证原则：

1. 最短的失败测试先落地，再写实现。
2. 测事件顺序和 exact bytes，不只测 sentinel/status。
3. 实验攻击 fixture 必须影响进程退出码。
4. skipped test 不算证据。
5. V1 完成不自动授权 provider/model；P3a 与 V2a 完成后，仍只有
   `transport-preflight` ticket 的显式决定能启动一次 transport check；
   preflight 通过也不自动启动 cohort。
6. Archive 有 Candidate 不等于 Candidate 已 Promotion。

## 10. 明确不做的事情

- 不把 Tape 当作现成 solution；它是历史、上下文和推导边界的思考模型。
- 不把 Anchor 当万能状态对象；v0 不依赖 Anchor 才能工作。
- 不用 graph 相似度、connected component、中心性或低熵直接判定成功。
- 不把低 semantic entropy 当入库标准；它只表示回答稳定，不表示正确。
- 不从一次成功轨迹自动抽取“所有步骤都是最佳实践”。
- 不让同一个 mutator 改 evaluator、sealed oracle、sandbox 或 Promotion policy。
- 不在没有 exact Decision Snapshot/Action/Outcome 的情况下跑更多 provider cohort。
- 不为了未来可能的大规模图，提前引入新存储服务。

## 11. 完成定义

这个计划的 v0 目标不是“Superchat 已经会自我学习”。v0 完成必须同时满足：

1. Actor 的先验 View 与后验 Outcome 在 Tape 上可被时间和事件身份严格区分。
2. 一次关键决策的 exact visible inputs、first Action 和 sealed evaluation 可重放。
3. Pattern 以带 scope/basis/counterevidence 的 Claim 存在，而不是被当作事实覆盖历史。
4. Applicability Graph 能保留冲突、缺失和边界，不把拓扑冒充因果。
5. 一个先验中没有恢复答案的 held-out 四路实验显示 Candidate-specific behavior change。
6. Promotion 由人明确批准，可通过追加 Revocation 回滚。
7. Future View 的变化能指回 exact Candidate、Evidence、Evaluation 和 Promotion。

达到这些条件后，项目才拥有一条最小但真实的“冻结权重 self-learning Runtime”纵切；在此之前，最准确的说法分别是 evidence capture、derived memory、candidate generation 或 contextual adaptation。
