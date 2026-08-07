# Superchat 证据边界精要（截至 2026-08-09）

状态：当前工程与实验的共同事实边界。它总结旧源码审查、Cohort、Audit、
Correction、Observer 与 Semantic Use 评审，不替代产品源码。

## 1. 领域模型

```text
DB         = SQLite 物理存储
Tape       = 追加式 Runtime 事实、身份、因果、顺序、回放
Projection = 从 Tape 折叠出的可重建读取视图
Closure    = 纯 Tape 解释器：回答 claim proved/open/unknown/invalid
Runtime    = Request → Turn/Run 生命周期与唯一终态所有者
Authority  = validate → effects → policy → approval → execute → Tape
Workspace  = Artifact 注册、revision、跨 Run 交付与共享编辑域
View       = 给读取者或模型的呈现；可以省略/摘要，但不能改写 Tape 历史
```

当系统说“原始内容被摘要代替”，准确含义只能是：当前 View 选择把一个后来追加的
summary/anchor 呈现给模型。原始 Entry 仍在 Tape 中；摘要必须有自己的身份、来源和
顺序，不能覆盖原 Entry。

## 2. 当前代码能证明什么

- Runtime 创建并终结明确的 Turn/Run，终态先持久化再交付。
- Authority 能证明已知工具的 requested/approval/started/completed-or-failed
  生命周期与 effect disposition。
- Closure 能从 Tape 判断 Runtime 终态和工具链是否满足合同。
- Workspace 能记录 Artifact provenance、revision 和发布事件，并在同一 Workspace
  内要求 `workspace_read(artifact_id, expected_revision)` 通过 producer Closure 与
  当前 revision 检查。
- Cohort v6 证明这些 seam 可以在冻结 Research Workflow 上形成完整、可回放的
  20-assignment 数据集。

## 3. 当前产品证据不能证明什么

- `tool.requested` 不保存完整工具参数；`tool.completed` 保存打印结果的 size/hash，
  不是 exact raw result bytes。
- Tape 不保存最终 provider outbound body、raw stream、exact first answer 或可靠的
  provider usage。
- `workspace_read` 成功只证明 Access/Delivery，不证明 Selection、Semantic Use、
  usefulness 或 Outcome contribution。
- Artifact/Claim/Outcome 的 hash、引用、相似度、citation 或评分都不能单独证明内容
  对结果产生了贡献。
- 当前模型工具不能跨到另一个 Workspace；producer/consumer 若要求不同 Workspace，
  缺少保留 provenance 的 transfer seam。
- 模型内部 attention、reasoning、pretraining policy 和“是否理解”不可从现有 Tape
  认证。

## 4. Claim 梯子

每一级都需要新增证据，不能自动升级：

```text
Artifact Fact
  → Access / exact delivery
  → Selection among proved-visible alternatives
  → Content Dependence
  → bounded Semantic Use observation
  → causal Task Effect
  → held-out Reusable Improvement
  → reviewed durable Learning/Promotion
```

当前产品稳定停在 Access。当前离线 v0.2 只是在设计一个未来可能检查
“fixed cells 与两输入使用一致”的 scorer；它本身仍没有模型数据。

## 5. 未来 Runtime 实验的九个前置条件

任何真实 provider 请求前都必须重新冻结并通过：

1. 选定 provider 确实经过最终可观察的 serialization seam；
2. 每 assignment 新 Session/Workspace，producer 与 consumer 在该 assignment 内共享
   Workspace，其他 history/memory/extension 被禁用或证明；
3. Artifact exact bytes、length、SHA、producer Tape range 在消费前后无漂移；
4. exact `workspace_info/workspace_read` 参数、返回 bytes、Authority execution 和
   Closure/Tape 顺序被 hash-link；
5. raw/parsed outbound witness 不保存 headers/secret，URL 去 query/userinfo；
6. first post-read request 包含 exact payload，withheld 的所有 request 均无 payload、
   target 或 truth-table 泄漏；
7. retry=0、无 provider/curl 隐藏 retry、工具 round 与 replacement attempt 分开；
8. raw earliest terminal answer、stream、usage-or-missing、monotonic latency 与所有
   unauthorized tool attempts 被保存；
9. harness、源码/dependency、schema、Prior、hooks/advice、budget、schedule 和
   missingness codes 有端到端 manifest/hash，preflight 失败即停。

## 6. 实验设计的硬规则

- Contract 在 instrument 之前；一个 probe 只回答一个问题。
- 第一份答案永不被 retry、best-of 或人工替换。
- record set 必须和冻结 schedule 一一对应；duplicate/extra/missing 都是
  `not-identifiable`。
- 证据 proof 使用严格枚举；Emacs Lisp 的任意 non-nil 不等于“已证明”。
- witness missing、parser failure、wrong answer、control failure 和 protocol failure
  必须是不同状态。
- sham 是控制诊断，withheld 是独立的复合程序比较；都不能冒充两输入内容观察。
- 固定构造任务只能报告 fixed-cell raw counts；没有任务抽样和重复 provider sample
  时，不使用 Wilson、sign-test、population 或 `caused`。
- harness failure 不是产品 failure；mock 必须先证明真的被调用。
- canonical suite 的 skip 不能当作 dependency-sensitive property 的 pass。

## 7. 当前授权边界

允许：离线读取源码、运行纯 scorer、独立攻击 v0.2。

不允许：Runtime harness、真实 provider/model 请求、Task Effect、Capability、Memory
promotion、Workflow extraction、Authority learning 或 Learning Layer。

下一门：`v02-independent-review`。只有新的 source/code/validity Reviewers 全部无法
重现假阳性，才可以讨论一个新的、单独冻结的 preflight；不是直接跑实验。
