# Google 的“时间经验”：ReasoningBank 与 DeepMind 时序学习的边界

研究日期：2026-08-12

## 先说结论

“Google 的时间经验”不是一个可直接对应到官方项目名的术语。结合本仓库此前
指定的 `google-research/reasoning-bank`，最可能的主参考是 **ReasoningBank**：
它把连续任务流中的“前一次任务结束后形成的经验”带到下一次任务。但它和
DeepMind 的 **Temporal Value Transport (TVT)**、经验回放是不同层次的东西：

```text
ReasoningBank：任务结束 → 提炼策略 → 下一任务检索 → 影响行动
TVT：后来的记忆读取 → 把价值信号传回更早的动作 → 更新策略
Replay：保留经历 → 离线再次训练/回放
```

因此，对 Superchat 最有用的不是照搬一个叫 “Memory” 的模块，而是把时间边界
说清楚：**当前 Run 只能读取 Run 开始前已被批准的派生经验；Run 结束后，才可以
基于完整轨迹和结果生成下一轮候选；任何候选都不能越过 Authority，也不能把
“被读取”冒充成“学会了”。**

本文只使用 Google Research/DeepMind 的官方页面、论文和公开源码作为技术证据。
ReasoningBank 仓库自己的 README 明确声明它不是官方支持的 Google 产品、仅供
演示，不应当当成生产系统规格。

## 1. 先把三个 Google/DeepMind 方向分开

| 方向 | 时间上的“经验”是什么 | 经验何时改变未来行为 | 真正改变的对象 | 对 Superchat 的直接价值 |
|---|---|---|---|---|
| ReasoningBank | 已完成任务的成功/失败轨迹，经 LLM 提炼成结构化策略 | 下一任务开始前检索并注入提示 | 外部 JSONL 经验池与提示上下文；不改模型权重 | 任务间的提炼、检索、迁移、失败经验 |
| MaTTS | 同一任务的多个并行轨迹，或同一轨迹的多轮修订 | 当前任务的额外推理与任务结束后的提炼 | 试验时的 rollout 数量和提炼输入 | 对比成功/失败轨迹，不把一次幸运路径当规则 |
| TVT | 一个较晚时刻读取的较早事件及其价值 | 训练时把后来的价值传回早期动作 | RL 的奖励/策略更新 | 说明“时间顺序”和“结果归因”不是同一件事 |
| Replay | 已采集的状态/动作/奖励序列 | 后续离线 replay/训练阶段 | RL 参数或价值估计 | 说明 Tape 可以是事实源，学习器应在热路径外工作 |

ReasoningBank 是当前 Superchat 讨论最接近的参考；TVT 和 Replay 用来解释“时间”
这个词的另一种含义，不能混成同一条产品管线。

## 2. ReasoningBank 的真实时间线

论文把测试时学习定义为一个只能顺序接收任务的流：

```text
Q = {q1, q2, ..., qN}
每次只揭示 q_i；完成 q_i 后，才处理它的轨迹并更新经验池；q_(i+1) 才能读到更新。
```

官方论文的流程是三步：

1. **Retrieval（任务前）**：用当前 query 的 embedding，从已有经验中取 top-k
   相似项，默认实验是 `k=1`，把所选策略作为额外 system instruction 注入 Agent。
2. **Construction（任务后）**：完整轨迹结束后，用 LLM-as-a-judge 给轨迹标记
   success/failure，再分别从成功轨迹提炼可迁移策略、从失败轨迹提炼反事实/预防性
   经验。
3. **Consolidation（下一任务前生效）**：把新项直接 append 到经验池。作者明确说
   为了隔离贡献，当前版本没有做复杂的合并、遗忘或清理。

结构化经验项只有三部分：`title`（策略名）、`description`（何时/为何使用的
一句话）和 `content`（1–若干句可执行的推理/操作洞见）。原始 query、原始轨迹、
status 和这些 memory items 会一起留在 JSON 记录中；query embedding 另存缓存。

公开源码把这个顺序写得很直白：

- `WebArena/run.py` 在启动任务时读已有 JSONL、调用 `select_memory`，再把选中
  项写入当前任务的 memory prompt；
- `WebArena/pipeline_memory.py` 先运行任务、再评估、最后调用 `induce_memory.py`；
- `induce_memory.py` 读取完整 step 轨迹和 judge 结果，按照 success/fail prompt
  产生最多三个 memory item，然后 append 到 JSONL；
- `prompts/memory_instruction.py` 对失败样本要求“反思失败原因并写出防止重犯的
  recovery/preventive strategy”，不是把失败动作原样保存成正例。

这说明它不是“每个 Tape event 都立刻成为 memory”，也不是“当前 Run 边运行边
修改自己的规则”。它是一个**任务间、闭环但有明确提交点的后处理管线**。

### 2.1 MaTTS 为什么被称为“经验扩展”

MaTTS 不等于再加一个 Memory 层：

- **Parallel scaling**：同一 query 生成多条轨迹，比较哪些模式在成功轨迹中一致、
  哪些错误导致失败，再提炼策略；
- **Sequential scaling**：同一条轨迹完成后多轮自检/修订，把中间纠错也作为提炼
  信号。

它真正增加的是“同一任务的可比较经验数量”，让提炼器有 contrastive signal，而不
是单纯把一条很长的 transcript 塞进 prompt。论文还显示经验数量过多会引入冲突：
在其 WebArena-Shopping 消融中，取 1 条经验优于取 2、3、4 条。因此“更多历史”
不是免费收益，相关性和质量比数量重要。

## 3. 这是否已经证明“学会了”？

ReasoningBank 的论文证据比“View 被读到了”强，但仍需要准确命名：

- 它报告 memory-free、trajectory memory、workflow memory 与 ReasoningBank 的
  success rate 和 interaction steps 差异；
- 它在 WebArena Multi、Mind2Web cross-task/cross-website/cross-domain 等设置中
  报告了迁移结果；
- 它报告失败轨迹加入后 ReasoningBank 的 WebArena-Shopping 分数从 46.5 提升到
  49.7，而 success-only 的对照和其他 memory 机制并不呈现相同收益；
- 它展示了从简单 checklist 到更有条件、可组合的策略演化案例。

这些是“外部经验在后续任务上带来行为增益”的证据。它们不是以下几件事的证据：

1. 当前模型内部权重发生了学习；论文机制明确是 test-time 外部记忆和 prompt 注入。
2. 某一次 View 被检索、某个 prompt 里出现文字，就已经产生了因果任务效果。
3. 一个 LLM 自评的 Success/Failure 一定是真实标签；论文的 limitation 明确承认
   judge 错误和歧义会污染 memory induction。
4. 经验池有自动的冲突解决、衰减、撤销、权限隔离或安全 promotion；当前实现为了
   简化实验，直接追加。
5. 结果已经证明长期重启、跨用户、跨环境或生产级运行安全；论文与 README 都把
   实现定位为研究/演示代码。

所以，给 Superchat 的严格定义应是：

> **学习** = 相对于无经验 control，在没有见过的后续任务上出现可重复、可归因的
> 行为改善；改善在重启后仍能由已批准的派生经验解释，撤销后消失，并且不能带来
> Authority/安全回归。

当前本仓库的真实实验已经遵循其中一部分：它能证明 View 选择、重启后的 View
持久性、撤销后的 View 为空以及 Authority 的 fail-closed 边界；但没有完成足够的
held-out control/treatment 任务对和可归因的最终任务结果，因此只能称为
`retrieval/use` 或 `durable selection`，不能称为已学会。见
[`experience-learning-real-tests-v0.md`](../design/experience-learning-real-tests-v0.md)
和 [`superchat-evidence-boundaries-2026-08-09.md`](./superchat-evidence-boundaries-2026-08-09.md)。

## 4. “时间”真正带来的架构约束

### 4.1 经验不是事实，提炼也不是覆盖

Google 的实现把原始轨迹和派生 memory item 一起保留；它不需要删除原始轨迹来
得到短策略。对 Superchat，这正好对应：

```text
Tape Entry（不可变事实）
        ↓ 任务结束后的离线提炼
Candidate/Experience（派生假设，有 source_ids）
        ↓ 独立 Evaluation + 用户/策略 Promotion
Experience View（当前 Run 可见的读模型）
```

View 是当前 Run 的上下文选择，不是 Tape 的替换点。摘要、经验和撤销都应追加新
事件；Projection 可以重建 View，但不能改写历史。

### 4.2 经验必须有“可见时间”和“生效时间”

至少需要记录两种顺序：

- `observed_at/closed_at`：哪个 Run 先结束、何时产生事实；
- `admitted_at/effective_from`：何时通过 Evaluation/Promotion，之后才允许被
  新 Run 读取。

这消除了常见竞态：一个正在执行的 Run 不能读取它自己还没结束的经验；一个刚被
  撤销的经验不能因为 prompt 缓存或旧 Projection 留在下一次请求里；同一任务的
  control 不能意外读取 treatment 产物。

### 4.3 失败经验是“防错建议”，不是负权限

ReasoningBank 会从失败中提炼 preventative lesson，但它的实现没有 Authority。
在 Superchat 中，经验文字只能提供建议；它不能从“不要调用某工具”的自然语言
变成禁止调用。禁止必须进入共享 Authority seam，由 Runtime 传入显式的
dependent-tool policy，并在 Tape 留下 denied/failed 证据。这样经验提炼和安全
执行各司其职。

### 4.4 Tape 适合当事实源，不必把 Tape 变成 Memory

TVT/Replay 的共同启示是“保留经历，稍后再用”，不是“历史日志本身就是策略”。
Superchat 可以继续用 Tape 做 append-only 历史：

- Runtime 在热路径写入完整 Run/Turn/tool 事实；
- 一个离线或任务结束后的提炼步骤读取封闭 Run；
- 它产生有来源、有版本、可撤销的 Candidate；
- Projection 只负责把已批准候选组装成 View；
- Runtime 只在 Run 开始时快照 View，Authority 仍是唯一效果闸门。

这比在 Tape、Memory、Workflow、Runtime 之间再加一层“学习总线”更薄，同时保留
了时间顺序和审计能力。

## 5. DeepMind 的 TVT：真正的长期时间归因是什么

TVT 是一个不同的问题：单个 episode 内，早期动作可能要到很久以后才得到奖励。
DeepMind 的 RMA 先把视觉事件编码进外部 memory，后面用 content-based read 找回
过去时刻；当 read strength 超过阈值时，TVT 把较晚时刻的 baseline value 按
`alpha × read_weight` 传回被读取的较早时刻，作为 fictitious reward 影响 RL 更新。
源码还明确排除距当前读取时刻太近的事件（`no_transport_period`），避免把短期
邻近信息误当长期归因。

它的实验不是“下一次 prompt 是否读到了文字”，而是 key-to-door / active-visual-
match 这类延迟奖励任务：关键行为发生在 explore phase，奖励在 exploit phase 才
出现，论文与官方实现用 last-phase reward 观察 TVT 是否让早期行为稳定学会。
官方实现 README 还明确把 `do_tvt=False`、`gamma` 等作为对照，并报告多次 replica
而不是一条成功 trace。

对 Superchat 的可用原则只有两个：

1. **因果关联需要跨时间链接**：后来的 Outcome 要能引用更早的 Decision/Tool/Run，
   但“相邻发生”不等于“早期行为造成了结果”；
2. **需要实际对照和重复**：要看 held-out task 的 outcome 是否改善，不能只看
   `workspace_read` 成功、View 满足或下一个请求包含某 action。

不应把 TVT 的 reward transport、梯度更新或 neural memory 直接塞进 Superchat。
Superchat 的外部策略候选和 Closure/Evaluation 是不同实现，但可以借它的
`earlier decision ← later outcome` 证据连接。

## 6. Replay 与“离线时间”

DeepMind 的官方 replay 说明把经验 replay 描述为：Agent 与环境交互收集经验，
把它们放进 replay buffer，之后重复回放来学习；由于实时交互能吸收的信号有限，
持续的 offline learning 很重要。Replay across Experiments 进一步说明，过去实验
的数据可以用于后续探索和 bootstrap，但这是 RL 学习器重用数据，不等于把所有历史
事件原样注入在线 prompt。

对本仓库的直接翻译是：

- Tape 是 replayable source of truth，不是“会自己更新的 memory”；
- Projection/View 是可重建的读取结果，不是训练权重；
- Candidate induction 是离线/任务间步骤，不应在 Authority 热路径里偷偷改规则；
- 经验只有经过独立的 outcome gate 才能进入下一轮可见 View；
- 需要保留完整来源，才能在发现 judge 错误或冲突时撤销并重建 View。

## 7. 最小可执行设计：不增加一个“大 Memory 框架”

当前最薄的实现边界可以写成四个动作，而不是新建一堆模块：

1. `close-run`：只接受已经有明确终态、完整 Tape 因果和工具效果的 Run；
2. `induce-candidate`：从该 Run 产生一个结构化策略候选，带 `source_ids`、
   outcome、scope、dependent-tools 和版本身份；这一步可以先由 fixture/人工完成，
   自动 LLM judge 不是前置条件；
3. `admit-or-revoke`：Evaluation/用户审查追加 promotion 或 revocation 事件，
   不覆盖旧候选；
4. `assemble-view`：下一次 Run 开始时只选择当前 scope 下 active、无冲突、已批准的
   候选，并把 instruction 和显式 action policy 一起快照进 context envelope。

实验只需围绕一个时间流验证：

```text
Run A (无经验) → outcome
     ↓ close + induce + admit
Run B (有经验) → outcome
Run C (未见任务 control/treatment) → outcome difference
restart → same selection
revoke → empty/blocked + Authority denial
```

最小成功信号不是“Run B 读到了 View”，而是 C 的 treatment 在重复 paired
outcomes 上比 control 更好、且撤销后该行为差异消失。若只观察到 B 的动作变化，
结果应命名为 `retrieval/use`；若只有 restart 后 View 仍存在，命名为
`durable selection`；两者都不能升级为 `learning`。

## 8. 明确不照搬的部分

- 不照搬 ReasoningBank 的 embedding cache：当前 Superchat 的 typed scope、
  applicability graph、Closure 和 revocation 已经提供更强的审计边界；相似度只是
  检索实现，不是批准依据。
- 不照搬其 LLM-as-a-judge 作为唯一真值：可把 judge 当候选提炼器或弱信号，但
  promotion 仍需可验证 outcome/人工判断/明确 contract。
- 不照搬直接 append、无冲突合并、无遗忘策略到产品热路径：研究代码为隔离贡献而
  简化，Superchat 需要 fail-closed、可撤销和 Authority policy。
- 不照搬 TVT 的神经权重更新：Superchat 当前目标是冻结 provider 权重下的外部
  策略适应；必须在 held-out 行为上证明效果后，才谈“学会”。
- 不把 MaTTS 的 parallel/sequential rollout 数量放进第一阶段：它适合在最小
  control/treatment 闭环成立后增加对比信号，不适合继续扩大尚未定义的 harness。

## 9. 建议的下一步顺序

1. 先冻结一页纸的“学会了”合同：明确 task stream、admission point、held-out
   control/treatment、重复次数、restart、revocation、Authority safety 和
   `not-identifiable` 状态。
2. 保持当前 Tape/Runtime/Authority/Projection/Closure 分工，只把诱导放在
   closed Run 之后；删除或停用尚未回答问题的多层 harness，而不是再冻结一批哈希。
3. 用一个极小的结构化 candidate 做 1 对 matched control/treatment，再做新的
   held-out stream；trace 必须同时保存 prompt、Decision Snapshot、tool effect、
   terminal outcome、source/effective order。
4. 只有当 held-out 行为改善在重启后仍成立、撤销后消失，且没有安全回归，才允许
   把结果命名为 Learning 或打开默认 Experience View。否则保留 `nil`，修一个明确
   的原因并换合同。

## 一手来源

- [Google Research：ReasoningBank 官方说明](https://research.google/blog/reasoningbank-enabling-agents-to-learn-from-experience/)
- [ReasoningBank 论文（arXiv:2509.25140）](https://arxiv.org/abs/2509.25140)
- [ReasoningBank 论文 HTML（含方法、实验和 limitations）](https://ar5iv.labs.arxiv.org/html/2509.25140)
- [ReasoningBank README，固定源码提交](https://github.com/google-research/reasoning-bank/tree/ed80611788292ea739f1effd31f16c53823b8a0d)
- [任务前检索：WebArena/run.py](https://github.com/google-research/reasoning-bank/blob/ed80611788292ea739f1effd31f16c53823b8a0d/WebArena/run.py)
- [任务后诱导：WebArena/induce_memory.py](https://github.com/google-research/reasoning-bank/blob/ed80611788292ea739f1effd31f16c53823b8a0d/WebArena/induce_memory.py)
- [检索与 embedding cache：WebArena/memory_management.py](https://github.com/google-research/reasoning-bank/blob/ed80611788292ea739f1effd31f16c53823b8a0d/WebArena/memory_management.py)
- [顺序管线：WebArena/pipeline_memory.py](https://github.com/google-research/reasoning-bank/blob/ed80611788292ea739f1effd31f16c53823b8a0d/WebArena/pipeline_memory.py)
- [成功/失败提炼 prompt](https://github.com/google-research/reasoning-bank/blob/ed80611788292ea739f1effd31f16c53823b8a0d/WebArena/prompts/memory_instruction.py)
- [DeepMind：Replay in biological and artificial neural networks](https://deepmind.google/blog/replay-in-biological-and-artificial-neural-networks/)
- [DeepMind：TVT 论文（Nature Communications）](https://www.nature.com/articles/s41467-019-13073-w)
- [DeepMind TVT 官方源码 README](https://github.com/deepmind/deepmind-research/blob/master/tvt/README.md)
- [DeepMind TVT 的 value transport 实现](https://github.com/deepmind/deepmind-research/blob/master/tvt/tvt_rewards.py)
- [DeepMind TVT 的 memory reader/writer 实现](https://github.com/deepmind/deepmind-research/blob/master/tvt/memory.py)

