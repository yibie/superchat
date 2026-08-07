# 冻结权重 Agent 适应：文献地图与 Superchat 产品边界

> 研究日期：2026-08-10。范围：ReAct、Generative Agents、Reflexion、
> Self-Refine、ExpeL、Voyager、OPRO、LATS，以及一组需要训练新参数、因而
> 不属于当前产品路线的对照工作。本文只使用原论文、作者项目、官方代码与
> `tape.systems` 等一手材料。它是研究地图，不是产品规格；本次没有运行模型、
> provider 或 Runtime harness，也没有据此宣称 Superchat 已实现 self-learning。

## 0. 结论先行

“不改模型权重”不是一个单一方法，而是三种不同东西：

1. **运行时求解/搜索底座**：同一题里多想几步、多试几条路径，结束后不留下
   可复用策略。ReAct、Self-Refine、LATS 属于这一层。它们能提高一次任务的
   成功率，但本身不是 durable learning。
2. **冻结权重的外部状态适应**：把经历提炼成可在以后读取的自然语言、代码或
   prompt policy。Generative Agents、Reflexion、ExpeL、Voyager、OPRO 分别
   展示了从“同一角色的长期记忆”到“跨任务经验/技能/prompt 候选”的不同强度。
   在八篇主表中，只有 ExpeL、Voyager、OPRO 给出了较明确的
   train/evaluation 或新世界
   复用；仍不能自动外推为跨领域、长期、安全的产品级提升。
   DGM、STOP、Gödel Agent 又补充了另一种对象：冻结基础模型，但搜索和改写
   agent scaffold/code。这也属于外部状态/程序适应，而不是参数训练。
3. **训练新参数**：Toolformer、Self-Instruct、Constitutional AI/RLAIF、STaR、
   Self-Rewarding LM 等工作的论文增益来自 fine-tuning、DPO 或 RL 后的新
   checkpoint。训练完成后当然可以冻结推理，但这不等于增益来自“冻结权重下
   的自学习”。这一路线不适用于当前只能调用冻结 provider 的 Superchat 产品
   假设。

因此，Superchat 的最小闭环对象不应叫“Memory 自己学了”“Skill 自己学了”或
“Tape 自己学了”。最小对象应是：**一个有版本的 View/Action policy 候选，
连同追加式 Outcome/feedback 证据，经过独立评估后显式 promotion；失败时可以
rollback 到旧版本。** 模型参数可以完全不动，但仅有持久化、检索或一次重试还
不能证明这个闭环成立。

Stanford CS329A 的官方课程安排也把 test-time compute、robust verification、
tool/code feedback、planning/search、train-time RL 与 open-ended evolution
分成不同课次；这支持“研究版图分叉”，而不是把所有改进都等同于权重更新
（[CS329A 官方课程页](https://cs329a.stanford.edu/)）。本稿不重复已有
[`verifiable-agent-improvement-prior.md`](./verifiable-agent-improvement-prior.md)
中的通用验证纪律，只补足无权重方法的分类和产品映射。

### 引用数口径

论文“被引用次数”会随日期变化，同一工作还常被数据库拆成 arXiv、会议和期刊
多个条目。本研究实际遇到了同名版本拆分与 API 限流，因而**不拼接不同索引的
数字，也不按引用数排序**。若后续展示引用数，必须同时写明索引、查询日期、
具体版本，并把它标成动态发现指标；引用数不是论文质量、可复现性或产品适用性
的证据。本稿的判断只依据下列固定论文版本和一手实现。

## 1. 三层总表

| 工作 | 本稿层级 | 被改变的对象 | 反馈来源 | 改模型权重 | 跨运行持久状态 | 论文支持的迁移范围 |
|---|---|---|---|---|---|---|
| ReAct | runtime/search substrate | 当前 Thought–Action–Observation 轨迹 | 环境 observation、API 返回 | prompting 否；论文另有 fine-tuning 变体 | 否 | 未见跨任务策略迁移 |
| Self-Refine | runtime/search substrate | 当前输出与自评后的修订稿 | 同一个 LLM 的自然语言反馈 | 否 | 否，仅当前样本 | 未见跨任务迁移 |
| LATS | runtime/search substrate | 单题 MCTS 树、visit/value、失败轨迹与 reflection | 环境反馈、terminal reward、LM value | 否 | 论文机制中不持久 | 多 benchmark，不等于迁移 |
| Generative Agents | external-state adaptation | observation、reflection、plan 构成的 memory stream | 环境感知 + 模型生成的重要度/反思 | 否 | 是，同一模拟角色生命周期 | 角色内长期连续性；不是 held-out task transfer |
| Reflexion | external-state adaptation | 对失败轨迹的语言反思 | 环境 reward、手写 heuristic、LLM self-eval、测试 | 否 | 同一任务多次 trial 间有界保留 | 主要是同题重试；未证跨任务持久迁移 |
| ExpeL | external-state adaptation | 可增删/投票的 insight 集 + 成功 trajectory pool | 训练任务 success/failure、成败对照 | 否 | 是 | 同域跨任务；另测 HotpotQA→FEVER |
| Voyager | external-state adaptation | 可执行代码 skill library + 自动 curriculum 进度 | 环境状态、执行错误、LLM self-verification | 否 | 是 | 新 Minecraft 世界中的新任务 |
| OPRO | external-state adaptation | prompt/instruction 候选与分数历史 | 训练集 objective/accuracy | 否 | 可保存候选轨迹与胜出 prompt | train→test；部分同域数据集迁移 |

这张表故意把“有外部状态”与“跨任务迁移”拆开。Generative Agents 的 memory
stream 很耐久，Reflexion 的经验能跨 trial，但它们不因此自动具有 ExpeL 那种
train-task→evaluation-task 证据；反过来，LATS 在多个领域都有效，也不代表它
把一个领域学到的策略带到了下一个领域。

## 2. Runtime / search substrate：一次任务能变好，任务结束不留策略

### 2.1 ReAct

**代码/论文事实。** ReAct 让 LLM 交替生成 reasoning trace 与 task-specific
action；action 从 Wikipedia API 或交互环境拿到 observation，再进入后续上下文。
few-shot prompting 路径不更新权重。论文在 HotpotQA、FEVER、ALFWorld、WebShop
上评估；摘要报告 ALFWorld 与 WebShop 相对 imitation/RL baseline 的绝对成功率
分别提高 34 和 10 个百分点。原论文也包含 ReAct trajectory 的 fine-tuning
实验，不能把该变体混入“纯 prompting”结论
（[ReAct, arXiv:2210.03629v3](https://arxiv.org/abs/2210.03629v3)；
[作者代码，固定提交](https://github.com/ysymyth/ReAct/tree/6bdb3a1fd38b8188fc7ba4102969fe483df8fdc9)）。

**边界。** prompting 版改变的是当前运行的工作轨迹，没有定义跨运行 memory、
Outcome 蓄积或 policy promotion。对未见 benchmark 样本的求解能力是同一机制的
泛化，不是把上一次失败提炼成下次可复用策略。ReAct 对 Superchat 最有价值的是
Execution 的 Thought/Action/Observation 语法和 Authority-gated tool loop，而不是
self-learning claim。

### 2.2 Self-Refine

**代码/论文事实。** Self-Refine 用同一个 LLM 依次做 initial generation、
FEEDBACK、REFINE，直到次数上限或模型判断停止；不需要 supervised data、额外
训练或 RL。论文在七类任务上用人工和自动指标评估，报告平均约 20 个百分点的
绝对提升
（[Self-Refine, arXiv:2303.17651v2](https://arxiv.org/abs/2303.17651v2)；
[作者代码，固定提交](https://github.com/madaan/self-refine/tree/9a206d41e5d2d0c241bb441f41eeadb945afaa55)）。

**边界。** 被改变的是一个样本的草稿，feedback 仍来自同源模型；论文机制没有
把经验写成跨运行可治理状态，也没有 held-out policy promotion。它证明的是
test-time refinement。没有外部 oracle 时，同一个模型可能只是更自信地重复错误；
增加调用次数和最终稿更漂亮，都不能单独证明语义正确。

### 2.3 LATS

**代码/论文事实。** LATS 把 ReAct 扩展为 Monte Carlo Tree Search：LM 提议
节点、充当 value function，并用外部环境反馈和 self-reflection 改善搜索。论文
覆盖 HotpotQA、HumanEval、WebShop 与数学任务，报告 GPT-4 HumanEval pass@1
92.7%，以及 GPT-3.5 WebShop 平均分 75.9
（[LATS, arXiv:2310.04406v3](https://arxiv.org/abs/2310.04406v3)；
[作者代码，固定提交](https://github.com/lapisrocks/LanguageAgentTreeSearch/tree/853d81614607dd27433faf17c7b0a7d660f95d22)）。

**边界。** 树的 visit/value 和 reflection 服务于当前题目的搜索；论文没有把它们
晋升成跨运行 policy。论文也明确讨论了相对简单 prompting 更高的推理成本；
LM value、self-reflection 和有限 rollout budget 都会使搜索偏斜。多领域分别跑出
好结果只证明 substrate 可移植，不证明学习状态跨领域迁移。对 Superchat 而言，
LATS 是可选的 Runtime 调度策略，不该被塞进 Memory 或当成已发生的 learning。

## 3. 冻结权重的外部状态适应

### 3.1 Generative Agents：长期身份记忆，不是任务奖励学习

**代码/论文事实。** 架构把 observation 写入 memory stream，用 recency、
relevance、importance 取回相关 memory objects；累计 importance 触发 reflection。
关键是：reflection 也写回同一个 stream，并保存对底层 memory objects 的引用；
plan 同样回写，供未来行为检索。这不是“摘要覆盖原记忆”，而是派生项与来源共同
存在。论文在 25-agent sandbox 中展示 Valentine’s Day party 等涌现行为，并用
人类 believability 排序和 observation/planning/reflection ablation 评估
（[Generative Agents, arXiv:2304.03442v2](https://arxiv.org/abs/2304.03442v2)；
[作者代码，固定提交](https://github.com/joonspk-research/generative_agents/tree/fe05a71d3e4ed7d10bf68aa4eda6dd995ec070f4)）。

**边界。** 反馈是感知事实和模型自己的 importance/reflection，不是独立任务
Outcome。持久性限于模拟角色/世界的长期连续性；论文没有 held-out tasks 上的
政策迁移门。believability 是“像一个连贯角色”，不等于回答正确或任务效果提高。
reflection 可能幻觉或过度概括；有 provenance 指针只说明来源可追，不说明推论
正确。它最适合作为 `Entry → Projection/Observation → View` 的范例。

### 3.2 Reflexion：同一任务跨 trial 的语言经验

**代码/论文事实。** Reflexion 把 sparse/binary reward、手写失败 heuristic、
LLM self-evaluation 或代码测试转成文字 reflection，追加到 episodic memory，供
下一次 trial 的 Actor 使用；不更新模型权重。论文在 ALFWorld、HotpotQA、
HumanEval/MBPP 等任务上测试，摘要报告 HumanEval pass@1 91%（其 GPT-4 baseline
80%），正文也报告 ALFWorld、HotpotQA 和 HumanEval 的相对改进。实际实验为控制
上下文，把长时 memory 限为最近若干 reflection；环境通常在 trial 间 reset
（[Reflexion, arXiv:2303.11366v4](https://arxiv.org/abs/2303.11366v4)；
[作者代码，固定提交](https://github.com/noahshinn/reflexion/tree/218cf0ef1df84b05ce379dd4a8e47f17766733a0)）。

**边界。** 这是“同题失败后再试”的耐久外部状态，不是已证的跨题 policy。
其 credit assignment 依赖 evaluator/heuristic/self-written tests，论文明确承认
self-evaluation 能力依赖与无成功保证。若 Superchat 采用这种结构，应把
Outcome 与 reflection 分成两条 Entry：前者是环境事实，后者是可争议 Observation；
否则模型解释会伪装成奖励本身。

### 3.3 ExpeL：跨任务 insight 与成功经验池

**代码/论文事实。** ExpeL 先在 training tasks 上用 trial-and-error 收集成功与
失败轨迹，再从成败对照和成功轨迹列表中提炼自然语言 insights；insight 可 ADD、
EDIT、UPVOTE、DOWNVOTE，成功经验进入可检索 experience pool。evaluation 时
权重不变，agent 同时读取 insights 与 top-k 相似成功轨迹。论文用四折验证覆盖
HotpotQA、ALFWorld、WebShop；图 5 报告各域优于 ReAct/Act，并用 insights-only、
retrieve-only 做消融。跨域实验把 HotpotQA insight 适配到 FEVER，成功率为
70±0.7%，相对 ReAct 63±0.4%；但它使用目标域 few-shot examples 来适配 insight，
不能写成零样本跨域迁移
（[ExpeL, arXiv:2308.10144v3](https://arxiv.org/abs/2308.10144v3)；
[作者仓库，固定提交](https://github.com/Andrewzh112/ExpeL/tree/692a677c6f94757345cf3205d85ccbe55fb13d82)）。

**边界。** 论文消融发现直接加入 reflection 反而可能因幻觉误导 insight 提炼，
也发现 insight 质量依赖更强的 LLM；这正说明自然语言 Observation 需要验证与
retirement，不是新真值。当前固定作者仓库只有 README/LICENSE 等材料，本研究
不能据此声称已有完整可重放实现。ExpeL 是八篇中最接近“经验→派生 policy→
held-out tasks”的对象，但仍缺少独立 review、版本 promotion、安全门与 rollback。

### 3.4 Voyager：经验证后提交的可执行技能库

**代码/论文事实。** Voyager 在 Minecraft 中用自动 curriculum 选目标，让 GPT-4
生成 JavaScript program，并把环境 feedback、execution error、self-verification
送入下一轮修订；只有 self-verification 判断任务完成后才把程序提交到 skill
library。技能以描述 embedding 索引，可为新任务取回并组合；LLM 权重不更新。
论文报告获得 3.3× 更多 unique items、移动 2.3× 更远、关键里程碑最快 15.3×，
并把已学 skill library 带到新的 Minecraft 世界执行新任务；另有 curriculum、
skill library、feedback/error/self-verification 等消融
（[Voyager, arXiv:2305.16291v2](https://arxiv.org/abs/2305.16291v2)；
[作者代码，固定提交](https://github.com/MineDojo/Voyager/tree/55e45a880755d0c8c66ca7fb5fe7962ac8974f89)）。

**边界。** 这是最接近“Action policy artifact”的案例，但 self-verification 仍由
LLM 完成，论文列出误判成功、遗漏任务状态和调用不存在 API 等失败。技能强依赖
Minecraft/Mineflayer action space，不能直接外推为通用能力。对 Superchat 更重要
的启示是：代码技能只有在 Outcome 证据、Authority effects、版本身份和 review
之后才有 promotion 资格；检索命中或成功运行一次都不够。

### 3.5 OPRO：可评分 prompt policy，而不是会成长的 Runtime

**代码/论文事实。** OPRO 把 optimization problem 写成自然语言；optimizer LLM
读取历次 solution–objective pairs 后提议新解，外部 scorer 计算 objective，再把
新 pair 加回 meta-prompt。prompt optimization 中，被改的是 instruction text，
scorer/optimizer 权重均不更新。论文用 training accuracy 选 prompt，再在 GSM8K、
BBH test set 评估，报告相对人工 prompt 最高 +8 与 +50 个百分点，并考察部分同域
数据集迁移
（[OPRO, arXiv:2309.03409v3](https://arxiv.org/abs/2309.03409v3)；
[官方代码，固定提交](https://github.com/google-deepmind/opro/tree/a76bdce2cbf6d4a0d1e570a6fcfe17be9c2abdd7)）。

**边界。** OPRO 是离线/批量 policy search，不是对话 Runtime 自动反思。论文
明确需要 training set 计算 objective；分析中常见 training accuracy 比 test 高
5–20 个百分点，并指出 optimizer 不善于利用错误案例、优化有方差和过拟合风险。
它给 Superchat 的直接设计是 `Candidate → Evaluation → Promotion`，而不是让
当前 prompt 在生产流量中自行覆写。最终 prompt 必须有不可变版本、held-out gate、
review 与 rollback。

### 3.6 冻结基础模型也能改 agent code/scaffold

这组近年反例阻止一个错误叙事：不能说“2025 年以后只有更新权重才算真正
self-improvement”。三者都保持 base LLM 不变，改变的是运行它的程序：

- **Darwin Gödel Machine (DGM)** 在 coding-agent 的 Python/tool/workflow
  空间里产生后代，以 archive 和 parent lineage 保留分支，并在 withheld
  benchmark 上选择/转移；论文把训练新 foundation model 留作未来工作
  （[DGM, arXiv:2505.22954v3](https://arxiv.org/html/2505.22954v3)；
  [archive/parent 搜索，固定提交](https://github.com/jennyzzt/dgm/blob/a565fd2d1dca504ef5104a7cc0f3bdc4ab9b4fd2/DGM_outer.py#L50-L190)）。
  archive lineage 提供可追溯候选族谱，但不自动等于 reviewed promotion 或原子
  rollback。
- **STOP** 用冻结 LM 递归改写 improver program，并在异常时退回 previous
  improver；论文明确它不是完整 recursive self-improvement
  （[STOP, arXiv:2310.02304](https://arxiv.org/abs/2310.02304)；
  [固定代码](https://github.com/microsoft/STOP/blob/0d6780c54306b2486dd36e9c4ae9b49aceb27ea4/run_improver.py#L122-L180)）。
  它的 `reliability_guard` 明确不是 security sandbox
  （[固定代码说明](https://github.com/microsoft/STOP/blob/0d6780c54306b2486dd36e9c4ae9b49aceb27ea4/helpers.py#L80-L94)），
  因而只能借预算封顶的离线候选优化，不能借其执行隔离假设。
- **Gödel Agent** 让冻结 LM 在运行时 introspect、`exec`、`setattr` 来 monkey-patch
  agent logic
  （[ACL 2025 论文](https://aclanthology.org/2025.acl-long.1354/)；
  [固定代码](https://github.com/Arvid-pku/Godel_Agent/blob/bbb508796be31c7140cdfc7106efd830a1324242/src/agent_module.py#L77-L191)）。
  当前代码还通过 `shell=True` 调宿主命令且没有硬 sandbox
  （[固定代码](https://github.com/Arvid-pku/Godel_Agent/blob/bbb508796be31c7140cdfc7106efd830a1324242/src/agent_module.py#L193-L276)）；
  这是 Superchat 不应照搬的 live self-modification 反例。当前仓库 MGSM 路径会
  在验证阈值后读取 test accuracy 并据此保存，但代码晚于论文；本稿只把它记为
  当前实现边界，不反推正式论文实验存在 test leakage。

两份公开综述也把参数更新与外部状态并列，而不是定义成二选一：
[Self-Evolving Agents Survey v4](https://arxiv.org/html/2507.21046v4) 的“what”轴
同时包含 model、context/memory、tools、architecture；
[Lifelong Agents roadmap v1](https://arxiv.org/html/2501.07278v1) 区分 working、
episodic、semantic、parametric memory，只有 parametric memory 必然触及内部
参数。它们能支持分类框架，不能证明某一方法的任务增益，更不能用动态引用趋势
推断社区方法份额已经转向。

## 4. 权重训练对照：结构可借，论文增益不在当前范围

下列工作可提供数据生成器、Evaluator 或 Reward 设计，但复现其论文 improvement
需要训练/更新参数。最终 checkpoint 能冻结部署，并不改变这项归类。

| 工作 | 论文中真正发生的更新 | 可借给 Runtime 的结构 | 不能声称 |
|---|---|---|---|
| Toolformer | 采样 API calls，按其对未来 token loss 的影响过滤，再 fine-tune GPT-J | tool call/result 轨迹、基于任务损失的过滤器 | “给冻结模型挂工具就得到 Toolformer 增益” |
| Self-Instruct | 175 个 seed tasks 生成/过滤约 52K instructions，再用 OpenAI fine-tuning job 训练 | candidate 生成、去重、过滤 | “纯 in-context 自学”；API 未披露具体参数/optimizer |
| Constitutional AI / RLAIF | critique/revision 后做 SL fine-tuning；AI preference labels 训练 PM 并对 policy 做 RL | constitution-based critique、AI feedback 数据 | “部署时每轮自动学习”或“完全无人类反馈” |
| STaR | 生成 rationale，保留答案正确者；失败题用正确答案 rationalize，再反复 fine-tune | oracle filter、失败保留 | hint 后 rationale 是原始答案的真实因果轨迹 |
| Self-Rewarding LM | SFT 后让模型产生/评分 preference pairs，再迭代 DPO 到新 checkpoint | judge/eval 数据管线 | “continual improvement 已无限成立”；论文也讨论 bias/reward hacking/饱和 |
| Absolute Zero | proposer/solver 自生成可验证题，Python verifier 给 reward，再以 TRR++/REINFORCE++（AdamW）更新同一模型参数 | proposer/solver 分工、代码 verifier、learnability reward | “zero-data”等于“zero-update”；自动运行不可信代码也不能绕过 sandbox/Authority |
| TTRL | 在无标签 test data 上以多数票伪标签做 GRPO/AdamW，公式显式更新参数 θ | 多 rollout、共识/熵/多数比例作为弱证据 | “冻结模型的 majority vote 就是 TTRL”或把多数票当 oracle |
| SEAL | self-edit 触发 LoRA/full SFT 得到 θ′；外层再以 held-out reward 更新 self-edit 生成策略 | 结构化 Candidate、隔离分支、held-out reward、比较/回滚 | “冻结目标模型也保留 SEAL 机制”或允许未审核的持久自我修改 |

一手来源：

- [Toolformer，NeurIPS 2023 论文](https://proceedings.neurips.cc/paper_files/paper/2023/file/d842425e4bf79ba039352da0f658a906-Paper-Conference.pdf)
- [Self-Instruct，ACL 2023 论文](https://aclanthology.org/2023.acl-long.754/)
  与[官方代码](https://github.com/yizhongw/self-instruct)
- [Constitutional AI / RLAIF，arXiv:2212.08073](https://arxiv.org/abs/2212.08073)
  与[官方材料](https://github.com/anthropics/ConstitutionalHarmlessnessPaper)
- [STaR，NeurIPS 2022 论文](https://proceedings.neurips.cc/paper_files/paper/2022/file/639a9a172c044fbb64175b5fad42e9a5-Paper-Conference.pdf)
  与[官方代码](https://github.com/ezelikman/STaR)
- [Self-Rewarding Language Models，ICML 2024](https://proceedings.mlr.press/v235/yuan24d.html)
- [Absolute Zero，NeurIPS 2025](https://proceedings.neurips.cc/paper_files/paper/2025/file/9837dc00ff67d176373268ed48042d49-Paper-Conference.pdf)
  与[作者项目](https://andrewzh112.github.io/absolute-zero-reasoner/)
- [TTRL，NeurIPS 2025](https://proceedings.neurips.cc/paper_files/paper/2025/file/be690ea16f005c174f6c4102a5970e67-Paper-Conference.pdf)
  与[官方代码](https://github.com/PRIME-RL/TTRL)
- [SEAL，NeurIPS 2025](https://proceedings.neurips.cc/paper_files/paper/2025/file/6b41e04c41726e2a60e456d0a2b961ab-Paper-Conference.pdf)
  与[作者项目](https://jyopari.github.io/posts/seal)

## 5. 映射到 tape.systems：事实、派生状态与执行必须分层

这里使用固定的 `tape.systems` 版本
[`69a0095f3cffeedd8fa5c065ee2a338d936aac78`](https://github.com/bubbuild/tape.systems/tree/69a0095f3cffeedd8fa5c065ee2a338d936aac78)。
其核心把 Tape/Entry、Anchor、View、Handoff 分开；站点还画出了 derived Index
和 Task-as-View-Assembler/DAG。后两者是网站架构抽象，不应写成 Bub 当前已经
完整实现的实体
（[核心术语](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L55-L114)，
[Entry→Index→View 图](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/components/minimal-model.tsx#L95-L121)，
[Task/DAG 扩展](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L251-L320)）。

| 文献对象 | tape-native 表达 | 必须保留的边界 |
|---|---|---|
| environment observation、tool result、task terminal | **Entry / Outcome Entry** | 原始反馈追加保存，不被 reflection 覆盖 |
| memory index、retrieval score、search tree、active policy state | **Projection / Index** | 可重建、可 reset；不是 Tape 事实 |
| reflection、insight、Generative Agents 高层推论 | **Observation Entry** | 带 source IDs、producer、算法/模型版本、时态；可被 challenge/retire |
| 当前任务拿到的 memory/skill/prompt | **View** | 记录 exact selected IDs、rendered bytes、omissions；“可访问”不等于“语义使用” |
| ReAct action、Voyager program、tool call | **Execution** | 继续受 Authority effect/policy/approval seam 约束 |
| reward、test result、user correction、task effect | **Outcome / feedback Entry** | 与模型自评分开；环境终态不自动证明因果 |
| ExpeL insight set、Voyager skill、OPRO prompt | **versioned Policy Candidate** | evaluation 通过前不进入默认 View/Action policy |
| 评审后启用/退休/回滚 | **Policy promotion Entry + Projection** | 追加决策，不改写旧 Entry；版本必须可回退 |

这也解释了为什么 Generative Agents 的 reflection 与 ExpeL insight 应被称为
Observation，而不是原始事实：它们能压缩和泛化来源，却可能语义越界。严格
append-only 系统应追加 successor/correction/retirement Entry，再让 Projection
折出 active 版本，不能把旧推论原地覆盖。

## 6. Superchat 当前能承接什么，不能承接什么

以下是当前工作树的代码事实，不是未来设计承诺：

1. **事实底座已有。** `superchat-tape-append-material` 保存完整 trusted prompt
   material；`superchat-tape-append-event(-record)` 为 Runtime event 分配逻辑
   identity，并在入库前 redaction/size-bound
   （`superchat-tape.el:346-416`）。`superchat-projection-register/replay/catch-up/
   rebuild` 提供可重建 read model seam（`superchat-projection.el:24-99`）。
2. **View 已接入 prompt，但不是学习闭环。** prompt hook 调
   `superchat-view-assemble` 并把 envelope 附到 Turn state
   （`superchat-prompt-hooks.el:239-264`）。Continuity 默认只选 user/assistant 与
   非 Runtime system material，最近优先、上限 50 条/4000 字，并固定披露
   `:represented []`（`superchat-tape-view.el:450-469,633-713`）。
3. **普通 compact anchor 目前不是 tape.systems handoff。** 非 branch 的
   `kind=anchor` 被 Continuity 标成 `unsupported-schema`，不进入默认 prompt
   View（`superchat-tape-view.el:450-469`）。只有显式 anchor recall 才会验证
   `source_ids` 为正整数、来源齐全且都是 prompt material；否则 blocked
   （`superchat-tape-view.el:871-923`）。所以不能声称 `/compact` 后默认从
   summary anchor 继续。
4. **有 Memory Projection 与 Claim/Feedback，但没有 Outcome/Promotion 合同。**
   Memory 搜索先 catch up projection，再直接消费 FTS/LIKE 返回的物化内容
   （`superchat-memory.el:443-456`）；它没有实现“索引命中后回读并验证 raw
   source facts”。`claim.recorded` 和 `feedback.recorded` 支持 basis、
   supersession、support/challenge/correct 的追加记录
   （`superchat-claims.el:69-151,182-220`），但当前没有 anchor-bounded episode、
   reward/outcome、policy candidate/evaluation/promotion/rollback schema。
5. **Runtime/Authority 有审计摘要，不足以复原完整 provider trajectory。**
   intent/response content 主要以 size/hash 进入 payload；event content 是有界
   summary（`superchat-runtime.el:199-205,495-542`；
   `superchat-tape.el:361-406`）。Authority 保存 execution-id、request-hash、
   effects/decision 与 result size/hash，而不是完整 args/result
   （`superchat-authority.el:553-570,613-650,790-864`）。这些证据能支持身份、
   因果和审计边界，但不能独立重建最终 provider request、raw stream 或完整工具
   trajectory，更不能直接导出精确训练/学习 episode。
6. **Authority 与 Closure 应保留原职责。** promoted skill/policy 的实际工具调用
   仍必须经过 validate→effects→policy→approval→execute→Tape。Closure 当前把
   anchor、memory、claim、feedback 列为 Runtime closure 的 known-irrelevant
   （`superchat-closure.el:166-190`）；它能证明运行闭合，不能证明答案正确、
   insight 有用或 learning 已完成。
7. **Workspace 可做版本化 artifact 载体，但不是 promotion gate。** 跨 Run 读取
   要求 expected revision，并以 Closure 和 provenance 判断 deliver/wait/reject
   （`superchat-workspace.el:602-645,941-1029`）。这适合承载 prompt/skill artifact
   的不可变 revision；仍需单独的 epistemic evaluation/promotion contract。

换句话说，当前 Superchat 已有 Tape、View、Projection、Runtime、Authority、
Closure 和一部分 Claim/Feedback 地基；尚没有论文意义上的 durable self-learning
闭环。Memory recall 只证明 access，Closure 只证明运行终态，skill artifact 存在
只证明 storage；三者都不证明 content dependence、held-out task effect 或 promotion。

## 7. 最小无权重闭环：对象和证据

不引入重型训练框架时，最小状态机可以是：

```text
Episode Entries
  → Outcome / Feedback Entries
  → Observation (带 source IDs，可反驳)
  → Policy Candidate (prompt / view rule / skill artifact，版本化)
  → held-out Evaluation
  → reviewed Promotion or Rejection
  → 新任务 View/Execution
  → 新 Outcome Entries
```

每个箭头需要不同证据：

- **Access**：候选是否被选入 View；记录 exact selected IDs、rendered bytes、
  policy version、omissions。
- **Semantic use**：输出是否内容特异地依赖该候选。至少做四路盲测：真实
  observation、withheld、同长度 sham、contradictory；仅“有/无 memory”不足以
  排除长度、语气和额外 token 的影响。
- **Task effect**：在冻结的 held-out tasks 与外部 oracle 上是否稳定改善，并报告
  failure、cost、latency 和安全副作用；不拿同一模型自评替代 outcome。
- **Promotion**：评审者根据预写 gate 决定启用哪个 immutable version；失败或
  证据冲突时追加 reject/retire/rollback Entry，不删除历史。

本仓现有 semantic-use v0.2 scorer 已被独立复审判为 **VETO**；在修正测量合同
以前，不应让其结果触发任何 promotion。这个 VETO 只约束当前测量仪器，不反驳
上述论文方法，也不表示 Superchat 已经运行了四路实验。

## 8. 最小阅读与实现次序

### 8.1 阅读顺序

1. **ReAct → Self-Refine → LATS**：先理解一次运行里的 observation loop、
   feedback/refine 与 search；同时明确它们为什么不是持久学习。
2. **Generative Agents → Reflexion**：看清 raw memory、derived reflection、
   provenance 和同题跨 trial 经验的区别。
3. **ExpeL → Voyager → OPRO**：依次看自然语言 insight、可执行 skill、可评分
   prompt policy，聚焦它们如何产生候选、如何验证、如何复用。
4. **训练对照组**：最后读 Toolformer/STaR/Self-Rewarding 等，借 evaluator、
   filter 和 data-loop 结构，但不要把新 checkpoint 增益误记成冻结权重闭环。

### 8.2 实现顺序（不增加重型依赖）

1. **先修测量合同。** 修复 semantic-use v0.2 的 false-positive 边界；在任何
   learning event/schema 之前，先能可信记录 exact View delivery、first output、
   oracle 和四路对照。
2. **追加最小事件，不换存储。** 在现有 Tape/Projection 上增加 Outcome、
   Observation、Policy Candidate、Evaluation、Promotion/Retirement 的版本化
   schema；继续用 SQLite/FTS 和现有 Workspace revision，不引入训练服务、
   vector DB 或 graph DB。
3. **先做 prompt/View policy。** 它比可执行技能风险低、rollback 简单；用固定
   held-out tasks 比较旧版与候选版，只有通过 review 才进入默认 View。
4. **再做受 Authority 管理的 skill/scaffold candidate。** 复用 Workspace
   artifact + revision + Closure delivery；借 STOP 的预算封顶和 DGM 的不可变
   分支/lineage/withheld eval，新增效果审查与真实 sandbox/approval，而不是让
   模型 live monkey-patch 活动工具。
5. **最后才谈自动 proposal。** 自动生成 Observation/候选可以借 ExpeL/OPRO，
   但 promotion 默认保持人工或独立 reviewer gated；有足够反例、成本与回滚数据
   后再评估是否放宽。

这条路线的第一个交付物不是“会自学的 Agent”，而是一个不会把存储、检索、
自评或假阳性误报成学习的可审计测量与 promotion seam。

## 9. 一手来源索引

固定论文版本：

- [ReAct — arXiv:2210.03629v3](https://arxiv.org/abs/2210.03629v3)
- [Generative Agents — arXiv:2304.03442v2](https://arxiv.org/abs/2304.03442v2)
- [Reflexion — arXiv:2303.11366v4](https://arxiv.org/abs/2303.11366v4)
- [Self-Refine — arXiv:2303.17651v2](https://arxiv.org/abs/2303.17651v2)
- [ExpeL — arXiv:2308.10144v3](https://arxiv.org/abs/2308.10144v3)
- [Voyager — arXiv:2305.16291v2](https://arxiv.org/abs/2305.16291v2)
- [OPRO — arXiv:2309.03409v3](https://arxiv.org/abs/2309.03409v3)
- [LATS — arXiv:2310.04406v3](https://arxiv.org/abs/2310.04406v3)

官方代码固定提交：

- [ysymyth/ReAct @ 6bdb3a1](https://github.com/ysymyth/ReAct/tree/6bdb3a1fd38b8188fc7ba4102969fe483df8fdc9)
- [joonspk-research/generative_agents @ fe05a71](https://github.com/joonspk-research/generative_agents/tree/fe05a71d3e4ed7d10bf68aa4eda6dd995ec070f4)
- [noahshinn/reflexion @ 218cf0e](https://github.com/noahshinn/reflexion/tree/218cf0ef1df84b05ce379dd4a8e47f17766733a0)
- [madaan/self-refine @ 9a206d4](https://github.com/madaan/self-refine/tree/9a206d41e5d2d0c241bb441f41eeadb945afaa55)
- [Andrewzh112/ExpeL @ 692a677](https://github.com/Andrewzh112/ExpeL/tree/692a677c6f94757345cf3205d85ccbe55fb13d82)
- [MineDojo/Voyager @ 55e45a8](https://github.com/MineDojo/Voyager/tree/55e45a880755d0c8c66ca7fb5fe7962ac8974f89)
- [google-deepmind/opro @ a76bdce](https://github.com/google-deepmind/opro/tree/a76bdce2cbf6d4a0d1e570a6fcfe17be9c2abdd7)
- [lapisrocks/LanguageAgentTreeSearch @ 853d816](https://github.com/lapisrocks/LanguageAgentTreeSearch/tree/853d81614607dd27433faf17c7b0a7d660f95d22)

## 10. 尚未证实 / 不属于本研究

- 未独立重跑八篇论文的 benchmark、human evaluation 或 ablation；本文复述的是
  论文报告，并用“论文报告”限定，不把作者数字当成本仓实测。
- 未验证这些旧 provider/model endpoint 在 2026 年能得到相同结果，也未比较
  当前模型上的 cost/latency。
- 未证明自然语言 reflection/insight 的逐 claim correctness、长期时态一致性或
  安全性；provenance 只让它可审计。
- 未证明当前 Superchat 可精确复原 provider trajectory；现有摘要/hash 边界明确
  不足。
- 未创建新事件 schema、Projection、View policy 或实验数据；实现顺序只是
  application-design hypothesis。
- 未把任何外部论文提升为本仓 learned structure，未改变 Runtime、Authority、
  Closure、Memory、Workspace 或 Capability 的既有权威边界。
