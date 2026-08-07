# 熵能否帮助冻结权重 Agent 学习：证据复核与 Tape 实验边界

> 研究日期：2026-08-10。范围只包括**不训练、不更新 foundation model
> 权重**的 Agent Runtime；训练论文只作为边界证据。本文使用原论文、会议页面、
> 作者代码和官方项目，不用动态引用数判断质量。它是一份研究记录，不是已实现的
> 产品规格；本次没有修改 Superchat 代码，也没有运行 provider 或在线 Agent 实验。

## 0. 结论先行

熵有用，但它更像**烟雾报警器**，不是**事故鉴定书**：它能提示“这里存在分叉、
不确定或信息不足，值得多看一眼”，不能单独说明“这一步是对的”或“这个做法导致了
成功”。

对冻结权重的 Agent Runtime，现有证据支持三种较窄的用途：

1. 在当前任务里决定**何时检索、提问、反思或多采样**；
2. 在多条当前候选中做**停止、过滤或排序**；
3. 把历史轨迹中的高不确定位置标成**待检查线索**，供独立的成功/失败对照器和
   Evaluator 做后续归因。

现有证据不支持两句更强的话：

- **“低语义熵 ≈ 成功，因此可以直接入库。”——不成立。** 低语义熵只说明多次
  生成的意思比较一致；模型可以稳定地说错。`Outcome`、环境验证、测试或人工纠正
  仍然必须独立存在。
- **“离线熵筛选 Agent 经验几乎空白。”——截至 2026-08 已不成立。** ExpSeek
  已直接用 step entropy 触发经验检索，MMPO 已把 Belief Entropy 用于 memory
  trajectory 选择，ExpWeaver 也分析了经验检索位置附近的 token entropy。更准确的
  空白是：**熵本身负责提取、验证可迁移成功模式的直接证据仍少。** 这些工作的
  pattern 或标签实际来自成功/失败对照、tool model、hindsight feedback、语义归因
  或可验证 Outcome；熵主要负责“何时看”与“先看哪条”。

因此，Superchat 若实验这条路线，熵只能成为追加到 Tape 的可争议
`Observation`，最多触发一次 `Observation Opportunity`；它不能直接晋升为
`Pattern`、Anchor 或当前 View policy。真正的最小闭环仍是：

```text
raw Entry + Outcome
        ↓
uncertainty Observation（熵只定位）
        ↓
success/failure 对照与来源归因（另一个过程提取 Pattern Claim）
        ↓
versioned Candidate
        ↓
held-out Evaluation
        ↓
append Promotion / Rejection，旧事实和旧候选仍保留
```

现有研究大多来自学术论文或研究型开源项目，不能据此声称“业界已规模化采用”。
Entropix 是未给出比较评测的研究原型；ExpSeek、MMPO 等提供研究证据，不是生产部署
证据。

## 1. 先把五种“熵”分开

同一个词在这些论文里指向不同对象。若不先写清分布是什么，数值无法比较，也无法
重放。

| 名称 | 随机变量/分布 | 回答的问题 | 冻结 Runtime 的主要代价 | 不能推出 |
|---|---|---|---|---|
| token predictive entropy | 某个生成位置的 next-token 概率分布 | “模型在这个字词位置有多少备选？” | 需要完整 logits 或足够完整的 logprobs；受 tokenizer、温度和截断影响 | 该 token 是关键 action；答案正确 |
| semantic entropy | 同一输入多次采样后，按含义聚类得到的意义分布 | “模型对答案的意思是否稳定？” | 多次生成、语义聚类；标准版还需序列概率 | 低熵答案真实；高熵答案必错 |
| answer-distribution entropy | 多次采样的最终答案/规范化 action 的经验频率 | “投票是否已经收敛？” | 多次采样和答案规范化 | 少数答案错误；多数答案是真值 |
| action/policy entropy | 语义 action、tool call 或 policy 对可执行选择的概率 | “Agent 在哪个行动分叉上不确定？” | 先定义稳定 action space，并获得其概率 | LLM token entropy 自动等于 action entropy |
| information gain | 观察前后对显式假设空间的熵差 | “问这个问题预计能减少多少未知？” | 需要 hypothesis/belief space 和观察模型 | 单次生成低熵；任务已经成功 |

三个容易混淆的点：

- “高熵 token”不是某个 token 自带的属性，而是该位置的**模型预测分布**较散；
- semantic entropy 是对**意思群**计数或加权，answer entropy 通常只对最终答案字符串
  或规范化答案计数；
- information gain 是取得观察前后的**差值**，不是把 token entropy 换一个名字。

任何写入 Tape 的熵测量都至少要记录：`kind`、模型与 tokenizer 版本、prompt/View
哈希、温度、采样数、可见 logprob 范围、聚类/归一化方法、原始 sample 引用和数值。
没有这些元数据，同一个小数既不可比较，也不可复现。

## 2. 用户所列工作的逐项复核

### 2.1 总表

| 工作 | 论文实际使用的信号 | 熵发生在哪个阶段 | foundation 权重是否不变 | 是否支持跨任务经验筛选 | 对冻结 Runtime 的判定 |
|---|---|---|---|---|---|
| Beyond the 80/20 Rule | token predictive entropy；屏蔽低熵 token 的梯度 | RLVR 训练与训练分析 | **否**，核心增益来自更新权重 | 否 | 只能借“高熵位置可能是分叉”的假设 |
| Reasoning with Exploration | token entropy；entropy bonus/advantage | PPO/GRPO 训练 | **否** | 否 | 训练证据，不能直接称 runtime self-learning |
| Semantic Uncertainty / Nature | semantic entropy | 推理后不确定性检测 | 是 | 否 | 可标 confabulation 风险，不能验真或直接入库 |
| SelfCheckGPT | 多样本一致性；核心方法不是 semantic entropy | 推理后 hallucination 检测 | 是 | 否 | 黑盒一致性基线，不是 Agent 经验机制 |
| Entropy Mechanism of RL | policy/token entropy 曲线 | RLVR 训练监控与 loss 设计 | **否** | 否 | 不能外推为冻结 Agent“学满”仪表盘 |
| FLARE | 单 token 低概率阈值，不是 Shannon entropy | 生成时触发检索 | 是，无额外训练 | 否 | 支持在线 uncertainty gating |
| Adaptive-Consistency | 最终答案分布；entropy 只是停止基线，主方法是 Beta posterior | 当前问题多次采样时 | 是 | 否 | 支持按收敛程度停采样，不沉淀经验 |
| DeepConf | top-k logprob 派生的 token confidence/self-certainty | test-time 轨迹过滤、投票、早停 | 是 | 否 | 当前题 candidate controller；有自信地错的问题 |
| Uncertainty of Thoughts | 显式 possibility set 上的预期 information gain | 当前任务提问/规划 | 是 | 否 | 支持主动补信息，不是 entropy-memory 方法 |
| Entropix | entropy + varentropy 的 sampler heuristic | token 解码时 | 是 | 否 | 未完整评测的开源原型，不是产业验证 |
| ExpSeek | assistant step 的平均 token entropy | 运行时触发 experience retrieval；阈值离线拟合 | foundation 是；controller **有拟合参数** | 是，Web agent 经验库 | 直接反例：领域不再空白；熵是 trigger，不是 pattern extractor |
| MMPO / Belief Entropy | 关于 progress/gap 的 anchor response 平均 token entropy | memory policy 训练；另有 training-free candidate probe | probe 是；主方法 **否** | 有 memory trajectory 选择，不是 pattern 提取 | 可作为 memory-quality proxy，必须配 Outcome |
| ExpWeaver | 经验自适应取用位置附近的 token entropy | prompting 版后验分析；另有 RL 版 | prompting 版是；RL 版否 | 有经验利用，不负责生成经验 | 支持“熵峰附近常需经验”的观察，不是熵控制器 |

### 2.2 高熵 token 与“关键决策点”：有相关性，不等于完成归因

**Beyond the 80/20 Rule。** 论文发现 RLVR 轨迹中少数高熵 token 经常落在逻辑连接、
转折或其他推理分叉，并在训练时只保留约 top 20% 高熵位置的梯度，低熵位置被 mask；
在所测数学推理模型上仍取得主要训练收益。这里的“20%”是 batch 中的 token 位置，
不是“Agent 轨迹的 20% steps”，更不是“这些片段都应进入 Memory”。核心实验仍是
RLVR 更新模型权重；推理期 temperature 消融只支持探索解释，没有测试经验提取或
跨任务迁移（[NeurIPS 2025 论文](https://proceedings.neurips.cc/paper_files/paper/2025/file/a797c2d2e0c1fdabf4d1ab8cd0b465c6-Paper-Conference.pdf)；
[arXiv:2506.01939](https://arxiv.org/abs/2506.01939)）。

**Reasoning with Exploration。** 该工作也把高 token entropy 与 pivotal/reflection/
rare exploratory actions 联系起来；但它的算法把 entropy 项加入 PPO/GRPO 的
advantage，训练并更新 policy。它给冻结 Runtime 的只有一个可检验假设：高熵附近
可能更值得做 observation 或 retrieval；它没有证明高熵步骤就是导致成功的步骤
（[arXiv:2506.14758](https://arxiv.org/abs/2506.14758)）。

这两篇可以支持“用熵减少人工查看范围”，不能支持“只剪高熵枝就提取出成功模式”。
成功与失败轨迹都可能在同一个分叉上高熵；真正的模式来自两组在相同前置条件下的
action、工具结果和 Outcome 差异。

### 2.3 语义熵：检测不稳定，不是无标签成功判定器

Semantic Uncertainty 把同一问题的多次回答按语义等价关系聚类，再计算意义分布的
熵；Nature 版本把它用于检测 confabulation。标准 semantic entropy 结合 sequence
probability，离散版本按 cluster 频率计算，因而可以黑盒使用。Nature 实验中通常对
一个问题生成多次答案；它在所测 QA 组合上比若干基线更好，但不是完美分类器
（[原论文，arXiv:2302.09664](https://arxiv.org/abs/2302.09664)；
[Nature 2024](https://www.nature.com/articles/s41586-024-07421-0)）。

论文自己的边界恰好否定“低语义熵即可入库”：它检测的是模型在可能任意编造时的
不确定，不会检测所有系统性、稳定且自信的错误。低 semantic entropy 的准确解释是
“这批 samples 对意思较一致”，不是“这个意思已被环境证实”。

SelfCheckGPT 同样依赖同一模型多次采样的一致性，但其核心黑盒方法是 BERTScore、
问答、n-gram 与 NLI 等 self-check，不应改写成“semantic entropy 方法”。论文还比较
了基于概率/entropy 的灰盒基线；它研究的是单次生成中的事实 hallucination 检测，
没有跨运行 Agent experience promotion
（[EMNLP 2023 论文](https://aclanthology.org/2023.emnlp-main.557/)）。

因此，semantic entropy 在 Tape 中最多承担 `uncertainty.measurement`：低熵可以降低
“意思漂移”的担忧，高熵可以触发核验；两者都不能替代 `Outcome`。

### 2.4 策略熵曲线：训练监控结论不能借给冻结 Runtime

The Entropy Mechanism of RL for Reasoning LMs 观察 RLVR 过程中 policy entropy
下降，并用 `R = -a exp(H) + b` 拟合所测设置中的 reward–entropy 关系；Clip-Cov 与
KL-Cov 直接改变训练 loss 以维持探索。论文也说明不同 policy 与 off-policy 数据可呈
不同模式。这是**模型参数训练**中的诊断和优化，不是外部 Memory/View 迭代的停止
规则（[arXiv:2505.22617](https://arxiv.org/abs/2505.22617)）。

所以“监控熵趋势就知道冻结 Agent 还能不能学”过强。外部 Candidate 是否饱和，要看
held-out Outcome、成本、回归与新领域覆盖；它与模型 next-token entropy 不是同一个
状态变量。

### 2.5 在线控制：研究较成熟，但多数用完即弃

**FLARE。** Active Retrieval Augmented Generation 不做额外训练，在预测下一句时
检查是否出现低于阈值的 token probability，并据此主动检索。它使用的是低概率触发，
不是完整分布的 Shannon entropy；机制回答“当前在哪里查资料”，不回答“哪段历史应
成为成功经验”
（[EMNLP 2023 论文](https://aclanthology.org/2023.emnlp-main.495/)）。

**Adaptive-Consistency。** 该工作让 self-consistency 根据当前答案分布是否收敛而
提前停止。论文评估了 entropy stopping，但正式 Adaptive-Consistency 主方法是
Beta posterior stopping rule；阈值和超参数在训练集上选择，foundation model 不变。
它节省当前问题的采样，不保存跨任务模式
（[arXiv:2305.11860](https://arxiv.org/abs/2305.11860)）。

**DeepConf。** 该方法不额外训练模型，用 top-k logprobs 派生 token confidence/
self-certainty，再做当前问题内的轨迹过滤、置信加权投票和早停。它虽定义 token
entropy，但控制器主要操作 confidence 聚合而非通用 entropy。论文还观察到某些题上
模型会高置信地答错，激进过滤反而损害结果；这再次说明 confidence 不能当 Outcome
（[arXiv:2508.15260](https://arxiv.org/abs/2508.15260)）。

**Uncertainty of Thoughts。** UoT 是 plug-and-play、冻结权重的信息搜寻规划：它
显式列举 possibility set，估算候选问题带来的预期 entropy reduction，然后选择预期
information gain 较高的问题。它不是 token entropy 或 semantic entropy，也没有
跨任务 Memory；价值在于说明“减少哪个未知”比单纯问“模型自信吗”更接近 Agent
行动语义（[NeurIPS 2024 论文](https://papers.nips.cc/paper_files/paper/2024/file/2b0e14abd8128e6bf98b6b0bec1cfcbf-Paper-Conference.pdf)；
[作者代码](https://github.com/zhiyuanhubj/UoT)）。

这些工作支持把不确定性做成**当前回合的 controller**。把同样信号持久化并治理为
跨任务 Candidate，仍需要另一套来源、评估和 promotion 合同。

### 2.6 Entropix：可以借实现思路，不能当成实证

Entropix 是 entropy/varentropy 驱动的开源 sampler。固定源码包含几类阈值分支：
低熵低 varentropy 接受 token，高熵低 varentropy 插入一个指定 token，高熵高
varentropy 重新采样；低熵高 varentropy 的 tree search 仍标为 TODO。README 把项目
标为研究/WIP，并没有给出承诺中的完整比较评测。因此它证明“这种 sampler 可以写”，
没有证明“熵控制普遍提高 Agent 成功率”，更没有 durable self-learning 或经验提取
（[固定 README](https://github.com/xjdr-alt/entropix/blob/0a7f7bb25ee50e5fc54a1b59953a718de1a6e1ef/README.md)；
[固定 sampler 源码](https://github.com/xjdr-alt/entropix/blob/0a7f7bb25ee50e5fc54a1b59953a718de1a6e1ef/entropix/sampler.py)）。

## 3. 2026 年直接证据：领域不再“几乎空白”

### 3.1 ExpSeek：熵触发经验，但经验不是熵提取的

ExpSeek 是目前最直接的反例。Web agent 在每个 assistant step 计算平均 token
entropy；超过过程/答案阈值时，controller 才从 experience base 取经验。foundation
agent 权重保持不变，但系统不是“零拟合”：它在带标签训练轨迹上用 logistic
regression 和 1,000 次 bootstrap 拟合阈值区间，再做概率式 intervention。

经验库的内容来自另一条管线：成功/失败 trajectory pair 交给 tool model 对照，生成
`Behavior / Mistake / Guidance` triplet，并分成 process 与 final experience pool。
换句话说：

```text
entropy = 何时请求经验
success/failure pair + tool model = 经验内容与方向
```

论文报告 process-step entropy 区分 correct/incorrect 的 AUC 只有 0.6223，answer-step
为 0.7187；统计上有信号，但重叠很大，不能当可靠的逐步成功标签。其 Web benchmark
结果支持 experience seeking，不支持把每个高熵 step 直接叫作错误或 pattern。
硬件成本也不可忽略：官方 README 的默认配置为 inference 与 entropy server 各用
2×A100；小模型推理可缩到单卡，但熵服务仍是额外资源
（[ACL 2026 论文](https://aclanthology.org/2026.findings-acl.1462/)；
[固定 README](https://github.com/WYRipple/ExpSeek/blob/acc2e7f212bc793ed257550e32400a7b0974c708/README.md)）。

还存在一个必须保留的复现边界：论文方法描述让 tool model 给每个 step 赋 binary
correctness，并以 incorrect steps 构造失败集合；但当前固定实现的 threshold script
把失败 trajectory 的所有 intermediate steps 计为 `process_false`，把成功 trajectory
的所有 intermediate steps 计为 `process_true`，没有读取逐 step tool label。论文的
细粒度标签叙述与该脚本的阈值拟合口径并不相同
（[固定阈值脚本，第 49–108 行](https://github.com/WYRipple/ExpSeek/blob/acc2e7f212bc793ed257550e32400a7b0974c708/offline/step5_entropy_threshold.py#L49-L108)）。

对 Superchat 的直接启示不是“照搬阈值”，而是把三个对象拆开：熵测量、经验 Claim、
Outcome 标签。若把失败 run 中每一个中间 step 都当错误，会把早期正确动作和后续失败
混在一起，无法建立可信 credit assignment。

### 3.2 MMPO：Belief Entropy 能挑 memory trajectory，仍不等于验真

MMPO 的主算法用 GRPO/AdamW 训练 memory management policy，超出本稿产品范围；
但论文先做了一个 training-free Belief Entropy probe：让模型根据当前 memory 回答
“任务进度与仍缺什么信息”，计算该 anchor response 的平均 token predictive
entropy。成功轨迹通常下降，失败轨迹上升或停滞；不训练地从 5 条 candidate memory
trajectory 中选择最低熵一条，在不同 context length 上也有小幅准确率增益。论文报告
`ΔH_BE` 与 accuracy 的 Pearson `r=-0.684`，说明它可作为长程 memory quality 的代理
信号（[arXiv:2605.30159](https://arxiv.org/abs/2605.30159)）。

边界同样明确：论文没有把 Belief Entropy 当 standalone correctness reward，而是与
verifiable Outcome 结合；直接对最终答案算 entropy 的替代方案更差，并可能鼓励过早
自信。它支持“在已有候选中选择较连贯的 memory trajectory”，不支持“低熵内容自动
变成可迁移成功模式”。

### 3.3 ExpWeaver：经验调用常出现在熵峰附近，但熵是后验分析

ExpWeaver 的 prompting 版保持模型参数不变，在推理中提供可选 `[Retrieve]` 资源，
让 foundation model 自己决定何时取经验；作者随后发现实际经验使用位置常靠近 token
entropy peak，并用有针对性的 removal 与随机 removal 比较其重要性。这里的 entropy
是后验分析，不是 online trigger。论文另有 GRPO 训练版，不能混入冻结权重结论
（[arXiv:2605.07164v1](https://arxiv.org/html/2605.07164v1)）。

这项工作进一步支持“高熵附近可能需要经验”，但仍由模型和资源接口决定取什么；熵
没有生成或验证经验。

## 4. 邻近研究说明：pattern extraction 已存在，但靠的不是熵

三项 2026 工作能帮助划清真正的空白：

- **AUQ** 是 training-free uncertainty-aware controller。它把 verbalized confidence
  与解释留在当前 trajectory，低置信触发 reflection、Best-of-N 或 memory expansion；
  它不是 entropy 方法，也没有建立跨任务经验库。论文自己的错误分析包含模型自信地
  确认错误的情况，说明 verbal confidence 同样不能做真值
  （[arXiv:2601.15703](https://arxiv.org/abs/2601.15703)）。
- **Critic Experience Bank** 用冻结的 API critic 在完整 trajectory 后追加 hindsight
  pseudo-label，把历史 step 分为 productive/unproductive，并取回相似案例校准未来
  step confidence；它不使用 entropy。它直接表明 trajectory depth 中的 token、
  consistency 和口头自信信号不稳定，执行后的 hindsight feedback 才能给 step 方向
  （[arXiv:2607.12397](https://arxiv.org/abs/2607.12397)）。
- **Trajectory-Informed Memory Generation** 用 LLM 做 trajectory intelligence、
  decision attribution 与 contextual learning，生成带来源的 strategy/recovery/
  optimization tip；基础 agent 权重不变，也没有 entropy。它说明 held-out task 上的
  pattern/memory transfer 已有人研究，但 pattern extractor 是语义与归因管线
  （[arXiv:2603.10600](https://arxiv.org/abs/2603.10600)）。

后两篇在当前日期仍是新近 preprint/technical report，不能当成已充分复现的定论；
它们仍足以否定“没有经验筛选研究”的广泛说法。

准确的研究边界应写成：

> 冻结权重 Agent 已有 uncertainty-aware retrieval、memory selection、hindsight
> experience bank 和 trajectory-informed pattern extraction；但**entropy 本身进行
> 跨任务 pattern extraction 与因果 credit assignment**的证据仍少。可靠系统把熵当
> 定位器，把 Outcome/对照/critic 当判据。

## 5. 映射到 Superchat 的 Tape / Outcome / Observation / Candidate / Evaluation

### 5.1 不改变 Tape 合同

熵不改变 `tape.systems` 的抽象：原始事实只追加，派生项带来源，View 临时选材。
它不应该成为覆盖旧 Entry 的“新真相”。一个实验性的对象映射如下；名称是研究草案，
不是已交付 event schema。

| 层 | 追加内容 | 熵的权限 | 必备来源关系 |
|---|---|---|---|
| raw Entry | 请求、所用 View、模型/工具版本、assistant action、tool result、环境返回 | 无；先保存可审计事实 | Run/Turn、causation、版本与原始 payload |
| Outcome | 测试通过/失败、环境 reward、用户纠正、任务完成证据 | 熵不能生成 Outcome | 指向被评价 Run/Action 和 verifier |
| Observation | entropy kind/value、samples、clusters、阈值、测量配置 | 只表示不确定或分叉线索 | `source_ids` 指向原始生成与样本 |
| Observation Opportunity | “此 step 值得检索/核验/对照” | 熵可触发，但不判成功 | 指向 Observation 与触发 policy 版本 |
| Pattern Claim | 适用前提、建议 action、反例、成功/失败证据 | 熵只能帮助选片段 | 同时引用正例、负例与 Outcome |
| Candidate | 有版本的 View/retrieval/prompt/action policy | 熵可作为一个 feature | 指向 Pattern Claims、生成器和变更说明 |
| Evaluation | held-out success、成本、回归、sham/contradictory 结果 | 判断 Candidate，不判断历史真假 | 指向固定 task split、baseline、Candidate 版本 |
| Promotion/Rejection | 人工或受控 policy 的采用决定 | 不能由单个熵阈值自动完成 | 指向 Evaluation；旧 Candidate 不删除 |

Anchor 可以帮助定位或携带某个已审查的派生状态，但“高熵”不使一段原始轨迹自动变成
Anchor。View 可以按 entropy Observation 多取或少取某些 Entry，但应保留
“哪个派生项对应哪些 raw Entry”的来源说明；这不是对 Tape 历史的替换。

### 5.2 当前实现能力边界

Superchat 当前 [superchat-llm.el](../../superchat-llm.el) 的 transport 只消费规范化
文本、tool call/result 与完成状态，没有持久化 logits/logprobs 字段。因此，直接
token entropy **目前不可用**；
provider 是否返回 top-k logprobs 也不能假设为统一能力。只看到 top-k 时得到的是截断
近似，还会随 provider 的归一化方式变化。

当前 provider-agnostic 路线可以做的是黑盒多采样后的 answer/action-distribution
entropy，或再加语义聚类得到 discrete semantic entropy；代价是调用次数和聚类误差。
这也是为什么第一步不应为 entropy 重构 Runtime，而应先用离线/shadow 实验验证它比
随机 gating 多带来什么。

## 6. 最小实验：先检验“是否值得触发”，再检验“是否帮助提取”

### 6.1 问题与不变量

实验只回答两个可证伪问题：

1. 在相同触发次数和调用成本下，不确定性 gating 是否比随机 gating 更会选中“取回
   经验后可改善 Outcome”的 step？
2. 在相同 extractor、token budget 与经验条数下，让 extractor 优先看高熵 step，
   是否比看全轨迹或随机 step 产生更好的 held-out transfer？

固定 model、prompt、tools、View policy、task split 和 evaluator；不更新模型权重。
Outcome 必须来自环境、测试或人工纠正，不能由同一 entropy 信号回填。先在已有 Tape
离线 replay，随后 shadow mode；只有验证通过才允许人审 Candidate。

### 6.2 四个 gating 对照组

在 held-out tasks 上设置：

1. **no-experience**：不取回经验；
2. **always-on**：每个可比 step 都给同一批经验；
3. **random-gated**：随机触发，但匹配 uncertainty 组的触发率、经验条数和 token 成本；
4. **uncertainty-gated**：根据预注册阈值触发。

当前 Superchat 的最小 uncertainty 信号可用 `N=5` 次独立 sample 的规范化 action/
answer distribution entropy；若做语义聚类，必须盲化并固定 cluster 规则。只有当 provider
能返回足够完整、可记录的 logits/logprobs 时，才另开 token entropy arm，不能把
top-k 截断值冒充完整熵。

主要指标不是 entropy 自身，而是：任务 success、工具错误、人工纠正、trigger precision、
每次新增成功的调用/token/延迟成本，以及相对 random-gated 的增益。阈值只在 train
split 选择，held-out 结果只读一次。

### 6.3 pattern extraction 的独立对照

从 outcome-matched 的成功/失败 threads 构造三组相同长度输入：

- extractor 看全轨迹；
- extractor 只看 entropy-ranked step 及其前后窗口；
- extractor 看随机 step 窗口，数量和 token budget 与 entropy 组匹配。

extractor 输出带 `source_ids` 的 `Pattern Claim`：适用前提、建议行为、已知失败条件、
正反例和置信说明。熵只决定给 extractor 哪些窗口，不参与 Claim 的成功标签。

每个 Candidate 用四格 semantic-use 对照评估：

| 条件 | 用途 |
|---|---|
| real | 给真实相关经验，测是否改善 Outcome |
| withheld | 不给经验，建立基础线 |
| sham | 给形式相似但无关经验，测 placebo/额外上下文效应 |
| contradictory | 给与候选相反的经验，测模型是否盲从 Memory |

再测未见任务迁移、已知旧任务回归、错误经验拒绝率和成本。只有独立 Outcome 上的
净收益才能支持 promotion。

### 6.4 预注册决策规则

- uncertainty-gated 若在匹配成本下不优于 random-gated：不产品化 entropy；
- gating 改善检索，但 entropy-ranked extraction 不改善 held-out transfer：只保留
  entropy 作为 retrieval controller，不宣称 self-learning pattern extraction；
- 两者都有稳定净收益，且 sham/contradictory 与回归门通过：追加 versioned Candidate
  的 promotion；
- 任一版本失败：追加 Rejection/retirement，View 回到已知安全版本，旧 Entry 不删除。

最安全的执行顺序是：`offline replay → shadow retrieval → human-gated Candidate`。
没有必要一开始就接 provider logits、自动写 Skill 或让当前 View 自行覆写。

## 7. 已证实、尚未证实、下一步

### 已证实

- token entropy 在若干推理/RL 研究中与分叉或探索位置相关，但最强训练结果更新了
  权重；
- semantic entropy、多样本一致性可以检测一部分不稳定/confabulation 风险，不能
  检测所有稳定错误；
- 冻结权重 Runtime 已有 uncertainty-triggered retrieval、adaptive sampling、
  information-seeking 和 memory candidate selection 的直接研究；
- ExpSeek 已把平均 token entropy 接到经验库触发器，明确推翻“几乎没有这类工作”；
- 直接 pattern 内容仍由成功/失败对照、tool/hindsight model、语义归因与 Outcome
  产生，而不是由熵产生。

### 尚未证实

- 高 token entropy step 比 action-space entropy 或信息增益更适合通用 tool Agent；
- 用 entropy-ranked 片段能提高跨任务 Pattern Claim 的准确性与迁移，而不只是节省
  extractor token；
- 黑盒 semantic/answer entropy 的调用成本在 Superchat 实际 provider 上值得；
- 任何统一阈值能跨模型、tokenizer、温度、任务与 View policy 保持校准；
- 研究原型已经构成可靠、低成本的产业部署方案。

### 下一步

不先改 Runtime。先从现有 Tape 选一组有独立 Outcome 的成功/失败 Run，冻结 task
split 和测量合同，完成上述离线四组 gating 与三组 extraction 对照。只有 entropy
相对随机控制表现出可重复的**增量价值**，才值得设计 provider capability、事件 schema
与 UI。

## 8. 一手资料索引

- [Beyond the 80/20 Rule, NeurIPS 2025](https://proceedings.neurips.cc/paper_files/paper/2025/file/a797c2d2e0c1fdabf4d1ab8cd0b465c6-Paper-Conference.pdf)
- [Reasoning with Exploration: An Entropy Perspective](https://arxiv.org/abs/2506.14758)
- [Semantic Uncertainty](https://arxiv.org/abs/2302.09664)
- [Detecting hallucinations in large language models using semantic entropy, Nature 2024](https://www.nature.com/articles/s41586-024-07421-0)
- [SelfCheckGPT, EMNLP 2023](https://aclanthology.org/2023.emnlp-main.557/)
- [The Entropy Mechanism of RL for Reasoning LMs](https://arxiv.org/abs/2505.22617)
- [FLARE / Active Retrieval Augmented Generation, EMNLP 2023](https://aclanthology.org/2023.emnlp-main.495/)
- [Adaptive-Consistency](https://arxiv.org/abs/2305.11860)
- [Deep Think with Confidence / DeepConf](https://arxiv.org/abs/2508.15260)
- [Uncertainty of Thoughts, NeurIPS 2024](https://papers.nips.cc/paper_files/paper/2024/file/2b0e14abd8128e6bf98b6b0bec1cfcbf-Paper-Conference.pdf)
- [Entropix，固定提交](https://github.com/xjdr-alt/entropix/tree/0a7f7bb25ee50e5fc54a1b59953a718de1a6e1ef)
- [ExpSeek, ACL Findings 2026](https://aclanthology.org/2026.findings-acl.1462/)
- [MMPO / Belief Entropy](https://arxiv.org/abs/2605.30159)
- [ExpWeaver / Rethinking Experience Utilization](https://arxiv.org/html/2605.07164v1)
- [AUQ](https://arxiv.org/abs/2601.15703)
- [Critic Experience Bank](https://arxiv.org/abs/2607.12397)
- [Trajectory-Informed Memory Generation](https://arxiv.org/abs/2603.10600)
