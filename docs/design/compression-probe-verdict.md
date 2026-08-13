# 压缩探针判定：正确/错误的信息结构

参考：<https://nathan.rs/posts/gzip-lm/>（压缩-预测等价）

## 结论（先说）

**正确性在这个域里有真实、gzip 可见的行为结构。** 52 个真实恢复
episode，行为骨架（动作序列）留一法压缩分类 **51/52** 正确；预测性
前缀（只看失败后的第一个恢复动作，不看结果）同样 **51/52**。

**加入任务文本反而破坏信号**（63%）。任务措辞是噪声，行为是结构。

**唯一的错分是 review-treatment**：行为模式与成功类完全相同
（read 失败 → workspace_info → read 成功），但 workspace_info 被环境
破坏，实际失败。margin 只有 7 字节——压缩器"几乎确定"它是成功，但
它是错的。这精确刻画了"错误在信息上的样子"：**错误有两种，一种是
结构外的（行为模式不同，易分），一种是结构内的（模式相同但环境
折断，不可分）。**

## 证据

- 数据：9 个 trace，52 个 episode（45 recovered / 7 failed）
- 序列化：每步 `ACTION <tool> OK|FAILED`，剥离经验注入文本、scope
  字符串、id、revision
- 标签（机械规则）：首次 read 失败后出现 read OK = recovered
- 分类：留一法，score = len(gzip(corpus+episode)) − len(gzip(corpus))

| 探针 | 准确率 |
|------|--------|
| 完整行为骨架 | 51/52 (98%) |
| 预测性前缀（决定点之前） | 51/52 (98%) |
| 骨架 + 任务文本（confound 对照） | 33/52 (63%) |

- 错分 1 例：`review-treatment`（workspace_info 被 cl-letf 破坏），
  margin +7（所有正确分类的 margin 绝对值均更大，最远 −13）

## 结构发现

1. **成功模式可压缩**：read FAILED → info OK → read OK 是成功语料的
   共享骨架；失败语料全是 read FAILED 重复。压缩在未被告知规则时
   自行发现了这个结构。
2. **30B 无协助恢复的证据**：老 paired-repeat 的 control arms 在
   30B trace 里被正确标记为 recovered（模型自己发现了 workspace_info），
   在 9B trace 里为 failed。同一 arm 设计、同一标签规则，压缩分类
   依然正确——它分的是行为，不是 harness 分组。
3. **规则文本的压缩相似度被词汇频率劫持**（负面结果）：候选规则
   文本对失败语料的增量压缩代价（108）小于成功语料（121）——因为
   规则文本里 "workspace_read fails" 词汇密度高。naive 压缩相似度
   不能直接当经验适用性闸门。

## 边界

- failed 类只有 7 个样本，语料不平衡（45:7）。结构分离的证据强，
  但失败类的内部结构只被 6 个邻居表征。
- 分类器分的是"行为骨架相同与否"，不是因果。它不能预测环境破坏
  （review-treatment 就是证明）。
- 样本全部来自同一个域（workspace revision error 恢复）。跨域
  结构未验证。

## 对产品的含义

- 行为骨架是一个廉价的"经验适用性"指纹：新 episode 的行为骨架与
  某经验的历史成功骨架压缩距离小 → 该经验适用。这比手写 scope
  字符串更诚实（scope 是贴标签，骨架是结构）。
- 结构内错误（模式正确但被环境折断）是压缩方法的天花板：这类
  错误必须靠结果（tool.failed）而非结构来识别——这正是现有
  review-needs 做的事。两条路互补。

## 外部实践（精读两篇最相关的）

### 1. NEC/NAIST 故障代码克隆检测（ICSME 2018, Ishio 等）

为 NEC（万人级开发团队）做的工具：开发者发现一个 bug，用 grep 式
命令找出产品里犯同样错误的代码片段。选 NCD 的三个理由：定义对
开发者可解释（“压得小=相似”）、多语言无需 AST 分析、对改名/重排
有韧性。实现：滑窗（0.8–1.2×查询 token 数）× Deflate(zlib) ×
阈值 0.5。结果：召回 1.000（全部真克隆都被找到），精确率 0.010
（大量误报），MAP 0.741，真克隆集中在 Top-20——**它是排名工具，
不是分类器**。对比基线：归一化 Levenshtein 距离 MAP 相当但漏掉
改过变量名的克隆——NCD 的改名韧性是决定性优势。已部署到公司多
个项目。

可搬的：
- **排名优先于硬分类**：适用性打分 = 候选排序 + 用户看 Top-N，
  不做硬闸门（他们的精确率教训）
- **改名韧性正是我们剥 id 的理由**：我们的 episode 序列化剥掉
  artifact-id/revision，是同类操作
- **Deflate 足够**：zstd/xz 无实质收益

### 2. LLM 熵重排故障定位（ICSE 2025, “Revisiting Unnaturalness for APR”）

用 LLM 的 token 熵做**重排器**：mask 每行代码 → 模型给熵 → 重排
已有 FL 工具的嫌疑行列表。无需测试用例，Top-N 故障定位显著提升。
补丁分类用 entropy-delta（原文熵 − 替换文熵，为正 = 更自然 = 更可
能正确）。两点与我们的探针同构：

- **熵只做重排，不单独分类**——把熵叠在已有候选列表上（他们的
  FL 工具 ≈ 我们的 scope 匹配/view）。这正是薄 harness 的做法。
- **“自然度差”即置信度**——对应我们探针里的 margin：成功语料
  分 − 失败语料分。他们用 delta 做二值标签，我们可用 margin 大小
  做置信度。

一个补充方向：他们的“熵之眼”在模型内部（mask 查询），我们的
“压缩之眼”在环境外部（gzip）。两个都廉价、都可重排，未来可以
双眼睛交叉验证。

## 复现

```sh
experiments/experience-learning-minimal/run-compression-probe.sh
```

产物：`experiments/experience-learning-minimal/compression-probe-2/verdict.el`
