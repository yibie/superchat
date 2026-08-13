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

## 复现

```sh
experiments/experience-learning-minimal/run-compression-probe.sh
```

产物：`experiments/experience-learning-minimal/compression-probe-2/verdict.el`
