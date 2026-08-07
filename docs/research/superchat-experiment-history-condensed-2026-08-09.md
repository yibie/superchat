# Superchat 实验史精要（截至 2026-08-09）

状态：当前历史入口。旧实验的完整文件已退休到
`experiments/archive/`，不再作为日常实现上下文读取。

## 一句话结论

Superchat 已经证明自己是一个可审计、可回放、能保存追加式事实与
Artifact lineage 的 Agent Runtime；它尚未证明 Artifact 的语义使用、任务收益、
可复用改进或 Learning。当前唯一活跃工作是离线
`Artifact Semantic Use v0.2` scorer 的独立复审。

## 阶段与结论

| 阶段 | 做了什么 | 最终结论 | 不得继承的错误结论 |
|---|---|---|---|
| Runtime Foundation / Auditability | 建立 Runtime、Tape、Authority、Closure、Projection、Workspace 与 Outcome 证据链 | Run、工具生命周期、Artifact 发布/交付和终态可以被持久化、回放和审计 | 记录了工具调用不等于模型使用了结果 |
| DeepSeek Cohort v6 | 20 个冻结 Research Workflow assignment；4 Calibration、12 Discovery、4 Replicate | 20/20 Evidence Closure，20/20 Workflow Contract，60/60 Artifact 可交付；这是可靠 Runtime 数据集 | 完成度和评分不能自动变成 Pattern、效果或 Learning |
| Experience Audit v0 | 只读检查 Artifact lineage、失败恢复、阶段转换 | 0 个合格 Pattern Observation；这是合法零结果 | Workflow 强制交接不能被重新命名为 emergent；数据中没有跨任务 Artifact reuse |
| Closed-loop Correction v0 | 追加旧 Claim、Feedback 和 replacement Claim，比较无 Claim、旧 Claim、当前 Claim | Persistence 与 Correction 通过；Consumption/Effect 无效 | 67 个模型 prompt 都含 `Question: nil`，因此评分不能解释为处理效果 |
| Transport investigation | 多轮 fake/live/Runtime/byte probe 定位 llm/plz UTF-8 与 harness 问题 | 保留了一个窄的 UTF-8 产品修复与回归测试；大量 probe 已退休 | harness 没击中 seam、动态绑定失效或输出捕获失败，不能算产品失败 |
| Context Observer Audit v0 | 尝试定义 Context Continuity O1/O2 的 finding 与 correspondence | 在 O1 前阻塞；representation 与 output item 的对应关系没有被合同唯一规定 | 文件名叫 Observer/View 不会自动产生可执行语义；不得替合同选择对应关系 |
| Experience Observation v1 | 追问 Agent 是否自然选择/复用旧 Artifact | 在 Cohort 前停止 | 当前证据可靠证明 Access，不证明 Selection、Semantic Use、Benefit 或 Learning |
| Artifact Semantic Use v0 / v0.1 | 设计 counterfactual A/B、sham、withheld 与 crossed v×x scorer | 两轮独立 VETO；发现 v-only、missingness、schedule、自验、replacement、truthy false 等假阳性 | `audit=PASS` 只覆盖写进去的反例，不能代替独立审查 |
| Artifact Semantic Use v0.2 | Luna-Max 实现严格 30-record、first-attempt、proof enum、grammar、sham/withheld/oracle gates | Commander 定向验收通过；状态仅为 `ready for independent review` | 尚未证明真实 Runtime/provider/model 中的 Semantic Use，也未授权任何请求 |

## 真正保留下来的成果

1. Tape 是追加式事实系统。后来的摘要、纠正、replacement Claim 或 View 只能追加
   新事实并改变读取视图，不能替换或抹掉旧 Tape 事实。
2. Runtime 负责 Request、Turn、Run 与终态；Authority 是所有模型工具的共同执行
   seam；Closure 只解释 Tape 是否足以证明某个 Runtime claim。
3. Workspace 能证明同一 Workspace 内、跨 Run、带 expected revision 的 Artifact
   交付资格；它不证明语义摄取。
4. Cohort v6 是可信零观察数据：Runtime 证据完整，但没有发现可脱离 Designer
   Prior 的重复结构。
5. 失败实验的正确生命周期是：保留结论与 provenance，整体压缩退休，不让过期
   文件继续像活跃权威一样出现在工作树。
6. 当前最窄的研究问题不是“Agent 学会了吗”，而是“在 exact first-output 与
   request-inclusion 都被证明后，固定构造单元的输出是否与同时依赖 Artifact 参数和
   task input 一致”。

## 当前读取规则

- 日常实现只读本文件、`superchat-evidence-boundaries-2026-08-09.md`、当前 handoff
  和 `experiments/artifact-semantic-use-v0/revisions/v0.2/`。
- 需要审计历史时，先读 archive manifest；只有明确问题需要原始证据时才解压。
- 不从旧合同、旧 probe、旧 decision map 或旧评分表直接推出新授权。

## 历史原件

完整原件按原始相对路径保存在：

```text
experiments/archive/superchat-active-experiment-materials-through-2026-08-09.zip
```

更早的历史和已退休 transport component 仍分别保存在同目录已有压缩包中。
