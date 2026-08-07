# 可验证 Agent 改进：外部方法论 Prior 记录

> 记录日期：2026-08-08。方法：只采信一手来源为课程主题定性；二手蒸馏仅作
> suggested 级别的方法论提示。创建本记录时没有安装任何 skill，也没有修改
> 代码、实验合同或审计输出。后续 transport 调查已整体退休，见
> `docs/research/failed-experiment-retirement-lessons-v0.md`。

## 0. 分类与生效范围

- **分类：`external methodological prior / suggested`**（按
  `docs/research/superchat-evidence-boundaries-2026-08-09.md` 的 prior/claim
  边界（原始 Experience Audit 合同已退休到 `experiments/archive/`）：suggested =
  外部暗示，只能成为 application-design hypothesis，不能成为 learned
  structure）。
- **生效日 2026-08-08，只前瞻使用**：适用于此后的设计评审与实验设计；
  不溯及任何既有结论。
- **不回填、不重分类**冻结的 DeepSeek Experience Cohort v6 与 Experience
  Audit Dataset v0（dataset-lock SHA-256 `e815b9c2…583ae`）；那些数据集的
  forced/suggested/emergent/unknown 判定保持冻结原样。
- **不授予任何权威**：本 prior 不改变 Runtime、Capability、Memory、
  Workflow、Policy 的任何接口、默认或门；不产生 Learning 主张。

## 1. 来源与各自能证明什么

**一手：Stanford CS329A 官方课程页**（<https://cs329a.stanford.edu/>，
Autumn 2025）。能独立支持的只有：课程存在及其正式议题清单——test-time
compute scaling、robust verification、learning from feedback with
tools/code、multi-step reasoning/planning、train-time scaling/RL、
open-ended evolution、search & deep research、agentic evaluations &
long-horizon tasks，以及相应论文阅读（Large Language Monkeys、Let's
Verify Step by Step、ReAct、STaR、AlphaCode、METR 长任务度量、
GDPVal、DeepScholar-Bench 等）。即"验证器、generation–verification
gap、长程评测是本领域正式研究议题"这一点有一手支撑。

**二手：非官方社区蒸馏仓库**
（<https://github.com/yusenthebot/stanford-ai-agent-skill>）的
`course/DIGEST.zh-CN.md`、`methodology/EVIDENCE_AND_LIMITS.zh-CN.md`、
`skill/SKILL.md`。该仓库自述"非官方学习资料，与 Stanford 无隶属或背书"。
其自报验证（155/155 引文 provenance、412/412 时间窗等）是仓库自述，
**本研究未独立复核**，不能作为课程视频内容的事实来源。七论点、七种
"看似进步实则退化"信号、路由式使用流程，只按 suggested prior 采纳其
方法骨架；凡需事实强度的论断（具体数字、视频时间戳引文）一律标
"未经独立复核"。

**双方共同不能证明的**：慢反馈/价值冲突领域的同等收益；同源 judge
一致性 ≠ 独立验证；pass@k 上升 ≠ 交付正确；50% time horizon ≠ 生产
SLA；当前"self-improvement"多依赖人设计的动作空间、奖励与门禁，不能
据此声称开放式自主学习已成立（此段仅见于二手仓库的
EVIDENCE_AND_LIMITS，未经独立复核）。

## 2. 固化的五个团队门槛

1. **决策对象**：每个任务先用一句话写清"正在决定什么"，再选方法；
   不按名词匹配路由（SKILL.md 路由优先级第一条）。本仓历史 transport
   调查先冻结 question/estimand 再谈仪器；该合同现已随调查归档，不是当前
   实现输入。
2. **完成证据**：完成 ≠ 参与者声明 done；完成 = 外部可复现证据满足预
   写验收（exit code、hash 链、cleanup proof、required record 字段齐
   全）。没有验收条件的任务先补验收条件再动手。
3. **可逆性与并行价值**：只并行"无数据/副作用依赖且可恢复"的节点；
   不可逆动作（写共享状态、外发、删除）进 gated path，要求最新状态 +
   独立验证 + 明确批准。并行本身不是价值，独立可回滚才是。
4. **失败保留**：失败不可抹除或粉饰；必须返回缺失信息、最后状态和
   无法继续的理由，落为机器可读 artifact（failures 向量、not_proved
   清单、fatal_error 字段）。因果链闭合后，应把完整证据组件连同哈希归档，
   从活动实现树移除过期 probe，而不是让每次失败永久占据当前工作面。
5. **能力晋升资格**：任何轨迹/配置进入复用前过准入门——正确性、来
   源多样性、安全检查同时通过；benchmark 或单次 probe 提升不自动授
   权部署、训练回写或权限扩大。对应本仓 audit 的 candidate gate 与
   `promotion_status: prohibited` 缺省。

## 3. 四角色最小职责

- **Commander**：书写决策对象、验收与停止条件；发出/收回执行授权；
  冻结与解冻工件；唯一能批准不可逆动作的人。
- **Researcher**：只读取证；区分 proved / inferred / not-proved；给
  file:line；不执行被禁动作，不把静态候选冒充已证根因。
- **Reviewer**：独立复核 claim↔evidence 对应；输出 ACCEPT/REVISE 及
  阻断项；不修饰失败，不把 reference-only 字段当作本次测量。
- **Executor**：只在授权形状内执行；单发、不重试；保留失败、守卫与
  清理证据；不扩大授权范围。

## 4. 已归档 transport probe 线的对照实例

本节只作历史方法实例。完整 probe、结果、合同与 preflight 已移入
`experiments/archive/closed-loop-transport-investigation-through-2026-08-08.zip`；
不得将其中任何文件当作当前产品门或下一实验授权。

- **验证矩阵优于口头 done**：B0（`superchat--update-status` void）、
  B0.1（curl code 7 真实 egress）、v0.2（`make-llm-ollama` void）各以
  no-run 的机器可读证据（exit code、fatal_error、guards hash、cleanup
  proof）证伪；v0.3 运行成功，但 stdout 捕获被工具截断
  （`capture.complete=false`）；
  最终 transport-runtime-live-bridge-v0 以 18 字段 required record +
  单发 + 五段 sha256 全等证明 ASCII 链路，而不是声明"集成可用"
  （仅 experiment-local 注入下成立，产品集成未证）。
- **失败保留产生下一步**：v0.2 的失败 artifact 直接给出 v0.3 的
  harness 修复点；v0.3 的截断记录直接给出 v0.4 的摘要化/封顶设计。
- **归因纪律**：B0.1 的 bypass 根因在证据不足时保持 open（last proven
  seam + missing observable），不用猜测填洞。
- **边界声明**：这套纪律是 Commander/Reviewer 显式设计的 harness
  约束（forced/suggested 级 prior 的产物），**不是 agent 自发涌现的
  学习行为**；禁止把它写进任何 Learning/emergent 叙事，禁止据此回填
  Cohort v6 的任何 prior 分类。

## 5. 明确不做

- 不安装该 skill，不引入其路由文件；
- 不修改代码、实验合同、preflight、审计输出；
- 本 prior 只用于前瞻设计评审；任何实验结论仍以本仓冻结证据为准；
  二手仓库的具体引文数字在被独立复核前不得进入验收条件。

## 来源清单

- 一手：[Stanford CS329A 课程主页](https://cs329a.stanford.edu/)（议题
  与阅读清单，2026-08-08 读取）。
- 二手（非官方，自述无背书；其自报验证未经本研究独立复核）：
  [DIGEST.zh-CN.md](https://github.com/yusenthebot/stanford-ai-agent-skill/blob/main/course/DIGEST.zh-CN.md)、
  [EVIDENCE_AND_LIMITS.zh-CN.md](https://github.com/yusenthebot/stanford-ai-agent-skill/blob/main/methodology/EVIDENCE_AND_LIMITS.zh-CN.md)、
  [SKILL.md](https://github.com/yusenthebot/stanford-ai-agent-skill/blob/main/skill/SKILL.md)。
- 本仓约定参照：`AGENTS.md`、`docs/research/harness-guardrails.md`、
  `docs/research/pi-dev-guardrails.md`、
  `docs/research/superchat-evidence-boundaries-2026-08-09.md`。
