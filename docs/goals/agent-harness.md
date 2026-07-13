# North Star: Agent Harness —— 在 Emacs 中做多代理脚手架

> 状态:**北极星文档**——不直接实现,为子目标定位与排序。
> 创建于 2026-07-13。
> 子目标:`workflow-async-engine.md`(✅ v1.2.0)、
> `subagent-async-engine.md`(✅ v1.2.1)、
> `agent-profiles.md`(✅ Phase 1+2+3;本文档为其定位)。

## 意图(用户声明)

在 Emacs 中实现类似 agent harness(Claude Code / OpenCode 这类
"围绕 LLM 的脚手架")的能力,以及多 agent 协作。此前的
agent-profiles 目标只是这个意图的一个切片;本文档把整个意图
摊开,以免逐个切片推进时丢失全局排序。

## Harness 解剖 × superchat 资产对照

一个完整的 agent harness 由约九层构成。对照(基于 2026-07-12/13
会话中逐条核实的代码事实):

| 层 | harness 里指什么 | superchat 现状 |
|---|---|---|
| Agent loop | 多轮工具调用循环 | ✅ llm.el 原生多轮 + agent-loop 包装(计数/渲染/tape) |
| 工具系统 | 注册表、门控、确认 | ✅ 29 个工具、allowlist、destructive 确认、permission hooks |
| 委派/子代理 | 隔离执行 + 汇报 | ✅ v1.2.1:异步、真并行、深度守卫、占位符渲染 |
| 上下文管理 | 压缩、记忆、检索 | ✅ tape v3 (FTS5) + /compact 锚点 + memory 自动召回 |
| Profile/角色 | 每 agent 的契约 | ✅ body/model/tools/type + 5 个 typed slots;护栏只可收紧 |
| 发现/路由 | 主代理知道有哪些 agent 可用 | ✅ delegate 工具描述从 registry 动态生成,包含自定义 `type: agent` skill |
| 编排 | 确定性的多步/扇出 | ✅ workflow 线性异步;步骤内可调 delegate 工具实现扇出(见"协作形态") |
| 控制面 | 列出/取消/超时运行中的 agent | ✅ v1.3.1:运行 registry、`/agents`、`/cancel-agent`、per-agent timeout |
| 可观测性 | 日志、审计、调试 | ✅ tape 全量记录 + tape-view 查询工具;⚠️ 只有事后查询,无实时视图(属控制面) |

结论:**九层里八层已立;v1.3 契约、发现和 profile 参数均已完成,
当前缺口集中在控制面。**

## 多 agent 协作形态支持矩阵

| 形态 | 现状 |
|---|---|
| Orchestrator-worker(主代理委派) | ✅ delegate 工具,LLM 自主决策 |
| Blackboard(黑板共享状态) | ✅ workspace 区域 + workspace_* 工具;异步模型下写入天然串行(v1.2.1 洞察 2) |
| Pipeline(线性流水) | ✅ workflow 引擎,$result/$stepN 传递 |
| Fan-out(一步扇出 N 个 worker) | ✅ workflow 步骤收集全局工具,含 delegate_to_subagent_parallel;v1.3.1 起统一经过 agent-loop 包装 |
| Peer messaging / agent teams | ❌ 明确暂缓(见"不做") |

## 真实缺口(按杠杆排序)

### 已完成的地基 — 契约层(agent-profiles Phase 1,✅ 2026-07-13)

body/model/tools/type 的运行时语义已在提交 `1e61f48` 补通:
preset body 进入 system context,model 到达 effective backend,
`tools: []` 明确表示零工具,无消费者的 backend 字段已删除。
17 个契约测试锁住主 agent、同步/异步 subagent 和显式 skill 路径。

### 已完成的缺口 1 — 发现/路由层(✅ `1152bcf`)

主代理的 LLM 只知道 researcher/executor/introspector 三个名字
(工具描述硬编码)。用户新写一个 `type: agent` 的 SKILL.md,
子代理机制能跑,但主代理永远不会主动委派给它——**多 agent
协作的入口被堵住了一半**。

方案:agent registry——`superchat-llm-tools-reload` 时枚举
内置预设 + skills 目录中 `type: agent` 的 SKILL.md,把
name + description 动态拼进 delegate 工具的 description。

关键简化:**不需要新增 when_to_use 字段**——SKILL.md 已有
`description`,Claude Code 的委派路由同样只靠 description。
这使 agent-profiles 的选型(F,5 字段)更稳:路由复用现有字段,
零新增。

### 已完成的缺口 2 — 字段层(✅ `9d5d077`、`e39a6c6`)

选型结论:**方案 F,5 个 typed slots**(temperature / max_tokens /
reasoning / max_tool_calls / confirm_destructive),tighten-only。
默认决定,维护者可否决。A 的 disallowed_tools、C 的组映射
留待真实需求出现后再加。

### 已完成的缺口 3 — 控制面(✅ v1.3.1)

- [x] 运行中子代理列表(名称、深度、已运行时长、占位符位置);
- [x] 取消:已核实 llm.el 0.24 的 `llm-chat-async` 返回 request,
  官方契约允许直接传给 `llm-cancel-request`;
- [x] per-agent timeout:全局上限 + SKILL.md `timeout` 收紧值,
  与手动取消共用 finish-once 路径;
- [x] workflow 步骤工具接入 agent-loop 包装。

### 缺口 4 — 协作层增强(远期,不急)

任务板(agents 认领任务的共享清单)等 blackboard 的结构化升级。
现有 workspace 黑板 + 报告聚合已覆盖主要价值,等真实使用中
黑板显出不够再立目标。

## 里程碑排序

- **v1.3 — harness contract(✅ 代码完成)**:基础契约(✅ `1e61f48`)→
  缺口 1 registry(✅ `1152bcf`)→ 缺口 2 F 字段
  (✅ `9d5d077`、`e39a6c6`)。三者合起来的验收标准:
  *用户写一个带
  description/model/temperature 的 `type: agent` SKILL.md,
  主代理能自主发现并委派它,它以自己的人格、模型、参数、
  收紧后的护栏运行。* 该验收标准已由 registry、preset contract、
  主/子代理参数透传和 tighten-only 护栏测试覆盖。
- **v1.3.1 — control plane(✅ 代码完成)**:缺口 3。维护者定夺(2026-07-13):
  控制面仍属 v1.3 harness 线,是同一里程碑的收尾阶段而非新的
  次版本——harness 要"管得住"才算完整。
- 更远:workflow 分支/条件、任务板、协作形态扩展——各立目标。

## 明确不做

- **Peer-to-peer agent messaging / teams**:Emacs 单实例单线程,
  报告聚合 + 黑板已覆盖协作价值;消息总线增加的协调复杂度
  在当前形态下没有对应收益。
- **自治外层循环**(while-not-done 包住 agent loop):llm.el 的
  内部多轮已是循环核心,外层再包一圈自治循环属于产品形态
  变化(Claude Code 的 headless/background 模式),不在本轮意图内。
- **workflow 分支/条件**:独立目标,与 harness 正交。

## 单实例约束(设计原则,非缺口)

所有 agent 共享同一个 Emacs:executor 写文件时用户可能正在
编辑同一 buffer;工具确认弹在用户面前。这是特性不是缺陷
(人在环内),但意味着 harness 的"自治程度"天花板低于
独立进程型 harness——控制面(缺口 3)因此比"更多自治"优先。
