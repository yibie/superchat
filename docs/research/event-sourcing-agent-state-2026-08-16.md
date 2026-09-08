# Event Sourcing as Foundation for Agent State

> 研究日期：2026-08-16。本文从 Event Sourcing 和 CQRS 文献出发，将 Superchat
> 已有的 Tape / Projection / Closure / Runtime 架构放入该框架下审视，并提出
> 面向诊断、可观测性和因果分析的下一批 Projection 设计。本文是一份研究记录，
> 不是已实现的产品规格；本次未修改 Superchat 代码。

## 0. 结论先行

Event Sourcing 已经回答了“Agent 应该如何记住自身状态”这个问题的**存储层**。
Superchat 的 Tape + Projection + Closure 三件套恰好实现了这套模式中最关键的分离：
事实存储（Tape）、派生读取（Projection）和合同验证（Closure）。剩下没解决的问题
主要在**分析层**——如何从 agent 事件流里提炼出人类可理解的诊断信息，以及如何把
诊断变成可操作的反馈回路。

诊断需求可以按四层递进来组织：

1. **Timeline View**：回答“这段 Run 里到底发生了什么？”——把分散的 Tape 事件
   拼成按时间排列的人类可读叙述。
2. **Diff/Material View**：回答“这次变动的净效果是什么？”——把事件折叠成
   Workspace/文件/状态的 before/after。
3. **Causation View**：回答“这个错误是怎么导致的？”——按 causation chain 从
   症状追溯到上游事件。
4. **Health/Statistics View**：回答“这段时期整体有什么问题？”——跨 Session/Run
   的聚合指标（latency、success rate、错误聚类）。

前三层在当前 Tape schema 中已有足够的事件类型和身份信息来支撑。第四层需要新增
少量元数据字段（latency ticks、模型标识、错误分类标签），但不需要改变 append-only
合同。

## 1. Event Sourcing 与 CQRS：基本模式

### 1.1 核心概念

Event Sourcing 的核心理念是：**不存储当前状态，存储导致状态变化的所有事件**。
当前状态是对事件流进行 `foldl`（左折叠）的结果。

在传统 ES/CQRS 术语中：

| 概念 | 定义 | 类比 |
|---|---|---|
| Event Store | 只追加的事件持久化层 | 数据库的 WAL，但是主要存储 |
| Event | 已发生的不可变事实 | 一条记录：谁在何时做了什么 |
| Aggregate | 事件的作用域/一致性边界 | 一个“账户”是一组事件的聚合根 |
| Command | 触发事件的意图表达 | “转账 100 元” |
| Projection | 从事件流派生的读模型 | `foldl apply initialState events` |
| CQRS | Command（写）与 Query（读）分离 | 写入走事件流，读取走物化视图 |
| Snapshot | 某时间点的聚合状态缓存 | 避免每次从头折叠 |
| Causation ID | 事件之间的“因为哪个事件”链 | 调试时沿链条回溯 |
| Correlation ID | 跨聚合/跨服务的追踪标识 | 分布式追踪的 trace ID |

### 1.2 为什么 Event Sourcing 天然适合 Agent 系统

Agent 系统与 ES 的匹配度极高，原因有三：

**第一，Agent 的执行天然是事件序列。** 用户输入 → LLM 调用 → 工具请求 → 工具
执行 → 工具结果 → LLM 再调用 → 最终响应。这不是一个“输入→输出”的快照，而是
一段有时序、有因果、有分支的事件流。如果用传统 ORM（存当前状态的快照），你会
丢掉所有中间过程；只有当“中间过程就是产品本身”时，ES 的额外开销才值回票价。

**第二，调试 Agent 需要时间旅行。** 当 Agent 做错事时，你最常问的问题不是“当前
状态是什么”而是“它是怎么走到这一步的”。ES 的 append-only 日志天然支持时间旅行：
回放到任意时间点，检查当时的 View、工具结果和模型输出。

**第三，Agent 的“学习”依赖于可审计的历史。** 无论是人类审查、自动评估还是未来
的 self-learning，都需要一个不可篡改的事实基础。ES 的不可变性给了这个基础。

### 1.3 经典文献摘要

| 来源 | 核心贡献 | 与 Agent 的相关性 |
|---|---|---|
| Greg Young (2010), *CQRS and Event Sourcing* | 首次系统阐述 CQRS/ES 模式；Event Store 是真相来源，Projection 是可丢弃的 | 奠定了“事件流是主存储”的范式 |
| Martin Fowler (2005, 2011), *Event Sourcing* 与 *CQRS* | 用通俗语言解释模式及权衡 | 明确了什么时候该用、什么时候不该用 |
| Microsoft patterns & practices (2017), *CQRS Journey* | ES 在企业系统中的大规模实战经验 | Snapshot、版本迁移、最终一致性 |
| Vernon (2013), *Implementing Domain-Driven Design* | 将 ES 嵌入 DDD 的 Aggregate/Event 框架 | 事件粒度与聚合边界 |
| Kleppmann (2017), *Designing Data-Intensive Applications* | Ch.11：流处理、派生数据与 CQRS 的统一视角 | 物化视图（Projection）与原始日志的关系 |

## 2. Superchat Tape → ES/CQRS 映射

Superchat 的当前架构在 ES/CQRS 框架下的定位非常清晰：

### 2.1 完整映射表

| ES/CQRS 概念 | Superchat 实现 | 文件 |
|---|---|---|
| Event Store | SQLite `tape` 表（WAL 模式，append 为主） | `superchat-db.el` |
| Event | `superchat-tape-append-event` 生成的 Runtime event | `superchat-tape.el` |
| Event ID（逻辑） | `evt_<sha256>` 格式，独立于 storage cursor | `superchat-tape.el` L170 |
| Event ID（物理） | SQLite 自增 `id`，全局单调 | `superchat-db.el` schema |
| Aggregate（Turn） | `:turn-id` envelope key | `superchat-tape.el` envelope |
| Aggregate（Run） | `:run-id` envelope key | `superchat-tape.el` envelope |
| Causation | `:causation-id` envelope key | `superchat-tape.el` envelope |
| Correlation | `:correlation-id` envelope key（跨 Run 追踪） | `superchat-tape.el` envelope |
| Actor identity | `:actor-id` envelope key | `superchat-tape.el` envelope |
| Parent Run | `:parent-run-id`（Subagent/Team 委派链） | `superchat-tape.el` envelope |
| Projection（纯） | `superchat-projection-replay` — 纯函数折叠 | `superchat-projection.el` |
| Projection（物化） | `superchat-projection-catch-up` — checkpoint 增量 | `superchat-projection.el` |
| Snapshot | `projection_checkpoint` 表存储 (name, version, last_id) | `superchat-db.el` |
| CQRS 写端 | `superchat-tape-append-event` / `superchat-tape-append-material` | `superchat-tape.el` |
| CQRS 读端 | `superchat-db-tape-select`（只读 SQL） + View 函数 | `superchat-tape-view.el` |
| 合同验证 | `superchat-closure-judge` — 纯解释器 | `superchat-closure.el` |
| 业务逻辑执行 | `superchat-runtime.el` — Run 生命周期 | `superchat-runtime.el` |
| 权限/审批 | `superchat-authority.el` — validate → policy → approve → execute | `superchat-authority.el` |
| Recovery | `tape-recovery/` 侧车 + `superchat-tape--reconcile-recovery-events` | `superchat-tape.el` |
| Session（tenant 隔离）| `session_id` 列 + `idx_tape_session` 索引 | `superchat-db.el` |

### 2.2 当前架构的优势（相对于 ES 最佳实践）

1. **逻辑 ID 与物理 ID 分离。** `evt_<sha256>` 是内容寻址、跨存储可比较的逻辑
   身份；SQLite `id` 是局部存储指针。这比 Bub 的单 Tape 内从 1 开始的数字 ID
   更接近 ES 最佳实践，支持未来的跨存储复制。

2. **Projection 有版本和 checkpoint。** `projection_checkpoint` 表 + `superchat-projection-catch-up`
   实现了标准的物化视图增量更新；Projection 可 rebuild（从 Tape 重放），不会因
   派生表损坏而丢失数据。

3. **闭合验证独立于存储。** `superchat-closure.el` 是纯函数解释器：它接收事件
   列表，返回判断。这相当于 ES 中的“对聚合的合同验证”。它能证明 Run 是否终态
   闭合、工具链是否完整、pending effects/approvals 是否已清偿。

4. **Causation chain 内置。** 每个 Runtime event 都携带 `:causation-id`，允许
   从任意事件沿链回溯。这是时间旅行调试的硬件基础。

5. **Actor 和 Profile 分离。** `:actor-id` 记录执行者身份，`:profile-id` 记录
   角色定义；两者不耦合。这符合 ES 中“事件携带执行上下文”的原则。

### 2.3 当前架构的差距（相对于 ES 理想状态）

1. **Projection 缺少标准分析层。** 当前 Projection 主要用于 Session/Workspace/
   Memory 等运行时读模型；缺少专门面向人类诊断的分析性 Projection（如 Run
   timeline、错误聚合、跨 Session 统计）。

2. **事件 payload 是摘要，不是完整参数。** `tool.requested` 和 `tool.completed`
   的 content 是最多 500 字的摘要；完整 args/result 只保存 size + hash。这保护了
   敏感数据，但也意味着不能从 Tape 完整还原 provider 交互。

3. **Causation chain 存储在 meta 中，不可直接做 SQL 图遍历。** JSON 字段中的
   `causation_id` 需要应用层解析；不能直接用递归 CTE 追溯因果链。

4. **缺少 latency/performance 元数据。** 当前事件不记录 timing。要做性能诊断，
   需要从 `created_at` 推断，但 `created_at` 是插入时间而非事件发生时间。

5. **错误分类依赖 content 模式匹配。** 没有显式的 `error_kind` 或 `severity`
   标签。工具错误和模型错误在事件层无法区分。

## 3. 诊断性 Projection：设计蓝图

以下四类诊断 Projection 均基于**当前 Tape schema 已有的事件类型和 envelope keys**
设计。它们不需要修改 Tape 的 append-only 合同，只需要新增注册的物化 Projection。

### 3.1 Timeline Projection：Run 的时间线叙述

**目的**：把一条 Run 的所有事件展开为人类可读的时间线，回答“这个 Run 里发生了
什么？”

**输入**：单个 `run-id`，从 Tape 回放的按时间排序的 Runtime events。

**输出结构**（伪 JSON）：

```json
{
  "run_id": "run_abc123",
  "actor_id": "agent.coder",
  "profile_id": "profile.coder",
  "capability": "coding",
  "started_at": "2026-08-16T10:00:00.000Z",
  "completed_at": "2026-08-16T10:02:15.000Z",
  "exit_status": "completed",
  "tool_calls": [
    {
      "seq": 1,
      "tool": "read-file",
      "requested_at": "10:00:03.000Z",
      "started_at": "10:00:03.100Z",
      "completed_at": "10:00:03.250Z",
      "disposition": "discharged",
      "summary": "read-file: /path/to/file.el"
    }
  ],
  "approvals": [
    {
      "tool": "shell-command",
      "requested_at": "10:01:00.000Z",
      "decided_at": "10:01:05.000Z",
      "disposition": "allow"
    }
  ],
  "errors": [],
  "child_runs": [
    {
      "run_id": "run_def456",
      "parent_run_id": "run_abc123",
      "actor_id": "agent.researcher",
      "started_at": "10:01:10.000Z"
    }
  ]
}
```

**实现路径**：

- `superchat-tape-replay-events` 按 session-id 回放 → 筛选 `run-id` 匹配的事件 →
  按 `recorded-at` 排序 → 按事件类型分组组装。
- 使用 `:causation-id` 连接 tool.requested → tool.completed 对。
- 使用 `:parent-run-id` 跨 Session 查找子代理事件（需要子代理 Session 也已知）。
- 需要新增字段：`duration_ms`（两个 `recorded_at` 的差值），可在 Projection 计算，
  无需写入 Tape。

**测试覆盖**：需要验证：单 Run 所有事件都被包含；排序正确；tool.approval 链完整；
子代理跨 Session 引用正确解析。

### 3.2 Causation Graph Projection：因果链回溯

**目的**：给定一个失败事件（如 `tool.failed` 或 `run.failed`），沿 causation chain
向上追溯 root cause。

**输入**：一个 `event-id` + 其所在 `session-id` 的全部事件。

**输出结构**：

```json
{
  "symptom_event": "evt_xxx",
  "symptom_type": "tool.failed",
  "causation_chain": [
    {
      "depth": 0,
      "event_id": "evt_xxx",
      "event_type": "tool.failed",
      "summary": "shell-command: npm install failed: EACCES"
    },
    {
      "depth": 1,
      "event_id": "evt_yyy",
      "event_type": "tool.execution.started",
      "summary": "shell-command: npm install"
    },
    {
      "depth": 2,
      "event_id": "evt_zzz",
      "event_type": "approval.decided",
      "disposition": "allow"
    },
    {
      "depth": 3,
      "event_id": "evt_www",
      "event_type": "tool.requested",
      "summary": "shell-command: npm install"
    }
  ],
  "contributing_events": [
    {
      "relation": "same_run",
      "event_id": "evt_vvv",
      "event_type": "tool.completed",
      "summary": "write-file: package.json"
    }
  ]
}
```

**算法**：

1. 从目标事件开始，取 `:causation-id` → 查找上游事件 → 上溯直到根（没有
   `:causation-id` 的事件或 `intent.received`）。
2. 向上游查找时，遇到 `approval.requested` 或 `tool.requested` 时标记为决策点。
3. 在同一个 Run 内，收集所有与 causation chain 中事件有 `:correlation-id` 或
   共享 `:tool-name` 的其他事件作为“关联事件”。

**当前限制**：causation 信息在 JSON meta 中，无法用 SQL 直接递归。需要应用层
构建内存中的事件索引（`event-id → event` map）后做 BFS/DFS。

**实现路径**：注册纯 Projection（不需要 checkpoint，因为输入是整个 Run 的事件
列表，实时回放即可）。可以做成 `/debug causation <event-id>` 命令。

### 3.3 Diff/Material Projection：Workspace 的净变动视图

**目的**：给定一段 Tape 范围（某个 Run 或某个时间窗口），展示对 Workspace 的
净文件变动。

**输入**：`session-id` + 可选的 `since-id` / `until-id`。

**输出结构**：

```json
{
  "session_id": "sess_abc",
  "range": {"from_id": 42, "to_id": 100},
  "files": [
    {
      "path": "src/foo.el",
      "operations": [
        {"type": "read", "tool": "read-file", "count": 3},
        {"type": "write", "tool": "write-file", "event_id": "evt_xxx"},
        {"type": "write", "tool": "write-file", "event_id": "evt_yyy"}
      ],
      "net_effect": "modified",
      "last_write_event": "evt_yyy"
    }
  ],
  "commands": [
    {"tool": "shell-command", "summary": "npm test", "count": 2}
  ]
}
```

**算法**：

1. 回放 Tape 范围，只保留 `kind=tool_call` (requested) 和 `kind=tool_result`
   (completed/failed) 事件。
2. 按工具名分组：`read-file`/`write-file`/`append-file`/`shell-command`。
3. 文件操作按 target path 聚合（path 从 tool_summary 中解析，或从 payload
   的 tool_name + 摘要中提取）。
4. 对每个文件，判断最终效果：如果只有 read → `read-only`；如果有 write →
   `modified`；如果有 write 且之前不存在 → `created`。

**限制**：
- 当前 `tool.completed` 只保存 size + hash，不保存完整内容。
- 文件路径未在 meta 中结构化存储，需要从 content 摘要的 `tool-name: path` 格式
  中解析。
- 建议未来为文件操作事件的 meta 添加 `:target_path` 字段。

**实现路径**：注册物化 Projection，scope 为 `session-id`。可服务于 `/diff` 命令
或 Subagent 完成后的变更摘要。

### 3.4 Health/Statistics Projection：跨 Session 的聚合指标

**目的**：从全量 Tape 中计算健康指标，辅助发现系统性问题。

**输入**：全部 Tape（或最近 N 天的 session）。

**输出结构**：

```json
{
  "window": {"from": "2026-08-09", "to": "2026-08-16"},
  "total_sessions": 45,
  "total_runs": 230,
  "total_tool_calls": 1200,
  "success_rate": {
    "runs": {"completed": 200, "failed": 18, "cancelled": 12},
    "tools": {"discharged": 1100, "denied": 50, "failed": 30, "blocked": 20}
  },
  "top_errors": [
    {"tool": "shell-command", "error_pattern": "EACCES", "count": 8},
    {"tool": "shell-command", "error_pattern": "command not found", "count": 5}
  ],
  "latency": {
    "avg_run_duration_s": 120,
    "p95_run_duration_s": 450,
    "avg_tool_duration_ms": 250
  },
  "model_usage": {
    "claude-sonnet-4": {"runs": 150, "avg_tokens": 5000},
    "gpt-4o": {"runs": 80, "avg_tokens": 3200}
  }
}
```

**算法**：

1. 用 `superchat-db-tape-select` 扫描 Tape，筛选 `kind=tool_result` 和 Runtime
   event（从 meta JSON 中解析）。
2. 用 SQL 按 session 和 kind 分组计数。
3. 错误分类：对 `kind=tool_result` 且 content 以 `tool.failed` 开头的行，用
   简单的 token 匹配（EACCES、not found、timeout、refused 等）聚类。
4. Latency：从事件的 `recorded_at` 差值计算。注：当前 `recorded_at` 是事件被
   Tape 接收的时间，不完全等于工具执行或 LLM 调用的 wall time。要获得精确
   延迟，需要在实际 invoke 点记录 `start_ns/end_ns` 并作为 payload 字段存入。

**限制**：

- 需要解析 JSON meta 列，SQL 无法直接做聚合查询；大的统计窗口建议先做物化表。
- 缺少显式的 error 分类标签和 latency 字段，当前依赖启发式解析。
- 未来建议新增的 meta 字段：`:latency_ms`、`:model_id`、`:error_category`。

**实现路径**：可做成周期性（如每次 compact 时）更新的物化统计表，或按需计算的
纯 Projection。建议先做按需版本，评估查询成本后再决定是否物化。

### 3.5 四种 Projection 的需要新增的 Tape 字段总结

| 字段 | 建议位置 | 当前状态 | 用途 | 优先级 |
|---|---|---|---|---|
| `:latency_ms` | meta | 未记录 | Health 统计、慢查询诊断 | 高 |
| `:model_id` | meta | 未记录（但可从 preset 推断） | 按模型分组的统计数据 | 中 |
| `:error_category` | meta | 未记录 | 错误自动分类和聚类 | 中 |
| `:target_path` | meta | 未记录（但在 summary 中常见） | Diff Projection 文件级聚合 | 中 |
| `:tool_name` (structured) | meta（已存在但需规范化） | 部分事件有，未统一 | 所有诊断 Projection 的工具分类 | 已部分满足 |
| `:provider_request_sha256` | meta | 未记录 | 完整 trajectory 审计 | 低（需评估隐私影响） |

所有这些字段都是**可选的增强**；缺少它们只会降低某种诊断的精度，不会破坏现有
Projection 和 Closure 的正确性。

## 4. 与其他 Agent 记忆/状态系统的对比

### 4.1 总结表

| 系统 | 存储模型 | 读模型 | 时间旅行 | 因果追踪 | 不可变性 | 成熟度 |
|---|---|---|---|---|---|---|
| **Superchat Tape** | SQLite append-only + 逻辑事件 ID | Closure（验证）+ Projection（折叠） | ✅ 完整回放 | ✅ causation-id chain | ✅ logical append-only | 内部 alpha |
| **Bub (tape.systems)** | JSONL 文件 + session 隔离 | TapeContext → message list | ⚠️ 可回放，但 reset 可删除 | ❌ 无 causation | ⚠️ 普通写入 append，但 reset 可破坏 | 早期开源原型 |
| **Zep / Graphiti** | Episodes → Graph (entities + facts) | Observations (派生聚类) | ❌ 无事件级回放 | ❌ 无因果链 | ⚠️ Observations 可原地 regen/retire | 闭源商业产品 |
| **LangGraph Checkpointer** | 序列化的 graph state（snapshot） | 读取最新 checkpoint | ⚠️ 有 checkpoint 历史，但不是全量事件 | ❌ 无因果 | ❌ 可覆盖 | 广泛使用 |
| **CrewAI / AutoGen Memory** | 大多为向量数据库 + chat message store | 语义检索 + 最近 N 条 | ❌ 无事件回放 | ❌ 无因果 | ❌ | 早期 |
| **Chronon / Feature Store** | 时序点查（point-in-time correct） | 训练/在线特征 | ⚠️ 有时序正确性，但非事件溯源 | ❌ | ❌ | ML infra |

### 4.2 关键差异分析

**Superchat vs LangGraph Checkpointer**：LangGraph 的 checkpointer 存的是执行图
的 checkpoint 快照，不是完整的事件流。它可以恢复到某个 checkpoint，但不能回放
checkpoint 之间的工具调用、审批决定和子代理委托。Superchat 的 Tape 记录了
全部中间步骤。这是“状态快照”与“事件流”之间的根本区别。

**Superchat vs Zep Observations**：Zep 的强项在知识图谱——从对话中提取实体和关系，
然后派生聚类。它的模型是“生成式 AI 的 memory”，模型回答“用户说了什么、他们
关心什么”。Superchat 的 Tape 回答“系统做了什么、为什么这么做”。两者可以互补：
Zep 提取用户上下文，Superchat 记录执行审计。

**Superchat vs Bub**：Bub 在概念设计上与 Superchat 最接近（都使用了 tape.systems
术语），但其实现尚未达到网站声明的不变量：reset 工具可以删除 Tape，Entry 的嵌套
payload 没有深层不可变保护，没有 Projection/Closure 层。Superchat 在逻辑层面上更
完整（有 causation、Closure 合同验证、物化 Projection），但缺少 Bub 网站画出的
DAG/Task Runtime 等高层模型。

## 5. 下一步建议

### 5.1 立即可做（不改变 Tape 合同）

1. **实现 Timeline Projection。** 这是最直接、最高价值的诊断工具。它只是对现有
   `superchat-tape-replay-events` 结果的重新组织。可以先做成 `/debug timeline <run-id>`
   命令。

2. **实现 Causation Trace。** 对任何一个失败事件，沿 `:causation-id` 回溯到根。
   算法简单（内存 BFS），可以先做成纯 Projection，验证后再物化。

3. **为 Runtime event 增加 `:latency_ms` 元数据。** 在事件产生点（Runtime 的
   provider 完成回调、Authority 执行回调）记录两个 timestamp 的差值，存入 meta。
   这是所有性能诊断的基础。

### 5.2 短期（需要 small schema change）

4. **规范化 tool event 的 `:tool_name` 和 `:target_path`。** 在
   `superchat-tape-append-event` 中接收可选的 structured 元数据，由调用方
   （Authority/Agent Loop）填入，不再依赖 content 摘要的解析。

5. **实现 Diff Projection。** 文件操作按 target path 聚合，展示 Run 的净效果。

6. **实现 Health 统计 Projection。** 按需或周期性更新，支持跨 Session 的趋势分析。

### 5.3 中期（需要设计评审）

7. **Controlled Trajectory Manifest。** 为需要完整审计的 Run 附加 content-addressed
   provider 请求/响应证据（用户同意 + redaction 后）。这将解除“不能从 Tape 还原完整
   provider 交互”的限制，但需要仔细的隐私设计。

8. **Causation Graph 的 SQL 物化。** 如果 causation 追踪成为高频操作，可考虑在
   `projection_checkpoint` 旁增加 `causation_index` 表，存储 (source_event_id,
   target_event_id) 边，支持 SQL 递归 CTE 遍历。

9. **跨 Session 的 Team/Topology Projection。** 对并行子代理、Team workflow 的
   跨 Session 追踪，需要统一的 `correlation-id` 索引和跨 Session 查询接口。

### 5.4 保持不做的

- **不从 Tape 删除或修改历史事件。** 保留 append-only 合同不动。
- **不再增加一个“Agent 状态机”。** Runtime + Tape + Projection 已经是足够的状态
  管理方案；不要引入另一个平行的状态存储。
- **不把 Closure 扩展到内容验证。** Closure 验证的是 Run 的 lifecycle 合同，不是
  模型输出的正确性。语义正确性验证是另一层（Evaluator），不应混入 Closure。

## 6. 参考资料

### 6.1 Event Sourcing / CQRS

- Young, G. (2010). *CQRS and Event Sourcing*. https://cqrs.files.wordpress.com/2010/11/cqrs_documents.pdf
- Fowler, M. (2005). *Event Sourcing*. https://martinfowler.com/eaaDev/EventSourcing.html
- Fowler, M. (2011). *CQRS*. https://martinfowler.com/bliki/CQRS.html
- Microsoft (2017). *CQRS Journey*. https://docs.microsoft.com/en-us/previous-versions/msp-n-p/jj554200(v=pandp.10)
- Vernon, V. (2013). *Implementing Domain-Driven Design*. Addison-Wesley. (Ch.8: Event Sourcing)
- Kleppmann, M. (2017). *Designing Data-Intensive Applications*. O'Reilly. (Ch.11: Stream Processing)

### 6.2 Agent Memory / State

- Superchat [架构文档](../architecture.md)（旧 Agent 架构快照已清理）
- Superchat Tape 深度研究：`docs/research/tape-systems-deep-study.md`
- Superchat 证据边界：`docs/research/superchat-evidence-boundaries-2026-08-09.md`
- Bub (tape.systems)：https://github.com/bubbuild/bub, https://github.com/bubbuild/tape.systems
- Zep / Graphiti：https://github.com/getzep/graphiti, https://help.getzep.com
- LangGraph：https://langchain-ai.github.io/langgraph/concepts/persistence/

### 6.3 时序与可观测性

- OpenTelemetry：distributed tracing 的工业标准，Span → Trace 模型可直接映射
  到 Tape 的 Event → Causation chain。
- Honeycomb (2016+). *Observability-Driven Development*. 提出的“高基数维度 +
  任意时间窗口”查询模式，与 Tape + 诊断性 Projection 的思路一致。
- Majors, C. et al. (2019). *Observability Engineering*. O'Reilly. 特别是关于
  结构化事件和宽事件（wide events）的讨论。

### 6.4 相关学术工作

- ExpSeek (2025). *Experience-Driven Web Agent*. 用 step entropy 触发 experience
  retrieval，对“从事件流中学习”有启示。arXiv.
- MMPO (2025). *Multi-step Memory Policy Optimization*. 把 belief entropy 用于
  memory trajectory 选择。arXiv.
- Semantic Uncertainty / Nature (2024). *Detecting confabulations in LLMs*.
  语义熵作为不确定性信号。Nature 593.

## 7. 已证实、推论、待验证

### 已证实（代码事实）

- Tape 存储 Runtime event 的逻辑 identity (`evt_<sha256>`)、causation chain、
  actor/profile、correlation 和 parent-run 关系。
- Projection 注册/重放/重建机制存在且经过 ERT 测试覆盖。
- Closure 能从 Tape 判断 Run 的 lifecycle 闭合状态和 pending effects。
- 当前 Tape 不保存完整 tool args/result 或 provider request body（只有 size/hash）。
- 当前事件缺少 latency 字段和结构化的工具元数据。

### 推论

- 四层诊断 Projection（Timeline / Causation / Diff / Health）中前三层的核心事件
  类型已存在于当前 Tape schema，可以在不改变 Tape 合同的前提下实现。
- 增加 `:latency_ms`、`:error_category`、`:target_path` 等可选元数据字段不会
  破坏 append-only 合同（它们是新增字段，不是修改现有事件）。
- 与其他 Agent 记忆系统相比，Superchat 的 ES 模型在时间旅行和因果追踪方面有
  结构性优势，但在高层语义分析（Zep 的 Observation）和易用性（LangGraph 的简单
  checkpoint）方面需要额外建设。

### 待验证

- Timeline/Causation Projection 在大 Session（数万条事件）下的内存和延迟表现。
- Health 统计 Projection 的查询 SQL 在真实数据量下是否需要物化表。
- 跨 Session 的子代理 causation 追踪的完整性和正确性。
- 新增 meta 字段后，现有 Projection rebuild 的向后兼容性。
