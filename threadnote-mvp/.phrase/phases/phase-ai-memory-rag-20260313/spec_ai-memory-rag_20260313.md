# Spec — phase-ai-memory-rag-20260313

## Summary

三件事合并为一个 phase：**SQLite 持久化升级**、**Memory 基础层**、**轻量混合检索（Hybrid Light RAG）**。

AI 服务优先级不变：Restart Note > Thread 路由 > Prepare View。v1 不做独立 AI chat 界面。

## Locked Decisions

| 决策 | 结论 |
|------|------|
| 存储层 | SQLite now（GRDB），不再写 JSON |
| AI 优先级 | Thread resume first |
| 检索方式 | Hybrid light（FTS5 + structural ranking，embedding 可选） |
| Memory 可见性 | Inspectable system records，有 provenance |
| 写入时机 | Hybrid sync+async（用户写同步，语义压缩异步） |
| Memory UI | Chrome popover，不加 sidecar tab |
| 渲染边界 | Render path 纯读，不在 `body` / `onAppear` 做数据库写入或 AI/metadata 副作用 |
| 与其他 phase 关系 | 取代 phase-persistence-sqlite，supersede 其他 active phase 的冲突任务 |

## Goals

- `PersistenceStore`（GRDB）成为唯一数据源，snapshot.json 停止写入
- `retrieval_documents` + FTS5 支撑全文召回，`memory_records` 支撑 AI 上下文裁剪
- Restart Note、Thread Suggestion、Prepare View 的输入源迁移到检索层
- 单 thread 达到 1000+ notes 时，AI 仍通过数据库检索 + 分层 memory + token budget 有效工作，而不是依赖当前页面已加载的 entries 数量
- Thread Memory popover 展示可溯源的系统 memory records
- 所有非 AI 基础能力在无 embedding、无云端 provider 时仍可正常工作；Restart Note / Prepare View 在无 backend 时必须显式提示未配置
- UI 写操作改为增量仓储写入，禁止全量 snapshot 重写阻塞主线程

## Non-Goals

- 独立 AI chat / "ask anything" 全局界面
- Memory sidecar tab（改为 popover）
- 完整本地 reranker / query-expansion 栈
- 多 Workspace 并行打开
- iCloud Sync

## Data Model

### 保留（关系型，first-class）

- `threads` — 已有
- `entries` — 已有
- `claims` — 已有
- `anchors` — 已有
- `tasks` — 已有
- `discourse_relations` — 已有

### 新增

#### `memory_records`

| 列 | 类型 | 说明 |
|----|------|------|
| id | TEXT PK | UUID |
| thread_id | TEXT | 可为 null（跨线程 memory） |
| scope | TEXT | `working` / `episodic` / `semantic` / `source` |
| kind | TEXT | 具体类型，如 `anchor_summary` / `stable_claim` / `source_fact` |
| source_entity_type | TEXT | `entry` / `claim` / `anchor` / `resource` |
| source_entity_id | TEXT | 对应源实体 ID |
| text | TEXT | memory 文本内容 |
| salience_score | REAL | 显著性评分 |
| freshness_score | REAL | 新鲜度评分 |
| created_at | TEXT | |
| updated_at | TEXT | |
| superseded_by_memory_id | TEXT | 被哪条 memory 取代 |
| is_inspectable | INTEGER | 1 = 用户可见 |

#### `retrieval_documents`

归一化召回单元，覆盖 entry / claim / anchor / memory / resource：

| 列 | 类型 | 说明 |
|----|------|------|
| id | TEXT PK | |
| owner_type | TEXT | 源实体类型 |
| owner_id | TEXT | 源实体 ID |
| thread_id | TEXT | |
| title | TEXT | |
| body | TEXT | |
| metadata_json | TEXT | 结构化补充信息 |
| created_at | TEXT | |
| updated_at | TEXT | |

#### `retrieval_fts`（FTS5 虚拟表）

```sql
CREATE VIRTUAL TABLE retrieval_fts USING fts5(
    title, body,
    content='retrieval_documents',
    content_rowid='rowid',
    tokenize='unicode61'
);
```

#### `embeddings`（可选 sidecar）

| 列 | 类型 | 说明 |
|----|------|------|
| retrieval_doc_id | TEXT PK FK | |
| model | TEXT | embedding 模型标识 |
| vector | BLOB | Float32 数组序列化 |
| created_at | TEXT | |

#### `memory_work_queue`（异步任务队列）

| 列 | 类型 | 说明 |
|----|------|------|
| id | TEXT PK | |
| thread_id | TEXT | |
| job_kind | TEXT | `semantic_compress` / `embed` / `rerank` |
| payload_json | TEXT | |
| status | TEXT | `pending` / `running` / `done` / `failed` |
| created_at | TEXT | |
| updated_at | TEXT | |

**唯一约束**：`(thread_id, job_kind)` 确保同类任务不重复入队。

## Write Timing

### 同步（每次用户写操作）

1. 写入源表（entry / claim / anchor 等）
2. 更新对应 `retrieval_documents`
3. 更新确定性 memory（无需 LLM）：
   - session memory：来自最近线程活动
   - episodic memory：来自 anchor 写入
   - stable memory：来自 `decided/solved/verified/dropped` 状态变更

### 异步（通过 `memory_work_queue`）

- 语义压缩（跨 entry 的 thread 状态归纳）
- 可选 embedding 生成
- 若 embedding 启用，可选重排序

**触发条件**：entry 创建/更新/rethread，claim 创建/supersede，anchor 写入，archive/restore。

## Retrieval 行为

### Restart Note 召回顺序（固定）

1. 同 thread 的 stable memory（semantic scope）
2. 最新 anchor / episodic memory
3. source memory
4. 原始 entries + claims 兜底

### 大线程约束（新增锁定）

- AI 输入构造不得以“前端当前已加载多少 entries”为依据
- thread open / resume / prepare 的上下文必须由 `RetrievalEngine` 基于数据库状态独立决定
- 大线程默认走 `query-specific retrieval + memory fallback + token budget`，只在检索不足时回退到少量 recent raw entries
- 任何用户可见 AI 结果都应可解释为“为什么拿了这些材料”，至少能指出来自 semantic / episodic / source / raw fallback 哪一层

**脏线程处理**：若异步 consolidation 未完成，使用上次提交的 memory + 原始表兜底，不阻塞 thread open。

### Thread Suggestion 排序维度

- title/goal FTS 命中
- stable claim 命中
- anchor / state summary 命中
- object overlap
- recency boost

### Prepare View

仅限当前 thread，取最强 claims + evidence + sources；v1 不做跨线程检索。

## Product Integration

### ThreadDocument.swift

保持 recovery-first：Restart Note → Continue → Working Stream，不改动主画布结构。

Restart Note 与 Prepare View 属于 AI-only surface：
- 无 backend：显示 `notConfigured` 提示，不展示确定性用户可见结果
- backend 调用失败：显示 `error` 提示，不静默降级
- 确定性 synthesize/request 只保留为 LLM 输入构造，不直接作为用户可见输出

### Thread Memory Popover

**不**在 ThreadInspectorView 加 Memory tab，保持 chrome-triggered popover。

popover 内容重构为可溯源的系统 memory records，默认布局：
- `Session Memory`（展开）
- `Stable Memory`（折叠）
- `Source Memory`（折叠）

每条 memory item 显示：短文本 + memory kind 标签 + 时间戳 + 溯源链接（entry/claim/anchor/resource）。

### Settled So Far

保留用户可见标签，数据源改为 stable memory + source provenance，不另建独立对象模型。

## 与其他 Active Phase 的边界

| Phase | 处理方式 |
|-------|---------|
| `phase-persistence-sqlite-20260313` | 本 phase 取代，标记为 superseded |
| `phase-resume-recovery-20260311` | 保留"3-line restart"交互契约，数据源改为检索层 + 原始表兜底 |
| `phase-thread-ledger-20260311` | 保留"Settled So Far"产品语言，实现改为 stable memory 视图 |
| `phase-plastic-resume-20260311` | 保留 goal-shaped 输出契约，resume inputs 的实现所有权移交本 phase |

冲突任务标记为 `superseded by phase-ai-memory-rag-20260313/taskXXX`，不静默保留。

## Acceptance Criteria

- [ ] 迁移保留所有 id、数量和关系
- [ ] 无云端 provider、无 embedding 时 retrieval/memory/routing support path 正常，Restart Note / Prepare View 显示未配置提示而不是确定性回退
- [ ] stable-role 变更（decided/solved 等）立即更新 stable memory
- [ ] anchor 写入立即影响 episodic memory
- [ ] thread suggestion 在有 claims/anchors/object mentions 时改善，不退化
- [ ] dirty-thread 在异步 consolidation 未完成时 fallback 正常
- [ ] backend 已配置但 LLM 调用失败时，Restart Note / Prepare View 显示显式错误
- [ ] Thread Memory popover 次要感，不挤占主 recovery 路径
- [ ] FTS5 全文搜索在中文 unicode61 分词下可用
- [ ] thread/resources 长列表为惰性渲染，滚动不触发持久化副作用
- [ ] 单 thread 达到 1000+ entries 时，Restart Note / Prepare View 的输入仍由数据库检索决定，prompt 裁剪不依赖 UI 分页状态
- [ ] `thread_aggregates` 能承载 counts/status/fingerprint basis 的高频读取，且损坏时可从 `entries/claims/anchors` 完整重建
