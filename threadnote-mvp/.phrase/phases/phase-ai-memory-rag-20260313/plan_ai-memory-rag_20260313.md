# Plan — phase-ai-memory-rag-20260313

## Milestones

### M1 — SQLite 存储层 + JSON 迁移

取代 `phase-persistence-sqlite-20260313`，建立唯一数据源。

**交付物**：
- `PersistenceStore`（GRDB DatabasePool，WAL 模式）
- `ThreadnoteRepository`：增量写入边界，禁止 UI 直接全量 `saveSnapshot`
- `DatabaseMigrator` v1 schema：threads / entries / claims / anchors / tasks / discourse_relations / object_mentions / entry_references / source_metadata
- `WorkspaceManager`：创建 `.threadnote` 目录包，NSFileBookmark 持久化
- 一次性 JSON 迁移：从 `snapshot.json` 导入全部数据，写入迁移版本标记，停止写 JSON
- AttachmentManager：文件复制到 `attachments/` 子目录，存相对路径

**架构守卫**：
- render path 纯读：`body` / `onAppear` 不得触发数据库写入、AI 任务或 metadata 回填
- cache 按 thread 精准失效，不允许任何小改动触发全局 cache wipe
- 长列表默认惰性渲染，避免历史 entries 和富媒体常驻内存

**验收**：迁移后 id / 数量 / 关系完整；重启后数据保留；旧 JSON 备份为 `.bak`

---

### M2 — Retrieval 层：documents + FTS5 + 结构化排序

建立召回基础，无需 LLM 即可支持 Restart Note。

**交付物**：
- `retrieval_documents` 表 + `retrieval_fts` FTS5 虚拟表
- FTS5 同步触发器（entry / claim / anchor / resource 写入时自动更新索引）
- `RetrievalEngine`：scope filter → FTS recall → structural boosts（thread match / settled status / entry kind / object overlap / recency）
- Thread Suggestion 排序升级（候选 thread 排序改用 RetrievalEngine 输出）

**验收**：无云端 provider 时 thread open 可产出可用 Restart Note；thread suggestion 有 claims / anchors 时不退化

---

### M3 — Memory 层：记录 + 写入时机 + 异步队列

建立 memory 结构，支持 Restart Note 精准召回和 Thread Memory popover。

**交付物**：
- `memory_records` 表
- `memory_work_queue` 表（异步任务队列，唯一约束：`thread_id + job_kind`）
- `MemoryPipeline`：
  - 同步路径：entry 写入 → working memory；anchor 写入 → episodic memory；claim 状态变更（decided / solved / verified / dropped）→ stable memory；source entry 写入 → source memory
  - 异步路径：`memory_work_queue` 消费，语义压缩（可选 LLM）
- Dirty thread fallback：异步 consolidation 未完成时，用上次提交 memory + 原始表兜底
- Thread Memory popover 重构：Session / Stable / Source 三组，每条显示 kind 标签 + 溯源链接

**验收**：stable-role 变更立即更新 stable memory；anchor 写入立即影响 episodic memory；popover 不挤占主 recovery 路径

---

### M4 — Restart Note + Prepare View 接入检索层

将 AI 工作流的输入源从全量内存切换到 RetrievalEngine。

**交付物**：
- Restart Note 召回顺序实现：semantic → episodic → source → recent raw entries
- Prepare View：当前 thread 内最强 claims + evidence + sources（无跨 thread）
- `AIIntegration` 输入裁剪：token 预算由 RetrievalEngine 负责，不再全量传入
- Restart Note / Prepare View 改为 AI-only 输出契约：无 backend 显示未配置，LLM 失败显示错误，不再静默展示确定性结果

**验收**：Restart Note prompt 可解释，有 provenance；Prepare View 输出与当前 thread 内容一致；未配置/失败状态对用户显式可见

---

### M5 — Embedding 可选钩子

不阻塞 M1–M4 上线，作为增量增强。

**交付物**：
- `embeddings` sidecar 表（`retrieval_doc_id`, `model`, `vector BLOB`）
- `EmbeddingProvider` 协议（与 `AIIntegration` 解耦）
- `memory_work_queue` 的 `embed` job 消费实现
- RetrievalEngine 在 embeddings 配置时启用向量重排序（可选路径）

**验收**：关闭 embedding 时功能无退化；开启时 retrieval 排序可改善

---

## 优先级

| 优先级 | 里程碑 | 理由 |
|--------|--------|------|
| P0 | M1 | 数据层迁移阻塞一切后续工作 |
| P0 | M2 | Restart Note 基础召回，核心产品路径 |
| P1 | M3 | Memory 可见性，Thread Memory popover |
| P1 | M4 | AI 工作流接入检索层，减少 token 消耗 |
| P2 | M5 | 可选增强，不影响 ship |

---

## 范围

**In scope**：
- SQLite + GRDB 取代 JSON
- FTS5 检索 + 结构化排序
- memory_records 写入与管理
- Restart Note / Thread Suggestion / Prepare View 接入检索层
- Thread Memory popover（不新增 sidecar tab）
- Embedding 可选钩子

**Out of scope**：
- 独立 AI chat 界面
- 多 Workspace 并行
- iCloud Sync
- 完整本地 reranker / query expansion 栈
- Memory sidecar tab

---

## 风险与依赖

| 风险 | 缓解 |
|------|------|
| GRDB SPM 与项目编译冲突 | 先用 `xcodebuild` 验证编译，M1 第一步引入依赖 |
| JSON 迁移数据丢失 | 迁移前备份原文件；迁移后验证 id / 数量；支持回滚到 .bak |
| FTS5 中文分词效果差 | 使用 `unicode61` tokenizer（unicode 边界切词），验收标准明确要求可用 |
| 异步 consolidation 阻塞 thread open | Dirty thread fallback 机制（M3），thread open 不等待异步完成 |
| Embedding 引入延迟 | M5 设计为可关闭，RetrievalEngine 主路径不依赖 embedding |
| Store 层切换影响现有 UI | M1 完成后立即做回归测试，ValueObservation 替代手动通知需充分测试 |

---

## 与其他 Phase 的协调

| Phase | 处理 |
|-------|------|
| `phase-persistence-sqlite-20260313` | 本 phase 取代，标记为 superseded |
| `phase-resume-recovery-20260311` | 保留"3-line restart"交互契约，M2/M3 完成后接入检索层 |
| `phase-thread-ledger-20260311` | 保留"Settled So Far"产品语言，M3 完成后实现改为 stable memory 视图 |
| `phase-plastic-resume-20260311` | 保留 goal-shaped 输出契约，M4 完成后 resume inputs 接入检索层 |

冲突任务在对应 phase task 文件中标记 `superseded by phase-ai-memory-rag-20260313/taskXXX`。

---

## 回滚方案

- M1 迁移：`snapshot.json.bak` 保留，可手动恢复；schema 通过 `DatabaseMigrator` 版本追踪
- M2/M3：`retrieval_documents` / `memory_records` 是附加表，不修改源表；删除附加表可退回 M1 状态
- M4：AI 工作流输入改为可切换（feature flag 或代码开关），可退回全量传入
- M5：embedding 配置关闭即可禁用向量路径，无需迁移
