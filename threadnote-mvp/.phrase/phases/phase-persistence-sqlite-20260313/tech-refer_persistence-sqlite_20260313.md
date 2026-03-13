# Tech Reference — phase-persistence-sqlite-20260313
# GRDB.swift 能力分析 × Threadnote 产品设计

---

## 1. 为什么选 GRDB.swift

| 维度 | snapshot.json | GRDB.swift |
|------|--------------|------------|
| 并发安全 | 全量覆盖写，多线程不安全 | WAL 模式，并发读 + 单写，内置锁管理 |
| 查询能力 | 全量加载进内存再过滤 | SQL 层过滤，只传输需要的行 |
| 全文搜索 | 无 | FTS5 内建，支持相关性排序 |
| 响应式更新 | 无，需手动通知 | ValueObservation + Combine，自动推送变更 |
| 迁移管理 | 手动版本号 | DatabaseMigrator，每次迁移恰好执行一次 |
| 向量/Embedding | 无 | 可存 REAL 列，配合自定义 SQL 函数 |
| 附件管理 | 无 | 配合 Workspace 目录，存相对路径 |

---

## 2. Schema 设计

### 核心表

```sql
-- threads
CREATE TABLE threads (
    id          TEXT PRIMARY KEY,          -- UUID
    title       TEXT NOT NULL,
    color       TEXT NOT NULL DEFAULT 'sky',
    status      TEXT NOT NULL DEFAULT 'active',
    goal_type   TEXT,
    goal_stage  TEXT,
    goal_prompt TEXT,
    created_at  TEXT NOT NULL,
    last_active_at TEXT NOT NULL
);

-- entries
CREATE TABLE entries (
    id                 TEXT PRIMARY KEY,
    thread_id          TEXT REFERENCES threads(id) ON DELETE SET NULL,
    parent_entry_id    TEXT REFERENCES entries(id) ON DELETE SET NULL,
    supersedes_entry_id TEXT,
    kind               TEXT NOT NULL,
    body_kind          TEXT NOT NULL DEFAULT 'text',
    body_text          TEXT,
    body_url           TEXT,
    body_title         TEXT,
    body_details       TEXT,
    summary_text       TEXT NOT NULL DEFAULT '',
    author_type        TEXT NOT NULL DEFAULT 'user',
    inbox_state        TEXT NOT NULL DEFAULT 'unresolved',
    importance_score   REAL,
    confidence_score   REAL,
    session_id         TEXT,
    created_at         TEXT NOT NULL
);

-- object_mentions (entry의 @mentions 展开为独立行，便于查询)
CREATE TABLE object_mentions (
    id         TEXT PRIMARY KEY,
    entry_id   TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,
    label      TEXT NOT NULL,
    kind       TEXT NOT NULL
);

-- entry_references ([[references]])
CREATE TABLE entry_references (
    id            TEXT PRIMARY KEY,
    entry_id      TEXT NOT NULL REFERENCES entries(id) ON DELETE CASCADE,
    label         TEXT NOT NULL,
    target_kind   TEXT NOT NULL,
    target_id     TEXT
);

-- source_metadata (source 类 entry 的扩展字段)
CREATE TABLE source_metadata (
    entry_id   TEXT PRIMARY KEY REFERENCES entries(id) ON DELETE CASCADE,
    title      TEXT,
    locator    TEXT,
    citation   TEXT,
    author     TEXT,
    published_at TEXT
);

-- fts_entries: FTS5 虚拟表，索引 summary_text + body_text
CREATE VIRTUAL TABLE fts_entries USING fts5(
    summary_text,
    body_text,
    content='entries',
    content_rowid='rowid',
    tokenize='unicode61'
);
```

### FTS5 同步触发器

```sql
-- entries 写入时自动同步 FTS 索引
CREATE TRIGGER entries_ai AFTER INSERT ON entries BEGIN
    INSERT INTO fts_entries(rowid, summary_text, body_text)
    VALUES (new.rowid, new.summary_text, new.body_text);
END;

CREATE TRIGGER entries_au AFTER UPDATE ON entries BEGIN
    INSERT INTO fts_entries(fts_entries, rowid, summary_text, body_text)
    VALUES ('delete', old.rowid, old.summary_text, old.body_text);
    INSERT INTO fts_entries(rowid, summary_text, body_text)
    VALUES (new.rowid, new.summary_text, new.body_text);
END;

CREATE TRIGGER entries_ad AFTER DELETE ON entries BEGIN
    INSERT INTO fts_entries(fts_entries, rowid, summary_text, body_text)
    VALUES ('delete', old.rowid, old.summary_text, old.body_text);
END;
```

---

## 3. 数据库事务能力 → 产品功能映射

### 3.1 Savepoint — 原子多步操作

**产品场景**：用户把一条 inbox entry route 到一个 thread，同时创建一个新的 Claim。
这是两步写操作，要么全成功，要么全回滚。

```swift
try db.inTransaction(.immediate) {
    try entry.updateChanges(db) { $0.threadID = thread.id; $0.inboxState = "resolved" }
    try db.inSavepoint {
        var claim = Claim(entryID: entry.id, ...)
        try claim.insert(db)
    }
    return .commit
}
```

**其他适用场景**：
- Thread 归档：同时更新 `thread.status`、所有 entry 的 `inbox_state`、写入 Anchor 记录
- Resume 合成结果写入：AI 生成多个 ResumeComponent，要么全写入要么全丢弃
- Entry supersede：新 entry 写入 + 旧 entry 标记 `supersedesEntryID`，保持引用完整性

---

### 3.2 ValueObservation — Store 层响应式

当前 Store 是 `@Observable` 的内存状态，所有视图靠 Swift 的 observation 机制刷新。
迁移到 GRDB 后，可以用 `ValueObservation` 替代手动通知：

```swift
// 订阅某个 thread 下的所有 entries（自动感知新增/修改/删除）
let observation = ValueObservation.tracking { db in
    try Entry.filter(Column("thread_id") == threadID)
        .order(Column("created_at").desc)
        .fetchAll(db)
}
.publisher(in: dbPool)
.receive(on: DispatchQueue.main)
.sink { entries in store.threadEntries[threadID] = entries }
```

**产品价值**：
- 多窗口（未来）：两个窗口操作同一个 thread，数据自动同步
- AI 写入后立即刷新 UI：AI 后台写入 Resume 或 Claim，无需手动触发刷新
- 实时 badge 更新：sidebar 上 inbox count 自动跟随数据库变化

---

### 3.3 FTS5 全文搜索 — 全局搜索功能

当前没有搜索功能。FTS5 可以直接支持：

```swift
// 搜索所有 entries，按相关性排序
func searchEntries(_ query: String) throws -> [Entry] {
    let pattern = FTS5Pattern(matchingAllTokensIn: query)
    return try dbPool.read { db in
        try Entry
            .joining(required: Entry.ftsEntries.matching(pattern))
            .order(sql: "rank")
            .fetchAll(db)
    }
}
```

**支持的搜索模式**：
- `"设计模式"` — 精确短语
- `设计 模式` — 同时包含两个词（AND）
- `设计*` — 前缀匹配（输入时实时搜索）
- 跨 thread、跨 entry kind 全量搜索

**与 AI 联动（见第 4 节）**：FTS5 可作为 AI 的 retrieval 层，先用关键词粗筛，再用 LLM 精排。

---

### 3.4 DatabaseMigrator — 数据模型演进

随着产品迭代，Models.swift 里的 struct 会变化。`DatabaseMigrator` 保证每次 migration 恰好执行一次：

```swift
var migrator = DatabaseMigrator()

migrator.registerMigration("v1_initial_schema") { db in
    // 创建 threads、entries、fts_entries 等基础表
}

migrator.registerMigration("v2_add_importance_score") { db in
    try db.alter(table: "entries") { t in
        t.add(column: "importance_score", .double)
        t.add(column: "confidence_score", .double)
    }
}

migrator.registerMigration("v3_object_mentions_table") { db in
    // 从 entries.body JSON 拆出 object_mentions 独立表
}
```

---

### 3.5 WAL 并发模式 — AI 后台写入不阻塞 UI

用 `DatabasePool`（WAL 模式）：
- UI 读（`dbPool.read`）和 AI 写（`dbPool.write`）并发进行
- AI 生成 Resume 组件、写入 importanceScore 等耗时操作不会卡主线程
- `ValueObservation` 在 AI 写入完成后自动推送 UI 更新

---

## 4. AI × 数据库 配合架构

### 4.1 当前 AI 功能 → 需要数据库支持的地方

| AI 功能 | 当前做法 | 数据库加持后 |
|---------|---------|-------------|
| Thread 路由建议 (`suggestThreads`) | 全量 thread 列表传给 LLM | FTS5 先按关键词筛选候选 threads，减少 token 消耗 |
| Resume 合成 (`resumeSynthesis`) | 全量 entries 传给 LLM | 按 `importance_score` 降序取 top-N entries，事务写入结果 |
| Entry importance 打分 | 无 | AI 后台批量打分，写入 `importance_score` 列，UI 自动刷新 |
| 全局语义搜索 | 无 | FTS5 粗筛 → LLM 精排 → 返回带 rationale 的结果 |

---

### 4.2 Embedding 存储方案

GRDB 没有内置向量支持，但可以用 REAL 列存向量（768 或 1536 维）：

```sql
CREATE TABLE entry_embeddings (
    entry_id    TEXT PRIMARY KEY REFERENCES entries(id) ON DELETE CASCADE,
    model       TEXT NOT NULL,      -- e.g. "text-embedding-3-small"
    embedding   BLOB NOT NULL,      -- Float32 array 序列化为 BLOB
    created_at  TEXT NOT NULL
);
```

**查询方式**：全量读出后在 Swift 层做余弦相似度计算（entries 数量 < 10k 时完全够用）。未来若需要更大规模，可以接入 `sqlite-vec` 扩展或独立向量库。

**产品场景**：
- "找和这条 note 语义相似的其他 note" — 先查 embedding，按余弦排序
- AI 路由建议的语义增强版：embedding 相似度 + FTS5 关键词 双路召回

---

### 4.3 AI 写入的事务安全

AI 的结果是异步生成的，写入时需要事务保护：

```swift
// Resume 合成：AI 生成完毕后，在一个事务里整体写入
func persistResumeResult(_ result: ResumeSynthesisResult, for threadID: UUID) async throws {
    try await dbPool.write { db in
        // 删除旧的 resume components（幂等）
        try ResumeComponent
            .filter(Column("thread_id") == threadID.uuidString)
            .deleteAll(db)

        // 写入新的
        for component in result.components {
            try component.insert(db)
        }

        // 更新 thread 的 last_active_at
        try Thread.updateOne(db, key: threadID) { $0.lastActiveAt = Date() }
    }
    // ValueObservation 自动触发 UI 刷新
}
```

---

### 4.4 数据库作为 AI 的上下文窗口管理器

当前 AI 调用直接把内存中的 entries 全量传入 prompt。数据量增大后这不可持续。
数据库可以承担"上下文裁剪"的角色：

```
用户触发 Resume 合成
     │
     ▼
数据库查询层（"什么值得传给 AI？"）
  - 按 importance_score 降序 top-20 entries
  - 按 created_at 取最近 7 天的 entries
  - FTS5 搜索与 thread goal 相关的 entries
  - 取所有 Claim 和 Anchor（结构化信息）
     │
     ▼
构造精简 prompt（< 4k tokens）
     │
     ▼
LLM 生成结果
     │
     ▼
事务写入数据库
     │
     ▼
ValueObservation 推送 UI
```

---

## 5. 技术选型结论

| 决策 | 选择 | 理由 |
|------|------|------|
| SQLite 库 | **GRDB.swift 7.x** | Swift 6 兼容，Codable 支持，FTS5，ValueObservation + Combine |
| 连接模式 | **DatabasePool** | WAL 并发读写，AI 后台写入不阻塞 UI |
| 向量存储 | **BLOB 列 + Swift 层余弦计算** | 当前数据量不需要专用向量库，保持简单 |
| FTS | **FTS5 + unicode61 tokenizer** | 支持中文分词（unicode61 按 unicode 边界切词），相关性排序 |
| 迁移 | **DatabaseMigrator** | 版本追踪，幂等，上线安全 |
| Workspace | **.threadnote 目录包** | 用户可见、可备份、可移动，符合 macOS 文档范式 |

---

## 6. 实现优先级建议

```
M1 WorkspaceManager + 目录结构
   └── M2 DatabaseManager (GRDB) + schema + migrator
       ├── M3 Store 层切换（ValueObservation 替代手动通知）
       ├── M4 FTS5 全局搜索 UI
       ├── M5 AttachmentManager（文件拖入 + 相对路径）
       └── M6 Embedding 列 + AI 上下文裁剪
```

M1-M3 是基础层，完成后产品功能不变但数据更稳。
M4-M6 是增量功能，可以按产品节奏逐步交付。
