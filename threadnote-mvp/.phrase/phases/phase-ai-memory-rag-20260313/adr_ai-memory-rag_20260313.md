# ADR — Storage & Retrieval Boundary
## phase-ai-memory-rag-20260313

---

## 背景

Threadnote 当前使用 `snapshot.json` 全量读写，无搜索能力，无法支撑 AI 的上下文裁剪需求。随着产品引入 Restart Note、Thread Suggestion、Prepare View 等 AI 功能，需要明确：

1. **存储边界**：什么数据存在哪里？谁是单一事实来源？
2. **检索边界**：AI 工作流的输入从哪里来？"拿多少"由谁决定？
3. **Memory 边界**：系统衍生的 memory records 与用户原创内容如何隔离？

---

## 决策

### A1 — SQLite (GRDB) 是唯一数据存储

**结论**：从 M1 起，`PersistenceStore` 使用 GRDB `DatabasePool`（WAL 模式），`snapshot.json` 停止写入。

**理由**：
- JSON 全量覆盖写，多线程不安全；GRDB WAL 模式支持并发读 + 单写
- JSON 无查询能力，AI 上下文裁剪需要 SQL 层过滤
- GRDB `ValueObservation` + Combine 可替代手动通知，减少 Store 层代码
- `DatabaseMigrator` 保证每次迁移恰好执行一次，schema 版本可追溯

**备选方案**：
- **CoreData**：XML/Binary 存储，无原生 FTS5，并发模型复杂，迁移成本高
- **SwiftData**：Swift 6 兼容但 API 尚不稳定（2026 年仍有 breaking changes 风险），FTS 支持弱
- **Raw SQLite3 C API**：直接控制，但 Swift 类型映射、迁移管理均需手写，维护成本高

**后果**：
- 需要一次性 JSON → SQLite 迁移（task006），并保留 .bak 文件用于回滚
- 所有新功能的数据读写必须通过 `PersistenceStore`，禁止直接操作 JSON

---

### A2 — Retrieval 是 App 基础设施，不是 AI 工作流

**结论**：`RetrievalEngine` 独立于 `AIIntegration`；AI 工作流消费 `RetrievalEngine` 的输出，不自己做检索。

**理由**：
- 检索需要在无 LLM 配置时仍可运行（产品目标：无 cloud provider 可正常工作）
- FTS5 + 结构化排序是确定性的，embedding 是可选增强，两者应分层
- 把检索逻辑放在 AI 工作流内会导致 AI workflow 测试困难、检索策略难以复用

**边界规则**：
- `RetrievalEngine`：只依赖 `PersistenceStore`，无 LLM 调用，返回排序后的 `RetrievalResult` 列表
- `EmbeddingProvider`：独立协议，与 `AIIntegration` 解耦；只在 M5 启用，不在 M1–M4 主路径上
- `AIIntegration`：从 `RetrievalEngine` 拿输入，组装 prompt，调用 LLM；不自己查询数据库

**后果**：
- Restart Note、Thread Suggestion、Prepare View 的"拿什么数据"逻辑在 `RetrievalEngine` 中，不在 AI 工作流中
- 关闭 LLM provider 时，`RetrievalEngine` 仍可返回结果供 UI 展示（降级模式）
- 同一 thread 在前端只加载第一页和已滚动加载多页时，AI 的输入边界必须一致；页面分页只影响显示，不影响检索结果

### A5 — 页面分页与 AI 上下文彻底解耦

**结论**：UI 分页只服务渲染性能；AI 上下文组装永远从数据库状态出发，不从“当前页面加载了多少 entries”推导。

**理由**：
- 分页是视图层优化，检索是语义层决策，把两者绑在一起会让 AI 输出随滚动状态漂移
- 大 thread 场景下，用户通常只看第一页，但 AI 仍必须有能力拿到旧但关键的 stable memory / anchor / source / evidence
- 数据库存储已经具备 retrieval_documents + memory_records，继续用页面加载量裁剪 AI 输入只是绕开现有基础设施

**边界规则**：
- `resume/prepare` 先生成 query，再由 `RetrievalEngine` 从 `retrieval_documents` / `memory_records` 召回材料
- UI 是否继续加载下一页 entries，不得改变检索 query、召回顺序或 token budget
- 原始 entries 只作为 fallback 层进入 prompt，且数量受预算约束

**后果**：
- 需要补大 thread 回归测试，验证分页前后 AI evidence pack 一致
- `openThread()` 可以继续服务 UI/detail，但不能再作为 AI 输入裁剪的事实来源

---

### A3 — Memory Records 不覆写用户内容

**结论**：`memory_records` 只存 AI 衍生内容，用户原创的 entry / claim / anchor 永不被 memory 系统修改或删除。

**理由**：
- 用户数据是不可侵犯的（PLAN.md 明确约束）
- memory records 可以 decay（降低 salience_score）或被 supersede（superseded_by_memory_id），但不会影响源实体
- 混入用户内容会使数据来源模糊，溯源链接失效

**边界规则**：
- `memory_records.source_entity_id` 指向源实体，单向引用，不反向修改
- 只有 `memory_records` 中 `is_inspectable = 1` 的行对用户可见（Thread Memory popover）
- AI 语义压缩（M3 task019）产生新 memory record，旧 record 标记 superseded，不删除

**后果**：
- 数据库体积会随时间增长（旧 memory records 不删除）；v1 不做清理策略，记录为 future work
- Restart Note 和 Prepare View 的输出可解释：每条输入可追溯到 source_entity_id

---

### A4 — Hybrid Light 检索：FTS5 + 结构化排序，Embedding 可选

**结论**：v1 ship 路径只依赖 FTS5 + 结构化 boosts；embedding 作为 M5 可选增强，不在 ship 标准内。

**理由**：
- Embedding 需要网络调用或本地模型（增加启动延迟 / 依赖）；v1 目标是离线可用
- FTS5 `unicode61` tokenizer 已支持中文 unicode 边界分词，满足 v1 需求
- 结构化排序（thread match / settled status / entry kind / object overlap / recency）已能区分候选质量
- embedding 可以在不改变 FTS 路径的前提下叠加，不需要重构检索层

**排序维度（固定顺序）**：
1. scope filter（thread 相关性）
2. FTS5 召回（关键词命中）
3. structural boosts：
   - settled status（decided / solved 优先）
   - entry kind（claim / anchor 优于 observation）
   - object overlap（@mentions 交叉）
   - recency（衰减权重）
4. （可选）embedding 向量重排序

**后果**：
- 无 embedding 时仍必须通过验收标准（task011 / task012）
- embedding 启用后 retrieval 排序可改善，但不得引入性能退化（thread open 须保持交互级响应）

---

## 约束汇总

| 约束 | 来源 |
|------|------|
| 无 cloud provider 时 thread open 正常工作 | Spec Goals |
| AI 不覆写用户笔记 | PLAN.md Assumptions |
| Memory 系统不作为主 workspace 对象 | PLAN.md Assumptions |
| Ship 路径不依赖 embedding | PLAN.md Assumptions |
| retrieval 是 app 基础设施，不是 AI 工作流 | 本 ADR A2 |

---

## 审阅清单

- [ ] M1 完成后验证 PersistenceStore 是唯一写入路径（无残留 JSON 写入）
- [ ] M2 完成后验证 RetrievalEngine 无 LLM 依赖（单测可在无 provider 配置下通过）
- [ ] M3 完成后验证 memory_records 不修改任何 entry / claim / anchor 行
- [ ] M4 完成后验证 AI prompt token 数在预算内（< 4k tokens，单 thread 百条 entry 场景）
- [ ] M4 完成后验证同一 1000 条 thread 在分页前后触发 AI prepare，召回证据包一致
- [ ] M5 完成后验证关闭 embedding 时主路径无变化

---

### A5 — Thread Aggregate 账本是派生视图，不是源数据

**结论**：新增 `thread_aggregates` 只作为高频读取账本，承载 `counts / status counters / fingerprint basis`；它不是事实源，永远可由 `entries / claims / anchors` 重建。

**理由**：
- 大 thread 下 `fetchThreadCounts()` 与 `fetchThreadFingerprintBasis()` 反复对原始表做计数，会把 AI 刷新判断和 header 装配拖回 O(N_thread)
- 这些聚合值本质是典型的 materialized view，更适合写时记账、读时直取
- 只把最常读、最稳定的聚合收到账本，可以明显降低读路径成本，同时避免把系统复杂度拉到完整 operation log

**边界规则**：
- `thread_aggregates` 只存最小聚合，不存原始条目内容
- 常规写路径允许“单 thread 重算 aggregate”，但不允许退回全 workspace 扫描
- `rebuildThreadAggregatesSync()` 必须存在，供 migration / repair 使用
- fingerprint 只依赖 thread metadata + aggregate + `thread_materialized_versions`，不再现场计数 raw tables

**后果**：
- `prepare/resume` 的读路径显著变轻，`fetchThreadCounts()` / `fetchThreadFingerprintBasis()` 从热点列表消失
- 同步写路径会多一次单-thread aggregate rebuild；这是用可控的 thread 级扫描换掉多次读时扫描，属于有意识的 trade-off
- 如果后续写时成本重新成为瓶颈，再继续把 aggregate 从“单 thread 重算”推进到更细的 patch 更新

### A6 — Aggregate patch 优先，rebuild 兜底

**结论**：常见写入优先用 patch 更新 `thread_aggregates`；只有碰到“删掉当前最大时间戳”“stable 退回非 stable”这类局部信息不足的场景，才退回单-thread rebuild。

**理由**：
- task038 已证明账本能明显降低读路径成本，但把所有写入都做成单-thread rebuild，会把写时成本重新拉高
- 普通 note 新增、status 单点变化、stable claim 新增、anchor 新增，本质都只影响少数计数或 `MAX(timestamp)`，没必要整条 thread 重扫
- 但 patch 不是宗教；当局部信息不足时，强行 patch 只会引入错误账本

**边界规则**：
- `entry`:
  - add/delete/move/status change 走 patch
- `claim`:
  - `non-stable -> stable` 走 patch
  - `stable -> non-stable` 回退 rebuild
  - `stable -> stable` 若时间戳倒退，回退 rebuild
- `anchor`:
  - add 走 patch
  - delete 或时间戳倒退，回退 rebuild
- `syncThreadRetrieval()` 这类 bootstrap/repair 路径必须顺手重建 aggregate

**后果**：
- 常见写入恢复到 O(1) 级记账
- 正确性仍由 rebuild 兜底，不把系统推进到脆弱的全量 diff 逻辑

### A7 — Thread detail 默认返回 memory preview，不返回整条 memory

**结论**：`openThread()` 默认返回最近一段 memory preview + `memoryCount`，不再在首开时把整条 thread 的所有 `memory_records` 一次性读出来。

**理由**：
- `memory_records` 是典型的“体量会随 thread 历史自然增长”的派生数据
- Thread 首开时，用户真正需要的是:
  - 当前 thread 是否有 memory
  - 大概有多少
  - 最近一批是什么
- 把几万条 memory records 全量捞出来，只是为了让面板显示“有多少条”，这是明显的数据结构错误

**边界规则**：
- `openThread()`:
  - 返回 `memory` preview
  - 额外返回 `memoryCount`
- Header / Inspector:
  - 计数优先读 `memoryCount`
  - 明细先显示 preview
- 如果后续需要“查看全部 memory”，应走显式分页或单独读取接口，而不是继续复用 thread 首开

**后果**：
- thread 首开固定成本明显下降
- UI 行为仍成立，因为用户仍能看到总数和最近一批内容
