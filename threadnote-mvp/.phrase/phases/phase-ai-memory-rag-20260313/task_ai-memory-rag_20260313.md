# Task List — phase-ai-memory-rag-20260313

## M1 — SQLite 存储层 + JSON 迁移

task001 [ ] 场景: 引入 GRDB 依赖 | Given: Package.swift 无 GRDB | When: 添加 GRDB SPM 依赖 | Then: 项目编译通过 | 验证: xcodebuild 无 error

task002 [ ] 场景: 创建 WorkspaceManager | Given: App 无已知 Workspace | When: 用户选择目录 | Then: 创建 .threadnote 目录包（db.sqlite + attachments/），NSFileBookmark 写入 UserDefaults | 验证: 目录结构完整，重启后 bookmark 可恢复路径

task003 [ ] 场景: 创建 DatabaseMigrator v1 schema | Given: GRDB 已引入 | When: App 首次打开数据库 | Then: 创建 threads / entries / claims / anchors / tasks / discourse_relations / object_mentions / entry_references / source_metadata 表 | 验证: 可插入并查询每张表

task004 [ ] 场景: 创建 PersistenceStore | Given: DatabaseMigrator v1 完成 | When: App 启动 | Then: PersistenceStore 使用 DatabasePool（WAL 模式）成为唯一数据读写入口 | 验证: 并发读写无死锁，WAL 文件存在

task005 [ ] 场景: Store 层切换到 PersistenceStore | Given: Store 当前调用 PersistenceManager（JSON） | When: 替换实现 | Then: 所有读写走 SQLite，snapshot.json 停止生成，ValueObservation 驱动 UI 更新 | 验证: 增删改数据后重启 App，数据保留

task006 [ ] 场景: 一次性 JSON 迁移 | Given: 存在 snapshot.json | When: App 以新版本首次启动 | Then: 导入所有 threads / entries / claims 等数据，写入 app_metadata.migration_version，旧文件备份为 snapshot.json.bak | 验证: id / 数量 / 关系完整，旧文件有 .bak 副本

task007 [ ] 场景: AttachmentManager | Given: Workspace 已初始化 | When: 用户拖入文件到 CaptureEditor | Then: 文件复制到 attachments/<hash>.<ext>，entry 存储相对路径而非 file:// 绝对路径 | 验证: 移动 .threadnote 目录后，图片仍可渲染

---

## M2 — Retrieval 层：documents + FTS5 + 结构化排序

task008 [ ] 场景: 创建 retrieval_documents 表 | Given: M1 完成 | When: DatabaseMigrator 添加 v2 migration | Then: 表含 id / owner_type / owner_id / thread_id / title / body / metadata_json / created_at / updated_at | 验证: 可插入并查询

task009 [ ] 场景: 创建 retrieval_fts FTS5 虚拟表 | Given: retrieval_documents 表存在 | When: migration 执行 | Then: FTS5 虚拟表（unicode61 tokenizer）建立，触发器在 retrieval_documents 写入时自动同步索引 | 验证: 中文关键词可命中，`rank` 列有值

task010 [ ] 场景: retrieval_documents 同步写入 | Given: PersistenceStore 处理 entry / claim / anchor / resource 写入 | When: 任意源实体创建或更新 | Then: 对应 retrieval_documents 行同步更新（title / body 从源实体映射） | 验证: 写入后立即可被 FTS 召回

task011 [ ] 场景: 创建 RetrievalEngine | Given: retrieval_documents + FTS5 就绪 | When: 调用 RetrievalEngine.recall(query:threadFilter:) | Then: 执行 scope filter → FTS recall → structural boosts（thread match / settled status / entry kind / object overlap / recency）返回排序结果 | 验证: 有 claims 的 thread 排序优于无任何结构化数据的 thread

task012 [ ] 场景: Thread Suggestion 接入 RetrievalEngine | Given: RetrievalEngine 完成 | When: 用户输入文字触发 thread suggestion | Then: 候选 thread 排序由 RetrievalEngine 输出决定，替代当前全量 LLM 路由 | 验证: 有 stable claims 的 thread 在相关 query 下排名靠前，无退化

---

## M3 — Memory 层

task013 [ ] 场景: 创建 memory_records 表 | Given: M2 完成 | When: DatabaseMigrator v3 migration | Then: 表含 spec 定义的全部列（scope / kind / source_entity_type / source_entity_id / salience_score / freshness_score / superseded_by_memory_id / is_inspectable 等） | 验证: 可插入并按 scope / is_inspectable 过滤查询

task014 [ ] 场景: 创建 memory_work_queue 表 | Given: memory_records 表存在 | When: migration 执行 | Then: 表含 id / thread_id / job_kind / payload_json / status / created_at / updated_at，唯一约束 (thread_id, job_kind) | 验证: 重复 enqueue 同类 job 不产生重复行

task015 [ ] 场景: MemoryPipeline 同步写入 — working memory | Given: entry 创建或更新 | When: PersistenceStore 写入 entry | Then: MemoryPipeline 同步更新对应 working memory record（session scope） | 验证: thread open 时可取到最新 working memory

task016 [ ] 场景: MemoryPipeline 同步写入 — episodic memory | Given: anchor 写入 | When: anchor 创建 | Then: MemoryPipeline 同步写入 episodic memory record，source_entity_type = anchor | 验证: anchor 写入后立即可在 episodic memory 查到

task017 [ ] 场景: MemoryPipeline 同步写入 — stable memory | Given: claim 状态变更为 decided / solved / verified / dropped | When: updateClaim(status:) 调用 | Then: MemoryPipeline 同步写入或更新 stable memory record（semantic scope） | 验证: 状态变更后 stable memory 记录存在且内容准确

task018 [ ] 场景: MemoryPipeline 同步写入 — source memory | Given: source 类 entry 创建 | When: entry.kind == .source 写入 | Then: MemoryPipeline 同步写入 source memory record | 验证: source entry 写入后 source memory 可查

task019 [ ] 场景: 异步语义压缩 | Given: memory_work_queue 有 pending semantic_compress job | When: MemoryPipeline 后台消费队列 | Then: 跨 entry 的 thread 状态归纳写入新 memory record，旧 record 标记 superseded_by_memory_id | 验证: 无 LLM 配置时跳过，不阻塞 thread open

task020 [ ] 场景: Dirty thread fallback | Given: 异步 consolidation 未完成 | When: thread open 触发 Restart Note | Then: 使用上次已提交的 memory records + 原始表兜底，不等待异步任务 | 验证: 关闭 LLM provider 时 thread open 正常，不卡顿

task021 [ ] 场景: Thread Memory popover 重构 | Given: memory_records 数据就绪 | When: 用户打开 Thread Memory popover | Then: 显示三组（Session / Stable / Source），每条显示短文本 + kind 标签 + 时间戳 + 溯源链接（entry / claim / anchor / resource） | 验证: popover 不遮挡主 recovery 路径，溯源链接可导航到源实体

---

## M4 — AI 工作流接入检索层

task022 [ ] 场景: Restart Note 输入接入 RetrievalEngine | Given: M2 / M3 完成 | When: 用户打开 thread 触发 Restart Note | Then: 召回顺序为 semantic memory → episodic memory → source memory → recent raw entries，token 预算由 RetrievalEngine 控制 | 验证: Restart Note 输入 prompt 可解释，有 provenance

task023 [ ] 场景: Prepare View 接入检索层 | Given: RetrievalEngine 完成 | When: 用户触发 Prepare View | Then: 取当前 thread 内最强 claims + evidence + sources，无跨 thread 检索 | 验证: 输出与当前 thread 内容一致，不引入其他 thread 数据

task024 [ ] 场景: AIIntegration token 预算裁剪 | Given: Restart Note / Prepare View 接入检索层 | When: 构造 AI prompt | Then: 传入 token 数受 RetrievalEngine 输出限制，不再全量传入 entries | 验证: 单 thread 百条 entry 时 prompt token 数在预算内（< 4k tokens 目标）

---

## M5 — Embedding 可选钩子

task025 [ ] 场景: 创建 embeddings sidecar 表 | Given: M2 完成 | When: 用户启用 embedding 功能 | Then: 建立 embeddings 表（retrieval_doc_id / model / vector BLOB / created_at），外键关联 retrieval_documents | 验证: 表存在，可插入 Float32 BLOB

task026 [ ] 场景: EmbeddingProvider 协议 | Given: embeddings 表存在 | When: 实现 EmbeddingProvider | Then: 协议与 AIIntegration 解耦，支持 local / API provider 替换 | 验证: mock provider 可生成并存储 embedding，不影响 M1–M4 功能

task027 [ ] 场景: memory_work_queue embed job 消费 | Given: EmbeddingProvider 完成 | When: retrieval_document 写入后 enqueue embed job | Then: 后台消费 job，生成 embedding 写入 embeddings 表 | 验证: job 消费完成后 embeddings 表有对应行

task028 [ ] 场景: RetrievalEngine 向量重排序 | Given: embeddings 已生成 | When: EmbeddingProvider 配置且 query embedding 可用 | Then: RetrievalEngine 在结构化排序后执行向量重排序（可选路径） | 验证: 关闭 embedding 时主路径无变化；开启时可在 Settings 中切换

---

## 跨 Phase 协调

task029 [ ] 场景: 标记 phase-persistence-sqlite-20260313 为 superseded | Given: M1 完成 | When: 更新 PHASES.md | Then: 将 phase-persistence-sqlite-20260313 移入 Superseded 区域，注明"superseded by phase-ai-memory-rag-20260313" | 验证: PHASES.md 更新，原 phase task 文件中冲突任务标注 superseded

task030 [ ] 场景: 更新 phase-resume-recovery 数据源 | Given: M2 / M3 完成 | When: 更新 change 文档 | Then: resume-recovery 的数据源改为检索层 + 原始表兜底，保留"3-line restart"交互契约 | 验证: change 文档注明 superseded 任务，实现迁移后交互不变

task031 [ ] 场景: 更新 phase-thread-ledger 实现方式 | Given: M3 完成 | When: 更新 change 文档 | Then: "Settled So Far"改为 stable memory 视图，保留产品语言 | 验证: change 文档注明 superseded 任务
