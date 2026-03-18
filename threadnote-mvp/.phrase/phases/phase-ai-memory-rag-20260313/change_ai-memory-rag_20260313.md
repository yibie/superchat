change050 日期:2026-03-18 | 文件:docs/large-thread-hardening-v2.md,docs/benchmarks/large-thread-latest.json | 操作:Modify | 影响:thread 首开 memory preview 记录 | 说明:追加 openThread memory preview 这一轮的原理、影响与 benchmark，保留历史记录并刷新最新 machine artifact | 关联:task040
change049 日期:2026-03-18 | 文件:tests/clean-room/applicationService.test.js | 操作:Modify | 影响:thread detail memory preview 回归 | 说明:新增 openThread 返回 memory preview + full memory count 的大 thread 回归，锁定首开不再默认全量拉取 memory | 关联:task040
change048 日期:2026-03-18 | 文件:renderer/src/components/thread/ThreadInspector.jsx,src/application/presenters/threadDocumentPresenter.js | 操作:Modify | 影响:Thread Memory 展示与 header 计数 | 说明:Thread Memory 面板改为显示总 memory 数与预览提示，header 优先使用 `memoryCount` 而非预览数组长度 | 关联:task040
change047 日期:2026-03-18 | 文件:src/application/services/threadnoteApplicationService.js,src/infrastructure/persistence/repositories/threadnoteRepository.js,src/infrastructure/persistence/stores/sqlitePersistenceStore.js,.phrase/phases/phase-ai-memory-rag-20260313/task_ai-memory-rag_20260313.md | 操作:Modify | 影响:openThread 首开读取路径 + phase task | 说明:将 openThread 的 memory 改为 preview + count，并新增 memory preview/count store-repository 接口与 task040 | 关联:task040

change046 日期:2026-03-18 | 文件:docs/large-thread-hardening-v2.md,docs/benchmarks/large-thread-latest.json | 操作:Modify | 影响:大 thread patch 账本记录 | 说明:追加 aggregate patch 更新这一轮的原理、性质与最新 benchmark，保留历史记录并刷新最新 machine artifact | 关联:task039
change045 日期:2026-03-18 | 文件:tests/clean-room/persistence.test.js | 操作:Modify | 影响:aggregate patch 回归覆盖 | 说明:新增 syncThreadRetrieval 补齐 aggregate 与常见写入不触发 aggregate rebuild 的回归验证 | 关联:task039
change044 日期:2026-03-18 | 文件:src/infrastructure/persistence/repositories/threadnoteRepository.js | 操作:Modify | 影响:write-path aggregate maintenance | 说明:将 entry/status/common claim/common anchor 的 aggregate 维护从单-thread 重算改为 patch 更新，只有边界 case 才回退 rebuild | 关联:task039
change043 日期:2026-03-18 | 文件:src/infrastructure/persistence/stores/sqlitePersistenceStore.js,.phrase/phases/phase-ai-memory-rag-20260313/task_ai-memory-rag_20260313.md | 操作:Modify | 影响:thread aggregate patch API + phase task | 说明:新增 `patchThreadAggregate()` 并补 task039，统一 aggregate patch 的 timestamp/max 与计数更新语义 | 关联:task039

change042 日期:2026-03-18 | 文件:docs/large-thread-hardening-v2.md,docs/benchmarks/large-thread-latest.json | 操作:Modify | 影响:大 thread 优化记录与最新基准 | 说明:追加 thread aggregate 账本这一轮的性能结果、优化原理与 trade-off，保留历史 benchmark 记录并刷新最新 machine artifact | 关联:task038
change041 日期:2026-03-18 | 文件:tests/clean-room/persistence.test.js | 操作:Modify | 影响:thread aggregate 回归覆盖 | 说明:新增 aggregate 重建、自动维护、fingerprint basis 与跨 thread move/delete 计数回归测试 | 关联:task038
change040 日期:2026-03-18 | 文件:src/application/services/threadnoteApplicationService.js | 操作:Modify | 影响:thread content fingerprint | 说明:将 decided/solved/verified/dropped 等 aggregate 字段纳入 fingerprint basis，进一步减少 AI 刷新对明细扫描的依赖 | 关联:task038
change039 日期:2026-03-18 | 文件:src/infrastructure/persistence/repositories/threadnoteRepository.js | 操作:Modify | 影响:写路径 aggregate 维护与 repair 入口 | 说明:在 entry/claim/anchor/delete 写入后触发单-thread aggregate rebuild，并新增 `rebuildThreadAggregatesSync()` 供 migration/repair 使用 | 关联:task038
change038 日期:2026-03-18 | 文件:src/infrastructure/persistence/stores/sqlitePersistenceStore.js | 操作:Modify | 影响:thread aggregate ledger 读写 | 说明:新增 `fetch/upsert/rebuildThreadAggregate`，并将 `fetchThreadCounts()` 与 `fetchThreadFingerprintBasis()` 切到 aggregate-first | 关联:task038
change037 日期:2026-03-18 | 文件:src/infrastructure/persistence/schema/appDatabase.js,.phrase/phases/phase-ai-memory-rag-20260313/task_ai-memory-rag_20260313.md | 操作:Modify | 影响:SQLite migration v8 + phase task | 说明:新增 `thread_aggregates` materialized ledger 表，并将 task037 标记完成、补充 task038 账本化任务 | 关联:task038

change033 日期:2026-03-18 | 文件:.phrase/phases/phase-ai-memory-rag-20260313/adr_ai-memory-rag_20260313.md | 操作:Modify | 影响:retrieval 与页面分页边界 | 说明:新增“页面分页与 AI 上下文彻底解耦”决策，明确 AI 输入必须数据库驱动而非依赖当前已加载 entries | 关联:task037
change034 日期:2026-03-18 | 文件:.phrase/phases/phase-ai-memory-rag-20260313/spec_ai-memory-rag_20260313.md | 操作:Modify | 影响:大 thread AI context 约束 | 说明:补充 1000+ notes thread 的数据库驱动检索目标、锁定规则与验收条件 | 关联:task037
change035 日期:2026-03-18 | 文件:.phrase/phases/phase-ai-memory-rag-20260313/plan_ai-memory-rag_20260313.md | 操作:Modify | 影响:M4 检索接线策略 | 说明:补充 resume/prepare 在大 thread 下走 query-specific retrieval 与页面分页解耦的实现要求 | 关联:task037
change036 日期:2026-03-18 | 文件:.phrase/phases/phase-ai-memory-rag-20260313/task_ai-memory-rag_20260313.md | 操作:Modify | 影响:phase 任务拆解 | 说明:新增 task037，要求将大 thread 的 AI 上下文切换为数据库驱动并补分页一致性验证 | 关联:task037

change032 日期:2026-03-14 | 文件:Sources/Persistence/ThreadnoteRepository.swift | 操作:Modify | 影响:启动期迁移持久化边界 | 说明:补充同步 metadata 读写与批量 entry 落盘接口，让 load 阶段的一次性迁移能原子补写旧数据 | 关联:task036
change031 日期:2026-03-14 | 文件:Threadnote.xcodeproj/project.pbxproj | 操作:Modify | 影响:Threadnote target sources | 说明:将 URLBodyMigration.swift 纳入 Xcode target，避免新增迁移文件只在 SwiftPM 下可见 | 关联:task036
change030 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreURLBodyMigrationTests.swift | 操作:Add | 影响:历史 URL body 迁移回归测试 | 说明:补充 Store.load 一次性迁移与 metadata flag 持久化覆盖，防止旧库升级后只改内存不落盘 | 关联:task036
change029 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/URLBodyMigrationTests.swift | 操作:Add | 影响:URL body 迁移纯逻辑测试 | 说明:覆盖纯 URL 和混合文本 URL 的补写规则，并验证已有 source metadata 不被覆盖 | 关联:task036
change028 日期:2026-03-14 | 文件:Sources/EntryCard.swift | 操作:Modify | 影响:Timeline 富内容渲染判定 | 说明:让 `.mixed` body 也走 RichBodyView，避免历史补写后仍被当作纯文本跳过链接卡片 | 关联:task036
change027 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:entry body 构建与加载期迁移 | 说明:复用统一 URL 识别逻辑，并在 load 时执行一次性补写后同步落盘和打 metadata 标记 | 关联:task036
change026 日期:2026-03-14 | 文件:Sources/Persistence/URLBodyMigration.swift | 操作:Add | 影响:legacy entry URL body backfill | 说明:新增一次性扫描与补写逻辑，将旧 `.text` entry 升级为 `.url/.mixed` 并补齐 source locator | 关联:task036

change025 日期:2026-03-14 | 文件:Sources/Persistence/ThreadnoteRepository.swift | 操作:Modify | 影响:AI snapshot repository boundary | 说明:新增 fetch/upsert/delete AISnapshot 接口，让 Store 可同步读缓存并异步写回最新综合结果 | 关联:task035
change024 日期:2026-03-14 | 文件:Sources/Persistence/PersistenceStore.swift | 操作:Modify | 影响:thread_ai_snapshots SQLite 访问 | 说明:新增 AI snapshot 的 fetch/upsert/delete 实现，保持 snapshot 持久化走单一 SQLite 边界 | 关联:task035
change023 日期:2026-03-14 | 文件:Threadnote.xcodeproj/project.pbxproj | 操作:Modify | 影响:Xcode scheme 构建输入 | 说明:将新增的 AITaskQueue.swift 纳入 Threadnote target，保证 `xcodebuild -scheme Threadnote build` 能编译新队列实现 | 关联:task035
change022 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift | 操作:Modify | 影响:AI snapshot / queue / background sweep 回归测试 | 说明:补充跨会话 snapshot 命中、指纹失效重综合、后台补路由和并发上限为 2 的回归覆盖 | 关联:task035
change021 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/PersistenceStoreTests.swift | 操作:Modify | 影响:thread_ai_snapshots 持久化测试 | 说明:补充 AI snapshot round-trip 测试，防止 SQLite 读写字段漂移 | 关联:task035
change020 日期:2026-03-14 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:应用激活触发器 | 说明:在首屏出现和 scene 回到 active 时调度 background sweep，确保后台 AI 能在用户返回时主动补齐 | 关联:task035
change019 日期:2026-03-14 | 文件:Sources/AI/AITaskQueue.swift | 操作:Add | 影响:全局 AI 并发门控 | 说明:新增优先级 AI 队列，为 routing 和 resume synthesis 提供统一 acquire/release 边界并限制并发为 2 | 关联:task035
change018 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:ThreadState cache / resume orchestration / background sweep | 说明:增加稳定 content fingerprint、持久化 snapshot 命中路径、resume 入库、route/resume 队列接线和后台 debounce/idle sweep | 关联:task035
change017 日期:2026-03-14 | 文件:Sources/Persistence/DatabaseRecords.swift | 操作:Modify | 影响:thread_ai_snapshots row | 说明:新增 ThreadAISnapshotRow，承载 headline/blocks/restart note/open loops/recovery lines 等 AI 快照字段 | 关联:task035
change016 日期:2026-03-14 | 文件:Sources/Persistence/AppDatabase.swift | 操作:Modify | 影响:SQLite migration v4 | 说明:新增 thread_ai_snapshots 表 migration，并为 repository/persistence store 增加 snapshot 读写接口 | 关联:task035

change015 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift | 操作:Modify | 影响:AI-only 输出回归测试 | 说明:更新 Restart Note / Prepare View 测试，覆盖未配置、成功、失败三种状态，移除无后端确定性回退预期 | 关联:task034
change014 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/TestFixtures.swift | 操作:Modify | 影响:MockAIBackend | 说明:为 mock backend 增加 resume/draft handler，支持 LLM-only 输出路径测试 | 关联:task034
change013 日期:2026-03-14 | 文件:Sources/Views/ThreadDocument.swift | 操作:Modify | 影响:Restart Note / Prepare View UI | 说明:按 notConfigured/loading/error/ready 显式渲染 AI 输出状态，不再展示确定性占位内容 | 关联:task034
change012 日期:2026-03-14 | 文件:Sources/Models.swift | 操作:Modify | 影响:ThreadState / PreparedView 状态模型 | 说明:新增 AIContentState，并让 Resume/Prepare 缓存显式区分未配置、加载中、就绪、错误 | 关联:task034
change011 日期:2026-03-14 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:DeterministicAIHelper 输出边界 | 说明:删除确定性 resume/draft 用户可见输出路径，presentation 校验失败不再回退到 snapshot 展示 | 关联:task034
change010 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:Resume Synthesis / Draft Preparation 状态机 | 说明:改为 AI-only 输出契约；无 backend 显示未配置，LLM 成功后才写入用户可见内容，失败时显式报错 | 关联:task034

change009 日期:2026-03-14 | 文件:README.md | 操作:Modify | 影响:持久化说明 | 说明:将 README 的持久化描述改为 workspace `.threadnote/db.sqlite`，并注明旧 `snapshot.json` 仅用于一次性迁移 | 关联:task033
change008 日期:2026-03-14 | 文件:Sources/PersistenceManager.swift | 操作:Delete | 影响:legacy JSON persistence | 说明:删除未调用的旧 JSON 快照持久化实现，避免与当前 workspace SQLite 主路径并存造成误导 | 关联:task033

change001 日期:2026-03-13 | 文件:Sources/Persistence/ThreadnoteRepository.swift | 操作:Add | 影响:增量持久化边界 | 说明:新增后台串行写入仓储，替换 UI 主线程全量 saveSnapshot 路径 | 关联:task032
change002 日期:2026-03-13 | 文件:Sources/Store.swift | 操作:Modify | 影响:ThreadnoteStore 数据流 | 说明:改为按 thread 精准失效 cache、增量 upsert/delete、AI 任务去重、metadata 后台队列 | 关联:task032
change003 日期:2026-03-13 | 文件:Sources/Services/LinkMetadataService.swift | 操作:Modify | 影响:metadata enrichment 队列 | 说明:加入有限并发、URL 去重、回调合并和 LRU cache，移除渲染驱动的 metadata 风暴 | 关联:task032
change004 日期:2026-03-13 | 文件:Sources/Views/ThreadDocument.swift | 操作:Modify | 影响:Working Stream 渲染 | 说明:主线程工作流列表改为 LazyVStack，减少历史条目和富媒体常驻内存 | 关联:task032
change005 日期:2026-03-13 | 文件:Sources/TimelineSheet.swift | 操作:Modify | 影响:Timeline 渲染 | 说明:Timeline 列表改为 LazyVStack，避免一次性创建所有 entry 卡顿 | 关联:task032
change006 日期:2026-03-13 | 文件:Sources/ResourceViews.swift | 操作:Modify | 影响:Resources 渲染 | 说明:资源面板改为惰性列表并移除 onAppear 持久化副作用 | 关联:task032
change007 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/PersistenceStoreTests.swift | 操作:Add | 影响:持久化回归测试 | 说明:补充 snapshot 替换和 discourse relation 替换测试，防止旧数据残留 | 关联:task032
