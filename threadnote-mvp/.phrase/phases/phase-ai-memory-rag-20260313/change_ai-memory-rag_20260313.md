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
