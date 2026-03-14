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
