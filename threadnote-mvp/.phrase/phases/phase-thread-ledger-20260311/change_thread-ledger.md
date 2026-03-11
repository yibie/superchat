# Change Log: Thread Ledger

change015 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/spec_note-focus-view.md,.phrase/phases/phase-thread-ledger-20260311/plan_thread-ledger.md,.phrase/phases/phase-thread-ledger-20260311/task_thread-ledger.md | 操作:Add | 影响:note-focus-view-spec | 说明:新增单条 note 的 Note Focus View 规格，定义 typed relationships 的二级展示边界 | 关联:task004
change014 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/spec_note-language.md,.phrase/phases/phase-thread-ledger-20260311/task_thread-ledger.md | 操作:Modify | 影响:discourse-graph-spec | 说明:将输入语言规格升级为以 Question、Claim、Evidence、Source 为核心节点，并加入 supports/opposes/informs/answers 关系定义 | 关联:task003
change013 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/spec_note-language.md,.phrase/phases/phase-thread-ledger-20260311/plan_thread-ledger.md,.phrase/phases/phase-thread-ledger-20260311/task_thread-ledger.md | 操作:Add | 影响:thread-input-language | 说明:新增 `#role / @object / [[reference]]` 的正式输入语言规格，并补 phase 计划与任务追踪 | 关联:task003
change012 日期:2026-03-11 | 文件:Sources/ContentView.swift,.phrase/phases/phase-thread-ledger-20260311/spec_thread-ledger.md | 操作:Modify | 影响:ThreadMemoryRail+phase-spec | 说明:移除右栏 Goal 卡，仅保留 Settled So Far 作为 thread 的进展账本 | 关联:task002
change011 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:CreateThread button icon | 说明:将无效 SF Symbol `square.stack.badge.plus` 替换为系统可用图标以消除运行时警告 | 关联:task002
change010 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:WhereYouLeftOffCard+ThreadWorkingSurface | 说明:将中间主工作区改为上次讨论到这里的单点入口，并在其下方单独显示 notes 区 | 关联:task002
change009 日期:2026-03-11 | 文件:Sources/Models.swift,Sources/Store.swift,Sources/ContentView.swift | 操作:Modify | 影响:ThreadStatus+homeThreads+ThreadToolsMenu | 说明:将 thread 状态收敛为 active 与 archived，并移除界面中的多余状态露出 | 关联:task001
change008 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:WorkspaceSidebar+StreamSidebarRow+ThreadSidebarRow+ListSidebarRow | 说明:将左栏命名统一为 Inbox、Threads、Lists，并移除 Active/Queue/Collection 等系统术语 | 关联:task002
change007 日期:2026-03-11 | 文件:Threadnote.xcodeproj/xcshareddata/xcschemes/Threadnote.xcscheme | 操作:Modify | 影响:LaunchAction | 说明:将 Run scheme 改为非 LLDB 启动器以绕开 Xcode 26 调试附着失败 | 关联:task002
change004 日期:2026-03-11 | 文件:Sources/Store.swift | 操作:Modify | 影响:completeThreadCreation()+createThread() | 说明:将 thread 创建与编辑收敛为标题承载意图 + 用户只选择类别 | 关联:task001
change005 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:NewThreadSheet+threadSubtitle() | 说明:移除独立 goal statement 等输入并清理标题与副标题重复 | 关联:task001
change006 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:threadInspector()+ThreadMemoryRail+ThreadToolsMenu | 说明:将右栏收敛为 Goal 与 Settled So Far，并把 tools 降级到中间工作区顶部菜单 | 关联:task002
change001 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/spec_thread-ledger.md | 操作:Add | 影响:phase-spec | 说明:初始化右栏账本与 goal 简化规格 | 关联:task001
change002 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/plan_thread-ledger.md | 操作:Add | 影响:phase-plan | 说明:定义实现里程碑与 phase gate 边界 | 关联:task001
change003 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-ledger-20260311/task_thread-ledger.md | 操作:Add | 影响:phase-task | 说明:拆分 goal 简化与右栏 ledger 任务 | 关联:task001
