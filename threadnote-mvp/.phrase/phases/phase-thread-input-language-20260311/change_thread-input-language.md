# Change Log: Thread Input Language

change001 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-input-language-20260311/spec_thread-input-language.md | 操作:Add | 影响:phase-spec | 说明:从 thread-ledger phase 抽离并独立沉淀输入语言正式规格 | 关联:task001
change002 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-input-language-20260311/plan_thread-input-language.md | 操作:Add | 影响:phase-plan | 说明:定义输入语言 phase 的里程碑与退出标准 | 关联:task001
change003 日期:2026-03-11 | 文件:.phrase/phases/phase-thread-input-language-20260311/task_thread-input-language.md | 操作:Add | 影响:phase-task | 说明:拆分 role、object/reference、discourse graph 三组任务 | 关联:task001
change004 日期:2026-03-11 | 文件:Sources/Models.swift | 操作:Modify | 影响:EntryKind+CaptureTag | 说明:扩展 `#role` 词典并补齐 role 到 entry kind 的映射 | 关联:task001
change005 日期:2026-03-11 | 文件:Sources/Store.swift | 操作:Modify | 影响:capture parsing+relation mapping | 说明:将显式 role 解析扩展到完整词典并把未标注输入默认收敛为 note | 关联:task001
change006 日期:2026-03-11 | 文件:Sources/CaptureComposer.swift | 操作:Modify | 影响:tag suggestion parsing | 说明:让 composer 支持完整 `#role` 词典的输入识别与补全 | 关联:task001
change007 日期:2026-03-11 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:stream helper text+entry badges | 说明:更新主输入区提示文案并为新增 role 补齐 badge 展示 | 关联:task001
change008 日期:2026-03-11 | 文件:Sources/QuickCaptureView.swift | 操作:Modify | 影响:quick capture copy | 说明:将 quick capture 文案改为新的输入语言语义 | 关联:task001
change009 日期:2026-03-11 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:captureClassification | 说明:移除旧的自动 role 猜测并将未标注输入默认归为 note | 关联:task001
change010 日期:2026-03-11 | 文件:Sources/Models.swift | 操作:Modify | 影响:entry-object-reference-metadata | 说明:新增 object mention 与 explicit reference 的持久化模型并接入 Entry/CaptureParseResult | 关联:task002
change011 日期:2026-03-11 | 文件:Sources/Store.swift | 操作:Modify | 影响:capture parser | 说明:新增 `@object` 与 `[[reference]]` 解析并在唯一命中时将引用绑定到现有 note 或 thread | 关联:task002
change012 日期:2026-03-12 | 文件:Sources/Store.swift | 操作:Modify | 影响:reference lookups | 说明:新增 note/thread/unresolved 三类 explicit reference 查询，避免与 reply 和 discourse relation 混用 | 关联:task002
change013 日期:2026-03-12 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:note cards+composer copy | 说明:在 note card 中单独展示 explicit references，并将继续输入提示更新为 `#role/@object/[[reference]]` 语义 | 关联:task002
change014 日期:2026-03-12 | 文件:Sources/Models.swift | 操作:Modify | 影响:DiscourseRelationKind | 说明:将关系集合统一为 `supports/opposes/informs/answers` 并保留旧值解码兼容映射 | 关联:task003
change015 日期:2026-03-12 | 文件:Sources/Store.swift | 操作:Modify | 影响:relation inference+ledger derivation | 说明:按新 typed relationships 更新关系推断，并让 `Settled So Far` 仅从 `#decided/#solved/#verified/#dropped` 顶层 notes 过滤生成 | 关联:task003
change016 日期:2026-03-12 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:relation badges+resolved labels | 说明:将关系徽章切换到新关系集合图标并补齐旧账本标签到新命名的兼容展示 | 关联:task003
change017 日期:2026-03-12 | 文件:Sources/Models.swift | 操作:Modify | 影响:CaptureTag suggestion metadata | 说明:为 `#role` 补全新增通俗标题、提示文案、搜索别名与排序优先级，降低学术化感受 | 关联:task004
change018 日期:2026-03-12 | 文件:Sources/CaptureComposer.swift | 操作:Modify | 影响:input core+autocomplete panel | 说明:移除 AppKit NSTextView 桥接并切换为 SwiftUI TextEditor，补全面板移到输入框下方且限制为高相关候选，规避 ViewBridge 报错并修复显示裁切 | 关联:task004
change019 日期:2026-03-12 | 文件:Sources/Store.swift | 操作:Modify | 影响:thread state rendering path | 说明:为 `threadState(for:)` 增加缓存并在 load/persist 时统一失效，避免每次输入都同步重算 resume synthesis | 关联:task004
change020 日期:2026-03-12 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:workbench+inspector state wiring | 说明:将 thread state 在 ContentView 内单次计算后下发到 Workbench/Inspector，移除同帧重复计算 | 关联:task004
change021 日期:2026-03-12 | 文件:Sources/CaptureComposer.swift | 操作:Modify | 影响:autocomplete context+layout | 说明:补全上下文改为仅匹配末尾 token 并将候选固定显示在输入框上方，减少错替换与底部裁切 | 关联:task004
change022 日期:2026-03-12 | 文件:Sources/ContentView.swift | 操作:Modify | 影响:autocomplete layering | 说明:为 Continue 卡片与补全面板添加高 zIndex，确保候选层级高于邻近卡片，避免被覆盖看起来像“截断” | 关联:task004
change023 日期:2026-03-12 | 文件:Sources/CaptureComposer.swift | 操作:Modify | 影响:autocomplete presentation layer | 说明:将补全候选从卡片内联层改为 TextEditor 锚点 popover，确保候选始终处于顶层浮层并避免被同页卡片遮挡 | 关联:task004
