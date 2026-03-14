# Change Log: AI Intake Architecture

change060 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift,Tests/ThreadnoteMVPTests/TestFixtures.swift | 操作:Modify | 影响:prompt budget + local serial queue regression | 说明:新增 resume recent-notes 预算裁剪、route 只发 top-3 候选和本地 backend 串行队列的测试覆盖，并为 mock backend 增加可配置并发偏好 | 关联:task036
change059 日期:2026-03-14 | 文件:Sources/Views/ThreadInspectorView.swift | 操作:Modify | 影响:AI debug prompt stats | 说明:在线程 AI debug 面板新增 Prompt Stats 字段，直接显示本次请求的输入字符统计和预算信息 | 关联:task036
change058 日期:2026-03-14 | 文件:Sources/Store.swift,Sources/AI/LLMProvider.swift | 操作:Modify | 影响:resume-route prompt budget | 说明:将 resume recent-notes 预算从大约 3000 tokens 收紧到 1200，限制单条 snippet 长度与总条数；route 只向 LLM 发送 top-3 候选并压缩候选字段，同时记录 route/resume prompt stats | 关联:task036
change057 日期:2026-03-14 | 文件:Sources/AIIntegration.swift,Sources/Models.swift,Sources/AI/AITaskQueue.swift | 操作:Modify | 影响:provider concurrency preference + debug state | 说明:为 AI backend 新增并发偏好接口，让本地 provider 把 AI 队列收紧为串行；同时为 thread/route debug 状态补充 prompt stats 字段 | 关联:task036

change056 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/LLMProviderTests.swift | 操作:Modify | 影响:Ollama timeout/default regression coverage | 说明:补充 native request 的 `keep_alive`/`timeoutInterval` 断言、本地超时错误包装测试与 Ollama 默认配置一致性测试，防止本地 provider 再次漂移 | 关联:task035
change055 日期:2026-03-14 | 文件:Sources/Settings/AISettingsView.swift | 操作:Modify | 影响:local backend defaults UI | 说明:将设置页默认模型与 baseURL 收口到 `AIProviderKind`，修复 Ollama UI 默认值与 provider 兜底值不一致的问题 | 关联:task035
change054 日期:2026-03-14 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:AI provider default configuration | 说明:为 provider 种类新增统一的默认模型、默认 baseURL 与本地 provider 判断，避免本地配置在多处硬编码分叉 | 关联:task035
change053 日期:2026-03-14 | 文件:Sources/AI/LLMProvider.swift,Sources/AI/OllamaChatModel.swift | 操作:Modify | 影响:local Ollama timeout + keep-alive | 说明:将 ping/route/resume/draft timeout 改为按本地/云端分流；Ollama native 请求新增 150 秒请求级超时、`keep_alive:30m` 与更明确的超时错误，降低本地模型冷启动导致的 `/api/chat` 超时 | 关联:task035

change049 日期:2026-03-14 | 文件:Sources/AI/OllamaChatModel.swift | 操作:Add | 影响:Ollama native provider path | 说明:新增原生 Ollama chat model，直接调用 `/api/chat` 并固定 `think:false`、原生 `format` 与基础 sampling options，绕过 openai-compatible 路径的额外 thinking 开销 | 关联:task034
change050 日期:2026-03-14 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:Ollama backend selection + AI timeout budget | 说明:将 `.ollama` 配置切换为 `OllamaChatModel`，LM Studio 保持 openai-compatible；同时按 native 路径收紧 ping/route/resume/draft timeout | 关联:task034
change051 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/LLMProviderTests.swift | 操作:Modify | 影响:Ollama provider regression coverage | 说明:新增 native request 构造单测并更新 live Ollama 连通性测试，验证 `/v1` baseURL 会被规范化到 `/api/chat` 且 ping 返回精确 `pong` | 关联:task034
change052 日期:2026-03-14 | 文件:Threadnote.xcodeproj/project.pbxproj | 操作:Modify | 影响:Xcode source membership | 说明:将 `Sources/AI/OllamaChatModel.swift` 加入 app 工程源码列表，避免 SwiftPM 通过但 Xcode app 目标缺文件 | 关联:task034

change048 日期:2026-03-14 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:local provider JSON output contract | 说明:为 route、resume、discourse、draft 四条 `generateObject` prompt 在末尾显式注入与 Codable 一致的 JSON 结构说明，修复 `supportsStructuredOutputs=false` 时本地模型缺少 schema 约束而输出漂移的问题 | 关联:task033

change047 日期:2026-03-14 | 文件:Threadnote.xcodeproj/project.pbxproj | 操作:Modify | 影响:Xcode build inputs | 说明:移除工程里对已删除 `Sources/PersistenceManager.swift` 的陈旧引用，恢复当前分支的 Xcode 构建 | 关联:task032
change046 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/TestFixtures.swift | 操作:Modify | 影响:MockAIBackend connectivity coverage | 说明:为测试 backend 增加可注入的 ping handler，并补充 route 连通性失败回归，确保 backend 不可达时 route 直接失败且不再偷偷判决 | 关联:task032
change045 日期:2026-03-14 | 文件:Sources/EntryCard.swift | 操作:Modify | 影响:route connectivity debug UI | 说明:在 note 卡片的 Routing Debug 中新增 connectivity 状态、失败详情和检查时间，直接暴露 route LLM 连通性 | 关联:task032
change044 日期:2026-03-14 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:backend configuration + route logging | 说明:为 backend 配置、ping 和 route planning 增加控制台日志，明确记录当前 backend、ping 结果和 route planner 的原始决策元数据 | 关联:task032
change043 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:LLM route orchestration | 说明:将 route 收口为 LLM 唯一判决链路，deterministic 只提供 support snapshot；新增 route 前置 connectivity probe、失败显式状态，并保留 auto-route 后的 route debug 快照 | 关联:task031

change042 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:discourse relation refresh boundary | 说明:删除未接线的 `refreshDiscourseRelationsViaAI` 备用路径与其死状态，保留当前生效的 deterministic relation rebuild 入口，避免 relation ownership 再次分叉 | 关联:task030
change041 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:route debug cache invalidation | 说明:将 route debug 失效从 thread-level derived state cache 中拆出，只失效未归类 inbox 条目的 route analysis，确保 auto-route 成功后仍可查看最后一次 route 快照 | 关联:task029

change040 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift | 操作:Modify | 影响:route debug regression | 说明:新增 auto-route 成功仍保留 route debug 快照与 ambiguous capture 暴露 route debug 的测试，防止 routing 再次退回黑箱 | 关联:task028
change039 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/ThreadRoutingEngineTests.swift | 操作:Modify | 影响:routing debug coverage | 说明:新增 routing debug state 测试，验证 ambiguous capture 留 inbox 时会暴露候选线程、分数和拒绝原因 | 关联:task027
change038 日期:2026-03-14 | 文件:Sources/EntryCard.swift | 操作:Modify | 影响:route debug UI | 说明:在 note 卡片中新增 route summary 与可展开诊断区，展示 normalized text、item type、routing queries、候选线程和分数组成 | 关联:task028
change037 日期:2026-03-14 | 文件:Sources/ThreadRoutingEngine.swift | 操作:Modify | 影响:routing diagnostics | 说明:将 suggest/decide 收口到统一的 route evaluation，并新增 route debug state，显式暴露 semantic score、retrieval score、阈值与最终判定原因 | 关联:task027

change036 日期:2026-03-14 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:resume debug payload | 说明:为 resume synthesis 记录 backend/model、finish reason、warnings、parsed object 与 raw response body，供 thread 页面直接查看 LLM 连通性和模型回复 | 关联:task026
change035 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift | 操作:Modify | 影响:resume AI debug regression | 说明:新增无 backend 时 thread state 必须显式暴露 `notConfigured` 调试状态的测试，防止 resume LLM 再次静默吞错 | 关联:task025
change034 日期:2026-03-14 | 文件:Tests/ThreadnoteMVPTests/ThreadStateSynthesizerTests.swift | 操作:Modify | 影响:presentation plan rejection coverage | 说明:新增无效 AI presentation plan 必须返回明确 rejection reason 的测试，避免继续无声回退到 deterministic cockpit | 关联:task025
change033 日期:2026-03-14 | 文件:Sources/Views/ThreadDocument.swift | 操作:Modify | 影响:AI debug panel | 说明:在线程页面新增 AI status 与可展开 debug 区块，显示 backend、model、finish reason、parsed response 与 raw response body | 关联:task026
change032 日期:2026-03-14 | 文件:Sources/Store.swift | 操作:Modify | 影响:resume synthesis observability | 说明:移除 resume 路径对 LLM 错误的静默吞掉行为，为未配置、请求失败、plan 被拒绝和成功应用 plan 建立显式 AI debug 状态并挂到 ThreadState | 关联:task025

change031 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/LLMProviderTests.swift | 操作:Modify | 影响:presentation block parsing coverage | 说明:新增 block kind 与 tone 解析测试，确保模型输出的 `nextMove`、大小写与空白变化可稳定归一化为受支持的 presentation plan 枚举 | 关联:task023
change030 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/ThreadStateSynthesizerTests.swift | 操作:Modify | 影响:AI presentation plan coverage | 说明:新增 AI plan 可改变 block 组合与顺序的测试，验证 presentation plan 不再退化为只改 headline 的伪更新 | 关联:task024
change029 日期:2026-03-13 | 文件:Sources/Models.swift | 操作:Modify | 影响:ThreadBlockKind | 说明:扩展受支持的 block kind 集合并让枚举可遍历，为 AI UI planner 提供受限但可表达的 cockpit 组件白名单 | 关联:task021
change028 日期:2026-03-13 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:resume synthesis prompt+response schema | 说明:将 LLM resume prompt 从 prose rewrite 升级为 constrained UI planner，并新增 presentation schema 解析，将模型输出映射为 ThreadPresentationPlan | 关联:task022
change027 日期:2026-03-13 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:ThreadPresentationPlan normalization | 说明:为 resume synthesis 增加 AI presentation plan 数据结构、白名单校验、数量限制与 fallback 归一化，让无效模型输出自动退回 deterministic presentation | 关联:task023

change026 日期:2026-03-13 | 文件:docs/ai-manual-test-dataset.md | 操作:Add | 影响:manual AI validation dataset | 说明:新增可直接手工验证 route、item type、object extraction、Restart cockpit 与 Prepare 的测试线程和粘贴用 notes 数据集 | 关联:task020

change021 日期:2026-03-13 | 文件:Sources/Models.swift | 操作:Modify | 影响:ThreadPresentation+ThreadBlock | 说明:为 thread state 新增 headline + blocks 的受限生成式 UI 数据结构，并将其挂到 ThreadState 上 | 关联:task016
change022 日期:2026-03-13 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:ThreadPresentation generator | 说明:deterministic thread state synthesis 现在会根据 judgment/gap/next move/evidence/sources 生成 ThreadPresentation blocks，替代 prose+bullet 双轨展示 | 关联:task017
change023 日期:2026-03-13 | 文件:Sources/Views/ThreadDocument.swift | 操作:Modify | 影响:Restart Note block renderer | 说明:Restart Note 区域改为 headline + adaptive card blocks 渲染，去掉 summary 与 bullets 的重复展示 | 关联:task018
change024 日期:2026-03-13 | 文件:Sources/Store.swift | 操作:Modify | 影响:LLM enrichment compatibility | 说明:Store 在 LLM resume 返回后只更新 presentation headline/文案，不改变 deterministic block 结构和核心状态字段 | 关联:task019
change025 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/ThreadStateSynthesizerTests.swift | 操作:Modify | 影响:ThreadPresentation coverage | 说明:新增 generative UI presentation blocks 与 Store 侧 presentation 回归测试，确保不再退回 prose/list 重复表达 | 关联:task019

change019 日期:2026-03-13 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:working read compression | 说明:将 anchor stateSummary、evidence 与 latest note 压成短句 working read，避免 Restart Note 继续内联整段原文 | 关联:task013
change020 日期:2026-03-13 | 文件:Sources/Store.swift | 操作:Modify | 影响:anchor summary generation | 说明:新写入的 checkpoint summary 与基于 note 生成的 next step 改用 compact working read，减少后续 Restart Note 的长文污染 | 关联:task014

change012 日期:2026-03-13 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:ThreadStateInput+ThreadStateSnapshot | 说明:为 Restart Note 新增 thread state 输入构建与状态快照模型，让 current judgment、judgment basis、main gap、next move 先以结构化状态产出 | 关联:task012
change013 日期:2026-03-13 | 文件:Sources/Store.swift | 操作:Modify | 影响:threadState orchestration | 说明:Store 先构建 ThreadStateInput 再生成 deterministic thread state snapshot，不再直接走模板优先的 resume synthesis | 关联:task012
change014 日期:2026-03-13 | 文件:Sources/AIIntegration.swift | 操作:Modify | 影响:DeterministicAIHelper thread state synthesis | 说明:用 claim/evidence/source/anchor/discourse 派生 thread-specific judgment、basis、gap 与 next move，替代基于 goal type 的通用 recovery 模板 | 关联:task013
change015 日期:2026-03-13 | 文件:Sources/Models.swift | 操作:Modify | 影响:ThreadState | 说明:为 ThreadState 增加 judgmentBasis 字段，显式保存当前判断依据而不是只保留文案结果 | 关联:task013
change016 日期:2026-03-13 | 文件:Sources/Views/ThreadDocument.swift | 操作:Modify | 影响:Restart Note rendering | 说明:Restart Note 区域改为先展示 summary，再展示结构化状态行，避免 UI 只渲染模板 bullet | 关联:task014
change017 日期:2026-03-13 | 文件:Sources/AI/LLMProvider.swift | 操作:Modify | 影响:resume enrichment prompt | 说明:LLM resume prompt 改为重写 deterministic state snapshot 的表达，不再把模型输出直接当作核心 thread state 判定 | 关联:task015
change018 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/ThreadStateSynthesizerTests.swift | 操作:Add | 影响:restart note regression coverage | 说明:新增 deterministic thread state synthesis 与 LLM enrichment 边界测试，并补充 Store 侧结构化 restart note 回归 | 关联:task015

change001 日期:2026-03-13 | 文件:Sources/Models.swift | 操作:Modify | 影响:CaptureInterpretation+ThreadSignature | 说明:新增 capture interpretation、candidate claims、routing signals 与 thread signature 数据结构，明确 intake 与 routing 中间边界 | 关联:task001
change002 日期:2026-03-13 | 文件:Sources/CaptureParser.swift | 操作:Modify | 影响:CaptureInterpreter | 说明:将显式 tag 解析、无 tag item type 推断、自然语言对象抽取与 candidate claim 提炼收敛为 CaptureInterpreter | 关联:task002
change003 日期:2026-03-13 | 文件:Sources/CaptureParser.swift | 操作:Modify | 影响:item type inference | 说明:让普通输入在无显式 `#tag` 时可推断为 claim/question/source/plan 等基础类型，而不再全部落为 note | 关联:task003
change004 日期:2026-03-13 | 文件:Sources/CaptureParser.swift | 操作:Modify | 影响:object extraction | 说明:对象提取从只识别用户手写 `@object` 升级为结合 NLTagger 与规则的自然语言对象抽取 | 关联:task004
change005 日期:2026-03-13 | 文件:Sources/CaptureParser.swift | 操作:Modify | 影响:candidate claims | 说明:为普通陈述型输入补充 candidate claim 提炼，减少用户先写 `#claim` 的前置要求 | 关联:task005
change006 日期:2026-03-13 | 文件:Sources/ThreadRoutingEngine.swift | 操作:Modify | 影响:ThreadSignatureEngine | 说明:新增 thread signature 派生逻辑，以 title/goal/claims/anchor/open loops/objects 作为 routing 主输入 | 关联:task006
change007 日期:2026-03-13 | 文件:Sources/ThreadRoutingEngine.swift | 操作:Modify | 影响:routing scoring | 说明:将 routing 重写为 capture semantics + thread signature + retrieval support 三段式评分，降低 retrieval-only 归类偏置 | 关联:task007
change008 日期:2026-03-13 | 文件:Sources/ThreadRoutingEngine.swift | 操作:Modify | 影响:ambiguity handling | 说明:保留 auto-route 阈值与分差约束，让歧义输入停留 inbox 而不是强制路由 | 关联:task008
change009 日期:2026-03-13 | 文件:Sources/Store.swift | 操作:Modify | 影响:ThreadnoteStore orchestration | 说明:Store 改为调用 CaptureInterpreter 与新的 ThreadRoutingEngine，删除自身对 capture 语义的直接推断职责 | 关联:task009
change010 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/StoreRoutingTests.swift | 操作:Modify | 影响:thread state / prepare regression | 说明:补充 auto-route、歧义保留 inbox、prepare/no-backend 回归，确保 thread state 与 prepare 能力在新 intake 机制下保持稳定 | 关联:task010
change011 日期:2026-03-13 | 文件:Tests/ThreadnoteMVPTests/CaptureInterpreterTests.swift | 操作:Add | 影响:no-backend deterministic coverage | 说明:新增 capture interpretation、signature-driven routing 与 no-backend 路径测试，验证核心自动化不依赖云端 backend | 关联:task011
