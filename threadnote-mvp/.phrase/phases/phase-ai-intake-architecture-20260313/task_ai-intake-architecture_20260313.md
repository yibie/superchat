# Task List — phase-ai-intake-architecture-20260313

task001 [x] 场景: 建立 CaptureInterpretation 数据结构 | Given: capture 语义散落在 Store 内部 | When: 抽出标准解释结果结构 | Then: normalizedText / itemType / objects / candidateClaims / routingSignals 成为显式输出 | 验证: 单元测试

task002 [x] 场景: 提取 CaptureInterpreter 模块 | Given: Store 直接处理 capture 解析 | When: 将 parse + infer + extract 逻辑迁出 Store | Then: capture 解释与 orchestration 解耦 | 验证: 单元测试

task003 [x] 场景: 无显式标签时自动判断 item 类型 | Given: 用户输入普通 note 且未写 #tag | When: CaptureInterpreter 分析输入 | Then: 系统可区分 note / claim / question / evidence 等基础类型 | 验证: 单元测试

task004 [x] 场景: 从自然语言中抽取对象 | Given: 用户未手写 @object | When: CaptureInterpreter 分析输入 | Then: 系统可抽取人名 / 组织 / 产品 / 地点等核心对象并结构化保存 | 验证: 单元测试

task005 [x] 场景: 从普通输入提炼 candidate claims | Given: 用户输入一条陈述型 note | When: CaptureInterpreter 分析输入 | Then: 系统生成候选 claim 信号，不要求用户先写 #claim | 验证: 单元测试

task006 [x] 场景: 建立 ThreadSignature 模型 | Given: routing 目前主要依赖 retrieval 文档聚合 | When: 引入 ThreadSignatureEngine | Then: 每个 active thread 派生 title / goal / claims / anchor / openLoops / objects 的紧凑签名 | 验证: 单元测试

task007 [x] 场景: 用 thread signature 驱动 route | Given: CaptureInterpretation 与 ThreadSignature 已存在 | When: 重写 ThreadRoutingEngine 内部评分模型 | Then: auto-route 由 capture semantics + thread signature 主导，retrieval 只做辅助证据 | 验证: 单元测试

task008 [x] 场景: ambiguous note 保持在 inbox | Given: 一条输入同时接近多个 thread 或语义不足 | When: RoutingEngine 评估结果 | Then: 返回 suggestion 或 stayInInbox，不强制 auto-route | 验证: 单元测试

task009 [x] 场景: Store 收敛为 orchestration 层 | Given: Store 当前混合 capture 理解、route、thread state 构造 | When: 接入新模块 | Then: Store 只负责编排和副作用，不再承载 capture intelligence 细节 | 验证: swift test

task010 [x] 场景: 保持 Thread State 与 Prepare 能力 | Given: 新 intake/routing 机制接入 | When: 用户打开 thread 或 Prepare | Then: Restart Note / current judgment / open loops / next action / prepared context 继续可用 | 验证: store-level 测试

task011 [x] 场景: 无 backend provider 时核心链路仍工作 | Given: 设备无有效 LLM backend | When: 用户 capture / open thread / prepare | Then: 系统仍可完成类型判断、对象抽取、route 与 thread state 的基础输出 | 验证: 回归测试

task012 [x] 场景: 建立 ThreadStateInputBuilder | Given: Restart Note 输入在 Store 内零散拼装 | When: 抽出 thread state 输入构建边界 | Then: goal/signature/claims/evidence/sources/anchor/delta/discourse 成为显式输入 | 验证: 单元测试

task013 [x] 场景: 用状态综合替代模板优先的 Restart Note | Given: deterministic resume 目前主要依赖通用模板 | When: 引入 deterministic thread state synthesizer | Then: current judgment / judgment basis / main gap / next move 由 thread-specific state 推导而来 | 验证: 单元测试

task014 [x] 场景: Restart Note UI 渲染结构化快照 | Given: 当前 UI 主要渲染 recovery lines 模板文案 | When: 接入新的 thread state snapshot | Then: Restart Note 展示 thread-specific summary 与结构化状态字段，而不是仅展示通用模板 | 验证: store-level 测试

task015 [x] 场景: 收敛 Restart Note 的 LLM enrichment 边界 | Given: 当前 LLM 异步可能直接覆盖 deterministic state | When: 将 LLM 约束为表达增强而非状态判定 | Then: 核心状态字段保持 deterministic 一致性，LLM 仅增强 restart note 文案 | 验证: 回归测试

task016 [x] 场景: 建立 ThreadPresentation 数据结构 | Given: Restart Note 同时保留 prose 与 bullet 两套冗余表示 | When: 引入 block-based ThreadPresentation | Then: thread state 通过 headline + blocks 表达工作界面，而不是重复字符串 | 验证: 单元测试

task017 [x] 场景: 用 deterministic generator 生成 thread cockpit | Given: ThreadStateSnapshot 已存在 | When: 基于 judgment/gap/next move/evidence/sources 生成受限 blocks | Then: thread 页面可根据状态生成不同 block 组合，而不是固定摘要模板 | 验证: 单元测试

task018 [x] 场景: 将 Restart Note 区域改为 block renderer | Given: ThreadDocument 当前仍渲染 summary + bullets | When: 接入 ThreadPresentation | Then: 页面展示 headline、重点卡片和行动块，不再重复同一状态的 prose/list 两种表达 | 验证: store-level 测试

task019 [x] 场景: 保持无 backend 与 LLM enrichment 兼容 | Given: generative UI 接入后仍需支持 deterministic fallback | When: backend 不可用或 LLM 返回晚到 | Then: deterministic presentation 可直接渲染，LLM 仅增强 headline/文案而不破坏 block 结构 | 验证: 回归测试

task020 [x] 场景: 提供可直接上手的 AI 手动测试数据集 | Given: 用户需要快速验证 route/restart/prepare/object extraction 等 AI 行为 | When: 提供线程初始化数据和可粘贴测试 notes | Then: 用户可在空库中按步骤直接验证主要 AI 能力，而不需要自己编测试数据 | 验证: 文档检查

task021 [x] 场景: 建立 AI presentation plan schema | Given: 当前 AI update 只能改 restart 文案，不能改 block 结构 | When: 为 resume synthesis 增加可返回的 presentation plan | Then: AI 可选择 block kind、顺序、标题、摘要与 items，而不再只改 headline | 验证: 单元测试

task022 [x] 场景: 将 LLM resume prompt 升级为 UI planner | Given: 当前 prompt 只要求 rewrite restart note | When: 在 prompt 中显式约束支持的 block 类型和 UI planning 目标 | Then: 有 backend 时 AI 可以真正重组 thread cockpit，而不只是重写 prose | 验证: 回归测试

task023 [x] 场景: 为 AI presentation plan 增加严格回退与校验 | Given: 模型输出可能为空、越界或胡乱规划 | When: 对 presentation plan 做 kind 白名单、block 数量与文本完整性校验 | Then: 无效 plan 自动回退到 deterministic presentation，不破坏 thread 页面 | 验证: 单元测试

task024 [x] 场景: 让 AI update 真正影响 cockpit 结构 | Given: 当前 LLM 返回后仍沿用 deterministic block 组合 | When: 将 presentation plan 接入 Store 和 ThreadDocument | Then: AI update 可以改变 block 组合与顺序，而不只是改 headline | 验证: store-level 测试

task025 [x] 场景: 显式暴露 resume LLM 的运行状态与错误 | Given: 当前 resume synthesis 失败会被 `try?` 静默吞掉 | When: 为 thread state 增加 AI debug 状态并在 Store 中捕获失败/无配置/无效 plan | Then: 用户能直接看到 LLM 未配置、请求失败或 plan 被拒绝，而不是误以为 AI 已工作 | 验证: 单元测试

task026 [x] 场景: 在 Thread UI 中显示 LLM 连通性与回复调试信息 | Given: 用户需要判断 AI 是否真的连上并看到模型回复内容 | When: 在线程页面渲染 AI status + backend/model/finish reason/parsed response/raw response | Then: 用户可直接排查 LLM 连接性与具体回复，而不需要猜测 AI 是否生效 | 验证: swift test + xcodebuild

task027 [x] 场景: 显式暴露 deterministic route 的判定过程 | Given: auto-route 当前只返回是否命中，用户看不到候选线程和阈值判断 | When: 为 routing engine 增加统一的 debug state，包含 interpretation、候选线程、语义分、retrieval 分和最终决策 | Then: note 没有被正确归类时，用户可以直接看到 route 为什么命中或留 inbox | 验证: 单元测试

task028 [x] 场景: 在 note 卡片中显示 route debug | Given: 用户需要在 inbox/stream 中直接看每条 note 的 route 判定 | When: 在 EntryCard 接入 route debug summary 与可展开候选线程诊断区 | Then: 用户无需猜测 route 是否是 AI、为何没归类好、以及当前最接近哪些 thread | 验证: swift test + xcodebuild

task029 [x] 场景:auto-route 后保留 route debug 快照 | Given: inbox note 命中 thread 后会触发 thread cache 失效 | When: 用户查看已自动归类 note 的 route debug | Then: 已路由 note 仍保留最后一次 route decision、候选线程与理由，而不会被 thread-level cache invalidation 清空 | 验证: swift test

task030 [x] 场景: 清理未接线的 discourse relation 备用分支 | Given: Store 内同时保留 deterministic relation rebuild 与未调用的 AI/heuristic append 分支 | When: 收敛 relation 刷新入口 | Then: discourse relation 只保留当前生效的一套刷新逻辑，避免后续误接线 | 验证: rg `refreshDiscourseRelationsViaAI`

task031 [x] 场景: LLM 成为 route 的唯一判决器 | Given: deterministic routing 现在只应提供候选与证据 | When: note 进入 auto-route 或 suggestion 流程 | Then: 最终 routed / inbox 决策只能来自 LLM route planner，deterministic 分数仅作为 support 输入与 debug 解释 | 验证: swift test

task032 [x] 场景: route 显式暴露 LLM 连通性、失败与日志 | Given: route 之前对 backend 不可达和请求失败缺少可观测性 | When: auto-route 检查 backend 并请求 plan | Then: route debug 会显示 connectivity 状态/详情/检查时间，失败不会静默 fallback，控制台可见 route 请求日志，Xcode 工程可正常构建当前改动 | 验证: swift test + xcodebuild

task033 [x] 场景: 本地 provider 在无 structured outputs 时仍返回可解析 JSON | Given: Ollama / LM Studio 配置 `supportsStructuredOutputs=false` 时不会自动收到 schema 约束 | When: route / resume / discourse / draft 触发 `generateObject` | Then: prompt 末尾显式注入与 Codable 完全一致的 JSON 结构说明，模型输出不再自由漂移到自创字段 | 验证: swift test

task034 [x] 场景: Ollama 走 native chat API 且关闭 thinking | Given: OpenAI-compatible `/v1/chat/completions` 会让 qwen3.5 本地推理额外消耗 thinking 延迟 | When: LLMProvider 配置 Ollama backend | Then: 底层模型切换到原生 `/api/chat` + `think:false`，LM Studio 仍保留 openai-compatible 路径，现有 generateText/generateObject 上层调用无需改动 | 验证: `swift test --filter LLMProviderTests`
