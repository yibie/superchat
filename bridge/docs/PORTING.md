# 移植对照表

基线：`@earendil-works/pi-ai@1.4.0`，commit `58c0b02ce552adab76014e7ed712ab7981359aa8`（`packages/ai`）。

状态标记：`todo` 未开始 / `wip` 进行中 / `done` 已移植且有测试 / `skip` 明确不移植（必填理由） / `gen` 生成物。

## 1. 核心（Tier 0）

| 上游 | Lisp | 状态 | 备注 |
|------|------|------|------|
| `src/types.ts` | `lisp/superchat-bridge-types.el` | done | 语义核心：Message / AssistantMessage / 事件 / Usage / ToolCall / 错误；测试 `test/superchat-bridge-types-test.el` |
| `src/utils/event-stream.ts` | `lisp/superchat-bridge-event-stream.el` | done | 事件流、`result` 承诺、`done`/`error` 终止；测试 `test/superchat-bridge-event-stream-test.el`（20/20 通过） |
| `src/models.ts` | `lisp/superchat-bridge-models.el` | wip | Tier 1 部分已完成（测试 `test/superchat-bridge-models-test.el`）：`createProvider`（仅 `api`）、`calculateCost`、`getSupportedThinkingLevels`、`clampThinkingLevel`、`hasApi`、`modelsAreEqual`，`createModels` 的 `setProvider`/`deleteProvider`/`clearProviders`/`getProviders`/`getProvider`/`getModels`/`getModel`/`getAuth`/`stream`/`streamSimple`（含 `applyAuth`、`transformHeaders`）。同文件内含 `src/api/lazy.ts` 的 `lazyStream`/`lazyApi`/`ProviderStreams`（该文件尚无归属）。未移植：modelsStore、`refresh`/`fetchModels`/publication、`checkAuth`/`getAvailable*`/`login`/`logout`/OAuth、`getAllModels`/`getModelsOfType`/`getModelOfType`、`complete*`、deferred、`generateImages`、`classify`、`filterModels`、`images`/`classifiers`、telemetry、`identity` 选项及 OpenCode 会话头 |
| `src/models-store.ts` | `lisp/superchat-bridge-models-store.el` | todo | 持久化模型目录接口 + 内存实现 |
| `src/model-catalog.ts` | `lisp/superchat-bridge-model-catalog.el` | done | `flattenChatModelCatalog`/`Image`/`Classifier` 及读取 `data/<provider>.json` 转结构体；类型体操（`ChatModelCatalog` 等）不移植；测试 `test/superchat-bridge-model-catalog-test.el`。上游 `model-catalog-types.test.ts` 只测 TS 类型，未译 |
| `src/utils/retry.ts` | `lisp/superchat-bridge-utils-retry.el` | done | `retryAssistantCall` 改为回调 + `run-at-time`（计时器函数是变量，测试不真等）；`isRetryableAssistantError`、`retryDelayMs`、`RetryPolicy`（struct）。Emacs 正则无 lookbehind，状态码一条改为手工检查。测试 `test/superchat-bridge-utils-retry-test.el`（22 个，译自 `test/retry.test.ts`；期望值部分由 node 跑源码取得）。回调不 await |
| `src/utils/overflow.ts` | `lisp/superchat-bridge-utils-overflow.el` | done | `isContextOverflow`、`isRecoverableLength`、`getOverflowPatterns`；26 条正则译为 Emacs 正则。测试 `test/superchat-bridge-utils-overflow-test.el`（10 个，译自 `test/overflow.test.ts`） |
| `src/utils/sanitize-unicode.ts` | `lisp/superchat-bridge-utils-sanitize-unicode.el` | done | `sanitizeSurrogates`：Emacs 字符串里的孤立代理字符 U+D800..U+DFFF；另有 `superchat-bridge-string-trim-js`、`superchat-bridge-utf16-length`（源码没有，JS `trim`/`length` 的等价物）。测试 3 个，期望值由 node 取得 |
| `src/utils/hash.ts` | `lisp/superchat-bridge-utils-hash.el` | done | `shortHash`，对 UTF-16 码元做 32 位运算，输出与源码逐字相同；测试 3 个，期望值全部由 node 运行源码取得 |
| `src/utils/headers.ts` | `lisp/superchat-bridge-utils-headers.el` | done | `headersToRecord`（输入为 HTTP 层的 alist）、`providerHeadersToRecord`；测试 2 个，期望值由 node 取得 |
| `src/utils/error-body.ts` | `lisp/superchat-bridge-utils-error-body.el` | done | `normalizeProviderError` / `formatProviderError` / `truncateErrorText` / `safeJsonStringify`；SDK 错误对象改为 plist（键见文件头）；测试 15 个，译自 `test/error-body.test.ts` |
| `src/utils/provider-retry.ts` | `lisp/superchat-bridge-utils-provider-retry.el` | done | `retryProviderRequest`，回调 + 计时器，延迟、次数、`retry-after(-ms)`、`x-should-retry`、`noRetryStatuses`、中止行为均保留；测试 15 个，译自 `test/provider-retry.test.ts` |
| `src/utils/json-parse.ts` | `lisp/superchat-bridge-utils-json-parse.el` | done | 宽容 JSON 解析（修复 + 流式前缀）；测试 `test/superchat-bridge-utils-json-parse-test.el`（35/35 通过，取自上游 commit 的 `test/json-parse.test.ts` 加移植补充） |
| `src/utils/text.ts` | `lisp/superchat-bridge-utils-text.el` | done | `contentText` / `getSystemMessageText` / `renderSystemMessageUpdate`；上游无 `text.test.ts`，测试 `test/superchat-bridge-utils-text-test.el`（12/12 通过，期望值由 node 直接跑 `text.ts` 取得），首切依赖 |
| `src/utils/validation.ts` | `lisp/superchat-bridge-utils-validation.el` | todo | |
| `src/utils/transcript.ts` | `lisp/superchat-bridge-utils-transcript.el` | wip | 仅 `createInitialSystemMessage`、`normalizeContext`（Models.stream 需要）；其余回放函数未移植；测试在 `test/superchat-bridge-utils-misc-test.el` |
| `src/utils/identity.ts` | `lisp/superchat-bridge-utils-identity.el` | todo | |
| `src/index.ts` | `lisp/superchat-bridge.el` | done | 入口只 `require` 源码 index 导出且 Lisp 已有的模块并 `provide`；源码 index 不导出 `stream`/`streamSimple`（走 `Models.stream`/`streamSimple`），所以不定义函数；provider 与 API 实现不在此加载（与源码一致）；测试 `superchat-bridge-occ-test-entry-file-loads` |
| `src/utils/uuid.ts` | `lisp/superchat-bridge-utils-uuid.el` | todo | |
| `src/utils/estimate.ts` | `lisp/superchat-bridge-utils-estimate.el` | todo | |
| `src/utils/typebox-helpers.ts` | `lisp/superchat-bridge-utils-typebox.el` | todo | `StringEnum` → plist schema |
| `src/utils/diagnostics.ts` | `lisp/superchat-bridge-utils-diagnostics.el` | todo | |
| `src/providers/faux.ts` | `lisp/superchat-bridge-provider-faux.el` | todo | 无网络的端到端验证 |
| `src/api/transform-messages.ts` | `lisp/superchat-bridge-api-transform-messages.el` | done | `transformMessages`、`splitErrorToolResultImages`；测试 `test/superchat-bridge-api-transform-messages-test.el`（11 个，译自 `transform-messages-*.test.ts` 与 `lax-message-content.test.ts`） |
| `src/compat.ts`（仅 `isModelType` 等被核心用到的谓词） | — | 见 §4 | 其余不移植 |
| `src/env-api-keys.ts` | `lisp/superchat-bridge-auth.el` | done | `findEnvKeys`/`getEnvApiKey`，并入 auth 文件；无 Google ADC 结果缓存、无 Bun `/proc` 兜底；测试 `test/superchat-bridge-auth-test.el`（译自 `env-api-keys.test.ts`） |
| `src/auth/types.ts`、`context.ts`、`helpers.ts`（仅 `envApiKeyAuth` 的 resolve）、`resolve.ts`（仅 API key 路径）、`credential-store.ts`（内存版，无 promise 链） | `lisp/superchat-bridge-auth.el` | wip | OAuth（`OAuthAuth`、`lazyOAuth`、`refreshStoredOAuthCredential`、`resolveStoredOAuth`）、`login`/`check`、`AuthPrompt`/`AuthEvent`/`AuthInteraction`/`LoginOptions` 未移植；stored OAuth 凭据会使解析返回 nil；测试 `test/superchat-bridge-auth-test.el` |
| `src/utils/abort.ts` | `lisp/superchat-bridge-utils-abort.el` | done | `AbortController`+`AbortSignal` 合为一个结构体；`raceWithAbortSignal` 为同步版；测试 `test/superchat-bridge-utils-abort-test.el` |
| `src/utils/models-error.ts` | `lisp/superchat-bridge-utils-models-error.el` | done | `ModelsError` 为 error 符号 `superchat-bridge-models-error`，data 为 (消息 code cause)；`formatThrownValue`（`diagnostics.ts` 未移植）以 `superchat-bridge-error-message-text` 代替；测试 `test/superchat-bridge-utils-models-error-test.el` |
| `src/utils/provider-env.ts` | `lisp/superchat-bridge-utils-provider-env.el` | done | 无 Bun `/proc/self/environ` 兜底；测试在 `test/superchat-bridge-utils-misc-test.el` |
| `src/utils/model-operations.ts` | `lisp/superchat-bridge-utils-model-operations.el` | wip | 仅 `assertChatModel`（`getModelType`/`isModelType` 已在 types.el）；image/classifier 的 assert 与 error-result 未移植 |
| `src/images.ts` | `lisp/superchat-bridge-images.el` | todo | 图像生成入口（Tier 2，见 §2b） |
| 无上游（上游用 `openai` npm 客户端） | `lisp/superchat-bridge-http.el` | done | HTTP 缝：`make-process` + curl（见 `docs/HTTP.md`）；`superchat-bridge-http-request`/`-abort`、`superchat-bridge-sse-parser`；请求头与 URL 走 `-K` 配置文件（0600，退出即删），正文走 stdin；测试 `test/superchat-bridge-http-test.el` |

## 2. API 实现（Tier 1 / 2）

| 上游 | Lisp | 状态 | 档位 |
|------|------|------|------|
| `src/api/openai-completions.ts` | `lisp/superchat-bridge-api-openai-completions.el` | done | Tier 1。`stream`、`streamSimple`、`convertMessages`、`buildParams`（全部 thinkingFormat 分支）、`convertTools`、`parseChunkUsage`、`mapStopReason`、`detectCompat`/`getCompat`（全部 provider 分支）、缓存控制、`reasoning_details` 重放、分块循环与事件；`openai` 客户端换成 `superchat-bridge-http` + `superchat-bridge-sse-parser`，`retryProviderRequest` 包住请求。`onPayload`/`onResponse`/`onProviderStreamEvent` 已移植（普通函数）。未移植（整块不做）：GitHub Copilot 动态头、客户端 identity 头（`utils/identity.ts` 未移植）、SDK 自带的 User-Agent/X-Stainless 头、`fetch` 选项、约束采样（`constrained-sampling.ts`：grammar 工具与 JSON-schema strict；带 `constrained-sampling` 的工具会使流以 `error` 事件结束）。`transcript.ts`/`estimate.ts`/`simple-options.ts` 中本文件用到的部分在文件内私有复制（`superchat-bridge-oc--` 前缀），待那些文件有归属后移走。测试 `test/superchat-bridge-api-openai-completions-test.el`（75 个，部分译自 `openai-completions-*.test.ts`，其余按源码行为写；经真实 DeepSeek provider 端到端）。上游 `openai-completions-tool-choice.test.ts`（1956 行，覆盖 toolChoice 的 provider 矩阵）、`-provider-stream-event`、`-vllm-priority` 的断言只部分译入 |
| `src/api/anthropic-messages.ts` | `lisp/superchat-bridge-api-anthropic-messages.el` | done | Tier 1。`stream`、`streamSimple`、`buildParams`（缓存控制 short/long/none、`PI_CACHE_RETENTION`、betas 及 `anthropic-beta` 头替换/抑制、预算思考与自适应思考、`mapThinkingLevelToEffort`、mid-conversation effort 标记、temperature 兼容、metadata、toolChoice、fallbacks）、`convertMessages`（thinking 签名、redacted、空签名兼容、错误工具结果图像后置、原生 tool_addition/tool_removal 与延迟占位工具）、`convertTools`（eager input、strict 工具及 Anthropic 关键字检查）、`mapStopReason`、严格工具降级重试、源码自带的 SSE 解码器（`iterateSseMessages`/`iterateAnthropicEvents`，非 `superchat-bridge-sse-parser`：保留无 data 事件、结尾无空行事件、raw 行）、usage/1h 缓存写入/fallback 模型计价、input_transformations 诊断。SDK 换成 `superchat-bridge-http`（POST `<base>/v1/messages?beta=true`，SDK 的头顺序与 `validateHeaders`），`retryProviderRequest` 包住请求；HTTP 错误文本同 Anthropic SDK `APIError`：`429 {整个 body 的 JSON}`。未移植（整块不做）：OAuth（`sk-ant-oat` key 以 error 事件结束；Claude Code 身份头/系统提示/工具名映射）、GitHub Copilot（该 provider 以 error 事件结束）、工作负载身份联合（federation）、`client`/`fetch` 选项、identity 头、SDK 的 User-Agent/X-Stainless 头及默认 10 分钟超时、constrained-sampling 的 grammar 部分。`transcript.ts`（`getInitialSystemMessage`/`getCurrentTools`/`getCurrentSystemMessage`/`resolveTranscript`）、`estimate.ts`、`simple-options.ts`（`buildBaseOptions`、`adjustMaxTokensForThinking` 等）、`constrained-sampling.ts` 的 JSON-schema 部分、`diagnostics.ts` 的 `appendAssistantMessageDiagnostic` 在文件内私有复制（`superchat-bridge-am--` 前缀）。测试 `test/superchat-bridge-api-anthropic-messages-test.el`（61 个，译自 `anthropic-sse-parsing`、`-thinking-disable`、`-temperature-compat`、`-force-adaptive-thinking`、`-adaptive-thinking-models`、`-mid-conversation-effort`、`-empty-thinking-signature-compat`（vercel/fireworks 目录断言未译）、`-error-tool-result-images`、`-cache-write-1h-cost`、`-auth-token`（User-Agent 与 OAuth 部分未译）、`-eager-tool-input-compat`、`-strict-tool-schema`、`-strict-tool-fallback`（pi-messages 一例未译；内联添加一例上游用 OAuth key，改用普通 key），其余按源码行为写）。未译：`anthropic-tool-name-normalization`、`-oauth`、`-federation*`（OAuth/联合）、`*-e2e`、`-opus-4-8-smoke`、`-thinking-binding-e2e`（需网络） |
| `src/api/openai-responses.ts` | `lisp/superchat-bridge-api-openai-responses.el` | todo | Tier 2 |
| `src/api/openai-responses-shared.ts` | `lisp/superchat-bridge-api-openai-responses-shared.el` | todo | Tier 2 |
| `src/api/openai-prompt-cache.ts` | `lisp/superchat-bridge-api-openai-prompt-cache.el` | done | `clampOpenAIPromptCacheKey`；`buildParams` 对任何设了 `long` 保留期的模型都要用；测试在 `test/superchat-bridge-api-openai-completions-test.el` |
| `src/api/google-generative-ai.ts` | `lisp/superchat-bridge-api-google-generative-ai.el` | todo | Tier 2 |
| `src/api/google-shared.ts` | `lisp/superchat-bridge-api-google-shared.el` | todo | Tier 2 |
| `src/api/mistral-conversations.ts` | `lisp/superchat-bridge-api-mistral-conversations.el` | todo | Tier 2 |
| `src/api/azure-openai-responses.ts` | `lisp/superchat-bridge-api-azure-openai-responses.el` | todo | Tier 2 |


## 2b. 图像生成（Tier 2，用户指定纳入）

对接有图像生成能力的大模型服务。上游把图像与对话模型并列建模（`ImageModel`、`ImageApi`、
`ImagesContext`、`AssistantImages`），通过 `images-api-registry` 分派；**不走** `event-stream`
的增量事件，而是一次性返回 `AssistantImages`（含 `output` 数组、`stopReason`、`errorMessage`）。

| 上游 | Lisp | 状态 | 说明 |
|------|------|------|------|
| `src/types.ts` 中 `ImageModel`/`ImageApi`/`ImagesContext`/`AssistantImages`/`ImagesOptions` | `lisp/superchat-bridge-types.el` | todo | 与 Tier 0 同文件，作为结构体一并移植 |
| `src/images.ts` | `lisp/superchat-bridge-images.el` | todo | `generateImages` 入口 |
| `src/images-api-registry.ts` | `lisp/superchat-bridge-images-api-registry.el` | todo | 按 `model.api` 分派的注册表 |
| `src/image-models.ts` | `lisp/superchat-bridge-image-models.el` | todo | 从生成目录中取图像模型 |
| `src/providers/images/register-builtins.ts` | `lisp/superchat-bridge-provider-images-register.el` | todo | 内置图像 API 的懒注册 |
| `src/api/openrouter-images.ts` | `lisp/superchat-bridge-api-openrouter-images.el` | todo | 基线 commit 中**唯一**的图像 API 实现 |
| `src/api/openrouter-images.lazy.ts` | — | skip | 懒加载专用；Lisp 用 `require`/autoload |

要点：

- 上游当前图像生成只有 OpenRouter 一条 API 路径；OpenAI images、Google Imagen 等不在
  `packages/ai` 的图像链中。若要覆盖，属于「上游没有的能力」，不是一对一移植，需单独立项。
- 图像模型清单同样由 `generate-models` 产出（`IMAGE_MODELS`），走 §5 生成链，不手抄。
- 鉴权：`images.ts` 要求显式传 `options.apiKey`；推荐经 `Models.generateImages()` 由 provider
  解析鉴权。Lisp 侧沿用同一分工。
- 测试：与 `faux` 同思路，用桩数据验证 registry 分派与 `stopReason`/`errorMessage` 语义，不发真实请求。
| `src/api/*.lazy.ts` | — | skip | 上游用它做按需加载；Lisp 用 `require`/autoload 表达，无对应文件 |

## 3. Provider（Tier 1 / 2）

Provider 文件基本是 `createProvider` 的参数表（如 `deepseek.ts` 仅 573 字节），依赖对应 API 就绪。
每个 provider 的 `*.models.ts` 不手抄，由 `sync/generate-models.el` 生成（见 §5）。

| 上游 | Lisp | 状态 |
|------|------|------|
| `src/providers/deepseek.ts`（+ `deepseek.models.ts`，读 `data/deepseek.json`） | `lisp/superchat-bridge-provider-deepseek.el` | wip（Tier 1：用桩 API 的测试通过；等 `superchat-bridge-api-openai-completions.el`，见文件头说明的接口） |
| `src/providers/anthropic.ts`（+ `anthropic.models.ts`，读 `data/anthropic.json`） | `lisp/superchat-bridge-provider-anthropic.el` | done（Tier 1）：`anthropicApiKeyAuth` 的 resolve（存储凭据 → `ANTHROPIC_AUTH_TOKEN` 作 Bearer 头 → `ANTHROPIC_OAUTH_TOKEN` → `ANTHROPIC_API_KEY`），懒加载 API；`oauth`（`lazyOAuth`）、`login`、federation 未移植（federation 变量解析为未配置）。测试 `test/superchat-bridge-provider-anthropic-test.el`（5 个：auth 译自 `anthropic-auth-token`，另有经 `superchat-bridge-models-stream-simple` 的桩 HTTP 端到端：文本、工具调用与结果、429 以 error 事件结束） |
| `src/providers/openai.ts` | `lisp/superchat-bridge-provider-openai.el` | todo |
| `src/providers/google.ts` | `lisp/superchat-bridge-provider-google.el` | todo |
| `src/providers/groq.ts` `xai` `together` `mistral` `zai` `minimax` `moonshotai` `qwen` `cerebras` `fireworks` `deepinfra` `nvidia` `huggingface` `opencode` `opencode-go` `vercel-ai-gateway` `openrouter` `baseten` | `lisp/superchat-bridge-provider-<id>.el` | todo（Tier 2，逐个落地） |
| `src/providers/<id>.ts` 与 `<id>.models.ts`，id 为 ant-ling baseten cerebras groq huggingface moonshotai moonshotai-cn nvidia qwen-token-plan qwen-token-plan-cn qwen-token-plan-individual together xiaomi xiaomi-token-plan-ams xiaomi-token-plan-cn xiaomi-token-plan-sgp zai zai-coding-cn（18 个，均与 `deepseek.ts` 同形） | `lisp/superchat-bridge-providers.el` | done | 一张表（id、名称、base URL、auth 标签、环境变量，逐字抄自各 .ts）加 `superchat-bridge-provider` / `superchat-bridge-provider-ids`；模型读 `data/<id>.json`。`api-key-auth` 结构体多一个 `env-vars` 槽（源码没有），供调用方告诉用户该设哪个变量。测试 `test/superchat-bridge-providers-test.el` |
| 无上游 | 同上 | done | `superchat-bridge-openai-compatible-provider`：本地服务器（LM Studio、Ollama）或任意 OpenAI 兼容端点；无 `:api-key-env` 时 auth 恒解析为占位 key `"none"`。同一测试文件 |
| `src/providers/faux.ts` | 见 §1 | todo |
| `src/providers/all.ts` | `lisp/superchat-bridge-provider-all.el` | todo（聚合，Tier 2 末尾） |
| `src/providers/*.models.ts`（约 47 个） | `gen` | gen | 由生成脚本产出 |
| `src/providers/cloudflare-*.ts` | — | skip | 依赖 Workers binding 运行时 |
| `src/providers/amazon-bedrock.ts` | — | skip | 依赖 aws-sdk 签名与流式事件 |
| `src/providers/azure.ts`、`google-vertex.ts` | — | skip（可复议） | 依赖云厂商认证链；如需要再单独设计 |
| `src/providers/github-copilot.ts`、`openai-codex.ts`、`kimi-coding.ts` | — | skip（可复议） | 依赖 OAuth 登录流程 |

## 4. 明确不移植

| 上游 | 理由 |
|------|------|
| `src/compat.ts`、`legacy-api-aliases` | 旧的全局 API 兼容层，属于产品层；只取核心用到的谓词 |
| `test/json-parse.test.ts` | 上游测试不逐字移植：用例按 `describe` 块一一对译进 `test/superchat-bridge-utils-json-parse-test.el`，另加移植用例（见 §8） |
| `src/auth/oauth/**`、`src/bun-oauth*.ts`、`src/auth/helpers.ts` 的 `lazyOAuth` 部分 | OAuth 流程依赖运行时与浏览器回调；Emacs 侧如需要另做 |
| `src/session-resources.ts` | Pi 产品层的会话资源管理 |
| `src/telemetry*`、`src/cli.ts` | 与接入层无关 |
| `src/models.generated.ts` | 生成物，改为由本仓库生成脚本继承上游逻辑 |
| `scripts/generate-test-image.ts` | 测试图片生成，不属于接入层 |

## 5. 模型清单的生成

上游 `scripts/generate-models.ts` 读取 provider 数据（`providers/data/.manifest.json` + 各 `*.models.ts`）
生成 `models.generated.ts`。本包：

已核对的实际情况（基线 commit）：

- `npm run generate-models` = `node scripts/generate-models.ts --strict`；另有 `check:model-data`、`hydrate-model-catalog` 两个相关脚本。
- `scripts/generate-models.ts` **约 131 KB**，依赖同目录 `model-data.ts`（13 KB）、`openrouter-catalog.ts`、`ai-gateway-pricing.ts`、`models-dev-reasoning-options.ts`、`openrouter-reasoning-options.ts`，并从 `../src/api/cloudflare.ts` 取常量。
- 它读取 `src/providers/*.models.ts` 与 `src/providers/data/` 下的模型数据；**`data/` 目录不在 git 树里**（未提交，属抓取产物），需要 `hydrate` 类步骤先取得。
- 输出除 `models.generated.ts` 外，还可产出 JSON 模型目录（`--json-only --json-output`）。

本包做法：

- 移植整条生成链：`sync/model-data.el`、`sync/generate-models.el` 及依赖的辅助脚本，保持字段、排序、`--strict` 校验语义一致；
- 输出 `lisp/superchat-bridge-models-<provider>.el`（以及可选的模型目录 JSON），附来源 commit 的头部注释；
- 生成物提交进 git，便于 diff 审查上游数据变化；
- 生成脚本本身属手写移植，受测试规则约束，且**必须先在 Tier 0/1 之后**——因为脚本类型引用 `types.ts` 与 `api/cloudflare.ts` 的常量。

**规模提醒**：这条链合计超过 150 KB TypeScript，其中含抓取与价格换算逻辑，工作量可能接近两个 `api/*.ts`。若首版想更快可用，可以先只移植 `check-model-data` + 手工维护的精简清单，把完整生成链放到 Tier 3；但这会偏离「延续上游做法」，需明确决定。

## 6. 命名约定

| TypeScript | Emacs Lisp |
|------------|------------|
| `camelCase` 字段 | `kebab-case` 键 |
| `providerId` | `provider-id` |
| 谓词 `isX` | `superchat-bridge-x-p` |
| 类型 | 结构体（`cl-defstruct`），`:include` 表达联合类型的公共字段 |
| 枚举字符串 | 符号，保留上游字符串常量名 |
| `async` | 返回 `Promise` 的等价物：完成回调 +（可选）deferred |
| `throw new Error(...)` | `signal` 上游语义对应的条件（`superchat-bridge-error` 子类型） |

## 7. 同步机制

1. `sync/upstream.sh`：sparse checkout `packages/ai`，记录 commit。
2. `sync/sync-report.el`：对比本表与上游文件清单，输出三类：
   - **新增文件**（上游有、本表无）→ 需人工决定是否移植；
   - **已移植文件变更**（上游 diff 命中已 `done` 的文件）→ 待审查更新；
   - **删除/改名**（本表有、上游无）→ 需处理。
3. 任何上游变更都不自动改写 `lisp/`；报告生成任务，由人审查、补测试后落地。
4. 基线 commit 更新需同步修改本文件首行与 README 的表格。
5. `sync/fetch-model-data.sh`：从 unpkg 取 `@earendil-works/pi-ai@<版本>/dist/providers/data/` 的
   `.manifest.json` 及其列出的全部 42 个 `*.json` 到 `data/`，逐个按 manifest 里的 sha256 校验，
   全部通过才落盘，任一不符即退出非零。基线 1.4.0 实测：42/42 通过，共 43 个文件、约 967 KB。
   HTTP 传输选型见 [`HTTP.md`](HTTP.md)。

## 8. 移植记录：`utils/json-parse.ts` 与 `utils/text.ts`

### `src/utils/json-parse.ts` → `lisp/superchat-bridge-utils-json-parse.el`

| 上游 | Lisp |
|------|------|
| `repairJson` | `superchat-bridge-repair-json` |
| `parseJsonWithRepair` | `superchat-bridge-parse-json-with-repair` |
| `parseToolCallArguments` | `superchat-bridge-parse-tool-call-arguments` |
| `parseStreamingJson` | `superchat-bridge-parse-streaming-json` |

- 测试 35 个全过。上游 `packages/ai` 工作树里没有 `test/json-parse.test.ts`，用例取自基线 commit
  58c0b02c 的该文件，按 `describe` 块顺序一一对译（测试中注 "source"），另加标注 "port" 的用例，
  覆盖 Emacs 运行时差异与修复逻辑本身。
- 移植初期自己编过两个期望值，均被上游实际行为推翻，已按 node 跑上游代码的结果改正：
  - `partial-half-a-member`：截断到半个成员的流式 JSON，`partial-json` 交出的是已闭合的部分
    （成员名或已读到的值），并非整体丢弃；
  - `streaming-unreadable`：流式解析收到的不可读文本，返回最后一次可解析的前缀，不抛错。
- 与原实现的差异（均源于 Emacs 运行时，注释写在文件头）：非 UTF-8 文本按 ISO-8859-1 读；
  不接受 `NaN`/`±Infinity`；代理对单独解码、孤立代理保留 `\uXXXX` 原文；第三步的
  `partial-json` 0.1.7 不在源码树内，`superchat-bridge--parse-partial-json` 为按其行为重写。
- JSON 值映射：对象为 hash table，数组为 vector，`true` → t，`false` → `:false`，
  `null` → `:null`；上游给 `{}` 的地方返回空 hash table，从不返回 nil。

### `src/utils/text.ts` → `lisp/superchat-bridge-utils-text.el`

| 上游 | Lisp |
|------|------|
| `contentText` | `superchat-bridge-content-text` |
| `getSystemMessageText` | `superchat-bridge-system-message-text` |
| `renderSystemMessageUpdate` | `superchat-bridge-render-system-message-update` |

- 上游没有 `text.test.ts`。测试 `test/superchat-bridge-utils-text-test.el`（12 个，全过）的期望值是
  直接运行 `src/utils/text.ts` 本体取得的：去掉 `import type` 与参数、返回值注解，函数体不动，
  用 node 22 打印各表达式的 `JSON.stringify`，每个断言旁注明对应 TS 表达式，便于换基线后重跑。
- 由此发现并修正的移植错误：
  1. 空列表拼接：上游 `[...].join(sep)` 对空数组得空字符串，移植版返回 nil；影响
     `contentText([])`、无文本块的内容，以及全空的 `getSystemMessageText`。改为 `mapconcat`。
  2. 缺 `(require 'seq)`（用了 `seq-filter`）。
- 语义要点：只取 text 块，thinking / image / tool call 一律跳过；字符串内容直接原样返回；
  `sections` 为 nil 值的项表示「已删除」，`getSystemMessageText` 跳过它而非输出 "null"，
  空串片段也不输出；`renderSystemMessageUpdate` 对删除项输出 `Removed system prompt section "x".`，
  对更新项输出 `Updated system prompt section "x":` 加两个换行再加新文本，片段间用空行连接。
