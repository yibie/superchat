# superchat-bridge

A faithful Emacs Lisp port of [`@earendil-works/pi-ai`](https://github.com/earendil-works/pi/tree/main/packages/ai).
Semantics and behavior follow the upstream package; Pi's product layer is intentionally not ported.

上游是 TypeScript。本包把 `packages/ai` 逐文件移植为 Emacs Lisp，作为 Emacs 里接入 LLM 服务的框架：
统一的 provider 协议、流式事件流、模型目录与鉴权。

## 同步基线

| 项 | 值 |
|----|----|
| 上游仓库 | `https://github.com/earendil-works/pi` |
| 上游路径 | `packages/ai` |
| 固定 commit | `58c0b02ce552adab76014e7ed712ab7981359aa8` |
| 上游提交时间 | 2026-10-22T00:29:24+02:00 |
| 上游包版本 | `@earendil-works/pi-ai@1.4.0` |
| 上游规模 | `src/` 约 230 个 .ts；`providers/` 94 个；`api/` 42 个 |

逐文件映射、移植状态与不移植理由见 [`docs/PORTING.md`](docs/PORTING.md)。

## 定位边界

上游的入口 `src/index.ts` 自称 *core only, side-effect free*：不含生成的模型目录、provider 工厂、
api-registry、OAuth 实现与 compat 层。本包与之对齐，分三类：

- **移植**：核心类型、事件流、Provider/Models 协议、模型目录接口、纯工具函数、`faux` provider。
- **按需移植**：各 `api/*` 实现与 `providers/*`，按 Tier 递进（见 PORTING.md）；包括图像生成链（§2b）。
- **不移植**：`compat`、`legacy-api-aliases`、`oauth*`、`auth/*`、`session-resources`、`telemetry`、
  `cli.ts`、`cloudflare-*`、`bedrock`、`google-vertex` 等依赖运行时或 OAuth 的部分。

## 目录结构

```
superchat-bridge/
├── superchat-bridge.el              ; 入口：require 核心模块，公开 API
├── lisp/
│   ├── superchat-bridge-types.el          ; src/types.ts
│   ├── superchat-bridge-event-stream.el   ; src/utils/event-stream.ts
│   ├── superchat-bridge-models.el         ; src/models.ts
│   ├── superchat-bridge-models-store.el   ; src/models-store.ts
│   ├── superchat-bridge-model-catalog.el  ; src/model-catalog.ts
│   ├── superchat-bridge-utils-*.el        ; src/utils/*.ts
│   ├── superchat-bridge-api-*.el          ; src/api/*.ts
│   ├── superchat-bridge-provider-*.el     ; src/providers/*.ts
│   └── superchat-bridge-models-*.el       ; 生成物：模型清单数据（见下）
├── test/                     ; ERT，对应上游 test/*.test.ts
├── sync/
│   ├── upstream.sh           ; sparse checkout 拉取 packages/ai
│   ├── generate-models.el    ; 移植自 scripts/generate-models.ts
│   └── sync-report.el        ; 差异 → 待审查的移植任务
├── docs/PORTING.md
└── README.md
```

## 模型清单：延续上游做法

上游用 `scripts/generate-models.ts` 从 provider 数据生成 `models.generated.ts`。本包不手抄模型清单，
而是把该脚本的生成逻辑移植为 `sync/generate-models.el`，产出 `lisp/superchat-bridge-models-*.el`。
生成物是数据、不是实现，不计入手写移植；生成逻辑本身一对一对应上游脚本。

## 移植规则

1. **语义基准是上游。** 类型、字段名、事件类型、错误语义、流式事件顺序以 `packages/ai` 为准。
   Lisp 侧只做命名转换：`camelCase` → `kebab-case`，谓词加 `-p`，构造器 `make-superchat-bridge-*`，
   访问器 `superchat-bridge-*-field`；另加通用省略前缀的短访问器（`superchat-bridge-message-*`）。前缀不与命名规则机械对应，
   所以映射表逐条手写。
2. **一个源文件对应一个 Lisp 文件。** 不做顺手重构合并；上游新增文件时同步新增模块。
3. **测试对应。** 上游 `test/x.test.ts` 能离线跑的，尽量转成 `test/superchat-bridge-x-tests.el` 的 ERT 用例；
   需要真实凭据的标为跳过并注明来源。
4. **同步不覆盖。** 上游变更由 `sync/` 发现、分类、生成任务，绝不自动改写本地实现；
   每个改动都要人工审查 + 测试通过才落地。

## 依赖取舍

| 上游依赖 | Emacs 侧 |
|----------|----------|
| SDK（anthropic / openai / google / aws） | 直接构造 HTTP 请求（`url` / `plz`） |
| JSON / SSE 解析 | `json-parse-string` / `json-serialize`，SSE 手写解析，对齐上游细节 |
| `AsyncIterable` + 事件流类 | 回调式事件流，`push` / `end` / 完成回调 + `result` 承诺 |
| `AbortSignal` | 取消 token（闭包 + 状态符号） |
| `typebox` | plist 形式的 JSON Schema |
| `structuredClone` | `copy-tree` / 显式拷贝（plist 无共享风险） |

## 路线图

| 阶段 | 内容 |
|------|------|
| Tier 0 | `types` → `event-stream` → `utils` 纯函数 → `faux` provider |
| Tier 1 | `api/openai-completions` + `providers/deepseek`；`api/anthropic-messages` + `providers/anthropic` |
| Tier 2 | 其余 `openai-completions` 家族 provider（groq / xai / together / mistral / zai / minimax …）；图像生成链（`images` / `images-api-registry` / `image-models` / `api/openrouter-images`） |
| Tier 3 | `generate-models.el` 生成模型清单；`sync/` 差异报告 |

## 状态

尚无代码落地。首次提交从 Tier 0 开始。
