# Decision: 权限按 DeepSeek Harness 的方式重做——模式、护栏、一次性升级

Status: proposed — 用户于 2026-09-17 选择 "(a) 换"（按 dsh 模型替换 Authority）；本记录只定契约，代码等用户确认后再动

## Problem

superchat 现在的 Authority（`superchat-authority.el`，2140 行）是一台决策机：每个工具先由 resolver 解析成 8 种 Effect 之一，再叠 5 个内置策略和任意策略层（只能收紧）、算批准哈希、查免提示读名单和敏感 buffer 名单，最后才决定 allow/ask/deny。默认体验是：除了 4 个读工具，**每一步都问用户**（编辑、求值、进程都先问）。用户 2026-09-17 的原话："我现在已经不想搞这个麻烦的 authority 了"。

DeepSeek Harness（`~/Documents/reference-code/deepseek-harness`，c389f96）没有这台机器。它的 Agent Note 明确否决了这条路："Generic ToolRuntime wrapping any tool — rejected: mechanically false for in-process tools; the declarative-effects rewrite is unjustified"（`.agents/notes/implemented/feature/2026-07-06-sandbox.md` § Alternatives）。按用户 2026-09-15 定下的规矩——dsh 没有对应结构的就删——Effect、策略层、批准哈希、免提示名单都该删。

这也回答了 M5 遗留的"模型选错目标改了来源文件"问题：dsh 不靠决策表，靠**模式 + 在真正写入的地方拦 + 被拦后一次性申请**。它保护的是区域（workspace 之外），不是指名的文件；本记录不假装能做到指名保护，[昨天的写入范围契约提案](2026-09-17-emacs-write-scope-contract.md)据此作废。

## Proposal

### dsh 的三件套，逐件对应到 Emacs

| dsh | 做什么 | superchat 对应 |
|---|---|---|
| **沙箱模式** `read-only` / `workspace-write` / `danger-full-access`，每个 session 一个值，`sandbox/mode` 事件记录切换（`packages/sandbox/sandbox-policy`） | 只管文件效果：`read-only` 不许写，`workspace-write` 只许写 workspace 根目录和临时目录之下，`danger-full-access` 不拦 | 同名三档，每个 Session 一个值，Tape 事件 `sandbox.mode`；写入对象是 **buffer 和文件**（Emacs 对象），不是操作系统进程 |
| **在 provider 层拦**：bash 用 OS 沙箱（Seatbelt/bwrap/Landlock），文件工具在 `ctx.fs` 实现里做"规范化路径后判包含"（`packages/fs/fs-sandbox`）；决策记录写明 "enforcement point: provider, not intent gate" | 拦截发生在干活的地方，任何调用者都绕不过；不在工具入口猜 | Emacs 的 provider 就是 buffer 和文件原语：见下文"两道护栏" |
| **一次性升级**：被拦的调用返回标记 `[sandbox: file access denied under <mode> mode]`；模型同一轮用同样的调用加 `sandbox_permissions` + `justification` 重试一次；这次重试才向用户要批准；`allowed-once` 只放行这一次 | 不问就能干活，只有真被拦了才问一次 | 同样的两个工具参数、同样的标记文本、同样的四种结果 |
| **批准策略** `ask` / `never`，每 session 一个值，`approval/policy` 事件；没有回答者时 `unavailable`，一律视为拒绝（`docs/subsystems/approval.md`） | `never` = 无人值守，任何申请直接拒绝 | 同名两档；`noninteractive` 的 Emacs 没有人可问，自然是 `unavailable` |
| **权限预设**：`workspace-write + ask`（默认）、`danger-full-access + never`，一个选择器同时写两个旋钮 | 用户只面对一个开关 | 同样两个组合；superchat 里 "Preset" 已另有所指，所以叫**权限档位**（permission level），命令 `/permissions` |

### 两道护栏（provider 层）

只在**工具 body 执行的动态范围内**生效，对用户自己的编辑和 Run 之外的 Emacs 没有任何影响。

1. **buffer 修改护栏。** 一个全局 `before-change-functions` 钩子：工具 body 执行期间，任何对"不可写 buffer"的文本修改都 signal `superchat-sandbox-denied`。`read-only` 下所有 buffer 不可写；`workspace-write` 下，有文件的 buffer 按其文件路径判定，无文件的 buffer 视为内存草稿、可写。`edit_buffer`、`replace_buffer`、`append_to_buffer`、`eval_elisp` 里的 `insert`/`delete-region`/`erase-buffer` 全部经过这一处。
2. **文件写入护栏。** 一个临时 `file-name-handler-alist` 处理器：工具 body 执行期间，写类文件操作（`write-region`、`delete-file`、`rename-file`、`copy-file`、`make-directory`、`delete-directory`、`set-file-modes`、`make-symbolic-link`）先把目标路径规范化（`file-truename` 到最深的已存在祖先，与 dsh 的 canonicalize-then-contain 相同），落在可写根之外就 signal；读类操作透传。`save-buffer`、`write-file`、`append-to-file`、`with-temp-file` 都经过 `write-region`，因此都被拦下。

可写根 = Session 的 workspace 根 + `temporary-file-directory`（dsh：`writableRoots` = workspace root + `/tmp` + `os.tmpdir()`，且只在 `workspace-write` 下非空）。workspace 根不新造概念：优先用已有的 Session Workspace 绑定（`/workspace DIR`，`superchat--workspace-binding-for-directory` 已做 `file-truename`，Request 里就是 `:workspace-root`），其次是 View 的 `superchat-tools-default-directory`（来源 buffer 的 project root），最后退到 Session 创建时的 `default-directory`。

**明确不承诺的**（与 dsh 对 `run_code` 的表述一致："containment, not a security boundary"）：有意绑定 `inhibit-modification-hooks`、临时置空 `file-name-handler-alist`、在 Lisp 里 `call-process` 出去改文件，仍然能写。这挡的是模型按普通方式选错目标，不是沙箱。shell/eshell 命令在操作系统里跑，两道护栏管不到——见待定第 4 项。

### 升级申请（escalation）——文本照抄 dsh

以下字符串来自 `packages/sandbox/sandbox/src/escalation.ts`、`packages/fs/tool-fs/src/sandbox.ts`、`packages/shell/tool-bash/src/index.ts`（c389f96），superchat 原样采用，只把 `command`/`operation` 按工具替换。

- **只在 confining 模式下**（`read-only`/`workspace-write`）才给 `edit_buffer`、`replace_buffer`、`append_to_buffer`、`eval_elisp`、`shell-command`、`eshell-command` 加两个可选参数（dsh："advertise the escalation fields unconditionally — rejected"）：
  - `sandbox_permissions`：枚举 `workspace-write` / `danger-full-access`；描述 "The wider sandbox mode this file operation needs. Only valid as a one-shot retry of an operation the sandbox just denied; requires justification and user approval."
  - `justification`："Required with sandbox_permissions: one sentence for the user explaining why this exact file operation needs the wider access."
  - 配对校验：缺一个或 justification 为空，分别以 "invalid escalation: sandbox_permissions requires a justification" / "invalid escalation: justification is only valid together with sandbox_permissions" / "invalid justification: expected a non-empty sentence" 失败。
- **被拦的调用**返回两行标记，Run 继续：
  ```text
  [sandbox: file access denied under <mode> mode]
  [sandbox: escalation available — retry this exact operation once with sandbox_permissions (the narrowest wider mode that suffices) + justification; the approval prompt asks the user]
  ```
  第二行只在 confining 模式下附加。护栏内部的 signal 文本："cannot write \"<path>\": file access denied under <mode> mode"。
- **带参数的重试是一次新的、独立记录的工具调用**（dsh："Auto-retry inside the same tool call — rejected"）。执行前按顺序：
  1. 严格加宽：`WIDER_MODES` = `read-only → (workspace-write danger-full-access)`，`workspace-write → (danger-full-access)`；不满足则 "sandbox escalation to \"<mode>\" is not strictly wider than this call's current \"<effective>\" mode"，**不问任何人**。
  2. 向批准缝提问，reason = "escalate sandbox to <mode>: <justification>"（这句进 Tape 的 `approval.asked`）。
  3. 四种结果：`allowed-once` → 只这一次以更宽模式执行，不记任何 Session 事件；`rejected` → "the user rejected escalating this operation to \"<mode>\""；`cancelled` → "approval for escalating to \"<mode>\" was cancelled"；`unavailable` → "sandbox escalation to \"<mode>\" requires approval, but no approval channel is available"。后三种什么都不执行。
- **批准策略 `never`**：在任何回答者之前直接 `rejected`。批处理（`noninteractive`）没有回答者 → `unavailable`。
- 提问通过现有聊天界面（`read-multiple-choice`），选项只有"允许一次 / 拒绝"；dsh 没有 `allow_always`，superchat 也去掉现在的 "a = 本会话一直允许"。

### 模型看到什么（照抄 dsh 的三处）

1. **系统提示不写当前模式。** dsh 试过在系统提示里写 "run under read-only sandbox"，模型 12 轮里 5 轮零工具调用、干脆不尝试，于是撤回（07-06 note § Alternatives）。
2. **一条"当前运行时上下文"快照**，作为随请求发送的 user 消息，只在文本变化时追加新的、声明取代旧快照的一条（dsh `RuntimeContextProjection`）。前缀 "Current runtime context. This snapshot supersedes earlier runtime-context snapshots." 之后两段，按模式与策略选用：
   - read-only: "Current DSH file policy: read-only. Any available operation enforced by the DSH file sandbox cannot modify files in the standing mode. Do not refuse a required modification from this policy alone: try an available tool normally and follow any denial and escalation guidance it returns."
   - workspace-write: "Current DSH file policy: workspace-write. Any available operation enforced by the DSH file sandbox may modify files under the session workspace: \"<root>\". Some platform temporary areas may also be writable."
   - danger-full-access: "Current DSH file policy: danger-full-access. The DSH file sandbox does not restrict file modifications by available operations."
   - approval never: "Approval prompts are disabled in this session: actions that require approval are rejected automatically — do not request sandbox escalation (do not set `sandbox_permissions`)."
   - approval ask: "Approval policy: ask. Operations that require approval may ask through the configured answerers; without an available answerer, the request fails closed."
   （superchat 把 "DSH file policy" 换成 "Superchat file policy"，其余不动。）
3. **工具描述**里的教学段落（dsh `bashDescription` 尾段）："Attempting a command the sandbox may deny is safe and expected: run it and read the marker rather than assuming the denial. When a command is denied and a wider mode would let it succeed, escalate immediately in the same turn — the one sanctioned exception to a denial: retry the exact same command once with `sandbox_permissions` (the narrowest wider mode that suffices) plus a one-sentence `justification`. Do not detour through chat to ask permission first — the approval prompt raised by that retry is how the user consents. If the session states approval prompts are disabled, there is no exception: a denial is final — do not set `sandbox_permissions`. Never escalate speculatively: ground the request in a real denial — normally the one this command just hit; escalating up front is fine only when this session already denied the same access. A rejected escalation is final for that command — stop and explain, never work around it — but it does not forbid attempting or escalating other commands later."

### 事件与档位（照抄 dsh 的名字）

| Tape 事件 | 载荷 | 何时 | 可见性 |
|---|---|---|---|
| `sandbox.mode` | `(:mode read-only\|workspace-write\|danger-full-access)` | 切换模式时 | 只进日志 |
| `approval.policy` | `(:policy ask\|never)` | 切换策略时 | 只进日志 |
| `permission.level` | `(:level NAME)` | 用户选档位且与当前不同时 | 只进日志（dsh `permission/preset`） |
| `approval.asked` / `approval.decided` | `(:id :tool :call-id :reason)` / `(:id :outcome)` | 每次向人提问，成对、必须在 Run 内 | 只进日志；模型看到的是工具结果 |

档位表（dsh 默认表原样）：`workspace-write` = `workspace-write + ask`，描述 "Write inside the workspace and permitted temporary directories; wider retries require approval."；`danger-full-access` = `danger-full-access + never`，描述 "Full file access without approval prompts."。两个旋钮被手动改到不匹配任何档位时显示为 `custom`（只显示，不能选）。命令 `/permissions [NAME]`：无参数列出当前档位与可选项，有参数切换；切换先记 `permission.level`，再只对**变化了的**旋钮写各自事件。子 agent 启动时继承父的显式模式，批准策略固定为 `never`（dsh 子 agent 的 delegation 规则）。

### 保留的薄缝

`superchat-tool-execute` 仍是每个模型工具调用的唯一入口，阶段改成 dsh 的三个 waterfall：`tools/pre-execute`（allow/deny/ask，没人管就是 allow）→ 批准（仅 `ask`）→ `tools/execute`（around）→ `tools/post-execute`（accept/replace/block）。保留：Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed` 及其载荷；执行身份与 Run 存活检查；未结执行 / 未决批准 → `audit-incomplete` 封印（M3a）；ADR-0005 的可信 wrapper 准入；ADR-0006 的拒绝交付。批准事件改名 `approval.asked` / `approval.decided`，只进日志不进模型上下文（dsh 同）。

### 删、留、改（按 2026-09-17 的全量盘点）

**删（dsh 无对应物）**
- Effect 词汇与解析：`superchat-authority-resolve-effects`、`-effect-argument`、`-register/unregister-effect-resolver`、`-effect-resolver-p`、`-visible-effect-resolver-names`、`superchat-tools.el` 里全部内置 resolver（198–317）与 `eval_elisp` 只读分类器、`superchat-register-tool` 的 `:effects` 必填、MCP 的 `superchat-mcp-server-effects` 与七种 kind 镜像。
- 决策机：`superchat-authority-decide`、`-decision-from-legacy`、`-compose-decisions`、5 个内置策略与 `--effective-decision` 的 15 条分支、`-register/unregister-policy`、`-normalize-policy`/`-policy-known-p`/`-policy-version-for`、Request 的 `:authority-policy`（Runtime 校验、Session 配置、`/authority` 命令、`authority.policy.selected` 事件、Tape View 投影、emacsclient 状态、compact/workflow/subagent 里的策略名）。
- 批准装饰：`superchat-authority-approval-hash`、`-register/unregister-approval-preview` 及三个内置 preview、`superchat-authority-approval-function` 的两级 ladder 与 `:approval-function` Request 键、`superchat-authority-confirm` 的 "a = 本会话允许" 与 `superchat-authority--session-allowlist`。
- 用户开关：`superchat-authority-unprompted-read-tools`、`-prompt-for-all-reads`、`-sensitive-buffers`、buffer-local `superchat-authority-mode`（explore/ask/execute，仓库里没有调用者）与 `superchat-authority-set-mode`、mode-line 的 `SChat[ask]`。
- Attendance：`:attendance` 键与 attended/unattended 基表——仓库里没有任何生产 producer 传 `unattended`；无人值守就是 `approval.policy = never`。
- agent-loop 贡献表 `superchat-authority--contributors`：`intercept`（dsh："Input rewriting is excluded because arguments are already logged and presented"）、`permission`、`confirmation`（`superchat-agent-destructive-tools`、`:confirm-destructive` guardrail）。`result-filter` 改为 `tools/post-execute` 监听。
- `superchat-select-tools.el` 里用 `effect-resolver-p` 定义"工具描述符"的写法，改为 Scope `tool` 层里有安装即可选。

**留（dsh 有对应物）**
- `superchat-tool-execute` 作为唯一缝，及其调用者：agent-loop wrapper 两处、subagent wrapper 两处；`superchat--agent-make-wrapper` 与 ADR-0005 的 `superchat-llm-tool-wrapper-required`。
- 执行身份与 evidence origin 检查（安装 `:live`、`:run-active-p`、`--require-current-evidence-origin`）。
- Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed` 及载荷（去掉 `:effects`、`:request-hash`、`:policy`、`:policy-version`、`:attendance` 四五个字段）；`approval.requested/decided` 改名 `approval.asked/decided`，载荷改为 dsh 的 `(:id :tool-name :call-id :reason)` / `(:id :outcome)`。
- Runtime 的未结工作标记 `superchat-runtime--note-authority-work`（effect/approval entered/settled）与 `audit-incomplete` 封印——改名去掉 "authority"，语义不变。
- `superchat-authority-transport-timer-owner`、`superchat--tool-execution-context`、`--make-human-input-operation`（ask_user 的可信输入路径）、`--normalize-tool-result`、ADR-0006 的拒绝交付、`tool-liveness` 故障记录。
- Invariant `tool-execution-passes-authority` 改名 `tool-execution-passes-the-pipeline`。

**改（翻译成 dsh 形状）**
- 三个 waterfall Event：`tools/pre-execute`（`(:kind allow|deny|ask :reason)`，默认 allow）、`tools/execute`（around）、`tools/post-execute`（`accept|block`）。现有 `tool-permission`、`tool-args-rewrite`、`tool-result-filter` 三个 Event 退役；`tool-before/after/failed` 保留。
- 新模块 `superchat-sandbox.el`：模式、可写根、两道护栏、标记文本、升级校验与 `approveEscalation` 编排。新模块 `superchat-approval.el`：`ask`/`never`、`approval/request` waterfall（聊天 View 注册人类回答者）、`approval.asked/decided`。档位与 `/permissions` 命令放在 `superchat.el`。
- **telega 插件**：现在注册两条策略层（`telega-digest`、`telega-triage`）、四个策略名、三组 effect resolver 与 preview，以及 Request 级 `:approval-function`。改为：每个 Request 注册自己的 `tools/pre-execute` 监听（按工具名 deny 自己不允许的调用）和 agent 作用域的 `approval/request` 回答者（自动放行已授权的读）。这是本轮里唯一需要重写逻辑而不是删除的地方。
- **MCP 工具**：dsh 的 MCP 桥没有任何批准门（`packages/mcp/mcp-client`），工具即普通工具。superchat 同样：默认 allow；是否保留"外部工具每次询问"的开关见待定第 7 项。
- **smoke case U** 与 `smoke-architecture` R7 读取策略层与 resolver 名单——随删除改写；canonical smoke 的 38 份基线不含 `approval.*` 行，事件序列不受影响。
- M3 中 9 条按名字属于 Authority 的测试与 5 条 `m3a/seal` 测试：前者随 wrapper/pipeline 改名重写，后者语义保留（未结执行/未决批准仍封 `audit-incomplete`）；M5 的 3 条 discovery 测试（免提示名单）删除，改为"读工具不问、workspace 内写不问"的新断言。`m5-tool-feedback` 里 11 条绑定批准函数的测试改绑新的回答者。

### 与既有决策的冲突

[2026-09-12 蓝图提案](2026-09-12-restructure-on-deepseek-harness.md)写着 "Kept and moved rather than rewritten: Authority decide and audit"、"a contributed allow cannot relax an inherited deny"，以及 "Port dsh's seams as they are (filesystem, shell, subprocess, sandbox) — rejected: superchat's world is Emacs objects"。本记录取代前两句；第三句仍成立——这里的护栏拦的是 Emacs 的 buffer 和文件原语，不是移植 dsh 的 shell 沙箱。`CONTEXT.md` 的 Authority 词条（"只能收紧的有效策略"）随之改写为模式 + 批准策略。

## Open choices

| # | 待定 | 选项 | 建议 |
|---|---|---|---|
| 1 | 默认档位 | (a) `workspace-write + ask`（dsh 默认） (b) `read-only + ask` (c) `danger-full-access + never` | **(a)** |
| 2 | workspace 根从哪来 | (a) 现有 `superchat-tools-default-directory`（project root 或 Workspace binding），没有就用 Session 创建时的目录 (b) 严格照 dsh 用 Session 创建时的目录 | **(a)**，project root 是 Emacs 用户的自然单位；与 dsh 的偏离写明 |
| 3 | `eval_elisp` 还问不问 | (a) 不问，靠两道护栏（dsh 的 `run_code` 也不问） (b) 继续每次问 | **(a)**——这是用户嫌烦的主要来源；边界如上写明 |
| 4 | shell/eshell 在 confining 模式下 | (a) 每次 `ask`，直到做出操作系统沙箱（macOS `sandbox-exec`、Linux bwrap）为止 (b) 直接 deny (c) 现在就做 OS 沙箱 | **(a)**；OS 沙箱列为后续独立切片 |
| 5 | 无文件 buffer 在 `workspace-write` 下 | (a) 可写（内存草稿） (b) 不可写 | **(a)** |
| 6 | 名字 | (a) 三档和两档都用 dsh 原名 (b) 起中文/自己的名 | **(a)**，便于对照 |
| 7 | MCP 与第三方注册工具 | (a) 与 dsh 一致：普通工具，默认 allow，想要提示自己注册 pre-execute 监听 (b) 保留一个"外部工具每次询问"的开关 | **(a)**；(b) 是 dsh 没有的东西 |
| 8 | 分几步交付 | (a) 三片：①批准缝 + 策略/模式事件 + 档位命令 ②两道护栏 + 升级参数 + 标记 ③删 Authority、改 telega/MCP、重写测试与文档 (b) 一次交付 | **(a)**，每片都能单独跑全量回归 |

## Alternatives considered

**只删策略层、哈希、预览、名单，保留 Effect 与逐次批准。** 留下的仍是"每步都问"的体验和 resolver 维护成本；dsh 没有 Effect 概念，保留它就仍要为每个新工具写分类。用户要的是不再打理这台机器。

**只拦 `edit_buffer` 名字，或扫描 eval 文本。** 见昨天提案的否决理由：拦不住 `save-buffer`，文本匹配漏报误报。dsh 也否决了 "command-string heuristic preflight"。

**指名保护单个文件（昨天的写入范围契约）。** dsh 无此概念；它靠区域边界 + 人看着 justification 批准。指名保护需要额外的声明入口和 UI，收益是防一类模型错误；先按 dsh 做，真实任务再证明需不需要。

**在系统提示里说明当前模式。** dsh 实测导致模型不敢尝试，已撤回；采用其后续的策略上下文消息方案。

**同一次工具调用内自动重试升级。** dsh 否决：日志无法重建"一次调用两次执行"；重试必须是新调用。

**移植 dsh 的 OS 级 shell 沙箱一并做。** 这是真正的操作系统工作（Seatbelt profile、bwrap），与 buffer/文件护栏无关，单独立切片；在它落地前 shell 走 `ask`。

## Acceptance criteria

新建独立回归（不改 M3/M5 受保护集合的规则不变；M3 中 9 条按名字属于 Authority 的测试由 leader 重写）：

1. `read-only`：任何工具对任何 buffer 的修改和任何文件写入都返回标记，磁盘字节与 buffer 内容不变，Tape 有 `tool.requested` + `tool.failed`；读工具照常，不问。
2. `workspace-write`：workspace 内文件与无文件 buffer 的编辑和保存**不问就成功**；workspace 外（含 `..`、绝对路径、指向外部的符号链接）被拦；`temporary-file-directory` 可写。
3. 升级：被拦后带 `sandbox_permissions`+`justification` 重试，批准函数被恰好调用一次；`allowed-once` 后该调用以更宽模式执行且写入落地；`rejected`/`cancelled`/`unavailable` 各返回自己的文本、什么都不改；不严格加宽的请求不问人直接失败；缺一个参数即失败。
4. `never`：不弹提示，申请直接 `rejected`；批处理（`noninteractive`）下没有回答者 → `unavailable`。
5. 护栏只在 body 动态范围内存在：body 前后 `before-change-functions` 与 `file-name-handler-alist` 完全恢复，含 signal 和 quit；并发的另一个 Session 用自己的模式和根。
6. 有意绕过（局部置空 `file-name-handler-alist`）确实能写——作为**文档边界测试**存在。
7. 切换档位：`sandbox.mode` / `approval.policy` 事件进 Tape，重开 Session 后模式仍在；策略上下文消息只在变化时追加。
8. 真实任务：M5 的 T2（改 emacs-lisp-mode 快捷键并持久化）在 `workspace-write + ask` 下重跑一次：workspace 内的编辑不弹任何提示；写 `~/.emacs.d` 时恰好弹一次升级提示。模型是否随后完成任务是另一项证据。
9. 删除后：`(require 'superchat)` 通过；canonical smoke 的 Tape 事件序列除 `approval.*` 改名外不变；字节编译无新增警告；README 双语的权限章节改写为三档两档。

## Risks

- `before-change-functions` 全局钩子在工具 body 期间对每次修改多一次检查；`with-silent-modifications` 绑定 `inhibit-modification-hooks` 的合法代码会绕过它——但那类代码改的是文本属性，不是文本。
- `file-name-handler-alist` 处理器要覆盖的写类操作表若不完整会漏拦；按 Emacs 手册 "Magic File Names" 逐项核对并逐项测试。
- 默认改为 `workspace-write + ask` 后，workspace 内的编辑不再逐次询问——这是有意的行为变化，README 与 CHANGELOG 必须写明，升级说明给出回到"每步都问"的办法（`read-only` 档位）。
- M3 的 9 条 Authority 测试、smoke 中批准事件名、27 份提到 Authority 的文档要同步改；这是删除量大于新增量的一轮，但涉及 12 个生产文件。
