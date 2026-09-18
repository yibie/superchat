# Decision: 权限按 DeepSeek Harness 的方式重做——档位、目标检查、一次性升级

Status: proposed — 用户 2026-09-17 决定"(a) 换"（按 dsh 模型替换 Authority）并选定默认档位 `workspace-write`；第二稿撤回了首稿的两道全局护栏（下文"为什么没有全局护栏"），其余细节等用户确认后再动代码

## Problem

superchat 现在的 Authority（`superchat-authority.el`，2140 行）是一台决策机：每个工具先由 resolver 解析成 8 种 Effect 之一，再叠 5 个内置策略和任意策略层（只能收紧）、算批准哈希、查免提示读名单和敏感 buffer 名单，最后才决定 allow/ask/deny。默认体验是：除了 4 个读工具，**每一步都问用户**（编辑、求值、进程都先问）。用户 2026-09-17 的原话："我现在已经不想搞这个麻烦的 authority 了"。

DeepSeek Harness（`~/Documents/reference-code/deepseek-harness`，c389f96）没有这台机器。它的 Agent Note 明确否决了这条路："Generic ToolRuntime wrapping any tool — rejected: mechanically false for in-process tools; the declarative-effects rewrite is unjustified"（`.agents/notes/implemented/feature/2026-07-06-sandbox.md` § Alternatives）。按用户 2026-09-15 定下的规矩——dsh 没有对应结构的就删——Effect、策略层、批准哈希、免提示名单都该删。

替代方案必须比 Authority 更少特例。dsh 的办法是：**一个档位开关 + 由真正干活的执行方自己检查目标 + 被拦后一次性申请**。它保护的是区域（workspace 之外），不是指名的文件；本记录不假装能做到指名保护，[写入范围契约提案](../rejected/2026-09-17-emacs-write-scope-contract.md)据此作废。

## Proposal

### 一、三个档位，含义只写能兑现的

每个 Session 一个档位（dsh 叫 permission preset；superchat 里 "Preset" 已另有所指，故称**权限档位**）。档位同时决定两个旋钮：`sandbox.mode`（`read-only` / `workspace-write` / `danger-full-access`）与 `approval.policy`（`ask` / `never`），名字照抄 dsh。

| 档位 | 明确目标的工具（`edit_buffer`、`append_to_buffer`、`replace_buffer`，新增 `save_buffer`、`write_file`、`rename_file`） | 完整 Emacs 控制（`eval_elisp`、`shell-command`、`eshell-command`） | MCP / 第三方注册工具 |
|---|---|---|---|
| `read-only` | 拒绝一切写；读工具照常 | 拒绝 | 按接入时声明的范围，未声明则拒绝 |
| **`workspace-write`（默认，用户 2026-09-17 选定）** | workspace 内直接做，不问；workspace 外拒绝并可一次性升级 | **每次问**（`ask`）；`never` 策略下拒绝 | 接入时声明为可信的直接做，否则每次问 |
| `danger-full-access` | 直接做 | 不问 | 不问 |

默认表照抄 dsh：`workspace-write` = `workspace-write + ask`，描述 "Write inside the workspace and permitted temporary directories; wider retries require approval."；`danger-full-access` = `danger-full-access + never`，描述 "Full file access without approval prompts."。两个旋钮被手动改到不匹配任何档位时显示为 `custom`（只显示，不可选）。`read-only` 作为第三档位提供"只看不动"。

`eval_elisp` 与 shell 归同一类：Lisp 本身就能 `call-process`，只拦名叫 shell 的工具不成规则。这一类叫**完整 Emacs 控制**——它能改变量、按键、hook、buffer，也能启动外部进程；契约不承诺任何目录边界能约束它，用户在 `workspace-write` 下逐次批准，或明确切到 `danger-full-access` 后不再被问。

命令 `/permissions [NAME]`：无参数列出当前档位与可选项，有参数切换。切换先记 `permission.level`（与当前不同时），再只对**变化了的**旋钮写 `sandbox.mode` / `approval.policy`（dsh `PermissionPresetService.set` 的写穿顺序）。子 agent 启动时继承父的显式模式，批准策略固定为 `never`（dsh 子 agent 的 delegation 规则）。

### 二、明确目标的工具，在自己执行的地方检查目标

没有全局护栏；每个知道目标的工具在改动之前解析真实目标并判断（dsh："enforcement point: provider, not intent gate"）。共用四个检查函数，不再有 Effect 表：

| 操作 | 执行处的检查 |
|---|---|
| 编辑指定 buffer（`edit_buffer` 等） | 取到实际 buffer；indirect buffer 追到基 buffer；有文件的按文件路径判定；无文件的按归属分三类——工具自己的临时 buffer（放行）、用户内容（`workspace-write` 下视为 workspace 内）、内部运行状态（Tape/聊天 View 等，拒绝） |
| 保存指定 buffer / 写指定文件（新增 `save_buffer`、`write_file`） | 写入前把目标路径规范化（`file-truename` 到最深的已存在祖先，与 dsh 的 canonicalize-then-contain 相同）并判包含；结果报告实际保存位置与字节数 |
| 重命名 / 删除文件（新增 `rename_file`、`delete_file`，可选） | 源与目标分别检查，两处都会变 |
| 读（`read_buffer`、`list_buffers`、`describe-*`、`apropos`） | 不检查；每个档位都允许读 |

可写根 = workspace 根 + `temporary-file-directory`（dsh：`writableRoots` = workspace root + `/tmp` + `os.tmpdir()`，只在 `workspace-write` 下非空）。workspace 根不新造概念：优先用已有的 Session Workspace 绑定（`/workspace DIR`，`superchat--workspace-binding-for-directory` 已做 `file-truename`，Request 里就是 `:workspace-root`），其次是 View 的 `superchat-tools-default-directory`（来源 buffer 的 project root），最后退到 Session 创建时的 `default-directory`。

**先补保存工具。** 现在保存全靠 `eval_elisp` 里的 `save-buffer`；不给模型明确的保存入口，普通任务会持续进批准流程。`save_buffer` / `write_file` 与 `edit_buffer` 一起构成 workspace 内零提示的编辑—保存路径。

### 三、被拦之后：一次性升级，文本照抄 dsh

以下字符串来自 `packages/sandbox/sandbox/src/escalation.ts`、`packages/fs/tool-fs/src/sandbox.ts`（c389f96），superchat 原样采用。只适用于**明确目标的工具**；完整 Emacs 控制类工具是先问后跑，没有"被拦"这一步。

- **只在 confining 模式下**（`read-only` / `workspace-write`）才给明确目标的写工具加两个可选参数（dsh："advertise the escalation fields unconditionally — rejected"）：
  - `sandbox_permissions`：枚举 `workspace-write` / `danger-full-access`；描述 "The wider sandbox mode this file operation needs. Only valid as a one-shot retry of an operation the sandbox just denied; requires justification and user approval."
  - `justification`："Required with sandbox_permissions: one sentence for the user explaining why this exact file operation needs the wider access."
  - 配对校验失败文本："invalid escalation: sandbox_permissions requires a justification" / "invalid escalation: justification is only valid together with sandbox_permissions" / "invalid justification: expected a non-empty sentence"。
- **被拒的调用**什么都没改（检查在改动之前），返回两行标记，Run 继续：
  ```text
  [sandbox: file access denied under <mode> mode]
  [sandbox: escalation available — retry this exact operation once with sandbox_permissions (the narrowest wider mode that suffices) + justification; the approval prompt asks the user]
  ```
  第二行只在 confining 模式下附加。检查函数内部的 signal 文本："cannot write \"<path>\": file access denied under <mode> mode"。
- **带参数的重试是一次新的、独立记录的工具调用**（dsh："Auto-retry inside the same tool call — rejected"）。执行前按顺序：
  1. 严格加宽：`WIDER_MODES` = `read-only → (workspace-write danger-full-access)`，`workspace-write → (danger-full-access)`；不满足则 "sandbox escalation to \"<mode>\" is not strictly wider than this call's current \"<effective>\" mode"，不问任何人。
  2. 向批准缝提问，reason = "escalate sandbox to <mode>: <justification>"（进 Tape 的 `approval.asked`）。
  3. 四种结果：`allowed-once` → 只这一次以更宽模式执行；`rejected` → "the user rejected escalating this operation to \"<mode>\""；`cancelled` → "approval for escalating to \"<mode>\" was cancelled"；`unavailable` → "sandbox escalation to \"<mode>\" requires approval, but no approval channel is available"。后三种什么都不执行。
- **不要求"原样重跑任意 Lisp"。** 一段 Lisp 前半段改了 workspace 内的文件、后半段想写外面的文件——这种部分效果的问题在本设计里不出现：`eval_elisp` 在 `workspace-write` 下先问后跑，明确目标的工具在改之前检查。

### 四、批准缝：`ask` / `never`，一次一答

`superchat-approval.el`（新，薄）：

- `approval.policy` 每 Session 一个值，`ask`（默认）或 `never`；`never` 在任何回答者之前直接 `rejected`（dsh：在 waterfall 之前强制，后注册的回答者也绕不过）。
- 提问走 `approval/request` waterfall：聊天 View 注册人类回答者（`read-multiple-choice`，选项只有"允许一次 / 拒绝"；dsh 没有 `allow_always`，superchat 去掉现在的 "a = 本会话一直允许"和 session allowlist）；批处理 `noninteractive` 没有回答者 → `unavailable` → 拒绝。
- 结果封闭：`allowed-once` / `rejected` / `cancelled` / `unavailable`；只有 `allowed-once` 放行。
- 批准绑定**这一次调用的参数快照**：等待期间调用不能换成另一组参数。用快照实现，不重建哈希决策机。
- 每次提问在 Tape 上成对记 `approval.asked` `(:id :tool-name :call-id :reason)` / `approval.decided` `(:id :outcome)`，必须在 Run 内；只进日志，不进模型上下文（dsh 同）。

工具执行入口 `superchat-tool-execute` 保留为唯一缝，阶段改成 dsh 的三个 waterfall：`tools/pre-execute`（`(:kind allow|deny|ask :reason)`，没人管就是 allow；dsh 的契约允许直接返回 `ask`，不要求先失败一次）→ 批准（仅 `ask`）→ `tools/execute`（around）→ `tools/post-execute`（`accept|block`）。档位对完整 Emacs 控制类工具和 MCP 工具的 `ask`/`deny` 就是一个内置的 pre-execute 监听者。保留：调用身份与 Run 存活检查、evidence origin 检查、Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed`、未结执行 / 未决批准 → `audit-incomplete` 封印（M3a）、ADR-0005 的可信 wrapper 准入、ADR-0006 的拒绝交付。

### 五、结果证据：从旧写入范围契约保留的一样东西

每个修改类工具的结果正文必须回答：**改了哪个对象**（buffer 名、文件路径）、**到了哪一层**（只在内存 / 已写盘，含字节数 / 已重新加载）、**验证了什么**（读回的有效值、在哪个 buffer）。这不是权限机制，是让"模型说完成了其实没完成"可核对的证据，[预算收尾指令](../implemented/2026-09-17-model-attempt-progress-and-budget-finalization.md)已要求最终答复按"已做／未做／未验证"分开陈述，工具结果按同一格式供料。

### 六、模型看到什么（照抄 dsh 的三处）

1. **系统提示不写当前档位。** dsh 试过在系统提示里写 "run under read-only sandbox"，模型 12 轮里 5 轮零工具调用、干脆不尝试，于是撤回（07-06 note § Alternatives）。
2. **一条"当前运行时上下文"快照**，作为随请求发送的 user 消息，只在文本变化时追加新的、声明取代旧快照的一条（dsh `RuntimeContextProjection`）。前缀 "Current runtime context. This snapshot supersedes earlier runtime-context snapshots." 之后按档位选用（"DSH" 换成 "Superchat"，其余不动）：
   - read-only: "Current Superchat file policy: read-only. Any available operation enforced by the Superchat file sandbox cannot modify files in the standing mode. Do not refuse a required modification from this policy alone: try an available tool normally and follow any denial and escalation guidance it returns."
   - workspace-write: "Current Superchat file policy: workspace-write. Any available operation enforced by the Superchat file sandbox may modify files under the session workspace: \"<root>\". Some platform temporary areas may also be writable."
   - danger-full-access: "Current Superchat file policy: danger-full-access. The Superchat file sandbox does not restrict file modifications by available operations."
   - approval never: "Approval prompts are disabled in this session: actions that require approval are rejected automatically — do not request sandbox escalation (do not set `sandbox_permissions`)."
   - approval ask: "Approval policy: ask. Operations that require approval may ask through the configured answerers; without an available answerer, the request fails closed."
3. **工具描述**：明确目标的写工具带 dsh 的教学段（"Attempting a command the sandbox may deny is safe and expected … A rejected escalation is final for that command — stop and explain, never work around it …"，原文见 `packages/shell/tool-bash/src/index.ts` `bashDescription`）；`eval_elisp` 的描述改写为"完整 Emacs 控制，在 workspace-write 下每次调用需要用户批准，优先使用 edit_buffer / save_buffer 完成 workspace 内的编辑与保存"。

### 七、为什么没有全局护栏

首稿提议用全局 `before-change-functions` 钩子和临时 `file-name-handler-alist` 处理器把任意 Lisp 也约束在 workspace 内。独立审阅（DevSpace，2026-09-17）指出并由 leader 在裸 Emacs 32 复现了三条：钩子函数一 signal，Emacs 就把 `before-change-functions` 清空，第二次 `insert` 成功；从另一个 buffer 的 `after-change-functions` 里改受保护 buffer 时修改钩子被抑制，护栏不运行；indirect buffer 没有文件名却直写基 buffer。另外 `file-name-handler-alist` 有自己的处理器选择规则，工具登记的 timer 可以在动态绑定退出后才运行，工具自己的临时 buffer 也会被误伤。继续补齐要处理处理器组合、回调归属、间接 buffer、局部钩子——维护成本会超过被删掉的 Authority。因此约束只在知道目标的工具执行处实施；任意 Lisp 用批准而不是护栏管。

### 八、删、留、改（按 2026-09-17 的全量盘点）

**删（dsh 无对应物）**
- Effect 词汇与解析：`superchat-authority-resolve-effects`、`-effect-argument`、`-register/unregister-effect-resolver`、`-effect-resolver-p`、`-visible-effect-resolver-names`、`superchat-tools.el` 里全部内置 resolver（198–317）与 `eval_elisp` 只读分类器、`superchat-register-tool` 的 `:effects` 必填、MCP 的 `superchat-mcp-server-effects` 与七种 kind 镜像。
- 决策机：`superchat-authority-decide`、`-decision-from-legacy`、`-compose-decisions`、5 个内置策略与 `--effective-decision` 的 15 条分支、`-register/unregister-policy`、`-normalize-policy`/`-policy-known-p`/`-policy-version-for`、Request 的 `:authority-policy`（Runtime 校验、Session 配置、`/authority` 命令、`authority.policy.selected` 事件、Tape View 投影、emacsclient 状态、compact/workflow/subagent 里的策略名）。
- 批准装饰：`superchat-authority-approval-hash`、`-register/unregister-approval-preview` 及三个内置 preview、两级 approval ladder 与 `:approval-function` Request 键、`superchat-authority-confirm` 的 "a = 本会话允许" 与 `superchat-authority--session-allowlist`。
- 用户开关：`superchat-authority-unprompted-read-tools`、`-prompt-for-all-reads`、`-sensitive-buffers`、buffer-local `superchat-authority-mode`（explore/ask/execute，仓库里没有调用者）与 `superchat-authority-set-mode`、mode-line 的 `SChat[ask]`。
- Attendance：`:attendance` 键与 attended/unattended 基表——仓库里没有生产 producer 传 `unattended`；无人值守就是 `approval.policy = never`。
- agent-loop 贡献表 `superchat-authority--contributors`：`intercept`（dsh："Input rewriting is excluded because arguments are already logged and presented"）、`permission`、`confirmation`（`superchat-agent-destructive-tools`、`:confirm-destructive` guardrail）。`result-filter` 改为 `tools/post-execute` 监听。
- `superchat-select-tools.el` 里用 `effect-resolver-p` 定义"工具描述符"的写法，改为 Scope `tool` 层里有安装即可选。

**留（dsh 有对应物）**
- `superchat-tool-execute` 作为唯一缝，及其调用者：agent-loop wrapper 两处、subagent wrapper 两处；`superchat--agent-make-wrapper` 与 ADR-0005 的 `superchat-llm-tool-wrapper-required`。
- 执行身份与 evidence origin 检查（安装 `:live`、`:run-active-p`、`--require-current-evidence-origin`）。
- Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed` 及载荷（去掉 `:effects`、`:request-hash`、`:policy`、`:policy-version`、`:attendance`）；`approval.requested/decided` 改名 `approval.asked/decided`，载荷改为 dsh 形状。
- Runtime 的未结工作标记 `superchat-runtime--note-authority-work`（entered/settled）与 `audit-incomplete` 封印——改名去掉 "authority"，语义不变。
- `superchat-authority-transport-timer-owner`、`superchat--tool-execution-context`、`--make-human-input-operation`（ask_user 的可信输入路径）、`--normalize-tool-result`、ADR-0006 的拒绝交付、`tool-liveness` 故障记录。
- Invariant `tool-execution-passes-authority` 改名 `tool-execution-passes-the-pipeline`。

**改（翻译成 dsh 形状）**
- 三个 waterfall Event：`tools/pre-execute`、`tools/execute`、`tools/post-execute`；现有 `tool-permission`、`tool-args-rewrite`、`tool-result-filter` 退役；`tool-before/after/failed` 保留。
- 新模块 `superchat-sandbox.el`：档位与两个旋钮、可写根、四个目标检查函数、标记文本、升级校验与 `approveEscalation` 编排、运行时上下文快照。新模块 `superchat-approval.el`：`ask`/`never`、`approval/request` waterfall、`approval.asked/decided`。档位与 `/permissions` 命令放在 `superchat.el`。新工具 `save_buffer`、`write_file`（`rename_file`、`delete_file` 可选）放在 `superchat-tools.el`。
- **telega 插件**：现在注册两条策略层（`telega-digest`、`telega-triage`）、四个策略名、三组 effect resolver 与 preview，以及 Request 级 `:approval-function`。改为：每个 Request 注册自己的 `tools/pre-execute` 监听（按工具名 deny 自己不允许的调用）和 agent 作用域的 `approval/request` 回答者（自动放行已授权的读）。这是唯一要重写逻辑而不是删除的地方。
- **MCP 工具**：dsh 的 MCP 桥没有批准门（`packages/mcp/mcp-client`）。superchat 在接入时按服务器声明可信与否（一个布尔，替代 `superchat-mcp-server-effects`）：可信的直接做，否则在 `workspace-write` 下每次问；本地 workspace 路径约束不了远端工具，契约不假装能。
- **smoke case U** 与 `smoke-architecture` R7 读取策略层与 resolver 名单——随删除改写；canonical smoke 的 38 份基线不含 `approval.*` 行，事件序列不受影响。
- M3 中 9 条按名字属于 Authority 的测试与 5 条 `m3a/seal` 测试：前者随 wrapper/pipeline 改名重写，后者语义保留；M5 的 3 条 discovery 测试（免提示名单）删除，改为"读工具不问、workspace 内写不问"的新断言；`m5-tool-feedback` 里 11 条绑定批准函数的测试改绑新的回答者。

### 九、与既有决策的冲突

[2026-09-12 蓝图提案](2026-09-12-restructure-on-deepseek-harness.md)写着 "Kept and moved rather than rewritten: Authority decide and audit"、"a contributed allow cannot relax an inherited deny"，以及 "Port dsh's seams as they are (filesystem, shell, subprocess, sandbox) — rejected: superchat's world is Emacs objects"。本记录取代前两句；第三句仍成立——目标检查针对的是 Emacs 的 buffer 和文件，不是移植 dsh 的 OS 沙箱。`CONTEXT.md` 的 Authority 词条（"只能收紧的有效策略"）随之改写为档位 + 批准策略。

## Open choices

已定：默认档位 `workspace-write`（用户 2026-09-17）；`eval_elisp` 与 shell 同类，在 `workspace-write` 下逐次问；不做全局护栏；名字照抄 dsh。仍待用户一句话确认（可回"都按建议"）：

| # | 待定 | 选项 | 建议 |
|---|---|---|---|
| 1 | workspace 根从哪来 | (a) 先用 `/workspace DIR` 绑定，其次来源 buffer 的 project root，最后 Session 启动目录 (b) 严格照 dsh 只用启动目录 | **(a)** |
| 2 | `read-only` 下的完整控制类工具 | (a) 拒绝（"只看不动"） (b) 问 | **(a)** |
| 3 | MCP 接入时的可信声明 | (a) 每个服务器一个布尔，默认不可信 → `workspace-write` 下每次问 (b) 与 dsh 完全一致：MCP 工具不问 | **(a)**；远端工具本地路径管不了，问一声是诚实的替代 |
| 4 | 交付切片 | (a) 四片：①批准缝 + 档位 + 事件 + `/permissions` ②`save_buffer`/`write_file` + 明确目标工具的检查 + 升级参数 ③完整控制类与 MCP 按档位问/拒 + 运行时上下文快照 ④删 Authority、改 telega、重写测试与文档 (b) 一次交付 | **(a)**，每片都能单独跑全量回归 |

## Alternatives considered

**只删策略层、哈希、预览、名单，保留 Effect 与逐次批准。** 留下的仍是"每步都问"的体验和 resolver 维护成本；dsh 没有 Effect 概念，保留它就仍要为每个新工具写分类。

**全局修改钩子 + 文件处理器把任意 Lisp 关进 workspace（首稿）。** 见第七节：三条缺陷已复现，且会误伤工具自己的临时 buffer；补齐它的维护成本超过被删的 Authority。

**只拦 `edit_buffer` 名字，或扫描 eval 文本。** 拦不住 `save-buffer`，文本匹配漏报误报；dsh 也否决了 "command-string heuristic preflight"。

**指名保护单个文件（写入范围契约）。** dsh 无此概念；它靠区域边界 + 人看着 justification 批准。指名保护需要额外的声明入口和 UI，先按 dsh 做，真实任务再证明需不需要。

**在系统提示里说明当前档位。** dsh 实测导致模型不敢尝试，已撤回；采用其后续的运行时上下文快照。

**同一次工具调用内自动重试升级。** dsh 否决：日志无法重建"一次调用两次执行"；重试必须是新调用。

**移植 dsh 的 OS 级 shell 沙箱。** 那是操作系统工作（Seatbelt profile、bwrap），做成后 shell 在 `workspace-write` 下可以像 dsh 一样不问；在此之前 shell 与 eval 同类逐次问。单独立案。

## Acceptance criteria

新建独立回归（M3/M5 受保护集合的规则不变；M3 中 9 条按名字属于 Authority 的测试由 leader 重写）：

1. `workspace-write`：workspace 内的 `edit_buffer` + `save_buffer` 从头到尾**不弹任何提示**，文件字节落地；workspace 外（含 `..`、绝对路径、指向外部的符号链接、indirect buffer 的基 buffer 在外）被拒，磁盘与 buffer 不变，Tape 有 `tool.requested` + `tool.failed`；`temporary-file-directory` 可写。
2. `workspace-write`：每次 `eval_elisp` / `shell-command` 都恰好问一次；拒绝后什么都没跑；`never` 下直接拒绝、不问。
3. 升级：被拒后带 `sandbox_permissions`+`justification` 重试，批准函数恰好调用一次；`allowed-once` 后该调用以更宽模式执行且写入落地；`rejected`/`cancelled`/`unavailable` 各返回自己的文本、什么都不改；不严格加宽的请求不问人直接失败；缺一个参数即失败。
4. `read-only`：所有写工具与完整控制类工具被拒，读工具照常。
5. `danger-full-access`：没有任何提示。
6. 批准绑定参数快照：等待期间替换参数的调用不被放行。
7. 切换档位：`permission.level` / `sandbox.mode` / `approval.policy` 事件进 Tape，重开 Session 后档位仍在；运行时上下文快照只在变化时追加。
8. 结果证据：每个修改类工具的结果正文含对象、层级、验证三项。
9. 真实任务：M5 的 T2（改 emacs-lisp-mode 快捷键并持久化）在默认档位重跑一次：workspace 内的编辑与保存零提示；`eval_elisp` 与写 `~/.emacs.d` 各问一次。模型是否随后完成任务是另一项证据。
10. 删除后：`(require 'superchat)` 通过；canonical smoke 事件序列除 `approval.*` 改名外不变；字节编译无新增警告；README 双语的权限章节改写为三档。

## Risks

- 默认改为 `workspace-write` 后，workspace 内的编辑与保存不再逐次询问——这是有意的行为变化，README 与 CHANGELOG 必须写明，并给出回到"只看不动"的办法（`read-only`）。
- 无文件 buffer 的三分类（工具临时 / 用户内容 / 内部状态）要有一个明确的判定函数并逐类测试；漏判"内部状态"会让模型改到聊天 View 或 Tape 缓冲。
- `eval_elisp` 在默认档位逐次问，配置类任务每次 1–2 个提示；嫌烦的用户要知道 `/permissions danger-full-access` 的存在。
- M3 的 9 条 Authority 测试、smoke 中批准事件名、27 份提到 Authority 的文档要同步改；这是删除量大于新增量的一轮，但涉及 12 个生产文件和 telega 插件的逻辑重写。
