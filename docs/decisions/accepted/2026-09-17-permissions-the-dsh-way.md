# Decision: 权限按 DeepSeek Harness 的方式重做——档位、目标检查、一次性升级

Status: accepted — 用户 2026-09-17 决定"(a) 换"（按 dsh 模型替换 Authority）、"默认选 workspace-write"，2026-09-18 对剩余两项答"都按建议"并提交第三稿（`aa8edf2d`）；本稿并入两位 Pi 读手通读后的六处事实修正。实现状态逐片记在[路线图](../../roadmap.md)

## Problem

superchat 现在的 Authority（`superchat-authority.el`，2140 行）是一台决策机：每个工具先由 resolver 解析成 8 种 Effect 之一，再叠 5 个内置策略和任意策略层（只能收紧）、算批准哈希、查免提示读名单和敏感 buffer 名单，最后才决定 allow/ask/deny。默认体验是：除了 4 个读工具，**每一步都问用户**（编辑、求值、进程都先问）。用户 2026-09-17 的原话："我现在已经不想搞这个麻烦的 authority 了"。

DeepSeek Harness（`~/Documents/reference-code/deepseek-harness`，c389f96）没有这台机器。它的 Agent Note 明确否决了这条路："Generic ToolRuntime wrapping any tool — rejected: mechanically false for in-process tools; the declarative-effects rewrite is unjustified"（`.agents/notes/implemented/feature/2026-07-06-sandbox.md` § Alternatives）。按用户 2026-09-15 定下的规矩——dsh 没有对应结构的就删——Effect、策略层、批准哈希、免提示名单都该删。

替代方案必须比 Authority 更少特例。dsh 的办法是：**一个档位开关 + 由真正干活的执行方自己检查目标 + 被拦后一次性申请**。它保护的是区域（workspace 之外），不是指名的文件；本记录不假装能做到指名保护，[写入范围契约提案](../rejected/2026-09-17-emacs-write-scope-contract.md)据此作废。

## Decision

用户的决定（原话）：2026-09-17 "我现在已经不想搞这个麻烦的 authority 了"、对"要不要按 dsh 模型替换 Authority"答 "a"、"默认选 workspace-write"；2026-09-18 "契约方面，剩下两项都按建议：workspace 根采用 /workspace 绑定 → 来源 project root → Session 启动目录；四片交付，前三片不接管正式 Session，第四片统一启用并删除临时开关。"

### 一、三个档位，含义只写能兑现的

每个 Session 一个档位（dsh 叫 permission preset；superchat 里 "Preset" 已另有所指，故称**权限档位**）。档位同时决定两个旋钮：`sandbox.mode`（`read-only` / `workspace-write` / `danger-full-access`）与 `approval.policy`（`ask` / `never`），名字照抄 dsh。

| 档位 | 读类工具 | 明确目标的写工具（`edit_buffer`、`append_to_buffer`、`replace_buffer`、`workspace_write`，新增 `save_buffer`、`write_file`；`rename_file`、`delete_file` 可选） | 完整 Emacs 控制（`eval_elisp`、`shell-command`、`eshell-command`、`define_tool` 及其生成的工具） | 运行时状态（`team_task`/`team_message` 的写动作、`render_ui`、`workspace_publish`、`delegate_to_subagent*`、`ask_user`） | MCP / 第三方注册工具 |
|---|---|---|---|---|---|
| `read-only` = `read-only + ask` | 允许 | 拒绝；可按第三节一次性升级到 `workspace-write` 或 `danger-full-access` | 拒绝，不可升级 | 允许 | 全部拒绝 |
| **`workspace-write` = `workspace-write + ask`（默认，用户 2026-09-17 选定）** | 允许 | workspace 内直接做，不问；workspace 外拒绝，可一次性升级到 `danger-full-access` | **每次问**（`ask`） | 允许 | 接入时声明为可信的直接做，否则每次问 |
| `danger-full-access` = `danger-full-access + never` | 允许 | 直接做 | 不问 | 允许 | 不问 |

三个档位都是 dsh 的旋钮组合：`workspace-write`（描述 "Write inside the workspace and permitted temporary directories; wider retries require approval."）、`danger-full-access`（"Full file access without approval prompts."）照抄 dsh 默认表；`read-only` 是 superchat 补的第三档，含义是**默认只读，写入需一次性升级**，不是"绝对禁止写"——想要绝对禁止就把它配成 `read-only + never`（`custom`），升级申请会以 `rejected` 结束。两个旋钮被手动改到不匹配任何档位时显示为 `custom`（只显示，不可选）。

"读类工具"指 `list_buffers`、`read_buffer`、`apropos`、`describe-function`、`describe-variable`、`workspace_read`、`workspace_info`、`sql`、`memory_search`、`tool_history`、`file_history`、`recent_errors`、`lsp-references`、`lsp-code-actions`、`lsp-hover`，以及 `team_task`/`team_message` 的 `list`/`read` 动作：每个档位都允许、不问（dsh："every mode permits reading"）。"运行时状态"指助手自己的记录与界面——团队任务和消息写进 Tape、`render_ui` 画进聊天 View、委派子 agent、向用户提问——它们不碰用户的文件和 buffer，三个档位都允许；子 agent 继承父的模式，批准策略固定为 `never`，所以 `workspace-write` 下的子 agent 调不了完整控制类工具（dsh 的 delegation 规则）。MCP 工具运行在远端，本地 workspace 路径管不了它，"可信"也表达不了"只读"，所以 `read-only` 下一律拒绝；可信声明只决定 `workspace-write` 下问不问。

`eval_elisp` 与 shell 归同一类：Lisp 本身就能 `call-process`，只拦名叫 shell 的工具不成规则。这一类叫**完整 Emacs 控制**——它能改变量、按键、hook、buffer，也能启动外部进程；契约不承诺任何目录边界能约束它，用户在 `workspace-write` 下逐次批准，或明确切到 `danger-full-access` 后不再被问。

命令 `/permissions [NAME]`：无参数列出当前档位与可选项，有参数切换。切换先记 `permission.level`（与当前不同时），再只对**变化了的**旋钮写 `sandbox.mode` / `approval.policy`（dsh `PermissionPresetService.set` 的写穿顺序）。子 agent 启动时继承父的显式模式，批准策略固定为 `never`（dsh 子 agent 的 delegation 规则）。

### 二、明确目标的工具，在自己执行的地方检查目标

没有全局护栏；每个知道目标的工具在改动之前解析真实目标并判断（dsh："enforcement point: provider, not intent gate"）。共用四个检查函数，不再有 Effect 表：

| 操作 | 执行处的检查 |
|---|---|
| 编辑指定 buffer（`edit_buffer` 等） | 取到实际 buffer；indirect buffer 追到基 buffer；有文件的按文件路径判定；无文件的分两类——普通 buffer 是草稿（`workspace-write` 下可写，含 `append_to_buffer` 新建的），Superchat 自己的运行状态（`superchat-mode` 的聊天 View、Session 拥有的 EditorWorkspace buffer、名字形如 `*superchat…*`／`*Superchat…*`／` *superchat…*` 的面板、预览与临时 buffer）**不在任何 workspace 之内**，与 workspace 外的文件同样处理：confining 模式下拒绝，可一次性升级到 `danger-full-access`。工具自己的临时 buffer 不经过这些工具，不需要单独一类。`edit_buffer` 只在真要替换时才检查，没找到文本、多处匹配、无变化都不触发检查也不触发批准 |
| 保存指定 buffer / 写指定文件（新增 `save_buffer`、`write_file`） | 写入前把目标路径规范化（`file-truename` 到最深的已存在祖先，与 dsh 的 canonicalize-then-contain 相同）并判包含；结果报告实际保存位置与字节数 |
| 重命名 / 删除文件（新增 `rename_file`、`delete_file`，可选） | 源与目标分别检查，两处都会变 |
| 读（`read_buffer`、`list_buffers`、`describe-*`、`apropos`） | 不检查；每个档位都允许读 |

可写根 = workspace 根 + `temporary-file-directory`（dsh：`writableRoots` = workspace root + `/tmp` + `os.tmpdir()`，只在 `workspace-write` 下非空）。workspace 根不新造概念：优先用已有的 Session Workspace 绑定（`/workspace DIR`，`superchat--workspace-binding-for-directory` 已做 `file-truename`，Request 里就是 `:workspace-root`），其次是 View 的 `superchat-tools-default-directory`（来源 buffer 的 project root），最后退到 Session 创建时的 `default-directory`。

远程路径（`file-remote-p`）一律视为 workspace 之外，不做 truename。路径判包含时根目录带尾部斜杠比较，名字只是以根目录名开头的兄弟目录不算在内。

**先补保存工具，并定清分工。** 现在保存全靠 `eval_elisp` 里的 `save-buffer`；不给模型明确的保存入口，普通任务会持续进批准流程。`save_buffer` 保存**已有文件的 buffer**；`write_file` 写**没有 live visiting buffer 的文件**。两者与 `edit_buffer` 一起构成 workspace 内零提示的编辑—保存路径。会影响用户数据的场景按下表定死：

| 场景 | 规则 |
|---|---|
| `save_buffer` 目标是 indirect 或 narrowed buffer | 保存基 buffer 当前访问的整个文件（widen 后的全文），结果报告基 buffer 名与文件路径 |
| `save_buffer` 目标没有文件 | 返回明确错误；不打开 minibuffer 询问文件名，不承担另存 |
| buffer 里已有用户未保存的修改，模型又改了一部分 | `save_buffer` 保存当前完整内容（含用户已有修改），结果里说明"保存前 buffer 已有未保存修改" |
| `write_file` 的目标文件已被某个 live buffer 访问 | 不写盘、不覆盖 buffer、不合并；返回正在访问它的 buffer 名，要求改走 `edit_buffer` + `save_buffer` |
| `write_file` 的目标不存在或无人访问 | 写入（父目录须已存在），结果报告路径与字节数 |

**目录检查的边界。** 目录检查只约束工具**直接**操作的 buffer 和文件。用户配置的 hook（如 `after-save-hook` 里的 Org tangle 会生成 workspace 外的文件）、文件处理器、mode 函数属于受信任的 Emacs 环境，契约不对它们的连带副作用提供目录隔离。

### 三、被拦之后：一次性升级，文本照抄 dsh

以下字符串来自 `packages/sandbox/sandbox/src/escalation.ts`、`packages/fs/tool-fs/src/sandbox.ts`（c389f96），superchat 原样采用。只适用于**明确目标的工具**；完整 Emacs 控制类工具是先问后跑，没有"被拦"这一步。

- 明确目标的写工具带两个可选参数。工具 schema 在 superchat 里是进程全局的（`superchat-llm-tools-list`；`llm-tool` 只有五个槽，没有按 Session 的参数表），所以参数在权限档位功能启用时**始终广告**，不随 Session 的模式变化；dsh 同样如此并接受这一限制（"the advertised target set is static while the effective mode is per-session — harmless by construction: the strict-wider check at execution … is the safety boundary"）。安全边界是下面第 1 步的严格加宽检查：
  - `sandbox_permissions`：枚举 `workspace-write` / `danger-full-access`；描述 "The wider sandbox mode this file operation needs. Only valid as a one-shot retry of an operation the sandbox just denied; requires justification and user approval."
  - `justification`："Required with sandbox_permissions: one sentence for the user explaining why this exact file operation needs the wider access."
  - 配对校验失败文本："invalid escalation: sandbox_permissions requires a justification" / "invalid escalation: justification is only valid together with sandbox_permissions" / "invalid justification: expected a non-empty sentence"。
- **被权限拒绝的调用**什么都没改（检查在实际编辑或保存之前），返回两行标记，Run 继续。这句只对权限拒绝成立：工具进入实际操作后的普通执行错误不承诺回滚，也不包装成权限拒绝：
  ```text
  [sandbox: file access denied under <mode> mode]
  [sandbox: escalation available — retry this exact operation once with sandbox_permissions (the narrowest wider mode that suffices) + justification; the approval prompt asks the user]
  ```
  第二行只在该调用的有效模式是 confining 时附加。检查函数内部的 signal 文本："cannot write \"<path>\": file access denied under <mode> mode"。
- **带参数的重试是一次新的、独立记录的工具调用**（dsh："Auto-retry inside the same tool call — rejected"）。执行前按顺序：
  1. 严格加宽：`WIDER_MODES` = `read-only → (workspace-write danger-full-access)`，`workspace-write → (danger-full-access)`；不满足则 "sandbox escalation to \"<mode>\" is not strictly wider than this call's current \"<effective>\" mode"，不问任何人。
  2. 向批准缝提问，reason = "escalate sandbox to <mode>: <justification>"（进 Tape 的 `approval.requested`）。
  3. 四种结果：`allowed-once` → 只这一次以更宽模式执行；`rejected` → "the user rejected escalating this operation to \"<mode>\""；`cancelled` → "approval for escalating to \"<mode>\" was cancelled"；`unavailable` → "sandbox escalation to \"<mode>\" requires approval, but no approval channel is available"。后三种什么都不执行。
- **不要求"原样重跑任意 Lisp"。** 完整控制类工具是先问后跑，所以不存在"沙箱中途拒绝、前半段已生效"的重试；但 Lisp 先修改状态、随后因普通错误退出的情况仍然存在，错误不会回滚——`eval_elisp` 现有描述"on an error, inspect any partial changes before retrying"保留不变。明确目标的工具在改之前检查，被权限拒绝时确实什么都没发生。

### 四、批准缝：`ask` / `never`，一次一答

`superchat-approval.el`（新，薄）：

- `approval.policy` 每 Session 一个值，`ask`（默认）或 `never`；`never` 在任何回答者之前直接 `rejected`（dsh：在 waterfall 之前强制，后注册的回答者也绕不过）。
- 提问走 `approval/request` waterfall：聊天 View 注册人类回答者（`read-multiple-choice`，选项只有"允许一次 / 拒绝"；dsh 没有 `allow_always`，superchat 去掉现在的 "a = 本会话一直允许"和 session allowlist）；批处理 `noninteractive` 没有回答者 → `unavailable` → 拒绝。
- 结果封闭：`allowed-once` / `rejected` / `cancelled` / `unavailable`；只有 `allowed-once` 放行。
- 批准绑定**这一次调用的参数快照**：批准缝在入口处对参数做逐层深拷贝（字符串、向量、hash-table、cons 都不共享），回答者拿到的是另一份拷贝；`superchat-approval-request-snapshot` 把入口快照连同结果一起返回，调用方在 `allowed-once` 之后**只执行这份快照**，等待期间原参数被改写或替换都不会在这次批准下运行。用快照实现，不重建哈希决策机。
- 所有回答者入口统一收尾：waterfall 回答者或兜底回答者抛错 → 关闭的 `unavailable`，照常记 `approval.decided` 并结清未决标记；任一入口的 `quit` → 记 `:interruption "user-quit"` 的拒绝、结清标记、把 quit 原样传播（批准缝发射时绑定 `superchat-event-waterfall-propagate-quit`，其他 waterfall 仍把 quit 读作拒绝）；只要问过人就续期传输等待。未决标记只在决定落盘之后清除——`approval.decided` 写入失败时标记保留，Run 照旧封 `audit-incomplete`。
- 每次提问在 Tape 上成对记 `approval.requested` `(:execution-id :reason)` / `approval.decided` `(:execution-id :decision "allow"|"deny")`（quit 时另带 `:reason "quit" :interruption "user-quit"`），必须在 Run 内；只进日志，不进模型上下文（dsh 同）。事件名沿用现有的，不改成 dsh 的 `approval/asked`：六条 `m3a/seal` 测试和 `superchat-tape.el` 的类型表都按现名计数，改名没有收益。未决批准标记（entered 在追加 requested 之前、settled 在追加 decided 之后）照旧经 Runtime 记账，`audit-incomplete` 封印不变。
- 回答者收到 `(:tool-name :args :reason :call-id :session-id :run-id)`；插件回答者据此自己渲染预览（取代 `register-approval-preview`，telega 的两个帮助窗口搬进它自己的回答者）。
- 默认人类回答者是变量 `superchat-approval-function`；每个 smoke 夹具和两条 seal 测试今天绑定的是 `superchat-authority-approval-function`，所以第①–③片里新变量为 nil 时回退读旧变量，第④片统一改名（含 `scripts/smoke.el` 与 leader 自有的两条 M3 绑定）。
- 人在决定期间必须续期传输等待（今天的 `superchat--extend-timeout` + `superchat-authority-transport-timer-owner`，`superchat-authority.el` 1176 行附近）；`authority` Service 的 `transport-timer-owner` 方法还被 subagent 并行映射使用，第④片改指到新家。

工具执行入口 `superchat-tool-execute` 保留为唯一缝，阶段改成 dsh 的三个 waterfall：`tools/pre-execute`（`(:kind allow|deny|ask :reason)`，没人管就是 allow；dsh 的契约允许直接返回 `ask`，不要求先失败一次）→ **档位规则**（固定阶段，不是监听者）→ 批准（仅 `ask`）→ `tools/execute`（around）→ `tools/post-execute`（`accept|block`）。组合规则只有一条：最终决定取 waterfall 结果与档位规则中更强的一个，`deny` > `ask` > `allow`。档位规则做成固定阶段的原因是 waterfall 监听者可以不调用 `next` 就结束链——telega 等插件对自己允许的调用返回 `allow`，只是结束了 waterfall，跳不过档位阶段；插件想更严才需要返回 `deny`/`ask`。这是一次三值比较，不是策略叠加引擎。

执行职责分工，避免固定阶段把一次性升级提前堵死或问两次：**固定档位阶段直接处理完整控制类和 MCP／第三方工具的 `ask`／`deny`；对明确目标的写工具不作常驻模式的提前拒绝**——目录限制、`read-only` 拒绝、升级参数校验与一次性升级的批准都由工具执行处（第二、三节）统一处理，固定阶段对它们返回 `allow`。插件 waterfall 的 `deny` 仍直接终止，不可通过升级绕过。保留：调用身份与 Run 存活检查、evidence origin 检查、Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed`、未结执行 / 未决批准 → `audit-incomplete` 封印（M3a）、ADR-0005 的可信 wrapper 准入、ADR-0006 的拒绝交付。

### 五、结果证据：从旧写入范围契约保留的一样东西

结果证据按工具的能力分两档，不为任意 Lisp 反推修改对象——那会把刚删掉的分析机制请回来：

- **明确目标的工具**必须报告：实际目标（解析后的 buffer 名 / 文件路径，indirect 时是基 buffer）、到了哪一层（只在内存 / 已写盘，含字节数）、做过的检查（目录判定结果、保存前是否已有未保存修改）。
- **`eval_elisp`、shell、MCP** 报告实际得到的返回值和输出；没有独立检查时明确写"未验证"。

这不是权限机制，是让"模型说完成了其实没完成"可核对的证据。[预算收尾指令](../implemented/2026-09-17-model-attempt-progress-and-budget-finalization.md)已要求最终答复按"已做／未做／未验证"分开陈述，工具结果按同一格式供料。

### 六、模型看到什么（照抄 dsh 的三处）

1. **系统提示不写当前档位。** dsh 试过在系统提示里写 "run under read-only sandbox"，模型 12 轮里 5 轮零工具调用、干脆不尝试，于是撤回（07-06 note § Alternatives）。
2. **一条"当前运行时上下文"快照**，作为 history 之后的 hard 用户片段随每次请求发送（现有 `plan` 片段是先例，`superchat-prompt-hooks.el` 200 行附近）；策略不变时逐字节相同。dsh 是"文本变化时才向日志追加一条 user 消息"（`RuntimeContextProjection`），superchat 的提示每次从 Tape 与片段重建，不需要那套簿记。前缀 "Current runtime context. This snapshot supersedes earlier runtime-context snapshots." 之后按档位选用（"DSH" 换成 "Superchat"，其余不动）：
   - read-only: "Current Superchat file policy: read-only. Any available operation enforced by the Superchat file sandbox cannot modify files in the standing mode. Do not refuse a required modification from this policy alone: try an available tool normally and follow any denial and escalation guidance it returns."
   - workspace-write: "Current Superchat file policy: workspace-write. Any available operation enforced by the Superchat file sandbox may modify files under the session workspace: \"<root>\". Some platform temporary areas may also be writable."
   - danger-full-access: "Current Superchat file policy: danger-full-access. The Superchat file sandbox does not restrict file modifications by available operations."
   - approval never: "Approval prompts are disabled in this session: actions that require approval are rejected automatically — do not request sandbox escalation (do not set `sandbox_permissions`)."
   - approval ask: "Approval policy: ask. Operations that require approval may ask through the configured answerers; without an available answerer, the request fails closed."
3. **工具描述**：明确目标的写工具带 dsh 的教学段（"Attempting a command the sandbox may deny is safe and expected … A rejected escalation is final for that command — stop and explain, never work around it …"，原文见 `packages/shell/tool-bash/src/index.ts` `bashDescription`）；`eval_elisp` 的描述改写为"完整 Emacs 控制，在 workspace-write 下每次调用需要用户批准，优先使用 edit_buffer / save_buffer 完成 workspace 内的编辑与保存"。

### 七、为什么没有全局护栏

首稿提议用全局 `before-change-functions` 钩子和临时 `file-name-handler-alist` 处理器把任意 Lisp 也约束在 workspace 内。独立审阅（DevSpace，2026-09-17）指出并由 leader 在裸 Emacs 32 复现了三条：钩子函数一 signal，Emacs 就把 `before-change-functions` 清空，第二次 `insert` 成功；从另一个 buffer 的 `after-change-functions` 里改受保护 buffer 时修改钩子被抑制，护栏不运行；indirect buffer 没有文件名却直写基 buffer。另外 `file-name-handler-alist` 有自己的处理器选择规则，工具登记的 timer 可以在动态绑定退出后才运行，工具自己的临时 buffer 也会被误伤。继续补齐要处理处理器组合、回调归属、间接 buffer、局部钩子——维护成本会超过被删掉的 Authority。因此约束只在知道目标的工具执行处实施；任意 Lisp 用批准而不是护栏管。

### 八、现有工具入口的去向

删掉全部 resolver 后，"没有监听者处理就默认 allow"必须对每个现有入口说清落在第一节的哪一列。按 `superchat-tools.el` 198–317 注册过 resolver 的名字逐一对应：

| 现有入口 | 去向 | 说明 |
|---|---|---|
| `list_buffers` `read_buffer` `apropos` `describe-function` `describe-variable` `workspace_read` `workspace_info` `sql` `memory_search` `tool_history` `file_history` `recent_errors` `lsp-*`、`team_task`/`team_message` 的 `list`/`read` | 读类 | 三档都允许；`superchat-authority-unprompted-read-tools` 与 `describe-variable` 的特殊处理一并删除 |
| `edit_buffer` `append_to_buffer` `replace_buffer` | 明确目标的写工具 | 目标为 buffer，按第二节分类判定 |
| `workspace_write` | 明确目标的写工具 | 目标是 Artifact 的 buffer 区域（可以是任何用户 buffer，且现实现绑定 `inhibit-read-only` 并 `widen`）或它的文件；按解析出的 buffer／路径判定，检查放进 `superchat-workspace-write` 解析出 Artifact 之后、修改之前 |
| `workspace_publish` | 运行时状态 | 它只往 Superchat 自己的数据目录 `superchat-data-directory/artifacts/` 新建不可变文件，不碰用户文件；按目录规则判它会在 `workspace-write` 下拒绝每一次发布。数据目录本身对 `write_file`／`save_buffer` 是 workspace 之外——其中有 `define_tool` 的批准账本，模型不能自己盖章 |
| `render_ui` | 运行时状态 | 画进聊天 View，三档允许 |
| `team_task` `team_message` 的写动作 | 运行时状态 | 写 Tape 记录，三档允许 |
| `delegate_to_subagent` `delegate_to_subagent_parallel` | 运行时状态 | 三档允许；子 agent 继承模式、批准策略 `never` |
| `ask_user`（可信输入） | 不变 | 走现有 `--make-human-input-operation` 路径，不进档位规则 |
| `eval_elisp` `shell-command` `eshell-command` | 完整 Emacs 控制 | 见第一节 |
| `define_tool` 及其生成的工具 | 完整 Emacs 控制 | 定义和调用都是任意 Lisp。现有两处自带确认——定义时的 `y-or-n-p` 与每次调用的 `superchat--syn-guarded-function` 自确认（`superchat-agent-confirm-synthesized`、`superchat--syn-call-confirmed`）——全部并入档位规则：`workspace-write` 下定义一次问一次（回答者可展示现有的定义预览 buffer）、调用每次问；`danger-full-access` 下都不问；`read-only` 下拒绝。不迁移它，`danger-full-access`"没有任何提示"的验收不成立 |
| MCP（`mcp:<server>:<tool>`） | MCP | 服务器名列表 `superchat-mcp-trusted-servers` 替代 `superchat-mcp-server-effects`（服务器定义本身在 mcp.el 的 `mcp-hub-servers`） |
| telega 插件的三个工具 | 由插件自己的 `tools/pre-execute` 监听决定 | 见"改"里的 telega 条目；插件监听者只能更严，不能绕过档位阶段 |

### 九、删、留、改（按 2026-09-17 的全量盘点）

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
- Tape 事件 `tool.requested` / `tool.execution.started` / `tool.completed` / `tool.failed` 及载荷（去掉 `:effects`、`:request-hash`、`:policy`、`:policy-version`、`:attendance`）；`approval.requested/decided` 名字不变，载荷瘦身为 `(:execution-id :reason)` / `(:execution-id :decision)`。
- Runtime 的未结工作标记 `superchat-runtime--note-authority-work`（entered/settled）与 `audit-incomplete` 封印——改名去掉 "authority"，语义不变。
- `superchat-authority-transport-timer-owner`、`superchat--tool-execution-context`、`--make-human-input-operation`（ask_user 的可信输入路径）、`--normalize-tool-result`、ADR-0006 的拒绝交付、`tool-liveness` 故障记录。
- Invariant `tool-execution-passes-authority` 改名 `tool-execution-passes-the-pipeline`。

**改（翻译成 dsh 形状）**
- 三个 waterfall Event：`tools/pre-execute`、`tools/execute`、`tools/post-execute`；现有 `tool-permission`、`tool-args-rewrite`、`tool-result-filter` 退役；`tool-before/after/failed` 保留。
- 新模块 `superchat-sandbox.el`：档位与两个旋钮、可写根、四个目标检查函数、标记文本、升级校验与 `approveEscalation` 编排、运行时上下文快照。新模块 `superchat-approval.el`：`ask`/`never`、`approval/request` waterfall、`approval.requested/decided`。档位与 `/permissions` 命令放在 `superchat.el`。新工具 `save_buffer`、`write_file`（`rename_file`、`delete_file` 可选）放在 `superchat-tools.el`。
- "可选工具"的定义从"有 effect resolver"改为"Scope `tool` 层有活安装，或在 `superchat-tools--builtin-tool-names` 里"（`superchat--tool-descriptor-p`、`superchat-register-tool` 的 `:effects` 必填、无生产调用者的 `tools` Service `descriptors` 方法一起改）；`tools`/`tools-builtin` 插件的 `:inject '(authority)` 改指新模块；`define_tool` 的内层自确认靠执行缝里三处 `superchat--syn-call-confirmed` 绑定压住，换缝时同一改动里删掉内层守卫。
- **telega 插件**：现在注册两条策略层（`telega-digest`、`telega-triage`）、四个策略名、三组 effect resolver 与 preview，以及 Request 级 `:approval-function`。改为：每个 Request 注册自己的 `tools/pre-execute` 监听（按工具名 deny 自己不允许的调用）和 agent 作用域的 `approval/request` 回答者（自动放行已授权的读）。这是唯一要重写逻辑而不是删除的地方。
- **MCP 工具**：dsh 的 MCP 桥没有批准门（`packages/mcp/mcp-client`）。superchat 用服务器名列表 `superchat-mcp-trusted-servers` 声明可信与否（替代 `superchat-mcp-server-effects`）：可信的直接做，否则在 `workspace-write` 下每次问；本地 workspace 路径约束不了远端工具，契约不假装能。
- **smoke case U** 与 `smoke-architecture` R7 读取策略层与 resolver 名单——随删除改写；canonical smoke 的 38 份基线不含 `approval.*` 行，事件序列不受影响。
- M3 中 3 条名字带 authority 的测试（`m3/request/*-through-authority` 两条、`m3a/removal/recovery-keeps-…-authority-gate`）与 6 条 `m3a/seal` 测试：前者随 wrapper/pipeline 改名重写，后者语义保留、只改两处批准函数变量名；M5 的 3 条 discovery 测试（免提示名单）删除，改为"读工具不问、workspace 内写不问"的新断言；`m5-tool-feedback` 里 11 条绑定批准函数的测试改绑新的回答者。

### 十、与既有决策的冲突

[2026-09-12 蓝图提案](../proposed/2026-09-12-restructure-on-deepseek-harness.md)写着 "Kept and moved rather than rewritten: Authority decide and audit"、"a contributed allow cannot relax an inherited deny"，以及 "Port dsh's seams as they are (filesystem, shell, subprocess, sandbox) — rejected: superchat's world is Emacs objects"。本记录取代前两句；第三句仍成立——目标检查针对的是 Emacs 的 buffer 和文件，不是移植 dsh 的 OS 沙箱。`CONTEXT.md` 的 Authority 词条（"只能收紧的有效策略"）随之改写为档位 + 批准策略。

## 已确认的选择

已定：默认档位 `workspace-write`（用户 2026-09-17）；`eval_elisp` 与 shell 同类，在 `workspace-write` 下逐次问；不做全局护栏；名字照抄 dsh；`read-only = read-only + ask`，写入靠一次性升级，完整控制类拒绝；MCP 在 `read-only` 下全拒、可信布尔只管 `workspace-write` 下问不问。以下两项用户 2026-09-18 答"都按建议"：

| # | 待定 | 选项 | 建议 |
|---|---|---|---|
| 1 | workspace 根从哪来 | (a) 先用 `/workspace DIR` 绑定，其次来源 buffer 的 project root，最后 Session 启动目录 (b) 严格照 dsh 只用启动目录 | **(a)** — 用户 2026-09-18 已确认 |
| 2 | 交付与切换 | (a) 四片，前三片落代码并独立验收但**正式 Session 继续走旧入口**（内部开关 `superchat--permission-levels-enabled`，默认 nil，第四片删除该开关）；第四片完成默认工具、telega、MCP 与调用入口迁移后统一启用新档位并删 Authority (b) 一次交付 | **(a)** — 用户 2026-09-18 已确认；避免界面显示新档位、行为仍由旧 Authority 决定的中间态 |

四片的内容：①批准缝（`ask`/`never`、回答者、参数快照、`approval.requested/decided`）+ 档位与三个事件 + `/permissions`；②`save_buffer`/`write_file` + 明确目标写工具的目标检查 + 升级参数与标记；③完整控制类（含 `define_tool`）与 MCP 按档位问/拒 + 运行时上下文快照；④删 Authority、改 telega、迁移调用入口、重写测试与文档、启用。

## 验收标准

新建独立回归（M3/M5 受保护集合的规则不变；M3 中 3 条名字带 authority 的测试由 leader 重写）：

1. `workspace-write`：workspace 内的 `edit_buffer` + `save_buffer` 从头到尾**不弹任何提示**，文件字节落地；workspace 外（含 `..`、绝对路径、指向外部的符号链接、indirect buffer 的基 buffer 在外）被拒，磁盘与 buffer 不变，Tape 有 `tool.requested` + `tool.failed`；`temporary-file-directory` 可写。
2. `workspace-write`：每次 `eval_elisp` / `shell-command` 都恰好问一次；拒绝后什么都没跑；`never` 下直接拒绝、不问。
3. 升级：被拒后带 `sandbox_permissions`+`justification` 重试，批准函数恰好调用一次；`allowed-once` 后该调用以更宽模式执行且写入落地；`rejected`/`cancelled`/`unavailable` 各返回自己的文本、什么都不改；不严格加宽的请求不问人直接失败；缺一个参数即失败；`rejected` 的结果让后续反馈说"不要重试"、参数错误说"改正后重试"（`superchat-llm.el` 1843–1910 的三张 `[Tool call …]` 前缀表随新文本同步）。
4. `read-only`：明确目标的写工具被拒且可按第 3 条升级；完整控制类与 MCP 被拒且不可升级（带 `sandbox_permissions` 的 `eval_elisp` 也拒绝）；读工具与运行时状态类照常。
5. `danger-full-access`：没有任何提示——包括 `define_tool` 的定义与调用、synthesized 工具的自确认、MCP 工具。
5a. 保存与写文件：indirect/narrowed buffer 的 `save_buffer` 保存基 buffer 全文；无文件 buffer 返回错误且不弹 minibuffer；含用户未保存修改的 buffer 保存完整内容并在结果中说明；`write_file` 遇到已被访问的文件不写盘、不改 buffer、返回该 buffer 名。
6. 批准绑定参数快照：等待期间替换参数的调用不被放行。
7. 切换档位：`permission.level` / `sandbox.mode` / `approval.policy` 事件进 Tape，重开 Session（含真正关闭再打开数据库连接）后档位仍在、两个 Session 互不串档；运行时上下文快照按第六节作为 hard 片段随每次请求发送，策略不变时逐字节相同，档位变化后的下一次请求反映新值。
8. 结果证据：明确目标的工具结果含实际目标、层级、检查三项；`eval_elisp`/shell/MCP 的结果含返回值，无独立检查时写"未验证"。
8a. 组合规则：一个返回 `allow` 且不调用 `next` 的 `tools/pre-execute` 监听者，不能让 `workspace-write` 下的 `eval_elisp` 免问。
9. 真实任务：M5 的 T2（改 emacs-lisp-mode 快捷键并持久化）在默认档位重跑一次：workspace 内的编辑与保存零提示；`eval_elisp` 与写 `~/.emacs.d` 各问一次。模型是否随后完成任务是另一项证据。
10. 删除后：`(require 'superchat)` 通过；canonical smoke 事件序列不变；字节编译无新增警告；README 双语的权限章节改写为三档。
11. 切换时点：第①–③片合入后，开关为 nil 时正式 Session 的行为与合入前逐字节一致（旧回归全绿）；第④片翻转后旧 Authority 符号不再存在。

## Alternatives considered

**只删策略层、哈希、预览、名单，保留 Effect 与逐次批准。** 留下的仍是"每步都问"的体验和 resolver 维护成本；dsh 没有 Effect 概念，保留它就仍要为每个新工具写分类。

**全局修改钩子 + 文件处理器把任意 Lisp 关进 workspace（首稿）。** 见第七节：三条缺陷已复现，且会误伤工具自己的临时 buffer；补齐它的维护成本超过被删的 Authority。

**只拦 `edit_buffer` 名字，或扫描 eval 文本。** 拦不住 `save-buffer`，文本匹配漏报误报；dsh 也否决了 "command-string heuristic preflight"。

**指名保护单个文件（写入范围契约）。** dsh 无此概念；它靠区域边界 + 人看着 justification 批准。指名保护需要额外的声明入口和 UI，先按 dsh 做，真实任务再证明需不需要。

**在系统提示里说明当前档位。** dsh 实测导致模型不敢尝试，已撤回；采用其后续的运行时上下文快照。

**同一次工具调用内自动重试升级。** dsh 否决：日志无法重建"一次调用两次执行"；重试必须是新调用。

**移植 dsh 的 OS 级 shell 沙箱。** 那是操作系统工作（Seatbelt profile、bwrap），做成后 shell 在 `workspace-write` 下可以像 dsh 一样不问；在此之前 shell 与 eval 同类逐次问。单独立案。

## Consequences

- 默认改为 `workspace-write` 后，workspace 内的编辑与保存不再逐次询问——这是有意的行为变化，README 与 CHANGELOG 必须写明，并给出回到"只看不动"的办法（`read-only`）。
- 无文件 buffer 的三分类（工具临时 / 用户内容 / 内部状态）要有一个明确的判定函数并逐类测试；漏判"内部状态"会让模型改到聊天 View 或 Tape 缓冲。
- `eval_elisp` 在默认档位逐次问，配置类任务每次 1–2 个提示；嫌烦的用户要知道 `/permissions danger-full-access` 的存在。
- M3 的 3 条 Authority 命名测试与 seal 测试里的两处变量名、27 份提到 Authority 的文档要同步改；这是删除量大于新增量的一轮，但涉及 12 个生产文件和 telega 插件的逻辑重写。
