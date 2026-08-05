# 主流 Agent Harness 的"模型生成代码/能力"信任围栏调研

> 调研日期:2026-07-20。方法:只采信一手资料(官方文档、GitHub 源码、规范原文);
> Emacs 部分直接读本机源码(GNU Emacs 31.0.90,
> `/opt/homebrew/Cellar/emacs-plus@31/31.0.90/share/emacs/31.0.90/lisp/`)。
> 查不到一手资料的论断标注"未能证实"。
> 目的:为 superchat homoiconic Phase 2(合成工具持久化与再水化)提供业界先例对照。

## 1. Claude Code:规则式 permission 系统 + hooks

来源:[Configure permissions](https://code.claude.com/docs/en/permissions)、
[Settings](https://code.claude.com/docs/en/settings)、
[Hooks reference](https://code.claude.com/docs/en/hooks)、
[MCP in Claude Code](https://code.claude.com/docs/en/mcp)。

### 1.1 allow/deny/ask 规则与 Bash 前缀匹配

- 规则形如 `Tool` 或 `Tool(specifier)`,分三个列表:allow(免确认)、ask(强制弹窗)、deny(拒绝)。求值顺序固定为 **deny → ask → allow,首个命中即定局,规则的具体程度不改变顺序**——宽 deny 会盖过窄 allow,"a deny rule can't carry allowlist exceptions"。
- Bash 规则支持通配:`Bash(npm run test *)` 前缀匹配,`*` 可出现在任意位置并跨参数;空格是词边界(`Bash(ls *)` 匹配 `ls -la` 不匹配 `lsof`);`:*` 后缀等价于尾部 ` *`。
- **防注入是现行设计的一部分**:"Claude Code is aware of shell operators, so a rule like `Bash(safe-cmd *)` won't give it permission to run the command `safe-cmd && other-cmd`. The recognized command separators are `&&`, `||`, `;`, `|`, `|&`, `&`, and newlines. A rule must match each subcommand independently."(早年文档"前缀匹配可被 `;` 注入绕过"的警告已被这套 shell 分隔符感知机制取代。)
- 但官方明确警告**用参数模式约束内容是脆弱的**:"Bash permission patterns that try to constrain command arguments are fragile"——`Bash(curl http://github.com/ *)` 拦不住选项前置、协议变体、重定向、`URL=... && curl $URL` 变量间接等,建议改用 deny + `WebFetch(domain:)` 或 PreToolUse hook 校验。环境运行器(`devbox run`、`npx`、`docker exec`)与 exec wrapper(`watch`、`find -exec`)是点名的绕过面。
- Read/Edit 的 deny 规则只覆盖内建文件工具和 Bash 里可识别的文件命令,"They don't apply to arbitrary subprocesses that read or write files indirectly... For OS-level enforcement... enable the sandbox"——**规则匹配与 OS 沙箱被明确定位为两层不同的东西**。

### 1.2 Permission modes

`default`(每工具首次使用弹窗)、`acceptEdits`(自动接受文件编辑)、`plan`(只读)、`auto`(自动批准 + 后台安全检查)、`dontAsk`(未预批准即自动拒绝)、`bypassPermissions`(跳过弹窗,但显式 ask 规则与 `rm -rf /` 等 "circuit breaker" 仍拦)。可用 `permissions.disableBypassPermissionsMode` 从管理层面禁用旁路模式。

### 1.3 "don't ask again" 的粒度与持久化(与 superchat 最相关)

- 批准粒度**按工具类型分层**:Bash 命令是 "Permanently per repository and command"(按仓库、按命令持久);文件编辑批准只到 "Until session end",**不落盘**。
- 持久化位置:"When you choose 'Yes, don't ask again' and the approval saves permanently, such as for a Bash command, Claude Code saves the rule to `.claude/settings.local.json` at the root of the git repository"——**项目级 local 文件(git-ignored),不是用户全局文件**。保存形态是命令前缀规则(空格分隔形式);复合命令按子命令拆开保存(单次最多 5 条)。
- settings 优先级:managed settings > 命令行参数 > `.claude/settings.local.json` > 项目 `.claude/settings.json` > `~/.claude/settings.json`;"If a tool is denied at any level, no other level can allow it"。
- **防篡改/防社工**:项目共享 `.claude/settings.json` 的 allow 规则要等用户接受 workspace trust 对话框后才生效(deny/ask 不受影响);`.claude` 是符号链接或 local 文件被提交进 git 时同样走 trust 检查。

### 1.4 Hooks 作为第二道围栏

- PreToolUse hook 可返回 JSON 决策 `permissionDecision: allow/deny/ask`,或以退出码 2 硬拦(stderr 反馈给模型)。退出码 1 是**非阻塞**错误、动作照常进行——"If your hook is meant to enforce a policy, use `exit 2`"。
- Hook 与规则的关系是**只紧不松**:"Hook decisions don't bypass permission rules. Claude Code evaluates deny and ask rules regardless of what a PreToolUse hook returns... A blocking hook also takes precedence over allow rules."

### 1.5 MCP 工具批准粒度

- 规则支持两级粒度:整 server(`mcp__github`)与单工具(`mcp__github__get_issue`);allow 通配必须锚定在 `mcp__<server>__` 前缀之后。持久化与 Bash 相同(项目 settings.local.json)。交互弹窗上 always-allow 具体给的是哪一档,文档未逐字说明——**未能证实**;Claude Desktop 的 MCP 批准粒度与存储位置亦**未能证实**。
- 值得注意的反向机制:MCP server 可在工具元数据设 `_meta["anthropic/requiresUserInteraction"]: true`,则该工具**每次调用都弹窗,任何模式、任何 allow 规则都不能免,也不提供 "don't ask again"**([文档](https://code.claude.com/docs/en/mcp#require-approval-for-a-specific-tool))。这是"工具作者宣告自己永远需要人在场"的先例,与 superchat "合成工具一律视为破坏性"的立场同构。
- 项目 `.mcp.json` 里的 server 首次使用前需 server 级批准("For security reasons, Claude Code prompts for approval before using project-scoped servers"),可用 `claude mcp reset-project-choices` 重置。

## 2. MCP 规范:human-in-the-loop 是规范级要求

来源:[MCP spec 2025-06-18 — Tools](https://modelcontextprotocol.io/specification/2025-06-18/server/tools)、
[Sampling](https://modelcontextprotocol.io/specification/2025-06-18/client/sampling)。

- Tools 页 User Interaction Model 原文:"For trust & safety and security, there **SHOULD** always be a human in the loop with the ability to deny tool invocations." 应用 SHOULD:清晰展示暴露给模型的工具、调用时给视觉指示、"Present confirmation prompts to the user for operations, to ensure a human is in the loop"。
- 工具自我描述不可信:"clients **MUST** consider tool annotations to be untrusted unless they come from trusted servers." —— **模型/服务器一侧的任何自我声明都不能作为放行依据**,这直接支持 superchat 不信任合成工具的 description 的立场。
- Clients SHOULD:"Prompt for user confirmation on sensitive operations; Show tool inputs to the user before calling the server... Validate tool results before passing to LLM; ... Log tool usage for audit purposes."
- Sampling 页对模型调用同样要求双向人审:"there SHOULD always be a human in the loop with the ability to deny sampling requests",流程图显式画出请求与响应各一次人工审核。

## 3. OpenAI Codex CLI:批准策略 × OS 沙箱的二维矩阵

来源:openai/codex 仓库迁移前文档快照(commit 7508e4f,[docs/config.md](https://raw.githubusercontent.com/openai/codex/7508e4fd2d0161a3c7558391f7c0ce5fe92224f5/docs/config.md)、[docs/sandbox.md](https://raw.githubusercontent.com/openai/codex/7508e4fd2d0161a3c7558391f7c0ce5fe92224f5/docs/sandbox.md));现行官方文档([approvals & security](https://learn.chatgpt.com/docs/agent-approvals-security.md)、[config reference](https://learn.chatgpt.com/docs/config-file/config-reference.md)、[sandboxing](https://learn.chatgpt.com/docs/sandboxing))。注:仓库 docs 已于 2026-01 迁往官方站。

- **两个正交维度**:`sandbox_mode` 决定技术上能做什么(`read-only` / `workspace-write` / `danger-full-access`);`approval_policy` 决定何时必须问人(`untrusted` / `on-failure`(现已 deprecated)/ `on-request` / `never`)。旧文档原文:`untrusted` = "Codex will prompt the user before running a command not in the 'trusted' set";`on-failure` = "If the command fails when run in the sandbox, Codex asks for permission to retry the command outside the sandbox";`on-request` = "The model decides when to escalate"。
- **UI 预设**:Auto(默认)= `workspace-write` + `on-request`;Read Only = `read-only` + `on-request`;Full Access = `--dangerously-bypass-approvals-and-sandbox`(别名 `--yolo`,"not recommended")。
- **沙箱实现**:macOS 12+ 用 Seatbelt(`sandbox-exec`);Linux 用 Landlock + seccomp;`workspace-write` 下网络默认禁用(`[sandbox_workspace_write] network_access = true` 打开);即使工作区可写,`<writable_root>/.git`、`.codex` 等**仍保持只读——沙箱保护自己的配置与 git 历史不被 agent 改写**。
- **信任按项目持久化**:config.toml 中 `[projects."/path/to/project"] trust_level = "trusted"`;现行文档补充:"Untrusted projects skip project-scoped .codex/ layers"——不信任的项目连项目级配置层都不加载。首次进入目录时,受版本控制的目录推荐 Auto、否则 read-only,"Codex may also start in read-only until you explicitly trust the working directory"。
- **逃逸即模式**:批准的语义不是"改规则",而是"这一条命令在沙箱外重跑一次"——确认门与沙箱耦合成"失败/提权时才打扰人"的节流阀。这是对抗确认疲劳的结构性方案:沙箱吃掉绝大多数无害操作,只有越界请求才见到人。

## 4. Cursor:从 denylist 到 allowlist + 沙箱 + 分类器

来源:[agent security](https://cursor.com/docs/agent/security)、
[run modes](https://cursor.com/docs/agent/security/run-modes)、
[permissions reference](https://cursor.com/docs/reference/permissions)、
[1.3 changelog](https://cursor.com/changelog/1-3)、
[auto-review changelog](https://cursor.com/changelog/auto-review)。

- 默认 "By default, terminal commands need your approval";官方对整套围栏的定性异常坦率:它们是 "best-effort guardrails rather than a hard security boundary"。
- **denylist 已被官方废弃**:1.3 changelog(2025-07-29)原文 "Improved security by removing denylist (in favor of allowlists) for auto-run"。(背景:第三方研究 Backslash 演示了 Base64、子 shell、写脚本等多种绕过——第三方来源,官方仅承认为安全改进。)**黑名单在命令语言上不可防守,这一点 Cursor 用产品史投了票。**
- 现行三种 Run Mode(3.6 起):**Auto-review**(默认:allowlist 命中即跑 → 可沙箱化的进沙箱 → 其余交 classifier 子代理裁决)、**Allowlist**、**Run Everything**(旧 YOLO 等价物,"Every tool call runs automatically";auto-run 旧称 YOLO 的对应关系现行文档已无表述,**未能证实(一手)**)。官方对 classifier 的免责:"The classifier can make mistakes... Auto-review is not a security boundary."
- allowlist 存放:`~/.cursor/permissions.json`(全局)与项目 `.cursor/permissions.json`,字段 `terminalAllowlist` / `mcpAllowlist` / `autoRun.allow_instructions`;免责声明:"Allowlists and `autoRun` instructions are best-effort convenience. They are not a security guarantee." allowlist 自身也有官方安全公告承认的绕过([GHSA-534m-3w6r-8pqr](https://github.com/cursor/cursor/security/advisories/GHSA-534m-3w6r-8pqr) 反引号/`$()`;[GHSA-82wg-qcm4-fp2w](https://github.com/cursor/cursor/security/advisories/GHSA-82wg-qcm4-fp2w) shell 内建不经 allowlist)。
- 保护开关(run modes 页):Browser Protection、File-Deletion Protection("including `rm` commands")、External-File Protection(工作区外文件);沙箱内 `.git/config`、`.vscode` 受保护。旧版 Dotfile Protection 现行文档未列——**未能证实(现行版本)**。

## 5. Aider:几乎无确认门,git 自动 commit 即安全网

来源:[aider.chat/docs/git.html](https://aider.chat/docs/git.html)、
[usage/commands](https://aider.chat/docs/usage/commands.html)、
[config/options](https://aider.chat/docs/config/options.html)。

- 核心哲学是**事后可撤销取代事前确认**:"Whenever aider edits a file, it commits those changes with a descriptive commit message",价值在于 "makes sure you never lose your work if aider makes an inappropriate change";usage 首页直言 "Aider will git commit all of its changes, so they are easy to track and undo."
- **dirty commit**:编辑已有未提交改动的文件前,先把用户改动单独 commit("Aider takes special care before editing files that already have uncommitted changes (dirty files). Aider will first commit any preexisting changes")——保证 AI 改动与人的改动在历史里可分离。
- 撤销通道:`/undo` = "Undo the last git commit if it was done by aider"(只回退 aider 自己的 commit)。
- 开关:`--auto-commits`(默认 True)、`--dirty-commits`(默认 True)、`--no-git`(文档提醒自行备份)。
- shell 命令:模型建议的命令有 `--suggest-shell-commands`(默认 True)开关;`--yes-always`("Always say yes to every confirmation")的存在佐证建议命令执行前有确认提示,但"确认后才执行"的确切交互流程在文档正文无原句——**未能证实到原文级**。
- 对 superchat 的意义:Aider 的安全网**只覆盖文件编辑**(git 可回滚的域);它并未解决"执行任意代码"的问题——不可逆副作用(网络、删除 git 外文件)不在这张网里。

## 6. OpenHands:sandbox-first,确认门退化为可选项

来源:[Runtime 架构](https://docs.openhands.dev/openhands/usage/architecture/runtime)、
[SDK security guide](https://docs.openhands.dev/sdk/guides/security)、
[config.template.toml](https://github.com/OpenHands/OpenHands/blob/main/config.template.toml)。

- Docker Runtime 是 "the core component that enables secure and flexible execution of AI agent's action";动机原句:"Executing untrusted code can pose significant risks to the host system. A sandboxed environment prevents malicious code from accessing or modifying the host system's resources." 所有动作经容器内的 action execution server 执行,后端只 "sending actions and receiving observations"。
- 因为爆炸半径被容器兜住,**确认门默认关闭**:config 模板 `#confirmation_mode = false`。SDK 提供三种 policy:`AlwaysConfirm()`、`NeverConfirm()`、`ConfirmRisky()`(需 security analyzer);风险分级 LOW/MEDIUM/HIGH/UNKNOWN,默认 analyzer 是 `LLMSecurityAnalyzer`——"leverages the LLM's understanding of action context",由模型在生成 tool call 时自附 security_risk 字段。等待确认时会话进入 `WAITING_FOR_CONFIRMATION`。
- CLI 二进制默认 "Always ask for confirmation",提供 `--always-approve` 与 `--llm-approve`;headless 强制 NeverConfirm(来源为 GitHub README,GUI 默认值的文档原句**未能证实**)。
- 对 superchat 的意义:这是谱系的另一极——**有沙箱才买得起"默认不确认"**;并且"让 LLM 自评风险再决定是否弹窗"的 `ConfirmRisky` 路线,恰是 MCP 规范"annotations 不可信"警告的反面教材式对照(OpenHands 自己也把它标为需要 analyzer 的可选项)。

## 7. Emacs 自身的信任模型(离 superchat 最近的先例)

来源:本机 GNU Emacs 31.0.90 源码,`/opt/homebrew/Cellar/emacs-plus@31/31.0.90/share/emacs/31.0.90/lisp/` 下的 `files.el`、`custom.el`、`cus-edit.el`、`progmodes/elisp-mode.el`(行号按该版本)。上游镜像:[emacs.git lisp/files.el](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/files.el)、[lisp/custom.el](https://git.savannah.gnu.org/cgit/emacs.git/tree/lisp/custom.el)。

### 7.1 file-local variables:safe 谓词 + risky 名单 + (name . value) 记忆

- **safe 判定**(`safe-local-variable-p`,files.el:4498):一个 (SYM . VAL) 安全,当且仅当 (a) `(SYM . VAL)` **原样出现在** `safe-local-variable-values` 里,或 (b) SYM 的 `safe-local-variable` 属性是一个谓词函数且对 VAL 返回非 nil。即**记忆的粒度是"名字+具体值"的整对**,换一个值就要重新批准。
- **risky 判定**(`risky-local-variable-p`,files.el:4515):`risky-local-variable` 属性非 nil,或名字后缀命中启发式正则(`-hooks?$`、`-functions?$`、`-program$`、`-map$`、`font-lock-keywords` 等)——**能持有代码的变量按名字模式默认危险**。
- **确认 UI**(`hack-local-variables-confirm`,files.el:3993):专用 buffer 逐条列出变量与值(unsafe 标 `*`,risky 标 `**`),按键 `y / n / ! / i / +`。选 `!` 时执行 `(customize-push-and-save 'safe-local-variable-values unsafe-vars)`——把整对 (VAR . VAL) 追加进列表并**立即写入 custom-file(或 init 文件)**;选 `i` 同样持久化进 `ignored-local-variable-values`(负面记忆);选 `+`(仅目录局部变量)把目录加进 `safe-local-variable-directories`(Emacs 30 新增,files.el:3981)——目录级全量放行。
- `customize-push-and-save`(cus-edit.el:1192)是这套持久化的统一出口:`add-to-list` 后调 `customize-save-variable` 写盘;若 Emacs 尚未初始化完则挂到 `after-init-hook` 延后写。**批准记录以人类可读的 elisp 形式躺在用户自己的 custom-file 里,无签名、无校验,防篡改完全依赖"该文件本来就是用户可信代码"这一事实。**
- **risky 变量没有 "!" 通道**:`offer-save` 只在存在 unsafe-vars 时出现,且 `customize-push-and-save` 只收 unsafe-vars——risky 变量每次都要 y,除非走 `+` 的目录级信任。这是"值级记忆对危险载体不开放"的先例。

### 7.2 `enable-local-eval`:eval 局部变量的三态门

files.el:804:`enable-local-eval` 默认 `'maybe`——"A value of t means obey `eval` variables. A value of nil means ignore them; anything else means query." 过滤逻辑在 `hack-local-variables-filter`(files.el:4188):`eval:` 表达式先过 `hack-one-local-variable-eval-safep`(结构性白名单,只认少数已知安全形态如 `safe-local-eval-forms` 里的 `add-hook 'before-save-hook 'time-stamp` 等)或此前被 `!` 记住;不安全的进 unsafe-vars 走同一个确认 buffer。也就是说 **`eval:` 的 y/n/! 与普通局部变量共用同一道门与同一份 (name . value) 记忆**——用户选 `!` 后,`(eval . <整个表达式>)` 这一对被存进 `safe-local-variable-values`,表达式变一个字符就重新弹窗。**这是"内容锚定批准"的弱形式:锚的是 sexp 原文相等,而非 hash。**

### 7.3 `custom-safe-themes`:内容寻址(SHA-256)批准的直接先例

custom.el:1319 与 1337(`load-theme`)、1483(`custom-theme-load-confirm`):

```elisp
(defcustom custom-safe-themes '(default)
  "Themes that are considered safe to load.
If the value is a list, each element should be either the SHA-256
hash of a safe theme file, or the symbol `default', ...")
```

`load-theme` 的实际检查(custom.el:1386–1401):

```elisp
((with-temp-buffer
   (insert-file-contents file)
   (let ((hash (secure-hash 'sha256 (current-buffer))))
     (when (or (member hash custom-safe-themes)
               (custom-theme-load-confirm hash))
       (eval-buffer nil nil file)
       t))))
```

关键实现细节:

- **hash 的对象是主题文件的完整文本内容**(`insert-file-contents` 后对整个 buffer 做 `secure-hash 'sha256`),不是文件路径、不是主题名。文件改一个字节,hash 失配,重新走确认。
- 确认流程(`custom-theme-load-confirm`)是**两问**:先 "Loading a theme can run Lisp code. Really load?";同意后再问 "Treat this theme as safe in future sessions?",第二问同意才 `(customize-push-and-save 'custom-safe-themes (list hash))` 把 hash 写入 custom-file。**确认前源码会以 `emacs-lisp-mode` pop 到 `*Custom Theme*` buffer 给用户看**——"批准前展示全文"是流程的一部分。
- hash 命中时**静默加载,零提示**(上面 `member` 分支直接 `eval-buffer`)。这正是"内容寻址 + 静默再水化"的完整先例:批准状态与内容绑定,内容不变则不打扰,内容变了自动降级回确认门。
- 逃逸阀:`custom-safe-themes` 可设 `t`(信任一切)、`default` 符号(信任 Emacs 自带 themes 目录);`load-theme` 的 `no-confirm` 参数供 init 文件使用。变量本身被标 `:risky t`——**批准列表自己不能被文件局部变量注入**,这是一个容易被忽略的自我保护细节。

### 7.4 Emacs 30 的 `trusted-content`:目录/文件级信任谓词

files.el:718(`:version "30.1"`):`trusted-content` 是文件/目录名列表(目录以 `/` 结尾表示前缀信任),或 `:all`("opens a gaping security hole",且"must never be set by a major or minor mode")。判定函数 `trusted-content-p`(files.el:741):

- 先看 buffer-local 的 `untrusted-content`(邮件客户端等可强制标脏,`permanent-local`,一票否决);
- 用 `buffer-file-truename` 比较以**防符号链接把文件"挪进"可信目录**(源码注释:"to try and avoid marking as trusted a file that's merely accessed via a symlink");
- 目录前缀匹配刻意不用 `file-in-directory-p`,注释写明取保守侧:"guilty until proven innocent";
- 用户 init 文件无条件可信("We can't avoid trusting the user's init file")。

消费方(本机 grep):`elisp-mode.el:862` 的 `elisp--safe-macroexpand-all` 在不可信 buffer 里拒绝展开非安全宏;`elisp-mode.el:2732` 的 flymake byte-compile 后端对不可信内容直接 `user-error` 拒跑。**注意:`trusted-content` 没有交互式批准/持久化流程——它是纯声明式 defcustom,用户手工在 init 里维护;不存在 "trust this file? y/n/!" 弹窗**(本机 Emacs 31.0.90 源码中无此类调用)。名为 `trusted-files` 的库/变量在本机 31.0.90 中不存在(`locate-library` 返回 nil)——任务描述中的 "trusted-files" 应即 `trusted-content`。

### 7.5 Emacs 模型小结

Emacs 用**四种粒度**分层:值级 (name . value) 记忆(safe-local-variable-values)、内容 hash 记忆(custom-safe-themes)、目录级(safe-local-variable-directories / trusted-content)、全局开关(`enable-local-eval` / `:all`)。所有批准记录统一经 `customize-push-and-save` 落到用户 custom-file,**形式是普通 elisp,可读可编辑,无防篡改**;对抗确认疲劳的手段是 `!`(正面记忆)与 `i`(负面记忆)对称提供,加上 safe 谓词让海量良性值根本不进门。

## 8. VSCode Workspace Trust:目录级信任的 UX 标杆

来源:[Workspace Trust 文档](https://code.visualstudio.com/docs/editing/workspaces/workspace-trust)、
[vscode 源码 workspaceTrust.ts](https://github.com/microsoft/vscode/blob/main/src/vs/workbench/services/workspaces/common/workspaceTrust.ts)。

- **Restricted Mode 限制什么**(文档原句):AI agents("Opening a workspace in restricted mode disables agents in that workspace")、终端("opening a terminal is blocked by default")、tasks、调试("debugging is also disabled")、一组危险的 workspace settings 被忽略、未声明支持 trust 的扩展默认禁用。
- **粒度**:单文件夹,或父文件夹递归——"You can also trust a parent folder, which applies trust to the parent folder and all its subfolders."
- **UX**:当前文档描述新文件夹以 Restricted Mode 打开并显示 trust banner(早期版本是首开模态对话框 "Do you trust the authors...";现行文档措辞为 banner/prompt,由 `security.workspace.trust.startupPrompt` 配置)。相关设置:`security.workspace.trust.enabled` / `.startupPrompt` / `.emptyWindow` / `.untrustedFiles` / `.banner`。
- **持久化位置**:文档只说加入 "Trusted Folders & Workspaces" 列表;源码证实(workspaceTrust.ts:36 `WORKSPACE_TRUST_STORAGE_KEY = 'content.trust.model.key'`;:279 `storageService.store(..., StorageScope.APPLICATION_SHARED, StorageTarget.MACHINE)`)——信任的 URI 列表 JSON 序列化后存在**用户全局 application 级 storage**,而非工作区内。落盘的具体物理文件名(state.vscdb)**未能证实**到源码原文级。
- 关键设计点:信任先于一切能力开启,且**信任记录放在被信任对象够不着的地方**(工作区内容无法改写自己的信任状态)——与 Claude Code 对项目 settings.json 的 workspace-trust 门是同一原则。

## 9. 综合对比表

### 9.1 批准粒度谱系

从最细到最粗,业界实际使用的粒度依次是:

| 粒度 | 实例 | 备注 |
|---|---|---|
| **每次调用确认(不可记住)** | Claude Code 的 `requiresUserInteraction` MCP 工具、org-ask connector;Emacs risky 变量(无 `!` 通道);Cursor 默认终端确认 | 工具/变量自身宣告"永远需要人在场" |
| **值级/内容级记住** | Emacs `safe-local-variable-values`(name+value 整对)、`eval:` 表达式原文相等 | 值变即重问 |
| **内容寻址 hash** | Emacs `custom-safe-themes`(文件全文 SHA-256) | 唯一的 hash 先例;命中即静默执行 |
| **命令前缀记住** | Claude Code "don't ask again"(Bash 前缀规则,按仓库);Cursor `terminalAllowlist`;Codex `untrusted` 模式的 trusted 命令集 | 匹配面大于批准时看到的那条命令 |
| **目录/项目级** | Emacs `safe-local-variable-directories`、`trusted-content`;Codex `[projects] trust_level`;VSCode Workspace Trust;Claude Code 的 workspace trust 门 | 一次决定覆盖整个目录树 |
| **全局开关** | `custom-safe-themes = t`、`trusted-content = :all`、Codex `--yolo`、Cursor Run Everything、Claude Code `bypassPermissions`、Aider `--yes-always` | 各家都保留但都标"危险/不推荐";Claude Code/OpenHands 允许管理面禁用 |

### 9.2 各家横向对比

| | 批准粒度 | 批准记录存放处 | 防篡改 | 对抗确认疲劳 | 沙箱与确认门的关系 |
|---|---|---|---|---|---|
| **Claude Code** | 工具/前缀/参数三档;Bash 按仓库+命令持久,文件编辑仅会话级 | 项目 `.claude/settings.local.json`(git-ignored) | 项目共享 settings 的 allow 需过 workspace trust;deny 任何层级不可被推翻;hooks 只紧不松 | allow 规则免弹窗;`auto` 模式后台安全检查;deny→ask→allow 固定序 | 规则匹配承认自身不是 OS 边界,越界执法明确指向 sandbox |
| **Codex CLI** | 命令级(trusted 集)+ 项目级 trust_level | 用户 `~/.codex/config.toml` | untrusted 项目不加载项目级 `.codex/` 层;沙箱内 `.git`/`.codex` 只读 | 沙箱吃掉无害操作,仅越界/失败时提权询问 | 二维正交:sandbox_mode × approval_policy;批准 = 单条命令沙箱外重跑 |
| **Cursor** | 命令 allowlist + 三个保护开关 | `~/.cursor/permissions.json` + 项目 `.cursor/permissions.json` | 无(官方自认 best-effort) | Auto-review:allowlist→沙箱→classifier 三级分流 | 沙箱是分流的中间层;classifier 明示"不是安全边界" |
| **Aider** | 几乎无门(shell 建议有确认) | — | — | 根本不设门,靠 git 事后回滚 | 无沙箱;安全网只覆盖 git 可回滚的域 |
| **OpenHands** | 可选 policy(Always/Never/ConfirmRisky) | config.toml(默认 `confirmation_mode = false`) | — | LLM analyzer 自评风险分级弹窗 | sandbox-first:Docker 兜底后默认零确认 |
| **Emacs local vars** | name+value 整对(`!`)/ 目录(`+`)/ 负面记忆(`i`) | custom-file(人类可读 elisp) | 无(依赖 custom-file 本身可信);批准列表变量标 `:risky` 防文件注入 | safe 谓词让良性值免审;`!`/`i` 对称记忆 | 无沙箱,门是唯一防线 |
| **Emacs themes** | **文件全文 SHA-256** | custom-file(`custom-safe-themes`) | 同上;hash 使记录与内容绑定,改文件即失效 | hash 命中静默加载,零打扰 | 无沙箱;确认前强制展示源码 buffer |
| **Emacs 30 trusted-content** | 文件/目录 | init 文件(手工维护,无交互流程) | truename 防符号链接;`untrusted-content` 一票否决;禁止 mode 代设 | 不可信时静默降级功能(flymake/宏展开拒跑)而非弹窗 | 无沙箱(源码注释两处 FIXME 提到希望用 bwrap) |
| **VSCode** | 文件夹 / 父文件夹递归 | 用户全局 application storage(工作区外) | 信任记录放在被信任对象够不着的地方 | Restricted Mode 静默降级能力而非反复弹窗 | 无沙箱;信任门是能力总闸 |

### 9.3 几条跨家共识

1. **黑名单不可防守,白名单才可防守**:Cursor 移除 denylist;Claude Code 警告参数模式脆弱、deny 优先且不可被推翻;Emacs 的 safe 谓词 + risky 启发式本质是白名单。
2. **批准记录放在被批准对象写不到的地方**:VSCode 全局 storage、Codex 的 `.git`/`.codex` 沙箱内只读、Claude Code 对项目 settings 的 trust 门、Emacs 把批准列表标 `:risky` 防局部变量注入。superchat 值得逐条对照。
3. **对抗确认疲劳的正道是"缩小需要确认的集合",不是"记住更粗的批准"**:Codex/OpenHands 用沙箱、Cursor 用三级分流、Emacs 用 safe 谓词与 hash 静默命中。
4. **每家都保留一个"永远弹窗"的档位**,且不可被任何 allow 覆盖(Claude Code `requiresUserInteraction`、Emacs risky 变量无 `!`)。
5. **自我声明不可信**:MCP 规范 "annotations... untrusted";OpenHands 的 LLM 自评风险是刻意标为可选的例外。

## 10. 对 superchat Phase 2 的启示

### 10.1 superchat 的处境

Emacs 进程内执行、**无沙箱可退**(连 Emacs 自己的源码都在 FIXME 里想要 bwrap)、合成工具要跨会话持久化。这意味着 Codex/OpenHands 的"沙箱吃掉确认"路线整条不可用,superchat 只能走 Emacs/Claude Code 这一支:**门 + 记忆 + 记录不可被对象自身改写**。

### 10.2 候选设计对照先例

候选:**定义即批准 + body SHA-256 锚定 + 启动静默再水化(hash 不符拒载)+ 每次调用确认保留**。

**最契合的先例是 `custom-safe-themes`,且逐点同构**(见 §7.3):

| 候选设计要素 | custom-safe-themes 对应 | 契合度 |
|---|---|---|
| 定义时人审批准 | `custom-theme-load-confirm` 两问 + 强制 pop 源码 buffer | 完全同构;**批准前展示全文是流程的一部分**,superchat 的代码预览 buffer(homoiconic 决策 #8)正好对上 |
| body SHA-256 锚定 | `(secure-hash 'sha256 <文件全文>)` 存入列表 | 完全同构;注意 Emacs hash 的是**载体文件全文**,superchat 若只 hash body 而不含 name/description/args-schema,见下"差异 2" |
| 启动静默再水化 | hash 命中 `member` 即 `eval-buffer`,零提示 | 完全同构;这正是 Emacs 让 init 里 `load-theme` 不烦人的机制 |
| hash 不符拒载(降级回门) | hash 失配即重新走 `custom-theme-load-confirm` | Emacs 是"降级为重新确认",候选是"拒载"——**拒载更保守,站得住**(Phase 2 启动时人不一定在场,不能弹窗,拒载 + 提示用 `M-x superchat-tools-review-synthesized` 重审是正确的无人值守语义) |
| 每次调用确认保留 | 无对应(theme 加载即生效) | 先例来自另一处:Claude Code `requiresUserInteraction`("每次弹窗,任何 allow 不能免,不提供 don't ask again")与 Emacs risky 变量无 `!` 通道。**这一条是候选设计比 custom-safe-themes 多出的一道门,恰好补上"参数依赖行为"缺口**(homoiconic 决策 #1:`(delete-directory path t)` 审阅时无害,`path="/"` 调用时才危险)——hash 锚定管"代码没变",调用确认管"这次的参数",两者正交,缺一不可 |

`safe-local-variable-values` 的 name+value 先例则提示另一半:Emacs 记住的是**名字和值的整对**。superchat 的对应物是"工具名 + 完整定义"——所以:

### 10.3 相对先例的差异与风险

1. **"定义即批准"折叠了 Emacs 的两问为一问**。`custom-theme-load-confirm` 把"这次加载吗"和"以后永远信吗"分开问。定义即批准等于默认第二问为 yes。可接受的理由:定义时的确认门本来就带完整代码预览,且每次调用确认保留着;但建议在定义确认的 UI 文案里**明说"批准即随本会话持久化,内容不变则跨会话静默载入"**,让用户知道自己在签什么。更保守的变体是保留两问(批准使用 / 批准持久化),成本只是一次 y/n。
2. **hash 的覆盖范围必须是完整定义,不只是 body**。Emacs hash 的是主题文件全文。若 superchat 只锚 body,攻击面是:再水化时 description/args-schema 被改(description 会进模型上下文,是提示注入载体;schema 变化可改变参数校验)。应对:SHA-256 计算在 name+description+args-schema+body 的规范化序列化之上——等价于"hash 载体文件全文"的先例语义。
3. **批准记录的防篡改比各家先例更严峻**。Emacs 的 hash 列表存 custom-file,威胁模型里"能改 custom-file 的人已经赢了"。superchat 的特殊性:**agent 自己有写文件的工具**。若批准记录(hash 清单)与 `synthesized-tools.el` 同在 data-dir 且 agent 可写,agent 可以"改 body + 改 hash 清单"一步完成自我放行。对策(对应 §9.3 共识 2):(a) 把写入批准清单的路径列入文件工具的 deny 名单(Codex 对 `.codex` 只读的先例);(b) 批准清单的写入只经确认门后的受信代码路径,永不暴露为工具可达;(c) tape 记录每次批准与再水化(MCP 规范 "Log tool usage for audit purposes")。**这是候选设计文本里没写、但先例一致要求的一条,建议补进 Phase 2 规格。**
4. **静默再水化与 goals 文档现行表述冲突,需明确改版**。`docs/goals/homoiconic-agent.md` 的 Phase 2 写的是"启动时不自动加载;`M-x superchat-tools-review-synthesized` 逐个过目"。候选设计把"人过目"移到定义时、用 hash 保证"载入的就是过目过的"。这在先例上站得住(custom-safe-themes 正是如此),而且体验显著更好("越用越强"不需要每次重启重新批准);但应把 goals 文档的"再水化必须人过目"改写为"再水化必须**与人过目过的内容逐字节一致**,否则拒载",避免规格自相矛盾。
5. **无负面记忆**。Emacs 的 `i`(ignored-local-variable-values)提供"永远拒绝"的对称通道。superchat 候选设计里用户拒绝一个合成工具后,agent 下回合可以原样重新 define。低成本补法:拒绝时把 hash 记入拒绝清单,同 hash 的 define_tool 直接拒绝不再弹窗(防 agent 磨人)。
6. **每次调用确认的疲劳风险是真实的,但先例支持"保守起步"**。业界缓解路径可供 Phase 3+ 借鉴而非现在做:Claude Code 允许把确认粒度细分到参数模式(deny/ask 支持 `Tool(param:value)`);Emacs 的 safe 谓词模式提示未来可以给合成工具附"参数安全谓词"(纯函数白名单校验参数)再谈免确认。**不建议现在引入任何"记住参数模式"机制**——那等于重蹈 Cursor denylist 的覆辙。

### 10.4 结论

候选设计与业界先例高度契合,核心三件套各有直接先例:**内容寻址批准 = custom-safe-themes(SHA-256 全文,命中静默、失配降级)**;**每次调用确认不可豁免 = Claude Code requiresUserInteraction / Emacs risky 无 `!`**;**定义时强制代码预览 = custom-theme-load-confirm 的 pop buffer**。没有发现更好的替代模式——沙箱路线不可用,前缀/模式记忆路线被 Cursor/Claude Code 的文档亲证脆弱。需要修的是四处边缘:hash 覆盖完整定义而非仅 body;批准清单对 agent 工具不可写 + tape 审计;拒绝也留 hash(负面记忆);定义确认文案明示持久化含义(或保留 Emacs 式两问)。
