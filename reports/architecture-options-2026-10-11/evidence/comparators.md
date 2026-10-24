# 同类工具的一手资料（对照用）

读取日期：2026-10-11。只记页面上写明的事实；"未写明"就是页面没有说。来源分两类：项目自己的 README / 官方文档，和第三方（论坛、博客，标出）。本研究没有安装或运行其中任何一个。

## gptel（Emacs）

来源：<https://github.com/karthink/gptel>（README，读于 2026-10-11）

- 预设用 `gptel-make-preset` 定义，"takes a name and keyword-value pairs of settings"；键没有固定清单，`:foo` 设 `gptel-foo`，例子里有 `:backend`、`:model`、`:system`、`:tools`、`:use-context`、`:context`、`:temperature`。
- 在提示正文里写 `@preset-name`，只对"a _single_ query"生效，"can be anywhere in the prompt"，只看"the final user turn"。菜单里另有 `oneshot` 作用域做同一件事。
- 工具：模型发出调用参数后"gptel uses to run the tool in your Emacs session"；可以在工具菜单里"require confirmation for all tool calls"（默认是否确认，页面未写明）。
- 去向：默认"The response will be inserted below"；`gptel-send` 菜单可以把回答重定向到回显区、某个会话、任意 buffer 或 kill ring。
- 检查请求："Pick one of the 'dry run' options in the menu to produce a buffer containing the request payload"，可以改了再发。
- 停止请求：README 正文未写明按键。
- 小模型："All the flagship models support tool use, as do many of the smaller open models"；没有对小模型表现的断言。

## agent-shell（Emacs，ACP 客户端）

来源：<https://github.com/xenodium/agent-shell>（README，读于 2026-10-11）

- "A native Emacs shell to interact with LLM agents powered by ACP"；驱动 Claude Agent、Codex、Gemini CLI、Goose、Cursor 等各自的 CLI。
- 状态：header 和 mode line 有忙碌指示；工具调用和思考段可折叠；可开成本和上下文用量指示。
- 中断：`C-c C-c` "Interrupt current agent operation"；`agent-shell-interrupt` 中断当前请求并**拒绝所有挂起的权限请求**；取消一轮时询问是否继续排队的提示；支持 steering。
- 权限：有"permission icon"和在权限按钮之间移动的命令；用 TAB / Shift-TAB 在可交互元素间移动。问题显示在 buffer 里还是小缓冲区，页面未明说。
- 多会话：`C-u M-x agent-shell` 开新会话；header 可显示 session id。
- 改动：有 diff buffer 模式，整体接受/拒绝；逐块接受页面未提。

## eca-emacs（Emacs，外部 eca 服务器）

来源：<https://github.com/editor-code-assistant/eca-emacs>（README，读于 2026-10-11）

- 待批准的工具调用显示在聊天 buffer 里，也显示在 `eca-workspaces` 仪表盘（"pending approval"）。聊天里 `C-c C-a` 接受全部、`C-c C-S-a` 接受下一个、`C-c C-s` 接受并记住本会话、`C-c C-r` 拒绝下一个；仪表盘 `a`/`A`/`x`；就地浮层 `a`/`d`。
- 上下文：正文里输入 `@` 补全附加上下文；可自动附 repo map、光标位置。
- 停止：`eca-chat-stop-prompt`；仪表盘 `s`；就地浮层 `s`。
- 多会话：有列出"all workspaces and chats"的仪表盘；聊天可恢复、改名、分叉。
- 改动：`eca-chat-diff-tool` 选 smerge 或 ediff；编辑完成后自动 revert 文件（有未保存改动的 buffer 不 revert）。聊天里的文件编辑能否事后拒绝，页面未写明。

## aider（终端）

来源：<https://aider.chat/docs/usage/modes.html>、<https://aider.chat/docs/usage/commands.html>（读于 2026-10-11）

- 四种模式：code（默认）、ask（"never make changes"）、architect、help。
- 单条消息前缀 `/code`、`/architect`、`/ask`、`/help` "applies just to that particular message"，下一条回到当前模式；`/chat-mode <mode>` 切换常驻模式；提示符随模式变（`>`、`ask>`、`architect>`）。
- architect 模式：主模型提方案，第二个 editor 模型把方案变成具体文件编辑；"Certain LLMs aren't able to propose coding solutions *and* specify detailed file edits all in one go"。
- 中断：Control-C "always safe"，部分回答留在对话里。
- `/undo`："Undo the last git commit if it was done by aider"；`/diff` 显示上一条消息以来的差异。

## Claude Code（终端）

来源：<https://code.claude.com/docs/en/permission-modes.md>（官方，读于 2026-10-11）

- 权限模式：`default`（只读不问）、`acceptEdits`（读、文件编辑和常见文件系统命令不问）、`plan`、`auto`（"Everything, with background safety checks"，由分类器模型代替人审）、`dontAsk`（"anything that would prompt is denied"，给 CI 和脚本）、`bypassPermissions`（只在隔离容器/VM 里）。
- 切换：`Shift+Tab` 循环；状态栏显示当前模式。
- 无人值守："Run in CI with an exact allowlist" 用 `--permission-mode dontAsk`；完全无人值守用 `--dangerously-skip-permissions` 且要求容器/VM。
- 后台任务列表（`/tasks`、Ctrl+B 等）：这次只在第三方指南里见到，官方页面未核对，**不作依据**。

第三方（仅作线索，不作依据）：<https://backgroundclaude.com/blog/permission-modes>（说默认模式在无人看守时会停住）。

## Cursor（IDE）

来源：<https://cursor.com/docs/background-agent>（官方，读于 2026-10-11；页面说该功能现名 Cloud Agents，"formerly called Background Agents"）

- 云端隔离 VM 里运行，"do not require your local machine to be connected to the internet"。
- 交付："merge-ready PRs" 加截图/视频/日志；可以接管远程桌面测试；仪表盘显示运行、对话、改动、产物。
- 接受/拒绝改动的具体界面，页面未写明。

第三方（论坛，仅作线索）：<https://forum.cursor.com/t/how-to-navigate-to-the-changes-made-in-chat-to-accept-or-reject-them/145639>、<https://forum.cursor.com/t/review-of-changes-done-in-agent-chat-window-disappears-after-cursor-restart/163021>：用户报告"Review panel"按文件接受/拒绝、Keep All / Undo All；重启后待审清单消失。

## 对照里能用的点（我的归纳，不是来源的话）

1. **一次性修饰符**：gptel 的 `@preset` 一次一问、aider 的 `/ask` 前缀一条一变，都是"写在这一条上"的做法；Superchat 的 `>技能`（正文中）和 `>annotate` 已经是同一类。
2. **挂起的审批有集中的地方**：eca 把待批准的调用同时放在聊天和仪表盘，agent-shell 的中断会一并拒绝挂起的权限；Superchat 的审批只在小缓冲区里，没有列表。
3. **中断后留下什么**：aider 把部分回答留在对话里；agent-shell 问要不要继续排队的提示。
4. **无人值守要显式降级**：Claude Code 要求 `dontAsk` 或容器；Superchat 的被唤醒执行用默认级别，`full-control` 调用会问一个可能不在的人（architecture.md "A woken record's new conversation has no recorded permission level"）。
5. **弱模型分工**：aider 的 architect 模式把"想方案"和"写编辑"分给两个模型，理由正是有的模型做不到一步到位。
