# superchat

superchat 是一个运行在 Emacs 内部的 LLM 聊天界面，基于钩子管线 (hook pipeline) 构建——记忆、技能、工作流、工具调用，每一项功能都以统一形状的插件形式注册到同一套管线中。如果你的目标是：在编辑器里拥有一个 Claude Code 风格的聊天界面，同时又希望在不 fork 项目的前提下编写自己的上下文构建器与后处理钩子，这就是为你准备的。

<!-- TODO: screenshot of @model switching -->

## 为什么选择 superchat

如果你已经安装了 gptel、ellama 或 chatgpt-shell，以下是 superchat 的增量：

| 功能                                  | gptel | ellama | chatgpt-shell | superchat |
|---------------------------------------|:-----:|:------:|:-------------:|:---------:|
| 通过 `llm.el` 支持多 provider          | ✓     | ✓      | (自有)         | ✓         |
| 流式响应                               | ✓     | ✓      | ✓             | ✓         |
| MCP 工具                               | (第三方) | ✗   | ✗             | ✓         |
| 对话中 `@model` 切换语法               | ✗     | ✗      | ✗             | **✓**     |
| `#file` 文件引用语法                   | (选区) | (选区)  | ✗            | **✓**     |
| SKILL.md 技能格式（frontmatter + body）| ✗     | ✗      | ✗             | **✓**     |
| type: workflow 多步骤工作流            | ✗     | ✗      | ✗             | **✓**     |
| SQLite + FTS5 持久化记忆               | ✗     | ✗      | ✗             | **✓**     |
| 基于钩子的扩展模型                      | ✗     | ✗      | ✗             | **✓**     |

选择 superchat，当你需要：
- 用 `@gpt-4o` 或 `@claude-sonnet` 在对话中随时切换模型。
- 用 `#path/to/file.el` 将文件内容内联到 prompt 中。
- 把可复用的提示词写成带 YAML frontmatter 的 SKILL.md 文件。
- 用 `/recall` 搜索过去的对话，并在每次提问时自动附加相关记忆。
- 通过 `(add-hook)` 扩展 prompt 管线——无需 fork，无需 monkey-patch。

选择 gptel 或 ellama，当你的需求更简单：单模型、单 provider、无需记忆或工作流的一次性对话。

## 安装

**依赖：** Emacs 28.1 或更高版本，[llm.el](https://github.com/ahyatt/llm) ≥ 0.24（GNU ELPA）。

superchat 尚未上架 MELPA——从源码安装：

```elisp
(add-to-list 'load-path "/path/to/superchat")
(require 'superchat)
```

配置 provider。superchat 支持 `llm.el` 所支持的所有后端：

```elisp
;; OpenAI
(setq superchat-llm-backend
      (make-llm-openai
       :key (getenv "OPENAI_API_KEY")
       :chat-model "gpt-4o-mini"))

;; Anthropic
;; (setq superchat-llm-backend
;;       (make-llm-claude
;;        :key (getenv "ANTHROPIC_API_KEY")
;;        :chat-model "claude-3-5-sonnet-20241022"))

;; Ollama
;; (setq superchat-llm-backend
;;       (make-llm-ollama
;;        :chat-model "llama3.1"))
```

可选：[mcp.el](https://github.com/lizqwerscott/mcp.el)，用于 MCP 服务器工具集成。

## 5 分钟上手

使用 `M-x superchat` 打开聊天界面。以下是你能在首次会话中做的五件事：

**1. 普通流式对话**

```
User: 用三句话解释什么是 Zettelkasten
Assistant: Zettelkasten 是一种个人知识管理方法…
```

**2. 对话中切换模型**

```
User: @gpt-4o 解释 SQLite FTS5 的工作原理
```

`@model` 切换仅对当前这一轮生效，下一条消息自动恢复默认模型。

**3. 附加文件作为上下文**

```
User: #src/main.py 这个函数做了什么？
```

文件内容会被内联到 prompt 中。superchat 会从你的输入中剥离 `#路径`，模型看到的是"这个函数做了什么？"加上文件内容。

**4. 记忆与回溯**

```
User: /remember LLM 的 temperature 控制随机性；0 是确定性，1 是创造性。
系统: Memory added: LLM 的 temperature 控制随机性

User: /recall temperature
系统: Retrieved 1 memory — will be attached to your next message.

User: 代码生成应该怎么设置 temperature？
```

记忆使用 SQLite FTS5 检索，当你的查询包含相关词汇时，记忆会自动附加到对话上下文中。

**5. 调用技能**

```
User: >code-review #src/utils.el
```

预制的技能文件存放在 `skills/*.md`。内置技能包括 `code-review`、`planning`、`refactor`。编写自己的技能请参考 [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md)。

## 核心概念

### 技能（SKILL.md）

技能是可复用的系统提示词，以带 YAML frontmatter 的 Markdown 文件存储。放在 `skills/*.md` 中，通过 `>skill-name` 调用。

`type: prompt`（默认类型）会将 body 作为当轮的系统提示词注入：

```markdown
---
name: explain-region
description: 向刚接触这个代码库的资深工程师解释一段代码
version: "1.0"
type: prompt
triggers: ["explain this", "这段代码什么意思"]
---

你是一位资深工程师，正在向同事解释代码。关注以下层面：
1. 这段代码实际做了什么（而非它"应该"做什么）
2. 为什么要这样组织
3. 有哪些不明显的边界情况
```

`type: workflow` 将每一行非空非注释内容作为独立的一个步骤，依次通过同一套管线执行（见下方"工作流"）。

字段规则：`name`、`description` 必填。`version` 默认 `"1.0"`。`type` 默认 `"prompt"`（有效值：`"prompt"`、`"workflow"`）。`triggers` 默认 `[]`。

更完整的示例见 `examples/standard-skills/`，快速入门见 [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md)。

### 工作流（type: workflow）

工作流是多步骤技能，每一行作为独立的一轮通过同一套钩子管线。适用于需要串联搜索、分析、保存等步骤的任务。

```markdown
---
name: git-commit-message
description: 从 git diff 生成 conventional commit 格式的提交信息
version: "1.0"
type: workflow
---

# 步骤 1：读取 diff。# 开头是注释，空行被跳过。
/web-search conventional commit 格式规范

# 步骤 2：用 conventional commit 格式撰写提交信息
@claude-3-5-sonnet-20241022 根据上面的 diff 和格式规范，写一条 conventional commit 格式的提交信息。摘要行不超过 72 个字符。
```

每一步同样会解析 `@model`、`>skill`、`/command`、`#file`，和用户手动输入的行为一致。步骤按顺序执行，不支持分支或条件——工作流是线性脚本。

### 记忆

记忆系统基于 SQLite 数据库，使用 FTS5 全文搜索。两个命令控制它：

- `/remember [文本]` — 捕获文本（不带参数时捕获上一轮对话）。
- `/recall <关键词>` — 搜索记忆，结果附加到下一轮对话中。

当你的查询足够长时（可通过 `superchat-memory-auto-recall-min-length` 配置，默认 8 个字符），记忆会在每一轮自动检索。检索结果由 `superchat-prompt-hook--memory-context` 钩子（`superchat-prompt-hooks.el:132`）注入到 prompt 中。

数据库文件位于 `~/.emacs.d/superchat/memory.db`（即 `superchat-data-directory` 目录下）。你可以用任何 SQLite 客户端查看：

```bash
sqlite3 ~/.emacs.d/superchat/memory.db "SELECT title FROM memories LIMIT 5"
```

记忆调优在 `M-x customize-group RET superchat-memory RET` 中。

### MCP

superchat 集成 [mcp.el](https://github.com/lizqwerscott/mcp.el)。通过 `mcp-hub-servers` 配置服务器，用 `/mcp-start` 启动，用 `/mcp` 查看状态。

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem"
                               "/Users/you/Documents")))))
```

MCP 工具会自动收集，并与内置工具一起传递给 `llm-chat` / `llm-chat-streaming`。

### 钩子管线（高级）

每一轮对话都经过 `superchat-core-run-turn`（`superchat-core.el:118`）：

```
解析 (@model, >skill, /command, #file)
  → system-prompt 钩子
    → build-prompt 钩子
      → post-turn 钩子（副作用）
```

每个钩子是一个函数 `(turn) → modified-turn or nil`。turn 结构体（`superchat-core.el:22`）承载了当轮的全部状态：`clean-input`、`system-prompt`、`prompt`、`retrieved-memories`、`context-files` 等。

编写你自己的 prompt 构建器：

```elisp
(defun my-git-blame-hook (turn)
  "将当前 buffer 的 git-blame 信息注入到 prompt 中。"
  (when-let ((buf (current-buffer))
             (file (buffer-file-name buf))
             (blame (shell-command-to-string
                     (format "git blame -L 1,20 -- %s" file))))
    (setf (superchat-turn-prompt turn)
          (concat "Git blame 上下文：\n" blame "\n\n"
                  (superchat-turn-prompt turn))))
  turn)

(add-hook 'superchat-build-prompt-functions #'my-git-blame-hook)
```

这正是 superchat 区别于其他 Emacs LLM 客户端的架构核心：功能即钩子，你可以自己写。

详见 [docs/architecture.md](docs/architecture.md)。

## 自定义选项

最常用的 defcustom（全部选项见 `M-x customize-group RET superchat RET`）：

- `superchat-llm-backend` — llm.el 的 provider struct。**必须**在 `M-x superchat` 之前设置。
- `superchat-llm-model` — 可选的 `:chat-model` 覆盖；被 `@model` 切换使用。
- `superchat-llm-streaming` — `t` 为流式（默认），`nil` 为阻塞模式。
- `superchat-llm-tool-names` — 内置工具白名单。默认只暴露小型只读集；设 `'all` 为全量。
- `superchat-data-directory` — 记忆、配置、命令的存储路径（默认 `~/.emacs.d/superchat/`）。
- `superchat-lang` — 自定义命令中 `$lang` 变量的语言（默认 `"English"`）。
- `superchat-response-timeout` — 响应卡死的超时秒数（默认 120）。
- `superchat-context-message-count` — 注入 prompt 的近期消息数量。
- `superchat-conversation-history-limit` — 缓冲区中保留的最大消息数。
- `superchat-display-single-window` — 将 Emacs frame 专用于 superchat（默认 `t`）。

## 配置速查

```elisp
;; 最小可用配置
(setq superchat-llm-backend
      (make-llm-openai
       :key (getenv "OPENAI_API_KEY")
       :chat-model "gpt-4o-mini"))

;; 数据目录（默认为 ~/.emacs.d/superchat/）
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; 自定义命令中 $lang 的语言
(setq superchat-lang "中文")

;; 历史规模
(setq superchat-context-message-count 10)
(setq superchat-conversation-history-limit 50)
```

## 故障排除

**提示 "superchat-llm-backend is unconfigured"**

在打开聊天之前，将 provider struct 赋值给 `superchat-llm-backend`。运行 `/backend` 确认配置。

**`@model` 提示"模型未找到"**

运行 `/models` 查看可用模型。用 `/refresh-models` 从后端重新加载。或通过 `(setq superchat-manual-models '("model-id-1" "model-id-2"))` 手动补充。

**工具调用超时**

内置工具（网页抓取、文件系统）和 MCP 工具的超时倍率为 `superchat-tool-timeout-multiplier`（默认 1.5），作用于 `superchat-response-timeout`（默认 120s），即工具总超时上限为 180s。一旦超时，superchat 会中断当前轮次。本地模型（Ollama）最容易遇到此问题，因为 LLM 生成工具调用 JSON 本身就需要时间。调高两项：`(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`。将 `superchat-show-ttft` 设为 `t` 可在回显区显示首 token 延迟，帮助你判断瓶颈在 LLM 端还是工具端。

**记忆不能持久化**

检查 `superchat-data-directory` 以及 SQLite 数据库是否可写。运行 `/recall test` 验证查询是否正常。记忆自动检索仅在查询超过 8 个字符时激活（可通过 `superchat-memory-auto-recall-min-length` 配置）。

**AI 回复后没有出现 "User:" 提示符**

通常是阻塞式工具调用导致。superchat 会在 `superchat-response-timeout`（120s）后自动恢复。如果频繁发生，检查你的 MCP 服务器或增大超时值。

## 项目状态与路线图

v1.0.1 已发布。稳定功能：聊天、流式、模型切换、技能、工作流、记忆、MCP、工具调用、钩子管线。实验性功能：主管线中没有临时性功能，但多轮 agent 循环不在 v1.x 范围内。规划中的工作见 [ROADMAP.md](ROADMAP.md)。

## 许可证

GPL-3.0-or-later。

## 背景

superchat 最初源自 [org-supertag](https://github.com/yibie/org-supertag) 项目的 chat-view 模块。自 v0.1 起已完全独立，不携带任何 org-supertag 依赖。
