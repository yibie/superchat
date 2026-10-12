# superchat

superchat 是一个运行在 Emacs 内部的 LLM 聊天界面，以共享 Runtime 和经 Scope 管理的插件组织模型传输、上下文收集、工具与生命周期监听。插件由一份组合列表挂载；部分产品功能共用插件，而不是各自拥有独立生命周期。如果你希望在编辑器里拥有一个 Claude Code 风格的聊天界面，同时能在不 fork 项目的前提下扩展它，这就是为你准备的。

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
| 独立 `.workflow` 文件的异步多步骤工作流 | ✗     | ✗      | ✗             | **✓**     |
| SQLite + FTS5 持久化记忆               | ✗     | ✗      | ✗             | **✓**     |
| 插件模型（Service / Stage / Event）     | ✗     | ✗      | ✗             | **✓**     |

选择 superchat，当你需要：
- 用 `@gpt-4o` 或 `@claude-sonnet` 在对话中随时切换模型。
- 用首部的 `#/绝对路径/file.el` 或 `#~/path/to/file.el` 将文件内容内联到 prompt 中。
- 把可复用的提示词写成带 YAML frontmatter 的 SKILL.md 文件。
- 用 `/recall` 搜索过去的对话，并在每次提问时自动附加相关记忆。
- 用一个 `superchat-define-plugin` 注册 prompt 收集器、工具或 Event 监听器来扩展——无需 fork，无需 monkey-patch，卸载即撤回。

选择 gptel 或 ellama，当你的需求更简单：单模型、单 provider、无需记忆或工作流的一次性对话。

## 安装

**依赖：** Emacs 30.1 或更高版本，[llm.el](https://github.com/ahyatt/llm) ≥ 0.31.1（GNU ELPA）。

用 Emacs 自带的 `package.el` 从 GNU ELPA 安装 `llm.el` 及其依赖：

```sh
emacs --batch \
  --eval "(require 'package)" \
  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
  --eval "(package-initialize)" \
  --eval "(package-refresh-contents)" \
  --eval "(package-install 'llm)"
```

在干净的 HOME 中运行时，实测安装了 `llm` 0.32.0，以及 `plz`、
`plz-event-source`、`plz-media-type` 等依赖。

superchat 尚未上架 MELPA——从源码安装：

```elisp
(require 'package)
(package-initialize)
(add-to-list 'load-path "/path/to/superchat")
(require 'llm-openai)
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

;; Ollama 原生后端
;; (setq superchat-llm-backend
;;       (make-llm-ollama
;;        :chat-model "llama3.1"))

;; LM Studio，或已启用 OpenAI-compatible API 的 Ollama
;; (setq superchat-llm-backend
;;       (make-llm-openai-compatible
;;        :key "lm-studio"
;;        :chat-model "qwen/qwen3.8-27b"
;;        :url "http://localhost:1234/v1/"))
```

对 Ollama，使用同一个 `make-llm-openai-compatible` 形式，并将 URL 和模型
替换成它的 OpenAI-compatible 端点所暴露的值（常见 URL 是
`http://localhost:11434/v1/`）。发送第一条消息前必须设置
`superchat-llm-backend`。如果它仍为 `nil`，`M-x superchat` 仍可打开聊天
buffer，但发送第一条消息时请求会被拒绝，并在发送时报告配置错误。

可选：[mcp.el](https://github.com/lizqwerscott/mcp.el)，用于 MCP 服务器工具集成。

## 5 分钟上手

使用 `M-x superchat` 打开聊天界面。它出现在当前 tab 右侧的侧窗口里，挨着你正在编辑的文件（编辑模式），原有的窗口都还在，只是变窄一些。在聊天里按 `C-c C-w`，这段对话就移进它自己的 tab 并占满（agent 模式）；再按一次回到你刚才那个文件旁边，没发出去的字还在。同一次 Emacs 里，之后打开聊天用你最后切换到的模式；`superchat-display-style`（`editor` 或 `agent`）决定一开始用哪种。关闭一段对话：在它的聊天窗口里 `M-x superchat-close-session-workspace`，agent 模式下关掉它的 tab 也行；还有 Run 在跑时会先问你。

以下是你能在首次会话中做的五件事：

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
User: #~/src/main.py 这个函数做了什么？
```

文件内容会被内联到 prompt 中。superchat 会从你的输入中剥离 `#路径`，模型看到的是"这个函数做了什么？"加上文件内容。

`@模型`、`>技能`、`/命令` 和 `#文件` 可以写在输入开头并在那里组合，例如 `@gpt-4o >code-review #~/src/main.py 审查这个文件`。`@模型`、`>技能` 和 `/命令` 也可以写在你输入的消息中间——`请 >code-review 看看这个`、`解释一下 @gpt-4o 这段`——但写在中间时只认已有的名字：存在的技能、你手动名单里或已经取到过的模型、存在的命令。标记必须独立成词，前面是空白或开头，后面是空白或结尾；模型收到的文字里不含它。其它写法（`a > b`、`> 一行引用`、`name@example.com`、`/usr/bin`、反引号里的名字）都是普通文字。`agent` 类型的技能写在开头会成为这段对话的模式；写在中间只作用于这一条消息，对话保持原样（`plan` 类型写在哪里都切换对话）。每种最多认一个，开头的优先：`/summarize` 之后的 `>技能` 是普通文字。Superchat 自己发出的消息（定时任务、被唤醒的工作记录）只认开头。`#文件` 只认开头，后面或末尾的 `#文件` 按正文保留。未加引号的文件路径必须是绝对路径（`#/...`）或 home 相对路径（`#~/...`）。

**4. 记忆与回溯**

```
User: /remember LLM 的 temperature 控制随机性；0 是确定性，1 是创造性。
回显区：Anchor added: LLM 的 temperature 控制随机性；0 是确定性，1 是创造性。 (tape ID: <id>)

User: /recall temperature
回显区：Retrieved 1 entries — will be attached to your next message.

User: 代码生成应该怎么设置 temperature？
```

`/remember` 和 `/recall` 的确认显示在 Emacs 回显区，而不是聊天 buffer
中的 `System` 条目。记忆使用 SQLite FTS5 检索，当你的查询包含相关词汇时，
记忆会自动附加到对话上下文中。

**5. 调用技能**

```
User: >code-review #src/utils.el
```

预制的技能文件存放在 `skills/*.md`。内置技能有 `code-review`、`planning`、`refactor` 和 `render-ui`（`render_ui` 工具的生成式界面语法）。和所有技能一样，它们通过 Preset 或 `>名字` 使用。编写自己的技能请参考 [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md)。

在 prompt 中输入 `/` 会打开实时命令补全，候选来自实际用于分发的内置和已注册命令，例如 `/workflow`、`/code`、`/research`、`/cowork`、`/single`、`/permissions`。内置恢复复查命令不提供；只读报表 `M-x superchat-experience-monitor` 与 Experience 恢复路径保留。精确删除范围和旧记录保留规则见 [M3a 决策](docs/decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md)。

## 核心概念

### 技能（SKILL.md）

技能是可复用的系统提示词，以带 YAML frontmatter 的 Markdown 文件存储。放在 `skills/*.md` 中，通过 `>skill-name` 调用。

**模型会自己发现 skill。** 只要请求里有 `read_file` 工具，就会带上一份 skill 目录：每个 skill 的名字、一句 `description` 和文件位置，格式与 Pi 相同。任务和某条描述对得上，或者你在话里点名某个 skill（比如“用 denote-journal 的 skill”），模型就会用 `read_file` 读这份 skill；skill 里用相对路径提到的文件，用到时再读。目录收录 skills 文件夹里任意深度的 `SKILL.md` 文件夹（Pi、Claude Code、Codex 共用的标准格式），以及带 `description` 的单个 `.md`/`.org` skill；`superchat-skills-extra-directories` 可以再加别的文件夹（比如 `~/.agents/skills`）。模式类 skill（`type: agent` 或 `plan`）和标了 `disable-model-invocation: true` 的，仍只能用 `>名字` 调用。`>名字` 也能找到 `名字/SKILL.md` 这样的文件夹。

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

`type: plan` 会启用只读的规划模式。多步骤配方使用独立的 `.workflow` 文件，
而不是 skill 类型（见下方"工作流"）。

`type: agent` 表示 Agent Profile；用独立于可变 `name` 的 `profile_id`
声明稳定历史身份，并用 `capability` 显式声明工作能力，例如
`profile_id: profile.coder` 与 `capability: coding`。

字段规则：`name`、`description` 必填。`version` 默认 `"1.0"`。`type` 默认 `"prompt"`。Agent Profile 应声明不可变的 `profile_id` 和显式默认 `capability`；修改 `name` 不影响历史身份，修改 `profile_id` 则表示创建新的 Profile。Request 可以省略或重复选择该 Capability，但冲突选择会在创建 Run 前被 Runtime 拒绝。旧 Profile 会告警，并以 `name` 作为兼容身份；未选择 Capability 时 Runtime 使用 `chat`，不会从 Profile 名称推导。`triggers` 默认 `[]`。

更完整的示例见 `examples/standard-skills/`，快速入门见 [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md)。

### 工作流（`.workflow` 文件）

工作流是放在 `<superchat-data-directory>/workflow/` 下的独立 `.workflow`
文件，每个非空、非注释行是一个步骤。步骤通过**异步回调链**顺序执行，
整个运行期间 Emacs 保持响应。使用 `>>name [args]`；第一次使用 `>>` 就会
自动加载入口并立即执行（`>` 保留给技能，`>>` 保留给工作流），也可以使用
`/workflow <name> [args]`。工作流名必须是当前目录的 basename；系统枚举实际
存在的 `.workflow` basename 后按 NFC 规范形态比较，因此 NFC/NFD 两种写法会
解析到同一个文件。`@model` 会先被模型切换解析消费，再进入 `>>` 分发，所以
`@` 开头的 token 在这里属于保留语法。`>>` 后没有名字时报告用法；这不等于
所有非空 token 都一定是可用工作流。

使用 `M-x superchat-workflow-edit` 打开已有工作流或准备一个新的工作流，然后用
`C-x C-s` 正常保存；打开编辑器本身不会保存或运行工作流。

名称保持简短，具体指令放在文件正文中（每行一个步骤，`#` 注释不执行）。含空格的名称调用时用双引号包住，例如 `/workflow "天气 预报" 杭州` 或 `>>"天气 预报" 杭州`；补全会自动加引号。不加引号时仍按空白分隔名称与参数，原有写法不变。

推荐使用 `/workflow <name> [args]`：先补全 `/workflow`，输入空格后再次补全，选择已保存的配方。Corfu/company 可以展示候选，也可按 `M-TAB`（`completion-at-point`）触发。补全只填入名称，不会执行；在名称后输入任务，再按 `C-c C-c` 执行。原有 `>>name [args]` 快捷写法继续兼容。

```text
# research.workflow —— # 开头是注释，空行会跳过。
Search for the latest information about "$input" on the web. Provide a comprehensive overview.

# 下一步通过 $result 看到上一步输出。
Based on this information: $result, summarise the three most important points concisely.

# 之前的步骤可用 $stepN 引用。
Step 1 found: $step1. Step 2 summarised: $step2. Write one concluding sentence about $input.
```

变量：`$input`（触发参数）、`$result`（上一步输出）、`$stepN`（第 N 步输出）、
`$lang`、`$date`。

每步注解：行首的 `@model-name` 覆盖该步模型；行首的 `/command args` 展开
用户命令 prompt template，并把 args 绑定到 `$input`；首部的 `#~/path/to/file`
或 `#/绝对路径/to/file` 附加上下文文件。其余文本作为该步骤的 prompt 发送，正文
后部的 `#...`（例如 "issue #42"）按普通文字保留。

工作流不支持分支或条件，是线性脚本。一个步骤仅凭唯一记录的 `run.completed`、且状态为 `completed` 才能前进。Run 尚无终态时等待；其他终态、终态冲突或记录不可读时停止推进。这是读取已有结局，不是重新判断整条历史是否合法，具体规则见[请求生命周期](docs/protocols/request-lifecycle.md)。参见可运行示例 `examples/research.workflow`。旧式
`type: workflow` SKILL.md 可以用 `M-x superchat-workflow-import-legacy-dir`
从 `.workflow` 文件生成。

首个长期 Research 纵切可通过 `M-x superchat-research-workflow` 启动：Collector、Analyst、Reviewer 作为顺序 Child Actor 运行并发布不可变 Artifact。重启 Emacs 后，`M-x superchat-research-workflow-status` 展示已有 Run 结局、Artifact 可用性与下一阶段的恢复决定。`M-x superchat-research-workflow-resume` 仅在前序 Run 结局和产物检查允许时启动下一阶段；产物不可用仍可能需要人工复查。

### 从外部查看 Run（`emacsclient`）

当 Emacs server 已经运行时，可按需加载 `superchat-emacsclient` 适配器，
通过 JSON 命令列出在途 Run、读取单个 Run 或 Session 历史，以及取消 Run：

```sh
emacsclient --eval '(superchat-emacsclient-list-runs)'
emacsclient --eval '(superchat-emacsclient-run-status "RUN-ID")'
emacsclient --eval '(superchat-emacsclient-session-history "SESSION-ID" 50)'
emacsclient --eval '(superchat-emacsclient-run-cancel "RUN-ID")'
```

适配器不会启动 server。`emacsclient --eval` 本来就能执行任意 Emacs Lisp；
能够连接该 server 的本地进程本来就拥有同样的能力。因此这些命令只是便利
接口，不是沙箱、鉴权层或安全边界。

### 共享 Workspace

`M-x superchat-artifact-board-set-region` 会把当前区域注册为当前 Session
Workspace 中的 Artifact。一个 Workspace 可以包含多个区域 Artifact；Coding
Agent 和委派的 Executor 可先用 `workspace_info` 查看，再按 Artifact ID 调用
`workspace_read` 或 `workspace_write`。`workspace_publish` 会把新的不可变生成
Artifact 保存到 Superchat 数据目录，因此正文可跨 Emacs 重启恢复。跨 Run 读取和工具写入都携带查询到的
读取其他 Actor 的 Artifact 需要其生产者 Run 有唯一的正常完成记录，并通过产物来源、生产者、修订和 Tape 顺序检查；过期写入仍被拒绝。Tape 只持久化 Artifact 元数据和生命周期事件；区域
Artifact 的正文仍留在原 buffer，发布的生成 Artifact 则持久保存。

### 权限档位

Superchat 操作的是 Emacs 的 **buffer**，不是文件。模型可以读文件（`read_file`）、把文件打开成 buffer（`open_file`）、修改 buffer。**它的修改停在 buffer 里：把 buffer 存盘是你的事**，和你平时用 Emacs 一样——mode line 上的 `*` 就是"有待保存的修改"。一个 Run 结束时如果留下了它改过、还没保存的 buffer，你会收到一条列出它们的消息，同时在另一个窗口出现一块**审阅屏**，光标自己进到它上面：一次一个 buffer，显示文件与 buffer 之间的彩色 diff，`s` 保存、`n` 先不存看下一个、`q` 结束，结束后光标回到原来的地方。审阅屏出现之前敲的键不算数；它出现时你若正在 minibuffer 里输入，等你输完光标再过去。你保存了这样的 buffer（任何方式，包括 `C-x C-s`），或者丢弃了它未保存的修改，模型在下一轮开头会被告知一次。装了 [diffs.el](https://github.com/LuciusChen/diffs.el) 时，diff 会有行号和词级高亮。`/changes` 随时列出这些 buffer 并打开同一块审阅屏，`M-x superchat-changes-diff` 也一样；`M-x superchat-changes-save`／`-revert` 针对单个 buffer；在文件窗口里用这几个命令时，用的是从那里就地发起时会进的那段对话（文件旁边那段，没有就是主对话）。不访问文件的 buffer 只列出、不进审阅屏；通过 `eval_elisp` 跑的代码改了什么不被跟踪。不想让审阅屏自己出现，把 `superchat-changes-review-after-run` 设为 nil。

Superchat 的默认修改路径不执行持久化，也不对抗你的环境：如果你装了 `super-save`、开了 `auto-save-visited-mode`、配置了保存时格式化之类，改过的 buffer 会落盘，那是因为**你**把 Emacs 配成了这样。所以承诺不是"磁盘一定不变"，而是"Superchat 不去写它"。

每个 Session 运行在一个权限档位上；`/permissions` 查看当前档位，`/permissions 档位名` 只修改这一个 Session。档位只决定已有工具怎样执行，从不增加工具。

| 档位 | 模型能做什么 |
|---|---|
| `edit-buffers`（默认） | 什么都能读；任何普通 buffer 都可以改，不用问你。凡是等于"完全控制 Emacs"的工具——`eval_elisp`、shell 命令、`define_tool` 以及 agent 自己写的工具——**每次调用**都问你（`y` 允许这一次，`n` 拒绝，`?` 先看工具名、理由和完整参数；你的回答只对你看到的那组参数有效；提问出现之前按下的键会被丢弃，不能回答它；正在执行的键盘宏不算人，会被直接拒绝）。MCP 和其它第三方工具也会问，除非它的服务器在 `superchat-mcp-trusted-servers` 里。 |
| `auto` | 完全控制类和第三方工具不问就执行；保存 buffer（`save_buffer`，预设列了它才有）仍然问你，模型也仍然可以用 `ask_user` 向你提问。回答审批问题时选**全部允许**（面板上的 `[Allow all]` 按钮、`M-x superchat-approval-allow-all`，或在 `M-x superchat-pending` 里按 `a`），就是放行这一次并把这段对话切到这个档位；`/permissions edit-buffers` 改回逐次询问。它是这段对话的档位，不是对某一个工具的放行。 |
| `danger-full-access` | 完全控制类和第三方工具不问就执行。谁也不问，所以模型也拿不到 `ask_user`。 |
| `read-only` | 只观察：不修改 buffer；完全控制类和第三方工具直接拒绝。 |

`eval_elisp` 和 shell 是逃生口：你批准了某次调用，那段代码就什么都能做，包括保存和写文件——这正是它们每次都要问的原因。Superchat 自己的 buffer（聊天 View 和内部 buffer）在任何档位下都不会被工具修改。`save_buffer` 不在默认工具里；确实需要模型保存的 Preset 可以点名提供它，那时每次调用都会问你——在没法问人的地方（`read-only`，或批准策略为 `never`，也就是 `danger-full-access` 的默认）会被拒绝。原来叫 `workspace-write` 的档位现在叫 `edit-buffers`，旧名仍然接受。

**读不受档位限制。** `read_buffer`（读活 buffer 的文本，含未保存改动；每次最多 2000 行／50 KiB，narrow 过的 buffer 只读 narrow 出的部分，结果会写明）、`read_file`（读磁盘字节，每次最多 2000 行／50 KiB，不创建 buffer）和 `open_file`（真正的 Emacs visit：major mode、你的 hook、文件局部变量都会运行，期间 Emacs 自己提的问题属于 Emacs，不是 Superchat 的批准）在任何档位下都不问人。文件能不能读，由你这个 Emacs 进程的操作系统权限决定，没有敏感路径名单。读出的内容会进入模型上下文，使用远程模型时就会发送给对应的服务商。插件可以用 `tools/pre-execute` 监听收紧这一切。

`/reset` 保留 Session 的档位和批准策略。

### 记忆

记忆系统基于 SQLite 数据库，使用 FTS5 全文搜索。两个命令控制它：

- `/remember [文本]` — 捕获文本（不带参数时捕获上一轮对话）。
- `/recall <关键词>` — 搜索记忆，结果附加到下一轮对话中。

当你的查询足够长时（可通过 `superchat-memory-auto-recall-min-length` 配置，默认 8 个字符），记忆会在每一轮自动检索。检索结果由 `superchat-prompt-hooks.el` 里的 `memory` 收集器注入到 prompt 中。

数据库文件位于 `~/.emacs.d/superchat/superchat.db`（即 `superchat-data-directory` 目录下）。你可以用任何 SQLite 客户端查看：

```bash
sqlite3 ~/.emacs.d/superchat/superchat.db "SELECT title FROM memory LIMIT 5"
```

**数据库里记下的工具调用内容。** 模型每调用一次工具，数据库都会记下它传入的参数和得到的结果（或被拒绝时收到的提示），每项最多 2000 个字符。其中包括模型读过的文件内容、它执行的 Lisp 求出的值。Superchat 会把名字像密钥的字段打码，但不会扫描文字内容，所以模型读过的文件里如果有密码，会原样存下。同一个对话之后的各轮会把每次调用重新作为一次工具调用、后面跟着它的结果发给模型，也就是模型当初调用时用的形式，模型因此记得自己观察到了什么。把 `superchat-tape-store-tool-material` 设为 nil 就一概不存，模型在每一轮结束后就会像以前一样忘掉工具结果。

记忆调优在 `M-x customize-group RET superchat-memory RET` 中。

### MCP

superchat 集成 [mcp.el](https://github.com/lizqwerscott/mcp.el)。通过 `mcp-hub-servers` 配置服务器，用 `/mcp-start` 启动，用 `/mcp` 查看状态。

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem"
                               "/Users/you/Documents")))))
```

服务器连接后，superchat 会用 `superchat-register-tool` 把它的每个工具以 `mcp:<server>:<tool>` 的名字注册进来。此后这些工具和其他已注册工具一样出现在 `superchat-get-llm-tools` 里——同一个工具面、同一份 request-local 工具清单、同一层 Tape 审计包装；一次调用要不要问你由[权限档位](#权限档位)决定：默认每次 MCP 调用都会问，直到你把它的服务器写进 `superchat-mcp-trusted-servers`。

注册发生在服务器经 superchat 启动时（`/mcp-start`、`/mcp-start-server`）。如果你是在别处启动的服务器——在 init 文件里调用 `mcp-hub-start-all-server`，或用 `mcp-hub` 界面——下次执行 `/mcp` 时会把它接上。

### Telega Cowork Session

加载 [telega.el](https://github.com/zevlg/telega.el) 后，打开任意 Telega chat 并执行
`M-x superchat-cowork-telega`，无需选中消息。bundle 插件会为当前 chat 创建或复用
一个 Session；你可以用自然语言连续追问、理解局面并准备回复。模型侧 Session 只能
读取当前或明确授权的 chat，不能发送消息或修改规则；`C-c C-d` 会把最新回答放回
原 Telega 输入区供你检查。

### 在你自己的 buffer 里用（不用打开聊天窗口）

| 入口 | 作用 |
|---|---|
| `M-x superchat-inline-ask` | 就光标处的代码提问，回答显示在该行下方的小框里，文件不会被改动。输入问题时按 `C-c SPC` 切换要附带的上下文（函数、表达式、当前行、窗口、整个文件、不附带）。在回答框上：`r` 追问，`v` 打开完整对话，`w` 复制，`q` 清除。 |
| `M-x superchat-inline-act` | 让 AI 就在你所在的位置把事情做完：它可以读别的 buffer、改当前或别的 buffer，需要审批时和聊天窗一样先把问题发布出来、Emacs 照常可用，等你回答（`M-x superchat-approval-allow` / `superchat-approval-deny`，或在 `M-x superchat-pending` 里按 `y` / `n`）；聊天窗不会打开。进度和回答显示在该行下方的小框里，结束时小框会写明这次调用了几次工具、哪些 buffer 被改了还没保存（保存由你决定）。输入指令时同样可按 `C-c SPC` 切换附带的上下文。在小框上：`d` 看被改 buffer 与文件的差异，`r` 追问（仍可用工具），`v` 打开完整对话，`w` 复制，`q` 取消并清除，`M-RET` 打开菜单。 |
| `M-x superchat-rewrite-region` | 改写选区：只改你选中的文字；它周围的内容（所在的 Org/Markdown 标题小节，或所在的函数）只给模型当参考，不许改、不许交回。回答里带着请求标记的会被拦下，比选区长很多的会提示你检查。结果边生成边在选区位置预览；你按 `C-c C-a` 接受之前，文件一个字节都不变（接受后一次撤销可全部退回），`C-c C-k` 拒绝。`C-c C-r` 在结果上再改一次，`C-c C-d` 看差异，`C-c C-m` 按冲突标记合并，`C-c C-n`/`C-c C-p` 在多个预览间跳转；矩形选区每段各有一个预览。 |
| `M-x superchat-annotate` | 批注：对选区（没有选区就是整个 buffer）逐处给意见，每条意见挂在它说的那处文字上——那几个字加波浪线，意见显示在那一行下面，成一块（把 `superchat-annotate-remark-style` 设为 `eol` 则显示在行尾）。你的文字不会被改动。只有在模型说的那一行（或上下两行以内）找到它引用的原文时才挂；找不到的只计数并列出，不乱挂。光标在批注上时 `M-n`/`M-p` 跳到下一条、上一条，`M-x superchat-annotate-list` 列出全部，`M-x superchat-annotate-clear` 清掉。配置和笔记上稳定；代码上差一些，小的本地模型在代码上不可靠。在对话里，消息中写 `>annotate`，这一条就带着对话历史、不带工具地发出，回答同时挂到叫出这段对话时你所在的 buffer 上；`>annotate 名字` 或 `>annotate "名字"` 指定别的 buffer 或文件。它只管这一条消息。 |
| `M-x superchat-pending` | 一张表：Superchat 正在做什么、在等什么。正在跑的执行（是你发起的，还是它自己开始的）、等你回答或等某个时间的工作记录、到点了却没启动的、说不清有没有启动的定时任务、还没到点的定时器、已经结束而你还没看的、工具改过还没保存的 buffer。`RET` 去那段对话（或那个 buffer），`v` 看工作记录，`k` 停掉正在跑的执行，`d` 清掉一条已结束的，`g` 重读。它只读，不排队、不写任何东西。 |
| `/work <目标>`（在聊天输入栏里） | 开一条工作记录并让它一轮一轮自己接着做：一次执行结束而记录还没做完，下一轮就在同一段对话里自己开始，直到记录完成、要等你、到了 `superchat-work-round-limit`（256）、或某一轮被停掉或失败。`/work resume` 在停下之后开下一轮，`/work pause` 关掉自动续，单独的 `/work` 显示记录现在的状态。轮次只在对话的 buffer 还开着时进行，重启后不会自己续；审批仍然一次一次问你。 |
| `#+begin_src superchat` | Org-babel：块内容作为问题，回答写入 `#+RESULTS:`。 |

就地问、就地做、改写选区都进对话窗口正在显示的那段对话（你文件旁边那段，没有就是主对话），不管文件在哪里；不再有单独的就地对话。每次请求只带它指着的内容和你的话，不带这段对话之前的轮次——和 Cursor 的就地编辑、gptel 的改写一样；在同一个小框里追问（`r`）会带上这个小框自己的第一问一答，以及之后你对它改过的 buffer 是否保存或丢弃。这一轮会记进对话：结束时对话窗口里会显示它，带一个按钮能跳回那个文件那一行，之后在对话窗口里发的消息也会带上它。对话正在干活时，就地的请求排队等候。想让某段代码的问题带上对话的历史，就用 `M-x superchat-send-region` 把代码送进对话，在那里问。
| Magit 提交 buffer | 加载 Magit 后，superchat 根据暂存的 diff 生成规范化提交信息。 |

### 统一流水线（高级）

每一次请求——聊天、inline 提问、改写、子 Agent、工作流步骤、压缩、插件 surface——都是一个
*Request envelope*，经 `superchat-pipeline-submit` 走同一条主线：

```
submit → parse（在产生输入的界面里）→ 准入 → 选择工具 → 收集上下文
  → 组装 prompt → (model-transport Service)
```

固定的 Stage 注册表已经退役。会话身份、状态装配、工具选择、prompt 组装和准入现在都是核心
直接调用；插件还能扩展的部分是 Event。插件在 `apply` 里往 `collect-context` Event 注册一个
监听器（返回带预算等级的 prompt 片段），或注册生命周期 Event。所有可见输出只送到 envelope
的 *Sink*（一小组回调），所以 inline 小框、聊天窗口和后台任务只是 Sink 不同。prompt 文本由
插件 `apply` 里注册的 `collect-context` 监听器提供（旧的
`superchat-build-prompt-functions` / `superchat-system-prompt-functions` 钩子已不存在）：

```elisp
(superchat-define-plugin my-git-blame
  :apply
  (lambda (_scope _config)
    (superchat-context-register
     'my:git-blame
     (lambda (env _state _tools)
       (when-let* ((file (and (eq (plist-get env :surface) 'chat)
                              (plist-get (plist-get env :context) :chat-file-ref))))
         (list (superchat-prompt-fragment
                :owner 'my-git-blame :id "my:git-blame" :slot 'context
                :text (shell-command-to-string (format "git blame -L 1,20 -- %s" file))
                :priority 0 :budget-class 'soft))))
     :owner 'my-git-blame)))
```

上一条回答还在生成时发出的消息会自动排队，等它结束后自动发送（原来手动的
`C-c C-q` 草稿已去掉）。对话超出模型上下文窗口时，superchat 会自动压缩成摘要锚点并重试
一次，默认开启（`superchat-handoff-auto-enabled`）。

当前请求路径见[架构地图](docs/architecture.md)，行为保证见[请求生命周期合同](docs/protocols/request-lifecycle.md)。

## 自定义选项

最常用的 defcustom（全部选项见 `M-x customize-group RET superchat RET`）：

- `superchat-llm-backend` — llm.el 的 provider struct。**必须**在发送第一条消息前设置；为 `nil` 时会在发送时拒绝请求。
- `superchat-llm-model` — 可选的 `:chat-model` 覆盖；被 `@model` 切换使用。
- `superchat-llm-streaming` — `t` 为流式（默认），`nil` 为阻塞模式。
- `superchat-llm-tool-names` — 提供给模型的内置工具集合；一次调用能做什么由 Session 的[权限档位](#权限档位)决定。设 `'all` 为全量。
- `superchat-data-directory` — 记忆、配置、命令的存储路径（默认 `~/.emacs.d/superchat/`）。
- `superchat-lang` — 自定义命令中 `$lang` 变量的语言（默认 `"English"`）。
- `superchat-response-timeout` — 每次 provider 请求的静默窗口（默认 120 秒，工具模式有调整）。
- `superchat-emacs-operating-note` — 一小段关于在 Emacs 里干活的说明（键位怎么写、buffer 与文件的区别、该如实报告什么），随带有 `eval_elisp` 或 `edit_buffer` 的请求放进系统提示。默认 `t`；`nil` 关闭；设成字符串则发送你自己的文字。每次这样的请求约多 250 个 token。它对真实模型的效果尚未验证。
- `superchat-response-timeout-hard-limit` — 可选的跨请求累计传输上限（秒）。默认 nil，即没有这道上限（2026-09-19 之前是 600）。
- `superchat-llm-max-tool-rounds` / `superchat-llm-round-limit-action` — 工具轮数预算与触顶行为（默认 nil，不限轮数 / `ask`，仅在设了上限时起作用）。
- `superchat-llm-reasoning` — 请求的思考档位，或沿用 provider 默认值（`nil`）；见 [Reasoning 设置](#reasoning-设置)。
- `superchat-context-message-count` — 注入 prompt 的近期消息数量。
- `superchat-conversation-history-limit` — 缓冲区中保留的最大消息数。
- `superchat-display-style` — 一次 Emacs 里聊天第一次在哪里打开：`editor`（默认）在代码旁的侧窗口，`agent` 在这段对话自己的 tab；聊天里按 `C-c C-w` 切换。
- `superchat-llm-warning-message` — 设为 nil，superchat 发出的请求就不再触发 llm 的"… API is not free software"提醒（以及弹出的 *Warnings* 窗口）；默认 t，交给 llm 自己决定（`llm-warn-on-nonfree`）。
- `superchat-display-single-window` — agent 模式下，聊天是它那个 tab 里唯一的窗口（默认 `t`）；编辑模式从不关窗口。
- `superchat-side-window-width` — 右侧侧窗口（聊天与计划面板）的宽度（默认占 frame 的 `0.42`）。

常用新选项：

- `superchat-llm-tool-names` —— 默认给模型的内置工具：`list_buffers`、`read_buffer`、`read_file`、`read_reference`、`open_file`、`apropos`、`describe-function`、`edit_buffer`、`eval_elisp`（`read_reference` 按 ID 读取 Superchat 随包的操作资料；只要请求里有这个工具，系统文字里就带一小段资料目录）；插件登记的工具也会一并提供；`'all` 为全部。一次调用能做什么由 Session 的[权限档位](#权限档位)决定。
- `superchat-sandbox-default-mode` / `superchat-approval-default-policy` —— 新 Session 起始的档位（默认 `edit-buffers` 和 `ask`）。`superchat-mcp-trusted-servers` —— 其工具不用询问就能运行的 MCP 服务器。
- `superchat-llm-set-model-capabilities` —— 声明本地模型的流式能力，例如 LM Studio：`(superchat-llm-set-model-capabilities "qwen2.5-coder" '(:streaming t :streaming-tool-use nil))`；未声明的模型走保守默认。
- `superchat-rewrite-confirmation` —— `overlay`（预览，`C-c C-a` 接受）或 `auto`。
- `superchat-memory-auto-recall-cross-session` —— 模型可自动看到同一个 workspace 里其他对话的哪些内容：`all`（默认）、`remembered`（只看 `/remember` 记下的）、`none`。不同 workspace 之间互不可见，临时对话只看得到它自己。你自己的 `/recall` 始终搜全部。
- `superchat-personal-workspace-directory` —— 个人 workspace 的目录：日程、RSS、聊天这类界面（`superchat-personal-workspace-modes` 里的 mode）归它；用 `M-x superchat-set-personal-workspace` 指定。不指定（默认）时，从这些界面叫出的是临时对话。
- `superchat-handoff-auto-enabled` —— 上下文溢出时自动压缩（默认 `t`）。

内置编辑与求值工具报告各自的操作结果。`edit_buffer` 修改 live buffer 中唯一匹配的文本，不负责保存或执行文件；如果一次修改会让 Lisp buffer 的括号或字符串不再配对，这次修改不会生效——buffer 保持原样，模型会被告知哪一行不配对，然后重新提交；`eval_elisp` 接收一个完整表达式（多步操作用 `progn` 或 `let`），返回表达式的值或长度受限、可被工具循环识别的错误。通过 `eval_elisp` 运行的代码不会向你提问：它一旦要弹出提问、输入框或读取按键，就会在那里停下，模型会被告知自己去查，只有你知道的事才通过 `ask_user` 问你。配置任务仍需检查实际生效的设置。让配置长期有效是分工完成的：模型修改 Emacs 实际加载的那个配置文件的 buffer，保存由你来做；没有任何工具说明要求模型自己保存。详见[编辑与求值结果](docs/components/superchat-tools.md#built-in-edit-and-evaluation-results)。

新写入的用户消息和模型答复会完整保存，供后续对话读取。上下文预算可能整条省略较早的消息，但不会用显示摘要替换已保存的原文。旧版本已经裁掉的文字无法从指纹恢复。详见[对话历史](docs/decisions/implemented/2026-09-17-preserve-conversation-material.md)。

`M-x superchat-act` 从当前 buffer 发出指令，加前缀参数则打开请求检查器。检查器按当前上下文预算发送最终编辑后的正文。之前几轮对话以只读形式显示在正文上方，并作为单独的消息发给模型，所以编辑正文既不会删掉也不会改动它们；想发一个不带它们的请求，就开一个新对话（`M-x superchat-new-session`），或者从更早的某一轮分支出去（`M-x superchat-branch-session`）。你从正文里删除的附件和来源选区不会被重新加入。系统提示预览包含原始系统配置和收集到的指令，各组装一次。详见[请求准备](docs/protocols/request-lifecycle.md#submission-and-preparation)。

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

### Reasoning 设置

`superchat-llm-reasoning` 默认是 `nil`：不发送任何思考字段，由 provider／服务端选择模式。要在 Ollama 等原生 provider 上关闭思考，请显式设为 `none`。Preset 明确设置的档位覆盖全局值；在 Preset 中使用 `reasoning: inherit` 才会沿用全局设置。原生 provider 使用 llm.el 对相应模型的支持。

对于类型恰为 `llm-openai-compatible` 的 provider，明确选择档位时，需要完整 chat 端点与模型的参数映射，或用户有意配置的 raw reasoning 参数。确认端点支持 `reasoning_effort="none"` 后，可用下面的声明让全局／Preset 设置选择该值：

```elisp
(setq superchat-llm-reasoning-parameter-overrides
      '(((llm-openai-compatible
          "http://localhost:1234/v1/chat/completions"
          "qwen/qwen3.8-27b")
         reasoning_effort
         (none . "none"))))
(setq superchat-llm-reasoning 'none)
```

其他档位只填写该端点支持的值，不能从模型名称或端口号推断协议。缺少映射时，请求在发送前失败，并说明如何选择服务端默认值或声明映射。prompt／provider 中已有的显式 raw reasoning 参数保持优先；希望 Preset 选择档位时，应移除强制的 raw 值。升级保留用户已保存的设置，不将显式 `none` 改成 `nil`。请求序列化只证明发送了什么，不能证明任意服务端都遵守该值。

## 故障排除

**提示 "superchat-llm-backend is unconfigured"**

`superchat-llm-backend` 为 `nil` 时，`M-x superchat` 仍可打开聊天界面，
但发送第一条消息时请求会被拒绝，并报告：

```text
[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]
```

发送第一条消息前，将 provider struct 赋值给 `superchat-llm-backend`。
运行 `/backend` 确认配置。

**`@model` 提示"模型未找到"**

运行 `/models` 查看可用模型。用 `/refresh-models` 从后端重新加载。或通过 `(setq superchat-manual-models '("model-id-1" "model-id-2"))` 手动补充。

**工具调用超时**

每次 provider 请求都有自己的静默窗口：默认 120 秒，工具模式乘以 1.5，并按上下文调整。新正文、reasoning 和工具进展会刷新窗口；早先的一句前言不会取消后续请求的保护。默认不再对整个传输对话设累计上限；需要的话可以用 `superchat-response-timeout-hard-limit` 设一个。Run 总期限（`superchat-runtime-default-deadline`，1800 秒）限制整个 Run。向你提的问题会等你——无论是批准提问，还是模型用 `ask_user` 在聊天窗口里挂出的提问面板：它开着的时候静默窗口和 Run 总期限都不走，你回答之后各自从剩下的时间继续。如果 Run 在提问开着时被别的东西结束（在别处被取消、出错失败），提问会自动关闭，并告诉你这次调用没有执行。如果某一次请求确实很慢，可调整静默窗口，例如 `(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`。这些计时器不能抢占同步 Lisp；详见[模型进展与工具预算](docs/protocols/request-lifecycle.md#model-progress-and-tool-budgets)。

**达到工具轮数上限**

一轮指一次调用了一个或多个工具的模型响应。默认不限轮数：任务一直调用工具，直到模型给出答复、你用 `C-c C-k` 停下，或到了 Run 总期限（[决定](docs/decisions/accepted/2026-09-29-no-tool-round-limit-by-default.md)）。设了上限时，达到该轮数，交互式 Emacs 询问是否再给同样多轮；拒绝或使用 batch 模式时，会撤掉工具并请求最终答复，`stop` 则立即结束。聊天中保留独立的预算提示，重新打开 Session 后也可见。将配置任务视为完成前，应核对答复中的已完成、未完成和未验证部分。可通过 `superchat-llm-max-tool-rounds` 和 `superchat-llm-round-limit-action` 调整行为，累计 hard 上限仍然有效。

**记忆不能持久化**

检查 `superchat-data-directory` 以及 SQLite 数据库是否可写。运行 `/recall test` 验证查询是否正常。记忆自动检索仅在查询超过 8 个字符时激活（可通过 `superchat-memory-auto-recall-min-length` 配置）。

**AI 回复后没有出现 "User:" 提示符**

先查看 Run 正在等待模型、工具还是人工输入。同步阻塞工具可能阻止 Emacs 计时器运行，所以设置超时并不意味着任意 Lisp 都能被中断。增加预算前，应检查该工具及其服务端。

## 编写插件

Superchat 通过 `superchat-plugins` 挂载默认组合。第三方插件是一个普通 Emacs 包，以 `superchat-define-plugin` 声明 Service 依赖和配置，再通过 Scope 注册贡献。卸载会撤回这些注册；活动工作的取消遵守 Runtime 生命周期合同，不属于任意插件清理代码的权限。从[可运行的插件教程](docs/plugin-guide.md)和[组合合同](docs/design/plugin-architecture.md)开始；产品例子是 `plugins/superchat-cowork-telega`。

## 项目状态与路线图

[剩余架构计划](docs/roadmap.md)区分当前实现、已接受但未完成的决定和提案。当前实现仍使用固定 Stage；已经接受的替代方向不等于已交付功能。

当前包版本为 v1.4.0。稳定功能包括聊天、流式、模型切换、技能、记忆、
MCP、按 Session 权限档位并带批准的工具调用、任意 buffer 里的 inline 提问与预览改写、带子
Agent 委派的 agent 模式、带固定 Stage 主线的插件内核、自动压缩，以及异步 `.workflow`
引擎（`>>name [args]` 或 `/workflow <name>`）。

## 许可证

GPL-3.0-or-later。

## 背景

superchat 最初源自 [org-supertag](https://github.com/yibie/org-supertag) 项目的 chat-view 模块。自 v0.1 起已完全独立，不携带任何 org-supertag 依赖。
