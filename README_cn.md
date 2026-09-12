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
| 独立 `.workflow` 文件的异步多步骤工作流 | ✗     | ✗      | ✗             | **✓**     |
| SQLite + FTS5 持久化记忆               | ✗     | ✗      | ✗             | **✓**     |
| 基于钩子的扩展模型                      | ✗     | ✗      | ✗             | **✓**     |

选择 superchat，当你需要：
- 用 `@gpt-4o` 或 `@claude-sonnet` 在对话中随时切换模型。
- 用首部的 `#/绝对路径/file.el` 或 `#~/path/to/file.el` 将文件内容内联到 prompt 中。
- 把可复用的提示词写成带 YAML frontmatter 的 SKILL.md 文件。
- 用 `/recall` 搜索过去的对话，并在每次提问时自动附加相关记忆。
- 通过 `(add-hook)` 扩展 prompt 管线——无需 fork，无需 monkey-patch。

选择 gptel 或 ellama，当你的需求更简单：单模型、单 provider、无需记忆或工作流的一次性对话。

## 安装

**依赖：** Emacs 29.1 或更高版本，[llm.el](https://github.com/ahyatt/llm) ≥ 0.31.1（GNU ELPA）。

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
User: #~/src/main.py 这个函数做了什么？
```

文件内容会被内联到 prompt 中。superchat 会从你的输入中剥离 `#路径`，模型看到的是"这个函数做了什么？"加上文件内容。

`@模型`、`>技能` 和 `#文件` 都是**首部命令语法**：只在整段输入开头解析，也可以在首部组合，例如 `@gpt-4o >code-review #~/src/main.py 审查这个文件`。一旦普通正文开始，后续的 `@...` 或 `#...`（包括放在末尾的 `#文件`）都按正文原样保留。未加引号的文件路径必须是绝对路径（`#/...`）或 home 相对路径（`#~/...`）。

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

预制的技能文件存放在 `skills/*.md`。内置技能包括 `code-review`、`planning`、`refactor`。编写自己的技能请参考 [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md)。

在 prompt 中输入 `/` 会打开实时命令补全。补全列表来自实际用于分发的内置
命令和已注册命令；当前包含 `/workflow`、`/review`、`/code`、`/research`、
`/cowork`、`/single`、`/authority` 这 7 个命令。

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

工作流不支持分支或条件，是线性脚本。进入下一步前，Closure 必须能从 Tape
证明上一 Run 已持久成功终止；open、unknown、invalid、failed 或 cancelled
都会停止推进。参见可运行示例 `examples/research.workflow`。旧式
`type: workflow` SKILL.md 可以用 `M-x superchat-workflow-import-legacy-dir`
从 `.workflow` 文件生成。

首个长期 Research 纵切可通过 `M-x superchat-research-workflow` 启动：Collector、Analyst、Reviewer 作为顺序 Child Actor 运行并发布不可变 Artifact。重启 Emacs 后，`M-x superchat-research-workflow-status` 会显示哪些 Run 可信、哪些 Artifact 可消费及下一步能否继续；`M-x superchat-research-workflow-resume` 只启动 Closure 允许的下一阶段。

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

`M-x superchat-workspace-set-region` 会把当前区域注册为当前 Session
Workspace 中的 Artifact。一个 Workspace 可以包含多个区域 Artifact；Coding
Agent 和委派的 Executor 可先用 `workspace_info` 查看，再按 Artifact ID 调用
`workspace_read` 或 `workspace_write`。`workspace_publish` 会把新的不可变生成
Artifact 保存到 Superchat 数据目录，因此正文可跨 Emacs 重启恢复。跨 Run 读取和工具写入都携带查询到的
revision：读取其他 Actor 的 Artifact 前会验证 producer Run 的持久 Closure，
过期写入则会被拒绝。Tape 只持久化 Artifact 元数据和生命周期事件；区域
Artifact 的正文仍留在原 buffer，发布的生成 Artifact 则持久保存。

### 记忆

记忆系统基于 SQLite 数据库，使用 FTS5 全文搜索。两个命令控制它：

- `/remember [文本]` — 捕获文本（不带参数时捕获上一轮对话）。
- `/recall <关键词>` — 搜索记忆，结果附加到下一轮对话中。

当你的查询足够长时（可通过 `superchat-memory-auto-recall-min-length` 配置，默认 8 个字符），记忆会在每一轮自动检索。检索结果由 `superchat-prompt-hooks.el` 里的 `memory` 收集器注入到 prompt 中。

数据库文件位于 `~/.emacs.d/superchat/superchat.db`（即 `superchat-data-directory` 目录下）。你可以用任何 SQLite 客户端查看：

```bash
sqlite3 ~/.emacs.d/superchat/superchat.db "SELECT title FROM memory LIMIT 5"
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

服务器连接后，superchat 会用 `superchat-register-tool` 把它的每个工具以 `mcp:<server>:<tool>` 的名字注册进来。此后这些工具和其他已注册工具一样出现在 `superchat-get-llm-tools` 里——同一个工具面、同一份 request-local 工具清单，也经过同一层 Authority 审批与 Tape 审计包装。

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
| `M-x superchat-rewrite-region` | 改写选区。结果边生成边在选区位置预览；你按 `C-c C-a` 接受之前，文件一个字节都不变（接受后一次撤销可全部退回），`C-c C-k` 拒绝。`C-c C-r` 在结果上再改一次，`C-c C-d` 看差异，`C-c C-m` 按冲突标记合并，`C-c C-n`/`C-c C-p` 在多个预览间跳转；矩形选区每段各有一个预览。 |
| `#+begin_src superchat` | Org-babel：块内容作为问题，回答写入 `#+RESULTS:`。 |
| Magit 提交 buffer | 加载 Magit 后，superchat 根据暂存的 diff 生成规范化提交信息。 |

### 统一流水线（高级）

每一次请求——聊天、inline 提问、改写、子 Agent、工作流步骤、压缩、插件 surface——都是一个
*Request envelope*，经 `superchat-pipeline-submit` 走同一组固定阶段：

```
resolve-session → admit → load-state → select-tools → collect-context
  → assemble-prompt → run-model → save-state → render → dispatch
```

插件不再靠调整钩子顺序，而是在某个阶段里登记贡献（`superchat-stage-add`），例如一个
`collect-context` 收集器返回带预算等级的 prompt 片段，或一个 `select-tools` 决定。所有可见
输出只送到 envelope 的 *Sink*（一小组回调），所以 inline 小框、聊天窗口和后台任务只是
Sink 不同。旧的 `superchat-build-prompt-functions` / `superchat-system-prompt-functions`
钩子通过兼容收集器继续有效，下面这段仍然可用：

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

上一条回答还在生成时发出的消息会自动排队，等它结束后自动发送（原来手动的
`C-c C-q` 草稿已去掉）。对话超出模型上下文窗口时，superchat 会自动压缩成摘要锚点并重试
一次，默认开启（`superchat-handoff-auto-enabled`）。

阶段表见 [docs/architecture.md](docs/architecture.md)，合同见
[docs/design/turn-pipeline-contract.md](docs/design/turn-pipeline-contract.md)。

## 自定义选项

最常用的 defcustom（全部选项见 `M-x customize-group RET superchat RET`）：

- `superchat-llm-backend` — llm.el 的 provider struct。**必须**在发送第一条消息前设置；为 `nil` 时会在发送时拒绝请求。
- `superchat-llm-model` — 可选的 `:chat-model` 覆盖；被 `@model` 切换使用。
- `superchat-llm-streaming` — `t` 为流式（默认），`nil` 为阻塞模式。
- `superchat-llm-tool-names` — 内置工具白名单。默认只暴露小型只读集；设 `'all` 为全量。
- `superchat-data-directory` — 记忆、配置、命令的存储路径（默认 `~/.emacs.d/superchat/`）。
- `superchat-lang` — 自定义命令中 `$lang` 变量的语言（默认 `"English"`）。
- `superchat-response-timeout` — 响应卡死的超时秒数（默认 120）。
- `superchat-context-message-count` — 注入 prompt 的近期消息数量。
- `superchat-conversation-history-limit` — 缓冲区中保留的最大消息数。
- `superchat-display-single-window` — 将 Emacs frame 专用于 superchat（默认 `t`）。

常用新选项：

- `superchat-llm-tool-names` —— 默认给模型的内置工具：`list_buffers`、`read_buffer`、`apropos`、`edit_buffer`、`eval_elisp`；插件登记的工具也会一并提供；`'all` 为全部。每次调用仍经 Authority 审批。
- `superchat-authority-unprompted-read-tools` —— 唯一不弹窗就能读的工具（默认 `read_buffer`、`list_buffers`、`apropos`、`describe-function`），且只限纯读取；命中 `superchat-authority-sensitive-buffers`（如密钥文件）照样询问。改动、求值、跑进程一律先问你。
- `superchat-llm-set-model-capabilities` —— 声明本地模型的流式能力，例如 LM Studio：`(superchat-llm-set-model-capabilities "qwen2.5-coder" '(:streaming t :streaming-tool-use nil))`；未声明的模型走保守默认。
- `superchat-rewrite-confirmation` —— `overlay`（预览，`C-c C-a` 接受）或 `auto`。
- `superchat-memory-auto-recall-cross-session` —— 模型可自动看到其他对话的哪些记忆：`remembered`（只看 `/remember` 记下的，默认）、`all`、`none`；你自己的 `/recall` 始终搜全部。
- `superchat-handoff-auto-enabled` —— 上下文溢出时自动压缩（默认 `t`）。

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

内置工具（网页抓取、文件系统）和 MCP 工具的超时倍率为 `superchat-tool-timeout-multiplier`（默认 1.5），作用于 `superchat-response-timeout`（默认 120s），即工具总超时上限为 180s。一旦超时，superchat 会中断当前轮次。本地模型（Ollama）最容易遇到此问题，因为 LLM 生成工具调用 JSON 本身就需要时间。调高两项：`(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`。将 `superchat-show-ttft` 设为 `t` 可在回显区显示首 token 延迟，帮助你判断瓶颈在 LLM 端还是工具端。

**记忆不能持久化**

检查 `superchat-data-directory` 以及 SQLite 数据库是否可写。运行 `/recall test` 验证查询是否正常。记忆自动检索仅在查询超过 8 个字符时激活（可通过 `superchat-memory-auto-recall-min-length` 配置）。

**AI 回复后没有出现 "User:" 提示符**

通常是阻塞式工具调用导致。superchat 会在 `superchat-response-timeout`（120s）后自动恢复。如果频繁发生，检查你的 MCP 服务器或增大超时值。

## 编写插件

Superchat v2 是一个小内核（插件与 Scope、Service、Stage、Event、Envelope 与 Sink）加一组默认插件：驱动器、模型循环、Authority、Tape、聊天窗口都以插件形式随包发布，由 `superchat-plugins` 挂载。第三方插件就是一个普通 Emacs 包，核心是一个 `superchat-define-plugin`（`:inject` 需要的 Service、`:config`、`:apply` 经 Scope 注册）；卸载即撤回它注册的全部工具、命令、策略、Stage 实现和 Event 监听。见[插件开发指南](docs/plugin-guide.md)与 `docs/design/plugin-architecture.md`；参考实现是 `plugins/superchat-cowork-telega`。

## 项目状态与路线图

当前包版本为 v1.4.0。稳定功能包括聊天、流式、模型切换、技能、记忆、
MCP、经 Authority 审批的工具调用、任意 buffer 里的 inline 提问与预览改写、带子
Agent 委派的 agent 模式、十阶段统一流水线、自动压缩，以及异步 `.workflow`
引擎（`>>name [args]` 或 `/workflow <name>`）。规划中的工作见
[archive/ROADMAP.md](archive/ROADMAP.md)。

## 许可证

GPL-3.0-or-later。

## 背景

superchat 最初源自 [org-supertag](https://github.com/yibie/org-supertag) 项目的 chat-view 模块。自 v0.1 起已完全独立，不携带任何 org-supertag 依赖。
