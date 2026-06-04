# SuperChat

## 简介

一个为 Emacs 量身打造的、上手友好的 Claude Code 风格聊天界面，基于 [llm.el](https://github.com/ahyatt/llm)。Superchat 让结构化提示与文件上下文对话变得简单——无需新增基础设施，只用你的编辑器即可。

- “/” 调用命令（支持补全），并且可以轻松创建自定义命令。
- “#” 在消息中直接附上文件，作为发送给 LLM 的一部分。
- 流式响应与可读输出，聊天体验清爽快捷。
- 与你现有的 llm.el 配置即插即用。

主要特性包括：
- 保留完整的命令系统
- 支持将文件作为上下文添加到对话（包含文本内联）
- 支持与多种大型语言模型（LLM）对话
- 使用 GPL-3 协议开源

## 安装与配置

### 依赖要求

- Emacs 28.1 或更高版本
- [llm](https://github.com/ahyatt/llm) 0.7（GNU ELPA）—— 会顺带依赖 `plz`、`plz-event-source`、`plz-media-type`、`compat`

可选：
- [mcp](https://github.com/lizqwerscott/mcp.el) —— MCP 服务器工具集成
- [org-ql](https://github.com/alphapapa/org-ql) —— 记忆系统查询缓存

### 安装步骤

1. 将 `superchat.el` 文件放置在您的 Emacs 加载路径中
2. 在您的 Emacs 配置文件（通常是 `~/.emacs.d/init.el`）中添加以下代码：

```elisp
(add-to-list 'load-path "/path/to/superchat")
(require 'superchat)
```

### 基本配置

Superchat 旨在与 `llm.el` 无缝集成。你只需要构建一个 llm provider struct 并赋值给 `superchat-llm-backend`；历史、流式、工具、MCP 都从这个 struct 衍生。

```elisp
;; 1. 选择一个 provider，并设置 superchat-llm-backend。

;; OpenAI：
(setq superchat-llm-backend
      (make-llm-openai
       :key (getenv "OPENAI_API_KEY")
       :chat-model "gpt-4o-mini"))

;; Anthropic Claude：
;; (setq superchat-llm-backend
;;       (make-llm-claude
;;        :key (getenv "ANTHROPIC_API_KEY")
;;        :chat-model "claude-3-5-sonnet-20241022"))

;; 本地 Ollama：
;; (setq superchat-llm-backend
;;       (make-llm-ollama
;;        :chat-model "llama3.1"))

;; 2. （可选）在不重建 backend 的情况下覆盖聊天模型。
(setq superchat-llm-model "gpt-4o")

;; 3. （可选）关闭流式响应（如需完全缓冲）。
;;    默认为 t（开启流式，使用 llm-chat-streaming）。
;;    设为 nil 改为阻塞式 llm-chat。
(setq superchat-llm-streaming t)
```

您可以通过以下变量配置 Superchat 自身的功能：

```elisp
;; 设置数据存储目录
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; 设置自定义命令中 $lang 变量的语言
(setq superchat-lang "中文")  ; 或 "English", "Français" 等

;; 响应超时保护（防止阻塞工具导致 UI 卡死）
(setq superchat-response-timeout 120)  ; 秒，设为 nil 可禁用

;; 智能完成检测延迟（用于非流式响应）
;; 主要用于本地模型 + 工具场景
(setq superchat-completion-check-delay 2)  ; 秒，默认为 2

;; 设置文件选择的默认目录
(setq superchat-default-directories '("~/Documents" "~/Projects"))
```

注意：Superchat 现在会自动管理其目录结构。`superchat-save-directory` 和 `superchat-command-dir` 变量已被移除。目录现在会根据需要动态创建，或者您可以使用 `M-x superchat-ensure-directories` 手动确保所有目录都存在。

## 快速开始

1. 使用 `M-x superchat` 启动 Superchat
2. 在提示符后输入您的问题。您可以按 `RET` 来输入多行内容。
3. 按 `C-c C-c` 发送消息。
4. 等待 AI 助手回复

### 基本对话示例

```
User: 你好，你能帮我做什么？
Assistant: 我是一个 AI 助手，可以帮助您回答问题、分析代码、提供建议等。
```

## 核心功能详解

### 基本对话

Superchat 允许您与各种大型语言模型进行自然语言对话。

### 命令系统

Superchat 提供了一个强大的命令系统，允许您定义和使用自定义提示词：

- 内置命令：如 `/create-question` 等
- 自定义命令定义：使用 `/define` 命令
- 查看所有可用命令：使用 `/commands` 命令

在输入提示符后，您可以使用自动补全功能来查看和选择命令：
1. 输入 `/` 字符
2. 继续输入命令的首字母，此时如果你有用 company 或 corfu 插件，从弹出的列表中选择命令。
3. 如果你没有 company 或 corfu，可以直接按下 `M-TAB`（或您的 Emacs 环境中的补全键）触发自动补全，从弹出的列表中选择命令。

例如，要使用 `/create-question` 命令，您可以输入 `/c` 然后按 `M-TAB`，系统会显示所有以 'c' 开头的可用命令供您选择。

### 文件上下文支持

Superchat 可以将文件内容作为上下文添加到对话中：

1. 在输入提示符后按 `#` 键
2. 选择要添加的文件
3. 文件内容将作为上下文提供给 AI

您也可以手动输入文件路径，格式为 `# /path/to/file`。

当设置了 `superchat-default-directories` 时，文件选择仅显示默认目录中的一级文件（不递归子目录），并按扩展名过滤：`org/md/txt/webp/png/jpg/jpeg`，便于快速定位常用文本与图片文件。

### 工具调用

Superchat 内置一个工具注册表，所配置的 llm backend 可以自动调用。你不需要任何额外配置——注册表会在 llm.el 可用时自动填充，并通过 `:tools` 关键字在每次调用 `llm-chat` / `llm-chat-streaming` 时传递给模型。

实现上仍保留完整工具库，覆盖 shell、文件 I/O、搜索、buffer 编辑；但默认注册表只暴露下面的小集合。

默认情况下，Superchat 只暴露一组小型只读工具，以降低首 token 等待时间：

- `read-file`、`list-files`、`search-text`、`read_buffer`

较大的旧工具库仍然保留，但需要显式开启：

- `shell-command`、`find-files`
- `write-file`、`append-file`、`quick-write`、`make_directory`
- `append_to_buffer`、`EditBuffer`、`ReplaceBuffer`

在聊天中使用 `/backend` 命令可以查看当前实际生效的配置：

```
User: /backend
Backend: llm.el
Provider: openai
Model: gpt-4o-mini
Streaming: yes
Tools: 4 registered
MCP tools: 0 registered
```

（`/tools` 作为 `/backend` 的别名保留，以保证 v0.5 之前的肌肉记忆继续可用。）

你可以通过代码覆盖、扩展或禁用注册表：

```elisp
;; 完全禁用内置工具
(setq superchat-llm-tool-names nil)

;; 恢复完整旧内置工具集
(setq superchat-llm-tool-names 'all)

;; 或者追加用 `llm-make-tool' 构造的自定义工具
(setq superchat-llm-tools-list
      (append superchat-llm-tools-list
              (list (llm-make-tool
                     :name "my-tool"
                     :description "Does X"
                     :args (list ...)
                     :function (lambda (args) ...)))))
```

#### 本地模型 + 工具调用的注意事项

如果使用 Ollama（或任何本地模型）并启用工具，响应延迟通常高于云端模型。Superchat 暴露两个相关旋钮：

```elisp
;; 在收到最后一个 chunk 后多等一会，避免把“静默的工具调用回程”误判为掉线。
(setq superchat-completion-check-delay 2)  ; 推荐 1-5 秒

;; 按比例放大响应超时。
(setq superchat-tool-timeout-multiplier 1.5)  ; 1.0 即不放大
```

如果看到 "⚠️ Response timeout" 警告，请优先调高 `superchat-response-timeout` 或 `superchat-tool-timeout-multiplier`。

### 工作流自动化

工作流就像把多轮对话录成“脚本”。一次指令即可完成搜索、分析、保存等步骤，再也不用手动重复。

- **聊天里直接调用**：输入 `>workflow-name 主题` 即可运行同名 `.workflow` 文件，自动复用你在 llm.el 中已经启用的工具与 MCP 服务器。
- **按需组合步骤**：可以继续使用 `#文件路径`、`@model`、`/write-file` 等语法，把文件、模型切换、结果保存串联起来。
- **一次配置，长期使用**：把工作流文件放在 `~/.emacs.d/superchat/workflow/`（或 `superchat-data-directory/workflow/`），想用随时调用。
- **简单线性执行**：每一行非空内容就是一个步骤，按顺序逐行执行。目前暂不支持 n8n 那样的分支/条件，请使用直线流程。

试试看：
1. 新建 `~/.emacs.d/superchat/workflow/ai-news-summary.workflow`，写入以下内容。
2. 在 Superchat 中输入 `>ai-news-summary AI技术`（或任何关键词）。
3. 工作流会自动检索新闻、生成摘要，并把 Markdown 报告写入你指定的文件。

```text
# Workflow: AI技术新闻摘要
# Description: 每周技术新闻摘要

/web-search 搜索关于 "$input" 相关的新闻

@qwen3-coder:30b-a3b-q8_0 分析上面（3 个角度：商业，技术，社会）搜索到的新闻信息，生成一份简洁的中文的新闻摘要

将分析结果保存到 #/Users/chenyibin/Documents/news-summary.md
```

### MCP (Model Context Protocol) 集成

Superchat 现在集成了 MCP (Model Context Protocol) 支持，让您能够通过标准化的协议连接各种外部服务和工具。MCP 提供了一个统一的接口来管理不同服务器提供的工具，大大扩展了 AI 的能力边界。

主要特性包括：
- **零配置架构**：自动检测并集成 MCP 服务器，无需手动配置
- **实时状态监控**：显示服务器连接状态和可用工具数量
- **无缝工具集成**：MCP 工具自动融入所配置的 llm backend 的工具系统
- **智能服务器管理**：支持启动、停止和监控多个 MCP 服务器

#### 安装和配置

1. **安装 MCP 包**：
   ```elisp
   ;; 使用 straight.el 安装
   (straight-use-package 'mcp)
   
   ;; 或者使用 use-package
   (use-package mcp)
   ```

2. **配置 MCP 服务器**（在您的 Emacs 配置中）：
   ```elisp
   ;; 示例：配置多个 MCP 服务器（含可选 Jina API Key）
   (setq mcp-hub-servers
         (let ((jina-token (getenv "JINA_API_KEY")))
           `(("filesystem" . (:command "npx"
                                    :args ("-y" "@modelcontextprotocol/server-filesystem"
                                           "/Users/yourname/Documents")))
             ("fetch" . (:url "https://mcp.jina.ai/sse"
                              :headers ,(when (and jina-token (> (length jina-token) 0))
                                          `((Authorization . ,(concat "Bearer " jina-token)))))))))
   ```

#### 使用方法

**查看 MCP 状态**：
- 使用 `/mcp` 命令查看当前 MCP 状态
- 显示已配置服务器数量、运行中服务器数量和可用工具数量

**启动 MCP 服务器**：
- 使用 `/mcp-start` 命令启动 MCP 服务器
- 系统会自动检测并启动已配置但未运行的服务器
- 启动的工具会自动集成到当前的 llm 会话中

**示例对话**：
```
用户: /mcp
系统: MCP 状态: 可用 ✓ | 已配置: 1 个服务器 | 运行中: 1 个服务器 | 可用工具: 15 个

用户: /mcp-start  
系统: 正在启动 MCP 服务器...
已启动服务器: filesystem
新增 15 个工具到 llm 会话

用户: 帮我列出 Documents 目录中的重要文件
AI: [使用 MCP 文件系统工具] 我为您找到了 Documents 目录中的重要文件...
```

#### 支持的 MCP 命令

- `/mcp` - 显示 MCP 状态和服务器信息
- `/mcp-start` - 启动 MCP 服务器并集成工具

#### 常见 MCP 服务器

以下是几个常用的 MCP 服务器示例：

```elisp
;; 文件系统服务器
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                                :args ("-y" "@modelcontextprotocol/server-filesystem" "/path/to/directory")))))

;; GitHub 服务器
(setq mcp-hub-servers
      '(("github" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-github")
                               :env ("GITHUB_PERSONAL_ACCESS_TOKEN" . "your_token"))))

;; SQLite 数据库服务器
(setq mcp-hub-servers
      '(("sqlite" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-sqlite" "path/to/database.db")))))

;; Web 搜索服务器
(setq mcp-hub-servers
      '(("brave-search" . (:command "npx"
                                    :args ("-y" "@modelcontextprotocol/server-brave-search")
                                    :env ("BRAVE_API_KEY" . "your_key")))))
```

注意：MCP 功能需要安装 `mcp.el` 包。如果未安装，相关命令会显示友好的错误提示。

### 记忆系统

Superchat 现在拥有一个持久化且可查询的记忆系统，允许 AI 记住过去的对话并在未来的交互中利用这些知识。该系统基于 Org-mode 文件构建，确保了透明度和用户控制。

主要特性包括：
- **分层记忆捕获**：自动捕获对话中的重要见解（经 LLM 总结），并允许用户通过明确命令保存记忆。
- **智能检索**：使用从您的查询中提取的 LLM 关键词来查找最相关的记忆，然后将其作为上下文提供给 AI。
- **记忆生命周期管理**：
    - **评分与衰减**：记忆根据其效用进行评分，分数随时间推移而衰减。
    - **自动归档**：不那么相关的记忆会自动移动到归档文件，保持活跃记忆的精简。
    - **自动合并**：相似或重复的记忆可以由 LLM 自动合并为单个、更全面的条目。（注意：此功能默认开启，每天合并一次。）

记忆系统的配置选项可在 `M-x customize-group RET superchat-memory RET` 中找到。

## 高级用法

### 自定义命令

您可以使用 `/define` 命令创建自定义提示词：

```
/define explain-code "请解释以下代码的作用：$input"
```

除了使用 `/define` 命令，您还可以通过更直接的方式创建自定义命令：只需在您的 `superchat-data-directory` 下的 `command` 目录中添加提示词文件。文件名（不含扩展名）将自动成为命令名。默认的文件扩展名是 `.prompt`，但也支持如 `.md`、`.org` 和 `.txt` 等其他格式。

**添加命令描述：**
为了在 `/commands` 列表中显示命令的用途，请使用 `命令名-描述文本.prompt` 的格式命名文件。Superchat 会自动解析第一个连字符之后的内容作为描述。

示例：
- `summarize.prompt` -> 命令：`/summarize` （无描述）
- `seo-优化网站内容.prompt` -> 命令：`/seo`，描述："优化网站内容"

文件的内容（例如 `请总结以下文本：$input`）即为提示词模板。

在自定义提示词中，您可以使用以下变量：
- `$input`：用户的输入内容
- `$lang`：设置的语言（默认为 English）

#### 设置 $lang 变量的语言

自定义命令中的 `$lang` 变量可以通过以下几种方式配置：

**方法一：通过 Emacs 自定义界面（推荐）**
```elisp
M-x customize-variable RET superchat-lang RET
```
然后将值从 "English" 改为您偏好的语言（如 "中文"、"Français"、"Español"）并保存设置。

**方法二：在配置文件中设置**
在您的 Emacs 配置文件中添加：
```elisp
(setq superchat-lang "中文")        ; 中文
(setq superchat-lang "English")     ; 英文
(setq superchat-lang "Français")    ; 法文
(setq superchat-lang "Español")     ; 西班牙文
```

**方法三：临时设置（当前会话）**
在 Emacs 中执行：
```elisp
M-x eval-expression RET (setq superchat-lang "中文") RET
```

**使用示例：**
当您设置 `(setq superchat-lang "中文")` 并使用内置的 `/create-question` 命令时：
- 模板：`"Please list all important questions related to $input in $lang."`
- 输入 "git" 后：`"Please list all important questions related to git in 中文."`

语言设置在每次发送消息时都会动态获取，因此您可以随时更改语言设置，立即生效。

### 上下文管理

- 使用 `/clear-context` 命令清除当前会话的所有上下文文件
- Superchat 会自动管理添加到会话中的文件

### 键绑定

- `RET` 或 `C-c C-c`：发送输入
- `#`：智能添加文件路径到上下文
- `C-c C-h`：显示命令列表
- `C-c C-s`：保存当前会话
- `/backend`：查看当前 llm backend、模型、流式、工具/MCP 数量（`/tools` 是兼容别名）
- `/mcp`：查看 MCP 状态和服务器信息
- `/mcp-start`：启动 MCP 服务器并集成工具

## 配置选项

以下是 Superchat 的主要自定义选项：

- `superchat-buffer-name`：聊天缓冲区的名称（默认为 "*superchat*"）
- `superchat-data-directory`：数据存储目录
- `superchat-lang`：自定义命令中 `$lang` 变量的语言设置（默认为 "English"）
- `superchat-display-single-window`：如果非 nil，则 Superchat 窗口将占据整个 Emacs 框架，提供一个专注的"单窗口"视图。默认开启。
- `superchat-default-directories`：文件选择的默认目录列表
- `superchat-general-answer-prompt`：通用回答提示词模板
- `superchat-context-message-count`：在提示词中包含的最近消息数量。
- `superchat-conversation-history-limit`：在会话中保留在内存中的最大消息数量。
- `superchat-response-timeout`：单次响应的硬性超时（秒），设为 nil 禁用。默认 120。
- `superchat-completion-check-delay`：关闭流式时，最后一个 chunk 之后等待多少秒再判定响应结束。默认 2。
- `superchat-tool-timeout-multiplier`：当工具调用介入时，对 `superchat-response-timeout` 的放大倍数。默认 1.5。
- `superchat-show-response-mode`：如果非 nil，在每次响应前显示一段简短的 "🤖 Synchronously generating..." / "🧠 Streaming..." 提示。默认 t。

**llm.el 集成：**

- `superchat-llm-backend`：llm provider struct（由 `make-llm-openai`、`make-llm-claude`、`make-llm-ollama` 等构造）。**必须在 `M-x superchat` 之前设置。** 默认为 nil（未配置）。
- `superchat-llm-model`：可选的 `:chat-model` 覆盖。为非 nil 时会转发给 `llm-chat` / `llm-chat-streaming`，并被 `@model` 切换使用。
- `superchat-llm-streaming`：非 nil（默认）时使用 `llm-chat-streaming` 增量输出；为 nil 时改用阻塞的 `llm-chat`。
- `superchat-manual-models`：可选的模型 ID 列表。当 backend 自带的 `:chat-model` 枚举为空，或想注入额外模型时，供 `/models` 使用。
- `superchat-llm-tool-names`：内置工具 allowlist。默认是小型只读集合 `("read-file" "list-files" "search-text" "read_buffer")`；设为 `all` 可恢复完整旧工具库，设为 nil 可禁用所有内置工具。
- `superchat-llm-tools-list`：用 `llm-make-tool` 构造的工具结构体缓存。首次调用 `superchat-get-llm-tools` 时从 `superchat-llm-tool-names` 自动生成；也可以通过 `append` 加入自定义工具。

### 记忆系统配置

这些选项控制 Superchat 的记忆系统。您可以通过 `M-x customize-group RET superchat-memory RET` 进行自定义。

- `superchat-memory-file`：主记忆 Org 文件的路径（默认为数据目录中的 `memory.org`）。
- `superchat-memory-archive-file`：归档记忆 Org 文件的路径（默认为数据目录中的 `memory-archive.org`）。
- `superchat-memory-explicit-trigger-patterns`：Tier 1 显式记忆命令的正则表达式模式。
- `superchat-memory-auto-capture-enabled`：启用/禁用自动记忆捕获（Tier 2）。
- `superchat-memory-auto-capture-minimum-length`：触发自动捕获的用户消息最小长度。
- `superchat-memory-use-org-ql-cache`：启用/禁用记忆查询的 `org-ql` 缓存。
- `superchat-memory-max-results`：每次检索返回的最大记忆数量。
- `superchat-memory-auto-recall-min-length`：自动记忆检索运行前的最小查询长度。
- `superchat-memory-auto-increment-access-count`：自动增加检索到的记忆的 `:ACCESS_COUNT:`。
- `superchat-memory-decay-factor`：应用于衰减期间访问计数的乘数。
- `superchat-memory-decay-min-access`：衰减后访问计数的下限。
- `superchat-memory-archive-threshold`：归档记忆的访问计数阈值。
- `superchat-memory-auto-prune-interval-days`：自动记忆修剪的间隔天数（0 或 `nil` 禁用）。
- `superchat-memory-merge-similarity-threshold`：合并记忆的 Jaccard 相似度阈值。
- `superchat-memory-auto-merge-interval-days`：自动记忆合并的间隔天数（0 或 `nil` 禁用）。警告：此功能存在不正确合并的风险。

### 新增函数

- `superchat-ensure-directories`：交互式函数，用于手动确保所有必要的目录都存在。使用方法：`M-x superchat-ensure-directories`
- `superchat-llm-tools-reload`：强制重载内置工具注册表（在增删工具后很有用）。使用方法：`M-x superchat-llm-tools-reload`

## 故障排除

### 常见问题

1. **提示 "superchat-llm-backend is unconfigured" / 聊天无法启动**：在打开聊天之前，使用 `make-llm-openai` / `make-llm-claude` / `make-llm-ollama` 等函数构造 provider struct 并赋值给 `superchat-llm-backend`。配置完成后运行 `/backend` 确认 provider、模型和工具数量。

2. **无法连接到 AI 服务**：请检查 llm.el 配置——provider struct 上的 `:key`、端点、`:chat-model`。同样的 struct 应当可以直接在 Superchat 外用 `llm-chat` 工作。

3. **文件上下文未正确添加**：确保文件路径正确且文件可读。您可以查看消息缓冲区中的诊断信息来排查问题。

4. **命令系统使用问题**：使用 `/commands` 命令查看所有可用命令及其用法。

5. **AI 响应后没有出现 "User:" 提示符**：通常由阻塞工具或网络问题导致。
   - **自动保护**：SuperChat 有 `superchat-response-timeout`（默认 120s）超时保护，可自动恢复 UI
   - **如果出现超时提示**：说明你的工具是阻塞的（同步网络调用等）
   - **解决方法**：用 `(setq superchat-response-timeout 300)` 提高超时，或修复你的工具
   - **查看状态**：在聊天中运行 `/backend` 查看流式与工具数量

6. **响应超时警告**：当出现 "⚠️ Response timeout after X seconds" 时，说明：
   - 你的工具在做同步/阻塞调用（如 `url-retrieve-synchronously`）
   - 网络非常慢
   - **修复**：调高 `superchat-response-timeout` 或 `superchat-tool-timeout-multiplier`

### 调试建议

- 确认 `superchat-llm-backend` 非 nil，并且 provider struct 在 Superchat 外也能正常工作（在 scratch buffer 中直接调用 `llm-chat` 试试）
- 查看 `*Messages*` 缓冲区中的诊断信息
- 确保所有依赖包（llm、mcp、org-ql）都已正确安装和加载

## 更新日志

### 版本 0.6 (2026-06-01)
- **记忆—灵魂 双轨分离**。记忆子系统现在维护两条独立轨道：原有的 `memory.org`（经过总结、评分、衰减的知识）与新增的原始事件 `soul.org`（逐字保留、带 mood + context、永不自动合并）。拆分动机：过度总结会丢失上下文与情绪，强行合并会抹掉天然存在的矛盾，而衰减又会让"那一刻的感受"凭空消失。
- **新增 `superchat-memory-add-raw`**，支持 `:mood` / `:context` / `:verbatim` 关键字参数。直接向 `soul.org` 追加带 `:ID:` / `:TIMESTAMP:` / `:MOOD:` / `:TYPE:` / `:CONTEXT:` / `:VERBATIM:` 属性的条目，返回条目 id。**不依赖 LLM**——灵魂轨道本质上就是一份 Org 日志。
- **矛盾共存**。新增 `:CONTRADICTION:` 标签（用逗号或空白分隔的 id 列表）、`:VALIDITY: expired` 属性、`:REPLACED_BY:` 链接。矛盾被**保留**而非合并——新旧两条都留存，方便回溯"什么时候、改成了什么"。
- **双向 paired-expired 呈现**。`superchat-memory-retrieve-with-context` 现在会同时检测"出向矛盾"（条目自身的 `:CONTRADICTION:`）与"入向矛盾"（其它条目的 `:CONTRADICTION:` 指向本条目）；当 query 命中主题时，附上 `:paired-expired` 与 `:contradiction-ids` 两个 plist 字段，数量上限由 `superchat-memory-contradiction-context-window` 控制。
- **手动审阅 UI**。新增次模式 `superchat-memory-review-mode`（y/n/e/s/q 键位）用于一键接受/拒绝合成记忆。`M-x superchat-memory-review-pending` 打开审阅缓冲区；条目跨会话保留（在 memory 文件中以 `:REVIEWED: nil` 标记）。
- **情绪标签字典**。新增 `defcustom superchat-memory-mood-taxonomy`，默认包含 curious / frustrated / tired / satisfied / neutral；`--resolve-mood` 对未知值回退到 "neutral"。
- **新增 defcustom**：
    - `superchat-memory-soul-file`（nil → `<data-dir>/soul.org`）：灵魂日志文件。
    - `superchat-memory-soul-synthesis-mode`（`manual`）：何时触发 `superchat-memory-synthesize-soul`——`manual` / `weekly` / `never`。
    - `superchat-memory-contradiction-context-window`（3）：每次 query 最多附带的 paired-expired 条目数。
    - `superchat-memory-mood-taxonomy`（字符串列表）：已识别的情绪标签。
- **`superchat-memory-synthesize-soul` 仅手动触发**。原 `auto-merge` 默认行为替换为审阅队列——由用户审阅灵魂轨道给出的建议，而不是让 LLM 静默合并记忆。函数在 gptel/llm 缺失时为 no-op。
- **测试覆盖**：新增 `test/test-memory-soul.el`（23 个测试，全部通过），覆盖文件 I/O、矛盾标签解析、情绪解析、paired-expired 检索、审阅队列、键位映射。运行不依赖 gptel、llm、org-ql。

### 版本 0.5（未发布）
- **后端硬切换：gptel → llm.el**（破坏性变更）。Superchat 不再以 gptel / gptel-context 为运行时依赖。聊天传输改用 [llm.el](https://github.com/ahyatt/llm)（GNU ELPA, 0.7）的 `llm-chat` / `llm-chat-streaming`。通过新增的 `superchat-llm-backend` defcustom，使用 `make-llm-openai` / `make-llm-claude` / `make-llm-ollama` 等进行配置。
- **Emacs 28.1 起步**（原 27.1）。llm.el 依赖的 `plz` 最低要求。
- **移除 `superchat-agent.el`**。WIP 的 gptel-agent 集成既无文档也没有测试，且 `gptel-agent` 不在 MELPA。原本调用它的逻辑请回落到标准 llm 工具调用。
- **移除 gptel 专用 workaround**（`gptel-curl--stream-cleanup :around` advice）。基于 plz 的 llm.el 传输不再有当时它为之绕过的上游 sentinel bug。
- **新增 defcustom**：
    - `superchat-llm-backend`（nil）：llm provider struct。
    - `superchat-llm-model`（nil）：可选的 `:chat-model` 覆盖，供 `@model` 切换使用。
    - `superchat-llm-streaming`（t）：非 nil 时使用 `llm-chat-streaming`，nil 时使用 `llm-chat`。
    - `superchat-manual-models`（nil）：当 backend 自带的模型枚举为空时，由 `/models` 列出的额外模型 ID。
- **新增 `/backend` 命令**（并加入斜杠命令帮助列表）。展示当前配置的 backend struct、provider 名称、聊天模型、流式模式、内置工具数量、MCP 工具数量。原 `/tools` 命令作为 `/backend` 的别名保留以保证兼容性。
- **内置工具注册表** `superchat-llm-tools-list`。原本注册为 gptel 工具的工具现改用 `llm-make-tool` 构造；默认只暴露小型只读子集，完整旧工具集可通过 `(setq superchat-llm-tool-names 'all)` 开启。可以用 `M-x superchat-llm-tools-reload` 强制重载。
- **测试套件更新**：删除 9 个 gptel 专用测试文件（`test-gptel-*`、`test-real-gptel-models`、`test-model-*`、`test-haip`、`test-completion-realistic`）。新增 `test/test-llm-backend.el`（28 个测试），覆盖新 backend show 命令、同步/异步派发器、模型切换、工具注册表。规范入口 `emacs -Q -l test/run-tests.el` 现在也会跑这些测试。

### 版本 0.4 (2025-10-13)
- **工作流集成**：工作流现在可以在 Superchat 内直接运行，并复用 `/prompt` 共享的 gptel 工具与 MCP 服务器；新增聊天快捷语法（`>workflow-name`）及工作流目录说明。
- **工具输出加固**：在处理 Jina/MCP 等工具返回的 Markdown 或 HTML 时自动清洗控制字符，修复 `json-value-p` 相关错误。
- **实用性提升**：新增 `superchat-version` 常量并补充 `superchat-tools.el` 的自动化测试，覆盖 Jina 回退与用户确认分支。

### 版本 0.3 (2025-10-03)
- **MCP 集成**：集成了 Model Context Protocol (MCP) 支持。
    - 零配置架构，自动检测和集成 MCP 服务器。
    - 实时状态监控，显示服务器连接状态和可用工具数量。
    - 无缝工具集成，MCP 工具自动融入 gptel 的工具系统。
    - 智能服务器管理，支持启动、停止和监控多个 MCP 服务器。
    - 新增 `/mcp` 和 `/mcp-start` 命令用于 MCP 管理。
- **gptel Tools 集成**：零配置集成 gptel 的工具调用功能。
    - 直接读取用户在 gptel 中配置的 tools，无需重复配置。
    - 在聊天界面中无缝使用 gptel 的工具调用功能。
    - 支持 function calling 能力，可以调用外部工具和 API。
    - 添加 `/tools` 命令查看当前 tools 状态。
- **@ 模型切换功能**：支持通过 `@model` 语法在对话中直接切换不同的 AI 模型。
    - 在聊天输入中使用 `@模型名` 语法快速切换模型。
    - 添加 `/models` 命令查看可用模型列表。
    - 临时模型切换，单次请求后自动恢复原模型。
- **Bug 修复**：修复了命令系统初始化和会话管理的问题。

### 版本 0.2 (2025-09-23)
- **记忆系统**：引入了全面的 AI 记忆系统，使 AI 能够从对话中进行持久学习。
    - 分层记忆捕获（显式、自动 LLM 总结）。
    - LLM 驱动的查询关键词提取，实现智能检索。
    - 记忆评分、衰减和自动归档。
    - 自动 LLM 驱动的相似记忆合并（默认禁用，需手动开启）。
- **Bug 修复**：解决了各种稳定性与兼容性问题。

## 许可证

Superchat.el 采用 GPL-3 协议开源。

## 背景

Superchat.el 最初源自 [org-supertag](https://github.com/yibie/org-supertag) 项目的 chat-view 模块。为便于单独使用与扩展，Superchat 去除了所有 org-supertag 特有依赖，现已完全独立。
