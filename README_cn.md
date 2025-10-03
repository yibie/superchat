# SuperChat

## 简介

一个为 gptel 打造的、上手友好的 Claude Code 风格聊天界面。Superchat 让结构化提示与文件上下文对话变得简单——无需新增基础设施，只用你的编辑器即可。

- “/” 调用命令（支持补全），并且可以轻松创建自定义命令。
- “#” 在消息中直接附上文件，作为发送给 LLM 的一部分。
- 流式响应与可读输出，聊天体验清爽快捷。
- 与你现有的 gptel 配置即插即用。

主要特性包括：
- 保留完整的命令系统
- 支持将文件作为上下文添加到对话（包含文本内联）
- 支持与多种大型语言模型（LLM）对话
- 使用 GPL-3 协议开源

## 安装与配置

### 依赖要求

- Emacs 27.1 或更高版本
- [gptel](https://github.com/karthink/gptel) 包
- [gptel-context](https://github.com/karthink/gptel) 包（gptel 的一部分）

### 安装步骤

1. 将 `superchat.el` 文件放置在您的 Emacs 加载路径中
2. 在您的 Emacs 配置文件（通常是 `~/.emacs.d/init.el`）中添加以下代码：

```elisp
(require 'superchat)
```

### 基本配置

Superchat 旨在与 `gptel` 无缝集成。所有与大型语言模型（LLM）相关的设置，例如模型（`gptel-model`）、API 密钥和温度等，都会自动从您的 `gptel` 配置中继承。如需配置 AI 的行为，请直接自定义 `gptel` 的相关变量。

您可以通过以下变量配置 Superchat 自身的功能：

```elisp
;; 设置数据存储目录
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; 设置自定义命令中 $lang 变量的语言
(setq superchat-lang "中文")  ; 或 "English", "Français" 等

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

### gptel Tools 集成

Superchat 现在完全支持 gptel 的 tools（工具调用）功能，让 AI 可以直接调用外部工具和服务来增强对话能力。

主要特性包括：
- **零配置集成**：自动读取您在 gptel 中配置的 tools，无需重复设置
- **智能工具调用**：AI 根据您的需求自动判断并使用合适的工具
- **无缝体验**：工具调用结果自然融入对话流程
- **状态查看**：使用 `/tools` 命令查看当前可用的工具状态

使用方法：
1. 在 gptel 中配置您的 tools：
   ```elisp
   (setq gptel-use-tools t)
   (setq gptel-tools (list ...))
   ```
2. 启动 Superchat 并正常对话
3. 当您的需求需要工具时，AI 会自动调用相应工具
4. 使用 `/tools` 命令查看工具状态

示例对话：
```
用户: 帮我搜索最新的 Emacs 新闻
AI: [自动调用 web_search 工具] 我为您找到了最新的 Emacs 新闻...
```

### MCP (Model Context Protocol) 集成

Superchat 现在集成了 MCP (Model Context Protocol) 支持，让您能够通过标准化的协议连接各种外部服务和工具。MCP 提供了一个统一的接口来管理不同服务器提供的工具，大大扩展了 AI 的能力边界。

主要特性包括：
- **零配置架构**：自动检测并集成 MCP 服务器，无需手动配置
- **实时状态监控**：显示服务器连接状态和可用工具数量
- **无缝工具集成**：MCP 工具自动融入 gptel 的工具系统
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
   ;; 示例：配置文件系统服务器
   (setq mcp-hub-servers
         '(("filesystem" . (:command "npx"
                                   :args ("-y" "@modelcontextprotocol/server-filesystem" "/Users/yourname/Documents"))))
   ```

#### 使用方法

**查看 MCP 状态**：
- 使用 `/mcp` 命令查看当前 MCP 状态
- 显示已配置服务器数量、运行中服务器数量和可用工具数量

**启动 MCP 服务器**：
- 使用 `/mcp-start` 命令启动 MCP 服务器
- 系统会自动检测并启动已配置但未运行的服务器
- 启动的工具会自动集成到当前的 gptel 会话中

**示例对话**：
```
用户: /mcp
系统: MCP 状态: 可用 ✓ | 已配置: 1 个服务器 | 运行中: 1 个服务器 | 可用工具: 15 个

用户: /mcp-start  
系统: 正在启动 MCP 服务器...
已启动服务器: filesystem
新增 15 个工具到 gptel 会话

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
'("github" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-github")
                      :env ("GITHUB_PERSONAL_ACCESS_TOKEN" . "your_token_here")))

;; SQLite 数据库服务器
'("sqlite" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-sqlite" "path/to/database.db")))

;; Web 搜索服务器
'("brave-search" . (:command "npx"
                            :args ("-y" "@modelcontextprotocol/server-brave-search")
                            :env ("BRAVE_API_KEY" . "your_api_key")))
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

除了使用 `/define` 命令，您还可以通过更直接的方式创建自定义命令：只需在您的 `superchat-data-directory` 下的 `command` 目录中添加提示词文件。文件名（不含扩展名）将自动成为命令名。默认的文件扩展名是 `.prompt`，但也支持如 `.md`、`.org` 和 `.txt` 等其他格式。例如，创建一个名为 `summarize.prompt` (或 `summarize.txt`) 的文件，内容为 `请总结以下文本：$input`，这将自动创建一个新的 `/summarize` 命令。

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
- `/tools`：查看当前 gptel tools 状态
- `/mcp`：查看 MCP 状态和服务器信息
- `/mcp-start`：启动 MCP 服务器并集成工具

## 配置选项

以下是 Superchat 的主要自定义选项：

- `superchat-buffer-name`：聊天缓冲区的名称（默认为 "*Superchat*"）
- `superchat-data-directory`：数据存储目录
- `superchat-lang`：自定义命令中 `$lang` 变量的语言设置（默认为 "English"）
- `superchat-display-single-window`：如果非 nil，则 Superchat 窗口将占据整个 Emacs 框架，提供一个专注的"单窗口"视图。默认开启。
- `superchat-default-directories`：文件选择的默认目录列表
- `superchat-general-answer-prompt`：通用回答提示词模板
- `superchat-context-message-count`：在提示词中包含的最近消息数量。
- `superchat-conversation-history-limit`：在会话中保留在内存中的最大消息数量。

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

## 故障排除

### 常见问题

1. **无法连接到 AI 服务**：请检查您的 gptel 配置是否正确，包括 API 密钥和端点设置。

2. **文件上下文未正确添加**：确保文件路径正确且文件可读。您可以查看消息缓冲区中的诊断信息来排查问题。

3. **命令系统使用问题**：使用 `/commands` 命令查看所有可用命令及其用法。

### 调试建议

- 检查 gptel 的配置是否正确
- 查看 `*Messages*` 缓冲区中的诊断信息
- 确保所有依赖包都已正确安装和加载

## 更新日志

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
