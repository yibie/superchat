# Superchat.el

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

您可以通过以下自定义选项来配置 Superchat：

```elisp
;; 设置使用的 AI 模型（如果为 nil，则使用 gptel 的默认模型）
(setq superchat-model "gpt-4") 

;; 设置数据存储目录
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; 设置会话保存目录
(setq superchat-save-directory "~/.emacs.d/superchat/chat-notes/")

;; 设置文件选择的默认目录
(setq superchat-default-directories '("~/Documents" "~/Projects"))
```

## 快速开始

1. 使用 `M-x superchat` 启动 Superchat
2. 在提示符后输入您的问题
3. 按 `RET` 或 `C-c C-c` 发送消息
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

## 高级用法

### 自定义命令

您可以使用 `/define` 命令创建自定义提示词：

```
/define explain-code "请解释以下代码的作用：$input"
```

除了使用 `/define` 命令，您还可以通过更直接的方式创建自定义命令：只需在您的 `superchat-data-directory` 下的 `prompts` 目录中添加提示词文件。文件名（不含扩展名）将自动成为命令名。默认的文件扩展名是 `.prompt`，但也支持如 `.md`、`.org` 和 `.txt` 等其他格式。例如，创建一个名为 `summarize.prompt` (或 `summarize.txt`) 的文件，内容为 `请总结以下文本：$input`，这将自动创建一个新的 `/summarize` 命令。

在自定义提示词中，您可以使用以下变量：
- `$input`：用户的输入内容
- `$lang`：设置的语言（默认为 English）

### 上下文管理

- 使用 `/clear-context` 命令清除当前会话的所有上下文文件
- Superchat 会自动管理添加到会话中的文件

### 键绑定

- `RET` 或 `C-c C-c`：发送输入
- `#`：智能添加文件路径到上下文
- `C-c C-h`：显示命令列表
- `C-c C-s`：保存当前会话

## 配置选项

以下是 Superchat 的主要自定义选项：

- `superchat-buffer-name`：聊天缓冲区的名称（默认为 "*Superchat*"）
- `superchat-model`：使用的 AI 模型（如果为 nil，则使用 gptel 的默认模型）
- `superchat-data-directory`：数据存储目录
- `superchat-save-directory`：会话保存目录
- `superchat-display-single-window`：如果非 nil，则 Superchat 窗口将占据整个 Emacs 框架，提供一个专注的“单窗口”视图。默认开启。
- `superchat-default-directories`：文件选择的默认目录列表
- `superchat-general-answer-prompt`：通用回答提示词模板
- `superchat-context-message-count`: 在提示词中包含的最近消息数量。
- `superchat-conversation-history-limit`: 在会话中保留在内存中的最大消息数量。

## 故障排除

### 常见问题

1. **无法连接到 AI 服务**：请检查您的 gptel 配置是否正确，包括 API 密钥和端点设置。

2. **文件上下文未正确添加**：确保文件路径正确且文件可读。您可以查看消息缓冲区中的诊断信息来排查问题。

3. **命令系统使用问题**：使用 `/commands` 命令查看所有可用命令及其用法。

### 调试建议

- 检查 gptel 的配置是否正确
- 查看 `*Messages*` 缓冲区中的诊断信息
- 确保所有依赖包都已正确安装和加载

## 许可证

Superchat.el 采用 GPL-3 协议开源。

## 背景

Superchat.el 最初源自 [org-supertag](https://github.com/yibie/org-supertag) 项目的 chat-view 模块。为便于单独使用与扩展，Superchat 去除了所有 org-supertag 特有依赖，现已完全独立。
��，Superchat 去除了所有 org-supertag 特有依赖，现已完全独立。
