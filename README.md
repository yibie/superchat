# Superchat.el

## Introduction

A friendly, Claude Code–style chat UI for gptel in Emacs. Superchat makes structured prompts and file‑grounded conversations effortless—no new infrastructure, just your editor.

- "/" for commands with completion, plus easy custom command creation.
- "#" to attach files as part of the message you send to the LLM.
- Clean, fast chat experience with streaming and readable output.
- Works out‑of‑the‑box with your existing gptel setup.

Key features include:
- Retaining the complete command system
- Adding the ability to include files as context in conversations
- Supporting conversations with various large language models (LLMs)
- Open-sourced under the GPL-3 license

## Installation and Configuration

### Requirements

- Emacs 27.1 or higher
- [gptel](https://github.com/karthink/gptel) package
- [gptel-context](https://github.com/karthink/gptel) package (part of gptel)

### Installation Steps

1. Place the superchat file in your Emacs load path
2. Add the following code to your Emacs configuration file (usually `~/.emacs.d/init.el`):

Example:
```elisp
(add-to-list 'load-path "~/.emacs.d/superchat")
(require 'superchat)
```

### Basic Configuration

You can configure Superchat using the following customization options:

```elisp
;; Set the AI model to use (if not set, gptel's default model will be used)
(setq superchat-model "gpt-4") 
;; Set the data storage directory
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; Set the session save directory
(setq superchat-save-directory "~/.emacs.d/superchat/chat-notes/")

;; Set default directories for file selection
(setq superchat-default-directories '("~/Documents" "~/Projects"))
```

## Quick Start

1. Launch Superchat with `M-x superchat`
2. Enter your question after the prompt. You can press `RET` to create multi-line input.
3. Press `C-c C-c` to send the message.
4. Wait for the AI assistant's reply

### Basic Conversation Example

```
User: Hello, what can you help me with?
Assistant: I am an AI assistant that can help you answer questions, analyze code, provide suggestions, and more.
```

## Core Features Explained

### Basic Conversations

Superchat allows you to have natural language conversations with various large language models.

### Command System

Superchat provides a powerful command system that allows you to define and use custom prompts:

- Built-in commands: such as `/create-question`
- Custom command definition: using the `/define` command
- View all available commands: using the `/commands` command

After the input prompt, you can use auto-completion to view and select commands:
1. Type the [/](file:///Users/chenyibin/Documents/emacs/package/superchat/superchat.el) character
2. Continue typing the first letter of the command; if you have company or corfu plugins, select the command from the pop-up list.
3. If you don't have company or corfu, press `M-TAB` (or your Emacs environment's completion key) to trigger auto-completion and select the command from the pop-up list.

For example, to use the `/create-question` command, you can type `/c` and then press `M-TAB`, and the system will display all available commands starting with 'c' for you to choose from.

### Conversation History

Superchat now automatically includes previous messages from the current session in new prompts. This allows you to ask follow-up questions or discuss topics in more detail without having to manually re-state the context of the conversation. The number of messages included is configurable.

### File Context Support

Superchat can add file content as context to the conversation:

1. Press the `#` key after the input prompt
2. Select the file to add
3. The file content will be provided as context to the AI

You can also manually enter the file path in the format `# /path/to/file`.

When `superchat-default-directories` is set, the file selection will show all files from the specified directories in a single list, making it easier to select files from predefined locations.

## Advanced Usage

### Custom Commands

You can create custom prompts using the `/define` command:

```
/define explain-code "Please explain what the following code does: $input"
```

In addition to the `/define` command, you can create custom commands by simply adding prompt files to the `prompts` directory within your `superchat-data-directory`. The filename (without extension) will automatically become the command name. The default file extension is `.prompt`, but other formats like `.md`, `.org`, and `.txt` are also supported. For example, creating a file named `summarize.prompt` (or `summarize.txt`) with the content `Please summarize the following text: $input` will create a new `/summarize` command.

In custom prompts, you can use the following variables:
- `$input`: The user's input content
- `$lang`: The set language (defaults to English)

### Context Management

- Use the `/clear-context` command to clear all context files from the current session
- Superchat automatically manages files added to the session

### Key Bindings

- `RET` or `C-c C-c`: Send input
- `#`: Smartly add file path to context
- `C-c C-h`: Show command list
- `C-c C-s`: Save current session

## Configuration Options

The main customization options for Superchat are:

- `superchat-buffer-name`: Name of the chat buffer (defaults to "*Superchat*")
- `superchat-model`: AI model to use (if nil, gptel's default model will be used)
- `superchat-data-directory`: Data storage directory
- `superchat-save-directory`: Session save directory
- `superchat-display-single-window`: If non-nil, make the Superchat window the only one in its frame, providing a dedicated view. Enabled by default.
- `superchat-default-directories`: List of default directories for file selection
- `superchat-general-answer-prompt`: General answer prompt template

## Troubleshooting

### Common Issues

1. **Unable to connect to AI service**: Please check if your gptel configuration is correct, including API key and endpoint settings.

2. **File context not added correctly**: Ensure the file path is correct and the file is readable. You can check the diagnostic information in the messages buffer to troubleshoot the issue.

3. **Command system usage issues**: Use the `/commands` command to view all available commands and their usage.

### Debugging Suggestions

- Check if gptel's configuration is correct
- View diagnostic information in the `*Messages*` buffer
- Ensure all dependency packages are correctly installed and loaded

## License

Superchat.el is open-sourced under the GPL-3 license.

## Background

Superchat.el is a standalone Emacs AI chat client that evolved from the chat-view module of the [org-supertag](https://github.com/yibie/org-supertag) project. Unlike the original version, Superchat removes all org-supertag-specific dependencies, making it a completely independent package.
ependent package.
