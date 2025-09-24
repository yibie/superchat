[中文](./README_cn.md)

# SuperChat

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

;; Set the language for $lang variable in custom commands
(setq superchat-lang "English")  ; or "中文", "Français", etc.

;; Set default directories for file selection
(setq superchat-default-directories '("~/Documents" "~/Projects"))
```

Note: Superchat now automatically manages its directory structure. The `superchat-save-directory` and `superchat-command-dir` variables have been removed. Directories are now created dynamically as needed, or you can use `M-x superchat-ensure-directories` to manually ensure all directories exist.

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

### Memory System

Superchat now features a persistent and queryable memory system, allowing the AI to remember past conversations and leverage that knowledge in future interactions. This system is built on Org-mode files, ensuring transparency and user control.

Key aspects include:
- **Tiered Memory Capture**: Automatically captures important insights from conversations (LLM-summarized) and allows explicit user commands to save memories.
- **Intelligent Retrieval**: Uses LLM-extracted keywords from your queries to find the most relevant memories, which are then provided as context to the AI.
- **Memory Lifecycle Management**:
    - **Scoring & Decay**: Memories are scored based on utility, with scores decaying over time.
    - **Automatic Archiving**: Less relevant memories are automatically moved to an archive file, keeping the active memory lean.
    - **Automatic Merging**: Similar or duplicate memories can be automatically consolidated by the LLM into a single, more comprehensive entry. (Note: This feature is enabled by default, merging daily.)

Configuration options for the memory system are available under `M-x customize-group RET superchat-memory RET`.

## Advanced Usage

### Custom Commands

You can create custom prompts using the `/define` command:

```
/define explain-code "Please explain what the following code does: $input"
```

In addition to the `/define` command, you can create custom commands by simply adding prompt files to the `command` directory within your `superchat-data-directory`. The filename (without extension) will automatically become the command name. The default file extension is `.prompt`, but other formats like `.md`, `.org`, and `.txt` are also supported. For example, creating a file named `summarize.prompt` (or `summarize.txt`) with the content `Please summarize the following text: $input` will create a new `/summarize` command.

In custom prompts, you can use the following variables:
- `$input`: The user's input content
- `$lang`: The set language (defaults to English)

#### Setting the Language for $lang Variable

The `$lang` variable in custom commands can be configured in several ways:

**Method 1: Through Emacs Customization Interface (Recommended)**
```elisp
M-x customize-variable RET superchat-lang RET
```
Then change the value from "English" to your preferred language (e.g., "中文", "Français", "Español") and save the settings.

**Method 2: In Configuration File**
Add to your Emacs configuration file:
```elisp
(setq superchat-lang "中文")        ; For Chinese
(setq superchat-lang "Français")    ; For French
(setq superchat-lang "Español")     ; For Spanish
```

**Method 3: Temporary Setting (Current Session)**
Execute in Emacs:
```elisp
M-x eval-expression RET (setq superchat-lang "中文") RET
```

**Example Usage:**
When you set `(setq superchat-lang "中文")` and use the built-in `/create-question` command:
- Template: `"Please list all important questions related to $input in $lang."`
- With input "git": `"Please list all important questions related to git in 中文."`

The language setting is dynamically retrieved each time you send a message, so you can change it anytime and it will take effect immediately.

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
- `superchat-lang`: Language setting for the `$lang` variable in custom commands (defaults to "English")
- `superchat-display-single-window`: If non-nil, make the Superchat window the only one in its frame, providing a dedicated view. Enabled by default.
- `superchat-default-directories`: List of default directories for file selection
- `superchat-general-answer-prompt`: General answer prompt template
- `superchat-context-message-count`: Number of most recent conversation messages to include in prompts.
- `superchat-conversation-history-limit`: Maximum number of conversation messages to retain in memory.

### Memory System Configuration

These options control Superchat's memory system. You can customize them via `M-x customize-group RET superchat-memory RET`.

- `superchat-memory-file`: Path to the main memory Org file (defaults to `memory.org` in data directory).
- `superchat-memory-archive-file`: Path to the archived memory Org file (defaults to `memory-archive.org` in data directory).
- `superchat-memory-explicit-trigger-patterns`: Regexp patterns that identify Tier 1 explicit memory commands in user text.
- `superchat-memory-auto-capture-enabled`: When non-nil, Tier 2 automatic captures may run after each exchange.
- `superchat-memory-auto-capture-minimum-length`: Minimum assistant response length to trigger auto-capture.
- `superchat-memory-use-org-ql-cache`: When non-nil and org-ql is available, enable `org-ql-cache` during queries.
- `superchat-memory-max-results`: Maximum number of memories returned per retrieval.
- `superchat-memory-auto-recall-min-length`: Minimum query length before automatic memory retrieval runs.
- `superchat-memory-llm-timeout`: Maximum seconds to wait for synchronous LLM utilities before falling back.
- `superchat-memory-title-weight`: Score weight applied when a query term matches the title.
- `superchat-memory-body-weight`: Score weight applied when a term matches the body content.
- `superchat-memory-keyword-weight`: Score weight applied when a term matches stored keywords.
- `superchat-memory-tag-weight`: Score weight applied when a term matches tags.
- `superchat-memory-access-weight`: Score contribution from the entry's `:ACCESS_COUNT:`.
- `superchat-memory-recency-weight`: Score contribution based on how recent the entry is.
- `superchat-memory-recency-half-life-days`: Half-life in days used when computing recency decay.
- `superchat-memory-auto-increment-access-count`: Automatically increment `:ACCESS_COUNT:` for retrieved memories.
- `superchat-memory-decay-factor`: Multiplier applied to access counts during decay.
- `superchat-memory-decay-min-access`: Lower bound for access counts after decay.
- `superchat-memory-archive-threshold`: Access count threshold for archiving memories.
- `superchat-memory-auto-prune-interval-days`: Interval in days for automatic memory pruning (0 or `nil` to disable).
- `superchat-memory-merge-similarity-threshold`: Jaccard similarity threshold for considering two memories for merging.
- `superchat-memory-auto-merge-interval-days`: Interval in days for automatic memory merging (0 or `nil` to disable). WARNING: This feature carries risks.

### New Functions

- `superchat-ensure-directories`: Interactive function to manually ensure all necessary directories exist. Usage: `M-x superchat-ensure-directories`

## Troubleshooting

### Common Issues

1. **Unable to connect to AI service**: Please check if your gptel configuration is correct, including API key and endpoint settings.

2. **File context not added correctly**: Ensure the file path is correct and the file is readable. You can check the diagnostic information in the messages buffer to troubleshoot the issue.

3. **Command system usage issues**: Use the `/commands` command to view all available commands and their usage.

### Debugging Suggestions

- Check if gptel's configuration is correct
- View diagnostic information in the `*Messages*` buffer
- Ensure all dependency packages are correctly installed and loaded

## CHANGLOG

### Version 0.2 (2025-09-23)
- **Memory System**: Introduced a comprehensive memory system for the AI, enabling persistent learning from conversations.
    - Tiered memory capture (explicit, automatic LLM-summarized).
    - LLM-powered query keyword extraction for intelligent retrieval.
    - Memory scoring, decay, and automatic archiving.
    - Automatic LLM-driven merging of similar memories (opt-in, disabled by default).
- **Bug Fixes**: Addressed various stability and compatibility issues.

## License

Superchat.el is open-sourced under the GPL-3 license.

## Background

Superchat.el is a standalone Emacs AI chat client that evolved from the chat-view module of the [org-supertag](https://github.com/yibie/org-supertag) project. Unlike the original version, Superchat removes all org-supertag-specific dependencies, making it a completely independent package.
