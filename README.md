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

Superchat is designed to integrate seamlessly with `gptel`. All settings related to the LLM, such as the model (`gptel-model`), API key, and temperature, are automatically inherited from your `gptel` configuration. To configure the AI's behavior, please customize the `gptel` variables directly.

You can configure Superchat's own options using the following variables:

```elisp
;; Set the data storage directory
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; Set the language for $lang variable in custom commands
(setq superchat-lang "English")  ; or "中文", "Français", etc.

;; Response timeout protection (prevents UI freezing from blocking tools)
(setq superchat-response-timeout 30)  ; seconds, nil to disable

;; Smart completion detection delay (for non-streaming responses)
;; Used primarily for Ollama + tools mode
(setq superchat-completion-check-delay 2)  ; seconds, default is 2

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

### gptel Tools Integration

Superchat now fully supports gptel's tools (function calling) functionality, enabling AI to directly call external tools and services to enhance conversation capabilities.

Key features include:
- **Zero Configuration Integration**: Automatically reads your gptel tools configuration without requiring additional setup
- **Intelligent Tool Calling**: AI automatically determines and uses appropriate tools based on your needs
- **Seamless Experience**: Tool call results naturally integrate into the conversation flow
- **Smart Completion Detection**: Automatically handles non-streaming tool responses (Ollama + tools)
- **Status Monitoring**: Use `/tools` command to view currently available tool status

Usage:
1. Configure your tools in gptel:
   ```elisp
   (setq gptel-use-tools t)
   (setq gptel-tools (list ...))
   ```
2. Launch Superchat and chat normally
3. When your needs require tools, AI will automatically call relevant tools
4. Use `/tools` command to check tool status

Example conversation:
```
User: Search for the latest Emacs news
AI: [automatically calls web_search tool] I found the latest Emacs news for you...
```

#### Technical Notes on Ollama + Tools

When using Ollama with tools enabled, there are known streaming limitations:

1. **gptel's Behavior**: gptel disables streaming for Ollama + tools mode ([see gptel-request.el:2019](https://github.com/karthink/gptel/blob/master/gptel-request.el#L2019))
   ```elisp
   ;; HACK(tool): no stream if Ollama + tools. Need to find a better way
   (not (and (eq (type-of gptel-backend) 'gptel-ollama)
            gptel-tools gptel-use-tools))
   ```

2. **Ollama's Issue**: Ollama's tool calling streaming implementation has known issues ([ollama/ollama#12557](https://github.com/ollama/ollama/issues/12557))

3. **Superchat's Solution**: Implements smart completion detection that:
   - Waits for `superchat-completion-check-delay` (default 2 seconds) after receiving response
   - If no new data arrives, considers the response complete
   - Automatically enters next conversation round
   - Falls back to 30-second timeout protection if needed

**Configuration**:
```elisp
;; Adjust completion check delay if needed
;; Lower values = faster completion, higher values = more reliable
(setq superchat-completion-check-delay 2)  ; 1-5 seconds recommended
```

**Note**: This only affects Ollama + tools mode. Normal streaming conversations complete immediately via standard signals.

### Workflow Automation

Workflows let you store entire conversations-as-recipes. One prompt can run several steps (search, analysis, saving output) without retyping anything.

- **Start from chat**: Type `>workflow-name topic` and Superchat runs the matching `.workflow` file. No extra setup is required—workflows reuse the gptel tools and MCP servers you already configured.
- **Keep everything in sync**: Steps can read local files with `#path`, call models with `@model`, and finish by writing results somewhere you choose.
- **Save once, repeat often**: Put workflow files under `~/.emacs.d/superchat/workflow/` (or `superchat-data-directory/workflow/`) and reuse them whenever you need the task again.
- **Simple, linear steps**: Each non-empty line is one step executed from top to bottom. Branching/conditional flows (like n8n) are not supported yet; keep instructions in a straight sequence.

Try it:
1. Create `~/.emacs.d/superchat/workflow/ai-news-summary.workflow` with the contents below.
2. In Superchat, run `>ai-news-summary AI Memory` (or any keyword).
3. The workflow searches the web, summarizes the news, and saves the Markdown report automatically.

```text
# Workflow: AI Tech News Digest
# Description: Weekly tech news summary

/web-search Search for news related to "$input"

@qwen3-coder:30b-a3b-q8_0 Analyze the findings (business, technology, society) and produce a concise English summary

Save the summary to #~/Documents/news-summary.md
```

### MCP (Model Context Protocol) Integration

Superchat now integrates MCP (Model Context Protocol) support, allowing you to connect various external services and tools through a standardized protocol. MCP provides a unified interface to manage tools from different servers, greatly expanding the AI's capabilities.

Key features include:
- **Zero-Configuration Architecture**: Automatically detects and integrates MCP servers without manual setup
- **Real-time Status Monitoring**: Displays server connection status and available tool counts
- **Seamless Tool Integration**: MCP tools automatically integrate into gptel's tool system
- **Intelligent Server Management**: Supports starting, stopping, and monitoring multiple MCP servers

#### Installation and Configuration

1. **Install MCP package**:
   ```elisp
   ;; Using straight.el
   (straight-use-package 'mcp)
   
   ;; Or using use-package
   (use-package mcp)
   ```

2. **Configure MCP servers** (in your Emacs configuration):
   ```elisp
  ;; Example: Configure multiple MCP servers (with optional Jina API key)
  (setq mcp-hub-servers
        (let ((jina-token (getenv "JINA_API_KEY")))
          `(("filesystem" . (:command "npx"
                                   :args ("-y" "@modelcontextprotocol/server-filesystem"
                                          "/Users/yourname/Documents")))
            ("fetch" . (:command "uvx" :args ("mcp-server-fetch")))
            ("jina-mcp-server"
             . (:url "https://mcp.jina.ai/sse"
                     :headers ,(when (and jina-token (> (length jina-token) 0))
                                 `((Authorization . ,(concat "Bearer " jina-token)))))))))
   ```

#### Usage

**Check MCP Status**:
- Use `/mcp` command to view current MCP status
- Shows configured server count, running server count, and available tool count

**Start MCP Servers**:
- Use `/mcp-start` command to start MCP servers
- System automatically detects and starts configured but not-running servers
- Started tools are automatically integrated into the current gptel session

**Example Conversation**:
```
User: /mcp
System: MCP Status: Available ✓ | Configured: 1 servers | Running: 1 servers | Available tools: 15

User: /mcp-start  
System: Starting MCP servers...
Started servers: filesystem
Added 15 tools to gptel session

User: List important files in my Documents directory
AI: [using MCP filesystem tools] I found the important files in your Documents directory...
```

#### Supported MCP Commands

- `/mcp` - Display MCP status and server information
- `/mcp-start` - Start MCP servers and integrate tools

#### Common MCP Servers

Here are some popular MCP server examples:

```elisp
;; Filesystem server
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                                :args ("-y" "@modelcontextprotocol/server-filesystem" "/path/to/directory")))))

;; GitHub server
'("github" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-github")
                      :env ("GITHUB_PERSONAL_ACCESS_TOKEN" . "your_token_here")))

;; SQLite database server
'("sqlite" . (:command "npx"
                      :args ("-y" "@modelcontextprotocol/server-sqlite" "path/to/database.db")))

;; Web search server
'("brave-search" . (:command "npx"
                            :args ("-y" "@modelcontextprotocol/server-brave-search")
                            :env ("BRAVE_API_KEY" . "your_api_key")))
```

Note: MCP functionality requires the `mcp.el` package. If not installed, related commands will show friendly error messages.

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
- `/tools`: View current gptel tools status
- `/mcp`: View MCP status and server information
- `/mcp-start`: Start MCP servers and integrate tools

## Configuration Options

The main customization options for Superchat are:

- `superchat-buffer-name`: Name of the chat buffer (defaults to "*Superchat*")
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

4. **"User:" prompt doesn't appear after AI response**: Usually caused by blocking tools or network issues.
   - **Automatic protection**: SuperChat has a 30-second timeout that will auto-recover the UI
   - **If timeout message appears**: Your tools are blocking (synchronous network calls, etc.)
   - **Solution**: Increase timeout with `(setq superchat-response-timeout 60)` or fix your tools
   - **Check status**: Run `/tools` in chat to see timeout settings

5. **Response timeout warnings**: You'll see "⚠️ Response timeout after X seconds" if:
   - Your tools are making synchronous/blocking calls (e.g., `url-retrieve-synchronously`)
   - Network is very slow
   - **Fix**: Either increase `superchat-response-timeout` or make your tools asynchronous

### Debugging Suggestions

- Check if gptel's configuration is correct
- View diagnostic information in the `*Messages*` buffer
- Ensure all dependency packages are correctly installed and loaded

## CHANGLOG

### Version 0.4 (2025-10-13)
- **Workflow Integration**: Workflows now run seamlessly inside Superchat, sharing the gptel tool stack and MCP servers configured for `/prompt` sessions. Added chat shortcut (`>workflow-name`) and documentation for storing reusable `.workflow` recipes.
- **Tool Output Hardening**: Sanitized workflow result handling to avoid `json-value-p` errors when tools (e.g., Jina reader, MCP fetchers) return rich Markdown or HTML.
- **Utility Additions**: Introduced `superchat-version` constant and expanded automated tests for `superchat-tools.el`, covering Jina fallback logic and user consent branches.

### Version 0.3 (2025-10-03)
- **MCP Integration**: Integrated Model Context Protocol (MCP) support.
    - Zero-configuration architecture for automatic MCP server detection and integration.
    - Real-time status monitoring showing server connection status and available tool counts.
    - Seamless tool integration with MCP tools automatically integrated into gptel's tool system.
    - Intelligent server management supporting starting, stopping, and monitoring multiple MCP servers.
    - Added `/mcp` and `/mcp-start` commands for MCP management.
- **gptel Tools Integration**: Zero-configuration integration of gptel's tool calling functionality.
    - Directly reads user-configured tools in gptel without redundant configuration.
    - Seamless use of gptel's tool calling functionality in chat interface.
    - Supports function calling capabilities to invoke external tools and APIs.
    - Added `/tools` command to view current tools status.
- **@ Model Switching**: Support for switching between different AI models using `@model` syntax directly in conversations.
    - Quick model switching using `@model_name` syntax in chat input.
    - Added `/models` command to view available model list.
    - Temporary model switching with automatic restoration after single request.
- **Bug Fixes**: Fixed command system initialization and session management issues.

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
