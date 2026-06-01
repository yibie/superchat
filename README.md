# SuperChat

## Introduction

A friendly, Claude Code–style chat UI for Emacs via [llm.el](https://github.com/ahyatt/llm). Superchat makes structured prompts and file‑grounded conversations effortless — no new infrastructure, just your editor.

- "/" for commands with completion, plus easy custom command creation.
- "#" to attach files as part of the message you send to the LLM.
- Clean, fast chat experience with streaming and readable output.
- Works out‑of‑the‑box with your existing llm.el setup.

Key features include:
- Retaining the complete command system
- Adding the ability to include files as context in conversations
- Supporting conversations with various large language models (LLMs)
- Open-sourced under the GPL-3 license

## Installation and Configuration

### Requirements

- Emacs 28.1 or higher
- [llm](https://github.com/ahyatt/llm) 0.7 (GNU ELPA) — pulls in `plz`, `plz-event-source`, `plz-media-type`, and `compat` as transitive dependencies

Optional:
- [mcp](https://github.com/lizqwerscott/mcp.el) — for MCP server tool integration
- [org-ql](https://github.com/alphapapa/org-ql) — for cached memory queries

### Installation Steps

1. Place the superchat file in your Emacs load path
2. Add the following code to your Emacs configuration file (usually `~/.emacs.d/init.el`):

```elisp
(add-to-list 'load-path "/path/to/superchat")
(require 'superchat)
```

### Basic Configuration

Superchat is designed to integrate seamlessly with `llm.el`. You build an llm provider struct once and assign it to `superchat-llm-backend`; history, streaming, tools, and MCP all flow from there.

```elisp
;; 1. Pick a provider and set superchat-llm-backend.

;; OpenAI:
(setq superchat-llm-backend
      (make-llm-openai
       :key (getenv "OPENAI_API_KEY")
       :chat-model "gpt-4o-mini"))

;; Anthropic Claude:
;; (setq superchat-llm-backend
;;       (make-llm-claude
;;        :key (getenv "ANTHROPIC_API_KEY")
;;        :chat-model "claude-3-5-sonnet-20241022"))

;; Local Ollama:
;; (setq superchat-llm-backend
;;       (make-llm-ollama
;;        :chat-model "llama3.1"))

;; 2. (Optional) override the chat model without rebuilding the backend.
(setq superchat-llm-model "gpt-4o")

;; 3. (Optional) turn streaming off if you need fully-buffered responses.
;;    Default is t (streaming on, uses llm-chat-streaming).
;;    Set to nil to use blocking llm-chat instead.
(setq superchat-llm-streaming t)
```

You can also configure Superchat's own options:

```elisp
;; Set the data storage directory
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; Set the language for $lang variable in custom commands
(setq superchat-lang "English")  ; or "中文", "Français", etc.

;; Response timeout protection (prevents UI freezing from blocking tools)
(setq superchat-response-timeout 120)  ; seconds, nil to disable

;; Smart completion detection delay (used when streaming is off)
(setq superchat-completion-check-delay 2)  ; seconds, default is 2

;; Set default directories for file selection
(setq superchat-default-directories '("~/Documents" "~/Projects"))
```

Note: Superchat automatically manages its directory structure. The `superchat-save-directory` and `superchat-command-dir` variables have been removed. Directories are now created dynamically as needed, or you can use `M-x superchat-ensure-directories` to manually ensure all directories exist.

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
1. Type the `/` character
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

### Tool Calling

Superchat ships with a built-in tool registry that the configured llm backend can call automatically. No tool config is required on your end — the registry is populated the first time llm.el is available, and the tools are passed to every `llm-chat` / `llm-chat-streaming` call as the `:tools` keyword.

The 13 built-in tools cover shell, file I/O, search, and buffer editing:

- `shell-command`, `read-file`, `list-files`, `search-text`, `find-files`
- `write-file`, `append-file`, `quick-write`, `make_directory`
- `read_buffer`, `append_to_buffer`, `EditBuffer`, `ReplaceBuffer`

Use the `/backend` command in chat to inspect what's wired up at runtime:

```
User: /backend
Backend: llm.el
Provider: openai
Model: gpt-4o-mini
Streaming: yes
Tools: 13 registered
MCP tools: 0 registered
```

(`/tools` is kept as an alias of `/backend` for back-compat with pre-v0.5 muscle memory.)

You can override or extend the registry programmatically:

```elisp
;; Disable the built-in registry entirely
(setq superchat-llm-tools-list nil)

;; Or append your own tool built with `llm-make-tool'
(setq superchat-llm-tools-list
      (append superchat-llm-tools-list
              (list (llm-make-tool
                     :name "my-tool"
                     :description "Does X"
                     :args (list ...)
                     :function (lambda (args) ...)))))
```

#### Notes on Tool Calling with Local Models

If you are using Ollama (or any local model) with tools enabled, response latency is typically higher than with cloud models. Two tunables are exposed:

```elisp
;; Buffer the per-token stream for a moment after the last chunk, so a
;; silent tool-call round-trip doesn't look like a dropped connection.
(setq superchat-completion-check-delay 2)  ; 1-5 seconds recommended

;; Stretch the response timeout proportionally to the base.
(setq superchat-tool-timeout-multiplier 1.5)  ; 1.0 to disable scaling
```

If you see "⚠️ Response timeout" warnings, raise `superchat-response-timeout` or `superchat-tool-timeout-multiplier` first.

### Workflow Automation

Workflows let you store entire conversations-as-recipes. One prompt can run several steps (search, analysis, saving output) without retyping anything.

- **Start from chat**: Type `>workflow-name topic` and Superchat runs the matching `.workflow` file. No extra setup is required — workflows reuse the llm tools and MCP servers you already configured.
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

Superchat integrates MCP (Model Context Protocol) support, allowing you to connect various external services and tools through a standardized protocol. MCP provides a unified interface to manage tools from different servers, greatly expanding the AI's capabilities.

Key features include:
- **Zero-Configuration Architecture**: Automatically detects and integrates MCP servers without manual setup
- **Real-time Status Monitoring**: Displays server connection status and available tool counts
- **Seamless Tool Integration**: MCP tools automatically integrate into the configured llm backend's tool system
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
             ("fetch" . (:url "https://mcp.jina.ai/sse"
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
- Started tools are automatically integrated into the current llm session

**Example Conversation**:
```
User: /mcp
System: MCP Status: Available ✓ | Configured: 1 servers | Running: 1 servers | Available tools: 15

User: /mcp-start  
System: Starting MCP servers...
Started servers: filesystem
Added 15 tools to llm session

User: List important files in my Documents directory
AI: [using MCP filesystem tools] I found the important files in my Documents directory...
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
(setq mcp-hub-servers
      '(("github" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-github")
                               :env ("GITHUB_PERSONAL_ACCESS_TOKEN" . "your_token"))))

;; SQLite database server
(setq mcp-hub-servers
      '(("sqlite" . (:command "npx"
                               :args ("-y" "@modelcontextprotocol/server-sqlite" "path/to/database.db")))))

;; Web search server
(setq mcp-hub-servers
      '(("brave-search" . (:command "npx"
                                    :args ("-y" "@modelcontextprotocol/server-brave-search")
                                    :env ("BRAVE_API_KEY" . "your_key")))))
```

Note: MCP functionality requires the `mcp.el` package. If not installed, related commands will show friendly error messages.

### Memory System

Superchat features a persistent and queryable memory system, allowing the AI to remember past conversations and leverage that knowledge in future interactions. This system is built on Org-mode files, ensuring transparency and user control.

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

In addition to the `/define` command, you can create custom commands by simply adding prompt files to the `command` directory within your `superchat-data-directory`. The filename (without extension) will automatically become the command name. The default file extension is `.prompt`, but other formats like `.md`, `.org`, and `.txt` are also supported.

**Adding Descriptions:**
To display a description for your command in the `/commands` list, use the filename format `NAME-DESCRIPTION.prompt`. Superchat automatically parses the text after the first hyphen as the command's purpose.

Examples:
- `summarize.prompt` -> Command: `/summarize` (No description)
- `seo-optimize_website_content.prompt` -> Command: `/seo`, Description: "optimize website content"

The file content (e.g., `Please summarize the following text: $input`) serves as the prompt template.

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
- `/backend`: View the active llm backend, model, streaming mode, and tool/MCP counts (`/tools` is a back-compat alias)
- `/mcp`: View MCP status and server information
- `/mcp-start`: Start MCP servers and integrate tools

## Configuration Options

The main customization options for Superchat are:

- `superchat-buffer-name`: Name of the chat buffer (defaults to "*superchat*")
- `superchat-data-directory`: Data storage directory
- `superchat-lang`: Language setting for the `$lang` variable in custom commands (defaults to "English")
- `superchat-display-single-window`: If non-nil, make the Superchat window the only one in its frame, providing a dedicated view. Enabled by default.
- `superchat-default-directories`: List of default directories for file selection
- `superchat-general-answer-prompt`: General answer prompt template
- `superchat-context-message-count`: Number of most recent conversation messages to include in prompts.
- `superchat-conversation-history-limit`: Maximum number of conversation messages to retain in memory.
- `superchat-response-timeout`: Hard ceiling in seconds for any single response (set to nil to disable). Default 120.
- `superchat-completion-check-delay`: When streaming is off, wait this many seconds after the last chunk before treating the response as done. Default 2.
- `superchat-tool-timeout-multiplier`: Multiplier applied to `superchat-response-timeout` when tools are in play. Default 1.5.
- `superchat-show-response-mode`: If non-nil, show a short "🤖 Synchronously generating..." / "🧠 Streaming..." indicator before each response. Default t.

**llm.el integration:**

- `superchat-llm-backend`: The llm provider struct (created by `make-llm-openai`, `make-llm-claude`, `make-llm-ollama`, etc.). **Must be set before `M-x superchat`.** Default nil (unconfigured).
- `superchat-llm-model`: Optional override for `:chat-model`. When non-nil, this is forwarded to `llm-chat` / `llm-chat-streaming` and used by the `@model` switch.
- `superchat-llm-streaming`: If non-nil (default), use `llm-chat-streaming` for incremental token output. If nil, use blocking `llm-chat`.
- `superchat-manual-models`: Optional list of model IDs to surface in `/models` when the backend's `:chat-model` enumeration is empty or you want to inject extras.
- `superchat-llm-tools-list`: Cached list of tool structs built by `llm-make-tool`. Auto-populated on first call to `superchat-get-llm-tools`; set to nil to disable the built-in registry, or `append` to it to add your own.

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
- `superchat-llm-tools-reload`: Force a reload of the built-in tool registry (useful after adding or removing tools). Usage: `M-x superchat-llm-tools-reload`

## Troubleshooting

### Common Issues

1. **"superchat-llm-backend is unconfigured" / chat refuses to start**: Set `superchat-llm-backend` to a provider struct from `make-llm-openai`, `make-llm-claude`, or `make-llm-ollama` before opening the chat. Run `/backend` after configuring to confirm the provider, model, and tool counts.

2. **Unable to connect to AI service**: Please check your llm.el configuration — `:key`, endpoint, and `:chat-model` on the provider struct. The same struct should work outside Superchat via `llm-chat`.

3. **File context not added correctly**: Ensure the file path is correct and the file is readable. You can check the diagnostic information in the messages buffer to troubleshoot the issue.

4. **Command system usage issues**: Use the `/commands` command to view all available commands and their usage.

5. **"User:" prompt doesn't appear after AI response**: Usually caused by blocking tools or network issues.
   - **Automatic protection**: SuperChat has a `superchat-response-timeout` (default 120s) that will auto-recover the UI
   - **If timeout message appears**: Your tools are blocking (synchronous network calls, etc.)
   - **Solution**: Increase timeout with `(setq superchat-response-timeout 300)` or fix your tools
   - **Check status**: Run `/backend` in chat to see streaming + tool counts

6. **Response timeout warnings**: You'll see "⚠️ Response timeout after X seconds" if:
   - Your tools are making synchronous/blocking calls (e.g., `url-retrieve-synchronously`)
   - Network is very slow
   - **Fix**: Either increase `superchat-response-timeout` or `superchat-tool-timeout-multiplier`

### Debugging Suggestions

- Check that `superchat-llm-backend` is non-nil and the underlying provider works outside Superchat (try `llm-chat` directly in a scratch buffer)
- View diagnostic information in the `*Messages*` buffer
- Ensure all dependency packages (llm, mcp, org-ql) are correctly installed and loaded

## CHANGELOG

### Version 0.6 (2026-06-01)
- **Memory-Soul dual-track separation**. The memory subsystem now keeps two distinct tracks: the existing synthesized `memory.org` (knowledge, scored, decayed) and a new raw-event `soul.org` (verbatim, with mood + context, never auto-merged). The critique behind the split: summarization loses context/emotion, merging erases natural contradictions, and decay forgets that "the feeling then" sometimes matters.
- **New `superchat-memory-add-raw`** with `:mood`, `:context`, `:verbatim` keyword args. Writes a tagged raw event to `soul.org` with `:ID:`, `:TIMESTAMP:`, `:MOOD:`, `:TYPE:`, `:CONTEXT:`, and `:VERBATIM:` properties. Returns the entry id. No LLM required — the soul track is just an Org log.
- **Contradiction coexistence**. New `:CONTRADICTION:` tag (comma/whitespace-separated ids), `:VALIDITY: expired` property, and `:REPLACED_BY:` link. Contradictions are *kept*, not merged — both the new and the old entry persist so the user can see what changed and when.
- **Bidirectional paired-expired surfacing**. `superchat-memory-retrieve-with-context` now detects both outgoing contradictions (the entry's own `:CONTRADICTION:`) and incoming ones (another entry points at this one), and attaches `:paired-expired` + `:contradiction-ids` plist keys when the query hits the topic. Capped at `superchat-memory-contradiction-context-window`.
- **Manual review UI**. `superchat-memory-review-mode` minor mode (y/n/e/s/q keybindings) for one-keystroke accept/reject of synthesized memories. `M-x superchat-memory-review-pending` opens the review buffer; entries stay in the queue across sessions (they live in the memory file with `:REVIEWED: nil`).
- **Mood tag taxonomy**. New `defcustom superchat-memory-mood-taxonomy` with sensible defaults (curious, frustrated, tired, satisfied, neutral). `--resolve-mood` falls back to "neutral" for unknown values.
- **New defcustoms**:
    - `superchat-memory-soul-file` (nil → `<data-dir>/soul.org`): the soul log file.
    - `superchat-memory-soul-synthesis-mode` (`manual`): when to run `superchat-memory-synthesize-soul` — `manual`, `weekly`, or `never`.
    - `superchat-memory-contradiction-context-window` (3): max paired-expired entries surfaced per query.
    - `superchat-memory-mood-taxonomy` (list of strings): recognized mood tags.
- **`superchat-memory-synthesize-soul`** is manual-trigger only. The `auto-merge` default is replaced by the review queue — the user reviews what the soul track suggests, not the LLM silently merging memories. The function is a no-op when gptel/llm is absent.
- **Test coverage**: new `test/test-memory-soul.el` (23 tests, all pass) covers file I/O, contradiction parsing, mood resolution, paired-expired retrieval, review queue, and keymap bindings. Runs without gptel, llm, or org-ql.

### Version 0.5 (Unreleased)
- **Backend hard-swap: gptel → llm.el** (BREAKING). Superchat no longer ships gptel / gptel-context as runtime dependencies. The chat transport is now `llm-chat` / `llm-chat-streaming` from [llm.el](https://github.com/ahyatt/llm) (GNU ELPA, 0.7). Configure via the new `superchat-llm-backend` defcustom with `make-llm-openai` / `make-llm-claude` / `make-llm-ollama` / etc.
- **Emacs 28.1 required** (was 27.1). llm.el's `plz` dependency requires it.
- **Removed `superchat-agent.el`**. The WIP gptel-agent integration was not documented, not tested, and `gptel-agent` is not on MELPA. Anything that used to call into it should fall back to standard llm tool calling.
- **Removed gptel-specific workaround** (`gptel-curl--stream-cleanup :around` advice). The plz-based llm.el transport does not exhibit the upstream sentinel bug it was working around.
- **New defcustoms**:
    - `superchat-llm-backend` (nil): the llm provider struct.
    - `superchat-llm-model` (nil): optional `:chat-model` override used by the `@model` switch.
    - `superchat-llm-streaming` (t): use `llm-chat-streaming` when non-nil, `llm-chat` when nil.
    - `superchat-manual-models` (nil): extra model IDs surfaced by `/models` when the backend's enumeration is empty.
- **New `/backend` command** (and slash-command help entry). Reports the configured backend struct, provider name, chat model, streaming mode, built-in tool count, and MCP tool count. The old `/tools` slash command is now an alias of `/backend` for back-compat.
- **Built-in tool registry** in `superchat-llm-tools-list`. The 13 tools that used to be registered as gptel tools are now built with `llm-make-tool` and auto-populated the first time llm.el is available. The cache can be cleared with `M-x superchat-llm-tools-reload`.
- **Test suite updated**: 9 gptel-specific test files deleted (`test-gptel-*`, `test-real-gptel-models`, `test-model-*`, `test-haip`, `test-completion-realistic`). New `test/test-llm-backend.el` (28 tests) covers the new backend show command, sync/async dispatchers, model switching, and the tool registry. The canonical entry point `emacs -Q -l test/run-tests.el` now also runs these.

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
