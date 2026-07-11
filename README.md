# superchat

superchat is a chat interface for large language models inside Emacs, built on a hook pipeline that turns every feature — memory, skills, workflows, tool calls — into a plugin in the same shape. If you want a Claude-Code-style chat in your editor but also want to write your own context-builders and post-turn side effects without forking the project, this is for you.

<!-- TODO: screenshot of @model switching -->

## Why superchat

If you already have gptel, ellama, or chatgpt-shell installed, here's what superchat adds:

| Feature                              | gptel | ellama | chatgpt-shell | superchat |
|--------------------------------------|:-----:|:------:|:-------------:|:---------:|
| Multi-provider via `llm.el`          | yes   | yes    | (own)         | yes       |
| Streaming                            | yes   | yes    | yes           | yes       |
| MCP tools                            | (3rd-party) | no | no       | yes       |
| Per-turn `@model` override syntax    | no    | no     | no            | **yes**   |
| `#file` reference syntax             | (region) | (region) | no       | **yes**   |
| SKILL.md skill format (frontmatter + body) | no | no | no       | **yes**   |
| Multi-step async workflows           | no    | no     | no            | **yes**   |
| SQLite-backed persistent memory with FTS5 | no | no | no         | **yes**   |
| Hook-based extension model           | no    | no     | no            | **yes**   |

Pick superchat when you want to:
- Switch models mid-conversation with `@gpt-4o` or `@claude-sonnet`.
- Attach files inline with `#path/to/file.el` and have the content inlined into the prompt.
- Write reusable skills as markdown files with frontmatter (`SKILL.md` format).
- Search your past conversations with `/recall` and have relevant memories automatically attached to every turn.
- Extend the prompt pipeline by writing a `(turn)` hook function and `(add-hook)` — no fork, no monkey-patch.

Pick gptel or ellama when your needs are simpler: single-model, single-provider, one-off chats without memory or workflows.

## Install

**Requirements:** Emacs 28.1 or higher, and [llm.el](https://github.com/ahyatt/llm) ≥ 0.24 from GNU ELPA.

superchat is not yet on MELPA — install from source:

```elisp
(add-to-list 'load-path "/path/to/superchat")
(require 'superchat)
```

Configure a provider. superchat talks to any backend that `llm.el` supports:

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

Optional: [mcp.el](https://github.com/lizqwerscott/mcp.el) for MCP server tool integration.

## 5-minute tour

Open the chat with `M-x superchat`. Here are five things you can do in your first session:

**1. Plain chat with streaming**

```
User: explain what a zettelkasten is in 3 sentences
Assistant: A Zettelkasten is a personal knowledge management method...
```

**2. Switch models mid-conversation**

```
User: @gpt-4o explain how SQLite FTS5 works
```

The `@model` override applies only to this turn. Your default model resumes on the next message.

**3. Attach a file as context**

```
User: #src/main.py what does this function do?
```

The file content is inlined into the prompt. superchat strips the `#path` from your query so the model sees only "what does this function do?" plus the file content.

**4. Remember and recall**

```
User: /remember LLM temperature controls randomness; 0 is deterministic, 1 is creative.
System: Memory added: LLM temperature controls randomness

User: /recall temperature
System: Retrieved 1 memory — will be attached to your next message.

User: how should I set temperature for code generation?
```

Memories are retrived with SQLite FTS5 and automatically attached to turns when your query contains relevant terms.

**5. Invoke a skill**

```
User: >code-review #src/utils.el
```

Pre-written skills live in `skills/*.md`. Built-in skills include `code-review`, `planning`, and `refactor`. See [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md) to write your own.

## Core concepts

### Skills (SKILL.md)

Skills are reusable system prompts packaged as markdown with YAML frontmatter. They live in `skills/*.md` and are invoked with `>skill-name`.

A `type: prompt` skill (the default) injects the body as the system prompt for that turn:

```markdown
---
name: explain-region
description: Explain a code region to a senior engineer new to this codebase
version: "1.0"
type: prompt
triggers: ["explain this", "这段代码什么意思"]
---

You are a senior engineer explaining code to a peer. Focus on:
1. What this code actually does (not what it's supposed to do)
2. Why it's structured this way
3. Any non-obvious edge cases
```

A `type: agent` skill activates a multi-turn agent preset with tool access; `type: plan` activates read-only planning mode. Multi-step recipes are standalone `.workflow` files, not a skill type (see Workflows below).

Field rules: `name` and `description` are required. `version` defaults to `"1.0"`. `type` defaults to `"prompt"` (valid values: `"prompt"`, `"agent"`, `"plan"`). `triggers` defaults to `[]`.

See `examples/standard-skills/` for more complete examples, and [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md) for the full quickstart.

### Workflows (.workflow files)

Workflows are multi-step linear recipes in standalone `.workflow` files under `<superchat-data-directory>/workflow/`. Each non-empty, non-comment line is one step; steps run sequentially through an **async callback chain**, so Emacs stays responsive for the whole run. Invoke with `>>name [args]` (`>` is reserved for skills, `>>` for workflows) or `/workflow <name> [args]`.

```
# research.workflow — lines starting with # are comments, blank lines are skipped.
Search for the latest information about "$input" on the web. Provide a comprehensive overview.

# Step 2 sees step 1's output through $result.
Based on this information: $result, summarise the three most important points concisely.

# Any earlier step is addressable as $stepN.
Step 1 found: $step1. Step 2 summarised: $step2. Write one concluding sentence about $input.
```

Variables: `$input` (the trigger argument), `$result` (previous step's output), `$stepN` (step N's output), `$lang`, `$date`.

Per-step annotations: `@model-name` at the start of a line overrides the model for that step; `/command args` at the start of a line expands a user-defined command's prompt template with its args bound to `$input`; `#path/to/file` attaches a context file (the token must contain `.` or `/`, so prose like "issue #42" is left alone).

There are no branches or conditionals — workflows are linear scripts. A step that fails stops the chain with an error notice. See `examples/research.workflow` for a runnable sample. Legacy `type: workflow` SKILL.md files can be produced from `.workflow` files with `M-x superchat-workflow-import-legacy-dir`.

### Memory

Memory is a SQLite database with FTS5 full-text search. Two commands control it:

- `/remember [text]` — capture text (or the last exchange, with no argument).
- `/recall <keywords>` — search memory and attach results to your next turn.

Memory auto-retrieves on every turn when your query is long enough (configurable via `superchat-memory-auto-recall-min-length`, default 8 characters). The retrieval results are prepended to your prompt by the `superchat-prompt-hook--memory-context` hook in `superchat-prompt-hooks.el:132`.

The database is at `~/.emacs.d/superchat/memory.db` (or your `superchat-data-directory`). You can inspect it with any SQLite client:

```bash
sqlite3 ~/.emacs.d/superchat/memory.db "SELECT title FROM memories LIMIT 5"
```

Memory tuning is under `M-x customize-group RET superchat-memory RET`.

### MCP

superchat integrates with [mcp.el](https://github.com/lizqwerscott/mcp.el). Configure servers by setting `mcp-hub-servers`, then start them with `/mcp-start` and check status with `/mcp`.

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx"
                        :args ("-y" "@modelcontextprotocol/server-filesystem"
                               "/Users/you/Documents")))))
```

MCP tools are automatically collected and passed to `llm-chat` / `llm-chat-streaming` alongside built-in tools.

### The hook pipeline (advanced)

Every turn flows through `superchat-core-run-turn` (`superchat-core.el:118`):

```
parse (@model, >skill, /command, #file)
  → system-prompt hooks
    → build-prompt hooks
      → post-turn hooks (side effects)
```

Each hook is a function `(turn) → modified-turn or nil`. The turn struct (`superchat-core.el:22`) carries all per-turn state: `clean-input`, `system-prompt`, `prompt`, `retrieved-memories`, `context-files`, etc.

To write your own prompt builder:

```elisp
(defun my-git-blame-hook (turn)
  "Prepend git-blame for the current buffer to the prompt."
  (when-let ((buf (current-buffer))
             (file (buffer-file-name buf))
             (blame (shell-command-to-string
                     (format "git blame -L 1,20 -- %s" file))))
    (setf (superchat-turn-prompt turn)
          (concat "Git blame context:\n" blame "\n\n"
                  (superchat-turn-prompt turn))))
  turn)

(add-hook 'superchat-build-prompt-functions #'my-git-blame-hook)
```

This is the architecture that makes superchat different from other Emacs LLM clients: features are hooks, and you can write your own.

See [docs/architecture.md](docs/architecture.md) for the full technical walkthrough.

## Customization

The most-used defcustoms (see `M-x customize-group RET superchat RET` for all):

- `superchat-llm-backend` — the `llm.el` provider struct. **Must be set** before `M-x superchat`.
- `superchat-llm-model` — optional `:chat-model` override; used by `@model` switch.
- `superchat-llm-streaming` — `t` for streaming (default), `nil` for blocking.
- `superchat-llm-tool-names` — built-in tool allowlist. Defaults to small read-only set; `'all` for the full library.
- `superchat-data-directory` — where memory, config, and commands live (default `~/.emacs.d/superchat/`).
- `superchat-lang` — language for `$lang` variable in custom commands (default `"English"`).
- `superchat-response-timeout` — seconds before a stuck response times out (default 120).
- `superchat-context-message-count` — how many recent messages to include in the prompt.
- `superchat-conversation-history-limit` — how many messages to keep in the buffer.
- `superchat-display-single-window` — dedicate the frame to superchat (default `t`).

## Configuration cheatsheet

```elisp
;; Minimum working config
(setq superchat-llm-backend
      (make-llm-openai
       :key (getenv "OPENAI_API_KEY")
       :chat-model "gpt-4o-mini"))

;; Data directory (default is ~/.emacs.d/superchat/)
(setq superchat-data-directory "~/.emacs.d/superchat/")

;; Language for $lang in custom commands
(setq superchat-lang "English")

;; History size
(setq superchat-context-message-count 10)
(setq superchat-conversation-history-limit 50)
```

## Troubleshooting

**"superchat-llm-backend is unconfigured"**

Set `superchat-llm-backend` to a provider struct before opening the chat. Run `/backend` to verify.

**"Model not found" for `@model`**

Run `/models` to see available models. Use `/refresh-models` to reload from the backend. Or add extras with `(setq superchat-manual-models '("model-id-1" "model-id-2"))`.

**Tool calls timing out**

Built-in tools (web fetch, file system) and MCP tools have a timeout multiplier of 1.5× applied to `superchat-response-timeout` (default 120s). If a tool blocks the response for longer than 180s total, superchat aborts the turn. This is most common with local models (Ollama) where the LLM itself takes time to produce tool-call JSON before the tool can even execute. Raise both values: `(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`. If `superchat-show-ttft` is `t`, you'll see the time-to-first-token in the echo area, which helps diagnose whether the LLM or the tool is the bottleneck.

**Memory not persisting**

Check `superchat-data-directory` and that the SQLite database is writable. Run `/recall test` to verify queries work. Memory auto-retrieval only activates for queries longer than 8 characters (configurable via `superchat-memory-auto-recall-min-length`).

**"User:" prompt doesn't appear after response**

Usually a blocking tool call. superchat auto-recovers after `superchat-response-timeout` (120s). If it happens often, check your MCP servers or increase the timeout.

**"Symbol's value as variable is void: superchat--dispatch-menu-transient"**

This occurs when `evil-collection` attempts to bind keys in superchat's transient dispatch menu and evaluates the symbol as a variable. Remove `superchat` from `evil-collection-mode-list`:

```elisp
(setq evil-collection-mode-list
      (remove 'superchat evil-collection-mode-list))
```

Or disable evil-collection for superchat entirely in your `evil-collection` use-package block.

## Project status & roadmap

v1.2.0 shipped. What's stable: chat, streaming, model switching, skills, memory, MCP, tool calling, agent mode with sub-agent delegation, and the hook pipeline. New in v1.2: the async `.workflow` engine (`>>name [args]` or `/workflow <name>`) with cross-step variables (`$result`, `$stepN`), a region-based shared workspace for multi-agent state (`workspace_read`/`workspace_write` tools), and per-tool lifecycle hooks in agent mode. See [ROADMAP.md](ROADMAP.md) for planned work.

## License

GPL-3.0-or-later.

## Background

superchat originated as the chat-view module of [org-supertag](https://github.com/yibie/org-supertag). It has been fully independent since v0.1 and no longer carries any org-supertag dependency.
