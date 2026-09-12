# superchat

superchat is a chat interface for large language models inside Emacs, built on a small plugin Kernel: every feature — memory, skills, workflows, tool calls, even the driver and the model loop — is a plugin of the same shape, mounted from one list and withdrawable at runtime. If you want a Claude-Code-style chat in your editor but also want to write your own context collectors, tools and lifecycle listeners without forking the project, this is for you.

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
| Plugin model (Services, Stages, Events) | no  | no     | no            | **yes**   |

Pick superchat when you want to:
- Switch models mid-conversation with `@gpt-4o` or `@claude-sonnet`.
- Attach files inline with a leading `#/absolute/path/to/file.el` or `#~/path/to/file.el`.
- Write reusable skills as markdown files with frontmatter (`SKILL.md` format).
- Search your past conversations with `/recall` and have relevant memories automatically attached to every turn.
- Extend it with a `superchat-define-plugin` that registers a prompt collector, a tool or an Event listener — no fork, no monkey-patch, unmount to undo.

Pick gptel or ellama when your needs are simpler: single-model, single-provider, one-off chats without memory or workflows.

## Install

**Requirements:** Emacs 29.1 or higher, and [llm.el](https://github.com/ahyatt/llm) ≥ 0.31.1 from GNU ELPA.

Install `llm.el` and its dependencies from GNU ELPA with Emacs' built-in
`package.el`:

```sh
emacs --batch \
  --eval "(require 'package)" \
  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
  --eval "(package-initialize)" \
  --eval "(package-refresh-contents)" \
  --eval "(package-install 'llm)"
```

In a clean HOME this installed `llm` 0.32.0 and its `plz`,
`plz-event-source`, and `plz-media-type` dependencies.

superchat is not yet on MELPA — install from source:

```elisp
(require 'package)
(package-initialize)
(add-to-list 'load-path "/path/to/superchat")
(require 'llm-openai)
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

;; Ollama's native backend
;; (setq superchat-llm-backend
;;       (make-llm-ollama
;;        :chat-model "llama3.1"))

;; LM Studio, or Ollama with its OpenAI-compatible API enabled
;; (setq superchat-llm-backend
;;       (make-llm-openai-compatible
;;        :key "lm-studio"
;;        :chat-model "qwen/qwen3.8-27b"
;;        :url "http://localhost:1234/v1/"))
```

For Ollama, use the same `make-llm-openai-compatible` form and replace the
URL and model with the values exposed by its OpenAI-compatible endpoint (often
`http://localhost:11434/v1/`). Set `superchat-llm-backend` before sending the
first message. If it is left `nil`, `M-x superchat` can still open the chat
buffer, but the first message is refused and reports the configuration error
when it is sent.

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
User: #~/src/main.py what does this function do?
```

The file content is inlined into the prompt. superchat strips the `#path` from your query so the model sees only "what does this function do?" plus the file content.

`@model`, `>skill`, and `#file` are command-head syntax: they are parsed only at the start of the input, and may be composed there, for example `@gpt-4o >code-review #~/src/main.py review this`. Once ordinary message text begins, later `@...` or `#...` text — including a trailing `#file` — remains literal body text. An unquoted file path must be absolute (`#/...`) or home-relative (`#~/...`).

**4. Remember and recall**

```
User: /remember LLM temperature controls randomness; 0 is deterministic, 1 is creative.
Echo area: Anchor added: LLM temperature controls randomness; 0 is deterministic, 1 is creative. (tape ID: <id>)

User: /recall temperature
Echo area: Retrieved 1 entries — will be attached to your next message.

User: how should I set temperature for code generation?
```

`/remember` and `/recall` report their confirmation in Emacs' echo area; they
do not add a `System` entry to the chat buffer. Memories are retrieved with
SQLite FTS5 and automatically attached to turns when your query contains
relevant terms.

**5. Invoke a skill**

```
User: >code-review #src/utils.el
```

Pre-written skills live in `skills/*.md`. Built-in skills include `code-review`, `planning`, and `refactor`. See [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md) to write your own.

**6. Work inside your own buffers** (no chat window needed)

| Entry point | What it does |
|---|---|
| `M-x superchat-inline-ask` | Ask about the code at point; the answer appears in an overlay under the line, the file is never edited. `C-c SPC` in the prompt cycles what is sent as context (function, sexp, line, window, buffer, none). On the overlay: `r` reply, `v` open the conversation, `w` copy, `q` clear. |
| `M-x superchat-rewrite-region` | Rewrite the region. The result streams into a preview over the region; your file stays byte-for-byte unchanged until `C-c C-a` accepts (one undo step) or `C-c C-k` rejects. `C-c C-r` refines, `C-c C-d` shows a diff, `C-c C-m` merges as conflict markers, `C-c C-n`/`C-c C-p` move between previews; a rectangle region gets one preview per segment. |
| `#+begin_src superchat` | Org-babel: the block body is the question, the answer lands in `#+RESULTS:`. |
| Magit commit buffer | With Magit loaded, superchat proposes a conventional commit message from the staged diff. |

Typing `/` in the prompt opens live command completion. It is built from the
same built-in and registered command sources used for dispatch; the current
list includes `/workflow`, `/review`, `/code`, `/research`, `/cowork`,
`/single` and `/authority`.

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

A `type: agent` skill activates a multi-turn Agent Profile with tool access.
Set a stable `profile_id` independently from the mutable `name`, and declare
`capability` explicitly for its work purpose (for example,
`profile_id: profile.coder` with `capability: coding`).
`type: plan` activates read-only planning mode. Multi-step recipes are
standalone `.workflow` files, not a skill type (see Workflows below).

Field rules: `name` and `description` are required. `version` defaults to `"1.0"`. `type` defaults to `"prompt"` (valid values: `"prompt"`, `"agent"`, `"plan"`). Agent Profiles should declare an immutable `profile_id` and an explicit default `capability`; changing `profile_id` creates a new historical identity, while changing `name` does not. A Request may omit or repeat that Capability, but Runtime rejects a conflicting selection before creating the Run. Legacy Profiles warn, fall back to `name` as identity, and use the Runtime-selected Capability (or `chat` when none was selected). `triggers` defaults to `[]`.

See `examples/standard-skills/` for more complete examples, and [docs/SKILLS_QUICKSTART.md](docs/SKILLS_QUICKSTART.md) for the full quickstart.

### Workflows (.workflow files)

Workflows are multi-step linear recipes in standalone `.workflow` files under `<superchat-data-directory>/workflow/`. Each non-empty, non-comment line is one step; steps run sequentially through an **async callback chain**, so Emacs stays responsive for the whole run. Invoke with `>>name [args]`; the first `>>` invocation loads the workflow entry point and executes it immediately (`>` is reserved for skills, `>>` for workflows). The `/workflow <name> [args]` command is also available. Workflow names must be direct-directory basenames; discovery compares the actual `.workflow` basenames in NFC form, so NFC and NFD spellings resolve to the same file. A leading `@model` is consumed by model-switch parsing before `>>` routing, so an `@`-prefixed token is reserved syntax at this surface. An empty `>>` name reports usage; this does not mean every non-empty token is an available workflow.

Use `M-x superchat-workflow-edit` to open an existing recipe or prepare a new one, then save it normally with `C-x C-s`; opening the editor never saves or runs the workflow.

Keep the recipe name short and put its instructions in the file body (one step per line; `#` comments are not steps). Names containing spaces must be double-quoted when invoked: `/workflow "weather report" London` or `>>"weather report" London`. Completion adds the quotes automatically; unquoted names still end at whitespace, so existing name/argument syntax is unchanged.

For discoverable input, use `/workflow <name> [args]`: complete `/workflow`, type a space, then use completion again to choose a saved recipe. Corfu/company can display the candidates; `M-TAB` (`completion-at-point`) also works. Completion fills only the name and never runs it. Type your task after the name, then press `C-c C-c` to execute. The `>>name [args]` shortcut remains supported.

```
# research.workflow — lines starting with # are comments, blank lines are skipped.
Search for the latest information about "$input" on the web. Provide a comprehensive overview.

# Step 2 sees step 1's output through $result.
Based on this information: $result, summarise the three most important points concisely.

# Any earlier step is addressable as $stepN.
Step 1 found: $step1. Step 2 summarised: $step2. Write one concluding sentence about $input.
```

Variables: `$input` (the trigger argument), `$result` (previous step's output), `$stepN` (step N's output), `$lang`, `$date`.

Per-step annotations: `@model-name` at the start of a line overrides the model for that step; `/command args` at the start of a line expands a user-defined command's prompt template with its args bound to `$input`; a leading `#~/path/to/file` or `#/absolute/path/to/file` attaches a context file. The remaining text is sent as the step prompt, and later `#...` body text (such as "issue #42") is left alone.

There are no branches or conditionals — workflows are linear scripts. Before advancing, each completed step must have a durable successful terminal proved from Tape by Closure; open, unknown, invalid, failed, or cancelled history stops the chain. See `examples/research.workflow` for a runnable sample. Legacy `type: workflow` SKILL.md files can be produced from `.workflow` files with `M-x superchat-workflow-import-legacy-dir`.

The concrete long-running Research slice uses `M-x superchat-research-workflow`: Collector, Analyst, and Reviewer run as sequential child Actors and publish immutable Artifacts. After restarting Emacs, use `M-x superchat-research-workflow-status` to inspect Run trust, Artifact availability, and the next resumable stage; `M-x superchat-research-workflow-resume` starts only that Closure-approved stage.

### External Run inspection (`emacsclient`)

When an Emacs server is already running, the optional
`superchat-emacsclient` adapter exposes JSON commands for listing active Runs,
reading one Run or Session history, and cancelling a Run:

```sh
emacsclient --eval '(superchat-emacsclient-list-runs)'
emacsclient --eval '(superchat-emacsclient-run-status "RUN-ID")'
emacsclient --eval '(superchat-emacsclient-session-history "SESSION-ID" 50)'
emacsclient --eval '(superchat-emacsclient-run-cancel "RUN-ID")'
```

The adapter never starts the server.  `emacsclient --eval` already evaluates
arbitrary Emacs Lisp: any local process that can connect to the server can do
the same, so these commands are a convenience surface, not a sandbox,
authentication layer, or security boundary.

### Shared workspace

`M-x superchat-workspace-set-region` makes the active region an Artifact in
the current Session Workspace. A Workspace can hold multiple region
Artifacts; coding agents and delegated executors can inspect them with
`workspace_info`, then target `workspace_read` or `workspace_write` by
Artifact ID. `workspace_publish` stores a new immutable generated Artifact
under the Superchat data directory so its content survives Emacs restarts.
Cross-Run reads and tool-driven writes include the reported
revision: reads consume another Actor's Artifact only after its producer Run
has a durable Closure proof, while stale writes are rejected. Artifact
metadata and lifecycle events are persisted; region content remains in its
original buffer, while published generated content is durable.

### Memory

Memory is a SQLite database with FTS5 full-text search. Two commands control it:

- `/remember [text]` — capture text (or the last exchange, with no argument).
- `/recall <keywords>` — search memory and attach results to your next turn.

Memory auto-retrieves on every turn when your query is long enough (configurable via `superchat-memory-auto-recall-min-length`, default 8 characters). The retrieval results join the prompt through the `memory` collector in `superchat-prompt-hooks.el`.

The database is at `~/.emacs.d/superchat/superchat.db` (or your `superchat-data-directory`). You can inspect it with any SQLite client:

```bash
sqlite3 ~/.emacs.d/superchat/superchat.db "SELECT title FROM memory LIMIT 5"
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

When a server connects, Superchat registers each of its tools with `superchat-register-tool` under the name `mcp:<server>:<tool>`. From then on they arrive inside `superchat-get-llm-tools` like any other registered tool — same tool surface, same request-local tool profile, and the same Authority approval and Tape audit wrapper.

Registration happens when the server is started through Superchat (`/mcp-start`, `/mcp-start-server`). A server you started elsewhere — `mcp-hub-start-all-server` in your init file, or the `mcp-hub` UI — is picked up the next time you run `/mcp`.

### Telega Cowork Session

With [telega.el](https://github.com/zevlg/telega.el) loaded, open any Telega
chat and run `M-x superchat-cowork-telega`; no message selection is required.
The bundled plugin opens or reuses a Session bound to that chat so you can ask
natural-language follow-ups, understand what happened, and prepare a response.
Its model-facing Session can only read the current or explicitly granted
chats.  It cannot send messages or modify rules; `C-c C-d` puts the latest
answer into the originating Telega input area for review.

### The turn pipeline (advanced)

Every request — chat, inline ask, rewrite, sub-agent, workflow step,
compaction, a plugin surface — is a *Request envelope* submitted through
`superchat-pipeline-submit` and driven through the same fixed stages:

```
parse → resolve-session → admit → load-state → select-tools → collect-context
  → assemble-prompt → (model-transport Service) → save-state
```

Plugins do not reorder the main line; they register a contribution inside a
Stage (`superchat-stage-add`) from their `apply`, for example a `collect-context` collector that
returns a labelled prompt fragment with a budget class, or a `select-tools`
decision. Visible output goes only to the envelope's *Sink* (a small set of
callbacks), so an inline overlay, the chat View and a background job differ
only in their Sink. Prompt text is contributed by a `collect-context` fragment registered from a
plugin's `apply` (the old `superchat-build-prompt-functions` /
`superchat-system-prompt-functions` hooks no longer exist):

```elisp
(superchat-define-plugin my-git-blame
  :apply
  (lambda (_scope _config)
    (superchat-stage-add
     'collect-context 'my:git-blame
     (lambda (env _state _tools)
       (when-let* ((file (and (eq (plist-get env :surface) 'chat)
                              (plist-get (plist-get env :context) :chat-file-ref))))
         (list (superchat-prompt-fragment
                :owner 'my-git-blame :id "my:git-blame" :slot 'context
                :text (shell-command-to-string (format "git blame -L 1,20 -- %s" file))
                :priority 0 :budget-class 'soft))))
     :owner 'my-git-blame)))
```

A message sent while the previous answer is still streaming is queued and sent
automatically when the Run ends (the former manual `C-c C-q` draft is gone).
When a conversation outgrows the model's context window, superchat compacts
it into a summary anchor and retries once — on by default
(`superchat-handoff-auto-enabled`).

See [docs/architecture.md](docs/architecture.md) for the stage table and
[docs/design/turn-pipeline-contract.md](docs/design/turn-pipeline-contract.md)
for the contract.

## Customization

The most-used defcustoms (see `M-x customize-group RET superchat RET` for all):

- `superchat-llm-backend` — the `llm.el` provider struct. **Must be set** before sending the first message; a nil value is refused at send time.
- `superchat-llm-model` — optional `:chat-model` override; used by `@model` switch.
- `superchat-llm-streaming` — `t` for streaming (default), `nil` for blocking.
- `superchat-llm-tool-names` — built-in tools offered to the model. Default: `list_buffers`, `read_buffer`, `apropos`, `edit_buffer`, `eval_elisp`; tools a plugin registers are offered too; `'all` for the full library. Every call still passes Authority.
- `superchat-authority-unprompted-read-tools` — the only tools that read without asking you (default `read_buffer`, `list_buffers`, `apropos`, `describe-function`), and only for plain reads; anything matching `superchat-authority-sensitive-buffers` (e.g. secrets files) asks anyway. Everything else — edits, evaluation, processes — asks first.
- `superchat-llm-set-model-capabilities` — declare a local model's streaming abilities, e.g. for LM Studio: `(superchat-llm-set-model-capabilities "qwen2.5-coder" '(:streaming t :streaming-tool-use nil))`; unknown models fall back to safe defaults.
- `superchat-rewrite-confirmation` — `overlay` (preview, accept with `C-c C-a`) or `auto`.
- `superchat-memory-auto-recall-cross-session` — what other sessions' memories the model may see automatically: `remembered` (only `/remember` anchors, default), `all`, or `none`; your own `/recall` always searches everything.
- `superchat-handoff-auto-enabled` — automatic compaction on context overflow (default `t`).
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

`M-x superchat` can open with `superchat-llm-backend` set to `nil`, but the
first message is refused when it is sent. The chat reports:

```text
[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]
```

Set `superchat-llm-backend` to a provider struct before sending the first
message. Run `/backend` to verify.

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

## Writing plugins

Superchat v2 is a small Kernel (plugins and Scopes, Services, Stages, Events, Envelope and Sink) with everything else — the driver, the model loop, Authority, the Tape, the chat window — shipped as default plugins mounted from `superchat-plugins`. A third-party plugin is an ordinary Emacs package built around one `superchat-define-plugin` (`:inject` the Services it needs, `:config`, `:apply` registering through its Scope); unmounting it withdraws every tool, command, policy, Stage implementation and Event listener it added. See the [plugin guide](docs/plugin-guide.md) and `docs/design/plugin-architecture.md`; `plugins/superchat-cowork-telega` is the reference plugin.

## Project status & roadmap

v1.4.0 is the current package version. Stable features include chat,
streaming, model switching, skills, memory, MCP, tool calling with Authority
approvals, inline ask and overlay rewrite in any buffer, agent mode with
sub-agent delegation, the plugin Kernel with its fixed Stage main line, automatic compaction, and
the async `.workflow` engine (`>>name [args]` or `/workflow <name>`). See [archive/ROADMAP.md](archive/ROADMAP.md)
for planned work.

## License

GPL-3.0-or-later.

## Background

superchat originated as the chat-view module of [org-supertag](https://github.com/yibie/org-supertag). It has been fully independent since v0.1 and no longer carries any org-supertag dependency.
