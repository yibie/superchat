# superchat

superchat is a chat interface for large language models inside Emacs, with a shared Runtime and Scope-accounted plugins for model transport, context collection, tools and lifecycle listeners. Plugins mount from one composition list; some product features share a plugin rather than having an independent lifecycle. If you want a Claude-Code-style chat in your editor but also want to extend it without forking the project, this is for you.

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

Typing `/` in the prompt opens live command completion from the same built-in and registered sources used for dispatch, including `/workflow`, `/code`, `/research`, `/cowork`, `/single` and `/permissions`. The built-in recovery review command is absent. `M-x superchat-experience-monitor` remains a read-only report; Experience's recovery path remains available. See the [M3a removal decision](docs/decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md) for the exact scope and retained historical records.

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

A step advances only from its unique recorded `run.completed` with status `completed`. A Run with no terminal waits; other terminal outcomes, conflicting terminals and unreadable records stop advancement. This reads the recorded outcome rather than judging the whole history; see the [request lifecycle](docs/protocols/request-lifecycle.md). See `examples/research.workflow` for a runnable sample. Legacy `type: workflow` SKILL.md files can be produced from `.workflow` files with `M-x superchat-workflow-import-legacy-dir`.

The concrete long-running Research slice uses `M-x superchat-research-workflow`: Collector, Analyst, and Reviewer run as sequential child Actors and publish immutable Artifacts. After restarting Emacs, use `M-x superchat-research-workflow-status` to inspect recorded Run outcomes, Artifact availability and the next resumable stage. `M-x superchat-research-workflow-resume` starts a stage only when the recorded predecessor outcomes and Artifact checks permit it; unavailable Artifacts may still require manual review.

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
Cross-Run reads and tool-driven writes include the reported revision. Reads consume another Actor's Artifact only when its producer has a unique recorded successful Run terminal and the Artifact passes its provenance, producer, revision and Tape-order checks; stale writes are rejected. Artifact
metadata and lifecycle events are persisted; region content remains in its
original buffer, while published generated content is durable.

### Permissions

Superchat works on Emacs **buffers**, not on files. The model can read a file (`read_file`), open a file as a buffer (`open_file`) and edit buffers. **Its edits stay in the buffer: saving a buffer to disk is yours**, the way it always is in Emacs — the `*` in the mode line is the pending change. When a Run ends having left edited buffers unsaved you get a message naming them, and `/changes` lists them at any time (`M-x superchat-changes-diff` / `-save` / `-revert`; edits made by code run through `eval_elisp` are not tracked).

Superchat's default modification path performs no persistence, and it does not fight your environment: if you run `super-save`, `auto-save-visited-mode`, format-on-save or similar, an edited buffer reaches the disk because *you* set Emacs up that way. So the promise is not "the disk never changes" — it is "Superchat does not write it".

Each Session runs at one permission level; `/permissions` shows it and `/permissions LEVEL` changes it for that Session only. A level decides how the tools that exist may run; it never adds tools.

| Level | What the model may do |
|---|---|
| `edit-buffers` (default) | Read anything; edit any ordinary buffer without asking. Anything that amounts to full control of Emacs — `eval_elisp`, shell commands, `define_tool` and tools the agent wrote — asks you **every call** (`y` allow this call, `n` refuse, `?` see the tool, the reason and the full arguments first; your answer covers exactly the arguments you saw). MCP and other third-party tools ask too, unless their server is in `superchat-mcp-trusted-servers`. |
| `danger-full-access` | Full-control and third-party tools run without asking. Nobody is asked anything, so the model also gets no `ask_user`. |
| `read-only` | Observe only: buffers are not edited; full-control and third-party tools are refused. |

`eval_elisp` and the shell are the escape hatch: once you approve a call, that code can do anything, including saving or writing files. That is why they ask every time. Superchat's own buffers (the chat View and its internal buffers) are never edited by tools, under any level. `save_buffer` is not among the default tools; a Preset that really needs the model to save can list it, and then every call asks you — and is refused where nobody can be asked (`read-only`, or the approval policy `never`, which is what `danger-full-access` uses). The level formerly called `workspace-write` is `edit-buffers`; the old name is still accepted.

**Reading is not restricted by the level.** `read_buffer`, `read_file` (the bytes on disk, at most 2000 lines / 50 KiB per call, no buffer created) and `open_file` (a real Emacs visit: major mode, your hooks and file-local variables run, and any question Emacs itself asks while doing so is Emacs's, not a Superchat approval) ask nobody under any level. Whether a file can be read is decided by the operating-system permissions of your Emacs process; there is no list of sensitive paths. What is read enters the model's context, so with a remote model it is sent to that provider. A plugin can tighten any of this with a `tools/pre-execute` listener.

`/reset` keeps the Session's level and approval policy.

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

When a server connects, Superchat registers each of its tools with `superchat-register-tool` under the name `mcp:<server>:<tool>`. From then on they arrive inside `superchat-get-llm-tools` like any other registered tool — same tool surface, same request-local tool profile, the same Tape audit wrapper, and the [permission level](#permissions) decides whether a call asks you: by default every MCP call does, until you list its server in `superchat-mcp-trusted-servers`.

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
`superchat-pipeline-submit` and driven through the same main line:

```
submit → parse (in the surface) → admission → select tools → collect context
  → assemble prompt → (model-transport Service)
```

The fixed Stage registry is retired. Session identity, state assembly, tool
selection, prompt assembly and admission are core calls; the pieces a plugin
still extends are Events. A plugin registers a listener on the
`collect-context` Event from its `apply` — a collector that returns labelled
prompt fragments with a budget class — or on a lifecycle Event. Visible
output goes only to the envelope's *Sink* (a small set of callbacks), so an
inline overlay, the chat View and a background job differ only in their Sink.
Prompt text is contributed by a `collect-context` listener registered from a
plugin's `apply` (the old `superchat-build-prompt-functions` /
`superchat-system-prompt-functions` hooks no longer exist):

```elisp
(superchat-define-plugin my-git-blame
  :apply
  (lambda (_scope _config)
    (superchat-context-register
     'my:git-blame
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

See the [architecture map](docs/architecture.md) for the current request path and the [request lifecycle contract](docs/protocols/request-lifecycle.md) for its guarantees.

## Customization

The most-used defcustoms (see `M-x customize-group RET superchat RET` for all):

- `superchat-llm-backend` — the `llm.el` provider struct. **Must be set** before sending the first message; a nil value is refused at send time.
- `superchat-llm-model` — optional `:chat-model` override; used by `@model` switch.
- `superchat-llm-streaming` — `t` for streaming (default), `nil` for blocking.
- `superchat-llm-tool-names` — built-in tools offered to the model. Default: `list_buffers`, `read_buffer`, `read_file`, `open_file`, `apropos`, `describe-function`, `edit_buffer`, `eval_elisp`; tools a plugin registers are offered too; `'all` for the full library. What a call may do is decided by the Session's [permission level](#permissions).
- `superchat-sandbox-default-mode` / `superchat-approval-default-policy` — the level new Sessions start at (default `edit-buffers` and `ask`). `superchat-mcp-trusted-servers` — MCP servers whose tools run without asking.
- `superchat-llm-set-model-capabilities` — declare a local model's streaming abilities, e.g. for LM Studio: `(superchat-llm-set-model-capabilities "qwen2.5-coder" '(:streaming t :streaming-tool-use nil))`; unknown models fall back to safe defaults.
- `superchat-rewrite-confirmation` — `overlay` (preview, accept with `C-c C-a`) or `auto`.
- `superchat-memory-auto-recall-cross-session` — what other sessions' memories the model may see automatically: `remembered` (only `/remember` anchors, default), `all`, or `none`; your own `/recall` always searches everything.
- `superchat-handoff-auto-enabled` — automatic compaction on context overflow (default `t`).
- `superchat-data-directory` — where memory, config, and commands live (default `~/.emacs.d/superchat/`).
- `superchat-lang` — language for `$lang` variable in custom commands (default `"English"`).
- `superchat-response-timeout` — silence window for each provider attempt (default 120 seconds, adjusted in tool mode).
- `superchat-response-hard-timeout` — aggregate transport limit across attempts (default 600 seconds).
- `superchat-llm-max-tool-rounds` / `superchat-llm-round-limit-action` — tool round budget and its boundary action (default 12 / `ask`).
- `superchat-llm-reasoning` — requested effort, or provider defaults (`nil`); see [reasoning settings](#reasoning-settings).
- `superchat-context-message-count` — how many recent messages to include in the prompt.
- `superchat-conversation-history-limit` — how many messages to keep in the buffer.
- `superchat-display-single-window` — dedicate the frame to superchat (default `t`).

The built-in edit and eval tools report operations separately from task completion. `edit_buffer` changes one unique match in a live buffer; it does not save or evaluate the file. `eval_elisp` accepts one complete expression (`progn` or `let` for multiple operations) and reports its return value or a bounded tool error recognized by the tool loop. Configuration work still needs verification of the effective setting and, when requested, persistence after loading. See [edit and evaluation results](docs/components/superchat-tools.md#built-in-edit-and-evaluation-results).

New user messages and model responses are preserved in full for later conversation history. The context budget may omit a whole older message; it does not replace the saved original with a display summary. Text already clipped by an older version cannot be recovered from its fingerprint. See [conversation history](docs/decisions/implemented/2026-09-17-preserve-conversation-material.md).

`M-x superchat-act` sends an instruction from the current buffer; a prefix argument opens the request inspector. The inspector sends the final edited body under the current context budget. History, attachments and source selections you remove there stay removed. Its system display includes the original system configuration and the collected instructions once; see [request preparation](docs/protocols/request-lifecycle.md#submission-and-preparation).

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

### Reasoning settings

`superchat-llm-reasoning` defaults to `nil`: no thinking field is sent and the provider/server chooses its mode. To disable thinking on a native provider such as Ollama, set it to `none` explicitly. An explicit Preset effort overrides the global value; use `reasoning: inherit` in the Preset to follow the global setting. Native providers use llm.el's model support.

For the exact `llm-openai-compatible` provider, an explicit effort needs a mapping for the complete chat endpoint and model, or a deliberately configured raw reasoning parameter. For an endpoint you have verified accepts `reasoning_effort="none"`, this declaration lets the global/Preset setting select it:

```elisp
(setq superchat-llm-reasoning-parameter-overrides
      '(((llm-openai-compatible
          "http://localhost:1234/v1/chat/completions"
          "qwen/qwen3.8-27b")
         reasoning_effort
         (none . "none"))))
(setq superchat-llm-reasoning 'none)
```

Declare other efforts only with values supported by that endpoint. Model names and port numbers do not establish its protocol. A missing mapping fails before sending, with instructions for selecting server defaults or declaring the mapping. Existing explicit raw reasoning parameters in the prompt/provider retain precedence, so remove a forced raw value when Presets should choose the effort. Saved custom values are preserved; an explicit `none` is not converted to `nil` during upgrade. Request serialization establishes what was requested, not whether an arbitrary server honored it.

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

Each provider attempt gets its own silence window: 120 seconds by default, multiplied by 1.5 in tool mode and adjusted for context. New text, reasoning and tool progress refresh that window; an earlier preamble does not disable later protection. `superchat-response-hard-timeout` separately limits the whole transport conversation to 600 seconds, and Runtime may impose its own Run deadline. Explicit human input suspends its transport clock balances. If an individual attempt is genuinely slow, configure the silence window, for example `(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`. These timers do not preempt synchronous Lisp; see [model progress and tool budgets](docs/protocols/request-lifecycle.md#model-progress-and-tool-budgets).

**Tool round limit reached**

A round is one model response invoking one or more tools. At the default 12 rounds, interactive Emacs asks whether to grant another 12. Declining, or using batch mode, requests a final answer without tools; `stop` ends immediately. The chat keeps a separate budget notice, including after reopening the Session. Read the answer's completed, incomplete and unverified work before treating a configuration task as done. Adjust `superchat-llm-max-tool-rounds` and `superchat-llm-round-limit-action` for your workflow; the aggregate hard limit still applies.

**Memory not persisting**

Check `superchat-data-directory` and that the SQLite database is writable. Run `/recall test` to verify queries work. Memory auto-retrieval only activates for queries longer than 8 characters (configurable via `superchat-memory-auto-recall-min-length`).

**"User:" prompt doesn't appear after response**

Check whether the Run is still waiting for the model, a tool or human input. A synchronous blocking tool can prevent Emacs timers from running, so a timeout is not a guarantee that arbitrary Lisp becomes interruptible. Inspect the tool and its server before increasing the budget.

**"Symbol's value as variable is void: superchat--dispatch-menu-transient"**

This occurs when `evil-collection` attempts to bind keys in superchat's transient dispatch menu and evaluates the symbol as a variable. Remove `superchat` from `evil-collection-mode-list`:

```elisp
(setq evil-collection-mode-list
      (remove 'superchat evil-collection-mode-list))
```

Or disable evil-collection for superchat entirely in your `evil-collection` use-package block.

## Writing plugins

Superchat uses `superchat-plugins` to mount its default composition. A third-party plugin is an ordinary Emacs package built around `superchat-define-plugin`: it declares Service dependencies and configuration, then registers contributions through its Scope. Unmounting withdraws those registrations; cancellation of active work follows the Runtime lifecycle contract, not arbitrary plugin cleanup. Start with the [runnable plugin guide](docs/plugin-guide.md) and [composition contract](docs/design/plugin-architecture.md); `plugins/superchat-cowork-telega` is a product example.

## Project status & roadmap

The [remaining architecture plan](docs/roadmap.md) distinguishes implemented behavior, accepted but unfinished decisions, and proposals. Fixed Stages are still present in the current implementation; their accepted replacement is not shipped functionality.

v1.4.0 is the current package version. Stable features include chat,
streaming, model switching, skills, memory, MCP, tool calling under per-Session
permission levels with approvals, inline ask and overlay rewrite in any buffer, agent mode with
sub-agent delegation, the plugin Kernel with its fixed Stage main line, automatic compaction, and
the async `.workflow` engine (`>>name [args]` or `/workflow <name>`).

## License

GPL-3.0-or-later.

## Background

superchat originated as the chat-view module of [org-supertag](https://github.com/yibie/org-supertag). It has been fully independent since v0.1 and no longer carries any org-supertag dependency.
