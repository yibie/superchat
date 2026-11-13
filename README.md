# Superchat

Superchat is an AI assistant that lives inside Emacs. You talk to it in a chat window beside the file you are editing, or call it right in any buffer: it reads your buffers and files, edits buffers for you, and leaves saving to you.

It solves three problems of an AI chat in a browser tab. You do not copy code back and forth, because it works on the buffers you have open. You keep working while it works, because answers appear beside your text or in an overlay. And it does not forget: every conversation, what it remembered for you, and the jobs you gave it are kept in a database, and it can keep doing a job when nobody is watching, such as rereading a failed compile or a new feed entry, and tell you only when there is something to say.

<!-- screenshot: the chat side window beside a source file, with an answer that edited the file and the review screen below -->

## Compared with gptel, ellama and chatgpt-shell

| Feature | gptel | ellama | chatgpt-shell | Superchat |
|---|:-:|:-:|:-:|:-:|
| Multi-provider via `llm.el` | yes | yes | (own) | yes |
| Streaming | yes | yes | yes | yes |
| MCP tools | (3rd-party) | no | no | yes |
| Per-message `@model` override | no | no | no | **yes** |
| `#file` reference syntax | (region) | (region) | no | **yes** |
| SKILL.md skill format (frontmatter + body) | no | no | no | **yes** |
| Multi-step async workflows | no | no | no | **yes** |
| SQLite-backed memory with FTS5 | no | no | no | **yes** |
| Plugin model (Services, Events) | no | no | no | **yes** |

Pick gptel or ellama when you want single-model, one-off chats without memory, tools or workflows.

## Install

You need Emacs 30.1 or later and [llm.el](https://github.com/ahyatt/llm) 0.31.1 or later from GNU ELPA. Install llm.el (it brings `plz`, `plz-event-source` and `plz-media-type`):

```sh
emacs --batch --eval "(require 'package)" \
  --eval "(setq package-archives '((\"gnu\" . \"https://elpa.gnu.org/packages/\")))" \
  --eval "(package-initialize)" --eval "(package-refresh-contents)" \
  --eval "(package-install 'llm)"
```

Superchat is not on MELPA yet; load it from a checkout and choose a backend:

```elisp
(require 'package)
(package-initialize)
(add-to-list 'load-path "/path/to/superchat")
(require 'llm-openai)
(require 'superchat)

(setq superchat-llm-backend
      (make-llm-openai :key (getenv "OPENAI_API_KEY") :chat-model "gpt-4o-mini"))
```

Any `llm.el` backend works: Anthropic, Ollama, LM Studio and others are shown in [configuration](docs/guide/configuration.md#backends). Optional: [mcp.el](https://github.com/lizqwerscott/mcp.el) for MCP servers.

## Usage by example

The text captures below come from a real run: a batch Emacs with a scripted model, its buffer text written out as is.

### Chat, switch models, attach files

`M-x superchat` opens the chat beside your file. Type after `* User:` and send with `C-c C-c`.

```text
…
* User: explain what a zettelkasten is in one sentence

** Assistant:
A Zettelkasten is a note-taking method: one idea per note, each note linked to related notes.
…
```

<!-- screenshot: the chat side window with this exchange, beside a source file -->

A few prefixes change what a message does:

```
@gpt-4o explain how SQLite FTS5 works         ← this message only, on another model
#~/src/main.py what does this function do?    ← the file goes along with the question
>code-review #~/src/main.py                   ← apply a skill
/permissions                                  ← run a command (type / to complete)
```

`C-c C-w` moves the conversation into its own tab and back. More: [docs/guide/chat.md](docs/guide/chat.md).

### Work inside your own buffers

No chat window needed. `M-x superchat-inline-ask` answers a question about the code at point in an overlay under the line; the file is not touched:

```text
def total(prices, tax):superchat inline · Ready · M-RET menu
It returns the sum of the prices with the tax added; tax is a rate such as 0.08.

    s = 0
…
```

<!-- screenshot: an inline-ask overlay under a line of Python code -->

`M-x superchat-rewrite-region` rewrites the selection into a preview; `C-c C-a` accepts it, `C-c C-k` rejects it, and until then the file is unchanged:

```text
* Monday
REWRITE  Ready  C-c C-a accept  C-c C-k reject  C-c C-d diff  RET menu
The meeting has moved to Thursday at 3 pm. Please bring the draft budget.
```

<!-- screenshot: a rewrite preview over a region in an Org file -->

`M-x superchat-inline-act` does a task where you are and may edit buffers; `M-x superchat-annotate` hangs remarks on the lines they are about; `M-x superchat-note` lets you write your own notes on the text and send them with your next message. More: [docs/guide/in-your-buffers.md](docs/guide/in-your-buffers.md).

### Skills

A skill is a markdown file with a short header. Save it in the skills directory (`skills/` under `superchat-data-directory`) and call it with `>explain-region`; the model also finds skills by their `description` when a task matches.

```markdown
---
name: explain-region
description: Explain a code region to a senior engineer new to this codebase
type: prompt
---
You are a senior engineer explaining code to a peer. Say what it does, why it is built this way, and any edge case.
```

Bundled skills: `code-review`, `planning`, `refactor`, `render-ui`. Several prompts in a row go in a `.workflow` file, run with `>>name`. More: [docs/guide/skills-and-workflows.md](docs/guide/skills-and-workflows.md).

### Memory

```
/remember LLM temperature controls randomness; 0 is deterministic, 1 is creative.
/recall temperature
```

`/remember` keeps a fact; `/recall` finds it and attaches it to your next message. Relevant memories are also found by themselves when you type a longer message. Everything lives in one SQLite file you can open yourself. More: [docs/guide/memory.md](docs/guide/memory.md).

### Permissions: it edits, you save

The model's edit tool works on buffers, not files: its changes sit in the buffer with the usual `*` until you save. When it leaves unsaved buffers, a review screen shows the diff of each (`s` save, `n` next, `q` stop). Anything that amounts to full control of Emacs (running Lisp, shell commands) asks you every call, unless you raise the level with `/permissions`.

```text
Review 1/1 · main.py (/…/main.py)
Unsaved.  The diff below is what saving would write to disk.
s: save   n: leave unsaved, next   q: stop (the rest stay unsaved)   g: refresh   SPC/DEL: scroll   M-n/M-p: next/previous change

--- a/main.py
+++ b/main.py
@@ -1,5 +1,3 @@
 def total(prices, tax):
-    s = 0
-    for p in prices:
-        s = s + p
+    s = sum(prices)
     return s * (1 + tax)
```

<!-- screenshot: the review screen with a colored diff and the s / n / q line -->

More: [docs/guide/permissions.md](docs/guide/permissions.md).

### Work that runs by itself

Say what you want watched, and Superchat sets it up after asking you once:

```
every morning tell me when the star count changes
when a compile fails here, find out why, and tell me only when it is my code
when new posts come into elfeed, tell me about the ones on Emacs
/work move every test helper into tests/helpers.el and make the suite pass
```

What came of it is one line in `M-x superchat-butler`, in the conversation you are using, and on the mode line:

```text
11-11 23:32  Command "tell me when the star count changes" first run: 41
11-11 23:32  Command "tell me when the star count changes" changed: 42 (was 41)
```

<!-- screenshot: M-x superchat-butler with two command lines, and the same line with its [bring in] button in the chat -->

`M-x superchat-pending` lists everything running, waiting or unseen; `RET` goes there, `k` stops or cancels, `t` brings a result into your message:

```text
 Ended, not looked at   Command "tell me when the star count change… changed: 42 (was 41)                                 butler
 Scheduled              tell me when the star count changes          ran 2 times, last: changed (2027-01-16 00:00), 1 ch… 20261111-233228-b10f
 Scheduled              renew the domain                             reminder at 2027-01-16 02:00                         20261111-233228-b10f
 Scheduled              when a compile fails here, tell me only whe… not looked yet; when a compilation fails in /privat… 20261111-233228-b10f
```

`M-x superchat-schedule-task` lists what you set to run. On macOS a compile rule can also try a fix in a sandboxed copy of the project and offer it as a diff. More: [docs/guide/unattended-work.md](docs/guide/unattended-work.md).

<!-- screenshot: M-x superchat-pending with a command row, a reminder and a compile rule -->

### Home and workspaces

A file inside a project opens that project's conversation, with the project's rules (its `AGENTS.md`, else `CLAUDE.md`). Anything else (a mail, a feed entry, a chat, a scratch buffer) opens its own conversation in Home, one per source: the same feed entry or file continues the same conversation after a restart, and Home conversations do not see each other. Home has its own rules file, `home/AGENTS.md` in the data directory. More: [docs/guide/chat.md](docs/guide/chat.md#which-conversation-opens).

### MCP

```elisp
(setq mcp-hub-servers
      '(("filesystem" . (:command "npx" :args ("-y" "@modelcontextprotocol/server-filesystem"
                                                "/Users/you/Documents")))))
```

Start servers with `/mcp-start`; their tools appear as `mcp:<server>:<tool>` and, at the default permission level, ask before each call until you trust the server in `superchat-mcp-trusted-servers`. More: [docs/guide/integrations.md](docs/guide/integrations.md#mcp).

### Telega

In a [telega.el](https://github.com/zevlg/telega.el) chat, `M-x superchat-cowork-telega` opens a conversation about that chat. It can read the chat but never send; `C-c C-d` puts its latest answer into the Telega input area for you to edit and send. More: [docs/guide/integrations.md](docs/guide/integrations.md#telega).

<!-- screenshot: a Cowork Telegram conversation beside a Telega chat, with an example group -->

### Plugins

Superchat's own parts are plugins mounted from `superchat-plugins`. Your own package can add a prompt collector, a tool, a command or an Event listener with `superchat-define-plugin`, without forking, and unmounting it undoes them. More: [docs/plugin-guide.md](docs/plugin-guide.md) and [docs/guide/advanced.md](docs/guide/advanced.md).

## Documentation

- [User guide](docs/guide/README.md): every feature in detail, configuration, troubleshooting.
- [Skills quickstart](docs/SKILLS_QUICKSTART.md) and [plugin guide](docs/plugin-guide.md): tutorials.
- [Architecture](docs/architecture.md), [roadmap](docs/roadmap.md), [decision records](docs/decisions/README.md) and the [documentation map](docs/README.md): how it is built and why.

## License

GPL-3.0-or-later.

## Background

Superchat started as the chat-view module of [org-supertag](https://github.com/yibie/org-supertag) and has been independent since v0.1; it does not depend on org-supertag. The current package version is 1.4.0; the [roadmap](docs/roadmap.md) separates what is implemented from what is planned.
