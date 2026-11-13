# User guide

How to use Superchat, one page per feature. The [README](../../README.md) is the short tour; these pages hold the detail. Why a feature works the way it does is in the [decision records](../decisions/README.md), and the contracts code relies on are in [components](../components/README.md) and [protocols](../protocols/request-lifecycle.md).

| Page | What it covers |
|---|---|
| [chat.md](chat.md) | Opening the chat, editor and agent mode, Home and workspaces, `@model`, `#file`, `>skill`, `/command`, history and compaction, `superchat-act` |
| [in-your-buffers.md](in-your-buffers.md) | Inline ask, inline act, region rewrite, annotate, notes, Org Babel, Magit |
| [permissions.md](permissions.md) | Buffers versus files, the review screen, permission levels, reading, tools the model writes |
| [unattended-work.md](unattended-work.md) | Command timers, compile rules and fixes, feed rules, the inbox, the butler, the pending list, the schedule, scheduled tasks, `/work` |
| [skills-and-workflows.md](skills-and-workflows.md) | SKILL.md files, the skill catalog, `.workflow` recipes, the research workflow |
| [memory.md](memory.md) | `/remember`, `/recall`, automatic recall, the database, what is kept about tool calls |
| [integrations.md](integrations.md) | MCP, Telega, elfeed search |
| [configuration.md](configuration.md) | Backends, a cheatsheet, the main options, what `edit_buffer` and `eval_elisp` report, reasoning settings |
| [troubleshooting.md](troubleshooting.md) | Common errors, timeouts, tool budgets |
| [advanced.md](advanced.md) | `emacsclient`, the shared workspace, the turn pipeline, writing plugins |

Tutorials: [SKILLS_QUICKSTART.md](../SKILLS_QUICKSTART.md) (a first skill), [plugin-guide.md](../plugin-guide.md) (a first plugin).
