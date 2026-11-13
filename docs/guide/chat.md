# The chat

How to open a conversation, what you can type in it, and which conversation a buffer belongs to.

## Opening and closing

`M-x superchat` opens the chat in a side window on the right of the tab you are in, beside the file you are editing (editor mode); your windows stay open, only narrower. Type after `* User:` and send with `C-c C-c`; `C-c C-k` stops the answer that is running. A message sent while the previous answer is still streaming is queued and sent when that Run ends.

Press `C-c C-w` in the chat to move the conversation into its own tab, which it fills (agent mode), and again to bring it back beside the file you came from; the text you had not sent stays. Within an Emacs session the chat opens in the mode you last switched to; `superchat-display-style` (`editor`, the default, or `agent`) sets the mode it starts in. In agent mode the chat is the only window of its tab while `superchat-display-single-window` is `t` (the default); editor mode never closes a window. `superchat-side-window-width` (default `0.42` of the frame) sets the width of the chat's and the plan panel's side windows.

To close a conversation, run `M-x superchat-close-session-workspace` in its chat window, or close its tab in agent mode; if a Run is still working you are asked first.

## Which conversation opens

Which conversation opens depends on where you are. From a file in a project, it is that project's main conversation. A project's rules are its `AGENTS.md`, else its `CLAUDE.md`, at the project root; up to 6000 characters of it go into each request. From anywhere else, it is the conversation of that buffer in **Home**, the workspace of everything outside a project. Commands run in the directory of the buffer you called from.

Home keeps one conversation per source: the same telega chat, elfeed entry, file or buffer continues the same conversation, even after you closed it or restarted Emacs, and conversations from different sources do not see each other. `superchat-source-key-functions` says how a buffer of a major mode names what it shows: an alist of `(MODE . FUNCTION)`. It comes with functions for telega chats (chat id and topic), elfeed entries, eww pages, dired directories, Info nodes, Gnus groups, notmuch searches and threads, and rcirc channels; none needs its package loaded. A buffer no function names is known by the file it visits, else by its mode and name.

Home has its own rules file, `home/AGENTS.md` in `superchat-data-directory`. "Remember …" said in Home is written there, and applies to Home's conversations only, never to a project's. Home also keeps work records and timers: a work record begun from one chat is listed in another Home conversation, which can take it up.

`superchat-personal-workspace-directory` names a Personal Workspace, the home of the agenda, feeds and chats (the modes in `superchat-personal-workspace-modes`); set it with `M-x superchat-set-personal-workspace`. Unset (the default), a conversation called from those buffers belongs to Home, one per source. `/workspace` binds, shows or clears the conversation's workspace. The rationale is in [a workspace is a record, not a directory](../decisions/accepted/2026-11-10-a-workspace-is-a-record-not-a-directory.md).

## What you can type

```
@gpt-4o explain how SQLite FTS5 works
#~/src/main.py what does this function do?
>code-review #~/src/main.py review this
/remember LLM temperature controls randomness
```

- `@model` switches the model for this message only; the default model is used again on the next one. `/models` lists models, `/refresh-models` reloads them from the backend, and `superchat-manual-models` adds model ids of your own.
- `#file` inlines a file into the prompt and removes the `#path` from your words, so the model sees your question plus the file content. An unquoted path must be absolute (`#/...`) or home-relative (`#~/...`). `#file` works at the start of the message only; a later `#file` stays ordinary text.
- `>skill` applies a skill to the message (see [skills and workflows](skills-and-workflows.md)); `>>name` runs a workflow.
- `/command` runs a command. Typing `/` opens completion over the built-in and registered commands, for example `/workflow`, `/code`, `/research`, `/cowork`, `/single`, `/permissions`, `/changes`, `/work`, `/remember`, `/recall`, `/backend`, `/mcp`. `C-c C-h` lists them.

`@model`, `>skill`, `/command` and `#file` may open the input and be composed there, for example `@gpt-4o >code-review #~/src/main.py review this`. `@model`, `>skill` and `/command` also work in the middle of a message, as in `please >code-review look at this` or `explain this @gpt-4o briefly`, but there only when the name exists: an existing skill, a model on your manual list or already fetched, an existing command. The token must stand alone, with a space or the start of the input before it and a space or the end after it; it is removed from what the model receives. Anything else (`a > b`, `> a quoted line`, `name@example.com`, `/usr/bin`, a name in backticks) stays ordinary text.

A skill of type `agent` written at the start becomes the conversation's mode; written in the middle it shapes that one message only and the conversation stays as it was (a `plan` skill switches the conversation either way). At most one of each kind is read, and what opens the input wins: after a `/command`, a later `>skill` is text. A message Superchat sends by itself (a scheduled task, a work record that wakes) is read at its start only. A line that starts with `> ` is a quote: nothing in it counts, and a message that opens with one is not a skill.

## History and long conversations

New user messages and model responses are kept in full for later conversation history. The context budget may omit a whole older message; it does not replace the saved original with a summary. Text already clipped by an older version cannot be recovered. See [conversation history](../decisions/implemented/2026-09-17-preserve-conversation-material.md).

When a conversation outgrows the model's context window, Superchat compacts it into a summary anchor and retries once; `superchat-handoff-auto-enabled` (default `t`) turns this on. `/compact` compacts by hand and `/compact-log` opens the latest summary.

`/reset` returns to the default chat mode. It keeps the conversation's permission level and approval policy and never clears history; in a Home conversation it also keeps the conversation in Home and its source.

## Sending from another buffer

`M-x superchat-send-region` sends the region to the chat. `M-x superchat-act` sends an instruction from the current buffer, with the active region as context. With a prefix argument it opens the request inspector, which sends the final edited body under the current context budget. Earlier turns are shown read-only above the body and go to the model as their own messages, so editing the body neither removes nor changes them; to send a request without them, start a new conversation (`M-x superchat-new-session`) or branch after an earlier turn (`M-x superchat-branch-session`). Attachments and source selections you remove from the body stay removed. See [request preparation](../protocols/request-lifecycle.md#submission-and-preparation).

`M-x superchat-experience-monitor` is a read-only report. The scope of the removed recovery review command is in the [M3a removal decision](../decisions/implemented/2026-09-15-m3a-remove-closure-and-claims.md).
