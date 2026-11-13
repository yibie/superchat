# Permissions and your files

What the model may do on its own, what it asks about, and what it leaves to you.

## Your edits stay in the buffer

Superchat works on Emacs **buffers**, not on files. The model reads files with `read_file`, opens a file as a buffer (`find-file-noselect` through `eval_elisp`, or `open_file` where a Preset lists it) and edits buffers with `edit_buffer`. **Its edits stay in the buffer: saving a buffer to disk is yours**, the way it always is in Emacs; the `*` in the mode line is the pending change.

When a Run ends having left edited buffers unsaved, you get a message naming them, and a **review screen** appears in another window: one buffer at a time, the colored diff of its file against the buffer. This capture comes from a batch run with a scripted model; "…" marks a shortened path.

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

If you asked and are waiting in the conversation with your hands off the keys, the cursor goes to the screen and comes back when you are done. If you have gone on with something (writing your next message, working in the code window beside the chat, using the minibuffer, or you pressed a key in the last 2 seconds), the screen still appears and the cursor stays where it is; `C-x o` takes you there. If you closed the conversation's window, only the message tells you. Of the other letters, only `diff-mode`'s that look or copy work (`p`, `N`, `P`, `{`, `}`, `<`, `>`, `W`, `r`, `w`, `h`, `?`); the rest do nothing, so another letter typed by accident changes no text.

When you save such a buffer (any way, `C-x C-s` included) or throw its unsaved edits away, the model is told so at the start of its next turn, once. With [diffs.el](https://github.com/LuciusChen/diffs.el) installed the diff gets its line numbers and word-level highlighting. `/changes` lists the buffers and opens the same screen at any time, as does `M-x superchat-changes-diff`. From a file's window, `M-x superchat-changes`, `M-x superchat-changes-diff` and `M-x superchat-changes-save` / `superchat-changes-revert` (these two act on one buffer) use the conversation an in-place command from there would join: the chat beside the file, else the main one. Buffers that visit no file are listed but not on the screen, and edits made by code run through `eval_elisp` are not tracked. Set `superchat-changes-review-after-run` to nil to stop the screen from appearing by itself.

Superchat's default modification path performs no persistence, and it does not fight your environment: if you run `super-save`, `auto-save-visited-mode`, format-on-save or similar, an edited buffer reaches the disk because you set Emacs up that way. The promise is not "the disk never changes"; it is "Superchat does not write it".

## Permission levels

Each conversation runs at one permission level; `/permissions` shows it and `/permissions LEVEL` changes it for that conversation only. A level decides how the tools that exist may run; it never adds tools. New conversations start at `superchat-sandbox-default-mode` (default `edit-buffers`) with `superchat-approval-default-policy` (default `ask`).

| Level | What the model may do |
|---|---|
| `edit-buffers` (default) | Read anything; edit any ordinary buffer without asking. Anything that amounts to full control of Emacs (`eval_elisp`, shell commands, `define_tool` and tools the agent wrote) asks you **every call**: `y` allow this call, `n` refuse, `?` see the tool, the reason and the full arguments first. Your answer covers exactly the arguments you saw; keys typed before the question appeared are discarded and cannot answer it, and a running keyboard macro is refused rather than allowed to answer. MCP and other third-party tools ask too, unless their server is in `superchat-mcp-trusted-servers`. |
| `auto` | Full-control and third-party tools run without asking; saving a buffer (`save_buffer`, where a Preset lists it) still asks, and the model can still put a question to you with `ask_user`. Answering an approval question with **Allow all** (the panel's button, `M-x superchat-approval-allow-all`, or `a` in `M-x superchat-pending`) allows that call and switches that conversation to this level; `/permissions edit-buffers` brings the questions back. It is a level of the conversation, not a grant for one tool. |
| `danger-full-access` | Full-control and third-party tools run without asking. Nobody is asked anything, so the model also gets no `ask_user`. |
| `read-only` | Observe only: buffers are not edited; full-control and third-party tools are refused. |

`eval_elisp` and the shell are the escape hatch: once you approve a call, that code can do anything, including saving or writing files. That is why they ask every time. Superchat's own buffers (the chat View and its internal buffers) are never edited by tools, under any level. `save_buffer` is not among the default tools; a Preset that needs the model to save can list it, and then every call asks you, and is refused where nobody can be asked (`read-only`, or the approval policy `never`, which is what `danger-full-access` uses). The former level name `workspace-write` is accepted as `edit-buffers`.

## Reading is not restricted

No level restricts reading. `read_buffer` returns a live buffer's text, unsaved changes included: at most 2000 lines and 50 KiB per call, a narrowed buffer only in its narrowed part, and the result says which. `read_file` returns the bytes on disk, with the same limits, and creates no buffer. `open_file`, where a Preset lists it, is a real Emacs visit: major mode, your hooks and file-local variables run, and any question Emacs itself asks while doing so is Emacs's, not a Superchat approval. None of them asks anybody. Whether a file can be read is decided by the operating-system permissions of your Emacs process; there is no list of sensitive paths. What is read enters the model's context, so with a remote model it is sent to that provider. A plugin can tighten any of this with a `tools/pre-execute` listener.

## Tools the model writes

A tool the model writes for itself (`define_tool`, off by default) is kept as its definition and belongs to the workspace it was written in; one written in Home belongs to Home; one written in a conversation of no workspace (one you unbound with `/workspace clear`, or one from before Home) stays in that conversation and is never written to disk. Each call of it is admitted under the conversation's permission level, like any full-control tool. Superchat records what happened when it was written: *approved* if you were asked and said yes, *admitted* if the level let it through unasked. After a restart only approved tools are loaded again. The others, and every stored tool that carries no such record, are listed in `M-x superchat-pending` until you look at them in `M-x superchat-tools-review-synthesized`: approve into a workspace, reject, or delete (delete removes the entry from `synthesized-tools.el` and marks its line in the ledger; nothing else is touched).

## Work that runs by itself

A scheduled task runs in a conversation of its own at `superchat-timer-task-level` (default `read-only`): it cannot change a buffer, run code or ask you. `/permissions` in that conversation, or the option, lets it do more. What a command timer, a compile rule and a fix may do is in [work that runs by itself](unattended-work.md).
