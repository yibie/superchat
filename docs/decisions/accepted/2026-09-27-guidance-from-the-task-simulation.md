# Decision: two pieces of guidance from the task simulation — local-set-key edits the mode's map; do not change what is outside the task

Status: accepted — user, 2026-09-27: "按照建议" to adding two sentences to the model's guidance (`.scratch/task-sim/SECOND-BATCH-REPORT.md`, failure classes 2 and 4), then "follow suggestion" to placing them: the key-binding sentence on the `emacs-keybindings` reference page, the other in the operating note tightened to stay within its 1,024 characters, and the canonical smoke baseline A updated after the user sees its diff.

## 一句话

模拟测试里有两类错误反复出现：一是不知道在 org 这类文件里用 `local-set-key` 改的是整个 org 模式的按键表；二是为了绕过障碍，去改任务以外的东西（清掉文件的“已修改”标记、去掉聊天窗口的侧边栏属性、改全局设置）。这次各补一句说明：前一句放进按键参考页（修快捷键时模型会读它），后一句放进每次都带的操作说明。

## Problem

- `local-set-key` changes `(current-local-map)`, which in a buffer whose major mode has a map *is* that map (`org-mode-map` in an Org buffer). In the simulation a hook in `org-mode-hook` calling it overrode `C-c j` for every Org buffer; models said it sets a buffer-local binding and fixed only the file (batch 2 T4-2: every new Org buffer rebound the key) or unset the key in the mode's map (batch 1 T4-3). The `emacs-keybindings` page, read in those runs, does not say it.
- To get past an obstacle, models changed state outside the task: `set-buffer-modified-p nil` on another buffer (batch 2 T4-3), the chat window's `window-side` parameter (T6-1), `kill-buffer-query-functions` set to nil for the whole Emacs (J-3). The operating note says to undo only what the task changed; nothing says not to change what the task is not about.
- The operating note's contract (`docs/decisions/implemented/2026-09-20-emacs-operating-note.md`) caps it at 1,024 characters; it had 985. The canonical smoke baseline A records the note's text.

## Decision

1. The `emacs-keybindings` page, section 1, gains: `local-set-key` and `local-unset-key` change the current buffer's local map, which in a major-mode buffer is that mode's map itself — the change reaches every buffer of the mode, a hook calling it redoes it in each new buffer, and fixing such a hook takes both the file and the running session (`remove-hook`, then the binding put back in the mode's map).
2. The operating note gains "Never change global settings, other buffers or Superchat's windows to get past an obstacle; say what blocks you." Three existing lines are reworded shorter with the same meaning and every phrase its tests pin; the note is 1,024 characters.
3. Smoke baseline A changes only by the note's new text, shown to the user before it is written.

## Alternatives considered

**Both sentences in the operating note, the cap raised to 1,280.** Lost: every request that can change Emacs would carry the key-binding detail, while the page is read exactly when a key binding is the task.

**Only the key-binding sentence.** Lost: the out-of-task changes were three of the batch's five failures' side effects, and nothing in the guidance speaks to them.

## Consequences

- Each request with `eval_elisp` or `edit_buffer` carries the same note size as before (at most 1,024 characters).
- Whether the models change behaviour for these lines is not shown until the task simulation runs again.
