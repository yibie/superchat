# Decision: The views of the Tape take keys

Status: implemented

## Problem

`/notes`, `/recall` and `/topic` showed their entries as read-only text:
to reach the conversation an entry came from, the person searched for it
again. Giving these lists keys, as
[the timers buffer](2026-11-20-the-timers-buffer-takes-keys.md) got,
would have put `superchat-chat.el` at 934 lines, over its cap of 900.

## Decision

- A new L5 module, `superchat-views.el` (cap 200), holds the buffers that
  browse the Tape: the timers buffer and `superchat-entries`, the list the
  notes, a search and a topic share.
- In an entry list `RET` opens the conversation of the entry at point,
  `n`/`p` go to the next or previous entry and `g` asks the view again.
- A view opens a conversation through `superchat-views-open-function`,
  which the chat sets, so the arrow is `chat → views` and not both ways.

## Alternatives considered

- Raise the chat's cap to 1000. Not taken: the person chose to split the
  views out, a thing of their own beside the chat buffer.
- Have the views require the chat. Not taken: the chat's slash commands
  open the views, and the two would require each other.
- Leave the entry lists read-only. Not taken: the person asked that every
  viewer be handled as the timers buffer was.

## Consequences

- `superchat-chat.el` falls to about 830 lines.
- `/help` stays plain text in the chat: it is a list of commands, with
  nothing to act on.
- `superchat-messages.el` keeps its own buffer and keys.
