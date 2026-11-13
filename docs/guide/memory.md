# Memory

Superchat keeps what you ask it to remember, and what was said and done in each conversation, in one SQLite database with FTS5 full-text search.

## Remember and recall

```
/remember LLM temperature controls randomness; 0 is deterministic, 1 is creative.
/recall temperature
how should I set temperature for code generation?
```

`/remember TEXT` stores the text as an anchor; `/remember` with no text stores the last exchange. `/recall KEYWORDS` searches memory and attaches the results to your next message. Both report in the echo area and add no entry to the chat. From a batch run with a scripted model:

```text
Anchor added: LLM temperature controls randomness; 0 is deterministic, 1 is creative. (tape ID: 3)
Retrieved 2 entries — will be attached to your next message.
```

Memory is also retrieved by itself on every message at least `superchat-memory-auto-recall-min-length` columns wide (default 8). The results join the prompt through the `memory` collector in `superchat-prompt-hooks.el`.

`superchat-memory-auto-recall-cross-session` says what the model may see automatically from the other conversations of the same workspace: `all` (the default), `remembered` (only `/remember` anchors) or `none`. Nothing crosses from one workspace to another, and a conversation in Home (or a temporary one) sees only itself. Your own `/recall` always searches everything.

## The database

The database is `superchat.db` in `superchat-data-directory` (default `~/.emacs.d/superchat/superchat.db`). Any SQLite client can read it:

```bash
sqlite3 ~/.emacs.d/superchat/superchat.db "SELECT title FROM memory LIMIT 5"
```

Memory tuning is under `M-x customize-group RET superchat-memory RET`.

## What the database keeps about tool calls

Each tool call the model makes is recorded with the arguments it sent and the result it got back (or the refusal it received), up to 2000 characters each. That includes text of files the model read and values of Lisp it evaluated. Superchat redacts fields with conventionally secret names, but it does not scan text for secrets, so a password inside a file the model read is stored as it is. Later turns of the same conversation send each call to the model again as a tool call followed by its result, the form it used when it made the call, so it remembers what it observed. Set `superchat-tape-store-tool-material` to nil to store none of it; the model then forgets tool results after each turn.

## When memory does not persist

Check `superchat-data-directory` and that the SQLite database is writable. Run `/recall test` to check that queries work, and remember that automatic retrieval needs a message of at least 8 columns.
