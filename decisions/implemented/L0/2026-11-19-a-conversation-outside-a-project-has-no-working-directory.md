# Decision: A conversation outside a project has no working directory

Status: implemented

## Problem

A conversation's scope is its project root, or `"home"` when
`default-directory` is in no project (`superchat-tape-scope-of`). `"home"`
meant two things at once: the memory the conversation shares (notes of every
project and of home), and a working directory, `~`, against which every
relative path of a tool resolved and in which `shell` ran. The second was
rarely meant:

- In the polyglot benchmark a chat started in a plain exercise folder read
  `/Users/…/bowling.py` for `bowling.py`, then searched the person's home
  for it and found the benchmark's Go copy of the exercise.
- Many Emacs buffers belong to no file or directory: `*scratch*`, a REPL, a
  mail or chat buffer, `*Messages*`. Their `default-directory` is often `~`
  by accident, so asking about such a buffer gave the model the whole home
  directory as its workspace.

## Decision

- Scope keeps its meaning for memory. The working directory is a fact of
  its own: `superchat-tape-directory` gives the scope of a project, else the
  `:directory` of the timeline's first `user` entry, else nil.
- `superchat-run` takes `:directory` and records it on the first `user`
  entry only. It never derives one from the current buffer: the inbox
  worker and timers call it from whatever buffer happens to be current.
- The chat takes it from the buffer it was opened from: a file's directory,
  a `dired` buffer's, else none.
- With no working directory: a file tool's relative path is an error, `This
  conversation has no working directory: give an absolute path`; `shell`
  runs in `superchat-tools-scratch`, a directory named after the timeline
  under `temporary-file-directory`, made when a process first runs there;
  the system prompt says the conversation works in Emacs, names the buffer
  it came from, says to give absolute paths, and to ask where a file named
  without a path is rather than search for it. Live, without that last
  line, DeepSeek at `auto` ran `find` over `~` and read an unrelated
  `x.py`; with it, it ran `pwd` and asked for the path.
- The permission rules do not change: outside a project, a read outside
  `superchat-directory` is still asked at `ask`.

## Alternatives considered

- Keep `~` as the working directory, and only tell the model in the system
  prompt. Not taken: the benchmark run had that line and still searched
  `~`.
- Hold every conversation outside a project in memory only, lost when
  Emacs exits. Not taken: the person asked that such a conversation "start
  in memory" and chose the reading "works in Emacs, not on the file
  system"; kept on the Tape, it stays reachable by `recall`, notes and
  `/resume`.
- Derive the directory in `superchat-run` from `default-directory`, or
  call `~` "none". Not taken: a background run would get a random buffer's
  directory, and a file in `~` or an eshell has a real one.
- Record it on the timeline's first entry, as the proposal said. Not
  taken: a timeline can begin with a `level` event, an inbox item or a
  timer, so the first `user` entry holds it.

## Consequences

- The person's existing conversations outside a project have no
  `:directory`, so their relative paths now fail with the message above;
  there is no migration (rule 3). A new chat opened from the file fixes it.
- A conversation started by a package with `superchat-run` and no
  `:directory`, outside a project, has none.
- `superchat-tools.el` is at 799 of 800 lines.
