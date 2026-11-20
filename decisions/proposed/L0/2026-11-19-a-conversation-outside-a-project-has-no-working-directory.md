# Decision: A conversation outside a project has no working directory

Status: proposed

## Problem

A conversation's scope is its project root, or `"home"` when
`default-directory` is in no project (`superchat-tape-scope-of`). `"home"`
then means two things at once: the memory the conversation shares (notes of
every project and of home), and a working directory, `~`, against which
every relative path of a tool resolves and in which `shell` runs
(`superchat-tools.el`, `superchat-context.el`). The second is rarely meant:

- In the polyglot benchmark a chat started in a plain exercise folder read
  `/Users/…/bowling.py` for `bowling.py`, then searched the person's home
  for it and found the benchmark's Go copy of the exercise.
- Many Emacs buffers belong to no file or directory: `*scratch*`, a REPL, a
  mail or chat buffer, `*Messages*`. Their `default-directory` is often
  `~` by accident, so asking about such a buffer gives the model the whole
  home directory as its workspace.

## Proposal

- Split the two meanings. Scope stays what it is for memory: a project
  root, or `"home"`. The working directory becomes a fact of its own,
  `:directory` on the first entry of a timeline:
  - in a project: the project root, as now;
  - outside a project, from a buffer visiting a file or a `dired`
    directory: that directory;
  - from a buffer with neither: none.
- A conversation with no working directory works in Emacs, not on disk:
  - its system prompt says so, and names the buffer it came from
    (`:origin`), which `read` (`buffer`), `edit` and `eval_elisp` reach;
  - a file tool given a relative path refuses: `This conversation has no
    working directory: give an absolute path`;
  - `shell` runs in a directory made for the timeline under
    `temporary-file-directory`, never in `~`.
- Absolute paths keep working, under the permission rules as now: outside
  any project a write or exec is asked at `ask`.

## Alternatives considered

- Keep `~` as the working directory, and only tell the model in the system
  prompt. Not taken alone: the benchmark run had that line and still
  searched `~`.
- Make every conversation outside a project ephemeral, held in an
  in-memory database and lost when Emacs exits. Not taken: the person
  asked that such a conversation "start in memory", and chose the reading
  "works in Emacs, not on the file system"; kept on the Tape, it stays
  reachable by `recall`, notes and `/resume`.
- Use the origin buffer's `default-directory` whatever it is. Not taken:
  for buffers like `*scratch*` that is `~` again.

## Acceptance criteria

- A chat opened from `*scratch*` has no working directory: `read` of
  `x.py` is refused with the message above, `read` of the buffer works,
  and `shell` `pwd` prints a directory under `temporary-file-directory`.
- A chat opened from a file outside any project resolves relative paths
  against that file's directory.
- A chat in a project behaves as now; every existing test passes.
- Notes of `"home"` still reach every conversation, as now.

## Risks

- The data of the first entry gains a key (L0), and L2, L3 and L5 each
  change: `superchat-tools.el` (path resolution, `shell`),
  `superchat-context.el` (the prompt line), `superchat-chat.el` (which
  directory a new chat records).
- A person who meant `~` must now give absolute paths, or open the chat
  from a buffer in that directory.
- `superchat-tools.el` is at 782 of 800 lines.
