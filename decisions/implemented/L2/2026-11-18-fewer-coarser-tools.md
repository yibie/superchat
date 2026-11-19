# Decision: Fewer, coarser tools

Status: implemented

## Problem

A session of about five hours in the person's own use (timeline
`20261118-165024-f2af`, 2206 entries) stopped twelve times at
`superchat-max-rounds`, and the person typed "继续" each time. 609 of its
739 model responses held one call, and a change to a file took two of them:
`edit` (149 calls), then `save_buffer` (116). Eleven edits found no OLD,
partly because `save_buffer` ran the person's formatter and changed the text
the model remembered. `ls` (3 calls), `find` (2) and `list_buffers` (1) were
barely used: the model listed with `shell` (210 calls).

## Decision

- `edit` and `write` save the file they change; `save_buffer` is gone. The
  buffer stays visited, so the person can undo there. A buffer that held
  unsaved changes of the person's is changed and not saved, with a notice.
  A save that changes the text says so, so that the model reads before the
  next edit.
- `read` takes `path` or `buffer` and lists a directory; `read_buffer`, `ls`
  and `list_buffers` are gone. `eval_elisp` lists buffers when needed.
- Seven built-in file and program tools remain: `read`, `grep`, `find`,
  `edit`, `write`, `shell`, `eval_elisp`.

## Alternatives considered

- Raise `superchat-max-rounds`. Not taken alone: it hides the cost of each
  change and keeps the stale text after a formatter; the person can still
  set it.
- Keep `save_buffer` and make edits save only when asked. Not taken: the
  model then needs two calls per change, the case that used the rounds.
- Save without the person's save hooks. Not taken: a file the formatter
  never saw differs from what the person's own saves make; the notice
  tells the model instead.
- Drop `find` too, since the model listed with `shell`. Not taken: `find` is
  a read tool, asked about at no level, where `shell` is asked about at
  `ask`.

## Consequences

- One call per change; the model learns of a reformatting at once.
- A change is on disk before the person sees it; undo in the buffer and a
  save restore it. Before, the person could review unsaved edits.
- The README's list of tools and the system prompt name the new set.
