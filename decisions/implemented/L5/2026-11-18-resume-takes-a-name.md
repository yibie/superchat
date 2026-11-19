# Decision: /resume takes a name

Status: implemented

## Problem

`/resume` offered the `timelines` view of the buffer's own scope and nothing
else. A timeline of another scope could not be opened from a chat buffer at
all, and the one the person wanted could not be named even when they knew
its name — they had to find a buffer whose scope matched, or guess from the
first line of each candidate. `NOTES.md` recorded it: no command opens a
timeline by its name.

## Decision

`/resume` takes an optional NAME. With it, that timeline opens, of any scope;
a NAME with no entry is a `user-error` naming it. Without it, the command
offers the same choice as before.

`spec/L5-interface.md` says so in the table of commands, and its DOC begins
with `[name]`.

A second command would have been a second way to open a conversation, which
`SPEC.md` forbids: one way to do each thing. The name belongs to the command
that opens timelines, as an argument rather than as a rival.

## Alternatives considered

- A new command, `/open NAME`, beside `/resume`. Two commands that both open
  a conversation and differ in whether they ask which one; the choice is the
  argument, not another verb.
- Widen the choice to every timeline of every scope. Then the list is every
  conversation the person ever had, in one `completing-read`, which is worse
  than typing six characters of a name. The name is the shorter path and the
  precise one.
- Refuse a name with no entry by opening an empty timeline of that name.
  Silently making a timeline is worse than saying the name is wrong: a typo
  would look like an empty conversation rather than a mistake.

## Consequences

- A conversation opens from any chat buffer when its name is known.
- The scope rule of `/resume` is unchanged for the command without an
  argument, and its tests of scope still hold.
- `test/test-chat.el` holds the name: a timeline of another scope opens by
  its name, and a name with no entry signals a `user-error`.
