# Decision: The timers buffer is a table

Status: implemented

## Problem

[The timers buffer took keys](2026-11-20-the-timers-buffer-takes-keys.md),
but each line printed the timer's raw data: its entry id, a timeline name of
date digits, a Lisp plist and the whole task. The person found it hard to
read: when it fires, how often and for which project were not said.

## Decision

- `*superchat: timers*` is a `tabulated-list-mode` table, soonest first,
  with a header and the columns Next, Repeats, Project and Task.
- Next is when the timer fires next, worked out from its data; Repeats
  says `once`, `daily HH:MM`, `dream HH:MM` or `every Nm/h/d`, with
  `~MM-DD` when it lapses; Project is the scope's directory name.
- `c`, `RET` and `g` act as before; `S` sorts by a column.

## Alternatives considered

- Keep plain lines and only reword them. Not taken: a table aligns the
  columns and gives a header and sorting for free, as `list-timers` does.
- Ask L4 for the next due time. Not taken: `superchat-timer--due` is
  private to L4 and gives the latest past time; the view works out the next
  one from the same data.

## Consequences

- The timeline name and entry id are no longer shown; `RET` still opens
  the conversation.
- An empty list shows only the header.
