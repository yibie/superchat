# Decision: The timers buffer takes keys

Status: implemented

## Problem

`/timers` showed the timers as read-only text. To cancel one the person
went back to a conversation and typed `/timers cancel ID`, though the
timer was on the screen in front of them.

## Decision

- `superchat-timers`, a command as well as what `/timers` runs, shows the
  buffer `*superchat: timers*` in `superchat-timers-mode`.
- On a timer's line `c` cancels it and lists anew, `RET` opens its
  conversation, `g` lists anew; the keys follow Emacs's own `list-timers`.
- `/timers cancel ID` still cancels one by its id.

## Alternatives considered

- A new L5 module for the buffer. Not taken: `/timers` opens it, and the
  module would need the chat while the chat needed it.
- `k` to cancel, as `list-buffers` kills. Not taken: the person's meow
  motion state moves with `k`, and `list-timers` uses `c`.

## Consequences

- `superchat-chat.el` grows by about 35 lines, to 875 of 900.
- Under evil the mode starts in a state that may take `c`; the person sets
  its initial state to `emacs`.
