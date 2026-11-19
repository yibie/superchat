# Decision: A timer is a write

Status: implemented

## Problem

In the person's own use, a chat asked to read six files "one per response"
set a `follow_up` of 5 seconds to read the next file later. Each timer woke
a run nobody watched, which read one file, `send`-ed it and set the next
timer: two chains, 11 runs and 11 notifications, none asked about. The
timer tools were class `read`, so even at `ask` nobody was asked, and a
run could make more runs without end.

## Decision

- `follow_up` and `schedule` are class `write`: they make work nobody
  watches. At `ask`, the person in a chat is asked before a timer is set,
  and a run nobody watches that would set one stops at a question; at
  `auto` nothing changes. `cancel_timer` stays `read`.
- `follow_up` takes 60 seconds or more: the timers are looked at once a
  minute, so a sooner time was a promise the inbox could not keep.
- `follow_up`'s description says it is for what waits on time or someone
  else, or a reminder asked for, never to split work the run can do now;
  `send`'s says it is for a run the system prompt says nobody watches.

## Alternatives considered

- Descriptions alone. Not taken: the model had misread an instruction
  against the words it had; the level is what the person controls.
- Leave timers `read` and refuse a timer set from a run started by a
  timer. Not taken: it forbids a legitimate check-again later, and needs
  the tool to know what started its run.

## Consequences

- At `ask`, "remind me in an hour" asks once; `a` stops the asking for the
  timeline.
- A chain of timers needs the person's yes at `ask`, or `auto`.
