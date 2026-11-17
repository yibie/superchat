# Decision: A run interrupted by Emacs exiting is marked on the Tape

Status: implemented

## Problem

A run lives in the memory of Emacs. When Emacs exits during a run (a
crash, `kill-emacs`, the machine stops), the Tape keeps what was appended
and nothing more. When Emacs starts again, the timeline is free, and
nothing on the Tape said that a run was cut off.

The worst place to be cut off is between an `assistant` entry with calls
and the `tool` entries that answer them. A call with no `tool` entry is not
sent (L3, "Messages"), so the next request dropped it without a word. That
call may have run halfway: a `shell` command started, an `edit` made, a file
saved. Neither the model nor the person was told.

## Decision

`superchat-run`, before anything else, reads from the Tape whether the
timeline's last run never ended. It never ended when, after the latest
`user` entry, no `error` or `cancelled` event comes and the last entry that
is not an event or a note is a `tool` entry, an `assistant` entry with
calls, or that `user` entry itself, unless an `error` event comes right
before it. If so, the run appends, for each call of that run with no
result, a `tool` entry with `:error t` and the text
`Interrupted: Emacs stopped while this call ran; it may have had effects.`,
then an `event` of type `interrupted`, and goes on as before. Nothing is run
or requested again by itself; the person's next message continues from the
Tape. `SPEC.md` names the event type `interrupted`, and the chat buffer
shows it among the turn's other entries.

The rule differs from the proposal in two places, both found while
building it. Notes are skipped with events, because the `note` tool appends
its note before its `tool` result, so a run cut off there ends on a note. A
`user` entry right after an `error` event is not marked, because a failed
handoff ends that way: its error event, then the user entry.

## Alternatives considered

- Write a start and a finish marker for each run, turn and tool call, and
  recover by reducing them, as pi's "Durable AgentHarness and session
  design" (`pi-mono/packages/agent/docs/durable-harness.md`) proposes. Not
  taken: pi needs the markers for its queues and pending writes, and
  Superchat has neither: one run per timeline at most, and a second is an
  error. The state that matters can be read from the entries already
  there.
- Continue the cut-off run by itself when Emacs starts again, pi's optional
  `retry_unfinished`. Not taken: a `write` or `exec` call may already have
  had its effect, and pi's own default is to mark the operation
  interrupted. A later decision may rerun only `read` calls, whose class
  says they are safe to run again.
- Leave it as it was: the next request drops the unanswered calls. Not
  taken: the model and the person are not told that an effect may have
  happened.

## Consequences

- The model sees an error result for a call that was cut off and can check
  whether its effect happened before going on.
- A timeline left unfinished by a defect, not by Emacs exiting, is marked
  as if Emacs had stopped; the event says only what is known.
- A timeline that ends with a `user` entry right after an `error` event
  from an earlier run, then cut off, is not marked: it looks the same as a
  failed handoff. No call had an effect in that case.
- The mark is made only when the next run starts; a timeline opened and
  read but not continued shows none.
- Building it showed that the chat buffer dropped entries of a turn that
  arrived together with the next `user` entry; `superchat-chat--add` now
  draws the old turn first.
- `spec/L3-agent.md`'s word ceiling went from 2240 to 2460: the rule is
  new, belongs to L3 and repeats no other home.
