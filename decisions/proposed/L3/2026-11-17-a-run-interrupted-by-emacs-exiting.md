# Decision: A run interrupted by Emacs exiting is marked on the Tape

Status: proposed

## Problem

A run lives in the memory of Emacs. When Emacs exits during a run (a
crash, `kill-emacs`, the machine stops), the Tape keeps what was appended
and nothing more. When Emacs starts again, the timeline is free, and
nothing on the Tape says that a run was cut off.

The worst place to be cut off is between an `assistant` entry with calls
and the `tool` entries that answer them. A call with no `tool` entry is not
sent (L3, "Messages"), so the next request drops it without a word. That
call may have run halfway: a `shell` command started, an `edit` made, a file
saved. Neither the model nor the person is told.

## Proposal

- A timeline is **unfinished** when its last entry that is not an `event`
  is a `user` entry, an `assistant` entry with calls, or a `tool` entry,
  and no `error` or `cancelled` event follows it. This is read from the
  Tape; no new marker is written while a run goes. A run that ends `done`
  ends with an `assistant` entry with no calls, and every other ending
  appends an `error` or `cancelled` event, so a finished timeline is never
  unfinished.
- When `superchat-run` starts on an unfinished timeline that has no active
  run, it first appends, for each call of the last `assistant` entry that
  has no `tool` entry, a `tool` entry with `:error t` and the text
  `Interrupted: Emacs stopped while this call ran; it may have had effects.`,
  then an `event` of type `interrupted`. Then the run goes on as now.
- Nothing is run again by itself, and no request is sent again by itself.
  The person's next message continues the conversation from the Tape.
- The table of entry kinds names the event type `interrupted`.

## Alternatives considered

- Write a start and a finish marker for each run, turn and tool call, and
  recover by reducing them, as pi's "Durable AgentHarness and session
  design" (`pi-mono/packages/agent/docs/durable-harness.md`) proposes. Not
  taken: pi needs the markers for its queues and pending writes, and
  Superchat has neither: one run per timeline at most, and a second is an
  error. The state that matters can be read from the entries already
  there.
- Continue the cut-off run by itself when Emacs starts again, pi's optional
  `retry_unfinished`. Not taken now: a `write` or `exec` call may already
  have had its effect, and pi's own default is to mark the operation
  interrupted. A later note may rerun only `read` calls, whose class says
  they are safe to run again.
- Leave it as it is: the next request drops the unanswered calls. Not
  taken: the model and the person are not told that an effect may have
  happened.

## Acceptance criteria

- On a Tape whose timeline ends with an `assistant` entry with two calls
  and one `tool` entry, `superchat-run` appends one `Interrupted:` `tool`
  entry for the other call and one `interrupted` event before the new
  `user` entry, and the request sends both results.
- On a timeline that ends with an `assistant` entry with no calls, or with
  an `error` or `cancelled` event, `superchat-run` appends nothing extra.
- On a timeline that ends with a `user` entry, only the `interrupted`
  event is appended.
- A timeline with an active run is never marked.
- The chat buffer shows the `interrupted` event when it shows the timeline.

## Risks

- A timeline left unfinished by a defect, not by Emacs exiting, is marked
  as if Emacs had stopped. The event says what is known, an unfinished run,
  and the person reads the rest.
- The mark is made only when the next run starts. A timeline opened and
  read but never continued shows no mark; `superchat-chat.el` would need
  its own check to show one sooner.
