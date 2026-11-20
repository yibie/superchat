# Decision: A dreaming timer is known by a key

Status: implemented

## Problem

`superchat-timer-dream` found a scope's dreaming timer by comparing its text
with the dreaming task. Changing that task's words (as
[tools and skills are found as needed](../L0/2026-11-20-tools-and-skills-are-found-as-needed.md)
did) left the person's timer unfound: `/dream` set a second one and
`/dream off` could not stop the old.

## Decision

- A dreaming timer carries `:dream t` in its data, and
  `superchat-timer-dream` finds it by that key and its scope, never by its
  text.
- A timer fires with the text it was set with; the task's words can change
  without touching timers already set.
- The task no longer says to load `recall` and `note`: they are offered in
  every request
  ([recall and note are offered in every request](../L2/2026-11-20-recall-and-note-are-offered-in-every-request.md)).

## Alternatives considered

- Keep the text as the key and never change the task's words. Not taken:
  the task is a prompt, and prompts are tuned.
- Find old dreaming timers by their text as well, so the person's earlier
  timer is found. Not taken: that is migration code, which rule 3 forbids.

## Consequences

- A dreaming timer set before this change has no `:dream`: the person
  cancels it with `/timers cancel ID` and sends `/dream` once more.
