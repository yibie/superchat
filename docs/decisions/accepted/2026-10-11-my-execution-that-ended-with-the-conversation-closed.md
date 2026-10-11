# An execution of mine that ended with its conversation closed is listed

Date: 2026-10-11
Status: implemented
Source: ruling 3 of
[a Run records who started it](2026-10-11-a-run-records-who-started-it.md)
("An execution the user started that ended after its window was closed is
listed as ended and not looked at; in the list only, not in the mode line's
count"), and the user's "同意，接着做第 3 条" (2026-10-11). Closes L2 of
`reports/fixes-review-2026-10-11/`.

## What changed

- One new fact, `run.ended-unwatched` (a session annotation, Actor `user`,
  the Run id in the envelope, payload `run-id` and `status`). The `changes`
  plugin's end-of-execution observer writes it when all of these hold:
  - the execution's Actor is not `timers` or `work-records`;
  - no other execution delegated it (`parent-run-id` is nil);
  - it was started from a conversation (surface `chat`), not asked from a
    buffer in place, rewritten, or run by a workflow;
  - the conversation has no live View when it ends.
- `superchat-pending` lists it under "Ended, not looked at", "started by
  you", with how it ended. `RET` goes to the conversation and `d` dismisses
  it; both record `run.seen`, as for the other rows.
- The mode line's `[Superchat: N]` and `superchat-changes-notices` count and
  show only what Superchat started by itself, as before. `D` there dismisses
  what that list shows and leaves the user's own rows.
- The row outlives a restart, like the others, and is gone once looked at.

## Decided while building it

The ruling says "the window was closed" without saying what that is; these
are mine and the user may change them.

- **Closed means the View's buffer is gone.** The product's own close
  (`superchat-close-session-workspace`) kills it, and the step-4 scenario
  "close the conversation while it runs" did the same. A conversation whose
  buffer is alive but shown in no window is not closed: counting that would
  list nearly every execution of someone who works in another window.
- **Looked at means `RET` or `d` on the row.** Opening the conversation some
  other way does not clear it. Nothing else writes `run.seen`.
- **The fact says how the execution ended.** A Run the user started does
  not hold the read's bound, so the bound can pass its `run.started` while
  it runs; the fact carries the status and lists the Run by itself.
- **When the fact cannot be written** the echo area says so, the execution's
  ending is not disturbed, and the row is not shown.

## Evidence

Script tests only. `scripts/seen-regression.el` gained 8 tests (21).

On the code before this change all 8 were run: 7 failed, 1 passed.

| Test | Red on |
|---|---|
| `my-execution-that-ended-with-the-conversation-closed-is-listed` | `(= 1 (length facts))`, was 0 |
| `my-unwatched-execution-outlives-a-restart-until-looked-at` | the rows listed, were none |
| `only-an-execution-started-from-a-conversation-counts` | the facts, were none |
| `the-fact-lists-my-execution-though-its-start-is-settled` | the rows listed, were none |
| `the-notices-list-dismisses-only-what-it-shows` | the rows listed, had only Superchat's own |
| `an-unwatched-ending-that-cannot-be-recorded-breaks-nothing` | the echo area's line, was not said |
| `work-superchat-started-is-one-row-with-the-conversation-closed` | the notice's `:origin`, was absent |
| `my-execution-that-ended-in-an-open-conversation-is-not-listed` | passed: a guard |

The review's probe
`fr-pending/a-users-execution-that-ended-while-its-view-was-killed-is-listed-nowhere`
(not edited) now fails: the row is there.

## Known limits

- An execution of the user's that Emacs quit in the middle of (L4 of the
  review) has no ending and no observer, so no fact: it is still listed
  nowhere.
- The conversations Telega and emacsclient start were not traced; whether
  they have a View in the sense used here is not known.
- A conversation hidden but alive is not "closed" (above).
- The count of unsaved buffers is of this Emacs, as for the other rows.
- Not verified: a real model, a person at the keyboard.
