# A Run records who started it, and five rulings on closing the loop

Date: 2026-10-11
Status: the five rulings accepted; slice 2 implemented; slice 3 (ruling 2)
implemented in [giving up and looking are facts](2026-10-11-giving-up-and-looking-are-facts.md);
ruling 1 implemented in [a stopped execution is not a failed one](2026-10-11-a-stopped-execution-is-not-a-failed-one.md);
rulings 3, 4 and 5 not started
Source: `reports/guidance-2026-10-11/GUIDANCE.md` (slice 2 and the five
questions it left for the user).

## The five rulings

The questions were put to the user as written below, each with the
guidance's recommendation in brackets. The user answered "同意你的建议。"
(2026-10-11), which accepts all five recommendations.

1. An execution the user stopped is recorded apart from one that failed,
   under a new word, `stopped`. [yes]
2. "Looked at" is written to the Tape, so it is remembered across a
   restart. [yes]
3. An execution the user started that ended after its window was gone is
   listed as "ended, not looked at" in `superchat-pending`. It does not
   count in the mode line. [list it, in the list only]
4. An approval question is answered in the conversation buffer and from
   `superchat-pending`; the minibuffer remains the fallback. [so]
5. The conversation a woken Work Record opens does not have to be a
   buffer. [it need not be]

None of the five is implemented by this record. Rulings 1 to 3 shape slice
3, ruling 4 slice 5, ruling 5 slice 6.

## What changed (slice 2)

A Run that Superchat started at a time says so on its own `run.started`:
the Actor is `timers` for a scheduled task and `work-records` for a woken
Work Record. A Run the user started keeps the Actor it had (`lead`). Before
this, all three recorded `lead`, so the Tape row of a Run could not say who
started it; that was read from the facts around it.

Nothing the user sees changes.

- `superchat-automated-input` is bound to the sender's name (`'timers`,
  `'work-records`) where it was bound to `t`. Every existing reader tests it
  for non-nil only.
- `superchat--plan-say-who-started` puts that name on the request plan as
  `:actor-id`. It is called at the three places a chat plan is made: the
  plain message, the command with a template, and the request composed for
  the queue.
- `superchat-changes--unattended-run-p` also counts a Run whose
  `run.started` carries one of the two Actors. The older reading
  (`timer.started` naming the Run; the first Run of a conversation holding
  `work.woken`) stays, for Runs recorded before today.
- `superchat-changes--notice-what` names a scheduled task whose Run ended
  before `timer.started` was written by the `timer.sending` recorded just
  before that Run began.

## Not done, against the guidance

The guidance asked that `unattended-run-p` stop replaying the conversation.
It still replays it once per call. Deciding from the Run's Actor alone
would have changed what seven existing tests assert (they write
`timer.started` or `work.woken` by hand onto Runs the user started), and
would have dropped the reading that Runs from before today depend on. A
version that asked the Runtime first and replayed only otherwise was
written and removed: it changed no outcome, so no test could tell it from
its absence.

## Evidence

`scripts/run-actor-regression.el`, 7 tests. On the code before the change:

| Test | Red on | Value then |
|---|---|---|
| a scheduled task's Run is started by timers | `(equal '("timers") …)` | `("lead")` |
| a woken record's Run is started by work-records | `(equal '("work-records") …)` | `("lead")` |
| the user's message after a woken Run is the user's | `(equal '("work-records" "lead") …)` | `("lead" "lead")` |
| the user's own message keeps its Actor | guard, passed | |

The other three were added after the first mutation pass and were not run
on the old code.

Mutations, on a copy: 10 made, 6 found at first. Of the 4 missed, three got
a test each (the command-with-template path, the queued request, a Run read
after this Emacs forgot it); the fourth was the Runtime-first reading
described above, removed. One more check (`notice-what` asking the Run's
Actor before using `timer.sending`) could not be made to matter and was
removed as well. After that, 9 of 9 found.

Thirteen related suites pass (run-actor 7, unattended-notice 11,
beside-user 13, pending 24, timer-fault 15, timer 17, work-record 25,
work-wake-fault 17, object-scope 18, timer-tool 5, work-tool 10,
input-syntax 18). Script tests only: no real model, no person at the
keyboard.

## Known limits

- Runs recorded before today carry `lead` whoever started them.
- A subagent's Run is not touched: it has a parent Run and its own Actor.
- Only the chat send is covered. Nothing else starts a Run on Superchat's
  own initiative today.
