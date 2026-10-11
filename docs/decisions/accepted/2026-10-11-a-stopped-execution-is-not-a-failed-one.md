# A stopped execution is not a failed one

Date: 2026-10-11
Status: implemented
Source: ruling 1 of
[a Run records who started it](2026-10-11-a-run-records-who-started-it.md)
("An execution the user stopped is recorded apart from one that failed,
under a new word, `stopped`"), and the user's "同意建议" (2026-10-11) to
doing it next, after finding out why a stop ended as `failed`. Closes FR-12
of `reports/fixes-review-2026-10-11/`.

## What was found

- The Runtime already tells the two apart. Stopping an execution ends its
  Run `cancelled` and appends `run.cancelled`; nothing there says `failed`.
- `failed` came from one place: `superchat-work-outcome`, which names how
  an execution left its Work Record, called everything that had not
  completed `failed`.
- `run.cancelled` does not say who cancelled. Its payload is the status and
  nothing else.

## What changed

- `superchat-work-outcome` answers `stopped` for a Run that ended
  `cancelled`. What the execution did to the record still comes first: one
  that completed the record, or registered a wait still standing, is
  `completed` or `waiting` whether or not it was stopped afterwards. The
  catch-up after a restart (`superchat-work-settle-missing`) reads the same
  terminal row and gives the same word.
- A record whose last execution was stopped, and that has no conclusion,
  says so in its text: "Its last execution was stopped before it finished
  (recorded by Superchat): check what it left before going on." The model
  that takes the record up reads this, as it does after an interruption.
- `superchat-pending`: the "Can be continued" row of such a record reads
  "its last execution was stopped", followed by the resume point when there
  is one. An execution Superchat started that was stopped reads "stopped"
  in "Ended, not looked at", where it read "cancelled".
- The Runtime, the Tape's terminal rows and the notices' stored status are
  not changed.

## What `stopped` does not say

It says the Run was cancelled before it finished, not by whom. The user's
stop is one way a Run is cancelled; the product cancels too: a sub-agent
when its parent ended, a Run whose Sink was lost, one whose model transport
was withdrawn, a blocking caller that exited. A Work Record's execution in
a conversation is reached by those rarely, but when it is, it reads
`stopped` as well. Telling them apart needs the reason on `run.cancelled`,
which this does not add.

## Evidence

Script tests only. `scripts/work-outcome-regression.el` gained 5 tests
(18), `scripts/pending-regression.el` 3 (27).

On the code before this change, 7 of the 8 were run: 6 failed, 1 passed.

| Test | Red on |
|---|---|
| `work-outcome/a-stopped-execution-is-recorded-as-stopped` | the outcome fact, was `failed` |
| `work-outcome/a-stopped-execution-is-caught-up-as-stopped` | the outcome fact, was `failed` |
| `work-outcome/the-stop-is-told-only-while-it-is-the-last-thing` | the outcomes, were `("failed" "continuable")` |
| `pending/a-record-whose-execution-was-stopped-says-so` | the row's detail, was the resume point alone |
| `pending/a-stopped-record-with-no-resume-point-still-says-stopped` | the row's detail, was `""` |
| `pending/work-superchat-started-that-was-stopped-reads-stopped` | the row's detail, was `cancelled` |
| `work-outcome/a-stop-after-the-record-was-completed-is-completed` | passed: a guard |

`work-outcome/a-record-with-a-conclusion-is-not-told-of-the-stop` was added
after the mutation pass and was not run on the old code.

Mutations, on a copy: 10; the first pass caught 9. The one missed (the
record's text naming the stop after a conclusion) got that test and is
caught now.

The review's probe `fr-pending/step-12-stopped-from-the-list` (not edited)
now fails on its assertion that the outcomes are `("failed")`.

## Known limits

- Who cancelled is not recorded (above).
- Records settled before this change keep `failed` for executions that were
  stopped; nothing is rewritten.
- The conversation itself shows a stop as it did before; whether that is
  clear enough is step 12 of the hands-on checklist, not done.
- Not verified: a real model, a person at the keyboard.
