# Giving up and looking are facts; the notices are read from the Tape

Date: 2026-10-11
Status: implemented (slice 3 of `reports/guidance-2026-10-11/GUIDANCE.md`)
Source: the user, 2026-10-11: "切片 3，把“放弃了”“看过了”写进记录".
Ruling 2 of
[a Run records who started it](2026-10-11-a-run-records-who-started-it.md)
("looked at" is written to the Tape and outlives a restart) is what this
implements. Rulings 1 (a `stopped` outcome) and 3 (the user's own
executions that ended with no window) stay accepted and not implemented.

## What changed

Three facts are new on the Tape, all session annotations:

| Fact | Written by | When | Payload |
|---|---|---|---|
| `timer.gave-up` | `superchat-timer--try-task` (Actor `timers`) | the failure that uses up the last try in this Emacs | `timer-id`, `occurrence`, `tries` |
| `work.wake-gave-up` | `superchat-work--try-wake` (Actor `work-records`) | the same, for waking a Work Record | `work-id`, `tries` |
| `run.seen` | `superchat-changes-notice-dismiss` (Actor `user`) | the user looked at an ended execution; the Run id is in the envelope | `run-id` |

- **Giving up is written once.** The fact is appended where the count
  passes the limit, not in the branch that passes an out-of-tries task
  over, which every later sweep reaches. When the fact cannot be written
  the echo area says so and nothing else changes.
- **A new Emacs still tries again.** That was so before and is not
  changed; the fact records what this Emacs did.
- **Looking is the one exit.** `superchat-changes-notice-dismiss` is what
  the notices list (`RET`, `d`, `D`) and `superchat-pending` (`RET`, `d`)
  call. `D` writes one fact for each execution. When the fact cannot be
  written the notice stays and the echo area says so.
- **The notices are what the Tape says.**
  `superchat-changes--unseen-from-tape` reads, in one query over every
  conversation, the Runs that began above a bound, how each ended and
  which were looked at. A Run is listed when its `run.started` names
  `timers` or `work-records`, it has no `run.seen`, and it ended or was
  cut off. `superchat-changes--notices` is kept as the result of the last
  read; nothing that redraws reads the Tape.
- **Cut off is told without waiting for the conversation to be reopened.**
  A Run with no ending on the Tape that this Emacs is not running is listed
  as `cut-off`; once reopening seals it (`run.failed`, `abandoned`), it is
  the same row.
- **When the Tape is read:** when an execution Superchat started ends;
  when one is looked at; when the `changes` plugin is mounted (so a new
  Emacs shows what an earlier one left); and on `g` in `superchat-pending`.
- **The bound.** `projection_checkpoint` holds, under `changes-unseen`, the
  storage id below which nothing is left to look at: one less than the
  oldest listed or still running execution's `run.started`, or the last row
  read when there is none. It only moves up. It is a cache position, not a
  fact: deleting it makes the next read start from the beginning and give
  the same list.

## Not as the guidance has it

- **The bound.** The guidance offers "the id of the last `run.seen`" as the
  lower bound. That is unsound: an execution not looked at can lie below a
  newer one that was. The bound here is below the oldest one not looked at.
- **Where giving up is written.** The guidance points at the out-of-tries
  branch; written there, each sweep would add a fact. It is written at the
  failure that reaches the limit.
- **Only the Run's own word counts.** `superchat-changes--unattended-run-p`
  still tells a Run from before the Actor was recorded by the facts around
  it, for the review screen. The Tape read does not: such a Run names the
  user and is not listed. Runs from before 59d24169 (2026-10-11) therefore
  never appear as notices in a new Emacs.

## Evidence

`scripts/seen-regression.el`, 13 tests; script tests only, the model is
scripted and "a new Emacs" is the Runtime's table and the caches cleared,
then the read the mount does.

On the code before this change (with empty stand-ins for the three names
it lacks, so that each test reaches its assertion), 11 of the 13 were
run: 10 failed, 1 passed.

| Test | Red on |
|---|---|
| `a-task-out-of-tries-leaves-one-fact` | `(= 1 (length facts))`, was 0 |
| `a-record-out-of-tries-leaves-one-fact` | `(= 1 (length facts))`, was 0 |
| `a-notice-is-still-there-in-a-new-emacs` | `(= 1 (length notices))`, was 0 |
| `looking-is-a-fact-a-new-emacs-remembers` | `(= 1 (length facts))`, was 0 |
| `a-task-cut-off-when-emacs-quit-is-listed` | `(= 1 (length notices))`, was 0 |
| `dismissing-all-records-each-one` | the `run.seen` facts, was none |
| `a-look-that-cannot-be-recorded-keeps-the-notice` | `(should-not (dismiss run))` |
| `an-execution-the-user-started-is-not-listed` | the listed Runs, was none |
| `what-is-settled-is-not-read-again` | `(> bound began)`, bound was 0 |
| `a-reload-reads-once-however-many-conversations` | `(= 1 few)`, was 0: red only because the stand-in reads nothing |
| `the-list-reads-nothing-when-it-is-drawn` | passed: a guard |

`a-wake-that-is-tried-again-has-not-given-up` and
`mounting-the-plugin-reads-what-an-earlier-emacs-left` were added after the
mutation pass and were not run on the old code.

Mutations, on a copy: 22; the first pass caught 20. The two missed (the
wake's fact written at every failure; the mount not reading) each got a
test, and both are caught now.

The review's probes (`reports/fixes-review-2026-10-11/evidence/fr-loop-probes.el`,
not edited): `a-task-out-of-tries-leaves-no-durable-fact-…` now fails, the
fact being there. `a-scheduled-task-cut-off-mid-run-is-told-to-nobody-…`
still passes: its restart clears the Runtime's table and reopens the
conversation but does not mount the plugin, which is where a new Emacs
reads the Tape. `seen/a-task-cut-off-when-emacs-quit-is-listed` does that
read and has the row.

Cost of the read, on synthetic rows (not a real database): with the oldest
execution not looked at lying under 100,000 later rows, 0.037 s; after
everything was looked at, 0.0002 s.

## Existing tests changed

None is in the M3 set.

- `unattended-notice/several-notices-are-one-mark-with-their-number` and
  `unattended-notice/a-notice-counts-what-was-left-unsaved` built their
  executions as the user's and named them by a hand-written
  `timer.started`. They now start them as `timers`, which is what a task
  does since slice 2. The second also clears the unsaved count taken when
  its execution ended, because the test changes the buffer afterwards.

## Known limits

- How many buffers an execution left unsaved is counted when it ends and
  kept in this Emacs; a notice read in a new Emacs says none.
- What started a listed execution (which task, which record) is asked of
  its conversation once for each execution in an Emacs, by two or three
  indexed reads.
- `superchat-timer-retry-state` and `superchat-work-wake-retry-state` still
  answer from this Emacs's counts, not from the facts. A new Emacs tries
  again at once, so "given up earlier" is not a state the list has to show;
  the facts are in the conversation's Tape.
- An execution not looked at keeps the bound below it for as long as it
  stays; every read then covers the rows after it.
- Another Emacs on the same database is not considered: its running
  execution would read as cut off here.
- The user's own executions are not listed however they ended (ruling 3),
  and an execution the user stopped is still recorded as it was (ruling 1).
- Not verified: a real model, a person at the keyboard, a real database, a
  killed process.
