# Slice 11d — the K2C divergence (report; no change made, no run spent)

Implementer session 39daf101, 2026-10-20. Read only: the nine K2C runs of 2026-10-13 in `.scratch/task-sim/runs/K2C-real-{D13,L4,L4find}-t{1,2,3}-*`, all on code `d840243f`.

## Where pass and fail part

Every failure has the same failing check, `first-execution-left-it-continuable` (L4find t2 also fails two follow-on checks). The fork is the model's last `work_record` call of the first execution, after it has done the first three books:

| run | verdict | last work_record of the first execution | record afterwards |
|---|---|---|---|
| D13 t1 | pass | update progress + resume | continuable |
| L4 t1 | pass | update progress + blocked ("waiting for the user to save") + resume | continuable |
| L4find t1 | pass | update progress + blocked ("paused as planned") + resume | continuable |
| L4 t3 | pass | update progress + resume | continuable |
| D13 t2 | fail | update … **wait_for user**, detail | waiting |
| L4 t2 | fail | update … **wait_for user**, detail | waiting |
| L4find t2 | fail | update, then **wait wait_for user** | waiting |
| D13 t3 | fail | start … **wait_for user**, detail | waiting |
| L4find t3 | fail | update … **wait_for user**, detail | waiting |

So the deciding step is one choice on the same request: having done three of six books because the user said "今天只做前三本，剩下的下次再做——把这件事的进度记下来", the model either records progress and stops (4 runs), or registers a wait for the user (5 runs). The model sees the same `work_record` description in all nine; the `blocked` text in the passing L4/L4find t1 shows they had the same reading and only did not add `wait_for`.

## What the check rewards is the wrong product behaviour

On `d840243f` the rounds of slice 7 are on by default (`superchat-work-rounds-when-started`), and the description the model reads says so: "if a turn ends with it neither done nor waiting, Superchat starts the next round itself". In all four passing runs the first Emacs's Tape ends with `work.outcome continuable`, then `queue.queued` and `work.round 1/256`: **Superchat had already queued the next round to do the remaining three books**. It never ran only because the harness quits the first Emacs as soon as the user's execution ends (`progress.log`: evidence, check, quit). In a person's Emacs those four runs would have finished all six books at once, against the user's explicit "the rest next time".

The five failing runs registered a wait for the user, which is exactly what keeps the rounds from going on, and the second conversation then picked the work up and completed it (four of the five end `completed`; L4find t2 stops at waiting). Under the product as it is, they did the right thing; the K2C check was written (M11 slice 7, before goal rounds) for a product without rounds.

## Why I did not make the one text change and the 3 + 3 runs

The only wording that could make K2C 3/3 under its current check is one that steers the model away from `wait_for user` when the user defers work — which, with rounds on, makes Superchat do the deferred work immediately. That would raise the number by making the product disobey the user, so I stopped before spending the six real runs. Nothing was changed; no TS_EXTRA_LISP file was written.

## What could be decided instead (your call)

1. **Fix the check, not the model** (harness only, `checks-goals.el`): the first execution passes when it leaves the record either `continuable` *with no round queued* or `waiting-user`; and add "no round ran in the first Emacs" as a check. Under that check the nine old runs would read 5 pass (the waits) and 4 fail (a round was queued) — to be confirmed by re-judging, which the slice-10 judge can do offline from the old evidence only partly (the old runs have no evidence/gather/).
2. **Give the model a plain way to say "the user deferred the rest"** (product text and probably code): today `wait_for user` is the only way, and its description is about questions ("For a question the user may answer later, register wait_for user"). A sentence such as "When the user says to do the rest later, register wait_for user with what is left, so Superchat does not go on by itself" is a text-only change, but it moves the model toward what the old check calls a failure; it fits only together with 1.
3. Leave K2C out of the surface numbers until 1 is decided; it is the one task whose verdict depends on the harness quitting before a queued round runs.

Evidence: per-run `evidence/result-phase1.json` and `evidence/http/*-request.json` (the `work_record` arguments, complete), `evidence/events-phase1.json` (`work.outcome`, `queue.queued`, `work.round`), `evidence/progress.log`.

---

# Slice 11d, after the leader's rulings (2026-10-20)

## (1) The check, fixed (harness only)

- `harness-tools/run-task.el`: the first Emacs of a restart task now runs the same generic gathering as the end and keeps it as `evidence/gather/state-phase1.eld` (nothing task-specific is gathered).
- `harness-tools/checks-goals.el`, K2C: `first-execution-left-it-continuable` is gone. Three checks replace it:
  - `first-execution-did-the-first-three`: the reading list in the first Emacs's buffer has exactly the first three STATUS values right and the last three without one. Read only from the gathered buffer, never from disk (the disk is the second Emacs's).
  - `first-execution-left-it-for-later`: the first `work.outcome` is `waiting` with a `work.waiting` `:for "user"`, or it is `continuable` and the first Emacs has no `queue.queued` and no `work.round`.
  - `no-round-in-the-first-emacs`: no `queue.queued` and no `work.round` in `events-phase1.json`.
- Offline re-judge, **partial**: the old runs have no first-Emacs gathering, so `did-the-first-three` cannot be judged; the other two use the very defuns of checks-goals.el (`slice-11d-k2c-rejudge.el`, results in `slice-11d-k2c-rejudge.tsv`). The nine runs of 10-13 flip exactly: the 4 old passes now fail both (round queued), the 5 old failures pass both; the run of 10-20 passes both.

## (2) Real baseline on D13: 3 runs, 2 pass

| run | verdict | first outcome | what it waited for | round in first Emacs | first three only |
|---|---|---|---|---|---|
| `K2C-real-11d-D13-t1-20261020-232936-31578` | pass | waiting | user | none | yes |
| `K2C-real-11d-D13-t2-20261020-233045-34063` | **fail** (`left-for-later`) | waiting | **time**, until 2026-10-21 23:31 (24 h later), detail "用户说'剩下的下次再做'，本轮先停下" | none | yes |
| `K2C-real-11d-D13-t3-20261020-233151-36505` | pass | waiting | user | none | yes |

All three: the second conversation picked the record up and completed it, all six marked right. Judge copied in after the model's Emacs ended (emacs.log); leak scan 3 runs, 0 hits.

## (3) Not triggered

The one failure is not "left continuable with a round queued"; it is a wait for a time 24 hours on, which Superchat would wake by itself tomorrow and do the rest without the user saying so. Per ruling (3) I did not add the wording and did not make the 3 + 3 runs. Whether a wait until "tomorrow" should count as "the rest next time" is a question about the check (and maybe the wording), for you.

---

# Slice 11d, option 2 experiment (2026-10-20)

Ruling: a `wait_for time` to a moment the user did not give is a failure, named apart. The judge now reports `info:first-failure-kind`: `left continuable with a round queued`, `deferred to a time the user did not give`, or `none`. The earlier D13 t2 (`K2C-real-11d-D13-t2-20261020-233045-34063`) is of the second kind.

The sentence, harness only, through `TS_EXTRA_LISP=ts-defer-wording.el` (advice on `superchat--maybe-make-llm-tool`, appended to the `work_record` description; the product wording is unchanged and waits for the user):

> When the user says to do the rest later, register wait_for user with what is left in detail; Superchat will not go on by itself.

Confirmed on the wire: the sentence is in the first request of all six runs.

| run | verdict | first outcome / failure kind |
|---|---|---|
| `K2C-real-11d-defer-D13-t1-20261020-233514-39503` | pass | waiting for user / none |
| `K2C-real-11d-defer-D13-t2-20261020-233609-42050` | pass | waiting for user / none |
| `K2C-real-11d-defer-D13-t3-20261020-233712-44562` | pass | waiting for user / none |
| `T4W-real-11d-defer-D13-t1-20261020-233805-46993` | pass | waited for its time, woke, completed |
| `T4W-real-11d-defer-D13-t2-20261020-234049-50247` | pass | same |
| `T4W-real-11d-defer-D13-t3-20261020-234333-52891` | pass | same |

K2C 3/3 with the sentence, against 2/3 without it on the same day and code (one deferred to a time). T4W 3/3: the sentence did not stop a time wait the user asked for. Leak scan: 6 runs, 0 hits. Three runs per arm is too few to call the difference an effect; it is a direction, not a measurement.

Real runs spent in 11d: 3 baseline + 6 here = 9.
