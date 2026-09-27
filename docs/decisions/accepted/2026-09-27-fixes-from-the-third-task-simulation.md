# Decision: what the third task simulation settles — the reviewer comes out; streamed text keeps its order; the finish check reads the whole last sentence

Status: accepted — user, 2026-09-28: "都按建议" (Q1 A: remove the reviewer; Q2 A: commit both fixes; Q3 A: report the ordering defect upstream).

## 一句话

第三批模拟测试把同样 6 个任务各跑 3 次，一组关审核员、一组开审核员。开着的审核员没抓到一个要紧的错话，反而让回答平均多等 19 秒、长了一大截，开头还总在回应一个用户看不见的纠正；所以建议把它拿掉，只保留“本次实际改动”那一行记录。测试中还发现两个与审核员无关的真缺陷：模型的回答偶尔有两个相邻片段顺序颠倒（`init.el` 显示成 `.elinit`），收尾检查会被句子里的点（文件名、e.g.、版本号）骗过。两处都已修好并测过，等你同意后提交。

打个比方：请来的审核员总在挑“你没证明你没重启电脑”这种毛病，真写错的“4 篇文章”却没看出来；而那两个缺陷，一个像打字机偶尔把相邻两个字打反，一个像读句子时把“init.el”里的点当成了句号。

## Evidence

`.scratch/task-sim/THIRD-BATCH-REPORT.md`; issues
`.scratch/issues/2026-09-27-stream-order`,
`.scratch/issues/2026-09-27-finish-check-dots`.

- Reviewer (Part B of `accepted/2026-09-27-claims-checked-against-the-record.md`),
  13 reviewed Runs: 12 findings, 1 OK; no material false claim caught; three
  times it called the unsaved buffer an unfinished request (saving is the
  person's); it missed a report's contradictory "4 篇"; +18.9 s and +3.4
  model requests per reviewed Run; corrected answers 74% longer, each opening
  on the invisible note ("You're right on both counts", "Corrected report").
- Stream order: 4 of 36 runs; the proxy's raw stream is correct, the kept
  answer has one adjacent swap.  plz-event-source 0.1.3 (and upstream main)
  gives each server-sent event its own timer at `current-time`; Emacs runs
  timers of equal time newest first.  Batch reproduction: 66 of 200 bursts
  of 20 events out of order; with the clock held still, (5 4 3 2 1).
- Finish check: `superchat-llm--finish-check-last-sentence` ends a sentence
  at every `.`; off-run T6 t2 ended on "Let me check whether ... (e.g. ...)."
  and the Run stopped with the task undone.

## Decision

1. **Part B is removed**: `superchat-report-review`, the review request and
   its correction, the `report-review` surface and `run.report-reviewed`.
   Part A (the record line under a tool-using answer) stays.  The accepted
   decision gains an amendment pointing here.
2. **Streamed events keep their arrival order.**  While plz-event-source
   dispatches as it does, Superchat replaces `plz-event-source--dispatch-event`
   (advice, installed only when the function takes two arguments) with the
   same dispatch whose timer is set strictly after the previous event's — one
   picosecond later on a tie; a clock set back by a second or more is taken
   as it is, so no event is held back.  Every provider llm.el streams through
   server-sent events (DeepSeek, OpenAI and compatible, Claude) is covered.
3. **The finish check reads the whole last sentence.**  `。！？` always end a
   sentence; `.` `!` `?` only before whitespace, and a dot closing an
   "e.g."-style abbreviation (two or more letter-dot pairs) does not.
4. **Upstream**: whether to report the ordering defect to plz-event-source
   (github.com/r0man/plz-event-source) is the user's choice; nothing is
   posted without it.

## Alternatives considered

**Keep the reviewer off as an experimental switch.**  Lost: code, a surface
and a Tape type to maintain for a check that showed no catch; the invisible-
note problem would still need solving before anyone turned it on.

**Rewrite the reviewer's question and measure again.**  Possible later (a
reviewer told that saving is the person's, that negatives need no proof,
that only unsupported lines are listed); it costs another 18 real runs and
does not by itself fix the answer opening on a note the person cannot see.

**Only report the ordering defect upstream.**  Lost: until a release, about
one streamed answer in ten shows a swap in every Superchat.

## Acceptance

- `scripts/task-sim-third-fixes-regression.el` (5 tests; 3 red before the
  fix at the gap, 2 guards); mutants: no advice, no tie-break, tie-break
  across a clock step, dots always ending a sentence, no abbreviation rule —
  each caught.  `report-review-regression.el` is removed with Part B.
- The full sweep passes.
- A later real batch shows no swapped answer (`harness/stream-order-check.py`).
