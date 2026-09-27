# Decision: a model's report is checked against what the Run actually did

Status: proposed — the user chose, 2026-09-27 ("按照建议"), to have this researched before deciding; the options below await the user.

## 一句话

模拟测试两批里，有 4 次模型的汇报和它自己的操作记录对不上：没改的文件说改了，摆反的窗口说摆好了，数据里没有的日期编了出来，检查结果明明是“无”却写成了“✅”。superchat 现在对这类话一概照单全收：没改东西就不弹审阅界面，界面上什么也不提示。这份提案给出几种“对账”办法。

打个比方：员工交来一份工作汇报，写着“已经把合同改好放你桌上了”。以前我们只看汇报；以后在汇报下面自动附一张“今天实际经手的文件清单”，要不要再请一位审核员对照着看一遍，由你决定。

## Problem (evidence)

- Batch 1 T4-3: "现在实测：… `(key-binding (kbd "C-c j"))` → `my/open-diary` ✅" while its own last check returned `:effective nil`.
- Batch 2 T4-3: never opened or edited `init.el` (one `read_file`), answered that the edit is in the unsaved `init.el` buffer and asked the person to save it. `superchat-changes-review-after-run` opens the review screen only when a Run leaves unsaved buffers, so nothing on screen contradicted the claim.
- Batch 2 T6-1: arranged diary right and gtd left — the reverse of the request — reported that layout exactly and said the split "is in place".
- Batch 2 T3-1 (and batch 1 T3-3): a date that is not in the data, chosen to put an item inside the requested week.

Sources: `.scratch/task-sim/FIRST-BATCH-REPORT.md`, `SECOND-BATCH-REPORT.md`.

## What others do

- **DeepSeek Harness**: no check of a report against the record. Its goal loop's round prompt tells the model to treat tool results and the workspace as authoritative and to "gather evidence that the whole objective is achieved" before marking the goal complete (`packages/goal/goal-round-driver/src/prompt.ts`) — an instruction, not a check. `guard` covers repeated tool calls and timeouts only.
- **Pi**: nothing; its system prompt says nothing about verification; its `before_run_end` seam can add a follow-up but ships no content check.
- **superchat today**: one finish check after a text-only reply (`docs/decisions/accepted/2026-09-23-one-finish-check-after-a-text-only-reply.md`) — about an unfinished reply, not a false one. The previous survey found no reference that checks reply content.
- **OpenHands**: a separate critic model scores the agent's work (0–1, with feedback) after the agent calls `FinishAction`; below a threshold (default 0.6) a follow-up prompt is sent automatically, up to 3 iterations (https://docs.openhands.dev/sdk/guides/critic).
- **Execution receipts / execution logs** (industry practice, e.g. https://github.com/zeroclaw-labs/zeroclaw/issues/4830): show or check the real execution log next to the answer; a claimed action with no matching record is flagged.

## Options

1. **A record line under every tool-using answer (deterministic, no model call).** When a Run that used tools ends, the chat shows one product-written line after the answer: which buffers this Run changed (unsaved or saved), whether it ran Emacs Lisp, or "this task changed no buffer or file". The person sees the contradiction at once (batch 2 T4-3 would read "本次没有改动任何 buffer 或文件" under "请你保存 init.el"). It does not stop a wrong claim; it makes it visible. Data exists: mutation targets on `tool.completed`, the change list behind `/changes`.
2. **A mechanical mismatch check fed back to the model (deterministic).** At the existing turn-stopping seam: when the answer names an open buffer or file as edited/changed/saved (or 改了/编辑了/保存) and the Run recorded no change to it, send one correction ("you say you edited X; this Run made no change to X; correct the report or do the edit") before the answer is final. Catches only claims about edits; a wording list is brittle (false alarms, misses).
3. **A reviewer call (OpenHands's shape, with the same model).** After a tool-using Run's final answer, one extra request with a fresh context: the person's request, the Run's tool lines (the same one-line observations later turns already see), and the answer; it lists claims the record does not support and parts of the request not met. If it lists any, they go back to the model once as a correction. Catches all four observed cases in principle; costs one request and several seconds per tool-using Run; a model reviewing its own kind of output may miss what it would itself invent.
4. **An instruction only (DeepSeek Harness's goal-round wording).** Add to the operating note: report only what a tool result in this Run shows. Cheapest; the note already says "Report what you checked", and the runs above happened with it.

## Recommendation

1 now (it costs nothing per request and removes the silent case), and 3 as an option measured in the task simulation before it becomes a default (same tasks with and without the reviewer: how many false claims it catches, how many false alarms, the added time). 2 and 4 are not recommended: 2 is brittle wording rules; 4 did not prevent the observed cases.

## Alternatives considered

Covered as options above. Signed receipts (HMAC per tool result) solve a problem superchat does not have — the Tape already holds the authentic record; what is missing is comparing it with the answer.

## Acceptance criteria

For 1: a tool-using Run that changed nothing shows the "changed nothing" line; one that edited a buffer names it and its saved/unsaved state; a Run without tools shows no line; the line is product text, never part of what the model is sent. For 3 (if chosen): the task simulation's claim failures replayed through it, and a same-tasks comparison of catches, false alarms and time.

## Risks

- 1 adds a line to every tool-using answer; if it is noisy the person learns to skip it. Keep it one line.
- 3 doubles the model's work at the end of a task and can itself be wrong; it must never change the buffer, only send a correction the model acts on.
