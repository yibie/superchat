# Decision: the operating note says the origin buffer is not the task's limit

Status: accepted — user, 2026-10-01: "同意，继续" to adding guidance for two failure classes of the goals batch (`.scratch/task-sim/GOALS-BATCH-REPORT.md`): the model not looking beyond the buffer it was called from, and the model stopping at the first obstacle. The first is delivered here. The second was written, measured the same day and withdrawn.

## 一句话

问"我接下来 7 天有什么事"时，模型常常只读了你当时所在的那个文件，就回答"我访问不了你的日程"。操作说明里加一句：当前文件只是你所在的位置，不是任务范围；日程、订阅、笔记、文件、仓库都在这个 Emacs 里，先问 Emacs，再说够不着。

## Problem

- Each request names its origin buffer (`[Emacs origin]`). Nothing in the system text says the user's other data is in the same Emacs and can be asked for. In task A1 (org agenda configured, the request sent from an unrelated file) the model read the origin buffer, called nothing else and answered that it could not reach the agenda files: 4 of 8 runs on the old note (goals-first t1, t2; note-old t4, t5).
- The note's contract capped it at 1,024 characters; it had 1,023.

## Decision

1. The note gains one line: "The origin buffer is where the user is, not the task's limit. Their agenda, feeds, notes, files and repositories live in this Emacs: ask it with eval_elisp (the packages and variables they configured) before saying you cannot reach something."
2. The cap is 1,280 characters; the note has 1,268 (about 310 tokens on each request that carries it, 60 more than before).
3. Smoke baseline A changes only by the note's text.

## Evidence (deepseek-v4-flash, real; `.scratch/task-sim/runs/`)

| | old note | new line |
|---|---|---|
| A1, answered without looking beyond the origin buffer | 4 of 8 | 0 of 11 |
| A1 passed | 4 of 8 | 10 of 11 (one Run cancelled by C-g while an org-agenda internal froze Emacs) |
| T6 (window layout), same hour | 4 of 6 | 3 of 6 |

The 11 new-line runs are 5 with both sentences (batch note-new) and 6 with this line alone (note2-new). T6 was unstable for both that hour; the new-line runs took longer, and one opened a new diary file instead of the existing one. This is not shown to be an effect of the line, and not shown not to be.

## Withdrawn the same day

"When a step inside the task fails, try another way the task allows; only when none is left, say what blocks you." It was aimed at A2 (a case-only rename: 0 of 3 in the first batch, each stopping at "target exists"). Same-hour runs: old note 5 of 5, with the sentence 4 of 5 (the failure renamed a file it should not have); T6 with it 1 of 3. The first batch's 0 of 3 was not caused by the note. The 2026-09-27 sentence stands unchanged.

## Alternatives considered

**A line in the `[Emacs origin]` block instead.** Lost: that block is facts about one buffer, frozen byte for byte in several baselines; advice belongs to the note.

**A reference page for org-agenda.** Not excluded; it answers "how", while the failure was not trying at all.

## Consequences

- Requests with `eval_elisp` or `edit_buffer` carry about 60 more tokens.
- `scripts/whole-emacs-guidance-regression.el` pins the line and the cap; the two older files' cap assertion moves to 1,280.
