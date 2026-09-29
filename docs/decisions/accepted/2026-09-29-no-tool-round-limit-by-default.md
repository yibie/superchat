# Decision: no tool round limit by default

Status: accepted — user, 2026-09-29: "工具调用次数为何限制得那么死", then "A" to "follow DeepSeek Harness: no per-task round limit by default; whoever wants one sets it; `C-c C-k` stops a task that has gone astray".

## 一句话

以前一个任务默认最多 12 轮工具调用，到了就停下来问你要不要再给 12 轮。这个数没有留下来历；DeepSeek Harness 和 Pi 的主循环都不设这种每个任务的上限。从现在起默认不限，想要上限的人自己设 `superchat-llm-max-tool-rounds`，任务跑偏时用 `C-c C-k` 停下。

## Evidence

- `superchat-llm-max-tool-rounds` defaulted to 12 with `superchat-llm-round-limit-action` `ask`; neither the code nor the decision records say why 12.  `implemented/2026-09-17-model-attempt-progress-and-budget-finalization.md` declined to raise or remove it ("a larger number is not evidence ...").
- DeepSeek Harness (local checkout): the agent loop has no per-turn round cap; `max_turn_requests` is a turn-end reason no producer emits yet; the only aggregate limits are goal rounds and Ralph rounds (256 each) and a subagent preset (64).  Pi: no per-turn limit in its sources.
- Task simulation 2026-09-29: 4 of 19 failures spent the budget (24 rounds there: the stand-in grants one extension); some were working towards the answer, some had lost the task.
- The same direction was taken for time on 2026-09-19 (`implemented/2026-09-19-no-cumulative-transport-limit-by-default.md`).

## Decision

1. `superchat-llm-max-tool-rounds` defaults to nil: a Run's tool rounds are not counted against a limit, the person is not asked, and no tool-free final request is forced.
2. A number still works exactly as before (the boundary action, the budget notice, the tool-free final request, the Tape record); `superchat-llm-round-limit-action` applies only then.
3. What bounds a Run by default: the person (`C-c C-k` cancels), the Run deadline (`superchat-runtime-default-deadline`, 1800 s, stopped while the person is being asked), the inactivity watchdog, and the provider's context window.  Sub-agents keep their own call limit (`superchat-agent-max-tool-calls`).
4. The 2026-09-17 decision gains an amendment pointing here: its budget finalization stands for anyone who sets a limit; its refusal to change the default is superseded.

## Alternatives considered

**Raise the default to 50 and keep asking.**  Lost: still an unexplained number, and the question interrupts a person who is away.

**Keep 12.**  Lost: it is stricter than both harnesses the project measures itself against, with no recorded reason.

## Consequences

- A model that has lost the task keeps calling tools until the person stops it, the Run deadline passes or the context fills.  The task simulation keeps its own 600-second deadline.
- The task simulation's stand-in for the budget question no longer comes into play.

## Acceptance

- `scripts/no-round-limit-regression.el`: the standard value is nil and the type accepts nil and a number; with the default, a Run of 30 tool rounds reaches its answer with no question asked and no budget recorded; red before the change at the gap.
- The existing budget tests (which set their own limits) pass unchanged; the full sweep passes.
