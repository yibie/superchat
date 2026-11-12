# Decision: 重启后补做和进展检查，现有的已经够，不另做

Status: accepted — 用户 2026-11-02 说"great, next step"，下一步是[提案](../proposed/2026-10-31-an-agent-that-acts-without-being-asked.md)第 9 步：别人写的内容，以及重启后补事件、进展检查。这一条只管后两样，查下来不用新代码。别人写的内容另写记录。

## Problem

提案把两件事放在第 9 步：

- **重启后补事件：** Emacs 关着的时候到点的事，开了以后怎么办；关的时候正在做的事，怎么办。原型用一张副作用队列，重启后接着做。
- **进展检查：** 文章第 6 节的 Watch Dog。文章说，很多异常不是进程崩了，而是模型陷在无效循环里、或者偏离了目标，所以要有一个外部的回路看着任务有没有进展。

一个人的 Emacs 不是一台常开的服务器。要先查清现在缺什么，再决定做不做。

## 查到的

**重启后补做，已经有：**

- 关着时到点的定时任务，Superchat 加载时补跑一次；重复的只补最近一次（`superchat-timer-catch-up`）。
- 退出时正在跑的任务记成 `timer.unconfirmed`，不再启动，列表里写 "May have started"，7 天内都在（`superchat-pending-unconfirmed-days`）。
- 退出时被打断的唤醒、上一个 Emacs 留下的结果，重启后管家里各有一行（`own-place/a-wake-cut-off-by-exiting-emacs-has-its-line`、`own-place/what-an-earlier-emacs-left-gets-a-line-when-superchat-starts`）。
- 编译失败这类事件，Emacs 关着时根本不会发生，没有可补的。RSS 也一样：elfeed 只在 Emacs 里更新。

**进展检查，已经有程序的上限：**

- 没人看着的任务一直调用工具，到执行期限（`superchat-runtime-default-deadline`，1800 秒）被停下，管家和列表里各一行，写着 `timed-out` 和原因。这次用探针确认过，写成了测试。
- 规则启动的执行各有自己的上限：查 180 秒、15 次读取；修 600 秒、3 次构建；每天的次数。
- 第 7 步的失控（67 次请求）已经由程序在 `nothing_to_tell` 之后当场结束。
- 子代理最多 120 次工具调用。工作记录连续 2 轮没有进展就停下等用户（`superchat-work-round-idle-limit`）。

## Decision

**不做副作用队列，也不做由模型判断进展的 Watch Dog。** 补做已经由各模块按 Tape 上的事实完成；进展由程序的上限兜住。模型判断"有没有进展"本身要花钱，而且它的话不算数，这和第 7、8 步的原则一样：程序决定，模型只写。

**`timer.unconfirmed` 的缺口照旧留着。** 原型能做到重启后不重复发送，是因为请求号由键确定；这里请求号是随机的，说不清是否发出去时只能记下、不再启动、告诉用户。一个人的 Emacs 退出时正好在发请求的情况少，等真实使用里出现再说。

## Consequences

- **费用：** 一个跑偏的、没人看着的任务，最多花掉 1800 秒的请求才被停下。没人按 `C-c C-k`。2026-09-29 用户定的"不设轮数上限"是以有人看着为前提的；规则启动的执行有自己的上限，用户亲手设的定时任务只有这个期限。要更紧，设 `superchat-runtime-default-deadline` 或 `superchat-llm-max-tool-rounds`。
- 原型的副作用队列和幂等投递仍然没有对应物，[研究页的对照表](../../research/super-agent.md#实现对照)照旧写着"缺"。

## 验证

`scripts/own-place-regression.el` 的 `own-place/a-runaway-task-ends-at-its-deadline-and-has-a-line`：脚本模型一直调用 `read_file`，期限设成 2 秒，任务被停下，管家一行 `timed-out`，列表里也有。改代码之前它就通过：这是守住现有行为的测试，不是先红后绿的测试。own-place 全部 55 项通过。
