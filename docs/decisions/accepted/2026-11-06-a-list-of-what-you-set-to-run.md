# Decision: 一张表列出你设下的定时任务和规则

Status: accepted — 用户 2026-11-06。用户取消一个每小时跑的任务时问"怎么在任务列表里停"，看了 `M-x superchat-pending` 后说"感觉这个定时任务的面板有点怪，它连日志都包含了"，又问"查看这个任务的命令为何是 superchat-pending 呢？感觉语义上关联不大"。我建议分成两个命令，用户答："对，我想写的是 superchat-schedule-task，如果是这样的话，就需要一个列表，上面能列出多少任务，以及这些任务的性质和状态，如果是定时任务，还需要显示执行了多少次，以免过多日志堆积"。列、按键和状态的说法是实现时定的，都可以改。

## Problem

`superchat-pending` 是 10-11 做的"还没了结的事"，后来装进了定时器、规则、跑完没看的结果、等批准的操作、没保存的 buffer。要找"我设了哪些定时任务"，得在这些东西里挑；一个每小时跑的任务每跑一次多一行，读起来像日志。

## Decision

**新命令 `M-x superchat-schedule-task`，只列你设下的、还在的定时器和规则，一样一行。**

- **表头：** 一共几个，几个定时器、几条规则，以及按键。
- **列：**
  - 种类：`reminder`、`task`、`command`、`command + brief`、`rule (compile)`、`rule (feeds)`。
  - 什么时候：`once`、`hourly`、`daily`……，规则写它在什么时候看。
  - 下一次。
  - 跑了几次。
  - 最近一次的结果：命令和规则是 `no change`、`told` 这类，任务是它回答的第一行。
  - 状态：`set`、`running`、`due, not started`、`N not looked at`。
  - 文字。
- **按键：** `RET` 看最近一次结果，算作看过；`t` 把最近一次结果带进你正在写的消息；`k` 先问你再取消；`g` 重读。
- **已取消的、只跑一次且已跑过的** 不列。
- **`superchat-pending` 不变，** 还是"所有还没了结、要你注意的事"，模式行的标记点开还是它。同一天它也改了：同一个任务跑了好几次只占一行，任务取消后那一行一起去掉。

## 实现时定的

- **只读别的模块已经记下的东西，** 不写新事实；唯一会写的是取消，在你答应之后。
- **跑了几次：** 任务和提醒数 `timer.fired`；命令和规则数 `command.result`（跳过的、被替换的不算），和 `superchat-timer-command-stats` 一样。
- **`RET` 进的是任务跑的那段对话，** 命令和规则是它打印的内容，和管家里的 `RET` 一样（`superchat-butler-open-line`，从 `superchat-butler-open` 拆出来）。

## 验证

- `scripts/schedule-task-regression.el`，11 个测试，真定时器、真命令、脚本化的模型：每种一行且种类对；跑 3 次的任务一行写 3 次和最近的结论；命令写结果；`k` 你不答应就不取消；跑完的一次性任务不列；`RET` 之后不再算没看；`t` 带进的是最近一次；没有任务时表头写 0；`due, not started` 和 `running` 两种状态。`feed/the-schedule-lists-it-as-a-feed-rule` 测 RSS 规则那一行。
- 故意改坏 11 处，第一轮 3 处没抓到（`running`、`due, not started`、RSS 规则的种类），补了测试后全部抓到。
- 整套回归 133 个文件 2207 项，只有 `m5-regression` 那 3 项不过，这之前就不过。
- 没在你的 Emacs 里看过。表是 `tabulated-list`，窗口窄时长的列会被截断。
