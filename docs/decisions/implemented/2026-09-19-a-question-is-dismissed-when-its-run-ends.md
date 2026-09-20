# Decision: Run 结束时，还开着的批准提问一并收场

Status: implemented（2026-09-19；同日 accepted——用户选定"这次一并修"）

## 一句话

批准提问等着人回答的时候，如果它所属的 Run 已经结束（总期限到了、在别处被取消、失败），提问立刻自己关掉，并告诉用户"这个 Run 已经结束，这次调用没有执行"——不再留下一个看上去还活着、按了也没用的提问。

## 为什么

实测（真实执行管线，Run 期限 2 秒，"人"读 4 秒）：期限到点，Run 以 `audit-incomplete` 结束；提问继续开着，直到人回答；回答被记进 Tape（`approval.decided` 落在 `run.failed` 之后），工具不执行，没有任何提示。用户 2026-09-19 裁定人的等待**应当**计入 Run 总期限（1800 秒），所以这种到点会真实发生；600 秒累计上限默认关闭之后，它是唯一还会在提问期间到点的时钟。取消也一样：Run 在别处被取消时，提问同样被留在屏幕上。

## 契约

1. 批准链在调用**最后的回答者**（默认的 `y`/`n`/`?` 提问，或脚本／插件换上的函数）期间，盯着这个 Run 是否还能执行动作（`superchat-runtime-effect-capable-p`）。一旦不能，立刻放弃这次提问：回答者被非局部退出打断（与 `C-g` 打断它是同一类事），默认提问从屏幕上消失，不需要任何按键。`approval/request` 的 waterfall listener 不在此列。
2. 结局记为拒绝：`approval.decided` 的 `:decision "deny"`、`:outcome "cancelled"`、`:reason "run-ended"`、`:interruption "run-ended"`——与用户 `C-g`（`"user-quit"`）区分开。工具不执行，不发起新的模型请求。
3. 告诉用户一行：哪个工具在等回答、Run 已经结束（带当时的状态）、这次调用没有执行。
4. **不变的**：Run 的终态不变——提问未决时封口的 Run 仍是 `audit-incomplete`（M3a／M6 已钉住的规则），本决定不把它改成 `timed-out`；晚到的回答不能复活 Run；提问出现之前的输入不能回答它；键盘宏不算人；批准等待期间无进展计时停表。没有所属 Run 的直接调用（没有 `:run-id`）不受影响。
5. 不新增计时器选项，不显示倒计时。

## 验收

1. 真实执行管线：Run 期限到点时回答者仍在阻塞 → 回答者在期限后约 1 秒内被放弃（不必等它自己返回）；Tape 上 `approval.decided` 为 `cancelled`／`run-ended`；工具未执行、没有第二次请求；用户收到点名该工具的一行消息；Run 终态仍是 `audit-incomplete`。
2. 同上，但 Run 是在提问期间被 `superchat-runtime-cancel` 取消的。
3. 交互式 Emacs（伪终端子进程，真实 `read-multiple-choice`）：不送任何按键，Run 结束后提问自行消失并返回。
4. 守护：Run 活着时照常回答；没有 `:run-id` 的直接调用照常（现有 `approval-input/*`、`approval-wait/*`、`m6-permissions/*` 不变）。

1、2、3 在实现前必须是红的。其余套件、canonical smoke、Kernel 检查不变。

## 交付记录（2026-09-19）

- 实现（`superchat-approval.el`）：`superchat-approval--ask-while-run-lives` 在调用最后的回答者期间每 0.5 秒看一次 Run 是否还能执行动作，不能了就用非局部退出放弃回答者（与 `with-timeout` 同一种机制），收掉帮助窗口，经 `superchat-approval--announce` 告诉用户；批准链把这种结局记为 `cancelled`，`approval.decided` 带 `:reason "run-ended" :interruption "run-ended"`。
- 回归：`approval-wait/a-deadline-with-the-question-open-dismisses-it`、`approval-wait/a-cancel-with-the-question-open-dismisses-it`（真实执行管线），`approval-input/the-question-goes-away-when-its-run-ends`（伪终端里的交互式子 Emacs，真实 `read-multiple-choice`，不送任何按键）。实现前三个都红：回答者被整整等满 8 秒；交互式提问不会自己消失。
- 变异体：放弃了但记成普通拒绝；放弃了但不告诉用户；过修——Run 活着也放弃提问——各自变红（最后一个同时让"读很久后允许"变红）。
- **受保护回归集的一处 leader 修改**：`m3a/seal/an-undecided-approval-seals-audit-incomplete-whatever-the-requested-ending` 的回答者在 `sleep-for` **之后**才记录"Run 是否已结束"，这假设了提问比 Run 活得久；现在回答者在睡眠中被放弃，那一行不再执行。改动只是把这笔观察放进 `unwind-protect`；断言一个字没动。证据（改前改后的读数、在旧产品上同样通过、运行时变异体仍让它变红）记在 `.scratch/m3-verification/REGRESSION-SET.md`。该测试钉住的产品规则——`audit-incomplete`、工具不执行、单一终态——前后都成立。
- 其余全部套件、canonical smoke（无基线变化）、Kernel 检查、加载通过；编译警告与基线一致。
