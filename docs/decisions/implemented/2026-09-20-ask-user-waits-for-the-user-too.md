# Decision: `ask_user` 等人回答时，Run 总期限同样不走

Status: implemented（2026-09-20；同日 accepted——用户裁定："ask_user 要"）

## 一句话

模型用 `ask_user` 向用户提问、聊天窗口里挂着"Required input … Waiting for your answer"面板的那段时间，不计入 Run 总期限。面板一直等到用户回答或按 Stop；回答之后期限从剩下的时间接着走。与[批准提问](2026-09-20-the-run-deadline-waits-for-an-open-question.md)同一条规则、同一个机制。

## 为什么

同一条原则：Superchat 停下来问用户一句话，就应该等用户；时间上限是给卡住的模型和无人值守的 Run 准备的。实测（真实执行管线，期限 2 秒）：问题挂着时 Run 在 2 秒 `timed-out`，面板变成 "Expired"，之后的回答被拒收。

`ask_user` 的面板不是模态的——用户可以去做别的，Run 会挂着占住这个 Session 的队列。这一点用户知情并接受：面板上有 Stop 按钮，`C-c C-k` 也能结束它；无进展计时在 `ask_user` 等待期间本来就是停的。

## 契约

1. Run 有一个已发布、尚未结清的 `ask_user` 问题期间，总期限停表；问题结清（回答、Stop、失败）时续表，期限顺延停表的时长。与未决的批准提问合并计算：两者任一存在就停，两者都没有了才续。
2. 停表期间期限**不会到点**：回答时和继续之前的同步期限检查，读到的是"没有过期"。问题发布**之前**期限已经过了的，照旧 `timed-out`（那不是等人造成的）。
3. 停表期间，给展示层的问题快照里 `:deadline` 为 nil（没有正在走的期限）；展示层目前不使用这个字段。
4. 不变的：问题的资格检查、单次结清、Q1／Q2 的归属、Stop 与取消、`audit-incomplete` 规则、无进展计时与（用户若设了的）累计上限在 `ask_user` 期间的行为。无人值守的 Session（批准策略 `never`）本来就不提供 `ask_user`，不受影响。

## 验收

真实执行管线（脚本模型调用 `ask_user`）：

1. 期限 2 秒，问题挂 4 秒后回答 → 回答被接受，模型继续，Run `completed`。
2. 同样挂 3 秒后回答，随后模型沉默 → 回答之后按剩余期限超时。
3. 守护：问题挂着时用户 Stop／取消 → Run 结束，不因期限，也不复活。
4. 守护：没有任何问题时期限照常（已有测试）。

1、2 在实现前必须是红的。其余套件、canonical smoke、Kernel 检查不变。

## 交付记录（2026-09-20）

- 实现（`superchat-runtime.el`）：把前一个决定的停表逻辑收成一条规则——`superchat-runtime--asking-p`（有未决批准，或有已发布未结清的 `ask_user` 问题）加 `superchat-runtime--deadline-sync`；批准的记账点、`ask_user` 问题的发布点和结清点都调用它。同步的期限检查统一走 `superchat-runtime--deadline-passed-p`（停表期间不会过期）。问题发布之前期限已过的，保持原来的顺序：先发布、不停表、由原有检查以 `timed-out` 结束。问题快照的 `:deadline` 在停表期间为 nil。
- 回归（`scripts/approval-wait-regression.el`，13 个，新增 3 个，真实执行管线、脚本模型调用 `ask_user`）：期限 2 秒、问题挂 4 秒后回答 → 回答被接受、模型收到答案、Run 完成；挂 3 秒后回答、随后模型沉默 → 回答后约 2 秒因期限超时；问题挂着时取消 → Run 结束、晚到的回答不被接受、没有第二次请求。实现前三个都红（Run 在 2 秒 `timed-out`，回答被拒收）。
- 变异体：`ask_user` 问题不算"在问人"→ 三个都红；问题结清后不续表 → "按剩余期限超时"红；回答时的期限检查无视停表 → 两个红。
- 受保护的 M3 回归集这次不受影响（139 个里 137 通过、2 跳过）。其余全部套件、canonical smoke（无基线变化）、Kernel 检查、加载通过；编译警告与基线一致。
