# Decision: 审批在问的地方回答

Status: accepted — 2026-10-12。用户亲手做完实机清单第 9 步之后提的要求，由 leader 会话转来：审批的交互不要只靠 `M-x` 命令，要像 `ask_user` 那样在对话里直接选。只有脚本测试；面板在真实窗口里的样子、按钮用键盘和鼠标按下去的手感，没有人看过。

## Problem

切片 5（[审批先发布，之后再回答](2026-10-11-an-approval-is-published-then-answered.md)）把问题画成对话末尾的一行纯文字，回答要另打命令。用户实机用过之后还指出两处措辞：

- 用户答了"不允许"，工具结果写的是 `[Tool call cancelled by user]`。那是拒绝，不是取消。
- 审批挂着时，对话底部的状态行显示 `Prefill / waiting first token… 1m59s`，把等用户的时间算成了等模型。

## Decision

1. **对话里是一个面板。** 和 `ask_user` 的问题用同一种画法（`superchat--input-show` 那样的原生按钮组，插在这次执行的对话记录里）：一行"Allow `工具名` once?"，一行状态，三个按钮 `[Allow once]` `[Deny]` `[Details]`。前两个调用现有的 `superchat-approval-answer`，第三个显示工具、原因和完整参数。
2. **审批不走 `ask_user` 工具。** `ask_user` 是模型的工具、异步的；审批是产品发起、工具调用同步等它的值。这次只换了呈现，发布、等待、回答、记账都没有动。
3. **面板留在对话记录里，写明怎么结束的。** 回答之后状态行变成 `Allowed once` 或 `Denied`；执行先结束了则是 `No longer open; nothing was allowed`。两个回答按钮随之停用。过期的按钮什么都不决定（`superchat-approval-answer` 对已结束的问题返回 nil）。
4. **画不出面板就退回原来那一行。** 对话没有活着的窗口 buffer、这次执行在那里没有自己的对话记录（例如子代理）、或画的过程中出错，都显示切片 5 的纯文字行。
5. **命令和表上的 `y` / `n` 保留**，小缓冲区兜底（`superchat-approval-prompt`）不变。
6. **拒绝说成拒绝。** 用户答"不允许"时，交给模型的工具结果是 `[Tool call denied by user: the user answered no to the approval question, so the tool did not run]`。执行结束、中途放弃等其它情形仍是 `[Tool call cancelled by user]`；`never` 策略的文字不变。
7. **状态行说等的是谁。** 一次执行有审批问题开着时，状态行是 `Waiting for your answer… <从提问起的时间>`。

## 实现时定的，用户可以改

- 按钮的字样是英文的 `Allow once` / `Deny` / `Details`，和 `ask_user` 面板的 `Answer` / `Stop` / `Hide` 一致。
- 没有"停掉这次执行"的按钮；停掉仍用表上的 `k` 或原有的停止命令。
- 面板永久留在对话记录里（和 `ask_user` 的面板一样），没有"隐藏"按钮。
- Tape 上工具失败的状态词仍是 `cancelled`（`superchat-tool-execution--record-failure` 的那个参数没有改），改的只是交给模型的文字。要不要给状态加一个 `denied`，没有问过用户。

## 先红后绿

`scripts/approval-publish-regression.el` 新增 5 条、改 1 条、另补 1 条。前 6 条在旧代码（`314d94c2`）上都红：

| 用例 | 旧代码上红在哪 |
|---|---|
| `the-conversation-shows-a-panel-of-three-buttons` | 看到的按钮应为 `("Allow once" "Deny" "Details")`，实际没有按钮（`:tags nil`） |
| `pressing-deny-answers-no` | 记下的回答应为 `(nil)`，实际是 `(:no-button)`（没有 Deny 按钮可按） |
| `pressing-allow-once-answers-yes` | `pressed` 应为真，实际为 nil |
| `an-answered-panel-says-so-and-its-buttons-do-nothing` | 对话里应有 `Denied`，实际没有 |
| `the-status-line-says-it-waits-for-you` | 应含 `Waiting for your answer`，实际是 `Prefill / waiting first token… ` |
| `a-later-no-refuses-the-call`（改了断言） | 应含 `Tool call denied by user`，实际是 `[Tool call cancelled by user]` |

后补的 `a-conversation-that-cannot-hold-a-panel-shows-the-line` 没有在旧代码上跑。现在 17 条全过。

## 变异测试

在主目录下的副本上改坏 11 处：从不画面板、Deny 回答成允许、Allow 回答成拒绝、两个结束字样各自写反、状态行不看问题、拒绝文字改回去、面板从不收尾、画面板出错不接住、没有 Details 按钮、按别的执行找提问时间。第一轮发现 10 处；漏的一处（拒绝写成 `Allowed once`）是因为断言不分大小写，匹配到了工具记录里的 "denied"。断言改成区分大小写、整行匹配后，11 处全部被发现。

没有被任何测试覆盖的：回答之后两个按钮"停用"这件事本身。测试是直接调用按钮的动作，过期的动作靠 `superchat-approval-answer` 返回 nil 而无效。

## 改了的现有测试

都不属于 M3 回归集。五条各改一行，把用户答"不"时断言的 `Tool call cancelled by user` 改成 `Tool call denied by user`：`tool-material-regression.el`、`approval-wait-regression.el`、`m6-permissions-regression.el`、`tool-observation-regression.el`、`m5-tool-feedback-regression.el`。另外切片 5 自己的一条用例从"读 overlay 里的文字"改成"读对话里的面板"。

## 没有证实的

- 没有人在真实窗口里看过面板，也没有人用键盘或鼠标按过按钮。测试调用的是按钮的动作函数。
- 状态行只验证了它的文字函数，没有验证问题开着时它在真实窗口里确实被定时刷新成这句话。
- 真实模型读到新的拒绝文字后说什么。

## 没有解决的

- 切片 5 列的几条都还在：工具管线同步等待、问题只在内存里、无人看守的权限级别、`top-level` 绕过记账。
- 用户说"模式行的方括号没看到在哪"。这次没有为它做任何事。
