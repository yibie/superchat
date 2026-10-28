# Decision: eval_elisp 里可以调用别的工具（eval 当 codemode）

Status: accepted 2026-10-26, implemented，待真实任务实测。用户的方向（2026-10-26）："尽量减少工具数量……使用 eval_elisp 这是没问题的"；"我愿意提供一个 automode，来跳过审批"；"emacs 本身已经是一个半沙箱环境"；"我认为 pi 的 codemode 还是有必要引入"。leader 给了两种做法：A，eval 当主力，表达式里经工具管线调用别的工具；B，照 Pi 另做一个独立脚本工具。用户选 A。下面的入口名、上限、拒绝的措辞、中断时记为 cancelled，都是实现时定的，用户可以改。

证据的范围：脚本模型，批处理 Emacs。没有真实模型跑过。

## Problem

第 6 片的 `elisp_script`（[记录](2026-10-02-m11-slice-6-elisp-script.md)）在 DeepSeek 上实测比不用脚本更差（8/15、12/15，不用脚本是 14/15），已经撤出默认工具。主要原因是语言受限：第一版 25 个脚本里 14 个被拒，多数只是想调用只读函数；放宽后 73 个里仍有 24 个被拒，多数是模型想在脚本里直接动手。

与此同时，`eval_elisp` 占了真实调用的 71%（data 实例 tape，2213 次中 1579 次，Fable 2026-10-23 的一次性统计）。模型已经在把 eval 当万能手用，但 eval 里改 buffer 没有 `edit_buffer` 的护栏（唯一匹配、括号配平），也没有层报告（"Layer: in memory only"）。G13 的 2458 行损坏就是这样来的。

## Decision

1. **入口 `superchat-tool`**：`(superchat-tool "NAME" :ARG VALUE ...)`，在 `eval_elisp` 表达式里和 `elisp_script` 脚本里都能用。它是全局函数，不是只在表达式字面里有效的局部绑定，所以表达式里定义的 lambda 里也能调用。被调工具执行失败时，返回它的失败文本，表达式继续；调用方式错了（没有这个工具、参数名不对、不能在这里调用、超过上限、不在工具调用里），就抛错，表达式失败。`superchat-script.el` 原来脚本里的 `tool` 现在就是调它。
2. **每次嵌套调用都走 `superchat-tool-execute`**，编号是"eval 调用编号/序号"。权限、Tape 上的 `tool.requested`/`tool.completed`、改动记账都和模型直接调用一样。
3. **会被审批的嵌套调用直接拒绝，不提问**（`superchat-tool-execution-cannot-ask`）。eval 的表达式在 `inhibit-interaction` 和 30 秒时限下运行；审批问题发布以后要等用户回答，而表达式运行时用户的回答进不来，只能等到时限。所以这类调用立刻返回 "[Tool call refused: … make it as a tool call of its own]"，告诉模型把它当成单独的一次工具调用。
4. **eval 里不能调用的**：`eval_elisp` 自己、`elisp_script`、`ask_user`、委派子代理，以及异步工具。
5. **上限**：一个表达式最多 100 次嵌套调用，和脚本相同。
6. **时限或 C-g 打断时，嵌套调用照样有结尾。** eval 被时限或 C-g 停下时是用 `throw` 跳出的，会越过嵌套调用自己的记账，那次调用就一直挂着没结束，整个 Run 最后记为 `audit-incomplete`（测试先复现了这个结果）。现在 `superchat-tool` 发现自己被跳出、而那次调用还没结束时，把它记为 `cancelled`（`superchat-tool-execution-settle-left`）。`cancelled` 不算失败，不进刹车的计数。
7. **eval_elisp 的说明加一句**：怎么用 `superchat-tool`，并提醒只返回需要的部分（值太长会剪掉中间）。

## 已知的副作用

- 刹车（连续 3 次失败就停）也数嵌套调用的失败：Runtime 按 Run 计连续失败，不分是不是嵌套的。一个表达式里连着 3 次嵌套调用失败（比如 buffer 名写错了还在循环里调），这次执行就会被停下。用户已经决定不给刹车分级，这里只记下来；默认工具减少以后，走嵌套调用的流量会变多，要在实测里数。
- 被拒绝的嵌套调用在 Tape 上记为 `tool.requested`（`:decision "ask"`）加 `tool.failed`（`denied`）。目前没有哪个投影按 `"ask"` 计数。

## 没有在这次做的

- 默认工具从 13 个减到 5 个。这依赖本片，但要先实测。
- eval 改了 buffer 时在结果里报告（tick 快照）。
- G32：`copy-file`、`rename-file`、`delete-file` 不受 eval 写盘拦截。
- 把"测 buffer 当前内容"的两层做法写进 eval 的说明。

## 验证

- `scripts/eval-code-mode-regression.el`，8 个测试：一次 eval 里先读后改只发两次请求，嵌套调用各有完整记录且编号在 eval 之下，改动进了未保存改动的记录；只有表达式的值回到模型；嵌套失败返回文本、eval 本身成功；不能调用的工具和错误参数各有说明；`edit-buffers` 档位下嵌套 `save_buffer` 被立刻拒绝，审批只问了 eval 自己那一次；嵌套调用被时限打断时记为结束，Run 正常完成；在工具调用之外用 `superchat-tool` 会报错说明用处；100 次上限。
- 审批那条测试用的是同步回答的替身：它证明的是"没有去问、10 秒内有结果"，不是"真的发布问题就会卡死"。卡死是从代码读出来的推论（审批发布后等待，而表达式运行时回答进不来）。
- 原有套件照常通过：elisp-script 18、eval-elisp 7、eval-write 7、eval-failure 13、approval-wait 13（加上时限修正之前跑的）、m6-permissions 62、tool-quit 5、composition 22、pending 27、m5-tool-feedback 33、approval-auto 14、m3 94（2 个跳过）、m5 11。

## 待实测

同第 6 片的 A1、A2、T6，各 5 次，同一模型：通过率不低于 14/15；数每个任务的请求次数、eval 里用 `superchat-tool` 的次数、被拒的嵌套调用次数。实测之前不改默认工具清单。
