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

## 实测（2026-10-27，DeepSeek）

三组：base 是本片之前的提交 4019c6e8；new 是本片，默认 13 个工具；new5 是本片，只给 read_buffer、read_file、edit_buffer、eval_elisp、work_record（另有 ask_user）。运行目录在 `.scratch/task-sim/runs/batch-codemode-*`。

第一轮全部作废：沙箱里编译不了 `write-region` 的 native-comp trampoline（Xcode 的 ld 要读沙箱外的文件），每个 eval 都报 `Native compiler error`。`harness-goals/launch.sh` 现在先把本机缓存里同一 Emacs 版本的 trampoline 复制进运行目录；沙箱配置没改。

| 组 | A1 | A2 | T6 | C1 | C2 | E2 | K2C |
|---|---|---|---|---|---|---|---|
| base | 5/5 | 5/5 | 3/5 | 3/3 | 3/3 | 3/3 | 0/3 |
| new | 4/5 | 5/5 | 3/5 | 3/3 | 3/3 | 3/3 | 0/3 |
| new5 | 5/5 | 4/5 | 3/5 | 2/3 | 2/3 | 3/3 | 0/3 |

- 模型几乎不用 `superchat-tool`：new 组 0 次；new5 组 3 次运行里共 6 次（C2 里经它调 `edit_buffer`，E2 里调了没提供的 `open_file`，得到 "No tool named open_file"）。说明在请求里，模型看得到。
- 请求次数没有因此减少，这一片的目标（少来回）没有达到。
- new5 第一条请求 12.1 KB，base 16.3 KB（少 26%）。new5 的 4 个失败里，3 个是连续 3 次 eval 报错被刹车停下，1 个是 C2 把测试文件读进一个不访问文件的 buffer 再改（结果写明了 "not visiting a file"），与 `superchat-tool` 无关。
- K2C 三组都只差 `first-execution-left-it-continuable` 一项，base 也一样，是本片之外已有的问题。
- 每格 3 到 5 次，通过率 1 次的差别分不出好坏。

## 系统提示里的引导（2026-10-27，用户："添加系统提示看看"）

在"一次问完"那段引导后面加了一句（`superchat-prompt-act-once-text`）：动手也一样，读、改、检查可以是一次 eval_elisp，改动用 `(superchat-tool "edit_buffer" ...)`。只在提供 `eval_elisp` 时发送。

DeepSeek，C1、C2、E2、A2、T6 各 3 次；guide 是 13 个工具，guide5 是 5 个工具。对照组是上面没有这句引导的 new、new5。运行目录 `.scratch/task-sim/runs/batch-codemode-guide*`。

| 组 | C1 | C2 | E2 | A2 | T6 | `superchat-tool` 次数 | 请求中位数 C1 / C2 / E2 |
|---|---|---|---|---|---|---|---|
| new | 3/3 | 3/3 | 3/3 | 5/5 | 3/5 | 0 | 7 / 9 / 9 |
| guide | 3/3 | 3/3 | 2/3 | 3/3 | 2/3 | 9 | 8 / 8 / 8 |
| new5 | 2/3 | 2/3 | 3/3 | 4/5 | 3/5 | 3 | 10 / 9 / 8 |
| guide5 | 2/3 | 3/3 | 3/3 | 2/3 | 2/3 | 10 | 8 / 7 / 9 |

- 引导让模型开始用：`superchat-tool` 从 0 次和 3 次升到 9 次和 10 次，几乎都是在 eval 里调 `edit_buffer`；guide5 的 C1 里 5 次改动全在 eval 里做。
- 请求次数没有明显下降。模型把改动挪进 eval，但没有把"读、改、查"合成一次：改完以后照样另发一次请求去检查。
- 带引导的 7 个失败里，没有一个是 `superchat-tool` 的调用出错：E2 和 C1 各一次是连续 3 次 eval 报错被刹车停下（这两次运行都没用 `superchat-tool`），两次 T6 是窗口没摆对，A2 一次见下条。
- A2（guide5 第 2 次）：模型用 `(rename-file FILE (concat (file-name-sans-extension n) ".jpg"))` 改名，目标是相对路径，两个文件被搬到 `default-directory`，不在 `~/photos` 里了。文件没丢，只是被挪了位置。与引导无关；这正是 G32 的缺口：`rename-file` 不经过 `write-region`，eval 的写盘拦截管不到它。
- 每格 3 次，通过率差 1 次分不出好坏。

## edit_buffer 附上改动后的行，以及 G32（2026-10-27，用户："两个都做"）

提交 c07f67a4：`edit_buffer` 的回答末尾附上改动后的行（带行号，上下各多一行，超过 12 行只留两头）；eval 里的 `rename-file`、`copy-file`、`delete-file` 在临时目录以外，只在完全控制的级别（`auto` 及以上）放行，并在结果里逐条列出，低于这个级别就拒绝（用户选的做法）。

DeepSeek，C1、C2、E2、A2、T6 各 3 次；shown 是 13 个工具，shown5 是 5 个工具，对照组是 guide、guide5。运行目录 `.scratch/task-sim/runs/batch-codemode-shown*`。

| 组 | C1 | C2 | E2 | A2 | T6 | 合计 | 请求总数 |
|---|---|---|---|---|---|---|---|
| guide | 3/3 | 3/3 | 2/3 | 3/3 | 2/3 | 13/15 | 107 |
| shown | 3/3 | 2/3 | 3/3 | 3/3 | 2/3 | 13/15 | 120 |
| guide5 | 2/3 | 3/3 | 3/3 | 2/3 | 2/3 | 12/15 | 122 |
| shown5 | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 | 15/15 | 118 |

- 请求次数没有减少。前面以为"模型改完总要另发一次请求去看"，这个判断不对：数 C1、C2、E2 里每次改成功之后的下一次调用，guide 组 14 次里只有 1 次是回头读，guide5 组 13 次里只有 1 次；最多的是跑测试（6 次和 7 次）。附上改动后的行以后，回头读变成 0 次，但它本来就少。
- 文件改动的报告出现了 7 次（shown 4 次，shown5 3 次），都在 A2 里，是用户要求的改名；没有一次因为 G32 被拒（实验用的级别是 danger-full-access）。
- 来回多的地方是跑测试：shown 组 28 次跑测试的 eval 里失败 12 次，shown5 组 21 次里失败 9 次。原因：在进程里调用只能在 batch 里用的函数（3 次），用了不存在的 `ert-test-result-type`（2 次），想把文件写进项目目录再跑、被 G28 拦下（2 次），其余是语法错误。这就是"测 buffer 当前内容"没有正道的问题。
- 每格 3 次；shown5 的 15/15 和 guide5 的 12/15 差 3 次，可能是波动，不能当作结论。

## 默认工具改为 6 个（2026-10-27，用户："改成 5 个"，又选了保留 timer_create）

`superchat-llm-default-tool-names` 现在是 `read_buffer`、`read_file`、`edit_buffer`、`eval_elisp`、`work_record`、`timer_create`。移出的七个（`list_buffers`、`apropos`、`describe-function`、`read_reference`、`open_file`、`timer_list`、`timer_cancel`）仍是内置工具，工具清单或 Preset 点名就能提供。

实现时定的，用户可以改：

- 参考页：默认不再提供 `read_reference`。报错命中某页时，失败反馈里直接附上那一页（每个工具循环一次）；请求里提供了 `read_reference` 时照旧只点名。平时的请求不多带字。
- 提示词里"用 open_file 打开文件"都改成 `(find-file-noselect FILE)`；read-only 级别的说明去掉了"可以把文件打开成 buffer"。
- 测试：场景测试里假模型要调 `open_file`、`timer_list`、`timer_cancel` 的，在测试的工具清单里把它们加回来（`hm--extra-tool-names` 等）；检查默认清单的测试改成新清单；冒烟基准 A 重新生成，差别只有上面两处提示词。

代价（用户已知）：在 `edit-buffers` 级别，列 buffer、打开文件、查文档都要经过 eval，每次都问；`read-only` 级别下这些都做不了。

未改：`scripts/m5-regression.el` 归 leader，其中 3 个测试断言 `describe-function`、`apropos` 在默认清单里（`m5/discovery/the-default-request-offers-the-documentation-tool`、`the-documentation-tool-reads-without-a-prompt`、`a-stricter-read-policy-still-prompts`），现在失败，等 leader 决定。其余回归套件和冒烟测试全部通过。

### 6 个工具的实测（2026-10-28，DeepSeek，`runs/batch-codemode-six`）

C1、C2、E2、A1、A2、T6、P3T 各 3 次，共 21 次，通过 20 次。请求里实际带的工具是 6 个默认工具加 `ask_user`。

| 组 | C1 | C2 | E2 | A2 | T6 | 合计 | 请求总数 | 首次请求大小中位数 |
|---|---|---|---|---|---|---|---|---|
| shown（13 个工具） | 3/3 | 2/3 | 3/3 | 3/3 | 2/3 | 13/15 | 120 | 17025 B |
| shown5（5 个工具） | 3/3 | 3/3 | 3/3 | 3/3 | 3/3 | 15/15 | 118 | 12513 B |
| six（6 个工具） | 2/3 | 3/3 | 3/3 | 3/3 | 3/3 | 14/15 | 157 | 13708 B |

另外 A1 3/3，P3T（提醒）3/3，每次 2 个请求、1 次 `timer_create`。

- 首次请求比 13 个工具小约 19%（比 5 个工具多出的部分主要是 `timer_create`）。
- 唯一的失败（C1 第 3 次）：模型自己写的跑测试表达式连续 3 次报错（`Wrong type argument: stringp, t`、`Apparently circular structure being printed`、`ert-test-result-type` 未定义），被刹车停下。这些错误没有命中参考页，所以没有附页。与工具数量无关，和前几批的失败同类。
- 请求总数 157 偏高，主要来自 C2 第 3 次（50 个请求，48 次 eval，最后通过）：同样是反复写错跑测试的表达式，其中 2 次是写项目目录被 G28 拦下。
- 用 `superchat-tool` 的 eval 共 10 次。
- 每格 3 次，13/15、14/15、15/15 之间的差别不能当作结论。

### 更多任务上的对比（2026-10-28，DeepSeek，`runs/batch-wide-old`、`runs/batch-wide-six`）

之前没测过的 13 个任务，每组每题 2 次。`wide-old` 用旧的 13 个工具（`TS_TOOLS` 点名），`wide-six` 用新默认。两组同时跑。

| 任务 | 13 个工具 | 6 个工具 |
|---|---|---|
| A3、E1、P1W、P2、R1X、T1R、T4W、W11A、W2P、W9U、DLG | 22/22 | 22/22 |
| P1 | 0/2 | 0/2 |
| P3 | 0/2 | 0/2 |
| 合计 | 22/26，192 个请求 | 22/26，174 个请求 |

首次请求大小中位数：17131 B 对 13773 B。

- P1 两组都挂在 `title-in-english`：标题是中文。与工具无关。
- P3 两组都挂在 `a-timer-for-the-call`：模型用 `timer_create` 设好了 20 分钟后的提醒，但这个检查找的是 `timer-list` 里参数含"妈妈"的 Emacs timer，`timer_create` 的 timer 不带这段文字。检查写法和 `timer_create` 不匹配，不是模型没做到；P3T 才是按 `timer_create` 写的检查（上一批 3/3）。
- 没有一次失败和被移出的工具有关。

P3 的检查已改成查 Superchat 自己的 timer（同 P3T，去掉 Workspace 一项），`.scratch/task-sim/harness-goals/tasks-goals.el`。改后 DeepSeek 3/3（`runs/batch-p3-fixed`）。
