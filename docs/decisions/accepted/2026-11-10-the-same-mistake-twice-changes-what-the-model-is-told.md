# Decision: 同一种错犯第二次，superchat 换一句话告诉模型

Status: accepted — 用户 2026-11-10 说"在 telega 这个案例里，最大的问题是，积累了三次错误之后，没能形成自我纠正的循环"，并同意单独设计一个方案；方案写成后，用户同日对它说"同意建议"，同意实现。下面 Problem 到"没有证实的"是当时的方案原文，只把标题和这一行改了；实现见文末"实现"一节。实现里有几处是执行方自己定的（列在"实现时定的"），用户还没有确认，其中最要紧的是：给模型的那几句话的英文原文、"第二次"的说明走的是哪条路、相近名字怎么算"相近"。

## Problem

**你这边发生了什么：** 2026-11-09 那次导出聊天记录的对话里，模型把同几种错一犯再犯。你的 11 条后续消息里，有 5 条是在让它回顾自己的工具错误。纠正是你手动做的。

**存档里的数字**（会话 `20261109-005550-3bd3`，只读查的副本）：

- 223 次工具调用，33 次失败，全是 `eval_elisp`。
- 失败后的下一次调用：27 次成功，6 次又失败。
- 把中间的成功跳过，下一次失败是同一种错的有 9 次（共 32 次）。
- "Expected exactly one expression"和"does not read as Lisp"各出现 6 次。
- 失败之后去查资料的只有 2 次。三个不存在的函数名都是没查就调的。
- 单次执行里连续失败最多 2 次，所以"连续三次失败就停"的刹车一次都没触发。

所以"积累三次错误"在这次对话里的实际样子是：同一种错隔着成功的调用反复回来。

**为什么没有纠正：**

1. **不按种类记错误。** 唯一的计数是连续失败次数，一次成功就清零（`superchat-runtime.el` 的 `superchat-runtime-note-tool-outcome`）。每次失败都在 `tool.failed` 上记了 `error-type`，没有代码读它。
2. **失败后的话对哪种错都一样。** `superchat-llm.el:2620`："Correct the arguments of the failed call and retry it." 函数不存在时不提示去查；超时的时候，它和工具自己说的"不要原样再跑"相反。
3. **刹车是终止，不是纠正。** 它触发后模型不再收到请求。下一轮历史里只有一行 `[Stopped after 3 failed calls in a row]`，以助手自己说的话的形式出现，没有错误种类，没有建议。会话 `20261106-034208-05ce` 连着三次执行都被刹停在同一种错上，每次用户发新消息后同样的错立刻又出现。
4. **纠正的话不存档。** 每轮的反馈只进内存里的请求，后面的回合看不到，事后也查不到发过什么。
5. **没有东西让模型先查再调。** 只有系统提示里一句静态的话。

已有而没起作用的：Experience 的恢复功能即使打开也不会返回东西，它依赖的数据在 M3a 之后没人写了。工具循环里有一个能改写失败反馈的挂钩（`superchat-tool-execution.el:440`），只装了刹车。

## Decision（提议）

1. **按种类记失败。** 在一次执行里，superchat 给每种错计数，成功的调用不清零。种类取 `tool.failed` 上已有的 `error-type`；函数或变量不存在时再加上那个名字；笼统的 `error` 再按报错文字的开头分。

2. **同一种错第二次出现时，换一句话。** 不再是"改一下参数再试"，而是说明这是这次执行里第几次犯同一种错，并给出针对这一种错的做法：

   | 种类 | 告诉模型什么 |
   |---|---|
   | 函数或变量不存在 | 这个名字不存在；下一次调用先确认名字存在，再用它 |
   | 文字读不成 Lisp | 括号不配对；长代码写进 buffer，用 `edit_buffer`，它会检查括号 |
   | 夹着不是列表的表达式 | 是哪一个表达式；一次只交列表 |
   | 类型不对 | 先看一眼那个值是什么，再操作它 |
   | 超时 | 不要原样再跑；缩小范围或分批 |

   这张表的措辞是提议，要用真实模型试过再定。

3. **名字不存在时，superchat 自己查。** 第一次就查，不等第二次：报错后面附上名字相近的、确实存在的函数或变量（最多几个）。这一步不靠模型想起来去查。

4. **第一次失败的话也按种类说。** 超时不再附"改参数再试"。

5. **刹车照旧在连续三次失败时停。** 但停的时候，在 `run.failed` 上记下这次执行里每种错各几次。

6. **被刹停或失败的执行，下一轮请求里带一段 superchat 写的说明。** 写明上一次执行为什么停、犯了哪几种错各几次。它作为 superchat 交给模型的材料出现，不冒充助手说过的话。

7. **换过的话记在 Tape 上。** 这样后面的回合看得到，事后也能查到模型当时被告知了什么。

不做的：

- 不恢复 Experience。它是另一套机制，M3a 已经搁置。
- 不拦调用。superchat 不在求值之前检查代码里的名字：宏、局部函数、运行时才定义的名字都会让这种检查出错。
- 跨对话的记忆（"这个模型总是猜 telega 的函数名"）不在本条里。

## 和别的改动的关系

- [几个列表一次交上来就按顺序执行](../accepted/2026-11-09-several-forms-in-one-eval-run-in-order.md)已经去掉了"Expected exactly one expression"的大部分来源，并让剩下的报错指明是哪个表达式。那 6 次里有一次其实是多了一个右括号，旧的报错却建议"用 progn 包起来"。
- [空 buffer 用 edit_buffer 整段写入](../accepted/2026-11-09-edit-buffer-writes-an-empty-buffer.md)给了"长代码写进 buffer"这条建议一个可用的工具。
- workspace 的定义是另一件事，见 [同一天的另一份提案](../accepted/2026-11-10-a-workspace-is-a-record-not-a-directory.md)。

## 怎么验证

**脚本化的假模型能证明的：** 计数不被成功清零；第二次同种失败时模型收到的是另一句话；名字不存在时报错后面附着相近的名字；刹停后下一轮请求里有那段说明，且不是助手的话；这些都记在 Tape 上。

**假模型证明不了的：** 模型看到这些话之后会不会真的改做法。这要接真实模型，拿同一个任务在改动前后各跑若干次，比较同种错重复的次数。这一步要花 API 额度，由用户决定做不做。

## 没有证实的

- 上面的数字是执行方从存档副本里数的，没有第二个人重算。
- 那次对话里发出去的请求原文查不到：223 条消息清单都是空的。所以"模型当时到底被告知了什么"是从代码推出来的。
- 有 4 次"读不成 Lisp"的原因查不到，存档把参数截在 2000 字。
- 换一句话是否有效，没有任何数据。

## 实现（2026-11-10）

一句话：同一次执行里同一种错第二次出现时，模型收到的不再是"改一下参数再试"，而是一段 superchat 写的、只针对这种错的话；被刹停的执行，下一轮请求开头先有一段 superchat 写的说明。

举个例子：模型把一段 1700 字、写在一行里、少一个右括号的表达式发了第二次。以前它第二次收到的和第一次一模一样（"读不成 Lisp……第 1 行第 0 列的括号没关上"，外加"改一下参数再试"）。现在第二次之后，下一次请求里，工具结果后面那句话变成"这种错在这次执行里已经犯过，不要小改一下再发，照 superchat 的说明做"，接着是一段 superchat 的说明：这已经是第 2 个读不成 Lisp 的表达式；表达式写在一行里时，"第 1 行"这种位置说明不了什么；换个做法——拆成几次短的调用，或者用 `edit_buffer` 写进一个 Lisp buffer，它的括号检查会指出是哪一行没配对，再执行那个 buffer。

### 做了什么

1. **按种类记失败**（提议第 1 条）。`superchat-tool-execution.el` 的 `superchat-tool-failure-kind` 从 `tool.failed` 已有的 `:error-type` 和报错文字算出种类：`void-function`、`void-variable` 后面加名字（`void-function sc-foo`）；笼统的 `error` 后面加报错开头到第一个句号、冒号或分号为止（`error Expected exactly one expression`）；其它就是 `:error-type` 本身。`superchat-tool-execution--record-failure` 把种类交给 `superchat-runtime-note-tool-outcome`。Runtime 在执行的记录上存 `:tool-failure-kinds`（每种几次，按第一次出现的顺序），成功的调用不清零；原来的"连续失败次数"照旧。

2. **第二次起换一句话**（第 2 条）。某一种错的次数到 2 或以上时，`superchat-runtime-note-tool-outcome` 用 `superchat-runtime-steer` 给这次执行排一条说明，来源记作 `self-correction`，下一次请求带上它——这和"收尾检查"那条说明（`finish-check`）走的是同一条路。同时，工具结果后面那条反馈里，"Correct the arguments of the failed call and retry it. Once it succeeds, continue the user's original request." 换成 Runtime 给的句子（llm 端用 `runtime-observer` 问 `tool-failure-advice`，由 `superchat-runtime--tool-failure-advice` 回答；`superchat--llm-tool-error-feedback` 多了参数 RETRY）。被用户或策略拒绝的调用仍说"不要重试"，不受影响。说明的原文（`superchat-runtime--failure-correction` 和 `superchat-runtime--failure-advice`，N 是这次执行里这一种错的次数）：

   - 反馈里那一句：`The same kind of failure happened before in this execution: do not send the failed call again with small edits.  Follow the Superchat note in this request, then continue the user's original request.`
   - 函数不存在：`` Superchat: `NAME' is not a defined function, and this execution has now called it N times.  Do not call it again.  Before you use a name, check in a call of its own that it exists: (fboundp 'NAME) answers t only for a defined name; the error lists defined names close to it. ``（变量不存在时是 `is not a defined variable ... used it N times.  Do not use it again.`，`boundp`。）
   - 读不成 Lisp：`Superchat: N expressions in this execution did not read as Lisp, so none of them ran.  Sending such an expression again with small edits fails the same way, and a position such as line 1 tells little when the whole expression is on one line.  Do it another way: split the work into several short eval_elisp calls, or write the code into an emacs-lisp-mode buffer with edit_buffer (an empty buffer takes the whole text when old_string is empty), whose Lisp check names the line of a parenthesis that does not match, and then evaluate that buffer.`
   - 夹着不是列表的表达式（`error Expected exactly one expression`）：`Superchat: the same kind of failure (error Expected exactly one expression) has now happened N times in this execution.  eval_elisp runs several forms only when each one is a list, and the error names the form that is not.  Send only lists: remove that symbol, number or string, or put it inside a list.`
   - 类型不对（`wrong-type-argument`）：`Superchat: the same kind of failure (wrong-type-argument) has now happened N times in this execution.  Before you operate on a value, look at it in a call of its own -- its printed form, or (type-of VALUE) -- and write the next call for what it is.`
   - 超时（`time-limit`）：`Superchat: N calls in this execution were stopped at the time limit.  Do not run such a call again as it was: make the work smaller -- fewer items, a smaller region, one step at a time -- or split it into batches; run long work in a process.`
   - 其它种类：`Superchat: the same kind of failure (KIND) has now happened N times in this execution.  Sending the call again with small edits fails the same way.  Read the error, check the assumption it contradicts in a separate small call, and then do it another way.`（笼统的 `error` 其它开头，说明后半是 `Read the error, check the assumption it contradicts in a separate small call, and then do it another way.`）

   提议里说这些措辞"要用真实模型试过再定"。现在的原文是执行方写的，没有用真实模型试过。

3. **名字不存在时 superchat 自己查**（第 3 条）。`eval_elisp` 报 `void-function` 或 `void-variable` 时（`superchat-tools--eval-error-result`），第一次就在报错后面加一行（`superchat-tools--close-names-text`）：`` Defined function names close to `NAME' (looked up by Superchat): `A', `B'.  Check that a name exists before you use it: (fboundp 'NAME). ``；一个都没找到时是 `` No defined function name is close to `NAME' (looked up by Superchat); the library that would define it may not be loaded. ``（变量同理，`boundp`）。"相近"的算法见下面"实现时定的"第 4 条。这一行是工具自己的报错文字，所以 `tool.failed` 上、后面回合的历史里都有。

   - **2026-11-10 补：变量当函数调、函数当变量用。** 用真实模型跑这个版本时，模型把变量当函数调了，相近名字那一行却把它引向不相干的函数。两个实例：`` Symbol's function definition is void: telega-chatbuf--chat ``，后面附的是 `` `telega-chat-mode' ``，而 `telega-chatbuf--chat` 是一个变量；`` Symbol's function definition is void: buffer-read-only ``，后面附的是 `` `Buffer-menu-toggle-read-only' ``、`` `Buffer-menu-toggle-files-only' ``，`buffer-read-only` 也是变量。现在，`void-function` 的名字是变量时（`boundp`、`default-boundp` 或 `special-variable-p`，`superchat-tools-variable-not-function-p`），不再列相近的函数名，而是说：`` `NAME' is a variable, not a function (looked up by Superchat): use its value -- write NAME without parentheses around it as the head of a form -- or (symbol-value 'NAME), or (buffer-local-value 'NAME BUFFER) for its value in another buffer. ``；反过来，`void-variable` 的名字是函数时：`` `NAME' is a function, not a variable (looked up by Superchat): call it as (NAME ...), or refer to it as #'NAME. ``。第二次的说明也跟着变（`superchat-runtime--name-is-p`）：`` Superchat: `NAME' is a variable, not a function, and this execution has now called it as a function N times.  It is a variable: use its value -- … `` 和 `` Superchat: `NAME' is a function, not a variable, and this execution has now used it as a variable N times.  It is a function: call it as (NAME ...), or refer to it as #'NAME. ``；被刹停后下一轮那段说明里这两种错的做法用同样的话。只看全局（和默认值）是否绑定，不看表达式执行时所在 buffer 的局部绑定。

4. **第一次失败也按种类说**（第 4 条）。只做了超时：一轮里有调用停在时间上限，反馈那一句换成 `A call stopped at the time limit must not be run again as it was: make it smaller or split it.  Correct any other failed call and retry it.  Once it succeeds, continue the user's original request.` 其它种类第一次失败时的反馈不变（函数不存在时报错里多了相近的名字）。

5. **刹车照旧，`run.failed` 上记每种错几次**（第 5 条）。刹车的规则、默认值、`superchat-tool-loop-hooks` 的默认值都没改。以 `failed` 或 `timed-out` 结束、又有失败调用的执行，结束记录上多一个 `:tool-failures`，例如 `((:kind "unreadable-expression" :count 3))`。

6. **下一轮带一段 superchat 写的说明**（第 6 条）。同样这种执行，在这一轮的 `turn.completed` 之后、下一轮开始之前，Tape 上追加一行 `system` 材料（topic `tool-failures`，`superchat-runtime--record-failure-summary`）。下一轮的历史把它作为 `[System note]` 开头的用户消息发出，不是助手说的话。原文：`Written by Superchat, not by the assistant: the previous execution was stopped by Superchat (Stopped after 3 failed calls in a row).  Its failed tool calls, by kind: unreadable-expression, 3 times.  Before calling tools again for this task:`，后面每种错一行 `- KIND: ` 加上第 2 条里那种错的做法（最多列 5 种）。不是刹车停的、而是以失败结束的执行，开头是 `the previous execution ended as failed.`。重新打开对话时，这一行显示为 `** System`。

7. **记在 Tape 上**（第 7 条）。第 2 条的说明排进去时记 `run.steered`（`:origin "self-correction"`，全文在 `:instruction`），下一次请求前作为材料记 `attachment.added`（`:source` 带 `:origin`），后面回合的历史里是 `[Injected material] Superchat: …`；第 6 条的说明是上面那一行 `system` 材料；第 3 条的名字在 `tool.failed` 的结果文字里。反馈那一句（第 2、4 条里替换掉的那句）和以前一样只在内存里的请求中，不记 Tape。

### 实现时定的（用户还没有确认）

1. **说明走"收尾检查"那条路，而不是改写工具结果。** 提议提到工具循环里能改写失败反馈的挂钩（`superchat-tool-loop-hooks`）。没用它：改写记成 `tool.rewritten`，后面回合的历史仍用原来的结果（组件说明写明了这一点），提议第 7 条"后面的回合看得到"就做不到；而且要改挂钩的默认值。排一条带来源的 steer，现成的代码就会把它记上 Tape、送进下一次请求、留在以后回合的历史里。代价：说明是工具结果之后单独的一条消息，不在报错文字里；在同一次执行里它就是一条用户消息，以 `Superchat:` 开头。
2. **反馈那一句怎么换。** 由 Runtime 回答一个新问题 `tool-failure-advice`：这一轮排了 `self-correction` 说明时换成"照 superchat 的说明做"那一句，否则这一轮有超时就换成超时那一句，否则不换。llm 以外没有 Runtime 的调用（同步循环、子代理）照旧。
3. **触发刹车的那一次失败不排说明。** 第三次连续失败会让执行停下，排了说明也送不出去，只会多一条 `run.steer-undelivered`。只在刹车挂钩在 `superchat-tool-loop-hooks` 里、并且 `superchat-tool-failure-brake` 开着时这样判断。
4. **"相近"怎么算。** 名字按 `-`、`/`、`:` 分成词。名字有两个词以上时，候选必须第一个词相同（同一个包），并且其余的词里至少有一半（至少 1 个）也出现在候选里；名字只有一个词时，按拼写比，`string-distance` 不超过 2。排序：共有的词多的在前，再按拼写距离。最多 5 个（`superchat-tools-close-names-max`），遍历所有符号最多 0.2 秒（`superchat-tools-close-names-seconds`），到时就用已经找到的。只对 `eval_elisp` 的报错做；别的工具内部的 `void-function` 是工具自己的毛病，不是模型猜的名字。
5. **函数名不同就是不同的种类。** 照提议第 1 条，`void-function` 的种类带名字，所以猜了两个不同的错名字，各算一次，不会触发第二次的说明；这种情况靠第 3 条的相近名字。
6. **第一次失败按种类说，只做了超时。** 其它种类第一次的反馈照旧。
7. **以失败或超时结束的执行都留说明，不只是刹停的。** 提议第 6 条写的是"被刹停或失败的执行"；`timed-out` 也算进去。被用户取消的不留。
8. **同一轮里同一种错出现几次，每次各排一条说明**（第 2 次、第 3 次……），没有合并。
9. **原来那行 `[Stopped after 3 failed calls in a row]` 仍是这次执行的回答**，下一轮历史里仍以助手的话出现；新的说明在它后面。
10. **不加选项。** 没有新的用户选项；关掉刹车（`superchat-tool-failure-brake` 设 nil）不会关掉说明。

### 验证

- **新测试** `scripts/self-correction-regression.el`（`"^self-correction/"`），15 个，全部经过真实的工具循环，模型是按脚本回答的假模型（`smoke--with-fixture`）。按提议"怎么验证"里假模型能证明的五件事：同一种错隔着成功的调用出现三次（失败、成功、成功、失败、七次成功、失败），按种类数到 3，连续失败从没到 2；第二次之后的请求里有针对那种错的说明，反馈那一句换掉了，第一次之后的请求照旧；2026-11-09 那种一行、少一个右括号的表达式发第二次时，说明指向拆小和 `edit_buffer`；同一个不存在的函数名调第二次；不存在的函数、变量第一次就附上相近的名字，没有相近的也说明，查找有上限；超时第一次就不说"改参数再试"；刹停的执行在 `run.failed` 上记 `:tool-failures`，Tape 上有一行 `system` 说明，排在这一轮 `turn.completed` 之后，下一轮请求里它以 `[System note]` 出现在新消息之前；说明记在 `run.steered` 和 `attachment.added` 上，下一轮的历史里有。另有两个守护测试：两个不同的错名字不算同一种错；最后成功了的执行不留说明。
- **在改之前的代码上跑**（加载路径指向改之前的副本，测试文件用改之后的）：15 个里 13 个失败，2 个守护测试前后都通过。
- **改了断言的已有测试：** 没有。没有已有测试断言反馈那一句在第二次同种失败后仍出现；`tool-nudge/the-soft-sentence-is-gone` 检查请求里没有 "Change approach"，新说明没有用这几个词。
- **用到改动过的函数、变量、文字的脚本和指定的几组**，改前改后各跑一遍，数字相同：`tool-hooks` 187/187，`tool-nudge` 28/28，`m5-tool-feedback` 33/33，`eval-failure` 13/13，`braked-round` 177/177，`edit-all` 30/30，`empty-buffer` 10/10，`eval-code-mode` 32/32，`work-resume` 179/179，`compile-watch` 405/405，`m7-buffer-first` 22/22，`timer-command` 219/219，`m5-execution` 62/62，`tool-error-type` 192/192，`wire-safe` 21/21，`context-recovery` 7/7，`several-forms` 6/6，`history-messages` 23/23，`request-evidence` 10/10，`round-brake` 172/172，`tool-quit` 5/5，`finish-check` 16/16，`m5-response` 11/11（这些数字含脚本加载进来的别的脚本的测试）。M3 只跑不改：139 项，137 过，2 跳过，和改之前相同。`m5-regression` 只跑不改：11 项里 3 项不过，改之前是同样的 3 项（`m5/discovery/…`，见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。
- **整套回归：** `scripts/` 下全部 `*-regression.el`，两边各跑一遍，同时最多 3 个 Emacs。改之前 140 个脚本 9506 项，9500 符合预期、3 项不过（就是 m5 那 3 项）；改之后 141 个脚本 9534 项，9528 符合预期、3 项不过（同样那 3 项）。原有的 140 个脚本，每个的测试数、通过数都和改之前一样；多出来的 28 项是新脚本（它自己的 15 项，加上它加载的 `eval-failure` 的 13 项）。这两遍没有偶发失败，不需要单独重跑。
- **Kernel check：** 两边都是 `KERNEL-CHECK OK (0 fails)`。**冒烟：** 两边都是 `SMOKE OK (0 mismatches)`，`scripts/smoke-expected/` 没有任何改动。
- **2026-11-10 补的那一处（变量当函数、函数当变量）：** 新脚本加了 4 个测试，两个方向各两个（第一次的报错文字、第二次的说明；变量那个也试了 `buffer-read-only`），共 19 个。改之后 19/19；在补之前的版本上这 4 个失败、其余 15 个通过；在改之前的代码上 19 个里 17 个失败、2 个守护测试通过。重跑：`eval-failure` 13/13，`m5-tool-feedback` 33/33，`tool-error-type` 192/192，`several-forms` 6/6，M3 139 项、137 过、2 跳过（文件没动），Kernel check `KERNEL-CHECK OK (0 fails)`，冒烟 `SMOKE OK (0 mismatches)`、基准没变；`superchat-tools.el` 和 `superchat-runtime.el` 的字节编译警告和改之前相同。整套回归没有为这一处重跑。
- **字节编译**改过的四个文件（`superchat-tool-execution.el`、`superchat-runtime.el`、`superchat-llm.el`、`superchat-tools.el`）：两边各 17 条警告，除行号外完全相同，没有新警告。

### 没有验证的

- **没有接真实模型。** 模型看到这些话之后会不会换做法、同一种错重复的次数会不会减少，都没有测。这要拿同一个任务在改动前后各跑若干次，要花 API 额度，由用户决定。
- 说明的英文原文是执行方写的，提议说要用真实模型试过再定，还没有试。
- 没有在用户自己的 Emacs 里试过，重新打开对话时 `** System` 那一行的样子只从代码推出，没有看过。
- "相近名字"的算法只在测试里定义的几个函数名上试过，没有在 telega 真实的函数表上试过；0.2 秒的上限在一个装了很多包的 Emacs 里够不够，没有测。
