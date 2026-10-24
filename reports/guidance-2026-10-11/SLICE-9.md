# 切片 9：工具调用打转时，产品要知道、要说、要刹

写于 2026-10-18，基于 HEAD `99cfa67c`。证据：用户真实 Tape 里 2026-10-18 07:27 与 07:40 两次执行（会话 20261018-002533-7aeb），各恰好 50 次工具调用被上限挡下。回放见本文末。

## 要解决的问题（一句话）

模型用 eval_elisp 调试自己的 Lisp 时会连错十几次、重复同一探针，产品对此一无所知：求值错误记成成功，撞上限的执行记成正常完成，用户在 auto 档下看不到任何打转的迹象。

## 裁定（用户 2026-10-18 已定："按照建议执行"）

- 不调上限。第二次执行 50 次零改动，100 次也是零。
- 做 9a–9e。9f（一表达式规则改自动 progn）和 9g（追踪 eval_elisp 改了哪些 buffer）**不做**，留给用户另行决定。

## 9a 求值错误是失败，不是成功的字符串 ★先做

- 今天：`superchat-tools.el:953` 把求值错误拼成 "Error: evaluating expression: …" 当结果返回；Tape 记 `tool.completed status=ok`。两次执行里 25 次报错全是 ok。
- 改：eval_elisp 的求值错误走工具失败路径（和被上限挡下的那条一样，`tool.failed`），模型收到的文本可以不变。"它试图向用户要输入"（:956）同样算失败。
- 不改：成功结果的形状；错误文本内容。
- 红测试：求值 `(car 1)` → Tape 里 `tool.failed`，没有 `tool.completed ok`；改动审阅的 Run 摘要把它算进失败数。
- 查：哪些现有测试断言 eval_elisp 报错时 status=ok，一一列出并改写；M3 只跑不改，若 M3 里有这类断言，带证据报 leader。

## 9b 撞上限写进结束事实，表上显示"被切断"

- 今天：`superchat-agent-loop.el:426/506` 挡下第 51 次并给模型一段文字；两次执行的 `run.completed` payload 只有 `status=completed`，没有 `:tool-budget`。Runtime 已有 `:tool-budget` 字段（`superchat-runtime.el:383, 2696, 2732`）和 `superchat-work.el:1439` 这一处读者，但这条路径没写它。
- 改：agent-loop 挡下调用时把 `:tool-budget (:action cut-off :limit 50 :used 50)` 放进 Runtime entry，让 `run.completed` / `response.completed` 带上；待办表和通知把这种结束显示成 "Cut off after 50 tool calls"，不是 "done"。
- 红测试：上限 3 的执行第 4 次调用被挡 → `run.completed` 带 `:tool-budget`；`superchat-pending-items` 里那行文字含 "cut off"；工作记录路径（:1439）照旧。

## 9c 连续失败刹车

- 依赖 9a。连续 N 次 `tool.failed`（默认 3，`defcustom`，nil 关）→ 下一请求前插一条系统消息："The last 3 tool calls failed. Change approach: test on a small sample, write the code to a scratch buffer first, or ask the user."
- 每次执行只插一次；计数在任一成功调用后归零。审批被拒不算失败（那是用户决定）。
- 红测试：3 次失败后请求消息列表里有这句；第 4 次失败不再插；中间一次成功后重新计数。

## 9d 预算提醒与可见计数

- 依赖 9b。用掉 70%（50 时为 35）时插一条系统消息："15 tool calls left in this execution. Finish, or stop and report what is done and what is not."只插一次。
- 状态行/模式行在执行中显示 `tools 37/50`。读 Runtime entry，不另存状态。
- 红测试：第 35 次后消息出现；计数文字随每次调用更新（批处理里读 `superchat-runtime-status` 对应字段即可）。

## 9e edit_buffer 加正则、全部替换、只数不改

- 今天 edit_buffer 只替换一处字面文本（`superchat-tools.el:546` 附近 "exactly one replacement"）。第一次执行里它一直在工具面上，模型 0 次使用，用 eval_elisp 写了 40 次正则。
- 改：参数加 `regexp`（布尔）、`all`（布尔）、`count_only`（布尔）。`count_only` 返回匹配数和前 3 处的行号与原文，不改 buffer。`all` 必须先在同一执行里 `count_only` 过同一 pattern 才允许，否则拒绝并说明（防止盲改）。
- 改动记录：全部替换记一次 `edit.applied`，带替换数。改动审阅和 Unsaved 行照旧。
- 不动：默认行为（不传新参数时和今天一模一样）；工具说明加两句，长度控制在 +300 字内。
- 红测试：count_only 不改 buffer、返回数对；all 未 count 先拒绝；all 替换 199 处后 `edit.applied` 带 199；非 regexp 路径原样。
- 冒烟基线若因工具说明变化而变，停下等用户批准。

## 停点

9a 完成（红→绿、现有断言清单）停一次；9b–9e 做完一起停；全量检查干净后停等提交批准。每个提交只含自己的文件。

## 回放摘要（Tape，UTC）

- 执行 1，07:27–07:35：40 eval_elisp + 10 read_buffer，8 次 Error，2 次真正改动（189+4 处）。5 次因 "Expected exactly one expression" 重写（两次执行合计）。
- 执行 2，07:40–07:52：46 eval_elisp，17 次 Error，0 次改动；下载 646 KB 到 /tmp，留两个隐藏 buffer。
- 两次 `run.completed status=completed`，无 `:tool-budget`；记录行 "no buffer or file was changed by a tool; ran Emacs Lisp 50 times (what it changed is not tracked)"。
