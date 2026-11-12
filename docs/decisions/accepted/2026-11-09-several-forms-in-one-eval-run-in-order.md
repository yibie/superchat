# Decision: 一次 eval_elisp 里连着写几个列表，就按顺序都执行

Status: accepted — 用户 2026-11-09 对评估里的七条建议说"同意建议"。本条是其中第 5 条的做法。建议原文是"让 `edit_buffer` 能写入整段新内容，多表达式自动包 `progn`。"；这里只在每一个都是列表时才按顺序执行，有一个不是列表就整段不执行。这一处和用户读到的建议不完全一样，2026-11-09 已经告诉用户，用户还没有回答。

## Problem

`eval_elisp` 是模型在用户的 Emacs 里执行一段 Lisp 的工具。它只收一个表达式。模型写了两个，比如 `(setq a 1) (setq b 2)`，什么都不执行，直接失败：`Expected exactly one expression; wrap multiple forms in progn`（要它把几个表达式包进一个 `progn`）。

2026-11-09 的对话 `20261109-005550-3bd3`（DeepSeek）里，206 次 `eval_elisp` 有 6 次这样失败。每一次都多来回一趟；每一次也算一次失败，而连着失败三次，这次执行就会被"刹车"停下（`superchat-tool-failure-brake`）。工具说明和 Emacs 操作说明都写着"只收一个表达式，几个就包进 progn"，[操作说明那条记录](../implemented/2026-09-20-emacs-operating-note.md)也把"`eval_elisp` 一次传了多个表达式"列为观察到的错误；模型还是会犯。2026-10-11 的一次执行里同样的拒绝让模型重写了 5 次（`reports/guidance-2026-10-11/SLICE-9.md`），和 nodecode 的对比里有 15 次（[调研](../../research/nodecode.md)）。

## Decision

**几个表达式连着写，每一个都是列表（括号括起来的一段），就按顺序都执行，像包在一个 `progn` 里；有一个不是列表，就一个都不执行。**

- **先把整段读完，再执行。** 和以前一样。整段里任何地方读不成 Lisp，什么都不执行，用原来的话报错（`Error: the expression does not read as Lisp, so nothing was evaluated: …`）。
- **只有一个表达式：** 和以前一样。
- **几个，都是列表：** 按顺序执行，结果是最后一个的值。中间和末尾可以有空白和行注释。审批时问用户的问题和 Tape（记录每次执行发生了什么的数据库）上的 `tool.requested`，显示的都是模型发来的原文，不是包好的 `progn`。
- **几个，其中有不是列表的（一个符号、一个数、一个字符串）：** 一个都不执行，和以前一样失败，话里点出第一个不是列表的：`Expected exactly one expression; wrap multiple forms in progn.  Several forms run in order only when each is a list, and form 2 of 2 is not: done.  Nothing was evaluated`。这样，模型在表达式后面顺手写的一句话，比如 `(foo) done`，不会在 `(foo)` 已经执行之后才被发现。
- **说明文字不改。** 告诉模型"一个表达式，几个就包进 progn"的那些话都留着，那仍是应该写成的样子。

代码在 `superchat-tools.el` 的 `superchat-tools--read-one-expression`：读出每一个表达式，返回那一个、包好的 `progn`，或者报错；`superchat-tool-eval-elisp` 执行它返回的东西。组件说明见 [superchat-tools](../../components/superchat-tools.md#built-in-edit-and-evaluation-results)。

## 实现时定的

1. **读字符时的两个开关只包住"读"本身。** 第一版把 `read-circle`、`read-eval` 设成 nil 的范围放得太大，后面用到 `cl-every` 时自动加载 `cl-extra`，那个文件就读不进来（`invalid-read-syntax "#1="`）。现在只在 `read-from-string` 那一步关掉，和改之前一样。
2. **`'x` 算列表。** `'x` 读出来是 `(quote x)`，是列表，所以 `(foo) 'x` 会执行，结果是 `x`。它不会改任何东西。
3. **函数名不改。** 仍叫 `superchat-tools--read-one-expression`：它返回的仍是一个表达式，只是可能是包好的 `progn`。
4. **报错的话保留旧的开头。** 旧的 `Expected exactly one expression; wrap multiple forms in progn` 原样在前，后面加上是第几个、是什么。

（另外两条，`edit.applied` 记录和 buffer 不存在时的回答，属于 [edit_buffer 写空 buffer 那一条](2026-11-09-edit-buffer-writes-an-empty-buffer.md)。）

## 验证

- **新测试** `scripts/several-forms-regression.el`（`"^several-forms/"`），6 个，全部经过真实的工具循环，模型是按脚本回答的假模型：一个表达式照旧；几个列表中间夹着注释，按顺序执行，返回最后一个的值；审批问题里的参数和 Tape 上 `tool.requested` 的参数是原文，没有 `progn`；列表后面跟一个符号，一个都不执行，并点名；列表中间有字符串，点名；第二个表达式没写完，第一个也不执行。
- **在改之前的代码的一份副本上跑：** 6 个里 5 个失败，都失败在旧的那句拒绝；`one-form-is-as-before` 前后都通过，它检查的就是"照旧"。验收人复跑：改之后 6/6，改之前 5 个失败、1 个照旧。
- **改了断言的三个已有测试**（规则变了，旧断言说的是旧规则）：
  - `eval-failure/more-than-one-expression-is-a-failed-call` 改名为 `eval-failure/more-than-one-expression-fails-when-one-is-not-a-list`：输入从 `(+ 1 2) (+ 3 4)`（现在会执行）换成 `(+ 1 2) done`，仍断言失败、带 "Expected exactly one expression"。
  - `m5-tool-feedback/eval/extra-form-is-rejected-before-first-effect` 名字不变：多出来的那个从列表换成符号 `m5tf--side-effect`，仍断言报错、第一个表达式的效果没有发生。
  - `m5-tool-feedback/eval/extra-form-after-comment-is-rejected` 改名为 `…/extra-form-after-comment-runs-in-order`：断言从"报错、没有效果"换成结果 `Result: 2`；第二个表达式把第一个的值乘 2，所以顺序也证明了。
  - 验收人复跑这两个脚本：改之后 13/13、33/33。
- **其它检查：** 用到 `eval_elisp` 的回归脚本全部跑过，只有 `m5-regression` 那 3 个已知失败（改之前同样失败）；Kernel 检查 `KERNEL-CHECK OK (0 fails)`，冒烟 `SMOKE OK (0 mismatches)`，基准没有变化；字节编译警告和改之前相同。
- **合并后的整套回归（验收时跑的）：** 今天的几项改动和验收时补的修正全部合并后，在一份不含 `.git` 和编辑器锁文件的副本上，跑了 `scripts/` 下全部 139 个定义了测试的脚本（共 9423 项）、Kernel check 和标准冒烟集，同时最多 4 个 Emacs。只有 `m5-regression` 的 3 项不过，这 3 项改之前就不过（见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。和改之前逐个脚本对比：原有的每个脚本，测试数和通过数都一样，只有 `context-recovery` 多了 1 项新测试；新增 5 个脚本，全过。Kernel check：`KERNEL-CHECK OK (0 fails)`；冒烟：`SMOKE OK (0 mismatches)`，基准没有改；M3 只跑不改：139 项，137 过，2 跳过。改之前那一遍是在机器很忙时跑的，有 4 个脚本偶发失败或卡住（`answer`、`feed`、`note`、`timer-brief`）；同样的测试单独重跑、以及合并后这一遍，都通过。
- **没有验证的：** 没有用真实模型跑过，失败次数有没有因此减少没有测。

## Alternatives considered

**保留拒绝，靠操作说明那句话。** 那句话从 2026-09-20 起就在操作说明里，工具说明也这么说，这个错误也早已被列为观察到的错误（[记录](../implemented/2026-09-20-emacs-operating-note.md)）；2026-11-09 还是出现了 6 次。模型一再错过的规则，每次都要多来回一趟、向刹车走一步，而这段文字要做什么本来没有疑问。

**不管是不是列表，都包起来。** 这是按建议字面的做法。没选：`(foo) done` 或 `(save-some-buffers) That should do it.` 这样的文字，会先执行 `(foo)`，再在 `done` 上报"变量没有值"——发现那是一句话时，第一个表达式已经生效了。不是列表的东西放在一串里执行也没有用处，有一个就整段拒绝，不会丢掉模型想做的事。

**一个一个执行，把每个的值都返回。** nodecode 是这么做的，并说明是哪一个出错（[调研](../../research/nodecode.md)）。没选：它改了结果的样子 `Result: VALUE`，而模型、Tape 和测试都按这个样子读；没有这个必要——模型自己写的 `progn` 也只返回最后一个值，要几个值就返回一个列表。

## Consequences

- 几个列表连着写，一次调用就完成，不再是一次失败加一次重写，也不再让刹车往前走。
- 后面的表达式出错时，前面的已经生效，和模型自己写的 `progn` 一样；报错不说是第几个出的错。
- 表达式后面跟了一句话，仍然在执行之前就失败，现在还说出是第几个、是什么。
