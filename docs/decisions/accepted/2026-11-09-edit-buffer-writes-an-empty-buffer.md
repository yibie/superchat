# Decision: 空 buffer 可以用 edit_buffer 一次写进全部内容

Status: accepted — 用户 2026-11-09 对评估里的七条建议说"同意建议"。本条是其中第 5 条的做法。建议原文是"让 `edit_buffer` 能写入整段新内容，多表达式自动包 `progn`。"；这里只在 buffer 是空的时候整段写入，不加新工具，也不加新参数。这一处和用户读到的建议不完全一样，2026-11-09 已经告诉用户，用户还没有回答。

## Problem

`edit_buffer` 是模型改 buffer 用的工具：给它一段 buffer 里原有的文字（`old_string`）和要换成的文字（`new_string`），它找到那一处，换掉。原有的文字不能是空的，所以一个空 buffer——比如刚打开、磁盘上还没有的新文件——没法用它写进第一行。

2026-11-09 的对话 `20261109-005550-3bd3`（DeepSeek）里就是这样。模型手上有 `read_buffer`、`read_file`、`edit_buffer`、`eval_elisp`、`work_record`、`timer_create`、`ask_user` 七个工具，一共调用了 223 次：`eval_elisp` 206 次，`read_file` 9 次，`read_buffer` 8 次，`edit_buffer` 0 次。任务里要写一个约 20 KB 的 Emacs Lisp 文件。模型只能用 `eval_elisp`（在用户的 Emacs 里执行一段 Lisp）把整段文字插进去：文字先写成 Lisp 字符串，再放进工具调用的 JSON 字符串里，引号和反斜杠转义了两层；`eval_elisp` 插进去的东西也不经过 [2026-09-19 的括号检查](../implemented/2026-09-19-a-broken-lisp-edit-is-sent-back.md)（结构化改 buffer 的工具在动手前先检查括号是否配对，`eval_elisp` 不在其中）。最后留下的 buffer 读不成 Lisp。

能往空 buffer 里写的 `append_to_buffer`、`replace_buffer` 不在默认工具里：用户 2026-10-26/27 把默认工具减到了六个（[eval_elisp 里可以调用别的工具](2026-10-26-eval-elisp-calls-tools.md)），并希望工具少。

## Decision

**空 buffer 里，`old_string` 给空字符串，`new_string` 就成为整个 buffer 的内容。** 不加工具，不加参数，`edit_buffer` 多认一种情况。

- **其它一切和普通的一处替换一样。** 权限级别把它当作一次修改来判断：`read-only` 级别下拒绝，`edit-buffers` 级别下不问。buffer 打上"有未保存改动"的标记，撤销一次就回到空的。Tape（记录每次执行发生了什么的数据库）上这次调用的 `tool.completed` 记下改了哪个 buffer，所以 `/changes` 和审阅界面会列出它。回答末尾照样有目标和层次两行，说明工具不保存、不求值。和一处替换一样、和 `all` 不同，它不写 `edit.applied` 这条记录。
- **回答说写了什么，并把内容给模型看。** 例如 `Wrote 300 lines, 7770 characters, into the empty buffer export.el. This tool does not save or evaluate the buffer.`，后面是 `Now, lines 1-300:`，前 6 行和后 6 行带行号，中间说省了多少行；和改一处时显示改动后的行是同一种写法。前后不再多加一行（本来就没有），末尾换行后的空行不显示。
- **括号检查照旧。** 空 buffer 算"读得通"，所以括号不配对的文字不会写进一个空的 Lisp buffer：调用失败，用的是那条检查原来的话（`Error: edit not applied; buffer export.el was not changed. It reads as Lisp now, and after this edit it would not: …`），Tape 上的错误类型是 `lisp-unbalanced`，buffer 仍然是空的、没有改动标记。
- **空 buffer、空 `old_string`、空 `new_string`：** `No change: buffer B is empty and new_string is empty.`
- **buffer 里有字，`old_string` 却是空的：** 仍然报错，错误类型仍是 `empty-old-string`，但话里说清为什么、怎么办：`Error: old_string must be nonempty here: it may be empty only when the buffer is empty, and buffer B has N characters. Read it and give text that occurs once. To add text at the end, replace the last line with that line followed by the new text.`
- **带 `regexp`、`all` 或 `count_only` 时，** 空 `old_string` 和以前一样报错，空 buffer 也一样。
- **说明文字只改两处。** 工具说明里 `Empty, missing or ambiguous old_string fails;` 改成 `Missing or ambiguous old_string fails; an empty old_string only writes new_string into an empty buffer;`（多 58 个字符）；`old_string` 参数的说明从 `Nonempty text copied …` 改成 `Text copied … exactly once. Empty only for an empty buffer, to write new_string as its whole text.`（多 62 个字符）。系统提示和 Emacs 操作说明不动。
- **在 `eval_elisp` 里嵌套调用**，`(superchat-tool "edit_buffer" :buffer_name B :old_string "" :new_string TEXT)`，结果相同。

"空"指模型能改到的那部分文字（accessible text）是空的：一个收窄（narrowing）到空范围的 buffer 也算空，文字写在那个位置；括号检查仍然看整个 buffer。

代码在 `superchat-tools.el`：`superchat-tools--edit-buffer-plan` 判断空的情况，`superchat-tools--edit-buffer-apply` 写入，`superchat-tools--edit-shown-lines` 多了参数 WHOLE 用来显示整段内容。组件说明见 [superchat-tools](../../components/superchat-tools.md#built-in-edit-and-evaluation-results)。

## 实现时定的

1. **`edit.applied` 这条记录。** 任务里写的是"和别的 `edit_buffer` 修改一样的 `edit.applied`"，但代码里普通的一处替换并不写 `edit.applied`，只有 `all` 写。所以空 buffer 的写入也不写，和一处替换一致；改了哪个 buffer 记在 `tool.completed` 上。测试检查的就是这一点。
2. **读字符时的两个开关只包住"读"本身。** 第一版把 `read-circle`、`read-eval` 设成 nil 的范围放得太大，后面用到 `cl-every` 时自动加载 `cl-extra`，那个文件就读不进来（`invalid-read-syntax "#1="`）。现在只在 `read-from-string` 那一步关掉，和改之前一样。（这一条属于[多个表达式那一条决定](2026-11-09-several-forms-in-one-eval-run-in-order.md)的代码，两条决定改的是同一个文件，一起记在这里。）
3. **buffer 不存在时。** 以前空 `old_string` 加一个不存在的 buffer 名，回答是"old_string 不能为空"；现在和非空 `old_string` 一向的做法一样，报 `No buffer named …`。
4. **`'x` 算列表。** `'x` 读出来是 `(quote x)`，是列表，所以 `(foo) 'x` 会执行，结果是 `x`。（同属多个表达式那一条，记在两边。）
5. **`open_file` 打开新文件时告诉模型怎么写。** `open_file` 打开一个磁盘上还没有的文件时，回答本来就说 "This is a new file: nothing exists on disk until the user saves the buffer."。现在 buffer 确实是空的时候，后面多一句 "The buffer is empty: edit_buffer with an empty old_string writes its whole text."——模型正要往里写的时候看到。buffer 不空（比如用户的 `auto-insert` 已经插了文件头）就不说这句。工具说明和系统提示都没改。
   - 任务原本说的是 `read_file`，但 `read_file` 对不存在的路径只回 `Error: … does not exist`，不打开 buffer；"This is a new file" 这句话属于 `open_file`（`superchat-tool-open-file`）。所以这句加在 `open_file` 的回答里。
   - `open_file` 2026-10-27 起不在默认工具里。默认工具下，模型按操作说明用 `eval_elisp` 执行 `(find-file-noselect F)` 打开文件，得到的只是 buffer 对象，看不到这句话。这条路上模型怎么知道空 buffer 能用 `edit_buffer` 写，靠的只是工具说明里新加的那半句。
   - 没有在真实模型上测过。

## 验证

- **新测试** `scripts/empty-buffer-regression.el`（`"^empty-buffer/"`），10 个，全部经过真实的工具循环，模型是按脚本回答的假模型：写入本身和它的记录、标记、撤销、回答；`read-only` 下被拒绝；括号不配对的文字写不进空的 Lisp buffer；空写进空；有字的 buffer 用空 `old_string` 时的话；带 `regexp`、`all`、`count_only` 时照旧；两处说明文字；`eval_elisp` 里嵌套调用；`a-new-file-opened-says-how-to-fill-it`（模型先用 `open_file` 打开一个不存在的文件，下一次请求里带着新加的那句话，接着用空 `old_string` 调 `edit_buffer`，buffer 里是那段文字、有改动标记，磁盘上没有文件）；以及这次事故的样子 `the-incident-a-new-lisp-file-in-one-call`：一个对应新 `.el` 文件的空 buffer，先收到少一个括号的 300 行，被拒绝，buffer 仍然是空的、没有改动标记；再收到配对的 300 行，一次写进去。
- **在改之前的代码的一份副本上跑：** 10 个里 9 个失败，都失败在"空 `old_string` 先被当成错误拒绝"（`read-only` 下也是，回答是那个错误而不是级别的拒绝）、说明文字是旧的、或者 `open_file` 的回答里没有那句话；`regexp-all-and-count-only-are-as-before` 前后都通过，它检查的就是"照旧"。验收人复跑：改之后 9/9（加 `open_file` 那个测试之前），改之前 8 个失败、1 个照旧。
- **改了断言的已有测试：** 本条没有。多个表达式那一条改了三个，见[那条记录](2026-11-09-several-forms-in-one-eval-run-in-order.md)。没有已有测试或冒烟基准断言 `open_file` 新文件那段话。
- **其它检查：** 用到 `edit_buffer`、`eval_elisp`、`read_file`、`open_file` 的回归脚本全部跑过，只有 `m5-regression` 那 3 个已知失败（改之前同样失败）；`answer/the-mode-line-says-a-question-waits` 在并行跑时失败过一次，单独在改后、改前各跑一次都通过。Kernel 检查 `KERNEL-CHECK OK (0 fails)`，冒烟 `SMOKE OK (0 mismatches)`，冒烟基准没有变化（基准里没有工具说明）。`superchat-tools.el` 字节编译的警告和改之前相同。
- **合并后的整套回归（验收时跑的）：** 今天的几项改动和验收时补的修正全部合并后，在一份不含 `.git` 和编辑器锁文件的副本上，跑了 `scripts/` 下全部 139 个定义了测试的脚本（共 9423 项）、Kernel check 和标准冒烟集，同时最多 4 个 Emacs。只有 `m5-regression` 的 3 项不过，这 3 项改之前就不过（见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。和改之前逐个脚本对比：原有的每个脚本，测试数和通过数都一样，只有 `context-recovery` 多了 1 项新测试；新增 5 个脚本，全过。Kernel check：`KERNEL-CHECK OK (0 fails)`；冒烟：`SMOKE OK (0 mismatches)`，基准没有改；M3 只跑不改：139 项，137 过，2 跳过。改之前那一遍是在机器很忙时跑的，有 4 个脚本偶发失败或卡住（`answer`、`feed`、`note`、`timer-brief`）；同样的测试单独重跑、以及合并后这一遍，都通过。
- **没有验证的：** 没有用真实模型跑过。模型现在会不会用 `edit_buffer` 写新文件，没有测。

## Alternatives considered

**把 `append_to_buffer` 或 `replace_buffer` 重新放回默认工具。** 它们今天就能写空 buffer。没选：用户 2026-10-26/27 把默认工具减少了，并实测过少的那一组（[记录](2026-10-26-eval-elisp-calls-tools.md)：六个工具时第一次请求比十三个工具小约 19%，没有一次失败和被移出的工具有关），2026-11-09 又说要工具少。多一个工具，每次请求都要带上它的说明；而这里的情况，`edit_buffer` 多认一种参数就够了。

**加一个新参数，比如 `mode`（`replace`、`append`、`write`）。** 没选，理由同上，只是小一些：每次请求都带着这个参数的说明，每次修改模型都多一个可能选错的地方；而"空 buffer 加空 `old_string`"本来就不会有别的意思，从已有的参数就认得出来。

**在 Emacs 操作说明里告诉模型用 `eval_elisp` 插入文字。** 2026-11-09 模型没人告诉也是这么做的，结果就是那个读不成 Lisp 的文件：文字是 JSON 字符串里的 Lisp 字符串，每个引号和反斜杠转义两次；`eval_elisp` 插进去的内容没有括号检查，坏了也不会被打回。

## Consequences

- 一个新的 Lisp 文件可以用一次 `edit_buffer` 写完，写进去之前先过括号检查；少一个括号会被打回，buffer 不会被写坏。
- 文字仍然放在 JSON 字符串里，转义一层；Lisp 字符串那一层只在这种写法里没有了。从 `eval_elisp` 里嵌套调用时，两层都还在。
- 有字的 buffer 用空 `old_string`，仍然要多来回一次，但回答里说了怎么往末尾加。
- 默认工具下模型用 `find-file-noselect` 打开新文件时，看不到 `open_file` 回答里那句话（见"实现时定的"第 5 条）。
