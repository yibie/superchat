# Decision: 一段常驻的 Emacs 操作说明（M5 第四片的最小形态）；第三片继续等证据

Status: implemented（2026-09-20；同日 accepted——用户选定：先内容、再决定载体；默认开、可关；暂不安排真人验证）

## 一句话

带着能改 Emacs 的工具的请求，系统提示里多一小段操作说明（一千字符以内），内容来自真实运行里模型反复犯的错，并且符合"模型改 Buffer、用户保存"的分工。默认开，用户可以关掉或换成自己的文字。"随包参考手册 + `read_reference` 取阅工具"（第三片）不在本决定内，继续是提案。

## 为什么

[操作现场与参考资料契约](../proposed/2026-09-16-emacs-operating-context-and-reference-contract.md)把第三、四片留到"证明补知识有用"之后，并写明内容先于载体：如果一小段常驻说明就够，按编号取阅的整套机制就是不必要的复杂度。

当时（2026-09-17）试过一份 1024 字的候选说明，有进步迹象但没过答复准确性的门槛，没有进默认配置。那份候选现在**与产品相反**：它写着"eval_elisp 能读、编辑和保存文件……要持久化就更新配置、保存并应用"，而 M7 之后模型不存盘——2026-09-19 的三次 act 失败正是被同类指示带出来的。所以不能拿它直接用，要重写。

2026-09-18 到 20 日的有人值守运行（两个模型、十一次）留下了一批具体、重复的知识错误，是这段说明的素材：

| 观察到的错误 | 出处 |
|---|---|
| 查询不存在的变量 `fundamental-mode-map`（三次运行） | chat-02 undo、chat-03、act-06 |
| `lookup-key` 的键参数用字面字符串而不是 `(kbd …)`；把返回的整数读成"未绑定" | 09-17 C/chat、chat-03 undo |
| 改了 Buffer 之后用 `load` 磁盘文件来"生效"——磁盘还是旧内容 | chat-03 undo |
| `eval_elisp` 一次传了多个表达式 | act-06 undo |
| 编造不存在的函数名 | act-02 undo、act-04 undo |
| 撤销时把"原值"当成"未绑定" | 09-17 B/chat、chat-03 undo |
| 答复里写了没有验证过的"重启后仍在" | chat-02、act-06 apply |

## 契约

1. **内容**：一段英文说明，不超过 1024 个字符，只含通用的 Emacs 知识、现有工具名和通用函数／变量名；不含任务目标、夹具路径或可以照抄的任务脚本。要点：问运行中的 Emacs 而不是猜（`user-init-file`；先确认符号存在；`fundamental-mode` 没有 keymap 变量）；键位用 `(kbd …)`，`lookup-key` 的整数含义，`key-binding` 才是有效绑定；改文件走 `open_file` → `edit_buffer`，改动留在 Buffer、磁盘要等用户保存，所以 `load FILE` 读到的是旧内容，要当场生效就对 Buffer 求值；`eval_elisp` 只收一个表达式；撤销是恢复先前观察到的值；只报告检查过的事，说明改的是哪个 Buffer、未保存、保存归用户，没重启过就不要声称重启有效。
   它**不得**含有任何要求模型保存或自行持久化的指示（与 M7 的回归同一条规则）。
2. **何时出现**：作为 `system-instructions` 收集器里的一块，走已有的"按最终工具面渲染"机制——最终工具面上有 `eval_elisp` 或 `edit_buffer` 时出现，都没有时不出现。和 `define_tool`、`render_ui` 两块同一种做法；不新增收集器、Service 或插件行。适用的 Surface 与该收集器现有范围相同。每次请求（含工具轮之后的后续请求）里至多出现一次。
3. **用户选项** `superchat-emacs-operating-note`：`t`（默认，用内置文字）、`nil`（不发）、或一个字符串（用用户自己的文字，同样只在上述工具在场时出现）。
4. **代价如实说**：带这些工具的每次请求多约 250 个 token，对所有模型一样。
5. **不变的**：工具集、权限档位、批准、`/changes`、任何工具的行为。说明是文字，不授予也不限制任何能力；模型不照做时，兜底的仍然是已有的机制（坏修改打回、提问、保存审阅）。
6. **效果未经验证**：用户决定暂不安排真人验证。本决定交付的是"这段文字按约定出现在请求里"，不是"模型因此做对"。文档和 CHANGELOG 必须这样写；不得从脚本回归推断真实模型的收益。第三片（手册与 `read_reference`）要不要做，仍等真实使用或之后的对照给出证据。

## 验收

新回归 `scripts/operating-note-regression.el`（真实准备／选择／组装路径，假 transport 捕获最终请求）：

1. 默认聊天请求的系统文字里有这段说明，且只有一次；工具轮之后的后续请求同样只有一次。
2. 最终工具面上没有 `eval_elisp` 和 `edit_buffer`（显式工具列表、`none`）→ 没有这段说明。
3. 选项为 `nil` → 没有；为字符串 → 出现的是那个字符串，内置文字不出现。
4. 当前 buffer 入口（act）的请求同样带有它。
5. 内置文字不超过 1024 字符；含 `kbd`、`eval-buffer`、`user-init-file`、未保存／保存归用户的表述；不含任何保存指示。

1、3（字符串一半）、4、5 在实现前必须是红的。canonical smoke 基线因系统文字多出这一块而变化的，逐项列出。其余套件、Kernel 检查不变。

## 交付记录（2026-09-20）

- 实现（`superchat-prompt-hooks.el`）：用户选项 `superchat-emacs-operating-note`、内置文字 `superchat-emacs-operating-note-text`（1014 字符）、块函数 `superchat-prompt-hook--emacs-operating-note`，作为第五块加入已有的 `system-instructions` 收集器（排在最后）。出现条件分两步，和已有机制一致：收集时，请求**已决定**的工具名里没有 `eval_elisp`／`edit_buffer`（显式列表或 `none`）就不给；工具"未定"时给（当前 buffer 入口在收集时还没选工具，之后才定为默认面）；之后若中间件改写了工具，按最终工具面由 `:render-tools` 重新渲染。没有新增收集器、Service、插件行或工具。
- 回归 `scripts/operating-note-regression.el`（6 个，真实准备／选择／组装路径，捕获请求真正携带的系统文字）：默认聊天请求带且只带一次、工具轮之后同样；显式工具列表与 `none` 不带；中间件把工具收窄后不带；选项 `nil` 不带、字符串则只带用户的文字；当前 buffer 入口同样带；内置文字不超过 1024 字符、含约定的要点、不含任何保存指示。
- 实现过程中红测抓到的三处，都是我的错，如实记下：块函数没有返回 Turn，导致收集器整体报错、**连原有的系统说明一起丢失**（一个原先"通过"的守护测试当时其实是空转）；测试最初从错误的位置读系统文字；当前 buffer 入口在收集时工具未定，第一版门控把它也排除了。
- 变异体（各自让对应测试变红）：不看工具一律发送；改写后不再按最终工具面渲染；无视用户的 `nil`；在文字里加一句要求保存。
- canonical smoke 基线变化，仅两处、各一行：`A.txt` 与 `A.full.txt` 的 `scripted-prompt-text`，系统文字在"并行工具调用"那一段之后多出这段说明。受保护的 `m3/step/full-baseline-rejects-missing-step-facts` 会重跑 smoke 的 A 场景，基线更新前随之失败、更新后通过；`scripts/m3-regression.el` 本身未改。
- 其余全部套件、Kernel 检查、加载通过；编译警告与改名后的基线一致；验收脚本校准通过，它发出的请求里同样带着这段说明。
- **未做、未证实**：没有任何真实模型运行验证这段说明的效果（用户的选择）。2026-09-19 通过的 DeepSeek 验收是在**没有**这段说明的产品上完成的；加了之后对它是好是坏，同样没有验证。第三片（随包手册与 `read_reference`）仍是提案。

## 更正（2026-09-20，同日）：两处不审慎的内容

复核指出内置文字里有两处建议本身不稳妥，在它被固定为第三片对照的共同基线之前改掉：

| 原文 | 问题 | 改为 |
|---|---|---|
| "To apply new buffer text now: `(with-current-buffer NAME (eval-buffer))`" | 无条件推荐执行整个 Buffer。验收用的 init 文件只有几行，执行整个 Buffer 无害；真实用户的 init 文件会因此把全部配置重跑一遍（重复的 hook、重新初始化的包、被切换的 mode）。 | "To make your change live, evaluate only the forms you changed, not the whole buffer: eval-buffer reruns all of it." |
| "say it is unsaved" | 预设"未保存"。用户可能已经在两个 Run 之间保存过（`m7-attended-act-01-deepseek` 的 undo 正是这种情形），那时这句话让模型报告一个没有检查过的状态，与同一段里"只报告检查过的事实"相悖。 | "name the buffer you edited and whether it is unsaved (buffer-modified-p)" |

为守住 1024 字符上限，另把 "a function or variable" 缩成 "a symbol"，并删去与后半句重复的 "The edit stays in the buffer"。现长 1023 字符。

验收第 5 条相应改为：含"只执行改动的表达式、不执行整个 Buffer"和 `buffer-modified-p` 的表述；不含无条件的 `eval-buffer` 用法和"say it is unsaved"。`operating-note/the-built-in-text-keeps-to-its-contract` 先红（缺 "only the forms you changed"）后绿。canonical smoke 基线变化仍只有 `A.txt`、`A.full.txt` 各一行里的这三处措辞。

**这仍是未经真实模型验证的文字。** 2026-09-19 通过的 DeepSeek 运行里，模型正是用 `eval-buffer` 让修改生效的；改成"只执行改动的表达式"之后模型做得更好还是更差，没有运行过。
