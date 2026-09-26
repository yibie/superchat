# Decision: an editor mode and an agent mode for one conversation, with the plain s/n review of unsaved changes in color

Status: accepted — user, 2026-09-24, over one conversation: "我还是喜欢侧边栏布局", then "1. 我希望在编辑文件时，可以是侧边栏为主。2. 但如果我希望是直接操作 emacs 的时候，我又希望以 agentic 的界面为主。", "关于 diff 其实我很满意当前这个交互……最好可以美化一下", "4. B 就可以了", and finally "1. A。 2. A"; amended 2026-09-25 when slice 1c was assigned: "审核界面不应该按键跳转，光标应该自动跳转到 diff 界面"; and before slice 2, after comparing with Cursor: "1d 2a"; implementation state per slice is in [the roadmap](../../roadmap.md).

## Problem

用户原话（2026-09-24，依次）：

- "superchat 与 emacs 的结合，是否足够顺滑；如果 superchat 能够和 cursor 一样，在侧边栏指挥 emacs 修改对应的文件，和选取，应当如何；希望可以用 diffs.el 为修改文件提供良好的视图，以及可以对文件选取，提供 annotation 的特性。"（参考 avante.nvim 与 unreal-agent）
- "我感觉这里有 2 套交互，一个是侧边栏对话窗口作为总控。一种是类似 gptel 和 gptel-agent 和 gptel-inline 这样，直接在 buffer 上修改的方式。"
- "1. 关于 diff 其实我很满意当前这个交互，虽然它不那么直观，但很明确，䒋乎很快看到，只不过内置的 ediff 是纯字符，没有进行颜色区分，最好可以美化一下 2. 关于共享对话，这个情况有点复杂。因为我没有设置 project——如果不在一个 project 里的话，怎么处理？ 3. 我还是喜欢侧边栏布局。 4. B 就可以了。"
- "恩，显然我陷入了既要又要的境地。1. 我希望在编辑文件时，可以是侧边栏为主。2. 但如果我希望是直接操作 emacs 的时候，我又希望以 agentic 的界面为主。这怎么办呢？"
- "1. A。 2. A"

证据、行号与探针在 `.scratch/cursor-like-integration/REPORT.md`。要解决的是：

1. **编辑文件时聊天窗把代码挡住**：打开聊天会为会话新建一个 tab，并在 `superchat-display-single-window`（默认 `t`）下删掉其他窗口。
2. **用户满意的审阅屏不在产品里**：有人值守验收时每次 Run 结束弹出的"一次一个 Buffer、`s` 存、`n`/`q` 不存"是验收脚本自己做的（`ca-reviewer-saves-p`），把 `diff -u` 当普通文字显示在 `special-mode` Buffer 里，所以没有颜色。产品里只有一条 echo area 提示、`/changes` 文字清单与 `superchat-changes-diff`（`diff-buffer-with-file`）。superchat 从不调用 ediff。
3. **在代码里让 AI 做的事，对话窗口看不到**：`superchat-act` 进聊天窗当前那段对话；就地问、就地做、改写选区进 `inline:<git 仓库根>`，不在仓库里就进 `inline:global`。
4. 其余不顺之处（指给 AI 看很费劲、聊天里的改动显示成 Lisp 列表）不在本决定内。

## Decision

1. **一段对话，两种模式，一个键切换。** 编辑文件时用**编辑模式**：对话在当前 tab 右侧的侧窗口里，代码留在左边。直接操作 Emacs 时用 **agent 模式**：对话占满它自己的 tab，也就是今天 `M-x superchat` 与有人值守验收时的样子。一个固定的键在两种模式之间切换，对话不变；同一次 Emacs 里打开时用上次的模式，启动后按选项 `superchat-display-style` 的初始值（默认编辑模式）。来源："我还是喜欢侧边栏布局"，随即补充为"编辑文件时侧边栏为主……直接操作 emacs 的时候……以 agentic 的界面为主"。
2. **审阅屏就是验收时那一块，搬进产品并上色。** 一次一个 Buffer，`s` 保存、`n` 先不存看下一个、`q` 结束；颜色来自 `diff-mode`，装了 diffs.el 时再加 `diffs-minor-mode` 的渲染；不锁住 Emacs。来源："关于 diff 其实我很满意当前这个交互……最好可以美化一下"。
3. **审阅屏在 Run 留下未存 Buffer 时自动出现，光标自己进到它上面**，可以关掉。来源："2. A"（当时定为不抢焦点）；2026-09-25 改为"审核界面不应该按键跳转，光标应该自动跳转到 diff 界面"。
4. **就地做的批准仍在 minibuffer。** 来源："4. B 就可以了"。
5. **就地问、就地做、改写选区进对话窗口当前显示的那段对话**（编辑模式或 agent 模式显示的同一段），不按 project 划分。来源："1. A"，以及"我没有设置 project"。
6. **不做**：逐块保留/撤回（在代码里就地取舍，或 diffs.el 多文件审阅的 `A`/`R`/`C-c C-c`）。**搁置**：批注发回 AI、引用标签与攒着不发、聊天里的小 diff、跟随与总控条。

## 编辑模式与 agent 模式

- **编辑模式**：对话显示在当前 tab 右侧的 side window（`display-buffer-in-side-window`，宽度可设）。不新建 tab，不删除其他窗口；从对话窗口里发起的操作不把对话挪进代码所在的窗口。
- **Agent 模式**：沿用今天的会话 tab 与整窗显示（`superchat--display-session-view`），`superchat-display-single-window` 只在这种模式下有意义。
- **切换键**：从编辑模式切到这段对话自己的 tab（没有就建）；在 agent 模式切回最近使用的那个编辑 tab，对话出现在它的侧窗口里。切换靠界面上的一个键，不靠设置项。键是对话窗口里的 `C-c C-w`（命令 `superchat-toggle-display-style`，在代码窗口里用 `M-x` 调用时作用于这个 tab 侧窗口里的对话）；它在对话里遮住的是 org 的 refile，对话里用不到。"最近使用的那个编辑 tab"是切出去时所在的那个 tab；它已经关掉时，用最近用过的另一个 tab；只剩对话自己的 tab 时，新开一个 tab，显示最近用过的非对话 Buffer，对话在它的侧窗口里。
- **所有权与关闭**：会话 Buffer 的所有权（[ADR 0004](../../adr/0004-session-editor-workspace-authority.md)）不变，侧窗口只是显示位置；agent 模式的 tab 仍是今天的会话 tab，关掉它走[关闭协议](../../protocols/session-editor-workspace-close.md)。一段只在侧窗口里显示过、没有自己 tab 的对话，由 `superchat-close-session-workspace` 在对话窗口里调用时，从该 Buffer 自己的所有权标记取得会话——协议已规定"live buffer-local ownership markers are the close authority"，这是在"直接给出"与"当前 tab"之外的第三个取得方式；协议第 1 步在编辑模式那一片随之修改。
- 同一个对话 Buffer 可能同时显示在侧窗口和它自己的 tab 里；输入区等状态在 Buffer 上，各窗口的光标位置互不影响。与 `superchat-plan` 已占用的侧窗口按槽位分开。
- **选项与各片的分工**（编辑模式那一片定）：`superchat-display-style` 取 `editor`（默认）或 `agent`。编辑模式那一片里，模式就是这个选项的值；切换键那一片加上键与"同一次 Emacs 里用上次的模式"，选项从此只给初始值。
- **侧窗口的槽位与宽度**：对话用 `superchat-side-window-slots` 里自己的一项 `chat`，槽位 -1，在计划面板（`plan`，0）之上；宽度就是 `superchat-side-window-width`——同一侧的侧窗口共用一列，另设一个宽度选项不会生效。打开对话时不关掉别处已经显示这个 Buffer 的窗口（共用的侧窗口函数替其它面板关掉），因为同一段对话可以同时显示在几处，另一个 frame 里的也算。
- **已经在这段对话自己的 tab 里**：编辑模式下，当前 tab 正是要显示的这段对话的 tab 时，按 agent 模式显示（占满这个 tab），不在它自己的 tab 里再开一个侧窗口显示它自己。
- **哪些操作把对话放上屏幕**：`superchat`、`superchat-new-session`、`superchat-open-session` 与 `superchat-send-region` 显示并选中对话窗口，`superchat-act` 显示但不选中，与今天相同；编辑模式下它们都用上面那个侧窗口。它们送进哪一段对话不在本片改动：`superchat-act` 与 `superchat-send-region` 今天进主对话 Buffer（`superchat-buffer-name`），侧窗口若显示着另一段对话，两者会不一致，由共用对话那一片定。
- **关闭时取得会话的顺序**：直接给出的编号；否则当前 Buffer 属于某段对话时（对话 Buffer 就是），用它自己的所有权标记；否则用当前 tab 的会话参数。

## 审阅屏

- **内容**：一次一个 Buffer。正文开头写明"第几个/共几个、Buffer 名与文件、模型已完成、这个 Buffer 未保存"和可用的键，下面是"磁盘文件对 Buffer"的统一 diff，标签用 `a/文件名`、`b/文件名`，审阅 Buffer 的 `default-directory` 为文件所在目录。diffs.el 占用顶栏，所以说明写在正文里。
- **键**：`s` 保存这个 Buffer（`superchat-changes-save`，用户自己的操作）；`n` 这个先不存、看下一个；`q` 结束审阅、其余保持未存；空格/退格翻页；`M-n`/`M-p` 在改动块之间移动。这些键放在 Buffer 局部的 `minor-mode-overriding-map-alist` 里，压过 `diff-mode` 与 diffs.el 的同名键；会改动源 Buffer 或另起逐块流程的键（`diff-mode` 的应用补丁，diffs.el 的 `A`/`R`/`U`/`C-c C-c` 与批注键）在审阅屏里停用。没按 `s` 就什么都不存。
- **颜色**：审阅 Buffer 用 `diff-mode`；diffs.el 在 load-path 上时，在同一个 Buffer 上开 `diffs-minor-mode`（diffs.el 为外部生成的 diff Buffer 准备的入口），diff 文字不变。批处理核对见报告第 10.2 节。
- **光标自己过去**（2026-09-25 起；此前是"不抢焦点"）：Run 结束自动出现时，它显示在对话窗口以外的窗口（编辑模式下是代码区；agent 模式下在对话旁分出一块），并被选中，不需要任何键；选中时丢弃此前积压的按键，所以出现之前敲的键不会存盘。这一刻若 minibuffer 正在用（在回答问题或输入命令），先显示、不选中，minibuffer 结束后再选中。审阅结束（最后一个 `s`/`n`，或 `q`）时，光标回到它出现之前所在的窗口。用户用命令主动打开时（`/changes`、`superchat-changes-diff`、就地做的 `d`）同样选中并丢弃积压的按键。这沿用[批准只接受提问之后的输入](../implemented/2026-09-19-an-approval-is-answered-by-input-that-came-after-it.md)的同一个道理。
- **候选**：Run 结束时是该 Run 与其委派子 Run 的推导（`superchat-changes-candidates`）；`/changes` 是本会话的推导。磁盘上还没有的新文件以"整文件新增"的 diff 显示，`s` 照常保存；不访问文件的 Buffer 没有文件可存，不进审阅屏，仍在 `/changes` 的文字清单里。
- **Run 结束的 echo 提示与 `/changes` 的文字清单照旧**；自动出现可由选项关掉。
- **从代码 Buffer 用命令打开**（2026-09-25 补）：`M-x superchat-changes`、`superchat-changes-diff`、`superchat-changes-save` 与 `-revert` 选 Buffer 时，用的是当前 Buffer 自己的对话；当前 Buffer 不是对话窗口时，用这段代码的就地命令会进的那段对话（第 2 片的规则：这个 tab 右侧显示的那段，没有就是主对话），不再报类型错误。问题见 `.scratch/issues/2026-09-25-superchat-changes-outside-a-chat-view.md`。

## 共用一段对话

- 规则：就地问、就地做、改写选区都进对话窗口当前显示的那段对话；两种模式显示的是同一段；还没打开过对话时用上一次那段，没有就新建；换对话在对话窗口里换。与 project 无关（Emacs 只把 git 目录自动当作 project；用户不设置 project）。
- `superchat-act` 已经这样路由（`superchat-submit` → `superchat--ensure-request-chat-buffer`）；就地命令的会话编号来自 `superchat-inline-session-id`，改为同样的来源。
- 对话窗口实时显示别处发起的一轮，并标明来自哪个 Buffer 哪一行、可以跳回去。
- 对话在跑任务时，代码里的新问题按 Runtime 的会话队列排队。
- **就地请求不带这段对话的历史**（2026-09-25，用户："1d"）：就地问、就地做、改写选区发给模型的只有它自己的引用（哪个 Buffer 哪几行、选区）和用户的话，没有这段对话之前的轮次；没有按次开关。这一轮照样记进对话、在对话窗口里显示，之后在对话窗口里发的请求把它当作历史带上——对话窗口是总控，看得见代码里发生的事。要让就地的事带上对话的历史，就把代码送进对话（`superchat-send-region`）在那里问。参照：Cursor 的就地编辑（`Cmd+K`）默认看不到侧栏对话，靠显式的 `@Chat` 或 `Cmd+L` 把两边连起来；gptel 的改写只发选区和要求。与 Cursor 不同的是，Cursor 不把就地编辑记进侧栏对话，这里按本决定的第 5 条记进去。
- **在同一个小框里追问带这个小框自己的轮次**：就地问、就地做的追问（`r`）带上这个小框之前的问答，仍不带对话里别的轮次——Cursor 的就地编辑也是在自己的输入框里接着改。改写选区的"再改一次"本来就把上一次的结果作为底稿发出，不需要历史。（建这一片时发现：自"之前几轮按真正的消息发送"那项改动起，就地追问的第一问一答没有再发给模型，这里一并修好。）
- **就地的一轮不改对话窗口关联的来源 Buffer**（用户："2a"）：它带自己的引用；对话窗口原先关联的来源（`[Emacs origin]`）不变，关联只由明确的入口设定。

## Token 约束

不新增模型工具，不往系统消息里加随请求变化的文字；两种模式与审阅屏对模型 token 为零。每一片的回归断言同一 Run 相邻请求的前缀逐字相同，系统消息与工具定义与本决定之前一致（实测缓存命中 88%–93% 依赖这一点，报告第 6 节）。`read_buffer` 无上限另见 `.scratch/issues/2026-09-24-read-buffer-has-no-window-or-cap.md`。

## 不照搬的

- avante：默认全部自动批准；"接受"即存盘（`noautocmd write!`）；选区编辑不预览、直接写进 Buffer；每处改动停下等确认。
- gptel-agent：每次编辑先确认、确认后直接写磁盘（`write-region`）。
- gptel：把回答直接写进用户的 Buffer 当作对话的默认做法；每次请求重读的"活的上下文"。

## 分片与验收

每片先写红测试（各自独立的测试文件，不进 `scripts/m3-regression.el`），单独验收，leader 提交。

1. **1a 审阅屏**：一次 Run 留下两个未存 Buffer 时，审阅屏自动出现在另一个窗口、焦点不动（1c 起改为光标自己进去），显示"1/2"与第一个的彩色 diff；`s` 之后磁盘文件等于 Buffer 并显示"2/2"；`n` 之后第二个仍未存；`q` 结束且不存任何东西；停用的键不改动源 Buffer；装了 diffs.el 时开着 `diffs-minor-mode`；新文件显示为整文件新增并可保存；不访问文件的 Buffer 不进审阅屏而仍在 `/changes` 清单里；`/changes` 与 `superchat-changes-diff` 打开同一块屏并选中它，打开前积压的按键不会存盘；关掉选项后不自动出现，echo 提示照旧。
2. **1b 编辑模式**：在一个文件里打开对话，对话在右侧侧窗口、代码仍可见、没有新 tab；从对话发起的操作不挪动代码窗口；在对话窗口里关闭会话按关闭协议进行；`superchat-display-style` 为 `agent` 时行为与今天一致；canonical smoke 不变（它逐字比较发给模型的请求）。（这一条原先写作"`frame` 设置下"，`frame` 是提案早先一稿里这个选项的值名。）
3. **1c 切换键与审阅屏的光标**：编辑模式按键进入对话自己的 tab 并占满，再按回到刚才的编辑 tab、对话在侧栏，输入框里没打完的字还在；同一次 Emacs 里再打开用上次的模式。Run 结束审阅屏自己出现时光标已在它上面，出现前敲的键不存盘；minibuffer 在用时等它结束再过去；审阅结束光标回到原来的窗口。
4. **2 共用一段对话**：在 `~/Documents` 下一个不在 git 里的文件里就地做一件事，对话窗口（无论哪种模式）里出现这一轮并能跳回那一行；就地请求里没有这段对话之前的轮次，之后在对话窗口里发的请求带上这一轮；对话在跑任务时就地问，问题排队并在前一个 Run 结束后执行；对话窗口关联的来源不因就地的一轮而变；审阅屏与 `/changes` 列出就地做改过的 Buffer。

第 1b、1c、2 片完成后各用 DeepSeek 做一次有人值守的真实模型验收，编辑模式与 agent 模式各跑一次；已接受的两入口验收是在整窗布局与脚本审阅屏下做的，在新条件下重跑。

## Alternatives considered

**只要侧栏**（本决定的前一稿"侧栏为默认、整窗可选"）。用户随即补充：直接操作 Emacs 时要 agent 界面为主。整窗对话因此是与侧栏并列的一种模式，而不是可选的旧样子。

**agent 模式在当前 tab 里"占满再恢复"，不用会话自己的 tab**。少一个 tab，但丢掉今天"关掉会话 tab 就按关闭协议收拾这段对话"的入口，还要另建一套恢复窗口布局的状态；会话 tab 本来就是今天的整窗形态。

**按从哪里打开来自动选模式**。操作 Emacs 时人常常正好在某个文件里，规则会猜错，从不同 Buffer 打开还会来回跳；"用上次的模式、一个键切换"更可预料。

**用设置项决定模式**。人一天里在两件事之间来回，模式应当随手一个键就换；Cursor 的"编辑器 / Agent"布局切换也是界面上的一个按钮。

**逐块保留/撤回**（在代码里就地取舍，或 diffs.el 多文件审阅）。用户对整 Buffer 的 s/n 满意，只要上色。

**就地做的批准显示在光标下方的覆盖层**（gptel-inline 的做法）。用户选择仍在 minibuffer。

**按 project 划分对话**（今天就地命令的规则）。用户不设置 project，Emacs 只把 git 目录当作 project，不在 git 里的文件全部落进一段全局对话；对话窗口因此看不到在代码里做过的事。

**审阅屏照搬脚本的模态等待**（`recursive-edit`）。脚本为了同步拿到答案；日常使用中锁住整个 Emacs 不合适。

**用 `diffs-items` 打开 diffs.el 自己的审阅**。它带自己的一套键与多文件堆叠视图，与"一次一个 Buffer、s/n"不合；`diffs-minor-mode` 正好给现有的屏上色。

**改动先不进 Buffer，放影子副本或补丁队列**；**ediff 或 smerge**；**走 diffs.el 的 CLI**；**活的上下文**。违背 buffer-first；流程重或要往 Buffer 里插标记；同一个 Emacs 里直接调函数更简单；会破坏只追加的前缀与缓存命中。

## Consequences

- 场景一（2026-09-10 定义为"像 ChatGPT 桌面应用的聊天窗"）成为两种模式之一的 agent 模式；编辑文件时的默认是侧窗口。
- 验收脚本自己的审阅屏被产品的审阅屏取代；有人值守验收在新布局与产品审阅屏下重跑。
- 审阅屏在 Run 结束时自动出现，属于可卸下的 `changes` 行（它的 `turn-completed` 监听）；卸下这一行时不再自动出现。`/changes` 命令（由 `chat-commands` 注册）与 `superchat-changes-diff` 不经这一行，照旧可用，与以前一样。
- `superchat-display-single-window` 只对 agent 模式有意义。
- 关闭协议第 1 步多一个取得会话的方式（对话 Buffer 自己的所有权标记），在编辑模式那一片修改协议文档。
- 就地命令不再产生 `inline:<仓库根>` 与 `inline:global` 会话；已存在的这类会话仍可打开，Tape 不改写。M8 的回归随之修改。
- diffs.el 是可选依赖：没装时颜色来自 `diff-mode`。它还年轻（0.x，不在 ELPA/MELPA），集成只用公开入口 `diffs-minor-mode`。它读新侧时优先用 live Buffer；旧侧的上下文展开与旧侧语法着色走 VC 后端，本决定不传后端，这两项可能不可用或退回无着色显示。
- 自动出现加单字母键，最大的风险是在别处打字时误存盘。1a 用"不抢焦点、切入时丢弃积压按键"挡住；1c 起光标自己过去，挡住的只剩"出现之前敲的键被丢弃"：审阅屏出现之后才敲下的 `s` 会存盘。审阅屏打开期间，用户环境（如 super-save）若先存了某个 Buffer，这一项的 diff 变空，屏上注明"文件已与 Buffer 相同"。
- 共用一段对话后，对话在跑任务时就地问答要排队；代码里的问题带整段历史，token 比 `inline:` 会话多，多出的大多能命中缓存。
