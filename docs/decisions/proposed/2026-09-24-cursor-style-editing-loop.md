# Decision: two interaction sets — a side panel as master control, and in-buffer work like gptel — with the plain s/n review of unsaved changes, in color

Status: proposed — partly decided by the user on 2026-09-24 (side panel is the default layout; in-buffer approvals stay in the minibuffer; unsaved changes are reviewed on the plain s/n screen from the attended runs, brought into the product and colored); open: which conversation in-buffer work joins, and whether the review screen appears by itself.

## Problem

用户原话（2026-09-24，依次）：

- "superchat 与 emacs 的结合，是否足够顺滑；如果 superchat 能够和 cursor 一样，在侧边栏指挥 emacs 修改对应的文件，和选取，应当如何；希望可以用 diffs.el 为修改文件提供良好的视图，以及可以对文件选取，提供 annotation 的特性。"（参考 avante.nvim 与 unreal-agent）
- "我感觉这里有 2 套交互，一个是侧边栏对话窗口作为总控。一种是类似 gptel 和 gptel-agent 和 gptel-inline 这样，直接在 buffer 上修改的方式。"
- "1. 关于 diff 其实我很满意当前这个交互，虽然它不那么直观，但很明确，䒋乎很快看到，只不过内置的 ediff 是纯字符，没有进行颜色区分，最好可以美化一下 2. 关于共享对话，这个情况有点复杂。因为我没有设置 project——如果不在一个 project 里的话，怎么处理？ 3. 我还是喜欢侧边栏布局。 4. B 就可以了。"

证据、行号与探针在 `.scratch/cursor-like-integration/REPORT.md`（第 9、10 节对应后两段话）。结论：

1. **聊天窗把代码挡住**：打开聊天会新建一个 tab，并默认删掉其他窗口。
2. **用户满意的审阅屏不在产品里**：有人值守验收时，每次 Run 结束弹出的"整窗、一次一个 Buffer、`s` 存、`n`/`q` 不存"是验收脚本自己做的（`ca-reviewer-saves-p`），把 `diff -u` 当普通文字插进 `special-mode` Buffer，所以没有颜色。产品里只有一条 echo area 消息、`/changes` 文字清单和 `superchat-changes-diff`（`diff-buffer-with-file`）。superchat 从不调用 ediff。
3. **两套交互各记各的对话**：`superchat-act` 进聊天窗当前那段对话；就地问、就地做、改写选区进 `inline:<git 仓库根>`，不在仓库里就进一段 `inline:global`。在代码里发生的事，侧栏的对话不知道。
4. 其余不顺之处（指给 AI 看很费劲、AI 改的时候看不见、聊天里的改动显示成 Lisp 列表）留到后面的分片。

## Proposal

两套交互共用一个发动机：今天的会话、Run、Tape、工具、权限档位与 [buffer-first](../implemented/2026-09-18-buffer-first-mutation.md)。

### 已定一：侧栏是默认布局

- 新选项 `superchat-display-style`：`side`（默认）把聊天放在右侧 side window（`display-buffer-in-side-window`），代码留在旁边，一个键开关，不新建 tab、不删除其他窗口；`frame` 保留今天的整窗行为，`superchat-display-single-window` 只在 `frame` 下有意义。场景一（2026-09-10 定义为"像 ChatGPT 桌面应用的聊天窗"）由用户这句话改为"代码旁的侧栏为默认、整窗可选"。
- 会话 Buffer 的所有权（[ADR 0004](../../adr/0004-session-editor-workspace-authority.md)、[关闭协议](../../protocols/session-editor-workspace-close.md)）不变，侧栏只是显示位置。会话 tab 在 `side` 下怎么处理、与 `superchat-plan` 已占用的侧窗口怎么分槽，实现前对照确认。

### 已定二：审阅屏搬进产品，并上色

把验收里那块屏原样搬进产品，只改两处：有颜色、不锁住 Emacs。

- **内容**：一次一个 Buffer。正文第一行写明"改动审阅 1/2 · init.el（~/.emacs.d/init.el）· 模型已完成，这个 Buffer 未保存"，下面是"磁盘文件对 Buffer"的统一 diff（标签用 `a/相对路径`、`b/相对路径`）。
- **键**：`s` 保存这个 Buffer（调用 `superchat-changes-save`，是用户自己的操作）；`n` 这个先不存、看下一个；`q` 结束审阅、其余保持未存；空格/退格翻页；`M-n`/`M-p` 跳到下一块/上一块。这三个键放在 Buffer 局部的 `minor-mode-overriding-map-alist` 里，压过 diff-mode 与 diffs.el 的同名键（批处理核对 C3 通过）。没按 `s` 就什么都不存。
- **颜色**：审阅 Buffer 用 `diff-mode`，删除行、新增行各自着色（核对 C1 通过，零依赖）；装了 diffs.el 时再开 `diffs-minor-mode`——diffs.el 为"别人生成的 diff Buffer"准备的渲染器——加上行号栏、彩色条和词级高亮，diff 文字本身不变（核对 C2 通过）。这就是用户最初说的"用 diffs.el 为修改文件提供良好的视图"。diffs.el 会占用顶栏，所以说明写在正文里。
- **不锁住 Emacs**：验收脚本用 `recursive-edit` 等按键，那是脚本需要同步拿到答案；产品里的审阅屏是普通 Buffer，可以切走再回来。
- **不抢焦点、不被误按**：自动出现时（见问题 2）只显示在侧栏旁的代码区，不选中它；用户切过去（一个键或点击）之后 `s`/`n`/`q` 才生效，切入时丢弃此前积压的按键——这样在侧栏里打字时不会因为按到 `s` 而存盘。这沿用 [批准只接受提问之后的输入](../implemented/2026-09-19-an-approval-is-answered-by-input-that-came-after-it.md) 的同一个道理。
- **入口**：Run 结束时它留下未存 Buffer（按 `superchat-changes-candidates` 对该 Run 与其委派子 Run 的推导）；`/changes`；Run 结束提示；就地做的 `d`。
- **边界**：磁盘上还没有的新文件，显示"磁盘上还没有这个文件，保存会写入整个 Buffer"与全文，`s` 照常保存；不访问文件的 Buffer 没有文件可存，不进审阅屏，仍在 `/changes` 里列出。
- **不做**：逐块保留/撤回。用户对整个 Buffer 的 `s`/`n` 满意。

### 已定三：就地做的批准仍在 minibuffer

就地做（`superchat-inline-act`）时工具要批准的问题照旧在 minibuffer 里问，与其他入口一致；2026-09-19 的三条批准约定不受影响。

### 待定：就地的命令进哪段对话（问题 1）

用户说自己不设置 project。事实：Emacs 不需要设置，任何 git 目录自动算一个 project（`project-find-functions` 默认只有 `project-try-vc`）；用户的 `~/.emacs.d` 是 git 仓库，`~/Documents` 不是。今天的就地会话按 git 仓库分，不在仓库里就全部进一段全局对话。

建议**跟着侧栏走，不看 project**：就地问、就地做、改写选区都进侧栏当前显示的那段对话；侧栏没开时就打开它（或用这个 Buffer 上次用过的那段）；换对话就在侧栏里换。这就是"侧栏作为总控"的字面意思，也是 gptel-inline 规则链的第一条（这个 Buffer 上次用过的对话）。

- 便宜的一半：`superchat-act` 已经是这样路由的（`superchat-submit` → `superchat--ensure-request-chat-buffer`），其余三个就地命令改成同样的路由即可。
- 贵的一半：聊天窗今天只实时显示自己发起的那一轮（它对 `turn-completed` 只刷新输入区、发一条消息），要新增"显示别处发起的一轮"，并标明来自哪个 Buffer 哪一行、可以跳回去。
- 代价：侧栏在跑任务时，代码里的新问题排队等待；代码里的问题带上整段对话的历史，token 比今天多（多出的大多能命中缓存，并可对某一句选择不带历史）；侧栏对话与来源 Buffer 的关联（`[Emacs origin]`）规则要另定；M8 的回归测试随之修改。它改变的是会话身份的规则，须先写进本记录与 [CONTEXT.md](../../../CONTEXT.md) 再实现。
- 另一选择：维持今天的两段对话，只在侧栏加一个总览，列出两边正在跑的任务和未存的改动。

### 搁置，以后再议

- **批注发回 AI**（在审阅里选几行写意见、一键发回）：用户最初的要求之一，这一轮没有提到，记为搁置而非取消。审阅屏用了 diffs.el 的渲染器之后，diffs.el 自己的选行（`v`）与批注（`a`）在这块屏上是否可用，没有验证。
- **引用标签与攒着不发**：在任意 Buffer 选一段放进侧栏输入框、发送时取快照带行号展开（对应 dsh 的 `@file`，不学 gptel 每次请求重读的"活的上下文"）。
- **聊天里每处改动显示成小 diff、代码窗口跟随、侧栏的总控条**（对应 dsh 的 diff 卡片与 `locations`）。
- **不做**：就地逐块保留/撤回；diffs.el 多文件逐块审阅（`A`/`R`/`C-c C-c`）。理由是用户对 s/n 审阅满意。

### Token 约束（贯穿全部）

不新增模型工具，不往系统消息里加随请求变化的文字；侧栏与审阅屏对模型 token 为零。每一片的回归都断言：同一 Run 相邻请求的前缀逐字相同，系统消息与工具定义与改动前一致（实测缓存命中 88%–93% 依赖这一点，报告第 6 节）。`read_buffer` 无上限另见 `.scratch/issues/2026-09-24-read-buffer-has-no-window-or-cap.md`。

### 不照搬的

- avante：默认全部自动批准；"接受"即存盘（`noautocmd write!`）；选区编辑不预览、直接写进 Buffer；每处改动停下等确认。
- gptel-agent：每次编辑先确认、确认后直接写磁盘（`write-region`）。
- gptel：把回答直接写进用户的 Buffer 当作对话的默认做法；"活的上下文"。

### 分片

每片先写红测试（各自独立的测试文件，不进 `scripts/m3-regression.el`），单独验收，leader 提交。

1. 侧栏布局 + 审阅屏（搬进产品、上色）。
2. 就地的命令进哪段对话（按问题 1 的答案）。
3. 以后按此顺序：引用标签、聊天里的小 diff、跟随与总控条。

第 1、2 片完成后各用 DeepSeek 做一次有人值守的真实模型验收；已接受的两入口验收是在整窗布局与脚本审阅屏下做的，要在新布局与产品审阅屏下重跑。

## 待用户决定

1. **就地的命令进哪段对话**：(A，建议) 跟着侧栏走，进侧栏当前显示的那段，不看 project；(B) 维持今天：在 git 目录里按仓库、不在就一段全局，侧栏另一段，侧栏加一个总览。
2. **审阅屏何时出现**：(A，建议——这是验收时"很快看到"的那个体验) Run 结束留下未存的改动时自动出现在侧栏旁的代码区，不抢焦点，可以关掉；(B) 只在用户要时出现（点提示、`/changes`、就地做的 `d`）。

## Alternatives considered

**逐块保留/撤回**（在代码里就地取舍，或 diffs.el 多文件审阅的 `A`/`R`/`C-c C-c`）。本记录前一稿的主张；用户答"关于 diff 其实我很满意当前这个交互……最好可以美化一下"，所以不做，只给整 Buffer 的 s/n 上色。

**就地做的批准显示在光标下方的覆盖层**（gptel-inline 的做法）。用户选择仍在 minibuffer（"B 就可以了"）。

**按 project 划分对话**。这是今天就地命令的规则，也是 gptel-inline 规则链的第三条；用户不设置 project，而 Emacs 只把 git 目录当作 project，不在 git 里的文件全部落进一段全局对话，与"侧栏总控"无关。它作为问题 1 的选项 B 保留。

**审阅屏照搬脚本的模态等待**（`recursive-edit`）。那是脚本为了同步拿到答案；日常使用中锁住整个 Emacs 直到按 s/n 不合适，产品里改为普通 Buffer。

**用 `diffs-items` 打开 diffs.el 自己的审阅**。它带自己的一套键（`s` 切换并排、`n` 下一块、`q` 恢复窗口）和多文件堆叠视图，与"一次一个 Buffer、s/n"不合；`diffs-minor-mode` 是 diffs.el 为外部 diff Buffer 准备的入口，正好给现有的屏上色。

**整窗聊天，另开 frame 看代码**。用户选择侧栏为默认。

**改动先不进 Buffer，放影子副本或补丁队列**；**ediff 或 smerge**；**走 diffs.el 的 CLI**；**活的上下文**。理由同前一稿：违背 buffer-first；流程重或要往 Buffer 里插标记；同一个 Emacs 里直接调函数更简单；会破坏只追加的前缀与缓存命中。

## Acceptance criteria

用户旅程（脚本化 transport 回归逐片覆盖，加 DeepSeek 有人值守验收）：

1. **侧栏**：从文件 Buffer 打开聊天，聊天在右侧、代码仍可见、没有新 tab；开关键能隐藏和恢复；`frame` 设置下行为与今天一致（canonical smoke 不变）。
2. **审阅屏**：一次 Run 留下两个未存 Buffer：审阅屏显示"1/2"与第一个的彩色 diff；`s` 之后磁盘文件等于 Buffer、看到"2/2"；`n` 之后第二个仍未存；`q` 结束且没有任何存盘。装了 diffs.el 时有行号栏与彩色条，没装时是 diff-mode 的颜色，两种情况 diff 文字相同。自动出现时焦点仍在原窗口，出现之前或之后在侧栏里按下的 `s` 不会存盘；切到审阅屏后积压的按键被丢弃。新文件显示全文并可保存；不访问文件的 Buffer 不进审阅屏。`/changes`、Run 结束提示与就地做的 `d` 打开同一块屏。
3. **对话规则**（若问题 1 选 A）：在 `~/Documents` 下一个不在 git 里的文件里就地问一句，侧栏当前那段对话里出现这一轮并能跳回那一行；侧栏在跑任务时就地问，问题显示为等待并在前一个 Run 结束后执行；选"不带历史"时请求里没有这段对话的历史。
4. **不增 token**：每片的回归断言同一 Run 相邻请求前缀逐字相同，系统消息与工具定义与本提案之前一致。

## Risks

- **自动出现的审阅屏被误按**：单字母键加上自动出现，最危险的是用户在别处打字时按到 `s` 存盘；靠"不抢焦点、切入时丢弃积压按键"防住，第 2 条验收专门覆盖。
- diffs.el 还年轻（0.x，不在 ELPA/MELPA，用 `package-vc` 安装），接口可能变：只用公开入口（`diffs-minor-mode`），集成代码集中在一处，作为可选依赖；没装时 diff-mode 的颜色足够。真实 frame 与主题下的外观没有验证。
- **跟着侧栏走**改变会话身份的规则：聊天窗要实时显示别处发起的一轮；忙时排队会让就地问答变慢；侧栏对话与来源 Buffer 的关联规则要另定；M8 的回归要修改。
- 侧栏与会话 tab、`superchat-plan` 侧窗口的配合没有设计验证。
- 布局与审阅方式都变了，已接受的两入口验收要在新条件下重跑。
