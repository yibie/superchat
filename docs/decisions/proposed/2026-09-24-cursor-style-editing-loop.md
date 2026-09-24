# Decision: two interaction sets — a side panel as master control, and in-buffer work like gptel — sharing one review of unsaved changes

Status: proposed — Claude's recommendation of 2026-09-24, first drafted as one Cursor-style loop and reorganised the same day after the user named two interaction sets; the questions in "待用户决定" are open.

## Problem

用户原话（2026-09-24）："superchat 与 emacs 的结合，是否足够顺滑；如果 superchat 能够和 cursor 一样，在侧边栏指挥 emacs 修改对应的文件，和选取，应当如何；希望可以用 diffs.el 为修改文件提供良好的视图，以及可以对文件选取，提供 annotation 的特性。"参考 avante.nvim 与 unreal-agent。读完第一稿后："我感觉这里有 2 套交互，一个是侧边栏对话窗口作为总控。一种是类似 gptel 和 gptel-agent 和 gptel-inline 这样，直接在 buffer 上修改的方式。"

模型已经能改任何普通 Buffer，[buffer-first](../implemented/2026-09-18-buffer-first-mutation.md) 又规定改动留在 Buffer、存盘归用户——这正是"待确认的修改"在 Emacs 里的天然形态。不顺的是外面这一层。证据、行号和探针在 `.scratch/cursor-like-integration/REPORT.md`；这里只列结论：

1. **两套交互各有一半，而且各记各的对话。** 从代码里发起的有四个入口：`superchat-act` 进聊天窗当前的对话、回答显示在聊天窗；就地问、就地做、改写选区进每个项目单独的一段 `inline:<项目根>` 对话、回答显示在光标处。在代码里问过、做过的事，聊天窗的对话不知道，反之亦然；而 gptel-inline 让所有就地问答都进同一段对话。
2. **聊天窗把代码挡住。** 打开聊天会新建一个 tab，并默认删掉其他窗口。
3. **指给 AI 看很费劲。** `#file` 只认输入开头的一个绝对路径；发送选区会把不带行号的文本立刻发出去。
4. **AI 改的时候看不见。** 被改的 Buffer 不显示、改动不标出；聊天里的调用显示成 Lisp 列表；Run 结束只有一条 echo area 消息。
5. **只能整个存或整个撤，也没法指着几行提意见。** `/changes` 的 diff 是 `diff-buffer-with-file`，存与撤都作用于整个 Buffer。

## Proposal

**两套交互，一个发动机，一本共用的改动总账。** 发动机就是今天的会话、Run、Tape、工具、权限档位与 buffer-first；总账就是"本会话改过、还没存的 Buffer"（[编辑标记](../implemented/2026-09-19-changes-recognizes-a-buffer-by-its-mark.md) 加 `/changes` 的推导）。两套交互只是两种前端。

### 桥：两套是不是同一段对话（问题 1）

- **同一段**（gptel-inline 的做法）：每个项目一段对话，侧栏和代码里都进它；侧栏里能看到"在 init.el 第 12 行问的"这一轮，并能跳过去；侧栏在跑任务时，代码里的新问题排队等待；可以对某一句选择"不带历史、单独问"。
- **两段**（今天）：代码里继续用项目的 inline 对话；侧栏另加一个总览，列出两边正在跑的任务和还没存的改动。
- 选"同一段"不只是换一个会话编号（代码事实见报告第 9.2 节）：聊天窗今天只实时显示自己发起的 Run，要加"显示别处发起的这一轮"；同一会话已有 Run 在跑时新请求会排队或并入；侧栏对话与来源 Buffer 的关联（`[Emacs origin]`）只由显式入口设置，从另一个 Buffer 就地发起的一轮如何同时出现要另定规则（登记，不在此解决）；M8 的回归测试假定 `inline:<项目根>`。它改变的是会话身份的规则，按"先定本体、再动代码"，须先写进本记录与 [CONTEXT.md](../../../CONTEXT.md) 再实现。

### 甲 · 侧栏总控（参考 avante；dsh 的 diff 卡片、跟随与 `@file`）

- **侧栏布局**：新选项 `superchat-display-style`——`side` 把聊天放在右侧 side window（`display-buffer-in-side-window`），代码留在旁边，一个键开关，不新建 tab、不删除其他窗口；`frame` 是今天的整窗行为。默认值见问题 3。会话 Buffer 的所有权（[ADR 0004](../../adr/0004-session-editor-workspace-authority.md)、[关闭协议](../../protocols/session-editor-workspace-close.md)）不变，侧栏只是显示位置；会话 tab 在 `side` 下如何处理、与 `superchat-plan` 已占用的侧窗口如何分槽，第一片实现前对照确认。
- **聊天里的小 diff 与跟随**：`edit_buffer`、`replace_buffer`、`append_to_buffer` 的调用渲染成 `#+begin_src diff` 块（超过 8 行折叠），标题带"Buffer 名:行号"跳转；只改显示，Tape 与发给模型的内容不变。编辑落到某个 Buffer 后，若它不可见而侧栏旁有代码窗口，就在那里显示它，不选中窗口、不抢焦点、不移动用户正在用的窗口里的光标，可关闭。对应 dsh 的 `DiffCallView`（带 `locations`，"for editor follow-along"）。
- **总控条**：侧栏顶部一行，列出还没存的改动（几个文件、增删行数）和正在跑的任务（含从代码里发起的），点开分别进审阅或跳到那一处。只读已有数据：改动清单来自 `superchat-changes-candidates`，任务来自 Runtime 的 Run 状态。
- **引用标签**：`superchat-add-to-chat` 在输入框插入一个只读小标签（如 `[init.el:12-30]`，可附备注），不发送；标签持有 Buffer 和两个 marker。**发送那一刻**才读取最新文本、带行号展开进用户消息，并随消息在受理时记录（[附件在受理时记录](../accepted/2026-09-16-chat-attachments-recorded-at-admission.md)、[原文完整保留](../implemented/2026-09-17-preserve-conversation-material.md)）。键入的 `#…`、`@…` 规则不变。对应 dsh 的 `@file`。

### 乙 · 就地（参考 gptel、gptel-agent、gptel-inline）

已有：就地问（`superchat-inline-ask`）、就地做（`superchat-inline-act`，可调工具、可改任意 Buffer、回答与"改了哪些 Buffer"显示在光标下方）、改写选区（`superchat-rewrite-region`，预览覆盖层，整段接受或拒绝）。新增三项：

- **改动就地显示、逐块保留或撤回**：结构化编辑落到一个可见的 Buffer 后，按"磁盘文件对 Buffer"计算出各块改动，用覆盖层标出：新行着色，被删的旧行以淡色虚拟行显示在原处（不是 Buffer 文本）。光标在某块上时，一个键保留（只取消标记，改动留在 Buffer 里，存盘仍归你）、一个键撤回（旧文本写回这一块，一次 undo 可还原），另有全部保留、全部撤回。不存盘；Buffer 文本里不出现任何标记字符，所以模型随后读 Buffer 读到的是真实内容。从侧栏发起的改动落到眼前这个 Buffer，也一样就地显示。这就是第一稿问题 2 里"avante 式在代码里取舍"那一项——它是就地这一套自己的审阅，不是 diffs.el 的替代。
- **批准就地回答**（问题 4）：就地做时，工具要批准的问题显示在光标下方的覆盖层里，一个键允许、一个键拒绝，`?` 看完整参数（gptel-inline 的做法）；实现为该入口在现有 `approval/request` 链上的回答者，并且必须守住 2026-09-19 的三条约定：提问之前按下的键不算数、`C-g` 仍是取消、键盘宏不能回答。其他入口仍用 minibuffer。
- **攒着不发**：就地提问时可以先把问题和选中的片段追加进对话、稍后再发（gptel-inline 的 append）；机制与甲的引用标签相同。

### 共用总账：diffs.el 审阅与批注发回（两套都能进）

- 一个命令（暂名 `superchat-review`；侧栏总控条、`/changes`、Run 结束提示、就地做的 `d` 都能进入）把本会话（或本 Run）改过、没存、**且磁盘上已有文件**的 Buffer 放进一个 [diffs.el](https://github.com/LuciusChen/diffs.el) 审阅：每个 Buffer 一个补丁条目（磁盘文件对 Buffer），补丁路径相对于传给 `diffs-items` 的 `:directory`（取这些文件的共同父目录），且不传 `:backend`——这时审阅的根就是 `:directory`。
- 逐块 `A` 保留、`R` 撤回；`C-c C-c` 把撤回的块写回 Buffer，不存盘、不暂存。批处理探针已验证：撤回的块回到 live Buffer，另一块不动，磁盘逐字节不变，Buffer 仍是"已修改"（报告第 5 节）。diffs.el 的 `g` 会重放创建时的补丁，superchat 在审阅里提供"按当前 Buffer 重建"。
- "改之前"以磁盘文件为准（今天 `/changes` 的规则，"保留"的最后一步就是存盘），与 buffer-first 已接受的推导与局限一致；就地显示用同一个基准，两处看到的改动一致。
- **批注发回**：在审阅里用 diffs.el 的 `v`（选行）与 `a`（写批注）；也可以把没改过的文件放进来专门批注（探针验证了未改动行上的批注能读回文件、行号与内容）。在审阅里按一个键（暂定 `C-c C-s`），把用户写的批注逐条展开为文件、旧/新侧与行号、那几行原文、摘要与理由，作为同一会话的下一条用户消息提交；发出后清除这些批注并按当前 Buffer 重建审阅。模型往审阅里写自己的批注不在本提案内。
- 不进 diffs.el 审阅、保持今天处理的两类：磁盘上还没有的新文件（撤回"整文件新增"的行为未验证）与不访问文件的 Buffer（diffs.el 写回要求有源文件）。没装 diffs.el 时，全部退回今天的 `diff-buffer-with-file` 与整 Buffer 存/撤。
- dsh 没有逐块审阅与带行号的批注；需要来自 superchat 自己的 buffer-first，这是对 `/changes` 的升级，不是新子系统。

### Token 约束（贯穿全部）

- 不新增模型工具，不往系统消息里加随请求变化的文字。侧栏、跟随、就地显示、审阅对模型 token 为零；标签与批注只增加用户实际发送的原文片段，且在发送那一刻取快照——不学 gptel 每次请求重读的"活的上下文"，因为实测的 88%–93% 缓存命中依赖"消息只追加"（报告第 6 节）。
- 每一片的回归都断言：同一 Run 相邻请求的前缀逐字相同，系统消息与工具定义与改动前一致。
- 选"同一段对话"后，代码里的每个问题都会带上这段对话的历史，token 比今天的 inline 对话多；多出的部分大多能命中缓存，并可对单句选择不带历史。
- `read_buffer` 无上限另见 `.scratch/issues/2026-09-24-read-buffer-has-no-window-or-cap.md`；一次调用改多处另立提案。

### 不照搬的

- avante：默认全部自动批准；"接受"时直接存盘（`noautocmd write!`）；选区编辑不预览、边流边写进 Buffer（`superchat-rewrite-region` 的预览更严，保留）；每处改动停下来等人确认。
- gptel-agent：每次编辑先确认、确认后直接写磁盘（`write-region`）。superchat 的改动直接进 Buffer、事后审阅；只借用它"在对话里用 -/+ 显示改动"的做法，这与 dsh 的 diff 卡片一致。
- gptel：把回答直接写进你的 Buffer 当作对话的默认做法；就地回答仍在覆盖层里，是否提供"把这段回答插到这里"由用户显式操作，不作默认。

### 分片

每片先写红测试（各自独立的测试文件，不进 `scripts/m3-regression.el`），单独验收，leader 提交；顺序取决于问题 2，建议：
1. 改动看得见：就地显示与逐块保留/撤回，加上聊天里的小 diff（两套都用得上）。
2. 侧栏布局、跟随、总控条。
3. 同一段对话（若问题 1 选"同一段"）。
4. diffs.el 审阅与批注发回。
5. 就地批准、攒着不发与引用标签。
第 1、3、4 片完成后各用 DeepSeek 做一次有人值守的真实模型验收。

## 待用户决定

1. **两套是不是同一段对话**：(A，建议——"总控"与 gptel-inline 合起来就是这个意思) 同一段：每个项目一段，侧栏和代码里都进它，侧栏能看到在代码里发生的每一轮；(B) 维持两段，侧栏只加一个总览。
2. **先做哪套**：(A，建议) 先做两套都用得上的"改动看得见、能就地逐块取舍"；(B) 先做侧栏；(C) 先做"同一段对话"。
3. **侧栏的默认布局**：用户 2026-09-10 定义的场景一是"像 ChatGPT 桌面应用的聊天窗"；选 A 就是把默认改成代码旁的侧栏，整窗降为选项，场景一的定义随之修改。(A，建议) 默认侧栏，整窗可选；(B) 默认整窗，侧栏可选；(C) 只要侧栏。
4. **就地做时的批准在哪里回答**：(A，建议) 在光标下方的覆盖层里，其他入口仍用 minibuffer；(B) 全部仍在 minibuffer。

以下三项按建议执行，除非用户另有意见：审阅以磁盘文件为"改之前"；批注在审阅里按一个键才发送；diffs.el 为可选依赖，没装就退回今天的 diff。

## Alternatives considered

**只做一个 Cursor 式循环**（本记录第一稿）。把就地这一套只当作"就地做的 `d` 键也进审阅"，漏掉了 gptel 一系那种"在哪里都能和同一段对话说话、改动就在眼前取舍"的体验，也把在代码里取舍与 diffs.el 审阅错当成二选一。

**自己写多文件审阅界面**。重复一个有人维护的包：diffs.el 已有逐块取舍、写回不存盘、跨布局稳定的选区与批注、给 agent 读的接口；superchat 保留 diff-mode 退路，所以依赖不是单点。

**每处改动先确认再应用**（avante 的确认框、gptel-agent 的 `:confirm t` 预览）。一个任务里模型每改一处都要停下，按"问人时时钟停"的规则会停很多次；而 buffer-first 已经让"待确认"有了天然形态——未存的 Buffer——事后逐块取舍即可。

**改动先不进 Buffer，放影子副本或补丁队列**。违背 buffer-first：modified 标记就是"待确认"的信号；模型要 `eval-buffer` 让改动生效、用户的钩子要运行，都需要文本在 Buffer 里。

**就地显示借用 diffs.el 的冲突模式或 diff-hl**。冲突模式要往 Buffer 里插冲突标记，会改变模型读到的文本，也会让 `eval-buffer` 出错；diff-hl 以版本库里的版本为基准，不是磁盘文件，版本库外的文件也覆盖不到。就地显示用只读覆盖层自己画，基准与审阅相同。

**ediff 或 smerge**。ediff 是两 Buffer 对照、流程重、没有批注；smerge 要插冲突标记，单个选区的这条路 `superchat-rewrite-merge` 已经提供。

**走 diffs.el 的 CLI 与技能**。superchat 与 diffs.el 在同一个 Emacs 里，直接调 Elisp 函数更简单，不经 socket 与 JSON。

**活的上下文**（gptel-add 每次请求重读）。会让每一轮请求的前部内容变化，破坏实测的只追加前缀与缓存命中；快照在发送时写进用户消息，既可复现也可审计。

**批注随下一条消息自动带上**。用户不容易知道批注什么时候被交出去、交了哪些。

**保持整窗聊天，另开 frame 看代码**。今天的变通；失去"一个 frame 里代码与聊天并排"，也无法跟随。

## Acceptance criteria

用户旅程（脚本化 transport 回归逐片覆盖；第 1、3、4 片另有 DeepSeek 有人值守验收）：

1. **改动看得见**：就地做改了眼前函数里的两处：两处在代码里标出（新行着色、旧行以淡色虚拟行显示），聊天或覆盖层里有对应的小 diff；撤回一处，旧文本回到这一块、Buffer 仍未存；保留另一处，标记消失、改动仍在 Buffer 里；Buffer 文本里没有任何标记字符，模型随后 `read_buffer` 读到的是真实内容；从侧栏发起、落到这个可见 Buffer 的改动同样标出。
2. **侧栏**：从文件 Buffer 打开聊天，聊天在右侧、代码仍可见、没有新 tab；开关键能隐藏和恢复；改动落到不可见的 Buffer 时它出现在代码窗口，光标与选中窗口没被移动；总控条列出还没存的改动和正在跑的任务；`frame` 设置下行为与今天一致（canonical smoke 不变）。
3. **同一段对话**（若选 A）：在 init.el 就地问一句，侧栏里出现这一轮并能跳回 init.el 那一行；侧栏在跑任务时就地问，问题显示为等待并在前一个 Run 结束后执行；选"单独问"时请求里没有这段对话的历史；`/changes` 与总控条列出就地做改过的 Buffer。
4. **审阅与批注**：审阅里两处各为一块；撤回一块、保留一块、`C-c C-c`：Buffer 只含保留的那处且仍未存；在审阅里选两行写"这里用 let"并发送：下一次请求里有文件、行号、那两行原文和这句话；AI 改完后重建审阅能看到新改动。新文件与不访问文件的 Buffer、以及没装 diffs.el 时，与今天一致。
5. **就地批准**（若问题 4 选 A）：就地做调用 `eval_elisp` 时，问题与参数显示在覆盖层，一个键允许、一个键拒绝；提问之前按下的键不能回答；`C-g` 取消；键盘宏不能回答（与 `scripts/approval-input-regression.el` 的约定一致）。
6. **引用标签与攒着不发**：选一段加入聊天并附备注，再打一句问题发送：请求里有 Buffer、文件、行号、带行号原文和备注；按发送之前没有任何请求发出；发送后原文再被改，已发出的内容不变。
7. **不增 token**：每片的回归断言同一 Run 相邻请求前缀逐字相同，系统消息与工具定义与本提案之前一致。

## Risks

- **同一段对话**改变会话身份的规则：聊天窗要实时显示别处发起的 Run；忙时排队会让就地问答变慢；侧栏对话与来源 Buffer 的关联规则要另定；M8 的回归需要随之修改。
- diffs.el 还年轻（0.x，不在 ELPA/MELPA，用 `package-vc` 安装），接口可能变：只用 README 公开的函数，集成代码集中在一个文件，作为可选依赖。
- diffs.el 默认旧侧来自版本库：新侧它优先读 live Buffer，旧侧的上下文展开（`e`）与旧侧语法着色要走 VC 后端；本方案不传 `:backend`，这两项可能不可用或退回无着色显示，没有验证。审阅的根只有在不传 `:backend` 时才等于 `:directory`（批处理探针三种目录布局都通过，交互环境未验证）。
- 就地显示与用户自己的编辑交错：用户在带标记的块里打字时要重新计算，标记绝不能进入 Buffer 文本；就地撤回与用户的 undo 历史要能共存。
- 就地批准必须原样守住批准的三条约定；覆盖层不可见（滚走、换窗口）时问题仍要能被看到和回答。
- 跟随会打扰：规则写死"不选中窗口、不移动用户正在使用的窗口的光标"，并可关闭。
- 若默认改为侧栏（问题 3 选 A），已接受的聊天入口验收是在整窗布局下做的，需在新布局下重跑。
- 批注能否真正减少模型的来回，没有测量；第 4 片的有人值守验收记录请求数与 token，与报告第 6 节的基线对照。
