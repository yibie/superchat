# Decision: a Cursor-style editing loop — side panel, visible edits, block review with diffs.el, line comments back to the model

Status: proposed — Claude's recommendation of 2026-09-24, after the user asked whether superchat's Emacs integration is smooth enough and how a Cursor-like side panel that changes files and selections should work, using diffs.el for the change view and for annotations; the five questions in "待用户决定" are open.

## Problem

用户原话（2026-09-24）："superchat 与 emacs 的结合，是否足够顺滑；如果 superchat 能够和 cursor 一样，在侧边栏指挥 emacs 修改对应的文件，和选取，应当如何；希望可以用 diffs.el 为修改文件提供良好的视图，以及可以对文件选取，提供 annotation 的特性。"参考：avante.nvim（Cursor 般的体验）、unreal-agent（节省 token）。

调查结论：模型已经能改任何普通 Buffer，[buffer-first](../implemented/2026-09-18-buffer-first-mutation.md) 又规定改动留在 Buffer、存盘归用户——这正是 Cursor 里"待确认修改"在 Emacs 里的天然形态。不顺的是外面这一层，五处都是代码事实（证据、行号和探针在 `.scratch/cursor-like-integration/REPORT.md`）：

1. **聊天窗把代码挡住。** `M-x superchat` 为会话选择或新建一个 tab，并在 `superchat-display-single-window`（默认 `t`）下删掉其他窗口。
2. **指给 AI 看很费劲。** `#file` 只认输入开头的一个绝对路径；`superchat-send-region` 把选区文本（不带行号）拼进输入框并立刻发送。
3. **AI 改的时候看不见。** `edit_buffer` 不显示、不标出改动；聊天里的调用显示成 `("init.el" "旧" "新")` 这样的 Lisp 列表；Run 结束只有一条 echo area 消息。
4. **只能整个存或整个撤。** `/changes` 的 diff 是 `diff-buffer-with-file`，存与撤都作用于整个 Buffer。
5. **没法指着几行提意见。** 只能用文字重新描述位置，模型往往整段重读才能找到。

## Proposal

把 Cursor 的循环翻译成 Emacs：**指给它看 → 让它做 → 看它改 → 逐块取舍 → 指着行提意见 → 再来；存盘始终归你。** 五个部分，各自可单独交付，顺序即分片顺序。

### 1. 侧栏

- 新选项 `superchat-display-style`：`side` 把聊天 View 放在右侧 side window（`display-buffer-in-side-window`），代码留在旁边，一个键开关，不新建 tab、不删除其他窗口；`frame` 是今天的整窗行为，`superchat-display-single-window` 只在 `frame` 下有意义。默认值见问题 1。
- `side` 下侧栏显示当前会话的 View，切换会话就是换侧栏里的 Buffer。会话 Buffer 的所有权（[ADR 0004](../../adr/0004-session-editor-workspace-authority.md)、[关闭协议](../../protocols/session-editor-workspace-close.md)）不变：侧栏只是显示位置。会话 tab 在 `side` 下怎么处理，第一片实现前对照这两份文档确认。
- DeepSeek Harness 没有对应（它不是编辑器）；布局参考 avante 的右侧侧栏。

### 2. 看它改

- **聊天里的小 diff。** `edit_buffer`、`replace_buffer`、`append_to_buffer` 的调用在聊天里渲染成 `#+begin_src diff` 块（旧→新，超过 8 行折叠），标题带"Buffer 名:行号"跳转；只改显示，Tape 与发给模型的内容不变。对应 dsh 的 `DiffCallView`/`DiffResultView`（聊天里同样最多 8 行）。
- **跟随。** 结构化编辑成功后，被改的 Buffer 如果可见，改动的行闪一下（`pulse`）；如果不可见而侧栏旁有代码窗口，就在那个窗口显示它。规则：不选中窗口、不抢焦点、不移动用户正在使用的窗口里的光标；可以关掉；`frame` 下只有聊天里的 diff 和跳转。对应 dsh `DiffCallView.locations`（"for editor follow-along"）。
- 两者都只读 `tool.completed` 已有的目标字段和 [Buffer 自身的编辑标记](../implemented/2026-09-19-changes-recognizes-a-buffer-by-its-mark.md)，不新增 Tape 事件。

### 3. 逐块取舍：diffs.el 审阅

- 一个命令（暂名 `superchat-review`；`/changes`、Run 结束提示、inline act 的 `d` 都能进入）把本会话（或本 Run）改过且未存、并且**已在磁盘上有文件**的 Buffer 放进一个 [diffs.el](https://github.com/LuciusChen/diffs.el) 审阅：每个 Buffer 一个补丁条目（磁盘文件对 Buffer）。补丁里的路径相对于传给 `diffs-items` 的 `:directory`，且不传 `:backend`：这时 diffs.el 的审阅根就是 `:directory` 本身（它用 `vc-root-dir` 定根，而 `vc-root-dir` 在 diff 类 Buffer 里读的是 `diff-vc-backend`，不传就为空）。`:directory` 取这些文件的共同父目录。
- 逐块 `A` 保留、`R` 撤回、`U` 重置；`C-c C-c`（`diffs-review-apply-decisions`）把撤回的块写回 Buffer，不存盘、不暂存。存盘仍是用户的 `save-buffer` 或 `superchat-changes-save`。批处理探针已验证：撤回的块回到 live Buffer，另一块不动，磁盘逐字节不变，Buffer 仍是"已修改"（报告第 5 节）。
- 基线见问题 3，建议保持今天的规则：和磁盘文件比，"保留"的最终确认就是存盘。这与 buffer-first 已接受的 `/changes` 推导与局限一致。
- diffs.el 的 `g` 重放创建时的补丁（`diffs.el:12105`），所以 superchat 在审阅里提供自己的"按当前 Buffer 重建"键。
- 不放进 diffs.el 审阅、保持今天处理的两类：磁盘上还没有的新文件（diffs.el 对"整文件新增"撤回后的行为没有验证），以及不访问文件的 Buffer（diffs.el 写回时要求有源文件）。它们照旧在 `/changes` 里列出，照旧可以显示、整体保存或放弃（`superchat-changes-revert` 对新文件是放弃并关闭这个 Buffer）。
- 没装 diffs.el：全部退回今天的 `diff-buffer-with-file` 与整 Buffer 存/撤。
- dsh 没有逐块审阅（改完即落盘，diff 卡只是展示）。需要来自 superchat 自己的 buffer-first；这是对 `/changes` 的升级，不是新子系统。

### 4. 指着行提意见：批注发回给 AI

- 在审阅里用 diffs.el 自己的 `v`（选行）和 `a`（写批注）。审阅里也可以放进一个没改过的文件（例如当前 Buffer）专门用来批注——探针验证了未改动行上的批注能读回 `(:file "guide.txt" :new-range (4 4) :summary …)`。
- superchat 在审阅里加一个键（暂定 `C-c C-s`）："把我的批注发给 AI"。发送时读 `diffs-review-annotations`，只取用户写的；每条展开成：文件、旧/新侧与行号、那几行原文（新侧取当前 Buffer，旧侧取磁盘文件）、摘要与理由。作为同一会话的下一条用户消息走普通聊天提交，所以它和用户打的字一样在受理时记录（[附件在受理时记录](../accepted/2026-09-16-chat-attachments-recorded-at-admission.md)、[原文完整保留](../implemented/2026-09-17-preserve-conversation-material.md)）。发出后这些批注从审阅中清除，审阅按当前 Buffer 重建。
- 让模型往审阅里写它自己的批注（diffs.el 支持）不在本提案内，以后单独问。
- dsh 部分对应：`@file` 与会话引用作为用户消息进入历史；没有带行号的批注。

### 5. 引用标签：在任意 Buffer 里指给 AI 看

- 命令暂名 `superchat-add-to-chat`：有选区用选区，没有就用当前函数；在聊天输入框插入一个只读小标签，如 `[init.el:12-30]`，可附一句备注，不发送。标签持有 Buffer 和两个 marker，Buffer 之后被改也跟着走。
- 发送时才读取并展开：Buffer 名、文件、行号范围、带行号的最新原文、备注。键入的 `#…`、`@…` 规则不变（仍只认开头）；标签是另一种东西，不改语法。
- `superchat-send-region` 保持"立刻发送"不变；选区进入模型的三条现有路径本提案不合并。
- 对应 dsh 的 `@file` 提及（`packages/context/file-reference`）。

### Token 约束（贯穿五片）

- 不新增模型工具，不往系统消息里加随请求变化的文字。侧栏、跟随、审阅对模型 token 为零；标签与批注只增加用户实际发送的原文片段。
- 实测：一个 Run 内请求只追加、系统消息与工具定义不变，DeepSeek 缓存命中 88%–93%（报告第 6 节）。每一片的回归都断言"同一 Run 相邻请求的前缀逐字相同、系统消息与工具定义与改动前一致"。
- `read_buffer` 无上限另见 `.scratch/issues/2026-09-24-read-buffer-has-no-window-or-cap.md`；一次调用改多处另立提案，都不在本提案内。

### 不从 avante 照搬的

- 默认全部自动批准（avante `auto_approve_tool_permissions = true`）：权限档位与批准照旧。
- "接受"时直接存盘（avante 确认后 `noautocmd write!`）：与 buffer-first 冲突。
- 选区编辑不预览、边流边写进 Buffer：`superchat-rewrite-region` 的预览覆盖层更严，保留。
- 每次改动都停下来等人确认：改动照旧直接进 Buffer，事后审阅；这也是 Cursor 和 diffs.el 的模型。

### 分片

1. 侧栏；2. 看它改；3. diffs.el 审阅；4. 批注发回；5. 引用标签。每片先写红测试（各自独立的测试文件，不进 `scripts/m3-regression.el`），单独验收，leader 提交；第 2–4 片完成后用 DeepSeek 做一次有人值守的真实模型验收。

## 待用户决定

1. **布局默认值**：用户 2026-09-10 定义的场景一是"像 ChatGPT 桌面应用的聊天窗"；选 A 就是把默认改成"代码旁边的侧栏"，整窗降为一个选项，场景一的定义随之修改。(A，建议) 默认侧栏，整窗保留为选项；(B) 默认仍整窗，侧栏为选项；(C) 只要侧栏。
2. **在哪里审阅**：(A，建议) 先做 diffs.el 审阅窗口；(B) 先做 avante 式、直接在代码 Buffer 里显示旧行和保留/撤回键；(C) 两个一起做。
3. **"改之前"以什么为准**：(A，建议) 磁盘上的文件（今天的规则，"保留"即存盘）；(B) AI 第一次改这个 Buffer 之前的文本（只存在内存里、挂在 Buffer 上），存盘后也能看到 AI 改了什么。
4. **批注怎么交给 AI**：(A，建议) 在审阅里按一个键才发送；(B) 随下一条聊天消息自动带上。
5. **diffs.el 是否必需**：(A，建议) 可选，没装就退回今天的 diff；(B) 必需依赖。

## Alternatives considered

**自己写一套审阅界面**（覆盖层、逐块按键）。输在重复一个有人维护的包：diffs.el 已经有逐块取舍、写回不存盘、跨布局稳定的选区与批注、给 agent 读的接口；superchat 仍保留 diff-mode 退路，所以依赖不是单点。

**avante 式在代码 Buffer 里逐次确认。** 新行写进 Buffer、旧行显示为幽灵行、每处改动停下等人确认。不作为第一步：一个任务里模型每改一处都要停，按"问人时时钟停"的规则会停很多次；按键与幽灵行装在用户正在编辑的 Buffer 里，Run 进行中容易和用户自己的操作冲突。它作为问题 2 的选项 B 保留，可在审阅窗口之后再做。

**改动先不进 Buffer，放影子副本或补丁队列，接受后才写入。** 违背 buffer-first：待确认修改就是未存的 Buffer，modified 标记就是信号；模型用 `eval-buffer` 让改动生效、用户的钩子运行，都需要文本在 Buffer 里。

**用 Emacs 自带的 ediff 或 smerge。** ediff 是两个 Buffer 对照、流程重、没有批注；smerge 要往 Buffer 里插冲突标记，而单个选区的这条路 `superchat-rewrite-merge` 已经提供。

**走 diffs.el 的 CLI 与技能**（外部 agent 的方式）。superchat 与 diffs.el 在同一个 Emacs 里，直接调 Elisp 函数更简单，不经过 socket 与 JSON。

**批注随下一条消息自动带上。** 用户不容易知道批注什么时候被交出去、交了哪些；它是问题 4 的选项 B。

**保持整窗聊天，另开 frame 看代码。** 今天的变通做法；失去"一个 frame 里代码与聊天并排"，也无法跟随。

## Acceptance criteria

用户旅程（脚本化 transport 回归逐片覆盖；第 2–4 片另有 DeepSeek 有人值守验收）：

1. **侧栏**：从文件 Buffer 打开聊天，聊天在右侧、代码仍可见、没有新 tab；开关键能隐藏和恢复；`frame` 设置下行为与今天逐字节一致（canonical smoke 不变）。
2. **看它改**：让 AI 在可见文件里改两处：两处在代码窗口闪过；聊天里出现两个 diff 块，回车跳到对应行；没有任何存盘；光标和选中窗口没被移动。
3. **取舍**：审阅里两处各为一块；撤回一块、保留一块、`C-c C-c`：Buffer 只含保留的那处且仍未存；存盘后磁盘只含那处。新文件与不访问文件的 Buffer 不进审阅、处理与今天一致；没装 diffs.el 时 `/changes` 与今天一致。
4. **提意见**：在审阅里选两行写"这里用 let"，按发送键：下一次请求里有文件、行号、那两行原文和这句话；AI 改完后重建审阅能看到新改动。
5. **引用标签**：在任一 Buffer 选一段，加入聊天并附备注，再打一句问题发送：请求里有 Buffer、文件、行号、带行号原文和备注；按发送之前没有任何请求发出。
6. **不增 token**：每片的回归断言同一 Run 相邻请求前缀逐字相同，系统消息与工具定义与本提案之前一致。

## Risks

- diffs.el 还年轻（0.x，不在 ELPA/MELPA，用 `package-vc` 安装），接口可能变：只用 README 公开的函数，集成代码集中在一个文件，作为可选依赖。
- diffs.el 用 `vc-root-dir` 或 `default-directory` 定审阅的根（`diffs.el:6802-6806`）；不传 `:backend` 时 `vc-root-dir` 为空，根就是 `:directory`，补丁路径必须相对于它。批处理探针在普通目录、仓库根、仓库子目录三种情况下都通过，交互环境没有验证。
- diffs.el 默认旧侧来自版本库里的某个版本：新侧它优先读 live Buffer（`diffs--worktree-lines`），旧侧的上下文展开（`e`）和旧侧语法着色要走 VC 后端（`diffs--revision-lines`）。本提案的旧侧是磁盘文件、又不传 `:backend`，这两项可能不可用或退回无着色显示，没有验证。
- `superchat-plan` 已经占用一个侧窗口（计划审阅与进度）；两个右侧面板需要约定槽位，第一片一起定。
- 跟随会打扰：规则写死"不选中窗口、不移动用户正在使用的窗口的光标"，并可关闭。
- 侧栏与会话 tab、EditorWorkspace 所有权的配合没有设计验证；第一片实现前对照 ADR 0004 与关闭协议。
- 若默认改为侧栏（问题 1 选 A），已接受的聊天入口验收是在整窗布局下做的，需在新布局下重跑。
- 批注能否真正减少模型的来回，没有测量；第 4 片的有人值守验收记录请求数与 token，与报告第 6 节的基线对照。
