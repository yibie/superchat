# Decision: M11 第 1 片——认定 workspace，对话归属于它

Status: accepted — 用户，2026-10-02："现在可以规划，然后实现"。下文"leader 的选择"一节里的机制是 leader 在这句话之下定的，随本片一起向用户报告。

## Problem

[workspace 契约](2026-10-01-workspace-work-record-session-rules.md)的 W1–W7、W10、S1、S2 还没有任何代码：一段对话只能用 `/workspace <目录>` 手动绑定到目录；对话窗口显示哪段对话取决于 tab，不取决于文件在哪；就地命令进对话窗口正在显示的那段。`superchat-workspace.el` 这个文件名和 `superchat-workspace-` 这个前缀被 Artifact Board 占着。

## Decision

本片分六个小片，按序进行；每个小片先写会失败的测试，再改代码，由 leader 亲自验证后提交。

| 小片 | 内容 | 契约条目 |
|---|---|---|
| 1.0 | 改名：`superchat-workspace.el` 改为 `superchat-artifact-board.el`，`superchat-workspace-` 前缀的函数和变量改为 `superchat-artifact-board-`，两个用户选项留废弃别名。行为不变 | 术语 |
| 1.1 | 新的 `superchat-workspace.el`：由目录或 buffer 得出 workspace（根目录、身份、名字）；读 `.superchat/id`；把目录声明为 workspace 的命令；挂在 `project-find-functions` 上的认定函数 | W1、W3、W4、W6 |
| 1.2 | 对话在创建时记下它的 workspace，之后不改；`/workspace <别的目录>` 在已有内容的对话里是在那边新开一段 | S1 |
| 1.3 | 从 project 里的文件叫出对话窗口、`superchat-act`、就地问、就地做、改写选区，进的都是该 workspace 的主对话 | W2、S2 |
| 1.4 | 对话窗口的标题写 workspace 的名字；对话独占的 tab 归入同名的 tab 组 | W7 |
| 1.5 | 不在任何 project 里时用临时 workspace：每次临时使用各一段对话，标题写明"临时"，tab 不归组 | W10 |

验收：`scripts/workspace-regression.el`；现有各回归、M3 回归、内核检查、canonical smoke 不变或只按本记录列出的方式变化；一个在两个 project 之间来回切换的真实任务，两边的请求里都没有对方的历史。

## leader 的选择

1. **先改名再建新模块。** `superchat-workspace-` 这个前缀要留给新概念，所以 1.0 先把 Artifact Board 的 Lisp 标识符改掉。给模型看的工具名（`workspace_read` 等）、Tape 事件名（`workspace.bound` 等）、插件行名在本片不改：它们是已保存的预设和历史记录里的数据。Session Buffer Set 的改名不在本片。
2. **身份文件不自动写。** 目录里有 `.superchat/id` 就用它；没有就沿用现在的算法（真实路径的哈希），这样已绑定的旧对话不变。只有用户执行声明命令（`superchat-workspace-declare`，或 `/workspace <目录>`）时才写这个文件，写入的是当时正在用的那个 id，所以此后改名、搬家都认得，也不需要迁移旧对话。"第一次说'记住'时写"留给第 2 片。
3. **认定函数排在版本库之后。** `project-find-functions` 上的函数只在 Emacs 自己认不出 project 的目录里起作用（那里有 `.superchat/` 才算），不接管版本库 project，所以 `C-x p f` 等在版本库里的行为不变。仓库子目录里的 `.superchat/` 不另成 workspace（W6）。它由插件挂载时加上，卸载时去掉。
4. **主对话是这个 workspace 最近活动的那段。** 不另存"主对话"指针：从 Tape 上找绑定了这个 workspace 的对话，取最近活动的一段；新开一段对话后，它就是主对话。子代理的对话不算。
5. **1.3 只改 project 里的行为，临时的放到 1.5。** 不在任何 project 里的 buffer 在 1.3 之后仍按现在的办法进对话，1.5 再换成临时 workspace。临时使用的界线：从没有 project 的 buffer 叫出时，这个 tab 旁边已经显示着一段临时对话就接着用，否则新开一段；重启 Emacs 后不自动接回，但历史列表里翻得到。

## 实现中定下的两点

- **在已绑定 workspace 的对话 View 里调用 `superchat`，停在这段对话。** 1.3 之后这类 View 不再是名为 `superchat-buffer-name` 的那个 buffer，老路径会另开一段没有绑定的对话；worker 的守卫测试测出后由 leader 定。临时对话的 View 同理（1.5）。
- **临时使用的界线是"到这段对话的 buffer 被关掉为止"**，取代上文第 5 点里"这个 tab 旁边显示着才接着用"。原界线下，`superchat-submit` 先后两次取对话会取到两段（`superchat--ensure-request-chat-buffer` 在建请求和执行请求时各调用一次），四套已有回归和 canonical smoke 因此变化。

## Alternatives considered

**新模块另起一个不冲突的名字，改名以后再做。** 输在 `superchat-workspace-` 前缀下会同时有两个模块的函数，读代码的人和 agent 都分不清。

**在每个 project 里第一次叫出对话时就写 `.superchat/id`。** 输在这会不经用户同意往他的目录里写文件，版本库里还会多出一个未跟踪的目录。

**认定函数排在版本库之前，让 `.superchat/` 总是赢。** 输在它会让整个 Emacs 把该仓库当成普通目录来列文件，忽略规则失效。

**把 `.superchat` 加进 `project-vc-extra-root-markers`。** 这是 Emacs 自带的标记办法，也能让子目录另成 project；输在它是用户选项，由 superchat 去改它的值比挂一个钩子函数更越界。

**为"主对话"存一个指针。** 输在多一处要与 Tape 保持一致的状态；最近活动这个规则不需要它。

## Consequences

- 从 project 里的文件叫出的对话不再是 tab 上记着的那段，而是这个 workspace 的主对话；[2026-09-24 记录](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)第 5 条随 1.3 被取代，其回归只在 project 内的情形改变。
- superchat 加载后 `project-find-functions` 多一个函数；含 `.superchat/` 的非版本库目录在整个 Emacs 里成为 project。
- `superchat-workspace-fallback-buffer`、`superchat-workspace-auto-create` 两个用户选项改名，旧名作为废弃别名保留。
- 在本片之前创建的对话没有 workspace，它们不是任何 workspace 的主对话，仍可从历史里打开。
