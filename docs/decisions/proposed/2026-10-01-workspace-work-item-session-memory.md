# Decision: workspace、work item、session、记忆——四个对象各管什么

Status: proposed — 契约草稿。方向来自用户 2026-10-01 的四条裁定和 "1C 2A 3A"；对象的具体形状是本记录的提议，逐条待用户确认。不含代码改动。

## 一句话

把"在哪儿做"（workspace）、"做哪件事"（work item）、"谁在做时的那段对话"（session）、"这个地方记住了什么"（记忆）分成四个对象。记忆和待办只属于一个 workspace，不存在到处都生效的那一层；每个 agent 用自己的对话，互相只传结论；不在任何 project 里的临时使用，各自落在一个用完即弃的临时 workspace 里。

## Problem

2026-10-01 的目标批次（`.scratch/task-sim/GOALS-BATCH-REPORT.md`）里，"个人助理"一组没有达到：说过的偏好重启后不在（P1，0/3），提醒不可靠（P3，1/3）。直接的修法是加一块记忆、加一个提醒工具，但两者都要先回答"它属于谁"：

- 记忆如果到处生效，写日记的规矩会进到代码仓库的对话里。用户：全局偏好记忆"极容易混淆"，不做。
- 提醒、子代理交接、跨重启继续，都需要一个比"一段对话"活得久、又比"整个 Emacs"小的东西来挂。用户：沿 work item 的 ID 继续探讨（[路线图的 work item 备注](../../roadmap.md)，Holon 的 [work items](https://holon.run/spec/work-items) 与 [wake and continuation](https://holon.run/spec/wake-and-continuation)）。
- 多个 agent 把各自的过程写进同一段对话会互相污染上下文。用户：不允许所有 agent、subagent 共享 session。
- 这些对象都需要一个"地方"来归属。用户：先发明 workspace；临时使用要有"临时 workspace"；实验阶段用 Emacs 的 project 充当 workspace。

## 现状（代码事实，2026-10-01 读源码核实）

| 事实 | 出处 |
|---|---|
| 已有"把一段对话绑定到一个目录"：`/workspace <目录>` 生成 `workspace-<目录真实路径哈希前 12 位>`，记为 Tape 事实 `workspace.bound`，重开对话时恢复（目录还在才恢复）；它设定工具的默认目录，并把 `workspace-id` 带进每个请求和 Tape 事件的信封。只能手动绑定，按对话绑定，可随时改绑或清除 | `superchat.el` `superchat--bind-workspace`、`superchat--cmd-workspace`、`superchat--restore-session-runtime-configuration`；`superchat-tape.el` `superchat-tape--envelope-keys` |
| 除了协作白板（Artifact Workspace，`superchat-workspace.el`）和工具默认目录，没有别的东西按这个 id 划分：记忆、回忆、对话列表、任务都不看它 | `superchat-memory.el`、`superchat-db.el` 的 `memory` 表（无 workspace 列）、`superchat-dispatcher.el` `superchat--attach-indexed-memories` |
| 对话窗口显示哪段对话，取决于当前 tab 或主对话 buffer，不取决于文件在哪个目录；就地问、就地做、改写选区进对话窗口正在显示的那段 | `superchat.el` `superchat`；[2026-09-24 记录](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)第 5 条 |
| 子代理已经用自己的对话：会话 id 为 `subagent-<角色>-<时间>-<随机>`，构造提示时不带主对话历史，只带任务文字和主 agent 明确交给它的一段背景；交回的是一份报告 | `superchat-subagent.el` `superchat--subagent-make-context`、`superchat--subagent-run-async` |
| 已有带 id 的"团队任务"：`T<n>`，有标题、状态（pending / in_progress / completed）、负责人、依赖；每次委派自动建一条并由子代理认领，完成后置为 completed。记在主对话的 Tape 上（`team/task`），只在一段对话内有效，没有等待条件，也不跨对话 | `superchat-subagent.el` `superchat-team-task`、`superchat--team-start-delegated-task` |
| 子代理工具（委派、团队任务、信箱）不在默认工具列表里，要 `/team` 才给模型 | `superchat-tools.el` `superchat-llm-default-tool-names`；`superchat.el` `superchat--apply-topology-overlay` |
| 记忆：只有 `/remember` 写入能跨对话被自动带上的记录；说"记住……"不写任何东西；模型没有写记忆的工具；回忆的查询对中文整句基本匹配不上 | `superchat.el` `superchat--cmd-remember`；`.scratch/issues/2026-10-01-recall-never-matches-a-chinese-sentence.md` |
| 没有任何"到点自己做事"的机制；计划（`emit_plan`）属于一段对话，没有 id 级的等待 | `superchat-plan.el` |

所以"workspace"不是从零发明：身份、Tape 上的记录和信封字段已经在，缺的是自动认定、临时 workspace，以及让记忆、待办、对话都认它。

## Proposal

### 四个对象

| 对象 | 是什么 | 身份 | 归属 | 寿命 |
|---|---|---|---|---|
| **Workspace** | 一个工作的地方 | 沿用现有的：目录真实路径的哈希 | — | 目录在就在 |
| **临时 workspace** | 不在任何 project 里时落到的地方 | 每次临时使用新生成一个，不对应目录 | — | 见"临时 workspace" |
| **Work item** | workspace 里的一件事 | 自己的 id | 恰好一个 workspace；有一个负责的 agent | 从创建到完成 |
| **Session** | 一个 agent 做事时自己的那段对话 | 现有的会话 id | 恰好一个 workspace，创建时定下 | 现有规则 |
| **记忆** | 这个 workspace 记住的事 | 现有的 Tape 记录 | 恰好一个 workspace | 到被用户删除为止 |

### Workspace 怎么认定（用户："2A"）

1. 从一个文件 buffer 叫出 superchat（对话窗口、就地问、就地做、改写选区）时：这个文件所在的 Emacs project 的根目录就是 workspace。Emacs 默认把版本库（如 git 仓库）的根当作 project。
2. 不在任何 project 里（没有文件的 buffer，或文件不在版本库里）：临时 workspace。
3. 对话窗口上写明当前是哪个 workspace，临时的也写明"临时"。
4. 现有的 `/workspace <目录>` 留作手动指定：把一个不是 project 的目录当作 workspace。

### 临时 workspace（用户："1C"）

- 每次临时使用各开一个，互不相通。
- 没有记忆区：在里面让模型"记住"，得到的答复是"这是临时 workspace，不保存"。
- 它里面的东西不会被带进任何别的对话：不参与回忆，不出现在别的 workspace 的待办里。

### Session

- 一段对话属于一个 workspace，创建时定下，之后不改。在一段已有内容的对话里执行 `/workspace <别的目录>`，是在那个 workspace 里新开一段对话，而不是把这一段搬过去。
- 每个 workspace 有一段主对话：从这个 workspace 的文件里叫出对话窗口，或者用就地命令，进的都是它。同一个 workspace 里仍可以新开、分支、切换对话（现有命令）。
- 编辑模式的侧边栏显示"旁边那个文件所在 workspace"的主对话，所以在代码里做的事，旁边的对话窗口仍然看得到。
- 一段对话里只有一个 agent 的过程。别的 agent 的过程不进来；进来的只有三样：委派时交出去的任务文字、交回的报告、work item 上写下的记录。

这改了 [2026-09-24 记录](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)的第 5 条（"就地命令进对话窗口当前显示的那段，不按 project 划分"）：当时把"我没有设置 project"读成了"不要按 project 分"，用户 2026-10-01 更正为"用 project 充当 workspace 来实验"；当时真正的问题"不在 project 里怎么办"由临时 workspace 回答。

### Work item

- 一条记录：目标（一句话）、状态（进行中 / 已完成）、负责的 agent、进度备注、等待条件、完成时的结果。记在 Tape 上，带 workspace id；当前状态由 Tape 折叠得出，不另存一份。
- "现在能不能做"由状态和等待条件算出来，不存（Holon 的做法）。
- 等待条件的种类：到某个时刻、等用户回答、等另一个 agent 的结果。第一期只做"到某个时刻"。
- 现有的"团队任务"就是这个对象的雏形：work item 做出来后，委派时建的是 work item，不再有两套。
- 一问一答就能结束的事不建 work item。
- 提醒是带"到某个时刻"条件的 work item，分两种（用户："A"）：
  - 纯提醒（"20 分钟后提醒我给妈妈打电话"）：到点在 Emacs 里提示用户，同时在该 workspace 的主对话里留一条消息；不启动模型。
  - 带任务的（"每天早上给我今天的日程"）：到点提示，并启动模型去做。
- 重启 Emacs 后，未完成的 work item 还在；过了点的，superchat 加载后补发一次，重复型只补最近一次。

### 记忆（用户：不做全局偏好记忆）

- 每条记忆属于一个 workspace。没有"所有 workspace 都带"的那一层。
- 模型有一个"记下来"的工具，写的就是现在 `/remember` 写的那种记录，带上 workspace id；写完在答复里说明记了什么。用户能查看和删除。
- 回忆只在当前 workspace 内进行，同一 workspace 的不同对话之间互通，跨 workspace 不通。
- 修好中文查询（已登记的问题）。
- 已有的、不属于任何 workspace 的旧记忆：留在 Tape 上，不再自动带进任何对话；用户可以用命令把某条归入一个 workspace。

### 不变的规则

1. 每段对话恰好属于一个 workspace，创建时定下。
2. 每个 work item、每条记忆恰好属于一个 workspace。
3. 没有任何模型可见的内容自动跨过 workspace 的边界：历史、记忆、回忆、待办列表都不跨。
4. 一段对话只装一个 agent 的过程；agent 之间只传任务文字、报告和 work item 记录。
5. 临时 workspace 没有记忆区，它里面的东西不会被带进任何别的对话。
6. Tape 仍然只追加、不删改（现有 Invariant）；"属于哪个 workspace""带不带上"都是读取时的选择，不是删除。

### 分期（提议的顺序，每期各自确认后再动代码）

| 期 | 内容 | 用什么验收 |
|---|---|---|
| 1 | workspace 自动认定、临时 workspace、对话归属与窗口标注、就地命令按 workspace 进对话 | 脚本回归；两个 project 来回切换的真实任务，检查请求里没有对方的历史 |
| 2 | workspace 记忆：记下来的工具、回忆限定在 workspace 内、中文查询 | 目标批次的 P1（重启后还记得）；新增"在 A 记的规矩不出现在 B"的任务 |
| 3 | work item 与"到某个时刻"的等待：提醒 | 目标批次的 P3；重启后提醒仍在的任务 |
| 4 | 委派改用 work item；"等另一个 agent 的结果" | 子代理任务，检查主对话里没有子代理的过程 |
| 5 | "等用户回答"的等待 | 与[提问等用户回答](../implemented/2026-09-20-ask-user-waits-for-the-user-too.md)的现有规则一起核对后再定 |

## 用户已定（2026-10-01，"1A … 3A 4A"）

1. **临时 workspace"不留"的边界**：它的对话仍能在历史对话列表里翻到，但不会被带进任何以后的对话。Tape 不删。
2. **workspace 之内，记忆怎么带**：规矩和偏好每次都带（有总量上限），其它事实靠搜索。
3. **一件事被唤醒后在哪段对话里继续**：新开一段只带这件事记录的对话，做完后在该 workspace 的主对话里留一条简短结果。

## 仍待用户决定

**临时 workspace 里设的提醒。** 用户问"为什么会有这个问题，可以看看 codex 是怎么处理的吗"。这个问题是本草稿自己造出来的：草稿给临时 workspace 规定了"结束"的时刻，又规定 work item 只属于一个 workspace，于是才有"它结束了，里面的提醒怎么办"。Codex 没有这个问题（据其文档 *Scheduled tasks* 与 *Projects and chats*，2026-10-01 逐字读过）：

- 不属于任何 project 的对话就是一段普通对话，留在最近对话列表里，不会被回收；"事情变大了就把它移进一个 project"。
- 定时任务是独立的东西，统一列在 Scheduled 里（它同时是收件箱）。创建时说明每次运行是回到当前这段对话，还是每次新开一段。
- 所以在没有 project 的对话里设定时任务没有任何特别之处。

据此的提议：去掉"临时 workspace 何时结束"这个概念。临时 workspace 只是"没有目录、没有记忆区、只有一段对话"的 workspace；里面的提醒和别处的一样，到完成为止都在。另加一个给人看的总表，列出所有 workspace 里未完成的 work item（给人看的列表不算"模型可见的内容跨 workspace"）。

## Alternatives considered

**全局偏好记忆加"按地方"的第二层。** 本记录作者先前的提议。用户否决：全局那层极容易混淆。

**不引入 workspace，只按 session 划分记忆。** 今天的自动回忆就是按 session 划的（别的 session 只放行 `/remember` 的记录）。输在：重启或新开对话后规矩就不在了，P1 测的正是这个；而放宽成"所有 session 互通"就是全局记忆。

**workspace 只能手动创建（"2C"）。** 输在用户选了完全自动；手动绑定保留为补充。

**一个长期存在的"临时区"，有自己的记忆（"1B"）。** 输在它会变成事实上的全局记忆：不在 project 里的使用都落进同一个地方。

**work item 和记忆属于 agent 而不属于 workspace（Holon 的做法）。** Holon 的 work item 是"由一个 agent 持有的持久目标记录"；每个 agent 有自己的 AgentHome，里面放它的职责约定（`AGENTS.md`）、记忆和材料，work item 的计划文件也在那里（`work-items/<id>/plan.md`）。也就是说 Holon 按"长期存在的角色"划分记忆，不按地方。superchat 没有长期运行的 agent，对话随 Emacs 开关；把 work item 和记忆挂在 workspace 上，重启后才有地方找回它们。负责的 agent 作为 work item 的一个字段保留。

**所有 agent 共用一段对话、用标签区分。** 用户否决：上下文污染。子代理今天已经各用各的对话，本记录把它写成规则。

**照搬 DeepSeek Harness 的 workspace。** 它的 workspace 是"一个目录加属于它的对话列表"，对模型不可见，不带记忆，也没有临时 workspace（据其 `docs/subsystems/workspace.md`，未读代码）。取其"目录的规范路径是身份"这一点，现有实现已是如此；记忆和 work item 的归属是本记录新增的。

## Acceptance criteria

本记录是契约草稿，验收的是"用户逐条确认或改掉"：四个对象的定义、六条规则、分期顺序、四个待定问题。确认后本记录移入 `accepted/`，每一期另立记录，先写会失败的测试再动代码。

## Risks

- **project 认定不合用户的直觉。** 笔记目录如果不是版本库，就是临时 workspace，记不住东西。第 1 期的真实任务要覆盖"笔记目录"这种情况；`/workspace <目录>` 是补救。
- **提醒依赖 superchat 已加载。** 今天没有全局模式，Emacs 启动后要等 superchat 被加载才会补发过点的提醒。第 3 期要说明用户的配置里需要什么。
- **改了已验收的行为。** 就地命令的去向（2026-09-24 第 5 条）和 `/workspace` 改绑的含义都变了，各自的回归要随之修改。
- **主对话按 workspace 分开后，跨 workspace 的事没有地方做**（例如"把笔记里的待办同步到代码仓库的 TODO"）。本记录没有回答；出现真实需求时再定，届时不得用全局记忆来绕。
- **Holon 与 superchat 的归属不同。** Holon 的两份规范和《为什么需要 WorkItem》一文于 2026-10-01 下载原文逐字读过（work items 页自述核对于 2026-05-26，wake and continuation 页核对于 2026-09-13）。取自它的是：readiness 由状态和等待算出、不存储；等待条件带 work item 的 id；恢复一件事时先读这件事的记录，再按需取证据，而不是读最近几轮对话；一轮就能答完的事不建 work item。没有取的是：归属于 agent、一个 agent 同时只有一个"当前 work item"、计划文件、外部事件入口。
