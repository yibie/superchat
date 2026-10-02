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

## 用户已定（2026-10-01 第二轮）

4. **临时 workspace 没有"结束"**（"1A"）：它只是"没有目录、没有记忆区、只有一段对话"的 workspace；里面的提醒和别处的一样，到完成为止都在。这个问题原是本草稿自己造出来的：Codex 里不属于 project 的对话就是一段普通对话，定时任务统一列在 Scheduled 里，创建时说明回到当前对话还是每次新开（其文档 *Scheduled tasks*、*Projects and chats*，2026-10-01 逐字读过）。
5. **现有带 workspace 的两个名字要换掉**：workspace 是一级概念，"Artifact Workspace" 和 "Session EditorWorkspace" 不能再含这个词。新名字待定。
6. **就地命令进文件所在 workspace 的主对话**（"3A"），改掉 2026-09-24 记录第 5 条。
7. **规矩和偏好对 workspace 里所有对话都带上，子代理也带**；靠搜索的事实只有主 agent 能查（"4A"）。
8. **模型只在用户明确说"记住"或"以后都这样"时才写记忆**，写完说明记了什么；自己推断的偏好不写（"5A"）。
9. **workspace 与 git 解耦**：不要求是版本库；推荐用 git，尤其是 jj 来初始化，但不是前提。
10. **workspace 要与 Emacs 自身的机制结合**：superchat 是 Emacs 的原生物（用户原话）。

## Emacs 自己已有的机制（2026-10-01 读 Emacs 32 的 `project.el`、`project-tab-groups` 和用户配置核实）

| 机制 | 事实 |
|---|---|
| `project.el` 认定 project | 由 `project-find-functions` 这个钩子决定，可以加自己的认定函数；默认只有按版本库认定的那一个。`project-vc-extra-root-markers` 让"含有某个标记文件的目录"成为 project，文档写明可用于版本库之外的 project |
| 临时 project | `project-current` 在需要时会给任意目录返回一个 "transient" project |
| 已知 project 列表 | `project-list-file` 持久保存；`project-switch-project` 切换；`project-buffers`、`project-name`、`project-kill-buffers` |
| tab-bar | 每个 tab 保存自己的窗口布局；tab 可以归入 tab 组 |
| 用户的配置 | `tab-bar-mode`、`global-tab-line-mode` 都开着，这一段在 `init.el` 里的标题就是 "Workspace：tab-bar + tab-line"；`project-tab-groups-mode` 让"切换 project"变成"选中或新建这个 project 的 tab 组"，`project-kill-buffers` 关掉该组；配置注释写明目标是"项目 ↔ tab 组 ↔ ibuffer 组三者一致" |
| superchat 今天 | 只用 tab-bar：给一个 tab 记上会话 id（agent 模式下对话自己的 tab）；不看 tab 组，不看 project，不用 tab-line |

## 从体验倒推（提议，2026-10-01 第三轮；用户问"从用户体验的角度反过来看，这个机制应当如何"）

用户不应该"管理"workspace。他只在三个时刻感觉到它：对话窗口上写着的名字；说"记住"时东西落在哪；换一件事做时 AI 的上下文跟着换。由此倒推：

| 体验上的要求 | 倒推出的机制 | 与上文草稿的差别 |
|---|---|---|
| 不用创建、不用选择：在哪儿叫它，它就是哪儿的 | 以按键那一刻所在的 buffer 为准，问 `project.el` 它属于哪个 project；只移动光标不切换 | 无 |
| 随时看得见自己在哪，而且和 Emacs 说的一致 | 对话窗口标题写 workspace 的名字，与 tab 组同名；临时的写明"临时" | 无 |
| 教给它的规矩留在这里，看得见、改得了、撤得掉 | 规矩是 workspace 目录里的一个普通文件；模型改的是它的 buffer，用户保存才生效（与"模型改 buffer、用户保存"是同一个动作） | 上文"记忆"一节写的是存进 superchat 的数据库、用命令查看和删除；这里改为文件。"靠搜索的事实"仍是这个 workspace 过去的对话记录 |
| 第一次需要时才出现，不要先做设置 | 在一个还不是 workspace 的目录里第一次说"记住"时，当场问一句是否把这个目录设为 workspace | 上文是先执行 `/workspace <目录>` |
| 目录改名、搬家后还认得 | 身份是放在目录里的一个编号（第一次需要时生成），不是路径的哈希。DeepSeek Harness 的 workspace 也明确不用路径当身份（其 `docs/subsystems/workspace.md`） | 上文沿用路径哈希 |
| 随手一用不留痕迹 | 临时 workspace 相当于 AI 的 `*scratch*`：没有目录，所以没有规矩文件，也就无处可记 | 无（解释更简单） |
| 它动了别处的东西，我要看得出来 | 保存前的审阅屏上，标出不在当前 workspace 里的文件 | 新增 |

这样看暴露出一个上文没有回答的问题：**个人助理的"地方"在哪**。日程、RSS、Telegram 这类界面不对应某个目录里的文件，按 `project.el` 的规则，它们"属于哪个 project"取决于打开时碰巧所在的目录。

## 用户已定（2026-10-01 第三轮，"1A 2A 3A 4A+B 5A"与四条"按建议"）

11. **AI 跟着按键那一刻所在的 buffer**：从哪个文件叫出它（对话窗口或就地命令），就是那个文件所在的 workspace；只移动光标不切换。
12. **规矩是目录里的一个普通文件**：模型改的是它的 buffer，用户保存才生效。在还不是 workspace 的目录里第一次说"记住"时，当场问是否把这里设为 workspace。
13. **身份是放在目录里的一个编号**，第一次需要时生成；目录改名、搬家后仍认得。
14. **不对应文件的界面（日程、RSS、Telegram 等）**：用户答"A+B"。本记录的读法（待用户确认）：用户指定一个"个人"workspace 之后，这些界面默认归它，个别界面可以另指给别的 workspace；没有指定之前算临时。"个人"workspace 是一个真实的目录，不是全局层：代码仓库里看不到它的规矩。
15. **改了不在当前 workspace 里的文件**：不拦，在保存前的审阅屏上标出。
16. **一个文件属于哪个 workspace，以 Emacs 的 project 机制为准**，superchat 不另设名单。
17. **屏幕上，workspace 就是它的 tab 组**；对话独占 tab 时归入这个组；临时的不归组。
18. **只支持一个根目录。**
19. **一级概念叫 Workspace；"Artifact Workspace" 改为 Artifact Board，"Session EditorWorkspace" 改为 Session Buffer Set。** 术语表已改；其余文档、代码里的文件名与函数名、给模型看的工具名、Tape 事件名各自排期，旧的 Tape 记录不改写。

用户在这一轮说的两句话，是后面设计的依据：

- **"这里最重要的是，emacs 独特的 frame buffer windows 的组合。"** buffer 是全局的内容，window 是看 buffer 的视口，frame 和 tab 装着一套窗口的摆法；同一个 buffer 可以同时出现在几个窗口、几个 tab、几个 frame 里。所以"属于哪个 workspace"是 buffer 的事，"我现在看着哪个 workspace"是窗口、tab、frame 的事，两者可以不一致。对话是一个 buffer，摆在哪儿是窗口的决定。今天请求里告诉模型的只有来源 buffer（名字、文件、模式、光标位置），没有窗口的摆法。
- **"workitem-id 实际上是记录的是工作记录。"** 见下一节。

## Work item 是工作记录（用户，第三轮；本节取代上文"Work item"一节里把它写成待办和提醒的部分）

id 标的不是一条待办，而是一件工作的记录：要交付什么、已经做了什么、改了哪些东西、现在停在哪、结果是什么。对话会被压缩、会结束、会换一个 agent 接手；记录留下来，凡是和这件事有关的对话、执行、子代理的活和改动，都凭这个 id 挂在同一条记录上。提醒和定时只是可以附在记录上的东西，不是它的本质。

与已有东西的关系：

- superchat 已经在每个答复下面附一行"这次任务的记录"（`superchat-render-run-record-line`）：由产品从 Tape 读出这次执行实际改了哪些 buffer、是否已保存，不由模型说了算（[已定的对账记录](../accepted/2026-09-27-claims-checked-against-the-record.md)）。工作记录是同一个想法放大一级：一件工作可能跨多次执行、多段对话、多个 agent。
- jj 的 change 是"改文件的那部分工作"的现成记录：change id 在改写后不变，描述、差异、演变历史都在，多份工作副本可以让多个 agent 并行。
- jj 管不到的：不改文件的工作；不是 jj 仓库的 workspace（用户已定 workspace 与 git、jj 解耦）；还没保存的改动。

待定的问题见第四轮提问：一件工作的边界从哪儿到哪儿；记录由 superchat 自己存、还是在 jj 仓库里直接用 change；术语表里的名字。上文"分期"里把 work item 的第一期写成提醒，需要随之重排。

## 用户已定（2026-10-01 第四轮，"1A 2A 3B 4A 5A"）

20. **个人 workspace**（第 14 条的读法得到确认）：用户指定一个真实的目录作为"个人"workspace 之后，不对应文件的界面默认归它，个别界面可以另指；指定之前算临时。它不是全局层。
21. **窗口的摆法是 workspace 的一部分**：superchat 知道当前的摆法（哪个 frame、哪个 tab、哪些窗口显示哪些 buffer），在请求里告诉模型，模型能按用户的话重新摆；布局的保存和恢复仍交给 Emacs。"告诉模型摆法"先用排窗口任务做新旧对比，有效再默认打开。
22. **一段对话里可以有多件工作，由模型判断"这是另一件事了"并另起一条记录**（Holon 的做法）。本记录作者建议的是"一段对话就是一件工作"，用户没有采纳。
23. **记录由 superchat 自己存**，哪儿都能用；workspace 是 jj 仓库、这件事又改了文件时，记录里带上对应的 jj change。做到多个 agent 并行时，在 jj 仓库里给每个子代理一份自己的工作副本和 change。
24. **术语：Work Record（工作记录）**，不再用 Work Item。术语表已加。

第 22 条的后果：一段对话与工作记录是多对多——一段对话先后做几件工作，同一时刻只有一条"当前记录"；一条记录可以先后被几段对话接着做。记录分得对不对取决于模型，所以要先定"模型什么都没说时产品怎么办"，见第五轮提问。

## 第五轮：按 Holon 的机制回答（用户："你先总结 holon 的机制，然后就可以回答以上问题了"）

来源：Holon 源码仓库 `github.com/holon-run/holon`，提交 `94df057`，2026-10-01 克隆后直接阅读。概念页 `docs/website/concepts/{runtime-model,context-continuity,memory,multi-agent-collaboration}.md`；规范页 `docs/website/spec/{work-items,wake-and-continuation,scheduler,tasks,tools,workspace-and-execution}.md`；RFC `instruction-loading`（accepted）、`agent-and-workspace-memory`（draft）、`long-lived-context-memory`、`agent-delegation-tool-plane`；给模型看的原文 `src/prompt/mod.rs`（`planning_discipline`、`work_item_first_execution`、`runtime_scheduling_contract`、`agent_home_contract` 各段）、`src/prompt/tool_guidance/*.md`、`src/tool/tool_descriptions/*.md`；上下文拼装 `src/context/mod.rs`；记录结构 `src/domain/work_item.rs`；定时器 `src/runtime/waiting.rs`。其实现对照表（`docs/rfc-implementation-matrix.md`）把 work item 模型标为"部分实现：流程纪律仍有一部分靠指令"。

### Holon 的机制

| 方面 | Holon 怎么做 |
|---|---|
| 四个对象 | Agent：长期存在的执行者，有自己的家目录。Workspace：项目根目录；一个 agent 同一时刻只有一个活动 workspace；没有挂项目时，家目录就是 workspace。WorkItem：一个目标的持久记录。Task：一次执行的句柄（一条命令，或一个子 agent） |
| 三层记录 | 流水账（ledger）：运行时自动写，只追加，全量。当前工作上下文：当前 WorkItem 加运行时从可信证据里提取的引用，模型不能写这些引用。归档片段（episode）：在"换了活干、进入等待、任务回来、结果定格、轮数到顶"这些边界上，运行时把这一段工作的证据定格成不可变的归档 |
| 什么时候有 WorkItem | 由模型建。系统提示词原文："WorkItem 是一个必须跨多次执行存活的目标的持久记录，不是每一轮或每个操作的日志。"普通问答、闲聊、一次性解释、短调查、一轮能做完的多步工作，都不建。只在这些情况建：用户明确要求记录、跟踪、监控、定时；工作跨轮或必须能恢复；在等外部状态或用户输入；有独立的生命周期和验收标准。拿不准时给轻量回答，或问用户要不要跟踪 |
| 模型怎么维护 | 目标一句话；计划是 agent 家目录里的一个普通文件（`work-items/<id>/plan.md`）；进度是一张清单。原文要求"只存上下文丢失后恢复所需的最少状态"，在重大变化之后、交接之前、等待之前、结束一次长执行之前更新；恢复时先读记录，再有限度地重查可能已变的外部事实；不把整段聊天或工具日志抄进计划 |
| 焦点 | 同一时刻至多一个当前 WorkItem。新建的不会自动成为当前。从 A 切到 B 会记下续接关系，B 完成后回到 A |
| 模型每轮看到什么 | 当前 WorkItem 的全文（目标、计划状态、计划文件预览、清单、等待）；当前工作的引用；其它 WorkItem 的候选清单，按调度状态分组（被触发的、可运行的、让出的、等用户的、被卡住的、最近完成的）；按相关性和预算选出的归档片段；最近几轮；当前输入。没有当前项时写明"无" |
| 上下文怎么不乱 | 不在 WorkItem 的边界上硬切历史。做的是"投影"：钉住权威的内容（用户意图、当前 WorkItem、等待）；按续接链和预算选最近几轮；折叠重复的唤醒和重试；更早的工作变成归档片段，按相关性取。提示里另写明：最近几轮只是背景证据，除非当前输入或当前 WorkItem 要求，不要去恢复旧任务或旧等待 |
| 等待与唤醒 | `WaitFor` 把等待（任务结果、定时器、外部事件、用户输入、系统）挂在 WorkItem 上，没有 WorkItem 时挂在 agent 上，然后让出这一轮。条件满足时运行时唤醒 agent 起新的一轮。能不能跑由状态和等待算出，不存储 |
| 定时器 | 独立的对象，不是 WorkItem：`CreateTimer` 建一次性或重复的定时器，持久保存，过了点的在恢复时补发。到点是往 agent 的队列里放一条消息，起一轮模型。要"等到那时再继续这件事"，另用 `WaitFor(timer)` |
| 完成 | 模型调用 `CompleteWorkItem`，必须附一段给用户看的完成报告；运行时把报告与这次执行、这条记录的版本绑在一起原子提交，并取消这条记录上的等待。只有"进行中"和"已完成"，没有重新打开 |
| 存在哪儿 | 运行时状态（账本、索引、缓存）在 agent 家目录的 `.holon/` 下，agent 不得编辑。计划文件是家目录里的普通文件 |
| 规矩与记忆的归属 | 指令文件三层：用户全局 `~/.agents/AGENTS.md`、agent 级 `agent_home/AGENTS.md`、workspace 级 `<workspace>/AGENTS.md`（没有它时才退而读 `CLAUDE.md`）。agent 家里另有两份精选记忆 `memory/operator.md`（用户偏好）和 `memory/self.md`，每轮各带一小段（默认 1500 字符），其余靠搜索。运行时证据在写入时带上 workspace 的 id，搜索默认限定在当前 workspace。检索对中文做二元切分。原文："一条记忆若成了 workspace 必须遵守的规矩，就应升格写进 workspace 的指令文件或仓库文档。"没有"记住"工具（RFC 列为未决） |
| 多 agent | 委派出去的是一个 Task，归创建它时的那条 WorkItem 名下；子 agent 的结果作为任务结果回来。子 agent 拿到的上下文以那条 WorkItem 的快照加明确交代为中心，不是父 agent 的整段对话。委派不扩大权限。要隔离改文件就开 git worktree，非 git 目录没有隔离办法 |

### 据此对第五轮七个问题的回答

| 问题 | 回答 | 与本记录作者上一轮建议的差别 |
|---|---|---|
| 什么时候算有一条记录 | 由模型按 Holon 那套规则建：只给必须跨轮存活的工作建，普通问答和一轮做完的事不建。"实际做了什么"仍由已有的东西自动留着：Tape，和每个答复下面那行"这次任务的记录" | 上一轮建议"动了手就自动开一条"，不采用 |
| 换了一件事后，前一件事的对话带不带 | 不在边界上硬切。当前记录每次都钉在请求里；最近几轮照带，并写明它们只是背景；更早的照现有的压缩办法处理，压缩点优先落在"换了活干、进入等待、任务回来"这些边界上 | 上一轮建议"换事即只带摘要"，不采用 |
| 模型看得到哪些记录 | 当前这条的全文，加本 workspace 其它记录的分组清单；细节用工具读 | 一致 |
| 怎么算完 | 模型标完成并必须附结论，产品把结论和这次执行绑在一起、旁边附上事实；用户也可以自己标完成。不设"重新打开"，后续的事另起一条并指向旧的 | 去掉了"重新打开" |
| 存在哪儿 | 记录在 superchat 的数据库里，只追加。需要长计划时，计划是 superchat 数据目录下的一个普通文件，不放进用户的项目目录 | 多了"计划是文件"这一点 |
| 提醒算不算记录 | 不算。定时器是独立的对象；"到点接着干某件事"是挂在记录上的等待 | 一致。Holon 的定时器到点一律起一轮模型；"纯提醒只提示、不启动模型"是用户此前的决定，保留，这是 superchat 与 Holon 的一处差别 |
| 规矩文件与 AGENTS.md | workspace 的规矩就是它根目录下的 `AGENTS.md`（没有它时退而读 `CLAUDE.md`），不另设 superchat 自己的规矩文件。用户明确说"记住"时，模型改的是 `AGENTS.md` 的 buffer，用户保存才生效。`.superchat/` 只放 workspace 的身份编号 | 上一轮建议"自己的文件，另外只读 AGENTS.md"，不采用 |

### 上一轮六条推论按 Holon 修正

1. 事实由产品写、叙述由模型写：与 Holon 一致（它的引用、账本、归档都由运行时写）。
2. "叙述就用每次答复本身"：不符合 Holon，撤回。有记录的工作，模型要自己维护记录里恢复所需的最少状态（目标、计划、进度、下一步），在重大变化后、交接前、等待前、结束长执行前更新。因为只有跨轮的工作才有记录，这笔开销只落在长工作上。
3. "子代理的活是一条子记录"：不符合 Holon，改为：委派出去的是一个任务，挂在主记录名下；子代理拿到的是这条记录的快照加明确交代；结果作为任务结果回到主记录。
4. 往 Emacs 的 project 钩子里加认定函数、5. 界面归属表、6. 告诉模型的摆法：与 Holon 无关，不变。

### superchat 有意与 Holon 不同的地方（都来自用户此前的裁定）

- Holon 的记录和记忆属于一个长期存在的 agent，workspace 只是标签；superchat 没有长期存在的 agent，记录和规矩属于 workspace，workspace 的主对话相当于 Holon 里那个长期存在的 agent。
- Holon 有用户全局的指令文件和 agent 级的用户偏好文件；superchat 不设全局层。
- Holon 没有项目时退回 agent 的家目录，那里有记忆；superchat 的临时 workspace 没有记忆，个人 workspace 要用户指定。
- Holon 的定时器到点一律起一轮模型；superchat 的纯提醒不启动模型。

## jj 的事实（供上一节用；`jj help -k glossary`，jj 0.45.1）

change 是"一个随时间演变的提交"，change id 在改写（描述、变基、合并）后不变；一个仓库可以有多个 workspace，即多份工作副本，"就是 Git 所说的 worktree"，各有自己的工作副本提交；操作日志记录每次操作，并发操作在日志里分叉再合并。Antigravity 的 "New Worktree Mode" 用的是同一种隔离，理由是"防止并行的子代理互相冲突"（其文档 *Projects*，2026-10-01 逐字读过）。

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
