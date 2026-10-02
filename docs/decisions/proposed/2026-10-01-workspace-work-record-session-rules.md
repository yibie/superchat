# Decision: workspace、工作记录、对话、规矩——各管什么，谁来记，什么时候记

Status: proposed — 逐条来自用户 2026-10-01 至 02 的裁定（见"来源"），整份待用户确认后移入 `accepted/`。不含代码改动；每一期动代码前另立记录。

## 一句话

"在哪儿做"是 workspace，"谁在做时的那段对话"是 session，"一件要跨多次执行才做得完的事"是工作记录，"这个地方的规矩"是目录里的 `AGENTS.md`。规矩和工作记录只属于一个 workspace，没有到处都生效的那一层。判断归模型，登记要走工具而不是写在答复里，保存、安排和每次执行结束时的裁定归产品。

## Problem

2026-10-01 的目标批次（`.scratch/task-sim/GOALS-BATCH-REPORT.md`）里，"个人助理"一组没有达到：说过的偏好重启后不在（P1，0/3），提醒不可靠（P3，1/3）。加一块记忆、加一个提醒工具之前，要先回答它们属于谁。

- 记忆如果到处生效，写日记的规矩会进到代码仓库的对话里。
- 提醒、子代理交接、跨重启继续，都需要一个比一段对话活得久的东西来挂。
- 多个 agent 把各自的过程写进同一段对话，会互相污染上下文。
- 模型在答复里说"等一下再看""接下来我会……"，人能懂，系统却不知道该不该、何时再安排执行。目标批次和此前几批里，"在答复里问用户而不去做""说了要做却停下"各出现多次。

## 现状

| 事实 | 出处 |
|---|---|
| `/workspace <目录>` 把一段对话绑定到一个目录：生成 `workspace-<真实路径哈希前 12 位>`，记为 Tape 事实 `workspace.bound`，重开对话时恢复；它设定工具的默认目录，并把 `workspace-id` 带进每个请求和 Tape 事件的信封。只能手动绑定，按对话绑定，可随时改绑 | `superchat.el` `superchat--bind-workspace`、`superchat--cmd-workspace`、`superchat--restore-session-runtime-configuration`；`superchat-tape.el` `superchat-tape--envelope-keys` |
| 除了 Artifact Board 和工具默认目录，没有别的东西按这个 id 划分 | `superchat-memory.el`；`superchat-db.el` 的 `memory` 表；`superchat-dispatcher.el` `superchat--attach-indexed-memories` |
| 对话窗口显示哪段对话，取决于当前 tab 或主对话 buffer；就地问、就地做、改写选区进对话窗口正在显示的那段 | `superchat.el` `superchat`、`superchat-conversation-session-id`；[2026-09-24 记录](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)第 5 条 |
| superchat 只用 tab-bar 给一个 tab 记上会话 id；不看 tab 组，不看 project | `superchat-editor-workspace.el` `superchat-editor-workspace--tab-session-id` |
| 请求里告诉模型的位置只有来源 buffer 的名字、文件、模式和光标位置，没有窗口的摆法 | `superchat.el` `superchat--origin-render` |
| 子代理用自己的对话（`subagent-<角色>-<时间>-<随机>`），构造提示时不带主对话历史，只带任务文字和主 agent 交给它的一段背景，交回一份报告 | `superchat-subagent.el` `superchat--subagent-make-context`、`superchat--subagent-run-async` |
| 已有带 id 的团队任务 `T<n>`（标题、状态、负责人、依赖），每次委派自动建一条；只在一段对话内有效，没有等待条件 | `superchat-subagent.el` `superchat-team-task`、`superchat--team-start-delegated-task` |
| 每个用过工具的答复下面有一行产品写的记录：这次执行改了哪些 buffer、是否已保存、跑了几次 Elisp；从 Tape 读出，不发给模型 | `superchat-render.el` `superchat-render-run-record-line`；[对账记录](../accepted/2026-09-27-claims-checked-against-the-record.md) |
| 一次执行以 `completed`、`failed`、`cancelled`、`timed-out`、`audit-incomplete`、`abandoned` 之一封存；这说的是这次执行本身，不说这件工作有没有做完。模型只回文字就停下时，有一次按答复措辞判断的收尾检查 | `superchat-runtime.el` `superchat-runtime--terminal-statuses`；[收尾检查记录](../accepted/2026-09-23-one-finish-check-after-a-text-only-reply.md) |
| 只有 `/remember` 写入能跨对话被自动带上的记录；说"记住……"不写任何东西；模型没有写记忆的工具；回忆的查询对中文整句基本匹配不上；superchat 不自动读目录里的 `AGENTS.md` | `superchat.el` `superchat--cmd-remember`；`.scratch/issues/2026-10-01-recall-never-matches-a-chinese-sentence.md` |
| 没有任何到点自己做事的机制 | — |

别的系统和 Emacs 自己怎么做，见[参考实现里的事实](../../research/workspace-and-work-record-references.md)。

## Proposal

### 对象

| 对象 | 是什么 | 归属 |
|---|---|---|
| Workspace | 一个工作的地方：Emacs 认作 project 的一个目录，加上 superchat 为它留着的规矩、对话和工作记录 | — |
| 临时 workspace | 不在任何 project 里时落到的地方 | — |
| 个人 workspace | 用户指定的一个真实目录，作为不对应文件的那些界面的家 | — |
| Session | 一个 agent 做事时自己的那段对话 | 恰好一个 workspace |
| 规矩 | workspace 根目录下的 `AGENTS.md` | 恰好一个 workspace |
| 工作记录 | 一件必须跨多次执行存活的工作的持久记录 | 恰好一个 workspace |
| 定时器 | 一张到点起作用的条子 | 恰好一个 workspace |

### Workspace

W1. 一个文件属于哪个 workspace，以 Emacs 的 project 机制为准，superchat 不另设名单。

W2. AI 跟着按键那一刻所在的 buffer：从哪个文件叫出对话窗口或用就地命令，就是那个文件所在的 workspace。只移动光标不切换。

W3. workspace 不要求是版本库。推荐用 git，尤其是 jj 来初始化，但不是前提。

W4. 身份是放在目录里的一个编号（`.superchat/` 下），第一次需要时生成；目录改名、搬家后仍是同一个 workspace。

W5. 普通目录在第一次有东西要留下时成为 workspace：用户在那里说"记住"，superchat 当场问是否把这个目录设为 workspace。

W6. 一个 workspace 只有一个根目录。

W7. 屏幕上，workspace 就是它的 tab 组：对话独占一个 tab 时，这个 tab 归入所在 workspace 的 tab 组；对话窗口的标题写 workspace 的名字，与 tab 组同名。

W8. 窗口的摆法是 workspace 的一部分：superchat 知道当前的摆法（哪个 frame、哪个 tab、哪些窗口显示哪些 buffer），在请求里告诉模型，模型能按用户的话重新摆。布局的保存和恢复交给 Emacs，superchat 不另存一份。

W9. 模型改了不在当前 workspace 里的文件，不拦；保存前的审阅屏上标出。

W10. 临时 workspace：不在任何 project 里时使用。每次临时使用各开一个，互不相通。它没有目录，所以没有规矩；只有一段对话；它里面的东西不会被带进任何别的对话。它的对话仍能在历史对话列表里翻到。它的 tab 不归组，标题写明"临时"。

W11. 个人 workspace：用户指定一个真实的目录。日程、RSS、Telegram 这类不对应文件的界面默认归它，个别界面可以另指给别的 workspace。指定之前，从这些界面叫出的是临时 workspace。它不是全局层：别的 workspace 看不到它的规矩。

### Session

S1. 一段对话属于一个 workspace，创建时定下，之后不改。

S2. 每个 workspace 有一段主对话。从这个 workspace 的文件里叫出对话窗口，或者用就地问、就地做、改写选区，进的都是它。同一个 workspace 里仍可以新开、分支、切换对话。

S3. 一段对话只装一个 agent 的过程。agent 之间只传三样：任务交代、交回的报告、工作记录。

### 规矩与回忆

R1. 没有全局层：不存在所有 workspace 都带上的规矩或记忆。

R2. workspace 的规矩是它根目录下的 `AGENTS.md`；没有它时退而读 `CLAUDE.md`。superchat 不另设自己的规矩文件。

R3. 规矩每次请求都带上，有总量上限；这个 workspace 里的所有对话都带，子代理也带。

R4. 模型只在用户明确说"记住"或"以后都这样"时写规矩；自己推断出来的偏好不写。它改的是 `AGENTS.md` 的 buffer，用户保存才生效；答复里说明写了什么。

R5. 规矩以外的事实靠搜索：搜的是本 workspace 过去的对话和工作记录，只有主 agent 能查，不跨 workspace。中文查询要能匹配。

### 工作记录

K1. 工作记录是一件必须跨多次执行存活的工作的持久记录，不是每一次执行、每一个操作的流水。流水是 Tape；每次执行实际改了什么，是答复下面那行产品写的记录。

K2. 一条工作记录让以后的执行能回答四个问题：要交付什么；做到哪里了；下一步为什么还不能做；回来后从哪里接上。

K3. 谁来记：判断归模型，登记走工具，保存和安排归产品。模型理解目标、分析证据、决定还需要做什么；它把"这件事在等什么、什么条件下可以继续"登记给产品，而不是只写在答复里。产品保存这些状态，按登记的等待条件和到达的信号安排后续执行，并在每次执行结束时从可信的事实里提取"碰过哪些 buffer、文件、对应的 jj change"附在记录上，模型不能写这部分。

K4. 什么时候记：接下一件跨轮的工作时建；重大变化之后、交给别人之前、进入等待之前、结束一次长执行之前更新；被唤醒后先读记录、核对已经变了的事实、再更新；交付时附上结论完成。

K5. 什么时候建：用户明确要求记录、跟踪、定时；工作跨轮或必须能恢复；在等别的东西；有独立的生命周期和验收标准。普通问答、一次执行就做完的事，不建。拿不准时不建，或者问用户要不要跟踪。

K6. 边界由模型判断：一段对话里可以先后有几件工作，同一时刻只有一条当前记录；同一个目标复用同一条，换了目标才另起。

K7. 每次执行结束，这条记录的结局由产品得出，不解析答复的文字：完成；可继续；等待（等用户回答、等另一个 agent 的结果、等到某个时刻）；失败。模型的话是证据，不是权威：工具和登记的状态优先于答复里"已完成""我在等"这样的说法。没完成、也没登记等待的记录是可继续的，产品接着安排下一次执行。

K8. 模型每次请求看到：当前记录的全文；本 workspace 其它记录的清单，按"可继续、在等用户、在等别的、最近完成"分组；细节用工具读。别的 workspace 的记录一概不带。

K9. 换了一件工作之后，不在边界上硬切对话历史：当前记录每次都钉在请求里；最近几轮照带，并写明它们只是背景，除非当前输入或当前记录要求，不去恢复旧的工作；更早的照现有的压缩办法处理，压缩点优先落在换工作、进入等待、委派的活回来这些边界上。

K10. 一件工作被唤醒后，新开一段只带这条记录的对话去做，做完后在该 workspace 的主对话里留一条简短结果。

K11. 完成：模型标完成并必须附一段结论；产品把结论和这次执行绑在一起，旁边附上事实。用户也可以自己标完成。没有"重新打开"：后续的事另起一条，指向旧的。

K12. 记录存在 superchat 的数据库里，只追加。需要长计划时，计划是 superchat 数据目录下的一个普通文件，不放进用户的项目目录。

K13. workspace 是 jj 仓库、这件工作又改了文件时，记录带上对应的 jj change。多个 agent 并行时，在 jj 仓库里给每个子代理一份自己的工作副本和 change。

K14. 委派出去的是一个任务，挂在主记录名下。子代理拿到的是这条记录的快照加明确交代，不是主对话；结果作为任务结果回到主记录。委派不扩大权限。

### 定时器与提醒

T1. 定时器是独立的对象，不是工作记录。它属于一个 workspace，持久保存；重启 Emacs 后，过了点的在 superchat 加载后补发一次，重复型只补最近一次。

T2. 纯提醒（"20 分钟后提醒我给妈妈打电话"）：到点在 Emacs 里提示用户，同时在该 workspace 的主对话里留一条消息；不启动模型。

T3. 带任务的定时（"每天早上给我今天的日程"）：到点提示，并启动模型去做。

T4. "到点接着干某件事"是挂在工作记录上的一个等待，不是提醒。

### 术语

一级概念叫 Workspace。原"Artifact Workspace"改为 Artifact Board，原"Session EditorWorkspace"改为 Session Buffer Set。记录叫 Work Record（工作记录），不用 Work Item。[术语表](../../../CONTEXT.md)已按此修改；其余文档、代码里的文件名与函数名、给模型看的工具名、Tape 事件名各自排期，已有的 Tape 记录不改写。

## 与 Holon 不同的地方

工作记录的机制取自 Holon；下面四处是有意的差别。

- Holon 的记录和记忆属于一个长期存在的 agent，workspace 只是标签。superchat 没有长期存在的 agent，记录和规矩属于 workspace；workspace 的主对话相当于 Holon 里那个长期存在的 agent。
- Holon 有用户全局的指令文件和 agent 级的用户偏好文件。superchat 不设全局层（R1）。
- Holon 没挂项目时退回 agent 的家目录，那里有记忆。superchat 的临时 workspace 没有记忆（W10），个人 workspace 要用户指定（W11）。
- Holon 的定时器到点一律起一轮模型。superchat 的纯提醒不启动模型（T2）。

## 改动的已验收行为

- [2026-09-24 记录](../implemented/2026-09-24-editor-mode-agent-mode-and-review-screen.md)第 5 条"就地命令进对话窗口正在显示的那段，不按 project 划分"，由 W2、S2 取代。
- `/workspace <目录>` 在一段已有内容的对话里执行，从"把这段对话改绑过去"变为"在那个 workspace 里新开一段"（S1 的后果）。
- 自动回忆的范围从"别的对话只放行 `/remember` 的记录"变为"本 workspace 内互通、跨 workspace 不通"（R5）。
- 有工作记录的工作，执行结束时的判断从"按答复措辞做一次收尾检查"变为 K7；没有工作记录的普通请求不变。

## 分期

每一期动代码前另立记录、先写会失败的测试、再用真实任务验收；顺序是提议。

| 期 | 内容 | 验收 |
|---|---|---|
| 1 | Workspace 认定与对话归属：W1–W7、W10、S1、S2 | 脚本回归；两个 project 来回切换的真实任务，检查请求里没有对方的历史 |
| 2 | 规矩与回忆：R1–R5 | 目标批次的 P1（重启后还记得）；新增"在 A 记的规矩不出现在 B"的任务 |
| 3 | 个人 workspace：W11 | 从日程界面提问的真实任务，带上了个人 workspace 的规矩 |
| 4 | 定时器与提醒：T1–T3 | 目标批次的 P3；重启后提醒仍在的任务 |
| 5 | 把摆法告诉模型：W8、W9 | 排窗口任务的新旧对比 |
| 6 | 工作记录：K1–K9、K11、K12 | 新增跨两段对话才能做完的真实任务；检查记录能回答 K2 的四个问题，换一段对话能接着做 |
| 7 | 等待与唤醒、委派、jj：K10、K13、K14、T4 | 子代理任务，检查主对话里没有子代理的过程；到点接着做的任务 |
| 8 | 等用户回答的等待 | 与[提问等用户回答](../implemented/2026-09-20-ask-user-waits-for-the-user-too.md)的现有规则一起核对后再定 |
| 贯穿 | 改名：Artifact Board、Session Buffer Set 进入其余文档和代码 | 各期顺带，不单独验收 |

## 留到各期再定的细节

这些是本记录作者的提议，用户没有逐条确认过，在对应的那一期提出：

- 普通目录成为 workspace 时，superchat 往 Emacs 的 `project-find-functions` 里加一个认 `.superchat/` 的函数，让全 Emacs 都把它当 project。这会改动一个全局钩子。（第 1 期）
- `AGENTS.md` 带进请求的字数上限，超出部分怎么取。（第 2 期）
- `/remember <文字>` 与对模型说"记住"是同一件事。已有的、不属于任何 workspace 的旧记忆留在 Tape 上，不再自动带进对话；用户可以用命令把某条归入一个 workspace。（第 2 期）
- "不对应文件的界面"按一张用户能改的 major mode 表来认；不在表里、又不在任何 project 里的（如 `*scratch*`）即使指定了个人 workspace 也仍是临时。（第 3 期）
- 告诉模型的摆法：当前 tab 里每个窗口显示什么、哪个是选中的、哪个是 superchat 自己的；别的 tab 和 frame 只给名字。（第 5 期）
- "可继续"的记录由产品接着安排，其次数上限与用户叫停的方式。（第 6 期）
- 一张给人看的总表，列出所有 workspace 里未完成的工作记录和定时器；它不进模型的上下文。（第 6 期）

## 来源

| 条目 | 用户原话（2026-10-01 至 02） |
|---|---|
| 先有 workspace，临时 workspace，project 充当 workspace | "的确需要先发明好 workspace 这个概念。尤其是如果用户临时使用，那么是否有一个「临时 workspace」这样的虚拟对象。""我们需要实验性的测试用 project 来当成一个 workspace 的虚拟对象来实验一把" |
| R1 | "打算不做全局偏好记忆，极容易混淆。" |
| S3 | "也不允许所有 agent ， subagent 共享 session，这非常容易造成上下文污染" |
| W10（每次各开一个）、W1（自动认定） | "1C 2A 3A" |
| W10（历史里翻得到、不带进以后的对话）、R3 与 R5（规矩每次带、其余靠搜索）、K10 | "1A … 3A 4A" |
| T2、T3 | 对"到点先在 Emacs 里提示，同时作为新消息进入原对话；纯提醒不启动模型"答"A" |
| W10（没有"结束"）、W2 与 S2（就地命令进文件所在 workspace 的主对话）、R3（子代理也带规矩）、R4 | "1A … 3A … 4A 5A" |
| 术语 | "现有的词得换掉（不能包含 workspace 这个一级概念）"；对"一级概念仍叫 Workspace；两个旧名字改成 Artifact Board 和 Session Buffer Set"答"同意"；对"叫 Work Record"答"5A" |
| W3 | "不能理解为何必须与git绑定实际上是解耦的，只不过会推荐使用git，尤其是jj来初始化项目。" |
| 与 Emacs 结合 | "关键是，我们需要将 workspace 这概念，与 emacs 本身的机制结合。因为 superchat 是 emacs 的原生物。""这里最重要的是，emacs 独特的 frame buffer windows 的组合。" |
| W2、R2 与 R4（规矩是文件、保存才生效）、W5、W4、W11、W9 | "1A 2A 3A 4A+B 5A" |
| W1、W7、W6 | "以 project 机制为准""对""只支持一个根目录" |
| K1、K2 | "我认为 workitem-id 实际上是记录的是工作记录" |
| W11（读法确认）、W8、K6、K12 与 K13 | "1A 2A 3B 4A 5A" |
| K5、K7–K9、K11、K12、K14、T1、T4、R2 | "你先总结 holo 的机制，然后就可以回答以上问题了"——按 Holon 的机制回答，见[参考实现里的事实](../../research/workspace-and-work-record-references.md) |
| K3、K4 | 指着 Holon 文章"把负责人、工作和每次执行拆开""模型判断下一步，运行时记住如何继续"两节说："这里好像说明了是谁来记录，以及记录时机的问题" |

## Alternatives considered

**全局偏好记忆，加"按地方"的第二层。** 用户否决：全局那层极容易混淆。

**不引入 workspace，只按 session 划分记忆。** 输在重启或新开对话后规矩就不在了，目标批次的 P1 测的正是这个；放宽成所有 session 互通就是全局记忆。

**workspace 只能手动创建。** 输在用户选了自动认定；手动的 `/workspace <目录>` 保留。

**一个长期存在的"临时区"，有自己的记忆。** 输在它会变成事实上的全局记忆：不在 project 里的使用都落进同一个地方。Antigravity 的默认 project、Holon 的家目录是这种做法。

**AI 跟着 tab 组走，或跟着光标实时切换。** 输在规矩应该跟着文件走：在代码仓库的 tab 组里打开日记来改，该用的是日记那边的规矩；实时切换则太跳。

**规矩存进 superchat 的数据库，或写在 `.superchat/` 下自己的文件里。** 前者输在用户看不见、改不了，还要另问"存之前要不要问"；后者输在 Holon 和 Codex 都把 workspace 的规矩放在 `AGENTS.md` 里，另设一份会让同一个地方有两套规矩。代价是 `AGENTS.md` 与别的 agent 共用：superchat 记下的东西它们会读到，它们的规矩也会进 superchat 的请求。

**一段对话就是一件工作。** 本记录作者的建议，用户没有采纳，选了由模型判断边界（K6）。

**只要动了手就自动开一条工作记录。** 本记录作者的建议，输在 Holon 的规则：记录是给必须跨多次执行存活的工作用的，不是每个操作的日志；每次执行实际做了什么，Tape 和答复下面那行记录已经留着。

**换了一件工作就把之前的对话换成摘要。** 本记录作者的建议，输在 Holon 的做法：钉住当前记录、保留最近几轮并标明是背景、更早的归档（K9）；硬切在模型分错边界时会丢掉需要的内容。

**工作记录就是 jj 的 change，或每条记录是 workspace 目录里的一个文件。** 前者输在 jj 记不了三样东西：不改文件的工作、不是 jj 仓库的 workspace、还没保存的改动。后者输在每次更新都会改动用户的工作副本，临时 workspace 也没有目录可放。

**提醒也算工作记录。** 输在记录是"一件工作现在的状态"，提醒只是一张到点的条子；Holon 里定时器也是独立的对象。

**叫 Work Item。** 输在这个词让本记录作者把它读成了待办；用户指出它记的是工作记录。

**所有 agent 共用一段对话、用标签区分。** 用户否决：上下文污染。

## Acceptance criteria

用户确认整份记录，或指出要改的条目。确认后移入 `accepted/`，路线图按"分期"更新，每一期另立记录。

## Risks

- **project 的认定不合直觉。** 笔记目录如果不是版本库，在被设为 workspace 之前是临时的，记不住东西。第 1 期的真实任务要覆盖这种目录。
- **边界由模型判断（K6）、登记靠模型调用工具（K3）。** 模型会忘，也会分错。K7 让"忘了登记"的后果是继续安排而不是悄悄停下，但"该建记录而没建"没有兜底。第 6 期要先用真实模型测再定默认。
- **`AGENTS.md` 与别的 agent 共用。** 见上文代价；写给编程 agent 的长规矩会占用 superchat 每次请求的字数。
- **提醒依赖 superchat 已加载。** 没有全局模式，Emacs 启动后要等 superchat 被加载才会补发过点的提醒。第 4 期要说明用户的配置里需要什么。
- **改了已验收的行为。** 见"改动的已验收行为"，各自的回归要随之修改。
- **跨 workspace 的事没有地方做**，例如"把笔记里的待办同步到代码仓库"。本记录没有回答；出现真实需求时再定，届时不得用全局层来绕。
- **改动一个 Emacs 全局钩子**（见"留到各期再定的细节"第一条）。
