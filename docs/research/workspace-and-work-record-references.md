# Workspace 与工作记录：参考实现里的事实

这页只记录别的系统和 Emacs 自己是怎么做的，供[契约](../decisions/proposed/2026-10-01-workspace-work-record-session-rules.md)引用。每一节写明读的是什么、哪一天读的；除注明"未读代码"的以外，都是直接读原文或源码，不是摘要。

## Holon

来源：`github.com/holon-run/holon`，提交 `94df057`，2026-10-01 至 02 克隆后阅读。概念页 `docs/website/concepts/{runtime-model,context-continuity,memory,multi-agent-collaboration}.md`；规范页 `docs/website/spec/{work-items,wake-and-continuation,scheduler,tasks,tools,workspace-and-execution}.md`；RFC `result-closure`（accepted）、`instruction-loading`（accepted）、`agent-and-workspace-memory`（draft）、`long-lived-context-memory`、`agent-delegation-tool-plane`；文章 `docs/website/zh-CN/blog/why-work-items.md`；给模型看的原文 `src/prompt/mod.rs`（`planning_discipline`、`work_item_first_execution`、`runtime_scheduling_contract`、`agent_home_contract` 各段）、`src/prompt/tool_guidance/*.md`、`src/tool/tool_descriptions/*.md`；上下文拼装 `src/context/mod.rs`；记录结构 `src/domain/work_item.rs`；定时器 `src/runtime/waiting.rs`。它的实现对照表 `docs/rfc-implementation-matrix.md` 把 work item 模型标为"部分实现：流程纪律仍有一部分靠指令"。

| 方面 | Holon 怎么做 |
|---|---|
| 四个对象 | Agent：长期存在的执行者，有自己的家目录（AgentHome）。Workspace：项目根目录；一个 agent 同一时刻只有一个活动 workspace；没有挂项目时，家目录就是 workspace。WorkItem：一个目标的持久记录。Task：一次执行的句柄（一条命令，或一个子 agent） |
| 三层记录 | 流水账（ledger）：运行时自动写，只追加，全量。当前工作上下文：当前 WorkItem 加运行时在每次执行结束时从可信证据里提取的引用，模型不能写这些引用。归档片段（episode）：在"换了活干、进入等待、任务回来、结果定格、轮数到顶"这些边界上，运行时把这一段工作的证据定格成不可变的归档 |
| WorkItem 要回答的四个问题 | 文章原文：要交付什么；做到哪里了；下一步为什么还不能做；回来后从哪里接上。"这些信息不必塞进同一个文件，更不必复制整段聊天和全部日志。关键是它们归属于同一项工作，并能在恢复时找到。" |
| 谁来记录 | 文章小标题："模型判断下一步，运行时记住如何继续"。Agent 理解目标、分析证据、决定还需要做什么；运行时保存工作状态，依据等待条件和到达的信号安排后续执行。"如果 Agent 只在回复里说'等作者修好后再看'，人能理解，系统却未必知道何时再安排执行。Agent 需要向运行时登记：哪项工作在等什么，什么条件下可以继续。" |
| 记录的时机 | 接受一项跨轮的委托时建；系统提示词要求在"重大变化之后、交接之前、等待之前、结束一次长执行之前"更新；信号到达后恢复时，先读记录，再核对事实，再更新记录；交付时写清验收依据、结果和仍需处理的事项 |
| 每次执行结束由谁裁定 | 运行时。RFC `result-closure`："运行时是结局的最终裁定者"；"助手的文字是证据，不是权威"；"工具和任务的状态优先于自由文本里的完成声明"；"明确的运行时原语优先于'我在等……'这样的措辞"；"不应该仅仅因为助手不再说话就宣称成功"；"结局必须不解析助手的文字就能得出"。结局四种：完成、可继续、失败、等待（等用户输入、等外部变化、等任务结果、等定时器）。未完成、没有等待的 WorkItem 是"可继续"的，调度器在空闲边界再次进入模型 |
| 什么时候有 WorkItem | 由模型建。系统提示词原文："WorkItem 是一个必须跨多次执行存活的目标的持久记录，不是每一轮或每个操作的日志。"普通问答、闲聊、一次性解释、短调查、一轮能做完的多步工作，都不建。只在这些情况建：用户明确要求记录、跟踪、监控、定时；工作跨轮或必须能恢复；在等外部状态或用户输入；有独立的生命周期和验收标准。拿不准时给轻量回答，或问用户要不要跟踪。同一个目标复用同一条 |
| 模型怎么维护 | 目标一句话；计划是 agent 家目录里的一个普通文件（`work-items/<id>/plan.md`）；进度是一张清单。原文："只存上下文丢失后恢复所需的最少状态"；不把整段聊天或工具日志抄进计划 |
| 焦点 | 同一时刻至多一个当前 WorkItem。新建的不会自动成为当前。从 A 切到 B 会记下续接关系，B 完成后回到 A |
| 模型每轮看到什么 | 当前 WorkItem 的全文（目标、计划状态、计划文件预览、清单、等待）；当前工作的引用；其它 WorkItem 的候选清单，按调度状态分组（被触发的、可运行的、让出的、等用户的、被卡住的、最近完成的）；按相关性和预算选出的归档片段；最近几轮；当前输入。没有当前项时写明"无" |
| 上下文怎么不乱 | 不在 WorkItem 的边界上硬切历史。做的是"投影"：钉住权威的内容（用户意图、当前 WorkItem、等待）；按续接链和预算选最近几轮；折叠重复的唤醒和重试；更早的工作变成归档片段，按相关性取。提示里另写明：最近几轮只是背景证据，除非当前输入或当前 WorkItem 要求，不要去恢复旧任务或旧等待 |
| 等待与唤醒 | `WaitFor` 把等待（任务结果、定时器、外部事件、用户输入、系统）挂在 WorkItem 上，没有 WorkItem 时挂在 agent 上，然后让出这一轮。条件满足时运行时唤醒 agent 起新的一轮。能不能跑由状态和等待算出，不存储 |
| 定时器 | 独立的对象，不是 WorkItem：`CreateTimer` 建一次性或重复的定时器，持久保存，过了点的在恢复时补发。到点是往 agent 的队列里放一条消息，起一轮模型。要"等到那时再继续这件事"，另用 `WaitFor(timer)` |
| 完成 | 模型调用 `CompleteWorkItem`，必须附一段给用户看的完成报告；运行时把报告与这次执行、这条记录的版本绑在一起原子提交，并取消这条记录上的等待。只有"进行中"和"已完成"，没有重新打开 |
| 存在哪儿 | 运行时状态（账本、索引、缓存）在 agent 家目录的 `.holon/` 下，agent 不得编辑。计划文件是家目录里的普通文件 |
| 规矩与记忆的归属 | 指令文件三层：用户全局 `~/.agents/AGENTS.md`、agent 级 `agent_home/AGENTS.md`、workspace 级 `<workspace>/AGENTS.md`（没有它时才退而读 `CLAUDE.md`）。agent 家里另有两份精选记忆 `memory/operator.md`（用户偏好）和 `memory/self.md`，每轮各带一小段（默认 1500 字符），其余靠搜索。运行时证据在写入时带上 workspace 的 id，搜索默认限定在当前 workspace。检索对中文做二元切分。原文："一条记忆若成了 workspace 必须遵守的规矩，就应升格写进 workspace 的指令文件或仓库文档。"没有"记住"工具（RFC 列为未决） |
| 多 agent | 委派出去的是一个 Task，归创建它时的那条 WorkItem 名下；子 agent 的结果作为任务结果回来。子 agent 拿到的上下文以那条 WorkItem 的快照加明确交代为中心，不是父 agent 的整段对话。委派不扩大权限。要隔离改文件就开 git worktree，非 git 目录没有隔离办法 |

## Codex

来源：`learn.chatgpt.com/docs/automations`（Scheduled tasks）与 `/docs/projects`（Projects and chats），2026-10-01 下载原文阅读。

- 不属于任何 project 的对话是一段普通对话，留在最近对话列表里。"事情变大了，就把它移进一个 project。"
- 一个 project 可以挂多个目录，其中一个为主目录。
- 定时任务是独立的东西，统一列在 Scheduled 里，它同时是收件箱。创建时说明每次运行是回到当前这段对话，还是每次新开一段。
- 长期的项目约定写在 `AGENTS.md` 或提交进仓库的文档里，"这样以后的对话也能用到"。

## Antigravity

来源：`antigravity.google/docs/projects/`，2026-10-01 下载原文阅读。

- 把一级概念从 Workspace（紧绑单个本地仓库）改成了 Project（"你希望 agent 工作的全部上下文与文件夹的一份配置"），一个 Project 可以跨多个目录；目录分"没有 Git 的本地目录"和"Git 检出"两种。
- 不带参数启动时，所有对话落进同一个默认 project。
- 在一个 project 里开新对话时选"本地模式"或"新 worktree 模式"；后者的理由是"防止并行的子代理互相冲突"。
- 把对话挪到另一个 project 是"分叉"：复制当前对话并在那边继续。

## DeepSeek Harness

来源：`docs/subsystems/workspace.md`，未读代码。workspace 是"用户工作的一个目录的持久记录"，加属于它的对话列表；对模型不可见，不带记忆；身份是生成的编号，"绝不是路径"。它的提醒模块（`docs/subsystems/schedule.md`，未读代码）支持"多久之后、某个时刻、每隔多久"，存盘，到点作为一条新消息回到原对话。

## jj

来源：`jj help -k glossary`，jj 0.45.1。change 是"一个随时间演变的提交"，change id 在改写（描述、变基、合并）后不变。一个仓库可以有多个 workspace，即多份工作副本，"就是 Git 所说的 worktree"，各有自己的工作副本提交。操作日志记录每次操作，并发操作在日志里分叉再合并。

## Emacs

来源：Emacs 32 的 `lisp/progmodes/project.el`；`project-tab-groups` 0.1 的源码；维护者的 Emacs 配置。2026-10-01 阅读。

- `project.el` 由 `project-find-functions` 这个钩子认定 project，默认只挂了按版本库认定的 `project-try-vc`；`project-vc-extra-root-markers` 让"含有某个标记文件的目录"成为 project，文档写明可用于版本库之外的 project。
- `project-current` 在需要时会给任意目录返回一个 "transient" project。
- 已知 project 的列表持久保存（`project-list-file`）；`project-switch-project` 切换；`project-buffers` 按 buffer 的 `default-directory` 判断归属。
- tab-bar 的每个 tab 保存自己的窗口布局；tab 可以归入 tab 组。
- 维护者的配置开着 `tab-bar-mode`、`global-tab-line-mode` 和 `project-tab-groups-mode`：切换 project 就是选中或新建这个 project 的 tab 组，`project-kill-buffers` 关掉该组。配置里这一段的标题是 "Workspace：tab-bar + tab-line"。
