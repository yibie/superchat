# Superchat 架构选项研究：保留主干的前提下，还有哪些更好的组织方式，以及每天用的人选哪一种

研究日期：2026-10-11。受检源码：`75e8337004d8f13971b70712d8e288a8d7cb123c`，导出到 `~/.cache/sc-arch-research/src/` 后在那里跑探针；报告里的"文件:行"都指这个快照。只读研究：没有改产品文件、现有测试、现有文档，没有提交；探针和输出都在本目录的 `evidence/`。没有调用真实模型；三个探针用的是回归集自带的脚本传输。上手清单（`docs/reports/2026-10-07-hands-on-checklist.md`）还没有真人填过，所以本报告里没有任何"真人用起来怎样"的断言，"用户看到什么"全部来自代码或决定记录里的沙箱运行。

## 一句话结论

**主干该留，一处该换，先做一张表。** 一条执行主线、一个工具管线、只追加的 Tape、按 Workspace 归属——这四样三轮评审都说该留，我的探针也没有找到反例；该换的一处是"定时任务和唤醒的工作要靠往聊天 buffer 里打字才能开始"（方向 B）；对每天在 Emacs 里用它的人，最先改善体验的是把"谁发起的"记在请求上、再给用户一张"现在挂着什么"的表（方向 A），因为今天回答"谁改了什么、在等什么"要开 3 到 4 个界面；在本地 Qwen 上，第二步是给一次性用法一个轻量的默认面（方向 C）：实测规矩写满 6 000 字上限的项目对话一次请求约 27 KB、估算约 6 600 token，已经贴着 8 192 的上限；没有规矩的临时对话也要 15 KB、约 3 800 token，其中 11 KB 是 14 个工具的说明。

## 读这份报告前要知道的四件事

1. **三轮评审的结论我都复核过，不重复。** 第三轮（`reports/architecture-review-main-2026-10-11/REPORT.md`）的 8 个缺陷 R3-01 到 R3-08 和 5 条结构建议仍然成立；本报告把它们当作"无论选哪个方向都要做"的共同前提，不再逐条论证。
2. **"方向"指的是组织方式的不同，不是修一个 bug。** 四个方向各自回答一个不同的问题：A 回答"用户怎么知道现在的状态"，B 回答"自动执行怎么进入 Runtime"，C 回答"一次请求默认带多少东西"，D 回答"审批问题放在哪里问"。它们可以叠加，不互斥。
3. **证据分三等。** "代码事实"带 文件:行；"测量"带探针名和输出文件；"推论"和"假设"都写明。没有真人也没有真实模型，所以凡涉及"模型会不会更准、用户会不会更快"的话都是推论或假设。
4. **用户已经定下的五条我没有碰。** Workspace 先于记忆、没有全局记忆、代理不共用对话、工作记录是"一件工作的记录"、机制要在弱模型上好用、定位是"可信地操作用户当前 Emacs 的 AI 层"、不抛弃现有结构。需要用户重新决定的事单列在最后一节。

## 共同前提：不管选哪个方向都要先做的事

这些不是"方向"，是地基上的裂缝；读者不要把它们误当成某个方向带来的好处。

| 事 | 性质 | 证据 |
|---|---|---|
| 修第三轮的 R3-01（旧"当前记录"走后门）和 R3-08（撤销一份插件安装会拆掉另一份的全局资源） | 缺陷，P1 | 第三轮评审第 2 节；两条探针都在它的 `evidence/` 里 |
| 修 R3-02 到 R3-07 | 缺陷，P2 | 同上 |
| **不带 Run 编号的未保存列表是超线性的。** `superchat-changes-candidates` 没给 Run 编号时把本对话每一个 Run 都算进来，再对每个 Run 各发一次 `LIKE` 全表扫描找它委派的子对话（`superchat-changes.el:218` 调 `--editing-runs`，后者在 `--child-sessions` 里 `cl-loop for run in run-ids … SELECT DISTINCT session_id FROM tape WHERE meta LIKE ?`；带 Run 编号时只剩一个 Run、一次扫描）。实测 Session 到 5 000 行时 2.1 秒、20 000 行时 30 秒（P3，探针调的正是不带编号的那种）。付这笔钱的是 `M-x superchat-changes`（`:615`）、`/changes`（`:627`）和保存/撤销时选 buffer（`--read-buffer`，`:569`）——正是"事后找回"要走的路；执行结束的审阅屏（`:487`）、通知计数（`:725`）、工作记录结果（`superchat-work.el:1021`）都带 Run 编号，只付一次回放。把超线性归到 `--child-sessions` 那个循环是读代码的推论，没有单独计时 | 代码事实 + 测量 + 推论；修法是局部的（一次查询查全部 run，或用已有的 Projection），不是换主干 | `evidence/p3-fold-cost.log` |
| `superchat-response-timeout` 仍是 120 秒 | 10-05 工具数报告列为本地模型的产品问题，没有改 | `superchat-llm.el:55`；`docs/reports/2026-10-05-tool-count-on-a-local-model.md` |
| 本地 Qwen 关思考要在 `superchat-llm-reasoning-parameter-overrides` 里声明端点，否则思考会把 8 192 的窗口用完、正文为空 | 决定记录里的真实运行 | `docs/decisions/accepted/2026-10-10-remarks-hung-on-the-text.md`（"思考开着那一遍…输出 6 610 个 token 全是思考"） |
| 用户没有列出或取消定时器的命令，只有模型的 `timer_list` / `timer_cancel` | 代码事实 | `superchat-timer.el` 里没有 `interactive` 函数（`grep -n interactive` 为空） |
| 任何改 Envelope 形状或插件行清单的方向都要付两笔固定成本：冒烟基线（`scripts/smoke-expected/`）变动要用户批准；M3 回归集钉着插件名单，只归 leader 改 | 项目规则 | `AGENTS.md`、`CLAUDE.md` |

## 四个方向，先用日常的例子讲

### 方向 A：把"谁发起的"记在请求上，给用户一张"现在挂着什么"的表

**日常的例子。** 你在写草稿，两分钟前让它"到点接着做"的那件事做完了，模式行多了个 `[Superchat: 1]`。今天你要知道"它改了什么、还等我什么"，得先 `M-x superchat-changes-notices` 按 `RET` 进那段对话，再 `M-x superchat-work-list` 按 `RET` 看记录和它的各次执行，再 `M-x superchat-changes` 看未保存的 buffer——三个命令、四个界面（P2 场景 S3）。方向 A 之后是一个命令：一张表，每行一件挂着的事——"在跑（谁发起的）""等用户回答""等时间（几点）""到点了没启动""启动了但说不清""做完了你还没看""有未保存的 buffer""等审批"——每行按 `RET` 去它所在的对话，按 `s` 保存、`k` 停、`c` 完成。

**改什么。** 两件事：

1. **发起者记在请求上——用已有的 Actor，不加新字段。** 术语表里"一次执行归谁"就是 Actor，`run.started` 已经带 `:actor-id`（`superchat-runtime.el:3375`：请求没给就是 `lead`；聊天给的是 `user`，`superchat.el:4078`、`:4091`）。定时器和唤醒走的是聊天的发送路径，所以它们的 Run 今天记的 Actor 和用户发起的一样——这是从路径推出来的，没有探针。A2 要做的是让这两条路把 Actor 记成 `timers` / `work-records`（这两个模块给自己的事实用的就是这两个 id，`superchat-timer.el:330`、`superchat-work.el:203`），并由 `superchat-pipeline-validate-envelope` 认它们。今天"这次执行是不是没人发起的"靠事后回放整段 Tape 去找 `timer.started` 或"第一个 Run 且对话里有 `work.woken`"（`superchat-changes--unattended-run-p`，`superchat-changes.el:465–479`）；`timer.started` 又是发送返回之后才写的（`superchat-timer.el` `--start-task`，第三轮 N2 就是这个时序）。记在请求上以后，通知、审阅屏策略、工作记录结果的判断都读那一行，不再回放。即使将来要另加字段也不能叫 origin——代码里 `origin` 已经是"用户所在的 buffer"（`superchat-turn` 的 `origin` 槽、`[Emacs origin]` 块、`origin` 收集器）。
2. **一个读模型 + 一个命令。** 新文件 `superchat-pending.el`：把 Runtime 的活动 Run（`superchat-runtime-active-runs`）、工作记录的等待（`superchat-work-records`）、定时器的 pending / fired-unstarted / unconfirmed（`superchat-timer-list`、`superchat-timer-unstarted-tasks`）、通知（`superchat-changes--notices`）、未保存 buffer（`superchat-changes-candidates`）拼成一张表；开放的审批问题今天没有登记表，第一步先不列，留给方向 D。

**不改什么。** Runtime 的接受、封存、Sink；Tape 只追加；工具管线；Workspace 归属；工作记录的四个问题（K2）和"产品裁定结局"（K7）；审阅屏和通知的现有行为。

**落在哪里。** `superchat-pipeline.el`（认这两个 Actor）、`superchat-runtime.el`（已经写 `:actor-id`，只需不再默认成 `lead`）、`superchat-timer.el` 和 `superchat-work.el`（发送时带上自己的 Actor）、`superchat-changes.el`（`--unattended-run-p` 改读 payload）、新 `superchat-pending.el` 和一个插件行（或并入 `changes` 行，避免动 M3 钉着的名单）。

**用户会感到什么不同。** 一个命令回答"谁改了什么、现在在等什么、怎么停、怎么继续"；通知不再因为时序漏掉；执行结束时不再回放整段 Tape（P3：20 000 行时 340 毫秒）。

**代价与风险。** 这两类 Run 的 `:actor-id` 变了，冒烟基线里定时任务和唤醒的记录可能跟着变（要用户批准）；M3 集如果钉了 Actor 只能由 leader 改；表里的"等审批"要等方向 D。风险：表做出来了用户还是回答不了四个问题——那说明缺的是事实不是界面，正是第一步要验证的。

**分几步，每步可停。** A1 只做表（读现有读模型，不动 Envelope；这就是第三轮建议 5）→ A2 定时器和唤醒的 Run 记自己的 Actor，`--unattended-run-p` 改读 `run.started` 的 `:actor-id` → A3 审批问题进表（依赖 D）。

### 方向 B：自动执行不再往聊天 buffer 里打字（"主干该换的那处"）

**日常的例子。** 今天每一件到点醒来的工作都会悄悄新开一个聊天 buffer（P4 实测：唤醒前 1 个 View，唤醒后 2 个），每个到点的定时任务在对话 buffer 被关掉后会重建一个；它们把任务文字插进输入区、调用 `superchat-send-input`、失败了再把文字删掉（`superchat-timer.el:353–382`）。更要紧的是它依赖"那一刻碰巧是当前的 buffer"：P4 里把对话 buffer 关掉后，定时器在一个目录已不存在的 buffer 里醒来，任务就没启动（`superchat: scheduled task … was not started: EditorWorkspace requires a valid default directory`，`superchat-editor-workspace.el:503`）。方向 B 之后，到点的任务和唤醒像子代理、workflow、就地问、改写、批注那样直接把 Envelope 交给 driver（这条路已有 10 个生产者在用：`superchat-annotate.el:469`、`superchat-compact.el:806`、`superchat-inline-act.el:291`、`superchat-inline.el:501`、`superchat-rewrite.el:567`、`superchat-skills.el:581`、`superchat-subagent.el:1244`、`superchat-workflow.el:704`、`superchat-magit.el:92`、`ob-superchat.el:94`），对话窗口开着就把回答流进去，没开就只记 Tape，打开时从 Tape 重建（今天重开对话就是这样做的，`superchat.el:3909`）。

**改什么。** `superchat-timer--start-task` 和 `superchat-work-wake`（`superchat-work.el:817–857`）不再找或建 View、不再插文字，而是构造一个 `chat` surface 的 Envelope（复用 `superchat-dispatcher.el` 里 `superchat--chat-envelope` 的组装，但不读 buffer 局部状态），Actor 记为 `timers` / `work-records`（和方向 A 共用），Sink 是"包一层"：View 活着就调 View 的 Sink（`superchat-render.el:1648`），否则只让 Runtime 记 Tape。`timer.started` 在 Runtime 接受时就能写（接受返回 Run 编号）。

**不改什么。** "Runtime 是唯一接受 Run 的地方"（它仍然经 driver）；唤醒仍然新开一段对话（K10 说的是 Session，不是 buffer）；对话的历史、规矩、`[Work records]` 块照带（这些由收集器按 Session 取，不看 buffer——`superchat-prompt-collector--workspace-rules` 从 Session 的 `workspace.bound` 取根）。**也不能改的两道闸门**：自动发送时绑定的 `superchat-automated-input`（`superchat-dispatcher.el:1412`；定时器在 `superchat-timer.el:373`、唤醒在 `superchat-work.el:849` 绑定）让解析点只在用户亲手发的消息里认正文标记（`superchat-core.el:140`），`>annotate` 对 Superchat 自己发的消息从不生效（决定 2026-10-10），`superchat-user-input-functions` 只对用户的普通消息运行（`superchat-dispatcher.el:1472–1473`，第 9 片）。Envelope 不经 `superchat-send-input` 组装时，要么在调用 `superchat-core-prepare-turn` 时同样绑定这个变量，要么干脆不解析；否则 B 会重新打开用户 10-10 关上的口子。

**要先解决的一处依赖（代码事实）。** 用户消息那一行是 `chat-turn-recorder` 行（`superchat.el:5294`）在 `run-sealing` 时记的（`superchat--hook-record-user-message`，`superchat.el:5268`），它先检查 `superchat--turn-belongs-to-current-view-p`（`:5261`）——读的是**封存那一刻当前 buffer** 的 `superchat--session-id`。没有 View 时它是 nil 则记；但若那一刻当前 buffer 恰好是另一个对话的 View，就不记。方向 B 要让这个判断读 Request 里的 Session，不读当前 buffer。这是一处推论（我读了代码，没有用探针复现"恰好在别的 View 里"这一条）。

**用户会感到什么不同。** 后台不再冒出看不见的聊天 buffer；自动执行不再因为"你当时在哪个 buffer"而启动失败；第三轮的 N2 一类时序问题从根上没有了；R3-05 那类"记在 View 寿命里"的状态有了明确的去处（Tape）。

**代价与风险。** 这是真正动主干的一处：组装聊天 Envelope 的发送路径与 View 的耦合要拆（`superchat-send-input`，`superchat-dispatcher.el:1424`，到 `superchat--chat-envelope`，`:1013–1032`；会话编号、预设、上下文文件、来源都是 View 的 buffer 局部状态——从定时器必须 `with-current-buffer view` 才能调用它这一点推出）；`superchat--active-run-id` 这个 buffer 局部变量有 25 处读写（`superchat.el` 15 处、`superchat-timer.el` 3、`superchat-runtime.el` 3、`superchat-agents-panel.el` 2、`superchat-dispatcher.el` 1、`superchat-work.el` 1）。风险：对话里显示自动执行的那一轮（今天是文字插进输入区后由正常发送路径渲染）要改成"从 Tape 渲染"，重开对话已经这样做，但"开着的窗口实时看到它"是新路径。冒烟基线中定时任务和唤醒的事件序列会变。

**分几步，每步可停。** B1 唤醒走 driver（唤醒本来就新开对话，没有旧 buffer 要兼顾；Sink 只记 Tape + 回显区）→ B2 定时任务走 driver，View 活着时包一层 → B3 删掉插入/删除文字那段和 View 重建。

### 方向 C：请求分两档，一次性用法默认轻量，长期工作才用完整面

**日常的例子。** 你在本机 Qwen 上选中一句话让它改得正式些。今天这一问带 6 000 字的规矩（实测改写请求 11 216 字节，其中规矩 5 999 字符占一大半）；在对话里随口问一句"今天几号"，带 14 个工具的 schema（11 144 字节）、操作说明（1 268 字符）、参考页索引、技能目录、窗口布局、工作记录块——临时对话一句话 15 252 字节、估算约 3 800 token；项目对话约 27 KB、约 6 600 token（P1）。本机 Qwen 只加载 8 192 的上下文，剩给历史和回答的不到 1 600。方向 C 之后，就地问、改写、批注、临时对话默认走"轻量档"：5 个工具（`read_buffer`、`edit_buffer`、`open_file`、`eval_elisp`、`ask_user`，schema 合计 4 353 字节），不带窗口布局、技能目录、参考页索引；需要定时、记录、委派时，模型或用户换到"完整档"。

**改什么。** 在这一轮输入上加一个位置 `:tier`（和 `:destination` 一样，只在 `superchat-core--parse-input` 认，`superchat-core.el:132–228`），由入口默认（`inline-*`、临时对话 → 轻量；项目对话 → 完整，或按后端：本地端点 → 轻量）和正文标记（`/full` 或一个一次性预设 `>full`）决定；`superchat-select-tools-decision` 的 baseline（`superchat-select-tools.el:311`）和收集器（`windows`、`skill-catalog`、`reference` 索引、`work-records` 都在 `superchat-prompt-hooks.el` 注册）按档出声。一个档其实就是一个 Preset（模型、工具、系统块），`superchat-preset-apply` 已经能对一轮生效（决定 2026-10-10 "一次性技能"）。进一步的做法是"工具目录 + 按需装载"：像 Claude Code 的延迟工具那样，默认只给目录行，模型用一个工具把需要的 schema 拉进来。

**不改什么。** 权限级别；Workspace 规矩每次都带（R3 是用户定的；6 000 字的上限不动）；完整档就是今天的默认面，一个工具不少；解析点只有一个。

**用户会感到什么不同。** 在本地模型上一次性用法的请求字节减到三分之一到一半（P1 可算：临时对话 15 252 → 约 8 000，若去掉 9 个工具和四个块）；首 token 等待是否变短**没有测**；模型选错工具是否变少**没有测**——10-05 的实验说工具从 14 减到 2 通过率没变、瓶颈在写 Elisp（`docs/reports/2026-10-05-tool-count-on-a-local-model.md`），所以 C 的收益首先是"能装进 8 192"，其次才是速度。

**代价与风险。** 轻量档没有 `timer_create`、`work_record`：用户在临时对话里说"20 分钟后提醒我"，模型要先换档，弱模型可能不会换——这正是 10-01 目标批次里失败的任务类型（P3 提醒 1/3）。所以 C 不能默认用在项目对话上，只能用在"一次性"入口和临时对话，并且要给模型一句"要定时或记录就说 /full"。风险：两档的说明和测试翻倍；冒烟基线 A/E 等要变。

**分几步，每步可停。** C1 只给 `inline-ask` / `inline-rewrite` / `inline-annotate` 和临时对话一个轻量 Preset，项目对话不动（P1 已给出各入口的基线数字）→ C2 本地端点默认轻量 + `/full` → C3 工具目录按需装载。

### 方向 D：审批问题不占小缓冲区，可以挂着好几个，`C-c C-k` 一并拒绝

**日常的例子。** 清单第 9 步：它问"允许 `shell-command` 一次吗？"，你不答，去别的 buffer 待一分钟。今天那个问题是 `read-multiple-choice` 同步阻塞在小缓冲区里（`superchat-approval.el:108–147`），Emacs 本身不卡（沙箱运行 3/3），但小缓冲区被占着；两分钟后到点的另一件工作醒来又问一个，叠在上面，两个都不动直到替身用户在 300 秒时回答（roadmap 第 355 行的 step-4 观察）。在本机 Qwen 上这更常见：它的工具错误几乎都在 `eval_elisp`，而 `eval_elisp` 在默认级别下每次都问（`superchat-sandbox.el:300–327`）——这是从两条事实推出来的推论，没有在真实 Emacs 里数过。方向 D 之后，问题显示在对话里那一轮下面（或方向 A 的表里）一行，`y` / `n` 回答，可以同时挂几个，`C-c C-k` 停止时把挂着的问题一并拒绝（agent-shell 就是这样做的，见对照）。

**改什么。** 审批的"最后一个回答者"从同步的 `superchat-approval-confirm` 换成一个返回"等着"的回答者；工具管线在 `ask` 这一步挂起、用户回答后再续。代码里这条路的一半已经有：agent loop 对每一个工具都以 `:async t :callback done` 调用管线（`superchat-agent-loop.el:459`），管线有异步交付（`superchat-tool-execution.el:383`、`:826`）。另一半没有：对 llm.el 声明的 `:async` 仍按每个工具自己的（`superchat-llm.el:1762`、`superchat-agent-loop.el:551`），同步工具的回答必须同步返回，所以挂起只能对声明为异步的工具做——要么把所有工具对 llm.el 声明为异步（`ask_user` 已经是，`superchat-tools.el:2176`），要么只对 `full-control` 类工具这样做。

**不改什么。** 审批的四种结局、快照参数、`approval.requested` / `approval.decided` 审计对、Run 取消时问题被撤回、问题挂着时时钟暂停（这些都在 `superchat-approval.el` 和决定记录里）。

**代价与风险。** 这是工具管线里的改动，正是三轮评审都说"不要碰"的那层的边上；"所有工具都异步"会改请求证据里的工具描述（`superchat-llm.el:1542` 记 `:async`），冒烟基线会变；M3 集里关于审批的测试只归 leader 改。风险：一个问题挂着、用户在别处又发了消息，Run 的排队和"问题不活过 Run"的规则要重新核一遍。

**分几步，每步可停。** D1 小缓冲区不变，但 `C-c C-k` 顺手把挂着的问题当作"否"（今天它只取消 Run；这一步很小）→ D2 对 `full-control` 类工具用非阻塞回答者 → D3 多个问题并列、进方向 A 的表。

## 七个场景，逐个方向走一遍

场景取自上手清单和评审。"现状"一栏的步数和界面数来自 `evidence/p2-step-counts.md`（静态计数，每行有出处）。

### S1 基本一轮：改一个文件，带规矩（清单 1、2）

- **现状**：`M-x superchat` → 打字 `C-c C-c` → 审阅屏自动出现并拿光标 → `s`/`n` → `q`。4 步，看 2 个地方。规矩 ≤ 6 000 字随请求。出错时：回答下方有产品写的记录行；失败只有回显区一行。
- **A**：一样；多一个地方可以事后查这次执行的结局。
- **B**：一样（用户发起的执行不走 B）。
- **C**：项目对话默认完整档，一样；临时对话轻量档，看不出来，但请求小一半。
- **D**：一样（这一轮不问审批）。

### S2 轻量一次性：改写选区 / 批注（清单 3）

- **现状**：选区 → `M-x superchat-rewrite-region` + 指令 → `C-c C-a`。3 步，1 个地方。请求 11 KB，其中 6 KB 是规矩。
- **A / B / D**：不变。
- **C**：请求去掉规矩以外的东西已经很少（改写本来不带工具），C 对改写几乎没有变化；对就地问（今天 4 个工具、13.8 KB）和临时对话有变化。改写的收益在 C 里是最小的——这是一个诚实的"方向 C 不是万能"的例子。

### S3 到点醒来的工作，事后找回（清单 5、6）

- **现状**：发一条 → `s` → `q`；两分钟后它在一段看不见的新对话里做，不抢光标，回显区一行、模式行 `[Superchat: 1]`；事后 `superchat-changes-notices` `RET` → `superchat-work-list` `RET` → `superchat-changes` `s`。找回至少 3 条命令，4 个界面（其中 `M-x superchat-changes` 付的是 P3 里超线性的那笔）；"到点没启动"的任务在 work / notice 两表都不出现（第三轮 OPEN-C）。
- **A**：一个命令：表里一行"记录 X，timer 发起，已完成，1 个 buffer 未保存，`RET` 去对话 / `s` 保存"；到点没启动、不确定的也各占一行。
- **B**：不再多出看不见的聊天 buffer；"启动了没有"在接受时就知道；用户打开那段对话时从 Tape 重建，看到的一样。
- **C**：唤醒的执行走完整档（它要 `work_record`），不变。
- **D**：唤醒的执行若问审批（默认级别下 `full-control` 会问，而没人在），问题挂在表里而不是堵住小缓冲区；这一条还牵涉"没人看守的执行该用什么级别"，见"需要用户决定"。

### S4 审批放着不答，然后停下（清单 9、12）

- **现状**：小缓冲区阻塞；Emacs 不卡；第二个问题会叠上去；`n` 后不执行、不编造。停：`C-c C-k`，改动留在 buffer，`superchat-changes` 看。2 步，2 个地方。
- **A**：表里看得到"等审批"（A3，依赖 D）。
- **B**：不变。
- **C**：轻量档里 `eval_elisp` 还在，问的次数不变；只有把 `eval_elisp` 也拿出轻量档才会变，那是另一个决定。
- **D**：问题在对话里一行，小缓冲区空着；`C-c C-k` 一并拒绝；多个问题并列。

### S5 执行途中关掉窗口 / 切到别处（清单 7、11）

- **现状**：杀掉 buffer 执行继续，只有 `ask_user` 失败；`M-x superchat` 重开看到答案（从 Tape 重建）。2 步，1 个地方。
- **A**：表里能看到"在跑（用户发起）"，不用重开也知道它没死。
- **B**：这正是 B 的常态：没有 View 也能跑、能记；今天靠"View 消失不终止 Run"的修补（决定 2026-10-07）达到同样效果，B 把它变成结构。
- **C / D**：不变。

### S6 重启之后；到点没启动 / 说不清（清单 13；决定 2026-10-06）

- **现状**：`M-x superchat` 触发扫描；说不清的任务在那段对话里留一句、记 `timer.unconfirmed`；`superchat-work-list` 看到 `interrupted`。2 步；"哪个定时任务到点没启动"没有用户命令，模型的 `timer_list` 把三种状态压成一种。
- **A**：表里三行三种状态，各有去处；这是 A 对用户最直接的价值。
- **B**：重启后的补跑不再依赖重建 View（今天 `superchat-timer-retry-tasks` 要 `superchat--render-session-view`）。
- **C / D**：不变。

### S7 本机 Qwen，8 192 上下文

- **现状**：项目对话一次请求约 27 KB、估算 6 600 token；临时对话 3 800；思考默认开着会把输出用光；120 秒无进展超时；工具错误集中在 `eval_elisp`，每次都问。
- **A**：不变（A 不碰请求）。
- **B**：不变。
- **C**：一次性入口和临时对话的请求减到约一半到三分之一（可算，未在真实模型上测）；项目对话不变——所以 C 不解决"6 000 字规矩 + 完整面 > 8 192"这件事，那要靠用户把规矩写短或把上下文加载到 16k。
- **D**：问得多的时候不堵小缓冲区；问的次数不变。

## 方向 × 场景 对比表

每格：对用户的变化（相对现状）。"—"表示没有变化。

| 场景 | 现状（步数 / 要看的地方） | A 发起者 + 一张表 | B 自动执行走 driver | C 两档默认面 | D 非阻塞审批 |
|---|---|---|---|---|---|
| S1 改一个文件 | 4 / 2 | — | — | 临时对话请求约减半；项目对话 — | — |
| S2 改写 / 批注 | 3 / 1 | — | — | 改写几乎 —；就地问减 | — |
| S3 醒来的工作，事后找回 | ≥3 条命令 / 4 | **1 条命令 / 1**；到点没启动也在表里 | 不多出隐藏 buffer；启动事实在接受时写 | — | 无人时的审批不堵小缓冲区 |
| S4 审批放着；停下 | 2 / 2；第二个问题会叠 | "等审批"一行（需 D） | — | — | **一行一个，`C-c C-k` 一并拒绝** |
| S5 关窗口 / 切走 | 2 / 1 | 表里看到"在跑" | 成为常态而非修补 | — | — |
| S6 重启；没启动 / 说不清 | 2 / 2；没有用户命令看定时器 | **三种状态各一行** | 补跑不依赖重建 View | — | — |
| S7 Qwen 8 192 | 项目对话 ~6 600 token | — | — | **一次性入口减到 1/2–1/3**；项目对话 — | 不堵，但次数不变 |
| 改动落在 | — | pipeline 校验、runtime 接受、timer/work 发送、changes 读、新 pending | timer/work 发送、dispatcher 组装、render Sink、turn-recorder | core 解析、select-tools、prompt-hooks 收集器、一个 Preset | approval 回答者、tool-execution 挂起、llm 工具声明 |
| 动不动主干 | 不动（加字段） | **动一处**（自动执行的入口） | 不动（加一档） | 动工具管线边上一处 |
| 第一步多大 | A1 一个读模型 + 一个命令 | B1 唤醒改走 driver | C1 四个入口一个 Preset | D1 `C-c C-k` 一并拒绝 |

## 技术证据

### 代码事实（快照 `75e83370`）

1. 一条执行主线：11 个生产者，10 个直接 `superchat-service-call nil 'driver 'submit`，聊天经 `superchat-pipeline-submit`（`superchat-dispatcher.el:965`）；定时任务和唤醒不是生产者，它们驱动聊天 View 去发送（`superchat-timer.el:353–382`：找或建 View、插文字、`superchat-send-input`、失败删文字；`superchat-work.el:817–857`：`superchat-workspace-new-conversation` 新建 View、插文字、发送）。
2. 解析只有一个点 `superchat-core--parse-input`（`superchat-core.el:132–228`），`:destination annotate` 和 `:skill-from-body` 都记在 `superchat-turn` 的 state 上——方向 C 的 `:tier` 有现成的位置。
3. Sink 是 7 处 `superchat-sink-create`（annotate ×2、inline-act、inline、pipeline、render、rewrite）；`>annotate` 已经是"包一层"的实例（`superchat-annotate-chat-sink`）。
4. 审批同步：`superchat-approval-confirm` 用 `read-multiple-choice`（`superchat-approval.el:108–147`），`superchat-approval-request-snapshot` 同步返回结局（`:346`）。
5. 管线异步路径存在：`superchat-tool-execute` 接受 `:async :callback`（`superchat-tool-execution.el:1052`），agent loop 总以 `:async t` 调它（`superchat-agent-loop.el:459`）；对 llm.el 的声明按每个工具（`superchat-llm.el:1762`、`superchat-agent-loop.el:551`）。
6. 默认工具 13 个加 `ask_user`（`superchat-tools.el:1501–1504`）；默认插件行 33 个（`superchat-plugin.el:236–241`）；收集器 15 个（`scripts/smoke-expected/V.txt`）。
7. "没人发起"靠回放：`superchat-changes--unattended-run-p` 回放整段 Session（`superchat-changes.el:465–479`）；`timer.started` 在发送返回后才写（`superchat-timer.el:353–382`，经 `:347` 的 `--note-started`）。
8. 用户消息记录依赖当前 buffer：`superchat--hook-record-user-message`（`superchat.el:5268`）先查 `superchat--turn-belongs-to-current-view-p`（`:5261`）。
9. 工作记录与定时器的 fold 是全表 `LIKE` 扫描（`superchat-work.el:55–70`、`superchat-timer.el:107–123`）；不带 Run 编号的未保存列表对每个 Run 各扫一次，带编号的只扫一次（`superchat-changes.el:218` → `--editing-runs` → `--child-sessions`）。
10. 忙碌判断已经在 Runtime 侧（`superchat-timer--session-busy-p` 用 `superchat-runtime-active-runs`）；buffer 局部的 `superchat--active-run-id` 只用来在发送后取新 Run 编号。
11. 新建 View 要求当前 buffer 的 `default-directory` 存在（`superchat-editor-workspace.el:503`）。

### 测量

**P1 一次请求多大**（`evidence/p1-request-size-2.log`、`p1-request-size.json`，各入口的系统文字和消息原文在 `p1-*-system.txt` / `p1-*-messages.txt`）。脚本传输，序列化的是 `llm-provider-chat-request` 的完整结果（含工具 schema）。token 是估算：ASCII 字符 / 4 + 非 ASCII 字符 / 1.5，不是分词器；Qwen 的分词器对中文可能接近 1 字 1 token，所以带中文规矩的那几行可能偏低。

| 入口 | 字节 | 估算 token | 系统文字（字符） | 工具 |
|---|---|---|---|---|
| 临时对话一句话 | 15 252 | ~3 800 | 3 069 | 14 个，11 144 字节 |
| 项目对话（6 000 字规矩 + 当前记录 + 3 技能） | 27 074 | ~6 600 | 10 820 | 同上 |
| 同一对话第二轮（历史含一次 `read_buffer`） | 27 534 | ~6 700 | 10 820 | 同上 |
| 就地问 | 13 808 | ~3 300 | 6 722 | 4 个，2 006 字节 |
| 改写选区 | 11 216 | ~2 650 | 6 996 | 无 |
| 批注命令 | 11 145 | ~2 630 | 7 191 | 无 |

系统文字的组成（临时对话 3 069 字符）：操作说明 1 268、工具使用三段 819、参考页索引 365、保存归用户 278、临时对话说明 327；项目对话再加技能目录 1 563、规矩 6 158、记住说明 349。工具 schema 里最大的是 `work_record`（2 501 字节）、`read_file`（1 231）、`timer_create`（1 191）；轻量五件合计 4 353 字节。

**P3 读"现在的状态"要多久**（`evidence/p3-fold-cost.log`、`p3-fold-cost.json`；隔离 SQLite，5 次取中位数，毫秒；行是合成的 run/tool/response/run.completed 四件一组，真实一轮约 12 行）：

| Session 行数 | 回放 | `--unattended-run-p` | `work-request-text`（每次请求都付） | `work-records` | `timer-list` | `changes-candidates`（不带 Run 编号：`/changes`、`M-x superchat-changes`） | 重开 View 的历史 |
|---|---|---|---|---|---|---|---|
| 36（3 轮真实） | 1.0 | 1.0 | 1.8 | 0.1 | 0.1 | 2.4 | 0.3 |
| 1 000 | 15 | 16 | 16 | 0.3 | 0.2 | 118 | 3.5 |
| 5 000 | 72 | 76 | 97 | 1.0 | 0.9 | **2 108** | 17 |
| 20 000 | 337 | 341 | 366 | 4.8 | 4.7 | **30 012** | 106 |

读法：回放类是线性的；不带 Run 编号的 `changes-candidates` 是 Run 数 × 整张 tape 表行数（`--child-sessions` 对每个 Run 发一次 `LIKE` 扫描，而且扫的是全库，不只本 Session——这一句是从循环读出来的推论，没有单独计时）；带 Run 编号的调用只剩一个 Run，代价和"回放"一列相当。合成行里每 4 行一个 Run，比真实（约 12 行一个 Run）Run 密度高 3 倍，所以同样行数下真实对话的候选计算大约是表中的三分之一；但真实数据库里别的对话的行也会被每次扫描计入，这一项没有测。多少行算"大"没有读用户的数据库，不知道。

**P4 自动执行要不要聊天 buffer**（`evidence/p4-view-dependency-4.log`）：唤醒一条记录，聊天 View 从 1 个变 2 个；对话 buffer 被关掉后定时任务到点，在一个目录已不存在的 buffer 里醒来：`timer.fired=1 timer.sending=0 timer.started=0`，产品打印 `was not started: EditorWorkspace requires a valid default directory`；把时钟推过 300 秒的重试等待、从目录存在的 buffer 补跑：新建了一个聊天 View（0 → 1，没有显示在任何窗口），`timer.sending=1 timer.started=1`。

**P2 步数**：见 `evidence/p2-step-counts.md`。

### 推论（从事实推出，没有单独验证）

- 本地 Qwen 在默认级别下是"审批密集"的：工具错误集中在 `eval_elisp`（10-05 报告）+ `eval_elisp` 每次都问（`superchat-sandbox.el:300–327`）。
- 方向 B 的"用户消息记录"要改读 Request，否则封存时当前 buffer 是别的 View 会漏记（事实 8）。
- 真实使用中 `/changes` 的代价比 P3 表低约三倍（真实一轮约 12 行、合成的 4 行一个 Run），但随整库增长（事实 9 + P3）。

### 假设（需要真实模型或真人）

- 轻量档让 Qwen 更快、更少选错工具——10-05 的实验说通过率不随工具数变，所以这条很可能不成立；C 的确定收益只是装进 8 192。
- 一张表能让用户在清单的"最后四个问题"上答"能"——要真人填清单。

## 已证实 / 未证实 / 没覆盖

**已证实（本研究实测或代码事实）**
- 一次请求各入口的大小和组成（P1）。
- 四个读模型的代价随 Session 增长的曲线，和不带 Run 编号的未保存列表（`/changes`）的超线性（P3）。
- 唤醒新建聊天 buffer；定时任务到点时若没有 View 就新建一个，并依赖醒来那一刻当前 buffer 的目录（P4）。
- 直接向 driver 提交的生产者有 10 个，自动执行不在其中（事实 1）。
- 审批同步阻塞；管线异步路径存在（事实 4、5）。
- 没有用户命令列定时器；`superchat-response-timeout` 仍 120。

**未证实**
- 任何方向在真实 DeepSeek / Qwen 上的效果；任何真人体验；轻量档的速度收益；表能否让用户答出四个问题。
- 方向 B 的 `chat-turn-recorder` 漏记场景（只读了代码）。
- 真实数据库的行数规模和整库扫描的真实代价。

**没覆盖**
- Telega、emacsclient 两个入口；GUI 焦点和颜色；真正杀进程；并发多 Run；第三方插件；全量字节编译。
- 第三轮 8 个缺陷的修法细节（它那里已有探针）。

## 需要用户决定的事

1. **没人看守的执行用什么权限级别。** 今天被唤醒的对话用默认 `edit-buffers`，`full-control` 调用会问一个可能不在的人（`docs/architecture.md` "A woken record's new conversation has no recorded permission level"）。Claude Code 对无人值守要求显式的 `dontAsk` 或容器。这不推翻任何已定的事，但 A（表里"等审批"）和 D（非阻塞审批）都会把这个问题摆到台面上。
2. **"一张表"是不是产品范围。** 第三轮说"必须有单一面板"仍是建议不是承诺；方向 A 的第一步就是它。
3. **一次性入口和临时对话默认少给工具（方向 C）。** 这改变"模型默认能做什么"；弱模型可能不会自己换档。用户 10-04 说过"连 Qwen 3.8 都能跑得很好"，C 是朝这个方向，但要用户认可"临时对话里说'提醒我'要先 `/full`"这种代价，或者选择只对 inline 入口做。
4. **审批界面（方向 D）。** 用户 2026-09-25 对审阅屏定过"光标要自动跳到 diff"，审批问题的位置也该由用户定；D1（`C-c C-k` 一并拒绝）不需要决定，D2 需要。
5. **唤醒"新开一段对话"是否必须是一个 buffer。** 方向 B 保留 Session、不保留 buffer；K10 的原话是"新开一段只带这条记录的对话"，我读作 Session。如果用户的意思是"要能在 buffer 列表里看到它"，B1 要改成"建 buffer 但不经它发送"。
6. 可能触及已定事项的一处：Workspace 规矩每次都带（R3）在 8 192 的模型上是请求里最大的一块（6 000 字）。我没有提议改 R3；但如果用户要在本机 Qwen 上用项目对话，要么把规矩写短，要么把 LM Studio 的上下文加到 16k。这是事实，不是建议。

## 推荐与第一步

**推荐方向 A，第二步 C（只对一次性入口），B 作为随后真正动主干的一处，D 先做最小的 D1，其余等用户决定。** 理由：

- 对每天用的人，五条判断标准里第一条（谁改了什么、在等什么、怎么停、怎么继续）今天最差——找回一件醒来的工作要 3 条命令 4 个界面，"到点没启动"没有地方看——而 A 用最小的改动（一个读模型 + 一个命令，不动 Envelope）直接回答它。
- A 的第二步（发起者进请求）同时消掉 N2 一类的时序问题和每次执行结束的整段回放，并给 B 留好接口（B 的 Envelope 要带自己的 Actor）。
- C 对弱模型的收益是确定的"装得下"，但只对一次性入口；它不动主干，可以独立停下。
- B 是"主干该换"的那处，证据够（事实 1、7、8、11，P4），但代价也最大，所以放在 A 之后：A 做完，B 的第一步（唤醒走 driver）只剩发送那一段要改。
- D 的 D2 以上要动工具管线边上的东西并改冒烟基线，先让用户决定。

**先做哪一步最能验证方向对不对：A1。** 只做"一张表"——读 `superchat-runtime-active-runs`、`superchat-work-records`、`superchat-timer-list` / `-unstarted-tasks`、`superchat-changes--notices`、`superchat-changes-candidates`，一个命令、每行 `RET` 去对话——不改任何 Envelope。然后在沙箱 Emacs 里跑清单第 5、6、9、12、13 步，再让用户在自己的 Emacs 里做同样五步并只看这张表回答清单的"最后四个问题"。能答，方向成立，接着做 A2；答不出来而原因是"事实不存在"（比如等审批没有登记、到点没启动的任务没有 id），那证明缺的是 A2 和 D 的事实，不是界面；答不出来而原因是"表太慢"（P3 的曲线），那先修共同前提里的超线性。这一步里真实模型和真人的部分是将来的运行，本研究没有做。

**C 的最便宜验证**：把 P1 的 `inline-ask` 请求换成轻量档（去掉 `timer_*`、`work_record`、`read_file`、`read_reference`、`apropos`、`describe-function`、`list_buffers`、`windows`、技能目录、参考索引），在本机 Qwen 上对同一问题各跑 5 遍，记首 token 时间、总时间、答案是否基于 buffer；这是一次将来的真实模型运行。

## 同类工具对照（一手资料，详见 `evidence/comparators.md`，读取日期 2026-10-11）

- **gptel**：`@preset` 写在提示任何位置只对"a _single_ query"生效，另有 `oneshot` 作用域；有"dry run"检查请求。Superchat 的正文 `>技能`、`>annotate` 和请求检查器已是同一类。（<https://github.com/karthink/gptel>）
- **agent-shell**：`C-c C-c` 中断并"rejects all pending permissions"；header/mode line 忙碌指示。方向 D1 取自这里。（<https://github.com/xenodium/agent-shell>）
- **eca-emacs**：待批准的工具调用同时在聊天 buffer 和 `eca-workspaces` 仪表盘里，`a`/`A`/`x` 批；方向 A 的"表里有等审批"取自这里。（<https://github.com/editor-code-assistant/eca-emacs>）
- **aider**：`/ask`、`/code`、`/architect` 作为单条消息前缀"applies just to that particular message"；architect 模式把"想方案"和"写编辑"分给两个模型，理由是有的模型做不到一步到位。方向 C 的"按档"与此同类。（<https://aider.chat/docs/usage/modes.html>）
- **Claude Code**：权限模式表里 `dontAsk` 是"anything that would prompt is denied"，给无人值守用；"需要用户决定"第 1 条取自这里。（<https://code.claude.com/docs/en/permission-modes.md>）
- **Cursor**：Cloud Agents 在云端 VM 跑，交付是 PR；和 Superchat 的"操作用户当前 Emacs"定位不同，只作参照。（<https://cursor.com/docs/background-agent>）

## 证据文件

- `evidence/run-probes.py`：探针运行器；`probe-manifest.json` 记录每条命令、退出码、秒数、日志；`initial-hashes.json` / `unchanged.json` 证明现场仓库的产品、测试、文档、受保护文件前后未变。
- `evidence/p1-request-size.el` → `p1-request-size.log`（首次，改写那一步因非交互调用没设 region 而失败，保留）、`p1-request-size-2.log`（完整）、`p1-request-size.json`、`p1-<入口>-system.txt` / `-messages.txt` / `-tools.json`。
- `evidence/p3-fold-cost.el` → `p3-fold-cost.log`、`p3-fold-cost.json`。
- `evidence/p4-view-dependency.el` → `p4-view-dependency.log`（夹具漏加载）、`-2.log`（ERT 测试重定义）、`-3.log`（唤醒用例通过；定时器用例卡在 300 秒的重试等待，探针没推时钟）、`-4.log`（最终，两个用例都通过）。探针自己的这三次失败都是探针的错，不是产品发现，日志原样保留。
- `evidence/p2-step-counts.md`：七个场景的步数与界面数，每行引用 文件:行。
- `evidence/comparators.md`：六个同类工具的一手资料摘录与出处。
- 快照：`~/.cache/sc-arch-research/src/`（`git archive 75e83370`）；临时文件在 `~/.cache/sc-arch-research/tmp/`，没有用 `/tmp`。
