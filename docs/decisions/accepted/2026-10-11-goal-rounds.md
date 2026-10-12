# Decision: 目标轮次：给工作记录加一个驾驭员（照 DeepSeek Harness 的 goal loop）

Status: accepted 2026-10-12, v1 implemented. 用户对九个问题的裁定由 leader 转达，原话："九个问题都按建议，第 8 条先不定。" `/work` 的两种行为（见"实现"一节）是实现者从方案里那句例子推的，leader 回复"按你推的做"。冒烟基线 V 项 34→35 由用户批准（leader 转达）。`superchat-work-rounds-when-started` 的默认值：用户裁定默认开（leader 2026-10-12 转达），代码未改。被丢弃的轮次计入上限：用户没有裁定，按现在的实现。

下面"Problem"到"要用户裁定的九个问题"是 2026-10-11 的提案原文，只改了这一段状态；裁定、实现和证据在文末三节。

证据的范围：GOAL-ROUNDS.md 全部是读代码得出的（DeepSeek Harness `c389f96`，Superchat `e99036b8`），没有跑探针，没有真实模型，没有真人。

## Problem

当晚的事实：一个 91 条勾选任务，四次执行都停在"说了下一步、没调工具"。Tape 3470 行 finish check 已用过一次，3516 行再次只说话，3517 行 `run.completed`。每一次都要用户再打一句"继续"。

Superchat 已经有 DeepSeek Harness（下称 DSH）目标循环需要的三样东西：

- 目标对象：工作记录（`work_record` start / update / wait / done / read；状态 completed / waiting-user / waiting-other / continuable；Run 末结局只看事实，不读答复的字）。
- 停前的缝和排队：`superchat-runtime--turn-stopping`，按会话的 follow-up FIFO。
- 谁发起的：`run.started` 上的 Actor（`lead` / `timers` / `work-records`，59d24169）。

缺的只有驾驭员：一次执行结束、当前记录仍是 continuable 且开着自动续时，没有东西开下一轮。

## 两个前提

1. **v1 只在对话窗口活着时跑。** 轮次的 Envelope 走 `superchat--build-chat-request`（读 `current-buffer` 的会话、预设、工具、级别），`superchat-runtime-submit` 要一个活的 `:buffer`，`dispatch-next` 把 Sink 已死的排队项按 `sink-dead` 丢掉。窗口一关，下一轮不会发生。无人看守要等切片 6（裁定 5：唤醒的新对话不必是 buffer）。
2. **审批仍是同步的小缓冲区提问。** 轮次省掉的是打"继续"，省不掉每次工具调用的 y。离开键盘有用要等切片 5（裁定 4：审批在对话和表上回答），再加上仍悬着的无人看守权限级别。

## DSH 怎么做（`c389f96`，代码事实）

- 每会话一条目标：`objective`、`phase`（active | paused | blocked | complete）、`maxGoalRounds` 默认 256、`revision`；`roundsStarted` 从会话日志投影。
- 激活 armed / disarmed 只在进程内，从不落盘；会话重开即 disarmed；人 `resume` 才重新 armed。
- 驾驭员：agent 空闲、目标 active 且 armed、没有别的排队输入、落盘检查点通过，才用 `followup` 排一条新 turn（不是同 turn 再一步）。到上限 → blocked `round-limit`。agent 出错、max-tokens 结束 → disarm；用户中止一轮 → pause。
- 模型工具：`get_goal`；`create_goal` 只在直接的人类 turn；`update_goal` 的 edit | pause | resume 要人类 turn，complete | blocked 在该目标的当前轮内也允许；blocked 要 ≥ 3 轮并给具体原因。轮内 complete / blocked 之后强制再走一步只写收尾话。
- 用户命令 `/goal <objective> | clear | edit | pause | resume`。
- 代价：每轮一个完整请求，历史全带（DSH 靠压缩控长度）。

## 对到 Superchat

| DSH | Superchat 已有（事实） | 缺 |
|---|---|---|
| goal 对象 | 工作记录及其四种状态；Run 末结局只看事实（K7） | 无 |
| armed / disarmed | 无。`superchat-work--armed` 已是"唤醒定时器已设"，名字被占 | 进程内激活态，另起名 |
| `followup` | `superchat-runtime-enqueue-follow-up`（写 `queue.queued`）加 `superchat-runtime--schedule-dispatch-next`；`dispatch-next` 在上一 Run 静默后起下一条并写 `queue.dispatched` / `queue.dropped` | 在 Run 末排一条轮次 Envelope 的人 |
| `turn-stopping` 缝 | `superchat-runtime--turn-stopping`；M3 钉死 inject / steer / submit 三种做法 | 无 |
| `requireDirectHuman` | `run.started` 的 Actor | 用它判"谁能开自动续" |
| `roundsStarted` 投影 | 无 | 每轮一条 `work.round`，从 Tape 数 |
| round-limit → blocked | `work.waiting :for user` 已有，表已列 | 到上限时写它，带 detail；或新词 `blocked`（问题 9） |
| max-tokens → disarm | 预算收尾写在 `response.completed` 和终态 payload 的 `:tool-budget`（`:action finalize`） | 驾驭员读它 |
| `/goal` 命令 | 无 | `/work <goal>`：v1 要有 |

## Proposal（建议，一条一行）

1. **轮次是同一对话里的新 Run**，不是同一 Run 里再一步。Tape 的 Run 边界留住，`C-c C-k` 落在轮间，结局按 Run 从事实判，表已按 Run 建模。
2. **工作记录就是目标对象**，不另加"goal"。激活态进程内、不落盘；重启即不续，表上回到 "Can be continued"。
3. **开自动续的权限用 Actor**：记录在 Actor 为 `lead` 的 Run 里 start，才开；`timers` / `work-records` / 子代理的 Run 不能开。轮内 done / wait 照旧允许。
4. **轮数从 Tape 数**（每轮一条 `work.round`），重启不归零。
5. **上限默认 256**，按记录计，用户可设；到上限写一条等待（`work.waiting :for user`，detail "round limit 256"），表今天就能列，不加行类型。不与 09-29 的决定冲突：那条去掉的是每任务的工具轮上限。
6. **停即暂停**：轮中 `C-c C-k` → 结局 `stopped`，记录暂停；用户说"继续"时模型在 `lead` Run 里调 `work_record resume`（新 action），或 `/work resume`。不是"下一条消息自动续"。
7. **v1 不强制收尾请求**：最后一轮的答复加切片 3 的通知就是收尾。这是与 DSH 的有意差别，省一个请求。
8. **v1 不设 blocked 门槛**：`wait_for user` 就是 DSH 的 blocked。
9. **会话有排队或 steer 时不排轮**；Run `failed` / `cancelled` / 预算收尾之后不排。
10. **轮次提示词**是该 Run 的第一条用户消息，Actor 用新的 `goal-rounds`（不加进 `superchat-changes--unattended-actors`），同一 Run 带 `work.round`：记录 id、goal、Round N/Max，"先 read 记录和工作区，不信自己早先的叙述；做实际进展并核验；做完就 done 带 conclusion；要等人就 wait_for user；还有活就直接结束本轮，下一轮会来"。

两个实现上的硬约束（来自 GOAL-ROUNDS.md）：

- Run 末排轮用 `superchat-runtime-enqueue-follow-up` 加 `superchat-runtime--schedule-dispatch-next`。不能用 `superchat-runtime-admit-envelope`：观察者跑时本 Run 已是终态，它会直接返回 `(:status process)` 而不提交。
- 轮次 Actor 用新词 `goal-rounds`，不得放进 `superchat-changes--unattended-actors`。

## 要改的地方

- `superchat-work.el`：激活态表（新名）；`superchat-work--on-turn-completed` 之后，结局 continuable 且开着且轮数 < 上限且会话无排队 → 构造轮次 Envelope 并排队。`work_record` 加 `resume`。
- `superchat-tools.el` 的工具描述和操作说明收集器：把第 10 条那几句告诉模型。渲染进最终工具面，冒烟基线会改一行，要用户批准。
- `superchat-pending.el`：新状态 "Continuing, round N/max"，和"正在做的记录不列成可继续"的隐藏逻辑配合。
- 构造 Envelope：v1 借 View（`with-current-buffer` 走 `superchat--build-chat-request`）；切片 6 之后改走无 View 的路径。
- `superchat-tape.el`：注册 `work.round`（若选 `blocked` 还有 `work.blocked`）；Actor 词表加 `goal-rounds`。

## 不碰

- `superchat-llm.el`：finish check 原样。
- 09-23 的决定不翻：它拒的是"目标循环当默认停止规则"。这里只在记录开着自动续时才动。
- 范围只聊天窗口：不含 inline、子代理、workflow、headless。
- `wait_for time` 的唤醒照旧：轮次是"现在还有活"，唤醒是"以后再做"，并存。
- K7 不翻：done 是模型的声明，结局从事实判。
- M3 回归集不改；若它与新事实或新 Actor 有差异，带证据报告给 leader。

## 代价与风险

- 每轮一个完整请求，历史全带。
- 开着自动续时会多一次重复请求（先 finish check 再轮）。
- finish check 的提示说"现在做，或交最终报告"，轮次提示却说"还有活就直接结束本轮"。两句相反，开着自动续的模型会先后收到两种指令。
- 从不调 done 的模型会跑到上限。256 轮乘以有人按 y，是没有上限的人时。刹车只有上限和 `C-c C-k`。
- 只靠模型推断意图，记录可能根本不会建：当晚四次执行一次也没建记录，工具描述写着 "Not for … anything one execution finishes"。所以 v1 要有用户命令。
- 新事实和新 Actor 进 Tape，冒烟基线若变动要用户批准。

## 测试

`scripts/goal-rounds-regression.el`，11 条，对应 GOAL-ROUNDS.md 的 11 条。在 `e99036b8` 上 8 条红、3 条通过；每条红在哪条断言、当时的值，见该文件开头的表。通过的三条（第 8、9、10 条）是护栏：今天没有任何东西会排轮，所以"不该排"自然成立，它们要到驾驭员写出来之后才有分辨力。第 8 条覆盖定时任务、被唤醒的记录和子代理。

脚本里模型的回答都写成陈述已做的事。回答若预告下一步，finish check 会在同一次执行里多发一次请求，把留给轮次的那一步脚本吃掉；这也印证了"代价与风险"里那条重复请求。

## 要用户裁定的九个问题（建议在前）

1. **轮次是什么。** 建议：同一对话里的新 Run。另一种：同一 Run 里再走一步。
2. **谁能开自动续。** 建议：两者都行，用户命令 `/work <goal>`，以及模型在 `lead` Run 里 start。另一种：只认用户命令。
3. **轮数上限。** 建议：默认 256，按记录计，用户可设。另一种：更小的默认值。
4. **停掉之后怎么续。** 建议：显式 resume 才续（模型在 `lead` Run 里调 `work_record resume`，或 `/work resume`）。另一种：下一条消息就续。
5. **要不要强制收尾请求。** 建议：不要，最后一轮的答复就是收尾。另一种：照 DSH 再走一步只写收尾话。
6. **blocked 的门槛。** 建议：v1 不设。另一种：照 DSH，至少 3 轮并给具体原因才许撒手。
7. **轮次的 Actor。** 建议：新词 `goal-rounds`，不算"Superchat 自己发起"。另一种：复用 `work-records`。复用的后果是每一轮结束都走通知路径、进模式行计数、在表上成一行"已结束没看"，256 轮就是 256 条通知，而你坐在旁边看着。
8. **无人看守的权限级别。** 没有建议。这是切片 7 离开键盘有用的前提，研究里仍悬着；v1 不依赖它。
9. **到上限怎么记。** 建议：复用 `work.waiting :for user`，detail 写 "round limit"，状态词和表的行类型都不动。另一种：照 DSH 加第五个词 `blocked`。

## 裁定（2026-10-12）

第 1、2、3、4、5、6、7、9 条按建议。第 8 条（无人看守的权限级别）没有定，v1 不依赖它。

## 实现（v1）

行为：

- 开自动续的两条路：用户发 `/work <目标>`（建记录、开自动续、把目标作为用户消息发给模型，模型看不到命令本身）；或模型在 Actor 为 `lead`、没有父 Run、入口为 chat 的 Run 里 `work_record start`，且 `superchat-work-rounds-when-started` 非 nil。
- 一次 chat 入口的 Run 结束时，当前记录是 continuable、开着自动续、这一 Run 不是 failed / cancelled / 预算收尾、会话没有排着的输入，就用 `superchat-runtime-enqueue-follow-up` 加 `superchat-runtime--schedule-dispatch-next` 排一轮，并写一条 `work.round`（`:work-id :round :limit :queue-id`）。轮次的 Run 的 Actor 是 `goal-rounds`，不在 `superchat-changes--unattended-actors` 里，所以不留通知。对话里写一行 `* Round N/Max on work record …`。
- 停：记录 completed；记录进入等待（自动续随之关掉，用户回答后要 resume 才再续）；到上限（写 `work.waiting :for user`，detail 含 "round limit N reached"）；这一轮被停掉、失败或被预算收尾。
- 续：`/work resume`（不问模型，直接排下一轮），或模型在用户自己的 Run 里调 `work_record resume`；别的 Run 里调会被拒绝。`/work pause` 关掉自动续；不带参数的 `/work` 显示状态。
- 激活态只在进程内（`superchat-work--continuing`）；重启后不续，轮数从 Tape 数（`superchat-work-rounds`），不归零，表上回到 "Can be continued" 并写 `rounds paused at N/Max`。跑着的那一行写 `round N/Max on work record`。
- 对话已关时仍照约束排一个会被丢弃的项，留下 `queue.dropped`，回显区说一句。

改的文件：`superchat-work.el`（驾驭员、`resume`、`/work`）、`superchat-pending.el`（两处文字）、`superchat-tools.el`（`work_record` 的说明和 `resume`）、`superchat.el`（登记 `/work`）、`superchat-tape.el`（只一行：把 `work.round` 登记为认可的事实类型，不登记每轮都会被拒收；提案的"代价与风险"写了这一点，"要改的地方"没列）。`superchat-llm.el` 没有碰。

名字：提案里的暂定名都没有改（`work.round`、`goal-rounds`、`resume`、`superchat-work-round-limit`）。提案之外新加：`/work pause`、不带参数的 `/work`、`superchat-work-rounds-when-started`、`superchat-work--continuing`、`superchat-work-rounds`。

实现者定的，用户可以改：

- `superchat-work-rounds-when-started`（默认 t，用户已裁定默认开）。开着时，只要模型在用户的 Run 里开了记录，Run 结束时记录没做完就会接着做下去，即使用户的意思是"以后再说"。`work-record` 套件里有两条用例正是这种情形，各自关掉了这个开关。
- 记录进入等待后自动续就关了。
- 会话有排着的输入时不排轮，等那条输入的 Run 结束再看。
- 轮数在排队那一刻计入；被丢弃的轮次也计。

## 证据

只有脚本测试（Tape 加脚本模型）。没有真实模型，没有真人。

- `scripts/goal-rounds-regression.el`，16 条，全过。在 `d051f21c`（实现之前）上，当时的 13 条里 10 条红、3 条过；各红在哪条断言见该文件开头的表。第 8、9、10 条实现前必然通过；第 14、15、16 条是实现之后补的，没在旧代码上跑。
- 变异（副本上）24 处：第一轮发现 21 处。漏的三处里，一处是变异本身写错，改对后被第 1、6、7 条发现；一处缺用例，补了第 16 条（轮数按各自的记录数）后被发现；一处仍未被发现：去掉"没有父 Run"这道判断。子代理 Run 的 Actor 本来就不是 `lead`，它开记录也本来就被拒绝，所以这道判断今天是多出来的一重保险，留着没有删。
- 第 8、9、10 条的分辨力：去掉"只认 lead"、放宽发起者 → 第 8 条红；去掉"有排队就不排" → 第 9 条红；去掉入口种类的判断、任何 Run 结束都排 → 第 10 条红。
- 受影响的现有测试（都不属于 M3 回归集）：`work-record` 两条用例各关掉 `superchat-work-rounds-when-started`；`approval-wait/the-clock-stops-for-the-question-and-continues-after` 的等待上限从 20 秒放宽到 40 秒（它原来要 19.5 秒，工具说明变长后是 20.5 秒；说明随后也压短了）；`work-record` 的夹具每个测试开始时清空 `superchat-work--continuing`。
- 冒烟基线：`scripts/smoke-expected/V.txt` 的 `commands-remounted` 34→35（多了 `/work`），用户批准。
- 全量检查的结果写在提交说明里，不写在这里。

## 没有证实、没有做

- 真实模型会不会在该 done 时 done、会不会跑到上限；真人看着一轮轮接下去是什么感受。
- finish check 和轮次提示两句方向相反，没有处理；开着自动续时每轮可能多一次请求。
- 一轮被审批问题卡住时的样子没有测。
- 两个前提仍在：v1 只在对话 buffer 活着时跑；审批虽已不占住 Emacs（切片 5），每次仍要人答。
- 第 8 条没有裁定。
