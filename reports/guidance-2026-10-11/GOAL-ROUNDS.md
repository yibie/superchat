# 切片 7：目标轮次（照 DeepSeek Harness 的 goal loop）

来源：用户 2026-10-11 晚，"应当向 deepseek harness 那样"，对着当晚 91 条勾选任务四次执行都停在"说了下一步、没调工具"的事实（Tape 3470 行 finish check 已用过一次，3516 行再次只说话，3517 行 `run.completed`）。
DeepSeek Harness（下称 DSH）读的是 `~/Documents/reference-code/deepseek-harness`，`c389f96`；Superchat 是 HEAD `e99036b8`。本文全部是读代码，没有跑探针，没有真实模型，没有真人。

## 一句话

Superchat 已经有 DSH 目标循环的目标对象（工作记录）、停前的缝（`turn-stopping`、按会话排队的 follow-up FIFO）和"谁发起的"（Actor，59d24169）；缺的只是驾驭员：一次执行结束、当前记录仍是 continuable 且开着自动续时，没有东西开下一轮。切片 7 加这个驾驭员，带上限，停了就暂停，重启就不续。

**两个前提，先说清。** (1) v1 只能在对话窗口活着时跑：轮次的 Envelope 走 `superchat--build-chat-request`（`superchat-dispatcher.el:1318`，读 `current-buffer` 的会话、预设、工具、级别），`superchat-runtime-submit` 要"一个活的 `:buffer`"（`superchat-runtime.el:3339`），`dispatch-next` 把 Sink 已死的排队项按 `sink-dead` 丢掉（其文档）。窗口一关，下一轮无声消失，正是复审 L2 的形状；无人看守要等切片 6（裁定 5）。(2) 审批仍是同步小缓冲区：轮次省掉的是你打"继续"，省不掉 24 次 y；离开键盘有用要等切片 5（裁定 4）加研究里悬着的无人看守权限级别。

## DSH 怎么做（`c389f96`，代码事实）

- 目标对象：每会话一条。`objective`、`phase`（active | paused | blocked | complete）、`maxGoalRounds` 默认 256、`revision`，`roundsStarted` 从会话日志投影（`packages/goal/goal/src/types.ts:44-83`，`goal/src/index.ts:244,253`）。
- 激活 armed / disarmed 只在进程内、从不落盘；会话重开即 disarmed；人 `resume` 才重新 armed（`goal/src/index.ts:255-265, 287-290, 361-379`）。
- 驾驭员：agent 空闲、目标 active 且 armed、没有别的排队输入、落盘检查点通过，才用 `followup` 排**一条新 turn**（不是同 turn 再一步）`<goal_round>`：Objective、Round N/Max，"把工作区和工具结果当真、别信自己早先的叙述、取证后再标完成、没做完就留给下一轮"（`goal-round-driver/src/index.ts:137-205`，`prompt.ts:13-26`）。到上限 → blocked `round-limit`（`:166-171`）。agent 出错、max-tokens 结束 → disarm；用户中止一轮 → pause（`:246-250, 259-282, 329-339`）。
- 模型工具：`get_goal`；`create_goal` 只在直接的人类 turn，允许从用户请求推断意图；`update_goal` 的 edit | pause | resume 要人类 turn，complete | blocked 在该目标的当前轮内也允许；blocked 要 ≥ 3 轮并给具体原因（`tool-goal/src/index.ts:44-48, 112-122, 284-311`；`authority.ts:99-117`）。轮内 complete / blocked 之后强制再走一步只写收尾话、不许再调工具（`wrapup.ts`）。
- 用户命令 `/goal <objective> | clear | edit | pause | resume`，显示 phase、rounds、activation（`command-goal/src/index.ts`）。
- 代价：每轮一个完整请求，历史全带（DSH 靠压缩控长度）。

## 对到 Superchat

| DSH | Superchat 已有（事实） | 缺 |
|---|---|---|
| goal 对象 | 工作记录：`work_record` start / update / wait / done / read（`superchat-tools.el:2019`，`superchat-work.el:537`）；状态 completed / waiting-user / waiting-other / continuable（`:164-170`）；Run 末结局只看事实、不读答复的字（K7，`:1058-1070`） | 无 |
| armed / disarmed | 无。`superchat-work--armed` 已是"唤醒定时器已设"，名字被占 | 进程内激活态，另起名 |
| `followup` | 按会话的 follow-up FIFO：`superchat-runtime-enqueue-follow-up`（写 `queue.queued`，`superchat-runtime.el:4751`）加 `superchat-runtime--schedule-dispatch-next`，`dispatch-next` 在上一 Run 静默后起下一条并写 `queue.dispatched` / `queue.dropped`（`:4843-4920`） | 在 Run 末排一条轮次 Envelope 的人 |
| `turn-stopping` 缝 | `superchat-runtime--turn-stopping`（`:1526`）；m3 1740-1800 钉死 inject / steer / submit 三种做法 | 无 |
| `requireDirectHuman` | `run.started` 的 Actor：`lead` / `timers` / `work-records`（59d24169） | 用它判"谁能开自动续" |
| `roundsStarted` 投影 | 无 | 每轮一条 `work.round`，从 Tape 数 |
| round-limit → blocked | `work.waiting :for user` 已有，表已列 | 到上限时写它，带 detail；或新词 `blocked`（决定 9） |
| max-tokens → disarm | 预算收尾写在 `response.completed` 和终态 payload 的 `:tool-budget`（`:action finalize`；`superchat-runtime.el:1468-1472, 2696`） | 驾驭员读它 |
| `/goal` 命令 | 无 | `/work <goal>`：v1 要有。当晚的例子说明只靠模型推断意图，记录根本不会建（见下文） |

## 设计（建议，一条一行）

1. **轮次是同一对话里的新 Run**，不是同一 Run 里再一步。DSH 用 `followup` 不用 `steer`。Tape 的 Run 边界留住，`C-c C-k` 落在轮间，结局按 Run 从事实判，表已按 Run 建模。
2. **工作记录就是目标对象**，不另加"goal"。激活态进程内、不落盘；重启即不续，表上回到 "Can be continued"。
3. **开自动续的权限用 Actor**：记录在 Actor 为 `lead` 的 Run 里 start，才开；`timers` / `work-records` / 子代理的 Run 不能开。轮内 done / wait 照旧允许，对应 DSH 允许轮内 complete / blocked。
4. **轮数从 Tape 数**（每轮一条 `work.round`），重启不归零；DSH 的 `roundsStarted` 也是投影。
5. **上限默认 256**，按记录计，用户可设；到上限写一条等待（`work.waiting :for user`，detail "round limit 256"），表今天就能列、不加行类型；要不要 DSH 的 `blocked` 一词见决定 9。不与 09-29 冲突：那条去掉的是每任务的工具轮上限，DSH 自己保留目标轮上限。
6. **停即暂停**：轮中 `C-c C-k` → 结局 `stopped`（裁定 1），记录暂停；用户说"继续"时模型在 `lead` Run 里调 `work_record resume`（新 action），或 `/work resume`。不是"下一条消息自动续"。
7. **v1 不强制收尾请求**：最后一轮的答复加切片 3 的通知就是收尾。记为与 DSH `<goal_complete>` 的有意差别，省一个请求。
8. **v1 不设 blocked 门槛**：`wait_for user` 就是 DSH 的 blocked；模型过早撒手再加 ≥ 3 轮规则。
9. **会话有排队或 steer 时不排轮**（DSH `competingQueued`）；Run `failed` / `cancelled` / 预算收尾之后不排。
10. **轮次提示词**是该 Run 的第一条用户消息，Actor 用新的 `goal-rounds`（不加进 `superchat-changes--unattended-actors`，见决定 7），同一 Run 带 `work.round`：记录 id、goal、Round N/Max，"先 read 记录和工作区，不信自己早先的叙述；做实际进展并核验；做完就 done 带 conclusion；要等人就 wait_for user；还有活就直接结束本轮，下一轮会来"。

## 不碰

- `superchat-llm.el`：finish check 原样。开着自动续时会多一次重复请求（先 finish check 再轮），留着。另一个代价要记下：finish check 的提示说"现在做，或交最终报告"，第 10 条却说"还有活就直接结束本轮"，两句相反，开着自动续的模型会先后收到两种指令。
- 09-23 的决定不翻：它拒的是"目标循环当默认停止规则"，并写明"适合长时间无人看守的目标"。切片 7 只在记录开着自动续时才动。
- 范围只聊天窗口（同 09-23）：不含 inline、子代理（policy `never`、自己的循环）、workflow、headless。
- `wait_for time` 的唤醒（K10 新开对话）照旧：轮次是"现在还有活"，唤醒是"以后再做"，并存。
- K7 不翻：done 是模型的声明，结局从事实判，没有人核对声明（09-27 记了 DSH 也没有）。

## 要改的地方

- `superchat-work.el`：激活态表（新名）；`superchat-work--on-turn-completed`（`:1172`）之后，结局 continuable 且开着且轮数 < 上限且会话无排队 → 构造轮次 Envelope，走 `superchat-runtime-enqueue-follow-up`（写 `queue.queued`）加 `superchat-runtime--schedule-dispatch-next`。不能用 `superchat-runtime-admit-envelope`：观察者跑时本 Run 已是终态，它会直接返回 `(:status process)` 而不提交（`superchat-runtime.el:4733-4746`）；排队这条路在本 Run 静默后由 `dispatch-next` 起下一轮，并留下 `queue.dispatched` / `queue.dropped`，测试 11 靠它。`work_record` 加 `resume`。
- `superchat-tools.el:2019` 工具描述和操作说明收集器：把第 10 条那几句告诉模型。渲染进最终工具面 → 冒烟基线改一行，要用户批准。
- `superchat-pending.el:171-215`：新状态 "Continuing, round N/max"，和 FR-05 的隐藏逻辑配合，不闪。
- 构造 Envelope：v1 借 View（`with-current-buffer` 走 `superchat--build-chat-request`，绑 `superchat-automated-input 'work-records`）；切片 6 之后改走无 View 的路径。

## 今天会红的测试（写在 `scripts/`，每条写明翻红的断言）

1. `lead` Run 里 start，Run 以 continuable 结束 → Tape 恰一条 `queue.queued`，随后 `queue.dispatched`；新 Run 的 `run.started` Actor 是 `goal-rounds`；首条消息含 goal 和 `1/256`；Tape 有 `work.round` 1。
2. 轮次 Run 写了 `work.completed` → 不再排；记录 completed。
3. 轮次 Run 写了 `work.waiting user` → 不再排；表显示 waiting。
4. 轮中 cancelled → 不再排；结局 `stopped`；记录暂停；之后 `lead` Run 里 resume → 再排。
5. failed → 不再排。预算收尾（`:tool-budget :action finalize`）→ 不再排。
6. 轮数到上限 → 一条 `work.waiting :for user` 带 "round limit" detail（或 `work.blocked`，按决定 9）、表上一行、不再排；没有通知（Actor 不在 unattended 名单里）。
7. 清空 Runtime 表模拟重启 → 不续；表 "Can be continued"；轮数从 Tape 读回、不归零。
8. `timers` / `work-records` / 子代理 Run 里 start → 不开。
9. 会话已有排队项 → 不排轮。
10. `inline-ask` / `inline-act` 永不排轮。
11. View 被杀后的轮 → `sink-dead` 丢掉并有 `queue.dropped`。这是 v1 的已知限制，测出来写明，不算修好。

## 当晚的例子在切片 7 下（推论，没跑过）

确定能触发的路：用户 `/work 把 review_sheet.md 91 条按规则勾完`（决定 2），记录在 `lead` Run 里建立并开着。第一轮以 "Let me extract…" 结束 → 结局 continuable → 排第 1 轮 → 模型接着做。每轮以模型不再调工具为界；done、wait_for user、上限或 `C-c C-k` 才停。仍然每次 shell 一次 y，但不用再打一次"继续"；窗口关了则什么都不发生（前提 1）。当晚没走到的路：模型自己 start。它四次执行一次也没建记录，工具描述写着 "Not for … anything one execution finishes"，模型完全可能把 91 条当成一次就能做完的事。所以只靠模型推断意图，切片 7 当晚一样不会动。

## 代价与风险

- 每轮一个完整请求，历史全带；研究里的轻量档（方向 C）可减。
- 从不调 done 的模型会跑到上限：256 乘以有人按 y，是没有上限的人时。刹车只有上限和 `C-c C-k`。建议默认 256，但表上显示轮数。
- 新事实进 Tape（`work.round`；若选 `blocked` 还有 `work.blocked`）要在 `superchat-tape.el` 注册；新 Actor `goal-rounds` 进 Actor 词表；M3 和冒烟若有差异，基线要用户批准、M3 只能 leader 改。

## 要用户决定（建议在前）

1. 轮次是新 Run（建议），还是同 Run 的步。
2. 谁能开：用户命令加模型在 `lead` Run 里 start（建议两者，同 DSH 可推断意图）。
3. 上限默认 256（建议）还是更小。
4. 停后：显式 resume 才续（建议），还是下一条消息就续。
5. 不强制收尾请求（建议），还是照 DSH 再走一步只写收尾。
6. v1 不设 blocked 门槛（建议）。
7. 轮次 Actor 用新的 `goal-rounds`（建议），还是复用 `work-records`。复用的后果（HEAD 代码事实）：`superchat-changes--unattended-run-p`（`superchat-changes.el:492`）按 `:actor-id` 是否在 `superchat-changes--unattended-actors` 里判"Superchat 自己发起"，`work-records` 在名单里；于是每一轮的结束都走通知路径（`:947`）、进 mode line 计数、在表上成一行 "Ended, not looked at" 直到你 dismiss，审阅回显反而被跳过（`:520`）。256 轮就是 256 条通知，而你坐在旁边看着。选择是"轮次像唤醒一样被通知"还是"轮次是你自己的对话在继续"。
8. 无人看守的权限级别：切片 7 离开键盘有用的前提，研究里仍悬着。
9. 到上限怎么记：复用 `work.waiting :for user` 带 "round limit" detail（建议，`superchat-work-status` 的四个词和表的行类型都不动），还是照 DSH 加第五个词 `blocked`。
