# 给实现者的下一步指导（2026-10-11）

对象：今天的三批提交 `981719f4`、`87db657c`、`18811e04`。依据：复审报告 `reports/fixes-review-2026-10-11/`、架构研究 `reports/architecture-options-2026-10-11/`、我今天在 `evidence/` 里跑的 14 次 Emacs 检查（清单在文末）。只有脚本模型，没有真实模型，没有真人在键盘前。仓库 698 个产品、测试、文档、受保护文件前后哈希一致。

## 一句话结论

三批提交都成立，没有发现写错的地方；半关的有三处：执行结束的平方级代价没了，但一轮结束仍随这条记录的事实数线性增长（合成 1000 次执行时 2.5 秒），启动时的补记扫描形状没量；"Asks you now" 不显示问的是什么；表上按 RET 之后那行不刷新。**下一步不是再写功能**：先把表补到真人能用（切片 1，小），然后让用户亲手做清单第 5、6、9、12、13 步、只看这张表回答清单最后四个问题；等用户做的时候，实现者做切片 4 的三个小修。"runtime 没有闭环"有一个能一次关掉两处、再收尾三处的结构性改动（切片 2 + 3）："谁发起的"记在执行上，"结束、放弃、看过了"都写进 Tape，表和通知只是 Tape 的读法；代价是三种新事实和一次必须有界的查询。

## 1. 三批提交的判定

| 提交 | 判定 | 证据 |
|---|---|---|
| `981719f4` FR-01/02/03 | 成立；FR-01 **半关** | 我的 `fr-settle-probes.el` 在今天代码上：1000 次执行下一轮结束从 28.4 秒降到 2.5 秒，补记本身 175 毫秒、6 次查询（原来 23 秒、1008 次）。剩下的 2.5 秒随工作事实数线性增长（0/50/200/1000 次 → 38/189/534/2462 毫秒），来自每次结束的几次全表扫描（整轮 72 次查询），是原有形状，不是新问题。FR-02：我的两条"关开行就再试"探针都翻红（`fr-scope-probes-on-head.log`：次数表现在是 `(1 . 时间)`，第二次尝试不再发生）。FR-03：`command-load-suite.log` 通过。 |
| `87db657c` FR-04–07 | 成立；两处**半关** | 他们的 6 个新用例放到 `981719f4` 的只读导出上跑，**6 个都红**，各在点名缺陷的断言上（`pending-suite-on-981719f4.log`）。我的 4 条探针在今天代码上各在缺陷断言处翻红（`fr-pending-probes-on-head.log`）。所以 FR-05 现在有我的探针在背后：`step-5-and-6` 在 `(memq 'continuable kinds)` 处失败，kinds 是 `(running unsaved)`。半关：Asks you now 没有问题文字；表上按 RET 之后那行不刷新，要按 g。 |
| `18811e04` FR-14 | 成立 | 4 个用例通过（`long-history-suite.log`，每个约 16 秒）。读代码没找到错。两点记着：超预算时每次发送最多 12 次重排、每次回放一次对话，装得下的对话不多做；压缩入口他们自己说没查。 |

没有发现写错的。三件小的：

- FR-01 的一个边：`superchat-work--on-turn-completed` 先 `remhash` 再 `(ignore-errors (superchat-work-settle-missing …))`（`superchat-work.el:1163-1164`）。如果补记在逐次处理之外就抛错（比如 `--rows` 读库失败），这笔欠账被忘到下次启动。P3。
- FR-01 的剩余就是复审的 FR-13：启动时 `settle-missing` 不带 session，对每段聚焦过记录的对话各做一次 `superchat-work-current`（两次 `work.%` 全表扫描：`--focused-record` 一次、`superchat-work-get` 一次）加两次回放。形状没量，不是这次引入的。量法：S = 50 / 200 段聚焦过记录的对话、每段 2000 行，计 `superchat-work-arm-all` 的秒数和 SELECT 次数。
- FR-05 的新规则是"一段对话里只要有执行在跑，它的当前记录就不列可以继续"，包括用户在主对话里问别的事的时候。可接受，但真人做第 5 步会看到记录消失一会儿；写进决定。

**`step-5-and-6` 为什么失败，查到的两件事：** 在 `981719f4` 的干净导出上、从仓库根目录跑，10/10 通过（`fr-pending-probes-on-981719f4.log`）；在今天的代码上只在 FR-05 那条断言上失败，那正是修好了。所以"修之前就失败"不是代码的事，是跑法的事；我不猜是哪一种。判别法：看他们日志里第一行 `Test … condition:`。已知的一个依赖：这个探针用 `default-directory` 去找 `scripts/pending-regression.el`，当前目录必须是仓库根。我自己唯一一次失败是老版本探针把 `wrec--file` 叫了两次，触发"File a.txt changed on disk. Reread?"，批处理里读不到回答就 `end-of-file`（`reports/fixes-review-2026-10-11/evidence/fr-pending-probes.log:154`）；那个版本已经不在了。

## 2. 下一步切片（按顺序，最多 6 个）

切片 7（目标轮次，照 DeepSeek Harness 的 goal loop；用户 2026-10-11 晚的方向"应当向 deepseek harness 那样"）单独写在同目录的 `GOAL-ROUNDS.md`：排在切片 6 之后，v1 只能在对话窗口活着时跑，无人看守依赖切片 5 和 6。

### 切片 1：把表补到真人能用（只读，不加事实）

- **行为变化**：(a) "Due, not started" 分三种话：`将在 HH:MM 再试，第 n 次` / `本 Emacs 已放弃，重启后再试`，定时任务和工作记录各自如此；(b) "Asks you now" 写出问的是什么：`ask_user` 的文字在 Runtime 条目上（`superchat-runtime.el:4300` 的 `:input-question` 里的 `:question`），审批的工具名在 Tape 上（每条 `approval.requested` 的 payload 带 `:tool-name`，`superchat-approval.el:275-278`），取这次执行最后一条没有配对 `decided` 的；(c) RET 之后表自己刷新；(d) 被你停掉的记录：结局从 `failed` 改为新词 `stopped`，"Can be continued" 行写 "its last execution was stopped by you"。
- **规则放哪**：(a) 定时器模块一个只读函数 `superchat-timer-retry-state`（读 `--retries`、`--not-before`，`superchat-timer.el:81-84`），工作记录同样一个（读 `--retries`，`superchat-work.el:739`）；表只拼字。(b) 表里。(c) `superchat-pending-open` 末尾调 `superchat-pending-refresh`。(d) `superchat-work-outcome`（`superchat-work.el:1036` 那行 `'failed`）：`cancelled` → `stopped`；`superchat-work-render` 和表的措辞读它。
- **今天会红的测试**：复审的 `fr-pending/a-failed-task-reads-the-same-whether-a-retry-is-armed-or-not` 和 `fr-pending/step-12-stopped-from-the-list`（它断言表里没有 stop 字样）今天过、改完会红，照它们在 `scripts/pending-regression.el` 里写断言相反的用例；新写 `pending/an-asking-row-says-what-is-asked`。复审目录的文件不改。
- **不碰**：Envelope、Runtime、Tape 的写路径；表仍不写任何东西（`pending/reading-the-list-writes-nothing` 必须继续过）。
- **用户决定**：`stopped` 是 `work.outcome` 的新取值。问："被你停掉的执行，记录上要不要和失败分开？"建议：要。
- **做完的标志**不是测试绿，是用户在自己的 Emacs 里做清单第 5、6、9、12、13 步，只看这张表回答最后四个问题。这是研究报告定的验证步，到今天没人做过。

### 切片 2：执行自己记下"谁发起的"（研究的 A2）

- **行为变化**：定时任务和唤醒开始的执行，`run.started` 的 `:actor-id` 记成 `timers` / `work-records`；用户开始的仍是今天的 `lead`。我的探针查到定时任务和唤醒开始的 Run 都记成 `lead`（`actor-probe.log`，已复现）；用户开始的也是 `lead`，这是代码事实（聊天的 Run 请求不带 `:actor-id`，`superchat-runtime.el:3375` 默认 `lead`），没有单独探针。所以 Actor 今天分不出谁发起。
- **规则放哪**：发送那一处。`superchat-timer--start-task`（`superchat-timer.el:353`）和 `superchat-work-wake` 都经 `superchat-send-input` 并绑 `superchat-automated-input t`；两边都是 `t`，分不出谁。把这个绑定的值改成发起者的名字（定时器绑 `'timers`，唤醒绑 `'work-records`），既有的检查都是非 nil 检查（`superchat-core.el:140` 只在它为 nil 时解析），不受影响；Run 请求在这个绑定非 nil 时把它作为 `:actor-id` 带上（请求端读 `(plist-get request :actor-id)`，`superchat-runtime.el:3375`）。然后 `superchat-changes--unattended-run-p`（`superchat-changes.el:483`）和 `--notice-what` 改读那一行，不再整段回放。
- **今天会红的测试**：照 `reports/guidance-2026-10-11/evidence/actor-probe.el` 在 `scripts/` 里写一个，断言 `("timers")` / `("work-records")`；复审目录的文件不改。
- **不碰**：用户发起的 Run 的 actor；Tape 格式；`superchat-automated-input` 的两个既有职责（`superchat-core.el:140` 只在它为 nil 时解析 `>annotate` 等标记；`superchat-user-input-functions` 只对普通用户消息跑）。
- **用户决定**：冒烟基线 A–E 里没有 actor 字段（查了 `scripts/smoke-expected`），M3 的 2172 行只是复制；若跑出差异，基线改动要用户批准、M3 只能 leader 改。
- 不叫 origin：代码里 origin 已是"用户所在 buffer"。

### 切片 3：结束、放弃、看过了，都写进 Tape；表读 Tape（"没有闭环"的那一个改动）

- **行为变化**：(1) 定时任务和唤醒放弃重试时各写一条事实（`timer.gave-up` / `work.wake-gave-up`，带次数）；(2) "看过了"是一条事实（`run.seen`，actor `user`）；(3) "Ended, not looked at" 从 Tape 读：actor 是 `timers` / `work-records` 的执行，有终态、或者没有终态且不在本 Emacs 的 Runtime 里（= 被切断，不用等重开对话时的封存），且没有 `run.seen`。重启后还在；切断的也在。
- **规则放哪**：(1) `superchat-timer--try-task` 的"次数用完"分支（`superchat-timer.el:418`）和 `superchat-work--try-wake` 的同一分支（`superchat-work.el:828`）；(2) `superchat-changes-notice-dismiss`（`superchat-changes.el:748`）：它已经是"这次结束看过了"的唯一出口，表和通知列表都调它，今天改内存、以后追加事实；(3) `superchat-changes--notices` 变成读模型，可留内存缓存，但按事件刷新（Run 结束、seen 写入），不能每次重绘都查。
- **约束，不是选项**：查询必须有界：下限用上一条 `run.seen` 的 id 或 `superchat-projection` 的检查点，对所有 actor 都要；actor 过滤（所以切片 2 先做）只决定谁进通知和 mode line 计数。否则复审 P3 的形状会在全部对话上重演。如果用户同意把你自己发起的执行也列出来，还要定"你的执行什么算看过了"：`run.seen` 只由 dismiss 写，没有别的信号时每一次你的执行都会变成"已结束没看"。最省的办法是执行结束的观察者在结束那一刻写一条"结束时 View 不在"的事实，表只列有这条事实的；这里不替用户设计，只是说明问题会出现。
- **今天会红的测试**：照复审的 `fr-loop/a-scheduled-task-cut-off-mid-run-…` 和 `fr-loop/a-task-out-of-tries-…` 在 `scripts/` 里写断言相反的用例（表里有行）；新写：带一条未看通知模拟重启（`clrhash` Runtime 表加清空 `superchat-changes--notices`）后表上仍有那行。复审目录的文件不改。
- **不碰**：表本身仍不写，写在 dismiss 里；`superchat-runtime-reconcile-session` 的封存规则；通知的语义仍是"Superchat 自己发起的才通知"。
- **用户决定**：两句。"看过了要不要成为 Tape 上的事实，重启也记得？"建议：要。"你自己发起、窗口关了之后结束的执行，要不要也列成已结束没看？"今天的决定是不通知；建议：列，但只进表、不进通知计数，因为没有窗口就没有别处能看到。

### 切片 4：三个小修（随时可做，互不依赖）

- FR-08：`superchat-scope-shared-effect`（`superchat-plugin.el:115`）先调 `acquire`，成功了才把计数从 0 记到 1；失败不记。红测试：照复审的 `fr-scope/an-acquire-that-signals-leaves-a-holder-nobody-owns` 在 `scripts/shared-resource-regression.el` 里写断言相反的用例：计数为 0 且下次挂载再 acquire。不碰 `superchat-scope-effect`。
- FR-09：`superchat-pending--unsaved` 用 `(car (car marks))`：标记是新的在前（`superchat-changes-mark-buffer` 用 `cons`，`superchat-changes.el:102`）。红测试：复审的 `fr-pending/an-unsaved-buffer-two-conversations-edited-names-the-first` 今天过、改完会红，照它在 `scripts/` 里写断言相反的用例。
- FR-10：规则放读的一侧。`superchat-prompt-hooks.el:589-590` 那个收集器在告诉模型"已挂上"之前，查 buffer 还活着且 `superchat-annotate-overlays` 非空；不让 `superchat-annotate-clear` 去碰 `superchat--annotated-buffers`（`superchat-dispatcher.el:982`，别的模块的表）。红测试：照复审 `fr-annotate-probes.el` 里清掉批注后的那条在 `scripts/annotate-chat-regression.el` 里写断言相反的用例。

### 切片 5：审批问题不再卡住整个 Emacs（研究的方向 D）

- **行为变化**：审批像 `ask_user` 一样"发布一个问题、把控制权还回来、之后再决定"；期间 Emacs 可用；表的 Asks you now 行上能答，`k` 能拒。今天 `superchat-approval-confirm` 用 `read-multiple-choice`（`superchat-approval.el:144`），是阻塞读键，清单第 9 步"去别的 buffer 待一分钟"做不到。
- **规则放哪**：`superchat-approval--decide` 的最后答题者（`superchat-approval.el:287-300`）；现成机制是 Runtime 的 `:input-question`（`superchat-runtime.el:4284-4310`），审批走同一条。
- **红测试的形状**：批处理测不了"阻塞"；测"发布后函数返回、稍后一次调用决定它、Tape 上 requested / decided 成对"。
- **不碰**：审批的 Tape 事件对、策略 `never`、`--ask-while-run-lives` 的"Run 没了就放弃问题"（`superchat-approval.el:167`）。
- **用户决定，先问再动**："问题放在哪里答：小缓冲区、表、还是对话 buffer？"建议：对话 buffer 里一行加表上可答，小缓冲区只做兜底。没定之前不写代码。

### 切片 6：唤醒不再往聊天 buffer 里打字（研究的方向 B，切片 2 之后）

- **行为变化**：到点的工作记录直接把 Envelope 交给 driver，不先建 View；View 有就包它的 Sink。
- **规则放哪**：`superchat-work-wake` 的发送段（`superchat-work.el` 里 `superchat-send-input` 那处）。
- **约束**：`superchat-automated-input` 的两个职责保留（见切片 2）；`chat-turn-recorder` 要先确认不依赖 View buffer。
- **红测试**：唤醒后 `(buffer-list)` 里没有新的聊天 buffer 而 Run 结束。
- **不碰**：定时任务那条先不动，它还有"当前 buffer 的目录"的依赖，另算。
- **用户决定**："唤醒开出的新对话必须是一个 buffer 吗？"建议：不必须，Tape 有它就够。

**六条规矩**：以上切片没有一条要重开用户定下的六条（workspace 优先、无全局记忆、代理不共享会话、work-item id 指一条记录、弱模型可用、不丢整体结构）。

**要用户决定的，集中一处**（每条一句，附建议）：

1. 被你停掉的执行，记录上要不要和失败分开，用新词 `stopped`？建议：要。（切片 1）
2. "看过了"要不要成为 Tape 上的事实 `run.seen`，重启也记得？建议：要。（切片 3）
3. 你自己发起、窗口关了之后结束的执行，要不要也列成"已结束没看"？今天的决定是不通知。建议：列，只进表不进通知计数；同意的话还要定"你的执行什么算看过了"。（切片 3）
4. 审批问题放在哪里答：小缓冲区、表、还是对话 buffer？建议：对话 buffer 一行加表上可答，小缓冲区兜底；没定之前不写代码。（切片 5）
5. 唤醒开出的新对话必须是一个 buffer 吗？建议：不必须。（切片 6）

既有决定、本文不动但用户可以再定：复审 L8（转去别的记录的缺口不记）、L11（未保存标记不过重启）。

## 3. "runtime 没有闭环"：一个改动能关几处

有。就是切片 2 + 3 合起来："发起者记在执行上；结束、放弃、看过了都是 Tape 事实；表和通知只是 Tape 的读法。"

- **关掉的**：L3（定时任务被切断）、L9（没看的通知过不了重启）两处全关；L5、L6（放弃重试没有事实）从部分到关；L1 的重启那一半（`approval.requested` 没有 `decided` 加 Run 被封存 = "被切断时正在问你"）。L2（你发起的执行在窗口没了之后结束）和 L4（你的执行被切断）是用户发起的 Run，只有用户同意列出它们、并定下"你的执行什么算看过了"（切片 3 的约束里那条）之后才关；按 actor 过滤的查询本身关不了它们。
- **关不掉的**：L7（观察者自己报错只进内存故障环，可以另加一类行读 `superchat-event-faults`，便宜但不过重启）、L8、L11（用户决定）、L1 的"答题要去小缓冲区"（切片 5）。
- **代价**：三种新事实（`timer.gave-up`、`work.wake-gave-up`、`run.seen`）进事件目录和决定记录；通知计数和 `*superchat notices*` 改成事件驱动刷新；查询必须有界（切片 3）；表的"不写任何东西"保持，写在 dismiss；冒烟 / M3 若有差异要用户批准。全程 Tape 加脚本模型就能验证，不需要真实模型。

## 4. 方法上要改的

1. **"先红后绿"每次写明翻红的断言。** 第 1、3 批写了，第 2 批没写，我替他们验了（6 个都红）。以后每个决定记录写：哪条测试、在旧代码上哪条断言红、值是什么。
2. **复审探针跑不起来，是先要查的阻碍，不是交给下一个复审者的脚注。** 看日志第一行 `Test … condition:`，把日志留进证据目录；是夹具问题就在决定里写明。本次 `step-5-and-6` 在干净导出上是过的。
3. **修完跑复审者的探针，记录翻红处。** FR-01 跑了（42 → 4 秒），FR-04 到 07 没跑；翻红的那一行就是"关上了"的证据。
4. **合成数据的数字不写成产品事实。** CHANGELOG 里"一千次执行让后面每次等半分钟"来自我的合成夹具，要带上"合成数据、脚本模型"。
5. **代价说形状，不只说"读一次"。** "读一次"只对一段对话成立；启动时是 S 段对话各两次全表扫描加两次回放。要么量（上面给了夹具），要么写出形状。
6. **验证的下一步是真人，不是下一个测试。** 清单从第 5 步起没人做过，表的 18 个测试过了不等于能回答四个问题。切片 1 之后停，等用户做五步。
7. 小的：`long-history-regression.el` 文件头的命令会跑进 50 个用例（55 秒），加选择器 `"^long-history/"`。

## 证据（`reports/guidance-2026-10-11/evidence/`）

- `manifest.json`：每条命令、退出码、秒数；`initial-hashes.json` / `unchanged.json`：698 个文件前后一致。`981719f4` 的只读导出用 `git archive` 放在 `~/.cache/sc-guide/src-981719f4`，仓库没动。
- `pending-suite-on-981719f4.log`（6 红）、`fr-pending-probes-on-981719f4.log`（10/10 过）、`fr-pending-probes-on-head.log`（4 翻红）、`fr-scope-probes-on-head.log`（FR-02 两条翻红，FR-08 仍复现）、`fr-settle-probes-on-head.log`（FR-01 数字）、`actor-probe.el` / `.log`（定时任务和唤醒的 Run 都是 `lead`；用户的 Run 是 `lead` 为代码事实）。
- 五个套件：`pending-suite.log`、`work-outcome-suite.log`、`work-wake-fault-suite.log`、`command-load-suite.log`、`long-history-suite.log`；确认层在今天的代码上：`kernel.log` 通过、`smoke.log` 0 处不符、`m3.log` 139 个用例 137 过 2 跳过 0 意外（M3 集只跑不改）。
- 没做：真实模型、真人、真实数据库、真杀进程；切片 3 的查询代价没量。
