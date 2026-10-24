# Superchat 架构复查（第三轮）

**结论：这批修复挡住了第二轮的原始反例，但“旧记录的旁路”和“插件撤销误伤另一份安装”仍能破坏承诺；应保留现有执行主干，补齐对象读取、资源所有权和结果交付三处边界。**

审阅日期：2026-10-11，America/Los_Angeles。受检提交：`75e8337004d8f13971b70712d8e288a8d7cb123c`；增量：`fcccf3aa..75e83370`。前两轮报告只作历史证据，不把它们的判断直接当作现状。

本轮只写本目录。未修改产品、既有测试、既有文档、两轮旧报告及受保护的 M3 文件；未提交、未改变版本历史。对 515 个产品/文档/测试/旧报告/受保护文件及指定用户配置做前后摘要比较，变化为零，见 [文件校验](evidence/unchanged.json)。运行所需临时目录在 `~/.cache/sc-codex-review/round3/`，不在 `/tmp`。

## 1. 先看修复到底关闭了什么

下面的“部分关闭”不是否认修复：**原来那条路确实被挡住，另有实测旁路仍到达同一类错误。** “关闭”也只覆盖写明的契约，不代表任意 Lisp 都受 Workspace 限制。

| 编号 | 用人话说 | 本轮状态 | 原探针实际输出与进一步证据 | 当前代码位置 |
|---|---|---|---|---|
| F1 | B 不该拿 A 的工作记录当自己的 | **部分关闭** | 旧探针在查找 `A-PRIVATE-GOAL` 时失败；三个工具动作均返回 `belongs to another workspace`。但旧 `work.focused` 仍使 B 的普通请求读到 A，并清掉 A 的等待、补写结果：R3-01 | `superchat-work.el:235–245, 524–530`；旁路 `:441–475, 966–975, 1009–1035` |
| F2 | B 不该列出、取消 A 的定时器 | **关闭（受检工具边界）** | 旧探针在查找 `A-PRIVATE-REMINDER` 时失败；取消收到归属拒绝。15 项对象范围测试及本轮脚本嵌套调用护栏保持对象不变 | `superchat-timer.el:222–237, 699–707, 743–756` |
| F3 | 唤醒没发出去，不能把等待吞掉 | **关闭（原故障点）** | 原探针通过并输出 `restores the wait, remains due, and has no active Run`；另跑 14 项唤醒故障测试 | `superchat-work.el:736–771, 817–854` |
| F4 | 工作功能关着时，不能答应到点继续 | **关闭（关行后新登记的缺口）** | 旧探针期望 `waiting.for = "time"`，实际为 nil；缓存的工具函数也拒绝，未创建等待。另有“行还开着却被别的安装拆掉计时器”的独立问题 R3-08 | `superchat-work.el:289–310, 537–550, 561–573` |
| F5 | 开始工作时给的进展、接续点、等待不能丢 | **关闭（start 的声明范围）** | 旧探针的 `should-not :progress` 失败，实际为 `"Already done"`；10 项既有测试验证接续点、等待、上弦和非法组合先拒绝 | `superchat-work.el:532–574` |
| N1 | 别的任务还在等，不能带着失败任务无限重试 | **部分关闭** | 原探针期待 6 次，实际 **1 次**：它未推进时钟，新等待门确实挡住了回调；15 项既有测试推进时钟后验证额度。但插件重挂清掉额度和等待：R3-03 | `superchat-timer.el:402–441`；重置 `:592–609, 760–770` |
| N2 | 执行在发送返回前就结束，也要留通知 | **部分关闭** | 原探针输出 `started=1 terminal=1 notices=((...))`，所以“无通知”断言失败；11 项既有测试含同步成功、失败、幂等通知。但撤销另一份 changes 安装后，同一反例重新出现：R3-08 | `superchat-timer.el:347–351`；`superchat-changes.el:809–835` |

旧文件完全未改，原样加载：

- [baseline-probes.log](evidence/baseline-probes.log)：退出 **1**，5 项中 1 项符合断言、4 项不符合；F3 是第二轮已改为“修复保持”的护栏，不是缺口断言。
- [delta-probes.log](evidence/delta-probes.log)：退出 **1**，`(= tries 6)` 的实际表达式为 `(= 1 6)`。
- [notice-probe.log](evidence/notice-probe.log)：退出 **1**，已有一条通知，`should-not` 不成立。

这些非零退出不是产品回归，也没有被改写成绿色。旧探针在第一次失败后不会继续证明其后所有断言；记录未被写坏等闭环证据来自 [object-scope](evidence/object-scope.log)、[work-tool](evidence/work-tool.log) 和本轮新增护栏，而不是从旧探针提前停止处推出来。

### 额外查过的旁路

| 路径 | 实际结果与证据范围 |
|---|---|
| 其他工具间接调用 | 本轮 `elisp_script → work_record / timer_cancel` 经真实工具循环仍被拒绝，未泄漏测试目标/提醒、未取消对象。[composition-probes](evidence/composition-probes.log) |
| 子代理、被唤醒的对话 | `object-scope` 的子代理测试真的委派、读取产品写下的报告，再以子会话身份调用工具，父记录写入被拒绝；唤醒会话能更新和完成自己的记录。子代理的“规则/请求 extras”不等于它的 Tape 有 Workspace 绑定 |
| 临时对话、无身份调用、同 Workspace 另一会话 | 既有对象范围集覆盖临时会话只看自己的 timer、临时会话不能操作项目工作记录、同域共享；无身份分支静态核对为拒绝。没有把任意显式 buffer/file 读取误说成 Workspace 文件沙箱 |
| 历史 Tape | 本轮实造修复前已存在的 `work.focused` 行形状并发送普通消息，复现 R3-01；既有 timer 故障集覆盖缺少 `awaits-start` 的旧 fired 行不会被擅自补跑 |
| 功能卸载、缓存函数、重新挂载 | 原全局卸载后的新等待/新 timer 均明确拒绝；重新挂载恢复可用。额外复现同进程重挂绕过重试条件、第二份安装清掉第一份资源，见 R3-03/R3-08 |
| 批注重生成、编辑上一条 | 19 项聊天批注集实际执行 regenerate 和 edit-last-message：前者再挂批注、后者恢复标记。本轮另测请求检查器、聊天 View 重开、两次未完命令，不把这些当作同一路径 |
| 获准的 `eval_elisp`、shell、用户自己的全局命令 | 按现行决定不受这两个对象工具的范围门限制；没有执行危险逃逸，也不把这种明确保留的能力列作绕过漏洞 |

## 2. 新增实证发现

编号表示**本轮新增的确认**，不表示每个缺陷都由最后一批提交新引入。R3-02 是把旧轮源码疑点变成复现；R3-07 在 `b954094e` 已如实登记，但此前没有探针；R3-08 的计时资源部分沿用旧实现，通知 hook 部分涉及本批新代码。

严重度是本地单用户产品的修复优先级：P1 为跨工作范围/正常组合会失约，P2 为有限条件下的记录或功能错误，不是网络安全评分。

| ID | 普通解释 | 严重度 | 探针确认 | file:line |
|---|---|---|---|---|
| R3-01 | 门口不让读，自动附带材料却仍把别人记录带进来，还替它改状态 | P1 | 是 | `superchat-work.el:441–475, 966–975, 1001–1036` |
| R3-02 | 执行完成后的工作结果写失败，产品把错误吞了，也不补写 | P2 | 是（定点存储故障） | `superchat-work.el:1020–1036, 909–938` |
| R3-03 | 关开一次定时器插件，就忘了刚才失败过多少次、还应等多久 | P2 | 是 | `superchat-timer.el:592–609, 760–770` |
| R3-04 | 引用的大小写不一样，仍被当作找到了原文 | P2 | 是 | `superchat-annotate.el:152–158, 165–191` |
| R3-05 | 只重开聊天窗口，批注还在，却忘了告诉模型“已经挂过” | P2 | 是 | `superchat-dispatcher.el:971–987`；`superchat-prompt-hooks.el:587–597` |
| R3-06 | 从请求检查器发送批注，模型没收到批注格式和带行号原文，答案却照批注解析 | P2 | 是 | `superchat-dispatcher.el:651–687, 741–751, 1124, 1165–1193` |
| R3-07 | 第一份批注没回来就再问一次，旧回答没有被新回答替换，两批叠在一起 | P2 | 是 | `superchat-annotate.el:340–361, 403–410, 562–567` |
| R3-08 | 撤掉某个预设的插件，误拆仍在使用的全局通知和计时资源 | P1 | 是 | `superchat-changes.el:829–835`；`superchat-timer.el:560–568, 764–770`；`superchat-work.el:778–781, 1045–1055` |

### R3-01：旧“当前记录”还在走后门

**普通解释：** 给工具加了门锁，但发消息时自动夹带的材料和自动补记没用同一把锁。旧数据指错一次，以后不需要模型再报编号就会越界。

- **代码事实：** `superchat-work-current`（`:186–196`）只取最后一个 focus 且排除已完成记录；`superchat-work-request-text` 虽筛选同 Workspace 的 `records`，却单独把全局查到的 `current` 完整渲染（`:472–475`）。用户输入钩子直接清它的 user wait（`:966–975`）；结束观察者也以它为补记目标（`:1009–1035`），均不调用新 `--for`。
- **复现：** A 建目标 `R3-A-PRIVATE-GOAL` 并等用户；B 的 Tape 只加入一条旧版合法的 `work.focused`，不塞目标正文。B 的直接 read 被拒绝；B 发一个无关普通问题，实际 provider 请求出现 A 的目标，A 的等待变 nil，A 多出 B 的 `continuable` outcome。
- **证据：** [domain-probes.el](evidence/domain-probes.el)，`r3-domain/legacy-focus-leaks-through-collector-and-clears-wait`；[成功复现日志](evidence/domain-probes-final.log) 的 `R3-01 OBSERVED`。
- **推论：** F1 的工具入口关闭不等于模型可见材料与后续状态归属关闭；现有数据升级不应依赖所有 focus 历史本来正确。
- **未验证：** 用户真实数据库中这种旧行的数量；本轮没有读取或迁移用户数据。没有假定新工具还能制造这种行。

### R3-02：结束之后的补记没有可见失败

**普通解释：** “模型这轮结束了”和“工作账本记好了”是两件事。现在第二件事失败时，第一件事照常显示完成，账本的缺口也没有报警。

- **代码事实：** `work--on-turn-completed` 在 `ignore-errors` 内追加 facts/outcome（`:1020–1035`），回报另有 `ignore-errors`（`:1036`）。它已在 Runtime 终态之后；错误被吞掉，Event 层没有机会记录这个异常。`work-arm-all`/`wake-due` 不修普通 Run 缺失的 outcome（`:909–938`）。
- **复现：** 对一条正常工作记录发送真实脚本模型回合，仅在 `superchat-tape-append-event` 收到 `work.outcome` 时抛错。命中一次；`run.completed=1`，outcomes 为零，fault ring 数量不变。恢复正常写入后调用实际恢复扫描，outcome 仍为零。
- **证据：** `r3-domain/post-terminal-work-write-failure-is-silent`；[探针](evidence/domain-probes.el) / [日志](evidence/domain-probes-final.log)。
- **推论：** 第二轮留下的（b）现在是**已确认缺陷**。不应把 `completed` 宣传为所有领域补记均成功；应显式记录/显示补记失败，并可幂等补齐，不重开 Run。
- **未验证：** 真实磁盘故障、SQLite 中途提交丢回执、work.facts 与 work.reported 各自失败的所有组合。这里不指控 Runtime 的真正迟到工具 settlement；那是另一条保留终态的契约。

### R3-03：重挂插件绕过同进程重试约束

**普通解释：** 刚被告知“今天这次已经试够了”或“还得等五分钟”，关开插件后却又马上试了一次。

- **代码事实：** 新 `--try-task` 已在尝试前检查次数和 `not-before`（`:412–420`）。但每次挂载都安排 `arm-all`；它调 `catch-up`，无条件清空两个表（`:592–609, 766`）。变量/函数说明的范围是“同一个 Emacs”，不是“同一次安装”。
- **复现一：** `retry-limit=0`，启动故障后普通 rescan 保持 1 次；同一 Emacs、同一虚拟时刻卸载重挂真实插件，变 2 次 `timer.sending`。
- **复现二：** `retry-limit=2`、等待 300 秒，普通 rescan 不发；重挂后虚拟时间增加 **0 秒**，再次发送。
- **证据：** [domain-probes-final.log](evidence/domain-probes-final.log) / [boundary-probes.log](evidence/boundary-probes.log)，相应 [domain](evidence/domain-probes.el) 和 [boundary](evidence/boundary-probes.el) ERT。
- **推论：** 重启后允许再试的策略被误扩大为任意重新挂载。应分开“从 Tape 重新上弦”和“新进程重置重试历史”。
- **未验证：** 本轮故障都发生在真正接受 Run 前；未证明外部副作用重复执行。没有反对现行“进程重启后重新尝试”的明确策略。

### R3-04：批注不是严格按原字引用定位

**普通解释：** 原文写的是 `TARGET`，模型引的是 `target`，程序仍把意见挂上去。在代码里，大小写能代表不同名字，不能把这种匹配称作“照原样找到”。

- **代码事实：** `--quote-on-line` 用 `search-forward`，未局部绑定 `case-fold-search`（`:152–158`）。行号及 ±2 边界确实存在，但字面匹配受目标 buffer 的大小写搜索设置影响。
- **复现：** 真实 annotation 请求，原文只有 `TARGET`，脚本答案 `1 | target | LOWERCASE-REMARK`；实际覆盖文字是 `TARGET`，批注成功。原文和修改计数不变。
- **证据：** `r3-annotation/quote-search-is-case-folded`，[探针](evidence/annotation-probes.el) / [日志](evidence/annotation-probes-02.log)。
- **推论：** `04e81358` 的“引用确实在该处”保证还缺确定的大小写语义。用局部精确搜索即可，不需要换定位架构。
- **未验证：** Unicode case table 的所有映射、真人对模糊匹配的偏好。若要允许大小写近似，应另明确规则，而不是依赖用户的搜索设置。

### R3-05：“按会话记住”实际只活在聊天 buffer

**普通解释：** 没有重启 Emacs，也没清批注；只把聊天关掉再打开，模型就失去那句防止重复修改原文的说明。

- **代码事实：** 记录是 `defvar-local superchat--annotated-buffers`，Session id 只是这份 buffer 状态的标签（`dispatcher:971–987`）；后续 collector 只读当前 View 的局部值（`prompt-hooks:587–597`）。批注 overlay 本身活在另一个 source buffer。
- **复现：** 聊天批注确实挂上两条；关闭 View，用现有 `superchat--render-session-view` 重开同一个 Session，再发普通问题。原文仍有两条 overlay；provider 历史仍有 `>annotate`，系统提示不再有“已挂上/已完成”说明。
- **证据：** `r3-annotation/reopening-view-forgets-hung-fact-while-overlay-lives`；[最终日志](evidence/annotation-probes-02.log)。
- **推论：** `992104fb` 正确防止了失败请求和其他 Session 被误告知，但把会话交付事实放进 View 的寿命中，仍不满足同一会话后续消息的承诺。**这不等同于要求重启后恢复 overlay**；同进程重开就能复现。
- **未验证：** 失去这句话后真实模型是否再次编辑原文、发生频率；本轮只证明 provider 输入缺少防护事实，不声称脚本模型造成了额外修改。

### R3-06：检查器漏掉批注请求的准备

**普通解释：** 发出去的是普通问题，回来却按“行号、引用、意见”表格阅卷；正常散文也会被记成看不懂。

- **代码事实：** `--preview-chat-prompt` 造的 context 没有 `:chat-annotate`（`dispatcher:659–670`），所以批注 collector 没有格式和原文可加；预览随后冻结为 `:chat-prepared-context`（`:741–751`）。发送时 `--chat-envelope` 才建 annotation spec、封装 Sink、把工具设为 none（`:1124, 1165–1193`），但不会补跑已经冻结的 collector。
- **复现：** 经真实 `superchat-act` 前缀检查器和 `superchat-request-send` 发 `Mark the issue >annotate`。预览及 provider 都没有 `1| alpha one`，系统没有 `LINE | QUOTE | REMARK`，工具确实为零。答案 `An ordinary prose answer.` 仍被 annotation Sink 计为 1 条 unparsed。
- **证据：** `r3-annotation/inspected-annotate-has-sink-but-no-row-contract`；[探针](evidence/annotation-probes.el) / [日志](evidence/annotation-probes-02.log)。
- **推论：** 同一个去向的准备不能只放在普通发送入口。检查器要么完整预览同一 spec，要么明确拒绝该组合，不能发送后才换交付约定。
- **未验证：** 用户手动在检查器中补齐格式时的所有组合；真实模型自发猜中格式不构成产品保证。接受记录已把检查器列为未测路径，本轮将其变成实证。

### R3-07：两次未完的批注命令叠加

**普通解释：** 连问两次同一段文字，第二次本应换掉第一次意见；现在两份都留下，列表统计却只记后一份。

- **代码事实：** 公共命令在**发送时**删旧 overlay（`:562–567`）；命令 Sink 在回答到达时只 place（`:403–410`），不像聊天 Sink 在交付时再删范围内旧批注（`:520–526`）。`--place` 每次覆盖单份 report（`:357–361`）。
- **复现：** 第一个脚本答案延迟 0.15 秒，第二次命令在第一份回来前发出；走真实 Runtime 排队。最终同一处有 `FIRST` 和 `SECOND` 两条，最新 report 的 `:placed` 却为 1。
- **证据：** `r3-annotation/two-pending-command-answers-accumulate`；[日志](evidence/annotation-probes-02.log)。
- **推论：** 两个入口对“替换旧批注”的时点不一致。一个有批次身份、确定替换规则的交付函数即可处理，不必取消所有旧 Run 或引入队列。
- **未验证：** 不同 Session 同时注同一 source、等待期间原文改行及多段部分重叠的完整矩阵。`b954094e` 是记录此限制的文档提交，不是声称修好了它。

### R3-08：插件清理没有精确到自己的安装

**普通解释：** 两个房间都在用同一个总开关，撤掉其中一个房间却把总电源关了；控制台还显示另一个房间正常供电。

- **代码事实：** 组合 API 允许同一插件在不同 Scope 各有安装（`superchat-plugin.el:377–393`，预设路径 `superchat.el:1826–1840`）。changes 的每份安装向同一个全局 hook 加同一个函数，清理时按函数名全局删除（`changes:829–835`）；timers/work-records 的清理分别 `disarm-all` 全局表（`timer:764–770`、`work:1051–1055`），不是该安装独占的资源。work 的全局用户输入 hook 也采用同样的增删方式。
- **复现一（直接重新打开 N2）：** 保持全局 changes 活跃，另建一个合法的子 Scope、挂 changes，再只 dispose 子 Scope。全局安装仍 active，迟到关联 hook 已消失。完整同步 timer → send → Runtime 路径得到 `terminal=1 started=1 notices=0`。
- **复现二（计时承诺）：** 全局行下已经上弦的一个提醒和一个 work time wait；在另一个 Scope 各挂同名行，运行安装回调，再只 dispose 此 Scope。两个全局行仍 active、两个领域“可用”检查仍为真；Tape 中的提醒/等待还在，两个 armed 表却都为空。
- **证据：** `r3-composition/disposing-second-changes-mount-removes-global-late-hook` 和 `.../disposing-second-timing-mounts-disarms-global-promises`；[探针](evidence/composition-probes.el) / [日志](evidence/composition-probes.log)。
- **推论：** 这不是 Kernel 的精确安装清理坏了，而是插件在 Scope 清理里撤销了共享全局副作用。单查 row active 不能证明它承诺的调度仍存在。对全局单例资源，可以禁止第二份安装或由单一全局所有者/引用计数管理；不能让任意安装 dispose-all。
- **未验证：** 本轮用与 Preset 相同的公开 mount/Scope API，而非真人在 UI 中切换特定自定义技能。没有声称默认只挂一份的配置每次都会漏通知；计时探针验证实际计时器被撤销，未真的等一小时。

## 3. 第二轮留下的三件事，今天怎么定性

### （a）一次性 agent skill 不挂声明的 plugins：真实存在，但属于明确接受的功能限制

**普通解释：** 同一张技能卡写在句中，只拿到人设等请求设置；写在开头，才安装卡上声明的附加能力。

代码事实：`superchat-preset.el:183–239` 的 `superchat-preset-apply` 改 Turn，不挂插件；`superchat-dispatcher.el:1673–1676` 的 one-message 分支跳过 activation；真正挂载在 `superchat.el:1826–1840`。本轮定义仅在挂载时贡献唯一系统标记的测试插件，用同一个技能各发一次：正文调用人设到达、mount count=0、能力标记缺失；开头调用 count=1、标记到达。[探针](evidence/open-items-probes.el) / [日志](evidence/open-items-probes.log)。

**判定：不是本批新增回归，也不违背 2026-10-10 明写的“不切换时没有 plugins”决定；但能力缺口是真的。** 不能把它概括成“整个预设一次性生效”。支持它需要另定生命周期，不建议为了本轮审阅擅自临时切换 Session。未测任意第三方插件组合和真实模型表现。

### （b）Run 结束后的事实写失败被吞：已确认缺陷 R3-02

不是说所有迟到事实都被拒绝，也不是让 Run 因补写工作摘要而重新启动。已确认的范围是工作记录 `turn-completed` 观察者：真正抛出的存储错误无可见故障记录，普通 Run 的缺失 outcome 没有被现有扫描补齐。

### （c）没有一处能看齐所有“挂着的事”：缺口真实，不能直接等同于已承诺界面的回归

**普通解释：** 工作记录表、后台完成通知和定时器文字各管一块；想知道“到点了却没启动”和“启动过没把握”，还得回相应对话找。

- 代码事实：work 列表是 Work Records（`work:604–638`）；notice 列表只收已经终结的无人执行（`changes:709–728, 783–803`）；timer tool 列 pending 与最近 24 小时 fired，不列 occurrence id、started/unconfirmed 状态（`timer:699–741`）。
- 实测：发送失败的 due-unstarted task 在 work/notice 两表均缺席；timer_list 显示 `No pending timers` 加 fired 的文本，不给该项 id。随后让一条无关真实脚本 Run 发生，驱动现有 retry 分类为 `timer.unconfirmed`，对话收到说明，但 tool list 文本前后完全相同、通知仍无条目。[boundary 探针](evidence/boundary-probes.el) / [日志](evidence/boundary-probes.log) 的 `OPEN-C`。
- **判定：** 统一查看能力尚未实现是实证；“必须有单一面板”仍是第二轮建议而非已接受的当前承诺，不列成第九个产品缺陷。当前 timer_list 把不同启动状态压成同一种 fired 描述，是这项建议的具体价值，不是要求新建任务队列。
- 边界：未做真人可发现性测试；不能从几个表没有该项，推成 Tape 没记录。unconfirmed 的对话说明确实存在。notice 跨进程不保留是已接受范围，不在本轮重新判错。

## 4. 各提交的新增行为与文档复核

| 提交 | 本轮判断 |
|---|---|
| `589e6266`, `a14ec1df` | 普通单安装路径有持续模式行通知；同步补通知实际有效。新的全局 hook 清理存在 R3-08，故 N2 不能按所有合法组合关闭 |
| `04e81358`, `a72ae118` | 共用 overlay 定位/显示，未改原文；±2 与选区边界由既有 24 项实测支持。精确引用有 R3-04；发送期替换有 R3-07。块的文本布局被检查，颜色/窗口折行未做人眼验收 |
| `b954094e` | 仅把未完成请求重叠的问题补进决定记录；本轮复现了该已披露限制，不误记为该提交造成的回归 |
| `844b264d`, `992104fb` | 普通聊天保留历史/规则、无工具、下一条恢复工具、失败/空定位不虚报、其他 Session 不串报都有正向证据；检查器和同 Session View 重开仍有 R3-06/R3-05 |
| `b768713e` | work/timer 模型工具门及 nested-script 门有效，用户全局命令没有被误禁；legacy current 的 collector/自动状态写入仍漏，R3-01 |
| `72d9a3e8` | 共享重试回调不再提前重试耗尽/未到时间的项；安装边界清状态仍绕过，R3-03 |
| `3a34db85` | start 附带进展/接续/等待确实保存；等待不合法先拒绝 start/update；关 work-records 后新 time wait 拒绝。未把此结论扩大为所有 action 的任意多余参数都受校验 |
| `75e83370` | 全局 timers 行卸载时 create 明确拒绝且不写，list 说明关闭，cancel 仍可用；这不修复已有计时器被第二份安装撤销的问题 |
| `498d0239` | 指定的四条陈述均已改：唤醒 Run 从 Tape 读；fired 与 started 分开；通知不只回显区；in-place 按 Workspace 路由。当前源码与受检行为支持这些更窄的说法 |

仍有**文档残留，不冒充新的运行缺陷**：

1. `docs/roadmap.md:375` 仍说 one-message skill “Not built ... (undecided)”，下一节 `:377–379` 已说 implemented，本轮 skill 探针也证明已实现。应把旧段标成当时状态或移除那句未完成声明。
2. `docs/roadmap.md:367` 仍用 “Still open ... echo-area line only”，后面 `:385–387` 已记录通知；作为 10 月 7 日历史可保留，但不该继续读成今天现状。
3. `docs/architecture.md:92` timer 测试数写 12，当前是 15；`:101` 的 “until the user looks at it” 不够准确：仅打开列表不清除，RET 打开具体会话、d/D 才清除（`changes:733–773`；11 项通知集实跑）。
4. 批注决定的真实运行总结说大小写不同的引用挂不上，当前 `search-forward` 实测能大小写折叠匹配。真实运行记录可以记当时现象，但不能代替确定的搜索规则（R3-04）。

这些是静态陈述与本轮行为证据的对照；没有给文档文字变更补造 ERT 失败，也没有改这些文档。

## 5. 哪些架构正确，应当保留

- **只有一个执行所有者。** 新 timer、work wake、annotation 仍走 driver → Runtime；annotation 不是另一个模型引擎。`superchat-pipeline.el:508`、`superchat-runtime.el:3336, 2604`；Kernel、smoke、M3 支持这条主线的既有行为，不是所有扩展组合正确的证明。
- **工具权限与领域归属分开是对的。** F1/F2 不是要重写权限管线；最小领域门已经挡住正常和 nested-script 工具。保留用户全局管理、可审批 full-control 与默认窄工具的不同语义。
- **事实与解释分开是对的。** Timer 的 fired/sending/started/unconfirmed、Work 的 woken/outcome 不应合并成一个“完成”布尔值；不确定时不自动重跑的保守恢复值得保留。R3-02 应补事实交付，不能重新开已封口 Run。
- **Sink 与 buffer 所有权分开是对的。** 聊天批注包装原 Sink，不把输出偷偷移到别的 Session；原文是 overlay 而非写入。R3-05 指出的是交付事实寿命，不是要把 source buffer 交给 Runtime 管。
- **Scope 的注册撤销机制无需重写。** 本轮 R3-08 的反例明确落在插件手动管理的全局 hook/timer 上，而非精确到安装的注册清理本身。保留 retire-before-dispose、接受时的原 Sink、不可重开终态和 append-only Tape。

## 6. 最多五条结构建议，按价值排序

以下是审阅建议，不另立路线图，也不授权生产修改。

| 顺序 | 建议与具体位置 | 不做会失去什么 |
|---|---|---|
| 1 | **让模型可见的“当前工作记录”只有一处带 Session 的解析。** 在 `work:186–196, 441–475` 引入/复用有归属语义的解析，并在 `:966–975, 1009–1035` 明确哪些自动动作允许以 current 为目标；子代理快照入口 `superchat-subagent.el:899–903` 也按同一语义检查。历史全局用户列表保持原样 | 新工具虽安全，旧 Tape 仍持续泄漏、清等待、污染结果；修一次入口会在另一个消费者复发 |
| 2 | **明确全局资源只能由谁关闭，把安装寿命与进程内重试寿命分开。** `changes:829–835` 用精确可撤销注册/安装独占闭包或受控单例；`timer:560–568, 592–609, 764–770` 与 `work:778–781, 1045–1055` 不让一份安装 disarm 另一份的资源；禁止不支持的重复安装也比半工作强 | 合法 Preset 撤销能消灭别处已许诺的提醒；row active 检查会给假信心；N1/N2 的局部修复再次失效 |
| 3 | **给执行结束后的领域补记一个可观察、可幂等恢复的结果。** 从 `work:1020–1036` 去掉无交代的吞错，围绕 `(work-id, run-id, fact-kind)` 核对和补齐；用现有 Tape/故障出口，不新建 Runtime、不把所有观察者变终态硬依赖 | 人看见 completed，却无法知道工作账本少了一次结果；普通 Run 的缺口长期保留 |
| 4 | **批注准备和交付共用一份有身份的规范。** `dispatcher:651–751, 989–1014, 1124–1193` 在冻结检查器之前确定 spec；`annotate:152–191, 340–361, 392–423, 496–543, 562–567` 统一精确匹配和批次替换；“已挂上”至少脱离 View 寿命，若需跨进程语义再以追加交付事实区分“曾挂上”和“现在仍有 overlay” | 普通发送能用，检查器却损坏；重开 View 丢语义；连续命令给重复意见和互相矛盾的计数。无需换成新界面框架 |
| 5 | **把现有读模型拼成一个薄的状态总览，再决定是否需要更多机制。** 读取 `work:604–650`、`timer:124–180, 443–469` 及 unconfirmed 事实、`changes:738–803`，明确 pending/due-unstarted/unknown/finished-unseen，并提供回原会话的入口；不把它做成新的持久任务队列 | 记录虽然存在，人仍无法一次分清“还没到点”“已到但没启动”“不确定”“已完未看”；会把有记录误认为有交代。这条需用户先接受产品范围 |

没有证据要求新 Runtime、新 Event 总线、通用任务队列或按文件长度拆模块。一次性技能的 plugins 问题先按接受记录标明限制；本轮不把支持完整临时 composition 偷塞进建议的修复范围。

## 7. 证据清单、已确认与未覆盖

### 实际执行

所有 Emacs 检查**串行**，使用第二轮 `run-checks.py` 的同一 package 初始化式，额外把临时文件和 workflow 目录固定到绝对 cache 路径。`SMOKE_REAL`、`SMOKE_BASELINE` 从环境去除。Emacs 32.0.50，llm 0.32.1；未调用真实模型、未执行 `.scratch/task-sim/` 中任何内容。

| 检查 | 实际结果 | 证据 |
|---|---|---|
| Kernel | 退出 0 | [kernel.log](evidence/kernel.log) |
| canonical smoke | `SMOKE OK (0 mismatches)` | [smoke.log](evidence/smoke.log) |
| 受保护 M3（原文件未改） | 139：137 expected，2 real-provider skipped，0 unexpected | [m3.log](evidence/m3.log) |
| object-scope / work-tool / timer-tool | 15 / 10 / 5，全部符合断言 | [scope](evidence/object-scope.log)、[work](evidence/work-tool.log)、[timer](evidence/timer-tool.log) |
| timer-start-fault / work-wake-fault / unattended-notice | 15 / 14 / 11，全部符合断言 | [timer-fault](evidence/timer-start-fault.log)、[wake](evidence/work-wake-fault.log)、[notice](evidence/unattended-notice.log) |
| annotate / annotate-chat / skill-once | 24 / 19 / 5，全部符合断言 | [annotate](evidence/annotate.log)、[chat](evidence/annotate-chat.log)、[skill](evidence/skill-once.log) |
| 原第二轮探针 | 6 个“缺口存在”断言变红；F3 修复护栏保持绿 | 上述三份原探针日志 |
| 本轮 domain / annotation / open-items / boundary / composition | 4 / 4 / 1 / 2 / 3，共 **14** 个观察探针符合断言；其中包含缺陷复现、功能限制与正向护栏，**不是14项产品验收通过** | [domain](evidence/domain-probes-final.log)、[annotation](evidence/annotation-probes-02.log)、[open](evidence/open-items-probes.log)、[boundary](evidence/boundary-probes.log)、[composition](evidence/composition-probes.log) |
| package load | 退出 0，记录 Emacs/llm 位置；有 `cl-typep` 优化警告，未判为新缺陷 | [package-load.log](evidence/package-load.log) |

可复跑命令逐项列在 [review-manifest.json](evidence/review-manifest.json)，执行器为 [run-review.py](evidence/run-review.py)；[交付校验](evidence/report-qa.json) 核对本地链接、日志存在、最终探针数量和文件摘要。例如：

```sh
python3 reports/architecture-review-main-2026-10-11/evidence/run-review.py \
  my-domain-rerun emacs \
  -l reports/architecture-review-main-2026-10-11/evidence/domain-probes.el
```

**保留失败过程：** 本轮 domain 探针初次组合夹具重复加载 ERT 定义，之后两次是本轮新文件括号错误；annotation 首跑一个用例在重开 View 时继承已删除的夹具目录。只修本轮探针加载/括号/绝对测试目录，未改产品或既有夹具。原始退出码 255/1 及日志均保留；这些失败不算产品发现。最终对应日志是 `domain-probes-final.log` 和 `annotation-probes-02.log`。

### 已确认

1. 原修复确实改变了具体行为，不是只多写了文档；F2/F3/F4/F5 的上述边界保持。
2. F1/N1/N2 的更宽闭环仍有实测反例；8 个新确认发现均有独立 ERT 和输出。
3. 主干检查正常；这说明本轮复现不是整个 Runtime 或测试环境都坏了，不说明产品目标已全部实现。
4. `498d0239` 的四项纠正已落地；另外残留的历史/当前混写如上列出。

### 未确认与未覆盖

- 未跑真实 DeepSeek/Qwen、用户日常 Emacs、GUI 颜色/窄窗口折行/鼠标/审批焦点、真实模型是否重复编辑批注原文。
- 未强杀进程、断电、真实数据库损坏或跨进程竞争；存储失败是明确限定到 work.outcome 的故障注入；重启恢复只重跑已有模拟边界。
- 未跑全量字节编译、全部历史脚本、完整依赖审计、大 Tape 性能；未审计所有 Telega/emacsclient、第三方插件或所有自定义 Preset 组合。
- 未重跑前两轮的真实模型实验；没有把既有报告的通过率当本轮实测。没有读取用户实际工作记录以估算受影响数量。
- 本轮未发现需要改 M3 断言或翻转 known-failure 标记的结果。给 leader 的信息是：M3 原文件 137 expected / 2 skipped，新增缺陷集中在它没有覆盖的领域组合边界。

**最后的判断：先修 R3-01 和 R3-08，再补结果写入与批注入口；现有主干足够承载这些修复，不需要重写。**
