# 三批修改的独立复审（2026-10-11）

**一句话结论：八个修复里六个真的关上了评审找到的那条路，但第二批提交给"每次执行结束"新加了一笔随"这条记录下结束过几次执行"的平方增长的扫描——合成数据上 200 次时每次结束多等 1.8 秒，4000 次时一次扫描近 5 分钟，用户真实的次数不知道；唤醒重试的"关开插件就再试一次"没修到；这张表读得对、不写东西，但在 `-L` 加 `require` 的开发装法下它的命令根本不存在，而且审批挂着时它只说"正在跑"。**

受检提交：`abb4f199`、`4bea4054`、`437ca4e8`，基线 `75e83370`。只读：未改产品、既有测试、既有文档和受保护文件；未提交。开始时对 639 个产品/测试/文档/插件/受保护/旧报告文件做了摘要，结束时再比一次，见 `evidence/unchanged.json`。所有 Emacs 检查串行，临时目录在 `~/.cache/sc-fable-review/`，`SMOKE_REAL`/`SMOKE_BASELINE` 已去掉，没有调用真实模型，没有真人在键盘前。每条命令、退出码、耗时和日志名都在 `evidence/manifest.json`，执行器是 `evidence/run-checks.py`。

读法：下面的"代码事实"能在文件里指到行；"推论"是从事实推出来、没有单独测的；"没试过"就是没试过。严重度沿用第三轮评审的尺度：P1 是正常用法下会失约或明显变慢，P2 是有限条件下的错误，P3 是措辞、文档或还没人踩到的设计缺口。

## 1. 每一项的判定

| 项目 | 判定 | 依据；我怎么试图弄坏它 |
|---|---|---|
| R3-01 当前记录只认够得着的 | **成立** | codex 的探针在今天的代码上于 `(string-match-p "R3-A-PRIVATE-GOAL" wires)` 处变红（`evidence/codex-domain-probes.log`）：B 的请求里不再有 A 的目标。读遍 `superchat-work-current` 的六个使用者（请求块、用户回答钩子、结束观察者、补记扫描、子代理快照 `superchat-subagent.el:899`、任务定时器拒绝 `superchat-timer.el:718`），都走它。工具不带编号走 `superchat-work--focused` 再被 `--for` 按名拒绝（object-scope 套件 18 个通过）。想找"换了 Workspace 绑定之后记录悄悄不算了"的旁路：绑定在对话有第一轮之后就不能改（`superchat.el:5683-5700`），这条路不存在。 |
| R3-02 补记失败要说、之后补上 | **部分成立：说出来、补上、只补一次都成立；但补记扫描是新的平方级代价（FR-01，P1）** | 失败确实进故障环并在回显区说出来（codex 探针在 `(= fault-count ...)` 处变红；work-outcome 套件 10 个通过）；"退出后补上、只补一次、唤醒的执行补完再报一次"我另写护栏确认（`fr-settle/guard-...`）。但补记扫描本身是新的平方级代价（**FR-01**），而且挂上时会成批补写（**FR-13**）。"对话已转去别的记录就不补"是决定了的，我确认它既不补也不记故障、不说话（`fr-settle/a-record-the-conversation-moved-on-from...`）。 |
| R3-03 重挂不清零 | **部分成立** | 定时器那一侧成立：codex 两个探针都在 `smoke--wait (= tries 2)` 处变红，我的护栏 `fr-scope/guard-the-timer-row-keeps-its-count-across-a-remount` 通过。工作记录那一侧没修到：决定记录说"唤醒重试计数同样不再在补扫时清空"，字面是真的，但那个计数在尝试之前从来没人读（**FR-02**）。 |
| R3-04 引用按原字匹配 | **成立** | codex 探针在 `(ann--placed buffer)` 为 nil 处变红；annotate 套件 27 个通过。想弄坏：省略号路径（`target...`）也走同一个 `--quote-on-line`，大小写不同照样不挂（`fr-annotate/guard-the-ellipsis-path-is-exact-too`）。 |
| R3-05 "已挂上"按对话记 | **成立，但说明过头** | annotate-chat 套件里关开 View 的用例通过。codex 的探针在今天代码上报的是类型错误（变量从 buffer-local 列表变成了哈希表），不是行为证据，已注明。想弄坏：清掉批注、杀掉原文 buffer，下一条请求仍被告知"已挂上"（**FR-10**），和 docstring"活得和批注一样久"不符。 |
| R3-06 检查器按批注准备 | **成立** | codex 探针在"预览正文不含带行号原文"处变红；annotate-chat 新用例确认行号原文只出现一次、无工具。边界：预览后把目标 buffer 杀掉再发送，当场报错不发（`fr-annotate/the-inspector-send-after-the-target-was-killed`），错误文案说"这段对话不是从 buffer 发起的"，不准确但无害。 |
| R3-07 回答到达时替换 | **成立** | codex 探针在 `(= 2 1)` 处变红；annotate 新用例通过。有没有误删别的批注：整个 buffer 的 `>annotate` 到达会换掉之前命令在某几行挂的批注，这是规则本身（"同一行的旧批注被换掉"）；第二个回答为空时第一个的也被删、什么都不留（`fr-annotate/an-empty-second-answer...`）。两条都是行为记录，不算缺陷。 |
| R3-08 最后一份撤掉才拆 | **成立** | codex 两个探针都在第一处"资源没了"断言变红（钩子还在、唤醒还上着弦）；shared-resource 套件 6 个通过。决定记录没查的"同一行装两份观察者会不会跑两次"：不会——`superchat-scope-merge` 按 id 去重、近的遮远的（`superchat-plugin.el:181-190`），我用第二份 `changes` 跑一轮带编辑的执行，回显一次、审阅屏一次（`fr-scope/guard-a-doubled-changes-row...`）。新函数本身有一个今天没人踩到的坑（**FR-08**）。 |
| 未保存列表的性能 | **成立** | 见第 6 节的表：`/changes` 的候选计算从上次的 30 秒（20000 行）降到 705 毫秒。限定：多 Run 路径现在一次查全部"委派"行，代价随整库的委派行数走；P3 数据没有委派，这个数字只代表没有委派的情况。 |
| 请求路径的回放次数 | **成立** | 读代码：`superchat-work-request-text` 现在把查到的 Workspace 传给 `superchat-work-current`，回放只剩一次（`superchat-work.el:466`、`:212-214`）。测量：20000 行时这一块 1 080 毫秒，对照同一轮的回放 329 毫秒；差的 750 毫秒是三次 `work.%` 全表扫描（`superchat-work-records` 一次 237 毫秒），这次数据里有 4000 条 `work.outcome`，上次一条没有，所以不能直接和上次的 905 对 406 比。回放次数这一项成立；这一块的总代价现在由工作记录事实的数量决定。 |
| 这张表 | **部分成立** | 它确实只读（他们的 `reading-the-list-writes-nothing` 通过），每类行都造得出来，我把清单第 5、6、9、12、13 步的状态造出来贴在第 5 节。但：命令在 `-L` 开发装法下不存在（**FR-03**）；审批挂着时只说"Running"（**FR-04**）；正在被接手的记录同时写成"可以继续"（**FR-05**）；子代理的执行写成"你发起的"（**FR-06**）；按 RET 去一条"已结束没看"的对话，打不开也把通知丢了（**FR-07**）；Unsaved 行指向最早而不是最近的编辑者（**FR-09**）；停掉之后表里看不出是你停的（第 5 节）。 |

## 2. 新发现

每一条：先用日常的话说，再给严重度、探针和位置。"探针"都在 `evidence/` 里，是 ERT 文件加输出日志；它们断言的是观察到的现象，缺陷也算"通过"。

### FR-01 每次执行结束，都把这条记录下结束过的每一次执行各扫一遍整张工作账本 — P1

**日常的话：** 你用一条工作记录做一件要做很多轮的事。今天之后，每一轮结束时 Superchat 都会回头把"这条记录下以前结束过的每一次执行"各查一遍"它记过结果没有"，每查一次都把 Tape 上所有工作记录的行读一遍。代价随轮数的平方增长：合成数据上做过 50 轮，每轮结束多等 0.3 秒；200 轮，1.8 秒；1000 轮，28 秒；P3 的另一组数据里 4000 轮时一次扫描 282 秒。这些数字都来自几乎只装着这些合成行的数据库和脚本模型，用户真实的一条记录下有多少次执行、感受如何，我不知道。启动时也要对每段聚焦过记录的对话这样扫一遍。

**代码事实：** `superchat-work--on-turn-completed` 先调 `superchat-work-settle-missing session run`（`superchat-work.el:1132`）；它对 `superchat-work--runs-under` 列出的每一个 Run 调 `superchat-work--facts-in-run`（`:1098-1099`），后者每次都跑 `superchat-work--rows`（`:1002-1010` → `:55-69`，一条 `LIKE '%"work.%'` 全表查询加逐行 JSON 解析）。挂上时 `superchat-work-arm-all` 对所有对话跑同一个扫描（`:954`）。

**测量（`fr-settle/run-end-cost-grows-with-the-runs-ended-under-the-record`，`evidence/fr-settle-probes-2.log`）：** 一条记录下有 R 次已结束、已有结果的执行，再跑一个脚本回合：

| R | 结束时的补记扫描 | 扫描里的 SQL 次数 | 整个回合（发送到结束） | 回合里的 SQL 次数 |
|---|---|---|---|---|
| 0 | 32 ms | 5 | 50 ms | 71 |
| 50 | 56 ms | 56 | 341 ms | 124 |
| 200 | 1 056 ms | 207 | 1 839 ms | 277 |
| 1 000 | 23 165 ms | 1 008 | 28 371 ms | 1 080 |

SQL 次数随 R 线性增，每次 SQL 读的行数也随 R 增，所以是平方。第 6 节的 P3 表里同一现象：1000 行（200 次执行）时 `settle-missing` 893 毫秒、一个回合 1.8 秒。

**推论：** 这是 `4bea4054` 引入的，第三轮评审说的"用现有 Tape/故障出口，不新建"做到了，但实现把"有没有记过"做成了 O(R) 次全扫。修法是局部的：`--facts-in-run` 对一个 Run 集合一次查出，或只看最后几次执行。

**没验证：** 用户真实数据库里一条记录下最多有多少次执行；真实模型下的感受（这里全是脚本模型，所以回合时间几乎全是扫描）。

### FR-02 关开 `work-records` 行，失败过的唤醒立刻再试一次，等待也跳过 — P2

**日常的话：** 评审的 R3-03 说"关开定时器插件就忘了试过几次"。定时器修好了；工作记录的唤醒没有：它根本不在尝试前看"试过几次"，所以关开这一行（或任何一次重新上弦）都会马上再唤醒一次，哪怕次数已经用完、哪怕还在 300 秒的等待里。每次尝试都新开一段对话。

**代码事实：** `superchat-work--try-wake`（`superchat-work.el:817-836`）只在失败之后把 `superchat-work--retries` 加一，成功时清掉；进入时不读。`superchat-work-wake-due`（`:930-944`）对每条到点的记录都调它；`superchat-work-arm-all`（`:947-960`）每次挂上都调 `wake-due`。决定记录里"工作记录的唤醒重试计数同样不再在补扫时清空"指的是去掉了 `clrhash`，那个表从来没被读过。

**探针：** `fr-scope/remounting-work-records-grants-a-failed-wake-another-try-at-once`（limit=0，第一次失败后同一虚拟时刻卸载重挂 → 2 次发送，2 条 `work.woken`）；`fr-scope/remounting-work-records-skips-the-wake-wait`（limit=3，等待 300 秒，重挂后 +0 秒 → 2 次发送）。对照 `fr-scope/guard-the-timer-row-keeps-its-count-across-a-remount`：定时器那边重挂后仍是 1 次。`evidence/fr-scope-probes-2.log`。

### FR-03 `M-x superchat-pending` 在 `(require 'superchat)` 的装法下不存在 — P2

**日常的话：** 新命令写在一个新文件里，可是包里没有任何文件 `require` 它，`superchat.el` 也没有为它写 `autoload`。用 `-L 仓库目录` 加 `(require 'superchat)` 这种开发装法（上手清单第 0 步让用户确认 `locate-library` 指向本仓库），`M-x superchat-pending` 会是 "[No match]"。通过 package.el 安装、生成了 autoloads 文件的装法可以用。决定记录说"靠 autoload 标记接进包里，和批注一样"——批注命令其实是由 `inline-rewrite` 行在挂载时 `require` 进来的。

**代码事实：** `superchat.el:65-98` 对其它从不被 require 的文件（compact、experience、plan、agents-panel、workflow）逐个写了 `autoload`；`superchat-pending` 没有。`superchat-default-plugins.el:73` 的 `(require 'superchat-annotate)`。

**探针：** `evidence/fr-pending-load.el`，输出：`after (require 'superchat): superchat-pending fboundp=nil commandp=nil | superchat-annotate fboundp=t`，挂完默认行之后仍是 nil；除它自己和它的测试，没有文件提到 `superchat-pending`。`evidence/fr-pending-load.log`。同样没被接进来的还有 `superchat-tape-browser`，所以这是既有做法的一个漏洞，不止这一处。

**没验证：** 用户自己的 Emacs 是哪种装法。

### FR-04 审批挂着的时候，表只说"Running / started by you"；"审批今天没有登记在任何地方"不属实 — P2

**日常的话：** 清单第 9 步：工具在等你答"允许不允许"，你去别的 buffer 待了一分钟，回来想看看它在等什么。表里只有一行"正在跑，你发起的"。其实 Runtime 内存里记着"这次执行在等人回答"（`superchat-runtime--asking-p`），Tape 上也写了 `approval.requested`、答完再写 `approval.decided`——事实是有的，表没读。已复现的是：问题挂着那一刻 `asking-p` 为真、Tape 上有 1 条 requested；答完有 1 条 decided。"挂着时 decided 还没有"是代码事实（`:379` 在问之前写，`:387/:393` 在问之后写），那一刻没有单独去 Tape 上查它不在。决定记录把不列审批的理由写成"今天它们没有登记在任何地方"，这句不对。

**代码事实：** `superchat-runtime.el:245-249`；`superchat-approval.el:379`（requested）、`:387/:393`（decided）；`superchat-pending.el:63-81` 只读 `superchat-changes--unattended-run-p`；`docs/decisions/accepted/2026-10-11-one-list-of-what-is-pending.md` "实现中定下的几点"最后一条。

**探针：** `fr-pending/step-9-an-open-approval-shows-as-running-only`：把审批函数换成"先读表再答不允许"，读到的表只有 `Running | started by you | execution run-… | <对话>`，同一时刻 `asking-p=t`、Tape 上 `approval.requested=1`；答完 `approval.decided=1`。`evidence/fr-pending-probes-2.log`。

### FR-05 正在被接手的记录，同时列成"Can be continued" — P2

**日常的话：** 清单第 5 步两分钟到了，记录被接手、执行正在跑。表里一行 "Running | started by Superchat: work record work-…"，下面又一行 "Can be continued | Step 5: … | add '- 检查完毕' at the end"——同一条记录，一边说在做，一边说你可以继续做。

**代码事实：** `superchat-pending--records`（`superchat-pending.el:83-131`）按 `superchat-work-status` 分类；唤醒先清掉等待再发送（`superchat-work.el:855-861`），所以执行期间记录的状态是 `continuable`，落进 `:123-130` 那个分支。它不看 Runtime 里有没有这条记录的执行在跑。

**探针：** `fr-pending/step-5-and-6-a-record-woken-at-its-time`，表文见第 5 节。

### FR-06 子代理的执行写成"started by you" — P2

**日常的话：** 主对话委派给一个子代理，表里两行 Running，都写"你发起的"；子代理那行的对话一栏是 `subagent-researcher-…`。谁发起的只按"定时任务/唤醒"判，其它一律算你。

**代码事实：** `superchat-pending.el:75-78`；Run 的公开状态里有 `:parent-run-id`（`superchat-runtime.el:377` 列出），表没读。

**探针：** `fr-pending/a-running-sub-agent-is-started-by-you`，`evidence/fr-pending-probes-2.log`。

### FR-07 在"已结束没看"的行上按 RET，先丢通知再找对话；对话打不开，通知已经没了 — P2

**日常的话：** 通知列表（`superchat-changes-notices`）里按 RET 是先把对话找出来、再清通知；这张表反过来。对话重建失败（比如数据库读不到）时，通知已经被清掉，模式行上的 `[Superchat: N]` 也少了一个，你什么都没看到。

**代码事实：** `superchat-pending.el:257-259`（`notice-dismiss` 在 `pop-to-buffer (superchat-pending--view session)` 之前）对比 `superchat-changes.el:767-781`。

**探针：** `fr-pending/opening-an-ended-row-forgets-it-before-the-conversation-is-shown`（把 `superchat--render-session-view` 换成报错：通知剩 0 条）和 `fr-pending/guard-the-notices-list-keeps-the-notice-when-the-conversation-cannot-be-shown`（同样条件下通知列表剩 1 条）。

### FR-08 `superchat-scope-shared-effect` 的建立函数一旦抛错，留下一个没人持有的计数，以后谁都建不起来 — P3

**日常的话：** 新函数先调"建立"、再登记"撤销"。"建立"要是报错，计数已经是 1、撤销没登记，以后第二份、第三份安装都以为自己不是第一个，永远不再建立。今天三个使用者的"建立"只是 `add-hook` 和 `run-at-time`，不会报错，所以产品今天没踩到；但它被当作"不含任何产品知识"的内核机制交出去了。

**代码事实：** `superchat-plugin.el:115-131`。"建立之后 apply 再报错"那条路是好的：子 Scope 被 dispose，计数回到 0（`fr-scope/guard-an-apply-that-fails-after-holding-lets-go`）。

**探针：** `fr-scope/an-acquire-that-signals-leaves-a-holder-nobody-owns`。

### FR-09 Unsaved 行的"所在对话"是最早编辑它的对话，不是最近的 — P3

**代码事实：** 标记是新的在前（`superchat-changes-mark-buffer`，`superchat-changes.el:96-103`），表取 `(car (car (last marks)))`（`superchat-pending.el:203`）。**探针：** `fr-pending/an-unsaved-buffer-two-conversations-edited-names-the-first`：两段对话先后改同一个 buffer，行里是第一段。

### FR-10 清掉批注、杀掉原文，模型仍被告知"已挂上" — P3

**代码事实：** `superchat--annotated-buffers` 只在挂上时写（`superchat-dispatcher.el:989-995`），`superchat-annotate-clear`（`superchat-annotate.el:308-315`）和 buffer 消亡都不清它；docstring 说"它活得和批注一样久"。**探针：** `fr-annotate/clearing-the-remarks-does-not-clear-what-the-model-is-told`、`fr-annotate/killing-the-source-...`。R3-05 修之前 View 局部变量也有同样问题，不是新引入；是说明过头。

### FR-11 三种"到点没启动"读起来一样：30 秒后会重试、300 秒后会重试、本 Emacs 已放弃 — P3

**代码事实：** `superchat-pending--tasks`（`superchat-pending.el:133-151`）的详情只有"not started"；重试状态在内存表 `superchat-timer--retries`/`--not-before` 里，Tape 上没有"放弃"这种事实。**探针：** `fr-pending/a-failed-task-reads-the-same-whether-a-retry-is-armed-or-not`（有重试计时器和没有时，详情文字相同）；`fr-loop/a-task-out-of-tries-leaves-no-durable-fact-...`（Tape 上只有 created/fired/sending；新 Emacs 会再试）。

### FR-12 停掉之后，记录的结局写成 `failed`，表里没有任何"被你停掉"的痕迹 — P3（措辞）

清单第 12 步要求"对话里看得出这次是被你停掉的"。表里停掉后的两行是 Unsaved 和 "Can be continued | … | add 甲2 and 甲3"，没有"stopped"；记录的 `:outcomes` 是 `("failed")`，因为 `superchat-work-outcome`（`superchat-work.el:1012-1022`）把一切非 completed 都算 failed。不是今天引入的。探针 `fr-pending/step-12-stopped-from-the-list`，表文见第 5 节。

### FR-13 挂上时的补记扫描会把聚焦期间所有缺结果的执行一次写完 — 行为记录，非缺陷

300 次聚焦期间结束、没有结果的执行（比如 `work-records` 行没挂着的时候结束的），下一次挂上时 `superchat-work-settle-missing` 一口气写 300 条 `work.outcome`，用了 4.8 秒；这条记录在 `M-x superchat-work-list` 里的 "Executions" 变成 301 行；请求里带的记录正文不变（108 字符前后一样，`superchat-work-render` 不列执行）。探针 `fr-settle/mount-writes-every-outcome-the-focused-record-lacks`。用户真实数据库里有多少这种执行没有查，也查不了。

### FR-14 一段很长的对话上，一条消息发不出去：历史片段"重排"后比预算还长，装配当场报错 — 另行发现，不属于今天的三批

**日常的话：** 测量时给对话造了 1000 轮历史再发一条消息，消息根本没发出去：提示词装配在"把历史压缩到剩余预算里"这一步报错。压缩后的历史开头有一行标题，把每一段保留原文的编号都列出来；1000 轮就是 1000 个编号，这行标题自己就超过了剩余预算，而装配规则规定"重排结果比给它的空间还长"是硬错误。

**代码事实：** `superchat-prompt.el:313-317`（refit 结果长于 `remaining` → `superchat-prompt--invalid :refit-result`）；标题在 `superchat-tape-view.el:1178-1194`（`segment` 是所有覆盖区间的拼接）。错误从 `superchat-send-input` 同步抛出，没有 Run 开始。

**证据：** `evidence/p3-fold-cost-2.log`、`p3-fold-cost-3.log` 的 `condition:` 段：`(superchat-prompt-invalid :field :refit-result :value "[Context v0 | raw 20261011-…#7, …#13, …" :id "history")`。条件：夹具的 `smoke-model` 用"restrictive defaults"的上下文上限，历史是 1000 条互不相邻的合成回答。

**没验证：** 真实模型的上下文上限下要多长的对话才会碰到；真实数据的历史是否会这样零散。和今天的三批提交无关，没有深究，只记下。

## 3. 第 2 问：今天之后，"runtime 有没有闭环"

正面回答：**没有完全闭环。** 下面是"一件事结束了，却没有任何持久的地方记下它是怎么结束的，或者没有人被告知"的清单。"已复现"是我这轮用探针造出来的；"读代码"是从代码看出来、没造；每条说三批修改关没关上。

| # | 结束的是什么 | 持久记下了吗 | 有人被告知吗 | 今天关上了吗 | 来源 |
|---|---|---|---|---|---|
| L1 | 工具在等审批，你没答（清单第 9 步） | Tape 有 `approval.requested`，没 decided | 只有小缓冲区那一问；表里只写 Running | **没关**；表本可以读 Tape/Runtime | 已复现：`fr-pending/step-9` |
| L2 | 你发起的执行，窗口已关掉，它结束了（清单第 7 步） | Tape 有结束事件 | 没有通知，没有表行（表只列"Superchat 自己开始"的结束） | 没关 | 已复现：`fr-pending/a-users-execution-that-ended-while-its-view-was-killed-...` |
| L3 | 定时任务启动后 Emacs 退出，它被切断 | 重开对话时封存为 `run.failed (abandoned)` | 没有通知、没有表行、不再启动（探针查了这三样）；重开后对话正文里怎么显示，没看 | 没关（唤醒的记录有 `--report-cut-off`，定时任务没有对应物） | 已复现：`fr-loop/a-scheduled-task-cut-off-mid-run-...` |
| L4 | 你发起的执行被 Emacs 退出切断 | 同上封存 | 表、通知都没有（探针查了）；对话重开后的正文没看 | 没关 | 已复现：`fr-loop/a-users-execution-cut-off-mid-run-...` |
| L5 | 定时任务的发送失败到放弃（本 Emacs 不再试） | Tape 上只有 created/fired/sending，没有"放弃" | 回显区一句话；表里和"30 秒后重试"同一措辞 | 部分：表让它可见了，但分不清状态；重启后再试 | 已复现：`fr-loop/a-task-out-of-tries-...`、`fr-pending/a-failed-task-reads-the-same-...` |
| L6 | 唤醒失败到放弃 | 同上，没有"放弃"事实 | 回显区一句话；表里是"Due, not started" | 部分；另外关开插件就再试（FR-02） | 已复现：`fr-scope/remounting-work-records-...` |
| L7 | 执行结束后的观察者自己报错（审阅屏、通知、补记） | 补记那一条现在进故障环（内存，100 条）；通知观察者的错只进故障环；审阅屏那条包在 `ignore-errors` 里连故障都没有 | 补记的失败会说；其它的不说 | 部分：只关了工作记录这一个观察者 | 已复现：`fr-loop/a-run-end-observer-that-fails-...`（通知观察者报错 → 故障环有、没说；审阅屏报错 → 什么都没有） |
| L8 | 执行结束、记录已转去别的记录 | 老记录没有 outcome | 不记故障、不说 | 没关，决定了不关 | 已复现：`fr-settle/a-record-the-conversation-moved-on-from-...` |
| L9 | Superchat 自己开始的执行结束了，你没看，然后重启 | Tape 有结束事件；通知在内存 | 重启后通知没了；唤醒的记录靠 `work.reported` 到主对话，定时任务没有 | 没关（已接受的范围） | 读代码：`superchat-changes.el:670-674`、`superchat-work.el:894-928`；要确认得带着一条未看的通知模拟重启，没做 |
| L10 | 给主对话的回报写失败 | 现在进故障环并说一句 | 下一次这段唤醒对话再结束、或重启时的 `--report-cut-off` 会补报 | **关了**（R3-02 的一部分） | 读代码 + work-outcome 套件 |
| L11 | 工具改了 buffer、没保存，然后重启 | Tape 有 `tool.completed`；"未保存"靠 buffer 标记 | 重启后 `/changes` 和表都列不出 | 没关（已接受） | 读代码：`superchat-changes.el:236-261`；要确认得带着一个有标记的未保存 buffer 模拟重启，没做 |

三批修改关上的是 L10，并让 L5、L6 在这个 Emacs 里可见；其余九条和用户说的"没有闭环"是一回事：**结束的事实大多在 Tape 上，但"告诉人"这一步要么在内存，要么没有。**

## 4. 第 3 问：这张表做对了吗

对照我自己研究里对 A1 的描述（一张只读的表，让用户答"谁改了什么、现在在等什么、怎么停、怎么继续"）和七个场景。下面的表文是批处理 Emacs 里 `*superchat pending*` 的实际内容（`buffer-string`，列宽照原样），来自 `evidence/fr-pending-probes-2.log`。路径里的 `ws-work-…` 是夹具的临时项目目录。

**第 5 步，到点醒来，执行刚发出去：**

```
 Running                started by Superchat: work record work-20261011031249-a859 execution run-20261011-031249-265797-1596            20261011-031249-bf13
 Can be continued       Step 5: add '- 检查完毕' to log.org two minutes later add '- 检查完毕' at the end                          20261011-031249-bf13
```

**第 5 步，执行还在跑、a.txt 已被改未保存：**

```
 Running                started by Superchat: work record work-20261011031249-a859 execution run-20261011-031249-265797-1596            20261011-031249-bf13
 Unsaved                a.txt                                        /Users/chenyibin/.cache/sc-fable-review/ws-work-sEevTb/a.txt 20261011-031249-bf13
 Can be continued       Step 5: add '- 检查完毕' to log.org two minutes later add '- 检查完毕' at the end                          20261011-031249-bf13
```

**第 6 步，执行结束、还没看：**

```
 Ended, not looked at   work record work-20261011031249-a859         completed, 1 unsaved when it ended                   20261011-031249-bf13
 Unsaved                a.txt                                        /Users/chenyibin/.cache/sc-fable-review/ws-work-sEevTb/a.txt 20261011-031249-bf13
 Can be continued       Step 5: add '- 检查完毕' to log.org two minutes later add '- 检查完毕' at the end                          20261011-031249-bf13
```

**第 9 步，审批问题挂着：**

```
 Running                started by you                               execution run-20261011-031249-750659-c24a            20261011-031249-0cac
```

**第 12 步，停之前和用 `k` 停之后：**

```
 Running                started by you                               execution run-20261011-031249-091299-ed24            20261011-031249-7322
 Unsaved                a.txt                                        /Users/chenyibin/.cache/sc-fable-review/ws-work-8mNPQn/a.txt 20261011-031249-7322
 Can be continued       Step 12: add three lines to log.org          add 甲2 and 甲3                                      20261011-031249-7322
---
 Unsaved                a.txt                                        /Users/chenyibin/.cache/sc-fable-review/ws-work-8mNPQn/a.txt 20261011-031249-7322
 Can be continued       Step 12: add three lines to log.org          add 甲2 and 甲3                                      20261011-031249-7322
```

**第 13 步，启动后行还没挂、记录到点没人接手；挂上后：**

```
 Due, not started       Step 13: add '- 重启后' after the restart    work record, due 2027-01-15 00:01, not taken up      20261011-031249-6471
---
 Ended, not looked at   work record work-20261011031249-d0cc         completed                                            20261011-031249-9af8
 Can be continued       Step 13: add '- 重启后' after the restart                                                         20261011-031249-9af8
```

**子代理在跑：**

```
 Running                started by you                               execution run-20261011-031247-464821-0563            subagent-researcher-20261011-031247-eea0
 Running                started by you                               execution run-20261011-031247-429398-e5ae            20261011-031247-9deb
```

清单的第 7 步（执行途中关掉对话窗口）由"你发起的执行结束了却在哪都列不出"那条探针覆盖（它杀掉 View 后执行照常结束）；第 11 步（执行途中切到别处）没有构造。

逐个问题：

- **谁改了什么。** 能答一半：Unsaved 行列出 buffer 和路径，对话一栏给出 session id；"谁"只分"你 / Superchat 自己（定时任务或某条记录）"，子代理算你（FR-06），两段对话改同一个 buffer 时写的是最早那段（FR-09）。记录改了哪些文件在 `v` 打开的记录全文里，表上没有。
- **现在在等什么。** 等用户回答、等时间、到点没人接手，都有行。**等审批没有**（FR-04，L1）。"到点没启动"分不出是还会重试、还是放弃了（FR-11）。
- **怎么停。** `k` 能停正在跑的，确认后调 `superchat-runtime-cancel`。停完记录的结局是 `failed`、表上无痕（FR-12）。我是用表的 `k` 停的，它直接调 `superchat-runtime-cancel`；停后 View 里的 `superchat--active-run-id` 仍指着那次执行（探针打印）。对话自己的 `C-c C-k`（`superchat-cancel`，`superchat.el:1339`）还会清暂停状态，两条路有没有差别没有比。
- **怎么继续。** "Can be continued" 行给接续点；但正在被接手的记录也在这里（FR-05），第 13 步挂上后它和"Ended, not looked at"并列，接续点为空时整行只有目标。

措辞和分类上还有：

- "Ended, not looked at" 的 What 一栏是 `work record work-20261011031249-a859`（记录编号），不是记录的目标；旁边 "Can be continued" 行才是目标。两行说的是同一件事，读者得自己对编号。
- "Conversation" 一栏是 session id，没有名字；RET 能去，但看不出是哪段对话。
- What 列宽 44，"started by Superchat: work record work-…" 58 个字符；批处理里 `buffer-string` 看到的是后面两列被顶歪（见第 5 步表文），GUI 里 tabulated-list 可能截断而不是顶歪，没看。
- 记录的 `(car (last ...))` 唤醒会话作为"所在对话"：RET 去的是最近一次唤醒的新对话，不是记录开始的主对话。合理，但表上没说。
- 临时对话、子代理对话没有另外标明（决定记录已列）。

它能不能让用户回答最后四个问题：**在"一件事从头到尾都正常"的路径上能答"在等什么/怎么继续"，在清单专门设计的几个岔路（审批挂着、停掉、子代理、重启后找回）上答不全或答错。** 这步本来就是为了让真人看着表答问题，这里只是把批处理里能造出来的状态先摆出来。

## 5. 第 4 问：有没有变慢

两张表。第一张是我自己的 `p3-fold-cost.el` 拷到 `evidence/` 后在今天的代码上重跑（原七列照旧，四列新加），第二张是 FR-01 的专项测量（第 2 节）。数字是这台机器上的中位数（5 次）或单次，不是目标。

**P3 重测（毫秒；`evidence/p3-fold-cost-4.log`、`p3-fold-cost.json`）：**

| 行数 | 回放 | unattended-run-p | `[Work records]` 块 | work-records | timer-list | `/changes` 候选 | 重开 View 的历史 | 表：读一遍 | 表：画一遍 | settle-missing（结束前的补记扫描，单次） | 一个回合（单次） |
|---|---|---|---|---|---|---|---|---|---|---|---|
| 36（3 轮真回合） | 1.0 | 1.0 | 1.6 | 0.1 | 0.1 | 2.3 | 0.3 | 0.4 | 0.4 | 3 | 103 |
| 1 001 | 14.9 | 15.6 | 25.5 | 3.0 | 0.3 | 31.0 | 3.5 | 3.7 | 3.8 | 765 | 1 605 |
| 5 002 | 74.2 | 101.2 | 212.6 | 17.4 | 1.3 | 180.5 | 18.6 | 21.1 | 20.9 | 18 053 | 发送在装配阶段失败，2 314（FR-14） |
| 20 002 | 328.7 | 331.2 | 1 080.1 | 237.0 | 5.1 | 704.9 | 109.3 | 250.2 | 251.4 | 282 395 | 发送在装配阶段失败，9 447（FR-14） |

和上次研究的数据有一处不同：这次合成的每个 Run 多一条 `work.outcome`（观察者本来就会写的那条），所以 20000 行里有 4000 条工作记录事实；上次一条都没有。凡是做 `work.%` 扫描的列（work-records、`[Work records]` 块、表）都因此比上次重，这是数据的差别，不是代码的。

读法：

- `changes-candidates`（`/changes`，不带 Run 编号）：上次研究 20000 行 30 秒，现在 705 毫秒——**修好了**，没有委派的情况下与回放同量级。
- `work-request-text`（每次请求的 `[Work records]` 块）：上次 905 对回放 406，现在 1 080 对 329——回放只剩一次，**修好了**；剩下的 750 毫秒是三次 `work.%` 扫描，随工作记录事实的数量走（这次 4000 条，见表下的说明）。
- `pending-items` / `pending-render`（这张表读一遍 / 画一遍）：1000、5000、20000 行分别 3.7、21、250 毫秒（画一遍 3.8、21、251）——**便宜**，远低于回放，刷新不是问题。注意它对每个正在跑的执行各回放一次它的对话（`superchat-pending.el:75`），这里只有零到一个在跑。
- `settle-missing`（执行结束前先做的补记扫描）：1000 行 765 毫秒、5000 行 18 秒、20000 行 282 秒（4 分 42 秒，单次）——**新的慢**，见 FR-01。启动时挂上 `work-records` 行跑的是同一个函数，对每段聚焦过记录的对话各来一遍。
- `one turn`（一个脚本回合从发送到结束）：36 行 103 毫秒，1000 行 1.6 秒（其中补记扫描 0.76 秒，模型是脚本的，所以几乎全是扫描）。5000 和 20000 行的回合在发送时就失败了（FR-14，装配阶段），没有 Run 开始，那两格是失败的发送本身用的时间。

**比上次慢的、快的、不变的：** 慢的只有执行结束（FR-01）和启动时的挂载扫描（同一函数）；快的是 `/changes` 和请求块；表自己很快。

## 6. 证据

### 跑了什么

全部串行，一次一个 Emacs。命令和退出码在 `evidence/manifest.json`。

| 检查 | 结果 | 日志 |
|---|---|---|
| Kernel 检查 | 退出 0 | `evidence/kernel.log` |
| 冒烟 | `SMOKE OK`，退出 0 | `evidence/smoke.log` |
| object-scope / shared-resource / work-outcome / pending | 18 / 6 / 10 / 12，全过 | 同名 `.log` |
| annotate / annotate-chat / m7-buffer-first / timer / timer-start-fault / unattended-notice / work-wake-fault / work-record | 全过 | 同名 `.log` |
| M3（只运行，文件未动） | 退出 0 | `evidence/m3.log` |
| codex 第三轮四个探针文件 | domain 3 红 1 绿、composition 2 红 1 绿、annotation 4 红、boundary 1 红 1 绿（OPEN-C 仍绿：它看的是工具列表和通知，不看新表） | `evidence/codex-*.log` |
| 我的探针 | `fr-pending-load`（1 个脚本）、`fr-scope`（7）、`fr-settle`（5）、`fr-pending`（10）、`fr-annotate`（6）、`fr-loop`（4），断言的是观察到的现象 | `evidence/fr-*.el` / `fr-*-2.log` |
| P3 重测 | 见第 5 节 | `evidence/p3-fold-cost-4.log`、`p3-fold-cost.json` |
| 文件未改 | 639 个文件摘要前后一致 | `evidence/unchanged.json` |

**保留的失败过程：** `fr-scope-probes`、`fr-settle-probes`、`fr-pending-probes` 第一次各有 1-3 个用例因我的夹具用法失败（`wrec--file` 在 View 之后再写一次磁盘文件，引出 "changed on disk" 的提问，批处理里读不到答案），改成从已开的 View 取 buffer 后第二次全部按预期；第一次 `fr-scope` 还有一个断言写反了（我以为第二份安装的观察者会跑两次，实际不会），改成护栏。`p3-fold-cost` 跑了四次：第一次被我中途停掉（20000 行时 5 次 `settle-missing` 要半小时，改成单次）；第二次在 5000 行的回合上碰到 FR-14 的装配错误而中止；第三次只放宽了回合的等待，错误其实在发送时同步抛出，照样中止；第四次把发送的错误记下来不中止，跑完。四次日志都留着（`p3-fold-cost.log`、`-2.log`、`-3.log`、`-4.log`），`run-checks.py` 的超时为后两次加了环境变量。这些都是探针自己的问题，不是产品发现。

### 已证实

1. 八个修复里 R3-01、04、05、06、07、08 关上了评审那条路；R3-02 和 R3-03 各只关了一半：R3-02 的"说出来、补上、只补一次"成立但补记扫描是新的平方级代价（FR-01），R3-03 的定时器一侧成立、唤醒一侧没修到（FR-02）——每条都有 codex 探针变红的具体断言和套件通过。
2. FR-01 到 FR-12 都有能复现的探针和输出；FR-01 有两组独立测量。
3. 三批修改没有让冒烟、M3、内核检查变红；这说明主干没坏，不说明目标都达到了。
4. `/changes` 和请求块的性能修复是真的（P3 数字）。

### 未证实

- 真实 DeepSeek / 本地 Qwen 上的任何行为；真人在真实 Emacs 窗口里看这张表的体验（列宽、CJK、颜色）。
- 用户真实数据库的规模：一条记录下最多几次执行（FR-01 的实际感受）、有多少缺结果的执行会被 FR-13 补写。
- 用户的 Emacs 是哪种装法（FR-03 的实际后果）。
- `k` 停掉之后 View 的残留状态有没有后果。
- FR-14（很长的对话上消息发不出去）在真实模型的上下文上限下要多长的对话才会碰到，没查；和今天的提交无关。

### 没覆盖

- 真杀进程、断电、数据库损坏；重启只用"清掉内存表 + 重开 View 封存"模拟。
- Telega、emacsclient 入口；预设切换时真人在 UI 里装第二份插件（只用了和预设相同的 mount/Scope API）。
- `--child-sessions` 多 Run 路径在有大量委派行的库上的代价。
- 同一 buffer 被两段对话批注、批注等待期间原文改行。
- R3-02 里 `work.facts` 和 `work.outcome` 各自失败的组合。
- M3 回归集的断言是否需要改：没有发现要改的；给 leader 的信息是 M3 在今天的代码上全过，新发现集中在它没覆盖的工作记录结束路径和这张表。
