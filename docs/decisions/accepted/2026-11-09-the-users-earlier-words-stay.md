# Decision: 历史放不下时，你前面说过的话优先留下，留不下时告诉你一次

Status: accepted — 用户 2026-11-09 对评估里的七条建议说"同意建议"。本条是其中第 4 条的做法。第 4 条说的是"实现已预留的钉住参数，把任务消息钉住"；这里不钉住某一条，而是把你前面说过的话从新到旧留下，最多占历史预算的八分之一，因为程序分不出哪一条是"任务"。这个差别 2026-11-09 已经告诉用户，用户还没有回答。同一条建议里的"已知云端模型用真实窗口"不在本条里，等用户另外选。

## Problem

每次发请求，superchat 会把这段对话前面的内容一起发给模型，好让它接得上。前面的内容有字数上限，叫"历史预算"（DeepSeek 是 40000 个字符）。挑的办法是从最新的往回挑，挑到放不下为止，更早的就不发了。

2026-10-22 已经有一条规则（提交 56558e29，没有单独的决定记录）：挑到放不下时，再补上一条用户消息，就是挑中的最早那条记录所回答的那句话，免得模型只看到一串工具调用、不知道是为了什么。那次的对话里，历史只留下 22 次探查，丢了它们要回答的问题，模型就一直探查下去。

2026-11-09 的对话 `20261109-005550-3bd3` 里，被补上的那一句是"继续"。你在这段对话里先说了任务（#10662），之后每次执行失败都只回一句"继续"。从 `request.message-list` 的记录看，09:16:03 UTC 开始的那次执行之前，每个请求的第一条都是任务；09:22:26 UTC 开始的那次（#11238）第一条变成了 #10939 "继续"，任务没了。那个请求的 `prompt.assembled` 记着预算 40000、用了 38874。模型随后问起一件不相干的事（它当时看得到的一个目录要怎么整理），你只好把任务再说一遍。

"继续""接着做"这种话很常见，所以"最近那一句"常常不是说要做什么的那一句。任务被挤掉的时候，你也不知道。

## Decision

**一、历史放不下时，你前面说过的话从新到旧留下，最多占预算的八分之一。** 从你那边看：前面挤得再满，模型也还能看到你最近说过的那些话，任务就在其中；最近的工具调用和回答为此最多让出八分之一的位置。

做法（`superchat-view--continuity-keep-question`，由 `superchat-view--continuity-select-records` 调用）。挑到放不下时：

1. 最近那一句照 2026-10-22 的规则留下，条件和腾位置的方式都不变：从已挑中的内容里，按从旧到新去掉不是用户消息的记录，直到条数和字数都放得下。
2. 然后看更早的用户消息，从新到旧一条条看。一条能留下，要满足两点：这条规则留下的所有用户消息（最近那一句也算）加起来不超过字数预算的八分之一（`superchat-view--continuity-user-words-share`，值是 8）；按第 1 步的办法能腾出位置。放不下的那条跳过，接着看更早的。所以一段很长的粘贴不会把它前面的任务挡掉。最近那一句本身就超过八分之一时，第 1 步照样留它，更早的就一条也不留了。
3. 挑中的最早那条本身就是一条用户消息时，2026-10-22 的规则什么都不补；第 2 步这时也照样做。
4. 留下的用户消息按原来的先后顺序放在挑中的内容前面。
5. 没有正的字数预算时，第 2 步不做。

从挑中的结果推出来的东西都照旧：哪些没发、为什么没发，请求里写明的说明，发给模型的消息，请求的留证记录，以及 `prompt-reconstructable` 检查（它用留证记录把请求重新拼出来对照）。夹在两条留下的记录之间、没发的那些，原因记为"位置不够"（`budget`）。一次工具调用和它的结果始终在一起：腾位置是按整条记录去掉的，调用和结果在这里是同一条记录。

**二、你最近说的话被挤掉时，告诉你一次。** 有了第一条以后，一条用户消息只有在八分之一里放不下时才会被挤掉。如果一个对话请求的历史因为位置不够，没有带上这段对话里 12 小时以内你发过的消息，回显区（Emacs 窗口最下面那一行）出现一行字，说清有几条你最近的消息模型收不到、历史预算是多少字符、你能做什么：`/compact`，或者调大 `superchat-context-max-chars` 或 `superchat-llm-context-window-overrides`。每段对话在这次 Emacs 运行里只说一次。不写进 Tape，也不加进任何请求。

做法：`superchat-pipeline--tell-recent-words-left-out`，在 `superchat-pipeline--dispatch-to-runtime` 里、请求被接受（`:status 'accepted`）之后调用。这里是对话请求的历史最终定下来的地方。它按请求的说明里"没发、原因是 `budget` 或 `oversized`"的那几段，到 Tape 里数类型为 `user` 的行，只数 `created_at` 离现在不到 12 小时（`superchat-pipeline-recent-words-seconds`）的。出错只记一条故障（`superchat-event-record-fault`），不影响请求。

**和第 4 条建议的不同。**

- **不钉住。** `superchat-view-assemble` 的 `pinned-storage-ids` 仍然保留不用，传了照样报错。钉住要先知道哪一条是任务，程序不知道：一段用了几个星期的对话，第一条消息不是第三个星期的任务；让模型或你来标，就是每次都要做一次判断。你最近说过的话最接近任务，又不用判断，而且很省：一句"继续"在历史里只占 8 个字符，预算是 40000。
- **云端模型的预算怎么算不改。** `superchat--llm-context-budget-chars` 只有在窗口大小来自你写的覆盖设置或正在运行的 LM Studio 时才按窗口算，否则用 40000；DeepSeek 用的是 40000。预算调大，每段对话的每个请求都要多花 token，这要你另外决定。提示里点了 `superchat-llm-context-window-overrides` 的名字，你可以只给一个模型调大。

## 实现时定的

- **八分之一怎么算：** 每条消息按它在历史里的那一行算长度，再加 1 个分隔符，和预算本身的算法一样；上限是 `(/ 预算 8)`，取整。
- **最近那一句留不下时（它一条就超过整个预算，或者腾不出位置），第 2 步仍然做**，从 0 开始算八分之一。设计里说的是"连同这条规则已经留下的"，这时已经留下的就是零条。测试 `user-words/not-told-when-only-calls-and-answers-go` 走到了这种情况。
- **"位置不够"包括 `oversized`**（一条单独就超过整个预算）。被取消的执行里的消息不算位置不够；被替换掉的那一轮本来就不在历史里。
- **12 小时：** 一天干活的对话都在里面。这段时间里的话，你会以为模型还记得；前一天的消息在长对话里掉出去，是预算本来就会做的事，每天早上都说一遍就成了噪音。时间用 Tape 行的 `created_at`（UTC），和 `superchat-pipeline--recent-words-clock` 比；测试里可以换掉这个钟。
- **只说对话界面自己的历史：** 请求的 `surface` 是 `chat`，并且带着对话的 Turn。子代理、工作流的一步、压缩、技能匹配、就地提问（in-place ask）都不说；只打开请求检查器看、不发出去，不说；`/recall` 不发请求，也不会说。
- **会在你打字时出现：** 排队等着的对话请求是由定时器发出去的（`superchat-runtime--schedule-dispatch-next`），所以这行字可能在你用 minibuffer 时出现，Emacs 会把它在 minibuffer 上盖一下。这一点照实写下，没有绕开。
- **没人打开的对话不说：** 定时任务跑的那段对话、被唤醒的工作记录跑的那段对话，是 superchat 自己开、自己发话的（"后台对话"）。它们开的时候 Tape 上记一条 `conversation.background`（`superchat-workspace-new-conversation` 带 BACKGROUND 时写，`superchat.el:4171`；调用处 `superchat-timer.el:474`、`superchat-work.el:943`），配置里就有 `:background`。这种对话里的 `user` 行是程序写的提示，你也没在看它，所以提示一句也不说，也不记成"已说过"。
- **仍然会把程序写的话算进去的情况：** 目标的下一轮（`superchat-work--queue-round`，`superchat-work.el:1319-1338`）排进那条工作记录所在的对话；后台子代理交回的报告（`superchat--subagent-queue-report`，`superchat-subagent.el:1551-1569`）排进启动它的那段对话。两者都是带着对话 Turn 的对话界面请求。那段对话是你自己开的（不是后台对话）时，它们写下的话也是 `user` 行，被挤掉时会算进"你最近的消息"。这一点照实写下，没有绕开。

## 验证

- **新测试** 在 `scripts/user-words-regression.el`（前缀 `user-words/`）。它借用 `scripts/history-messages-regression.el` 的测试环境：每个请求都用 llm.el 自己的 OpenAI 兼容请求构造器序列化，也就是 DeepSeek 收到的样子，断言查的是这些消息。单独成文件，2026-10-22 那套测试就不用动。
  - `the-task-behind-go-on-stays`：事故的样子。任务，四轮"继续"加很多次调用，预算在最新一轮里面就用完。请求里依次是任务、四条"继续"、最新的调用和结果。
  - `a-long-paste-does-not-hide-the-task`：任务和后面之间有一段 300 多字的粘贴，粘贴被跳过，任务留下。
  - `the-eighth-holds`：12 条更早的消息里只留最新的 4 条，再多一条就超过八分之一；挑中的内容只少了刚好够的位置，再多一次调用就放不下。
  - `an-oldest-user-message-no-longer-stops-them`：挑中的最早那条是用户消息，前面的消息照样留下。
  - `ahead-of-an-assistant-text`：挑中的第一条是助手的文字。
  - `calls-keep-their-results-and-the-evidence-rebuilds-it`：调用和结果不分开；留证记录拼出的请求就是发出去的这个（`reproduced`），第一条是任务，Tape 上没有 `prompt-reconstructable` 的违例。
  - `the-user-is-told-once`、`not-told-when-only-calls-and-answers-go`、`not-told-for-words-older-than-twelve-hours`、`not-told-for-another-surface`：提示只说一次；只挤掉调用和回答时不说；12 小时以前的不说；别的界面和只打开检查器时不说。
  - `not-told-in-a-conversation-nobody-opened`：一段后台对话（`superchat-workspace-new-conversation nil t`）的历史挤掉了任务，不说，也不记成已说过；同一个测试里你打开的对话照样说一次。
- **改之前都不过：** 前 10 项在改动前的代码副本里全部失败，原因就是本条要去掉的：请求里只有一句"继续"、没有任务，或者根本没有提示。`not-told-in-a-conversation-nobody-opened` 在去掉后台判断的副本里失败：后台对话里也说了 "Superchat: the model is no longer sent 1 of your messages…"。
- **发给模型的样子：** 留下的几条用户消息在请求里是连着的几条 `user` 消息，不合并。挑中的第一条是工具调用时：`user, user, …, assistant(tool_calls), tool, …`；是助手文字时：`user, user, assistant, user, assistant, user`；是用户消息时：`user, user, assistant(tool_calls), tool, assistant, user`。连着的 `user` 消息以前就有：告诉模型你保存了什么的那条说明（[下一轮知道你保存了什么](../implemented/2026-09-25-the-next-turn-learns-what-the-user-saved.md)）就紧挨在新问题前面。
- **原有测试一项没改。** 2026-10-22 规则的测试（`history-messages/kept-calls-keep-the-question-they-served`、`a-question-already-kept-is-not-repeated`、`a-small-budget-keeps-calls-with-their-results`）照旧通过。
- **跑过的检查：** history-messages、user-words、m5-history-material、context-recovery、tool-compaction、tool-observation、user-saves、shared-conversation、m5-request-context、context-header、long-history、tape-lineage 全过；M3 139 项，137 项过、2 项跳过；Kernel check 通过；标准 smoke 0 处不符，保存的副本没改。两个改动的文件字节编译后没有新警告。
- **合并后的整套回归（验收时跑的）：** 今天的几项改动和验收时补的修正全部合并后，在一份不含 `.git` 和编辑器锁文件的副本上，跑了 `scripts/` 下全部 139 个定义了测试的脚本（共 9423 项）、Kernel check 和标准冒烟集，同时最多 4 个 Emacs。只有 `m5-regression` 的 3 项不过，这 3 项改之前就不过（见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。和改之前逐个脚本对比：原有的每个脚本，测试数和通过数都一样，只有 `context-recovery` 多了 1 项新测试；新增 5 个脚本，全过。Kernel check：`KERNEL-CHECK OK (0 fails)`；冒烟：`SMOKE OK (0 mismatches)`，基准没有改；M3 只跑不改：139 项，137 过，2 跳过。改之前那一遍是在机器很忙时跑的，有 4 个脚本偶发失败或卡住（`answer`、`feed`、`note`、`timer-brief`）；同样的测试单独重跑、以及合并后这一遍，都通过。
- **没有验证的：** 没有用真实的模型服务商发过，所以不知道 DeepSeek、Claude 等是否都接受连着的 `user` 消息；没有在真实的 Emacs 会话里看过这行提示，不知道它会不会马上被执行中的状态信息盖掉；没有在你真实的 Tape 上重放那段对话。

## Alternatives considered

**钉住对话的第一条消息。** 参数早就留好了，第一条也常常就是任务。没选：只有短对话的第一条才是任务。一段用了几个星期的对话做过很多件事，第一条是最旧、最不相干的那件，钉住它每个请求都要一直付这份预算；任务不在第一条时，事故的样子还是解决不了。

**把短于某个长度的用户消息当成"不是问题"跳过。** "继续"不带任务，那就去找最近一条长于比如 20 个字的。没选：长短不等于有没有内容。"用中文""删掉它"很短，却正是指令；一段粘贴的日志很长，却不是。不管定多长，总会在某处丢掉真的指令。留下短的几乎不花什么。

**前面的用户消息全都留下，不设上限。** 最简单，也永远不会丢任务。没选：几个星期的对话有几百条用户消息，有的是整段粘贴，全留下会把模型接着干活要用的最近的调用和回答挤出预算。八分之一就是给这个损失定的上限。

**每次有东西没发都告诉你。** 请求里的说明本来每次都数着没发的条数；每次都说，长对话里几乎每个请求都会说，说的还是你不在意的调用和回答。没选：每次都说的提示没人看。只说一次、只说你自己最近的话，说的才是会改变你做法的那件事。

## Consequences

- 你说过"继续"好几次之后，模型仍能看到几条之前的任务。
- 最近的调用和回答最多让出八分之一的历史预算给你前面的话（另外加上 2026-10-22 那条规则本来就给最近那一句的位置）。
- 历史里可以有几段互不相连的内容；请求里写明哪些没发的说明本来就能表达这一点。
- 看到提示后，可以 `/compact` 或调大预算；程序不替你调。
- 程序仍然不知道哪一条是任务；`pinned-storage-ids` 继续保留不用。
- 2026-10-22 的规则没有自己的决定记录，只有提交 56558e29 和上面列的三项测试。
