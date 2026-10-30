# nodecode 的做法，与 superchat 的对照

这页记 nodecode 是怎么做的，以及 superchat 的对应情况。来源是原文：`github.com/nodecode-ai/nodecode` 提交 `666bb2f`（2026-10-28，版本 0.1.3），随附的能力包在 `github.com/nodecode-ai/cells` 提交 `bb87041`（2026-10-28）。对照表只读了代码和文档；文末四节 ARC-AGI-3 测试运行了它的发布包 v0.1.3。superchat 一侧是 `perf/lineage-max-id-pushdown` 分支的代码，以及 `.scratch/task-sim/runs` 里的运行记录。2026-10-28 阅读。下文 nodecode 的路径相对它的仓库根目录。

## 一句话

两者都让模型在一个正在运行的 Lisp 进程里执行代码。nodecode 的进程归 agent 自己所有（SBCL），模型只有一个 `eval` 工具，没有沙箱；superchat 的进程是用户正在用的 Emacs，所以保留 6 个工具和权限级别，`eval_elisp` 是受约束的主力。下面多数差别都能追到这两点。

## nodecode 是什么

- 一个 SBCL 进程（网关），终端界面、浏览器页面、手机、Discord/Telegram/Slack 机器人和程序（`nodecode rpc`）都连到同一个进程（`README.md` 的 "One organism, every client"）。
- 模型只有一个工具 `eval`：执行 Common Lisp，定义的东西每个写成一个文件、用 git 提交、以后每次启动都加载（`README.md:27`，`src/README.md:103`）。
- 别的能力都是 `eval` 里调用的函数：`sh`、`edit`、`write-file`、`patch`、`look`、`recall-entry`、`context`、`fork` 等（`src/src/engine/turn.lisp:83`）。MCP 服务器的工具、Chrome、网页搜索也做成函数，不注册工具（`src/README.md:517`、`:581`、`:645`）。
- 规模：`src/src` 约 9.98 万行 Lisp，`src/test` 约 6.7 万行。它包含网关、终端界面、聊天频道、从别的 agent 导入等，范围和 superchat（一个 Emacs 包，根目录 `*.el` 约 5.79 万行，`scripts/*.el` 约 3.89 万行）不同，行数不能当作同一种东西的大小来比。
- 它移植了 Pi 的几件东西：`pi-chrome` 的浏览器扩展、`pi-web-access`、`notify.ts`（`src/README.md:508`、`:579`、`:547`）。Pi 的 codemode 见 [pi-codemode.md](pi-codemode.md)。

## 对照

| 方面 | nodecode | superchat |
|---|---|---|
| 工具数 | 1 个，`eval`（`src/src/engine/exec.lisp:2437`） | 默认 6 个：`read_buffer`、`read_file`、`edit_buffer`、`eval_elisp`、`work_record`、`timer_create`（`superchat-llm-default-tool-names`，`superchat-tools.el:2132`）；真实调用里 `eval_elisp` 占 71%（[决定](../decisions/accepted/2026-10-26-eval-elisp-calls-tools.md)） |
| 别的能力怎么调 | 都是 `eval` 里的函数；模型把 `sh`、`look` 这类函数当成工具直接调用时，结果告诉它这是 `eval` 里的函数（`turn.lisp:555`） | 表达式里 `(superchat-tool "NAME" ...)` 经同一条工具管线调用别的工具；会要审批的嵌套调用直接拒绝（`superchat-tools.el:1508`） |
| 系统提示和手册 | 系统提示四句话（`turn.lisp:54`）；手册在进程里，用 `(help)` 读，每次请求只带每个主题一行（`turn.lisp:234`）；规则在用得着的时候由工具结果说出来 | 由一组提示钩子拼成，按最终工具面决定带不带（`superchat-prompt-hooks.el:470`）；参考页不默认提供，报错命中某页时在失败反馈里附上那一页 |
| 一次 eval 的输入 | 一段或多段 form，依次执行；某段出错时写明 "ERROR in form i of n"，前面的照旧生效（`turn.lisp:93`） | 只接受一个完整表达式，多个要包进 `progn`（`superchat-tools--read-one-expression`，`superchat-tools.el:1175`） |
| 括号没闭合 | 多行片段按缩进补全，结果第一行说补了什么；单行片段拒绝（`turn.lisp:93`） | 不执行，说明哪个括号没闭合、在第几行（`superchat-tools--unreadable-text`，`superchat-tools.el:1146`） |
| 大段字面文字 | `strings` 参数：文字放在 JSON 里和代码并排发送，代码里用 `($ "name")` 取，只经过 JSON 转义（`exec.lisp:2454`） | 没有；文字写在 Elisp 字符串里，经 `superchat-tool` 调 `edit_buffer` 时的新旧文字也是 |
| 运行时间长 | `yield_time_ms`（默认 10 秒，最多 600 秒）到了就转到后台继续跑，从不杀掉；结束后由框架在下一轮、或唤醒会话时把结果读给模型（`exec.lisp:2451`，`turn.lisp:95`） | 30 秒时限，由外部进程发 SIGUSR2 停下；结果告诉模型长任务改用 `make-process`（`superchat-tool-eval-time-limit`，`superchat-tools.el:1345`、`:1364`） |
| 结果长度 | 32000 字符（`turn.lisp:720`）；只有最后一个值完整打印，太大的值按一张阶梯逐级减小打印的层数和长度（`*value-print-ladder*`，`turn.lisp:1097`） | 20000 字符，超出时保留开头和末尾 4000（`superchat-tools.el:1206`） |
| 写盘 | 模型有用户账户的全部权限，`(edit ...)`、`(write-file ...)` 直接写盘 | 表达式写临时目录以外的文件一律拒绝（G28）；改名、复制、删除只在完全控制的级别放行并逐条列出（G32）（`superchat-tools.el:1269`、`:1305`）；改动留在 buffer 里，保存由用户在审阅界面决定 |
| 权限与审批 | 没有："Nodecode ships no sandbox"（`README.md:195`）；`guard` 能力包按规则拒绝几类危险命令，自称 "a speed bump, not a security boundary"（`cells` 仓库 `tools/nodecode-guard/README.md`） | 级别 `read-only`、`edit-buffers`（默认）、`auto`、`danger-full-access`（`superchat-sandbox.el:47`）；`edit-buffers` 下 `eval_elisp` 每次都问，`edit_buffer` 不问（`superchat-tools.el:1472`） |
| 模型定义的东西 | 每个定义写成层里的一个文件，git 提交，以后每次启动都加载；正在调用栈上的函数被重新定义时先"扣住"，跑通一次才写入；有编号的代（generation），可以回滚（`src/README.md:103`，`turn.lisp:112`） | `define_tool` 写的工具保存为定义，属于写它的 Workspace（[决定](../decisions/accepted/2026-10-13-a-synthesized-tool-is-a-definition.md)）；`eval_elisp` 里执行的 `defun` 只在这次 Emacs 的内存里 |
| 上下文变长 | 不做摘要：估计到窗口的 85% 时整轮淘汰，直到 50%，留下可寻址的桩；桩里列出被淘汰片段开头那行 `;;` 笔记；`(recall-entry seq)` 原样读回；模型可以直接编辑 `context.txt` 来增删（`src/src/engine/context.lisp:22`、`:33`，`src/src/engine/working-context.md`） | 锚点压缩：另起一次模型调用把旧对话写成摘要，存成 Tape 上的锚点，原始条目保留（`superchat-compact.el:1`，[architecture](../architecture.md) 的 "Prompt and history"） |
| 提示缓存 | Anthropic 请求里最多放 4 个 `cache_control` 断点：尾部、上一轮边界、系统、工具（`src/src/engine/provider.lisp:5280`）；会变的内容放在缓存前缀之后（`src/README.md:384`）；记录缓存读写的 token | 不放断点；llm.el（0.31.x，以及本地的 `../llm`）的 Claude 请求里也没有 `cache_control` |
| 失败与轮数 | 一次回答里的工具轮数不设上限，停下靠取消、服务端失败和上下文预算（`turn.lisp:42`）；`src/src/session/lifecycle.lisp:194` 的注释提到 `MAX-TOOL-ROUNDS`，`src/` 里没有它的定义。另有可选的回合预算 `*turn-budget*`（秒数、调用次数、token），默认不设；超过后再调用会被拒绝，模型用手头的东西回答，拒绝后还调用就结束这次回答（`src/src/engine/meter.lisp:16`、`:79`）。每个工具结果末尾带一行用量：用时、轮数、调用次数、上下文占用和当前时间（`meter.lisp:23`）。按 consecutive、in a row、brake 搜索 `src/`，没有按连续失败停下的规则 | 连续 3 次工具失败就停下这次执行（`superchat-tool-failure-brake`，`superchat-runtime.el:96`）；一次执行最多 120 次工具调用（`superchat-agent-max-tool-calls`，`superchat-agent-loop.el:55`） |
| 并行 | `(fork "任务1" "任务2" ...)` 最多 4 个分叉，每个带着整个上下文、从同一个缓存前缀开始；分叉不能再分叉，它定义的东西不写入层（`turn.lisp:177`） | 委派子 agent：自己的 Session，只带准备好的提示（有当前 Work Record 时加它的快照），沿用父级的权限级别，从不提问（[architecture](../architecture.md) 的入口表） |
| 持续做一件事 | `goal` 能力包：每轮结束后跑检查命令，或让一个分叉当裁判；没通过就自动开下一轮；`--k N` 在 N 个 git worktree 里同时试，留最好的（`cells` 仓库 `tools/nodecode-goal/README.md`） | Work Record：执行结束后按 Tape 上的事实决定是否继续，没有进展或失败就停下等用户；定时器和等待唤醒（[architecture](../architecture.md) 的 "Workspaces, Work Records and timed work"） |
| 记忆和技能 | `define-memory`、`define-skill` 也是定义，存在知识包里；按使用记录排序的索引放在对话开头（`src/README.md:719`） | SQLite FTS5 记忆，每轮自动召回；`SKILL.md` 目录随提供 `read_file` 的请求发送，模型用 `read_file` 读（`README.md`） |
| 入口 | 终端、网页、手机、聊天机器人、rpc，都连到同一个进程 | Emacs 里的聊天窗口、就地问答和改写、Org Babel、Magit 等（[architecture](../architecture.md) 的入口表） |
| 更新 | 多数版本在运行中逐个替换定义，不重启；`nodecode rollback N` 回到某一代（`README.md` 的 "Updates while it runs"） | 普通 Emacs 包 |
| 测量 | 25 个公开 ARC-AGI-3 游戏：同用 Claude Opus 5.5，nodecode 1.81 亿 token，Claude Code 3.93 亿（少 54%，费用少 41%）（`README.md:132`）。报告出自 Foresight Labs（FL-2026-006）；nodecode 的 README 把 Research 链接指向 foresight-labs.eu，nodecode.ai 自己的模型也写着 "with Foresight Labs finetunes"（`turn.lisp:127`），所以这是同一方的自测，不是独立评测（推论）。这里没有读那篇报告，也没有查过有没有第三方复现。报告没有公开提示词、游戏适配器和代码，同一游戏玩过多次时取最好的一次（`README.md:138`）。nodecode 的源码注释写明它的几处设计是照 ARC-AGI-3 的试跑改的：缩进补括号（"The ARC-AGI-3 trials lost 29 of 289 tool calls to \"unbalanced form\""，`turn.lisp:864`）、值的阶梯打印（`turn.lisp:1146`）、淘汰桩的折叠（"The ARC-AGI-3 runs evicted 290 times in a turn"，`src/test/engine/context-test.lisp:768`），所以这是它调过参的测试集 | DeepSeek 上的任务模拟，每格 3 到 5 次，记录通过率、请求数、首次请求大小（[决定](../decisions/accepted/2026-10-26-eval-elisp-calls-tools.md)）。两边在同一测试机上的 ARC-AGI-3 对比见文末三节 |

## 差别从哪里来

宿主。nodecode 的 `eval` 在网关进程的工作线程上跑，用户的界面是另一个进程连进来，所以跑得久的表达式可以转到后台，不影响用户。superchat 的表达式在用户的 Emacs 里跑，`superchat-tools--eval-stopped-result` 的原话是 "While an expression runs, Emacs does nothing else, so the user's Emacs was frozen that long"，所以只能限时，把长任务交给外部进程。

信任。nodecode 把进程整个交给模型，安全交给运行环境（"Run it in a container or a VM"，`README.md:195`；`src/README.md:784`）。superchat 和用户共用一个 Emacs，工具的边界就是权限的边界：`edit-buffers` 级别下 `edit_buffer` 不用问，`eval_elisp` 每次都问。所以 superchat 只留 `eval_elisp` 一个工具，不只是少几个工具，而是默认级别下每次改动都要问用户。

上下文。nodecode 不做摘要，选择整轮淘汰、模型自己写的笔记和模型自己编辑上下文；它自己的文档写明还没有证明这样更省（`src/src/engine/working-context.md:124`："No learned editing policy or cost improvement is claimed; benchmark matched tasks before claiming a win"）。superchat 选择摘要锚点。

## superchat 自己的数据：eval_elisp 在哪里失败

2026-10-22（读不成 Lisp 时说明原因的那次改动）以后的 265 次真实运行里，`eval_elisp` 请求 1185 次，失败 382 次。其中 146 次是沙箱里 native-comp 编译失败（决定里"第一轮全部作废"的那批），不是模型的问题。按 `evidence/events.json` 里 `tool.failed` 的 `:result-text` 归类（`python3 -I .scratch/task-sim/harness-tools/eval-failure-kinds.py .scratch/task-sim/runs 20261022`，各类的匹配规则在脚本里），其余 236 次是：

| 类别 | 次数 | nodecode 的对应做法 |
|---|---|---|
| 读不成 Lisp：括号没闭合 | 27 | 多行片段按缩进补全；这 27 次里有多少是多行、补全后是否就是模型想要的，没有核实 |
| 读不成 Lisp：多个表达式没包 `progn` | 15 | 一次可以有多段 form |
| 读不成 Lisp：其它 | 6 | — |
| `Wrong type argument` | 50 | — |
| 函数不存在 | 41 | — |
| 变量不存在 | 24 | — |
| 参数个数不对 | 9 | — |
| 写盘被拒（G28） | 6 | 不拦 |
| 调用只能在 batch 里用的函数 | 6 | — |
| 其它 | 52 | — |

读不成 Lisp、根本没有运行的共 48 次，约占 236 次的五分之一。明写为转义错误的只有 1 次（`Invalid escape char syntax: \S not followed by -`），所以 `strings` 通道的收益在现有数据里看不出来。它针对的是 Elisp 字符串里的反斜杠和引号；经 `superchat-tool` 调 `edit_buffer` 的次数多了以后，这类文字会变多，这一点是推测。

## 可以考虑借的（候选，未决定）

1. 一次 `eval_elisp` 允许多个表达式，出错时说是第几个。对应上表 15 次失败。改动在 `superchat-tools--read-one-expression`，"先读完全部，再执行"不变。
2. 跑测试有一条正道。决定里记录的来回最多的地方是跑测试（shown 组 28 次跑测试的 eval 里失败 12 次），原因写的是"测 buffer 当前内容没有正道"。nodecode 的做法是长任务转后台、结束时框架把结果读给模型；Emacs 里对应的是一个函数：把 buffer 当前内容写到临时目录，用 `emacs --batch` 作为子进程跑 ERT，结果在下一轮送回。借的是做法，不是代码。
3. 用 Claude 模型时加提示缓存断点。要么改 llm.el，要么在 superchat 发请求前改请求体。DeepSeek 按前缀自动缓存：ARC 三节里 superchat 的 12 局，输入有 98.5% 到 99.7% 命中，所以这一条只关 Claude。
4. `strings` 字面文字通道。现有数据只支持 1 次，等嵌套的 `edit_buffer` 多了再量。
5. 一次执行内部缩减上下文。nodecode 的模型可以自己编辑上下文：ARC 开推理的 ls20，它在第 90 次请求前把 176 条消息换成 8 条加一段笔记（事实 `context.edited`）。superchat 的上下文只在新执行开始时从 Tape 重建，一次执行内部只增不减，同一游戏一次执行涨到 30.8 万 token。这种缩减对得分有没有用，没有证据：开推理三轮里 nodecode 只在第 1 轮的 ls20 和 ft09 各编辑过一次，那局 ls20 只过了 1 关；两边在 ls20 上过 3 关的两局（nodecode 第 2 轮、superchat 第 3 轮）都没有在执行内部缩减上下文。
6. 只送回最近几次回复的推理。开推理时，推理占请求内容的大头（superchat 的 ls20 从第 40 次请求起占 80%）。DeepSeek 对当前一问里的助手消息也接受 ""（[决定](../decisions/accepted/2026-10-28-deepseek-reasoning-is-sent-back.md)里的实测表），所以 API 上可行；对模型表现的影响没有测过。
7. 回复中途卡住时重试。nodecode 把 200 响应头之后 120 秒没有数据算作一次失败的尝试，最多重试 5 次（`provider.lisp:77`、`:86`）。superchat 的自动重试覆盖的是 HTTP 429/5xx、`Operation timeout.` 和几种 curl 错误（`superchat--llm-transient-error-p`，`superchat-llm.el:271`）；自己的无响应计时器到点时，执行以 `timed-out` 结束，不重试。ARC 第 2 轮 ft09 遇到过一次：DeepSeek 回了 200 响应头后 184.5 秒没有正文，那次执行就此结束，靠跟进才接上。用户 2026-10-29 决定做，已实现：[决定](../decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md)，默认重发 2 次。25 个游戏那一批实地触发 4 次，都在同一次执行里重发（见那一节）。

不照搬的：只留一个工具（默认级别下每次改动都要问）；不设失败刹车，默认不设轮数上限；把模型执行的定义自动保存进用户的 Emacs（nodecode 为此配了扣住、代、回滚、启动时跳过失效定义一整套，superchat 没有对应的东西）。

## ARC-AGI-3 试跑（2026-10-28，DeepSeek，两边各 3 局）

两个 harness 都由我们自己跑，条件相同。测试机在 `.scratch/arc-bench/`（不进版本库），每局的证据在 `.scratch/arc-bench/runs/<harness>-<游戏>-pilot-<时间>/evidence/`。

- 游戏：官方工具包 `arc-agi` 0.9.9 的本地引擎。匿名密钥就能下载全部 25 个公开游戏（183 关），每关带人类基线。每局由 `tools/arc-server.py` 提供本地 HTTP 接口和 `arc3` 命令，算分用工具包自带的官方公式。
- 模型：`deepseek-v4-flash`（DeepSeek 的说明写它现在由 DeepSeek-V4.1-Flash-1017 提供），不开推理。superchat 自己发 `thinking: {"type": "disabled"}`；nodecode 不发，由代理补上同样的值。
- 题面和跟进消息两边一字不差（`harness/task.txt`、`harness/nudge.txt`）。一轮回答或执行结束、游戏没赢时，最多再发 3 次跟进。
- 每局上限 1500 万 token、60 分钟。token 由同一个代理按 DeepSeek 返回的 usage 计数（`proxy/model-proxy.py`），额度用完后代理对两边都回 429。
- superchat：出厂设置（6 个默认工具，失败刹车和每次执行 120 次工具调用的上限照旧），`danger-full-access`，运行在 task-sim 那样的沙箱 Emacs 里，代码是工作区的快照。
- nodecode：发布包 v0.1.3（build 7a035bfaf，sha256 已核对），用 `--ephemeral rpc` 驱动，运行在自己的沙箱里。设置：`update.mode` 为 `off`，预置 models.dev 目录，运行目录放在 `/private/tmp`（家目录下有 `~/AGENTS.md`、`~/.cursorrules`，它会往上一直读到根目录）。
- 每局各跑 1 次。

| 游戏 | harness | 过关 | 得分 | 动作（工具包计分卡） | 请求 | token | 请求中位数 / 最大（token） | 分钟 | 怎么结束 | 花费（models.dev 价格） |
|---|---|---|---|---|---|---|---|---|---|---|
| ls20 | superchat | 0/7 | 0 | 1579 | 303 | 1501 万 | 44750 / 104815 | 21 | 额度用完 | $0.101 |
| ls20 | nodecode | 0/7 | 0 | 446 | 228 | 1502 万 | 70534 / 106573 | 17 | 额度用完 | $0.074 |
| ft09 | superchat | 0/6 | 0 | 275 | 273 | 1157 万 | 40063 / 74739 | 21 | 3 次跟进用完 | $0.094 |
| ft09 | nodecode | 0/6 | 0 | 165 | 222 | 1501 万 | 62940 / 116501 | 14 | 额度用完 | $0.087 |
| vc33 | superchat | 1/7 | 0.18 | 273 | 358 | 1449 万 | 36358 / 69750 | 25 | 3 次跟进用完 | $0.099 |
| vc33 | nodecode | 0/7 | 0 | 1091 | 245 | 1505 万 | 70553 / 114160 | 23 | 额度用完 | $0.091 |

- 6 局一共只过了 1 关：superchat 在 vc33 用 31 步过了第 1 关（基线 7 步，这一关得 5.1 分）。用这个模型，不开推理，两边都几乎玩不动，所以"通关要多少 token"比不出来，能比的只有过程。
- 两边的输入 token 都有 98% 到 99% 命中 DeepSeek 的缓存；每局花费 7 到 10 美分。
- nodecode 在 ft09 和 vc33 各是一次回答（221 和 244 轮）一直做到额度用完；在 ls20，第一次回答做了 215 轮后自己结束，跟进那次做到额度用完。superchat 的执行有 3 次被失败刹车停下（"Stopped after 3 failed calls in a row"），1 次到了每次执行 120 次工具调用的上限（vc33），之后由跟进消息接着做；另有 1 次（ft09）用了 119 次工具调用后自己结束。到上限时没有问人（替身的回答记录是空的）。每次执行怎么结束，由 `tools/endings.py` 从 `events.json` 读出。
- 动作数取自工具包的计分卡。游戏接口自己的计数与它不同时以计分卡为准（nodecode ls20：计分卡 446，接口 449）。
- 分钟从第一次请求算到最后一次被代理放行的请求结束。额度用完后 superchat 每 30 秒重试一次被拒的请求，nodecode 不重试；按整局时钟算，superchat ls20 是 26 分钟。表里的数由 `tools/table.py` 算出。
- nodecode 在 ls20 的第一轮回答结束时报告：模型把 `sh` 改写成了自己的辅助函数，又把 `NODECODE.EVOLVED` 包改了名，此后每次 eval 都报 "The value NIL is not of type PACKAGE"，它从里面修不回来。这是模型执行的定义直接改动运行中程序的实例。
- 两边的模型都自己写了辅助函数，在一次 eval 里循环走很多步（superchat 的 `my-arc-act`，nodecode 层里的 `act`、`grid` 等定义）。
- 请求中位数（每次请求的 prompt token，`tools/table.py` 算出）：nodecode 62940 到 70553 token，superchat 36358 到 44750 token。模型目录给 nodecode 的窗口是 1048576，到 85% 才开始淘汰，这几局都远没到；superchat 的历史有自己的预算。
- 差异：nodecode 发 `max_tokens`（393216，取自模型目录），superchat 不发；6 局里没有一次回复因输出上限被截断（`finish_reason` 为 `length` 的回复数为 0）。nodecode 每个会话多一次起标题的附带调用，算在它的用量里。

## ARC-AGI-3 开推理（2026-10-28，两边各 3 局）

同样 3 个游戏，两边都开推理再跑一次。和上一节相比只改了下面这些，其余（题面、跟进、代理、每局 1500 万 token、每局 1 次）不变。证据在 `.scratch/arc-bench/runs/<harness>-<游戏>-think-<时间>/evidence/`。

- 推理：两边都是 `thinking: {"type": "enabled"}` 加 `reasoning_effort: "high"`。superchat 把 `superchat-llm-reasoning` 设为 `maximum`，由 llm-deepseek 自己发这两项；nodecode 把 `effort` 设为 `high`，它只发 `reasoning_effort`，`thinking` 由代理补上。逐个请求核对过（`tools/reasoning-wire.py`）：nodecode 每局只有 1 次请求不推理，是起标题的附带调用。
- superchat 的代码带着 [DeepSeek 推理送回的修复](../decisions/accepted/2026-10-28-deepseek-reasoning-is-sent-back.md)。没有它，superchat 开推理在第 2 次请求就 400，这次测试跑不起来。快照：ls20 和 ft09 是 `b4f09ff7db7a`，vc33 是 `892b8e5e77d8`，两者只差这个文件。
- 时间上限改成 3 小时（上一节是 60 分钟），两边一样，让 token 额度成为唯一起作用的限制。6 局同时跑（上一节是两局同时跑），所以分钟数不能和上一节比。

| 游戏 | harness | 过关 | 得分 | 动作 | 请求 | token | 请求中位数 / 最大（token） | 推理 token | 分钟 | 怎么结束 | 花费 |
|---|---|---|---|---|---|---|---|---|---|---|---|
| ls20 | superchat | 1/7 | 0.5 | 483 | 88 | 1505 万 | 192292 / 308149 | 262133 | 29 | 额度用完 | $0.218 |
| ls20 | nodecode | 1/7 | 2.2 | 353 | 151 | 1509 万 | 79659 / 276623 | 310600 | 38 | 额度用完 | $0.262 |
| ft09 | superchat | 5/6 | 56.26 | 362 | 115 | 1510 万 | 105943 / 304183 | 359223 | 40 | 额度用完 | $0.301 |
| ft09 | nodecode | 6/6 | 100 | 102 | 69 | 1016 万 | 165647 / 251491 | 147375 | 17 | 通关 | $0.143 |
| vc33 | superchat | 3/7 | 18.38 | 221 | 132 | 1503 万 | 90034 / 273677 | 252491 | 31 | 额度用完 | $0.221 |
| vc33 | nodecode | 4/7 | 19.14 | 258 | 115 | 1503 万 | 129122 / 262861 | 181327 | 25 | 额度用完 | $0.178 |

第一次过第 k 关时，这一局已经用掉的 token（`tools/levels.py`，按游戏服务器和代理的时钟对齐，精确到秒）：

| 游戏 | harness | 第 1 关 | 第 2 关 | 第 3 关 | 第 4 关 | 第 5 关 | 第 6 关 |
|---|---|---|---|---|---|---|---|
| ls20 | superchat | 263 万 | | | | | |
| ls20 | nodecode | 93 万 | | | | | |
| ft09 | superchat | 32 万 | 50 万 | 72 万 | 135 万 | 1478 万 | |
| ft09 | nodecode | 39 万 | 80 万 | 148 万 | 287 万 | 722 万 | 915 万 |
| vc33 | superchat | 79 万 | 157 万 | 425 万 | | | |
| vc33 | nodecode | 69 万 | 365 万 | 715 万 | 1322 万 | | |

- 开推理以后两边都能玩：6 局一共过了 20 关，上一节是 1 关。nodecode 通关了 ft09：6 关都比人类基线用的步数少，共 102 步、1016 万 token。
- 合计 nodecode 过了 11 关，三局平均分 40.4；superchat 过了 9 关，平均分 25.0。每关的花费互有先后：ft09 前 4 关和 vc33 第 2、3 关，superchat 用的 token 更少；ls20 和 vc33 的第 1 关、ft09 第 5 关起，nodecode 更省。每种组合只跑了 1 次；下一节又跑了两轮，nodecode 在这一轮的领先没有再出现。
- 请求被推理撑大，两边都是。superchat 的 ls20 一次执行做到额度用完：到第 40 次请求，送回的推理已占请求内容的 80%，最后一次请求 30.8 万 token。nodecode 也送回推理，vc33 最后一次请求里推理占 68%。两边送回的范围不同：nodecode 送回每一轮的推理；superchat 只送回当前这次执行的，跟进以后的新执行不带前一次执行的推理（ft09 第 77 次请求：前一次执行留下的 30 条助手消息都没有这个字段）。
- nodecode 的模型自己裁过上下文：ls20 第 90 次请求前，模型用 nodecode 的上下文编辑把 176 条消息换成 8 条，留下一段 1782 个字符的笔记（事实 `context.edited`）。ft09 在通关以后也做过一次，vc33 没有。这几局里，superchat 的消息数只在跟进开始新执行时下降（ft09 从 204 条到 62 条，vc33 从 222 条到 80 条，是从 Tape 重建早先几轮），一次执行内部只增不减；ls20 没有跟进，一直涨到 186 条。所以 ls20 的请求中位数 nodecode 是 8.0 万，superchat 是 19.2 万。
- superchat 这一轮没有一次被失败刹车停下，也没有到 120 次工具调用的上限；工具调用失败的比例是 4% 到 17%。ft09 和 vc33 的第一次执行自己结束（75 和 106 次调用），跟进后的那次执行做到额度用完。nodecode 每局都只有一次回答：ft09 通关后结束，ls20 和 vc33 做到额度用完。
- nodecode ls20 的第 125 次请求带了一张图，DeepSeek 不收（400，"You have uploaded an unsupported image"），nodecode 去掉图片后重试，接着做下去。
- 两边都整局重开过：nodecode 的 ls20 和 vc33、superchat 的 ft09，计分卡里都记了不止一遍。计分卡按最好的那一遍算（工具包 `scorecard.py:241`，`max(run.score for run in self.runs)`），所以重开不会丢掉已经过的关。
- 输入 token 两边都有约 99% 命中缓存。每局花费 $0.14 到 $0.30，6 局合计约 $1.32。没有一次回复因输出上限被截断，所以 nodecode 发 `max_tokens`、superchat 不发，这一轮也没有影响。

## ARC-AGI-3 开推理再跑两轮（2026-10-29，两边各 6 局）

上一节（下面叫第 1 轮）每种组合只有 1 次，分不清差别来自 harness 还是碰巧。同样 3 个游戏、同样条件又跑了两轮（第 2、3 轮）。每轮 6 局同时跑、相隔 40 秒启动，和第 1 轮一样（`harness/launch-batch.sh`）；第 2 轮 00:09 到 00:56，第 3 轮 00:57 到 01:51。证据在 `.scratch/arc-bench/runs/<harness>-<游戏>-think2-<时间>/evidence/` 和 `-think3-` 下。

- 条件和第 1 轮相同，逐项核对过。推理参数逐个请求核对（`tools/reasoning-wire.py`）：superchat 每个请求都是 `thinking` 为 enabled 加 `reasoning_effort: "high"`，nodecode 每局只有起标题的那 1 次不推理。开推理 18 局的每个回复里，DeepSeek 的 `system_fingerprint` 都是 `bc19ba97cbe016479903222bb1578db6`，中途没有换模型（`tools/checks.py`）。第 2、3 轮没有 400。superchat 的快照都是 `d78300c221a1`，代码文件和第 1 轮相同，只差文档。

| 游戏 | harness | 轮 | 过关 | 得分 | 动作 | 请求 | token | 请求中位数 / 最大（token） | 推理 token | 分钟 | 怎么结束 | 花费 |
|---|---|---|---|---|---|---|---|---|---|---|---|---|
| ls20 | superchat | 2 | 1/7 | 3.57 | 343 | 207 | 1506 万 | 59140 / 161300 | 185249 | 35 | 额度用完 | $0.191 |
| ls20 | superchat | 3 | 3/7 | 16.42 | 203 | 116 | 1519 万 | 145726 / 230176 | 168274 | 25 | 额度用完 | $0.167 |
| ls20 | nodecode | 2 | 3/7 | 21.43 | 469 | 104 | 1507 万 | 139210 / 302472 | 178039 | 27 | 额度用完 | $0.184 |
| ls20 | nodecode | 3 | 1/7 | 3.57 | 419 | 113 | 1521 万 | 131758 / 249760 | 164139 | 25 | 额度用完 | $0.167 |
| ft09 | superchat | 2 | 6/6 | 95.21 | 168 | 100 | 926 万 | 71358 / 194119 | 206174 | 40 | 通关 | $0.177 |
| ft09 | superchat | 3 | 6/6 | 100 | 138 | 75 | 719 万 | 72658 / 223747 | 176624 | 20 | 通关 | $0.141 |
| ft09 | nodecode | 2 | 4/6 | 19.07 | 1109 | 103 | 1560 万 | 130677 / 317423 | 234744 | 32 | 额度用完 | $0.227 |
| ft09 | nodecode | 3 | 4/6 | 47.62 | 125 | 103 | 1530 万 | 136414 / 318026 | 230062 | 31 | 额度用完 | $0.211 |
| vc33 | superchat | 2 | 2/7 | 10.71 | 829 | 151 | 1511 万 | 86818 / 229291 | 241409 | 39 | 额度用完 | $0.220 |
| vc33 | superchat | 3 | 3/7 | 21.43 | 334 | 193 | 1511 万 | 70647 / 180295 | 308049 | 46 | 额度用完 | $0.272 |
| vc33 | nodecode | 2 | 3/7 | 20.84 | 136 | 99 | 1502 万 | 119484 / 355009 | 282760 | 38 | 额度用完 | $0.238 |
| vc33 | nodecode | 3 | 3/7 | 4.97 | 239 | 103 | 1515 万 | 155788 / 283668 | 189471 | 26 | 额度用完 | $0.183 |

三轮放在一起（`tools/spread.py think think2 think3`）：

| 游戏 | harness | 第 1 轮 | 第 2 轮 | 第 3 轮 | 过关 低 / 中 / 高 | 得分 低 / 中 / 高 |
|---|---|---|---|---|---|---|
| ls20 | superchat | 1 关，0.5 | 1 关，3.57 | 3 关，16.42 | 1 / 1 / 3 | 0.5 / 3.57 / 16.42 |
| ls20 | nodecode | 1 关，2.2 | 3 关，21.43 | 1 关，3.57 | 1 / 1 / 3 | 2.2 / 3.57 / 21.43 |
| ft09 | superchat | 5 关，56.26 | 6 关，95.21 | 6 关，100 | 5 / 6 / 6 | 56.26 / 95.21 / 100 |
| ft09 | nodecode | 6 关，100 | 4 关，19.07 | 4 关，47.62 | 4 / 4 / 6 | 19.07 / 47.62 / 100 |
| vc33 | superchat | 3 关，18.38 | 2 关，10.71 | 3 关，21.43 | 2 / 3 / 3 | 10.71 / 18.38 / 21.43 |
| vc33 | nodecode | 4 关，19.14 | 3 关，20.84 | 3 关，4.97 | 3 / 3 / 4 | 4.97 / 19.14 / 20.84 |

第 2、3 轮第一次过第 k 关时，这一局已经用掉的 token（`tools/levels.py`）：

| 游戏 | harness | 轮 | 第 1 关 | 第 2 关 | 第 3 关 | 第 4 关 | 第 5 关 | 第 6 关 |
|---|---|---|---|---|---|---|---|---|
| ls20 | superchat | 2 | 86 万 | | | | | |
| ls20 | superchat | 3 | 59 万 | 587 万 | 1519 万 | | | |
| ls20 | nodecode | 2 | 67 万 | 408 万 | 830 万 | | | |
| ls20 | nodecode | 3 | 104 万 | | | | | |
| ft09 | superchat | 2 | 31 万 | 43 万 | 130 万 | 371 万 | 582 万 | 868 万 |
| ft09 | superchat | 3 | 95 万 | 170 万 | 195 万 | 257 万 | 544 万 | 674 万 |
| ft09 | nodecode | 2 | 138 万 | 209 万 | 287 万 | 688 万 | | |
| ft09 | nodecode | 3 | 62 万 | 100 万 | 189 万 | 302 万 | | |
| vc33 | superchat | 2 | 1212 万 | 1383 万 | | | | |
| vc33 | superchat | 3 | 20 万 | 53 万 | 154 万 | | | |
| vc33 | nodecode | 2 | 118 万 | 184 万 | 415 万 | | | |
| vc33 | nodecode | 3 | 122 万 | 436 万 | 858 万 | | | |

- 三轮合计，superchat 过了 9、9、12 关，共 30 关（满分 60）；nodecode 过了 11、10、8 关，共 29 关。每轮三局的平均分 superchat 是 25.0、36.5、46.0，nodecode 是 40.4、20.4、18.7；9 局平均 superchat 35.8，nodecode 26.5。
- 每个游戏两边的得分范围都重叠：ls20 两边都是两次 1 关、一次 3 关，中位数都是 3.57；ft09 superchat 56.26 到 100，nodecode 19.07 到 100；vc33 superchat 10.71 到 21.43，nodecode 4.97 到 20.84。按开跑前定的标准（范围不重叠才算差别），三个游戏都分不出两边。第 1 轮 nodecode 的领先（40.4 对 25.0）后两轮没有再出现：按每轮平均分，nodecode 赢 1 轮，superchat 赢 2 轮。superchat 9 局平均分更高，主要来自 ft09。25 个游戏各跑 1 次的结果见下一节，也是打平。
- 同一组合三次之间的差距，比两边之间的差距大；一局的走向常由模型开头选的打法决定。nodecode 的 ft09 第 1 轮 102 步通关、得 100 分；第 2 轮模型先写了一个逐格点击的 Python 脚本（"brute-force sweep"），过第 1 关时已走了 823 步（基线 43 步），这一关只得 0.3 分，整局 19.07 分。
- 能比"通关要多少 token"的只有 ft09：superchat 通关 2 次（第 2 轮 168 步、926 万 token，第 3 轮 138 步、719 万），nodecode 1 次（第 1 轮 102 步、1016 万）。其余 15 局都做到额度用完。
- 第 2、3 轮的请求中位数 superchat 是 5.9 万到 14.6 万 token，nodecode 是 12.0 万到 15.6 万。nodecode 的模型这两轮一次也没有编辑上下文（6 局的 `context.edited` 都是 0；第 1 轮 ls20 和 ft09 各 1 次）。
- 每次执行怎么结束（`tools/endings.py`）：superchat 第 2、3 轮共 12 次执行，自己结束 5 次（其中 2 次是通关），做到额度用完 4 次，失败刹车 1 次（第 2 轮 ls20，第 45 次调用），另外 2 次在下一条说；没有一次到 120 次工具调用的上限，工具调用失败的比例是 6% 到 12%。nodecode 6 局都只有一次回答，都做到额度用完。
- superchat 有 2 次执行不是因为游戏而结束，都由跟进消息接上：
  - 第 2 轮 ft09 的第 35 次请求，DeepSeek 回了 200 响应头，184.5 秒后正文还是 0 字节（代理记下 `IncompleteRead(0 bytes read)`）。superchat 的无响应计时器到点（`superchat-response-timeout` 120 秒，调用工具时乘 1.5），这次执行以 `timed-out` 结束，没有重试；跟进以后接着玩，最后通关。superchat 的自动重试覆盖的是 HTTP 429/5xx、`Operation timeout.` 和几种 curl 错误（`superchat--llm-transient-error-p`，`superchat-llm.el:271`）；无响应计时器到点时直接结束执行，不经过这个判断。2026-10-29 改为什么都没收到时重发（[决定](../decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md)）。nodecode 把 200 响应头之后 120 秒没有数据算作一次失败的尝试，最多重试 5 次（`provider.lisp:77`、`:86`）；它这 9 局没有遇到这种情况。
  - 第 3 轮 vc33 的第 51 次请求，模型的工具调用参数不是合法 JSON：以 `{"progn\n  (defun arcg` 开头，没有 `"expression"` 键，也没写完。superchat 记下 `[API error: An error occurred when calling an LLM tool: "Invalid JSON in tool call arguments: could not parse JSON stream: 2, nil, 8"]`，这次执行失败。superchat 本有处理这种错误的修复重发（`superchat-llm-unknown-tool-retries`，默认 2 次；给 `llm-tool-call-error` 的纠正说明在 `superchat-llm.el:2474`），这次没有起作用：紧接着的第 52 次请求只有 1.96 万 token，是跟进开的新执行，不是带纠正说明的重发。原因在 llm.el 和 superchat 之间：不流式时，llm.el 在成功回调里解析参数，抛出的 `llm-tool-call-error` 经它的 `:on-error` 交给 superchat 时变成了 `error`，修复重发只认 `llm-tool-*`（[事后分析 0001](../postmortems/0001-a-broken-tool-call-lost-its-type.md)，2026-10-29 已修复）。18 局里，superchat 的 1170 次工具调用有这 1 次参数不合法，nodecode 的 949 次没有。
- 游戏源码一直没被读到。开推理 18 局里，superchat 有 6 局、nodecode 有 1 局，模型试过读运行目录以外的东西（游戏源码 `games/vc33/…/vc33.py`、测试机目录、工具包的 site-packages）。得到的都是沙箱的 "Operation not permitted"、superchat 自己的 "is not readable by this Emacs"，或者错误被丢掉后的空结果（`tools/checks.py` 列出每次调用和它的回答）。运行目录里测试机自己的记录是读得到的：superchat ft09 第 2 轮在跟进以后读了自己的代理记录（`evidence/http/*-response.txt`），从中找回已走的步数和关数。真实使用 superchat 时没有这个目录；这只影响这一局。
- 输入 token 两边都有 99.1% 到 99.7% 命中缓存。第 2、3 轮 12 局花费 $2.38，开推理三轮 18 局合计 $3.70。

## ARC-AGI-3 全部 25 个游戏（2026-10-29，两边各 25 局）

25 个公开游戏，每个游戏两边各跑 1 局，开推理，其余条件和前两节相同（题面、跟进、代理、每局 1500 万 token、3 小时、`reasoning_effort: "high"`）。6 局同时跑、相隔 40 秒启动（`harness/launch-many.sh`），02:50 到 08:20。证据在 `.scratch/arc-bench/runs/<harness>-<游戏>-g25-<时间>/evidence/`，汇总由 `tools/many.py g25` 算出。

- superchat 换了版本：快照 `9785cc747fc6`，开跑前冻结一次，25 局都用它（`harness/freeze-code.sh`）。它比第 2、3 轮的 `d78300c221a1`（第 1 轮的两个快照，代码文件与之相同）多了 2026-10-29 的两处修复，运行的代码只差 `superchat-llm.el`（[事后分析 0001](../postmortems/0001-a-broken-tool-call-lost-its-type.md)、[一次什么都没收到的尝试会重发](../decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md)）。所以 superchat 在 ls20、ft09、vc33 上的这一局不和前三轮合在一起算。nodecode 仍是同一个发布包。
- 条件逐项核对过（`tools/checks.py`、`tools/reasoning-wire.py`）：50 局的回复里 `system_fingerprint` 都是 `bc19ba97cbe016479903222bb1578db6`；50 局的请求都是推理开、`reasoning_effort: "high"`，没有异常；6359 次请求没有 400，有 2 次 502（cd82 两边各一次，DeepSeek 在回复之前断开）。

| 游戏 | 关数 | superchat 过关 | 得分 | 动作 | nodecode 过关 | 得分 | 动作 |
|---|---|---|---|---|---|---|---|
| ar25 | 8 | 5 | 41.67 | 204 | 7 | 77.78 | 447 |
| bp35 | 9 | 2 | 4.86 | 126 | 1 | 0.11 | 129 |
| cd82 | 6 | 6（通关） | 67.49 | 302 | 3 | 17.61 | 178 |
| cn04 | 6 | 2 | 14.04 | 137 | 1 | 4.76 | 63 |
| dc22 | 6 | 2 | 13.25 | 248 | 1 | 0.34 | 515 |
| ft09 | 6 | 4 | 47.62 | 241 | 4 | 47.62 | 141 |
| g50t | 7 | 1 | 0.98 | 272 | 1 | 3.57 | 170 |
| ka59 | 7 | 1 | 0.2 | 259 | 4 | 17.06 | 294 |
| lf52 | 10 | 2 | 0.86 | 309 | 2 | 1.12 | 254 |
| lp85 | 8 | 5 | 41.67 | 195 | 5 | 37.28 | 144 |
| ls20 | 7 | 1 | 3.57 | 457 | 1 | 3.57 | 861 |
| m0r0 | 6 | 2 | 14.29 | 116 | 2 | 10.3 | 380 |
| r11l | 6 | 1 | 4.76 | 65 | 4 | 47.62 | 48 |
| re86 | 8 | 4 | 27.78 | 593 | 5 | 41.67 | 407 |
| s5i5 | 8 | 3 | 16.67 | 345 | 4 | 20.98 | 396 |
| sb26 | 8 | 4 | 15.75 | 392 | 1 | 2.78 | 420 |
| sc25 | 6 | 3 | 13.9 | 256 | 3 | 12.02 | 242 |
| sk48 | 8 | 1 | 0.35 | 347 | 1 | 0.19 | 238 |
| sp80 | 6 | 1 | 4.76 | 845 | 1 | 4.76 | 79405 |
| su15 | 9 | 3 | 13.33 | 197 | 3 | 13.33 | 95 |
| tn36 | 7 | 2 | 8.52 | 514 | 2 | 5.92 | 3696 |
| tr87 | 6 | 1 | 2.83 | 241 | 5 | 40.76 | 397 |
| tu93 | 9 | 8 | 56.11 | 343 | 5 | 33.33 | 307 |
| vc33 | 7 | 3 | 8.38 | 246 | 3 | 13.22 | 191 |
| wa30 | 9 | 1 | 2.22 | 419 | 3 | 13.33 | 396 |
| 合计 | 183 | 68 | 平均 17.03 | | 72 | 平均 18.84 | |

- 两边打平。过关 superchat 68、nodecode 72（满分 183）；25 个游戏的平均分 superchat 17.03、nodecode 18.84；逐个游戏比得分，superchat 高 11 个、nodecode 高 10 个、平手 4 个，符号检验 p = 1.0。这点差距比前三轮里同一组合前后两次的差距还小（ft09 nodecode 从 19.07 到 100）。
- 两边各有大胜：superchat 通关 cd82（这一批唯一的通关，302 步、1389 万 token），tu93 过 8 关对 5 关；nodecode 在 tr87 过 5 关对 1 关，r11l 4 关对 1 关，ar25 7 关对 5 关。除了 superchat cd82，49 局都做到额度用完，所以这一批"通关要多少 token"只有 1 个数。
- ls20、ft09、vc33 这一批的得分：nodecode 是 3.57、47.62、13.22，都在它前三轮的范围里；superchat 是 3.57、47.62、8.38，ft09 和 vc33 低于它前三轮的最低分（56.26、10.71）。这两局没有不合法的工具调用，也没有回复卡住，两处修复改动的代码都没有走到；推论是同一组合本来的波动，每局只有 1 次，分不清。
- 动作列把所有动作都算上，包括模型重开（RESET）以后放弃的那几遍。计分只取最好的一遍（`scorecard.py:241`）：每关得分是 (人类基线步数 / 这一遍用的步数)² × 100，最高 100，再按关的序号加权平均（`scorecard.py:168`、`:185` 到 `:205`）。所以 6 关的游戏只过了第 1 关、且没比人类多用步数时，得分是 100 × 1/21 = 4.76，这一批有 4 局是这个数。nodecode sp80 的 79,405 步来自模型写的三个随机搜索脚本（`.arc/rand.py`、`rand2.py`、`rand3.py`）：每次重开，再随机走一串，一共重开 6,073 次，第 20、21 分钟各两万多步；这些都在试第 2 关，没有过，也没有拉低得分。nodecode tn36 的 3,696 步里有 183 次重开，superchat sp80 的 845 步里有 54 次。
- 花费：50 局 $10.99，superchat $6.09、nodecode $4.90。差的 $1.19 全在输出 token 上：superchat 输出 774 万、nodecode 575 万；输入两边约 99.4% 命中缓存，命中和未命中的花费两边几乎一样。superchat 每局平均 139 次请求，nodecode 115 次。
- 照出厂设置，两边结构上的不同：superchat 每次执行有 30 分钟上限，这一批截断了 11 次执行（10 局）；其中 9 次截断时有请求在路上，superchat 自己关掉了它（代理记下 `BrokenPipeError`），lp85 和 re86 的截断恰好碰上额度用完。失败刹车停下 5 次。superchat 25 局里 22 局用了跟进（1 次的 14 局、2 次的 8 局），nodecode 只有 dc22 用了 1 次：测试机自动发的"继续"是 superchat 这一批打法的一部分，真实使用时要用户自己发。分钟数两边不能比（6 局同时跑、上限截断、下一条的等待）；superchat 每局中位数 40 分钟，nodecode 28 分钟。
- 测试机的一个缺陷，对 superchat 不利：DeepSeek 有 4 次在回了 200 之后断开（ka59 第 102 次，sk48 第 28、108 次，su15 第 138 次请求）。代理用 HTTP/1.0 转发（`proxy/model-proxy.py`），已经发出 200 响应头，断开后把自己的 502 文字接在后面再关连接；superchat 收到的是一个"完整"的坏回复，错误没有传回，只能等无响应窗口到点再重发。窗口随请求大小放大，这 4 次是 646 到 1054 秒，而上游实际在 37 到 422 秒就断了；如果断开时就能报错并重发，4 次合计可以少等约 43 分钟。这 4 次等待所在的执行，后来都被 30 分钟上限截断。nodecode 是流式请求，同一批里上游断开 4 次，它每次记一次 `turn.provider_retry` 后重发，没有等无响应窗口。这几次请求没有 usage，不占额度。对得分的影响没有量；去掉 ka59、sk48、su15 三个游戏，仍是打平：过关 63 对 64（满分 159），平均分 18.73 对 20.02，10 比 9、平手 3，p = 1.0。细节见[决定](../decisions/accepted/2026-10-29-a-silent-attempt-is-sent-again.md)的 Consequences。
- 两处修复的实战情况：不合法的工具调用参数在 50 局、6359 次请求里一次也没有出现，事后分析 0001 的修复没有被实地用到。卡住重发触发了上一条的 4 次，每次都在同一次执行里逐字节相同地重发；走的是窗口到点这条路，真实使用时 curl 直接报错的那条路没有测到。真实使用时 superchat 经 curl 走 HTTP/2，测试机代理和 DeepSeek 之间是 HTTP/1.1；HTTP/2 上的断开 curl 报 92、16、18 或 56；这一批跑的时候自动重试只认 56，之后改为四个都认（[DeepSeek 中途断开](deepseek-drop-curl-codes.md)）。cd82 第 56 次请求 DeepSeek 在回复之前断开，代理回 502，superchat 30 秒后重发，走的是原有的 5xx 重试。
- 两边的模型都试过读游戏源码，都没读到（`tools/checks.py` 列出每次调用和回答）：superchat 13 局、23 次调用，nodecode 7 局、10 次。nodecode tn36 用 `lsof` 看到测试机一个 Python 进程的工作目录，也试过 `/System/Volumes/Data/Users/…` 这条路径。得到的都是沙箱的 "Operation not permitted"、superchat 自己的 "is not readable by this Emacs"，或者错误被丢掉后的空结果；superchat lp85 用 `find /` 按文件名找 `lp85`，没有找到，按 `arc` 找到的只是无关的系统文件。源码的完整路径写在运行目录里游戏服务器的日志 `evidence/game/server.log` 里，这个日志读得到。两边也都读过自己运行目录里测试机的记录（动作日志、服务器日志、代理记下的请求和回复）：superchat 12 局、35 次，nodecode 9 局、31 次。superchat cn04 列出过自己的代码快照 `code/` 下的文件名，没有读内容。真实使用时没有这些目录；这是两边都有的测试机问题。

## 没有做的

- 对照表里 nodecode 一侧来自代码和文档；只有四节 ARC 测试运行过它的发布包。
- 没有读 FL-2026-006 报告，也没有查过有没有第三方复现。
- 只在四节 ARC 测试的 74 局里看过 DeepSeek 的缓存命中，日常任务没有核实。
- ARC 的 25 个公开游戏开推理各跑了 1 次；ls20、ft09、vc33 另有开推理 3 次（superchat 那 3 次是两处修复之前的版本）和不开推理 1 次。模型只有 `deepseek-v4-flash`。
- 测试机代理把上游断开变成"完整"的坏回复（见 25 个游戏那一节），这个缺陷没有修，受影响的 ka59、sk48、su15 没有重跑。
- 聊天频道、网页、导入别的 agent、profiles 没有对照。
