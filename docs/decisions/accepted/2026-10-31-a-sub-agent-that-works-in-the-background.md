# Decision: 委派可以放到后台，对话不用等

Status: accepted — 用户，2026-10-31："开始"，针对[主动 Agent 提案](../proposed/2026-10-31-an-agent-that-acts-without-being-asked.md)的第 1 步"不挡路"。这一步的范围比提案窄，见"和提案的差别"。"实现时定的"各条都可以改。

## Problem

`delegate_to_subagent` 的结果在子代理交回时才送达，主执行要一直等。这段时间用户发的话进不去：执行中的消息要等下一次发请求（`superchat-llm.el` 的 `drain-steers-function`），而子代理没交回就没有下一次请求。这一步解决的是"用户开口了，但不想等"。[审查](../../research/super-agent.md)查明它不是主动工作的前提：不同对话的执行本来就互不阻塞。

## Decision

`delegate_to_subagent` 多一个可选参数 `background`：

- 给了 `background`：工具立刻回答"已启动"和子代理的编号。发起委派的执行可以结束，用户可以接着说话。
- 子代理交回时，报告在这段对话里开始一次新的执行。这次执行排在对话的 follow-up 队列里（做法和 `superchat-work--queue-round` 相同），对话正忙时等它结束，不插进正在进行的执行。
- 没给 `background`：行为不变。

## 实现时定的

1. **后台子代理不是发起执行的子执行。** 它不带 `:parent-run-id`，否则父执行一结束，`superchat-runtime--cancel-children` 就会取消它。代价：用户停掉对话里的执行时，后台子代理不跟着停；要在 `superchat-pending` 里按 `k`，或用 `superchat-subagent-cancel`。
2. **报告启动的执行不算"没人看着的工作"。** 它的 Actor 是 `subagent-report`，不在 `superchat-changes--unattended-actors` 里，所以不进模式行的计数。但这时没有人在等这个回答，审阅屏不出现（[光标留在你去的地方](2026-10-31-the-cursor-stays-where-you-went.md)）。
3. **失败和停止也报告。** 超时、被停止、出错时，报告就是那段错误文字，模型会被告知。工具调用阶段就失败的（比如预设名不存在）作为这次工具调用的结果返回，不启动新的执行。
4. **同时运行的后台子代理有上限。** 上限是 `superchat-subagent-parallel-max`，超过时工具回答说明已满，不启动。
5. **对话窗口关了也能交回。** 报告到达时，对话的 buffer 不在就重新打开（`superchat--render-session-view`，定时任务也这样做）。打不开时在回显区说一句；有当前工作记录时，任务和结果仍记在记录上（K14）。
6. **`superchat-pending` 说明后台子代理为谁工作。** 这一行写"working in the background for conversation …"，不写"started by you"。

## 和提案的差别

提案的第 1 步写的是"`shell-command` 和委派都改成后台"。这里只做委派，原因是查代码后发现的事实：

- 默认工具是 `read_buffer`、`read_file`、`edit_buffer`、`eval_elisp`、`work_record`、`timer_create`（`superchat-llm-default-tool-names`）。`shell-command` 和 `delegate_to_subagent` 都不在里面。
- 把 `shell-command` 改成异步工具后，`eval_elisp` 里的 `(superchat-tool "shell-command" ...)` 会报错，因为 eval 里不能调用异步工具（[2026-10-26 的决定](2026-10-26-eval-elisp-calls-tools.md)）。
- 默认路径上卡住 Emacs 的是 `eval_elisp`，它有 30 秒时限，超时的文字已经让模型改用 `make-process`。

`delegate_to_subagent_parallel` 也不变：它仍然等全部交回。

## Alternatives considered

**把委派一律改成后台。** 没选：很多任务里模型要拿报告接着做，一律后台会多出一次执行，也会改变现有行为。由模型按任务选择。

**报告用 `superchat-runtime-inject` 送进去。** 没选：inject 不会启动执行，没有执行在跑时，报告要一直等到用户下次说话。

**报告用 `superchat-runtime-steer` 插进正在进行的执行。** 没选：执行可能正在发最后一次请求，那时插进去的话送不到模型（`run.steer-undelivered`）。

**保留 `:parent-run-id`，只让取消子执行时跳过后台子代理。** 没选：`assert-parent-attach-active` 和父执行的记账都以"子执行在父执行之内"为前提，后台子代理不满足这个前提。

**另加一个 `delegate_in_background` 工具。** 没选：用户要求尽量减少工具数量（2026-10-26）。

## Consequences

- 这只对 `delegate_to_subagent` 可用的配置生效。默认工具表里没有它，要在 `superchat-llm-tool-names` 里加上或由预设提供。
- 每份后台报告都会多一次模型请求，有额外费用。
- 没有变化的阻塞：`eval_elisp` 运行时 Emacs 仍然不响应，最长 30 秒；打开 `shell-command` 时它仍然同步；审批问题仍然占着一层 recursive edit。
- 子代理和 follow-up 队列都只在内存里。Emacs 退出时，正在运行的后台子代理和它的报告都丢失（`kill-emacs-hook` 清空队列），下次启动不会补上。这是主动 Agent 提案第 6 步的事。
- 模型什么时候该选 `background`，只靠工具说明引导，没有在真实模型上测过。

## 验证

- `scripts/subagent-background-regression.el`，脚本模型，11 个测试。写代码之前 9 个红；另外 2 个是不变行为的守护。8 个变异（保留 `:parent-run-id`、不记 Actor、去掉上限、丢掉提前结束的结果、不重开对话、不写对话里那一行、不调度 follow-up、改掉列表文字）都被测试抓到；其中"丢掉提前结束的结果"第一次没抓到，补了一条断言。
- 原有套件照常通过：composition 22、pending 27、work-record 25、workspace-subagent-rules 4、goal-rounds 16、object-scope 66、m6-permissions 60、m3 135（2 个跳过）。
- 真实模型，DeepSeek `deepseek-v4-flash`，批处理 Emacs，3 次（`.scratch/subagent-background/real-deepseek.el`）：提示里明说"我不想等"。3 次第一次执行都结束而子代理还在运行；第二个问题都在子代理交回之前做完；报告都启动了一次 Actor 为 `subagent-report` 的执行。

没有验证的：没有明说"不想等"时模型会不会选 `background`；真人在键盘前的体验；Emacs 窗口下的显示。
