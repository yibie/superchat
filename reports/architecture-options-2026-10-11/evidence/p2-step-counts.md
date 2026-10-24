# P2：现有产品上，七个场景各要几步、要看几个地方（静态计数）

计数规则：一"步"是用户的一次命令或一个按键序列（`M-x xxx` 加参数算一步；在输入区打字不算步，只算发送）。"要看的地方"是用户为了回答"谁改了什么、现在在等什么"必须打开或盯着的界面数。每行引用快照 `75e83370` 的 文件:行。现有产品的"用户看到什么"只引代码或决定记录里的沙箱运行；上手清单（`docs/reports/2026-10-07-hands-on-checklist.md`）尚未填写，所以这里没有任何"真人体验"的断言。

常用入口和按键（代码事实）：

| 动作 | 绑定 / 命令 | 出处 |
|---|---|---|
| 打开对话 | `M-x superchat` | `superchat.el:4099` |
| 发送 | `C-c C-c` → `superchat-send-input` | `superchat.el:4952`；`superchat-dispatcher.el:1424` |
| 停止 | `C-c C-k` → `superchat-cancel` | `superchat.el:4959`，`:1339` |
| 暂停 / 继续 | `C-c C-p` / `C-c C-r` | `superchat.el:4962–4963` |
| 侧栏 ↔ 整页 | `C-c C-w` | `superchat.el:4967`，`:3572` |
| 审阅屏 | 执行结束自动出现并拿光标 `superchat-changes--review-after-run`；`s` 保存、`n` 下一个、`q` 离开 | `superchat-changes.el:481`，`:306–308` |
| 手动审阅 | `M-x superchat-changes` 或 `/changes` | `superchat-changes.el:611`，`:624` |
| 未保存提示 | 回显区一行 `superchat-changes--announce` | `superchat-changes.el:267` |
| 执行结束提示 | 回显区一行 `superchat--notify-run-terminal`（不在该对话窗口时） | `superchat.el:990` |
| 无人看守的执行结束 | 模式行 `[Superchat: N]`，鼠标点或 `M-x superchat-changes-notices`；列表里 `RET` 去对话并清掉、`d` 清一条、`D` 清全部 | `superchat-changes.el:668`，`:794`，`:777–779` |
| 工作记录 | `M-x superchat-work-list`；`RET` 打开一条（含 Executions 段）、`c` 标完成 | `superchat-work.el:631`，`:619–620`，`:641` |
| 定时器 | 用户没有列出/取消定时器的命令：`superchat-timer.el` 里没有 `interactive` 函数；只有模型的 `timer_list` / `timer_cancel` 工具 | `grep -n interactive superchat-timer.el` 为空 |
| 审批 | 小缓冲区 `read-multiple-choice`，同步阻塞，`y`/`n`/`?` | `superchat-approval.el:108–147` |
| 改写选区 | `M-x superchat-rewrite-region`；`C-c C-a` 接受、`C-c C-k` 拒绝 | `superchat-rewrite.el:592`，`:614` |
| 批注 | `M-x superchat-annotate`；对话里 `>annotate` | `superchat-annotate.el`；`superchat-core.el:152–167` |

## 逐场景

### S1 基本一轮：让它改一个文件，并带规矩（清单 1、2）

| 步 | 用户做什么 | 用户看到什么 | 出处 |
|---|---|---|---|
| 1 | 在 `log.org` 里 `M-x superchat` | 对话在侧栏打开（`superchat-display-style` 默认 `editor`） | `superchat.el:4099`；roadmap M10 1b |
| 2 | 打字，`C-c C-c` | 流式回答；结束后回答下方一行产品写的记录（改了哪些 buffer、是否保存） | `superchat-render.el` `superchat-render-run-record-line`；决定 2026-09-27 |
| 3 | 审阅屏自动出现并拿光标，按 `s` 或 `n` | diff 着色，一次一个 buffer | `superchat-changes.el:481`，`:306–307` |
| 4 | `q` | 光标回原处 | `superchat-changes.el:308` |

步数：4（不含打字）。要看的地方：2（对话、审阅屏），回显区偶有一行。规矩：`AGENTS.md` ≤ 6000 字符随每次请求（`superchat-workspace.el:245`）。

### S2 轻量一次性：改写选区 / 批注（清单 3）

| 步 | 用户做什么 | 出处 |
|---|---|---|
| 1 | 选中文字 | — |
| 2 | `M-x superchat-rewrite-region`，输入指令 | `superchat-rewrite.el` |
| 3 | 看替换文字，`C-c C-a` 接受（或 `C-c C-k` 拒绝） | `:592`，`:614` |

步数：3。要看的地方：1（浮层）。批注命令：2 步（`M-x superchat-annotate` + 指令），意见以 overlay 挂在行下，`M-n`/`M-p` 走动，`superchat-annotate-clear` 清掉；对话里写 `>annotate` 是 1 步，回答同时留在对话里（决定 2026-10-10）。

### S3 到点醒来的工作 + 事后找回（清单 5、6）

| 步 | 用户做什么 | 用户看到什么 | 出处 |
|---|---|---|---|
| 1 | 发送带"用工作记录跟踪…2 分钟后接着做"的消息 | 第一次执行照常，审阅屏出现 | 同 S1 |
| 2–3 | `s`、`q` | — | — |
| （两分钟后） | 什么也不做 | 新开一段对话（不显示）里执行；**不弹审阅屏、不拿光标**；回显区一行"N buffers … unsaved"；模式行 `[Superchat: 1]` | `superchat.el:3971`（新对话）；`superchat-changes.el:481`（`--unattended-run-p` 跳过审阅屏）；`:267`；`:668` |
| 4 | `M-x superchat-changes-notices` → `RET` | 到那段新对话；通知被清掉 | `superchat-changes.el:794`，`:777` |
| 5 | `M-x superchat-work-list` → `RET` | 记录全文 + "Executions" 段（每次执行的编号、对话、结局） | `superchat-work.el:631`，`:641` |
| 6 | `M-x superchat-changes` → `s` | 看到"- 检查完毕"并保存 | `superchat-changes.el:611` |

步数：事后找回至少 3 条命令（4–6），加 2 次 `RET` / `s`。要看的地方：4（通知列表、被唤醒的对话、工作记录详情、审阅屏），加主对话里那一行"记录被接手"。没有一处同时回答"谁改了什么、现在在等什么"（第三轮评审 3(c) 的 OPEN-C 探针实测：到点没启动的任务在 work / notice 两表都缺席）。

### S4 审批放着不答，然后停下（清单 9、12）

| 步 | 用户做什么 | 用户看到什么 | 出处 |
|---|---|---|---|
| 1 | 发送"用 shell 命令看一下…" | 小缓冲区出现 `Allow 'shell-command' once? (…)`，同步等待 | `superchat-approval.el:108–147`；`superchat-sandbox.el:300–327`（full-control 在 `edit-buffers` 下 ask） |
| 2 | 去别的 buffer 待一分钟 | Emacs 不卡（定时器照跑，沙箱运行 3/3，roadmap 第 343 行）；**小缓冲区被这个问题占着**；期间到点的另一个执行会在上面再叠一个问题（roadmap 第 355 行，step-4 运行观察） | roadmap `docs/roadmap.md:343`，`:355` |
| 3 | 回来按 `n` | 工具不执行，回答不编造结果 | 决定 2026-09-17 M6 |
| 4 | 多步任务跑到一半 `C-c C-k` | Run 以 `cancelled` 封存；已做的改动留在 buffer | `superchat.el:1339`；request-lifecycle |
| 5 | `M-x superchat-changes` | 审阅屏列出未保存 buffer | `superchat-changes.el:611` |

步数：停下并看清状态 2 步。要看的地方：2（对话、审阅屏）。审批问题没有列表、没有"全部拒绝"。

### S5 执行途中关掉窗口 / 切到别处（清单 7、11）

| 步 | 用户做什么 | 用户看到什么 | 出处 |
|---|---|---|---|
| 1 | 发任务，`C-x k` 杀掉对话 buffer | 执行继续；只有之后的 `ask_user` 调用失败并告诉模型"用户问不到" | 决定 2026-10-07 第 2 条；沙箱运行 3/3、1/1 |
| 2 | 半分钟后 `M-x superchat` | 对话从 Tape 重建，看到答案 | `superchat.el:3909` |
| — | 切到另一个 Workspace 的对话问一句 | 各答各的 | roadmap `:342`（3/3、1/1） |

步数：2。要看的地方：1。

### S6 重启之后；到点却没启动 / 说不清（清单 13；决定 2026-10-06）

| 步 | 用户做什么 | 用户看到什么 | 出处 |
|---|---|---|---|
| 1 | 启动 Emacs，`M-x superchat` | `work-records` / `timers` 行挂上时扫描：到期的唤醒、没启动的任务被补跑；"说不清"的任务在对话里留一句并记 `timer.unconfirmed` | `superchat-work.el:909`（`wake-due`）；`superchat-timer.el`（`catch-up` / `retry-tasks`）；决定 2026-10-06 第 3 条 |
| 2 | `M-x superchat-work-list` → `RET` | 两次执行；被打断的那次结局 `interrupted` | `superchat-work.el:641`；决定 2026-10-06 第 7 条 |
| — | 想知道"哪个定时任务到点没启动" | 没有用户命令；模型的 `timer_list` 把 fired / unstarted / unconfirmed 压成一种"fired"描述 | 第三轮评审 3(c) OPEN-C |

步数：2。要看的地方：2，且"到点未启动 / 不确定"只在那段对话的文字里。

### S7 弱模型（本机 Qwen，8192 上下文）

不是操作步数，是每次请求的固定开销（P1 实测，脚本传输，估算 token 的办法见 `p1-request-size.log`）：

| 入口 | 请求字节 | 估算 token | 其中系统文字 | 其中工具 schema |
|---|---|---|---|---|
| 临时对话一句话 | 15 252 | ~3 800 | 3 069 字符 | 14 个，11 144 字节 |
| 项目对话（6000 字规矩 + 当前记录 + 3 个技能） | 27 074 | ~6 600 | 10 820 字符 | 同上 |
| 就地问 | 13 808 | ~3 300 | 6 722 字符 | 4 个，2 006 字节 |
| 改写选区 | 11 216 | ~2 650 | 6 996 字符 | 无 |
| 批注命令 | 11 145 | ~2 630 | 7 191 字符 | 无 |

另三件代码事实：`superchat-response-timeout` 仍是 120 秒（`superchat-llm.el:55`；10-05 的工具数报告把它列为本地模型的问题，没有改）；关思考要在 `superchat-llm-reasoning-parameter-overrides` 里声明端点，否则本地 Qwen 开着思考，批注实验里 8191 token 全被思考用掉、正文为空（决定 2026-10-10 批注）；工具错误几乎都在 `eval_elisp`（工具数报告），而 `eval_elisp` 在默认级别下每次都问（`superchat-sandbox.el:300–327`）。
