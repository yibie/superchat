# Decision: M11 第 5 片——把窗口的摆法告诉模型

Status: accepted — 用户，2026-10-02："开始第五片"。条目来自 [workspace 契约](2026-10-01-workspace-work-record-session-rules.md)的 W8、W9；"leader 的选择"一节是 leader 在这句话之下定的机制，随本片向用户报告。

## Problem

请求里只说"用户是从哪个 buffer 发的请求"，不说屏幕上现在怎么摆：有几个窗口、各显示什么、对话窗口在哪、有哪些 tab。用户说"把右边那个换成日程""排成左右两栏"时，模型只能先用 `eval_elisp` 去查，或者猜。真实任务 T6（排成左右两栏）一直不稳定。另外，模型改了当前 workspace 以外的文件，保存前的审阅屏不会提醒。

## Decision

- **W8：** 对话、就地问、就地做的请求，在"请求从哪来"那段之后多一段 `[Emacs windows]`：当前 frame 的 tab（多于一个或开着 tab-bar 时），当前 tab 的每个窗口——显示的 buffer、访问的文件、宽度、从左上角起的位置、哪个是选中的、哪个是这段对话。这段在请求被收集的那一刻取，是事实不是指令。重新摆仍由模型用 `eval_elisp` 做；布局的保存和恢复交给 Emacs。
- **W9：** 模型改过的文件如果不在这段对话的 workspace 里，不拦；`/changes` 的清单在那一行后面注明"outside this workspace"，审阅屏在 diff 上方写一行"Outside this conversation's workspace (根目录)."。临时对话没有 workspace，不标。

验收：`scripts/window-arrangement-regression.el`；T6（排成左右两栏）改前改后各跑几次对比。

## leader 的选择

1. **对话窗口不写 buffer 名，写"this conversation"。** 名字对模型没用，而且测试夹具里的名字是随机的，写进去会让基线每次都不一样。
2. **窗口最多列 12 个，tab 最多列 10 个**，多出的只报数；buffer 名超过 60 个字符截断。
3. **只在对话、就地问、就地做三种请求里带。** 子代理、workflow、压缩这些自己备好文字的请求不带，和规矩、操作说明的范围一致。用户在请求检查器里确认过的文字不再重新插入。
4. **位置从所列窗口的左上角算起**，不把菜单栏、tab 栏算进去。**不写高度**：高度随 frame 和回显区变化，完整跑 M3 套件时终端 frame 会矮一行，写进去会让同一请求的文字随环境变。
5. **"在 workspace 外"按文件的真实路径判断**，看它是不是在根目录之下。
6. **smoke 的每个用例先把窗口恢复成一个窗口显示 `*scratch*`、关掉 tab 栏，结束后放回原样**（改 `scripts/smoke.el` 的夹具），这样请求里的窗口一段不受之前测试留下的摆法影响。

## Alternatives considered

**只在用户提到"窗口"时才带。** 输在要猜用户的意思，而且"右边那个"这类说法没有"窗口"二字。

**带上每个窗口里看得到的文字。** 输在太长，而且模型要看内容可以自己读 buffer。

**在 workspace 外的改动先问再改。** 输在契约 W9 写明不拦。

## Consequences

- 每次对话请求多一小段文字（两个窗口约 150 个字符）。
- smoke 夹具每个用例都会重置窗口摆法。
- canonical smoke 的基线 A（请求文字多了这一段）和 V（收集器多一个）随之改变。
