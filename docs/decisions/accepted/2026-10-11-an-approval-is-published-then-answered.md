# Decision: 审批先发布，之后再回答

Status: accepted — 2026-10-11。用户对"审批问题在哪里回答"的裁定是"同意你的建议"，建议是：对话 buffer 里一行加表上可答，小缓冲区兜底（五条裁定里的第 4 条，见 `2026-10-11-a-run-records-who-started-it.md`）。界面的具体形式用户没有定，下面"实现时定的"各条都可以改。只有脚本测试，没有真实模型，没有真人；最关键的一步（在键盘前继续编辑）批处理测不了。

## Problem

审批用 `read-multiple-choice` 提问（`superchat-approval-confirm`）。那是阻塞读键：问题挂着时整个 Emacs 停在提问上。实机清单第 9 步"去别的 buffer 待一分钟"做不到；第 4 步的真实运行里还见过两个审批叠在一起、都停着。

代码事实：工具执行调用审批并等它的返回值（`superchat-tool-execution.el` 里 `superchat-approval-request-snapshot` 那一处），同步工具要把结果同步交回模型库。所以"发布后函数立刻返回、稍后由回调继续"需要把整条工具管线改成异步，这次没有做。

## Decision

审批的调用栈仍然在等，但等的时候 Emacs 在它下面照常运行。

- **发布**：新的回答函数 `superchat-approval-publish` 把问题登记进内存里的一张表，在对话里显示一行，在回显区说一句，然后等待。
- **等待**：在键盘前是一次 recursive edit，用户可以继续编辑、切 buffer、用别的命令；问题被回答后这一层自动退出。没有键盘时（批处理）让定时器和进程照常跑。
- **回答**：`superchat-approval-answer HANDLE ALLOW` 是唯一的决定入口，只对还开着的问题生效一次。
  - 对话里：`M-x superchat-approval-allow`、`superchat-approval-deny`、`superchat-approval-details`。在某段对话的 buffer 里调用，答的是那段对话的问题；只有一个问题开着时在哪里调用都行；开着几个而当前 buffer 不属于其中任何一段时报错，不猜。
  - 表里：`M-x superchat-pending` 的 "Asks you now" 行上按 `y` 允许、`n` 拒绝；`k` 仍是停掉那次执行。
- **默认入口**：`superchat-approval-function` 的默认值改为 `superchat-approval-ask`。它按新选项 `superchat-approval-prompt` 选择：`publish`（默认）或 `minibuffer`。
- **小缓冲区是兜底**：`superchat-approval-confirm` 一字未改，把 `superchat-approval-prompt` 设为 `minibuffer` 就回到原来的行为。

没有变的：

- `approval.requested` / `approval.decided` 这一对事实，以及"允许的是提问那一刻的参数快照"。
- 策略 `never` 在任何回答者之前就拒绝。
- "问题不比它的执行活得久"：执行结束后问题被放弃，仍由 `superchat-approval--ask-while-run-lives` 负责，记成 `run-ended`。
- 批处理里和键盘宏运行时，默认回答者算"没有人"，结果是 `unavailable`。
- 提问期间执行的不活动计时不走。
- 模型用 `ask_user` 提的问题仍走它自己的那套面板，这次没有碰。

## 实现时定的，用户可以改

- **对话里的那一行是挂在对话 buffer 末尾的一段提示文字**（overlay），不是可点的按钮，也不是像 `ask_user` 那样的面板。回答靠命令。
- **没有给回答绑全局按键。** 表里是 `y` / `n`；对话里要 `M-x`。
- **等待用 recursive edit。** 所以问题开着时模式行会多一层方括号。
- **用户在问题之上自己又进了一层**（比如正在小缓冲区里、或另一次 recursive edit 里）时回答，决定会被记下，但要等用户离开自己那一层才生效，不替用户退出。
- **同时开着几个问题时，栈里靠外的那个先被回答，也要等靠里的那个有了结果才能继续。** 两个都能回答，只是继续的顺序是后问的先走。
- `C-]`（`abort-recursive-edit`）等同于在旧提问上按 `C-g`：记成用户放弃，这次执行被打断。

## 先红后绿

`scripts/approval-publish-regression.el`，11 条。最先写的 7 条在 `e99036b8` 上都红：

| 用例 | 旧代码上红在哪 | 当时的值 |
|---|---|---|
| `an-open-question-is-listed-and-answered-later` | 问题开着时看到的应是 1 个问题、工具 `eval_elisp`、requested 1、decided 0 | 什么都没看到（`nil`） |
| `a-later-no-refuses-the-call` | 决定应是 `("rejected")` | `("unavailable")` |
| `a-question-is-answered-once-and-only-by-its-handle` | 四次回答的结果应是 `(nil 0 t nil)` | `nil` |
| `a-question-does-not-outlive-its-run` | 应拿到问题的 handle | `nil` |
| `the-conversation-shows-the-question-and-answers-it` | 对话里那一行应含 `eval_elisp` | 空字符串 |
| `the-list-answers-the-row-that-asks` | 表里应有 1 行 asking | 0 行 |
| `the-default-answerer-publishes-unless-told-otherwise` | 小缓冲区只应被读 1 次 | 2 次 |

旧代码上没有这些函数，所以红的直接原因多是"没有这个机制"。有分辨力的证据是变异测试。

后补的 4 条没有在旧代码上跑：两个问题同时开着时命令答的是所在对话的那一个、表答的是那一行的那一个、表只答"在问"的行、键盘宏不算人。

## 变异测试

在副本上把实现改坏 16 处，每处跑这一套。第一轮：11 处被发现，2 处漏掉，3 处让整套卡死。卡死不算发现。

- 漏掉的两处：已决定的问题仍被列为"开着"；表对任何一行都肯回答。各补了断言。
- 卡死的三处：发布后不清掉登记、被告知用小缓冲区却照样发布、按工具名而不是按对话找问题。测试改成在这三种情况下明确失败：每条用例从空的登记表开始，等待加了时限，两个问题的夹具会把剩下没答的拒掉。

第二轮 16 处全部被发现，每处都跑完了整套。

写这套测试时我自己错了一处：`a-question-does-not-outlive-its-run` 最初断言执行的状态是 `cancelled`。现行契约里，问题还开着时执行被停掉，状态是 `audit-incomplete`（`approval-wait` 和 `m6-permissions` 两套都这样断言），改成了接受这两种。

第一版实现还弄坏过现有测试：我把 `superchat-approval-confirm` 本身改成了"默认发布"，直接调用它的 `approval-input` 那一套就在批处理里一直等，4 条超时。改成保留 `superchat-approval-confirm` 原样、另加 `superchat-approval-ask` 之后，那一套 6 条全过。

## 没有证实的

- **在键盘前继续编辑。** 批处理不能进 recursive edit，测试走的是"没有键盘"那条等待路径。从定时器或进程过滤器里进 recursive edit 之后，编辑、切窗口、别的执行的流式输出是否都正常，没有人看过。这是这一片最大的未知。
- **实机清单第 9 步对这一片仍是未验证。** 用户在 2026-10-11 亲手做过一次第 9 步，但那次运行没有跑到这一片的代码：运行中的 Emacs 启动于本地 18:21:54，`superchat-approval.el` 最后改于 19:08:53；emacsclient 查到 `superchat-approval-prompt` 不存在、`superchat-approval-publish` 未定义、`superchat-approval-function` 仍是 `superchat-approval-confirm`。Tape 里那次的 12 次审批最长开着 15 秒，没有"离开一分钟再回答"。（这些是 leader 会话查到并转来的，本会话没有重查。）要验证需重启 Emacs 或重新加载 `superchat-approval` 后再做。
- **冒烟出现过一次没有复现的不符。** 提交前的全量检查里，冒烟那一遍报了 1 mismatch；那份日志只留了结论行，不知道是哪一项。之后在同一份快照上单独跑 4 遍、在两份全新快照上按同样顺序各跑第一遍，6 遍都是 0 mismatches。原因没有查清。冒烟用的是固定回答"允许"的替身，不经过发布逻辑，所以推断与这次改动无关，但这只是推论。
- 对话末尾那一行在真实窗口里的位置和样子（会不会压在输入区后面）。
- 两个问题同时开着时的实际手感。
- 真实模型。

## 没有解决的

- 工具管线仍是同步的：问题开着时，那次执行的调用栈一直挂着。
- 问题只登记在内存里，重启后没有（它的执行也没有了）。Tape 上留着没有配对的 `approval.requested`。
- 无人看守的权限级别仍然悬着：没人在时，问题会一直开着，直到执行的期限。
- `top-level` 会直接拆掉等待的那层栈，不经过"放弃"的记账。
