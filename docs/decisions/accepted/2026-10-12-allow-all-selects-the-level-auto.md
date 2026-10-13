# Decision: "全部允许"把这段对话切到档位 auto

Status: accepted 2026-10-12, implemented。用户看过后说照现在的样子提交（leader 转达："commit slice 5c as is"）。用户原话（leader 转达）："审批这里，应当添加一个选项 auto，或者 allow all time"。做法由 leader 写在转达里；下面"实现时定的"几条是实现者定的，用户可以改。

证据的范围：脚本模型，批处理 Emacs。没有真实模型，没有人按过新按钮。

## Problem

审批问题只有"允许这一次"和"拒绝"。一段对话里连着做几十次 `eval_elisp` 或 shell 的人，要答几十次。

已有的出路都不是用户要的：

- `/permissions danger-full-access` 是模式 `danger-full-access` 加策略 `never`。`never` 让这段对话没有向用户提问的通道（模型的 `ask_user` 不可用），`save_buffer` 也因为问不了而被拒绝。
- 模式 `danger-full-access` 加策略 `ask` 正是"完全控制不问，保存仍问，模型仍可提问"。完全控制类工具放不放行只看模式，不看策略（`superchat-sandbox--level-rule`）。但它以前只能显示成 `custom`，选不了。
- 按工具的永久放行（批准哈希、免提示名单）是 [2026-09-17 的决定](../implemented/2026-09-17-permissions-the-dsh-way.md) 特意删掉的。这里没有把它带回来。

## Decision

1. **第四个档位 `auto`**：`:sandbox danger-full-access :approval ask`。`/permissions auto` 可选，`/permissions` 的名单里列出。它是会话的档位，不是对某个工具的放行：选了它，这段对话里所有完全控制类和第三方工具都不再问。
2. **审批多一种回答"全部允许"**：这一次调用按"允许一次"放行（记录里仍是 `allowed-once`），同时把这段对话切到 `auto`。三个入口：对话面板的按钮 `[Allow all]`，`M-x superchat-approval-allow-all`，`M-x superchat-pending` 里那一行上按 `a`。
3. **只影响这一段对话**。别的对话、新开的对话仍是各自的档位。
4. **回到逐次询问**用 `/permissions edit-buffers`，不另加回答项。
5. **不加新状态、新事实**：走现有的 `permission.level`、`sandbox.mode`、`approval.policy`。档位只记在 Tape 上，重启后读回来仍是 `auto`。

## 实现时定的，用户可以改

- **先记档位，再放行。** 档位记不下（写 Tape 出错）时报错，问题仍开着，什么都没放行。
- 已经回答过、或已随执行作废的问题，"全部允许"什么都不做，也不切档位。
- 回答之后面板上写 `Allowed; this conversation no longer asks for tools; saving still asks`（2026-10-13 改，架构走查 C6：原来写的是 `…no longer asks`，而 `auto` 档位下保存仍然会问）；回显区说明现在是 `auto`、怎么改回去。
- 档位名单的顺序是 `edit-buffers, auto, danger-full-access, read-only`。
- 按钮字样是英文 `Allow all`，和面板上其它按钮一致。
- 按"全部允许"之前没有二次确认。

## 改了什么

- `superchat-sandbox.el`：`superchat-permission-levels` 多一项。
- `superchat-approval.el`：`superchat-approval-answer-all`、命令 `superchat-approval-allow-all`、面板的结束语、纯文字行里多列一个命令。
- `superchat.el`：面板多一个按钮；回答后停用的是三个回答按钮。
- `superchat-pending.el`：`superchat-pending-allow-all`，键 `a`。

## 证据

`scripts/approval-auto-regression.el`，14 条，全过。

最先写的 12 条在改动前的代码（`b1900632`）上：11 条红，1 条是护栏。

| 用例 | 旧代码上红在哪 |
|---|---|
| allow-all-runs-this-call-and-selects-auto | `(= 1 aw--effect)`，实际 0：没有"全部允许"，测试只好拒绝 |
| the-next-full-control-call-is-not-asked | 应只问 1 次，实际 2 次 |
| an-answered-question-is-not-answered-again-with-all | `superchat-approval-answer-all` 不存在 |
| saving-is-still-asked-and-can-be-asked | `Unknown permission level: auto` |
| the-model-keeps-ask-user | 同上（这条当时的写法后来改了，见下） |
| another-conversation-is-not-switched | 档位应为 `auto`，实际不是 |
| the-level-is-read-back-from-the-tape | 记录里的档位应为 "auto"，实际 nil |
| permissions-names-and-selects-it | 名单里没有 `auto` |
| the-panel-has-allow-all-and-pressing-it-answers | 面板上没有这个按钮 |
| the-command-answers-with-all | `(= 1 aw--effect)`，实际 0 |
| the-list-answers-with-all | `(= 1 aw--effect)`，实际 0 |
| allow-once-still-asks-the-next-time | 护栏，本来就过 |

后补的 2 条（过期的按钮不起作用；档位记不下时问题仍开着）没在旧代码上跑。

"模型仍可提问"那条改过一次：最初它看的是请求里的工具名单，结果 `danger-full-access` 下名单里也有 `ask_user`，对照不成立。现在它看的是那条让 `ask_user` 可用的提问通道有没有挂上（`superchat--with-supported-input`），和 `m6-permissions` 里现成的那条用例同一种看法：`auto` 下挂上，`danger-full-access` 下没有。

变异（副本上）14 处，全部被发现。第一轮有 1 处让整套卡住（档位整个删掉，定时器里报错，问题一直开着），卡住没有算作发现；把测试改成这种情况下明确失败之后，它被 3 条用例发现。

现有测试改了一处：`m6-permissions` 里断言档位名单字样的那一行，加上了 `auto`。不属于 M3 回归集。

## 没有证实、没有做

- 没有人在真实窗口里看过四个按钮的面板，没有人按过 `[Allow all]`。
- 真实模型在 `auto` 下的行为没有跑。
- `auto` 下 `save_buffer` 真的弹出一个问题并被回答，没有走完整条路径；测的是档位规则给出"问"、策略是 `ask`。
- 用 `M-x superchat-approval-allow-all` 或按钮时，如果档位记不下，用户看到的是一条报错；这条报错的样子没有人看过。
- 第三方（MCP）工具在 `auto` 下也不再问，这是模式 `danger-full-access` 原有的含义，没有为 `auto` 单独收窄。
