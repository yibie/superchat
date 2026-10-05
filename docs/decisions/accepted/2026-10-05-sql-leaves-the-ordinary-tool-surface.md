# Decision: `sql` 退出普通模型工具面；历史类工具跟着执行走

Status: accepted — 用户，2026-10-05：对"`sql` 是否退出普通模型的工具面（包括内置的 research 预设）"回答"同意"。"实现中定下的几点"是在这个回答之下定的机制，随本次向用户报告。

## Problem

M11 第 2 片让自动回忆和模型的 `memory_search` 只在本 workspace 内找，并在"没有覆盖的地方"记下了一个口子：模型的 `sql` 工具能直接查整张记录表。2026-10-05 用现有的回忆测试夹具复现：在项目 P 的对话里说一句带暗号的话，从项目 Q 的对话、从不属于任何项目的临时对话里调用 `sql`，都读到了 P 的原话；同样条件下 `memory_search` 读不到。

查的时候还发现第二个口子。`memory_search`、`tool_history`、`file_history`、`recent_errors` 四个工具取"当前对话"用的是工具运行那一刻当前 buffer 的 `superchat--session-id`，不是发起调用的那次执行所属的对话：

- 取不到时传下去的是 nil，而底下的查询把 nil 当作"查全部对话"。
- 当前 buffer 显示的是另一个 workspace 的对话时，查的就是那一个。

## Decision

1. **Superchat 自带的东西都不再把 `sql` 提供给模型。** 它本来就不在默认工具面上；这次把它从内置的 `research` 预设里拿掉，并且 `superchat-llm-tool-names` 设为 `all` 时也不再包含它。
2. **`sql` 工具本身保留**，只在用户自己点名时存在：用户把 `"sql"` 写进 `superchat-llm-tool-names` 的名单，或写进自己预设的工具清单。它的说明改为明说"读所有 workspace 的所有对话"。
3. **四个历史类工具以执行所属的对话为准**（工具管线给每次调用绑定的 `superchat--tool-execution-context`），没有执行上下文时才用当前 buffer 的。
4. **说不出是哪段对话时什么都不查**，返回一句错误，不再退回到查全部对话。
5. **不变的**：用户自己输入的 `/recall` 仍按 `superchat-memory-recall-scope`，默认搜全部对话；`eval_elisp` 和 shell 仍只由权限级别管，workspace 不隔离它们。

验收：`scripts/workspace-scope-regression.el`。

## 实现中定下的几点

- "用户点名"的判断在 `superchat-tools--sql-offered-p`：只认名单里写着 `"sql"`，不认 `all`。
- 取对话的函数是 `superchat-tools--history-session`，四个工具共用；顺序与 `work_record`、定时器工具取对话的顺序相同。
- 底下的查询函数（`superchat-view-tool-history` 等）没有改：nil 表示全部对话仍是它们的约定，`/recall` 这样的用户入口还在用。改的是模型工具这一层不再把 nil 传下去。

## Alternatives considered

**改写模型写的 SQL，给它加上 workspace 条件。** 没选：SQL 可以用子查询、`WITH`、连接绕开任何字符串层面的改写，改写只能做到"看起来隔离"。

**删掉 `sql` 工具。** 没选：用户想自己查库时仍有用，点名启用是用户的明确选择；删除是比"退出普通工具面"更大的改动，用户没有要求。

**保留在 `all` 里。** 没选：`all` 的意思是"自带的都要"，设它的人未必知道其中一个工具会越过 workspace。

## Consequences

- 用 `/research` 的对话不再有 `sql`；要跨对话找东西，模型用 `memory_search`（本 workspace 内）。
- 以前把 `superchat-llm-tool-names` 设成 `all` 并依赖 `sql` 的配置，需要改成名单并点名。
- 委派的子代理调用历史类工具时，查的是子代理自己的对话。子代理的对话没有绑定 workspace，所以 `memory_search` 在那里只看得到它自己；内置的三个子代理预设都不带这四个工具。

## 没有覆盖的地方

- 没有用真实模型跑；证据是脚本测试。
- 点名启用 `sql` 之后，它仍然读全部对话，没有范围。
- 用户的 `/recall` 搜全部对话时，结果没有标出各条来自哪个 workspace。
- Telega 和 emacsclient 两个入口怎样取对话没有查。
