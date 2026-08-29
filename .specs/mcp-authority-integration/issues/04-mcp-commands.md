# 04 三个 /mcp-* 命令符合既有的内建命令契约

status: done
blocked_by: 01
writer:
authority: `n/a` — 本切片不引入也不修改业务规则：它让三个 MCP 命令**符合**既有的内建命令契约（零参数调用、返回字符串才渲染），而不是去改那条契约。派发契约本身的权威模块尚未进入 Kernel 授权图 —— 那正是 K2 待补的「Turn routing」，不在本 SPEC 范围内。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Problem and goal 末段（三个坏命令）
context_ref: `KERNEL.md` — 授权图当时**没有**派发规则的条目（已知空缺；K2 已补上 `Turn routing` 授权行）
evidence_ref: E-20260827-003

## Goal

`/mcp-start-server`、`/mcp-stop-server`、`/mcp-health` 不再报错或静默无输出。

## Scope

- `superchat.el:1280-1282` 所指向的三个入口，以及/或 `superchat-mcp.el` 中对应函数。
- `test/test-superchat-mcp.el` — 追加本切片测试。

**硬约束（不得违反）**：`superchat--handle-default-command`
（`superchat-dispatcher.el:452-462`）对所有内建命令都是 `(funcall func)` 零参数调用，
且只有返回值是字符串时才渲染为 `:buffer`。修复必须让这三个命令**适配**该契约
—— 例如零参数入口内部用 `completing-read` 在活跃服务器中选一台，健康检查把 alist
格式化成字符串。**不得**为此修改通用派发的实参传递方式：那会改变所有内建命令的
契约，其权威模块还没被 Kernel 记录，属于越界。

## Acceptance

1. `/mcp-start-server` 与 `/mcp-stop-server` 不再触发 `wrong-number-of-arguments`。
2. `/mcp-health` 产生可渲染的字符串输出，而不是 `:noop`。
3. `superchat--builtin-commands` 中其余条目的调用方式未被改动
   （用一条断言钉住通用派发契约未变）。

## Verification

`emacs -Q --batch -L . -l test/run-tests.el` —— 0 unexpected，总数不低于 895。

## Verification summary (do)

JJ change: `sxxslrvn`。改动文件为 Scope 三项：`superchat.el`、`superchat-mcp.el`、
`test/test-superchat-mcp.el`。

`emacs -Q --batch -L . -l test/run-tests.el` → **944 tests, 928 as expected, 0 unexpected,
16 skipped**（本切片开始前 935 / 919 / 0 / 16）。指挥方独立复跑，结果一致。

**硬约束已守住**：`superchat-dispatcher.el` 不在改动范围内 —— 通用派发契约
（`superchat--handle-default-command`：所有内建命令零参数调用、返回字符串才渲染）未被修改。
三个命令改为适配该契约，新增零参数包装入口，实测 arity 均为 `(0 . 0)`：

| 命令 | 入口 | arity |
| --- | --- | --- |
| `/mcp-start-server` | `superchat-mcp-start-server-command` | `(0 . 0)` |
| `/mcp-stop-server` | `superchat-mcp-stop-server-command` | `(0 . 0)` |
| `/mcp-health` | `superchat-mcp-health-report` | `(0 . 0)` |

契约本身也被钉住：新增测试遍历 `superchat--builtin-commands` 的每一个条目，
断言其余命令仍按零参数调用（`test/test-superchat-mcp.el:1095` 附近）。

**折入本切片的两处遗留已修**（切片 03 复核发现）：`superchat.el` 中
`superchat--detect-response-mode` 与 `superchat-backend-show` 不再 append 原始
`superchat-mcp-get-tools` plist；该符号在 `superchat.el` 中仅余一条解释为何不再使用的注释
（`superchat.el:1310`）。`/tools` 不再重复计数 MCP 工具。

## Evidence
