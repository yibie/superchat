# 04 三个 /mcp-* 命令符合既有的内建命令契约

status: blocked
blocked_by: 01
writer:
authority: `n/a` — 本切片不引入也不修改业务规则：它让三个 MCP 命令**符合**既有的内建命令契约（零参数调用、返回字符串才渲染），而不是去改那条契约。派发契约本身的权威模块尚未进入 Kernel 授权图 —— 那正是 K2 待补的「Turn routing」，不在本 SPEC 范围内。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Problem and goal 末段（三个坏命令）
context_ref: `KERNEL.md` — 授权图目前**没有**派发规则的条目（已知空缺）
evidence_ref:

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

## Evidence
