# 02 每服务器声明 effects，未声明者回落 unknown

status: blocked
blocked_by: 01
writer:
authority: `superchat-tools.el` | owned — Kernel 授权图「Tool effect declaration」。superchat-mcp.el 只是**提供**声明值，经 `superchat-register-tool` 的 `:effects` 这个既有接口交出去；它不得成为第二个裁决点。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Decision and boundaries「Effects are declared per server」
context_ref: `KERNEL.md` — Concepts/Effect；`CONTEXT.md` — Authority
evidence_ref:

## Goal

MCP 工具能向 Authority 交出 effect 声明；未声明的服务器落到与今天等价的 `unknown`。

## Scope

- `superchat-mcp.el` — 新增 `defcustom superchat-mcp-server-effects`：
  服务器名 → effect 描述符列表。值结构必须**为工具层预留位置**，
  使得日后加入按工具覆盖不需要改动已有声明的形状。
- `superchat-mcp.el` — 一个纯查表函数：给定服务器名返回 effect 描述符列表，
  无声明时返回单个 `unknown` 描述符。
- `test/test-superchat-mcp.el` — 追加本切片测试。

**不在本切片**：把这些值真正交给 `superchat-register-tool`（那是切片 03）。
**不得改动** `superchat-mcp-servers` 的类型或语义。
**不得实现**按工具覆盖。

## Acceptance

1. 已声明的服务器 → 精确返回其声明的 effect 描述符。
2. 未声明的服务器 → 单个 `unknown` 描述符；这与今天的 de-facto 行为等价
   （`superchat-authority.el:322-330`：无 resolver → `unknown` → attended 时 ask、
   unattended 时 deny）。
3. `superchat-mcp-servers` 的 `:type` 与取值语义与改动前逐字节一致。
4. 值结构能容纳一条按工具的声明而不破坏既有形状（用一个构造出的样例断言，
   即使读取逻辑本期不实现）。

## Verification

`emacs -Q --batch -L . -l test/run-tests.el` —— 0 unexpected，总数不低于 895。

## Evidence
