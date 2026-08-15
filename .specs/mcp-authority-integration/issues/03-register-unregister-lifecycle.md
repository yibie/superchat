# 03 MCP 工具经 superchat-register-tool 上下线，走与插件工具相同的执行路径

status: blocked
blocked_by: 01, 02
writer:
authority: `superchat-tools.el` | owned — Kernel 授权图「Model-facing tool surface」。本切片把 MCP 的工具暴露从 superchat-llm.el 的旁路搬回这个权威模块。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — AC「Register one MCP server's tools」/「Unregister one MCP server's tools」；Unchanged contracts 1-3
context_ref: `KERNEL.md` — Concepts/Tool；工具执行生命周期条目；「A synthesized tool may not shadow a built-in or MCP tool name」
evidence_ref:

## Goal

MCP 工具与插件工具走**完全相同**的一条路：
`superchat-tools--external-llm-tools` → `superchat--agent-wrap-function` →
`superchat-tool-execute`。服务器停止后不留任何残留。

## Scope

- `superchat-mcp.el` — 服务器连接时用 `superchat-register-tool` 注册每个工具
  （名字 `mcp:<server>:<tool>`，`:effects` 取自切片 02）；停止时用
  `superchat-unregister-tool` 逐个注销。
- `superchat-llm.el:782-784` — **删除**把 `mcp-tools` 直接 append 进请求工具表的三行。
  这是 MCP 工具绕开 allowlist 机制出现在工具面上的那条路径。
- `superchat-synthesis.el:96` — 影子守卫必须继续可用：
  `superchat--syn-check-name` 要能看到已注册的 MCP 名字。保留
  `superchat-mcp-get-tools`，或换成等价的注册表读取器，由 `do` 择一并说明理由。
- `test/test-superchat-mcp.el` — 追加本切片测试。

## Acceptance

1. 注册后到达模型工具面的是 `llm-tool` 结构，不是 plist。
2. 调用一个已注册的 MCP 工具会到达 `superchat-tool-execute`。
3. 注销后，该服务器的工具、effect resolver、approval preview **三者全部消失**
   （悬空 resolver 是静默的，必须显式断言），且**其它服务器的工具不受影响**。
4. 同名重复注册是替换，不是追加出第二条。
5. `superchat--syn-check-name` 能看到已注册的 MCP 名字（今天恒为 nil，
   且 `ignore-errors` 正好吞掉了签名错误）。
6. `superchat-llm-tool-names` 的五动词默认值未被改动。

## Verification

`emacs -Q --batch -L . -l test/run-tests.el` —— 0 unexpected，总数不低于 895。
额外：确认 `superchat-llm.el` 中已不存在对 `superchat-mcp-get-tools` 的调用
（`superchat-synthesis.el` 中的那处如保留则属预期）。

## Evidence
