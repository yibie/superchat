# 03 MCP 工具经 superchat-register-tool 上下线，走与插件工具相同的执行路径

status: done
blocked_by: 01, 02
writer:
authority: `superchat-tools.el` | owned — Kernel 授权图「Model-facing tool surface」。本切片把 MCP 的工具暴露从 superchat-llm.el 的旁路搬回这个权威模块。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — AC「Register one MCP server's tools」/「Unregister one MCP server's tools」；Unchanged contracts 1-3
context_ref: `KERNEL.md` — Concepts/Tool；工具执行生命周期条目；「A synthesized tool may not take a name already on the live tool surface」（K1 时名为「…may not shadow a built-in or MCP tool name」，K2 重命名）
evidence_ref: E-20260827-003

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

## Verification summary (do)

JJ change: `sxxslrvn`。改动文件为 Scope 四项：`superchat-mcp.el`、`superchat-llm.el`、
`superchat-synthesis.el`、`test/test-superchat-mcp.el`。

`emacs -Q --batch -L . -l test/run-tests.el` → **935 tests, 919 as expected, 0 unexpected,
16 skipped**（本切片开始前 915 / 899 / 0 / 16）。指挥方独立复跑，结果一致。

**实现形态（指挥方逐项在代码中确认，非依赖测试）**：
`superchat-mcp.el` 中不存在任何 `llm-make-tool` 或 `superchat--agent-wrap-*` 调用 ——
MCP 工具确实经 `superchat-register-tool` 走插件工具那条路径，没有第二套包装。
`superchat-llm.el` 中对 `superchat-mcp-get-tools` 的调用已删除。
`superchat--syn-existing-names` 改为读注册表（`superchat-mcp-registered-tool-names`），
不再对 plist 调 `llm-tool-name`。
`superchat-mcp-unregister-server-tools` 经 `superchat-unregister-tool` 连带清理 effect
resolver 与 approval preview，只作用于本服务器前缀；`superchat-mcp-stop-server` 在触碰连接
**之前**先注销。

**第一轮对抗性复核判定 MUST_FIX**（11 种变异抓住 7 种，实现攻不破，但测试未钉住本切片的标题
行为）。三处缺口：停止路径、连接路径、防重名断言假通过（MCP 工具已是注册插件工具，
`superchat-get-llm-tools` 那一半即可满足断言）。另发现一处可移植性陷阱：
`test-superchat-mcp--make-tool` 在原生实现存在时返回真 `llm-tool` 结构，而
`test-superchat-mcp--tool-slot` 只接受 mock cons，在 load-path 含 llm.el 的机器上会失败。

**返工后指挥方独立变异复验，四条全部被抓住**（在真树上逐条施加、逐条还原、sha 校验）：

| 变异 | 结果 |
| --- | --- |
| 删 `superchat-mcp-stop-server` 中的注销调用 | 2 unexpected |
| 删 `superchat-mcp-start-servers` 回调中的 `register-all-tools` | 1 unexpected |
| 删 `superchat-mcp-start-server` 已运行分支中的注册 | 1 unexpected |
| 防重名读取换成 `nil` | `FAILED mcp/synthesis-shadow-guard-sees-mcp-names-without-signalling`（`test/test-superchat-mcp.el:972`） |

还原后基线复跑 935 / 919 / 0 unexpected / 16 skipped，与变异前一致。

**本切片解除了切片 01 记录的交付约束**：`superchat--syn-existing-names` 与请求工具面均已不再
接触 plist，`define_tool` 的崩溃路径关闭。01 与 03 现同处 change `sxxslrvn`。

留给切片 04 的产品发现（复核方提出，非本切片 Scope）：`superchat.el:1304`
`superchat--detect-response-mode` 与 `superchat.el:1865` `superchat-backend-show` 仍在
append 原始 `superchat-mcp-get-tools` plist；两处均只取 `length` 不会崩，但 MCP 工具现在同时
存在于 `superchat-get-llm-tools` 中，`/tools` 会重复计数。

非阻塞观察（交 `learn` 判断）：hub 瞬时故障会使 `superchat-mcp-get-tools` 返回 nil，
`superchat-mcp-register-server-tools` 随即把该服务器所有已注册名字视为陈旧并注销，
而用户看到的消息仍是「已在运行」，与实际发生的事不符。

## Evidence
