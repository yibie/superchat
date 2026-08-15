# 01 MCP 工具收集对真实 mcp.el 签名成立，且按服务器正确归属

status: ready
blocked_by:
writer:
authority: `superchat-mcp.el` | source-backed — Kernel 授权图「MCP server lifecycle」。本切片只改 superchat 如何向 mcp.el 提问，不改 mcp.el 的规则。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Problem and goal 缺陷 1/2/3；Seams and verification「Contract layer」
context_ref: `KERNEL.md` — Architecture boundaries「MCP server lifecycle」
evidence_ref:

## Goal

`superchat-mcp-get-tools` 对任何已发布的 `mcp.el` 返回一份**去重、按服务器正确归属**
的工具列表，而不是今天的 nil。

## Scope

- `superchat-mcp.el:23` — `declare-function` 改为真实的 `&key` 参数表
  （现为 `(asyncp categoryp errorHandle)`，该签名从不存在）
- `superchat-mcp.el:222-250` — `superchat-mcp-get-tools`：
  - 对整个 hub **只调用一次** `(mcp-hub-get-all-tool :categoryp t)`，
    不再按服务器循环（`mcp-hub-get-all-tool` 自己已 maphash 遍历全部已连接服务器，
    见 `../mcp.el/mcp-hub.el:97-115`）
  - 归属改用每个工具的 `:category`（值为 `"mcp-<server>"`），
    **不再**使用 `:server` —— `mcp.el` 从不设该键
  - `superchat-mcp--active-servers` 的会话白名单语义保持不变
- `test/test-superchat-mcp.el` — **重写**（现有内容调用了已不存在的
  `superchat-get-gptel-tools`，见 `test/test-superchat-mcp.el:87`）
- `test/run-tests.el` — 接入该测试文件

本切片**不**改变返回值的类型：此处仍返回 plist 列表，转成 `llm-tool` 是切片 03 的事。

## Acceptance

1. 契约层：断言活的 `mcp-hub-get-all-tool` 确实接受 `:categoryp`
   （`func-arity` / `help-function-arglist`）。`mcp.el` 缺席时该测试 skip，不是失败。
2. 行为层（stub `mcp-hub-get-all-tool`，无需真实 mcp.el）：
   - hub 函数被调用**恰好一次**，而不是每个活跃服务器一次；
   - 两台服务器暴露同名工具时，产生两个不同的 `mcp:<server>:<tool>` 名字，**无重复条目**；
   - 归属来自 `:category`；把 `:server` 键塞进 stub 数据不改变任何结果。
3. `test/test-superchat-mcp.el` 真正被 `test/run-tests.el` 加载并执行
   （不在 runner 里的测试文件不算覆盖）。

## Verification

`emacs -Q --batch -L . -l test/run-tests.el`
—— 0 unexpected；总数不低于基线 895；本切片新增测试出现在输出中且行为层未被 skip。

## Evidence
