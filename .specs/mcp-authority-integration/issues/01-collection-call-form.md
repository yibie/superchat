# 01 MCP 工具收集对真实 mcp.el 签名成立，且按服务器正确归属

status: done
blocked_by:
writer:
authority: `superchat-mcp.el` | source-backed — Kernel 授权图「MCP server lifecycle」。本切片只改 superchat 如何向 mcp.el 提问，不改 mcp.el 的规则。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Problem and goal 缺陷 1/2/3；Seams and verification「Contract layer」
context_ref: `KERNEL.md` — Architecture boundaries「MCP server lifecycle」
evidence_ref: E-20260827-003

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

## Verification summary (do)

JJ change: `sxxslrvn` "mcp: repair the tool surface and route it through Authority".
改动文件恰为 Scope 三项：`superchat-mcp.el`、`test/test-superchat-mcp.el`、`test/run-tests.el`。

`emacs -Q --batch -L . -l test/run-tests.el` → **907 tests, 891 as expected,
0 unexpected, 16 skipped**（基线 895 / 879 / 0 / 16）。净增 12 个测试，skipped 数未变，
即新增测试无一被 skip。指挥方独立复跑，结果一致。

**对简报的一处偏离，已独立验证成立**：`declare-function` 未写成
`(&key asyncp categoryp)`，而是 `(&rest keys)` 加注释记录真实签名。理由是
`declare-function` 中的 `&key` 会被字节编译器当作第三个必填位置参数，使唯一的关键字
调用点报 `called with 2 arguments, but requires 3`。以最小样例独立字节编译复现该警告，
理由成立，采纳。

**对抗性复核发现的中间态风险（阻塞交付，不阻塞本切片验收）**：
本切片使 `superchat-mcp-get-tools` 从"永远返回 nil"变为返回真实 plist 列表。两个消费端
随即会对 plist 调用 `llm-tool-name` 并抛出未捕获的 `wrong-type-argument llm-tool`：

- `superchat-synthesis.el:93-96` —— `ignore-errors` 只包住收集调用，外层
  `(mapcar #'llm-tool-name ...)` 裸露；后果是只要有 MCP 服务器连着，`define_tool` 整体失效；
- `superchat-agent-loop.el:427` —— 经 `superchat-llm.el:1289` 的 `(mapcar wrap-fn tools)`
  到达，是每一个带工具请求的必经路径。

复核方以"忠实假 hub"在真实仓库上做了 before/after 对照，两条路径均复现。
现有测试套件抓不到它，因为 `test/test-llm-backend.el:203/545/2987` 把
`superchat-mcp-get-tools` 整个 stub 成 nil。

**处置**：不在本切片加临时闸——闸只能加在消费端（属切片 03 Scope），加在收集端会推翻
本切片自身的验收。改为约束交付单元：**切片 01 不得脱离切片 03 单独交付**，两者必须落在
同一个 change 里。

非阻塞的后续建议（交 `learn` 判断是否提升）：`declare-function` 的正确性目前只由字节编译
警告守着，而仓库没有编译门禁；若要它不再腐烂，需在测试流程中加一次 `batch-byte-compile`
并把警告当作失败。

## Evidence
