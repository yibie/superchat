# 02 每服务器声明 effects，未声明者回落 unknown

status: done
blocked_by: 01
writer:
authority: `superchat-tools.el` | owned — Kernel 授权图「Tool effect declaration」。superchat-mcp.el 只是**提供**声明值，经 `superchat-register-tool` 的 `:effects` 这个既有接口交出去；它不得成为第二个裁决点。
spec_ref: `.specs/mcp-authority-integration/SPEC.md` — Decision and boundaries「Effects are declared per server」
context_ref: `KERNEL.md` — Concepts/Effect；`CONTEXT.md` — Authority
evidence_ref: E-20260827-003

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

## Verification summary (do)

JJ change: `sxxslrvn`。改动文件恰为 Scope 两项：`superchat-mcp.el`、`test/test-superchat-mcp.el`。
`superchat-register-tool` 在 `superchat-mcp.el` 中只出现于注释，未被调用——没有偷跑切片 03。

`emacs -Q --batch -L . -l test/run-tests.el` → **915 tests, 899 as expected, 0 unexpected,
16 skipped**（本切片开始前 907 / 891 / 0 / 16）。净增 8 个测试，skipped 数未变。指挥方独立复跑，
结果一致。

实现方另做了六种变异验证（去掉回落、去掉复制、把空声明当有效、kind 写错、类型里删掉工具层格位、
把 `superchat-mcp-servers` 类型改成 `(repeat symbol)`），每次失败的都恰是应当失败的用例。

**两处超出简报的判断，均已复核**：

1. 回落描述符为 `(:effect unknown :resource dynamic)`，**省略 `:scope`**，
   而 Authority 无 resolver 时构造的是 `(superchat-authority--effect 'unknown 'dynamic name)`。
   指挥方独立验证该省略不改变任何决策结果：`superchat-authority--decision-for-effect`
   （`superchat-authority.el:373-380`）只读 `:effect` 的 kind；全仓仅两处读 `:scope` ——
   `superchat-authority.el:507` 的策略判断（`developer-project` 的包含检查要求
   `:resource 'file`，`dynamic` 不触发）与 `:656` 的 Tape 哈希（仅 `process-exec`/
   `external-write` 生效）。若切片 03 希望 Tape 记录带上工具名，注册时工具名已知，
   在那里补 `:scope` 最自然。
2. 「声明存在但 effects 为空」也判为 `unknown`（简报未要求，实现方主动加严）。
   理由：空声明不等于「该服务器什么都不做」。已写成测试钉住。

## Evidence
