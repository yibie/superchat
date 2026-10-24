# Harness 横向比较 — 第 0 步核对与更正

日期 2026-10-20。执行者：pi（sc-architect）。只读研究。

## 第 0 步：先核对，再比较

### Harness² 源码定位

BRIEF 说 Harness² 位置未知、期望 commit `1cfe254`，找不到就用二手分析文档。实际核对结果：

- **代码事实**：源码找到了，在 `/private/tmp/harness2`，`git rev-parse HEAD` = `1cfe254e8d25b15f37048c3730134686abe60335`，与分析文档第 4 行声明的 commit `1cfe254` 一致。
- **代码事实**：`harness2/core/`、`harness2/config.py`、`harness2/core/spec.py`、`harness2/cli/run_sweep.py`、`harness2/substrate/{codex,opencode}.py`、`harness2/benchmarks/` 均在，可直接引 `file:line`。
- 因此 COMPARISON.md 中 Harness² 列**全部改为一手、对照源码**，不再标"二手"。

### K 节第 1 条：`IMPROVER_MODEL` 默认等于 `SOLVER_MODEL_OC`

分析文档（K 节第 1 条，`~/Documents/temp_convert/2026-10-19_harness2-架构与设置分析.md:458-461`）原文：

> `IMPROVER_MODEL = env("HARNESS2_IMPROVER_MODEL", SOLVER_MODEL_OC)`

- **代码事实（已核对）**：`/private/tmp/harness2/harness2/config.py:101` 为
  `IMPROVER_MODEL = env("HARNESS2_IMPROVER_MODEL", SOLVER_MODEL_OC)`，与文档逐字一致。
- **结论**：**确认，无更正**。同一模型默认既当 solver 又当 improver，归因被污染的批评成立（这是分析文档的解读，不是代码不符）。

### K 节第 2 条：`PHASE_WIDTH` 与 `k` 解耦

分析文档（K 节第 2 条，`:464-466`）原文：

> 并行度是常数 2，不是 `min(k, N)`。k=5 的 sequential 是 15 个阶段串行。

- **代码事实（已核对）**：`config.py:176` 为 `PHASE_WIDTH = 2`（硬编码常量，不随 k 变）。
- **代码事实（已核对）**：`harness2/core/recursion.py:354-355` 的 `run_parallel(..., propose_workers: int = PHASE_WIDTH, rollout_workers: int = PHASE_WIDTH)` 和 `:464` 的 `run_sequential(..., rollout_workers: int = PHASE_WIDTH)` 都用常数 2 作默认并行度。
- **代码事实（已核对）**：`recursion.py:321` `n = max(1, min(len(items), width))` 说明单批实际宽度是 `min(剩余项数, 2)`，上限就是 2，与 k 无关。
- **结论**：**确认，无更正**。"k 影响阶段数、不影响并行度"的机制描述与源码一致；"k=5 sequential 15 阶段串行"是推论（k 次嵌套 pass，每次 k 个阶段，见 `recursion.py:452-478` 的 `run_sequential` 循环 `for t in range(1, k + 1)`）。

### 其余 K 条目（K3–K8）的抽查

第 0 步只要求核 K1、K2。以下是我顺手对照源码、供参考的抽查（不是穷尽复核）：

- K 条目的"账本只记方法调用，不记选择/反思"与 `ledger.py:8-10`（"selection and reflection are recorded outside this ledger"）一致。
- "证据规模：文件化 + 硬下限 obs_trunc"与 `context.py:105-129`（evidence 写成文件，`obs_trunc` 逐 observation 生效，trajectory 整体写全）一致；`config.py:156` `JB_OBS_TRUNC = 50000` 一致。
- "命名/指纹"与 `runmark.py:32-47`（`fingerprint` 对 harness 树做 sha1，`hexdigest()[:16]`）一致。
- 其余 K3–K8 未逐条穷尽核对，不在此声明已核。

## Superchat 侧新增不符处

BRIEF 要求只记录**新**的、不在 `reports/architecture-map-2026-10-13/CONTRADICTIONS.md`（C1–C25）里的不符处。

- **无新增**。本次横向比较通读 Superchat 的权限/工具/持久化/审批/证据相关源码（`superchat-sandbox.el`、`superchat-approval.el`、`superchat-tools.el`、`superchat-tape.el`、`superchat-runtime.el`），未发现超出 C1–C25 的新文档-代码矛盾。C22（默认工具清单滞后）在本次又确认了一次，但已在 25 条内，不重复记录。

## leader 抽查（2026-10-20，Claude 会话 fdfd47ce）

抽查了 14 处引用：DSH 5 处（`agent.ts:257,467-470`，`tools/src/index.ts:1081-1114`，`user-approval/src/index.ts:52-57`）、pi 4 处（`agent-loop.ts:263-270,322-330,727-747`，`truncate.ts:11-12`）、Harness² 5 处（`ledger.py`、`context.py:454`、`runmark.py:32-47`、`config.py:101,176`、`judge.py:15-19`），全部对得上。Superchat 侧 5 处（`superchat-tape.el:25,40`、`superchat-sandbox.el:47-66,266`、`superchat-llm.el:2128`）对得上。

两处 Superchat 格子漏了 HEAD 里切片 9 的事实，COMPARISON.md 未改，读者按此修正：

- §1、§4"工具轮预算默认 nil、默认不限"：HEAD 另有每次执行 50 次工具调用的上限 `superchat-agent-max-tool-calls`（`superchat-agent-loop.el:55`），撞上限写事实、待办表显示被切断（切片 9b，`01e3d47a`）。RECOMMENDATIONS §1 的前提"可能继续烧轮次"要打折：轮次有上限，只是上限大。
- §4"工具循环内没有连续失败处理"：HEAD 有连续失败计数和第 3 次失败时的软提醒（切片 9c；`superchat-tool-execution.el:433`，`superchat-llm.el:3493`）。RECOMMENDATIONS §4 建议的"停止/降级"正是切片 11c 要把它改硬的内容。

RECOMMENDATIONS §1 "token 截断时这批工具调用一律失败不执行"（pi `agent-loop.ts:263-270`）是本次比较里唯一一条此前没人提过的新建议，可作为切片 11b 钩子的第二个使用者，待用户裁定。

## 版本确认

| 系统 | 位置 | HEAD |
|---|---|---|
| Superchat | 本仓库 | `01e3d47af77950ebf6c532f03f2757383f5d87e6` |
| DeepSeek Harness | `~/Documents/reference-code/deepseek-harness` | `c389f96bf3a9b6807cb71ed6bdad5849be0df6d8` |
| pi | `~/Documents/reference-code/pi` | `a13d35a742c6ef8462812a28fbe1d8c8b7431c32` |
| Harness² | `/private/tmp/harness2` | `1cfe254e8d25b15f37048c3730134686abe60335` |
