# 四个 harness 的横向比较：任务书

日期 2026-10-20。执行者：pi（sc-architect）。只读研究：不改产品、测试、文档，不提交，不碰 `~/.config/nova-emacs/superchat/` 下的真实数据库。产出只写到 `reports/harness-comparison-2026-10-20/`。

## 比较对象

| 系统 | 位置 | 版本 |
|---|---|---|
| Superchat | 本仓库 | HEAD `01e3d47a`（detached，jj 导出） |
| DeepSeek Harness | `~/Documents/reference-code/deepseek-harness` | `c389f96` |
| pi | `~/Documents/reference-code/pi` | `a13d35a` |
| Harness² | 本机克隆应在 commit `1cfe254`（见分析文档第 4 行），位置未知 | 先用 `mdfind -name run_sweep.py`、`mdfind -name ledger.py` 找；找不到就只用 `~/Documents/temp_convert/2026-10-19_harness2-架构与设置分析.md`，并把每个 Harness² 格子标为"二手，未对照源码" |

## 第 0 步：先核对，再比较

按 Harness² 自己的规矩：读到的和代码对照。分析文档 K 节第 1 条（改进者默认等于执行者）和第 2 条（并行度与 k 解耦）要对着源码核；核不了就写"无法核对"。核对结果写 `CORRECTIONS.md`。

## 九个维度

每格必须带 `file:line`（Superchat 用 HEAD 行号；其它用各自 commit）。没有的写"无此机制"并引用说明它没有的那处代码。

1. 执行单位与停止规则：一次执行是什么、什么让它结束、纯文字回复算不算结束、有没有目标循环和上限。
2. 工具面与发现：常驻几个、按名单还是按包、模型能否自己启用、插件工具怎么进来。
3. 硬约束与软提示：工具循环内模型跳不过的钩子有哪些；哪些约束只是提示词。
4. 失败怎么记：工具报错是失败还是成功的字符串；连续失败有没有处理；拒绝、取消、超时各记成什么。
5. 持久化与重建：什么写盘、重启后能从记录重建什么、什么只在内存。
6. 无人看守路径：定时、唤醒、子代理各依赖什么活着（窗口、buffer、进程）。
7. 审批与权限：谁来问、问在哪、有没有"全自动"档、全自动下还有什么会问。
8. 证据纪律：轨迹怎么记、截断规则、判定和任务是否分离、运行能否复现（指纹、种子）。
9. 实验与改进循环：有没有把真实轨迹变成 harness 改动的循环；成本；分母是什么。

## 产出

- `COMPARISON.md`：九张表，每维一张，四列。每表后一段"同"与"异"。
- `CORRECTIONS.md`：分析文档与源码不符处；以及你读 Superchat 时发现的与已有文档不符处（已有的 25 条在 `reports/architecture-map-2026-10-13/CONTRADICTIONS.md`，不要重复，只加新的）。
- `RECOMMENDATIONS.md`：每个维度一段：Superchat 该借什么、不借什么、理由。每条建议必须指回一个 Superchat 的事实（代码行或 `reports/` 下已有的测量）。不写没有事实支撑的建议。

## 纪律

- 每做完一个维度就写盘，再做下一个，部分产出要能独立成立。
- 区分代码事实、推论、未核对。
- 不跑 Emacs，不跑任何测试，不开真实模型。
- 做完回复一句话加三个文件路径。
