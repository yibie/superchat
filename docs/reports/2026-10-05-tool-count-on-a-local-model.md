# 实验报告：给本地模型的工具减到多少个

日期：2026-10-04 至 05。起因：用户，2026-10-04："我希望这些设计可以针对本地模型做优化，比如就连 Qwen 3.8 这么弱智的模型都能运行得很好"；随后："做一个实验，看可以将工具调用简化到多少个，本地模型也能跑得顺畅，调用准确"。实验由 worker agent 执行，leader 设计并复核。产品代码没有改动。

## 一句话结论

在本机的 `qwen/qwen3.8-27b` 上，把工具从 14 个减到 2 个，通过率没有下降；拖累它的是每次请求慢、Emacs Lisp 写不对、摆窗口这类任务本身难。每格只有 2 次运行，只能看出大的差别。

## 怎么做的

- 模型：LM Studio 里的 `qwen/qwen3.8-27b`，本机 `http://127.0.0.1:1234/v1`，非流式。
- 代码：提交 `92fd4c6b` 之后的工作副本（M11 第 1–9 片）。
- 环境：沙箱里的终端 Emacs；权限 `danger-full-access`；审阅屏关闭；每次运行的总时限 900 秒（第一轮）、1500 秒（第二轮）。
- 任务（`.scratch/task-sim/harness-goals/`）：A1 列出 7 天内的日程；A2 把目录里的 `.JPG` 改成 `.jpg`；T6 把窗口排成左右两栏；P3T 20 分钟后提醒（只在有 `timer_create` 的清单里跑）。判定由脚本检查最终状态，不看答复文字。
- 五种工具清单，每格 2 次。`ask_user` 在每种清单里都会自动加上，下表的个数已含它：

| 名字 | 个数 | 工具 |
|---|---|---|
| t14 | 14 | 默认清单 |
| t8 | 9 | read_buffer, read_file, open_file, edit_buffer, eval_elisp, describe-function, timer_create, work_record |
| t5 | 6 | read_buffer, open_file, edit_buffer, eval_elisp, timer_create |
| t3 | 4 | read_buffer, edit_buffer, eval_elisp |
| t1 | 2 | eval_elisp |

- 测试台的改动（都在 `.scratch/task-sim/harness-goals/`，不在版本库里）：`launch.sh` 的 `local` 模式；`TS_TOOLS`（指定工具清单）；`TS_RESPONSE_TIMEOUT`；`batch.sh` 的 `TS_MODE`；`toolcount.sh`、`toolcount-report.py`。

## 第一轮：量到的是超时，不是工具数量

产品默认 `superchat-response-timeout` 为 120 秒，带工具的请求约 180 秒没有进展就放弃。36 次运行里 20 次失败，全部是 `[Response timeout: no progress for N s; … last phase: waiting-first-token]`，其中 15 次一次请求都没完成。通过：t14 4/8，t8 4/8，t5 3/8，t3 2/6，t1 3/6。这一轮各清单之间的差别不能用。

## 第二轮：无进展超时放宽到 900 秒

| 清单 | 通过 | A1 秒 | A2 秒 | T6 秒（通过） | P3T 秒 | 工具说明字节 |
|---|---|---|---|---|---|---|
| t14 | 6/8 | 171 | 259 | 1201（0/2） | 33 | 11479 |
| t8 | 7/8 | 459 | 779 | 575（1/2） | 27 | 9594 |
| t5 | 7/8 | 361 | 382 | 1027（1/2） | 25 | 5556 |
| t3 | 4/6 | 158 | 1296（1/2） | 1500（1/2） | — | 3457 |
| t1 | 6/6 | 272 | 1038 | 1500（2/2） | — | 1476 |

秒数是一次运行的中位数；没标通过数的格子都是 2/2。每次请求的耗时中位数在 11 到 108 秒之间；每个任务 2 到 11 次请求。

事实：

- 72 次运行（两轮）里，没有一次调用了没提供的工具；工具调用的格式都能被解析。
- P3T 在有 `timer_create` 的三种清单里 6/6（第二轮），每次 2 个请求、25–33 秒。
- 失败的工具调用几乎都在 `eval_elisp` 里：调用不存在的函数（`window-side`、`get-buffer-windows`、`remove-if`、`window-inhibit-siding-p` 等）、括号没配对（`End of file during parsing`）、把变量当函数。T6 里约一半的调用失败（t14 11/21，t8 11/20，t1 13/21）。
- 第二轮 6 次失败：5 次是 T6，1 次是 t3 的 A2。其中 3 次运行状态是 completed 而答复为 `[Empty response from llm]`，3 次跑满 1500 秒被取消。
- 只有 `eval_elisp` 时（t1），T6 两次都通过，但都跑到了 1500 秒的时限。

## 推论（没有验证）

- 对这个模型，瓶颈不在选工具，而在写 Emacs Lisp。参数少、不用写代码的专用工具（`timer_create`）最快最稳。
- 所以方向更像是"让它少写代码"而不是"把工具减到最少"：给常见的事一个专用工具，`eval_elisp` 作兜底。这还只是推论，要做一个专用工具（例如摆窗口）在同样条件下重测 T6 才知道。
- 工具说明的字节数减到八分之一，单次请求并没有随之变快；本地是否复用了请求前缀没有查。

## 发现的产品问题（没有改）

1. 默认的无进展超时（120 秒，带工具约 180 秒）对本地模型太短。
2. 模型返回空答复时，执行以 `[Empty response from llm]` 结束，状态是 completed，没有重试，也没有提示。
3. 用户自己的配置（`~/.config/nova-emacs/modules/nova-ai.el`）把 `superchat-llm-tool-names` 设成了 `list_buffers`、`read_buffer`、`render_ui`、`define_tool` 四个，不是产品的默认清单；本实验量的是产品默认面及其子集。

## 没有做的

- 每格只有 2 次；没有加跑。
- 只测了一个本地模型；没有在 DeepSeek 上跑同样的五种清单。
- 系统文字没有动（另一组对照：3228 个字符对精简版）。
- 默认权限 `edit-buffers` 下的体验没有量。

## 证据在哪

- 两轮的运行目录：`.scratch/task-sim/runs/batch-toolcount-local-t*/` 与 `batch-toolcount2-local-t*/`（每次运行有请求与回复原文、工具调用、判定结果）。
- worker 的两份报告：`.scratch/m11/toolcount-report.md`、`.scratch/m11/toolcount2-report.md`（含每个失败运行的事实）。
- 之前的精简建议：`.scratch/m11/simplification-review.md`。
