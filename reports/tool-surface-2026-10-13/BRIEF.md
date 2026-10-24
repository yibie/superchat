# 实验任务书：精简的工具面，插件注册的工具，模型自己找工具

日期 2026-10-13。起因：用户，"我希望测试出，最近精简的工具组合（但工具可以被插件注册，可以渐进式发现）"。
前一次实验（`docs/reports/2026-10-05-tool-count-on-a-local-model.md`）在本地 Qwen 上量了 5 种清单，结论是工具数量不影响通过率、瓶颈在写 Lisp。它没量的三件事是本实验的问题：

1. 在用户实际用的 `deepseek-v4-flash` 上，精简到 4 个核心工具，通过率和调用次数怎么变。
2. 模型**缺**一个工具时，能不能自己找到它（10-05 的 P3T 只在带 `timer_create` 的清单上跑过）。
3. 行（row）拥有的工具和插件注册的工具，在"精简"面上实际怎么出现。

## 三种工具面（以请求里实际带的为准，不以配置为准）

| 面 | 配置 | 接线检查里实际到达模型的 | 字节 |
|---|---|---|---|
| D13 | 默认 | 13 个默认 + ask_user | 11 757 |
| L4 | `read_buffer,edit_buffer,eval_elisp,open_file` | 4 个 + work_record（行拥有，配置滤不掉）+ ask_user | 7 131 |
| L4+find | L4 + 原型 `ts-discovery.el` | L4 的 6 个 + enable_tools；系统文本多一段 `<dormant_tools>` | 7 569 |

`define_tool` 不在任何一面，免得它成为逃生口。`enable_tools` 只在 chat 面注册，子代理拿不到。

## 原型（只在测试台，不是产品）

`.scratch/task-sim/harness-tools/ts-discovery.el`，照 `define_tool` 的热插机制写：
- 系统文本里列出休眠工具（默认面的内置工具减去核心，减去行已经提供的 work_record），每个一句话；行拥有的其它工具（子代理、workspace、memory）不列，否则精简面能够到的比默认面还多，那是另一个产品问题；
- 插件工具 `enable_tools(names)` 把点名的休眠工具插进正在跑的请求，下一步可调。
它绕过了 select-tools 契约里"显式限制优先、登记表不得悄悄扩大基线"这条；产品版要先决定模型申请工具时权限边界放在哪，和自造工具的裁定同形。

## 任务（7 个，测试台现成，脚本判定，不看答复文字）

核心能做的：T1 日记汇总、K2C org 属性、A2 改后缀、C1 修 elisp。
核心做不了的：P3T 提醒、T4W 工作记录+定时，都要 `timer_create`。**发现这一维只靠这一个休眠工具**。
对照：DLG 派子代理。它自己的 `:configure` 把 `delegate_to_subagent` 追加进工具名单，三种面都拿到，所以它量的是"精简核心 + 任务自带的行工具"，不是发现。

每格 3 次，共 63 次，串行。3 次只能看出大的差别（约 2/3 对 0/3），同 10-05 的诚实度。

## 每次运行记什么（全部来自现有证据文件）

通过、请求数、工具调用数、失败调用数、调了没提供的工具数、第 1 次请求的工具数组与字节、秒数；L4+find 另记 enable_tools 调了没有、点了哪些名、之后过没过。

## 条件与局限

- 真实 DeepSeek V4-flash，思考关闭，权限 `danger-full-access`，所以**审批次数没有量**（用户上周 75 次审批是另一条线）。
- 上周 Tape 里的 run_shell 44、read_buffer 30 来自用户当时的 5 名配置，不是默认面的产物。
- 代码：HEAD（每次运行的 `evidence/code-commit.txt`）。运行目录在 `.scratch/task-sim/runs/`，汇总在本目录 `evidence/`。
