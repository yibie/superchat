# M10 第 1b 片：编辑模式与 agent 模式的有人值守验收（2026-09-25）

## 结论

**编辑模式与 agent 模式各有一次通过：chat 入口在编辑模式（对话在 `work.el` 右边的侧窗口）、act 入口在 agent 模式，都在产品提交 `68098701` 上、以 DeepSeek 和用户在场审批的方式通过配置修改／撤销验收；四个阶段各 11 项自动检查全过，四份最终答复经 leader 审阅，没有与观察相矛盾的"已生效／已保存／已撤销"陈述。审阅用的是产品自己的审阅屏（第 1a 片），不再是验收脚本自带的那块。** 两种模式都是第二次才通过；每种只有一次通过，不是成功率。`68098701` 的代码与 `c356b84b`（第 1b 片）相同，其后的提交只改了路线图。

## 用户裁定

2026-09-25，用户选 "A"：按"两种模式各一次通过（都在第二次）"接受第 1b 片的验收；下一步先处理"回复里编造工具记录"那一项登记问题，再做第 1c 片。

## 运行

| | 第 1 轮：chat 入口、编辑模式（`m10-1b-attended-chat-editor-02`） | 第 2 轮：act 入口、agent 模式（`m10-1b-attended-act-agent-02`） |
|---|---|---|
| 产品 | `68098701` | 同左（`--same-product-as` 核对：代码、依赖、Emacs、ABC harness 全同） |
| 验收脚本 | `run-config.el` `9144fd59bd` | `8fb04b3a6a`：只多一处，等审阅屏时用 `accept-process-output`，不用 `sit-for`（见下） |
| 模型 | 请求 `deepseek-chat`，关闭 thinking | 同左；响应报告 `deepseek-flash` |
| apply | completed，67.1 秒（24.5 秒在等用户）；9 次请求、12 次工具调用；5 次 `eval_elisp` 由用户批准；11/11；重启读到 `describe-function` | completed，235.2 秒（98.3 秒在等用户）；16 次请求（两次网络失败后自动重试）、16 次工具调用；6 次批准，到达 12 轮上限时用户同意加一轮；11/11；重启读到 `describe-function` |
| undo | completed，40.0 秒；7 次请求、9 次工具调用；4 次批准；11/11；重启读到 `ignore` | completed，188.4 秒；5 次请求、5 次工具调用；3 次批准；11/11；重启读到 `ignore` |
| 审阅屏 | apply：脚本等了 3 秒没见到，自己按 `/changes` 的方式打开（原因见下）；undo：自己出现、未被选中；两阶段都经它保存 | 两阶段都自己出现、未被选中，经它保存 |

两轮 `work.el` 始终是基线；没有任何表达式执行命令；两轮都没有接触 harness 内部。审阅记录在各运行目录的 `REVIEW.md`（`.scratch`，不入库）。

## 两种模式第一次为什么没通过

- **第 1 轮第一次（`m10-1b-attended-chat-editor-01`）**：审阅时一个 `s` 落进了 `work.el`（`source_untouched` 为假，其余 10 项都过）。当时脚本给的提示是"按 `C-x o` 切过去"；llm 每次请求都弹一个 `*Warnings*` 窗口，屏幕上是三个窗口，从对话按一次 `C-x o` 先到 `work.el`。用户确认当时按了一次 `C-x o` 就按了 `s`。之后提示改为按入口给出可靠的进入方式（chat 入口：`M-x superchat-changes`，从对话里直接选中已打开的审阅屏；act 入口：`C-x o` 直到光标进了它、看清再按）。
- **第 2 轮第一次（`m10-1b-attended-act-agent-01`）**：apply 通过；undo 里模型只做了两次读取，第二次回复是纯文字，开头是七条从未执行过的"工具记录"（`# Tool: edit_buffer(…) -> [ok] …` 之类），接着说"已经撤销了"。产品把没有工具调用的回复当作最终答案，结束检查只在回复为空或最后一句宣布下一步时才触发，所以没有拦住；实际什么都没撤销。这一项另行登记，不属于第 1b 片（同一轮里编辑模式、agent 模式与审阅屏都正常）。

## 审阅屏

四个通过的阶段里，产品的审阅屏三次自己出现、未被选中；第四次（第 1 轮 apply）是用户在屏出现前先敲了 `M-x`（用户确认），而脚本当时用 `sit-for` 等屏——有按键待处理时它立即返回、不跑定时器，产品正是从定时器打开审阅屏的——所以脚本 3 秒后自己打开了它（已量过：待处理一个键时，`sit-for` 5 秒内屏不出现，`accept-process-output` 0.06 秒出现且按键保留）。第 2 轮起脚本改用 `accept-process-output`。第 1 轮第一次的两个阶段里，审阅屏也都是自己出现的。

## 另外看到的

- **网络故障自动重试第一次在真实环境里生效**：第 2 轮 apply 的第 7、第 15 次请求都遇到 `curl error code 35: SSL connect error`，产品各等 30.0 秒后重发，都成功（2026-09-23 的决定：最多 10 次、间隔 30 秒）。这是连接失败，不是限流；限流下的行为仍未在真实环境里见过。
- 模型两次调用失败后自行改正（`read_buffer` 读一个 keymap 名；`read_file` 读不存在的 `custom.el`），产品的失败提示照常起作用。

## 保留在案的瑕疵

1. 第 1 轮 apply 说改动在"第七行所在的那个块内"；那个块在 7 行文件的第 3–4 行（undo 说对了：第 4 行）。
2. 第 1 轮两份答复与第 2 轮 apply 的结尾，又把 `lookup-key` 返回的数字读成"已有前缀绑定"，并把 `work.el` 的有效绑定 `(keymap (104 . ignore))`（来自 `emacs-lisp-mode-map`）算到 `global-map` 头上；第 2 轮 apply 还说 text-mode"也只有 `h`"。模型收到的请求里有两句互相矛盾的话：系统里的操作说明（2026-09-24 已改对："数字表示那张表里没有这个键"）与它先读的 `emacs-keybindings` 参考页（仍是旧说法："数字表示前几个键已被绑定"）；它的读法跟的是参考页。只涉及范围的推理，不涉及改动、保存或撤销的状态。
3. 第 2 轮 apply 说 `emacs-lisp-mode-map`"经由 `prog-mode-map` 到达"（实为继承）；第 2 轮 undo 用对话缓冲区里的 `C-c C-c` 作为"其他绑定未受影响"的抽查，属实但不相干。

## 另行登记的问题（都不属于第 1b 片）

- `.scratch/issues/2026-09-25-llm-nonfree-warning-each-request.md`：llm 对每次请求都发"not free software"警告，弹出 `*Warnings*` 窗口；用户配置没有关它，验收保持默认。
- `.scratch/issues/2026-09-25-superchat-changes-outside-a-chat-view.md`：在代码 buffer 里 `M-x superchat-changes` 报类型错误。
- `.scratch/issues/2026-09-25-reference-page-misstates-lookup-key-number.md`：参考页仍是 `lookup-key` 数字的旧错误说法，与操作说明矛盾。
- `.scratch/issues/2026-09-25-a-reply-with-invented-tool-records-ends-the-run.md`：由编造工具记录组成的纯文字回复被当作最终答案；附不用真实模型的复现脚本。

## 没有覆盖的

- 图形界面（GUI）的 Emacs、主题与颜色：两轮都在用户的终端里跑 `emacs -nw`。
- chat 入口在 agent 模式、act 入口在编辑模式这两种组合没有真人运行（用户选了两轮方案）。
- 在别处打字时审阅屏自己出现"误存盘"的风险由第 1a 片的回归测试覆盖；这次运行里没有出现。
