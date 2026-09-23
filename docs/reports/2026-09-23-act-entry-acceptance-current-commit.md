# act 入口在当前产品提交上的配置修改／撤销验收（2026-09-23）

## 结论

**act 入口的配置修改与撤销验收，在产品提交 `afba469b`（change `kpypmork`）上、以 DeepSeek 和用户在场审批的方式通过：两个阶段各 11 项自动检查全过，两份最终答复经 leader 审阅没有与观察相矛盾的状态陈述。** 这是 act 入口自 `9c0c3c3e` 以来第一次在新提交上通过。**chat 入口没有在这个提交上重跑**：它最近一次通过是 `a7879950`（2026-09-22，用户裁定其 undo 有效）。所以这仍然不是"同一版本双入口均已实测"。

## 运行

| | |
|---|---|
| 运行 | `.scratch/m5-config-acceptance/runs/m8-attended-act-deepseek-03`（不入库），审阅见其中 `REVIEW.md` |
| 产品 | 提交 `afba469b`；冻结的 57 个 `.el` 与该提交逐个相同 |
| 模型 | 请求 `deepseek-chat`，关闭 thinking；响应报告的模型名为 `deepseek-flash` |
| apply | completed，65.9 秒；4 次 `eval_elisp` 由用户批准；用户审阅后保存；11/11；重启读到 `describe-function` |
| undo | completed，42.1 秒；3 次 `eval_elisp` 由用户批准；用户审阅后保存；11/11；重启读到 `ignore` |

## 这次与上次的不同

同一入口、同一模型的上一次完整运行 `m8-attended-act-deepseek-01` 没有通过：apply 以半句话结束，undo 看不到上一轮观察到的原绑定，把键改成了未绑定。之后产品做了三件事：下一轮能看到之前的工具观察（`2026-09-23-the-next-turn-sees-previous-tool-results`）、半句话结束时提醒一次（`2026-09-23-one-finish-check-after-a-text-only-reply`）、网络层偶发错误先重试（`2026-09-23-retry-transient-provider-errors`，因 `m8-attended-act-deepseek-02` 在 apply 正确完成后撞上网络超时而做）。

这次 undo 请求的历史里带着 apply 的 11 行工具观察，其中包括"把 `#'ignore` 改成 `#'describe-function`"那一次编辑；undo 答复把撤销后的状态与"改动前的观测值"逐个对照。这是工具观察在真实任务里起作用的一次观察，不是它提高成功率的证明。收尾检查和网络重试在这次运行里都没有被触发。

## 保留在案的瑕疵

apply 答复末段"will now shadow nothing"表述不清；undo 答复有一处错字"改动作之后"。都不涉及已生效／已保存／已撤销的陈述。

## 不保证

- chat 入口在这个提交上的结果（没有运行）。
- 成功率。act 入口在新提交上的三次运行里：`-01` 未通过（半句话与失忆），`-02` 因网络超时暂停，`-03` 通过。这是运行历史，三次之间产品不同，不能据此估计成功率。
- 其他模型、长期稳定性；HTTP 429／5xx 的重试（llm 未传出状态码，已登记）。
