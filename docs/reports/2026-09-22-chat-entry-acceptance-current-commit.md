# chat 入口在当前产品提交上的配置修改／撤销验收（2026-09-22）

## 结论

**chat 入口的配置修改与撤销验收，在当前产品提交 `a7879950`（change `suupkvss`）上、以 DeepSeek 和用户在场审批的方式通过；用户裁定 apply 与 undo 两个阶段都有效。** 这补上了 M5 收口时留下的"chat 入口没有在最终提交上重跑"那一项。**act 入口没有在这个提交上重跑**：它最近一次通过仍是 `9c0c3c3e` 上的 `m7-attended-act-06-deepseek`，所以这不是"当前版本双入口均已实测"。

任务与 M5 相同：给 emacs-lisp-mode 加 `C-c t h` → `describe-function`，现在就能用、重启后还在、说明改了哪个文件和为什么；随后在同一个 Session、同一个入口撤销。分工按 M7：模型改 Buffer 并让它当场生效，不保存；用户审阅 diff 并保存；独立检查器另起 Emacs 验证重启。

## 这次运行

| | |
|---|---|
| 运行 | `.scratch/m5-config-acceptance/runs/m8-attended-chat-deepseek-01`（不入库），审阅见其中 `REVIEW.md` |
| 产品 | 提交 `a7879950`；运行冻结的 57 个 `.el` 与该提交逐个相同 |
| 模型 | 请求 `deepseek-chat`，llm.el 原生 DeepSeek 提供方，`superchat-llm-reasoning` 为 `none`（关闭 thinking）；DeepSeek 在全部 16 次响应里报告的模型名是 `deepseek-flash` |
| apply | completed，51 秒；4 次 `eval_elisp` 均由用户批准，等待 13.6 秒；用户审阅后保存 `init.el`；自动检查 11/11；重启检查读到 `describe-function` |
| undo | completed，236 秒；3 次 `eval_elisp` 均由用户批准，等待 217 秒；用户审阅后保存 `init.el`；自动检查 11/11；重启检查读到 `ignore` |

模型没有自行存盘，只改了 `home/init.el`；来源 `work.el` 与其他 major mode 保持基线；读过的文件都在夹具 home 里，`eval_elisp` 参数里没有绝对路径。apply 开始时模型自行读取了参考页 `emacs-keybindings`，这是 M5 第三片按需参考机制第一次在真实任务里被模型主动使用；这只是一次观察，不证明该机制提升了成功率。

运行结束时 driver 打印了 "HARNESS CONTACT in apply"。唯一的匹配是模型自己命名的结果键 `:abc-control-binding`，它取自夹具对照 buffer ` *abc-existing-control*` 的名字，而该 buffer 在扫描器自己的无害名单上；工具结果侧没有任何匹配，undo 阶段干净。按误报处理，扫描器问题另行登记（`.scratch/m5-config-acceptance/issues/05-…`），未修改。

## 答复审阅与用户裁定

apply 答复说明了改的是 `user-init-file` 指向的 `init.el` 以及为什么，说明 Buffer 未保存、保存归用户、只求值了改动的那一个 form、已在来源、同 mode 对照 buffer 和两个其他 mode 里查过绑定、没有重启。这些与观察一致。

**保留在案的瑕疵：**

- undo 答复有两句关于磁盘的话是错的："回到了磁盘文件的原文"／"buffer 与磁盘上的 `#'ignore` 版本一致"，以及"磁盘副本已经是 `#'ignore`"。实际上用户在 apply 之后已经保存，undo 时磁盘上是 `#'describe-function`（undo 的保存审阅 diff 显示了这一点）。模型在这一轮里自己用 `read_file` 读到过 `#'describe-function`，仍沿用了它在 apply 答复里说过的、当时正确的话；产品发给它的内容里没有错误信息。若照这句话理解，用户可能不保存，重启后会回到 `describe-function`。同一份答复也写明了 Buffer 未保存、保存由用户决定、保存后重启才会恢复。
- 两份答复都把 `(lookup-key global-map (kbd "C-c t h"))` 的返回值 `2` 说成"一个前缀"。`C-c t` 在 global-map 里没有绑定，`2` 表示这个序列在 global-map 里没有绑定。

按 `.scratch/m5-config-acceptance/ATTENDED-T2-PLAN.md` 的答复规则，undo 答复原本判为不通过。用户裁定（2026-09-22）："不用，可以明确 undo 也是有效的。让我们直接验收。" 据此本轮通过。按用户对 M5 收口的说明（2026-09-19），接受一份有瑕疵的答复不构成"末尾声明可以抵消任何错误"的规则；上面的瑕疵照旧记录，不写成"答复所有陈述均准确"。

## 保证什么，不保证什么

**保证**：在上表的产品提交、模型配置与人工流程下，chat 入口有一次完整通过（经用户裁定），证据齐全。

**不保证**：

- act 入口在这个提交上的结果。收口之后 act 入口在同一提交上跑了一次（`m8-attended-act-deepseek-01`），**没有通过**：apply 以一句"我再确认一下……"却不发工具调用的回复结束（验收问题 03 第三次出现），undo 因为拿不到上一轮观察到的原绑定 `ignore`，把键恢复成了未绑定。详见该运行 `REVIEW.md`。
- 成功率。之前同一标签序列里还有一次 qwen 运行 `m8-attended-chat-01`，因命令未指定 DeepSeek 而用了 LM Studio 的 `qwen/qwen3.8-27b`，在 apply 用满 1200 秒实验上限后被取消，未完成、不计入。这是运行历史，不是成功率估计。
- 其他模型、长期稳定性，以及模型在用户保存之后能否始终以新观察更正自己先前的说法。
