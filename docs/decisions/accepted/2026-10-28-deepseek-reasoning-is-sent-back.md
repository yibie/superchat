# Decision: DeepSeek 的推理随请求传回

Status: accepted 2026-10-28, implemented — 用户在 ARC 对比测试卡住时选了"先修产品再跑"。

## Problem

DeepSeek 的推理模式（thinking）下，最后一条用户消息之后的助手消息必须带 `reasoning_content`，否则请求返回 400："The `reasoning_content` in the thinking mode must be passed back to the API"。llm.el 0.32.1 的 `llm-deepseek` 能读出回复里的推理（`llm-provider-extract-reasoning`），但组装下一次请求时从不送回：`llm-openai--build-messages` 只送 OpenAI Responses 的推理记录（`:openai-reasoning-id`）。

`deepseek-v4-flash`（DeepSeek 的说明写它由 DeepSeek-V4.1-Flash-1017 提供）在请求不带 `thinking` 参数时默认推理。superchat 的 `superchat-llm-reasoning` 默认是 nil，不发这个参数。所以用 `make-llm-deepseek` 的默认配置时，一次执行只要调用了工具，第二次请求就失败。task-sim 一直把推理设成 `none`，没有碰到。这是 2026-10-28 的 ARC 对比测试开推理时发现的（`.scratch/arc-bench/runs/superchat-vc33-smoke-think-*`：第 2 次请求 400）。

## Decision

用户 2026-10-28 选了"先修产品再跑"；另一个选项是只在测试代理里补字段。修在 superchat 这一层是实现时定的，用户可以改。`superchat-llm.el` 里三处 advice，都只对 `llm-deepseek` 生效：

1. `superchat--llm-deepseek-keep-reply-reasoning`（包 `llm-provider-utils-process-result`）：llm.el 把 DeepSeek 的一次回复记进 prompt 的这段时间里，这次回复的 `:reasoning` 放在 `superchat--llm-deepseek-reply-reasoning`。流式和非流式两条路径到这里时都带着 `:reasoning`。
2. `superchat--llm-deepseek-note-reasoning`（包 `llm-provider-utils-append-to-prompt`）：这次回复追加的助手交互，把推理记在 `multi-turn-plist` 的 `:deepseek-reasoning-content`。llm.el 把一次回复的正文和工具调用记成两条交互；第一条带推理，第二条带 ""。
3. `superchat--llm-deepseek-send-reasoning`（包 `llm-provider-chat-request`）：推理不是 `none` 的 DeepSeek 请求里，助手消息带 `reasoning_content`。交互记着推理的，带它记着的；没记着、又在最后一条用户消息之后的，带 ""。其它 provider 的请求和推理为 `none` 的请求原样不动。

从 Tape 重建的早先几轮不带推理。

## DeepSeek 实际检查什么

DeepSeek 的说明写的是 "the `reasoning_content` of all previous turns should be passed back to the API"。2026-10-28 直接调用 API（`deepseek-v4-flash`，`thinking` 为 enabled，`reasoning_effort` 为 high，除最后两行）试出来的是：

| 消息形状 | 结果 |
|---|---|
| 当前一问里，带工具调用的助手消息没有 `reasoning_content` | 400 |
| 同上，带真实推理 | 200 |
| 同上，带 "" | 200 |
| 当前一问里，正文和工具调用分成两条助手消息，只有第一条带推理 | 400 |
| 同上，两条都带（真实推理、""） | 200 |
| 早先几轮（最后一条用户消息之前）的助手消息不带推理 | 200 |
| 早先几轮带 "" | 200 |
| 早先几轮工具调用那条带推理、最后的正文不带 | 200 |
| 不发 `thinking`：第一次请求的回复里有推理（46 个推理 token） | 200 |
| 不发 `thinking`，当前一问里带工具调用的助手消息没有 `reasoning_content` | 400 |

也就是说：只检查当前一问；"" 也接受；llm.el 分开的两条都要带；不发 `thinking` 也在推理。

## 推理落在哪里

- 一次执行里：记在 llm.el 的交互上，随后面每次请求送回。
- Tape：每次请求的证据 `decision.snapshot.recorded` 里，prepared envelope 记着交互的 `:multi-turn`，所以推理文本也在那里。`superchat-tape-history-messages` 不读它，重建早先几轮时不带推理。
- superchat 的工具失败提示是一条用户消息（roadmap 的 9c 一节）。所以在同一次执行里，失败提示之后，DeepSeek 把前面的助手消息算作早先几轮，不再检查；它们记下的推理照样送回。

## 验证

- `scripts/deepseek-reasoning-regression.el`，9 个测试，都经过 llm.el 自己的 `llm-provider-utils-process-result` 和 `llm-provider-chat-request`：一次回复的两条交互一条带推理、一条带 ""；下一次请求送回；推理设置为 nil 时也送；没有正文的回复，推理落在工具调用那条；推理为 `none` 时没有这个字段；`llm-openai-compatible` 不变；早先几轮不带；`superchat--llm-set-history` 换掉早先几轮以后，当前的推理还在；`superchat--llm-drop-dangling-tool-use` 去掉一条交互以后仍然对得上。去掉三处 advice 再跑：7 个失败，2 个（本来就不该变的）通过。
- 原有套件全部通过：m5-execution 62、history-messages 23、m5-response 11、context-recovery 6、tool-compaction 5、tool-observation 9、task-sim-fixes 6、m5-request-context 11、m5-history-material 8；Kernel check OK；冒烟集 0 处不一致；M3 139 个，137 个符合预期，2 个跳过。字节编译的警告和改动前一样，10 条。
- 真实模型：ARC 测试机（`.scratch/arc-bench/runs/`，不进版本库）上的 4 次冒烟测试，`deepseek-v4-flash`，游戏 vc33，每次额度 30 万 token，superchat 运行在沙箱 Emacs 里。用户自己的 Emacs 里没有跑过。
  - 改动前，推理 `maximum`（`superchat-vc33-smoke-think-20261028-203613`）：第 2 次请求 400，错误就是开头那句。
  - 改动后，推理 `maximum`（`superchat-vc33-smoke-fixed-max-20261028-205502`）：18 次请求全部被接受。最后一条用户消息之后的 100 条助手消息都带 `reasoning_content`，94 条是推理原文，6 条是 ""。到额度才停。
  - 改动后，推理 nil，测试代理不改请求（`superchat-vc33-smoke-fixed-shipped-20261028-210512`），也就是出厂默认：请求里没有 `thinking`，DeepSeek 自己开了推理（16 次回复里 14 次带推理，单次最多 2625 个推理 token）。16 次请求全部被接受。
  - 改动后，推理 nil，测试代理补了 `thinking: disabled`（`superchat-vc33-smoke-fixed-default-20261028-205502`）：24 次请求全部被接受。代理关掉了推理，所以这一局只说明：不推理时，助手消息带 "" 也被接受。
  - 之后 ARC 开推理的正式测试（`superchat-ls20-think-20261028-210756`、`superchat-ft09-think-20261028-210916`、`superchat-vc33-think-20261028-211036`），推理 `maximum`，每局 88 到 132 次请求，都做到 1500 万 token 额度用完，没有一次 400。2026-10-29 同样条件又跑两轮，superchat 6 局（`superchat-*-think2-*`、`superchat-*-think3-*`），每局 75 到 207 次请求，也没有一次 400；其中 ft09 两局在额度用完前通关。合计带这个修复的 9 局，没有一次 400。

## Alternatives considered

只在测试代理里补字段：代理记住每次回复的 `reasoning_content`，按工具调用编号塞回下一次请求。不动产品，马上就能跑对比；但默认配置照样坏着，测出来的也不是用户手上的 superchat。用户没有选它。

修在 llm.el：给 `llm-deepseek` 实现 `llm-provider-extract-for-multi-turn`，让聊天补全的消息组装送回 `reasoning_content`。这是这件事最终该在的地方。但 superchat 依赖已发布的 llm.el（≥ 0.31.1），上游改了也要等发布，在那之前 DeepSeek 用户照样坏。`superchat-llm.el` 里已有同样做法的先例：`superchat--llm-refuse-cut-off-reply`、`superchat--llm-keep-unparsed-stop-reason`。

让后面的执行也送回早先几轮的推理：要让 `superchat-tape-history-messages` 和 `superchat--llm-history-interactions` 多带一个字段。DeepSeek 不检查早先几轮（上表），送回去会让每次请求变大。nodecode 是全部送回的。这次不做。

所有助手消息一律带 ""：最简单，也能通过 API；但模型在同一次执行里看不到自己前一步的推理，而 DeepSeek 的说明写推理要接进上下文。

## Consequences

- 用 DeepSeek 时，不设推理（默认）和显式开推理，调用工具的执行都能继续下去。
- 只覆盖 llm.el 的 `llm-deepseek`（`make-llm-deepseek`）。用 `llm-openai-compatible` 连 DeepSeek 的配置不在范围内：按上面的实测规则，它不发 `thinking` 时 DeepSeek 照样推理，调用工具后的第二次请求照样 400。这是推论，没有用这个 provider 实测。
- 一次执行里的请求变大：每条助手消息多一段推理。ARC 开推理的测试里（`superchat-ls20-think-20261028-210756`），一次执行做到 1500 万 token 额度用完：到第 40 次请求，送回的推理占请求内容的 80%，最后一次请求 30.8 万 token。DeepSeek 对当前一问里的助手消息也接受 ""，所以只保留最近几次回复的推理在 API 上可行；对模型表现的影响没有测过，也没有决定要做。
- 后面的执行里，早先几轮不带推理，模型看不到上一次执行的推理。
- 推理为 `none` 的请求一个字节也不变：task-sim 和 ARC 不开推理的 6 局的结果仍然有效。
- `decision.snapshot.recorded` 的证据变大，大小和推理长度相当。
- llm.el 上游支持 DeepSeek 送回推理以后，这三处 advice 应该去掉。
