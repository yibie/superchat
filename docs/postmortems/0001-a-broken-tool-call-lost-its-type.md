# Postmortem 0001: A broken tool call lost its type without streaming

Status: resolved

## Executive summary

不流式时，模型写出不合法的工具调用参数，superchat 本该带着纠正说明重发（`superchat-llm-unknown-tool-retries`），实际却让整次执行以 `[API error: ...]` 失败。原因是错误类型在路上丢了：llm.el 在请求的成功回调里解析参数，解析失败抛出 `llm-tool-call-error`；superchat 把它转给请求的 `:on-error`，而 llm.el 的 `:on-error` 一律按 `error` 报；重发只认 `llm-tool-*` 类型。测试没有发现，是因为假模型在 llm.el 那一层直接按测试写好的类型调用错误回调，正好跳过了丢类型的两层。教训：一个约定跨两个库时，测试要穿过真实的那几层。

## Summary

`superchat-llm-unknown-tool-retries` 的说明写的是：llm.el 报告的不存在的工具名和格式错误的参数，最多连续两次修好请求再发，不结束这一轮。流式请求按说明工作。不流式请求遇到不合法的 JSON 参数时，修复重发从不发生，执行直接失败。

## Impact

- 不流式的请求都受影响。带工具时，superchat 只在模型同时有 `:streaming` 和 `:streaming-tool-use` 两项能力时才流式（`superchat-llm--resolve-streaming`）；ARC 测试里用 `make-llm-deepseek`、其余默认，带工具的请求都是不流式的（每次回复的 TTFT 一行写着 "non-partial provider"）。子 agent 走同一个循环（`:unknown-tool-retries` 由 `superchat-subagent.el` 传入），同样受影响。
- ARC 测试里见到 1 次：`superchat-vc33-think3-20261029-010010` 的第 51 次请求，是开推理 9 局 superchat 的 1170 次工具调用里唯一的一次。测试机的跟进消息接上了这一局；用户自己用时，执行就停在那里，要用户让它继续。
- 本文只核实了参数不合法这一种。不流式时，不存在的工具名、缺少或多出的参数会不会走同一条路，没有核实。流式请求走另一条路，这次也没有核实。

## Timeline

- 2026-10-21：真实探测发现，不流式时参数解析错误到达 superchat 时类型是 `error`、也没有停止原因（`scripts/reply-cut-off-regression.el` 的 2b）。当时只处理了回复因输出上限被截断（`length`）这一种：截断检查同时接受 `error` 和 `llm-tool-*`。
- 2026-10-29 01:15:42（UTC-7）：ARC 第 3 轮 vc33 的第 51 次请求。模型的参数以 `{"progn\n  (defun arcg` 开头，没有 `"expression"` 键，也没写完，停止原因是 `tool_calls`。superchat 记下 `[API error: An error occurred when calling an LLM tool: "Invalid JSON in tool call arguments: could not parse JSON stream: 2, nil, 8"]`，执行失败。第 52 次请求是跟进开的新执行（1.96 万 token），不是带纠正说明的重发。
- 2026-10-29：读代码定位到丢类型的位置，用 llm.el 的真实非流式路径复现（只替换 plz 的请求），修复。

## Root cause

错误类型要穿过三层才到 superchat 的错误回调，第二层把它丢了：

1. llm.el 0.32.1 的不流式 `llm-chat-async`（`llm-provider-utils.el:403`）在 plz 的成功回调里调用 `llm-provider-utils-extract-all`。参数不合法时，`llm-provider-utils-parse-openai-tool-arguments` 抛出 `llm-tool-call-error`（`llm-provider-utils.el:748`）。这里没有 `condition-case`，plz 调用成功回调的 `plz--respond` 也没有（`plz.el:877`）。
2. superchat 的 `superchat--llm-request-plz-propagate-callback-errors` 接住 `llm-error`，调用 `(funcall on-error (car err) (error-message-string err))`，类型是传了的。但 llm.el 的 `:on-error` 写的是 `(lambda (_ data) ...)`：丢掉第一个参数，以 `error` 和消息调用错误回调（`llm-provider-utils.el:429`）。
3. superchat 的错误回调只在停止原因是 `length` 时把 `error` 认作截断；其余情况下 `superchat--llm-tool-protocol-error-p` 不认 `error`，执行走到最后一个分支，以 `[API error: ...]` 失败。

测试为什么没发现：`scripts/smoke.el` 的假网络替换的是 `llm-chat-async` 本身，错误按测试写的类型直接送进 superchat 的错误回调。superchat 的包装和 llm.el 的 `:on-error` 两层都不在测试路径上，所以测试只证明了"类型对时会重发"，没证明"类型能到"。

## Guardrails added

- 修复：包装层在调用 `:on-error` 期间把原来的条件绑定在 `superchat--llm-callback-error` 上；superchat 的错误回调收到 `error`、而绑定的是 `llm-tool-*` 条件时，把类型认回来，再走截断检查和修复重发。从 `:on-error` 到错误回调是同步调用（`llm-provider-utils-callback-in-buffer`、`superchat--callback-in-view`），绑定一路可见。
- [`scripts/tool-error-type-regression.el`](../../scripts/tool-error-type-regression.el)，5 个测试。第 1 个走 llm.el 的真实非流式路径，用 `make-llm-deepseek`，只替换 `plz-media-type-request`，喂录下的那条回复：断言 llm.el 交出的是 `error`，并且 superchat 在回调运行期间持有 `llm-tool-call-error`。llm.el 以后如果自己保住了类型，这个断言会失败，提醒这层补救可以去掉。其余 4 个走完整对话：不合法的参数被纠正后重发、重发有上限、不是工具错误的照旧失败、截断照旧按截断处理。修复前第 1 到第 3 个失败。
- `scripts/smoke.el` 的假网络加了 `:raised` 键，模拟"成功回调里抛出、正在报告的条件"。

## Lessons

- 一个约定跨两个库时（这里是错误类型），测试要穿过真实的那几层。假模型替换的层级越高，越多转换被跳过；对这种约定，要在第三方代码之下替换（这里是 plz）。
- 2026-10-21 已经看到"不流式时错误以 `error` 到达"，当时只修了截断那一种情况。看到一个类型丢失时，要查还有哪些分支靠这个类型。
