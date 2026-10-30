# Decision: 一次什么都没收到的尝试会重发

Status: accepted — 用户在 ARC 对比的下一步候选里选了"都做"，其中一项是"回复卡住时自动重试"。

## Problem

`superchat-response-timeout` 管的是一次尝试的无响应时间：收到文字、推理或工具调用数据就重新计时，到点就结束这次执行，结局是 `timed-out`。一次尝试从头到尾一个字节都没收到，通常是服务端卡住了，重新发一次多半就好；可是这种情况和"回复到一半停住"一样，都直接结束执行。

ARC 第 2 轮 ft09（`superchat-ft09-think2-20261029-001038`）的第 35 次请求：DeepSeek 回了 200 响应头，184.5 秒后正文还是 0 字节（测试机的代理记下 `IncompleteRead(0 bytes read)`）。superchat 的计时器到点（120 秒，带工具时乘 1.5），执行以 `timed-out` 结束，靠测试机发的跟进消息才接上，这一局最后通关。用户自己用的时候，执行就停在那里。

superchat 已有的自动重试（`superchat-llm-transient-retries`）只认 HTTP 429/5xx、`Operation timeout.` 和几种 curl 错误（`superchat--llm-transient-error-p`）。计时器到点时直接结束执行，不经过这条路。

## Decision

用户的原话是"都做"（2026-10-29），回答的是三个下一步候选，第 2 个是"回复卡住时自动重试（候选第 7 条）"；候选第 7 条见 [nodecode 对照](../../research/nodecode.md) 的"可以考虑借的"。

- 计时器到点时，如果这次尝试还没有收到任何回答文字、推理或工具调用数据，就放弃这次尝试，等 `superchat-llm-transient-retry-interval`（默认 30 秒）后重发。连续最多 `superchat-llm-stall-retries` 次，默认 2 次；有一次回复成功就重新计数。
- 放弃一次尝试的顺序：先把它标为作废，它之后送来的回复、错误、流式片段都不算数；然后用 `llm-cancel-request` 取消它的请求；最后报告 `retry-scheduled`，等待后重发。顺序不能反：取消请求本身会让它的错误回调回来，必须先作废。
- 收到过东西的尝试到点时，照旧以 `timed-out` 结束。重发会重复已经显示给用户的文字，或者重复已经开始执行的工具调用。
- `superchat-llm-stall-retries` 设为 0，就是第一次安静就结束，和以前一样。

## 次数和等待

默认 2 次。nodecode 对同样的情况重试 5 次：200 响应头之后 120 秒没有数据，算一次失败的尝试（`provider.lisp:77`、`:86`）。superchat 带工具时一个窗口是 180 秒，再加上请求越大窗口越长（每 256 token 加 1 秒），所以每多重试一次，最坏要多等好几分钟。默认 2 次时，一直卡住的服务端要过 3 个窗口和 2 次等待才结束，带工具时约 10 分钟，不重试时是 3 分钟。

等待间隔和 HTTP 429/5xx 共用 `superchat-llm-transient-retry-interval`：一个设置管两种"服务端暂时不行"的情况。

## 不流式时，慢和卡住分不清

不流式时，回复整段到达之前没有任何进度，所以一个只是慢的回复也算安静：到点后会被重发，而不是失败。带工具时，superchat 只在模型同时有 `:streaming` 和 `:streaming-tool-use` 能力时才流式；ARC 测试里 `make-llm-deepseek` 带工具的请求都是不流式的。所以一个确实需要比窗口更长时间才能生成完的回复，会被重发 2 次后才以 `timed-out` 结束，多花 2 次请求的时间。窗口已经随请求大小放大，这种情况只在回复本身生成得慢时出现；遇到时调大 `superchat-response-timeout`。

## Alternatives considered

用现有的 `superchat-llm-transient-retries`（默认 10 次）。输在代价：每次重试都要先等满一个窗口，一直卡住时要 10 个窗口加 10 次等待，带工具时超过半小时才结束。HTTP 错误是立刻知道的，次数多不要紧；安静要等满窗口才知道，次数要少。

到点立刻重发，不等待。输在一致性：服务端卡住常是暂时的，和 429/5xx 一样需要缓一下；单独为它设一个间隔，多一个设置却没有多一种情况。

照旧结束，让用户手动继续。输在无人值守的场合：定时任务、长任务、用户走开时，一次卡住就让整件事停下，而重发一次通常就好。

收到过部分内容也重发。输在安全：重发会重复已经显示的文字，或者重复已经开始执行的工具调用；只有什么都没收到时，重发才不会重复任何事。

## Consequences

- 一次尝试完全安静时，执行不立刻结束：Runtime 收到 `retry-scheduled`（`:error` 写 "No response for Ns"），live 状态是 `retrying`；等待结束后以 attempt reason `transient-retry` 开新的一次尝试，开一个新 Step。
- 被放弃的尝试的请求被取消；它之后送来的任何东西都被丢掉，即使是一个完整的回复。服务端在取消前已经生成的部分是否计费，没有核实。
- 计时器的机制测试（`scripts/m5-execution-regression.el` 的 `m5w-with-run`）把重发设为 0，继续以 `timed-out` 观察计时器；`scripts/no-cumulative-limit-regression.el` 用同一个夹具。重发本身由 `scripts/stall-retry-regression.el` 测。
- [请求生命周期](../../protocols/request-lifecycle.md) 的 "Model progress and tool budgets" 写明到点之后怎么办。
- 实地第一次触发在 25 个游戏那批的 ka59（`superchat-ka59-g25-20261029-040255`）第 102 次请求：DeepSeek 回了 200，之后 422 秒只发来 7 个换行就断开。请求体 255,407 个字符，按 superchat 的估算（每 2 个字符 1 token，再乘 1.10）是 140,475 token，窗口是 180 + 533 = 713 秒；到点后放弃这次尝试，30 秒后重发。代理记下的两次请求相隔 745.4 秒，和 713 + 30 = 743 秒相差不到 3 秒；重发的请求和原请求逐字节相同，同一次执行接着玩。请求这么大时一个窗口约 12 分钟，默认 2 次重发要过 3 个窗口和 2 次等待，约 37 分钟，比 30 分钟的 Runtime 上限还长，所以上限会先到。
- 同一次里，等满窗口有一部分是测试机代理造成的。代理用 HTTP/1.0 转发（`.scratch/arc-bench/proxy/model-proxy.py`），已经发出了 DeepSeek 的 200 响应头；上游断开后，它把自己的 502 状态行和错误文字接在那 7 个换行后面，再关闭连接。HTTP/1.0 的正文到关闭为止，所以 curl 看到的是一个完整的 200 回复；llm.el 在 plz 的计时器里解析它，在第 8 个字符（那段 502 文字的开头）报 `(json-parse-error 8 nil 8)`（`Error running timer 'plz--respond'`）。这个错误没有传回 superchat，所以只能等窗口到点。真实使用时没有这个代理，superchat 经 curl 走 HTTP/2；那里的断开 curl 报 92、16、18 或 56；测量时自动重试只认 56，2026-10-29 起四个都认（[DeepSeek 中途断开](../../research/deepseek-drop-curl-codes.md)、[重试决定的第三次修订](2026-09-23-retry-transient-provider-errors.md)）。
- 同一批里又触发 3 次，都是这种情况：sk48（`superchat-sk48-g25-20261029-055801`）第 28、108 次请求和 su15（`superchat-su15-g25-20261029-061043`）第 138 次请求，上游分别在 37、62、62 秒断开，superchat 按窗口等了 646、1054、734 秒，加 30 秒后逐字节相同地重发，代理记下的间隔是 677.1、1088.4、769.4 秒。连同 ka59，4 次都在同一次执行里接着玩；如果断开当时就报错并重发，4 次合计可以少等约 43 分钟。这 4 次等待所在的执行，后来都在 30 分钟上限处被截断。nodecode 是流式请求，同一批里上游断开 4 次（cd82 一次在回复之前，ka59 一次、tr87 两次在 200 之后），它每次记一次 `turn.provider_retry` 后重发，没有等窗口。
