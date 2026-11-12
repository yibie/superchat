# Postmortem 0002: Text a request could not carry

Status: resolved

## Executive summary

2026-11-09，一个对话的 12 次执行里有 7 次结束时只显示 `[llm-chat-streaming error: Apparently circular structure being printed]`，用户每次只能打"继续"。真正的原因和"循环结构"无关：工具结果里有一个 telega 存成两半的 emoji（UTF-16 代理对的两半，各算一个字符），而发给模型的请求是 JSON，`json-serialize` 不收这种字符，请求根本组不出来。估算请求大小时退回去"打印整个 prompt"，prompt 里的工具是指回执行本身的闭包，打印就报"循环结构"；八处一样的错误处理又把这句话安在 `llm-chat-streaming` 名下，存成助手的回答。没被测试发现，是因为八月的修复没有测试它要建立的性质（"`json-serialize` 接受"），而测试用的假模型替换在 llm.el 序列化之前。教训：一个修复要配上它所声称的性质的测试；一个文本分给两个编码器时，两边都要测；报错的文字要说出真正出错的那一层。

## Summary

发给模型的每个请求都要先用 `json-serialize` 编成 JSON。Emacs 的字符串能装 JSON 装不下的东西：代理对的一半、原始字节（被切断的多字节字符、按字节读的文件、SQLite 交回来的半个代理）、大于 U+10FFFF 的字符。请求里任何一处有一个这样的字符，整个请求就组不出来。

superchat 只在工具执行的关口处理过其中一种形状（整段 UTF-8 被当字节读进来），而且处理得不对；用户的消息和上下文片段根本不经过那个关口。请求组不出来以后，估算、错误处理、执行记录三处又各自把问题藏了一层。

## Impact

- 对话 `20261109-005550-3bd3`，模型 `deepseek-v4-flash`，经 `llm-deepseek`，流式。任务是从一个 telega 群导出一位成员最近三周的消息。12 次执行里 7 次以这句话结束（assistant 行 #10756、#10806、#10868、#10915、#10935、#11231、#11371），这是那几次唯一可见的文字。
- 7 次里有 6 次，失败前最后一个工具结果里有带 emoji 的 telega 字符串（tool.completed 行 #10752、#10864、#10911、#10931、#11227、#11367）。完成了的执行没有一次有这种结果。第 7 次（#10802）查不了：Tape 只存结果的前 2000 个字符。
- 模型读到了这句话。回复 #10877 把它解释成 "telega plists with circular references"，改了返回的内容，去躲一个并不存在的问题。
- 同一句话在 2026-10-03 结束过 2 次执行（对话 `20261002-185205-fa8b`，行 #1722、#1756）。之前的工具结果（`read_buffer`、`run_shell`）里有被切成两半的中文字符（Tape 里是 `\345\211`、`\346`）。这两次是同一个原因，是推断；没有经 `run_shell` 复现。
- 整个 Tape 自 2026-10-01 起：262 次 `run.started`，25 次 `run.failed`。25 次里 9 次是这句话，10 次是 `[Stopped after 3 failed calls in a row]`，4 次是 DeepSeek 的 `reasoning_content` 协议错误，2 次没有 assistant 行。
- 其他入口也会这样：用户消息里有一个代理对，或从名字里有代理对的 buffer 开始对话（`[Emacs origin]` 片段），执行都在任何请求发出之前结束（已复现，0 个请求）。

## Timeline

- 2026-08-14，提交 cb8de3da：工具执行关口加上 `superchat-tool-execution--normalize-tool-result`，说明里写的正是这种失败（"killing the next streaming request with `wrong-type-argument json-value-p'"）。它是为一种形状写的：整段 UTF-8 被当字节读进来。没有测试。
- 2026-08-30，提交 3c98627e：估算请求大小的 `superchat--llm-serialized-attempt` 的第三条退路是 `(prin1-to-string prompt)`。
- 2026-10-03：对话 `20261002-185205-fa8b` 的 2 次执行以这句话结束（#1722、#1756）。
- 2026-11-09：对话 `20261109-005550-3bd3` 的 7 次执行以这句话结束。用户说"先查中断，看看这里是否存在架构，机制方面存在的问题"。
- 2026-11-09：对照用户的 Tape（只读）、源码和批处理复现，查清下面的机制；在用户的 Emacs 里确认一个群名的最后两个字符是 U+D83D U+DEA9，带 `telega-emoji-p` 和 `telega-display "🚩"` 属性。用户对七条建议说"同意建议"，修复按[决定](../decisions/accepted/2026-11-09-a-request-that-cannot-be-encoded.md)完成。

## Root cause

每一步都核实过：

1. telega 把群名里的 emoji 存成代理对的两半，每一半是一个字符。
2. `eval_elisp` 用 `(format "%S" value)` 输出值（`superchat-tools--eval-value-result`），两半原样留着。
3. `superchat-tool-execution--normalize-tool-result` 的判断 `(catch 'raw-byte (mapc ... value))` 对每个多字节字符串都成立，因为 `mapc` 返回它的序列；所以每个多字节结果都走 `(decode-coding-string (string-as-unibyte value) 'utf-8)`。正常文字经过它不变；半个代理变成三个原始字节（ED A0 BD），`json-serialize` 照样不收。用 8 种输入测这个函数，处理后仍有 5 种被拒（一对两半、切成两半的中文字符、一个原始字节、大于 U+10FFFF 的字符、latin-1 字节）。
4. Tape 用 json.el 的 `json-encode` 编码，原始字节写成 4 个 ASCII 字符 `\355`。所以 `tool.completed` 行里是一段看起来没问题的文字，和准备发给模型的不一样。这也是之后打"继续"能开始的原因：之前的轮次从 Tape 读回来，那里已经是 ASCII。
5. 每次尝试前，传输层估算请求大小：`superchat--llm-estimate-context-tokens` 调 `superchat--llm-serialized-attempt`。它先对 llm.el 的请求、再对 superchat 自己的证据形式调 `json-serialize`，两次都报 `(wrong-type-argument json-value-p ...)`，都被 `(condition-case nil ... (error nil))` 吞掉。第三条退路打印 prompt；prompt 里的工具是指回执行本身的闭包，打印就报 `Apparently circular structure being printed`。打开 `print-circle` 能打印出来，有 108,991 个字符：对提供工具的请求，这条退路永远不会成功。
6. 这个错误从 `send-request` 经八份一样的处理之一出来，处理调用 `finalize`，文字是 `(format "[llm-chat-streaming error: %s]" ...)`。可是 `llm-chat-streaming` 根本没被调用，请求从没组出来。`run.failed` 只记了 `{"status":"failed"}`。
7. 这句话成了执行的回答，存成 `assistant` 行，在下一次请求的历史里发给模型。
8. 第二层：绕过估算以后，llm.el 自己的请求层（`llm-request-plz.el:149`，对请求体调 `json-serialize`）在同一段文字上失败。只修估算，只会换一句报错。

缺口在表示上：superchat 里没有一个地方说"这段文字能发出去"。每个入口各管各的，或者不管；一个文字分给 Tape 和模型两个编码器，两边的规则不一样；估算会失败，失败后的退路决定了执行的结局；报错的文字是按代码位置写的，不是按出错的层写的。

为什么没发现：

- `scripts/` 下没有一个脚本提到 `normalize-tool-result`、代理码点或 `json-value-p`。八月的修复没有测试"`json-serialize` 接受它"这条性质。
- 标准测试夹具（`scripts/smoke.el`）替换 `llm-chat-streaming` 和 `llm-chat-async`，这些测试里请求体从来不经过 llm.el 的序列化。0001 记过同一条教训（"一个约定跨两个库时，测试要穿过真实的那几层"），针对的是另一个约定。
- Tape 行看起来没问题（第 4 步），可见的文字指向打印，而不是编码（第 5、6 步）。

## Guardrails added

- `superchat-wire.el` 的 `superchat-wire-safe-text`：一个函数，一条性质，返回的文字 `json-serialize` 都收。能发的文字原样返回（`eq`）；代理对的两半合成一个字符，包括 SQLite 交回的三个字节的半个代理；读得通的 UTF-8 字节读成文字；其余变成 U+FFFD。
- 两处用它：工具执行关口（结果分给 Tape、界面、模型之前，三处拿到同一段文字），和模型传输层的 `superchat--llm-prompt-wire-safe`（组好 prompt 时一次，每次尝试前再一次；系统文字、每条消息、每个工具结果）。
- 估算只量不判：组不出请求时用 `superchat--llm-prompt-size` 数文字的字节，不打印，不看工具的函数，什么 prompt 都返回一个数。`(prin1-to-string prompt)` 删掉了。`superchat-show-ttft-breakdown` 的大小一行也改成数，不再用 `%S` 打印工具。
- 组不出请求的尝试经一个出口 `fail-attempt` 结束，文字是 `[Superchat could not start this request: REASON]`；`run.failed` 带 `:failure`（`:class`、`:phase`、`:error-type`、`:reason`）。
- 关闭主对话时写的 `session-cache.org` 也写成能发的文字、按 UTF-8 写：原来一个原始字节会让写入在关闭 buffer 时弹出 "Select coding system"。
- [`scripts/wire-safe-regression.el`](../../scripts/wire-safe-regression.el)，21 个测试。随机字符串上的性质测试；工具结果、用户消息、上下文片段三个入口，从 SQLite 读回的历史；没有序列化器时的估算；llm.el 自己的请求体（只替换 plz），断言 llm.el 本身不收代理对，llm.el 以后改了这条断言会失败；失败的记录；大小一行；会话缓存。没有这次修改时（`superchat-wire-safe-text` 原样返回参数），每个测试单独运行，18 个失败，3 个是守护测试照样通过；代理对在工具结果、用户消息、buffer 名里的三个整次执行都以 `failed` 结束，工具结果那次显示的正是 `[llm-chat-streaming error: Apparently circular structure being printed]`。测试夹具里 `print-circle` 绑定为 nil：脚本为 ERT 的输出打开了它，打开时打印工具的闭包会成功，2026-11-09 的失败就出现不了。

## Lessons

- 一个修复要配上它所声称的性质的测试。八月的修复说的是"让 `json-serialize` 收下工具结果"，可是没有一个测试调 `json-serialize`；它的判断恒为真，它把一种被拒的形状换成了另一种，8 种输入里 5 种照样被拒。一条性质测试（随机塞坏字符，断言 `json-serialize` 收下）当时就会失败。
- 一个文字分给两个编码器时，两边都要测。Tape 的 `json-encode` 把原始字节写成 `\355`，模型那边的 `json-serialize` 直接拒绝；看 Tape 行的人看到的是没问题的文字，之后的"继续"也因此能开始，问题就更难看出来。
- 一条退路要能成功，否则它决定的就是结局。估算失败后打印 prompt，对带工具的请求永远会报错；一个只用来量大小的步骤，结束了整次执行。
- 报错的文字要说出真正出错的那一层。`[llm-chat-streaming error: ...]` 说的是一个没被调用的函数，"circular" 说的是打印；用户看不出是编码，模型照字面理解，去改了不相干的东西。
- 测试替换的那一层要在被测的转换之下。假模型替换在 llm.el 序列化之前，请求体从不被编码；0001 的教训在另一个约定上又出现了一次。
