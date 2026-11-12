# Decision: 请求里有发不出去的字符时，先修好再发；实在组不出请求，就说清是 superchat 组不出来

Status: accepted — 用户 2026-11-09 对评估里的七条建议说"同意建议"。本条是其中第 1 到 3 条的做法。和用户读到的第 3 条有两处不同：不在失败以后"找出哪条消息编不了，替换，告诉模型，继续"，因为发送前的关口已经把文字修好了；失败的文字仍然存成这一轮的回答，只是换了措辞，写明是 superchat 说的。这两处不同 2026-11-09 已经报告给用户，用户还没有回答。

## Problem

2026-11-09，对话 `20261109-005550-3bd3`（模型 `deepseek-v4-flash`，经 `llm-deepseek`，流式）的 12 次执行里有 7 次结束时只显示 `[llm-chat-streaming error: Apparently circular structure being printed]`，用户每次只能打"继续"。2026-10-03 同一句话也结束过 2 次执行。证据和完整经过在[事故复盘](../../postmortems/0002-text-a-request-could-not-carry.md)里，这里只说机制：

- 发给模型的请求是 JSON。llm.el 每次都用 `json-serialize` 把请求编成 JSON，它只收真正的 Unicode 字符。Emacs 的字符串能装更多东西：
  - 代理对的一半。一个 emoji 在 UTF-16 里写成两个"半字符"，合起来叫一个代理对；telega 把群名里的 emoji 存成这两半，每一半在 Emacs 里算一个字符。
  - 原始字节：被切成两半的中文字符、按字节读进来的文件、SQLite 交回来的半个代理。
  - 大于 U+10FFFF 的字符。

  请求里任何一处有一个这样的字符，整个请求就组不出来。
- 工具执行的关口（每个工具结果分给 Tape、界面、模型之前都要经过的那一处）有一个处理函数，只管一种形状：整段 UTF-8 被当字节读进来。碰到半个代理，它反而变出三个原始字节，`json-serialize` 照样不收。用户的消息和上下文片段根本不经过这个关口。
- 每次尝试前，superchat 先估算请求有多大。估算时 `json-serialize` 失败，就退回去把整个 prompt 打印出来数字数。prompt 里的工具是指回执行本身的闭包，一打印就报 `Apparently circular structure being printed`。
- 这个错误从八份一样的错误处理之一出来，写成 `[llm-chat-streaming error: %s]`。可是 `llm-chat-streaming` 根本没被调用，请求从没组出来。`run.failed` 只记了状态，没有原因。这句话成了这一轮的回答，模型在下一次请求里读到，把它解释成 telega 数据里的"循环引用"，去躲一个不存在的问题。

## Decision

用户 2026-11-09 先说"先查中断，看看这里是否存在架构，机制方面存在的问题"，看完评估后说"同意建议"，又说"你可以提出修改方案，然后让 opus 来作为执行方，然后你来验收"。本条对应的三条建议，原文：

1. 发送前加最后一道关口：一个函数，一条性质（`json-serialize` 接受），覆盖所有入口，工具关口改用同一个函数。配一个随机塞坏字符的测试。
2. 估算失败就退回按字数估算，删掉 `(prin1-to-string prompt)`。
3. 把"本地组不出请求"做成正式的错误类型：找出哪条消息编不了，替换，告诉模型，继续。`run.failed` 记下类别、阶段、原因。系统报错不再存成助手发言。

第 6 条的前一半（"把 8 处错误处理收成一个出口"）也在这里做了。第 6 条的后一半和第 4、5、7 条不在本条里，做到哪一步见[路线图](../../roadmap.md)。

做法：

**一、一个函数说"这段文字能发出去"。** `superchat-wire.el` 里的 `superchat-wire-safe-text`：它返回的文字，`json-serialize` 一定收。

- 本来就能发的文字原样返回，是同一个对象（`eq`）。所以平常的情况只多一次编码，什么都不变。
- 本来发不出去的：
  - 读得通的 UTF-8 字节，读成它表示的文字；
  - 代理对的两半合成一个字符，包括 SQLite 交回来的三个字节（ED Ax xx）表示的半个代理；
  - 剩下仍然不是 Unicode 字符的，换成 U+FFFD（替换符 `�`）。
- 不加说明，别的什么都不改。

**二、两个地方用它。**

- 工具执行的关口：`superchat-tool-execution--normalize-tool-result` 就是这一个调用。结果分给 Tape、界面、模型之前就修好，三处拿到的是同一段文字。
- 模型传输层：`superchat--llm-prompt-wire-safe` 让请求里的每段文字都过这个函数，包括系统文字、每条消息的文字（含多段消息里的文字段）、每个工具结果。
  - 组好 prompt 时过一次；每次尝试前，在 `send-request` 里、估算和记录之前再过一次。
  - 执行中途加进来的东西（Step 插入的材料、用户中途追加的话、最后一轮的"请直接回答"、重试时重建的系统文字）都在这一步之前加进去，所以文字从哪条路进来都一样。
  - 它只改消息里的文字，不换消息对象：`superchat--llm-set-history` 是按对象认出之前的轮次的。
  - 请求证据里记的用户输入和系统文字（`provider-prompt`、`provider-system-prompt`）也是修过、真正发出去的那一份。

Tape 里的用户输入按用户打的原样保存；改的只是发给模型的那一份。

**三、估算只量，不决定结局。** `superchat--llm-estimate-context-tokens` 能拿到编好的请求就量它。拿不到时用 `superchat--llm-prompt-size` 数文字的字节：系统文字、消息、工具调用、工具结果，以及每个工具的名字、说明和参数。
- 它不打印任何东西，不看工具的函数。
- 看过一定数量的值就停。
- 不管 prompt 里装着什么，都返回一个数。

`(prin1-to-string prompt)` 删掉了。

**四、实在组不出请求，经一个出口结束，说清是谁、为什么。** 一次尝试在准备或发出时出错，经 `fail-attempt` 结束这次执行；原来的八份处理收成了这一处。
- 每个调用点传的阶段，就是它那次 `send-request` 的理由。
- 之前的 `superchat-llm-evidence-recording-error` 分支不变。第一次尝试在有证据约定时把这种错误当作记录错误，这条规则也不变。

这一轮显示的回答是 `[Superchat could not start this request: REASON]`。REASON 由 `superchat--llm-local-failure` 给出：
- `json-serialize` 拒收时，写成 `a text in it cannot be encoded as JSON (N characters, starting "…")`：只引被拒文字的前 40 个字符，去掉文字属性，再过一遍同一个函数；
- 别的错误，用错误本身的消息；
- 最长 600 个字符。

被拒的整段文字从不引用：发不出去的正是它，而这句原因会进下一次请求的历史。

**五、执行记下原因。** 传输层结束执行时，把 `:failure` 交给 Runtime。它是一个列表，有四项：

| 项 | 内容 |
|---|---|
| `:class` | `json-serialize` 拒收（`wrong-type-argument` 带 `json-value-p`）时是 `"request-not-encodable"`，别的错误是 `"request-not-started"` |
| `:phase` | 尝试的理由：`initial`、`tool-followup`、`final-answer`、`tool-protocol-retry`、`transient-retry` |
| `:error-type` | 错误的名字 |
| `:reason` | 上面那句原因 |

Runtime 把它放在这次执行的记录上；状态是 `failed` 时，`run.failed` 在状态旁边带上它。别的方式失败的执行不带 `:failure`，结束的先后顺序不变。

**六、失败的文字仍然是这一轮的回答。** 它照旧存成 `assistant` 行，在下一次请求的历史里发给模型，和别的失败一样。[重试的决定](2026-09-23-retry-transient-provider-errors.md)第六次修订就是让模型在那里读到失败文字的。用户打"继续"时，模型需要知道上一次没有答完。变的只是措辞：写明是 superchat 说的，不再冒充一个根本没被调用的库。

## 实现时定的

- **`superchat-show-ttft-breakdown` 打开时，大小那一行也改成数，不再打印。** 它原来用 `%S` 打印请求的工具，碰上同样的闭包就报错。错误从 `superchat-runtime-submit` 出来，用户的消息根本发不出去。现在它也用 `superchat--llm-prompt-size` 数。
- **关闭主对话时写的会话缓存也走同一个函数。** `superchat--summarize-session-on-exit` 把对话全文写进 `session-cache.org`。
  - 原样写时，一个原始字节或大于 U+10FFFF 的字符会让写入在关闭 buffer 时弹出 "Select coding system"。批处理里它去读标准输入，直接报错。
  - 半个代理在这个 Emacs（32.0.50）上不弹问题，被写成字节 ED A0 BD，读回来变成原始字节。读它的只有 `superchat--process-cached-session-on-startup`，读回来的文字存成一条待确认的记忆。
  - 现在写的是 `superchat-wire-safe-text` 的结果，写入时把 `coding-system-for-write` 绑成 `utf-8`：写入不会再问，读回来的是原来的字符。
- **失败原因里引的那 40 个字符不带文字属性。** telega 的文字带属性；带着属性用 `%S` 引，会写成 `#("xxx…" 0 40 (face …))`。
- **关口没经过的地方。** 工具的说明、参数和提供方选项，示例，助手消息里的工具调用，DeepSeek 留着的推理文字，都不经过关口。
  - Superchat 不设示例；工具说明和选项来自程序本身或解析过的 JSON。
  - 工具调用和推理来自模型的回复，由 `json-parse-string` 解析。实测它遇到孤立的半个代理或坏字节会直接报错，不会交出这种字符。
  - 这些地方要是真有发不出去的文字，执行照样结束，带上面那句原因。

## 验证

**先写会失败的测试。** [`scripts/wire-safe-regression.el`](../../../scripts/wire-safe-regression.el) 有 21 个测试。在没有这次修改的代码上，`superchat-wire-safe-text` 用一个原样返回参数的替身，每个测试单独运行：18 个失败，3 个是守护测试照样通过。其中：

- 工具结果里有代理对的整次执行以 `failed` 结束，界面显示的正是 `[llm-chat-streaming error: Apparently circular structure being printed]`。
- 用户消息、buffer 名里有代理对的两次执行也以 `failed` 结束，0 个请求到达模型。
- 会话缓存那个测试报 `(end-of-file "Error reading from stdin")`，之前打出了 `Select coding system (default raw-text):`。
- `superchat-show-ttft-breakdown` 那个测试报 `(error "Apparently circular structure being printed")`。

修改后 21 个全部通过。

测试夹具里 `print-circle` 绑成 nil。脚本为了 ERT 的输出把它全局打开了，打开时打印工具的闭包能成功，2026-11-09 的失败就出现不了。

**测试覆盖的内容：**
- 函数本身：固定的例子，加 3000 个随机字符串（由半个代理、原始字节、大于 U+10FFFF 的字符拼成）上的性质测试；
- 工具结果、用户消息、上下文片段三个入口的整次执行，每个请求都用 llm.el 自己的组装函数加 `json-serialize` 编一遍；
- 从 SQLite 读回来的之前轮次；
- 没有编码器时的估算；
- llm.el 自己的请求体，只替换 plz。它断言 llm.el 本身不收代理对：llm.el 以后改了，这条会失败；
- 失败的记录、大小那一行、会话缓存。

**代价：** 一段 20,000 字符、需要修的结果，一次测量用了 24 到 45 毫秒，看里面装的是什么；时间随长度线性增长。

**验收后补的一处（上下文恢复的重发）。** 测试在 [`scripts/context-recovery-regression.el`](../../../scripts/context-recovery-regression.el) 里，叫 `context-recovery/a-resumed-request-that-cannot-start-says-why`：一次执行先做了一轮工具调用，请求被拒说太长，压缩之后重发的那个请求在发出前出错。它检查执行以 `failed` 结束，界面显示 `[Context recovery retry failed: CR-NO-CURL]`，`run.failed` 带 `:failure`，`:class` 是 `"request-not-started"`，`:phase` 是 `"tool-followup"`，`:reason` 是 `"CR-NO-CURL"`。在没有这处改动的副本上单独跑，它停在 `(should (equal "request-not-started" (plist-get failure :class)))`，拿到的是 nil：界面那句话已经对了，`:failure` 不在。改后通过。

**合并后的整套回归（验收时跑的）：** 今天的几项改动和验收时补的修正全部合并后，在一份不含 `.git` 和编辑器锁文件的副本上，跑了 `scripts/` 下全部 139 个定义了测试的脚本（共 9423 项）、Kernel check 和标准冒烟集，同时最多 4 个 Emacs。只有 `m5-regression` 的 3 项不过，这 3 项改之前就不过（见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。和改之前逐个脚本对比：原有的每个脚本，测试数和通过数都一样，只有 `context-recovery` 多了 1 项新测试；新增 5 个脚本，全过。Kernel check：`KERNEL-CHECK OK (0 fails)`；冒烟：`SMOKE OK (0 mismatches)`，基准没有改；M3 只跑不改：139 项，137 过，2 跳过。改之前那一遍是在机器很忙时跑的，有 4 个脚本偶发失败或卡住（`answer`、`feed`、`note`、`timer-brief`）；同样的测试单独重跑、以及合并后这一遍，都通过。

**没有验证的：**
- 没有接真实模型跑；
- 没有在用户自己的 Emacs 里试；
- MCP 的 JSON 解析器不在测试环境里，没有核实它会不会交出发不出去的字符。

## Alternatives considered

**在每个工具里各自清理文字**（`eval_elisp`、`run_shell`、`read_buffer`）。输在入口太多：工具结果、用户消息、上下文片段、从 SQLite 读回的历史、Step 插入的材料。只修其中一处，正是八月那次修复之后这个问题又回来的原因。那次修复只在一个关口管了一种形状。

**只扩展工具关口。** 输在用户消息和上下文片段不经过它。用户消息里有代理对、从名字里有代理对的 buffer 开始对话，执行都在任何请求发出之前就结束了（已复现，0 个请求）。

**记录时就清理，让 Tape 里只有干净的文字。** 输在 Tape 按用户打的原样保存输入；能不能发出去是请求的性质，不是记录的性质。工具关口是原来就有的例外，那里本来就要让 Tape、界面、模型拿同一段文字。

**什么都不修，只用一句清楚的错误结束执行。** 输在代理对是一个完整的字符，能无损合回去。不修的话，群名里一个 emoji 就会让每一次读到它的执行都结束。实在编不了的，照样结束执行，带上类别和原因。

**每换掉一个字符，就给模型加一句说明。** 输在 U+FFFD 本身已经说明这里原来有东西；加说明要花 token，还会改变记下来的工具结果。

**失败以后再修：找出编不了的那条消息，替换，告诉模型，再发。** 这是第 3 条建议的原话，也是评估最初向用户提的做法。输在关口已经在每次尝试之前修好了文字：对关口经过的文字，第二条路永远不会走到；关口没经过的地方，又没有一条"消息"可以替换。这和第 3 条的原话不同：修在发送之前，不在失败之后；不重发；告诉模型的只有 U+FFFD。错误类型和 `run.failed` 上的类别、阶段、原因，照第 3 条做了。

**照旧打印 prompt，只是绑上 `print-circle`。** 输在为了数字数要打印十万多个字符的闭包（2026-11-09 那个请求是 108,991 个），量的还是错的东西：是 Emacs 打印工具的样子，不是请求的大小。

**失败的文字不再当作助手的发言，换一个角色或者不发。** 输在 2026-09-23 的决定要模型在那里读到失败文字；而且工具结果之后的消息顺序必须对模型服务商仍然合法，换角色或丢掉一条消息都可能破坏它。改成措辞写明是 superchat 说的。这收窄了第 3 条最后一句（"系统报错不再存成助手发言"）：文字仍然存成助手的发言，只是不再写成某个库或模型说的。

## Consequences

- 工具结果、用户消息、上下文片段里有代理对时，发给模型的是带 emoji 的文字，Tape 的 `tool.completed` 行记的就是发出去的那一段。2026-11-09 那种情况（`eval_elisp` 的结果里有代理对）在脚本测试里能正常完成。
- 文字里有原始字节或孤立的半个代理时，模型收到的是 U+FFFD，不另外告诉它。
- 本来就能发的文字，每次尝试每段文字多一次 `json-serialize`。
- 仍然组不出来的请求（比如工具自己的说明发不出去，或提供方选项有问题），执行以 `[Superchat could not start this request: …]` 结束，`run.failed` 写明原因。
- 上下文恢复后重发的请求在发出前出错时，这一轮仍然写 `[Context recovery retry failed: %s]`：这句话说的是发生了什么，没有换。`run.failed` 现在和另外八处一样带 `:failure`，由 `superchat--llm-local-failure` 给出，`:class` 是 `"request-not-started"`（或 `"request-not-encodable"`），`:phase` 是被拒的那次尝试的理由。`quit` 那一支不变：quit 是用户自己停下，不是请求组不出来。
- 从 Tape 重拼出来的请求（`superchat-tape-request-evidence`），文字也是修过、真正发出去的那一份；Tape 里的行仍是用户打的原样。见[消息清单的决定](2026-11-09-a-message-list-names-its-messages.md)第 8 条。
- 标准冒烟夹具在 llm.el 编码之前就替换了它，抓不到这一类问题；这一类由 `scripts/wire-safe-regression.el` 管，运行命令见[开发说明](../../agents/development.md)。
- [架构说明](../../architecture.md)写明了关口在哪里、`run.failed` 带什么。
