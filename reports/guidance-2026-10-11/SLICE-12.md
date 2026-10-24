# 切片 12：推迟的活只等用户；被截断的回复一个调用都不跑

日期 2026-10-21。用户裁定（10-21）："按照建议"——两件都做：
1. 那句话写进产品 `work_record` 的说明；
2. 模型回复被输出上限截断时，那批工具调用一律不执行。

基线 HEAD `f725e3b3`。切片 11 已落地：上限 120、工具循环两个硬钩子、连续 3 次失败硬刹车。

总约束不变：每个新机制都有自己的 `defcustom` 关闭开关；只改给模型的文字时要说明改了哪一句。

## 先说我查实的事（不用再查一遍，但要引用）

- **F1** 那句话在 `.scratch/task-sim/harness-tools/ts-defer-wording.el`，原文：
  `When the user says to do the rest later, register wait_for user with what is left in detail; Superchat will not go on by itself.`
  测试台结果（`evidence/slice-11d-k2c.md`）：K2C 加句前 2/3、加句后 3/3，T4W 3/3；每边 3 次，是方向不是测量。
- **F2** 那句话里"Superchat will not go on by itself"在今天的产品里是真的，链条如下：
  模型 `wait_for user` → 本次执行结束时 `superchat-work--after-execution`（`superchat-work.el:1425` 起）走"Done, or waiting: the rounds are over"→ `superchat-work-pause`；
  用户下一条任何消息 → `superchat-work--on-user-input` → `superchat-work--answered`（只清 `waiting-user`）→ 记录变成 `continuable`，但 `superchat-work-continuing-p` 仍为 nil；
  只有 `/work resume`（`superchat-work-continue`）或新的 `start` 才重新开轮。所以无关的一句"谢谢"不会让 Superchat 自己把推迟的活干了。这条没有测试钉住，12a 要钉。
- **F3** 截断的回复今天怎么处理（都是代码事实）：
  - (a) 截在某个调用的参数中间：llm.el `llm-provider-utils-parse-openai-tool-arguments` 对不完整 JSON 发 `llm-tool-call-error`，发生在任何调用执行之前；`superchat-llm.el` 的 error-cb 把它当"模型可纠正"（`superchat--llm-tool-protocol-error-p`），丢掉悬空记录、附一句纠正（`superchat--llm-tool-protocol-error-feedback`："The tool arguments were not complete valid JSON. Retry with shorter arguments…"）重发，最多 `superchat-llm-unknown-tool-retries`（默认 2）次；Runtime 把这一步记成 `failed`（`superchat-runtime.el:3783`）。**所以"半截参数被执行"今天不会发生**（这句是代码事实：解析在执行之前）。"模型收到纠正并重试"这半句只是读代码得来的：`superchat-llm.el:1357` 附近的注释说，响应处理途中发出的条件到 error-cb 时只剩 `error` 加一句话；若流式路径上 `llm-tool-call-error` 也是这样到达，`superchat--llm-tool-protocol-error-p` 就认不出，今天的实际行为会是"Run 失败、不重试"。12b 的实机运行要记下截断时到达 error-cb 的到底是哪个类型、什么消息，报告里写明。缺的是：模型不知道原因是输出上限，可能原样再发一次超长调用；Tape 里没有一条"回复被截断"。
  - (b) 截在两个完整调用之间、或完整调用之后：这些调用照常执行，什么都不记。
  - (c) 只有文字、没有调用的回复被截断：按正常答案收尾，Run 正常结束，什么都不记。
  - Superchat 现在只在上下文溢出判断里看 `:stop-reason`（`superchat--llm-context-overflow-response-p`，`superchat-llm.el:1167`），流式 chunk 里认得这个键但不用（`:3145`）。
- **F4** `:stop-reason` 从哪来：用户实际加载的是 `~/Documents/emacs/package/llm/`（`nova-ai.el:100`，0.31.3），不是 elpa 里的 0.31.1。那份目录里 `llm-openai.el`、`llm-provider-utils.el`、`llm.el`、`llm-test.el` 有**未提交的工作区改动**，加了 `llm-provider-extract-stop-reason`、openai-compatible 流式 handler 发 `:stop-reason`（`length`/`stop`/`tool-use`/`content-filter`）、以及 `llm-context-length-exceeded`。Superchat 的溢出检测已经依赖这份改动。
  但用户用的 provider 是 `make-llm-deepseek`（`nova-ai.el:146`，流式开着），`llm-deepseek` 有自己的流式 handler（`llm-deepseek--get-partial-chat-response`），**不发 `:stop-reason`**。非流式路径 `llm-provider-utils-extract-all` 会带上（deepseek 继承 openai-compatible 的 extract 方法）。
- **F5** 回归测试用的假网络在 `scripts/smoke.el`：`smoke--deliver` 自己拼结果 plist（`:text`/`:tool-uses`）直接喂 `llm-provider-utils-process-result`，绕过 llm 的传输层；加一个 `:stop-reason` 字段是一行的事。它测不到 F4 那层，F4 那层靠一次实机运行。
- **F6** 可用的缝：`llm-provider-utils-process-result`（defun，流式和非流式都经过）拿到的 partial-result 里同时有 `:stop-reason` 和 `:tool-uses`，此时文字还没附进 prompt、一个调用都没跑，而且 error-callback 就在手上。`superchat--llm-capture-provider-tool-uses`（2026-09-27 的"ask_user 必须单独一批"就是在这儿拒的）看不到 stop-reason。纠正文字在 error-cb 的 protocol-error 分支选（`superchat-llm.el:3300` 起）。

## 12a 那句话进产品

- `superchat-tools.el:2248` `work_record` 的 `:description` 末尾加 F1 那句，一字不改，别的文字不动。
- 钉住 F2 的测试（预期一到就绿；如果红了，那是发现，报告给我，不要改产品去凑）：开一条记录、模型 `wait_for user`、用户一条无关消息、模型只回文字 → 没有 `work.round`、没有 `queue.queued`；记录是 `continuable` 且 `superchat-work-continuing-p` 为 nil；之后 `/work resume` 能开下一轮。用 `scripts/work-record-regression.el` 现有的 `hm--turn`/fixture 写法。
- 说明文字的断言：在 `work-record/the-two-ways-to-wait-for-the-user-are-told-apart` 里加一条，或另起一个测试。`scripts/work-record-regression.el` 不在 M3 集里（`scripts/m3-regression.el` 不加载它），你可以改。
- 测试台：`ts-defer-wording.el` 现在是无条件追加，产品有了这句之后再跑会重复一遍。改成"说明里已含这句就不加"（幂等），保留文件，11d 的证据还能对上。
- 文档：`docs/roadmap.md` 里 f725e3b3 加的那条 open observation 改成已裁定（日期 2026-10-21，提交号），不要删。`docs/decisions/accepted/2026-10-11-goal-rounds.md` 只在一张表里引了"…worked on in rounds…"，没有逐字引全段，不用动；发现别处逐字引了再说。
- 停点一：报告 + diff，等我核对再提交。

## 12b 被截断的回复一个调用都不跑

一句话的规则：provider 说这条回复在输出上限处被截断（`:stop-reason` 是 `length`），这条回复里的工具调用一个都不执行；Superchat 告诉模型原因，让它分段重来；连着被截断到了次数，执行停下并说明原因。每一次都记在 Tape 上。

- 开关：`defcustom`，默认 t；nil 就是今天的行为（F3）。名字由你起，要像这个项目的其它开关一样是人话。
- 判断只认 `:stop-reason` 等于 `length`。没有 `:stop-reason`（库或 provider 不给）就什么都不做，和今天一样；不许从文字或 JSON 长度去猜。
- 三种情况分开处理：
  - (A) 截在参数中间，以 `llm-tool-call-error` 到达 error-cb：这次请求的 stop-reason 是 `length` 时，纠正文字换成截断的那句（下面），不再说"invalid JSON"。stop-reason 要按 prompt 记下（流式 chunk 里的 `:stop-reason` 先于 on-success 到达；按 `superchat--llm-raw-tool-uses` 那样以 prompt 为键）。
  - (B) 截断但每个调用的 JSON 都完整：在任何文字附进 prompt、任何调用执行之前拒掉整批。建议的缝是 `:around` `llm-provider-utils-process-result`：看到 `length` 且有 `:tool-uses` 且开关开着，直接调它拿到的 error-callback、带一个 superchat 自己的错误类型，不调原函数；error-cb 把这个类型当模型可纠正（加进 `superchat--llm-tool-protocol-error-p` 或旁边）。你要核实 `superchat--llm-drop-dangling-tool-use` 在"什么都没附上"时是空操作（它看最后一条交互的角色和内容，应该是）。
  - (C) 只有文字的回复被截断：不纠正、不重发，答案照旧算答案（收尾规则这次不改，没裁定）；但要写事实，答案下面加一行让用户看到回复是被截断的。
- 给模型的纠正文字（英文，一处定义）：
  `Your reply was cut off at the output limit (N tokens), so none of its tool calls ran. Do the same work in smaller pieces: fewer calls per reply, and split long content over several calls.`
  Superchat 发了 `max_tokens`（`superchat--llm-request-max-tokens` 或 preset 的 `max_tokens`）时 N 是那个数；没发就写 `the provider's output limit`。
- 动手前两处核实，核实结果写进报告：
  - `tool-error-retries` 是请求闭包里的局部变量（`superchat-llm.el:2945`）。查它在工具轮次之间会不会归零：按 Run 累计就不能叫"in a row"，措辞跟着代码走。
  - `llm-provider-utils-streaming-accumulate` 没有符号值的分支，DeepSeek 每个中间 chunk 都带 `finish_reason: null`。确认 `llm-provider-utils-json-val` 把 `:null` 变成 nil、只有最后一个 chunk 贡献 `:stop-reason`；要是两个 chunk 都带，累加后会变成 nil，截断就漏检。
- 次数：复用 `superchat-llm-unknown-tool-retries`（默认 2，即第 3 次截断就停）。停下时和 11b 的停一个形状：Run 的结局事实、答案下一行、待办表一行，三处都写"Stopped: the reply was cut off at the output limit 3 times in a row"（数字按实际）。今天 protocol-retry 用完是 `failed` 收尾，你看哪条路能把三处都写全就走哪条。
- Tape：每次截断一条事实（名字你定，报告里写明；回复一级的事实归 Runtime，放在写 `response.completed` 的那层或 LLM adapter 的 observer 都行），字段至少有 `:stop-reason`、`:max-tokens`（或 nil）、`:tool-calls`（没跑的调用数）、`:text-chars`、`:attempt`。开关关着也写这条事实（这是看见，不是机制）。Tape view 一行能看出来。
- DeepSeek 那条路（F4）：实机要能看到截断，`llm-deepseek--get-partial-chat-response` 得像 openai-compatible 的 handler 一样读 `(aref choices 0)` 的 `finish_reason`、过 `llm-openai--normalize-stop-reason`、加 `:stop-reason`，三行。规矩：补丁先写到 `reports/guidance-2026-10-11/evidence/slice-12-llm-deepseek-stop-reason.diff`，只打到那份 fork 的工作区（那里本来就带着未提交的 stop-reason 改动），**不在那个仓库提交**，停点里单独说明；用户不要就撤掉。fork 的 `llm-test.el` 里已有 stop-reason 的测试，顺手加一条 deepseek 的，便宜就加。
- 红测试（假网络 `smoke--deliver` 加 `:stop-reason`；需要的话让 `:chunks` 也能带 plist，用来模拟"先到一个 stop-reason chunk 再出 parse 错"）：
  1. 两个完整调用 + `length` → 两个调用的工具体都没跑（没有它们的 `tool.requested`/`tool.completed`），下一次请求的 prompt 文本里有那句纠正，Tape 有截断事实且 `:tool-calls` 2。
  2. (A) 的形状：stop-reason `length` 之后以 `llm-tool-call-error` 到达 → 纠正文字是截断那句，不是 JSON 那句；没有调用跑。
  3. `stop`（或没有 stop-reason）+ 调用 → 照常执行，没有截断事实。
  4. 连着三次 `length` → 没有第四次请求；Run 结局、答案下一行、待办表一行三处齐全。
  5. 开关 nil → 调用照常执行，截断事实仍然写。
  6. 只有文字 + `length` → 事实写了、答案下面有那一行、Run 正常结束、没有纠正请求。
  7. `superchat-llm-streaming` 为 nil 走 `llm-chat-async` 那条 → 和 1 一样。
- 实机（先做清单第 0 步的数据目录核对，上次漏进过用户的真库）：临时 preset 把 `max_tokens` 设成 60（`superchat-preset.el:43`），让模型用 `edit_buffer` 往一个 buffer 写两千字 → 预期：截断、纠正、模型分段，或三次后停下。Tape 摘录进 evidence。
- 不做、不盖：Claude 的 `stop_reason` 和 OpenAI Responses 的 `incomplete_details`（fork 只规范化了 openai-compatible 一族），报告里写"未覆盖"；只有文字被截断时的收尾规则；**不用 11b 的逐调用钩子做这件事**——钩子在 llm 已经附上助手记录、进了包装之后才按调用触发，截断要在第一个调用之前、一个地方拒掉整批，pi 的提法里"钩子的第二个使用者"这句话不成立，报告里写明。
- 停点二：报告 + diff + 实机 Tape 摘录，等我核对再提交。

## 纪律

- 每项先红测试（12a 的钉住测试例外，预期绿），报告里逐条写翻红的断言。
- 变异测试；全量检查（Kernel、冒烟、M3 只跑不改、回归、编译警告）。
- 提交按路径限定，不带 `reports/`、实机清单、`.claude/`、`.scratch/`。fork 目录不提交。
- 报告写到 `reports/guidance-2026-10-11/evidence/slice-12-report.md`，两个停点各一节。
- 用户只在停点批准提交；我核对后转达。
