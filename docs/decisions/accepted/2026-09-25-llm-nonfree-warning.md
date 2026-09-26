# Decision: what superchat does about llm's "not free software" warning on every request

Status: accepted — user, 2026-09-25: "(setq llm-warn-on-nonfree nil) 我希望是这个，但这个配置项是 superchat 这边设置的所以，应该是 superchat-llm-warning-message 的开关". Not the proposal's option A: superchat gets its own switch. Issue `.scratch/issues/2026-09-25-llm-nonfree-warning-each-request.md`.

## Decision

- **A superchat option, `superchat-llm-warning-message`.** Non-nil (the default) leaves llm's warning to llm: it warns as `llm-warn-on-nonfree` says. Nil turns the warning off for superchat's own requests: while superchat makes a chat request, `llm-warn-on-nonfree` counts as nil. llm checks that variable at the moment of the call (its `:before` method on `llm-chat-streaming` / `llm-chat-async`), and superchat makes those calls in one place, so the option covers every request superchat sends and nothing else: other packages' use of llm, and llm's own setting, are unchanged.
- **Default on**, so llm's deliberate notice stays what a new user sees; the user who wants it gone sets `(setq superchat-llm-warning-message nil)` in their superchat configuration.

## 一句话

superchat 用的 llm 包，每向 DeepSeek、OpenAI 这类服务商发一次请求，就弹一次"这个 API 不是自由软件"的 `*Warnings*` 窗口；superchat 一次任务要发十来次请求，窗口关了又弹。这份提案问的是：这件事交给用户自己在配置里关（superchat 只在 README 里讲清楚），还是 superchat 替用户把它减到"每个服务商每次开 Emacs 只提醒一次"。

打个比方：一栋楼的门卫（llm）每次有人进出都要念一遍"本楼不是公共场所"。这句话是门卫的，不是我们（superchat）的；我们能做的是告诉住户"门卫这句话可以让他别念"，或者跟门卫商量"对我们家的客人只念第一次"。

## Problem

已查清的事实：

- llm 的声明：`llm-warn-on-nonfree`（llm 自己的用户选项，默认 `t`，说明是 "Whether to issue a warning when using a non-free LLM."）打开时，llm 在每次 `llm-chat`、`llm-chat-async`、`llm-chat-streaming` 调用之前（`:before` 方法，`llm-0.32.1/llm.el:408`、`:526`、`:599`，嵌入类调用另有几处）调用 `llm--warn-on-nonfree`，用 `(lwarn 'llm :warning "%s API is not free software, and your freedom to use it is restricted. See %s for the details on the restrictions on use." ...)` 发警告（`llm.el:57-68`）。函数自己的说明写着 "All non-free LLMs should call this function on each llm function invocation"——每次都警告是 llm 有意的设计。
- llm 的 README（"`llm` and the use of non-free LLMs" 一节）说明了原因（GNU ELPA 的包不应推广非自由软件，所以提醒用户），也说明了关法："You can turn this off the same way you turn off any other warning, by clicking on the left arrow next to the warning when it comes up. Alternatively, you can set `llm-warn-on-nonfree` to `nil`. This can be set via customization as well."
- 哪些服务商会触发：凡是给 `llm-nonfree-message-info` 返回了条款链接的。DeepSeek（`llm-deepseek.el:47`）、OpenAI（`llm-openai.el:120`）以及继承它的 OpenAI 兼容接口（LM Studio 就走这条，所以脚本排练里出现过 "Qwen 3.8 API is not free software"，链接却是 OpenAI 的条款）、Claude、Gemini、Azure 都会；Ollama 明确返回 nil（`llm-ollama.el:90-95`，注释说无法知道模型是否自由），不会。
- 警告类型是 `llm`。Emacs 自带的 `warning-suppress-types` 里加上 `(llm)`，警告仍记进 `*Warnings*` Buffer，但不再弹窗口；加进 `warning-suppress-log-types` 则完全不记（两者的说明文字见 Emacs 的 `warnings.el`）。
- superchat 只在一处调用 llm 发对话请求（`superchat-llm.el` 里的 `llm-chat-streaming` / `llm-chat-async`），每个工具轮一次。M10 第 1b 片的有人值守运行里一个阶段发了十个请求，`*Warnings*` 窗口反复出现；编辑模式下它让屏幕多出第三个窗口，审阅屏占了它的位置，从对话按一次 `C-x o` 到的是文件而不是审阅屏，一个本该给审阅屏的键落进了 `work.el`（该运行的 `REVIEW.md`）。
- superchat 自己的文字从没提过这条警告；用户自己的配置里也没有关它（2026-09-25 查过）。

## Proposal

**推荐 A：不改 llm 的默认，不改 superchat 的行为，在 README（中英文两份）的 Troubleshooting 里加一条。** 内容：这条警告来自 llm 包，是 llm 对服务商条款的声明，不是 superchat 的错误；superchat 每个工具轮发一次请求，所以默认会反复弹；两种关法由用户选：

```elisp
;; 完全不提醒（llm README 给的办法）
(setq llm-warn-on-nonfree nil)
;; 仍记在 *Warnings* 里，但不弹窗口
(add-to-list 'warning-suppress-types '(llm))
```

理由：这是另一个包的用户选项和它有意的声明，llm 的 README 已经把关法交给用户；由用户在自己的配置里决定，最不越界。代价：没读 README 的用户照旧每轮被弹一次。

## 参考实现怎么做

- **DeepSeek Harness**（`~/Documents/reference-code/deepseek-harness`，`c389f96`）：没有任何关于服务商许可或"非自由软件"的提醒（在全仓库搜 "free software"、"nonfree"、"non-free"、"terms of service/use" 都没有这类代码）。它对"会反复出现的警告"有一个做法：同一件事只警告一次——`subprocess-local` 的 `fallbackWarningIssued`（"Provider-lifetime latch suppressing repeated weaker-containment warnings"，`packages/subprocess/subprocess-local/src/index.ts:58-59`、`:215-216`），`compaction-basic` 按"服务商/模型"记住已经警告过的配置问题（`warnedPressureConfigTargets`，`packages/compaction/compaction-basic/src/index.ts:157-159`）。
- **Pi**（`~/Documents/reference-code/pi`，`3e4bc26`）：同样没有服务商许可提醒（同样的搜索没有结果）。
- 所以两个参考都没有可以直接对照的东西；DeepSeek Harness 的"每个服务商/模型只警告一次"是选项 B 的形式来源，但它管的是它自己的警告，不是别的包的声明。

## Alternatives considered

**B. superchat 让这条声明"每个服务商、每次开 Emacs 只出现一次"。** superchat 发请求前看 `llm-warn-on-nonfree`：是 nil，什么都不做；是非 nil，并且本次 Emacs 里还没为这个服务商（`llm-name`）提醒过，就照常让 llm 发这一次警告；之后 superchat 自己的请求在调用 llm 时临时把 `llm-warn-on-nonfree` 绑成 nil（只在这次调用里，别的包用 llm 不受影响）。声明的文字和链接仍是 llm 的，用户至少看到一次。没有选它作推荐，因为它改变了另一个包有意的行为（llm 说要每次都提醒），而 superchat 分不出用户是有意保留默认 `t`，还是没管；第一次请求仍然弹窗口。若用户选 B，可以再加一个 superchat 选项让想每次都看到的人关掉这个"只一次"。

**C. 什么都不做。** 保持现状，连 README 也不写。代价为零，但用户每次都被弹、也不知道这是哪里来的，编辑模式下的窗口问题照旧。没有选它，因为 A 的代价只是几行文档。

**D.（不建议）superchat 直接把 `llm-warn-on-nonfree` 设为 nil，或把 `(llm)` 加进 `warning-suppress-types`。** 这等于在用户不知情时替用户关掉了 llm 的声明，也改了影响其它包的全局设置；issue 里已经写明"不能不经用户同意就藏起这条声明"。列在这里只为说明它被考虑过。

## Acceptance criteria

- A：README.md 与 README_cn.md 的 Troubleshooting 各有一条，写明来源（llm）、为什么反复出现（每个工具轮一次请求）、上面两种关法，以及"是否关由你决定"。无代码改动，无基线变化。
- B（若选）：离线测试在假网络下向同一个非自由服务商连发三次请求，只计到一次 `llm` 警告；`llm-warn-on-nonfree` 为 nil 时一次也没有；换一个服务商再出现一次；调用结束后 `llm-warn-on-nonfree` 的全局值不变；别处直接调用 llm 仍每次警告。issue 的复现脚本数法可直接改成这个测试。

## Risks

- A：用户不读 README，体验不变；有人值守验收里 `*Warnings*` 窗口仍会出现，除非用户在自己的配置里关掉。
- B：superchat 动态改变另一个包的选项，llm 以后若改变警告的发出方式（例如不再经过这个变量），"只一次"会悄悄失效或变成每次；需要一条测试钉住。
- 两者都不改变 llm 对服务商条款的判断，也不替用户判断条款本身。

## Questions for the user

1. 这条"不是自由软件"的提醒怎么处理？**A**（推荐）：superchat 不动，只在说明书里写清楚是哪来的、怎么关，由你自己在配置里决定 / B：superchat 让它每个服务商每次开 Emacs 只出现一次 / C：什么都不做。
2. 你自己的配置要不要现在就关？这是你的配置，不属于 superchat 的改动：**是**（关掉，或只是不弹窗口）/ 否（保留默认，验收时照旧会弹）。
