# Decision: 每次请求的消息清单只记消息在哪一行，不再把正文抄一遍

Status: accepted — 用户 2026-11-09 对评估里的七条建议说"同意建议"。本条是其中第 7 条（查清 488 条告警）查出来的修法。验收时又找到三处缺口，同日按验收方的设计补上（下文第 6 到 8 条）。第 7 条建议的是查清原因；修法（消息清单按身份记一条消息，不抄它的正文）是验收方的设计，2026-11-09 已报告给用户，用户还没有答复。

本条修改 [请求头的决定](../implemented/2026-09-13-reconstructable-provider-request.md) 和 [Step 材料的决定](2026-09-23-request-evidence-includes-step-material.md) 里消息清单的写法。

## Problem

**用户这边发生了什么：** 对话一长，Superchat 就没法事后说清"某一次到底给模型发了什么"。每次这样，它都在 Tape（记录一切的数据库）里写一条告警。可这些告警没有任何代码去读，也没有人看到。2026-10-01 以来，用户的 Tape 里攒了 488 条，分布在 153 段对话里，没有一条写着是哪一次执行（Run）发出的。

**机制：** 每次把请求发给模型之前，Superchat 写两条记录：

- `request.header`（请求头）：系统提示和工具说明。
- `request.message-list`（消息清单）：这次请求带了哪些消息、按什么顺序。

写完之后有一条检查叫 `prompt-reconstructable`，它只用这两条记录把请求重新拼出来。拼不出来，就写一条 `invariant.violated`，这就是上面说的告警。

我把每条告警和它前面最近的那条消息清单、那条请求头对上，再看告警自己写的原因。Tape 是只读打开的。488 条分成三类：

| 原因 | 告警里写的 | 条数 | 对话数 | 时间（UTC） | 现状 |
|---|---|---|---|---|---|
| A. 消息清单超过 `superchat-tape-max-inline-bytes`（16384 字节） | `no message-list row for request` | 127 | 5 | 10-03 到 11-06 | 还在发生；本条修掉 |
| B. 请求头超过同一上限（G16） | `header payload was bounded away; schemas not recorded` | 219 | 8 | 10-13 到 10-22 15:07 | 2026-10-23 的提交 `26dc86ba` 已结束 |
| C. 执行提交时没带请求编号 | `no message-list row for request` | 142 | 142 | 10-07 到 11-01 | 第 6 条修掉；已有的 142 条留在 Tape 上，是历史 |

有两段对话同时有两类，所以对话数加起来是 155，不是 153。

**A：清单太大，只剩一个指纹。** Tape 碰到一条超过 16384 字节的记录，只留它的大小和哈希，也就是一个指纹（`superchat-tape--encoded-payload`）。指纹上没有请求编号，检查就找不到这次请求的清单。

清单之所以大，是因为 `superchat-tape--request-evidence-item` 对工具调用和 Step 材料只记"是哪一行"。其余的每条消息，它都把正文整段抄进清单。于是同一句话，会被抄进之后每一次请求的清单里。

2026-10-01 以来，1389 条清单里有 127 条只剩指纹，每条 16432 到 24017 字节，后面都跟着一条告警。对话 `20261105-233040-046d` 里，168 条清单有 50 条被截成指纹，有 50 条告警。

抄的正文其实从来没用上。重拼请求时，每条消息的正文都是从它所指的那一行读的，不读清单里的抄本。

**B：请求头太大（已结束）。** `26dc86ba` 之前，请求头过了上限也只留指纹。消息清单是完整的，但请求头里没有工具说明，重拼报 `unknown`。

Tape 里有 281 条请求头是这样存的，从 2026-10-13 00:33 到 2026-10-22 15:07，之后一条也没有。219 条 B 类告警在同一分钟结束。

对话 `20261021-235246-08f5`（65 条清单、65 条告警）只有 B 类。`20261021-233047-cb1e` 是 53 条 A 加 36 条 B。那次提交之后，Tape 上的告警是 80 条 C 和 50 条 A，没有 B。

**C：没带请求编号（误报，第 6 条修掉）。** 有的调用方直接调 `superchat-runtime-submit`，不带 `:request-id`。这时请求上下文里没有编号，`superchat-tape-request-record` 就用 `"request"` 代替（`(or request-id "request")`），检查却拿 nil 去找，所以找不到。

清单本身是完整的，能重拼，所以这是误报。用户 Tape 上每一次来源为 `supertag` 的执行（共 142 次，最近一次在 2026-11-01）都恰好产生一条。用脚本化的批处理 Emacs 照样提交一次，在现有代码上能复现。

**为什么告警没有执行编号：** `superchat--llm-record-request-evidence` 调 `superchat-invariant-check` 时，执行编号那个参数写死成 nil，而它手里的请求上下文有 `:run-id`。`superchat-pipeline.el` 里提交前的那次检查也传 nil，因为那时执行还不存在。

## Decision

用户 2026-11-09 对第 7 条建议"查清 488 条告警"说"同意建议"。本条记下上面的分类，和针对 A 的修法：

1. **消息清单只说"由哪些行组成"。** 一条消息指向本对话（Session）里的某一行时，只记是哪一行，不记正文：

   | 消息种类 | 清单里记 | 重拼时从哪读正文 |
   |---|---|---|
   | Step 材料 | `:kind "step-material"`、`:storage-id`（它的 `attachment.added` 行）、`:form`（原样不变） | 那一行的内容 |
   | 工具调用 | `:kind "tool-observation"`、`:storage-id`、`:source-ids`（发起和结果两行）、`:lossy`（原样不变） | 那两行 |
   | 原始消息：用户或助手的话、系统事实、历史里收进来的材料 | `:class "raw-fact"`、`:kind`、`:storage-id` | 那一行的内容；收进来的材料从它的 `attachment.added` 内容读 |
   | 压缩摘要（Anchor） | `:class "model-interpretation"`、`:storage-id`（摘要那一行）、`:source-ids`、`:lossy` | 摘要那一行的内容 |

   压缩摘要也有自己的一行，就是 Anchor 那一行，所以同样只记身份。重拼的代码本来就从这些行读，不用改。

2. **别的对话的消息、没有行号的消息，照旧连正文一起记。** 重拼只读本对话的行，读不回别处的正文。

3. **版本号。** `superchat-tape-request-message-list-schema` 改成 2。它的说明写明：版本 1 的条目带正文，版本 2 不带。`superchat-tape-request-template-version` 保持 2：拼出来的样子没变，重拼也是按这个版本号分路的。改之前写下的清单带着正文，照旧能重拼，不要求它们是新格式。

4. **清单仍可能超限，超了照旧报出来。** 按用户那种大小的收据算，约 270 条原始消息，或 215 条工具调用，或 183 条标了 lossy 的工具调用，就会填满 16384 字节。用户 Tape 上最长的清单是 49 条。超限的清单照样只存指纹，检查照样报 `no message-list row for request`，不藏起来。

5. **告警写上执行编号。** 检查把手里的 Run id 传进去，告警就能说出是哪一次执行。

下面三条是验收时找到的缺口，2026-11-09 补上。

6. **每次执行都有请求编号（修掉 C）。** 请求进 `superchat-runtime-submit` 时没带 `:request-id`，Runtime 就给它一个，像它本来就会给没带 Session id 的请求补一个 Session id 那样。
   - 编号用 Runtime 自己造编号的函数 `superchat-runtime--new-id`，前缀是 `req`，看上去像 `req-20261109-150416-320155-2e3d`。
   - 只在入口补一次，补在请求自己的副本上，调用方手里的那份不动。之后 Step 记录（`step.started`、`step.ended`）、传输层拿到的请求上下文、请求证据、检查，读到的都是同一个编号。
   - 带了编号的请求，编号原样不变。
   - `superchat-tape-request-record` 里的 `(or request-id "request")` 留着：它服务的是既没有 Runtime、也没有编号、直接调这个函数的调用方。
   - 用户 Tape 上已经有的 142 条 C 类告警不删也不改，它们是当时发生过的事。改之后，不带编号提交的执行不再产生这种告警。

7. **检查重拼的是它跟着的那一次尝试。** 原来检查调 `superchat-tape-request-evidence` 时不带 Step，于是重拼的是这个请求里最新的一条**完整**清单。一次执行的每次尝试都用同一个请求编号记清单；如果这一次的清单超限只剩指纹，而同一次执行更早的一次是完整的，检查就拿早的那条，误判为通过。
   - 现在 `superchat--llm-record-request-evidence` 把这次尝试的 Step 也交给检查，`prompt-reconstructable` 再把它交给 `superchat-tape-request-evidence`，只重拼这个 Step 的清单。
   - 还没有 Step 的尝试，照旧不按 Step 筛。

8. **重拼出来的请求，文字就是发出去的那一份。** 传输层在每次尝试前把每段文字过一遍 `superchat-wire-safe-text`（见[组不出请求的决定](2026-11-09-a-request-that-cannot-be-encoded.md)），Tape 里的行却按用户打的原样保存。于是用户打的 `ab` 加一个代理对的两半，在下一次请求的历史里发出去的是 `ab` 加一个 emoji（字符码 `97 98 128681`），从 Tape 重拼出来的却是 `ab` 加六个原始字节（`97 98 4194285 4194208 4194237 4194285 4194234 4194217`）：既不是发出去的样子，`json-serialize` 也不收。
   - 现在 `superchat-tape-request-evidence` 在出口把重拼出的系统文字和 `:messages` 里的每段文字都过一遍 `superchat-wire-safe-text`。只在这一处做，所以两种模板版本的重拼路径都管到。
   - 本来就能发的文字原样返回，是同一个对象，什么都不变。
   - Tape 的行不改，仍是用户打的原样。

## 验证

**新测试** `scripts/request-evidence-regression.el`，前缀 `request-evidence/`。用标准测试夹具、脚本化的模型、真实的 SQLite Tape，并用 llm.el 自己的 OpenAI 请求构造器取下每次真正发出的消息：

| 测试 | 证明什么 | 改之前（在改动前代码的副本里） |
|---|---|---|
| `a-long-conversation-keeps-every-message-list` | 四轮对话里，之前的用户和助手消息加起来超过上限；每条清单都完整存下，没有告警 | 不过：清单被存成 `(:omitted t :size 18880 …)` |
| `every-request-rebuilds-as-it-was-sent` | 每次请求从 Tape 重拼出来的消息，和真正发出的逐条一致 | 不过：重拼得到 `unknown` |
| `each-item-kind-is-named-and-rebuilds-the-same` | 压缩摘要、系统事实、收进来的材料、用户和助手消息都只记行号；带正文的旧格式（schema 1）清单也重拼出同样的结果 | 不过：条目里还有 `:content` |
| `a-list-over-the-limit-is-reported-with-its-run` | 仍超限的清单照样报告，告警写着执行编号 | 不过：告警的执行编号是 nil |
| `an-item-of-another-session-keeps-its-text` | 别的对话的消息带正文记下 | 通过：这是守护测试，改前改后都过 |

旧格式那一部分在改之前本来就能过；那个测试在改之前不过，是因为新格式那一条断言。

**改后在本树上跑过**，每次最多三个 Emacs，每个限时 900 秒：

- 相关脚本全过：`history-messages`、`m5-history-material`、`context-recovery`、`tool-compaction`、`tool-observation`、`user-saves`、`m5-request-context`、`finish-check`、`large-header`、`step-material`、`m5-execution`、`long-history`、`request-evidence`。
- 内核检查：`KERNEL-CHECK OK (0 fails)`。
- 标准 smoke：`SMOKE OK (0 mismatches)`，没有改任何保存的基准。
- M3（只跑不改）：`Ran 139 tests, 137 results as expected, 0 unexpected, 2 skipped`。
- `superchat-tape.el` 和 `superchat-llm.el` 编译到临时目录：警告和改之前一样多、内容一样。

**C 类的复现：** 直接用 `superchat-runtime-submit` 提交、不带 `:request-id`、来源设为 `supertag`。在改前和改后的代码上都得到一条 `no message-list row for request`。改后这条告警带着执行编号。第 6 条之后，同样的提交不再得到告警（下面的 `a-run-submitted-without-a-request-id-is-given-one`）。

**第 6 到 8 条补的测试**，在同一个文件里。"改之前"是在今天四项改动合并之后、第 6 到 8 条之前那棵树的副本上跑的：副本里只有新测试，没有第 6 到 8 条的代码改动。

| 测试 | 证明什么 | 改之前 |
|---|---|---|
| `a-run-submitted-without-a-request-id-is-given-one` | 直接提交、不带 `:request-id`、来源 `supertag` 的执行：正常完成，没有告警；`step.started`、`request.message-list`、`step.ended` 带同一个非空、不是 `"request"` 的编号；用这个编号重拼得到 `reproduced` | 不过：`(should-not (re--events session "invariant.violated"))` 拿到一条 `invariant.violated` |
| `a-request-that-brought-its-id-keeps-it` | 自己带了 `:request-id` 的请求，三行都用它，重拼得到 `reproduced` | 通过：守护测试 |
| `the-check-rebuilds-the-attempt-it-follows` | 一次执行两次尝试（一个工具调用，然后是它的后续请求），只让第二条清单超限：记下一条告警，写着这次执行的编号 | 不过：`(should (= 1 (length violations)))`，实际是 `(= 1 0)`，一条告警都没有 |
| `a-rebuilt-request-carries-the-text-that-was-sent` | 用户打 `ab` 加代理对的两半；下一次请求从 Tape 重拼出的这句话，等于那次请求真正发出去的 `ab🚩`，`json-serialize` 收它；Tape 那一行仍是原样 | 不过：`(equal "ab🚩" "ab\355\240\275\355\272\251")`，长度 3 对 8 |
| `a-rebuilt-request-of-sendable-text-is-unchanged` | 文字本来就能发时，重拼结果和不过 `superchat-wire-safe-text` 的结果一样 | 通过：守护测试 |

**第 6 到 8 条改后在本树上跑过**，每次最多三个 Emacs：

- `request-evidence` 10 个、`wire-safe` 21 个、`context-recovery` 7 个、`history-messages` 23 个、`large-header` 4 个、`step-material` 8 个、`m5-execution` 62 个、`finish-check` 16 个、`run-actor` 155 个、`own-place` 115 个、`m5-request-context` 11 个、`user-words` 34 个、`tool-observation` 9 个、`user-saves` 31 个，全过。
- M3（只跑不改）：`Ran 139 tests, 137 results as expected, 0 unexpected, 2 skipped`。
- 标准 smoke：`SMOKE OK (0 mismatches)`，没有改基准。内核检查：`KERNEL-CHECK OK (0 fails)`。
- 改过的四个文件（`superchat-runtime.el`、`superchat-llm.el`、`superchat-tape.el`、`superchat-invariant.el`）编译到临时目录，和改之前一样是 15 条警告，内容一样。

**合并后的整套回归（验收时跑的）：** 今天的几项改动和验收时补的修正全部合并后，在一份不含 `.git` 和编辑器锁文件的副本上，跑了 `scripts/` 下全部 139 个定义了测试的脚本（共 9423 项）、Kernel check 和标准冒烟集，同时最多 4 个 Emacs。只有 `m5-regression` 的 3 项不过，这 3 项改之前就不过（见 [2026-10-26 的记录](2026-10-26-eval-elisp-calls-tools.md)）。和改之前逐个脚本对比：原有的每个脚本，测试数和通过数都一样，只有 `context-recovery` 多了 1 项新测试；新增 5 个脚本，全过。Kernel check：`KERNEL-CHECK OK (0 fails)`；冒烟：`SMOKE OK (0 mismatches)`，基准没有改；M3 只跑不改：139 项，137 过，2 跳过。改之前那一遍是在机器很忙时跑的，有 4 个脚本偶发失败或卡住（`answer`、`feed`、`note`、`timer-brief`）；同样的测试单独重跑、以及合并后这一遍，都通过。

**没有验证的：** 没用真实模型，也没在用户正在用的 Emacs 里看过。没有跑整个 `scripts/` 目录。`supertag` 这个调用方在哪个包里，没有找到它的源码。"每次 supertag 执行都走直接提交"是从 Tape 推断的：142 段 supertag 对话都有 `intent.received`，`prompt.assembled` 一条也没有，而走 `superchat-pipeline.el` 的提交会写它。这一点没有在那个包里核实。第 6 条只用脚本化的提交验证过，没有用那个包本身跑过。

## Alternatives considered

**消息清单无论多大都整条存下（像请求头那样加 `:keep-payload`）。** 改一行就能消掉 A。没选，因为两种记录写的次数差得远：请求头一个系列只写一次，消息清单每次尝试都写，而且每次都把之前所有消息再抄一遍。n 次请求的对话就会把历史存大约 n 遍；`20261105-233040-046d` 有 168 条清单，长的每条约 21 KB。Tape 会被它已经有的正文越撑越大。

**调高 `superchat-tape-max-inline-bytes`。** 只是把 A 开始发生的地方往后推，不能消掉它：对话再长一点，多高的上限都会越过。这个上限 Tape 上所有记录共用，为一种记录调它会改变全部记录。

**正文照抄，接受长对话丢证据。** 什么都不用改。没选，因为丢证据的正是长对话，而长对话里请求最容易出错、也最难说清。超过那个点之后，每次请求还要多写一条没人读的告警。何况抄进去的正文重拼时根本不读。

**C 类只在记录证据的地方补名字：`superchat--llm-record-request-evidence` 没有请求编号时，用执行编号给证据起名。**（第 6 条没选的做法。）它也能消掉告警，但这次执行的 Step 记录里请求编号仍然是 nil，"请求证据连到它所在的 Step"就只有一个方向成立：从证据找得到 Step，从 Step 找不到证据。

## Consequences

- 长对话的每次请求都能从 Tape 重拼。
- 新格式的清单依赖它所指的那些行。旧格式其实也一样：重拼从来不读抄进去的正文。
- C 类误报不再发生：不带 `:request-id` 提交的执行，由 Runtime 补上编号（第 6 条）。用户 Tape 上已有的 142 条留着，最后一条是 2026-11-01 的；读告警的人要知道它们是误报。
- 检查重拼的是它跟着的那一次尝试的清单（第 7 条）。这次的清单被截成指纹时，同一次执行更早的完整清单不再替它过关。
- 重拼出来的请求，文字是发给模型的那一份（第 8 条）；Tape 的行仍是用户打的原样。两者只在文字发不出去、被修过的地方不同。
- 还没有代码读 `invariant.violated`。要不要读、怎么给人看，另做决定。合适的位置是 `superchat-pending`（`superchat-pending.el`），也就是列出需要用户注意的事的那张单子。
- 进度见 [roadmap](../../roadmap.md)。
