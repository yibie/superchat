# tape.systems 与 Bub Tape：从理论声明到实际数据流

状态：源码复核稿。外部证据固定在 `bubbuild/bub`
[`20b8f9262e7da04544781fa79b2fe4db7cd789b1`](https://github.com/bubbuild/bub/commit/20b8f9262e7da04544781fa79b2fe4db7cd789b1)
（2026-08-06）与 `bubbuild/tape.systems`
[`69a0095f3cffeedd8fa5c065ee2a338d936aac78`](https://github.com/bubbuild/tape.systems/commit/69a0095f3cffeedd8fa5c065ee2a338d936aac78)
（2026-05-21）。线上 `tape.systems` 页面于 2026-08-10 读取，但关键网站声明均引用前述
固定 commit。Superchat 对照以同日当前工作树为准，其中 Context Continuity 文件含
未提交修改，不能伪装成远端固定版本。Akshay 的 X Article 与 Zep 官方文档是同日读取的
动态外部材料，本文明确把作者主张与官方可核实产品行为分开，不把它们伪装成固定源码。
OSS Graphiti 对照固定在 `getzep/graphiti`
[`425bf2481b51437e43455e09d241c5f46e3d95f3`](https://github.com/getzep/graphiti/commit/425bf2481b51437e43455e09d241c5f46e3d95f3)
（2026-08-05）；Zep Python SDK 接口证据固定在
[`d3619d047fba5c8f2a2a1da44ce0338ab7625eab`](https://github.com/getzep/zep-python/commit/d3619d047fba5c8f2a2a1da44ce0338ab7625eab)。

## 一句话结论

`tape.systems` 提供了一套很有用的历史与上下文语言：Tape 保存事实，Anchor 标记
重建边界，View 按任务组装上下文，Handoff 追加新边界而不删除旧事实。Bub 已实现
这套语言的一个小型可运行子集，但没有把网站写出的全部不变量变成代码约束：正常
聊天路径会顺序追加 JSONL，然而 `reset` 可以删除或替换主 Tape，`TapeEntry` 也只有
浅层冻结；网站中的图状 Memory、跨 Tape View、带来源的派生事实和训练出口则不是
`src/bub/tape.py` 已交付的能力。

这意味着 Superchat 值得借用的是**领域词汇和读写分离原则**，不是照搬 Bub 当前的
存储实现，也不能因一个类叫 `Tape` 或 `frozen=True` 就宣布“不可变、只追加”已经成立。
若目标是不改模型权重的 self-learning，最小闭环也不是“多写一些 Memory/Skill/Tape”，
而是把未来实际采用的 **View policy 与 Action policy 做成可版本化候选**，再把
Outcome、人工 feedback、反例与 promotion/rollback 判断作为新事实追加。

## 1. 取证方法与事实标签

本文只使用以下一手材料：

- `tape.systems` 固定 commit 的站点源码，以及 2026-08-10 的官方页面；
- Bub 固定 commit 中的源码、同仓文档与测试；
- Akshay @akshay_pachaar 的 X Article 与 Zep 官方 Episodes/Observations 文档；
- Graphiti 固定 commit 的源码与测试树（只做静态搜索，未运行其 provider/数据库测试）；
- Zep Python SDK 固定 commit 的 Observation read client 与返回类型；
- Superchat 当前源码与 ERT 测试。

文中结论分为四类：

- **网站声明**：`tape.systems` 或 Bub 官方文档写出的设计目标；
- **代码事实**：固定 commit 的控制流或数据结构直接执行的行为；
- **测试事实**：测试明确断言且本次实际运行通过的行为；
- **推论/未证实**：从代码事实推导的工程含义，或当前材料没有证明的能力。

源码事实的 GitHub 链接全部固定到上述 commit；没有用 `main` 浮动链接代替证据。
`tape.systems` 仓库自述其范围包含 core 与四类扩展，见
[`README` 第 1–24 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/README.md#L1-L24)；
该站点仓库没有测试目录，package scripts 只有 dev/build/start/lint，见
[`package.json` 第 1–10 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/package.json#L1-L10)。
所以站点可构建不等于它对 Bub 的实现映射已经通过行为测试。

本次在 Bub 固定 commit 实际运行：

```bash
uv run pytest -q tests/test_builtin_tape.py \
  tests/test_file_tape_store_entry_ids.py \
  tests/test_fork_store_merge_back.py \
  tests/test_tape_search_output.py \
  tests/test_builtin_model_runner.py
```

准确结果是 `17 passed in 0.41s`。这只是下列五个文件中的 17 个测试，不代表所有未写
性质都成立：

```text
tests/test_builtin_tape.py
tests/test_file_tape_store_entry_ids.py
tests/test_fork_store_merge_back.py
tests/test_tape_search_output.py
tests/test_builtin_model_runner.py
```

## 2. tape.systems 实际声明了什么

### 2.1 最小模型

`tape.systems` 固定源码把四个词定义为：

| 术语 | 网站声明 |
|---|---|
| Tape | 按时间排列的事实序列 |
| Entry | 不可变的单条事实记录 |
| Anchor | 用于状态重建的逻辑检查点 |
| View | 为某项任务组装出的上下文窗口 |

这些定义和三条总不变量——历史只追加、派生项不替换原始事实、上下文每次构造而非
整段继承——均见
[`lib/i18n.tsx` 第 55–95 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L55-L95)。
Bub 自己的固定 commit 文档也逐字承诺 Entry 不可变、历史不覆盖、Anchor 不是删除点，见
[`tape-and-context.mdx` 第 12–33 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/website/src/content/docs/docs/concepts/tape-and-context.mdx#L12-L33)。

### 2.2 Append、Anchor、Handoff

网站固定源码进一步声明：

- Append 是唯一改变事实集合的操作，只能写到 Tape 尾部；顺序由单调 ID 跟踪；
- 旧事实不能原地修改，纠正应追加而非删除；
- Anchor 不删除之前的历史，只给重建提供起点，并可携带结构化 state；
- Handoff 依次追加 Anchor、附带最少继承 state、把执行起点移到新 Anchor 之后。

Append 的原文证据见
[`lib/i18n.tsx` 第 97–114 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L97-L114)；
Anchor 与 Handoff 的固定证据见
[`lib/i18n.tsx` 第 116–152 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L116-L152)。
因而，“从 Anchor 之后读”只是 View 的选择规则，不表示 Anchor 之前的 Entry 被删除。

### 2.3 Index 是网站抽象，不是 Bub 当前实体

**网站声明。** 最小模型图明确画出 `Entries → Index (derived) → View`，见
[`minimal-model.tsx` 第 95–108 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/components/minimal-model.tsx#L95-L108)；
Memory 文案进一步要求 `derived index + raw reload`，并说索引命中后要回读原始事实，见
[`lib/i18n.tsx` 第 213–225 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L213-L225)。

**代码事实。** Bub 固定树中没有 `Index` 类型、物化表、checkpoint 或 index→raw
provenance。`tape.search` 只构造 `TapeQuery`，File store 读取 Entry 后做 substring/fuzzy
扫描并直接返回 raw `TapeEntry`，见
[`FileTapeStore.search`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L107-L172)
与
[`tape.search` tool](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tools.py#L271-L287)。
现有 search 测试还使用 fake query/store，只证明输出格式与 marker 过滤，不证明真实索引
或 raw reload。因此准确说法是“Bub 有 scan-based search”，不是“Bub 已实现网站的
derived Index”。

### 2.4 Task-as-View-Assembler / DAG 是网站架构，不是 Bub Runtime

**网站声明。** 固定站点源码把 Task 定义为特殊 View Assembler：装配 Task Schema、
Task Config、Task Harness 与 Task Description；DAG node 对应 topic 内的一次 Turn 边界，
Runtime projection 被写成状态事实源，见
[`lib/i18n.tsx` 第 251–320 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L251-L320)。
图中又列出 `task_init/node_init/node_fin/task_checkpoint/task_fin` anchors，见
[`dag-workflow.tsx` 第 287–347 行](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/components/dag-workflow.tsx#L287-L347)。

**代码搜索事实。** Bub 固定树中没有 Task Schema、Task Harness、DAG/topic Runtime、
`.feature` harness loader、上述 task/node anchors，也没有与网站 Runtime projection 对应的
状态投影合同。引入这套图文的
[`tape.systems` PR #6](https://github.com/bubbuild/tape.systems/pull/6)
只修改站点文件。Bub 的 AGENTS、skills 和 allowed tools 可以作为输入，但不能据此声称
Task-as-View-Assembler/DAG 已由 Bub 交付。

### 2.5 网站中的其他扩展模型不是自动交付物

网站还描述了以下更高层能力：

- Sessions：单 Tape 多轮、多 Session 隔离、topic
  绑定 Anchor；
- Strategies：compact 不删除历史，summary 引用
  来源，fork/merge 只追加 delta、不改写 mainline；
- Memory：Anchor 可组成非线性图，View 依策略从
  多节点组装，图边要有 lineage/provenance；
- Teams：共享 Tape 的 Entry 保留 origin，跨 Tape
  协作通过 View，原 Tape 仍隔离；
- Appendix：同一事实流可供访问控制、可观测、
  评测与训练使用，评分和标签应作为派生事实追加。

固定证据分别见
[`Sessions`](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L154-L179)、
[`Strategies`](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L181-L249)、
[`Memory/Teams`](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L326-L369)
与
[`Eval/Training`](https://github.com/bubbuild/tape.systems/blob/69a0095f3cffeedd8fa5c065ee2a338d936aac78/lib/i18n.tsx#L423-L457)。
尤其是 Training/RL 文案只说按 Anchor 切片、附 reward、导出 trajectory 交给训练层；它
是外部消费模式，不是 Bub 已交付在线 self-learning 的证据。

这些是模型声明。除非源码存在相应身份、图边、来源、策略和出口，不能从网页插图
反推 Bub 或 Superchat 已经实现它们。

## 3. `src/bub/tape.py` 的真实边界

### 3.1 `TapeEntry`：统一外壳，开放 payload

**代码事实。** `TapeEntry` 是 `frozen=True` dataclass，字段只有 `id`、`kind`、
`payload`、`meta` 和 `date`；`message/system/anchor/tool_call/tool_result/error/event`
工厂都先生成 `id=0` 的待存记录，真正 ID 由 store 分配。参见
[`TapeEntry` 与七个工厂](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L23-L68)。

这是一种“少量稳定外壳 + 任意字典 payload”的设计：

- 优点是消息、工具、Anchor、Runtime event 都能共用一个追加接口；
- 代价是 kind/payload 的内部 schema 没有版本、因果、actor、来源或 validation；
- `id` 是某个 store 中的数字位置，不是跨 store、fork 或复制后仍稳定的逻辑身份。

**代码事实，不是推测。** `frozen=True` 只禁止给 dataclass 字段重新赋值，并没有深冻
两个字典。`copy()` 和 store 都只做一层 `dict(...)`，见
[`copy()`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L33-L34)
及
[`InMemoryTapeStore.read/append`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L315-L325)。
本次离线复现：读取 `tool_call` 后修改嵌套 `calls[0]`，下一次读取已存 Entry 会看到
修改后的值。这与网站的“旧事实不能原地修改”不一致。

### 3.2 Store 合同：名字叫 append-only，但合同含 destructive reset

**代码事实。** 同步与异步 `Protocol` 都暴露四个操作：`list_tapes`、`reset`、
`fetch_all`、`append`，见
[`TapeStore`/`AsyncTapeStore`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L71-L93)。

所以这里的 “append-only” 精确含义只能是：**在没有调用 reset 的一段 store 生命周期
内，普通写入使用 append。** 它不是类型层面不可破坏的历史合同。

当前具体实现包括：

- `InMemoryTapeStore`：每个 tape 用 list 存 Entry、从 1 分配 ID；`reset` 删除整条
  list 和 next-id，见
  [`InMemoryTapeStore`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L301-L325)；
- `AsyncTapeStoreAdapter`：用 `asyncio.to_thread` 包装同步 store，并没有新增事务或
  不变量，见
  [`AsyncTapeStoreAdapter`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L328-L344)；
- `UnavailableTapeStore`：所有方法都抛同一个 `INVALID_INPUT`，是配置失败 sentinel，
  见
  [`UnavailableTapeStore`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L347-L366)。

`is_async_tape_store` 只检查 `append` 是否是 coroutine function，见
[`is_async_tape_store`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L95-L96)。
这是便利的运行时判别，不是对其余三个方法都满足异步合同的完整验证。

### 3.3 `TapeQuery`：不可变 builder，查询仍由 store 解释

**代码事实。** `TapeQuery` 自身是 frozen dataclass；`query`、`after_anchor`、
`last_anchor`、`between_anchors`、`between_dates`、`kinds` 和 `limit` 都通过
`dataclasses.replace` 返回新 query，`all()` 只把 query 交给 store。参见
[`TapeQuery`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L99-L143)。

`InMemoryQueryMixin` 的执行顺序为：

1. 先选 Anchor 范围；
2. 再按日期、全文 substring、kind 过滤；
3. 最后从结果头部做 limit。

范围选择的细节是：named `after_anchor` 取最后一个同名 Anchor；
`between_anchors(start,end)` 取最后一个 start 与其后第一个 end，且两端 Anchor 都不
进入结果；找不到边界就抛 `NOT_FOUND`。源码见
[`_anchor_index`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L190-L206)
和
[`fetch_all`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L252-L298)。

日期边界接受 ISO date 或 datetime；纯 date 的 start/end 分别扩到 UTC 当日最早/最晚，
naive datetime 被当作 UTC，见
[`_parse_datetime_boundary`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L209-L234)。
内存全文搜索把 `kind/date/payload/meta` JSON 化后做 casefold substring，并不是语义检索，
见
[`_entry_matches_query`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L237-L249)。

### 3.4 `TapeContext` 是 Bub 当前最接近 View 的东西

**代码事实。** `TapeContext` 只有 `anchor`、可选 `select` callback 和可变 `state`
字典。默认 anchor 是 `LAST_ANCHOR`；它负责改写 query，而 `build_messages` 再把查询
结果交给 selector；没有 selector 时只保留 `kind="message"`。参见
[`TapeContext` 与 build_messages](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tape.py#L146-L187)。

这实现了 View 的最小机制，但不是一个可审计的 View 实体：返回值只是 message list，
没有 view ID、选择策略 ID、covered/omitted range、source IDs 或 representation 关系。

**代码事实。** Bub 的默认 selector 知道如何把 Anchor、message、tool call/result 转成
模型消息，见
[`default_tape_context/_select_messages`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/context.py#L12-L64)。
但默认 `TapeContext` 会先查询“最后一个 Anchor 之后”，而切片排除了 Anchor 自身。
因此默认路径不会把最新 Anchor 的 state 交给 selector。本次构造
`message(before) → anchor(state={summary}) → message(after)`，`read_messages()` 实际只
返回 `after` message。Bub 文档所说“Anchor state supplies the minimum inherited state”
在默认数据流中没有被实现；相关文档声明见
[`tape-and-context.mdx` 第 27–33 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/website/src/content/docs/docs/concepts/tape-and-context.mdx#L27-L33)。

### 3.5 `handoff` 写入存在，但既不原子也不完成 state 继承

**代码事实。** `Tape.handoff` 先 append Anchor，再单独 append 一个 `handoff` event，见
[`Tape.handoff`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tape.py#L143-L155)。
这两个写入没有事务包围；第二次 append 失败时，第一个 Anchor 已可留在 Tape。因此它
实现了“写两个 Entry 并让默认 query 后移”，没有实现原子的 phase transition。
结合上一节的默认 View 行为，Anchor state 虽被保存，却没有作为下一阶段最小状态进入
默认 prompt。本次测试树搜索也没有找到 `Tape.handoff`、自动 context-overflow handoff
重试或 state 继承的直接断言。

默认 Agent 在首个 Turn 的 fork 内先调用 `ensure_bootstrap_anchor`；如果 Tape 中没有 Anchor，
就写 `session/start` Anchor + handoff event，见
[`ensure_bootstrap_anchor`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tape.py#L114-L117)
与
[`Agent.run_stream`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/agent.py#L101-L109)。
模型遇到 context-length error 时，Agent 最多一次自动写一个只含 reason/error 的 Anchor，
再用原 prompt 重试，见
[`_stream_events_with_auto_handoff`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/agent.py#L198-L264)。
由于旧消息被最新 Anchor 切到 View 之外，而这个 state 又没有被 selector 继承，该重试会
丢掉此前语义上下文。它是 overflow recovery，不是有最小继承状态的摘要 handoff；若拿来
做评测，还必须区分第一次失败所见 View 与重试所见 View，不能把重试输出冒充 first attempt。

## 4. `tape.py` 之外的直接调用者与真实落盘路径

`src/bub/tape.py` 只拥有底层词汇。真正聊天流程由 `builtin` 层连接：

```text
provide_tape_store hook
  → FileTapeStore(~/.bub/tapes)
  → Agent.tape（同步 store 包成 async）
  → session_tape(workspace, session_id)
  → ForkTapeStore（本 Turn 暂存）
  → ensure_bootstrap_anchor（首个 Turn 写 session/start）
  → TapeContext 查询 + selector 组装 messages
  → ModelRunner 调 provider / tools
  → Tape.record_chat 追加事实
  → stream 被消费或关闭后 merge_back
  → FileTapeStore/TapeFile 分配 ID 并追加一行 JSONL
```

### 4.1 Store 和 context 由 hook 注入

Framework 在 `running()` 期间取第一个 `provide_tape_store` 结果，并允许 plugin 返回
同步、异步 store 或 context manager；`build_tape_context` 同样走 hook。见
[`BubFramework.running/build_tape_context`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/framework.py#L344-L380)。
内置 hook 返回 `~/.bub/tapes` 下的 `FileTapeStore` 和默认 context，见
[`BuiltinImpl`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/hook_impl.py#L355-L364)。

`Agent.tape` 在 hook 没有提供 store 时退回内存 store，并用 adapter 包装同步实现，见
[`Agent.tape`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/agent.py#L53-L62)。

### 4.2 Session 只是在文件名层隔离

`Tape.session_tape` 把 workspace 的 resolved path 和 session ID 各做一个截断 MD5，
拼成 `<workspace-hash>__<session-hash>`，见
[`session_tape`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tape.py#L235-L240)。
这能避免相同 session ID 在两个 workspace 误共用默认文件，但没有 tenant、actor、ACL
或跨 Tape lineage 语义。

### 4.3 每个普通 Turn 先写 fork，stream 结束才合并

`Agent.run_stream` 为 session 建 scoped Tape，普通 session 使用 `merge_back=True`，
`temp/` session 使用 `False`；fork context 直到 stream 最后一个 chunk 被消费后才退出，
见
[`run_stream`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/agent.py#L85-L125)。
测试明确断言普通 session 在消费完成后才 merge，而 `temp/` 不 merge，见
[`test_builtin_agent.py` 第 167–200 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_builtin_agent.py#L167-L200)。

所以“模型产生了输出”不自动等于“父 Tape 已经持久化”：调用者必须消费或关闭 stream；
临时 session 的 fork 事实按设计不会进入父 store。

### 4.4 Context 先读 Tape，完成后再记录本轮

`ModelRunner.build_messages` 先调用 `tape.read_messages()`，再前置 system prompt、追加
steering 与本轮 user prompt；真正调用 provider 的就是这份 message list，见
[`build_messages`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/model_runner.py#L266-L293)
及
[`completion_response`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/model_runner.py#L83-L110)。

完成时 `Tape.record_chat` 按存在性依次追加：system、context error、本轮 messages、
tool calls、tool results、error、assistant message，最后总会追加一个 `event/run`，见
[`record_chat`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tape.py#L157-L200)。
Run event 可带 usage/provider/model，但 Entry 没有强制 terminal contract，也没有因果
边或“这些 tool results 属于哪个 call”的存储级约束。

这里还要区分“默认 builtin 行为”和“Framework 不变量”。内置 Agent 会追加 loop event，
ModelRunner 成功回答或完成工具调用后会调用 `record_chat`；但 Framework 的 stage 可以被
plugin 替换。官方 README 的 EchoPlugin 直接实现 `run_model` 并返回 prompt，没有 Tape
写入合同，见
[`README` 第 42–87 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/README.md#L42-L87)。
此外，空 prompt 会在创建 scoped Tape 前返回，`temp/` session 又明确不 merge 回父 store，
见
[`Agent.run_stream`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/agent.py#L85-L125)。
所以“默认 builtin execution 会产生 Entries”成立；“每一种 Bub execution 都耐久产生新
Entries”不成立。

### 4.5 JSONL store 才是默认持久化实现

`FileTapeStore` 为每个 tape 懒建 `<name>.jsonl`；搜索以外的 query 复用内存过滤，
append/read 委托给 `TapeFile`，见
[`FileTapeStore`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L99-L195)。

`TapeFile` 在单个对象的 thread lock 内增量读取文件，以最后缓存 Entry 的 ID + 1 分配
新 ID，再用 `open("a")` 写一行 JSON，见
[`TapeFile`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L198-L284)。
测试只证明同一 `FileTapeStore` 中两次顺序 fork merge 得到 `[1,2]`，见
[`test_file_tape_store_entry_ids.py`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_file_tape_store_entry_ids.py#L9-L23)。
它没有证明多进程或多个 store 实例并发追加时仍唯一、单调、无 torn write。

### 4.6 Fork 还会在持久化前删掉非文本 prompt parts

`ForkTapeStore.append` 会原地改写 payload：若顶层 `content` 或 `prompt` 是 parts list，
只保留 `type="text"` 的 part，然后才放入 fork，见
[`_redact_payload/append`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L66-L85)。
因此默认 Agent 路径保存的是经过选择的文字记录，不是 provider 实际看到的多模态请求
原件；这也不是按 secret key 做的通用安全 redaction。

## 5. 网站不变量与 Bub 当前实现的逐项核对

| 网站声明 | Bub 当前代码事实 | 判定 |
|---|---|---|
| 历史只追加、永不覆盖 | 普通 append 写尾部；但 store 合同有 `reset`，文件实现会 unlink | 部分实现 |
| Entry 不可变 | dataclass 字段浅冻结；嵌套 payload/meta 仍可变 | 未实现为强不变量 |
| Anchor 不是删除点 | 普通 Anchor 追加且 query 只改变读范围 | 已实现 |
| Anchor state 是下一阶段最小继承状态 | state 被保存；默认 View 排除最新 Anchor，未自动注入 state | 保存了，未完成继承 |
| Handoff 是带最小 state 的阶段转移 | 依次 append Anchor 与 event；无事务，默认 View 不消费该 Anchor state | 写入机制有，转移合同未完成 |
| Context 每轮构造 | 每次 provider call 前从 query + selector 重新组 messages | 已实现最小版本 |
| View 有任务取材语义 | 只有 callback 返回 message list，无 View 身份、来源或遗漏披露 | 部分实现 |
| Derived Index 命中后 raw reload | 只有 File/InMemory scan search；无 Index、checkpoint、source refs | 未实现 |
| fork/merge 只追加 delta，不改写 mainline | 正常 merge 逐条 append；fork reset + merge 会 reset 父 Tape | 被例外路径违反 |
| 多 session 隔离 | 默认文件名绑定 workspace path + session ID | 文件命名层实现 |
| 多 Agent origin 可追踪 | Entry 没有强制 actor/origin 字段 | 未实现 |
| Anchor graph / cross-tape View | 核心只有单 Tape 线性 Anchor 查询 | 未实现 |
| Task-as-View-Assembler / DAG Runtime | 只存在于站点源码；Bub 无对应 schema/harness/anchors/projection | 未实现 |
| Execution 必产生新 Entries | 默认 builtin 记录；plugin、空输入与 temp session 可不持久化 | 不是 Framework 不变量 |
| Eval/Training 派生事实保留 provenance | 可追加任意 event，但无 reward/source/derivation contract | 未证实 |

### 5.1 最直接的反例：`reset` 会删除或替换主历史

`Tape.reset(archive=False)` 调 store reset，然后追加一个新的 `session/start` handoff；只有
显式 `archive=True` 才先另写 `.bak`，见
[`Tape.reset`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tape.py#L214-L233)。
默认文件 store 的 reset 直接 `unlink` JSONL，见
[`TapeFile.reset`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L212-L220)。

这不是只有管理员能调用的内部函数：`tape.reset` 注册成默认 `agent_use=True` 的模型工具，
见
[`tape.reset`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/tools.py#L290-L294)
和
[`Tool.agent_use` 默认值](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/tools.py#L88-L98)。

Fork 也不是天然只追加 delta：若 bound tape 在 fork 内 reset，`merge_back()` 会先 reset
父 Tape 再逐条写 fork entries，见
[`ForkTapeStore.merge_back`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/src/bub/builtin/store.py#L87-L96)。
测试明确要求旧 `before` 被新 `inside` 替换，见
[`test_fork_store_merge_back.py` 第 62–74 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_fork_store_merge_back.py#L62-L74)。

因此不能写“Bub Tape 总是 append-only”。准确说法是：“Bub 的普通聊天写入路径追加
Entry；公开 reset 路径可以抹掉或替换 Tape，除非调用者主动 archive。”

### 5.2 文档与源码还有命名漂移

Bub 同 commit 文档把调用者写成 `TapeService.session_tape`、
`TapeService.fork_tape(tape_name, ...)` 和 `ForkTapeStore.fork`，见
[`tape-and-context.mdx` 第 37–68 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/website/src/content/docs/docs/concepts/tape-and-context.mdx#L37-L68)。
源码中的实际类型是 `Tape`，`fork_tape` 不接 tape name，`ForkTapeStore` 也没有
`fork` 方法。概念文档可帮助找入口，但最终 API 事实必须回到源码。

## 6. 测试真正证明了什么

| 测试事实 | 固定证据 |
|---|---|
| fork context 内写入在退出前不进入 parent；退出时可 merge，`merge_back=False` 会丢弃 | [`test_builtin_tape.py` 第 12–35 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_builtin_tape.py#L12-L35) |
| fork reset + merge 会替换 parent；不 merge 则保留 parent | [`test_fork_store_merge_back.py` 第 48–89 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_fork_store_merge_back.py#L48-L89) |
| 默认文件 store 的顺序 merge 分配单调 ID | [`test_file_tape_store_entry_ids.py`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_file_tape_store_entry_ids.py#L9-L23) |
| `record_chat` 的 run event 可保存 usage，`Tape.info` 可读 token/cache 指标 | [`test_builtin_model_runner.py` 第 92–122 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_builtin_model_runner.py#L92-L122)、[`test_builtin_tape.py` 第 37–72 行](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_builtin_tape.py#L37-L72) |
| `tape.search` 输出会过滤自身 marker | 该测试使用 fake query/store，只证明格式，不证明 `TapeQuery` 或 File search：[`test_tape_search_output.py`](https://github.com/bubbuild/bub/blob/20b8f9262e7da04544781fa79b2fe4db7cd789b1/tests/test_tape_search_output.py#L12-L74) |

本次源码和测试搜索没有找到对以下性质的直接测试：

- `TapeEntry` 深层不可变；
- after/between Anchor 的重复名称、缺失边界与 precedence；
- 日期边界和 FileTapeStore 的真实全文/模糊搜索；
- 默认 selector 是否继承最新 Anchor state；
- reset archive 的内容完整性；
- 多进程并发 ID 唯一性和 append 原子性；
- View 的来源、遗漏、representation 或跨 Tape lineage。

“现有测试没有覆盖”不等于这些路径一定错误；它只表示不能把它们列为测试已证明的
产品保证。

## 7. Zep Observations：从召回走向派生模式，但还不是 Learning

这一节是补充旁证，不是 Bub 的实现说明。材料包括 Akshay @akshay_pachaar 于 2026-08-09
发布的 X Article
[*Your Agent Remembers Everything and Understands Nothing*](https://x.com/i/article/2086395867582603264)
（[canonical post](https://x.com/akshay_pachaar/status/2086451430580470095)），以及 2026-08-10
读取的 Zep 官方
[`Episodes`](https://help.getzep.com/episodes.md)、
[`Facts`](https://help.getzep.com/facts.md) 与
[`Observations`](https://help.getzep.com/observations.md) 文档。文章末尾披露由 ZepAI 赞助；
所以它是一手的作者论述和产品示例，不是独立产品审计。

### 7.1 作者主张、官方合同与公开源码是三层证据

**作者主张。** 文章给出的层次是 Episodes → Facts → Observations：Episode 保存原始输入，
Fact 是实体间的具体 claim，Observation 则跨多个 episode/fact 提炼结构模式。文章进一步
声称生成分两阶段：确定性算法先把 fact 化成 entity-pair + relationship signature，再把
共享 signature 的 episodes 连成 connected components；LLM 只为算法选定的 cluster 写名称
和摘要，不决定 grouping。

**Zep 官方产品合同。** Episode 是开发者交给 Zep 的 message/text/JSON raw artifact，Zep
原样保存；Fact 是有时态和 episode provenance 的 graph edge。Observation 是系统自动从
graph 派生、evidence-backed 的 cross-entity context；官方文档只说它基于 facts/entities 的
structural clusters，新的 evidence 会让已有 Observation merge/regenerate，后继项可使旧项
supersede/retire。Observation 不能由用户直接 create/edit/delete，可被 Smart Context Assembly
选入 Context Block；当前只对 Flex Plus 与 Enterprise 客户提供。

固定 SDK 也只暴露 list/get/search 等读取接口，见
[`observation/client.py` 第 30–198 行](https://github.com/getzep/zep-python/blob/d3619d047fba5c8f2a2a1da44ce0338ab7625eab/src/zep_cloud/graph/observation/client.py#L30-L198)。
返回类型 `DerivedNode` 有 `episode_ids`、`start_at`、`latest_evidence_at`、`end_at`，见
[`derived_node.py` 第 12–69 行](https://github.com/getzep/zep-python/blob/d3619d047fba5c8f2a2a1da44ce0338ab7625eab/src/zep_cloud/types/derived_node.py#L12-L69)。
这证明 episode 级来源集合与证据时间窗，不证明 Observation 每一句或每个 claim 都有精确
fact mapping。

**公开源码边界。** Graphiti 固定 commit 中没有 Observation API、模块、producer 或测试。
其 README 明确把 OSS temporal graph engine 与 Zep 托管层/专有 Context Graph Engine 分开，见
[`README` 第 83–122 行](https://github.com/getzep/graphiti/blob/425bf2481b51437e43455e09d241c5f46e3d95f3/README.md#L83-L122)。
OSS 已有的 Community 机制是在 Entity graph 的 `RELATES_TO` 上做 label propagation，再用
LLM 汇总 community；它不是文章所述 episode-signature Observation pipeline，见
[`community_operations.py` 第 29–136 行](https://github.com/getzep/graphiti/blob/425bf2481b51437e43455e09d241c5f46e3d95f3/graphiti_core/utils/maintenance/community_operations.py#L29-L136)。
该 Community query 也没有按 edge valid/invalid/expired 做时态过滤；这是派生 View 可能丢失
时态约束的实测风险，但 Community ≠ 闭源 Observation，不能反过来断言 Zep Observations 有
同一个问题。

因此，精确 signature、episode graph、connected-component grouping、no embedding/ML 和
“LLM 绝不参与 grouping”目前都只是赞助文章对闭源机制的描述；Zep 公开文档与 Graphiti
固定源码没有把它们证明成可复核实现。

### 7.2 Connected component 没有证明 root cause 或 action priority

即使暂时接受文章的 clustering 机制，connected component 只证明哪些 episodes 因共享
signature 被放进同一个 cluster；它不编码业务依赖方向、因果强度、充分原因或行动成本。
文中“auth service 是 root blocker”需要解释有向依赖语义；“完成 auth 就清除三人的所有
blocker”还假设没有其它 blocker；“给 backend team 资源是最高杠杆行动”又增加了资源会
缩短工期、且没有更优干预的因果前提。这些不是 grouping 算法决定的 structural property，
而是 LLM summary/推荐做出的额外语义推论。

同理，“better retrieval 不能解决”只对平面 top-k 召回成立：如果所需 conclusion 从未存成
单条文本，多返回几条近邻不会自动生成它；但有向依赖已经在图中时，多跳 graph query 也可
在查询时计算 root/路径，不必先物化 Observation。文章 PM 场景是构造示例，没有 A/B、
held-out、sham 或 withheld 对照，不能证明 Observation 让任务行动稳定变好。

### 7.3 它与 tape.systems 的准确映射

| Zep 概念 | tape.systems / Superchat 映射 | 不能声称什么 |
|---|---|---|
| Episode | 最接近可回读的 raw Entry/material；是派生层的来源 | 不是 Observation 本身，也不自动是已验证事实 |
| Fact | 从 Episode 抽取的带时间 claim；应保留 source refs | “抽取成功”不使 claim 成为不可争议真值 |
| Entity/fact graph | 可再生的 Index/Projection | 不是 Tape 本体；图的当前状态不应覆盖 raw Entry |
| Observation | derived Index/Projection item + evidence window；可追加为 View candidate | 不是原始 Entry、客观事实、Anchor/Handoff 或 Learning |
| Context Block inclusion | View policy 选择 Observation 进入本轮上下文 | 只证明 Access/Selection，不证明模型 Semantic Use |
| Regenerate/retire | Projection 当前值或派生项版本发生变化 | 不应重写/删除原始 Episode/Tape evidence |

这与 tape.systems 的“derived index + raw reload”比简单向量召回更接近：派生层寻找跨事实
结构，结果仍应能回到支持它的 raw evidence。区别在于 Zep 文档允许产品侧原地表现为
Observation regenerate/retire；若落到 Superchat 的逻辑 append-only Tape，正确形状应是
追加 `observation.created/versioned/retired/superseded` evidence，带 source IDs、证据时窗、
算法/模型版本，Projection 再物化“当前 Observation”。“read-only”只描述用户不能直接
编辑该产品对象，不表示它是不可修正真值。

### 7.4 Observation 为什么仍不是 self-learning

Observation 解决的是“多个事实组合后显出什么模式”，能够改善后续 View 的候选材料；
它没有证明系统应采取哪个 Action，也没有记录该 Action 带来的 Outcome，更没有比较候选
policy、held-out 评估、safety/cost gate、promotion 或 rollback。即使 Observation 进入 prompt
并让一次回答更好，那也只是一个待评测的 content intervention。

最小语义使用实验至少要做四路盲测：真实 Observation、withheld、同长度但内容无关的 sham、
与证据冲突的 contradictory。每路要耐久记录 exact selected IDs、最终 rendered bytes、
first output 与 Outcome；先验证回答对 Observation **具体内容**有依赖，再在 held-out task 上
验证 effect，并经过 review/promotion，才能声称 durable learning。若 sham 也触发“使用成功”
或 withheld 与真实组无可区分，测量合同会产生 false positive，不能据此晋升 policy。

因此可以把 Observation 当作下一节闭环中的 Pattern Claim/evidence-backed View item，不能
把它直接当 promoted policy。要称为无权重训练的 learning，仍需把“哪个版本的 View/Action
policy 因哪些 Observation 被提名、如何评估、是否晋升、后续 Outcome 如何”作为追加事实。

## 8. 与 Superchat 当前实现的对照

### 8.1 两者使用同一组词，但责任边界不同

| 关注点 | Bub | Superchat 当前工作树 |
|---|---|---|
| 逻辑事实接口 | `TapeEntry` + store protocol；kind/payload 开放 | `superchat-tape.el` 生成逻辑 event ID、时间、schema、session 与可选因果/actor/workspace envelope |
| 物理存储 | 默认每 Tape 一个 JSONL；ID 每 Tape 从 1 开始 | SQLite `tape` 表；全表自增 storage cursor |
| 只追加边界 | 普通 append，但 protocol/tool 暴露 reset | 产品 Tape API 只暴露 append/replay；Projection reset 只重建派生表，不删除 Tape |
| Entry 物理形态 | 一种开放 payload 的 `TapeEntry` 外壳 | 完整 trusted material 与 bounded Runtime envelope 两种 schema，尚未统一 |
| Anchor | 一种 Entry；handoff 追加 anchor + event | 普通 compact/remember `kind=anchor` 与 branch `anchor.created` 两套形态，尚无统一 schema |
| View | `TapeContext` 选择后直接返回 message list | Context Continuity envelope 披露 covered/omitted/recallable ranges 与 selection policy |
| Projection | 若干调用者反向扫描 event；无通用 projection/checkpoint seam | 注册 reducer；物化 Projection 有 version/checkpoint/rebuild |
| Closure | 没有对应纯判断层 | 独立解释 Tape，判断 Runtime claim 是否 proved/open/unknown/invalid |
| Fork/分支 | Turn-local ForkTapeStore；可丢弃或 merge/reset parent | Session lineage 和 View 读取由明确 parent/branch 元数据处理；不把 branch merge 当作 Tape 重写 |
| Handoff | 一个方法写 Anchor + event，但非原子且 state 未继承 | compact、Subagent 交付、Workspace 跨 Run 读取是三种不同流程，无统一 Handoff 原语 |
| Learning 基础 | 可记录 chat/run，未见 Outcome/Promotion 合同 | 有 Claim/Feedback 与 Memory Projection；无 Pattern→Candidate→Evaluation→Promotion 闭环 |

Superchat 的逻辑 event envelope 可见于当前
[`superchat-tape.el`](../../superchat-tape.el)：Runtime event 进入 Tape 前分配独立于 storage
cursor 的 event ID，递归 redaction，超限 payload 只保存 size/hash；SQLite 只负责物理追加。
但同一模块的 `superchat-tape-append-material` 会为 trusted user/assistant/system/anchor 保存
完整 content，Runtime event 则把结构化 payload 放在 meta、把 content 限为 500 字摘要。
这是两种物理 schema，不应写成已经统一的 Entry 合同。Projection 的
reducer/checkpoint/rebuild seam 见
[`superchat-projection.el`](../../superchat-projection.el)。

### 8.2 Superchat 当前 Context View 比 Bub message list 多证明什么

当前工作树的 Context Continuity 路径把 material row 正规化为 `raw-fact`，输出明确的
selection policy、lineage、covered/omitted/recallable range；显式 recall 可按 Anchor、
storage IDs 或 range 回读。生产路径固定 `:represented []`，所以它只声称“本次 View
选择了哪些原始事实、没选择哪些”，没有声称 summary 已语义代表原文。

必须同时记录一个关键限制：当前默认 Continuity 只把 user、assistant 与非 Runtime
system rows 当 prompt material。普通 compact/remember Anchor 既不是带 branch schema 的
结构标记，也不是 prompt material，会以 `unsupported-schema` 留在 omissions，不能进入
默认 items。显式 Anchor recall 会校验 `source_ids` 是正整数、来源完整且确为 prompt
material，否则返回 blocked。也就是说：`/compact` 确实追加摘要 Anchor、折叠当前 buffer，
原始 Tape 也仍可回读；但下一轮默认 View **并不从这个 summary Anchor 继承状态**。因此
当前 Superchat compact 不能称为已经实现 tape.systems 的 Handoff。

对应实现位于当前未提交的
[`superchat-tape-view.el` material/omission](../../superchat-tape-view.el#L450-L469)、
[`assemble disclosure`](../../superchat-tape-view.el#L633-L713) 与
[`Anchor recall validation`](../../superchat-tape-view.el#L871-L923)，行为由
[`test/test-tape-view.el`](../../test/test-tape-view.el) 覆盖。由于这两个文件相对
`81197f7c64fd2bc96016eec86bdf2cbc3f5f530a` 有本地修改，本文只把它们当作
“2026-08-10 当前工作树事实”，不提供虚假的远端 commit permalink。

### 8.3 不应反向夸大的地方

- Superchat View 披露来源范围，不等于模型语义使用了这些内容；
- `:represented []` 表示 representation 仍只是合同边界，没有非空生产行为；
- Compact Anchor 是后来追加的摘要和回溯标记，不会替换原 Tape Entry，也不会进入当前
  默认 Continuity View；
- Projection 表/checkpoint 可 reset/rebuild，不代表 Tape 本身被重写；
- Memory 的 SQLite/FTS Projection 可重建，但当前 recall 直接消费 projection 内容，未实现
  网站声明的“Index 命中后按 source refs 回读 raw facts”，见
  [`superchat-memory-retrieve`](../../superchat-memory.el#L443-L454)；
- `claim.recorded` 与 `feedback.recorded` 能追加 statement、scope、basis、supersession 与
  correction，但没有 Outcome、Pattern、Candidate、Intervention、Promotion/rollback 合同，见
  [`superchat-claim-record`/`superchat-feedback-record`](../../superchat-claims.el#L69-L151)；
- Closure 是 Runtime trace 的纯解释器，只证明 lifecycle、pending effect/approval 与 durable
  terminal 等合同；Anchor、Memory、Claim、Feedback 在 Closure v0 中是 known-irrelevant，
  所以 Closure 不证明答案正确、经验有用或学习完成，见
  [`superchat-closure--step`](../../superchat-closure.el#L166-L190)；
- Bub 网站中的 Anchor graph、teams、eval、training 不能自动算作 Superchat roadmap
  或现有能力。

### 8.4 当前 Runtime/Authority 证据不能复原完整 provider trajectory

这是证据精度问题，不是 Tape 是否存在的问题。`intent.received` 与 `response.completed`
的 Runtime payload 只记录 `content-size` 和 SHA-256；Tape row content 是最多 500 字的摘要。
Authority 的 tool requested/completed 证据会保存 execution ID、request hash、effects、policy/
approval decision，以及 result size/hash，但不耐久保存完整 tool args/result。对应当前实现见
[`superchat-runtime.el` content metadata](../../superchat-runtime.el#L199-L205)、
[`superchat-authority.el` requested/completed evidence](../../superchat-authority.el#L613-L650) 与
[`superchat-tape.el` bounded summary](../../superchat-tape.el#L265-L286)。

这些记录足以做受限的身份、因果、权限与终态审计，也刻意避免把敏感明文全塞进事件；
它们不足以从 Tape 单独复原最终 prepared provider request、完整 prompt/tool args/results、
raw stream 或 first output。因而不能声称“当前 Tape 可精确 replay 某次模型决策”或“可直接
导出完整训练 trajectory”。未来若确需该能力，应追加经用户同意、可 redaction、
content-addressed 的 trajectory evidence object/manifest，而不是静默放宽现有事件 payload。

### 8.5 当前工作树的相关 ERT 不是全绿

本次在当前工作树实际运行：

```bash
emacs -Q --batch -L . \
  -l test/test-tape-view.el \
  -l test/test-compact.el \
  -l test/test-projection.el \
  -f ert-run-tests-batch-and-exit
```

结果为 66 个测试中 65 passed、1 failed；失败项是
`test-auto-handoff-compacts-long-prompt`，观测为 `compact-called nil`。与此同时，
`view/assemble-anchor-without-class-schema-stays-out-of-items` 通过，直接支持“普通 Anchor
以 unsupported-schema 留在默认 items 之外”这一当前事实。本文不把这组 relevant suite
表述为全绿，也不从单个失败外推所有 Compact 行为失效。

## 9. 对 Superchat 的最小可用启示

### 可以保留或借用

1. **四词模型足够清楚。** Entry 是事实，Anchor 是边界，View 是按用途取材，Handoff
   是追加新边界并转移执行起点。继续保持这些词不互相代替。
2. **Query builder 与 store 分离。** Bub 的 frozen `TapeQuery` 很小，说明选择条件可以
   是值对象；Superchat 无需为了同一目的再造重型 DSL。
3. **同步/异步 adapter 很薄。** 只有出现真实第三方 store 兼容需求时才需要同类 adapter；
   目前不预建抽象。

### 明确不要照搬

1. 不把 destructive `reset` 放进逻辑 Tape 合同。需要“新会话”就追加边界并改变 View；
   需要清理物理数据则另设有审计和保留策略的管理操作。
2. 不用 `frozen=True` 或浅 copy 宣称事实不可变；跨信任边界要深复制、序列化后重建，
   或使用真正不可变的数据形状。
3. 不让 Anchor state 只写不读。任何“最少继承 state”都必须指出哪个 View item 消费了
   哪个 Anchor 字段，并有测试证明。
4. 不把 message list 叫可审计 View。至少保留 selection policy、来源范围、遗漏与
   recall 说明；若将来产生非空 `represented`，必须有派生项到 source IDs/range 的映射。
5. 不从 JSONL 单对象 thread lock 推出多 Agent 共享 Tape 安全；共享追加需要数据库
   constraint/transaction 或经过证明的单 writer seam。

### 不改模型权重时，self-learning 的最小闭环是什么

这里的“learning”不能靠名词认定。Memory 是可召回材料，Skill/Profile 是配置或能力的
包装，Tape 是事实载体；任何一个“被写入”都不自动说明系统未来行为已改善。若不做
模型权重训练，真正被学习和晋升的最小对象应是两类**版本化行为政策**：

- **View policy**：对明确 scope/lineage/audience/sensitivity 的事实，决定下一次选什么、
  省略什么、如何表示，以及对应的 raw source refs；
- **Action policy**：在某类任务和证据条件下决定采用哪个 capability/tool/workflow、参数
  与停止规则；运行时效应仍必须经过 Authority，不能因“学到”而绕过审批。

最小闭环应按追加事实表达为：

```text
Episode/View receipt + Action-policy version
  → Action/Runtime evidence
  → Outcome + human feedback + counterexample/missing-evidence
  → Pattern Claim
  → Candidate View/Action-policy version
  → held-out / safety / cost evaluation
  → Promotion 或 Rejection
  → 后续 Run 显式引用 promoted version
  → 新 Outcome；必要时 Rollback
```

其中 Outcome/feedback 必须是追加证据，Candidate 与 promoted version 必须有稳定身份、
basis、适用 scope、评估集、reviewer、版本和 rollback target。只有观察到后续 Run 实际引用
了晋升版本，并在预先约定的 Outcome gate 上得到结果，才能声称完成一次可审计的
self-learning 闭环。`Memory recalled`、`Skill 文件已生成`、`prompt 发生变化`、
`claim.recorded` 或 `Closure valid` 单独都不够。

现有 Superchat 已提供 Tape、Projection、Claim/Feedback、Runtime/Authority/Closure 这些
底座，但还缺上述 Outcome、Policy Candidate/Evaluation/Promotion/rollback 状态机，也缺
可证明 first-attempt/content-dependence 的精确或受控 trajectory evidence。最短路线不是
先做训练器，而是先统一 typed Anchor/Handoff 与受控 trajectory manifest，再增加独立的
learning events/projection 和 held-out gate；Closure v0 继续只管 Runtime 闭合。

## 10. 已证实、尚未证实、下一步

### 已证实

- Bub 默认聊天路径会在 fork 中记录 Entry，普通 session 在 stream 关闭时逐条合并到
  JSONL，并在单 store 顺序场景分配单调数字 ID；
- Anchor 查询能把默认 Context 起点移到最新 Anchor 之后；
- Bub 的公开 reset 和 fork-reset merge 可以删除或替换父 Tape；
- Entry 的嵌套 payload 没有深层不可变保证；
- Bub 当前 View 是 message list 组装机制，不是带 provenance 的持久 View；
- tape.systems 的 derived Index、Task-as-View-Assembler/DAG、graph/team/eval/training 是
  上层模型声明，不是 `tape.py` 的实现事实；
- Zep 官方文档确认 Episode 原样保存、Observation 是自动派生且有 evidence 的可检索对象，
  会 regenerate/retire 并可被选入 Context；这说明“派生模式进入 View”是产品能力，不说明
  Observation 是原始事实或系统已经 Learning；
- Graphiti 固定 commit 没有 Observation API/pipeline；其 Community 是 Entity graph label
  propagation + LLM summary，不能用来证明文章的 episode-signature 算法已经开源；
- Superchat 当前普通 Compact Anchor 被 Continuity 标为 `unsupported-schema`，不进入默认
  View，所以不能称为已实现 tape.systems Handoff；
- Superchat 当前 Runtime/Authority 证据能审计身份、因果、权限和终态的一部分，但不能
  独立复原完整 provider trajectory；
- 当前 Superchat 没有版本化 View/Action policy 的 Candidate→Evaluation→Promotion/
  rollback 闭环，不能声称已经 self-learning。

### 尚未证实

- Bub 默认 JSONL store 在多进程、多 Agent 并发下的唯一顺序和 crash atomicity；
- Anchor state 在默认 Bub prompt 中有任何隐藏的继承路径；本次直接执行只观察到它被
  排除；
- `tape.systems` 线上 deployment 是否精确对应本文固定的站点 commit；页面本身不显示
  deployment commit；
- X Article 所述 deterministic signature/connected-component grouping 是否是 Zep 当前
  production 实现；公开官方文档没有给出该精度的算法或源码证据；
- 任一系统仅凭 Tape 记录就能证明 Selection、Semantic Use、Task Effect 或 Learning。

### 下一步

这份研究不要求新增 Superchat 模块。若目标只是守住 Tape/View 边界，下一步优先补合同
测试：证明产品 Tape 接口没有 destructive reset，并证明每个 Context View 的 selected、
omitted 与 recalled storage range 能从 SQLite 重建，同时先修清当前 auto-handoff 测试
失败的实际原因。

若目标是无权重训练的 self-learning，下一步不是直接新增 Memory 或训练器，而是先冻结
typed Anchor/Handoff 与经同意、可 redaction 的 trajectory manifest；随后定义版本化
View/Action policy、Outcome/feedback、Candidate/Evaluation/Promotion/rollback 事件和独立
Projection，最后用 held-out、safety、cost gate 证明晋升。多 writer store 则是另一项
问题，出现真实需求时再设计 transactional append/merge seam。

但 Observation 四路实验还不能立即进入 promotion gate：当前 Artifact Semantic Use v0.2
已被独立复审判为 `VETO/not-identifiable`，能接受 unknown model-visible field 中的目标、
缺失 prompt/Artifact inputs、重复 key、未 join bytes 等假阳性，见当前
[`v0.2 README`](../../experiments/artifact-semantic-use-v0/revisions/v0.2/README.md) 与
[`Decision Map`](../../experiments/artifact-semantic-use-v0/DECISION-MAP.md)。下一步先修测量
合同，并证明 withheld/同长度 sham 不会被判成 content dependence；否则任何 Observation
“成功”都不能授权 policy promotion。
