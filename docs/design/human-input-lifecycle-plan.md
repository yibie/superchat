# 人类输入与执行等待：开发计划

日期：2026-09-08。状态：用户已同意开始；第 0 步隔离验证进行中，生产实现尚未开始。
依据：[源码调研与三轮反思](human-input-lifecycle-research.md)。

## 目标体验

用户运行天气查询。模型缺少城市时显式调用 `ask_user`（拟定名称）：

1. 当前回答显示 `Waiting for your input` 和问题，不推进后续模型请求或 Workflow 步骤。
2. 用户可以继续编辑草稿、看历史、使用其他 Session；回答不是新的普通聊天消息。
3. 用户选择深圳，答案回到原工具调用；模型在新一轮中依据深圳生成查询参数。
4. 隐藏问题面板后仍可重新打开；重复提交、旧面板、已取消执行的回答均不生效。
5. 用户明确取消或整体期限到达，等待失效，后续步骤不继续。
6. 超时/取消时正确释放对应 transport；不得因为旧请求继续占用模型而阻塞新请求。

这不是对所有自然语言问句的保证。保证从明确的输入请求生效点开始；不回滚此前 Effects，也不声称非空文本必然足以解决业务问题。

## 首期建议采用的行为

以下是计划提出的选择，不把它们写成已实现事实。

### 明确提问，不改变所有 UI

- 新增专用异步提问工具。首期每次一个问题，支持单选和自由文本；输入 schema 明确问题、选项与结构约束。
- `render_ui` 保持展示工具，`send-to-chat` 保持普通新输入；不得继续把二者介绍成必需回答的实现方式。
- 提问不授予权限。真正访问网络、读取位置、写文件仍经过 Authority。
- 回答中的“我不知道”可以是合法文本，交回模型澄清；不以 UI 校验冒充语义判断。
- 空白必答文本、非法选项仍留在等待状态并给出英文反馈。

### 一次结算与明确取消

- 每个输入请求绑定精确 Session、Run、工具调用及运行时生成的请求句柄；显示标题、当前 buffer 名不作为关联依据。
- 一个请求最多结算一次；先发布结算状态，再交付回调，防止重复点击和同步重入。
- UI 只提交答案或取消意图，不拥有 Run 终态。
- 隐藏面板不取消；关闭 Session、取消所属 Run 或期限到达须使问题失效。
- 首期不提供会被误解为成功的“跳过必答问题”。取消入口应明确写当前回答或当前 Workflow 执行，作用范围必须与实现一致。
- 无 UI/headless、准入失败及 UI 创建失败必须显式失败，不允许以普通成功文字绕过等待。
- 不承诺跨 Emacs 重启恢复。必要的 Tape 审计照常追加，但不把审计日志当成持久 continuation。

### 同批工具与后续参数

推荐首期独占提问批次：同一个模型响应只能包含一个 ask，不能混入其他工具。
整批检查必须发生在任一工具 Effect 前；`[effect, ask]`、`[ask, effect]`、多个 ask 都不能部分执行。
不保存同批旧参数等待回答后直接执行；回答后由新模型轮次生成后续调用。

这条规则以第 0 步找到可靠整批 seam 为前提。若 native streaming/async 接口无法提供它，应先提出最小 adapter 变更，不能偷偷降为“尽量串行”。
首期推荐混合批次 fail-closed，终止该执行而不是自动重试；显示可操作错误，不让 Workflow 当作 completed 推进。
具体终态与审计映射在第 0 步核定，不新增模糊的成功状态。

### 三种时间分别处理

| 时间 | 建议语义 |
|---|---|
| 模型沉默 watchdog | 只在真正等待模型响应时计时；已接受的人类输入等待不计入 |
| transport hard cap | 只约束模型 transport，不把人类思考时间算成请求卡死；沿现有多轮计时方式选择最小改动，禁止每次 UI 刷新偷偷刷新额度 |
| Runtime 整体 deadline | 保持独立的总期限；等待界面显示它，到期后明确失效，不偷偷延长或关闭 |

首期不额外加一个默认 180 秒“用户回答超时”。没有整体期限时，可等到用户回答或主动取消。
取消确认等既有 Authority 交互不能被此次改动意外改变；如需统一它们的计时，另列明确改动。

## 模块职责

| Module | 本次责任 | 不应承担 |
|---|---|---|
| Runtime | 输入等待与所属 Run 活性的衔接、终止失效、公开等待信息、精确 transport 释放 | 加载 UI 插件、按界面状态推断完成 |
| LLM adapter | 原生工具批次准入接点、模型/人类等待阶段切换、transport 计时与句柄上报 | 决定回答归哪个当前聊天窗口 |
| Authority | 提问仍经既有执行/审计 seam，一次工具结算，保留拒绝及 consumer 条件语义 | 把用户回答当成权限允许 |
| 问题呈现 Adapter | 结构化问题显示、回答入口、重开/失效反馈；可复用 widgets/TextUI | 创建第二个 Run 注册表、调用 send-input 冒充回答 |
| Workflow | 等待中不推进；取消/失败后停止；有效工具结果后的正常完成才能推进 | 解析“缺少城市”等自然语言来决定暂停 |
| Tape / Closure | 追加可核验事实，并准确解释新引入的执行事实 | 用 UI 展示成功证明用户已经回答 |

输入请求模型应有一个唯一 owner，而不是 Runtime、GenUI、Workflow 各存一套 pending 真相。
第 0 步决定放在 Runtime 内部还是确有必要增加一个 headless Module；不得为命名整齐而增加透传层。
若新增文件，先确定层级并更新层级测试；Kernel 不反向 require UI。
UI Adapter 的可变部分是呈现方式，不需要新通用注册框架。

## 实施顺序与停止条件

### 0. 固定两个接口与可执行反例

只做隔离探针和设计落点，不接真实 provider。

- 真实 native llm streaming/async 路径核对完整工具批次何时可见；验证混合批次能否在 Effects 前拒绝。
- 沿 Runtime、Authority、LLM、Workflow 核对 withheld callback、取消、整体 deadline、回调重入、失败收尾。
- 列出输入请求创建/结算/失效的唯一 owner、最小 Interface、三种 timer 的实际处理点。
- 明确新 Tape 事实是否必要及 Closure 如何处理；不能简单全列为无关事件，也不能制造不存在的 tool.completed。
- 阅读 ADR 0004/0006 配对检查；有语义冲突时明确提出，不按例外绕过。

交付：两个可重跑探针、更新后的精确接口/文件范围。若只能借全局 advice、绕 Authority 或改无关调度器实现，则退回设计。
后续步骤依赖这里通过；本计划不把尚未验证的 seam 当作事实。

### 1. 修复异常终止后的 transport 释放

预计 owner：`superchat-runtime.el` 与直接 Runtime/LLM 测试。

- 使用当前 Run 捕获的精确 handle 做 best-effort abort；先发布终态/quiesced 和清槽，再经过取消 adapter。
- 异常终结与“终结后才返回 handle”都能释放原 handle；正常完成不额外 abort。
- 不在取消回调后读取 global/latest handle；不误取消新 Run B 或更新的模型轮次。
- 只在取消 adapter 附近隔离其 error/quit，不掩盖原始终态/交付结果。
- 验证通过后合并既有重复 abort 路径；不提前删除。

验收：elapsed wait/hard/deadline、主动取消、失败、audit-incomplete、同步终结后返回 handle、嵌套新 handle、取消重入、adapter error/quit、正常完成零误杀。
对 provider 真正停止的保证另需实机日志；单测只能证明调用了正确取消接口。

### 2. 打通无真实 UI 的最小输入闭环

预计 owner：Runtime、LLM adapter、工具描述/Authority 的必要接点及直接测试，精确范围由第 0 步确定。

- 注册显式 ask；测试 Adapter 暂存回答，不立即回调。
- 生效的等待阻止后续模型轮次；三种时间按上表执行。
- 结构化回答验证和一次交付；取消、到期、拒绝、无 UI 均有明确结算。
- 混合批次零 Effects；回答后新一轮参数来自包含回答的上下文。
- 用真实 Runtime、native llm scheduler、Authority、临时 SQLite/Tape 和 Closure 验证；仅替换模型 transport 和人类输入 Adapter。

交付：一个普通聊天的端到端闭环，而非仅几个状态 setter 单测。

### 3. 接入真实 Emacs 交互与 Workflow

预计 owner：问题 UI Adapter、`superchat.el`、必要的 Workflow 接点、tools/skills 说明及直接测试。

- 默认主聊天显示英文等待状态和回答入口；问题用宿主生成的固定结构呈现，不要求模型生成任意 UI 程序。
- 回答操作可调用 minibuffer 的选择/文本输入；不让一个长期打开的 minibuffer 垄断用户编辑过程。
- 待答问题可重开；只管理自身界面，不改用户窗口布局或覆盖草稿。不同 Run 的问题不得互相替换丢失。
- 回答原请求，不发送新聊天；取消按钮的文字和作用范围一致。
- 在真实三步 Workflow 中：第一步提问时第二步不启动，回答后继续；取消后余下步骤不运行。
- 优先复用既有非 completed 停止路径；如果已有行为足够，不改 Workflow 引擎。
- 更新 `render-ui` skill 与工具说明：展示用 render_ui，必须取得输入用 ask；权限用既有审批。
- 首期不把普通草稿自动当成问题回答；提供明确回答入口，保留现有草稿/暂存行为。

### 4. 回归与真实事故复测

- 按每步风险运行 focused、相关模块、canonical；安装依赖环境与 skip 明确区分。
- 独立 Codex reviewer 检查；实现 worker 使用 Astra low，按模块分工；共享 Runtime 改动串行交接，不使用 Claude。
- 真实 LM Studio 验收前先读取已加载 backend 类型、库版本、有效工具/streaming 路径，避免把磁盘新代码当成 daemon 已加载代码。
- 经确认后使用隔离数据与测试配置复测天气场景：等待超过原模型沉默阈值、回答深圳、继续查询、取消、关闭重开、重复点击。
- 检查无第二条冒充回答的 User 消息、无等待期间后续步骤、无新请求被旧请求持续占用。
- 记录实际 transport 取消与服务端状态；外部天气服务失败与交互协议失败分开归因。
- 完成时提供用户操作说明及已验证/未验证边界；不以 batch 通过替代 GUI/provider 验收。

## 最终验收矩阵

| 场景 | 必须看到的结果 |
|---|---|
| 普通聊天需要城市 | 明确等待，回答原问题一次，新模型轮次使用城市 |
| Workflow 第一阶段提问 | 第二阶段尚未启动；回答后正常继续，取消则停止 |
| 同批 ask 与其他工具（两种顺序） | 任一 Effect 前拒绝，无部分执行、无成功推进 |
| 等待超过旧模型 timeout | 不报模型沉默超时；独立总期限按声明处理 |
| 提交与取消/到期同时发生 | 一个结算结果，回调最多一次，Tape 无伪成功 |
| 隐藏、切换 Session、重开 | 原问题仍可定位，不把答案送到当前错误 Session |
| 清空/关闭/已终结后点击旧控件 | 明确失效，无新执行、无旧工具 Effect |
| 无关 Run B 正在输出 | A 等待/取消不停止或污染 B |
| 草稿、C-c C-q、取消改问 | 既有编辑体验保持，不被回答动作覆盖 |
| 模型只输出普通“请告诉我”文本 | 不谎称进入结构化等待；这是工具选择能力的边界 |

## 不在首期范围

跨 Emacs 重启恢复、通用任务依赖图、多客户端 RPC、自动识别所有自然语言需求、语义答案裁判、自动位置定位授权、任意旧按钮升级为问题回答、重写整个 Workflow 或 Render。

## 当前完成情况

- 已完成：源码调研、三轮反思、此开发计划。
- 进行中：第 0 步探针与独立验收设计。
- 未完成：探针结论、实现、回归与真实体验验收。
- 没有创建远程 issue 或执行提交/推送。

### 执行启动记录（2026-09-08）

用户同意开始，并明确要求独立 worker 承担审查与模拟真实用户测试。
复用现有 Herdr panes，均确认显示 Astra low；不使用 Claude：

- `superchat_astra_worker`：第 0-A，原生 streaming/async 整批准入探针。
- `sc_spec_workflow`：第 0-B，等待/计时/取消/Workflow 和 transport 释放探针。
- `sc_spec_transport`：独立审查与用户入口模拟，不参与生产实现。

本阶段 worker 只可写各自隔离临时探针；生产文件和仓库测试保持冻结。
网络防护先于加载安装，两种模型 transport 都替换；不访问用户 daemon、provider、GUI 或真实数据。
测试须区分 batch 键盘入口模拟和真人 GUI 验收，不将前者解释成后者。
第 0 步结论经独立核查后，由 leader 才交接下一阶段实现。

独立 reviewer 的首轮验收提醒：旧 Workflow chain 测试替换 Runtime submit 并假定 resume-decision 为 advance，不能作为此次端到端证明。
必须经过实际 Workflow、Runtime、Closure；等待状态须仍被 active-runs/Dispatcher 草稿保护正确识别，不能因新增状态允许第二个普通 Run。

### 第 0 步中间证据（待整体独立核查）

- A 原矩阵：原生 streaming/async 共 16 场景，混合批次在现有 capture seam 可于 Authority 前拒绝。
  独立 reviewer 复跑后发现原 dispatch wrapper 登记在重叠请求中漏掉一个 prompt，原登记不能作为隔离证明。
  reviewer 改为 Superchat prompt-build 登记的独立 A/B/foreign 探针 2/2；仅证明正确登记下的有界可行性，生产登记位置仍需修正验证。
- 真实聊天 pending 草稿/no-second-Run 的行为观察正确，但 reviewer 入口测试因启动时 tar/gtar/openssl 探测被 guard 拦截而失败，尚不记为绿色 UI 验收。
- B worker 的 12/12 基线矩阵走真实 public Workflow/Runtime/Authority/Tape/Closure；模型入口被替换，native result/scheduler 保留，不独立证明 SSE。
  等待工具无结果时，取消/期限触发 audit-incomplete；迟到结果按当前 Closure 合同仍可追加 tool.completed，且不会恢复 A 执行。
  同一 handle 在部分取消路径被 abort 两次，无工具的 wait/hard/failure 与迟到 handle 路径则可能 abort 零次。
- 这些是当前基线和探针发现，不是产品已修复。显式输入的取消需要 Authority 如实结算且阻止 native success continuation；不能推及未知外部异步 Effects，也不能豁免 Closure。

临时证据：A `/private/tmp/human-input-phase0a.YTDlIo/`；独立 A `/private/tmp/human-input-reviewA.qseGX1/`；B `/tmp/superchat-phase0b.emf2Kp/REPORT.md`。
原始失败保留，后续修正不得改写它们的通过状态。整体 Phase 0 尚未放行。

A 登记位置澄清：build 登记的绿色隔离探针只覆盖普通路径；生产设计应在每次真实 native dispatch 前，完成 evidence prompt 复制、attempt-tools 设置和活性检查后登记实际 attempt-prompt。
不得将一次 build 的登记泛化到每个 attempt，也不得通过全局 native generic wrapper 实施策略。
async epistemic evidence 路径仍未覆盖。

A worker 修正探针报告：`/private/tmp/human-input-registration.UURRKu/corrected.log`，3/3（16 原矩阵场景与两种模式 overlap/foreign），guard 0；build observer 仍只证明普通路径，不证明 evidence-copy 或生产登记存活期。原 native generic wrapper 重叠失败保持为历史红。

独立入口复核：Org 的 tar/gtar 与 openssl 默认 capability 探测导致旧 guard 红；在 guard 之后、产品之前仅将 `dired-guess-shell-gnutar` 和 `smime-openssl-program` 两个 fixture capability 设为 nil，新进程入口 2/2、guard 0。
未替换 M-x/send/Runtime、未清空 guard hits，未改 HOME/CODEX_HOME；这是 batch 入口验证，不是 GUI 验收。旧红与调用栈保留于独立 A 目录。

### 第 1 步有界放行（2026-09-08）

独立 B fresh scratch 复跑 12/12，另加 2/2 断言；六份产品源码 hash 一致，非预期 IO 0。
报告位于 `/private/tmp/human-input-reviewB.QKjWae/`。它核实了 transport 漏取消、重复取消与 B 隔离，只支持独立的 transport 释放修复，不代表整体输入生命周期通过。

leader 已交接 `sc_spec_workflow` tests-first 实施：生产仅 `superchat-runtime.el`，直接测试 `test/test-runtime.el`，确需 native fixture 时可改 `test/test-llm-backend.el`。
`sc_spec_transport` 保持独立 reviewer，不参与实现。Authority/Closure 与 generic 迟到工具合同冻结；显式输入的非成功结算仍待设计验证。
原生嵌套轮次导致旧 handle 未上报的情况必须具名测试；如 Runtime 根本收不到该 handle，先报告最小 LLM 改动需求，不自行扩展生产范围。

### 用户确认的取消体验（2026-09-08，覆盖候选中的拒绝取消规则）

取消表示“停止继续做”，不是撤销已经发生的事实。用户明确否决了 R2/R3 临时候选中“进入 finish/freeze 后，虽任务仍未向用户结束，却拒绝取消”的体验。那些候选保留为历史设计，不构成生产实现授权。

- 任务仍在进行时，收到取消应尽快接受停止请求，停止后续模型请求、工具执行和 Workflow 推进，并使待答问题失效。
- 尝试取消精确归属的底层请求；已发生的外部操作不能假称撤销，不能确认停止的操作须如实说明。
- 尚未开始交付的最终答案及迟到续跑不得因内部收尾而继续送出。已经展示的内容可以保留。
- 必要的审计保存和资源清理可以继续，但不能成为拒绝用户停止的理由。已提交的 Tape 事实不改写；停止已接受不等于所有历史结果都必须改成 cancelled。
- 只有任务确实已经结束，才报告无活动任务可取消；不得仅凭内部 freeze 状态判断。实际交付入口、完成判断及已进入回调的处理须在新设计中给出准确时序。
- 接受停止与 durable status、交付/下一步许可须明确区分，尤其不能因 Closure 证明 completed 就在用户已停止后推进 Workflow。
- “尽快响应”不承诺强行抢占任意 Emacs 同步代码，也不承诺外部服务瞬间终止。需以实际入口和事件循环验证响应边界。

下一步仍为设计修订及独立挑战：精确结算、不可变审计、Q1/Q2 身份及原始异常合同保留；以新的停止体验重新安排最小实现与测试，不先发布 ask/UI。独立 reviewer 专门验证停止后的零续跑、零新效果、未交付内容抑制和真实用户入口。
