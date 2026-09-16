# Decision: Emacs operating context and reference reading without Preset activation

Status: 第一、二片 accepted，第三、四片 proposed — 用户于 2026-09-16 同意将前两片定案、后两片留待基线对照之后收敛（"同意"）；未改变生产代码、默认值或已接受契约

## Problem

[AI layer 方向提案](2026-09-16-emacs-ai-layer-tool-surface.md)要求模型拥有操作 Emacs 的知识、现场和执行能力。具体载入路径必须区分资料读取与配置应用。本文收敛这项区别及其四个可独立验证的切片；执行状态仍由[路线图](../../roadmap.md)管理，不另建里程碑队列。

[superchat-skills.el](../../../superchat-skills.el) 的 `superchat-skills-load` 返回 Preset。[superchat-preset.el](../../../superchat-preset.el) 的 `superchat-preset-apply` 可以设置本轮模型和工具、运行 pre；agent/plan 的正文进入系统提示，dispatcher 可以激活 View 的 Capability 和 Scope。prompt 型也不是保证无配置副作用的参考资料接口。现有 `>name` 保留这些配置语义。

当前缺口是：默认聊天工具没有已实现的 describe-function；聊天入口没有可靠的编辑来源；模型没有按条目读取随包资料、且不应用 Preset 的入口。buffer 与 Workspace Artifact 的读取不能替代这种接口；获准的 eval 仍有文件读取能力，所以不能把缺口表述成整个系统无法读文件。

## Proposal

交付顺序采用用户提出的四片：默认文档发现、共用现场、资料载体与取用、操作指南正文。前两片可以先通过实际请求和任务验证，不等待第三片的内容完整。

**本记录只对第一、二片定案。** 第三、四片的接口——`read_reference` 的参数形状、索引与正文的字符上限、是否进入免提示默认名单——写在下面是为了说明方向，不是已决定的约定：它们要等第一、二片的基线对照（同一模型、同一组任务，加入文档发现与现场前后）跑完再收敛。理由是那次对照可能证明模型在拿到`describe-function` 与现场之后，只需要一小段常驻说明即可完成目标任务，那样整套按 ID 取用的机制就是不必要的复杂度。第四片的正文由同一次对照暴露的实际失败决定，载体设计不应比它更早冻结。新资料是数据；它不成为 Capability、Preset、Service 或第二个授权中心。

### Guarantees and explicit configuration

| 配置情形 | 本提案承诺 |
|---|---|
| 全新、未定制的默认聊天 | 已安装资料存在；默认发现和取用工具可见；默认 collector 提供准确现场和短索引；现有默认 Authority 下资料读取无需提示。 |
| 升级后正常启动，相关变量没有显式设置 | 新包初始化使用新默认值；内置资料与正在运行的包版本一致，不依赖用户技能目录是否为空。 |
| 用户已设置内置工具列表或请求/Preset 的显式工具集 | 保留原选择，不自动追加名字；工具缺失时提示或诊断说明缺项，不能宣称完整默认体验仍然成立。当前内置 code Preset 也有自己的显式列表，不由全局默认值自动扩充。 |
| 用户已设置免提示名单、prompt-for-all-reads 或更严格策略 | 保留原选择；正常申请或拒绝仍由 Authority 决定。工具可见不代表可以免提示执行。 |
| 用户自定义插件组合 | 在仍挂载 chat-collectors 的组合中获得现场修复；主动移除该行的组合不被后台重新挂载。 |
| 运行中热加载新文件 | 不根据“值恰好等于旧默认值”猜测用户意图，不改写已接受请求和既有绑定值；以新请求实际生效的配置为准。 |

产品保证是默认安装和默认配置下可复现的行为，以及所有配置下均成立的语义约束。它不意味着绕过显式用户选择、deny 或既有[工具上限](../../protocols/request-lifecycle.md#request-rewriting-and-tool-selection)。升级说明给出缺失的配置项及恢复标准默认值的方法，不在后台 union 名单。

### Slice 1: documentation discovery

把 `describe-function` 加入 `superchat-llm-default-tool-names` 的内置默认集合，并核对 `superchat-select-tools.el` 的软依赖 fallback 与相关说明。它已存在于 `superchat-authority-unprompted-read-tools` 的默认值，本片不扩大免提示名单。默认集合是 defconst；用户选择入口是 defcustom `superchat-llm-tool-names`，两者不是同一个变量。

本片针对普通默认聊天及继承这套默认值的请求。显式 tools、既有受限 Preset 和用户配置保持有效；不借此静默扩充全部 Capability。有效的结果是模型收到可调用的文档工具，不能仅以常量里出现一个字符串验收。代价要如实说明：默认请求从此多带一个工具 schema，这是 M4 之后第一次默认体验发生可观察的变化，验收时一并记录请求体积的变化。

### Slice 2: shared origin context

来源由用户入口确定。`superchat-act` 使用调用时的来源；用户从非聊天 buffer 显式调用 `superchat`、`superchat-new-session` 或 `superchat-open-session` 时，将该 exact buffer 关联到最终选中的 View。从聊天界面新建或选择 View、自动恢复历史时不推测外部来源：已有 View 保留自己的进程内关联，新建或重建的 View 没有关联则报告未知。`superchat-branch-session` 创建的新 View 不从历史或父 View 隐式继承来源，后续由显式编辑入口建立关联。普通焦点切换不重绑，不通过 other-buffer、名字或最近使用顺序猜测来源。从聊天窗口自身调用入口时没有外部来源：聊天 View 不把自己登记为编辑来源，该请求按未知来源处理。

每次准备新请求时在关联的 exact buffer 上捕获一次快照，包括检查预览；快照包含名称、文件、major mode、buffer 修改 tick、buffer 中的 point 和当时有效选区，不长期复用打开 View 时的旧文本。检查后的确认发送、排队、后续 View 重绑和异步回调不重新捕获。位置表示关联 buffer 中的 point；没有确切 window 证据时，不声称它就是用户眼前的光标。

进程内对象与可序列化事实分开。推荐通过 Envelope 的 owner-prefixed `:context` 携带 `:emacs-origin-buffer` 与 `:emacs-origin-snapshot`；后者只含数据。活对象不写入 Tape，也不混进持久上下文证据使用的 `:context-envelope`。对象改名保持身份，死亡后不以同名新对象代替；可报告捕获时事实与来源已失效。借用对象不赋予 Scope 或 Session 关闭它的权限，遵守[编辑对象关闭契约](../../protocols/session-editor-workspace-close.md)。

复用现有 `chat-collectors` 的 apply 注册现场 contributor；act 最终同样走 chat Surface。本片不新增 Service、Surface 或默认 profile 行。collector 消费请求快照，返回一次现场 metadata 和必要选区；它不在收集时改绑目标，也不修改用户原始输入。卸载该行后停止贡献新现场，用户 buffer 和已经记录的事实保留。

动态现场放入 `context` / hard，用户显式选区放入 `attachment` / hard / required，二者使用现有字符预算。选区不放入 system/instruction；超预算明确拒绝，不静默删掉，也不转入系统槽躲开预算。无来源属于正常数据；有明确来源却无法构造合法片段属于应报告的收集错误。新片段须保留来源关系并遵守[请求生命周期](../../protocols/request-lifecycle.md)。

迁移同时处理 `superchat--input-with-origin` 的输入拼接，以及 `superchat-build-request-plan` 将 assembled preview 回填到 prompt、送出时形成 chat-prompt-override 的路径。预览和送出共享同一份冻结现场，并携带足够的结构化组装信息保证只渲染一次；不通过扫描用户文本里的标记猜是否已注入。检查器中经用户编辑的最终正文按原有精确送出约定执行，并在送出前重新校验预算。已删除的现场或选区不由 collector 恢复；hard/required 约束普通自动组装，不赋予 collector 覆盖用户显式检查编辑的权力。Tape 继续遵循既有用户正文记录规则，不把自动现场或选区拼入 intent.received。

本片使模型获得正确来源事实，不等于 edit_buffer 已支持 exact 对象写入，也不等于 eval 的执行上下文已统一；这两项执行改进仍属于[方向提案](2026-09-16-emacs-ai-layer-tool-surface.md)的后续职责。

### Slice 3: bundled reference data and reading

首次交付采用必需顶层 `.el` 模块中的纯字符串资料，**正文直接写在该 `.el` 里**，不引入由 Markdown 生成 `.el` 的构建步骤（仓库目前没有 Makefile 或 CI，新增构建步骤会把一个内容问题变成一个发布问题）。代价要认：在 elisp 字符串字面量里写带转义的长篇散文，编辑手感差且无法使用 Markdown 工具链；若第四片的正文规模证明这一点不可接受，再单独决定是否引入生成步骤。条目包含稳定 ID、标题、用途摘要、正文和由正文计算的内容版本；索引与正文读取取自同一份数据。资料不走 `superchat-skills-load`、Preset 解析或用户目录播种。加载了该版本的模块后，其条目即可读取；资料文件中即使出现 tools/model 等文字，也不解释成配置。

建议模型入口为 `read_reference`，参数只有字符串 `id`。它只返回内置已知 ID 的身份、内容版本和正文；未知 ID、路径样式输入或超出参数 schema 的输入明确失败，不拼接目录、不执行正文、不退回任意文件搜索。首版常驻索引最多 1024 字符，每个正文条目最多 4096 字符；超长资料拆成单独可发现的条目，发布验证拒绝越界，不在读取时伪装完整地截断。

读取前后，模型配置、工具选择、当前 Preset、Capability、插件组合、Scope 和 Authority 不因资料内容改变。正常工具审计和结果追加仍存在，不能把“无配置副作用”误写成“不产生任何运行记录”。用户自有参考资料及外部路径加载不在首版接口内；保留既有技能和用户文本。

入口选择为内置，理由是这是随包发布、需要在默认安装中可用的基础能力。接入现有 tools-builtin 的 resolver、候选选择、物化与可信 wrapper；效果如实声明为 Emacs 内部参考条目的 local-read，scope 使用稳定资料 ID。执行仍经过 Authority、Run 存活检查及 Tape 记录，不存在另一条无审计读取通道。若未来读取磁盘文件，必须声明实际文件 Resource，不能沿用内部条目身份掩盖文件访问。

将该入口加入内置默认工具集合和免提示默认名单，仅影响相应默认值。插件工具不能靠自己的 effect 声明取得豁免，但用户可以明确将插件工具名加入免提示名单；“插件工具永远不能免提示”不成立。本片也不改变更早的 deny/ask、敏感读取或 prompt-for-all-reads 的优先级。

默认短索引由现有 collector 机制贡献，并用 `:render-tools` 与 `superchat--request-render-tool-guidance` 根据最终工具面渲染。请求 waterfall 移除取用入口后，索引不继续广告它，不自动补回工具或改写权限。模型若采用另一个实际可见入口，仍按那个入口的效果和授权执行；不能把获准 eval 等替代路径冒称为免提示参考读取。

Surface 范围明确为默认 chat/act，以及沿用现有只读默认规则的 inline-ask。当前 inline-ask 从免提示名单构造候选，加入 read_reference 因而会让该工具进入它的默认候选；这是本片明确包含的结果。显式工具集、none 和其他 Surface 的既有基线保持原约束；索引只广告最终工具面中存在的取用入口。

system/instruction 当前不计入 assembler 的用户消息预算。索引只占短小的 system 片段并遵守上述独立上限；正文通过工具结果按需进入上下文。沿用 `prompt.assembled` 的 system-chars 与 message-chars，以及最终 provider 证据报告成本；不声称系统文字免费，也不声称当前成本完全不可观测。本片不把整个操作指南常驻到系统提示。

分发承诺限定为：每一种声明支持的安装产物必须能加载参考模块，并读出规定 ID、内容版本及非空正文。现有手工源码安装和将来新增的归档安装分别验证。MELPA 构建是否包括目录由具体 recipe/产物决定；不能从缺少 recipe 推导出一个未生成的包已丢文件。将资料放入 `.el` 消除本功能对外部技能目录的依赖，不自动证明整个包的 Telega 等资源分发已正确。

### Slice 4: Emacs operating guide

正文围绕真实任务组织：查询已安装符号和文档；区分临时与持久修改；定位用户实际配置；处理变量 setter、local/default、有效 keymap 与 hook/advice；应用修改后读取真实结果。知识告诉模型如何调用既有能力，不替换用户配置结构，不创建另一套配置语言。

操作指南的内容随正在运行的包版本更新，不向用户技能目录覆盖文件。通过短索引发现和工具取用，无需用户输入 `>emacs-guide`，也不固定增加一轮独立模型技能匹配。沿用[方向提案](2026-09-16-emacs-ai-layer-tool-surface.md)的持久快捷键任务检验实际效果；简单现场操作按自身完成条件验证，不每次启动整套配置测试。

## Alternatives considered

**直接增加一个 >emacs-guide skill。** 它延续 Preset 解析和显式用户触发，prompt 型仍可以携带配置项；无法独立保证读取无配置副作用。既有技能功能保留，参考取用使用独立的数据接口。

**先重做技能播种和升级。** 可以改善用户 Preset 分发，但无助于区分资料与配置。随包参考数据直接跟随代码版本，本功能不等待整个技能系统迁移。

**先增加任意文件读取工具。** 这是可能独立有用的能力，但会引入路径、敏感文件与授权范围。本次按稳定 ID 取随包条目即可完成知识取用，不扩大成通用文件系统接口。

**给现场新建 profile 行或 Service。** 现有 chat-collectors 同时覆盖 chat 与 act；新增默认行还会增加自定义插件列表的升级问题。复用现有组合边界，按实际需要增加 contributor。

**为确保开箱即用而强制补齐用户名单。** 会把用户显式选择误当过期缓存。保留选择，通过文档和诊断说明默认能力何时可用；不引入新的强制工具例外。

**把来源和全文放进 system。** 它们不会受到当前用户消息预算约束，且用户选区不应变成系统指令。动态现场使用有预算的数据片段，系统提示只保留短的固定说明和索引。

## Acceptance criteria

以下是实现前的红到绿测试规格。**这些测试由 leader 写进独立的测试机 `scripts/m5-regression.el`，每条另配一个破坏脚本（`.scratch/m5-verification/mutants/`）证明它确实会红。** 用户 2026-09-16 裁定新测试不再进 `scripts/m3-regression.el`，另行分割测试机；该文件继续承载 M3/M3a/M4，同样归 leader 所有。新测试机自建 fixture、直接构建在 canonical smoke 的 helper 之上，不加载 M3 测试集——加载它会把 139 条测试一并拖进来，分割就只剩形式。worker 交付的端到端证据仍放各自的 scratch 目录，那是一次性的存证，不是回归。使用真实准备/选择/组装路径和假 transport 捕获最终请求，不用直接拼一个期望列表代替请求测试。

| 切片 | 核心红到绿场景 | 当前红点与实现后要求 |
|---|---|---|
| 1：文档发现 | 无活动计划的默认聊天从真实 driver 到假 transport，捕获模型实际收到的工具，并经可信 wrapper 调 describe-function 查询一个已知函数 | 当前默认请求看不到该工具。实现后可见且返回真实文档，默认 ask 模式无额外提示；显式省略它的名单和 none 不被扩大，prompt-for-all-reads 仍要求确认。 |
| 2：两入口同一现场 | 在 A 设置 mode、point、选区；分别 act、从 A 打开聊天后发送，以及 act inspect-send；捕获最终 provider 请求和相关 Tape 事实 | 当前聊天没有现场，act 拼入用户输入。实现后现场事实一致且仅出现一次；选区不在 system，intent.received 保持既有用户正文且不混入自动现场；预览送出不重复添加现场。 |

第二片另有两个必要的边界测试：其一，在请求排队后改名 A、创建占用旧名的 B、切换焦点并将 View 关联 C，已排队请求仍保留 A 的原快照；杀掉 A 后不读取 B，未关联的恢复 View 明确未知，持久化 payload 不含活 buffer。其二，卸载 chat-collectors 后新请求不再有现场贡献且来源 buffer 存活；小预算容不下显式选区时 provider 不启动，不通过系统槽绕过预算。

第二片的检查器用例还包括：预览后修改来源 buffer，再从预览正文删除选区并发送；最终请求保留检查后的正文，不重新抓取新选区或补回已删除内容，最终正文仍通过预算检查。

第三片验证：读资料前后的配置快照一致；正常包装调用留下工具证据；未知 ID 和路径输入失败且没有文件读取；工具从最终工具面移除后不广告该入口，仍可见但 Authority 更严格时正常申请或拒绝且索引不承诺免提示；Authority 拒绝时读取函数没有执行；默认 inline-ask 的新候选行为符合上文；有旧技能文件的用户仍可取得新包资料；安装产物和索引/正文长度校验通过。

第四片进行同一模型、相同初始 Emacs 状态的任务对照，分别记录完成结果、模型往返、耗时和用户介入，不预先断言速度提升。临时 buffer 设置、持久快捷键、绑定失效诊断分别检查对应真实结果。完成内容指南不等于通过这些产品验收。

生产实现各片按[开发验证要求](../../agents/development.md)运行相关行为、canonical smoke、Kernel 与受影响模块编译；当前这份文档交付只验证引用、变更范围和已报告的只读事实，不重跑未变更的整套回归。

## Risks

配置选择、工具可见性和执行授权具有不同职责；它们不能由“默认可用”一词合并。特别是 inline-ask 当前把免提示名单用于候选选择，必须测试并报告这一联动。

现场快照可能在执行前过时；它只证明捕获时事实，不授予写入权限，也不替代写入前的版本检查。Inspector、排队和 Scope 退出需要同时满足原有交付约定。

参考内容会影响模型判断，文字正确性需要随版本维护；数据读取不改变配置，不等于知识必然正确。首版不引入用户目录覆盖、外部资料自动加载或通用知识注册中心。

用户报告中的旁支分发问题单独记录在 [.scratch/emacs-reference-followups/](../../../.scratch/emacs-reference-followups/issues/01-package-descriptor-release-consistency.md)。依赖描述符在开发期允许滞后；tar 安装与 package-vc 的依赖来源不同，不将其混写为所有安装路径已经失败。
