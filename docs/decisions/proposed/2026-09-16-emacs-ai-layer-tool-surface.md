# Decision: Emacs AI layer through operating knowledge, live context and existing tools

Status: proposed — 用户于 2026-09-16 重申控制 Emacs 和修改配置的目标，并要求参考 Omarchy 的 agent 配置与 yayster.el；本记录已据源码调整优先级，尚未经用户决定

## Problem

[AI layer 蓝图](../accepted/2026-09-12-deepseek-harness-blueprint.md)要求默认安装即可使用，AI 可以通过 Superchat 操作 Emacs。[路线图](../../roadmap.md)中 M0–M4 及 M3a 的完成说明运行底座和界面组合已经交付；工具面仍是待决定事项。下一项产品能力需要以用户能完成的工作来定义。

现有 [tools](../../components/superchat-tools.md) 已提供 buffer 读取与编辑、符号发现和 `eval_elisp`；[superchat-synthesis.el](../../../superchat-synthesis.el) 已实现工具生成、持久化和批准后重新载入。能力覆盖面已有基础。缺口是一次操作如何知道自己作用于哪个真实对象、配置从哪里加载、实际行为是否改变，以及失败后哪些变化可恢复。

`superchat-act` 在 [superchat.el](../../../superchat.el) 捕获 buffer、文件、mode、point 和选区，dispatcher 将快照交给模型。[superchat-tools.el](../../../superchat-tools.el) 的普通 buffer 工具仍按名称定位，`edit_buffer` 只有旧文本唯一匹配检查；它没有调用现有 [Change Set](../../../superchat-change-set.el) 原子编辑入口，也没有要求读取版本。[superchat-workspace.el](../../../superchat-workspace.el) 的 Artifact 版本规则是可参考的实现，但普通用户 buffer 不因此成为 Artifact。

`eval_elisp` 实际执行 `(eval (read expression) t)`，具有当前 Emacs 进程的完整能力。保存一段配置、使当前会话生效和证明重新启动后生效，是不同结果。单次求值返回值不能证明三者都成立。

默认操作知识也是具体缺口。[superchat.el](../../../superchat.el) 的 general-answer 模板仍是通用问答指令，内置 code preset 只有一段通用编码指令；其他 collector 已补充工具使用提示，但未形成操作 Emacs 的完整方法。[superchat-skills.el](../../../superchat-skills.el) 已有从文本文件载入 Preset 的机制，随包提供的四份指南面向 code-review、refactor、planning 和 render-ui，尚无面向用户日常 Emacs 操作与配置的指南。源码中的异步 skill matcher 并不等于默认请求会自动使用它。

## Proposal

把 Superchat 的产品目标定义为：用户从聊天窗口或当前编辑位置交代任务后，AI 能观察真实 Emacs 状态、发现现成能力、执行操作、检查行为结果；当任务要求持久配置时，修改用户实际加载的配置并验证其持久效果。

优先让模型获得默认可用的 Emacs 操作知识、简短准确的现场和发现实际能力的方法，再用现有工具完成任务。保留 Emacs Lisp 作为通用执行语言。按任务暴露的问题补齐对象身份、冲突检查和结果结构，配置修改由这些能力组合完成。

### Operating knowledge and live context first

参考资料的载体、读取语义、默认配置边界和现场迁移由[四片契约草案](2026-09-16-emacs-operating-context-and-reference-contract.md)具体收敛。该草案取代本节通过 Preset/skill 应用路径提供参考资料的建议；其他工具与产品目标仍为方向提案。

Omarchy 的可移植方法是把操作知识随产品交付，并让 agent 使用系统现成命令；yayster 的可移植方法是每次用户请求提供简短现场，并让模型通过自省与 eval 操作当前 Emacs。这两项先于新工具清单。下方接口表继续作为可能需要补强的职责，不作为开始产品验证的前置工程。

- 默认指令说明如何区分提问和操作请求：对操作请求使用工具完成工作，对提问提供解释；不能要求用户每次指定一个额外的操作员角色。
- 随包提供面向使用者的 Emacs 操作指南，内容包括发现实际符号和文档、定位配置入口、变量作用域、有效按键绑定、hook/advice，以及各类修改如何生效和检查。仓库开发用 AGENTS.md 与用户运行时的操作指南具有不同受众。
- 参考资料按条目读取，不进入 Preset 应用或技能激活。短索引与现场复用现有 context collector，具体载体、权限和默认可达条件见四片契约草案；既有 >skill 配置语义保留。
- 提供有界的真实环境摘要，例如 Emacs 版本、来源 buffer/file/mode、工作目录、配置入口，以及与任务相关的包或配置结构。状态变化后按需重读，不把整个配置、所有 buffer 内容或所有符号一次塞入 prompt。
- 优先调用用户与已安装包原本就在使用的函数；反复出现的操作组合再决定是否抽成普通辅助函数或专门工具。默认可见的工具与指南描述一致，例如 code preset 当前并未选择 apropos，不能在提示中假定它已可调用。
- 内置参考资料跟随正在运行的包版本，读取不依赖用户技能目录播种。既有技能的初始化与升级属于独立功能，用户已有文本保留。

Omarchy 能预先规定自己的系统组件和配置布局；Superchat 面对用户各自的 Emacs。这里需要根据运行状态和用户配置建立少量有证据的本机信息，保持原有配置结构。

### Model-facing tools

下表是待决定的目标职责。`inspect_emacs`、`test_elisp` 仅为职责候选名；实现设计先用同一验收任务检验既有入口，再比较扩展既有 describe/eval 入口、复用普通辅助函数与新增模型工具的成本，不预定必须增加两个工具。精确参数、默认工具集合与兼容性届时收敛，不能把表中的建议当作当前已实现接口。

| 入口 | 建议职责 |
|---|---|
| 既有 `list_buffers` / `read_buffer` | 返回可读名称及准确对象引用、文本版本和必要上下文；支持有界读取，避免默认输出整个环境。 |
| 既有 `apropos` 及 describe 工具 | 发现真实存在的 command/function/variable，并提供签名、文档和定义位置；默认工具选择要让用户实际可用，无需手动拼装。 |
| 候选职责 `inspect_emacs` | 对明确的 context、symbol 或 key binding 做结构化检查：调用来源、mode、buffer-local/default 值、有效按键绑定和加载来源线索。不能把任意查询都视为无需确认的读取。 |
| 既有 `edit_buffer` | 在准确对象和预期版本上应用修改，复用 Change Set；返回实际修改、冲突或原错误。保存到磁盘是单独可报告的结果。 |
| 既有 `eval_elisp` | 执行没有专门工具覆盖的 Emacs API，显式指定必要的 buffer 上下文，并返回可区分的值与错误；继续由 Authority 决定是否执行。 |
| 候选职责 `test_elisp` | 在独立 Emacs 进程中检查明确的候选代码或配置，管理超时与退出，返回加载参数、断言结果、退出状态和有界输出。它不能冒充实时 Emacs，也不默认具有文件或网络隔离。 |
| 既有 `define_tool` | 当重复任务值得成为可调用能力时，复用已有生成与持久化机制；不把每次临时配置操作都注册成永久工具。 |

最终内置默认集合仍须与 Surface 上可见的 plugin tools 合并考虑；当前五个内置默认名字不代表所有默认模型工具。不得把表中的工具数当作全部工具数，也不能为收敛工具面静默移除已有能力。

### Accurate targets and useful results

“当前”默认指用户发起任务时捕获的对象。打开聊天窗口、弹出帮助或预览不能把编辑目标改成新选中的 buffer。模型需要操作另一个对象时，重新查询并明确选择。

进程内对象引用指向确切的 live buffer；对象改名不改变身份，对象被删除后引用失效，不以同名新 buffer 代替。引用与其进程生命周期一致，不写成可跨重启恢复的资源能力，也不赋予 Session 关闭用户 buffer 的权限。[Session EditorWorkspace 约定](../../protocols/session-editor-workspace-close.md)继续成立。

文本读取返回可检查的版本；写入在最终执行时核对目标与版本。用户或另一个任务已经改变内容时，返回冲突并重新读取。已有唯一旧文本匹配仍可作为附加检查，不能代替对象身份与版本检查。

配置文件若有用户尚未保存的修改，必须识别这项状态；不得以保存本次 AI 修改为由顺带保存或覆盖未授权的已有修改。跨多个文件的修改不自动获得全局事务保证，首个交付任务先限定一个配置落点。

工具结果明确区分操作成功、执行错误、目标失效和冲突；使用既有统一工具管线与 Tape 记录，不靠模型从一句 “Error: …” 自行猜测执行状态。不为这些结果建立第二套 Run 终态。

### Live changes and persistent configuration

| 用户意图 | 完成条件 |
|---|---|
| 临时调整当前编辑环境 | 指定对象的实际行为改变，并明确作用范围和本次会话有效性。 |
| 持久修改一项配置 | 正确配置位置已保存；当前实例与新启动验证实例分别报告结果。 |
| 创建可复用的 Emacs 命令 | 人类可读的 Elisp 定义有明确载入位置；通过 Emacs 正常命令机制可调用，按用户要求决定是否另作模型工具。 |

优先遵守用户现有配置结构。检查 `user-init-file`、`user-emacs-directory`、`custom-file`、相关模块和加载次序；不能默认把所有改动追加到 init.el 尾部，也不自动引入另一套“AI 管理配置”。

定义位置只是配置来源的线索。`symbol-file` 指向定义符号的库，不能证明用户最后一次赋值来自那里。存在多个赋值、延迟加载或模式覆盖时，检查实际生效的值或 keymap，并给出有证据的配置候选位置；来源仍不明确时只报告这一限制，不宣称已经找到了唯一配置来源。

变量配置保留其原生语义，例如 custom setter 与 buffer-local/default 的差异；快捷键配置检查有效 keymap 和可能覆盖它的 minor mode、emulation/Evil 状态及延迟加载。重新加载配置不得重复增加相同 hook/advice/绑定。

验证应对应任务的完成条件。临时、局部操作通常通过实际状态回读确认；要求跨重启保持的配置修改，在与目标任务相关的包和加载条件下验证。无需让每次普通操作都经过完整配置启动测试。启动一个空的 `emacs -Q` 仅加载几行表达式，不能证明用户完整配置能够启动。验证报告列出所加载的入口、依赖和未覆盖的图形界面或平台条件。

已知可逆的操作保留具体反向修改：本次文件补丁、旧绑定或旧变量值。撤销时再次检查用户是否已经继续修改，不能覆盖后续工作。恢复配置文件与恢复任意实时 Elisp 状态不是同一个保证；无法可靠撤销的副作用应报告，必要时在配置恢复后通过重新启动生效，不自动退出用户的工作 Emacs。

### Reuse existing composition and Authority

[Service](../../../superchat-service.el) 的 Definition/Provider/Consumer 机制已经存在；模型工具与内部 Service 不是一一对应关系。工具可以复用普通函数、Change Set 或真正具有独立生命周期的 Service。buffer、command、eval、process、file、org、magit、lsp 是需要覆盖的领域，不是必须先创建的八个 Service。

[渲染为普通函数的决定](../accepted/2026-09-16-rendering-is-a-function-not-a-service.md)提供同一取舍：没有真实替换或组合价值的共享函数保持普通函数。长时间运行的测试进程若需要统一的取消、超时与结果收取，可以形成有独立生命周期的接口；是否抽取，以真实消费者和责任为依据。

所有模型调用沿用 [tools](../../components/superchat-tools.md) 与 [Authority](../../components/superchat-authority.md)。现有 explore/ask/execute 模式、敏感读取、上层 deny、合成工具批准规则保持各自语义。本提案不以“AI layer”目标自动批准无提示执行，也不增加第二个授权中心。

完整的实时 Elisp 访问属于对当前 Emacs 进程的信任。Authority 在入口判断模型工具是否可以运行，并不把任意获准 Lisp 隔离在一个文件范围内。不能同时宣传任意 Elisp 完全控制与同进程权限数据不可改写；需要严格受限的任务使用能实施该限制的动作或真正隔离的后端。

### Reference implementations

本次对照固定到 Omarchy v3.8.4（`8fcc9d6048af4cb0e3af8512c78049857a3b53dd`）和 yayster.el（`397138d3d349e8008ef910c237b6d6a39d9fb420`）；Omarchy 当前默认分支的 4.0.0.alpha 不用于解释用户未明确版本的体验。

| 参考机制 | 源码依据与迁移判断 |
|---|---|
| Omarchy 给各 agent 安装同一份系统知识 | [安装脚本](https://github.com/omacom/omarchy/blob/v3.8.4/install/config/omarchy-ai-skill.sh)把同一份指南链接到 `.agents/skills`、`.claude/skills`、`.codex/skills`、`.pi/agent/skills`。关键是原生技能加载与随产品维护知识；Superchat 可以复用自己的载入机制。 |
| 指南说明具体操作和生效方法 | [用户操作 skill](https://github.com/omacom/omarchy/blob/v3.8.4/default/omarchy-skill/SKILL.md)列配置位置、命令发现、备份和典型任务；Hyprland 修改后 reload 并检查 configerrors，Waybar 修改后 restart。应移植这种具体方法，而非只加一句“请验证”。 |
| 动作与可发现说明来自现成实现 | [CLI](https://github.com/omacom/omarchy/blob/v3.8.4/bin/omarchy)从脚本元数据生成帮助和 JSON；[主题命令](https://github.com/omacom/omarchy/blob/v3.8.4/bin/omarchy-theme-set)复用配置生成和组件重载。Emacs 已有函数、interactive、docstring 和源码定位，可直接提供相应发现能力。 |
| 各 agent 的适配保持具体 | v3.8.4 的 [Pi 配置脚本](https://github.com/omacom/omarchy/blob/v3.8.4/install/config/pi.sh)仅额外安装明暗主题扩展，通用 launcher 负责安装和启动；所核查配置未向 Pi 安装另一套系统 prompt。仓库根 AGENTS.md 面向源码开发，安装脚本提供给运行中 agent 的系统操作知识是前述 skill。技能目录分发表示可由原生机制发现，不代表 Omarchy 强制每个 harness 每轮注入全文。 |
| 启动参数减少交互停顿 | [别名](https://github.com/omacom/omarchy/blob/v3.8.4/default/bash/aliases)的 cx/cy 使用各 agent 的不逐次审批模式；这是交互成本的一项来源，不能据此推出修改准确。Superchat 继续使用当前 Authority 模式。 |
| yayster 的少量工具与现场 | [实现](https://github.com/yayster/yayster.el/blob/397138d3d349e8008ef910c237b6d6a39d9fb420/yayster.el)有七个默认能力，以 apropos、describe、eval_elisp 为发现和执行主通道。每个用户 turn 注入最多 12 个 buffer 名、当前 buffer/mode、目录和工具清单；工具循环使用捕获的真实来源 buffer。它未实现配置持久化流程、独立结果验证或通用回滚。 |

上述资料支持“减少环境猜测和重复操作推理”的设计推断。本轮未在 Omarchy 或 yayster 中实测任务速度和成功率，不能声称它们已通过对照证明优于 Superchat。yayster 每个用户请求重新建立历史且串行调用模型；不复制其文本 ACTION/ARGS 协议、来源 buffer 死亡后继续在其他 current-buffer 执行的 fallback，或为小上下文放弃 Superchat 已接受的历史契约。

### Evidence for this proposal

本轮检查基于父提交 `7da55807`。当前 [回归集合](../../../.scratch/m3-verification/REGRESSION-SET.md) 的脚本 SHA-256 为 `30b2aebf36b62a44c63add7d89a69954eff7ebbc334892216100e37fffb47532`，与交付记录一致。本轮复跑 139 项：137 项预期结果、0 项意外、2 项真实模型用例跳过，无已知失败。56 个破坏脚本的有效性核对自交付记录，本轮未重跑整个破坏集合、canonical smoke 或真实模型。

本轮读取了 Emacs 本机文档：`with-timeout` 只在等待外部事件时检查，不能终止不让出执行的 Lisp 循环；`customize-set-variable` 使用变量的 custom setter 或设置 default；`symbol-file` 返回定义来源。这些结果支持独立验证、保留配置语义和谨慎解释来源的设计。

DevSpace 对蓝图中的独立 dsh checkout 返回 `Path is outside allowed roots`。本提案只引用 Superchat 已接受的蓝图与仓库内明确记录，不声称本轮核查过 dsh `c389f96` 的实现，也不据此宣称新增设计与 dsh 逐项一致。

## Alternatives considered

**所有工作只依靠现有 eval。** 它保留最大的覆盖面，也值得继续作为通用通道；但每个任务都必须重写对象定位、版本检查、失败识别与独立验证，重复责任直接影响可靠性。本提案保留 eval，并把这些可复用责任落实在既有或确有必要的新入口。

**为每个 Emacs API 或每个包预先建一个模型工具。** 稳定、高频操作可有专门工具；完整枚举会不断追随用户安装的包，并增加模型 schema 和维护负担。先通过 Emacs 的发现机制与 Elisp 覆盖能力，真实重复任务再决定专门适配。

**一次建立八套能力 Service。** 领域清单不证明 provider 替换或生命周期需求，且现有注册基础设施已经存在。先让目标任务暴露实际需要共享的职责，再复用现有 Service 机制。

**先拆分大型源码文件。** 文件规模可以作为维护信号，但拆文件本身不证明 AI 能完成一个新的 Emacs 任务。能力实现中遇到明确职责与测试接缝时可以抽取，不能用行数或文件数代替产品验收。

**把配置统一迁入 Superchat 自有文件或 DSL。** 这样容易控制格式，但会产生第二个配置来源，改变既有 Emacs 配置组织。首个交付采用用户已有加载结构；新增文件只在存在明确加载位置和用户意图时发生。

## Acceptance criteria

先以同一模型和同一组明确任务比较当前默认行为与增加操作指南、现场上下文后的行为：当前 buffer 的临时设置、持久快捷键修改、已有快捷键不生效的诊断。分别记录首次完成情况、模型往返次数、耗时和用户中断次数；按各自任务检查真实结果。补强工具以这些试验暴露的重复问题为依据，不预先宣布技能文字已经保证准确快速。

首个完整配置验收任务是：“给当前 major mode 增加一个指定快捷键，立即可用，重新启动后仍然存在，并能撤销本次改动。”键与命令由验收场景明确提供，不要求用户先知道 keymap 名称或配置文件路径。

- 聊天入口与 `superchat-act` 都能完成该任务；切换到聊天或预览窗口不改变来源对象。
- 检查返回有效绑定、相关文档与配置落点证据，区分定义文件和用户配置文件。
- 在用户原有配置组织中做一次局部修改；读取之后发生用户修改时报告冲突；同名新 buffer 不接管失效目标。
- 当前目标 mode 中的有效按键绑定命中指定命令；明确记录并验证共享或继承 keymap 的作用范围，无相关共享或继承关系的其他 mode 不受影响。
- 使用指定的候选配置入口和必要依赖启动新的 Emacs 验证实例；目标 mode 激活后仍有正确绑定，重复加载不重复产生副作用。
- 语法错误、缺包、错误 keymap 和延迟加载覆盖能够产生可观察失败，不能把“eval 返回”或“文件写入”当作全部成功。
- 对验证进程中的超时或错误，可以终止该验证而不退出用户的工作 Emacs。具体文件与网络隔离须另行证明。
- 撤销恢复本次配置补丁和已知旧绑定；若用户在此后继续修改，返回冲突而不覆盖；不承诺任意 Elisp 的通用回滚。
- 至少一次真实模型端到端完成任务，并由独立检查读取实际绑定及新实例结果；覆盖既有聊天与当前 buffer 两条用户路径。
- 按 [development verification](../../agents/development.md) 完成相关回归、canonical smoke、Kernel 与所改模块编译；受保护回归集合由其既定所有者维护。

## Risks

完整配置可以包含任意代码、平台依赖、GUI 条件和延迟动作。首个任务的成功证明其明确覆盖的配置环境，不能宣称所有 Emacs 配置都可静态识别或可靠重载。

实时 eval 可能产生无法逆转的外部效果，也可能阻塞 Emacs 主执行。独立测试降低在工作实例试错的成本，不能保证获准代码的所有后续行为都可被当前进程强制中断或撤销。

配置文件、未保存 buffer 与运行状态有不同版本和提交时刻。结果必须逐项陈述已确认事实，避免一处成功掩盖另一处失败。文本修改的原子性不等于磁盘、内存、工具审计三者构成同一事务。

本记录是产品与接口方向提案。精确 schema、默认工具选择、配置来源无法唯一确定时的交互，以及对已知可逆 live 操作的具体覆盖，需要在实现设计中收敛；不把这些未定项作为 worker 可自行扩展全局权限或承诺通用回滚的授权。
