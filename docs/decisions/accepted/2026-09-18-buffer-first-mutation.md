# M7 — Buffer-first mutation：模型的写能力以 Buffer 为界

Status: accepted — 用户 2026-09-18 提出原则，对四个范围问题答"都按建议"，审阅草案后对两个命名问题都选 (a)，并要求收紧四处契约细节（持久化的措辞与 `save_buffer` 的批准组合、`/changes` 的推导与局限、`open_file` 的宿主副作用、保留内部 Buffer 保护）；本稿已全部并入。实现状态逐片记在[路线图](../../roadmap.md)。

## 一句话

**普通 Agent 的 mutation capability 是 Buffer；持久化不是默认 mutation capability。** Superchat 操作的是 Emacs 的工作对象 Buffer，不是磁盘文件：模型可以读文件、把文件打开成 Buffer、修改 Buffer；把 Buffer 写回磁盘通常由用户和用户的 Emacs 环境完成。`eval_elisp` 等完整控制类工具是显式逃生口，每次都问，批准后不受这条保证约束。

```text
File ──visit──▶ Buffer ──▶ read / edit ──▶ modified（work.el*）
                                              │
                                              └─ 持久化：用户 / Emacs 环境
```

## 为什么

- 2026-09-18 有人值守的 M5 验收（`.scratch/m5-config-acceptance/runs/attended-chat-01/REVIEW.md`）：模型在第 57 秒用 `edit_buffer` 改了被明确保护的来源文件 `work.el`，第 67 秒用 `save_buffer` 存盘。两步都**没有问任何人**——M6 的默认档位规定工作区内的内置写操作不问——在场的人没有机会拦。
- 继续研究"怎样让模型更安全地写文件"（保护名单、可执行的写保护范围）是在给一个不该有的原语打补丁。用户的问题更接近本质：**一个 Emacs-native 的 Agent 为什么要拥有"写文件"这个原语？** 在 Emacs 里，用户工作的对象本来就是 Buffer，保存从来是 Buffer 生命周期里的另一件事；"有未保存修改"几十年来就由 `*` 标记、`buffer-modified-p`、undo 历史、退出时的保存提问来表达。
- 这是对 DeepSeek Harness 的**有意偏离**（它有 write/edit 文件工具）。理由：dsh 的宿主没有 Buffer 这个一等对象，Emacs 有。此前"dsh 有没有对应结构"的裁决方式在这一点上由用户明确覆盖。

## 一、工具面

| 工具 | 决定 | 说明 |
|---|---|---|
| `list_buffers`、`read_buffer` | 保留，默认 | — |
| `read_file` | 保留，默认 | 只看磁盘，不创建 visiting buffer（2026-09-18 契约不变）。读有两条路，写只有一条 |
| **`open_file`** | **新增，默认** | File → Buffer，见第二节 |
| `edit_buffer`、`append_to_buffer`、`replace_buffer` | 保留，核心；`edit_buffer` 默认 | 只改 Buffer；**去掉** `sandbox_permissions`／`justification` 参数和目标路径检查（见第四节） |
| `save_buffer` | **退出默认工具面；保留为"每次都问"的窄例外** | 需要模型保存时，问的是一句能看懂的话（"把 buffer X 存到路径 Y"），而不是逼模型走 `eval_elisp` 写 `(save-buffer)`。**只有 Preset／Request 的显式工具名单点名时才存在；任何档位都不会把它加进工具面**。批准组合见第三节 |
| `write_file` | **退役** | 它绕过 Buffer、修改标记、undo、major mode 和用户的查看。新建文件 = `open_file` 一个还不存在的路径，然后编辑 Buffer |
| `eval_elisp`、shell 类、`define_tool` 及 agent 自写工具 | 不变：完整 Emacs 控制，每次问 | 逃生口。批准 = "这一次允许模型越过 Buffer-only 模型"。它能 `(save-buffer)`、`(write-region …)`，本契约**不假装**约束它 |
| `workspace_write`、`workspace_publish`、`workspace_read`、`workspace_info` | 保留 | region Artifact 写的是 Buffer；generated／durable Artifact 写的是 Superchat 自己数据目录里的文件——那是 Superchat 的状态，不是用户文件 |
| `rename_file`、`delete_file` | 不做 | 本来就没做；现在有了不做的理由 |

Superchat 自己的落盘（Tape、记忆、发布的 Artifact、agent 自写工具的存档、telega 规则）都在它自己的数据目录里，不属于本契约说的"持久化用户文件"。

## 二、`open_file`

- 参数：`path`。返回 Buffer 名（之后模型只拿 Buffer 身份工作，不再拿 pathname 到处写）、是否新文件、行数、major mode，以及"这个 Buffer 此前是否已经打开／是否已有未保存修改"。
- 语义：**把磁盘上的对象纳入当前 Emacs 工作集**，即 `find-file-noselect`。
- **它不是纯读取。** `open_file` 不获得焦点、不修改磁盘，但它执行 Emacs 正常的文件访问语义：major mode、用户配置的 mode hook 和 `find-file-hook`、文件局部变量处理，以及由此产生的 Emacs 自己的 UI 提问（例如"不安全的局部变量，是否应用"）。**这些属于宿主环境行为：不经过 Superchat 的批准，也不保证进入 Tape。**（2026-09-18 的有人值守验收里，正是这样一个提问出现在一次已批准的 `eval_elisp` 中间，既不在 Tape 上，也没被脚本记下。）工具描述和 README 都要写明这一点。真正"只看磁盘字节、不进入 Emacs 工作集"的入口始终是 `read_file`。
- 不抢焦点：不切换窗口、不弹出 Buffer；已打开的文件直接返回现有 Buffer，不重复打开、不 revert。
- 不存在的路径：得到一个访问该路径的空 Buffer，磁盘上什么都没有，直到用户保存。父目录不存在时照样返回 Buffer，并在结果里说明"保存前需要先有这个目录"。
- 拒绝：远程（TRAMP）路径、目录、Superchat 自己的数据目录、超过 `large-file-warning-threshold` 的文件（不触发 Emacs 的大文件提问）。
- 权限：三个档位都允许（它不改任何东西）；仍经过 `tools/pre-execute`。

## 三、档位

**档位只决定已有工具怎样被允许执行，不决定模型有什么工具。** 工具集合由 Preset／Request 决定；任何档位都不扩张它。

| 档位 | 含义 | 读（buffer／file／open_file） | 编辑普通 Buffer | 完整控制 | MCP／第三方 |
|---|---|---|---|---|---|
| `read-only` | 只观察 | 允许 | **拒绝**（不可升级） | 拒绝 | 拒绝 |
| **`edit-buffers`**（默认） | 普通 Buffer 可修改，完整控制类仍需批准 | 允许 | 允许，不问 | 每次问 | 可信服务器不问，其余每次问 |
| `danger-full-access` | 完整控制类也不询问；批准策略默认 `never` | 允许 | 允许 | 不问 | 不问 |

**默认档位改名为 `edit-buffers`。** `workspace-write` 在新模型下已经不准确（没有 workspace 边界，也不写磁盘），只作为兼容别名：`/permissions workspace-write` 解析为 `edit-buffers`；旧 Tape 上的 `workspace-write`（`permission.level`、`sandbox.mode` 事件）读出来就是 `edit-buffers`；**不再写入任何新的 `workspace-write` 事件**；UI、新文档、给模型的说明都用 `edit-buffers`；用旧名时给一次弃用提示；不迁移、不重写旧 Tape——历史事实不变，新世界不再积累错误的名字。用户选项 `superchat-sandbox-default-mode` 的旧值同样按别名接受。

**`save_buffer` 的批准组合**——它是"每次都问"的工具，"问"就是现有的批准缝，不为它另造一套：

| 沙箱模式 + 批准策略 | 显式提供了 `save_buffer` 时 |
|---|---|
| `read-only`（任一策略） | 拒绝 |
| `edit-buffers` + `ask` | 每次问 |
| `edit-buffers` + `never` | 拒绝（不能弹问题） |
| `danger-full-access` + `never`（该档位的默认） | **拒绝**——即使 Preset 显式提供了它，也不能弹问题 |
| `danger-full-access` + `ask`（显示为 `custom`） | 每次问 |

也就是说 `save_buffer` 在固定档位阶段里恒为 `ask`（`read-only` 下为 `deny`），不因 `danger-full-access` 变成 `allow`：持久化不是默认 mutation capability，哪怕在最宽的档位。

**Buffer 能力的边界（保留的执行点）。** 删除 M6 的写沙箱时，留下一个很小的 Buffer 目标判定，它不叫 sandbox，也没有路径概念：

```text
ordinary user buffer        → 可编辑
Superchat internal buffer   → 拒绝（任何档位）
```

内部 Buffer 指聊天 View、Session 拥有的 Buffer、` *superchat…*` 内部 Buffer——沿用 M6 现有判定（`superchat-sandbox-owned-state-buffer-p`／`superchat-sandbox-internal-buffer-p`），编辑类工具在动手前调用它。没有它，"普通 Buffer 都可编辑、Superchat 自己的 Buffer 永远不可编辑"就没有执行点。

工作区根（`/workspace` 绑定 → 来源 project root → 启动目录）**不再是写权限的边界**：它只保留为"相对路径相对于谁"和给模型的上下文说明。批准策略 `ask`／`never` 的其余规则不变；`never` 下完整控制一律拒绝，`ask_user` 不提供。

## 四、对用户环境的诚实承诺

> **Superchat 的默认修改路径不执行持久化。** 普通模型修改止于 Buffer；持久化通常由用户或用户的 Emacs 环境完成。`save_buffer` 是显式提供时才存在的窄例外，每次调用都需要批准。

- 用户装了 `super-save`、开了 `auto-save-visited-mode`、挂了保存相关 hook 或 format-on-save：Agent 改了 Buffer 之后随之落盘，是用户自己选的编辑环境语义，不是 Superchat 越权。Superchat **不探测、不对抗、不绕开**这些机制，也不试图把 Emacs 重新沙箱化。
- 因此本契约**不承诺**"Agent 编辑 Buffer 不会改变磁盘"。README 要用这句原话说明，并举 `super-save` 为例。
- 给模型的说明同样如实：你改的是 Buffer；保存由用户决定；需要落盘才能继续的事（比如要跑外部命令读这个文件）就告诉用户，或用 `ask_user` 请用户保存。

## 五、未保存修改的出口

"你马上就看到 `work.el*`"只在用户正看着那个 Buffer 时成立；`open_file` 打开的 Buffer 默认在后台，子 agent 和工作流更没人看着。出口只做汇总，**不创造新的领域对象**（没有 mutation ledger、待定变更、写入证据、文件批准状态）。

**候选的定义**——完全由 Tape 加当前 Emacs 状态推导：

```text
该 Run（或该 Session）的结构化 Buffer 修改工具事件的目标 Buffer
    （edit_buffer / append_to_buffer / replace_buffer / workspace_write 的 region 目标）
        ∩
此刻仍 live 且 buffer-modified-p 为真的 Buffer
        ↓
/changes 候选
```

`buffer-modified-p` 只能说"这个 Buffer 现在有未保存修改"，说不了"这些修改是谁造成的"；上面的交集是不另造账本时最干净的定义。它意味着清单里的 Buffer 是"这个 Run 用结构化工具碰过、现在有未保存修改"，其中也可能混有用户自己的编辑——这是可以接受的，因为用户看的是差异。

**明确的局限：`eval_elisp`（及其它完整控制类工具）自己修改了哪个 Buffer，Superchat 不承诺能归因，因此可能不出现在清单里。** 不为补这个洞去监控整个 Emacs。

1. **Run 结束时的提醒（用户 2026-09-18 确认的表述）。** Run 结束时，若该 Run 的结构化编辑工具留下仍 live 且 modified 的 Buffer，Superchat 发出一次**非持久化提醒**（echo area 的一条消息）。该提醒不进入 Tape，也不保证保留在聊天历史中。用户可随时通过 `/changes` 从 Tape 的既有 `tool.completed` 目标字段与当前 Buffer 状态重新计算候选列表。提醒承担"有机会看到"，`/changes` 承担"可重新查询"；两者都是 UI，不是 durable truth，也不为此新增 Tape 事件。
   - **`tool.completed` 上的 `:buffer`／`:buffer-file` 是执行结果证据，不是新领域对象。** Tape 原本不记录工具参数（只有参数个数和结果指纹），所以结构化编辑工具把自己解析出的目标交给执行器，由执行器写在该次调用的 `tool.completed` 事实上；`eval_elisp` 的执行体不设置它，这正是"不归因"的来源。两条约束：
     - `:buffer` 记录**真正被修改的 base buffer** 的名字（indirect buffer 记它的 base），不是模型传进来的名字。
     - `:buffer-file` 是**当次执行时观察到的**文件身份，只用于证据和定位；`/changes` 展示和判断的始终是 live buffer 此刻的状态（此刻的名字、此刻访问的文件、此刻是否 modified）。Buffer 后来被 `rename-buffer` 或 `set-visited-file-name` 过，不把旧路径当作当前真相。定位顺序：先按记录的名字找 live buffer，找不到再按记录的文件找；两者都找不到的就不列出（不为此另存 Buffer 身份）。
2. **命令 `/changes`**（及 `M-x superchat-changes`）：列出本 Session 的候选。对每一个：`diff` 是**观察**（Emacs 自带的 `diff-buffer-with-file`）；`save` 和 `revert` 是**明确的用户操作**（用户自己执行的 `save-buffer`／`revert-buffer`），所以它们与"模型默认不持久化"不冲突。它是用户的命令，不是模型的工具。
3. 子 agent、工作流、inline 入口的结构化修改归到发起它们的 Session 名下，同样进入候选。

## 六、M6 里要删、要留、要改的

**删**（第二片的大部分）：`write_file` 及其工具定义；`edit_buffer`／`append_to_buffer`／`replace_buffer`／`workspace_write` 的 `sandbox_permissions`＋`justification` 参数与教学文字；`superchat-sandbox-run-direct-write` 的"拒绝→升级→重新解析→写"流程、两行拒绝标记、`superchat-sandbox-escalation-*`、`superchat-sandbox-path-writable-p`／`writable-roots`／`canonical-path` 中只为写检查服务的部分；`superchat-llm.el` 里对 `[sandbox: file access denied` 的分类和逐调用升级反馈；运行时上下文片段里"可写目录"那句；对应的 m6 回归（targets/、escalation/、write/ 各组）和 read_file 契约里提到 `write_file` 的文字。

**留**：批准缝（`superchat-approval.el`）整个不动；档位与三个 Session 事件；固定档位阶段与 `tools/pre-execute`；完整控制类、MCP 信任名单；**内部 Buffer 保护这个执行点**（第三节"Buffer 能力的边界"：`superchat-sandbox-owned-state-buffer-p`／`internal-buffer-p` 和编辑类工具动手前对它的调用）；`read_file`；`save_buffer` 的实现本身（保存整个 base buffer、不问文件名）——但它的门从"目标路径检查＋升级"换成档位阶段的"每次问"。

**改**：默认档位改名 `edit-buffers`（旧名作别名，见第三节）；`read-only` 对编辑类工具从"拒绝并提供升级"改成直接拒绝；`save_buffer` 在档位阶段恒为 `ask`；默认工具面 = `list_buffers` `apropos` `describe-function` `read_buffer` `read_file` `open_file` `edit_buffer` `eval_elisp`；工具结果的证据行保留"Target／Layer"两行（Layer 恒为"in memory only"），去掉"Checks: sandbox mode…"。

## 七、M5 验收口径随之调整

T2（加快捷键、当场生效、重启后还在、可撤销）里，"重启后还在"不再是模型一个人的事：

- 模型：找到实际的 init 文件 → `open_file` → `edit_buffer`；让绑定当场生效（目前仍需 `eval_elisp`，每次由人批准；将来的 `define_key` 类结构化工具不在本里程碑）。**到此为止，不保存。**
- 人：在 `/changes` 或审阅窗口里看差异，决定保存。验收脚本现有的"审阅后才加载"那道门扩成"审阅并保存"。
- 独立检查器：保存之后再做重启检查；撤销同理（模型改回 Buffer，人审阅并保存）。
- 答复准确性：模型必须如实说"我改了 Buffer X，尚未保存，需要你保存后重启才生效"；说"已保存"即不通过。

要如实记录的局限：2026-09-18 那种失败（模型直接去改 `work.el`）在新模型下**仍可能发生在 Buffer 层**——磁盘不动、`/changes` 里一眼可见、一键还原，但"模型选错了目标"这件事本契约不解决。想让普通 Buffer 工具拒绝某些 Buffer（保护名单）可以以后做，且同样不约束 `eval_elisp`。

## 八、验收

1. 默认工具面下，脚本模型经真实管线依次 `open_file` → `edit_buffer`：磁盘字节不变、Buffer 为 modified、没有任何批准提问；Run 结束的聊天里列出该 Buffer；`/changes` 能看差异、保存、还原。
2. `write_file` 不存在（注册、选择、执行都找不到）；编辑类工具的 schema 里没有升级参数；发 `sandbox_permissions` 会因未知参数失败而不是被忽略。
3. `save_buffer` 不在默认面，任何档位都不会把它加进工具面；显式点名时按第三节的五行组合表逐行验证（含 `danger-full-access + never` 拒绝、`danger-full-access + ask` 每次问）；保存的是 base buffer 的整个文件。
4. `read-only`：编辑类工具直接拒绝，文字里不再出现升级提示。
5. Superchat 自己的状态 Buffer 任何档位都改不了（沿用并保留 M6 的回归）；删除写沙箱之后这条回归仍然为绿，证明执行点还在。
5a. 档位名：新写入的事件、`/permissions` 的显示和补全、给模型的运行时上下文都是 `edit-buffers`；`/permissions workspace-write` 生效并给出弃用提示；一条只含旧名事件的 Tape 重开后读出 `edit-buffers`，且 Tape 行原样未动；此后不再出现新的 `workspace-write` 事件。
5b. `/changes` 的候选只由 Tape 事件 ∩ 当前 modified 推导：结构化工具改过的 Buffer 出现；只被 `eval_elisp` 改过的 Buffer 不出现，且这一点被测试明确断言（作为已声明的局限，而不是缺陷）；已保存或已关闭的 Buffer 不出现。
6. `open_file`：不抢焦点；已打开的返回原 Buffer；不存在的路径不创建磁盘文件；远程、目录、数据目录、超大文件报错；确实运行了 major mode、mode hook 和 `find-file-hook`（与 `read_file` 的回归形成对照），且这些宿主行为不产生任何 Superchat 批准事件。
7. 用户环境：测试里挂一个"Buffer 一改就保存"的 hook（模拟 `super-save`），编辑后磁盘确实变了，且 Superchat 没有任何拒绝或告警——证明它不对抗用户环境。
8. 既有套件、canonical smoke、Kernel 检查、加载、编译警告：只允许因工具面变化而动的基线变化，逐个列在提交说明里。
9. 真实模型：有人值守的 T2，chat 与 act 两个入口，按第七节口径。M5 在此之后才可能关闭。

## 九、分片（每片独立验收、leader 提交）

1. `open_file` ＋ 未保存修改的出口（Run 结束清单、`/changes`）。只加不删，现有行为不变。
2. 写路径切换：`write_file` 退役，`save_buffer` 退出默认面并改为"每次问"，编辑类工具去掉目标检查与升级参数，`read-only` 直接拒绝；删除第六节"删"列的代码与回归；README／CHANGELOG／组件文档同步。
3. 验收脚本按第七节改口径（审阅并保存），用脚本模型排练通过后，用户在场跑 T2。
4. （M7 之后）`superchat-authority.el` 改名为 `superchat-tool-execution.el`——用户裁定 ④(a)，顺延至此。

## 交付后的更正（2026-09-19）

第二片漏删了一句与本契约相反的模型可见文字：`eval_elisp` 的工具说明仍写着 "For persistence, save the actual loaded configuration and verify in a fresh Emacs that loads it"（M5 时期留下的）。有人值守的三次 act 运行都经 `eval_elisp` 自行存盘并另起 Emacs 自证，与这句话逐项对应。已改为"本工具不用于保存文件；保存由用户决定；要让配置长期有效，就修改 Emacs 实际加载的那个配置文件的 Buffer，并告诉用户保存哪个 Buffer"，并加回归 `m7/write-path/no-default-tool-tells-the-model-to-persist`（默认工具面上任何说明都不得要求模型保存）。第六节"要删"的清单应当包含"所有要求模型自行持久化的模型可见文字"，当时没有写这一条。

## 不做

文件系统写沙箱的任何加强；保护名单；`define_key`／`set_variable`／`add_hook` 等结构化 Emacs 工具（方向认可，另立里程碑）；对 `super-save` 等用户环境的探测或提示；把 `eval_elisp` 关进沙箱。

## 已确认的选择（2026-09-18）

| 问题 | 用户的选择 |
|---|---|
| `save_buffer` 的去留 | 退出默认工具面，保留为每次都问的窄工具 |
| 工作区边界是否还管 Buffer 编辑 | 不管；只保留内部 Buffer 保护 |
| 未保存修改的出口 | 做：Run 结束清单 + `/changes` |
| 与 M5、改名的先后 | M7 先行；M5 在 M7 之后用新口径重跑 T2；`superchat-authority.el` 改名顺延到 M7 之后 |
| 默认档位的名字 | 改为 `edit-buffers`，`workspace-write` 作兼容别名，不再写入新事件 |
| `danger-full-access` 下 `save_buffer` 是否进工具面 | 不进：档位不负责扩张工具集合 |
