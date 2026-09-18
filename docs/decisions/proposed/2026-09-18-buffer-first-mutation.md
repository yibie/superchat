# M7 — Buffer-first mutation：模型的写能力以 Buffer 为界

Status: proposed — 用户 2026-09-18 提出原则并对四个问题答"都按建议"；本文是据此写的契约草案，**等用户审阅，未动代码**。

## 一句话

Superchat 操作的是 Emacs 的工作对象 **Buffer**，不是磁盘文件。模型可以读文件、把文件打开成 Buffer、修改 Buffer；**把 Buffer 写回磁盘不是模型的普通能力**，由用户和用户的 Emacs 环境负责。`eval_elisp` 等完整控制类工具是显式逃生口，每次都问，批准后不受这条保证约束。

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
| `save_buffer` | **退出默认工具面；保留为"每次都问"的窄工具** | 用户裁定 ①(a)。需要模型保存时，问的是一句能看懂的话（"把 buffer X 存到路径 Y"），而不是逼模型走 `eval_elisp` 写 `(save-buffer)`。只有显式工具名单或 Preset 点名时才提供 |
| `write_file` | **退役** | 它绕过 Buffer、修改标记、undo、major mode 和用户的查看。新建文件 = `open_file` 一个还不存在的路径，然后编辑 Buffer |
| `eval_elisp`、shell 类、`define_tool` 及 agent 自写工具 | 不变：完整 Emacs 控制，每次问 | 逃生口。批准 = "这一次允许模型越过 Buffer-only 模型"。它能 `(save-buffer)`、`(write-region …)`，本契约**不假装**约束它 |
| `workspace_write`、`workspace_publish`、`workspace_read`、`workspace_info` | 保留 | region Artifact 写的是 Buffer；generated／durable Artifact 写的是 Superchat 自己数据目录里的文件——那是 Superchat 的状态，不是用户文件 |
| `rename_file`、`delete_file` | 不做 | 本来就没做；现在有了不做的理由 |

Superchat 自己的落盘（Tape、记忆、发布的 Artifact、agent 自写工具的存档、telega 规则）都在它自己的数据目录里，不属于本契约说的"持久化用户文件"。

## 二、`open_file`

- 参数：`path`。返回 Buffer 名（之后模型只拿 Buffer 身份工作，不再拿 pathname 到处写）、是否新文件、行数、major mode，以及"这个 Buffer 此前是否已经打开／是否已有未保存修改"。
- 语义：**把磁盘上的对象纳入当前 Emacs 工作集**，即 `find-file-noselect`。它走 Emacs 的完整打开流程：major mode、`find-file-hook`、文件局部变量（含 Emacs 自己的"不安全局部变量"提问——那是用户环境的提问，不是 Superchat 的批准）。这正是它和 `read_file` 的区别，描述里要写明。
- 不抢焦点：不切换窗口、不弹出 Buffer；已打开的文件直接返回现有 Buffer，不重复打开、不 revert。
- 不存在的路径：得到一个访问该路径的空 Buffer，磁盘上什么都没有，直到用户保存。父目录不存在时照样返回 Buffer，并在结果里说明"保存前需要先有这个目录"。
- 拒绝：远程（TRAMP）路径、目录、Superchat 自己的数据目录、超过 `large-file-warning-threshold` 的文件（不触发 Emacs 的大文件提问）。
- 权限：三个档位都允许（它不改任何东西）；仍经过 `tools/pre-execute`。

## 三、档位（用户裁定 ②(a)：工作区边界不再管 Buffer 编辑）

| 档位 | 读（buffer／file／open_file） | 编辑 Buffer | `save_buffer`（非默认） | 完整控制 | MCP／第三方 |
|---|---|---|---|---|---|
| `read-only` | 允许 | **拒绝**（不可升级） | 拒绝 | 拒绝 | 拒绝 |
| **`workspace-write`（默认；名字见"待定"）** | 允许 | 允许，不问：任何普通 Buffer | **每次问** | 每次问 | 可信服务器不问，其余每次问 |
| `danger-full-access` | 允许 | 允许 | 不问 | 不问 | 不问 |

- 两类 Buffer 任何档位下普通 Buffer 工具都不能改：Superchat 自己的状态 Buffer（聊天 View、Session 拥有的 Buffer、` *superchat…*` 内部 Buffer）——沿用 M6 现有判定。
- 工作区根（`/workspace` 绑定 → 来源 project root → 启动目录）**不再是写权限的边界**：编辑不落盘，边界要防的事不存在了。它只保留为"相对路径相对于谁"和给模型的上下文说明。
- 批准策略 `ask`／`never` 不变；`never` 下 `save_buffer` 和完整控制一律拒绝，`ask_user` 不提供（M6 规则不变）。

## 四、对用户环境的诚实承诺

> **Superchat 自己不执行持久化。** 一个被修改的 Buffer 是否、何时被写回磁盘，由用户和用户的 Emacs 环境决定。

- 用户装了 `super-save`、开了 `auto-save-visited-mode`、挂了保存相关 hook 或 format-on-save：Agent 改了 Buffer 之后随之落盘，是用户自己选的编辑环境语义，不是 Superchat 越权。Superchat **不探测、不对抗、不绕开**这些机制，也不试图把 Emacs 重新沙箱化。
- 因此本契约**不承诺**"Agent 编辑 Buffer 不会改变磁盘"。README 要用这句原话说明，并举 `super-save` 为例。
- 给模型的说明同样如实：你改的是 Buffer；保存由用户决定；需要落盘才能继续的事（比如要跑外部命令读这个文件）就告诉用户，或用 `ask_user` 请用户保存。

## 五、未保存修改的出口（用户裁定 ③(a)）

"你马上就看到 `work.el*`"只在用户正看着那个 Buffer 时成立；`open_file` 打开的 Buffer 默认在后台，子 agent 和工作流更没人看着。出口只做汇总，**不创造新状态**：

1. **每个 Run 结束时**，聊天里追加一段：这个 Run 改过、当前仍未保存的文件 Buffer 清单（Buffer 名、文件路径、是否新文件）。数据来源是 Tape 上本 Run 的 `edit_buffer`／`append_to_buffer`／`replace_buffer`／`workspace_write` 记录 ∩ 此刻 `buffer-modified-p` 为真的 Buffer。没有就不出现。
2. **命令 `/changes`**（及 `M-x superchat-changes`）：列出本 Session 改过且仍未保存的 Buffer；对每一个可以看差异（Emacs 自带的 `diff-buffer-with-file`）、保存（`save-buffer`，用户自己执行）、还原（`revert-buffer`）。它是用户的命令，不是模型的工具。
3. 子 agent、工作流、inline 入口改过的 Buffer 归到发起它们的 Session 名下，同样出现在清单里。
4. 不引入"待定变更""写入证据""文件批准状态"之类的新对象；Emacs 的 modified 标记就是状态。

## 六、M6 里要删、要留、要改的

**删**（第二片的大部分）：`write_file` 及其工具定义；`edit_buffer`／`append_to_buffer`／`replace_buffer`／`workspace_write` 的 `sandbox_permissions`＋`justification` 参数与教学文字；`superchat-sandbox-run-direct-write` 的"拒绝→升级→重新解析→写"流程、两行拒绝标记、`superchat-sandbox-escalation-*`、`superchat-sandbox-path-writable-p`／`writable-roots`／`canonical-path` 中只为写检查服务的部分；`superchat-llm.el` 里对 `[sandbox: file access denied` 的分类和逐调用升级反馈；运行时上下文片段里"可写目录"那句；对应的 m6 回归（targets/、escalation/、write/ 各组）和 read_file 契约里提到 `write_file` 的文字。

**留**：批准缝（`superchat-approval.el`）整个不动；档位与三个 Session 事件；固定档位阶段与 `tools/pre-execute`；完整控制类、MCP 信任名单；Superchat 自己状态 Buffer 的判定（`superchat-sandbox-owned-state-buffer-p`／`internal-buffer-p`）；`read_file`；`save_buffer` 的实现本身（保存整个 base buffer、不问文件名）——但它的门从"目标路径检查＋升级"换成档位阶段的"每次问"。

**改**：`read-only` 对编辑类工具从"拒绝并提供升级"改成直接拒绝；默认工具面 = `list_buffers` `apropos` `describe-function` `read_buffer` `read_file` `open_file` `edit_buffer` `eval_elisp`；工具结果的证据行保留"Target／Layer"两行（Layer 恒为"in memory only"），去掉"Checks: sandbox mode…"。

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
3. `save_buffer` 不在默认面；显式点名时：默认档位下每次问一次、`read-only` 和 `never` 拒绝、`danger-full-access` 不问；保存的是 base buffer 的整个文件。
4. `read-only`：编辑类工具直接拒绝，文字里不再出现升级提示。
5. Superchat 自己的状态 Buffer 任何档位都改不了（沿用并保留 M6 的回归）。
6. `open_file`：不抢焦点；已打开的返回原 Buffer；不存在的路径不创建磁盘文件；远程、目录、数据目录、超大文件报错；确实运行了 major mode 和 `find-file-hook`（与 `read_file` 的回归形成对照）。
7. 用户环境：测试里挂一个"Buffer 一改就保存"的 hook（模拟 `super-save`），编辑后磁盘确实变了，且 Superchat 没有任何拒绝或告警——证明它不对抗用户环境。
8. 既有套件、canonical smoke、Kernel 检查、加载、编译警告：只允许因工具面变化而动的基线变化，逐个列在提交说明里。
9. 真实模型：有人值守的 T2，chat 与 act 两个入口，按第七节口径。M5 在此之后才可能关闭。

## 九、分片（每片独立验收、leader 提交）

1. `open_file` ＋ 未保存修改的出口（Run 结束清单、`/changes`）。只加不删，现有行为不变。
2. 写路径切换：`write_file` 退役，`save_buffer` 退出默认面并改为"每次问"，编辑类工具去掉目标检查与升级参数，`read-only` 直接拒绝；删除第六节"删"列的代码与回归；README／CHANGELOG／组件文档同步。
3. 验收脚本按第七节改口径（审阅并保存），用脚本模型排练通过后，用户在场跑 T2。
4. （M7 之后）`superchat-authority.el` 改名为 `superchat-tool-execution.el`——用户裁定 ④(a)，顺延至此。

## 不做

文件系统写沙箱的任何加强；保护名单；`define_key`／`set_variable`／`add_hook` 等结构化 Emacs 工具（方向认可，另立里程碑）；对 `super-save` 等用户环境的探测或提示；把 `eval_elisp` 关进沙箱。

## 待定（请用户在审阅时顺手定）

1. **默认档位的名字。** `workspace-write` 这个词在新模型下不再准确（没有"workspace"边界，也不"write"磁盘）。(a) 建议：改名为 `edit-buffers`，旧名作为别名继续接受（Tape 上的旧事件、用户的 `/permissions workspace-write` 都还认）；(b) 保留旧名，只改说明文字。
2. **`danger-full-access` 下 `save_buffer` 是否进默认面。** (a) 建议：不进——默认面只有一份，档位不改变工具面，只改变问不问；(b) 进。
