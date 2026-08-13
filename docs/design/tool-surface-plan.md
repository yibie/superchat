# 工具面重规划：从 Emacs 对象模型出发

前提文档：`architecture-positioning.md`（定位）、`emacs-oriented.md`
（第一性原理）。现状工具是 gptel 时代的历史遗留；本文从对象模型
重头设计，不迁就现状。

## 0. 设计原则（从两份文档提炼，逐条可执行）

1. **工具是对象上的方法**，不是 syscall 包装。新工具的默认形状：
   「对象身份 + 方法参数」，对象身份自足（名字/符号/id），不要求
   模型补全外部上下文（cwd、offset、行号）。
2. **语义自足**：模型调用工具的完整上下文在参数里。反例（现状）：
   `read-file` 的 offset/limit 要求模型盲猜范围；`EditBuffer` 的唯一
   匹配约束反而是正例（歧义 → 显式失败）。
3. **权威副本在 buffer，落盘由人**。工具改活对象，`C-x C-s` 是人
   的动作。撤销、modified flag、revert 检测复用 Emacs 四十年安全网。
4. **消歧不是限权**：废弃路径式工具的正当理由是模型可预测性与审计
   信息量；攻击面由 eval-elisp 决定，收敛工具面不改变它。
5. **每工具一个 effect 声明**（kind + resource）。resource 词汇表：
   `file / buffer / workspace / process / emacs / tape / team /
   database / language-server / tool-definition`。

## 1. 现状盘点（全部 36 个工具，按去向分）

### 废弃（路径式文件 I/O，8 个）

`read-file` `write-file` `append-file` `quick-write` `list-files`
`find-files` `search-text` `make_directory`

理由：语义欠定（cwd 相对谁、覆盖风险、范围盲猜）；与 `#` 引用和
buffer 工具语义重叠（消歧原则）；实机验证模型在它们上表现最差。

### 保留（Emacs 对象面，17 个）

| 对象 | 工具 | 备注 |
|------|------|------|
| buffer | `read_buffer` `EditBuffer` `ReplaceBuffer` `append_to_buffer` | 命名不统一（camelCase 混 snake_case，历史遗留），重规划时统一为 snake_case |
| elisp 运行时 | `describe_function` `describe_variable` `eval_elisp` | 反射 + 本体 |
| language server | `lsp_references` `lsp_hover` `lsp_code_actions` | 语义索引 |
| workspace | `workspace_read` `workspace_info` `workspace_write` `workspace_publish` | 带 revision 的对象 |
| tape | `memory_search` `file_history` `tool_history` `recent_errors` | 历史视图 |

### 保留（组织与逃生舱，10 个）

`delegate_to_subagent[_parallel]` `team_task` `team_message`
`define_tool` `render_ui` `emit_plan` `sql` `shell_command`
`eshell_command`

（`shell_command` 是唯一的宿主逃生舱；`eshell_command` 是 Emacs
域内的进程入口。）

## 2. 对象面缺口（重头设计发现的问题）

### 缺口 1：没有 buffer 列表工具

`read_buffer` 需要名字，但模型没有任何办法**发现**存在哪些 buffer。
身份寻址的闭环缺了"枚举"这一步。必补：`list_buffers`。

### 缺口 2：命名不统一

`EditBuffer` / `ReplaceBuffer`（camelCase，gptel 遗产）与
`read_buffer`（snake_case）并存。统一为：`edit_buffer`
`replace_buffer` `append_to_buffer` `read_buffer` `list_buffers`。

### 缺口 3：elisp 运行时的反射面不完整

对象面最大的资产是"运行时能回答关于自己的问题"，但只有
`describe_function` / `describe_variable` 两个反射工具。补齐：
`apropos`（按名找符号）、`find_definition`（跳到定义位置，配合
LSP 的缺口兜底）。这两个都是零风险 local-read。

### 缺口 4：org 对象面为零

定位文档点名的原生域（org 子树）还没有工具。列为 v2 域扩展
（见 §4），不阻塞 v1。

## 3. 工具面 v1 目标形状

```
buffer 对象面（权威副本在这里）
  list_buffers            local-read  buffer
  read_buffer             local-read  buffer
  edit_buffer             local-write buffer   （唯一匹配约束）
  replace_buffer          local-write buffer
  append_to_buffer        local-write buffer

elisp 反射面（事实 vs 文本证据）
  describe_function       local-read  emacs
  describe_variable       local-read  emacs
  apropos                 local-read  emacs
  find_definition         local-read  emacs
  eval_elisp              process-exec emacs   （本体；审批）

语义索引面
  lsp_references          local-read  language-server
  lsp_hover               local-read  language-server
  lsp_code_actions        local-write language-server

workspace 面（跨 Run 的活对象）
  workspace_info          local-read  workspace
  workspace_read          local-read  workspace
  workspace_write         local-write workspace
  workspace_publish       local-write workspace

tape 历史面
  memory_search / file_history / tool_history / recent_errors
                          local-read  tape

组织面
  subagent / team / define_tool / render_ui / emit_plan / sql

进程面（逃生舱）
  eshell_command          process-exec process   （Emacs 域内）
  shell_command           process-exec process   （宿主；高门槛）
```

**被排除的**：一切路径式 I/O。模型与文件的唯一通道是：用户 `#`
注入（范围由人定）、buffer 工具（活对象）、workspace（跨 Run 对象）。

## 4. 域扩展路线（v2+，每域一个对象模型）

按定位文档 §3 的原生域优先级，每域先回答"身份是什么、方法是什么"：

1. **org 域**：身份 = org 子树 id；方法 = 列出/读子树/插入子树/
   改 TODO 状态/查 agenda。验收判据：Claude 可检验判据 #2（上手成本）
2. **进程/编译域**：compilation buffer 已经是对象——补一个
   `compile` 工具（进 compilation buffer，RET 可跳转）
3. **gnus/notmuch、elfeed、calendar、Tramp**：各域适配层已在社区
   完成二十年，superchat 只做工具面包装

**每域必须携带 effect 声明**——域扩展的审批语义随 resource 维度
自动成立，不另设计。

## 5. 迁移路径（现状 → v1）

| 步骤 | 内容 | 风险 |
|------|------|------|
| A | 8 个路径式工具从注册表移除；preset `:tools`、skills frontmatter、提示词、`superchat-llm-tool-names` 默认表同步换血 | 高（面广）；测试逐个修 |
| B | 新增 `list_buffers`、`apropos`、`find_definition`；统一 buffer 工具命名为 snake_case（旧名保留 alias 一个版本周期？——不，一步到位，测试同步改） | 低 |
| C | 实机回归：plan/code 两条 preset 流程在真实模型上重验（阶段 4 遗留的验证债） | 中 |
| D | 用 Claude 的 4 条可检验判据收集 Tape 证据（工具面缺口的 eval-elisp 转向率等） | 低 |

## 6. Pi 对照：最小核心 + 能力分层（2026-08-14 修订）

Pi 的实现给了三条可照搬的结构原则（pi 文档：index.md、
extensions.md）：

1. **核心刻意小**："designed to stay small at the core"——核心工具
   只有 读/改/写/执行/搜索 一族；模型 schema 短 → TTFT 低、小模型
   可用、prompt 开销小。
2. **扩展注册工具**：`pi.registerTool()` 与 superchat 的
   `superchat-register-tool` 同构——域能力由扩展面/Preset 注册，
   不进默认 schema。
3. **事件拦截是运行时能力**：pi 的 extension 事件拦截（阻止
   tool_call、注入 context、compaction）对应 superchat 的
   Authority/Tape/skills——**这些从来不是工具，是运行时**。

### 修订后的 v1 目标：核心 4 个

```
list_buffers    local-read  buffer   枚举身份（对象面的闭包）
read_buffer     local-read  buffer   读活对象
edit_buffer     local-write buffer   改活对象（唯一匹配约束）
eval_elisp      process-exec emacs   问运行时/执行（Pi 的 bash 对应物）
```

**其余全部退出默认 schema**，各归其位：

| 去向 | 工具 | 机制 |
|------|------|------|
| 扩展面（preset/扩展注册） | describe_function/describe_variable/apropos、lsp 三件、workspace 四件、sql | `superchat-register-tool` + preset `:tools`；不进 `superchat-llm-tool-names` 默认表 |
| 运行时能力（模型不直接调） | memory_search（已有自动 recall）、file_history/tool_history/recent_errors（Tape 浏览器的人脸）、delegate/team（subagent 生命周期）、render_ui/emit_plan（按需 attach，现状已是） | 注入、钩子、命令、面板 |
| 逃生舱 | shell_command / eshell_command | 保留审批门槛；eshell 优先 |

### 为什么 eval-elisp 能承担"执行"

Pi 的 bash 万能性来自"宿主本来就有一切 CLI"；Emacs 的对应物是
eval-elisp——宿主本来就是 Lisp 机器。模型用 eval-elisp 可以：
读 symbol 当前定义（describe 的替代）、跑测试（ERT 是 Lisp 对象）、
查询任何运行时状态。代价是它必须会写 elisp——这正是"面向 Emacs"
的模型技能假设，与定位文档一致。describe 系列留在扩展面作为
低风险反射的兜底，不常驻 schema。

### 迁移路径修订

- 步骤 A 不变（8 个路径式工具退场）
- 步骤 B 改为：**默认 schema 砍到核心 4**（+ 现有 on-demand 机制
  照常让预设/输入点名 attach 扩展工具）；buffer 工具命名统一
- 步骤 C/D 不变

## 7. 明确不做

- 不引入任何新路径式工具（含"改进版 read_file"）
- 不做真沙箱（见 emacs-oriented.md：可归因性不是不可能性）
- v2 域扩展不阻塞 v1 迁移
