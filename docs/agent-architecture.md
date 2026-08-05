# Superchat Agent 架构文档

> 文档版本：2026-07-25
> 对应实现：Phase 1–5 + Tape 统一（Phase A–F）完成后的 superchat

## 1. 设计目标

Superchat 不是一组 Agent 产品，而是 Emacs 原生的 Agent Runtime：

> **Runtime 组合 Agent Profile、Capability、Authority、Topology 与
> Workspace；Tape 保存 Actor 的可审计历史。**

同时保持 backward-compatible：`M-x superchat` 默认行为不变。

## 2. 核心抽象

### 2.1 Actor、Agent Profile 与 Agent Instance

- **Actor**：Tape 事件和 Runtime 动作的归属身份。用户、Workflow step、
  Compactor 和 Agent Instance 都可以是 Actor；并非所有 Actor 都是 Agent。
- **Agent Profile**：静态、可复用的角色定义。当前由 `type=agent` 的
  `superchat-preset` 承载，通过 `capability` 显式引用默认 Capability；
  `profile-id` 是独立于可变 `name` 的不可变身份；修改它表示创建另一个
  Profile，而不是改名。它也不参与 Capability 推导。Profile 不拥有 Session
  状态，也不能授予有效 Authority。
- **Agent Instance**：按照一个 Agent Profile 参与 Runtime 的 Actor。现阶段
  直接使用 `actor-id` 作为实例身份，`profile-id` 指向其 Profile；不再增加一套
  重复的 `agent-id`。`actor-id` 表示一次 Runtime 参与身份，不是永久的
  user-like identity；跨 Run 连续性必须由 Topology 显式保留，不能靠名称猜测。

**Agent Profile 不是 Agent Instance**：Profile 是静态配置，不拥有运行状态；
Actor 是 Runtime 中的参与者身份；Run 是一次执行实例。

Capability 说明工作目的，Topology 说明参与者如何组织，Authority 说明允许产生
哪些效果；它们都不等于 Agent Profile。

### 2.2 Preset

定义在 `superchat-preset.el`。

```elisp
(cl-defstruct superchat-preset
  name profile-id description type capability skill-body tools model
  temperature max-tokens reasoning max-tool-calls confirm-destructive timeout
  pre version triggers source source-file)
```

- `type`：`prompt`（默认）/ `agent` / `plan` / `workflow`
- `profile_id`：Agent Profile 的稳定历史身份；不随 `name` 改名
- `capability`：Profile 的默认 Capability；与 `name` 独立
- `tools`：该 preset 可用的工具名称列表
- `model`：可选的模型覆盖
- `pre`：加载前执行的函数（符号名）

Skill 是 preset 的一种来源。`superchat-skills-load` 现在返回 `superchat-preset`。
只有 `type=agent` 的 Preset 是 Agent Profile；其他 Preset 仍只是配置。Runtime
对非 Agent Preset 按“Request 显式选择 → Preset 默认值 → `chat`”解析
Capability，绝不从 `profile-id` 推导。`superchat-preset-profile-id` 是
Profile identity 的唯一语义来源，Runtime、Session 和 Subagent 不再各自从
Preset 名称派生。
解析边界会将声明值或旧格式的名称回退规范化为 Struct 中的 `profile-id`；运行时
不再保存“声明来源”这一配置层概念。

Agent Profile 的 `capability` 是其 Run 合同。Request 可以省略它或重复选择同一
Capability；若选择不同 Capability，Runtime 会在创建 Run 和写入 Tape 前拒绝。
这不需要 Capability registry；同一角色若要承担另一种工作，应使用另一个
Profile。

旧 Agent Skill 缺少 `profile_id` 时会告警，并仍以 `name` 作为兼容身份；重新
导出会写入显式 `profile_id`。新 Profile 应显式声明该字段，Session 恢复按它
重新定位已改名的 Profile。

旧 Agent Skill 缺少 `capability` 时，Runtime admission 会告警。Runtime
不会从 Profile 名称推导 Capability；Request 也未显式选择时仅回退到 `chat`。
迁移完成并引入相应格式版本后，缺少 `capability` 的 Agent Profile 应成为
格式错误。

Agent Profile contract 不包含 Authority grant。有效 Authority 来自 Runtime
配置，并在 Team/Subagent 委派时只能收紧；Profile 中的行为偏好或确认护栏不能
扩大它。

### 2.3 Turn 与 Run

`superchat-turn` struct 新增 `preset` slot。一次用户输入 = 一个 turn，turn 携带本次请求使用的 preset、tools、target-model。

Run 是某个 Actor 对 Turn 的一次具体执行。Runtime status 和 `run.started`
通过 `actor-id` 记录参与身份，并在 Actor 是 Agent 时额外记录 `profile-id`；
`run.started` payload 同时保存最终解析出的 `capability`。Child Run 还记录
直接 `parent-run-id`；四者共同表达参与者、角色、当前执行和直接委派来源。
Team 的 `subagent.started/completed/failed` 只在 child Run 已获得 `run-id`
后发布，并统一携带 `actor-id/profile-id/run-id/parent-run-id`。旧 Tape 中的
`agent_id` 只允许在 Projection 兼容读取，不能由新事件写入。

### 2.4 Session

Session 是可由 Tape Projection 恢复的长期逻辑上下文。以下 buffer-local
变量只是当前 Emacs Surface 的 View cache：

| 变量 | 含义 |
|---|---|
| `superchat--session-id` | SQLite tape 的 session_id |
| `superchat--active-capability` | 当前 Session 选择的 Capability |
| `superchat--active-preset` | 当前 session 的默认 preset |
| `superchat--session-name` | 可选的人类可读名称 |
| `superchat--conversation-history` | 内存中的对话历史 |

## 3. Skill / Preset 文件格式

```yaml
---
name: coder
description: Autonomous coding agent
version: "1.1"
type: agent
profile_id: profile.coder
capability: coding
tools: [read-file, search-text, shell-command, write-file]
model: claude-sonnet-4
---

You are a coding assistant...
```

`type: agent` 的 skill 会触发 agent loop；`type: plan` 进入只读规划模式。

## 4. Agent Loop

实现文件：`superchat-agent-loop.el`

### 4.1 架构选择

我们没有自己实现 LLM tool protocol，而是**复用 llm.el 的原生多轮 tool loop**，在 tool function 外层包一个观测壳。

流程：

```
LLM 请求 tool
   │
   ▼
llm.el 调用被包装的 tool function
   │
   ▼
shell: 渲染 tool call + 写 tape + 确认（如需）
shell: 调用原始 tool function
shell: 渲染 tool result + 写 tape
   │
   ▼
返回结果给 llm.el，继续下一轮
```

### 4.2 配置

- `superchat-agent-max-tool-calls`：单轮最大 tool 调用次数（默认 50）
- `superchat-agent-confirm-destructive`：破坏性工具调用前确认（默认 t）
- `superchat-agent-destructive-tools`：破坏性工具列表

### 4.3 工具包装

`superchat--agent-wrap-function` 返回一个包装函数，用于：

1. 递增 tool call 计数
2. 检查 max-tool-calls
3. 渲染 tool call 到主 buffer
4. 写 `tool_call` 行到 tape
5. 确认破坏性操作
6. 调用原始函数
7. 渲染 tool result
8. 写 `tool_result` 行到 tape

### 4.4 入口

Dispatcher 在 `superchat-send-input` 中判断：如果 turn 的 preset 是 `agent` 类型，调用 `superchat--agent-run`，否则走普通 LLM 查询。

## 5. 会话压缩（Compaction）

实现文件：`superchat-compact.el`

### 5.1 触发

用户输入 `/compact`，或编程调用 `superchat-compact-session`。

### 5.2 流程

1. 从 `superchat--conversation-history` 收集最近 N 条消息
2. 用 `superchat-compact-prompt` 调用 LLM 生成摘要
3. 将摘要作为 `anchor` 行写入 SQLite tape
4. 把内存历史替换为一条 system anchor 消息
5. 在主 buffer 插入系统提示：Session compacted.

### 5.3 Prompt 组装

`superchat-prompt-hook--conversation-history` 现在会：

1. 读取当前 session 最新 anchor
2. 在 prompt 最前面加入 `Session context (anchor):`
3. 再追加最近对话历史

## 6. 子代理（Sub-agent）

实现文件：`superchat-subagent.el`

### 6.1 内置 Preset

| Preset | 用途 | 工具 |
|---|---|---|
| `researcher` | 只读调研 | `read-file`, `search-text`, `list-files`, `read_buffer` |
| `executor` | 自动执行 | researcher 工具 + `shell-command`, `write-file`, `append-file`, `EditBuffer` |
| `introspector` | Emacs 内省 | `read_buffer`, `describe-function`, `describe-variable`, `eval-elisp` |

### 6.2 隔离模型

`superchat--subagent-run-async` 会：

- 生成独立 `session_id`
- 绑定独立 temp buffer（避免污染主 buffer 的渲染）
- 清空 `superchat--conversation-history`
- 设置 `superchat--active-preset` 为子代理 preset
- 构造带独立 `actor-id`、`topology=subagent` 和收紧后 Authority 的 Runtime Request
- 通过 `run-id` 与 `parent-run-id` 记录 child Run 及其直接委派来源
- 通过 `superchat-runtime-submit` 复用同一 Run 生命周期
- 立即返回参与者 ID；只有 Child Tape 能证明 `durable-run-terminal` 为
  `proved/completed` 时，callback 才交付原始报告

### 6.3 Handoff Contract v0

`superchat--subagent-finish` 是 Parent 消费 Child report 的唯一 seam：

```text
Child Run → Tape replay → Closure judgment → handoff decision → Parent
```

`deliver` 交付原报告；`wait`、`manual-review` 和 `reject` 都隐藏原报告并返回
明确错误。Closure 只证明 Child execution 已闭合，不证明报告内容正确。

### 6.4 Artifact Delivery Contract v0

Parent 通过 Workspace 消费 Child Artifact 时，还必须提供 `workspace_info`
返回的 revision。`workspace_read` 只在以下事实同时成立时交付内容：Child Run
已由 Closure 证明成功闭合；Workspace Projection 中的当前 Artifact 仍属于该
Run、revision 未变化；最后一次 Artifact 事件发生在 terminal seal 之前。

这只证明带版本的 Artifact reference 可以跨 Actor 交付，不证明内容正确；URI
解析仍属于 Artifact Adapter，副作用仍属于 Authority。没有新增 Artifact 状态或
事件。

### 6.5 调用方式

**手动 slash 命令：**

```text
/subagent researcher 查一下 superchat-send-input 的调用链
```

**Agent 模式自动委派：**

主 agent preset 的 `tools` 里包含 `delegate_to_subagent`，LLM 会自己决定何时委派。

**编程调用：**

```elisp
(superchat-tool-delegate-to-subagent-async
 (lambda (report) (message "%s" report))
 "researcher" "task" "context")
```

### 6.6 报告渲染

子代理返回的报告通过 `superchat--subagent-render-report` 插入主 buffer：

```org
** Sub-agent report: researcher
#+begin_quote
报告内容
#+end_quote
```

### 6.7 并行子代理

新增 `delegate_to_subagent_parallel` 工具，接受 JSON 数组形式的任务列表，按 `superchat-subagent-parallel-max`（默认 3）分 chunk 并发执行。

```text
delegate_to_subagent_parallel(tasks: "[
  {\"preset\": \"researcher\", \"task\": \"...\"},
  {\"preset\": \"executor\", \"task\": \"...\"}
]")
```

并行委派使用异步 callback 链；各 llm.el 请求独立运行，结果按输入顺序聚合。

## 7. Tape.systems 映射

| tape.systems | superchat 实现 |
|---|---|
| Tape | SQLite `tape` 表 |
| Entry | `tape` 表中的一行 |
| Anchor | `kind = 'anchor'` 的行 |
| Tool call | `kind = 'tool_call'` 的行 |
| Tool result | `kind = 'tool_result'` 的行 |
| View | prompt 组装时从 anchor + 最近 entries 构建 |

原则：append-only，不删除旧记录；corrections 追加新行。

## 8. 测试

新增测试文件：

- `test/test-agent-loop.el`
- `test/test-compact.el`
- `test/test-subagent.el`

这些测试已加入 `test/run-tests.el`。

当前测试结果：150 个测试，147 通过，3 个 pre-existing ecosystem/LSP 失败。

## 9. 使用示例

### 启动一个 coding agent session

```text
C-u M-x superchat
;; 选择 coder
```

或启动后输入：

```text
/agent coder
```

### 手动委派子代理

```text
/subagent researcher 查一下当前项目里哪些文件引用了 llm.el
```

### 压缩会话

```text
/compact
```

### 创建自定义 agent skill

文件 `~/.emacs.d/superchat/skills/coder.md`：

```markdown
---
name: coder
description: Autonomous coding agent
type: agent
profile_id: profile.coder
capability: coding
tools: [read-file, search-text, shell-command, write-file]
---

You are an experienced Emacs Lisp developer...
```

## 10. Tape.systems 统一实现（Phase A–F）

在 Obelisk 的启发下，superchat 从 `tape + memory` 双表模型转向单一 `tape` 表模型，使用 Entry / Anchor / View 三个原语。

### 10.1 数据结构

| 原语 | 含义 | 实现 |
|---|---|---|
| Entry | 不可变交互单元 | `tape` 表的 `user`/`assistant`/`tool_call`/`tool_result` |
| Anchor | 可回溯的提炼标记 | `tape` 表的 `anchor` kind |
| View | 运行时 entry 集合 | `superchat-tape-view.el` 中的查询函数 |
| Handoff | 超出上下文窗口时的切换 | `superchat-handoff--maybe-auto-compact` |

### 10.2 Schema v3

- `topic` 列：给 entry 打标签，用于 view 分组。
- `tape_fts`：FTS5 全文索引，`trigram` tokenizer，支持 CJK。
- `superchat-db-tape-select`：只读参数化 SQL 查询入口。

### 10.3 `/remember` → Anchor

`/remember` 现在会：

1. 追加一条 `user` entry：`[remember] <text>`
2. 追加一条 `anchor` entry，content 为 distilled observation
3. 可选写回 legacy `memory` 表（受 `superchat-memory-legacy-writes-enabled` 控制）

### 10.4 `/recall` → View

`/recall` 优先搜索 `tape_fts`，无结果时回退到 legacy `memory` 表。

### 10.5 `/compact` + `/expand`

- `/compact` 生成 anchor，但**不删除**原始 tape entries。
- `/expand` 把最新 anchor 的 source entries 拉回内存历史。

### 10.6 结构化检索工具

新增工具：

- `sql`：只读 SQL 查询
- `memory_search`：tape FTS5 搜索
- `tool_history`：按工具名查历史
- `file_history`：按文件路径查历史
- `recent_errors`：最近错误型 tool result

### 10.7 自动 Handoff

`superchat-handoff-auto-enabled`（默认 nil）开启后，当 prompt 长度超过 `superchat-handoff-char-threshold` 时自动 `/compact`。

### 10.8 迁移

`M-x superchat-memory-migrate-to-tape` 把 legacy memory 表的 accepted rows 转成 tape anchors。

## 11. 未来方向

### 12.1 并行 Sub-agent（已实现基础版）

`delegate_to_subagent_parallel` 已支持通过异步 callback 链并发运行多个子代理。

### 12.2 Workspace Buffer

利用 Emacs buffer 作为共享数据结构，创建 `*superchat-workspace*` org buffer：

- orchestrator 创建任务节点
- researcher/executor 在节点下写入结果
- verifier 写入验证结果
- 用户可直接查看、编辑、运行

这是 Emacs-native multi-agent 最自然的路径。

### 12.3 Task Graph

把 workflow 升级为 DAG，节点可指定 agent preset，依赖满足后自动调度。

## 12. 参考文件

| 文件 | 作用 |
|---|---|
| `superchat-preset.el` | Preset struct、frontmatter 解析 |
| `superchat-agent-loop.el` | Agent loop、tool 包装 |
| `superchat-compact.el` | 会话压缩、anchor、handoff |
| `superchat-subagent.el` | 子代理 preset、runner、报告渲染 |
| `superchat-tape-view.el` | Tape View 函数 |
| `superchat-skills.el` | Skill 加载、应用 preset |
| `superchat-skills-standard.el` | Standard skill frontmatter |
| `superchat-dispatcher.el` | 路由、Topic hook 调用 |
| `superchat-llm.el` | Tool 收集、sync LLM 调用 |
| `superchat-render.el` | Prompt、tool call/result、subagent 报告渲染 |
| `superchat-prompt-hooks.el` | Prompt 组装，包含 anchor |
| `superchat-tools.el` | 工具注册，含 introspection、delegation、tape 检索 |
| `superchat-memory.el` | Legacy memory facade + migration command |
