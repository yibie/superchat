# Superchat Agent 架构文档

> 文档版本：2026-06-21  
> 对应实现：Phase 1–5 + Tape 统一（Phase A–F）完成后的 superchat

## 1. 设计目标

把 superchat 从“单次聊天客户端”升级为轻量级 agentic 框架：

> **session = buffer + preset（skill + tools + model）+ 可压缩的历史 + 可选子代理**

同时保持 backward-compatible：`M-x superchat` 默认行为不变。

## 2. 核心抽象

### 2.1 Preset

定义在 `superchat-preset.el`。

```elisp
(cl-defstruct superchat-preset
  name description type skill-body tools model backend pre
  version triggers source source-file)
```

- `type`：`prompt`（默认）/ `agent` / `plan` / `workflow`
- `tools`：该 preset 可用的工具名称列表
- `model`：可选的模型覆盖
- `backend`：可选的后端覆盖
- `pre`：加载前执行的函数（符号名）

Skill 是 preset 的一种来源。`superchat-skills-load` 现在返回 `superchat-preset`。

### 2.2 Turn

`superchat-turn` struct 新增 `preset` slot。一次用户输入 = 一个 turn，turn 携带本次请求使用的 preset、tools、target-model。

### 2.3 Session

Session 不是独立对象，而是以下 buffer-local 状态的集合：

| 变量 | 含义 |
|---|---|
| `superchat--session-id` | SQLite tape 的 session_id |
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

`superchat--subagent-run` 会：

- 生成独立 `session_id`
- 绑定独立 temp buffer（避免污染主 buffer 的渲染）
- 清空 `superchat--conversation-history`
- 设置 `superchat--active-preset` 为子代理 preset
- sync 调用 `superchat--llm-generate-answer-sync` 并启用 agent-mode
- 返回最终报告

### 6.3 调用方式

**手动 slash 命令：**

```text
/subagent researcher 查一下 superchat-send-input 的调用链
```

**Agent 模式自动委派：**

主 agent preset 的 `tools` 里包含 `delegate_to_subagent`，LLM 会自己决定何时委派。

**编程调用：**

```elisp
(superchat-tool-delegate-to-subagent "researcher" "task" "context")
```

### 6.4 报告渲染

子代理返回的报告通过 `superchat--subagent-render-report` 插入主 buffer：

```org
** Sub-agent report: researcher
#+begin_quote
报告内容
#+end_quote
```

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

### 12.1 并行 Sub-agent

把 `superchat--subagent-run` 改为 async，支持一次启动多个 researcher，结果聚合后返回给主 agent。

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
