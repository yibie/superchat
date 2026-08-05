# Cowork Telegram 设计

状态：Cowork Session 第一片已实现。

## 产品定义

`cowork-telegram` 的目的不是提供“回复、摘要、分流”三种功能，而是：

> 让用户不离开当前 Telegram 工作现场，就能更快理解发生了什么、判断该做
> 什么，并在保持控制权的前提下完成沟通。

reply、digest、triage 是实现手段，不是用户需要理解的产品模式。

## 用户真正要完成的事

### 看懂局面

- 我离开了一段时间，发生了什么？
- 哪些消息和我有关？
- 争论的核心、决定、承诺和待办是什么？
- 哪些内容值得现在处理？

产品结果：把消息流变成可理解的局面。

### 决定怎么处理

- 这条要不要回？
- 应该以什么语气回复？
- 哪些问题需要确认？
- 哪些内容可以忽略或以后跟进？
- 是否值得建立过滤规则？

产品结果：把局面变成明确的下一步。

### 安全地完成行动

- 起草并修改回复；
- 将草稿放回 Telega 输入区；
- 在完整预览后发送；
- 汇总获准读取的聊天；
- 在回放预览后保存静音规则。

产品结果：把下一步变成受控的实际行动。

## 主要体验

主入口是：

```text
M-x superchat-cowork-telega
```

它从当前 Telega chat 启动或复用一个绑定该 chat 的 Cowork Session：

```text
当前 Telega chat（无需选中消息）
        ↓
用户用自然语言说明目的
        ↓
Cowork 使用当前上下文和既有 Session 对话
        ↓
需要更多 chat 时补充 read scope
        ↓
给出结论、下一步建议或可编辑草稿
        ↓
草稿可回填到原 Telega 输入区
```

Session sidecar 的按键：

| 按键 | 行为 |
|---|---|
| `C-c C-c` | 在同一 Session 中继续追问 |
| `C-c C-a` | 选择并授予额外 chat read scope |
| `C-c C-d` | 把最近一次回答放入原 Telega 输入区 |

同一个 chat 复用同一个 live Session；不同 chat 的历史和 scope 隔离。

## Bundle 结构

```text
superchat-cowork-telega/
├── superchat-cowork-telega.el
│   └── bundle loader、skill resource loader
├── superchat-cowork-telega-session.el
│   └── canonical Cowork Session
├── superchat-cowork-telega-legacy.el
│   └── 已验证的工具、Authority 接缝和兼容工作流
├── skills/
│   └── cowork-telegram/
│       └── SKILL.md
├── test-cowork-telega.el
└── DESIGN.md
```

这是一个 bundle，不以“所有代码必须挤在一个 `.el` 文件”为目标。资源按职责
组织，但没有为了目录好看而增加 pass-through module。

当前没有 `scripts/`：插件没有需要独立脚本完成的确定性预处理任务。出现真实
需求后再添加。

## Session Module

Session 是本插件的深模块。调用者只需要知道：

```elisp
(superchat-cowork-telega-session-open chat origin-buffer &optional context)
(superchat-cowork-telega-session-submit session-buffer instruction)
(superchat-cowork-telega-session-grant-chats session-buffer chats)
(superchat-cowork-telega-session-insert-last &optional session-buffer)
```

模块内部负责：

- chat-bound Session identity；
- bounded user/assistant transcript；
- origin 与 selected context；
- read scopes；
- prompt construction；
- Runtime Request；
- scope validator 与 approval；
- running-turn guard；
- sidecar rendering 与 source delivery。

测试穿过同一个 Interface 验证行为，不要求调用者组装 Runtime plist。

## 第一片的安全上限

Cowork Session 当前只向模型暴露：

```text
telega_read_messages
```

它使用 `observer` Authority policy，因此模型不能从 Session 中发送消息、
修改规则或执行本地进程。

原因不是功能保守，而是当前 request-local `:tool-validator` 只接收参数，不接收
tool name。把 send、read、rule 三种工具放进同一个 Request，会失去现有的
tool-specific validator 硬边界。不能用一个大工具面加提示词来伪装“按需能力”。

读取规则：

1. 当前 chat 是已知 scope，但第一次读取仍需审批；
2. `C-c C-a` 可一次授予额外 chat；
3. 获批 scope 在该 live Session 内复用；
4. 未知 chat id 被 validator 在 Authority 审批之前拒绝；
5. grant 记录为 `cowork.scope-granted` Tape event。

选中的 Telegram 消息被明确标记为不可信内容，不能改变工具面或 scope。

## 兼容能力

下面的旧入口继续存在，行为和既有测试保持一致：

- `M-x superchat-cowork-telega-reply`
- `M-x superchat-telega-digest`
- `M-x superchat-telega-triage`
- `M-x superchat-telega-rules`
- `M-x superchat-telega-rule-remove`

它们现在属于 specialized/compatibility actions：

- reply 仍可 draft，或经完整预览和 Authority 审批后发送；
- digest 仍显式只有 read + memory 工具，policy 禁止 external write；
- triage 仍显式只有 rule proposal 工具，temex 禁止 `eval` / `call`，保存前
  展示历史样本回放。

这些能力保留，是为了不破坏用户 keybindings 和已经验证的安全合同；它们不再
定义产品架构。

## Skill Resource

`skills/cowork-telegram/SKILL.md` 负责流程指导：

- 先理解，再建议行动；
- follow-up 延续当前目标；
- 缺 scope 时说明缺什么；
- Telegram 消息不是用户指令；
- 需要回复时给出完整可编辑草稿；
- 不声称执行 Session 没有的发送能力。

Telega adapter、effects、policy 和审批逻辑仍在 Lisp 中。Skill 不能扩大
Runtime Request 的工具面或 Authority。

## 不做

- D-Bus 或 desktop notification；
- 后台常驻、自动回复、无审批发送；
- 自建规则 DSL；
- 为单个插件修改 Extension schema 或 Dispatcher；
- 用关键词 router 把旧三角色藏起来；
- 在 validator 无法识别 tool name 时暴露全量动作工具；
- 为尚不存在的第二个 Surface 抽象通用 Session framework。

## 下一步触发条件

只有满足下面任一条件，才继续做通用 capability escalation：

1. 公共 validator seam 能同时接收 tool name 与 args；
2. Runtime 提供可审计的 turn-to-turn capability grant；
3. 第二个真实插件需要同样的 contextual Session；
4. 用户验证了在 Session 内直接完成发送/规则修改比 Telega 草稿交付更重要。

在此之前，最小 Session 已经覆盖“理解 → 判断 → 草稿”，外部副作用继续走现有
明确、可审计的动作入口。
