# superchat 定位：Emacs 的 AI OS 层

## 一句话

superchat 活在 Emacs 里，以 **Emacs 的对象模型**（buffer、文件、进程、
包、org、magit、LSP）为操作对象，是 **Emacs 的 AI OS 层**——不是面向
宿主操作系统的桌面 agent。它可以任意操作 Emacs；宿主 OS 只在被
明确标记的通道上可达。

## 1. 架构回顾（代码现状）

```
表面层    superchat.el（会话视图/命令表/defcustom）+ render + dispatcher + parser
执行内核  core（Turn 数据）→ runtime（Turn/Run 生命周期，唯一属主）→ llm（传输）
横切缝①  Authority：所有模型工具执行过 validate→effects→policy→approval→execute→Tape
横切缝②  Tape：append-only 事件史（身份/因果/顺序/重放）
读模型    Closure（纯判定器：从 Tape 证明声明）+ Projection（可重放/物化的读视图）
域模块    Workspace / Memory / Skills / Workflows / Subagents / Experience
角色层    Presets：profile 带 capability（code/research/cowork）+ tools + model + prompt
```

关键事实（代码证据）：

- Authority 的 effects 分类已经体现操作对象边界：buffer 类工具声明为
  `local-write`/buffer effect；`shell-command`、`eshell-command`、
  `eval-elisp` 三个被单独声明为 `process-exec` effect
  （superchat-tools.el:144）。**宿主流出通道在政策层已与 Emacs 对象面区分**。
- 文件工具带保护路径检查（`superchat-tool--protected-path-p`）——Emacs
  配置等敏感路径拒绝写入。
- Subagent 走同一个 Runtime/Authority（`superchat--subagent-wrap-tool`
  包装工具）——子代理与主代理共享同一 Emacs 操作空间，不另开权限面。
- Preset 层领域无关：capability 只是 profile 的属性
  （superchat.el:592-625 已定义 code/research/cowork），类型有
  agent/plan/workflow。
- 经验系统机制通用：typed failure → 上下文 → 候选 → 干预 → 审查 →
  结构监控/自动层。目前只实现了 workspace 版本错误一个域，机制本身
  对任何带类型的失败适用。

## 2. 双身份：coding 与非 coding 共用基座

### 已成立

| 证据 | 说明 |
|------|------|
| 内核五件套无关代码 | Run/Tape/Authority/Closure/Projection 管事件、政策、证明 |
| capability 已共存 | code/research/cowork 三个 profile 定义于 superchat.el |
| 非 coding 工具已存在 | memory_search、sql、render_ui、workflow、team_task、subagent 委派 |
| 扩展面全是文本 | Skills（markdown）、斜杠命令、>>workflow 配方 |
| 交互面 Emacs 原生 | buffer 会话、minibuffer/completing-read、keymap |

### 待兑现

- system prompt 是 coding 味（code capability 的
  "Work as a coding assistant..."）；非 coding profile 需要各自的
  prompt 与工具集
- 经验系统只验证过一个域；跨任务泛化等真实多任务数据
- research/cowork 的工具集深度低于 code

## 3. Emacs 操作对象边界

### 已是 Emacs 对象面

buffer 读写改、文件（经 Emacs + 保护路径）、describe-*/eval-elisp、
LSP、workspace artifact、memory（SQLite/FTS）、org（org-babel）、
magit、minibuffer 交互、subagent 同空间协作。

### 例外与政策

| 通道 | 归属 | 政策 |
|------|------|------|
| `shell-command` | 宿主逃生舱 | Authority 已标记 `process-exec`；应保持高审批门槛，能不用就不用 |
| `eshell-command` | Emacs 域内 | Emacs 自己的 shell，优先于 shell-command |
| `eval-elisp` | Emacs 本身 | 即操作对象本体 |

### 不做什么

浏览器自动化、桌面控制、OS 级自动化——设计上排除。新能力优先建在
Emacs 原生域：org 笔记、gnus/notmuch 邮件、elfeed RSS、calendar/diary、
Tramp 远程文件。

### 诚实边界

这是**界面承诺，不是沙箱承诺**。文件工具毕竟写宿主文件系统，
shell-command 毕竟是进程执行。superchat 不沙箱，它中介——把 Emacs
对象面做成 agent 的世界，把流出通道做成被政策标记的例外。

## 4. 方向含义

1. 新 capability（非 coding）按 Emacs 原生域扩展，不引入 OS 自动化工具
2. `shell-command` 政策收紧（审批 + eshell 优先）；evaluation 通道维持
   process-exec 分类
3. 经验系统作为通用机制推广到非 coding 域——等真实使用数据，不提前
4. 已交付证据链（本会话）：审查流程 + minibuffer 审查、压缩结构探针
   （51/52）、结构监控、自动层两级（标注/排序/告警 + 候选选择）、
   真实数据验证——全部默认关闭、Tape 可审计
