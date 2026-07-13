# North Star (II): Homoiconic Agent —— 自扩展工具 + 生成式 UI

> 状态:**北极星文档**——不直接实现,为子目标定位与排序。
> 创建于 2026-07-13。
> 前置:harness 北极星(`agent-harness.md`)九层已立;
> **有门的 eval** 已成立(`7814fee`:eval-elisp 进入 destructive gate)。
> 灵感来源:thebeach.dev/posts/lisp-agent + github.com/jamiebeach/lisp-agent
> (100 行 Common Lisp,单一 `eval` 工具,能力在对话中长出来);
> UI 载体候选:github.com/d12frosted/vui.el(React 式声明式组件,
> v1.3.0,MELPA,API 稳定,Emacs 29.1+,GPL-3.0)。

## 论点:superchat 已经住在世界上部署最广的 live Lisp image 里

lisp-agent 的核心洞察是同像性——code is data——所以 agent 不需要预置
工具目录,一个 `eval` 就够,能力在运行时长出来。作者用 Docker 里的
SBCL 造了一个 Lisp 世界给 agent 玩。

superchat 不需要造:**它生来就在 Lisp 世界里**,而且这个世界比 SBCL
玩具环境更彻底——四十年的自文档系统(`describe-function`)、全量可
内省、可 advice、可热重定义,并且装着用户的真实工作。`eval-elisp`
工具今天就在(introspector 预设的标配),我们只是一直把它当配角。

## 核心统一:两个想法其实是一个机制

**自扩展工具**和**生成式 UI** 看起来是两件事,在 Lisp 里是同一件事:

> **agent 写 s-expression;门决定它变成什么。**
>
> - 目标是工具注册表 → 得到一个新工具(自扩展能力)
> - 目标是 buffer 挂载点 → 得到一块新界面(生成式 UI)

这在非同像语言里做不到:web 世界的 "generative UI" 要发明 JSON schema、
组件白名单、序列化协议;在 Emacs 里,**UI 就是代码,代码就是数据**,
vui 组件本身就是一个 s-expression。这是"Lisp 适合 AI"这句话在
superchat 上最实的兑现。

## 安全脊柱(先于功能确定,不可协商)

### 原则:生成的代码是数据,直到某个门把它变成行为

harness 全线(tighten-only、confirm 门、白名单、深度守卫)的哲学是收权;
universal eval 的哲学是放权——eval 是一切工具的超集,它在场时权限边界
形同虚设。lisp-agent 作者自己承认 "this is a toy for a sandbox",而
**Emacs 没有沙箱可退**:agent 就跑在你的编辑器、你的文件、你的 shell
凭证旁边。所以在 Emacs 里的正确形态是**有门的 eval**,不是裸 eval。

### 关键风险:延迟执行会绕过定义时的门

这是本目标最需要设计力的地方,也是天真实现必然踩的坑:

- **定义时**门会拦(eval-elisp 已是 destructive,`7814fee` 验证过);
- **但生成物在此后被调用时,不再经过那道门**。

两种生成物的处理必须分开:

| 生成物 | 后续调用路径 | 结论 |
|---|---|---|
| 合成的**工具** | 注册进工具表 → 每次调用都走 agent-loop / subagent 的 wrapper | ✅ 自动落入既有的确认+权限门,无需新机制 |
| 生成 UI 的**事件处理器** | 用户点按钮 → 直接 funcall 闭包 | ❌ **完全不经过任何 agent 门** |

因此:**生成式 UI 的事件处理器不得是任意 lambda**。按钮只能派发
**声明式动作词表**中的动作(如"把这段文本回传给 agent"、"以这些参数
调用工具 X"——后者随即落入工具门),不能挂裸代码。这条限制不是保守,
是必需:否则 agent 只要渲染一个诱人的按钮,就能让用户亲手点掉自己的
安全门。

### 再水化(rehydration)必须人过目

lisp-agent 把 transcript 当源码,重启时重放,技能自动复活——很优雅,
但直接照搬意味着**agent 写的代码会在无人在场时被自动重新执行**。
superchat 的形态必须是:合成物持久化为**人类可读的 elisp 文件**
(`<data-dir>/synthesized-tools.el`),加载是**显式 opt-in**,首次加载前
要求用户过目(像对待任何第三方 elisp 一样)。tape 记录全过程供审计。

## 分期

### Phase 0 —— 有门的 eval —— ✅ 已完成(`7814fee`)

`eval-elisp` 进入 `superchat-agent-destructive-tools`;主 agent 与子代理
两条 wrapper 路径均已验证拦截;profile 的 `confirm_destructive: false`
不能给它免票(tighten-only 兜底)。本目标的前提由此成立。

### Phase 1 —— 工具合成(自扩展能力)

- 新工具 `define_tool`:agent 提供 name / description / args / body
  (elisp s-expression),经确认门后 `defun` + `llm-make-tool` 注册,
  下一轮即可调用。
- 合成工具**天然是工具**:此后每次调用都走既有 wrapper,受
  max_tool_calls、confirm、permission hooks、tape 记录全套约束。
  无需为它发明新的安全机制——这正是统一机制的红利。
- 合成物默认**会话级**(不落盘),验证价值后再进 Phase 2。
- 验收:*agent 遇到没有现成工具的任务(如"解析这个 org 表格"),
  自己写出工具、注册、调用、完成任务。*

### Phase 2 —— 持久化与再水化

- 合成工具写入 `<data-dir>/synthesized-tools.el`(人类可读)。
- 启动时**不自动加载**;提供 `M-x superchat-tools-review-synthesized`
  让用户逐个过目、批准、载入。批准状态记入 tape。
- 验收:*"这个 agent 越用越强"成立,且每一份新增能力都经过人眼。*

### Phase 3 —— 生成式 UI(vui.el)

- vui.el 作为**可选依赖**(`(require 'vui nil t)`),缺席时功能优雅降级,
  聊天主界面完全不受影响。
- 新工具 `render_ui`:agent 提供一个 vui 组件描述,渲染到**独立 buffer**
  (不侵入 org-mode 聊天缓冲区)。典型场景:调研结果的可交互对比表、
  多方案选择器、带按钮的 diff 预览、任务板视图。
- **事件处理器受限于声明式动作词表**(见"安全脊柱"),不接受裸 lambda。
- 与 harness 的天然结合:v1.3.1 的控制面(`/agents`)是一个天生的 vui
  用例——运行中子代理的**实时**面板(当前只有静态 echo),按钮 = 取消。
  这是验证 vui 集成的最佳首个场景:**我们自己的需求,不是假想的**。

## 依赖边界(一个意外发现)

vui.el 要求 Emacs 29.1,而 superchat 声明 `(emacs "28.1")`——看似冲突。
**但 superchat 的实际下限早已是 29.1**:`superchat-db.el` 自己写着
"Requires Emacs 29+ (built-in sqlite support)",tape/memory 全系依赖
内置 sqlite。也就是说 **Package-Requires 里的 28.1 已经是不实声明**
(28.1 上跑不了 memory/tape/compact/recall)。

因此本目标顺带修正:`Package-Requires` 提到 `(emacs "29.1")`,让声明与
现实一致——**这项修正本身就该做,与 vui 无关**;做完之后,vui 的版本
要求是免费的。

## 明确不做

- **裸 eval 作为唯一工具**(lisp-agent 的形态):Emacs 无沙箱可退,
  既有的 29 个工具 + 白名单 + 确认门是有价值的边界,不为优雅牺牲。
- **无人值守的再水化**:见安全脊柱。
- **用 vui 重写聊天主界面**:org-mode 缓冲区是 superchat 的身份
  (可保存、可导出、可 org-babel),生成式 UI 是**新增的一块表面**,
  不是替换。
- **agent 自我修改 superchat 自身的代码**(advice/redefine 核心函数):
  合成工具住在自己的命名空间里;改 harness 本体属于人的工作。

## 排序建议

Phase 1(工具合成)自成闭环、无新依赖、直接兑现"能力在对话中长出来",
建议先做。Phase 3(生成式 UI)的首个落地场景选控制面板——把 v1.3.1 的
`/agents` 从静态 echo 升级为实时可交互面板,用自己的真实需求验证 vui
集成,再开放 `render_ui` 给 agent。
