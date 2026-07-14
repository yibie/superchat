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

## 核心统一:两个想法其实是一个机制(门是多态的)

**自扩展工具**和**生成式 UI** 看起来是两件事,在 Lisp 里是同一件事:

> **agent 写 s-expression;一道门决定它变成什么。**
>
> - 目标是工具注册表 → 得到一个新工具(自扩展能力)
> - 目标是 buffer 挂载点 → 得到一块新界面(生成式 UI)

这在非同像语言里做不到:web 世界的 "generative UI" 要发明 JSON schema、
组件白名单、序列化协议;在 Emacs 里,**UI 就是代码,代码就是数据**,
vui 组件本身就是一个 s-expression。这是"Lisp 适合 AI"这句话在
superchat 上最实的兑现。

**但那道"门"不是同一道**(2026-07-14 grilling 的修正)。两类生成物的
安全模型必须分开,否则"统一"就是自欺:

| 生成物 | 门的形态 | 为什么 |
|---|---|---|
| 工具 | **确认门 + 每次调用的 wrapper** | 代码要真跑;每次调用都可能带新参数 |
| UI | **白名单校验器**(校验通过才 eval) | UI 高频重渲染,每次弹框等于废掉功能 |

"统一"成立在**机制层**(都是 agent 写 s-expression、都由一道门裁决),
不成立在**门的实现层**。见下方"生成式 UI 为什么不能自由 eval"。

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
| 合成的**工具** | 注册进工具表 → 每次调用都走 wrapper | ⚠️ **不会自动落入确认门**——见下 |
| 生成 UI 的**事件处理器** | 用户点按钮 → 直接 funcall 闭包 | ❌ **完全不经过任何 agent 门** |

#### 订正:合成工具并不会"自动落入既有的门"

> 本文档 2026-07-13 初版曾断言"合成工具注册进工具表 → 自动落入既有的
> 确认+权限门,无需新机制"。**这是假的**,2026-07-14 的 grilling 开场
> 就核实掉了。

`superchat--agent-destructive-p` 是**纯名字匹配**:

```elisp
(defun superchat--agent-destructive-p (tool-name _args)
  (member tool-name superchat-agent-destructive-tools))
```

合成工具带着一个**全新的名字**,不在那张表里 → `confirm-p` 返回 nil →
**零确认执行**,哪怕 body 是 `(delete-file "~/")`。wrapper 确实会走
(计数、tape 记录都在),但**确认那一环根本不会发生**。

所以决策 1(见下方 Phase 1)不是"额外的保险",而是**补上这个洞的必需
品**:注册合成工具时,把它的名字加进 destructive 名单。

#### 生成式 UI 为什么不能自由 eval

**生成式 UI 的事件处理器不得是任意 lambda**——但光有这条**无法执行**:
如果我们 eval agent 写的 vui s-expression,render body 本身就是任意代码,
`(vui-text (progn (delete-file "~/x") "hi"))` 在**渲染那一刻**就跑了,
按钮都不用点。想靠"禁止 lambda"来防守,是在 Lisp 上做黑名单——
`(funcall (intern "delete-file"))`、宏展开、`#.` 读取期求值,防不胜防。

**唯一可防守的形态是白名单**(2026-07-14 定):agent 写的 UI
s-expression 先过**校验器**,只有**白名单内的 vui 构造子 + 字面量**能
活下来,通过校验才 eval。按钮只能写成:

```elisp
(vui-button "Cancel" :on-click (action cancel-agent :id "subagent-x"))
```

`action` 是**我们的**构造子——它把一个声明式动作变成走门的闭包。agent
永远交不出一个能直接执行任意代码的处理器。

这条限制不是保守,是必需:否则 agent 只要渲染一个诱人的按钮,就能让
用户亲手点掉自己的安全门。

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

> 规格由 2026-07-14 的 grilling 逐条定死。九条决策 + 一条实现约束,
> 全部有理由,不要在实现时"顺手优化掉"。

| # | 决策 | 理由 |
|---|---|---|
| 1 | 合成工具**一律视为破坏性**:注册时把工具名加进 `superchat-agent-destructive-tools`,每次调用都确认 | 从一个名字无法判断任意代码会干什么。"定义时确认一次就够"被**参数依赖行为**击穿:`(delete-directory path t)` 审阅时无害,之后以 `path="/"` 调用就不是 |
| 2 | **`superchat-agent-confirm-synthesized`**(默认 t)**独立于**全局 `confirm-destructive` | 全局关确认表达的是"我信任 superchat 自带的经审工具",**不等于**"我信任 LLM 三秒前写的代码"。要关它得另外明确地关 |
| 3 | 机制 = 新工具 **`define_tool`**(name / description / args-schema / body 一次交齐) | 能力成为 harness 一等公民:进工具 schema(模型每回合自动看见,不靠回忆)、可按名授予子代理、tape 里有独立身份、Phase 2 有东西可持久化。裸 eval-elisp 三样全无 |
| 4 | **默认关闭**,不进 `superchat-llm-tool-names` 默认名单 | 与 write-file / shell-command / eval-elisp 的现有姿态一致——它们本来就都不在默认名单里。自扩展是能力跳变,应当是刻意的动作 |
| 5 | 子代理**可以**合成,但**ctx 内自用、用完即弃** | 工具注册表是全局的;若子代理能写进去,一份**可分享的** SKILL.md 就能往主代理的工具箱里塞东西 |
| 6 | **热注册**:把包装后的新工具注入**飞行中** prompt 的 tools slot,同一次运行的下一轮即可调用 | v1.3.2 修好 loop 后物理上可行(openai/claude/ollama 三个 provider 都在每轮构建请求时重读 `llm-chat-prompt-tools`)。这是"能力在对话中长出来"的完整兑现;也是决策 5 有意义的前提——子代理只活一次运行,没有"下一回合" |
| 7 | **命名**:拒绝与内置/MCP 工具重名;**允许重定义自己先前合成的**(需重新确认);elisp 函数进 `superchat-syn--` 命名空间 | 遮蔽内置工具是灾难级攻击面(被提示注入的 agent 把 `read-file` 换成外传版本,你永远不会注意到)。允许自我重定义是因为**迭代正是回报**:loop 修好后 agent 能看到自己的工具报错 → 改 → 重试 |
| 8 | **确认展示**:pretty-print 的**代码预览 buffer**(复用 `superchat-tool--confirm-diff` 的模式),而非 minibuffer | 现状是安全剧场:`superchat--agent-ask-confirm` 对超过 200 字符的参数只显示 `(see details in buffer)`,而 buffer 里是一行转义的 `%S`。**在看不懂的代码上点"是",那道门就不存在** |
| 9 | **发现性**:`define_tool` 在当前工具集里时,往 system prompt 注入一行提示("若没有现成工具能完成任务,你可以用 define_tool 写一个") | 否则模型遇到没工具的任务只会硬着头皮直接回答——功能建好了却没人触发。仓库已有先例:`superchat-prompt-hook--parallel-tool-calls` 就是工具启用时才注入指引 |

**实现约束(非决策,是事实倒逼)**:合成工具存在**独立变量**
`superchat-llm-synthesized-tools`,在 `superchat--collect-llm-tools` 时并入。
**不能**直接追加进 `superchat-llm-tools-list`——`superchat-llm-tools-reload`
是从硬编码列表**全量重建**的,而它会在 registry 签名变化时自动触发
(用户存一个 SKILL.md 就会),合成的工具会**无声消失**。

- 合成物默认**会话级**(不落盘),验证价值后再进 Phase 2。
- 验收:*agent 遇到没有现成工具的任务(如"解析这个 org 表格"),
  自己写出工具、注册、**在同一次运行里调用它**、完成任务。*

### Phase 2 —— 持久化与再水化

- 合成工具写入 `<data-dir>/synthesized-tools.el`(人类可读)。
- 启动时**不自动加载**;提供 `M-x superchat-tools-review-synthesized`
  让用户逐个过目、批准、载入。批准状态记入 tape。
- 验收:*"这个 agent 越用越强"成立,且每一份新增能力都经过人眼。*

### Phase 3 —— 生成式 UI(vui.el)

- vui.el 作为**可选依赖**(`(require 'vui nil t)`),缺席时功能优雅降级,
  聊天主界面完全不受影响。
- 新工具 `render_ui`:agent 提供一个 vui 组件 s-expression,**先过白名单
  校验器,通过才 eval**,渲染到**独立 buffer**(不侵入 org-mode 聊天
  缓冲区)。典型场景:调研结果的可交互对比表、多方案选择器、带按钮的
  diff 预览、任务板视图。
- **白名单校验(2026-07-14 定)**:只有白名单内的 vui 构造子 + 字面量
  能通过;`lambda` / `funcall` / `eval` / `progn` 等一概拒绝。事件处理器
  只能写成 `(action <动作名> :args …)`——`action` 是**我们的**构造子,
  把声明式动作变成走门的闭包。理由见"安全脊柱":黑名单在 Lisp 上防不住,
  白名单才可防守。
- **动作词表**从控制面板需要的那一个起步(`cancel-agent`),按真实需求生长。
  不预先设计一整套词表。
- 与 harness 的天然结合:v1.3.1 的控制面(`/agents`)是一个天生的 vui
  用例——运行中子代理的**实时**面板(当前只有静态 echo),按钮 = 取消。
  这是验证 vui 集成的最佳首个场景:**我们自己的需求,不是假想的**。

## 依赖边界(一个意外发现)

vui.el 要求 Emacs 29.1,而 superchat 声明 `(emacs "28.1")`——看似冲突。
**但 superchat 的实际下限早已是 29.1**:`superchat-db.el` 自己写着
"Requires Emacs 29+ (built-in sqlite support)",tape/memory 全系依赖
内置 sqlite。也就是说 **Package-Requires 里的 28.1 已经是不实声明**
(28.1 上跑不了 memory/tape/compact/recall)。

v1.3.2 已将 `Package-Requires` 修正为 `(emacs "29.1")` 并同步 llm.el
0.31.1,让声明与现实一致——**这项修正与 vui 无关,但移除了它的版本
障碍**。

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
