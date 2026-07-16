# Phase 3 实现计划:生成式 UI(vui.el)—— 开发 agent 交接文档

> 类型:**可执行的实现计划**,交由开发 agent 实现。
> 创建于 2026-07-16。
> 母目标:`docs/goals/homoiconic-agent.md` Phase 3(北极星 II)。
> 前置事实:vui.el 已侦察(见下"侦察结论"),`read` 安全边界已核实。
> 版本归属:v1.3 harness 线之后的新方向;编号由维护者定,不要自作主张 tag。

## 你要实现什么(一句话)

让 agent 输出**可交互的界面**,而不只是文字。机制与 Phase 1
(`define_tool`)同源——agent 写 s-expression,一道门决定它变成什么;
工具走确认门,**UI 走白名单校验门**。

## 铁律(先读,违反即推翻整个安全模型)

1. **绝不 eval agent 未经白名单校验的 s-expression。** 校验在 `read` 之
   后、`eval` 之前,遍历整棵树,只放行白名单内的构造子和字面量。
2. **`:on-click`(以及一切事件处理器)不得是裸 lambda。** vui 的默认用法
   就是 `(vui-button "x" :on-click (lambda () ...))`——这是**真实攻击面**,
   不是理论。事件处理器只能写成 `(action <动作名> :key val ...)`,由**我们**
   把它转成走门的闭包。agent 永远交不出一个能直接跑任意代码的处理器。
3. **动作只能来自我们注册的动作词表。** 未知动作名 → 校验失败。词表从
   `cancel-agent` 一个起步,按真实需求生长,不预先设计一整套。
4. **UI 渲染进独立 buffer,绝不碰 org-mode 聊天缓冲区。** 聊天 buffer 是
   superchat 的身份(可保存、导出、org-babel);生成式 UI 是**新增的一块
   表面**,不是替换。
5. **vui.el 是可选依赖。** `(require 'vui nil t)`;缺席时一切功能优雅降级,
   聊天主界面完全不受影响。

## 侦察结论(已核实的事实,别再重新发现)

在临时环境装 vui-20260702.1513 跑通验证,以下为事实:

- **依赖**:`Package-Requires: ((emacs "29.1"))`,**零传递依赖**。与
  superchat 现有底线(v1.3.2 已修正为 29.1)完全对齐,不引入新第三方包。
- **模块分层**:核心构造子 `vui-text` / `vui-button` / `vui-fragment` 在
  `vui.el`;`vui-heading` / `vui-collapsible` / `vui-code` / `vui-table` 等
  在 `vui-components.el`,**要单独 `(require 'vui-components)`**。
- **挂载**:`vui-mount`(整 buffer)、`vui-mount-inline`(嵌入位置)、
  `vui-render-to-buffer`。
- **组件定义**:`(vui-defcomponent NAME (ARGS) :state ((KEY INIT)...) :render FORM)`。
  实例化用 `(vui-component 'NAME :prop val)`。
- **⚠️ 状态键是 keyword**:`(vui-set-state :count v)` 才生效;用 `'count`
  会静默 warning 且不更新。写组件时切记。
- **按钮机制**:标准 Emacs button(`action` text-property + `category`),
  **不是私有 widget**——所以 `push-button` 可用、可测,也正因如此它天然
  接受任意函数作 action(见铁律 2)。
- **batch 可测**:挂载→渲染→点击→状态更新→重渲染全链路可在 `--batch`
  下复现。**关键**:重渲染由定时器驱动(`vui-render-delay` 默认 0.01),
  测试里点击后调 **`(vui-flush-sync)`** 强制同步重渲染,再断言 buffer。

## `read` 安全边界(已核实,这是白名单方案成立的前提)

- **Emacs Lisp 的 `read` 不支持 `#.`(读取期求值)**:`(read "#.(...)")`
  直接报 `invalid-read-syntax`,无副作用。**所以"先 read 再校验再 eval"是
  可防守的**——校验发生在任何代码执行之前。(Common Lisp 有 `#.`,Emacs
  没有;别照搬 CL 的担忧。)
- **多 form 注入无效**:用 `read-from-string` / `read` 只读第一个 form,
  `"(vui-text \"a\") (delete-file \"x\")"` 的第二个 form 被忽略。
- **反引号**读成 `(\` ...)` 结构 → `\`` 不在白名单 → 拒。
- **`(funcall (intern "delete-file"))`** 只是普通列表 → 靠树遍历,`funcall`
  不在白名单 → 拒。

结论:**接收 agent 的 UI 为字符串,用 `read-from-string` 读成一个 form,
遍历校验,通过才 eval。** 这条路径无 read-time-eval 后门。

## 分阶段实现

### Stage 3a —— 内部控制面板(不碰 agent、不碰 eval,先验证 vui 集成)

**目标**:把 v1.3.1 那个静态 echo 的 `/agents` 升级成 vui 实时面板。
**这是我们自己的真实需求,用它验证 vui 干净嵌进 superchat**,零安全风险
(UI 由我们写,不是 agent 写)。

- 新模块 `superchat-agents-panel.el`,`(require 'vui nil t)`。
- 一个 `vui-defcomponent`,渲染 `superchat--subagent-running`(运行中子代理
  的 alist,见 `superchat-subagent.el`):每行显示 id / preset / 深度 /
  已运行秒数,行尾一个 **Cancel 按钮**,`:on-click` 调
  `superchat-subagent-cancel`(已存在,v1.3.1)。
- `/agents` 命令改为:vui 在场 → 弹面板 buffer;vui 缺席 → 回退到现有
  `superchat-subagent-list-running` 的静态 echo(**保留它作为 fallback**)。
- **实时更新**:子代理 launch / finish / cancel / timeout 都经过
  `superchat-subagent.el` 里的少数几个函数(`--subagent-run-async` 注册、
  `--subagent-finish` 清理)。在这些点调一个 `superchat-agents-panel-refresh`
  (面板 buffer 存活时 `vui-flush-sync`,否则 no-op)。**不要用轮询定时器**
  ——生命周期钩子已经齐全,轮询是浪费。
- **验收**:开两个并行子代理 → `/agents` 显示两行 → 点一个 Cancel →
  那行消失、对应子代理收到 `[Cancelled by user]` → 另一行还在。

### Stage 3b —— 白名单校验器(纯函数,安全核心,红队测试)

**目标**:一个纯函数,判定一段 agent 写的 UI s-expression 是否只由白名单
构造子 + 字面量 + `(action ...)` 处理器组成。**这是整个 Phase 3 的门**,
必须先于 `render_ui` 存在并被红队测试压过。

- 新模块 `superchat-genui.el`。
- `superchat-genui--allowed-constructors`:defcustom,白名单。初始集(保守,
  按需扩)= `vui-fragment vui-text vui-button vui-strong vui-italic vui-muted
  vui-heading vui-success vui-warning vui-error vui-code`。**不含**任何带
  副作用或能引入代码的:`vui-use-effect` / `vui-use-async` / `vui-defcomponent`
  一律不入白名单。
- `superchat-genui-validate (form)` → 返回 `t` 或**拒绝原因字符串**(供模型
  自我修正,与 `define_tool` 的错误反馈风格一致)。遍历规则:
  - 列表且 `car` 是白名单构造子 → 递归校验其余元素;
  - 关键字参数 `:on-click` / `:on-... ` 的值**必须**是 `(action NAME . plist)`
    形式,`NAME` 在动作词表内,plist 值必须是字面量 → 否则拒;
  - 字面量(字符串、数字、`t`、`nil`、keyword)→ 放行;
  - **其它一切**(`lambda` `funcall` `eval` `progn` `apply` `\`` 符号裸引用、
    未知构造子、非白名单函数调用)→ 拒,原因写明"不允许的形式:X"。
- **动作词表** `superchat-genui-actions`:alist `(NAME . HANDLER-BUILDER)`。
  `cancel-agent` → 一个接收 plist(`:id`)、返回走门闭包的构造子;闭包内部
  调 `superchat-subagent-cancel`。
- **红队测试**(`test/test-genui.el`,这是本 stage 的重点,不是附属品):
  每条都必须**拒绝**并给出原因——
  `(vui-text (progn (delete-file "~/x") "hi"))`、
  `(vui-button "x" :on-click (lambda () (shell-command "rm -rf ~")))`、
  `(vui-button "x" :on-click (funcall (intern "delete-file")))`、
  `(vui-text (eval (read user-input)))`、
  `` (vui-text `,(delete-file "x")) ``、
  `(vui-button "x" :on-click (action unknown-action))`(动作不在词表)、
  `(unknown-constructor "x")`。
  以及必须**放行**:`(vui-fragment (vui-text "hi") (vui-button "Cancel"
  :on-click (action cancel-agent :id "sub-1")))`。
- **额外测试**:`read` 边界——`(delete-file "x")` 拼在合法 UI **后面**作第
  二个 form,校验器(经 `read-from-string` 只取第一个 form)不受影响。

### Stage 3c —— `render_ui` 工具(把 3a 的渲染 + 3b 的门接起来)

**目标**:agent 可用的工具。默认关闭、opt-in,与 `define_tool` 同姿态。

- `superchat-tool-render-ui (component)`:接收 UI s-expression 字符串 →
  `read-from-string` → `superchat-genui-validate` → 通过则把 `(action ...)`
  处理器展开成走门闭包、`eval` 成 vui vnode、`vui-mount` 到独立 buffer;
  失败则**返回拒绝原因字符串**(模型据此自我修正)。
- 注册进 `superchat-tools.el`,**不进** `superchat-llm-tool-names` 默认名单
  (铁律 5 / opt-in)。vui 缺席时工具返回 "Error: vui.el not available"。
- **发现性**:仿 `superchat-prompt-hook--self-extension`,`render_ui` 在场
  时往 system prompt 注入一行(说明它能画什么、动作词表有哪些)。
- **验收(端到端,mock LLM)**:agent 调 `render_ui` 交一段带 Cancel 按钮的
  面板 → 校验通过 → buffer 里渲染出来 → 点 Cancel → 走门闭包触发。同时:
  agent 交一段含 `lambda` 的 UI → 被拒 → 返回可读原因。

## 明确不做(本轮 Phase 3)

- **不让 agent 定义 vui 组件**(`vui-defcomponent`)——只允许它写一棵
  vnode 树。组件定义引入命名/生命周期/状态,是更大的攻击面,无当前需求。
- **不做纯数据 DSL 重写**。维护者在 grilling 已选定"白名单校验后再 eval"
  而非"纯数据永不 eval";`read` 无 `#.` 后门使前者可防守。别推翻它。
- **不预先设计动作词表**。只做 `cancel-agent`,其余按真实需求加。
- **不碰聊天主界面的渲染**。

## 测试与验收总纲

- 三个 stage 各自的验收见上;红队测试(3b)是**硬门槛**,缺一条绕过路径
  都算未完成。
- canonical runner(`test/run-tests.el`)必须把 `test-genui.el`(以及 3a/3c
  的测试)加进去;vui 相关测试用 `(skip-unless (featurep 'vui))` 守卫,
  这样不装 vui 的 CI/环境不会红。
- 全套跑通(除既有 3 个 `ecosystem/lsp-*` 失败),再提交。
- 提交粒度:3a / 3b / 3c 各一个 commit,message 说清"为什么这么设计"而非
  "改了什么"。安全相关的决定(铁律 1-3)必须在 commit message 里有据可查。

## 交接注记

- 每个 stage 结束请回报,我(架构侧)会审查——尤其 3b 的红队测试,我会用
  batch Emacs 独立跑绕过尝试。
- 遇到 vui API 与本文档"侦察结论"不符,**以实际 API 为准并回报**(vui 版本
  可能变);不要照本文档硬写。
- 版本号/tag/发布一律不碰,交维护者。CHANGELOG 写在 Unreleased 段。
