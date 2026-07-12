# Goal: 子代理异步化 —— async sub-agent engine

> 状态:**已实现(2026-07-12,main 分支,未打 tag)。** 版本归属:v1.2.1(补丁级,原候选号 v1.3)。
> 创建于 2026-07-12。实现于同日。
>
> 实现笔记:Part A-E 全部落地。与方案的偏差:子代理不复用
> `superchat--llm-generate-answer`(它绑定主 buffer 的超时 timer 与
> 模式指示器),而是新增轻量的 `superchat--subagent-llm-async`
> 直接走 `llm-chat-async`;工具包装也不复用 agent-loop 的
> wrap(其渲染目标经由动态 `superchat-buffer-name`,异步回调时
> 会指向主 buffer),改为闭包捕获 ctx 的独立包装器——工具错误
> 以字符串返回而非重新 signal,避免打断 llm.el 的异步机器。
> 隶属:multi-agent 方向(subagent 子系统)。
> 前置:v1.2 的 workflow 异步引擎(`superchat-workflow.el`)已验证回调链模式可行。

## 背景:multi-agent 的最后一块短板

v1.2 时点,superchat 的 multi-agent 能力盘点:

**已具备**:LLM 自主委派(`delegate_to_subagent` / `delegate_to_subagent_parallel`)、
角色化预设(researcher / executor / introspector,权限按角色收窄)、
真隔离(独立 session_id / tape / buffer / 对话历史)、
黑板协调(workspace 工具)、权限门控(agent permission hooks)。

**短板全部集中在执行模型上**(`superchat-subagent.el`):

1. **同步阻塞**:`superchat--subagent-run`(:93)走
   `superchat--llm-generate-answer-sync`,子代理运行期间 Emacs 冻结。
   v1.2 已把 workflow 引擎改成异步回调链,子代理没跟上。
2. **假并行**:`superchat--subagent-run-parallel`(:148)用
   `make-thread` + `thread-join`。Emacs 线程是合作式的,HTTP I/O
   无真并发,且 `thread-join` 期间主线程等待——UI 照样冻结。
   代码内 docstring(:153-156)已自认此限制。
3. **只有 fork-join**:派出→等待→收报告。主代理无法中途干预,
   子代理之间无运行中通信(workspace 理论可用,但阻塞模型下没有"中途")。
4. **无递归守卫**:内置预设的工具表不含 delegate 工具,但自定义
   agent preset 若加入 `delegate_to_subagent` 可无限套娃,无深度限制。
5. **并行写 workspace 顺序不确定**:多线程交错写同一区域,无排序。

## 诉求

1. 子代理运行期间 **Emacs 保持可交互**(与 workflow 引擎同标准)。
2. 并行委派获得 **真正的 HTTP 层并发**(llm.el 异步 API + 多 curl 进程)。
3. 主 buffer 有 **进度渲染**:委派时插入占位行,完成时原位回填报告。
4. **深度守卫**:defcustom 限制委派深度,默认 1(子代理不得再委派)。
5. 并行结果 **聚合顺序确定**(按 spec 顺序,不按完成顺序)。

## 关键洞察

### 洞察 1:异步化的真正难点是动态绑定失效

同步版靠动态 `let` 营造子代理环境(`superchat-buffer-name` /
`superchat--session-id` / `superchat--conversation-history` /
`superchat--active-preset`,见 :105-108)。异步回调触发时这些绑定
早已退出作用域——**子代理上下文必须改为显式对象**(closure 捕获的
plist / struct),tape 写入的 session_id、报告渲染的目标 buffer
都从上下文对象取,不再依赖动态环境。这是本目标最大的重构点。

### 洞察 2:异步模型反而消灭了数据竞争

Emacs 的异步回调(进程 filter / sentinel / timer)全部在主线程执行。
迁移到回调链后,并行子代理对 workspace / tape / buffer 的写入天然
串行化——只剩"完成顺序不确定",不再有交错写。短板 5 由架构自动解决,
只需在聚合层按 spec 顺序排序。

### 洞察 3:llm.el 工具本就支持 async

`superchat--maybe-make-llm-tool` 与 agent-loop 的 wrap-function 已有
async 分支(callback 作首参)。把 delegate 工具改注册为 **async tool**,
主代理的 agent loop 在等待子代理时不阻塞,llm.el 原生挂起该工具调用。
不需要发明新机制。

## 方案

### Part A — async runner(核心)

- 新增 `superchat--subagent-run-async (preset-name task context callback)`:
  - 构造显式上下文 plist:`(:session-id ... :preset ... :history nil :depth N)`。
  - 走 `superchat--llm-generate-answer`(异步版,已存在,签名含
    callback / stream-callback / tools / agent-mode)。
  - 完成回调里:tape 落盘(显式 session-id)、`funcall callback report`。
  - 错误路径:`[Error: ...]` 字符串同样经 callback 返回(与 workflow
    引擎的错误约定一致),不 signal。
- 同步版 `superchat--subagent-run` 保留为 legacy(内部同步场景 +
  向后兼容),docstring 标注。

### Part B — delegate 工具切换 async

- `delegate_to_subagent` 注册为 async tool:
  `(lambda (callback preset task &optional context) ...)`。
- `delegate_to_subagent_parallel`:N 个 async 子代理同时发起
  (curl 子进程天然并行),计数器 + 结果向量(按 spec 索引写入),
  全部完成后聚合并 `funcall callback`。
  `superchat-subagent-parallel-max` 改为发起窗口(信号量式排队),
  不再是线程块大小。
- 删除 `superchat--subagent-run-in-thread` 与 make-thread 路径。

### Part C — 进度渲染

- 委派时主 buffer 插入占位行(如 `⏳ Sub-agent researcher: running…`),
  用 text-property 或 marker 标记。
- 完成时原位替换为 `** Sub-agent report: NAME` + 报告(复用
  `superchat--subagent-render-report` 的格式)。
- 多个并行子代理各自持有占位 marker,互不干扰。

### Part D — 深度守卫

- `defcustom superchat-subagent-max-depth 1`。
- 上下文 plist 携带 `:depth`;子代理环境中 delegate 工具检查
  `depth >= max` 时直接返回错误字符串(不 signal,保持工具契约)。

### Part E — 测试计划

- mock 异步 LLM(参考 test-workflow.el 的 mock 模式):
  - 回调链完整性:委派 → 报告回填 → 主代理续跑。
  - 并行聚合:乱序完成,断言按 spec 顺序聚合。
  - 深度守卫:depth=1 的子代理再委派 → 错误字符串。
  - 错误传播:子代理失败不拖垮兄弟代理,聚合报告含 `[ERROR: ...]`。
- 迁移 test-subagent.el 中依赖同步返回值的用例。

## 不做的事(本目标范围外)

- 运行中的代理间消息总线(workspace 黑板已够用,等真实需求)。
- 子代理取消/超时 UI(依赖 llm.el 的请求取消能力,另立目标)。
- workspace 的写锁(见洞察 2,异步模型下不需要)。

## 参考

- `superchat-workflow.el` Part C —— 已验证的异步回调链模式(v1.2)。
- `superchat-subagent.el` :93-171 —— 待替换的同步/线程实现。
- `superchat-agent-loop.el` async wrap 分支 —— async 工具的既有契约。
