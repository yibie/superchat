# Goal: Agent Profiles —— 让 preset 成为真实的运行时契约

> 状态:**进行中——Phase 1 已实现,Phase 2+3 待实现。**
> 里程碑:v1.3(harness contract 的一部分)。
> 创建于 2026-07-12。
> 隶属:**`agent-harness.md` 北极星**。v1.3 的完整排序:
> Phase 1 基础契约(✅)→ agent registry(北极星缺口 1,不在本文档)
> → Phase 2+3 字段(北极星缺口 2)。
> 前置:v1.2.1 异步子代理引擎(显式 ctx 是 profile 的天然运行时载体)。
>
> 选型结论(2026-07-13,调研 6 候选后):**方案 F,5 个 typed
> slots**,即本文档 Phase 2+3 的直译。routing 不新增 when_to_use
> 字段——agent registry 复用现有 description。选择理由与边界已
> 内联在本文档,不依赖外部设计稿。

## 背景:preset 是 profile 的数据模型雏形,但不是可靠的运行时契约

`superchat-preset` 看起来已经是 per-agent profile(角色、模型、工具集、
类型),但 2026-07-12 的代码审查证实:**四个字段里只有两个真正贯通了
运行时**,其余是"能解析、能存储、不消费"的空契约:

| Profile 维度 | 实际现状 |
|---|---|
| 角色/人格 `:body` | ⚠️ 只在显式 `>skill` 时作为普通 prompt context 注入(`superchat-skills-build-prompt`);`/agent` 后续回合和 delegated subagent 都没有使用它,也不是真正的 system message |
| 专属模型 `:model` | ⚠️ 能解析并写进 turn,但 dispatcher 在 `superchat-preset-apply` **之前**缓存了 `target-model`(superchat-dispatcher.el:288 vs :293);`superchat--execute-llm-query` 只接受显式传参、从不回读 `turn.target-model`,子代理无参调用因此拿不到 preset 的 model |
| 工具集 `:tools` | ✅ 非空名单可以收窄;⚠️ `tools: []` 解析为 nil,而 nil 表示"继承全局工具",无法表达零工具 |
| 执行类型 `:type` | ✅ 真正参与 agent/plan/workflow 分流 |
| `:backend` | ❌ 能解析(superchat-preset.el:174)、能存储(:209),没有任何运行时消费者 |

关键证据:

- `superchat-preset-apply`(superchat-preset.el:102)只写 preset/model/tools,不消费 body。
- body 只在 `superchat-skills-build-prompt`(显式 skill 调用)中拼接。
- 异步子代理(superchat-subagent.el `--run-async`)直接
  `(setf turn-prompt (concat context task))`,preset body 不参与;
  同步 runner 同样。
- `tools: []` 实测(2026-07-12)解析为 nil。
- frontmatter parser 不是真 YAML:`0.2`、`20`、`true` 均得到字符串,
  新增数值/布尔字段必须显式类型转换 + 范围校验 + 错误提示。

**因此本目标的第一阶段不是加字段,而是把已有字段的语义端到端补通。**

## 分期

### Phase 1 —— 补通现有契约(本目标的核心)—— ✅ 已实现(2026-07-13)

> 实现笔记:比预期多修了一层——`turn.system-prompt` 本身就是
> 死端 slot(hooks 在填、无人消费),连语言指令 hook 的输出也
> 从未到达过 LLM。Phase 1 把整条 system-prompt 管线接通
> (`build-llm-prompt` 增加 `:context` 参数,generate-answer
> 同步/异步两版透传,dispatcher/agent-run/subagent 四个调用点
> 全部接入),preset body 顺着这条管线走。model 语义定为
> "用户显式 @model 优先,preset 填空";`tools: []` 用 `'none`
> 哨兵贯穿 preset→turn→collect;`:backend` 按既定立场删除
> (slot、双份 loader、validate、export field-order 全清)。
> agent/plan 类型显式调用时 skills-invoke 不再把 body 拼进
> user prompt,避免与 system prompt 双份注入。
> 17 个契约测试(test-preset-contract.el)锁死全部语义。

1. [x] **body 进入委派 prompt**:delegated subagent(sync + async 两条路径)
   把 preset body 作为 system 语境注入;`/agent` 模式的后续回合同样。
   body 通过 llm.el 的 `:context` 注入,与
   ob-superchat/magit/rewrite 在 v1.2.0 修正后的用法一致。
2. [x] **model 贯通**:dispatcher 在 preset-apply **之后**再取
   `target-model`;`superchat--execute-llm-query` 以显式 `@model`
   为优先、preset model 填空,同步修复子代理路径。
3. [x] **`tools: []` = 零工具**:区分 "字段缺失(继承)" 与 "显式空表
   (无工具)",以 `'none` 哨兵贯穿 preset、turn 和 tool collection。
4. [x] **`:backend` 定夺**:删除无运行时消费者的 slot、loader 和
   export 字段,不再保留"能写不能用"的配置。
5. [x] **回归测试锁死以上四条**:`test/test-preset-contract.el`
   的 17 个测试覆盖运行时契约,并已加入 canonical runner。

### Phase 2 —— 推理参数(typed slots,不加 :profile plist)

- `temperature`、`max-tokens`、`reasoning` 三个 **typed slot** 直接加在
  `superchat-preset` 上。不引入宽泛的 `:profile` plist——preset 本身就是
  profile,双层配置来源(`preset.model` vs `preset.profile.temperature`)
  会把稳定的公开字段变成字符串化的隐式协议。
- `superchat--build-llm-prompt` 透传(llm.el 的 `llm-make-chat-prompt`
  原生支持 `:temperature` / `:max-tokens`);reasoning 覆盖全局
  `superchat-llm-reasoning`。
- frontmatter 显式类型转换 + 范围校验(温度 [0,2] 等),非法值给出
  明确 warning 而非静默吞掉。

### Phase 3 —— 只收紧的护栏(tighten-only)

Profile **只能收紧**全局值,不能放宽:

```
effective-max-tool-calls = min(global, profile)
effective-confirm        = global-confirm OR profile-confirm
```

- 字段:`max_tool_calls`、`confirm_destructive`。
  缺省 = 继承;试图放宽(如全局 confirm=t 时写 `confirm_destructive:
  false`)被忽略并显式 warning,不静默生效。
- 理由:SKILL.md 是可分享的文本文件,一旦复制进本地目录就无法凭
  `:source` 字段区分信任等级。要"executor 80 次免确认",用户应显式
  调全局值,再让 researcher 收紧。
- 执行点:v1.2.1 的子代理工具包装器已闭包捕获 ctx,把全局 defcustom
  读取替换为 min/OR 合成即可;主 agent loop 的 wrapper 同理。
- `superchat-subagent-max-depth` 第一版继续是全局编排上限。它约束
  调用树而非单个 agent 的工具预算;没有真实的差异化需求前不加入
  profile。这样 Phase 2+3 严格保持选定的 5 个 typed slots。

## 明确不做(第一版)

- **不改 permission hook 签名**。现有 hook 调用被 `ignore-errors` 包裹
  (superchat-agent-loop.el:69/:91):把 `(tool-name args)` 改成
  `(tool-name args ctx)` 会让旧的两参数 hook 抛 arity error 后被静默
  忽略——一个本该返回 deny 的安全 hook 变成"没有意见",这是实质性的
  授权回归。profile 的内建护栏由 wrapper 直接执行;将来若需可编程的
  per-agent 权限,新增 context-aware 的**新** hook 变量,不动旧协议。
- **per-agent timeout**:异步子代理直接走 `llm-chat-async`,真正的
  timeout 需要 timer + 请求取消能力,不是加字段能解决的;另立目标。
- **workspace 写权限布尔字段**:挡不住 `shell-command` 直接写文件,
  给出虚假安全感,不做。
- **tape 粒度开关**:tape 是审计记录,不允许 profile 自行关闭。
- **memory policy**:需先定义 none/session/shared 的语义,暂缓。

## 验证计划

- [x] Phase 1 回归:委派路径 prompt 含 body;preset model 到达
  effective-backend(mock 后断言);`tools: []` 得到空工具集;
  `:backend` 字段移除。
- Phase 2:frontmatter 类型转换(字符串→数值/布尔)、非法值 warning、
  参数到达 `llm-make-chat-prompt`。
- Phase 3:min/OR 合成矩阵(全局松/紧 × profile 松/紧 四象限);
  放宽尝试被忽略且有 warning。

## 工作量判断

Phase 1 已作为独立提交完成。剩余 Phase 2+3 仍不应合成一次大改:
Phase 2 需要给非-YAML 的 frontmatter parser 补类型层;Phase 3 涉及
安全边界和主/子 agent 两套 wrapper。各自以独立测试矩阵锁死后再推进。
