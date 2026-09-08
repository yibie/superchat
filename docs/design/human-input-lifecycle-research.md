# 人类输入生命周期：Superchat、pi 与 DeepSeek Harness

日期：2026-09-08。状态：源码调研与设计候选，不是已批准的实现规格。

## 结论先行

问题不是“Workflow 应该多等几秒”，也不是“侧窗应该变成 minibuffer”。
目前展示一个选择界面就完成了工具调用；之后点击按钮是另一条聊天输入。
缺少的是：明确请求人类输入、把回答交回这个请求、在请求未解决时阻止相应执行提前推进的合同。

三个必须分开的概念：

- **展示**：内容或动作入口已呈现，不必等待用户。
- **提问**：一个具体执行请求用户输入，回答须关联原请求。
- **审批**：允许或拒绝某个 Effect，不因用户回答了城市名称而取得权限。

“问题已结算”也不等于“已取得继续所需的信息”。取消、跳过、界面不可用不能冒充有效回答。
结构合法的自由文本不保证语义充分；系统不能声称自己能识别所有自然语言中的依赖条件。

## 来源与方法

三个现有 Herdr Codex/Astra worker 分别读取 pi、dsh、Superchat；leader 对照证据并与反思者进行三轮交叉讨论。
没有 Claude 参与。本轮没有运行产品或参考项目测试、安装依赖、访问 provider、操作 GUI/daemon，亦没有生产代码改动。
结论是源码事实与设计推论；阅读已有测试不等于本轮测试通过。

持久参考目录为 `/Users/chenyibin/Documents/reference-code/`，leader 核对两个 checkout 的 HEAD 与干净状态：

- pi：`3e4bc2680ea8eca162207974b88667ccfc20a564`，上游 `earendil-works/pi`。
- dsh：`c389f96bf3a9b6807cb71ed6bdad5849be0df6d8`，上游 `deepseek-ai/deepseek-harness`。

下列外部链接固定到这些提交；本地 Superchat 引用以符号为主，避免把历史行号当作当前合同。

## Superchat：现在到底发生了什么

1. `superchat-tool-render-ui` 是同步工具；`superchat-genui--render-expanded` 显示界面后返回 buffer 名称。
   它没有把“用户尚未选择”作为工具未完成状态。
   来源：[工具](../../superchat-tools.el)、[GenUI](../../superchat-genui.el)。
2. `superchat-genui--build-send-to-chat` 捕获固定文本，点击时寻找聊天 buffer，保护已有草稿，然后调用 `superchat-send-input`。
   它没有捕获原问题的待完成回调，也不携带原问题身份。这是新输入，不是原工具的回答。
   同时 [render-ui skill](../../skills/render-ui.md) 建议“需要选择时用按钮”，造成能力描述与执行语义错位。
3. 原生 `llm-provider-utils-execute-tool-uses` 为异步工具传入完成回调，全部结果到达才调用整体 success。
   但循环仍启动同批后续工具：延迟整体完成不等于阻止同批 Effects。
   核对的依赖是本机 `llm-0.31.1/llm-provider-utils.el:929–1023`，不是对事故 daemon 加载版本的证明。
   [Agent Loop](../../superchat-agent-loop.el) 与 [Authority](../../superchat-authority.el) 已有异步包装及结果交付 seam。
4. `superchat--pause-boundary` / `superchat-resume` 是人工暂停的精确 continuation。
   它发生在下一模型请求之前，不是人类问题协议；暂停取消 wait timer，但 hard timer、Runtime deadline 分别存在。
   来源：[UI](../../superchat.el)、[LLM](../../superchat-llm.el)。
5. `superchat-workflow--resume-decision` 用 Closure 判断执行完成后推进；它不判断文本中的“尚缺城市”。
   这不是 Closure 证明错误，而是执行完成与业务信息充分并非同一事实。
   来源：[Workflow](../../superchat-workflow.el)。

独立问题：LLM timeout 的 finalize 与 Runtime 显式 cancel/deadline 的 transport abort 路径不对称。
修复取消释放不会自动建立问题归属；新增人类输入也不能代替验证 transport 是否真的停止。

## pi：小接口中的等待，也有明确局限

- [question 示例](https://github.com/earendil-works/pi/blob/3e4bc2680ea8eca162207974b88667ccfc20a564/packages/coding-agent/examples/extensions/question.ts#L44)
  注册 sequential 工具，在 execute 内 await `ctx.ui.custom`。这不是默认工具集合中的功能。
- 回答通过该 invocation 的 Promise 返回，再由
  [agent loop](https://github.com/earendil-works/pi/blob/3e4bc2680ea8eca162207974b88667ccfc20a564/packages/agent/src/agent-loop.ts#L686)
  包成原 `toolCallId` 的结果；不是普通用户消息。
- sequential 标志使同个 response 的整批工具串行，但不能撤销排在问题前面的 Effects。
- Esc 返回取消选择的文字结果，循环可继续；示例忽略 `_signal`。
  因此不能把它当成“必需回答”和取消传播已完整解决的模板。
- [custom UI 实现](https://github.com/earendil-works/pi/blob/3e4bc2680ea8eca162207974b88667ccfc20a564/packages/coding-agent/src/modes/interactive/interactive-mode.ts#L2758)
  有单次关闭保护，但 custom 与带 signal/timeout 的 input/select 接口不同。
  对该示例只 abort Agent 不足以让等待 Promise 结算，是源码推论，未运行复现。
- 模型 response 在工具阶段前已结束；所查 loop 没有人类等待计时器。
  不能据此声称它实现了 Superchat 各类时钟的自动暂停/恢复。

## dsh：输入请求与 UI 展示解耦

实际路径：

```text
ask_user_question
  → await scoped user-questions/request
  → Host pending event → Client PendingQuestion → 用户提交
  → 原 pending event 结算 → 原 tool call 的结果 → loop 继续
```

- [提问工具](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/interaction/tool-ask-user/src/index.ts#L58)
  await 输入，而 [问题服务](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/interaction/user-questions/src/index.ts#L86)
  分发请求。UI 不直接决定 agent 执行完成。
- [PendingQuestion UI](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/client/ui-user-questions/src/client/index.ts#L58)
  等待结构化回答并在 finally 中移除呈现。
- [Gateway](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/api/gateway/src/index.ts#L413)
  用 pending eventId 与 clientId 处理回复；已结算或过期 delivery 的回复不重新完成执行。
  浏览器重连可再次看到 Host 内存中尚待处理的事件。这不是宿主重启后恢复 continuation 的保证。
- [审批服务](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/interaction/user-approval/src/index.ts#L258)
  是另一条合同：只允许明确允许的结果通过，并主动 race abort。
  question 服务没有对所有自定义 answerer 提供同样的 abort race；不能泛化为统一取消保证。
- 提问工具没有 `timeoutMs`，对应 timeout policy 不为它启动工具计时器。
  这与“人类等待时暂停所有既有超时”是两种不同设计。
  来源：[timeout policy](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/guard/timeout-policy/src/index.ts#L55)。
- 问题工具没有声明 concurrency safety，因而是独占工具屏障，但之前的 Effects 仍可能已发生。
  来源：[工具调度](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/core/tools/src/index.ts#L1266)。
  而同批后续工具的参数是在回答之前生成的；串行等待并不根据新回答重新生成这些参数。
  来源：[批次执行](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/core/agent-loop/src/tool-calls.ts#L199)。
- UI 可以跳过，编码为 `selected: []`；所查问题服务没有强制完整回答校验。
  所以 dsh 同样不能直接证明“获得有效必需信息才继续”。
  来源：[QuestionComposer](https://github.com/deepseek-ai/deepseek-harness/blob/c389f96bf3a9b6807cb71ed6bdad5849be0df6d8/packages/client/ui-user-questions/src/client/QuestionComposer.tsx#L203)。

## 三轮反思带来的收敛

1. **等待不等于满足条件。** 不能只将 render_ui 异步化；必须区分回答、取消、无效输入，而且同批后续工具可能先执行。
2. **连续性不等于持久化。** dsh 的重连仍依赖活着的 Host；准确关联并不要求先建设新 Task 注册表。
   关闭面板不能留下找不回的等待；取消的实际范围必须与界面文字一致；三个时钟须逐一归类。
3. **收窄承诺。** 只对明确发起的输入请求承诺归属、结算和后续执行约束，不声称识别所有自然语言依赖、回滚此前 Effects 或验证答案的全部业务含义。

## 首选设计候选，而非实现授权

先比较并验证一个进程内的显式提问操作。其小 Interface 应隐藏关联、结算与呈现生命周期，UI 只做 Adapter：

- 生产者明确请求输入；渲染卡片和普通 send-to-chat 保持各自用途。
- 运行时保存该次请求的关联；不以当前 buffer 名、最新 Run 或显示文本推断。
- 回答原请求一次；有效性限于声明的结构约束，如非空、合法选项、选择数量。
- 等待期间不继续下一模型轮或相应 Workflow 步骤；无关工作和 Emacs 输入仍可进行。
- 隐藏面板不算回答，须可重新打开；过期、取消必须显式失效并拒绝旧回复。
- 取消要标明实际范围：当前回答或相应 Workflow 执行。取消后不能借普通 completed 偷渡到下一步。
- human wait 与模型沉默分开；transport hard cap 和 Runtime 整体期限如何处理须显式决定，不能全局关闭计时器。
- 首期不承诺 Emacs 重启后恢复；Tape 的审计记录不自动成为可执行 continuation。

暂不引入：自然语言“缺信息”分类器、通用依赖图、远程多客户端协议、持久化执行调度器、所有 UI 共用的全局暂停、用回答自动授予 Authority。

替代方案是持久 suspended interaction：适用于明确要求跨进程恢复/终结后接续的工作。
它需要恢复规则、版本、审计和重新执行语义，当前事故尚不足以要求支付这些复杂度。
进程内方案仍需真实取消与失效处理，不是仅保存一个 callback。

## 设计前必须做的两项可证伪调查

### A. 工具批次准入

比较独占 ask-only 批次与串行屏障。前者更容易定义保证，但必须在任一 Effect 前见到完整批次；不能在 ask 已执行时才检查。
需分别核实原生 streaming/async 路径的整批 seam，不预设修改上游 parser 或 Authority。

最强反例是 `[effect, ask]` 与 `[ask, effect]`。如果选择 ask-only 合同，两者都必须在 Effect 前拒绝；多个 ask 如何处理也须明确。
如果选择串行合同，就不得宣称保护问题之前的 Effects。任何方案都不撤销此前批次的行为。
还须定义必要条件何时生效：混合批次被拒绝后，不能仅返回一段错误文字，就默认下一次普通 completed 表明该条件已满足。
这不意味着必须新增条件注册表，而是准入失败与后续推进的语义也需要验收。

### B. 等待、取消与期限

沿真实 Runtime → native llm → Authority → 回答 → Workflow 路径，确认：

- withheld 回答时，后续请求与步骤为零；合法回答后仅继续一次。
- 空/无效回答、重复回答、切换 Session、清空、关闭、取消、超时后的回复不错误推进。
- 模型沉默、transport hard cap、Run deadline 各由谁拥有，哪些时间包含人类等待。
- 面板隐藏后可找回，过期后能看见原因，无关 Run 正常工作。
- 取消途中完成回调重入时，Authority/Tape/Closure 不出现伪造成功或缺失收尾。
- 回答为合法“我不知道”时，后续可以澄清，但不把格式合法表述为城市已获得。

必须保持 [ADR 0004](../adr/0004-session-editor-workspace-authority.md) 的 Session editor ownership 与 Runtime 生命周期分离，
以及 [ADR 0006](../adr/0006-authority-refusal-consumer-delivery.md) 的拒绝一次交付/原始 consumer condition 语义。
如果新设计确实需要改变它们，应明确提出冲突，而不是把人类等待当作例外绕过。

这两项调查完成后才能确定实现范围、公开状态、工具说明和验收；本文不宣称已经 code-ready。
