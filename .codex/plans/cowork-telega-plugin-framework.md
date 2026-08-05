# Cowork Telega：从角色工作流收敛为上下文插件入口

状态：Approved plan  
范围：`plugins/superchat-cowork-telega/` 与兼容入口 `superchat-telega.el`  
不包含：本轮不实现代码，不修改 Extension schema，不重写 Dispatcher

## 目标

把当前面向用户的 reply、digest、triage 三套角色式工作流，收敛成一个
`M-x superchat-telega` 上下文入口：

1. 用户用自然语言表达意图。
2. 插件根据当前 Telega 上下文选择下一段 host-side flow。
3. 含混或混合意图先澄清；缺少 chat scope 时使用现有选择/确认 UI。
4. 每类任务仍构造不同、显式收窄的 Runtime Request。
5. 结果回到原来源：reply 回 Telega，digest 回 Org buffer，triage/rules
   回 message/Tape。

这一步只收敛产品入口，不宣称已经实现完整的 Codex 式多轮代理。

## 设计原则

1. **入口统一，权限分离。** 单一命令不等于单一大权限 profile。
2. **路由不是安全边界。** 安全仍由 explicit tools、Authority policy、
   effect、validator、approval 和 Tape 保证。
3. **复用现有主干。** 保留 Extension composition、Runtime submit 和
   Authority execution，不新增 plugin/surface framework。
4. **先锁行为，再删重复。** 先补 Request 形状与兼容入口测试，再把旧命令
   降为 wrapper。
5. **第二个真实复用方出现前不抽象。** Telega 私有的 5–10 行协调逻辑留在
   plugin 内。

## 决策

### 1. Canonical 入口

在
`plugins/superchat-cowork-telega/superchat-cowork-telega.el`
中定义交互命令 `superchat-telega`。

- 文件与 feature 继续叫 `superchat-cowork-telega`。
- 使用方加载 `(require 'superchat-cowork-telega)`。
- 不把 plugin feature 改成 `superchat-telega`，避免和现有
  `superchat-telega.el` / feature 冲突。
- `superchat.el` 不 eager-require plugin。

### 2. Plugin-local 纯路由

新增内部纯函数：

```elisp
(superchat-cowork-telega--route-intent instruction host-metadata)
;; => (:route reply|digest|triage|rules|clarify
;;     :confidence high|medium|low
;;     :reason "...")
```

约束：

- 输入只包含用户本次 instruction 与 host metadata。
- host metadata 可包含 surface、当前 chat id、已选择 chat 数、command
  origin；不得把 selected message 内容用于权限路由。
- selected message 只是 reply context，不能改变 route 或工具面。
- 混合意图或低置信度返回 `clarify`；入口先用短提示或
  `completing-read` 澄清，不提交 Runtime Request。
- 路由可使用最小确定性 heuristic；具体关键词只是 UX 实现细节，不构成
  安全合同。
- `rules` 的 list/remove 继续走 host-side 本地命令，不为了统一而塞进
  model run。

### 3. Request 形状就是权限合同

| Route | 显式工具面 | Authority / validator |
|---|---|---|
| reply, draft | `none` | 不发送 |
| reply, approve | `telega_send_message`, `memory_search` | 当前 chat validator；最终 draft 必须审批 |
| digest | `telega_read_messages`, `memory_search` | scope approval；policy deny external-write |
| triage | `telega_propose_rules` | sampling consent；policy deny external-write / exec / 非规则写入 |
| rules | 无 model tools | 现有本地 list/remove 与 Tape |

reply approve Request 必须补上显式 `:tools`。不得依赖 Telega extension 的
全量工具注入后再用提示词约束模型。

Extension composition 的既有“显式 `:tools` 不被放宽”语义必须由回归测试
锁定。包含 `send`、`forward`、`reply` 等内容的 hostile prompt 或 selected
message 不能扩大 digest/triage 工具面。

### 4. 两层确认，各管一件事

只有满足以下条件时才做 route confirmation：

```text
统一自然语言入口自动推断 + write-capable reply route
```

route confirmation 只展示并确认：

- 将进入 reply workflow；
- 当前 target chat / scope。

拒绝后不得构造或提交 Runtime Request。

以下路径不重复做 route confirmation：

- 旧 reply-specific wrapper；
- 调用方显式传入 route；
- 用户主动执行 reply-specific 命令。

最终 draft 的 recipient、chat id、完整消息内容仍必须在
`telega_send_message` 执行前经过 Authority approval。route confirmation
不能替代最终副作用审批。

### 5. 旧命令与双实现

Plugin 内现有命令保留为 compatibility/debug wrappers：

- `superchat-cowork-telega` 固定 route 为 `reply`
- `superchat-telega-digest` 固定 route 为 `digest`
- `superchat-telega-triage` 固定 route 为 `triage`
- `superchat-telega-rules` 与 `superchat-telega-rule-remove` 保留本地行为

wrapper 不复制 scope、Request construction、submit 或 delivery 逻辑。

Root `superchat-telega.el` 第一阶段采用兼容软转发：

1. `superchat.el` 继续只 autoload `superchat-telega-cowork`。
2. 用户实际调用该命令时，尝试
   `(require 'superchat-cowork-telega nil t)`。
3. plugin 可用时，转调 plugin 的 explicit reply wrapper。
4. plugin 不在 load-path 时，暂时执行 root legacy fallback。
5. `(require 'superchat)` 不得因此加载 plugin。

兼容 fallback 有明确寿命：经过两个 minor releases，或 plugin 进入默认发行
路径后（以先到者为准），删除重复 legacy 实现；root 文件只保留 soft shim
和清晰安装提示。若 soft delegation 引发重复工具注册或加载顺序问题，本轮可
回滚为 root legacy 不变，但这只是发布回退，不算完成双实现收敛。

## 不做什么

- 不给 `superchat-define-extension` 增加 `:router` / `:intents`。
- 不扩展 `superchat-build-request-plan` 支持 Telega agent/preset flow。
- 不新增 Surface Adapter、session factory 或新的通用 request type。
- 不使用 LLM classifier 作为前置安全判断。
- 不增加依赖。
- 不恢复 D-Bus、desktop notification 或后台无人值守行为。
- 不在本轮实现跨 turn scope lease 或多轮 scoped conversation。

## 执行阶段

### 阶段 0：回归测试先行

修改：

- `plugins/superchat-cowork-telega/test-cowork-telega.el`
- `test/test-ecosystem.el`

先增加失败测试：

1. `route-intent` 对相同 instruction 与 host metadata 给出稳定的
   route/confidence/reason。
2. 改变 hostile selected message 内容不改变 route。
3. 混合或低置信意图进入 `clarify`，且不 submit。
4. reply approve composition 后工具集合恰为
   `("telega_send_message" "memory_search")`。
5. reply 不包含 `telega_read_messages` 和 `telega_propose_rules`。
6. digest/triage 即使 prompt 要求发送，组合后仍无 send tool。
7. digest 缺少或拒绝 scope consent 时不 submit。
8. triage 缺少或拒绝 sampling consent 时不 submit。
9. 自动推断 reply route 拒绝 route confirmation 后不 submit。
10. explicit reply wrapper 不做 route confirmation，但最终 send 仍经过
    Authority。
11. 三个 plugin 旧命令与 canonical runner 的固定 route 等价。
12. root `superchat-telega-cowork` 在 plugin 可用时 soft-delegate。
13. plugin 不可用时 legacy fallback 仍可加载。
14. `(require 'superchat)` 后 `superchat-cowork-telega` feature 仍未加载。

验收：测试先在预期合同处失败，现有 Authority、Tape、validator、无 D-Bus
回归不放松。

### 阶段 1：收窄现有 reply Request

修改：

- `plugins/superchat-cowork-telega/superchat-cowork-telega.el`

只给 approve reply Request 增加显式 send + memory 工具面；draft 继续
`none`。

验收：

- reply 工具集合精确相等测试通过；
- 当前 chat validator 和最终 Authority approval 测试继续通过；
- digest/triage 测试无变化。

回滚：删除该显式字段即可恢复；不涉及数据迁移。

### 阶段 2：统一入口与纯路由

修改：

- `plugins/superchat-cowork-telega/superchat-cowork-telega.el`

实现：

- `superchat-cowork-telega--route-intent`
- 一个最小的 plugin-local route runner
- `M-x superchat-telega`
- `clarify` host flow
- 仅自动推断 write route 的 route confirmation

复用现有 reply/digest/triage scope collection、request builders、extension
composition、Runtime submit 和 deliveries。

验收：

- router 本身不调用 LLM、Runtime、Authority 或 Telega RPC；
- 缺 scope 或拒绝确认时不 submit；
- unified entry 不走 blocking submit；
- 没有新增依赖或 core schema 改动。

回滚：删除入口/router；旧命令仍可调用原 builders。

### 阶段 3：旧 plugin 命令降为 wrapper

修改：

- `plugins/superchat-cowork-telega/superchat-cowork-telega.el`

让旧命令传 explicit route 调用统一 runner。explicit route 不再触发额外
route confirmation。

验收：

- 旧命令名仍是 interactive；
- 用户已有 keybinding 不失效；
- wrappers 不复制 Request construction 或 submit；
- wrapper 等价测试通过。

回滚：恢复旧函数体；builders 与新入口不受影响。

### 阶段 4：root soft delegate

修改：

- `superchat-telega.el`
- `test/test-ecosystem.el`
- 原则上不修改 `superchat.el`

让 `superchat-telega-cowork` 在调用时优先 soft-load plugin 并转调 explicit
reply wrapper；plugin 缺失时执行 legacy fallback。

验收：

- core eager-load integration 测试通过；
- plugin 可用与不可用两条路径均有 ERT；
- soft delegate 不重复注册工具；
- root entry 不旁路 canonical plugin Request 权限合同。

回滚：恢复 root legacy entry；plugin 入口仍可独立发布。

### 阶段 5：文档收口

修改：

- `plugins/superchat-cowork-telega/DESIGN.md`
- `docs/plugin-guide.md`
- `README.md`
- `README_cn.md`

文档内容：

- 主入口为 `M-x superchat-telega`；
- 加载 feature 是 `superchat-cowork-telega`；
- 旧命令是 compatibility/debug wrappers；
- route confirmation 与 Authority approval 的职责不同；
- digest/triage 的不可发送来自 explicit tools + policy；
- plugin 是 canonical Telega 实现，root 是有期限的 compatibility path。

## 验证

Plugin 本地 ERT：

```sh
rtk proxy emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
  -l plugins/superchat-cowork-telega/test-cowork-telega.el \
  -f ert-run-tests-batch-and-exit
```

主套件：

```sh
rtk proxy emacs -Q --batch -L . -l test/run-tests.el
```

Root / ecosystem：

```sh
rtk proxy emacs -Q --batch -L . -L plugins/superchat-cowork-telega \
  -l test/test-ecosystem.el \
  -f ert-run-tests-batch-and-exit
```

静态检查：

```sh
rtk grep "superchat--" plugins/superchat-cowork-telega/superchat-cowork-telega.el
rtk grep "superchat-runtime-submit-blocking|llm-chat|superchat--llm-generate-answer" \
  plugins/superchat-cowork-telega superchat-telega.el
rtk grep "dbus|notifications-notify|telega-notifications-msg-temex" \
  plugins/superchat-cowork-telega
```

完成标准：

- 所有新增与既有测试通过，0 unexpected。
- reply/digest/triage 的工具面精确符合表格。
- 所有发送仍走 validator + Authority final approval。
- scope/route 被拒绝时没有 Runtime submit 或外部副作用。
- core 不 eager-load plugin。
- 无 D-Bus/desktop-notification 生产依赖。
- 文档与实际 command/feature 名称一致。

## 后续重新设计的触发条件

只有出现以下任一事实，才重新评估 core-level surface/session abstraction：

1. 第二个非 Telega integration 重复 intent routing、scope confirmation 和
   source delivery；
2. Telega 需要一次 scope grant 后在同一来源 surface 连续多轮运行，并能在
   每一 turn 恢复、缩小和审计权限；
3. 多个 surface 需要共享 pending clarification、route history 或 scope
   lease lifecycle；
4. 删除 root fallback 后，多个 plugin 重复相同 soft-load/autoload glue。

## ADR

### Decision

Use a single contextual command in the Telega plugin, backed by a small pure
UX router and the existing narrowed Runtime Request builders. Keep authority in
request shapes and tool execution, not in intent classification.

### Consequences

- 用户只需发现一个主入口。
- reply/digest/triage 仍有彼此隔离的工具面与 policy。
- 兼容命令短期保留，但不再拥有独立业务逻辑。
- core API 本轮保持不变。

### Rejected alternatives

- 在 Extension/Dispatcher 增加通用 intent router：当前只有一个真实需求。
- 强制经过 `M-x superchat`：会削弱 source-specific delivery，且当前
  Dispatcher plan builder 不覆盖这些 agent/preset flows。
- 用单个大 profile 暴露全部 Telega tools：会破坏 digest/triage 的结构性
  不可发送。
- 立即删除 root legacy fallback：会破坏未安装 plugin 的既有用户。

### Revisit

When a second integration repeats this interaction shape, or when Telega needs
auditable multi-turn scoped conversations.
