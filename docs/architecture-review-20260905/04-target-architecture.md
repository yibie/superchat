# Superchat 综合架构：单一权威、精确对象、可组合执行

日期：2026-09-05。版本：D3，三轮审查后的最终设计稿。
状态：当前 Kernel K26；P1–P8 以[第二篇 §1 唯一状态表](02-problems.md#1-结论与证据等级)
为准。本篇保留 D0→D3 三轮审查和目标设计，不维护重复台账。

本文综合 [01 演进](01-evolution.md)、[02 问题](02-problems.md)、
[03 重构方案](03-refactoring.md)。它不替代当前 K26、已确认 SPEC 或现有 Protocol。
三轮审查结束后用户授权 P1–P8 闭环；实际采纳与有界 no-change 见 §11。

## 1. 架构决策摘要

保留三层结构和现有主要模块，不另造 Agent 框架。目标不是更多抽象，而是让一个
Interface 真正隐藏其 Implementation 的复杂性，调用者不再记住额外的名称、顺序、
状态复制和错误隔离规则。

采用三个约束：

1. **选定后不再重新识别。** 已取得 buffer object、Session、Run、event identity，
   下游使用该身份，不按标签猜测替代对象。
2. **只有 owner 改变状态。** Runtime 结束 Run，EditorWorkspace 关闭所属资源，
   Tape 追加事实，Projection 更新可重建状态，Render 修改自己的 transcript。
3. **跨 Seam 的事实必须显式。** 请求作用域通过参数表达；异步配置捕获到 lexical
   context；可选扩展定义缺席与失败结果，不能仅凭加载了某符号就改变隐含合同。

最终采纳仍遵循小切片：修复 UI 同步 exact-object 连续性与 Tape 查询参数化，随后
分别验证异步路径、隐式 seams、transport controls、存储 readers、Memory provenance
和 Workflow 简化。没有把这些工作合成一个新框架。

## 2. 当前与目标的模块图

```text
用户入口 / Workflow / Subagent / Skills / 集成 Adapter
                  │ 请求目的、组织、已选定对象
                  ▼
       准备 + Extension composition + producer acceptance
          │                        │
          │                        └── EditorWorkspace
          │                            allocation / validation / close
          ▼
       Runtime ────────→ LLM transport
          │                    │ 工具调用
          │                    ▼
          │                 Authority
          │                    │ 经审批的 effect
          ├──────────────┬─────┘
          ▼              ▼
        Tape ───────→ Closure（判断）
          │              │
          │              └── Runtime 决定是否可写 terminal seal
          ▼
    DB / Projection → Memory / Claims / Experience / Artifact Views

Runtime 的 stream / delivery → Render（exact View 中的 owner region）
Plan / Runtime 状态 → 原生 buffer / tab / side-window / widget
```

图描述职责和数据关系，不声称所有箭头都是同步调用或目前已经通过显式 adapter。
P3 指出的符号级反向依赖已逐条判断：Authority→Agent Loop、Runtime→Experience
与 LLM→Synthesis 为有界 no-change，Tool Quit accounting 已验收
（`E-20260905-006`）；Runtime→Render headless 缺席兼容已修复
（`E-20260905-008`）；普通 raw tool Request 缺少可信 callable wrapper 时在
provider 前 fail closed（`E-20260905-011`，[ADR 0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md)/K25）。prepared 是可信内部
边界，不是安全认证或外部迁移绕过方式。

## 3. 权威与 Interface

| 模块 | 调用者应知道 | 模块内部应隐藏 | 绝不能顺手承担 |
| --- | --- | --- | --- |
| Core / Request preparation | 输入、准备结果、hook 合同 | token 解析与准备顺序 | 执行 completion |
| Runtime | 有效 Request、Run handle、status/cancel | terminal、deadline、publication、quiescence | EditorWorkspace ownership 与 kill |
| LLM | 输入、transport controls、observer | provider 差异、tool rounds、流式归一化 | 独立 Run terminal 权威 |
| Authority | 工具描述、参数、context、effect/policy | validate→approve→execute→audit | 因工具可见就授予执行权 |
| Tape | append/query 语义、event/material 区别 | 事件身份、redaction、存储适配 | 当前 UI 或可变“真理” |
| Projection | reducer、scope、版本 | checkpoint 与 rebuild | 改写 Tape |
| Closure | query + trace → verdict | 生命周期证明解释 | 控制执行或保存第二 Run 状态 |
| EditorWorkspace | Session/role、exact buffer、close 结果 | marker classification、acquire/rollback、retained | 结束 Run 或依 tab 名猜资源 |
| Render | exact View 与 transcript owner | marker/overlay/region 状态 | 选择、创建或拥有 Session 资源 |

这些是设计约束，不要求每格都增加新的 public function。现有 Interface 能表达的，
直接复用；只有确有两个不同 Adapter 或真实调用负担，才建立新的 Seam。

## 4. 首批设计 A：UI exact-object 连续性

### 修复前缺口

`superchat--ensure-chat-view` 正确返回 exact owned object，但
`superchat--insert-prompt` 按请求名称重新 lookup。第二篇 P1 的真实复现证明 foreign
buffer 被写入；因此只检查 Runtime admission 是不完整的验收面。

### 首步合同

- 输入：请求的显示名称、可选 Session ID。
- 输出：实际创建/复用的 live owned buffer；可因名称冲突带后缀。
- 效果：仅在返回 object 初始化 mode/title/prompt；foreign buffer 不变。
- 缺少 Session ID：保留现有生成/复用规则，不新建全局 Session registry。
- 权威：调用现有 EditorWorkspace public Interface；不直接解释 markers。

首步允许的兼容实现是取得 object 后绑定它的**实际名称**以调用现有同步 Render
入口，范围包含 `superchat--ensure-chat-view` 初始化和 `superchat--render-session-view`
同步重建两处；后者也重新绑定了请求名称，不能漏修。这不是长期身份方案，
也不能覆盖未经验证的异步路径。
仓库异步生产入口随后验证为已经捕获 exact object（`E-20260905-007`），因此没有
实施全 Render 私有接口迁移。未知 private caller/advice 或未来新增入口若不满足该
前提，再触发独立设计；本结论不把未实施迁移算作完成。

时序必须精确：ensure 在 rename 和 mode 初始化完成后，紧邻 Render 调用读取
`(buffer-name buffer)`；不能只替换 rename 之前 let* 的 RHS。reopen 在 reset/prompt
入口前也读取返回 object 的当前名称。首步仅证明名称稳定的同步调用段，不承诺
任意 advice、timer 重入、Render 内部 hook 改名/杀对象的安全性。初始化异常不
增加 automatic child-close/rollback，已污染历史 buffer 也不自动“修复”。

### 失效与交错语义

| 场景 | 应有行为 |
| --- | --- |
| 请求名称已被 foreign 占用 | 自动消歧 owned 名称；只写 owned |
| owned buffer 后续改名 | 保持 object 身份，不能跳到占用旧名称的对象 |
| buffer 已死亡 | 不由 Render 创建替身；沿原 caller 的丢弃/报告语义处理 |
| 两个 Run 共用 View | 保留 owner-keyed transcript，不因 current selection 混写 |
| Session close 被用户拒绝 | retained 与手动关闭保持现有公共合同 |

此表为整体目标。首步新增证明仅覆盖同步初始化与重建；rename/晚到 callback 的
完整 object 贯穿属于后续 Render 收口。已有并发、dead View no-retarget 和 close
回归必须继续保持，不能将它们写成本次新交付能力。

### 首步范围与验收

首批限定 `superchat.el`、`test/test-native-ui.el`；若为满足这一步必须改变 Render public
Interface 或跨多个异步 producer，停止扩大首步，单独安排后续收口。
先为真实 ensure 和真实 render-session-view 分别添加碰撞 red，证明 foreign
内容/marker/mode/modified 不变、owned prompt 可用、重建历史正确、返回 buffer 通过
公共 validation。只断言 owned prompt 正确不够：它可能正确而 foreign 仍被污染。
fixture 必须区分初始分配名、请求名、最终消歧名，并断言废弃初始名未被重新创建。
再跑 UI、Dispatcher/Render 相关 focused
和 canonical，保留原有 no-retarget、并发 transcript 测试。

## 5. 首批设计 B：Session 查询的结构与值分离

保持 `superchat-view-entries SESSION-ID &optional SINCE-ID KINDS LIMIT`。

1. SQL 结构只由代码生成：Session predicate、可选 cursor、N 个 kind 占位符、排序、
   现有 limit。
2. 绑定参数依 SQL 顺序排列，kind 字符串没有语法地位。
3. nil/空 kinds 继续不限制 kind，不增加 kind 注册表或收窄合法 material kind。
4. 返回格式不变，不借此迁移所有 DB 读者或取消 SQL 工具。

修复前 since-id 分支会重置 params；先完成 Session/cursor 参数，再追加 kinds，
或一次按 SQL 顺序构造。至少一例同时覆盖 cursor、两个 kinds、limit，并验证
正常 kind 实际命中，防止恒空结果的伪修复。

首批限定 `superchat-tape-view.el`、`test/test-tape-view.el`。
必须用真实临时 SQLite，覆盖 A/B Session、正常/引号/注释/多 kind、cursor、limit
及无 kind。P2 注入字符串不得扩大范围。现有 Anchor 来源完整性、预算披露和
append-only 测试继续通过。没有数据迁移和外部服务依赖。

## 6. 候选的最终处置与边界

| 候选 | 本稿决定 | 可进入实现的前置 |
| --- | --- | --- |
| Workflow 无效 authority 参数链 | 已完成（`E-20260905-004`） | 第七 ignored 槽、第八 Session 与真实 policy 来源保持 |
| 私有 `--file-path` | 保留；删除建议撤回 | `superchat-workflow-edit` 以它计算新 recipe 目标，已有文件仍走 resolver |
| Workflow edit | 已交付（`E-20260905-003`，自 K20） | 单目录、安全 identity、普通保存；不新增执行或 provider 行为 |
| Kernel 可选符号调用 | 逐项完成：两项修复，其余有界 no-change（`E-20260905-006`、`E-20260905-008`、`E-20260905-011`） | 新 seam 或第二 caller 出现时重新证明缺席/失败语义 |
| transport controls | 当前 producer 有界 no-change（`E-20260905-009`） | 第二 producer、新 async control 或真实 cross-Run leak 再触发 |
| Request 字段表 | 未实施 | 当前验证不支持新 schema/codegen；未知外部字段仍未知 |
| 统一 Tape/DB 命名结果 facade | 未实施，P5 有界 no-change（`E-20260905-010`） | 出现相同 reader 合同或测得重复负担再考虑 |
| `/remember` bridge | 已采用 future-only typed provenance（`E-20260905-012`） | 仅显式 recall/exact selected Anchor 过滤；无 backfill/text dedup/refill |
| replay cache / 批事务 | 未实施 | 真实规模测量、失效规则、崩溃一致性验证 |

这避免把整个系统的所有不确定性变成一个永远完不成的重构项目。

## 7. 为什么不采用更“统一”的模型

- **一个通用 Session context 管一切：拒绝。** Session 身份、Authority snapshot、
  transport controls 和 View markers 生命周期不同，合并会扩大所有调用者的知识面。
- **一个通用 close-on-finish：拒绝。** Skills operation child 与 accepted Subagent
  host 的寿命不同；Runtime quiescence 不等于 editor close。
- **所有读者都变成 Projection：拒绝。** Closure 是判断，Artifact registry 是资源
  活态，FTS 是检索；同用 Tape 不代表同一职责。
- **全系统依赖注入容器：拒绝。** 现有 callback 与有限 adapter seam 足以逐项演进；
  容器会新增命名、注册与生命周期责任。
- **换 Request struct 并拒绝未知字段：暂拒。** 现有外部 plist 扩展面未核清，首批
  修复不需要 breaking Request 迁移。

## 8. 兼容、恢复与验证

不改变 Tape schema、历史 Entry、公开 Runtime live-buffer admission、Session ID
fallback、Authority tighten-only 或已批准 close 确认/retained 机制。
不修改第三方私有调用、发布版本或外部服务配置。

首批两项代码改动分别可回退，无数据迁移。只回退自身精确改动，不能 reset 共享
工作区。后续若引入存储兼容迁移，必须另有版本、回放、恢复与失败路径设计。

| 验证层 | 证明什么 | 不证明什么 |
| --- | --- | --- |
| 隔离 Emacs / SQLite 反例 | 审计时 P1/P2 确实存在 | 所有入口、真实服务都受影响 |
| 新 red→green Interface 测试 | 精确修复与作用域保持 | GUI、性能、长期运行 |
| canonical + plugin standalone | 既有功能回归 | 未收录测试、所有外部消费者 |
| require / authority / citation gates | 指定结构与引用正确 | 完整语义正确、零 bug |
| 后续真实环境 canary | 声明环境中的交互行为 | 任意 provider 或部署矩阵 |

本节记录设计审计阶段的验证；后续实现验收见 §11。审计新增执行：P1 临时 buffer 碰撞复现、P2 临时 SQLite 查询复现。
R2 期间主审还直接提取第二篇附录的两个 Lisp 块执行：均 exit 0，分别重现 foreign
prompt 误写和 `normal=(A), injected=(A B)`。这是诊断通过，不是修复测试绿灯。
前序独立验收基线为 canonical 1220 total / 1203 expected / 0 unexpected / 17 skipped，
plugin 46 total / 45 expected / 0 unexpected / 1 skipped；并非本轮重新跑出的结果。

实施时的最小验证入口：

```sh
emacs -Q --batch -L . -l test/test-native-ui.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-tape-view.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
```

R3 期间主审实际执行前两项：Native UI 51/51 expected、Tape View 69/69 expected，
均 0 unexpected、exit 0。这是**未修复代码的既有基线**，不包含待新增碰撞/查询
回归；与两个反例同时成立，说明现有绿灯没有覆盖缺陷。canonical 本轮未重跑。
Render/no-retarget 回归主要在 `test/test-llm-backend.el`，例如
`test-render-late-stream-chunk-stays-in-owned-region`、
`test-dead-exact-view-never-retargets-by-name`、
`test-workflow-stream-sink-captures-owner-view-object`；Runtime 另有
`runtime/default-terminal-delivery-resets-response-region`。不假定存在独立 test-render 文件。

## 9. 指定模型的至少三轮审查

用户已明确指定“你现在这个模型”。审查者 `/root/architecture_review` 通过
`fork_turns=all` 继承主代理模型，无模型 override；复用同一个独立上下文依次审查。
不虚构不可见的模型版本号。三个既有 Herdr agent 仅做分区事实调查，不计入三轮，
没有为审查新增 Herdr pane。

| 轮次 | 输入与焦点 | 必须留下的结果 | 当前状态 |
| --- | --- | --- | --- |
| R1 | D0 + 三篇依据 + 实际源码；反驳事实、抽象和遗漏 | R1-01 Required：ensure-only 遗漏真实 reopen；纳入同步重建。R1-02/03：补复现入口和跨层例子 | D1 修订，R2 确认三项关闭 |
| R2 | D1 + R1 处置；生命周期、兼容、失败/回滚 | R2-01 Required：actual name 必须在 rename/mode 后紧邻调用采集；R2-02：明确 params 组装顺序和组合正向测试 | D2 修订，R3 确认两项关闭 |
| R3 | D2 + 全部未决项；模拟开发者按文档实现 | 无 Blocker/Required；R3-01 Suggestion：补验证命令与真实 Render 测试位置，已接受写入 §8 | 已完成；D3 仅补清单、结果与准入判定，不新增设计语义 |

每轮都要先审后改，再将修订稿交下一轮。不得预填 PASS，不能把同一报告改名三次。
至少三轮后，若首批仍有 Required/semantic 未决，继续审查，不由“轮数用完”放行。
审查记录集中追加在本文，不制造多份竞争的最终架构。

实际证据分工：R1 reviewer 独立运行 P2 与 ensure-only 修复仍污染 reopen 的探针；
R2/R3 reviewer 做源码、处置和实施路径核验，没有谎报重跑全套。主审的附录复现
与两组 focused 另见 §8。三轮发现全部接受，无未决拒绝项；审查不证明外部服务、
任意动态 Lisp 扩展或所有后续候选已经安全。

## 10. 开始代码的判定

设计门必须同时满足（首批 A/B 已满足）：

- 指定模型实际完成至少三轮，身份与每轮处置可核验。
- P1/P2 的代码证据、首步范围、保持合同、red/green、回退均已明确。
- 首批无未解决 blocker/Required/语义冲突；后续候选的未决项不偷渡进首批。
- 文档事实与建议分开，没有将历史校验数字写成本轮结果。

执行门单列：用户随后已明确授权 P1–P8，且每项按工作流实施或完成有界
no-change 验证。设计就绪、获准执行与验收结果仍是不同状态。

**D3 判定：P1–P8 均已按第二篇唯一状态表收口。** 其中有界 no-change 表示候选
经验证后不实施，不等于把全 Render/private Interface、统一 DB facade、Request
schema 或 controls 对象重构冒充完成。

## 11. 实施收尾（2026-09-05）

证据：[EVIDENCE.md](../../EVIDENCE.md) `E-20260905-002`。只修改两组实现与对应测试：
`superchat.el` 的同步 Render 调用、`superchat-tape-view.el` 的 kinds 参数绑定，
以及 `test/test-native-ui.el`、`test/test-tape-view.el`。

- A 两条真实碰撞回归保护 foreign 内容、mode、modified 和既有 Render marker
  对象/buffer/position/insertion-type，并验证 owned prompt、历史、公共 validation。
- B 真实 SQLite 矩阵保持 Session/cursor/kind/六列/ASC 合同；无 limit 三条与
  limit=2 两条明确可区分，不能靠恰好两条匹配制造假绿。
- 独立最终 focused：A 2/2、Native UI 53/53；B 1/1、Tape View 70/70，均无
  unexpected。canonical 复用 B 独立检查的 1223 total / 1206 expected /
  0 unexpected / 17 skipped，RUNNER_GATE_OK；它早于最终 limit fixture 补充，
  该纯测试增量由最终 focused 覆盖，不宣称最终全套重跑。
- learn 将 18 项机械引用漂移重锚，K18 → K19，保留 since 与既有语义。leader
  收尾亲自复跑 strict：979 refs / 0 errors / 0 warnings；authority form：ok。

没有新 registry、classifier、query framework、数据迁移或 Git/JJ 写操作。
原有上游 SPEC terminal-authority blocker 未改，本批短路径修复不依赖它的解决。

其后闭环映射为：P1 `E-20260905-002`、`E-20260905-007`；P2
`E-20260905-002`；P3 `E-20260905-006`、`E-20260905-008`、
`E-20260905-011` 与 [ADR 0005](../adr/0005-fail-closed-ordinary-llm-tool-admission.md)；P4 `E-20260905-009`；P5
`E-20260905-010`；P6 `E-20260905-012`；P7 `E-20260905-004`；P8
`E-20260905-005`。P6 只过滤 selected exact Anchor 对应的原 bridge Memory；raw
user 与 Anchor 始终保留，且没有历史 backfill、文本去重、refill、Anchor 递归或
Continuity representation。完整当前状态只维护在第二篇，不在本节复制状态表。
