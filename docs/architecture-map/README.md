# Superchat 架构地图试点

这是供开发讨论使用的历史工作区快照，不是完整依赖分析器或架构正确性证明。
当前行为以代码和测试为准；模块分工见[架构说明](../architecture.md)。

- [执行与呈现职责图](runtime.html)：先定位负责模块，再追踪交接。
- [模型与工具输出时序图](output.html)：普通工具成功路径，共用 Render 但分开权限与呈现。
- 可编辑源：[职责 JSON](runtime.architecture.json)、[时序 JSON](output.sequence.json)。只编辑源，HTML 由 Archify 生成。

## 阅读边界

箭头表示逻辑调用/交接，**不是 `require` 边**。Kernel 不 require 中层；可选 UI 函数交接不改变这一点。
颜色沿用 Archify 类型，只帮助区分职责；“backend/database”不表示部署服务，Tape 也不等于物理 DB。
本图省略 Provider、Core prepare、Extension 细节、DB、Projection 及辅助调用；没有连线不等于没有关系。
Authority 也写 Tape；Closure 接收 trace 纯解释，不自行加载 Tape。

时序图仅展示默认呈现、普通非 evidence-contract 工具路径。真实 async 返回可能延后；
evidence-contract preflight、错误、取消与恢复分支不能从该主线推断。
呈现键是 `:transcript-owner`，并非强制 Run ID；Workflow 可共享 owner。
自定义回调仍有动态 owner 兼容；nil legacy 不承诺 Run 隔离。

## 源码索引

以下文件链接配稳定 symbol 定位，不把易漂移行号当作身份。
对应的读取时字节哈希见 [source-snapshot.json](source-snapshot.json)。这是本地工作区证据，
没有启用 Archify 的 public-commit `sources` 验证，也没有用 HEAD 冒充未提交修改。
哈希只能发现源码变化，不能自动证明图的语义；变化后应重查相关函数和测试。

| 图中的关系 | 文件与真实 seam |
| --- | --- |
| Request 身份固定、EW 验证、Runtime 提交 | [Dispatcher](../../superchat-dispatcher.el)：`superchat--submit-chat-request` |
| buffer 归属与轮换 | [EditorWorkspace](../../superchat-editor-workspace.el)：`superchat-editor-workspace-validate-buffer`、`superchat-editor-workspace-rollover-session-view` |
| Run / owner 发布、模型启动、默认流与最终交付 | [Runtime](../../superchat-runtime.el)：`superchat-runtime-submit`、`superchat-runtime--deliver`、`superchat-runtime--finish` |
| owner 的公开读取 | [Runtime](../../superchat-runtime.el)：`superchat-runtime-status` |
| 创建 wrapper、模型轮次结束显式 close | [LLM](../../superchat-llm.el)：`superchat--llm-generate-answer` |
| 呈现资格、调用与结果呈现、Authority 交接 | [Agent Loop](../../superchat-agent-loop.el)：`superchat--agent-wrap-function`、`superchat--agent-render-tool-call`、`superchat--agent-render-tool-result` |
| 权限、实际执行与工具结果审计 | [Authority](../../superchat-authority.el)：`superchat-tool-execute` |
| 文本区域、显式 owner、final 预格式化 | [Render](../../superchat-render.el)：`superchat--response-region-begin`、`superchat--response-region-close`、`superchat--response-region-insert-transcript` |
| 持久事实 / 纯判断 | [Tape](../../superchat-tape.el)：`superchat-tape-append-event-record`；[Closure](../../superchat-closure.el)：`superchat-closure-judge` |

## 当前已交付与待检查工作

不新增任务系统，不由连线自动推断风险。讨论一个改动时，只补充：
**改变哪条交接 → 唯一负责模块 → 必须保持的相邻行为 → 真实回归测试**。
拟议变化与当前图分开，独立 check 后才更新当前图。

| 已有工作 | 从图看落点 | 不能越过的边界 |
| --- | --- | --- |
| Slice01 clear / output，E001；reasoning 独立 E002 | EW 轮换及 Render / Agent Loop 显式呈现 | opaque owner 不以 Run ID 替代；nil legacy 仍有界 |
| Slice02 测试卫生，E003 | 测试 fixture / cleanup；生产拓扑不变 | 不因修 mock 改 Runtime 或弱化断言 |
| Slice03 Org Babel，E004 | 图外同步 adapter，入口借用 exact buffer | 不改共享 Skill/Preset 权威 |
| Slice04 Subagent / Closure，E005 | Closure 有限七个观察事件；Subagent 生产未改 | 观察不是完成证明；不宣称所有父 Session 路径相同 |
| Slice05 late tool admission，E006 | Authority 最早及首条审计前接纳检查 | terminal-first 零新增执行审计；已接纳取消仍可 audit-incomplete |
| 独立 refusal delivery，E007 / K32 | public Authority 局部拒绝交付 once | 原 consumer error/quit 传播；成功 async swallowing 不变 |
| O1 deadline acquisition，E008 / K33 | Runtime `--arm-deadline` 精确 timer 资源获取 | inactive exact timer 获取与 eq-only rollback；不增加终态、身份或全局 timer 清扫 |

上表的 Slice、E 和 K 编号只用于识别历史修复批次，不是当前任务或发布流程。
旧流程台账已经移除；源码快照仍保留当时的产品文件哈希，不代表当前工作树。
S1 已获本批有界 no-change 授权（discarded isolated test entry，无 physical leak），
fixture 未改，不声称全部测试状态都已清理。S2 保持 Babel literal concatenation no-change。
相关已有测试入口：`output-context/`、`output-p2/`、`output-prompt/`（cmd/native/agent 测试文件）；
以及 Runtime、Authority、Closure 的直接套件。本批 fresh 测试见临时交接报告
`/private/tmp/framework-remaining-do/WORKER_REPORT.md`；旧阶段结果仍按原 Evidence 归属。

## 复现与验证

使用 [Archify](https://github.com/tt-a1i/archify) 固定提交
`c6519401f7b91b9d43011657880893b0a8955548`，包版本 `2.17.0-dev.1`。
工具下载在 `/private/tmp/superchat-archify.o8KkGf`；无全局安装、npm install、自动更新或仓库工具代码拷贝。
本批复用该本地目录并核对包版本；固定提交归属沿用原下载记录，目录无 `.git`，未联网重新解析提交。
临时目录不是永久依赖；失效后需取同一固定版本，不能静默换 main。

从项目根目录运行（`ARCHIFY_DIR` 指向已核实工具目录中的 `archify`）：

```sh
export ARCHIFY_UPDATE_CHECK_DISABLED=1
node "$ARCHIFY_DIR/bin/archify.mjs" validate architecture docs/architecture-map/runtime.architecture.json --quality showcase --json
node "$ARCHIFY_DIR/bin/archify.mjs" deliver architecture docs/architecture-map/runtime.architecture.json docs/architecture-map/runtime.html --quality showcase --json
node "$ARCHIFY_DIR/bin/archify.mjs" validate sequence docs/architecture-map/output.sequence.json --quality showcase --json
node "$ARCHIFY_DIR/bin/archify.mjs" deliver sequence docs/architecture-map/output.sequence.json docs/architecture-map/output.html --quality showcase --json
```

交付摘要：[职责图回执](runtime.delivery.json)、[时序图回执](output.delivery.json)，均为 showcase 9/9、零错误/警告。
浏览器检查为独立无头 Chrome，不连接用户日常浏览器或 Emacs GUI：
[职责图浏览器回执](runtime.visual-check.json)、[时序图浏览器回执](output.visual-check.json)。
四种桌面尺寸的溢出检查与两种主题截图，和代码正确性/真实 provider 验证是不同的证据。
首次时序布局曾因纵向溢出失败；调整纵向间距后重新生成并检查，不隐藏溢出、不缩小字体。

职责图 JSON/HTML 未改，其 browser evidence 和截图查看结论是历史验证。
本批时序图修正已过时的 Slice05 卡片及成功审计标签，重新 deliver 至临时路径后逐字发布；
`output.delivery.json` 保留实际临时输出路径，仓库 HTML 与该产物字节相等。
新时序图四尺寸 browser evidence 为 pass，并实际查看 1440×900 浅色与
2048×1320 深色截图：主要节点、顺序、连线和卡片无裁切；小字号细节适合用 viewer 放大阅读。
这是有界截图检查，不是搜索/导出等全部交互验收，也不是独立语义审查。
