# Pi(pi.dev)的自写扩展模型与信任围栏调查

> 补充报告,服务于 superchat `define_tool` 信任围栏设计。调查日期:2026-07-20。
> 主要一手来源:pi.dev 官网、GitHub 仓库 `earendil-works/pi`(原 `badlogic/pi-mono`)的
> `docs/extensions.md` / `docs/security.md` / README、Mario Zechner 2025-11-30 博文。

## 0. 身份确认

pi.dev 确为 Mario Zechner(badlogic,libGDX 作者)的极简 coding agent。仓库原为
`badlogic/pi-mono`,现已迁移/更名为 **`earendil-works/pi`**(npm scope 也从
`@mariozechner/*` 变为 `@earendil-works/pi-coding-agent`),pi.dev 是官网。核心自述:
四个内置工具(read / bash / edit / write)、约 1000 token 的系统提示词、无 sub-agent、
无 plan mode、默认无 MCP,"其余一切都是扩展"。

- 官网:<https://pi.dev/>
- 仓库:<https://github.com/earendil-works/pi>
- 作者设计博文:<https://mariozechner.at/posts/2025-11-30-pi-coding-agent/>

## 1. 扩展/工具模型:TypeScript 文件 + 热加载,agent 自写自用是官方卖点

**形态与位置**([extensions.md](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/extensions.md)):

- 扩展是 **TypeScript 模块**,导出 `default function (pi: ExtensionAPI)`。
- 自动发现目录:
  - `~/.pi/agent/extensions/*.ts`(全局,所有项目)
  - `.pi/extensions/*.ts`(项目本地)
  - 另可经 `settings.json` 引入 npm/git 包(`"packages": ["npm:@foo/bar@1.0.0", ...]`)。
- 扩展 API 极宽:`pi.registerTool()` 注册 LLM 可调用的工具、`pi.on("tool_call")` 拦截/
  改写/阻断工具调用、`pi.registerCommand()`、完整 TUI 组件、甚至 `before_provider_request`
  改写发给模型的原始 payload。

**热加载与"会话内自写工具"**:

- 自动发现目录中的扩展可用 `/reload` 热重载;`ctx.reload()` 让扩展程序化触发同一流程。
- 更进一步:**`pi.registerTool()` 在启动后任何时刻调用都立即生效**——文档原文:
  > "`pi.registerTool()` works both during extension load and after startup. … New tools
  > are refreshed immediately in the same session, so they appear in `pi.getAllTools()`
  > and are callable by the LLM without `/reload`."
- `extensions.md` 第一行即是:**"pi can create extensions. Ask it to build one for your
  use case."** ——agent 读自己的扩展文档、写 `.ts` 文件、`/reload`、测试、循环迭代,
  是官方推荐工作流。Armin Ronacher 的评述
  ([lucumr.pocoo.org/2026/1/31/pi/](https://lucumr.pocoo.org/2026/1/31/pi/))概括为:
  > "if you want the agent to do something that it doesn't do yet, you don't go and
  > download an extension or a skill … You ask the agent to extend itself."

即:pi 具备与 superchat `define_tool` + 热注入完全同构的能力,且把它当作核心卖点而非
需要围栏的风险面。

## 2. 信任围栏:逐调用批准 = 零;唯一内置门是"目录级一次性 project trust"

**默认零确认(YOLO)**。作者博文原话
([来源](https://mariozechner.at/posts/2025-11-30-pi-coding-agent/)):

> "pi runs in full YOLO mode and assumes you know what you're doing. It has unrestricted
> access to your filesystem and can execute any command without permission checks or
> safety rails. No permission prompts for file operations or commands."

仓库 README 同样明确:**"No permission popups. Run in a container, or build your own
confirmation flow with extensions inline with your environment and security requirements."**
([coding-agent README](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/README.md))

**确认门被降级为"可选扩展示例"**:permission gates(rm -rf / sudo 前确认)、path
protection(禁写 `.env`)在文档里是 *example use cases*,靠 `pi.on("tool_call")` 返回
`{ block: true }` 自行实现,官方不内置。

**唯一内置围栏:project trust**([security.md](https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/security.md)):

- 当项目目录含 `.pi/settings.json`、`.pi/extensions` 等资源且无已存决定时,交互式启动
  会问一次"Trust project?"。决定按**规范化目录路径**存入 `~/.pi/agent/trust.json`,
  父目录决定可覆盖子目录,之后**静默生效、不再询问**。
- 信任决定的粒度是目录,**不是文件内容**——没有任何 hash/签名/内容锚定。信任后该目录下
  扩展怎么改都照常加载。
- 官方对其定位极克制:**"Project trust is only an input-loading guard. It prevents a
  repository from silently changing pi's settings or extensions before you approve it.
  It does not make untrusted code, untrusted prompts, or untrusted model output safe."**
- 注意:**全局目录 `~/.pi/agent/extensions/` 不经任何 trust 流程**,放进去即加载。
  agent 自写扩展通常正是写到这里或已信任的项目目录,因此 agent 自写→持久化→再加载的
  全链路上没有任何人工审阅点(除非用户自己装 permission-gate 扩展)。

**无沙箱是明确立场**(security.md):

> "Pi does not include a built-in sandbox. … This is intentional. … A partial in-process
> sandbox would be easy to misunderstand as a security boundary … Real isolation needs
> to come from the operating system or a virtualization/container boundary."

安全边界声明也直白:prompt injection、用户自装扩展/技能的行为"generally outside the
security boundary"。

## 3. 持久化与再水化:纯文件系统,重启静默加载,无内容校验

- **扩展即磁盘上的 `.ts` 文件**,没有独立的"已批准工具注册表"。重启后,全局目录扩展
  无条件加载;项目目录扩展只查 `trust.json` 里的目录级决定,命中即静默加载。
- **无 hash、无签名、无"内容变更需重新批准"机制**。extensions.md 与 security.md 全文
  没有任何 content-hash/verification 概念;安全模型是一句话:
  > "**Security:** Extensions run with your full system permissions and can execute
  > arbitrary code. Only install from sources you trust."
- 会话级状态另有通道:`pi.appendEntry(customType, data)` 把扩展状态写进 session 文件
  (JSONL),`session_start` 时从 `ctx.sessionManager.getEntries()` 重建——这是"扩展的
  运行时状态"再水化,与扩展代码本身的加载无关。
- 第三方包(npm/git)装前的唯一指引也是人工的:"Review source code before installing
  third-party packages."(README)

## 4. 作者关于 approval fatigue / 安全的公开论述

博文 "YOLO by default" 一节是最完整的表态
([来源](https://mariozechner.at/posts/2025-11-30-pi-coding-agent/)):

> "If you look at the security measures in other coding agents, they're mostly security
> theater. As soon as your agent can write code and run code, it's pretty much game over.
> The only way you could prevent exfiltration of data would be to cut off all network
> access … which makes the agent mostly useless."

> "Since we cannot solve this trifecta of capabilities (read data, execute code, network
> access), pi just gives in. Everybody is running in YOLO mode anyways to get any
> productive work done, so why not make it the default and only option?"

立场拆解:

1. **逐调用批准被判定为剧场**:能写码+跑码的 agent 已越过任何 in-process 门,批准
   prompt 只制造 fatigue,不制造安全。
2. **责任外移到 OS 边界**:要安全就上 container/VM/micro-VM(containerization.md 给了
   三套模式:Plain Docker / Gondolin micro-VM / OpenShell 策略沙箱)。
3. **确认流是用户的自定义空间**:担心的人可以用 `tool_call` 拦截事件十行代码自建门——
   围栏是扩展生态的一员,不是 harness 的义务。

## 5. 与 superchat 的对比:可迁移与不可迁移

处境同构点:两者都是宿主进程内执行、无沙箱、支持 LLM 自写工具并持久化跨会话。

**pi 在批准粒度谱系上的落点**:`每次调用确认 ——— 首次定义确认 ——— 目录级一次确认 ———
零确认` 这条谱系上,pi 落在最右两格之间:全局扩展零确认,项目扩展目录级一次确认,
**且确认对象是"这个目录"而非"这段代码"**——比 superchat 候选设计(逐工具 pp 预览 +
body SHA-256 锚定 + 每次调用另有门)松了两到三档。

**可迁移的取舍**:

- *"in-process 门不是安全边界,只是意图确认"* 这一认知可直接迁移:superchat 的确认门
  应当被诚实定位为"防误操作/防静默漂移",而非防恶意——真正的对抗性防护(prompt
  injection 后的 exfiltration)pi 论证得很清楚,in-process 无解。
- *trust 决定要持久化、命中后静默* 与 superchat "hash 一致则静默加载"方向一致;pi 证明
  了"批准过的东西不再打扰"是可用性底线。
- *把更严的门做成可选层* 的架构思路(pi 的 permission-gate 扩展)对应 superchat 的
  "每次调用确认门可按工具配置"。

**不可迁移的取舍**:

- pi 的零围栏建立在**用户画像**上:终端里的专业开发者,默认理解"agent 在我机器上跑任意
  命令"意味着什么,且随手能 `docker run`。superchat 面向 Emacs 用户,elisp 工具体在
  **编辑器长驻进程**内执行,一个坏工具能改 init.el、挂 hook、动 `~/.authinfo`,爆炸半径
  和用户预期都不同——照搬 YOLO 不成立。
- pi 的目录级信任无内容锚定,意味着"信任后 agent(或任何写得动磁盘的东西)改扩展体,
  下次重启静默生效"。这正是 superchat 用 **body SHA-256 锚定**要堵的静默漂移窗口——
  pi 在这点上是**反例/空白**,不是先例:它没有解决这个问题,而是宣布不解决。
- pi 依赖"扩展是 .ts 文件,用户会去 review 源码"的开发者习惯;superchat 的 pp 预览确认
  把 review 内嵌进批准流,对非专业受众是必要的替代。

**一句话**:pi 为"批准过就别再烦我"提供了强支持,为"定义时即審阅"和"内容 hash 锚定"
提供的不是先例而是缺位——它显式放弃了这两层,并把理由(security theater / trifecta
无解)写成了宣言。superchat 候选设计相当于在 pi 的诚实认知(in-process 无法防恶意)
之上,补上 pi 因用户群不同而不需要、superchat 因用户群不同而必需的两道意图确认门。

## 来源清单

- pi.dev 官网:<https://pi.dev/>
- 扩展文档:<https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/extensions.md>
- 安全文档:<https://github.com/earendil-works/pi/blob/main/packages/coding-agent/docs/security.md>
- coding-agent README(project trust、/trust、No permission popups):<https://github.com/earendil-works/pi/blob/main/packages/coding-agent/README.md>
- Mario Zechner,"What I learned building an opinionated and minimal coding agent"(2025-11-30):<https://mariozechner.at/posts/2025-11-30-pi-coding-agent/>
- Armin Ronacher,"Pi: The Minimal Agent Within OpenClaw"(2026-01-31):<https://lucumr.pocoo.org/2026/1/31/pi/>
- npm 包页:<https://www.npmjs.com/package/@mariozechner/pi-coding-agent>
