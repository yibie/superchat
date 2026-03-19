# Task List: Electron Clean-Room Rebuild

task001 [x] 场景: 冻结旧 Electron 结构为废弃资产 | Given: 用户已明确要求现有 Electron 模块全部废弃 | When: 在文档中确认 clean-room rebuild 以 Swift 参考为唯一输入并停止引用旧结构作为契约 | Then: 后续实现不会继续围绕旧 Electron 修补 | 验证: 文档检查
task002 [x] 场景: 重建 domain model 契约 | Given: Swift `Models.swift` 与相关参考文档定义了核心产品语义 | When: 重新实现 thread/entry/claim/anchor/task/threadState/resource/memory 等领域模型 | Then: 新 Electron 拥有独立且完整的领域语义基础 | 验证: 单元测试
task003 [x] 场景: 重建 capture intelligence 层 | Given: 输入语义先于 routing | When: 实现 capture interpretation、object extraction、candidate claims、reference parsing | Then: raw capture 可稳定转换成领域输入 | 验证: 单元测试
task004 [x] 场景: 重建 routing engine | Given: thread 分流不能退化成全文检索 | When: 实现 thread signature、suggest/decide/debug 与阈值保护 | Then: inbox 安全策略和 auto-route 边界被保留 | 验证: 单元测试
task005 [x] 场景: 重建 discourse heuristics | Given: relation 推断是 thread 语义增强的一部分 | When: 实现 heuristic relation helpers 与 candidate pair inference | Then: thread 内关系增强具备 deterministic 基础 | 验证: 单元测试
task006 [x] 场景: 重建 resource derivation | Given: resources 是派生视图不是第二套容器 | When: 实现 link/media/mention 分类和 counts 聚合 | Then: 全局与 thread 内资源视图具备统一基础 | 验证: 单元测试
task007 [x] 场景: 重建 SQLite schema | Given: persistence 是本地优先产品基础 | When: 定义 core entities + retrieval + memory + ai snapshots 表结构与 migration | Then: 新数据库边界与 Swift 产品语义一致 | 验证: 命令执行+文档检查
task008 [x] 场景: 重建 row mappers 与 persistence store | Given: domain model 不应直接绑定数据库字段 | When: 实现 row mappers、CRUD 和 snapshot 读写 | Then: 持久化层成为唯一 SQLite 入口 | 验证: 单元测试
task009 [x] 场景: 重建 repository 边界 | Given: UI 与 persistence 之间需要增量仓储层 | When: 实现异步串行写入、retrieval access、memory access 与 optional vector setup | Then: renderer 不直接拼装数据库写入 | 验证: 单元测试
task010 [x] 场景: 重建 workspace 与 attachment contract | Given: workspace 是 Threadnote 的存储边界 | When: 实现 workspace create/open/restore 和 SHA-256 attachment copy | Then: 数据与附件保持 workspace-local 且可搬迁 | 验证: 手动验证+单元测试
task011 [x] 场景: 重建 retrieval 层 | Given: restart/route/prepare 需要确定性召回支撑 | When: 实现 retrieval documents、FTS recall、thread ranking 和 tokenizer | Then: retrieval 成为支撑层而非 intake 主引擎 | 验证: 单元测试
task012 [x] 场景: 重建 memory pipeline | Given: restart 依赖 working/episodic/semantic/source memory | When: 实现 memory scopes 写入与读取 | Then: thread memory 具备 inspectable system record 基础 | 验证: 单元测试
task013 [x] 场景: 重建 AI contracts | Given: AI runtime 不能直接由 SDK API 定义 | When: 重新实现 route/resume/prepare/discourse request-response contracts | Then: domain 与 AI runtime 边界清晰 | 验证: 单元测试
task014 [x] 场景: 基于 Vercel AI SDK 重建 AI provider runtime | Given: 新版本 AI 接入依赖 provider abstraction | When: 实现 provider registry、config store、ping 与 model handle factory | Then: local/cloud provider 都能通过统一 runtime 接入 | 验证: 单元测试
task015 [x] 场景: 重建 AI request queue | Given: route/resume/prepare/discourse 会竞争资源 | When: 实现带优先级与取消语义的 request queue | Then: AI runtime 拥有稳定的并发控制边界 | 验证: 单元测试
task016 [x] 场景: 重建 route/resume/prepare/discourse AI services | Given: Threadnote AI 是四层内部能力而不是聊天页 | When: 实现 planner services 并绑定 deterministic snapshot 输入 | Then: AI enhancement 可被 application runtime 安全调用 | 验证: 单元测试
task017 [x] 场景: 重建 desktop shell | Given: 应用必须有 main window、quick capture window 与 menu/shortcut | When: 实现 app startup、window factory、capture shortcut 与 secure bridge | Then: 桌面壳层可独立运行并承载后续 renderer | 验证: 手动验证
task018 [x] 场景: 重建 application runtime use cases | Given: renderer 不应直接调用 repository/AI runtime | When: 实现 workspace、capture、thread、resource、settings 等 app services | Then: UI 层获得稳定的 use-case contract | 验证: 单元测试
task019 [x] 场景: 重建 workbench shell | Given: Swift 主界面是 Sidebar/Canvas/Sidecar 三栏结构 | When: 实现 shell layout、surface router、panel/modal controller | Then: 新 Electron 拥有正确的信息架构骨架 | 验证: 手动验证
task020 [x] 场景: 重建 capture editor runtime | Given: 输入系统是高风险产品核心 | When: 实现 token-aware editor、syntax highlight、cursor overlay、relation picker、file drop、IME-safe sync | Then: 输入语言体验与 Swift 参考结构一致 | 验证: 手动验证+测试
task021 [x] 场景: 重建 Stream 页面 | Given: Stream 是主 capture surface | When: 实现 stream composer、date-grouped timeline 和 inbox 流 | Then: 用户可在不选 thread 时持续录入内容 | 验证: 手动验证
task022 [x] 场景: 重建 Thread 页面 | Given: Thread 页面必须优先服务继续工作 | When: 实现 thread header、prepare section、continue composer、working stream | Then: thread workbench 回到 continue-first 路径 | 验证: 手动验证
task023 [x] 场景: 重建 Thread Sidecar | Given: Restart Note 与 thread resources 属于二级信息 | When: 实现 sidecar tabs、restart note blocks、AI state、thread resources panel | Then: 二级信息不再抢占主工作面 | 验证: 手动验证
task024 [x] 场景: 重建 Resources 页面与 Source Detail | Given: resources/source 是派生工作面的一部分 | When: 实现 global resources、thread resources、source detail modal 和 open-thread/open-source 流程 | Then: links/media/mentions/source 行为完整恢复 | 验证: 手动验证
task025 [x] 场景: 重建 timeline entry component family | Given: entry card 承载 reply、route、source、edit、delete 等复杂交互 | When: 实现 entry card、reply composer、route state、hover actions、relation badge、source open | Then: timeline 交互闭环恢复 | 验证: 手动验证
task026 [x] 场景: 重建 AI settings 与 provider config 页面 | Given: 本地和云端 provider 都是一等后端入口 | When: 实现 settings panel、save/test connection、conditional fields | Then: AI runtime 配置具备可见入口 | 验证: 手动验证
task027 [x] 场景: 重建 rich body renderer 与 metadata 服务 | Given: source/resource 可读性依赖 rich preview | When: 实现 link/media/document/audio/video/local file 预览与 metadata enrichment | Then: source 与 attachment 不再退化成文本路径 | 验证: 手动验证
task028 [x] 场景: 补齐 clean-room 核心测试 | Given: 重建过程无法依赖旧 Electron 回归套件作为真实保障 | When: 为 domain、routing、persistence、repository、AI runtime、editor token logic 添加测试 | Then: 新结构具备独立回归保护 | 验证: 命令执行
task029 [ ] 场景: 执行 clean-room 手动验收 | Given: 桌面工作流、编辑体验和 rich preview 需要真实交互验证 | When: 按 workspace/capture/route/thread/resources/settings 全路径执行手动检查 | Then: 新 Electron 是否达到可工作状态有明确证据 | 验证: 手动验证清单
task030 [x] 场景: 建立 clean-room 源码入口骨架 | Given: 用户同意先冻结旧代码，再建立新结构入口 | When: 在 `src/` 下新增 desktop/application/domain/infrastructure/renderer-clean/ui-system 骨架，并明确旧树只读 | Then: 后续实现有清晰的新落点，且不会再把新功能写回旧目录 | 验证: 文档检查
task031 [x] 场景: 清理旧 Electron 冻结源码树 | Given: clean-room runtime、测试入口和 renderer 已切到新路径 | When: 删除 `src/main`、`src/shared`、`src/renderer` 以及对应旧测试和 parity smoke | Then: 仓库不再保留会误导实现边界的 legacy 运行代码 | 验证: 命令检查
task032 [x] 场景: 重做 React renderer 视觉系统 | Given: 新 renderer 已能运行但视觉语言仍与旧版过于相似 | When: 更换全局 design tokens、布局气质、按钮/卡片/编辑器/预览样式 | Then: 主界面在信息架构不变前提下与旧版形成明确视觉断层 | 验证: 构建+手动启动
task033 [x] 场景: 将 React renderer 改成 flat content-forward 工作台 | Given: 当前 UI 仍保留杂志化渐变、厚阴影和大圆角语言 | When: 将 `renderer/src/styles.css` 与 `renderer/src/ui/App.jsx` 切到白底、系统 sans、细边框、时间线圆点与轻量 hero 结构 | Then: Stream/Thread/Resources/Settings/Quick Capture 全部回到内容优先的扁平桌面知识应用风格 | 验证: `npm run build:renderer`
task034 [x] 场景: 让 Electron 开发态支持真正的前端热刷新 | Given: 当前 `npm run dev` 只是先 build renderer 再 loadFile 打开静态产物 | When: 新增 Vite dev server + Electron 联调脚本，并让窗口在开发态改用 `loadURL()` 指向 dev server 页面 | Then: 修改 `renderer/src/*` 后无需重建即可通过 HMR 或 `Cmd+R` 看到最新样式与脚本 | 验证: `npm run dev`
task035 [x] 场景: Thread 页面结构向 Swift 版 thread document 靠拢 | Given: 当前 `ThreadSurface` 仍是单栏标题+时间线+底部固定输入，且 inspector 入口与线程二级工具未形成稳定分工 | When: 将主区重排为标题摘要/Continue/Working Stream，收起底部固定 editor，并把 inspector 收敛为 `Restart Note | Resources` 同时增加 `Thread Memory` 与 `Thread Tools` 顶部 chrome | Then: 主路径聚焦继续工作，Restart Note 保持在 inspector 中而不回流主面板，线程页面信息层级与 Swift 版更接近 | 验证: `npm run build:renderer` + `npm test`
task036 [x] 场景: 用户编辑已发送 entry card 时不再被旧草稿覆盖 | Given: entry card 进入编辑态后父组件会因时间戳或高亮等状态变化重渲染 + 无附件 entry 默认传入新的空数组 | When: 编辑器仅在进入编辑态时注入一次初始草稿而不是每次重渲染都 replace | Then: 用户输入不会回弹到原值且 inline edit 可稳定保存 | 验证: `npm run test:renderer -- threadNavigation`
