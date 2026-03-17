# Audit: Thread Navigation Instability

## Summary
- 这次问题不是单个点击入口绑错，而是 renderer 曾经同时把 `selectedThreadID`、共享 `workbench.thread`、以及晚到的异步 `open-thread` 结果都当成“当前 thread”来源。
- 只要用户快速点 A -> B，或者 `open-thread` 叠加后台 AI refresh，旧结果就可能把新选择覆盖掉，表现为 thread 串线、引用跳错、inspector 内容对不上。
- 本轮收敛后的正式模型是：
  - 导航真源只有 `selectedThreadID`
  - 详情真源只有 `threadDetailsByID[threadID]`
  - 晚到结果必须同时满足 `requestedThreadID` 和 token 校验

## Failure Matrix
| 入口/链路 | 旧失败模式 | 当前保护 |
| --- | --- | --- |
| Sidebar / ThreadBadge | 点击 thread B 后仍显示 thread A 的旧详情 | renderer 只按 `selectedThreadID` 读 `getThreadDetail(threadID)` |
| 引用 token / backlink | 从当前 surface 或旧缓存猜 thread，导致跳到错误 thread | `focusEntry(entryID, { threadID })` 直接使用目标 entry 自身的 threadID |
| `app:open-thread` | 打开 thread 时等待 AI refresh，放大串线窗口 | IPC 立即返回 deterministic detail，AI refresh 后台事件推送 |
| 后台 `thread-updated` | 晚到刷新可能覆盖当前展示 thread | payload 必带 `threadID`，renderer 仅按 key merge |
| 重复打开同一 thread | 第一次旧请求晚到后覆盖第二次新请求 | `openThread` 为每个 thread 维护 request token，旧 token 结果丢弃 |

## Renderer State Contract
- `useNavigation`
  - `selectedThreadID` 只表达“当前选中哪个 thread”
  - `focusEntry` 若携带 `threadID`，只负责切换导航，不直接写 detail
- `useWorkbench`
  - `threadDetailsByID` 是唯一 thread detail cache
  - `threadLoadingByID` 只按 threadID 跟踪加载态
  - `thread` 保留为兼容空值，不能再作为真实状态源
- `ThreadSurface` / `ThreadInspector`
  - 只能消费 `getThreadDetail(selectedThreadID)`
  - 允许 `null -> loading -> ready` 三态，不允许回退到其他 thread 的旧 detail

## Main Process Contract
- `app:open-thread(threadID)`
  - 立即返回 `appService.openThread(threadID)` 的 deterministic 结果
  - 不再等待 `openThreadWithAI(threadID)`
- `app:thread-updated`
  - 仅作为后台 AI refresh 结果通道
  - payload 必须同时包含 `threadID` 和 `thread`
  - renderer 若发现 `payload.thread.thread.id !== payload.threadID`，直接丢弃

## Test Coverage Added
- clean-room
  - `threadDetailState` 覆盖按 key upsert、stale token 丢弃、错配 payload 丢弃
- renderer
  - `ThreadBadge` 打开正确 thread
  - `Sidebar` thread row 打开正确 thread
  - 引用 token 使用目标 entry 的 `targetThreadID`
  - backlink 使用 source entry 的 `sourceThreadID`
  - thread A -> B 切换时不再显示 A 的旧内容
  - `thread-updated` 错配 payload 不得写入 cache

## Remaining Risks
- 这次收敛的是 thread navigation 主链路，不是整个 renderer 状态管理的终局重构。
- 如果后续新增入口绕过 `openThread` / `focusEntry` 直接写 surface 状态，串线问题会重新出现。
- 如果 mutation IPC 返回值继续夹带“顺手覆盖当前 thread”的隐式语义，也会重新污染 renderer；新入口必须继续遵守“只按 `thread.id` merge cache”的规则。
