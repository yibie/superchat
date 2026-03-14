# Plan: Resume Recovery

## Milestone 0: Phase Gate

- 固化 PR/FAQ、spec、plan、task 文档
- 锁定本阶段只做 Resume 恢复边界，不扩展其他系统

## Milestone 1: Three-Line Recovery Model

- 定义三条恢复槽位的稳定结构
- 为 `Build / Study / Research` 映射不同文案与语义
- 定义降级规则，避免内容不足时硬生成空话

## Milestone 2: Resolved Boundary

- 定义 `Resolved So Far` 的信息边界
- 只允许 settled 判断、已做决定、已排除方向、已确认事实进入
- 禁止历史 note 墙与资源墙进入默认 Resume 层

## Milestone 3: Product and UI Convergence

- 将 goal 元信息从主 Resume 降级
- 压缩默认态为一个极小恢复入口
- 验证不同 goal type 下都能快速进入工作状态


## Milestone 4: Thread Workbench Structure

- 将 thread 页面收敛为单栏 `Restart Note + Continue + Working Stream`
- 把 `Thread Memory` 保留在线程标题上方的容器级工具带
- 把 `Resources / Timeline` 收敛为右侧 inspector 的外露 dock tabs，而不是在主工作面重复放一组同名按钮
- 让右侧 inspector 变成按需展开的 secondary dock，收起时仍保留外露 tabs
- 让 inspector 内容更像 distill/workbench，而不是附件列表或历史墙

## Exit Criteria

- 默认 Resume 不再是一面信息墙
- 三种 goal type 都有稳定的三行恢复语义
- 用户可通过 stable memory 二级路径控制信息量
- thread 主页面优先把用户带回现场，而不是带进浏览模式
