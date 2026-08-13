# 交互层路线图：把"运行中也能说话"补上

基线：`/tmp/claude-interaction-review.md`（P0-P8 缺口）、
`/tmp/claude-craft-interaction-review.md`（想法裁决 + Craft 对照）、
`docs/design/architecture-positioning.md`（Emacs 的 AI OS 层定位）。

## 已决策

1. **textui 替换 vui.el，立即执行。** vui 只用于两处（genui 白名单、
   agents 面板）；textui 的 plist 数据模型让生成的 UI **不需要 eval**——
   白名单从"允许的函数符号"变成"允许的 `:type` 关键字 + 属性白名单"，
   安全模型升级；Flex/Grid 布局 + 具名区域替换是后续内联 genui 的底座。
2. **控件 skill 按 textui 语法写**（不是 vui 版）——textui 既已定为运行时，
   Claude 审查中"写两遍"的顺序冲突按另一条支路解决：直接写 textui 版。
3. 机器只排序、标注、选择；判定权在人；自动层默认关闭（既有纪律不变）。

## 十个想法的裁决（Claude 审查结论）

| 想法 | 裁决 | 关键条件 |
|------|------|---------|
| 控件 skill | 可行（高性价比） | 按 textui 语法写 |
| 控制通道（P0） | 可行，零依赖，解锁面最大 | Runtime 加公开列举函数 |
| Tape 审计浏览器 | 可行，数据全就绪 | tabulated-list 三层视图 |
| 审批 diff 侧窗 | 可行，**不能用 ediff** | diff-mode buffer + unified diff |
| 进度与通知面 | 通知可行；进度有条件 | "步骤"维度依赖计划对象 |
| 计划对象视图 | 有条件 | 新协议：emit_plan 工具 + Tape 事件 |
| textui 替换 vui | **已决策执行** | agents 面板低风险；genui 重写 validator/白名单/action 展开 |
| 后台任务面板 | 有条件 | 三个数据源缺两个（主 Run、workflow 状态要新建） |
| 暂停态 | 可行但语义受限 | 只能停在工具轮次边界 |
| 生成式 UI 内联 | 有条件 | 硬依赖 textui 区域替换 |

## Craft 该学 / 不该学

**该学（按优先级）**：① 权限三档 + y/n/a（superchat 零件全有：只读工具集
= Explore、现状 = Ask、confirm-destructive nil = Execute；dangerous 判据用
effects 分类，并修掉 destructive-tools 手工名单与 effects 表的漂移）；
② acceptance criteria（Closure 接得住）；③ 会话是有状态对象（org TODO +
org-agenda）；④ Steer/Queue 显式区分；⑤ 回合 diff 回看。

**不该学**：浏览器自动化、IM 集成（与定位冲突）；自动 repair loop
（学判据不学自动重跑）；kanban 拖拽；公网分享；iframe/PDF 渲染。

## 六阶段路线

| 阶段 | 交付 | 依赖 | 对应 |
|------|------|------|------|
| 0 | **textui 替换 vui**（agents 面板迁 textui + genui 重写 + 控件 skill） | 无 | 未来想法 1+2 |
| 1 | 控制通道（Run 中可写 + `C-c C-k` 取消 + queue 重定向） | 无 | 形态 1 |
| 2 | 审批可判断（diff 预览 + y/n/a + 三档模式） | 弱依赖 1 | 形态 4 |
| 3 | 后台可见性（通知 + 会话/Tape 浏览器合并入口） | 无 | 形态 5½+6 |
| 4 | 计划对象（org 子树 + acceptance criteria + emit_plan） | 1、3 | 形态 3 |
| 5 | 工具边界控制（steer + paused 同一检查点） | 1、2 | 形态 8 |
| 6 | 统一任务面板 + genui 内联 | 0、1、5 | 形态 2+7 |

## 三处合并

1. Tape 浏览器与会话状态对象 = 同一个会话列表入口
2. 暂停与 steer = 同一个工具轮次边界检查点
3. 审批 diff 与回合 diff = 同一个 diff 生成函数

## 关键代码锚点（Claude 核对）

- `superchat-runtime.el:893` cancel 已实现且竞态安全；公开 API 只有 6 个
  defun，需新增 active-runs 列举
- `superchat-dispatcher.el:424-429` Run 期间 prompt 置 nil 的三行
- `superchat-authority.el:255` `register-approval-preview` 公开注册接口
- `superchat-authority.el:482-524` `--effective-decision` 多输入合成 cond
- `superchat-tools.el:143-150` effects 分类（process-exec / local-write）
- `superchat-agent-loop.el:48-53` destructive-tools 名单与 effects 表漂移
- `superchat-genui.el:22-24` vui 白名单（textui 迁移的主战场）
