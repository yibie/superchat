# 求教：Emacs 计划审批面板怎么做才好看

> 给 Claude 的界面设计咨询。目标：让下面这个"能用但素"的面板变好看，
> 不换技术栈（TextUI 元素 + widget.el 控件），不改交互语义。

## 背景（30 秒）

superchat 是 Emacs 里的 AI 聊天客户端。当模型提出一个计划（propose plan）
时，右侧弹出一个 42% 宽的侧窗面板，全生命周期都在这一个窗口里：

1. **审查帧**：勾选步骤 + 一个 Approve 按钮（widget.el checkbox/push-button，
   可点击）
2. **批准后同窗切为进度帧**：每步显示 `[✓]` / `[→]` / `[ ]`，由模型的
   `report_plan_progress` 工具调用驱动刷新
3. 清除计划后关窗

## 当前长相（真实渲染文本）

```
** Plan: 读取并恢复 superchat-demo-note.txt 第二行
为指定文本，验证结果
3/3 steps selected
[x] 1. 读取 superchat-demo-note.txt 文件内容
     done when: 文件内容包含三行，第二行是"这行会被模型改写。"
[ ] 2. 将 superchat-demo-note.txt 第二行恢复为"这行会被模型改写。"
     done when: 文件第二行内容为"这行会被模型改写。"
[ ] 3. 验证修改结果
     done when: 文件内容与预期一致
< Approve selected steps >
```

进度帧同样素：

```
** Plan: xxx — approved
2/3 steps done
[✓] 1. ...
[→] 2. ...
[ ] 3. ...
```

注意：`**` 是字面量（TextUI 的 `:text` 不渲染 markdown），这是现状之一。

## 可用的设计词汇（硬约束，来自 TextUI 引擎）

元素类型（`(list :type X ...)`）：

- `:text` — 纯文本，**没有颜色/字面/对齐选项**，只能 `:value` 一个字符串
- `:flex` — 行/列容器，支持 `:gap`、`:padding`、`:border t`
- `:grid` — 网格，`:columns`、`:min-column-width`、`:gap`、`:padding`、`:border`
- `:image` — 图像块（行×列像素？），无图像时渲染 `:alt` 文本
- 原生控件（widget.el 自定义 widget 类，通过 `:textui-attach` 接入）：
  checkbox、push-button、文本字段等。**控件的脸由 widget.el 的 face 决定**
  （`widget-button-face` 等），Emacs 主题可定制

其他约束：

- 窗口在右侧，宽 0.42，高度动态（面板内容撑开）
- 终端（TUI）里也要能用；纯 Emacs，无 CSS/图片资源
- 刷新 = 整帧重渲染（或 region 局部刷新），无状态式增量 UI
- 内容动态变化：步骤数不定（1~N 步）、文本长、会自动换行
- 同一个窗口两帧切换（审查→进度），视觉要有连续性
- 交互：勾选 checkbox、点按钮；进度帧无交互（纯展示）

## 请教的问题

1. **层级**：这个面板的信息层级该怎么排？现在标题、计数、步骤平铺，没有
   视觉重心。用 border box / 分组 / 间距能做出什么层级感？
2. **状态符号**：`[✓] [→] [ ]` 够不够？在无颜色可用的约束下，还有什么
   让"当前正在做哪步"一眼可见的办法（缩进？分隔线？`→` 行加框？）？
3. **验收标准**：`done when:` 现在每步跟一行，字一多就占半屏。有没有更
   省空间又保留信息的方式（折叠、两列 grid、步骤行内截断+详情区）？
4. **按钮与计数**：Approve 按钮和 "n/n selected" 的位置、大小；进度帧的
   "n/n done" 要不要一个迷你进度条（字符画）？
5. **两帧的连续性**：审查帧 → 进度帧切换时，视觉上哪些元素该保持不变
   （标题？步骤列表布局？），哪些该变？
6. **空状态**：无计划时的 "No plan in this session." 该怎么呈现？

## 交付物期望

不用写完整代码。给：布局草图（ASCII 或 mermaid 均可）+ 用上述元素词汇
描述的帧结构要点 + 每处改动的理由（最多三五处，别过度设计）。最好给
两三个方向对比（比如"极简间距派" vs "盒子卡片派"），说明各自的取舍。
