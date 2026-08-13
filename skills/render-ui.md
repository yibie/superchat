---
name: render-ui
description: 生成式界面语法：TextUI 白名单元素、布局模式与受信动作
version: "1.0"
type: prompt
---

# Render UI Skill

你可以在 superchat 里通过 `render_ui` 工具渲染界面。界面是**数据，不是代码**：
TextUI 解释你的 s 表达式，绝不求值。写错只会得到验证错误字符串，可以重试。

## 元素语法（白名单，只有这四种）

```elisp
;; 1. 文本（宽度感知自动换行）
(:type :text :value "一行文本")

;; 2. 线性布局（:row 横向 / :column 纵向）
(:type :flex :direction :row
       :children ((:type :text :value "左")
                  (:type :text :value "右")))

;; 3. 网格（等宽列，最小列宽决定列数）
(:type :grid :columns 2 :min-column-width 20
       :children ((:type :text :value "a")
                  (:type :text :value "b")))

;; 4. 按钮（唯一可交互元素；动作必须来自受信注册表）
(:type button :label "取消" :action (action cancel-agent :id "42"))
```

## 规则

1. **每种元素只能出现列出的属性**。:text 只有 :value；:flex 有
   :direction 和 :children；:grid 有 :columns、:min-column-width、
   :children；button 有 :label 和 :action。
2. **:children 是元素的列表**，可以是任意嵌套的四元素组合。
3. **动作只能有两个**（其他会被拒绝）：
   - `(action send-to-chat :text "完整提示词")` —— 把文本发进聊天输入区
     （限 8000 字符；输入区已有内容时不会覆盖）
   - `(action cancel-agent :id "agent-id")` —— 取消指定子代理
4. **不要用 `define_tool` 画界面**，不要在按钮里发明新动作。
5. 单个 s 表达式为宜；多个顶层形式会被自动包进纵向布局并提示。

## 布局模式（照着用）

**选项列表**（纵向 flex，每行一个按钮）：

```elisp
(:type :flex :direction :column
       :children ((:type button :label "方案 A"
                         :action (action send-to-chat :text "选 A"))
                  (:type button :label "方案 B"
                         :action (action send-to-chat :text "选 B"))))
```

**标题 + 网格**：

```elisp
(:type :flex :direction :column
       :children ((:type :text :value "当前状态")
                  (:type :grid :columns 2 :min-column-width 24
                         :children ((:type :text :value "运行中: 3")
                                    (:type :text :value "完成: 12")))))
```

**带解释的确认行**：

```elisp
(:type :flex :direction :column
       :children ((:type :text :value "检测到 3 个冲突，是否逐一处理？")
                  (:type :flex :direction :row
                         :children ((:type button :label "逐一处理"
                                           :action (action send-to-chat
                                                           :text "逐一处理冲突"))
                                    (:type button :label "跳过"
                                           :action (action send-to-chat
                                                           :text "跳过冲突"))))))
```

## 何时用

- 需要用户在几个选项中选择（用按钮，不用提问文本）
- 展示结构化结果表（用 :grid）
- 提供可重复执行的动作入口（用按钮 + send-to-chat）

## 何时不用

- 只要文字回答的，就正常回答，不要渲染界面
- 需要表格排序/过滤/编辑的，交给普通文本或 Emacs 原生工具，界面只做轻交互
