# 计划面板渲染丑的真正原因：三处具体代码问题

> 写给 Pi。上一份 [`plan-view-polish-consult.md`](plan-view-polish-consult.md) 里
> "混合派"的设计意图（头部识别 + 步骤体无框 + 聚光框）是对的，已经照方案实现
> （`c9bae8a`）。但实际渲染很丑（见用户截图：一个几乎空的巨大方框，方框下面
> 接一条对不齐的短横线）。问题不在设计方向，在于**实现时没有用 TextUI 真正
> 提供的机制**。以下三点都直接读自 `~/Documents/emacs/package/textui` 源码，
> 不是猜测。

## 根因 1：头部被套进 `:border t` 的盒子里，一行字撑出一个空盒子

`superchat-plan.el:203-211`（review 帧）和 `:244-261`（progress 帧）把标题包成：

```elisp
(list :type :flex :direction :column :border t :padding 1
      :children (list (list :type :text :value "Plan: ...")))
```

`textui.el:860-888`（`textui--render-layout-box`）的实际行为：`:padding` 是标量，
上下左右都加，且不受方向区分（没有"只加水平"这种选项，README `### Flex` 一节
明确说 `:padding` 是全向的）。对只有一行文字的容器套 `:padding 1` + `:border t`，
渲染出的是「顶边框 + 空白行 + 文字行 + 空白行 + 底边框」共 5 行，而这个容器又
没给 `:layout :width`，会撑到父容器（0.42 宽侧窗）的整个可用宽度——所以看到
的是一个很宽、几乎全是空白的方盒子，只有中间一行字。这不是 bug，是**这个元素
用错了地方**：`:border t` 该用在"信息本身撑得起一个盒子"的地方，不该用在一行
标题上。

**修：头部不要 `:border t`，就是纯 `:text`。**

## 根因 2：紧跟着的分隔线是写死的 24 个字符，宽度对不上盒子

`superchat-plan.el:212`：

```elisp
(list :type :text :value (make-string 24 ?─))
```

不管面板实际多宽，这条线永远是 24 格。当上面的盒子撑满整个侧窗宽度（可能
四五十格），下面这条线只有 24 格——两条线对不齐，视觉上就是"断的"。更糟的
是：盒子自己的下边框（`└────┘`）已经是一条封底线了，紧接着再插一条不等宽的
`─────`，等于同一个位置画了两条不同宽度的分隔线，纯冗余。

**根子在于：渲染函数根本没用 TextUI 给的宽度。** `superchat-plan.el:299` ：

```elisp
(defun superchat-plan--view-frame (&optional _width)
```

`_width` 用下划线打头直接扔掉了。但 README「A first interface」一节的模型是：

```text
state + available width -> render function -> widget.el buffer
```

`width` 就是 TextUI 在每次渲染时告诉你"你现在实际有多宽"的唯一入口。仓库自带
的三个原型（`examples/textui-btop-prototype.el:157-190`、
`textui-k9s-local-refresh-prototype.el:199-207`、
`textui-lazygit-prototype.el:120-142`）全都是这么画标题线的——不用 `:border`
元素，而是拿到 `width` 后手工拼一条正好等宽、标题嵌在线里的横线，例如
btop 原型：

```elisp
(defun textui-btop-prototype--title-line (width key title right face)
  (let* ((inside (max 0 (- width 2)))
         (left (format "─%s%s" key title))
         (line (concat "╭" left (make-string (- inside ...) ?─) right "╮")))
    ...))
```

**修：把 `width` 一路传下去，分隔线按 `width` 现算，而不是写死常量。**

```elisp
(defun superchat-plan--header-rule (width)
  "A single divider exactly as wide as the panel currently is."
  (make-string (max 1 width) ?─))

(defun superchat-plan--review-frame (proposed width)
  ...
  (list (list :type :text :value (format "Plan: %s" ...))
        (list :type :text :value (superchat-plan--header-rule width))
        ...))
```

调用侧两处都要把 `width` 真正递进去（现在两处都在扔掉它）：

```elisp
;; superchat-plan--view-frame 也要接住 width 并往下传
(defun superchat-plan--view-frame (&optional width)
  ...
  (cond
   (proposed (superchat-plan--review-frame proposed width))
   (active (superchat-plan--progress-frame active width))
   ...))

;; 首次打开：lambda 参数不能再叫 _width
(textui-open superchat-plan--view-buffer
             (lambda (width) (superchat-plan--view-frame width)))
```

这样头部就变成：标题一行 + 一条**正好撑满面板宽度**的横线，干净，且和面板
宽度变化（用户拖动窗口）自动同步——TextUI 本身在宽度变化时会自动重渲染
（README「Point and resizing behavior」：*"TextUI refreshes when the usable
width changes"*），只是现在的代码没利用这个数字。

聚光框（progress 帧里 in-progress 那一步的 `:border t`，`superchat-plan.el:283-287`）
不用动——它包的是两行真实内容（状态行 + accept 行），盒子不空，视觉分量是
挣来的，保留。

## 根因 3（次要，不影响这次视觉问题，但值得顺手改）：控件绕开了 TextUI 的 `:action` 约定

`superchat-plan.el:144-171` 给 checkbox 和 push-button 都手写了 `:notify`，
点击后自己手动调 `textui-request-refresh`。但 TextUI 的原生控件约定是
`:action`（README「A first interface」示例；`textui.el:1173-1183` 会包一层
`:action`，自动做"这次点击后该不该重渲染"的判重和调度，而不是每次都强制
刷新一遍）。现在的写法能跑，只是没吃到这层去重——步骤多、连点几下会比用
`:action` 多刷几次。不紧急，顺手对齐即可，不是这次"丑"的原因。

另外 `superchat-plan--view-update`（`superchat-plan.el:342-351`）里用
`textui-update` 存了一份"渲染好的帧"进 `textui-state`，但注册的渲染函数根本
不读 `textui-state`（每次都直接从 buffer-local 的 `superchat-plan--proposed`
现算）——这个 `textui-state` 写入是死代码，直接改成 `(textui-request-refresh
buffer)` 更诚实，效果一样。

## 优先级

1. **P0（这次截图的丑就是它）**：头部去掉 `:border t`，分隔线改成用 `width`
   现算，不再写死 24。
2. **P1（顺手）**：`superchat-plan--view-frame` 及两个 `--frame` 函数真正接住
   `width` 参数，两处 `textui-open`/更新回调把它传进去。
3. **P2（不紧急）**：`:notify` → `:action`；`superchat-plan--view-update` 里
   `textui-update` 的死状态写入换成直接 `textui-request-refresh`。

跟上一版"混合派"设计比：结论没变（头部 = 身份，无框；步骤体 = 无框；聚光框
= 唯一的框），只是这次头部的"身份感"不再靠一个盒子撑，而是靠一条贴合面板
实际宽度的横线撑——更轻，且不会再和下面的东西宽度对不上。

顺带一提（不是 bug，供知情）：截图里 checkbox 显示成图形方块 ☑ 而不是文本
`[x]`，是 GUI Emacs 下 widget.el checkbox 的正常图形化表现；纯终端（TUI）里
会退化成文本 `[X]`/`[ ]`，两边都能用，不用管。
