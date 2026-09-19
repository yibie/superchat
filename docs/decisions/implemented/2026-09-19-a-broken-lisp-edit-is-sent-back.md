# Decision: 把 Lisp Buffer 改坏的那次修改，由工具打回

Status: implemented（2026-09-19；同日 accepted——用户裁定："这是模型能力问题"；"出现这类问题时，打回重试，并把出错信息带给 llm 是正确的方法"；随后在两个问题上选定：由**工具自己**打回、被打回的修改**不留**在 Buffer 里）

## 一句话

结构化的 Buffer 修改工具在动手之前先自查：如果一个 Lisp Buffer 本来读得通、这次修改之后会读不通（括号或引号不配对），这次调用就失败，Buffer 原样不动，工具结果里写明哪里坏了；模型在同一个 Run 里看到并重新提交。

## 为什么

有人值守的验收 `m7-attended-chat-02`（2026-09-19）里，模型按 M7 的路径正确地 `open_file` → `edit_buffer`，但替换文本多带了一个右括号：

```
old_string:  …#'ignore)
new_string:  …#'describe-function))
```

它之后用单独的 `define-key` 让绑定当场生效，从未求值过自己改的 Buffer，工具只回了一句"已替换，未保存未求值"，没有任何东西告诉它改坏了。用户审阅后保存，新 Emacs 读 init 时报 `Invalid read syntax`。撤销时多余的括号被原样保留。

归因是模型能力问题。处理办法不是换权限、不是让人盯得更紧，而是让**能机械发现的错误在工具这一层就被打回**，把出错信息交还给模型——与 M5"工具结果要给模型可用的反馈"是同一类事。

## 契约

1. **范围**：`edit_buffer`、`append_to_buffer`、`replace_buffer` 三个结构化修改工具；目标 Buffer 的 major mode 派生自 `lisp-data-mode`（`emacs-lisp-mode`、`lisp-interaction-mode`、`lisp-mode` 等）。其他 Buffer 没有同样客观又便宜的检查，不在本决定内，行为不变。
2. **判据**：整个 Buffer（不受 narrowing 影响）能否被 Emacs 的 sexp 扫描完整走完——和 `check-parens` 同一个判据，注释和字符串里的括号不算。不求值、不加载、不运行 mode hook。
3. **打回**：修改前读得通、修改后读不通 → 调用失败。
   - **Buffer 原样不动**，字面意义上的：检查在一份私有副本上预演，真正的 Buffer 从未被改过——文本、modified 标记、undo 记录、change hook、锁文件都和没调用过一样。
   - 这次调用**不算一次修改**：不写入 `:buffer`／`:buffer-file`，不会出现在 `/changes` 和 Run 结束提醒里。
   - 工具结果以 `Error:` 开头（沿用这几个工具已有的失败写法，会被现有的工具失败反馈识别），写明：没有修改；哪一种不配对（多出的右括号／没有闭合的左括号或引号）；在预演结果里的行号、列号和那一行的原文；修改前是读得通的；请改正后重新提交。措辞里不出现 `refused`／`denied`／`declined`／`cancelled`／`blocked`——那些词会被现有反馈判为"用户或策略拒绝、不要重试"，而这里恰恰要它重试。
   - 工具不替模型修，不猜意图。
4. **本来就坏的 Buffer**：修改前就读不通 → 无法归因于这次修改，也可能正是在修它，所以照常修改；结果后面加一行事实：现在读得通了，或仍然读不通、位置在哪。
5. **读得通 → 读得通**：结果文字与今天完全相同，不多一个字。
6. **已接受的代价**（用户选定）：需要两次调用才配平的修改会在第一次被打回；模型必须一次改对。
7. **不改的**：权限档位、批准、`/changes` 推导、`open_file`／`read_file`、`eval_elisp`。`eval_elisp` 改 Buffer 不经过这项检查——它是完整控制，本来就不在结构化工具的保证之内。

## 验收

新回归 `scripts/edit-check-regression.el`：

1. `m7-attended-chat-02` 的那一次修改，原样重放 → 打回；结果点名第 4 行；Buffer 文本、modified 标记、undo 记录不变，change hook 没有运行，没有锁文件，没有记录修改目标。
2. 正确的那一次修改 → 通过，结果文字与今天逐字相同。
3. 打回的结果被现有反馈识别为工具失败，且**不是**"不可重试"。
4. 本来就坏的 Buffer：修好它的修改照常生效并说明"现在读得通"；没修好的照常生效并说明位置。
5. 非 Lisp Buffer 不检查；注释和字符串里的括号不算；narrowing 之内的修改按整个 Buffer 判断，narrowing 不变。
6. `append_to_buffer`、`replace_buffer` 同样打回。
7. 经真实执行管线：脚本模型先提交坏修改、再提交好修改 → 第二次请求里带着出错信息；最终 Buffer 是好内容；Tape 上只有第二次调用带 `:buffer`；`/changes` 只有一条。

1、3、4（说明行）、6、7 在实现前必须是红的且红在这个缺口上；2、5 是守护。其余套件、canonical smoke、Kernel 检查不变；如有基线变化逐项列出。

之后重开 chat 入口的有人值守验收（新目录）。

## 交付记录（2026-09-19）

- 实现（`superchat-tools.el`）：`superchat-tools--checked-edit` 把修改先在一份私有副本上预演——副本带着目标 Buffer 的全文、语法表、`parse-sexp-lookup-properties`／`syntax-propertize-function` 和同样的 narrowing，不运行任何 mode 函数或 hook；用 `scan-sexps` 走完整个文本（`check-parens` 的同一个判据）。三个工具的 `:perform` 都经过它；被打回时不调用 `superchat-tools--write-evidence`，所以不记录修改目标。真正的 Buffer 只在预演通过（或本来就读不通）之后才被改。
- 打回时模型读到的原文（`m7-attended-chat-02` 那一次调用重放）：`Error: edit not applied; buffer init.el was not changed. It reads as Lisp now, and after this edit it would not: a closing parenthesis with nothing to close at line 4, column 71: (define-key emacs-lisp-mode-map (kbd "C-c t h") #'describe-function))). Correct the text you are inserting so that parentheses and strings balance, then call again.`
- 回归 `scripts/edit-check-regression.el`，7 个测试，对应验收 1–7。实现前 6 红 1 绿：红的全部红在"工具替换了文本、什么也没说"上；"正确的修改逐字不变"这一条前后都绿。
- 变异体（各自让对应测试变红）：从不打回（原行为）；在真 Buffer 上预演；只检查 narrowing 之内；不分 mode 一律检查；措辞里用 `denied`（会被现有反馈判成"不要重试"）；被打回的调用仍记录修改目标。其中"一律检查"第一轮没被咬住——我的非 Lisp 例子改之前就不配对——补强测试后变红。
- 端到端（验收脚本，脚本模型，零模型调用，`runs/m7-calibrate-02-edit-check`）：新场景 `sent-back-then-correct` 先提交真实运行里那次坏修改、再提交正确修改——两个入口都 `complete=true`，重启检查通过；`correct` 与 `source-damage` 两个旧场景结果不变。
- 其余全部套件、canonical smoke（无基线变化）、Kernel 检查、加载通过；编译警告与基线一致。
