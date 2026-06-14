# Goal: Emacs 原生融合

> 状态:**v1 落地完成 (2026-06-14)。** 里程碑候选:v1.2。
> 创建于 2026-06-15；首次实现在 2026-06-14。
>
> **已落地**:
> - 方向一 P1: `superchat-rewrite-region` + ediff (新文件 `superchat-rewrite.el`)
> - 方向三 capf: `#` 文件路径补全(四种 sigil 全覆盖)
> - 方向二 P1: `superchat-send-region` / `superchat-send-defun` / `superchat-send-region-or-defun`
> - 测试: `test/test-rewrite.el` (12 项 ERT,全部通过)
>
> **剩余方向**(未排期):
> - 方向二: project.el 集成、mode 感知上下文(flymake/eglot 诊断)
> - 方向三: transient.el 菜单、dired/embark 集成
> - 方向四: Magit/eglot/org-babel 深挂钩
> - 方向一扩展:多 region/矩形选区、回复代码块当 patch(一键 diff 应用)

## 问题陈述

superchat 当前是一个「住在 Emacs 里的独立聊天 app」——Emacs 是宿主,
不是上下文来源。它能读写 buffer/文件,但靠的是 LLM 的 tool-calling
(`EditBuffer` / `read_buffer` / `ReplaceBuffer`,见 `superchat-tools.el:487`),
这条路间接、不确定:每次都赌模型会不会正确调用工具。

「与 Emacs 更好地融合」= **让编辑器状态双向流动**:
Emacs 丰富的上下文(point、region、project、major-mode、诊断、版本控制)
自动流入 LLM;LLM 的输出流回**确定性的原生编辑动作**(可预览、可 `undo`)。

核心权衡:**确定性原生命令 vs. 万能 tool-calling**。
后者代码少、看着通用,但不可预测;前者代码多一点,但快、可预测、可撤销,
这才是 Emacs 用户期待的手感。本 goal 的方向整体偏向前者。

---

## 方向(按杠杆从高到低)

### 一、写回编辑器(最高杠杆,当前最弱)

- **确定性的 region 重写命令** `superchat-rewrite-region`
  - 选中代码 → 输入指令(如「重构这段」)→ 用 `ediff` / `smerge` 预览 diff → 一键应用。
  - 不走 LLM tool-calling,直接把 region 内容作为 prompt、把回复替换回原位。
  - 对标 gptel rewrite、ellama。这是体验差距最大的一块。
- **把回复里的代码块当 patch**:渲染成可一键(如 `C-c C-c`)应用的 diff,
  而非复制粘贴。

### 二、读取编辑状态(让上下文自动流入)

- **自动带上 region / defun / buffer**:`M-x` 命令直接把选区或
  `treesit` 的 enclosing function 作为上下文,免去手打 `#path`。
- **project.el 集成**:`search-text` / `find-files` 自动锁定当前 project 根
  (现状是 `default-directory`)。
- **mode 感知上下文**:注入 `flymake` / `eglot` 在 point 处的诊断、major-mode。
  一键把 `compilation` / `eshell` 报错 buffer 发去提问。

### 三、用 Emacs 原生 UI 替代自定义语法(可发现性)

- **capf 补全** `@model` / `>skill` / `/command` / `#file`,让 corfu/company
  原生补全。**已起步**(commit `4d1ca81` 启用 corfu/company),需做完。
- **transient.el 菜单**(magit 风格):把 @/>// /# sigil 语法做成可视菜单,
  新用户不必背语法。
- **dired / embark 集成**:dired 标记文件 → 发给 superchat 当上下文。

### 四、生态深挂钩

- **Magit**:从 staged diff 生成 commit message / 解释 diff。
  现有 `git-commit-message` workflow 示例是 prompt 层;原生 magit hook 更紧。
- **eglot / LSP 作为工具源**:把 code actions、references、hover 暴露成 tool,
  比 grep 准。
- **org-babel**:`#+begin_src superchat` 源码块,在文档里就地调用。

---

## 优先级建议

1. **方向一(region 重写 + ediff 预览)** —— 若只做一件事,做这个。
   它把 superchat 从「聊天框」变成「编辑器原生能力」。
2. **方向三的 capf** —— 已起步,继续做完,低风险高回报。
3. 方向二、四按需推进。

## 验收标准(草案)

- [x] `superchat-rewrite-region` 可对选区生成改写并经 `ediff` 预览后应用,全程不依赖 LLM tool-calling,结果可 `undo`。
  - 文件: `superchat-rewrite.el` (新增, 166 行)
  - `superchat-rewrite-region` 为 `;;;###autoload` 全局命令
  - 使用 `atomic-change-group` 保证原子撤销
  - `superchat-rewrite-confirmation` defcustom 支持 `ediff` / `auto` 两种模式
- [x] `@` / `>` / `/` / `#` 四种 sigil 在 corfu/company 下均可原生补全。
  - `@` / `/` / `>` 为已有实现；`#` 新增使用 `completion-file-name-table`
  - 修改: `superchat--completion-at-point` (superchat.el)
- [x] 至少一个「读取编辑状态」入口:一键将当前 region 或 defun 作为上下文发送。
  - 新增三个 `;;;###autoload` 命令:
    - `superchat-send-region` — 发送选区上下文
    - `superchat-send-defun` — 发送闭包 defun 上下文 (treesit → traditional 两层回退)
    - `superchat-send-region-or-defun` — 合体命令 (region 优先,回退到 defun)
  - 修改: `superchat.el` (+124 行,含 helper 函数)
- [x] 每项落地随 PR 扩展 ERT 覆盖(沿用 ROADMAP 的 cross-cutting 约定)。
  - 新增 `test/test-rewrite.el` (12 项测试,全部通过)
  - 测试覆盖: 代码块提取 (5) + defun 边界检测 (3) + `#` capf 补全 (3) + 上下文插入 (1)
  - 追加至 `test/run-tests.el`

## 开放问题

- region 重写如何处理多 region / 矩形选区?先只支持单连续 region?
  - **v1 决策**: 仅支持单连续 region(Emacs 标准 `use-region-p` 行为)。
- capf 触发时机:在 prompt 行首 sigil 后即触发,还是显式 `completion-at-point`?
  - **v1 落地**: corfu/company 自动补全 (corfu-auto-prefix=0 + company-capf)。
    同时也保留了 `superchat--insert-*-and-complete` 显式触发路径。
- transient 菜单与现有 sigil 语法共存,还是逐步替代?
  - **未落地**: 留待后续迭代。
- 写回动作的默认确认级别:总是走 ediff,还是小改动直接应用 + `undo` 兜底?
  - **v1 决策**: 默认 `ediff` 预览, 提供 `superchat-rewrite-confirmation` 切换为 `auto`。

## 来源

- 2026-06-15 与维护者的方向讨论(本对话)。
- 现状审计:`superchat-tools.el`(buffer/文件工具)、`superchat.el`(sigil 解析)、
  `README.md`、`ROADMAP.md`。
