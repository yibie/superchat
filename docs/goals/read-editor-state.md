# Goal: 读取编辑状态(project.el + 诊断注入)

> 状态:**v1 落地完成 (2026-06-14)。** 里程碑候选:v1.2。
> 创建于 2026-06-15；实现在 2026-06-14。
> 隶属:`emacs-native-integration.md` 方向二的剩余部分。
>
> **已落地**:
> - Part A: `superchat-tools-default-directory` + `superchat-tools--base-dir` (3 工具函数已更新)
> - Part B: `superchat--project-root`, `superchat--region-diagnostics`, `superchat--file-relative-to-project`,
>   `superchat-send-include-diagnostics` defcustom; send-region/defun 上下文头部含 project 根 + 相对路径
> - Part C: `test/test-context.el` (11 项 ERT,全部通过)

## 问题陈述

`superchat-send-region` / `superchat-send-defun`(`superchat.el:1448`)已经能把
选区/defun 代码块 + major-mode 插进 prompt,但:

- **只带 buffer 名**,没带项目路径、没带诊断 —— LLM 看不到这段代码在项目里的位置,
  也看不到编译器/linter 已经报出的错。
- 工具 `search-text` / `find-files` / `list-files`(`superchat-tools.el`)默认用
  **聊天 buffer 的 `default-directory`**,而聊天 buffer 不在用户项目里,
  所以工具搜索范围是错的。

这正是 goal「让编辑器上下文自动流入 LLM」尚未兑现的一半:代码进来了,
但**项目坐标系和诊断信号没进来**。

关键事实:eglot 的诊断通过 flymake 后端上报,因此 `flymake-diagnostics`
一条路径即可同时覆盖 flymake + eglot,无需分别处理。

## 方案

### Part A — 工具按 project 根定位(`superchat-tools.el`)

- 新增 defvar `superchat-tools-default-directory`(nil)。
- 新增 helper `superchat-tools--base-dir` → `(or superchat-tools-default-directory default-directory)`。
- `list-files` / `search-text` / `find-files` 的 `default-directory` 默认值换成
  `(superchat-tools--base-dir)`。纯增量;未设置时行为不变。

### Part B — 上下文富化(`superchat.el`)

- 新增 `superchat--project-root`:`project-current` → 根目录,失败回 nil。
- `send-region` / `send-defun` 调用时:
  - 解析源 buffer 的 project 根,`setq superchat-tools-default-directory`,
    让后续工具搜索锁定到正确项目。
  - context 头部从 `[Context from buffer X]` 升级为带 **project 根 + 相对路径**。
  - 新增 `superchat--region-diagnostics`:`(flymake-diagnostics beg end)` 收集区间内
    诊断(自动含 eglot),格式化成 `line:col severity message`,附在代码块后。
- 新增 defcustom `superchat-send-include-diagnostics`(默认 `t`)开关诊断注入。

### Part C — 测试(`test/test-context.el`,新文件)

- `superchat-tools--base-dir` 回退逻辑(有/无 `superchat-tools-default-directory`)。
- `superchat--project-root` 在非项目目录返回 nil。
- `superchat--region-diagnostics` 用合成 flymake 诊断验证格式化。
- 追加到 `test/run-tests.el`。

## 范围控制

不碰 prompt-hook 管道、不动 MCP、不引入新依赖(project.el / flymake 均内置)。
估算 ~+90 行实现 + 测试。

## 验收标准(草案)

- [x] `search-text` / `find-files` / `list-files` 在 send-region 之后默认搜索源 buffer 的 project 根,而非聊天 buffer 的 `default-directory`;无项目时回退原行为。
  - 新增 `superchat-tools-default-directory` defvar + `superchat-tools--base-dir` helper
  - 三个工具函数 (`list-files`, `search-text`, `find-files`) 默认路径改为 `(superchat-tools--base-dir)`
  - send-region/send-defun 调用时 setq `superchat-tools-default-directory` 为源 project 根
- [x] send-region/defun 的 context 头部包含 project 根 + 文件相对路径。
  - 新增 `superchat--project-root` (封装 `project-current`)
  - 新增 `superchat--file-relative-to-project` (文件路径相对于 project 根)
  - send-region/send-defun 的 context 头部从 `;; from buffer: X` 升级为 `;; project: ... 
;; file: ...`
- [x] 当源 buffer 启用 flymake/eglot 且区间内有诊断时,诊断以 `line:col severity message` 形式附在代码块后;`superchat-send-include-diagnostics` 为 nil 时不注入。
  - 新增 `superchat--region-diagnostics` (收集 + 格式化 flymake 诊断,自动覆盖 eglot)
  - 新增 defcustom `superchat-send-include-diagnostics` (默认 `t`)
  - 诊断为空时不插入空块
- [x] 新增 `test/test-context.el`,覆盖 base-dir 回退、project-root 非项目返回 nil、诊断格式化,接入 `run-tests.el`,全绿。
  - 11 项测试全部通过: base-dir (3) + project-root (1) + file-relative (1) + diagnostics (3) + 工具集成冒烟 (3)

## 开放问题

- 诊断默认开还是关?**v1 决策**: 默认 `t`。区间无诊断时静默(不插空块)。
- context 头部相对路径以 project 根为基准,还是以 `default-directory`?**v1 决策**: 以 project 根为基准。
- 是否需要在工具搜索范围切换时给用户可见提示(echo 一行 project 根)?**v1 决策**: 暂不做,保持静默。
- `superchat-tools-default-directory` 的生命周期:**v1 决策**: 持续,直到下一次 send-* 覆盖。

## 来源

- 2026-06-15 与维护者的方向讨论(本对话)。
- 现状审计:`superchat.el:1448`(send-region/defun + context 插入)、
  `superchat-tools.el`(search-text/find-files/list-files 的 `default-directory` 默认)、
  `superchat-prompt-hooks.el`(prompt 管道,本 goal 不触及)、
  `superchat-core.el`(turn 结构)。
