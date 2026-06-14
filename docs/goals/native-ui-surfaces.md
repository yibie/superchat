# Goal: 原生 UI 入口(transient 菜单 + dired/embark)

> 状态:**已记录,未排期。** 创建于 2026-06-15。
> 隶属:`emacs-native-integration.md` 方向三的剩余部分(capf 四 sigil 补全已落地)。
> 里程碑候选:v1.2。

## 问题陈述

superchat 的能力靠 sigil 语法触发:`@model`、`>skill`、`/command`、`#file`。
keymap 把首字符绑到插入+补全命令(`superchat-mode-map`,`superchat.el:1577`:
`/`→`superchat--insert-slash-and-complete`,`@`→`--insert-at-and-complete`,
`#`→`superchat--smart-hash`)。capf 已能在输入后补全。

问题:**这些语法不可发现**。新用户不知道有哪些 sigil、每个能做什么;
老用户也要凭记忆敲。Emacs 解决可发现性的范式是 **transient 菜单**(magit 签名体验)
和 **dired/embark 动作分发**——superchat 目前都没接。

## 方案

### Part A — transient 命令面板

- 新增 `superchat-dispatch`(transient,绑到 `superchat-mode-map` 的 `C-c C-d`
  及全局可选入口),分组列出:
  - **模型**:切换 `@model`(读 `superchat--list-models`)。
  - **技能/命令**:`>skill` / `/command`(读已注册命令表)。
  - **上下文**:`#file`、send-region/defun、rewrite-region。
  - **会话**:save、reset、clear。
- transient 是 Emacs 28+ 内置,无新依赖。
- 纯增量:现有 sigil + capf 路径不动,菜单只是第二入口。

### Part B — dired / embark 集成

- `superchat-dired-send`:在 dired 中对**标记文件**(`dired-get-marked-files`)
  批量调用 `superchat--add-file-to-context`(`superchat.el:1330`),
  并切到聊天 buffer。绑到 dired 的 `C-c s`(可选,文档说明,不强制全局改键)。
- embark:为 `file` / `symbol` target 注册 `superchat-send-*` 动作,
  让 `embark-act` 能把任意 target 丢给 superchat。embark 为可选依赖,
  `with-eval-after-load 'embark` 守卫,缺失时静默跳过。

## 范围控制

无新硬依赖(transient/dired 内置;embark 软依赖守卫)。不动 sigil 解析、
不动 prompt 管道。估算 ~+120 行 + 测试。

## 验收标准(草案)

- [x] `M-x superchat-dispatch`(或 `C-c C-d`)打开 transient,覆盖模型/技能/上下文/会话四组,各项可正确触发对应命令。
  - 新增 `superchat-dispatch` interactive 命令
  - `superchat--dispatch-menu-transient` 为 transient 定义(eval-after-load 'transient)
  - `superchat--fallback-dispatch` 为 completing-read 回退(transient 不可用时)
  - `superchat-mode-map` 绑定 `C-c C-d` → `superchat-dispatch`
  - 四个 helper: `superchat--transient-switch-model`, `superchat--transient-pick-skill`, `superchat--transient-pick-command`
- [x] dired 中标记多个文件 → `superchat-dired-send` → 全部进入 superchat 上下文,聊天 buffer 前置显示。
  - 新增 `superchat-dired-send` (`;;;###autoload`)
  - `eval-after-load 'dired` 在 `dired-mode-map` 绑定 `C-c s`
  - 使用 `dired-get-marked-files` 批量收集 + `superchat--add-file-to-context`
- [x] embark 安装时,`embark-act` 在 file/symbol 上出现 superchat 动作;未安装时无报错、无副作用。
  - `eval-after-load 'embark` 注册 `superchat-embark-file-send` (file target, key `s`)
  - `eval-after-load 'embark` 注册 `superchat-embark-symbol-send` (symbol target, key `S`)
  - `declare-function` 声明;无 embark 时静默跳过
- [x] 测试覆盖 transient 定义可加载、dired 批量收集逻辑;接入 `run-tests.el`,全绿。
  - 新增 `test/test-native-ui.el` (10 项测试,全部通过)
  - 覆盖: dispatch 存在性 (4) + keymap binding (2) + dired-send (2) + embark 隔离 (2)
  - 追加至 `test/run-tests.el`

## 开放问题

- transient 入口键位:仅 buffer 内 `C-c C-d`,还是提供全局 autoload 命令?**v1 决策**: 两者都给,全局命令不绑键。
- dired-send 是否同时把文件名拼进 prompt,还是只加 context-files?**v1 决策**: 只加 context,prompt 留空待用户输入。
- embark 动作命名空间是否与现有命令冲突?**v1 已核对**: `s` / `S` 键在 `superchat-mode-map` 无冲突(已验证 ERT)。

## 来源

- 2026-06-15 与维护者的方向讨论(本对话)。
- 现状审计:`superchat.el:1577`(superchat-mode-map / sigil 键位)、
  `superchat.el:1249`(`--smart-hash`)、`:1285`/`:1290`(insert-and-complete)、
  `:1330`(`--add-file-to-context`)。
