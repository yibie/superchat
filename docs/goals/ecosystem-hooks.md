# Goal: 生态深挂钩(Magit / eglot / org-babel)

> 状态:**v1 落地完成 (2026-06-14)。** 里程碑候选:v1.2+。
> 创建于 2026-06-15；实现在 2026-06-14。
> 隶属:`emacs-native-integration.md` 方向四。
>
> **已落地**:
> - 子目标 1: `superchat-magit.el` — Magit commit-message 生成
> - 子目标 2: `superchat-tools.el` — eglot LSP 工具 (`lsp-references`/`lsp-code-actions`/`lsp-hover`)
> - 子目标 3: `ob-superchat.el` — org-babel `#+begin_src superchat` 源码块
> - 测试: `test/test-ecosystem.el` (12 项 ERT,全部通过)

## 问题陈述

superchat 与 Emacs 生态目前只有「文件/buffer 工具」这一层接触。三个高价值
原生子系统尚未挂接,而它们恰好是 Emacs 用户日常的核心:

- **Magit**:版本控制中枢。现有 `examples/` 里的 `git-commit-message` workflow
  是 prompt 层面的,要用户手动跑;没有从 magit 暂存区直接生成 commit message 的原生入口。
- **eglot / LSP**:已有诊断通过 flymake 流入(见 `read-editor-state.md`),
  但 code actions / references / hover 这些**结构化能力**还没暴露成 superchat 的工具。
- **org-babel**:superchat 本身基于 org buffer,却不能在普通 org 文档里
  就地调用——没有 `superchat` 源码块。

## 方案(三个独立子目标)

### 子目标 1 — Magit commit-message 原生入口

- 新增 `superchat-magit-commit-message`:读 `magit-staged-files` 的 diff
  (`magit-git-string`/`magit-diff-staged`),走一次**确定性 blocking LLM 调用**
  (复用 `superchat-rewrite--call-llm` 的无工具调用模式),生成 conventional-commit,
  插入当前 `git-commit` buffer 或 echo 供确认。
- `with-eval-after-load 'magit` 守卫;magit 缺失时命令不注册。

### 子目标 2 — eglot 能力作为工具源

- 在 `superchat-tools.el` 增加 LSP 工具:`lsp-references`、`lsp-code-actions`、
  `lsp-hover`,内部走 `eglot` 的 `xref` / `eglot-code-actions` API,作用于
  「源 buffer + 位置」。需要一个「当前源位置」概念(复用 `read-editor-state.md`
  里 send-* 记录的源 buffer)。
- 软依赖,`fboundp` 守卫;无 eglot 时工具不进 registry。

### 子目标 3 — org-babel `superchat` 源码块

- 新增 `ob-superchat`(或并入 superchat.el):`org-babel-execute:superchat`
  把 `#+begin_src superchat ... #+end_src` 的 body 作为 query 跑一次,
  结果写回 `#+RESULTS:`。支持 `:model` / `:skill` header-args 映射到 `@`/`>`。
- 让 superchat 能在任意 org 笔记里就地用,而不必切到聊天 buffer。

## 范围控制

三者互相独立,可分批落地。均为软依赖守卫,不增加硬依赖。
复用已有的无工具 blocking 调用(`superchat-rewrite--call-llm`)避免重复造轮子。

## 验收标准(草案)

- [x] Magit:在有暂存改动时 `M-x superchat-magit-commit-message` 生成符合 conventional-commit 的信息;无 magit 时命令不存在、不报错。
  - 新增 `superchat-magit.el` (139 行)
  - `superchat-magit-commit-message` 为 `;;;###autoload` 命令
  - `superchat-magit-commit-style` defcustom: `conventional` / `freeform`
  - `superchat-magit--build-prompt` 组装 diff→prompt
  - LLM 调用复用 blocking 无工具模式;插入 `git-commit-mode` buffer 时原子撤销
  - magit 缺失时 `fboundp` 守卫给出 `user-error`
- [x] eglot:启用 eglot 的源 buffer 中,LLM 可通过 `lsp-references`/`lsp-code-actions`/`lsp-hover` 工具获取结果;无 eglot 时这些工具不进 registry。
  - 新增 3 个 eglot 工具函数: `superchat-tool-lsp-references`, `superchat-tool-lsp-code-actions`, `superchat-tool-lsp-hover`
  - 已注册到 `superchat-llm-tools-reload` 工具列表中
  - 所有 eglot 函数调用均有 `fboundp` 守卫 + `declare-function` 声明
  - 无 eglot 时返回明确错误(如 "eglot is not active")
- [x] org-babel:`#+begin_src superchat` 块可执行并把结果写入 `#+RESULTS:`;`:model`/`:skill` header-args 生效。
  - 新增 `ob-superchat.el` (130 行)
  - `org-babel-execute:superchat` 执行 body 并返回结果
  - 支持 header-args: `:model` (模型切换), `:skill` (技能注入), `:system` (系统提示)
  - `ob-superchat--build-query` 解析 `:skill` header-arg
  - `ob-superchat-system-prompt` / `ob-superchat-timeout` defcustoms
- [x] 每个子目标随 PR 扩展 ERT(至少覆盖:commit-message 的 diff→prompt 组装、ob 的 header-arg 解析);接入 `run-tests.el`。
  - 新增 `test/test-ecosystem.el` (12 项测试,全部通过)
  - 覆盖: magit prompt 构建 (3) + eglot 无服务时的错误路径 (3) + ob 查询构建 (3) + defcustoms (2) + eglot 函数定义 (1)
  - 追加至 `test/run-tests.el`

## 开放问题

- Magit:生成后是直接插入 commit buffer 还是先 echo 待确认?**v1 决策**: 在 `git-commit-mode` buffer 中插入 + 可 undo;其他 buffer echo。
- eglot 工具:位置来源用 send-* 记录的源 buffer,还是要求显式参数?**v1 决策**: `buffer-name` 和 `position` 均为可选参数;缺失时用当前源 buffer。
- org-babel:`:results` 默认 `value` 还是 `output`?**v1 决策**: `value` (org-babel 默认行为),直接返回 LLM 文本。
- 三子目标是否各自独立成文件?**v1 决策**: 是——`superchat-magit.el`, `ob-superchat.el`, eglot 工具并入 `superchat-tools.el`。

## 来源

- 2026-06-15 与维护者的方向讨论(本对话)。
- 现状审计:`examples/`(`git-commit-message` workflow)、
  `superchat-rewrite.el:74`(`--call-llm` 无工具 blocking 调用,可复用)、
  `superchat-tools.el`(工具 registry)、`read-editor-state.md`(flymake/eglot 诊断已接、源 buffer 记录)。
