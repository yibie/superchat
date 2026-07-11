# Goal: 恢复 `.workflow` 文件 + 异步线性执行引擎

> 状态:**待实现。** 里程碑候选:v1.2。
> 创建于 2026-06-16。
> 隶属:独立方向(workflow 子系统)。

## 背景:能力退化

superchat 历史上有完整的 workflow 引擎:

- **`d228c72`**(2025-10-13)`feat(workflow): linear recipes reuse gptel tools` —— 1238 行的
  `superchat-workflow.el`,支持 `.workflow` 文件、逐行步骤、`@model` / `/command` / `#context`
  解析、跨步骤变量累积(`$input` `$result` `$step1`…)、结果摘要。
- **`6c2f8a3`** 把 workflow 重构成 SKILL.md 的 `type: workflow` 子类型。
- **`485b531`** 加了 legacy `.workflow` → SKILL.md 的导入 shim。

重构后,当前的 `superchat-workflow.el`(94 行)**严重退化**:

1. `superchat-workflow-execute`(`superchat-workflow.el:56`)只调用 `superchat-core-run-turn`
   把每步 prompt **拼出来**,然后 `(push (cons step prompt) results)` —— **从不调用 LLM**。
   line 82-83 的 `FIXME: synchronous execution for now` 自认占位。
2. 跨步骤数据流丢失:`results` 里上一步的输出**从不注入**下一步的变量。
3. `on-done` 回调只在 `null steps` 分支触发,正常跑完直接 `(nreverse results)` 返回,
   dispatcher(`superchat-dispatcher.el:288`)也不接收返回值渲染。
4. 入口退化为 SKILL.md 子类型,`.workflow` 独立文件只剩 import shim,不能直接执行。

即:**现在执行一个 workflow 不产生任何模型回答**,只在 echo area `message` 几行。

## 三点诉求(用户)

1. workflow 用**独立 `.workflow` 文件**(回到 `<data-dir>/workflow/*.workflow`),不走 SKILL.md 子类型。
2. **恢复真实执行能力**:每步真正过 LLM、跨步骤传递输出。
3. 线性执行可接受,但**必须异步**——执行期间不能卡住 Emacs。

> 关键发现:即便是老引擎的 `-execute-workflow-stream`(`d228c72`)也是**同步**的——
> `superchat-workflow-call-llm` 在 `dotimes` 里 blocking 调用。所以诉求 #3
> **从未被任何历史版本满足**。本目标不是单纯「回滚」,而是「恢复文件格式 + 执行语义,
> 但把执行器重建为异步 callback 链」。

## 方案

### Part A — `.workflow` 文件加载(恢复独立入口)

复用老引擎的目录约定(`d228c72`):

- `superchat-workflow-directory`:defcustom,默认 `<superchat-data-directory>/workflow/`。
- `superchat-workflow--list` / `--exists-p` / `--load`:扫描 `*.workflow`、按名加载内容。
- 触发方式(二选一,实现时定):
  - 复用现有 `>name` dispatch:dispatcher 在 skills 之外**先查 `.workflow`**,命中则交给
    异步执行器;或
  - 新增 `/workflow <name> [args]` 命令 + 补全(老引擎有 `-completion-workflows`)。
- 保留 `superchat-workflow-import-legacy*`(已在当前文件)用于一次性迁移。

### Part B — 文件格式与解析(恢复逐行步骤语义)

沿用老引擎(`superchat-workflow-parse-line`,`d228c72:95`)的每行一步语义:

- 跳过空行与 `#` 注释行(当前 `superchat-workflow-parse-steps` 已有,保留)。
- 每行可含 `@model`(一次性模型覆盖)、`/command`(已注册命令)、`#context`(文件/区域上下文)。
- 变量:`$input`(触发参数)、`$lang`、`$date`(当前已有,保留),**新增** `$result`
  (上一步输出)与 `$stepN`(第 N 步输出)。
- 一个 step 结构体:`(:model :command :contexts :prompt)`。

### Part C — 异步线性执行器(本目标的核心)

把同步 `dotimes` 循环重写为 **callback 递归链**,复用 superchat 现成的异步生成入口
`superchat--llm-generate-answer`(`superchat.el:1067`,带 streaming + 完成回调)。

执行模型:

```
run-step(i, ctx):
  if i >= len(steps): finalize(ctx); return
  step      = steps[i]
  prompt    = substitute(step.prompt, ctx)         ; $input/$result/$stepN/$lang/$date
  render-step-header(i)                             ; 在 superchat buffer 写 "** Workflow step i/N"
  superchat--llm-generate-answer(
     prompt,
     callback        = λ(answer):                   ; 完成回调
        ctx[$result]      = answer
        ctx[$step{i+1}]   = answer
        run-step(i+1, ctx)                          ; 链到下一步
     stream-callback = superchat--stream-llm-result ; 复用现有流式渲染
     target-model    = step.model)
```

要点:

- **零阻塞**:每步发起后立即返回事件循环,下一步在上一步的完成回调里启动。Emacs 全程可交互。
- **复用渲染**:沿用 `superchat--stream-llm-result` / `superchat--process-llm-result`
  (`superchat-render.el:181/196`),workflow 输出与普通对话同构地落进 buffer。
- **错误中断**:任一步 callback 收到 error,渲染失败信息、停止后续步骤(老引擎的
  `catch 'superchat-workflow-execution-error` 语义,改为 callback 内 early-return)。
- **进度反馈**:每步前写一行 `** Workflow: step i/N`(取代老引擎的 echo-area `message`,
  因为异步下 echo 会被覆盖)。
- **`#context` 注入**:复用 `superchat--current-context-files` 机制,把 step 的文件上下文
  喂给 `superchat--llm-generate-answer` 的 `context-files` 形参。

### Part D — dispatcher 接线

- `superchat-dispatcher.el:285-288`:workflow 不再走 SKILL.md `:type` 分支。
  改为「`.workflow` 命中 → 调异步执行器」,执行器自行渲染,不返回需 dispatch 的 result plist。
- 移除/改写当前 `superchat-workflow-execute` 的占位实现。

## 验收

- `<data-dir>/workflow/foo.workflow`(3+ 步,含 `$result` 引用)可被发现并执行。
- 执行期间 Emacs **不挂起**:能切 buffer、能 `C-g` 中断。
- 每步真实产出模型回答,逐步流式渲染进 superchat buffer。
- 上一步输出经 `$result` / `$stepN` 进入下一步 prompt(端到端验证一次)。
- 任一步失败 → 停止后续步骤并显示失败信息。
- ERT:解析(`parse-line`/变量替换/`$result` 注入)可同步测;执行链用 mock 的
  `superchat--llm-generate-answer`(立即同步回调)验证「链式推进 + 上下文传递 + 错误中断」。

## 非目标(本期)

- **不做 DAG**:不引入 step `id` / `depends-on` / 分支 / 并行。仅严格线性。
  (DAG 留作后续目标;数据流依赖在 Part C 的 `$result`/`$stepN` 已部分具备。)
- 不做工具调用循环的 workflow 级编排(单步内 llm.el 自己的 tool loop 仍生效)。
- 不做 workflow 可视化编辑器。
