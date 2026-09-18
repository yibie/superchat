# Decision: Emacs 写入范围契约——模型选错目标时也不能改受保护的来源

Status: rejected — 2026-09-17 用户改选按 DeepSeek Harness 的方式替换 Authority（"a"）；指名保护单个文件在 dsh 里没有对应结构，且本记录依赖的执行期护栏（buffer 只读 + 文件处理器）经复现不可靠。取代它的是 [权限档位契约](../proposed/2026-09-17-permissions-the-dsh-way.md)，其中保留了本记录的"三个可观察问题"作为结果证据要求。正文按提出时冻结。

## Problem

M5 最终真实任务里，`superchat-act` 入口的模型在用户明确要求"不要修改这个文件"之后，仍然用 `edit_buffer` 加 `eval_elisp` 中的 `save-buffer` 改写并保存了来源文件 `work.el`（[验收记录](../../reports/2026-09-17-m5-execution-handoff.md)）。工具忠实执行了错误参数；撤销后恢复原状不能让这次越界变成合格。

现有 [Authority](../../components/superchat-authority.md) 只看工具声明的 Effect：`edit_buffer`、`replace_buffer`、`append_to_buffer` 声明 `local-write buffer :scope <buffer 名>`，所以策略层能按名字拒绝；但 `eval_elisp` 只能声明 `process-exec emacs <表达式全文>`，`shell-command`/`eshell-command` 只能声明 `process-exec process <命令>`。一段 Lisp 或一条 shell 命令会碰哪个 buffer、哪个文件，Authority 在执行前不可能知道。自然语言的禁令不会自动变成机器可执行的规则；只拦 `edit_buffer` 的名字，或在 eval 文本里搜索路径，都挡不住 `(with-current-buffer (car (buffer-list)) (save-buffer))` 这类间接写法。

同一次任务还暴露了答复无法核对的三个空白：模型说"已改"，但没有说改的是哪个对象；说"已保存"，但分不清只改了内存、已写入磁盘、还是已重新加载；说"已验证"，但没有说明验证做了哪些检查。这三个空白让人工审阅只能逐条对照 Tape，也让验收器只能靠外部后置条件判定。

## Proposal

### 每次写入必须能回答的三个可观察问题

| 问题 | 谁回答 | 何时回答 |
|---|---|---|
| **目标是什么**：这次调用会改哪个 buffer、哪个文件 | 工具的 Effect 解析器，通过 `:resource`/`:scope` 声明 | 执行前，供 Authority 决策；不是事后从结果推断 |
| **改到哪一层**：只在内存、已写入磁盘、还是已重新加载生效 | 工具结果正文明确写出到达的层级和证据（buffer 名、文件路径、字节数、加载返回值） | 每次工具返回 |
| **验证做了什么**：读回了哪个有效值、在哪个 buffer、是否在新 Emacs 中 | 工具结果携带读回的观察值；最终答复按"已做／未做／未验证"分开陈述（[预算收尾指令](../implemented/2026-09-17-model-attempt-progress-and-budget-finalization.md)已要求这种格式） | 每次工具返回与最终答复 |

能回答第一个问题的工具称为**有界写入**（bounded write）：目标在参数里，Effect 能命名它。不能回答的称为**无界写入能力**（unbounded write capability）：`eval_elisp`、`shell-command`、`eshell-command`。契约不假装无界能力可以靠文本检查变成有界。

### 受保护集合（protected set）

一个 Run 可以携带一组受保护资源：buffer 名和文件绝对路径。集合是 Envelope 的显式元数据，由 Runtime 保存在 Run 条目上，随 `run.started` 记录进 Tape；中间件不能扩大或清空它。来源从哪里来是下面待定的第一项。

### 两层执行，各自的承诺

**第一层——Authority 策略层 `write-scope`**（tighten-only，批准不能推翻）：

- `local-write` 且 `:scope` 命中受保护集合中的 buffer 名 → `deny`，disposition `write-scope-denied`。
- 受保护集合非空时的 `process-exec process`（shell/eshell）→ 待定第三项：`deny` 或 `ask`。外部进程写文件发生在 Emacs 之外，第二层管不到，所以不能承诺。
- 其他 Effect 不变。拒绝走既有 [ADR-0006](../../adr/0006-authority-refusal-consumer-delivery.md) 路径，作为工具结果回到模型；Run 继续，不因一次拒绝终结。

**第二层——Emacs 内写入护栏**（只在工具 body 的动态范围内生效，覆盖 `eval_elisp` 与任何有界工具的间接写入）：

- 受保护 buffer 在 body 执行期间置 `buffer-read-only`。
- 受保护路径注册临时 `file-name-handler-alist` 处理器：`write-region`、`delete-file`、`rename-file`、`copy-file`、`set-file-modes`、`make-directory` 等写类操作直接 signal `superchat-write-scope-violation`，读类操作透传原语。`save-buffer`、`write-file`、`append-to-file`、`with-temp-file` 都经过 `write-region`，因此都被拦下。
- signal 由既有工具错误链交付（[工具反馈决策](../implemented/2026-09-17-emacs-tool-feedback.md)），错误正文点名被保护的对象和触发的操作；Tape 记 `tool.failed`，disposition `write-scope-violation`。
- 护栏在 body 返回或 signal 后立即卸下；不影响用户自己的编辑，不影响 Run 之外的 Emacs。

**明确不承诺的**：第二层挡的是"模型按普通方式选错目标"。有意绑定 `inhibit-read-only`、临时置空 `file-name-handler-alist`、在 Lisp 里 `call-process` 出去改文件，仍然能写。这不是沙箱，也不是事务回滚；文档必须这样写，验收也按这个边界测。

### 与既有契约的关系

- Runtime 仍是 Run 接受、取消、终结的唯一所有者；受保护集合只是 Run 元数据。
- 每个模型工具调用仍然只经过 `superchat-tool-execute` 一个缝；本契约新增一个策略层和一个 body 期间的护栏，不新增第二个授权中心。
- 结构化配置工具（如"给某个 mode 设置按键""往 init 追加一段"）能让目标进入参数，减少模型自己拼 Lisp 的机会，是本契约之后的独立决策（待定第四项）；路线图"一个任务证明需要一个 seam 才建"的规则不变。

## Open choices

| # | 待定 | 选项 | 建议 |
|---|---|---|---|
| 1 | 受保护集合从哪里来 | (a) 只有显式声明：`superchat-write-scope-protected` 变量 ＋ Envelope `:protected`；`superchat-act` 的检查器提供"保护来源 buffer／文件"开关，默认关 (b) `superchat-act` 自动保护其来源，检查器可解除 (c) 两者都要，act 默认自动保护 | (a)。act 也常用于"改这个 buffer"，自动保护会把这类任务变成拒绝；先做显式，等真实任务说明默认值再改 |
| 2 | `eval_elisp` 的护栏深度 | (a) 上述第二层护栏，文档写明可被有意绕过 (b) 受保护集合非空时直接 deny `eval_elisp` (c) 不为 eval 做护栏，只拦有界工具 | (a)。(b) 让当前唯一的保存／加载手段消失，要先有结构化工具；(c) 就是今天的状态 |
| 3 | 受保护集合非空时 shell/eshell | (a) deny (b) ask | (a)。外部进程的写入既看不见也拦不住，ask 只是把责任推给批准者 |
| 4 | 结构化配置工具 | (a) 护栏落地并跑过真实任务后再单独立案 (b) 与本契约一起做 | (a)。哪些动词是真需要的，应由带护栏的真实任务暴露 |

## Alternatives considered

**只按名字拦 `edit_buffer`。** 来源误改的实际路径是 `edit_buffer` 加 `eval_elisp` 里的 `save-buffer`；拦一个名字挡不住第二步，也挡不住 `replace_buffer` 或以别名引用同一 buffer。它会制造"已保护"的错觉。

**扫描 eval 表达式文本里的路径或 buffer 名。** `(buffer-list)`、变量间接、`find-file-noselect` 返回值都不含目标字面量；文本匹配既有漏报也有误报，而且会把一次批准绑定到一个无法解释的判定上。

**受保护集合非空时禁用 `eval_elisp`。** 这是唯一能宣称"绝不越界"的做法，但现在保存、加载、读回有效绑定都靠它；在结构化工具存在之前，等于让任务无法完成。作为第 2 项的选项保留。

**完整沙箱或事务回滚（快照全部 buffer 与文件，失败时回滚）。** 成本与范围都远超"选错目标不能写"这一需求，且回滚本身要判断哪些变化属于模型，与用户中途编辑冲突。不在本契约内。

**把禁令写进系统提示，靠模型遵守。** 已经试过：C 组短说明改善了状态，但 B/act 在明确禁令下仍然越界。提示可以继续存在，不能作为保证。

## Acceptance criteria

独立脚本回归（新文件，不改受保护的 M3/M5 集合），全部不需要真实模型：

1. 受保护 buffer 上的 `edit_buffer`／`replace_buffer`／`append_to_buffer` 在 Authority 被拒，Tape 有 `tool.requested` 与 `tool.failed`（`write-scope-denied`），body 未执行，批准函数未被调用；同名之外的 buffer 照常。
2. `eval_elisp` 中的 `save-buffer`、`write-region`、`with-temp-file`、`delete-file`、`rename-file` 指向受保护路径时 signal `superchat-write-scope-violation`，磁盘字节与 mtime 不变，错误正文点名对象；指向其他路径时照常。
3. 通过 `(with-current-buffer (get-buffer …) (insert …))` 修改受保护 buffer 被 `buffer-read-only` 拒绝；body 结束后该 buffer 恢复原来的只读状态，用户可以继续编辑。
4. 护栏只在 body 动态范围内存在：body 前后 `file-name-handler-alist` 与各 buffer 的 `buffer-read-only` 完全恢复，包括 body signal 或 quit 的情况；并发的另一个 Run 不受此 Run 的集合影响。
5. 受保护集合随 Run 记录进 Tape，`run.started` 载荷可读回；中间件的候选替换不能改变它。
6. 有意绕过（局部置空 `file-name-handler-alist`）确实能写——作为**文档边界测试**存在，证明契约没有多承诺。
7. 真实任务：在 `5291b745` 时越界的同一 B/act 任务，在带护栏的产品上重跑一次；要求来源文件字节不变，且 Tape 中出现至少一次 `write-scope-denied` 或 `write-scope-violation`。模型是否随后完成任务是另一项证据，不作为本契约的验收。

## Risks

- `file-name-handler-alist` 处理器覆盖的操作表若不完整，会漏掉某个写类原语；表要按 Emacs 手册 "Magic File Names" 的操作列表逐项核对，并在回归里逐项测。
- 受保护 buffer 置只读期间，模型的合法读取（`read_buffer`、`buffer-substring`）不受影响，但任何依赖 `buffer-modified-p` 副作用的工具行为需要复核。
- 第 1 项选 (a) 时，用户忘记声明就没有保护；这是显式契约的代价，README 必须给出一行示例。
- 策略层按 buffer 名匹配；改名后的 buffer 需要靠文件路径的第二层兜住，无文件的 buffer 改名后不再受保护——契约里要写明。
