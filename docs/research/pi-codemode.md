# Pi 1.0 的 Codemode：参考实现里的事实

这页只记 Pi 是怎么做的，以及 superchat 现在的对应情况，供第 6 片的设计引用。来源都是原文：`github.com/earendil-works/pi` 的 `v1.0.0`（提交 `a13d35a`，2026-10-01），本地在 `~/Documents/reference-code/pi`；读了 `packages/coding-agent/docs/codemode.md`、`cli.md`（Enable codemode、Tool search 两节）、`settings.md`（`codemode.*`）、`mcp.md`（Control tool exposure）、`packages/coding-agent/src/extensions/codemode/tool.ts`；以及 Earendil 的两篇文章 `earendil.com/posts/pi-1-0/`、`earendil.com/posts/you-said-no-mcp/`（2026-09-29）。2026-10-02 阅读。

## Pi 怎么做

| 方面 | Pi 1.0 |
|---|---|
| 是什么 | 一个叫 `codemode` 的工具。模型写一段 JavaScript，在 QuickJS 沙盒里作为 async 函数体运行；脚本用 `await tools.<name>({...})` 调别的工具。原文："Only the script's output reaches the model, so a script can run calls in parallel and filter large results before the model sees them." |
| 输入 | 原样的 JavaScript，不是 JSON，不带代码围栏。可选首行 `// @options: {"max_output_tokens": …, "timeout_ms": …}`。对支持的模型用语法约束采样，让它直接写源码而不是 JSON 转义的字符串 |
| 沙盒 | 没有 Node、文件系统、网络、定时器；只能通过工具和 `models` 碰外界。256 MB 内存。脚本不能再起脚本 |
| 嵌套调用 | 走和直接调用同一条管线：参数校验、钩子、权限检查"exactly as for direct calls"。嵌套调用的结果不进模型的上下文，只有脚本的输出进。调用是真的，失败前已做的不撤销 |
| 返回值 | 声明了输出结构的工具返回结构化值（`bash` 返回 `{ output, exit_code, … }`，输出最多 1 MiB）；其它工具返回文字；失败则抛出 Error，可用 `Promise.allSettled()` |
| 输出 | `text()`、`console.log()`、`return`、`image()`；默认最多 10000 个 token，超出的保留头尾，全文写到临时文件 |
| 找工具 | `codemode` 的说明里按命名空间列出工具的 TypeScript 声明，共用 3000 token 的预算；放不下的和"deferred"的不列，脚本用 `searchTools()`（BM25）、`describeTool()`、`ALL_TOOLS` 找。说明在 MCP 服务器连上时保持不变，以免缓存失效 |
| 状态 | `store(key, value)` / `load(key)`：小的 JSON 值，成功的脚本写进会话记录（`codemode-store`），恢复会话、分支都各自看得到 |
| 与普通工具并存 | 设置 `codemode.mode`：`on`（默认）普通工具照常声明，说明里加一句"脚本里怎么调"；`only` 普通工具对模型隐藏，只能经脚本调用 |
| 何时开启 | 默认不开（`"defaultTools": ["+codemode"]` 打开）；配置了以 codemode 方式暴露的 MCP 服务器时自动开启 |
| 给模型的指引 | 系统提示里一句："Use codemode to batch independent tool calls (Promise.allSettled), chain them, or filter large output, instead of many separate calls." |
| 为什么 | `you-said-no-mcp`："Codemode is special in that it runs where the harness runs. It's best understood as a mechanism to orchestrate and coordinate tool calls."；"Because Codemode also runs on the harness side, its state is also maintained as part of the session transcript instead of the file system."；选 JavaScript 是因为"small versions of JavaScript can be shipped as WASM binaries and allow reasonable levels of protection." |

## superchat 现在的情况（2026-10-02，代码与任务记录）

- 每调用一次工具，就要把整段上下文再发一次给模型。任务记录（`.scratch/task-sim/runs` 下 387 次真实运行）：每个任务发请求的中位数 10 次，最多 51 次；工具调用中位数 12 次，最多 52 次。一次 T6 运行：13 次请求，第一次 11 KB、最后一次 28 KB，合计 252 KB；每次请求带 13 个工具的说明，约 8.6 KB。
- 默认工具清单 12 个（`superchat-llm-default-tool-names`），每次请求都带全部说明。
- `eval_elisp` 已经是"写代码在宿主里执行"：Emacs Lisp 在用户正在用的 Emacs 里运行。但它属于"完全控制"一类（`superchat-sandbox-full-control-tools`），默认权限下要用户批准；它不经过 superchat 的工具管线，通过它做的修改不记账（`/changes` 原文："edits made through eval_elisp are not tracked"）。
- 系统提示里已有一句允许并行："You can batch INDEPENDENT read-only tool calls into a single turn."

## Emacs 里与沙盒相关的现成机制（未验证用法，只记存在）

- `unsafep.el`（`unsafep` 函数）：判断一个 Lisp 表达式是否"安全"（不改全局状态、只调用被标为安全的函数）；`safe-functions` 可以添加被认为安全的函数。
