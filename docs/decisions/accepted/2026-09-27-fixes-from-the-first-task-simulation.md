# Decision: fixes from the first task-simulation batch — a bounded eval result, ask_user alone is corrected not fatal, the preflight names the request

Status: accepted — user, 2026-09-27: "都按建议" to "先修 P1、P2、P3，并核实 P4，再重跑" after the first real batch of the task simulation (`.scratch/task-sim/FIRST-BATCH-REPORT.md`). The fix directions are the ones the issues list first; they follow contracts the product already has (the read tools' caps; the tool-protocol retry).

## 一句话

模拟测试的第一批里，有两次任务不是模型做错，而是产品把它“判了死刑”：一次是模型用 `eval_elisp` 打印出 108 KB 的按键表，把对话撑爆；一次是模型把“问用户”和别的工具放在同一批，产品直接报错收场。这次让这两种情况都变成“告诉模型、让它改正”，并把误导人的“文件过大”改成“请求过长”。

打个比方：以前是学生交卷时多夹了一张草稿纸，老师就把整份卷子作废；以后是老师把草稿纸退回去，说“这张不用交，重新交一份”。

## Problem

Evidence: `.scratch/task-sim/runs/T4-real-t2-*` and `T6-real-t1-*`; issues `.scratch/issues/2026-09-26-eval-elisp-result-has-no-cap.md`, `…-ask-user-in-a-batch-ends-the-run.md`, `…-context-preflight-message-says-file.md`.

- `superchat-tool-eval-elisp` returns `(format "Result: %S" result)`: an error is bounded to 1,024 characters, a value is not. One keymap dump returned 108,699 bytes; the next request was estimated at 69,237 tokens and refused by the context preflight, whose recovery compacts earlier turns and cannot shorten the Run's own tool result. The Run ended.
- `superchat--llm-capture-provider-tool-uses` signals `llm-error` "Required input must be the only tool in its batch" when `ask_user` shares a response with another call. `llm-error` takes the fatal branch of the request's error callback; the person saw "[API error: An error occurred in LLM operations: …]". The tool-protocol retry branch (types named `llm-tool-…`: drop the dangling tool-use record, append a correction as a user message, resend, at most the unknown-tool retry limit) already handles model-correctable call mistakes and does not see this one.
- The context preflight says "[文件过大：…]" though no file is involved; it is the request that is too long.

## Decision

1. **A successful `eval_elisp` value is bounded.** Printed as before; when the printed value is longer than `superchat-tool-eval-result-max-chars` (20,000), the result keeps its first 16,000 and last 4,000 characters around one line saying how many characters were cut from the middle, how long the whole value is, and to return something smaller (a count, a filtered list, one lookup) to see it whole. The result still begins with `Result: `. Shorter values are unchanged, character for character.
2. **`ask_user` in a shared batch is a correctable call mistake.** The rule stays (the input tool is called alone); breaking it signals `superchat-llm-input-not-alone`, which the request's error callback treats as a tool-protocol error: none of that response's calls run, the model is told that `ask_user` must be the only call in its response, that none of the calls ran, and to call `ask_user` by itself and continue after the answer; then the request is resent. The shared retry limit bounds repeats; past it, or where tool-protocol retry does not apply (a final-answer request, an evidence-contract Run), the Run ends as before.
3. **The preflight names the request.** "[请求过长：估算输入 N tokens + 输出预留 M > 模型上下文 L tokens；请求未发送。可以压缩对话、让工具返回更少内容，或换上下文更大的模型。]"

P4 (llm's 65,536 for `deepseek-chat`) is not a code change: `deepseek-chat` is a retired name answered by DeepSeek Flash; the task simulation uses `deepseek-v4-flash` from now on, and the user's own configuration is theirs to change (issue `.scratch/issues/2026-09-26-deepseek-chat-context-length.md`).

## Alternatives considered

**Spill the full eval value to a buffer the model reads in windows (DeepSeek Harness's spill).** Keeps everything reachable; lost for now because a value is usually one Lisp object whose middle is rarely what the model wanted, and the model can re-ask a smaller question at once. Can be added if runs show the cut losing needed data.

**Cap with `print-length`/`print-level`.** Bounds the printer itself; lost because it changes ordinary results (every list longer than N) that are below any sensible size limit.

**Run the other calls and hold the question.** Saves a round; lost because what the other calls do may depend on the answer the model is asking for, and it changes the order the model asked for.

**Keep the batch rule fatal.** Lost: the model can correct it in one step, and the person sees an API error that is not one.

## Acceptance

A new regression file (not the M3 set), red at each gap on the current tree first: a small value unchanged; a 108,000-character value cut to the head and tail with the note, the whole length and the cut count in it, still starting with `Result: `; a value exactly at the limit unchanged; a Run whose model returns `ask_user` with another call then an answer completes, sends a second request carrying the correction, and runs neither call of the first batch; the preflight message says 请求过长 and not 文件过大. Then mutants, the full sweep, and the task-simulation rerun.

## Consequences

- A model that printed a huge value now sees a cut with a pointer instead of a lost Run; a value between 20,000 characters and the context limit, which used to arrive whole, now arrives cut.
- A response that breaks the input rule costs one extra request instead of the task.
