# Decision: C-g stops a model's Emacs Lisp that runs away

Status: accepted — user, 2026-09-30: "A" to "first find out what C-g does while a tool runs; if the ending is wrong, fix the product first, and let the stand-in press C-g after 30 s frozen".

## 一句话

模型写的一段 Emacs Lisp 如果陷入死循环，你的 Emacs 就冻住了。本该按 `C-g` 就能停，但 superchat 是在 Emacs 屏蔽 `C-g` 的地方（网络回调和定时器里）执行这段代码的，所以按多少次都没用，只能强关 Emacs。修法：执行模型代码的那一段放开 `C-g`；按下后，这次调用记为“被用户打断”，整个任务取消。这套收场逻辑早就写好了，只是 `C-g` 一直到不了那里。

## Evidence

- Task simulation 2026-09-30, run T4-real-t3-20260930-003359: an `eval_elisp` expression looped for ever (`re-search-backward "^"` returned point to the match); no timer ran again; killed at 720 s.
- `superchat-tool-eval-elisp` evaluates with `inhibit-interaction` but leaves `inhibit-quit` as the caller has it.  The call chain comes from llm.el's response callback, which plz runs from a timer (`plz--sentinel`: `run-at-time 0`); Emacs runs timers and process filters with `inhibit-quit` t.
- In a terminal Emacs: a loop in a timer is not broken by three C-g presses; the same loop with `inhibit-quit` bound to nil stops at the first (`.scratch/task-sim`, 2026-09-30).  End to end, the fake model's `(while t ...)` through Superchat: three stand-in C-g presses, Emacs frozen until killed.
- A quit that does reach the tool is handled: `superchat-tool-execution--handle-quit` records "[Tool call interrupted by user]", cancels the Run and re-signals; the chat shows the call as stopped (`scripts/tool-quit-regression.el`).

## Decision

1. `superchat-tool-eval-elisp` evaluates the model's expression with `inhibit-quit` nil, so a C-g during it quits there.
2. The existing quit handling is the ending: the call recorded as interrupted by the user, the Run cancelled, no further request, the quit re-signalled.
3. Only `eval_elisp`: the other tools run Superchat's own bounded code.

## Consequences

- A C-g pressed while the model's code runs stops the task, as a C-g stops any command.
- Code that the model runs and that itself expects `inhibit-quit` (it runs as a timer would) now can be quit.  Not seen; a person's C-g is the intent.

## Acceptance

- A red test: `superchat-tool-eval-elisp` called with `inhibit-quit` t (as from a timer) on an expression that sets `quit-flag` and loops a bounded while signals `quit` instead of returning the loop's value.
- End to end in the task simulation: the fake model's endless loop is stopped by the stand-in's C-g and the Run ends cancelled.
- The full sweep passes.
