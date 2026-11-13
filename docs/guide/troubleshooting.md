# Troubleshooting

## "superchat-llm-backend is not configured"

`M-x superchat` can open with `superchat-llm-backend` set to nil, but the first message is refused when it is sent. The chat reports:

```text
[Error: superchat-llm-backend is not configured. Set it to a `make-llm-*' struct (e.g. (make-llm-openai :key ... :chat-model ...)).]
```

Set `superchat-llm-backend` to a provider struct before sending the first message (see [configuration](configuration.md#backends)). Run `/backend` to check it.

## "Model not found" for `@model`

Run `/models` to see the available models and `/refresh-models` to reload them from the backend, or add your own: `(setq superchat-manual-models '("model-id-1" "model-id-2"))`.

## Tool calls time out

Each provider attempt gets its own silence window: 120 seconds by default (`superchat-response-timeout`), multiplied by 1.5 in tool mode (`superchat-tool-timeout-multiplier`) and adjusted for context. New text, reasoning and tool progress refresh that window; an earlier preamble does not disable later protection. There is no cumulative limit on the whole transport conversation by default; `superchat-response-timeout-hard-limit` can set one. The Run deadline (`superchat-runtime-default-deadline`, 1800 seconds) bounds the whole Run.

A question put to you waits for you, whether an approval question or the model's own `ask_user` panel in the chat window: while it is open neither the silence window nor the Run deadline runs, and both continue with what was left after you answer. If a Run is ended by something else while its question is open (cancelled elsewhere, a failure), the question is dismissed and you are told the call was not run.

If an individual attempt is slow, widen the silence window, for example `(setq superchat-response-timeout 300 superchat-tool-timeout-multiplier 1.2)`. These timers do not preempt synchronous Lisp; see [model progress and tool budgets](../protocols/request-lifecycle.md#model-progress-and-tool-budgets).

## Tool round limit reached

A round is one model response invoking one or more tools. By default there is no round limit: a task calls tools until the model answers, you stop it with `C-c C-k`, or the Run deadline passes ([decision](../decisions/accepted/2026-09-29-no-tool-round-limit-by-default.md)). With `superchat-llm-max-tool-rounds` set, at that many rounds interactive Emacs asks whether to grant as many again. Declining, or batch mode, requests a final answer without tools; `stop` (`superchat-llm-round-limit-action`) ends at once. The chat keeps a separate budget notice, including after reopening the conversation. Read the answer's completed, incomplete and unverified work before treating a configuration task as done.

## Cut off after 120 tool calls

The hard limit is `superchat-agent-max-tool-calls`, 120 calls per execution. While an execution runs, the status line shows `tools N/120`. When 15 calls are left (`superchat-tool-budget-reminder-left`; nil turns it off) the model is told so once. Calls past the limit are refused, and the execution's end says so: the chat, `M-x superchat-pending` and the notices list read `Cut off after 120 tool calls`, and the ending facts carry `:tool-budget (:action cut-off :limit 120 :used 120)`.

## Stopped after failed calls in a row

When `superchat-tool-failure-brake` (default 3; nil turns it off) tool calls in a row have failed, the execution stops: no further call and no further request, and the chat, `M-x superchat-pending` and the notices list read `Stopped after 3 failed calls in a row`. A call that succeeds starts the count again, and a call you or the permission level refused is not a failure. An `edit_buffer` error (text not found, more than one match) counts as a failed call, like an `eval_elisp` error.

The brake is one of `superchat-tool-loop-hooks`: functions asked about every tool call before its body and after its result, which may let it be, give the model other text (the original stays on the Tape, the rewrite is recorded as `tool.rewritten`), or stop the execution. nil turns them all off.

## A reply cut off at the output limit

When the provider says a reply was cut off at the output limit (stop reason `length`), none of that reply's tool calls runs (`superchat-cut-off-reply-runs-nothing`, default `t`; nil runs them). The model is told `Your reply was cut off at the output limit (N tokens), so none of its tool calls ran. Do the same work in smaller pieces: …` and asked again. After `superchat-llm-unknown-tool-retries` (2) such retries in a row the execution stops, and the chat, `M-x superchat-pending` and the notices list read `Stopped: the reply was cut off at the output limit 3 times in a row`. A reply of text only that was cut off is still the answer, with a line under it saying so. Every cut-off is the fact `response.cut-off` on the Tape, with the option off too. Without a stop reason from the provider nothing changes; nothing is guessed from the text.

## Memory does not persist

See [memory](memory.md#when-memory-does-not-persist).

## The "User:" prompt does not come back after a response

Check whether the Run is still waiting for the model, a tool or your input (`M-x superchat-pending` lists all three). A synchronous blocking tool can keep Emacs timers from running, so a timeout does not make arbitrary Lisp interruptible. Look at the tool and its server before raising the budget.

## "Symbol's value as variable is void: superchat--dispatch-menu-transient"

This happens when `evil-collection` binds keys in Superchat's transient dispatch menu and evaluates the symbol as a variable. Remove `superchat` from `evil-collection-mode-list`:

```elisp
(setq evil-collection-mode-list
      (remove 'superchat evil-collection-mode-list))
```

Or turn evil-collection off for Superchat in your `evil-collection` use-package block.
