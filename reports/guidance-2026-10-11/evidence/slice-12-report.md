# Slice 12 report

Implementer session 39daf101. Base `f725e3b3`.

## Stop point one — 12a: the sentence goes into the product (2026-10-21, not committed)

### What changed

- `superchat-tools.el`: the `work_record` `:description` ends with F1's sentence, word for word, after "…whether it is done, waiting or continuable.":
  `When the user says to do the rest later, register wait_for user with what is left in detail; Superchat will not go on by itself.`
  Nothing else in the description changed. This is the only text given to the model that changed.
- `scripts/work-defer-regression.el` (new, 1 test): pins F2.
- `scripts/work-record-regression.el`: one assertion added to `work-record/the-two-ways-to-wait-for-the-user-are-told-apart` (the description ends with the sentence). No other test changed.
- `docs/roadmap.md`: the open observation added in `f725e3b3` is marked ruled 2026-10-21 with a paragraph naming slice 12a and the commit title. It was not deleted. A commit cannot name its own hash, so the paragraph names the title ("Work the user puts off waits for the user"); the hash can be added in a later commit if you want it.
- `CHANGELOG.md`: one entry.
- Harness (not committed, `.scratch/`): `ts-defer-wording.el` adds the sentence only when the description does not already contain it. Checked: on the new product code the sentence occurs once; on an older description (`"Old text."`) it is still appended; other tools are untouched. The file is kept, so 11d's evidence still matches.
- `docs/decisions/accepted/2026-10-11-goal-rounds.md`: not touched. The full description is quoted verbatim nowhere outside the product; the only other copies are old wire captures under `reports/` (`architecture-options-2026-10-11`, `tool-surface-2026-10-13`), which are evidence of what was sent then and were left alone.

### The pin test (F2), expected green, was green

`work-defer/a-wait-for-the-user-is-not-taken-up-by-itself`, in the `hm--turn`/`wrec--with` fixture style, using the goal-rounds and work-resume helpers:
1. The user's execution: the model starts a record, registers `wait_for user` with "three books are left for next time", answers in text. The record is `waiting-user`.
2. The user says "thanks"; the model answers in text only.
3. Then: 0 `work.round`, 0 `queue.queued` in the conversation and on the whole Tape, 4 provider requests in all, no `goal-rounds` Run; the record is `continuable` and `superchat-work-continuing-p` is nil.
4. `/work resume` echoes "Superchat goes on with …", one `work.round` follows from a `goal-rounds` Run, and the record completes.

Green on the code before the sentence and after it. So the second half of the sentence is true of the product today, as F2 said; nothing in the product was changed to make it so.

### The flipped assertion

`work-record/the-two-ways-to-wait-for-the-user-are-told-apart`, before the product change: FAILED at
`(string-suffix-p " When the user says to do the rest later, register wait_for user with what is left in detail; Superchat will not go on by itself." work)`. After: 25/25 of `work-record/` pass.

### Mutants (copy `~/.cache/sc-mut-12a`), 4 of 4 caught

| mutant | caught by |
|---|---|
| the user's answer also restarts the rounds (`superchat-work--on-user-input` calls `superchat-work-continue`) | work-defer |
| a waiting record is not paused at the end of the execution (the "Done, or waiting" branch does nothing) | work-defer |
| the sentence says "Superchat will go on by itself" | work-record told-apart |
| the sentence is missing | work-record told-apart |

### Full check (snapshot `~/.cache/sc-snap-12a`, run alone)

Kernel OK; smoke `SMOKE OK (0 mismatches)`, baselines unchanged (no baseline holds the `work_record` description); M3 (run only, not changed) 137 passed, 2 skipped by design; 106 regression suites, none with an unexpected result; compile warnings 79. The `load:` line's `Unknown type llm-provider-utils-tool-use` is the same as on 11b's check. The first attempt did not run at all (the check script had lost its execute bit); it was rerun through zsh on the same snapshot. The 5 files to commit are byte-identical to the snapshot.

### To commit (5 files, by path)

`superchat-tools.el`, `scripts/work-defer-regression.el`, `scripts/work-record-regression.el`, `CHANGELOG.md`, `docs/roadmap.md`. Diff: `slice-12a.diff`.

### Not shown

No real model run was made for 12a; the evidence for the wording is 11d's harness runs (three per arm, a direction). The pin test uses the scripted model.

## Stop point two — 12b: a reply cut off at the output limit runs no call (2026-10-21, not committed)

12a was committed as `80417ca0` (5 files).

### The two checks asked for before the change

- `tool-error-retries` (`superchat-llm.el`, the request closure) starts at 0 once per request closure, that is once per Run, and is never reset: it counts every tool-protocol retry of the Run, not a run of them in a row. So 12b does not use it. It has its own counter, `cut-off-streak`, which every reply that was not cut off resets to 0 (in `response-cb`, which tool rounds also reach). "In a row" is therefore what the code does, and the wording says so. The limit is `superchat-llm-unknown-tool-retries` (2), as asked: the 3rd cut-off in a row stops.
- The fork's `llm-provider-utils-streaming-accumulate` has no symbol branch. Chunks with `finish_reason: null` do not add the key: `llm-provider-utils-json-val` turns `:null` into nil and `llm-openai--normalize-stop-reason` of nil is nil, so `(when stop-reason …)` adds nothing. Only the last chunk contributes `:stop-reason`. If two chunks both carried a symbol, `pcase (type-of current)` has no `symbol` case and returns nil, so the accumulated value would be nil and the cut-off missed. Not seen with DeepSeek; recorded as a limit. (12b does not depend on the accumulation for the stream: it reads `:stop-reason` from each chunk as it arrives.)

### What changed

- `superchat-cut-off-reply-runs-nothing` (default t; nil = today's behaviour).
- `superchat--llm-cut-off-feedback`: the sentence, one definition; N is `llm-chat-prompt-max-tokens` of the prompt (what Superchat sent as `max_tokens`), else `the provider's output limit`.
- (B) `:around` `llm-provider-utils-process-result` (`superchat--llm-refuse-cut-off-reply`): stop reason `length` and the request registered a handler for this prompt → the handler writes the fact and, if the option is on and there are `:tool-uses`, the whole reply is refused through the error callback with `superchat-llm-reply-cut-off`, without calling the original: no text appended, no call run. `superchat--llm-drop-dangling-tool-use` is then a no-op: the last interaction is the user's message or the tool results, not an assistant record (checked by reading it: it acts only on a trailing `assistant` interaction whose content is not a string). Same seam for streaming and non-streaming.
- (A) A reply whose arguments do not parse: the stream's `:stop-reason` is kept per request as it arrives; without streaming (see the real run below) `:around` `llm-provider-utils-extract-all` keeps the stop reason of a response whose arguments failed to parse (`superchat--llm-unparsed-stop-reason`) for the error callback. When it is `length` and the error is a tool-protocol error or plain `error`, it becomes `superchat-llm-reply-cut-off`.
- The error callback's new branch: retry with the cut-off sentence (attempt reason `tool-protocol-retry`) while the streak is within the limit; then `superchat-runtime-stop-tools` with `Stopped: the reply was cut off at the output limit N times in a row` and finalize as failed — the same shape as 11b's stop, so the Run's ending facts, the notice under the answer and the pending row carry it (test 4 checks all three).
- (C) Text only and `length`: not refused, not retried, still the answer; the fact is written and `superchat-render-run-cut-off-line` adds under the answer `The reply was cut off at the output limit (N tokens); the answer may be incomplete.`
- Fact `response.cut-off`, written by Runtime (observer case `reply-cut-off`, beside `response.completed`'s layer), registered in `superchat-tape.el` as a session annotation (not model history): `:stop-reason "length" :max-tokens :tool-calls :text-chars :attempt :refused`. `:attempt` is the place in the streak (1, 2, 3). `:tool-calls` is nil in case (A): the calls could not be counted. Its summary is the Tape view's line, e.g. `Reply cut off at the output limit (60 tokens): 2 tool calls not run`. Written with the option off too.
- `scripts/smoke.el`: a step may carry `:stop-reason`, plist chunks, and `:unparsed-stop-reason` (what the extract-all advice leaves). M3 untouched.
- Not done with 11b's per-call hooks, as you said: they run per call, after llm.el has appended the assistant record and inside the tool wrapper; a cut-off must refuse the whole reply in one place before its first call. "The hooks' second user" does not hold.
- Not covered: Claude `stop_reason`, OpenAI Responses `incomplete_details` (the fork normalizes only the openai-compatible family).

### Red tests (`scripts/reply-cut-off-regression.el`) and what was red on `80417ca0`

| test | red at |
|---|---|
| the words say the limit or that there is one | `void-function superchat--llm-cut-off-feedback` |
| 1 two whole calls cut off run neither | `(should-not (th--facts session "tool.requested"))` |
| 2 cut off in the middle of arguments is said so | `(should (string-match-p rco--sentence second))` (the JSON sentence was sent) |
| 3 a reply that was not cut off runs its calls | green (guard) |
| 4 three in a row stop the execution visibly | `(should-not (th--facts session "tool.requested"))` |
| a reply in between starts the count again | `(should (= 6 (length smoke--requests)))` |
| 5 with the option off the calls run and the fact is kept | `(should fact)` |
| 6 text cut off is still the answer with a line under it | `(should fact)` |
| 7 without streaming the same | `(should-not (th--facts session "tool.requested"))` |
| 2b without streaming the stop reason is kept for the error | added after the first real run, not run on `80417ca0`; red on the code before its fix (the function did not exist) |

Now 10/10.

### Mutants (`~/.cache/sc-mut-12b`), 12 of 13 caught

Caught: never refuse; refuse with the option off; never stop; stop one early; streak not reset; limit not said; mid-arguments not mapped; kept reason not set; fact only when refused; no line under the answer; text-only refused; wrong call count. Survived: the error callback not clearing the kept stop reason. Equivalent: every new attempt clears it at its start, and nothing reads it in between.

### DeepSeek (F4) — the fork, not this repository

`slice-12-llm-deepseek-stop-reason.diff`: `llm-deepseek--get-partial-chat-response` reads `finish_reason` of `(aref choices 0)` through `llm-openai--normalize-stop-reason` and adds `:stop-reason`; a test `llm-test-deepseek-streaming-stop-reason` (red without the change, green with it; `finish_reason: null` adds no key). Applied to the working tree of `~/Documents/emacs/package/llm/` only, beside its uncommitted stop-reason changes; not committed there. Remove it if the user does not want it (`~/.cache/sc-tools/llm-deepseek.el.orig` and `llm-test.el.before-12b` are the files before).

**But the user's tool requests do not stream.** `deepseek-v4-flash` has no `streaming-tool-use` capability (no override in `nova-ai.el`), so `superchat-llm--resolve-streaming` sends requests with tools without streaming. Both real runs below were non-streaming (`"stream"` absent from the request, `chat.completion` responses). The DeepSeek streaming patch therefore matters only for requests without tools; the path the user's tool requests take is the non-streaming one, whose stop reason the fork already gives (`llm-provider-utils-extract-all`).

### Real runs (task-sim harness, sandboxed home and data directory; the user's real database unchanged: mtime and size identical before and after)

Harness-only task `CUT1` (`tasks-goals.el`, no check) and `TS_EXTRA_LISP=ts-cut-off-probe.el`, which loads copies of the user's llm fork (the test ELPA's llm 0.32.1 gives no stop reason) and gives every prompt `:max-tokens 60` — what a preset's `max_tokens` does to the prompt; a preset file was not used. Prompt: write about two thousand characters to `~/notes/draft.org` with `edit_buffer`. Tape excerpts: `slice-12b-real-tape.txt`.

1. `CUT1-real-12b-probe-20261021-194430-12177` (before the extract-all fix): request 1 `finish_reason: length` with `read_buffer` and an `eval_elisp` with empty arguments → refused whole (`response.cut-off :tool-calls 2 :refused t`), sentence sent with "(60 tokens)"; request 2 a normal `read_buffer`, ran; request 3 `length` in the middle of `edit_buffer`'s arguments → **the Run failed with `[API error: An error occurred when calling an LLM tool: "Invalid JSON in tool call arguments: End of file while parsing JSON …"]` and no retry at all.** What reached the error callback was plain `error` with that message (not `llm-tool-call-error`, which would have been retried with the JSON sentence): F3's "read from the code only" half is confirmed for the non-streaming path — today a cut-off in the arguments fails the Run. This led to the extract-all advice and test 2b.
2. `CUT1-real-12b-probe2-20261021-194758-14738` (after): request 1 a normal `read_buffer`, ran; requests 2–4 each `length` in the middle of `edit_buffer`'s arguments (58 characters each), each recorded (`:attempt 1, 2, 3`, `:tool-calls nil`, `:refused t`), requests 3 and 4 carry the cut-off sentence with "(60 tokens)"; after the third, `run.failed` with `:tool-budget (:action stopped :reason "Stopped: the reply was cut off at the output limit 3 times in a row")`; 4 requests in all. With 60 tokens the model could not make any edit small enough; it did not split the content. That the model works in smaller pieces at a realistic limit is not shown.

### Full check (snapshot `~/.cache/sc-snap-12b`, alone)

First run: everything clean but compile warnings 80 (one docstring of `superchat--llm-keep-unparsed-stop-reason` wider than 80). Fixed and rerun on a fresh snapshot: kernel OK; smoke `SMOKE OK (0 mismatches)` (baselines unchanged; `scripts/smoke.el` changed only in what a step may carry); M3 (run only, not changed) 137 passed, 2 skipped by design; 107 regression suites, none with an unexpected result; compile warnings 79. The `load:` line's `Unknown type llm-provider-utils-tool-use` is as on every earlier check. The 10 files below are byte-identical to the snapshot. No existing test was changed.

### To commit (10 files, by path)

`superchat-llm.el`, `superchat-render.el`, `superchat-runtime.el`, `superchat-tape.el`, `scripts/smoke.el`, `scripts/reply-cut-off-regression.el`, `CHANGELOG.md`, `README.md`, `README_cn.md`, `docs/roadmap.md`. Diff: `slice-12b.diff`. Not in the repository: the fork patch (above) and the harness files `ts-cut-off-probe.el`, `ts-fork-*.el` (copies of the fork) and task `CUT1` in `.scratch/task-sim/harness-tools/`.
