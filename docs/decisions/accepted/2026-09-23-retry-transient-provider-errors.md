# Decision: retry a transient provider error before failing the Run

Status: accepted under the user's standing rule of 2026-09-18 (a defect an ordinary user would hit is fixed in the product, red test first, before acceptance continues); the mechanism follows DeepSeek Harness's `llm-retry` defaults, and the user may revise its numbers.

## Problem

Every provider error except a tool-protocol error ends the Run at once (`superchat-llm.el`, the error callback of `superchat--llm-generate-answer`, final branch: `finalize "[API error: …]" 'failed`). On 2026-09-23 a network glitch to `api.deepseek.com` — curl 28 "Operation timeout." after 10 s, and in another run curl 35 "SSL connect error" — ended an attended act acceptance whose work was already correct (`.scratch/m5-config-acceptance/runs/m8-attended-act-deepseek-02/REVIEW.md`), and failed a scripted inline-act check. Issue: `.scratch/issues/2026-09-23-a-transient-provider-error-fails-the-run.md`.

## What the references do

DeepSeek Harness mounts `llm-retry` in its base: errors classified `TRANSPORT`, `TIMEOUT`, `SERVER`, `RATE_LIMIT` or `EMPTY_RESPONSE` are retried up to 5 times after the first request, with exponential backoff from 500 ms to at most 10 s and ±10% jitter (`packages/llm/llm/src/retry-policy.ts:14-24`). pi's coding agent retries retryable provider errors 3 times with backoff from 2 s.

## Decision

1. **Retryable:** a transport failure (curl could not resolve, connect, finish TLS, send or receive; the transfer timed out; the peer returned nothing), HTTP 429, and HTTP 5xx. **Not retryable:** any other HTTP status (a request the provider rejected), tool-protocol errors (their own path), context overflow (its own recovery path), cancellation, and a missing or empty response (governed by the 2026-09-17 missing-provider-response decision, unchanged).
2. **Only before anything was delivered.** A failing attempt is retried only if none of its output reached the user (no streamed text delivered to the Sink for that attempt); otherwise it fails as today, so a retry never repeats text.
3. **Bounded:** at most `superchat-llm-transient-retries` consecutive retries (default 5); the count resets when a provider response succeeds. Delays 0.5 s, 1 s, 2 s, 4 s, 8 s (doubling, capped at 10 s), each ±10% jitter.
4. **The same lifecycle.** A retry is one more attempt of the same Run through the existing send path, with attempt reason `transient-retry`, so request evidence, the inactivity clock and the Run deadline treat it like any attempt. A cancelled or terminal Run sends nothing after the backoff. When the retries are exhausted the Run fails with the last error, as today.

Not decided here: changing the transport's own timeouts, retrying empty responses, a user-visible countdown.

## Acceptance

Scripted transport: one transport error then an answer completes the Run with two requests and a `transient-retry` attempt on record; 429 and 503 retry; 400 does not; six consecutive transport errors fail after five retries with the last error; a cancel during the backoff sends nothing more; a streamed partial answer followed by an error does not retry; the count resets after a success. Canonical smoke and M3 unchanged. Then the act entry's attended acceptance on DeepSeek, in a new directory.

## Narrowing recorded at implementation (2026-09-23)

The pinned llm 0.32.1 passes Superchat's error callback only the type `error` and a string (`llm-request-plz.el:97-120` classifies, `llm-provider-utils.el`'s `:on-error` discards the classification). Transport failures are recognizable from that string ("Operation timeout." for curl 28; "curl error code N: …" for the others), but the HTTP status of a 429 or 5xx is not: only the provider's body text arrives. So this slice retries transport failures only — curl 28 (timeout), 6 (could not resolve), 7 (could not connect), 35 (TLS handshake), 52 (empty reply), 55 (send failure) and 56 (receive failure) — and does **not** retry HTTP 429 or 5xx, and never guesses from body text. That part of the decision waits on `.scratch/issues/2026-09-23-llm-drops-the-http-status-of-a-failed-request.md`. The Runtime's decision-snapshot attempt-reason list gains `transient-retry` so a retry records like any attempt.

## Amendment by the user (2026-09-23)

Asked whether 429 and 5xx should be retried once their status is available, the user answered: "可以类似 codex 这样子重连10次，每一次重连间隔30秒" — reconnect like codex does, 10 times, 30 seconds apart. Applied as one policy for every retryable error:

1. **Policy:** at most `superchat-llm-transient-retries` = 10 consecutive retries, each after a fixed `superchat-llm-transient-retry-interval` = 30 seconds (no doubling, no jitter). This replaces the 5 retries with 0.5–10 s backoff above. Everything else above stands: only before anything of the failing attempt was delivered, only while the Run is active, the count resets on a successful response, attempt reason `transient-retry`.
2. **HTTP 429 and 5xx become retryable.** Superchat keeps the HTTP status at the transport boundary with a hook around llm's plz error handler (`llm-request-plz--handle-error`), made available to Superchat's error callback for the duration of that call; the hook is installed only when that function exists and a test drives the real llm error chain with a stubbed transport, so an llm upgrade that breaks it fails a test instead of silently disabling the retry. Every other HTTP status stays a failure, and response bodies are still never parsed for a verdict.
3. **Timing.** Ten 30-second waits are five minutes; the inactivity clock is stopped during a wait (as now) and the Run deadline (default 30 minutes) still applies.

## Second amendment by the user (2026-09-23): the wait is visible

Asked whether a retry wait should say so on screen instead of showing only "Waiting…" for up to five minutes (the question offered "加上提示，比如'网络出错，30 秒后重连（第 1/10 次）'" as the recommended option A), the user answered "A". Applied:

1. **What Runtime knows.** When the model layer schedules a transient retry it tells Runtime, which records on the Run `:retry` = `(:attempt N :limit M :delay SECONDS :at SEND-TIME :error SUMMARY)`, sets `:model-phase` to `retry-wait`, and sends the Sink the new status `retrying`. SUMMARY is `HTTP NNN` when the status is known, else the first line of llm's error text cut to 80 characters; it never contains a response body. When the retried request is sent, Runtime clears `:retry`, sets `:model-phase` to `waiting-first-token` and sends `waiting`. A terminal Run never carries `:retry`.
2. **What the surfaces show.** The chat window's status line, refreshed every second, reads `Provider error; retrying in Ns (N/M)… · SUMMARY` and counts down; the header's phase reads `retrying N/M`. Inline ask and inline act show `superchat inline · Provider error; retrying in Ns (N/M)…` from the moment the retry is scheduled (not counted down), and the rewrite preview `REWRITE  Provider error; retrying in Ns (N/M)…`; each goes back to its waiting line when the retry is sent. The seconds shown are rounded up. emacsclient keeps reporting `waiting` for a retry wait, so its external status set is unchanged.
3. **Nothing else changes:** when to retry, how often, the chat transcript of a Run with no retry, and the final error after the last retry.

## Third amendment by the user (2026-10-29): a dropped HTTP/2 connection

Measuring how a DeepSeek reply dropped midway reaches Superchat ([research](../../research/deepseek-drop-curl-codes.md)) found that Superchat's requests go over HTTP/2 (plz leaves the protocol to curl, and `api.deepseek.com` offers HTTP/2), and that there curl reports a drop as one of four exit codes: 92 (the stream was reset), 16 (an HTTP/2 framing error: a GOAWAY, or a TLS close before the response headers), 18 (a partial transfer: the same two after the headers) or 56 (a receive failure). Only 56 was in the narrowed list above. Offered four next steps with the first recommended — retry the other three codes — the user answered "同意". Applied:

1. **Also retryable:** `curl error code` 16, 18 and 92. plz 0.9.1 reports only exit codes 1 to 90 as curl errors (`plz.el:890`); 92 reaches Superchat as plz's own sentence `Unexpected curl process status:"exited abnormally with code 92⏎" code:92.  Please report this bug to the `plz' maintainer.` (⏎ is a real newline). It is recognized by its start and by ` code:92.  Please report`. The same sentence with any other code is not, and neither are `curl process killed` and `curl process interrupted`, which cancelling a request produces.
2. **Unchanged:** a retry still happens only when the failing attempt delivered no text and no reasoning, at most 10 times in a row, 30 seconds apart, the count starting over after a reply goes through. Tool-call data received before the drop does not stop a retry, and need not: llm.el runs tools only after the whole reply has arrived and been parsed (`llm-provider-utils.el:477-519`, the tools run from `:on-success`), and Superchat shows no part of a tool call while it streams, so a resend repeats nothing.
3. **The same caveat as the narrowing above:** the match depends on the wording of llm 0.32.1 and plz 0.9.1. Two tests in `scripts/transient-retry-regression.el` run llm.el's and plz's real error chain with a stand-in curl that exits with the code, streaming and not, so a change of wording fails a test instead of silently stopping the retry.
4. **Not covered:** an HTTP/2 stream reset with NO_ERROR after the response headers. curl then exits 0 with a short body, plz raises `json-end-of-file` in its own timer, and no callback runs; the attempt waits out its inactivity window and is then handled by the [silent-attempt retry](2026-10-29-a-silent-attempt-is-sent-again.md).

A code that in fact comes back on every request — a broken proxy between Emacs and the provider, say — is retried 10 times, about five minutes, before the error shows. That was already so for 56.

Verified against a local TLS server that drops the reply in 16 HTTP/2 ways (`.scratch/arc-bench/tools/drop-emulator.py`), with the three curls on this machine, through llm.el and plz: 45 of the 48 drops are now retried; the other 3 are the stream reset with NO_ERROR after the headers. Not yet seen against a real DeepSeek drop over HTTP/2.

## Fourth amendment by the user (2026-10-30): a reply body cut short

Asked to go on with candidates 2 to 4 of the [research](../../research/deepseek-drop-curl-codes.md) ("继续断开研究里的 2 到 4"), candidate 3 being to catch, on Superchat's side, the case point 4 of the third amendment left uncovered. Applied:

1. **The body that does not parse reaches the error callback.** plz-media-type parses a JSON reply inside the success callback it gives plz, so a `json-error` there escaped in plz's timer. `superchat--llm-plz-report-unparsed-body`, around `plz`, wraps that callback for the requests llm.el starts for Superchat only (`superchat--llm-plz-delivered` is bound while `llm-request-plz-async` starts one): a `json-error` raised before llm.el was handed the reply goes to plz's ELSE as `Unparsed response body: CONDITION`. A parse error raised after llm.el had the reply, and every request made outside Superchat, are left as they were.
2. **It is retried.** `Unparsed response body: (json-…` is a transient error, under the same rules as above: nothing of the attempt was delivered, at most 10 times, 30 seconds apart. That covers a body cut short (`json-end-of-file`) and a 200 whose body is not JSON at all (`json-parse-error`), such as the ARC bench proxy's 502 text after DeepSeek's 200. Both used to wait out the inactivity window and be sent again by the silent-attempt retry; they are now sent again 30 seconds after the error. A misconfigured endpoint that answers 200 with HTML is retried 10 times, about five minutes, before the error shows; before, it waited three inactivity windows.
3. **Verified:** `scripts/transient-retry-regression.el` runs a stand-in curl that writes a 200 and a JSON body cut short and exits 0, through llm.el's and plz's real chain, without streaming (red before the change: no callback, the execution never ended); a unit test pins what the wrapper takes and leaves. Against the local TLS server, the HTTP/2 stream reset with NO_ERROR after the headers now reaches the error callback and is retried with all three curls.
4. **Not covered: streaming.** With `text/event-stream` a stream cut inside an event ended the execution with `[Missing final response from llm]`, not retried; a stream cut after a whole event and before `[DONE]` was taken as a complete answer, showing only the text that arrived. Decided below.

Upstream reports for plz (exit codes above 90) and plz-media-type (a parse error calls neither THEN nor ELSE) are drafted in `.scratch/issues/2026-10-30-upstream-*.md`, not posted.

## Fifth amendment by the user (2026-10-30): a DeepSeek stream without `[DONE]`

The user took the recommendation ("按照建议"): a streamed reply that did not receive `[DONE]` did not finish. Applied, for DeepSeek only, because that is the provider measured; one real stream ends in `data: [DONE]`, and another OpenAI-compatible server need not send it:

1. While llm.el starts a streaming request to DeepSeek (`superchat--llm-stream-needs-done`), `superchat--llm-watch-stream-end` notes the `[DONE]` event. A stream that ends successfully without it reaches the error callback as `Reply stream ended before [DONE]`, instead of llm.el's success callback.
2. It is a transient error under the same rules: nothing delivered, it is sent again 30 seconds later, at most 10 times. Text delivered: nothing is sent again, the execution fails, and the chat keeps the text with `[Reply stream ended before [DONE]]` under it. Reasoning delivered without text: the execution fails, as for any transient error after reasoning.
3. **Verified:** `scripts/transient-retry-regression.el`, a stand-in curl through llm.el's and plz's real chain: cut inside the first event (sent again), cut after a whole event (fails, text and line kept), 300 events and `[DONE]` in one chunk with curl gone at once (not a cut, 5 of 5 runs), another provider without `[DONE]` (unchanged). Five mutants of the change are caught. Three real DeepSeek streams through llm.el, plz and curl ended without an error.
4. **Not covered:** another provider's stream cut short is still taken as complete. An error after partial text from any other cause (a curl exit code, say) still fails the execution with the text and without the reason in the chat. A streamed drop against the local TLS server was not run; the stand-in curl writes what curl writes.

## Sixth amendment by the user (2026-10-30): the reason stays under partial text

The user took the recommendation ("按照建议"). An error after partial text, from any provider and any cause, failed the execution and showed the text alone, which reads like a whole answer. The chat now shows the text with `[API error: REASON]` under it, the line a failure without text already shows; this replaces the `[Reply stream ended before [DONE]]` line of the fifth amendment, and point 4's "without the reason" no longer holds.

The answer of a failed execution is part of the next request's history, so the model sees the same two parts as one assistant message; it already saw `[API error: REASON]` alone when no text had arrived. `scripts/transient-retry-regression.el`: two tests, red before the change.

Real DeepSeek, asked by the user ("接真实服务商看看"), through Superchat's chat, llm.el, plz and curl, with tools off so the reply streams, and a wrapper that cuts curl's output after a set number of events and exits with a set code (`.scratch/arc-bench/runs/real-partial-20261030/`):

| Provider type | Cut | Exit | What happened |
|---|---|---|---|
| `llm-deepseek` | none | 0 | Completed, whole answer |
| `llm-deepseek` | after text | 92 | Failed; text with `[API error: …code:92…]` under it. Asked next turn, the model said its answer was cut off |
| `llm-deepseek` | after text | 0 | Failed; text with `[API error: Reply stream ended before [DONE]]` under it |
| `llm-deepseek` | after reasoning, default thinking | 0 | Failed with that line; not sent again, because reasoning was delivered |
| `llm-deepseek` | before any event | 0 | Hung, then fixed as below: sent again, completed |
| `llm-openai-compatible` | after text | 92 | Failed; text with the reason under it |
| `llm-openai-compatible` | after text | 0 | Completed with 35 of the 60 numbers and no sign of the cut (the fifth amendment's limit) |
| `llm-openai-compatible` | none | 0 | Completed, whole answer |

The hung case is new. With headers and no body, plz-event-source has opened no source, so closing it raises `cl-no-applicable-method`, in plz's timer and again in plz-media-type's ELSE, and no callback runs, for any provider. `superchat--llm-plz-report-unparsed-body` now takes that one error and calls the request's `:on-error` with `Unparsed response body: (cl-no-applicable-method plz-event-source-close nil)`, which is transient. Two tests in `scripts/transient-retry-regression.el`, red before the change. The wrapper cuts on purpose; a drop by DeepSeek itself has still not been observed.

