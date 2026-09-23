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
