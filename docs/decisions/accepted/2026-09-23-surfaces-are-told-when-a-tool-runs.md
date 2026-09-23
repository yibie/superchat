# Decision: every surface is told when a tool runs

Status: accepted (user, 2026-09-23: "1. 按照建议", choosing option A of `.scratch/issues/2026-09-22-no-surface-is-told-a-tool-is-running.md`).

## Problem

The Sink contract lists `calling-tool` among a Run's statuses (`superchat-sink.el:23`), but Runtime never sends it: a Sink receives `waiting` when the Run starts, stream chunks, and the terminal status. `:model-phase` in the Run status has no value for "a tool body is executing" either. So the inline act overlay, designed to show "Working: tool…", shows "Waiting…" through a whole tool-using task on a provider that does not stream tool-calling requests, and emacsclient's mapping of a `calling-tool` phase never matches.

## Decision

1. **When a tool body starts** (the moment tool execution reports `effect-entered` to Runtime), Runtime sends the Run's Sink the status `calling-tool` and sets the Run's `:model-phase` to `calling-tool`.
2. **When the Run's last open tool body settles** (`effect-settled` leaves no open effect), Runtime sends `waiting` and sets `:model-phase` to `waiting-first-token`: the Run is waiting for the model again. The model layer's own phase updates on the next attempt proceed as today.
3. **Only live, non-terminal Runs**, through the same Sink emission path Runtime uses for its other statuses (execution scope, live check, fault capture); a Sink fault here is recorded like any other Sink fault and does not fail the tool.
4. **Surfaces keep their own meaning:** inline act shows "Working: tool…" for `calling-tool`; inline ask keeps mapping it to "Typing…"; rewrite has no tools. **The chat window must not change**: its Sink ignores `calling-tool` today and must render exactly as before, including when `waiting` arrives mid-Run; the canonical smoke baseline stays byte-identical.

Not decided: showing which tool runs, an approval-specific status, or changing when `waiting` is first sent.

## Acceptance

Scripted transport: during a tool body the inline act overlay's header reads "Working: tool…" (observed while the body runs, not by calling the header function) and returns to "Waiting…" after it; two tools in one step send one `calling-tool` and one `waiting` after the last settles; a terminal Run receives nothing more; the Run status reports `:model-phase` `calling-tool` during the body; the chat window's transcript and the smoke baseline are unchanged. The test `inline-act/run/status-header-shows-working-during-a-tool-call` is strengthened to observe the real status rather than the header function alone.
