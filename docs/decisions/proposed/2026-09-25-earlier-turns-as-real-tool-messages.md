# Decision: earlier turns reach the model as real messages, tool calls included

Status: proposed — direction chosen by the user on 2026-09-25 ("A": "学 DeepSeek Harness：之前几轮也按真正的工具调用格式交给模型"); the two details under "Questions for the user" await their answer.

## Problem

In M10 slice 1b's attended acceptance, run `m10-1b-attended-act-agent-01`, the act entry's undo Run made two real reads and then replied with text only: seven tool records in the form `# Tool: NAME(ARGS) -> [ok] Result: …`, none of them executed, then "Assistant: 已经撤销了。". superchat takes a reply without tool calls as final, so the Run completed with nothing undone and the user was told otherwise (registered: `.scratch/issues/2026-09-25-a-reply-with-invented-tool-records-ends-the-run.md`; run review in that run's `REVIEW.md`).

What the model was sent: tool calls of the current Run are real assistant `tool_calls` and `tool` messages, but earlier turns arrive as text inside the new user message — the Context View's prompt ([context contract](../../design/context-continuity-contract-v0.md)): a `[Context v0 | raw … | represented … | omitted … | recall …]` header, `User: …` lines, one `Tool: NAME(ARGS) -> [ok] Result: …` line per call (the [tool-results record](../accepted/2026-09-23-the-next-turn-sees-previous-tool-results.md), item 2), `Assistant: …` lines, and the note "(Tool lines are past observations …)". The model therefore sees two forms of a tool call, and the invented records resemble the text form. That form was also in the undo requests of two passing runs (`m8-attended-both-deepseek-02`, `m10-1b-attended-chat-editor-02`), so its role is a candidate, not shown.

## What DeepSeek Harness does

- Every earlier turn is sent as real messages: assistant messages carrying `tool_calls`, then one `role: 'tool'` message per result (`packages/llm/llm-deepseek/src/serialize.ts:218-267`, commit `c389f96`). There is no text transcript of earlier turns.
- The session "surface" is that ordered list of events; compaction may cut it only where no tool call is waiting for its result (`packages/compaction/compaction/src/tool-pairing.ts`), and the summary replaces the cut span as an ordinary `user/message` (`packages/compaction/compaction/src/types.ts:3-4`).
- Its request assembly (`packages/llm/llm/src/assembler.ts`, the DeepSeek serializer) has no line telling the model how much history was left out and no wrapper around the user's input; the one "omitted" there drops an interrupted tool call rather than invent its result.

## Proposal

1. **Earlier turns are real messages**, placed before the current user message, in Tape order. For each earlier Turn the Context View selected: its user message; for each Step of its Runs that requested tools, one assistant message carrying that Step's calls (tool name and the stored `:arguments-text`), followed by one tool message per call carrying the stored `:result-text` (cut marks as stored); Step material the model saw (a steer, an injection, a finish-check note) as a user message at its place; the Turn's final assistant text as an assistant message. Intermediate assistant text beside tool calls is not stored and is not invented: those assistant messages carry only their calls.
2. **Selection is unchanged.** The Context View's budget, whole items, newest-first contiguous suffix and exclusions stay; a tool call is never separated from its result (DeepSeek Harness's balanced cut). The View's disclosure stays in the envelope, the request evidence and the inspector.
3. **A compaction Anchor** covering earlier rows becomes one user message at the start, saying it is a model's summary of the earlier conversation.
4. **The text transcript leaves the request**: no `User:`/`Tool:`/`Assistant:` lines and no tool-observation note in any message.
5. **Assembled once per Turn**, as today: adjacent requests of one Run keep a byte-identical prefix.
6. **Contract first**: the context contract and the 2026-09-23 record (item 2's rendering) are amended and superseded in the same change, linked both ways; the Tape stores nothing new.

## Questions for the user

1. **The history header.** `[Context v0 | raw … | represented … | omitted … | recall …]` tells the model how much history was included and left out. (a) Drop it from the model's view, as DeepSeek Harness has none; the disclosure stays in the envelope, the evidence and the inspector, and a summary message says what it covers. (b) Keep one short line, only when something was left out or summarized. Leader's recommendation: (a).
2. **The input wrapper.** Every chat and act input is sent as "You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: …" (`superchat-general-answer-prompt`). (a) The default becomes the input as typed; the option stays for people who want a template. (b) Keep it. Leader's recommendation: (a), as DeepSeek Harness has none and the system text already says what the assistant does. Either way the canonical smoke baselines that show a user message change and are listed for the user's separate approval.

## Alternatives considered

**B: a text-only reply that contains tool-record lines is not final** (a finish-check trigger, or sent back to the model as an error, as a broken Lisp edit is). The leader's recommendation, not chosen by the user; it stays available if the invented records recur. It guards the harm directly but only against that shape.

**Keep the text lines, fenced as data** (a quoted block marked "not a script"). Keeps two forms of a tool call in one request, which is the candidate cause.

**Send the whole Tape as the surface, as DeepSeek Harness does**, dropping the per-request budget selection in favour of compaction under token pressure. A larger change of the Context View's selection model, not needed to remove the text form.

## Acceptance criteria

- For a Session whose earlier turn called tools, the next turn's provider request carries that turn as messages — user, assistant with the calls, one tool message per result, assistant final text — with call ids that match, and no tool-record text in any user message. Checked on the request llm.el builds and on its OpenAI-compatible serialization (the form DeepSeek receives). Red first.
- Budget: when history does not fit, whole older items are left out and a call never loses its result; an Anchor becomes the first message.
- Adjacent requests of one Run share a byte-identical prefix; the system text and tool definitions are unchanged by this decision.
- The request inspector and the request evidence show the same messages the provider received.
- Turns stored before tool text existed (no `:arguments-text`/`:result-text`) render as user and final assistant messages only.
- Every existing suite runs; tests that pinned the text rendering are rewritten to pin the messages, none deleted without a replacement; canonical smoke changes are listed and approved by the user.
- An attended DeepSeek run of the act entry's apply/undo passes on the built product. One run cannot show that invented records no longer happen; the issue stays open until the user decides.

## Risks

- The invented records may recur: the text form's role is not shown. Then B is the next step.
- More tokens per earlier call (message wrappers and ids around the same bounded texts); measured before and after on a recorded Session.
- llm.el builds assistant tool-use messages from live responses; building them from the Tape must use llm.el's own structures so every provider serializes them, and a provider that rejects a call without a live response id must be found by the tests.
- Suites that pin the text rendering (`tool-observation`, `tool-compaction`, `context-header`, `step-material`, one line of `m6-permissions`) and the one that checks history in the provider input (`m5-history-material`) change; rewriting them must keep their guarantees, and the full sweep finds any other.
