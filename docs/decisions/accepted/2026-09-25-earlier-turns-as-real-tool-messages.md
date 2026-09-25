# Decision: earlier turns reach the model as real messages, tool calls included

Status: accepted — user, 2026-09-25: direction "A" ("学 DeepSeek Harness：之前几轮也按真正的工具调用格式交给模型"), then "按照你的建议" on both details (the history header leaves the model's view; the input wrapper's default becomes the input as typed); implementation state is in [the roadmap](../../roadmap.md).

## Problem

In M10 slice 1b's attended acceptance, run `m10-1b-attended-act-agent-01`, the act entry's undo Run made two real reads and then replied with text only: seven tool records in the form `# Tool: NAME(ARGS) -> [ok] Result: …`, none of them executed, then "Assistant: 已经撤销了。". superchat takes a reply without tool calls as final, so the Run completed with nothing undone and the user was told otherwise (issue `.scratch/issues/2026-09-25-a-reply-with-invented-tool-records-ends-the-run.md`; the run's `REVIEW.md`).

What the model was sent: tool calls of the current Run are real assistant `tool_calls` and `tool` messages, but earlier turns arrive as text inside the new user message — the Context View's prompt ([context contract](../../design/context-continuity-contract-v0.md), section 10): a `[Context v0 | …]` header, `User: …` lines, one `Tool: NAME(ARGS) -> [ok] Result: …` line per call ([the tool-results record](2026-09-23-the-next-turn-sees-previous-tool-results.md), item 2), `Assistant: …` lines and the note "(Tool lines are past observations …)", followed by the input wrapped as "You are a helpful assistant. Please provide a clear and comprehensive answer to the user's question: …". The model sees two forms of a tool call, and the invented records resemble the text form. That form was also in the undo requests of two passing runs (`m8-attended-both-deepseek-02`, `m10-1b-attended-chat-editor-02`), so its role is a candidate, not shown.

## Decision

1. **Earlier turns are real messages**, placed before the current user message, in Tape order (user: "A"). For each earlier Turn the Context View selected: its user message; for each Step of its Runs that requested tools, one assistant message carrying that Step's calls, followed by one tool message per call; Step material the model saw as a user message at its place; the Turn's final assistant text as an assistant message. Intermediate assistant text beside tool calls is not stored and is not invented: those assistant messages carry only their calls.
2. **The history header leaves the model's view** (user: "按照你的建议", detail 1a). The View's disclosure — covered, represented, omitted, recallable — stays in the envelope, the request evidence and the inspector.
3. **The input goes as typed** (detail 2a): `superchat-general-answer-prompt` defaults to `$input`; the option stays for a template.
4. **The text transcript leaves every conversation request**: no `User:`/`Tool:`/`Assistant:` lines, no tool-observation note, in chat, act and inline ask.

## How a Turn becomes messages

- **Calls and results.** A call's wire id is its Tape `:execution-id`, on the assistant message's call and on its tool message, so ids match and are the same in every request that carries them. The tool message carries the stored `:result-text`; a result that was cut ends with ` … [cut: N more characters]` from the stored original length.
- **Arguments.** A call is sent with its arguments as a JSON object keyed by parameter name. New `tool.requested` rows store `:arguments-text` in that form: the positional arguments named by the tool's parameter list at the time of the call. Rows stored earlier hold a positional JSON array, named at assembly by the tool's current parameter list. A call whose stored arguments do not read as a complete array or object (cut, dropped for size, or from a tool no longer defined) is sent with empty arguments, and its tool message begins with `(Arguments not kept in full: TEXT)`, TEXT being the stored text with its cut mark, or `none` when nothing was stored.
- **Step material** (a steer, an injection, a finish-check note) is a user message carrying `[Injected material] TEXT`, the line the contract's Step-material amendment already defines, so it is not taken for typed input.
- **A compaction Anchor** covering earlier rows is one user message at the start that says it is a model's summary of the earlier conversation — as DeepSeek Harness replaces a compacted span with a `user/message` carrying the summary.
- **Selection and accounting are unchanged**: the Context View's budget, whole items, newest-first contiguous suffix and exclusions; an item still costs its canonical rendered line; a call is never separated from its result. Only the form the provider receives changes.
- **Assembled once per Turn**, as today: adjacent requests of one Run keep a byte-identical prefix; the system text and tool definitions are unchanged.
- **Compaction's own request**, which asks a model to summarize a span, keeps the text rendering: it is material to summarize, not a conversation to continue.
- **Recovery after a request that is too long.** When compaction resumes the Run (the provider refused the request as too long, or the estimate before sending said it would be), the resumed request's earlier-turn messages are replaced by the messages of the View assembled after compaction; the current Turn's own messages stay as they were. Until now the new View's text replaced the old View's text inside the user message; with the history sent as messages that text is not there, so the replacement found nothing and the Run failed (found while building, 2026-09-25).
- **The request inspector** shows the earlier-turn messages read-only, between the system text and the editable prompt, and its Copy includes them. They are not part of the editable prompt, so editing the prompt neither removes nor changes them; until now an edited prompt was sent without the saved selection and therefore without any earlier turn (found while building, 2026-09-25). Whether one request may be sent without its earlier turns is the user's decision.
- **Evidence.** The request evidence names the same items; `superchat-tape-request-evidence` rebuilds the messages above, and the inspector shows them.

## What DeepSeek Harness does

- Every earlier turn is sent as real messages: assistant messages carrying `tool_calls`, then one `role: 'tool'` message per result (`packages/llm/llm-deepseek/src/serialize.ts:218-267`, commit `c389f96`). There is no text transcript of earlier turns.
- The session "surface" is that ordered list of events; compaction cuts it only where no tool call waits for its result (`packages/compaction/compaction/src/tool-pairing.ts`), and the summary replaces the span as an ordinary `user/message` (`packages/compaction/compaction/src/types.ts:3-4`).
- Its request assembly (`packages/llm/llm/src/assembler.ts`, the DeepSeek serializer) has no line telling the model how much history was left out and no wrapper around the user's input; the one "omitted" there drops an interrupted tool call rather than invent its result.

## Alternatives considered

**B: a text-only reply that contains tool-record lines is not final** (a finish-check trigger, or sent back to the model as an error, as a broken Lisp edit is). The leader's recommendation, not chosen; it stays available if the invented records recur. It guards the harm directly but only against that shape.

**Keep the text lines, fenced as data** (a quoted block marked "not a script"). Keeps two forms of a tool call in one request, which is the candidate cause.

**Name the arguments at assembly for every row** instead of storing new rows named. No change to what the Tape stores, but a tool whose parameter list changed between the call and a later turn would be shown with the wrong names; storing the names with the call records what was sent.

**Send the whole Tape as the surface, as DeepSeek Harness does**, dropping the per-request budget selection for compaction under token pressure. A larger change of the Context View's selection model, not needed to remove the text form.

**Keep one header line when something was left out** (detail 1b). Not chosen: DeepSeek Harness has none, and the disclosure stays where people read it.

## Consequences

- The [context contract](../../design/context-continuity-contract-v0.md) gains an amendment that replaces section 10's rendering for provider requests and the tool-observation rendering of its 2026-09-23 amendment; [the tool-results record](2026-09-23-the-next-turn-sees-previous-tool-results.md) item 2 is superseded by this record.
- `tool.requested` rows written after this change carry `:arguments-text` as a named JSON object; readers accept both forms.
- The canonical smoke baselines that show a user message or a second turn change (the wrapper and the history); each change is listed for the user's separate approval before a baseline is touched.
- Suites that pin the text rendering (`tool-observation`, `tool-compaction`, `step-material`) and the ones that check history in the provider input or the inspector (`m5-history-material`, `m5-request-context`) are rewritten to pin the messages; none loses a guarantee without a replacement. The new behaviour gets its own test file. (`context-header` pins the disclosure in the View's text, which is kept, and the `Tool:` line in `m6-permissions` is the approval prompt's own label; neither changes.)
- More tokens per earlier call were expected (message wrappers and ids around the same bounded texts). Measured 2026-09-25 on a recorded Session, the undo request of `m10-1b-attended-act-agent-01` (11 selected items, 9 of them calls): the old history text was 10,176 characters as sent (JSON-escaped); the same items as messages, serialized by llm.el, 9,681 characters. About 5% fewer by superchat's own estimate (serialized characters), because the header, the role labels and the doubly escaped newlines are gone; the provider's own token count was not measured.
- The invented records may recur, since the text form's role is not shown; the issue stays open until the user decides, and B is the next step if they do. An attended DeepSeek run of the act entry's apply/undo on the built product is the acceptance; one run cannot show that they no longer happen.
