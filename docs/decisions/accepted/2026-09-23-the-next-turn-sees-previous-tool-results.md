# Proposal: the next turn sees what the previous turns' tools returned

Status: accepted (user, 2026-09-23: "按照你的建议", after the codex review). Built first; the finish check follows. The mechanism below is settled; its exact contract is written into `docs/design/context-continuity-contract-v0.md` and the Tape event specs before code.

## Problem

A new turn's prompt carries each earlier turn's user message and final assistant text, plus admitted injections and compaction Anchors, but never tool rows (`superchat-view--derive-message`, `superchat-tape-view.el:714-733`; tool rows are never messages). Whatever the model observed through tools survives only if its final text restated it. In `runs/m8-attended-act-deepseek-01` the apply turn observed that the key was bound to `ignore`, ended on a half sentence, and the undo turn, not knowing the original value, left the key unbound.

The Tape cannot supply the missing facts today. On the ordinary path a `tool.requested` event records the tool name and `:argument-count`, and a `tool.completed` event records `:result-size` and `:result-hash` (`superchat-tool-execution.el:295-302`); neither the arguments nor the result text are stored. (The `epistemic-learning-v0` evidence path does store exact arguments and results, `superchat-tool-execution.el:271-296, 535-541`; ordinary tool calls do not take it.) Part of the reason is recorded in code: Tape summaries must never contain tool arguments or results because they are not redacted (`superchat-tape.el:772-775`). No accepted decision says ordinary tool bodies must never be stored.

## What the references do

DeepSeek Harness stores every `tool/result` in its append-only session log and re-sends all earlier user messages, assistant messages (with their tool calls and intermediate text) and tool results on every request, shrinking only under context pressure: compaction at 80% of the window, a pruner that cuts results over 8 KB to 4 KB head and 1 KB tail, spilling of results over 50 KB to a file (`packages/core/session/src/surface.ts:20-26`, `packages/compaction/*`, `packages/spill/spill-policy`). pi persists every message including tool results and re-sends them all until compaction, whose summary keeps each tool result to 2000 characters (`packages/coding-agent/src/core/compaction/utils.ts:84-155`). Neither has a model-facing recall tool in its default composition.

## Decision (proposed; superseded in part by the review below)

1. **Store what the tools returned, bounded.** `tool.requested` records the arguments and `tool.completed` / `tool.failed` record the result text, each cut to `superchat-tape-tool-material-max-chars` (default 2000; pi uses 2000 to serialize results for its compaction summarizer, not as a storage bound) with a marker saying how much was cut, and passed through the Tape's existing secret redaction (`superchat-tape--redact`). Old rows are not rewritten.
2. **Put it in later turns' context.** The Context View treats these as prompt material, rendered as one `Tool:` line per call ("NAME(ARGS) → RESULT"), under the same budget and the same newest-first contiguous-suffix rule as user and assistant rows. Intermediate assistant text is not stored today and stays out; that is a separate decision.
3. **Contract first.** `docs/design/context-continuity-contract-v0.md` gains the tool evidence class and its rendering before code.

Not proposed: a recall tool; storing results unbounded; changing what the current Run sees.

## Alternatives considered

- **Only the previous turn's tool results.** Solves the undo case with less history, but keeps a rule neither reference has, and loses older observations one turn later.
- **Do not store; ask the model to restate before-values in its final report** (one more line in the operating note). No storage change, but it depends on the model, and it failed exactly when the final report was a half sentence.
- **Keep observations in memory for the live Emacs only.** Breaks the rule that model-visible history is derived from the Tape (M1), and is lost on restart.

## What changes for the user

The local database keeps up to 2000 characters of every tool result, which includes file text the model read and results of evaluated Lisp; secrets in conventionally named fields are redacted as elsewhere on the Tape. History in later turns contains `Tool:` lines, so older user and assistant rows leave the budget sooner in long sessions; compaction handles that as it does now.

## Acceptance

Scripted: after a turn whose tool read "ignore", the next turn's prompt contains that tool line; a result longer than the bound is cut with its marker; a secret-named field is redacted; old rows without stored material render as today; the budget still yields a contiguous recent suffix. Real-model: the act entry's apply/undo on DeepSeek, where undo must restore `ignore`.

## Review (2026-09-23, three codex reviewers, leader re-checked the cited lines)

Record: `.scratch/discussions/2026-09-23-finish-check-and-tool-material-review.md`. All three found the cross-turn gap real and the draft not implementable as written. Agreed corrections:

- **Privacy is the first decision, not a detail.** `superchat-tape--redact` redacts by field name; a secret inside file text or an `eval_elisp` string is stored as is and sent to the model again. Storing tool bodies needs an explicit user decision on persistence and a way to turn it off, and the redactor must not be described as a secret filter.
- **Fit the Tape's own bound.** A payload over `superchat-tape-max-inline-bytes` (16384 bytes) is replaced wholesale by size and hash (`superchat-tape.el:24-29, 378-390`); 2000 characters of arguments plus 2000 of result can be up to about 16 KB in UTF-8, so the bound must be in bytes and leave room.
- **Provenance and pairing.** A tool line must be derived from identified rows (execution id, call identity, succeeded / failed / refused, truncation flag), never paired by adjacency, and a failure must never read as an observation.
- **M1 and compaction.** The rows a tool line comes from must be referenced by `request.message-list` so a request can be rebuilt; compaction input and Anchor source ids must include them, or the observation is lost again after compaction.
- **Untrusted text.** Tool output is external data; it must be framed as a past observation, not as an instruction.
- **Visibility is selection, not a promise.** Under the budget and the newest-first rule, a tool line is selectable in later turns, not carried in every turn.

Reviewers differed on scope: two chose storing with provenance for all later turns, one chose exposing it to the next turn only, and all three said storage and privacy must be settled either way. Two of three would do this proposal before the finish check.

## Settled by the user (2026-09-23)

1. **Stored by default, bounded, one switch.** Ordinary tool events keep the call's arguments and the result text, bounded in bytes so the whole payload stays under `superchat-tape-max-inline-bytes`, with a truncation marker and the original size. One user option turns storage off. The text is kept in the event payload, not in the row's `content`: summaries stay free of arguments and results, and full-text search does not index it.
2. **Selectable in all later turns.** Tool lines are prompt material for every later turn, chosen by the existing budget and newest-first rule; they are not promised in every turn.
3. **Everything the review required is part of the mechanism:** identified pairing by execution id with the call's status (succeeded, failed, refused, blocked) and a truncation flag; framing as a past observation of external data; request evidence (M1) and compaction include the rows a tool line comes from; the user is told plainly that file text and evaluated results are stored and that secrets inside text are not redacted.
