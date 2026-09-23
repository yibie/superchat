# Proposal: one finish check when a tool-using Run ends on an unfinished-looking reply

Status: accepted (user, 2026-09-23: "按照你的建议", after the codex review), in the narrowed form below; built after the tool-material decision.

## Problem

A model working with tools sometimes replies with text only, announcing a step it then does not take: "Verification passed. Let me also confirm the file on disk still holds the old text…". superchat treats any reply without a tool call as the final answer (`superchat-llm.el:3076-3127`; `superchat--llm-tool-round-p`, `1841-1860`), so the Run ends there and the user gets half a sentence. Recorded four times, all DeepSeek, non-streaming (`.scratch/m5-config-acceptance/issues/03-…`; the fourth in the M5 reference comparison, slot 2 undo). The raw body of the latest (`runs/m8-attended-act-deepseek-01`, apply, request 9) has no tool call and `finish_reason: stop`: the model did not emit the call, nothing dropped it. Consequence in that run: the half sentence became the turn's answer, and the next turn could not know what the model had observed.

## What the three codebases do (read in full by three readers; the leader re-checked each cited line)

- **Decision rule — identical everywhere.** superchat, DeepSeek Harness (`packages/core/agent-loop/src/agent.ts:469-470`) and pi (`packages/agent/src/agent-loop.ts:222`) all end the turn when a reply has no tool-call block. None inspects the text; none detects an unfinished reply by default.
- **The seam before the end — present everywhere.** DeepSeek Harness runs `agent/turn-stopping` once before closing a turn; a listener that steers makes the loop take another step (`agent.ts:300-312`). pi's durable harness runs `before_run_end`, whose `{ followUp }` is committed as a user message and continues the run (`harness/runtime/drive/boundary.ts:160-206`). superchat already has the same seam: before finalizing a text-only reply it calls `turn-stopping` and, when a steer or injection is pending, sends one more attempt (`superchat-llm.el:3076-3106`, `superchat-runtime.el:1471-1496`, pinned by `scripts/m3-regression.el:1709-1829`).
- **No reference ships a content check on that seam.** DeepSeek Harness's only built-in driver past a stop is the goal loop: with an armed goal, each turn end gets a fixed `<goal_round>` prompt telling the model to inspect state, verify, and mark the goal complete (`packages/goal/goal-round-driver/src/prompt.ts:13-27`, cap 256 rounds). Its Stop-hook bridge can force continuation but has no cap and cannot see the reply text. pi registers no `before_run_end` handler.

## Evidence for a trigger

All recorded real-model final answers of Runs that used tools (39, from `.scratch/**/evidence/assistant.txt`, calibration runs excluded): the four half answers are 159, 265, 308 and 423 characters and each ends with a sentence in which the model announces its own next action ("Let me…"). Every other answer is 718 characters or longer and opens with a summary. A trigger "last sentence announces the model's own next action" catches 4/4 and fires on 0 real answers; "shorter than 600 characters" catches 4/4 and fires only on two empty answers, which are broken too. The sample is one task family (key-binding configuration); a correct answer to a lookup can be one line, so the length rule alone would re-check such answers. This is run history, not a rate.

## Decision (proposed; superseded in part by the review below)

At superchat's existing `turn-stopping` seam, add one finish check:

1. It applies only when the Run has made at least one tool call and the model's reply has no tool call.
2. It fires when the reply is empty, shorter than `superchat-llm-finish-check-min-chars` (default 600), or its last sentence announces the model's own next action (English "Let me…", "I'll…", "Now I will…", "Next, I…"; Chinese "让我…", "我先/再/来/将…", "接下来我…"; "Let me know" excluded).
3. It fires at most once per Run and counts as one tool round against the existing budget. If the model's next reply again has no tool call, that reply is final, whatever it says.
4. The injected message, as a user-role note like the existing tool-failure feedback: "Your last message has no tool call, so the task would end here. If a step you described is still undone, do it now with a tool. If everything is done, give your final report: what you changed and where, what is unsaved, and what you did not verify."
5. It is recorded on the Tape (one `run.finish-checked` event with the trigger that fired) and can be turned off with `superchat-llm-finish-check` (default on).

Not proposed: a required completion tool, a goal loop, a second model call to classify the reply, any change to "a reply without tool calls is final" when the check does not fire.

## Alternatives considered

- **Always one confirmation after any tool-using Run.** No wording rule, catches everything, but costs one extra request and a repeated report on every tool task, including the long correct ones (33 of the 39 above; the other six are the four half answers and two empty ones).
- **A required `finish` tool (as Cline's `attempt_completion`), or DeepSeek Harness's goal loop.** Robust, but a protocol change for every model and preset, and a model that never calls it hangs or loops. Worth it for long unattended goals, not as the default stop rule.
- **Do nothing; fix only what the next turn remembers.** Leaves the user with half an answer.

## Acceptance (scripted transport)

A half answer after a tool call gets one continuation and the Run ends on the model's next reply; a long final answer gets none; a short correct answer gets one and the Run then ends; a second half answer in the same Run is final; a Run with no tool call never triggers; the option off never triggers; the Tape records the check. Canonical smoke unchanged; M3 green. Real-model check on DeepSeek afterwards.

## Risks

A false fire costs one extra request and a repeated report. A half answer that is long and does not end by announcing an action is still missed. Wording rules are English and Chinese only.

## Review (2026-09-23, three codex reviewers, leader re-checked the cited lines)

Record: `.scratch/discussions/2026-09-23-finish-check-and-tool-material-review.md`. All three found the failure real and the draft not implementable as written. Agreed corrections, each verified:

- **Drop the length trigger.** A correct answer to a lookup, a refusal or "12/12 passed" is short; the 600-character rule would re-ask them, and a false fire is not only a wasted request: the model may go on to change things. Keep only an empty reply and a last sentence in which the model announces its own action not yet taken, with exclusions tested (quoted text, code, "let me know", requests addressed to the user). Call it what it is, a recovery for an announced step not taken, not completion detection.
- **Not on the listener.** `turn-stopping` listeners receive the Run and the Turn, not the reply text, and are notified once per Run (`superchat-runtime.el:1471-1492`). The check belongs in the text-only branch of `superchat-llm.el` (3076-3127), after the existing pending-input recheck: a pending steer or injection wins and the check does not also fire.
- **Never during budget finalization.** When the round budget has asked for a final answer, tools are gone and the model was told to report; the check must not fire, must not extend the budget, and must not fire on a cancelled or terminal Run (`superchat-llm.el:2990-3024, 3187-3195`).
- **Through the Step, on the record.** The note must reach the model the way steering and injections do, so the request evidence (M1) contains it; a `run.finish-checked` event alone cannot reconstruct the extra message, and a new event type must be registered (`superchat-tape.el:72-90`). The note must also say not to retry a call the user refused.
- **Empty is not missing.** An explicit empty reply and a missing response take different paths today (`superchat-llm.el:3107-3127`); a missing response stays a failure.
- **Scope.** Interactive chat and act first. Inline act only after its overlay handles an intermediate reply without showing Ready and restarting; not subagent, workflow, Org Babel, Magit or headless consumers, whose protocols are not "report what you changed".

Reviewers differed on shipping: one would ship the narrowed check for chat and act, two would ship it only as an opt-in trial first.

## Settled by the user (2026-09-23)

The narrowed check, on by default for the chat window and act only: it fires on an empty reply or a last sentence announcing the model's own action not yet taken (no length rule); it is decided in the text-only branch of the model layer after pending input; never during budget finalization or on a cancelled or terminal Run; at most once per Run; the note reaches the model through the Step so request evidence contains it, and tells the model not to retry refused calls; one user option turns it off. Inline act joins later, once its overlay handles an intermediate reply.

## Narrowing recorded at implementation (2026-09-23)

The settled terms say the note reaches the model "through the Step so request evidence contains it". The first half holds; the second cannot today, for a reason older than this decision: request evidence contains no material a Step claims mid-Run, for any steer or injection (`.scratch/issues/2026-09-23-request-evidence-omits-step-claimed-material.md`). What is guaranteed: the note is queued through `superchat-runtime-steer`, marked as system-originated (`finish-check`), claimed by the next Step, delivered to the model as a user-role message, and recorded on the Tape (`run.steered` and the admitted material, with its text). What is not: `superchat-tape-request-evidence` rebuilding the continuation request with the note in it. That is the same for every user steer; fixing it is a separate decision. This is a narrowing, not an equivalent implementation.

Lifted on 2026-09-23 by [request evidence includes Step material](2026-09-23-request-evidence-includes-step-material.md): the continuation request's evidence now contains the note (`finish-check/request-evidence-omits-step-material` passes without its expected-failure marker).
