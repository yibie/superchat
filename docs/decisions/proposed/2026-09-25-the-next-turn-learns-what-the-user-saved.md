# Decision: the next turn learns what the user saved

Status: proposed — design only, no code. Issue `.scratch/issues/2026-09-25-the-next-turn-is-not-told-what-the-user-saved.md`. Open for the user's decision.

## In one paragraph

superchat's model edits buffers and never saves; saving is the user's. When the user saves a buffer the model edited, nothing records it, so the next turn only has the model's own earlier words — "unsaved", "the disk still contains the old text" — and a model that then reads the disk finds new text with no explanation. It is like leaving a colleague a note "I haven't posted the letter", posting it yourself, and not telling them: when they find the letter gone, they guess. The proposal is to tell the next turn, in one short line, which of the buffers it edited the user saved since the last turn.

## Problem

- In the attended run `history-messages-attended-act-agent-01` (`.scratch/m5-config-acceptance/runs/history-messages-attended-act-agent-01/REVIEW.md`, statement 3) the user saved `init.el` on the review screen after the apply turn. The undo turn's request said nothing about it; every mention of saving there said "unsaved" or "does not save". The model read the disk, found the applied text, and explained it with an invention: "your original file was already in this state … it was like this when the session started".
- In `m10-1b-attended-act-agent-02`, the same situation, the model described the disk correctly. The gap does not force the error; it leaves the model to guess.
- Code facts: the review screen's `s` (`superchat-changes-review-save`) and `superchat-changes-save` call `save-buffer` and append nothing to the Tape (`superchat-changes.el`); a save with `C-x C-s` does the same. `superchat-changes-revert`, the user discarding the model's unsaved edits, is the same kind of event and is not recorded either. The reproduction `.scratch/issues/2026-09-25-the-next-turn-is-not-told-what-the-user-saved.el` shows turn 2's request mentions saving only as "unsaved".

## What the references do

Neither has a buffer layer or a review screen: their tools write files directly. The comparable question is how the model learns that something outside it changed between turns.

**DeepSeek Harness** (`~/Documents/reference-code/deepseek-harness`, `c389f96`):

- *Files the model edits*: it does not tell the model about outside changes. It stops a stale write instead. The optional `fs-observation-policy` plugin records the version of each file the session read (`packages/fs/fs-observation-policy/src/index.ts:21-100`); a later write or edit of a file that changed since that read fails with `FS_STALE_VERSION`, "file changed since it was read" (`packages/fs/fs-local/src/index.ts:188-191`, `:241-247`), and the model is told "— re-read the file, then retry" (`packages/fs/tool-fs/src/error.ts:30-31`). "A session resumes with no observed state" (`packages/fs/fs-observation-policy/README.md:42`).
- *Files that shape the model's instructions*: it does tell the model. When an `AGENTS.md`/`CLAUDE.md` it loaded has changed, the next request carries a `<system-reminder>` "Updated instructions from: PATH — This file changed after it was loaded. Use the following content instead …" (`packages/context/agent-instructions/src/render.ts:171-183`, `:236-242`; README `:32`, `:185`), appended once as a durable message.
- *The pattern for such notes*: a plugin appends one source-attributed user message at the step boundary, stored in the session history (`packages/context/time-context/README.md:67`, `src/index.ts:216`), and the README states the cost: "Append-only; newly visible content follows the reusable request prefix and does not invalidate existing KV-cache entries" (`time-context/README.md:128`, `agent-instructions/README.md:179`).

**Pi** (`~/Documents/reference-code/pi`, `3e4bc26`):

- *Files*: no tracking. `edit` reads the file from disk at each call (`packages/coding-agent/src/core/tools/edit.ts:185-186`); nothing records what was read or tells the model a file changed.
- *What the user did outside the model*: recorded and sent. A shell command the user runs with `!cmd` is stored as a `bashExecution` message and reaches the model as a user message "Ran `cmd`" with its output (`packages/coding-agent/src/core/messages.ts:28-39`, `:82-100`, `:152-161`); `!!cmd` keeps it out.
- *Extension notes for the next turn*: `pi.sendMessage(message, { deliverAs: "nextTurn" })` queues a custom message that is sent with the next user prompt, as a user-role message after it (`docs/extensions.md:1422-1446`; `src/core/agent-session.ts:1270-1273`, `:1495-1496`; conversion `src/core/messages.ts:163-170`).

In short: both send a short, durable, append-only user-role note when something outside the model changed that the model's picture depends on (DeepSeek Harness for instruction files, Pi for the user's own shell commands); DeepSeek Harness additionally refuses a stale write. Neither re-states file state on every request.

## Options

**A. A save note for the next turn (recommended).** When the user saves — or discards — a buffer this Session's tools edited and left unsaved, the Tape records it (one fact per buffer: which buffer, which file, saved or discarded). The next Turn's request carries one user message, before its question and after the previous turn's answer (before the question, unlike Pi's `nextTurn` messages, because the save happened before the question was typed and earlier turns are sent in Tape order), such as `[Injected material] The user saved init.el (~/.emacs.d/init.el) after your last turn; the file on disk now holds the buffer's text.` It is part of the history from then on, like DeepSeek Harness's changed-instructions note and Pi's `!cmd` message.
  - Sub-question — which saves count: (A1) any save of such a buffer, including `C-x C-s`, which needs a save hook on the buffers the Session's tools marked; or (A2) only the review screen's `s` and `superchat-changes-save`. A1 is recommended: the model's picture is wrong the same way whichever key the user pressed.
  - Cost: one line per saved buffer, sent once, then kept in the history like any earlier message. No change to the system text or the tool definitions; the note is a new message after the last one the previous request carried, so the request prefix stays append-only.

**B. Nothing; the model reads the disk.** Keep the product as is, as Pi does for files. The model can always read the disk and the buffer; statement 3 is its inference. Cost: none. Risk: the error recurs whenever the model explains a difference it was not told about.

**C. A state line in every request.** The per-request runtime context (`superchat-sandbox-runtime-context-text`, already sent with each request) gains a line listing the buffers this Session edited that are still unsaved, so the model always sees the current state rather than an event. Covers every way of saving without a hook. Cost: one line in every request while such buffers exist. That text sits in the current user message, before the Run's own tool rounds, so it must be fixed for the whole Run (a save during a Run is then told only on the next Turn); neither reference re-states file state on every request.

## Recommendation

**A with A1**: record the user's save or discard of a buffer the Session's tools edited, and tell the next turn once. It matches both references' form (a short durable user-role note, appended), keeps the request prefix append-only, and adds nothing to requests where nothing was saved. B is the fallback if the user prefers no new fact.

## Questions for the user

1. Tell the next turn when you saved or discarded a buffer the model edited? **A** (recommended) / B (no) / C (a state line in every request).
2. If A: which saves count? **A1: any save, including `C-x C-s`** (recommended) / A2: only the review screen and `superchat-changes-save`.

## Technical notes (for the implementation, not for the decision)

- Record before code: the note is new model-visible material, so the [context contract](../../design/context-continuity-contract-v0.md) gains an amendment (a Tape event type, its payload, its rendering as an earlier-turn message, its selection and accounting), before any code.
- Delivery candidates: `superchat-runtime-inject` already queues identified material for a Session's next matching Turn, recorded as `attachment.added` and sent as `[Injected material] TEXT` at that Turn's first Step. It is process-local and matched by exact execution Scope (a save made before a restart, or from another Scope, would be lost), so a durable Tape fact read at the next Turn's assembly is the likelier form; that choice belongs in the amendment.
- Which buffers: the marks `superchat-changes-candidates` already reads (`superchat-changes--marks`) identify the buffers this Session's tools edited; the same marks can decide which saves count.
- `superchat-changes.el` is part of the M10 review-screen work; any change there is coordinated with it.
