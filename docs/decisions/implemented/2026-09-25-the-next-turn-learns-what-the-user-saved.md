# Decision: the next turn learns what the user saved

Status: implemented — seen in both M10 slice 1c attended runs on product `6e9a73ad`: both undo requests carried the note and both undo answers described the disk correctly ([acceptance report](../../reports/2026-09-25-m10-1c-acceptance.md)); the leader took its place on the user's follow-up list (2026-09-25, "这几个问题继续让子代理处理") as the go-ahead to close it. Decided — user, 2026-09-25: "A". The user did not answer the sub-question; the leader took the recommended A1 (any save counts). Issue `.scratch/issues/2026-09-25-the-next-turn-is-not-told-what-the-user-saved.md`.

## Problem

- In the attended run `history-messages-attended-act-agent-01` (`.scratch/m5-config-acceptance/runs/history-messages-attended-act-agent-01/REVIEW.md`, statement 3) the user saved `init.el` on the review screen after the apply turn. The undo turn's request said nothing about it; every mention of saving there said "unsaved" or "does not save". The model read the disk, found the applied text, and explained it with an invention: "your original file was already in this state … it was like this when the session started".
- In `m10-1b-attended-act-agent-02`, the same situation, the model described the disk correctly. The gap does not force the error; it leaves the model to guess.
- Code facts: the review screen's `s` (`superchat-changes-review-save`) and `superchat-changes-save` call `save-buffer` and append nothing to the Tape (`superchat-changes.el`); a save with `C-x C-s` does the same. `superchat-changes-revert`, the user discarding the model's unsaved edits, is the same kind of event and is not recorded either. The reproduction `.scratch/issues/2026-09-25-the-next-turn-is-not-told-what-the-user-saved.el` shows turn 2's request mentions saving only as "unsaved".

## Decision

1. **A save note.** When the user saves a buffer that a Session's tools edited and left unsaved — it is one of that Session's review candidates just before the save — each such Session's Tape gets one admitted-material row (`attachment.added`, actor `runtime`, topic `inject`, source `(:kind user-save)`), outside any Run or Turn, whose text is "The user saved BUFFER (FILE); the file on disk now holds the buffer's text." Any save counts (A1): `C-x C-s`, the review screen's `s`, `superchat-changes-save`; a save made while a tool runs does not (the model's `save_buffer`, or code it evaluated), since that is not the user.
2. **A discard note.** When the user throws those unsaved edits away — `revert-buffer`, including `superchat-changes-revert`, or, for a new file that was never saved, `superchat-changes-revert` killing the buffer — the text is "The user discarded your unsaved edits to BUFFER (FILE); the buffer holds the file's text again." (for a new file: "…; the file was never written.").
3. **Once per settled edit.** After the note the buffer no longer carries that Session's edit marks, so a later save of the same buffer is not reported until the model edits it again.
4. **How the model gets it.** Like any admitted material: a user message `[Injected material] TEXT` among the earlier turns, in Tape order, so in the next Turn it comes after the previous answer and before the new question; from then on it is history, selected and accounted like any raw item, and the request evidence rebuilds it. The system text and the tool definitions do not change; the request prefix stays append-only.
5. **Off with the `changes` row.** When the `changes` plugin row is unmounted, nothing is recorded.

## In one paragraph

superchat's model edits buffers and never saves; saving is the user's. If nothing recorded the user's save of a buffer the model edited, the next turn would only have the model's own earlier words — "unsaved", "the disk still contains the old text" — and a model that then read the disk would find new text with no explanation. It is like leaving a colleague a note "I haven't posted the letter", posting it yourself, and not telling them: when they find the letter gone, they guess. So the next turn is told, in one short line, which of the buffers it edited the user saved or discarded since the last turn.

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

## Alternatives considered

**B. Nothing; the model reads the disk.** The product as it was, as Pi does for files. The model can always read the disk and the buffer; statement 3 is its inference. Cost: none. Risk: the error recurs whenever the model explains a difference it was not told about. It lost because it leaves the model to guess, and one attended run showed the guess going wrong.

**C. A state line in every request.** The per-request runtime context (`superchat-sandbox-runtime-context-text`, already sent with each request) gains a line listing the buffers this Session edited that are still unsaved, so the model always sees the current state rather than an event. Covers every way of saving without a hook. Cost: one line in every request while such buffers exist. That text sits in the current user message, before the Run's own tool rounds, so it must be fixed for the whole Run (a save during a Run is then told only on the next Turn); neither reference re-states file state on every request. It lost to A on cost: A's note is sent once, and only when something was saved or discarded.

**A2. Only the review screen's `s` and `superchat-changes-save` count.** No save hook on the edited buffers; a save with `C-x C-s` goes untold. It lost to A1 because the model's picture is wrong the same way whichever key the user pressed.

## Consequences

- The note is new model-visible material, so the [context contract](../../design/context-continuity-contract-v0.md) carries it as amendment 2026-09-25 (b): admitted material between Turns, an `attachment.added` row with no Run or Turn id, rendered, selected, accounted and rebuilt like any admitted material.
- Delivery is a durable Tape row written at the save or discard (`superchat-runtime-admit-between-turns`, source `(:kind user-save)`), not `superchat-runtime-inject`'s process-local queue, so a note survives a restart and does not depend on the next Turn's execution Scope.
- Which buffers count is decided by the edit marks `superchat-changes-candidates` already reads (`superchat-changes--marks`); a save or revert clears that Session's marks from the buffer (`superchat-changes.el`, `before-save-hook`, `after-save-hook` and the revert hooks).
- Offline coverage: `scripts/user-saves-regression.el` (8 tests); state in [the roadmap](../../roadmap.md).
