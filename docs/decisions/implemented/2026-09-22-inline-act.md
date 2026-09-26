# Decision: M8 — inline act: a task from the source buffer, finished and reported in place

Status: implemented — proposed by the leader on 2026-09-22 after the design-goal review (`.scratch/discussions/2026-09-22-design-goal-review.md`) and built the same day in three worker slices at the user's request ("你来提出一个新的方案，然后启用 pi 作为 worker agent"); the user accepted the design after it was built ("非常同意", 2026-09-22). Implementation state is in [the roadmap](../../roadmap.md#milestones).

## Problem

The second target journey (2026-09-10) is "invoke superchat on the buffer being edited, run a task, get feedback in place". Today that journey is split in two:

- `superchat-inline-ask` answers in an overlay in the source buffer, with four read-only tools (`superchat-select-tools.el:22-26`); `superchat-rewrite-region` previews a replacement of the region with no tools at all (`superchat-rewrite.el:327`).
- `superchat-act` captures the origin buffer, then hands the request to `superchat-submit`, which creates the chat buffer, displays it and runs the Turn there (`superchat.el:4107-4135`, `4078-4104`). The user who asked for work "right here" is moved to the chat window to watch it happen.

Three independent reviewers (2026-09-22) rated this the most visible gap between the product and its design goal, and all three named the same next step: one bounded path where a task is started from the source buffer, the tools run, approval and progress are shown where the user is, and the result lands there too.

What already exists and is reused, verified in the source:

- The approval question is `read-multiple-choice` in the minibuffer (`superchat-approval.el:108-135`), independent of the surface. An inline Run that calls an `ask`-class tool already asks the user in place; nothing new is needed for approval.
- Buffer edits are buffer-first (M7): `edit_buffer`, `append_to_buffer`, `replace_buffer`, `open_file` change the live buffer, mark it for `/changes`, and saving stays the user's (`superchat-changes.el`). An edit made from an inline Run lands directly in the buffer the user is looking at.
- The inline overlay, its Sink and its keymap (`superchat-inline.el:300-470`) already stream text, show a status header and offer reply / visit / clear / copy.
- A surface is a plugin row that registers its name and loads its code (`superchat-default-plugins.el:48-56`); the tool baseline for an unlisted surface is the ordinary default baseline (`superchat-select-tools.el:78-80, 224-239`).

## Decision

Add one surface, `inline-act`, and one command, `superchat-inline-act`: an instruction typed at point in any buffer runs a full tool Turn (default tool baseline, ordinary permission levels and approval) and reports in an overlay at point. The chat window is not opened. Follow-ups, cancel, and a review of what the Run edited are reachable from the overlay.

Explicitly not proposed: any change to `superchat-act` (it keeps sending to the chat window), any new tool, any change to permission levels or approval, any change to the inline-ask baseline, any attempt to judge task success for the user.

## Mechanism

1. **Row and surface.** A new row `inline-act` in `superchat-default-plugins.el`, modelled on `inline-ask`: `:apply` requires `superchat-inline-act`, registers surface `inline-act`. It joins the default profile; `headless` disables it as a user-interface row. The entry point refuses without its row, as the other surfaces do.
2. **Command.** `superchat-inline-act INSTRUCTION` (new file `superchat-inline-act.el`) reads an instruction from the minibuffer, takes the reference at point the same way inline-ask does (region, defun, line; C-c SPC cycles), creates an inline overlay reusing `superchat-inline--make-overlay` and `superchat-inline--sink`, and submits an envelope `:surface 'inline-act`, `:tools nil` (so the default baseline applies), the same `:context` keys as inline-ask plus `:inline-act t`, to `driver` `submit`. The Session is the inline Session of the buffer (`superchat-inline-session-id`).
3. **Progress in place.** The overlay header distinguishes `calling-tool` from `typing` ("Working: tool…" versus "Typing…"). The Sink status callback already receives both symbols; inline-ask collapses them and inline-act does not.
4. **Result in place.** At `final` or `error` the overlay body ends with a short footer: the number of tool calls the Run made (counted from `tool.completed` / `tool.failed` events on the Tape for this Run) and, when the Run left buffers edited and unsaved, the text of `superchat-changes-run-summary`. The footer states facts recorded on the Tape; it does not say the task succeeded.
5. **Keys on a ready overlay.** `r` follow-up in the same Session (reuses the inline reply path with `:surface 'inline-act`), `d` diff of an edited buffer against its file (`superchat-changes-diff`), `v` visit the Session, `q` clear (which cancels a live Run, as inline-ask does), `M-RET` menu.

The prompt sees the reference block through the existing `inline-reference` collector, so nothing is added to prompt assembly; the operating note and tool descriptions are the same as in the chat window. (Until 2026-09-25 they were not: the system-instructions collector listed only `chat` and `inline-ask`, so in-place act requests went without any system text; found while preparing M10 slice 2's acceptance and fixed, issue `2026-09-25-in-place-act-gets-no-system-instructions`.)

## Alternatives considered

**Render the chat window in a side window and keep `superchat-act`.** Keeps two experiences and still moves the user's attention; the reviewers' gap is exactly that the feedback is not where the work is.

**Widen `inline-ask` to the full tool baseline.** Rejected: inline-ask's docstring promises that an ask never puts a question in front of the user, and that promise is what makes it safe to bind to a cheap key. A task that may edit and may ask is a different verb.

**Build capability seams first (the roadmap's "Later").** Rejected for this step: the seams answer a different question (which verbs exist), and the tool surface is undecided; this proposal uses the verbs already delivered.

## Acceptance (scripted transport, no real model; a new test file `scripts/inline-act-regression.el`, prefix `inline-act/`)

1. With the default profile, `inline-act` is an active row and `inline-act` is a registered surface; unmounting the row withdraws the surface and `superchat-inline-act` refuses with a message naming the row and the profile, submitting nothing.
2. From a temporary buffer, `superchat-inline-act` with a script whose first step calls `edit_buffer` on that buffer and whose second step answers text: the Run completes; the buffer holds the edit; the overlay is `ready`, shows the answer text, and its footer reports one tool call and one unsaved edited buffer; no chat buffer was created.
3. The provider request of that Run carried the default tool baseline (at least `edit_buffer` and `read_buffer`), not the inline-ask four.
4. A script step that calls an `ask`-class tool (`eval_elisp`) reaches the approval seam: with `superchat-approval-function` answering deny, the Tape records `approval.requested` then a refusal, the tool body did not run, and the overlay still reaches `ready` with the model's follow-up text.
5. `q` on a streaming overlay cancels the Run and removes the overlay; `r` on a ready overlay submits a second envelope with `:surface 'inline-act` in the same Session.
6. Canonical smoke set and the existing regression files pass unchanged; the default profile's row list gains exactly one row.

Real-model evidence (LM Studio) is a separate, later step, run by the leader, not part of the worker's slices.

## Risks

- **A weak worker model.** Slices are kept to one file plus one test file each; every brief carries the exact functions to reuse and the exact commands to run.
- **Overlay under an edit.** If the model edits the region the overlay covers, the overlay's range moves with the text (overlay semantics); the footer is what tells the user which buffers changed. Not solved in this proposal: an edit that deletes the whole overlay range.
- **Approval in the minibuffer while point is in the source buffer** is the same experience as today's chat path and is not changed here.
