# Proposal: M9 — close G4 at the composition layer; a separate, narrow load-isolation entry only if wanted

Status: accepted (user, 2026-09-22: "同意", on the revised light version; review in `.scratch/discussions/2026-09-22-m9-weight-review.md`)

## What the first draft asked, and why it was withdrawn

The first draft made every product row "lazy" (register entry points at mount, load the implementation on first use), cut the six product requires out of `superchat-dispatcher.el`, added rows for nine modules that have none, and asserted that after a `headless` request every product feature is unloaded. Three independent reviewers, with the leader re-checking the cited lines, found:

- **The blueprint does not do that.** DeepSeek Harness loads an enabled row's module when the row is initialised and skips only disabled rows (`deepseek-harness/vendor/loader/src/config/entry.ts:124-127, 277-297`). "Load at mount, unless disabled" is the blueprint; "load on first use" is a performance policy the first draft promoted to an architecture rule.
- **There is no measured cost to recover.** `(require 'superchat)` from source takes 0.24–0.31 s in batch and loads 46 superchat features; `headless` takes about the same (reviewer measurement, repeated by the leader). Lazy loading would move a fraction of that to the user's first keystroke and move load errors there too.
- **Unmounting was never meant to unload code.** M4 promises that a withdrawn row's registrations are gone and its entry point refuses; Emacs cannot unload an autoloaded definition, and the M4 record says so.
- **The draft contradicted itself and M4.** New rows for nine modules conflict with "the default profile is unchanged row for row"; "every product feature nil after headless" conflicts with M4's `headless`, which keeps memory and workspace on purpose.
- **Two details were wrong.** The "two memory functions in `superchat.el`" are `defcustom`s (`superchat.el:4932, 4950`); moving `superchat-llm-configure` into the chat row does not make it withdrawable, because it replaces a global adapter plist with no Scope undo (`superchat-llm.el:3882-3906`).

## Decision (proposed)

Split G4 into the two things it was conflating, and judge them separately.

1. **Composition layer: close it now, with evidence, and no production code change.** The accepted criterion is M4's: removing a row withdraws its registrations, its entry point refuses naming the row, an in-flight Run is not harmed, remounting restores it, `headless` completes a request with no user-interface row mounted. Add only the behaviour checks M4 did not already make, for the rows that are not surfaces (memory, workspace, plan, subagent, workflow, compact, changes). One new focused test file; no second list of which module is "spine".
2. **Load isolation: not a goal of this milestone.** Recorded as an explicit narrowing: the default entry `(require 'superchat)` loads the whole product, including modules whose rows are disabled. If the user later wants "a cold, UI-less Emacs does not load chat code", that is a separate narrow slice: a second entry point that loads the execution path and the rows the chosen profile enables, cutting only the dependencies that entry actually hits, verified by one real driver→Runtime→mock→Tape check. The default entry stays as it is.

Not proposed: lazy rows as a rule, new rows for helper modules, cutting the dispatcher's requires, relaxing `superchat-profile-mount`'s refusal of undefined rows, a line-count or feature-count target.

## Acceptance

- A new file `scripts/composition-regression.el` (prefix `composition/`): for each default row that is not a surface, unmount → its registrations (Services, tools, listeners, commands) are absent and its entry points refuse naming the row; a chat Turn still completes; remount → it works again. Scripted transport only.
- M3, smoke (byte-identical baseline) and kernel check unchanged and green.
- The roadmap states G4 as: composition layer `done` with that evidence; load isolation `not a goal`, with the sentence above.

## Alternatives considered

**The first draft (lazy everything, spine list, dispatcher cut).** Withdrawn for the reasons above.

**Close G4 on M4's evidence alone, adding nothing.** Rejected narrowly: M4 tested the surface rows; the non-surface rows' withdrawal has not been checked one by one, and the reviewers asked for that check before calling the layer done.

## Amendment (accepted 2026-09-22, user: "同意建议")

Slice 1's verification found that unmounting `subagent`, `workspace` or `memory` left their tools offered to the model, because `tools-builtin` registers every built-in tool name (`superchat-tools.el:2120-2149`). Three reviewers agreed this is a real gap against this record's acceptance (`.scratch/discussions/2026-09-22-m9-tool-gap.md`). The user lifted "no production code change" for this fix and accepted:

- The built-in tool groups move by *registration ownership* only: `tools-builtin` stops registering them and the owning row registers them as built-in descriptors in its own Scope, so unmount withdraws them. Implementations, descriptions, the reserved-name list, the user's tool-name list and the permission classes are unchanged; the default tool surface is unchanged. Groups: `subagent` owns `delegate_to_subagent`, `delegate_to_subagent_parallel`, `team_task`, `team_message`; `workspace` owns `workspace_write`, `workspace_publish`, `workspace_read`, `workspace_info`; `memory` owns `memory_search` (the user's ruling: turning memory off turns its search off).
- The bypasses close too: an explicit Preset or request tool list cannot select a withdrawn tool, and a tool already materialized before the unmount refuses to start a new body.
- Public entry points that start new work (`>>` workflow coordination, `/workflow`, `/subagent`, opening the agents panel) refuse when their row is unmounted; callbacks, cancel, cleanup and read-only inspection of work already running are not blocked. This is a second slice.
