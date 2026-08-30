# Chat input lifecycle

status: confirmed
revision: 2
context_refs: KERNEL.md K33; CONTEXT.md; docs/spec-agents/WORKFLOW.md

## Problem and goal

Real interactive chat duplicates the initial User header, consumes the only
input prompt during ordinary generation, and silently ignores `/clear` typed
thereafter. Restore one visible editable draft throughout ordinary chat.

## Unchanged contracts

Runtime owns lifecycle; Authority, Tape, Closure and EditorWorkspace remain
unchanged. Exact buffer/Session targeting, opaque custom transcript owners,
legacy borrowed input, retained owner ends, prompt fontification validity,
Workflow/ordinary interleave, pause/resume/cancel and late delivery isolation
remain. No automatic execution, steering, cancellation or durable draft promise.

## Decision and boundaries

User confirmed: during an active ordinary chat Run, C-c C-c leaves input in
place and reports `已暂存，当前回答结束后可编辑并发送`. Repeated sends are
idempotent. After terminal, the user explicitly sends the same editable draft.
Use public Runtime status and existing UI tracking, not all Session activity
as a blanket prohibition. Workflow work does not itself prohibit ordinary send.
`/clear` uses existing command parsing and confirmation before input consumption;
no preserves View/Run/draft, yes uses existing exact Session rollover.
C-c C-q edits the same visible draft rather than maintaining another hidden copy.

## Model delta

One View owns one editable pending draft. This is transient UI state, not a
message queue, Run, durable object or new registry. Render remains prompt owner.

## Action Contracts

- AC1: interactive fresh/reopened View has one valid input; existing draft and
  reading point survive reopening. Actual C-c C-c reaches ordinary Runtime.
- AC2: after ordinary submit, streaming and final/error/cancel preserve next
  draft text/properties/exact marker/point. Output never enters it; unsent draft
  is absent from model/history/Tape. Repeated active sends leave one request/Run
  and give the confirmed feedback. Terminal never automatically sends it.
- AC3: C-c C-q operates on this same input, with existing text offered for
  editing; it never creates a hidden duplicate or auto-sends. Legacy pending
  values may be drained by existing prompt creation compatibility.
- AC4: parsed `/clear` reaches confirmation before consumption. No is unchanged;
  yes yields usable fresh Session with exact buffer/config retained and old late
  callbacks isolated. No new policy for unrelated slash commands.
- AC5: synchronous final-before-submit-return, UI reentry, fontification,
  history point, Workflow interleave and prior owner/targeting guards remain.

## Seams and verification

UI initialization/follow-up and dispatcher input boundary reuse Render's existing
prompt seam. Tests use call-interactively superchat and actual C-c C-c macros,
real isolated SQLite/Runtime/Tape/Closure and installed llm 0.31.1, mock both
native transport entries, crm fail-closed guard. Preserve discriminating red;
focused ordinary/real, relevant suites, canonical, full real llm socket-excluded,
read-only strict/authority and parens/whitespace. No GUI/provider claim.

## Compatibility and migration

Approved behavior revision: ordinary active sends no longer expose concurrent
ordinary Runs; they retain the draft with feedback. C-c C-q no longer appends
into a hidden next-prompt string; both keys share the visible editable input.
Users edit that one draft and explicitly send after completion. Consumers
depending on hidden queued-follow-up behavior must use visible input instead;
no blanket change to all callback or slash-command semantics. Kernel promotion
and any required durable migration record belong to learn after text review.

## Out of scope

Runtime/Authority/LLM/EW production, knowledge/old issues/architecture, raw tool
tags, permission wording, bell, TTFT and other acceptance findings. No new queue
registry/classifier or lifecycle action. Actual scope conflict returns to plan.

## Issue map

01-input-lifecycle: one vertical result, no dependencies.

## Revision notes

Rev2: approved test-only scope increment: test/test-control-channel.el only the
form formerly named control/queued-follow-up-appends-and-is-consumed-without-steering
(accurate rename allowed). Retire hidden-string/mode-line queued representation
assertions in favor of actual Render marker/visible editing, no duplicate/shadow
copy, no steer/auto-submit. Nearby no-auto-send and every other form remain
reader-equal. Whole control/canonical required. No product semantic expansion.

Rev1 captures the user's explicit visible-draft decision after the plan probe
proved missing ordinary active-send policy. Earlier probe/report is historical
at /private/tmp/human-prompt-lifecycle/WORKER_REPORT.md.
