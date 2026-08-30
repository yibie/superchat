# Runtime deadline acquisition

status: confirmed
revision: 1
context_refs: KERNEL.md K32; CONTEXT.md; EVIDENCE.md E-20260906-003 and E-20260906-007

## Problem and goal

O1: a published Run can lose its activated deadline timer when an injected
run-at-time after-advice signals before slot publication. Natural production
occurrence has not been established. User authorized the bounded repair.

## Unchanged contracts

Runtime owns Run lifecycle and deadlines. Preserve absolute deadline meaning,
disabled/one-shot behavior, submit finish/re-signal, exact borrowed hosts and
K32 callback contracts. No new terminal state, identity, registry or global sweep.

## Decision and boundaries

Own an inactive native timer before configuring, publishing and activating it.
Use local unwind cleanup for error/quit, cancelling the exact owned timer and
compare-clearing only its matching slot. Foreign/replaced timers and other Runs
remain untouched. No transport-wide inhibit-quit or generic transaction helper.
Unexpected activation/reentry lifecycle conflict returns to plan with evidence.

## Model delta

Compatible resource acquisition correction, no new ontology or callback policy.
S1 discarded isolated test entry is explicitly accepted bounded no-change with
no physical leak; its durable disposition waits for check/learn.

## Action Contracts

1. Save old after-advice red and test error/quit before/after activation and slot
   mutation and at commit boundary using captured exact objects. Original condition,
   timer-list absence, no dangling owned slot, foreign timer and borrowed host survive.
2. Real temp SQLite/Tape/Closure guard submission failure reaches no transport;
   positive elapsed deadline, cancel/completed/failed, disabled and A/B isolation
   retain existing behavior. Only model transport is mocked in positive paths.
3. Architecture documentation reflects learned framework01–05/E007 and current
   K32; O1 remains this batch pending check. Preserve logical-call versus require,
   opaque-owner and incomplete-path-map qualifications and historical visual evidence.

## Seams and verification

Production: superchat-runtime.el --arm-deadline only; adjacent exact slot cleanup
only if a concrete failing test proves necessary. Tests: test/test-runtime.el.
Docs: docs/architecture-map README/source-snapshot and necessary existing artifacts.
One Slice; independent check then learn and independent Kernel text review.

## Compatibility and migration

Native timer primitives replace run-at-time convenience allocation; advice on the
old convenience function is no longer this acquisition seam. No caller migration
or new runtime callback semantics. Fault probes follow actual native seams.

## Out of scope

S1 fixture edits, old Slice rewrites, other production/tests/helpers/runners,
Kernel/Evidence/STATUS/ADR/Protocol/doctrine writes during do. No VCS/network/
provider/socket/install/liveGUI/userDB/HOME changes or new agents/panes/worktrees.

## Issue map

01-deadline-acquisition, single approved end-to-end Slice, blocked_by=[];
docs refresh is companion do output. No goal closure or new authorization implied.

## Revision notes

Rev1 captures the user-approved remaining batch and residual plan O1/S1 bounds.
