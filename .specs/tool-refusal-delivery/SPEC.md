# Tool refusal delivery

status: confirmed
revision: 1
context_refs: KERNEL.md K31; CONTEXT.md; EVIDENCE.md E-20260906-006; docs/protocols/kernel-review.md; docs/runbooks/kernel-check.md

## Problem and goal

Remaining blocked/deny/declined-approval/admitted run-inactive async refusals can
redeliver a consumer error as a false audit-failure message. User approved consult
option A: refusal once, original consumer error/quit propagation. This is one
independent end-to-end Slice, not a reopening of completed framework01–05.

## Unchanged contracts

Gate1 remains outside the outer error handler. Both admission gates, legitimate
refusal audit rows, admitted approval cancellation/audit-incomplete, preexecution
gate, true prewrite/exact-evidence failure routing and Closure interpretation stay.
Successful invoke-async consumer-error swallowing and implementation-failure
delivery remain. Runtime owns lifecycle; no new terminal/recovery action is allowed.

## Decision and boundaries

Authority-local refusal closure reuses refusal-delivery-started, set before
delivery. Gate2/blocked/deny/declined use it; private --invoke receives the necessary
optional refusal closure for run-inactive. No global --deliver semantic change,
registry, dynamic context authority, warning channel or wrapper production change.

## Model delta

Refusal outcome and consumer exception remain distinct: consumer failure cannot
be relabelled audit failure or justify another result. No identity/Tape schema
change. K31's unresolved old-refusal exception is revised only after do/check and
independent Kernel-text review during learn, not in this action.

## Action Contracts

1. Every listed refusal branch with normal/error/quit consumer delivers once,
   preserves exact first message and original error/quit symbol/data, writes only
   its legitimate audit, and invokes no implementation. Consumer error produces
   no false audit-failed result. Existing gate1/gate2 tests remain.
2. Real AgentLoop and Subagent/native llm scheduler paths use actual Runtime,
   SQLite/Tape and enabled Closure guard, transport mocked only. Observe immediate
   Run status, terminal state and scheduler continuation with throwing receivers;
   wrapper once masking is not proof of Authority correctness. Probe multi-tool
   duplicate-result risk within existing fixtures if reachable; report facts.
3. Preserve genuine prewrite/audit/exact-evidence failures, successful async
   swallowed error, implementation-failure delivery and normal sync returns.
4. Any need for a new Runtime observer/error terminal contract returns to plan
   with red/trace retained. No unverified claim that every callback fault stalls
   or terminates a Run, and no implicit lifecycle implementation.

## Seams and verification

Production: superchat-authority.el superchat-tool-execute and necessary private
--invoke parameter only. Tests: test/test-authority.el, test/test-runtime.el;
test/test-agent-loop.el and test/test-subagent.el if real caller fixtures require.
Tests-first direct and real-chain red, guard-removal negative control, then fresh
focused ordinary/isolated llm0.31.1, caller suites, canonical and full real LLM
file excluding named UTF8 socket fixture. Check parens/whitespace, read-only strict
and authority; retain skips/exclusions and distinguish raw process exit from ERT.

## Compatibility and migration

Breaking on the formerly swallowed custom async consumer-error path: such callers
now observe the original error and must catch/handle it at their own boundary if
isolation is desired. No claim of zero observable change. User explicitly approved
this alternative and migration; success async swallowing remains unchanged.

## Out of scope

O1/S1; old framework Slice rewrites; Runtime/LLM/AgentLoop/Subagent production;
all other tests/helpers/runners; Kernel/Evidence/STATUS/ADR/Protocol/architecture-map.
No network/provider/socket/GUI/install/userDB/HOME/CODEX_HOME/VCS/new agents/panes.

## Issue map

01-refusal-delivery: single approved end-to-end Slice, blocked_by=[]; independent
check then separately authorized learn. No goal closure is implied.

## Revision notes

Rev1 captures explicit option A approval and cfr/framework-refusal-consult.md.
Its stalled-Run and multi-tool claims require real observation, not assumption.
