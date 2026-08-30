# 01 Refusals deliver once and preserve consumer conditions

status: done
blocked_by: []
writer: do
authority: superchat-authority.el — tool outcome accounting and refusal delivery; Runtime lifecycle remains read-only
spec_ref: .specs/tool-refusal-delivery/SPEC.md
context_ref: KERNEL.md K31; CONTEXT.md; EVIDENCE.md E-20260906-006; docs/protocols/kernel-review.md
evidence_ref: E-20260906-007

## Goal

Implement approved option A for remaining refusal branches, with real caller
and Run-state observations. Preserve the separately learned framework01–05.

## Scope

- Product: superchat-authority.el superchat-tool-execute and necessary private
  --invoke optional refusal parameter. Existing local flag and refusal closure.
- Direct tests: test/test-authority.el, test/test-runtime.el; necessary real caller
  regressions may use test/test-agent-loop.el/test/test-subagent.el.
- This Slice's short status/verification only. SPEC is capture-owned, knowledge
  learn-owned; other products/tests/runner/helper/architecture-map read-only.

## Acceptance

SPEC AC1–4. blocked/deny/declined/admitted run-inactive × normal/error/quit:
exact original condition/data, callback once/message, legitimate Tape/Closure
unchanged and zero effect. Guard must precede consumer invocation. Keep gate1/2,
real prewrite/exact-evidence, successful invoke-async swallowing and implementation
failure delivery tests. Private --invoke omitted optional parameter remains usable.
Real AgentLoop/Subagent native scheduler paths must observe Run state and schedule
continuation. Do not infer consumer-visible once from wrapper masking; direct
Authority counts and native original-condition checks are complementary. New Runtime
cleanup semantics require blocked→plan, never an out-of-scope implementation.

## Verification

Arrangement: approved single Slice, no predecessor dependency; tests not yet run.
Tests-first ordinary branch matrix and real throwing-receiver regression, saved
red; minimal closure implementation; guard-removal negative control. Use existing
llm0.31.1 packages, actual temp SQLite/Tape and Closure guard; transport-only mocks.
Inspect multi-tool aggregation under a fault without new infrastructure.
Fresh direct ordinary/real, relevant caller suites, canonical and full real
test/test-llm-backend.el excluding test-ollama-utf8-split-chunk-integrity. Record
actual results/skips/exclusions, Run observations, parens/whitespace, read-only
strict and authority. Baseline attribution: final audit canonical1361/1320/0/41,
full-real201/201 socket excluded, K31 strict1087/0/0; no fresh baseline rerun claim.

## Evidence

Empty until independent check and learn. Compatibility is explicitly breaking for
custom async consumers that relied on swallowed errors: handle them at the caller
boundary. No implicit permission to change all callbacks or Run terminal behavior.

## Worker verification — 2026-09-06

Implementation complete, status remains doing pending independent check/learn.
Only Authority's two declared forms changed; old test forms retained. Direct
12-cell red showed double callback; native red showed swallowed original error.
Local refusal closure now marks attempted delivery before the consumer; gate1,
global deliver, successful async and implementation-failure behavior unchanged.
Guard-removal controls fail at direct count and native original condition.

Fresh total/pass/fail/skip: focused ordinary 13/7/0/6, real 13/13/0/0;
Authority/Runtime/Closure/AgentLoop/Subagent ordinary 323/310/0/13, real 323/323/0/0;
canonical 1364/1321/0/43; full real LLM 201/201/0/0 with named UTF8 socket excluded.
Native matrix has 21 single-tool and 4 two-tool cases, actual Runtime/SQLite/guard,
transport mocked only. Before-forwarding receiver faults leave running-model with
one request; after-forwarding faults allow second request and later completed.
Admitted approval cancellation remains audit-incomplete. Subagent's legacy context
omits liveness predicate: no new fallback added; admitted-inactive native coverage
uses AgentLoop. No automatic Runtime recovery claim or lifecycle change.

Parens/old-form preservation, whitespace and exact five-file scope pass. Read-only
strict: 1087 refs/31 errors/0 warnings, all audited 1238→1241 construct reanchors;
authority map passes. Kernel semantic review/learn still required. Detailed logs,
commands, negative controls and historical/setup-failure attribution are in
/private/tmp/framework-refusal-do/WORKER_REPORT.md. S1/O1 remain outside this Slice.

## Proposed learn handoff (not published)

This done/backlink candidate may be applied only after independent text review and
Evidence append/prefix verification. Implementation crc PASS; candidate K32 and
ADR0006 retain scoped breaking migration. No aggregate goal closure.
