# 04 Real Subagent chains close with known observation events

status: done
phase: independent check PASS and learn complete; next arrange05 not executed
blocked_by: []
predecessor: .specs/framework-regression-repairs/issues/03-org-babel.md — done after independent check and learn, E-20260906-004
writer: do (listed product and direct tests only, after predecessor check and learn)
authority: superchat-closure.el — sole observation classification and Closure judgment; superchat-subagent.el — existing parent-context selection only if the real async chain proves fallback repair necessary
spec_ref: .specs/framework-regression-repairs/SPEC.md
context_ref: KERNEL.md (K29); CONTEXT.md; .specs/framework-regression-repairs/SPEC.md (AC4); docs/protocols/session-editor-workspace-close.md
evidence_ref: E-20260906-005

## Goal

Known Team/Subagent observations no longer poison legitimate child Run traces,
including no-parent-Session calls and the parent Run's real asynchronous delegate path.

## Scope

- superchat-closure.el: exact known non-Run-lifecycle event set addition only.
- superchat-subagent.el: conditional fallback selection using exact parent
  tool-context Session only after the real acceptance test proves the need.
- test/test-closure.el and test/test-subagent.el: direct classification and genuine
  asynchronous delegation/Runtime/Tape/Authority/Closure integration cases.
- Runtime, LLM, Agent Loop, Tools, Skills and test runner/helper are read-only.
  Calling them in tests grants no production-write scope.

## Acceptance

1. Exactly seven event types join known-irrelevant observations:
   team.task.created, team.task.updated, team.message.sent, team.decision.published,
   subagent.started, subagent.completed and subagent.failed. No prefix acceptance.
2. Observations alone cannot prove completion or discharge an effect/approval.
   Actual Run terminal facts still determine completed/failed/cancelled handoff.
   Unknown sibling names, malformed envelopes and illegal lifecycle transitions
   retain fail-closed behavior; ordinary validation is not bypassed.
3. With no parent Session/team binding, real async child success is completed with
   deliver; real provider-boundary failure is failed with reject, rather than
   audit-incomplete caused by known observation types. Actual Team/Subagent event
   producers run, Closure guard stays enabled, child host retention remains intact.
4. A real parent Runtime Run calls delegate_to_subagent through its async tool
   wrapper and callback protocol, creates/finishes a real child Run, accounts the
   parent tool and receives its report once. Assert exact parent-run-id, child
   Session, Team event placement, terminal traces and handoff. Exercise callback
   execution with an unrelated current buffer to expose ambient fallback errors.
5. Explicit team-id still wins. Only recorded red evidence from (4) permits using
   the exact parent tool-context Session for fallback. The conditional repair is
   explicit team-id, then exact parent tool-context Session, then existing ambient
   Session/child-id fallback when no parent Session exists. This reconciles the
   original shorthand with confirmed AC4; it does not authorize a separate parent
   Team-id source, registry or new identity. Test an unrelated ambient Session,
   explicit team precedence and unchanged no-parent fallback before changing code.

## Verification

### Check and learn — 2026-09-06

Independent c04 PASS accepted. E-20260906-005 was appended and verified unique
before this done backlink. K30 promotes only the approved finite seven observation
names/witness and no-obligation/no-completion meaning, retaining validation and
unknown fail-closed behavior;18 mechanical anchors audited against actual constructs.
Fresh strict1077/errors0/warnings0 and authority form pass. No fallback/identity
entry change: F1 proves View-local Session retention via callback-in-view, while a
borrowed non-View without local Session uses child-id fallback with exact parent
Run lineage and no foreign pollution. This is not universal retention.
Reused independent real81/81, ordinary81/76/0/5, canonical1353/1316/0/37 and own
probe20/0; saved-baseline seven red3/0/3 and child red2/0/2. Worker cleanup evidence
is historical/code-reviewed, not checker-rerun, and zero timer allocation is not
positive timer-cleanup proof. No new ERT in learn; no05 action. Full report:
/private/tmp/framework04-learn/WORKER_REPORT.md.

### Do handoff — 2026-09-06

Only product delta is the seven literal event names in Closure's existing
known-irrelevant branch. Subagent production is unchanged: real native parent
async scheduling from an unrelated buffer passes default/explicit team and child
success/failure combinations. Native agent-wrap-tool already wraps invocation with
callback-in-view targeting its captured buffer; no genuine fallback red occurred,
so conditional fallback authorization was not exercised.

Tests-first original Closure red2/0/2 and real no-parent success audit-incomplete
are retained. Additional pre-change Closure snapshot replay verifies all seven
individually unknown and both real child success/failure audit-incomplete; loading
that saved implementation occurs only in isolated test processes, no repo restore.
The final matrix checks finite/combined observations, valid open effect/approval
traces, unknown/malformed/illegal cases, actual native llm async tool scheduler,
Authority, Team/child Runtime/SQLite/Tape/Closure, parent followup/final, lineage,
Team placement, no foreign pollution, once/late callback and retained/public-close
hosts. Real model transport callbacks alone are replaced. Existing test forms and
all other production are unchanged. No claim that observation status proves Run
completion; actual terminal facts control handoff.

Final direct ordinary81/76/0/5 and isolated llm0.31.1 real81/81/0/0; canonical
1353/1316/0/37 exit0 plus separate RUNNER_GATE_OK. Exact commands are in
/private/tmp/framework04-do/WORKER_REPORT.md.
Three scoped assertion-injection cleanup probes observe actual buffers/DB/path
after unwind, preserve the injected assertion, and leave terminal Run tables;
configured timeout-free cases allocate no new retained timers, no rescue needed.
Source/test parens, reader equality and scoped whitespace checked; authority form
valid. Read-only K29 strict1072/errors18/warnings0 records construct relocation,
not an authorization to edit Kernel in do. Evidence remains empty pending check
and learn. S1/O1/S2 excluded; no05, knowledge/SPEC/architecture-map changes.

### Arrangement — 2026-09-06

The user-confirmed five serial edges remain01→02→03→04→05.03 is done and its
unique E-20260906-004 resolves to accepted independent check/learn at K29; this
removes04's predecessor blocker.05 still requires04 check+learn. This action only
arranges04, leaves evidence_ref empty, and runs no acceptance tests.

Exact do ownership: superchat-closure.el's finite known-irrelevant branch in
superchat-closure--apply-event; superchat-subagent.el's team-id fallback in
superchat--subagent-make-context only if the real parent async red proves need;
test/test-closure.el and test/test-subagent.el direct regressions, plus this
Slice's short status/verification summary. Runtime/LLM/AgentLoop/Tools/Skills,
Authority/Tape/DB/EditorWorkspace, helper/runner, other tests, knowledge/SPEC and
architecture-map are read-only. S1/O1/S2 are excluded. No broad cleanup.

Source reachability: Runtime submit binds full tool-execution context before
native superchat--llm-generate-answer builds superchat--agent-make-wrapper;
superchat-get-llm-tools supplies async delegate_to_subagent with
superchat-tool-delegate-to-subagent-async. Agent wrapper crosses Authority, then
superchat--subagent-run-async creates real Team task, prepares child tools, acquires
the child protocol host and submits child Runtime. Its delivery callback calls
superchat--subagent-finish and Closure-gated handoff, then the tool callback resumes
the parent LLM loop. Team member events preserve child Run and parent Run lineage.
Current make-context chooses explicit dynamic team-id, ambient Session, then child
id; parent-context already carries parent Run/Session. This is a source-supported
test seam, not proof that fallback repair is needed or that the full chain passes.
Existing tests replacing Team producers or disabling guard do not satisfy AC4.

Tests-first sequence:

| Gate | Real setup and red/green oracle |
| --- | --- |
| Finite classification | Add each of the seven exact event names separately and together to valid traces; require known-irrelevant witness and unchanged terminal proof. Existing code must fail at unknown classification before the finite addition. |
| Negative matrix | Unknown siblings team.task.deleted, team.message.received, team.decision.revoked, subagent.progress plus arbitrary prefix variants remain unknown/non-proved. Bad schema, missing required identity/event-id, duplicate IDs, invalid causation/provenance and illegal Run/effect/approval ordering retain their existing invalid/unknown verdicts. Do not convert unknown to valid or require every malformed case to share one verdict. |
| Observation is not proof | Observation-only/open-start traces cannot prove terminal; insert observations around pending approval/requested or executing effect and retain non-proved readiness. Real lifecycle facts remain necessary. |
| No-parent async child | Real temporary SQLite, built-in Preset, real dispatcher/tool preparation, Team producers, child Runtime and enabled Closure guard; no parent Session/team binding. Delay transport callbacks until submit returns; success delivers completed report once, transport failure gives failed/reject once, never observation-induced audit-incomplete. Assert actual task/member rows and host retained until explicit public close. |
| Parent native chain | Submit actual parent Runtime with real delegate descriptor and native LLM wrapper. Mock only model transport/scheduling: first parent round invokes its supplied async wrapped tool, child transport resolves later, real callback supplies report to parent followup, then parent final. Capture actual published IDs and Tape events; require one tool execution/outcome, one child handoff and one final parent delivery. No direct wrapper-only substitute. |
| Context and precedence | Invoke the supplied async tool and delayed child callback from an unrelated current buffer with a foreign Session. Assert parent-run-id, distinct child Session, expected Team placement and no foreign Tape pollution. Repeat with explicit team-id and with no parent context. Save genuine fallback red before the conditional change; if it passes, leave Subagent product unchanged. |
| Failure and retention | Parent chain child transport failure returns error report once while actual child terminal is failed and handoff rejects; parent tool accounting is real. Duplicate late transport completion cannot duplicate callback/report. Accepted exact child hosts remain publicly discoverable and closable after success/failure; unrelated buffers survive. |

Use already installed isolated llm0.31.1 with package-initialize before require;
load test-helper before product to isolate user configuration/data. Keep actual
SQLite/Tape appends, Authority policy/execution, Team producers, Runtime terminal
statuses and Closure guard; do not stub any to force green. No provider/socket.
Drive supplied native transport callbacks deterministically without replacing the
LLM loop or manually sealing Runs. New fixture-local cleanup captures actual child
buffers, timers/Run handles and DB paths, cancels remaining work and publicly closes
owned scopes in unwind-protect even after assertion failure. Do not alter shared
helper/runner or unrelated old assertions.

Planned bounded commands from repository root (not run by arrange):

```sh
emacs -Q --batch -L . -l test/test-closure.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-subagent.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Run new focused selectors first for red/green, then both direct files ordinary and
real-dependency environments. Real prefix: emacs -Q --batch --eval '(progn
(require (quote package)) (setq package-user-dir
"/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa")
(package-initialize) (require (quote llm)))' -L . followed by direct file/ERT args.
Verify llm0.31.1 provenance, check-parens and changed-line whitespace; report exact
commands/counts/exits, dependency skips and canonical RUNNER_GATE_OK separately.
Record Kernel drift read-only for later learn. No network/install/socket/provider/
GUI/user DB/HOME/CODEX_HOME/VCS operations or new agents/panes/worktrees.

Scope appears sufficient from the inspected callgraph; full real-chain execution
remains the do proof. If genuine acceptance requires Runtime/new identity behavior,
or exposes a new precedence contract, retain red and return blocked→plan. Do not
reclassify required acceptance as out of scope or silently enlarge the finite set.

Use actual Runtime, Authority, Tape/temporary SQLite, Closure, Team producers and
Subagent orchestration. Mock the provider/transport schedule only, preserving real
async tool callback semantics. Stubbing team-start-delegated-task, disabling the
Closure guard, manually setting final statuses, or directly invoking a wrapper
without driving its async callback cannot satisfy acceptance.

Capture focused red/green for all seven types and unknown counterexamples, no-parent
success/failure, and the real parent delegate chain. Parent-context fallback is a
conditional production change; if no red evidence appears, leave subagent production
unchanged. Run direct suites and canonical. Extra Runtime/Team identity behavior
requires blocked → plan, not a hidden scope increase.

## Evidence

Source inspection confirms the finite Closure list lacks all seven types and
superchat--subagent-make-context falls back to child identity after ambient bindings.
The old report did not drive the complete parent async chain; its binding experiment
is not substitute evidence. This turn has no repair tests. Slice 05 waits for
verified check/learn and the evidence reference supplied by learn.
