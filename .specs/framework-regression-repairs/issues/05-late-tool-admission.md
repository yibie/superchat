# 05 Terminal-first tool arrivals are refused before execution audit

status: done
blocked_by: []
predecessor: .specs/framework-regression-repairs/issues/04-subagent.md — done after independent check and learn, E-20260906-005
writer: do (listed Authority and direct tests only, after predecessor check and learn)
authority: superchat-authority.el — sole tool admission and audit-before-effect decision; Runtime lifecycle and Closure interpretation remain their existing read-only production authorities
spec_ref: .specs/framework-regression-repairs/SPEC.md
context_ref: KERNEL.md (K30); CONTEXT.md; .specs/framework-regression-repairs/SPEC.md (AC5 and Compatibility and migration); docs/adr/0005-fail-closed-ordinary-llm-tool-admission.md; docs/protocols/kernel-review.md; docs/runbooks/kernel-check.md
evidence_ref: E-20260906-006

## Goal

A first tool arrival after a real Run is terminal produces a readable refusal and
no new Run execution audit/effect, preserving the already valid terminal trace.

## Scope

- superchat-authority.el: earliest liveness admission and immediately-before-first-
  audit liveness gate, preserving the existing pre-execution gate/accounting.
- test/test-authority.el, test/test-runtime.el, test/test-closure.el: real submit,
  cancel, late wrapper, admission-race and admitted-approval cancellation regressions.
- Runtime/Closure/LLM/Agent Loop/Subagent production and knowledge are read-only.
  The user explicitly authorized the Kernel contract change, not Kernel writes now.

## Acceptance

1. For a valid Runtime-bound terminal-first call, earliest liveness refusal occurs
   before intercept/normalizer/resolver/policy/approval or effect callbacks. Sync
   returns a readable result; async delivers it once. Zero Run execution audit
   appends, zero implementation/prehook effects and zero new approval prompts.
2. Recheck immediately before first audit append handles a Run terminated during
   pre-audit callbacks. Preserve the existing check before execution; early admission
   alone does not authorize an implementation after its Run stops.
3. Real submit → cancel → first late wrapped call retains the same terminal Run and
   unchanged Tape, with durable cancellation proved and trace-well-formed valid.
   Cover ordinary sync and async paths through the shared Authority seam, including
   terminal outcomes beyond clean cancellation where a terminal trace is available.
4. After tool.requested/approval.requested already admitted a call, cancellation
   while approval is open remains audit-incomplete. Its outstanding audit is not
   erased or relabelled as never admitted; the later execution gate prevents effect.
5. Illegal late execution/unknown events and old invalid traces remain fail-closed.
   trace-well-formed reports invalid for illegal transitions; existing public
   unknown projection is not weakened. Audit prewrite failure still blocks effects.

## Verification

### Learn — 2026-09-06, independently reviewed K31 publication

Aggregate05 implementation check is accepted, including the corrected narrow once
clause; the earlier withdrawn verdict remains historical below. This done
backlink follows independent K31 candidate review and verified unique006 Evidence
append with the prior prefix unchanged. K31 pairs the approved
non-admission migration and scoped consumer-condition semantics; existing admitted
accounting, Closure and legacy callback limits remain. Next is independent final
five-slice audit plus residual plan, not goal completion or a SPEC terminal change.
Reviewer: independent Claude test worker, c05k/framework05-kernel-review.md,
acceptable AS WRITTEN/no required finding. Published Kernel is byte-identical to
the reviewed candidate; optional new clauses were not adopted. Final publication
gates and limits: /private/tmp/framework05-learn/WORKER_REPORT.md.

### Required callback correction — 2026-09-06; narrow recheck pending

Independent c05 callback-scope review withdrew only the earlier PASS's once
clause: gate2 consumer error entered the audit-failure handler and redelivered.
The original check/report and other passing evidence remain historical. Approved
Authority-only correction adds function-local refusal-delivery-started, sets it
before gate2 delivery and re-signals the original consumer error in outer-err.
Quit propagation, actual audit prewrite failure, existing successful async
consumer-error swallowing and the execution gate remain unchanged.

Tests-first ordinary and captured-real-Runtime resolver-cancel regressions each
failed on callback count 2. Green covers gate1/gate2 × error/quit, exact condition
data, refusal message, callback once and zero audit/effect. Removing just the guard
in a temporary scoped negative control reproduces both count failures. Existing
12 native supplied-wrapper scenarios remain; the new fault probe uses their
captured real context explicitly, not a claimed native scheduler consumer fault.
Fresh focused ordinary5/5, real4/4; direct real236/236/0/0,
ordinary236/232/0/4; canonical1361/1320/0/41, exit0. Read-only strict1077/13/0
retains the already audited mechanical citation drift; authority form, parens,
old-form and scope/whitespace checks pass. No Kernel/Evidence writes.

Old deny/block/admitted refusal redelivery remains an unresolved separate
observation, neither fixed nor waived. Full exact commands/logs and limits:
/private/tmp/framework05-callback-fix/WORKER_REPORT.md. Status stays doing,
evidence_ref empty; stop for narrow independent recheck, no learn/closure.

### Do — 2026-09-06; implementation complete, independent check pending

Authority adds only the earliest structural-validation-following liveness refusal
and the immediately-pre-first-audit recheck (11 lines). Existing pre-execution
gate remains unchanged. Direct Authority/Runtime/Closure tests replace only the
approved terminal-first four-event oracle and add the real native sync/async
cancelled/completed/failed/timed-out matrix, preaudit resolver cancellation and
already-admitted approval cancellation. Runtime, SQLite/Tape, Closure guard,
registered tools/policy and native supplied wrappers are actual; only transport
is mocked. Supplementary normalize/unavailable-Run checks use captured real
context and are not claimed as native Request producers.

Historical original-Authority red: native 3/1/2 (admitted case already passed),
plus old-oracle replacement 1/0/1. Final real llm0.31.1 direct suites 234/234/0/0;
ordinary 234/231/0/3; canonical 1359/1319/0/40, exit 0. The three required native
tests are non-skipped in real dependency execution (12 native scenarios).
Independent temporary negative controls distinguish all three gates. Three
injected-failure cleanup probes observe actual dead buffers, closed DBs, removed
paths and terminal Runs, preserving assertion conditions. Exploratory fixture
cleanup placement was corrected; five earlier private leaked directories were
observed then manually rescued, separately recorded, not counted as cleanup pass.

Parens and original-form scope checks pass. Read-only Kernel strict: 1077 refs,
13 errors (audited unchanged test-construct citation offsets), 0 warnings; authority
map form valid. Approved semantic migration of old inactive four-event clauses is
separate from these mechanical offsets and remains for learn. Kernel is untouched.
Full commands, logs, setup-failure provenance, negative controls and limits:
/private/tmp/framework05-do/WORKER_REPORT.md. evidence_ref remains empty; no learn
or aggregate closure. S1/O1 remain separate, S2 no-change.

### Arrangement — 2026-09-06

04 done resolves unique E-20260906-005/K30. The approved five serial edges stay
01→02→03→04→05;04 check+learn satisfies the last predecessor. This action writes
only05, leaves evidence_ref empty and performs no implementation/ERT acceptance.

Do product ownership is superchat-authority.el's earliest admission and
immediately-pre-first-audit liveness gates in superchat-tool-execute, using the
existing run-active-p contract/refusal delivery. Preserve structural validation,
legacy contexts without a liveness predicate and the existing pre-execution gate
in superchat-authority--invoke. No Runtime/Closure/LLM/AgentLoop/Subagent production
changes, no new registry or identity. Direct tests are exactly test/test-authority.el,
test/test-runtime.el and test/test-closure.el plus this Slice's short status summary;
all other tests/helper/runner/knowledge/SPEC/ADR/architecture-map remain read-only.

Source seam: superchat-tool-execute currently normalizes policy, then executes
intercept/normalize/validate/effect resolver and decision callbacks before its first
tool.requested append. _request-active currently validates exact evidence origin,
not general Runtime liveness. superchat-authority--invoke checks liveness only
after admission/approval, recording run-inactive failure. Add the approved earlier
distinction without removing that admitted-call protection or swallowing original
callback/error/quit contracts. Pure required validation is not permission to invoke
pre-audit user callbacks on terminal-first calls.

Native reachability fits test/test-runtime.el: load actual AgentLoop/LLM/Tools
within new isolated tests, register fixture-local sync and async tools through the
real registry, submit real Runtime and capture the tool structs supplied to the
mocked model transport. Invoke those native wrappers after terminal through real
llm tool scheduling/callbacks. Runtime publication supplies the liveness closure;
do not handcraft a nil predicate as the decisive proof. No test/test-agent-loop.el
or test/test-llm-backend.el edits are needed. Existing runtime isolation macro
disables Closure guard, so decisive tests must explicitly enable it after that
binding or use local isolated state with guard t. Keep SQLite/Tape/Authority/Closure
and tool wrappers actual; only model transport/scheduling is mocked. Mocked audit
failure is confined to the deliberate negative test and cannot serve positive proof.

Tests-first sequence and observable gates:

| Gate | Setup and required oracle |
| --- | --- |
| Terminal-first native sync/async | Real published tool-bearing Run, retain supplied wrapper, terminalize before first tool arrival. Arm intercept, normalizer, validator, effect resolver, policy/permission/approval and pre/post implementation hook counters; terminal-first invocation returns readable refusal, async callback once, all Authority callback/effect counters zero and no new execution-audit rows. Native wrapper bookkeeping/presentation is not a second Authority policy and is not rewritten. |
| Real terminal matrix | Public cancel; normal model final completed; model transport error failed; existing real deadline timer handler timed-out when a valid terminal trace is obtainable. Capture actual Run status and byte/value-equivalent persisted Tape before/after late wrapper; retain valid trace and proved matching durable terminal. No manual status assignments or forged terminal append. Dispose actual timers, hosts and DB paths through unwind cleanup. |
| Pre-audit race | Start active; a real registered intercept/normalizer/resolver or policy callback cancels that exact Runtime Run before tool.requested. Snapshot Tape after cancellation inside that callback; second gate must return refusal without any later execution-audit row or effect. Assert callback actually ran while active and cancellation terminal proof is real. Exercise sync and async refusal routing. |
| Already admitted approval | Real tool.requested and approval.requested exist when approval callback cancels exact Run, then returns allow. Assert actual audit-incomplete and retained rows; existing execution gate refuses implementation/prehook despite approval response. Do not reinterpret as zero-admission or erase audit. |
| Gate discrimination | The terminal-first test must fail against old four-event behavior; pre-audit-race test must fail when only earliest gate exists; admitted test must still need the existing execution gate. Save those targeted red/negative controls without reverting unrelated work. |
| Validation negatives | Existing malformed Session/attendance/function/args/async-callback validation and invalid liveness predicates stay fail-closed. Test unknown Run/unknown event and illegal late tool.execution.started/tool.completed or approvals against real Closure; no blanket late-event ignore or predicate-free Runtime simulation. Preserve ordinary nil-legacy predicate omission compatibility. |
| Audit prewrite failure | Active valid context with deliberate first append failure cannot run implementation/prehook; preserve existing ordinary and exact-evidence audit failure routing and original error/quit behavior. Do not fabricate a successful audit or manually bypass Authority. |

Counters observe actual registered callbacks; do not replace resolver/policy with
fake final decisions to claim end-to-end Authority. Captured before/after Tape
must distinguish cancellation's own terminal rows from prohibited tool-admission
rows. For audit-incomplete or unknown traces, require unchanged trace and existing
non-proved/invalid interpretation, never a fabricated proved terminal. F1 remains:
View-local Session via exact callback-in-view; borrowed non-View without local
Session may use child-id Team fallback.05 adds no parent Session fallback.
Context-only normalize/validate callbacks not exposed by a native Request are
supplementary shared-seam tests using captured real Runtime context; do not invent
a native producer for them. Native wrapper matrix remains mandatory with actual
reachable intercept/resolver/policy/approval/hooks, and real pre-audit cancellation
can use the registered effect resolver. This distinction does not authorize a
wrapper/Runtime change or replace the native end-to-end test with a seam-only test.

Approved future learn migration, not mechanical drift:

- Old authority/executor-does-not-run-after-runtime-termination explicitly expects
  tool.requested, approval.requested, approval.decided, tool.failed. Replace that
  terminal-first oracle with zero audit and add separate real admitted coverage.
- Kernel Approval and tool-execution lifecycle currently says every model-invoked call follows
  requested→decided→executing and tool.requested precedes every branch; its
  run-inactive failure statement must be narrowed to already admitted calls.
- Kernel Execute a model-invoked Tool permitted effect says every other path,
  including inactive Run, records failure; introduce terminal-first non-admission
  and retain audit-before-effect for admitted work.
- Paired audit-before-effect invariant says every model-invoked tool records intent;
  qualify admission versus refusal. Keep unaccounted approval/effect terminal guard,
  Closure illegal-transition contract and approved finite observations intact.
- Dedicated clear invariant's pending05 inactive-audit note must be reconciled
  after verification, without changing View isolation. ADR0005 trusted ordinary
  wrapper requirement stays enacted; no prepared-tool bypass is a migration.

Learn must run/read Kernel pairs, hunt counterexamples and use independent review
under kernel-review Protocol. External consumers of old four-event refusal traces
must accept non-admission without new Run execution rows; historical Tape remains
append-only. No Kernel/ADR/SPEC/STATUS/Evidence writes in arrange or do.

Planned gates (not executed by arrange), repository root:

```sh
emacs -Q --batch -L . -l test/test-helper.el -l test/test-authority.el -l test/test-runtime.el -l test/test-closure.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Run focused new selectors red/green first, then direct ordinary and real suites.
Real prefix adds --eval '(progn (require (quote package)) (setq package-user-dir
"/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa")
(package-initialize) (require (quote llm)))' before test loads; verify llm0.31.1
provenance. Required native tests must have non-skipped real results. Check parens,
changed whitespace, original unrelated forms and scope. Record ERT counts/exits,
optional/NFC/socket skips, separate RUNNER_GATE_OK and read-only Kernel drift.
No dependency install/network/provider/socket/GUI/userDB/HOME/CODEX_HOME/VCS actions,
new agents/panes/worktrees, O1 timer repair or other scope growth. S1/O1 remain
separate unresolved and S2 no-change; architecture-map is historical navigation.

Source-supported scope is feasible; execution proof is still pending. A genuine
Runtime/identity/precedence conflict or native chain that needs out-of-scope writes
returns blocked→plan with red retained, not a direct-seam-only downgrade. Stop
before do; completion later still requires independent check+learn, not invented
SPEC terminal state or automatic publication.

Replace authority/executor-does-not-run-after-runtime-termination's old four-event
oracle with a terminal-first zero-audit oracle, and retain separate admitted-call
coverage of the late execution gate. Use real Runtime liveness closures and actual
temporary SQLite/Tape/Closure for decisive tests, not only a lambda returning nil.
Capture Tape snapshots before/after late arrival; count callback, audit and effect
invocations. Inject cancellation in a real pre-audit callback to exercise the second
gate, and in approval to prove the admitted distinction. Test illegal synthetic
traces through unmodified real Closure as negative fixtures, never through a stubbed
verdict. Run focused Authority/Runtime/Closure suites and canonical.

If green requires Runtime or Closure production changes beyond this SPEC, mark
blocked and return to plan. Keep every failed case in the handoff.

## Evidence

Source inspection confirms the existing inactive test expects tool.requested,
approval.requested, approval.decided, tool.failed, matching the old Kernel clause.
This slice intentionally migrates that contract; it is not test hygiene. No code
has changed or repair verification run yet.

After check, learn must update paired Kernel Authority/lifecycle/action/invariant
entries and necessary durable decision records under the existing Kernel review
protocol, recording the changed audit contract and old-test replacement. Old Tape
is untouched; external consumers of the four events must handle refusal as
non-admission. This is the final serial slice; completion requires its check/learn,
not an invented terminal SPEC status or automatic publication.
