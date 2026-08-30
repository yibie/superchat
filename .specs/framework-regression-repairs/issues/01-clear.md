# 01 Clear preserves the exact View and starts a fresh Session

status: done
phase: rev1–6 and aggregate01 independently COMPLETE; learn recorded E-20260906-001; next arrange02
blocked_by: []
writer: do (listed product/tests only, after leader scope confirmation)
authority: superchat-render.el — sole View prompt/state/position and final rendering authority (prompt/O3 only); superchat-agent-loop.el — captured tool presentation eligibility and diagnostic boundary (context only); Runtime published identity/lifecycle, Authority execution/audit and EditorWorkspace membership/close remain read-only authorities
spec_ref: .specs/framework-regression-repairs/SPEC.md
context_ref: KERNEL.md; CONTEXT.md; docs/adr/0004-session-editor-workspace-authority.md; docs/protocols/session-editor-workspace-close.md
evidence_ref: E-20260906-001

## Goal

SPEC revision 6 AC1-R6 supersedes prior execution scopes. P1/P2/prompt/O3/Context
and the F7 test correction independently PASS; the final aggregate01 audit is
COMPLETE. E-20260906-001 was appended and verified before this backfill. Historical
phase records below retain their original scope/blocker/evidence provenance;
their pending statements are historical, not current blockers. The separate
batch5 reasoning correction is E-20260906-002 and does not belong to this Slice.

Fulfil SPEC AC1: clear retains the exact chat object, name and configuration,
then a real chat submission creates one Run in a fresh Session with no old context.

## Scope

### Current rev6 phases — prompt → O3 → context

Arrangement record: capture completed SPEC rev6 first; arrange marked the previous
rev5 execution phase stale, then made only prompt ready under the approved plan.
P2 independent PASS is retained from
`/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/p2check/output-p2-check.md`;
remaining plan check and prior investigation are linked in SPEC AC1-R6. No new
production verification is implied by this administrative transition.

| Internal01 phase | State / explicit blocked_by | Writable production | Writable direct tests |
| --- | --- | --- | --- |
| prompt (O1/O2) | independently PASS; blocked_by: [] | superchat-render.el, View prompt ownership/reuse only | test/test-native-ui.el; test/test-cmd-dispatch.el; test/test-workflow.el |
| O3 | independently PASS; blocked_by: [] | superchat-render.el, meaningful-final computation before mutation only | test/test-native-ui.el; test/test-cmd-dispatch.el; test/test-llm-backend.el |
| context | independently PASS including F7 correction; blocked_by: [] | superchat-agent-loop.el, captured eligibility and diagnostic boundary only | test/test-agent-loop.el; test/test-cmd-dispatch.el; test/test-llm-backend.el |
| O4 | independently verified no-change in aggregate check | none | use the above authorized direct regressions; Runtime tests read-only |

These are subphases of existing01, not new issues. Each bounded do stops for
independent check before the next. After context independent check, aggregate01
check + learn must finish before02 →03 →04 →05. Other Slice files/edges unchanged.
SPEC belongs to capture; only this Slice state/short verification summary is an
additional future do write. All unlisted files, Runtime/Authority/Closure/DB/Tape,
Workflow/LLM/UI/EW production, runner/helper and knowledge are read-only. No new
registry, facade, identity, replay/schema unification or generic auto-seal.

### Historical P2 phase — SPEC revision 5 (independently PASS)

The following former scope and OPEN-decision wording are preserved as history;
AC1-R6 and the current phase table supersede them, without reopening P2 do.

| Writable product | Exact responsibility |
| --- | --- |
| superchat-render.el | Explicit owned operations, once-only presentation begin, non-current projection preservation, one shared View prompt body; keep P1/marker guarantees |
| superchat-agent-loop.el | Pass captured opaque owner after existing Session/exact-buffer gate; migrate both call/result and remove call-with-owner dependency |
| superchat-runtime.el | Default status/stream/deliver/audit-warning presentation and LLM owner configuration only; preserve lifecycle and custom callback compatibility |
| superchat.el | Status/reasoning owner argument plumbing; preserve renderer-only branch |
| superchat-workflow.el | Start/step headers, stream/delivery/fail/final presentation, shared View prompt |
| superchat-llm.el | Capture superchat--llm-transcript-owner once and pass to existing round-end close only |

Writable direct tests: test/test-agent-loop.el, test/test-cmd-dispatch.el,
test/test-native-ui.el, test/test-llm-backend.el, test/test-runtime.el,
test/test-workflow.el. Fixed-arity stubs may gain the owner argument while
retaining behavioral assertions. test/test-editor-workspace.el,
test/test-compact.el and test/test-subagent.el are read-only regressions.
All other product/tests, runner/helper, Authority/Closure/DB/Tape and knowledge
are read-only. SPEC edits belong to capture, not P2 do.

P2 has no predecessor (`blocked_by: []`): independent P1 PASS and sound P2 plan
check are complete per user/leader handoff. Ready permits the next leader do
handoff, not production execution during this capture/arrange turn. Exactly five
serial slices remain: aggregate01 check + learn before02, then03,04,05. No sixth
issue. OPEN missing/unresolvable-owner policy remains a separate aggregate01
closure condition, not implicit permission for broad legacy or suppression.

### Historical P1 phase — SPEC revision 4 (preserved)

Only superchat-render.el is writable production: meaningful final content opens
its own segment at the retained owner end independent of status/prior partial;
nil-content close is idempotent without an empty Assistant. Authority is Render's
existing transcript seam; Runtime/Authority/Closure retain lifecycle/execution
authority and are read-only. P1 test write owners are test/test-llm-backend.el,
test/test-cmd-dispatch.el and test/test-native-ui.el, P1 cases only. All historical
scope below is read-only for this phase. No P2, owner-policy change, call-with-owner
refactor, generic insert seal, callback compatibility change or helper/runner edit.

Acceptance: preserve narration/tools/custom owner/exact target, final delivery once
and existing lifecycle; real Runtime/LLM loop/SQLite/Tape/Authority/Closure with
guard enabled, transport-only mocks, red View assertion before implementation.
Cover sync/async, string/plist final, no narration, multiple rounds, partial control
and rename/dead/same-name no-retarget. Record actual llm0.31.1 provenance and full
focused/canonical/gate exits/skips. Existing AC1–8 remain regression obligations.

P1 has no predecessor (`blocked_by: []`) and is explicitly authorized for do.
P1 completion is not full01 closure: unresolved-owner semantics remain OPEN.
Keep evidence_ref empty and 02 blocked until aggregate01 independent check + learn;
no sixth issue, P2, learn or 02 execution is authorized here.

### Historical rev1–3 scope (preserved)

- Product: superchat-editor-workspace.el and superchat.el, limited to the dedicated
  session-view rollover and its UI/primary/tab/retained-scope integration.
- Product extension (SPEC revision 2): superchat-agent-loop.el, only its existing
  tool presentation selection and call/result plumbing using captured Session.
- Product extension (SPEC revision 3): superchat-agent-loop.el captures actual
  transcript-owner from public Runtime status at wrapper creation;
  superchat-render.el only closed-owner append position and complete previous
  owner/compatibility-projection restoration, including error/quit cleanup.
- Tests: test/test-editor-workspace.el, test/test-cmd-dispatch.el,
  test/test-native-ui.el, test/test-agent-loop.el, only direct AC1 cases and their
  isolated fixtures.
- Read-only seams: superchat-dispatcher.el actual submit, superchat-runtime.el
  submit/cancel/status/delivery, Authority/LLM/Tape/Closure and existing Render reset.
  UI does not write ownership markers. No generic transfer or new state registry.

The existing entry points are superchat--clear-chat-and-context,
superchat--ensure-chat-view, superchat, superchat-editor-workspace-owner-session,
superchat-editor-workspace-buffer and superchat-session-workspace-close.
Knowledge files are outside do scope; hand citation/model changes to learn.

## Acceptance

### Current rev6 acceptance (all prior clear/P1/P2 guards retained)

R6-Prompt. Use live pending :prompt-marker as the unsubmitted-draft criterion.
Adopt only consumed/prepared pending with nil prompt marker. A nonnil stale marker
is neither a valid prompt nor implicit consumed identity. Never use text/order
to infer owner. Place owner boundary and status line before the live prompt on
their own line; preserve exact draft, properties and relative cursor. Reuse one
shared View prompt and its association. Real public/M-x Workflow and programmatic
Runtime submit without Dispatcher consumption must retain the draft and exactly
one active prompt after final, with no draft in model input/history/Tape. Test
stale-marker rejection and preserve historical User rows. Test consumed/prepared
adoption, prompt refresh/independent notice and concurrent ordinary input.

R6-O3. Compute content, bounded reasoning and formatted final string BEFORE any
View/state mutation, including state creation/prepare/publish/clear/detach/delete.
Use nonmutating state reads for computation. Real Runtime guard + SQLite/Tape/
Closure, transport-only mocks: injected formatter error AND quit leave prior
same-owner View text/properties/state/markers/projection intact; final remains
durable and Runtime warning/terminal/once behavior remains. Preserve P2 other-owner
integrity, P1 meaningful-final/no-partial/retained-end and nil round-close
idempotence. No automatic repair, retry, fallback or broad rollback guarantee.

R6-Context. Capture once at wrapper creation: nil legacy versus nonnil claimed
context; public Run status must match claimed Session/Run and supply actual custom
transcript-owner. No owner sentinel, nil-target trick, late requery or liveness
presentation condition. Existing arrival Session/exact-buffer gates remain.
Unresolved claim withholds both call/result presentation and warns once outside
transcript via display-warning. Publish warned guard before warning. Narrowly
contain eligibility/diagnostic error AND quit; ignore-errors alone is insufficient.
Diagnostics cannot prevent effects/count/audit/sync result/async callback once.
Do not catch Authority/implementation/consumer quit or broaden valid render callback
failure policy. Cover direct and evidence-action-recorded sites, sync/async,
diagnostic error/quit/reentry, nil/Session-only/unknown/mismatched/valid/custom owner,
clear/dead/rename/delayed delivery and actual make-wrapper → wrap-tool capture.

Binder source result: Runtime full context at superchat-runtime.el:1647 reaches
LLM wrapper construction at superchat-llm.el:1739 and Agent copy at :512. Authority
approval-context at superchat-authority.el:1397 copies the complete context and
changes only :causation-id; :1406 binds it around approval; decision-context copies
it again. Thus this shipped binder preserves identity, not a Session-only path.
No approval-path test ran in capture. Context do MUST prove real Runtime-origin
wrapper/approval callback identity and custom owner resolution; incomplete custom
claims use the approved policy, not an unreported exclusion. Authority read-only.

R6-O4. Retain bounded/unbounded semantics and actual borrowed/headless producers;
unbounded stream → independent notice establishes the necessary boundary → final
preserves the notice. No production repair or generic insertion auto-seal.

Expected-green P2 adjustments: default-handoffs-initialize-without-status retains
its one existing User prompt instead of adopting/reinserting it; begin-existing-
owner-does-not-reselect still adopts consumed/prepared nil-marker pending;
absent-owner-does-not-adopt-pending and consumed-first real Workflow interleave
remain unchanged. Error/quit cases keep P2 other-owner guarantees and add O3
same-owner integrity. Session-gate positive tests now need valid published Runtime
identity; replace Session-only positive presentation with explicit withholding
negative, retaining effects/results assertions. The real original-session capture
chain must use valid published identity so clear rejection cannot pass merely
because the new unresolved-context gate rejected it earlier.

Future do is tests-first, isolated existing llm0.31.1 and real Runtime/SQLite/Tape/
Authority/Closure guard, transport-only mocks. Retain P1 real9, clear matrix and P2
regressions, focused/canonical exits and exact skips; known realLLM fixture3 failures
remain02, not waived or repaired here. No tests ran or are claimed for capture.
Exact pending Kernel learn sentences are in SPEC rev6; Kernel remains untouched.

### Rev6 prompt independent check

Fontification correction: independent promptcheck/output-prompt-check.md reported
NOT PASS because Org replaces the face used for header validity. This required
fix is now implemented, awaiting narrow independent recheck; O3 remains blocked.
Only Render, native/cmd tests and this correction summary changed. The inserted
header carries dedicated superchat-prompt-header, rear-nonsticky for that property;
state/marker remains identity. Font-lock no longer invalidates it, while deleted/
replaced header and detached-marker negatives remain. No O3/context changes.

Fresh logs: /private/tmp/framework-output-prompt-fontification/. red.log 2/0/2/0
exit1 proves actual Org-fontified Runtime/Workflow failure. Existing72 matrix cases
remain; an additional72 run after asserted org-level-1 fontification. Direct tests
cover empty/multiline editing, repeated refontification, noninheritance into draft/
adjacent insertion, and same-text replacement invalidity. Actual native LLM
Workflow/send interleave also starts fontified. A direct fontify fixture initially
moved point; save-excursion now isolates that setup, with production point/property
assertions unchanged. Intermediate failing logs remain, not reported as green.
targeted-final.log 11/10/0/1 exit0; focused-real-final.log 318/316/0/2 exit0;
p1-p2-real.log 10/10/0/0 exit0 (P1 real9 plus P2 round-end);
canonical-final.log 1327/1296/0/31 exit0, RUNNER_GATE_OK separately recorded.
Real llm0.31.1 provenance is in focused-real.log. Whole realLLM known02 trio and
EW/compact/subagent results are reused from previous independently reproduced
prompt results, not rerun; no socket test executed. Strict/authority and parens
were run read-only; exact results/commands are in
/private/tmp/framework-output-prompt-fontification/WORKER_REPORT.md. evidence_ref
stays empty; no independent PASS or aggregate01 closure is claimed.

Pending. This gate blocks O3; evidence_ref remains empty. Prompt do changed only
superchat-render.el, test/test-native-ui.el, test/test-cmd-dispatch.el and this
status/summary. Workflow tests ran read-only. No O3 preformat or context policy.

Render now adopts only nil-prompt pending; no-arg prompt creation targets pending;
shared prompt reuse normalizes its existing association. State-associated marker
plus surviving prompt-header face validates an editable prompt without text/order
identity. New owners reserve their own line before live input; shared prompt is
also the independent-notice fallback after terminal. Output preserves point with
save-excursion. Existing close/marker error/quit logic remains, including O3's
still-unfixed formatter-failure behavior. No caller, registry or lifecycle change.

Logs: /private/tmp/framework-output-prompt/. red-verified.log: 3/1/2/0 exit1
(Runtime pending identity and O2 failure/notice duplicate). reserve-red.log:
4/2/2/0 exit1 (separate real Runtime/Workflow live-draft failures).
final-notice-red.log: 2/0/2/0 exit1 (post-terminal notice entered draft).
Initial red.log was a new-test parse failure, not product evidence. Intermediate
direct-first.log caught an accidental nested macro during point-preservation edits;
fixed before final gates. All traces retained, no test assertions removed.

Final total/pass/fail/skip and exit: focused-real.log (native/cmd/workflow/agent/
runtime) 315/313/0/2 exit0; direct-ordinary-final.log (native/cmd/workflow)
172/168/0/4 exit0; llm-real.log 196/193/3/0 exit1, excluding the prohibited local
socket test by selector; readonly-real.log (EW/compact/subagent) 121/121/0/0 exit0;
canonical-final.log 1324/1293/0/31 exit0, RUNNER_GATE_OK separate from ERT verdict.
Real dependency provenance is installed isolated llm0.31.1/llm.elc, recorded in
direct-real-first.log. P1 real9, P2 real round-end/Workflow, original capture-chain
and clear gates pass non-skipped in their real suites. The new real-LLM Workflow
draft→ordinary-send case passes there; ordinary/canonical skip it explicitly.
The two live-draft tests cover 72 combined transport-adapter scenarios: Runtime/
public Workflow × status on/off × stream/final-only × empty/single/multiline ×
completed/cancelled/failed, checking marker/property/relative-point preservation,
one prompt and no unsubmitted draft in input/history/Tape. Real native LLM loop
has additional Workflow/B interleave evidence; adapter matrix is not a provider test.

Known02 failures preserved: test-runtime-overlapping-tool-round-controls-remain-run-local,
test-tools-list-can-expose-all-builtins, test-tools-list-populates-default-small-tool-surface.
Skip names and dependency warnings remain in logs. No socket/network/provider run.
parens.log passes all three edited Lisp files; whitespace.log shows no trailing
whitespace in either edited test and one existing untouched process-result blank
line in Render. kernel-strict.log remains non-green: 1031 refs/210 errors/0 warnings,
exit1; citation drift and approved pending semantic revisions go to learn, Kernel
unchanged. authority-map.log reports valid form, exit0; Render matches its existing
transcript authority, not proof that all Kernel prose is current. Full command/
result report is /private/tmp/framework-output-prompt/WORKER_REPORT.md.

### Rev6 O3 independent check

Predecessor satisfied: promptcheck/output-prompt-fontification-recheck.md read in
full, independent R6-Prompt PASS. Leader explicitly authorized O3 do; context
remains blocked. No new Slice or evidence_ref.

O3 do handoff: only Render close now precomputes meaningful content's bounded
reasoning and formatted string using nonmutating existing-state reads, before
state lookup/creation/adoption/prepare/publish/clear/discard/detach/delete; existing
replacement reuses the formatted value. No snapshots/rollback/fallback/retry in
production, no catch of formatter error/quit. Closed nil round is formatter-free
and idempotent; active nil round's existing accumulated-narration close is retained.
Native/cmd direct tests plus this summary are the only additional edits; LLM test
file and all other production/knowledge remain read-only. No context/learn/02.

Logs /private/tmp/framework-output-o3/: native-red.log 2/1/1/0 exit1 and
runtime-red.log 1/0/1/0 exit1 precede implementation. Final targeted-verified.log
4/4/0/0 exit0 includes formatter error/quit × absent/inactive/active × B/no-B,
exact text/properties/state/parts/marker identity/type/status/projection/point and
overlay preservation, one prepublication format, and closed nil-round no format.
Real Runtime/SQLite/Tape/Closure guard proves exactly one durable final content row
and response.completed, completed status/warning, no duplicate terminal delivery,
unchanged live View on failure, healthy explicit rebuild after fault removal.
No automatic recovery claim. O2 prompt assertions remain unchanged.

Fresh focused-real-final.log 322/320/0/2 exit0 (native/cmd/workflow/agent/runtime),
p1-p2-real.log 10/10/0/0 exit0, canonical-final.log 1331/1300/0/31 exit0.
Real llm0.31.1 provenance in focused-real.log; P1 real9, P2 round-end, actual capture
chain, clear guards, original72 + fontified72 prompt matrix and native LLM interleave
pass non-skipped. Known02 whole realLLM failures and EW/compact/subagent results
are explicitly reused from prior independent checks, not rerun or waived. Socket
test is outside fresh P1/P2 selector and skipped by canonical's dependency gate.
kernel-strict.log refs1031/errors210/warnings0 exit1 (195 construct drift,15 blank
citations); authority-map/parens pass. Whitespace inventory only finds the prior
untouched process-result blank line, tests clean. Full commands/results/limits:
/private/tmp/framework-output-o3/WORKER_REPORT.md. Independent O3 check remains next.

Pending. This gate blocks context; original Runtime warning/durable/once and
same-owner failure guarantees require actual results, not inferred success.

### Rev6 context independent check

O3 predecessor independently PASS: scratchpad/o3check/output-o3-check.md read in
full. Context do explicitly authorized. Render and the reported reasoning-overlay
finding remain read-only and separately investigated; no aggregate01 closure.

Context do changed only Agent Loop eligibility/diagnostic plus agent/cmd/llm tests
and this summary. Each tool wrapper captures public Session/Run/custom-owner
eligibility once; nil legacy remains compatible, nonnil unresolved claims withhold
both call/result only. No later eligibility lookup/liveness gate, sentinel, Run-id
owner substitute or fake target. Warning guard is published before display-warning;
only capture/diagnostic error+quit are contained. Authority/implementation/consumer
and valid rendering error policy remain unchanged. Existing recovery-envelope
lookup is retained, not used to re-evaluate presentation eligibility.

Logs /private/tmp/framework-output-context/: red.log 1/0/1/0 exit1 before production
change (missing diagnostic on invalid claim); final64-case direct matrix covers
8 invalid/faulted identities × sync/async × warning normal/error/quit/reentry,
unchanged B text/projection, effects/count/results/callback-once and real
tool.requested/tool.completed counts. Result-first async diagnostic error/quit
delivery, original tool/consumer quit propagation, nil legacy, no post-capture
status query, rename and same-Session cancelled refusal are separately verified.
Three existing Session-only positive tests now use actual published Runtime custom
identity; original assertions remain. Actual make-wrapper→wrap-tool capture and
real Runtime-origin approval binder pass, including two actual approval.decided
events for outer/child tools and preserved public custom owner.

Real LLM evidence loop exercises sync/async error/quit/reentrant recorded callback
diagnostics with actual Authority/Tape/Closure guard, effects and final completion.
Instrumentation only observes then invokes original Authority and its real recorded
callback; no fabricated audit/admission. Artificial unknown/mismatched identity
cases retain existing audit/Closure invalid-identity cleanup diagnostics in logs;
they are not claimed as valid Runs or repaired Authority contracts.

Fresh focused-real-final.log 327/325/0/2 exit0; llm-targeted.log 11/11/0/0 exit0;
llm-real-full.log 197/194/3/0 exit1 (exact known02 trio retained, socket excluded);
canonical-final.log 1337/1304/0/33 exit0. Two added canonical skips (approval binder
and real evidence loop) have non-skipped real results. P1/P2/O3/fontified prompt/
original clear guards remain green. Strict refs1031/errors210/warnings0 exit1,
195 construct/15 blank drift; authority-map/parens pass, all four changed Lisp
files have no trailing whitespace. Real llm0.31.1 provenance in focused-real.log.
Initial result-first test parse failure and early fixture approval assumptions
were fixed without weakening contract assertions; logs retained. Full commands,
actual/reused results and limitations: /private/tmp/framework-output-context/WORKER_REPORT.md.
Independent Context check remains required; evidence_ref empty, no learn/02.

Context subsequently independently PASS in scratchpad/ctxcheck/output-context-check.md.
F7 test-only correction replaces the preflight test's unknown run-1 claim with
agent-render-test--published-context; public Session/Run/custom owner, zero warning
and exact preflight error are asserted before no-render. Temporary scoped negative
control invokes the actual wrapper recorded callback before the same preflight
error: fails specifically at (should-not rendered), exit1. Production frozen.
Fresh logs /private/tmp/framework-context-preflight/: target.log and target-real.log
1/1/0/0 exit0; agent.log 33/31/0/2 exit0; agent-real.log 33/33/0/0 exit0 with existing
llm0.31.1; static.log parens/whitespace pass. Canonical1337/1304/0/33 and other broad
gates are reused from the independent Context check, not rerun. Only agent test
and this summary changed; F7 narrow recheck pending, no aggregate closure/learn/02.

Pending. Then aggregate01 independent check and learn; no02 beforehand.

### Historical P2 acceptance (all historical AC1/P1 guards retained)

P2-1. Explicit non-nil owner operations work under nil and deliberately wrong
ambient superchat--runtime-transcript-owner. Render receives opaque values that
may equal default Run IDs; it never queries Runtime or substitutes identity.
Ordinary A operations with no A state do not adopt pending, copy B markers or
select A. Compare all seven compatibility variable identities and correct live
marker positions under B, including success/error/quit. Do not waive insertion-
type or existing marker regressions as pre-existing issues.

P2-2. Begin once at presentation initialization: Runtime DEFAULT initial-status,
stream and deliver call begin before the owned operation, after exact-buffer/
Session admission; Workflow start calls begin(correlation-id) before its first
header, then ensures the View prompt. Repeated begin for existing A under current
B leaves B selected, including late callbacks and A state made by an earlier
ordinary explicit operation. Missing-state operations never implicitly begin.
Test no prepare-for-response and skipped initial-status paths, pending adoption,
workflow header before prompt and independent editable draft. If first-init vs
selected-View semantics need a new policy/registry, retain red and return plan.

P2-3. Runtime supplies new superchat--llm-transcript-owner beside its existing
LLM request configuration bindings. Actual generate-answer lexically captures it
once; a later real tool-round callback closes that owner despite ambient B.
Neither LLM nor Render looks up Runtime or reads the timer-owner struct. This
configuration channel is not ambient Render owner addressing. Nil headless
configuration retains compatibility without claiming isolation.

P2-4. Complete caller chain migration: Agent call/result, Runtime default
status/stream/final/audit notice, UI status/reasoning, Workflow start/step header,
captured stream/delivery close/failure/final summary, LLM round-end. One Render
View body serves Workflow and process-result prompt maintenance. Keep both
necessary closes, independent-turn placement, P1 meaningful-final/retained-end/
following-marker behavior and empty-final two-layer substitution.

P2-5. Keep legacy omitted/nil entry and custom stream/delivery arity/dynamic
binding behavior, with matching counter/return/callback-once and error/quit
outcomes. Claimed Session with unresolved owner remains OPEN, not silently routed
to a newly approved legacy/suppression policy. Final annotations target actual
owner state; timing values stay View-local, no per-Run timing isolation claim.

P2-6. Delete call-with-owner only after both production callers and its
restoration need are removed. Search all production/tests for remaining required
references; update test seams rather than leave a hidden production dependency.
Remove migrated internal dynamic branches, keep only required legacy/custom
compatibility bindings and the variable they need. No generic auto-seal,
registry, typed-item facade, replay unification or lifecycle/Authority change.

### Historical AC1 acceptance (regression obligations)

1. Validate exact buffer, old owner, exact-local valid role session-view, UI Session,
   legal old state and distinct fresh empty legal target before mutation. Reject
   foreign/unowned, bad/inherited/partial markers, wrong role, identity mismatch,
   dead/replaced object and occupied/illegal target without changing them.
2. Active Runs trigger yes-or-no before all mutations, including cache/clear effects.
   Refusal changes nothing and names manual cancellation/public close retry. After
   consent, guard reentry before cancel callbacks and revalidate all conditions plus
   zero active Runs after cancellation returns. A synchronous new Run blocks rollover.
3. Publish UI Session and EditorWorkspace ownership coherently despite watcher/hook
   error/quit, cancellation callback reentry and View-reset failure. Sequential
   setq-local alone is insufficient proof. Partial cancellation is irreversible and
   reported accurately; failed rollover never masquerades as zero-effect cancellation.
4. Success keeps the exact buffer/name and original Capability/Profile, Topology,
   Authority policy/mode/allowlist/guardrails and other applicable config; reset is
   unchanged. Old Tape remains byte/content/provenance-preserved and is absent from
   the new View's actual outgoing prompt. Actual chat submit accepts exactly one
   new-Session Run; the borrowed buffer is that same object.
5. Primary M-x superchat returns the cleared primary object under its new Session.
   Tab handling follows actual Session associations, including a foreign current tab,
   headless and old remaining protocol-host cases. No live old scope is marked closed;
   old survivors remain discoverable with their stable ID and public-close entry.
6. Real Runtime cancel followed by late stream, wrapped tool and custom delivery,
   plus synchronous reentry, never contaminates the new View/Session or executes a
   late tool effect. Exercise both before and after the next real submit. AC5's old
   trace audit migration is separate; AC1 isolation must pass now within this scope.
7. Tool presentation captures expected runtime-context Session when wrapping begins
   and checks the exact live target before each call/result display, including async
   arrival. Match Runtime default View-session semantics; nil old context and
   non-chat borrowed buffers retain compatibility. Suppression must preserve
   Authority execution, counting, sync return and async at-most-once delivery.
   Test matching/mismatch, nil, rename/death and delayed Session changes, with a
   real agent-make-wrapper → wrap-tool capture case. No EW marker reads, Runtime
   liveness gate or general callback-in-view change. Revision 3 adds the next case.
8. Capture public Runtime status :transcript-owner by context :run-id when wrapping,
   preserving custom owners. Session gate remains first. For same-Session late
   sync/async calls/results, Render appends at the closed owning Run block's end
   and fully restores prior current-owner and compatibility marker projection on
   success/error/quit. Test real A cancel/B midstream, B finalization versus control
   with one Assistant/no fragments, subsequent C, exact old-block position, later
   unbound UI/independent-turn operation and clear rejection. No run-id substitution,
   creation-time dynamic-owner assumption, new registry/marker system, hidden record
   or independent-log downgrade. Nil context retains legacy compatibility only.

## Verification

2026-09-06 P2 do handoff: explicit owner operations, once-only begin, all listed
callers and actual LLM round-end capture migrated; call-with-owner deleted after
Agent/Render tests and zero remaining .el references. Custom Runtime stream/
delivery dynamic compatibility remains. Default synchronous UI mode indicator
passes the existing LLM request owner configuration without changing its custom
callback arity. One Render prompt body preserves ordinary live-prompt reuse and
Workflow independent-pending behavior via an explicit option. Error/quit restores
temporary append/rewrite marker insertion types. No unresolved-owner policy change.

Logs: /private/tmp/framework-output-p2/. Red traces: native-red.log (3/0/3/0),
round-red.log, insertion-red.log, close-red.log, prompt-red.log and
workflow-real.log (each 1/0/1/0); workflow-reselect-trace.log identifies the
previous implicit mode-indicator status path. Final total/pass/fail/skip:
focused-ordinary-final.log 504/484/0/20 exit0;
focused-real-final.log 307/305/0/2 exit0;
llm-real-verified.log 196/193/3/0 exit1 (three known02 fixture failures; socket
test explicitly excluded, not an ERT skip);
readonly-real-final.log EW/compact/subagent 121/121/0/0 exit0;
canonical-verified.log 1316/1286/0/30 exit0, separate RUNNER_GATE_OK present.
All P1 real9, real captured round-end, real Workflow, native wrapper capture and
original clear/same-Session late matrix passed without dependency skips in the
real-llm runs. Provenance: Emacs31.0.91, existing isolated llm0.31.1 .elc,
load-prefer-newer=nil; older-bytecode warnings retained. No sockets/providers.
check-parens and scoped read-only git diff --check exit0; Kernel strict reports
1031 refs/210 errors/0 warnings exit1 (read-only citation drift, exact log retained),
authority-map exit0. Kernel mechanism revisions remain learn-owned. Only six
authorized products, five direct test files and this status/summary changed;
test-agent-loop and EW/compact/subagent were verified read-only. evidence_ref
stays empty, status remains doing until independent check; aggregate01 is not
closed and no02/learn is authorized. Full commands/results: WORKER_REPORT.md in
the same temporary log directory.

P2 tests-first order: establish explicit/no-state/repeated-begin red cases and
actual LLM round-end wrong-ambient red; implement Render and migrate callers in
the six-file scope; prove real A/B/custom-owner late sync/async, clear/dead/rename/
same-name replacement, Workflow and custom callbacks; prove deletion; rerun all
P1 and original AC1 guards and the six direct suites plus read-only EW/compact/
subagent and canonical. Use fresh isolated llm0.31.1/SQLite, terminal guard on,
real Authority/Tape/Closure, only transport mocks. Preserve known realLLM3 failures
for02 and explicit dependency skips/socket exclusion. No network/socket action.
Kernel strict/authority checks are read-only; SPEC's exact Kernel mechanism
sentence list is a learn handoff, not authority to write knowledge in do.

Use test-helper temporary data/SQLite and fresh batch processes; deterministic
transport scheduling with real Runtime/EditorWorkspace/Authority/Tape/Closure.
Create the View through the product owner path, not hand-installed markers for
the success case. Negative marker cases deliberately vary exact locality.

Capture red/green for clear→submit and the full cancellation/late-callback matrix.
Inject error and quit at each publication boundary and synchronous reentry during
cancel; compare real owner query, UI Session, scope state, content/config, tabs and
public Run statuses. Observe custom delivery through actual Runtime, not direct
invocation of a closure detached from its Runtime gate. Enumerate and public-close
the old residual scope. Run focused tests, then the canonical suite per SPEC.

If tests require extra Runtime behavior or another unapproved state/ownership
design, report the failing trace, mark blocked and return to plan. Never drop a
matrix row as out of scope, or advance to 02 while 01 remains unverified.

## Evidence

### P2 capture/arrange handoff — no production execution

SPEC rev5 captures user-approved compatible P2 and the complete independent
review at scratchpad/p2plan/output-p2-plan-check.md (full absolute source in SPEC).
The old phase was marked stale, then this Slice refreshed to ready for P2.
Only SPEC.md and issues/01-clear.md were written. No suites or probes ran here.
P1 independent PASS and existing evidence are retained: real9/9, native57/57,
canonical1307/1279/0/28; known realLLM195/192/3/0 with one socket exclusion is
reused evidence, not a fresh result. Earlier reports remain below unchanged.

All review corrections are incorporated: begin call sites/once-only no-reselection,
absent-state no-adoption, Runtime LLM configuration lexical capture, opaque owner,
shared View prompt, fixed-arity test stubs, exact Kernel learn sentence list and
View-local timing limitation. Review's error/quit observation is explicitly not
a waiver of required marker guarantees. No unresolved first-init conflict was
established by the reviewed facts; a new real conflict must return to plan.

WORKER_REPORT framework_output_p2_capture_arrange STATUS=ready. Next action is
leader handoff to P2 tests-first do; no do this turn. evidence_ref stays empty,
aggregate01 missing/unresolvable-owner policy stays OPEN; no02/learn.

### Bounded assertion follow-up — framework_output_p1_assertions

2026-09-06: Independent AC1-P1 check passed with no required fix. Leader resolved
the marker finding as the already approved rev4 compatible behavior: preserving
known other-owner coincident markers does not infer identity from marker priority.
Kernel paired updates remain learn-owned; aggregate01 unresolved-owner is OPEN.

Only prepare's docstring in superchat-render.el changed in production. P1 tests
in test/test-llm-backend.el now assert the expected Assistant count
`1 + (narration ? rounds : 0)`, no pending face, no live status markers or initial
prefill residue. Dead targets retain the unchanged replacement-buffer oracle.
Added a real async/custom-owner empty-final-after-tool case and a native direct
Render nil/empty case; existing nil-close idempotence is retained and rechecked.
No policy, architecture, call-with-owner, runner/helper or other production edit.

The empty-final investigation distinguishes two existing contracts: real LLM
response-cb normalizes empty content to `[Empty response from llm]` before Runtime
delivery, so it cannot exercise process-llm-result's separate
`Assistant did not provide a response.` fallback. The real-loop case verifies
the former once in its own segment/history/Tape with proved Closure; the direct
Render case uses real isolated SQLite/history and verifies the latter once after
narration/tools for both nil and empty string. Neither behavior was changed or
presented as the other. All new assertions passed existing implementation; no
manufactured red or new semantic repair was needed.

Fresh logs under /private/tmp/framework-output-p1 (total/expected/unexpected/skipped):
assertions-real-final.log 9/9/0/0 exit0; assertions-native.log 3/3/0/0 exit0;
assertions-native-full.log 57/57/0/0 exit0; assertions-cmd.log 44/44/0/0 exit0;
assertions-llm-ordinary.log 196/182/0/14 exit0; assertions-canonical.log
1307/1279/0/28 exit0 with separate RUNNER_GATE_OK. The nine real P1 tests skip in
ordinary/canonical and are actually executed without skips in real-final.
assertions-kernel-strict.log remains 1031 refs/69 errors/0 warnings exit1;
assertions-authority-map.log valid exit0. Citation drift is retained, not repaired.

Commands use the earlier recorded fresh batch/isolated package prefix and direct
file commands. Focused selector is `^output-p1/`. assertions-real-final.log also
prints actual symbol-file/package provenance: llm0.31.1 .elc files and metadata
commit 34ac61091994c10bda45eed54433a1a87ca26af1, Emacs31.0.91.
The real LLM suite uses exactly
`--eval '(ert-run-tests-batch-and-exit (quote (not "^test-ollama-utf8-split-chunk-integrity$")))'`
after loading test/test-llm-backend.el. It runs 195/192/3/0, exit1
(assertions-llm-real.log): the same known overlapping mock-prompt/append-response
failure and two p4 tool-count pollution failures. One local socket test is
explicitly excluded, not an ERT skip or current pass. No new socket rerun,
environment permission action, external network/provider/install/user DB/live GUI,
HOME/CODEX_HOME/VCS change or new agent/pane occurred. No post-ERT hook diagnostic.

WORKER_REPORT framework_output_p1_assertions STATUS=done; stop for recheck.
Evidence_ref stays empty; no learn, P2, owner-policy decision or 02 advancement.

### P1 do handoff — framework_output_p1_do

WORKER_REPORT framework_output_p1_do STATUS=done means the approved P1 do phase
only. Independent Claude check is pending. The original unresolved-owner finding
remains OPEN; neither broad legacy nor suppression was selected. No P2, learn,
02 advancement or aggregate01 closure. Evidence_ref is intentionally empty.

Sequential capture wrote SPEC rev4 AC1-P1; arrange marked the existing Slice stale
then ready for P1, retaining all prior histories and five serial edges. Do changed
only superchat-render.el and P1 cases in test/test-llm-backend.el and
test/test-native-ui.el. Cmd tests were run without edits. Other production,
runner/helper, Kernel/Evidence/STATUS/CONTEXT and other slices were not written.

Render's existing close now ensures a segment for meaningful content even without
status/prior partial; the duplicate status-dependent branch in process-llm-result
is removed. Prepare uses the retained end before a prompt fallback and advances
coincident following markers. Close preserves following owners' marker positions
after rewriting its own segment. No new state, registry, owner architecture,
call-with-owner change, generic insert seal, lifecycle or callback-policy change.
Nil/whitespace-only close on an already closed region remains idempotent.

Logs are under /private/tmp/framework-output-p1. Valid tests-first red.log is
8 total / 2 expected / 6 unexpected / 0 skipped, exit 1: every failure is final
missing from View after real Runtime completion and durable final storage; partial
and dead-buffer controls pass. native-red.log is 2/1/1/0, exit 1: retained-owner
final absent, nil-close control passes. red-first.log was a fixture skip-unless
error, red-second.log an unconfigured test allowlist, and red-third.log exposed
the fixture's unconsumed UI prompt; they are retained but are not final product-red
evidence. The final fixture consumes/prepares the actual owned View before the
real chat producer submit and uses the real Session allowlist, not an Authority stub.

The eight real P1 cases cover sync/async, string/plist final, no narration,
three tool rounds, partial control, custom owner and exact rename/dead/same-name
replacement. They use actual llm-provider-utils-execute-tool-uses -> native
wrapped tool -> Authority, real Runtime/SQLite/Tape/Closure with guard enabled,
and only llm-chat-streaming replaced as model transport. Async completion is
delayed until submit returns. Terminal Closure is explicitly proved, tool request
counts and one run.completed are asserted, duplicate final delivery is rejected,
and View/history/Tape are checked. No acceptance authority is stubbed.

| Final log | Result total/expected/unexpected/skipped | Exit |
| --- | --- | --- |
| real-final.log (P1 selector, dependency provenance included) | 8/8/0/0 | 0 |
| native-green-first.log (P1 selector) | 2/2/0/0 | 0 |
| native.log (whole native suite) | 56/56/0/0 | 0 |
| cmd.log (whole cmd suite) | 44/44/0/0 | 0 |
| ew.log (whole EW suite, read-only) | 34/34/0/0 | 0 |
| agent-real.log (whole Agent Loop, read-only) | 32/32/0/0 | 0 |
| llm-ordinary-final.log | 195/182/0/13 | 0 |
| llm-real-unsandboxed.log | 195/192/3/0 | 1 |
| canonical-final.log | 1305/1278/0/27 | 0 |
| kernel-strict.log | 1031 refs / 69 errors / 0 warnings | 1 |
| authority-map.log | authority map form valid | 0 |

Commands run from repository root, Emacs 31.0.91, fresh -Q --batch -L .:
`-l test/test-native-ui.el -f ert-run-tests-batch-and-exit`, and equivalently cmd,
EW, Agent Loop and llm-backend. P1 selectors replace `-f` with
`--eval '(ert-run-tests-batch-and-exit "^output-p1/")'`.
Canonical: `emacs -Q --batch -L . -l test/run-tests.el`.
Strict: `emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md`.
Authority map: `bash docs/spec-agents/check-kernel.sh .`.

All real-llm commands prepend this exact evaluation before `-L .`:

```sh
--eval '(progn (require (quote package)) (setq package-user-dir "/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa") (package-initialize) (require (quote llm)))'
```

real-final.log additionally prints emacs-version, symbol-file for llm-make-tool
and llm-provider-utils-execute-tool-uses, package-alist's llm descriptor and
load-prefer-newer. Actual files are llm-0.31.1/llm.elc and
llm-0.31.1/llm-provider-utils.elc; metadata commit is
34ac61091994c10bda45eed54433a1a87ca26af1, load-prefer-newer=nil. Existing newer-source
warnings are retained. This is the loaded dependency evidence, not llm0.32 or
provider-universal evidence. No installation, external network/provider, user DB,
live GUI, HOME/CODEX_HOME or VCS changes. The existing UTF-8 test initially failed
on sandbox socket binding; explicitly approved local simulated socket rerun passed.

The full real-llm suite is NOT green. Its three remaining failures are unchanged
existing cases: test-runtime-overlapping-tool-round-controls-remain-run-local,
test-tools-list-can-expose-all-builtins and
test-tools-list-populates-default-small-tool-surface. Excluding every P1 case
still reproduces them (llm-real-without-p1.log; fourth sandbox socket failure).
existing-fixture-diagnostic.log directly records real append-response rejecting
`(mock-prompt :text "request-b" :tools nil)` as wrong-type-argument llm-chat-prompt;
the existing protected-body cleanup then leaves two p4 tools, causing 31/33 and
5/7 assertions. These are Slice02's already approved fixture/cleanup repairs,
not authority to alter their assertions or broaden P1. No such fixes were made.

Canonical retains all 27 skip names, including the eight native-llm P1 tests;
their separate real-final.log non-skipped results are the required evidence.
RUNNER_GATE_OK is recorded separately from ERT's verdict. No post-ERT kill-hook
diagnostic occurs in the final direct/canonical logs. Strict errors remain citation
construct/line drift in previously edited EW/UI/Agent Loop/Render and tests;
Render's approved final-display semantics also need eventual paired knowledge
review. Kernel stays read-only; a valid authority-map form is not semantic proof.

Stop for independent check. All rev1–3 evidence and fixture-fix history below
remain intact.

### Independent-check correction — framework_clear_fixture_fix

Independent check identified the standalone teardown diagnostic as a test fixture
defect, not an unrelated production cleanup issue. Changed only
test/test-agent-loop.el:20 from `(defvar superchat-buffer-name nil)` to
`(defvar superchat-buffer-name)`. Product configuration now initializes the value.
This correction supersedes the earlier diagnostic attribution; historical logs
remain intact. No production, SPEC, Kernel, Evidence, other Slice or broad cleanup
changes. The separate incomplete-context boundary is not implemented here.

Logs in /private/tmp/framework-clear-do:

- fixture-value-red.log: loading test-agent-loop before product produced
  `bound=t value=nil`; after requiring superchat the value remained nil despite
  its configured default. fixture-direct-red.log: ERT 32/30/0/2 followed by
  `Error in kill-emacs-hook ... wrong-type-argument stringp nil`.
- fixture-value-green.log: before product `bound=nil`; after require, both actual
  value and evaluated `standard-value` equal `"*superchat*"`; explicit equality
  assertion and process exit succeed.
- fixture-agent-ordinary.log: 32 total / 30 expected / 0 unexpected / 2 skipped.
- fixture-agent-real-llm.log: 32/32/0/0; the required real
  make-wrapper-wrap-tool-captures-original-session test passes without skipping.
- fixture-cmd-ordinary.log and fixture-cmd-real-llm.log: each 44/44/0/0. The latter
  retains actual make-wrapper→wrap-tool ordinary/custom owner sync/async traces.
- fixture-canonical.log: 1295/1276/0/19. Skips remain explicit, not passes.

All green processes exit 0, with no post-ERT kill-emacs-hook diagnostic. Direct
commands are `emacs -Q --batch -L . -l test/test-agent-loop.el -f
ert-run-tests-batch-and-exit` and the same with test/test-cmd-dispatch.el;
canonical is `emacs -Q --batch -L . -l test/run-tests.el`. Real-llm direct commands
use exactly the existing package-user-dir/package-initialize/require-llm prefix
recorded below; no install/network/HOME override. Value probes load
test/test-agent-loop.el, inspect boundp, require superchat, then compare its value
with `(eval (car (get 'superchat-buffer-name 'standard-value)) t)`.

WORKER_REPORT framework_clear_fixture_fix STATUS=done; independent recheck pending.
Evidence_ref stays empty. Stop without learn or advancing 02.

### Final do handoff — implementation done, independent check pending

WORKER_REPORT framework_clear_do STATUS=done. This is do completion only, not
independent check, learn, Evidence publication or feature closure. Evidence_ref
remains empty. Stop here; Slice 02 remains blocked. SPEC rev3 and original five
serial edges are retained, as are the rev1 blocked report and rev2 red/green logs.

Actual production writes:

- superchat-editor-workspace.el: dedicated exact Session View rollover, existing
  classifier/Run-status validation, consent before mutation, existing allocation/
  close-stack reentry guards, full post-cancel revalidation, protected identity
  restoration after error/quit, actual cancellation-status reporting, and actual
  old-tab retagging only when no old hosts remain. Freshness includes material
  rows via a read-only DB existence query; event-only replay was insufficient.
- superchat.el: reset transaction restores text plus existing detached markers and
  overlays on failure; cancellation happens before this View text transaction.
  UI does not write ownership markers. Configuration/reset behavior and ordinary
  unowned chat compatibility remain. Foreign non-chat name collisions reject.
  Primary reopen follows the fresh primary when an old tab has no View, while
  another tab's existing View retains ordinary M-x routing.
- superchat-agent-loop.el: captured Session admission and actual transcript-owner
  from public Runtime status at wrapper creation. Session gate runs before tool
  presentation; execution/counters/returns/callbacks remain on existing seams.
- superchat-render.el: retain the existing owner state's assistant-end as a closed
  block boundary, append late records there, advance coincident following markers,
  and restore the prior complete selected-owner compatibility projection using the
  existing owner state. No new registry, marker field, Runtime liveness gate or
  parallel lifecycle authority was introduced.

Actual test writes: test/test-cmd-dispatch.el, test/test-agent-loop.el and
test/test-native-ui.el. test/test-editor-workspace.el was run completely and left
unchanged. Runtime/Authority/Closure/LLM/Dispatcher, runner/helper, Kernel/Evidence/
STATUS/CONTEXT and other slices were not edited by this worker. No Git/JJ write,
new agent/pane/worktree, install/network/provider/live GUI/user DB, HOME or
CODEX_HOME change. Current Herdr caller was verified as w3:p5C.

#### Executed acceptance matrix

- Actual clear → Dispatcher submit creates exactly one completed Run with the
  same exact buffer, new Session/owner, preserved profile/config/allowlist/mode/
  guardrails/name, unchanged old Tape rows and no OLD-TAPE-SECRET in the actual
  outgoing prompt.
- Refusal retains View/Tape/Session/workspace state and an existing cache sentinel.
  Actual cancel-delivery variants cover rename, kill plus same-name replacement,
  role/owner/UI identity changes, target material/state changes and synchronous
  clear reentry. A cancellation delivery submitting a second old-Session Run
  blocks rollover. A fault after real cancellation leaves one cancelled and one
  running Run and unchanged ownership; actual statuses are reported.
- Both identity publication writes and real Render reset are exercised with
  error and quit; owner/UI Session/text/prompt marker are restored. Invalid,
  inherited/partial ownership/role metadata, ordinary unowned action input,
  mismatched UI, illegal old state, identical target and material-only occupied
  target reject. Foreign primary content/locals remain unchanged.
- Real cancellation → late stream/terminal/custom delivery/sync or async wrapped
  tool is checked before and after the next real fresh-Session Run starts streaming.
  New View is unchanged, old custom delivery stays once, effects stay zero and
  async callback count advances exactly once per invocation. No zero-old-audit
  assertion substitutes for View isolation.
- Actual native tabs in batch cover foreign current tab, primary reopen, another
  live tab's ordinary routing, retained old protocol-host discovery and public
  close. Old survivors remain open and retain their tab entry; empty old scope's
  actual tabs retag to the new Session.
- Session gate tests cover matching/mismatch with continued delivery, nil legacy,
  non-chat borrowed buffers, rename/death and delayed Session changes. A real
  llm make-wrapper → wrap-tool chain passes without a skip.
- Real same-Session A cancel → B midstream → late A sync/async, ordinary/custom
  owner matrix passes. A's call/result remain after A cancellation and before B,
  survive B finalization, and B/C match control with exactly three Assistant
  blocks and no B/C streaming remnants. Prior B owner/compatibility markers and
  later unbound independent-turn insertion remain correct. Real-llm-loaded cmd
  tests use make-wrapper → wrap-tool for every ordinary/custom owner variant.
- A separately admitted async tool delays its result until B streaming; result
  is placed in A, callback is once, B is preserved and finalizes. Its real cancel
  yields audit-incomplete because the effect was still outstanding: this is an
  observed existing contract, not a cancelled pass or Slice05 audit migration.
  Render projection restoration also passes explicit error/quit fault injection.

#### Tests-first observations and retained logs

Logs live in /private/tmp/framework-clear-do. Initial fixture syntax errors and an
approval EOF were corrected before claiming product-red evidence. The named red
logs below are the valid reruns; no assertion was removed to obtain green.

| Red log | Actual product failure | Green evidence |
| --- | --- | --- |
| red-final.log (rev1) | all four original clear/late/reentry tests failed | final cmd suite |
| gate-red.log (rev2) | captured Session selection/delayed presentation failed | rev3-agent-real-llm.log |
| rev2-clear-red.log | real submit ownership, clear cancellation/reentry and refusal failed; 3 unexpected/5 total | final cmd suite |
| rev3-owner-red.log | A late call detached B's active start marker | same-Session final matrix |
| rev3-owner-first.log / rev3-owner-second.log | owner binding alone placed records before A cancellation | rev3-retention-green.log |
| rev3-retention-red.log | B finalize erased A late records at a coincident boundary | final cmd suite |
| rev3-revalidation-red.log | material-only target was wrongly accepted | final cmd suite |
| rev3-foreign-red.log | foreign named primary was cleared | rev3-foreign-green.log |
| rev3-primary-route-red.log | unconditional primary priority stole another live tab's View | final native suite |

#### Final commands, environment and results

All commands ran in repository root with /opt/homebrew/bin/emacs (GNU Emacs
31.0.91), fresh -Q --batch, test-helper isolation before product loading, private
temporary SQLite/data roots and deterministic transport callbacks. No provider
was contacted. Commands and log basenames:

| Command | Log | Actual result |
| --- | --- | --- |
| emacs -Q --batch -L . -l test/test-editor-workspace.el -f ert-run-tests-batch-and-exit | final-direct-editor-workspace.log | 34 total, 34 expected, 0 unexpected, 0 skip; exit 0 |
| emacs -Q --batch -L . -l test/test-native-ui.el -f ert-run-tests-batch-and-exit | final-direct-native-ui.log | 54/54, 0 unexpected, 0 skip; exit 0 |
| emacs -Q --batch -L . -l test/test-cmd-dispatch.el -f ert-run-tests-batch-and-exit | final-direct-cmd-dispatch.log | 44/44, 0 unexpected, 0 skip; exit 0 |
| emacs -Q --batch -L . -l test/test-agent-loop.el -f ert-run-tests-batch-and-exit | final-direct-agent-loop.log | 32 total, 30 expected, 0 unexpected, 2 skip; exit 0 |
| emacs -Q --batch -L . -l test/run-tests.el | final-canonical.log | 1295 total, 1276 expected, 0 unexpected, 19 skip; exit 0 |
| emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md | final-kernel-strict.log | 1031 refs, 69 errors, 0 warnings; exit 1 |
| bash docs/spec-agents/check-kernel.sh . | final-authority-map.log | authority map form valid; exit 0 |

The real llm runs activated only the already existing isolated dependency tree:

```sh
emacs -Q --batch --eval '(progn (require (quote package)) (setq package-user-dir "/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa") (package-initialize) (require (quote llm)))' -L . -l test/test-agent-loop.el -f ert-run-tests-batch-and-exit
emacs -Q --batch --eval '(progn (require (quote package)) (setq package-user-dir "/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa") (package-initialize) (require (quote llm)))' -L . -l test/test-cmd-dispatch.el -f ert-run-tests-batch-and-exit
```

rev3-agent-real-llm.log: 32/32 expected, zero unexpected/skips, exit 0; specifically
agent-render/make-wrapper-wrap-tool-captures-original-session passed 1/32.
rev3-cmd-real-llm.log: 44/44 expected, zero unexpected/skips, exit 0; explicit
SAME-SESSION real-llm-make-wrapper-chain=t traces cover ordinary/custom sync/async.
These prove real dependency capture; canonical's same test skip is not a pass.
Real-llm-loaded canonical was not run and is not claimed.

#### Risks and check/learn handoff

- Canonical's 19 skip names are retained in final-canonical.log: optional llm/
  synthesis/Subagent cases, TextUI alternatives and two NFC filesystem cases.
  RUNNER_GATE_OK is only the runner contract marker; ERT's separate 1295/1276/0/19
  verdict is the result used above.
- Both ordinary and real-llm agent-loop standalone exits print a summarize
  kill-emacs-hook diagnostic (wrong-type-argument stringp nil), after successful
  ERT. It is retained, not counted as an ERT failure or hidden as clean teardown.
  No runner/helper or unrelated hygiene repair was made in this slice.
- Strict Kernel references are not green: 69 construct/line mismatches arise in
  edited EW/UI/Agent Loop/Render and direct-test references. The full list is in
  final-kernel-strict.log. Kernel is read-only here; learn must refresh references
  and paired clear/transcript statements after independent check. The form-only
  authority-map check is not proof of semantic conformance.
- Independent check is still required, particularly publication failure cleanup,
  old-tab retention, existing closed-boundary semantics and the selected-owner
  restoration. There is no claim of learn, feature closure, provider testing,
  live GUI testing or Slice05's zero-new-audit behavior. Do not start 02.

### Revision 3 arrangement and rev2 progress

User explicitly approved the derived same-Session issue and minimal Render scope.
Do paused at a test boundary, capture wrote SPEC rev3, this Slice was marked stale
then re-arranged ready; existing five edges and empty evidence_ref remain. Resume
only 01 do. Rev1 blocked history and rev2 logs are preserved, not reclassified.

Rev2 progress logs in /private/tmp/framework-clear-do: gate-red.log recorded
the presentation red; gate-real-llm.log has 4/4 expected with real existing llm
loaded (plus a summarize kill-hook diagnostic). rev2-clear-red.log has 5 total,
2 expected and 3 unexpected; rev2-publication.log has 10/10 clear cases expected,
including real submit/reentry/refusal and publication/reset error/quit. These are
intermediate results, not full AC1 or canonical completion. Remaining original
matrix obligations and all rev3 tests still require execution before handoff.

### Revision 2 arrangement

The user confirmed the compatible extension to superchat-agent-loop.el and
test/test-agent-loop.el and authorized only 01 do. The rev1 execution contract was
marked stale, then re-arranged ready under SPEC revision 2; the blocked handoff
below is preserved as history, not a current scope blocker. Five serial slices and
all predecessor edges are unchanged. Evidence remains empty until learn. All four
existing red tests and all original AC1 obligations remain mandatory.

### Do handoff — blocked, 2026-09-05

Leader authorized this slice's do after arranging. Tests-first work added only
four regression tests and their two helpers at the end of
test/test-cmd-dispatch.el, leaving existing tests and their line anchors intact.
No production change was made. This slice's status/verification summary is the
only other repository edit; evidence_ref remains empty. No learn or Slice 02.

**Scope blocker:** the required late wrapped-tool isolation fails in the existing
read-only presentation chain. superchat--agent-wrap-function in
superchat-agent-loop.el renders tool-call text before calling Authority and renders
the refused result afterward. superchat--agent-render-buffer checks only the exact
buffer object's liveness. Both sync and async wrappers therefore write into the
same live object after clear changes its UI Session. Changing EditorWorkspace
ownership markers cannot change either branch. The presentation bridge
superchat--callback-in-view is in read-only superchat-llm.el and also checks only
liveness; it is not a writable superchat.el seam. Render is the existing transcript
authority and is read-only too. An advice/redefinition from UI or a new registry
would conceal that ownership boundary rather than implement the approved seam.
Return to plan to decide a narrowly authorized existing presentation/wrapper scope;
no extra Runtime behavior or new state model has been assumed or implemented.

The minimal decisive sequence is real EditorWorkspace View acquisition → real
Runtime submit (capture its actual tool-execution context at the mocked transport
boundary) → real Runtime cancel → actual clear → invoke the actual Agent Loop
sync/async wrapper with that captured context. SQLite/Tape/Closure/Authority and
rendering are real; Closure guard stays enabled. The wrapper is constructed through
superchat--agent-wrap-function, the shipped execution-wrapper constructor; this
does not claim full llm provider tool negotiation. Tool implementation is a counted
local fixture. No authority result or lifecycle status is stubbed.

Observed red matrix in red-final.log and canonical.log:

| Case | Actual observation |
| --- | --- |
| clear → real Dispatcher submit | exact owner still clear-owned-before; new UI Session rejected by public validator, before transport |
| real cancel → clear → late stream | View unchanged |
| real cancel → clear → late terminal/custom delivery | View unchanged; cancellation custom delivery happened once, no second delivery |
| real cancel → clear → late sync wrapped tool | effects=0; tool-call and refusal-result text inserted into cleared View |
| real cancel → clear → late async wrapped tool | effects=0, callback-count=1; same View contamination |
| active Run → clear with cancellation-delivery reentry fixture | confirmations=0, deliveries=0, old Run still running-model; UI Session already changed, owner old |

The late-tool oracle makes no assertion about zero old-Run audit events. The
failure is visible new-View contamination, not Slice 05's separate audit migration.
An additional read-only real cancellation probe establishes that the reentry
fixture is reachable: first=cancelled, second=running-model,
same-exact-buffer=t, active=t, via a custom cancellation delivery that submits the
second Run while the first cancel is still on the stack. The clear-driven regression
cannot reach that delivery yet because existing clear never cancels.

### Commands, environment and actual results

All runs used /opt/homebrew/bin/emacs, GNU Emacs 31.0.91, fresh -Q --batch processes,
repository root cwd, existing test-helper loaded before product modules, and private
temporary data/SQLite roots. No real llm dependency activation, installation, network,
provider, live GUI or user DB; HOME/CODEX_HOME unchanged. Only the transport boundary
is replaced in the new tests; cleanup also prevents reentrant submission reaching
a provider. Test data directories are removed in unwind cleanup.

Logs are in /private/tmp/framework-clear-do/:

| Command | Log | Result |
| --- | --- | --- |
| emacs -Q --batch -L . -l test/test-cmd-dispatch.el --eval '(ert-run-tests-batch-and-exit "^cmd/clear-owned-view-")' | red-final.log | 4 total, 0 expected, 4 unexpected, 0 skipped; exit 1 |
| emacs -Q --batch -L . -l test/test-editor-workspace.el -f ert-run-tests-batch-and-exit | direct-editor-workspace.log | 34/34 expected, 0 unexpected, 0 skipped; exit 0 |
| emacs -Q --batch -L . -l test/test-native-ui.el -f ert-run-tests-batch-and-exit | direct-native-ui.log | 53/53 expected, 0 unexpected, 0 skipped; exit 0 |
| emacs -Q --batch -L . -l test/test-cmd-dispatch.el -f ert-run-tests-batch-and-exit | direct-cmd-dispatch.log | 34 total, 30 expected, 4 unexpected, 0 skipped; exit 1 |
| emacs -Q --batch -L . -l test/run-tests.el | canonical.log | 1279 total, 1257 expected, 4 unexpected, 18 skipped; exit 1 |
| emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md | kernel-strict.log | refs 1031, errors 0, warnings 0 |
| bash docs/spec-agents/check-kernel.sh . | authority-map.log | authority map form valid; exit 0 |

The four unexpected ERT results in both complete suites are precisely the four new
cmd/clear-owned-view-* regressions; all existing canonical results remain as expected
or skipped. RUNNER_GATE_OK is printed despite canonical exit 1: it is not a passing
ERT verdict. All 18 skip names are retained in canonical.log; they include absent
optional llm functionality, TextUI alternative-path cases and two NFC filesystem
capability cases. They are not passes. Initial helper use of skip-unless outside
an ERT body produced a fixture error; corrected to ert-skip before any product-red
claim. red-final.log is the valid rerun, not that initial fixture failure.

Kernel is read-only and reports no citation drift. SHA-256 comparison against turn
entry confirmed both authorized production files, Agent Loop/Runtime/Authority/LLM/
Dispatcher/Render, Kernel/Evidence/STATUS/CONTEXT/SPEC, runner/helper and untouched
EditorWorkspace/native test files remained unchanged. Source refs were inspected
by construct, not inferred from the form-only authority check.

### Remaining verification and next boundary

There is no green implementation, and independent check must not mark AC1 complete.
The first decisive late-tool failure blocks production work under the current scope.
After plan resolves that boundary, continue tests-first for the remaining rows:
late callbacks after the next accepted new-Session Run; consent refusal including
cache/Tape/View/state; cancellation-triggered kill/rename/role/owner/target mutations;
partial cancellation; publication error/quit and View-reset failure; preserved
configuration/outgoing prompt; primary M-x routing; actual Session tabs and old
protocol-host discovery/public close. These are unmet acceptance obligations, not
out-of-scope exclusions or deferred Slice 05 work. Keep Slice 02 blocked until 01
eventually completes independent check and learn.
