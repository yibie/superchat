# Framework regression repairs

status: confirmed
revision: 6
context_refs:
  - AGENTS.md
  - SPEC-AGENTS.md
  - docs/spec-agents/WORKFLOW.md
  - .claude/skills/capture/SKILL.md
  - .claude/skills/arrange/SKILL.md
  - CONTEXT.md
  - KERNEL.md (K26)
  - STATUS.md
  - docs/spec-agents/parallel-work.md
  - docs/adr/0004-session-editor-workspace-authority.md
  - docs/adr/0005-fail-closed-ordinary-llm-tool-admission.md
  - docs/protocols/session-editor-workspace-close.md
  - docs/protocols/kernel-review.md
  - docs/runbooks/kernel-check.md

## Problem and goal

Capture the user's confirmed repair contract, then arrange exactly five serial,
independently checked outcomes. Revision 6 captures the user-approved remaining
prompt/O3/context repairs and O4 no-change decision after independent Claude plan
check (sound), within existing Slice01. P1 and P2 independent PASS remain history
and regression baselines. This turn authorizes capture/arrange only; do waits for
leader handoff. Internal01 runs prompt → O3 → context with independent check after
each bounded subphase, then aggregate01 check + learn before02.
Other slices retain their serial check/learn prerequisites.

Source inspection confirms clear changes the UI Session without changing its
EditorWorkspace owner; Org Babel reads a Preset as a plist and assumes a symbol
skill parameter; Closure lacks seven emitted Team/Subagent observation types;
Authority records a late tool request before checking Run liveness. Test fixtures
also have ten cleanup placement defects and an incomplete mock prompt contract.

The read-only Claude report is an investigation input, not new verification:
`/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/claude_framework_test_report.md`.
Its findings map as follows: report F1 → Slice 01; F7/F8/F9 → 02;
F4 → 03; F2 → 04; F3 → 05. Report F5/F6 are explicitly preserved.
Its generic transfer suggestion and alternative Team identity design are superseded
by the user's narrower decisions below. Its provider/test counts are historical,
not this worker's test results; its HOME override is not an authorized recipe here.

## Unchanged contracts

- Tape is append-only. Old Session Entries, raw facts, Anchors, and source relations
  remain intact; a new View does not rewrite historical Entries.
- Runtime alone owns Run lifecycle, terminal publication, cancellation, quiescence,
  exact borrowed hosts, and at-most-once delivery. Revision 5 permits only its
  default presentation handoffs and LLM owner configuration plumbing to change.
- EditorWorkspace alone owns live editor membership and close. UI cannot write
  ownership markers. Names/tabs/projects are projections, never ownership proof.
- clear preserves the exact chat buffer, its original name including the primary
  name, Capability and Authority configuration; reset behavior is unchanged.
- Approval already admitted before cancellation remains an outstanding obligation;
  cancellation while approval is open still yields audit-incomplete.
- Unknown/malformed Tape and illegal execution traces stay fail-closed. Report F5
  bad-Tape handling and F6 raw-plus-Anchor recall retention are not repaired.
- Kernel layering, synchronous adapters, process-pump restrictions, tighten-only
  delegation, accepted child-host retention, and existing Team identity remain.

## Decision and boundaries

Capture writes only this SPEC; arrange updates only Slice 01; subsequent do writes
only Slice 01's listed production/tests and its state/verification summary, using
apply_patch. Other slices and knowledge remain read-only. All existing edits
belong to their authors and remain untouched. No Git/JJ writes, new agents,
panes, worktrees, installation, network, providers, user DB, or live GUI. Keep the
user-requested model unchanged. Do not change HOME or CODEX_HOME.

The approved execution order is 01 → 02 → 03 → 04 → 05. Each predecessor must
finish do, check, and learn before the next becomes ready. Slices 02–05 begin
blocked with explicit predecessor paths. Evidence references remain empty until
learn records verified evidence. This feature's future product scopes overlap
only serially; the still-confirmed older SPECs listed in STATUS have completed
implementation per their records, with administrative closure gaps. If another
worker resumes overlapping implementation, stop and return scope coordination to
leader/plan; do not solve it by creating a worktree.

## Model delta

1. Add one narrowly scoped Session View rollover action to EditorWorkspace,
   preserving Session/editor separation. This is not arbitrary buffer transfer,
   ownership adoption, a new registry, or a second Runtime state machine.
2. Correct test isolation and Org Babel's use of existing Preset/Skill semantics.
3. Closure recognizes exactly seven already-produced observation event types as
   known non-Run-lifecycle facts; observations cannot prove Run completion.
4. Explicitly revise the Kernel's inactive-call audit contract: a tool first
   arriving after terminal is rejected before admission and emits no Run execution
   audit. This is a breaking audit-contract change, not a silent implementation
   fix. The later execution gate and admitted-call accounting remain.

## Action Contracts

### AC1-R6 — Remaining View prompt, final failure and context boundaries

This is the current phase contract; rev1–5 authorization and evidence histories
below are preserved, not permission to edit their former product scopes. The
previous unresolved-owner decision is now approved as specified here, but its
implementation/verification remains OPEN and aggregate01 is not complete.
Investigation inputs (not new test results in this capture):
`/private/tmp/framework-output-remaining/WORKER_REPORT.md` and
`/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/remaining/output-remaining-plan-check.md`.

**Prompt (O1/O2), one Render View prompt ownership rule.** A live pending
`:prompt-marker` identifies an unsubmitted editable draft. Begin may adopt only
consumed/prepared pending state whose prompt marker is nil. Do not infer identity
from text, ordering or marker priority. A nonnil stale/detached marker is not a
valid prompt and must not silently qualify as consumed pending. Owner boundary
and status line must be before the live prompt, on their own line, preserving
exact draft text/properties and cursor position relative to the draft. Reuse the
shared View prompt with its existing association; do not copy association into
callers or consume/delete the draft when M-x Workflow starts. Preserve historical
User rows. Ordinary explicit absent-owner operations still never adopt pending;
repeated begin(A) under B must not reselect A.

Tests first: real M-x/public Workflow start with an editable draft, subsequent
ordinary input and prompt refresh/independent notices; real programmatic Runtime
submit with draft and no Dispatcher consumption. After final, retain exact draft
and properties/cursor, exactly one active editable prompt, and no draft in model
input, conversation history or Tape. A marker still in the buffer but no longer
on a valid prompt is a negative case, not evidence that a prompt exists. Include
consumed/prepared nil-marker adoption and stale-marker negatives without deleting
historical User rows. Product only superchat-render.el; direct tests only
test/test-native-ui.el, test/test-cmd-dispatch.el, test/test-workflow.el.

**O3, meaningful-final computation before mutation.** Compute response content,
bounded reasoning and the formatted string before ANY View/state mutation:
including state creation, prepare, publish, status clear, marker detach or region
delete. Read an existing state nonmutatingly when computing inputs. Nil-content
round-close keeps its existing idempotent semantics. Preserve original error/quit
propagation and Runtime delivery warning, durable terminal publication and once
semantics. No broad transaction abstraction, fallback, retry or rollback promise.
This adds same-owner integrity while preserving P2 other-owner guarantees and P1
retained-end/following-marker protection. Real Runtime guard/SQLite/Tape/Closure
tests with transport-only mocks inject formatter error and quit: prior View text,
properties, state/marker identities and projection remain intact; exact final is
durable, Runtime still closes/delivers once and warns as before. Healthy rendering
of persisted content remains possible, without claiming automatic live-View repair.
Product only superchat-render.el; direct tests only test/test-native-ui.el,
test/test-cmd-dispatch.el, test/test-llm-backend.el.

**Context, explicit legacy versus claimed eligibility.** Agent Loop distinguishes
nil legacy context from nonnil claimed Runtime context at wrapper creation. For
a claim, capture eligibility once from public Run status, requiring matching
Session/Run identity and the actual published custom transcript-owner. Render
continues to receive only an opaque owner; never substitute Run id, introduce an
owner sentinel, pass a fake nil target or requery eligibility at delivery. Keep
existing exact-buffer/Session arrival gates, no liveness presentation gate.
Nil legacy compatibility is retained, not claimed as Run isolation.

An unresolved claim withholds BOTH tool call and result presentation. Emit at
most one warning per wrapper outside the transcript via existing display-warning;
publish the warned guard before calling the diagnostic to resist synchronous
reentry. Decide eligibility before effects. Narrowly contain eligibility/diagnostic
failures, including injected error AND quit, so diagnostics cannot prevent effects,
counts, sync results, audit or async at-most-once consumer delivery. ignore-errors
alone does not catch quit. Do not wrap Authority, implementation or consumer in
that containment; preserve their original quit. Do not broaden existing valid
render callback failure policy without new evidence and plan approval.

Cover sync/async, direct call/result and evidence-action-recorded presentation
sites, warning error/quit/reentry, valid/custom owner, nil legacy, Session-only,
unknown/mismatching Run claims, clear/dead/rename and delayed result routing.
Use actual make-wrapper → wrap-tool and Authority evidence/approval paths, with
real Runtime status and no fabricated admission evidence. Product only
superchat-agent-loop.el; direct tests only test/test-agent-loop.el,
test/test-cmd-dispatch.el, test/test-llm-backend.el. Authority remains read-only.

Read-only binder trace: Runtime submit constructs full :session-id/:run-id context
(superchat-runtime.el:1647); LLM creates the Agent wrapper under that context
(superchat-llm.el:1739), and make-wrapper copies it (superchat-agent-loop.el:512).
Authority context derivations copy the whole plist; approval-context changes only
:causation-id (superchat-authority.el:1397), dynamically binds that full context
around the approval callback (:1406), then derives decision-context by another
copy. Thus this shipped binder preserves full identity, including resolution of
custom owner through public status; it is not a Session-only producer. This is
source reachability evidence, not a newly run approval-path test. Context do must
exercise a real Runtime-origin wrapper/approval callback identity path and record
its result; custom callbacks supplying incomplete claims use the approved warning/
withholding policy, not a silent exclusion or Authority change.

**O4 no-change regression.** Keep existing bounded/unbounded assistant regions
and actual borrowed/headless producers. Independent-turn insertion may establish
the necessary end boundary; generic insert-transcript must not auto-seal. No new
registry, facade, owner identity, Tape schema/replay/formatter redesign. O4 has no
production repair; retain read-only producer tracing and direct Render/Runtime
regression of unbounded stream → independent notice → final preserving the notice.

Existing P2 expected-green meanings: default-handoffs-initialize-without-status
keeps its one User prompt but now retains the live prompt rather than adopting it;
begin-existing-owner-does-not-reselect still adopts consumed/prepared pending;
absent-owner-does-not-adopt-pending and real-workflow-headers-prompt-and-interleaved-owner
(which consumes first) retain their guarantees. P2 error/quit tests retain other-
owner projection/propagation and gain O3 same-owner preservation. Positive
agent-render/session-gate-keeps-execution-and-delayed-delivery must use real valid
published identity; its old Session-only presentation expectation is explicitly
replaced by a negative withholding case. The real make-wrapper-wrap-tool-captures-
original-session test must also start with valid published identity so empty output
after clear proves Session capture, not merely unresolved-claim rejection.

Only the listed phase product/tests are writable during future do; Runtime,
Workflow/LLM production, Authority/Closure/DB/Tape, EW, UI, runner/helper and all
knowledge remain read-only. Carry all P1/P2/original clear regressions, real llm
0.31.1 provenance, focused/canonical counts/exits/skips and known Slice02 fixture
failures honestly. No tests are claimed run for this capture/arrange. Prompt and
O3 belong to01, not02. No sixth issue; evidence_ref remains empty until learn.

### AC1-P2 — Explicit transcript-owner presentation

Historical rev5 phase, independently PASS; AC1-R6 governs current execution scope
and the now-approved formerly OPEN context policy. All P2 guards remain required.

User-approved compatible refine; no new concept, identity, registry or Authority.
Render receives an opaque transcript-owner value, which may equal the default
Run-id string, and never queries Runtime. Custom Request owners remain intact.
Placement (:current-run / :independent-turn) is distinct from that opaque value.
Internal explicit owner calls ignore nil or wrong ambient Render owner bindings.

Interface: append optional transcript-owner arguments to existing prepare,
stream, status, reasoning, close and process-result entries; insert-transcript
keeps its placement argument and adds the concrete owner separately. Preserve old
argument positions and legacy omitted/nil entry behavior in a compatibility
adapter. This is not endorsement of broader legacy semantics: claimed Session
with unresolvable Runtime owner remains OPEN, blocking aggregate01 closure but not
this authorized phase. Do not suppress, reinterpret or silently legitimize it.

Separate begin(owner), ordinary owned operations, and View prompt maintenance:

- begin initializes an owner's presentation once, including the legitimate
  pending adoption/selection previously implicit in lookup. Repeating begin for
  an existing owner is not reselection: while B is selected, repeated begin(A)
  must leave B and its compatibility projection selected. No initialization
  registry or parallel owner/marker association is authorized.
- Call begin before Runtime DEFAULT initial-status, default stream and default
  delivery operations (current sites superchat-runtime.el:1900, :1915, :990),
  after the existing target/Session gate. This supports paths without an initial
  prepared status. Workflow start calls begin(correlation-id) before its first
  header (superchat-workflow.el:555), then ensures its editable View prompt.
- Ordinary explicit A operations with absent A state create A's state without
  adopting pending, copying B's projection or selecting A. The repeated-begin
  rule also applies when A's state already exists from such an operation.
  Initialization must not be inferred from marker priority or callback lateness.
  If a real path reveals a new conflict between first initialization and current
  View selection, preserve its red trace and return to plan, rather than adding
  an implicit selection policy or new state registry.
- Owned operations publish compatibility only when their owner equals the
  current owner (equal comparison). Non-current operations leave the seven
  compatibility variable identities intact throughout success/error/quit;
  live marker positions may move correctly with text insertion. Preserve all
  existing error/quit marker/insertion-type regression guarantees; a report's
  pre-existing-problem observation is not permission to weaken acceptance.
- Consolidate Workflow ensure-view-prompt and process-result prompt maintenance
  into one Render View body. Explicit View prompt creation remains distinct
  from owner selection; existing independent/pending prompt and draft behavior
  remain. No generic insert-transcript auto-seal.

Runtime binds new superchat--llm-transcript-owner next to existing LLM request
configuration dynamics, using the published entry's :transcript-owner. LLM
captures that opaque value lexically once in generate-answer and passes it to
its existing round-end close. This is transport configuration capture, not ambient
Render owner addressing. Neither Render nor LLM queries Runtime for this value,
uses a Run-id substitute or depends on the transport timing struct. Nil headless
configuration enters existing compatibility behavior.

Migrate both Agent Loop call/result sites after their captured Session/exact
buffer gate; keep actual public-status owner capture at wrapper creation. Migrate
Runtime default status/stream/deliver and default audit-warning presentation; UI
status/reasoning; the complete Workflow start/step header, stream, delivery close,
failure/final summary and prompt chain. Keep Runtime custom stream/delivery callback
arity and localized dynamic compatibility binding. Remove the UI Run-id fallback
only for real Runtime statuses; preserve the renderer-only owner branch.

Delete call-with-owner only after both production callers and their restoration
need are gone, with no remaining references requiring the function. Remove only
migrated internal dynamic branches; keep the dynamic variable/legacy adapter and
custom callback bridge while required. Keep tool-before-record ordering close and
LLM round-end close, P1 meaningful-final/retained-end/following-marker behavior,
nil-close idempotence, exact-buffer/Session gates and lifecycle/Authority/Tape/
Closure execution, counters, sync returns and callback-once behavior.

Timing values remain pre-existing View-local values, not per-Run state. Final
annotation targets the actual owner's state without selecting it; no per-Run
timing isolation or transport timing callback interface change is claimed.

Verification is tests-first: explicit nil/wrong ambient; A absent-state with
pending; repeated begin(A) while B is selected; actual default LLM round-end;
real A/B/custom-owner late sync/async, clear/dead/rename/replacement, Workflow and
custom callback compatibility; error/quit marker guarantees; all P1 and original
AC1 regressions. Real SQLite/Tape/Authority/Closure guard and native llm wrappers
remain enabled, only transport mocked. Adjust fixed-arity stubs in the authorized
tests (including test/test-workflow.el:739) without deleting behavioral assertions.

Current P2 production scope is exactly superchat-render.el (owned operations/View
prompt), superchat-agent-loop.el (tool presentation), superchat-runtime.el (default
presentation and LLM owner configuration only), superchat.el (status/reasoning),
superchat-workflow.el (presentation chain), superchat-llm.el (captured round-end
owner only). Direct tests: test/test-agent-loop.el, test/test-cmd-dispatch.el,
test/test-native-ui.el, test/test-llm-backend.el, test/test-runtime.el,
test/test-workflow.el. EW/compact/subagent tests are read-only regression. No
runner/helper, Authority/Closure/DB/Tape production or knowledge writes.

### AC1-P1 — Final content after a tool round

Historical rev4 phase authorization below is retained; rev5 AC1-P2 now governs
current scope. Independent P1 PASS and its assertions remain regression baseline.

Revision 4 is the user-approved compatible P1 repair only. Meaningful final
content must ensure its own response segment at the retained transcript-owner
end, without requiring a status marker or a prior partial. Preserve preceding
narration/tool records, custom transcript-owner, exact buffer identity and
at-most-once final delivery. Nil-content round-close remains idempotent and must
not create an empty Assistant. Rename follows the captured live object; dead or
same-name replacement buffers receive no retargeted output.

The current P1 do write scope supersedes the broader historical Slice01 scope:
production only superchat-render.el; P1 tests only test/test-llm-backend.el,
test/test-cmd-dispatch.el and test/test-native-ui.el. All other production,
tests, runner/helper and knowledge are read-only. No owner architecture refactor,
call-with-owner migration, generic insert-transcript seal or callback compatibility
change is authorized. The unresolved-owner finding remains OPEN; neither broad
legacy compatibility nor suppression is selected. It does not block this bounded
P1 phase, but prevents claiming complete Slice01 closure. P2 is not authorized.

Tests-first verification uses real Runtime/LLM loop/SQLite/Tape/Authority/Closure
with the terminal guard enabled and only model transport mocked. Do not stub Tape
append or bypass Authority evidence. Cover sync/async tools, string/plist finals,
no narration, multiple rounds, partial control, custom owner and exact-buffer
rename/dead/replacement behavior. Verify View plus history/durable outcomes.
Use only the existing isolated llm 0.31.1 dependency tree and record actual load
provenance, focused/canonical exits and skips. Probe reports with disabled guards
and mocked Tape are investigation, not acceptance. No provider-universal claim.

### AC1 — Clear through exact Session View rollover

Preconditions/input: one exact live owned session-view buffer, its exact old
owner and role, matching UI Session, and a distinct fresh empty legal target
Session. Validate target availability, ownership metadata/locality, old scope
legality, and public Runtime active statuses. Ordinary unowned borrowed buffers
are legal Runtime input but are not eligible for this ownership action.

With active Runs, ask yes-or-no before mutation. Refusal leaves Run, buffer,
Session, state, markers, config, cache, and tabs unchanged, with manual cancel /
public close and retry guidance. After agreement, guard synchronous reentry before
cancel callbacks. Use real Runtime cancellation; after cancel returns, revalidate
all exact identities, role, target emptiness/legality, scope state, and absence of
active Runs. A newly submitted/reentrant Run invalidates the attempted publication.

Permitted effects: EditorWorkspace performs the dedicated rollover and publishes
ownership coherently with the UI Session change; UI clears its View/context using
existing render/reset seams. The same buffer and name continue with a fresh Session
and no implicit old-context lineage. Preserve Capability/Profile, Topology,
Authority policy/mode/allowlist/guardrails and applicable configuration.

Failure invariant: consecutive setq-local forms are not atomicity evidence.
error/quit at publication or synchronous reentry cannot leave UI Session and owner
half-published. Establish protected publication/rollback within the approved seam,
including View-reset failure consistency. Already performed cancellations cannot
be rolled back; report their actual statuses and remaining work, never claim a
zero-effect failure after cancellation. If satisfying this requires extra Runtime
behavior, a new state registry, or a broader ownership design, mark blocked and
return to plan before expanding scope.

Old protocol hosts remain owned by the old scope, discoverable and closable through
public EditorWorkspace operations; live survivors must not be labelled closed.
Tab handling targets actual associated Session tabs, never blindly the current tab,
and must preserve an entry point to old survivors. Primary M-x superchat must find
the rolled-over primary View, not reopen the old Session through a stale tab.

Verification: clear → actual chat submit → exactly one new-Session Run; real
temporary Tape proves old Entries remain and do not enter the new prompt. Test
configuration identity, primary reopen, foreign/malformed/locality cases, refusal,
partial cancellation, cancel callback reentry, publication error/quit, and retained
scope discovery/public close. Real Runtime cancel followed by late stream, wrapped
tool, custom delivery, and synchronous reentry is mandatory red/green coverage.
The AC1 late-tool oracle is no effect or contamination of the new View/Session;
AC5 owns the separate old-trace zero-new-audit migration. A failed AC1 isolation
matrix cannot be deferred to Slice 05 or dismissed as out of scope.

Revision 2 additionally authorizes the existing Agent Loop tool-presentation gate:
capture expected Session from runtime-context :session-id when the wrapper is
created, together with the exact target buffer. Existing agent-render-buffer and
call/result paths compare that captured identity with the live View immediately
before call rendering and when a result actually arrives, including delayed async
results. Never derive expected identity from the new View or inspect EditorWorkspace
markers. Nil old context and non-chat borrowed buffers retain compatibility aligned
with Runtime's default View-session predicate. Suppressing presentation leaves
Authority execution, counters, sync return and async at-most-once callback intact.
Runtime liveness is not a presentation condition: same-Session legitimate refusal
remains visible. No changes to general llm callback-in-view or Runtime production.
The Session gate alone does not isolate different Runs in the same Session.

Direct tests cover matching/mismatching Session with delivery, nil compatibility,
renamed/dead exact objects, Session change before async completion, and at least
one actual agent-make-wrapper → wrap-tool chain proving capture at creation.

Revision 3 includes the confirmed same-Session derived issue in this same Slice.
Agent Loop captures the actual :transcript-owner through public Runtime status
using runtime-context :run-id at wrapper creation. Never infer it from run-id,
the new View or the creation-time dynamic owner (which can be nil); custom Request
owners remain authoritative. Apply Session admission first. Nil legacy context
retains compatibility without claiming Run isolation.

Render alone appends a late call/result at the end of its already closed owning
Run block and restores the previously selected UI owner and complete compatibility
marker projection, including on error/quit. Binding owner alone is insufficient:
it can insert before the old cancellation block and leave that old owner selected.
No new registry, parallel marker system, independent-log downgrade or hidden record.
Keep Authority execution, counting, return/callback behavior and AC5 old audit
contract unchanged. Runtime, Authority, Closure and llm callback-in-view remain
read-only; the new production scope is only the necessary existing Render append
position and projection-restoration seams.

Verify real/custom owners, A cancel → B midstream → A late sync/async tools,
B finalization versus control (one Assistant, no fragments), subsequent Run C,
late records at closed A's end outside B, restored B current-owner and compatibility
markers, later unbound UI/independent-turn operations, error/quit restoration and
clear's Session-first rejection. Existing Slice01 direct test files remain the
only test write scope. All previous AC1 cases remain required.

### AC2 — Restore failure-safe test cleanup and faithful prompt fixture

Precondition: enumerate the exact ten reader-confirmed single-body unwind-protect
forms listed in Slice 02. Move each existing cleanup into its unwind cleanup area,
preserving assertions and lifetimes. Nine embed cleanup in protected progn;
the core-pipeline case places delete-file after the entire unwind-protect. Both
forms skip cleanup on assertion failure; record this source correction explicitly.

The mock LLM prompt must implement append-response's content and role semantics,
including successive responses, when real llm is loaded. No no-op replacement,
blank content, or assertion deletion to hide the defect. The p4 tool-count drift
is a pollution consequence, not authority to change product tool counts.

Verify every cleanup by deliberately failing its protected body after resource
acquisition and inspecting the real released resources, including timers, tools,
buffers, files/directories and DB handles as applicable. Verify restored symbol
bindings, faithful appended content, and subsequent tool surface assertions in
the same process, then ordinary isolated and real-llm-loaded suite paths.

### AC3 — Org Babel Skill context

Input: expanded Babel body plus symbol or string skill name. Use the existing
Skill loader and superchat-preset-skill-body. A present real skill body precedes
the Babel body; missing skill silently returns the body unchanged. Preserve
model/system options, blocking exact-buffer identity and all other adapter behavior.
Verify with real temporary skill files and the existing loader, not a fake plist.

### AC4 — Subagent observation-compatible Closure

Recognize exactly team.task.created, team.task.updated, team.message.sent,
team.decision.published, subagent.started, subagent.completed, subagent.failed
as known non-Run-lifecycle events. No prefix wildcard. Retain schema, provenance,
causal/identity validation and unknown fail-closed behavior; these observations
alone must never establish a terminal Run or discharge effects/approvals.

Verify real no-parent-Session asynchronous Subagent success/failure and a real parent
Runtime Run → asynchronous delegate tool → child Runtime → callback/handoff chain,
with actual Authority, Tape, Closure and Team event producers. Explicit team-id
retains precedence. Only if that real chain proves ambient fallback wrong may
superchat-subagent.el use the exact parent tool-context Session for fallback;
record the red evidence before that conditional change. No new Team identity.

### AC5 — Reject terminal-first tool arrivals before audit admission

For a valid Runtime-bound tool context, check liveness at the earliest execution
entry before intercept/normalizer/resolver/policy/approval/effect work, and again
immediately before the first Run execution audit append. Terminal-first arrival
returns a readable refusal (sync or async callback once), with zero new Run
execution audit and zero effect. Recheck before execution remains mandatory.

If admission already wrote tool.requested and approval is pending, cancellation
retains audit-incomplete accounting. Do not reinterpret old/forged unknown or
illegal late execution as benign; trace-well-formed stays invalid for illegal
transitions (other public judgments may expose unknown by their existing lattice).
Real submit/cancel/late-wrapper tests must show unchanged terminal Tape and proved
durable cancellation after terminal-first rejection, plus an active-to-terminal
race inside a pre-audit callback. Keep prewrite failure blocking and both wrapper
paths' shared Authority boundary.

## Seams and verification

| Slice | Product write scope after leader authorization | Direct test write scope |
| --- | --- | --- |
| 01 P2 (current) | superchat-render.el; superchat-agent-loop.el; superchat-runtime.el; superchat.el; superchat-workflow.el; superchat-llm.el, only AC1-P2 responsibilities | test/test-agent-loop.el; test/test-cmd-dispatch.el; test/test-native-ui.el; test/test-llm-backend.el; test/test-runtime.el; test/test-workflow.el |
| 02 | none | ten named forms across seven files in Slice 02; test/test-llm-backend.el mock fixture and directly necessary fixture regressions |
| 03 | ob-superchat.el | test/test-ecosystem.el |
| 04 | superchat-closure.el; superchat-subagent.el only if AC4's real test proves need | test/test-closure.el; test/test-subagent.el |
| 05 | superchat-authority.el | test/test-authority.el; test/test-runtime.el; test/test-closure.el |

All other production and test files are read-only, including Runtime outside
AC1-P2's limited plumbing, test helper, and canonical runner. Direct integration tests may invoke read-only
modules without gaining write ownership. New tests go in the listed existing
canonical test modules. This keeps each acceptance oracle reachable within its
slice; missing implementation authority is a plan blocker, not a test waiver.

Use fresh batch Emacs with test/test-helper.el loaded before product modules,
temporary SQLite/data/skill paths and isolated process-local registries. Mock only
provider/transport boundaries needed for deterministic scheduling; do not stub
the authorities under acceptance. No live service is required. Existing installed
llm dependencies may be activated via isolated package-user-dir/package-initialize
and existing load paths; missing dependencies block the real-llm gate, never justify
installation or pretending the gate passed. Keep ordinary mock and real-llm results
separate. Canonical entry: emacs -Q --batch -L . -l test/run-tests.el.

Each do handoff supplies focused red/green cases, canonical results, exact command
and load environment, skips and unmet gates, plus citation drift for learn. check
verifies contracts, engineering boundaries and references. learn alone records
Evidence and knowledge, following Kernel review/pairs/strict-reference protocols;
leader coordinates the required independent fresh-context review. This worker
does not create an agent to perform it.

## Compatibility and migration

Revision 5 overrides historical phase restrictions only for AC1-P2's six listed
files. The independent plan check source is
`/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/p2plan/output-p2-plan-check.md`.
Its begin/capture corrections are incorporated above; its marker-error observation
does not waive existing regression guarantees. P1 PASS is reused, not rerun here.

Exact current Kernel mechanism sentences for later paired learn revision in
"Render the Chat Buffer Transcript" (K26, lines 697–700):

Rev6 adds these precise pending learn changes, without editing Kernel now:

- In sentence 1 below, qualify pending adoption: only consumed/prepared pending
  with nil prompt marker is adoptable; an unsubmitted live View prompt remains
  associated with pending and is reused before/after Workflow/programmatic submit.
- "A workflow step and ordinary input can therefore stream concurrently while
  retaining separate owner regions; the workflow keeps an independent prompt in
  the original View." Clarify shared View prompt reuse/association and draft
  preservation, rather than creating a second prompt or adopting a live draft.
- "A persistent system notice is inserted before the current prompt without
  consuming unrelated prompt or draft; this includes the explicit warning that
  a metadata-only attachment did not inject the full file body, although its
  metadata block still includes a prefix of up to 500 characters." Keep this
  invariant and pair source/verification references with prompt refresh/reuse.
- "when the optional Render presentation module is absent, Runtime's default
  initial-status and stream handoffs are no-ops while provider execution, terminal
  publication, delivery, and quiescence continue; when a Render function is
  present, Runtime invokes it and does not reinterpret its `error` or `quit` as
  absence." Preserve this distinction. Add meaningful-final pure computation
  before View/state mutation and retain terminal delivery warning/once behavior;
  do not promise automatic repair, fallback, retry or general rollback.
- Sentence 4 below needs the claimed-context eligibility/one outside-transcript
  warning boundary: nil legacy is distinct from unresolved claims, with no change
  to Authority effects/audit/consumer delivery or opaque owner identity.
- Sentence 3's bounded/unbounded behavior remains O4 no-change; update citations
  only where necessary. Identity, lifecycle, exact-buffer and Tape invariants stay.

1. "Runtime binds a concrete Run owner around its stream and delivery callbacks;
   before a request is accepted, the editable prompt is held by the pending owner."
   Revise mechanism to explicit default handoffs, once-only begin/adoption and
   LLM lexical configuration capture; binding remains for custom compatibility.
   Keep the pending identity/lifetime contract.
2. "A selected owner may have a status marker, assistant start/end boundary,
   persistent boundary, prompt marker, accumulated text parts, and reasoning block."
   Clarify these are owner states even when not selected; no new state authority.
3. "the selected owner's assistant subregion has two forms: bounded, with start
   and end markers, where stream writes at the moving end and close rewrites only
   that range; or unbounded, with no end boundary, where stream and close fall
   back to the owner prompt line or `point-max`."
   Replace selection-dependent wording with addressed owner; pair with already
   approved P1 opening/retained-end behavior without weakening bounded rewrites.
4. "Current-Run tool call/result records use owner `:current-run` and are inserted
   into the ordered Run transcript block after that round, outside a later
   assistant-subregion rewrite."
   Distinguish placement selector from explicit opaque transcript-owner; preserve
   ordering and both necessary closes.

Review source/verification citations and paired prompt/Workflow/timing sentences
with these changes. Preserve verbatim identity invariant "Owner is explicit rather
than inferred from text or marker priority; no ownerless placement choice or
caller-maintained marker association is used." Keep exact object, Runtime lifecycle,
Authority and append-only Tape invariants. View-local timing is a retained limit,
not a new per-Run guarantee. Only learn writes Kernel after verification; no Kernel,
CONTEXT, Protocol, schema or persisted data migration occurs in capture/arrange.

AC1 retains user-visible clear semantics while adding the narrowly authorized
EditorWorkspace ownership transition. Existing exact ownership/close invariants
remain. Existing old hosts and Tape require no migration or rewrite. Update the
relevant Session/EditorWorkspace/clear/tab action knowledge only after verification.
Revision 2 adds a compatible Session presentation boundary at Agent Loop's existing
tool-render selection seam. No identity, ownership, Runtime lifecycle or persisted
data migration changes. CONTEXT terminology and the EditorWorkspace close protocol
remain unchanged; after verification learn reviews the affected transcript/Session
Kernel clauses through the existing Kernel review protocol.

AC2/AC3 preserve intended fixture and Skill contracts. AC4 extends only the finite
known-observation vocabulary and keeps existing identity and lifecycle contracts.

AC5 deliberately replaces K26's statement that every inactive call records failure
and the four-event expectation in authority/executor-does-not-run-after-runtime-termination.
Split that old test into terminal-first zero-audit refusal versus already-admitted
execution-gate rejection/approval-cancel accounting. Retain the latter gate. Old
persisted bad traces are not rewritten, repaired, grandfathered or made valid.
External callers relying on the old four audit events must treat the readable
refusal as non-admission; there is no synthetic Run event replacement. Unknown
external consumers, release numbering and publication remain outside authorization.
learn must update all paired Authority/lifecycle/action/invariant Kernel wording
and necessary ADR/Protocol records together after evidence and independent review;
capture/arrange do not write those files now.

## Out of scope

Generic transfer/adoption; new Team IDs or state registries; Runtime production
outside AC1-P2's default presentation/configuration seam; broad UI/transport/refactor changes; report F5/F6 repair;
dependency installation; network/provider/live-GUI/user-state testing; Git/JJ
writes; concurrent implementation; knowledge promotion during capture/arrange;
automatic do after arranging; formal SPEC closure workaround.

## Issue map

1. [01 clear](issues/01-clear.md) — ready, no predecessor.
2. [02 test hygiene](issues/02-test-hygiene.md) — blocked by 01 check + learn.
3. [03 Org Babel](issues/03-org-babel.md) — blocked by 02 check + learn.
4. [04 Subagent](issues/04-subagent.md) — blocked by 03 check + learn.
5. [05 late tool admission](issues/05-late-tool-admission.md) — blocked by 04 check + learn.

## Revision notes

Revision 5: user authorized P2 and accepted independent plan check with corrections.
Capture records AC1-P2; arrange marks the old phase stale then ready for P2 in
existing01. No do this turn. Preserve rev1–4 histories, P1 PASS, five serial edges
and empty evidence_ref. Claimed-but-unresolvable owner remains OPEN; its policy
blocks aggregate01 closure, not P2 execution. No new callback compatibility policy.

Revision 4: user approved P1 only after the output repair plan. Capture records
the compatible final-display contract above; arrange refreshes existing Slice01
for its P1 phase, preserving all earlier histories and empty evidence_ref. Five
serial slices and their check/learn prerequisites remain unchanged. No persisted
schema or knowledge migration; learn later reviews transcript citations and
approved wording after independent check. P2 and missing-owner policy stay open.

Revision 3: user confirmed the same-Session A/B late tool issue and the necessary
Render scope while rev2 do was in progress. Preserve all rev1/2 tests, logs and
blocked history; pause do for capture/arrange, then resume only 01. Five serial
slices remain unchanged and evidence_ref stays empty. The read-only Claude
probe-same-session-v2.el is investigation input, not this worker's verification.
Learn reviews affected transcript/clear Kernel statements after independent check;
no knowledge or persisted data is migrated during capture/arrange/do.

Revision 2: user approved the minimal Agent Loop/test-agent-loop scope extension
after Slice 01's four real red tests exposed stale Session tool presentation.
The historical blocked handoff and all four red tests remain, evidence_ref stays
empty, and arrange supersedes that scope blocker before resuming only Slice 01.
The third llm-generate-answer argument in those tests is correctly stream-callback;
no test correction is required on that point. All original AC1 obligations remain.

Revision 1 captures the user-confirmed five-slice plan and explicit AC5 breaking
contract migration. No new product design alternative is selected here. Read-only
Lisp-reader inspection found all ten cleanup-free unwind-protect forms; the
core-pipeline placement correction above refines the report, not the repair goal.
No product regression tests or red/green repair checks have run in this turn.
Evidence refs are intentionally empty. The only remaining execution authorization
is leader review of these concrete scopes. Conditional AC1 implementation feasibility
and AC4 fallback need must be established by their real tests; neither is claimed
solved by this document. Existing administrative SPEC-terminal gaps are unchanged.
