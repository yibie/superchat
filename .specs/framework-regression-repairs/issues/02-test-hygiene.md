# 02 Tests clean up on failure and preserve appended prompt content

status: done
phase: independent Claude PASS accepted; learned E-20260906-003; next arrange03, not executed here
blocked_by: []
predecessor: .specs/framework-regression-repairs/issues/01-clear.md — done after independent aggregate check and learn, E-20260906-001
writer: do (listed test forms and fixture only, after predecessor check and learn)
authority: n/a: no business rule changes; this slice repairs resource cleanup and the mock representation of the existing llm prompt contract
spec_ref: .specs/framework-regression-repairs/SPEC.md
context_ref: KERNEL.md; .specs/framework-regression-repairs/SPEC.md (AC2); test/test-helper.el
evidence_ref: E-20260906-003

## Goal

Make each of the ten affected tests release its actual resources on an assertion
failure and make the LLM mock faithfully append response content when real llm loads.

## Scope

Exactly these ten existing tests, their cleanup placement and directly necessary
failure-injection verification, plus test/test-llm-backend.el's
test-llm--with-mock-llm prompt fixture and direct content-semantic regressions:

| File | Test |
| --- | --- |
| test/test-cmd-dispatch.el | cmd/session-view-rebuild-resets-response-region |
| test/test-control-channel.el | control/cancel-command-cancels-active-run |
| test/test-core-pipeline.el | pipeline/file-reference-inlines-source-and-reports-actual-status |
| test/test-llm-backend.el | test-runtime-overlapping-tool-round-controls-remain-run-local |
| test/test-llm-backend.el | test-runtime-ordinary-wrapper-rejection-finishes-once-and-quiesces |
| test/test-native-ui.el | native-ui/status-refresh-timer-is-released-by-terminal-mode-off-and-kill |
| test/test-runtime.el | runtime/every-live-terminal-path-quiesces-exactly-once |
| test/test-runtime.el | runtime/closure-shadow-reports-disagreement-without-rejecting-run |
| test/test-runtime.el | runtime/default-terminal-delivery-resets-response-region |
| test/test-workflow.el | workflow/unicode-basename-resolution-is-bidirectional |

All production, test-helper, runner, other tests and knowledge files are read-only.
Existing tool-count assertions are verification consumers, not permission to weaken
or rewrite them. Preserve changes Slice 01 and other workers already made.

## Acceptance

1. Reader inspection proves each named unwind-protect has actual cleanup forms.
   Nine currently bury cleanup in protected progn; core-pipeline instead puts
   delete-file after the entire unwind-protect. Move each to the cleanup area with
   correct lexical resource scope. Preserve test assertions and normal behavior.
2. For all ten, deliberately fail within the protected body after acquisition and
   prove relevant tool registrations, timers, buffers, files/directories and DB
   handles are released. Log each test, injected failure point, resources observed
   before/after and outcome. A single example or macroexpand-only check is insufficient.
3. Mock append-response preserves actual content, supplied/default role, append order
   and earlier prompt content over successive appends, according to the locally
   loaded llm contract. Validate downstream content, not merely a non-error return.
   Neither a no-op append function nor removing assertions satisfies this result.
4. With real llm loaded, the overlapping-tool-round test no longer raises a prompt
   type error; p4 tool registrations are gone even after forced failure. Existing
   test-tools-list assertions pass afterward in the same process at their original
   expected surface. No product tool-count change is justified by pollution.

## Verification

### Check and learn — 2026-09-06

Independent Claude framework02-check.md PASS has no required finding or blocker.
E-20260906-003 was appended and verified unique before this backlink. Learn reuses
the checker's fresh canonical1341/1307/0/34, socket-excluded full-real201/201/0/0,
direct ordinary515/494/0/21 and real515/513/0/2, named10/10 and content2/2 each
environment,32-point acceptance probes and real native/mock differential. Its
separate19-attempt probe is qualified in Evidence:15 clean,2 bounded S1 states,
1 injected O1 timer gap,1 injection not fired, rather than19 passing cases.
S1 has no physical leak; O1 requires separate production-reachability plan.

Fresh learn gates: K28 strict1072 references/errors0/warnings0 and authority form
pass. Only86 numeric citation positions were reanchored (llm38/runtime37/native11);
enacted semantics and since fields are unchanged. No ERT rerun, product/test change,
socket acceptance,03 arrangement or03 do. Historical do/arrange records below
retain their original pending-state provenance; current02 is checked and learned.
Full learn report: /private/tmp/framework02-learn/WORKER_REPORT.md.

### Do handoff — 2026-09-06

Only the seven scoped test files changed. Reader verifies all ten original
cleanup-count0 forms now have cleanup; p4 adds a local nested unwind so accepted
Runs quiesce while its existing Tape fixture is still bound. Original assertions
and every other preexisting top-level form remain reader-equal, except the approved
mock fixture. Runtime deadline cleanup and newly acquired shadow-test host cleanup
are local to the named forms; a preexisting borrowed host remains alive/intact.

Failure injection: all31 planned acquisition checkpoints were red; p4 polluted
both unchanged tool-list consumers before harness rescue. Final ordinary and real
llm runs cover32 checkpoints (including borrowed-host retention), preserve the
original injected condition and scoped function bindings, and observe zero live
owned buffers/timers/paths/DB handles/tools/active Runs before rescue. Both p4
checkpoints then pass both tool-list consumers in the same process.

Two new fixture regressions verify original text/tools/interactions, exact content,
default/nil user and explicit roles, append return/order, and actual LLM dispatch
finalize/followup provider-observed content. Real llm0.31.1 full file excluding the
named UTF-8 socket test is201/201/0/0; the historical three failures are resolved.
Named ten: ordinary10/10 and real10/10. Seven direct files: ordinary515/494/0/21,
real515/513/0/2. Canonical1341/1307/0/34, exit0 plus RUNNER_GATE_OK. Parens, unchanged
assertions/other forms, new-line whitespace and authority-map checks pass. Read-only
KernelK27 strict:1072 references,86 line/construct drift errors,0 warnings; all86
existing named constructs located, no Kernel edit. Reanchor belongs to later learn.

Full commands, red/green logs, resource matrix, exclusions and correction history:
/private/tmp/framework02-do/WORKER_REPORT.md. Evidence remains empty; this is do
completion for independent check, not Slice check+learn completion. No production,
runner/helper, other Slice, SPEC or knowledge changes; no socket/provider/network.

### Current arrangement — 2026-09-06

The five serial edges remain01→02→03→04→05. Only02 is ready;03–05 files are
unchanged. Scope remains the ten named forms above plus the existing mock fixture
and directly necessary content/failure regressions. No production or runtime
semantic changes, runner/helper edits, other assertion cleanup or tool-count
weakening. Evidence remains empty until this Slice's own check+learn.

Fresh read-only Lisp-reader inventory: /private/tmp/framework02-arrange/reader.log
and forms.el. All ten still have cleanup-count0 and progn bodies. Current start
lines in table order: cmd111, control37, core86, llm8171, llm8591, native1090,
runtime443, runtime1725, runtime3390, workflow139. Names are authoritative; line
numbers are diagnostic. Nine cleanup tails are within protected progn; core's
delete-file is outside the entire unwind-protect. Existing Claude report F7–F9
and probe-hygiene-inject.el were read, not executed; the latter covers only three
forms and cannot replace ten-form acceptance.

Tests-first failure matrix, using scoped injection into the actual named test
after acquisition and before its cleanup. Capture exact objects outside the
test's dynamic lets; preserve the original injected assertion condition after
unwind. Harness rescue, if necessary to remove intentionally leaked red resources,
must happen AFTER observations and be reported separately, never counted as test
cleanup. No broad error swallowing or shared helper/registry redesign.

| Form (table order) | Failure point and captured objects | Required post-unwind oracle |
| --- | --- | --- |
| cmd/session-view-rebuild | before render-session-view after DB/material/View setup; capture actual View, connection, directory | exact buffer dead, captured SQLite handle rejects select, directory absent |
| control/cancel-command | after buffer/locals exist, before first cancellation assertion | captured buffer dead; scoped function bindings restored |
| core/file-reference | after file contents written; repeat each suffix including nil/png | captured exact file absent, original failure unchanged |
| llm overlapping controls | after both p4 registrations and buffers; additional checkpoint after accepted Runs captures handles/registry | both exact buffers dead, both registrations absent from actual registry; acquired Runs/timers released by test-local cleanup; original condition survives |
| llm ordinary wrapper rejection | after p3 registration/host creation before admission assertion | exact host dead and p3 name absent; do not infer cleanup from restored globals |
| native status timer | after timer acquired in each terminal/mode-off/kill branch | captured timer objects absent from timer-list/idle list and captured buffers dead; nil restored local is insufficient |
| runtime live terminal paths | after accepted handle before terminal dispatch, each kind | exact View dead; actual deadline timer absent and acquired Run resources released, not merely lost with rebound runs hash; fake transport owner is not proof of physical timer cleanup |
| runtime closure shadow | after submission/DB open before completion assertions | captured connection unusable, directory absent; capture request host and release if acquired by this test, retain any preexisting borrowed host |
| runtime default delivery | after prepared View/terminal path before assertions, each failed/cancelled/timed-out row | exact buffer dead; enclosing existing temp-store closes actual connection/deletes directory; no helper edits |
| workflow Unicode | after real file write/resolve, both NFC→NFD and NFD→NFC cases | actual file and directory absent |

Move existing cleanup to unwind cleanup with resource bindings still in scope;
add only locally necessary release of actually acquired resources within these
ten forms where an early failure would otherwise leave a timer/Run/host. Do not
count a restored let-bound registry/connection variable as release of the captured
object. Preserve original body assertions, independent failure identity and
normal cleanup behavior. Genuine scope/semantic conflict returns to plan.

Existing dependency check (no install): dependencies.log records Emacs31.0.91,
SQLite available, actual elpa/llm-0.31.1/llm.elc version(0 31 1). Source llm.el:364
defines append-response(prompt,response,&optional role): preserve prior interaction
sequence, append one interaction with role (or role 'user) and exact response
content, returning the updated interaction list. The existing fixture's mock-prompt
currently keeps :text/:tools only and supplies no append-response adapter.

Fixture acceptance must preserve original :text/tools and prior interactions;
assert default/nil user role, explicit assistant/user role, exact string content,
append order and multiple appends, plus downstream provider-observed content in
the real-loaded finalize/followup path. Retain current mock representation consumers;
do not substitute a no-op, empty content or remove expectations. Scope permits the
existing fixture and direct content regressions only, not broad mock migration.

After deliberate p4 failure, inspect the actual registration collection, then run
the unchanged test-tools-list-populates-default-small-tool-surface and
test-tools-list-can-expose-all-builtins in THE SAME PROCESS. This pollution oracle
is mandatory; fresh-process passing counts cannot prove preceding cleanup.

Planned commands (not run by arrange): ordinary named ten-test selector after
loading the seven listed files; repeat with the existing isolated llm prefix;
direct mock content regressions; same-process forced-failure→tools-list oracle;
full realLLM `-l test/test-llm-backend.el --eval
'(ert-run-tests-batch-and-exit (quote (not test-ollama-utf8-split-chunk-integrity)))'`;
canonical `emacs -Q --batch -L . -l test/run-tests.el`. Include direct suites,
check-parens/whitespace and read-only strict/authority as appropriate to the edits.
The real prefix is `emacs -Q --batch --eval '(progn (require (quote package))
(setq package-user-dir "/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa")
(package-initialize) (require (quote llm)))' -L .`. Test-helper supplies isolation;
temporary DB/config only, no HOME/CODEX_HOME change or provider.

Independent predecessor baseline is full realLLM199/196/3/0, socket excluded.
All three known failures must become green through actual fixture/cleanup repairs;
new direct tests may increase total counts. Record ordinary optional-llm and NFC
skips distinctly; real fixture acceptance cannot be waived as a skip. Socket is
separately excluded until explicitly authorized, with no new socket request here.
Arrangement ran reader/dependency inspection only, no fresh broad ERT or deliberate
failure tests and no implementation. Next is leader-authorized02 do.

First read each complete affected form with the Emacs Lisp reader and classify
cleanup boundaries; keep line numbers diagnostic because predecessors may move them.
Execute deliberate-failure probes against the actual forms/resources in temporary
isolation, catching only the deliberate assertion after unwind cleanup has run.
Restore test symbol bindings even if the probe itself fails. Separate expected
injected failures from unexpected regression failures in the report.

Run direct content-semantic fixture checks and affected tests in fresh ordinary
mock and existing-real-llm-loaded environments, then canonical. The real dependency
path must be present and explicitly loaded via isolated package-user-dir and
package-initialize as necessary; do not install/download, alter HOME/CODEX_HOME,
or use a provider. Missing real llm is an unmet gate, not a passing skip. Use existing
runner/test-helper isolation and test-local bindings; neither file is writable.

## Evidence

This capture turn ran a read-only Emacs reader traversal of test/*.el: exactly the
ten named forms have one body and no cleanup arguments, all bodies headed by progn.
Source reading independently identifies the core-pipeline cleanup outside that form.
No deliberate-failure/green repair execution has occurred yet; Claude's counts are
historical inputs only. F9 is an F7/F8 contamination consequence. Slice 03 waits for
this slice's do/check/learn and learn-populated evidence_ref.
