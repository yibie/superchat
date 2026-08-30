# 07 纠正 Subagent、Skills 与同步边界适配器生命周期

status: done
blocked_by: 05
writer:
authority: Session EditorWorkspace acquisition/rollback/ownership — `superchat-editor-workspace.el` | owned; Skills operation-child close-once — `superchat-skills.el` | owned; Subagent acceptance/pre-accept rollback — `superchat-subagent.el` | owned; synchronous host contracts — `ob-superchat.el`, `superchat-magit.el`, `superchat-rewrite.el` | owned at their integration boundaries
spec_ref: `.specs/session-editor-workspace/SPEC.md` Revision 5 — Skills match and Subagent child lifetimes；Blocking adapter identity；Issue map S5
context_ref: `KERNEL.md` K17 — Session EditorWorkspace acquisition/rollback；Skills operation-child；Subagent accepted-host lifetime；exact blocking adapters
evidence_ref: E-20260904-005

## Arrange result

This is a fresh Revision 5 correction handoff over the existing Revision 4
implementation, not a from-zero implementation.  Preserve the existing exact-
buffer migration and passing tests; correct it in place.  The Revision 4 `do`
and adversarial `check` results remain below as superseded history, not current
acceptance evidence.

`blocked_by: 05` is satisfied by `E-20260904-003`; Slice06/K16 is complete via
`E-20260904-004`.  STATUS already records Slice07 as the next correction and is
not changed by arrange.  All dirty production/test content is shared baseline:
do not restore, clean, or overwrite it wholesale.

The authorized files are sufficient.  Runtime publishes before delivery,
returns non-empty Run/Turn IDs after normal acceptance, and synchronously
delivers a published terminal Run before propagating provider conditions.
Subagent can therefore mark accepted on delivery entry before untrusted
callbacks, with valid handle return as the other proof.  Runtime/LLM changes
are neither needed nor authorized.

## Goal

Tests-first correct Revision 4 so EditorWorkspace atomically reports
acquisition freshness and rolls back only an exact fresh acquisition; Skills
closes its operation-child exactly once on every exit; accepted Subagents
retain their trackable child host while genuine pre-accept rejection rolls
back fresh-only; and all three blocking adapters prove the complete ownership
matrix without changing their synchronous contracts.

Runtime continues to borrow exact buffers and never requires/calls the middle-
tier EditorWorkspace or interprets its private markers.

## Fixed public interface

The existing API remains buffer-only and behavior-compatible:

```elisp
(superchat-editor-workspace-buffer SESSION ROLE &optional CREATE)
;; => BUFFER or nil
```

Add exactly:

```elisp
(superchat-editor-workspace-acquire-buffer SESSION ROLE)
;; => (:buffer BUFFER :newly-acquired BOOLEAN)
```

This is one authority-owned lookup-or-create transaction.  The plist is a
plain ephemeral result, not a token, lease, registry entry, or ownership
capability.  The authority computes freshness; producers never infer it from
names, markers, buffer-list scans, or separate lookups.

Add exactly:

```elisp
(superchat-editor-workspace-rollback-acquisition
 SESSION ROLE BUFFER NEWLY-ACQUIRED)
```

Contract:

- validate SESSION and ROLE through existing authority checks;
- false `NEWLY-ACQUIRED` is an unconditional no-op, without inspecting BUFFER;
- true validates the exact live owned SESSION/ROLE/BUFFER tuple before effects;
- mismatch is zero-effect and never retargets by name;
- already released fresh buffer and repeated rollback are harmless;
- exact fresh rollback ordinarily closes that buffer only—never the Session,
  Runs, tabs, or durable state, and never bypasses kill queries;
- a kill query or kill hook `error`/`quit` leaving BUFFER live publishes
  `retained`; that exact survivor stays enumerable and retryable through
  `superchat-session-workspace-close`;
- rollback failure is secondary and cannot replace the producer's original
  submission signal.

The old API retains all existing lazy/reuse/reentrancy/publication-rollback
behavior and return shape.

## Corrected lifecycle boundaries

### Skills operation-child

Each asynchronous match owns one child Session and
`skills-match-protocol` host, opens no tab, and returns no Session ID.  Preserve
the callback result shape.  After terminal/callback processing, request public
whole-Session close exactly once for completed, cancelled, provider error,
callback `error`, callback `quit`, synchronous pre-accept rejection, and a
provider `error`/`quit` propagated after Runtime already accepted/delivered.

Publish the one-shot guard before calling close.  An unwind boundary must keep
callback nonlocal exits from skipping close.  Post-accept propagation shares
the same attempt; it is not a second rejection cleanup.  Close refusal/failure
does not mask the primary condition or trigger direct kill.  Diagnose the
exact child Session and public retry action; retained survivors remain
enumerable/retryable without repeating match, delivery, callback, or first
close attempt.  Repeated matches leave no hidden accumulation.

### Subagent trackable child

Acquire the child Session's `subagent-protocol` host through the new API.
Team, parent Run, Artifact Workspace/root, buffer name, and delivery presence
never determine editor ownership.

Acceptance is proved by either a normally returned handle with non-empty Run
and Turn IDs or entry into the producer's terminal delivery wrapper.  Set the
accepted guard on wrapper entry before `superchat--subagent-finish` or any
untrusted callback.  Thus accepted success/cancel/provider error/callback
`error`/callback `quit`/post-publication synchronous signal all retain the host
for ordinary Session-lifetime close.

Only synchronous exit with neither valid handle nor delivery entry is genuine
pre-accept rejection.  Call rollback with the original SESSION, ROLE, BUFFER,
and freshness: fresh true rolls back; reuse false remains.  Refusal leaves a
retained enumerable retryable survivor; rollback failure cannot mask the
original rejection; repeated rejection cannot accumulate hidden fresh hosts.

### Blocking adapters

Org Babel, Magit, and Rewrite stay legal blocking adapters with unchanged
synchronous return/trimming/error/timeout/model/system/tool-free/insertion/
ediff contracts.  For the exact invocation/source buffer:

- ordinary unowned: mint one non-empty adapter-call Session without mutation;
- exactly owned: reuse its public owner Session and validate the exact object;
- owner/Session mismatch: public validation rejects before Runtime effects;
- malformed exact-local metadata: public authority rejects without repair;
- inherited/default metadata: grants no ownership and follows borrowed rules;
- every branch preserves marker locality and values;
- Rewrite submits the source; later preview buffers remain lexical resources.

Production callers use only public owner/validation APIs and never private
marker symbols.

## Scope and exclusive worker ownership

One fresh correction worker owns only:

Production:

- `superchat-editor-workspace.el`
- `superchat-skills.el`
- `superchat-subagent.el`
- `ob-superchat.el`
- `superchat-magit.el`
- `superchat-rewrite.el`

Tests:

- `test/test-editor-workspace.el`
- `test/test-skills.el`
- `test/test-skills-integration.el` only if real public async coverage cannot
  live in `test/test-skills.el`
- `test/test-subagent.el`
- `test/test-ecosystem.el`
- `test/test-rewrite.el`

Runtime/LLM files and tests are verification inputs, not worker ownership.

## Red-before-green order

### 1. Acquisition/rollback authority

First add red authority tests: old buffer-only return; fresh exact+t; same-role
reuse exact+nil; false no-op; exact fresh rollback; dead/repeat idempotence;
wrong Session/role/object zero effects; query and hook `error`/`quit` retained
survivor; enumeration and later public retry.  Then minimally implement by
reusing the single private classifier/state machinery—no second ledger,
classifier, name lookup, or whole-Session rollback.

### 2. Skills accumulation/close matrix

Add red tests for success, cancel, provider error, callback `error`, callback
`quit`, synchronous pre-accept rejection, and post-accept propagated provider
`error`/`quit`.  Prove callback/terminal processing precedes exactly one close,
reentry/failure cannot duplicate, and primary conditions survive.  Cover
refusal → retained → public retry and repeated matches via public discovery,
not a hidden Session ID captured only from a mock.  Preserve model-free sync
keyword matching.  Then implement minimum close-once/unwind logic.

### 3. Subagent acceptance/fresh/reuse matrix

Add red tests for exact child acquisition; preserved Actor/Team/parent/
Artifact fields; fresh rollback on real pre-accept rejection; reuse
preservation; delivery-before-return acceptance before untrusted callback;
handle-only acceptance; accepted success/cancel/provider/callback error/quit
and propagated signal host retention; refusal retained/retry; repeated genuine
rejection without accumulation.  Then minimally add accepted guard/rollback
without changing Runtime or running/team projections.

### 4. Blocking adapter matrix

For each adapter add ordinary, exactly owned, owner-mismatch, malformed-local,
and inherited/default tests.  Assert exact submitted object, correct Session or
pre-submit rejection, marker locality/values unchanged, and original blocking
result/error behavior.  Rewrite also proves source != preview hosts.  Use
public APIs or controlled public-function stubs for mismatch; do not reproduce
the private classifier.  Only then make any minimal adapter correction.

## Acceptance

1. Both EditorWorkspace APIs have exactly the fixed shapes; old lookup is
   unchanged and freshness is atomic.
2. Rollback is exact, false-no-op, fresh-only, idempotent, retained/retryable,
   and has no whole-Session effect.
3. Skills closes exactly once on every listed terminal/callback/provider/
   cancel/synchronous rejection path.
4. Skills refusal is actionable and publicly discoverable/retryable; retries
   do not repeat work and repeated matches do not hide accumulated hosts.
5. Subagent acceptance is proven by valid handle or delivery-before-return;
   every accepted outcome retains the host until public Session close.
6. Only genuine pre-accept rejection invokes rollback: fresh rolls back, reuse
   survives, refusal retains, and original rejection remains primary.
7. Child editor scopes stay distinct despite shared Team/parent/Artifact/root;
   callback at-most-once and existing projections do not regress.
8. All adapters pass owned/mismatch/malformed/inherited/marker/preview cases
   while preserving synchronous contracts.
9. No producer private-marker read, name/default inference, global hook,
   validator registry, attestation token, Request callback field, second
   ledger/classifier, or durable Session registry is introduced.
10. Runtime, LLM, and their tests remain byte-untouched by this correction.

## Verification

Run focused suites and report exact total/expected/unexpected/skipped counts:

```sh
emacs -Q --batch -L . -l test/test-editor-workspace.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-skills.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-skills-integration.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-subagent.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-ecosystem.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-rewrite.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
```

Skills integration is required if changed/needed for real public-path proof;
otherwise inspect and record why focused Skills coverage suffices.  Canonical
is always mandatory.

Structural searches:

```sh
rg -n 'superchat-editor-workspace-(acquire-buffer|rollback-acquisition)' \
  superchat-editor-workspace.el superchat-skills.el superchat-subagent.el \
  test/test-editor-workspace.el test/test-skills.el test/test-subagent.el
rg -n ':ephemeral-buffer|:host-authority-mode' \
  superchat-skills.el superchat-subagent.el ob-superchat.el \
  superchat-magit.el superchat-rewrite.el
rg -n 'superchat--editor-workspace-(owned-p|session-id|role)' --glob '*.el'
rg -n 'get-buffer-create|generate-new-buffer|kill-buffer|\*superchat\*' \
  superchat-skills.el superchat-subagent.el ob-superchat.el \
  superchat-magit.el superchat-rewrite.el
rg -n 'superchat-runtime-submit(-blocking)?' \
  superchat-skills.el superchat-subagent.el ob-superchat.el \
  superchat-magit.el superchat-rewrite.el
```

Expect zero legacy ownership keys; private marker production access confined
to authority and authority tests; no name-based Runtime host; Rewrite preview
hits separately classified; async submit only Skills/Subagent and blocking
submit only the three adapters; no forbidden infrastructure.

Also require changed-file `check-parens`; scratch byte compile with `.elc`
outside the repo; focused `git diff --check` and owned-file diff review;
read-only proof Runtime/LLM and their tests were untouched; canonical kernel-
require/eager-integration/blocking-location/process-pump gates; and read-only
Kernel citation/authority-map checks.  Report drift only—never edit Kernel.

Mark `done` only after all acceptance/gates pass.  Record red evidence, final
exact counts, search classification, warnings, focused diff, and risk in this
Slice; keep `evidence_ref` empty for independent check/learn.

## Forbidden changes and hard stop

Do not modify Runtime, LLM, their lifecycle tests, Dispatcher, Agent Loop,
Workflow, Compact, Extension, Telega, plugin guidance, metadata, canonical
runner, unrelated code/tests, SPEC, KERNEL, EVIDENCE, CONTEXT, STATUS,
Protocols, ADRs, Runbooks, Lessons, doctrine, or other Slices.  Do not rewrite
Revision 4 code wholesale; change product semantics; add producer direct kill,
whole-Session Subagent rollback, accepted-Subagent auto-close, marker reads, or
Git/JJ writes.

If the fixed APIs cannot be implemented inside EditorWorkspace/test ownership,
or accepted cannot be reliably established from handle/delivery without a
Runtime change, set `blocked` and record the smallest failing ERT/reproduction
and missing authority.  Do not work around it with Runtime hooks, validator
callbacks, Request fields, registries, or widened scope.

## Worker handoff

Assign one fresh `do` worker.  It must reread repository/doctrine guidance,
SPEC Revision 5, K16, CONTEXT/STATUS, this entire Slice/history, and current
owned files plus focused diffs.  Treat Revision 4 implementation as shared
baseline.  Execute the four red-before-green phases in order and proactively
report completion/blocker to `superchat_lead` with files, red/green evidence,
exact totals, searches, risks, and Git/JJ writes (`none`).

## Evidence

## Revision 5 verification summary (do)

- Required-finding correction kept Phase 1 green at 34/34; the acquisition and
  rollback authority implementation did not need another production change.
  Its original red was 31/34 because both Revision 5 public APIs were absent.
- Phase 2 follow-up red reproduced the independent reviewer result
  `REENTRANT_CALLBACKS=2 CLOSES=1` (the focused recursive probe reached
  excessive nesting before correction).  A single guard now claims both
  delivery and close before the untrusted callback; after is callbacks=1,
  closes=1.  A second red proved diagnostic failure replaced the primary
  rejection; green isolates close/diagnostic conditions as secondary.  Skills
  is 23/23 and Skills integration is 11/11.  Real Runtime coverage includes
  success, cancel, provider error/quit, callback error/quit, while the only
  constructible genuine pre-accept case replaces the public submission seam;
  five consecutive rejections leave public live-host growth at zero.  Close
  refusal retains an enumerable host and public retry closes it.
- Phase 3 follow-up first produced a useful red: a Tape append failure becomes
  Runtime `audit-incomplete` delivery and is therefore accepted, not a valid
  pre-accept fixture.  The corrected matrix uses real acquisition/validation/
  rollback with only the public submission rejection seam for genuine
  pre-accept fresh/reuse cases.  All accepted cases use real Runtime with only
  transport/Tape boundaries stubbed: handle-only, delivery-before-return,
  success, cancel, provider error/quit, callback error/quit, and propagated
  synchronous conditions retain the exact host.  Rollback refusal is retained
  and public retry succeeds.  Subagent is 47 total, 45 expected, 0 unexpected,
  2 skipped; callback-at-most-once and running/team regressions remain green.
- Phase 4 test red exposed that dynamically binding special marker variables
  for every case shadowed real owned locals.  Green matrices use real buffers,
  public owner/validate authority, and real local marker classification;
  Ecosystem is 16/16 and Rewrite is 23/23.  Ordinary, inherited/default,
  exact-owned, malformed, unchanged marker locality, and Rewrite source versus
  preview identity are covered.  Adapter owner mismatch is unreachable at the
  real entry because the candidate Session is the exact queried owner (or a
  mint on an ordinary buffer); the reachable malformed pre-submit rejection
  and the public exact mismatch authority test cover that seam without a
  private classifier stub.
- Canonical ERT: 1240 total, 1223 expected, 0 unexpected, 17 skipped;
  `RUNNER_GATE_OK`.  Its architecture gates cover kernel requires, eager
  integrations, blocking-call locations, and process-pump owners.
- Changed-file `check-parens`, scratch byte compilation to `/tmp`, and focused
  `git diff --check` passed.  Compilation reported the pre-existing Skills
  unused lexical binding warning and Rewrite optional
  `region-extract-function` declaration warning.  No repository `.elc` was
  created; the six existing experiment `.elc` files remain untouched.
- Structural searches find the two new APIs only in EditorWorkspace,
  Skills/Subagent, and their tests; zero legacy ownership keys in the five
  producers; private marker access remains in the authority and pre-existing
  authority-focused tests; async Runtime submission remains Skills/Subagent,
  blocking submission remains Org/Magit/Rewrite, and Rewrite-only buffer
  creation is its lexical preview pair.
- Read-only strict Kernel check: 1035 references, 44 citation errors, 0
  warnings, caused by current dirty-tree line drift; authority-map form is
  `ok`.  Kernel was not edited.  Runtime/LLM and their tests already contain
  shared uncommitted changes and were not edited by this correction worker.
- Focused risk: genuine pre-accept rejection is not naturally constructible by
  either valid producer request; those two cases replace only the public
  Runtime submission seam, while every accepted path crosses the real Runtime
  lifecycle.  Git/JJ write operations: none.  `evidence_ref` remains empty for
  independent `check`/`learn`; this result is ready for independent recheck.

No Revision 5 implementation evidence.  `evidence_ref` is intentionally empty.

## Superseded Revision 4 do/check history

The prior `do` migrated the five producer/adapters to exact buffers.  It
reported: Subagent 44 total / 42 expected / 0 unexpected / 2 skipped; Skills
19/19; Skills integration 11/11; Ecosystem 15/15; Rewrite 22/22; canonical
1228 total / 1211 expected / 0 unexpected / 17 skipped with
`RUNNER_GATE_OK`.  Structural/check-parens/scratch-compile/diff gates passed;
Kernel strict was 1035 references / 29 citation errors / 0 warnings and the
authority map was `ok`.

Independent adversarial `check` rejected that `done` claim.  Skills terminal/
rejection and Subagent pre-accept paths could leave undiscoverable child hosts;
tests retained mocked Request Session IDs unavailable to production.  Adapter
tests also lacked the full owned/mismatch/malformed/inherited/marker/preview
matrix.  Those green numbers are only the retained Revision 4 baseline.

Revision 5 resolves the semantic gap with Skills operation-child close-once
and Subagent exact fresh-only rollback.  It supersedes every older instruction
that kept Skills hosts for manual close, excluded EditorWorkspace from worker
ownership, or treated every synchronous Subagent signal as pre-acceptance.

## Revision 5 learn summary

- Independent `slice07_final_recheck` closed every prior Required finding and
  returned PASS with no findings.  Canonical ERT was 1240 total, 1223
  expected, 0 unexpected, 17 skipped, with `RUNNER_GATE_OK`.
- Learn assigned confirmed Evidence `E-20260904-005` after checking ledger
  uniqueness, then populated this Slice's `evidence_ref`.
- K17 promotes the additive plain acquisition value, exact fresh-only
  rollback, Skills operation-child close-once, Subagent accepted-host
  retention/pre-accept rollback choice, and exact blocking-adapter contracts.
- This learn pass independently reran all six focused groups: EditorWorkspace
  34/34; Skills 23/23; Skills integration 11/11; Subagent 47 total, 45
  expected, 0 unexpected, 2 skipped; Ecosystem 16/16; Rewrite 23/23.
- Final Kernel strict check reports 1062 references, 0 errors, 0 warnings;
  authority map is `ok`.  Pairs reports 84 entries, 533 shared groups, 213
  universal sentences, and 138 narrowed in 36 entries.
- No production, test, SPEC, Protocol/ADR/Runbook/Lesson, or other Slice file
  was changed by learn.  Git/JJ write operations: none.
- Next permitted action is `arrange` for Slice08; Slice08's `blocked_by: 05`
  dependency is satisfied by `E-20260904-003`.
