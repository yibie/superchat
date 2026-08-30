# 01 Account for tool-call quit without losing durable facts

status: done
blocked_by:
authority: `superchat-authority.el` owns tool audit and outcome ordering; `superchat-runtime.el` owns exact Run terminal/cancellation and recovery admission; `superchat-experience.el` owns recovery-learning classification
spec_ref: `.specs/tool-quit-accounting/SPEC.md` revision 1, both Action Contracts
context_ref: `KERNEL.md` K20 tool execution, Closure, Runtime terminal, and Architecture boundaries
evidence_ref: E-20260905-006

## Goal

Preserve an original user `quit` while durably accounting for the tool phase,
interrupting only its owning Run, and excluding that interruption from
automatic Runtime/Experience recovery.  Make ordinary and exact outcomes use
one filter/freeze/persist/notify/deliver order without changing ordinary error,
permission, approval, rewrite, or headless behavior.

## Scope

Production owners:

- `superchat-authority.el`: phase-labelled interruption audit, original-condition
  preservation, result freezing, and detached success/failure notifications.
- `superchat-runtime.el`: request-local exact-Run interruption callback,
  audit-incomplete fallback, and interruption recovery exclusion.
- `superchat-experience.el`: interruption exclusion from recovery-learning.

Direct tests:

- `test/test-authority.el`
- `test/test-runtime.el`
- `test/test-agent-loop.el`
- `test/test-experience.el`
- `test/test-closure.el`

`superchat-agent-loop.el` and `superchat-closure.el` are read-only.  Needing to
change either production module, add a public Interface, event/schema, recovery
category, registry/DI mechanism, or storage migration is a hard stop back to
plan.  The runner and all other production and knowledge files are out of scope.

## Acceptance

1. A pre-hook `quit` appends a `tool.failed` with interruption class and `pre`
   phase, never calls the implementation, interrupts the exact owning Run when
   supplied, and re-signals the original symbol/data after cleanup.
2. An implementation `quit` records phase `implementation` and explicitly does
   not claim effects were absent.  A result-filter `quit` after a normal return
   records `result-processing`; implementation receipt metadata is distinct
   from the final result fingerprint.
3. A post-success or post-failure notification `quit` after a durable outcome
   adds no contradictory second tool outcome.  It still interrupts the exact
   Run and re-signals the original condition.  Cleanup `error`/`quit` cannot
   replace it, and remaining hooks need not run.
4. Both ordinary and exact success filter first, freeze a detached result,
   append one `tool.completed` with its fingerprint, notify post hooks using a
   separate detached copy, then deliver the same frozen value.  Hook mutation
   and return values cannot alter Tape or delivery.
5. Failure notifications run only after durable `tool.failed`, receive detached
   data, and cannot change the outcome.  A later notification quit never
   reclassifies a genuine implementation error as user interruption.
6. Interruption-class failures do not trigger Runtime recovery intervention or
   Experience failed/recovery learning.  Existing typed tool failures still do.
   Closure replay accounts the interrupted executing Effect as failed without
   adding or changing Closure production logic.
7. Runtime's private callback closes over one Run ID and delegates to
   `superchat-runtime-cancel`.  Sync reentry, delayed async invocation after
   submit returns, late/repeated calls, and simultaneous Runs A/B prove that A
   cannot cancel B and terminal calls are harmless.
8. If interruption outcome audit fails, the existing audit-failure callback
   seals `audit-incomplete`; no fake `tool.completed` or `run.cancelled` is
   written.  The original `quit` remains primary.
9. Except for the SPEC-approved post/failure notification move and detached-copy
   semantics in Acceptance 4–5, ordinary hook/implementation `error`,
   permission, approval, rewrite, inactive-Run, exact-evidence validation, and
   Agent-Loop-absent headless behavior retain their existing results and event
   order.
10. Tests use temporary SQLite/Tape state and local fake tools.  A Lisp
    `signal 'quit` test is reported as such, not as interactive keyboard `C-g`.

## Verification

Tests-first order: capture current focused baseline; add the smallest real
Authority/temporary-Tape quit regression and observe red; implement the minimum
Authority/Runtime/Experience changes; then add phase, notification, sync/async,
reentry, two-Run, audit failure, recovery exclusion, and Closure replay matrix.

Run focused tests for each changed file, then the five complete files and
`emacs -Q --batch -L . -l test/run-tests.el`.  Run `check-parens` on changed
Elisp, inspect the focused diff and forbidden-symbol residue, and run the
read-only Kernel strict and authority-map gates.  Report totals as
total/expected/unexpected/skipped and classify approved semantic citation drift
separately from mechanical line movement.

## Evidence

Initial red: the two smallest Authority regressions both failed because the
request-local interrupt callback was not invoked and no phase-labelled failure
was present.  After the minimum implementation, focused additions passed:
Authority 7/7, Runtime 2/2, Experience 1/1, Closure replay 1/1.

Complete direct files: Authority 82/82; Runtime 105/105; Experience 29/29;
Closure 27/27; Agent Loop 27 total, 26 expected, 0 unexpected, 1 skipped.
Canonical: 1239 total, 1221 expected, 0 unexpected, 18 skipped,
`RUNNER_GATE_OK`.  Changed Elisp passes `check-parens`; the authority-map form
is valid.  K20 strict is intentionally left for `learn`: 992 refs, 127 errors,
0 warnings, consisting of approved semantic targets plus accumulated mechanical
line drift after the new Authority/Runtime code and tests; Kernel was not edited.

Tests signal the Lisp `quit` condition and do not claim an interactive keyboard
`C-g` was exercised.  Independent `check` must validate all Acceptance items
before `learn` assigns an Evidence ID; `evidence_ref` remains empty.

Required-fix red phase: independent check found that a legitimate nil
implementation return lost its receipt-presence fact, interruption events still
entered Experience prefix/skeleton/review consumers, and AC1–8 lacked direct
ordinary/exact sync/async and Runtime-bridge matrix evidence.  The three fixes
remain within the original owner scope and add no new product semantics.

Required-fix red was reproduced independently: nil receipt 0/1 and the real
temporary-SQLite Experience consumer matrix 0/1.  Green production changes
separate receipt presence from its value and route recovery view, run prefix,
review outcome selection, and run skeleton/monitor input through one private
Experience eligibility predicate.  Tape retention and ordinary failures remain
unchanged.

AC-to-test map:

- AC1: `authority/pre-hook-quit-is-audited-before-original-propagates`,
  `authority/exact-quit-matrix-preserves-one-durable-outcome`, and
  `runtime/authority-quit-cancels-exact-run-through-real-bridge`.
- AC2: `authority/implementation-quit-is-partial-and-preserves-condition`,
  `authority/filter-quit-records-receipt-not-final-fingerprint`, and
  `authority/filter-quit-retains-a-nil-implementation-receipt`.
- AC3: `authority/post-notification-quit-does-not-add-second-outcome`,
  `authority/delayed-async-post-quit-keeps-frozen-success`, and
  `authority/async-genuine-error-survives-failure-notification-quit`.
- AC4: `authority/ordinary-success-freezes-before-post-notification`,
  `authority/evidence-post-hook-cannot-mutate-returned-result`, and the exact
  and delayed-async tests above.
- AC5: the exact failure-notification row in
  `authority/exact-quit-matrix-preserves-one-durable-outcome` plus
  `authority/async-genuine-error-survives-failure-notification-quit`.
- AC6: `experience/user-interruption-is-not-a-recovery-sample`,
  `experience/user-interruption-is-absent-from-all-learning-views`,
  `runtime/user-interruption-does-not-open-recovery-intervention`, and
  `closure/user-interrupted-tool-is-still-an-accounted-failure`.
- AC7: `runtime/tool-interrupt-callback-cancels-only-its-captured-run` and
  `runtime/authority-quit-cancels-exact-run-through-real-bridge`.
- AC8: `authority/interruption-audit-failure-keeps-original-quit-primary` and
  `runtime/interruption-audit-gap-seals-incomplete-not-cancelled`.
- AC9: the five complete direct files, including existing permission, approval,
  rewrite, exact evidence, ordinary error, and headless Agent Loop tests.
- AC10: Runtime/Experience bridge tests use temporary SQLite/Tape state and all
  tools/transports are local fakes; tests use Lisp `signal 'quit`, not keyboard
  `C-g`.

Required-fix verification: Authority 87/87; Runtime 107/107; Experience 30/30;
Closure 27/27; Agent Loop 27 total, 26 expected, 0 unexpected, 1 skipped.
Canonical 1247 total, 1229 expected, 0 unexpected, 18 skipped,
`RUNNER_GATE_OK`.  Eight relevant Elisp files pass `check-parens`; authority-map
form is valid.  K20 strict remains read-only at 992 refs, 142 errors, 0 warnings;
the increase is mechanical citation movement from the required production and
test insertions, while the already approved semantic delta still awaits learn.
