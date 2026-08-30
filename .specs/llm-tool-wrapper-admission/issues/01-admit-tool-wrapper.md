# 01 Reject unwrapped ordinary tools before provider effects

status: done
blocked_by:
writer: do
authority: `superchat-llm.el` | source-backed — Kernel authority map assigns model transport to LLM; this Slice narrows its pre-provider Request admission and does not move tool policy into LLM.
spec_ref: `.specs/llm-tool-wrapper-admission/SPEC.md` — Action Contract “Admit a tool-bearing LLM Request”
context_ref: `KERNEL.md` K24 — Tool provenance/wrapper contract, Model transport authority, and Kernel layering
evidence_ref: E-20260905-011

## Goal

A non-empty ordinary, non-prepared tool list reaches the provider only after
LLM has obtained a callable trusted wrapper.  Absence of that wrapper produces
one stable pre-provider rejection while all existing admitted paths retain
their behavior.

## Scope

- `superchat-llm.el` — add the minimum admission check after effective tools,
  prepared status, and wrapper selection are known and before any provider
  effect.  Do not require Agent Loop or move policy/execution into LLM.
- `test/test-llm-backend.el` — tests-first behavior, layering, and real Runtime
  terminal coverage through existing fixtures.
- `test/test-runtime.el` — may be edited only if the real Runtime rejection
  terminal cannot be asserted through `test/test-llm-backend.el`; it is test
  scope only.

Production Runtime, Authority, Agent Loop, Subagent, provider adapters, runner,
and all knowledge files are read-only.  `prepared-tools` remains a trusted
internal producer contract, not authentication of arbitrary or malicious
Lisp.  External callers cannot migrate by labelling raw tools prepared.

## Acceptance

1. A non-empty, non-prepared list with neither a request-local callable wrapper
   nor an available callable Agent Loop wrapper fails before provider request
   construction/invocation; provider and tool effect counts are zero.
2. The same refusal is proven in a fresh/headless LLM load where Agent Loop is
   absent, and when its symbol is present but wrapper production yields a
   non-callable value.  No new top-level require is introduced.
3. A trusted request-local callable wrapper is selected, applied exactly once,
   and the invoked tool still reaches the real Authority seam.
4. The ordinary distribution Agent Loop wrapper remains admitted and crosses
   Authority; permission, approval, policy, tool result, and Tape behavior are
   unchanged.
5. Correctly prepared Subagent tools keep exact object/function identity and
   are not wrapped again.  Tests do not claim this proves arbitrary external
   prepared Lisp trustworthy.
6. Empty/no-tools sync and async Requests retain provider, delivery, and
   terminal behavior.
7. Exact-evidence success and its existing missing/malformed-wrapper failure
   remain stricter and unchanged; the new ordinary error does not replace the
   evidence error contract.
8. A real Runtime submission of the rejected ordinary Request reaches exactly
   one failed terminal, delivery and quiescence complete, and no provider or
   tool effect occurs.  Prefer proving this inside the LLM test file; use
   `test/test-runtime.el` only if its existing fixture is necessary.
9. Unknown Request keys and all non-tool transport controls remain compatible;
   no registry, DI seam, wrapper classifier, schema, or second Authority is
   added.
10. Repository search shows the implementation change is confined to LLM
    admission and the owned tests.  Runtime/Authority/AgentLoop/Subagent
    production files are unchanged.

## Verification

Red-before-green order:

1. Add focused cases for absent/non-callable wrapper and prove provider count
   remains zero; add the real Runtime terminal case before production repair.
2. Run those cases against the old implementation and preserve the meaningful
   failures.
3. Add the smallest LLM admission check.
4. Add/retain positive matrices for request-local, ordinary distribution,
   prepared Subagent, no-tools, and exact evidence.

Then run focused new tests, complete `test/test-llm-backend.el`, the applicable
Runtime tests, and the canonical runner:

```sh
emacs -Q --batch -L . -l test/test-llm-backend.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-runtime.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch -L test -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Also run check-parens on every changed Elisp file, inspect the focused diff,
and search for new Kernel→middle-tier requires.  Use local fake providers/tools
only; do not contact a real provider.

Hard-stop and return to `plan` if satisfying acceptance requires changing
Runtime, Authority, Agent Loop, Subagent, a public Request/schema contract, the
prepared-tool identity model, or adding a new public Interface/registry.

## Evidence

For `check`, retain the initial admission failure, fresh focused/full/canonical
counts, provider/tool zero-effect assertions, real Runtime terminal result,
layering/residue searches, and syntax/strict/authority results.  Leave
`evidence_ref` empty until `learn` records independently accepted evidence.

## Do verification summary — 2026-09-05

- Red: the focused three-test selection was `0/3` before the admission check.
  Both absent and non-callable wrapper cases reached the fake provider, and the
  real Runtime submission was accepted.  The request-local positive case also
  exposed and corrected a test-fixture arity mistake before production repair.
- Green: focused `3 total / 3 expected / 0 unexpected / 0 skipped`; complete
  LLM `185 / 180 / 0 / 5`; Runtime `111 / 111 / 0 / 0`; canonical
  `1262 / 1244 / 0 / 18`, `RUNNER_GATE_OK`.
- The rejection is the stable
  `superchat-llm-tool-wrapper-required` condition and occurs before provider
  construction/invocation.  The negative cases assert zero provider calls and
  zero tool effects.  The Runtime case asserts one failed terminal, one failed
  delivery, quiescence, and one durable `run.failed` event.
- Existing full-suite cases retain exact-evidence precedence, prepared-tool
  identity/no double wrapping, ordinary Agent Loop wrapping, and no-tools
  sync/async behavior.  The new positive case additionally invokes an ordinary
  request-local wrapper through real Authority.
- `check-parens` and focused whitespace checks pass.  Kernel strict is
  `1020 refs / 29 errors / 0 warnings`; the errors are mechanical citation
  drift after the owned LLM insertions and remain for check/learn.  The
  authority-map form check passes.  No Kernel require, registry, schema,
  Runtime production, Authority, Agent Loop, or Subagent change was made.
- Not established: callable Lisp is not authenticated against malicious
  producers, and `prepared-tools` remains the trusted internal contract.
  `evidence_ref` remains empty pending independent check and learn.

## Required-fix verification — 2026-09-05

- AC1–2: a real child `emacs -Q` loads only the LLM Kernel path, proves both
  the Agent Loop feature and wrapper producer are absent, observes the stable
  admission condition, and counts prompt construction, provider-request
  construction, and provider invocation as zero.
- AC5: a captured prepared provider prompt retains `eq` identity for both the
  original tool object and its function; request-local and Agent Loop wrapper
  application counts remain zero while the fake provider returns normally.
- AC8: the real Runtime/Tape fixture now leaves the default Closure terminal
  guard enabled.  Rejection still preserves the original condition, one
  durable failed terminal and delivery, quiescence, and zero provider/tool
  effects; it does not degrade to `audit-incomplete`.
- Required focused `5 / 5`; complete LLM `187 / 182 / 0 / 5`; Runtime
  `111 / 111 / 0 / 0`; canonical `1264 / 1246 / 0 / 18`,
  `RUNNER_GATE_OK`.  `check-parens`, focused whitespace, and authority form
  pass.  Strict is `1020 refs / 37 errors / 0 warnings`: the prior 29 LLM
  citation shifts remain, plus 8 mechanical test citation shifts from these
  additions; all remain for learn.
- The original positive-case arity failure was a fixture defect, not a
  production red.  No production file changed in this required-fix pass.

Scope correction: the initially reported strict result of
`1020 refs / 37 errors / 0 warnings` included eight apparent test shifts caused
solely by an accidental lexical-binding header inserted into the unrelated
late-LLM-load child fixture.  Removing that one line restores strict to
`1020 / 29 / 0`; the remaining 29 are the pre-existing LLM production citation
shifts for learn.  The independently accepted Required matrix and its test
counts are otherwise unchanged.
