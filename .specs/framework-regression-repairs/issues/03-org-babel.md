# 03 Org Babel injects a real Skill body for symbol and string parameters

status: done
phase: independent check PASS and learn complete; next arrange04 not executed
blocked_by: []
predecessor: .specs/framework-regression-repairs/issues/02-test-hygiene.md — done after independent check and learn, E-20260906-003
writer: do (listed adapter and direct tests only, after predecessor check and learn)
authority: ob-superchat.el — Babel parameter normalization and adapter query construction; existing Preset parsing/Skill loading authority remains read-only in superchat-preset.el and superchat-skills.el
spec_ref: .specs/framework-regression-repairs/SPEC.md
context_ref: KERNEL.md (K28, Preset and blocking adapter contracts); CONTEXT.md; .specs/framework-regression-repairs/SPEC.md (AC3); docs/protocols/session-editor-workspace-close.md
evidence_ref: E-20260906-004

## Goal

Babel :skill uses existing Preset content for either accepted parameter type,
while a missing skill silently leaves the expanded source body unchanged.

## Scope

- ob-superchat.el: ob-superchat--build-query and directly necessary existing
  accessor/loader declarations only.
- test/test-ecosystem.el: direct Babel regressions with real temporary skill files.
- Preset/Skills, Runtime, EditorWorkspace, test helper and runner stay read-only.

## Acceptance

1. Both symbol and string skill names resolve through the real Skill loader to a
   real superchat-preset; superchat-preset-skill-body precedes the original expanded
   body with the existing separator. Exact expected content is asserted.
2. Missing skill and absent skill return body unchanged, without new warning/error.
3. Actual org-babel-execute:superchat passes the composed query onward, preserving
   existing model/system options, body expansion and returned result handling.
4. Exact borrowed invocation-buffer identity and malformed/foreign marker behavior
   remain governed by the existing public EditorWorkspace/Runtime adapter contract.

## Verification

### Check and learn — 2026-09-06

Independent Claude c03 PASS accepted; E-20260906-004 was appended and verified
unique before this backlink. Reused independent focused11/11 and ecosystem22/22
in both ordinary and isolated llm0.31.1 environments, EW5/5, Runtime3/3 and
canonical1347/1313/0/34 exit0. The checker reports31 probe checks with zero
failures in each environment; the preserved probe log contains31 OK rows.
These adapter tests capture the blocking Runtime boundary, not real Run/Tape or
provider execution. Optional/NFC and socket limits remain. Source red is historical.
S2 empty Skill body retains approved literal separator concatenation, no change or
followup obligation. S1/O1 remain separate unresolved observations from02.
K29 only relocates the unchanged call-llm citation81→84 and updates provenance;
fresh strict1072/errors0/warnings0 and authority form pass. No enacted semantics
or since fields changed. Full learn report: /private/tmp/framework03-learn/WORKER_REPORT.md.
No04 arrangement or implementation occurred in this learn.

### Do handoff — 2026-09-06

Production delta is only build-query's symbol/string normalization and real
superchat-preset-skill-body access, plus two declarations. Existing executor and
call-llm forms are reader-equal. Six direct Babel tests plus one temporary Skill
fixture were appended to ecosystem; every existing test form is unchanged.

Valid tests-first red: focused11/7/4/0 exit1, all four failures at exact composed
content (symbol, string, actual executor, actual Org source block). An earlier
test-source parse error is retained separately and is not acceptance red.
Green fresh: focused ordinary and llm0.31.1 each11/11; full ecosystem each22/22;
public EW validation5/5; Runtime layering/blocking3/3; canonical1347/1313/0/34,
exit0 plus separate RUNNER_GATE_OK. No focused/direct skips. Canonical optional
dependency/NFC skips are retained; the socket fixture is not exercised.

Real files/loader/Preset type, basename versus display alias, plain/frontmatter
content, symbol/string, nil/missing silence and unavailable-loader fallback pass.
Actual Org prologue/epilogue expansion,18 model/system/result combinations,
missing-backend refusal and real execute-src-block RESULTS insertion pass through
only blocking Runtime capture; no real Run/Tape lifecycle is claimed for that
capture. Existing borrowed identity matrix and public mismatch rejection pass.
A deliberate assertion captures and verifies release of two actual files, their
directory and the exact Org buffer; no shared cleanup helper was changed.

Parens, unchanged out-of-scope forms, exact three-file scope and whitespace pass;
authority form valid. Read-only K28 strict1072 references/errors1/warnings0: sole
old ob-superchat.el:81@ob-superchat--call-llm citation now belongs at84, with its
target form unchanged. Kernel stays read-only until learn. No knowledge/SPEC,
runner/helper, other Slice or architecture-map edits. S1/O1 remain outside03.
Evidence stays empty pending independent check and learn. Commands, exact results
and limits: /private/tmp/framework03-do/WORKER_REPORT.md.

### Arrangement — 2026-09-06

The already approved five serial Slices remain01→02→03→04→05.02 is done and its
unique E-20260906-003 backlink resolves to the accepted independent check/learn;
03 has no remaining predecessor blocker and is ready for a separate do handoff.
04/05 files and dependencies are unchanged. evidence_ref stays empty until03 learn.

Writable product scope is exactly ob-superchat.el's ob-superchat--build-query and
necessary accessor/loader declarations. Test scope is test/test-ecosystem.el's
direct Babel cases; no edits to other integration assertions. Do may update this
Slice's status/short verification summary. Preset/Skills parsing and lookup,
Runtime/EditorWorkspace identity, ob-superchat--call-llm, org-babel-execute:superchat,
runner/helper and knowledge remain read-only. The public executor and blocking
handoff are exercised, not rewritten. No new skill resolver, identity policy,
registry, result semantics or broad exception policy. S1/O1 from02 remain separate
Evidence observations; neither is assigned to03. Architecture-map documentation is
navigation only and its K27 source snapshot is historical, not current authority.

Source facts: superchat-skills--find-file resolves basename plus configured
extension; superchat-skills-load reads the actual file and calls
superchat-preset-from-plist, with frontmatter name serving as display name.
The skill-body slot holds the file body. Babel currently invokes symbol-name for
all inputs and reads plist :body from that struct. The fix must preserve existing
two-newline concatenation, body bytes and optional-loader fallback. Existing
loader error containment remains error-only; no new quit swallowing or unsupported
parameter-type contract. No producer needs a change outside the candidate scope.

Tests-first sequence, all new direct cases in test/test-ecosystem.el:

| Gate | Setup and observation | Required result |
| --- | --- | --- |
| Real Skill red | Temporary skills directory, real .md file, actual superchat-skills-load; assert superchat-preset-p and exact skill-body first | Existing builder fails to deliver the expected composed query for symbol and string names; retain actual red condition/content |
| Filename versus display name | File basename differs from frontmatter name; exercise basename as symbol/string and separately the absent display-name filename | Both basename inputs load the same real body; display name does not become an alias and missing lookup returns expanded body silently |
| Content and fallback | Frontmatter and plain-body files, multiline/Unicode body with intentional trailing newline; nil skill, missing symbol, missing string; preserve existing loader-unavailable fallback | Exact skill-body + two newlines + expanded body; no trimming/substitution of Skill content; absent/missing returns exact body with no new message/warning/error |
| Actual Babel entry | Real Org expansion and org-babel-execute:superchat through unchanged call-llm; capture only blocking Runtime request/result boundary as existing ecosystem test does | Nontrivial expanded body is used, not raw body; exact composed query reaches input/prompt; model, explicit/default system, timeout, tools none, record-conversation nil and origin remain unchanged |
| Result path | Same actual Babel entry with whitespace result, nil result and model-boundary error; backend absent negative case | Existing trim, no-response placeholder, formatted error and missing-backend rejection unchanged; no new result policy |
| Real Org block integration | Temporary Org source block executed through org-babel-execute-src-block with local confirmation disabled; actual expansion/result insertion, model boundary captured | Expected composed query and returned text appear through actual #+RESULTS path; no provider call or GUI |
| Borrowed identity regression | Existing ecosystem/blocking-adapters-use-public-identity-matrix, plus exact captured invocation object in new entry cases | Ordinary and inherited remain unowned borrowed inputs; exact owner Session retained; malformed local markers reject before submission; marker locality/values unchanged |

Do must inspect installed Org's expansion seam when constructing the nontrivial
fixture (for example supported prologue/epilogue or source-block expansion); assert
that expansion actually changed the raw input and compare exact expected bytes.
Do not stub the Skill loader, Preset accessor, Org expander or public EditorWorkspace
classification to manufacture acceptance. Capture the blocking Runtime boundary
only for the adapter request/result contract; do not describe that capture as a
real Run/Tape lifecycle test. Existing Runtime/EW suites provide the unchanged
authority regression. If expanded-body/result acceptance actually needs a new
production behavior outside build-query, retain the failing test and return plan.

For identity, K28 and the existing matrix expressly state that adapter owner
mismatch is structurally unreachable: Session comes from the real owner query or
is minted for an ordinary buffer. Do not invent a foreign-owner adapter path by
faking the query. Run the existing read-only public
editor-workspace/validate-buffer-rejects-owner-session-mismatch negative test, which
proves the actual validation contract; retain ordinary borrowed-object acceptance.

Use test-helper before product loading, temporary Org buffers/files and bound
superchat-skills-directory, and unwind cleanup of actual objects and paths.
Require the real Skills/Org features during setup before capturing diagnostics;
fixture setup messages are not evidence that missing Skill lookup warns. No default
skill installation into user state, provider, network, socket, live GUI, user DB,
HOME/CODEX_HOME changes or dependency installation. Existing isolated llm0.31.1
dependencies are available; their presence is not a new provider test claim.

Planned commands from repository root, not run by arrange:

```sh
emacs -Q --batch -L . -l test/test-ecosystem.el --eval '(ert-run-tests-batch-and-exit "^ecosystem/ob-")'
emacs -Q --batch -L . -l test/test-ecosystem.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-editor-workspace.el --eval '(ert-run-tests-batch-and-exit "^editor-workspace/validate-buffer-")'
emacs -Q --batch -L . -l test/test-runtime.el --eval '(ert-run-tests-batch-and-exit (quote (member runtime/kernel-requires-stay-inside-kernel runtime/eager-distribution-does-not-require-integrations runtime/blocking-submit-is-limited-to-sync-adapters))))'
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch -L . -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Repeat the full ecosystem file with the already installed isolated package prefix:
`emacs -Q --batch --eval '(progn (require (quote package)) (setq package-user-dir
"/private/tmp/claude-503/-Users-chenyibin-Documents-emacs-package-superchat/0052b418-a3b9-45fa-b0c5-a65a7cb6e008/scratchpad/elpa")
(package-initialize) (require (quote llm)))' -L .`, followed by the same ecosystem
load/ERT arguments. Check parens and changed-line whitespace for the two scoped
files. Record exact commands, exits, skips/exclusions and any mechanical Kernel
drift without editing Kernel in do. Canonical RUNNER_GATE_OK and ERT counts are
separate results. Socket/provider/GUI acceptance is not implied by any gate.

This arrangement performed source/dependency-path and predecessor checks only,
no fresh ERT, deliberate-failure or acceptance runs. Candidate scope is sufficient
for the confirmed contract; no new decision or scope conflict identified. Stop
before implementation and hand off ready03 to leader.

## Evidence

Source inspection: ob-superchat--build-query currently calls symbol-name on every
skill and plist-get :body on the loader result. No implementation or repair tests
yet. The initial blocked state was satisfied by02 check+learn E-20260906-003;
this arrange records readiness, not implementation evidence. Slice04 still waits
for03 do, independent check and learn.
