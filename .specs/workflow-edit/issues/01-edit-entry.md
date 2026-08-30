# 01 Safely edit an existing or new Workflow recipe

status: done
blocked_by:
authority: `superchat-workflow.el` | owned — Workflow path resolution and the new edit entry; `superchat.el` only declares its lazy autoload
spec_ref: `.specs/workflow-edit/SPEC.md` revision 1, Action Contract “Edit a Workflow recipe”
context_ref: `CONTEXT.md`; `KERNEL.md` K19 Workflow identity, integration layering, and Architecture boundaries
evidence_ref: E-20260905-003

## Goal

Deliver one user-visible tracer path: completion selects or accepts a name,
existing safe recipes open unchanged, new safe NFC names open as unsaved
file-visiting buffers with one minimal valid template, and an ordinary save
makes the recipe discoverable and loadable.

## Scope

- `superchat-workflow.el` — implement `superchat-workflow-edit` by composing
  the existing completion, resolver, prospective-path, and loader ownership;
  add only the minimum new-target logic required by the confirmed contract.
- `superchat.el` — add the explicit lazy autoload that makes
  `M-x superchat-workflow-edit` reachable after loading the main package.
- `test/test-workflow.el` — add real temporary filesystem/buffer tests and
  retain the existing resolver, execution, Session, and role-host guarantees.
- `README.md` — add a concise command and `C-x C-s` usage sentence.
- `README_cn.md` — add the equivalent concise Chinese sentence.

Read-only: `test/test-runtime.el`, `test/run-tests.el`, `KERNEL.md`, and
`docs/spec-agents/check-kernel.sh`.  No other file is authorized; do not modify
Runtime, Authority, EditorWorkspace, LLM/transport, Tape, runner, or knowledge.

## Acceptance

### Tests first: red conditions

Add focused tests before production implementation.  They must fail because
the public command/autoload and editing behavior are absent, not because a
fixture stubs out the existing resolver.

1. In a fresh isolated Emacs that requires only `superchat`, the command is
   autoloaded and interactive without eagerly loading the Workflow integration.
   Invoking it is the call-time load boundary; the existing eager-distribution
   rule remains unchanged.
2. Completion contains real existing recipe basenames but accepts a new name.
3. An existing file opens its real canonical file-visiting buffer with exact
   content unchanged.  An NFC lookup of an existing NFD filename opens that
   actual file and does not rename or rewrite it.
4. A new NFD spelling resolves to an NFC basename target in the single
   configured directory.  The directory may be lazily created, the buffer has
   one format comment and one parseable non-comment placeholder step, and the
   target file does not exist before save.
5. After an ordinary `save-buffer`, the same recipe is returned by the real
   list/resolver and its exact saved content is returned by the real loader.
6. Editing the unsaved draft and invoking the command again returns the same
   visiting buffer with the draft byte-for-byte unchanged and no duplicate
   template.
7. Empty names, `../`, nested paths, unsafe existing paths/symlinks, and real
   NFC ambiguity fail before any target or foreign file buffer is opened.
   Existing resolver safety tests remain behavioral and continue exercising
   real files.
8. Opening or preparing a recipe causes zero Workflow execution, Runtime
   submission, and provider effect.  No production test may write the user's
   configured Workflow directory.

Use temporary directories and real Emacs file buffers.  Minibuffer input may
be intercepted, but path, persistence, and `find-file` behavior may not be
mocked.

### Green contract

- Implement one `;;;###autoload` public interactive command in
  `superchat-workflow.el` and one explicit autoload declaration in
  `superchat.el`; add no top-level plugin require.
- Reuse existing path classifiers.  First resolve an existing candidate; only
  on missing does the command NFC-normalize the new basename and request its
  prospective path from `superchat-workflow--file-path`.
- Reject before opening when neither branch yields a safe target.  If meeting
  this acceptance requires changing the pre-existing observable contract of
  `--resolve` or `--file-path`, stop and return to `plan` rather than expanding
  the implementation.
- Insert the template only for a genuinely new target with no pre-existing
  visiting buffer.  Never overwrite existing content or an unsaved draft.
- Keep all current recipe discovery, parsing, execution, Authority policy,
  stable Session, dead-View role-host reuse, and integration layering behavior.
- Add only symmetric, focused README usage text; do not rewrite the Workflow
  guide.

## Verification

Run in red-before-green order and report each layer separately:

```sh
emacs -Q --batch -L . -l test/test-workflow.el \
  --eval "(ert-run-tests-batch-and-exit 'WORKFLOW-EDIT-SELECTOR)"
emacs -Q --batch -L . -l test/test-workflow.el -f ert-run-tests-batch-and-exit
emacs -Q --batch -L . -l test/test-runtime.el \
  --eval "(ert-run-tests-batch-and-exit 'runtime/eager-distribution-does-not-require-integrations)"
emacs -Q --batch -L . -l test/run-tests.el
emacs -Q --batch --eval \
  "(dolist (file '(\"superchat-workflow.el\" \"superchat.el\" \"test/test-workflow.el\")) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens)))"
emacs -Q --batch -L test -l test/kernel-check.el -- check --strict KERNEL.md
bash docs/spec-agents/check-kernel.sh .
```

Also inspect the focused diff, confirm README/README_cn symmetry, search for
eager `(require 'superchat-workflow)`, and verify that no test or implementation
wrote outside its temporary directory.

Hard-stop and return to `plan` if implementation requires a second Workflow
directory/registry, a custom save layer, automatic execution/provider work,
storage migration, changes to Runtime/Authority/EditorWorkspace, relaxation of
the integration gate, or a behavioral change to the existing resolver or
prospective-path helper.

## Evidence

For `check`, preserve the red failure showing the absent command/behavior and
the green focused results.  Report fresh full Workflow, layering, canonical,
syntax, citation, and authority results separately.  Leave `evidence_ref`
empty until verified `learn` records the outcome.

## Do verification summary — 2026-09-05

Tests-first red was 0/4 because the command and main-package autoload were
absent.  Green focused is 5 total, 4 expected, 0 unexpected, 1 filesystem-
capability skip; full Workflow is 55/53/0/2, the eager-integration gate is
1/1, and canonical is 1228 total / 1210 expected / 0 unexpected / 18 skipped
with `RUNNER_GATE_OK`.  `check-parens` passed for all three Elisp files and the
authority form is valid.  Kernel strict moved from the K19 baseline 979/0/0 to
979 refs / 54 errors / 0 warnings because owned source/test insertions shifted
citations; the new edit Action Contract and mechanical re-anchors remain for
independent check then learn.  `evidence_ref` remains empty.
