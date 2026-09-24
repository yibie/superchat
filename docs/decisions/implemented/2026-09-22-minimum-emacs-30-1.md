# Decision: the minimum Emacs is 30.1

Status: implemented — the user's ruling of 2026-09-22 ("将最低版本改为 30.1"); verified on a locally built Emacs 30.1 on 2026-09-23.

## Problem

Superchat declared `(emacs "29.1")`. On 2026-09-22 it was run on a locally built Emacs 29.4 (`.scratch/g3-emacs29/README.md`): it loads and completes real tasks, but two product behaviours differ from newer Emacs. `copy-tree` does not copy records on 29, so recording request evidence rewrote the live prompt; and C-g could not cancel an approval question asked from a provider callback. The declared minimum was close to true but not true.

## Decision

The declared minimum is Emacs 30.1: `Package-Requires` in `superchat.el`, `superchat-pkg.el`, and both READMEs. Superchat is written and tested against 30.1 and later; no workaround for Emacs 29 is kept.

Verification on 30.1 (2026-09-23, `.scratch/g3-emacs30/README.md`) found that the C-g problem was not Emacs 29's alone: Emacs 30's `read-multiple-choice` also reads each key with `read-event`, which under `inhibit-quit` returns C-g as a character; only Emacs 31 moved it to the minibuffer. `superchat-approval-confirm` now binds `inhibit-quit` to nil around the question, so C-g cancels it on every supported Emacs. With that fix every suite, the canonical smoke, the kernel check and a real DeepSeek chat session pass on 30.1.

## Alternatives considered

**Keep 29.1 and fix both behaviours.** The `copy-tree` difference needs a record-copying fallback in the evidence serializer and the C-g one needs the same binding as above. It lost because nobody here runs Emacs 29, so a 29 path would stay untested between releases, and the user chose the version the code is exercised on.

**Declare 31 or later.** It would have avoided the C-g problem without code, but it excludes the current stable release line for no other reason; the fix is one binding.

## Consequences

- Test harnesses that feed keys to `read-multiple-choice` must handle both of its input paths: Emacs 30's `read-event` and Emacs 31's minibuffer (`scripts/m6-permissions-regression.el`, `m6p--confirm-with-keys`); the test of the modal `read-key` path skips on 30, which has no such path.
- Not verified: a GUI Emacs 30.1, and Emacs 30.2.
