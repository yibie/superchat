---
name: handoff-v1.0.1-patch
audience: deepseek-v4-pro
status: ready
predecessor: docs/handoff-v0.9-v1.0-melpa-ready.md (partially shipped)
---

# Handoff: v1.0.1 — finish the three skipped patch items

## Why you are reading this

The prior handoff (`handoff-v0.9-v1.0-melpa-ready.md`) had 7 steps.
You shipped steps 1, 2, 5, 7 cleanly. Steps 3, 4, 6 were skipped
or done incorrectly. v1.0.0 is already tagged; this handoff
finishes the three loose ends and ships v1.0.1.

## What needs fixing, with why the prior pass got it wrong

### Step A — checkdoc (was Step 3, marked `⚠️ Requires interactive Emacs`)

**That diagnosis is wrong.** checkdoc runs in batch mode. The
prior handoff already gave you the batch script. Here it is again,
cleaned up to actually work:

```bash
for f in superchat*.el; do
  emacs -batch --eval "
(progn
  (require 'checkdoc)
  (find-file \"$f\")
  (let ((checkdoc-arguments-in-order-flag nil)
        (checkdoc-verb-check-experimental-flag nil)
        (checkdoc-spellcheck-documentation-flag nil)
        (checkdoc-diagnostic-buffer \"*warn*\"))
    (checkdoc-current-buffer t))
  (with-current-buffer \"*warn*\"
    (when (> (buffer-size) 0)
      (princ (format \"=== %s ===\n%s\" \"$f\" (buffer-string))))))" 2>&1
done > /tmp/checkdoc-report.log 2>&1
grep -c "^=== " /tmp/checkdoc-report.log
cat /tmp/checkdoc-report.log | head -80
```

If `checkdoc-current-buffer` signals or refuses in batch (some
Emacs versions are funny), fall back to:

```bash
for f in superchat*.el; do
  emacs -batch -l checkdoc --eval "
(progn
  (find-file \"$f\")
  (goto-char (point-min))
  (while (re-search-forward \"^(\\\\(defun\\\\|defcustom\\\\|defvar\\\\|defmacro\\\\)\" nil t)
    (save-excursion
      (condition-case err
          (when-let ((problem (checkdoc-defun-info nil)))
            (when problem
              (princ (format \"%s:%d: %s\n\" \"$f\" (line-number-at-pos) problem))))
        (error nil)))))" 2>&1
done
```

### What to actually do

1. Run the script. Capture violations into `/tmp/checkdoc-report.log`.
2. Fix mechanical ones (period at end of first sentence, first-line
   length < 80, backticks around function references).
3. **Do NOT change semantics.** If a docstring's first sentence is
   inherently >80 chars and you can't shorten without losing meaning,
   leave it and list it in the final report under "checkdoc tolerances".
4. Re-run the script. Iterate until either empty output or every
   remaining warning is a documented tolerance.

### Acceptance

- The batch script produces either empty output or only tolerated
  warnings.
- Test count stays at 103/103.
- No byte-compile regression.

### Commit

`docs: checkdoc-clean docstrings across all modules`

---

### Step B — package-lint (was Step 4, marked `⚠️ Requires package-lint pkg`)

**Also wrong.** `package-install` works in batch. Here's the script
that installs it and runs it:

```bash
emacs -batch --eval "
(progn
  (require 'package)
  (add-to-list 'package-archives
    '(\"melpa\" . \"https://melpa.org/packages/\") t)
  (add-to-list 'package-archives
    '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)
  (package-initialize)
  (unless (package-installed-p 'package-lint)
    (package-refresh-contents)
    (package-install 'package-lint))
  (require 'package-lint)
  (find-file \"superchat.el\")
  (let ((report (package-lint-buffer)))
    (if report
        (dolist (entry report)
          (princ (format \"%s\n\" entry)))
      (princ \"clean\n\"))))" 2>&1 | tee /tmp/package-lint-report.log
```

If package-lint refuses because of how MELPA archive is configured,
also accept this fallback:

```bash
emacs -batch --eval "
(progn
  (require 'package)
  (setq package-user-dir (expand-file-name \"~/.emacs.d/elpa-batch\"))
  (add-to-list 'package-archives
    '(\"melpa\" . \"https://melpa.org/packages/\") t)
  (package-initialize)
  (unless (package-installed-p 'package-lint)
    (package-refresh-contents)
    (package-install 'package-lint)))" 2>&1
```

### What to actually do

1. Run the script. Fix every ERROR. Tolerate WARNINGS only with
   justification (in the final report).
2. Likely targets:
   - Missing `;; URL:` header in `superchat.el` (add: `;; URL: https://github.com/yibie/superchat`)
   - Missing `;; Keywords:` header (add: `;; Keywords: ai, chat, llm, agent` — already present? verify)
   - `Package-Requires:` not matching reality — **see Step C below**
3. Re-run script. Iterate until errors are zero.

### Acceptance

- `package-lint-buffer` reports either zero errors or only
  documented tolerances.
- Test count stays at 103/103.
- No byte-compile regression.

### Commit

`fix(package): package-lint clean for v1.0.1 — URL header, etc`

---

### Step C — `(llm "0.7")` bump (was Step 6, validated wrong)

**The prior pass validated `(llm "0.7")` by running tests on 0.31.0.**
That tells you "0.31 works," not "0.7 is the minimum." MELPA reviewers
check that `Package-Requires:` is the actual minimum.

### Research already done for you

I checked `llm.el`'s `NEWS.org` for every `llm-*` symbol superchat
uses. Results:

| symbol               | introduced in |
|----------------------|---------------|
| `llm-name`           | 0.8           |
| `llm-make-chat-prompt` | 0.14        |
| `llm-models`         | 0.24          |
| `llm-make-tool`      | (not in NEWS — present by ≤0.24 based on related tool-use changelog) |
| `llm-chat-streaming` | (not in NEWS — predates the tracked changelog window, safe at 0.8) |
| `llm-backend`, `llm-chat`, `llm-tools`, etc | older or untracked |

**The binding minimum is `llm-models` at 0.24.0.**

### What to do

1. Update `superchat.el` header:
   ```
   ;; Package-Requires: ((emacs "28.1") (llm "0.24"))
   ```

2. Update `superchat-pkg.el`:
   ```elisp
   '((emacs "28.1") (llm "0.24"))
   ```

3. **DO NOT TRY TO INSTALL llm-0.24 to verify.** The reasoning above
   is sufficient evidence for MELPA review; running tests against
   0.24 is nice-to-have but not blocking. If you happen to install
   it and tests pass, great — note it in the final report. If you
   can't install it (old GNU ELPA versions get garbage-collected),
   that's expected; the changelog evidence holds.

### Acceptance

- `grep "llm" superchat.el | grep "Package-Requires"` shows `0.24`.
- `superchat-pkg.el` shows `0.24`.
- Test count stays at 103/103 (you're not changing code — tests
  run on whatever llm version your environment has).

### Commit

`fix(deps): bump (llm "0.7") → (llm "0.24") to match llm-models requirement`

---

### Step D — v1.0.1 release ceremony

1. Update `superchat.el` header: `Version: 1.0.1`.
2. Update `superchat-pkg.el`: `(define-package "superchat" "1.0.1" ...)`.
3. Update `ROADMAP.md` revision history with today's date and a
   one-line "v1.0.1: checkdoc/package-lint cleanup + llm minimum
   version correction".
4. README CHANGELOG: append a `### 1.0.1` entry.
5. `git tag -a v1.0.1 -m "Patch: lint cleanup + correct llm minimum"`
6. **DO NOT push the tag.** User does that.

### Acceptance

- `git tag` shows both `v1.0.0` and `v1.0.1`.
- `superchat.el` and `superchat-pkg.el` both say `1.0.1`.
- Test count stays at 103/103.

### Commit

`release: v1.0.1`

---

## Validation checklist (run after EACH step)

```bash
git status   # must be clean before each step

# Byte-compile
rm -f superchat*.elc
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
  | grep -E "Warning|Error"
# Baseline (HEAD 01fb9ab): tolerated warnings from llm/org references.
# Nothing should get worse.

# Tests
emacs -batch -L . -L test -l ert \
  -l test/test-core-pipeline.el \
  -l test/test-llm-backend.el \
  -l test/test-memory-soul.el \
  -l test/test-memory-sqlite-facade.el \
  -l test/test-prompt-hooks.el \
  -l test/test-skills.el \
  -l test/test-skills-integration.el \
  -l test/test-skills-roundtrip.el \
  -l test/test-workflow.el \
  -l test/test-post-turn-hooks.el \
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED" | tail -5
# Must remain: Ran 103 tests, 103 results as expected, 0 unexpected
```

---

## Out of scope (do NOT touch)

- Anything else in the v1.0.0 codebase.
- Re-running steps 1, 2, 5, 7 from the prior handoff (already shipped).
- Submitting to MELPA. User does that.
- Pushing the v1.0.1 tag. User does that.
- Refactoring for lint compliance — only docstring / header edits.
- Changing test assertions.

## When NOT to ship a step

- If checkdoc reports zero issues after running the batch script,
  Step A's commit message becomes: `docs: confirm checkdoc clean
  across all modules` (still commit so the verification is on
  record — but with no file changes if none were needed).
- If package-lint reports zero issues, Step B's commit becomes
  similar: `docs(package): confirm package-lint clean` with whatever
  the actual diff is (might just be header additions).
- Step C and D are required — they have real file changes.

## Final report

Reply with:

1. Commit SHA list (4 commits, A → B → C → D).
2. Per-step: lines added/removed across files touched.
3. Test totals at start and end (must both be 103/103).
4. Byte-compile output before and after (must be no worse).
5. checkdoc batch script output — paste the FULL output if non-empty,
   or "clean" if empty.
6. package-lint output — same treatment.
7. checkdoc tolerances and package-lint tolerances (if any), each
   with file:line and one-sentence justification.
8. `git tag` output (showing both v1.0.0 and v1.0.1).
9. The exact `grep Package-Requires superchat.el` output.

If any step reveals a real blocker (e.g. package-lint demands a
structural change), commit what's done, mark the rest as deferred,
and append "Q&A for follow-up" with the specific blocking question.
**Do NOT skip steps because "it needs interactive Emacs."** That
diagnosis is wrong; the scripts above run in batch.
