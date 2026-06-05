---
name: handoff-v0.9-v1.0-melpa-ready
audience: deepseek-v4-pro
status: ready
predecessors:
  - docs/handoff-v0.7-finish-and-bub-phase1-d.md (shipped, commits 68ed5a5..bce5db8)
  - docs/handoff-v0.7-skills-workflow.md (shipped, commits 22dfb66..4782cf3)
  - docs/handoff-v0.9-hook-alignment.md (shipped)
  - docs/handoff-v0.9-split.md (shipped)
  - docs/handoff-v0.8-sqlite-memory.md (shipped)
---

# Handoff: v0.9 stabilization → v1.0 MELPA-ready

## Why you are reading this

The functional surface of superchat is done: Bub-style hook pipeline,
SQLite memory facade, unified SKILL.md format with workflow sub-type
— all SHIPPED. What's left between here and a v1.0 MELPA submission
is **mechanical packaging work**: autoloads, lint, CI, version
metadata, and finishing one cosmetic test.

This handoff completes v0.9 and ships v1.0. Six commits, in order,
one per step.

## Trust the repo, not your priors

`git status` is the source of truth. Re-verify before each step:

```bash
git status        # must be clean before you begin each step
git log --oneline -5
wc -l superchat*.el
```

HEAD this handoff was written against:
```
bce5db8  feat(core): migrate user-side tape recording to post-turn hook (Bub Phase 1 D)
```

If `git status` shows uncommitted changes when you begin, **stop**
and ask the user.

## Reality check on the ROADMAP

The `ROADMAP.md` v0.9 section is out of date — some "TODO" items
are actually done. Verified state on `bce5db8`:

| ROADMAP claim                                            | Actual on bce5db8                 |
|----------------------------------------------------------|-----------------------------------|
| `defcustom` missing `:type`                              | ✅ all 42 have `:type` already    |
| `cl-no-applicable-method` abort at `test-llm-backend:147` | ✅ 36/36 passing, no abort        |
| Working-tree noise / `.gitignore` reconciliation          | ✅ done (`20fe346`, `4a9ca0d`)    |
| `;;;###autoload` only 2                                  | ⚠️ true — see Step 2              |
| `checkdoc` / `package-lint` clean                         | ❓ not run — see Steps 3, 4       |
| CI on Emacs 28.1 / 29 / 30                                | ❌ no `.github/` — see Step 5     |
| `(llm "0.7")` minimum bump                                | ❓ not measured — see Step 6      |

Test baseline (verified):
```
Ran 103 tests, 102 results as expected, 1 unexpected
FAILED  roundtrip/code-review-byte-identical
```

Your job is to clear the `⚠️ ❓ ❌` rows, ship v1.0, and leave the
test suite green. The one red test is a v0.7 known-failure you will
fix in Step 1.

---

## Step 1 — Repair the v0.7 known-failure

**Goal**: `Ran 103 tests, 103 results as expected, 0 unexpected`.

### What's broken

`test/test-skills-roundtrip.el::roundtrip/code-review-byte-identical`
asserts that a SKILL.md written via `superchat-skills-standard-export`
round-trips byte-identical to the input. It fails because
`superchat-skills-load` (in `superchat-skills.el`) calls
`(setq body (string-trim ...))` on the body after stripping the
frontmatter — that trims the leading `\n\n` separator between the
frontmatter close (`---\n`) and the first body line. The exporter
then writes the trimmed body, producing a file shorter than the
input by exactly those two newlines.

Original commit context: the prior session declined to fix this and
left it as a "known contract boundary." You'll fix it properly.

### Approach

In `superchat-skills.el`, in the function where you find
`(setq body (string-trim (substring raw (match-end 0))))`, replace
the trimmed substring with the raw substring:

```elisp
(setq body (substring raw (match-end 0)))
```

That alone may flip the test green. Verify:

```bash
emacs -batch -L . -L test -l ert -l test/test-skills-roundtrip.el \
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED" | tail -5
```

If the test still fails, the next-most-likely cause is the exporter
writing an extra trailing newline or reordering frontmatter keys.
Read `superchat-skills-standard.el`'s `-export` function and check
that:
1. Frontmatter key order matches the test fixture
   (`name, description, version, type[, triggers]`).
2. Body is written verbatim — no `\n` appended, no `string-trim`.

### Acceptance

- Test count remains 103. All 103 pass.
- No other test regresses. Run the full suite:
  ```bash
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
  ```
- Byte-compile produces no new warnings vs. the baseline you capture
  *before* your first edit:
  ```bash
  rm -f superchat*.elc
  emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
    | grep -E "Warning|Error" > /tmp/v10-bc-baseline.log
  # ... do your work ...
  rm -f superchat*.elc
  emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
    | grep -E "Warning|Error" > /tmp/v10-bc-current.log
  diff /tmp/v10-bc-baseline.log /tmp/v10-bc-current.log
  ```

### Commit

`fix(skills): preserve body verbatim in load so export round-trips byte-identical`

---

## Step 2 — `;;;###autoload` coverage

**Goal**: every public interactive entry point a user might run via
`M-x` is autoloaded.

### What's missing

Today only `superchat` and `superchat-ensure-directories` in
`superchat.el` are autoloaded. The other public interactive commands
across modules are NOT, which means users need to `(require 'superchat-xxx)`
first or it just fails. Audit them.

### Approach

1. Find all interactive entry points outside `superchat.el`:
   ```bash
   awk '/^\(defun\s+superchat-[a-z]/{name=$2; file=FILENAME}
        /^\s*\(interactive/{if(name)print file": "name; name=""}' \
     superchat*.el | grep -v "^\\./superchat\\.el:"
   ```

2. For each, decide whether it's a USER-FACING command (a thing a
   user might run from `M-x`) or an INTERNAL helper that happens to
   be interactive (e.g. for debug). Autoload only the former.

3. Use this filter: if the docstring is written for end-users, or
   it's documented in `README.md`, autoload it. If it's a
   testing/debug helper, don't.

4. Likely autoload candidates (verify each by reading the docstring):
   - `superchat-send-input` (dispatcher) — yes, it's the C-c C-c binding
   - `superchat-memory-import-from-org` (memory) — yes, one-shot migration
   - `superchat-memory-prune` (memory) — yes, maintenance
   - `superchat-workflow-import-legacy` (workflow) — yes, one-shot migration
   - `superchat-skills-standard-list` (skills-standard) — yes
   - `superchat-skills-standard-import` (skills-standard) — yes
   - `superchat-models` related — read & decide
   - `superchat-save-*` — read & decide
   - `superchat-mcp` — read & decide

5. Insert `;;;###autoload` on the line directly above each
   `(defun ...)` you autoload. Generate the loaddefs check:
   ```bash
   emacs -batch --eval "(progn
     (let ((generated-autoload-file (expand-file-name \"superchat-autoloads.el\")))
       (update-directory-autoloads \".\")))" 2>&1 | tail -5
   ```
   The resulting `superchat-autoloads.el` is a build artifact —
   don't commit it. It's regenerated by MELPA.

### Acceptance

- `grep -c ';;;###autoload' superchat*.el | grep -v ':0$'` shows
  at least 8 hits across multiple files.
- The autoloads file generates cleanly with no errors.
- No test regression.
- No byte-compile regression.

### Commit

`feat(autoloads): mark user-facing interactive commands as autoloads`

---

## Step 3 — `checkdoc` cleanup

**Goal**: every `.el` file passes `M-x checkdoc-current-buffer` clean.

### What checkdoc checks

- First line of docstrings is a complete sentence (max ~80 chars)
- Docstrings end with a period
- Function references in docstrings use backticks: `` `foo' ``
- No trailing whitespace
- File headers in correct format

### Approach

1. Run checkdoc on every file, collect violations:
   ```bash
   for f in superchat*.el; do
     emacs -batch --eval "
       (progn
         (find-file \"$f\")
         (let ((checkdoc-diagnostic-buffer \"*checkdoc-batch*\"))
           (checkdoc-current-buffer t)
           (with-current-buffer checkdoc-diagnostic-buffer
             (when (> (buffer-size) 0)
               (princ (format \"\\n=== %s ===\\n\" \"$f\"))
               (princ (buffer-string))))))" 2>&1
   done > /tmp/checkdoc-report.log
   wc -l /tmp/checkdoc-report.log
   ```

2. Fix them. The bulk are docstring punctuation and length —
   mechanical edits. Don't rewrite documentation, just fix the
   format.

3. The audit is per-file; commit per-file if any single file's
   diff exceeds 50 lines, otherwise one commit is fine.

### Hard rules

- Do NOT change behavior. Don't refactor functions to make
  docstrings shorter — just shorten the first sentence.
- Do NOT delete a docstring to silence a warning. Write a one-line
  summary instead.
- If checkdoc complains about a docstring you literally cannot
  shorten without losing meaning, leave it and note in the commit
  body: `Tolerated: <file>:<line> — <reason>`.

### Acceptance

- `checkdoc` produces zero warnings for `superchat*.el`.
- Test count unchanged: 103/103.
- No byte-compile regression.

### Commit

`docs: checkdoc-clean docstrings across all modules`

(or multiple `docs(<module>): checkdoc-clean ...` commits if you
split by file)

---

## Step 4 — `package-lint` cleanup

**Goal**: `superchat.el` passes `M-x package-lint-current-buffer`
clean. (Other files are non-package-entry helper files; if
package-lint flags them, it's informational.)

### Install package-lint (if not already)

```bash
emacs -batch --eval "(progn
  (require 'package)
  (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\"))
  (package-initialize)
  (package-refresh-contents)
  (unless (package-installed-p 'package-lint)
    (package-install 'package-lint)))"
```

### Run

```bash
emacs -batch --eval "(progn
  (require 'package)
  (package-initialize)
  (require 'package-lint))" \
  -L . -l superchat.el \
  --eval "(find-file \"superchat.el\")" \
  --eval "(package-lint-current-buffer)" 2>&1 | tail -30
```

### Common things it flags (be ready to fix)

- `Package-Requires:` mismatch with actual `(require ...)` calls
- Missing autoload cookie on public-facing entry
- Missing `;; URL:` header
- Missing `;; Keywords:` header
- Symbols that look like they're from another package's namespace
- `lexical-binding: t` missing from file header

### Approach

1. Fix every error.
2. Tolerate warnings only with justification (in commit body).
3. Update `superchat.el`'s header to include:
   ```
   ;; URL: https://github.com/yibie/superchat
   ;; Keywords: ai, chat, llm, agent
   ```
   (Confirm the repo URL with the user if uncertain.)

### Acceptance

- `package-lint` produces zero errors on `superchat.el`.
- Test count unchanged: 103/103.
- No byte-compile regression.

### Commit

`fix(package): package-lint clean for v1.0 submission`

---

## Step 5 — GitHub Actions CI

**Goal**: `.github/workflows/test.yml` runs ERT on Emacs 28.1,
29.x, and 30.x for every push and PR.

### Create the workflow

`.github/workflows/test.yml`:

```yaml
name: tests

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      fail-fast: false
      matrix:
        emacs_version: ['28.1', '29.4', '30.1']

    steps:
      - uses: actions/checkout@v4

      - uses: purcell/setup-emacs@master
        with:
          version: ${{ matrix.emacs_version }}

      - name: Install dependencies
        run: |
          emacs -batch --eval "(progn
            (require 'package)
            (add-to-list 'package-archives
              '(\"gnu\" . \"https://elpa.gnu.org/packages/\"))
            (add-to-list 'package-archives
              '(\"melpa\" . \"https://melpa.org/packages/\"))
            (package-initialize)
            (package-refresh-contents)
            (unless (package-installed-p 'llm)
              (package-install 'llm)))"

      - name: Byte-compile
        run: |
          emacs -batch -L . \
            --eval "(setq byte-compile-error-on-warn nil)" \
            -f batch-byte-compile superchat*.el

      - name: Run tests
        run: |
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
            -f ert-run-tests-batch-and-exit
```

### Verify locally before committing

```bash
# Lint the YAML
python3 -c "import yaml; yaml.safe_load(open('.github/workflows/test.yml'))"
```

### Notes

- If tests need SQLite for the memory tests, `ubuntu-latest`
  bundles it. If you see `sqlite3` errors in any Emacs version,
  check `(require 'sqlite)` availability — Emacs 29+ has builtin,
  Emacs 28 needs `(emacsql-sqlite3)` or fallback.
- If any specific Emacs version reveals a real test failure, do
  NOT silence it. Document it in the commit body and either fix
  it or restrict the matrix (with explicit reason).

### Acceptance

- `.github/workflows/test.yml` exists.
- YAML parses.
- (Cannot verify CI itself runs without push — the user will see
  it on the GitHub side.)

### Commit

`ci: add GitHub Actions matrix test on Emacs 28.1 / 29 / 30`

---

## Step 6 — Bump `(llm "0.7")` to the actual tested minimum

**Goal**: `Package-Requires:` claim matches reality.

### Why this matters

MELPA reviewers check that `Package-Requires:` reflects the actual
minimum version of each dependency that the code USES. Today
`superchat.el` says `(llm "0.7")`, but the v0.5 → v0.9 work used
APIs that may not exist in 0.7.

### Approach

1. Identify which `llm` API symbols superchat uses:
   ```bash
   grep -nE "\\(llm-[a-z]" superchat*.el | sort -u -t: -k3
   ```

2. Check each one against the `llm.el` changelog at
   `https://github.com/ahyatt/llm/blob/main/NEWS.org`. Find the
   minimum version that introduced ALL of them.

3. As a sanity check, try installing the candidate minimum and
   running tests:
   ```bash
   emacs -batch --eval "(progn
     (require 'package)
     (package-initialize)
     (package-install-file \"/path/to/llm-X.Y.tar\"))" \
     ...
   ```
   (This is best-effort; if you can't easily install old versions,
   pick the version that introduced the latest API you use and
   document the choice.)

4. Update `superchat.el`:
   ```
   ;; Package-Requires: ((emacs "28.1") (llm "X.Y"))
   ```

### Acceptance

- `superchat.el` header reflects the minimum version verified to
  contain every `llm-*` symbol the code uses.
- Document your chosen version + reasoning in the commit body.

### Commit

`fix(deps): bump (llm "...") to actual minimum API version used`

---

## Step 7 — v1.0 release ceremony

**Goal**: tagged v1.0.0 release ready for MELPA submission.

### Required changes

1. **`superchat.el` header**:
   ```
   ;; Version: 1.0.0
   ```
   (was `0.5`)

2. **`README.md` + `README_cn.md`**: add a CHANGELOG section near
   the top with the v0.5 → v1.0.0 milestone summary. Format:
   ```markdown
   ## Changelog

   ### 1.0.0 (2026-06-XX)
   - Bub-style agent runtime (turn struct + hook pipeline)
   - SQLite memory backend (replaces org-mode storage)
   - Unified SKILL.md format with `type: workflow` sub-type
   - Public API stable; MELPA-ready

   ### 0.5 (2026-XX-XX)
   - Switch from gptel to llm.el
   ...
   ```
   Pull SHAs and dates from `git log --oneline` and the ROADMAP.

3. **`AGENTS.md`**: bump the "Active milestone" section to v1.0 if
   present. If not present, leave alone.

4. **`ROADMAP.md`**: mark v0.9 and v1.0 as SHIPPED. Add a revision
   history entry dated today.

5. **Git tag**:
   ```bash
   git tag -a v1.0.0 -m "First MELPA-ready stable release"
   ```
   **DO NOT push the tag.** The user will do that after reviewing.

### MELPA recipe (do NOT submit; just prepare the file)

Create `superchat-pkg.el` (or update if it exists):

```elisp
(define-package "superchat" "1.0.0"
  "AI chat for Emacs with hook-pipeline agent runtime."
  '((emacs "28.1") (llm "X.Y"))
  :authors '(("Yibie" . "yibie@outlook.com"))
  :maintainer '("Yibie" . "yibie@outlook.com")
  :keywords '("ai" "chat" "llm" "agent")
  :url "https://github.com/yibie/superchat")
```

Use the version from Step 6 for the llm requirement.

The actual MELPA recipe (a separate file in the MELPA repo, not in
this repo) is the user's job to submit. You're just making sure
this repo is shaped right for it.

### Acceptance

- `git log --oneline -1` shows the v1.0 commit on HEAD.
- `git tag` shows `v1.0.0`.
- `superchat.el` header says `Version: 1.0.0`.
- README has CHANGELOG.
- ROADMAP shows v0.9 and v1.0 as SHIPPED with this commit.
- `superchat-pkg.el` exists.
- Tests still 103/103.
- Byte-compile still clean.

### Commit

`release: v1.0.0`

Tag separately (user will push tag manually).

---

## Validation checklist (run after EACH step)

```bash
# 1. Working tree
git status

# 2. Byte-compile clean vs baseline
rm -f superchat*.elc
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
  | grep -E "Warning|Error" > /tmp/v10-bc-current.log
diff /tmp/v10-bc-baseline.log /tmp/v10-bc-current.log
# Empty diff = OK. Non-empty = explain in commit body or revert.

# 3. Tests
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
```

Expected test progression:

| After step | Total | Passing | Failing |
|------------|-------|---------|---------|
| Baseline   | 103   | 102     | 1       |
| Step 1     | 103   | 103     | 0       |
| Step 2     | 103   | 103     | 0       |
| Step 3     | 103   | 103     | 0       |
| Step 4     | 103   | 103     | 0       |
| Step 5     | 103   | 103     | 0       |
| Step 6     | 103   | 103     | 0       |
| Step 7     | 103   | 103     | 0       |

---

## Out of scope (do NOT touch)

- v0.8.5 MCP v2 work (multi-server, namespace, health checks).
  Defer to v1.1.
- Bub Phase 2 (hook registry, command dispatch as hook chain).
  Out of scope until external plugin demand exists.
- Adding new defcustoms.
- Refactoring any module to make lint happy. Move docstrings, not
  function boundaries.
- The `roundtrip/code-review-byte-identical` test's assertion shape.
  Fix the production code so the test passes; don't relax the test.
- Submitting to MELPA. User does that.
- Pushing the v1.0.0 tag. User does that.

---

## Decisions to make (state your choice in final report)

1. **Min Emacs version**: keep `28.1` or bump to `28.2`?
   *Default*: keep `28.1`. Bumping costs users; no concrete reason
   to require 28.2.
2. **Cask / Eldev for reproducible dev**: add either?
   *Default*: no. GitHub Actions CI from Step 5 covers reproducibility.
   Cask adds dev friction for no MELPA benefit.
3. **Autoload threshold**: a function is "user-facing enough" to
   autoload if ___?
   *Default*: documented in README OR docstring written in
   second-person ("Run this when you ..."). Internal helpers stay
   private.
4. **`superchat.el.elc`-style artifacts in git**: any present?
   *Default*: verify with `git ls-files | grep -E '\.elc$'`. Should
   already be empty per ROADMAP. If anything appears, remove and
   ensure `.gitignore` catches it.

---

## Final report

Reply with:

1. Commit SHA list, one per step (7 commits).
2. Diff in line counts for every `.el` file you touched.
3. Test totals: before/after for each step.
4. Byte-compile diff against the baseline you captured at the very
   beginning. Must be empty.
5. The exact `(llm "X.Y")` version you settled on, with the
   reasoning from Step 6.
6. List of every function you marked `;;;###autoload` in Step 2,
   one line each: `file:lineno function-name`.
7. checkdoc tolerances (Step 3) — lines where you deliberately
   left a warning unfixed, with reason.
8. package-lint tolerances (Step 4) — same.
9. Resolutions for the four decisions above.
10. Anything you skipped, with reason.
11. The exact `git tag` output after Step 7.

If any step reveals an unrecoverable blocker (e.g. package-lint
demands a structural change), commit what's done, mark the rest
as deferred, and append a "Q&A for follow-up" section with your
specific blocking question.
