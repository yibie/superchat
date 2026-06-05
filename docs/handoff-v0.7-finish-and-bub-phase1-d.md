---
name: handoff-v0.7-finish-and-bub-phase1-d
audience: deepseek-v4-pro
status: ready
predecessors:
  - docs/handoff-v0.9-split.md (shipped)
  - docs/handoff-v0.9-hook-alignment.md (shipped, commits 0c1bb37..037a7fd)
  - docs/handoff-v0.8-sqlite-memory.md (shipped, commits bf7cc43..7a5d893)
  - docs/handoff-v0.7-skills-workflow.md (PARTIAL — steps 1-4 shipped, 5-6 pending; 5 tests red)
  - docs/design/AGENTS-phase1-plan.md (Bub Phase 1 plan — A/B/C migrated by v0.9; D pending)
---

# Handoff: finish v0.7 + Bub Phase 1 migration D

## Why you are reading this

A prior implementer started v0.7 (Skills v2 + Workflow restoration)
and got through steps 1-4 cleanly, but the test suite regressed: 5
tests are red on `main`. v0.7 step 5 (legacy `.workflow` import) and
step 6 (docs) were never started.

Separately, the Bub-style agent runtime plan in
`docs/design/AGENTS-phase1-plan.md` calls for four migrations
(A/B/C/D) of features into the hook pipeline. A/B/C shipped in
v0.9-hook-alignment. Migration **D** — moving tape recording to a
`post-turn` hook — is still imperative; `superchat-post-turn-functions`
is defined but has zero registrants, and `superchat--record-message`
writes to the SQLite tape directly from the dispatch path.

You will do four things, in order, one commit each:

1. Fix the 5 red tests (do not change product code unless the test
   reveals a real bug — see Step 1 for what's actually broken).
2. Ship v0.7 step 5 (legacy import shim).
3. Ship v0.7 step 6 (docs + ROADMAP update).
4. Ship Bub Phase 1 migration D (post-turn tape hook).

**Do not start step 2 until step 1 is green. Do not start step 4 until
v0.7 is shipped and tagged in ROADMAP.**

---

## Trust the repo, not your priors

The previous session that produced this handoff repeatedly read
stale views of `superchat.el` and produced edits that did not land
on disk. Treat every claim below as needing verification:

```bash
# Always start by re-confirming
git status        # must be clean before each step
git log --oneline -10
wc -l superchat*.el
```

If `git status` shows uncommitted changes when you begin, **stop**
and ask the user. The HEAD this handoff was written against:

```
HEAD: 4782cf3  feat(dispatch): route SKILL.md type=workflow to step executor (step 4)
```

## State of the world (verified 2026-06-05)

### Modules and line counts

```
superchat.el                1551   public entry + record-message + history
superchat-core.el            139   turn struct, hook defvars, pipeline runner
superchat-dispatcher.el      377   send-input, execute-llm-query, dispatch-result
superchat-prompt-hooks.el    168   5 hooks for system+build-prompt
superchat-memory.el          337   SQLite facade
superchat-db.el              425   SQLite layer (memory + tape)
superchat-skills.el          749   skills loader + implicit-match
superchat-skills-standard.el 305   SKILL.md frontmatter loader (v0.7 step 1 done)
superchat-workflow.el         94   workflow executor (v0.7 step 3 done)
superchat-llm.el             173
superchat-render.el          249
superchat-mcp.el             108
superchat-models.el          207
superchat-parser.el           77
superchat-save.el             87
superchat-executor.el        227
superchat-tools.el           545
```

### What's wired into the hook pipeline today

`superchat-core-run-turn` runs: parse → `system-prompt-functions`
→ `build-prompt-functions` → `post-turn-functions`.

| Hook variable                       | Registrants                                                                                                                                       | Status                  |
|-------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------|
| `superchat-system-prompt-functions` | `superchat-prompt-hook--language-instruction`                                                                                                     | active                  |
| `superchat-build-prompt-functions`  | `--file-inline`, `--template-substitution`, `--memory-context`, `--conversation-history` (in `superchat-prompt-hooks.el`)                         | active                  |
| `superchat-post-turn-functions`     | **(none)**                                                                                                                                        | **YOU FIX IN STEP 4**   |
| `superchat-command-hooks`           | used by `superchat--handle-command` for third-party command extension                                                                             | working as designed     |

### What's NOT in the hook pipeline (intentional, in `superchat.el`)

- `superchat--record-message` — writes to `superchat--conversation-history`
  and calls `superchat-db-tape-append`. Called from
  `superchat--dispatch-result` (user message side) and from the
  streaming/non-streaming callbacks in `superchat-llm.el` (assistant
  side). Step 4 of this handoff replaces the dispatcher-side call
  with a `post-turn` hook. **The assistant-side call stays put** —
  see "Why not migrate the assistant side too" below.

### Test baseline (verified)

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
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED " | tail -10
```

Result on `4782cf3`:
```
Ran 99 tests, 94 results as expected, 5 unexpected
```

The five reds:
```
FAILED  roundtrip/code-review-byte-identical
FAILED  roundtrip/preserves-triggers-list
FAILED  roundtrip/preserves-type-workflow
FAILED  roundtrip/preserves-version
FAILED  test-prompt-building-with-skill
```

The first four are all from `test/test-skills-roundtrip.el` (v0.7
step 2). They strongly imply `--export` is still the minimal-writer
the prior handoff said to fix in step 2 — i.e. step 2 landed the
TESTS but not the matching production change. The fifth is in
`test/test-skills-integration.el` and may be collateral. Investigate
both before assuming.

---

## Step 1 — Make the test suite green again (FIRST)

**Goal**: `Ran 99 tests, 99 results as expected, 0 unexpected`.
No new tests, no deletions, no `skip-unless`. If you find that a
test is wrong, document it in the commit message and the final
report — do not silently relax assertions.

### Approach

1. Reproduce locally:
   ```bash
   emacs -batch -L . -L test -l ert -l test/test-skills-roundtrip.el \
     -f ert-run-tests-batch-and-exit 2>&1 | tail -40
   ```
2. Read `superchat-skills-standard.el` end to end. Confirm whether
   `-export` actually preserves `version`, `type`, `triggers` in the
   field order documented by step 2 of the prior handoff
   (`name, description, version, type, triggers`). If it doesn't,
   make it. The prior handoff already specified the contract — your
   job is to match it.
3. The byte-identical test will fail if either side writes a stray
   blank line or reorders keys. Look for trailing-newline mismatches
   first; they're the most common round-trip break.
4. Triage `test-prompt-building-with-skill` separately. If it depends
   on the export shape, fixing 1-3 may green it incidentally.

### Acceptance

- Test count stays at 99. All 99 pass.
- No production change outside `superchat-skills-standard.el` unless
  you can justify it. If you do touch other modules, list every file
  in the commit body.
- Byte-compile produces no new warnings vs. the baseline you capture
  *before* your first edit:
  ```bash
  rm -f superchat*.elc
  emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
    | grep -E "Warning|Error" > /tmp/v07d-bc-baseline.log
  # ... do your work ...
  rm -f superchat*.elc
  emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
    | grep -E "Warning|Error" > /tmp/v07d-bc-after.log
  diff /tmp/v07d-bc-baseline.log /tmp/v07d-bc-after.log
  ```

### Commit

`fix(skills): repair SKILL.md round-trip export per v0.7 step 2 contract`

---

## Step 2 — v0.7 step 5: legacy `.workflow` import shim

Pick up from the prior handoff (`docs/handoff-v0.7-skills-workflow.md`,
section "Step 5"). The contract there is authoritative; this section
only flags the things that need a fresh eye now that step 3 + 4 are
on disk.

### What changed since the prior handoff was written

- `superchat-workflow.el` already exists (94 lines). Read it first.
  Its public API is whatever step 3 chose; if it deviates from the
  prior handoff's contract, document the actual API in your commit
  body and write the shim against the actual API, not the prior
  spec.
- A test fixture skill (`>test-workflow some-arg`) may or may not
  exist under `examples/standard-skills/`. Check before adding —
  if step 4's commit added it, reuse it.

### Choose Option A (write SKILL.md to disk) unless

The legacy directory is read-only, or the standard skills directory
is missing/unwritable. Then fall back to Option B (in-memory only)
with a `display-warning`. Document the choice in the docstring of
`superchat-workflow-import-legacy-dir`.

### Acceptance

- `M-x superchat-workflow-import-legacy` works on a temp dir
  containing a fake `.workflow` file.
- One new ERT test:
  `workflow-import-legacy-synthesises-skill` in
  `test/test-workflow.el`.
- Test count goes to 100. All pass.

### Commit

`feat(workflow): legacy .workflow file import shim`

---

## Step 3 — v0.7 step 6: docs + ROADMAP

Per the prior handoff. Specifically:

- `README.md` / `README_cn.md`: section on unified SKILL.md format
  + `type: workflow` example. One screenshot or one code block
  each, not both.
- `docs/SKILLS_QUICKSTART.md`: canonical frontmatter form, workflow
  quickstart subsection.
- `docs/SKILL_FORMAT_CONVERSION.md`: field ordering
  (`name`, `description`, `version`, `type`, `triggers`).
- `ROADMAP.md`:
  - Mark v0.7 SHIPPED.
  - List commit SHAs from steps 1-5 of the prior handoff + step 1
    + step 2 of THIS handoff.
  - Fill in the four open-question resolutions (defaults from the
    prior handoff are fine unless you disagree — say which one you
    changed and why).
- Resolve the four "Decisions to resolve" from the prior handoff.
  Defaults are likely fine; just pick and document.

### Acceptance

- README files and quickstart show the new format.
- ROADMAP has v0.7 in SHIPPED state with concrete commit SHAs.
- No test changes.

### Commit

`docs(skills): v0.7 SKILL.md format + workflow restoration shipped`

---

## Step 4 — Bub Phase 1 migration D: post-turn tape hook

This is **the new work**. It implements the last item from
`docs/design/AGENTS-phase1-plan.md` § Step 1.4 / migration D, which
says:

> Tape recording → post-turn hook

### The problem in concrete terms

`superchat-post-turn-functions` is defined in `superchat-core.el:62`
but no one registers anything on it. Tape writes happen via
`superchat--record-message` in `superchat.el:516-547`, called
imperatively from `superchat-dispatcher.el:345, 367` (user side)
and from `superchat-llm.el`'s streaming/non-streaming callbacks
(assistant side).

The user-side call is the migration target. Today the dispatcher
hand-calls `(superchat--record-message "user" user-message)` right
before dispatch. After this step, the dispatcher does nothing, and
a hook on `superchat-post-turn-functions` writes the user side of
the tape after the pipeline finishes.

### Why not migrate the assistant side too

The assistant message is only known *after* the LLM finishes (or
streams). The hook chain runs synchronously inside
`superchat-core-run-turn`, before the LLM is even called. Migrating
the assistant write to a post-turn hook would require either (a)
making the hook chain re-run after LLM completion (out of scope —
that's Phase 2 territory) or (b) splitting `post-turn` into "post-prepare"
and "post-llm" sub-phases (also out of scope).

So the assistant write stays in the LLM callback. This handoff
explicitly limits migration D to the user-side call.

### Implementation

1. **Pick a home for the hook.** Add it to `superchat.el`, next to
   `superchat--record-message`. Do not put it in
   `superchat-prompt-hooks.el` (that file is about prompt building).
   Do not create a new `superchat-tape-hooks.el` for one function —
   YAGNI for now.

2. **Write the hook**:
   ```elisp
   (defun superchat--hook-record-user-message (turn)
     "Post-turn hook: append the user side of TURN to the SQLite tape.
   Reads `superchat-turn-clean-input', not `inbound', so model/skill
   prefixes and #file refs do not pollute the tape."
     (let ((text (string-trim (or (superchat-turn-clean-input turn) ""))))
       (when (> (length text) 0)
         (superchat--record-message "user" text)))
     nil)
   ```

   Hook returns nil — post-turn hooks are side-effect only per
   `superchat-core.el:11`.

3. **Register it**:
   ```elisp
   (add-hook 'superchat-post-turn-functions
             #'superchat--hook-record-user-message)
   ```

4. **Delete the imperative calls** in
   `superchat-dispatcher.el:345` and `:367`:
   - Line 345 is inside `superchat--dispatch-result`'s `:llm-query`
     branch. Remove the `(when ...) (superchat--record-message "user" ...)`
     block.
   - Line 367 is inside the `:llm-query-and-mode-switch` branch.
     Same removal.

5. **Verify** the post-turn hook fires once per `superchat-send-input`
   call. `superchat-core-run-turn` runs `post-turn-functions` at the
   end of every invocation. The dispatcher path that calls
   `superchat--execute-llm-query` does NOT re-invoke
   `superchat-core-run-turn`, so there's no double-write risk —
   verify by re-reading `superchat-dispatcher.el:271-319` (the main
   send-input flow) to confirm the chain is single-pass.

### Test additions

Add `test/test-post-turn-hooks.el` with at minimum:

- `post-turn-hook-records-user-message-once` — drive a fake turn
  through `superchat-core-run-turn` with the LLM mocked out and
  `superchat-db-tape-append` mocked via `cl-letf`. Assert tape was
  called exactly once with the user's clean input.
- `post-turn-hook-skips-empty-input` — turn with empty
  clean-input must not call tape.
- `post-turn-hook-uses-clean-input-not-inbound` — turn with
  inbound `"@gpt-4 hello"` and clean-input `"hello"` must tape
  `"hello"` only.

If existing `test/test-llm-backend.el::test-send-input-*` tests
rely on tape behavior, they should keep passing because the path
moves but the effect is identical. If any do break, the most
likely cause is double-recording — verify the dispatcher no
longer calls `record-message` for the user side.

### Acceptance

- Test count goes from 100 (after step 2) to 103. All 103 pass.
- `grep -n 'record-message "user"' superchat*.el` returns exactly
  one match (the new hook).
- Byte-compile clean (no new warnings).
- Manual smoke (note in report, you can't drive Emacs interactively):
  open a chat buffer, send a message, confirm
  `M-x superchat-db-tape-recent` (or whatever the existing inspector
  is — see `superchat-db.el`) shows the user message exactly once.

### Commit

`feat(core): migrate user-side tape recording to post-turn hook (Bub Phase 1 D)`

---

## Out of scope (do NOT touch)

- The implicit-match subsystem in `superchat-skills.el`.
- `superchat-memory.el` internals (v0.8 just shipped).
- MCP integration.
- The assistant-side tape write (see "Why not migrate the assistant
  side too" above).
- Splitting `post-turn-functions` into phases. Phase 2.
- Adding a hook registry, removing turn struct fields, or any other
  Phase 2 work.
- `superchat-dispatcher.el`'s overall structure — only delete the
  two `record-message` lines in step 4; leave everything else.

If you find what looks like dead code in `superchat.el`
(e.g. `superchat--build-final-prompt`, `--hook-file-context`,
`--hook-tape-replay`, `--make-inline-context`, `--hook-memory-inject`),
**verify with grep before deleting**:

```bash
# A function is dead iff:
#   (a) no other .el file calls it
#   (b) no test file calls it
#   (c) it is not declared autoload
grep -rn 'function-name' . --include='*.el'
```

If it's truly dead, deletion is fine, but put it in a SEPARATE
commit after step 4, not bundled into any step's commit. The prior
session hallucinated several rounds of "dead code cleanup" — keep
your deletions auditable.

---

## Validation checklist (run after EACH step)

```bash
# 1. Working tree
git status

# 2. Byte-compile clean vs baseline
rm -f superchat*.elc
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 \
  | grep -E "Warning|Error" > /tmp/bc-current.log
diff /tmp/v07d-bc-baseline.log /tmp/bc-current.log
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
  $([ -f test/test-post-turn-hooks.el ] && echo "-l test/test-post-turn-hooks.el") \
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED" | tail -10
```

Expected progression of test totals:

| After step | Total | Passing | Failing |
|------------|-------|---------|---------|
| Baseline   | 99    | 94      | 5       |
| Step 1     | 99    | 99      | 0       |
| Step 2     | 100   | 100     | 0       |
| Step 3     | 100   | 100     | 0       |
| Step 4     | 103   | 103     | 0       |

---

## Final report

Reply with:

1. Commit SHA list, one per step (4 commits minimum; +1 if you do
   an audited dead-code cleanup).
2. Diff in line counts for every `.el` file you touched.
3. Test totals: before/after for each step.
4. Byte-compile diff against the baseline you captured at the very
   beginning. Must be empty.
5. Resolutions for the four v0.7 open decisions (from the prior
   handoff, defaults are fine — just state which you used).
6. Any test you investigated and concluded was wrong, with the
   reasoning. **Do NOT silently weaken assertions.**
7. Anything you skipped, with reason.
8. The output of `grep -rn 'record-message "user"' --include='*.el' .`
   after step 4 — must show one match.

If any step reveals an unrecoverable problem (e.g. step 1's reds
turn out to require an architectural change), commit what's done,
mark the rest as deferred, and append a "Q&A for follow-up" section
to the bottom of this file with your specific blocking question.
