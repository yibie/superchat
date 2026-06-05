# Handoff: v0.7 — Skills v2 + Workflow restoration

**Audience**: implementer model (deepseek-v4-pro).
**Predecessors**:
- `docs/handoff-v0.9-split.md` (shipped)
- `docs/handoff-v0.9-hook-alignment.md` (shipped — A1)
- `docs/handoff-v0.8-sqlite-memory.md` (shipped)

**Goal**: ship two tracks under one milestone.

- **Track A**: unify `skills/*.md` and `SKILL.md` formats on the
  SKILL.md frontmatter convention; harden the loader against
  missing fields.
- **Track B**: restore workflow as a `type: workflow` sub-type of
  the unified SKILL.md format — multi-step linear recipes whose
  body executes line-by-line through the same pipeline as chat.
  Workflow was deleted by `31e4fd3` when skills landed; ROADMAP
  v0.7 commits to bringing it back, **without** collapsing it into
  skills (they solve different problems).

**Non-goals**:
- No new defcustoms beyond what's listed below.
- Do NOT touch the implicit-match subsystem
  (`superchat-skills--match`, `superchat-skills--try-implicit`,
  `superchat-skills-implicit-match-p`,
  `superchat-skills-match-confidence-threshold`). It must keep
  working for both `type: prompt` and `type: workflow` skills.
- No new dependencies.
- No reformatting of `examples/standard-skills/code-review/` beyond
  what the unification requires.
- Do NOT migrate the `superchat-skills-standard.el` API surface
  (`-import`, `-export`, `-list`, `-initialize`). Their semantics
  stay; only their internals tighten.

---

## State you inherit

```
HEAD:           9750811  docs(roadmap): v0.7 framing
superchat.el:                    1550 lines
superchat-skills.el:              733 lines  (includes implicit-match — don't touch)
superchat-skills-standard.el:     237 lines  (working v1; harden)
superchat-core.el:                139 lines  (hook pipeline owner)
superchat-prompt-hooks.el:        168 lines  (A1 hooks)
superchat-dispatcher.el:          369 lines
superchat-memory.el:              337 lines  (v0.8 SQLite facade)
tests:                            73/73 passing
```

Test runner (paste verbatim — works in batch):

```bash
emacs -batch -L . -L test -l ert \
  -l test/test-core-pipeline.el \
  -l test/test-llm-backend.el \
  -l test/test-memory-soul.el \
  -l test/test-memory-sqlite-facade.el \
  -l test/test-prompt-hooks.el \
  -l test/test-skills.el \
  -l test/test-skills-integration.el \
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED " | tail -10
```

Baseline before you start: capture the line, e.g.
`Ran 73 tests, 73 results as expected, 0 unexpected`.
After Step 6 of this handoff: expect ≥ 95 tests, 0 unexpected.

---

## What's already in place

### `superchat-skills-standard.el` (do not rewrite, only tighten)

| Function                              | Purpose                                          | Tighten? |
|---------------------------------------|--------------------------------------------------|----------|
| `--find-skills`                       | scan a directory for `SKILL.md` containers       | no       |
| `--load-metadata`                     | parse YAML frontmatter (`name`, `description`, `triggers`) | **YES** |
| `--extract-content`                   | strip frontmatter + HTML comments                | minor    |
| `--load-references`                   | read `references/*.md` siblings                  | no       |
| `--convert`                           | standard-skill → internal plist                  | **YES** |
| `--discover-all`                      | merge all directories                            | no       |
| `-merge-registry`                     | union with implicit-match registry               | no       |
| `-export`                             | internal → SKILL.md (write to disk)              | **YES** |
| `-import`                             | interactive: copy a standard skill into user dir | no       |
| `-list`                               | M-x list standard skills                         | no       |
| `-initialize`                         | hook into superchat skill discovery              | no       |

Specifically `--load-metadata` returns `:name` defaulted to the
directory's basename when `name:` is missing — that is silent and
wrong. `--convert` does not preserve a `type:` field. Both will be
fixed in Step 2.

### `skills/*.md` (3 files: code-review, planning, refactor)

No frontmatter. Body is the system prompt directly. Trigger derives
from filename. These need to grow a SKILL.md-style frontmatter
header so the loader has one path for everything.

### `examples/standard-skills/code-review/`

Already in canonical SKILL.md form. Use as the golden round-trip
fixture.

### Git history to mine for Track B (read-only)

```
6f32427  Oct 2025  major workflow refactor with dynamic timeout system
                   (last known `superchat-workflow.el`, 458 lines)
d228c72  Oct 2025  linear recipes reuse gptel tools
                   (introduced the >workflow-name trigger + .workflow format)
31e4fd3            deleted superchat-workflow.el when skills landed
```

Recover the old file with:
```bash
git show 6f32427:superchat-workflow.el > /tmp/workflow-prior-art.el
```
**This is reference material**, not a copy-paste source. The old file
used gptel and the old executor. You will write a smaller, modern
version that calls `superchat-core-run-turn` per step (see Step 4).

---

## Canonical SKILL.md format (this milestone defines it)

```markdown
---
name: code-review
description: 专业代码审查，关注正确性/可读性/性能/安全/风格/可维护性。
version: "1.0"
type: prompt           # default; may be omitted
triggers: ["审查", "code review", "review my code"]
---

# Skill body (becomes the system prompt for type=prompt)
你是一个专业的代码审查助手。...
```

For `type: workflow`:

```markdown
---
name: ai-news-summary
description: 每周技术新闻摘要
version: "1.0"
type: workflow
---

# Each non-empty body line is one step.
# Comments (#) and blank lines are skipped.
# $input expands to the user-supplied argument from `>name argument`.
# Variables: $input $lang $date

/web-search "$input" 最新新闻
@qwen3-coder:30b 给上述结果做 3 个角度的中文摘要（商业/技术/社会）
将分析结果保存到 #~/Documents/news-summary.md
```

**Frontmatter rules** (these are the contract):

| Field         | Required? | Default                          | Validation                     |
|---------------|-----------|----------------------------------|--------------------------------|
| `name`        | yes       | (no default — refuse to load)    | non-empty string               |
| `description` | yes       | (no default — refuse to load)    | non-empty string               |
| `version`     | no        | `"1.0"`                          | string                         |
| `type`        | no        | `"prompt"`                       | one of `"prompt"`, `"workflow"`|
| `triggers`    | no        | `'()`                            | list of strings                |

"Refuse to load" = `--load-metadata` returns nil, the skill is skipped
with a `warn`-level message. **No nil-deref crash**, no silent default
that masks malformed input.

---

## Execution plan (6 steps, one commit each)

### Step 1 — Frontmatter validator + corpus migration

**Files**: `superchat-skills-standard.el`, `skills/*.md`.

1. Add `superchat-skills-standard--parse-frontmatter` as a new pure
   function. Input: full file content string. Output: alist of
   `(key . value)` pairs, or `nil` if no frontmatter block.
   Trim values, support quoted strings, support
   `key: ["a", "b"]` list shorthand for `triggers`.

2. Add `superchat-skills-standard--validate` taking the alist.
   Returns `(t . plist)` on success or `(nil . error-string)` on
   failure. Required: `name`, `description`. Type must be one of
   `"prompt"` / `"workflow"` (case-insensitive). Default `type` to
   `"prompt"`, `version` to `"1.0"`.

3. Refactor `--load-metadata` to use the two new helpers. On
   validation failure: `(display-warning 'superchat-skills (format
   "Skipping %s: %s" skill-dir error-string) :warning)` and return
   `nil`. `--find-skills`+`--discover-all` must skip nil entries.

4. Add frontmatter to `skills/code-review.md`,
   `skills/planning.md`, `skills/refactor.md`. The frontmatter goes
   *above* the existing body; body content stays unchanged. Use:

```yaml
---
name: code-review
description: 专业的代码审查助手
version: "1.0"
type: prompt
---
```

   (Adjust `name` and `description` per file. Keep current names —
   `code-review` not `code_review`.)

5. The existing `superchat-skills--find-file` / `--load` already
   read these files as raw bodies; make them call the new
   frontmatter parser to strip the header before returning the body
   as the prompt. Skill content stays compatible (body unchanged
   after migration), so other call sites don't see a difference.

**Test additions** (`test/test-skills.el`):

- `skill-frontmatter-parses-required-fields`
- `skill-frontmatter-rejects-missing-name`
- `skill-frontmatter-rejects-missing-description`
- `skill-frontmatter-rejects-bad-type`
- `skill-frontmatter-defaults-version-and-type`
- `skill-frontmatter-parses-triggers-list`
- `skill-corpus-code-review-loads-cleanly`
- `skill-corpus-planning-loads-cleanly`
- `skill-corpus-refactor-loads-cleanly`

Acceptance: tests green, `M-x list-superchat-skills`-equivalent (or
its functional equivalent) shows all three in-repo skills plus any
under standard directories.

Commit: `feat(skills): unify SKILL.md frontmatter across in-repo and standard skills`.

### Step 2 — Round-trip fidelity

**Files**: `superchat-skills-standard.el`,
`test/test-skills-roundtrip.el` (new).

`-export` currently writes a minimal SKILL.md but doesn't preserve
`type:`, `triggers:`, or `version:`. Fix it so that:

```
SKILL.md → --load-metadata → plist → -export → SKILL.md′

(string= (with-temp-buffer (insert-file-contents original) (buffer-string))
         (with-temp-buffer (insert-file-contents reexported) (buffer-string)))
```

returns `t` for the `examples/standard-skills/code-review/` fixture
and any new fixtures you add.

**Test additions** (`test/test-skills-roundtrip.el`):

- `roundtrip/code-review-byte-identical`
- `roundtrip/preserves-type-workflow`
- `roundtrip/preserves-triggers-list`
- `roundtrip/preserves-references-dir-link`
- `roundtrip/preserves-version`

If byte-identical is impossible due to YAML key ordering, lock the
field ordering: `name`, `description`, `version`, `type`, `triggers`.
Document the chosen ordering in `superchat-skills-standard.el`'s
header comment.

Commit: `test(skills): SKILL.md round-trip byte-identical fixtures`.

### Step 3 — Workflow file format + parser

**Files**: NEW `superchat-workflow.el`.

```elisp
;;; superchat-workflow.el --- Multi-step linear recipes -*- lexical-binding: t; -*-

;;; Commentary:
;; v0.7 restores workflow as a SKILL.md sub-type (type: workflow).
;; Each non-empty, non-comment line in the skill body is one step.
;; Steps execute top-to-bottom through superchat-core-run-turn —
;; the same pipeline as chat, including all registered hooks.
;;
;; Variable substitution: $input (the trigger argument), $lang,
;; $date. Substitution happens once, before parsing into steps.
```

Public API (this is the contract):

```elisp
(superchat-workflow-parse-steps STRING) → list-of-strings
;; Splits STRING into a list of step strings.  Skips blank lines and
;; comment lines (lines whose first non-whitespace char is `#`).
;; Preserves $input / $lang / $date for late binding.

(superchat-workflow-substitute STRING VARS) → string
;; VARS is a plist (:input "foo" :lang "English" :date "2026-06-04").
;; Returns STRING with variables expanded.

(superchat-workflow-execute SKILL-PLIST ARGUMENT &optional ON-DONE)
;; SKILL-PLIST has :name :body :type (= "workflow").  ARGUMENT is
;; the user-supplied text after `>name`.  ON-DONE is invoked with
;; the final step's result when execution finishes.
;;
;; Each step builds a fresh `superchat-turn-new STEP-STRING`, calls
;; `superchat-core-run-turn`, then hands the populated turn's prompt
;; to `superchat--llm-generate-answer` exactly the way the chat
;; dispatcher does.  Steps run sequentially; later steps see earlier
;; steps' results as conversation history via the normal hook.
```

No condition-case wraps swallowing step failures. If step N errors,
later steps don't run; the user sees the error in the chat buffer.
The old `superchat-workflow.el` had a custom `StepResult` protocol;
**you don't need it** — the hook pipeline already returns a turn
struct with `error` slot for the same purpose.

**Test additions** (`test/test-workflow.el`, new):

- `workflow-parse-skips-blank-and-comments`
- `workflow-parse-preserves-step-order`
- `workflow-substitute-expands-input-lang-date`
- `workflow-substitute-leaves-unknown-vars-alone`
- `workflow-execute-runs-steps-sequentially` (mock the LLM
  executor with `cl-letf`)
- `workflow-execute-stops-on-first-error`
- `workflow-execute-passes-context-between-steps`

Commit: `feat(workflow): restore step-by-step execution as SKILL.md type`.

### Step 4 — Wire `type: workflow` into the trigger path

**Files**: `superchat-skills.el`,
`superchat-dispatcher.el` (only the place that calls
`superchat-skills-load`).

Today `>name` resolves a skill name and the loaded body becomes the
system prompt. After this step:

1. `superchat-skills-load` returns the full plist
   `(:name X :description Y :type T :body B :version V :triggers TS)`,
   not just the body string. Update callers.

2. In the dispatcher, after resolving the skill:
   ```elisp
   (if (string= (plist-get skill :type) "workflow")
       (superchat-workflow-execute skill argument)
     ;; existing single-prompt path
     ...)
   ```

3. The single-prompt path is unchanged — body string still goes into
   the system prompt slot of the next turn.

**Test additions** (`test/test-skills-integration.el`):

- `dispatch/skill-name-with-type-prompt-uses-single-call`
- `dispatch/skill-name-with-type-workflow-invokes-executor`
- `dispatch/missing-type-defaults-to-prompt`

Acceptance: an interactive smoke (note in final report, you can't
verify it yourself) — `>code-review some-snippet` works as before;
a new `>test-workflow some-arg` (you'll add a fixture skill) runs
multi-step.

Commit: `feat(dispatch): route SKILL.md type=workflow to step executor`.

### Step 5 — Legacy `.workflow` import shim

**Files**: `superchat-workflow.el`, new fixture skill.

Old workflow files lived in `~/.emacs.d/superchat/workflow/` as
`name.workflow` plain text. v0.7 doesn't require users to migrate
manually:

1. Add `superchat-workflow-import-legacy-dir DIR` — scans `DIR` for
   `*.workflow`, synthesises a SKILL.md frontmatter, and either:
   - **Option A** (preferred): writes a new SKILL.md container
     under the standard skills directory, leaving the original
     `.workflow` file untouched.
   - **Option B** (fallback if writing fails): synthesises an
     in-memory skill plist at load time without touching disk.

   Pick A unless writing fails — then warn and fall back to B.
   Document the choice in the function docstring.

2. Synthesised frontmatter:
   ```yaml
   ---
   name: <filename-without-extension>
   description: "Imported from legacy workflow file <path>"
   version: "1.0"
   type: workflow
   ---
   ```
   followed by the original file's body verbatim.

3. Add an `M-x superchat-workflow-import-legacy` interactive command
   that prompts for the directory (defaulting to
   `~/.emacs.d/superchat/workflow/`).

**Test addition**:

- `workflow-import-legacy-synthesises-skill` — create a temp dir
  with a fake `.workflow` file, run import, assert a SKILL.md
  appears with correct frontmatter and the body preserved.

Add one fixture: `examples/standard-skills/test-workflow/SKILL.md`
with `type: workflow` and 2-3 trivial steps (e.g. `echo hello`,
`echo $input` — avoid steps that hit the network so the file is
self-contained as a docs example).

Commit: `feat(workflow): legacy .workflow file import shim`.

### Step 6 — Documentation + ROADMAP

- `README.md` / `README_cn.md`: section on the unified SKILL.md
  format and the `type: workflow` extension. One example each.
- `docs/SKILLS_QUICKSTART.md`: update to reflect the canonical
  frontmatter form. Add a workflow quickstart subsection.
- `docs/SKILL_FORMAT_CONVERSION.md`: update with the new field
  ordering (`name`, `description`, `version`, `type`, `triggers`).
- `ROADMAP.md`: mark v0.7 SHIPPED, list commit SHAs, fill in the
  resolutions to the four open questions in the existing v0.7
  section.

Commit: `docs(skills): v0.7 SKILL.md format + workflow restoration shipped`.

---

## Validation checklist (run after EACH step)

```bash
# byte-compile must produce no NEW warnings vs baseline
rm -f superchat*.elc
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 | grep -E "Warning|Error" | tee /tmp/v07-bytecompile.log
# diff against the pre-handoff baseline you captured at session start

# tests
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
  -f ert-run-tests-batch-and-exit 2>&1 | grep -E "^Ran|FAILED" | tail -10
```

Pre-handoff baseline: `Ran 73 tests, 73 results as expected, 0 unexpected`.

Post-Step-6 target: `Ran 95+ tests, all expected, 0 unexpected`.

If the implicit-match subsystem regresses (any
`superchat-skills--match*` test or
`superchat-skills--try-implicit*` test starts failing): **STOP**.
You broke a system that's explicitly off-limits in this handoff.
Revert the offending step and report.

---

## Decisions to resolve (state your choice in final report)

The ROADMAP v0.7 section lists four open questions. You're expected
to make a call on each and document it:

1. **Per-step tool confirmation**: does each workflow step re-prompt
   for tool confirmation, or run with the user-confirmed list?
   *Default*: run with the confirmed list, no per-step re-prompt.
   Steps are a single user intent.
2. **Step-by-step progress in chat**: how visible is each step?
   *Default*: each step's output renders into the buffer with the
   streaming-pending face from v0.6, separated by a thin divider.
3. **Variable substitution scope**: `$input $lang $date` per the
   handoff. Anything else?
   *Default*: just those three. Anything more is YAGNI for v0.7.
4. **`version: "1.0"` enforcement**: required field, optional with
   default, or fully optional?
   *Default*: optional, default `"1.0"`. Don't reject files missing it.

If you disagree with any default, document why before changing it.

---

## Out of scope (do NOT touch)

- The implicit-match subsystem (lines ~251-377 of
  `superchat-skills.el`). It must continue to work for both
  `type: prompt` and `type: workflow` skills, but you don't
  change it.
- `superchat-core.el`'s hook contract.
- `superchat-memory.el` (v0.8 just shipped — leave alone).
- MCP integration (`superchat-mcp.el`) — v0.8.5.
- A2 (tape post-turn) — v1.x.
- The 6 untracked `examples/standard-skills/` rearrangements — do
  the minimum needed for Steps 4-5 fixtures and stop.
- Skill name with hyphens vs underscores debate — **hyphens win**
  (matches existing in-repo files). Don't bikeshed.

---

## Final report

Reply with:

1. Commit SHA list, one per step (6 commits).
2. Diff in line counts: `skills/*.md` (before/after), the two
   `superchat-skills*.el` files, the new `superchat-workflow.el`.
3. Test count: before/after. Both must be 0 unexpected.
4. New files created (with line counts).
5. Resolutions to the four open decisions, with one-sentence
   justifications each.
6. Anything you skipped, with reason.
7. Byte-compile diff against baseline — must be empty (no new
   warnings).

If you stop mid-handoff (e.g. step 4 reveals a deeper problem),
commit what's done, mark the rest as deferred, and write a Q&A
section at the bottom of this file with the open question.
