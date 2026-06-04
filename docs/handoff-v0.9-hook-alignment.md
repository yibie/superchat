# Handoff: hook pipeline alignment (v0.9 finisher, A1)

**Audience**: implementer model (deepseek-v4-pro).
**Predecessor**: `docs/handoff-v0.9-split.md` (already executed —
commits `a17f67e`, `a8b7cd1`, `fe363cb`, `2336396`, `e6f3198`).

**Goal**: make `superchat-core.el`'s hook pipeline the **only** path that
builds a turn's prompt. Today `superchat-send-input` calls
`superchat-core-run-turn` *and* `superchat--build-final-prompt` in
parallel, with `--build-final-prompt` doing 5 distinct things in one
180-line function. After this change, `superchat-core-run-turn` is
authoritative: dispatcher just builds a `superchat-turn`, runs it through
core, then hands `turn.prompt` + `turn.tools` to the LLM layer.

**Non-goals**:
- No new user-visible behavior.
- No new defcustoms, no renamed public symbols, no new dependencies.
- Do not refactor `superchat-llm.el`, `superchat-memory.el`, or
  `superchat-skills.el`. Their entry points stay exactly as today.
- Do not invent new hook variables. The four already in `superchat-core.el`
  are the contract:
  - `superchat-system-prompt-functions`
  - `superchat-build-prompt-functions`
  - `superchat-post-turn-functions`
  - `superchat-command-hooks`

---

## Current state you inherit

```
superchat-core.el        135 lines  (turn struct + hook chain + empty hook vars)
superchat-dispatcher.el  436 lines  (the bloat — fix this)
superchat-llm.el         173 lines  (untouched)
superchat-render.el      249 lines  (untouched)
superchat.el            1549 lines  (untouched, except possibly the require list)
```

Tests: `36/36 passing` (baseline). After this handoff you must end at
the same or higher count, with no test deletions, no test mocking
expectations weakened, and no `skip-unless` added.

Run tests:
```bash
emacs -batch -L . -L test -l ert \
      -l test/test-llm-backend.el \
      -l test/test-memory-soul.el \
      -f ert-run-tests-batch-and-exit 2>&1 | tail -20
```

(There may be additional test files under `test/`; discover them with
`ls test/*.el` and load each one. The 36 figure aggregates all of them.)

## The five concerns inside `superchat--build-final-prompt`

Read `superchat-dispatcher.el:126-203` end-to-end before changing
anything. The function bundles:

| # | Concern                                  | Lines (approx) | Belongs on hook                          |
|---|-------------------------------------------|----------------|------------------------------------------|
| 1 | Language instruction (`Your response must be in X`) | 133-137 | `superchat-system-prompt-functions` |
| 2 | Memory context formatting (reads `turn.retrieved-memories`) | 138-141 | `superchat-build-prompt-functions` |
| 3 | File-ref extraction + inlining (`#file.org`) | 143-176 | `superchat-build-prompt-functions` |
| 4 | Template variable substitution (`$input`, `$lang`) | 177-190 | `superchat-build-prompt-functions` |
| 5 | Conversation history context | 191       | `superchat-build-prompt-functions` |

The "assemble final string" step (lines 192-197 — `delq nil` + `mapconcat`)
becomes the implicit result of the hook chain: each hook writes into
`turn.system-prompt` or `turn.prompt`, and the dispatcher reads the final
values off the turn.

## Required `superchat-turn` slot semantics

Define these as the contract every hook honours. Currently the turn
struct already has these slots — you are giving them precise meaning:

- `system-prompt` (string): assembled by `system-prompt-functions`.
  Empty string is allowed. No trailing newline.
- `prompt` (string): the user-facing prompt that will be sent to the
  model, **excluding** the system prompt (the LLM layer composes them).
  Built by `build-prompt-functions`. Hooks may **append** to it but
  should not destructively overwrite a non-empty value unless they
  explicitly intend to (document it in their docstring).
- `retrieved-memories` (list): populated before hooks run, by the
  dispatcher, via `superchat-memory-retrieve` (whatever it's called
  today — keep the call site). Hooks consume it; nothing else writes it.
- `context-files` (list): set by `superchat-core--parse-input` already.
  Hooks may read.
- `tools` (list): NOT touched by hooks in this iteration. The dispatcher
  still calls `superchat--collect-llm-tools` after `core-run-turn`
  returns. (Moving tool collection onto a hook is a v1.x concern.)

## Concrete migration plan (one commit per step)

### Step 1 — Add the hook functions (additive, no behavior change yet)

Create file `superchat-prompt-hooks.el` (new). It owns the five hook
functions extracted from `--build-final-prompt`. Each is a small,
single-purpose function with the signature `(turn) → modified-turn`.

Hook function names (use these exactly):

- `superchat-prompt-hook--language-instruction` (concern 1)
- `superchat-prompt-hook--memory-context` (concern 2)
- `superchat-prompt-hook--file-inline` (concern 3)
- `superchat-prompt-hook--template-substitution` (concern 4)
- `superchat-prompt-hook--conversation-history` (concern 5)

Each function:

1. Receives `turn` (a `superchat-turn` struct).
2. Reads only `turn`'s slots and global config (`superchat-lang`,
   `superchat-inline-file-content`, etc.).
3. Modifies one slot via `setf`.
4. Returns the modified `turn`.

If a hook has nothing to contribute (e.g. memory list is empty),
return `turn` unchanged. **Never return nil** — that's reserved for the
hook chain "skip me" signal and would silently discard prior work.

Order of registration (in `superchat-prompt-hooks.el` at load time):

```elisp
(add-hook 'superchat-system-prompt-functions
          #'superchat-prompt-hook--language-instruction)

;; build-prompt order matters — later hooks see earlier hooks' work
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--file-inline)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--template-substitution)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--memory-context)
(add-hook 'superchat-build-prompt-functions
          #'superchat-prompt-hook--conversation-history)
```

**Note on order**: `template-substitution` must run *after* `file-inline`
because the latter mutates `turn.clean-input` (replacing the file ref
with file content). `memory-context` and `conversation-history` are
prepends that don't depend on input mutation; they go last so they
wrap the assembled prompt.

After this commit:
- `superchat-prompt-hooks.el` exists (~150 lines).
- `superchat.el` requires it.
- `superchat--build-final-prompt` is **still called** and **still does
  the same work**. Hooks run in parallel but their output is discarded.
- Tests: 36/36 pass.

This step is purely defensive: it lets you write tests for the hooks
in isolation before flipping the dispatcher over.

### Step 2 — Test the hook functions in isolation

New file: `test/test-prompt-hooks.el`. Minimum 12 ERT cases:

| Test                                        | Asserts                                            |
|---------------------------------------------|----------------------------------------------------|
| `language-instruction/english-noop`         | turn unchanged when lang is English                |
| `language-instruction/chinese-appends`      | `turn.system-prompt` contains "Chinese"            |
| `memory-context/empty-list-noop`            | `turn.prompt` unchanged                            |
| `memory-context/populated-prepends`         | `turn.prompt` starts with formatted memories       |
| `file-inline/no-fileref-noop`               | turn unchanged                                     |
| `file-inline/valid-textual-file`            | `turn.prompt` contains file content                |
| `file-inline/missing-file-warns-keeps-going`| no error, turn returned, warning logged            |
| `file-inline/binary-file-skipped`           | file content NOT inlined                           |
| `template-substitution/dollar-input`        | `$input` replaced verbatim                         |
| `template-substitution/dollar-lang`         | `$lang` replaced                                   |
| `template-substitution/no-vars`             | input appended after template                      |
| `conversation-history/limit-honored`        | `superchat-context-message-count=2` → 2 messages   |

Each test constructs a `superchat-turn` via `superchat-turn-new`,
pre-fills relevant slots, calls the hook directly (not the chain),
and asserts on the returned turn's slots. No buffer setup, no LLM,
no async.

Use `cl-letf` to stub `superchat--conversation-context-string`,
`superchat--read-inline-file-content`, and any other I/O so tests
are hermetic.

After this commit: 36 → 48 passing.

### Step 3 — Add one pipeline-integration test (also passes today)

New file: `test/test-core-pipeline.el`. At least 4 cases:

| Test                                  | Asserts                                                    |
|---------------------------------------|------------------------------------------------------------|
| `pipeline/empty-input-no-crash`       | `superchat-core-run-turn` returns a turn for `""`          |
| `pipeline/parses-model-switch`        | `@gpt-4 hello` → `turn.target-model = "gpt-4"`             |
| `pipeline/all-hooks-chain`            | with all 5 hooks registered, `turn.prompt` contains all 5 contributions |
| `pipeline/hook-erroring-is-isolated`  | a hook that `(error "boom")` does not abort the chain      |

The last one matters: `superchat-core--run-hook-chain`
(superchat-core.el:73) has a `condition-case nil ... (error nil)`
wrapper. Confirm it still swallows errors and skips the bad hook.
If that test fails, the wrapper is broken and you must fix it
before proceeding to step 4.

After this commit: 48 → 52 passing.

### Step 4 — Flip the dispatcher (the real change)

This is the only step with behavioral risk. **Make it one commit.**
If it breaks tests, revert this commit only — steps 1-3 are
independently valuable.

Changes:

1. In `superchat-send-input` (currently `superchat-dispatcher.el:286`),
   after the existing input parsing but before any prompt assembly:
   - Construct `(setq turn (superchat-turn-new input session-id))`.
   - Populate `turn.retrieved-memories` via existing memory-retrieval
     call (keep the exact call site that's there today).
   - Populate `turn.context-files` is already done by
     `superchat-core--parse-input`.
   - Call `(setq turn (superchat-core-run-turn turn))`.
   - Read `(superchat-turn-prompt turn)` and
     `(superchat-turn-system-prompt turn)` for the LLM call.

2. Delete `superchat--build-final-prompt` and
   `superchat--execute-llm-query`. Their work is now done by the
   hooks + core.

3. Inline the LLM call into `superchat-send-input` (or a small new
   helper `superchat-dispatcher--send-turn`). It receives `turn` and
   calls `superchat--llm-generate-answer` with the assembled prompt.

4. Audit every call site of `superchat--build-final-prompt`:
   ```
   grep -rn "superchat--build-final-prompt\|superchat--execute-llm-query" \
        --include="*.el"
   ```
   If `superchat-skills.el` calls them, that call must be migrated to
   construct a turn and run it through core too. **If skills relies on
   the plist return shape `(:prompt ... :user-message ...)`, do not
   change the skill API** — keep a thin internal helper that mimics
   the old return shape from a populated turn. The point is to delete
   the duplicate logic, not to rewrite skills.

Acceptance for step 4:
- `superchat-dispatcher.el` drops from 436 to ≤ 220 lines.
- `wc -l superchat-core.el` may rise slightly (~20 lines) if you add
  helper accessors; that's fine.
- All 52 tests still pass. No new `(skip-unless ...)`. No
  `should-error` that didn't exist before.
- Manual smoke (the implementer can't do this, just note it in the
  final report): `M-x superchat`, type "hello", hit RET, see a normal
  response.

### Step 5 — Cleanup commit

- Remove the now-dead helpers in `superchat-dispatcher.el`.
- Update `ROADMAP.md` v0.9 section: mark hook-alignment as shipped,
  list the commit SHAs.
- Update `superchat-core.el:116`'s docstring (currently says "Request-
  time decisions such as tool collection and backend invocation stay
  in the dispatcher" — true for tools, no longer true for prompt
  assembly; rephrase precisely).
- Update `AGENTS.md` quirks list if there's a relevant entry about
  dispatcher-vs-core duality.

## Validation checklist (run after EACH step)

```bash
# tests
emacs -batch -L . -L test -l ert \
      $(ls test/*.el | sed 's/^/-l /') \
      -f ert-run-tests-batch-and-exit 2>&1 | tail -10

# byte-compile (no new warnings vs baseline)
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 | grep -E "Warning|Error"
```

Baseline byte-compile output: capture it on the current HEAD before
you start (`git stash` any work, recompile, save the output to
`/tmp/baseline-warnings.txt`). Diff against your output after each step.

## What to do when something surprises you

If at any step you discover:

- A test that mocks `superchat--build-final-prompt` directly →
  rewrite the test to mock the hook function instead. Document this
  in the commit message.
- A skill or external caller depending on `:user-message` plist key →
  preserve that key in a compatibility helper, do NOT change the
  contract.
- A hook order that produces a different prompt than the old code →
  STOP. The hook order is the bug. Re-read the migration plan's
  "Order of registration" section. If after rereading the order is
  truly wrong for some case, document the case in
  `docs/handoff-v0.9-hook-alignment.md` (append a Q&A section at the
  bottom) and ask the human reviewer.
- The hook chain swallowing an error that was previously surfaced to
  the user → the wrapper in `superchat-core--run-hook-chain`
  (`(condition-case nil ... (error nil))`) is too aggressive for this
  use. Tighten it to `(condition-case err ... (error (message
  "superchat-core: hook %S failed: %s" fn err) current))` so failures
  are observable. This is the one place you're allowed to modify
  `superchat-core.el` beyond docstrings.

## Out of scope

- Tools collection moving onto a hook — v1.x.
- Append-only tape / replaying turns from records — v1.x (A2).
- Adapter abstraction (stdio, telegram) — v1.x.
- Touching `superchat-memory.el`, `superchat-skills.el`,
  `superchat-tools.el`, `superchat-executor.el`, `superchat-db.el`,
  `superchat-models.el`, `superchat-render.el`, `superchat-mcp.el`,
  `superchat-save.el`, `superchat-llm.el`. The only files you create
  or modify are:
  - NEW: `superchat-prompt-hooks.el`
  - NEW: `test/test-prompt-hooks.el`
  - NEW: `test/test-core-pipeline.el`
  - MODIFIED: `superchat-dispatcher.el` (mostly deletions)
  - MODIFIED: `superchat.el` (one new require)
  - MODIFIED: `superchat-core.el` (docstring fix, possibly one wrapper tighten)
  - MODIFIED: `ROADMAP.md`, possibly `AGENTS.md`

## Final report

Reply with:

1. Commit SHA list (one per step).
2. `wc -l superchat*.el` before/after.
3. Test count before/after (expect: 36 → 52+).
4. Byte-compile diff vs baseline (expect: zero new warnings).
5. Anything you skipped, with reason.
6. Any Q&A appended to the bottom of this handoff file.
