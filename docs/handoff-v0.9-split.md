# Handoff: superchat.el monolith split (v0.9 prep)

**Audience**: implementer model (deepseek-v4-pro).
**Goal**: split `superchat.el` (2589 lines) into 6 focused files so the
`superchat-core.el` hook pipeline is actually the only turn orchestrator.
After this refactor, superchat's shape matches a minimal hook-first agent
runtime (think `bub`): tiny `core` + replaceable builtins + UI as an adapter.

**Non-goal**: no behavior changes. No new features. No API renames visible
to users. This is pure refactor. If a change requires touching public
`defcustom` names, `interactive` commands, or `;;;###autoload` symbols —
stop and flag it.

---

## Repo state you inherit

- Branch: `main`. Working tree has unrelated dirty files
  (`.claude/`, `.omo/`, `.pi/`, `:memory:`, `superchat.elc`) — **do not
  touch or commit them**. Only commit files you intentionally modify.
- Emacs min version: 28.1.
- LLM dependency: `(llm "0.7")` (GNU ELPA). Project already migrated off
  `gptel`.
- Test entrypoint: `test/test-llm-backend.el` (29 tests, 28 pass, 1
  pre-existing abort at line ~147 — leave it alone, not your job).
- Run tests: `emacs -batch -L . -L test -l ert -l test/test-llm-backend.el -f ert-run-tests-batch-and-exit`
- Line counts before you start (verify with `wc -l *.el`):
  - `superchat.el` 2589
  - `superchat-core.el` 135 (pipeline scaffold — already the bub-shaped core)
  - `superchat-executor.el` 227
  - `superchat-memory.el` 337
  - `superchat-skills.el` 733
  - `superchat-tools.el` 545
  - `superchat-db.el` 425

## Conventions you must follow

- `lexical-binding: t` header on every new file.
- Private symbols: `superchat--foo`. Public: `superchat-foo`. Do not change
  any existing symbol's visibility.
- Each new file ends with `(provide 'superchat-FOO)` and a final
  `;;; superchat-FOO.el ends here`.
- `superchat.el` must `(require 'superchat-FOO)` for each new file, in the
  order the existing requires sit (currently near the top of `superchat.el`).
- No new `defcustom`. No defaults changes. No docstring rewrites unless
  the original docstring is being moved verbatim.
- Do NOT add backwards-compat shims, aliases, or deprecation wrappers.
  Internal callers update in lockstep.
- Do NOT introduce any new dependency. Especially: no `dash`, no `s`.
- Commit style: follow the repo's existing convention (see `git log`).
  Conventional-commits-ish: `refactor(split): extract superchat-models.el`.
  Each step = one commit. Include the Claude co-author trailer the repo
  already uses if you see it in `git log`; otherwise omit it.

## The 6 splits (do in this order — each step is one commit)

For every step:
1. Move the listed functions/defvars/defcustoms verbatim into the new file.
2. Add necessary `(require ...)` to the new file (only what it actually uses).
3. Add `(require 'superchat-NEW)` to `superchat.el`.
4. Run the test suite. **Must stay at 28/29 passing.** No new failures.
5. Commit.

**Do not collapse steps.** Each commit must be reversible on its own.

### Step 1 — `superchat-models.el` (~180 lines, safest)

Move these from `superchat.el`:

- `superchat--model-list-cache` (line 267)
- `superchat--model-list-cache-ttl` (272)
- `superchat--invalidate-model-cache` (276)
- `superchat-manual-models` defcustom (918)
- `superchat--parse-model-switch` (913)
- `superchat--get-ollama-models` (928)
- `superchat-sync-ollama-models` (942)
- `superchat--get-available-models` (961)
- `superchat-refresh-models` (1005)
- `superchat-model-list` (1016)
- `superchat--is-ollama-backend-p` (447)

Check: `grep -n "superchat--model-list-cache\|superchat--parse-model-switch\|superchat--get-available-models\|superchat--get-ollama-models\|superchat-sync-ollama-models\|superchat-refresh-models\|superchat-model-list\|superchat--is-ollama-backend-p\|superchat-manual-models" superchat*.el` to confirm all call sites still resolve.

Acceptance: tests pass; `M-x superchat-model-list` and `M-x superchat-refresh-models` still work interactively (the implementer can't verify this — just confirm `;;;###autoload` cookies are preserved if they existed).

### Step 2 — `superchat-save.el` (~60 lines)

Move:

- `superchat--format-conversation` (2171)
- `superchat--save-as-new-file` (2176)
- `superchat--save-append-to-node` (2192)
- `superchat--save-as-subnode` (2203)
- `superchat--save-conversation` (2218)
- `superchat-default-save-method` defcustom (85)

Note: `superchat-default-save-method` is a defcustom but lives near the
top with other config. Moving it to `superchat-save.el` is fine because
nothing else references it. **Verify with grep first**:
`grep -rn "superchat-default-save-method" .`

If anything outside the save flow touches it, leave the defcustom in
`superchat.el` and only move the functions.

### Step 3 — `superchat-mcp.el` (~100 lines)

Move:

- `superchat-mcp-available-p` (1752)
- `superchat-mcp-servers-running-p` (1758)
- `superchat-mcp-get-server-count` (1765)
- `superchat-mcp-get-running-server-count` (1771)
- `superchat-mcp-start-servers` (1777)
- `superchat-mcp-get-tools` (1791)
- `superchat-mcp-status` (1798)
- The `(defvar mcp-hub-servers)` and `(defvar mcp-server-connections)`
  forward-declarations at lines 68-69.

Step 2 and Step 3 can be done in parallel branches if convenient, but
commit them sequentially on `main`.

### Step 4 — `superchat-render.el` (~190 lines)

Move:

- `superchat--md-to-org` (710)
- `superchat--insert-prompt` (787)
- `superchat--prepare-for-response` (802)
- `superchat--update-status` (810)
- `superchat--ttft-log` macro (819)
- `superchat--prepare-assistant-response-area` (827)
- `superchat--annotate-ttft` (839)
- `superchat--stream-llm-result` (859)
- `superchat--process-llm-result` (874)
- `superchat--insert-system-message` (2314)
- `superchat--refresh-prompt` (2324)

**Risk**: these touch buffer-local state (`superchat--current-turn`,
`superchat-buffer-name`, point/mark, faces). The new file will need to
`(require 'superchat)` OR `superchat.el` keeps the buffer-local
`defvar`s and `superchat-render.el` only declares them with
`(defvar superchat--current-turn)` forward-declarations.

**Prefer the second option** — forward-declare in render.el, keep state
ownership in `superchat.el`. This avoids a require cycle.

Faces used (search and confirm they remain defined in `superchat.el`):
- `superchat-streaming-pending` (recent commit `ccd8f1c` added this)
- Any other face referenced by `--stream-llm-result` /
  `--process-llm-result`.

### Step 5 — `superchat-llm.el` (~320 lines)

Move:

- `superchat--provider-name` (1851) — `cl-defgeneric` + `cl-defmethod`
- `superchat--provider-chat-model` (1869)
- `superchat--provider-with-chat-model` (1882)
- `superchat--effective-llm-backend` (1902)
- `superchat--should-attach-tools-p` (1919)
- `superchat--collect-llm-tools` (1936)
- `superchat--llm-extract-text` (1949)
- `superchat--build-llm-prompt` (1959)
- `superchat--llm-generate-answer-sync` (1976)
- `superchat--llm-generate-answer` (1995)

**Risk**: `superchat--llm-generate-answer` is the longest function
(~175 lines) and touches callbacks, streaming, error recovery, tool
results, and the buffer. Read it end-to-end before moving. It almost
certainly calls render.el functions; that's fine, `superchat-llm.el`
should `(require 'superchat-render)`.

Defcustoms it depends on (must remain available — leave in `superchat.el`
or also move, your call, but **prefer leaving them in `superchat.el`** so
config is in one place):
- `superchat-llm-backend`, `superchat-llm-model`, `superchat-llm-streaming`
- `superchat-llm-reasoning`, `superchat-llm-tools-enabled`
- `superchat-response-timeout`, `superchat-tool-timeout-multiplier`
- `superchat-show-ttft`, `superchat-show-ttft-breakdown`

Just forward-declare them in `superchat-llm.el` with bare
`(defvar superchat-llm-backend)` lines and let `superchat.el` own the
defcustoms.

### Step 6 — `superchat-dispatcher.el` (~200 lines, highest value, highest risk)

Move:

- `superchat-send-input` (1556)
- `superchat--dispatch-result` (1652)
- `superchat--handle-command` (1514)
- `superchat--handle-default-command` (1527)
- `superchat--execute-llm-query` (1503)
- `superchat--build-final-prompt` (1394)
- `superchat--format-retrieved-memories` (1360)
- `superchat--read-inline-file-content` (1480)
- `superchat--render-inline-context` (1493)
- `superchat--textual-file-p` (1474)
- `superchat--normalize-file-path` (1336)
- `superchat--extract-file-path` (1348)
- `superchat--extract-file-paths` (search for it — used by core.el line 110)

**After moving, do the bub-alignment fix**:

Right now `superchat-send-input` parses input + builds prompt itself,
duplicating logic that `superchat-core-run-turn` already does via the
`superchat-system-prompt-functions` / `superchat-build-prompt-functions`
hooks. The current `superchat-core.el:116` says explicitly:

> "Request-time decisions such as tool collection and backend invocation
> stay in the dispatcher so plain chat does not pay agent/tool overhead
> before the first token."

That comment will become inaccurate after this step. **The actual goal
of step 6** is:

1. Move dispatcher functions to `superchat-dispatcher.el`.
2. Have `superchat-send-input` build a `superchat-turn` via
   `superchat-turn-new`, then call `superchat-core-run-turn` on it.
3. Register the existing prompt-build logic (`superchat--build-final-prompt`,
   memory retrieval, file-context inlining, system prompt assembly) as
   functions on `superchat-system-prompt-functions` and
   `superchat-build-prompt-functions` instead of inlining them.
4. After `superchat-core-run-turn` returns the populated turn, the
   dispatcher calls `superchat--llm-generate-answer` with the turn's
   `prompt` and `tools` slots.

**This is the only step where you should expect tests to need adjustment.**
If a test mocks `superchat-send-input` or asserts on intermediate state,
its expectations may shift. Do NOT change test assertions to make them
pass — if a test breaks because behavior changed, that means the
refactor changed behavior, which is forbidden. Stop, revert step 6, and
ask the human reviewer.

If step 6 turns out to require any behavioral change to be feasible,
**commit steps 1-5 and leave step 6 as a follow-up TODO** in
`ROADMAP.md` under v0.9. Steps 1-5 are valuable on their own.

---

## Validation checklist (run after EACH step)

```bash
# from repo root
emacs -batch -L . -L test -l ert -l test/test-llm-backend.el \
      -f ert-run-tests-batch-and-exit 2>&1 | tail -20

# byte-compile sanity (must produce zero new warnings)
emacs -batch -L . --eval "(setq byte-compile-error-on-warn nil)" \
      -f batch-byte-compile superchat*.el 2>&1 | grep -E "Warning|Error"
```

Expected test result: **28 passed, 1 unexpected**
(the pre-existing abort at `test/test-llm-backend.el` ~line 147 is not
your problem — it predates this refactor).

Expected byte-compile result: zero new warnings. Pre-existing warnings
are allowed; new ones are not. Compare against a baseline by running
the byte-compile once on `main` before you start and saving the output.

## Out of scope (do NOT do these)

- Do not touch `superchat-memory.el`, `superchat-skills.el`,
  `superchat-tools.el`, `superchat-db.el`, `superchat-executor.el`,
  `superchat-parser.el`, `superchat-skills-standard.el`, or `superchat-core.el`
  (except step 6's hook registration).
- Do not commit `superchat.elc`. Add a `.gitignore` entry only if it
  doesn't already exist (check `.gitignore` first — the project's
  gitignore is reportedly out of date but that is a separate v0.9 task).
- Do not delete or rename `:memory:`, `.claude/`, `.omo/`, `.pi/` — those
  are unrelated working-tree noise.
- Do not write a CHANGELOG or update README. The roadmap update (adding
  this refactor under v0.9) is fine and encouraged at the end.

## When done

Update `ROADMAP.md` v0.9 section: add a bullet
`- [x] Split superchat.el monolith into focused modules (steps 1-N done)`
listing which steps completed. If step 6 was deferred, mark it
`- [ ] Step 6: dispatcher → core pipeline integration (deferred)`
with a one-line reason.

Final report back: list each commit SHA, the file(s) it created,
line-count delta of `superchat.el` after each step, and the
post-step test result. Flag anything you skipped or that surprised you.
