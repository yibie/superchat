# Handoff: v0.8 — SQLite memory facade

**Audience**: implementer model (deepseek-v4-pro).
**Predecessors**:
- `docs/handoff-v0.9-split.md` (shipped, commits `a17f67e`..`e6f3198`)
- `docs/handoff-v0.9-hook-alignment.md` (shipped, commits `0c1bb37`..`037a7fd`)

**Goal**: collapse the ~2200-line Org-mode memory subsystem into a thin
compatibility facade over `superchat-db.el` (SQLite + FTS5). After this
handoff, `superchat-memory.el` is ≤ 400 lines, all memory tests pass
against the SQLite store, and the existing chat surface keeps working
without behavior changes visible to end-users.

**Why now**: v0.6 shipped a Memory/Soul dual-track separation that
already strained against Org-mode's limitations (no real FTS, custom
keyword enrichment, manual contradiction pairing). `superchat-db.el`
already implements SQLite + FTS5 + tape API. The org-mode layer is
duplication.

---

## Inputs you inherit

### 1. A stashed draft

```
stash@{0}: On main: wip/v0.8-sqlite-memory-facade
```

Apply with:
```bash
git stash show -p stash@{0} > /tmp/sqlite-memory.patch
# read it; do NOT apply yet
```

The stash rewrites `superchat-memory.el` from **2516 → 337 lines**. It
keeps:

- `superchat-memory-capture-explicit`
- `superchat-memory-capture-conversation`
- `superchat-memory-add-raw`
- `superchat-memory-summarize-session-history`
- `superchat-memory-import-from-org` (new — migration command)

It **deletes** (the org-only complexity that SQLite + FTS5 replaces):

- org-ql search backend
- RELATED multi-hop BFS
- LLM keyword enrichment
- Contradiction pairing via `:CONTRADICTION:` tag
- Mood taxonomy resolution
- `ACCESS_COUNT` decay
- Soul synthesis (`superchat-memory-synthesize-soul`)

**Critical gaps in the stash** (verify with grep, then add back):

The stashed draft does NOT define these, but `superchat.el` declares
and calls them (see `superchat.el:39-44`):

- `superchat-memory-retrieve` (called at `superchat.el:1493`)
- `superchat-memory-auto-capture` (declared at `superchat.el:42`)
- `superchat-memory-compose-title` (called at `superchat.el:853, 1506`)

If you unstash as-is, `superchat.el` won't load. **Re-add these as
thin wrappers over `superchat-db-memory-search` / `-search-simple` /
similar.** Their semantics:

- `retrieve` returns a list of rows (today: plists with keys like
  `:id :title :content :keywords`). Map SQLite rows to the same
  shape so call sites in `superchat.el` don't break.
- `auto-capture` is a heuristic trigger that decides whether to
  capture a turn (length threshold, command-mode trigger, etc.).
  Keep the simplest reasonable heuristic — length ≥
  `superchat-memory-auto-capture-minimum-length`. The previous
  multi-tier logic is over-engineered.
- `compose-title` derives a short title from content (max
  `superchat-memory-title-max-length` chars, current default 78).
  Keep this — it's a pure string function, no storage concern.

### 2. Companion tests (already on disk, untracked)

`test/test-memory-sqlite-facade.el` (57 lines, 2 tests, untracked).
This is the start of the test rewrite — but only 2 tests cover the
facade. Expand it.

The currently-tracked tests that WILL break after this change:

- `test/superchat-memory-org-ql-tests.el` (16 tests) — tests org-ql
  search, RELATED BFS, keyword association, merge-candidates. **All
  obsolete.** Delete the file.
- `test/test-memory-soul.el` (33 tests) — tests soul.org I/O,
  contradiction parsing, mood taxonomy, review queue, paired-expired
  retrieval. About **half remain valid** (the parts about raw event
  capture in soul.org are preserved; the parts about contradiction
  pairing / mood taxonomy / synthesis are deleted with the org layer).
  **Audit each test**; rewrite those that test surviving behavior
  against the new API; delete the rest.

### 3. Storage primitives already in place

`superchat-db.el` (425 lines, on `main`) provides:

```
memory: insert, search (FTS5), search-simple, get-by-id, set-review,
        set-replaced, count, delete-by-id, delete-rejected-older-than,
        list-review
tape:   replay, last-anchor, search, append (verify via grep)
schema: ensure-schema, current-schema-version (v1 in place)
stats:  stats, vacuum
```

If something the facade needs isn't there, **add it to `superchat-db.el`
under the schema-v1 envelope or bump to v2 with a migration step**.
Document the version bump in the commit message.

### 4. Call sites that must keep working

From `grep -n "superchat-memory-" superchat.el`:

| Call site                                    | Function called                          | What it expects back |
|----------------------------------------------|------------------------------------------|----------------------|
| `superchat.el:853`                           | `compose-title`                          | string               |
| `superchat.el:1373-1374`                     | `summarize-session-history`              | string or nil        |
| `superchat.el:1492-1493`                     | `retrieve`                               | list of rows         |
| `superchat.el:1506-1507`                     | `compose-title` + `capture-explicit`     | id (integer)         |
| `superchat.el:1510`                          | `capture-conversation`                   | id (integer)         |

The dispatcher's prompt-hooks layer (added in A1) consumes
`turn.retrieved-memories` which is populated from `retrieve`. Verify
`superchat-prompt-hooks.el:superchat-prompt-hook--memory-context` still
formats them correctly. **If the SQLite row shape differs from the
plist shape**, fix the hook function, not the call site.

---

## Constraints

- **No new defcustoms beyond what the stash already defines**, except
  to add `superchat-memory-sqlite-path` if SQLite needs a path override
  separate from `superchat-data-directory`. Default: derive from
  `superchat-data-directory`. The stash currently has 6 defcustoms;
  budget for ≤ 8 total.
- **No new dependencies.** SQLite is built into Emacs 29+. Project
  minimum is Emacs 28.1 — verify SQLite availability there. If 28.1
  doesn't have SQLite, this milestone requires bumping the minimum
  (mention in commit, update Package-Requires header in `superchat.el`).
- **Preserve `add-raw` writing to `soul.org`** — per the companion
  test `test-memory-sqlite-soul-raw-log-stays-org`, raw event capture
  stays in append-only org. Only the durable/searchable layer moves
  to SQLite. This is intentional dual-track persistence: SQLite for
  retrieval, soul.org for ground-truth append-only log.
- **No breaking changes to interactive commands.** `M-x
  superchat-memory-review-pending`, `M-x superchat-memory-synthesize-soul`
  etc. — if a command becomes a no-op because its underlying mechanism
  was deleted, keep the command bound and have it message the user
  ("synthesis is a SQL aggregate now; run `M-x superchat-memory-stats`
  instead"). Do NOT delete the autoload entries.
- **Migration path**: `superchat-memory-import-from-org` must read
  the existing `memory.org` and `soul.org` from `superchat-data-directory`
  and insert rows into SQLite. This is a one-shot, idempotent command
  (rerunnable without duplicating). Surface a confirmation prompt
  before running. Document in README.

---

## Execution plan (5 steps, one commit each)

### Step 1 — Apply and de-stash safely

```bash
git stash apply stash@{0}   # do NOT pop; keep the stash as backup
```

`superchat-memory.el` is now 337 lines and `superchat.el` won't load
(missing functions). **Commit step 1 separately as
`refactor(memory): WIP — apply stashed SQLite facade draft`** so
later steps have a clear bisect anchor.

(The handoff allows this single commit to have failing tests — it's
clearly marked WIP. Subsequent commits must pass tests.)

### Step 2 — Restore the missing 3 functions

Add `superchat-memory-retrieve`, `superchat-memory-auto-capture`,
`superchat-memory-compose-title` as thin SQL-backed wrappers. Verify
`superchat.el` byte-compiles and loads. At this point `M-x superchat`
must launch without error.

Acceptance:
```bash
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 | grep -E "Error|undefined function" | wc -l
# must be 0
```

Commit: `feat(memory): restore retrieve/auto-capture/compose-title as SQLite wrappers`.

### Step 3 — Rewrite tests

- Delete `test/superchat-memory-org-ql-tests.el` (16 obsolete tests).
- Audit `test/test-memory-soul.el` (33 tests) test-by-test:
  - Keep + rewrite if testing surviving behavior (add-raw → soul.org,
    raw-event reading, soul-file location, mood-on-write).
  - Delete if testing deleted behavior (contradiction pairing,
    mood-taxonomy resolution, synthesis, review queue using org).
- Expand `test/test-memory-sqlite-facade.el` from 2 → at least 12
  tests covering: capture (explicit, conversation, auto), retrieve
  (FTS5 hit, FTS5 miss, limit honored), prune, import-from-org
  round-trip, compose-title length cap.

Acceptance:
```bash
emacs -batch -L . -L test -l ert \
  $(ls test/test-*.el | sed 's/^/-l /') \
  -l test/superchat-memory-org-ql-tests.el 2>/dev/null \
  -f ert-run-tests-batch-and-exit 2>&1 | grep "^Ran" | tail -1
# expect: at least N tests, 0 unexpected failures (where N >= 96 - 49 removed + 12 added = ~59)
```

Document the test delta in the commit:
```
test(memory): rewrite memory tests against SQLite facade

- Delete superchat-memory-org-ql-tests.el (16 obsolete tests)
- Audit test-memory-soul.el: kept N, rewrote N, deleted N
- Expand test-memory-sqlite-facade.el from 2 to 12 tests
- Net: <pre> -> <post> tests, 0 unexpected failures
```

### Step 4 — Migration command

Implement `superchat-memory-import-from-org` and wire `M-x` autoload.
Read `memory.org` and `soul.org` from `superchat-data-directory` if
present, insert each entry into SQLite. Skip duplicates (compare on
content hash). Print a summary at end. Add 1 ERT test using a fixture
org file under `test/fixtures/` (create the directory).

If there's an existing `superchat-memory-migrate-from-org` mentioned
in ROADMAP TODOs, name it that for consistency.

Commit: `feat(memory): one-shot import from memory.org/soul.org to SQLite`.

### Step 5 — Documentation + ROADMAP

- `README.md` / `README_cn.md`: add a "v0.8 migration" section under
  Memory describing the SQLite move and the one-shot import command.
- `ROADMAP.md`: mark v0.8 SHIPPED, list commit SHAs, move
  open-questions outcomes inline.
- `AGENTS.md` (if a memory quirk exists there): update.
- Delete `docs/memory-design.org` if it's still referenced (check —
  ROADMAP v0.6 cites it as a source; either keep it as historical
  record or move under `docs/design/` alongside
  `memory-soul-separation-idea.md`).

Commit: `docs(memory): document v0.8 SQLite migration; mark v0.8 shipped`.

---

## Validation checklist (after each step)

```bash
# 1. byte-compile must be clean
rm -f superchat*.elc
emacs -batch -L . -f batch-byte-compile superchat*.el 2>&1 | grep -E "Warning|Error"
# baseline: zero new warnings vs pre-step

# 2. tests must pass
emacs -batch -L . -L test -l ert \
  -l test/test-llm-backend.el \
  -l test/test-core-pipeline.el \
  -l test/test-prompt-hooks.el \
  -l test/test-memory-sqlite-facade.el \
  $(ls test/test-memory-soul.el 2>/dev/null | sed 's/^/-l /') \
  -f ert-run-tests-batch-and-exit 2>&1 | grep "^Ran"
# baseline (pre-step): "Ran 98 tests, 96 results as expected, 2 unexpected"
# the 2 unexpected pre-existing failures both vanish after Step 3.
# expected post-Step-5: "Ran N tests, all expected, 0 unexpected"

# 3. interactive smoke (the implementer can't do this — record in final report)
# M-x superchat → type "hello" → see normal response
# M-x superchat-memory-review-pending → buffer opens (or no-op message)
# M-x superchat-memory-import-from-org (with a test memory.org present) → import
```

If at any point the 2 pre-existing failures
(`superchat-memory-org-ql-combines-keywords-across-fields` and
`test-memory-soul-retrieve-with-context-surfaces-paired-expired`) are
STILL failing after their containing files are touched — they're
obsolete; delete them as part of step 3.

---

## Decisions left open (surface in final report, don't decide silently)

1. **Soul.org future**: stash preserves `add-raw → soul.org`. Long-term
   question: should soul events also move to a SQLite `raw_events`
   table for unified storage? **Default for v0.8: keep dual-track
   (SQLite for facade, soul.org for raw log).** Don't change unless
   strongly indicated by the test rewrite.
2. **LLM keyword enrichment**: deleted by the stash. The companion
   test for it (`test-memory-keyword-association-discovers-indirect-entry`
   etc.) is obsolete. **Confirm with reviewer**: are there any
   user-facing flows that depended on LLM-enriched keywords beyond
   retrieval? If not, deletion stands.
3. **Synthesis command**: `superchat-memory-synthesize-soul` becomes a
   no-op or aggregator. **Default: convert to a function that runs
   `SELECT title, count(*) FROM memory_entries GROUP BY review_status`
   and inserts the summary into a buffer.** It loses the LLM-driven
   synthesis. If that's wrong for the user, they'll say so.

---

## Out of scope

- MCP v2 (moved to v0.8.5 — see ROADMAP).
- Skills v2 (v0.7 — separate handoff).
- Adapter abstraction (v1.x).
- Touching `superchat-skills.el`, `superchat-tools.el`,
  `superchat-llm.el`, `superchat-render.el`, `superchat-dispatcher.el`,
  `superchat-models.el`, `superchat-save.el`, `superchat-mcp.el`,
  `superchat-prompt-hooks.el`, `superchat-core.el`,
  `superchat-executor.el`, `superchat-parser.el`,
  `superchat-skills-standard.el`.
- The 33 untracked files in `docs/` and `test/` that became visible
  after the `.gitignore` rewrite — separate cleanup task, don't touch
  here.

## Final report

Reply with:

1. Commit SHA list, one per step.
2. `wc -l superchat-memory.el` before/after (expect 2516 → ≤ 400).
3. Test count before/after (expect 98 → ~60-ish, all passing).
4. List of test files deleted / rewritten / created.
5. List of API gaps in the stash you filled (the 3+ functions).
6. Decisions made on the 3 open-questions, with one-sentence
   justifications.
7. Any SQLite schema bumps you did (and the migration code).
