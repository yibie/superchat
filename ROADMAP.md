# Superchat Roadmap (v0.5 → v1.0)

> Status snapshot: **v0.6 shipped** (Memory-Soul dual-track separation).
> Next milestone: **v0.7 — Skills v2: standard format + workflow merge**.

This document is the single source of truth for the post-v0.5 release plan.
It is **reconstructed**, not derived from a pre-existing artifact — see
"Provenance" below for the inputs it was inferred from.

---

## Provenance

No 0.6+ plan existed in the repo, in `~/.claude/plans/`, or in the project
session archive. This roadmap was inferred from:

- The current state of `superchat.el`, `superchat-memory.el`, `superchat-skills.el`, etc.
- Design notes in `docs/`: `memory-design.org`, `memory-implementation-plan.md`, `SKILLS_QUICKSTART.md`
- The unversioned design idea `docs/design/memory-soul-separation-idea.md` (2026-03-18)
- The repo's stated direction in `AGENTS.md` quirk #8 (gptel → llm.el, drop `superchat-agent.el`)

Every milestone below lists its **source** so you can audit the inference.

---

## Milestones

### v0.5 — Backend hard-swap (gptel → llm.el) — SHIPPED

Source: AGENTS.md quirk #8 + commit history.

- Commits: `19bb2d8` (main migration, 19 files, +1781/-1927) + `890e561` (`superchat--glob-to-regexp` for Emacs 28.1 compat)
- Removed `superchat-agent.el` (was WIP-on-main, no docs/tests, `gptel-agent` not on MELPA)
- New minimum: **Emacs 28.1** (was 27.1; `llm.el`'s `plz` dep requires it)
- New dependency: `(llm "0.7")` from GNU ELPA (currently installed: 0.31.0)
- Removed gptel-specific `:around` advice on `gptel-curl--stream-cleanup`
- New defcustoms: `superchat-llm-backend`, `superchat-llm-model`, `superchat-llm-streaming`, `superchat-manual-models`
- New `/backend` command (replaces `/tools`; `/tools` kept as alias)
- Built-in tool registry (13 tools via `llm-make-tool`, auto-populated)
- Test suite rewritten: `test/test-llm-backend.el` (29 tests, 28 pass; 1 pre-existing abort in test 7 — see v0.9)

### v0.6 — Memory-Soul dual-track separation — SHIPPED

Source: `docs/design/memory-soul-separation-idea.md` (5 todos) + `docs/memory-design.org` + `docs/memory-implementation-plan.md`.

**The critique**: current `superchat-memory.el` is too "knowledgified" — it summarizes too aggressively (loses context/emotion), merges contradictions (memory is naturally contradictory), and decays (sometimes "the feeling then" matters).

**Key deliverables** (all 8 shipped):

- [x] `superchat-memory-add-raw` — entry function with `:mood`, `:context`, `:verbatim` keywords
- [x] `soul.org` separate from `memory.org` in the data directory
- [x] `superchat-memory-synthesize-soul` — manual-trigger only, never auto
- [x] Contradiction coexistence: `:CONTRADICTION:` tag + `:VALIDITY: expired` property + `:REPLACED_BY:` link
- [x] Mood tag taxonomy: `:MOOD:` (frustrated / curious / tired / etc.)
- [x] Retrieval re-includes ±3 surrounding messages + mood + time context
- [x] Replace `superchat-memory-auto-merge-enabled` default with manual review UI
- [x] `defcustom superchat-soul-synthesis-mode` (`manual` / `weekly` / `never`)

**Implementation notes**:

- New defcustoms: `superchat-memory-soul-file`, `superchat-memory-soul-synthesis-mode`, `superchat-memory-contradiction-context-window`, `superchat-memory-mood-taxonomy`
- New entry points: `superchat-memory-add-raw`, `superchat-memory-retrieve-with-context`, `superchat-memory-synthesize-soul`, `superchat-memory-review-pending` (interactive)
- New review mode: `superchat-memory-review-mode` with y/n/e/s/q keybindings for one-keystroke accept/reject
- Bidirectional contradiction surfacing: both outgoing (entry's own `:CONTRADICTION:`) and incoming (another entry points at this one) are detected by `--enrich-with-context`
- Soul track reads/writes `soul.org` directly; no LLM required for raw event capture. Synthesis remains a no-op when gptel/llm is absent.
- Test coverage: new `test/test-memory-soul.el` (23 tests, all pass) covering file I/O, contradiction parsing, mood resolution, paired-expired retrieval, review queue, and keymap bindings.

**Open questions**:

- How to surface contradictions in `/commands` output without noise?
- Should `:CONTRADICTION:` show both sides side-by-side in retrieval?
- How to make manual review feel low-friction (one keystroke accept/reject)?

**Files likely affected**: `superchat-memory.el`, new `data-directory/soul.org`, possibly `superchat.el` for review UI.

### v0.7 — Skills v2: standard format + workflow merge

Source: `superchat-skills-standard.el` (240 lines, scaffolding only) + `superchat-skills.el` (workflow support) + `examples/standard-skills/`.

The current skill system has two parallel paths:

- In-repo `skills/*.md` files loaded via `>skill-name`
- OpenAI/Anthropic `SKILL.md` format (with `name`, `description`, `version: "1.0"` header)
- `superchat-skills-standard.el` is the bidirectional converter, but the merge is loose

**Key deliverables**:

- [ ] Finish `superchat-skills-standard` to handle missing fields gracefully (no nil-deref on incomplete headers)
- [ ] Unify in-repo `skills/*.md` and `SKILL.md` format — pick one canonical header (suggest: `SKILL.md`-style with `name:` + `description:` + `version:`)
- [ ] Fold `superchat-skills.el` `>workflow-name` into the unified skill system (workflow becomes a sub-type, not a separate parser)
- [ ] Test coverage for round-trip: `SKILL.md` → internal → `SKILL.md` (byte-identical)
- [ ] Validate examples in `examples/standard-skills/` against the new canonical form
- [ ] `superchat-skills-standard-import` and `-export` become stable, documented entry points

**Open questions**:

- Keep workflow as a sub-type of skill, or vice versa?
- Should `version: "1.0"` in `SKILL.md` be enforced or optional?
- Skill name with hyphens vs underscores: which is canonical?

**Files likely affected**: `superchat-skills.el`, `superchat-skills-standard.el`, `skills/*.md`, `examples/standard-skills/`.

### v0.8 — SQLite memory facade

Source: stash `wip/v0.8-sqlite-memory-facade` (recovered 2026-06-04).

**The pivot**: replace the Org-mode memory store (`memory.org` + `soul.org`,
org-ql search, RELATED multi-hop BFS, keyword LLM enrichment,
contradiction pairing, mood taxonomy, ACCESS_COUNT decay, soul synthesis)
with a thin compatibility facade over `superchat-db.el` (SQLite + FTS5).

**Why**: the org-mode layer accreted ~2200 lines of bespoke
search/scoring/decay code that SQLite + FTS5 already provides natively.
v0.6 shipped the dual-track separation; v0.8 collapses both tracks onto
one storage engine.

**Key deliverables**:

- [ ] Rewrite `superchat-memory.el` as a facade over `superchat-db` (the
      stashed 337-line version is the draft — needs companion test rewrite
      before unstashing)
- [ ] Migrate `soul.org` / `memory.org` retrieval call sites to facade API
- [ ] Rewrite `test/superchat-memory-org-ql-tests.el` and
      `test/test-memory-soul.el` against the SQLite store (or delete
      org-ql-specific tests outright)
- [ ] One-shot migration command `M-x superchat-memory-migrate-from-org`
      that imports existing `memory.org` / `soul.org` into the SQLite store
- [ ] Decide fate of `superchat-memory-soul-synthesis-mode` defcustom
      (likely retire — synthesis is a SQL aggregate now)

**Open questions**:

- Keep raw event log (`soul.org`) as append-only org-mode, or move that
  too? (The stashed version moves it.)
- LLM keyword enrichment — keep or delete? SQLite FTS5 handles most
  recall without it.

### v0.8.5 — MCP v2: multi-server orchestration

Source: `superchat.el` `/mcp` + `/mcp-start` commands + `mcp.el` dependency.

Currently MCP is "zero-config" but limited in practice:

- Single active server lifetime (no orchestration across multiple)
- No tool namespace conflict resolution (two servers both exposing `search_text` collide)
- No persistence of running servers across sessions
- No per-server health checks

**Key deliverables**:

- [ ] Multi-server tool namespace: `mcp:<server-name>:<tool-name>` to disambiguate
- [ ] Server lifecycle: explicit start/stop per server, plus auto-start on first use
- [ ] Health check on `/mcp` invocation (lightweight ping, e.g. `plz-curl` GET on SSE endpoint)
- [ ] Per-session server selection: `:chat-mcp-servers` alist (subset of `mcp-hub-servers`)
- [ ] Graceful degradation if a server is down (continue with other tools, log warning)
- [ ] `/mcp` output shows per-server status table (configured / running / tools / health)

**Open questions**:

- How to surface server-down events in chat (inline message vs silent log)?
- Auto-start vs explicit start: default to explicit for safety?
- Should tool namespace be `mcp:name:tool` or `name::tool`?

**Files likely affected**: `superchat.el`, possibly new `superchat-mcp.el` if it grows.

### v0.9 — Stabilization pass

This is the "MELPA-readiness" milestone, not a feature milestone.

**Key deliverables**:

- [x] Split superchat.el monolith into focused modules (2589 → ~1548 lines):
  - [x] Step 1: superchat-models.el (model listing, caching, @model switching)
  - [x] Step 2: superchat-save.el (conversation save to Org files)
  - [x] Step 3: superchat-mcp.el (MCP server management)
  - [x] Step 4: superchat-render.el (buffer rendering, MD→Org, TTFT)
  - [x] Step 5: superchat-llm.el (LLM backend, tools, streaming)
  - [x] Step 6: superchat-dispatcher.el (dispatch, prompt building, send-input) — pure move only
  - [x] Step 6 follow-up: hook pipeline alignment (commits 0c1bb37, 0932fed, 19ccfed, 9bcef78) — prompt building now runs exclusively through superchat-core-run-turn hooks. superchat--build-final-prompt deleted; replaced by 5 focused hook functions in superchat-prompt-hooks.el.

- [ ] All `defcustom` have `:type` and full docstrings (no missing `:type` like `superchat-memory-relation-suggestion-threshold`)
- [ ] All public interactive functions have `;;;###autoload` (currently only 2)
- [ ] No type-error suppression (`as any` equivalents are not used in this repo — verify)
- [ ] `M-x checkdoc` clean on all `.el` files
- [ ] `M-x package-lint` clean
- [ ] CI runs ERT on Emacs 28.1, 29.x, 30.x (via `.github/workflows/test.yml`)
- [ ] Bump `(llm "0.7")` to actual minimum tested version (currently installed: 0.31.0)
- [ ] Fix the pre-existing test abort at `test/test-llm-backend.el:147` (`cl-no-applicable-method` not caught by `(error nil)`)
- [ ] Clean up working-tree noise: `.claude/`, `.omo/`, `.elc` files; commit or revert `threadnote-mvp/` deletions
- [ ] Reconcile `.gitignore` (it lists `test/`, `docs/`, `AGENTS.md` but they are tracked)

**Open questions**:

- Min Emacs: keep 28.1, or bump to 28.2?
- Add `Cask` / `Eldev` for reproducible dev environment?
- Remove `superchat.elc` from tracked state?

### v1.0 — First MELPA-ready stable release

v0.9 + version bump to `1.0.0`, tag, push, submit to MELPA.

**Key deliverables**:

- [ ] `;; Version: 1.0` header in `superchat.el`
- [ ] Git tag `v1.0.0` on `main`
- [ ] CHANGELOG entry for v1.0 (in `README.md` / `README_cn.md`)
- [ ] MELPA submission: `superchat-pkg.el` (autoload declarations) + `Cask` or manual recipe
- [ ] README + AGENTS.md reflect v1.0 state
- [ ] `;; Package-Requires: ((emacs "28.1") (llm "<bumped>"))` finalized

---

## Cross-cutting concerns

- **Style**: `lexical-binding: t` everywhere; `superchat--` (private) vs `superchat-` (public) prefix — keep it consistent
- **Tests**: each milestone extends ERT coverage in the same PR, not as follow-up
- **Docs**: each milestone updates `AGENTS.md` quirks + `README.md` / `README_cn.md` sections
- **No silent breaking changes**: any defcustom default change must use `:safe` predicate and a one-version deprecation cycle
- **Working tree hygiene**: don't ship commits that include `*.elc`, `~/.claude/`, or unrelated deletions

## Status legend

- `SHIPPED` — committed and pushed to `main`
- `NEXT` — active work, current milestone
- `📋` — planned, scope frozen
- `🚧` — in progress

## Revision history

- 2026-06-01: Initial draft. Reconstructed from current state + `docs/design/memory-soul-separation-idea.md` + `docs/`. v0.5 listed as SHIPPED (commits `19bb2d8` + `890e561`).
- 2026-06-01: v0.6 SHIPPED. All 8 Memory-Soul dual-track deliverables complete. Next milestone is v0.7 (Skills v2).
