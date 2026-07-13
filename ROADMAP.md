# Superchat Roadmap (v0.5 → v1.2)

> Status snapshot: **v1.2.0 shipped (2026-07-12). v1.2.1 (async
> sub-agent engine) on main 2026-07-12, deliberately untagged. MELPA
> not yet submitted.**
> v1.2.1 details: `docs/goals/subagent-async-engine.md` — sub-agent runner
> migrated off `make-thread`/sync calls onto the async callback-chain
> model proven by the v1.2 workflow engine: true parallel delegation,
> in-place progress placeholders, delegation depth guard
> (`superchat-subagent-max-depth`). North star: **agent harness**
> (`docs/goals/agent-harness.md`) — build Claude-Code-style agent
> scaffolding and multi-agent collaboration in Emacs. Next milestone
> **v1.3 "harness contract"**: preset runtime contract
> (`agent-profiles.md` Phase 1) → agent registry so the main agent
> can discover custom `type: agent` skills (delegate tool description
> is currently hardcoded to the 3 built-ins) → profile fields
> (plan F: 5 typed slots, tighten-only). Then **v1.4 "control
> plane"**: list/cancel/timeout for running sub-agents. Deferred:
> MELPA submission, workflow branching, the 3 pre-existing
> `ecosystem/lsp-*` test failures.
>
> ⚠️ This document has gaps: some shipped items are still marked [ ] and
> the llm version is stale (0.7 → 0.24). See CHANGELOG.md for accurate
> per-version detail.

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
- New dependency: `(llm "0.24")` from GNU ELPA
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

### v0.7 — Skills v2: standard format + workflow restoration — ✅ SHIPPED

Shipped in commits:
- `22dfb66` feat(skills): unify SKILL.md frontmatter across in-repo and standard skills
- `acd2458` test(skills): SKILL.md round-trip tests
- `6c2f8a3` feat(workflow): restore step-by-step execution as SKILL.md type
- `4782cf3` feat(dispatch): route SKILL.md type=workflow to step executor
- `68ed5a5` fix(skills): repair SKILL.md round-trip export (regex + version/triggers)
- `485b531` feat(workflow): legacy .workflow file import shim (step 5)

**Open questions resolved**:
- Field ordering: name, description, version, type, triggers (aligned with export)
- triggertters format: JSON array [\"a\", \"b\"] in YAML frontmatter
- version field: optional, defaults to \"1.0\"
- type field: \"prompt\" (default) or \"workflow\"

Source: `superchat-skills.el` (733 lines, includes implicit-match
subsystem ROADMAP didn't note) + `superchat-skills-standard.el`
(237 lines, v1 implementation — not "scaffolding only" as previously
recorded) + `examples/standard-skills/` + git history of the deleted
`superchat-workflow.el` (last seen at `6f32427`, removed by `31e4fd3`).

**Two parallel concerns to address**:

#### Track A — SKILL.md format unification

- In-repo `skills/*.md` files (currently 3: code-review, planning,
  refactor) use an ad-hoc header. Other skills loaded via
  `superchat-skills-standard.el` use OpenAI/Anthropic `SKILL.md`
  frontmatter (`name:` / `description:` / `version:`).
- `superchat-skills-standard.el` already does bidirectional
  conversion, but missing-field handling can nil-deref and
  round-trip fidelity hasn't been pinned down with tests.

#### Track B — Workflow as a SKILL.md sub-type

Workflow used to be a first-class concept in superchat — `.workflow`
files executed line-by-line, each line being one step (`/command`,
`@model`, plain prompt, or `#file` reference). It was deleted by
`31e4fd3` when skills landed, but skills solved a *different* problem:
**skills = single LLM call with a long prompt template (a role);
workflow = multi-step linear recipe (a flow).** Folding them into one
parser at the time was a regression.

v0.7 restores workflow as a **sub-type of the unified SKILL.md
format**. A skill file declares its type in frontmatter:

```
---
name: ai-news-summary
description: 每周技术新闻摘要
version: "1.0"
type: workflow      # default: "prompt"
---

# Workflow body — non-empty lines are steps, executed top-to-bottom
/web-search "$input" 最新新闻
@qwen3-coder:30b 给上述结果做 3 个角度的中文摘要（商业 / 技术 / 社会）
将分析结果保存到 #~/Documents/news-summary.md
```

Trigger stays unified: `>name` resolves a skill or a workflow
depending on `type:` in the loaded file. No `>>` syntax, no namespace
collision — file existence + frontmatter is the source of truth.

**Key deliverables**:

- [x] Track A: `skills/*.md` (3 files) migrated to SKILL.md frontmatter
- [x] Track A: `superchat-skills-standard--load-metadata` rejects
      missing required fields gracefully
- [x] Track A: round-trip test (`test/test-skills-roundtrip.el`) exists
- [x] Track A: `examples/standard-skills/code-review/` validated
- [x] Track B: `superchat-workflow.el` step executor (147 lines)
- [x] Track B: SKILL.md loader honours `type: workflow`
- [x] Track B: import shim for legacy `.workflow` files
- [x] Implicit-match subsystem untouched — works for both types

**Open questions**:

- Should workflow steps run with the user-confirmed tool list, or
  re-prompt per step?
- How to surface step-by-step progress in the chat buffer?
  (Probably reuse the streaming-pending face from v0.6.)
- `type: workflow` body uses what variable substitution? Today's
  workflow had `$input`; keep that and add `$lang`?
- Should round-trip testing pin `version: "1.0"` as required or
  accept omitted?

**Files likely affected**: `superchat-skills.el`,
`superchat-skills-standard.el`, NEW `superchat-workflow.el`,
`skills/*.md`, `examples/standard-skills/`.

**Reference commits to mine**:

- `6f32427` (Oct 2025) — last major workflow refactor; contains the
  458-line `superchat-workflow.el` step executor with timeout/error
  protocol
- `d228c72` (Oct 2025) — original `>workflow-name` integration; shows
  the `.workflow` plain-text format and how `/command`+`@model`+`#file`
  composed per line
- `31e4fd3` — the commit that deleted `superchat-workflow.el` when
  skills landed

### v0.8 — SQLite memory facade ✅ SHIPPED

Source: stash `wip/v0.8-sqlite-memory-facade` + handoff `docs/handoff-v0.8-sqlite-memory.md`.

**The pivot**: replaced the Org-mode memory store with a thin compatibility
facade over `superchat-db.el` (SQLite + FTS5).

Commits: `e8feffd`, `49c47e7`, `fe7b9a7`.

**Key deliverables**:

- [x] Rewrite `superchat-memory.el` as a facade over `superchat-db`
      (2516 → 337 lines)
- [x] Rewrite test suite: delete org-ql tests (16), audit soul tests
      (33 → 8 surviving), expand facade tests (2 → 13)
- [x] One-shot migration `M-x superchat-memory-import-from-org`
      imports `memory.org` into SQLite

**Decisions on open questions**:
- Soul.org: kept as dual-track — `add-raw` now writes to SQLite tape;
  soul.org remains as historical archive, not actively written.
  Long-term: move to SQLite `raw_events` table (deferred to v0.8.5).
- LLM keyword enrichment: deleted. SQLite FTS5 handles most recall.
  No user-facing flow was confirmed to depend on it.
- Synthesis: `superchat-memory-synthesize-soul` retired. Use
  `M-x superchat-memory-stats` for aggregate counts. Interactive
  binding preserved with no-op message.

### v0.8.5 — MCP v2: multi-server orchestration — ✅ SHIPPED (2026-06-10)

Source: `superchat.el` `/mcp` + `/mcp-start` commands + `mcp.el` dependency.

Shipped in commit (upcoming). `superchat-mcp.el` rewrote from 109 → 326 lines
with all 6 deliverables complete.

**Key deliverables** (all shipped):

- [x] Multi-server tool namespace: `mcp:<server>:<tool>` via `--prefix-tool`
- [x] Server lifecycle: `superchat-mcp-start-server`, `superchat-mcp-stop-server`
- [x] Health check: `superchat-mcp-server-health`, `superchat-mcp-check-all-health`
- [x] Per-session server selection: `superchat-mcp-servers` defcustom
- [x] Graceful degradation: tool collection catches per-server errors
- [x] `/mcp` output: per-server status table with health + tool counts

**Resolved open questions**:

- Namespace: `mcp:<server>:<tool>` (colon-separated, readable)
- Auto-start vs explicit: explicit `superchat-mcp-start-server`; `/mcp-start` starts all
- Server-down events: surfaced as incrementally degrading status (tools count drops)

Currently MCP is "zero-config" but limited in practice:

- Single active server lifetime (no orchestration across multiple)
- No tool namespace conflict resolution (two servers both exposing `search_text` collide)
- No persistence of running servers across sessions
- No per-server health checks

**Key deliverables**:

- [x] Multi-server tool namespace: `mcp:<server>:<tool>` via `--prefix-tool`
- [x] Server lifecycle: `superchat-mcp-start-server`, `superchat-mcp-stop-server`
- [x] Health check: `superchat-mcp-server-health`, `superchat-mcp-check-all-health`
- [x] Per-session server selection: `superchat-mcp-servers` defcustom
- [x] Graceful degradation: tool collection catches per-server errors
- [x] `/mcp` output: per-server status table with health + tool counts

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

- [x] Autoload coverage: 13 `;;;###autoload` across 10 files (was 2 before v0.9)
- [x] CI: `.github/workflows/test.yml` exists, runs ERT on Emacs 28.1
- [x] Bumped `(llm "0.7")` → `(llm "0.24")` — actual minimum tested
- [x] Pre-existing test abort resolved: 51/51 green, no failures

- [x] All `defcustom` have `:type` — `superchat-ollama-timeout-multiplier` is a `defvaralias` (false positive). Real defcustom `superchat-tool-timeout-multiplier` has proper `:type 'number`.
- [x] `M-x checkdoc` — critical issues fixed (commit 6c98fa5). Remaining: ~8 style nitpicks (imperative form, two-spaces-after-period). Package-lint not installed.
- [x] Clean up working-tree noise: removed `superchat.el.bak` and `:memory:`; ignored `*.elc`, `*.bak`, `:memory:`, `.omc/`, `.omo/`, `.pi/`, `.claude/` (commit `20fe346`)
- [x] Reconcile `.gitignore`: removed bogus entries for already-tracked `test/`, `docs/`, `AGENTS.md`; added proper ignores (commit `20fe346`)
- [x] Track 19 docs/test files that were hidden by the broken `.gitignore` (commit `4a9ca0d`)

**Open questions**:

- Min Emacs: keep 28.1, or bump to 28.2?
- Add `Cask` / `Eldev` for reproducible dev environment?
- Remove `superchat.elc` from tracked state?

### v1.0 — First MELPA-ready stable release

**Key deliverables**:

- [x] `;; Version: 1.0.1` header in `superchat.el` (patched to 1.0.1)
- [x] Git tags `v1.0.0` and `v1.0.1` on `main`
- [x] `superchat-pkg.el` exists with correct metadata
- [x] README + AGENTS.md reflect v1.0 state (rewritten 2026-06)
- [x] `;; Package-Requires: ((emacs "28.1") (llm "0.24"))` finalized
- [x] `docs/architecture.md` (new, 326 lines) — pipeline + hook architecture
- [x] `docs/SKILLS_QUICKSTART.md` updated with type:workflow
- [x] 3 new example skills: explain-region, git-commit-message, weekly-tech-digest

- [x] CHANGELOG entry — standalone `CHANGELOG.md` created (2026-06-10). Covers v0.2→v1.0.1.
- [x] MELPA submission guide: `docs/MELPA_SUBMISSION.md` created with recipe + checklist. Actual PR to melpa/melpa pending manual submission by maintainer.

### v1.1 (untagged) — tape.systems unification + agent mode — ✅ SHIPPED (2026-06/07, on main)

Source: commits `e583b2c`..`aef7779` (2026-06-21 → 2026-07-01). Shipped
directly to `main` without a tag; included in the v1.2.0 release range.

**Key deliverables** (all shipped):

- [x] Tape schema v3: FTS5 trigram index, `topic` column, View layer (Phase A)
- [x] `/remember` + `/recall` via tape anchors and views (Phase B)
- [x] `/compact` preserves tape entries; `/expand` restores anchors (Phase C)
- [x] SQL + structured tape retrieval tools (Phase D)
- [x] Topic lifecycle hooks and automatic handoff (Phase E)
- [x] Migration command + legacy memory write flag (Phase F)
- [x] Agent loop with tool observability and guardrails (Phase 3)
- [x] Session compaction with tape anchors (Phase 4)
- [x] Sub-agents: researcher / executor / introspector presets (Phase 5)
- [x] `delegate_to_subagent` + `delegate_to_subagent_parallel` tools

### v1.2.0 — Async workflow engine + shared workspace — ✅ SHIPPED (2026-07-12)

Source: `docs/goals/workflow-async-engine.md` + commit `1b794c9`, tag `v1.2.0`.

**Decision reversal**: v0.7 folded workflow into SKILL.md as
`type: workflow` and explicitly rejected a `>>` prefix ("No `>>` syntax").
v1.2 reverses both: the SKILL.md sub-type executor was a placeholder that
never called the LLM, and unified `>name` dispatch created a namespace
collision between skills and workflows. Workflows are standalone
`.workflow` files again, and `>>name` is their dedicated prefix
(`>` stays reserved for skills; `/workflow <name>` is an equivalent entry).

**Key deliverables** (all shipped):

- [x] Async linear executor: steps chain through LLM completion callbacks,
      Emacs never blocks (the historical engine was always synchronous —
      this was never true in any prior version)
- [x] `.workflow` files under `<data-dir>/workflow/`, one step per line
- [x] Cross-step variables: `$result`, `$stepN` (+ `$input`/`$lang`/`$date`)
- [x] Per-step annotations: `@model` (line start), `/command` (expands the
      command's prompt template, args bound to `$input`), `#path` (context
      files, resolved at invocation time; token must look like a path)
- [x] Error handling: sync errors and `[Error: ...]` callback strings both
      stop the chain with a rendered failure notice
- [x] `superchat-workspace.el`: marker-tracked shared region (or fallback
      buffer) + `workspace_read` / `workspace_write` / `workspace_info` tools
- [x] Agent-mode per-tool lifecycle hooks: pre-tool, permission gate
      (allow/deny, deny wins), post-tool, post-tool-failure
- [x] DB migration ordering fix (v2 → v3 no longer aborts on
      "no such column: topic"); `_schema_version` kept to a single row
- [x] llm.el `:context` fix in ob-superchat / magit / rewrite
- [x] Tests 179 → 198; `test-workspace.el` wired into the runner

**Known debt carried forward**:

- 3 pre-existing `ecosystem/lsp-*` test failures (present since v1.0.1)
- Legacy import shim (`superchat-workflow-import-legacy*`) still converts
  `.workflow` → SKILL.md, which is now the deprecated direction
- No branches/conditionals in workflows — linear only, by design for now

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
- 2026-06-04: **Path order reshuffled.** v0.9 monolith split + hook
  alignment shipped first (commits `a17f67e`..`037a7fd`), then v0.8
  SQLite memory facade was leapfrogged ahead of v0.7 (commits
  `bf7cc43`..`7a5d893`). v0.7 (Skills v2) remains the outstanding gap.
- 2026-06-04: ROADMAP v0.7 rewritten. Earlier draft mis-described
  `superchat-skills-standard.el` as "scaffolding only" — it is a
  working v1 implementation. Earlier draft also missed that workflow
  was a real subsystem (`superchat-workflow.el`, 458 lines) deleted
  by `31e4fd3` when skills landed. v0.7 now explicitly restores
  workflow as a `type: workflow` sub-type of the SKILL.md format
  rather than "folding workflow into skills" — those solve different
  problems (single LLM call vs multi-step linear recipe) and shouldn't
  collapse.
- 2026-06-10: **MCP v2 shipped.** `superchat-mcp.el` rewritten 109→326 lines.
  All 6 deliverables: namespace, per-server lifecycle, health checks,
  per-session server selection, graceful degradation, status table.
- 2026-07-12: **v1.2.0 shipped.** Added v1.1 (untagged tape.systems +
  agent mode block, 2026-06-21..07-01) and v1.2.0 milestones. Documented
  the v0.7 decision reversal: workflows moved back out of SKILL.md into
  standalone `.workflow` files with a dedicated `>>name` prefix — the
  "No `>>` syntax" call in v0.7 is superseded.
