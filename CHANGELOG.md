# Changelog

All notable changes to superchat.

## Unreleased (v1.3.2)

### v1.3.2 tool-loop repair

- Fixed the P0 tool-loop gap: llm.el runs a requested tool and appends the
  result to the mutable chat prompt, but the caller must send that prompt
  again. The asynchronous chat, synchronous executor, and asynchronous
  sub-agent paths now do so until the model returns text.
- Added `superchat-llm-max-tool-rounds` (default 12) alongside the existing
  total tool-call guard. At the limit, `superchat-llm-round-limit-action`
  can ask for another budget, send one final request with tools removed, or
  stop. Batch use automatically takes the tool-free final-answer path.
- Tool call/result transcript blocks remain in the chat buffer while the
  final streamed answer is formatted. Sub-agent cancellation follows the
  newest llm.el request, and its timeout pauses while a round-limit prompt is
  awaiting the user's answer.
- Raised the declared runtime floor to Emacs 29.1 (SQLite already required
  it) and llm.el 0.31.1.

### v1.3.1 control plane

- `/agents` lists active sub-agents with id, preset, depth, elapsed time,
  and progress-placeholder position. `/cancel-agent <id>` cancels one run.
- Sub-agent requests retain the object returned by `llm-chat-async` and cancel
  through llm.el's supported `llm-cancel-request` API. Completion, failure,
  manual cancellation, and timeout share one finish-once path.
- `superchat-subagent-timeout` bounds all runs (default 300 seconds); agent
  frontmatter may set a shorter per-agent `timeout`, preserved by import/export.
- Workflow steps now opt into the existing agent-loop tool wrappers, gaining
  the same call counting, confirmation, lifecycle hooks, rendering, and tape
  behavior as normal agent turns.
- `eval-elisp` is now destructive by default. Introspector and custom agent
  skills must pass the same confirmation/permission gate before evaluating
  arbitrary Elisp, preventing a shared skill from disabling global guardrails.

### Agent registry and profiles

- Delegation tool descriptions are generated from the live agent registry:
  the three built-ins remain first, and custom `type: agent` SKILL.md files
  are discoverable by name and description without editing Lisp. Prompt and
  workflow skills are excluded from delegation.
- Agent presets now carry typed `temperature`, `max_tokens`, and `reasoning`
  settings through main-agent, synchronous sub-agent, and asynchronous
  sub-agent calls to `llm-make-chat-prompt`. Invalid values warn and inherit.
- Per-agent `max_tool_calls` and `confirm_destructive` guardrails apply to
  both main and delegated agents. They are intentionally tighten-only:
  effective limits use `min(global, profile)` and confirmation uses
  `global OR profile`, so a shared SKILL.md cannot weaken global policy.
- Legacy synchronous sub-agent runs now receive an isolated buffer-local
  tool-call counter instead of sharing state with the caller.

### Preset runtime contract (agent-profiles Phase 1)

- **System prompts now actually reach the LLM.** `turn.system-prompt`
  was a dead-end slot: hooks populated it (language instruction, tool
  guidance) but nothing sent it to the provider. The pipeline is now
  wired end to end — `superchat--build-llm-prompt` gained a `:context`
  parameter, both `superchat--llm-generate-answer` variants accept a
  system prompt, and the dispatcher, agent runs, and both sub-agent
  paths pass it through.
- **Agent/plan preset bodies are personas.** `superchat-preset-apply`
  prepends the skill body to the turn's system prompt for `type: agent`
  and `type: plan` presets, so the persona reaches every turn —
  including `/agent`-mode follow-ups and delegated sub-agents, which
  previously never saw it. Explicit `>skill` invocation no longer also
  embeds the body in the user prompt (it would have been sent twice).
- **Preset models reach the effective backend.**
  `superchat--execute-llm-query` falls back to the turn's target-model,
  fixing both the dispatcher ordering bug (model was captured before
  preset application) and the sub-agent path (which calls with no
  explicit model). Precedence: an explicit per-turn `@model` wins;
  the preset fills the slot only when empty.
- **`tools: []` means zero tools.** An explicitly empty tools list in
  SKILL.md frontmatter now parses to a `none` sentinel (absent key
  still inherits the global tool set) and propagates through preset →
  turn → tool collection.
- **`:backend` preset field removed.** It parsed and stored but had no
  runtime consumer; a `backend:` key in frontmatter is now ignored.
- Tests: 211 → 228; new `test-preset-contract.el` locks all of the
  above end to end.

## 1.2.1 (2026-07-12, untagged)

### Async sub-agent engine

- `superchat--subagent-run-async`: sub-agents now run through async
  llm.el callbacks — Emacs stays interactive during delegation. The
  sync runner is kept as a legacy path.
- `superchat--subagent-run-parallel-async` replaces the `make-thread`
  implementation: requests run as concurrent llm.el calls (separate
  curl processes), so parallel delegation is genuinely parallel;
  `superchat-subagent-parallel-max` is now a launch window with
  queueing. Results aggregate in spec order regardless of completion
  order.
- Explicit execution context replaces dynamic bindings (which don't
  survive async callbacks): session id, depth, and the tool-call
  counter travel through closures. Sub-agent tool calls are gated by
  the same permission hooks / confirmation as the main agent loop and
  logged to the sub-agent's own tape; tool errors return as strings
  instead of re-signaling.
- Delegation depth guard: `superchat-subagent-max-depth` (default 1) —
  at the limit, delegate tools degrade to a denial stub; below it,
  nested delegation re-enters at depth + 1.
- Progress placeholders: `/subagent` and the delegate tools insert a
  "⏳ running…" line immediately and replace it in place with the
  report on completion; concurrent placeholders coexist via markers.
- `delegate_to_subagent` / `delegate_to_subagent_parallel` are now
  registered as async llm.el tools; `/subagent` delegates without
  blocking.
- Tests: 202 → 211 (async runner, spec-order aggregation, launch
  window, error isolation, depth guard, placeholder lifecycle).

## 1.2.0 (2026-07-12)

### Workflow engine (rewritten)

- Standalone `.workflow` files restored under `<data-dir>/workflow/`, with a
  fully **asynchronous** linear executor — steps chain through LLM completion
  callbacks, Emacs never blocks. Replaces the v0.7 SKILL.md `type: workflow`
  placeholder that never actually called the LLM.
- New `>>name [args]` prefix invokes workflows (`>` stays reserved for
  skills); `/workflow <name> [args]` is an equivalent entry point.
- Cross-step variables: `$result` (previous step), `$stepN` (any step),
  plus `$input` / `$lang` / `$date`.
- Per-step annotations: `@model` one-shot override (line start),
  `/command` expands the command's prompt template with args bound to
  `$input` (line start), `#path` context files (token must contain `.`
  or `/`; resolved once at invocation time).
- LLM error responses stop the chain with a rendered failure notice
  instead of leaking into the next step's `$result`.
- Sample recipe: `examples/research.workflow`.

### Shared workspace (new)

- `superchat-workspace.el`: designate a highlighted region as a shared
  state area (`superchat-workspace-set-region`); markers track buffer
  edits. Falls back to an auto-created `*superchat-workspace*` buffer.
- Three new LLM tools: `workspace_read`, `workspace_write`,
  `workspace_info` — main agent and delegated sub-agents coordinate
  through them.

### Agent mode

- Per-tool lifecycle hooks: `superchat-agent-pre-tool-functions`,
  `superchat-agent-permission-functions` (programmatic allow/deny gate,
  deny wins), `superchat-agent-post-tool-functions`,
  `superchat-agent-post-tool-failure-functions`.

### tape.systems unification + sub-agents (untagged v1.1 block, 2026-06/07)

- Tape schema v3: FTS5 trigram index, `topic` column, View layer.
- `/remember`, `/recall`, `/compact`, `/expand` rebuilt on tape anchors.
- SQL + structured tape retrieval tools; topic lifecycle hooks.
- Agent loop with tool observability and guardrails; session compaction.
- Sub-agent presets (researcher / executor / introspector) and
  `delegate_to_subagent` / `delegate_to_subagent_parallel` tools.

### Fixes

- DB schema: migrations now run before creating indexes/FTS tables that
  depend on migrated columns — v2 databases previously aborted the upgrade
  with "no such column: topic" and were left without `tape_fts`.
  `_schema_version` uses DELETE+INSERT to keep a single authoritative row.
- `ob-superchat` / `superchat-magit` / `superchat-rewrite`: system prompts
  go through llm.el's `:context` instead of overwriting `interactions`
  with a plain plist (which dropped the user message).
- Workflow parser: inline prose (`@john.doe`, `#42`, `foo/bar`) is no
  longer eaten as annotations; `$step10` is no longer corrupted by the
  `$step1` substitution.
- Version metadata: `;; Version:` header and the stale
  `superchat-version` defconst (was "0.5") both bumped to 1.2.0.

### Tests

- Suite: 179 → 198. `test-workspace.el` wired into `run-tests.el`
  (previously never ran); the always-true async error test replaced with
  real sync-error and error-string chain tests; new regressions for
  `$step10`, inline annotation parsing, command template expansion, and
  workspace replace semantics.

## 1.0.1 (2026-06)

### Documentation

- **README rewrite**: 569→296 lines. Replaced gptel/org-ql era content with
  hook-pipeline positioning, comparison table, 5-minute tour, Core Concepts,
  and honest limitations section.
- **README_cn.md rewrite**: 557→296 lines. Same structure, native Chinese.
- **`docs/architecture.md`**: new 326-line technical companion covering
  pipeline, turn struct, hook signatures, turn lifecycle, dispatcher,
  command/skill extension, and non-pluggable gaps.
- **`docs/SKILLS_QUICKSTART.md`**: updated to canonical SKILL.md format
  with `type: workflow` section.
- **3 new example skills**: `explain-region` (type: prompt),
  `git-commit-message` (type: workflow), `weekly-tech-digest` (type: workflow).
- **AGENTS.md**: fixed stale facts (file count 17, llm 0.24, SQLite memory,
  gptel migration complete).

## 1.0.0 (2026-06)

Tagged. `superchat-pkg.el` created.

## 0.9 (2026-06) — Stabilization pass

### Monolith split

- `superchat.el` split from 2589→~1548 lines into 6 focused modules:
  `superchat-models.el`, `superchat-save.el`, `superchat-mcp.el`,
  `superchat-render.el`, `superchat-llm.el`, `superchat-dispatcher.el`.

### Hook pipeline alignment

- `superchat--build-final-prompt` deleted. Replaced by 5 focused hook functions
  in `superchat-prompt-hooks.el`.
- Prompt building now runs exclusively through `superchat-core-run-turn` hooks
  (system-prompt → build-prompt → post-turn).

### Infrastructure

- **Autoload coverage**: 13 `;;;###autoload` across 10 files.
- **CI**: `.github/workflows/test.yml` added.
- **llm minimum**: bumped to `0.24` (was `0.7`).
- **Working-tree hygiene**: removed `superchat.el.bak` and `:memory:`.
- **`.gitignore`**: reconciled bogus entries; tracked 19 hidden docs/test files.

### Known gaps (deferred to v1.1)

- `superchat-ollama-timeout-multiplier`: `defvaralias` false positive in `:type` check.
- `checkdoc` and `package-lint` never run.

## 0.8 (2026-06-04) — SQLite memory facade

- **The pivot**: replaced the Org-mode memory store with a thin compatibility
  facade over `superchat-db.el` (SQLite + FTS5).
- Rewrote `superchat-memory.el` as a facade over `superchat-db` (2516 → 337 lines).
- Rewrote test suite: deleted org-ql tests (16), audited soul tests (33 → 8),
  expanded facade tests (2 → 13).
- One-shot migration `M-x superchat-memory-import-from-org`.
- Soul.org kept as dual-track historical archive (not actively written).
- LLM keyword enrichment deleted (FTS5 handles most recall).
- `superchat-memory-synthesize-soul` retired.

### MCP v2 — deferred

The planned MCP v2 multi-server orchestration was deferred to v1.1.
Current `superchat-mcp.el` (109 lines) only handles basic start/stop/tool collection.

## 0.7 (2026-06) — Skills v2: standard format + workflow restoration

### SKILL.md format unification

- In-repo `skills/*.md` files migrated to canonical SKILL.md frontmatter
  (`name`/`description`/`version`/`type`/`triggers`).
- `superchat-skills-standard--parse-frontmatter` with graceful missing-field
  handling.

### Workflow restoration (`type: workflow`)

- `superchat-workflow.el` revived as step-by-step executor.
- SKILL.md `type: workflow` dispatches to step executor rather than single
  LLM prompt.
- Legacy `.workflow` file import shim (auto-synthesize SKILL.md on load).

## 0.6 (2026-06-01) — Memory-Soul dual-track

- **Memory-Soul separation**: two distinct tracks — synthesized `memory.org`
  (knowledge, scored, decayed) and raw-event `soul.org` (verbatim, mood,
  context, never auto-merged).
- **`superchat-memory-add-raw`**: with `:mood`, `:context`, `:verbatim`
  keyword args. No LLM required.
- **Contradiction coexistence**: `:CONTRADICTION:` tag, `:VALIDITY: expired`,
  `:REPLACED_BY:` link. Contradictions kept, not merged.
- **Manual review UI**: `superchat-memory-review-mode` (y/n/e/s/q keybindings).
- **Mood tag taxonomy**: `defcustom superchat-memory-mood-taxonomy`.
- **New defcustoms**: `superchat-memory-soul-file`,
  `superchat-memory-soul-synthesis-mode`,
  `superchat-memory-contradiction-context-window`,
  `superchat-memory-mood-taxonomy`.
- **23 new tests** in `test/test-memory-soul.el`.

## 0.5 (2026-05) — Backend hard-swap: gptel → llm.el

- **BREAKING**: gptel replaced by [llm.el](https://github.com/ahyatt/llm) (GNU ELPA, 0.24).
- **Emacs 28.1 required** (was 27.1).
- **Removed** `superchat-agent.el`.
- **Removed** gptel-specific `:around` advice on `gptel-curl--stream-cleanup`.
- **New defcustoms**: `superchat-llm-backend`, `superchat-llm-model`,
  `superchat-llm-streaming`, `superchat-manual-models`.
- **New `/backend` command** (replaces `/tools`; alias kept).
- **Built-in tool registry**: 13 tools via `llm-make-tool`.
- Test suite rewritten: 9 gptel test files deleted; new `test/test-llm-backend.el`.

## 0.4 (2025-10-13)

- **Workflow Integration**: workflows run inside superchat, sharing the tool stack.
- **Tool Output Hardening**: sanitized workflow results for rich Markdown/HTML.
- **Utility Additions**: `superchat-version` constant, expanded tool tests.

## 0.3 (2025-10-03)

- **MCP Integration**: zero-config MCP server detection, real-time status,
  tool integration via `/mcp` and `/mcp-start` commands.
- **gptel Tools Integration**: zero-config tool calling via gptel's registry.
- **@ Model Switching**: switch models mid-conversation with `@model` syntax.
  `/models` command for available model list.

## 0.2 (2025-09-23)

- **Memory System**: persistent AI memory with tiered capture, LLM-powered
  keyword extraction, scoring/decay, and auto-merge of similar memories.
- **Bug Fixes**: stability and compatibility improvements.
