# AGENTS.md — Superchat (Emacs Lisp)

An Emacs-native chat and Agent Runtime.  It is one package split across
top-level `.el` modules; `llm` (ahyatt, GNU ELPA) is the model transport.
The gptel → llm migration completed in v0.5.

> This is a repository navigation guide, not the product specification.
> `superchat.el` carries the current package header; `superchat-pkg.el` is the
> generated/installation descriptor.

> **SPEC-AGENTS is installed in this repository.** Its doctrine — the file the
> framework calls `AGENTS.md` — lives at [`SPEC-AGENTS.md`](SPEC-AGENTS.md),
> because this file already owns that name. Its six action skills live in
> `.claude/skills/{plan,capture,arrange,do,check,learn}/`, not in the repo-root
> `skills/`, which is superchat's own bundled Agentic Skills directory.
> Entry point for a start review: [`START.md`](START.md).

## What this package is

- Interactive chat UI and Runtime coordinator inside Emacs (`M-x superchat`).
- Tape-backed Turn/Run lifecycle with Authority-gated tools, replayable
  Projections, Closure judgments, Workspaces, Workflows, and Subagents.
- Hooks into `llm` for model transport; it does not bundle a provider backend.
- User-facing prefix syntax (parsed by Core/parser/skills modules):
  - `/name` — slash commands (built-in + user prompts in `<data>/command/`)
  - `>name` — Agentic Skills (loads `skills/<name>.md` as context)
  - `>>name` — async linear `.workflow` recipes
  - `#path` — attach a file as context
  - `@model` — switch model mid-conversation
- Memory and Workspace are Tape consumers backed by SQLite/FTS and Projection.
- Every model-invoked tool crosses the shared Authority execution seam.

## File map (do not rename casually)

Line counts are intentionally omitted: navigate by layer and ownership, not
file size.

### Kernel (the nine files, headless-loadable)

| File | Role |
|---|---|
| `superchat-core.el` | Turn data and pure request/prompt preparation; it does not own execution completion. |
| `superchat-preset.el` | Prompt/Agent Profile configuration, stable profile identity, Capability defaults, and runtime knobs. |
| `superchat-db.el` | Physical SQLite implementation for Tape rows, projection checkpoints, Memory, and FTS. |
| `superchat-tape.el` | Logical Runtime event interface: stable event identity, causation, ordering, redaction, and replay. Calls DB for persistence. |
| `superchat-projection.el` | Generic reducer/checkpoint seam for replayable and materialized read views. |
| `superchat-closure.el` | Pure Tape interpreter answering whether a Runtime claim is proved/open/unknown/invalid. It stores no state. |
| `superchat-authority.el` | Shared tool execution seam: validate → effects → policy → approval → execute → Tape. |
| `superchat-runtime.el` | Unique owner of Request → Turn/Run lifecycle, terminal state, delivery, and public Run status. |
| `superchat-llm.el` | `llm.el` transport adapter: request, streaming, cancellation, normalized result. |

### Middle tier

| File(s) | Role |
|---|---|
| `superchat-dispatcher.el` | Surface input, command routing, and Runtime Request construction. |
| `superchat-agent-loop.el` | Compatibility/instrumentation around tool-loop behavior; Runtime still owns lifecycle and Authority owns execution. |
| `superchat-executor.el` | Pure prompt substitution, context, and result helpers retained for Skills compatibility; no Run lifecycle or model transport. |
| `superchat-extension.el` | Activation-scoped contributions: tools, context, knowledge, commands, and policies. |
| `superchat-synthesis.el` | User-gated Agent-authored tools injected into the current tool loop. |
| `superchat-memory.el` | Memory Projection facade, review/correction events, and recall. |
| `superchat-workspace.el` | Workspace/Artifact registry, revisions, Projection, and shared editing tools. |
| `superchat-experience.el` | Post-Run evaluator observations, conditional relations, and human Candidate dispositions. Its task View promotes at most one Candidate, never as universal truth. |
| `superchat-claims.el` | Claim and human Feedback capture over durable Tape references; derives a replay-only Current View and does not promote into Memory. |
| `superchat-tape-view.el` | Read-only operational views over persisted Tape data. |
| `superchat-tape-browser.el` | Interactive face for `superchat-tape-view.el` queries: session list, entry list, and reconstructed chat View. |
| `superchat.el` | Main UI, configuration, Session View, commands, and package header. |
| `superchat-render.el`, `superchat-prompt-hooks.el`, `superchat-parser.el` | Rendering, prompt enrichment, and pure token parsing. |
| `superchat-tools.el` | Built-in tool descriptors/effect resolvers and plugin registration primitives. |
| `superchat-skills.el`, `superchat-skills-standard.el` | Skill resolution plus standard `SKILL.md` import/export. |
| `superchat-models.el`, `superchat-save.el` | Model selection/cache and conversation export. |
| `superchat-plan.el` | Structured plan lifecycle: propose, review, approve, and execution progress. Plan data is the source of truth; the side-window frame is its projection. |
| `superchat-widgets.el` | Shared panel UI primitives: right-side-window placement, per-panel-kind slot allocation, and push-button element construction. |
| `superchat-mcp.el` | MCP v2 multi-server orchestration: per-server lifecycle, health checks, `mcp:<server>:<tool>` namespace disambiguation, and graceful degradation. |
| `superchat-pkg.el` | Package installation descriptor. |

### Plugins / integrations (the eight, loaded on demand)

| File(s) | Role |
|---|---|
| `superchat-rewrite.el` | Deterministic region rewrite: instruct, preview the diff with ediff, apply or reject. No tool-calling; fenced code blocks are parsed from the response. |
| `superchat-compact.el` | Async Session compaction and anchor lifecycle. |
| `superchat-workflow.el` | Async linear recipe engine; Closure-gated step handoff from durable Tape history. |
| `superchat-subagent.el` | Child Actor/Run orchestration through the same Runtime and Authority seams. |
| `superchat-agents-panel.el`, `superchat-genui.el` | Optional Actor UI and generated-VUI validation/rendering. |
| `superchat-magit.el` | Magit hook generating conventional-commit messages from the staged diff via a blocking LLM call. Soft-depends on Magit; registers its command only when Magit is loaded. |
| `ob-superchat.el` | Org-babel `superchat` source blocks: the body is sent as a query and the result is written to `#+RESULTS:`. |

Only the declared synchronous boundary adapters may use blocking Runtime calls because their host contracts require synchronous results: `superchat-runtime.el`, `ob-superchat.el`, `superchat-magit.el`, and `superchat-rewrite.el`. The `runtime/blocking-submit-is-limited-to-sync-adapters` test checks that every other `.el` file contains zero `superchat-runtime-submit-blocking` calls.

## Module layering (enforced by tests)

The three tiers are:

- **Kernel — exactly these nine files:** `superchat-core.el`,
  `superchat-preset.el`, `superchat-db.el`, `superchat-tape.el`,
  `superchat-projection.el`, `superchat-closure.el`,
  `superchat-authority.el`, `superchat-runtime.el`, `superchat-llm.el`.
  A kernel module may only `require` other kernel modules. The kernel must stay
  loadable headless — no UI, no chat distribution, and no integration package
  pulled in from below. The ERT assertion is
  `runtime/kernel-requires-stay-inside-kernel` in `test/test-runtime.el`.
- **Plugins / integrations — exactly these eight features:**
  `superchat-rewrite`, `superchat-compact`, `superchat-workflow`,
  `superchat-subagent`, `superchat-agents-panel`, `superchat-genui`,
  `superchat-magit`, `ob-superchat`.
  No file outside that list may top-level `(require 'PLUGIN)`. Only another
  plugin may require one; for example, the agents panel requires the subagent
  module it visualizes. Integration packages load on demand, never via a
  top-level require. Everything else reaches them through autoloads and the
  kernel registries. The ERT assertion is
  `runtime/eager-distribution-does-not-require-integrations` in
  `test/test-runtime.el`.
- **Middle tier — every other `.el` file.** Middle-tier modules may require
  kernel and other middle-tier modules, but may not require a plugin.
  `superchat-widgets.el` is the clearest example: it is required by
  `superchat-plan.el`, which is middle tier, so `superchat-widgets.el` could
  not be registered as a plugin without breaking the second gate; it is UI,
  so it could not be kernel either.

When adding a module, pick the tier first. A new plugin must also be added to
the feature-name list inside
`runtime/eager-distribution-does-not-require-integrations`; otherwise the gate
rejects the first file that requires it.

The adjacent process-pump invariant is enforced by
`runtime/blocking-pumps-stay-behind-declared-seams` in `test/test-runtime.el`:
`accept-process-output` is allowed in exactly two owners,
`superchat-runtime.el` and `superchat-subagent.el`, and must occur zero times
everywhere else.

## Names that look interchangeable but are not

- **DB vs Tape:** DB is the SQLite physical implementation. Tape is the logical
  event interface and owns event identity, causation, ordering, redaction, and
  replay semantics. Product code should prefer Tape unless it is implementing
  a storage/query adapter.
- **Projection vs Closure:** Projection folds events into reusable read views
  such as Memory, Session, Team, or Workspace state. Closure interprets Tape
  against contract v0 and answers “can this Runtime claim be proved?” Closure
  does not replace Projection and is not a state store.
- **Agent Loop vs Runtime:** Agent Loop is a compatibility and instrumentation
  wrapper around model/tool-loop behavior. Runtime alone creates, completes,
  cancels, and exposes Runs/Turns.
- **Executor vs Runtime:** Executor contains legacy-compatible prompt/context
  helpers used by Skills. It does not call the model or participate in Run
  lifecycle; executable work goes through Runtime.

## Tape / tape.systems 领域约定

- `tape.systems` 在本项目中指 Entry / Anchor / View / Handoff 组成的历史与
  上下文模型，不要把它和 `kind = "system"` 的某一类 Tape 行混为一谈。
- Tape 的逻辑合同是 **append-only**。已经进入 Tape 的 Entry 是不可变事实；
  摘要、修正、反馈、判断和 Anchor 都必须追加成新 Entry/Event。产品代码不得
  通过 Tape 接口覆盖或删除旧 Entry。
- Anchor 是追加到 Tape 的“提炼卡片”和回溯标记，不是删除点。`/compact`
  追加 Anchor 后，只会折叠当前 buffer 的内存会话历史；原始 Tape Entry 仍然
  存在，并可依靠 `source_ids` 由 `/expand` 找回。
- View 是为某个用途临时选择、组合和渲染 Tape Entry 的读模型。View 可以少看
  一些原始 Entry，也可以展示 Anchor 或其他派生结果，但这不等于 Tape 中的
  原始内容被替换。
- Projection 可以把 Tape 折叠成“当前状态”，物化表和 checkpoint 也可以更新
  或重建；它们是可再生的读模型，不是对 Tape 历史的修改。Workspace 文件等
  Resource 可以发生变化，但描述这些变化的 Runtime 证据仍应追加到 Tape。
- 禁止笼统地说“原始 Tape 内容已被摘要替代”。准确说法是：“当前 View 让
  某个派生项在本次上下文中代表一段原始 Entry；原始 Entry 仍保留在 Tape。”
- 如果 View 声明某段原始范围已被 `:represented`，必须能指出“哪个派生项对应
  哪些原始 Entry/range”的来源关系。这是在审计 View 的取材说明，不是在证明
  Tape 发生了替换。当前 Continuity 生产路径只产生 `raw-fact`，并固定输出
  `:represented []`；非空 representation 仍是合同边界，不是已交付行为。

## Run tests

The project uses **ERT** (Emacs built-in). No Cask, no Makefile, no CI.

```bash
# Canonical full suite:
emacs -Q --batch -L . -l test/run-tests.el

# If dependencies are installed in your load path, single-file ERT also works:
emacs -Q --batch -L . -l test/test-skills.el -f ert-run-tests-batch-and-exit
```

`test/run-tests.el` loads the canonical Skills, Runtime, Tape, Closure,
Authority, Projection, Workspace, Workflow, Subagent, extension, UI, and
integration tests.  A few historical/debug scripts under `test/` remain
outside that runner; do not infer suite membership from the filename alone.

## Repo quirks (these WILL bite an agent)

1. **Two "Agentic Skills" namespaces — keep them separate.**
   - `skills/` at the repo root = **in-Emacs superchat skills** consumed by the chat UI via `>skill-name`. These are markdown prompt text. (`code-review.md`, `planning.md`, `refactor.md`.)
   - Codex/OMX/Claude skill tooling is unrelated to that product directory.
   - `superchat-skills-standard.el` is the OpenAI/Anthropic `SKILL.md` import/export layer — not OMC.

2. **The gptel-curl--stream-cleanup `:around` advice was removed in v0.5.** The llm.el transport (plz-based) does not have the upstream sentinel bug it was working around. Do not reintroduce it.

3. **Package metadata currently has two physical representations.**
   `superchat.el` is authoritative for the current Version/Package-Requires
   header; `superchat-pkg.el` is the installation descriptor and can lag during
   development. Do not copy package headers into ordinary modules.

4. **`docs/AGENTS.md` is a historical Chinese design journal**, not an
   instruction file. Source of truth for current behavior is the `.el` files,
   tests, and current architecture docs.

5. **The worktree is often intentionally dirty.** Preserve unrelated edits and
   inspect focused diffs; never restore files merely because status is noisy.

6. `ROADMAP.md` preserves historical milestones and may describe older floors.
   `superchat-agent.el` remains deleted; if a diff mixes gptel and llm code, it
   is stale work because the hard swap already merged in v0.5.

7. **`emacs -Q` and `--batch` do not activate installed packages.** Do not
   conclude that `llm` is absent from a bare `locate-library` result. For an
   isolated transport check, set the temporary `package-user-dir` and call
   `package-initialize` before `require`; add a pinned source checkout with
   `-L` before the second evaluation. This verified shape loads llm 0.31.1 and
   `llm-ollama` without loading the user's init file:

   ```sh
   emacs -Q --batch \
     --eval '(progn (require '\''package) (setq package-user-dir "/tmp/elpa") (package-initialize))' \
     -L /path/to/pinned/llm \
     --eval '(progn (require '\''llm) (require '\''llm-ollama))'
   ```

   The temporary package directory must already contain llm's dependencies
   (`compat`, `plz`, `plz-event-source`, and `plz-media-type`).  `-L` changes
   `load-path` after startup, so it is suitable for an explicit later
   `require`, not for an init file that `-Q` intentionally skips.

8. **Inline widget.el controls in the chat buffer: see the
   `emacs-widget-controls` skill** for the full how-to (explicit
   `:keymap widget-keymap` required on Emacs 31, no
   `widget-button-pressed-hook`, `:notify` deferral, keymap-resolution
   tests).  Superchat-specific: plan review/progress live in a TextUI
   side window (`superchat-plan.el`), never inline — a bare element
   list renders as one inline row there, so frames are a one-element
   list wrapping a column flex.

## Important interfaces

- User entry: `M-x superchat`; interactive integrations expose their own
  autoloaded commands. Use `rg -n ";;;###autoload" --glob '*.el'` for the
  current command surface instead of maintaining a count here.
- Runtime: `superchat-runtime-submit`, `superchat-runtime-submit-blocking`,
  `superchat-runtime-status`, and `superchat-runtime-cancel`.
- Tape: `superchat-tape-append-event`,
  `superchat-tape-append-event-record`, and `superchat-tape-replay-events`.
- Closure: the single pure interface `superchat-closure-judge`.
- Projection: registration/replay/catch-up functions in
  `superchat-projection.el`; concrete projections remain owned by their domain
  modules.
- Plugin primitives: tool/command/policy registration plus declarative
  activation in `superchat-extension.el`.

## Style / conventions

- `lexical-binding: t` everywhere.
- Internal helpers: `superchat--name` (double dash). Public: `superchat-name` (single dash). Stick to it.
- Optional dependencies: `(require 'foo nil t)` so the package loads even if llm/mcp/org-ql are absent. Pre-v0.5 also allowed gptel/gptel-agent (now removed).
- llm and mcp functions are `declare-function`'d at the top of the file that uses them.
- No formatter config; no linter config. Match the surrounding file when editing.

## When in doubt

- README is bilingual (`README.md` EN, `README_cn.md` ZH). Both are kept in sync; prefer the code.
- `examples/standard-skills/` shows a third-party skill directory layout that `superchat-skills-standard.el` can import.
- `test/SKILL_TESTING.md` is a manual testing guide for the skills feature, not a script.
