# AGENTS.md — Superchat (Emacs Lisp)

A standalone AI chat client for Emacs. Single package, 7 `.el` files, no MELPA packaging yet. Backend is migrating `gptel` → `llm` (see quirks #8).

> Note: this `AGENTS.md` is **gitignored** (see `.gitignore` line 7) but **tracked** — there is no `superchat-pkg.el`, no `Cask`, no `.dir-locals.el`, no `.editorconfig`, and no `.github/` CI. Treat it as a per-machine agent hint, not a published file.

## What this package is

- Interactive chat UI for LLMs in an Org-mode buffer (`M-x superchat`).
- Hooks into `llm` (ahyatt, GNU ELPA) for the LLM transport (post-v0.5). Pre-v0.5 used `gptel` (karthink). Does NOT bundle the backend.
- Three user-facing prefix parsers (in `superchat-parser.el`):
  - `/name` — slash commands (built-in + user prompts in `<data>/command/`)
  - `>name` — Agentic Skills (loads `skills/<name>.md` as context)
  - `#path` — attach a file as context
  - `@model` — switch model mid-conversation
- Memory subsystem stores Org-mode records (`superchat-memory.el`).
- Tool subsystem provides a few native web/Jina fetchers (`superchat-tools.el`).

## File map (do not rename casually)

| File | Lines | Role |
|---|---|---|
| `superchat.el` | ~2200 | Main entry, `M-x superchat`, `superchat-mode` minor mode, gptel advice workaround. Only file with `;; Version:` / `Package-Requires:` headers. |
| `superchat-memory.el` | ~1780 | Org-based memory: tiered capture, scoring, decay, optional `org-ql` cache. |
| `superchat-skills.el` | ~730 | The `>skill-name` Agentic Skills system. Loads `skills/<name>.md`. |
| `superchat-tools.el` | ~500 | Native tools (url/jina fetchers) registered into gptel. |
| `superchat-skills-standard.el` | ~240 | Convert OpenAI/Anthropic `SKILL.md` format ↔ internal. |
| `superchat-executor.el` | ~230 | Prompt execution engine (var substitution, context, LLM call). Used by `superchat-skills.el`. |
| `superchat-parser.el` | ~80 | Pure parsers for `@`, `/`, `#`. No superchat deps. |

All files use `lexical-binding: t`. Internal functions are prefixed `superchat--` (double dash) — keep this convention.

## Run tests

The project uses **ERT** (Emacs built-in). No Cask, no Makefile, no CI.

```bash
# Canonical skills tests (the only entry point with a runner):
emacs -Q -l test/run-tests.el

# If dependencies are installed in your load path, single-file ERT also works:
emacs -Q -L . -l test/test-skills.el -f ert-run-tests-batch-and-exit

# Memory + org-ql tests (requires org-ql and gptel):
emacs -Q -L . -l test/superchat-memory-org-ql-tests.el -f ert-run-tests-batch-and-exit
```

`test/run-tests.el` only loads `test-skills.el` and `test-skills-integration.el` — most other `test/test-*.el` and `test/*-test.el` files are **ad-hoc scripts** and not part of the regular suite. They live in a directory that is in `.gitignore` but is actually tracked (see quirks below).

## Build / install

There is no build step. To install locally for development:

```elisp
(add-to-list 'load-path "/path/to/superchat")
(require 'superchat)
```

Data dir is created automatically on first `M-x superchat` at `~/.emacs.d/superchat/` (or `superchat-data-directory` if customized).

## Repo quirks (these WILL bite an agent)

1. **`.gitignore` is partly stale.** It lists `AGENTS.md`, `test/`, `docs/`, `ClAUDE.md`, `SUPERCHAT_PLAN.md`, `.kilocode/`, `.vscode/` — but the existing `test/`, `docs/`, and `docs/AGENTS.md` ARE in git (`git ls-files` confirms). The ignore is ineffective for files that were `git add -f`'d. If you create a new file matching one of those ignore patterns, it will be silently ignored unless you `git add -f`.

2. **Two "Agentic Skills" namespaces — keep them separate.**
   - `skills/` at the repo root = **in-Emacs superchat skills** consumed by the chat UI via `>skill-name`. These are markdown prompt text. (`code-review.md`, `planning.md`, `refactor.md`.)
   - OMC / Claude / OpenCode `skill` tooling is unrelated; this repo has no `.omc/skills/` and no project-local OMC skills.
   - `superchat-skills-standard.el` is the OpenAI/Anthropic `SKILL.md` import/export layer — not OMC.

3. **Do not remove the `gptel-curl--stream-cleanup` `:around` advice at the bottom of `superchat.el` (lines ~2181-2204)** *unless* you are also doing the v0.5 backend swap. It works around an upstream gptel bug where a non-200 response causes a bare `search-backward` to fail in the process sentinel, leaving the FSM stuck and the user callback unfired. The advice becomes unnecessary after the llm.el migration because plz doesn't have the same bug.

4. **Only `superchat.el` has a `;; Version:` / `;; Package-Requires:` header.** Other files rely on `superchat.el` to be the entry point. Do not duplicate these headers elsewhere.

5. **The `docs/AGENTS.md` file is gitignored and is a Chinese-language design journal** about the agentic-skills migration (workflow → skills). It is not an instruction file for OpenCode agents — do not treat it as such. Source of truth for current behavior is the `.el` files and `README.md`.

6. **Worktree state**: the working tree contains a large staged deletion of `threadnote-mvp/` (a Swift project). That directory is unrelated historical work being removed — do not resurrect it.

7. **Branch is `main`** (not `master`).

8. **v0.5 shipped, v0.6+ planned** (see `ROADMAP.md` for the full post-v0.5 plan). The gptel → llm.el hard swap is complete (commits `19bb2d8` + `890e561`). `superchat-agent.el` was removed. Min Emacs is 28.1; new dep `(llm "0.7")`. The `:around` advice on `gptel-curl--stream-cleanup` was removed in v0.5. Next milestone: v0.6 (Memory-Soul dual-track). If you see a `git diff` mixing gptel and llm code, it's an old branch — v0.5 already merged.

## Public API (autoloaded)

Only two `;;;###autoload` declarations exist, both in `superchat.el`:

- `M-x superchat` — open/switch to the chat buffer (in `org-mode` with `superchat-mode` minor mode).
- `M-x superchat-ensure-directories` — create the data dir tree.

In-chat commands (registered as `superchat-` interactive defuns, not autoloaded because they need the buffer to exist): `superchat-send-input` (`C-c C-c`), `superchat--list-commands` (`C-c C-h`), `superchat--save-conversation` (`C-c C-s`), plus slash commands like `/define`, `/commands`, `/tools`, `/mcp`, `/mcp-start`, `/clear-context`, `/models`. Post-v0.5: `/tools` is replaced by `/backend` (shows active llm provider/model) and `/models` is re-pointed at the llm provider's `:chat-model` enumeration.

## Style / conventions

- `lexical-binding: t` everywhere.
- Internal helpers: `superchat--name` (double dash). Public: `superchat-name` (single dash). Stick to it.
- Optional dependencies: `(require 'foo nil t)` so the package loads even if llm/mcp/org-ql are absent. Pre-v0.5 also allowed gptel/gptel-agent (now removed).
- llm and mcp functions are `declare-function`'d at the top of the file that uses them.
- No formatter config; no linter config. Match the surrounding file when editing.

## Configuration entry points

Top-level defcustoms in `superchat.el`:
- `superchat-buffer-name` (default `*superchat*`, lowercase)
- `superchat-data-directory`, `superchat-default-directories`
- `superchat-lang` (used in custom-prompt `$lang`)
- `superchat-response-timeout` (default 120s; protects against blocking tools)
- `superchat-completion-check-delay` (default 2s; used for Ollama+tools)
- `superchat-display-single-window` (default `t`)
- `superchat-context-message-count`, `superchat-conversation-history-limit`

Memory tuning lives under `defgroup superchat-memory` (customize via `M-x customize-group RET superchat-memory RET`).

## When in doubt

- README is bilingual (`README.md` EN, `README_cn.md` ZH). Both are kept in sync; prefer the code.
- `examples/standard-skills/` shows a third-party skill directory layout that `superchat-skills-standard.el` can import.
- `test/SKILL_TESTING.md` is a manual testing guide for the skills feature, not a script.
