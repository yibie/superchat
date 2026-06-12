# MELPA Submission Guide

## Checklist

- [x] `superchat-pkg.el` with correct metadata (version 1.0.1, llm 0.24)
- [x] 13 `;;;###autoload` declarations across 10 files
- [x] `;; Version: 1.0.1` header in superchat.el
- [x] `;; Package-Requires: ((emacs "28.1") (llm "0.24"))`
- [x] GPL-3.0-or-later license
- [x] Git tags: v1.0.0, v1.0.1 on main
- [x] `CHANGELOG.md` (v0.2→v1.0.1)
- [x] README.md + README_cn.md rewritten for v1.0
- [x] `docs/architecture.md` technical documentation
- [x] `docs/SKILLS_QUICKSTART.md` skill authoring guide
- [x] 3 example skills in `examples/standard-skills/`
- [x] ROADMAP.md synced to actual state
- [x] Test suite: 51/51 green
- [x] MCP v2 multi-server orchestration shipped

## Recipe

Add to `recipes/superchat` in [melpa/melpa](https://github.com/melpa/melpa):

```elisp
(superchat :fetcher github :repo "yibie/superchat")
```

## Submission steps

1. Fork https://github.com/melpa/melpa
2. Create branch `superchat`
3. Add the recipe file above
4. Push and open a PR

## Optional dependencies

- `mcp` — for MCP server integration
- `mcp-hub` — for MCP multi-server orchestration

Both are NOT in `Package-Requires` (optional). superchat works fully without them.

## Why this is ready for MELPA

- Emacs 28.1+ minimum (uses only stable APIs)
- Single external dependency: `llm` from GNU ELPA
- All internal functions use `superchat--` prefix
- All public functions use `superchat-` prefix
- No MELPA policy violations (no external non-ELPA deps, no code in `*-pkg.el`, license is GPL-3)
