---
name: git-commit-message
description: Generate a conventional-commit message from the current diff or attached file
version: "1.0"
type: workflow
---

# Each line is one step. # comments and blank lines are skipped.
# Variables: $input

/web-search conventional commit format specification (types: feat, fix, docs, style, refactor, perf, test, chore, ci, build)
@claude-3-5-sonnet-20241022 Analyze the diff context above. Write a single conventional-commit message. Summary line under 72 chars. Body explains what and why (not how). If the input is not a git diff, reply "No diff found — attach a diff file with #path or paste one."
