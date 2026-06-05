# git-commit-message

A `type: workflow` skill that generates a conventional-commit message from a git diff.

## Invocation

```
>git-commit-message #/path/to/diff.txt
```

Or paste a diff directly into the chat buffer and run:

```
>git-commit-message
```

## What it does

1. Searches the web for the conventional commit format specification (feat, fix, docs, etc.).
2. Instructs the model to write a single conventional-commit message with a summary line under 72 characters and a body explaining what and why.

## How to customize

Edit `SKILL.md` and change the model on line 9 (`@claude-3-5-sonnet-20241022`) to your preferred model. You can also adjust the web-search query on line 8 to include your team's commit conventions.
