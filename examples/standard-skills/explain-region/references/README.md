# explain-region

A `type: prompt` skill that instructs the model to explain code at the level of a senior engineer.

## Invocation

```
>explain-region #path/to/file.el
```

Or paste code and invoke:

```
>explain-region
[your pasted code]
```

## What it does

The skill injects a system prompt that tells the model to focus on:
- What the code actually does (behavior, not intent)
- Why it's structured this way (conventions, constraints, history)
- Non-obvious edge cases
- Hidden dependencies

It explicitly instructs the model NOT to explain basic language syntax — the audience is another senior engineer.

## Triggers

The `triggers` field in the frontmatter enables implicit matching. If the user types "explain this code" or "这段代码什么意思" without explicitly invoking `>explain-region`, superchat may auto-select this skill.

## How to customize

Edit the system prompt body in `SKILL.md` to adjust the explanation depth, add domain-specific focus areas, or change the audience level.
