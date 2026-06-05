# weekly-tech-digest

A `type: workflow` skill that searches for recent tech news and produces a multi-angle summary report.

## Invocation

```
>weekly-tech-digest AI safety
>weekly-tech-digest open source databases
```

## What it does

1. Searches the web for the given topic plus "tech news this week."
2. Summarizes the results from 3 angles: business impact, technical depth, social implications.
3. Saves the report to `~/Documents/weekly-tech-digest.md`.

Each run overwrites the previous report — rename the output file in `SKILL.md` if you want to keep history.

## Variables

- `$input` — the topic you provide after `>weekly-tech-digest`.
- `$lang` — the value of `superchat-lang` (used for the report language).

## How to customize

- Change the output path on the last line (e.g., `#~/Documents/ai-news.md`).
- Adjust the model on line 10 to your preferred LLM.
- Add more angles by editing the summary instructions on line 10.
