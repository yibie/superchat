# Decision records

A decision record keeps the part of a change that code and specs cannot carry: why it was made and what was given up. Every change that alters behaviour, a contract shared across files, a format, or the development process adds or updates one record in the same commit. A purely mechanical edit is exempt.

## Layout

`docs/decisions/{proposed,accepted,implemented,rejected}/YYYY-MM-DD-topic.md`. The folder is the status; the date is when the topic was first proposed. A record moves between folders as its status changes and is never rewritten into a different decision: supersede it with a new record and link both ways.

| Folder | Meaning |
|---|---|
| `proposed/` | A recommendation the user has not decided on |
| `accepted/` | The user has decided it. The status says nothing about implementation: whether the decision is unbuilt, partly built or built is stated item by item in [the roadmap](../roadmap.md). The record cites the user's words so the question is not asked again |
| `implemented/` | Shipped; the record describes what is, in the present tense |
| `rejected/` | Considered and declined; the verdict is on the status line |

## File format

The first three lines are exactly:

```markdown
# Decision: <title>

Status: proposed | accepted | implemented | rejected — <one-line reason>
```

Body sections, in this order and with these names:

- `proposed/`: `## Problem`, `## Proposal`, free sections, `## Alternatives considered`, `## Acceptance criteria`, `## Risks`.
- `accepted/`: `## Problem`, `## Decision` (what the user decided, with the source), free sections, `## Alternatives considered`, `## Consequences`. The prose does not say "will"; implementation state is the roadmap's job.
- `implemented/`: `## Problem`, `## Decision` (present tense), free sections, `## Alternatives considered`, `## Consequences`.
- `rejected/`: the proposal frozen as it was.

`## Alternatives considered` is mandatory: each real alternative and why it lost, one paragraph each. A decision without its losers gets re-argued.

## Cross-references

A record cites the component specs and the roadmap milestones it touches with relative links, never by prose name alone. When code later moves or renames something the record mentions, the record is updated in the same change (facts only, not the decision).

## Writing rules

Present tense; no "previously", "now", "no longer". History lives in version control and in superseded records. One fact has one home: if a spec or [the roadmap](../roadmap.md) already states it, link there instead of restating. The roadmap owns the order of work; a record owns one decision.
