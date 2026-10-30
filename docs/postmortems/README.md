# Postmortems

A postmortem is a backward-looking record of a failure that reached a place it should not have: what broke, the mechanism, why every safety net missed it, and the guardrails added so the same class fails loudly next time. It is not a decision record; a decision record explains a choice and its rejected alternatives.

Write one when the bug is subtle (the mechanism is not obvious and a careful engineer would re-derive it the hard way), systemic (it escaped because of a gap in tests, tooling or conventions, not a typo), and costly to rediscover. Otherwise the fix and its test are enough.

## File format

`docs/postmortems/NNNN-topic.md`, numbered in order. Sections in this order:

```markdown
# Postmortem NNNN: <title>

Status: resolved | open

## Executive summary
One paragraph a reader absorbs in thirty seconds: what broke, the root cause in plain words, why it escaped, the durable lesson.

## Summary
## Impact
## Timeline
Evidence in order, not a teaching sequence.
## Root cause
The representational or process gap, not only the wrong line.
## Guardrails added
Links to the test, rule, spec sentence or smoke case that now catches this class.
## Lessons
```

## Index

| # | Title |
|---|---|
| [0001](0001-a-broken-tool-call-lost-its-type.md) | A broken tool call lost its type without streaming |
