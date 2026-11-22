# How a skill grows: `/learn calendar-`

Three `/learn` rounds over a test tape. The questions and the model's draft are
scripted. Every result is what the form returned in Emacs. Every file below was
written by `superchat-learn.el`.

Files live in `skills/calendar/`:
- `roles.eld`: per function and argument position, `hole` (filled each call) or `keep` (literal kept).
- `evidence.eld`: template → ids of the tool results it ran as.
- `rejected.eld`: forms the person deleted from a draft; never drafted again.

## Round 1: new skill

Runs: `(calendar-leap-year-p 2028)`, `(... 2027)`, `(calendar-last-day-of-month 2 2027)`,
`(... 4 2025)`, and `(calendar-gregorian-from-absolute (+ 739975 100))`, where
739975 is copied from the earlier result of `(calendar-absolute-from-gregorian '(12 25 2026))`.
Before saving, you put `2` back for `MONTH` in the last-day block.

`roles.eld`
```elisp
(((calendar-leap-year-p . 0) . hole) ((calendar-last-day-of-month . 0) . keep) ((calendar-last-day-of-month . 1) . hole) ((calendar-absolute-from-gregorian . 0) . hole))
```

`evidence.eld`
```elisp
(("((calendar-leap-year-p YEAR))" 3 7) ("((calendar-last-day-of-month MONTH YEAR))" 11 15) ("((calendar-absolute-from-gregorian DATE))" 19) ("((calendar-gregorian-from-absolute (+ NUMBERS-OR-MARKERS NUMBERS-OR-MARKERS2)))" 21))
```

## Round 2: the skill grows

New runs: `(calendar-day-name '(11 22 2026))`, `(calendar-day-of-week '(1 1 2027))`,
and one more leap year. You delete the day-of-week section from the draft before saving.

`rejected.eld` (new)
```elisp
("(calendar-day-of-week DATE)")
```

`evidence.eld`: leap year now `3 7 27`, `calendar-day-name` added
```elisp
(("((calendar-day-name DATE))" 31) ("((calendar-leap-year-p YEAR))" 3 7 27) ("((calendar-last-day-of-month MONTH YEAR))" 11 15) ("((calendar-absolute-from-gregorian DATE))" 19) ("((calendar-gregorian-from-absolute (+ NUMBERS-OR-MARKERS NUMBERS-OR-MARKERS2)))" 21))
```

## Round 3: nothing new

Runs: one more leap year, one more day-of-week.

```
No new form for the skill of calendar-: all 6 are in it or were dropped
```

`evidence.eld`: the repeat leap-year run is recorded (`41`); nothing is drafted.
```elisp
(("((calendar-day-name DATE))" 31) ("((calendar-leap-year-p YEAR))" 3 7 27 41) ("((calendar-last-day-of-month MONTH YEAR))" 11 15) ("((calendar-absolute-from-gregorian DATE))" 19) ("((calendar-gregorian-from-absolute (+ NUMBERS-OR-MARKERS NUMBERS-OR-MARKERS2)))" 21))
```

## Final `SKILL.md`

````markdown
---
name: calendar
version: Emacs 32.0.50
description: Dates, leap years and weekdays in Emacs's calendar.
---
## calendar-leap-year-p
What calendar-leap-year-p returns.
```elisp
(calendar-leap-year-p YEAR)
```
YEAR: from the request.

## calendar-last-day-of-month
What calendar-last-day-of-month returns.
```elisp
(calendar-last-day-of-month 2 YEAR)
```
MONTH: differs between runs: check it. YEAR: from the request.

## calendar-absolute-from-gregorian
What calendar-absolute-from-gregorian returns.
```elisp
(calendar-absolute-from-gregorian DATE)
```
DATE: from the request.

## calendar-gregorian-from-absolute
What calendar-gregorian-from-absolute returns.
```elisp
(calendar-gregorian-from-absolute
 (+ NUMBERS-OR-MARKERS NUMBERS-OR-MARKERS2))
```
NUMBERS-OR-MARKERS: read from the result of (calendar-absolute-from-gregorian '(12 25 2026)). NUMBERS-OR-MARKERS2: from the request.

## calendar-day-name
What calendar-day-name returns.
```elisp
(calendar-day-name DATE)
```
DATE: from the request.
````

The line `MONTH: differs between runs: check it.` was written with the draft.
Putting `2` back into the block afterwards does not rewrite it; `roles.eld`
records the decision, so later drafts keep `2`.
