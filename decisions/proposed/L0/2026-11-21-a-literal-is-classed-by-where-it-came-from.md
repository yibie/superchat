# Decision: A literal is classed by where it came from

Status: proposed

## Problem

`/learn` keeps recipes as the code that ran: `(calendar-last-day-of-month 2
2027)`, where a skill needs `(calendar-last-day-of-month MONTH YEAR)`. Two
offline experiments on the person's Tape (217 successful `eval_elisp`
forms) showed the limits of syntax alone:

- Replacing every number and string with a placeholder merges forms that
  differ only in a value, but also `(load "/tmp/verify.el")` with
  `(load "/tmp/dedup.el")`, where the string is what the code is.
- Normalising (macro expansion, renaming locals) and anti-unifying the
  instances of a shape gives more exact templates, with holes named from
  the callee's arglist, but did not change the count: 211 shapes either
  way, because most forms are one-off exploration.
- Templates built from expanded code are unreadable; holes inside a
  quoted list all take one name; a value bound with `let` before use
  gets no name.

Whether a literal is an input or part of the code is the open question;
how many times a form recurs does not answer it.

## Proposal

Class each literal of a cited form by its origin, as TraceCompiler
(El Yadouni and Li, arXiv:2608.02680, 2026) classes the bindings of
tool calls: "behavioral, not name-based". The Tape holds what that needs:
the run's `user` text, every earlier tool result, and, since answers name
their sources, which calls a result rests on.

1. **Classes.** For each number and string literal of a cited
   `eval_elisp` form, in its run:
   - `input`: the value occurs in the run's `user` text and in no earlier
     tool result of the run;
   - `copied #ID`: the value occurs in exactly one earlier tool result of
     the run, #ID, and not in the `user` text; the hard edge of the paper;
   - `suspected`: the value occurs in more than one of those places;
   - `constant`: it occurs in none, and all instances of the template agree
     on it;
   - `unknown`: anything else, including a value the model chose itself.
   Matching is exact: a string as written, a number as a whole token of
   the text. Values too short to say anything (numbers below 10, strings of
   fewer than 3 characters, `t`, `nil`) are `unknown`, because they occur
   everywhere; the paper names this shared-value false positive.
2. **Holes.** `input` and `copied` literals become holes; `suspected` ones
   become holes marked for the person; `constant` and `unknown` stay as
   written. A hole is named by the parameter it reaches: the callee's
   arglist for a direct argument, the use of a `let` variable for a bound
   one. A quoted list whose elements are all holes of one argument is one
   hole. A `copied` hole records its producer, so a skill can say where
   the value is got from.
3. **Two forms of the code.** Shapes are compared on the normalised form
   (expanded, locals renamed); templates are written on the form as it ran.
4. **Round trip.** A template is kept only when putting each instance's
   values back into its holes and reading the result gives that instance's
   code exactly. A template the model proposes is held to the same check.
5. **The person decides roles that stay open.** In the `/learn` draft,
   `suspected` and `unknown` literals are marked. Keeping a hole or a
   literal there records the role for that function and argument position
   (beside `rejected.eld`), and later drafts use it before rule 1.
6. **Declining.** A form of class `write` or `exec` whose target (a file,
   buffer or process argument) is `unknown` or `suspected` gets no
   `interface.eld` entry: its recipe stays as text, as the paper declines
   to compile an irreversible effect it cannot pin down.
7. **Evidence.** A later cited form that is an instance of a recipe's
   template, by the round trip, adds one to that recipe's count of runs;
   the draft and the skill show it.

An offline replay over the person's Tape comes first, as with the
earlier experiments: it reports how many literals of successful forms
each class takes, before any code is written.

## Alternatives considered

- Replace every literal with a placeholder (the first experiment). Not
  taken: it merged `(load "/tmp/verify.el")` with `(load "/tmp/dedup.el")`.
- Let a model decide each literal's role (suggested in two outside
  answers). Not taken alone: a model may name holes, but its roles cannot
  be checked; origin and the round trip can.
- A hand-written catalogue of function arguments and their roles
  (suggested in an outside answer). Not taken: it covers only what someone
  wrote; rule 5 builds it from the person's own decisions.
- A weighted score of evidence, reuse and risk to promote candidates
  (suggested in an outside answer). Not taken: nothing to set the weights
  from.
- The paper's recency rule, giving an ambiguous value to the most recent
  producer. Not taken: the paper's own ablation shows it is where its
  accuracy comes from, not the exclusion test; here an ambiguous value is
  `suspected` and shown to the person.

## Acceptance criteria

- The offline replay reports, for the Tape's successful `eval_elisp`
  forms, the count of literals in each class, with examples of each.
- `(calendar-last-day-of-month 2 2027)` cited after a question naming
  February 2027 gives `(calendar-last-day-of-month MONTH YEAR)` with both
  holes `input`.
- A telega call whose chat id was read from an earlier result gives a hole
  `copied #ID` naming that call.
- `(load "/tmp/verify.el" nil t)` keeps its string: `unknown`, no hole.
- A template that does not give back each instance's code is not kept.
- A `write` form with an `unknown` target gets no interface entry.

## Risks

- Exact matching misses values the model reformatted (`"2027"` in the
  request, `2027` in the code is caught; `Feb` against `2` is not); such
  values fall to `unknown` and stay as written, which is safe but merges
  less.
- Most of the Tape's forms predate cited answers; the replay must use all
  successful forms with their runs, and its numbers say what the rule can
  do, not what `/learn` will see.
- `superchat-learn.el` is at 216/250 lines; the analysis will not fit
  there. Where it lives (an L1 view of bindings, or an L5 module) is for
  the person to decide when it is built.
- The paper's results come from one synthetic benchmark and one agent's
  traces; its authors say its detection of false positives was not
  measured.
