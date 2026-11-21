# Decision: A literal is classed by where it came from

Status: implemented

## Problem

`/learn` kept recipes as the code that ran: `(calendar-last-day-of-month 2
2027)`, where a skill needs a form with holes. Two offline experiments on
the person's Tape (217 successful `eval_elisp` forms) showed the limits of
syntax alone:

- Replacing every number and string with a placeholder merges forms that
  differ only in a value, but also `(load "/tmp/verify.el")` with
  `(load "/tmp/dedup.el")`, where the string is what the code is.
- Normalising and anti-unifying the instances of a shape gives more exact
  templates, with holes named from the callee's arglist, but did not change
  the count: 211 shapes either way, because most forms are one-off.
- Templates built from macro-expanded code are unreadable; a value bound
  with `let` before use got no name.

Whether a literal is an input or part of the code is the open question;
how many times a form recurs does not answer it.

## Decision

Each literal of a cited form is classed by its origin, as TraceCompiler
(El Yadouni and Li, arXiv:2608.02680, 2026) classes the bindings of tool
calls, "behavioral, not name-based". It is built in `superchat-learn.el`,
the person's choice over a new module.

1. **Classes.** A form's run is the latest `user` entry before its result
   and the `tool` results between. A number of at least 100, or a string of
   at least 3 characters, is `input` when it occurs in the user text and in
   no earlier result; `copied #ID` when it occurs in exactly one earlier
   result, #ID, and not the user text; `suspected` when it occurs in more
   than one of those; `unknown` when it occurs in none. Matching is exact:
   a string as written, a number as a whole token. Smaller values occur
   everywhere and stay as written; the replay showed `15` and `80` linked
   to unrelated results at a threshold of 10.
2. **Holes.** `input`, `copied` and `suspected` literals become holes, named
   in upper case by the argument they reach: the callee's arglist for a
   direct argument, the first use of a `let` variable, the variable of a
   `setq`. One value is one hole. A quoted list of literals is one hole.
3. **Round trip.** A template is printed, read back, and each hole's value
   put back by name; only when that gives the form exactly is it kept.
   Otherwise the form stays as it ran.
4. **Comparing.** Forms compare with `let`, `let*` and `lambda` variables
   renamed in binding order; equal templates become one recipe with all
   their results' ids. Templates are written on the code as it ran.
5. **The draft.** The model is given each template with its holes, their
   values and origins, and copies the template. Under each kept block with
   holes, code adds where each came from: "from the request", or "read from
   the result of" the code of the producing call, never its Tape id, which
   a later run cannot use. The count of runs stays in `evidence.eld`, not in
   the skill's text, where it would go stale. The header counts the holes,
   the `suspected` ones, and the `unknown` values left as written.
6. **The person's decisions.** A saved block matches an offered template
   when it is the template with any holes' values put back. For a function
   named with the package's prefix, each hole left records `hole` and each
   value put back `keep`, by argument position, in `roles.eld` beside the
   skill; later drafts use these before the origin. Generic functions such
   as `list` or `equal` record nothing: one decision would spread to every
   use of them.
7. **Runs of one shape.** Templates equal once literals, quoted lists of
   literals and upper-case symbols are blanked (locals renamed) are runs of
   one procedure. They are generalised into one template: where the runs'
   values differ the position is a hole, `varies` when no run had a hole
   there, marked for the person like `suspected`. It is kept only when every
   run's code comes back from it. A skill's existing recipes are matched by
   this shape too, so a later run of a recipe adds evidence and is not
   drafted again.
8. **Evidence.** `evidence.eld` beside the skill maps a template to the ids
   of the results it ran as. A later `/learn` adds the runs of templates
   already in the skill; saving adds those of the templates kept.

## Alternatives considered

- Replace every literal with a placeholder (the first experiment). Not
  taken: it merged `(load "/tmp/verify.el")` with `(load "/tmp/dedup.el")`.
- Let a model decide each literal's role (suggested in two outside
  answers). Not taken alone: a model names holes in its text, but its roles
  cannot be checked; origin and the round trip can.
- A hand-written catalogue of function arguments and their roles
  (suggested in an outside answer). Not taken: it covers only what someone
  wrote; `roles.eld` is built from the person's own decisions.
- A weighted score of evidence, reuse and risk to promote candidates
  (suggested in an outside answer). Not taken: nothing to set the weights
  from.
- The paper's recency rule, giving an ambiguous value to the most recent
  producer. Not taken: the paper's own ablation shows it is where its
  accuracy comes from, not the exclusion test; here an ambiguous value is
  `suspected` and counted in the header for the person.
- Have the model mark which literals came from the request, which it can
  see when the request rewords a value ("二月" for `2`). Not taken for now:
  its marks cannot be checked; it can come later, marked for the person.
- Compare macro-expanded forms. Not taken: expansion runs the person's
  macros, and in the experiment renaming locals did all the merging.

## Consequences

- On the person's Tape, the replay over 223 successful forms (1173
  literals) found 111 `copied`, 120 `suspected`, 2 `input`, 629 `unknown`
  and 311 too small; 105 forms got holes. Requests are mostly in Chinese
  and code in English, so `input` is rare and most holes are values read
  from earlier results.
- A synthetic set written by Pi with DeepSeek (48 cases, each value labelled
  with its true role, traps included) scored: of 53 values made holes by
  origin, 49 were right (92%); every `copied` hole named the right step;
  half the values from the request were found (32 of 63), the rest reworded
  in the request ("March 15, 2026, 12:00" for `"2026-03-15 12:00"`) or too
  small. Generalising runs of one shape then merged all 12 repeated
  procedures into one template each (4 before it), with no two procedures
  merged; its 8 `varies` holes were all true inputs.
- A single run of `(calendar-last-day-of-month 2 2027)` after a question on
  February 2027 gives `(calendar-last-day-of-month 2 YEAR)`: the month 2 is
  too small to trace; a second run with another month makes it `MONTH`.
- `(load "/tmp/a.el")` and `(load "/tmp/b.el")` in two runs become one
  template with a `varies` hole: the person decides, and `keep` is recorded.
- Not built: declining an `interface.eld` entry for a `write` form with an
  unclear target, since `/learn` does not draft `interface.eld` yet.
- The run's context is its latest `user` entry: after the person types
  "继续", the values of the original request fall to `unknown`.
- A value the model reformatted (`Feb` for `2`) is not matched and stays as
  written: safe, but it merges less.
- `superchat-learn.el` went from 216 to 637 lines; its cap from 250 to
  680, and the product's total cap from 6,000 to 6,300 lines, the person's
  choice to raise caps rather than restructure. `spec/L5-interface.md`'s
  ceiling went from 4420 to 4880 words.
