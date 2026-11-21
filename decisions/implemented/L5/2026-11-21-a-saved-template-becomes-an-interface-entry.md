# Decision: A saved template becomes an interface entry

Status: implemented

## Problem

`/learn` drafts templates with named, typed holes and their origins, but a
skill's `interface.eld`, which turns recipes into typed tools, was still
written by hand. Rule 6 of
[a literal is classed by where it came from](2026-11-21-a-literal-is-classed-by-where-it-came-from.md),
no tool for a write whose target is unclear, was not built because nothing
drafted entries.

## Decision

When `C-c C-c` saves a draft, each template a saved block matches becomes
an entry of `interface.eld` beside the skill, unless an entry with the same
code is there. Values the person put back are written into the code; the
holes left are the args, lower-cased, typed `number`, `string` or `boolean`
by their value. The name is the first PREFIX function's name without PREFIX,
with `_` for `-`; the doc is the section's heading and each hole's origin.

The class is `read` only when every function the code calls is marked
side-effect-free or pure by Emacs, ends in `-p`, or is in
`superchat-learn-read-functions`; anything else is `write`, so the tool
asks at level `ask`. A `write` entry with a `suspected` or `varies` hole is
not made: the recipe stays text only. The person changes a class in
`interface.eld`; the save message says so.

## Alternatives considered

- **The person picks each class in the draft.** Discussed in this session.
  It needs an editable interface block in the draft buffer and parsing it
  back; a conservative guess plus editing the file gives the same control
  with less code.
- **The model picks the class.** Its answer cannot be checked, and a wrong
  `read` runs a write without asking.
- **Entries for every template, unclear targets included.** Rule 6 of the
  literal-classes note rejects it, after TraceCompiler declining to compile
  an irreversible effect whose target the trace leaves unclear.

## Consequences

- A hole whose value is a list, such as a whole date, cannot be an arg, so
  its template makes no entry. An entry with no hole says it takes no input.
- A draft block that leaves out the template's top-level `require` forms
  still matches it, and the template's own text is put in its place: in a
  live run the model dropped `(require 'calendar)` and the recipe was lost.
- Two templates that differ only by a `require` make two entries.
- Most package functions are not marked side-effect-free, so most entries
  start as `write` and ask until the person changes them.
- `superchat-learn.el`'s cap rises from 680 to 760 lines, by the person's
  standing choice for this module; the product's total limit is 7,000.
